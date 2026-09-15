;;; e-board.el --- Independent Board observation capability for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The Board capability is the consumer-facing surface for durable participant
;; and run observation.  It is intentionally separate from `subagents': child
;; spawning and live controls may be absent while committed Board facts remain
;; queryable.  The small run-set registry below retains only the current,
;; bounded detached value for one chat session; SQLite remains authoritative.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'e-capabilities)
(require 'e-board-observation)
(require 'e-board-orchestration)
(require 'e-board-orchestration-actions)
(require 'e-context)
(require 'e-chat-service)
(require 'e-layers)
(require 'e-skills)
(require 'e-work)

(defconst e-board-instructions
  "Board is the durable observation surface for the current chat's Board. Use `list', `status', and `read' for participant activity and bounded durable transcripts; use `list-runs' and `run-status' for orchestration. These actions read committed SQLite facts and never classify a row from private live execution state. Read e://board/skills/board for the action contract."
  "Compact model-facing guidance for the Board capability.")

(defconst e-board-skill
  (string-join
   '("# Board observation actions"
     ""
     "Board actions read one bounded detached SQL result at a time. They remain available to ordinary owner chats even when child supervision is disabled."
     ""
     "## Participant activity"
     ""
     "- `list`: returns one ordered mixed participant/activity page. Use its opaque cursor for the next page."
     "- `status`: input `(:participant-id STRING)`, returning the exact durable participant outcome."
     "- `read`: input `(:participant-id STRING :raw BOOLEAN :limit INTEGER)`. Ordinary reads use the durable projection; `:raw` requests a bounded durable session transcript page."
     ""
     "## Runs"
     ""
     "- `list-runs`: returns the bounded active run-set for this Board. Entries are ordered by attention/actionability and latest durable event."
     "- `run-status`: input `(:run-id STRING)`, returning one bounded durable run projection."
     ""
     "Committed Board facts are authoritative. Missing or stale process-local handles only make live controls unavailable; they never hide, reorder, reject, or mutate a durable row."
     "")
   "\n")
  "Detailed Board capability action reference.")

(defvar e-board--run-set-states
  (make-hash-table :test 'eq :weakness 'key)
  "Current bounded run-set states keyed by live binding or legacy context.

Production entries are keyed by the exact chat binding and therefore retire
with that binding.  The identity-keyed table deliberately does not use
`equal': binding structs mutate as readiness and subscriptions are installed.
Detached provider contexts use `e-board--run-set-detached-states' instead.")

(defvar e-board--run-set-detached-states
  (make-hash-table :test 'eq :weakness 'key)
  "Bounded fallback states grouped by detached harness identity.

This table exists only for provider tests and other contexts without a live
binding.  Its weak harness keys let the whole session group retire when the
detached owner disappears; each value is a small equal-key session table.")

(defvar e-board--run-set-controls
  (make-hash-table :test 'eq :weakness 'key)
  "Request-local refresh controls keyed by bounded run-set state objects.")

(cl-defstruct (e-board--run-set-barrier
               (:constructor e-board--run-set-barrier-create))
  state binding run-id work unsubscribe start-generation query-work
  await-refresh-p query-settled-p fence-generation expected-presence)

(defvar e-board--run-set-barriers
  (make-hash-table :test 'eq :weakness 'key)
  "Active Board-owned run-set barriers keyed by session-scoped state.")

(defun e-board--run-set-binding (harness session-id)
  "Return the exact live binding for HARNESS and SESSION-ID, when present."
  (and (e-harness-p harness)
       (stringp session-id)
       (e-chat-service-binding harness session-id)))

(defun e-board-run-set-state-for (harness session-id &optional board-id)
  "Return the bounded run-set state for HARNESS and SESSION-ID.
When no state exists, create a restoring state.  BOARD-ID is retained only in
the detached value and may be supplied by the caller that knows the binding."
  (let* ((binding (e-board--run-set-binding harness session-id))
         (detached-states
          (and (not binding)
               (or (gethash harness e-board--run-set-detached-states)
                   (let ((table (make-hash-table :test 'equal)))
                     (puthash harness table e-board--run-set-detached-states)
                     table))))
         (table (or (and binding e-board--run-set-states)
                    detached-states))
         (key (or binding (copy-sequence session-id)))
         (state (gethash key table)))
    (or state
        (setq state
              (puthash key
                       (e-board-orchestration-run-set-state-create
                        :board-id board-id)
                       table)))
    (when (and board-id
               (null (e-board-orchestration-run-set-state-board-id state)))
      (setf (e-board-orchestration-run-set-state-board-id state)
            (copy-sequence board-id)))
    state))

(defun e-board--run-set-controls (state)
  "Return STATE's request-local refresh controls."
  (gethash state e-board--run-set-controls))

(defconst e-board--run-set-mapped-work-spec
  (e-work-spec-create
   :id "board-run-set-state-map" :execution 'cooperative
   :interactive-policy 'async :owner 'board
   :runner
   (lambda (parent arguments _context)
     (let ((child (plist-get arguments :child))
           (mapper (plist-get arguments :mapper)))
       (setf (e-work-handle-cancel-function parent)
             (lambda (_handle)
               (when (and (e-work-handle-p child)
                          (not (memq (plist-get (e-work-status child) :state)
                                     '(finished failed cancelled))))
                 (e-work-cancel child))))
       (e-work-on-settle
        child
        (lambda (settled)
          (pcase (plist-get (e-work-status settled) :state)
            ('finished
             (condition-case error
                 (e-work-finish parent
                                (funcall mapper
                                         (e-work-handle-result settled)))
               (error (e-work-fail parent error))))
            ('failed (e-work-fail parent (e-work-handle-error settled)))
            ('cancelled (e-work-cancel parent)))))
       :deferred)))
  "Work contract for Board-owned detached run-set state installation.

This mapper is deliberately owned by the Board application layer: the
model-facing orchestration action adapter must not become a dependency of
Board readiness composition merely because both consume `e-work' handles.")

(defun e-board--run-set-map-work (child mapper)
  "Return request-scoped work mapping CHILD through Board MAPPER."
  (e-work-start e-board--run-set-mapped-work-spec
                (list :child child :mapper mapper)))

(defconst e-board--run-set-query-spec
  (e-work-spec-create
   :id "board-run-set-query" :execution 'cooperative
   :interactive-policy 'async :owner 'board
   :runner (lambda (_parent _arguments _context) :deferred))
  "Work contract for a stable Board run-set query carrier.

The carrier stays pending when a committed-write notification races its SQL
child.  This lets the owner readiness boundary receive the refreshed detached
projection rather than settling on the first stale snapshot.")

(defun e-board--run-set-query-fail (state work error)
  "Mark STATE unavailable before failing stable query WORK with ERROR.

The detached state is the observation boundary shared by readiness, context,
and status.  A query-start or value-install error must therefore publish the
unavailable state before its carrier fails, so a barrier cannot remain bound
to a previously ready value while the request that should refresh it has
failed."
  (e-board-orchestration-run-set-state-update
   state nil :restore-state 'unavailable)
  (e-work-fail work error))

(defun e-board--run-set-query-child-start (binding state work)
  "Start one SQL run-set child for stable query WORK."
  (let* ((target (e-chat-service-publication-target binding))
         (child (e-board-orchestration-actions-run-set target))
         ;; Keep the Board-owned mapper boundary, but install STATE only after
         ;; the stable carrier decides that no committed refresh raced CHILD.
         (mapped (e-board--run-set-map-work child #'identity))
         (controls (e-board--run-set-controls state)))
    (setq controls (plist-put controls :current-child mapped))
    (puthash state controls e-board--run-set-controls)
    (e-work-on-settle
     mapped
     (lambda (settled)
       (e-board--run-set-query-child-settled
        binding state work mapped settled)))
    mapped))

(defun e-board--run-set-query-child-settled
    (binding state work child settled)
  "Advance stable run-set WORK after CHILD SETTLED."
  (when (and (eq (plist-get (e-board--run-set-controls state) :query-work)
                work)
             (not (e-request-terminal-p (e-work-handle-lifecycle work)))
             (eq (plist-get (e-board--run-set-controls state) :current-child)
                 child))
    (let* ((status (e-work-status settled))
           (controls (e-board--run-set-controls state))
           (rerun-p (plist-get controls :rerun-p)))
      (setq controls (plist-put controls :current-child nil))
      (if rerun-p
          (if (and (e-chat-service-binding-p binding)
                   (not (eq (e-chat-service-binding-lifecycle-state binding)
                            'retired)))
              (progn
                ;; Do not expose CHILD's potentially stale value.  The next
                ;; bounded SQL read becomes the one that settles readiness.
                (setq controls (plist-put controls :rerun-p nil))
                (puthash state controls e-board--run-set-controls)
                (condition-case error
                    (e-board--run-set-query-child-start binding state work)
                  (error
                   (setq controls (plist-put controls :query-work nil))
                   (puthash state controls e-board--run-set-controls)
                   (e-board--run-set-query-fail state work error))))
            ;; A binding that retired while the stale child was settling must
            ;; not install that child or leave its readiness pending.
            (setq controls (plist-put controls :query-work nil))
            (puthash state controls e-board--run-set-controls)
            (e-board-orchestration-run-set-state-update
             state nil :restore-state 'unavailable)
            (e-work-cancel work))
        (setq controls (plist-put controls :query-work nil))
        (puthash state controls e-board--run-set-controls)
        (pcase (plist-get status :state)
          ('finished
           (condition-case error
               (progn
                 (e-board-orchestration-run-set-state-set-value
                  state (e-work-handle-result settled))
                 (e-work-finish work (e-work-handle-result settled)))
             (error (e-board--run-set-query-fail state work error))))
          ('cancelled
           (e-board-orchestration-run-set-state-update
            state nil :restore-state 'unavailable)
           (e-work-cancel work))
          (_
           (e-board-orchestration-run-set-state-update
            state nil :restore-state 'unavailable)
           (e-work-fail work (or (e-work-handle-error settled)
                                 '(e-board-orchestration-error
                                   "Board run-set query failed")))))))))

(defun e-board--run-set-query-start (binding state controls)
  "Start one bounded durable run-set query for BINDING and STATE."
  (let ((work (e-work-start e-board--run-set-query-spec nil)))
    (setq controls (plist-put controls :query-work work))
    (puthash state controls e-board--run-set-controls)
    (setf (e-work-handle-cancel-function work)
          (lambda (_handle)
            (when-let ((current (plist-get
                                 (e-board--run-set-controls state)
                                 :current-child)))
              (unless (e-request-terminal-p
                       (e-work-handle-lifecycle current))
                (e-work-cancel current)))))
    (condition-case error
        (e-board--run-set-query-child-start binding state work)
      (error
       (setq controls (plist-put controls :query-work nil))
       (puthash state controls e-board--run-set-controls)
       (e-board--run-set-query-fail state work error)))
    work))

(defun e-board--run-set-refresh (binding state)
  "Refresh STATE after a durable Board commit without polling."
  (when-let ((controls (e-board--run-set-controls state)))
    (if-let ((work (plist-get controls :query-work)))
        (unless (memq (plist-get (e-work-status work) :state)
                      '(finished failed cancelled))
          (setq controls (plist-put controls :rerun-p t))
          (puthash state controls e-board--run-set-controls))
      (e-board--run-set-query-start binding state controls))))

(defconst e-board--run-set-barrier-spec
  (e-work-spec-create
   :id "board-run-set-barrier" :execution 'cooperative
   :interactive-policy 'async :owner 'board
   :runner (lambda (_parent _arguments _context) :deferred))
  "Work contract for waiting on one exact durable Board run projection.")

(defun e-board--run-set-barrier-remove (barrier)
  "Detach BARRIER from its run-set state and request-local registry."
  (when-let* ((unsubscribe
               (e-board--run-set-barrier-unsubscribe barrier)))
    (setf (e-board--run-set-barrier-unsubscribe barrier) nil)
    (funcall unsubscribe))
  (let* ((state (e-board--run-set-barrier-state barrier))
         (remaining
          (delq barrier (gethash state e-board--run-set-barriers))))
    (if remaining
        (puthash state remaining e-board--run-set-barriers)
      (remhash state e-board--run-set-barriers))))

(defun e-board--run-set-barrier-run (value run-id)
  "Return VALUE's exact RUN-ID entry when it is ready, or nil."
  (and (plist-get value :ready-p)
       (seq-find (lambda (run)
                   (equal (plist-get run :run-id) run-id))
                 (plist-get value :runs))))

(defun e-board--run-set-barrier-query-settled (barrier _settled)
  "Observe BARRIER after its refresh query settles without a state wake-up."
  (let ((work (e-board--run-set-barrier-work barrier)))
    (unless (e-request-terminal-p (e-work-handle-lifecycle work))
      (setf (e-board--run-set-barrier-query-settled-p barrier) t)
      (let ((state (e-board--run-set-barrier-state barrier)))
        (e-board--run-set-barrier-observe
         barrier
         (e-board-orchestration-run-set-state-value state)
         (e-board-orchestration-run-set-state-generation state))))))

(defun e-board--run-set-barrier-observe (barrier value &optional generation)
  "Settle BARRIER from detached run-set VALUE when its contract is met.

When BARRIER starts during a refresh, the ready value at its starting
generation is the pre-controller snapshot and remains provisional.  A later
observer-visible generation, or the refresh carrier's settlement itself, is a
bounded completion point: an absent exact run is then a real projection
failure, including a bounded projection whose omitted count hides that run.
When no refresh is active at entry, a ready missing-run value is already a
completed refresh and fails immediately."
  (let* ((work (e-board--run-set-barrier-work barrier))
         (state (e-board--run-set-barrier-state barrier))
         (generation (or generation
                         (e-board-orchestration-run-set-state-generation
                          state)))
         (fence-generation (e-board--run-set-barrier-fence-generation barrier))
         (fenced-p (integerp fence-generation))
         (refresh-complete-p
          (or (e-board--run-set-barrier-query-settled-p barrier)
              (not (e-board--run-set-barrier-await-refresh-p barrier))
              (> generation
                 (e-board--run-set-barrier-start-generation barrier)))))
    (unless (e-request-terminal-p (e-work-handle-lifecycle work))
      (cond
       ((eq (plist-get value :restore-state) 'unavailable)
        (e-work-fail
         work
         (list 'e-board-orchestration-error
               "Board run-set projection became unavailable")))
       (fenced-p
        (cond
         ((> generation fence-generation)
          (if (not (plist-get value :ready-p))
              (e-work-fail
               work
               (list 'e-board-orchestration-error
                     "Board run-set refresh did not become ready"))
            (let ((present-p
                   (e-board--run-set-barrier-run
                    value (e-board--run-set-barrier-run-id barrier))))
              (if (eq (e-board--run-set-barrier-expected-presence barrier)
                      'absent)
                  (if present-p
                      (e-work-fail
                       work
                       (list 'e-board-orchestration-error
                             "Finalized Board run remained active"))
                    (e-work-finish work (copy-tree value t)))
                (if present-p
                    (e-work-finish work (copy-tree value t))
                  (e-work-fail
                   work
                   (list 'e-board-orchestration-error
                         "Board run-set projection omitted the exact run")))))))
         ((e-board--run-set-barrier-query-settled-p barrier)
          (e-work-fail
         work
         (list 'e-board-orchestration-error
                 "Board run-set refresh did not publish a new generation")))))
       ((e-board--run-set-barrier-run
         value (e-board--run-set-barrier-run-id barrier))
        ;; STATE already owns this detached VALUE.  Returning a copy keeps the
        ;; barrier consumer-shaped while compact status and context continue to
        ;; read the identical session-scoped projection.
        (e-work-finish work (copy-tree value t)))
       ((and (plist-get value :ready-p) refresh-complete-p)
        (e-work-fail
         work
         (list 'e-board-orchestration-error
               "Board run-set projection omitted the exact run")))))))

(defun e-board--run-set-barriers-cancel (state)
  "Cancel every active run-set barrier for STATE."
  (dolist (barrier (copy-sequence (gethash state e-board--run-set-barriers)))
    (let ((work (e-board--run-set-barrier-work barrier)))
      (unless (e-request-terminal-p (e-work-handle-lifecycle work))
        (e-work-cancel work))))
  (remhash state e-board--run-set-barriers))

(defun e-board-run-set-generation (binding)
  "Return BINDING's current shared Board run-set generation.

Callers use this detached receipt before starting controller work and pass it
back to `e-board-run-set-await-run-start'.  The receipt is only a fence; Board
continues to own both the projection and the observer-visible generation."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (e-board-orchestration-run-set-state-generation
   (e-board-run-set-state-for
    (e-chat-service-binding-harness binding)
    (e-chat-service-binding-session-id binding)
    (e-chat-service-binding-board-id binding))))

(defun e-board-run-set-await-run-start
    (binding run-id &optional receipt-generation expected-presence)
  "Return cancellable work waiting for exact RUN-ID in BINDING's run-set.

The barrier observes only the existing session-scoped detached Board state.
Without RECEIPT-GENERATION it settles when a ready projection contains RUN-ID
and otherwise waits for the next observer-visible refresh.  With a generation
receipt captured before controller work, it waits for a strictly newer ready
projection and checks EXPECTED-PRESENCE (`present' by default, or `absent'
for a consumed/finalized run).  If a refresh is already active at construction,
the current generation is also a fence so an intermediate value cannot settle
the barrier before the coalesced refresh.  Projection failure fails the barrier
and binding retirement cancels it; no SQL read or polling is added."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (unless (and (stringp run-id) (not (string-empty-p run-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p run-id)))
  (unless (or (null receipt-generation) (integerp receipt-generation))
    (signal 'wrong-type-argument
            (list 'or-null-integer-p receipt-generation)))
  (setq expected-presence (or expected-presence 'present))
  (unless (memq expected-presence '(present absent))
    (signal 'wrong-type-argument
            (list '(member present absent) expected-presence)))
  (let* ((harness (e-chat-service-binding-harness binding))
         (session-id (e-chat-service-binding-session-id binding))
         (state (e-board-run-set-state-for
                 harness session-id
                 (e-chat-service-binding-board-id binding)))
         (controls (e-board--run-set-controls state))
         (query-work (plist-get controls :query-work))
         (query-active-p
          (and (e-work-handle-p query-work)
               (not (e-request-terminal-p
                     (e-work-handle-lifecycle query-work)))))
         (generation
          (e-board-orchestration-run-set-state-generation state))
         (work (e-work-start e-board--run-set-barrier-spec nil))
         (barrier (e-board--run-set-barrier-create
                   :state state :binding binding :run-id (copy-sequence run-id)
                   :work work :start-generation generation
                   :query-work query-work
                   :await-refresh-p (or (integerp receipt-generation)
                                        query-active-p)
                   :query-settled-p (not query-active-p)
                   ;; A receipt fences the controller's first publication.
                   ;; When a refresh is already active, its current state is
                   ;; itself only an intermediate snapshot; fence at least
                   ;; the generation visible at barrier construction so a
                   ;; coalesced final refresh cannot be bypassed.
                   :fence-generation
                   (if query-active-p
                       (max generation (or receipt-generation generation))
                     receipt-generation)
                   :expected-presence expected-presence)))
    (puthash state
             (cons barrier (gethash state e-board--run-set-barriers))
             e-board--run-set-barriers)
    (setf (e-work-handle-cancel-function work)
          (lambda (_handle)
            (e-board--run-set-barrier-remove barrier)))
    (e-work-add-cleanup
     work
     (lambda (_settled)
       (e-board--run-set-barrier-remove barrier)))
    (setf (e-board--run-set-barrier-unsubscribe barrier)
          (e-board-orchestration-run-set-state-subscribe
           state
           (lambda (value value-generation)
             (e-board--run-set-barrier-observe
              barrier value value-generation))))
    (when query-active-p
      (e-work-on-settle
       query-work
       (lambda (settled)
         (e-board--run-set-barrier-query-settled barrier settled))))
    (e-board--run-set-barrier-observe
     barrier (e-board-orchestration-run-set-state-value state) generation)
    (when (eq (e-chat-service-binding-lifecycle-state binding) 'retired)
      (e-work-cancel work))
    work))

(defun e-board-run-set-bind-binding (binding)
  "Start BINDING's initial bounded run-set query and commit wake-up path.
The returned work is the exact durable projection used by the binding
readiness boundary.  The Board layer owns this application service seam; the
chat service only knows that the hook returns asynchronous readiness work."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (let* ((harness (e-chat-service-binding-harness binding))
         (session-id (e-chat-service-binding-session-id binding))
         (state (e-board-run-set-state-for
                 harness session-id
                 (e-chat-service-binding-board-id binding)))
         (existing (e-board--run-set-controls state)))
    (if existing
        (or (plist-get existing :query-work)
            (e-work-start
             (e-work-spec-create
              :id "board-run-set-bound" :execution 'cheap
              :interactive-policy 'async :owner 'board
              :runner (lambda (bound-binding _context) bound-binding))
             binding))
      (let* ((service (e-chat-service-binding-sqlite-service binding))
             (board-id (e-chat-service-binding-board-id binding))
             (controls (list :binding binding :query-work nil :rerun-p nil))
             (observer
              (e-board-sqlite-service-observe-commits
               service board-id
               (lambda ()
                 (when (not (eq (e-chat-service-binding-lifecycle-state binding)
                                'retired))
                   (e-board--run-set-refresh binding state)))))
             (cleanup
              (e-chat-service-binding-register-cleanup
               binding
               (lambda (_binding)
                 (when-let ((current (e-board--run-set-controls state)))
                   ;; Cancel barriers before cancelling a held query.  Query
                   ;; cancellation publishes unavailable; a retiring binding
                   ;; must settle its consumers as cancelled instead of
                   ;; exposing that retirement transition as a query failure.
                   (e-board--run-set-barriers-cancel state)
                   (when-let ((query (plist-get current :query-work)))
                     (unless (memq (plist-get (e-work-status query) :state)
                                   '(finished failed cancelled))
                       (e-work-cancel query)))
                   (when-let ((wake (plist-get current :observer)))
                     (e-board-sqlite-commit-observation-cancel wake))
                   (remhash state e-board--run-set-controls)
                   (remhash binding e-board--run-set-states)
                   (setf (e-board-orchestration-run-set-state-subscribers state)
                         nil))))))
        (setq controls (plist-put controls :observer observer))
        (setq controls (plist-put controls :cleanup cleanup))
        (puthash state controls e-board--run-set-controls)
        (e-board--run-set-query-start binding state controls)))))

(defun e-board-run-set-retire-binding (binding)
  "Retire BINDING's Board run-set state and wake-up subscription."
  (when-let ((state (gethash binding e-board--run-set-states)))
    (when-let ((controls (e-board--run-set-controls state)))
      (when-let ((observer (plist-get controls :observer)))
        (e-board-sqlite-commit-observation-cancel observer))
      ;; See the binding cleanup callback above: retirement is cancellation,
      ;; not an observation failure, even when a query is currently held.
      (e-board--run-set-barriers-cancel state)
      (when-let ((work (plist-get controls :query-work)))
        (unless (memq (plist-get (e-work-status work) :state)
                      '(finished failed cancelled))
          (e-work-cancel work)))
      (remhash state e-board--run-set-controls))
    (setf (e-board-orchestration-run-set-state-subscribers state) nil)
    (remhash binding e-board--run-set-states))
  nil)

(defun e-board-run-set-subscribe (binding callback &optional selected-run-id)
  "Subscribe CALLBACK to BINDING's compact run status.
CALLBACK receives a detached compact status immediately and after each durable
Board refresh.  SELECTED-RUN-ID is used only for the activity-link summary."
  (let* ((state (e-board-run-set-state-for
                 (e-chat-service-binding-harness binding)
                 (e-chat-service-binding-session-id binding)
                 (e-chat-service-binding-board-id binding)))
         (wrapped
          (lambda (value _generation)
            (let* ((runs (plist-get value :runs))
                   (selected (or selected-run-id
                                 (plist-get (car runs) :run-id))))
              (funcall callback
                       (e-board-orchestration-run-set-compact-status
                        state selected))))))
    (funcall wrapped (e-board-orchestration-run-set-state-value state) 0)
    (e-board-orchestration-run-set-state-subscribe state wrapped)))

(defun e-board-run-set-update (harness session-id projections &rest options)
  "Update one session-scoped Board run-set value from PROJECTIONS.
The projections are detached values supplied by the Board SQL application
service; this function does not consult private live state."
  (apply #'e-board-orchestration-run-set-state-update
         (e-board-run-set-state-for
          harness session-id
          (and (e-chat-service-binding harness session-id)
               (e-chat-service-binding-board-id
                (e-chat-service-binding harness session-id))))
         projections options))

(defun e-board-run-set-context-provider (&optional states)
  "Return a context provider reading bounded Board run-set STATES.
STATES may be a function of HARNESS and SESSION-ID; by default the provider
uses `e-board-run-set-state-for' and emits restoring until an application
query installs the first durable value."
  (e-context-provider-create
   :name 'board-run-set
   :priority 215
   :cache-placement 'dynamic-context
   :build
   (cl-function
    (lambda (&key harness session-id turn-id context-purpose)
      (ignore turn-id context-purpose)
      (when (and harness session-id)
        (let* ((binding (e-chat-service-binding harness session-id))
               (board-id (and binding
                              (e-chat-service-binding-board-id binding)))
               (state (if (functionp states)
                          (funcall states harness session-id)
                        (e-board-run-set-state-for harness session-id board-id)))
               (context (e-board-orchestration-run-set-context state)))
          (list
           (list :role 'system
                 :content
                 (format "Board run-set projection (bounded, detached): %s"
                         (e-prin1-safe
                          (plist-get context :projection)))))))))))

(defun e-board-capability-create (&optional states)
  "Create the independent Board observation capability."
  (e-capability-with-skills-create
   :id 'board
   :name "Board"
   :instruction-priority 225
   :instructions e-board-instructions
   :context-providers (list (e-board-run-set-context-provider states))
   :actions (append (e-board-observation-parent-alist)
                    (e-board-orchestration-actions-parent-alist t))
   :skills (list
            (e-skill-spec-create
             :name "board"
             :description "Read durable Board participant activity and run projections."
             :content e-board-skill))))

(defun e-board-layer-create ()
  "Create the independent Board observation layer."
  (e-layer-create
   :id 'board
   :name "Board"
   :requires '(async-control)
   :capabilities (list (e-board-capability-create))))

(defvar e-board--binding-open-hook-unsubscribe
  (e-chat-service-register-binding-open-hook #'e-board-run-set-bind-binding)
  "Unregister function for the Board binding-open application hook.")

(provide 'e-board)

;;; e-board.el ends here

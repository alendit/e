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

(defun e-board--run-set-query-start (binding state controls)
  "Start one bounded durable run-set query for BINDING and STATE."
  (let* ((target (e-chat-service-publication-target binding))
         (child (e-board-orchestration-actions-run-set target))
         ;; Keep the readiness carrier behind the state installation.  The
         ;; SQL action and the generic binding readiness callback can settle
         ;; on the same event-loop turn; mapping here makes the detached
         ;; value visible to status/context before owner readiness observes
         ;; the query as finished.
         (work
          (e-board-orchestration-actions--map-work
           child
           (lambda (value)
             (e-board-orchestration-run-set-state-set-value state value)
             value))))
    (setq controls (plist-put controls :query-work work))
    (puthash state controls e-board--run-set-controls)
    (e-work-on-settle
     work
     (lambda (settled)
       (when (eq (plist-get (e-board--run-set-controls state) :query-work)
                 work)
         (let ((status (e-work-status settled))
               (controls (e-board--run-set-controls state)))
           (setq controls (plist-put controls :query-work nil))
           (puthash state controls e-board--run-set-controls)
           (if (eq (plist-get status :state) 'finished)
               nil
             (e-board-orchestration-run-set-state-update
              state nil :restore-state 'unavailable))
           (when (and (plist-get controls :rerun-p)
                      (e-chat-service-binding-p binding)
                      (not (eq (e-chat-service-binding-lifecycle-state binding)
                               'retired)))
             (setq controls (plist-put controls :rerun-p nil))
             (puthash state controls e-board--run-set-controls)
             (e-board--run-set-query-start binding state controls))))))
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

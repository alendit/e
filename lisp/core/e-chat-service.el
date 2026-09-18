;;; e-chat-service.el --- Board-backed chat application service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell-neutral board client for chat presentation shells.  The harness owns
;; the private transcript and turn endpoint; callers submit and observe only
;; through the board binding created here.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'e-capabilities)
(require 'e-board-orchestration)
(require 'e-board-sqlite-service)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-session)
(require 'e-session-async)
(require 'e-session-board-policy)
(require 'e-session-query)
(require 'e-work)
(require 'seq)
(require 'subr-x)

(defvar e-chat-default-harness-id)

(defgroup e-chat-service nil
  "Shell-neutral chat service operations."
  :group 'e)

(define-error 'e-chat-service-invalid-activity
  "e chat service activity has invalid public attributes")

(defcustom e-chat-service-default-harness-id :chat-default
  "Harness registry id used by shell-neutral default chat commands."
  :type 'symbol
  :group 'e-chat-service)

(defconst e-chat-service-observer-page-limit 16
  "Maximum board messages translated during one chat observer drain.")

(defconst e-chat-service-subscriber-limit 8
  "Maximum presentation subscribers admitted to one chat binding.")

(defconst e-chat-service-sqlite-poll-interval 0.25
  "Seconds between bounded SQLite change queries for a live presentation.")

(defconst e-chat-service-session-summary-page-limit 64
  "Maximum persisted session summaries returned to one presentation request.")

(defconst e-chat-service-projection-capacity 256
  "Maximum immutable board events retained per chat projection category.")

(defcustom e-chat-service-idle-close-delay 300
  "Seconds without a presentation client before an idle board closes."
  :type 'number
  :group 'e-chat-service)

(defconst e-chat-service--curation-activity-count-keys
  '(:kept-source-count :summary-count :summarized-source-count
    :erased-source-count)
  "Required count fields accepted by the curation formatter.")

(defconst e-chat-service--curation-source-kinds
  '("current-state" "dynamic-context" "tool-result" "trace"
    "retrieved-excerpt")
  "Public semantic source kinds accepted in curation stubs.")

(defconst e-chat-service--public-live-harness-event-types
  '(turn-started provider-request-started provider-request-finished
    turn-retrying reasoning-delta tool-started tool-finished
    action-started action-finished action-failed hook-audit turn-steered
    assistant-delta backend-empty-output token-usage provider-anchor-candidate
    turn-summary compaction-started compaction-prepared
    compaction-summary-started compaction-finished compaction-failed
    queue-changed session-reset tool-progress)
  "Harness events safe and useful for request-local live presentation.

This is deliberately fail-closed.  Durable audit events and unknown event
types never cross the chat presentation boundary merely because the harness
emitted them.")

(defconst e-chat-service--public-hook-audit-keys
  '(:owner :hook-id :outcome :truth-status :summary :pending-summary)
  "Generic hook status fields allowed to cross the presentation boundary.")

(defun e-chat-service--public-live-harness-event (event)
  "Return a presentation-safe copy of allowlisted harness EVENT."
  (let ((copy (copy-tree event t)))
    (when (eq (plist-get copy :type) 'hook-audit)
      (let ((payload (plist-get copy :payload)))
        (setq copy
              (plist-put
               copy :payload
               (cl-loop for key in e-chat-service--public-hook-audit-keys
                        when (plist-member payload key)
                        append (list key (copy-tree (plist-get payload key)
                                                    t)))))))
    copy))

(defun e-chat-service--curation-source-stub (stub)
  "Return validated content-free curation source STUB."
  (unless (and (proper-list-p stub) (zerop (% (length stub) 2)))
    (signal 'e-chat-service-invalid-activity
            (list 'context-curated :source-stub-shape stub)))
  (let ((tail stub)
        keys)
    (while tail
      (push (pop tail) keys)
      (pop tail))
    (unless (and (= (length keys) (length (delete-dups (copy-sequence keys))))
                 (cl-every (lambda (key)
                             (memq key '(:disposition :source-kind :tool-name)))
                           keys)
                 (plist-member stub :disposition)
                 (plist-member stub :source-kind))
      (signal 'e-chat-service-invalid-activity
              (list 'context-curated :source-stub-keys (nreverse keys)))))
  (let ((disposition (plist-get stub :disposition))
        (source-kind (plist-get stub :source-kind))
        (tool-name (and (plist-member stub :tool-name)
                        (plist-get stub :tool-name))))
    (unless (and (memq disposition '(kept summarized erased))
                 (member source-kind e-chat-service--curation-source-kinds)
                 (or (not (plist-member stub :tool-name))
                     (and (equal source-kind "tool-result")
                          (stringp tool-name)
                          (not (string-empty-p tool-name))
                          (not (string-match-p "[[:cntrl:]]" tool-name))
                          (<= (length tool-name) 256))))
      (signal 'e-chat-service-invalid-activity
              (list 'context-curated :source-stub stub)))
    (append (list :disposition disposition
                  :source-kind (copy-sequence source-kind))
            (when tool-name (list :tool-name (copy-sequence tool-name))))))

(defun e-chat-service--curation-counts (projection)
  "Return validated curation counts and source stubs from public PROJECTION."
  (unless (and (proper-list-p projection)
               (zerop (% (length projection) 2)))
    (signal 'e-chat-service-invalid-activity
            (list 'context-curated :shape projection)))
  (let ((tail projection)
        keys)
    (while tail
      (let ((key (pop tail)))
        (unless (and (keywordp key) tail)
          (signal 'e-chat-service-invalid-activity
                  (list 'context-curated :shape projection)))
        (push key keys)
        (pop tail)))
    (unless (and (= (length keys) (length (delete-dups (copy-sequence keys))))
                 (cl-every
                  (lambda (key)
                    (memq key
                          (append e-chat-service--curation-activity-count-keys
                                  '(:source-stubs))))
                  keys)
                 (cl-every (lambda (key) (plist-member projection key))
                           e-chat-service--curation-activity-count-keys))
      (signal 'e-chat-service-invalid-activity
              (list 'context-curated :keys (nreverse keys))))
    (let ((kept (plist-get projection :kept-source-count))
          (summaries (plist-get projection :summary-count))
          (summarized (plist-get projection :summarized-source-count))
          (erased (plist-get projection :erased-source-count))
          (stubs-present-p (plist-member projection :source-stubs))
          (stubs (and (plist-member projection :source-stubs)
                      (plist-get projection :source-stubs))))
      (unless (and (cl-every (lambda (value)
                               (and (integerp value) (>= value 0)))
                             (list kept summaries summarized erased))
                   (or (> kept 0) (> summarized 0) (> erased 0))
                   (eq (= summaries 0) (= summarized 0))
                   (<= summaries summarized))
        (signal 'e-chat-service-invalid-activity
                (list 'context-curated :counts projection)))
      (when stubs-present-p
        (unless (and (proper-list-p stubs) stubs
                     (<= (length stubs) 16))
          (signal 'e-chat-service-invalid-activity
                  (list 'context-curated :source-stubs stubs)))
        (setq stubs (mapcar #'e-chat-service--curation-source-stub stubs))
        (unless (and (= kept
                        (seq-count (lambda (stub)
                                     (eq (plist-get stub :disposition) 'kept))
                                   stubs))
                     (= summarized
                        (seq-count
                         (lambda (stub)
                           (eq (plist-get stub :disposition) 'summarized))
                         stubs))
                     (= erased
                        (seq-count (lambda (stub)
                                     (eq (plist-get stub :disposition) 'erased))
                                   stubs)))
          (signal 'e-chat-service-invalid-activity
                  (list 'context-curated :source-stub-counts projection))))
      (list kept summaries summarized erased stubs))))

(defun e-chat-service--public-curation-projection (projection)
  "Return validated content-free public curation PROJECTION.

Private frame, response, provider, and activity identities are not accepted
by this projection boundary."
  (pcase-let ((`(,kept ,summaries ,summarized ,erased ,stubs)
               (e-chat-service--curation-counts projection)))
    (append
     (list :kept-source-count kept
           :summary-count summaries
           :summarized-source-count summarized
           :erased-source-count erased)
     (when stubs (list :source-stubs (copy-tree stubs t))))))

(defun e-chat-service--curation-source-stub-label (stub)
  "Return compact human-readable label for curation source STUB."
  (pcase (plist-get stub :source-kind)
    ("tool-result"
     (let ((name (plist-get stub :tool-name)))
       (if name (format "tool output · %s" name) "tool output")))
    ("current-state" "current state")
    ("dynamic-context" "dynamic context")
    ("trace" "trace")
    ("retrieved-excerpt" "retrieved excerpt")))

(defun e-chat-service--curation-source-description (stubs disposition)
  "Describe STUBS having DISPOSITION with stable first-seen grouping."
  (let ((counts (make-hash-table :test #'equal))
        order)
    (dolist (stub stubs)
      (when (eq (plist-get stub :disposition) disposition)
        (let ((label (e-chat-service--curation-source-stub-label stub)))
          (unless (gethash label counts)
            (setq order (append order (list label))))
          (puthash label (1+ (gethash label counts 0)) counts))))
    (when order
      (mapconcat
       (lambda (label)
         (let ((count (gethash label counts)))
           (if (> count 1) (format "%s ×%d" label count) label)))
       order ", "))))

(defun e-chat-service--curation-line (text stubs disposition)
  "Append a safe source breakdown to curation line TEXT when available."
  (let ((description
         (e-chat-service--curation-source-description stubs disposition)))
    (if description (format "%s — %s" text description) text)))

(defun e-chat-service-format-context-curation (projection)
  "Format content-free public context-curation PROJECTION for chat shells."
  (pcase-let ((`(,kept ,summaries ,summarized ,erased ,stubs)
               (e-chat-service--curation-counts projection)))
    (string-join
     (delq nil
           (list
            (and (> kept 0)
                 (e-chat-service--curation-line
                  (format "kept %d" kept) stubs 'kept))
            (and (> summarized 0)
                 (e-chat-service--curation-line
                  (format "summarized %d source%s into %d summar%s"
                          summarized (if (= summarized 1) "" "s")
                          summaries (if (= summaries 1) "y" "ies"))
                  stubs 'summarized))
            (and (> erased 0)
                 (e-chat-service--curation-line
                  (format "erased %d" erased) stubs 'erased))))
     (if stubs "\n" " · "))))

(cl-defstruct (e-chat-service-binding
               (:constructor e-chat-service--binding-create))
  harness session-id subscribers default-tags default-to idle-close-timer
  lifecycle-generation lifecycle-state readiness-work first-persistence-error
  cleanup-callbacks
  continuation-owner-p
  sqlite-service board-id principal participant-id participant-name
  endpoint-token endpoint-generation
  turn-port activity-subscription pickup-subscription
  pickup-readiness-wakeup-p executing-turns)

(defvar e-chat-service--binding-open-hooks nil
  "Application-owned hooks invoked after a live Board binding is installed.

Each hook receives the detached coordination-only binding and may return one
`e-work' handle whose settlement is part of the binding readiness boundary.
The core service owns only this generic extension seam; Board-specific policy
stays in the Board application layer.")

(defun e-chat-service-register-binding-open-hook (function)
  "Register FUNCTION as a generic binding-open readiness hook.
Return an idempotent unregister function.  FUNCTION must return nil or an
`e-work' handle when called with a live binding."
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (push function e-chat-service--binding-open-hooks)
  (let ((active-p t))
    (lambda ()
      (when active-p
        (setq active-p nil)
        (setq e-chat-service--binding-open-hooks
              (delq function e-chat-service--binding-open-hooks))))))

(defun e-chat-service-binding-register-cleanup (binding callback)
  "Register CALLBACK for BINDING retirement and return an unsubscribe thunk."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (if (eq (e-chat-service-binding-lifecycle-state binding) 'retired)
      (funcall callback binding)
    (push callback (e-chat-service-binding-cleanup-callbacks binding)))
  (let ((active-p t))
    (lambda ()
      (when active-p
        (setq active-p nil)
        (setf (e-chat-service-binding-cleanup-callbacks binding)
              (delq callback
                    (e-chat-service-binding-cleanup-callbacks binding)))))))

(defconst e-chat-service--binding-ready-spec
  (e-work-spec-create
   :id "chat-board-ready" :execution 'cooperative :interactive-policy 'async
   :owner 'e-chat-service
   :runner
   (lambda (parent arguments _context)
     (let ((child (plist-get arguments :child))
           (binding (plist-get arguments :binding)))
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
            ('finished (e-work-finish parent binding))
            ('cancelled (e-work-cancel parent))
            ('failed (e-work-fail parent (e-work-handle-error settled))))))
       :deferred)))
  "Map one binding readiness child to the binding value consumers expect.")

(defconst e-chat-service--binding-readiness-set-spec
  (e-work-spec-create
   :id "chat-board-readiness-set" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat-service
   :runner
   (lambda (parent arguments _context)
     (let* ((children (plist-get arguments :children))
            (binding (plist-get arguments :binding))
            (cancel-set nil))
       (setf cancel-set
             (e-work-await-set
              children :mode 'all
              :on-settle
              (lambda (settled-set)
                (let ((failed
                       (seq-find
                        (lambda (child)
                          (memq (plist-get (e-work-status child) :state)
                                '(failed cancelled)))
                        (plist-get settled-set :done))))
                  (if failed
                      (if (eq (plist-get (e-work-status failed) :state)
                              'cancelled)
                          (e-work-cancel parent)
                        (e-work-fail parent
                                      (or (e-work-handle-error failed)
                                          '(e-chat-service-error
                                            "binding readiness failed"))))
                    (e-work-finish parent binding))))))
       (setf (e-work-handle-cancel-function parent)
             (lambda (_handle)
               (when cancel-set (funcall cancel-set))
               (dolist (child children)
                 (when (and (e-work-handle-p child)
                            (not (memq (plist-get (e-work-status child) :state)
                                       '(finished failed cancelled))))
                   (e-work-cancel child)))))
       :deferred)))
  "Join generic binding-open readiness children without blocking callers.")

(defconst e-chat-service--binding-readiness-failure-spec
  (e-work-spec-create
   :id "chat-board-readiness-failure" :execution 'cheap
   :interactive-policy 'async :owner 'e-chat-service
   :runner
   (lambda (parent arguments _context)
     (e-work-fail parent (plist-get arguments :error)))))

(defun e-chat-service--binding-open-readiness-start (binding)
  "Run generic binding-open hooks for BINDING and return readiness work."
  (let (works first-error)
    (dolist (hook (copy-sequence e-chat-service--binding-open-hooks))
      (condition-case error
          (let ((work (funcall hook binding)))
            (if (null work)
                nil
              (unless (e-work-handle-p work)
                (signal 'wrong-type-argument (list 'e-work-handle-p work)))
              (push work works)))
        (error (unless first-error (setq first-error error)))))
    (cond
     (first-error
      (dolist (work works)
        (unless (memq (plist-get (e-work-status work) :state)
                      '(finished failed cancelled))
          (e-work-cancel work)))
      (e-work-start e-chat-service--binding-readiness-failure-spec
                    (list :error first-error)))
     ((null works) nil)
     ((null (cdr works)) (car works))
     (t
      (e-work-start e-chat-service--binding-readiness-set-spec
                    (list :children (nreverse works) :binding binding))))))

(defun e-chat-service--binding-ready-work (binding)
  "Return work that settles to BINDING after its readiness child settles."
  (let ((readiness (e-chat-service-binding-readiness-work binding)))
    (if (or (null readiness)
            (eq (plist-get (e-work-status readiness) :state) 'finished))
        (e-work-start
         (e-work-spec-create
          :id "chat-board-bound" :execution 'cheap :interactive-policy 'async
          :owner 'e-chat-service
          :runner (lambda (bound-binding _context) bound-binding))
         binding)
      (e-work-start e-chat-service--binding-ready-spec
                    (list :child readiness :binding binding)))))

(defun e-chat-service--binding-readiness-state (binding)
  "Return BINDING's request-local readiness state.
The state deliberately treats a missing readiness child as ready: child
participant bindings do not own the Board run-set capability.  A failed or
cancelled child is unavailable even while the binding's retirement callback is
still pending, so no new pickup can be claimed through it."
  (cond
   ((not (e-chat-service--binding-live-p binding)) 'unavailable)
   ((null (e-chat-service-binding-readiness-work binding)) 'ready)
   ((eq (plist-get
         (e-work-status (e-chat-service-binding-readiness-work binding))
         :state)
        'finished)
    'ready)
   ((memq (plist-get
           (e-work-status (e-chat-service-binding-readiness-work binding))
           :state)
         '(failed cancelled))
    'unavailable)
   (t 'waiting)))

(defun e-chat-service--binding-reusable-p (binding)
  "Return non-nil when BINDING can satisfy another open request."
  (eq (e-chat-service--binding-readiness-state binding) 'ready))

(defun e-chat-service--participant-name (metadata role)
  "Return the bounded display name for participant METADATA and ROLE."
  (let ((name (or (plist-get metadata :participant-name)
                  (plist-get metadata :subagent-label)
                  (plist-get metadata :name))))
    (cond
     ((and (stringp name) (not (string-empty-p name)))
      (copy-sequence name))
     ((eq role 'owner) "Main")
     (t nil))))

(cl-defstruct (e-chat-service-create-operation
               (:constructor e-chat-service--create-operation-create))
  work harness store session-id metadata admission first-input-work
  owner-admission-work settled creation-key board-id participant-id)

(cl-defstruct (e-chat-service-owner-admission-ticket
               (:constructor e-chat-service--owner-admission-ticket-create))
  "Request-scoped stable-key owner admission ticket.

The ticket deliberately exposes only the deterministic identity and the
request work.  It is not an action result and contains no durable aggregate or
live binding; callers must wait for WORK's explicit settlement before using
the Board id as an address."
  creation-key board-id session-id participant-id work)

(cl-defstruct (e-chat-service-participant-operation
               (:constructor e-chat-service--participant-operation-create))
  "One async admission of a private session into an already-live SQL Board."
  work parent-binding harness store session-id metadata participant-id pickup-selector
  observer-selector default-tags default-to binding session-result child settled)

(cl-defstruct (e-chat-service-bind-operation
               (:constructor e-chat-service--bind-operation-create))
  "One request-local composition of association query and Board controller."
  work harness session-id association continuation-owner-p prerequisite
  child settled)

(cl-defstruct (e-chat-service-open-operation
               (:constructor e-chat-service--open-operation-create))
  "One detached validation and binding of an existing SQLite Board session."
  work target harness session-id routing-arguments child settled)

(cl-defstruct (e-chat-service-owner-open-operation
               (:constructor e-chat-service--owner-open-operation-create))
  "One exact Board-to-owner resolution and live binding request."
  work board-id harness child binding-work association settled)

(cl-defstruct (e-chat-service-legacy-resolve-operation
               (:constructor e-chat-service--legacy-resolve-operation-create))
  "One request-local legacy session association migration resolution."
  work harness session-id child association settled)

(defconst e-chat-service--create-operation-spec
  (e-work-spec-create
   :id "chat-session-create" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat-service
   :runner
   (lambda (_work operation _context)
     (e-chat-service--start-create-operation operation)
     :deferred)))

(defconst e-chat-service--participant-operation-spec
  (e-work-spec-create
   :id "chat-participant-create" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat-service
   :runner
   (lambda (_work operation _context)
     (e-chat-service--start-participant-operation operation)
     :deferred)))

(defconst e-chat-service--open-operation-spec
  (e-work-spec-create
   :id "chat-board-open" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat-service
   :runner
   (lambda (_handle operation _context)
     (e-chat-service--start-open-operation operation)
     :deferred))
  "Work contract for one existing SQLite Board-session open.")

(defconst e-chat-service--owner-open-operation-spec
  (e-work-spec-create
   :id "chat-board-owner-open" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat-service
   :runner
   (lambda (_handle operation _context)
     (e-chat-service--start-owner-open-operation operation)
     :deferred)))

(defconst e-chat-service--legacy-resolve-operation-spec
  (e-work-spec-create
   :id "chat-legacy-session-resolve" :execution 'cooperative
   :interactive-policy 'async :owner 'e-chat-service
   :runner
   (lambda (_handle operation _context)
     (e-chat-service--start-legacy-resolve-operation operation)
     :deferred)))

(defun e-chat-service-owner-admission-identities (creation-key)
  "Return deterministic owner identities derived from CREATION-KEY.

CREATION-KEY is the application-owned identity of a document or other
consumer resource.  The derivation uses no clock, process counter, or live
runtime state, so retries and a later process produce the same private
session, Board, participant, and principal tuple."
  (unless (and (stringp creation-key)
               (not (string-empty-p creation-key))
               (<= (string-bytes creation-key) 4096))
    (signal 'e-session-error
            (list "Owner creation key must be a bounded non-empty string"
                  creation-key)))
  (let ((digest (secure-hash 'sha256
                             (concat "e-board-owner\0" creation-key))))
    (list :creation-key (copy-sequence creation-key)
          :session-id (concat "ses_" (substring digest 0 32))
          :board-id (concat "brd_" (substring digest 32 64))
          :participant-id (concat "ptc_" (substring digest 0 32))
          :principal (concat "chat:ses_" (substring digest 0 32)))))

(defun e-chat-service-owner-admission-work (ticket)
  "Return TICKET's request-scoped admission work."
  (unless (e-chat-service-owner-admission-ticket-p ticket)
    (signal 'wrong-type-argument
            (list 'e-chat-service-owner-admission-ticket-p ticket)))
  (e-chat-service-owner-admission-ticket-work ticket))

(defun e-chat-service-owner-admission-provisional-board-id (ticket)
  "Return TICKET's deterministic provisional Board id."
  (unless (e-chat-service-owner-admission-ticket-p ticket)
    (signal 'wrong-type-argument
            (list 'e-chat-service-owner-admission-ticket-p ticket)))
  (copy-sequence (e-chat-service-owner-admission-ticket-board-id ticket)))

(cl-defun e-chat-service-board-fact-start
    (binding &key author tags attributes content source-fact-key)
  "Append one Board fact through BINDING's asynchronous SQL service."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (e-board-sqlite-service-record-append-start
   (e-chat-service-binding-sqlite-service binding)
   (e-chat-service-binding-board-id binding)
   'fact 'fact source-fact-key
   :author (copy-tree author t) :tags (copy-tree tags t)
   :attributes (copy-tree attributes t) :content content))

(defun e-chat-service-publication-target (binding)
  "Return BINDING's detached SQL Board publication address.
The returned value retains only the live SQL service, durable Board id,
and immutable transaction defaults.  The service may retain bounded live
pickup/outcome callbacks, but the target does not retain BINDING, its harness,
presentation subscribers, executing turns, or any Board aggregate."
  (unless (and (e-chat-service-binding-p binding)
               (e-chat-service--sql-binding-p binding))
    (signal 'wrong-type-argument (list 'sqlite-chat-binding-p binding)))
  (e-board-sqlite-publication-target-create
   (e-chat-service-binding-sqlite-service binding)
   (e-chat-service-binding-board-id binding)
   :author (format "session:%s" (e-chat-service-binding-session-id binding))))

(cl-defstruct (e-chat-service-subscription
               (:constructor e-chat-service--subscription-create))
  binding function active-p drain-scheduled state drain-timer
  lifecycle-generation sqlite-cursor sqlite-query-work
  sqlite-rerun-p)

(defvar e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key)
  "Board-backed chat bindings, first by harness identity then session id.")

(defvar e-chat-service--board-bindings
  (make-hash-table :test 'eq :weakness 'key)
  "Live chat bindings by runtime identity, then durable Board id.")

(defvar e-chat-service--binding-works
  (make-hash-table :test 'eq :weakness 'key)
  "In-flight bounded binding work, first by harness then session id.")

(defvar e-chat-service--pending-creations
  (make-hash-table :test 'eq :weakness 'key)
  "Bounded first-input admissions, first by harness then session identity.")

(defun e-chat-service--harness-pending-creations (harness)
  "Return HARNESS's bounded pending first-input table."
  (or (gethash harness e-chat-service--pending-creations)
      (puthash harness (make-hash-table :test 'equal)
               e-chat-service--pending-creations)))

(defvar e-chat-service--continuation-reconciling
  (make-hash-table :test 'eq :weakness 'key)
  "Reconciliation markers by runtime identity, then durable Board id.")

(defvar e-chat-service--continuation-admissions
  (make-hash-table :test 'eq :weakness 'key)
  "Continuation admission works by runtime identity, Board id, and key.")

(defun e-chat-service--runtime-coordination-table
    (registry runtime &optional create)
  "Return REGISTRY's Board-keyed coordination table for RUNTIME.
Create and register the table when CREATE is non-nil.  Runtime identity is
compared with `eq'; equal Board ids in distinct databases never share live
coordination."
  (or (gethash runtime registry)
      (when create
        (let ((table (make-hash-table :test 'equal)))
          (puthash runtime table registry)
          table))))

(defun e-chat-service--runtime-coordination-prune (registry runtime)
  "Remove RUNTIME's empty inner coordination table from REGISTRY."
  (when-let ((table (gethash runtime registry)))
    (when (zerop (hash-table-count table))
      (remhash runtime registry))))

(defun e-chat-service--binding-runtime (binding)
  "Return BINDING's authoritative SQLite runtime identity."
  (e-board-sqlite-service-runtime
   (e-chat-service-binding-sqlite-service binding)))

(defun e-chat-service--board-bindings-for (binding)
  "Return live bindings sharing BINDING's runtime and Board identity."
  (when-let ((table
              (e-chat-service--runtime-coordination-table
               e-chat-service--board-bindings
               (e-chat-service--binding-runtime binding))))
    (gethash (e-chat-service-binding-board-id binding) table)))

(defun e-chat-service--register-board-binding (binding)
  "Register live BINDING under its runtime-scoped durable Board identity."
  (let* ((runtime (e-chat-service--binding-runtime binding))
         (board-id (e-chat-service-binding-board-id binding))
         (table
          (e-chat-service--runtime-coordination-table
           e-chat-service--board-bindings runtime t)))
    (puthash board-id (cons binding (gethash board-id table)) table)
    binding))

(defun e-chat-service--unregister-board-binding (binding)
  "Remove BINDING from its runtime-scoped live Board coordination."
  (let* ((runtime (e-chat-service--binding-runtime binding))
         (board-id (e-chat-service-binding-board-id binding))
         (table
          (e-chat-service--runtime-coordination-table
           e-chat-service--board-bindings runtime)))
    (when table
      (let ((remaining (delq binding (gethash board-id table))))
        (if remaining
            (puthash board-id remaining table)
          (remhash board-id table))
        (e-chat-service--runtime-coordination-prune
         e-chat-service--board-bindings runtime)))
    binding))

(defun e-chat-service--publish-sqlite-continuation-claim
    (binding run-id publication-key status &optional error)
  "Append RUN-ID's continuation STATUS through SQLite BINDING."
  (e-board-sqlite-service-orchestration-fact-start
   (e-chat-service-binding-sqlite-service binding)
   (e-chat-service-binding-board-id binding)
   (list :version e-board-orchestration-fact-version
         :type 'continuation-claim
         :idempotency-key
         (e-board-orchestration-continuation-claim-key publication-key status)
         :payload
         (append (list :run-id run-id :publication-key publication-key
                       :status status)
                 (when error (list :error error))))))

(defun e-chat-service--watch-sqlite-continuation-admission
    (binding run-id publication-key work)
  "Publish canonical settlement for continuation admission WORK."
  (let* ((runtime (e-chat-service--binding-runtime binding))
         (board-id (e-chat-service-binding-board-id binding))
         (admissions
          (e-chat-service--runtime-coordination-table
           e-chat-service--continuation-admissions runtime t))
         (admission-key (cons board-id publication-key)))
    (puthash admission-key work admissions)
    (e-work-on-settle
     work
     (lambda (settled)
       (let* ((status (e-work-status settled))
              (state (plist-get status :state))
              (claim
               (condition-case error
                   (e-chat-service--publish-sqlite-continuation-claim
                    binding run-id publication-key
                    (if (eq state 'finished) 'published 'failed)
                    (pcase state
                      ('cancelled "Continuation admission cancelled")
                      ('failed (e-work-error-message
                               (plist-get status :error)))))
                 (error
                  (e-chat-service--sql-note-failure binding error)
                  nil))))
         ;; Keep the admission key occupied through the durable claim write.
         ;; Removing it before that write settles lets a coalesced Board event
         ;; enqueue the same continuation a second time.
         (if (and (e-work-handle-p claim)
                  (eq (gethash admission-key admissions) settled))
             (progn
               (puthash admission-key claim admissions)
               (e-work-on-settle
                claim
                (lambda (claim-settled)
                  (when (eq (gethash admission-key admissions) claim)
                    (if (eq state 'finished)
                        ;; Once the continuation ran, neither a stale read nor
                        ;; a failed claim write may enqueue it again in this
                        ;; process.  A later durable published observation can
                        ;; release either bounded sentinel; process restart
                        ;; falls back to the session input idempotency key.
                        (puthash
                         admission-key
                         (if (eq (plist-get (e-work-status claim-settled) :state)
                                 'finished)
                             'published-awaiting-observation
                           'publication-failed-awaiting-observation)
                         admissions)
                      (remhash admission-key admissions)
                      (e-chat-service--runtime-coordination-prune
                       e-chat-service--continuation-admissions runtime))))))
           (when (eq (gethash admission-key admissions) settled)
             (remhash admission-key admissions)
             (e-chat-service--runtime-coordination-prune
              e-chat-service--continuation-admissions runtime))))))
    work))

(defun e-chat-service--sqlite-orchestration-projections (page)
  "Reduce detached orchestration records in PAGE by run id."
  (when (plist-get page :truncated)
    (signal 'e-board-orchestration-error
            (list "Continuation query exceeds bounded run page")))
  (let ((groups (make-hash-table :test 'equal)) order)
    (dolist (row (plist-get page :records))
      (let* ((record (plist-get row :record))
             (fact (e-board-orchestration-fact-from-record record))
             (run-id (and fact (plist-get (plist-get fact :payload) :run-id))))
        (when run-id
          (puthash run-id (append (gethash run-id groups) (list record)) groups)
          (when (and (eq (plist-get fact :type) 'manifest)
                     (not (member run-id order)))
            (push run-id order)))))
    (mapcar (lambda (run-id)
              (e-board-orchestration-reduce (gethash run-id groups)))
            (nreverse order))))

(defun e-chat-service--continuation-input (prompt projection)
  "Append PROJECTION's detached terminal view to continuation PROMPT."
  (format
   (concat "%s\n\n"
           "Terminal Board projection (already queried; consume this bounded "
           "value directly and do not query or poll run status):\n\n"
           "```elisp\n%S\n```")
   prompt
   (e-board-orchestration-continuation-view projection)))

(defun e-chat-service--reconcile-sqlite-continuation (binding)
  "Query terminal continuations through BINDING and route to their targets.

BINDING owns the bounded Board query and its process-local admission fence;
the manifest continuation's validated session id owns delivery of the
resulting input."
  (let* ((runtime (e-chat-service--binding-runtime binding))
         (board-id (e-chat-service-binding-board-id binding))
         (reconciling
          (e-chat-service--runtime-coordination-table
           e-chat-service--continuation-reconciling runtime t)))
    (if (gethash board-id reconciling)
        ;; A terminal commit landed after the active query was submitted.  One
        ;; rerun is sufficient to observe every coalesced commit because the
        ;; query reads the durable Board facts again after this query settles.
        (puthash board-id 'rerun reconciling)
      (puthash board-id 'running reconciling)
      (let ((query
             (e-board-sqlite-service-orchestration-runs-start
              (e-chat-service-binding-sqlite-service binding) board-id 32)))
        (e-work-on-settle
         query
         (lambda (settled)
           (let ((rerun-p (eq (gethash board-id reconciling) 'rerun)))
             (remhash board-id reconciling)
             (e-chat-service--runtime-coordination-prune
              e-chat-service--continuation-reconciling runtime)
             (when (eq (plist-get (e-work-status settled) :state) 'finished)
               (condition-case error
                   (dolist (projection
                            (e-chat-service--sqlite-orchestration-projections
                             (e-work-handle-result settled)))
                     (let* ((continuation (plist-get projection :continuation))
                            (key (and continuation
                                      (plist-get continuation :publication-key)))
                            (admission-key (and key (cons board-id key)))
                            (admissions
                             (and key
                                  (e-chat-service--runtime-coordination-table
                                   e-chat-service--continuation-admissions
                                   runtime))))
                       (if (eq (plist-get continuation :state) 'published)
                           ;; The durable claim now fences every later process;
                           ;; release this process-local race sentinel.
                           (when (and admissions
                                      (gethash admission-key admissions))
                             (remhash admission-key admissions)
                             (e-chat-service--runtime-coordination-prune
                              e-chat-service--continuation-admissions runtime))
                         (when (and (plist-get projection :terminal-status)
                                    continuation
                                    (not (and admissions
                                              (gethash admission-key admissions))))
                           (e-chat-service--watch-sqlite-continuation-admission
                            binding (plist-get projection :run-id) key
                            (e-chat-service-queue-session
                             (e-chat-service-binding-harness binding)
                             (plist-get continuation :session-id)
                             (e-chat-service--continuation-input
                              (plist-get continuation :prompt)
                              projection)
                             :metadata
                             (list :display 'hidden
                                   :board-run-id (plist-get projection :run-id)
                                   :board-continuation-key key)
                             :source-input-key
                             (list "orchestration-continuation" key 0)))))))
                 (error
                  (e-chat-service--sql-note-failure binding error))))
             (when (and rerun-p (e-chat-service--binding-live-p binding))
               (e-chat-service--reconcile-sqlite-continuation binding)))))))))

(defun e-chat-service--reconcile-binding-continuation (binding)
  "Reconcile continuations through BINDING's SQL service."
  (e-chat-service--reconcile-sqlite-continuation binding))

(defun e-chat-service-reconcile-sqlite-continuation-target (target)
  "Reconcile TARGET's terminal runs through its live continuation owner.

TARGET is a detached SQL publication address.  This lookup retains no Board
or session aggregate: it only locates an already-live binding for the same
runtime and durable Board id, then starts a bounded SQLite reconciliation."
  (e-board-sqlite-publication-target--require target)
  (let* ((service (e-board-sqlite-publication-target--service target))
         (runtime (e-board-sqlite-service-runtime service))
         (board-id (e-board-sqlite-publication-target--board-id target))
         (table
          (e-chat-service--runtime-coordination-table
           e-chat-service--board-bindings runtime)))
    (dolist (binding (and table (copy-sequence (gethash board-id table))))
      (when (and (e-chat-service--binding-live-p binding)
                 (e-chat-service-binding-continuation-owner-p binding))
        (e-chat-service--reconcile-binding-continuation binding)))))

(defun e-chat-service--harness-bindings (harness)
  "Return the session binding table owned by HARNESS."
  (or (gethash harness e-chat-service--bindings)
      (puthash harness (make-hash-table :test 'equal)
               e-chat-service--bindings)))

(defun e-chat-service--harness-binding-works (harness)
  "Return HARNESS's request-local binding-work table."
  (or (gethash harness e-chat-service--binding-works)
      (puthash harness (make-hash-table :test 'equal)
               e-chat-service--binding-works)))

(defun e-chat-service--binding-live-p (binding)
  "Return non-nil while SQL BINDING owns live process coordination."
  (and (e-chat-service-binding-p binding)
       (e-board-sqlite-service-p
        (e-chat-service-binding-sqlite-service binding))
       (not (memq (e-chat-service-binding-lifecycle-state binding)
                  '(retiring retired)))))

(defun e-chat-service--sql-binding-p (binding)
  "Return non-nil when BINDING is coordination-only SQLite state."
  (and (e-chat-service-binding-p binding)
       (e-board-sqlite-service-p
        (e-chat-service-binding-sqlite-service binding))))

(defun e-chat-service--sql-message-event (binding message)
  "Translate detached canonical SQLite MESSAGE for BINDING."
  (let* ((kind (or (plist-get message :kind)
                   (plist-get message :record-kind)))
         (message-id (plist-get message :id))
         (turn-id (if (eq kind 'input)
                      message-id
                    (or (plist-get message :source-turn-id) message-id)))
         (selected-p
          (or (eq kind 'input)
              (equal (plist-get message :subject-participant-id)
                     (e-chat-service-binding-participant-id binding))))
         (identity
          (list :canonical-row-p t
                :board-id (plist-get message :board-id)
                :board-seq (plist-get message :seq)
                :created-at (plist-get message :created-at)
                :message-id message-id :board-kind kind
                :tags (copy-tree (plist-get message :tags) t)
                :attributes (copy-tree (plist-get message :attributes) t)
                :subject-participant-id
                (plist-get message :subject-participant-id)
                :participant-name (plist-get message :participant-name)
                :selected-participant-p selected-p
                :source-turn-id (plist-get message :source-turn-id))))
    (pcase kind
      ('input
       (append identity
               (list :type 'message-added
                     :session-id (e-chat-service-binding-session-id binding)
                     :turn-id turn-id
                     :payload
                     (list :message
                           (list :id message-id :role 'user
                                 :content (plist-get message :content)
                                 :turn-id turn-id
                                 :created-at (plist-get message :created-at)
                                 :references (plist-get message :reference)
                                 :metadata
                                 (copy-tree (plist-get message :attributes) t)
                                 :board-id (plist-get message :board-id)
                                 :board-seq (plist-get message :seq)
                                 :selected-participant-p t)))))
      ('output
       (append identity
               (list :type 'message-added
                     :session-id (e-chat-service-binding-session-id binding)
                     :turn-id turn-id
                     :payload
                     (list :message
                           (list :id message-id :role 'assistant
                                 :content (plist-get message :content)
                                 :terminal-output t :turn-id turn-id
                                 :created-at (plist-get message :created-at)
                                 :metadata
                                 (copy-tree (plist-get message :attributes) t)
                                 :board-id (plist-get message :board-id)
                                 :board-seq (plist-get message :seq)
                                 :subject-participant-id
                                 (plist-get message :subject-participant-id)
                                 :participant-name
                                 (plist-get message :participant-name)
                                 :source-turn-id
                                 (plist-get message :source-turn-id)
                                 :selected-participant-p selected-p)))))
      ('activity
       (when-let* ((activity-kind (plist-get message :activity-kind))
                   ((eq activity-kind 'context-curated)))
         (append identity
                 (list :type activity-kind
                       :session-id
                       (e-chat-service-binding-session-id binding)
                       :turn-id turn-id
                       :payload
                       (copy-tree (plist-get message :attributes) t)))))
      (_ nil))))

(defun e-chat-service--sql-deliver-event (binding event)
  "Deliver detached EVENT directly to BINDING's live subscribers."
  (dolist (subscription
           (copy-sequence (e-chat-service-binding-subscribers binding)))
    (when (e-chat-service-subscription-active-p subscription)
      (condition-case error
          (funcall (e-chat-service-subscription-function subscription)
                   (copy-tree event t))
        (error
         (setf (e-chat-service-subscription-state subscription)
               (list 'faulted error)
               (e-chat-service-subscription-active-p subscription) nil))))))

(defun e-chat-service--sql-notify-event (binding event)
  "Publish transient EVENT or wake canonical SQLite change queries."
  (if (plist-get event :board-seq)
      (dolist (subscription
               (copy-sequence (e-chat-service-binding-subscribers binding)))
        (when (e-chat-service-subscription-active-p subscription)
          (setf (e-chat-service-subscription-sqlite-rerun-p subscription) t)
          (e-chat-service--schedule-subscription-drain subscription)))
    (e-chat-service--sql-deliver-event binding event)))

(defun e-chat-service--sql-note-failure (binding error &optional owner-suspect-p)
  "Publish BINDING's SQLite ERROR visibly.
When OWNER-SUSPECT-P is non-nil, retain the first bounded error on both the
session owner and live binding.  Read failures remain request-local."
  (let ((reported-error error))
    (when owner-suspect-p
      (setq reported-error
            (e-session-note-persistence-failure
             (e-harness-sessions (e-chat-service-binding-harness binding))
             (e-chat-service-binding-session-id binding) error))
      (unless (e-chat-service-binding-first-persistence-error binding)
        (setf (e-chat-service-binding-first-persistence-error binding)
              (copy-tree reported-error t))))
  (e-chat-service--sql-notify-event
   binding
   (list :type 'turn-failed
         :session-id (e-chat-service-binding-session-id binding)
         :turn-id nil :selected-participant-p t
         :payload (list :error (e-work-error-message reported-error)
                        :persistence-suspect (and owner-suspect-p t))))))

(defun e-chat-service--cancel-executing-deliveries (binding)
  "Cancel every live delivery outcome owned by retiring BINDING.
The durable pickup remains SQLite-owned.  This releases request-owned producer
callbacks immediately so closing a live controller cannot strand a task in
`running'."
  (when-let ((executing (e-chat-service-binding-executing-turns binding)))
    (let (delivery-ids)
      (maphash (lambda (delivery-id _turn-id)
                 (push delivery-id delivery-ids))
               executing)
      (dolist (delivery-id delivery-ids)
        (condition-case error
            (e-board-sqlite-service-notify-delivery-outcome
             (e-chat-service-binding-sqlite-service binding)
             delivery-id 'cancelled
             (list :reason 'binding-retired
                   :session-id
                   (e-chat-service-binding-session-id binding)))
          (error
           ;; Retirement owns cleanup and must notify the remaining delivery
           ;; observers even when one request callback is defective.
           (message "e-chat delivery retirement callback failed: %s"
                    (e-work-error-message error)))))
      (clrhash executing))))

(defun e-chat-service--retire-binding (binding)
  "Retire SQL BINDING and release its process-local coordination."
  (when (and (e-chat-service-binding-p binding)
             (not (eq (e-chat-service-binding-lifecycle-state binding)
                      'retired)))
    (let* ((harness (e-chat-service-binding-harness binding))
           (session-id (e-chat-service-binding-session-id binding))
           (board-id (e-chat-service-binding-board-id binding))
           (bindings (and harness (gethash harness e-chat-service--bindings))))
      (setf (e-chat-service-binding-lifecycle-state binding) 'retiring)
      (cl-incf (e-chat-service-binding-lifecycle-generation binding))
      (dolist (subscription
               (copy-sequence
                (e-chat-service-binding-subscribers binding)))
        (setf (e-chat-service-subscription-active-p subscription) nil
              (e-chat-service-subscription-drain-scheduled subscription) nil
              (e-chat-service-subscription-state subscription)
              (list 'detached nil)
              (e-chat-service-subscription-sqlite-rerun-p subscription) nil)
        (cl-incf
         (e-chat-service-subscription-lifecycle-generation subscription))
        (when-let ((timer
                    (e-chat-service-subscription-drain-timer subscription)))
          (when (timerp timer) (cancel-timer timer))
          (setf (e-chat-service-subscription-drain-timer subscription) nil))
        (when-let ((work
                    (e-chat-service-subscription-sqlite-query-work
                     subscription)))
          (unless (memq (plist-get (e-work-status work) :state)
                        '(finished failed cancelled))
            (ignore-errors (e-work-cancel work)))
          (setf (e-chat-service-subscription-sqlite-query-work subscription)
                nil)))
      (setf (e-chat-service-binding-subscribers binding) nil)
      (let ((callbacks
             (prog1 (copy-sequence
                     (e-chat-service-binding-cleanup-callbacks binding))
               (setf (e-chat-service-binding-cleanup-callbacks binding) nil))))
        (dolist (callback callbacks)
          (condition-case error
              (funcall callback binding)
            (error
             ;; Binding retirement must release every request-owned observer;
             ;; one defective extension cannot strand the remaining cleanup.
             (message "e chat binding cleanup callback failed: %s"
                      (e-work-error-message error))))))
      (when-let* ((port (e-chat-service-binding-turn-port binding))
                  (subscription
                   (e-chat-service-binding-activity-subscription binding)))
        (e-harness-attached-turn-port-stop-observing port subscription)
        (setf (e-chat-service-binding-activity-subscription binding) nil))
      (when-let ((subscription
                  (e-chat-service-binding-pickup-subscription binding)))
        (e-board-sqlite-pickup-observation-cancel subscription)
        (setf (e-chat-service-binding-pickup-subscription binding) nil))
        (e-chat-service--cancel-executing-deliveries binding)
      (setf (e-chat-service-binding-readiness-work binding) nil
            (e-chat-service-binding-pickup-readiness-wakeup-p binding) nil)
      (when-let ((timer (e-chat-service-binding-idle-close-timer binding)))
        (when (timerp timer) (cancel-timer timer))
        (setf (e-chat-service-binding-idle-close-timer binding) nil))
      (setf (e-chat-service-binding-lifecycle-state binding) 'retired)
      (when (and bindings (eq (gethash session-id bindings) binding))
        (remhash session-id bindings)
        (when (= (hash-table-count bindings) 0)
          (remhash harness e-chat-service--bindings)))
      (when board-id
        (e-chat-service--unregister-board-binding binding))
      binding)))

(defun e-chat-service--discard-binding (binding)
  "Discard an unpublished SQL BINDING and its live coordination."
  (and (e-chat-service--retire-binding binding) t))

(defun e-chat-service-release-unobserved-binding (binding)
  "Retire BINDING when no presentation subscribed after readiness.
This is the narrow lifecycle operation for a surface that disappears while
its asynchronous binding is still being admitted.  Retirement runs on the
next event-loop turn so other callbacks sharing the same readiness work can
subscribe first; a new subscriber cancels this timer through the ordinary
binding lease path.  Durable SQLite state is never removed."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (when (e-chat-service--binding-live-p binding)
    (e-chat-service--cancel-idle-close binding)
    (let ((generation
           (or (e-chat-service-binding-lifecycle-generation binding) 0))
          timer)
      (setq timer
            (run-at-time
             0 nil
             (lambda ()
               (when (and
                      (eq timer
                          (e-chat-service-binding-idle-close-timer binding))
                      (= generation
                         (or (e-chat-service-binding-lifecycle-generation
                              binding)
                             0)))
                 (setf (e-chat-service-binding-idle-close-timer binding) nil)
                 (when (and (e-chat-service--binding-live-p binding)
                            (not (cl-some
                                  #'e-chat-service-subscription-active-p
                                  (e-chat-service-binding-subscribers
                                   binding))))
                   (e-chat-service--retire-binding binding))))))
      (setf (e-chat-service-binding-idle-close-timer binding) timer)))
  binding)

(defun e-chat-service-binding (harness session-id)
  "Return HARNESS SESSION-ID's live chat board binding, or nil."
  (when-let ((bindings (gethash harness e-chat-service--bindings)))
    (when-let ((binding (gethash session-id bindings)))
      (if (and (e-chat-service--binding-live-p binding)
               (not (eq (e-chat-service--binding-readiness-state binding)
                        'unavailable)))
          binding
        (e-chat-service--retire-binding binding)
        (remhash session-id bindings)
        nil))))

(defun e-chat-service-board-session-p (harness session-id)
  "Return non-nil when SESSION-ID has a live SQL Board binding.
An unopened durable association requires an asynchronous query and is not
reconstructed for this process-local predicate."
  (and (e-chat-service-binding harness session-id) t))

(defun e-chat-service--board-has-active-subscriber-p (binding)
  "Return non-nil when BINDING's runtime Board has a live subscriber."
  (cl-some
   (lambda (binding)
     (cl-some #'e-chat-service-subscription-active-p
              (e-chat-service-binding-subscribers binding)))
   (e-chat-service--board-bindings-for binding)))

(defun e-chat-service--cancel-idle-close (binding)
  "Cancel BINDING's pending idle close, if any."
  (when-let ((timer (e-chat-service-binding-idle-close-timer binding)))
    (when (timerp timer) (cancel-timer timer))
    (setf (e-chat-service-binding-idle-close-timer binding) nil)))

(defun e-chat-service-close-board (binding)
  "Retire all process-local coordination for BINDING's durable Board.
The SQLite Board itself has no application-owned close lifecycle."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (let ((current-bindings
         (copy-sequence (e-chat-service--board-bindings-for binding))))
    (dolist (current current-bindings)
      (e-chat-service--retire-binding current))
    nil))

(defun e-chat-service--schedule-idle-close (binding)
  "Schedule registry-owned board cleanup after BINDING becomes idle."
  (e-chat-service--cancel-idle-close binding)
  (let ((generation
         (or (e-chat-service-binding-lifecycle-generation binding) 0)))
    (setf (e-chat-service-binding-idle-close-timer binding)
          (run-at-time
           (max 0 e-chat-service-idle-close-delay) nil
           (lambda ()
             (when (= generation
                      (or (e-chat-service-binding-lifecycle-generation binding)
                          0))
               (setf (e-chat-service-binding-idle-close-timer binding) nil)
               (if (not (e-chat-service--binding-live-p binding))
                     ;; An external close can win the race with this timer;
                     ;; run the same exact terminal cleanup instead of
                     ;; leaving a stale service catalog behind.
                     (e-chat-service--retire-binding binding)
                   (when (not (e-chat-service--board-has-active-subscriber-p
                               binding))
                     (e-chat-service-close-board binding)))))))))

(defun e-chat-service--subscription-drain-callback
    (subscription binding-generation subscription-generation)
  "Run SUBSCRIPTION's deferred drain for its captured generations."
  (let ((binding (e-chat-service-subscription-binding subscription)))
    (when (and (= binding-generation
                  (or (e-chat-service-binding-lifecycle-generation binding) 0))
               (= subscription-generation
                  (or (e-chat-service-subscription-lifecycle-generation
                       subscription)
                      0)))
      (if (e-chat-service--binding-live-p binding)
          (e-chat-service--sql-subscription-query-start subscription)
        (e-chat-service--retire-binding binding)))))

(defun e-chat-service--sql-subscription-deliver-row (subscription row)
  "Deliver canonical ROW once and advance SUBSCRIPTION's cursor."
  (let* ((binding (e-chat-service-subscription-binding subscription))
         (position (plist-get row :position))
         (cursor (or (e-chat-service-subscription-sqlite-cursor subscription)
                     0)))
    (when (> position cursor)
      (setf (e-chat-service-subscription-sqlite-cursor subscription) position)
      (when-let* ((event
                   (e-chat-service--sql-message-event
                    binding (plist-get row :record))))
        (condition-case error
            (funcall (e-chat-service-subscription-function subscription)
                     (copy-tree event t))
          (error
           (e-chat-service--retire-subscription
            subscription (list 'faulted error))))))))

(defun e-chat-service--sql-subscription-settled
    (subscription generation initial-kind work)
  "Apply WORK to SUBSCRIPTION when GENERATION remains current."
  (when (and (e-chat-service-subscription-active-p subscription)
             (= generation
                (or (e-chat-service-subscription-lifecycle-generation
                     subscription)
                    0))
             (eq work
                 (e-chat-service-subscription-sqlite-query-work subscription)))
    (setf (e-chat-service-subscription-sqlite-query-work subscription) nil)
    (let ((status (e-work-status work)))
      (pcase (plist-get status :state)
        ('finished
         (let* ((result (plist-get status :result))
                (next (plist-get result :next)))
           (pcase initial-kind
             (_
              (dolist (row (plist-get result :records))
                (e-chat-service--sql-subscription-deliver-row
                 subscription row))
              (unless next
                (setf (e-chat-service-subscription-sqlite-cursor subscription)
                      (or (plist-get result :through)
                          (e-chat-service-subscription-sqlite-cursor
                           subscription)
                          0)))))
           (when (e-chat-service-subscription-active-p subscription)
             (let ((immediate
                    (or next
                        (e-chat-service-subscription-sqlite-rerun-p
                         subscription))))
               (setf (e-chat-service-subscription-sqlite-rerun-p subscription)
                     nil)
               (e-chat-service--schedule-subscription-drain
                subscription
                (unless immediate e-chat-service-sqlite-poll-interval))))))
        ((or 'failed 'cancelled)
         ;; A read failure is request-local.  Keep the presentation lease and
         ;; retry its detached page without poisoning the Board/session owner.
         (setf (e-chat-service-subscription-state subscription)
               (list 'read-failed (copy-tree (plist-get status :error) t)))
         (when (e-chat-service-subscription-active-p subscription)
           (e-chat-service--schedule-subscription-drain
            subscription e-chat-service-sqlite-poll-interval)))))))

(defun e-chat-service--sql-subscription-query-start (subscription)
  "Start one serialized bounded SQLite query for SUBSCRIPTION."
  (setf (e-chat-service-subscription-drain-scheduled subscription) nil
        (e-chat-service-subscription-drain-timer subscription) nil)
  (when (and (e-chat-service-subscription-active-p subscription)
             (not (e-chat-service-subscription-sqlite-query-work subscription)))
    (let* ((binding (e-chat-service-subscription-binding subscription))
           (service (e-chat-service-binding-sqlite-service binding))
           (board-id (e-chat-service-binding-board-id binding))
           (cursor (e-chat-service-subscription-sqlite-cursor subscription))
           (initial-kind (and (null cursor) 'view))
           (work
            (pcase initial-kind
              ('view
               (e-board-sqlite-service-visible-window-start
                service board-id :limit e-chat-service-projection-capacity))
              (_
               (e-board-sqlite-service-record-page-start
                service board-id :after cursor
                :limit e-chat-service-observer-page-limit
                :selector '(:kinds (input output activity))))))
           (generation
            (or (e-chat-service-subscription-lifecycle-generation subscription)
                0)))
      (setf (e-chat-service-subscription-sqlite-query-work subscription) work)
      (e-work-on-settle
       work
       (lambda (settled)
         (e-chat-service--sql-subscription-settled
          subscription generation initial-kind settled))))))

(defun e-chat-service--schedule-subscription-drain (subscription &optional delay)
  "Schedule one later bounded independent observer drain for SUBSCRIPTION."
  (let* ((binding (e-chat-service-subscription-binding subscription))
         (readiness (e-chat-service-binding-readiness-work binding))
         (binding-generation
          (or (e-chat-service-binding-lifecycle-generation binding) 0))
         (subscription-generation
          (or (e-chat-service-subscription-lifecycle-generation subscription)
              0)))
    (when (and (or (null readiness)
                   (eq (plist-get (e-work-status readiness) :state) 'finished))
               (e-chat-service-subscription-active-p subscription)
               (not (e-chat-service-subscription-drain-scheduled subscription)))
      (setf (e-chat-service-subscription-drain-scheduled subscription) t
            (e-chat-service-subscription-drain-timer subscription)
            (run-at-time (or delay 0) nil
                         #'e-chat-service--subscription-drain-callback
                         subscription binding-generation
                         subscription-generation)))))

(defun e-chat-service--retire-subscription (subscription &optional state)
  "Release SUBSCRIPTION's presentation lease and record optional STATE."
  (let ((binding (e-chat-service-subscription-binding subscription)))
    (setf (e-chat-service-subscription-active-p subscription) nil
          (e-chat-service-subscription-drain-scheduled subscription) nil)
    (cl-incf (e-chat-service-subscription-lifecycle-generation subscription))
    (when-let ((timer (e-chat-service-subscription-drain-timer subscription)))
      (when (timerp timer) (cancel-timer timer))
      (setf (e-chat-service-subscription-drain-timer subscription) nil))
    (when-let ((work
                (e-chat-service-subscription-sqlite-query-work subscription)))
      (unless (memq (plist-get (e-work-status work) :state)
                    '(finished failed cancelled))
        (ignore-errors (e-work-cancel work)))
      (setf (e-chat-service-subscription-sqlite-query-work subscription) nil))
    (when state
      (setf (e-chat-service-subscription-state subscription) state))
    (setf (e-chat-service-binding-subscribers binding)
          (delq subscription
                (e-chat-service-binding-subscribers binding)))
    (unless (eq (e-chat-service-binding-lifecycle-state binding) 'retired)
      (unless (cl-some #'e-chat-service-subscription-active-p
                       (e-chat-service-binding-subscribers binding))
        (e-chat-service--schedule-idle-close binding)))))

(defconst e-chat-service--board-role-root "owner"
  "Durable chat board role for the user-facing owning session.")

(defconst e-chat-service--board-role-participant "participant"
  "Durable chat board role for a private execution session.")

(defun e-chat-service--routing-policy
    (participant-id pickup-selector observer-selector default-tags default-to)
  "Return one validated, detached routing policy for PARTICIPANT-ID.
`:self' is a caller-facing admission shorthand only; durable state contains the
resolved participant identity so restart never needs shell or caller policy."
  (unless (stringp participant-id)
    (signal 'e-session-error
            (list "Routing policy participant id must be a string"
                  participant-id)))
  (let* ((observer-selector
          (if (eq observer-selector :self)
              (list :subject-participant-id participant-id)
            observer-selector))
         (default-to (if (eq default-to :self) participant-id default-to))
         ;; Keep caller-owned values untouched until session's bounded
         ;; admission walk has completed.  The session copy is the one durable
         ;; detached representation used after this check.
         (policy (list :participant-id participant-id
                       :pickup-selector pickup-selector
                       :observer-selector observer-selector
                       :default-tags default-tags
                       :default-to default-to)))
    (unless (e-session-board-routing-policy-valid-p policy)
      (signal 'e-session-error (list "Invalid board routing policy" policy)))
    (e-session-board-routing-policy-copy-value policy)))

(defun e-chat-service--annotate-admission-records (records)
  "Return bounded RECORDS with their v6 journal positions attached.

Board-backed creation is admitted as one small root-plus-association batch.
The query row is derived from those detached canonical records before the
batch crosses the SQLite adapter, so the worker receives no session mirror or
semantic interpretation responsibility."
  (let ((position 0))
    (mapcar
     (lambda (record)
       (setq position (1+ position))
       (let ((copy (copy-tree record t)))
         (plist-put copy :journal-position position)
         (unless (plist-member copy :timestamp)
           (when-let ((timestamp (or (plist-get copy :created-at)
                                     (plist-get copy :updated-at))))
             (plist-put copy :timestamp timestamp)))
         copy))
     records)))

(defun e-chat-service--routing-override-conflicts-p
    (policy participant-id participant-id-supplied-p
           pickup-selector pickup-selector-supplied-p
           observer-selector observer-selector-supplied-p
           default-tags default-tags-supplied-p default-to default-to-supplied-p)
  "Return non-nil when supplied routing values conflict with POLICY."
  (or (and participant-id-supplied-p
           (not (equal participant-id (plist-get policy :participant-id))))
      (and pickup-selector-supplied-p
           (not (equal pickup-selector (plist-get policy :pickup-selector))))
      (and observer-selector-supplied-p
           (let ((expected
                  (if (eq observer-selector :self)
                      (list :subject-participant-id
                            (plist-get policy :participant-id))
                    observer-selector)))
             (not (equal expected
                         (plist-get policy :observer-selector)))))
      (and default-tags-supplied-p
           (not (equal default-tags (plist-get policy :default-tags))))
      (and default-to-supplied-p
           (let ((expected
                  (if (eq default-to :self)
                      (plist-get policy :participant-id)
                    default-to)))
             (not (equal expected (plist-get policy :default-to)))))))

(defun e-chat-service--publish-ready-binding (binding)
  "Release BINDING's presentation query gate after SQL admission."
  (dolist (subscription
           (copy-sequence (e-chat-service-binding-subscribers binding)))
    (e-chat-service--schedule-subscription-drain subscription)))
(defun e-chat-service--sql-settle-pickup (binding delivery-id transition)
  "Settle DELIVERY-ID by TRANSITION and continue its SQLite FIFO."
  (let ((work
         (e-board-sqlite-service-transition-pickup-start
          (e-chat-service-binding-sqlite-service binding)
          (e-chat-service-binding-board-id binding) delivery-id transition)))
    (e-work-on-settle
     work
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (if (eq (plist-get status :state) 'finished)
             (when-let* ((next (plist-get (plist-get status :result) :next)))
               (e-chat-service--sql-deliver-pickup binding next))
           (e-chat-service--sql-note-failure
            binding (plist-get status :error) t)))))
    work))

(defun e-chat-service--sql-delivery-metadata (binding pickup)
  "Return harness metadata for BINDING's claimed detached PICKUP."
  (append
   (list :input-origin 'board
         :board-delivery-id (copy-tree (plist-get pickup :delivery-id) t)
         :board-id (e-chat-service-binding-board-id binding)
         :board-participant-id (e-chat-service-binding-participant-id binding)
         :board-message-id (plist-get pickup :message-id)
         :board-subscription-ids
         (copy-tree (plist-get pickup :subscription-ids) t)
         :board-event-seq-range
         (copy-tree (plist-get pickup :event-seq-range) t)
         :board-input-mode (plist-get pickup :mode)
         :board-reference (copy-tree (plist-get pickup :reference) t)
         :board-requester-actor
         (copy-tree (plist-get pickup :requester-actor) t)
         :board-cause-metadata
         (copy-tree (plist-get pickup :cause-metadata) t)
         :board-endpoint-token
         (e-chat-service-binding-endpoint-token binding)
         :board-endpoint-generation
         (e-chat-service-binding-endpoint-generation binding))
   (copy-tree
    (plist-get (plist-get pickup :cause-metadata) :input-attributes) t)))

(defun e-chat-service--sql-submit-claimed-pickup (binding pickup)
  "Submit claimed PICKUP through BINDING's attached harness port."
  (let* ((port (e-chat-service-binding-turn-port binding))
         (active (e-harness-attached-turn-port-active-turn port))
         (active-p (eq (plist-get active :status) 'running))
         (mode (or (plist-get pickup :mode) 'inject))
         (prompt (plist-get pickup :content))
         (metadata (e-chat-service--sql-delivery-metadata binding pickup))
         (delivery-id (plist-get pickup :delivery-id)))
    (condition-case error
        (progn
          (puthash (copy-tree delivery-id t) 'submitting
                   (e-chat-service-binding-executing-turns binding))
          (let ((turn-id
                 (cond
                  ((not active-p)
                   (e-harness-attached-turn-port-submit
                    port prompt :metadata metadata))
                  ((eq mode 'queue)
                   (e-harness-attached-turn-port-queue
                    port prompt :metadata metadata))
                  (t
                   (e-harness-attached-turn-port-steer
                    port prompt :metadata metadata)))))
            (when turn-id
              (puthash (copy-tree delivery-id t) turn-id
                       (e-chat-service-binding-executing-turns binding)))
            turn-id))
      (error
       (remhash delivery-id (e-chat-service-binding-executing-turns binding))
       (e-board-sqlite-service-notify-delivery-outcome
        (e-chat-service-binding-sqlite-service binding)
        delivery-id 'failed
        (list :error (e-work-error-message error)))
       (e-chat-service--sql-settle-pickup binding delivery-id 'fail)
       (e-chat-service--sql-note-failure binding error)
       nil))))

(defun e-chat-service--sql-deliver-pickup (binding pickup)
  "Claim and submit one ready detached PICKUP exactly once in this process."
  (pcase (e-chat-service--binding-readiness-state binding)
    ('waiting
     ;; A readiness notification is a single edge, not a polling loop.  The
     ;; subsequent bounded resume query reads the authoritative FIFO after the
     ;; Board capability has settled and therefore cannot lose a pickup that
     ;; arrived while the initial run-set projection was restoring.
     (setf (e-chat-service-binding-pickup-readiness-wakeup-p binding) t))
    ('ready
     (let* ((delivery-id (plist-get pickup :delivery-id))
            (executing (e-chat-service-binding-executing-turns binding)))
       (when (and (eq (plist-get pickup :state) 'ready)
                  (not (gethash delivery-id executing)))
         (puthash (copy-tree delivery-id t) 'claiming executing)
         (let ((work
                (e-board-sqlite-service-transition-pickup-start
                 (e-chat-service-binding-sqlite-service binding)
                 (e-chat-service-binding-board-id binding) delivery-id 'claim)))
           (e-work-on-settle
            work
            (lambda (settled)
              (let ((status (e-work-status settled)))
                (if (eq (plist-get status :state) 'finished)
                    (e-chat-service--sql-submit-claimed-pickup
                     binding (plist-get (plist-get status :result) :pickup))
                  (remhash delivery-id executing)
                  (e-board-sqlite-service-notify-delivery-outcome
                   (e-chat-service-binding-sqlite-service binding)
                   delivery-id 'failed
                   (list :error (e-work-error-message
                                 (plist-get status :error))))
                  (e-chat-service--sql-note-failure
                   binding (plist-get status :error) t)))))
           work))))))

(defun e-chat-service--sql-notify-turn-deliveries (binding event status)
  "Settle BINDING's live deliveries for terminal harness EVENT as STATUS."
  (let ((turn-id (plist-get event :turn-id))
        (executing (e-chat-service-binding-executing-turns binding))
        delivery-ids)
    (maphash
     (lambda (delivery-id executing-turn-id)
       (when (equal executing-turn-id turn-id)
         (push delivery-id delivery-ids)))
     executing)
    (dolist (delivery-id delivery-ids)
      (remhash delivery-id executing)
      (e-board-sqlite-service-notify-delivery-outcome
       (e-chat-service-binding-sqlite-service binding)
       delivery-id status event))))

(defun e-chat-service--sql-context-curated-source-key (binding event)
  "Return EVENT's stable private source key for BINDING's curation row."
  (let ((source-id
         (or (plist-get event :board-activity-sequence)
             (plist-get event :activity-entry-id)
             (plist-get event :id))))
    (unless source-id
      (signal 'e-chat-service-invalid-activity
              (list 'context-curated :missing-source-identity)))
    (list "harness-activity"
          (e-chat-service-binding-session-id binding)
          source-id)))

(defun e-chat-service--sql-publish-context-curated (binding event projection)
  "Persist EVENT's safe curation PROJECTION and wake BINDING subscribers."
  (let* ((attributes
          (e-chat-service--public-curation-projection projection))
         (participant-id (e-chat-service-binding-participant-id binding))
         (work
          (e-board-sqlite-service-record-append-start
           (e-chat-service-binding-sqlite-service binding)
           (e-chat-service-binding-board-id binding)
           'activity 'context-curation
           (e-chat-service--sql-context-curated-source-key binding event)
           :author (format "participant:%s" participant-id)
           :subject-participant-id participant-id
           :source-turn-id (plist-get event :turn-id)
           :tags (copy-tree (e-chat-service-binding-default-tags binding) t)
           :activity-kind 'context-curated
           :attributes attributes)))
    (e-work-on-settle
     work
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (if (eq (plist-get status :state) 'finished)
             (when-let* ((message
                          (plist-get (plist-get status :result) :message))
                         (projected
                          (e-chat-service--sql-message-event binding message)))
               ;; Canonical rows wake the cursor-backed observer.  The cursor,
               ;; rather than this callback, decides whether the row is new.
               (e-chat-service--sql-notify-event binding projected))
           (e-chat-service--sql-note-failure
            binding (plist-get status :error) t)))))
    work))

(defun e-chat-service--sql-harness-event (binding event)
  "Translate one live harness EVENT for coordination-only BINDING."
  (let ((type (plist-get event :type)))
    (pcase type
      ('input-consumed
       (when-let* ((delivery-id
                    (plist-get (plist-get event :payload) :delivery-id)))
         ;; Retain only this live delivery-to-turn correlation until the
         ;; canonical harness terminal event arrives.  The pickup is durably
         ;; consumed now, but consumption is not execution completion.
         (when-let* ((turn-id (plist-get event :turn-id)))
           (puthash (copy-tree delivery-id t) turn-id
                    (e-chat-service-binding-executing-turns binding)))
         (e-chat-service--sql-settle-pickup binding delivery-id 'consume)))
      ('turn-finished
       (let* ((turn-id (plist-get event :turn-id))
              (output-message
               (e-harness-attached-turn-port-assistant-message
                (e-chat-service-binding-turn-port binding) turn-id))
              (output (and output-message
                           (plist-get output-message :content)))
              (work
               (e-board-sqlite-service-record-append-start
                (e-chat-service-binding-sqlite-service binding)
                (e-chat-service-binding-board-id binding)
                'output 'output
                (list "harness-output"
                      (e-chat-service-binding-session-id binding) turn-id)
                :author
                (format "participant:%s"
                        (e-chat-service-binding-participant-id binding))
                :subject-participant-id
                (e-chat-service-binding-participant-id binding)
                :participant-name
                (e-chat-service-binding-participant-name binding)
                :source-turn-id turn-id :content output
                :attributes (copy-tree (plist-get event :payload) t))))
         (e-chat-service--sql-notify-turn-deliveries binding event 'done)
         (e-work-on-settle
          work
          (lambda (settled)
            (let ((status (e-work-status settled)))
              (if (eq (plist-get status :state) 'finished)
                  (let ((result (plist-get status :result)))
                    (when (eq (plist-get result :status) 'posted)
                      (e-chat-service--sql-notify-event
                       binding
                       (e-chat-service--sql-message-event
                        binding (plist-get result :message))))
                    (e-chat-service--sql-notify-event
                     binding
                     (append (copy-tree event t)
                             (list :selected-participant-p t))))
                (e-chat-service--sql-note-failure
                 binding (plist-get status :error) t)))))))
      ((or 'turn-failed 'turn-cancelled)
       (e-chat-service--sql-notify-turn-deliveries
        binding event (if (eq type 'turn-failed) 'failed 'cancelled))
       (e-chat-service--sql-notify-event
        binding (append (copy-tree event t)
                        (list :selected-participant-p t))))
      ('context-frame-consumed
       (when-let* ((curation
                    (plist-get (plist-get event :payload) :curation)))
         (e-chat-service--sql-publish-context-curated
          binding event curation)))
      ((or 'message-added) nil)
      (_
       (when (memq type e-chat-service--public-live-harness-event-types)
         (e-chat-service--sql-notify-event
          binding (append (e-chat-service--public-live-harness-event event)
                          (list :selected-participant-p t))))))))

(defun e-chat-service--sql-resume-ready (binding)
  "Request and run BINDING's exact bounded ready pickup after restart."
  (pcase (e-chat-service--binding-readiness-state binding)
    ('waiting
     (setf (e-chat-service-binding-pickup-readiness-wakeup-p binding) t))
    ('ready
     (let ((board-work
            (e-board-sqlite-service-board-get-start
             (e-chat-service-binding-sqlite-service binding)
             (e-chat-service-binding-board-id binding))))
       (e-work-on-settle
        board-work
        (lambda (settled)
          (when (eq (plist-get (e-work-status settled) :state) 'finished)
            (let* ((board (plist-get (e-work-status settled) :result))
                   (generation (plist-get board :generation))
                   (pickup-work
                    (e-board-sqlite-service-pickup-page-start
                     (e-chat-service-binding-sqlite-service binding)
                     (e-chat-service-binding-board-id binding) generation
                     (e-chat-service-binding-participant-id binding) 16)))
              (e-work-on-settle
               pickup-work
               (lambda (pickups-settled)
                 (if (eq (plist-get (e-work-status pickups-settled) :state)
                         'finished)
                     (when-let* ((ready
                                  (seq-find
                                   (lambda (pickup)
                                     (eq (plist-get pickup :state) 'ready))
                                   (plist-get (e-work-status pickups-settled)
                                              :result))))
                       (e-chat-service--sql-deliver-pickup binding ready))
                   (e-chat-service--sql-note-failure
                    binding (plist-get (e-work-status pickups-settled)
                                       :error)))))))))))))

(defun e-chat-service--sql-readiness-settled (binding settled)
  "Release BINDING pickup delivery after its readiness SETTLED.
The callback owns the one wake-up edge from the initial Board projection;
failed or cancelled readiness drops the edge and never claims a pickup."
  (when (e-chat-service--binding-live-p binding)
    (if (eq (plist-get (e-work-status settled) :state) 'finished)
        (when (e-chat-service-binding-pickup-readiness-wakeup-p binding)
          (setf (e-chat-service-binding-pickup-readiness-wakeup-p binding) nil)
          (e-chat-service--publish-ready-binding binding)
          (e-chat-service--sql-resume-ready binding))
      (setf (e-chat-service-binding-pickup-readiness-wakeup-p binding) nil))))

(defun e-chat-service--install-sqlite-binding
    (harness session-id association &optional continuation-owner-p
             board-readiness-p)
  "Install a coordination-only binding from detached ASSOCIATION.
When BOARD-READINESS-P is nil, the binding is a child participant binding and
does not acquire the owner-chat Board run-set readiness obligation."
  (or (e-chat-service-binding harness session-id)
      (let* ((policy (plist-get association :routing-policy))
             (board-id (plist-get association :board-id))
             (principal (plist-get association :principal))
             (participant-id (plist-get policy :participant-id))
             (token (vector 'e-chat-sqlite session-id
                            (e-session-generate-id)))
             binding port subscription)
        (unless (and (stringp board-id) principal
                     (stringp participant-id)
                     (e-session-board-routing-policy-valid-p policy))
          (signal 'e-session-error
                  (list "Malformed detached Board association" association)))
        (setq binding
              (e-chat-service--binding-create
               :harness harness :session-id session-id
               :board-id board-id :principal (copy-tree principal t)
               :participant-id participant-id
               :participant-name
               (copy-tree (plist-get association :participant-name) t)
               :sqlite-service
               (e-board-sqlite-service-create
                (e-session-storage-runtime-store (e-harness-sessions harness)))
               :endpoint-token token :endpoint-generation 1
               :default-tags (copy-tree (plist-get policy :default-tags) t)
               :default-to (copy-tree (plist-get policy :default-to) t)
               :subscribers nil
               :cleanup-callbacks nil
               :executing-turns (make-hash-table :test 'equal)
               :pickup-readiness-wakeup-p nil
               :continuation-owner-p continuation-owner-p
               :lifecycle-generation 0 :lifecycle-state 'active))
        (setq port
              (e-harness-attached-turn-port-create
               :harness harness :session-id session-id
               :attachment-token token
               :authorizer
               (lambda (candidate-harness candidate-session-id candidate-token)
                 (and (eq candidate-harness harness)
                      (equal candidate-session-id session-id)
                      (equal candidate-token token)
                      (eq (e-chat-service-binding harness session-id) binding)
                      (e-chat-service--binding-live-p binding)))
               :follow-up-publisher
               (lambda (candidate-harness candidate-session-id prompt &rest args)
                 (apply #'e-chat-service--post
                        candidate-harness candidate-session-id prompt 'queue
                        args))))
        (setf (e-chat-service-binding-turn-port binding) port)
        (puthash session-id binding
                 (e-chat-service--harness-bindings harness))
        (e-chat-service--register-board-binding binding)
        ;; Install the owner-chat readiness child before any pickup observer or
        ;; restart query can deliver a durable input.  Notifications that race
        ;; this child set one bounded wake-up bit and are reread after success.
        (setf (e-chat-service-binding-readiness-work binding)
              (and board-readiness-p
                   (e-chat-service--binding-open-readiness-start binding)))
        (when (e-chat-service-binding-readiness-work binding)
          ;; One initial scan is required even when no notification races the
          ;; readiness child; the bit is also the coalescing edge for any
          ;; pickup observed while that child is pending.
          (setf (e-chat-service-binding-pickup-readiness-wakeup-p binding) t))
        (setf (e-chat-service-binding-pickup-subscription binding)
              (e-board-sqlite-service-observe-pickups
               (e-chat-service-binding-sqlite-service binding)
               board-id
               (lambda (pickups)
                 (when (e-chat-service--binding-live-p binding)
                   (dolist (pickup pickups)
                     (when (equal
                            (plist-get pickup :participant-id)
                            (e-chat-service-binding-participant-id binding))
                       (e-chat-service--sql-deliver-pickup
                        binding pickup)))))))
        (setq subscription
              (e-harness-attached-turn-port-observe-activity
               port (lambda (event)
                      (e-chat-service--sql-harness-event binding event))))
        (setf (e-chat-service-binding-activity-subscription binding)
              subscription)
        (when continuation-owner-p
          (e-chat-service--reconcile-binding-continuation binding))
        (if-let* ((readiness
                   (e-chat-service-binding-readiness-work binding)))
            (e-work-on-settle
             readiness
             (lambda (settled)
               (e-chat-service--sql-readiness-settled binding settled)))
          (setf (e-chat-service-binding-pickup-readiness-wakeup-p binding) nil)
          (e-chat-service--publish-ready-binding binding)
          (e-chat-service--sql-resume-ready binding))
        binding)))

(defun e-chat-service--pending-owner-admission-data (pending)
  "Return PENDING's deterministic detached owner-admission data."
  (let* ((creation-key (e-chat-service-create-operation-creation-key pending))
         (identities
          (and creation-key
               (e-chat-service-owner-admission-identities creation-key)))
         (session-id
          (or (plist-get identities :session-id)
              (e-chat-service-create-operation-session-id pending)))
         (principal
          (or (plist-get identities :principal)
              (format "chat:%s" session-id)))
         (board-id
          (or (plist-get identities :board-id)
              (let ((identity
                     (secure-hash 'sha256 (format "chat-session:%s" session-id))))
                (concat "brd_" (substring identity 0 32)))))
         (participant-id
          (or (plist-get identities :participant-id)
              (let ((identity
                     (secure-hash 'sha256 (format "chat-session:%s" session-id))))
                (concat "ptc_" (substring identity 32 64)))))
         (policy
          (e-chat-service--routing-policy
           participant-id '(:tags (main)) '(:tags (main)) '(main) nil))
         (session
          (or (e-chat-service-create-operation-admission pending)
              (let ((value
                     (e-board-sqlite-service-session-admission
                      :id session-id
                      :metadata
                      (e-chat-service-create-operation-metadata pending)
                      :principal principal :board-id board-id
                      :association-role e-chat-service--board-role-root
                      :routing-policy policy)))
                (setf (e-chat-service-create-operation-admission pending)
                      (copy-tree value t))
                value)))
         (records (plist-get session :admission-records)))
    (list
     :creation-key creation-key :session-id session-id :principal principal
     :board-id board-id :participant-id participant-id
     :records records
     :query-delta (plist-get session :query-delta)
     :participant
     (list :id participant-id :author "e-chat"
           :principal principal :controller principal
           :name
           (e-chat-service--participant-name
            (e-chat-service-create-operation-metadata pending) 'owner)
           :role 'owner :state 'active
           :subscription-id (concat "sub_" participant-id)
           :publication-pending nil))))

(defun e-chat-service--settle-pending-owner-admission (pending settled)
  "Settle PENDING from terminal owner-admission work SETTLED."
  (let* ((harness (e-chat-service-create-operation-harness pending))
         (session-id (e-chat-service-create-operation-session-id pending))
         (table (e-chat-service--harness-pending-creations harness))
         (status (e-work-status settled)))
    (when (eq (gethash session-id table) pending)
      (remhash session-id table))
    (unless (e-chat-service-create-operation-settled pending)
      (setf (e-chat-service-create-operation-settled pending) t)
      (if (eq (plist-get status :state) 'finished)
          (let ((association
                 (plist-get (plist-get status :result) :association)))
            (if association
                (e-work-finish
                 (e-chat-service-create-operation-work pending)
                 (list :id session-id
                       :creation-key
                       (e-chat-service-create-operation-creation-key pending)
                       :board-id (plist-get (e-chat-service--pending-owner-admission-data
                                             pending)
                                            :board-id)
                       :metadata
                       (copy-tree
                        (e-chat-service-create-operation-metadata pending) t)
                       :association (copy-tree association t)))
              (e-work-fail
               (e-chat-service-create-operation-work pending)
               (list 'e-session-error
                     "Owner admission returned no association" session-id))))
        (if (eq (plist-get status :state) 'cancelled)
            ;; Preserve cancellation as cancellation even though the child
            ;; callback runs synchronously while the parent cancel function
            ;; is unwinding.
            (e-work-cancel (e-chat-service-create-operation-work pending))
          (e-work-fail
           (e-chat-service-create-operation-work pending)
           (plist-get status :error)))))))

(defun e-chat-service--start-pending-owner-admission (pending)
  "Start or return PENDING's atomic empty-owner admission work."
  (or (e-chat-service-create-operation-owner-admission-work pending)
      (let* ((data (e-chat-service--pending-owner-admission-data pending))
             (service
              (e-board-sqlite-service-create
               (e-session-storage-runtime-store
                (e-chat-service-create-operation-store pending))))
             (work
              (e-board-sqlite-service-admit-session-owner-start
               service
               (plist-get data :session-id)
               (plist-get data :board-id)
               (plist-get data :principal)
               (plist-get data :records)
               (plist-get data :query-delta)
               (plist-get data :participant))))
        (setf (e-chat-service-create-operation-owner-admission-work pending)
              work)
        (setf (e-work-handle-cancel-function
               (e-chat-service-create-operation-work pending))
              (lambda (_handle)
                (let* ((session-id
                        (e-chat-service-create-operation-session-id pending))
                       (table
                        (e-chat-service--harness-pending-creations
                         (e-chat-service-create-operation-harness pending)))
                       (child
                        (e-chat-service-create-operation-owner-admission-work
                         pending)))
                  (when (eq (gethash session-id table) pending)
                    (remhash session-id table))
                  (when (and (e-work-handle-p child)
                             (not (memq (plist-get (e-work-status child) :state)
                                        '(finished failed cancelled))))
                    (e-work-cancel child)))))
        (e-work-on-settle
         work
         (lambda (settled)
           (e-chat-service--settle-pending-owner-admission pending settled)))
        work)))

(defun e-chat-service--finish-bind-operation (operation binding error)
  "Settle OPERATION exactly once with BINDING or ERROR."
  (unless (e-chat-service-bind-operation-settled operation)
    (setf (e-chat-service-bind-operation-settled operation) t
          (e-chat-service-bind-operation-child operation) nil)
    (let* ((harness (e-chat-service-bind-operation-harness operation))
           (session-id (e-chat-service-bind-operation-session-id operation))
           (table (e-chat-service--harness-binding-works harness))
           (work (e-chat-service-bind-operation-work operation)))
      (when (eq (gethash session-id table) work)
        (remhash session-id table))
      (if error
          (e-work-fail work error)
        (e-work-finish work binding)))))

(defun e-chat-service--start-bind-controller (operation association)
  "Install OPERATION's SQL binding from detached ASSOCIATION."
  (condition-case error
      (let ((harness (e-chat-service-bind-operation-harness operation)))
        (setf (e-chat-service-bind-operation-association operation)
              (copy-tree association t))
        (unless (e-session-storage-sqlite-p (e-harness-sessions harness))
          (signal 'e-session-storage-error
                  (list "Chat Board binding requires SQLite"
                        (e-chat-service-bind-operation-session-id operation))))
        (let* ((binding
                 (e-chat-service--install-sqlite-binding
                 harness
                 (e-chat-service-bind-operation-session-id operation)
                 association
                 (e-chat-service-bind-operation-continuation-owner-p operation)
                 t))
               (ready (e-chat-service--binding-ready-work binding)))
          (e-work-on-settle
           ready
           (lambda (settled)
             (let ((status (e-work-status settled)))
               (if (eq (plist-get status :state) 'finished)
                   (e-chat-service--finish-bind-operation
                    operation (plist-get status :result) nil)
                 (e-chat-service--retire-binding binding)
                 (e-chat-service--finish-bind-operation
                  operation nil
                  (or (plist-get status :error)
                      '(e-work-cancelled "Board readiness cancelled")))))))))
    (error
     (e-chat-service--finish-bind-operation operation nil error))))

(defun e-chat-service--bind-association-settled (operation child)
  "Continue OPERATION from terminal exact-association CHILD."
  (let ((status (e-work-status child)))
    (if (eq (plist-get status :state) 'finished)
        (let ((association (plist-get status :result)))
          (if association
              (e-chat-service--start-bind-controller operation association)
            (e-chat-service--finish-bind-operation
             operation nil
             (list 'e-session-missing
                   (e-chat-service-bind-operation-session-id operation)))))
      (e-chat-service--finish-bind-operation
       operation nil (plist-get status :error)))))

(defun e-chat-service--bind-creation-settled (operation child)
  "Continue OPERATION after pending session-creation CHILD settles."
  (let ((status (e-work-status child)))
    (if (eq (plist-get status :state) 'finished)
        (let ((association
               (plist-get (plist-get status :result)
                          :association)))
          (if association
              (e-chat-service--start-bind-controller operation association)
            (e-chat-service--finish-bind-operation
             operation nil
             (list 'e-session-missing
                   (e-chat-service-bind-operation-session-id operation)))))
      (e-chat-service--finish-bind-operation
       operation nil (plist-get status :error)))))

(defun e-chat-service--run-bind-operation (_handle operation _context)
  "Start OPERATION without awaiting SQLite association or Board reads."
  (if-let* ((association
             (e-chat-service-bind-operation-association operation)))
      (e-chat-service--start-bind-controller operation association)
    (let* ((prerequisite
            (e-chat-service-bind-operation-prerequisite operation))
           (child
            (or prerequisite
                (e-session-async-board-association
                 (e-harness-sessions
                  (e-chat-service-bind-operation-harness operation))
                 (e-chat-service-bind-operation-session-id operation)))))
      (setf (e-chat-service-bind-operation-child operation) child)
      (e-work-on-settle
       child
       (lambda (settled)
         (if prerequisite
             (e-chat-service--bind-creation-settled operation settled)
           (e-chat-service--bind-association-settled operation settled))))
      (when (and prerequisite
                 (e-chat-service-bind-operation-continuation-owner-p operation))
        (when-let* ((pending
                     (gethash
                      (e-chat-service-bind-operation-session-id operation)
                      (e-chat-service--harness-pending-creations
                       (e-chat-service-bind-operation-harness operation)))))
          (e-chat-service--start-pending-owner-admission pending)))))
  :deferred)

(defconst e-chat-service--bind-operation-spec
  (e-work-spec-create
   :id "chat-board-bind" :execution 'cooperative :interactive-policy 'async
   :owner 'e-chat-service :runner #'e-chat-service--run-bind-operation))

(defun e-chat-service-binding-start
    (harness session-id &optional association continuation-owner-p)
  "Return immediately with work opening SESSION-ID's bounded live binding.

ASSOCIATION, when supplied, is the detached exact row already requested by a
Daily surface.  CONTINUATION-OWNER-P designates this exact binding as the live
application owner for Board continuation delivery.  Concurrent callers share
only this in-flight work; its query result is not retained after the live
controller has been built."
  (let ((binding (e-chat-service-binding harness session-id)))
    (if (e-chat-service--binding-reusable-p binding)
        (progn
          (when continuation-owner-p
            (setf (e-chat-service-binding-continuation-owner-p binding) t)
            (e-chat-service--reconcile-binding-continuation binding))
          (e-chat-service--binding-ready-work binding))
      ;; A failed readiness child may still be visible for the short interval
      ;; before its owner operation retires it.  Never hand that binding back
      ;; as if it were a usable Board controller.
      (when binding
        (e-chat-service--retire-binding binding))
      (let* ((table (e-chat-service--harness-binding-works harness))
             (pending
              (gethash session-id
                       (e-chat-service--harness-pending-creations harness)))
             (current (gethash session-id table)))
        (when (and current continuation-owner-p)
          (when-let* ((operation (e-work-handle-arguments current)))
            (when (e-chat-service-bind-operation-p operation)
              (setf (e-chat-service-bind-operation-continuation-owner-p operation)
                    t)
              (when pending
                (e-chat-service--start-pending-owner-admission pending)))))
        (or current
            (let* ((operation
                    (e-chat-service--bind-operation-create
                     :harness harness :session-id session-id
                     :continuation-owner-p continuation-owner-p
                     :prerequisite
                     (and pending
                          (e-chat-service-create-operation-work pending))
                     :association (and association (copy-tree association t))))
                   (work
                    (e-work-prepare
                     e-chat-service--bind-operation-spec operation
                     :context (list :domain-ref session-id
                                    :work-kind 'chat-board-bind))))
              (setf (e-chat-service-bind-operation-work operation) work)
              (puthash session-id work table)
              (e-work-start-prepared work :arguments operation)
              work))))))

(defun e-chat-service--start-create-operation (operation)
  "Retain OPERATION only until its first input can be admitted atomically."
  (let* ((harness (e-chat-service-create-operation-harness operation))
         (session-id (e-chat-service-create-operation-session-id operation))
         (table (e-chat-service--harness-pending-creations harness)))
    (when (gethash session-id table)
      (signal 'e-session-duplicate (list session-id)))
    (puthash (copy-sequence session-id) operation table)
    :deferred))

(cl-defun e-chat-service-owner-admission-start (&key harness creation-key metadata)
  "Start deterministic owner admission for CREATION-KEY.

Return a request-scoped ticket immediately.  Its Board id is deterministic and
safe to display as preparing, while its WORK must settle before the Board is
used as a routable address.  Repeated calls while admission is pending share
the same work; calls after a restart re-submit the same durable identities and
the SQLite owner admission remains idempotent."
  (let* ((harness (or harness (e-chat-service-default-harness)))
         (store (e-harness-sessions harness))
         (identities (e-chat-service-owner-admission-identities creation-key))
         (session-id (plist-get identities :session-id))
         (metadata (e-harness--normalize-session-metadata metadata))
         (pending-table (e-chat-service--harness-pending-creations harness))
         (pending (gethash session-id pending-table)))
    (unless (e-session-storage-sqlite-p store)
      (signal 'e-session-storage-error
              (list "Chat session creation requires SQLite" session-id)))
    (if (and pending
             (e-chat-service-create-operation-p pending)
             (equal creation-key
                    (e-chat-service-create-operation-creation-key pending)))
        (e-chat-service--owner-admission-ticket pending)
      (let* ((operation
              (e-chat-service--create-operation-create
               :harness harness :store store :session-id session-id
               :metadata metadata :creation-key (copy-sequence creation-key)
               :board-id (plist-get identities :board-id)
               :participant-id (plist-get identities :participant-id)))
             (arguments (list :harness harness :metadata metadata))
             (work
              (e-work-prepare
               e-chat-service--create-operation-spec arguments
               :context (list :domain-ref session-id
                              :work-kind 'chat-session-create))))
        (setf (e-chat-service-create-operation-work operation) work
              (e-work-handle-arguments work) arguments)
        (e-work-start-prepared work :arguments operation)
        ;; Stable-key admission is an application operation in its own right;
        ;; do not wait for a presentation binding or a first input to submit
        ;; the atomic empty-owner transaction.
        (e-chat-service--start-pending-owner-admission operation)
        (e-chat-service--owner-admission-ticket operation)))))

(defun e-chat-service--owner-admission-ticket (operation)
  "Return the detached public ticket for owner-admission OPERATION."
  (let* ((identities
          (or (and (e-chat-service-create-operation-creation-key operation)
                   (e-chat-service-owner-admission-identities
                    (e-chat-service-create-operation-creation-key operation)))
              (e-chat-service--pending-owner-admission-data operation))))
    (e-chat-service--owner-admission-ticket-create
     :creation-key (copy-sequence
                    (or (plist-get identities :creation-key)
                        (e-chat-service-create-operation-creation-key operation)))
     :board-id (copy-sequence (plist-get identities :board-id))
     :session-id (copy-sequence
                  (or (plist-get identities :session-id)
                      (e-chat-service-create-operation-session-id operation)))
     :participant-id (copy-sequence (plist-get identities :participant-id))
     :work (e-chat-service-create-operation-work operation))))

(cl-defun e-chat-service-create-session-start (&key harness metadata id creation-key)
  "Start durable chat session creation and return a stable `e-work'.

When CREATION-KEY is supplied, return an owner-admission ticket with a
provisional Board id.  The historical explicit ID form remains a work handle
for generic chat callers; it has no stable-key promise."
  (if creation-key
      (e-chat-service-owner-admission-start
       :harness harness :creation-key creation-key :metadata metadata)
    (let* ((harness (or harness (e-chat-service-default-harness)))
           (store (e-harness-sessions harness))
           (session-id (or id (e-session-generate-id))))
      (unless (e-session-storage-sqlite-p store)
        (signal 'e-session-storage-error
                (list "Chat session creation requires SQLite" session-id)))
      (let* ((operation
              (e-chat-service--create-operation-create
               :harness harness :store store :session-id session-id
               :metadata (e-harness--normalize-session-metadata metadata)))
             (arguments
              (list :harness harness
                    :metadata (e-harness--normalize-session-metadata metadata)))
             (work
              (e-work-prepare
               e-chat-service--create-operation-spec arguments
               :context (list :domain-ref session-id
                              :work-kind 'chat-session-create))))
        (setf (e-chat-service-create-operation-work operation) work
              (e-work-handle-arguments work) arguments)
        (e-work-start-prepared work :arguments operation)
        work))))

(defun e-chat-service--open-target-board-id (target)
  "Return TARGET's detached Board id for an asynchronous open."
  (cond
   ((and (e-chat-service-binding-p target)
         (e-chat-service--sql-binding-p target))
    (e-chat-service-binding-board-id target))
   ((and (stringp target) (not (string-empty-p target))) target)
   (t
    (signal 'wrong-type-argument
            (list 'sqlite-board-id-or-binding-p target)))))

(defun e-chat-service--owner-association-valid-p
    (association board-id generation trusted-principal)
  "Signal when detached OWNER ASSOCIATION is not an exact Board tuple.
The check is intentionally application-owned: the SQL adapter returns
candidates, while this service validates the identity joins before any live
binding is installed."
  (let* ((candidate-board-id (plist-get association :board-id))
         (role (plist-get association :association-role))
         (policy (plist-get association :routing-policy))
         (principal (plist-get association :principal))
         (session-id (plist-get association :session-id))
         (participant-id (plist-get association :participant-id))
         (participant (plist-get association :participant))
         (policy-participant-id (plist-get policy :participant-id)))
    (unless (and (equal candidate-board-id board-id)
                 (integerp generation) (> generation 0)
                 (stringp session-id) (not (string-empty-p session-id))
                 (member role '("owner" owner))
                 (stringp principal) (not (string-empty-p principal))
                 (or (null trusted-principal)
                     (equal principal trusted-principal))
                 (e-session-board-routing-policy-valid-p policy)
                 (stringp participant-id)
                 (equal participant-id policy-participant-id)
                 (listp participant)
                 (equal participant-id (plist-get participant :id))
                 (member (plist-get participant :role) '(owner "owner"))
                 (or (null (plist-get participant :principal))
                     (equal principal (plist-get participant :principal))))
      (signal 'e-session-error
              (list "Malformed or conflicting Board owner association"
                    board-id association)))
    association))

(defun e-chat-service--exact-live-owner-binding
    (harness association)
  "Return or reject HARNESS's existing exact live OWNER ASSOCIATION.
No replacement or mutation is attempted when a process-local binding points at
another durable identity."
  (let* ((session-id (plist-get association :session-id))
         (binding (e-chat-service-binding harness session-id)))
    (when binding
      (unless (and (equal (e-chat-service-binding-board-id binding)
                          (plist-get association :board-id))
                   (equal (e-chat-service-binding-principal binding)
                          (plist-get association :principal))
                   (equal (e-chat-service-binding-participant-id binding)
                          (plist-get association :participant-id))
                   (equal (e-chat-service-binding-default-tags binding)
                          (plist-get (plist-get association :routing-policy)
                                     :default-tags))
                   (equal (e-chat-service-binding-default-to binding)
                          (plist-get (plist-get association :routing-policy)
                                     :default-to)))
        (signal 'e-session-error
                (list "Existing live Board binding conflicts with owner"
                      (plist-get association :board-id) session-id)))
      binding)))

(defun e-chat-service--finish-owner-open-operation
    (operation result error)
  "Settle owner OPEN operation exactly once with RESULT or ERROR."
  (unless (e-chat-service-owner-open-operation-settled operation)
    (setf (e-chat-service-owner-open-operation-settled operation) t
          (e-chat-service-owner-open-operation-child operation) nil
          (e-chat-service-owner-open-operation-binding-work operation) nil)
    (if error
        (e-work-fail (e-chat-service-owner-open-operation-work operation) error)
      (e-work-finish (e-chat-service-owner-open-operation-work operation)
                     result))))

(defun e-chat-service--owner-open-association-settled
    (operation child)
  "Validate owner candidates from CHILD and install one exact binding."
  (let ((status (e-work-status child)))
    (if (not (eq (plist-get status :state) 'finished))
        (e-chat-service--finish-owner-open-operation
         operation nil
         (or (plist-get status :error)
             '(e-work-cancelled "Board owner resolution cancelled")))
      (condition-case error
          (let* ((result (plist-get status :result))
                 (board-id (e-chat-service-owner-open-operation-board-id operation))
                 (owners (plist-get result :owners))
                 (generation (plist-get result :generation))
                 (trusted-principal (plist-get result :trusted-principal)))
            (when (plist-get result :missing-board)
              (signal 'e-session-missing (list board-id 'board)))
            (unless (= (length owners) 1)
              (signal 'e-session-error
                      (list (if (null owners)
                                "Board has no current owner"
                              "Board has multiple current owners")
                            board-id (length owners))))
            (let* ((association
                    (e-chat-service--owner-association-valid-p
                     (car owners) board-id generation trusted-principal))
                   (harness
                    (e-chat-service-owner-open-operation-harness operation))
                   (binding (e-chat-service--exact-live-owner-binding
                             harness association)))
              (if binding
                  (e-chat-service--finish-owner-open-operation operation binding nil)
                (let ((binding-work
                       (e-chat-service-binding-start
                        harness (plist-get association :session-id)
                        association t)))
                  (setf (e-chat-service-owner-open-operation-association operation)
                        (copy-tree association t)
                        (e-chat-service-owner-open-operation-binding-work operation)
                        binding-work)
                  (e-work-on-settle
                   binding-work
                   (lambda (settled)
                     (let ((binding-status (e-work-status settled)))
                       (if (eq (plist-get binding-status :state) 'finished)
                           (e-chat-service--finish-owner-open-operation
                            operation (plist-get binding-status :result) nil)
                         (e-chat-service--finish-owner-open-operation
                          operation nil (plist-get binding-status :error))))))))))
        (error
         (e-chat-service--finish-owner-open-operation operation nil error))))))

(defun e-chat-service--start-owner-open-operation (operation)
  "Start the detached Board owner candidate query for OPERATION."
  (let* ((harness (e-chat-service-owner-open-operation-harness operation))
         (store (e-harness-sessions harness))
         (service (e-board-sqlite-service-create
                   (e-session-storage-runtime-store store)))
         (child
          (e-board-sqlite-service-board-owner-resolve-start
           service (e-chat-service-owner-open-operation-board-id operation))))
    (setf (e-chat-service-owner-open-operation-child operation) child
          (e-work-handle-cancel-function
           (e-chat-service-owner-open-operation-work operation))
          (lambda (_handle)
            (when (and (e-work-handle-p child)
                       (not (memq (plist-get (e-work-status child) :state)
                                  '(finished failed cancelled))))
              (e-work-cancel child))))
    (e-work-on-settle
     child
     (lambda (settled)
       (e-chat-service--owner-open-association-settled operation settled)))))

(cl-defun e-chat-service-open-board-owner-start
    (board-id harness &key)
  "Open exactly one current owner binding for BOARD-ID asynchronously.
The request never creates or repairs a Board.  Zero, multiple, missing, or
identity-conflicting durable rows fail only this request."
  (unless (and (stringp board-id) (not (string-empty-p board-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p board-id)))
  (unless (e-session-storage-sqlite-p (e-harness-sessions harness))
    (signal 'e-session-storage-error
            (list "Board owner open requires SQLite" board-id)))
  (let* ((operation
          (e-chat-service--owner-open-operation-create
           :board-id (copy-sequence board-id) :harness harness))
         (work
          (e-work-prepare
           e-chat-service--owner-open-operation-spec operation
           :context (list :domain-ref board-id :work-kind 'chat-board-owner-open))))
    (setf (e-chat-service-owner-open-operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    work))

(defun e-chat-service--finish-legacy-resolve-operation
    (operation result error)
  "Settle legacy RESOLVE OPERATION exactly once."
  (unless (e-chat-service-legacy-resolve-operation-settled operation)
    (setf (e-chat-service-legacy-resolve-operation-settled operation) t
          (e-chat-service-legacy-resolve-operation-child operation) nil)
    (if error
        (e-work-fail (e-chat-service-legacy-resolve-operation-work operation)
                     error)
      (e-work-finish (e-chat-service-legacy-resolve-operation-work operation)
                     result))))

(defun e-chat-service--legacy-owner-settled (operation settled)
  "Validate the exact owner candidate from legacy resolver SETTLED."
  (let ((status (e-work-status settled)))
    (if (not (eq (plist-get status :state) 'finished))
        (e-chat-service--finish-legacy-resolve-operation
         operation nil (plist-get status :error))
      (condition-case owner-error
          (let* ((result (plist-get status :result))
                 (session-id
                  (e-chat-service-legacy-resolve-operation-session-id operation))
                 (board-id
                  (plist-get result :board-id))
                 (owners (plist-get result :owners))
                 (generation (plist-get result :generation))
                 (trusted-principal (plist-get result :trusted-principal))
                 (matching
                  (cl-remove-if-not
                   (lambda (candidate)
                     (equal (plist-get candidate :session-id) session-id))
                   owners)))
            (when (plist-get result :missing-board)
              (signal 'e-session-missing (list board-id 'board)))
            (unless (= (length matching) 1)
              (signal 'e-session-error
                      (list (if (null matching)
                                "Legacy session owner is missing"
                              "Legacy session has multiple owners")
                            session-id board-id)))
            (e-chat-service--finish-legacy-resolve-operation
             operation
             (list :association
                   (e-chat-service--owner-association-valid-p
                    (car matching) board-id generation trusted-principal)
                   :board-id board-id :generation generation)
             nil))
        (error
         (e-chat-service--finish-legacy-resolve-operation
          operation nil owner-error))))))

(defun e-chat-service--legacy-association-settled (operation settled)
  "Validate legacy association SETTLED and query its exact Board owner."
  (let ((status (e-work-status settled)))
    (if (not (eq (plist-get status :state) 'finished))
        (e-chat-service--finish-legacy-resolve-operation
         operation nil (plist-get status :error))
      (condition-case resolve-error
          (let* ((association (plist-get status :result))
                 (session-id
                  (e-chat-service-legacy-resolve-operation-session-id operation))
                 (board-id (plist-get association :board-id)))
            (unless (and association
                         (equal (plist-get association :session-id) session-id)
                         (member (plist-get association :association-role)
                                 '(owner "owner"))
                         (stringp board-id)
                         (not (string-empty-p board-id)))
              (signal 'e-session-error
                      (list "Legacy session is not a valid Board owner"
                            session-id)))
            (setf (e-chat-service-legacy-resolve-operation-association operation)
                  (copy-tree association t))
            (let* ((harness
                    (e-chat-service-legacy-resolve-operation-harness operation))
                   (service
                    (e-board-sqlite-service-create
                     (e-session-storage-runtime-store
                      (e-harness-sessions harness))))
                   (owner-work
                    (e-board-sqlite-service-board-owner-resolve-start
                     service board-id)))
              (setf (e-chat-service-legacy-resolve-operation-child operation)
                    owner-work)
              (e-work-on-settle
               owner-work
               (lambda (owner-settled)
                 (e-chat-service--legacy-owner-settled
                  operation owner-settled)))))
        (error
         (e-chat-service--finish-legacy-resolve-operation
          operation nil resolve-error))))))

(defun e-chat-service--start-legacy-resolve-operation (operation)
  "Resolve one valid legacy SESSION-ID association without mutation."
  (let* ((harness (e-chat-service-legacy-resolve-operation-harness operation))
         (child
          (e-session-async-board-association
           (e-harness-sessions harness)
           (e-chat-service-legacy-resolve-operation-session-id operation))))
    (setf (e-chat-service-legacy-resolve-operation-child operation) child
          (e-work-handle-cancel-function
           (e-chat-service-legacy-resolve-operation-work operation))
          (lambda (_handle)
            (let ((current
                   (e-chat-service-legacy-resolve-operation-child operation)))
              (when (and (e-work-handle-p current)
                         (not (memq (plist-get (e-work-status current) :state)
                                    '(finished failed cancelled))))
                (e-work-cancel current)))))
    (e-work-on-settle
     child
     (lambda (settled)
       (e-chat-service--legacy-association-settled operation settled)))))

(defun e-chat-service-resolve-legacy-session-start (harness session-id)
  "Resolve legacy SESSION-ID as a valid owner association without mutation."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p session-id)))
  (unless (e-session-storage-sqlite-p (e-harness-sessions harness))
    (signal 'e-session-storage-error
            (list "Legacy Board migration requires SQLite" session-id)))
  (let* ((operation
          (e-chat-service--legacy-resolve-operation-create
           :harness harness :session-id (copy-sequence session-id)))
         (work
          (e-work-prepare
           e-chat-service--legacy-resolve-operation-spec operation
           :context (list :domain-ref session-id
                          :work-kind 'chat-legacy-session-resolve))))
    (setf (e-chat-service-legacy-resolve-operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    work))

(defun e-chat-service--finish-open-operation (operation result error)
  "Settle SQLite Board open OPERATION exactly once."
  (unless (e-chat-service-open-operation-settled operation)
    (setf (e-chat-service-open-operation-settled operation) t
          (e-chat-service-open-operation-child operation) nil)
    (if error
        (e-work-fail (e-chat-service-open-operation-work operation) error)
      (e-work-finish (e-chat-service-open-operation-work operation) result))))

(defun e-chat-service--open-binding-settled (operation child)
  "Settle OPERATION from its exact binding CHILD."
  (let ((status (e-work-status child)))
    (pcase (plist-get status :state)
      ('finished
       (e-chat-service--finish-open-operation
        operation (plist-get status :result) nil))
      ('failed
       (e-chat-service--finish-open-operation
        operation nil (plist-get status :error)))
      ('cancelled
       (e-chat-service--finish-open-operation
        operation nil '(e-work-cancelled "Board open cancelled"))))))

(defun e-chat-service--open-association-settled (operation child)
  "Validate CHILD's detached association and continue OPERATION."
  (let ((status (e-work-status child)))
    (if (not (eq (plist-get status :state) 'finished))
        (e-chat-service--finish-open-operation
         operation nil
         (or (plist-get status :error)
             '(e-work-cancelled "Board association query cancelled")))
      (condition-case error
          (let* ((association (plist-get status :result))
                 (target (e-chat-service-open-operation-target operation))
                 (board-id (e-chat-service--open-target-board-id target))
                 (policy (plist-get association :routing-policy))
                 (routing (e-chat-service-open-operation-routing-arguments
                           operation)))
            (unless (and association
                         (equal board-id (plist-get association :board-id)))
              (signal 'e-session-missing
                      (list (e-chat-service-open-operation-session-id operation)
                            'board-association)))
            (when (and (e-chat-service-binding-p target)
                       (not (equal (e-chat-service-binding-principal target)
                                   (plist-get association :principal))))
              (signal 'e-session-error
                      (list "Board principal conflicts with durable association"
                            board-id)))
            (unless (e-session-board-routing-policy-valid-p policy)
              (signal 'e-session-error
                      (list "Malformed durable Board routing policy"
                            (e-chat-service-open-operation-session-id operation))))
            (when (apply #'e-chat-service--routing-override-conflicts-p
                         policy routing)
              (signal 'e-session-error
                      (list "Routing policy override conflicts with durable state"
                            (e-chat-service-open-operation-session-id operation))))
            (let ((binding-work
                   (e-chat-service-binding-start
                    (e-chat-service-open-operation-harness operation)
                    (e-chat-service-open-operation-session-id operation)
                    association)))
              (setf (e-chat-service-open-operation-child operation)
                    binding-work)
              (e-work-on-settle
               binding-work
               (lambda (settled)
                 (e-chat-service--open-binding-settled operation settled)))))
        (error
         (e-chat-service--finish-open-operation operation nil error))))))

(defun e-chat-service--start-open-operation (operation)
  "Start OPERATION's detached exact association query."
  (let ((child
         (e-session-async-board-association
          (e-harness-sessions
           (e-chat-service-open-operation-harness operation))
          (e-chat-service-open-operation-session-id operation))))
    (setf (e-chat-service-open-operation-child operation) child)
    (setf (e-work-handle-cancel-function
           (e-chat-service-open-operation-work operation))
          (lambda (_handle)
            (when-let* ((current
                         (e-chat-service-open-operation-child operation)))
              (unless (memq (plist-get (e-work-status current) :state)
                            '(finished failed cancelled))
                (e-work-cancel current)))))
    (e-work-on-settle
     child
     (lambda (settled)
       (e-chat-service--open-association-settled operation settled)))))

(cl-defun e-chat-service-open-board-start
    (target harness &optional session-id
            &key (participant-id nil participant-id-supplied-p)
            (pickup-selector nil pickup-selector-supplied-p)
            (observer-selector nil observer-selector-supplied-p)
            (default-tags nil default-tags-supplied-p)
            (default-to nil default-to-supplied-p))
  "Asynchronously open one exact Board owner or existing SESSION-ID binding.
When SESSION-ID is omitted, TARGET is resolved through the Board-owned exact
owner operation; no owner is created or repaired."
  (if (null session-id)
      (e-chat-service-open-board-owner-start target harness)
    (unless (e-session-storage-sqlite-p (e-harness-sessions harness))
      (signal 'e-session-storage-error
              (list "Asynchronous Board open requires SQLite" session-id)))
    (e-chat-service--open-target-board-id target)
    (let* ((routing-arguments
          (list participant-id participant-id-supplied-p
                pickup-selector pickup-selector-supplied-p
                observer-selector observer-selector-supplied-p
                default-tags default-tags-supplied-p
                default-to default-to-supplied-p))
         (operation
          (e-chat-service--open-operation-create
           :target target :harness harness :session-id session-id
           :routing-arguments routing-arguments))
         (work
          (e-work-prepare
           e-chat-service--open-operation-spec operation
           :context (list :domain-ref session-id
                          :work-kind 'chat-board-open))))
      (setf (e-chat-service-open-operation-work operation) work)
      (e-work-start-prepared work :arguments operation)
      work)))

(defun e-chat-service--finish-participant-operation
    (operation result error)
  "Settle participant OPERATION exactly once with RESULT or ERROR."
  (unless (e-chat-service-participant-operation-settled operation)
    (setf (e-chat-service-participant-operation-settled operation) t)
    (let ((work (e-chat-service-participant-operation-work operation))
          (binding (e-chat-service-participant-operation-binding operation)))
      (if error
          (progn
            (when binding
              (setf (e-chat-service-binding-first-persistence-error binding)
                    (copy-tree error t)
                    (e-chat-service-participant-operation-binding operation)
                    nil))
            ;; Settlement is the invariant even if an unexpected local
            ;; inverse signals.  Preserve and publish the first admission
            ;; error, then let a cleanup defect remain visible to its caller.
            (unwind-protect
                (when binding
                  (e-chat-service--discard-binding binding))
              (e-work-fail work error)))
        (when binding
          (e-chat-service--publish-ready-binding binding))
        (e-work-finish work (copy-tree result t))))))

(defun e-chat-service--start-sql-participant-operation (operation parent)
  "Admit OPERATION through coordination-only SQL PARENT."
  (let* ((harness (e-chat-service-participant-operation-harness operation))
         (store (e-chat-service-participant-operation-store operation))
         (session-id
          (e-chat-service-participant-operation-session-id operation))
         (board-id (e-chat-service-binding-board-id parent))
         (principal (e-chat-service-binding-principal parent))
         (participant-id
          (or (e-chat-service-participant-operation-participant-id operation)
              (concat
               "ptc_"
               (substring
                (secure-hash
                 'sha256 (format "chat-participant:%s:%s" board-id session-id))
                0 32))))
         (pickup-selector
          (or (e-chat-service-participant-operation-pickup-selector operation)
              '(:tags (main))))
         (observer-selector
          (or (e-chat-service-participant-operation-observer-selector operation)
              '(:tags (main))))
         (default-tags
          (or (e-chat-service-participant-operation-default-tags operation)
              '(main)))
         (routing-policy
          (e-chat-service--routing-policy
           participant-id pickup-selector observer-selector default-tags
           (e-chat-service-participant-operation-default-to operation)))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id
           :metadata (e-chat-service-participant-operation-metadata operation)
           :principal principal :board-id board-id
           :association-role e-chat-service--board-role-participant
           :routing-policy routing-policy))
         (records (plist-get session :admission-records))
         (query-delta (plist-get session :query-delta))
         (participant
          (list :id participant-id :author "e-chat" :principal principal
                :controller principal :role 'participant :state 'active
                :name
                (e-chat-service--participant-name
                 (e-chat-service-participant-operation-metadata operation)
                 'participant)
                :subscription-id (concat "sub_" participant-id)
                :publication-pending nil))
         (child
          (e-board-sqlite-service-admit-participant-start
           (e-chat-service-binding-sqlite-service parent)
           session-id board-id records query-delta participant)))
    (setf (e-chat-service-participant-operation-session-result operation)
          (copy-tree session t)
          (e-chat-service-participant-operation-child operation) child)
    (e-work-on-settle
     child
     (lambda (settled)
       (setf (e-chat-service-participant-operation-child operation) nil)
       (let ((status (e-work-status settled)))
         (if (not (eq (plist-get status :state) 'finished))
             (e-chat-service--finish-participant-operation
              operation nil
              (e-session-note-persistence-failure
               store session-id
               (or (plist-get status :error)
                   '(e-work-cancelled "Participant admission cancelled"))))
           (condition-case error
               (let* ((result (plist-get status :result))
                      (association (plist-get result :association))
                      (binding
                       (e-chat-service--install-sqlite-binding
                        harness session-id association nil nil)))
                 (setf (e-chat-service-participant-operation-binding operation)
                       binding)
                 (e-chat-service--finish-participant-operation
                  operation session nil))
             (error
              (e-chat-service--finish-participant-operation
               operation nil error)))))))))

(defun e-chat-service--start-participant-operation (operation)
  "Enqueue OPERATION through its live SQL parent binding."
  (condition-case error
      (let ((parent
             (e-chat-service-participant-operation-parent-binding operation)))
        (unless (e-chat-service--sql-binding-p parent)
          (signal 'wrong-type-argument
                  (list 'e-chat-sql-binding-p parent)))
        (e-chat-service--start-sql-participant-operation operation parent))
    (error
     (e-chat-service--finish-participant-operation operation nil error))))
(cl-defun e-chat-service-create-participant-start
    (parent-binding harness &key metadata id participant-id pickup-selector
           observer-selector (default-tags '(main)) default-to)
  "Start private participant admission and immediately return stable work.

PARENT-BINDING is a live SQL coordination value.  SQLite identity and
association rows are admitted by one transaction; no aggregate is created."
  (let* ((store (e-harness-sessions harness))
         (session-id (or id (e-session-generate-id))))
    (unless (e-session-storage-sqlite-p store)
      (signal 'e-session-storage-error
              (list "Chat participant admission requires SQLite" session-id)))
    (unless (e-chat-service--sql-binding-p parent-binding)
      (signal 'wrong-type-argument
              (list 'e-chat-sql-binding-p parent-binding)))
    (let* ((operation
            (e-chat-service--participant-operation-create
             :parent-binding parent-binding :harness harness :store store
             :session-id session-id
             :metadata (e-harness--normalize-session-metadata metadata)
             :participant-id participant-id
             :pickup-selector
             (if (consp pickup-selector)
                 (copy-tree pickup-selector t)
               pickup-selector)
             :observer-selector
             (if (consp observer-selector)
                 (copy-tree observer-selector t)
               observer-selector)
             :default-tags (copy-tree default-tags t)
             :default-to
             (if (consp default-to) (copy-tree default-to t) default-to)))
           (work
            (e-work-prepare
             e-chat-service--participant-operation-spec operation
             :context (list :domain-ref session-id
                            :work-kind 'chat-participant-create))))
      (setf (e-chat-service-participant-operation-work operation) work)
      (e-work-start-prepared work :arguments operation)
      work)))

(defun e-chat-service--harness-has-capability-p (harness capability-id)
  "Return non-nil when HARNESS has active capability CAPABILITY-ID."
  (memq capability-id
        (mapcar #'e-capability-id
                (e-harness-active-capabilities harness))))

(defun e-chat-service--harness-for-instance (instance)
  "Return the live chat harness for INSTANCE."
  (let* ((instance-id (e-harness-instance-id instance))
         (harness
          (condition-case err
              (e-harness-instance-get-or-create instance-id)
            ((e-harness-instance-missing e-harness-registry-missing)
             (user-error "No e harness registered for %S" (cadr err))))))
    (unless (e-chat-service--harness-has-capability-p harness 'chat-session)
      (user-error "Harness instance %S does not provide chat-session capability"
                  instance-id))
    harness))

(defun e-chat-service-default-harness-id ()
  "Return the effective default chat harness id."
  (if (boundp 'e-chat-default-harness-id)
      e-chat-default-harness-id
    e-chat-service-default-harness-id))

(defun e-chat-service-default-harness ()
  "Return the configured default chat harness."
  (let* ((harness-id (e-chat-service-default-harness-id))
         (instance (e-harness-instance-get harness-id))
         (harness
          (if instance
              (e-chat-service--harness-for-instance instance)
            (condition-case err
                (e-harness-registry-get-or-create harness-id)
              (e-harness-registry-missing
               (user-error "No e harness registered for %S" (cadr err)))))))
    (unless (e-chat-service--harness-has-capability-p harness 'chat-session)
      (user-error "Harness %S does not provide chat-session capability"
                  harness-id))
    harness))

(cl-defun e-chat-service--subscribe
    (harness session-id function &key start-seq)
  "Create one SQLite change subscription for FUNCTION after START-SEQ."
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (let* ((binding (or (e-chat-service-binding harness session-id)
                      (signal 'e-session-error
                              (list "SQL Board binding is not ready"
                                    session-id))))
         (_capacity
          (when (>= (length (e-chat-service-binding-subscribers binding))
                    e-chat-service-subscriber-limit)
            (user-error "Chat board subscriber limit reached for %s"
                        session-id)))
         (subscription
          (e-chat-service--subscription-create
           :binding binding :function function :active-p t
           :state 'active :lifecycle-generation 0
           :sqlite-cursor start-seq)))
    (e-chat-service--cancel-idle-close binding)
    (setf (e-chat-service-binding-subscribers binding)
          (cons subscription (e-chat-service-binding-subscribers binding)))
    (e-chat-service--schedule-subscription-drain subscription)
    subscription))

(defun e-chat-service-subscribe (harness session-id function)
  "Subscribe FUNCTION from a bounded SQLite view for HARNESS SESSION-ID."
  (e-chat-service--subscribe harness session-id function))

(defun e-chat-service-subscribe-from-cursor
    (harness session-id cursor function)
  "Subscribe FUNCTION strictly after SQLite Board CURSOR."
  (unless (and (integerp cursor) (>= cursor 0))
    (signal 'wrong-type-argument (list 'natnump cursor)))
  (e-chat-service--subscribe
   harness session-id function :start-seq cursor))

(defun e-chat-service-unsubscribe (subscription)
  "Idempotently retire board-observer SUBSCRIPTION."
  (when (e-chat-service-subscription-p subscription)
    (e-chat-service--retire-subscription subscription))
  nil)

(cl-defun e-chat-service-post
    (binding prompt &key (mode 'inject) tags attributes to references metadata)
  "Post PROMPT through board-first BINDING with generic routing fields."
  (unless (e-chat-service-binding-p binding)
    (signal 'wrong-type-argument (list 'e-chat-service-binding-p binding)))
  (e-chat-service--post
   (e-chat-service-binding-harness binding)
   (e-chat-service-binding-session-id binding)
   prompt mode :tags tags :attributes attributes :to to
   :references references :metadata metadata))

(cl-defun e-chat-service--post-sqlite
    (harness session-id prompt mode &key references metadata tags attributes to
             source-input-key)
  "Submit PROMPT immediately through SQLite and return its admission work."
  (let* ((store (e-harness-sessions harness))
         (binding (e-chat-service-binding harness session-id))
         (pending-table (e-chat-service--harness-pending-creations harness))
         (pending (gethash session-id pending-table))
         (service
          (or (and (e-chat-service--sql-binding-p binding)
                   (e-chat-service-binding-sqlite-service binding))
              (e-board-sqlite-service-create
               (e-session-storage-runtime-store store))))
         (source-key
          (or (copy-tree source-input-key t)
              (list "chat-admission" session-id (e-session-generate-id))))
         (effective-tags
          (or (copy-tree tags t)
              (and binding
                   (copy-tree (e-chat-service-binding-default-tags binding) t))
              '(main)))
         (effective-attributes
          (append (copy-tree attributes t) (copy-tree metadata t)
                  (and references
                       (list :references (copy-tree references t)))))
         (work
          (if pending
              (progn
                (when (e-chat-service-create-operation-first-input-work pending)
                  (user-error "Initial chat admission is already pending"))
                (let* ((data
                        (e-chat-service--pending-owner-admission-data pending))
                       (admission-work
                        (e-board-sqlite-service-admit-session-input-start
                         service session-id
                         (plist-get data :board-id)
                         (plist-get data :principal)
                         (plist-get data :records)
                         (plist-get data :query-delta)
                         (plist-get data :participant)
                         :author (format "session:%s" session-id)
                         :tags effective-tags
                         :attributes effective-attributes
                         :to to :mode mode :content prompt
                         :reference (copy-tree references t)
                         :source-input-key source-key)))
                  (setf
                   (e-chat-service-create-operation-first-input-work pending)
                   admission-work)
                  admission-work))
            (e-board-sqlite-service-append-route-start
             service nil :session-id session-id
             :author (format "session:%s" session-id)
             :tags effective-tags :attributes effective-attributes
             :to (or to
                     (and binding
                          (e-chat-service-binding-default-to binding)))
             :mode mode :content prompt :reference (copy-tree references t)
             :source-input-key source-key))))
    (e-work-on-settle
     work
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (if (not (eq (plist-get status :state) 'finished))
             (progn
               (when pending
                 (remhash session-id pending-table)
                 (unless (e-chat-service-create-operation-settled pending)
                   (setf (e-chat-service-create-operation-settled pending) t)
                   (e-work-fail
                    (e-chat-service-create-operation-work pending)
                    (plist-get status :error))))
               (when-let* ((current
                            (e-chat-service-binding harness session-id)))
                 (e-chat-service--sql-note-failure
                  current (plist-get status :error) t)))
           (let* ((result (plist-get status :result))
                  (association (plist-get result :association))
                  (current
                   (or (e-chat-service-binding harness session-id)
                       (and association
                            (e-chat-service--install-sqlite-binding
                             harness session-id association nil t)))))
             (when pending
               (remhash session-id pending-table)
               (unless (e-chat-service-create-operation-settled pending)
                 (setf (e-chat-service-create-operation-settled pending) t)
                 (e-work-finish
                  (e-chat-service-create-operation-work pending)
                  (list :id session-id
                        :metadata
                        (copy-tree
                         (e-chat-service-create-operation-metadata pending) t)
                        :association
                        (copy-tree association t)))))
             (when (and current
                        (eq (plist-get result :status) 'posted))
               (e-chat-service--sql-notify-event
                current
                (e-chat-service--sql-message-event
                 current (plist-get result :message))))
             (when current
               (when-let* ((pickup
                            (seq-find
                             (lambda (candidate)
                               (equal
                                (plist-get candidate :participant-id)
                                (e-chat-service-binding-participant-id
                                 current)))
                             (plist-get result :pickups))))
                 (e-chat-service--sql-deliver-pickup current pickup))))))))
    work))

(cl-defun e-chat-service--post
    (harness session-id prompt mode &key references metadata tags attributes to source-input-key)
  "Enqueue PROMPT for HARNESS SESSION-ID without awaiting SQLite.

The returned value is request-scoped admission work.  SQLite assigns the
canonical message and delivery identities; callers learn those facts only
from work settlement and canonical Board publication."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (unless (e-session-storage-sqlite-p (e-harness-sessions harness))
    (signal 'e-session-storage-error
            (list "Chat submission requires SQLite" session-id)))
  (e-chat-service--post-sqlite
   harness session-id prompt mode :references references
   :metadata metadata :tags tags :attributes attributes :to to
   :source-input-key source-input-key))

(cl-defun e-chat-service-submit-session
    (harness session-id prompt &key references metadata)
  "Submit PROMPT and return request-scoped admission work."
  (e-chat-service--post harness session-id prompt 'inject
                        :references references :metadata metadata))

(cl-defun e-chat-service-steer-session
    (harness session-id prompt &key metadata)
  "Post steering PROMPT and return request-scoped admission work."
  (e-chat-service--post harness session-id prompt 'inject :metadata metadata))

(cl-defun e-chat-service-queue-session
    (harness session-id prompt &key references metadata source-input-key)
  "Queue PROMPT and return request-scoped admission work.
SOURCE-INPUT-KEY lets durable callers retry one queued input exactly once."
  (e-chat-service--post harness session-id prompt 'queue
                        :references references :metadata metadata
                        :source-input-key source-input-key))

(defun e-chat-service-abort-session (harness session-id)
  "Abort the current board-bound turn for HARNESS SESSION-ID."
  (when-let ((binding (e-chat-service-binding harness session-id)))
    (e-harness-attached-turn-port-abort
     (e-chat-service-binding-turn-port binding))))

(defun e-chat-service--binding-active-turn (binding)
  "Return BINDING's live harness turn, if one is running."
  (let ((turn
         (e-harness-attached-turn-port-active-turn
          (e-chat-service-binding-turn-port binding))))
    (and (eq (plist-get turn :status) 'running) turn)))

(defun e-chat-service-active-turn (harness session-id)
  "Return SESSION-ID's running turn using presentation-facing identity.
The returned `:id' is in the same namespace as board-derived event `:turn-id'
values delivered by `e-chat-service-subscribe'.  This is a live-controller
query: an unopened persistent Board has no process-local running turn and must
not trigger a durable read merely because presentation code asks for status."
  (when-let* ((binding (e-chat-service-binding harness session-id)))
    (e-chat-service--binding-active-turn binding)))

(defun e-chat-service-active-turn-p (harness session-id)
  "Return non-nil when HARNESS SESSION-ID's board participant is running."
  (and (e-chat-service-active-turn harness session-id) t))

(defun e-chat-service-session-options (harness session-id)
  "Return the effective option projection for SESSION-ID.
Chat presentation controls use this service operation instead of depending on
the harness context owner directly."
  (e-harness-session-options harness session-id))

(defun e-chat-service-session-store (harness)
  "Return HARNESS's private session store for controlled shell metadata work."
  (e-harness-sessions harness))

(defun e-chat-service--query-row-session-summary (row)
  "Return the bounded shell summary represented by detached query ROW."
  (let* ((session-id (plist-get row :session-id))
         (board-id (plist-get row :board-id))
         (principal (plist-get row :principal))
         (association
          (when (or board-id principal)
            (append
             (list :board-id board-id :principal principal)
             (when-let* ((role (plist-get row :association-role)))
               (list :association-role role))
             (when-let* ((policy (plist-get row :routing-policy)))
               (list :routing-policy (copy-tree policy t)))))))
    (list :id session-id
          :name (plist-get row :name)
          :summary (plist-get row :summary)
          :title (or (plist-get row :name)
                     (plist-get row :summary)
                     session-id)
          :metadata (copy-tree (plist-get row :metadata) t)
          :created-at (plist-get row :created-at)
          :updated-at (plist-get row :updated-at)
          :last-message-at (plist-get row :last-message-at)
          :latest-assistant-marker
          (copy-tree (plist-get row :latest-assistant-marker) t)
          :message-count (plist-get row :message-count)
          :current-branch (plist-get row :current-branch)
          :turn-options (copy-tree (plist-get row :turn-options) t)
          :board-id board-id
          :principal principal
          :association association
          :journal-position (plist-get row :journal-position))))

(cl-defun e-chat-service-root-session-page-start
    (harness &key cursor (limit e-chat-service-session-summary-page-limit))
  "Return immediately with work reading one bounded root-session page.

CURSOR is a prior page's stable SQLite cursor.  The returned work owns its
detached result; no page is installed in HARNESS or a process-wide catalog."
  (let ((store (e-harness-sessions harness)))
    (unless (e-session-storage-sqlite-p store)
      (signal 'e-session-storage-error
              (list "Chat session navigation requires SQLite")))
    (e-session-async-query-page
     store :cursor cursor :limit limit :root-p t)))

(defun e-chat-service-root-session-page-value (work)
  "Return WORK's detached shell-shaped root-session page.

Signal when WORK has not finished successfully.  This accessor performs only
bounded in-process mapping and never waits for storage."
  (let ((status (e-work-status work)))
    (unless (eq (plist-get status :state) 'finished)
      (signal 'e-session-storage-error
              (list "Root session page is not available"
                    (plist-get status :state)
                    (plist-get status :error))))
    (let* ((page (plist-get status :result))
           (rows
            (mapcar #'e-chat-service--query-row-session-summary
                    (plist-get page :rows))))
      (list :rows (cl-remove-if-not #'e-chat-service--root-session-p rows)
            :next (copy-tree (plist-get page :next) t)
            :limit (plist-get page :limit)
            :byte-count (plist-get page :byte-count)
            :byte-limit (plist-get page :byte-limit)))))

(defun e-chat-service--root-session-p (session)
  "Return non-nil when detached SQL summary SESSION is a chat Board owner."
  (let ((state (plist-get session :association)))
    (and (stringp (plist-get session :board-id))
         (stringp (plist-get session :principal))
         (equal (plist-get state :association-role)
                e-chat-service--board-role-root))))

(defun e-chat-service-state (harness session-id)
  "Return SESSION-ID's process-local SQL coordination state."
  (if-let* ((binding (e-chat-service-binding harness session-id)))
      (list :board-id (e-chat-service-binding-board-id binding)
            :message-count nil
            :active-turn (e-chat-service--binding-active-turn binding))
    (list :board-id nil :message-count 0 :active-turn nil)))

(defun e-chat-service-pending-hook-summary (harness session-id turn-id)
  "Return TURN-ID's pending-hook summary from bounded executing state.

This reads only the live turn input already owned by the harness.  It never
queries or reconstructs the durable session transcript."
  (when-let* ((messages
               (plist-get
                (e-harness-executing-session-state harness session-id)
                :messages))
              (prompt
               (seq-find
                (lambda (message)
                  (and (eq (plist-get message :role) 'user)
                       (equal (plist-get message :turn-id) turn-id)))
                messages)))
    (let ((summary (plist-get (plist-get prompt :metadata) :pending-summary)))
      (and (stringp summary) summary))))

(defun e-chat-service-active-capabilities (harness)
  "Return HARNESS's active capabilities for shell affordance discovery."
  (e-harness-active-capabilities harness))

(defun e-chat-service-structured-blocks (harness session-id)
  "Return SESSION-ID's private structured-block registry projection."
  (e-harness-structured-blocks harness session-id))

(defun e-chat-service-message-presentation (harness session-id message)
  "Return generic display content and details for durable MESSAGE.
The service applies the active structured-block registry once, then gives the
parsed blocks to capability-owned detail providers.  Presentation shells never
need to know a block kind or capability policy."
  (let ((content (plist-get message :content)))
    (if (not (and (eq (plist-get message :role) 'assistant)
                  (stringp content)))
        (list :content content :details nil)
      (let* ((registry (e-harness-structured-blocks
                        harness session-id (plist-get message :turn-id)))
             (rendered (e-structured-blocks-render content registry)))
        (list :content (plist-get rendered :text)
              :details
              (e-harness-message-details
               harness session-id message
               :structured-blocks (plist-get rendered :blocks)))))))

(defun e-chat-service-prompt-catalog (harness)
  "Return HARNESS's named prompt catalog for composer completion."
  (e-harness-prompts harness))

(defun e-chat-service-active-turns (harness)
  "Return HARNESS's board-derived active-turn index for shell diagnostics."
  (let ((result (make-hash-table :test 'equal)))
    (when-let ((bindings (gethash harness e-chat-service--bindings)))
      (maphash
       (lambda (session-id binding)
         (when (e-chat-service--binding-live-p binding)
           (when-let ((active-turn
                       (e-chat-service--binding-active-turn binding)))
             (puthash session-id active-turn result))))
       bindings))
    result))

(defun e-chat-service-append-seed-message (harness session-id message)
  "Enqueue explicit pre-turn seed MESSAGE for SQL SESSION-ID."
  (let ((store (e-harness-sessions harness)))
    (unless (e-session-storage-sqlite-p store)
      (signal 'e-session-storage-error
              (list "Chat seed append requires SQLite" session-id)))
    (e-session-append-message store session-id (copy-tree message t))))

(provide 'e-chat-service)

;;; e-chat-service.el ends here

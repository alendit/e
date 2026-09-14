;;; e-subagent-runner.el --- Subagent spawn and run coordination for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Spawn coordination for subagents.  The coordinator resolves a spawnable type
;; to its harness instance, creates a fresh child session carrying durable
;; lineage metadata, seeds the child's own store (default prompt-only, optional
;; explicit messages), admits it to the Board, and drives it through a
;; pluggable runner seam.  The default runner starts one non-blocking child turn
;; on the child harness and settles the private live capability record from turn
;; events.  Durable lifecycle/report facts are published to the Board; no
;; terminal result is retained in the live owner.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-chat-service)
(require 'e-session)
(require 'e-runtime-store-codec)
(require 'e-subagent-live)
(require 'e-work)
(require 'e-board-orchestration-actions)

(define-error 'e-subagent-error "e subagent error")
(define-error 'e-subagent-unknown-type
  "No spawnable subagent type is registered for id" 'e-subagent-error)

(defun e-subagent--lineage-id (live board-id parent-session-id)
  "Return the live tmp-lineage root for PARENT-SESSION-ID in LIVE.

Lineage is execution coordination, not a reason to synchronously reconstruct
the parent's durable session.  Follow only the bounded live execution owner;
an ordinary root session seeds its own lineage id."
  (let ((current parent-session-id)
        (remaining 64)
        parent)
    (while (and (> remaining 0)
                (setq parent
                      (e-subagent-live-find-by-session live board-id current)))
      (setq current
            (when-let* ((callbacks (plist-get parent :callbacks))
                        (getter (plist-get callbacks :record))
                        ((functionp getter))
                        (record (funcall getter)))
              (plist-get record :parent-session-id))
            remaining (1- remaining)))
    current))

(defun e-subagent--type-instance (type)
  "Return the spawnable harness instance for TYPE, or signal."
  (let ((instance (e-harness-instance-get type)))
    (unless (and instance (e-harness-instance-subagent-p instance))
      (signal 'e-subagent-unknown-type (list type)))
    instance))

(defvar e-subagent--configured-harnesses (make-hash-table :test 'eq :weakness 'key)
  "Harnesses whose declaring instance's minimal layers/config were applied.
Keyed weakly by harness so a torn-down harness is re-configured if recreated.")

(defun e-subagent--live-chat-binding (harness session-id)
  "Return SESSION-ID's process-local chat binding or signal clearly."
  (or (e-chat-service-binding harness session-id)
      (signal 'e-subagent-error
              (list "Parent Board binding is not ready" session-id))))

(defconst e-subagent-max-intervention-reason-length 240
  "Maximum width of the audit reason retained for one intervention.")

(defconst e-subagent--inherited-prompt-cache-options
  '(:prompt-cache-default :prompt-cache-retention)
  "Prompt-cache policy options inherited by child sessions when unspecified.")

(defun e-subagent-publication-target (parent-harness parent-session-id)
  "Return the explicit SQL target for PARENT-HARNESS's parent session."
  (let ((binding
         (e-subagent--live-chat-binding parent-harness parent-session-id)))
    (e-board-sqlite-publication-target-create
     (e-chat-service-binding-sqlite-service binding)
     (e-chat-service-binding-board-id binding)
     :author (format "producer:subagent:%s:%s"
                     (e-chat-service-binding-board-id binding)
                     parent-session-id)
     :tags '(subagent))))

(cl-defun e-subagent--publish-board-fact
    (target &key tags attributes content source-fact-key)
  "Publish one bounded fact through explicit SQL TARGET."
  (e-board-sqlite-publication-target-fact-start
   target content source-fact-key
   :tags (copy-tree tags t) :attributes (copy-tree attributes t)))

(defun e-subagent--publish-lifecycle (target record &optional include-result)
  "Publish RECORD's lifecycle state through execution-owned TARGET.
RECORD is a detached publication value, not the live owner record.  The live
owner never stores this durable status or history."
  (let ((attributes (list :participant-id (plist-get record :participant-id)
                          :status (plist-get record :status)
                          :type (plist-get record :type)
                          :parent-session-id (plist-get record :parent-session-id)
                          :session-id (plist-get record :session-id))))
    (when include-result
      (dolist (key '(:result-summary :result :outputs :error))
        (when (plist-member record key)
          (setq attributes
                (plist-put attributes key (copy-tree (plist-get record key)))))))
    (e-subagent--publish-board-fact
     target
     :tags (list 'change (plist-get record :status))
     :attributes attributes
     :content (format "Subagent %s is %s"
                      (plist-get record :participant-id)
                      (plist-get record :status))
     :source-fact-key
     ;; Durable idempotency follows the admitted child session/participant.
     (list 'subagent-lifecycle (plist-get record :participant-id)
           (plist-get record :status)))))

(defcustom e-subagent-child-layer-ids '(subagents-child)
  "Layer ids always added to a spawned child harness.
These are appended to a type's declared `:layers' so a child can always report
its result, the same way base layers ride along on every harness.  A type may
still exclude one explicitly by redefining its `:layers'; that is a supported
choice, not an error."
  :type '(repeat symbol)
  :group 'e)

(defun e-subagent--child-harness (instance)
  "Return INSTANCE's live child harness, applying its declared setup once.
The instance's `:layers' (plus `e-subagent-child-layer-ids') become the
harness's enabled layer set and its `:layer-config' entries seed runtime
capability config, but only on the first creation, so a later `configure-type'
the parent applies is never clobbered."
  (let ((harness (e-harness-instance-get-or-create
                  (e-harness-instance-id instance))))
    (unless (gethash harness e-subagent--configured-harnesses)
      (let ((layers (e-harness-instance-layers instance)))
        (when (or layers e-subagent-child-layer-ids)
          (e-harness-set-enabled-layer-ids
           harness
           (append layers
                   (seq-remove (lambda (id) (memq id layers))
                               e-subagent-child-layer-ids)))))
      (dolist (entry (append (e-harness-instance-layer-config instance) nil))
        (e-harness-set-capability-config
         harness (car entry) (copy-sequence (cdr entry))))
      (puthash harness t e-subagent--configured-harnesses))
    harness))

(defun e-subagent--child-metadata
    (instance parent-harness parent-session-id lineage-id label
              &optional assignment project-root)
  "Return durable child metadata for INSTANCE under a parent lineage.
Inherit the parent's project root so repository AGENTS.md files and
=.agents/skills= are available to the child from its first turn."
  (let ((role (e-harness-instance-kind instance))
        (project-root
         (or project-root
             (and parent-session-id
                  (e-harness-project-root parent-harness parent-session-id)))))
    (append
     (list :tmp-lineage-id lineage-id)
     (when parent-session-id (list :parent-session-id parent-session-id))
     (when project-root (list :project-root project-root))
     (when role (list :subagent-role (symbol-name role)))
     (when (and (stringp label) (not (string-empty-p label)))
       (list :subagent-label label))
     (when assignment
       (list :board-run-id (plist-get assignment :run-id)
             :board-task-key (plist-get assignment :task-key)
             :board-attempt (plist-get assignment :attempt))))))

(defun e-subagent--seed-child (child-harness child-session-id seed-messages)
  "Append SEED-MESSAGES to CHILD-SESSION-ID's own store in CHILD-HARNESS.
Each seed is a backend-neutral message plist; the parent chooses exactly what to
hand over, so nothing leaks that it did not name."
  (dolist (message (append seed-messages nil))
    (e-chat-service-append-seed-message
     child-harness child-session-id message)))

(defun e-subagent--inherit-prompt-cache-policy
    (parent-harness parent-session-id child-harness child-session-id)
  "Give a child session its parent's prompt-cache policy when unspecified.
The child derives its own cache key from its model, root, layers, and tools;
an explicit child harness or session policy always wins."
  (let ((parent-options
         (e-harness-display-options parent-harness parent-session-id))
        (child-defaults (e-harness-default-options child-harness))
        (child-options
         (copy-sequence
          (e-harness-session-options child-harness child-session-id)))
        changed)
    (dolist (key e-subagent--inherited-prompt-cache-options)
      (when (and (plist-member parent-options key)
                 (not (plist-member child-defaults key))
                 (not (plist-member child-options key)))
        (setq child-options
              (plist-put child-options key (plist-get parent-options key)))
        (setq changed t)))
    (when changed
      (e-harness-set-session-options
       child-harness child-session-id child-options))))

(defun e-subagent-direct-runner (child-harness child-session-id prompt
                                               seed-messages on-settle &optional on-progress)
  "Seed and start one non-blocking child turn, settling through ON-SETTLE.
Returns a handle plist carrying a `:cancel' function that aborts the child's
active turn.  ON-SETTLE is called as (STATUS &key summary outputs error)."
  (e-subagent--seed-child child-harness child-session-id seed-messages)
  (let ((settled nil)
        (last-assistant nil)
        subscription)
    (cl-labels
        ((finish
          (status &rest args)
          (unless settled
            (setq settled t)
            (when on-progress
              (funcall on-progress
                       (pcase status
                         ('done 'turn-finished)
                         ('failed 'turn-failed)
                         ('cancelled 'turn-cancelled))))
            (when subscription
              (e-chat-service-unsubscribe subscription))
            (apply on-settle status args))))
      (setq subscription
            (e-chat-service-subscribe
             child-harness child-session-id
             (lambda (event)
               (when-let* ((message (plist-get (plist-get event :payload) :message))
                           ((eq (plist-get message :role) 'assistant))
                           (content (plist-get message :content)))
                 (setq last-assistant content))
               (pcase (plist-get event :type)
                 ((or 'provider-request-started 'provider-request-finished
                      'tool-started 'tool-finished 'action-started
                      'action-finished 'action-failed 'turn-steered)
                  (when on-progress
                    (funcall on-progress (plist-get event :type))))
                 ('turn-finished
                  (finish 'done
                          :summary last-assistant))
                 ('turn-failed
                  (finish 'failed
                          :error (or (plist-get (plist-get event :payload)
                                                :error)
                                     "Subagent turn failed")))
                 ('turn-cancelled
                  (finish 'cancelled))))
             ))
      (condition-case err
          (let ((admission
                 (e-chat-service-submit-session
                  child-harness child-session-id prompt)))
            (e-work-on-settle
             admission
             (lambda (settled)
               (let* ((status (e-work-status settled))
                      (state (plist-get status :state)))
                 (pcase state
                   ('failed
                    (finish 'failed
                            :error (e-work-error-message
                                    (plist-get status :error))))
                   ('cancelled (finish 'cancelled)))))))
        (error
         (finish 'failed :error (e-work-error-message err))))
      (list :cancel
            (lambda ()
              (ignore-errors
                (e-chat-service-abort-session
                 child-harness child-session-id)))))))

(defun e-subagent--progress-summary (event)
  "Return a bounded human-readable progress summary for child EVENT."
  (pcase event
    ('turn-started "Started child turn")
    ('provider-request-started "Started provider request")
    ('provider-request-finished "Finished provider request")
    ('tool-started "Started tool")
    ('tool-finished "Finished tool")
    ('action-started "Started action")
    ((or 'action-finished 'action-failed) "Finished action")
    ('turn-steered "Steered child turn")
    ('turn-finished "Finished child turn")
    ('turn-failed "Child turn failed")
    ('turn-cancelled "Child turn cancelled")
    (_ (format "%s" event))))

(defun e-subagent--record-progress (live board-id participant-id work-handle event)
  "Publish bounded child EVENT progress through WORK-HANDLE and LIVE."
  (let ((snapshot
         (e-subagent-live-record-progress
          live board-id participant-id
          (list :participant-id participant-id
                :event event
                :summary (e-subagent--progress-summary event)
                :at (float-time)))))
    (e-work-progress work-handle snapshot)
    snapshot))

(defun e-subagent--work-spec ()
  "Return the cooperative work spec that wraps a spawned child turn.
The child turn is driven by the harness, not this runner, so the spec's runner
defers; `e-subagent-spawn' finishes/fails the handle from the settle callback.
The handle exists so a subagent is awaitable as an `e-work' handle."
  (e-work-spec-create
   :id "subagent"
   :description "Track a spawned subagent child turn."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'subagents
   :runner (lambda (_handle _arguments _context) :deferred)))

(defun e-subagent--pending-result (board-id participant-id work-handle)
  "Return the bounded pending result for PARTICIPANT-ID admission."
  (list :participant-id participant-id
        :await-ref (e-subagent-live-reference board-id participant-id)
        :status 'pending
        :session-id participant-id
        :work-id (e-work-handle-id work-handle)))

(defun e-subagent--settle-work-handle (handle status args)
  "Settle work HANDLE from a subagent STATUS and settle ARGS.
The handle mirrors the record's terminal state so `await' can observe it; its
finished result carries the compact summary and outputs."
  (when (e-work-handle-p handle)
    (pcase status
      ('done (e-work-finish handle
                            (list :summary (plist-get args :summary)
                                  :result (copy-tree (plist-get args :result))
                                  :outputs (plist-get args :outputs))))
      ('failed (e-work-fail handle
                            (list 'e-subagent-error
                                  (or (plist-get args :error)
                                      "Subagent turn failed"))))
      ('cancelled (e-work-cancel handle)))))

(defconst e-subagent--missing-admitted-report-error
  "Subagent completed without an accepted report"
  "Bounded terminal error for a gated child that reports only final prose.")

(defconst e-subagent--terminal-payload-byte-limit 4096
  "Maximum canonical bytes retained or published for one terminal payload.")

(defun e-subagent--bounded-terminal-args (args)
  "Return a bounded detached copy of terminal ARGS.

This bound applies to both ad-hoc lifecycle payloads and run-bound reports;
the live owner never becomes a bypass around the Board's bounded observation
contract."
  (let ((fields nil))
    (dolist (key '(:summary :result :outputs :error))
      (when (plist-member args key)
        (setq fields (plist-put fields key (plist-get args key)))))
    (if fields
        (e-runtime-store-codec-decode
         (e-runtime-store-codec-encode-bounded
          fields e-subagent--terminal-payload-byte-limit))
      nil)))

(defun e-subagent--report-assignment (record)
  "Return RECORD's detached child assignment for report admission."
  (list :run-id (plist-get record :run-id)
        :task-key (plist-get record :task-key)
        :attempt (plist-get record :attempt)
        :participant-id (plist-get record :participant-id)
        :session-id (plist-get record :session-id)
        :parent-session-id (plist-get record :parent-session-id)))

(defun e-subagent--effective-settlement (report-state status args)
  "Return effective (STATUS . ARGS) for one runner settlement.
A gated child cannot translate final prose into success.  An accepted report
also supplies the exact result observed by its work handle, independently of
later final prose.  REPORT-STATE is runner-owned closure state, never a
durable or public live-table projection."
  (let ((report (and report-state (plist-get report-state :report))))
    (cond
     ((and report-state
           (plist-get report-state :report-admission)
           (eq status 'done)
           (null report))
      (cons 'failed
            (list :error e-subagent--missing-admitted-report-error)))
     ((and report (eq status 'done))
      (cons status
            (list :summary (plist-get report :summary)
                  :result (copy-tree (plist-get report :result))
                  :outputs (copy-tree (plist-get report :outputs)))))
     (t (cons status args)))))

(defun e-subagent--settle-runner
    (live board-id participant-id record target report-state work-handle
         status args)
  "Settle runner STATUS and ARGS through the report-admission boundary."
  (pcase-let* ((`(,status . ,args)
                (e-subagent--effective-settlement
                 report-state status args)))
    (setq args (e-subagent--bounded-terminal-args args))
    (e-subagent--settle-work-handle work-handle status args)
    (apply #'e-subagent--settle
           live board-id participant-id record target report-state status args)))

(defun e-subagent--durable-assignment (record)
  "Return RECORD's persisted orchestration assignment, or nil."
  (when-let ((run-id (plist-get record :run-id)))
    (list :run-id run-id
          :task-key (plist-get record :task-key)
          :attempt (plist-get record :attempt))))

(defun e-subagent--publish-terminal-report (target record status)
  "Publish RECORD's terminal orchestration fact through TARGET.
SQLite source keys provide idempotency; no terminal publication state is
retained in the private live owner."
  (when-let ((assignment (e-subagent--durable-assignment record)))
    (e-board-orchestration-actions-publish-terminal
     target assignment status
     :summary (or (plist-get record :result-summary) "")
     :result (copy-tree (plist-get record :result))
     :outputs (or (plist-get record :outputs) [])
     :error (plist-get record :error)
     :author (list :session-id (plist-get record :session-id)))))

(defun e-subagent--settle
    (live board-id participant-id record target report-state status &rest args)
  "Publish one detached terminal settlement and remove live capabilities.
A child-reported structured result is authoritative for this settlement, while
the Board remains the durable authority.  Ad-hoc children publish their
bounded terminal result in one lifecycle fact; run-bound children publish the
existing orchestration terminal report.  A competing late callback is a
no-op because live state has already been removed."
  (pcase-let* ((`(,status . ,args)
                (e-subagent--effective-settlement
                 report-state status args)))
    (setq args (e-subagent--bounded-terminal-args args))
    (when (e-subagent-live-get live board-id participant-id)
      (let* ((finished-at (float-time))
             (terminal (copy-tree record))
             (assignment (e-subagent--durable-assignment terminal)))
        (setq terminal (plist-put terminal :status status))
        (setq terminal (plist-put terminal :finished-at finished-at))
        (setq terminal (plist-put terminal :last-turn-at finished-at))
        (when (plist-member args :summary)
          (setq terminal (plist-put terminal :result-summary
                                    (plist-get args :summary))))
        (when (plist-member args :outputs)
          (setq terminal (plist-put terminal :outputs
                                    (copy-tree (plist-get args :outputs)))))
        (when (plist-member args :result)
          (setq terminal (plist-put terminal :result
                                    (copy-tree (plist-get args :result)))))
        (when (plist-member args :error)
          (setq terminal (plist-put terminal :error (plist-get args :error))))
        (unwind-protect
            (progn
              ;; Run-bound terminal reports remain canonical.  The lifecycle
              ;; observation still records terminal state but does not copy
              ;; report payload into that path.
              (unless assignment
                (e-subagent--publish-lifecycle target terminal t))
              (when assignment
                (e-subagent--publish-lifecycle target terminal nil)
                (e-subagent--publish-terminal-report target terminal status))
              terminal)
          (e-subagent-live-remove live board-id participant-id))))))

(defun e-subagent--drive-turn
    (live board-id publication-target participant-id record
          parent-harness parent-session-id source-turn-id
          child-harness session-id prompt seed-messages runner report-state
          &optional work-handle on-running)
  "Start one child turn and wire its settle + work handle.
PUBLICATION-TARGET is the execution-owned durable Board destination.  RECORD
is detached runner context; LIVE retains only the capabilities needed by this
turn.  Reuse WORK-HANDLE when admission prepared it, otherwise mint a fresh
cooperative handle.  ON-RUNNING runs immediately before invoking RUNNER."
  (let* ((runner (or runner #'e-subagent-direct-runner))
         ;; Prepare before enrollment: board ownership must be established
         ;; before runner entry, just as it is for model-facing tool work.
         (work-handle
          (or work-handle
              (e-work-prepare
               (e-subagent--work-spec) nil
               :context (list :session-id parent-session-id
                              :turn-id source-turn-id
                              :work-kind 'subagent
                              :domain-ref (e-subagent-live-reference
                                           board-id participant-id))))))
    (when-let ((enroll (e-harness-work-enrollment-function parent-harness)))
      (funcall enroll work-handle nil))
    (e-work-start-prepared work-handle)
    (e-subagent-live-update live board-id participant-id
                            :work-handle work-handle)
    ;; Invoking the runner is the first point at which a provider turn may
    ;; start.  Admission has already committed; publish the truthful running
    ;; state immediately before that call.
    (let ((running (plist-put (copy-tree record) :status 'running)))
      (e-subagent--publish-lifecycle publication-target running))
    (when on-running
      (funcall on-running (copy-tree record)))
    (e-subagent--record-progress live board-id participant-id
                                 work-handle 'turn-started)
    (condition-case error
        (let ((handle
               (if (eq runner #'e-subagent-direct-runner)
                   (funcall runner
                            child-harness session-id prompt seed-messages
                            (lambda (status &rest args)
                              (e-subagent--settle-runner
                               live board-id participant-id record
                               publication-target report-state
                               work-handle status args))
                            (lambda (event)
                              (e-subagent--record-progress
                               live board-id participant-id work-handle event)))
                 (funcall runner
                          child-harness session-id prompt seed-messages
                          (lambda (status &rest args)
                            (when (e-subagent-live-get live board-id participant-id)
                              (e-subagent--record-progress
                               live board-id participant-id work-handle
                               (pcase status
                                 ('done 'turn-finished)
                                 ('failed 'turn-failed)
                                 ('cancelled 'turn-cancelled))))
                            (e-subagent--settle-runner
                             live board-id participant-id record
                             publication-target report-state
                             work-handle status args))))))
          (when (and (listp handle) (functionp (plist-get handle :cancel)))
            (e-subagent-live-update live board-id participant-id
                                    :cancel (plist-get handle :cancel)))
          handle)
      (error
       (e-subagent--settle-work-handle work-handle 'failed
                                       (list :error (e-work-error-message error)))
       (e-subagent--settle live board-id participant-id record
                           publication-target report-state 'failed
                           :error (e-work-error-message error))
       nil))))

(cl-defun e-subagent-spawn
    (live parent-harness parent-session-id
          &key source-turn-id type prompt seed-messages label schedule runner
          run-id task-key attempt project-root report-admission
          on-running on-failure)
  "Spawn a subagent and return its bounded admission result.
LIVE owns only private process-local execution capabilities.  The child
participant identity is its admitted session id, and is the only identity
returned to callers.  Before durable admission settles, the result contains
the admission work reference and the durable participant/session identity."
  (unless (stringp source-turn-id)
    (signal 'wrong-type-argument (list 'stringp :source-turn-id)))
  (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
    (signal 'wrong-type-argument (list 'stringp :prompt)))
  (let* ((type (e-subagent--normalize-type type))
         (instance (e-subagent--type-instance type))
         (child-harness (e-subagent--child-harness instance))
         (parent-binding
          (e-subagent--live-chat-binding parent-harness parent-session-id))
         (board-id (e-chat-service-binding-board-id parent-binding))
         (lineage-id (e-subagent--lineage-id live board-id parent-session-id))
         (assignment (and run-id
                          (list :run-id run-id :task-key task-key :attempt attempt)))
         (_ (when (or run-id task-key attempt)
              (unless (and (stringp run-id) (stringp task-key)
                           (integerp attempt) (>= attempt 0))
                (signal 'wrong-type-argument
                        (list 'e-board-orchestration-assignment assignment)))))
         (metadata (e-subagent--child-metadata
                    instance parent-harness parent-session-id lineage-id label
                    assignment project-root))
         (schedule (or schedule 'direct))
         (producer-target
          (e-subagent-publication-target parent-harness parent-session-id))
         (admission-target parent-binding)
         (child-session-id (e-session-generate-id))
         (participant-id child-session-id)
         (work-handle
          (e-work-prepare
           (e-subagent--work-spec) nil
           :context (list :session-id parent-session-id
                          :turn-id source-turn-id
                          :work-kind 'subagent
                          :domain-ref (e-subagent-live-reference
                                       board-id participant-id))))
         (record (list :board-id board-id
                       :participant-id participant-id
                       :type type
                       :role (e-harness-instance-kind instance)
                       :session-id participant-id
                       :parent-session-id parent-session-id
                       :label label
                       :schedule schedule
                       :child-harness child-harness
                       :run-id run-id
                       :task-key task-key
                       :attempt attempt
                       :report-admission report-admission
                       :status 'queued))
         (report-state (list :report-admission report-admission :report nil))
         (callbacks
          (list :record
                (lambda () (copy-tree record))
                :report
                (lambda (accepted)
                  (setq report-state
                        (plist-put report-state :report (copy-tree accepted))))
                :reported
                (lambda () (plist-get report-state :report))))
         (pending
          (e-subagent--pending-result board-id participant-id work-handle))
         admission-work admitted-result)
    (e-subagent-live-reserve-admission
     live board-id participant-id
     :work-handle work-handle :assignment assignment :callbacks callbacks
     :report-admission report-admission
     :parent-session-id parent-session-id :lineage-id lineage-id)
    (condition-case error
        (setq admission-work
              (e-chat-service-create-participant-start
               admission-target child-harness
               :id participant-id :participant-id participant-id
               :metadata metadata
               :pickup-selector '(:tags (subagent))
               :observer-selector :self :default-tags '(subagent)
               :default-to :self))
      (error
       (e-subagent-live-forget-admission live board-id participant-id)
       (e-work-fail work-handle error)
       (when on-failure
         (funcall on-failure error pending))
       (setq pending (plist-put pending :status 'failed))
       (setq pending (plist-put pending :error error))))
    (when admission-work
      (e-work-on-settle
       admission-work
       (lambda (settled-admission)
         (let ((status (e-work-status settled-admission)))
           (pcase (plist-get status :state)
             ('finished
              (if (null (e-subagent-live-pending-admission
                         live board-id participant-id))
                  ;; A caller may cancel while admission is still pending.
                  ;; The request-owned work handle has already been retired;
                  ;; do not resurrect a live child when SQL later settles.
                  nil
                (condition-case error
                    (progn
                    (e-subagent-live-install
                     live board-id participant-id
                     :harness child-harness :work-handle work-handle
                     :callbacks callbacks :report-admission report-admission)
                    (e-subagent--publish-lifecycle producer-target record)
                    (e-subagent--inherit-prompt-cache-policy
                     parent-harness parent-session-id
                     child-harness participant-id)
                    (e-subagent--drive-turn
                     live board-id producer-target participant-id record
                     parent-harness parent-session-id source-turn-id
                     child-harness participant-id prompt seed-messages runner
                     report-state work-handle on-running)
                      (setq admitted-result (copy-tree record)))
                  (error
                   (if (e-subagent-live-get live board-id participant-id)
                       (progn
                         (e-subagent--settle-work-handle
                          work-handle 'failed
                          (list :error (e-work-error-message error)))
                         (e-subagent--settle
                          live board-id participant-id record producer-target
                          report-state 'failed
                          :error (e-work-error-message error)))
                     (e-subagent-live-forget-admission
                      live board-id participant-id)
                     (e-work-fail work-handle error)
                     (when on-failure
                       (funcall on-failure error pending)))))))
             ('failed
              (e-subagent-live-forget-admission live board-id participant-id)
              (let ((admission-error (plist-get status :error)))
                (e-work-fail work-handle admission-error)
                (when on-failure
                  (funcall on-failure admission-error pending))))
             ('cancelled
              (e-subagent-live-forget-admission live board-id participant-id)
              (e-work-cancel work-handle)))))))
    (or admitted-result
        (let ((work-status (e-work-status work-handle)))
          (if (eq (plist-get work-status :state) 'failed)
              (append pending
                      (list :status 'failed
                            :error (plist-get work-status :error)))
            pending)))))

(defun e-subagent--normalize-type (value)
  "Return VALUE as a spawnable type keyword.
Actions arrive as JSON, so a type id reaches here as a string; the harness
catalog keys instances by keyword."
  (cond
   ((keywordp value) value)
   ((and (symbolp value) value) (intern (concat ":" (symbol-name value))))
   ((stringp value) (intern (concat ":" (string-remove-prefix ":" value))))
   (t (signal 'wrong-type-argument (list 'keywordp :type)))))

(defun e-subagent--layer-symbol (value)
  "Return VALUE as a layer id symbol.
Layer ids arrive from the action surface as strings."
  (cond
   ((and (symbolp value) value) value)
   ((stringp value) (intern value))
   (t (signal 'wrong-type-argument (list 'symbolp value)))))

(defun e-subagent--capability-symbol (value)
  "Return VALUE as a capability id symbol."
  (cond
   ((and (symbolp value) value) value)
   ((stringp value) (intern value))
   (t (signal 'wrong-type-argument (list 'symbolp value)))))

(defun e-subagent--capability-config-plist (value)
  "Return VALUE normalized to a capability config plist.
Action arguments arrive with string keys; intern them to keywords."
  (let (plist)
    (while value
      (let ((key (pop value))
            (val (and value (pop value))))
        (setq plist
              (plist-put plist
                         (cond
                          ((keywordp key) key)
                          ((symbolp key)
                           (intern (concat ":" (symbol-name key))))
                          ((stringp key)
                           (intern (concat ":" (string-remove-prefix ":" key))))
                          (t (signal 'wrong-type-argument (list key))))
                         val))))
    plist))

(defun e-subagent-configure-type
    (type &rest args)
  "Configure the shared harness for spawnable TYPE and return its state.
ARGS is a plist of `:enable-layers', `:disable-layers', and `:layer-config'.
ENABLE-LAYERS and DISABLE-LAYERS toggle layers on the type's shared harness, so
the parent turns individual capabilities on or off for every child of that type.
LAYER-CONFIG is an alist mapping a capability id to an option plist, applied as
that capability's runtime config; this is the generic way to pass or overwrite
layer configuration (e.g. `agents-std-context' `:skills-include').  Because
children of a type share one harness, this configures the type, not a single
child."
  (let* ((type (e-subagent--normalize-type type))
         (instance (e-subagent--type-instance type))
         (harness (e-subagent--child-harness instance))
         (enable-layers (plist-get args :enable-layers))
         (disable-layers (plist-get args :disable-layers))
         (layer-config (plist-get args :layer-config)))
    (dolist (layer (append disable-layers nil))
      (e-harness-disable-layer-id harness (e-subagent--layer-symbol layer)))
    (dolist (layer (append enable-layers nil))
      (e-harness-enable-layer-id harness (e-subagent--layer-symbol layer)))
    (dolist (entry (append layer-config nil))
      (e-harness-set-capability-config
       harness
       (e-subagent--capability-symbol (car entry))
       (e-subagent--capability-config-plist (cdr entry))))
    (list :type type
          :enabled-layers (e-harness-enabled-layer-ids harness)
          :capability-config
          (mapcar (lambda (entry)
                    (let ((id (e-subagent--capability-symbol (car entry))))
                      (cons id (e-harness-capability-config harness id))))
                  (append layer-config nil)))))

(defun e-subagent-report (live board-id session-id outputs summary &optional result)
  "Record a child-reported structured result for SESSION-ID in LIVE.
OUTPUTS is a structured artifact list; SUMMARY is a short result string, and
RESULT is optional bounded application-owned structured data.  The
report is accepted only while the live execution exists.  Durable terminal
publication happens exactly once during settlement; this function retains only
the runner-owned closure callback and never writes a terminal result inventory.
Return a detached acknowledgement, or nil when SESSION-ID is not live."
  (when-let ((entry (e-subagent-live-find-by-session live board-id session-id)))
    (let* ((proposed (append (list :summary summary :outputs outputs)
                             (when result (list :result result))))
           (callbacks (plist-get entry :callbacks))
           (reported (plist-get callbacks :reported))
           (setter (plist-get callbacks :report))
           (record-getter (plist-get callbacks :record))
           (record (and (functionp record-getter) (funcall record-getter)))
           (admission (e-subagent-live-report-admission
                       live board-id session-id))
           (assignment (e-subagent--durable-assignment record)))
      (if (and reported (funcall reported))
          (list :participant-id session-id :session-id session-id :reported t)
        (let ((accepted
               (if (and admission (functionp admission))
                   (funcall admission
                            (append (copy-tree assignment)
                                    (list :participant-id session-id
                                          :session-id session-id
                                          :parent-session-id
                                          (plist-get record :parent-session-id)))
                            proposed)
                 proposed)))
          (unless (and (listp accepted)
                       (plist-member accepted :summary)
                       (plist-member accepted :outputs))
            (signal 'e-subagent-live-error
                    (list "Report admission returned an invalid report")))
          (unless (functionp setter)
            (signal 'e-subagent-live-error
                    (list "Live report callback is unavailable")))
          (funcall setter accepted)
          (list :participant-id session-id :session-id session-id :reported t))))))

(defun e-subagent--live-record (live board-id participant-id)
  "Return detached runner context from a private record callback."
  (when-let* ((entry (or (e-subagent-live-get live board-id participant-id)
                         (e-subagent-live-pending-admission
                          live board-id participant-id)))
              (callbacks (plist-get entry :callbacks))
              (getter (plist-get callbacks :record)))
    (and (functionp getter) (funcall getter))))

(defun e-subagent--record-intervention
    (publication-target participant-id record action reason)
  "Publish one ACTION intervention through PUBLICATION-TARGET."
  (when (and reason (not (stringp reason)))
    (signal 'wrong-type-argument (list 'stringp reason)))
  (let* ((bounded-reason
          (and reason (truncate-string-to-width
                       reason e-subagent-max-intervention-reason-length nil nil "...")))
         (at (float-time)))
    (e-subagent--publish-board-fact
     publication-target
     :tags (list 'intervention action)
     :attributes (list :participant-id participant-id
                       :action action
                       :reason bounded-reason
                       :parent-session-id (plist-get record :parent-session-id)
                       :session-id participant-id)
     :content (format "Subagent %s %s%s"
                      participant-id action
                      (if bounded-reason (format ": %s" bounded-reason) ""))
     :source-fact-key
     (list 'subagent-intervention participant-id action at))
    record))

(defun e-subagent--cancel-and-retire
    (live board-id publication-target participant-id action reason)
  "Cancel PARTICIPANT-ID and retire all live coordination.
PUBLICATION-TARGET belongs to the caller's current action or presentation
context.  Cancellation and local retirement run even when audit publication
fails."
  (let* ((entry (e-subagent-live-get live board-id participant-id))
         (pending (and (null entry)
                       (e-subagent-live-pending-admission
                        live board-id participant-id)))
         (record (e-subagent--live-record live board-id participant-id))
         (cancel (and entry (plist-get entry :cancel)))
         (work-handle (and (or entry pending)
                           (plist-get (or entry pending) :work-handle)))
         snapshot)
    (unless record
      (user-error "Subagent %s is unavailable in this process" participant-id))
    (unwind-protect
        (setq snapshot
              (e-subagent--record-intervention
               publication-target participant-id record action reason))
      (when (functionp cancel)
        (funcall cancel))
      (when (e-subagent-live-get live board-id participant-id)
        (e-subagent--settle-work-handle work-handle 'cancelled nil)
        (setq snapshot
              (e-subagent--settle
               live board-id participant-id record publication-target nil
               'cancelled)))
      (when pending
        (e-work-cancel work-handle)
        (e-subagent-live-forget-admission live board-id participant-id)
        (setq snapshot (plist-put (copy-tree record) :status 'cancelled)))
      (unless (e-subagent-live-get live board-id participant-id)
        (e-subagent-live-remove live board-id participant-id)))
    snapshot))

(defun e-subagent-interrupt
    (live board-id publication-target participant-id &optional reason)
  "Abort PARTICIPANT-ID's active child turn and retire live coordination."
  (e-subagent--cancel-and-retire
   live board-id publication-target participant-id 'interrupt reason))

(defun e-subagent-shutdown
    (live board-id publication-target participant-id &optional reason)
  "Interrupt PARTICIPANT-ID deliberately and retire live coordination."
  (e-subagent--cancel-and-retire
   live board-id publication-target participant-id 'shutdown reason))

(defun e-subagent-steer
    (live board-id publication-target participant-id prompt &optional reason)
  "Steer PARTICIPANT-ID's running child turn with PROMPT."
  (let* ((entry (e-subagent-live-get live board-id participant-id))
         (record (e-subagent--live-record live board-id participant-id))
         (harness (and entry (plist-get entry :harness))))
    (unless (and record harness)
      (user-error "Subagent %s has no live child harness" participant-id))
    (e-chat-service-steer-session harness participant-id prompt)
    (e-subagent--record-intervention
     publication-target participant-id record 'steer reason)))

(defun e-subagent-send (live board-id participant-id prompt)
  "Queue a follow-up PROMPT to PARTICIPANT-ID's child session."
  (let* ((entry (e-subagent-live-get live board-id participant-id))
         (harness (and entry (plist-get entry :harness))))
    (unless harness
      (user-error "Subagent %s is not executing" participant-id))
    (e-chat-service-queue-session harness participant-id prompt)
    (e-subagent--live-record live board-id participant-id)))

(defun e-subagent-raw-read (live board-id participant-id &optional limit)
  "Return a bounded raw transcript excerpt for PARTICIPANT-ID."
  (let* ((record (e-subagent--live-record live board-id participant-id))
         (harness (e-subagent-live-harness live board-id participant-id))
         (session-id participant-id)
         (limit (or limit 20))
         (messages
          (and harness
               (plist-get
                (e-harness-executing-session-state harness session-id)
                :messages)))
         (tail (last messages limit)))
    (unless record
      (user-error "Subagent %s is unavailable in this process" participant-id))
    (list :participant-id participant-id
          :session-id session-id
          :session-uri (format "session://e/sessions/%s/messages" session-id)
          :messages (mapcar (lambda (message)
                              (list :role (plist-get message :role)
                                    :content (plist-get message :content)))
                            tail))))

(provide 'e-subagent-runner)

;;; e-subagent-runner.el ends here

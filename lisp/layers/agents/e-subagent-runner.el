;;; e-subagent-runner.el --- Subagent spawn and run coordination for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Spawn coordination for subagents.  The coordinator resolves a spawnable type
;; to its harness instance, creates a fresh child session carrying durable
;; lineage metadata, seeds the child's own store (default prompt-only, optional
;; explicit messages), records the child in a registry, and drives it through a
;; pluggable runner seam.  The default runner starts one non-blocking child turn
;; on the child harness and settles the record from turn events.  The child's
;; last assistant message becomes the compact result unless the child reports a
;; structured result, which is authoritative.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-chat-service)
(require 'e-session)
(require 'e-subagent-registry)
(require 'e-work)
(require 'e-board-orchestration-actions)

(define-error 'e-subagent-error "e subagent error")
(define-error 'e-subagent-unknown-type
  "No spawnable subagent type is registered for id" 'e-subagent-error)

(defun e-subagent--lineage-id (registry parent-session-id)
  "Return the live tmp-lineage root for PARENT-SESSION-ID in REGISTRY.

Lineage is execution coordination, not a reason to synchronously reconstruct
the parent's durable session.  Follow only the bounded live subagent registry;
an ordinary root session seeds its own lineage id."
  (let ((current parent-session-id)
        (remaining 64)
        parent)
    (while (and (> remaining 0)
                (setq parent
                      (e-subagent-registry-find-by-session registry current)))
      (setq current (plist-get parent :parent-session-id)
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

(defun e-subagent--publish-lifecycle (target record)
  "Publish RECORD's lifecycle state through execution-owned TARGET.
TARGET is retained only by the active runner closure.  The registry never owns
publication capability or durable history."
  (e-subagent--publish-board-fact
   target
   :tags (list 'change (plist-get record :status))
   :attributes (list :subagent-id (plist-get record :subagent-id)
                     :status (plist-get record :status)
                     :type (plist-get record :type)
                     :parent-session-id (plist-get record :parent-session-id)
                     :session-id (plist-get record :session-id))
   :content (format "Subagent %s is %s"
                    (plist-get record :subagent-id)
                    (plist-get record :status))
   :source-fact-key
   ;; `subagent-id' is a process-local display identity whose sequence starts
   ;; over after Emacs restarts.  Durable idempotency follows the globally
   ;; unique child session instead.
   (list 'subagent-lifecycle (plist-get record :session-id)
         (plist-get record :status))))

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
    (instance parent-harness parent-session-id lineage-id label &optional assignment)
  "Return durable child metadata for INSTANCE under a parent lineage.
Inherit the parent's project root so repository AGENTS.md files and
=.agents/skills= are available to the child from its first turn."
  (let ((role (e-harness-instance-kind instance))
        (project-root
         (and parent-session-id
              (e-harness-project-root parent-harness parent-session-id))))
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

(defun e-subagent--record-progress (registry subagent-id work-handle event)
  "Publish bounded child EVENT progress through WORK-HANDLE and REGISTRY."
  (let ((snapshot
         (e-subagent-registry-record-progress
          registry subagent-id event (e-subagent--progress-summary event))))
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

(defun e-subagent--pending-result (subagent-id session-id work-handle)
  "Return the bounded public pending result for SUBAGENT-ID admission."
  (list :subagent-id subagent-id
        :await-ref (format "subagent:%s" subagent-id)
        :status 'pending
        :session-id session-id
        :work-id (e-work-handle-id work-handle)))

(defun e-subagent--settle-work-handle (handle status args)
  "Settle work HANDLE from a subagent STATUS and settle ARGS.
The handle mirrors the record's terminal state so `await' can observe it; its
finished result carries the compact summary and outputs."
  (when (e-work-handle-p handle)
    (pcase status
      ('done (e-work-finish handle
                            (list :summary (plist-get args :summary)
                                  :outputs (plist-get args :outputs))))
      ('failed (e-work-fail handle
                            (list 'e-subagent-error
                                  (or (plist-get args :error)
                                      "Subagent turn failed"))))
      ('cancelled (e-work-cancel handle)))))

(defun e-subagent--durable-assignment (record)
  "Return RECORD's persisted orchestration assignment, or nil."
  (when-let ((run-id (plist-get record :run-id)))
    (list :run-id run-id
          :task-key (plist-get record :task-key)
          :attempt (plist-get record :attempt))))

(defun e-subagent--publish-terminal-report (target record status)
  "Publish RECORD's terminal orchestration fact through TARGET.
SQLite source keys provide idempotency; no terminal publication state is
retained in the live registry."
  (when-let ((assignment (e-subagent--durable-assignment record)))
    (e-board-orchestration-actions-publish-terminal
     target assignment status
     :summary (or (plist-get record :result-summary) "")
     :outputs (or (plist-get record :outputs) [])
     :error (plist-get record :error)
     :author (list :session-id (plist-get record :session-id)))))

(defun e-subagent--settle (registry target subagent-id status &rest args)
  "Settle SUBAGENT-ID in REGISTRY to STATUS with ARGS.
A child-reported structured result is authoritative: once reported, later
chatter never overwrites the recorded summary or outputs.  Publish the durable
terminal facts, then remove all live execution coordination.  A competing late
terminal callback is a no-op."
  (when-let* ((record (gethash subagent-id
                               (e-subagent-registry-records registry))))
    (when (memq (plist-get record :status) '(queued running blocked))
      (let* ((reported (plist-get record :reported))
           (finished-at (float-time))
           (fields (list :status status
                         :finished-at finished-at
                         :last-turn-at finished-at)))
      (unless reported
        (when (plist-member args :summary)
          (setq fields (plist-put fields :result-summary
                                  (plist-get args :summary))))
        (when (plist-member args :outputs)
          (setq fields (plist-put fields :outputs (plist-get args :outputs)))))
      (when (plist-member args :error)
        (setq fields (plist-put fields :error (plist-get args :error))))
        (let ((terminal
               (apply #'e-subagent-registry-update
                      registry subagent-id fields)))
          (unwind-protect
              (progn
                (e-subagent--publish-lifecycle target terminal)
                (e-subagent--publish-terminal-report target terminal status)
                terminal)
            (e-subagent-registry-remove registry subagent-id)))))))

(defun e-subagent--drive-turn
    (registry publication-target subagent-id
              parent-harness parent-session-id source-turn-id
              child-harness session-id prompt seed-messages runner
              &optional work-handle on-running)
  "Start one child turn for SUBAGENT-ID and wire its settle + work handle.
PUBLICATION-TARGET is the execution-owned durable Board destination.
Reuse WORK-HANDLE when admission prepared it, otherwise mint a fresh
cooperative handle.  Mirror the record's terminal state onto the handle and
settle the record from RUNNER's callback.  Store the handle and any `:cancel'
function on the record.  ON-RUNNING runs after admission and registration,
immediately before invoking RUNNER.  Return RUNNER's handle plist.  Shared by
`e-subagent-spawn'."
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
                              :domain-ref (format "subagent:%s" subagent-id))))))
    (when-let ((enroll (e-harness-work-enrollment-function parent-harness)))
      (funcall enroll work-handle nil))
    (e-work-start-prepared work-handle)
    (e-subagent-registry-update registry subagent-id :work-handle work-handle)
    ;; Invoking the runner is the first point at which a provider turn may
    ;; start.  Admission has already committed and the record is registered;
    ;; publish the truthful running state immediately before that call.
    (let ((running
           (e-subagent-registry-update registry subagent-id :status 'running)))
      (e-subagent--publish-lifecycle publication-target running))
    (when on-running
      (funcall on-running (e-subagent-registry-get registry subagent-id)))
    (e-subagent--record-progress registry subagent-id work-handle 'turn-started)
    (condition-case error
        (let ((handle
               (if (eq runner #'e-subagent-direct-runner)
                   (funcall runner
                            child-harness session-id prompt seed-messages
                            (lambda (status &rest args)
                              (e-subagent--settle-work-handle work-handle status args)
                              (apply #'e-subagent--settle
                                     registry publication-target
                                     subagent-id status args))
                            (lambda (event)
                              (e-subagent--record-progress
                               registry subagent-id work-handle event)))
                 (funcall runner
                          child-harness session-id prompt seed-messages
                          (lambda (status &rest args)
                            (e-subagent--record-progress
                             registry subagent-id work-handle
                             (pcase status
                               ('done 'turn-finished)
                               ('failed 'turn-failed)
                               ('cancelled 'turn-cancelled)))
                            (e-subagent--settle-work-handle work-handle status args)
                            (apply #'e-subagent--settle
                                   registry publication-target
                                   subagent-id status args))))))
          (when (and (listp handle) (functionp (plist-get handle :cancel)))
            (e-subagent-registry-update registry subagent-id
                                        :cancel (plist-get handle :cancel)))
          handle)
      (error
       (e-subagent--settle-work-handle work-handle 'failed
                                       (list :error (e-work-error-message error)))
       (e-subagent--settle registry publication-target subagent-id 'failed
                           :error (e-work-error-message error))
       nil))))

(cl-defun e-subagent-spawn
    (registry parent-harness parent-session-id
              &key source-turn-id type prompt seed-messages label schedule runner
              run-id task-key attempt on-running on-failure)
  "Spawn a subagent of TYPE under a parent lineage and return its record.
REGISTRY tracks the child.  PARENT-HARNESS and PARENT-SESSION-ID identify the
spawning session, whose lineage the child inherits so they share one tmp root.
SOURCE-TURN-ID is the parent turn that initiated the child.  PROMPT is the
child's task.  RUN-ID, TASK-KEY, and ATTEMPT optionally bind the child to one
durable orchestration assignment.  SEED-MESSAGES are optional explicit context
messages.  LABEL is a human-scannable stub.  SCHEDULE is `direct' (default) or
`queue'.  RUNNER overrides the default direct-turn runner for tests; it is
called as (CHILD-HARNESS CHILD-SESSION-ID PROMPT SEED-MESSAGES ON-SETTLE) and
returns a handle plist carrying `:cancel'.  Before durable admission settles,
return a bounded pending result and retain one bounded admission-coordination
record in REGISTRY.  ON-RUNNING observes the first post-admission runner
boundary; ON-FAILURE observes a failure before a registered child can publish
its own terminal assignment without blocking spawn."
  (unless (stringp source-turn-id)
    (signal 'wrong-type-argument (list 'stringp :source-turn-id)))
  (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
    (signal 'wrong-type-argument (list 'stringp :prompt)))
  (let* ((type (e-subagent--normalize-type type))
         (instance (e-subagent--type-instance type))
         (child-harness (e-subagent--child-harness instance))
         (parent-binding
          (e-subagent--live-chat-binding parent-harness parent-session-id))
         (lineage-id (e-subagent--lineage-id registry parent-session-id))
         (assignment (and run-id (list :run-id run-id :task-key task-key :attempt attempt)))
         (_ (when (or run-id task-key attempt)
              (unless (and (stringp run-id) (stringp task-key) (integerp attempt) (>= attempt 0))
                (signal 'wrong-type-argument (list 'e-board-orchestration-assignment assignment)))))
         (metadata (e-subagent--child-metadata
                    instance parent-harness parent-session-id lineage-id label assignment))
         (schedule (or schedule 'direct))
         (producer-target
          (e-subagent-publication-target parent-harness parent-session-id))
         (admission-target
          parent-binding)
         (child-session-id (e-session-generate-id))
         (work-handle
          (e-work-prepare
           (e-subagent--work-spec) nil
           :context (list :session-id parent-session-id
                          :turn-id source-turn-id
                          :work-kind 'subagent
                          :domain-ref child-session-id)))
         (subagent-id
          (e-subagent-registry-reserve-admission
           registry :work-handle work-handle
           :type type :role (e-harness-instance-kind instance)
           :session-id child-session-id
           :parent-session-id parent-session-id
           :label label :schedule schedule
           :run-id run-id :task-key task-key :attempt attempt))
         (pending
          (e-subagent--pending-result
           subagent-id child-session-id work-handle))
         admission-work admitted-result)
    (condition-case error
        (setq admission-work
            (e-chat-service-create-participant-start
               admission-target child-harness :id child-session-id :metadata metadata
               :pickup-selector '(:tags (subagent))
               :observer-selector :self :default-tags '(subagent)
               :default-to :self))
      (error
       (e-subagent-registry-forget-admission registry subagent-id)
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
              (condition-case error
                  (progn
                    (setq admitted-result
                          (e-subagent-registry-register
                           registry
                           :subagent-id subagent-id :work-handle work-handle
                           :type type :role (e-harness-instance-kind instance)
                           :session-id child-session-id
                           :parent-session-id parent-session-id
                           :label label :schedule schedule
                           :child-harness child-harness
                           :run-id run-id :task-key task-key :attempt attempt))
                    (e-subagent--publish-lifecycle producer-target admitted-result)
                    (e-subagent--inherit-prompt-cache-policy
                     parent-harness parent-session-id
                     child-harness child-session-id)
                    (e-subagent--drive-turn
                     registry producer-target subagent-id
                     parent-harness parent-session-id
                     source-turn-id child-harness child-session-id prompt
                     seed-messages runner work-handle on-running)
                    (when (gethash subagent-id
                                   (e-subagent-registry-records registry))
                      (setq admitted-result
                            (e-subagent-registry-get registry subagent-id))))
                (error
                 (if (gethash subagent-id
                              (e-subagent-registry-records registry))
                     (progn
                       (e-subagent--settle-work-handle
                        work-handle 'failed
                        (list :error (e-work-error-message error)))
                       (e-subagent--settle
                        registry producer-target subagent-id 'failed
                        :error (e-work-error-message error)))
                   (e-subagent-registry-forget-admission
                    registry subagent-id)
                   (e-work-fail work-handle error)
                   (when on-failure
                     (funcall on-failure error pending))))))
             ('failed
              (e-subagent-registry-forget-admission registry subagent-id)
              (let ((admission-error (plist-get status :error)))
                (e-work-fail work-handle admission-error)
                (when on-failure
                  (funcall on-failure admission-error pending))))
             ('cancelled
              (e-subagent-registry-forget-admission registry subagent-id)
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

(defun e-subagent-report (registry session-id outputs summary)
  "Record a child-reported structured result for SESSION-ID in REGISTRY.
OUTPUTS is a structured artifact list; SUMMARY is a short result string.  The
report is authoritative: it marks the record reported so a later final message
cannot overwrite it.  Return the normalized record, or nil when SESSION-ID is
not a tracked child."
  (when-let* ((record (e-subagent-registry-find-by-session registry session-id))
              (subagent-id (plist-get record :subagent-id)))
    (e-subagent-registry-update
     registry subagent-id
     :reported t
     :outputs outputs
     :result-summary summary)))

(defun e-subagent--record-intervention
    (registry publication-target subagent-id action reason)
  "Record and publish one ACTION intervention through PUBLICATION-TARGET."
  (when (and reason (not (stringp reason)))
    (signal 'wrong-type-argument (list 'stringp reason)))
  (let* ((record (e-subagent-registry-get registry subagent-id))
         (bounded-reason
          (and reason (truncate-string-to-width
                       reason e-subagent-max-intervention-reason-length nil nil "...")))
         (intervention (list :action action :reason bounded-reason :at (float-time))))
    (e-subagent-registry-update registry subagent-id :last-intervention intervention)
    (e-subagent--publish-board-fact
     publication-target
     :tags (list 'intervention action)
     :attributes (list :subagent-id subagent-id
                       :action action
                       :reason bounded-reason
                       :parent-session-id (plist-get record :parent-session-id)
                       :session-id (plist-get record :session-id))
     :content (format "Subagent %s %s%s"
                      subagent-id action
                      (if bounded-reason (format ": %s" bounded-reason) ""))
     :source-fact-key
     (list 'subagent-intervention (plist-get record :session-id) action
           (plist-get intervention :at)))
    (e-subagent-registry-get registry subagent-id)))

(defun e-subagent--cancel-and-retire
    (registry publication-target subagent-id action reason)
  "Cancel SUBAGENT-ID, publish ACTION, and retire all live coordination.
PUBLICATION-TARGET belongs to the caller's current action or presentation
context; it is never rediscovered through registry-retained parent authority.
Cancellation and local retirement run even when audit publication fails."
  (let ((cancel (e-subagent-registry-cancel-function registry subagent-id))
        snapshot)
    (unwind-protect
        (setq snapshot
              (e-subagent--record-intervention
               registry publication-target subagent-id action reason))
      (when cancel
        (funcall cancel))
      (if-let* ((record
                 (gethash subagent-id
                          (e-subagent-registry-records registry))))
          (progn
            (e-subagent--settle-work-handle
             (plist-get record :work-handle) 'cancelled nil)
            (setq snapshot
                  (e-subagent--settle
                   registry publication-target subagent-id 'cancelled)))
        (when snapshot
          (setq snapshot (plist-put snapshot :status 'cancelled)))))
    snapshot))

(defun e-subagent-interrupt
    (registry publication-target subagent-id &optional reason)
  "Abort SUBAGENT-ID's active child turn and retire its live coordination.
PUBLICATION-TARGET is the caller-owned SQL audit destination.  REASON is
bounded audit data and never reaches the child."
  (e-subagent--cancel-and-retire
   registry publication-target subagent-id 'interrupt reason))

(defun e-subagent-shutdown
    (registry publication-target subagent-id &optional reason)
  "Interrupt SUBAGENT-ID deliberately and retire its live coordination.
PUBLICATION-TARGET is the caller-owned SQL audit destination.  REASON is
bounded audit data and never reaches the child."
  (e-subagent--cancel-and-retire
   registry publication-target subagent-id 'shutdown reason))

(defun e-subagent-steer
    (registry publication-target subagent-id prompt &optional reason)
  "Steer SUBAGENT-ID's running child turn with PROMPT.
Steers the active turn in place through the child harness so the parent can
communicate mid-flight.  PUBLICATION-TARGET is the caller-owned SQL audit
destination.  Return the normalized record."
  (let ((harness (e-subagent-registry-child-harness registry subagent-id))
        (session-id (plist-get (e-subagent-registry-get registry subagent-id)
                               :session-id)))
    (unless harness
      (user-error "Subagent %s has no live child harness" subagent-id))
    (e-chat-service-steer-session harness session-id prompt)
    (e-subagent--record-intervention
     registry publication-target subagent-id 'steer reason)))

(defun e-subagent-send (registry subagent-id prompt)
  "Queue a follow-up PROMPT to SUBAGENT-ID's child session.
Unlike `e-subagent-steer', this submits a follow-up turn rather than steering
the active one, so it needs a live running turn to queue behind.  Terminal
children have already left the live registry.  Return the normalized record."
  (let ((status (e-subagent-registry-status registry subagent-id))
        (harness (e-subagent-registry-child-harness registry subagent-id))
        (session-id (plist-get (e-subagent-registry-get registry subagent-id)
                               :session-id)))
    (unless (memq status '(queued running blocked))
      (user-error "Subagent %s is not executing" subagent-id))
    (unless harness
      (user-error "Subagent %s has no live child harness" subagent-id))
    (e-chat-service-queue-session harness session-id prompt)
    (e-subagent-registry-get registry subagent-id)))

(defun e-subagent-raw-read (registry subagent-id &optional limit)
  "Return a bounded raw transcript excerpt for SUBAGENT-ID.
Returns the child's last LIMIT messages (default 20) as compact role/content
plists plus the child's `session://' URI, so the parent can pull detail on
demand without the transcript entering its own context."
  (let* ((record (e-subagent-registry-get registry subagent-id))
         (harness (e-subagent-registry-child-harness registry subagent-id))
         (session-id (plist-get record :session-id))
         (limit (or limit 20))
         (messages
          (and harness
               (plist-get
                (e-harness-executing-session-state harness session-id)
                :messages)))
         (tail (last messages limit)))
    (list :subagent-id subagent-id
          :session-id session-id
          :session-uri (format "session://e/sessions/%s/messages" session-id)
          :messages (mapcar (lambda (message)
                              (list :role (plist-get message :role)
                                    :content (plist-get message :content)))
                            tail))))

(provide 'e-subagent-runner)

;;; e-subagent-runner.el ends here

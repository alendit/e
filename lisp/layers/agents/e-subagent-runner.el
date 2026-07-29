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
(require 'e-session)
(require 'e-subagent-registry)
(require 'e-work)

(define-error 'e-subagent-error "e subagent error")
(define-error 'e-subagent-unknown-type
  "No spawnable subagent type is registered for id" 'e-subagent-error)

(defun e-subagent--lineage-id (parent-harness parent-session-id)
  "Return the tmp lineage id for PARENT-HARNESS PARENT-SESSION-ID.
Reuses the parent's own lineage id when it already has one, so a grandchild
shares the whole lineage root; otherwise the parent's own session id seeds a new
lineage."
  (or (when (and parent-harness parent-session-id)
        (ignore-errors
          (when-let* ((store (e-harness-sessions parent-harness))
                      (session (e-session-get store parent-session-id)))
            (plist-get (plist-get session :metadata) :tmp-lineage-id))))
      parent-session-id))

(defun e-subagent--type-instance (type)
  "Return the spawnable harness instance for TYPE, or signal."
  (let ((instance (e-harness-instance-get type)))
    (unless (and instance (e-harness-instance-subagent-p instance))
      (signal 'e-subagent-unknown-type (list type)))
    instance))

(defvar e-subagent--configured-harnesses (make-hash-table :test 'eq :weakness 'key)
  "Harnesses whose declaring instance's minimal layers/config were applied.
Keyed weakly by harness so a torn-down harness is re-configured if recreated.")

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

(defun e-subagent--child-metadata (instance parent-session-id lineage-id label)
  "Return durable child session metadata for INSTANCE under a parent lineage."
  (let ((role (e-harness-instance-kind instance)))
    (append
     (list :tmp-lineage-id lineage-id)
     (when parent-session-id (list :parent-session-id parent-session-id))
     (when role (list :subagent-role (symbol-name role)))
     (when (and (stringp label) (not (string-empty-p label)))
       (list :subagent-label label)))))

(defun e-subagent--seed-child (child-harness child-session-id seed-messages)
  "Append SEED-MESSAGES to CHILD-SESSION-ID's own store in CHILD-HARNESS.
Each seed is a backend-neutral message plist; the parent chooses exactly what to
hand over, so nothing leaks that it did not name."
  (dolist (message (append seed-messages nil))
    (e-session-append-message
     (e-harness-sessions child-harness)
     child-session-id
     (copy-sequence message))))

(defun e-subagent-direct-runner (child-harness child-session-id prompt
                                               seed-messages on-settle)
  "Seed and start one non-blocking child turn, settling through ON-SETTLE.
Returns a handle plist carrying a `:cancel' function that aborts the child's
active turn.  ON-SETTLE is called as (STATUS &key summary outputs error)."
  (e-subagent--seed-child child-harness child-session-id seed-messages)
  (let ((settled nil)
        subscription)
    (cl-labels
        ((finish
          (status &rest args)
          (unless settled
            (setq settled t)
            (when subscription
              (e-harness-unsubscribe child-harness subscription))
            (apply on-settle status args))))
      (setq subscription
            (e-harness-subscribe
             child-harness
             (lambda (event)
               (pcase (plist-get event :type)
                 ('turn-finished
                  (finish 'done
                          :summary (e-subagent--last-assistant-text
                                    child-harness child-session-id)))
                 ('turn-failed
                  (finish 'failed
                          :error (or (plist-get (plist-get event :payload)
                                                :error)
                                     "Subagent turn failed")))
                 ('turn-cancelled
                  (finish 'cancelled))))
             :session-id child-session-id))
      (condition-case err
          (e-harness-prompt-async child-harness child-session-id prompt)
        (error
         (finish 'failed :error (e-work-error-message err))))
      (list :cancel
            (lambda ()
              (ignore-errors
                (e-harness-abort child-harness child-session-id)))))))

(defun e-subagent--last-assistant-text (harness session-id)
  "Return the last assistant message text for SESSION-ID in HARNESS, or nil."
  (let ((content
         (cl-some (lambda (message)
                    (and (eq (plist-get message :role) 'assistant)
                         (plist-get message :content)))
                  (reverse (e-harness-messages harness session-id)))))
    (and (stringp content) content)))

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

(defun e-subagent--settle (registry subagent-id status &rest args)
  "Settle SUBAGENT-ID in REGISTRY to STATUS with ARGS.
A child-reported structured result is authoritative: once reported, later
chatter never overwrites the recorded summary or outputs.  A terminal record is
never resurrected."
  (when (memq (e-subagent-registry-status registry subagent-id)
              '(queued running blocked))
    (let ((reported (e-subagent-registry-reported-p registry subagent-id))
          (fields (list :status status :finished-at (float-time))))
      (unless reported
        (when (plist-member args :summary)
          (setq fields (plist-put fields :result-summary
                                  (plist-get args :summary))))
        (when (plist-member args :outputs)
          (setq fields (plist-put fields :outputs (plist-get args :outputs)))))
      (when (plist-member args :error)
        (setq fields (plist-put fields :error (plist-get args :error))))
      (apply #'e-subagent-registry-update registry subagent-id fields))))

(defun e-subagent--drive-turn
    (registry subagent-id child-harness session-id prompt seed-messages runner)
  "Start one child turn for SUBAGENT-ID and wire its settle + work handle.
Mint a fresh cooperative `e-work' handle, mirror the record's terminal state
onto it, and settle the record from RUNNER's callback.  Store the handle and
any `:cancel' function on the record.  Return RUNNER's handle plist.  Shared by
`e-subagent-spawn' (first turn) and `e-subagent-resume' (a later turn on an
existing child session)."
  (let* ((runner (or runner #'e-subagent-direct-runner))
         ;; A cooperative work handle mirrors the record's terminal state so a
         ;; subagent is awaitable as an `e-work' handle.  Its runner defers; the
         ;; settle callback below finishes/fails/cancels it.  Resume mints a new
         ;; handle here because the prior one is already spent, and the waitable
         ;; resolver reads the record's current `:work-handle', so `await'
         ;; re-tracks the resumed turn without touching the resolver registry.
         (work-handle (e-work-start (e-subagent--work-spec) nil))
         (handle (funcall runner
                          child-harness session-id prompt seed-messages
                          (lambda (status &rest args)
                            (e-subagent--settle-work-handle
                             work-handle status args)
                            (apply #'e-subagent--settle
                                   registry subagent-id status args)))))
    (e-subagent-registry-update registry subagent-id :work-handle work-handle)
    (when (and (listp handle) (functionp (plist-get handle :cancel)))
      (e-subagent-registry-update registry subagent-id
                                  :cancel (plist-get handle :cancel)))
    handle))

(cl-defun e-subagent-spawn
    (registry parent-harness parent-session-id
              &key type prompt seed-messages label schedule runner)
  "Spawn a subagent of TYPE under a parent lineage and return its record.
REGISTRY tracks the child.  PARENT-HARNESS and PARENT-SESSION-ID identify the
spawning session, whose lineage the child inherits so they share one tmp root.
PROMPT is the child's task.  SEED-MESSAGES are optional explicit context
messages.  LABEL is a human-scannable stub.  SCHEDULE is `direct' (default) or
`queue'.  RUNNER overrides the default direct-turn runner for tests; it is
called as (CHILD-HARNESS CHILD-SESSION-ID PROMPT SEED-MESSAGES ON-SETTLE) and
returns a handle plist carrying `:cancel'."
  (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
    (signal 'wrong-type-argument (list 'stringp :prompt)))
  (let* ((type (e-subagent--normalize-type type))
         (instance (e-subagent--type-instance type))
         (child-harness (e-subagent--child-harness instance))
         (lineage-id (e-subagent--lineage-id parent-harness parent-session-id))
         (metadata (e-subagent--child-metadata
                    instance parent-session-id lineage-id label))
         (child-session (e-harness-create-session
                         child-harness :metadata metadata))
         (child-session-id (plist-get child-session :id))
         (schedule (or schedule 'direct))
         (record (e-subagent-registry-register
                  registry
                  :type type
                  :role (e-harness-instance-kind instance)
                  :session-id child-session-id
                  :parent-session-id parent-session-id
                  :label label
                  :schedule schedule
                  :child-harness child-harness))
         (subagent-id (plist-get record :subagent-id)))
    (e-subagent--drive-turn
     registry subagent-id child-harness child-session-id
     prompt seed-messages runner)
    ;; A synchronous runner may already have settled the record; only a
    ;; still-live record advances to running.
    (when (memq (e-subagent-registry-status registry subagent-id)
                '(queued))
      (e-subagent-registry-update registry subagent-id :status 'running))
    (e-subagent-registry-get registry subagent-id)))

(defun e-subagent-resume (registry subagent-id &optional prompt runner)
  "Resume a settled-but-live SUBAGENT-ID with one new turn on its child session.
The recovery path for a subagent whose turn ended in `failed' or `cancelled'
while its child session and full transcript stayed live -- typically a
transient backend error.  Rather than discard the child's accumulated context
(what a fresh `e-subagent-spawn' would do), start one more turn on the existing
session with PROMPT (default a minimal continue).

Refuses a record that was explicitly `shutdown' (a deliberate terminal intent,
unlike a failure), one already `running', or one whose child harness is gone.
Transitions the record back to `running', clears the prior error and reported
flag so the resumed turn's result can land, mints a fresh awaitable work
handle, and re-arms the settle callback.  Return the normalized record."
  (let* ((status (e-subagent-registry-status registry subagent-id))
         (record (e-subagent-registry-get registry subagent-id))
         (harness (e-subagent-registry-child-harness registry subagent-id))
         (session-id (plist-get record :session-id))
         (prompt (let ((value (and (stringp prompt) (string-trim prompt))))
                   (if (and value (not (string-empty-p value)))
                       value
                     "Continue where you left off."))))
    (unless (memq status '(failed cancelled))
      (user-error "Subagent %s is %s, not resumable (only failed or cancelled)"
                  subagent-id status))
    (when (e-subagent-registry-shutdown-p registry subagent-id)
      (user-error "Subagent %s was shut down; spawn a fresh child instead"
                  subagent-id))
    (unless harness
      (user-error "Subagent %s has no live child harness" subagent-id))
    ;; Re-open the record before driving so the next `--settle' sees a live
    ;; record and advances it; clear terminal residue.
    (e-subagent-registry-update registry subagent-id
                                :status 'running
                                :error nil
                                :finished-at nil
                                :reported nil)
    (e-subagent--drive-turn
     registry subagent-id harness session-id prompt nil runner)
    (e-subagent-registry-get registry subagent-id)))

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
         (harness (e-harness-instance-get-or-create type))
         (enable-layers (plist-get args :enable-layers))
         (disable-layers (plist-get args :disable-layers))
         (layer-config (plist-get args :layer-config)))
    (ignore instance)
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

(defun e-subagent-interrupt (registry subagent-id)
  "Abort SUBAGENT-ID's active child turn, leaving the record inspectable.
Return the normalized record."
  (when-let ((cancel (e-subagent-registry-cancel-function registry subagent-id)))
    (funcall cancel))
  (e-subagent--settle registry subagent-id 'cancelled)
  (e-subagent-registry-get registry subagent-id))

(defun e-subagent-shutdown (registry subagent-id)
  "Interrupt SUBAGENT-ID if running and mark its record terminally shut down.
Unlike a transient failure, shutdown is a deliberate terminal intent, so the
record is flagged `:shutdown' and `e-subagent-resume' refuses it.  Return the
normalized record."
  (e-subagent-interrupt registry subagent-id)
  (e-subagent-registry-update registry subagent-id :shutdown t)
  (e-subagent-registry-get registry subagent-id))

(defun e-subagent-steer (registry subagent-id prompt)
  "Steer SUBAGENT-ID's running child turn with PROMPT.
Steers the active turn in place through the child harness so the parent can
communicate mid-flight.  Return the normalized record."
  (let ((harness (e-subagent-registry-child-harness registry subagent-id))
        (session-id (plist-get (e-subagent-registry-get registry subagent-id)
                               :session-id)))
    (unless harness
      (user-error "Subagent %s has no live child harness" subagent-id))
    (e-harness-steer-active-turn harness session-id prompt)
    (e-subagent-registry-get registry subagent-id)))

(defun e-subagent-send (registry subagent-id prompt)
  "Queue a follow-up PROMPT to SUBAGENT-ID's child session.
Unlike `e-subagent-steer', this submits a follow-up turn rather than steering
the active one, so it needs a running turn to queue behind.  A settled child
has none, so guard status up front like `e-subagent-resume' rather than let
`e-harness-queue-prompt' raise the low-level `e-harness-no-active-turn': point a
`failed' or `cancelled' child at resume, refuse a `done' or shut-down child, and
require a live child harness.  Return the normalized record."
  (let ((status (e-subagent-registry-status registry subagent-id))
        (harness (e-subagent-registry-child-harness registry subagent-id))
        (session-id (plist-get (e-subagent-registry-get registry subagent-id)
                               :session-id)))
    (when (memq status '(failed cancelled))
      (user-error "Subagent %s is %s; use resume to start a new turn, not send"
                  subagent-id status))
    (when (eq status 'done)
      (user-error "Subagent %s is done; spawn a fresh child instead of send"
                  subagent-id))
    (when (e-subagent-registry-shutdown-p registry subagent-id)
      (user-error "Subagent %s was shut down; spawn a fresh child instead"
                  subagent-id))
    (unless harness
      (user-error "Subagent %s has no live child harness" subagent-id))
    (e-harness-queue-prompt harness session-id prompt)
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
         (messages (and harness
                        (e-harness-messages harness session-id)))
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

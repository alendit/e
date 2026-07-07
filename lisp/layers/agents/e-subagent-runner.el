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
         (finish 'failed :error (error-message-string err))))
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
         (child-harness (e-harness-instance-get-or-create type))
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
                  :schedule schedule))
         (subagent-id (plist-get record :subagent-id))
         (runner (or runner #'e-subagent-direct-runner))
         (handle (funcall runner
                          child-harness child-session-id prompt seed-messages
                          (lambda (status &rest args)
                            (apply #'e-subagent--settle
                                   registry subagent-id status args)))))
    (when (and (listp handle) (functionp (plist-get handle :cancel)))
      (e-subagent-registry-update registry subagent-id
                                  :cancel (plist-get handle :cancel)))
    ;; A synchronous runner may already have settled the record; only a
    ;; still-live record advances to running.
    (when (memq (e-subagent-registry-status registry subagent-id)
                '(queued))
      (e-subagent-registry-update registry subagent-id :status 'running))
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
  "Interrupt SUBAGENT-ID if running and mark its record terminal.
Return the normalized record."
  (e-subagent-interrupt registry subagent-id))

(provide 'e-subagent-runner)

;;; e-subagent-runner.el ends here

;;; e-subagent-actions.el --- Subagent capability actions for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability-owned access to subagents.  Agents reach these through
;; `e-actions-call' from `run_elisp', never a model-facing tool surface,
;; matching the `elisp-job', task-queue, and Agent Shell Fleet precedent.  The
;; actions bind the active harness/session context so `spawn' records the child
;; under the calling session's lineage and `report' resolves the calling child.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-chat-service)
(require 'e-subagent-live)
(require 'e-subagent-runner)

(defvar e-subagent-actions-default-live (e-subagent-live-create)
  "Process-wide private live execution owner shared by the capability.")

(defun e-subagent-actions--participant-id (arguments)
  "Return the required durable participant id from ARGUMENTS."
  (let ((value (plist-get arguments :participant-id)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp :participant-id)))
    value))

(defun e-subagent-actions--schedule (value)
  "Normalize a schedule VALUE from the action surface."
  (cond
   ((null value) nil)
   ((symbolp value) value)
   ((stringp value) (intern (string-remove-prefix ":" value)))
   (t (signal 'wrong-type-argument (list 'symbolp :schedule)))))

(defun e-subagent-actions--publication-target (context)
  "Return CONTEXT's explicit parent-session SQL publication target."
  (e-subagent-publication-target
   (plist-get context :harness) (plist-get context :session-id)))

(defun e-subagent-actions--board-id (context)
  "Return CONTEXT's durable Board id."
  (e-chat-service-binding-board-id
   (or (e-chat-service-binding
        (plist-get context :harness) (plist-get context :session-id))
       (signal 'e-subagent-error
               (list "Parent Board binding is not ready")))))

(defun e-subagent-actions--cancel-with-audit
    (operation live context arguments)
  "Run cancellation OPERATION with CONTEXT's explicit audit target.
If target resolution fails, still run cancellation with an unavailable target
so request-owned child coordination is retired, then re-signal the original
resolution error.  An unexpected cancellation defect wins when it leaves the
record live."
  (let ((participant-id (e-subagent-actions--participant-id arguments))
        (reason (plist-get arguments :reason))
        board-id
        target target-error)
    (condition-case error
        (setq board-id (e-subagent-actions--board-id context))
      (error
       (when-let ((identity
                   (e-subagent-live-find-identity live participant-id)))
         (setq board-id (car identity)))
       (unless board-id
         (signal (car error) (cdr error)))))
    (condition-case error
        (setq target (e-subagent-actions--publication-target context))
      (error (setq target-error error)))
    (if (not target-error)
        (funcall operation live board-id target participant-id reason)
      (condition-case cancellation-error
          (funcall operation live board-id nil participant-id reason)
        (error
         ;; A nil audit target is expected to reject publication after the
         ;; runner has cancelled and removed the live record.  Preserve any
         ;; other defect that prevented that required cleanup.
         (when (e-subagent-live-get live board-id participant-id)
           (signal (car cancellation-error) (cdr cancellation-error)))))
      (signal (car target-error) (cdr target-error)))))

(defun e-subagent-actions--spawn (live context arguments)
  "Spawn a subagent from ARGUMENTS under CONTEXT's session lineage."
  (e-subagent-spawn
   live
   (plist-get context :harness)
   (plist-get context :session-id)
   :source-turn-id (plist-get context :turn-id)
   :type (plist-get arguments :type)
   :prompt (plist-get arguments :prompt)
   :seed-messages (plist-get arguments :seed-messages)
   :label (plist-get arguments :label)
   :schedule (e-subagent-actions--schedule (plist-get arguments :schedule))))

(defun e-subagent-actions--list (_live _context _arguments)
  "Signal that durable Board observation owns child listing."
  (user-error "Child listing is provided by Board observation (DP3)"))

(defun e-subagent-actions--status (_live _context _arguments)
  "Signal that durable Board observation owns child status."
  (user-error "Child status is provided by Board observation (DP3)"))

(defun e-subagent-actions--read (_live _context _arguments)
  "Signal that durable Board observation owns child results."
  (user-error "Child results are provided by Board observation (DP3)"))

(defun e-subagent-actions--steer (live context arguments)
  "Steer a running subagent's active turn through LIVE."
  (e-subagent-steer live
                    (e-subagent-actions--board-id context)
                    (e-subagent-actions--publication-target context)
                    (e-subagent-actions--participant-id arguments)
                    (plist-get arguments :prompt)
                    (plist-get arguments :reason)))

(defun e-subagent-actions--send (live context arguments)
  "Queue a follow-up prompt to a subagent through LIVE."
  (e-subagent-send live
                   (e-subagent-actions--board-id context)
                   (e-subagent-actions--participant-id arguments)
                   (plist-get arguments :prompt)))

(defun e-subagent-actions--interrupt (live context arguments)
  "Interrupt a subagent in LIVE."
  (e-subagent-actions--cancel-with-audit
   #'e-subagent-interrupt live context arguments))

(defun e-subagent-actions--shutdown (live context arguments)
  "Shut down a subagent in LIVE."
  (e-subagent-actions--cancel-with-audit
   #'e-subagent-shutdown live context arguments))

(defun e-subagent-actions--configure-type (_live _context arguments)
  "Configure a spawnable type's shared harness from ARGUMENTS."
  (e-subagent-configure-type
   (plist-get arguments :type)
   :enable-layers (plist-get arguments :enable-layers)
   :disable-layers (plist-get arguments :disable-layers)
   :layer-config (plist-get arguments :layer-config)))

(defun e-subagent-actions--report (live context arguments)
  "Record a child-reported structured result for CONTEXT's own session."
  (or (e-subagent-report
       live
       (e-subagent-actions--board-id context)
       (plist-get context :session-id)
       (plist-get arguments :outputs)
       (plist-get arguments :summary)
       (plist-get arguments :result))
      (list :status 'ignored
            :reason "Calling session is not a tracked subagent")))

(defun e-subagent-actions--action (live handler parameters)
  "Return a cheap work action descriptor binding LIVE into HANDLER.
HANDLER is called as (LIVE CONTEXT ARGUMENTS)."
  (e-action-cheap-create
   :owner 'subagents
   :parameters parameters
   :runner (lambda (arguments context)
             (funcall handler live context arguments))))

(defconst e-subagent-actions--spawn-parameters
  '(:type "object"
    :properties
    (:type
     (:type "string"
      :description "Spawnable subagent type id, e.g. reviewer.")
     :prompt
     (:type "string"
      :description "The child's task prompt.")
     :seed-messages
     (:type "array"
      :description "Optional explicit context messages appended before the task prompt.")
     :label
     (:type "string"
      :description "Optional human-scannable stub.")
     :schedule
     (:type "string"
      :description "direct (default) or queue."))
    :required ["type" "prompt"])
  "Action parameters for subagent spawn.")

(defconst e-subagent-actions--participant-id-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string"
      :description "Durable participant/session id returned by spawn."))
    :required ["participant-id"])
  "Action parameters for subagent lookup operations.")

(defconst e-subagent-actions--read-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string"
      :description "Durable participant/session id returned by spawn.")
     :raw
     (:type "boolean"
      :description "Return a bounded raw transcript excerpt plus the session:// URI instead of the compact result.")
     :limit
     (:type "integer"
      :description "Maximum raw messages to return (default 20)."))
    :required ["participant-id"])
  "Action parameters for the read action.")

(defconst e-subagent-actions--steer-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string"
      :description "Durable participant/session id returned by spawn.")
     :prompt
     (:type "string"
      :description "Prompt to steer into the running turn or queue as a follow-up.")
     :reason
     (:type "string"
      :description "Optional bounded audit reason; it is not sent to the child."))
    :required ["participant-id" "prompt"])
  "Action parameters for steer and send.")

(defconst e-subagent-actions--send-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string"
      :description "Durable participant/session id returned by spawn.")
     :prompt
     (:type "string"
      :description "Prompt to queue as a follow-up turn."))
    :required ["participant-id" "prompt"])
  "Action parameters for send.")

(defconst e-subagent-actions--intervention-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string"
      :description "Durable participant/session id returned by spawn.")
     :reason
     (:type "string"
      :description "Optional bounded audit reason; it is not sent to the child."))
    :required ["participant-id"])
  "Action parameters for interrupt and shutdown.")

(defconst e-subagent-actions--report-parameters
  '(:type "object"
    :properties
    (:outputs
     (:type "array"
      :description "Structured artifact list, each (:kind :value|:uri :label).")
     :result
     (:type "object"
      :description "Optional bounded application-owned structured result.")
     :summary
     (:type "string"
      :description "Short result summary."))
    :required [])
  "Action parameters for the child-side report action.")

(defconst e-subagent-actions--configure-type-parameters
  '(:type "object"
    :properties
    (:type
     (:type "string"
      :description "Spawnable subagent type id to configure, e.g. tool-user.")
     :enable-layers
     (:type "array"
      :description "Layer ids to enable on the type's shared harness, e.g. [\"web\"].")
     :disable-layers
     (:type "array"
      :description "Layer ids to disable on the type's shared harness.")
     :layer-config
     (:type "object"
      :description "Alist mapping a capability id to its option plist, e.g. ((agents-std-context :skills-include (\"writing\"))). Generic way to pass or overwrite a layer's configuration."))
    :required ["type"])
  "Action parameters for configuring a spawnable type's harness.")

(defun e-subagent-actions-parent-alist (&optional live)
  "Return the parent-facing subagent actions plist bound to LIVE.
These are the actions a session uses to spawn and manage its children:
spawn, list, status, read, steer, send, interrupt, shutdown,
configure-type.  The child-side `report' is not here; see
`e-subagent-actions-child-alist'."
  (let ((live (or live e-subagent-actions-default-live)))
    (list
     :spawn
     (e-subagent-actions--action
      live #'e-subagent-actions--spawn e-subagent-actions--spawn-parameters)
     :list
     (e-subagent-actions--action
      live #'e-subagent-actions--list nil)
     :status
     (e-subagent-actions--action
      live #'e-subagent-actions--status
      e-subagent-actions--participant-id-parameters)
     :read
     (e-subagent-actions--action
      live #'e-subagent-actions--read
      e-subagent-actions--read-parameters)
     :steer
     (e-subagent-actions--action
      live #'e-subagent-actions--steer
      e-subagent-actions--steer-parameters)
     :send
     (e-subagent-actions--action
      live #'e-subagent-actions--send
      e-subagent-actions--send-parameters)
     :interrupt
     (e-subagent-actions--action
      live #'e-subagent-actions--interrupt
      e-subagent-actions--intervention-parameters)
     :shutdown
     (e-subagent-actions--action
      live #'e-subagent-actions--shutdown
      e-subagent-actions--intervention-parameters)
     :configure-type
     (e-subagent-actions--action
      live #'e-subagent-actions--configure-type
      e-subagent-actions--configure-type-parameters))))

(defun e-subagent-actions-child-alist (&optional live)
  "Return the child-facing subagent actions plist bound to LIVE.
A spawned child gets only `report', so it can set a structured result for its
own session without seeing the spawn surface or the spawnable type catalog."
  (let ((live (or live e-subagent-actions-default-live)))
    (list
     :report
     (e-subagent-actions--action
      live #'e-subagent-actions--report
      e-subagent-actions--report-parameters))))

(provide 'e-subagent-actions)

;;; e-subagent-actions.el ends here

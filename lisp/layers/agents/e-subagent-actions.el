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
(require 'e-json)
(require 'e-subagent-live)
(require 'e-subagent-runner)

(defvar e-subagent-actions-default-live (e-subagent-runner-live-owner)
  "Process-wide private live execution owner shared by the runner capability.

The action adapter and runner consumer operations must observe one exact live
execution boundary so a child can report through `subagents' while a Board
consumer dispatches it.  The owner remains private and carries no durable
inventory or terminal projection.")

(defun e-subagent-actions--participant-id (arguments)
  "Return the required durable participant id from ARGUMENTS."
  (let ((value (plist-get arguments :participant-id)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp :participant-id)))
    value))

(defun e-subagent-actions--schedule (value)
  "Map canonical schedule VALUE to the runner's domain symbol."
  (cond
   ((null value) nil)
   ((stringp value) (intern (string-remove-prefix ":" value)))
   (t (signal 'wrong-type-argument (list 'stringp :schedule)))))

(defun e-subagent-actions--seed-message (value)
  "Map one canonical seed message VALUE to the session domain form."
  (unless (and (listp value)
               (stringp (plist-get value :role))
               (stringp (plist-get value :content)))
    (signal 'wrong-type-argument (list 'canonical-seed-message value)))
  (list :role (intern (plist-get value :role))
        :content (plist-get value :content)))

(defun e-subagent-actions--seed-messages (arguments)
  "Map canonical seed-message vector in ARGUMENTS to a domain list."
  (let ((messages (plist-get arguments :seed-messages)))
    (and messages
         (mapcar #'e-subagent-actions--seed-message
                 (append messages nil)))))

(defun e-subagent-actions--layer-config (arguments)
  "Map canonical layer-config object in ARGUMENTS to an owner alist."
  (let ((value (plist-get arguments :layer-config))
        result)
    (while value
      (let* ((key (pop value))
             (config (pop value)))
        (unless (or (null config) (listp config))
          (signal 'wrong-type-argument
                  (list 'canonical-capability-config config)))
        (push (cons (intern (substring (symbol-name key) 1)) config)
              result)))
    (nreverse result)))

(defun e-subagent-actions--publication-target (context)
  "Return CONTEXT's explicit parent-session SQL publication target."
  (condition-case error
      (e-subagent-publication-target
       (plist-get context :harness) (plist-get context :session-id))
    (error
     ;; Keep the action's public domain error stable when the parent Board
     ;; closes between lookup and audit publication.  The cancellation wrapper
     ;; still requests child cancellation with a nil audit target before this
     ;; error is returned to the caller.
     (signal 'e-subagent-error
             (list "Parent Board publication target is unavailable" error)))))

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
       (when-let* ((identity
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
        ;; A nil audit target is expected to reject the intervention
        ;; publication.  The runner has still requested cancellation before
        ;; that audit attempt; preserve the original public target error
        ;; rather than leaking the audit target's low-level type error.
        (unless (and (eq (car cancellation-error) 'wrong-type-argument)
                     (memq (cadr cancellation-error)
                           '(e-board-sqlite-publication-target
                             e-board-sqlite-publication-target-p)))
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
   :seed-messages (e-subagent-actions--seed-messages arguments)
   :label (plist-get arguments :label)
   :schedule (e-subagent-actions--schedule (plist-get arguments :schedule))))

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
   :enable-layers (and (plist-member arguments :enable-layers)
                       (append (plist-get arguments :enable-layers) nil))
   :disable-layers (and (plist-member arguments :disable-layers)
                        (append (plist-get arguments :disable-layers) nil))
   :layer-config (and (plist-member arguments :layer-config)
                      (e-subagent-actions--layer-config arguments))))

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

(defun e-subagent-actions--canonical-string (value)
  "Return VALUE as a canonical string or explicit JSON null."
  (cond
   ((stringp value) value)
   ((null value) e-json-null)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun e-subagent-actions--canonical-output (value)
  "Project one owner output VALUE into canonical JSON."
  (list :kind (e-subagent-actions--canonical-string (plist-get value :kind))
        :uri (e-subagent-actions--canonical-string (plist-get value :uri))
        :value (e-subagent-actions--canonical-string
                (plist-get value :value))
        :label (e-subagent-actions--canonical-string (plist-get value :label))))

(defun e-subagent-actions--canonical-record (value)
  "Project transient subagent record VALUE into canonical JSON."
  (list :board-id (e-subagent-actions--canonical-string
                   (plist-get value :board-id))
        :participant-id (e-subagent-actions--canonical-string
                         (plist-get value :participant-id))
        :session-id (e-subagent-actions--canonical-string
                     (plist-get value :session-id))
        :parent-session-id (e-subagent-actions--canonical-string
                            (plist-get value :parent-session-id))
        :type (e-subagent-actions--canonical-string (plist-get value :type))
        :role (e-subagent-actions--canonical-string (plist-get value :role))
        :label (e-subagent-actions--canonical-string (plist-get value :label))
        :schedule (e-subagent-actions--canonical-string
                   (plist-get value :schedule))
        :status (e-subagent-actions--canonical-string
                 (plist-get value :status))
        :await-ref (e-subagent-actions--canonical-string
                    (plist-get value :await-ref))
        :work-id (e-subagent-actions--canonical-string
                  (plist-get value :work-id))
        :run-id (e-subagent-actions--canonical-string
                 (plist-get value :run-id))
        :task-key (e-subagent-actions--canonical-string
                   (plist-get value :task-key))
        :attempt (if (integerp (plist-get value :attempt))
                     (plist-get value :attempt)
                   e-json-null)
        :result-summary (e-subagent-actions--canonical-string
                         (plist-get value :result-summary))
        :error (e-subagent-actions--canonical-string
                (plist-get value :error))
        :outputs (vconcat (mapcar #'e-subagent-actions--canonical-output
                                  (or (plist-get value :outputs) nil)))
        :result (let ((result (plist-get value :result)))
                  (if (e-json-value-p result) result e-json-null))))

(defun e-subagent-actions--canonical-config-result (value)
  "Project configure-type VALUE without exposing domain config records."
  (list :type (e-subagent-actions--canonical-string (plist-get value :type))
        :enabled-layers
        (vconcat (mapcar #'e-subagent-actions--canonical-string
                         (or (plist-get value :enabled-layers) nil)))
        :configured-capabilities
        (vconcat
         (mapcar (lambda (entry)
                   (e-subagent-actions--canonical-string (car entry)))
                 (or (plist-get value :capability-config) nil)))))

(defun e-subagent-actions--canonical-result (value)
  "Project one subagent action VALUE into canonical JSON."
  (cond
   ((and (listp value) (plist-member value :reported))
    (list :participant-id
          (e-subagent-actions--canonical-string
           (plist-get value :participant-id))
          :session-id
          (e-subagent-actions--canonical-string
           (plist-get value :session-id))
          :reported (if (eq (plist-get value :reported) t)
                        t e-json-false)))
   ((and (listp value) (plist-member value :enabled-layers))
    (e-subagent-actions--canonical-config-result value))
   ((and (listp value) (plist-member value :participant-id))
    (e-subagent-actions--canonical-record value))
   ((e-json-value-p value) value)
   (t (signal 'e-json-error
              (list "Subagent action returned a noncanonical result")))))

(defun e-subagent-actions--action (live handler parameters)
  "Return a cheap work action descriptor binding LIVE into HANDLER.
HANDLER is called as (LIVE CONTEXT ARGUMENTS)."
  (e-action-cheap-create
   :owner 'subagents
   :parameters parameters
   :runner (lambda (arguments context)
             (e-subagent-actions--canonical-result
              (funcall handler live context arguments)))))

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
      :items (:type "object"
              :properties (:role (:type "string")
                           :content (:type "string"))
              :required ["role" "content"]
              :additionalProperties :json-false)
      :description "Optional explicit context messages appended before the task prompt.")
     :label
     (:type "string"
      :description "Optional human-scannable stub.")
     :schedule
     (:type "string"
      :description "direct (default) or queue."))
    :required ["type" "prompt"]
    :additionalProperties :json-false)
  "Action parameters for subagent spawn.")

(defconst e-subagent-actions--participant-id-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string"
      :description "Durable participant/session id returned by spawn."))
    :required ["participant-id"]
    :additionalProperties :json-false)
  "Action parameters for subagent lookup operations.")

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
    :required ["participant-id" "prompt"]
    :additionalProperties :json-false)
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
    :required ["participant-id" "prompt"]
    :additionalProperties :json-false)
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
    :required ["participant-id"]
    :additionalProperties :json-false)
  "Action parameters for interrupt and shutdown.")

(defconst e-subagent-actions--report-parameters
  '(:type "object"
    :properties
    (:outputs
     (:type "array"
      :items (:type "object"
              :properties (:kind (:type "string")
                           :uri (:type "string")
                           :value (:type "string")
                           :label (:type "string"))
              :required ["kind"]
              :additionalProperties :json-false)
      :description "Structured artifact list, each with string kind and uri/value/label.")
     :result
     (:type "object"
      :description "Optional bounded application-owned structured result.")
     :summary
     (:type "string"
      :description "Short result summary."))
    :required []
    :additionalProperties :json-false)
  "Action parameters for the child-side report action.")

(defconst e-subagent-actions--configure-type-parameters
  '(:type "object"
    :properties
    (:type
     (:type "string"
      :description "Spawnable subagent type id to configure, e.g. tool-user.")
     :enable-layers
     (:type "array"
      :items (:type "string")
      :description "Layer ids to enable on the type's shared harness, e.g. [\"web\"].")
     :disable-layers
     (:type "array"
      :items (:type "string")
      :description "Layer ids to disable on the type's shared harness.")
     :layer-config
     (:type "object"
      :description "Object mapping capability ids to canonical option objects."))
    :required ["type"]
    :additionalProperties :json-false)
  "Action parameters for configuring a spawnable type's harness.")

(defun e-subagent-actions-parent-alist (&optional live)
  "Return the parent-facing subagent actions plist bound to LIVE.
These are the actions a session uses to spawn and manage its children:
spawn, steer, send, interrupt, shutdown, and configure-type.  Board-owned
observation actions are contributed by the independent `board' capability.
The child-side `report' is not here; see `e-subagent-actions-child-alist'."
  (let ((live (or live e-subagent-actions-default-live)))
    (append
     (list
      :spawn
      (e-subagent-actions--action
       live #'e-subagent-actions--spawn e-subagent-actions--spawn-parameters))
     (list
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
       e-subagent-actions--configure-type-parameters)))))

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

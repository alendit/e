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
(require 'e-subagent-registry)
(require 'e-subagent-runner)

(defvar e-subagent-actions-default-registry (e-subagent-registry-create)
  "Process-wide default subagent registry shared by the capability.")

(defun e-subagent-actions--subagent-id (arguments)
  "Return the required subagent id from ARGUMENTS."
  (let ((value (plist-get arguments :subagent-id)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp :subagent-id)))
    value))

(defun e-subagent-actions--schedule (value)
  "Normalize a schedule VALUE from the action surface."
  (cond
   ((null value) nil)
   ((symbolp value) value)
   ((stringp value) (intern (string-remove-prefix ":" value)))
   (t (signal 'wrong-type-argument (list 'symbolp :schedule)))))

(defun e-subagent-actions--spawn (registry context arguments)
  "Spawn a subagent from ARGUMENTS under CONTEXT's session lineage."
  (e-subagent-spawn
   registry
   (plist-get context :harness)
   (plist-get context :session-id)
   :type (plist-get arguments :type)
   :prompt (plist-get arguments :prompt)
   :seed-messages (plist-get arguments :seed-messages)
   :label (plist-get arguments :label)
   :schedule (e-subagent-actions--schedule (plist-get arguments :schedule))))

(defun e-subagent-actions--list (registry context _arguments)
  "Return CONTEXT session's direct children from REGISTRY, newest-first."
  (e-subagent-registry-list registry (plist-get context :session-id)))

(defun e-subagent-actions--status (registry _context arguments)
  "Return one subagent record from REGISTRY."
  (e-subagent-registry-get registry
                           (e-subagent-actions--subagent-id arguments)))

(defun e-subagent-actions--read (registry _context arguments)
  "Return the compact result and outputs for a subagent in REGISTRY.
With `:raw' non-nil, return a bounded transcript excerpt and the child's
`session://' URI instead."
  (let ((subagent-id (e-subagent-actions--subagent-id arguments)))
    (if (plist-get arguments :raw)
        (e-subagent-raw-read registry subagent-id (plist-get arguments :limit))
      (let ((record (e-subagent-registry-get registry subagent-id)))
        (list :subagent-id subagent-id
              :status (plist-get record :status)
              :result-summary (plist-get record :result-summary)
              :outputs (plist-get record :outputs)
              :session-id (plist-get record :session-id))))))

(defun e-subagent-actions--steer (registry _context arguments)
  "Steer a running subagent's active turn in REGISTRY."
  (e-subagent-steer registry
                    (e-subagent-actions--subagent-id arguments)
                    (plist-get arguments :prompt)))

(defun e-subagent-actions--send (registry _context arguments)
  "Queue a follow-up prompt to a subagent in REGISTRY."
  (e-subagent-send registry
                   (e-subagent-actions--subagent-id arguments)
                   (plist-get arguments :prompt)))

(defun e-subagent-actions--interrupt (registry _context arguments)
  "Interrupt a subagent in REGISTRY."
  (e-subagent-interrupt registry
                        (e-subagent-actions--subagent-id arguments)))

(defun e-subagent-actions--shutdown (registry _context arguments)
  "Shut down a subagent in REGISTRY."
  (e-subagent-shutdown registry
                       (e-subagent-actions--subagent-id arguments)))

(defun e-subagent-actions--configure-type (_registry _context arguments)
  "Configure a spawnable type's shared harness from ARGUMENTS."
  (e-subagent-configure-type
   (plist-get arguments :type)
   :enable-layers (plist-get arguments :enable-layers)
   :disable-layers (plist-get arguments :disable-layers)
   :layer-config (plist-get arguments :layer-config)))

(defun e-subagent-actions--report (registry context arguments)
  "Record a child-reported structured result for CONTEXT's own session."
  (or (e-subagent-report
       registry
       (plist-get context :session-id)
       (plist-get arguments :outputs)
       (plist-get arguments :summary))
      (list :status 'ignored
            :reason "Calling session is not a tracked subagent")))

(defun e-subagent-actions--action (registry handler parameters)
  "Return a cheap work action descriptor binding REGISTRY into HANDLER.
HANDLER is called as (REGISTRY CONTEXT ARGUMENTS)."
  (e-action-cheap-create
   :owner 'subagents
   :parameters parameters
   :runner (lambda (arguments context)
             (funcall handler registry context arguments))))

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

(defconst e-subagent-actions--subagent-id-parameters
  '(:type "object"
    :properties
    (:subagent-id
     (:type "string"
      :description "Subagent id returned by spawn."))
    :required ["subagent-id"])
  "Action parameters for subagent lookup operations.")

(defconst e-subagent-actions--read-parameters
  '(:type "object"
    :properties
    (:subagent-id
     (:type "string"
      :description "Subagent id returned by spawn.")
     :raw
     (:type "boolean"
      :description "Return a bounded raw transcript excerpt plus the session:// URI instead of the compact result.")
     :limit
     (:type "integer"
      :description "Maximum raw messages to return (default 20)."))
    :required ["subagent-id"])
  "Action parameters for the read action.")

(defconst e-subagent-actions--steer-parameters
  '(:type "object"
    :properties
    (:subagent-id
     (:type "string"
      :description "Subagent id returned by spawn.")
     :prompt
     (:type "string"
      :description "Prompt to steer into the running turn or queue as a follow-up."))
    :required ["subagent-id" "prompt"])
  "Action parameters for steer and send.")

(defconst e-subagent-actions--report-parameters
  '(:type "object"
    :properties
    (:outputs
     (:type "array"
      :description "Structured artifact list, each (:kind :value|:uri :label).")
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

(defun e-subagent-actions-alist (&optional registry)
  "Return the subagent capability actions plist bound to REGISTRY."
  (let ((registry (or registry e-subagent-actions-default-registry)))
    (list
     :spawn
     (e-subagent-actions--action
      registry #'e-subagent-actions--spawn e-subagent-actions--spawn-parameters)
     :list
     (e-subagent-actions--action
      registry #'e-subagent-actions--list nil)
     :status
     (e-subagent-actions--action
      registry #'e-subagent-actions--status
      e-subagent-actions--subagent-id-parameters)
     :read
     (e-subagent-actions--action
      registry #'e-subagent-actions--read
      e-subagent-actions--read-parameters)
     :steer
     (e-subagent-actions--action
      registry #'e-subagent-actions--steer
      e-subagent-actions--steer-parameters)
     :send
     (e-subagent-actions--action
      registry #'e-subagent-actions--send
      e-subagent-actions--steer-parameters)
     :interrupt
     (e-subagent-actions--action
      registry #'e-subagent-actions--interrupt
      e-subagent-actions--subagent-id-parameters)
     :shutdown
     (e-subagent-actions--action
      registry #'e-subagent-actions--shutdown
      e-subagent-actions--subagent-id-parameters)
     :configure-type
     (e-subagent-actions--action
      registry #'e-subagent-actions--configure-type
      e-subagent-actions--configure-type-parameters)
     :report
     (e-subagent-actions--action
      registry #'e-subagent-actions--report
      e-subagent-actions--report-parameters))))

(provide 'e-subagent-actions)

;;; e-subagent-actions.el ends here

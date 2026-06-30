;;; e-task-queue-actions.el --- Task queue capability actions for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability-owned access to the agent task queue.  Agents and other
;; capabilities reach the queue through `e-actions-call', never a model-facing
;; tool surface, matching the `elisp-job' and Agent Shell Fleet precedent.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-layers)
(require 'e-skills)
(require 'e-task-queue)

(defconst e-task-queue-actions-instructions
  "Use Task Queue actions to enqueue background agent work, list tracked tasks, inspect a task's status and outputs, and cancel queued or running tasks. Read e://task-queue/skills/task-queue for the action contract."
  "Compact Task Queue coordinator guidance.")

(defconst e-task-queue-actions-skill
  (string-join
   '("# Task Queue work actions"
     ""
     "The task queue runs agent prompts as ordinary harness turns at bounded concurrency (default 2). The e harness does not know about the queue."
     ""
     "## Actions"
     ""
     "- `enqueue`: input `(:prompt STRING :metadata PLIST :harness-instance-id KEYWORD)`. Returns a `queued` task record immediately; the dispatcher may start it before the call returns. `metadata` is an opaque plist the enqueuer owns. `harness-instance-id` is optional and defaults to the queue default, resolved at dispatch time."
     "- `list-tasks`: returns compact normalized records, newest-first."
     "- `task-status`: input `(:task-id STRING)`. Returns one normalized record."
     "- `read-task`: input `(:task-id STRING)`. Returns the task's collected outputs."
     "- `cancel-task`: input `(:task-id STRING)`. Cancels a queued task before it runs or interrupts a running task and drops its result."
     ""
     "## Status lifecycle"
     ""
     "`queued` -> `running` -> `done`; a turn error lands `failed`; cancel lands `cancelled`. Terminal states are immutable."
     ""
     "## Harness selection"
     ""
     "Each task picks its harness instance like a chat buffer does. A task with no `harness-instance-id` runs on the queue default; an unregistered instance id fails just that task without stalling the dispatcher.")
   "\n")
  "Detailed Task Queue action reference.")

(defvar e-task-queue-actions-default-queue
  (e-task-queue-create)
  "Default in-memory task queue backing the capability actions.")

(defun e-task-queue-actions--task-id (arguments)
  "Return the required task id from ARGUMENTS."
  (let ((value (plist-get arguments :task-id)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp :task-id)))
    value))

(defun e-task-queue-actions--enqueue (queue arguments)
  "Enqueue work described by ARGUMENTS on QUEUE."
  (e-task-queue-enqueue
   queue
   :prompt (plist-get arguments :prompt)
   :metadata (plist-get arguments :metadata)
   :harness-instance-id (plist-get arguments :harness-instance-id)))

(defun e-task-queue-actions--list-tasks (queue _arguments)
  "Return tracked task records from QUEUE, newest-first."
  (e-task-queue-list queue))

(defun e-task-queue-actions--task-status (queue arguments)
  "Return one task record from QUEUE."
  (e-task-queue-get queue (e-task-queue-actions--task-id arguments)))

(defun e-task-queue-actions--read-task (queue arguments)
  "Return the outputs collected for a task in QUEUE."
  (let ((task-id (e-task-queue-actions--task-id arguments)))
    (list :task-id task-id
          :outputs (e-task-queue-outputs queue task-id))))

(defun e-task-queue-actions--cancel-task (queue arguments)
  "Cancel a task in QUEUE."
  (e-task-queue-cancel queue (e-task-queue-actions--task-id arguments)))

(defun e-task-queue-actions--action (handler parameters)
  "Return a task queue action descriptor for HANDLER with PARAMETERS."
  (e-action-create :handler handler :parameters parameters))

(defconst e-task-queue-actions--enqueue-parameters
  '(:type "object"
    :properties
    (:prompt
     (:type "string"
      :description "Prompt the queued task submits as one harness turn.")
     :metadata
     (:type "object"
      :description "Opaque provenance plist the enqueuer owns.")
     :harness-instance-id
     (:type "string"
      :description "Harness instance id to run on. Defaults to the queue default."))
    :required ["prompt"])
  "Action parameters for task queue enqueue.")

(defconst e-task-queue-actions--task-id-parameters
  '(:type "object"
    :properties
    (:task-id
     (:type "string"
      :description "Task id returned by enqueue."))
    :required ["task-id"])
  "Action parameters for task lookup operations.")

(cl-defun e-task-queue-capability-create (&key (id 'task-queue) (name "Task Queue") queue)
  "Create the Task Queue capability.
QUEUE defaults to `e-task-queue-actions-default-queue'."
  (let ((queue (or queue e-task-queue-actions-default-queue)))
    (e-capability-with-skills-create
     :id id
     :name name
     :instruction-priority 255
     :instructions e-task-queue-actions-instructions
     :actions
     (list :enqueue
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--enqueue queue arguments))
            e-task-queue-actions--enqueue-parameters)
           :list-tasks
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--list-tasks queue arguments))
            nil)
           :task-status
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--task-status queue arguments))
            e-task-queue-actions--task-id-parameters)
           :read-task
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--read-task queue arguments))
            e-task-queue-actions--task-id-parameters)
           :cancel-task
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--cancel-task queue arguments))
            e-task-queue-actions--task-id-parameters))
     :skills
     (list
      (e-skill-spec-create
       :name "task-queue"
       :description "Enqueue and observe background agent work through a bounded queue."
       :content e-task-queue-actions-skill)))))

(defun e-task-queue-layer-create ()
  "Create the Task Queue layer."
  (e-layer-create
   :id 'task-queue
   :name "Task Queue"
   :capabilities (list (e-task-queue-capability-create))))

(provide 'e-task-queue-actions)

;;; e-task-queue-actions.el ends here

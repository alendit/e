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
(require 'subr-x)
(require 'e-capabilities)
(require 'e-layers)
(require 'e-skills)
(require 'e-task-queue)

(defconst e-task-queue-actions-instructions
  "Use Task Queue actions to enqueue background agent work, list tracked tasks, inspect a task's status and outputs, and cancel queued or running tasks, and pause or resume individual tasks or the whole queue. Read e://task-queue/skills/task-queue for the action contract."
  "Compact Task Queue coordinator guidance.")

(defconst e-task-queue-actions-skill
  (string-join
   '("# Task Queue work actions"
     ""
     "The task queue runs agent prompts as ordinary harness turns at bounded concurrency (default 2). The e harness does not know about the queue."
     ""
     "## Actions"
     ""
     "- `enqueue`: input `(:prompt STRING :summary STRING :metadata PLIST :harness-instance-id STRING)`. Returns a `queued` task record immediately; the dispatcher may start it before the call returns. `summary` is a short human-worded stub (like a topic title) shown in the queue list; supply it so the list is scannable instead of showing a prompt prefix. `metadata` is an opaque plist the enqueuer owns. `harness-instance-id` is optional (an instance id string such as `chat-project-e`) and defaults to the queue default, resolved at dispatch time."
     "- `list-tasks`: returns compact normalized records, newest-first."
     "- `task-status`: input `(:task-id STRING)`. Returns one normalized record."
     "- `read-task`: input `(:task-id STRING)`. Returns the task's collected outputs."
     "- `cancel-task`: input `(:task-id STRING)`. Cancels a queued task before it runs or interrupts a running task and drops its result."
     "- `pause-task`: input `(:task-id STRING)`. Holds a queued task in place or stops a running task at its next turn boundary, landing it `paused`."
     "- `resume-task`: input `(:task-id STRING)`. Returns a paused task to `queued`, where it re-runs from its prompt."
     "- `pause-all` / `resume-all`: no input. Set or clear the queue-level pause gate and pause/resume every non-terminal task."
     ""
     "## Status lifecycle"
     ""
     "`queued` -> `running` -> `done`; a turn error lands `failed`; cancel lands `cancelled`. Terminal states are immutable. `pause` holds a task in the non-terminal `paused` state and `resume` returns it to `queued`. A `failed` task with retries left and a session to reference is auto-retried: it re-arms as `queued` with an analyze-the-failure-and-continue prompt that references the failed session, tracked by the record's `retries` counter (see `e-task-queue-max-retries`)."
     ""
     "## Harness selection"
     ""
     "Each task picks its harness instance like a chat buffer does. A task with no `harness-instance-id` runs on the queue default; an unregistered instance id fails just that task without stalling the dispatcher.")
   "\n")
  "Detailed Task Queue action reference.")

(defvar e-task-queue-actions-default-queue
  (e-task-queue-create :directory e-task-queue-directory)
  "Default durable task queue backing the capability actions.
It persists to `e-task-queue-directory'; `e-task-queue-load' rehydrates it.")

(defun e-task-queue-actions-ensure-loaded ()
  "Rehydrate the default durable queue from disk once, and return it.
Idempotent: the `loaded' property guards against repeated disk reads.  Every
path that first touches the shared default queue -- the layer factory and the
list buffer alike -- calls this, so rehydration and re-dispatch of persisted
queued work never depend on a harness happening to build the task-queue layer
first."
  (unless (get 'e-task-queue-actions-default-queue 'loaded)
    (ignore-errors (e-task-queue-load e-task-queue-actions-default-queue))
    (put 'e-task-queue-actions-default-queue 'loaded t))
  e-task-queue-actions-default-queue)

(defun e-task-queue-actions--task-id (arguments)
  "Return the required task id from ARGUMENTS."
  (let ((value (plist-get arguments :task-id)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp :task-id)))
    value))

(defun e-task-queue-actions--instance-id (value)
  "Normalize a harness instance id VALUE from the action surface.
Actions arrive as JSON, so an instance id reaches here as a string; the harness
catalog keys instances by keyword.  Intern a string into the keyword the catalog
expects and pass an existing keyword or nil through unchanged."
  (cond
   ((null value) nil)
   ((keywordp value) value)
   ((stringp value)
    (intern (concat ":" (string-remove-prefix ":" value))))
   (t (signal 'wrong-type-argument (list 'stringp :harness-instance-id)))))

(defun e-task-queue-actions--enqueue (queue arguments)
  "Enqueue work described by ARGUMENTS on QUEUE."
  (e-task-queue-enqueue
   queue
   :prompt (plist-get arguments :prompt)
   :summary (plist-get arguments :summary)
   :metadata (plist-get arguments :metadata)
   :harness-instance-id (e-task-queue-actions--instance-id
                         (plist-get arguments :harness-instance-id))))

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

(defun e-task-queue-actions--pause-task (queue arguments)
  "Pause a task in QUEUE."
  (e-task-queue-pause queue (e-task-queue-actions--task-id arguments)))

(defun e-task-queue-actions--resume-task (queue arguments)
  "Resume a task in QUEUE."
  (e-task-queue-resume queue (e-task-queue-actions--task-id arguments)))

(defun e-task-queue-actions--pause-all (queue _arguments)
  "Pause QUEUE and return its tasks newest-first."
  (e-task-queue-pause-all queue)
  (e-task-queue-list queue))

(defun e-task-queue-actions--resume-all (queue _arguments)
  "Resume QUEUE and return its tasks newest-first."
  (e-task-queue-resume-all queue)
  (e-task-queue-list queue))

(defun e-task-queue-actions--action (handler parameters)
  "Return a task queue action descriptor for HANDLER with PARAMETERS."
  (e-action-create :handler handler :parameters parameters))

(defconst e-task-queue-actions--enqueue-parameters
  '(:type "object"
    :properties
    (:prompt
     (:type "string"
      :description "Prompt the queued task submits as one harness turn.")
     :summary
     (:type "string"
      :description "Short human-worded stub describing the task, shown in the queue list (like a topic title). Optional; falls back to a prompt prefix.")
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
            e-task-queue-actions--task-id-parameters)
           :pause-task
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--pause-task queue arguments))
            e-task-queue-actions--task-id-parameters)
           :resume-task
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--resume-task queue arguments))
            e-task-queue-actions--task-id-parameters)
           :pause-all
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--pause-all queue arguments))
            nil)
           :resume-all
           (e-task-queue-actions--action
            (lambda (arguments)
              (e-task-queue-actions--resume-all queue arguments))
            nil))
     :skills
     (list
      (e-skill-spec-create
       :name "task-queue"
       :description "Enqueue and observe background agent work through a bounded queue."
       :content e-task-queue-actions-skill)))))

(defun e-task-queue-layer-create ()
  "Create the Task Queue layer.
Rehydrate the default durable queue from disk the first time the layer is
built, so queued and paused work survives an Emacs restart."
  (e-task-queue-actions-ensure-loaded)
  (e-layer-create
   :id 'task-queue
   :name "Task Queue"
   :capabilities (list (e-task-queue-capability-create))))

(provide 'e-task-queue-actions)

;;; e-task-queue-actions.el ends here

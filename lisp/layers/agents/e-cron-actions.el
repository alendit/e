;;; e-cron-actions.el --- Cron schedule routing and capability for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Turns a declarative schedule entry into a live `e-cron' schedule whose action
;; routes a fire to an existing e primitive.  The `e-cron' engine owns timing
;; and calls a plain action function; this module builds that function so a
;; schedule can enqueue a task-queue prompt, wake a registered background
;; session, or call a named handler.  It is the seam where the timing engine
;; meets the execution substrates, and the only place that depends on both.
;;
;; It also exposes the schedules as capability actions so an agent can register,
;; list, and control schedules through `e-actions-call'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-cron)
(require 'e-layers)
(require 'e-skills)
(require 'e-task-queue)
(require 'e-task-queue-actions)
(require 'e-background-session)

(define-error 'e-cron-actions-invalid-action "Invalid cron action spec")

(defconst e-cron-actions-instructions
  "Use Cron Schedule actions to register cron-like schedules that enqueue a task-queue prompt or wake a background session on a recurrence, list every schedule with its next and last fire, and enable, disable, or remove one at runtime. Read e://cron/skills/cron for the action contract."
  "Compact Cron Schedule coordinator guidance.")

(defconst e-cron-actions-skill
  (string-join
   '("# Cron Schedule work actions"
     ""
     "Schedules run agent work on a recurrence, built on Emacs timers -- no external cron. The engine owns timing only: a fire routes to the task queue or a background session."
     ""
     "## Recurrence (`when`)"
     ""
     "- Interval: `(:every SECONDS)` fires every SECONDS."
     "- Calendar: `(:at \"HH:MM\" :on (mon tue wed thu fri))` fires at that local time on those weekdays. Weekday symbols are `sun mon tue wed thu fri sat`; omit `:on` for every day."
     ""
     "## Action (`action`)"
     ""
     "- `(:enqueue (:prompt STRING :harness-instance-id ID :metadata PLIST))`: enqueue a task-queue prompt on each fire."
     "- `(:wake TRIGGER-ID)`: fire the registered background-session trigger with that id on each fire."
     "- `(:call SYMBOL)`: call a named zero/one-argument handler on each fire."
     ""
     "## Guard"
     ""
     "`guard` is an optional zero-argument predicate evaluated at fire time. Non-nil fires the action; nil skips this fire. The guard decides *whether* to fire, never *when*: the schedule re-arms for its next time regardless. It runs synchronously and must be cheap and side-effect free."
     ""
     "## Catch-up"
     ""
     "`catch-up` is `skip` (default) or `run`. When Emacs was asleep across one or more fire times, `skip` moves to the next future fire; `run` fires once now before re-arming."
     ""
     "## Actions"
     ""
     "- `register-schedule`: input `(:id SYMBOL :when PLIST :action PLIST :guard SYMBOL :catch-up SYMBOL :metadata PLIST :enabled BOOLEAN)`. Registers (replacing an existing id) and arms an enabled schedule."
     "- `list-schedules`: returns each schedule with its `when`, next and last fire, last guard result, and enabled state."
     "- `schedule-status`: input `(:id SYMBOL)`. Returns one schedule descriptor."
     "- `enable-schedule` / `disable-schedule`: input `(:id SYMBOL)`. Arm or disarm without unregistering."
     "- `remove-schedule`: input `(:id SYMBOL)`. Stops and unregisters.")
   "\n")
  "Detailed Cron Schedule action reference.")

;; --- routing an action spec to an engine closure ----------------------------

(defun e-cron-actions--enqueue-action (spec)
  "Return an engine action that enqueues a task-queue prompt from SPEC."
  (let ((prompt (plist-get spec :prompt))
        (instance-id (plist-get spec :harness-instance-id))
        (metadata (plist-get spec :metadata)))
    (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
      (signal 'e-cron-actions-invalid-action (list :enqueue :prompt prompt)))
    (lambda (_schedule)
      (e-task-queue-enqueue
       (e-task-queue-actions-ensure-loaded)
       :prompt prompt
       :metadata metadata
       :harness-instance-id
       (e-task-queue-actions--instance-id instance-id)))))

(defun e-cron-actions--wake-action (trigger-id)
  "Return an engine action that fires background-session trigger TRIGGER-ID.
The trigger is resolved by id at fire time, so re-registering the trigger takes
effect without touching the schedule."
  (unless trigger-id
    (signal 'e-cron-actions-invalid-action (list :wake :trigger nil)))
  (lambda (_schedule)
    (if-let ((trigger (e-background-session-get trigger-id)))
        (e-background-session-fire trigger)
      (signal 'e-cron-actions-invalid-action
              (list :wake :unknown-trigger trigger-id)))))

(defun e-cron-actions--call-action (handler)
  "Return an engine action that calls HANDLER.
HANDLER is called with the schedule when it accepts one argument."
  (unless (functionp handler)
    (signal 'e-cron-actions-invalid-action (list :call handler)))
  (lambda (schedule)
    (condition-case nil
        (funcall handler schedule)
      (wrong-number-of-arguments (funcall handler)))))

(defun e-cron-actions--build-action (spec)
  "Return the engine action function for action SPEC.
SPEC is a plist naming one action kind: `(:enqueue (:prompt ...))',
`(:wake TRIGGER-ID)', or `(:call HANDLER)'.  A bare function is used directly."
  (cond
   ((null spec) (signal 'e-cron-actions-invalid-action (list nil)))
   ((functionp spec) (lambda (schedule) (funcall spec schedule)))
   ((plist-member spec :enqueue)
    (e-cron-actions--enqueue-action (plist-get spec :enqueue)))
   ((plist-member spec :wake)
    (e-cron-actions--wake-action (plist-get spec :wake)))
   ((plist-member spec :call)
    (e-cron-actions--call-action (plist-get spec :call)))
   (t (signal 'e-cron-actions-invalid-action (list spec)))))

(cl-defun e-cron-actions-register (&key id when action guard (catch-up 'skip)
                                        metadata (enabled t))
  "Register a routed schedule and return it.
ACTION is a declarative action spec routed through
`e-cron-actions--build-action'.  The remaining keys pass through to
`e-cron-register'.  The original ACTION spec is stored under METADATA's
`:action-spec' so the overview can show what a schedule fires."
  (e-cron-register
   :id id
   :when when
   :action (e-cron-actions--build-action action)
   :guard guard
   :catch-up catch-up
   :metadata (plist-put (copy-sequence metadata) :action-spec action)
   :enabled enabled))

;; --- capability action surface ----------------------------------------------

(defun e-cron-actions--symbol (value key)
  "Return VALUE as a symbol for action argument KEY, or signal.
Actions arrive as JSON, so an id or handler name reaches here as a string."
  (cond
   ((null value) nil)
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t (signal 'wrong-type-argument (list key value)))))

(defun e-cron-actions--required-id (arguments)
  "Return the required schedule id symbol from ARGUMENTS."
  (or (e-cron-actions--symbol (plist-get arguments :id) :id)
      (signal 'wrong-type-argument (list :id nil))))

(defun e-cron-actions--describe (schedule)
  "Return a normalized descriptor for SCHEDULE."
  (let ((next (e-cron-schedule-next-fire schedule))
        (last (e-cron-schedule-last-fire schedule))
        (guard-at (e-cron-schedule-last-guard-at schedule)))
    (list :id (e-cron-schedule-id schedule)
          :when (e-cron-schedule-when schedule)
          :action (plist-get (e-cron-schedule-metadata schedule) :action-spec)
          :catch-up (e-cron-schedule-catch-up schedule)
          :enabled (and (e-cron-schedule-enabled schedule) t)
          :has-guard (and (e-cron-schedule-guard schedule) t)
          :next-fire (and next (format-time-string "%FT%T%z" next))
          :last-fire (and last (format-time-string "%FT%T%z" last))
          :last-guard-result (e-cron-schedule-last-guard-result schedule)
          :last-guard-at (and guard-at (format-time-string "%FT%T%z" guard-at)))))

(defun e-cron-actions--register (arguments)
  "Register a schedule described by ARGUMENTS and return its descriptor."
  (e-cron-actions--describe
   (e-cron-actions-register
    :id (e-cron-actions--required-id arguments)
    :when (plist-get arguments :when)
    :action (plist-get arguments :action)
    :guard (let ((guard (plist-get arguments :guard)))
             (cond ((functionp guard) guard)
                   ((null guard) nil)
                   (t (e-cron-actions--symbol guard :guard))))
    :catch-up (or (e-cron-actions--symbol (plist-get arguments :catch-up)
                                          :catch-up)
                  'skip)
    :metadata (plist-get arguments :metadata)
    :enabled (if (plist-member arguments :enabled)
                 (and (plist-get arguments :enabled) t)
               t))))

(defun e-cron-actions--list (_arguments)
  "Return descriptors for every registered schedule."
  (mapcar #'e-cron-actions--describe (e-cron-list)))

(defun e-cron-actions--status (arguments)
  "Return the descriptor for one schedule named in ARGUMENTS."
  (let ((id (e-cron-actions--required-id arguments)))
    (e-cron-actions--describe
     (or (e-cron-get id) (signal 'e-cron-unknown-schedule (list id))))))

(defun e-cron-actions--enable (arguments)
  "Enable and arm the schedule named in ARGUMENTS."
  (e-cron-actions--describe (e-cron-enable (e-cron-actions--required-id arguments))))

(defun e-cron-actions--disable (arguments)
  "Disable the schedule named in ARGUMENTS."
  (e-cron-actions--describe (e-cron-disable (e-cron-actions--required-id arguments))))

(defun e-cron-actions--remove (arguments)
  "Stop and unregister the schedule named in ARGUMENTS."
  (let ((id (e-cron-actions--required-id arguments)))
    (e-cron-remove id)
    (list :id id :removed t)))

(defun e-cron-actions--action (handler parameters)
  "Return a cron cheap work action descriptor for HANDLER with PARAMETERS."
  (e-action-cheap-create
   :owner 'cron
   :parameters parameters
   :runner (lambda (arguments _context)
             (funcall handler arguments))))

(defconst e-cron-actions--register-parameters
  '(:type "object"
    :properties
    (:id
     (:type "string"
      :description "Stable schedule id.")
     :when
     (:type "object"
      :description "Recurrence: (:every SECONDS) or (:at \"HH:MM\" :on (mon ...)).")
     :action
     (:type "object"
      :description "Action spec: (:enqueue (:prompt ...)), (:wake TRIGGER-ID), or (:call SYMBOL).")
     :guard
     (:type "string"
      :description "Optional zero-argument predicate symbol gating each fire.")
     :catch-up
     (:type "string"
      :description "Missed-fire policy: skip (default) or run.")
     :metadata
     (:type "object"
      :description "Opaque plist passed through to the fired work.")
     :enabled
     (:type "boolean"
      :description "Arm immediately. Defaults to true."))
    :required ["id" "when" "action"])
  "Action parameters for schedule registration.")

(defconst e-cron-actions--id-parameters
  '(:type "object"
    :properties
    (:id
     (:type "string"
      :description "Schedule id."))
    :required ["id"])
  "Action parameters for schedule lookup operations.")

(defun e-cron-capability-create ()
  "Create the Cron Schedule capability."
  (e-capability-with-skills-create
   :id 'cron
   :name "Cron Schedule"
   :instruction-priority 255
   :instructions e-cron-actions-instructions
   :actions
   (list :register-schedule
         (e-cron-actions--action #'e-cron-actions--register
                                 e-cron-actions--register-parameters)
         :list-schedules
         (e-cron-actions--action #'e-cron-actions--list nil)
         :schedule-status
         (e-cron-actions--action #'e-cron-actions--status
                                 e-cron-actions--id-parameters)
         :enable-schedule
         (e-cron-actions--action #'e-cron-actions--enable
                                 e-cron-actions--id-parameters)
         :disable-schedule
         (e-cron-actions--action #'e-cron-actions--disable
                                 e-cron-actions--id-parameters)
         :remove-schedule
         (e-cron-actions--action #'e-cron-actions--remove
                                 e-cron-actions--id-parameters))
   :skills
   (list
    (e-skill-spec-create
     :name "cron"
     :description "Register and observe cron-like schedules that fire agent work."
     :content e-cron-actions-skill))))

(defun e-cron-layer-create ()
  "Create the Cron Schedule layer."
  (e-layer-create
   :id 'cron
   :name "Cron Schedule"
   :capabilities (list (e-cron-capability-create))))

(provide 'e-cron-actions)

;;; e-cron-actions.el ends here

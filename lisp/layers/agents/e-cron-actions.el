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
(require 'e-board-sqlite-service)
(require 'e-cron)
(require 'e-layers)
(require 'e-skills)

(define-error 'e-cron-actions-invalid-action "Invalid cron action spec")

(defconst e-cron-actions-instructions
  "Use Cron Schedule actions to publish descriptive facts to an explicitly configured SQLite Board target on a recurrence. Read e://cron/skills/cron for the action contract."
  "Compact Cron Schedule coordinator guidance.")

(defconst e-cron-actions-skill
  (string-join
   '("# Cron Schedule work actions"
     ""
     "Schedules publish board facts on a recurrence, built on Emacs timers -- no external cron. The engine owns timing only."
     ""
     "## Recurrence (`when`)"
     ""
     "- Interval: `(:every SECONDS)` fires every SECONDS."
     "- Calendar: `(:at \"HH:MM\" :on (mon tue wed thu fri))` fires at that local time on those weekdays. Weekday symbols are `sun mon tue wed thu fri sat`; omit `:on` for every day."
     ""
     "## Action (`action`)"
     ""
     "- `(:publish (:content STRING :tags LIST :attributes PLIST))`: publish one observation-only fact through the configured SQLite target."
     ""
     "## Catch-up"
     ""
     "`catch-up` is `skip` (default) or `run`. When Emacs was asleep across one or more fire times, `skip` moves to the next future fire; `run` fires once now before re-arming."
     ""
     "## Actions"
     ""
     "- `register-schedule`: input `(:id SYMBOL :when PLIST :action PLIST :catch-up SYMBOL :metadata PLIST :enabled BOOLEAN)`. Registers (replacing an existing id) and arms an enabled schedule."
     "- `list-schedules`: returns each schedule with its `when`, next and last fire, last guard result, and enabled state."
     "- `schedule-status`: input `(:id SYMBOL)`. Returns one schedule descriptor."
     "- `enable-schedule` / `disable-schedule`: input `(:id SYMBOL)`. Arm or disarm without unregistering."
     "- `remove-schedule`: input `(:id SYMBOL)`. Stops and unregisters.")
   "\n")
  "Detailed Cron Schedule action reference.")

;; --- routing an action spec to one SQL target -------------------------------

(defvar e-cron-actions-publication-target nil
  "Explicit SQL Board target used by cron action registration.")

(defun e-cron-actions-configure-publication-target (target)
  "Configure explicit SQL TARGET for later cron registrations."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (setq e-cron-actions-publication-target target))

(defun e-cron-actions--publish-action (target spec)
  "Return an engine action that publishes SPEC through SQL TARGET."
  (let ((content (plist-get spec :content))
        (tags (plist-get spec :tags))
        (attributes (plist-get spec :attributes)))
    (unless (and (stringp content) (not (string-empty-p (string-trim content))))
      (signal 'e-cron-actions-invalid-action (list :publish :content content)))
    (lambda (schedule)
      (e-board-sqlite-publication-target-fact-start
       target content
       (list 'cron (e-cron-schedule-id schedule)
             (float-time (e-cron-schedule-last-fire schedule)))
       :tags (append '(cron) (copy-tree tags t))
       :attributes (copy-tree attributes t)))))

(defun e-cron-actions--build-action (target spec)
  "Return the engine action function for action SPEC.
SPEC must be `(:publish (:content ... :tags ... :attributes ...))'."
  (cond
   ((null spec) (signal 'e-cron-actions-invalid-action (list nil)))
   ((not (listp spec))
    (signal 'e-cron-actions-invalid-action (list spec)))
   ((plist-member spec :publish)
    (e-cron-actions--publish-action target (plist-get spec :publish)))
   (t (signal 'e-cron-actions-invalid-action (list spec)))))

(cl-defun e-cron-actions-register (&key id when action publication-target
                                        (catch-up 'skip) metadata (enabled t))
  "Register a routed schedule and return it.
ACTION is a declarative action spec routed through
`e-cron-actions--build-action'.  The remaining keys pass through to
`e-cron-register'.  The original ACTION spec is stored under METADATA's
`:action-spec' so the overview can show what a schedule fires."
  (e-cron-register
   :id id
   :when when
   :action (e-cron-actions--build-action
            (let ((target (or publication-target
                              e-cron-actions-publication-target)))
              (unless (e-board-sqlite-publication-target-valid-p target)
                (signal 'wrong-type-argument
                        (list 'e-board-sqlite-publication-target-p target)))
              target)
            action)
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
      :description "Action spec: (:publish (:content STRING :tags LIST :attributes PLIST)).")
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

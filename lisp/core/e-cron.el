;;; e-cron.el --- Cron-like schedule engine for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A declarative, cron-like schedule engine built on Emacs timers.  A schedule
;; entry pairs a recurrence (`when') with an action to fire; the engine computes
;; the next fire time, arms a single Emacs timer per entry, and re-arms after
;; each fire.  It owns *timing only*: an action is a plain function the engine
;; calls, so this module never depends on the task queue or background sessions.
;; A higher layer builds the closures that route a fire to those primitives.
;;
;; Two knobs shape a fire:
;;
;;   - `guard': an optional deterministic predicate evaluated at fire time.  A
;;     non-nil result fires the action; nil skips this fire.  The guard decides
;;     *whether* to fire, never *when* the next fire lands: the schedule always
;;     recomputes and re-arms regardless of the guard result.  It runs
;;     synchronously in the live Emacs and must be cheap and side-effect free.
;;
;;   - `catch-up': `skip' (default) or `run'.  When wall time has advanced past
;;     one or more fire times -- an Emacs that was suspended, a laptop that
;;     slept -- `skip' moves straight to the next future fire, while `run' fires
;;     once now to make up for the missed occurrences before re-arming.
;;
;; Time is read through `e-cron-current-time-function' so tests drive next-fire
;; computation and fire routing against an injected clock instead of waiting on
;; real timers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup e-cron nil
  "Cron-like schedule engine for e."
  :group 'e
  :prefix "e-cron-")

(defvar e-cron-current-time-function #'current-time
  "Function returning the current time as an Emacs time value.
Rebind in tests to drive next-fire computation from an injected clock.")

(defvar e-cron-fire-functions nil
  "Abnormal hook run after a schedule entry fires its action.
Each function is called with the schedule.  Intended for observation (shells,
tests); handlers must not mutate the schedule.")

(define-error 'e-cron-unknown-schedule "Unknown schedule id")
(define-error 'e-cron-invalid-when "Invalid schedule recurrence")

(cl-defstruct (e-cron-schedule (:constructor e-cron-schedule--create))
  "A registered schedule entry.

Configuration fields are set at registration:
ID is a stable key.  WHEN is the recurrence plist -- either `(:every SECONDS)'
or `(:at \"HH:MM\" :on (DAY...))' where DAY is a weekday symbol (sun..sat) and an
empty or absent `:on' means every day.  ACTION is a one-argument function the
engine calls with the schedule when it fires.  GUARD is nil or a zero-argument
predicate gating each fire.  CATCH-UP is `skip' or `run'.  METADATA is opaque.

Runtime fields track the live timer and last observations:
TIMER is the armed Emacs timer.  NEXT-FIRE is the computed next fire time.
LAST-FIRE is when the action last ran.  LAST-GUARD-RESULT and LAST-GUARD-AT
record the most recent guard evaluation.  ENABLED gates arming."
  id
  when
  action
  guard
  (catch-up 'skip)
  metadata
  ;; runtime
  timer
  next-fire
  last-fire
  last-guard-result
  last-guard-at
  (enabled t))

(defvar e-cron--schedules (make-hash-table :test 'equal)
  "Registered schedules keyed by id.")

;; --- time helpers -----------------------------------------------------------

(defun e-cron--now ()
  "Return the current time through `e-cron-current-time-function'."
  (funcall e-cron-current-time-function))

(defconst e-cron--weekday-indices
  '((sun . 0) (mon . 1) (tue . 2) (wed . 3) (thu . 4) (fri . 5) (sat . 6))
  "Map weekday symbols to `decode-time' day-of-week indices (0 = Sunday).")

(defun e-cron--weekday-index (day)
  "Return the day-of-week index for weekday symbol DAY, or signal."
  (or (cdr (assq day e-cron--weekday-indices))
      (signal 'e-cron-invalid-when (list :on day))))

(defun e-cron--parse-hh-mm (string)
  "Parse a \"HH:MM\" STRING into a cons of hour and minute, or signal."
  (if (and (stringp string)
           (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'" string))
      (let ((hour (string-to-number (match-string 1 string)))
            (minute (string-to-number (match-string 2 string))))
        (unless (and (<= 0 hour 23) (<= 0 minute 59))
          (signal 'e-cron-invalid-when (list :at string)))
        (cons hour minute))
    (signal 'e-cron-invalid-when (list :at string))))

;; --- next-fire computation --------------------------------------------------

(defun e-cron--next-interval (seconds now)
  "Return the next fire time SECONDS after NOW."
  (unless (and (numberp seconds) (> seconds 0))
    (signal 'e-cron-invalid-when (list :every seconds)))
  (time-add now (seconds-to-time seconds)))

(defun e-cron--next-calendar (at on now)
  "Return the next fire time at AT (\"HH:MM\") on weekdays ON after NOW.
ON is a list of weekday symbols; empty means every day.  Scans forward day by
day so the returned time is the earliest AT strictly after NOW on an allowed
weekday.  Uses `encode-time' per candidate day so local-time rules (including
DST transitions) apply."
  (let* ((hm (e-cron--parse-hh-mm at))
         (hour (car hm))
         (minute (cdr hm))
         (allowed (mapcar #'e-cron--weekday-index on))
         (base (decode-time now))
         (mday (nth 3 base))
         (month (nth 4 base))
         (year (nth 5 base)))
    (cl-loop
     for offset from 0 to 7
     for candidate = (encode-time
                      (list 0 minute hour (+ mday offset) month year nil -1 nil))
     for weekday = (nth 6 (decode-time candidate))
     when (and (time-less-p now candidate)
               (or (null allowed) (memq weekday allowed)))
     return candidate
     finally (signal 'e-cron-invalid-when (list :on on)))))

(defun e-cron--next-fire (when now)
  "Return the next fire time for recurrence WHEN after NOW."
  (cond
   ((plist-member when :every)
    (e-cron--next-interval (plist-get when :every) now))
   ((plist-member when :at)
    (e-cron--next-calendar (plist-get when :at) (plist-get when :on) now))
   (t (signal 'e-cron-invalid-when (list when)))))

;; --- registry ---------------------------------------------------------------

(defun e-cron-get (id)
  "Return the registered schedule with ID, or nil."
  (gethash id e-cron--schedules))

(defun e-cron-list ()
  "Return all registered schedules, sorted by id."
  (sort (hash-table-values e-cron--schedules)
        (lambda (a b)
          (string< (format "%s" (e-cron-schedule-id a))
                   (format "%s" (e-cron-schedule-id b))))))

(cl-defun e-cron-register (&key id when action guard (catch-up 'skip)
                                metadata (enabled t))
  "Register a schedule entry and return it.
An existing schedule with the same ID is stopped and replaced.  See
`e-cron-schedule' for the field meanings.  Registration validates WHEN by
computing an initial next-fire; an enabled entry is armed immediately."
  (unless id (signal 'e-cron-invalid-when (list :id nil)))
  (unless (functionp action)
    (signal 'wrong-type-argument (list 'functionp :action)))
  (unless (memq catch-up '(skip run))
    (signal 'e-cron-invalid-when (list :catch-up catch-up)))
  (when-let ((existing (e-cron-get id)))
    (e-cron-stop existing))
  (let ((schedule (e-cron-schedule--create
                   :id id
                   :when when
                   :action action
                   :guard guard
                   :catch-up catch-up
                   :metadata metadata
                   :enabled enabled)))
    ;; Compute an initial next-fire so an invalid WHEN fails at registration
    ;; rather than at the first tick.
    (setf (e-cron-schedule-next-fire schedule)
          (e-cron--next-fire when (e-cron--now)))
    (puthash id schedule e-cron--schedules)
    (when enabled (e-cron-start schedule))
    schedule))

(defun e-cron-remove (id)
  "Stop and unregister the schedule with ID."
  (when-let ((schedule (e-cron-get id)))
    (e-cron-stop schedule)
    (remhash id e-cron--schedules)))

;; --- arming and firing ------------------------------------------------------

(defun e-cron--cancel-timer (schedule)
  "Cancel SCHEDULE's armed timer, if any."
  (when-let ((timer (e-cron-schedule-timer schedule)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (e-cron-schedule-timer schedule) nil))

(defun e-cron--arm (schedule)
  "Arm SCHEDULE's timer for its current next-fire.
The timer waits at least zero seconds; a next-fire already in the past (a
missed fire the caller has decided to run) fires on the next event-loop tick."
  (e-cron--cancel-timer schedule)
  (let* ((now (e-cron--now))
         (next (e-cron-schedule-next-fire schedule))
         (delay (max 0 (float-time (time-subtract next now))))
         (id (e-cron-schedule-id schedule)))
    (setf (e-cron-schedule-timer schedule)
          (run-at-time delay nil #'e-cron--on-timer id))))

(defun e-cron--on-timer (id)
  "Timer callback: fire the schedule with ID if it is still enabled.
Resolves the schedule by id so a replaced or removed entry does not fire a
stale closure."
  (when-let ((schedule (e-cron-get id)))
    (when (e-cron-schedule-enabled schedule)
      (e-cron-fire schedule)
      ;; Re-arm for the next occurrence unless firing disabled the entry.
      (when (e-cron-schedule-enabled schedule)
        (e-cron--advance schedule (e-cron--now))
        (e-cron--arm schedule)))))

(defun e-cron--advance (schedule now)
  "Set SCHEDULE's next-fire to the next occurrence strictly after NOW."
  (setf (e-cron-schedule-next-fire schedule)
        (e-cron--next-fire (e-cron-schedule-when schedule) now)))

(defun e-cron-fire (schedule)
  "Fire SCHEDULE now, honoring its guard, and return non-nil when it fired.
Evaluates the guard first and records the result; a nil guard skips the action
but still returns from a normal fire so the caller re-arms.  The action is
called with the schedule.  A `skip'-policy entry whose next-fire is already in
the past and whose guard passes still fires here -- the missed-fire decision
belongs to the caller (`e-cron-start'), not to a fire it has already chosen to
run."
  (let ((now (e-cron--now)))
    (setf (e-cron-schedule-last-guard-at schedule) now)
    (let ((guard (e-cron-schedule-guard schedule)))
      (if (and guard (not (setf (e-cron-schedule-last-guard-result schedule)
                                (funcall guard))))
          nil
        (when (null guard)
          (setf (e-cron-schedule-last-guard-result schedule) t))
        (setf (e-cron-schedule-last-fire schedule) now)
        (funcall (e-cron-schedule-action schedule) schedule)
        (run-hook-with-args 'e-cron-fire-functions schedule)
        t))))

(defun e-cron-start (schedule)
  "Enable SCHEDULE and arm its timer, applying the catch-up policy.
When the computed next-fire is already in the past -- Emacs was asleep across
one or more occurrences -- `skip' advances to the next future fire and arms,
while `run' fires once now before advancing.  Returns SCHEDULE."
  (setf (e-cron-schedule-enabled schedule) t)
  (let ((now (e-cron--now)))
    (when (or (null (e-cron-schedule-next-fire schedule))
              (not (time-less-p now (e-cron-schedule-next-fire schedule))))
      (when (eq (e-cron-schedule-catch-up schedule) 'run)
        (e-cron-fire schedule))
      (e-cron--advance schedule now)))
  (e-cron--arm schedule)
  schedule)

(defun e-cron-stop (schedule)
  "Cancel SCHEDULE's timer without unregistering it.  Returns SCHEDULE."
  (e-cron--cancel-timer schedule)
  schedule)

(defun e-cron-enable (id)
  "Enable and arm the schedule with ID.  Returns the schedule or signals."
  (e-cron-start (or (e-cron-get id) (signal 'e-cron-unknown-schedule (list id)))))

(defun e-cron-disable (id)
  "Disable the schedule with ID, cancelling its timer.  Returns the schedule."
  (let ((schedule (or (e-cron-get id) (signal 'e-cron-unknown-schedule (list id)))))
    (setf (e-cron-schedule-enabled schedule) nil)
    (e-cron-stop schedule)))

(provide 'e-cron)

;;; e-cron.el ends here

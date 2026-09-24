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
;; Definitions vs. state.  A schedule *definition* (id, recurrence, action) is
;; declarative configuration: the owning layer or init re-registers it on every
;; load.  When the ordinary runtime is active, its SQLite cron port owns the
;; durable anchor and last-fire state.  Explicitly in-memory schedules retain
;; only a process-local compatibility cache.  Registration is therefore
;; idempotent in timing: the next fire is anchored on the restored last-fire (or
;; first-registration anchor), never on the moment of re-registration, so
;; reloading a layer does not push an interval schedule's fire out.  The due
;; time is last-fire plus the recurrence's interval; the interval is derived
;; once from the definition and cached on the schedule.
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
(require 'e-cron-storage)
(require 'e-work)

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

(defvar e-cron-storage nil
  "Optional process-wide cron-owned durable storage port.")

(defun e-cron-configure-storage (storage)
  "Install cron STORAGE, or nil while no runtime is active."
  (unless (or (null storage) (e-cron-storage-p storage))
    (signal 'wrong-type-argument (list 'e-cron-storage-p storage)))
  (setq e-cron-storage storage))

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

INTERVAL is the fixed inter-fire span in seconds derived once from WHEN, or
nil for a calendar recurrence with no constant interval; it anchors the due
time and avoids recomputing the span on every tick.

Runtime fields track the live timer and last observations:
TIMER is the armed Emacs timer.  NEXT-FIRE is the computed next fire time.
LAST-FIRE is when the action last ran (hydrated from persisted state at
registration).  LAST-GUARD-RESULT and LAST-GUARD-AT record the most recent
guard evaluation.  ENABLED gates arming."
  id
  when
  action
  guard
  (catch-up 'skip)
  metadata
  storage
  storage-ready-p
  revision
  definition-revision
  anchor
  unresolved-firings
  interval
  ;; runtime
  timer
  next-fire
  last-fire
  last-guard-result
  last-guard-at
  (enabled t))

(defvar e-cron--schedules (make-hash-table :test 'equal)
  "Registered schedules keyed by id.")

(defvar e-cron--state (make-hash-table :test 'equal)
  "Process-local cron state for explicitly in-memory schedules.
Each plist holds `:last-fire' and `:anchor' as `float-time' values.  The
ordinary runtime instead restores these values through its SQLite port.")

(defvar e-cron--state-loaded nil
  "Non-nil once the process-local cron cache has been initialized.")

(defvar e-cron--active-firings (make-hash-table :test 'equal)
  "Process-local durable firing identities currently executing an action.")

(defun e-cron--active-firing-key (storage id firing-id)
  "Return process-local key for STORAGE, schedule ID, and FIRING-ID."
  (format "%x:%s:%s"
          (sxhash-eq (e-cron-storage-runtime storage)) id firing-id))

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

(defun e-cron--derive-interval (when)
  "Return the fixed inter-fire span in seconds for WHEN, or nil.
An `:every' recurrence has a constant interval.  A daily `:at' recurrence with
no `:on' restriction repeats every 86400 s, so it too has a constant span; an
`:at' with weekday restrictions has no single span and returns nil (its due
time is computed by calendar scan instead).  Validates WHEN as a side effect."
  (cond
   ((plist-member when :every)
    (let ((seconds (plist-get when :every)))
      (unless (and (numberp seconds) (> seconds 0))
        (signal 'e-cron-invalid-when (list :every seconds)))
      seconds))
   ((plist-member when :at)
    ;; Parse to validate; a per-day recurrence has a 24h span, a
    ;; weekday-restricted one does not.
    (e-cron--parse-hh-mm (plist-get when :at))
    (and (null (plist-get when :on)) 86400))
   (t (signal 'e-cron-invalid-when (list when)))))

(defun e-cron--basis (schedule)
  "Return the float-time basis for SCHEDULE's due computation.
The basis is the last-fire when known, else the persisted first-registration
anchor -- so a never-fired schedule's first fire is anchored to when it was
first registered rather than to the moment of the current re-registration."
  (let ((last (e-cron-schedule-last-fire schedule)))
    (if last
        (float-time last)
      (or (e-cron-schedule-anchor schedule)
          (e-cron--state-ensure-anchor schedule)))))

(defun e-cron--due-time (schedule)
  "Return the time SCHEDULE is next due, possibly in the past.
For a fixed-INTERVAL recurrence the due time is basis-plus-interval: the
schedule is due one interval after the basis (last-fire, or the persisted
first-registration anchor when it has never fired).  A due time in the past
means a fire came due while Emacs was down or between reloads; `e-cron-start'
detects that and applies the catch-up policy.  A weekday calendar recurrence
has no constant span, so its due time is simply the next calendar fire."
  (let ((interval (e-cron-schedule-interval schedule)))
    (if interval
        (seconds-to-time (+ (e-cron--basis schedule) interval))
      (e-cron--next-fire (e-cron-schedule-when schedule) (e-cron--now)))))

(defun e-cron--next-after (schedule now)
  "Return SCHEDULE's next fire time strictly after NOW.
For a fixed-INTERVAL recurrence this is the earliest basis-plus-k*interval
strictly after NOW, preserving the schedule's phase across missed fires and
re-registrations instead of resetting it to now.  A weekday calendar
recurrence has no constant span and defers to the calendar scan."
  (let ((interval (e-cron-schedule-interval schedule)))
    (if interval
        (let* ((basis (e-cron--basis schedule))
               (elapsed (- (float-time now) basis))
               (steps (if (< elapsed 0) 0 (1+ (floor elapsed interval)))))
          (seconds-to-time (+ basis (* interval steps))))
      (e-cron--next-fire (e-cron-schedule-when schedule) now))))

;; --- runtime state persistence ----------------------------------------------

(defun e-cron--state-load ()
  "Return the process-local cadence cache when no SQLite port is active."
  (unless e-cron--state-loaded
    (setq e-cron--state-loaded t))
  e-cron--state)

(defun e-cron--state-write ()
  "Retain the process-local compatibility cache without disk persistence."
  nil)

(defun e-cron--state-get (id)
  "Return the persisted state plist for ID, or nil."
  (gethash (format "%s" id) (e-cron--state-load)))

(defun e-cron--state-record-fire (schedule fire-time)
  "Record SCHEDULE's FIRE-TIME in the persisted state and flush to disk."
  (let* ((key (format "%s" (e-cron-schedule-id schedule)))
         (existing (gethash key e-cron--state))
         (anchor (or (plist-get existing :anchor)
                     (float-time fire-time))))
    (puthash key (list :last-fire (float-time fire-time) :anchor anchor)
             e-cron--state))
  (e-cron--state-write))

(defun e-cron--state-ensure-anchor (schedule)
  "Ensure SCHEDULE has a persisted first-registration anchor, returning it.
The anchor is the first time the schedule was ever seen; it seeds the due-time
computation for a schedule that has never fired so re-registration does not
keep pushing the first fire forward."
  (let* ((key (format "%s" (e-cron-schedule-id schedule)))
         (existing (gethash key (e-cron--state-load))))
    (or (plist-get existing :anchor)
        (let ((anchor (float-time (e-cron--now))))
          (puthash key (plist-put (copy-sequence existing) :anchor anchor)
                   e-cron--state)
          (e-cron--state-write)
          anchor))))

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

(defun e-cron--submit-settlement
    (storage id firing-id expected-state state result &optional on-settle)
  "Enqueue one durable firing settlement and surface only its own failure."
  (e-cron-storage-submit
   storage 'write 'settle
   (list id firing-id expected-state state result)
   (lambda (settled error)
     (when error
       (message "Cron settlement failed for %s/%s: %s"
                id firing-id (error-message-string error)))
     (when on-settle
       (funcall on-settle settled error)))))

(defun e-cron--finish-registration (schedule durable error)
  "Apply detached DURABLE registration state to live SCHEDULE."
  (when (eq schedule (e-cron-get (e-cron-schedule-id schedule)))
    (if error
        (progn
          (setf (e-cron-schedule-enabled schedule) nil)
          (message "Cron registration failed for %s: %s"
                   (e-cron-schedule-id schedule)
                   (error-message-string error)))
      (setf (e-cron-schedule-revision schedule) (plist-get durable :revision)
            (e-cron-schedule-definition-revision schedule)
            (plist-get durable :definition-revision)
            (e-cron-schedule-anchor schedule) (plist-get durable :anchor)
            (e-cron-schedule-last-fire schedule)
            (when-let* ((last (plist-get durable :last-fire)))
              (seconds-to-time last)))
      (let ((storage (e-cron-schedule-storage schedule))
            (id (e-cron-schedule-id schedule)))
        (e-cron-storage-submit
         storage 'read 'cadence (list id)
         (lambda (cadence cadence-error)
           (when (eq schedule (e-cron-get id))
             (if cadence-error
                 (progn
                   (setf (e-cron-schedule-enabled schedule) nil)
                   (message "Cron cadence query failed for %s: %s"
                            id (error-message-string cadence-error)))
               (let ((unresolved (plist-get cadence :unresolved)))
                 (setf (e-cron-schedule-unresolved-firings schedule)
                       (copy-tree unresolved t))
                 (dolist (firing unresolved)
                   (when (and (eq (plist-get firing :state) 'claimed)
                              (not (gethash
                                    (e-cron--active-firing-key
                                     storage id (plist-get firing :firing-id))
                                    e-cron--active-firings)))
                     (e-cron--submit-settlement
                      storage id (plist-get firing :firing-id)
                      'claimed 'unsafe
                      '(:reason interrupted-action-unknown))))
                 (setf (e-cron-schedule-storage-ready-p schedule) t)
                 (when (e-cron-schedule-enabled schedule)
                   (e-cron-start schedule)))))))))))

(cl-defun e-cron-register (&key id when action guard (catch-up 'skip)
                                metadata (enabled t)
                                (storage e-cron-storage))
  "Register a schedule entry and return it.
An existing schedule with the same ID is stopped and replaced.  See
`e-cron-schedule' for the field meanings.  Registration validates WHEN (via
the cached interval), hydrates LAST-FIRE from persisted state, and anchors the
next fire on that persisted history rather than on registration time -- so
re-registering an unchanged definition does not shift its cadence.  An enabled
entry is armed immediately."
  (unless id (signal 'e-cron-invalid-when (list :id nil)))
  (unless (functionp action)
    (signal 'wrong-type-argument (list 'functionp :action)))
  (unless (memq catch-up '(skip run))
    (signal 'e-cron-invalid-when (list :catch-up catch-up)))
  (when-let* ((existing (e-cron-get id)))
    (e-cron-stop existing))
  (let* (;; Derive the fixed interval once; also validates WHEN so an invalid
         ;; recurrence fails at registration rather than at the first tick.
         (interval (e-cron--derive-interval when))
         (persisted (and (not storage) (e-cron--state-get id)))
         (last (when-let* ((secs (plist-get persisted :last-fire)))
                 (seconds-to-time secs)))
         (anchor (if storage
                     (float-time (e-cron--now))
                   (plist-get persisted :anchor)))
         (schedule (e-cron-schedule--create
                    :id id
                    :when when
                    :action action
                    :guard guard
                    :catch-up catch-up
                    :metadata metadata
                    :storage storage
                    :storage-ready-p (not storage)
                    :anchor anchor
                    :interval interval
                    :last-fire last
                    :enabled enabled)))
    (puthash id schedule e-cron--schedules)
    (if storage
        (e-cron-storage-submit
         storage 'write 'register
         (list id (list :when when :catch-up catch-up) anchor)
         (lambda (durable error)
           (e-cron--finish-registration schedule durable error)))
      (setf (e-cron-schedule-next-fire schedule) (e-cron--due-time schedule))
      (when enabled (e-cron-start schedule)))
    schedule))

(defun e-cron-remove (id)
  "Stop and unregister the schedule with ID."
  (when-let* ((schedule (e-cron-get id)))
    (e-cron-stop schedule)
    (remhash id e-cron--schedules)))

;; --- arming and firing ------------------------------------------------------

(defun e-cron--cancel-timer (schedule)
  "Cancel SCHEDULE's armed timer, if any."
  (when-let* ((timer (e-cron-schedule-timer schedule)))
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
  (when-let* ((schedule (e-cron-get id)))
    (when (e-cron-schedule-enabled schedule)
      (e-cron-fire schedule)
      ;; Re-arm for the next occurrence unless firing disabled the entry.
      (when (and (e-cron-schedule-enabled schedule)
                 ;; An action may replace or remove its own definition.  The
                 ;; callback that began under the old definition must never
                 ;; re-arm that retired schedule after returning.
                 (eq schedule (e-cron-get id)))
        (unless (e-cron-schedule-storage schedule)
          (e-cron--advance schedule (e-cron--now))
          (e-cron--arm schedule))))))

(defun e-cron--advance (schedule now)
  "Set SCHEDULE's next-fire to the next occurrence strictly after NOW."
  (setf (e-cron-schedule-next-fire schedule)
        (e-cron--next-after schedule now)))

(defun e-cron--run-local-fire (schedule now)
  "Run one in-memory SCHEDULE firing at NOW."
  (setf (e-cron-schedule-last-guard-at schedule) now)
  (let ((guard (e-cron-schedule-guard schedule)))
    (if (and guard
             (not (setf (e-cron-schedule-last-guard-result schedule)
                        (funcall guard))))
        nil
      (when (null guard)
        (setf (e-cron-schedule-last-guard-result schedule) t))
      (setf (e-cron-schedule-last-fire schedule) now)
      (e-cron--state-record-fire schedule now)
      (funcall (e-cron-schedule-action schedule) schedule)
      (run-hook-with-args 'e-cron-fire-functions schedule)
      t)))

(defun e-cron--settle-action-work
    (schedule storage id firing-id active-key work)
  "Settle durable FIRING-ID only after asynchronous action WORK settles."
  (e-work-on-settle
   work
   (lambda (settled)
     (let* ((status (e-work-status settled))
            (state (plist-get status :state))
            (terminal-state (if (eq state 'finished) 'done 'failed))
            (result
             (if (eq state 'finished)
                 '(:completed t)
               (let ((error
                      (if (eq state 'failed)
                          (e-work-error-message (plist-get status :error))
                        "Cron Board publication was cancelled")))
                 (message "Cron action failed for %s: %s" id error)
                 (list :error error)))))
       (e-cron--submit-settlement
        storage id firing-id 'claimed terminal-state result
        (lambda (_durable _error)
          (remhash active-key e-cron--active-firings)))
       (when (eq state 'finished)
         (run-hook-with-args 'e-cron-fire-functions schedule))))))

(defun e-cron--finish-claim
    (schedule owner-enabled owner-definition-revision firing-id now next
              claim error)
  "Continue SCHEDULE only after its durable CLAIM settles."
  (let ((storage (e-cron-schedule-storage schedule))
        (id (e-cron-schedule-id schedule)))
    (if error
        (message "Cron claim failed for %s/%s: %s"
                 id firing-id (error-message-string error))
      (let ((current-p
             (and (eq schedule (e-cron-get id))
                  (eq owner-enabled (e-cron-schedule-enabled schedule))
                  (equal owner-definition-revision
                         (e-cron-schedule-definition-revision schedule))
                  (equal owner-definition-revision
                         (plist-get claim :definition-revision))
                  (equal firing-id (plist-get claim :firing-id)))))
        (setf (e-cron-schedule-revision schedule) (plist-get claim :revision)
              (e-cron-schedule-last-fire schedule) now
              (e-cron-schedule-next-fire schedule) next)
        (if (not current-p)
            (e-cron--submit-settlement
             storage id firing-id 'claimed 'skipped
             '(:reason live-definition-replaced-before-start))
          (setf (e-cron-schedule-last-guard-at schedule) now)
          (let ((guard (e-cron-schedule-guard schedule)))
            (if (and guard
                     (not (setf (e-cron-schedule-last-guard-result schedule)
                                (funcall guard))))
                (e-cron--submit-settlement
                 storage id firing-id 'claimed 'skipped '(:guard nil))
              (when (null guard)
                (setf (e-cron-schedule-last-guard-result schedule) t))
              (let ((active-key
                     (e-cron--active-firing-key storage id firing-id)))
                (puthash active-key t e-cron--active-firings)
                (condition-case action-error
                    (let ((action-result
                           (funcall (e-cron-schedule-action schedule)
                                    schedule)))
                      (if (e-work-handle-p action-result)
                          (e-cron--settle-action-work
                           schedule storage id firing-id active-key
                           action-result)
                        (e-cron--submit-settlement
                         storage id firing-id 'claimed 'done
                         '(:completed t)
                         (lambda (_durable _error)
                           (remhash active-key e-cron--active-firings)))
                        (run-hook-with-args
                         'e-cron-fire-functions schedule)))
                  (error
                   (e-cron--submit-settlement
                    storage id firing-id 'claimed 'failed
                    (list :error (error-message-string action-error))
                    (lambda (_durable _error)
                      (remhash active-key e-cron--active-firings)))
                   (message "Cron action failed for %s: %s"
                            id (error-message-string action-error))))))))
        (when (and (eq schedule (e-cron-get id))
                   (e-cron-schedule-enabled schedule))
          (e-cron--arm schedule))))))

(defun e-cron-fire (schedule)
  "Fire SCHEDULE now, honoring its guard, and return non-nil when it fired.
Evaluates the guard first and records the result; a nil guard skips the action
but still returns from a normal fire so the caller re-arms.  A fire records and
persists LAST-FIRE, so the schedule's cadence and due time survive restarts.
The action is called with the schedule.  A `skip'-policy entry whose next-fire
is already in the past and whose guard passes still fires here -- the
missed-fire decision belongs to the caller (`e-cron-start'), not to a fire it
has already chosen to run."
  (let* ((now (e-cron--now))
         (storage (e-cron-schedule-storage schedule))
         (owner-enabled (e-cron-schedule-enabled schedule))
         (owner-definition-revision
          (e-cron-schedule-definition-revision schedule))
         (due (or (e-cron-schedule-next-fire schedule) now))
         (next (e-cron--next-after schedule now))
         (firing-id
          (and storage
               (format "%s:%d:%s"
                       (e-cron-schedule-id schedule)
                       (or (e-cron-schedule-definition-revision schedule) 1)
                       (format "%.6f" (float-time due)))))
         )
    (if (not storage)
        (e-cron--run-local-fire schedule now)
      (when (e-cron-schedule-storage-ready-p schedule)
        (e-cron-storage-submit
         storage 'write 'claim
         (list (e-cron-schedule-id schedule) firing-id
               (float-time due) (float-time now) (float-time next))
         (lambda (claim error)
           (e-cron--finish-claim
            schedule owner-enabled owner-definition-revision firing-id
            now next claim error)))))))

(defun e-cron--skip-due-firing (schedule due now)
  "Durably skip SCHEDULE's missed DUE firing at NOW."
  (let* ((storage (e-cron-schedule-storage schedule))
         (next (e-cron--next-after schedule now))
         (firing-id
          (format "%s:%d:%s"
                  (e-cron-schedule-id schedule)
                  (or (e-cron-schedule-definition-revision schedule) 1)
                  (format "%.6f" (float-time due))))
         (id (e-cron-schedule-id schedule)))
    (setf (e-cron-schedule-last-fire schedule) now
          (e-cron-schedule-next-fire schedule) next)
    (e-cron-storage-submit
     storage 'write 'claim
     (list id firing-id (float-time due) (float-time now) (float-time next))
     (lambda (claim error)
       (if error
           (message "Cron skip claim failed for %s: %s"
                    id (error-message-string error))
         (setf (e-cron-schedule-revision schedule)
               (plist-get claim :revision))
         (e-cron--submit-settlement
          storage id firing-id 'claimed 'skipped
          '(:reason catch-up-policy)))))))

(defun e-cron-start (schedule)
  "Enable SCHEDULE and arm its timer, applying the catch-up policy.
The due time is derived from persisted state (last-fire plus interval, or the
first-registration anchor), so a fire that came due while Emacs was down --
or while a layer was between reloads -- is detected here.  When the due time
has passed, `skip' advances to the next future occurrence and arms, while
`run' fires once now before advancing.  Returns SCHEDULE."
  (setf (e-cron-schedule-enabled schedule) t)
  (when (or (not (e-cron-schedule-storage schedule))
            (e-cron-schedule-storage-ready-p schedule))
    (let ((now (e-cron--now))
          (due (e-cron--due-time schedule))
          fired-p)
      (setf (e-cron-schedule-next-fire schedule) due)
      (unless (time-less-p now due)
        (if (eq (e-cron-schedule-catch-up schedule) 'run)
            (progn
              (setq fired-p (e-cron-fire schedule))
              (unless (e-cron-schedule-storage schedule)
                (e-cron--advance schedule now)))
          (if (e-cron-schedule-storage schedule)
              (e-cron--skip-due-firing schedule due now)
            (e-cron--advance schedule now))))
      ;; Durable firing re-arms only after its claim callback.  Every other
      ;; path has a stable next occurrence already.
      (unless (and fired-p (e-cron-schedule-storage schedule))
        (e-cron--arm schedule))))
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

(defun e-cron-delete-history (id)
  "Explicitly delete durable firing history for schedule ID."
  (let ((schedule (or (e-cron-get id)
                      (signal 'e-cron-unknown-schedule (list id)))))
    (unless (e-cron-schedule-storage schedule)
      (signal 'e-cron-storage-error
              (list "Cron history deletion requires SQLite storage")))
    (e-cron-storage-submit
     (e-cron-schedule-storage schedule) 'write 'delete-history (list id)
     (lambda (_result error)
       (when error
         (message "Cron history deletion failed for %s: %s"
                  id (error-message-string error)))))))

(provide 'e-cron)

;;; e-cron.el ends here

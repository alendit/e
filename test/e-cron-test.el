;;; e-cron-test.el --- Tests for the cron schedule engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for `e-cron'.  An injected clock drives next-fire computation and
;; fire routing deterministically: `e-cron-current-time-function' returns a
;; mutable time value the test advances, so schedules are exercised without
;; waiting on real Emacs timers.  Timers are stubbed to fire synchronously so a
;; test can assert re-arm behavior without the event loop.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-cron)

(defmacro e-cron-test--with-registry (&rest body)
  "Run BODY with an isolated schedule registry and no armed timers.
Timers are stubbed to no-ops so registration and start do not touch the real
event loop; tests drive firing directly.  Persisted state is isolated to a
fresh in-memory table with persistence to disk disabled, so tests never touch
`e-cron-state-file'."
  (declare (indent 0) (debug t))
  `(let ((e-cron--schedules (make-hash-table :test 'equal))
         (e-cron--state (make-hash-table :test 'equal))
         (e-cron--state-loaded t)
         (e-cron-state-file nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (&rest _) 'stub-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'timerp)
                (lambda (value) (eq value 'stub-timer))))
       ,@body)))

(defun e-cron-test--time (string)
  "Return the Emacs time for local STRING in \"YYYY-MM-DD HH:MM:SS\" form."
  (encode-time (parse-time-string string)))

(defmacro e-cron-test--with-clock (start &rest body)
  "Bind the cron clock to a mutable value initialized to time STRING START.
`e-cron-test--set' rebinds it inside BODY."
  (declare (indent 1) (debug t))
  `(let* ((e-cron-test--clock (e-cron-test--time ,start))
          (e-cron-current-time-function (lambda () e-cron-test--clock)))
     (cl-flet ((e-cron-test--set (string)
                 (setq e-cron-test--clock (e-cron-test--time string))))
       (ignore #'e-cron-test--set)
       ,@body)))

;; --- next-fire computation --------------------------------------------------

(ert-deftest e-cron-test-next-interval ()
  "An interval recurrence fires SECONDS after now."
  (let* ((now (e-cron-test--time "2026-06-15 09:00:00"))
         (next (e-cron--next-fire '(:every 1800) now)))
    (should (= 1800 (round (float-time (time-subtract next now)))))))

(ert-deftest e-cron-test-next-calendar-same-day ()
  "A calendar recurrence fires later the same allowed day."
  ;; 2026-06-15 is a Monday.
  (let* ((now (e-cron-test--time "2026-06-15 08:00:00"))
         (next (e-cron--next-fire '(:at "09:00" :on (mon tue wed thu fri)) now)))
    (should (string= "2026-06-15 09:00:00"
                     (format-time-string "%F %T" next)))))

(ert-deftest e-cron-test-next-calendar-skips-to-allowed-weekday ()
  "A calendar recurrence past today's time jumps to the next allowed weekday."
  ;; Friday 2026-06-19 after 09:00 -> next weekday fire is Monday 2026-06-22.
  (let* ((now (e-cron-test--time "2026-06-19 10:00:00"))
         (next (e-cron--next-fire '(:at "09:00" :on (mon tue wed thu fri)) now)))
    (should (string= "2026-06-22 09:00:00"
                     (format-time-string "%F %T" next)))))

(ert-deftest e-cron-test-next-calendar-every-day ()
  "An absent :on fires the next day when today's time has passed."
  (let* ((now (e-cron-test--time "2026-06-15 10:00:00"))
         (next (e-cron--next-fire '(:at "09:00") now)))
    (should (string= "2026-06-16 09:00:00"
                     (format-time-string "%F %T" next)))))

(ert-deftest e-cron-test-invalid-when-signals ()
  "An unrecognized recurrence signals at computation time."
  (should-error (e-cron--next-fire '(:bogus 1) (current-time))
                :type 'e-cron-invalid-when)
  (should-error (e-cron--next-fire '(:at "99:99") (current-time))
                :type 'e-cron-invalid-when))

;; --- firing and guard -------------------------------------------------------

(ert-deftest e-cron-test-fire-runs-action ()
  "A fire with no guard runs the action and records last-fire."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let* ((fired 0)
             (schedule (e-cron-register
                        :id 'run :when '(:every 60)
                        :action (lambda (_s) (cl-incf fired)))))
        (should (e-cron-fire schedule))
        (should (= 1 fired))
        (should (e-cron-schedule-last-fire schedule))
        (should (eq t (e-cron-schedule-last-guard-result schedule)))))))

(ert-deftest e-cron-test-guard-gates-fire ()
  "A non-nil guard fires; a nil guard skips without running the action."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let* ((fired 0)
             (allow t)
             (schedule (e-cron-register
                        :id 'guarded :when '(:every 60)
                        :guard (lambda () allow)
                        :action (lambda (_s) (cl-incf fired)))))
        (should (e-cron-fire schedule))
        (should (= 1 fired))
        (setq allow nil)
        (should-not (e-cron-fire schedule))
        (should (= 1 fired))
        (should-not (e-cron-schedule-last-guard-result schedule))))))

(ert-deftest e-cron-test-guard-skip-still-rearms ()
  "A skipped guard does not stop the schedule from re-arming for its next time."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let* ((schedule (e-cron-register
                        :id 'rearm :when '(:every 60)
                        :guard (lambda () nil)
                        :action #'ignore))
             (before (e-cron-schedule-next-fire schedule)))
        ;; Advance the clock and drive the timer callback directly.
        (setq e-cron-test--clock (e-cron-test--time "2026-06-15 09:01:00"))
        (e-cron--on-timer 'rearm)
        (let ((after (e-cron-schedule-next-fire schedule)))
          (should (time-less-p before after))
          (should (e-cron-schedule-timer schedule)))))))

;; --- catch-up ---------------------------------------------------------------

(ert-deftest e-cron-test-catch-up-skip ()
  "Skip policy advances past a missed fire without firing."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let* ((fired 0)
             (schedule (e-cron-register
                        :id 'skip :when '(:every 60) :catch-up 'skip
                        :action (lambda (_s) (cl-incf fired)))))
        ;; Jump the clock well past the first next-fire, then re-start.
        (setq e-cron-test--clock (e-cron-test--time "2026-06-15 09:05:00"))
        (e-cron-start schedule)
        (should (= 0 fired))
        (should (time-less-p e-cron-test--clock
                             (e-cron-schedule-next-fire schedule)))))))

(ert-deftest e-cron-test-catch-up-run ()
  "Run policy fires once for a missed fire before re-arming to the future."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let* ((fired 0)
             (schedule (e-cron-register
                        :id 'run-catchup :when '(:every 60) :catch-up 'run
                        :action (lambda (_s) (cl-incf fired)))))
        (setq e-cron-test--clock (e-cron-test--time "2026-06-15 09:05:00"))
        (e-cron-start schedule)
        (should (= 1 fired))
        (should (time-less-p e-cron-test--clock
                             (e-cron-schedule-next-fire schedule)))))))

;; --- enable / disable / remove ----------------------------------------------

(ert-deftest e-cron-test-disable-cancels-timer ()
  "Disabling clears the armed timer and the enabled flag."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let ((schedule (e-cron-register :id 'toggle :when '(:every 60)
                                       :action #'ignore)))
        (should (e-cron-schedule-timer schedule))
        (e-cron-disable 'toggle)
        (should-not (e-cron-schedule-enabled schedule))
        (should-not (e-cron-schedule-timer schedule))
        (e-cron-enable 'toggle)
        (should (e-cron-schedule-enabled schedule))
        (should (e-cron-schedule-timer schedule))))))

(ert-deftest e-cron-test-disabled-timer-does-not-fire ()
  "A disabled schedule whose timer callback runs does not fire the action."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let* ((fired 0)
             (_ (e-cron-register :id 'off :when '(:every 60) :enabled nil
                                 :action (lambda (_s) (cl-incf fired)))))
        (e-cron--on-timer 'off)
        (should (= 0 fired))))))

(ert-deftest e-cron-test-remove-unregisters ()
  "Removing a schedule drops it from the registry."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (e-cron-register :id 'gone :when '(:every 60) :action #'ignore)
      (should (e-cron-get 'gone))
      (e-cron-remove 'gone)
      (should-not (e-cron-get 'gone)))))

;; --- persisted state / reload idempotence -----------------------------------

(ert-deftest e-cron-test-interval-cached ()
  "The fixed interval is derived once and cached on the schedule."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (should (= 60 (e-cron-schedule-interval
                     (e-cron-register :id 'i :when '(:every 60)
                                      :action #'ignore))))
      ;; A daily calendar recurrence has a constant 24h span.
      (should (= 86400 (e-cron-schedule-interval
                        (e-cron-register :id 'd :when '(:at "09:00")
                                         :action #'ignore))))
      ;; A weekday-restricted calendar recurrence has no single span.
      (should-not (e-cron-schedule-interval
                   (e-cron-register :id 'w :when '(:at "09:00" :on (mon))
                                    :action #'ignore))))))

(ert-deftest e-cron-test-fire-persists-last-fire ()
  "Firing records last-fire in the shared state table."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let ((schedule (e-cron-register :id 'p :when '(:every 60)
                                       :action #'ignore :enabled nil)))
        (e-cron-fire schedule)
        (should (= (float-time (e-cron-test--time "2026-06-15 09:00:00"))
                   (plist-get (e-cron--state-get 'p) :last-fire)))))))

(ert-deftest e-cron-test-reregister-preserves-cadence ()
  "Re-registering an unchanged definition does not push the next fire out.
The next fire is anchored on the persisted last-fire plus the interval, so a
reload between fires keeps the same due time instead of resetting to now."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      ;; First registration + fire at 09:00 -> next due 10:00 (hourly).
      (let ((schedule (e-cron-register :id 'h :when '(:every 3600)
                                       :action #'ignore :enabled nil)))
        (e-cron-fire schedule))
      ;; A reload 20 minutes later must keep the 10:00 due time, not 10:20.
      (setq e-cron-test--clock (e-cron-test--time "2026-06-15 09:20:00"))
      (let ((schedule (e-cron-register :id 'h :when '(:every 3600)
                                       :action #'ignore)))
        (should (string= "2026-06-15 10:00:00"
                         (format-time-string
                          "%F %T" (e-cron-schedule-next-fire schedule))))))))

(ert-deftest e-cron-test-due-while-down-fires-on-start ()
  "A fire that came due while Emacs was down runs once on the next start.
The due time is last-fire plus interval; when that has passed, `run' catch-up
fires once and re-arms to the next future occurrence."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      (let ((schedule (e-cron-register :id 'due :when '(:every 3600)
                                       :action #'ignore :enabled nil)))
        (e-cron-fire schedule))
      ;; "Restart" three hours later: due time (10:00) is well past.
      (setq e-cron-test--clock (e-cron-test--time "2026-06-15 12:30:00"))
      (let* ((fired 0)
             (schedule (e-cron-register
                        :id 'due :when '(:every 3600) :catch-up 'run
                        :action (lambda (_s) (cl-incf fired)))))
        (should (= 1 fired))
        ;; The catch-up fire records last-fire = now (12:30), so the phase
        ;; follows the actual fire and the next due is now + interval (13:30).
        (should (string= "2026-06-15 13:30:00"
                         (format-time-string
                          "%F %T" (e-cron-schedule-next-fire schedule))))))))

(ert-deftest e-cron-test-never-fired-anchor-is-stable ()
  "A never-fired interval schedule anchors its first fire to first registration.
Re-registering before the first fire keeps the original due time rather than
re-basing to the new registration moment."
  (e-cron-test--with-registry
    (e-cron-test--with-clock "2026-06-15 09:00:00"
      ;; First registration anchors at 09:00 -> first due 10:00.
      (e-cron-register :id 'n :when '(:every 3600) :action #'ignore)
      (setq e-cron-test--clock (e-cron-test--time "2026-06-15 09:30:00"))
      (let ((schedule (e-cron-register :id 'n :when '(:every 3600)
                                       :action #'ignore)))
        (should (string= "2026-06-15 10:00:00"
                         (format-time-string
                          "%F %T" (e-cron-schedule-next-fire schedule))))))))

(ert-deftest e-cron-test-state-round-trips-through-file ()
  "Fire state written to `e-cron-state-file' hydrates a fresh session."
  (let ((file (make-temp-file "e-cron-state-test-" nil ".eld")))
    (unwind-protect
        (progn
          ;; Session 1: fire and persist to the real file.
          (let ((e-cron--schedules (make-hash-table :test 'equal))
                (e-cron--state (make-hash-table :test 'equal))
                (e-cron--state-loaded t)
                (e-cron-state-file file))
            (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'stub))
                      ((symbol-function 'cancel-timer) #'ignore)
                      ((symbol-function 'timerp) (lambda (v) (eq v 'stub))))
              (e-cron-test--with-clock "2026-06-15 09:00:00"
                (e-cron-fire (e-cron-register :id 'rt :when '(:every 3600)
                                              :action #'ignore :enabled nil)))))
          ;; Session 2: a fresh in-memory state loads last-fire from disk.
          (let ((e-cron--schedules (make-hash-table :test 'equal))
                (e-cron--state (make-hash-table :test 'equal))
                (e-cron--state-loaded nil)
                (e-cron-state-file file))
            (should (= (float-time (e-cron-test--time "2026-06-15 09:00:00"))
                       (plist-get (e-cron--state-get 'rt) :last-fire)))))
      (delete-file file))))

(provide 'e-cron-test)

;;; e-cron-test.el ends here

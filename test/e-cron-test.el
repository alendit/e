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
event loop; tests drive firing directly."
  (declare (indent 0) (debug t))
  `(let ((e-cron--schedules (make-hash-table :test 'equal)))
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

(provide 'e-cron-test)

;;; e-cron-test.el ends here

;;; e-background-session-test.el --- Tests for background-session triggers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for `e-background-session'.  A fake backend with a small delay
;; keeps a turn "running" long enough to exercise the busy-skip guard.  The
;; debounce window is forced to zero so a requested fire runs on the next tick.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-harness)
(require 'e-backend)
(require 'e-background-session)

(defun e-background-session-test--harness (&optional delay)
  "Return a harness whose fake backend completes after DELAY seconds."
  (e-harness-create
   :backend (e-backend-fake-create
             :items '((:type assistant-message :content "ok")
                      (:type done :reason stop))
             :delay (or delay 0))))

(defun e-background-session-test--wait (predicate &optional timeout)
  "Spin the event loop until PREDICATE is non-nil or TIMEOUT elapses."
  (let ((deadline (+ (float-time) (or timeout 2.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defmacro e-background-session-test--with-trigger (var args &rest body)
  "Bind VAR to a registered trigger built from ARGS, run BODY, then clean up."
  (declare (indent 2))
  `(let ((,var (apply #'e-background-session-register ,args)))
     (unwind-protect
         (progn ,@body)
       (e-background-session-remove (e-background-trigger-id ,var)))))

(ert-deftest e-background-session-test-fire-submits-turn ()
  "A manual fire submits a turn and lazily creates a session."
  (let ((harness (e-background-session-test--harness)))
    (e-background-session-test--with-trigger trigger
        (list :id 'test-fire :harness harness :prompt "do the thing")
      (should-not (e-background-trigger-session-id trigger))
      (let ((turn-id (e-background-session-fire trigger)))
        (should turn-id)
        (should (e-background-trigger-session-id trigger))
        (e-harness-wait-batch harness (e-background-trigger-session-id trigger) 2.0)
        (should (equal (mapcar (lambda (m) (plist-get m :role))
                               (e-harness-messages
                                harness (e-background-trigger-session-id trigger)))
                       '(user assistant)))))))

(ert-deftest e-background-session-test-reuses-session ()
  "Repeated fires continue the same lazily created session."
  (let ((harness (e-background-session-test--harness)))
    (e-background-session-test--with-trigger trigger
        (list :id 'test-reuse :harness harness :prompt "again")
      (e-background-session-fire trigger)
      (let ((session-id (e-background-trigger-session-id trigger)))
        (e-harness-wait-batch harness session-id 2.0)
        (e-background-session-fire trigger)
        (should (equal session-id (e-background-trigger-session-id trigger)))
        (e-harness-wait-batch harness session-id 2.0)
        (should (= 4 (length (e-harness-messages harness session-id))))))))

(ert-deftest e-background-session-test-busy-skip ()
  "A fire is dropped while the session already has a running turn."
  (let ((harness (e-background-session-test--harness 5.0)))
    (e-background-session-test--with-trigger trigger
        (list :id 'test-busy :harness harness :prompt "slow")
      (should (e-background-session-fire trigger))
      ;; The first turn is still running (5s delay); a second fire must skip.
      (should-not (e-background-session-fire trigger))
      (e-harness-abort harness (e-background-trigger-session-id trigger)))))

(ert-deftest e-background-session-test-predicate-gates-fire ()
  "A predicate returning nil suppresses the fire."
  (let ((harness (e-background-session-test--harness))
        (allow nil))
    (e-background-session-test--with-trigger trigger
        (list :id 'test-pred :harness harness :prompt "p"
              :predicate (lambda () allow))
      (should-not (e-background-session-fire trigger))
      (should-not (e-background-trigger-session-id trigger))
      (setq allow t)
      (should (e-background-session-fire trigger))
      (e-harness-wait-batch harness (e-background-trigger-session-id trigger) 2.0))))

(ert-deftest e-background-session-test-empty-prompt-skips ()
  "A prompt function returning nothing skips the fire without a session."
  (let ((harness (e-background-session-test--harness)))
    (e-background-session-test--with-trigger trigger
        (list :id 'test-empty :harness harness :prompt (lambda () "  "))
      (should-not (e-background-session-fire trigger))
      (should-not (e-background-trigger-session-id trigger)))))

(ert-deftest e-background-session-test-debounce-coalesces ()
  "Multiple debounced requests within the window run a single fire."
  (let ((harness (e-background-session-test--harness))
        (fires 0))
    (let ((e-background-session-fire-functions
           (list (lambda (_trigger) (cl-incf fires)))))
      (e-background-session-test--with-trigger trigger
          (list :id 'test-debounce :harness harness :prompt "d"
                :debounce-seconds 0.05)
        (e-background-session--request-fire trigger)
        (e-background-session--request-fire trigger)
        (e-background-session--request-fire trigger)
        (e-background-session-test--wait (lambda () (> fires 0)) 2.0)
        (e-harness-wait-batch harness (e-background-trigger-session-id trigger) 2.0)
        (should (= 1 fires))))))

(ert-deftest e-background-session-test-start-arms-watches ()
  "Starting a trigger adds a live watch for each existing path."
  (let* ((harness (e-background-session-test--harness))
         (dir (make-temp-file "e-bg-" t))
         (watched (expand-file-name "inbox.org" dir)))
    (unwind-protect
        (progn
          (write-region "* Inbox\n" nil watched nil 'silent)
          (e-background-session-test--with-trigger trigger
              (list :id 'test-arm :harness harness :prompt "triage"
                    :paths (list watched) :debounce-seconds 0.05)
            (e-background-session-start trigger)
            (should (e-background-trigger-enabled trigger))
            (should (= 1 (length (e-background-trigger-watches trigger))))
            (e-background-session-stop trigger)
            (should-not (e-background-trigger-enabled trigger))
            (should-not (e-background-trigger-watches trigger))))
      (delete-directory dir t))))

(ert-deftest e-background-session-test-change-event-fires ()
  "A change event dispatched to the handler debounces into one fire.
File-notify change delivery is platform-dependent (macOS kqueue reports atomic
renames only as `stopped'), so this drives the dispatch logic the module owns
with a synthesized event rather than depending on real OS delivery."
  (let* ((harness (e-background-session-test--harness))
         (dir (make-temp-file "e-bg-" t))
         (watched (expand-file-name "inbox.org" dir))
         (fires 0))
    (unwind-protect
        (let ((e-background-session-fire-functions
               (list (lambda (_trigger) (cl-incf fires)))))
          (write-region "* Inbox\n" nil watched nil 'silent)
          (e-background-session-test--with-trigger trigger
              (list :id 'test-change :harness harness :prompt "triage"
                    :paths (list watched) :debounce-seconds 0.05)
            (e-background-session-start trigger)
            (e-background-session--on-fs-event
             trigger watched (list 1 'changed watched))
            (should (e-background-session-test--wait (lambda () (> fires 0)) 2.0))
            (e-harness-wait-batch harness (e-background-trigger-session-id trigger) 2.0)
            (should (= 1 fires))))
      (delete-directory dir t))))

(ert-deftest e-background-session-test-stopped-event-rearms-when-enabled ()
  "A `stopped' event on an enabled trigger re-arms the path and fires.
This is the atomic-rename case (editor save / `write-region'); a `stopped'
arriving after the trigger is disabled is our own teardown and must be ignored."
  (let* ((harness (e-background-session-test--harness))
         (dir (make-temp-file "e-bg-" t))
         (watched (expand-file-name "inbox.org" dir))
         (fires 0))
    (unwind-protect
        (let ((e-background-session-fire-functions
               (list (lambda (_trigger) (cl-incf fires)))))
          (write-region "* Inbox\n" nil watched nil 'silent)
          (e-background-session-test--with-trigger trigger
              (list :id 'test-stopped :harness harness :prompt "triage"
                    :paths (list watched) :debounce-seconds 0.05)
            (e-background-session-start trigger)
            (e-background-session--on-fs-event
             trigger watched (list 1 'stopped watched))
            (should (e-background-session-test--wait (lambda () (> fires 0)) 2.0))
            (e-harness-wait-batch harness (e-background-trigger-session-id trigger) 2.0)
            (should (= 1 fires))
            ;; A stopped event after disabling is teardown -> no further fire.
            (setf (e-background-trigger-enabled trigger) nil)
            (e-background-session--on-fs-event
             trigger watched (list 1 'stopped watched))
            (e-background-session-test--wait (lambda () (> fires 1)) 0.3)
            (should (= 1 fires))))
      (delete-directory dir t))))

(provide 'e-background-session-test)

;;; e-background-session-test.el ends here

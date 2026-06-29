;;; e-harness-compaction-async-test.el --- Async compaction tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused tests for the harness-owned asynchronous compaction lifecycle.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-session)

(defun e-harness-compaction-async-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-harness-compaction-async-test--seed-session (harness)
  "Create and seed session-1 in HARNESS with compactable messages."
  (e-harness-create-session harness :id "session-1")
  (let ((store (e-harness-sessions harness)))
    (e-session-append-message store "session-1"
                              '(:role user :content "old question"))
    (e-session-append-message store "session-1"
                              '(:role assistant :content "old answer"))
    (e-session-append-message store "session-1"
                              '(:role user :content "new question"))))

(ert-deftest e-harness-compaction-async-test-start-returns-before-summary ()
  "Async compaction returns a request before the backend summary finishes."
  (let* ((backend
          (e-backend-create
           :name 'delayed-summary
           :start
           (cl-function
            (lambda (&key messages options on-item on-done &allow-other-keys)
              (ignore messages options)
              (run-at-time
               0.05 nil
               (lambda ()
                 (funcall on-item
                          '(:type assistant-message
                            :content "Old exchange summary."))
                 (funcall on-done '(:status done))))
              (e-backend-request-create
               :metadata '(:provider delayed-summary))))))
         (harness (e-harness-create :backend backend))
         record
         failure)
    (e-harness-compaction-async-test--seed-session harness)
    (let ((request
           (e-harness-compact-session-start
            harness "session-1"
            :keep-recent-tokens 1
            :on-done (lambda (value) (setq record value))
            :on-error (lambda (err) (setq failure err)))))
      (should (e-backend-request-p request))
      (should-not record)
      (should-not failure)
      (should (e-harness-compaction-async-test--wait-until
               (lambda () record)))
      (should-not failure)
      (should (equal (plist-get record :summary)
                     "Old exchange summary."))
      (should (= (length (e-session-compactions
                          (e-harness-sessions harness)
                          "session-1"))
                 1)))))

(ert-deftest e-harness-compaction-async-test-cancel-ignores-late-summary ()
  "Cancelling async compaction prevents late backend callbacks from mutating."
  (let* ((backend-request-cancelled nil)
         (backend
          (e-backend-create
           :name 'late-summary
           :start
           (cl-function
            (lambda (&key messages options on-item on-done &allow-other-keys)
              (ignore messages options)
              (let ((request
                     (e-backend-request-create
                      :cancel (lambda ()
                                (setq backend-request-cancelled t)
                                t)
                      :metadata '(:provider late-summary))))
                (run-at-time
                 0.05 nil
                 (lambda ()
                   (funcall on-item
                            '(:type assistant-message
                              :content "Too late."))
                   (funcall on-done '(:status done))))
                request)))))
         (harness (e-harness-create :backend backend))
         record
         failure)
    (e-harness-compaction-async-test--seed-session harness)
    (let ((request
           (e-harness-compact-session-start
            harness "session-1"
            :keep-recent-tokens 1
            :on-done (lambda (value) (setq record value))
            :on-error (lambda (err) (setq failure err)))))
      (should (e-backend-request-p request))
      (should (e-backend-cancel-request request))
      (should backend-request-cancelled)
      (should (equal failure '(quit "Context compaction cancelled")))
      (accept-process-output nil 0.1)
      (should-not record)
      (should-not (e-session-compactions
                   (e-harness-sessions harness)
                   "session-1")))))

(provide 'e-harness-compaction-async-test)

;;; e-harness-compaction-async-test.el ends here

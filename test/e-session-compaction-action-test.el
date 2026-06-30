;;; e-session-compaction-action-test.el --- Async session compaction action tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused tests for the session-compaction action.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-harness)
(require 'e-layer)
(require 'e-session)

(defun e-session-compaction-action-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, bounded by TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 1)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-session-compaction-action-test--seed-session (harness session-id)
  "Seed HARNESS SESSION-ID with compactable transcript messages."
  (let ((store (e-harness-sessions harness)))
    (e-session-append-message store session-id
                              '(:role user :content "old"))
    (e-session-append-message store session-id
                              '(:role assistant :content "old answer"))
    (e-session-append-message store session-id
                              '(:role user :content "new"))))

(ert-deftest e-session-compaction-action-test-starts-before-summary ()
  "The session compaction action starts asynchronously and settles later."
  (let* ((backend (e-backend-create
                   :name 'delayed-action-summary
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done
                                   &allow-other-keys)
                      (ignore messages options)
                      (run-at-time
                       0.05 nil
                       (lambda ()
                         (funcall on-item
                                  '(:type assistant-message
                                    :content "Action compacted summary."))
                         (funcall on-done '(:status done))))
                      (e-backend-request-create
                       :metadata '(:provider delayed-action-summary))))))
         (harness (e-harness-create :backend backend))
         result)
    (e-harness-create-session harness :id "session-1")
    (e-session-compaction-action-test--seed-session harness "session-1")
    (e-harness-set-intrinsic-capabilities
     harness
     (e-layer-capabilities (e-core-layer-create)))
    (setq result
          (e-actions-call
           'session-compaction
           :compact
           '(:keep_recent_tokens 1)
           (list :harness harness
                 :session-id "session-1"
                 :turn-id "turn-1")))
    (should (eq (plist-get result :status) 'started))
    (should-not (e-session-compactions
                 (e-harness-sessions harness) "session-1"))
    (should
     (e-session-compaction-action-test--wait-until
      (lambda ()
        (e-session-compactions
         (e-harness-sessions harness) "session-1"))))
    (let ((finished
           (cl-find 'action-finished
                    (e-session-activity-events
                     (e-harness-sessions harness) "session-1")
                    :key (lambda (event) (plist-get event :event-type)))))
      (should finished)
      (should (string-match-p
               "Session compacted into"
               (plist-get (plist-get (plist-get finished :payload) :result)
                          :content))))))

(provide 'e-session-compaction-action-test)

;;; e-session-compaction-action-test.el ends here

;;; e-layer-compaction-tool-test.el --- Async compact_session tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused tests for the legacy compact_session tool registration.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-harness)
(require 'e-layer)
(require 'e-session)
(require 'e-tools)

(defun e-layer-compaction-tool-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, bounded by TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 1)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-layer-compaction-tool-test--seed-session (harness session-id)
  "Seed HARNESS SESSION-ID with compactable transcript messages."
  (let ((store (e-harness-sessions harness)))
    (e-session-append-message store session-id
                              '(:role user :content "old"))
    (e-session-append-message store session-id
                              '(:role assistant :content "old answer"))
    (e-session-append-message store session-id
                              '(:role user :content "new"))))

(ert-deftest e-layer-compaction-tool-test-registers-async-start ()
  "The legacy compact_session tool exposes async start metadata."
  (let ((registry (e-tools-registry-create)))
    (e-layer-register-compact-session-tool registry)
    (let* ((tool (gethash "compact_session"
                          (e-tools-registry-tools registry)))
           (metadata (plist-get tool :metadata)))
      (should (functionp (plist-get tool :start)))
      (should (eq (plist-get metadata :blocking-class) 'network)))))

(ert-deftest e-layer-compaction-tool-test-start-returns-before-summary ()
  "compact_session starts compaction asynchronously and settles by callback."
  (let* ((backend (e-backend-create
                   :name 'delayed-tool-summary
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
                                    :content "Compacted summary."))
                         (funcall on-done '(:status done))))
                      (e-backend-request-create
                       :metadata '(:provider delayed-tool-summary))))))
         (harness (e-harness-create :backend backend))
         (registry (e-tools-registry-create))
         result
         failure
         request)
    (e-harness-create-session harness :id "session-1")
    (e-layer-compaction-tool-test--seed-session harness "session-1")
    (e-layer-register-compact-session-tool registry)
    (setq request
          (e-tools-start
           registry
           '(:id "tool-1"
             :name "compact_session"
             :arguments (:keep_recent_tokens 1))
           :context (list :harness harness
                          :session-id "session-1"
                          :turn-id "turn-1")
           :on-done (lambda (value) (setq result value))
           :on-error (lambda (err) (setq failure err))))
    (should (e-backend-request-p request))
    (should-not result)
    (should-not failure)
    (should
     (e-layer-compaction-tool-test--wait-until
      (lambda () result)))
    (should-not failure)
    (should (eq (plist-get result :status) 'ok))
    (should (string-match-p
             "Session compacted into"
             (e-tools-result-content-text result)))))

(ert-deftest e-layer-compaction-tool-test-action-starts-before-summary ()
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
    (e-layer-compaction-tool-test--seed-session harness "session-1")
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
     (e-layer-compaction-tool-test--wait-until
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

(provide 'e-layer-compaction-tool-test)

;;; e-layer-compaction-tool-test.el ends here

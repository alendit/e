;;; e-actions-test.el --- Tests for e action dispatch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for context-bound capability action dispatch.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-chat-session)
(require 'e-harness)

(ert-deftest e-actions-test-call-chat-session-action ()
  "Action dispatch resolves active chat-session actions and injects context."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (e-actions-call
     'chat-session
     :rename
     '(:name "Renamed")
     (list :harness harness :session-id "session-1"))
    (should (equal (e-harness-session-title harness "session-1")
                   "Renamed"))))

(ert-deftest e-actions-test-call-validates-required-arguments ()
  "Action dispatch reports missing descriptor-required arguments."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-actions-call
      'chat-session
      :rename
      nil
      (list :harness harness :session-id "session-1"))
     :type 'e-actions-invalid-arguments)))

(ert-deftest e-actions-test-call-normalizes-string-names-and-arguments ()
  "Action dispatch accepts JSON-like string names and argument keys."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (arguments (make-hash-table :test #'equal)))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (puthash "name" "String renamed" arguments)
    (e-actions-call
     "chat-session"
     ":rename"
     arguments
     (list :harness harness :session-id "session-1"))
    (should (equal (e-harness-session-title harness "session-1")
                   "String renamed"))))

(ert-deftest e-actions-test-call-uses-current-tool-context ()
  "Action dispatch uses `e-tools-current-context' when options omit context."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (e-tools--current-context
          (list :harness harness :session-id "session-1")))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (e-actions-call 'chat-session :rename '(:name "Context renamed"))
    (should (equal (e-harness-session-title harness "session-1")
                   "Context renamed"))))

(provide 'e-actions-test)

;;; e-actions-test.el ends here

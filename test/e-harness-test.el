;;; e-harness-test.el --- Tests for e harness service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for core harness lifecycle behavior.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)

(ert-deftest e-harness-test-prompt-writes-user-and-assistant-messages ()
  "Prompting writes user and assistant messages to the session."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (let ((messages (e-harness-messages harness "session-1")))
      (should (equal (mapcar (lambda (message) (plist-get message :role)) messages)
                     '(user assistant)))
      (should (equal (plist-get (cadr messages) :content) "answer")))
    (should (member 'turn-started (mapcar (lambda (event) (plist-get event :type)) events)))))

(ert-deftest e-harness-test-abort-idle-session-is-explicit-error ()
  "Aborting without an active turn surfaces a lifecycle error."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-harness-abort harness "session-1")
     :type 'e-harness-no-active-turn)))

(provide 'e-harness-test)

;;; e-harness-test.el ends here

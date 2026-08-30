;;; e-harness-test.el --- Harness facade smoke tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small public application-service smoke tests.  Semantic scenario families
;; live in the independently runnable composition suites.

;;; Code:

(require 'ert)
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-harness-test-capability-config-is-harness-local ()
  "Runtime capability config belongs to one harness."
  (let ((first (e-harness-create :backend (e-backend-fake-create :items nil)))
        (second (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-set-capability-config first 'dummy-config '(:value "first"))
    (e-harness-set-capability-config second 'dummy-config '(:value "second"))
    (should (equal (e-harness-capability-config first 'dummy-config)
                   '(:value "first")))
    (should (equal (e-harness-capability-config second 'dummy-config)
                   '(:value "second")))
    (e-harness-set-capability-config first 'dummy-config nil)
    (should-not (e-harness-capability-config first 'dummy-config))
    (should (equal (e-harness-capability-config second 'dummy-config)
                   '(:value "second")))))

(ert-deftest e-harness-test-prompt-writes-user-and-assistant-messages ()
  "Prompting writes user and assistant messages to the session."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (let ((messages (e-harness-messages harness "session-1")))
      (should (string-match-p "\\`[0-9A-HJKMNP-TV-Z]\\{26\\}\\'"
                              (plist-get (car messages) :turn-id)))
      (should (equal (mapcar (lambda (message) (plist-get message :role)) messages)
                     '(user assistant)))
      (should (eq (plist-get (car messages) :origin) 'human))
      (should (equal (plist-get (cadr messages) :content) "answer")))
    (should (member 'turn-started (mapcar (lambda (event) (plist-get event :type)) events)))))

(ert-deftest e-harness-test-state-reports-session-and-active-turn ()
  "Harness state reports settled session status."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should (equal (e-harness-state harness "session-1")
                   '(:session-id "session-1" :active-turn nil :message-count 0)))))

(provide 'e-harness-test)

;;; e-harness-test.el ends here

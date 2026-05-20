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
(require 'e-context)
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

(ert-deftest e-harness-test-async-prompt-wait-settles-turn ()
  "Async prompting tracks an active turn until wait settles it."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id (e-harness-prompt-async harness "session-1" "question")))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     turn-id))
      (should (equal (plist-get (e-harness-wait harness "session-1" 1.0)
                                :status)
                     'done))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     nil)))))

(ert-deftest e-harness-test-abort-cancels-queued-async-turn ()
  "Aborting a queued async turn settles it as cancelled."
  (let* ((called nil)
         (backend (e-backend-create
                   :name "slow"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options on-item)
                              (setq called t)))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question" :delay 1.0)
    (e-harness-abort harness "session-1")
    (should (equal (plist-get (e-harness-wait harness "session-1" 0.1)
                              :status)
                   'cancelled))
    (should (equal called nil))))

(ert-deftest e-harness-test-async-provider-error-is-surfaced ()
  "Async provider failures settle as errors and emit turn-failed."
  (let* ((backend (e-backend-create
                   :name "failing"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options on-item)
                              (error "provider failed")))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'error))
      (should (string-match-p "provider failed" (plist-get settled :error))))
    (should (member 'turn-failed
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-harness-test-follow-up-appends-user-message ()
  "Follow-up submits another turn against the same session."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "first")
    (e-harness-follow-up harness "session-1" "second")
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant user assistant)))))

(ert-deftest e-harness-test-reset-clears-session-messages ()
  "Reset clears transcript messages for a session."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (e-harness-reset harness "session-1")
    (should (equal (e-harness-messages harness "session-1") nil))))

(ert-deftest e-harness-test-state-reports-session-and-active-turn ()
  "Harness state reports settled session status."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (should (equal (e-harness-state harness "session-1")
                   '(:session-id "session-1" :active-turn nil :message-count 0)))))

(ert-deftest e-harness-test-prompt-uses-context-strategy ()
  "Prompting delegates backend message construction to the context strategy."
  (let* ((captured-messages nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore options)
                              (setq captured-messages messages)
                              (funcall on-item '(:type done :reason stop))))))
         (context-strategy
          (e-context-create
           :name 'test-context
           :build (cl-function
                   (lambda (&key sessions session-id options)
                     (ignore sessions session-id options)
                     '(:strategy test-context
                       :messages ((:role user :content "from context"))
                       :options (:model "context-model"))))))
         (harness (e-harness-create
                   :backend backend
                   :context-strategy context-strategy)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "raw prompt")
    (should (equal captured-messages
                   '((:role user :content "from context"))))))

(ert-deftest e-harness-test-prompt-passes-tool-definitions-as-options ()
  "Prompting includes registered tool definitions in backend options."
  (let* ((captured-options nil)
         (backend (e-backend-create
                   :name "capture"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages)
                              (setq captured-options options)
                              (funcall on-item '(:type done :reason stop))))))
         (tools (e-tools-registry-create))
         (harness (e-harness-create :backend backend :tools tools)))
    (e-tools-register tools
                      :name "current_time"
                      :description "Return the current time."
                      :parameters '(:type "object" :properties nil)
                      :handler (lambda (_arguments) "now"))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "raw prompt")
    (let* ((tool (car (plist-get captured-options :tools)))
           (parameters (plist-get tool :parameters)))
      (should (equal (plist-get parameters :type) "object"))
      (should (hash-table-p (plist-get parameters :properties)))
      (should (equal tool
                     `(:type "function"
                       :name "current_time"
                       :description "Return the current time."
                       :parameters ,parameters
                       :strict :json-false))))))

(provide 'e-harness-test)

;;; e-harness-test.el ends here

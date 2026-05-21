;;; e-chat-session-test.el --- Tests for chat session capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for chat-session semantic actions without presentation rendering.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-chat-session)
(require 'e-harness)

(ert-deftest e-chat-session-test-submit-validates-and-queues-prompt ()
  "Submitting validates prompt text and queues an async harness turn."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-chat-session-submit harness "session-1" "")
     :type 'user-error)
    (let ((turn-id (e-chat-session-submit harness "session-1" "hello")))
      (should (equal (plist-get (e-harness-state harness "session-1")
                                :active-turn)
                     turn-id))
      (should (equal (plist-get (car (e-harness-messages harness "session-1"))
                                :content)
                     "hello")))))

(ert-deftest e-chat-session-test-abort-reset-and-rename ()
  "Chat-session actions delegate abort, reset, and rename to the harness/store."
  (let ((harness (e-harness-create
                  :backend (e-backend-create
                            :name "delayed"
                            :stream (lambda (&rest _args) nil)))))
    (e-harness-create-session harness :id "session-1")
    (e-chat-session-submit harness "session-1" "hello" :delay 1.0)
    (e-chat-session-abort harness "session-1")
    (should (equal (plist-get (e-harness-wait harness "session-1" 0.1)
                              :status)
                   'cancelled))
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "persisted"))
    (e-chat-session-reset harness "session-1")
    (should-not (e-harness-messages harness "session-1"))
    (e-chat-session-rename harness "session-1" "Renamed")
    (should (equal (e-session-display-title
                    (e-harness-sessions harness)
                    "session-1")
                   "Renamed"))))

(ert-deftest e-chat-session-test-options-and-context ()
  "Chat-session actions update options and build context preview data."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-chat-session-set-model harness "session-1" "gpt-test")
    (e-chat-session-set-effort harness "session-1" "high")
    (should (equal (e-harness-session-options harness "session-1")
                   '(:model "gpt-test" :reasoning-effort "high")))
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "context question"))
    (should (equal (mapcar (lambda (message)
                             (plist-get message :content))
                           (plist-get
                            (e-chat-session-context harness "session-1")
                            :messages))
                   '("context question")))))

(ert-deftest e-chat-session-test-capability-actions ()
  "The chat-session capability exposes stable shell action names."
  (let ((capability (e-chat-session-capability-create)))
    (should (eq (e-capability-id capability) 'chat-session))
    (dolist (action '(:submit :abort :reset :rename :set-model :set-effort
                      :context))
      (should (functionp (e-capabilities-action capability action))))))

(provide 'e-chat-session-test)

;;; e-chat-session-test.el ends here

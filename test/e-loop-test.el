;;; e-loop-test.el --- Tests for e agent loop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for turn execution against fake backends and tools.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-loop)
(require 'e-tools)

(ert-deftest e-loop-test-persists-assistant-message ()
  "Assistant stream messages are appended and lifecycle events are emitted."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-delta :content "hello")
                            (:type assistant-message :content "hello")
                            (:type done :reason stop))))
         (events nil)
         (messages nil)
         (result (e-loop-run-turn
                  :session-id "session-1"
                  :turn-id "turn-1"
                  :messages '((:role user :content "hi"))
                  :backend backend
                  :tools (e-tools-registry-create)
                  :options '(:model "fake")
                  :on-event (lambda (event) (push event events))
                  :append-message (lambda (message) (push message messages)))))
    (should (equal (plist-get result :status) 'done))
    (should (equal (plist-get (car messages) :role) 'assistant))
    (should (equal (plist-get (car messages) :content) "hello"))
    (should (member 'turn-started (mapcar (lambda (event) (plist-get event :type)) events)))
    (should (member 'turn-finished (mapcar (lambda (event) (plist-get event :type)) events)))))

(ert-deftest e-loop-test-persists-delta-only-assistant-message ()
  "Assistant deltas are persisted when no final assistant message arrives."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-delta :content "hel")
                            (:type assistant-delta :content "lo")
                            (:type done :reason stop))))
         (messages nil))
    (e-loop-run-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options nil
     :on-event #'ignore
     :append-message (lambda (message) (push message messages)))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           messages)
                   '(assistant)))
    (should (equal (plist-get (car messages) :content) "hello"))))

(ert-deftest e-loop-test-persists-empty-output-assistant-message ()
  "Turns with no assistant output append a visible assistant placeholder."
  (let* ((backend (e-backend-fake-create
                   :items '((:type done :reason stop))))
         (events nil)
         (messages nil))
    (e-loop-run-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options nil
     :on-event (lambda (event) (push event events))
     :append-message (lambda (message) (push message messages)))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           messages)
                   '(assistant)))
    (should (equal (plist-get (car messages) :content) "✅ Done"))
    (should (equal (plist-get (plist-get (car messages) :metadata) :synthetic)
                   t))
    (should (member 'backend-empty-output
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))
    (should (member 'message-added
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-loop-test-surfaces-backend-error ()
  "Backend error items stop the turn with an explicit error."
  (let* ((backend (e-backend-fake-create
                   :items '((:type backend-error :content "provider failed"))))
         (events nil))
    (should-error
     (e-loop-run-turn
      :session-id "session-1"
      :turn-id "turn-1"
      :messages '((:role user :content "hi"))
      :backend backend
      :tools (e-tools-registry-create)
      :options nil
      :on-event (lambda (event) (push event events))
      :append-message #'ignore)
     :type 'e-loop-backend-error)
    (should (member 'turn-started
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-loop-test-executes-tool-call-and-appends-result ()
  "Tool calls execute through the registry and append tool result messages."
  (let* ((backend (e-backend-fake-create
                   :items '((:type tool-call :id "call-1" :name "echo" :arguments (:text "hi"))
                            (:type done :reason stop))))
         (tools (e-tools-registry-create))
         (messages nil))
    (e-tools-register tools
                      :name "echo"
                      :description "Echo text."
                      :handler (lambda (arguments) (plist-get arguments :text)))
    (e-loop-run-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools tools
     :options '(:max-tool-iterations 1)
     :on-event #'ignore
     :append-message (lambda (message) (push message messages)))
    (let ((ordered (nreverse messages)))
      (should (equal (mapcar (lambda (message) (plist-get message :role))
                             ordered)
                     '(tool-call tool assistant)))
      (should (equal (plist-get (plist-get (cadr ordered) :content) :status)
                     'ok))
      (should (equal (plist-get (cl-third ordered) :content) "✅ Done")))))

(ert-deftest e-loop-test-emits-intermittent-reasoning-and-tool-call-events ()
  "Reasoning deltas and tool calls are surfaced before the final message."
  (let* ((backend (e-backend-fake-create
                   :items '((:type reasoning-delta :content "thinking")
                            (:type tool-call :id "call-1" :name "echo" :arguments (:text "hi"))
                            (:type assistant-message :content "done")
                            (:type done :reason stop))))
         (tools (e-tools-registry-create))
         (events nil))
    (e-tools-register tools
                      :name "echo"
                      :description "Echo text."
                      :handler (lambda (arguments) (plist-get arguments :text)))
    (e-loop-run-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools tools
     :options nil
     :on-event (lambda (event) (push event events))
     :append-message #'ignore)
    (let ((types (mapcar (lambda (event) (plist-get event :type))
                         (nreverse events))))
      (should (equal types
                     '(turn-started
                       reasoning-delta
                       tool-started
                       tool-finished
                       message-added
                       turn-finished))))))

(ert-deftest e-loop-test-requeries-backend-after-tool-result ()
  "Tool results are fed back into the backend until an assistant message settles."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "tool-followup"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore options)
                              (setq calls (1+ calls))
                              (if (= calls 1)
                                  (progn
                                    (should (equal (mapcar (lambda (message)
                                                             (plist-get message :role))
                                                           messages)
                                                   '(user)))
                                    (funcall on-item
                                             '(:type tool-call
                                               :id "call-1"
                                               :name "echo"
                                               :arguments (:text "hi")))
                                (funcall on-item '(:type done :reason tool-use)))
                                (should (equal (mapcar (lambda (message)
                                                         (plist-get message :role))
                                                       messages)
                                               '(user tool-call tool)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "saw tool result"))
                                (funcall on-item '(:type done :reason stop)))))))
         (tools (e-tools-registry-create))
         (messages nil))
    (e-tools-register tools
                      :name "echo"
                      :description "Echo text."
                      :handler (lambda (arguments) (plist-get arguments :text)))
    (e-loop-run-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools tools
     :options nil
     :on-event #'ignore
     :append-message (lambda (message) (push message messages)))
    (should (equal calls 2))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (nreverse messages))
                   '(tool-call tool assistant)))))

(provide 'e-loop-test)

;;; e-loop-test.el ends here

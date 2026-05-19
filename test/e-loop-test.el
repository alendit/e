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
     :options nil
     :on-event #'ignore
     :append-message (lambda (message) (push message messages)))
    (should (equal (plist-get (car messages) :role) 'tool))
    (should (equal (plist-get (plist-get (car messages) :content) :status) 'ok))))

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
                                               '(user tool)))
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
                   '(tool assistant)))))

(provide 'e-loop-test)

;;; e-loop-test.el ends here

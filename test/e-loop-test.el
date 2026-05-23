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

(defun e-loop-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

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
                  :on-event (lambda (type payload)
                              (push (list :type type :payload payload)
                                    events))
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

(ert-deftest e-loop-test-empty-output-does-not-persist-assistant-message ()
  "Turns with no assistant output surface an error without fake content."
  (let* ((backend (e-backend-fake-create
                   :items '((:type done :reason stop))))
         (events nil)
         (messages nil))
    (should-error
     (e-loop-run-turn
      :session-id "session-1"
      :turn-id "turn-1"
      :messages '((:role user :content "hi"))
      :backend backend
      :tools (e-tools-registry-create)
      :options nil
      :on-event (lambda (type payload)
                  (push (list :type type :payload payload) events))
      :append-message (lambda (message) (push message messages)))
     :type 'e-loop-empty-output)
    (should (null messages))
    (should (member 'backend-empty-output
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
      :on-event (lambda (type payload)
                  (push (list :type type :payload payload) events))
      :append-message #'ignore)
     :type 'e-loop-backend-error)
    (should (member 'turn-started
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-loop-test-emits-intermittent-reasoning-and-tool-call-events ()
  "Reasoning deltas and tool calls are surfaced before the final message."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "fake-tool-followup"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options)
                              (setq calls (1+ calls))
                              (if (= calls 1)
                                  (progn
                                    (funcall on-item
                                             '(:type reasoning-delta
                                               :content "thinking"))
                                    (funcall on-item
                                             '(:type tool-call
                                               :id "call-1"
                                               :name "echo"
                                               :arguments (:text "hi")))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
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
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message #'ignore)
    (let ((types (mapcar (lambda (event) (plist-get event :type))
                         (nreverse events))))
      (should (equal types
                     '(turn-started
                       reasoning-delta
                       tool-started
                       tool-finished
                       turn-finished))))))

(ert-deftest e-loop-test-tool-finished-includes-call-and-result ()
  "Tool-finished descriptors include the original call and executed result."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "fake-tool-finished-followup"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options)
                              (setq calls (1+ calls))
                              (if (= calls 1)
                                  (progn
                                    (funcall on-item
                                             '(:type tool-call
                                               :id "call-1"
                                               :name "echo"
                                               :arguments (:text "hi")))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
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
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message #'ignore)
    (let* ((event (cl-find 'tool-finished events
                           :key (lambda (event) (plist-get event :type))))
           (payload (plist-get event :payload)))
      (should (equal (plist-get (plist-get payload :tool-call) :id)
                     "call-1"))
      (should (equal (plist-get (plist-get payload :result) :content)
                     "hi")))))

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

(ert-deftest e-loop-test-tool-call-response-commentary-does-not-settle-turn ()
  "Assistant commentary before a tool call does not prevent tool follow-up."
  (let* ((calls 0)
         (events nil)
         (backend (e-backend-create
                   :name "tool-commentary-followup"
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
                                             '(:type assistant-delta
                                               :content "I'll inspect."))
                                    (funcall on-item
                                             '(:type assistant-message
                                               :content "I'll inspect."))
                                    (funcall on-item
                                             '(:type tool-call
                                               :id "call-1"
                                               :name "echo"
                                               :arguments (:text "hi")))
                                    (funcall on-item '(:type done :reason stop)))
                                (should (equal (mapcar (lambda (message)
                                                         (plist-get message :role))
                                                       messages)
                                               '(user tool-call tool)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "final after tool"))
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
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message (lambda (message) (push message messages)))
    (should (equal calls 2))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (nreverse messages))
                   '(tool-call tool assistant)))
    (should (equal (plist-get (car (last (nreverse messages))) :content)
                   "final after tool"))
    (should (cl-find "I'll inspect."
                     events
                     :test #'equal
                     :key (lambda (event)
                            (plist-get (plist-get event :payload) :content))))))

(ert-deftest e-loop-test-start-turn-settles-after-async-backend-done ()
  "Async turn execution does not append the assistant message before provider completion."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "async answer")
                            (:type done :reason stop))))
         (events nil)
         (messages nil)
         (settled nil))
    (e-loop-start-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options nil
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message (lambda (message) (push message messages))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :error err))))
    (should (null settled))
    (should (null messages))
    (should (e-loop-test--wait-until (lambda () settled)))
    (should (equal (plist-get settled :status) 'done))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (nreverse messages))
                   '(assistant)))
    (should (equal (mapcar (lambda (event) (plist-get event :type))
                           (nreverse events))
                   '(turn-started turn-finished)))))

(ert-deftest e-loop-test-start-turn-requeries-backend-after-async-tool-result ()
  "Async turn execution starts a follow-up backend request after synchronous tool results."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "async-tool-followup"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore options on-error on-request-start)
                      (setq calls (1+ calls))
                      (run-at-time
                       0 nil
                       (lambda ()
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
                               (funcall on-item
                                        '(:type done :reason tool-use)))
                           (should (equal (mapcar (lambda (message)
                                                    (plist-get message :role))
                                                  messages)
                                          '(user tool-call tool)))
                           (funcall on-item
                                    '(:type assistant-message
                                      :content "final answer"))
                           (funcall on-item
                                    '(:type done :reason stop)))
                         (funcall on-done '(:status done))))
                      nil))))
         (tools (e-tools-registry-create))
         (events nil)
         (messages nil)
         (settled nil))
    (e-tools-register tools
                      :name "echo"
                      :description "Echo text."
                      :handler (lambda (arguments) (plist-get arguments :text)))
    (e-loop-start-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools tools
     :options nil
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message (lambda (message) (push message messages))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :error err))))
    (should (e-loop-test--wait-until (lambda () settled)))
    (should (equal calls 2))
    (should (equal (plist-get settled :status) 'done))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (nreverse messages))
                   '(tool-call tool assistant)))
    (should (equal (mapcar (lambda (event) (plist-get event :type))
                           (nreverse events))
                   '(turn-started tool-started tool-finished turn-finished)))))

(ert-deftest e-loop-test-start-turn-waits-for-delayed-async-tool-result ()
  "Async turn execution waits for async tools before the follow-up request."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "delayed-tool-followup"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore options on-error on-request-start)
                      (setq calls (1+ calls))
                      (run-at-time
                       0 nil
                       (lambda ()
                         (if (= calls 1)
                             (progn
                               (should (equal (mapcar (lambda (message)
                                                        (plist-get message :role))
                                                      messages)
                                              '(user)))
                               (funcall on-item
                                        '(:type tool-call
                                          :id "call-1"
                                          :name "later"
                                          :arguments (:text "hi")))
                               (funcall on-item
                                        '(:type done :reason tool-use)))
                           (should (equal (mapcar (lambda (message)
                                                    (plist-get message :role))
                                                  messages)
                                          '(user tool-call tool)))
                           (funcall on-item
                                    '(:type assistant-message
                                      :content "final answer"))
                           (funcall on-item
                                    '(:type done :reason stop)))
                         (funcall on-done '(:status done))))
                      nil))))
         (tools (e-tools-registry-create))
         (events nil)
         (messages nil)
         (settled nil))
    (e-tools-register tools
                      :name "later"
                      :description "Return later."
                      :start
                      (cl-function
                       (lambda (&key arguments on-done on-error
                                      on-request-start)
                         (ignore on-error on-request-start)
                         (run-at-time
                          0.05 nil
                          (lambda ()
                            (funcall on-done
                                     (plist-get arguments :text))))
                         nil)))
    (e-loop-start-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools tools
     :options nil
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message (lambda (message) (push message messages))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :error err))))
    (should (e-loop-test--wait-until
             (lambda ()
               (cl-find 'tool-started events
                        :key (lambda (event) (plist-get event :type))))))
    (should (equal calls 1))
    (should (null settled))
    (should (e-loop-test--wait-until (lambda () settled)))
    (should (equal calls 2))
    (should (equal (plist-get settled :status) 'done))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (nreverse messages))
                   '(tool-call tool assistant)))
    (should (equal (mapcar (lambda (event) (plist-get event :type))
                           (nreverse events))
                   '(turn-started tool-started tool-finished turn-finished)))))

(ert-deftest e-loop-test-start-turn-runs-async-tools-serially ()
  "Multiple async tool calls run serially in provider order."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "serial-tools"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore options on-error on-request-start)
                      (setq calls (1+ calls))
                      (run-at-time
                       0 nil
                       (lambda ()
                         (if (= calls 1)
                             (progn
                               (funcall on-item
                                        '(:type tool-call
                                          :id "call-1"
                                          :name "later"
                                          :arguments (:text "first")))
                               (funcall on-item
                                        '(:type tool-call
                                          :id "call-2"
                                          :name "later"
                                          :arguments (:text "second")))
                               (funcall on-item
                                        '(:type done :reason tool-use)))
                           (should (equal (mapcar (lambda (message)
                                                    (plist-get message :role))
                                                  messages)
                                          '(user tool-call tool
                                                 tool-call tool)))
                           (funcall on-item
                                    '(:type assistant-message
                                      :content "done"))
                           (funcall on-item
                                    '(:type done :reason stop)))
                         (funcall on-done '(:status done))))
                      nil))))
         (tools (e-tools-registry-create))
         (started nil)
         (finishers nil)
         (messages nil)
         (settled nil))
    (e-tools-register tools
                      :name "later"
                      :description "Return later."
                      :start
                      (cl-function
                       (lambda (&key arguments on-done on-error
                                      on-request-start)
                         (ignore on-error on-request-start)
                         (push (plist-get arguments :text) started)
                         (push (lambda ()
                                 (funcall on-done
                                          (plist-get arguments :text)))
                               finishers)
                         nil)))
    (e-loop-start-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools tools
     :options nil
     :on-event (lambda (_type _payload))
     :append-message (lambda (message) (push message messages))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :error err))))
    (should (e-loop-test--wait-until (lambda () started)))
    (should (equal (nreverse (copy-sequence started)) '("first")))
    (should (equal (length finishers) 1))
    (funcall (pop finishers))
    (should (e-loop-test--wait-until
             (lambda () (= (length started) 2))))
    (should (equal (nreverse (copy-sequence started))
                   '("first" "second")))
    (should (null settled))
    (funcall (pop finishers))
    (should (e-loop-test--wait-until (lambda () settled)))
    (should (equal calls 2))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (nreverse messages))
                   '(tool-call tool tool-call tool assistant)))))

(provide 'e-loop-test)

;;; e-loop-test.el ends here

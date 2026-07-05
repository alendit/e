;;; e-loop-test.el --- Tests for e agent loop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for turn execution against fake backends and tools.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e)
(require 'e-backend)
(require 'e-dev-profile)
(require 'e-loop)
(require 'e-request)
(require 'e-tools)
(require 'e-work)

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
         (result (e-loop-run-turn-batch
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

(ert-deftest e-loop-test-sync-run-turn-rejects-hot-path ()
  "The synchronous run-turn wrapper cannot run inside marked hot paths."
  (let ((messages nil)
        (backend (e-backend-fake-create
                  :items '((:type assistant-message :content "hello")
                           (:type done :reason stop)))))
    (let ((err (should-error
                (e-request-with-hot-path 'loop-run-turn
                  (e-loop-run-turn-batch
                   :session-id "session-1"
                   :turn-id "turn-1"
                   :messages '((:role user :content "hi"))
                   :backend backend
                   :tools (e-tools-registry-create)
                   :options nil
                   :on-event #'ignore
                   :append-message (lambda (message)
                                     (push message messages))))
                :type 'e-request-blocking-call-in-hot-path)))
      (should (equal (cdr err) '(e-loop-run-turn-batch loop-run-turn))))
    (should-not messages)))

(ert-deftest e-loop-test-persists-delta-only-assistant-message ()
  "Assistant deltas are persisted when no final assistant message arrives."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-delta :content "hel")
                            (:type assistant-delta :content "lo")
                            (:type done :reason stop))))
         (messages nil))
    (e-loop-run-turn-batch
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
     (e-loop-run-turn-batch
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
                   :items '((:type backend-error
                              :content "provider failed"
                              :payload (:provider-error full)))))
         (events nil))
    (let ((err
           (should-error
            (e-loop-run-turn-batch
             :session-id "session-1"
             :turn-id "turn-1"
             :messages '((:role user :content "hi"))
             :backend backend
             :tools (e-tools-registry-create)
             :options nil
             :on-event (lambda (type payload)
                         (push (list :type type :payload payload) events))
             :append-message #'ignore)
            :type 'e-loop-backend-error)))
      (should (equal (cdr err)
                     '("provider failed" (:provider-error full)))))
    (should (member 'turn-started
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-loop-test-provider-request-lifecycle-surrounds-final-event ()
  "Provider request lifecycle events bracket backend work before turn finish."
  (let* ((backend
          (e-backend-create
           :name "lifecycle"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata '(:provider codex
                                    :transport url-retrieve
                                    :url-host "example.test"
                                    :url-path "/codex/responses"
                                    :timeout-seconds 180)))
              (funcall on-item '(:type assistant-message :content "hello"))
              (funcall on-item '(:type done :reason stop))
              (funcall on-done '(:status done))
              nil))))
         (events nil))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options nil
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message #'ignore)
    (let* ((ordered (nreverse events))
           (types (mapcar (lambda (event) (plist-get event :type))
                          ordered))
           (started (seq-find
                     (lambda (event)
                       (eq (plist-get event :type)
                           'provider-request-started))
                     ordered))
           (finished (seq-find
                      (lambda (event)
                        (eq (plist-get event :type)
                            'provider-request-finished))
                      ordered))
           (started-payload (plist-get started :payload))
           (finished-payload (plist-get finished :payload)))
      (should (equal types
                     '(turn-started
                       provider-request-started
                       provider-request-finished
                       turn-finished)))
      (should (equal (plist-get started-payload :provider) 'codex))
      (should (equal (plist-get started-payload :transport) 'url-retrieve))
      (should (equal (plist-get started-payload :url-host) "example.test"))
      (should (equal (plist-get started-payload :url-path) "/codex/responses"))
      (should (equal (plist-get started-payload :timeout-seconds) 180))
      (should (eq (plist-get started-payload :status) 'started))
      (should (eq (plist-get finished-payload :status) 'done))
      (should (numberp (plist-get finished-payload :elapsed-seconds)))
      (should-not (plist-member started-payload :url))
      (should-not (plist-member finished-payload :url)))))

(ert-deftest e-loop-test-request-lifecycle-includes-scalar-diagnostics ()
  "Provider request lifecycle payloads expose sanitized adapter diagnostics."
  (let* ((request
          (e-backend-request-create
           :metadata
           (list :provider 'codex
                 :transport 'websocket
                 :url-host "example.test"
                 :url-path "/backend-api/codex/responses"
                 :timeout-seconds 180
                 :model "leaky-top-level"
                 :diagnostics
                 (list :model "gpt-5.5"
                       :reasoning-effort "high"
                       :response-store :json-false
                       :prompt-cache-key-present t
                       :provider-anchor-present nil
                       :input-message-count 3
                       :nested '(:unsafe "value")
                       :vector ["unsafe"]
                       :function (symbol-function 'ignore)))))
         (payload (e-loop--request-lifecycle-payload request 'started))
         (diagnostics (plist-get payload :diagnostics)))
    (should (equal (plist-get payload :provider) 'codex))
    (should (equal (plist-get payload :transport) 'websocket))
    (should-not (plist-member payload :model))
    (should (equal diagnostics
                   '(:model "gpt-5.5"
                     :reasoning-effort "high"
                     :response-store :json-false
                     :prompt-cache-key-present t
                     :provider-anchor-present nil
                     :input-message-count 3)))
    (should-not (plist-member diagnostics :nested))
    (should-not (plist-member diagnostics :vector))
    (should-not (plist-member diagnostics :function))))

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
    (e-loop-run-turn-batch
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
                       provider-request-started
                       reasoning-delta
                       tool-started
                       provider-request-finished
                       tool-finished
                       provider-request-started
                       provider-request-finished
                       turn-finished))))))

(ert-deftest e-loop-test-emits-raw-reasoning-events-without-assistant-text ()
  "Raw reasoning events are surfaced without becoming assistant output."
  (let* ((backend (e-backend-create
                   :name "fake-raw-reasoning"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options)
                              (funcall on-item
                                       '(:type reasoning-raw-delta
                                         :stream-kind raw
                                         :content "raw thinking"))
                              (funcall on-item
                                       '(:type assistant-message
                                         :content "done"))
                              (funcall on-item
                                       '(:type done :reason stop))))))
         (events nil)
         (messages nil))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options nil
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message (lambda (message)
                       (push message messages)))
    (let ((events (nreverse events)))
      (should (memq 'reasoning-raw-delta
                    (mapcar (lambda (event)
                              (plist-get event :type))
                            events)))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             (nreverse messages))
                     '("done"))))))

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
    (e-loop-run-turn-batch
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

(ert-deftest e-loop-test-refreshes-messages-after-tool-requesting-context-refresh ()
  "A tool result may ask the loop to rebuild context before follow-up sampling."
  (let* ((calls 0)
         (second-request-messages nil)
         (refreshed-messages
          '((:role compaction-summary :content "summary")
            (:role user :content "recent prompt")))
         (backend (e-backend-create
                   :name "fake-refresh-after-tool"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore options)
                              (setq calls (1+ calls))
                              (if (= calls 1)
                                  (progn
                                    (funcall on-item
                                             '(:type tool-call
                                               :id "call-1"
                                               :name "refreshing_tool"
                                               :arguments nil))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (setq second-request-messages messages)
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
         (tools (e-tools-registry-create)))
    (e-tools-register
     tools
     :name "refreshing_tool"
     :description "Compact session."
     :handler
     (lambda (_arguments)
       (e-tools-result-create
        (plist-get (e-tools-current-context) :tool-call)
        'ok
        "compacted"
        '(:refresh-context t))))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "old prompt"))
     :backend backend
     :tools tools
     :options nil
     :on-event #'ignore
     :append-message #'ignore
     :refresh-messages (lambda () refreshed-messages))
    (should (= calls 2))
    (should (equal second-request-messages refreshed-messages))))

(ert-deftest e-loop-test-tool-lifecycle-prepares-call-before-append ()
  "The tool lifecycle can transform a call before the loop appends it."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "fake-tool-lifecycle-pre"
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
                                               :arguments (:text "raw")))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
         (messages nil)
         (tool-lifecycle
          (e-tool-lifecycle-create
           :prepare (lambda (tool-call)
                      (plist-put (copy-sequence tool-call)
                                 :arguments '(:text "prepared")))
           :start (cl-function
                   (lambda (tool-call &key on-done &allow-other-keys)
                     (funcall on-done
                              (list :tool-call-id (plist-get tool-call :id)
                                    :name (plist-get tool-call :name)
                                    :status 'ok
                                    :content (plist-get
                                              (plist-get tool-call :arguments)
                                              :text)))
                     nil)))))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tool-lifecycle tool-lifecycle
     :options nil
     :on-event #'ignore
     :append-message (lambda (message)
                       (push message messages)))
    (let* ((appended (nreverse messages))
           (tool-call (cl-find 'tool-call appended
                               :key (lambda (message)
                                      (plist-get message :role))))
           (tool-result (cl-find 'tool appended
                                 :key (lambda (message)
                                        (plist-get message :role)))))
      (should (equal (plist-get (plist-get (plist-get tool-call :content)
                                           :arguments)
                                :text)
                     "prepared"))
      (should (equal (plist-get (plist-get tool-result :content) :content)
                     "prepared")))))

(ert-deftest e-loop-test-tool-lifecycle-result-is-appended-and-emitted ()
  "The loop appends and emits the lifecycle-shaped tool result."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "fake-tool-lifecycle-post"
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
                                               :arguments nil))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
         (events nil)
         (messages nil)
         (tool-lifecycle
         (e-tool-lifecycle-create
           :start (cl-function
                   (lambda (tool-call &key on-done &allow-other-keys)
                     (funcall on-done
                              (list :tool-call-id (plist-get tool-call :id)
                                    :name (plist-get tool-call :name)
                                    :status 'ok
                                    :content "post-processed"))
                     nil)))))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tool-lifecycle tool-lifecycle
     :options nil
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message (lambda (message)
                       (push message messages)))
    (let* ((tool-message (cl-find 'tool messages
                                  :key (lambda (message)
                                         (plist-get message :role))))
           (tool-event (cl-find 'tool-finished events
                                :key (lambda (event)
                                       (plist-get event :type))))
           (event-result (plist-get (plist-get tool-event :payload) :result)))
      (should (equal (plist-get (plist-get tool-message :content) :content)
                     "post-processed"))
      (should (equal (plist-get event-result :content)
                     "post-processed")))))

(ert-deftest e-loop-test-tool-result-metadata-is-appended-on-message ()
  "Tool result metadata is durable on the appended tool message."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "fake-tool-result-metadata"
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
                                               :arguments nil))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
         (messages nil)
         (tool-lifecycle
          (e-tool-lifecycle-create
           :start (cl-function
                   (lambda (tool-call &key on-done &allow-other-keys)
                     (funcall on-done
                              (list :tool-call-id (plist-get tool-call :id)
                                    :name (plist-get tool-call :name)
                                    :status 'ok
                                    :content "result"
                                    :metadata '(:tool-usage
                                                ((:kind resource-usage
                                                  :tool "echo"
                                                  :resources
                                                  ((:uri "file://a"
                                                    :operation read)))))))
                     nil)))))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tool-lifecycle tool-lifecycle
     :options nil
     :on-event #'ignore
     :append-message (lambda (message)
                       (push message messages)))
    (let ((tool-message (cl-find 'tool messages
                                 :key (lambda (message)
                                        (plist-get message :role)))))
      (should
       (equal (plist-get tool-message :metadata)
              '(:tool-usage
                ((:kind resource-usage
                  :tool "echo"
                  :resources ((:uri "file://a"
                               :operation read))))))))))

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
    (e-loop-run-turn-batch
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
    (e-loop-run-turn-batch
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
         (request nil)
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
     :on-request-start (lambda (value)
                         (setq request value))
     :append-message (lambda (message) (push message messages))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :error err))))
    (should (null settled))
    (should (null messages))
    (should (e-loop-test--wait-until (lambda () settled)))
    (should (e-work-handle-p
             (plist-get (e-backend-request-metadata request)
                        :work-handle)))
    (should (equal (plist-get settled :status) 'done))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (nreverse messages))
                   '(assistant)))
    (should (equal (mapcar (lambda (event) (plist-get event :type))
                           (nreverse events))
                   '(turn-started
                     provider-request-started
                     provider-request-finished
                     turn-finished)))))

(ert-deftest e-loop-test-profile-records-backend-start-span ()
  "Enabled dev profiling records loop backend startup spans."
  (let* ((profile-directory (make-temp-file "e-loop-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (settled nil))
    (unwind-protect
        (progn
          (e-dev-profile-start)
          (e-loop-start-turn
           :session-id "session-1"
           :turn-id "turn-1"
           :messages '((:role user :content "hi"))
           :backend backend
           :tools (e-tools-registry-create)
           :options nil
           :on-event (lambda (&rest _args))
           :append-message (lambda (&rest _args))
           :on-done (lambda (result) (setq settled result))
           :on-error (lambda (err) (setq settled (list :error err))))
          (should (e-loop-test--wait-until (lambda () settled)))
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "loop.backend-start"
                               aggregates nil nil #'equal))))
      (delete-directory profile-directory t))))

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

(ert-deftest e-loop-test-start-turn-requeries-backend-after-pending-input ()
  "Async turn execution starts a follow-up request after pending user input."
  (let* ((calls 0)
         (pending nil)
         (backend (e-backend-create
                   :name "pending-input-followup"
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
                                        '(:type assistant-message
                                          :content "first answer"))
                               (setq pending
                                     '((:role user
                                        :content "steer here"
                                        :metadata (:source chat-composer))))
                               (funcall on-item
                                        '(:type done :reason stop)))
                           (should (equal (mapcar (lambda (message)
                                                    (plist-get message :role))
                                                  messages)
                                          '(user assistant user)))
                           (should (equal (plist-get (car (last messages))
                                                     :content)
                                          "steer here"))
                           (funcall on-item
                                    '(:type assistant-message
                                      :content "final answer"))
                           (funcall on-item
                                    '(:type done :reason stop)))
                         (funcall on-done '(:status done))))
                      nil))))
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
     :drain-pending-input (lambda ()
                            (prog1 pending
                              (setq pending nil)))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :error err))))
    (should (e-loop-test--wait-until (lambda () settled)))
    (should (equal calls 2))
    (should (equal (plist-get settled :status) 'done))
    (let ((ordered-messages (nreverse messages))
          (ordered-events (nreverse events)))
      (should (equal (mapcar (lambda (message) (plist-get message :role))
                             ordered-messages)
                     '(assistant user assistant)))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             ordered-messages)
                     '("first answer" "steer here" "final answer")))
      (should (equal (mapcar (lambda (event) (plist-get event :type))
                             ordered-events)
                     '(turn-started turn-finished))))))

(ert-deftest e-loop-test-start-turn-persists-tool-result-when-tool-quits ()
  "Async turn execution records a tool result when tool execution quits."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "async-tool-quit-followup"
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
                                          :id "call-quit"
                                          :name "quit-tool"
                                          :arguments nil))
                               (funcall on-item
                                        '(:type done :reason tool-use)))
                           (should (equal (mapcar (lambda (message)
                                                    (plist-get message :role))
                                                  messages)
                                          '(user tool-call tool)))
                           (let ((tool-result (nth 2 messages)))
                             (should (equal (plist-get
                                             (plist-get tool-result :content)
                                             :tool-call-id)
                                            "call-quit"))
                             (should (eq (plist-get
                                          (plist-get tool-result :content)
                                          :status)
                                         'error))
                             (should (equal (plist-get
                                             (plist-get tool-result :content)
                                             :content)
                                            "Quit")))
                           (funcall on-item
                                    '(:type assistant-message
                                      :content "handled quit"))
                           (funcall on-item
                                    '(:type done :reason stop)))
                         (funcall on-done '(:status done))))
                      nil))))
         (tools (e-tools-registry-create))
         (events nil)
         (messages nil)
         (settled nil))
    (e-tools-register tools
                      :name "quit-tool"
                      :description "Quit."
                      :handler (lambda (_arguments)
                                 (signal 'quit nil)))
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

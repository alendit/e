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
(require 'e-context-lifetime)
(require 'e-dev-profile)
(require 'e-loop)
(require 'e-request)
(require 'e-tools)
(require 'e-openai)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-work)

(defun e-loop-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(ert-deftest e-loop-test-inherited-observation-does-not-promote-anchor ()
  "An inherited observation cannot advance a provider anchor candidate."
  (let* ((candidate '(:provider-id openai :metadata (:response-id "resp-2")))
         (options '(:provider-continuation t
                    :provider-anchor-provider-id openai
                    :context-capabilities (:continuation linear
                                           :observation-delivery inherited)
                    :observation-delivery inherited
                    :current-state-fingerprint "state-fingerprint"))
         (promoted
          (e-loop--promote-continuation-candidate
           options candidate 3 '((:role user :content "tool result")))))
    (should (equal promoted options))))

(ert-deftest e-loop-test-replaceable-observation-promotes-linear-anchor ()
  "A request-local replacement can advance a linear provider anchor."
  (let* ((candidate '(:provider-id openai :metadata (:response-id "resp-2")))
         (options '(:provider-continuation t
                    :provider-anchor-provider-id openai
                    :context-capabilities
                    (:continuation linear
                     :observation-delivery request-local-replaceable)
                    :observation-delivery request-local-replaceable
                    :current-state-fingerprint "state-fingerprint"))
         (promoted
          (e-loop--promote-continuation-candidate
           options candidate 3 '((:role user :content "tool result")))))
    (should (equal (plist-get (plist-get promoted :provider-anchor)
                              :metadata)
                   '(:response-id "resp-2")))
    (should (equal (plist-get promoted :provider-anchor-delta-messages)
                   '((:role user :content "tool result"))))))

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

(ert-deftest e-loop-test-attaches-provider-replay-items-to-assistant-message ()
  "Opaque provider replay items persist with the output they precede."
  (let* ((replay-item
          '(:type provider-replay-item
            :provider-id openai
            :item (:type "reasoning" :encrypted_content "ciphertext")))
         (backend (e-backend-fake-create
                   :items (list replay-item
                                '(:type assistant-message :content "hello")
                                '(:type done :reason stop))))
         messages)
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options '(:model "fake")
     :on-event #'ignore
     :append-message (lambda (message) (push message messages)))
    (let ((assistant (car messages)))
      (should (eq (plist-get assistant :role) 'assistant))
      (should (equal
               (plist-get (plist-get assistant :metadata)
                          :provider-replay-items)
               (list replay-item))))))

(ert-deftest e-loop-test-attaches-provider-replay-items-to-tool-call ()
  "Opaque provider replay items persist before their following tool call."
  (let* ((calls 0)
         (replay-item
          '(:type provider-replay-item
            :provider-id openai
            :item (:type "reasoning" :encrypted_content "ciphertext")))
         (backend
          (e-backend-create
           :name "provider-replay-tool"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item replay-item)
                    (funcall on-item
                             '(:type tool-call
                               :id "call-1"
                               :name "echo"
                               :arguments (:stated_purpose "Echo text."
                                           :text "hi")))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item '(:type assistant-message :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools (e-tools-registry-create))
         messages)
    (e-tools-test-register tools
                           :name "echo"
                           :description "Echo text."
                           :handler (lambda (arguments)
                                      (plist-get arguments :text)))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools tools
     :options '(:model "fake")
     :on-event #'ignore
     :append-message (lambda (message)
                       (setq messages (append messages (list message)))))
    (let* ((tool-message
            (seq-find (lambda (message)
                        (eq (plist-get message :role) 'tool-call))
                      messages))
           (tool-call (plist-get tool-message :content)))
      (should (equal (plist-get tool-call :provider-replay-items)
                     (list replay-item))))))

(ert-deftest e-loop-test-passes-context-segments-to-backend-options ()
  "Derived context segments reach adapters without becoming session state."
  (let* ((segments '((:kind static-prefix
                      :id stable
                      :fingerprint "stable-fp"
                      :messages ((:role system :content "stable")))
                     (:kind current-state
                      :id dynamic
                      :fingerprint "dynamic-fp"
                      :messages ((:role system :content "dynamic")))))
         captured-options
         (backend
          (e-backend-create
           :name "segment-capture"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages)
              (setq captured-options options)
              (funcall on-item '(:type assistant-message :content "done"))
              (funcall on-item '(:type done :reason stop)))))))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role system :content "stable")
                 (:role system :content "dynamic")
                 (:role user :content "hello"))
     :backend backend
     :tools (e-tools-registry-create)
     :options '(:model "fake")
     :segments segments
     :on-event #'ignore
     :append-message #'ignore)
    (should (equal (plist-get captured-options :segments) segments))))

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
      (should (stringp (plist-get started-payload :provider-request-id)))
      (should (= (plist-get started-payload :provider-request-ordinal) 1))
      (should (equal (plist-get started-payload :provider-request-id)
                     (plist-get finished-payload :provider-request-id)))
      (should-not (plist-member started-payload :request-shape))
      (should-not (plist-member finished-payload :request-shape))
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
         (payload (e-loop--request-lifecycle-payload request 'started "request-1" 1 nil))
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
  "Reasoning and cheap tool events preserve their real lifecycle order."
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
                                               :arguments (:stated_purpose "Echo text."
                                                           :text "hi")))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
         (tools (e-tools-registry-create))
         (events nil))
    (e-tools-test-register tools
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
                       tool-finished
                       provider-request-finished
                       provider-request-started
                       provider-request-finished
                       turn-finished))))))

(ert-deftest e-loop-test-followup-request-carries-tool-cause ()
  "The request induced by a tool result has a stable cause join."
  (let* ((calls 0)
         (backend
          (e-backend-create
           :name "cause-followup"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item
                             '(:type tool-call :id "marker-call"
                               :name "process_marker"
                               :arguments (:stated_purpose "Mark success."
                                           :signal "success" :note "ok")))
                    (funcall on-item
                             '(:type token-usage :usage (:input-tokens 10)))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item
                         '(:type token-usage :usage (:input-tokens 20)))
                (funcall on-item
                         '(:type assistant-message :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools (e-tools-registry-create))
         events)
    (e-tools-test-register tools :name "process_marker" :description "mark"
                      :handler (lambda (_arguments) "ok"))
    (e-loop-run-turn-batch
     :session-id "session-1" :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend :tools tools
     :options '(:tools ((:name "process_marker")))
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message #'ignore)
    (let* ((ordered (nreverse events))
           (started (seq-filter
                     (lambda (event)
                       (eq (plist-get event :type) 'provider-request-started))
                     ordered))
           (usage (seq-filter
                   (lambda (event)
                     (eq (plist-get event :type) 'token-usage))
                   ordered))
           (followup (plist-get (cadr started) :payload))
           (followup-usage (plist-get (cadr usage) :payload)))
      (should (equal (plist-get followup :caused-by-tool-call-id)
                     "marker-call"))
      (should (equal (plist-get followup :caused-by-tool-name)
                     "process_marker"))
      (should (equal (plist-get followup-usage :caused-by-tool-call-id)
                     "marker-call"))
      (should (equal (plist-get followup-usage :provider-request-id)
                     (plist-get followup :provider-request-id))))))

(ert-deftest e-loop-test-followup-request-carries-all-tool-causes ()
  "A marker remains a cause when another tool finishes in the same response."
  (let* ((calls 0)
         (backend
          (e-backend-create
           :name "multi-cause-followup"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item
                             '(:type tool-call :id "marker-call"
                               :name "process_marker"
                               :arguments (:stated_purpose "Mark progress.")))
                    (funcall on-item
                             '(:type tool-call :id "echo-call"
                               :name "echo"
                               :arguments (:stated_purpose "Echo text.")))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item '(:type token-usage :usage (:input-tokens 20)))
                (funcall on-item '(:type assistant-message :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools (e-tools-registry-create))
         events)
    (dolist (name '("process_marker" "echo"))
      (e-tools-test-register tools :name name :description name
                        :handler (lambda (_arguments) "ok")))
    (e-loop-run-turn-batch
     :session-id "session-1" :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend :tools tools
     :options '(:tools ((:name "process_marker") (:name "echo")))
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message #'ignore)
    (let* ((started
            (seq-filter
             (lambda (event)
               (eq (plist-get event :type) 'provider-request-started))
             (nreverse events)))
           (followup (plist-get (cadr started) :payload))
           (causes (plist-get followup :caused-by-tool-calls)))
      (should
       (equal (append causes nil)
              '((:id "marker-call" :name "process_marker")
                (:id "echo-call" :name "echo")))))))

(ert-deftest e-loop-test-normalizes-tool-call-backend-item-spelling ()
  "Provider tool_call items follow the backend-neutral tool-call path."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "fake-tool-call-spelling"
                   :stream (cl-function
                            (lambda (&key messages options on-item)
                              (ignore messages options)
                              (setq calls (1+ calls))
                              (if (= calls 1)
                                  (progn
                                    (funcall on-item
                                             '(:type tool_call
                                               :id "call-1"
                                               :name "echo"
                                               :arguments (:stated_purpose "Echo text."
                                                           :text "hi")))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
         (tools (e-tools-registry-create))
         (appended nil)
         (events nil))
    (e-tools-test-register tools
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
     :append-message (lambda (message)
                       (push message appended)))
    (let ((tool-call (cl-find 'tool-call appended
                              :key (lambda (message)
                                     (plist-get message :role)))))
      (should (member 'tool-started
                      (mapcar (lambda (event) (plist-get event :type))
                              events)))
      (should (equal (plist-get (plist-get tool-call :content) :type)
                     'tool-call))
      (should (equal (plist-get (plist-get tool-call :content) :name)
                     "echo")))))

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
                                               :arguments (:stated_purpose "Echo text."
                                                           :text "hi")))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
         (tools (e-tools-registry-create))
         (events nil))
    (e-tools-test-register tools
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

(ert-deftest e-loop-test-invalid-stated-purpose-is-bounded-tool-error ()
  "A missing or invalid purpose becomes a bounded tool error and settles."
  (dolist (arguments '((:text "hi")
                       (:stated_purpose "SECRET\npurpose" :text "hi")))
    (let* ((calls 0)
           (handler-called nil)
           (backend
            (e-backend-create
             :name "invalid-stated-purpose"
             :stream
             (cl-function
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
                               (list :type 'tool-call
                                     :id "invalid-purpose-call"
                                     :name "echo"
                                     :arguments arguments))
                      (funcall on-item
                               '(:type done :reason tool-use)))
                  (should (equal (mapcar (lambda (message)
                                           (plist-get message :role))
                                         messages)
                                 '(user tool-call tool)))
                  (funcall on-item
                           '(:type assistant-message :content "settled"))
                  (funcall on-item '(:type done :reason stop)))))))
           (tools (e-tools-registry-create))
           (messages nil)
           (events nil))
      (e-tools-test-register
       tools
       :name "echo"
       :description "Echo text."
       :handler (lambda (_arguments)
                  (setq handler-called t)
                  "should-not-run"))
      (let ((result
             (e-loop-run-turn-batch
              :session-id "session-invalid-purpose"
              :turn-id "turn-invalid-purpose"
              :messages '((:role user :content "hi"))
              :backend backend
              :tools tools
              :options nil
              :on-event (lambda (type payload)
                          (push (list :type type :payload payload) events))
              :append-message (lambda (message)
                                (setq messages
                                      (append messages (list message)))))))
        (should (equal (plist-get result :status) 'done))
        (should (= calls 2))
        (should-not handler-called)
        (let* ((tool-call
                (cl-find 'tool-call messages
                         :key (lambda (message) (plist-get message :role))))
               (tool-result
                (cl-find 'tool messages
                         :key (lambda (message) (plist-get message :role))))
               (call-content (plist-get tool-call :content))
               (result-content (plist-get tool-result :content)))
          (should (eq (plist-get call-content :type) 'tool-call))
          (should (eq (plist-get (plist-get call-content :metadata)
                                :purpose-status)
                      'invalid))
          (should-not (plist-member (plist-get call-content :arguments)
                                    :stated_purpose))
          (should-not (string-match-p
                       (regexp-quote "SECRET")
                       (prin1-to-string messages)))
          (should (equal (plist-get result-content :tool-call-id)
                         "invalid-purpose-call"))
          (should (equal (plist-get result-content :name) "echo"))
          (should (eq (plist-get result-content :status) 'error))
          (should (eq (plist-get (plist-get result-content :metadata) :error)
                      'e-tools-invalid-stated-purpose)))
        (should (equal (plist-get (car (last messages)) :content)
                       "settled"))
        (should (memq 'turn-finished
                      (mapcar (lambda (event) (plist-get event :type))
                              events)))))))

(ert-deftest e-loop-test-unexpected-call-preparation-error-surfaces ()
  "An internal preparation defect fails the turn instead of becoming rejection."
  (let* ((backend
          (e-backend-fake-create
           :items '((:type tool-call
                     :id "defective-call"
                     :name "echo"
                     :arguments (:stated_purpose "Exercise preparation."
                                 :text "hi"))
                    (:type done :reason tool-use))))
         (tools (e-tools-registry-create))
         (handler-called nil)
         (messages nil))
    (e-tools-test-register
     tools
     :name "echo"
     :description "Echo text."
     :handler (lambda (_arguments)
                (setq handler-called t)
                "should-not-run"))
    (cl-letf (((symbol-function 'e-tools-prepare-call)
               (lambda (&rest _arguments)
                 (error "synthetic preparation defect"))))
      (let ((err
             (should-error
              (e-loop-run-turn-batch
               :session-id "session-preparation-defect"
               :turn-id "turn-preparation-defect"
               :messages '((:role user :content "hi"))
               :backend backend
               :tools tools
               :options nil
               :on-event #'ignore
               :append-message (lambda (message)
                                 (push message messages))))))
        (should (string-match-p "synthetic preparation defect"
                                (error-message-string err)))))
    (should-not handler-called)
    (should-not messages)))

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
                                               :arguments (:stated_purpose "Refresh context.")))
                                    (funcall on-item
                                             '(:type done :reason tool-use)))
                                (setq second-request-messages messages)
                                (funcall on-item
                                         '(:type assistant-message
                                           :content "done"))
                                (funcall on-item
                                         '(:type done :reason stop)))))))
         (tools (e-tools-registry-create)))
    (e-tools-test-register
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

(ert-deftest e-loop-test-context-refresh-applies-one-atomic-request-projection ()
  "A context refresh updates messages and all request metadata together."
  (let* ((calls 0)
         second-request-messages
         second-request-options
         (initial-segments
          '((:kind current-state
             :messages ((:role system :content "STATE-A")))))
         (refreshed-segments
          '((:kind current-state
             :messages ((:role system :content "STATE-B")))))
         (backend (e-backend-create
                   :name "fake-atomic-refresh"
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (setq calls (1+ calls))
                      (if (= calls 1)
                          (progn
                            (funcall on-item
                                     '(:type tool-call
                                       :id "call-refresh"
                                       :name "refreshing_tool"
                                       :arguments (:stated_purpose "Refresh context.")))
                            (funcall on-item '(:type done :reason tool-use)))
                        (setq second-request-messages messages
                              second-request-options options)
                        (funcall on-item
                                 '(:type assistant-message :content "done"))
                        (funcall on-item '(:type done :reason stop)))))))
         (tools (e-tools-registry-create)))
    (e-tools-test-register
     tools
     :name "refreshing_tool"
     :description "Refresh context."
     :handler
     (lambda (_arguments)
       (e-tools-result-create
        (plist-get (e-tools-current-context) :tool-call)
        'ok
        "refreshed"
        '(:refresh-context t))))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role system :content "STATE-A")
                 (:role user :content "prompt"))
     :backend backend
     :tools tools
     :options '(:state "A")
     :segments initial-segments
     :on-event #'ignore
     :append-message #'ignore
     :refresh-context
     (lambda ()
       (list :messages '((:role system :content "STATE-B")
                         (:role user :content "prompt")
                         (:role tool :content "refreshed"))
             :options '(:state "B"
                        :observation-delivery request-local-replaceable
                        :current-state-fingerprint "fp-b")
             :segments refreshed-segments
             :observation-frontier
             '(:delivery request-local-replaceable
               :fingerprint "fp-b"))))
    (should (= calls 2))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           second-request-messages)
                   '("STATE-B" "prompt" "refreshed")))
    (should (equal (plist-get second-request-options :state) "B"))
    (should (equal (plist-get (car (plist-get second-request-options :segments))
                             :messages)
                   '((:role system :content "STATE-B"))))
    (should (equal (plist-get second-request-options
                              :current-state-fingerprint)
                   "fp-b"))))

(ert-deftest e-loop-test-refresh-fences-candidate-after-stable-projection-change ()
  "A stale response candidate cannot overwrite a refreshed stable decision."
  (let* ((calls 0)
         second-request-messages
         second-request-options
         (backend (e-backend-create
                   :name "fake-refresh-fence"
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (setq calls (1+ calls))
                      (if (= calls 1)
                          (progn
                            (funcall on-item
                                     '(:type provider-anchor-candidate
                                       :provider-id openai
                                       :metadata (:response-id "stale-candidate")))
                            (funcall on-item
                                     '(:type tool-call
                                       :id "call-refresh-fence"
                                       :name "refreshing_tool"
                                       :arguments (:stated_purpose "Refresh stable projection.")))
                            (funcall on-item '(:type done :reason tool-use)))
                        (setq second-request-messages messages
                              second-request-options options)
                        (funcall on-item
                                 '(:type assistant-message :content "done"))
                        (funcall on-item '(:type done :reason stop)))))))
         (tools (e-tools-registry-create)))
    (e-tools-test-register
     tools
     :name "refreshing_tool"
     :description "Refresh stable projection."
     :handler
     (lambda (_arguments)
       (e-tools-result-create
        (plist-get (e-tools-current-context) :tool-call)
        'ok
        "refreshed"
        '(:refresh-context t))))
    (e-loop-run-turn-batch
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role system :content "STABLE-A")
                 (:role system :content "STATE-A")
                 (:role user :content "prompt"))
     :backend backend
     :tools tools
     :options '(:provider-continuation t
                :provider-anchor-provider-id openai
                :context-capabilities
                (:continuation linear
                 :observation-delivery request-local-replaceable)
                :observation-delivery request-local-replaceable
                :provider-anchor
                (:provider-id openai
                 :metadata (:response-id "prior-anchor"))
                :continuation-projection-identity (:stable-prefix "A"))
     :segments '((:kind stable-context
                  :messages ((:role system :content "STABLE-A")))
                 (:kind current-state
                  :messages ((:role system :content "STATE-A"))))
     :on-event #'ignore
     :append-message #'ignore
     :refresh-context
     (lambda ()
       (list :messages '((:role system :content "STABLE-B")
                         (:role system :content "STATE-B")
                         (:role user :content "prompt")
                         (:role tool :content "refreshed"))
             :options '(:provider-continuation t
                        :provider-anchor-provider-id openai
                        :context-capabilities
                        (:continuation linear
                         :observation-delivery request-local-replaceable)
                        :observation-delivery request-local-replaceable
                        :provider-anchor
                        (:provider-id openai
                         :metadata (:response-id "fresh-anchor"))
                        :context-rendering-strategy stateless
                        :continuation-projection-identity
                        (:stable-prefix "B"))
             :segments '((:kind stable-context
                          :messages ((:role system :content "STABLE-B")))
                         (:kind current-state
                          :messages ((:role system :content "STATE-B"))))
             :observation-frontier
             '(:delivery request-local-replaceable
               :fingerprint "state-b"))))
    (should (= calls 2))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           second-request-messages)
                   '("STABLE-B" "STATE-B" "prompt" "refreshed")))
    (should (equal (plist-get (plist-get second-request-options
                                          :provider-anchor)
                              :metadata)
                   '(:response-id "fresh-anchor")))
    (should (eq (plist-get second-request-options
                           :context-rendering-strategy)
                'stateless))))

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
                                               :arguments (:stated_purpose "Echo raw text."
                                                           :text "raw")))
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
                                               :arguments (:stated_purpose "Echo text.")))
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
                                               :arguments (:stated_purpose "Echo text.")))
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
                                               :arguments (:stated_purpose "Echo text."
                                                           :text "hi")))
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
    (e-tools-test-register tools
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
                                               :arguments (:stated_purpose "Echo text."
                                                           :text "hi")))
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
    (e-tools-test-register tools
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
                                          :arguments (:stated_purpose "Echo text."
                                                      :text "hi")))
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
    (e-tools-test-register tools
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

(ert-deftest e-loop-test-start-turn-requeries-after-tool-settles-during-start ()
  "A request reported after synchronous tool completion must not wedge follow-up."
  (let* ((calls 0)
         (backend (e-backend-create
                   :name "immediate-tool-followup"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore options on-error on-request-start)
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
                                       :name "immediate"
                                       :arguments (:stated_purpose "Return one."
                                                   :text "one")))
                            (funcall on-item
                                     '(:type tool-call
                                       :id "call-2"
                                       :name "immediate"
                                       :arguments (:stated_purpose "Return two."
                                                   :text "two")))
                            (funcall on-item
                                     '(:type done :reason tool-use)))
                        (should (equal (mapcar (lambda (message)
                                                 (plist-get message :role))
                                               messages)
                                       '(user tool-call tool
                                              tool-call tool)))
                        (funcall on-item
                                 '(:type assistant-message
                                   :content "final answer"))
                        (funcall on-item '(:type done :reason stop)))
                      (funcall on-done '(:status done))
                      nil))))
         (tools (e-tools-registry-create))
         (events nil)
         (messages nil)
         (settled nil))
    (e-tools-test-register tools
                      :name "immediate"
                      :description "Return while starting."
                      :start
                      (cl-function
                       (lambda (&key arguments on-done on-error
                                      on-request-start)
                         (ignore on-error on-request-start)
                         (funcall on-done (plist-get arguments :text))
                         (e-tools-request-create
                          :metadata '(:transport immediate)))))
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
                   '(tool-call tool tool-call tool assistant)))
    (should (equal (mapcar (lambda (event) (plist-get event :type))
                           (nreverse events))
                   '(turn-started
                     tool-started tool-finished
                     tool-started tool-finished
                     turn-finished)))))

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
                                          :arguments (:stated_purpose "Handle quit.")))
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
    (e-tools-test-register tools
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
                                          :arguments (:stated_purpose "Return later."
                                                      :text "hi")))
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
    (e-tools-test-register tools
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
                                          :arguments (:stated_purpose "Return first."
                                                      :text "first")))
                               (funcall on-item
                                        '(:type tool-call
                                          :id "call-2"
                                          :name "later"
                                          :arguments (:stated_purpose "Return second."
                                                      :text "second")))
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
    (e-tools-test-register tools
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

(ert-deftest e-loop-test-backend-deadline-settles-stalled-provider-request ()
  "A provider request that never calls back settles through the Work deadline."
  (let* ((cancelled nil)
         (deadline (+ (float-time) 0.02))
         (backend
          (e-backend-create
           :name "stalled-provider"
           :start
           (cl-function
            (lambda (&key on-request-start &allow-other-keys)
              (let ((request
                     (e-backend-request-create
                      :cancel (lambda ()
                                (setq cancelled t)
                                t)
                      :metadata '(:provider fake :transport timer))))
                (funcall on-request-start request)
                request)))))
         (events nil)
         (settled nil))
    (e-loop-start-turn
     :session-id "session-1"
     :turn-id "turn-1"
     :messages '((:role user :content "hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options (list :deadline deadline)
     :on-event (lambda (type payload)
                 (push (list :type type :payload payload) events))
     :append-message (lambda (&rest _args))
     :on-done (lambda (result)
                (setq settled result))
     :on-error (lambda (err)
                 (setq settled (list :error err))))
    (should (e-loop-test--wait-until (lambda () settled) 1))
    (should cancelled)
    (should (eq (car (plist-get settled :error))
                'e-work-deadline-exceeded))
    (let* ((ordered-events (nreverse events))
           (types (mapcar (lambda (event) (plist-get event :type))
                          ordered-events))
           (started (cl-find 'provider-request-started ordered-events
                             :key (lambda (event)
                                    (plist-get event :type))))
           (finished (cl-find 'provider-request-finished ordered-events
                              :key (lambda (event)
                                     (plist-get event :type)))))
      (should (equal types
                     '(turn-started
                       provider-request-started
                       provider-request-finished)))
      (should (numberp (plist-get (plist-get started :payload) :deadline)))
      (should (eq (plist-get (plist-get finished :payload) :status)
                  'error)))))

(ert-deftest e-loop-test-context-curation-stays-out-of-tool-queue ()
  "The reserved curation carrier is consumed by the loop, not dispatched."
  (let* ((frame
          (e-context-lifetime-frame-create
           :id "frame-loop"
           :generation-id "generation-loop"
           :consumer-request-id "consumer-loop"
           :observations
           '((:observation-id "observation-loop"
              :kind "current-state"
              :source-entry-ref "external:canvas:loop"
              :source-fingerprint "canvas-loop"
              :effective-delivery "request-local-replaceable"
              :body (:content "canvas")))))
         (backend
          (e-backend-create
           :name "curation-carrier"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall
               on-item
               '(:type context-curate
                 :arguments (:keep (1) :summaries nil)))
              (funcall on-item
                       '(:type assistant-message :content "answer"))
              (funcall on-item '(:type done :reason stop))))))
         (messages nil)
         (curation-effects nil))
    (e-loop-run-turn-batch
     :session-id "session-loop"
     :turn-id "turn-loop"
     :messages '((:role user :content "prompt"))
     :backend backend
     :tools (e-tools-registry-create)
     :options
     '(:model "fake"
       :context-lifetime-enabled t
       :context-capabilities
       (:continuation none
        :observation-delivery request-local-replaceable
        :reserved-effect-carrier context-curate-wire))
     :lifetime-frame frame
     :on-response-complete
     (lambda (payload)
       (setq curation-effects (plist-get payload :curation-effects)))
     :on-event #'ignore
     :append-message (lambda (message)
                       (setq messages (append messages (list message)))))
    (should (= (length curation-effects) 1))
    (should (equal (plist-get (car curation-effects) :arguments)
                   '(:keep (1) :summaries nil :erase nil)))
    (should-not (seq-find (lambda (message)
                            (eq (plist-get message :role) 'tool-call))
                          messages))
    (should (equal (plist-get (car messages) :content) "answer"))))

(ert-deftest e-loop-test-candidate-before-curation-stays-immediate-only ()
  "A candidate cannot become durable when later output curates the response."
  (let* ((events nil)
         (backend
          (e-backend-create
           :name "candidate-before-curation"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              ;; Provider item order is not an ownership guarantee.  The
              ;; completed response, rather than candidate arrival time,
              ;; decides whether its state is safe to persist.
              (funcall on-item
                       '(:type provider-anchor-candidate
                         :provider-id fake
                         :metadata (:response-id "response-curated")))
              (funcall on-item
                       '(:type context-curate
                         :arguments (:keep nil :summaries nil :erase nil)))
              (funcall on-item
                       '(:type assistant-message :content "answer"))
              (funcall on-item '(:type done :reason stop)))))))
    (e-loop-run-turn-batch
     :session-id "session-candidate-before-curation"
     :turn-id "turn-candidate-before-curation"
     :messages '((:role user :content "prompt"))
     :backend backend
     :tools (e-tools-registry-create)
     :options '(:model "fake"
                :provider-continuation t
                :provider-anchor-provider-id fake
                :context-capabilities
                (:continuation linear
                 :observation-delivery request-local-replaceable))
     :on-event
     (lambda (type payload)
       (push (list :type type :payload payload) events))
     :append-message #'ignore)
    (let ((candidate
           (plist-get
            (seq-find
             (lambda (event)
               (eq (plist-get event :type) 'provider-anchor-candidate))
             events)
            :payload)))
      (should candidate)
      (should (plist-get candidate :immediate-followup-only))
      (should-not (plist-get candidate :accepted-for-persistence)))))

(ert-deftest e-loop-test-curation-only-response-continues-with-opaque-ack ()
  "A reserved-only response commits, acknowledges, then permits one answer."
  (let* ((request-count 0)
         (requests nil)
         (started-tools nil)
         (curation-payloads nil)
         (backend
          (e-backend-create
           :name "curation-only-followup"
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (push (list :messages (copy-tree messages)
                          :options (copy-tree options))
                    requests)
              (setq request-count (1+ request-count))
              (pcase request-count
                (1
                 (funcall on-item
                          '(:type tool-call :id "inspect-call"
                            :name "inspect" :arguments nil))
                 (funcall on-item
                          (list :type 'provider-anchor-candidate
                                :provider-id 'openai
                                :metadata
                                (list
                                 :response-id "response-tool"
                                 :prompt-layout-revision
                                 (e-openai-responses-prompt-layout-revision options)
                                 :reasoning-identity
                                 (e-openai-responses-reasoning-identity options))))
                 (funcall on-item '(:type done :reason tool-use)))
                (2
                 (funcall on-item
                          '(:type context-curate
                            :arguments (:keep (1) :summaries nil)
                            :provider-replay-item
                            (:type provider-replay-item
                             :provider-id openai
                             :item (:type "function_call_output"
                                    :call_id "curation-call"
                                    :output ""))))
                 (funcall on-item
                          (list :type 'provider-anchor-candidate
                                :provider-id 'openai
                                :metadata
                                (list
                                 :response-id "response-curation"
                                 :prompt-layout-revision
                                 (e-openai-responses-prompt-layout-revision options)
                                 :reasoning-identity
                                 (e-openai-responses-reasoning-identity options))))
                 (funcall on-item '(:type done :reason stop)))
                (3
                 (funcall on-item
                          '(:type assistant-message :content "answer"))
                 (funcall on-item '(:type done :reason stop))))))))
         (tool-lifecycle
          (e-tool-lifecycle-create
           :start
           (cl-function
            (lambda (tool-call &key on-done &allow-other-keys)
              (push (plist-get tool-call :name) started-tools)
              (funcall on-done
                       (list :tool-call-id (plist-get tool-call :id)
                             :name (plist-get tool-call :name)
                             :status 'ok :content "inspected"))
              nil))))
         (durable-messages nil)
         (options '(:model "gpt-test"
                    :provider-continuation t
                    :provider-anchor-provider-id openai
                    :context-lifetime-enabled t
                    :context-capabilities
                    (:continuation linear
                     :observation-delivery request-local-replaceable
                     :reserved-effect-carrier context-curate-wire))))
    (e-loop-run-turn-batch
     :session-id "session-curation-only"
     :turn-id "turn-curation-only"
     :messages '((:role user :content "inspect"))
     :backend backend
     :tools (e-tools-registry-create)
     :tool-lifecycle tool-lifecycle
     :options options
     :on-response-complete
     (lambda (payload)
       (when (plist-get payload :curation-effects)
         (push payload curation-payloads)))
     :on-event #'ignore
     :append-message
     (lambda (message)
       (setq durable-messages
             (append durable-messages (list (copy-tree message))))))
    (should (= request-count 3))
    (should (equal started-tools '("inspect")))
    (should (= (length curation-payloads) 1))
    (let* ((third (nth 2 (nreverse requests)))
           (body (e-openai-codex-request-body
                  :messages (plist-get third :messages)
                  :options (plist-get third :options)
                  :tools nil))
           (input (append (plist-get body :input) nil))
           (ack (seq-find (lambda (item)
                           (and (equal (plist-get item :type)
                                       "function_call_output")
                                (equal (plist-get item :call_id)
                                       "curation-call")))
                         input)))
      (should (equal (plist-get body :previous_response_id)
                     "response-curation"))
      (should ack)
      (should (equal (plist-get ack :call_id) "curation-call"))
      (should (equal (plist-get ack :output) "")))
    (should-not (string-match-p "function_call_output"
                                (prin1-to-string durable-messages)))
    (should (equal (plist-get (car (last durable-messages)) :content)
                   "answer"))))

(ert-deftest e-loop-test-duplicate-curation-recovers-once-then-uses-empty-output ()
  "A closed opportunity receives one correction, then ordinary empty-output."
  (let* ((request-count 0)
         (completion-count 0)
         (requests nil)
         (events nil)
         (frame
          (e-context-lifetime-frame-create
           :id "frame-duplicate" :generation-id "generation-duplicate"
           :consumer-request-id "consumer-duplicate"
           :observations
           '((:observation-id "observation-duplicate"
              :kind "current-state"
              :source-entry-ref "external:duplicate:1"
              :source-fingerprint "duplicate-fingerprint"
              :effective-delivery "request-local-replaceable"
              :body (:role "user" :content "DUPLICATE-SOURCE")))))
         (backend
          (e-backend-create
           :name "repeated-curation-only"
           :stream
           (cl-function
            (lambda (&key options on-item &allow-other-keys)
              (setq requests (append requests (list (copy-tree options))))
              (setq request-count (1+ request-count))
              (funcall on-item
                       (e-openai-decoder--context-curation-effect
                        '(:keep nil :summaries nil :erase nil)
                        (format "curation-call-%d" request-count)))
              (funcall on-item
                       (list :type 'provider-anchor-candidate
                             :provider-id 'openai
                             :metadata
                             (list :response-id
                                   (format "response-curation-%d"
                                           request-count))))
              (funcall on-item '(:type done :reason stop)))))))
    (let ((error
           (should-error
            (e-loop-run-turn-batch
             :session-id "session-repeated-curation"
             :turn-id "turn-repeated-curation"
             :messages '((:role tool
                          :content (:tool-call-id "inspect-call"
                                    :name "inspect" :content "inspected")))
             :backend backend
             :tools (e-tools-registry-create)
             :options '(:model "fake"
                        :context-lifetime-enabled t
                        :reserved-effect-carrier context-curate-wire
                        :context-capabilities
                        (:continuation none
                         :observation-delivery inherited
                         :reserved-effect-carrier context-curate-wire))
             :lifetime-frame frame
             :on-response-complete
             (lambda (payload)
               (when (plist-get payload :curation-effects)
                 (setq completion-count (1+ completion-count))
                 (e-context-lifetime-frame-complete-for-consumer
                  (plist-get payload :frame)
                  "consumer-duplicate"
                  (or (plist-get payload :response-entry-id)
                      "response-entry-duplicate"))))
             :on-event
             (lambda (type payload)
               (push (list :type type :payload payload) events))
             :append-message #'ignore)
            :type 'e-loop-empty-output)))
      (should-not (eq (car-safe error) 'e-context-lifetime-invalid-record)))
    (should (= request-count 3))
    (should (= completion-count 1))
    (should (= (seq-count
                (lambda (event)
                  (eq (plist-get event :type)
                      'context-curation-duplicate-ignored))
                events)
               1))
    (should (eq (plist-get (nth 0 requests) :reserved-effect-carrier)
                'context-curate-wire))
    (should-not (plist-get (nth 1 requests) :reserved-effect-carrier))
    (should-not (plist-get (nth 2 requests) :reserved-effect-carrier))
    (let* ((third-body
            (e-openai-codex-request-body
             :messages nil :options (nth 2 requests) :tools nil))
           (correction
            (seq-find
             (lambda (item)
               (and (equal (plist-get item :type) "function_call_output")
                    (equal (plist-get item :call_id) "curation-call-2")))
             (append (plist-get third-body :input) nil))))
      (should correction)
      (should (equal
               (plist-get correction :output)
               e-openai-decoder--context-curation-duplicate-correction)))))

(ert-deftest e-loop-test-duplicate-curation-retains-frame-bound-validation ()
  "A closed opportunity does not turn an invalid disposition into recovery."
  (let* ((request-count 0)
         (completion-count 0)
         (events nil)
         (frame
          (e-context-lifetime-frame-create
           :id "frame-strict-duplicate"
           :generation-id "generation-strict-duplicate"
           :consumer-request-id "consumer-strict-duplicate"
           :observations
           '((:observation-id "observation-strict-duplicate"
              :kind "current-state"
              :source-entry-ref "external:strict:1"
              :source-fingerprint "strict-fingerprint"
              :effective-delivery "request-local-replaceable"
              :body (:role "user" :content "STRICT-SOURCE")))))
         (backend
          (e-backend-create
           :name "strict-duplicate-curation"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (setq request-count (1+ request-count))
              (funcall on-item
                       (e-openai-decoder--context-curation-effect
                        (list :keep (list request-count)
                              :summaries nil :erase nil)
                        (format "strict-curation-%d" request-count)))
              (funcall on-item '(:type done :reason stop)))))))
    (should-error
     (e-loop-run-turn-batch
      :session-id "session-strict-duplicate"
      :turn-id "turn-strict-duplicate"
      :messages '((:role user :content "curate once"))
      :backend backend
      :tools (e-tools-registry-create)
      :options '(:model "fake"
                 :context-lifetime-enabled t
                 :reserved-effect-carrier context-curate-wire)
      :lifetime-frame frame
      :on-response-complete
      (lambda (payload)
        (when (plist-get payload :curation-effects)
          (setq completion-count (1+ completion-count))
          (e-context-lifetime-frame-complete-for-consumer
           (plist-get payload :frame)
           "consumer-strict-duplicate"
           (plist-get payload :response-entry-id))))
      :on-event
      (lambda (type payload)
        (push (list :type type :payload payload) events))
      :append-message #'ignore)
     :type 'e-context-lifetime-invalid-record)
    (should (= request-count 2))
    (should (= completion-count 1))
    (should-not
     (seq-find
      (lambda (event)
        (eq (plist-get event :type)
            'context-curation-duplicate-ignored))
      events))))

(ert-deftest e-loop-test-curation-opportunity-is-scoped-to-each-new-frame ()
  "Two tool-created frames each admit one curation in the same turn."
  (let* ((request-count 0)
         (tool-count 0)
         (completion-frames nil)
         (requests nil)
         (messages nil)
         (backend
          (e-backend-create
           :name "multi-frame-curation"
           :stream
           (cl-function
            (lambda (&key options on-item &allow-other-keys)
              (setq requests (append requests (list (copy-tree options))))
              (setq request-count (1+ request-count))
              (pcase request-count
                ((or 1 3)
                 (funcall on-item
                          (list :type 'tool-call
                                :id (format "tool-%d" request-count)
                                :name "inspect" :arguments nil))
                 (funcall on-item '(:type done :reason tool-use)))
                ((or 2 4)
                 (funcall on-item
                          (e-openai-decoder--context-curation-effect
                           '(:keep (1) :summaries nil :erase nil)
                           (format "curation-%d" request-count)))
                 (funcall on-item '(:type done :reason stop)))
                (5
                 (funcall on-item
                          '(:type assistant-message :content "multi-frame answer"))
                 (funcall on-item '(:type done :reason stop)))
                (_ (error "Unexpected request %d" request-count)))))))
         (tool-lifecycle
          (e-tool-lifecycle-create
           :start
           (cl-function
            (lambda (tool-call &key on-done &allow-other-keys)
              (setq tool-count (1+ tool-count))
              (funcall on-done
                       (list :tool-call-id (plist-get tool-call :id)
                             :name "inspect" :status 'ok
                             :content (format "result-%d" tool-count)))
              nil)))))
    (e-loop-run-turn-batch
     :session-id "session-multi-frame"
     :turn-id "turn-multi-frame"
     :messages '((:role user :content "inspect twice"))
     :backend backend
     :tools (e-tools-registry-create)
     :tool-lifecycle tool-lifecycle
     :options '(:model "fake"
                :context-lifetime-enabled t
                :reserved-effect-carrier context-curate-wire
                :context-capabilities
                (:continuation none
                 :observation-delivery inherited
                 :reserved-effect-carrier context-curate-wire))
     :on-tool-observation
     (lambda (_payload)
       (let ((index tool-count))
         (e-context-lifetime-frame-create
          :id (format "frame-%d" index)
          :generation-id "generation-multi-frame"
          :consumer-request-id (format "consumer-%d" index)
          :observations nil)))
     :on-response-complete
     (lambda (payload)
       (when (plist-get payload :curation-effects)
         (let ((frame (plist-get payload :frame)))
           (setq completion-frames
                 (append completion-frames
                         (list (e-context-lifetime-frame-id frame))))
           (e-context-lifetime-frame-complete-for-consumer
            frame
            (e-context-lifetime-frame-consumer-request-id frame)
            (or (plist-get payload :response-entry-id)
                (format "response-%d" request-count))))))
     :on-event #'ignore
     :append-message
     (lambda (message) (setq messages (append messages (list message)))))
    (should (= request-count 5))
    (should (= tool-count 2))
    (should (equal completion-frames '("frame-1" "frame-2")))
    (should (equal (plist-get (car (last messages)) :content)
                   "multi-frame answer"))
    (should-not (plist-get (nth 0 requests) :reserved-effect-carrier))
    (should (eq (plist-get (nth 1 requests) :reserved-effect-carrier)
                'context-curate-wire))
    (should-not (plist-get (nth 2 requests) :reserved-effect-carrier))
    (should (eq (plist-get (nth 3 requests) :reserved-effect-carrier)
                'context-curate-wire))
    (should-not (plist-get (nth 4 requests) :reserved-effect-carrier))))

(ert-deftest e-loop-test-stateless-curation-replays-call-and-ack-only-once ()
  "Fresh stateless curation replays its call/output pair exactly once."
  (let* ((request-count 0)
         (requests nil)
         (curation-count 0)
         (durable-messages nil)
         (backend
          (e-backend-create
           :name "stateless-curation-replay"
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (push (list :messages (copy-tree messages)
                          :options (copy-tree options))
                    requests)
              (setq request-count (1+ request-count))
              (pcase request-count
                (1
                 (dolist
                     (item
                      (e-openai-codex-parse-stream
                       (mapconcat
                        (lambda (event)
                          (format "data: %s\n\n" (json-encode event)))
                        (list
                         '(:type "response.output_item.done"
                           :item (:type "function_call"
                                  :call_id "curation-call"
                                  :name "context-curate"
                                  :arguments "{\"keep\":[1],\"summaries\":[]}"))
                         '(:type "response.completed"
                           :response (:id "response-curation")))
                        "")))
                   (funcall on-item item)))
                (2
                 (funcall on-item
                          '(:type assistant-message :content "answer"))
                 (funcall on-item '(:type done :reason stop))))))))
         (options '(:model "gpt-test"
                    :provider-continuation nil
                    :context-lifetime-enabled t
                    :context-capabilities
                    (:continuation none
                     :observation-delivery inherited
                     :reserved-effect-carrier context-curate-wire))))
    (e-loop-run-turn-batch
     :session-id "session-stateless-curation"
     :turn-id "turn-stateless-curation"
     :messages '((:role user :content "old request")
                 (:role tool-call
                  :content (:id "historical-call"
                            :name "historical-tool"
                            :arguments nil))
                 (:role tool
                  :content (:tool-call-id "historical-call"
                            :name "historical-tool"
                            :status ok
                            :content "historical output"))
                 (:role user :content "Hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options options
     :on-response-complete
     (lambda (payload)
       (when (plist-get payload :curation-effects)
         (setq curation-count (1+ curation-count))))
     :on-event #'ignore
     :append-message
     (lambda (message)
       (setq durable-messages
             (append durable-messages (list (copy-tree message))))))
    (should (= request-count 2))
    (should (= curation-count 1))
    (let* ((second (nth 1 (nreverse requests)))
           (historical-tool
            (seq-find
             (lambda (message)
               (and (eq (plist-get message :role) 'tool)
                    (equal (plist-get (plist-get message :content)
                                      :tool-call-id)
                           "historical-call")))
             (plist-get second :messages)))
           (body (e-openai-codex-request-body
                  :messages (plist-get second :messages)
                  :options (plist-get second :options)
                  :tools nil))
           (input (append (plist-get body :input) nil))
           (call-position
            (cl-position-if
             (lambda (item)
               (and (equal (plist-get item :type) "function_call")
                    (equal (plist-get item :name) "context-curate")
                    (equal (plist-get item :call_id) "curation-call")))
             input))
           (ack-position
            (cl-position-if
             (lambda (item)
               (and (equal (plist-get item :type) "function_call_output")
                    (equal (plist-get item :call_id) "curation-call")
                    (equal (plist-get item :output) "")))
             input))
           (user-position
            (cl-position-if
             (lambda (item)
               (and (equal (plist-get item :role) "user")
                    (equal (plist-get
                            (aref (plist-get item :content) 0)
                            :text)
                           "Hi")))
             input))
           (historical-output-count
            (cl-count-if
             (lambda (item)
               (and (equal (plist-get item :type) "function_call_output")
                    (equal (plist-get item :call_id) "historical-call")))
             input)))
      (should-not (plist-member body :previous_response_id))
      (should historical-tool)
      (should-not (plist-get (plist-get historical-tool :metadata)
                             :provider-replay-items))
      (should (= historical-output-count 1))
      (should (integerp user-position))
      (should (integerp call-position))
      (should (integerp ack-position))
      (should (< user-position call-position))
      (should (< call-position ack-position)))
    (should-not (string-match-p "provider-request-replay-items"
                                (prin1-to-string durable-messages)))
    (let* ((next-body
            (e-openai-codex-request-body
             :messages durable-messages
             :options options
             :tools nil))
           (next-input (append (plist-get next-body :input) nil)))
      (should-not
       (seq-some
        (lambda (item)
          (and (member (plist-get item :type)
                       '("function_call" "function_call_output"))
               (or (equal (plist-get item :name) "context-curate")
                   (equal (plist-get item :call_id) "curation-call"))))
        next-input)))))

(ert-deftest e-loop-test-anchored-first-curation-sends-immediate-ack-only ()
  "Fresh anchored curation sends one request-local acknowledgement delta."
  (let* ((request-count 0)
         (requests nil)
         (durable-messages nil)
         (backend
          (e-backend-create
           :name "anchored-first-curation"
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (push (list :messages (copy-tree messages)
                          :options (copy-tree options))
                    requests)
              (setq request-count (1+ request-count))
              (pcase request-count
                (1
                 (dolist
                     (item
                      (e-openai-codex-parse-stream
                       (mapconcat
                        (lambda (event)
                          (format "data: %s\n\n" (json-encode event)))
                        (list
                         '(:type "response.output_item.done"
                           :item (:type "function_call"
                                  :call_id "curation-call"
                                  :name "context-curate"
                                  :arguments "{\"keep\":[1],\"summaries\":[]}"))
                         '(:type "response.completed"
                           :response (:id "response-curation")))
                        "")
                       nil
                       (e-openai-responses-reasoning-identity options)))
                   (funcall on-item item)))
                (2
                 (funcall on-item
                          '(:type assistant-message :content "answer"))
                 (funcall on-item '(:type done :reason stop))))))))
         (options '(:model "gpt-test"
                    :provider-continuation t
                    :provider-anchor-provider-id openai
                    :context-lifetime-enabled t
                    :context-capabilities
                    (:continuation linear
                     :observation-delivery request-local-replaceable
                     :reserved-effect-carrier context-curate-wire))))
    (e-loop-run-turn-batch
     :session-id "session-anchored-first-curation"
     :turn-id "turn-anchored-first-curation"
     :messages '((:role user :content "old request")
                 (:role tool-call
                  :content (:id "historical-call"
                            :name "historical-tool"
                            :arguments nil))
                 (:role tool
                  :content (:tool-call-id "historical-call"
                            :name "historical-tool"
                            :status ok
                            :content "historical output"))
                 (:role user :content "Hi"))
     :backend backend
     :tools (e-tools-registry-create)
     :options options
     :on-response-complete #'ignore
     :on-event #'ignore
     :append-message
     (lambda (message)
       (setq durable-messages
             (append durable-messages (list (copy-tree message))))))
    (should (= request-count 2))
    (let* ((second (nth 1 (nreverse requests)))
           (historical-tool
            (seq-find
             (lambda (message)
               (and (eq (plist-get message :role) 'tool)
                    (equal (plist-get (plist-get message :content)
                                      :tool-call-id)
                           "historical-call")))
             (plist-get second :messages)))
           (body (e-openai-codex-request-body
                  :messages (plist-get second :messages)
                  :options (plist-get second :options)
                  :tools nil))
           (input (append (plist-get body :input) nil)))
      (should (equal (plist-get body :previous_response_id)
                     "response-curation"))
      (should historical-tool)
      (should-not (plist-get (plist-get historical-tool :metadata)
                             :provider-replay-items))
      (should (= (length input) 1))
      (should (equal (plist-get (car input) :type)
                     "function_call_output"))
      (should (equal (plist-get (car input) :call_id)
                     "curation-call"))
      (should-not
       (seq-find (lambda (item)
                   (equal (plist-get item :type) "function_call"))
                 input)))
    (should (= (length durable-messages) 1))
    (should (equal (plist-get (car durable-messages) :role) 'assistant))
    (should-not (string-match-p "curation-call\|provider-replay"
                                (prin1-to-string durable-messages)))))

(ert-deftest e-loop-test-response-preflight-precedes-assistant-append ()
  "A completion preflight runs before the assistant append callback."
  (let* ((order nil)
         (preflight-id nil)
         (completion-id nil)
         (append-id nil)
         (backend
          (e-backend-create
           :name "response-preflight-order"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall on-item
                       '(:type assistant-message :content "answer"))
              (funcall on-item '(:type done :reason stop)))))))
    (e-loop-run-turn-batch
     :session-id "session-preflight-order"
     :turn-id "turn-preflight-order"
     :messages '((:role user :content "prompt"))
     :backend backend
     :tools (e-tools-registry-create)
     :options '(:model "fake")
     :on-response-preflight
     (lambda (payload)
       (setq preflight-id (plist-get payload :response-entry-id))
       (setq order (append order
                           (list (list 'preflight
                                       (plist-get payload :assistant-content)))))
       'prepared-completion)
     :on-response-complete
     (lambda (payload)
       (setq completion-id (plist-get payload :response-entry-id))
       (setq order (append order
                           (list (list 'complete
                                       (plist-get payload
                                                  :curation-preflight))))))
     :on-event #'ignore
     :append-message
     (lambda (message)
       (setq append-id (plist-get message :id))
       (setq order (append order
                           (list (list 'append
                                       (plist-get message :content)))))))
    (should (stringp preflight-id))
    (should (equal preflight-id completion-id))
    (should (equal preflight-id append-id))
    (should (equal order
                   '((preflight "answer")
                     (append "answer")
                     (complete prepared-completion))))))

(ert-deftest e-loop-test-tool-descendant-frame-survives-response-race ()
  "A tool bundle frame remains current whichever completion callback wins."
  (dolist (provider-first '(t nil))
    (let* ((request-count 0)
           (pending-tool-done nil)
           (captured-requests nil)
           (tool-frame nil)
           (response-frames nil)
           (settled nil)
           (frame-a
            (e-context-lifetime-frame-create
             :id "frame-A"
             :generation-id "generation-race"
             :consumer-request-id "consumer-A"
             :observations
             '((:observation-id "observation-A"
                :kind "current-state"
                :source-entry-ref "external:canvas:A"
                :source-fingerprint "canvas-A"
                :effective-delivery "request-local-replaceable"
                :body (:content "CANVAS-A")))))
           (backend
            (e-backend-create
             :name "tool-descendant-frame-race"
             :start
             (cl-function
              (lambda (&key messages options on-item on-done on-error
                             on-request-start)
                (ignore on-error on-request-start)
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      captured-requests)
                (setq request-count (1+ request-count))
                (if (= request-count 1)
                    (progn
                      (funcall on-item
                               '(:type tool-call
                                 :id "call-race"
                                 :name "race-tool"
                                 :arguments nil))
                      (funcall on-item '(:type done :reason tool-use))
                      (let ((finish
                             (lambda ()
                               (funcall pending-tool-done
                                        '(:tool-call-id "call-race"
                                          :name "race-tool"
                                          :status ok
                                          :content "BUNDLE-RESULT")))))
                        (if provider-first
                            (progn
                              (funcall on-done '(:status done))
                              (funcall finish))
                          (funcall finish)
                          (funcall on-done '(:status done)))))
                  (funcall on-item
                           '(:type assistant-message
                             :content "follow-up"))
                  (funcall on-item '(:type done :reason stop))
                  (funcall on-done '(:status done)))))))
           (tool-lifecycle
            (e-tool-lifecycle-create
             :start
             (cl-function
              (lambda (_tool-call &key on-done &allow-other-keys)
                (setq pending-tool-done on-done)
                nil))))
           (options
            '(:model "fake"
              :context-lifetime-enabled t
              :context-capabilities
              (:continuation none
               :observation-delivery request-local-replaceable))))
      (e-loop-start-turn
       :session-id "session-race"
       :turn-id "turn-race"
       :messages '((:role user :content "prompt"))
       :backend backend
       :tool-lifecycle tool-lifecycle
       :options options
       :lifetime-frame frame-a
       :on-response-complete
       (lambda (payload)
         (let ((frame (plist-get payload :frame)))
           (push frame response-frames)
           (when (e-context-lifetime-frame-p frame)
             (e-context-lifetime-frame-complete-for-consumer
              frame
              (e-context-lifetime-frame-consumer-request-id frame)
              (format "response-%s"
                      (plist-get payload :provider-request-ordinal))))))
       :on-tool-observation
       (lambda (payload)
         (let* ((tool-call (plist-get payload :tool-call))
                (result (plist-get payload :result))
                (frame
                 (e-context-lifetime-frame-create
                  :id (format "frame-B-%s" (if provider-first "first" "last"))
                  :generation-id "generation-race"
                  :consumer-request-id "consumer-B"
                  :observations
                  (list
                   (list :observation-id "observation-B"
                         :kind "tool-result"
                         :source-entry-ref "external:tool-result:call-race"
                         :source-fingerprint "tool-result-B"
                         :effective-delivery "inherited"
                         :body (list :tool-call tool-call
                                     :tool-result result))))))
           (setq tool-frame frame)
           frame))
       :on-done (lambda (result) (setq settled result))
       :on-error (lambda (err) (setq settled (list :error err)))
       :on-event #'ignore
       :append-message (lambda (&rest _message)))
      (should (e-loop-test--wait-until (lambda () settled)))
      (should (equal (plist-get settled :status) 'done))
      (should (= request-count 2))
      (should (e-context-lifetime-frame-p tool-frame))
      (should (equal (e-context-lifetime-frame-id tool-frame)
                     (format "frame-B-%s"
                             (if provider-first "first" "last"))))
      (let ((follow-up-messages
             (plist-get (car captured-requests) :messages)))
        ;; The descendant frame is not merely metadata: the actual follow-up
        ;; request carries the paired call/result bundle in either callback
        ;; ordering.
        (should (string-match-p "call-race"
                                (prin1-to-string follow-up-messages)))
        (should (string-match-p "BUNDLE-RESULT"
                                (prin1-to-string follow-up-messages))))
      (let ((observation (car (e-context-lifetime-frame-observations
                               tool-frame))))
        (should (equal (plist-get observation :kind) "tool-result"))
        (should (equal (plist-get (plist-get observation :body)
                                  :tool-result)
                       '(:tool-call-id "call-race"
                         :name "race-tool"
                         :status ok
                         :content "BUNDLE-RESULT")))))))

(ert-deftest e-loop-test-invalid-curation-stops-later-tool-calls ()
  "An invalid reserved control stops later calls without semantic mutation."
  (let* ((started nil)
         (messages nil)
         (curation-effects nil)
         (frame
          (e-context-lifetime-frame-create
           :id "frame-invalid-control"
           :generation-id "generation-invalid-control"
           :consumer-request-id "consumer-invalid-control"
           :observations
           '((:observation-id "observation-invalid-control"
              :kind "current-state"
              :source-entry-ref "external:canvas:invalid-control"
              :source-fingerprint "invalid-control"
              :effective-delivery "request-local-replaceable"
              :body (:content "canvas")))))
         (tool-lifecycle
          (e-tool-lifecycle-create
           :prepare #'identity
           :start
           (cl-function
            (lambda (tool-call &key on-done &allow-other-keys)
              (push (plist-get tool-call :name) started)
              (funcall on-done
                       (list :tool-call-id (plist-get tool-call :id)
                             :name (plist-get tool-call :name)
                             :status 'ok
                             :content (format "result-%s"
                                               (plist-get tool-call :name))))
              nil))))
         (backend
          (e-backend-create
           :name "invalid-curation-order"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall on-item
                       '(:type tool-call
                         :id "call-before-invalid"
                         :name "before-invalid"
                         :arguments nil))
              ;; A duplicate disposition is a malformed reserved control.  The
              ;; later ordinary call must not be dispatched after this point.
              (funcall on-item
                       '(:type context-curate
                         :arguments (:keep (1) :erase (1))))
              (funcall on-item
                       '(:type tool-call
                         :id "call-after-invalid"
                         :name "after-invalid"
                         :arguments nil))
              (funcall on-item '(:type done :reason stop))))))
         (tools (e-tools-registry-create)))
    (should-error
     (e-loop-run-turn-batch
      :session-id "session-invalid-control"
      :turn-id "turn-invalid-control"
      :messages '((:role user :content "prompt"))
      :backend backend
      :tools tools
      :tool-lifecycle tool-lifecycle
      :options
      '(:model "fake"
        :context-lifetime-enabled t
        :context-capabilities
        (:continuation none
         :observation-delivery request-local-replaceable
         :reserved-effect-carrier context-curate-wire))
      :lifetime-frame frame
      :on-response-complete
      (lambda (payload)
        (setq curation-effects (plist-get payload :curation-effects)))
      :on-event #'ignore
      :append-message
      (lambda (message)
        (setq messages (append messages (list message)))))
     :type 'e-context-lifetime-invalid-record)
    (should (equal started '("before-invalid")))
    (should-not curation-effects)
    (should-not
     (seq-find (lambda (message)
                 (equal (plist-get (plist-get message :content) :name)
                        "after-invalid"))
               messages))
    ;; The malformed response never reaches the completed-response boundary,
    ;; so no assistant/session semantic completion is emitted.
    (should-not (seq-find (lambda (message)
                            (eq (plist-get message :role) 'assistant))
                          messages))))

(ert-deftest e-loop-test-curation-before-later-tool-call-fails-atomically ()
  "A tool call after a valid curation is rejected before it is queued."
  (let* ((started nil)
         (curation-effects nil)
         (messages nil)
         (frame
          (e-context-lifetime-frame-create
           :id "frame-mixed-curation"
           :generation-id "generation-mixed-curation"
           :consumer-request-id "consumer-mixed-curation"
           :observations
           '((:observation-id "observation-mixed-curation"
              :kind "current-state"
              :source-entry-ref "external:canvas:mixed-curation"
              :source-fingerprint "mixed-curation"
              :effective-delivery "inherited"
              :body (:content "canvas")))))
         (tool-lifecycle
          (e-tool-lifecycle-create
           :start
           (cl-function
            (lambda (tool-call &key on-done &allow-other-keys)
              (push (plist-get tool-call :name) started)
              (funcall on-done
                       (list :tool-call-id (plist-get tool-call :id)
                             :name (plist-get tool-call :name)
                             :status 'ok :content "unexpected"))
              nil))))
         (backend
          (e-backend-create
           :name "mixed-curation-order"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall on-item
                       '(:type context-curate
                         :arguments (:keep (1) :summaries nil)))
              (funcall on-item
                       '(:type tool-call :id "call-after-curation"
                         :name "after-curation" :arguments nil)))))))
    (should-error
     (e-loop-run-turn-batch
      :session-id "session-mixed-curation"
      :turn-id "turn-mixed-curation"
      :messages '((:role user :content "prompt"))
      :backend backend
      :tools (e-tools-registry-create)
      :tool-lifecycle tool-lifecycle
      :options '(:model "fake" :context-lifetime-enabled t
                 :context-capabilities (:continuation none
                                        :observation-delivery inherited
                                        :reserved-effect-carrier
                                        context-curate-wire))
      :lifetime-frame frame
      :on-response-complete
      (lambda (payload)
        (setq curation-effects (plist-get payload :curation-effects)))
      :on-event #'ignore
      :append-message
      (lambda (message)
        (setq messages (append messages (list message)))))
     :type 'e-context-lifetime-invalid-record)
    (should-not started)
    (should-not curation-effects)
    (should-not (seq-find
                 (lambda (message)
                   (equal (plist-get (plist-get message :content) :name)
                          "after-curation"))
                 messages))))

(ert-deftest e-loop-test-multiple-curations-fail-before-completion ()
  "A second reserved curation invalidates the complete response."
  (let* ((curation-effects nil)
         (frame
          (e-context-lifetime-frame-create
           :id "frame-multiple-curations"
           :generation-id "generation-multiple-curations"
           :consumer-request-id "consumer-multiple-curations"
           :observations
           '((:observation-id "observation-multiple-curations"
              :kind "current-state"
              :source-entry-ref "external:canvas:multiple-curations"
              :source-fingerprint "multiple-curations"
              :effective-delivery "inherited"
              :body (:content "canvas")))))
         (backend
          (e-backend-create
           :name "multiple-curations"
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall on-item
                       '(:type context-curate
                         :arguments (:keep (1) :summaries nil)))
              (funcall on-item
                       '(:type context-curate
                         :arguments (:keep (1) :summaries nil))))))))
    (should-error
     (e-loop-run-turn-batch
      :session-id "session-multiple-curations"
      :turn-id "turn-multiple-curations"
      :messages '((:role user :content "prompt"))
      :backend backend
      :tools (e-tools-registry-create)
      :options '(:model "fake"
                 :context-lifetime-enabled t
                 :context-capabilities (:continuation none
                                        :observation-delivery inherited
                                        :reserved-effect-carrier
                                        context-curate-wire))
      :lifetime-frame frame
      :on-response-complete
      (lambda (payload)
        (setq curation-effects (plist-get payload :curation-effects)))
      :on-event #'ignore
      :append-message #'ignore)
     :type 'e-context-lifetime-invalid-record)
    (should-not curation-effects)))

(ert-deftest e-loop-test-provider-compaction-clears-before-tool-follow-up ()
  "A compact request uses opaque output once, then sends ordinary tool input.

Exercise the real loop-to-OpenAI request-body boundary for both transports:
the opaque item belongs to the first request only.  A safe anchored
follow-up carries only the result because the anchor already contains the
call; stateless fallback carries the complete call/result pair."
  (dolist (transport '(http websocket))
    (dolist (with-anchor '(t nil))
      (let* ((calls 0)
           (requests nil)
           (backend
            (e-backend-create
             :name "provider-compaction-tool-follow-up"
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (let ((body
                       (e-openai-codex-request-body
                        :messages messages :options options :tools nil)))
                  (push (list :body body
                              :options (copy-tree options)
                              :messages (copy-tree messages))
                        requests)
                  (setq calls (1+ calls))
                  (if (= calls 1)
                      (progn
                        (when with-anchor
                          (funcall on-item
                                   (list :type 'provider-anchor-candidate
                                         :provider-id 'openai
                                         :metadata
                                         (list
                                          :response-id
                                          "immediate-tool-anchor"
                                          :prompt-layout-revision
                                          (e-openai-responses-prompt-layout-revision
                                           options)
                                          :reasoning-identity
                                          (e-openai-responses-reasoning-identity
                                           options)))))
                        (funcall on-item
                                 '(:type tool-call
                                   :id "compact-call"
                                   :name "echo"
                                   :arguments (:stated_purpose "Echo text."
                                               :text "hello")))
                        (funcall on-item '(:type done :reason tool-use)))
                    (funcall on-item
                             '(:type assistant-message :content "tool-seen"))
                    (funcall on-item '(:type done :reason stop))))))))
           (tools (e-tools-registry-create))
           (options (list :model "gpt-5.6"
                          :responses-transport transport
                          :context-lifetime-enabled t
                          :provider-continuation t
                          :provider-anchor-provider-id 'openai
                          :context-capabilities
                          '(:continuation linear
                            :observation-delivery request-local-replaceable)
                          :observation-delivery 'request-local-replaceable
                          :provider-compaction-output
                          [(:type "encrypted" :marker "COMPACT-TOOL")]
                          :provider-compaction-delta-messages nil
                          :context-rendering-strategy
                          'opaque-provider-compaction)))
      (e-tools-test-register
       tools
       :name "echo"
       :description "Echo text."
       :handler
       (lambda (arguments)
         (e-tools-result-create
          (plist-get (e-tools-current-context) :tool-call)
          'ok
          (plist-get arguments :text))))
      (let ((result
             (e-loop-run-turn-batch
              :session-id "session-provider-compaction-tool"
              :turn-id "turn-provider-compaction-tool"
              :messages '((:role user :content "prompt"))
              :backend backend
              :tools tools
              :options options
              :on-event #'ignore
              :append-message #'ignore)))
        (should (equal (plist-get result :status) 'done)))
      (let* ((ordered (nreverse requests))
             (first (car ordered))
             (second (cadr ordered))
             (first-body (plist-get first :body))
             (second-body (plist-get second :body))
             (second-options (plist-get second :options))
             (second-input (append (plist-get second-body :input) nil)))
        (should (= calls 2))
        (should-not (plist-member first-body :previous_response_id))
        (should (equal (aref (plist-get first-body :input) 0)
                       '(:type "encrypted" :marker "COMPACT-TOOL")))
        (should-not (plist-member second-options
                                  :provider-compaction-output))
        (should-not (eq (plist-get second-options
                                   :context-rendering-strategy)
                        'opaque-provider-compaction))
        (if with-anchor
            (should (equal (plist-get second-body :previous_response_id)
                           "immediate-tool-anchor"))
          (should-not (plist-member second-body :previous_response_id)))
        (should (equal (mapcar (lambda (item) (plist-get item :type))
                               second-input)
                       (if with-anchor
                           '("function_call_output")
                         '("message" "function_call"
                           "function_call_output"))))
        (should-not (string-match-p "COMPACT-TOOL"
                                    (prin1-to-string second-input))))))))

(ert-deftest e-loop-test-provider-compaction-clears-before-pending-steering ()
  "A compact request does not replay opaque output into pending steering."
  (dolist (transport '(http websocket))
    (let* ((calls 0)
           (drains 0)
           (requests nil)
           (backend
            (e-backend-create
             :name "provider-compaction-steering"
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (let ((body
                       (e-openai-codex-request-body
                        :messages messages :options options :tools nil)))
                  (push (list :body body
                              :options (copy-tree options)
                              :messages (copy-tree messages))
                        requests)
                  (setq calls (1+ calls))
                  (if (= calls 1)
                      (progn
                        (funcall on-item
                                 (list :type 'provider-anchor-candidate
                                       :provider-id 'openai
                                       :metadata
                                       (list
                                        :response-id "steering-anchor"
                                        :prompt-layout-revision
                                        (e-openai-responses-prompt-layout-revision
                                         options)
                                        :reasoning-identity
                                        (e-openai-responses-reasoning-identity
                                         options))))
                        (funcall on-item
                                 '(:type assistant-message :content "first"))
                        (funcall on-item '(:type done :reason stop)))
                    (funcall on-item
                             '(:type assistant-message :content "second"))
                    (funcall on-item '(:type done :reason stop))))))))
           (options (list :model "gpt-5.6"
                          :responses-transport transport
                          :context-lifetime-enabled t
                          :provider-continuation t
                          :provider-anchor-provider-id 'openai
                          :context-capabilities
                          '(:continuation linear
                            :observation-delivery request-local-replaceable)
                          :observation-delivery 'request-local-replaceable
                          :provider-compaction-output
                          [(:type "encrypted" :marker "COMPACT-STEER")]
                          :provider-compaction-delta-messages nil
                          :context-rendering-strategy
                          'opaque-provider-compaction)))
      (let (result)
        (e-loop-start-turn
         :session-id "session-provider-compaction-steering"
         :turn-id "turn-provider-compaction-steering"
         :messages '((:role user :content "prompt"))
         :backend backend
         :tools (e-tools-registry-create)
         :options options
         :drain-pending-input
         (lambda ()
           (setq drains (1+ drains))
           (when (= drains 2)
             '((:role user :content "steer-now"))))
         :on-event #'ignore
         :append-message #'ignore
         :on-done (lambda (value) (setq result value))
         :on-error (lambda (err) (setq result (list :status 'error
                                                     :error err))))
        (should (e-loop-test--wait-until (lambda () result)))
        (should (equal (plist-get result :status) 'done)))
      (let* ((ordered (nreverse requests))
             (first-body (plist-get (car ordered) :body))
             (second (cadr ordered))
             (second-body (plist-get second :body))
             (second-options (plist-get second :options))
             (second-input (append (plist-get second-body :input) nil)))
        (should (= calls 2))
        (should (= drains 4))
        (should-not (plist-member first-body :previous_response_id))
        (should (equal (aref (plist-get first-body :input) 0)
                       '(:type "encrypted" :marker "COMPACT-STEER")))
        (should-not (plist-member second-options
                                  :provider-compaction-output))
        (should-not (eq (plist-get second-options
                                   :context-rendering-strategy)
                        'opaque-provider-compaction))
        (should (equal (plist-get second-body :previous_response_id)
                       "steering-anchor"))
        (should (string-match-p "steer-now"
                                (prin1-to-string second-input)))
        (should-not (string-match-p "COMPACT-STEER"
                                    (prin1-to-string second-input)))))))

(ert-deftest e-loop-test-provider-compaction-clears-before-synchronous-start-follow-up ()
  "A synchronous backend completion clears compact state before follow-up.

The backend invokes the tool call and terminal callback before it returns and
never publishes a request handle, exercising the completion path that cannot
rely on `provider-request'."
  (let* ((calls 0)
         (requests nil)
         (backend
          (e-backend-create
           :name "provider-compaction-sync-start"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done &allow-other-keys)
              (let ((body
                     (e-openai-codex-request-body
                      :messages messages :options options :tools nil)))
                (push (list :body body :options (copy-tree options))
                      requests)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall on-item
                               (list :type 'provider-anchor-candidate
                                     :provider-id 'openai
                                     :metadata
                                     (list
                                      :response-id "sync-immediate-anchor"
                                      :prompt-layout-revision
                                      (e-openai-responses-prompt-layout-revision
                                       options)
                                      :reasoning-identity
                                      (e-openai-responses-reasoning-identity
                                       options))))
                      (funcall on-item
                               '(:type tool-call
                                 :id "sync-compact-call"
                                 :name "echo"
                                 :arguments (:stated_purpose "Echo text."
                                             :text "hello")))
                      (funcall on-item '(:type done :reason tool-use)))
                  (funcall on-item
                           '(:type assistant-message :content "tool-seen"))
                  (funcall on-item '(:type done :reason stop)))
                (funcall on-done '(:status done)))))))
         (tools (e-tools-registry-create))
         (options '(:model "gpt-5.6"
                    :context-lifetime-enabled t
                    :provider-continuation t
                    :provider-anchor-provider-id openai
                    :context-capabilities
                    (:continuation linear
                     :observation-delivery request-local-replaceable)
                    :observation-delivery request-local-replaceable
                    :provider-compaction-output
                    [(:type "encrypted" :marker "COMPACT-SYNC")]
                    :provider-compaction-delta-messages nil
                    :context-rendering-strategy opaque-provider-compaction)))
    (e-tools-test-register
     tools
     :name "echo"
     :description "Echo text."
     :handler
     (lambda (arguments)
       (e-tools-result-create
        (plist-get (e-tools-current-context) :tool-call)
        'ok
        (plist-get arguments :text))))
    (let ((result
           (e-loop-run-turn-batch
            :session-id "session-provider-compaction-sync"
            :turn-id "turn-provider-compaction-sync"
            :messages '((:role user :content "prompt"))
            :backend backend
            :tools tools
            :options options
            :on-event #'ignore
            :append-message #'ignore)))
      (should (equal (plist-get result :status) 'done)))
    (let* ((ordered (nreverse requests))
           (first-body (plist-get (car ordered) :body))
           (second-body (plist-get (cadr ordered) :body))
           (second-options (plist-get (cadr ordered) :options))
           (second-input (append (plist-get second-body :input) nil)))
      (should (= calls 2))
      (should-not (plist-member first-body :previous_response_id))
      (should (equal (aref (plist-get first-body :input) 0)
                     '(:type "encrypted" :marker "COMPACT-SYNC")))
      (should-not (plist-member second-options
                                :provider-compaction-output))
      (should-not (eq (plist-get second-options
                                 :context-rendering-strategy)
                      'opaque-provider-compaction))
      (should (equal (plist-get second-body :previous_response_id)
                     "sync-immediate-anchor"))
      (should (equal (mapcar (lambda (item) (plist-get item :type))
                             second-input)
                     '("function_call_output")))
      (should-not (string-match-p "COMPACT-SYNC"
                                  (prin1-to-string second-input))))))

(ert-deftest e-loop-test-disabled-lifetime-does-not-refresh-before-pending-steering ()
  "Callbacks do not opt a disabled turn into a lifetime projection turn."
  (let* ((calls 0)
         (drains 0)
         (refresh-count 0)
         (settled nil)
         (requests nil)
         (backend
          (e-backend-create
           :name "fake-disabled-pending-refresh"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (push (list :messages (copy-tree messages)
                          :options (copy-tree options))
                    requests)
              (setq calls (1+ calls))
              (funcall on-item
                       (list :type 'assistant-message
                             :content (if (= calls 1) "A" "B")))
              (funcall on-item '(:type done :reason stop)))))))
    (e-loop-start-turn
     :session-id "session-disabled-pending"
     :turn-id "turn-disabled-pending"
     :messages '((:role user :content "prompt"))
     :backend backend
     :tools (e-tools-registry-create)
     :options '(:state "A" :context-lifetime-enabled nil)
     :on-event #'ignore
     :append-message #'ignore
     :on-response-complete (lambda (_payload) nil)
     :refresh-context
     (lambda ()
       (setq refresh-count (1+ refresh-count))
       (list :messages '((:role system :content "STATE-B")
                         (:role user :content "prompt"))
             :options '(:state "B" :context-lifetime-enabled t)))
     :drain-pending-input
     (lambda ()
       (setq drains (1+ drains))
       (when (= drains 2)
         '((:role user :content "steer after A"))))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :status 'error
                                                    :error err))))
    (should (e-loop-test--wait-until (lambda () settled)))
    (should (equal (plist-get settled :status) 'done))
    (let* ((ordered (nreverse requests))
           (request-b (nth 1 ordered))
           (messages-b (plist-get request-b :messages))
           (printed-b (prin1-to-string messages-b)))
      (should (= calls 2))
      (should (= refresh-count 0))
      (should (string-match-p "steer after A" printed-b))
      (should-not (string-match-p "STATE-B" printed-b)))))

(ert-deftest e-loop-test-disabled-lifetime-refresh-does-not-merge-runtime-bundle ()
  "A disabled refresh remains authoritative and drops the runtime bundle."
  (let* ((calls 0)
         (refresh-count 0)
         (settled nil)
         (requests nil)
         (backend
          (e-backend-create
           :name "fake-disabled-bundle-refresh"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (push (list :messages (copy-tree messages)
                          :options (copy-tree options))
                    requests)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item
                             '(:type provider-replay-item
                               :provider-id fake
                               :item (:type "disabled-replay"
                                      :id "REPLAY-DISABLED")))
                    (funcall on-item
                             '(:type tool-call
                               :id "call-disabled-refresh"
                               :name "disabled_refreshing_tool"
                               :arguments (:stated_purpose "Refresh without lifetime.")))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item
                         '(:type assistant-message :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools (e-tools-registry-create)))
    (e-tools-test-register
     tools
     :name "disabled_refreshing_tool"
     :description "Refresh without enabling lifetime projection."
     :handler
     (lambda (_arguments)
       (e-tools-result-create
        (plist-get (e-tools-current-context) :tool-call)
        'ok
        "RAW-DISABLED-BUNDLE"
        '(:refresh-context t))))
    (e-loop-start-turn
     :session-id "session-disabled-bundle"
     :turn-id "turn-disabled-bundle"
     :messages '((:role system :content "STATE-A")
                 (:role user :content "prompt"))
     :backend backend
     :tools tools
     :options '(:state "A" :context-lifetime-enabled nil)
     :on-event #'ignore
     :append-message #'ignore
     :on-response-complete (lambda (_payload) nil)
     :on-tool-observation (lambda (_payload) nil)
     :refresh-context
     (lambda ()
       (setq refresh-count (1+ refresh-count))
       (list :messages '((:role system :content "STATE-B")
                         (:role user :content "prompt"))
             :options '(:state "B" :context-lifetime-enabled nil)))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :status 'error
                                                    :error err))))
    (should (e-loop-test--wait-until (lambda () settled)))
    (should (equal (plist-get settled :status) 'done))
    (let* ((ordered (nreverse requests))
           (request-b (nth 1 ordered))
           (messages-b (plist-get request-b :messages))
           (printed-b (prin1-to-string messages-b)))
      (should (= calls 2))
      (should (= refresh-count 1))
      (should (string-match-p "STATE-B" printed-b))
      (dolist (marker '("RAW-DISABLED-BUNDLE"
                         "call-disabled-refresh"
                         "REPLAY-DISABLED"))
        (should-not (string-match-p marker printed-b))))))

(ert-deftest e-loop-test-refresh-keeps-runtime-bundle-for-one-stateless-followup ()
  "An incompatible refresh keeps the tool bundle for B, but not for C."
  (let* ((calls 0)
         (drains 0)
         (settled nil)
         (refresh-bundle-requests nil)
         (backend
          (e-backend-create
           :name "fake-refresh-stateless-bundle"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (push (list :messages (copy-tree messages)
                          :options (copy-tree options))
                    refresh-bundle-requests)
              (setq calls (1+ calls))
              (pcase calls
                (1
                 (funcall on-item
                          '(:type provider-replay-item
                            :provider-id fake
                            :item (:type "refresh-replay"
                                   :id "REPLAY-REFRESH"
                                   :response-id "RESP-A-ID")))
                 (funcall on-item
                          '(:type tool-call
                            :id "call-refresh-bundle"
                            :name "refreshing_tool"
                            :arguments (:stated_purpose "Refresh context.")))
                 (funcall on-item '(:type done :reason tool-use)))
                (2
                 (funcall on-item
                          '(:type assistant-message :content "RESP-B-ID"))
                 (funcall on-item '(:type done :reason stop)))
                (3
                 (funcall on-item
                          '(:type assistant-message :content "RESP-C-ID"))
                 (funcall on-item '(:type done :reason stop)))
                (_
                 (error "Unexpected refresh bundle request %S" calls)))))))
         (tools (e-tools-registry-create)))
    (e-tools-test-register
     tools
     :name "refreshing_tool"
     :description "Refresh context while preserving the immediate bundle."
     :handler
     (lambda (_arguments)
       (e-tools-result-create
        (plist-get (e-tools-current-context) :tool-call)
        'ok
        "RAW-REFRESH-BUNDLE"
        '(:refresh-context t))))
    (e-loop-start-turn
     :session-id "session-refresh-bundle"
     :turn-id "turn-refresh-bundle"
     :messages '((:role system :content "STATE-A")
                 (:role user :content "prompt"))
     :backend backend
     :tools tools
     :options '(:state "A" :context-lifetime-enabled t)
     :on-event #'ignore
     :append-message #'ignore
     ;; A non-nil lifetime callback opts this loop into the runtime bundle
     ;; merge.  The callback itself is intentionally not part of this seam.
     :on-tool-observation (lambda (_payload) nil)
     :refresh-context
     (lambda ()
       (list :messages '((:role system :content "STATE-B")
                         (:role user :content "prompt"))
             :options '(:state "B"
                        :context-lifetime-enabled t
                        :context-rendering-strategy stateless
                        :continuation-projection-identity
                        (:stable-prefix "B"))))
     :drain-pending-input
     (lambda ()
       (setq drains (1+ drains))
       (when (= drains 3)
         '((:role user :content "steer after B"))))
     :on-done (lambda (result) (setq settled result))
     :on-error (lambda (err) (setq settled (list :error err))))
    (should (e-loop-test--wait-until (lambda () settled)))
    (let* ((ordered (nreverse refresh-bundle-requests))
           (request-b (nth 1 ordered))
           (request-c (nth 2 ordered))
           (messages-b (plist-get request-b :messages))
           (messages-c (plist-get request-c :messages))
           (printed-b (prin1-to-string messages-b))
           (printed-c (prin1-to-string messages-c)))
      (should (= calls 3))
      (should (= drains 5))
      (should (equal (plist-get (plist-get request-b :options) :state)
                     "B"))
      (should (eq (plist-get (plist-get request-b :options)
                            :context-rendering-strategy)
                  'stateless))
      (dolist (marker '("STATE-B" "RAW-REFRESH-BUNDLE"
                         "call-refresh-bundle" "REPLAY-REFRESH"
                         "RESP-A-ID"))
        (should (string-match-p (regexp-quote marker) printed-b)))
      (dolist (marker '("STATE-B" "steer after B"))
        (should (string-match-p (regexp-quote marker) printed-c)))
      (dolist (marker '("RAW-REFRESH-BUNDLE" "call-refresh-bundle"
                         "REPLAY-REFRESH" "RESP-A-ID" "RESP-B-ID"))
        (should-not (string-match-p (regexp-quote marker) printed-c)))
      (should-not
       (seq-some (lambda (message)
                 (memq (plist-get message :role) '(tool-call tool)))
                 messages-c)))))

(provide 'e-loop-test)

;;; e-loop-test.el ends here

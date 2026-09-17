;;; e-openai-chat-completion-composition-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI Chat Completions stream composition.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-json)
(require 'e-backend)
(require 'e-dev-profile)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-loop)
(require 'e-openai)
(require 'url-http)

(load (expand-file-name "e-openai-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-openai-test-chat-completion-url-appends-chat-path ()
  "Chat Completion providers append /chat/completions unless already present."
  (should (equal (e-openai-chat-completion-url "https://gateway.example.test/v1")
                 "https://gateway.example.test/v1/chat/completions"))
  (should (equal (e-openai-chat-completion-url "https://gateway.example.test/v1/")
                 "https://gateway.example.test/v1/chat/completions"))
  (should (equal (e-openai-chat-completion-url
                  "https://gateway.example.test/v1/chat/completions")
                 "https://gateway.example.test/v1/chat/completions")))

(ert-deftest e-openai-test-chat-completion-request-body-maps-neutral-messages ()
  "Chat Completion request bodies use OpenAI-compatible messages and tools."
  (should
   (equal
    (e-openai-chat-completion-request-body
     :messages '((:role system :content "Layer instructions.")
                 (:role user :content "hello")
                 (:role assistant :content "hi")
                 (:role tool-call
                  :content (:id "call-1"
                            :name "read"
                            :arguments (:uri "file://README.md")))
                 (:role tool
                  :content (:tool-call-id "call-1"
                            :content (:ok t))))
     :options '(:model "claude-test" :instructions "Base instructions.")
     :tools '((:type "function"
               :name "read"
               :description "Read a URI."
               :parameters (:type "object")
               :strict :json-false)))
    '(:model "claude-test"
      :stream t
      :messages [(:role "system" :content "Base instructions.")
                 (:role "system" :content "Layer instructions.")
                 (:role "user" :content "hello")
                 (:role "assistant" :content "hi")
                 (:role "assistant"
                  :content nil
                  :tool_calls [(:id "call-1"
                                :type "function"
                                :function (:name "read"
                                           :arguments "{\"uri\":\"file://README.md\"}"))])
                 (:role "tool"
                  :tool_call_id "call-1"
                  :content "{\"ok\":true}")]
      :tools [(:type "function"
               :function (:name "read"
                          :description "Read a URI."
                          :parameters (:type "object")
                          :strict :json-false))]
      :tool_choice "auto"))))

(ert-deftest e-openai-test-parse-chat-completion-stream ()
  "Chat Completion SSE chunks become backend-neutral stream items."
  (should
   (equal
    (e-openai-chat-completion-parse-stream
     "data: {\"choices\":[{\"delta\":{\"content\":\"p\",\"role\":\"assistant\"},\"index\":0}]}\n\n\
data: {\"choices\":[{\"delta\":{\"content\":\"ong\"},\"index\":0}]}\n\n\
data: {\"choices\":[{\"finish_reason\":\"stop\",\"index\":0,\"delta\":{}}],\"usage\":{\"completion_tokens\":2,\"prompt_tokens\":3,\"total_tokens\":5}}\n\n\
data: [DONE]\n\n")
    '((:type assistant-delta :content "p")
      (:type assistant-delta :content "ong")
      (:type token-usage
       :usage (:input-tokens 3
               :cached-input-tokens nil
               :cache-creation-input-tokens nil
               :output-tokens 2
               :reasoning-output-tokens nil
               :total-tokens 5))
      (:type assistant-message :content "pong")
      (:type done :reason stop)))))

(ert-deftest e-openai-test-parse-chat-completion-tool-call-stream ()
  "Chat Completion tool-call deltas become backend-neutral tool calls."
  (let ((first (e-json-serialize
                (list :choices
                      (vector
                       (list :delta
                             (list :tool_calls
                                   (vector
                                    (list :index 0
                                          :id "call-1"
                                          :type "function"
                                          :function
                                          (list :name "read"
                                                :arguments "{\"uri\":"))))
                             :index 0)))))
        (second (e-json-serialize
                 (list :choices
                       (vector
                        (list :delta
                              (list :tool_calls
                                    (vector
                                     (list :index 0
                                           :function
                                           (list :arguments
                                                 "\"file://README.md\"}"))))
                              :index 0))))))
    (should
     (equal
      (e-openai-chat-completion-parse-stream
       (concat "data: " first "\n\n"
               "data: " second "\n\n"
               "data: {\"choices\":[{\"finish_reason\":\"tool_calls\",\"index\":0,\"delta\":{}}]}\n\n"))
      '((:type tool-call
         :id "call-1"
         :name "read"
         :arguments (:uri "file://README.md"))
        (:type done :reason tool-calls))))))

(ert-deftest e-openai-test-chat-completion-tool-request-round-trips-canonical-json ()
  "Chat Completions tool request arguments serialize canonical JSON exactly."
  (let* ((arguments '(:object nil
                      :array []
                      :flags [:json-false :json-null]
                      :items [(:empty nil :values [1 :json-false])]))
         (body (e-openai-chat-completion-request-body
                :messages (list (list :role 'tool-call
                                       :content (list :id "call-json"
                                                      :name "inspect"
                                                      :arguments arguments)))
                :options '(:model "gpt-test")
                :tools nil))
         (wire-arguments
          (plist-get
           (plist-get
            (aref (plist-get (aref (plist-get body :messages) 1) :tool_calls) 0)
                  :function)
           :arguments)))
    (should (equal (e-json-parse-string wire-arguments) arguments))))

(ert-deftest e-openai-test-parse-chat-completion-length-skips-partial-tool-call ()
  "Chat Completion streams can stop before tool-call JSON is complete."
  (let ((text (e-json-serialize
               (list :choices
                     (vector
                      (list :delta
                            (list :content "I will update it.")
                            :index 0)))))
        (tool-start (e-json-serialize
                     (list :choices
                           (vector
                            (list :delta
                                  (list :tool_calls
                                        (vector
                                         (list :index 0
                                               :id "call-1"
                                               :type "function"
                                               :function
                                               (list :name "write"
                                                     :arguments "{\"uri\""))))
                                  :index 0)))))
        (length-finish
         "data: {\"choices\":[{\"finish_reason\":\"length\",\"index\":0,\"delta\":{}}]}\n\n"))
    (should
     (equal
      (e-openai-chat-completion-parse-stream
       (concat "data: " text "\n\n"
               "data: " tool-start "\n\n"
               length-finish))
      '((:type assistant-delta :content "I will update it.")
        (:type assistant-message :content "I will update it.")
        (:type done :reason length))))))

(ert-deftest e-openai-test-chat-completion-harness-streams-token-provider ()
  "Generic token-auth Chat Completion harnesses stream through injected requesters."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((eng-chat
             :name "Engineering AI Chat"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api chat-completion
             :requires-openai-auth nil)))
         (captured nil)
         (harness
          (e-openai-create-harness
           :provider 'eng-chat
           :model "claude-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (setq captured (list :url url :headers headers :body body))
              "data: {\"choices\":[{\"delta\":{\"content\":\"gateway answer\",\"role\":\"assistant\"},\"index\":0}]}\n\n\
data: {\"choices\":[{\"finish_reason\":\"stop\",\"index\":0,\"delta\":{}}]}\n\n")))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (should (equal (plist-get captured :url)
                   "https://gateway.example.test/v1/chat/completions"))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant)))
    (should (equal (plist-get (cadr (e-harness-messages harness "session-1"))
                              :content)
                   "gateway answer"))))

(provide 'e-openai-chat-completion-composition-test)

;;; e-openai-chat-completion-composition-test.el ends here

;;; e-openai-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI/Codex auth, request mapping, and stream parsing.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-openai)
(require 'url-http)

(defun e-openai-test--jwt ()
  "Return a fake JWT with a Codex account-id claim."
  (let* ((payload (json-encode
                   '(:https://api.openai.com/auth
                     (:chatgpt_account_id "acct-test"))))
         (encoded (base64-encode-string payload 'no-line-break)))
    (setq encoded (string-replace "+" "-" encoded))
    (setq encoded (string-replace "/" "_" encoded))
    (setq encoded (replace-regexp-in-string "=+$" "" encoded))
    (format "header.%s.signature" encoded)))

(defun e-openai-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 1.0)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(ert-deftest e-openai-test-auth-file-uses-codex-home ()
  "Codex auth file resolution honors CODEX_HOME."
  (let ((process-environment
         (cons "CODEX_HOME=/tmp/e-codex-home" process-environment)))
    (should (equal (e-openai-codex-auth-file)
                   "/tmp/e-codex-home/auth.json"))))

(ert-deftest e-openai-test-read-auth-token-and-account-id ()
  "Auth parsing extracts the access token and account id."
  (let* ((token (e-openai-test--jwt))
         (auth (list :tokens (list :access_token token
                                   :refresh_token "refresh"))))
    (should (equal (e-openai-codex-auth-access-token auth) token))
    (should (equal (e-openai-codex-auth-account-id auth) "acct-test"))))

(ert-deftest e-openai-test-request-body-maps-neutral-messages ()
  "OpenAI request body uses Responses API input items."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role user :content "hello")
                 (:role assistant :content "hi"))
     :options '(:model "gpt-test" :instructions "Be terse."))
    '(:model "gpt-test"
      :store :json-false
      :stream t
      :instructions "Be terse."
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "hello")])
              (:type "message"
               :role "assistant"
               :content [(:type "output_text" :text "hi")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "high")))))

(ert-deftest e-openai-test-request-body-defaults-to-gpt55-high-effort ()
  "OpenAI request bodies default to GPT-5.5 with high reasoning effort."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role user :content "hello"))
     :options nil)
    '(:model "gpt-5.5"
      :store :json-false
      :stream t
      :instructions "You are a helpful assistant."
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "hello")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "high")))))

(ert-deftest e-openai-test-request-body-maps-reasoning-effort-option ()
  "Backend-neutral reasoning effort maps to the Responses reasoning object."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role user :content "hello"))
     :options '(:model "gpt-test" :reasoning-effort "low"))
    '(:model "gpt-test"
      :store :json-false
      :stream t
      :instructions "You are a helpful assistant."
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "hello")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "low")))))

(ert-deftest e-openai-test-request-body-maps-prompt-cache-options ()
  "Backend-neutral prompt cache options map to Responses cache fields."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role user :content "hello"))
     :options '(:model "gpt-test"
                :prompt-cache-key "cache-key"
                :prompt-cache-retention "24h"))
    '(:model "gpt-test"
      :store :json-false
      :stream t
      :instructions "You are a helpful assistant."
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "hello")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "high")
      :prompt_cache_key "cache-key"
      :prompt_cache_retention "24h"))))

(ert-deftest e-openai-test-request-body-uses-continuation-anchor ()
  "Continuation sends previous_response_id with fresh context and transcript delta."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role system :content "current instructions")
                 (:role system :content "changed dynamic context")
                 (:role user :content "old prompt")
                 (:role assistant :content "old answer")
                 (:role user :content "new prompt"))
     :options '(:model "gpt-test"
                :provider-continuation t
                :provider-anchor (:provider-id openai
                                  :metadata (:response-id "resp-1"))
                :provider-anchor-delta-messages
                ((:role system :content "changed dynamic context")
                 (:role user :content "new prompt"))))
    '(:model "gpt-test"
      :store t
      :stream t
      :instructions "You are a helpful assistant.\n\ncurrent instructions\n\nchanged dynamic context"
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "new prompt")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "high")
      :previous_response_id "resp-1"))))

(ert-deftest e-openai-test-request-body-full-replay-when-continuation-disabled ()
  "Provider anchors are ignored unless continuation mode is enabled."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role system :content "current instructions")
                 (:role user :content "old prompt")
                 (:role assistant :content "old answer")
                 (:role user :content "new prompt"))
     :options '(:model "gpt-test"
                :provider-anchor (:provider-id openai
                                  :metadata (:response-id "resp-1"))
                :provider-anchor-delta-messages
                ((:role user :content "new prompt"))))
    '(:model "gpt-test"
      :store :json-false
      :stream t
      :instructions "You are a helpful assistant.\n\ncurrent instructions"
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "old prompt")])
              (:type "message"
               :role "assistant"
               :content [(:type "output_text" :text "old answer")])
              (:type "message"
               :role "user"
               :content [(:type "input_text" :text "new prompt")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "high")))))

(ert-deftest e-openai-test-request-body-stores-full-replay-when-enabled-without-anchor ()
  "Continuation mode stores full replay requests when no anchor is valid."
  (let ((body (e-openai-codex-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "gpt-test" :provider-continuation t))))
    (should (eq (plist-get body :store) t))
    (should-not (plist-member body :previous_response_id))
    (should (equal (plist-get body :input)
                   [(:type "message"
                     :role "user"
                     :content [(:type "input_text" :text "hello")])]))))

(ert-deftest e-openai-test-request-body-websocket-defaults-to-implicit-store ()
  "Responses WebSocket requests default to stored responses without serializing true."
  (let ((body (e-openai-codex-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "gpt-test"
                          :responses-transport websocket))))
    (should-not (plist-member body :store))
    (should-not (plist-member body :stream))))

(ert-deftest e-openai-test-request-body-websocket-store-true-is-implicit ()
  "Responses WebSocket store=true uses the stored default wire shape."
  (let ((body (e-openai-codex-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "gpt-test"
                          :responses-transport websocket
                          :response-store t))))
    (should-not (plist-member body :store))
    (should-not (plist-member body :stream))))

(ert-deftest e-openai-test-request-body-response-store-overrides-websocket-default ()
  "Explicit response-store config overrides the WebSocket store default."
  (let ((body (e-openai-codex-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "gpt-test"
                          :responses-transport websocket
                          :response-store :json-false
                          :provider-continuation t))))
    (should (eq (plist-get body :store) :json-false))
    (should-not (plist-member body :stream))))

(ert-deftest e-openai-test-request-body-websocket-store-false-ignores-durable-anchor ()
  "Unstored WebSocket requests do not use persisted provider anchors."
  (let ((body (e-openai-codex-request-body
               :messages '((:role system :content "current instructions")
                           (:role user :content "old prompt")
                           (:role assistant :content "old answer")
                           (:role user :content "new prompt"))
               :options '(:model "gpt-test"
                          :responses-transport websocket
                          :response-store :json-false
                          :provider-continuation t
                          :provider-anchor (:provider-id openai
                                            :metadata (:response-id "resp-1"))
                          :provider-anchor-delta-messages
                          ((:role user :content "new prompt"))))))
    (should (eq (plist-get body :store) :json-false))
    (should-not (plist-member body :previous_response_id))
    (should (equal (plist-get body :input)
                   [(:type "message"
                     :role "user"
                     :content [(:type "input_text" :text "old prompt")])
                    (:type "message"
                     :role "assistant"
                     :content [(:type "output_text" :text "old answer")])
                    (:type "message"
                     :role "user"
                     :content [(:type "input_text" :text "new prompt")])]))))

(ert-deftest e-openai-test-request-context-reports-continuation-metadata ()
  "Request context metadata reports continuation mode without prompt content."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((eng-responses
             :name "Engineering Responses"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :requires-openai-auth nil)))
         (context
          (e-openai--request-context
           :provider 'eng-responses
           :messages '((:role system :content "current instructions")
                       (:role user :content "new prompt"))
           :options '(:model "gpt-test"
                      :provider-continuation t
                      :provider-anchor
                      (:provider-id openai
                       :covered-entry-id "entry-1"
                       :metadata (:response-id "resp-1"))
                      :provider-anchor-delta-messages
                      ((:role user :content "new prompt"))
                      :prompt-cache-key "cache-key"
                      :prompt-cache-retention "24h"
                      :tools ((:name "lookup"
                               :description "Lookup."
                               :parameters (:type "object"))))))
         (metadata (plist-get context :metadata)))
    (should (equal (plist-get metadata :provider-continuation) 'used))
    (should (equal (plist-get metadata :provider-anchor-response-id) "resp-1"))
    (should (equal (plist-get metadata :provider-anchor-covered-entry-id)
                   "entry-1"))
    (should (equal (plist-get metadata :provider-continuation-delta-count) 1))
    (should (equal (plist-get metadata :diagnostics)
                   '(:model "gpt-test"
                     :reasoning-effort "high"
                     :response-store t
                     :prompt-cache-key-present t
                     :prompt-cache-retention-present t
                     :provider-continuation used
                     :previous-response-id-present t
                     :provider-anchor-present t
                     :input-message-count 1
                     :tool-count 1
                     :responses-transport http)))))

(ert-deftest e-openai-test-request-context-reports-full-replay-metadata ()
  "Continuation metadata distinguishes enabled full replay from disabled mode."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((eng-responses
             :name "Engineering Responses"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :requires-openai-auth nil)))
         (context
          (e-openai--request-context
           :provider 'eng-responses
           :messages '((:role user :content "hello"))
           :options '(:model "gpt-test"
                      :provider-continuation t
                      :provider-anchor-invalidation-reason tools-changed))))
    (let ((metadata (plist-get context :metadata)))
      (should (equal (plist-get metadata :provider-continuation) 'full))
      (should (equal (plist-get metadata :provider-anchor-invalidation-reason)
                     'tools-changed)))))

(ert-deftest e-openai-test-request-context-reports-responses-transport ()
  "Responses request metadata reports the selected transport."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((eng-responses
             :name "Engineering Responses"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         (context
          (e-openai--request-context
           :provider 'eng-responses
           :messages '((:role user :content "hello"))
           :options '(:model "gpt-test")))
         (metadata (plist-get context :metadata))
         (body-data (json-parse-string
                     (plist-get context :body)
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false)))
    (should (eq (plist-get context :responses-transport) 'websocket))
    (should (eq (plist-get metadata :responses-transport) 'websocket))
    (should-not (plist-member body-data :store))
    (should-not (plist-member body-data :stream))
    (should (eq (plist-get (plist-get metadata :diagnostics)
                           :response-store)
                t))))

(ert-deftest e-openai-test-request-context-rejects-websocket-chat-completions ()
  "WebSocket transport is only valid for Responses profiles."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((eng-chat
             :name "Engineering Chat"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api chat-completion
             :responses-transport websocket
             :requires-openai-auth nil))))
    (should-error
     (e-openai--request-context
      :provider 'eng-chat
      :messages '((:role user :content "hello"))
      :options '(:model "gpt-test"))
     :type 'e-openai-provider-invalid)))

(ert-deftest e-openai-test-request-body-preserves-explicit-reasoning-option ()
  "Explicit provider reasoning options take precedence over default effort."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role user :content "hello"))
     :options '(:model "gpt-test" :reasoning (:effort "minimal")))
    '(:model "gpt-test"
      :store :json-false
      :stream t
      :instructions "You are a helpful assistant."
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "hello")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "minimal")))))

(ert-deftest e-openai-test-request-body-moves-system-messages-to-instructions ()
  "Codex requests do not send forbidden system input messages."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role system :content "Layer instructions.")
                 (:role system :content "Visible buffer context.")
                 (:role user :content "hello"))
     :options '(:model "gpt-test" :instructions "Base instructions."))
    '(:model "gpt-test"
      :store :json-false
      :stream t
      :instructions "Base instructions.\n\nLayer instructions.\n\nVisible buffer context."
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "hello")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "high")))))

(ert-deftest e-openai-test-request-body-includes-function-call-before-output ()
  "Tool-call transcript messages serialize before function-call outputs."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role user :content "hello")
                 (:role tool-call
                  :content (:id "call-1"
                            :name "read"
                            :arguments (:uri "buffer://README.md")))
                 (:role tool
                  :content (:tool-call-id "call-1"
                            :content (:ok t))))
     :options '(:model "gpt-test"))
    '(:model "gpt-test"
      :store :json-false
      :stream t
      :instructions "You are a helpful assistant."
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "hello")])
              (:type "function_call"
               :call_id "call-1"
               :name "read"
               :arguments "{\"uri\":\"buffer://README.md\"}")
              (:type "function_call_output"
               :call_id "call-1"
               :output "{\"ok\":true}")]
      :tool_choice "auto"
      :parallel_tool_calls t
      :reasoning (:effort "high")))))

(ert-deftest e-openai-test-responses-url-appends-responses-path ()
  "Responses providers append /responses unless the base URL already has it."
  (should (equal (e-openai-responses-url "https://gateway.example.test")
                 "https://gateway.example.test/responses"))
  (should (equal (e-openai-responses-url "https://gateway.example.test/")
                 "https://gateway.example.test/responses"))
  (should (equal (e-openai-responses-url "https://gateway.example.test/responses")
                 "https://gateway.example.test/responses")))

(ert-deftest e-openai-test-token-provider-uses-env-key-authorization ()
  "Token-auth providers read bearer tokens from their configured env key."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-compatible-gateway
             :name "OpenAI-Compatible Gateway"
             :base-url "https://gateway.example.test"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :requires-openai-auth nil)))
         (captured nil)
         (backend
          (e-openai-backend-create
           :provider 'openai-compatible-gateway
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (setq captured (list :url url :headers headers :body body))
              "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (e-backend-stream backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "gateway-model")
                      :on-item #'ignore)
    (should (equal (plist-get captured :url)
                   "https://gateway.example.test/responses"))
    (should (equal (cdr (assoc "Authorization" (plist-get captured :headers)))
                   "Bearer test-gateway-token"))
    (should (assoc "Accept" (plist-get captured :headers)))
    (should (assoc "Content-Type" (plist-get captured :headers)))
    (should-not (assoc "chatgpt-account-id" (plist-get captured :headers)))
    (should-not (assoc "originator" (plist-get captured :headers)))
    (should-not (assoc "OpenAI-Beta" (plist-get captured :headers)))))

(ert-deftest e-openai-test-codex-provider-omits-prompt-cache-retention ()
  "The ChatGPT-backed Codex endpoint keeps cache keys but omits retention."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (json-encode
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
         (captured nil)
         (backend
          (e-openai-codex-backend-create
           :auth-file auth-file
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (setq captured (json-read-from-string body))
              "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (unwind-protect
        (progn
          (e-backend-stream backend
                            :messages '((:role user :content "hello"))
                            :options '(:model "gpt-test"
                                       :prompt-cache-key "cache-key"
                                       :prompt-cache-retention "24h")
                            :on-item #'ignore)
          (should (equal (alist-get 'prompt_cache_key captured)
                         "cache-key"))
          (should-not (assq 'prompt_cache_retention captured)))
      (delete-file auth-file))))

(ert-deftest e-openai-test-token-provider-keeps-prompt-cache-retention ()
  "Token-auth Responses providers keep explicit prompt-cache retention."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-compatible-gateway
             :name "OpenAI-Compatible Gateway"
             :base-url "https://gateway.example.test"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :requires-openai-auth nil)))
         (captured nil)
         (backend
          (e-openai-backend-create
           :provider 'openai-compatible-gateway
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (setq captured (json-read-from-string body))
              "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (e-backend-stream backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "gateway-model"
                                 :prompt-cache-key "cache-key"
                                 :prompt-cache-retention "24h")
                      :on-item #'ignore)
    (should (equal (alist-get 'prompt_cache_key captured) "cache-key"))
    (should (equal (alist-get 'prompt_cache_retention captured) "24h"))))

(ert-deftest e-openai-test-backend-captures-default-provider-at-create-time ()
  "Backends created from the default provider do not follow later default changes."
  (let* ((process-environment
          (append '("GATEWAY_ONE_KEY=one-token"
                    "GATEWAY_TWO_KEY=two-token")
                  process-environment))
         (e-openai-model-providers
          '((gateway-one
             :name "Gateway One"
             :base-url "https://one.example.test"
             :env-key "GATEWAY_ONE_KEY"
             :wire-api responses
             :requires-openai-auth nil)
            (gateway-two
             :name "Gateway Two"
             :base-url "https://two.example.test"
             :env-key "GATEWAY_TWO_KEY"
             :wire-api responses
             :requires-openai-auth nil)))
         (e-openai-default-provider 'gateway-one)
         (captured nil)
         (backend
          (e-openai-backend-create
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore headers body)
              (setq captured url)
              "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (setq e-openai-default-provider 'gateway-two)
    (e-backend-stream backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "gateway-model")
                      :on-item #'ignore)
    (should (equal captured "https://one.example.test/responses"))))

(ert-deftest e-openai-test-provider-default-model-is-used-by-harness ()
  "Provider default models are used when callers do not pass a model."
  (let ((e-openai-model-providers
         '((openai-compatible-gateway
            :name "OpenAI-Compatible Gateway"
            :base-url "https://gateway.example.test"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :requires-openai-auth nil
             :continuation t
             :default-model "gateway-default"))))
    (should (equal (e-harness-default-options
                    (e-openai-create-harness
                     :provider 'openai-compatible-gateway
                     :request-function #'ignore))
                   '(:model "gateway-default"
                     :reasoning-effort "high"
                     :provider-continuation t
                     :provider-anchor-provider-id openai)))))

(ert-deftest e-openai-test-default-harness-uses-codex-websocket-continuation ()
  "The built-in Codex profile uses stored WebSocket continuation."
  (should (equal (e-harness-default-options
                  (e-openai-create-harness :request-function #'ignore))
                 '(:model "gpt-5.5"
                   :reasoning-effort "high"
                   :provider-continuation t
                   :provider-anchor-provider-id openai))))

(ert-deftest e-openai-test-codex-profile-uses-implicit-websocket-store ()
  "Codex WebSocket requests use the generic stored-response default implicitly."
  (let* ((auth-file (make-temp-file "e-openai-auth" nil ".json"))
         (auth (json-encode
                (list :tokens
                      (list :access_token (e-openai-test--jwt)
                            :refresh_token "refresh")))))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert auth))
          (let* ((context
                  (e-openai--request-context
                   :provider 'codex
                   :auth-file auth-file
                   :messages '((:role user :content "hello"))
                   :options nil))
                 (body (json-parse-string (plist-get context :body)
                                          :object-type 'plist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object :json-false)))
            (should (eq (plist-get context :responses-transport) 'websocket))
            (should-not (plist-member body :store))
            (should-not (plist-member body :stream))
            (should (eq (plist-get (plist-get (plist-get context :metadata)
                                              :diagnostics)
                                   :response-store)
                        t))))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest e-openai-test-normalizes-legacy-codex-response-store ()
  "Reloaded legacy Codex defaults drop the obsolete store=false override."
  (should
   (equal
    (e-openai--normalize-model-providers
     `((codex
        :name "ChatGPT Codex"
        :base-url ,(concat e-openai-codex-default-base-url "/codex")
        :wire-api responses
        :responses-transport websocket
        :response-store :json-false
        :continuation t
        :requires-openai-auth t)
       (custom-codex
        :name "Custom Codex"
        :base-url ,(concat e-openai-codex-default-base-url "/codex")
        :wire-api responses
        :responses-transport websocket
        :response-store :json-false
        :continuation t
        :requires-openai-auth t)))
    `((codex
       :name "ChatGPT Codex"
       :base-url ,(concat e-openai-codex-default-base-url "/codex")
       :wire-api responses
       :responses-transport websocket
       :continuation t
       :requires-openai-auth t)
      (custom-codex
       :name "Custom Codex"
       :base-url ,(concat e-openai-codex-default-base-url "/codex")
       :wire-api responses
       :responses-transport websocket
       :response-store :json-false
       :continuation t
       :requires-openai-auth t)))))

(ert-deftest e-openai-test-provider-profile-normalizes-legacy-codex-response-store ()
  "Provider lookup repairs stale built-in Codex store overrides."
  (let ((e-openai-model-providers
         `((codex
            :name "ChatGPT Codex"
            :base-url ,(concat e-openai-codex-default-base-url "/codex")
            :wire-api responses
            :responses-transport websocket
            :response-store :json-false
            :continuation t
            :requires-openai-auth t))))
    (should-not
     (plist-member (e-openai-provider-profile 'codex) :response-store))))

(ert-deftest e-openai-test-responses-profile-can-disable-continuation ()
  "Responses profiles do not use provider continuation unless explicitly enabled."
  (let ((e-openai-model-providers
         '((openai-no-continuation
            :name "OpenAI No Continuation"
            :base-url "https://gateway.example.test"
            :env-key "OPENAI_GATEWAY_API_KEY"
            :wire-api responses
            :requires-openai-auth nil
            :default-model "gateway-default"))))
    (should (equal (e-harness-default-options
                    (e-openai-create-harness
                     :provider 'openai-no-continuation
                     :request-function #'ignore))
                   '(:model "gateway-default"
                     :reasoning-effort "high")))))

(ert-deftest e-openai-test-responses-profile-can-enable-continuation ()
  "Responses profiles opt into provider continuation explicitly."
  (let ((e-openai-model-providers
         '((openai-continuation
            :name "OpenAI Continuation"
            :base-url "https://gateway.example.test"
            :env-key "OPENAI_GATEWAY_API_KEY"
            :wire-api responses
            :requires-openai-auth nil
            :continuation t
            :default-model "gateway-default"))))
    (should (equal (e-harness-default-options
                    (e-openai-create-harness
                     :provider 'openai-continuation
                     :request-function #'ignore))
                   '(:model "gateway-default"
                     :reasoning-effort "high"
                     :provider-continuation t
                     :provider-anchor-provider-id openai)))))

(ert-deftest e-openai-test-generic-harness-streams-token-provider ()
  "Generic token-auth harnesses stream through injected requesters."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-compatible-gateway
             :name "OpenAI-Compatible Gateway"
             :base-url "https://gateway.example.test"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :requires-openai-auth nil)))
         (harness
          (e-openai-create-harness
           :provider 'openai-compatible-gateway
           :model "gateway-model"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers body)
              "data: {\"type\":\"response.output_text.done\",\"text\":\"gateway answer\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant)))
    (should (equal (plist-get (cadr (e-harness-messages harness "session-1"))
                              :content)
                   "gateway answer"))))

(ert-deftest e-openai-test-parse-sse-events ()
  "Responses SSE events become backend-neutral stream items."
  (should
   (equal
    (e-openai-codex-parse-stream
     "event: response.output_text.delta\ndata: {\"type\":\"response.output_text.delta\",\"delta\":\"he\"}\n\n\
event: response.output_text.done\ndata: {\"type\":\"response.output_text.done\",\"text\":\"hello\"}\n\n\
event: response.completed\ndata: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")
    '((:type assistant-delta :content "he")
      (:type assistant-message :content "hello")
      (:type done :reason stop)))))

(ert-deftest e-openai-test-parse-function-call-event ()
  "Responses function calls become backend-neutral tool calls."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"now\",\"arguments\":\"{\\\"format\\\":\\\"iso\\\"}\"}}\n\n")
    '((:type tool-call
      :id "call-1"
      :name "now"
      :arguments (:format "iso"))))))

(ert-deftest e-openai-test-parse-reasoning-summary-delta ()
  "Responses reasoning summary deltas become backend-neutral reasoning items."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"checking context\"}\n\n")
    '((:type reasoning-delta :content "checking context")))))

(ert-deftest e-openai-test-parse-message-output-item ()
  "Responses message output items become assistant messages."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"message\",\"content\":[{\"type\":\"output_text\",\"text\":\"hello from item\"}]}}\n\n")
    '((:type assistant-message :content "hello from item")))))

(ert-deftest e-openai-test-parse-live-codex-message-shape-deduplicates ()
  "The live Codex text event sequence prefers canonical final text."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.output_text.delta\",\"delta\":\"pong\"}\n\n\
data: {\"type\":\"response.output_text.done\",\"text\":\"pong\"}\n\n\
data: {\"type\":\"response.content_part.done\",\"part\":{\"type\":\"output_text\",\"text\":\"pong\"}}\n\n\
data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"message\",\"content\":[{\"type\":\"output_text\",\"text\":\"pong\"}]}}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")
    '((:type assistant-delta :content "pong")
      (:type assistant-message :content "pong")
      (:type done :reason stop)))))

(ert-deftest e-openai-test-parse-completed-usage ()
  "Responses completed usage becomes provider-neutral token usage."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.output_text.done\",\"text\":\"ok\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\",\"usage\":{\"input_tokens\":202598,\"input_tokens_details\":{\"cached_tokens\":7552},\"output_tokens\":419,\"output_tokens_details\":{\"reasoning_tokens\":139},\"total_tokens\":203017}}}\n\n")
    '((:type assistant-message :content "ok")
      (:type token-usage
       :usage (:input-tokens 202598
               :cached-input-tokens 7552
               :output-tokens 419
               :reasoning-output-tokens 139
               :total-tokens 203017))
      (:type done :reason stop)))))

(ert-deftest e-openai-test-parse-completed-response-id ()
  "Responses completed ids become provider anchor candidates."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"status\":\"completed\"}}\n\n")
    '((:type provider-anchor-candidate
       :provider-id openai
       :metadata (:response-id "resp-1"))
      (:type done :reason stop)))))

(ert-deftest e-openai-test-parse-identical-canonical-messages-are-preserved ()
  "Separate canonical text-done events with the same content are not deduped."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.output_text.done\",\"text\":\"same\"}\n\n\
data: {\"type\":\"response.output_text.done\",\"text\":\"same\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")
    '((:type assistant-message :content "same")
      (:type assistant-message :content "same")
      (:type done :reason stop)))))

(ert-deftest e-openai-test-debug-diagnostics-record-ignored-events ()
  "Debug diagnostics record raw response and ignored provider event summaries."
  (let ((e-openai-codex-debug t)
        (e-openai-codex--last-diagnostics nil)
        (stream "data: {\"type\":\"response.unknown\",\"item\":{\"type\":\"mystery\"}}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n"))
    (should (equal (e-openai-codex-parse-stream stream)
                   '((:type done :reason stop))))
    (should (equal (plist-get e-openai-codex--last-diagnostics :raw-response)
                   stream))
    (should
     (equal (plist-get e-openai-codex--last-diagnostics :events)
            '((:event-type "response.unknown"
               :item-type "mystery"
               :parsed-type nil)
              (:event-type "response.completed"
               :item-type nil
               :parsed-type done))))))

(ert-deftest e-openai-test-parse-stream-appends-raw-response-buffer ()
  "Raw provider responses are retained in a hidden inspection buffer."
  (let ((buffer-name e-openai-codex-raw-responses-buffer-name)
        (stream "data: {\"type\":\"response.completed\"}\n\n"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (unwind-protect
        (progn
          (e-openai-codex-parse-stream stream)
          (should (string-prefix-p " " buffer-name))
          (should (get-buffer buffer-name))
          (with-current-buffer buffer-name
            (should (string-match-p
                     (regexp-quote stream)
                     (buffer-string)))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest e-openai-test-parse-json-error-response ()
  "Non-stream provider JSON errors become backend error items."
  (should
   (equal
    (e-openai-codex-parse-stream
     "{\"error\":{\"message\":\"Invalid schema\",\"type\":\"invalid_request_error\"}}")
    '((:type backend-error
      :content "Invalid schema"
      :payload (:error (:message "Invalid schema"
                       :type "invalid_request_error")))))))

(ert-deftest e-openai-test-parse-html-error-response ()
  "HTML provider error responses become explicit backend error items."
  (let* ((items
          (e-openai-codex-parse-stream
           "<html><body><h1>Web server is returning an unknown error</h1>\
<p>Error reference number: 520</p></body></html>"))
         (item (car items)))
    (should (= (length items) 1))
    (should (equal (plist-get item :type) 'backend-error))
    (should (string-match-p "HTML" (plist-get item :content)))
    (should (string-match-p "520" (plist-get item :content)))
    (should (equal (plist-get (plist-get item :payload) :response-kind)
                   'html))
    (should (string-match-p "520"
                            (plist-get (plist-get item :payload)
                                       :preview)))))

(ert-deftest e-openai-test-response-failed-keeps-summary-and-payload ()
  "Responses failure events keep a readable summary and full payload."
  (let* ((items
          (e-openai-codex-parse-stream
           "data: {\"type\":\"response.failed\",\"response\":{\"status\":\"failed\",\"error\":{\"code\":\"context_length_exceeded\",\"message\":\"Your input exceeds the context window of this model.\"}}}\n\n"))
         (item (car items)))
    (should (equal (plist-get item :type) 'backend-error))
    (should (equal (plist-get item :content)
                   "Your input exceeds the context window of this model."))
    (should (equal (plist-get (plist-get item :payload) :type)
                   "response.failed"))
    (should (equal (plist-get
                    (plist-get
                     (plist-get (plist-get item :payload) :response)
                     :error)
                    :code)
                   "context_length_exceeded"))))


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

(ert-deftest e-openai-test-chat-completion-provider-builds-chat-request ()
  "Provider profiles can select the Chat Completions wire API."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((eng-chat
             :name "Engineering AI Chat"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api chat-completion
             :requires-openai-auth nil
             :default-model "claude-default")))
         (context (e-openai--request-context
                   :provider 'eng-chat
                   :messages '((:role user :content "hello"))
                   :options nil)))
    (should (equal (plist-get context :wire-api) 'chat-completion))
    (should (equal (plist-get context :url)
                   "https://gateway.example.test/v1/chat/completions"))
    (should (equal (cdr (assoc "Authorization" (plist-get context :headers)))
                   "Bearer test-gateway-token"))
    (should (equal (json-parse-string (plist-get context :body)
                                      :object-type 'plist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object :json-false)
                   '(:model "claude-default"
                     :stream t
                     :messages ((:role "system"
                                  :content "You are a helpful assistant.")
                                (:role "user" :content "hello")))))))

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
               :output-tokens 2
               :reasoning-output-tokens nil
               :total-tokens 5))
      (:type assistant-message :content "pong")
      (:type done :reason stop)))))

(ert-deftest e-openai-test-parse-chat-completion-tool-call-stream ()
  "Chat Completion tool-call deltas become backend-neutral tool calls."
  (let ((first (json-encode
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
        (second (json-encode
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

(ert-deftest e-openai-test-parse-chat-completion-length-skips-partial-tool-call ()
  "Chat Completion streams can stop before tool-call JSON is complete."
  (let ((text (json-encode
               (list :choices
                     (vector
                      (list :delta
                            (list :content "I will update it.")
                            :index 0)))))
        (tool-start (json-encode
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
    (e-harness-prompt harness "session-1" "question")
    (should (equal (plist-get captured :url)
                   "https://gateway.example.test/v1/chat/completions"))
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant)))
    (should (equal (plist-get (cadr (e-harness-messages harness "session-1"))
                              :content)
                   "gateway answer"))))

(ert-deftest e-openai-test-default-http-request-start-accepts-keyword-arguments ()
  "The default async HTTP requester accepts the backend keyword call shape."
  (let (captured-url captured-method captured-headers captured-body)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _args)
                 (setq captured-url url)
                 (setq captured-method url-request-method)
                 (setq captured-headers url-request-extra-headers)
                 (setq captured-body url-request-data)
                 (let ((buffer (generate-new-buffer " *e-openai-test-http*")))
                   (with-current-buffer buffer
                     (insert "HTTP/1.1 200 OK\n\n"
                             "data: {\"type\":\"response.completed\"}\n\n"))
                   (with-current-buffer buffer
                     (funcall callback nil))
                   buffer))))
      (let (response error)
        (e-openai-codex--http-request-start
         :url "https://example.test/codex/responses"
         :headers '(("Authorization" . "Bearer test"))
         :body "{}"
         :on-complete (lambda (value) (setq response value))
         :on-error (lambda (err) (setq error err)))
        (should-not error)
        (should (equal response
                       "data: {\"type\":\"response.completed\"}\n\n")))
      (should (equal captured-url "https://example.test/codex/responses"))
      (should (equal captured-method "POST"))
      (should (equal captured-headers '(("Authorization" . "Bearer test"))))
      (should (equal (decode-coding-string captured-body 'utf-8) "{}")))))

(ert-deftest e-openai-test-default-http-request-start-returns-error-body ()
  "HTTP error responses with bodies continue through backend parsing."
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (_url callback &rest _args)
               (let ((buffer (generate-new-buffer " *e-openai-test-http*")))
                 (with-current-buffer buffer
                   (insert "HTTP/1.1 400 Bad Request\n\n"
                           "{\"error\":{\"message\":\"Invalid request\"}}"))
                 (with-current-buffer buffer
                   (funcall callback '(:error (error http 400))))
                 buffer))))
    (let (response error)
      (e-openai-codex--http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete (lambda (value) (setq response value))
       :on-error (lambda (err) (setq error err)))
      (should-not error)
      (should (equal response
                     "{\"error\":{\"message\":\"Invalid request\"}}"))
      (should (equal (plist-get (car (e-openai-codex-parse-stream response))
                                :content)
                     "Invalid request")))))

(ert-deftest e-openai-test-default-http-request-start-normalizes-header-bytes ()
  "Multibyte ASCII headers must not make a Unicode request body invalid."
  (let (captured-request)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _args)
                 (ignore url)
                 (let ((buffer (generate-new-buffer " *e-openai-test-http*")))
                   (with-current-buffer buffer
                     (mm-disable-multibyte)
                     (setq url-current-object (url-generic-parse-url url)
                           url-http-target-url url-current-object
                           url-http-method url-request-method
                           url-http-version "1.1"
                           url-http-extra-headers url-request-extra-headers
                           url-http-data url-request-data
                           url-http-proxy nil
                           url-http-referer nil
                           url-http-attempt-keepalives t
                           url-extensions-header nil
                           url-mime-encoding-string nil
                           url-mime-charset-string nil
                           url-mime-language-string nil
                           url-mime-accept-string nil
                           url-privacy-level nil
                           url-user-agent nil
                           url-http-real-basic-auth-storage nil)
                     (setq captured-request (url-http-create-request))
                     (insert "HTTP/1.1 200 OK\n\n"
                             "data: {\"type\":\"response.completed\"}\n\n"))
                   (with-current-buffer buffer
                     (funcall callback nil))
                   buffer))))
      (let (response error)
        (e-openai-codex--http-request-start
         :url "https://example.test/codex/responses"
         :headers `(("Authorization" . ,(string-to-multibyte "Bearer test"))
                    ("Content-Type" . "application/json"))
         :body (json-encode '(:text "▌ unicode body"))
         :on-complete (lambda (value) (setq response value))
         :on-error (lambda (err) (setq error err)))
        (should-not error)
        (should (equal response
                       "data: {\"type\":\"response.completed\"}\n\n")))
      (should (= (string-bytes captured-request)
                 (length captured-request))))))

(ert-deftest e-openai-test-default-http-request-times-out ()
  "A default url-retrieve request that never calls back times out visibly."
  (let ((e-openai-request-timeout-seconds 0.01)
        (buffer nil)
        (error-count 0)
        (complete-count 0)
        error)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _callback &rest _args)
                 (setq buffer
                       (generate-new-buffer " *e-openai-test-http*"))
                 buffer)))
      (e-openai-codex--http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete (lambda (_value)
                      (setq complete-count (1+ complete-count)))
       :on-error (lambda (err)
                   (setq error-count (1+ error-count))
                   (setq error err)))
      (should (e-openai-test--wait-until (lambda () error) 0.2))
      (should (eq (car error) 'e-openai-request-timeout))
      (should (= error-count 1))
      (should (= complete-count 0))
      (should-not (buffer-live-p buffer)))))

(ert-deftest e-openai-test-timeout-settles-once ()
  "A late url callback after timeout does not settle the request again."
  (let ((e-openai-request-timeout-seconds 0.01)
        (callback nil)
        (error-count 0)
        (complete-count 0)
        error)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url cb &rest _args)
                 (setq callback cb)
                 (generate-new-buffer " *e-openai-test-http*"))))
      (e-openai-codex--http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete (lambda (_value)
                      (setq complete-count (1+ complete-count)))
       :on-error (lambda (err)
                   (setq error-count (1+ error-count))
                   (setq error err)))
      (should (e-openai-test--wait-until (lambda () error) 0.2))
      (with-temp-buffer
        (insert "HTTP/1.1 200 OK\n\n"
                "data: {\"type\":\"response.completed\"}\n\n")
        (funcall callback nil))
      (should (eq (car error) 'e-openai-request-timeout))
      (should (= error-count 1))
      (should (= complete-count 0)))))

(ert-deftest e-openai-test-backend-streams-through-injected-requester ()
  "The Codex backend streams parsed events from an injected HTTP requester."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (json-encode
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
         (e-openai-model-providers
          `((codex
             :name "ChatGPT Codex HTTP"
             :base-url ,(concat e-openai-codex-default-base-url "/codex")
             :wire-api responses
             :responses-transport http
             :requires-openai-auth t)))
         (seen nil)
         (captured nil)
         (backend
          (e-openai-codex-backend-create
           :auth-file auth-file
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (setq captured (list :url url :headers headers :body body))
              "data: {\"type\":\"response.output_text.done\",\"text\":\"ok\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (unwind-protect
        (progn
          (e-backend-stream backend
                            :messages '((:role user :content "hello"))
                            :options '(:model "gpt-test")
                            :on-item (lambda (item) (push item seen)))
          (should (equal (nreverse seen)
                         '((:type assistant-message :content "ok")
                           (:type done :reason stop))))
          (should (equal (plist-get captured :url)
                         "https://chatgpt.com/backend-api/codex/responses"))
          (should (assoc "Authorization" (plist-get captured :headers)))
          (should (assoc "chatgpt-account-id" (plist-get captured :headers))))
      (delete-file auth-file))))

(ert-deftest e-openai-test-websocket-backend-streams-response-events ()
  "Responses WebSocket profiles stream JSON events through backend callbacks."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         opened-url opened-headers sent on-message done-status error seen request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (url &rest args)
                 (setq opened-url url)
                 (setq opened-headers (plist-get args :custom-header-alist))
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (should (eq websocket 'fake-websocket))
                 (setq sent (json-parse-string text
                                               :object-type 'plist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object :json-false))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.output_text.delta"
                             :delta "ok")))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.completed"
                             :response (:id "resp-ws-1"
                                        :status "completed"))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (setq request
              (e-backend-start backend
                               :messages '((:role user :content "hello"))
                               :options '(:model "gpt-test")
                               :on-item (lambda (item) (push item seen))
                               :on-done (lambda (status)
                                          (setq done-status status))
                               :on-error (lambda (err) (setq error err))))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (e-backend-request-p request))
        (should (equal opened-url "wss://gateway.example.test/v1/responses"))
        (should (equal (cdr (assoc "Authorization" opened-headers))
                       "Bearer test-gateway-token"))
        (should (equal (cdr (assoc "OpenAI-Beta" opened-headers))
                       "responses_websockets=2026-02-06"))
        (should (equal (plist-get sent :type) "response.create"))
        (should-not (plist-member sent :store))
        (should-not (plist-member sent :stream))
        (should (equal (nreverse seen)
                       '((:type assistant-delta :content "ok")
                         (:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"))
                         (:type done :reason stop))))
        (should (eq (plist-get (e-backend-request-metadata request)
                               :transport)
                    'websocket))))))

(ert-deftest e-openai-test-websocket-buffers-assistant-message-candidates ()
  "Responses WebSocket internals do not leak assistant-message-candidate items."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         on-message done-status error seen)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket _text)
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.content_part.done"
                             :part (:type "output_text"
                                    :text "candidate answer"))))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.output_item.done"
                             :item (:type "message"
                                    :content
                                    [(:type "output_text"
                                      :text "candidate answer")]))))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.completed"
                             :response (:id "resp-ws-1"
                                        :status "completed"))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "hello"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item seen))
                         :on-done (lambda (status)
                                    (setq done-status status))
                         :on-error (lambda (err) (setq error err)))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (equal (nreverse seen)
                       '((:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"))
                         (:type assistant-message
                          :content "candidate answer")
                         (:type done :reason stop))))))))

(ert-deftest e-openai-test-websocket-does-not-use-http-request-timeout ()
  "Responses WebSocket requests do not inherit the HTTP whole-request timeout."
  (let* ((e-openai-request-timeout-seconds 0.01)
         (e-openai-websocket-idle-timeout-seconds nil)
         (process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         on-message done-status error seen)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args) t))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "hello"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item seen))
                         :on-done (lambda (status)
                                    (setq done-status status))
                         :on-error (lambda (err) (setq error err)))
        (accept-process-output nil 0.05)
        (should-not error)
        (should-not done-status)
        (funcall on-message
                 'fake-websocket
                 (json-encode
                  '(:type "response.completed"
                    :response (:id "resp-ws-1" :status "completed"))))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (equal (nreverse seen)
                       '((:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"))
                         (:type done :reason stop))))))))

(ert-deftest e-openai-test-websocket-completion-closes-request ()
  "Responses WebSocket completion closes the transport after settling."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         on-message done-status error late-seen (close-count 0))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket _text)
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.completed"
                             :response (:id "resp-ws-1"
                                        :status "completed"))))
                 (funcall on-message
                          websocket
                          (json-encode
                           '(:type "response.output_text.delta"
                             :delta "late")))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "hello"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item late-seen))
                         :on-done (lambda (status)
                                    (setq done-status status))
                         :on-error (lambda (err) (setq error err)))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (= close-count 1))
        (should (equal (nreverse late-seen)
                       '((:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"))
                         (:type done :reason stop))))))))

(ert-deftest e-openai-test-websocket-idle-timeout-settles-error ()
  "Responses WebSocket idle timeout settles stalled requests as errors."
  (let* ((e-openai-websocket-idle-timeout-seconds 0.01)
         (process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         error done-status (close-count 0))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (&rest _args) 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args) t))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "hello"))
                         :options '(:model "gpt-test")
                         :on-item #'ignore
                         :on-done (lambda (status)
                                    (setq done-status status))
                         :on-error (lambda (err) (setq error err)))
        (should (e-openai-test--wait-until (lambda () error) 0.2))
        (should-not done-status)
        (should (eq (car error) 'e-openai-request-timeout))
        (should (= close-count 1))))))

(ert-deftest e-openai-test-websocket-cancel-closes-request ()
  "Responses WebSocket cancellation runs its cleanup path."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :requires-openai-auth nil)))
         (close-count 0)
         request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (&rest _args) 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args) t))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (setq request
            (e-backend-start (e-openai-backend-create :provider 'openai-websocket)
                             :messages '((:role user :content "hello"))
                             :options '(:model "gpt-test")
                             :on-item (lambda (_item) nil)
                             :on-done (lambda (_status) nil)
                             :on-error (lambda (_err) nil)))
      (should (e-backend-cancel-request request))
      (should (= close-count 1)))))

(ert-deftest e-openai-test-websocket-store-false-full-replays-follow-up ()
  "Unstored WebSocket follow-ups replay input without previous_response_id."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :response-store :json-false
             :continuation t
             :requires-openai-auth nil)))
         sends on-message done-count seen)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let* ((payload (json-parse-string text
                                                    :object-type 'plist
                                                    :array-type 'list
                                                    :null-object nil
                                                    :false-object :json-false))
                        (index (length sends)))
                   (push payload sends)
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.output_text.done"
                               :text ,(if (= index 0) "answer one" "answer two"))))
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.completed"
                               :response
                               (:id ,(if (= index 0) "resp-local-1" "resp-local-2")
                                :status "completed")))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start backend
                         :messages '((:role user :content "one"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item seen))
                         :on-done (lambda (_status)
                                    (setq done-count (1+ (or done-count 0))))
                         :on-error #'signal)
        (should (e-openai-test--wait-until
                 (lambda () (= (or done-count 0) 1))
                 0.2))
        (e-backend-start backend
                         :messages '((:role user :content "one")
                                     (:role assistant :content "answer one")
                                     (:role user :content "two"))
                         :options '(:model "gpt-test")
                         :on-item (lambda (item) (push item seen))
                         :on-done (lambda (_status)
                                    (setq done-count (1+ (or done-count 0))))
                         :on-error #'signal)
        (should (e-openai-test--wait-until
                 (lambda () (= (or done-count 0) 2))
                 0.2))
        (let* ((first (cadr sends))
               (second (car sends))
               (second-response second))
          (should (eq (plist-get first :store) :json-false))
          (should-not (plist-member first :previous_response_id))
          (should (eq (plist-get second-response :store) :json-false))
          (should-not (plist-member second-response :previous_response_id))
          (should (equal (plist-get second-response :input)
                         '((:type "message"
                            :role "user"
                            :content ((:type "input_text" :text "one")))
                           (:type "message"
                            :role "assistant"
                            :content ((:type "output_text" :text "answer one")))
                           (:type "message"
                            :role "user"
                            :content ((:type "input_text" :text "two")))))))
        (should-not (seq-some (lambda (item)
                                (eq (plist-get item :type)
                                    'provider-anchor-candidate))
                              seen))))))

(ert-deftest e-openai-test-backend-default-request-is-cancellable ()
  "The default OpenAI request path exposes a cancellable url-retrieve handle."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (json-encode
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
         (e-openai-model-providers
          `((codex
             :name "ChatGPT Codex HTTP"
             :base-url ,(concat e-openai-codex-default-base-url "/codex")
             :wire-api responses
             :responses-transport http
             :requires-openai-auth t)))
         (request nil)
         (buffer nil)
         (backend
          (e-openai-codex-backend-create :auth-file auth-file)))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url _callback &rest _args)
                     (setq buffer
                           (generate-new-buffer " *e-openai-test-http*"))
                     buffer)))
          (e-backend-start backend
                           :messages '((:role user :content "hello"))
                           :options '(:model "gpt-test")
                           :on-item #'ignore
                           :on-done #'ignore
                           :on-error #'ignore
                           :on-request-start (lambda (handle)
                                               (setq request handle)))
          (should (e-backend-request-p request))
          (should (buffer-live-p buffer))
          (should (e-backend-cancel-request request))
          (should-not (buffer-live-p buffer))
          (should (equal (plist-get (e-backend-request-metadata request)
                                    :transport)
                         'url-retrieve))
          (should (equal (plist-get (e-backend-request-metadata request)
                                    :cancellable)
                         t)))
      (delete-file auth-file))))

(ert-deftest e-openai-test-backend-cancel-deletes-url-network-process ()
  "Cancelling a url-retrieve request deletes its network process."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (json-encode
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
         (e-openai-model-providers
          `((codex
             :name "ChatGPT Codex HTTP"
             :base-url ,(concat e-openai-codex-default-base-url "/codex")
             :wire-api responses
             :responses-transport http
             :requires-openai-auth t)))
         (request nil)
         (buffer nil)
         (process nil)
         (backend
          (e-openai-codex-backend-create :auth-file auth-file)))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url _callback &rest _args)
                     (setq buffer
                           (generate-new-buffer " *e-openai-test-http*"))
                     (setq process
                           (make-network-process
                            :name "chatgpt.com"
                            :server t
                            :service t
                            :host 'local
                            :buffer buffer))
                     buffer)))
          (e-backend-start backend
                           :messages '((:role user :content "hello"))
                           :options '(:model "gpt-test")
                           :on-item #'ignore
                           :on-done #'ignore
                           :on-error #'ignore
                           :on-request-start (lambda (handle)
                                               (setq request handle)))
          (should (e-backend-request-p request))
          (should (process-live-p process))
          (should (e-backend-cancel-request request))
          (should-not (process-live-p process))
          (should-not (buffer-live-p buffer)))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file auth-file))))

(ert-deftest e-openai-test-codex-harness-runs-minimal-prompt-flow ()
  "The Codex harness helper can run prompt to persisted assistant message."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (json-encode
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
         (harness
          (e-openai-codex-create-harness
           :auth-file auth-file
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers body)
              "data: {\"type\":\"response.output_text.done\",\"text\":\"real-ish answer\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-harness-prompt harness "session-1" "question")
          (should (equal (mapcar (lambda (message) (plist-get message :role))
                                 (e-harness-messages harness "session-1"))
                         '(user assistant)))
          (should (equal (plist-get (cadr (e-harness-messages harness "session-1"))
                                    :content)
                         "real-ish answer")))
      (delete-file auth-file))))

(provide 'e-openai-test)

;;; e-openai-test.el ends here

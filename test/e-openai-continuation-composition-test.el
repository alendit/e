;;; e-openai-continuation-composition-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI continuation and provider-profile composition.

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

(ert-deftest e-openai-test-gpt56-full-fallback-restores-breakpoint ()
  "The safe body for a failed continuation contains the stable marker."
  (let* ((revision e-openai-gpt56-explicit-cache-layout-revision)
         (messages '((:role system :content "Stable instructions.")
                     (:role system :content "Fresh dynamic state.")
                     (:role user :content "new prompt")))
         (options
          `(:model "gpt-5.6-sol"
            :prompt-cache-key "cache-key"
            :provider-continuation t
            :provider-anchor
            (:provider-id openai
             :metadata (:response-id "resp-1"
                        :prompt-layout-revision ,revision
                        :reasoning-identity
                        (:effort "high" :summary "auto")))
            :provider-anchor-delta-messages
            ((:role system :content "Fresh dynamic state.")
             (:role user :content "new prompt"))
            :segments ((:kind static-prefix
                        :messages ((:role system
                                    :content "Stable instructions.")))
                       (:kind current-state
                        :messages ((:role system
                                    :content "Fresh dynamic state."))))))
         (incremental
          (e-openai-codex-request-body
           :messages messages :options options))
         (full
          (e-openai-codex-request-body
           :messages messages
           :options (e-openai-responses-options-without-provider-anchor options)))
         (stable-block
          (aref (plist-get (aref (plist-get full :input) 0) :content) 0)))
    (should (equal (plist-get incremental :previous_response_id) "resp-1"))
    (should-not (plist-member full :previous_response_id))
    (should (equal (plist-get stable-block :prompt_cache_breakpoint)
                   '(:mode "explicit")))))

(ert-deftest e-openai-test-replays-encrypted-reasoning-before-carrier-message ()
  "Stateless replay restores opaque reasoning immediately before its output."
  (let* ((reasoning-item
          '(:type "reasoning" :id "rs-1" :encrypted_content "ciphertext"
            :summary nil))
         (body
          (e-openai-codex-request-body
           :messages
           `((:role user :content "first prompt")
             (:role assistant
              :content "first answer"
              :metadata
              (:provider-replay-items
               ((:type provider-replay-item
                 :provider-id openai
                 :item ,reasoning-item))))
             (:role user :content "second prompt"))
           :options '(:model "gpt-5.6"
                      :include-encrypted-reasoning t)))
         (input (append (plist-get body :input) nil)))
    (should (equal (plist-get body :include)
                   ["reasoning.encrypted_content"]))
    (should (equal (mapcar (lambda (item) (plist-get item :type)) input)
                   '("message" "reasoning" "message" "message")))
    (should (equal (nth 1 input)
                   '(:type "reasoning" :id "rs-1"
                     :encrypted_content "ciphertext" :summary [])))
    (should (equal (plist-get (nth 1 input) :summary) []))
    ;; Request normalization must not mutate the persisted replay record.
    (should (plist-member reasoning-item :summary))
    (should (equal (plist-get (nth 2 input) :role) "assistant"))))

(ert-deftest e-openai-test-replays-encrypted-reasoning-before-tool-call ()
  "Stateless replay restores opaque reasoning before its function call."
  (let* ((reasoning-item
          '(:type "reasoning" :id "rs-1" :encrypted_content "ciphertext"
            :summary (:type "summary_text" :text "kept")))
         (body
          (e-openai-codex-request-body
           :messages
           `((:role user :content "call the tool")
             (:role tool-call
              :content
              (:type tool-call
               :id "call-1"
               :name "echo"
               :arguments (:text "hi")
               :provider-replay-items
               ((:type provider-replay-item
                 :provider-id "openai"
                 :item ,reasoning-item))))
             (:role tool
              :content
              (:tool-call-id "call-1" :content "hi")))
           :options '(:model "gpt-5.6")))
         (input (append (plist-get body :input) nil)))
    (should (equal (mapcar (lambda (item) (plist-get item :type)) input)
                   '("message" "reasoning" "function_call"
                     "function_call_output")))
    (should (equal (nth 1 input)
                   '(:type "reasoning" :id "rs-1"
                     :encrypted_content "ciphertext"
                     :summary [(:type "summary_text" :text "kept")])))))

(ert-deftest e-openai-test-replays-multiple-reasoning-items-across-tool-continuations ()
  "Each successive tool continuation replays reasoning summaries as arrays."
  (let* ((first-reasoning
          '(:type "reasoning" :id "rs-1" :encrypted_content "ciphertext-1"
            :summary [(:type "summary_text" :text "first")] ))
         (second-reasoning
          '(:type "reasoning" :id "rs-2" :encrypted_content "ciphertext-2"
            ;; A provider object is explicitly projected to a one-element
            ;; summary array by the Responses adapter.
            :summary (:type "summary_text" :text "second")))
         (body
          (e-openai-codex-request-body
           :messages
           `((:role user :content "call both tools")
             (:role tool-call
              :content
              (:type tool-call
               :id "call-1"
               :name "first"
               :arguments (:value 1)
               :provider-replay-items
               ((:type provider-replay-item
                 :provider-id openai
                 :item ,first-reasoning))))
             (:role tool
              :content (:tool-call-id "call-1" :content "one")
              :metadata nil)
             (:role tool-call
              :content
              (:type tool-call
               :id "call-2"
               :name "second"
               :arguments (:value 2)
               :provider-replay-items
               ((:type provider-replay-item
                 :provider-id openai
                 :item ,second-reasoning))))
             (:role tool
              :content (:tool-call-id "call-2" :content "two")
              :metadata nil))
           :options '(:model "gpt-5.6" :include-encrypted-reasoning t)))
         (input (append (plist-get body :input) nil))
         (reasoning-items
          (seq-filter (lambda (item)
                        (equal (plist-get item :type) "reasoning"))
                      input)))
    (should (equal (mapcar (lambda (item) (plist-get item :type)) input)
                   '("message" "reasoning" "function_call"
                     "function_call_output" "reasoning" "function_call"
                     "function_call_output")))
    (should (= (length reasoning-items) 2))
    (should (equal (mapcar (lambda (item) (plist-get item :summary))
                          reasoning-items)
                   '([(:type "summary_text" :text "first")]
                     [(:type "summary_text" :text "second")])))))

(ert-deftest e-openai-test-gpt56-invalidates-legacy-layout-anchor ()
  "An anchor without the explicit-layout revision forces a safe full request."
  (let* ((body
          (e-openai-codex-request-body
           :messages '((:role system :content "Stable instructions.")
                       (:role system :content "Fresh dynamic state.")
                       (:role user :content "new prompt"))
           :options
           '(:model "gpt-5.6-sol"
             :prompt-cache-key "cache-key"
             :provider-continuation t
             :provider-anchor
             (:provider-id openai :metadata (:response-id "legacy-resp"))
             :provider-anchor-delta-messages
             ((:role system :content "Fresh dynamic state.")
              (:role user :content "new prompt"))
             :segments ((:kind static-prefix
                         :messages ((:role system
                                     :content "Stable instructions.")))
                        (:kind current-state
                         :messages ((:role system
                                     :content "Fresh dynamic state.")))))))
         (input (append (plist-get body :input) nil))
         (stable-block (aref (plist-get (car input) :content) 0)))
    (should-not (plist-member body :previous_response_id))
    (should (equal (mapcar (lambda (item) (plist-get item :role)) input)
                   '("developer" "developer" "user")))
    (should (equal (plist-get stable-block :prompt_cache_breakpoint)
                   '(:mode "explicit")))))

(ert-deftest e-openai-test-reasoning-summary-fences-continuation-anchor ()
  "A changed effective reasoning summary cannot reuse a Responses anchor."
  (let* ((base
          '(:model "gpt-test"
            :reasoning-summary "auto"
            :provider-continuation t
            :provider-anchor
            (:provider-id openai
             :metadata (:response-id "resp-1"
                        :reasoning-identity
                        (:effort "high" :summary "detailed")))
            :provider-anchor-delta-messages
            ((:role user :content "new prompt"))))
         (unsafe (e-openai-codex-request-body
                  :messages '((:role user :content "new prompt"))
                  :options base))
         (safe-options (copy-tree base)))
    (setq safe-options
          (plist-put safe-options
                     :provider-anchor
                     '(:provider-id openai
                       :metadata (:response-id "resp-1"
                                  :reasoning-identity
                                  (:effort "high" :summary "auto")))))
    (should-not (plist-member unsafe :previous_response_id))
    (should (equal (plist-get
                   (e-openai-codex-request-body
                     :messages '((:role user :content "new prompt"))
                     :options safe-options)
                   :previous_response_id)
                   "resp-1"))))

(ert-deftest e-openai-test-reasoning-summary-missing-anchor-identity-forces-replay ()
  "A Responses anchor without reasoning identity cannot authorize continuation."
  (let* ((options '(:model "gpt-test"
                    :prompt-cache-key "cache-key"
                    :provider-continuation t
                    :provider-anchor-delta-messages
                    ((:role user :content "new prompt"))))
         (options
          (plist-put
           (copy-sequence options)
           :provider-anchor
           (list :provider-id 'openai
                 :metadata
                 (list :response-id "resp-1"
                       :prompt-layout-revision
                       (e-openai-responses-prompt-layout-revision options)))))
         (body (e-openai-codex-request-body
                :messages '((:role user :content "new prompt"))
                :options options)))
    (should-not (plist-member body :previous_response_id))))

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
                                  :metadata (:response-id "resp-1"
                                             :reasoning-identity
                                             (:effort "high" :summary "auto")))
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
      :reasoning (:effort "high" :summary "auto")
      :previous_response_id "resp-1"))))

(ert-deftest e-openai-test-request-body-continuation-includes-in-turn-tool-output ()
  "Anchored follow-up requests include tool messages appended during the turn."
  (let ((body
         (e-openai-codex-request-body
          :messages '((:role system :content "current instructions")
                      (:role system :content "changed dynamic context")
                      (:role user :content "old prompt")
                      (:role assistant :content "old answer")
                      (:role user :content "new prompt")
                      (:role tool-call
                       :content (:type tool-call
                                 :id "call-1"
                                 :name "inspect"
                                 :arguments (:target "state")))
                      (:role tool
                       :content (:tool-call-id "call-1"
                                 :name "inspect"
                                 :status ok
                                 :content "fresh state")))
          :options '(:model "gpt-test"
                     :provider-continuation t
                     :provider-anchor (:provider-id openai
                                       :metadata (:response-id "resp-1"
                                                  :reasoning-identity
                                                  (:effort "high" :summary "auto")))
                     :provider-anchor-source-message-count 5
                     :provider-anchor-delta-messages
                     ((:role system :content "changed dynamic context")
                      (:role user :content "new prompt"))))))
    (should (equal (plist-get body :previous_response_id) "resp-1"))
    (should
     (equal
      (plist-get body :input)
      [(:type "message"
        :role "user"
        :content [(:type "input_text" :text "new prompt")])
       (:type "function_call"
        :call_id "call-1"
        :name "inspect"
        :arguments "{\"target\":\"state\"}")
       (:type "function_call_output"
        :call_id "call-1"
        :output "fresh state")]))))

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
      :reasoning (:effort "high" :summary "auto")))))

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
      :reasoning (:effort "minimal" :summary "auto")))))

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
      :reasoning (:effort "high" :summary "auto")))))

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
      :reasoning (:effort "high" :summary "auto")))))

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
    (e-backend-stream-batch backend
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
                                    (e-json-serialize
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
              (setq captured (e-json-parse-string body))
              "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (unwind-protect
        (progn
          (e-backend-stream-batch backend
                            :messages '((:role user :content "hello"))
                            :options '(:model "gpt-test"
                                       :prompt-cache-key "cache-key"
                                       :prompt-cache-retention "24h")
                            :on-item #'ignore)
          (should (equal (plist-get captured :prompt_cache_key)
                         "cache-key"))
          (should-not (plist-member captured :prompt_cache_retention)))
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
              (setq captured (e-json-parse-string body))
              "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (e-backend-stream-batch backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "gateway-model"
                                 :prompt-cache-key "cache-key"
                                 :prompt-cache-retention "24h")
                      :on-item #'ignore)
    (should (equal (plist-get captured :prompt_cache_key) "cache-key"))
    (should (equal (plist-get captured :prompt_cache_retention) "24h"))))

(ert-deftest e-openai-test-gpt56-omits-deprecated-prompt-cache-retention ()
  "GPT-5.6 keeps its cache key but omits the legacy retention policy."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-compatible-gateway
             :name "OpenAI-compatible gateway"
             :base-url "https://gateway.example.test"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :requires-openai-auth nil)))
         captured
         (backend
          (e-openai-backend-create
           :provider 'openai-compatible-gateway
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (setq captured (e-json-parse-string body))
              "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (e-backend-stream-batch
     backend
     :messages '((:role user :content "hello"))
     :options '(:model "gpt-5.6-sol"
                :prompt-cache-key "cache-key"
                :prompt-cache-retention "24h")
     :on-item #'ignore)
    (should (equal (plist-get captured :prompt_cache_key) "cache-key"))
    (should-not (plist-member captured :prompt_cache_retention))
    (should-not (plist-member captured :prompt_cache_options))))

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
    (e-backend-stream-batch backend
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
                     :reasoning-summary "auto"
                     :provider-continuation t
                     :provider-anchor-provider-id openai)))))

(ert-deftest e-openai-test-provider-profile-normalizes-builtin-codex-requirements ()
  "Every lookup applies current built-in Codex wire and semantic constraints."
  (let ((e-openai-model-providers
         `((codex
            :name "ChatGPT Codex"
            :base-url ,(concat e-openai-codex-default-base-url "/codex")
            :wire-api responses
            :responses-transport websocket
            :response-store :json-false
            :continuation t
            :requires-openai-auth t))))
    (let ((profile (e-openai-provider-profile 'codex)))
      (should (eq (plist-get profile :response-store) :json-false))
      (should (plist-member profile :prompt-cache-breakpoint-mode))
      (should-not (plist-get profile :prompt-cache-breakpoint-mode))
      (should (eq (plist-get profile :responses-context-layout)
                  'developer-input))
      (should (plist-get profile :include-encrypted-reasoning))
      (should (eq (plist-get profile :observation-delivery)
                  'inherited))
      (should-not (plist-member profile :websocket-idle-close-seconds)))
    (let ((backend (e-openai-backend-create :provider 'codex)))
      ;; Doom may replace the canonical profile after backend construction.
      ;; Lookup-time normalization must prove that later configuration too.
      (setq e-openai-model-providers
            `((codex
               :name "ChatGPT Codex"
               :base-url ,(concat e-openai-codex-default-base-url "/codex")
               :wire-api responses
               :responses-transport websocket
               :continuation t
               :requires-openai-auth t)))
      (let ((profile (e-openai-provider-profile 'codex))
            (capabilities (e-backend-context-capabilities backend nil)))
        (should (eq (plist-get profile :response-store) :json-false))
        (should (eq (plist-get profile :responses-context-layout)
                    'developer-input))
        (should (eq (plist-get profile :observation-delivery)
                    'inherited))
        (should-not (plist-member profile :websocket-idle-close-seconds))
        (should (equal (plist-get capabilities :observation-delivery)
                       'inherited))))))

(ert-deftest e-openai-test-builtin-codex-normalization-preserves-opt-out ()
  "An explicit inherited Codex declaration remains conservative."
  (let ((e-openai-model-providers
         `((codex
            :name "ChatGPT Codex"
            :base-url ,(concat e-openai-codex-default-base-url "/codex")
            :wire-api responses
            :responses-transport websocket
            :observation-delivery inherited
            :continuation t
            :requires-openai-auth t))))
    (let* ((profile (e-openai-provider-profile 'codex))
           (capabilities
            (e-backend-context-capabilities
             (e-openai-backend-create :provider 'codex)
             nil)))
      (should (eq (plist-get profile :observation-delivery) 'inherited))
      (e-openai-test--assert-observation-delivery capabilities nil)
      (should (eq (plist-get capabilities :continuation) 'linear)))))

(ert-deftest e-openai-test-canonical-api-profile-shares-responses-contract ()
  "The first-party API profile differs from Codex only in provider capability."
  (let* ((profile (e-openai-provider-profile 'openai))
         (capabilities
          (e-backend-context-capabilities
           (e-openai-backend-create :provider 'openai)
           nil)))
    (should (equal (plist-get profile :base-url)
                   e-openai-api-default-base-url))
    (should (equal (plist-get profile :env-key) "OPENAI_API_KEY"))
    (should-not (plist-get profile :requires-openai-auth))
    (should (eq (plist-get profile :responses-transport) 'websocket))
    (should (eq (plist-get profile :response-store) :json-false))
    (should (eq (plist-get profile :responses-context-layout)
                'developer-input))
    (should (eq (plist-get profile :prompt-cache-breakpoint-mode) 'explicit))
    (should (eq (plist-get profile :observation-delivery)
                'inherited))
    (should (plist-get profile :include-encrypted-reasoning))
    (should (plist-get profile :continuation))
    (should (equal (plist-get profile :default-model) "gpt-5.6"))
    (should (equal (plist-get capabilities :observation-delivery)
                   'inherited))
    (e-openai-test--assert-observation-delivery capabilities nil)))

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
                     :reasoning-effort "high"
                     :reasoning-summary "auto")))))

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
                     :reasoning-summary "auto"
                     :provider-continuation t
                     :provider-anchor-provider-id openai)))))

(ert-deftest e-openai-test-backend-default-request-is-cancellable ()
  "The default OpenAI request path exposes a cancellable url-retrieve handle."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (e-json-serialize
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
                                    (e-json-serialize
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

(provide 'e-openai-continuation-composition-test)

;;; e-openai-continuation-composition-test.el ends here

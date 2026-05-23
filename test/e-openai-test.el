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
               :output "(:ok t)")]
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
            :default-model "gateway-default"))))
    (should (equal (e-harness-default-options
                    (e-openai-create-harness
                     :provider 'openai-compatible-gateway
                     :request-function #'ignore))
                   '(:model "gateway-default" :reasoning-effort "high")))))

(ert-deftest e-openai-test-default-harness-uses-gpt55-high-effort ()
  "The default OpenAI harness uses GPT-5.5 and high reasoning effort."
  (should (equal (e-harness-default-options
                  (e-openai-create-harness :request-function #'ignore))
                 '(:model "gpt-5.5" :reasoning-effort "high"))))

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

(ert-deftest e-openai-test-backend-streams-through-injected-requester ()
  "The Codex backend streams parsed events from an injected HTTP requester."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (json-encode
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
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

(ert-deftest e-openai-test-backend-default-request-is-cancellable ()
  "The default OpenAI request path exposes a cancellable url-retrieve handle."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (json-encode
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
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

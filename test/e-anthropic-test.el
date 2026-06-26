;;; e-anthropic-test.el --- Tests for e Anthropic Messages backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for Anthropic Messages auth, request mapping, and stream parsing.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-anthropic)

(ert-deftest e-anthropic-test-request-body-maps-neutral-messages ()
  "Anthropic request body uses Messages turns with explicit max_tokens."
  (should
   (equal
    (e-anthropic-request-body
     :messages '((:role user :content "hello")
                 (:role assistant :content "hi"))
     :options '(:model "claude-test" :max-tokens 1024 :effort "high"))
    '(:model "claude-test"
      :max_tokens 1024
      :stream t
      :messages [(:role "user"
                  :content [(:type "text" :text "hello")])
                 (:role "assistant"
                  :content [(:type "text" :text "hi")])]
      :thinking (:type "adaptive")
      :output_config (:effort "high")))))

(ert-deftest e-anthropic-test-request-body-moves-system-messages-to-system-field ()
  "System messages and the instructions option fold into the top-level system field."
  (should
   (equal
    (e-anthropic-request-body
     :messages '((:role system :content "Layer instructions.")
                 (:role system :content "Visible buffer context.")
                 (:role user :content "hello"))
     :options '(:model "claude-test" :max-tokens 1024
                :instructions "Base instructions."))
    '(:model "claude-test"
      :max_tokens 1024
      :stream t
      :system "Base instructions.\n\nLayer instructions.\n\nVisible buffer context."
      :messages [(:role "user"
                  :content [(:type "text" :text "hello")])]
      :thinking (:type "adaptive")
      :output_config (:effort "high")))))

(ert-deftest e-anthropic-test-request-body-omits-system-when-empty ()
  "No system field is sent when there are no system messages or instructions."
  (should-not
   (plist-member
    (e-anthropic-request-body
     :messages '((:role user :content "hello"))
     :options '(:model "claude-test" :max-tokens 1024))
    :system)))

(ert-deftest e-anthropic-test-request-body-maps-tool-definitions ()
  "Backend-neutral tools map to Messages tools with input_schema."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role user :content "hello"))
      :options '(:model "claude-test" :max-tokens 1024)
      :tools '((:type "function"
                :name "read"
                :description "Read a URI."
                :parameters (:type "object")
                :strict :json-false)))
     :tools)
    [(:name "read"
      :description "Read a URI."
      :input_schema (:type "object"))])))

(ert-deftest e-anthropic-test-request-body-maps-tool-call-and-result-turns ()
  "Tool-call messages become tool_use blocks; tool results become user tool_result turns."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role user :content "hello")
                  (:role tool-call
                   :content (:id "call-1"
                             :name "read"
                             :arguments (:uri "file://README.md")))
                  (:role tool
                   :content (:tool-call-id "call-1"
                             :content (:ok t))))
      :options '(:model "claude-test" :max-tokens 1024))
     :messages)
    [(:role "user"
      :content [(:type "text" :text "hello")])
     (:role "assistant"
      :content [(:type "tool_use"
                 :id "call-1"
                 :name "read"
                 :input (:uri "file://README.md"))])
     (:role "user"
      :content [(:type "tool_result"
                 :tool_use_id "call-1"
                 :content "{\"ok\":true}")])])))

(ert-deftest e-anthropic-test-request-body-adds-cache-control-on-system ()
  "Prompt caching attaches a cache_control breakpoint to the system block."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role system :content "Stable instructions.")
                  (:role user :content "hello"))
      :options '(:model "claude-test" :max-tokens 1024 :prompt-cache t))
     :system)
    [(:type "text"
      :text "Stable instructions."
      :cache_control (:type "ephemeral"))])))

(ert-deftest e-anthropic-test-request-body-cache-control-honors-ttl ()
  "An explicit prompt-cache-ttl is forwarded to cache_control."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role system :content "Stable instructions.")
                  (:role user :content "hello"))
      :options '(:model "claude-test" :max-tokens 1024
                 :prompt-cache t :prompt-cache-ttl "1h"))
     :system)
    [(:type "text"
      :text "Stable instructions."
      :cache_control (:type "ephemeral" :ttl "1h"))])))

(ert-deftest e-anthropic-test-request-body-cache-control-uses-segment-breakpoint ()
  "Segment-aware caching stops before current-state system context."
  (let* ((body
          (e-anthropic-request-body
           :messages '((:role system :content "Stable instructions.")
                       (:role system :content "Current buffer.")
                       (:role user :content "hello"))
           :options
           '(:model "claude-test"
             :max-tokens 1024
             :prompt-cache t
             :segments ((:kind static-prefix
                         :id stable-instructions
                         :fingerprint "stable-fp"
                         :messages ((:role system
                                     :content "Stable instructions.")))
                        (:kind current-state
                         :id current-buffer
                         :fingerprint "current-fp"
                         :messages ((:role system
                                     :content "Current buffer.")))))))
         (system (plist-get body :system))
         (messages (plist-get body :messages)))
    (should (equal system
                   [(:type "text"
                     :text "Stable instructions."
                     :cache_control (:type "ephemeral"))
                    (:type "text"
                     :text "Current buffer.")]))
    (should (equal messages
                   [(:role "user"
                     :content [(:type "text" :text "hello")])]))))

(ert-deftest e-anthropic-test-request-body-top-level-cache-control ()
  "Top-level automatic cache mode leaves system as plain content."
  (let ((body (e-anthropic-request-body
               :messages '((:role system :content "Stable instructions.")
                           (:role user :content "hello"))
               :options '(:model "claude-test"
                          :max-tokens 1024
                          :prompt-cache t
                          :prompt-cache-mode top-level
                          :prompt-cache-ttl "1h"))))
    (should (equal (plist-get body :cache_control)
                   '(:type "ephemeral" :ttl "1h")))
    (should (equal (plist-get body :system) "Stable instructions."))))

(ert-deftest e-anthropic-test-request-body-sends-container-when-configured ()
  "Container ids are sent only when configured in turn options."
  (let ((body (e-anthropic-request-body
               :messages '((:role user :content "hello"))
               :options '(:model "claude-test"
                          :max-tokens 1024
                          :anthropic-container-id "container-1"))))
    (should (equal (plist-get body :container) "container-1"))))

(ert-deftest e-anthropic-test-request-body-caches-tools-when-no-system ()
  "With caching enabled and no system, the breakpoint lands on the last tool."
  (let ((tools (plist-get
                (e-anthropic-request-body
                 :messages '((:role user :content "hello"))
                 :options '(:model "claude-test" :max-tokens 1024
                            :prompt-cache t)
                 :tools '((:type "function" :name "a" :description "A"
                           :parameters (:type "object"))
                          (:type "function" :name "b" :description "B"
                           :parameters (:type "object"))))
                :tools)))
    (should-not (plist-member (aref tools 0) :cache_control))
    (should (equal (plist-get (aref tools 1) :cache_control)
                   '(:type "ephemeral")))))

(ert-deftest e-anthropic-test-request-body-system-plain-without-caching ()
  "Without caching the system field stays a plain string."
  (should
   (equal
    (plist-get
     (e-anthropic-request-body
      :messages '((:role system :content "Stable instructions.")
                  (:role user :content "hello"))
      :options '(:model "claude-test" :max-tokens 1024))
     :system)
    "Stable instructions.")))

(ert-deftest e-anthropic-test-parse-text-stream ()
  "Messages SSE text events become backend-neutral stream items."
  (should
   (equal
    (e-anthropic-parse-stream
     "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"role\":\"assistant\",\"content\":[],\"usage\":{\"input_tokens\":10}}}\n\n\
event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"he\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"llo\"}}\n\n\
event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
    '((:type assistant-delta :content "he")
      (:type assistant-delta :content "llo")
      (:type assistant-message :content "hello")
      (:type token-usage
       :usage (:input-tokens 10
               :cached-input-tokens nil
               :cache-creation-input-tokens nil
               :output-tokens 5
               :reasoning-output-tokens nil
               :total-tokens nil))
      (:type done :reason stop)))))

(ert-deftest e-anthropic-test-parse-stream-maps-cache-tokens ()
  "Cache read and creation token counts map into neutral usage."
  (should
   (equal
    (seq-find (lambda (item) (eq (plist-get item :type) 'token-usage))
              (e-anthropic-parse-stream
               "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":10,\"cache_read_input_tokens\":4,\"cache_creation_input_tokens\":6}}}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"))
    '(:type token-usage
      :usage (:input-tokens 10
              :cached-input-tokens 4
              :cache-creation-input-tokens 6
              :output-tokens 5
              :reasoning-output-tokens nil
              :total-tokens nil)))))

(ert-deftest e-anthropic-test-emits-cache-anchor-candidate ()
  "Successful cached Anthropic responses emit a durable provider anchor candidate."
  (let (items)
    (e-anthropic--emit-response-items-with-context
     "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"
     '(:metadata (:provider anthropic
                  :model "claude-test"
                  :anthropic-cache-mode explicit
                  :anthropic-cache-breakpoint system-stable-prefix
                  :anthropic-breakpoint-segment-id "stable-instructions"
                  :anthropic-breakpoint-fingerprint "stable-fp"
                  :full-history t))
     (lambda (item) (push item items)))
    (should
     (equal
      (nreverse items)
      '((:type provider-anchor-candidate
         :provider-id anthropic
         :metadata (:provider anthropic
                    :model "claude-test"
                    :anthropic-cache-mode explicit
                    :anthropic-cache-breakpoint system-stable-prefix
                    :anthropic-breakpoint-segment-id "stable-instructions"
                    :anthropic-breakpoint-fingerprint "stable-fp"
                    :full-history t))
        (:type done :reason stop))))))

(ert-deftest e-anthropic-test-parse-non-stream-json-error ()
  "A non-stream JSON error body becomes a backend error item."
  (should
   (equal
    (e-anthropic-parse-stream
     "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"max_tokens is required\"}}")
    '((:type backend-error
       :content "max_tokens is required"
       :payload (:type "error"
                 :error (:type "invalid_request_error"
                         :message "max_tokens is required")))))))

(ert-deftest e-anthropic-test-parse-non-stream-html-error ()
  "A non-stream HTML error body becomes a single backend error item."
  (let* ((items (e-anthropic-parse-stream
                 "<html><head><title>520</title></head><body><h1>Web server is returning an unknown error</h1></body></html>"))
         (item (car items)))
    (should (= (length items) 1))
    (should (eq (plist-get item :type) 'backend-error))
    (should (eq (plist-get (plist-get item :payload) :response-kind) 'html))
    (should (string-match-p "HTML" (plist-get item :content)))
    (should (string-match-p "unknown error" (plist-get item :content)))))

(ert-deftest e-anthropic-test-parse-non-stream-text-error ()
  "A non-stream, non-JSON text body becomes a single backend error item."
  (let* ((items (e-anthropic-parse-stream "upstream connect error or disconnect/reset before headers"))
         (item (car items)))
    (should (= (length items) 1))
    (should (eq (plist-get item :type) 'backend-error))
    (should (eq (plist-get (plist-get item :payload) :response-kind) 'text))
    (should (string-match-p "upstream connect error" (plist-get item :content)))))

(ert-deftest e-anthropic-test-parse-empty-body-returns-no-items ()
  "A truly empty body yields no items so the loop reports empty output."
  (should (null (e-anthropic-parse-stream "")))
  (should (null (e-anthropic-parse-stream "   \n  "))))

(ert-deftest e-anthropic-test-parse-tool-use-stream ()
  "Messages tool_use blocks accumulate input JSON into a neutral tool call."
  (should
   (equal
    (e-anthropic-parse-stream
     "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_1\",\"name\":\"read\",\"input\":{}}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"uri\\\":\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"file://README.md\\\"}\"}}\n\n\
event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
    '((:type tool-call
       :id "toolu_1"
       :name "read"
       :arguments (:uri "file://README.md"))
      (:type done :reason tool-use)))))

(ert-deftest e-anthropic-test-parse-max-tokens-stop-is-surfaced ()
  "A truncated turn surfaces a distinct max-tokens done reason."
  (should
   (equal
    (e-anthropic-parse-stream
     "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Writing now.\"}}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"max_tokens\"}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n")
    '((:type assistant-delta :content "Writing now.")
      (:type assistant-message :content "Writing now.")
      (:type done :reason max-tokens)))))

(ert-deftest e-anthropic-test-messages-url-appends-messages-path ()
  "Messages providers append /messages unless the base URL already has it."
  (should (equal (e-anthropic-messages-url "https://gateway.example.test/v1")
                 "https://gateway.example.test/v1/messages"))
  (should (equal (e-anthropic-messages-url "https://gateway.example.test/v1/")
                 "https://gateway.example.test/v1/messages"))
  (should (equal (e-anthropic-messages-url
                  "https://gateway.example.test/v1/messages")
                 "https://gateway.example.test/v1/messages")))

(ert-deftest e-anthropic-test-request-context-uses-bearer-auth ()
  "Bearer providers send x-api-key and anthropic-version headers."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (context (e-anthropic--request-context
                   :provider 'eng-anthropic
                   :messages '((:role user :content "hello"))
                   :options '(:model "claude-test" :max-tokens 1024))))
    (should (equal (plist-get context :url)
                   "https://gateway.example.test/v1/messages"))
    (should (equal (cdr (assoc "x-api-key" (plist-get context :headers)))
                   "test-token"))
    (should (equal (cdr (assoc "anthropic-version" (plist-get context :headers)))
                   e-anthropic-version))
    (should (assoc "Content-Type" (plist-get context :headers)))
    (should (equal (json-parse-string (plist-get context :body)
                                      :object-type 'plist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object :json-false)
                   '(:model "claude-test"
                     :max_tokens 1024
                     :stream t
                     :messages ((:role "user"
                                 :content ((:type "text" :text "hello"))))
                     :thinking (:type "adaptive")
                     :output_config (:effort "high"))))))

(ert-deftest e-anthropic-test-request-context-reports-cache-metadata ()
  "Anthropic request metadata explains cache placement without omitting history."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (context
          (e-anthropic--request-context
           :provider 'eng-anthropic
           :messages '((:role system :content "Stable instructions.")
                       (:role system :content "Current buffer.")
                       (:role user :content "hello"))
           :options
           '(:model "claude-test"
             :max-tokens 1024
             :effort "low"
             :prompt-cache t
             :prompt-cache-ttl "1h"
             :anthropic-container-id "container-1"
             :tools ((:name "lookup"
                      :description "Lookup."
                      :parameters (:type "object")))
             :segments ((:kind static-prefix
                         :id stable-instructions
                         :fingerprint "stable-fp"
                         :messages ((:role system
                                     :content "Stable instructions.")))
                        (:kind current-state
                         :id current-buffer
                         :fingerprint "current-fp"
                         :messages ((:role system
                                     :content "Current buffer.")))))))
         (metadata (plist-get context :metadata)))
    (should (equal (plist-get metadata :anthropic-cache-mode) 'explicit))
    (should (equal (plist-get metadata :anthropic-cache-breakpoint)
                   'system-stable-prefix))
    (should (equal (plist-get metadata :anthropic-breakpoint-segment-id)
                   "stable-instructions"))
    (should (equal (plist-get metadata :anthropic-breakpoint-fingerprint)
                   "stable-fp"))
    (should (equal (plist-get metadata :anthropic-cache-ttl) "1h"))
    (should (equal (plist-get metadata :anthropic-container-id)
                   "container-1"))
    (should (equal (plist-get metadata :provider) 'anthropic))
    (should (equal (plist-get metadata :model) "claude-test"))
    (should (equal (plist-get metadata :segment-fingerprints)
                   '("stable-fp" "current-fp")))
    (should (equal (plist-get metadata :anthropic-beta-headers) nil))
    (should (eq (plist-get metadata :full-history) t))
    (should (= (plist-get metadata :segment-fingerprint-count) 2))
    (should (equal (plist-get metadata :diagnostics)
                   '(:model "claude-test"
                     :effort "low"
                     :max-tokens 1024
                     :prompt-cache t
                     :anthropic-cache-mode explicit
                     :anthropic-cache-breakpoint system-stable-prefix
                     :anthropic-cache-ttl "1h"
                     :anthropic-container-id-present t
                     :input-message-count 1
                     :tool-count 1)))))

(ert-deftest e-anthropic-test-request-context-sends-gated-context-management ()
  "Raw Anthropic context_management is sent only with explicit beta headers."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY"
             :context-management (:edits [(:type "clear_tool_results")])
             :beta-headers ("context-management-test"))))
         (context
          (e-anthropic--request-context
           :provider 'eng-anthropic
           :messages '((:role user :content "hello"))
           :options '(:model "claude-test" :max-tokens 1024)))
         (body (json-parse-string (plist-get context :body)
                                  :object-type 'plist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object :json-false))
         (metadata (plist-get context :metadata)))
    (should (equal (plist-get body :context_management)
                   '(:edits ((:type "clear_tool_results")))))
    (should (equal (cdr (assoc "anthropic-beta" (plist-get context :headers)))
                   "context-management-test"))
    (should (equal (plist-get metadata :anthropic-context-management)
                   'requested))
    (should (equal (plist-get metadata :anthropic-beta-headers)
                   '("context-management-test")))))

(ert-deftest e-anthropic-test-request-context-omits-unused-beta-headers ()
  "Anthropic beta headers are sent only for active context-management requests."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY"
             :beta-headers ("context-management-test"))))
         (context
          (e-anthropic--request-context
           :provider 'eng-anthropic
           :messages '((:role user :content "hello"))
           :options '(:model "claude-test" :max-tokens 1024)))
         (body (json-parse-string (plist-get context :body)
                                  :object-type 'plist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object :json-false))
         (metadata (plist-get context :metadata)))
    (should-not (assoc "anthropic-beta" (plist-get context :headers)))
    (should-not (plist-member body :context_management))
    (should-not (plist-member metadata :anthropic-context-management))
    (should-not (plist-member metadata :anthropic-beta-headers))))

(ert-deftest e-anthropic-test-request-context-reports-top-level-cache-metadata ()
  "Top-level cache mode is visible in sanitized request metadata."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (context
          (e-anthropic--request-context
           :provider 'eng-anthropic
           :messages '((:role user :content "hello"))
           :options '(:model "claude-test"
                      :max-tokens 1024
                      :prompt-cache t
                      :prompt-cache-mode top-level)))
         (metadata (plist-get context :metadata)))
    (should (equal (plist-get metadata :anthropic-cache-mode) 'top-level))
    (should (equal (plist-get metadata :anthropic-cache-breakpoint)
                   'provider-managed))
    (should (eq (plist-get metadata :full-history) t))))

(ert-deftest e-anthropic-test-request-context-authorization-header ()
  "Bearer providers can send Authorization: Bearer instead of x-api-key."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((gw
             :name "Gateway"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :auth-header authorization
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (context (e-anthropic--request-context
                   :provider 'gw
                   :messages '((:role user :content "hi"))
                   :options '(:model "claude-test" :max-tokens 8))))
    (should (equal (cdr (assoc "Authorization" (plist-get context :headers)))
                   "Bearer test-token"))
    (should-not (assoc "x-api-key" (plist-get context :headers)))
    (should (assoc "anthropic-version" (plist-get context :headers)))))

(ert-deftest e-anthropic-test-request-context-prefixes-model ()
  "Provider model prefixes are applied to the request model id."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((bedrockish
             :name "Bedrock-ish"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY"
             :model-prefix "anthropic.")))
         (context (e-anthropic--request-context
                   :provider 'bedrockish
                   :messages '((:role user :content "hi"))
                   :options '(:model "claude-opus-4-8" :max-tokens 8))))
    (should (equal (plist-get (json-parse-string (plist-get context :body)
                                                 :object-type 'plist
                                                 :array-type 'list
                                                 :null-object nil
                                                 :false-object :json-false)
                              :model)
                   "anthropic.claude-opus-4-8"))))

(ert-deftest e-anthropic-test-sigv4-auth-is-not-yet-supported ()
  "Selecting a SigV4 provider signals a clear unsupported error."
  (let ((e-anthropic-model-providers
         '((bedrock
            :name "Amazon Bedrock"
            :base-url "https://bedrock-runtime.example.test"
            :auth sigv4))))
    (should-error
     (e-anthropic--request-context
      :provider 'bedrock
      :messages '((:role user :content "hi"))
      :options '(:model "claude-test" :max-tokens 8))
     :type 'e-anthropic-unsupported)))

(defconst e-anthropic-test--text-stream
  "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n\
event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"gateway answer\"}}\n\n\
event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"}}\n\n\
event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"
  "A minimal Messages text stream used by integration tests.")

(ert-deftest e-anthropic-test-backend-streams-through-injected-requester ()
  "The Anthropic backend streams parsed events from an injected requester."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY")))
         (seen nil)
         (captured nil)
         (backend
          (e-anthropic-backend-create
           :provider 'eng-anthropic
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (setq captured (list :url url :headers headers :body body))
              e-anthropic-test--text-stream)))))
    (e-backend-stream backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "claude-test" :max-tokens 1024)
                      :on-item (lambda (item) (push item seen)))
    (should (equal (nreverse seen)
                   '((:type assistant-delta :content "gateway answer")
                     (:type assistant-message :content "gateway answer")
                     (:type done :reason stop))))
    (should (equal (plist-get captured :url)
                   "https://gateway.example.test/v1/messages"))
    (should (equal (cdr (assoc "x-api-key" (plist-get captured :headers)))
                   "test-token"))))

(ert-deftest e-anthropic-test-harness-streams-prompt-flow ()
  "The Anthropic harness helper runs prompt to persisted assistant message."
  (let* ((process-environment
          (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
         (e-anthropic-model-providers
          '((eng-anthropic
             :name "Engineering Anthropic"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "ANTHROPIC_GATEWAY_KEY"
             :default-model "claude-default")))
         (harness
          (e-anthropic-create-harness
           :provider 'eng-anthropic
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers body)
              e-anthropic-test--text-stream)))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-prompt harness "session-1" "question")
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant)))
    (should (equal (plist-get (cadr (e-harness-messages harness "session-1"))
                              :content)
                   "gateway answer"))))

(ert-deftest e-anthropic-test-create-harness-default-options ()
  "The Anthropic harness seeds model, max-tokens, and effort defaults."
  (let ((e-anthropic-model-providers
         '((eng-anthropic
            :name "Engineering Anthropic"
            :base-url "https://gateway.example.test/v1"
            :auth bearer
            :env-key "ANTHROPIC_GATEWAY_KEY"
            :default-model "claude-default"))))
    (should (equal (e-harness-default-options
                    (e-anthropic-create-harness
                     :provider 'eng-anthropic
                     :request-function #'ignore))
                   (list :model "claude-default"
                         :max-tokens e-anthropic-default-max-tokens
                         :effort e-anthropic-default-effort)))))

(defconst e-anthropic-test--model-info-json
  (concat
   "{\"data\":["
   "{\"model_name\":\"claude-opus-4-8\","
   "\"model_info\":{\"max_input_tokens\":1000000,\"max_output_tokens\":128000}},"
   "{\"model_name\":\"claude-opus-4-5-20251101\","
   "\"model_info\":{\"max_input_tokens\":200000,\"max_output_tokens\":64000}},"
   "{\"model_name\":\"claude-haiku-4-5-20251001\","
   "\"model_info\":{\"max_input_tokens\":200000}}"
   "]}")
  "A representative LiteLLM `/model/info' payload for tests.")

(ert-deftest e-anthropic-test-context-window-reads-gateway-catalog ()
  "Context-window lookup reads max-input-tokens from the gateway catalog."
  (e-anthropic-reset-context-window-cache)
  (let ((calls 0))
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get)
               (lambda (&rest _) (cl-incf calls) e-anthropic-test--model-info-json)))
      (should (equal (e-anthropic-context-window "claude-opus-4-8") 1000000))
      ;; Gateway truth, not the public-docs number (which lists 4.5 as 1M).
      (should (equal (e-anthropic-context-window "claude-opus-4-5-20251101")
                     200000))
      (should (equal (e-anthropic-context-window "claude-haiku-4-5-20251001")
                     200000))
      ;; Unknown model -> nil (no static fallback).
      (should-not (e-anthropic-context-window "no-such-model"))
      ;; The catalog is fetched once and cached in-memory for the session.
      (should (= calls 1))))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-context-window-nil-when-gateway-unavailable ()
  "When the gateway query fails, context-window lookup returns nil (no fallback)."
  (e-anthropic-reset-context-window-cache)
  (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
            ((symbol-function 'e-anthropic--http-get)
             (lambda (&rest _) (signal 'e-anthropic-backend-error '("boom")))))
    (should-not (e-anthropic-context-window "claude-opus-4-8")))
  (e-anthropic-reset-context-window-cache))

(ert-deftest e-anthropic-test-reset-context-window-cache-forces-refetch ()
  "Resetting the cache makes the next lookup re-query the gateway."
  (e-anthropic-reset-context-window-cache)
  (let ((calls 0))
    (cl-letf (((symbol-function 'e-anthropic--headers) (lambda (&rest _) nil))
              ((symbol-function 'e-anthropic--http-get)
               (lambda (&rest _) (cl-incf calls) e-anthropic-test--model-info-json)))
      (e-anthropic-context-window "claude-opus-4-8")
      (e-anthropic-reset-context-window-cache)
      (e-anthropic-context-window "claude-opus-4-8")
      (should (= calls 2))))
  (e-anthropic-reset-context-window-cache))

(provide 'e-anthropic-test)

;;; e-anthropic-test.el ends here

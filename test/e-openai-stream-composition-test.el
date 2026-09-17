;;; e-openai-stream-composition-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI Responses stream and diagnostics composition.

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
    (e-harness-test-prompt-batch harness "session-1" "question")
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

(ert-deftest e-openai-test-parse-created-only-sse-is-not-non-stream-text ()
  "A valid Responses start event is not mislabeled as a non-stream body."
  (should-not
   (e-openai-codex-parse-stream
    "data: {\"type\":\"response.created\",\"response\":{\"id\":\"resp-1\",\"status\":\"in_progress\"}}\n\n")))

(ert-deftest e-openai-test-parse-incomplete-response-is-terminal ()
  "Official Responses incomplete events retain their terminal reason."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.incomplete\",\"response\":{\"status\":\"incomplete\",\"incomplete_details\":{\"reason\":\"max_output_tokens\"}}}\n\n")
    '((:type done :reason length)))))

(ert-deftest e-openai-test-parse-top-level-error-event ()
  "Official top-level Responses error events remain provider errors."
  (let* ((items
          (e-openai-codex-parse-stream
           "data: {\"type\":\"error\",\"code\":\"server_error\",\"message\":\"Generation failed\",\"param\":null}\n\n"))
         (item (car items)))
    (should (= (length items) 1))
    (should (eq (plist-get item :type) 'backend-error))
    (should (equal (plist-get item :content)
                   "server_error: Generation failed"))
    (should (equal (plist-get (plist-get item :payload) :code)
                   "server_error"))))

(ert-deftest e-openai-test-parsers-accept-crlf-sse-framing ()
  "Both HTTP wire APIs accept standard CRLF-delimited SSE events."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.output_text.done\",\"text\":\"ok\"}\r\n\r\ndata: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\r\n\r\n")
    '((:type assistant-message :content "ok")
      (:type done :reason stop))))
  (should
   (equal
    (e-openai-chat-completion-parse-stream
     "data: {\"choices\":[{\"delta\":{\"content\":\"ok\"},\"index\":0}]}\r\n\r\ndata: {\"choices\":[{\"finish_reason\":\"stop\",\"index\":0,\"delta\":{}}]}\r\n\r\n")
    '((:type assistant-delta :content "ok")
      (:type assistant-message :content "ok")
      (:type done :reason stop)))))

(ert-deftest e-openai-test-refusals-remain-assistant-output ()
  "Provider refusals settle as visible assistant output on both wire APIs."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.refusal.done\",\"refusal\":\"I cannot help with that.\"}\n\ndata: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")
    '((:type assistant-message :content "I cannot help with that.")
      (:type done :reason stop))))
  (should
   (equal
    (e-openai-chat-completion-parse-stream
     "data: {\"choices\":[{\"delta\":{\"refusal\":\"I cannot help with that.\"},\"index\":0}]}\n\ndata: {\"choices\":[{\"finish_reason\":\"stop\",\"index\":0,\"delta\":{}}]}\n\n")
    '((:type assistant-delta :content "I cannot help with that.")
      (:type assistant-message :content "I cannot help with that.")
      (:type done :reason stop)))))

(ert-deftest e-openai-test-premature-http-responses-stream-retries ()
  "A truncated Responses HTTP attempt retries instead of failing the turn."
  (let* ((process-environment
          (cons "OPENAI_PREMATURE_TEST_KEY=test-token" process-environment))
         (e-harness-retry-initial-backoff-seconds 0.01)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.01)
         (e-harness-retry-max-elapsed-seconds 1.0)
         (e-openai-model-providers
          '((premature-test
             :name "Premature Responses Test"
             :base-url "https://example.test/v1"
             :env-key "OPENAI_PREMATURE_TEST_KEY"
             :wire-api responses
             :responses-transport http
             :requires-openai-auth nil)))
         (attempts 0)
         (events nil)
         (harness
          (e-openai-create-harness
           :provider 'premature-test
           :model "test-model"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers body)
              (cl-incf attempts)
              (if (= attempts 1)
                  "data: {\"type\":\"response.created\",\"response\":{\"id\":\"resp-1\",\"status\":\"in_progress\"}}\n\n"
                "data: {\"type\":\"response.output_text.done\",\"text\":\"recovered\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n"))))))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 2.0)))
      (should (eq (plist-get settled :status) 'done)))
    (should (= attempts 2))
    (should (= 1 (seq-count (lambda (event)
                              (eq (plist-get event :type) 'turn-retrying))
                            events)))
    (should-not (seq-find (lambda (event)
                            (eq (plist-get event :type) 'turn-failed))
                          events))
    (should
     (equal (plist-get (car (last (e-harness-messages harness "session-1")))
                       :content)
            "recovered"))))

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

(ert-deftest e-openai-test-parse-tool-arguments-preserves-canonical-json ()
  "Responses tool arguments retain nested arrays, objects, false, and null."
  (let* ((items
          (e-openai-codex-parse-stream
           "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call-json\",\"name\":\"inspect\",\"arguments\":\"{\\\"object\\\":{},\\\"array\\\":[],\\\"flags\\\":[false,null],\\\"items\\\":[{\\\"empty\\\":{},\\\"values\\\":[1,false]}]}\"}}\n\n"))
         (arguments (plist-get (car items) :arguments)))
    (should (equal arguments
                   '(:object nil
                     :array []
                     :flags [:json-false :json-null]
                     :items [(:empty nil :values [1 :json-false])])))))

(ert-deftest e-openai-test-parse-encrypted-reasoning-for-stateless-replay ()
  "Encrypted reasoning output becomes an opaque provider replay item."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"reasoning\",\"id\":\"rs-1\",\"encrypted_content\":\"ciphertext\",\"summary\":null}}\n\n")
    '((:type provider-replay-item
       :provider-id openai
       :item (:type "reasoning"
              :id "rs-1"
              :encrypted_content "ciphertext"
              :summary :json-null))))))

(ert-deftest e-openai-test-parse-reasoning-summary-presence-is-diagnostic-only ()
  "Encrypted reasoning preserves whether summary is absent, empty, or present."
  (dolist (case
           '(("" .
              "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"reasoning\",\"encrypted_content\":\"ciphertext\"}}\n\n")
             (":summary:[]" .
              "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"reasoning\",\"encrypted_content\":\"ciphertext\",\"summary\":[]}}\n\n")
             (":summary:[{...}]" .
              "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"reasoning\",\"encrypted_content\":\"ciphertext\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"diagnostic\"}]}}\n\n")))
    (let* ((parsed (e-openai-codex-parse-stream (cdr case)))
           (item (plist-get (car parsed) :item)))
      (if (string-empty-p (car case))
          (should-not (plist-member item :summary))
        (should (plist-member item :summary))))))

(ert-deftest e-openai-test-parse-tool-call-event ()
  "Responses tool_call items become backend-neutral tool calls."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"tool_call\",\"id\":\"call-1\",\"name\":\"glob\",\"arguments\":{\"uri\":\"session:///\",\"limit\":20}}}\n\n")
    '((:type tool-call
      :id "call-1"
      :name "glob"
      :arguments (:uri "session:///" :limit 20))))))

(ert-deftest e-openai-test-parse-reasoning-summary-delta ()
  "Responses reasoning summary deltas become backend-neutral reasoning items."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"checking context\"}\n\n")
    '((:type reasoning-delta
       :stream-kind summary
       :content "checking context")))))

(ert-deftest e-openai-test-parse-raw-reasoning-delta ()
  "Responses raw reasoning deltas remain distinct from visible summary deltas."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.reasoning_text.delta\",\"delta\":\"private details\"}\n\n")
    '((:type reasoning-raw-delta
       :stream-kind raw
       :content "private details")))))

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
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\",\"usage\":{\"input_tokens\":202598,\"input_tokens_details\":{\"cached_tokens\":7552,\"cache_write_tokens\":4096},\"output_tokens\":419,\"output_tokens_details\":{\"reasoning_tokens\":139},\"total_tokens\":203017}}}\n\n")
    '((:type assistant-message :content "ok")
      (:type token-usage
       :usage (:input-tokens 202598
               :cached-input-tokens 7552
               :cache-creation-input-tokens 4096
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

(ert-deftest e-openai-test-parse-completed-response-id-records-layout ()
  "Responses anchors retain the prompt layout needed for safe continuation."
  (should
   (equal
    (e-openai-codex-parse-stream
     "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"status\":\"completed\"}}\n\n"
     e-openai-gpt56-explicit-cache-layout-revision)
    `((:type provider-anchor-candidate
       :provider-id openai
       :metadata (:response-id "resp-1"
                  :prompt-layout-revision
                  ,e-openai-gpt56-explicit-cache-layout-revision))
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

(ert-deftest e-openai-test-parse-stream-does-not-retain-raw-response-by-default ()
  "Raw provider responses are not retained unless debug mode is enabled."
  (let ((e-openai-codex-debug nil)
        (buffer-name e-openai-codex-raw-responses-buffer-name)
        (stream "data: {\"type\":\"response.completed\"}\n\n"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (unwind-protect
        (progn
          (e-openai-codex-parse-stream stream)
          (should (string-prefix-p " " buffer-name))
          (should-not (get-buffer buffer-name)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest e-openai-test-parse-json-error-response ()
  "Non-stream provider JSON errors become backend error items."
  (should
   (equal
    (e-openai-codex-parse-stream
     "{\"error\":{\"message\":\"Invalid schema\",\"type\":\"invalid_request_error\"}}")
    '((:type backend-error
      :content "invalid_request_error: Invalid schema"
      :payload (:error (:message "Invalid schema"
                       :type "invalid_request_error")))))))

(ert-deftest e-openai-test-parse-json-server-error-is-retryable ()
  "A non-stream JSON server error keeps the code used by retry policy."
  (let* ((item
          (car
           (e-openai-codex-parse-stream
            "{\"error\":{\"message\":\"Generation failed\",\"code\":\"server_error\"}}")))
         (content (plist-get item :content)))
    (should (equal content "server_error: Generation failed"))
    (let ((details (e-openai-diagnostics-normalize-error-details
                    content (plist-get item :payload) nil)))
      (should (eq (plist-get details :retryable) t))
      (should (eq (plist-get details :retry-reason) 'provider-unavailable)))))

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
                   (concat "context_length_exceeded: "
                           "Your input exceeds the context window of this model.")))
    (should (equal (plist-get (plist-get item :payload) :type)
                   "response.failed"))
    (should (equal (plist-get
                    (plist-get
                     (plist-get (plist-get item :payload) :response)
                     :error)
                    :code)
                   "context_length_exceeded"))))

(ert-deftest e-openai-test-response-failed-server-error-is-retryable ()
  "Official Responses server_error failures retain their retryable code."
  (let* ((item
          (car
           (e-openai-codex-parse-stream
            "data: {\"type\":\"response.failed\",\"response\":{\"status\":\"failed\",\"error\":{\"code\":\"server_error\",\"message\":\"The model failed to generate a response.\"}}}\n\n")))
         (content (plist-get item :content)))
    (should (equal content
                   "server_error: The model failed to generate a response."))
    (let ((details (e-openai-diagnostics-normalize-error-details
                    content (plist-get item :payload) nil)))
      (should (eq (plist-get details :retryable) t))
      (should (eq (plist-get details :retry-reason) 'provider-unavailable)))))

(provide 'e-openai-stream-composition-test)

;;; e-openai-stream-composition-test.el ends here

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
      :parallel_tool_calls t))))

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
      :parallel_tool_calls t))))

(ert-deftest e-openai-test-request-body-includes-function-call-before-output ()
  "Tool-call transcript messages serialize before function-call outputs."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role user :content "hello")
                 (:role tool-call
                  :content (:id "call-1"
                            :name "read_buffer"
                            :arguments (:name "README.md")))
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
               :name "read_buffer"
               :arguments "{\"name\":\"README.md\"}")
              (:type "function_call_output"
               :call_id "call-1"
               :output "(:ok t)")]
      :tool_choice "auto"
      :parallel_tool_calls t))))

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

(ert-deftest e-openai-test-default-http-request-accepts-keyword-arguments ()
  "The default HTTP requester accepts the keyword call shape used by backend."
  (let (captured-url captured-method captured-headers captured-body)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest _args)
                 (setq captured-url url)
                 (setq captured-method url-request-method)
                 (setq captured-headers url-request-extra-headers)
                 (setq captured-body url-request-data)
                 (let ((buffer (generate-new-buffer " *e-openai-test-http*")))
                   (with-current-buffer buffer
                     (insert "HTTP/1.1 200 OK\n\n"
                             "data: {\"type\":\"response.completed\"}\n\n"))
                   buffer))))
      (should
       (equal
        (e-openai-codex--http-request
         :url "https://example.test/codex/responses"
         :headers '(("Authorization" . "Bearer test"))
         :body "{}")
        "data: {\"type\":\"response.completed\"}\n\n"))
      (should (equal captured-url "https://example.test/codex/responses"))
      (should (equal captured-method "POST"))
      (should (equal captured-headers '(("Authorization" . "Bearer test"))))
      (should (equal (decode-coding-string captured-body 'utf-8) "{}")))))

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

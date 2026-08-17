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
(require 'e-dev-profile)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-openai)
(require 'url-http)

(defvar url-current-object)
(defvar url-extensions-header)
(defvar url-http-attempt-keepalives)
(defvar url-http-data)
(defvar url-http-extra-headers)
(defvar url-http-method)
(defvar url-http-proxy)
(defvar url-http-real-basic-auth-storage)
(defvar url-http-referer)
(defvar url-http-target-url)
(defvar url-http-version)
(defvar url-mime-encoding-string)

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

(defun e-openai-test--without-explicit-cache-fields (body)
  "Return JSON-like BODY without provider-specific explicit cache fields."
  (let ((copy (json-parse-string
               (json-encode body)
               :object-type 'plist
               :array-type 'array
               :null-object nil
               :false-object :json-false)))
    (cl-remf copy :prompt_cache_options)
    (dolist (item (append (plist-get copy :input) nil))
      (dolist (content (append (plist-get item :content) nil))
        (when (listp content)
          (cl-remf content :prompt_cache_breakpoint))))
    copy))

(ert-deftest e-openai-test-auth-file-uses-codex-home ()
  "Codex auth file resolution honors CODEX_HOME."
  (let ((process-environment
         (cons "CODEX_HOME=/tmp/e-codex-home" process-environment)))
    (should (equal (e-openai-codex-auth-file)
                   "/tmp/e-codex-home/auth.json"))))

(ert-deftest e-openai-test-websocket-timeout-default-migrates-on-reload ()
  "Live old WebSocket timeout defaults migrate when uncustomized."
  (let* ((symbol 'e-openai-websocket-idle-timeout-seconds)
         (saved (get symbol 'saved-value))
         (customized (get symbol 'customized-value))
         (theme (get symbol 'theme-value)))
    (unwind-protect
        (progn
          (put symbol 'saved-value nil)
          (put symbol 'customized-value nil)
          (put symbol 'theme-value nil)
          (let ((e-openai-websocket-idle-timeout-seconds nil))
            (e-openai--migrate-websocket-idle-timeout-default)
            (should (equal e-openai-websocket-idle-timeout-seconds 60)))
          (let ((e-openai-websocket-idle-timeout-seconds 180))
            (e-openai--migrate-websocket-idle-timeout-default)
            (should (equal e-openai-websocket-idle-timeout-seconds 60)))
          (put symbol 'saved-value '(nil))
          (let ((e-openai-websocket-idle-timeout-seconds nil))
            (e-openai--migrate-websocket-idle-timeout-default)
            (should-not e-openai-websocket-idle-timeout-seconds)))
      (put symbol 'saved-value saved)
      (put symbol 'customized-value customized)
      (put symbol 'theme-value theme))))

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
      :text (:verbosity "low")
      :reasoning (:effort "high")))))

(ert-deftest e-openai-test-request-body-maps-explicit-text-verbosity ()
  "OpenAI request bodies map backend-neutral text verbosity."
  (should
   (equal
    (e-openai-codex-request-body
     :messages '((:role user :content "hello"))
     :options '(:model "gpt-test" :text-verbosity "high"))
    '(:model "gpt-test"
      :store :json-false
      :stream t
      :instructions "You are a helpful assistant."
      :input [(:type "message"
               :role "user"
               :content [(:type "input_text" :text "hello")])]
      :tool_choice "auto"
      :parallel_tool_calls t
      :text (:verbosity "high")
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

(ert-deftest e-openai-test-gpt56-marks-stable-context-cache-boundary ()
  "GPT-5.6 caches only the stable system prefix in explicit mode."
  (let* ((body
          (e-openai-codex-request-body
           :messages '((:role system :content "Static instructions.")
                       (:role system :content "Stable project guidance.")
                       (:role system :content "Current buffer state.")
                       (:role user :content "hello"))
           :options
           '(:model "gpt-5.6-sol"
             :instructions "Base instructions."
             :prompt-cache-key "cache-key"
             :segments ((:kind static-prefix
                         :id static
                         :messages ((:role system
                                     :content "Static instructions.")))
                        (:kind stable-context
                         :id stable
                         :messages ((:role system
                                     :content "Stable project guidance.")))
                        (:kind current-state
                         :id dynamic
                         :messages ((:role system
                                     :content "Current buffer state.")))))))
         (input (append (plist-get body :input) nil))
         (static-block (aref (plist-get (nth 0 input) :content) 0))
         (stable-block (aref (plist-get (nth 1 input) :content) 0))
         (dynamic-block (aref (plist-get (nth 2 input) :content) 0)))
    (should (equal (plist-get body :instructions) "Base instructions."))
    (should (equal (mapcar (lambda (item) (plist-get item :role)) input)
                   '("developer" "developer" "developer" "user")))
    (should-not (plist-member static-block :prompt_cache_breakpoint))
    (should (equal (plist-get stable-block :prompt_cache_breakpoint)
                   '(:mode "explicit")))
    (should-not (plist-member dynamic-block :prompt_cache_breakpoint))
    (should (equal (plist-get body :prompt_cache_options)
                   '(:mode "explicit")))
    (should (equal (plist-get body :prompt_cache_key) "cache-key"))))

(ert-deftest e-openai-test-gpt55-keeps-flattened-cache-prefix-shape ()
  "Older models keep automatic caching and flattened system instructions."
  (let ((body
         (e-openai-codex-request-body
          :messages '((:role system :content "Stable instructions.")
                      (:role system :content "Dynamic state.")
                      (:role user :content "hello"))
          :options
          '(:model "gpt-5.5"
            :instructions "Base instructions."
            :prompt-cache-key "cache-key"
            :segments ((:kind static-prefix
                        :messages ((:role system
                                    :content "Stable instructions.")))
                       (:kind current-state
                        :messages ((:role system
                                    :content "Dynamic state."))))))))
    (should (equal (plist-get body :instructions)
                   "Base instructions.\n\nStable instructions.\n\nDynamic state."))
    (should (equal (length (plist-get body :input)) 1))
    (should-not (plist-member body :prompt_cache_options))))

(ert-deftest e-openai-test-gpt56-without-cache-key-keeps-flattened-shape ()
  "GPT-5.6 does not select explicit mode without its stable routing key."
  (let ((body
         (e-openai-codex-request-body
          :messages '((:role system :content "Stable instructions.")
                      (:role system :content "Dynamic state.")
                      (:role user :content "hello"))
          :options
          '(:model "gpt-5.6-sol"
            :segments ((:kind static-prefix
                        :messages ((:role system
                                    :content "Stable instructions.")))
                       (:kind current-state
                        :messages ((:role system
                                    :content "Dynamic state."))))))))
    (should (string-match-p "Stable instructions"
                            (plist-get body :instructions)))
    (should (equal (length (plist-get body :input)) 1))
    (should-not (plist-member body :prompt_cache_options))))

(ert-deftest e-openai-test-gpt56-continuation-reuses-carried-breakpoint ()
  "A matching layout anchor sends dynamic context without stable duplication."
  (let* ((revision e-openai-gpt56-explicit-cache-layout-revision)
         (body
          (e-openai-codex-request-body
           :messages '((:role system :content "Stable instructions.")
                       (:role system :content "Fresh dynamic state.")
                       (:role user :content "old prompt")
                       (:role assistant :content "old answer")
                       (:role user :content "new prompt"))
           :options
           `(:model "gpt-5.6-sol"
             :prompt-cache-key "cache-key"
             :provider-continuation t
             :provider-anchor
             (:provider-id openai
              :metadata (:response-id "resp-1"
                         :prompt-layout-revision ,revision))
             :provider-anchor-delta-messages
             ((:role system :content "Fresh dynamic state.")
              (:role user :content "new prompt"))
             :segments ((:kind static-prefix
                         :messages ((:role system
                                     :content "Stable instructions.")))
                        (:kind current-state
                         :messages ((:role system
                                     :content "Fresh dynamic state.")))))))
         (input (append (plist-get body :input) nil)))
    (should (equal (plist-get body :previous_response_id) "resp-1"))
    (should (equal (mapcar (lambda (item) (plist-get item :role)) input)
                   '("developer" "user")))
    (should (equal (plist-get
                    (aref (plist-get (car input) :content) 0) :text)
                   "Fresh dynamic state."))
    (should-not (plist-member
                 (aref (plist-get (car input) :content) 0)
                 :prompt_cache_breakpoint))
    (should (equal (plist-get body :prompt_cache_options)
                   '(:mode "explicit")))))

(ert-deftest e-openai-test-gpt56-segmented-continuation-needs-no-breakpoint ()
  "A segmented implicit profile carries stable context without unsupported fields."
  (let* ((revision e-openai-gpt56-segmented-context-layout-revision)
         (body
          (e-openai-codex-request-body
           :messages '((:role system :content "Stable instructions.")
                       (:role system :content "Fresh dynamic state.")
                       (:role user :content "old prompt")
                       (:role assistant :content "old answer")
                       (:role user :content "new prompt"))
           :options
           `(:model "gpt-5.6-sol"
             :prompt-cache-key "cache-key"
             :prompt-cache-breakpoint-mode nil
             :responses-context-layout developer-input
             :provider-continuation t
             :responses-transport websocket
             :response-store :json-false
             :provider-anchor
             (:provider-id openai
              :metadata (:response-id "resp-1"
                         :prompt-layout-revision ,revision))
             :provider-anchor-delta-messages
             ((:role system :content "Fresh dynamic state.")
              (:role user :content "new prompt"))
             :segments ((:kind stable-context
                         :messages ((:role system
                                     :content "Stable instructions.")))
                        (:kind current-state
                         :messages ((:role system
                                     :content "Fresh dynamic state.")))))))
         (input (append (plist-get body :input) nil))
         (dynamic-block (aref (plist-get (car input) :content) 0)))
    (should (equal (plist-get body :instructions)
                   "You are a helpful assistant."))
    (should (equal (plist-get body :previous_response_id) "resp-1"))
    (should (equal (mapcar (lambda (item) (plist-get item :role)) input)
                   '("developer" "user")))
    (should (equal (plist-get dynamic-block :text) "Fresh dynamic state."))
    (should-not (plist-member dynamic-block :prompt_cache_breakpoint))
    (should-not (plist-member body :prompt_cache_options))))

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
                        :prompt-layout-revision ,revision))
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
           :options (e-openai-codex--without-provider-anchor options)))
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
            :summary [(:type "summary_text" :text "kept")]))
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
    (should (equal (nth 1 input) reasoning-item))))

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
                                       :metadata (:response-id "resp-1"))
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

(ert-deftest e-openai-test-request-body-websocket-store-false-uses-local-anchor ()
  "Unstored WebSocket requests can use the active connection's response anchor."
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
    (should (equal (plist-get body :previous_response_id) "resp-1"))
    (should (equal (plist-get body :input)
                   [(:type "message"
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

(ert-deftest e-openai-test-profile-records-request-context-spans ()
  "Enabled dev profiling records OpenAI request construction subspans."
  (let* ((profile-directory (make-temp-file "e-openai-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((eng-responses
             :name "Engineering Responses"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :requires-openai-auth nil))))
    (unwind-protect
        (progn
          (e-dev-profile-start)
          (e-openai--request-context
           :provider 'eng-responses
           :messages '((:role user :content "new prompt"))
           :options '(:model "gpt-test"
                      :tools ((:name "lookup"
                               :description "Lookup."
                               :parameters (:type "object")))))
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (dolist (event '("openai.request-context"
                             "openai.request-body"
                             "openai.request-json"
                             "openai.request-headers"))
              (should (alist-get event aggregates nil nil #'equal)))))
      (delete-directory profile-directory t))))

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
          (e-backend-stream-batch backend
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
    (e-backend-stream-batch backend
                      :messages '((:role user :content "hello"))
                      :options '(:model "gateway-model"
                                 :prompt-cache-key "cache-key"
                                 :prompt-cache-retention "24h")
                      :on-item #'ignore)
    (should (equal (alist-get 'prompt_cache_key captured) "cache-key"))
    (should (equal (alist-get 'prompt_cache_retention captured) "24h"))))

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
              (setq captured (json-read-from-string body))
              "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n")))))
    (e-backend-stream-batch
     backend
     :messages '((:role user :content "hello"))
     :options '(:model "gpt-5.6-sol"
                :prompt-cache-key "cache-key"
                :prompt-cache-retention "24h")
     :on-item #'ignore)
    (should (equal (alist-get 'prompt_cache_key captured) "cache-key"))
    (should-not (assq 'prompt_cache_retention captured))
    (should-not (assq 'prompt_cache_options captured))))

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
                     :provider-continuation t
                     :provider-anchor-provider-id openai)))))

(ert-deftest e-openai-test-default-harness-uses-codex-websocket-continuation ()
  "The built-in Codex profile uses connection-local WebSocket continuation."
  (should (equal (e-harness-default-options
                  (e-openai-create-harness :request-function #'ignore))
                 '(:model "gpt-5.5"
                   :reasoning-effort "high"
                   :provider-continuation t
                   :provider-anchor-provider-id openai))))

(ert-deftest e-openai-test-codex-profile-uses-required-unstored-websocket-mode ()
  "Codex WebSocket requests explicitly use the backend-required store=false."
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
                   :messages '((:role system :content "stable instructions")
                               (:role user :content "hello"))
                   :options '(:model "gpt-5.6-sol"
                              :prompt-cache-key "cache-key"
                              :segments
                              ((:kind stable-context
                                :messages
                                ((:role system
                                  :content "stable instructions")))))))
                 (body (json-parse-string (plist-get context :body)
                                          :object-type 'plist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object :json-false)))
            (should (eq (plist-get context :responses-transport) 'websocket))
            (should (eq (plist-get body :store) :json-false))
            (should-not (plist-member body :stream))
            (should-not (plist-member body :prompt_cache_options))
            (should (equal (plist-get body :instructions)
                           "You are a helpful assistant."))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   (plist-get body :input))
                           '("developer" "user")))
            (should-not
             (plist-member
              (car (plist-get (car (plist-get body :input)) :content))
              :prompt_cache_breakpoint))
            (should (equal (plist-get context :prompt-layout-revision)
                           e-openai-gpt56-segmented-context-layout-revision))
            (should (equal
                     (plist-get
                      (plist-get (plist-get context :metadata) :diagnostics)
                      :prompt-cache-mode)
                     "implicit-segmented"))
            (should (eq (plist-get (plist-get (plist-get context :metadata)
                                              :diagnostics)
                                   :response-store)
                        :json-false))))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

(ert-deftest e-openai-test-normalizes-builtin-codex-wire-requirements ()
  "Reloaded built-in Codex profiles retain store=false and disable breakpoints."
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
       :response-store :json-false
       :continuation t
       :requires-openai-auth t
       :prompt-cache-breakpoint-mode nil
       :responses-context-layout developer-input
       :include-encrypted-reasoning t)
      (custom-codex
       :name "Custom Codex"
       :base-url ,(concat e-openai-codex-default-base-url "/codex")
       :wire-api responses
       :responses-transport websocket
       :response-store :json-false
       :continuation t
       :requires-openai-auth t)
      (openai
       :name "OpenAI API"
       :base-url ,e-openai-api-default-base-url
       :wire-api responses
       :responses-transport websocket
       :response-store :json-false
       :responses-context-layout developer-input
       :prompt-cache-breakpoint-mode explicit
       :include-encrypted-reasoning t
       :continuation t
       :requires-openai-auth nil
       :env-key "OPENAI_API_KEY"
       :default-model "gpt-5.6")))))

(ert-deftest e-openai-test-provider-profile-normalizes-builtin-codex-requirements ()
  "Provider lookup applies current built-in Codex cache and store constraints."
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
      (should (plist-get profile :include-encrypted-reasoning)))))

(ert-deftest e-openai-test-canonical-api-profile-shares-responses-contract ()
  "The first-party API profile differs from Codex only in provider capability."
  (let ((profile (e-openai-provider-profile 'openai)))
    (should (equal (plist-get profile :base-url)
                   e-openai-api-default-base-url))
    (should (equal (plist-get profile :env-key) "OPENAI_API_KEY"))
    (should-not (plist-get profile :requires-openai-auth))
    (should (eq (plist-get profile :responses-transport) 'websocket))
    (should (eq (plist-get profile :response-store) :json-false))
    (should (eq (plist-get profile :responses-context-layout)
                'developer-input))
    (should (eq (plist-get profile :prompt-cache-breakpoint-mode) 'explicit))
    (should (plist-get profile :include-encrypted-reasoning))
    (should (plist-get profile :continuation))
    (should (equal (plist-get profile :default-model) "gpt-5.6"))))

(ert-deftest e-openai-test-codex-and-api-profiles-render-equivalent-common-body ()
  "Canonical providers share one body modulo explicit-cache capability."
  (let* ((process-environment
          (cons "OPENAI_API_KEY=test-api-token" process-environment))
         (auth-file (make-temp-file "e-openai-auth" nil ".json"))
         (auth (json-encode
                (list :tokens
                      (list :access_token (e-openai-test--jwt)
                            :refresh_token "refresh"))))
         (messages '((:role system :content "stable guidance")
                     (:role system :content "dynamic state")
                     (:role user :content "hello")))
         (options '(:model "gpt-5.6"
                    :prompt-cache-key "shared-key"
                    :segments
                    ((:kind stable-context
                      :messages ((:role system :content "stable guidance")))
                     (:kind current-state
                      :messages ((:role system :content "dynamic state")))))))
    (unwind-protect
        (progn
          (with-temp-file auth-file
            (insert auth))
          (let* ((codex
                  (e-openai--request-context
                   :provider 'codex
                   :auth-file auth-file
                   :messages messages
                   :options options))
                 (api
                  (e-openai--request-context
                   :provider 'openai
                   :messages messages
                   :options options))
                 (codex-body (plist-get codex :body-data))
                 (api-body (plist-get api :body-data)))
            (should-not (plist-member codex-body :prompt_cache_options))
            (should (equal (plist-get api-body :prompt_cache_options)
                           '(:mode "explicit")))
            (should
             (equal
              (e-openai-test--without-explicit-cache-fields codex-body)
              (e-openai-test--without-explicit-cache-fields api-body)))))
      (when (file-exists-p auth-file)
        (delete-file auth-file)))))

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

(ert-deftest e-openai-test-complete-http-response-classifies-truncated-json ()
  "An SSE event cut mid-JSON is a retryable premature stream."
  (dolist (case
           '((responses
              . "data: {\"type\":\"response.output_text.delta\",\"delta\":\"cut")
             (chat-completion
              . "data: {\"choices\":[{\"delta\":{\"content\":\"cut")))
    (let* ((wire-api (car case))
           (items (e-openai--complete-response-items
                   (cdr case) (list :wire-api wire-api)))
           (item (car items)))
      (should (= (length items) 1))
      (should (eq (plist-get item :type) 'backend-error))
      (should (string-match-p "premature"
                              (downcase (plist-get item :content)))))))

(ert-deftest e-openai-test-complete-http-response-classifies-empty-body ()
  "An empty successful HTTP body is a retryable premature stream."
  (dolist (wire-api '(responses chat-completion))
    (let* ((items (e-openai--complete-response-items
                   "" (list :wire-api wire-api)))
           (item (car items)))
      (should (= (length items) 1))
      (should (eq (plist-get item :type) 'backend-error))
      (should (string-match-p "premature"
                              (downcase (plist-get item :content)))))))

(ert-deftest e-openai-test-chat-completion-classifies-non-stream-body ()
  "Chat Completions preserves an unexpected gateway body as an error."
  (let* ((items (e-openai--complete-response-items
                 "<html><body>upstream unavailable</body></html>"
                 '(:wire-api chat-completion)))
         (item (car items)))
    (should (= (length items) 1))
    (should (eq (plist-get item :type) 'backend-error))
    (should (string-match-p "Chat Completions stream"
                            (plist-get item :content)))
    (should (eq (plist-get (plist-get item :payload) :response-kind)
                'html))))

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

(ert-deftest e-openai-test-complete-http-response-rejects-premature-sse ()
  "A completed HTTP body without a terminal event is a retryable failure."
  (dolist (wire-api '(responses chat-completion))
    (let (items)
      (e-openai--emit-response-items
       (if (eq wire-api 'responses)
           "data: {\"type\":\"response.created\",\"response\":{\"id\":\"resp-1\",\"status\":\"in_progress\"}}\n\n"
         "data: {\"choices\":[{\"delta\":{\"content\":\"partial\"},\"index\":0}]}\n\n")
       (list :wire-api wire-api)
       (lambda (item) (push item items)))
      (should (= (length items) 1))
      (let ((item (car items)))
        (should (eq (plist-get item :type) 'backend-error))
        (should (string-match-p "premature"
                                (downcase (plist-get item :content))))
        (should (eq (plist-get (plist-get item :payload) :wire-api)
                    wire-api))
        (should (eq (plist-get (plist-get item :payload) :response-kind)
                    'sse))))))

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
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
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

(ert-deftest e-openai-test-http-server-error-retries-with-status-metadata ()
  "A generic HTTP 5xx body retries using preserved transport metadata."
  (let* ((process-environment
          (cons "OPENAI_HTTP_ERROR_TEST_KEY=test-token" process-environment))
         (e-harness-retry-initial-backoff-seconds 0.01)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.01)
         (e-harness-retry-max-elapsed-seconds 1.0)
         (e-openai-model-providers
          '((http-error-test
             :name "HTTP Error Responses Test"
             :base-url "https://example.test/v1"
             :env-key "OPENAI_HTTP_ERROR_TEST_KEY"
             :wire-api responses
             :responses-transport http
             :requires-openai-auth nil)))
         (attempts 0)
         (events nil)
         (harness
          (e-openai-create-harness
           :provider 'http-error-test
           :model "test-model"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers body)
              (cl-incf attempts)
              (if (= attempts 1)
                  (e-openai--http-response-create
                   :status 503
                   :retry-after 0.01
                   :body "{\"error\":{\"message\":\"Generation failed\"}}")
                "data: {\"type\":\"response.output_text.done\",\"text\":\"recovered\"}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n"))))))
    (e-harness--install-activity-sink harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 2.0)))
      (should (eq (plist-get settled :status) 'done)))
    (should (= attempts 2))
    (should (= 1 (seq-count (lambda (event)
                              (eq (plist-get event :type) 'turn-retrying))
                            events)))
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
              :summary nil))))))

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

(ert-deftest e-openai-test-debug-raw-response-retention-is-bounded ()
  "Debug raw payload retention keeps only the configured trailing byte budget."
  (let ((e-openai-codex-debug t)
        (e-openai-codex-raw-responses-max-bytes 128)
        (e-openai-codex--last-diagnostics nil)
        (buffer-name " *e-openai-codex-raw-responses-test*")
        (stream (concat (make-string 512 ?x) "END")))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (let ((e-openai-codex-raw-responses-buffer-name buffer-name))
      (unwind-protect
          (progn
            (e-openai-codex-parse-stream stream)
            (with-current-buffer buffer-name
              (should (<= (string-bytes (buffer-string)) 128))
              (should (string-suffix-p "END\n" (buffer-string))))
            (let ((raw (plist-get e-openai-codex--last-diagnostics
                                  :raw-response)))
              (should (<= (string-bytes raw) 128))
              (should (string-suffix-p "END" raw))))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))))))

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
    (should (e-harness--retryable-error-p content (plist-get item :payload)))))

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
    (should (e-harness--retryable-error-p content (plist-get item :payload)))))

(ert-deftest e-openai-test-response-error-message-bounds-large-message ()
  "Responses failure message text is capped before becoming diagnostics."
  (let* ((e-openai-diagnostic-string-max-bytes 32)
         (text (make-string 200 ?x))
         (message
          (e-openai-codex--response-error-message
           `(:type "response.failed"
             :response (:error (:message ,text))))))
    (should (< (string-bytes message) 180))
    (should (string-prefix-p (make-string 32 ?x) message))
    (should (string-match-p "OpenAI diagnostic string truncated" message))))

(ert-deftest e-openai-test-response-error-message-bounds-fallback-event ()
  "Unexpected Responses failure events are printed through a bounded preview."
  (let* ((e-openai-diagnostic-print-length 4)
         (e-openai-diagnostic-print-level 3)
         (e-openai-diagnostic-string-max-bytes 48)
         (e-openai-diagnostic-result-max-bytes 220)
         (event `(:type "response.failed"
                  :unexpected ,(make-list 30 (make-string 100 ?z))))
         (message (e-openai-codex--response-error-message event)))
    (should (< (string-bytes message) 360))
    (should (string-match-p "OpenAI diagnostic" message))
    (should-not (string-match-p (make-string 80 ?z) message))))


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
               :cache-creation-input-tokens nil
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
    (e-harness-test-prompt-batch harness "session-1" "question")
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

(ert-deftest e-openai-test-sync-http-request-rejects-hot-path-before-start ()
  "The synchronous Codex HTTP wrapper fails before starting transport in hot paths."
  (let (started)
    (cl-letf (((symbol-function 'e-openai-codex--http-request-start)
               (lambda (&rest _args)
                 (setq started t)
                 (error "transport should not start"))))
      (let ((err (should-error
                  (e-request-with-hot-path 'openai-sync-http
                    (e-openai-codex--http-request
                     :url "https://example.test/codex/responses"
                     :headers nil
                     :body "{}"))
                  :type 'e-request-blocking-call-in-hot-path)))
        (should (equal (cdr err)
                       '(e-openai-codex--http-request openai-sync-http))))
      (should-not started))))

(ert-deftest e-openai-test-sync-backend-stream-rejects-hot-path-before-request ()
  "The provider sync stream wrapper fails before issuing sync requests."
  (let ((backend (e-openai-backend-create
                  :provider 'codex
                  :request-function
                  (lambda (&rest _args)
                    (error "request function should not run")))))
    (let ((err (should-error
                (e-request-with-hot-path 'openai-stream
                  (funcall (e-backend--stream backend)
                           :messages nil
                           :options nil
                           :on-item #'ignore))
                :type 'e-request-blocking-call-in-hot-path)))
      (should (equal (cdr err)
                     '(e-openai-backend-stream openai-stream))))))

(ert-deftest e-openai-test-default-http-request-start-returns-error-body ()
  "HTTP error bodies retain status metadata through backend parsing."
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (_url callback &rest _args)
               (let ((buffer (generate-new-buffer " *e-openai-test-http*")))
                 (with-current-buffer buffer
                   (setq-local url-http-response-status 503)
                   (insert "HTTP/1.1 503 Service Unavailable\n\n"
                           "{\"error\":{\"message\":\"Generation failed\"}}"))
                 (with-current-buffer buffer
                   (funcall callback '(:error (error http 503))))
                 buffer))))
    (let (response error)
      (e-openai-codex--http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete (lambda (value) (setq response value))
       :on-error (lambda (err) (setq error err)))
      (should-not error)
      (should (e-openai--http-response-p response))
      (let* ((item (car (e-openai--complete-response-items
                         response '(:wire-api responses))))
             (payload (plist-get item :payload)))
        (should (equal (plist-get item :content) "Generation failed"))
        (should (= (plist-get payload :status) 503))
        (should (e-harness--retryable-error-p
                 (plist-get item :content) payload))))))

(ert-deftest e-openai-test-default-http-request-preserves-empty-rate-limit ()
  "An empty HTTP 429 body retains status and Retry-After for the harness."
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (_url callback &rest _args)
               (let ((buffer (generate-new-buffer " *e-openai-test-http*")))
                 (with-current-buffer buffer
                   (setq-local url-http-response-status 429)
                   (insert "HTTP/1.1 429 Too Many Requests\nRetry-After: 7\n\n"))
                 (with-current-buffer buffer
                   (funcall callback '(:error (error http 429))))
                 buffer))))
    (let (response error)
      (e-openai-codex--http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete (lambda (value) (setq response value))
       :on-error (lambda (err) (setq error err)))
      (should-not error)
      (let* ((item (car (e-openai--complete-response-items
                         response '(:wire-api responses))))
             (payload (plist-get item :payload)))
        (should (eq (plist-get item :type) 'backend-error))
        (should (= (plist-get payload :status) 429))
        (should (= (plist-get payload :retry-after) 7))
        (should (e-harness--retryable-error-p
                 (plist-get item :content) payload))))))

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

(ert-deftest e-openai-test-default-http-timeout-rearms-on-response-progress ()
  "Incoming HTTP response bytes extend the adapter's idle deadline."
  (let ((e-openai-request-timeout-seconds 0.08)
        buffer
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
       :on-complete #'ignore
       :on-error (lambda (err) (setq error err)))
      (e-openai-test--wait-until (lambda () nil) 0.05)
      (with-current-buffer buffer
        (insert "data: {\"type\":\"response.created\"}\n\n"))
      ;; This crosses the original absolute deadline but remains inside the
      ;; idle interval measured from the response-buffer insertion above.
      (e-openai-test--wait-until (lambda () nil) 0.05)
      (should-not error)
      (should (buffer-live-p buffer))
      ;; A genuinely idle stream still fails once the re-armed deadline passes.
      (should (e-openai-test--wait-until (lambda () error) 0.15))
      (should (eq (car error) 'e-openai-request-timeout))
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
          (e-backend-stream-batch backend
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

(ert-deftest e-openai-test-websocket-completion-retains-connection ()
  "Responses WebSocket completion retains the transport for reuse."
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
         on-message done-status error late-seen request (close-count 0))
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
        (setq request
              (e-backend-start backend
                               :messages '((:role user :content "hello"))
                               :options '(:model "gpt-test")
                               :on-item (lambda (item) (push item late-seen))
	                       :on-done (lambda (status)
	                                  (setq done-status status))
	                       :on-error (lambda (err) (setq error err))))
        (should (e-openai-test--wait-until (lambda () done-status) 0.2))
        (should-not error)
        (should (= close-count 0))
        (should (equal (nreverse late-seen)
                       '((:type provider-anchor-candidate
                          :provider-id openai
                          :metadata (:response-id "resp-ws-1"))
                         (:type done :reason stop))))
        (should (e-backend-cancel-request request))
        (should (= close-count 1))))))

(ert-deftest e-openai-test-websocket-reuses-compatible-session-connection ()
  "Compatible follow-ups reuse one connection and send incremental input."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth nil)))
         (open-count 0)
         (close-count 0)
         sends
         on-message
         (done-count 0)
         second-request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (cl-incf open-count)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let* ((payload
                         (json-parse-string text
                                            :object-type 'plist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object :json-false))
                        (response-id
                         (if (null sends) "resp-one" "resp-two")))
                   (push payload sends)
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.completed"
                               :response (:id ,response-id
                                          :status "completed")))))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start
         backend
         :messages '((:role user :content "one"))
         :options '(:model "gpt-test"
                    :session-id "session-one"
                    :provider-continuation t)
         :on-item #'ignore
         :on-done (lambda (_status) (cl-incf done-count))
         :on-error #'signal)
        (should (e-openai-test--wait-until (lambda () (= done-count 1)) 0.2))
        (setq second-request
              (e-backend-start
               backend
               :messages '((:role user :content "one"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"))
                 :provider-anchor-delta-messages
                 ((:role tool
                   :content (:tool-call-id "call-one" :content "result")))
                 :provider-anchor-source-message-count 1)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 2)) 0.2))
        (should (= open-count 1))
        (should (= close-count 0))
        (let* ((chronological (nreverse sends))
               (second (cadr chronological))
               (diagnostics
                (plist-get (e-backend-request-metadata second-request)
                           :diagnostics)))
          (should (equal (plist-get second :previous_response_id) "resp-one"))
          (should (equal (mapcar (lambda (item) (plist-get item :type))
                                 (plist-get second :input))
                         '("function_call_output")))
          (should (eq (plist-get diagnostics :websocket-reused) t))
          (should (eq (plist-get diagnostics :websocket-request-mode)
                      'incremental))
          (should (= (plist-get diagnostics :websocket-reuse-count) 1)))
        (e-backend-cancel-request second-request)
        (should (= close-count 1))))))

(ert-deftest e-openai-test-websocket-changed-instructions-continue-incrementally ()
  "Replacement instructions keep the current response anchor and socket."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth nil)))
         (open-count 0)
         sends
         on-message
         (done-count 0)
         second-request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (cl-incf open-count)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let ((payload
                        (json-parse-string text
                                           :object-type 'plist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :json-false)))
                   (push payload sends)
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.completed"
                               :response
                               (:id ,(if (= (length sends) 1)
                                         "resp-one"
                                       "resp-two")
                                :status "completed")))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start
         backend
         :messages '((:role system :content "instruction one")
                     (:role user :content "one"))
         :options '(:model "gpt-test" :session-id "session-one")
         :on-item #'ignore
         :on-done (lambda (_status) (cl-incf done-count))
         :on-error #'signal)
        (should (e-openai-test--wait-until (lambda () (= done-count 1)) 0.2))
        (setq second-request
              (e-backend-start
               backend
               :messages '((:role system :content "instruction two")
                           (:role user :content "two"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"))
                 :provider-anchor-delta-messages
                 ((:role user :content "two"))
                 :provider-anchor-source-message-count 2)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 2)) 0.2))
        (should (= open-count 1))
        (let* ((second (car sends))
               (diagnostics
                (plist-get (e-backend-request-metadata second-request)
                           :diagnostics)))
          (should (equal (plist-get second :previous_response_id) "resp-one"))
          (should (equal (mapcar (lambda (item) (plist-get item :type))
                                 (plist-get second :input))
                         '("message")))
          (should (equal (plist-get second :instructions)
                         "You are a helpful assistant.\n\ninstruction two"))
          (should (equal (plist-get diagnostics :websocket-request-mode)
                         'incremental))
          (should-not (plist-get diagnostics :websocket-fallback-reason)))
        (e-backend-cancel-request second-request)))))

(ert-deftest e-openai-test-websocket-properties-compare-json-object-contents ()
  "Fresh nested JSON objects do not invalidate equivalent tool definitions."
  (let ((first-parameters (make-hash-table :test 'equal))
        (second-parameters (make-hash-table :test 'equal)))
    (puthash "type" "object" first-parameters)
    (puthash "properties" (make-hash-table :test 'equal) first-parameters)
    (puthash "properties" (make-hash-table :test 'equal) second-parameters)
    (puthash "type" "object" second-parameters)
    (let ((first
           (list :model "gpt-test"
                 :tools (vector (list :name "inspect"
                                      :parameters first-parameters))))
          (second
           (list :model "gpt-test"
                 :tools (vector (list :name "inspect"
                                      :parameters second-parameters)))))
      (should (e-openai-codex--json-value-equal-p first second))
      (should-not
       (e-openai-codex--websocket-changed-property-names first second))
      (puthash "additionalProperties" :json-false second-parameters)
      (should-not (e-openai-codex--json-value-equal-p first second))
      (should
       (equal (e-openai-codex--websocket-changed-property-names first second)
              '(":tools"))))))

(ert-deftest e-openai-test-websocket-reconnects-with-full-request ()
  "A follow-up after server close reconnects without the stale response id."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth nil)))
         (open-count 0)
         sends
         on-message
         on-close
         current-websocket
         (done-count 0)
         second-request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (cl-incf open-count)
                 (setq on-message (plist-get args :on-message))
                 (setq on-close (plist-get args :on-close))
                 (setq current-websocket
                       (intern (format "fake-websocket-%d" open-count)))))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let ((payload
                        (json-parse-string text
                                           :object-type 'plist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :json-false)))
                   (push payload sends)
                   (funcall on-message
                            websocket
                            (json-encode
                             `(:type "response.completed"
                               :response
                               (:id ,(if (= (length sends) 1)
                                         "resp-one"
                                       "resp-two")
                                :status "completed")))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start
         backend
         :messages '((:role user :content "one"))
         :options '(:model "gpt-test" :session-id "session-one")
         :on-item #'ignore
         :on-done (lambda (_status) (cl-incf done-count))
         :on-error #'signal)
        (should (e-openai-test--wait-until (lambda () (= done-count 1)) 0.2))
        (funcall on-close current-websocket)
        (setq second-request
              (e-backend-start
               backend
               :messages '((:role user :content "one")
                           (:role user :content "two"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"))
                 :provider-anchor-delta-messages
                 ((:role user :content "two"))
                 :provider-anchor-source-message-count 2)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 2)) 0.2))
        (should (= open-count 2))
        (let* ((second (car sends))
               (diagnostics
                (plist-get (e-backend-request-metadata second-request)
                           :diagnostics)))
          (should-not (plist-member second :previous_response_id))
          (should (= (length (plist-get second :input)) 2))
          (should (eq (plist-get diagnostics :websocket-request-mode) 'full))
          (should (eq (plist-get diagnostics :websocket-fallback-reason)
                      'new-connection)))
        (e-backend-cancel-request second-request)))))

(ert-deftest e-openai-test-websocket-unresolved-anchor-retries-full-request ()
  "An unavailable connection-local response id retries once with full input."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((openai-websocket
             :name "OpenAI WebSocket"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth nil)))
         sends
         on-message
         (done-count 0)
         second-request)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let ((payload
                        (json-parse-string text
                                           :object-type 'plist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :json-false)))
                   (push payload sends)
                   (pcase (length sends)
                     (1
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-one"
                                             :status "completed")))))
                     (2
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.failed"
                                  :response
                                  (:error
                                   (:code "response_not_found"
                                    :param "previous_response_id"
                                    :message "previous_response_id not cached"))))))
                     (3
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-two"
                                             :status "completed")))))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
        (e-backend-start
         backend
         :messages '((:role user :content "one"))
         :options '(:model "gpt-test" :session-id "session-one")
         :on-item #'ignore
         :on-done (lambda (_status) (cl-incf done-count))
         :on-error #'signal)
        (should (e-openai-test--wait-until (lambda () (= done-count 1)) 0.2))
        (setq second-request
              (e-backend-start
               backend
               :messages '((:role user :content "one")
                           (:role user :content "two"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"))
                 :provider-anchor-delta-messages
                 ((:role user :content "two"))
                 :provider-anchor-source-message-count 2)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 2)) 0.2))
        (let* ((chronological (nreverse sends))
               (incremental (cadr chronological))
               (fallback (caddr chronological))
               (diagnostics
                (plist-get (e-backend-request-metadata second-request)
                           :diagnostics)))
          (should (equal (plist-get incremental :previous_response_id)
                         "resp-one"))
          (should-not (plist-member fallback :previous_response_id))
          (should (= (length (plist-get fallback :input)) 2))
          (should (eq (plist-get diagnostics :websocket-request-mode)
                      'full-retry))
          (should (eq (plist-get diagnostics :websocket-fallback-reason)
                      'previous-response-unresolved)))
        (e-backend-cancel-request second-request)))))

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

(ert-deftest e-openai-test-websocket-error-diagnostics-are-bounded ()
  "Responses WebSocket library errors do not raw-print unbounded details."
  (let* ((e-openai-diagnostic-print-length 4)
         (e-openai-diagnostic-print-level 3)
         (e-openai-diagnostic-string-max-bytes 48)
         (e-openai-diagnostic-result-max-bytes 220)
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
         on-error error done-status (close-count 0))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket _text)
                 (funcall on-error
                          websocket
                          :payload (make-list 40 (make-string 100 ?w)))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (e-backend-start (e-openai-backend-create :provider 'openai-websocket)
                       :messages '((:role user :content "hello"))
                       :options '(:model "gpt-test")
                       :on-item #'ignore
                       :on-done (lambda (status)
                                  (setq done-status status))
                       :on-error (lambda (err) (setq error err)))
      (should (e-openai-test--wait-until (lambda () error) 0.2))
      (should-not done-status)
      (should (eq (car error) 'error))
      (should (string-match-p "Responses WebSocket error" (cadr error)))
      (should (string-match-p "OpenAI diagnostic" (cadr error)))
      (should-not (string-match-p (make-string 80 ?w) (cadr error)))
      (should (< (string-bytes (cadr error)) 420))
      (should (= close-count 1)))))

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

(ert-deftest e-openai-test-websocket-store-false-without-anchor-full-replays ()
  "Direct unstored WebSocket calls replay when no prior anchor is supplied."
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
                                                    :null-object :json-null
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
                                     (:role assistant
                                      :content "answer one"
                                      :metadata
                                      (:provider-replay-items
                                       ((:type provider-replay-item
                                         :provider-id openai
                                         :item
                                         (:type "reasoning"
                                          :id "rs-websocket"
                                          :encrypted_content "ciphertext"
                                          :summary nil)))))
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
                           (:type "reasoning"
                            :id "rs-websocket"
                            :encrypted_content "ciphertext"
                            :summary nil)
                           (:type "message"
                            :role "assistant"
                            :content ((:type "output_text" :text "answer one")))
                           (:type "message"
                            :role "user"
                            :content ((:type "input_text" :text "two"))))))
          ;; Empty JSON arrays decode as nil with `:array-type list', while
          ;; JSON null decodes as `:json-null' above.
          (let ((reasoning (nth 1 (plist-get second-response :input))))
            (should (plist-member reasoning :summary))
            (should-not (eq (plist-get reasoning :summary) :json-null))))
        (should (seq-some (lambda (item)
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
          (e-harness-test-prompt-batch harness "session-1" "question")
          (should (equal (mapcar (lambda (message) (plist-get message :role))
                                 (e-harness-messages harness "session-1"))
                         '(user assistant)))
          (should (equal (plist-get (cadr (e-harness-messages harness "session-1"))
                                    :content)
                         "real-ish answer")))
      (delete-file auth-file))))

(provide 'e-openai-test)

;;; e-openai-test.el ends here

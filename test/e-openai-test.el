;;; e-openai-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI/Codex auth, request mapping, and stream parsing.

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

(ert-deftest e-openai-test-auth-file-uses-codex-home ()
  "Codex auth file resolution honors CODEX_HOME."
  (let ((process-environment
         (cons "CODEX_HOME=/tmp/e-codex-home" process-environment)))
    (should (equal (e-openai-codex-auth-file)
                   "/tmp/e-codex-home/auth.json"))))

(ert-deftest e-openai-test-http-timeout-default-is-disabled ()
  "Buffered HTTP reasoning responses have no unsafe implicit deadline."
  (should-not (default-value 'e-openai-request-timeout-seconds)))

(ert-deftest e-openai-test-codex-gpt6-models-use-supported-session-options ()
  "Codex defaults to Sol, accepts Luna, and omits unsupported SQL anchors."
  (let ((sol (e-openai-codex-create-harness))
        (luna (e-openai-codex-create-harness :model "gpt-6-luna")))
    (should (equal (plist-get (e-harness-default-options sol) :model)
                   "gpt-6-sol"))
    (should (equal (plist-get (e-harness-default-options luna) :model)
                   "gpt-6-luna"))
    (should-not (plist-get (e-harness-default-options sol)
                           :provider-continuation))
    (should-not (plist-get (e-harness-default-options luna)
                           :provider-continuation))))

(ert-deftest e-openai-test-read-auth-token-and-account-id ()
  "Auth parsing extracts the access token and account id."
  (let* ((token (e-openai-test--jwt))
         (auth (list :tokens (list :access_token token
                                   :refresh_token "refresh"))))
    (should (equal (e-openai-codex-auth-access-token auth) token))
    (should (equal (e-openai-codex-auth-account-id auth) "acct-test"))))

(ert-deftest e-openai-test-codex-harness-runs-minimal-prompt-flow ()
  "The Codex harness helper can run prompt to persisted assistant message."
  (let* ((token (e-openai-test--jwt))
         (auth-file (make-temp-file "e-auth" nil ".json"
                                    (e-json-serialize
                                     (list :tokens
                                           (list :access_token token
                                                 :refresh_token "refresh")))))
         (captured nil)
         (harness
          (e-openai-codex-create-harness
           :auth-file auth-file
           :base-url (concat e-openai-codex-default-base-url
                             "/codex/responses")
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (setq captured (list :url url :headers headers
                                   :body (e-json-parse-string body)))
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
                         "real-ish answer"))
          (let* ((headers (plist-get captured :headers))
                 (body (plist-get captured :body))
                 (header-id (cdr (assoc "session-id" headers))))
            (should (equal (plist-get captured :url)
                           "wss://chatgpt.com/backend-api/codex/responses"))
            (should (equal header-id "session-1"))
            (should-not (assoc "session_id" headers))
            (should (equal (plist-get body :prompt_cache_key) header-id))
            (should-not (plist-member body :prompt_cache_options))
            (should-not (plist-member body :prompt_cache_retention))))
      (delete-file auth-file))))

(ert-deftest e-openai-test-codex-backend-constructor-unifies-base-and-full-url ()
  "The public Codex backend gives equivalent endpoint spellings one affinity."
  (let* ((auth-file (make-temp-file "e-codex-auth" nil ".json"
                                    (e-json-serialize
                                     (list :tokens
                                           (list :access_token (e-openai-test--jwt)
                                                 :refresh_token "refresh")))))
         (e-openai-model-providers
          `((codex
             :name "ChatGPT Codex HTTP"
             :base-url ,(concat e-openai-codex-default-base-url "/codex")
             :wire-api responses :responses-transport http
             :requires-openai-auth t)))
         (native-context (symbol-function 'e-openai--request-context))
         contexts requests
         (endpoints (list e-openai-codex-default-base-url
                          (concat e-openai-codex-default-base-url
                                  "/codex/responses"))))
    (unwind-protect
        (cl-letf (((symbol-function 'e-openai--request-context)
                   (lambda (&rest args)
                     (let ((context (apply native-context args)))
                       (push context contexts)
                       context))))
          (dolist (endpoint endpoints)
            (e-backend-stream-batch
             (e-openai-codex-backend-create
              :auth-file auth-file :base-url endpoint
              :request-function
              (cl-function
               (lambda (&key url headers body)
                 (push (list :url url :headers headers
                             :body (e-json-parse-string body)) requests)
                 (concat "data: {\"type\":\"response.output_text.done\",\"text\":\"ok\"}\n\n"
                         "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n"))))
             :messages '((:role user :content "same prompt"))
             :options '(:model "gpt-6-sol" :session-id "same-session")
             :on-item #'ignore))
          (setq requests (nreverse requests) contexts (nreverse contexts))
          (should (= (length requests) 2))
          (should (= (length contexts) 2))
          (dolist (request requests)
            (let* ((headers (plist-get request :headers))
                   (body (plist-get request :body))
                   (header-id (cdr (assoc "session-id" headers))))
              (should (equal (plist-get request :url)
                             "https://chatgpt.com/backend-api/codex/responses"))
              (should (equal header-id "same-session"))
              (should-not (assoc "session_id" headers))
              (should (equal (plist-get body :prompt_cache_key) header-id))
              (should-not (plist-member body :prompt_cache_options))
              (should-not (plist-member body :prompt_cache_retention))))
          (should (equal (plist-get (plist-get (car requests) :body)
                                    :prompt_cache_key)
                         (plist-get (plist-get (cadr requests) :body)
                                    :prompt_cache_key)))
          (dolist (context contexts)
            (let ((diagnostics
                   (plist-get (plist-get context :metadata) :diagnostics)))
              (should (eq (plist-get diagnostics
                                    :prompt-cache-capability-evidence)
                          'first-party-codex-session-affinity))
              (should (eq (plist-get diagnostics :codex-session-affinity-selected)
                          t)))))
      (delete-file auth-file))))

(ert-deftest e-openai-test-public-openai-alias-constructor-unifies-endpoints ()
  "A token-auth API alias uses one boundary policy for base and full endpoints."
  (let* ((process-environment
          (cons "OPENAI_API_KEY=test-api-token" process-environment))
         (e-openai-model-providers
          `((enterprise-label
             :name "Enterprise first-party OpenAI label"
             :base-url ,e-openai-api-default-base-url
             :wire-api responses :responses-transport http
             :responses-context-layout developer-input
             :requires-openai-auth nil :env-key "OPENAI_API_KEY")))
         (native-context (symbol-function 'e-openai--request-context))
         contexts requests
         (segments
          '((:kind static-prefix
             :messages ((:role system :content "Stable alias instructions")))
            (:kind history :messages ((:role user :content "same question")))))
         (endpoints (list e-openai-api-default-base-url
                          (concat e-openai-api-default-base-url "/responses"))))
    (cl-letf (((symbol-function 'e-openai--request-context)
               (lambda (&rest args)
                 (let ((context (apply native-context args)))
                   (push context contexts)
                   context))))
      (dolist (endpoint endpoints)
        (e-backend-stream-batch
         (e-openai-backend-create
          :provider 'enterprise-label :base-url endpoint
          :request-function
          (cl-function
           (lambda (&key url headers body)
             (push (list :url url :headers headers
                         :body (e-json-parse-string body)) requests)
             (concat "data: {\"type\":\"response.output_text.done\",\"text\":\"ok\"}\n\n"
                     "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n"))))
         :messages '((:role system :content "Stable alias instructions")
                     (:role user :content "same question"))
         :options (list :model "gpt-6-sol" :session-id "alias-session"
                        :segments segments)
         :on-item #'ignore))
      (setq requests (nreverse requests) contexts (nreverse contexts))
      (should (= (length requests) 2))
      (should (= (length contexts) 2))
      (dolist (request requests)
        (let* ((body (plist-get request :body))
               (input (plist-get body :input))
               (content (aref (plist-get (aref input 0) :content) 0)))
          (should (equal (plist-get request :url)
                         "https://api.openai.com/v1/responses"))
          (should (assoc "Authorization" (plist-get request :headers)))
          (should (equal (plist-get body :prompt_cache_options)
                         '(:mode "implicit")))
          (should (equal (plist-get content :prompt_cache_breakpoint)
                         '(:mode "explicit")))
          (should-not (plist-member body :prompt_cache_key))))
      (should (equal (plist-get (plist-get (car requests) :body) :input)
                     (plist-get (plist-get (cadr requests) :body) :input)))
      (dolist (context contexts)
        (let ((diagnostics
               (plist-get (plist-get context :metadata) :diagnostics)))
          (should (eq (plist-get diagnostics :prompt-cache-capability-evidence)
                      'first-party-responses-documented))
          (should (eq (plist-get diagnostics :prompt-cache-boundary-policy)
                      'implicit-latest-eligible-plus-stable-guard))
          (should (= (plist-get diagnostics :prompt-cache-stable-guard-count)
                     1)))))))

(provide 'e-openai-test)

;;; e-openai-test.el ends here

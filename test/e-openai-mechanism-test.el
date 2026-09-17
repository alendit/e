;;; e-openai-mechanism-test.el --- OpenAI owner mechanism tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Direct profile, decoder, transport, diagnostics, and compaction mechanism
;; tests. Composed public behavior remains in `e-openai-test.el'.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-json)
(require 'e-backend)
(require 'e-dev-profile)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el"
             (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(require 'e-loop)
(require 'e-openai)
(require 'url-http)
(load (expand-file-name "e-openai-test-support.el"
             (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

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
            (e-openai-profile--migrate-websocket-idle-timeout-default)
            (should (equal e-openai-websocket-idle-timeout-seconds 60)))
          (let ((e-openai-websocket-idle-timeout-seconds 180))
            (e-openai-profile--migrate-websocket-idle-timeout-default)
            (should (equal e-openai-websocket-idle-timeout-seconds 60)))
          (put symbol 'saved-value '(nil))
          (let ((e-openai-websocket-idle-timeout-seconds nil))
            (e-openai-profile--migrate-websocket-idle-timeout-default)
            (should-not e-openai-websocket-idle-timeout-seconds)))
      (put symbol 'saved-value saved)
      (put symbol 'customized-value customized)
      (put symbol 'theme-value theme))))


(ert-deftest e-openai-test-http-timeout-default-migrates-on-reload ()
  "The old HTTP timeout migrates unless the user explicitly customized it."
  (let* ((symbol 'e-openai-request-timeout-seconds)
         (saved (get symbol 'saved-value))
         (customized (get symbol 'customized-value))
         (theme (get symbol 'theme-value)))
    (unwind-protect
        (progn
          (put symbol 'saved-value nil)
          (put symbol 'customized-value nil)
          (put symbol 'theme-value nil)
          (let ((e-openai-request-timeout-seconds 180))
            (e-openai-profile--migrate-http-timeout-default)
            (should-not e-openai-request-timeout-seconds))
          (put symbol 'saved-value '(180))
          (let ((e-openai-request-timeout-seconds 180))
            (e-openai-profile--migrate-http-timeout-default)
            (should (= e-openai-request-timeout-seconds 180))))
      (put symbol 'saved-value saved)
      (put symbol 'customized-value customized)
      (put symbol 'theme-value theme))))


(ert-deftest e-openai-test-request-context-reasoning-summary-profile-override ()
  "Request summary overrides a Responses profile and adapter default."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-openai-model-providers
          '((eng-responses
             :name "Engineering Responses"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :reasoning-summary "detailed"
             :requires-openai-auth nil)))
         (profile-context
          (e-openai--request-context
           :provider 'eng-responses
           :messages '((:role user :content "hello"))
           :options '(:model "gpt-test")))
         (request-context
          (e-openai--request-context
           :provider 'eng-responses
           :messages '((:role user :content "hello"))
           :options '(:model "gpt-test"
                      :reasoning-summary "auto")))
         (profile-reasoning
          (plist-get (plist-get profile-context :body-data) :reasoning))
         (request-reasoning
          (plist-get (plist-get request-context :body-data) :reasoning)))
    (should (equal (plist-get profile-reasoning :summary) "detailed"))
    (should (equal (plist-get request-reasoning :summary) "auto"))
    (should (equal (plist-get (plist-get profile-context :metadata)
                              :reasoning-identity)
                   '(:effort "high" :summary "detailed")))
    (should (equal (plist-get (plist-get request-context :metadata)
                              :reasoning-identity)
                   '(:effort "high" :summary "auto")))
    (should (equal (plist-get (plist-get (plist-get profile-context :metadata)
                                        :diagnostics)
                              :reasoning-summary)
                   "detailed"))
    (should (equal (plist-get (plist-get (plist-get request-context :metadata)
                                        :diagnostics)
                              :reasoning-summary)
                   "auto"))))


(ert-deftest e-openai-test-context-curation-revision-fences-material-layout ()
  "Curation revision fences continuation identity without source leakage."
  (let* ((base-options
          '(:model "gpt-5.6-sol"
            :responses-context-layout developer-input
            :observation-delivery inherited
            :reserved-effect-carrier context-curate-wire
            :provider-continuation t
            :responses-transport websocket
            :response-store :json-false))
         (first-options
          (append
           base-options
           '(:segments
             ((:kind static-prefix
               :messages ((:role system :content "stable policy")))
              (:kind current-state
               :messages ((:role system :content "SOURCE-ONE")))))))
         (second-options
          (append
           base-options
           '(:segments
             ((:kind static-prefix
               :messages ((:role system :content "stable policy")))
              (:kind current-state
               :messages ((:role system :content "SOURCE-TWO")))))))
         (first (e-openai-responses-prompt-layout-revision first-options))
         (second (e-openai-responses-prompt-layout-revision second-options)))
    (should (equal first second))
    (should (equal
             (plist-get first :context-curation-revision-identity)
             (e-context-lifetime-curation-revision-identity)))
    (should-not (string-match-p
                 "SOURCE-ONE\|SOURCE-TWO\|frame\|generation\|observation\|fingerprint"
                 (prin1-to-string first)))
    (should-not (plist-member
                 (e-openai-codex-request-body
                  :messages '((:role system :content "stable policy")
                              (:role system :content "SOURCE-ONE"))
                  :options first-options)
                 :prompt_cache_options))
    (let ((e-context-budget-estimate-bytes-per-token 2.0))
      (should-not
       (equal first
              (e-openai-responses-prompt-layout-revision first-options))))
    (let ((e-context-lifetime-curation-presentation-revision
           "context-curation-presentation-test-v2"))
      (should-not
       (equal first
              (e-openai-responses-prompt-layout-revision first-options))))
    (let ((e-context-lifetime-curation-schema-revision
           "context-curate-test-v3"))
      (should-not
       (equal first
              (e-openai-responses-prompt-layout-revision first-options))))
    (let* ((anchor
            (list :provider-id 'openai
                  :metadata (list :response-id "response-curation"
                                  :prompt-layout-revision first
                                  :reasoning-identity
                                  '(:effort "high" :summary "auto"))))
           (continuation-options
            (plist-put (copy-sequence first-options)
                       :provider-anchor anchor)))
      (should (equal
               (e-openai-responses--continuation-response-id
                continuation-options)
               "response-curation"))
      (let ((e-context-budget-estimate-bytes-per-token 2.0))
        (should-not
         (e-openai-responses--continuation-response-id
          continuation-options))))))


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
                       :metadata (:response-id "resp-1"
                                  :reasoning-identity
                                  (:effort "high" :summary "auto")))
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
                     :reasoning-summary "auto"
                     :response-store t
                     :prompt-cache-key-present t
                     :prompt-cache-retention-present t
                     :provider-continuation used
                     :previous-response-id-present t
                     :provider-anchor-present t
                     :provider-compaction-selected nil
                     :provider-compaction-source-entry-id nil
                     :input-message-count 1
                     :tool-count 1
                     :responses-transport http
                     :observation-delivery inherited
                     :replaceable-current-state-present nil
                     :current-state-fingerprint nil
                     :context-rendering-strategy nil
                     :provider-anchor-safety nil)))))


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
         (body-data (e-json-parse-string (plist-get context :body))))
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


(ert-deftest e-openai-test-codex-profile-uses-required-unstored-websocket-mode ()
  "Codex WebSocket requests explicitly use the backend-required store=false."
  (let* ((auth-file (make-temp-file "e-openai-auth" nil ".json"))
         (auth (e-json-serialize
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
                 (body (e-json-parse-string (plist-get context :body))))
            (should (eq (plist-get context :responses-transport) 'websocket))
            (should (eq (plist-get body :store) :json-false))
            (should-not (plist-member body :stream))
            (should-not (plist-member body :prompt_cache_options))
            (should (equal (plist-get body :instructions)
                           "You are a helpful assistant."))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   (append (plist-get body :input) nil))
                           '("developer" "user")))
            (should-not
             (plist-member
              (aref (plist-get (aref (plist-get body :input) 0) :content) 0)
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
    (e-openai-profile--normalize-model-providers
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
       :include-encrypted-reasoning t
       :observation-delivery inherited)
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
       :observation-delivery inherited
       :include-encrypted-reasoning t
       :continuation t
       :requires-openai-auth nil
       :env-key "OPENAI_API_KEY"
       :default-model "gpt-5.6")))))


(ert-deftest e-openai-test-builtin-codex-uses-general-idle-policy ()
  "The built-in Codex profile uses the general WebSocket idle policy."
  (let* ((providers
          `((codex
             :name "ChatGPT Codex"
             :base-url ,(concat e-openai-codex-default-base-url "/codex")
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth t)
            (codex-lookalike
             :name "ChatGPT Codex"
             :base-url "https://gateway.example.test/codex"
             :wire-api responses
             :responses-transport websocket
             :continuation t
             :requires-openai-auth t)))
         (normalized (e-openai-profile--normalize-model-providers providers))
         (canonical (cdr (assq 'codex normalized)))
         (lookalike (cdr (assq 'codex-lookalike normalized))))
    (should-not (plist-member canonical :websocket-idle-close-seconds))
    (should-not (plist-member lookalike :websocket-idle-close-seconds))))


(ert-deftest e-openai-test-request-context-resolves-websocket-idle-policy ()
  "Request context resolves profile retention once with global fallbacks."
  (let ((process-environment
         (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token"
               process-environment)))
    (dolist (case '((271 nil 271)
                    (nil nil nil)
                    (271 0 0)
                    (271 17 17)))
      (pcase-let ((`(,global ,profile-value ,expected) case))
        (let* ((e-openai-websocket-connection-idle-seconds global)
               (e-openai-model-providers
                `((websocket-policy
                   :name "WebSocket Policy"
                   :base-url "https://gateway.example.test/v1"
                   :env-key "OPENAI_GATEWAY_API_KEY"
                   :wire-api responses
                   :responses-transport websocket
                   ,@(when profile-value
                       (list :websocket-idle-close-seconds profile-value))
                   :requires-openai-auth nil)))
               (context
                (e-openai--request-context
                 :provider 'websocket-policy
                 :messages '((:role user :content "hello"))
                 :options '(:model "gpt-test"))))
          (should (plist-member context :websocket-idle-close-seconds))
          (should (equal (plist-get context :websocket-idle-close-seconds)
                         expected)))))))


(ert-deftest e-openai-test-codex-and-api-profiles-render-equivalent-common-body ()
  "Canonical providers share one body modulo explicit-cache capability."
  (let* ((process-environment
          (cons "OPENAI_API_KEY=test-api-token" process-environment))
         (auth-file (make-temp-file "e-openai-auth" nil ".json"))
         (auth (e-json-serialize
                (list :tokens
                      (list :access_token (e-openai-test--jwt)
                            :refresh_token "refresh"))))
         (messages '((:role system :content "stable guidance")
                     (:role user :content "hello")
                     (:role system :content "dynamic state")))
         (options '(:model "gpt-5.6"
                    :prompt-cache-key "shared-key"
                    :segments
                    ((:kind stable-context
                      :messages ((:role system :content "stable guidance")))
                     (:kind history
                      :messages ((:role user :content "hello")))
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
                  (e-openai-http--response-create
                   :status 503
                   :retry-after 0.01
                   :body "{\"error\":{\"message\":\"Generation failed\"}}")
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
    (should
     (equal (plist-get (car (last (e-harness-messages harness "session-1")))
                       :content)
            "recovered"))))


(ert-deftest e-openai-test-debug-diagnostics-record-ignored-events ()
  "Debug diagnostics record raw response and ignored provider event summaries."
  (let ((e-openai-codex-debug t)
        (e-openai-diagnostics--last-diagnostics nil)
        (stream "data: {\"type\":\"response.unknown\",\"item\":{\"type\":\"mystery\"}}\n\n\
data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}\n\n"))
    (should (equal (e-openai-codex-parse-stream stream)
                   '((:type done :reason stop))))
    (should (equal (plist-get e-openai-diagnostics--last-diagnostics :raw-response)
                   stream))
    (should
     (equal (plist-get e-openai-diagnostics--last-diagnostics :events)
            '((:event-type "response.unknown"
               :item-type "mystery"
               :parsed-type nil)
              (:event-type "response.completed"
               :item-type nil
               :parsed-type done))))))


(ert-deftest e-openai-test-debug-raw-response-retention-is-bounded ()
  "Debug raw payload retention keeps only the configured trailing byte budget."
  (let ((e-openai-codex-debug t)
        (e-openai-codex-raw-responses-max-bytes 128)
        (e-openai-diagnostics--last-diagnostics nil)
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
            (let ((raw (plist-get e-openai-diagnostics--last-diagnostics
                                  :raw-response)))
              (should (<= (string-bytes raw) 128))
              (should (string-suffix-p "END" raw))))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))))))


(ert-deftest e-openai-test-response-error-message-bounds-large-message ()
  "Responses failure message text is capped before becoming diagnostics."
  (let* ((e-openai-diagnostic-string-max-bytes 32)
         (text (make-string 200 ?x))
         (message
          (e-openai-decoder--response-error-message
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
         (message (e-openai-decoder--response-error-message event)))
    (should (< (string-bytes message) 360))
    (should (string-match-p "OpenAI diagnostic" message))
    (should-not (string-match-p (make-string 80 ?z) message))))



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
    (should (equal (e-json-parse-string (plist-get context :body))
                   '(:model "claude-default"
                     :stream t
                     :messages [(:role "system"
                                  :content "You are a helpful assistant.")
                                (:role "user" :content "hello")])))))


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
      (e-openai-http-request-start
       :url "https://example.test/codex/responses"
       :headers '(("Authorization" . "Bearer test"))
       :body "{}"
       :on-complete (lambda (value) (setq response value))
       :on-error (lambda (err) (setq error err)))
      (should-not error)
      (should (e-openai-http-response-p response))
      (let* ((item (car (e-openai--complete-response-items
                         response '(:wire-api responses))))
             (payload (plist-get item :payload)))
        (should (equal (plist-get item :content) "Generation failed"))
        (should (= (plist-get payload :status) 503))
        (let ((details (e-openai-diagnostics-normalize-error-details
                        (plist-get item :content) payload nil)))
          (should (eq (plist-get details :retryable) t))
          (should (eq (plist-get details :retry-reason)
                      'provider-unavailable)))))))


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
      (e-openai-http-request-start
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
        (let ((details (e-openai-diagnostics-normalize-error-details
                        (plist-get item :content) payload nil)))
          (should (eq (plist-get details :retryable) t))
          (should (= (plist-get details :retry-after-seconds) 7))
          (should (eq (plist-get details :retry-reason) 'rate-limit)))))))


(ert-deftest e-openai-test-normalizes-provider-retry-hints ()
  "The OpenAI adapter owns transient classification and reset parsing."
  (let* ((now (float-time
               (encode-time (parse-time-string
                             "2026-07-03 08:20:00 +0000"))))
         (absolute
          (e-openai-diagnostics--retry-after-from-text
           (concat "429 rate limit. Limit resets at: "
                   "2026-07-03 08:23:02 UTC")
           now)))
    (should (= absolute 182.0))
    (should (= (e-openai-diagnostics--retry-after-from-text
                "please retry after 2 minutes" now)
               120.0)))
  (dolist (case
           '(("Rate limit exceeded" nil rate-limit)
             ("server_error: Generation failed" nil provider-unavailable)
             ("connection reset by peer" nil transport)
             ("stream ended prematurely" nil premature-stream)
             ("request failed" (:status 503) provider-unavailable)))
    (pcase-let ((`(,message ,payload ,reason) case))
      (let ((details (e-openai-diagnostics-normalize-error-details message payload nil)))
        (should (eq (plist-get details :retryable) t))
        (should (eq (plist-get details :retry-reason) reason)))))
  (should-not
   (eq (plist-get
        (e-openai-diagnostics-normalize-error-details "500: internal error" nil nil)
        :retryable)
       t))
  (should-not
   (eq (plist-get
        (e-openai-diagnostics-normalize-error-details
         "invalid request" '(:status 400) nil)
        :retryable)
       t)))


(ert-deftest e-openai-test-websocket-retains-only-latest-response ()
  "Only the latest completed response can authorize immediate continuation."
  (let* ((session (e-openai-websocket-session-create))
         (url "wss://gateway.example.test/v1/responses")
         (headers nil)
         (response-index 0)
         sends
         on-message)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let ((payload
                        (e-json-parse-string text)))
                   (setq sends (append sends (list payload)))
                   (funcall
                    on-message
                    websocket
                    (e-json-serialize
                     `(:type "response.completed"
                       :response (:id ,(format "resp-%d"
                                               (cl-incf response-index))
                                  :status "completed")))))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args) t)))
      (cl-labels
          ((start (body full)
             (e-openai-websocket-request-start
              :session session
              :url url
              :headers headers
              :body-data body
              :full-body-data full
              :request-metadata '(:diagnostics nil)
              :on-item #'ignore
              :on-complete #'ignore
              :on-error #'signal)))
        (start '(:model "gpt-test" :input nil)
               '(:model "gpt-test" :input nil))
        (start '(:model "gpt-test" :input nil
                :previous_response_id "resp-1")
               '(:model "gpt-test" :input nil))
        ;; The first response is now older and must not be reused.
        (start '(:model "gpt-test" :input nil
                :previous_response_id "resp-1")
               '(:model "gpt-test" :input nil))
        (should-not (plist-member (nth 0 sends) :previous_response_id))
        (should (equal (plist-get (nth 1 sends) :previous_response_id)
                       "resp-1"))
        (should-not (plist-member (nth 2 sends) :previous_response_id))
        (should (equal
                 (e-openai-websocket--session-latest-response-id session)
                 "resp-3"))
        (should (equal
                 (e-openai-websocket--session-latest-response-properties
                  session)
                 '(:model "gpt-test")))))))


(ert-deftest e-openai-test-websocket-profile-idle-close-nil-does-not-schedule ()
  "A nil resolved idle policy retains the existing no-timer fallback."
  (let ((session (e-openai-websocket-session-create))
        scheduled)
    (setf (e-openai-websocket--session-websocket session) 'fake-websocket
          (e-openai-websocket--session-connection-id session) "e-ws-test")
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 (setq scheduled t)
                 'fake-timer)))
      (e-openai-websocket--schedule-idle-close session nil))
    (should-not scheduled)
    (should-not
     (e-openai-websocket--session-idle-timer session))))


(ert-deftest e-openai-test-websocket-profile-idle-close-explicit-zero-schedules-value ()
  "An explicit zero-second profile policy reaches the scheduler unchanged."
  (let ((session (e-openai-websocket-session-create))
        scheduled-seconds)
    (setf (e-openai-websocket--session-websocket session) 'fake-websocket
          (e-openai-websocket--session-connection-id session) "e-ws-test")
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seconds _repeat _callback)
                 (setq scheduled-seconds seconds)
                 'fake-timer))
              ((symbol-function 'timerp)
               (lambda (timer) (eq timer 'fake-timer)))
              ((symbol-function 'cancel-timer) #'ignore))
      (e-openai-websocket--schedule-idle-close session 0))
    (should (= scheduled-seconds 0))
    (should (eq (e-openai-websocket--session-idle-timer session)
                'fake-timer))))


(ert-deftest e-openai-test-websocket-terminal-and-transport-cleanup ()
  "Failed, partial, cancelled, and transport-failed requests clear response state."
  (let* ((url "wss://gateway.example.test/v1/responses")
         (headers nil)
         (mode nil)
         on-message
         on-error
         (close-count 0)
         partial-item-seen
         transport-error)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket _text)
                 (pcase mode
                   ('partial
                    (funcall on-message
                             websocket
                             (e-json-serialize
                              '(:type "response.output_text.delta"
                                :delta "partial"))))
                   ('failed
                    (funcall on-message
                             websocket
                             (e-json-serialize
                              '(:type "response.failed"
                                :response
                                (:id "failed-response"
                                 :status "failed"
                                 :error (:code "server_error"
                                          :message "failed"))))))
                   ('transport
                    (funcall on-error websocket :payload "transport failed")))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)
                 t)))
      (cl-labels
          ((empty-response-state-p (session)
             (and (null
                   (e-openai-websocket--session-latest-response-id
                    session))
                  (null
                   (e-openai-websocket--session-latest-response-properties
                    session))))
           (prepare-session ()
             (let ((session (e-openai-websocket-session-create)))
               (e-openai-websocket--session-open session url headers)
               (e-openai-websocket--session-record-response
                session "prior-response" '(:model "gpt-test"))
               session))
           (start (session on-item on-error)
             (e-openai-websocket-request-start
              :session session
              :url url
              :headers headers
              :body-data '(:model "gpt-test" :input nil)
              :full-body-data '(:model "gpt-test" :input nil)
              :request-metadata '(:diagnostics nil)
              :on-item on-item
              :on-complete #'ignore
              :on-error on-error)))
        ;; A partial event never admits an unconfirmed response.  Cancelling
        ;; the active request then clears the prior immediate state.
        (let* ((session (prepare-session))
               (request
                (progn
                  (setq mode 'partial)
                  (start session
                         (lambda (item) (setq partial-item-seen item))
                         #'ignore))))
          (should partial-item-seen)
          (should (equal
                   (e-openai-websocket--session-latest-response-id
                    session)
                   "prior-response"))
          (should (e-backend-cancel-request request))
          (should (empty-response-state-p session)))
        ;; A failed terminal event closes the socket and clears the response
        ;; state; the failed response ID is never admitted.
        (let* ((session (prepare-session))
               (request (progn
                          (setq mode 'failed)
                          (start session #'ignore #'ignore))))
          (should request)
          (should-not
           (e-openai-websocket--session-websocket session))
          (should (empty-response-state-p session)))
        ;; Transport failure follows the same close path before surfacing the
        ;; request error.
        (let* ((session (prepare-session))
               (request
                (progn
                  (setq mode 'transport)
                  (start session #'ignore
                         (lambda (error) (setq transport-error error))))))
          (should request)
          (should transport-error)
          (should-not
           (e-openai-websocket--session-websocket session))
          (should (empty-response-state-p session)))
        (should (= close-count 3))))))


(ert-deftest e-openai-test-websocket-incomplete-settles-without-anchor ()
  "An incomplete response settles once without admitting its response id."
  (let* ((e-openai-websocket-idle-timeout-seconds nil)
         (url "wss://gateway.example.test/v1/responses")
         (headers nil)
         on-message
         (send-count 0)
         (first-done-items 0)
         (second-done-items 0)
         (first-complete-count 0)
         (second-complete-count 0)
         first-error
         second-error
         sends
         scheduled-seconds
         (close-count 0)
         (session (e-openai-websocket-session-create)))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (cl-incf send-count)
                 (setq sends
                       (append
                        sends
                        (list
                         (e-json-parse-string text))))
                 (if (= send-count 1)
                     (progn
                       ;; A later completed frame must be ignored after the
                       ;; incomplete terminal event has settled the request.
                       (funcall on-message
                                websocket
                                (e-json-serialize
                                 '(:type "response.incomplete"
                                   :response (:id "resp-incomplete"
                                              :status "incomplete"
                                              :incomplete_details
                                              (:reason "max_output_tokens")))))
                       (funcall on-message
                                websocket
                                (e-json-serialize
                                 '(:type "response.completed"
                                   :response (:id "resp-late"
                                              :status "completed")))))
                   (funcall on-message
                            websocket
                            (e-json-serialize
                             '(:type "response.completed"
                               :response (:id "resp-followup"
                                          :status "completed")))))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args)
                 (cl-incf close-count)))
              ((symbol-function 'run-at-time)
               (lambda (seconds _repeat _callback)
                 (setq scheduled-seconds seconds)
                 'fake-idle-timer))
              ((symbol-function 'timerp)
               (lambda (timer) (eq timer 'fake-idle-timer)))
              ((symbol-function 'cancel-timer) #'ignore))
      ;; A pre-existing clean anchor must survive the unrelated incomplete
      ;; request and remain usable by the immediately following request.
      (e-openai-websocket--session-open session url headers)
      (e-openai-websocket--session-record-response
       session "resp-clean" '(:model "gpt-test"))
      (e-openai-websocket-request-start
       :session session
       :url url
       :headers headers
       :body-data '(:model "gpt-test" :input nil)
       :full-body-data '(:model "gpt-test" :input nil)
       :request-metadata '(:diagnostics nil)
       :idle-close-seconds 17
       :on-item (lambda (item)
                  (when (eq (plist-get item :type) 'done)
                    (cl-incf first-done-items)))
       :on-complete (lambda (_status) (cl-incf first-complete-count))
       :on-error (lambda (err) (setq first-error err)))
      (should (= first-done-items 1))
      (should (= first-complete-count 1))
      (should-not first-error)
      (should-not
       (e-openai-websocket--session-active-request session))
      (should (equal (plist-get (car sends) :previous_response_id) nil))
      (should (equal
               (e-openai-websocket--session-latest-response-id session)
               "resp-clean"))
      ;; The next request starts directly after incomplete settlement and
      ;; reuses the preserved clean anchor without a manual cancellation.
      (let* ((second-request
              (e-openai-websocket-request-start
               :session session
               :url url
               :headers headers
               :body-data '(:model "gpt-test"
                            :input nil
                            :previous_response_id "resp-clean")
               :full-body-data '(:model "gpt-test" :input nil)
               :request-metadata '(:diagnostics nil)
               :idle-close-seconds 17
               :on-item (lambda (item)
                          (when (eq (plist-get item :type) 'done)
                            (cl-incf second-done-items)))
               :on-complete (lambda (_status)
                              (cl-incf second-complete-count))
               :on-error (lambda (err) (setq second-error err))))
             (diagnostics
              (plist-get (e-backend-request-metadata second-request)
                         :diagnostics))
             (second-payload (cadr sends)))
        (should (e-backend-request-p second-request))
        (should (= second-done-items 1))
        (should (= second-complete-count 1))
        (should-not second-error)
        (should (equal (plist-get second-payload :previous_response_id)
                       "resp-clean"))
        (should (eq (plist-get diagnostics :websocket-request-mode)
                    'incremental))
        (should (equal
                 (e-openai-websocket--session-latest-response-id session)
                 "resp-followup")))
      (should-not
       (e-openai-websocket--session-active-request session))
      (should (eq (e-openai-websocket--session-websocket session)
                  'fake-websocket))
      (should (= scheduled-seconds 17))
      (should (= close-count 0)))))


(ert-deftest e-openai-test-websocket-properties-compare-json-object-contents ()
  "Fresh nested JSON objects do not invalidate equivalent tool definitions."
  (let ((first-parameters '(:type "object" :properties nil))
        (second-parameters '(:type "object" :properties nil)))
    (let ((first
           (list :model "gpt-test"
                 :tools (vector (list :name "inspect"
                                      :parameters first-parameters))))
          (second
           (list :model "gpt-test"
                 :tools (vector (list :name "inspect"
                                      :parameters second-parameters)))))
      (should (e-openai-websocket--json-value-equal-p first second))
      (setq second-parameters
            (append second-parameters
                    (list :additionalProperties e-json-false)))
      (setq second
            (list :model "gpt-test"
                  :tools (vector (list :name "inspect"
                                       :parameters second-parameters))))
      (should-not (e-openai-websocket--json-value-equal-p first second)))))


(ert-deftest e-openai-test-websocket-recorded-property-snapshot-detaches-strings ()
  "Mutable request-property strings cannot mutate an anchor snapshot."
  (let* ((e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-websocket-connection-idle-seconds nil)
         (model (copy-sequence "gpt-test"))
         (tool-name (copy-sequence "inspect"))
         (tools (vector (list :type "function" :name tool-name)))
         (session (e-openai-websocket-session-create))
         (url "wss://gateway.example.test/v1/responses")
         (headers nil)
         sends
         on-message)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-message (plist-get args :on-message))
                 'fake-websocket))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let ((payload
                        (e-json-parse-string text)))
                   (setq sends (append sends (list payload)))
                   ;; Leave the first response in flight so the test can
                   ;; mutate both top-level and nested source strings before
                   ;; settlement.  Complete the second request immediately.
                   (when (= (length sends) 2)
                     (funcall on-message
                              websocket
                              (e-json-serialize
                               '(:type "response.completed"
                                 :response (:id "resp-two"
                                            :status "completed"))))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (e-openai-websocket-request-start
       :session session
       :url url
       :headers headers
       :body-data (list :model model :tools tools :input nil)
       :full-body-data (list :model model :tools tools :input nil)
       :request-metadata '(:diagnostics nil)
       :on-item #'ignore
       :on-complete #'ignore
       :on-error #'signal)
      ;; Mutate the original request while the first response is still in
      ;; flight.  Request-start must already own a detached property snapshot.
      (aset model 0 ?X)
      (aset tool-name 0 ?c)
      (funcall on-message
               'fake-websocket
               (e-json-serialize
                '(:type "response.completed"
                  :response (:id "resp-one" :status "completed"))))
      ;; A second mutation after completion must also leave the recorded
      ;; identity unchanged.
      (aset model 0 ?Y)
      (aset tool-name 0 ?l)
      (let ((recorded
             (e-openai-websocket--session-latest-response-properties
              session)))
        (should (equal (plist-get recorded :model) "gpt-test"))
        (should (equal (plist-get (aref (plist-get recorded :tools) 0) :name)
                       "inspect")))
      (let* ((request
              (e-openai-websocket-request-start
               :session session
               :url url
               :headers headers
               :body-data (list :model model
                                :tools tools
                                :input nil
                                :previous_response_id "resp-one")
               :full-body-data (list :model model :tools tools :input nil)
               :request-metadata '(:diagnostics nil)
               :on-item #'ignore
               :on-complete #'ignore
               :on-error #'signal))
             (second (cadr sends))
             (diagnostics
              (plist-get (e-backend-request-metadata request)
                         :diagnostics)))
        ;; The changed mutable leaf forces a complete request; only the latest
        ;; detached property snapshot is retained for immediate continuation.
        (should-not (plist-member second :previous_response_id))
        (should (eq (plist-get diagnostics :websocket-request-mode) 'full))))))


(ert-deftest e-openai-test-context-curation-is-a-reserved-backend-effect ()
  "The Responses adapter decodes context-curate without making a tool call."
  (let ((item
         (e-openai-decoder--event-item
          '(:type "response.output_item.done"
            :item
            (:type "function_call"
             :call_id "curation-call"
             :name "context-curate"
             :arguments
             "{\"keep\":[1],\"summaries\":[{\"sources\":[2,3],\"text\":\"selected\"}],\"erase\":[4]}")))))
    (should (eq (plist-get item :type) 'context-curate))
    (should-not (plist-member item :name))
    (should (equal (plist-get item :arguments)
                   '(:keep [1] :summaries
                           [(:sources [2 3] :text "selected")]
                     :erase [4])))
    (should-not (string-match-p
                 "frame\|observation\|fingerprint\|schema-version"
                 (prin1-to-string (plist-get item :arguments))))))


(ert-deftest e-openai-test-context-curation-carries-function-output-ack ()
  "A reserved Responses curation retains its opaque wire acknowledgement."
  (let* ((item
          (e-openai-decoder--event-item
           '(:type "response.output_item.done"
             :item
             (:type "function_call"
              :call_id "curation-call"
              :name "context-curate"
              :arguments
              "{\"keep\":[1],\"summaries\":[],\"erase\":[]}"))))
         (replay (plist-get item :provider-replay-item))
         (wire-item (plist-get replay :item))
         (corrective
          (cadr (plist-get item :provider-corrective-replay-items)))
         (corrective-wire-item (plist-get corrective :item))
         (invalid
          (cadr (plist-get item :provider-invalid-replay-items)))
         (invalid-wire-item (plist-get invalid :item)))
    (should (equal (plist-get replay :provider-id) 'openai))
    (should (equal (plist-get wire-item :type) "function_call_output"))
    (should (equal (plist-get wire-item :call_id) "curation-call"))
    (should (equal (plist-get wire-item :output) ""))
    (should (equal (plist-get corrective :provider-id) 'openai))
    (should (equal (plist-get corrective-wire-item :type)
                   "function_call_output"))
    (should (equal (plist-get corrective-wire-item :call_id)
                   "curation-call"))
    (should (equal
             (plist-get corrective-wire-item :output)
             e-openai-decoder--context-curation-duplicate-correction))
    (should (equal (plist-get invalid :provider-id) 'openai))
    (should (equal (plist-get invalid-wire-item :type)
                   "function_call_output"))
    (should (equal (plist-get invalid-wire-item :call_id)
                   "curation-call"))
    (should (equal
             (plist-get invalid-wire-item :output)
             e-openai-decoder--context-curation-invalid-correction))))

(ert-deftest e-openai-test-context-curation-v8-guidance-is-frame-scoped ()
  "Reserved carrier guidance states the complete per-frame invocation rule."
  (let* ((tool (e-openai-responses-context-curation-tool-definition))
         (description (plist-get tool :description)))
    (should (equal e-context-lifetime-curation-schema-revision
                   "context-curate-v8"))
    (dolist (meaning '("at most once"
                       "After its acknowledgement, continue"
                       "only after later tool work or context refresh"
                       "labels belong only to the currently presented frame"
                       "cannot be reused for an earlier frame"))
      (should (string-match-p (regexp-quote meaning) description)))
    ;; HTTP and WebSocket Responses share this body projection.  Exercise both
    ;; transport identities so carrier exposure and suppression cannot drift.
    (dolist (transport '(http websocket))
      (let* ((options (list :model "gpt-test"
                            :responses-transport transport
                            :reserved-effect-carrier 'context-curate-wire))
             (body (e-openai-codex-request-body
                    :messages '((:role user :content "curate"))
                    :options options))
             (wire-tool
              (seq-find
               (lambda (candidate)
                 (equal (plist-get candidate :name) "context-curate"))
               (append (plist-get body :tools) nil))))
        (should wire-tool)
        (should (equal (plist-get wire-tool :description) description))
        (should-not
         (seq-find
          (lambda (candidate)
            (equal (plist-get candidate :name) "context-curate"))
          (append
           (plist-get
            (e-openai-codex-request-body
             :messages '((:role user :content "continue"))
             :options (plist-put (copy-sequence options)
                                 :reserved-effect-carrier nil))
            :tools)
           nil)))))))


(ert-deftest e-openai-test-context-curation-full-replay-pair-is-not-anchored-call ()
  "Full replay restores the curation call/output pair; anchors send output only."
  (let* ((effect (e-openai-decoder--context-curation-effect
                  '(:keep [1] :summaries [] :erase []) "curation-call"))
         (replays (plist-get effect :provider-replay-items))
         (messages
          `((:role user :content "prompt")
            (:role tool
             :content (:tool-call-id "tool-call" :content "result")
             :metadata (:provider-replay-items ,replays))))
         (full
          (e-openai-codex-request-body
           :messages messages
           :options '(:model "gpt-test")
           :tools nil))
         (incremental
          (e-openai-codex-request-body
           :messages messages
           :options
           '(:model "gpt-test"
             :provider-continuation t
             :response-store t
             :provider-anchor
             (:provider-id openai
              :metadata (:response-id "resp-curation"
                         :reasoning-identity
                         (:effort "high" :summary "auto")))
             :provider-anchor-delta-messages
             ((:role tool
               :content (:tool-call-id "tool-call" :content "result")
               :metadata (:provider-replay-items
                          ((:type provider-replay-item
                            :provider-id openai
                            :full-replay-only t
                            :item (:type "function_call"
                                   :call_id "curation-call"
                                   :name "context-curate"
                                   :arguments "{\"keep\":[1],\"summaries\":[],\"erase\":[]}"))
                           (:type provider-replay-item
                            :provider-id openai
                            :item (:type "function_call_output"
                                   :call_id "curation-call"
                                   :output "")))))))
           :tools nil))
         (full-input (append (plist-get full :input) nil))
         (incremental-input (append (plist-get incremental :input) nil))
         (full-call
          (cl-position-if
           (lambda (item)
             (and (equal (plist-get item :type) "function_call")
                  (equal (plist-get item :name) "context-curate")
                  (equal (plist-get item :call_id) "curation-call")))
           full-input))
         (full-output
          (cl-position-if
           (lambda (item)
             (and (equal (plist-get item :type) "function_call_output")
                  (equal (plist-get item :call_id) "curation-call")))
           full-input))
         (full-ordinary-output
          (cl-position-if
           (lambda (item)
             (and (equal (plist-get item :type) "function_call_output")
                  (equal (plist-get item :call_id) "tool-call")))
           full-input))
         (incremental-output
          (cl-position-if
           (lambda (item)
             (and (equal (plist-get item :type) "function_call_output")
                  (equal (plist-get item :call_id) "curation-call")))
           incremental-input))
         (incremental-ordinary-output
          (cl-position-if
           (lambda (item)
             (and (equal (plist-get item :type) "function_call_output")
                  (equal (plist-get item :call_id) "tool-call")))
           incremental-input)))
    (should-not (plist-member full :previous_response_id))
    (should (integerp full-ordinary-output))
    (should (integerp full-call))
    (should (integerp full-output))
    (should (< full-ordinary-output full-call))
    (should (< full-call full-output))
    (should (equal (plist-get incremental :previous_response_id)
                   "resp-curation"))
    (should (integerp incremental-ordinary-output))
    (should (integerp incremental-output))
    (should (< incremental-ordinary-output incremental-output))
    (should-not
     (seq-find
      (lambda (item)
        (and (equal (plist-get item :type) "function_call")
             (equal (plist-get item :name) "context-curate")))
      incremental-input))))


(ert-deftest e-openai-test-provider-compaction-rejects-malformed-output ()
  "Malformed compact responses fail at the adapter boundary."
  (dolist (response
           '("{\"object\":\"response\",\"output\":[]}"
             "{\"object\":\"response.compaction\",\"output\":null}"
             "{\"object\":\"response.compaction\",\"output\":{}}"))
    (should-error
     (e-openai-compaction--decode response)
     :type 'e-openai-provider-invalid)))

(provide 'e-openai-mechanism-test)

;;; e-openai-mechanism-test.el ends here

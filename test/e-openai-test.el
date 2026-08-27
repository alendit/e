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
(require 'e-loop)
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

(defun e-openai-test--assert-observation-delivery (capabilities replaceable)
  "Assert per-kind delivery in CAPABILITIES according to REPLACEABLE."
  (dolist (kind '(current-state dynamic-context))
    (should
     (eq (e-backend-observation-delivery-for-kind capabilities kind)
         (if replaceable 'request-local-replaceable 'inherited))))
  (dolist (kind '(tool-result trace retrieved-excerpt))
    (should
     (eq (e-backend-observation-delivery-for-kind capabilities kind)
         'inherited))))

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

(ert-deftest e-openai-test-http-timeout-default-is-disabled ()
  "Buffered HTTP reasoning responses have no unsafe implicit deadline."
  (should-not (default-value 'e-openai-request-timeout-seconds)))

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
            (e-openai--migrate-http-timeout-default)
            (should-not e-openai-request-timeout-seconds))
          (put symbol 'saved-value '(180))
          (let ((e-openai-request-timeout-seconds 180))
            (e-openai--migrate-http-timeout-default)
            (should (= e-openai-request-timeout-seconds 180))))
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
  "GPT-5.6 keeps the stable prefix before an inherited late frontier."
  (let* ((options
          '(:model "gpt-5.6-sol"
            :instructions "Base instructions."
            :prompt-cache-key "cache-key"
            :observation-delivery inherited))
         (first-messages
          '((:role system :content "Static instructions.")
            (:role system :content "Stable project guidance.")
            (:role user :content "hello C")
            (:role system :content "Current buffer state C.")))
         (second-messages
          '((:role system :content "Static instructions.")
            (:role system :content "Stable project guidance.")
            (:role user :content "hello D")
            (:role system :content "Current buffer state D.")))
         (segments
          '((:kind static-prefix
             :id static
             :messages ((:role system :content "Static instructions.")))
            (:kind stable-context
             :id stable
             :messages ((:role system :content "Stable project guidance.")))
            (:kind history
             :id history
             :messages ((:role user :content "hello C")))
            (:kind current-state
             :id dynamic
             :messages ((:role system :content "Current buffer state C.")))))
         (first
          (e-openai-codex-request-body
           :messages first-messages
           :options (plist-put (copy-tree options) :segments segments)))
         (second-segments
          (copy-tree segments))
         (_ (plist-put
             (car (plist-get (nth 2 second-segments) :messages))
             :content "hello D"))
         (_ (plist-put
             (car (plist-get (nth 3 second-segments) :messages))
             :content "Current buffer state D."))
         (second
          (e-openai-codex-request-body
           :messages second-messages
           :options (plist-put (copy-tree options)
                               :segments second-segments)))
         (first-input (append (plist-get first :input) nil))
         (second-input (append (plist-get second :input) nil))
         (first-static-block (aref (plist-get (nth 0 first-input) :content) 0))
         (first-stable-block (aref (plist-get (nth 1 first-input) :content) 0))
         (first-frontier-block
          (aref (plist-get (nth 3 first-input) :content) 0)))
    (should (equal (plist-get first :instructions) "Base instructions."))
    (should (equal (mapcar (lambda (item) (plist-get item :role)) first-input)
                   '("developer" "developer" "user" "developer")))
    (should (equal (mapcar (lambda (item) (plist-get item :role)) second-input)
                   '("developer" "developer" "user" "developer")))
    ;; The stable prefix is byte/order-identical while only the late frontier
    ;; changes.  The observation retains instruction authority as developer
    ;; input and is not copied into top-level instructions.
    (should (equal (cl-subseq first-input 0 2)
                   (cl-subseq second-input 0 2)))
    (should (equal (plist-get (aref (plist-get (nth 2 first-input) :content) 0)
                             :text)
                   "hello C"))
    (should (equal (plist-get first-frontier-block :text)
                   "Current buffer state C."))
    (should-not (string-match-p "Current buffer state C."
                                (prin1-to-string second)))
    (should (equal (plist-get
                    (aref (plist-get (nth 3 second-input) :content) 0)
                    :text)
                   "Current buffer state D."))
    (should-not (plist-member first-static-block :prompt_cache_breakpoint))
    (should (equal (plist-get first-stable-block :prompt_cache_breakpoint)
                   '(:mode "explicit")))
    (should-not (plist-member first-frontier-block :prompt_cache_breakpoint))
    (should (equal (plist-get first :prompt_cache_options)
                   '(:mode "explicit")))
    (should (equal (plist-get first :prompt_cache_key) "cache-key"))))

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

(ert-deftest e-openai-test-gpt56-without-cache-key-direct-body-keeps-flattened-shape ()
  "A direct GPT-5.6 body without segments keeps the legacy projection."
  (let ((body
         (e-openai-codex-request-body
          :messages '((:role system :content "Stable instructions.")
                      (:role system :content "Dynamic state.")
                      (:role user :content "hello"))
          :options
          '(:model "gpt-5.6-sol"))))
    (should (equal (plist-get body :instructions)
                   "You are a helpful assistant.\n\nStable instructions.\n\nDynamic state."))
    (should (equal (length (plist-get body :input)) 1))
    (should-not (plist-member body :prompt_cache_options))))

(ert-deftest e-openai-test-inherited-frontier-without-cache-key-keeps-developer-input ()
  "A developer-input profile keeps an inherited frontier without cache fields."
  (let* ((messages
          '((:role system :content "Static instructions.")
            (:role system :content "Stable project guidance.")
            (:role user :content "hello")
            (:role system :content "Current buffer state.")))
         (body
          (e-openai-codex-request-body
           :messages messages
           :options
           '(:model "gpt-5.5"
             :responses-context-layout developer-input
             :observation-delivery inherited
             :segments ((:kind static-prefix
                         :messages ((:role system
                                     :content "Static instructions.")))
                        (:kind stable-context
                         :messages ((:role system
                                     :content "Stable project guidance.")))
                        (:kind history
                         :messages ((:role user :content "hello")))
                        (:kind current-state
                         :messages ((:role system
                                     :content "Current buffer state.")))))))
         (input (append (plist-get body :input) nil)))
    (should (equal (plist-get body :instructions)
                   "You are a helpful assistant."))
    (should-not (plist-member body :prompt_cache_key))
    (should-not (plist-member body :prompt_cache_options))
    (should (equal (mapcar (lambda (item) (plist-get item :role)) input)
                   '("developer" "developer" "user" "developer")))
    (should (equal
             (plist-get
              (aref (plist-get (nth 0 input) :content) 0)
              :text)
             "Static instructions."))
    (should (equal
             (plist-get
              (aref (plist-get (nth 3 input) :content) 0)
              :text)
             "Current buffer state."))
    (should-not
     (plist-member
      (aref (plist-get (nth 1 input) :content) 0)
      :prompt_cache_breakpoint))))

(ert-deftest e-openai-test-inherited-curation-frontier-keeps-markers-and-literals ()
  "Inherited curation sources keep marker/source order and exact literals."
  (let* ((source-one '(:kind "structured" :value 7 :items (alpha beta)))
         (source-two '(:kind "structured" :value 9 :items (gamma delta)))
         (segments-one
          `((:kind static-prefix
             :messages ((:role system :content "STATIC-POLICY")))
            (:kind stable-context
             :messages ((:role system :content "STABLE-GUIDANCE")))
            (:kind history
             :messages ((:role user :content "durable prompt")))
            (:kind dynamic-context
             :messages ((:role system :content "[1, ~2 tokens]")
                        (:role system :content ,source-one)
                        (:role system :content "[2, ~3 tokens]")
                        (:role system :content "SOURCE-TWO")))))
         (segments-two
          `((:kind static-prefix
             :messages ((:role system :content "STATIC-POLICY")))
            (:kind stable-context
             :messages ((:role system :content "STABLE-GUIDANCE")))
            (:kind history
             :messages ((:role user :content "durable prompt")))
            (:kind dynamic-context
             :messages ((:role system :content "[1, ~2 tokens]")
                        (:role system :content ,source-two)
                        (:role system :content "[2, ~3 tokens]")
                        (:role system :content "SOURCE-TWO")))))
         (messages-one
          (append
           '((:role system :content "STATIC-POLICY")
             (:role system :content "STABLE-GUIDANCE")
             (:role user :content "durable prompt"))
           (plist-get (car (last segments-one)) :messages)))
         (messages-two
          (append
           '((:role system :content "STATIC-POLICY")
             (:role system :content "STABLE-GUIDANCE")
             (:role user :content "durable prompt"))
           (list (list :role 'system :content "[1, ~2 tokens]")
                 (list :role 'system :content source-two)
                 (list :role 'system :content "[2, ~3 tokens]")
                 (list :role 'system :content "SOURCE-TWO"))))
         (options-one
          `(:model "gpt-5.5"
            :responses-context-layout developer-input
            :observation-delivery inherited
            :segments ,segments-one))
         (options-two
          `(:model "gpt-5.5"
            :responses-context-layout developer-input
            :observation-delivery inherited
            :segments ,segments-two))
         (body-one (e-openai-codex-request-body
                    :messages messages-one :options options-one))
         (body-two (e-openai-codex-request-body
                    :messages messages-two :options options-two))
         (input-one (append (plist-get body-one :input) nil))
         (input-two (append (plist-get body-two :input) nil))
         (prefix-one (cl-subseq input-one 0 3))
         (prefix-two (cl-subseq input-two 0 3))
         (late-one (nthcdr 3 input-one))
         (wire (json-encode body-one)))
    (should (equal (mapcar (lambda (item) (plist-get item :role)) input-one)
                   '("developer" "developer" "user"
                     "developer" "developer" "developer" "developer")))
    (should (equal (mapcar (lambda (item)
                             (plist-get (aref (plist-get item :content) 0)
                                        :text))
                           late-one)
                   (list "[1, ~2 tokens]"
                         (json-encode source-one)
                         "[2, ~3 tokens]"
                         "SOURCE-TWO")))
    (should (equal prefix-one prefix-two))
    (should (equal (plist-get (aref (plist-get (nth 0 input-one) :content) 0)
                             :text)
                   "STATIC-POLICY"))
    (should (equal (plist-get (aref (plist-get (nth 2 input-one) :content) 0)
                             :text)
                   "durable prompt"))
    (should (= (cl-count "SOURCE-TWO"
                         (mapcar (lambda (item)
                                   (plist-get
                                    (aref (plist-get item :content) 0)
                                    :text))
                                 input-one)
                         :test #'equal)
               1))
    (should-not (string-match-p
                 "frame\|generation\|observation\|fingerprint\|backing\|replay"
                 wire))))

(ert-deftest e-openai-test-inherited-frontier-rejects-mismatched-canonical-partition ()
  "A canonical inherited frontier cannot silently accept a mismatched partition."
  (should-error
   (e-openai-codex-request-body
    :messages '((:role system :content "Stable instructions.")
                (:role user :content "hello")
                (:role system :content "Current buffer state."))
    :options
    '(:model "gpt-5.5"
      :responses-context-layout developer-input
      :observation-delivery inherited
      :segments ((:kind stable-context
                  :messages ((:role system
                              :content "Stable instructions.")))
                 (:kind current-state
                  :messages ((:role system
                              :content "Current buffer state."))))))
   :type 'e-openai-context-projection-invalid))

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
         (first (e-openai-codex--prompt-layout-revision first-options))
         (second (e-openai-codex--prompt-layout-revision second-options)))
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
              (e-openai-codex--prompt-layout-revision first-options))))
    (let ((e-context-lifetime-curation-presentation-revision
           "context-curation-presentation-test-v2"))
      (should-not
       (equal first
              (e-openai-codex--prompt-layout-revision first-options))))
    (let* ((anchor
            (list :provider-id 'openai
                  :metadata (list :response-id "response-curation"
                                  :prompt-layout-revision first)))
           (continuation-options
            (plist-put (copy-sequence first-options)
                       :provider-anchor anchor)))
      (should (equal
               (e-openai-codex--continuation-response-id
                continuation-options)
               "response-curation"))
      (let ((e-context-budget-estimate-bytes-per-token 2.0))
        (should-not
         (e-openai-codex--continuation-response-id
          continuation-options))))))

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
             :observation-delivery inherited
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

(ert-deftest e-openai-test-request-local-current-state-replaces-instructions ()
  "A proven replaceable frontier stays in request-local instructions."
  (let* ((segments
          '((:kind static-prefix
             :id static
             :messages ((:role system :content "Static policy.")))
            (:kind stable-context
             :id stable
             :messages ((:role system :content "Stable project guidance.")))
            (:kind current-state
             :id current
             :messages ((:role system :content "Current canvas.")))
            (:kind history
             :id history
             :messages ((:role user :content "first prompt")))))
         (second-segments
          '((:kind static-prefix
             :id static
             :messages ((:role system :content "Static policy.")))
            (:kind stable-context
             :id stable
             :messages ((:role system :content "Stable project guidance.")))
            (:kind current-state
             :id current
             :messages ((:role system :content "Changed canvas.")))
            (:kind history
             :id first-prompt
             :messages ((:role user :content "first prompt")))
            (:kind history
             :id first-answer
             :messages ((:role assistant :content "first answer")))
            (:kind history
             :id second-prompt
             :messages ((:role user :content "second prompt")))))
         (base-options
          `(:model "gpt-5.6-sol"
            :instructions "Base instructions."
            :prompt-cache-key "cache-key"
            :prompt-cache-breakpoint-mode explicit
            :responses-context-layout developer-input
            :observation-delivery request-local-replaceable
            :context-capabilities (:observation-delivery
                                   request-local-replaceable)
            :replaceable-current-state
            ((:role system :content "Current canvas."))
            :context-segment-message-count 4
            :segments ,segments))
         (first-body
          (e-openai-codex-request-body
           :messages '((:role system :content "Static policy.")
                       (:role system :content "Stable project guidance.")
                       (:role system :content "Current canvas.")
                       (:role user :content "first prompt"))
           :options base-options))
         (second-options
          (append
           base-options
           `(:provider-continuation t
             :provider-anchor
             (:provider-id openai
              :metadata (:response-id "resp-1"
                         :prompt-layout-revision
                         ,e-openai-gpt56-explicit-cache-layout-revision))
             :provider-anchor-delta-messages
             ((:role user :content "second prompt")))))
         (second-body
          (e-openai-codex-request-body
           :messages '((:role system :content "Static policy.")
                       (:role system :content "Stable project guidance.")
                       (:role system :content "Changed canvas.")
                       (:role user :content "first prompt")
                       (:role assistant :content "first answer")
                       (:role user :content "second prompt"))
           :options
           (plist-put
            (plist-put
             (plist-put second-options
                        :replaceable-current-state
                        '((:role system :content "Changed canvas.")))
             :segments
             second-segments)
            :context-segment-message-count
            (length (cl-loop for segment in second-segments
                             append (plist-get segment :messages))))))
    (should (equal (plist-get first-body :instructions)
                   "Base instructions.\n\nCurrent canvas."))
    (should (equal (mapcar (lambda (item) (plist-get item :role))
                           (append (plist-get first-body :input) nil))
                   '("developer" "developer" "user")))
    (should (equal (plist-get second-body :instructions)
                   "Base instructions.\n\nChanged canvas."))
    (should (equal (plist-get second-body :previous_response_id) "resp-1"))
    (should (equal (mapcar (lambda (item) (plist-get item :role))
                           (append (plist-get second-body :input) nil))
                   '("user")))
    (should (equal (plist-get
                    (aref (plist-get (aref (plist-get first-body :input) 0)
                                     :content)
                          0)
                    :text)
                   "Static policy.")))))

(ert-deftest e-openai-test-replaceable-frontier-does-not-delete-equal-durable-messages ()
  "Semantic segments retain equal durable messages in full and anchored input."
  (let* ((same-message '(:role system :content "same value"))
         (segments `((:kind stable-context
                      :messages ((:role system :content "stable")))
                     (:kind history
                      :messages (,same-message))
                     (:kind current-state
                      :messages (,same-message))
                     (:kind history
                      :messages ((:role user :content "prompt")))))
         (base-options `(:model "gpt-5.6-sol"
                         :prompt-cache-key "equal-key"
                         :prompt-cache-breakpoint-mode explicit
                         :responses-context-layout developer-input
                         :observation-delivery request-local-replaceable
                         :context-capabilities
                         (:observation-delivery request-local-replaceable)
                         :replaceable-current-state (,same-message)
                         :context-segment-message-count 4
                         :segments ,segments))
         (messages `((:role system :content "stable")
                     ,same-message
                     ,same-message
                     (:role user :content "prompt")))
         (full (e-openai-codex-request-body
                :messages messages
                :options base-options))
         (anchored (e-openai-codex-request-body
                    :messages messages
                    :options
                    (append base-options
                            '(:provider-continuation t
                              :provider-anchor
                              (:provider-id openai
                               :metadata (:response-id "resp-equal"
                                          :prompt-layout-revision
                                          "responses-explicit-cache-v1"))
                              :provider-anchor-delta-messages
                              ((:role system :content "same value"))
                              :provider-anchor-source-message-count 4)))))
    ;; The current-state occurrence is represented by the semantic segment;
    ;; the equal history occurrence remains explicit input.
    (should (= (cl-count "same value"
                         (mapcar (lambda (item)
                                   (plist-get (aref (plist-get item :content) 0)
                                              :text))
                                 (append (plist-get full :input) nil))
                         :test #'equal)
               1))
    (should (equal (plist-get anchored :previous_response_id) "resp-equal"))
    (should (= (cl-count "same value"
                         (mapcar (lambda (item)
                                   (plist-get (aref (plist-get item :content) 0)
                                              :text))
                                 (append (plist-get anchored :input) nil))
                         :test #'equal)
               1))
    (should (equal (plist-get
                    (aref (plist-get (aref (plist-get anchored :input) 0)
                                     :content)
                          0)
                    :text)
                   "same value"))))

(ert-deftest e-openai-test-replaceable-frontier-rejects-ambiguous-partition ()
  "A non-empty replaceable frontier cannot silently remain explicit input."
  (dolist (options
           (list
            '(:model "gpt-5.6-sol"
              :responses-context-layout developer-input
              :observation-delivery request-local-replaceable
              :replaceable-current-state
              ((:role system :content "current"))
              ;; These are reserved harness-derived values.  A direct caller
              ;; must not be able to turn them into an ambiguity escape.
              :replaceable-current-state-partitioned t
              :context-segment-message-count 999)
            '(:model "gpt-5.6-sol"
              :responses-context-layout developer-input
              :observation-delivery request-local-replaceable
              :replaceable-current-state
              ((:role system :content "current"))
              :context-segment-message-count 1
              :segments
              ((:kind stable-context
                :messages ((:role system :content "stable")))))
            '(:model "gpt-5.6-sol"
              :responses-context-layout developer-input
              :observation-delivery request-local-replaceable
              :replaceable-current-state
              ((:role system :content "current"))
              ;; A forged delta/count pair cannot authorize a segment list
              ;; whose content is not the exact request prefix.
              :provider-anchor-delta-messages
              ((:role user :content "new prompt"))
              :context-segment-message-count 1
              :segments
              ((:kind stable-context
                :messages ((:role system :content "not the prefix")))))))
    (should-error
     (e-openai-codex-request-body
      :messages '((:role system :content "stable")
                  (:role system :content "current"))
      :options options)
     :type 'e-openai-context-projection-invalid)))

(ert-deftest e-openai-test-profile-context-capabilities-are-conservative ()
  "Only an explicitly proven OpenAI profile gets replaceable delivery."
  (let ((replaceable
         (e-openai--profile-context-capabilities
          '(:wire-api responses
            :continuation t
            :observation-delivery request-local-replaceable
            :prompt-cache-breakpoint-mode explicit)
          nil))
        (inherited
         (e-openai--profile-context-capabilities
          '(:wire-api responses
            :continuation t
            :prompt-cache-breakpoint-mode explicit)
          nil))
        (chat
         (e-openai--profile-context-capabilities
          '(:wire-api chat-completion :continuation t)
          nil)))
    (e-openai-test--assert-observation-delivery replaceable t)
    (should (eq (plist-get replaceable :continuation) 'linear))
    (e-openai-test--assert-observation-delivery inherited nil)
    (e-openai-test--assert-observation-delivery chat nil)
    (should (eq (plist-get chat :continuation) 'none))))

(ert-deftest e-openai-test-profile-context-capabilities-reject-unknown-delivery ()
  "A misspelled observation delivery never silently selects inherited mode."
  (should-error
   (e-openai--profile-context-capabilities
    '(:wire-api responses
      :continuation t
      :observation-delivery request-local-replacable)
    nil)
   :type 'e-openai-provider-invalid))

(ert-deftest e-openai-test-first-party-capabilities-follow-effective-identity ()
  "First-party replacement proof does not cross endpoint or transport overrides."
  (let* ((openai-profile (e-openai-provider-profile 'openai))
         (codex-profile (e-openai-provider-profile 'codex))
         (canonical-openai
          (e-openai--profile-context-capabilities
           openai-profile nil :provider 'openai :request-function nil))
         (canonical-codex
          (e-openai--profile-context-capabilities
           codex-profile nil :provider 'codex :request-function nil))
         (openai-endpoint-override
          (e-openai--profile-context-capabilities
           openai-profile nil
           :provider 'openai
           :base-url "https://gateway.example.test/v1"))
         (codex-endpoint-override
          (e-openai--profile-context-capabilities
           codex-profile nil
           :provider 'codex
           :base-url "https://gateway.example.test/codex"))
         (openai-request-override
          (e-openai--profile-context-capabilities
           openai-profile nil :provider 'openai :request-function #'ignore))
         (codex-request-override
          (e-openai--profile-context-capabilities
           codex-profile nil :provider 'codex :request-function #'ignore))
         (openai-transport-override
          (e-openai--profile-context-capabilities
           openai-profile '(:responses-transport http) :provider 'openai))
         (codex-transport-override
          (e-openai--profile-context-capabilities
           codex-profile '(:responses-transport http) :provider 'codex))
         (noncanonical-codex
          (e-openai--profile-context-capabilities
           (plist-put (copy-sequence codex-profile)
                      :name "Custom Codex")
           nil :provider 'codex))
         (custom-profile
          (plist-put
           (plist-put (copy-sequence openai-profile)
                      :base-url "https://custom.example.test/v1")
           :observation-delivery 'request-local-replaceable))
         (custom-proof (e-openai--profile-context-capabilities
                        custom-profile nil
                        :provider 'custom-proven
                        :request-function #'ignore))
         (custom-without-proof
          (e-openai--profile-context-capabilities
           (let ((copy (copy-sequence custom-profile)))
             (cl-remf copy :observation-delivery)
             copy)
           nil
           :provider 'custom-unproven
           :request-function #'ignore))
         (custom-endpoint-override
          (e-openai--profile-context-capabilities
           custom-profile nil
           :provider 'custom-proven
           :base-url "https://other.example.test/v1"
           :request-function #'ignore))
         (custom-transport-override
          (e-openai--profile-context-capabilities
           custom-profile '(:responses-transport http)
           :provider 'custom-proven
           :request-function #'ignore)))
    (dolist (capabilities (list canonical-openai canonical-codex))
      (should (eq (plist-get capabilities :observation-delivery)
                  'inherited))
      (e-openai-test--assert-observation-delivery capabilities nil)
      (should (eq (plist-get capabilities :continuation) 'linear)))
    (dolist (capabilities (list openai-endpoint-override
                                codex-endpoint-override
                                openai-request-override
                                codex-request-override
                                openai-transport-override
                                codex-transport-override
                                noncanonical-codex
                                custom-endpoint-override
                                custom-transport-override))
      (e-openai-test--assert-observation-delivery capabilities nil)
      (should (eq (plist-get capabilities :continuation) 'none)))
    (e-openai-test--assert-observation-delivery custom-without-proof nil)
    (should (eq (plist-get custom-without-proof :continuation) 'linear))
    ;; A named custom profile carries its own endpoint proof and may use the
    ;; injected requester used by its conformance test.
    (should (equal (plist-get custom-proof :observation-delivery)
                   e-openai--request-local-observation-delivery-map))
    (e-openai-test--assert-observation-delivery custom-proof t)
    (should (eq (plist-get custom-proof :continuation) 'linear))))

(ert-deftest e-openai-test-first-party-endpoint-override-disables-harness-anchor ()
  "An effective first-party endpoint override uses stateless harness context."
  (let* ((process-environment
          (cons "OPENAI_API_KEY=test-api-token" process-environment))
         (harness
          (e-openai-create-harness
           :provider 'openai
           :base-url "https://gateway.example.test/v1"
           :request-function #'ignore))
         (provider
          (e-context-provider-create
           :name 'override-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list '(:role system :content "override state"))))))
    (e-harness-activate-capability
     harness
     (e-capability-create
      :id 'override-state-capability
      :context-providers (list provider)))
    (e-harness-create-session harness :id "override-session")
    (let* ((context (e-harness-turn-context
                     harness "override-session" "override-turn"))
           (options (plist-get context :options)))
      (should (eq (plist-get options :observation-delivery) 'inherited))
      (should (eq (plist-get (plist-get options :context-capabilities)
                            :continuation)
                  'none))
      (should (eq (plist-get options :context-rendering-strategy)
                  'stateless)))))

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
         (normalized (e-openai--normalize-model-providers providers))
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

(ert-deftest e-openai-test-request-context-rejects-negative-websocket-idle-policy-before-request ()
  "A negative explicit policy fails before opening a WebSocket."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token"
                process-environment))
         (e-openai-model-providers
          '((websocket-policy
             :name "WebSocket Policy"
             :base-url "https://gateway.example.test/v1"
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :websocket-idle-close-seconds -1
             :requires-openai-auth nil)))
         (backend (e-openai-backend-create :provider 'websocket-policy))
         (websocket-opened nil))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (&rest _args)
                 (setq websocket-opened t)
                 'fake-websocket)))
      (should-error
       (e-backend-start backend
                       :messages '((:role user :content "hello"))
                       :options '(:model "gpt-test")
                       :on-item #'ignore
                       :on-done #'ignore
                       :on-error #'ignore)
       :type 'e-openai-provider-invalid))
    (should-not websocket-opened)))

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
    (let ((details (e-openai--normalize-error-details
                    content (plist-get item :payload) nil)))
      (should (e-harness--retryable-error-p details))
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
    (let ((details (e-openai--normalize-error-details
                    content (plist-get item :payload) nil)))
      (should (e-harness--retryable-error-p details))
      (should (eq (plist-get details :retry-reason) 'provider-unavailable)))))

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
        (let ((details (e-openai--normalize-error-details
                        (plist-get item :content) payload nil)))
          (should (e-harness--retryable-error-p details))
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
        (let ((details (e-openai--normalize-error-details
                        (plist-get item :content) payload nil)))
          (should (e-harness--retryable-error-p details))
          (should (= (plist-get details :retry-after-seconds) 7))
          (should (eq (plist-get details :retry-reason) 'rate-limit)))))))

(ert-deftest e-openai-test-normalizes-provider-retry-hints ()
  "The OpenAI adapter owns transient classification and reset parsing."
  (let* ((now (float-time
               (encode-time (parse-time-string
                             "2026-07-03 08:20:00 +0000"))))
         (absolute
          (e-openai--retry-after-from-text
           (concat "429 rate limit. Limit resets at: "
                   "2026-07-03 08:23:02 UTC")
           now)))
    (should (= absolute 182.0))
    (should (= (e-openai--retry-after-from-text
                "please retry after 2 minutes" now)
               120.0)))
  (dolist (case
           '(("Rate limit exceeded" nil rate-limit)
             ("server_error: Generation failed" nil provider-unavailable)
             ("connection reset by peer" nil transport)
             ("stream ended prematurely" nil premature-stream)
             ("request failed" (:status 503) provider-unavailable)))
    (pcase-let ((`(,message ,payload ,reason) case))
      (let ((details (e-openai--normalize-error-details message payload nil)))
        (should (e-harness--retryable-error-p details))
        (should (eq (plist-get details :retry-reason) reason)))))
  (should-not
   (e-harness--retryable-error-p
    (e-openai--normalize-error-details "500: internal error" nil nil)))
  (should-not
   (e-harness--retryable-error-p
    (e-openai--normalize-error-details
     "invalid request" '(:status 400) nil))))

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

(ert-deftest e-openai-test-http-idle-timeout-rearms-on-real-url-chunks ()
  "Actual `url.el' process-filter chunks extend an explicit idle timeout."
  (let* ((e-openai-request-timeout-seconds 0.12)
         (url-proxy-services nil)
         (server nil)
         (clients nil)
         (timers nil)
         response
         error)
    (cl-labels
        ((send-chunk (client data)
           (when (process-live-p client)
             (process-send-string
              client
              (format "%x\r\n%s\r\n" (string-bytes data) data))))
         (serve-request (client data)
           (when (string-match-p "\r\n\r\n" data)
             (set-process-filter client #'ignore)
             (process-send-string
              client
              (concat "HTTP/1.1 200 OK\r\n"
                      "Content-Type: text/event-stream\r\n"
                      "Transfer-Encoding: chunked\r\n"
                      "Connection: close\r\n\r\n"))
             (push (run-at-time
                    0.08 nil #'send-chunk client
                    "data: {\"type\":\"response.created\"}\n\n")
                   timers)
             (push (run-at-time
                    0.16 nil #'send-chunk client
                    "data: {\"type\":\"response.completed\"}\n\n")
                   timers)
             (push (run-at-time
                    0.24 nil
                    (lambda ()
                      (when (process-live-p client)
                        (process-send-string client "0\r\n\r\n")
                        (process-send-eof client))))
                   timers))))
      (unwind-protect
          (progn
            (setq server
                  (make-network-process
                   :name "e-openai-stream-server"
                   :server t
                   :host 'local
                   :service t
                   :noquery t
                   :log (lambda (_server client _message)
                          (push client clients)
                          (set-process-query-on-exit-flag client nil)
                          (set-process-filter client #'serve-request))))
            (let ((port (process-contact server :service)))
              (e-openai-codex--http-request-start
               :url (format "http://127.0.0.1:%s/responses" port)
               :headers '(("Content-Type" . "application/json"))
               :body "{}"
               :on-complete (lambda (value) (setq response value))
               :on-error (lambda (err) (setq error err))))
            (should (e-openai-test--wait-until
                     (lambda () (or response error)) 1.0))
            (should-not error)
            (should (string-match-p "response.completed" response)))
        (dolist (timer timers)
          (when (timerp timer)
            (cancel-timer timer)))
        (dolist (client clients)
          (when (process-live-p client)
            (delete-process client)))
        (when (process-live-p server)
          (delete-process server))))))

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

(ert-deftest e-openai-test-websocket-retains-only-latest-response ()
  "Only the latest completed response can authorize immediate continuation."
  (let* ((session (e-openai-codex--websocket-session-create))
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
                        (json-parse-string text
                                           :object-type 'plist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :json-false)))
                   (setq sends (append sends (list payload)))
                   (funcall
                    on-message
                    websocket
                    (json-encode
                     `(:type "response.completed"
                       :response (:id ,(format "resp-%d"
                                               (cl-incf response-index))
                                  :status "completed")))))))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args) t)))
      (cl-labels
          ((start (body full)
             (e-openai-codex--websocket-request-start
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
                 (e-openai-codex--websocket-session-latest-response-id session)
                 "resp-3"))
        (should (equal
                 (e-openai-codex--websocket-session-latest-response-properties
                  session)
                 '(:model "gpt-test")))))))

(ert-deftest e-openai-test-websocket-profile-idle-close-nil-does-not-schedule ()
  "A nil resolved idle policy retains the existing no-timer fallback."
  (let ((session (e-openai-codex--websocket-session-create))
        scheduled)
    (setf (e-openai-codex--websocket-session-websocket session) 'fake-websocket
          (e-openai-codex--websocket-session-connection-id session) "e-ws-test")
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 (setq scheduled t)
                 'fake-timer)))
      (e-openai-codex--websocket-schedule-idle-close session nil))
    (should-not scheduled)
    (should-not
     (e-openai-codex--websocket-session-idle-timer session))))

(ert-deftest e-openai-test-websocket-profile-idle-close-explicit-zero-schedules-value ()
  "An explicit zero-second profile policy reaches the scheduler unchanged."
  (let ((session (e-openai-codex--websocket-session-create))
        scheduled-seconds)
    (setf (e-openai-codex--websocket-session-websocket session) 'fake-websocket
          (e-openai-codex--websocket-session-connection-id session) "e-ws-test")
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seconds _repeat _callback)
                 (setq scheduled-seconds seconds)
                 'fake-timer))
              ((symbol-function 'timerp)
               (lambda (timer) (eq timer 'fake-timer)))
              ((symbol-function 'cancel-timer) #'ignore))
      (e-openai-codex--websocket-schedule-idle-close session 0))
    (should (= scheduled-seconds 0))
    (should (eq (e-openai-codex--websocket-session-idle-timer session)
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
                             (json-encode
                              '(:type "response.output_text.delta"
                                :delta "partial"))))
                   ('failed
                    (funcall on-message
                             websocket
                             (json-encode
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
                   (e-openai-codex--websocket-session-latest-response-id
                    session))
                  (null
                   (e-openai-codex--websocket-session-latest-response-properties
                    session))))
           (prepare-session ()
             (let ((session (e-openai-codex--websocket-session-create)))
               (e-openai-codex--websocket-session-open session url headers)
               (e-openai-codex--websocket-session-record-response
                session "prior-response" '(:model "gpt-test"))
               session))
           (start (session on-item on-error)
             (e-openai-codex--websocket-request-start
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
                   (e-openai-codex--websocket-session-latest-response-id
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
           (e-openai-codex--websocket-session-websocket session))
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
           (e-openai-codex--websocket-session-websocket session))
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
         (session (e-openai-codex--websocket-session-create)))
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
                         (json-parse-string text
                                            :object-type 'plist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object :json-false))))
                 (if (= send-count 1)
                     (progn
                       ;; A later completed frame must be ignored after the
                       ;; incomplete terminal event has settled the request.
                       (funcall on-message
                                websocket
                                (json-encode
                                 '(:type "response.incomplete"
                                   :response (:id "resp-incomplete"
                                              :status "incomplete"
                                              :incomplete_details
                                              (:reason "max_output_tokens")))))
                       (funcall on-message
                                websocket
                                (json-encode
                                 '(:type "response.completed"
                                   :response (:id "resp-late"
                                              :status "completed")))))
                   (funcall on-message
                            websocket
                            (json-encode
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
      (e-openai-codex--websocket-session-open session url headers)
      (e-openai-codex--websocket-session-record-response
       session "resp-clean" '(:model "gpt-test"))
      (e-openai-codex--websocket-request-start
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
       (e-openai-codex--websocket-session-active-request session))
      (should (equal (plist-get (car sends) :previous_response_id) nil))
      (should (equal
               (e-openai-codex--websocket-session-latest-response-id session)
               "resp-clean"))
      ;; The next request starts directly after incomplete settlement and
      ;; reuses the preserved clean anchor without a manual cancellation.
      (let* ((second-request
              (e-openai-codex--websocket-request-start
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
                 (e-openai-codex--websocket-session-latest-response-id session)
                 "resp-followup")))
      (should-not
       (e-openai-codex--websocket-session-active-request session))
      (should (eq (e-openai-codex--websocket-session-websocket session)
                  'fake-websocket))
      (should (= scheduled-seconds 17))
      (should (= close-count 0)))))

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
                         'incremental)))
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
      (puthash "additionalProperties" :json-false second-parameters)
      (should-not (e-openai-codex--json-value-equal-p first second)))))

(ert-deftest e-openai-test-websocket-recorded-property-snapshot-detaches-strings ()
  "Mutable request-property strings cannot mutate an anchor snapshot."
  (let* ((e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-websocket-connection-idle-seconds nil)
         (model (copy-sequence "gpt-test"))
         (tool-name (copy-sequence "inspect"))
         (tools (vector (list :type "function" :name tool-name)))
         (session (e-openai-codex--websocket-session-create))
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
                        (json-parse-string text
                                           :object-type 'plist
                                           :array-type 'list
                                           :null-object nil
                                           :false-object :json-false)))
                   (setq sends (append sends (list payload)))
                   ;; Leave the first response in flight so the test can
                   ;; mutate both top-level and nested source strings before
                   ;; settlement.  Complete the second request immediately.
                   (when (= (length sends) 2)
                     (funcall on-message
                              websocket
                              (json-encode
                               '(:type "response.completed"
                                 :response (:id "resp-two"
                                            :status "completed"))))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (e-openai-codex--websocket-request-start
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
               (json-encode
                '(:type "response.completed"
                  :response (:id "resp-one" :status "completed"))))
      ;; A second mutation after completion must also leave the recorded
      ;; identity unchanged.
      (aset model 0 ?Y)
      (aset tool-name 0 ?l)
      (let ((recorded
             (e-openai-codex--websocket-session-latest-response-properties
              session)))
        (should (equal (plist-get recorded :model) "gpt-test"))
        (should (equal (plist-get (aref (plist-get recorded :tools) 0) :name)
                       "inspect")))
      (let* ((request
              (e-openai-codex--websocket-request-start
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
          (should (eq (plist-get diagnostics :websocket-request-mode) 'full)))
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
         error
         second-request
         third-request
         fourth-request)
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
                                             :status "completed")))))
                     (4
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-three"
                                             :status "completed")))))
                     (5
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-four"
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
        (let* ((chronological (reverse sends))
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
                      'full-retry)))
        (setq third-request
              (e-backend-start
               backend
               :messages '((:role user :content "one")
                           (:role user :content "two")
                           (:role user :content "three"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-one"))
                 :provider-anchor-delta-messages
                 ((:role user :content "three"))
                 :provider-anchor-source-message-count 2)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error (lambda (err) (setq error err))))
        (should (e-openai-test--wait-until
                 (lambda () (or error (= done-count 3)))
                 0.2))
        (should-not error)
        (setq fourth-request
              (e-backend-start
               backend
               :messages '((:role user :content "one")
                           (:role user :content "two")
                           (:role user :content "three")
                           (:role user :content "four"))
               :options
               '(:model "gpt-test"
                 :session-id "session-one"
                 :provider-continuation t
                 :provider-anchor
                 (:provider-id openai
                  :metadata (:response-id "resp-two"))
                 :provider-anchor-delta-messages
                 ((:role user :content "three")
                  (:role user :content "four"))
                 :provider-anchor-source-message-count 3)
               :on-item #'ignore
               :on-done (lambda (_status) (cl-incf done-count))
               :on-error #'signal))
        (should (e-openai-test--wait-until (lambda () (= done-count 4)) 0.2))
        (let* ((chronological (nreverse sends))
               (removed-anchor-request (nth 3 chronological))
               (stale-anchor-request (nth 4 chronological))
               (removed-diagnostics
                (plist-get (e-backend-request-metadata third-request)
                           :diagnostics))
               (stale-diagnostics
                (plist-get (e-backend-request-metadata fourth-request)
                           :diagnostics)))
          (should-not (plist-member removed-anchor-request
                                      :previous_response_id))
          (should (eq (plist-get removed-diagnostics :websocket-request-mode)
                      'full))
          ;; Only the latest response (resp-three) is eligible.  The older
          ;; resp-two anchor is intentionally sent as a full request.
          (should-not (plist-member stale-anchor-request
                                      :previous_response_id))
          (should (eq (plist-get stale-diagnostics
                                 :websocket-request-mode)
                      'full)))
        (e-backend-cancel-request fourth-request)))))

(ert-deftest e-openai-test-websocket-unresolved-anchor-retries-one-canonical-bundle-for-both-error-forms ()
  "Both unavailable-response error forms get one complete canonical retry.

The second request is the matching immediate continuation and therefore carries
only its function-call output.  The adapter's one retry must remove the
unavailable response id and reconstruct the call, opaque reasoning replay, and
result from the canonical messages supplied by the caller."
  (dolist (error-form '("error" "response.failed"))
    (let* ((process-environment
            (cons "OPENAI_API_KEY=test-gateway-token" process-environment))
           (e-openai-model-providers
            '((openai-websocket
               :name "OpenAI WebSocket"
               :base-url "https://gateway.example.test/v1"
               :env-key "OPENAI_API_KEY"
               :wire-api responses
               :responses-transport websocket
               :continuation t
               :requires-openai-auth nil)))
           (sends nil)
           (on-message nil)
           (done-count 0)
           (failure nil)
           recovery-request)
      (cl-letf (((symbol-function 'websocket-open)
                 (lambda (_url &rest args)
                   (setq on-message (plist-get args :on-message))
                   'fake-websocket))
                ((symbol-function 'websocket-send-text)
                 (lambda (websocket text)
                   (let ((payload
                          (json-parse-string
                           text
                           :object-type 'plist
                           :array-type 'list
                           :null-object nil
                           :false-object :json-false)))
                     (push payload sends)
                     (pcase (length sends)
                       (1
                        (funcall on-message
                                 websocket
                                 (json-encode
                                  '(:type "response.completed"
                                    :response (:id "resp-latest"
                                               :status "completed")))))
                       (2
                        (funcall on-message
                                 websocket
                                 (json-encode
                                  (if (equal error-form "error")
                                      '(:type "error"
                                        :code "previous_response_not_found"
                                        :param "previous_response_id"
                                        :message "previous response not found")
                                    '(:type "response.failed"
                                      :response
                                      (:error
                                       (:code "previous_response_not_found"
                                        :param "previous_response_id"
                                        :message "previous response not found")))))))
                       (3
                        (funcall on-message
                                 websocket
                                 (json-encode
                                  '(:type "response.completed"
                                    :response (:id "resp-recovered"
                                               :status "completed")))))
                       (_ (error "Unexpected retry beyond one canonical recovery"))))))
                ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
        (let ((backend (e-openai-backend-create :provider 'openai-websocket)))
          ;; Prime the connection-local response state so the next request is
          ;; a real immediate continuation rather than a new-connection full
          ;; request.
          (e-backend-start
           backend
           :messages '((:role user :content "seed"))
           :options '(:model "gpt-test" :session-id "session-one")
           :on-item #'ignore
           :on-done (lambda (_status) (cl-incf done-count))
           :on-error (lambda (err) (setq failure err)))
          (should (e-openai-test--wait-until
                   (lambda () (or failure (= done-count 1)))
                   0.2))
          (should-not failure)
          (setq failure nil)
          (setq recovery-request
                (e-backend-start
                 backend
                 :messages
                 '((:role user :content "request before tool")
                     (:role tool-call
                      :content (:id "call-latest"
                                :name "inspect"
                                :arguments (:target "raw")
                                :provider-replay-items
                                ((:provider-id openai
                                  :item (:type "reasoning"
                                         :id "replay-latest"
                                         :encrypted_content "RAW-REPLAY"
                                         :summary [])))))
                     (:role tool
                      :content (:tool-call-id "call-latest"
                                :name "inspect"
                                :content "RAW-RESULT")))
                 :options
                 '(:model "gpt-test"
                   :session-id "session-one"
                   :provider-continuation t
                   :provider-anchor
                   (:provider-id openai
                    :metadata (:response-id "resp-latest"))
                   :provider-anchor-delta-messages
                   ((:role tool
                     :content (:tool-call-id "call-latest"
                               :name "inspect"
                               :content "RAW-RESULT")))
                   :provider-anchor-source-message-count 3)
                 :on-item #'ignore
                 :on-done (lambda (_status) (cl-incf done-count))
                 :on-error (lambda (err) (setq failure err))))
          (should (e-openai-test--wait-until
                   (lambda () (or failure (= done-count 2)))
                   0.2))
          (let* ((chronological (reverse sends))
                 (incremental (nth 1 chronological))
                 (fallback (nth 2 chronological))
                 (fallback-input (plist-get fallback :input))
                 (replay
                  (seq-find
                   (lambda (item)
                     (equal (plist-get item :type) "reasoning"))
                   fallback-input))
                 (call
                  (seq-find
                   (lambda (item)
                     (and (equal (plist-get item :type) "function_call")
                          (equal (plist-get item :call_id) "call-latest")))
                   fallback-input))
                 (result
                  (seq-find
                   (lambda (item)
                     (and (equal (plist-get item :type)
                                 "function_call_output")
                          (equal (plist-get item :call_id) "call-latest")))
                   fallback-input))
                 (diagnostics
                  (plist-get (e-backend-request-metadata recovery-request)
                             :diagnostics)))
            ;; The initial continuation is exactly the matching output delta.
            (should (equal (plist-get incremental :previous_response_id)
                           "resp-latest"))
            (should (equal
                     (mapcar (lambda (item) (plist-get item :type))
                             (plist-get incremental :input))
                     '("function_call_output")))
            ;; The unavailable id is absent from the one complete canonical
            ;; retry, which carries every protocol item needed for replay.
            (should-not (plist-member fallback :previous_response_id))
            (should call)
            (should replay)
            (should result)
            (should (equal (plist-get replay :encrypted_content)
                           "RAW-REPLAY"))
            (should (equal (plist-get result :output) "RAW-RESULT"))
            (should (= (length sends) 3))
            (should-not failure)
            ;; Keep the one-retry assertion tied to the request that was
            ;; actually started; the separate request above must not affect
            ;; the completed recovery count.
            (should (= done-count 2))
            (should (eq (plist-get diagnostics :websocket-request-mode)
                        'full-retry))))))))

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

(ert-deftest e-openai-test-context-curation-is-a-reserved-backend-effect ()
  "The Responses adapter decodes context-curate without making a tool call."
  (let ((item
         (e-openai-codex--event-item
          '(:type "response.output_item.done"
            :item
            (:type "function_call"
             :call_id "curation-call"
             :name "context-curate"
             :arguments
             "{\"keep\":[1],\"summaries\":[{\"sources\":[2,3],\"text\":\"selected\"}]}")))))
    (should (eq (plist-get item :type) 'context-curate))
    (should-not (plist-member item :name))
    (should (equal (plist-get item :arguments)
                   '(:keep (1) :summaries
                           ((:sources (2 3) :text "selected")))))
    (should-not (string-match-p
                 "frame\|observation\|fingerprint\|schema-version"
                 (prin1-to-string (plist-get item :arguments))))))

(ert-deftest e-openai-test-context-curation-wire-is-opt-in ()
  "The curation carrier is present only in an opted-in Responses request."
  (let* ((messages '((:role user :content "prompt")))
         (enabled
          (e-openai-codex-request-body
           :messages messages
           :options
           `(:model "gpt-test"
             :context-lifetime-enabled t
             :reserved-effect-carrier context-curate-wire)
           :tools nil))
         (disabled
          (e-openai-codex-request-body
           :messages messages
           :options '(:model "gpt-test")
           :tools nil))
         (enabled-tools (plist-get enabled :tools)))
    (should (= (length enabled-tools) 1))
    (should (equal (plist-get (aref enabled-tools 0) :name)
                   "context-curate"))
    ;; The carrier is the exact optional keep/summaries shape.  Internal
    ;; frame/source identities do not belong in the provider schema.
    (let* ((parameters (plist-get (aref enabled-tools 0) :parameters))
           (properties (plist-get parameters :properties))
           (summary-schema (plist-get properties :summaries))
           (summary-properties
           (plist-get (plist-get summary-schema :items) :properties))
           (source-schema (plist-get summary-properties :sources))
           (text-schema (plist-get summary-properties :text)))
      (should (equal (sort (copy-sequence
                            (cl-loop for (key value) on properties by #'cddr
                                     collect key))
                           (lambda (left right)
                             (string< (symbol-name left)
                                      (symbol-name right))))
                     '(:keep :summaries)))
      (should-not (plist-member parameters :required))
      (should (equal (plist-get (plist-get properties :keep) :type)
                     "array"))
      (should (equal (plist-get (plist-get properties :summaries) :type)
                     "array"))
      (should (equal (plist-get source-schema :minItems) 1))
      (should (equal (plist-get source-schema :maxItems) 16))
      (should (equal (plist-get text-schema :minLength) 1))
      (should-not (plist-member properties :frame-id))
      (should-not (plist-member properties :source-observation-ids))
      (should-not (string-match-p
                   "frame-id\\|source-observation-ids\\|schema-version"
                   (json-encode enabled))))
    (should-not (plist-member disabled :tools))))

(ert-deftest e-openai-test-context-curation-carries-function-output-ack ()
  "A reserved Responses curation retains its opaque wire acknowledgement."
  (let* ((item
          (e-openai-codex--event-item
           '(:type "response.output_item.done"
             :item
             (:type "function_call"
              :call_id "curation-call"
              :name "context-curate"
              :arguments
              "{\"keep\":[1]}"))))
         (replay (plist-get item :provider-replay-item))
         (wire-item (plist-get replay :item)))
    (should (equal (plist-get replay :provider-id) 'openai))
    (should (equal (plist-get wire-item :type) "function_call_output"))
    (should (equal (plist-get wire-item :call_id) "curation-call"))
    (should (equal (plist-get wire-item :output) ""))))

(ert-deftest e-openai-test-context-curation-stream-carries-function-output-ack ()
  "The streaming Responses parser preserves the reserved call identity."
  (let* ((items
          (e-openai-codex-parse-stream
           "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"curation-call\",\"name\":\"context-curate\",\"arguments\":\"{\\\"keep\\\":[1]}\"}}\n\n"))
         (item (car items))
         (replay (plist-get item :provider-replay-item)))
    (should (eq (plist-get item :type) 'context-curate))
    (should (equal (plist-get (plist-get replay :item) :type)
                   "function_call_output"))
    (should (equal (plist-get (plist-get replay :item) :call_id)
                   "curation-call"))))

(ert-deftest e-openai-test-context-curation-request-emits-function-output ()
  "The immediate continuation carries the reserved call acknowledgement."
  (let* ((replay
          '(:type provider-replay-item
            :provider-id openai
            :item (:type "function_call_output"
                   :call_id "curation-call"
                   :output "")))
         (body
          (e-openai-codex-request-body
           :messages
           `((:role user :content "prompt")
             (:role assistant
              :content "selected"
              :metadata (:provider-replay-items (,replay))))
           :options
           `(:model "gpt-test"
             :provider-continuation t
             :response-store t
             :provider-anchor
             (:provider-id openai :metadata (:response-id "resp-promotion"))
             :provider-anchor-delta-messages
             ((:role assistant
               :content "selected"
               :metadata (:provider-replay-items (,replay)))))
           :tools nil))
         (input (append (plist-get body :input) nil))
         (ack (seq-find (lambda (item)
                          (equal (plist-get item :type)
                                 "function_call_output"))
                        input)))
    (should (equal (plist-get body :previous_response_id)
                   "resp-promotion"))
    (should ack)
    (should (equal (plist-get ack :call_id) "curation-call"))
    (should (equal (plist-get ack :output) ""))
    (should-not (seq-find (lambda (item)
                            (equal (plist-get item :name)
                                   "context-curate"))
                          input))))

(ert-deftest e-openai-test-context-curation-full-replay-pair-is-not-anchored-call ()
  "Full replay restores the curation call/output pair; anchors send output only."
  (let* ((effect (e-openai-codex--context-curation-effect
                  '(:keep (1)) "curation-call"))
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
             (:provider-id openai :metadata (:response-id "resp-curation"))
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
                                   :arguments "{\"keep\":[1]}"))
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

(ert-deftest e-openai-test-loop-late-curation-ack-joins-tool-followup ()
  "A late reserved curation ack joins the ordinary tool result on the wire."
  (let* ((request-count 0)
         (requests nil)
         (durable-messages nil)
         (started-tools nil)
         (first-items nil)
         (curation-arguments
          (json-encode
           '(:keep [1])))
         (first-response
          (mapconcat
           (lambda (event)
             (format "data: %s\n\n" (json-encode event)))
           (list
            (list :type "response.output_item.done"
                  :item (list :type "function_call"
                              :call_id "call-ordinary"
                              :name "inspect"
                              :arguments (json-encode
                                          '(:target "state"))))
            (list :type "response.output_item.done"
                  :item (list :type "function_call"
                              :call_id "curation-call"
                              :name "context-curate"
                              :arguments curation-arguments))
            (list :type "response.completed"
                  :response (list :id "resp-A" :status "completed")))
           ""))
         (second-response
          (mapconcat
           (lambda (event)
             (format "data: %s\n\n" (json-encode event)))
           (list
            (list :type "response.output_text.done" :text "follow-up")
            (list :type "response.completed"
                  :response (list :id "resp-B" :status "completed")))
           ""))
         (backend
          (e-backend-create
           :name "openai-late-promotion-ack"
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (push (list :messages (copy-tree messages)
                          :options (copy-tree options))
                    requests)
              (cl-incf request-count)
              (let ((items
                     (e-openai-codex-parse-stream
                      (if (= request-count 1)
                          first-response
                        second-response))))
                (when (= request-count 1)
                  (setq first-items (copy-tree items)))
                (dolist (item items)
                  (funcall on-item item)))))))
         (tool-lifecycle
          (e-tool-lifecycle-create
           :start
           (cl-function
            (lambda (tool-call &key on-done &allow-other-keys)
              (push (plist-get tool-call :name) started-tools)
              (funcall on-done
                       (list :tool-call-id (plist-get tool-call :id)
                             :name (plist-get tool-call :name)
                             :status 'ok
                             :content "ordinary-result"))
              nil))))
         (frame
          (e-context-lifetime-frame-create
           :id "frame-openai-late-ack"
           :generation-id "generation-openai-late-ack"
           :consumer-request-id "consumer-openai-late-ack"
           :observations
           '((:observation-id "observation-openai-late-ack"
              :kind "current-state"
              :source-entry-ref "external:canvas:openai-late-ack"
              :source-fingerprint "canvas-openai-late-ack"
              :effective-delivery "request-local-replaceable"
              :body (:content "canvas")))))
         (options
          '(:model "gpt-test"
            :provider-continuation t
            :provider-anchor-provider-id openai
            :context-lifetime-enabled t
            :context-capabilities
            (:continuation linear
             :observation-delivery request-local-replaceable
             :reserved-effect-carrier context-curate-wire))))
    (e-loop-run-turn-batch
     :session-id "session-openai-late-ack"
     :turn-id "turn-openai-late-ack"
     :messages '((:role user :content "inspect state"))
     :backend backend
     :tool-lifecycle tool-lifecycle
     :options options
     :lifetime-frame frame
     :on-event #'ignore
     :append-message
     (lambda (message)
       ;; This callback represents the durable transcript boundary.  The
       ;; loop must not retrofit the provider-only acknowledgement into it.
       (setq durable-messages
             (append durable-messages (list (copy-tree message))))))
    (let* ((first-types (mapcar (lambda (item) (plist-get item :type))
                                first-items))
           (requests (nreverse requests))
           (follow-up (nth 1 requests))
           (body (e-openai-codex-request-body
                  :messages (plist-get follow-up :messages)
                  :options (plist-get follow-up :options)
                  :tools nil))
           (input (append (plist-get body :input) nil))
           (outputs
            (seq-filter (lambda (item)
                          (equal (plist-get item :type)
                                 "function_call_output"))
                        input))
           (ordinary-output-position
            (cl-position-if
             (lambda (item)
               (and (equal (plist-get item :type) "function_call_output")
                    (equal (plist-get item :call_id) "call-ordinary")))
             input))
           (curation-output-position
            (cl-position-if
             (lambda (item)
               (and (equal (plist-get item :type) "function_call_output")
                    (equal (plist-get item :call_id) "curation-call")))
             input)))
      ;; The actual adapter stream order is ordinary call, reserved control,
      ;; then completion; the reserved control never enters ordinary tools.
      (should (< (cl-position 'tool-call first-types)
                 (cl-position 'context-curate first-types)))
      (should (member 'done first-types))
      (should (equal started-tools '("inspect")))
      (should (= (length outputs) 2))
      (should (seq-find (lambda (item)
                          (and (equal (plist-get item :call_id)
                                      "call-ordinary")
                               (equal (plist-get item :output)
                                      "ordinary-result")))
                        outputs))
      (should (seq-find (lambda (item)
                          (and (equal (plist-get item :call_id)
                                      "curation-call")
                               (equal (plist-get item :output) "")))
                        outputs))
      (should (integerp ordinary-output-position))
      (should (integerp curation-output-position))
      (should (< ordinary-output-position curation-output-position))
      (should (equal (plist-get body :previous_response_id) "resp-A"))
      (should-not (string-match-p "provider-replay-item"
                                  (prin1-to-string durable-messages)))
      (should-not (string-match-p "curation-call"
                                  (prin1-to-string durable-messages))))))

(ert-deftest e-openai-test-provider-compaction-capability-follows-effective-profile ()
  "Only the proven first-party compact endpoint advertises opaque compaction."
  (let ((e-openai-model-providers
         (append e-openai-model-providers
                 `((custom-proven
            :name "Proven custom"
            :base-url ,e-openai-api-default-base-url
            :wire-api responses
            :responses-transport http
            :continuation t
            :provider-compaction opaque
            :requires-openai-auth nil
            :env-key "OPENAI_API_KEY")))))
    (should (eq (plist-get
                 (e-backend-context-capabilities
                  (e-openai-backend-create :provider 'openai)
                  nil)
                 :provider-compaction)
                'opaque))
    (should (eq (plist-get
                 (e-backend-context-capabilities
                  (e-openai-backend-create :provider 'codex
                                           :compaction-request-function #'ignore)
                  nil)
                 :provider-compaction)
                'none))
    (should (eq (plist-get
                 (e-backend-context-capabilities
                  (e-openai-backend-create
                   :provider 'openai
                   :base-url "https://gateway.example.test/v1"
                   :compaction-request-function #'ignore)
                  nil)
                 :provider-compaction)
                'none))
    (should (eq (plist-get
                 (e-backend-context-capabilities
                  (e-openai-backend-create
                   :provider 'openai
                   :request-function #'ignore)
                  nil)
                 :provider-compaction)
                'none))
    (should (eq (plist-get
                 (e-backend-context-capabilities
                  (e-openai-backend-create
                   :provider 'custom-proven
                   :compaction-request-function #'ignore)
                  nil)
                 :provider-compaction)
                'opaque))))

(ert-deftest e-openai-test-provider-compaction-batch-uses-public-http-shape ()
  "The compact adapter sends only model and portable input and detaches output."
  (let* ((captured nil)
         (process-environment
          (cons "OPENAI_API_KEY=test-api-token" process-environment))
         (backend
          (e-openai-backend-create
           :provider 'openai
           :compaction-request-function
           (cl-function
            (lambda (&key url headers body)
              (setq captured (list :url url :headers headers :body body))
              "{\"object\":\"response.compaction\",\"output\":[{\"type\":\"encrypted\",\"payload\":\"opaque\"}],\"usage\":{\"input_tokens\":2,\"total_tokens\":3}}"))))
         (messages
          '((:role user
             :content "durable intent"
             :metadata (:provider-anchor "ANCHOR" :provider-replay-items ("RAW-E")))
            (:role assistant :content "durable answer")))
         (result
          (e-backend-provider-compaction-batch
           backend
           :messages messages
           :options '(:model "gpt-5.6" :session-id "compact-session")))
         (body
          (json-parse-string
           (plist-get captured :body)
           :object-type 'plist
           :array-type 'list
           :null-object nil
           :false-object :json-false)))
    (should (equal (plist-get captured :url)
                   "https://api.openai.com/v1/responses/compact"))
    (should (equal (cdr (assoc "Authorization" (plist-get captured :headers)))
                   "Bearer test-api-token"))
    (should (equal (plist-get body :model) "gpt-5.6"))
    (should (listp (plist-get body :input)))
    (should-not (plist-member body :previous_response_id))
    (should-not (string-match-p "RAW-E\|ANCHOR\|provider-replay"
                                (plist-get captured :body)))
    (should (equal (e-backend-provider-compaction-result-output result)
                   '((:type "encrypted" :payload "opaque"))))
    (should (equal (e-backend-provider-compaction-result-usage result)
                   '(:input-tokens 2 :total-tokens 3)))))

(ert-deftest e-openai-test-provider-compaction-rejects-malformed-output ()
  "Malformed compact responses fail at the adapter boundary."
  (dolist (response
           '("{\"object\":\"response\",\"output\":[]}"
             "{\"object\":\"response.compaction\",\"output\":null}"
             "{\"object\":\"response.compaction\",\"output\":{}}"))
    (should-error
     (e-openai--provider-compaction-decode response)
     :type 'e-openai-provider-invalid)))

(ert-deftest e-openai-test-provider-compaction-output-starts-a-fresh-chain ()
  "Opaque compact output is followed by only the post-coverage delta."
  (let* ((output '((:type "encrypted" :payload "opaque")))
         (delta '((:role user :content "after coverage")))
         (options `(:model "gpt-5.6"
                    :provider-continuation t
                    :provider-anchor
                    (:provider-id openai
                     :metadata (:response-id "old-response"))
                    :provider-compaction-output ,output
                    :provider-compaction-delta-messages ,delta))
         (http-body (e-openai-codex-request-body
                     :messages '((:role user :content "full portable context"))
                     :options options
                     :tools nil))
         (websocket-body
          (e-openai-codex-request-body
           :messages '((:role user :content "full portable context"))
           :options (plist-put (copy-sequence options)
                               :responses-transport 'websocket)
           :tools nil))
         (second-options
          '(:model "gpt-5.6"
            :provider-continuation t
            :response-store t
            :provider-anchor
            (:provider-id openai
             :metadata (:response-id "fresh-anchor"))))
         (second-http-body
          (e-openai-codex-request-body
           :messages delta :options second-options :tools nil))
         (second-websocket-body
          (e-openai-codex-request-body
           :messages delta
           :options (plist-put (copy-sequence second-options)
                               :responses-transport 'websocket)
           :tools nil)))
    (dolist (body (list http-body websocket-body))
      (should-not (plist-member body :previous_response_id))
      (should (equal (aref (plist-get body :input) 0)
                     (car output)))
      (should (equal (plist-get (aref (plist-get body :input) 1) :role)
                     "user"))
      (should (equal (plist-get
                      (aref (plist-get body :input) 1)
                     :content)
                     [(:type "input_text" :text "after coverage")])))
    (dolist (body (list second-http-body second-websocket-body))
      (should (equal (plist-get body :previous_response_id)
                     "fresh-anchor"))
      (should-not (plist-member body :provider-compaction-output)))))

(ert-deftest e-openai-test-provider-compaction-keeps-stable-prefix-by-layout ()
  "Opaque history replacement keeps stable/current semantic context once.

Flattened Responses layouts carry the trusted stable prefix in instructions;
segmented layouts carry it as fresh developer input while current state stays
in instructions.  Neither path copies the covered checkpoint outside opaque
provider output."
  (let* ((output '((:type "encrypted" :marker "C1-COVERED")))
         (delta '((:role user :content "post-coverage")))
         (messages '((:role system :content "STATIC-STABLE-POLICY")
                     (:role system :content "STABLE-CONTEXT")
                     (:role system :content "CURRENT-STATE")
                     (:role system :content "C1-COVERED")))
         (segments '((:kind static-prefix
                      :messages ((:role system :content "STATIC-STABLE-POLICY")))
                    (:kind stable-context
                     :messages ((:role system :content "STABLE-CONTEXT")))
                    (:kind current-state
                     :messages ((:role system :content "CURRENT-STATE")))
                    (:kind history
                     :messages ((:role system :content "C1-COVERED")))))
         (common `(:instructions "BASE-POLICY"
                   :observation-delivery request-local-replaceable
                   :context-capabilities
                   (:observation-delivery request-local-replaceable)
                   :replaceable-current-state
                   ((:role system :content "CURRENT-STATE"))
                   :context-segment-message-count 4
                   :segments ,segments
                   :provider-compaction-output ,output
                   :provider-compaction-delta-messages ,delta))
         (flattened
          (e-openai-codex-request-body
           :messages messages
           :options (append '(:model "gpt-5.5") common)))
         (segmented
          (e-openai-codex-request-body
           :messages messages
           :options (append
                     '(:model "gpt-5.6-sol"
                       :prompt-cache-key "stable-key"
                       :prompt-cache-breakpoint-mode explicit
                       :responses-context-layout developer-input)
                     common)))
         (flattened-input (append (plist-get flattened :input) nil))
         (segmented-input (append (plist-get segmented :input) nil)))
    (should (equal (plist-get flattened :instructions)
                   "BASE-POLICY\n\nSTATIC-STABLE-POLICY\n\nSTABLE-CONTEXT\n\nCURRENT-STATE"))
    (should-not (string-match-p "C1-COVERED"
                                (plist-get flattened :instructions)))
    (should (equal (car flattened-input) (car output)))
    (should (string-match-p "post-coverage"
                            (prin1-to-string (cdr flattened-input))))
    (should (equal (plist-get segmented :instructions)
                   "BASE-POLICY\n\nCURRENT-STATE"))
    (should-not (string-match-p "C1-COVERED"
                                (plist-get segmented :instructions)))
    (should (equal (car segmented-input) (car output)))
    (should (string-match-p "STATIC-STABLE-POLICY"
                            (prin1-to-string (car (cdr segmented-input)))))
    (should (string-match-p "STABLE-CONTEXT"
                            (prin1-to-string (car (cddr segmented-input)))))
    (should (string-match-p "post-coverage"
                            (prin1-to-string (cadddr segmented-input))))))

(ert-deftest e-openai-test-provider-compaction-async-failure-is-optional ()
  "An optional async compact failure reports once without a usable result."
  (let ((process-environment
         (cons "OPENAI_API_KEY=test-api-token" process-environment))
        (done nil)
        (errors nil)
        (backend
         (e-openai-backend-create
          :provider 'openai
          :compaction-request-function
          (lambda (&rest _args)
            "{\"object\":\"response.compaction\",\"output\":null}"))))
    (e-backend-provider-compaction-start
     backend
     :messages '((:role user :content "portable"))
     :options '(:model "gpt-5.6")
     :on-done (lambda (result) (setq done result))
     :on-error (lambda (error-value) (push error-value errors)))
    (should (e-openai-test--wait-until (lambda () errors)))
    (should-not done)
    (should (= (length errors) 1))))

(provide 'e-openai-test)

;;; e-openai-test.el ends here

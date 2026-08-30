;;; e-openai-request-composition-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI profile and request composition.

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

(load (expand-file-name "e-openai-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

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
      :reasoning (:effort "high" :summary "auto")))))

(ert-deftest e-openai-test-request-body-retains-invocation-envelope-schema ()
  "Responses carries the decorated operation schema without reinterpretation."
  (let* ((tool
          '(:type "function"
            :name "read"
            :description "Read a URI."
            :parameters (:type "object"
                          :properties (:uri (:type "string")
                                        :stated_purpose
                                        (:type "string" :minLength 1 :maxLength 200))
                          :required ["uri" "stated_purpose"]
                          :additionalProperties :json-false)
            :strict :json-false))
         (body (e-openai-codex-request-body
                :messages '((:role user :content "hello"))
                :options '(:model "gpt-test")
                :tools (list tool)))
         (wire-tool (aref (plist-get body :tools) 0))
         (round-trip
          (json-parse-string
           (json-encode body)
           :object-type 'plist
           :array-type 'list
           :null-object nil
           :false-object :json-false))
         (round-trip-tool (car (plist-get round-trip :tools))))
    (should (equal (plist-get (plist-get wire-tool :parameters) :required)
                   ["uri" "stated_purpose"]))
    (should (eq (plist-get (plist-get wire-tool :parameters)
                           :additionalProperties)
                :json-false))
    (should (equal (plist-get (plist-get round-trip-tool :parameters)
                             :required)
                   '("uri" "stated_purpose")))
    (should (eq (plist-get (plist-get round-trip-tool :parameters)
                           :additionalProperties)
                :json-false))))

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
      :reasoning (:effort "high" :summary "auto")))))

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
      :reasoning (:effort "high" :summary "auto")))))

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
      :reasoning (:effort "low" :summary "auto")))))

(ert-deftest e-openai-test-reasoning-summary-precedence-and-explicit-join ()
  "Request, profile, and adapter summary choices compose predictably."
  (let ((profile-options
         (e-openai-profile-harness-default-options
          '(:wire-api responses :reasoning-summary "detailed")
          "gpt-test")))
    (should (equal (plist-get profile-options :reasoning-summary)
                   "detailed"))
    (should (equal (plist-get
                    (e-openai-codex-request-body
                     :messages '((:role user :content "hello"))
                     :options '(:model "gpt-test"
                                :reasoning-effort "low"
                                :reasoning-summary "auto"
                                :reasoning (:effort "minimal"
                                            :extra "preserved")))
                   :reasoning)
                   '(:effort "minimal" :extra "preserved" :summary "auto")))
    (should (equal (plist-get
                    (e-openai-codex-request-body
                     :messages '((:role user :content "hello"))
                     :options '(:model "gpt-test"
                                :reasoning-summary "auto"
                                :reasoning (:summary "detailed")))
                   :reasoning)
                   '(:effort "high" :summary "detailed")))
    ;; A shadowed lower-precedence value is not selected or validated.
    (should (equal (plist-get
                    (e-openai-codex-request-body
                     :messages '((:role user :content "hello"))
                     :options '(:model "gpt-test"
                                :reasoning-summary "invalid"
                                :reasoning (:summary "detailed")))
                   :reasoning)
                   '(:effort "high" :summary "detailed")))))

(ert-deftest e-openai-test-reasoning-summary-rejects-disable-and-unknown-values ()
  "Responses cannot omit, disable, or misspell the summary choice."
  (dolist (options '((:reasoning-summary nil)
                    (:reasoning-summary :json-false)
                    (:reasoning-summary "verbose")
                    (:reasoning nil)
                    (:reasoning :json-false)
                    (:reasoning (effort "high"))
                    (:reasoning (:effort . "high"))))
    (should-error
     (e-openai-codex-request-body
      :messages '((:role user :content "hello"))
      :options options)
     :type 'e-openai-provider-invalid)))

(ert-deftest e-openai-test-reasoning-summary-does-not-change-chat-completions ()
  "Chat Completions request bodies do not gain Responses reasoning fields."
  (let ((body
         (e-openai-chat-completion-request-body
          :messages '((:role user :content "hello"))
          :options '(:model "chat-test" :reasoning-summary "detailed"))))
    (should-not (plist-member body :reasoning))
    (should-not (plist-member body :reasoning-summary))))

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
      :reasoning (:effort "high" :summary "auto")
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
             :messages ((:role system :content "[ephemeral context source 1, ~2 tokens]")
                        (:role system :content ,source-one)
                        (:role system :content "[ephemeral context source 2, ~3 tokens]")
                        (:role system :content "SOURCE-TWO")))))
         (segments-two
          `((:kind static-prefix
             :messages ((:role system :content "STATIC-POLICY")))
            (:kind stable-context
             :messages ((:role system :content "STABLE-GUIDANCE")))
            (:kind history
             :messages ((:role user :content "durable prompt")))
            (:kind dynamic-context
             :messages ((:role system :content "[ephemeral context source 1, ~2 tokens]")
                        (:role system :content ,source-two)
                        (:role system :content "[ephemeral context source 2, ~3 tokens]")
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
           (list (list :role 'system :content "[ephemeral context source 1, ~2 tokens]")
                 (list :role 'system :content source-two)
                 (list :role 'system :content "[ephemeral context source 2, ~3 tokens]")
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
                   (list "[ephemeral context source 1, ~2 tokens]"
                         (json-encode source-one)
                         "[ephemeral context source 2, ~3 tokens]"
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
                         :prompt-layout-revision ,revision
                         :reasoning-identity
                         (:effort "high" :summary "auto")))
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
                         ,e-openai-gpt56-explicit-cache-layout-revision
                         :reasoning-identity
                         (:effort "high" :summary "auto")))
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
                                          "responses-explicit-cache-v1"
                                          :reasoning-identity
                                          (:effort "high" :summary "auto")))
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
         (e-openai-profile-context-capabilities
          '(:wire-api responses
            :continuation t
            :observation-delivery request-local-replaceable
            :prompt-cache-breakpoint-mode explicit)
          nil))
        (inherited
         (e-openai-profile-context-capabilities
          '(:wire-api responses
            :continuation t
            :prompt-cache-breakpoint-mode explicit)
          nil))
        (chat
         (e-openai-profile-context-capabilities
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
   (e-openai-profile-context-capabilities
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
          (e-openai-profile-context-capabilities
           openai-profile nil :provider 'openai :request-function nil))
         (canonical-codex
          (e-openai-profile-context-capabilities
           codex-profile nil :provider 'codex :request-function nil))
         (openai-endpoint-override
          (e-openai-profile-context-capabilities
           openai-profile nil
           :provider 'openai
           :base-url "https://gateway.example.test/v1"))
         (codex-endpoint-override
          (e-openai-profile-context-capabilities
           codex-profile nil
           :provider 'codex
           :base-url "https://gateway.example.test/codex"))
         (openai-request-override
          (e-openai-profile-context-capabilities
           openai-profile nil :provider 'openai :request-function #'ignore))
         (codex-request-override
          (e-openai-profile-context-capabilities
           codex-profile nil :provider 'codex :request-function #'ignore))
         (openai-transport-override
          (e-openai-profile-context-capabilities
           openai-profile '(:responses-transport http) :provider 'openai))
         (codex-transport-override
          (e-openai-profile-context-capabilities
           codex-profile '(:responses-transport http) :provider 'codex))
         (noncanonical-codex
          (e-openai-profile-context-capabilities
           (plist-put (copy-sequence codex-profile)
                      :name "Custom Codex")
           nil :provider 'codex))
         (custom-profile
          (plist-put
           (plist-put (copy-sequence openai-profile)
                      :base-url "https://custom.example.test/v1")
           :observation-delivery 'request-local-replaceable))
         (custom-proof (e-openai-profile-context-capabilities
                        custom-profile nil
                        :provider 'custom-proven
                        :request-function #'ignore))
         (custom-without-proof
          (e-openai-profile-context-capabilities
           (let ((copy (copy-sequence custom-profile)))
             (cl-remf copy :observation-delivery)
             copy)
           nil
           :provider 'custom-unproven
           :request-function #'ignore))
         (custom-endpoint-override
          (e-openai-profile-context-capabilities
           custom-profile nil
           :provider 'custom-proven
           :base-url "https://other.example.test/v1"
           :request-function #'ignore))
         (custom-transport-override
          (e-openai-profile-context-capabilities
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
    ;; injected requester used by its conformance test.  Assert the stable
    ;; kind-scoped semantic projection, not the profile owner's variable.
    (should
     (equal
      (plist-get custom-proof :observation-delivery)
      '((:kind current-state :mode request-local-replaceable)
        (:kind dynamic-context :mode request-local-replaceable)
        (:kind tool-result :mode inherited)
        (:kind trace :mode inherited)
        (:kind retrieved-excerpt :mode inherited))))
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

(provide 'e-openai-request-composition-test)

;;; e-openai-request-composition-test.el ends here

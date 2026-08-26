;;; e-provider-continuation-integration-test.el --- Provider continuation integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic integration tests for provider continuation across the harness,
;; OpenAI adapter, tool execution, and follow-up request construction.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-capabilities)
(require 'e-context)
(require 'e-harness)
(require 'e-openai)
(require 'e-session)
(require 'e-tools)
(load (expand-file-name
       "../e2e/e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(defun e-provider-continuation-integration--sse (&rest events)
  "Return an SSE stream containing JSON EVENTS."
  (mapconcat
   (lambda (event)
     (format "data: %s\n\n" (json-encode event)))
   events
   ""))

(defun e-provider-continuation-integration--input-types (body)
  "Return the Responses input item types from JSON BODY."
  (mapcar (lambda (item) (alist-get 'type item))
          (alist-get 'input body)))

(defun e-provider-continuation-integration--first-input-text (body)
  "Return the first input message text from JSON BODY."
  (alist-get
   'text
   (aref (alist-get 'content (aref (alist-get 'input body) 0)) 0)))

(defun e-provider-continuation-integration--jwt ()
  "Return a fake JWT carrying the ChatGPT account claim used by Codex auth."
  (let* ((payload (json-encode
                   '(:https://api.openai.com/auth
                     (:chatgpt_account_id "acct-test"))))
         (encoded (base64-encode-string payload 'no-line-break)))
    (setq encoded (string-replace "+" "-" encoded))
    (setq encoded (string-replace "/" "_" encoded))
    (setq encoded (replace-regexp-in-string "=+$" "" encoded))
    (format "header.%s.signature" encoded)))

(defun e-provider-continuation-integration--run-ephemeral-anchor-profile
    (continuation observation-delivery)
  "Run a normal tool follow-up for semantic CONTINUATION and DELIVERY.
Return captured request options/messages and persisted anchor ids.  This fake
backend is intentionally provider-neutral; OpenAI wire acknowledgement is
covered by the adapter tests below."
  (let* ((request-count 0)
         (requests nil)
         (current-state nil)
         (harness-ref nil)
         (promotion-input nil)
         (promotions-at-later-request nil)
         (replay-marker "REPLAY-EPHEMERAL")
         (raw-result
          (format "UNIQUE-%s-TOOL-RESULT"
                  (symbol-name observation-delivery)))
         (dynamic-provider
          (e-context-provider-create
           :name (intern (format "ephemeral-%s-state" observation-delivery))
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (when current-state
                      (list (list :role 'system :content current-state))))))
         (backend
          (e-backend-create
           :name (format "ephemeral-anchor-%s" continuation)
           :context-capabilities
           (list :continuation continuation
                 :observation-delivery observation-delivery)
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (push (list :messages (copy-tree messages)
                          :options (copy-tree options))
                    requests)
              (cl-incf request-count)
              (pcase request-count
                (1
                 (funcall on-item
                          '(:type assistant-message :content "seed"))
                 (funcall on-item
                          '(:type provider-anchor-candidate
                            :provider-id fake
                            :metadata (:response-id "resp-clean")))
                 (funcall on-item '(:type done :reason stop)))
                (2
                 ;; Response A only emits a tool call.  Its candidate is
                 ;; usable for the one matching result follow-up, but is not a
                 ;; durable anchor owner.
                 ;; The replay item is paired with the call before the tool
                 ;; result is produced; it is part of the same ephemeral
                 ;; bundle, not durable semantic context.
                 (funcall on-item
                          (list :type 'provider-replay-item
                                :provider-id 'fake
                                :item (list :type "fake-replay"
                                             :id replay-marker)))
                 (funcall on-item
                          '(:type tool-call
                            :id "call-ephemeral"
                            :name "inspect-ephemeral"
                            :arguments (:target "raw")))
                 (funcall on-item
                          '(:type provider-anchor-candidate
                            :provider-id fake
                            :metadata (:response-id "resp-A")))
                 (funcall on-item '(:type done :reason tool-use)))
                (3
                 ;; B is the sole consumer of the paired call/result bundle.
                 (should (plist-get options :context-promotion-frame-id))
                 (let* ((observation-ids
                         (plist-get options
                                    :context-promotion-observation-ids))
                        (tool-observation-id
                         (seq-find
                          (lambda (observation-id)
                            (string-prefix-p "observation:tool-bundle:"
                                             observation-id))
                          observation-ids)))
                   (should tool-observation-id)
                   (setq promotion-input
                         (list
                          :type 'context-promote
                          :schema-version 1
                          :frame-id
                          (plist-get options :context-promotion-frame-id)
                          :source-observation-ids (list tool-observation-id)
                          :facts
                          '((:id "selected-tool-fact"
                             :value "selected from tool result"))))
                   (funcall on-item promotion-input))
                 (funcall on-item
                          '(:type assistant-message :content "B"))
                 (funcall on-item
                          '(:type provider-anchor-candidate
                            :provider-id fake
                            :metadata (:response-id "resp-B")))
                 (funcall on-item '(:type done :reason stop)))
                (4
                 ;; The durable promotion must already be committed before
                 ;; this real later provider request is admitted.
                 (setq promotions-at-later-request
                       (copy-tree
                        (mapcar
                         #'e-session--context-record
                         (e-session-context-promotions
                          (e-harness-sessions harness-ref)
                          "ephemeral-anchor-session"))))
                 (funcall on-item
                          '(:type assistant-message :content "later"))
                 (funcall on-item
                          '(:type provider-anchor-candidate
                            :provider-id fake
                            :metadata (:response-id "resp-later")))
                 (funcall on-item '(:type done :reason stop))))))))
         (capability
          (e-capability-create
           :id (intern (format "ephemeral-anchor-%s" continuation))
           :context-providers (list dynamic-provider)
           :tools
           (list
            (lambda (registry)
              (e-tools-register
               registry
               :name "inspect-ephemeral"
               :description "Return one uniquely identifiable ephemeral result."
               :work
               (e-tools-cheap-work
                "e2e.ephemeral-anchor.inspect"
                (lambda (_arguments) raw-result)))))))
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list capability)
           :default-options
           (list :model "fake"
                 :provider-continuation t
                 :provider-anchor-provider-id 'fake))))
    (setq harness-ref harness)
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-board-e2e-create-session harness :id "ephemeral-anchor-session")
      (e-board-e2e-prompt-batch
       harness "ephemeral-anchor-session" "seed prompt")
      (setq current-state "STATE-ONE")
      (e-board-e2e-prompt-batch
       harness "ephemeral-anchor-session" "inspect prompt")
      (setq current-state "STATE-TWO")
      (e-board-e2e-prompt-batch
       harness "ephemeral-anchor-session" "later prompt"))
    (list :requests (nreverse requests)
          :anchors
          (mapcar (lambda (anchor)
                    (plist-get (plist-get anchor :metadata) :response-id))
                  (e-session-provider-anchors
                   (e-harness-sessions harness)
                   "ephemeral-anchor-session"))
          :promotion-input promotion-input
          :promotions-at-later-request promotions-at-later-request
          :replay-marker replay-marker
          :raw-result raw-result)))

(ert-deftest e-provider-continuation-integration-test-ephemeral-bundle-does-not-contaminate-anchor ()
  "A tool bundle uses A once, and B cannot replace the prior clean anchor."
  (dolist (profile '((linear inherited)
                     (branchable inherited)
                     (linear request-local-replaceable)))
    (let* ((continuation (nth 0 profile))
           (delivery (nth 1 profile))
           (result
            (e-provider-continuation-integration--run-ephemeral-anchor-profile
             continuation delivery))
           (requests (plist-get result :requests))
           (follow-up (nth 2 requests))
           (later (nth 3 requests))
           (follow-up-options (plist-get follow-up :options))
           (later-options (plist-get later :options))
           (follow-up-messages (plist-get follow-up :messages))
           (later-messages (plist-get later :messages))
           (follow-up-printed (prin1-to-string follow-up-messages))
           (later-printed (prin1-to-string later-messages))
           (promotion-input (plist-get result :promotion-input))
           (promotions-at-later-request
            (plist-get result :promotions-at-later-request)))
      (should (= (length requests) 4))
      ;; A is allowed only at the frontier that carries the matching result.
      (should (equal
               (plist-get (plist-get follow-up-options :provider-anchor)
                          :metadata)
               '(:response-id "resp-A")))
      (should (string-match-p
               (regexp-quote (plist-get result :raw-result))
               follow-up-printed))
      (should (string-match-p "call-ephemeral" follow-up-printed))
      (should (string-match-p
               (regexp-quote (plist-get result :replay-marker))
               follow-up-printed))
      ;; B selected the tool-result observation, and the promotion was
      ;; durable before the separate later request began.
      (should (= (length (plist-get promotion-input
                                    :source-observation-ids))
                 1))
      (should (string-prefix-p
               "observation:tool-bundle:"
               (car (plist-get promotion-input
                               :source-observation-ids))))
      (should (equal
               (plist-get (car promotions-at-later-request) :facts)
               '((:id "selected-tool-fact"
                  :value "selected from tool result"))))
      (should (string-match-p "selected from tool result" later-printed))
      ;; Later context is rebuilt from the durable session path and never
      ;; inherits the consumed call/result bundle or A/B response ids.
      (should-not (string-match-p
                   (regexp-quote (plist-get result :raw-result))
                   later-printed))
      (should-not (string-match-p "call-ephemeral" later-printed))
      (should-not (string-match-p
                   (regexp-quote (plist-get result :replay-marker))
                   later-printed))
      (should-not (string-match-p "resp-A" later-printed))
      (should-not (string-match-p "resp-B" later-printed))
      (should-not (equal
                   (plist-get (plist-get later-options :provider-anchor)
                              :metadata)
                   '(:response-id "resp-A")))
      (should-not (equal
                   (plist-get (plist-get later-options :provider-anchor)
                              :metadata)
                   '(:response-id "resp-B")))
      (should-not (member "resp-A" (plist-get result :anchors)))
      (should-not (member "resp-B" (plist-get result :anchors)))
      (cond
       ((and (eq continuation 'linear)
             (eq delivery 'inherited))
        ;; Inherited observations cannot advance a linear continuation, so
        ;; the later request takes the safe stateless path.
        (should-not (plist-get later-options :provider-anchor)))
       (t
        ;; Branchable inherited observations and request-local replacement
        ;; may repeatedly branch from the clean seed anchor, but never from
        ;; contaminated descendants.
        (should (equal
                 (plist-get (plist-get later-options :provider-anchor)
                            :metadata)
                 '(:response-id "resp-clean")))))
      (when (eq delivery 'request-local-replaceable)
        ;; The replaceable canvas is safe by itself, but the inherited tool
        ;; result makes this mixed frontier contaminated and non-persistable.
        (should-not (plist-get follow-up-options
                               :lifetime-ephemerals-clean-p))))))

(ert-deftest e-provider-continuation-integration-test-null-summary-replays-safely ()
  "Provider reasoning summary JSON null becomes an array on next full replay."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token"
                process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-model-providers
          '((replay-e2e
             :name "Replay E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :response-store :json-false
             :continuation nil
             :include-encrypted-reasoning t
             :requires-openai-auth nil)))
         (call-count 0)
         captured-body
         (harness
          (e-openai-create-harness
           :provider 'replay-e2e
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (cl-incf call-count)
              (if (= call-count 1)
                  (e-provider-continuation-integration--sse
                   '((type . "response.output_item.done")
                     (item . ((type . "reasoning")
                              (id . "rs-provider")
                              (encrypted_content . "ciphertext")
                              (summary . nil))))
                   '((type . "response.output_text.done")
                     (text . "seed answer"))
                   '((type . "response.completed")
                     (response . ((id . "resp-seed")
                                  (status . "completed")))))
                (setq captured-body (json-read-from-string body))
                (e-provider-continuation-integration--sse
                 '((type . "response.output_text.done")
                   (text . "replay accepted"))
                 '((type . "response.completed")
                   (response . ((id . "resp-replay")
                                (status . "completed")))))))))))
    (e-board-e2e-create-session harness :id "null-summary-replay")
    (e-board-e2e-prompt-batch harness "null-summary-replay" "seed prompt")
    (let* ((assistant
            (seq-find (lambda (message)
                        (eq (plist-get message :role) 'assistant))
                      (e-harness-messages harness "null-summary-replay")))
           (record
            (car (plist-get (plist-get assistant :metadata)
                            :provider-replay-items)))
           (item (plist-get record :item)))
      (should (equal (plist-get item :type) "reasoning"))
      ;; Provider output remains opaque until it crosses back onto the wire.
      (should (plist-member item :summary))
      (should-not (plist-get item :summary)))
    (e-board-e2e-prompt-batch harness "null-summary-replay" "continue")
    (let* ((input (append (alist-get 'input captured-body) nil))
           (reasoning
            (seq-find (lambda (item)
                        (equal (alist-get 'type item) "reasoning"))
                      input)))
      (should (= call-count 2))
      (should reasoning)
      (should (equal (alist-get 'encrypted_content reasoning) "ciphertext"))
      (should (equal (alist-get 'summary reasoning) [])))))

(ert-deftest e-provider-continuation-integration-test-tool-followup-advances-anchor ()
  "An anchored tool turn advances to its response and sends only tool output."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-model-providers
          '((continuation-e2e
             :name "Continuation E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :response-store t
             :continuation t
             :requires-openai-auth nil)))
         (requests nil)
         (call-count 0)
         (harness
          (e-openai-create-harness
           :provider 'continuation-e2e
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (setq call-count (1+ call-count))
              (push (json-read-from-string body) requests)
              (pcase call-count
                (1
                 (e-provider-continuation-integration--sse
                  '((type . "response.output_text.done")
                    (text . "seed answer"))
                  '((type . "response.completed")
                    (response . ((id . "resp-seed")
                                 (status . "completed"))))))
                (2
                 (e-provider-continuation-integration--sse
                  '((type . "response.output_item.done")
                    (item . ((type . "function_call")
                             (call_id . "call-1")
                             (name . "inspect")
                             (arguments . "{\"target\":\"state\"}"))))
                  '((type . "response.completed")
                    (response . ((id . "resp-tool")
                                 (status . "completed"))))))
                (_
                 (e-provider-continuation-integration--sse
                  '((type . "response.output_text.done")
                    (text . "final answer"))
                  '((type . "response.completed")
                    (response . ((id . "resp-final")
                                 (status . "completed"))))))))))))
    (e-harness-activate-capability
     harness
     (e-capability-create
      :id 'inspect-capability
      :tools
      (list
       (lambda (registry)
         (e-tools-register
          registry
          :name "inspect"
          :description "Inspect state."
          :work
          (e-tools-cheap-work
           "e2e.provider-continuation.inspect"
           (lambda (arguments)
             (format "fresh %s"
                     (plist-get arguments :target)))))))))
    (e-board-e2e-create-session harness :id "session-1")
    (e-board-e2e-prompt-batch harness "session-1" "seed")
    (e-board-e2e-prompt-batch harness "session-1" "inspect now")
    (let* ((ordered (nreverse requests))
           (anchored-tool-request (nth 1 ordered))
           (followup-request (nth 2 ordered)))
      (should (= call-count 3))
      (should (equal (alist-get 'previous_response_id anchored-tool-request)
                     "resp-seed"))
      (should (equal (e-provider-continuation-integration--input-types
                      anchored-tool-request)
                     '("message")))
      (should (equal (e-provider-continuation-integration--first-input-text
                      anchored-tool-request)
                     "inspect now"))
      (should (equal (alist-get 'previous_response_id followup-request)
                     "resp-tool"))
      (should (equal (e-provider-continuation-integration--input-types
                      followup-request)
                     '("function_call_output")))
      (let* ((input (alist-get 'input followup-request))
             (function-output (aref input 0)))
        (should (equal (alist-get 'call_id function-output) "call-1"))
        (should (equal (alist-get 'output function-output)
                       "fresh state")))
      ;; The request above selected a session-owned anchor produced by the
      ;; preceding real OpenAI response.  Keep a direct persistence assertion
      ;; alongside the wire-level previous_response_id proof.
      (let* ((anchors (e-session-provider-anchors
                       (e-harness-sessions harness)
                       "session-1"))
             (latest (car (last anchors))))
        (should (equal (plist-get (plist-get latest :metadata) :response-id)
                       "resp-final"))))))

(ert-deftest e-provider-continuation-integration-test-forged-anchor-fields-are-ignored ()
  "Forged anchor fields cannot enter a real OpenAI request or persisted state."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token"
                process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-model-providers
          '((forged-anchor-e2e
             :name "Forged Anchor E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :response-store t
             :continuation t
             :observation-delivery request-local-replaceable
             :responses-context-layout developer-input
             :requires-openai-auth nil)))
         (current-state "FORGED-CURRENT-STATE-MUST-ONLY-BE-IN-INSTRUCTIONS")
         (requests nil)
         (harness
          (e-openai-create-harness
           :provider 'forged-anchor-e2e
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (push (json-read-from-string body) requests)
              (e-provider-continuation-integration--sse
               '((type . "response.output_text.done")
                 (text . "real answer"))
               '((type . "response.completed")
                 (response . ((id . "resp-real")
                              (status . "completed")))))))))
         (dynamic-provider
          (e-context-provider-create
           :name 'forged-anchor-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state))))))
    (setf (e-harness-default-options harness)
          (plist-put
           (copy-sequence (e-harness-default-options harness))
           :provider-anchor
           '(:provider-id openai :metadata (:response-id "forged-response"))))
    (setf (e-harness-default-options harness)
          (plist-put
           (e-harness-default-options harness)
           :provider-anchor-delta-messages
           '((:role user :content "FORGED-DELTA"))))
    (setf (e-harness-default-options harness)
          (plist-put
           (e-harness-default-options harness)
           :provider-anchor-source-message-count
           999))
    (e-harness-activate-capability
     harness
     (e-capability-create
      :id 'forged-anchor-current-state-capability
      :instructions "stable policy"
      :context-providers (list dynamic-provider)))
    (e-board-e2e-create-session harness :id "forged-anchor-session")
    ;; Seed the live session options directly as hostile persisted/session
    ;; state; normal session-option normalization already drops unknown keys,
    ;; but the harness boundary must remain safe if such state is restored.
    (let ((session (e-session-get (e-harness-sessions harness)
                                  "forged-anchor-session")))
      (plist-put
       session
       :turn-options
       '(:provider-anchor
         (:provider-id openai :metadata (:response-id "forged-session-response"))
         :provider-anchor-delta-messages
         ((:role user :content "FORGED-SESSION-DELTA"))
         :provider-anchor-source-message-count 777)))
    (let* ((context (e-harness-turn-context
                     harness "forged-anchor-session" "before-request"))
           (options (plist-get context :options)))
      (dolist (key '(:provider-anchor
                     :provider-anchor-delta-messages
                     :provider-anchor-source-message-count))
        (should-not (plist-member options key))))
    (e-board-e2e-prompt-batch harness "forged-anchor-session" "prompt")
    (let* ((body (car requests))
           (input (alist-get 'input body))
           (input-json (json-encode input))
           (anchors (e-session-provider-anchors
                     (e-harness-sessions harness)
                     "forged-anchor-session"))
           (latest (car (last anchors))))
      (should (= (length requests) 1))
      (should-not (alist-get 'previous_response_id body))
      (should (string-match-p
               (regexp-quote current-state)
               (alist-get 'instructions body)))
      (should-not (string-match-p (regexp-quote current-state) input-json))
      (should-not (string-match-p "FORGED-DELTA" input-json))
      (should-not (string-match-p "FORGED-SESSION-DELTA" input-json))
      (should (= (length anchors) 1))
      (should (equal (plist-get (plist-get latest :metadata) :response-id)
                     "resp-real"))
      (should-not (equal (plist-get (plist-get latest :metadata) :response-id)
                         "forged-response")))))

(ert-deftest e-provider-continuation-integration-test-refresh-tool-replaces-current-state-atomically ()
  "A refresh-context tool changes continuation instructions without stale state."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token"
                process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-model-providers
          '((refresh-e2e
             :name "Refresh E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :response-store t
             :continuation t
             :observation-delivery request-local-replaceable
             :responses-context-layout developer-input
             :requires-openai-auth nil)))
         (current-state "STATE-A")
         (requests nil)
         (call-count 0)
         (dynamic-provider
          (e-context-provider-create
           :name 'refresh-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (harness
          (e-openai-create-harness
           :provider 'refresh-e2e
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (let ((parsed (json-read-from-string body)))
                (push parsed requests)
                (cl-incf call-count)
                (if (= call-count 1)
                    (e-provider-continuation-integration--sse
                     '((type . "response.output_item.done")
                       (item . ((type . "function_call")
                                (call_id . "refresh-call")
                                (name . "refresh-state")
                                (arguments . "{}"))))
                     '((type . "response.completed")
                       (response . ((id . "resp-refresh")
                                    (status . "completed")))))
                  (e-provider-continuation-integration--sse
                   '((type . "response.output_text.done")
                     (text . "refreshed answer"))
                   '((type . "response.completed")
                     (response . ((id . "resp-final")
                                  (status . "completed"))))))))))))
    (e-harness-activate-capability
     harness
     (e-capability-create
      :id 'refresh-current-state-capability
      :instructions "stable policy"
      :context-providers (list dynamic-provider)
      :tools
      (list
       (lambda (registry)
         (e-tools-register
          registry
          :name "refresh-state"
          :description "Refresh current state."
          :work
          (e-tools-cheap-work
           "e2e.provider-continuation.refresh-state"
           (lambda (_arguments)
             (setq current-state "STATE-B")
             (e-tools-result-create
              (plist-get (e-tools-current-context) :tool-call)
              'ok
              "state refreshed"
              '(:refresh-context t)))))))))
    (e-board-e2e-create-session harness :id "refresh-session")
    (e-board-e2e-prompt-batch harness "refresh-session" "refresh now")
    (let* ((ordered (nreverse requests))
           (first (nth 0 ordered))
           (second (nth 1 ordered))
           (second-input (alist-get 'input second))
           (function-output (aref second-input 0)))
      (should (= call-count 2))
      (should (equal (alist-get 'instructions first)
                     "You are a helpful assistant.\n\nstable policy\n\nSTATE-A"))
      (should (equal (alist-get 'instructions second)
                     "You are a helpful assistant.\n\nstable policy\n\nSTATE-B"))
      (should-not (string-match-p "STATE-A" (json-encode second)))
      (should (equal (alist-get 'previous_response_id second)
                     "resp-refresh"))
      (should (= (length second-input) 1))
      (should (equal (alist-get 'type function-output)
                     "function_call_output"))
      (should (equal (alist-get 'call_id function-output)
                     "refresh-call"))
      (should (equal (alist-get 'output function-output)
                     "state refreshed")))))

(ert-deftest e-provider-continuation-integration-test-fresh-turn-advances-each-tool-response ()
  "A fresh multi-tool turn chains each stored response without replay."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-model-providers
          '((continuation-e2e
             :name "Continuation E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :response-store t
             :continuation t
             :requires-openai-auth nil)))
         (requests nil)
         (call-count 0)
         (harness
          (e-openai-create-harness
           :provider 'continuation-e2e
           :model "gpt-test"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (setq call-count (1+ call-count))
              (push (json-read-from-string body) requests)
              (if (< call-count 3)
                  (e-provider-continuation-integration--sse
                   `((type . "response.output_item.done")
                     (item . ((type . "function_call")
                              (call_id . ,(format "call-%d" call-count))
                              (name . "inspect")
                              (arguments . ,(format
                                             "{\"target\":\"state-%d\"}"
                                             call-count)))))
                   `((type . "response.completed")
                     (response . ((id . ,(format "resp-tool-%d" call-count))
                                  (status . "completed")))))
                (e-provider-continuation-integration--sse
                 '((type . "response.output_text.done")
                   (text . "final answer"))
                 '((type . "response.completed")
                   (response . ((id . "resp-final")
                                (status . "completed")))))))))))
    (e-harness-activate-capability
     harness
     (e-capability-create
      :id 'inspect-capability
      :tools
      (list
       (lambda (registry)
         (e-tools-register
          registry
          :name "inspect"
          :description "Inspect state."
          :work
          (e-tools-cheap-work
           "e2e.provider-continuation.inspect"
           (lambda (arguments)
             (format "fresh %s" (plist-get arguments :target)))))))))
    (e-board-e2e-create-session harness :id "session-1")
    (e-board-e2e-prompt-batch harness "session-1" "inspect twice")
    (let ((ordered (nreverse requests)))
      (should (= call-count 3))
      (should-not (alist-get 'previous_response_id (nth 0 ordered)))
      (should (equal (alist-get 'previous_response_id (nth 1 ordered))
                     "resp-tool-1"))
      (should (equal (e-provider-continuation-integration--input-types
                      (nth 1 ordered))
                     '("function_call_output")))
      (should (equal (alist-get 'previous_response_id (nth 2 ordered))
                     "resp-tool-2"))
      (should (equal (e-provider-continuation-integration--input-types
                      (nth 2 ordered))
                     '("function_call_output"))))))

(ert-deftest e-provider-continuation-integration-test-websocket-isolated-by-session ()
  "Concurrent harness sessions do not share one active Responses request."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-model-providers
          '((continuation-websocket-e2e
             :name "Continuation WebSocket E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :response-store t
             :continuation t
             :requires-openai-auth nil)))
         (harness
          (e-openai-create-harness
           :provider 'continuation-websocket-e2e
           :model "gpt-test"))
         (open-count 0)
         (callbacks (make-hash-table :test 'eq))
         sockets)
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (let ((socket (intern (format "fake-websocket-%d"
                                               (cl-incf open-count)))))
                   (puthash socket (plist-get args :on-message) callbacks)
                   (push socket sockets)
                   socket)))
              ((symbol-function 'websocket-send-text)
               (lambda (_websocket _text) t))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (e-board-e2e-reset-runtime)
      (e-chat-service-create-session :harness harness :id "session-one")
      (e-chat-service-create-session :harness harness :id "session-two")
      (e-board-e2e-prompt-async harness "session-one" "first request")
      (e-board-e2e-prompt-async harness "session-two" "second request")
      (should (= open-count 2))
      (dolist (socket sockets)
        (let ((on-message (gethash socket callbacks)))
          (funcall on-message socket
                   (json-encode
                    '(:type "response.output_text.done" :text "done")))
          (funcall on-message socket
                   (json-encode
                    `(:type "response.completed"
                      :response (:id ,(format "response-%s" socket)
                                 :status "completed"))))))
      (should (eq (plist-get (e-board-e2e-wait-batch
                              harness "session-one" 1.0)
                             :status)
                  'done))
      (should (eq (plist-get (e-board-e2e-wait-batch
                              harness "session-two" 1.0)
                             :status)
                  'done)))))

(ert-deftest e-provider-continuation-integration-test-websocket-gpt56-explicit-cache ()
  "Built-in Codex and OpenAI use canonical late-frontier input on native sockets."
  (dolist (provider-id '(codex openai))
    (let* ((process-environment
            (cons "OPENAI_API_KEY=test-api-token" process-environment))
           (e-harness-auto-compaction-enabled nil)
           (e-openai-websocket-idle-timeout-seconds nil)
           (e-openai-websocket-connection-idle-seconds nil)
           (e-openai-model-providers
            (list
             (cons
              'codex
              (list
               :name "ChatGPT Codex"
               :base-url (concat e-openai-codex-default-base-url "/codex")
               :wire-api 'responses
               :responses-transport 'websocket
               :continuation t
               :requires-openai-auth t))
             (cons 'openai (e-openai--builtin-openai-profile))))
           (current-state "OBSERVATION-OLD")
           (dynamic-provider
            (e-context-provider-create
             :name 'cross-turn-current-state
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      (list (list :role 'system :content current-state)))))
           (harness
            (e-openai-create-harness
             :provider provider-id
             :model "gpt-5.6-sol"))
           (capabilities
            (e-backend-context-capabilities
             (e-harness-backend harness)
             nil))
           (open-count 0)
           (send-count 0)
           (original-websocket-start
            (symbol-function 'e-openai-codex--websocket-request-start))
           backend-requests
           sends
           on-message)
      (e-harness-activate-capability
       harness
       (e-capability-create
        :id 'cross-turn-current-state-capability
        :instructions "stable instructions"
        :context-providers (list dynamic-provider)))
      (cl-letf (((symbol-function 'e-openai-codex-read-auth)
                 (lambda (&optional _auth-file)
                   (list :tokens
                         (list :access_token
                               (e-provider-continuation-integration--jwt)))))
                ((symbol-function 'e-openai-codex--websocket-request-start)
                 (lambda (&rest args)
                   (let ((request (apply original-websocket-start args)))
                     (push request backend-requests)
                     request)))
                ((symbol-function 'websocket-open)
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
                     (cl-incf send-count)
                     (push payload sends)
                     (funcall on-message websocket
                              (json-encode
                               `(:type "response.output_text.done"
                                 :text ,(format "answer-%d" send-count))))
                     (funcall on-message websocket
                              (json-encode
                               `(:type "response.completed"
                                 :response
                                 (:id ,(format "resp-%d" send-count)
                                  :status "completed")))))))
                ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
        (e-board-e2e-create-session harness :id "session-one")
        (e-session-set-turn-options
         (e-harness-sessions harness)
         "session-one"
         '(:prompt-cache-default t))
        (e-board-e2e-prompt-batch harness "session-one" "first prompt")
        (should-not
         (e-session-provider-anchors
          (e-harness-sessions harness) "session-one"))
        (setq current-state "OBSERVATION-NEW")
        (e-board-e2e-prompt-batch harness "session-one" "second prompt")
        (let* ((ordered (nreverse sends))
               (first (car ordered))
               (second (cadr ordered))
               (first-input (plist-get first :input))
               (second-input (plist-get second :input))
               (stable-block
                (car (plist-get (car first-input) :content)))
               (second-literal (prin1-to-string second))
               (diagnostics
                (plist-get (e-backend-request-metadata
                            (car backend-requests))
                           :diagnostics))
               (anchors
                (e-session-provider-anchors
                 (e-harness-sessions harness) "session-one"))
               (anchor-ids
                (mapcar
                 (lambda (anchor)
                   (plist-get (plist-get anchor :metadata) :response-id))
                 anchors)))
          (ert-info ((format "Built-in WebSocket provider: %S" provider-id))
            (should (eq (plist-get capabilities :observation-delivery)
                        'inherited))
            (dolist (kind '(current-state dynamic-context))
              (should
               (eq (e-backend-observation-delivery-for-kind
                    capabilities kind)
                   'inherited)))
            (dolist (kind '(tool-result trace retrieved-excerpt))
              (should
               (eq (e-backend-observation-delivery-for-kind
                    capabilities kind)
                   'inherited)))
            (should (= open-count 1))
            (should (= send-count 2))
            (should (equal (plist-get first :type) "response.create"))
            (should (eq (plist-get first :store) :json-false))
            (should (equal (plist-get first :include)
                           '("reasoning.encrypted_content")))
            (should-not (plist-member first :previous_response_id))
            (should (equal (plist-get first :instructions)
                           "You are a helpful assistant."))
            (should (string-match-p "OBSERVATION-OLD"
                                    (prin1-to-string first-input)))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   first-input)
                           '("developer" "user" "developer")))
            (should (equal (plist-get stable-block :text)
                           "stable instructions"))
            (should (eq (plist-get second :store) :json-false))
            (should (equal (plist-get second :include)
                           '("reasoning.encrypted_content")))
            (should-not (plist-member second :previous_response_id))
            (should (equal (plist-get second :instructions)
                           "You are a helpful assistant."))
            (should-not (string-match-p "OBSERVATION-OLD" second-literal))
            (should (string-match-p "OBSERVATION-NEW"
                                    (prin1-to-string second-input)))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   second-input)
                           '("developer" "user" "assistant" "user"
                             "developer")))
            (should (equal (plist-get
                            (car (plist-get (nth 3 second-input) :content))
                            :text)
                           "second prompt"))
            (should (eq (plist-get diagnostics :websocket-request-mode)
                        'full))
            (should-not (plist-get diagnostics :previous-response-id-present))
            (should (eq (plist-get diagnostics :websocket-reused) t))
            (should (eq (plist-get diagnostics :observation-delivery)
                        'inherited))
            (should-not (plist-get diagnostics
                                   :replaceable-current-state-present))
            (should (eq (plist-get diagnostics :provider-anchor-safety)
                        'hold-inherited-observation))
            (should-not anchor-ids)
            (if (eq provider-id 'codex)
                (progn
                  (should-not (plist-member first :prompt_cache_options))
                  (should-not (plist-member second :prompt_cache_options))
                  (should-not
                   (string-match-p "prompt_cache_breakpoint"
                                   (prin1-to-string first-input)))
                  (should (equal (plist-get diagnostics :prompt-cache-mode)
                                 "implicit-segmented")))
              (should (equal (plist-get first :prompt_cache_options)
                             '(:mode "explicit")))
              (should (equal (plist-get second :prompt_cache_options)
                             '(:mode "explicit")))
              (should (equal (plist-get stable-block
                                        :prompt_cache_breakpoint)
                             '(:mode "explicit")))
              (should (equal (plist-get diagnostics :prompt-cache-mode)
                             "explicit")))))))))


(ert-deftest e-provider-continuation-integration-test-canonical-observe-promote-forget ()
  "A streamed inherited tool turn promotes a fact and forgets its raw bundle.

The OpenAI Responses adapter, harness, loop, session, semantic projection,
and ordinary tool execution are real.  Only the request function supplies a
deterministic streamed Responses transcript.  The inherited profile is
linear: its changing frontier may continue the response that produced the
tool call once, but cannot make that response or its descendant a later
ordinary-turn anchor."
  (let* ((process-environment
          (cons "OPENAI_API_KEY=credential-free-canonical-test"
                process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-context-lifetime-shadow-projection-enabled t)
         (e-openai-model-providers
          '((canonical-observe-promote-e2e
             :name "Canonical Observe Promote E2E"
             :base-url "https://canonical-observe-promote.example.test/v1"
             :auth bearer
             :env-key "OPENAI_API_KEY"
             :wire-api responses
             :responses-transport http
             :response-store t
             :responses-context-layout developer-input
             :include-encrypted-reasoning t
             :observation-delivery inherited
             :continuation t
             :requires-openai-auth nil)))
         (current-state nil)
         (raw-result "RAW-CANONICAL-TOOL-RESULT")
         (requests nil)
         (request-projections nil)
         (request-count 0)
         (tool-count 0)
         (promotions-at-next-request nil)
         (harness nil)
         (dynamic-provider
          (e-context-provider-create
           :name 'canonical-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (when current-state
                      (list (list :role 'system :content current-state))))))
         (capability
          (e-capability-create
           :id 'canonical-observe-promote-capability
           :instructions "STABLE-CANONICAL-INSTRUCTIONS"
           :context-providers (list dynamic-provider)
           :tools
           (list
            (lambda (registry)
              (e-tools-register
               registry
               :name "inspect-canonical"
               :description "Return one deterministic canonical-test result."
               :parameters '(:type "object"
                             :properties (:target (:type "string"))
                             :required ["target"])
               :work
               (e-tools-cheap-work
                "integration.canonical-observe-promote.inspect"
                (lambda (_arguments)
                  (cl-incf tool-count)
                  raw-result)))))))
         (request-function
          (cl-function
           (lambda (&key url headers body)
             (ignore url headers)
             (let ((parsed (json-read-from-string body)))
               (setq requests
                     (append requests
                             (list (list :body body :parsed parsed)))))
             (cl-incf request-count)
             (pcase request-count
               (1
                (e-provider-continuation-integration--sse
                 '((type . "response.output_text.done")
                   (text . "D0 durable answer"))
                 '((type . "response.completed")
                   (response . ((id . "resp-d0")
                                (status . "completed"))))))
               (2
                ;; R1 is the streamed tool-producing response.  Its response
                ;; id is usable only by the matching function-call-output
                ;; continuation below.
                (e-provider-continuation-integration--sse
                 '((type . "response.output_item.done")
                   (item . ((type . "reasoning")
                            (id . "reasoning-r1")
                            (encrypted_content . "RAW-CANONICAL-REPLAY")
                            (summary . []))))
                 '((type . "response.output_item.done")
                   (item . ((type . "function_call")
                            (call_id . "call-canonical-inspect")
                            (name . "inspect-canonical")
                            (arguments . "{\"target\":\"raw\"}"))))
                 '((type . "response.completed")
                   (response . ((id . "resp-r1")
                                (status . "completed"))))))
               (3
                ;; The promotion carrier is a real streamed Responses item.
                ;; Its trusted frame/observation frontier comes from the
                ;; harness projection captured immediately before this call.
                (let* ((projection (car (last request-projections)))
                       (options (plist-get projection :options))
                       (frame-id
                        (plist-get options :context-promotion-frame-id))
                       (observation-id
                        (seq-find
                         (lambda (value)
                           (string-prefix-p "observation:tool-bundle:" value))
                         (plist-get options
                                    :context-promotion-observation-ids)))
                       (promotion-arguments
                        (json-encode
                         (list
                          :schema-version 1
                          :frame-id frame-id
                          :source-observation-ids (vector observation-id)
                          :facts
                          (vector
                           (list :id "canonical-fact"
                                 :value "PROMOTED-CANONICAL-FACT"))))))
                  (unless (and frame-id observation-id)
                    (error "Missing canonical promotion frontier: %S"
                           projection))
                  (let ((response
                         (e-provider-continuation-integration--sse
                          (list (cons 'type "response.output_item.done")
                                (cons 'item
                                      (list (cons 'type "function_call")
                                            (cons 'call_id
                                                  "call-context-promote")
                                            (cons 'name "context-promote")
                                            (cons 'arguments
                                                  promotion-arguments))))
                          '((type . "response.output_text.done")
                            (text . "D1 promoted answer"))
                          '((type . "response.completed")
                            (response . ((id . "resp-r2")
                                         (status . "completed")))))))
                    response)))
               (4
                ;; This read happens at provider dispatch time, before the
                ;; ordinary request can be sent, and proves promotion commit
                ;; ordering independently of the resulting request body.
                (setq promotions-at-next-request
                      (copy-tree
                       (e-session-context-promotions
                        (e-harness-sessions harness)
                        "canonical-observe-promote")))
                (e-provider-continuation-integration--sse
                 '((type . "response.output_text.done")
                   (text . "D2 canonical answer"))
                 '((type . "response.completed")
                   (response . ((id . "resp-r3")
                                (status . "completed"))))))
               (_
                (error "Unexpected canonical request %S" request-count)))))))
    (setq harness
          (e-openai-create-harness
           :provider 'canonical-observe-promote-e2e
           :model "gpt-5.6-sol"
           :request-function request-function))
    (e-harness-activate-capability harness capability)
    (let ((original-body (symbol-function 'e-openai-codex-request-body)))
      (cl-letf (((symbol-function 'e-openai-codex-request-body)
                 (lambda (&rest args)
                   (let ((body (apply original-body args)))
                     (setq request-projections
                           (append request-projections
                                   (list (list :messages
                                               (copy-tree (plist-get args
                                                                      :messages))
                                               :options
                                               (copy-tree (plist-get args
                                                                      :options))
                                               :body body))))
                     body))))
        (e-board-e2e-create-session
         harness :id "canonical-observe-promote")
        (e-board-e2e-prompt-batch
         harness "canonical-observe-promote" "D0 durable seed")
        (setq current-state "CURRENT-CANONICAL-FRONTIER-ONE")
        (e-board-e2e-prompt-batch
         harness "canonical-observe-promote" "D1 inspect durable state")
        (setq current-state "CURRENT-CANONICAL-FRONTIER-TWO")
        (e-board-e2e-prompt-batch
         harness "canonical-observe-promote" "D2 later ordinary turn")
        (let* ((ordered requests)
               (d0 (nth 0 ordered))
               (tool-followup (nth 2 ordered))
               (d2 (nth 3 ordered))
               (d0-body (plist-get d0 :parsed))
               (followup-body (plist-get tool-followup :parsed))
               (followup-input (alist-get 'input followup-body))
               (d2-body (plist-get d2 :parsed))
               (d2-input (alist-get 'input d2-body))
               (d2-printed (json-encode d2-body))
               (anchors
                (e-session-provider-anchors
                 (e-harness-sessions harness)
                 "canonical-observe-promote"))
               (anchor-ids
                (mapcar
                 (lambda (anchor)
                   (plist-get (plist-get anchor :metadata) :response-id))
                 anchors))
               (promotion-record
                (plist-get (car promotions-at-next-request)
                           :context-record)))
          (should (= request-count 4))
          (should (= (length ordered) 4))
          (should (= tool-count 1))
          (should-not (alist-get 'previous_response_id d0-body))
          (should (equal (alist-get 'previous_response_id followup-body)
                         "resp-r1"))
          (should (equal
                   (mapcar (lambda (item) (alist-get 'type item))
                           followup-input)
                   '("function_call_output")))
          (let ((function-output (aref followup-input 0)))
            (should (equal (alist-get 'call_id function-output)
                           "call-canonical-inspect"))
            (should (equal (alist-get 'output function-output)
                           raw-result)))
          ;; R1 is a one-shot causal continuation; R2 is its contaminated
          ;; descendant.  Neither may authorize the ordinary D2 request.
          (should-not (alist-get 'previous_response_id d2-body))
          (should (string-match-p "D0 durable seed" d2-printed))
          (should (string-match-p "D1 inspect durable state" d2-printed))
          (should (string-match-p "D2 later ordinary turn" d2-printed))
          (should (string-match-p "PROMOTED-CANONICAL-FACT" d2-printed))
          (should (string-match-p "CURRENT-CANONICAL-FRONTIER-TWO"
                                  d2-printed))
          (should-not (string-match-p "CURRENT-CANONICAL-FRONTIER-ONE"
                                      d2-printed))
          (dolist (marker '("RAW-CANONICAL-REPLAY"
                            "RAW-CANONICAL-TOOL-RESULT"
                            "call-canonical-inspect"
                            "resp-r1"
                            "resp-r2"))
            (should-not (string-match-p marker d2-printed)))
          ;; The late inherited frontier remains input-owned, while stable
          ;; instructions stay separate from the changing current value.
          (should-not (string-match-p "CURRENT-CANONICAL-FRONTIER-TWO"
                                      (alist-get 'instructions d2-body)))
          (should
           (seq-some
            (lambda (item)
              (and (equal (alist-get 'role item) "developer")
                   (string-match-p "CURRENT-CANONICAL-FRONTIER-TWO"
                                   (json-encode item))))
            d2-input))
          (should (= (length promotions-at-next-request) 1))
          (should (equal
                   (plist-get promotion-record :facts)
                   '((:id "canonical-fact"
                      :value "PROMOTED-CANONICAL-FACT"))))
          (should-not (member "resp-r1" anchor-ids))
          (should-not (member "resp-r2" anchor-ids)))))))

(ert-deftest e-provider-continuation-integration-test-branchable-inherited-reuses-clean-anchor ()
  "Branchable inherited observations reuse one clean anchor without promotion."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token"
                process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-openai-model-providers
          '((branchable-inherited-e2e
             :name "Branchable Inherited E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :response-store t
             :responses-context-layout developer-input
             :prompt-cache-breakpoint-mode explicit
             :continuation t
             :requires-openai-auth nil)))
         (current-state nil)
         (requests nil)
         (call-count 0)
         (dynamic-provider
          (e-context-provider-create
           :name 'branchable-inherited-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (when current-state
                      (list (list :role 'system :content current-state))))))
         (harness
          (e-openai-create-harness
           :provider 'branchable-inherited-e2e
           :model "gpt-5.6-sol"
           :request-function
           (cl-function
            (lambda (&key url headers body)
              (ignore url headers)
              (let ((parsed (json-read-from-string body)))
                (push parsed requests)
                (cl-incf call-count)
                (e-provider-continuation-integration--sse
                 `((type . "response.output_text.done")
                   (text . ,(format "answer-%d" call-count)))
                 `((type . "response.completed")
                   (response . ((id . ,(format "resp-%d" call-count))
                                (status . "completed")))))))))))
    ;; Exercise the provider-neutral branchable contract while retaining the
    ;; real OpenAI request renderer for effective-input assertions.
    (setf (e-backend--context-capabilities (e-harness-backend harness))
          (lambda (&rest _)
            '(:continuation branchable
              :observation-delivery inherited
              :prefix-cache explicit
              :provider-compaction none
              :reasoning-state replayable)))
    (e-harness-activate-capability
     harness
     (e-capability-create
      :id 'branchable-inherited-current-state-capability
      :instructions "stable instructions"
      :context-providers (list dynamic-provider)))
    (e-board-e2e-create-session harness :id "branchable-inherited")
    (e-session-set-turn-options
     (e-harness-sessions harness)
     "branchable-inherited"
     '(:prompt-cache-default t))
    ;; The first response is the clean anchor: no current-state observation
    ;; exists yet, so it is safe to persist and reuse.
    (e-board-e2e-prompt-batch
     harness "branchable-inherited" "first durable prompt")
    (setq current-state "inherited state one")
    (e-board-e2e-prompt-batch
     harness "branchable-inherited" "second durable delta")
    (setq current-state "inherited state two")
    (e-board-e2e-prompt-batch
     harness "branchable-inherited" "third durable delta")
    (let* ((ordered (nreverse requests))
           (first (nth 0 ordered))
           (second (nth 1 ordered))
           (third (nth 2 ordered))
           (input-texts
            (lambda (body)
              (mapcar
               (lambda (item)
                 (alist-get 'text
                            (aref (alist-get 'content item) 0)))
               (append (alist-get 'input body) nil))))
           (anchors (e-session-provider-anchors
                     (e-harness-sessions harness)
                     "branchable-inherited")))
      (should (= call-count 3))
      (should (= (length anchors) 1))
      (should (equal (plist-get (plist-get (car anchors) :metadata)
                                :response-id)
                     "resp-1"))
      (should (equal (alist-get 'previous_response_id second) "resp-1"))
      (should (equal (alist-get 'previous_response_id third) "resp-1"))
      (should (equal (funcall input-texts second)
                     '("inherited state one" "second durable delta")))
      (should (equal (funcall input-texts third)
                     '("inherited state two" "second durable delta"
                       "answer-2" "third durable delta")))
      (should-not (member "first durable prompt"
                          (funcall input-texts second)))
      (should-not (member "first durable prompt"
                          (funcall input-texts third)))
      (should-not (member "inherited state one"
                          (funcall input-texts third))))))

(ert-deftest e-provider-continuation-integration-test-websocket-failure-retry-clears-active-request ()
  "A terminal Responses failure releases the socket before harness retry."
  (let* ((process-environment
          (cons "OPENAI_GATEWAY_API_KEY=test-gateway-token" process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-harness-retry-initial-backoff-seconds 0.01)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.01)
         (e-harness-retry-max-elapsed-seconds 1.0)
         (e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-model-providers
          '((continuation-websocket-e2e
             :name "Continuation WebSocket E2E"
             :base-url "https://gateway.example.test/v1"
             :auth bearer
             :env-key "OPENAI_GATEWAY_API_KEY"
             :wire-api responses
             :responses-transport websocket
             :response-store t
             :continuation t
             :requires-openai-auth nil)))
         (harness
          (e-openai-create-harness
           :provider 'continuation-websocket-e2e
           :model "gpt-test"))
         (open-count 0)
         (send-count 0)
         (events nil)
         (callbacks (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (let ((socket (intern (format "fake-websocket-%d"
                                               (cl-incf open-count)))))
                   (puthash socket (plist-get args :on-message) callbacks)
                   socket)))
              ((symbol-function 'websocket-send-text)
               (lambda (socket _text)
                 (cl-incf send-count)
                 (let ((on-message (gethash socket callbacks)))
                   (if (= send-count 1)
                       (funcall
                        on-message socket
                        (json-encode
                         '(:type "response.failed"
                           :response
                           (:status "failed"
                            :error
                            (:code "server_error"
                             :message "529: overloaded_error")))))
                     (funcall on-message socket
                              (json-encode
                               '(:type "response.output_text.done"
                                 :text "recovered")))
                     (funcall on-message socket
                              (json-encode
                               '(:type "response.completed"
                                 :response (:id "response-recovered"
                                            :status "completed"))))))))
              ((symbol-function 'websocket-close) (lambda (&rest _args) t)))
      (e-board-e2e-reset-runtime)
      (e-chat-service-create-session :harness harness :id "session-one")
      (e-harness--install-activity-sink
       harness (lambda (event) (push event events)) :session-id "session-one")
      (e-board-e2e-prompt-async harness "session-one" "recover once")
      (let ((result (e-board-e2e-wait-batch harness "session-one" 1.0)))
        (should (eq (plist-get result :status) 'done)))
      (should (= send-count 2))
      (should (= open-count 2))
      (let ((types (mapcar (lambda (event) (plist-get event :type)) events)))
        (should (= (seq-count (lambda (type) (eq type 'turn-retrying)) types)
                   1))
        (should-not (memq 'turn-failed types))))))

(provide 'e-provider-continuation-integration-test)

;;; e-provider-continuation-integration-test.el ends here

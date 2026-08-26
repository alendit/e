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
  "Built-in Codex and OpenAI replace current state on their native sockets."
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
        (let* ((anchors
                (e-session-provider-anchors
                 (e-harness-sessions harness) "session-one"))
               (latest (car (last anchors))))
          (should (equal (plist-get (plist-get latest :metadata) :response-id)
                         "resp-1")))
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
            (should (equal (plist-get capabilities :observation-delivery)
                           e-openai--request-local-observation-delivery-map))
            (dolist (kind '(current-state dynamic-context))
              (should
               (eq (e-backend-observation-delivery-for-kind
                    capabilities kind)
                   'request-local-replaceable)))
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
                           "You are a helpful assistant.\n\nOBSERVATION-OLD"))
            (should-not (string-match-p
                         "OBSERVATION-OLD"
                         (prin1-to-string
                          (let ((without-instructions (copy-sequence first)))
                            (cl-remf without-instructions :instructions)
                            without-instructions))))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   first-input)
                           '("developer" "user")))
            (should (equal (plist-get stable-block :text)
                           "stable instructions"))
            (should (eq (plist-get second :store) :json-false))
            (should (equal (plist-get second :include)
                           '("reasoning.encrypted_content")))
            (should (equal (plist-get second :previous_response_id) "resp-1"))
            (should (equal (plist-get second :instructions)
                           "You are a helpful assistant.\n\nOBSERVATION-NEW"))
            (should-not (string-match-p "OBSERVATION-OLD" second-literal))
            (should-not (string-match-p "OBSERVATION-NEW"
                                        (prin1-to-string second-input)))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   second-input)
                           '("user")))
            (should (equal (plist-get
                            (car (plist-get (car second-input) :content))
                            :text)
                           "second prompt"))
            (should (eq (plist-get diagnostics :websocket-request-mode)
                        'incremental))
            (should (eq (plist-get diagnostics :previous-response-id-present)
                        t))
            (should (eq (plist-get diagnostics :websocket-reused) t))
            (should (eq (plist-get diagnostics :observation-delivery)
                        'request-local-replaceable))
            (should (eq (plist-get diagnostics
                                   :replaceable-current-state-present)
                        t))
            (should (eq (plist-get diagnostics :provider-anchor-safety)
                        'advance-eligible))
            (should (equal anchor-ids '("resp-1" "resp-2")))
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

(ert-deftest e-provider-continuation-integration-test-websocket-tool-chain-uses-older-clean-anchor ()
  "A WebSocket tool chain branches from its older clean anchor.

The harness, context-lifetime projection, ordinary tool lifecycle, OpenAI
Responses renderer, and WebSocket continuation state all remain real.  Only
the socket transport is deterministic fake state."
  (let* ((process-environment
          (cons "OPENAI_API_KEY=credential-free-websocket-test"
                process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-context-lifetime-shadow-projection-enabled t)
         (e-openai-websocket-idle-timeout-seconds nil)
         (e-openai-websocket-connection-idle-seconds nil)
         (e-openai-model-providers
          '((websocket-composed-e2e
             :name "WebSocket Composed E2E"
             :base-url "https://websocket-composed.example.test/v1"
             :wire-api responses
             :responses-transport websocket
             :response-store :json-false
             :responses-context-layout developer-input
             :include-encrypted-reasoning t
             :observation-delivery request-local-replaceable
             :continuation t
             :requires-openai-auth nil
             :env-key "OPENAI_API_KEY")))
         (current-state nil)
         (raw-tool-result "RAW-COMPOSED-TOOL-RESULT")
         (request-projections nil)
         (sent-requests nil)
         (backend-requests nil)
         (events nil)
         (callbacks (make-hash-table :test #'eq))
         (current-socket nil)
         (open-count 0)
         (send-count 0)
         (harness
          (e-openai-create-harness
           :provider 'websocket-composed-e2e
           :model "gpt-5.6-sol"))
         (dynamic-provider
          (e-context-provider-create
           :name 'websocket-composed-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (when current-state
                      (list (list :role 'system
                                  :content current-state))))))
         (capability
          (e-capability-create
           :id 'websocket-composed-capability
           :instructions "STABLE-COMPOSED-INSTRUCTIONS"
           :context-providers (list dynamic-provider)
           :tools
           (list
            (lambda (registry)
              (e-tools-register
               registry
               :name "inspect-composed"
               :description "Return one deterministic composed-test result."
               :parameters '(:type "object"
                             :properties (:target (:type "string"))
                             :required ["target"])
               :work
               (e-tools-cheap-work
                "integration.websocket-composed.inspect"
                (lambda (_arguments)
                  raw-tool-result)))))))
         (original-body (symbol-function 'e-openai-codex-request-body))
         (original-websocket-start
          (symbol-function 'e-openai-codex--websocket-request-start)))
    (e-harness-activate-capability harness capability)
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
                   body)))
              ((symbol-function 'e-openai-codex--websocket-request-start)
               (lambda (&rest args)
                 (let ((request (apply original-websocket-start args)))
                   (setq backend-requests
                         (append backend-requests (list request)))
                   request)))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (let ((socket (make-symbol
                                (format "fake-composed-websocket-%d"
                                        (1+ open-count)))))
                   (cl-incf open-count)
                   (puthash socket
                            (list :on-message (plist-get args :on-message)
                                  :on-close (plist-get args :on-close))
                            callbacks)
                   (setq current-socket socket)
                   socket)))
              ((symbol-function 'websocket-send-text)
               (lambda (websocket text)
                 (let* ((payload
                         (json-parse-string
                          text
                          :object-type 'plist
                          :array-type 'list
                          :null-object nil
                          :false-object :json-false))
                        (requested-response-id
                         (plist-get payload :previous_response_id))
                        (projection
                         (seq-find
                          (lambda (candidate)
                            (equal
                             (plist-get (plist-get candidate :body)
                                        :previous_response_id)
                             requested-response-id))
                          (reverse request-projections)))
                        (on-message
                         (plist-get (gethash websocket callbacks)
                                    :on-message)))
                   (let ((callback on-message))
                     (setq on-message
                           (lambda (socket frame)
                             (run-at-time
                              0 nil
                              (lambda ()
                                (funcall callback socket frame))))))
                   (cl-incf send-count)
                   (setq sent-requests
                         (append sent-requests
                                 (list (list :ordinal send-count
                                             :body payload
                                             :projection projection
                                             ;; Keep the serialized transport
                                             ;; measurement beside its parsed
                                             ;; request so dynamic response ids
                                             ;; cannot change which request is
                                             ;; being compared.
                                             :wire-bytes (string-bytes text)
                                             :input-item-count
                                             (length (or (plist-get payload :input)
                                                         nil))))))
                   (pcase send-count
                         (1
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.output_text.done"
                                  :text "clean answer")))
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-r0"
                                             :status "completed")))))
                     (2
                      ;; R1 is the only response that carries the raw
                      ;; provider replay and ordinary tool call.
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.output_item.done"
                                  :item (:type "reasoning"
                                         :id "reasoning-r1"
                                         :encrypted_content
                                         "RAW-COMPOSED-REPLAY"
                                         :summary []))))
                      (funcall on-message websocket
                               (json-encode
                                (list
                                 :type "response.output_item.done"
                                 :item
                                 (list :type "function_call"
                                       :call_id "call-composed-inspect"
                                       :name "inspect-composed"
                                       :arguments
                                       (json-encode '(:target "raw"))))))
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-r1"
                                             :status "completed")))))
                     (3
                      ;; The real tool result is already in the harness's
                      ;; consumed frame.  Return the reserved wire effect
                      ;; using only the frame/observation ids supplied by
                      ;; that projection.
                      (let* ((options (plist-get projection :options))
                             (frame-id
                              (plist-get options
                                         :context-promotion-frame-id))
                             (observation-id
                              (seq-find
                               (lambda (value)
                                 (string-prefix-p
                                  "observation:tool-bundle:" value))
                               (plist-get
                                options
                                :context-promotion-observation-ids)))
                             (arguments
                              (json-encode
                               (list
                                :schema-version 1
                                :frame-id frame-id
                                :source-observation-ids
                                (vector observation-id)
                                :facts
                                (vector
                                 (list :id "composed-fact"
                                       :value "PROMOTED-COMPOSED-FACT"))))))
                        (unless (and frame-id observation-id)
                          (error "Missing composed promotion frontier: %S"
                                 projection))
                        (funcall on-message websocket
                                 (json-encode
                                  (list
                                   :type "response.output_item.done"
                                   :item
                                   (list :type "function_call"
                                         :call_id "call-context-promote"
                                         :name "context-promote"
                                         :arguments arguments))))
                        (funcall on-message websocket
                                 (json-encode
                                  '(:type "response.output_text.done"
                                    :text "final answer")))
                        (funcall on-message websocket
                                 (json-encode
                                  '(:type "response.completed"
                                    :response (:id "resp-r2"
                                               :status "completed"))))))
                     (4
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.output_text.done"
                                  :text "warm ordinary answer")))
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-warm"
                                             :status "completed")))))
                     (5
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.output_text.done"
                                  :text "reconstructed ordinary answer")))
                      (funcall on-message websocket
                               (json-encode
                                '(:type "response.completed"
                                  :response (:id "resp-recovered"
                                             :status "completed")))))
                         (_ (error "Unexpected composed WebSocket request %S"
                                   send-count)))
                   t)))
              ((symbol-function 'websocket-close)
               (lambda (&rest _args) t)))
      (e-board-e2e-reset-runtime)
      (e-board-e2e-create-session harness :id "websocket-composed")
      (e-harness--install-activity-sink
       harness (lambda (event) (push event events))
       :session-id "websocket-composed")
      (e-board-e2e-prompt-batch
       harness "websocket-composed" "D0 durable seed")
      ;; This turn creates R1, executes the ordinary tool, and completes R2.
      (e-board-e2e-prompt-batch
       harness "websocket-composed" "D1 inspect durable state")
      (setq current-state "CURRENT-COMPOSED-INSTRUCTIONS")
      ;; The later ordinary turn must branch from the older R0 on the same
      ;; socket, while carrying only portable post-R0 data and the promotion.
      (e-board-e2e-prompt-batch
       harness "websocket-composed" "D2 later ordinary turn")
      (let* ((tool-followup (nth 2 sent-requests))
             (tool-followup-body (plist-get tool-followup :body))
             (tool-followup-input-printed
              (prin1-to-string (plist-get tool-followup-body :input)))
             (warm
              (seq-find
               (lambda (request)
                 (let ((body (plist-get request :body)))
                   (and (plist-get body :previous_response_id)
                        (string-match-p
                         "D2 later ordinary turn"
                         (prin1-to-string (plist-get body :input))))))
               sent-requests))
             (warm-body (plist-get warm :body))
             (warm-input (plist-get warm-body :input))
             (warm-options (plist-get (plist-get warm :projection) :options))
             (warm-input-printed (prin1-to-string warm-input))
             (warm-body-printed (prin1-to-string warm-body))
             (warm-wire-bytes (plist-get warm :wire-bytes))
             (warm-input-item-count (plist-get warm :input-item-count))
             (event-types
              (mapcar (lambda (event) (plist-get event :type))
                      (reverse events)))
             (warm-diagnostics-request
              (seq-find
               (lambda (request)
                 (let ((diagnostics
                        (plist-get (e-backend-request-metadata request)
                                   :diagnostics)))
                   (and (eq (plist-get diagnostics
                                      :websocket-request-mode)
                            'incremental)
                        (= (plist-get diagnostics
                                      :websocket-reuse-count)
                           3))))
               backend-requests))
             (warm-diagnostics
              (plist-get (e-backend-request-metadata
                          warm-diagnostics-request)
                         :diagnostics))
             (warm-finished-activity
              (seq-find
               (lambda (event)
                 (and (eq (plist-get event :event-type)
                          'provider-request-finished)
                      (eq (plist-get
                           (plist-get (plist-get event :payload)
                                      :diagnostics)
                           :websocket-anchor-position)
                          'older)))
               (reverse
                (e-session-activity-events
                 (e-harness-sessions harness) "websocket-composed"))))
             (warm-finished-diagnostics
              (plist-get (plist-get warm-finished-activity :payload)
                         :diagnostics))
             (anchor-ids
              (mapcar
               (lambda (anchor)
                 (plist-get (plist-get anchor :metadata) :response-id))
               (e-session-provider-anchors
                (e-harness-sessions harness) "websocket-composed")))
             (socket current-socket)
             (on-close (plist-get (gethash socket callbacks) :on-close)))
        (should (= send-count 4))
        (should (= open-count 1))
        (should (member 'tool-started event-types))
        (should (member 'tool-finished event-types))
        (should (equal (plist-get tool-followup-body :previous_response_id)
                       "resp-r1"))
        (dolist (marker '("call-composed-inspect"
                          "RAW-COMPOSED-TOOL-RESULT"))
          (should (string-match-p marker tool-followup-input-printed)))
        (should (equal (plist-get warm-body :previous_response_id)
                       "resp-r0"))
        (should (equal (plist-get (plist-get warm-options :provider-anchor)
                                  :metadata)
                       '(:response-id "resp-r0")))
        (should (eq (plist-get warm-diagnostics
                               :websocket-request-mode)
                    'incremental))
        (should (eq (plist-get warm-diagnostics
                               :websocket-anchor-position)
                    'older))
        (should (plist-member warm-diagnostics
                              :websocket-idle-close-seconds))
        (should-not (plist-get warm-diagnostics
                               :websocket-idle-close-seconds))
        (should (eq (plist-get warm-diagnostics
                               :previous-response-id-present)
                    t))
        (should (eq (plist-get warm-diagnostics :websocket-reused) t))
        (should (numberp warm-wire-bytes))
        (should (numberp warm-input-item-count))
        ;; The completed activity is the public projection boundary: both
        ;; adapter-owned scalar diagnostics survive without any ledger data.
        (should warm-finished-activity)
        (should (eq (plist-get warm-finished-diagnostics
                               :websocket-anchor-position)
                    'older))
        (should (plist-member warm-finished-diagnostics
                              :websocket-idle-close-seconds))
        (should-not (plist-get warm-finished-diagnostics
                               :websocket-idle-close-seconds))
        (should (eq (plist-get warm-finished-diagnostics
                               :websocket-request-mode)
                    'incremental))
        (should (eq (plist-get warm-finished-diagnostics
                               :previous-response-id-present)
                    t))
        (should-not (string-match-p "resp-r0"
                                    (prin1-to-string warm-finished-diagnostics)))
        (should-not (string-match-p "resp-r1"
                                    (prin1-to-string warm-finished-diagnostics)))
        (should-not (string-match-p "resp-r2"
                                    (prin1-to-string warm-finished-diagnostics)))
        (dolist (marker '("RAW-COMPOSED-TOOL-RESULT"
                          "call-composed-inspect"))
          (should-not (string-match-p
                       marker
                       (prin1-to-string warm-finished-diagnostics))))
        (should (string-match-p "STABLE-COMPOSED-INSTRUCTIONS"
                                (plist-get warm-body :instructions)))
        (should (string-match-p "CURRENT-COMPOSED-INSTRUCTIONS"
                                (plist-get warm-body :instructions)))
        (should (string-match-p "PROMOTED-COMPOSED-FACT"
                                warm-body-printed))
        (dolist (marker '("D1 inspect durable state"
                          "D2 later ordinary turn"))
          (should (string-match-p marker warm-input-printed)))
        (dolist (marker '("RAW-COMPOSED-REPLAY"
                          "RAW-COMPOSED-TOOL-RESULT"
                          "call-composed-inspect"
                          "provider-replay-item"
                          "resp-r1"
                          "resp-r2"))
          (should-not (string-match-p marker warm-body-printed)))
        (should-not (string-match-p "CURRENT-COMPOSED-INSTRUCTIONS"
                                    warm-input-printed))
        (should-not (member "resp-r1" anchor-ids))
        (should-not (member "resp-r2" anchor-ids))
        (should (member "resp-r0" anchor-ids))
        ;; Lose the socket before another ordinary turn.  The harness may
        ;; still select R0, but the adapter must reconstruct without the
        ;; connection-local response ledger.
        (funcall on-close socket)
        (e-board-e2e-prompt-batch
         harness "websocket-composed" "D3 after socket loss")
        (let* ((recovered (car (last sent-requests)))
               (recovered-body (plist-get recovered :body))
               (recovered-input (plist-get recovered-body :input))
               (recovered-input-printed (prin1-to-string recovered-input))
               (recovered-body-printed (prin1-to-string recovered-body))
               (recovered-wire-bytes (plist-get recovered :wire-bytes))
               (recovered-input-item-count
                (plist-get recovered :input-item-count))
               (recovered-request
                (seq-find
                 (lambda (request)
                   (let ((diagnostics
                          (plist-get (e-backend-request-metadata request)
                                     :diagnostics)))
                     (and (eq (plist-get diagnostics
                                        :websocket-request-mode)
                              'full)
                          (eq (plist-get diagnostics
                                         :websocket-fallback-reason)
                              'new-connection))))
                 backend-requests))
               (recovered-diagnostics
                (plist-get (e-backend-request-metadata recovered-request)
                           :diagnostics)))
          (should (= send-count 5))
          (should (= open-count 2))
          (should-not (plist-member recovered-body
                                     :previous_response_id))
          (should (eq (plist-get recovered-diagnostics
                                 :websocket-request-mode)
                      'full))
          (should (eq (plist-get recovered-diagnostics
                                 :websocket-fallback-reason)
                      'new-connection))
          (should (eq (plist-get recovered-diagnostics
                                 :previous-response-id-present)
                      nil))
          (should (string-match-p "PROMOTED-COMPOSED-FACT"
                                  recovered-body-printed))
          (should (string-match-p "CURRENT-COMPOSED-INSTRUCTIONS"
                                  (plist-get recovered-body :instructions)))
          (dolist (marker '("D0 durable seed"
                            "D1 inspect durable state"
                            "D2 later ordinary turn"
                            "D3 after socket loss"))
            (should (string-match-p marker recovered-input-printed)))
          (dolist (marker '("RAW-COMPOSED-REPLAY"
                            "RAW-COMPOSED-TOOL-RESULT"
                            "call-composed-inspect"
                            "resp-r1"
                            "resp-r2"
                            "CURRENT-COMPOSED-INSTRUCTIONS"))
            (should-not (string-match-p marker recovered-input-printed)))
          (should (numberp recovered-wire-bytes))
          (should (numberp recovered-input-item-count))
          (ert-info
              ((format
                "Composed wire metrics: warm bytes=%d input-items=%d; cold bytes=%d input-items=%d"
                warm-wire-bytes warm-input-item-count
                recovered-wire-bytes recovered-input-item-count))
            (should (< warm-wire-bytes recovered-wire-bytes))
            (should (< warm-input-item-count recovered-input-item-count))))))))

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

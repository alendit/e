;;; e-context-lifetime-e2e-test.el --- Feature 88 normal-turn E2E -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Credential-free, real-owner evidence for the first Feature 88 Slice 6
;; scenario.  The requester is instrumented, but the OpenAI adapter, semantic
;; capability resolver, harness, loop, session, context projection, and
;; ordinary tool registry are all production implementations.

;;; Code:

(require 'ert)
(require 'json)
(require 'e)
(require 'e-capabilities)
(require 'e-context)
(require 'e-context-lifetime)
(require 'e-harness)
(require 'e-harness-base)
(require 'e-openai)
(require 'e-session)
(require 'e-tools)
(require 'e-chat-service)
(require 'e-board-runtime)
(load (expand-file-name "e-board-e2e-support.el"
                       (file-name-directory (or load-file-name
                                                 buffer-file-name)))
      nil nil t)

(defun e-context-lifetime-e2e--provider-anchor-fingerprints (context)
  "Return the semantic anchor projection carried by CONTEXT."
  (plist-get
   (plist-get (plist-get context :options)
              :continuation-projection-identity)
   :provider-anchor-fingerprints))

(defun e-context-lifetime-e2e--sse (&rest events)
  "Return an SSE response containing JSON EVENTS."
  (mapconcat (lambda (event)
               (format "data: %s\n\n" (json-encode event)))
             events
             ""))

(defun e-context-lifetime-e2e--json-body (body)
  "Parse Responses JSON BODY into keyword-plist data."
  (json-parse-string body
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun e-context-lifetime-e2e--count (needle haystack)
  "Return the non-overlapping count of NEEDLE in HAYSTACK."
  (let ((start 0)
        (count 0))
    (while (and (stringp haystack)
                (string-match (regexp-quote needle) haystack start))
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun e-context-lifetime-e2e--store-provider-chain
    (response-items response-id inherited-items literal-input output-items)
  "Store the full provider-visible chain for RESPONSE-ID.

The injected transport models Responses continuation recursively: a response
inherits the chain selected by its previous response id, receives the current
literal input, and then emits OUTPUT-ITEMS.  Instructions are intentionally
not part of this chain because they are request-local in the adapter contract."
  (puthash response-id
           (append (copy-tree inherited-items)
                   (copy-tree literal-input)
                   (copy-tree output-items))
           response-items))

(defun e-context-lifetime-e2e--model-context-text (effective-context)
  "Return the literal model-visible portion of EFFECTIVE-CONTEXT."
  (prin1-to-string
   (list :instructions (plist-get effective-context :instructions)
         :input (plist-get effective-context :input))))

(defun e-context-lifetime-e2e--marker-stats (markers text)
  "Return occurrence and UTF-8 byte statistics for MARKERS in TEXT."
  (mapcar
   (lambda (marker)
     (let ((occurrences (e-context-lifetime-e2e--count marker text)))
       (list (cons 'marker marker)
             (cons 'occurrences occurrences)
             (cons 'bytes
                   (* occurrences
                      (string-bytes
                       (encode-coding-string marker 'utf-8)))))))
   markers))

(defun e-context-lifetime-e2e--request-metrics (request &optional markers)
  "Return bounded scalar metrics for captured provider REQUEST.
MARKERS may be one marker string or a list of raw observation markers.  Input
sizes are measured from the exact JSON body values, and marker statistics are
computed from the effective model-visible input."
  (let* ((literal (plist-get request :literal-input))
         (inherited (plist-get request :inherited-items))
         (effective (plist-get (plist-get request :effective-context)
                               :input))
         (markers (cond ((null markers) nil)
                        ((stringp markers) (list markers))
                        (t markers)))
         (literal-json (json-encode literal))
         (effective-json (json-encode effective))
         (marker-stats
          (e-context-lifetime-e2e--marker-stats markers effective-json))
         (marker-bytes
          (apply #'+ (mapcar (lambda (stat) (alist-get 'bytes stat))
                             marker-stats)))
         (previous (plist-get (plist-get request :parsed)
                              :previous_response_id)))
    (list
     (cons 'literal_input_items (length literal))
     (cons 'literal_input_bytes
           (string-bytes (encode-coding-string literal-json 'utf-8)))
     (cons 'inherited_items (length inherited))
     (cons 'effective_input_items (length effective))
     (cons 'effective_input_bytes
           (string-bytes (encode-coding-string effective-json 'utf-8)))
     (cons 'previous_response_id_present (if previous t :json-false))
     (cons 'previous_response_id previous)
     (cons 'raw_marker_occurrences marker-stats)
     (cons 'raw_marker_bytes marker-bytes)
     ;; Keep the earlier scalar name as an alias for consumers that only
     ;; need the aggregate consumed-observation size.
     (cons 'raw_observation_bytes marker-bytes))))

(defun e-context-lifetime-e2e--tool-register (registry on-call)
  "Register the ordinary normalize-price tool in REGISTRY."
  (e-tools-register
   registry
   :name "normalize-price"
   :description "Normalize one price for Feature 88 credential-free E2E."
   :parameters '(:type "object"
                 :properties (:sku (:type "string"))
                 :required ["sku"])
   :work
   (e-tools-cheap-work
    "e2e.context-lifetime.normalize-price"
    (lambda (_arguments)
      (funcall on-call)
      "OBSERVATION-ONE first-divergence=normalize-price"))))

(defun e-context-lifetime-e2e--prompt-batch (harness session-id prompt)
  "Run PROMPT and retain the raw settled entry for useful failures."
  (e-board-e2e-prompt-async harness session-id prompt)
  (let ((result (e-board-e2e-wait-batch harness session-id)))
    (unless (eq (plist-get result :status) 'done)
      (error "E2E turn failed: %s"
             (or (plist-get result :error)
                 (plist-get result :condition)
                 "unknown error")))
    result))

(defun e-context-lifetime-e2e--prompt-through-harness
    (harness session-id prompt)
  "Submit PROMPT through the normal attached harness/backend path.

This is used after deliberate compaction, when the board source may not yet
have a presentation-side attachment for the newly compacted session head."
  (let* ((binding (e-chat-service-binding harness session-id))
         (attachment (and binding
                          (e-chat-service-binding-attachment binding))))
    (unless attachment
      (error "E2E session has no attached harness port: %s" session-id))
    (e-harness-attached-turn-port-submit-batch
     (e-board-runtime-attachment-turn-port attachment)
     prompt)))

(defun e-context-lifetime-e2e--run-tool-observe-curate
    (&optional erase-p)
  "Run the credential-free tool curation case.

When ERASE-P is non-nil, explicitly erase the ordinary tool-result source;
otherwise its label is omitted from the curation response and its receipt is
preserved for the next turn.

This intentionally drives the real OpenAI Responses adapter with an injected
transport.  The transport is the only fake boundary: all context, capability,
loop, session, and ordinary tool behavior remains production behavior."
  (let* ((process-environment
          (cons "E_CONTEXT_LIFETIME_TOKEN=credential-free-test"
                process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-context-lifetime-shadow-projection-enabled t)
         (e-openai-model-providers
          '((context-lifetime-e2e
             :name "Feature 88 Context Lifetime E2E"
             :base-url "https://api.openai.com/v1"
             :env-key "E_CONTEXT_LIFETIME_TOKEN"
             :wire-api responses
             :responses-transport http
             :response-store t
             :responses-context-layout developer-input
             :include-encrypted-reasoning t
             :observation-delivery request-local-replaceable
             :continuation t
             :requires-openai-auth nil)))
         (canvas "CANVAS-ONE")
         (requests nil)
         (response-items (make-hash-table :test #'equal))
         (effective-contexts nil)
         (request-projections nil)
         (request-count 0)
         (tool-count 0)
         (curation-wire-arguments nil)
         (curation-canvas-label nil)
         (curation-tool-label nil)
         (captured-tool-frame nil)
         (harness nil)
         (stable-provider
          (e-context-provider-create
           :name 'e2e-stable-context
           :cache-placement 'stable-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content "STABLE-CONTEXT")))))
         (canvas-provider
          (e-context-provider-create
           :name 'e2e-canvas
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content canvas)))))
         (capability
          (e-capability-create
           :id 'context-lifetime-e2e
           :instructions "C"
           :context-providers (list stable-provider canvas-provider)
           :tools
           (list
            (lambda (registry)
              (e-context-lifetime-e2e--tool-register
               registry
               (lambda () (setq tool-count (1+ tool-count))))))))
         (request-function
          (cl-function
           (lambda (&key url headers body)
            (ignore headers)
            (let* ((parsed (e-context-lifetime-e2e--json-body body))
                   (previous-response-id
                    (plist-get parsed :previous_response_id))
                   (literal-input (copy-tree (plist-get parsed :input)))
                   (inherited-items
                    (copy-tree
                     (and previous-response-id
                          (gethash previous-response-id response-items))))
                   (effective-context
                    (list :instructions (plist-get parsed :instructions)
                          :literal-input literal-input
                          :inherited-items inherited-items
                          :input (append inherited-items literal-input))))
              (setq effective-contexts
                    (append effective-contexts (list effective-context)))
              (setq requests
                    (append requests
                            (list (list :url url
                                        :body body
                                        :parsed parsed
                                        :literal-input literal-input
                                        :inherited-items inherited-items
                                        :effective-context
                                        effective-context))))
            (setq request-count (1+ request-count))
            (pcase request-count
              (1
               (e-context-lifetime-e2e--store-provider-chain
                response-items
                "resp-A"
                inherited-items
                literal-input
                (list
                 (list :type "reasoning"
                       :id "reasoning-one"
                       :encrypted_content "REPLAY-ONE"
                       :summary [])
                 (list :type "function_call"
                       :call_id "call-normalize-price"
                       :name "normalize-price"
                       :arguments "{\"stated_purpose\":\"Normalize the price.\",\"sku\":\"SKU-ONE\"}")))
               (e-context-lifetime-e2e--sse
                '((type . "response.output_item.done")
                  (item . ((type . "reasoning")
                           (id . "reasoning-one")
                           (encrypted_content . "REPLAY-ONE")
                           (summary . []))))
                '((type . "response.output_item.done")
                  (item . ((type . "function_call")
                           (call_id . "call-normalize-price")
                           (name . "normalize-price")
                           (arguments . "{\"stated_purpose\":\"Normalize the price.\",\"sku\":\"SKU-ONE\"}"))))
                '((type . "response.completed")
                  (response . ((id . "resp-A")
                               (status . "completed"))))))
              (2
               (let* ((sources
                       (e-context-lifetime-frame-curation-sources
                        captured-tool-frame))
                      (tool-source
                       (seq-find
                        (lambda (candidate)
                          (and (stringp (plist-get candidate :value))
                               (string-prefix-p
                                "OBSERVATION-ONE"
                                (plist-get candidate :value))))
                        sources))
                      (tool-label (and tool-source
                                       (plist-get tool-source :label)))
                      (canvas-source
                       (seq-find
                        (lambda (candidate)
                          (and (stringp (plist-get candidate :value))
                               (equal (plist-get candidate :value)
                                      canvas)))
                        sources))
                      (canvas-label (and canvas-source
                                         (plist-get canvas-source :label)))
                      (curation-arguments
                       (json-encode
                        (append
                         (list
                          :keep (vector canvas-label)
                          :summaries (vector))
                         (when erase-p
                           (list :erase (vector tool-label)))))))
                 (unless (and tool-source tool-label
                              canvas-source canvas-label)
                   (error "Follow-up lacks trusted curation frontier"))
                 (unless (/= tool-label canvas-label)
                   (error "Tool and canvas labels unexpectedly match: %s"
                          tool-label))
                 (setq curation-canvas-label canvas-label)
                 (setq curation-tool-label tool-label)
                 (setq curation-wire-arguments curation-arguments)
                   (e-context-lifetime-e2e--store-provider-chain
                    response-items
                    "resp-B"
                    inherited-items
                    literal-input
                     (list
                     (list :type "function_call"
                           :call_id "call-context-curate"
                           :name "context-curate"
                           :arguments curation-arguments)
                     (list :type "message"
                           :role "assistant"
                           :content (list (list :type "output_text"
                                                 :text "B complete")))))
                   (e-context-lifetime-e2e--sse
                    (list (cons 'type "response.output_item.done")
                          (cons 'item
                                (list (cons 'type "function_call")
                                      (cons 'call_id "call-context-curate")
                                      (cons 'name "context-curate")
                                      (cons 'arguments curation-arguments))))
                    '((type . "response.output_text.done")
                      (text . "B complete"))
                    '((type . "response.completed")
                      (response . ((id . "resp-B")
                                   (status . "completed")))))))
              (3
               (e-context-lifetime-e2e--store-provider-chain
                response-items
                "resp-C"
                inherited-items
                literal-input
                (list
                 (list :type "message"
                       :role "assistant"
                       :content (list (list :type "output_text"
                                             :text "C complete")))))
               (e-context-lifetime-e2e--sse
                '((type . "response.output_text.done")
                  (text . "C complete"))
                '((type . "response.completed")
                  (response . ((id . "resp-C")
                               (status . "completed"))))))))))))
    (setq harness
          (e-openai-create-harness
           :provider 'context-lifetime-e2e
           :model "gpt-e2e"
           :request-function request-function))
    (dolist (base-capability
             (e-layer-capabilities (e-harness-base-layer-create)))
      (e-harness-activate-capability harness base-capability))
    (e-harness-activate-capability harness capability)
    (let ((original-body (symbol-function 'e-openai-codex-request-body))
          (original-frame
           (symbol-function 'e-harness-context-lifetime-tool-observation-frame)))
      (cl-letf (((symbol-function 'e-openai-codex-request-body)
                 (lambda (&rest args)
                   (let ((body (apply original-body args)))
                     (setq request-projections
                           (append request-projections
                                   (list (list :messages
                                               (copy-tree (plist-get args :messages))
                                               :options
                                               (copy-tree (plist-get args :options))
                                               :body body))))
                     body)))
                ((symbol-function
                  'e-harness-context-lifetime-tool-observation-frame)
                 (lambda (&rest args)
                   (let ((frame (apply original-frame args)))
                     ;; Capture the core-produced frame, but return it
                     ;; untouched to the normal harness callback.
                     (setq captured-tool-frame frame)
                     frame))))
        (e-board-e2e-create-session harness :id "context-lifetime-e2e")
        (e-context-lifetime-e2e--prompt-batch
         harness "context-lifetime-e2e" "D1")
        (let* ((first-request (nth 0 requests))
               (follow-up-request (nth 1 requests))
               (first-body (plist-get first-request :body))
               (follow-up-effective
                (plist-get follow-up-request :effective-context))
               (follow-up-effective-input
                (plist-get follow-up-effective :input))
               (follow-up-effective-text
                (e-context-lifetime-e2e--model-context-text
                 follow-up-effective))
               (follow-up-options
                (plist-get (nth 1 request-projections) :options))
               (follow-up-input
                (plist-get (plist-get follow-up-request :parsed) :input)))
          (should (= request-count 2))
          (should (= (length effective-contexts) 2))
          (should (= tool-count 1))
          (should (string-match-p "C" first-body))
          (should (string-match-p "D1" first-body))
          (should (string-match-p "CANVAS-ONE" first-body))
          (should (equal
                   (plist-get
                    (plist-get (plist-get follow-up-options :provider-anchor)
                               :metadata)
                    :response-id)
                   "resp-A"))
          (should (equal (plist-get (plist-get follow-up-request :parsed)
                                    :previous_response_id)
                         "resp-A"))
          (should (= (length
                      (seq-filter
                       (lambda (item)
                         (equal (plist-get item :type) "function_call"))
                       follow-up-effective-input))
                     1))
          (should (= (e-context-lifetime-e2e--count
                      "REPLAY-ONE" follow-up-effective-text)
                     1))
          (should (= (e-context-lifetime-e2e--count
                      "OBSERVATION-ONE" follow-up-effective-text)
                     1))
          ;; The matching continuation inherits the original durable prompt
          ;; through the recursive provider chain, while the tool bundle is
          ;; only introduced at this request frontier.
          (should (string-match-p "D1" follow-up-effective-text))
          (should (= (length
                      (seq-filter
                       (lambda (item)
                         (equal (plist-get item :type)
                                "function_call_output"))
                       follow-up-effective-input))
                     1))
          (should (= (length
                      (seq-filter
                       (lambda (item)
                         (equal (plist-get item :type)
                                "function_call_output"))
                       follow-up-input))
                     1))))
        (e-context-lifetime-e2e--prompt-batch
         harness "context-lifetime-e2e" "D2")
        (let* ((next-request (nth 2 requests))
               (next-body (plist-get next-request :body))
               (next-effective
                (plist-get next-request :effective-context))
               (next-effective-input
                (plist-get next-effective :input))
               (next-effective-instructions
                (plist-get next-effective :instructions))
               (next-effective-text
                (e-context-lifetime-e2e--model-context-text
                 next-effective))
               (next-effective-input-text
                (prin1-to-string next-effective-input))
               (next-input-text
                (prin1-to-string
                 (plist-get (plist-get next-request :parsed) :input)))
               (curations
                (e-session-context-promotions
                 (e-harness-sessions harness)
                 "context-lifetime-e2e"))
               (curation-record
                (plist-get (car curations) :context-record))
               (receipt-event
                (seq-find
                 (lambda (entry)
                   (let ((receipt
                          (plist-get (plist-get entry :payload) :receipt)))
                     (and (memq (plist-get entry :event-type)
                                '(tool-finished "tool-finished"))
                          (listp receipt)
                          (equal (plist-get receipt :tool-call-id)
                                 "call-normalize-price"))))
                 (e-harness-session-activity-events
                  harness "context-lifetime-e2e")))
               (receipt
                (and receipt-event
                     (plist-get receipt-event :payload)
                     (plist-get (plist-get receipt-event :payload) :receipt)))
               (details-uri (and receipt (plist-get receipt :details-uri)))
               (receipt-input
                (seq-find
                 (lambda (item)
                   (and (string-prefix-p "- " item)
                        (string-match-p "tool_call_id" item)
                        (string-match-p "details_uri" item)
                        (string-match-p "call-normalize-price" item)))
                 (split-string (or next-effective-instructions "")
                               "\n" t)))
               (receipt-input-text
                receipt-input)
               (erased-tool-call-ids
                (e-session-erased-tool-call-ids
                 (e-harness-sessions harness) "context-lifetime-e2e"))
               (current-path
                (e-session-current-path
                 (e-harness-sessions harness)
                 "context-lifetime-e2e"))
               (tool-result-entry
                (seq-find
                 (lambda (entry)
                   (and (eq (plist-get entry :type) 'message)
                        (eq (plist-get entry :role) 'tool)
                        (string-match-p
                         "OBSERVATION-ONE"
                         (prin1-to-string entry))))
                 current-path))
               (anchors
                (e-session-provider-anchors
                 (e-harness-sessions harness)
                 "context-lifetime-e2e")))
          (should (= request-count 3))
          (should (= (length effective-contexts) 3))
          (should-not
           (plist-get (plist-get next-request :parsed)
                      :previous_response_id))
          (should (equal
                   (plist-get
                    (plist-get (plist-get next-request :parsed) :reasoning)
                    :summary)
                   "auto"))
          (should receipt-event)
          (should (equal (plist-get receipt :tool-call-id)
                         "call-normalize-price"))
          (should (equal (plist-get receipt :tool) "normalize-price"))
          (should (equal (plist-get receipt :stated-purpose)
                         "Normalize the price."))
          (should (stringp details-uri))
          (should (e-session-tmp-reference-available-p
                   harness "context-lifetime-e2e" details-uri))
          (should (string-match-p "CANVAS-ONE" next-effective-text))
          (dolist (marker '("OBSERVATION-ONE" "REPLAY-ONE" "resp-A" "resp-B"))
            (should-not (string-match-p marker next-effective-text))
            (should-not (string-match-p marker next-input-text)))
          (if erase-p
              (progn
                (should (equal erased-tool-call-ids
                               (list "call-normalize-price")))
                (should-not receipt-input)
                ;; The registered tool schema remains in the request envelope;
                ;; these assertions inspect the model-visible input frontier.
                (dolist (marker '("OBSERVATION-ONE" "call-normalize-price"
                                  "normalize-price" "Normalize the price."
                                  "tmp://tool-invocations/" "tool receipt"
                                  "tool receipts" "earlier tool receipt"
                                  "earlier tool receipts" "receipt omitted"
                                  "receipts omitted"))
                  (should-not (string-match-p marker next-effective-text))
                  (should-not (string-match-p marker next-input-text)))
                (should-not (string-match-p "call-normalize-price" next-body))
                (should-not (string-match-p "Normalize the price." next-body))
                (should-not (string-match-p "tmp://tool-invocations/"
                                            next-body)))
            (progn
              (should-not (member "call-normalize-price"
                                  erased-tool-call-ids))
              (should receipt-input)
              (dolist (marker '("call-normalize-price" "normalize-price"
                                "Normalize the price." "tmp://tool-invocations/"))
                (should (string-match-p marker receipt-input-text)))
              (let ((non-receipt-instructions
                     (replace-regexp-in-string
                      (regexp-quote receipt-input-text) ""
                      (or next-effective-instructions "") t t)))
                (dolist (marker '("call-normalize-price" "normalize-price"
                                  "Normalize the price."
                                  "tmp://tool-invocations/"))
                  (should-not (string-match-p marker
                                              non-receipt-instructions))))))
          (should (string-match-p "normalize-price" next-body))
          (dolist (marker '("OBSERVATION-ONE" "REPLAY-ONE" "resp-A" "resp-B"))
            (should-not (string-match-p marker next-body)))
          (should (= (length curations) 1))
          (let* ((items (plist-get curation-record :items))
                 (item (car items)))
            (should (= (length items) 1))
            (should (eq (plist-get item :kind) 'exact))
            (should (equal (plist-get item :value) "CANVAS-ONE"))
            (should (= (length (plist-get item :source-observation-ids)) 1))
            (should (= (length (plist-get item :source-refs)) 1))
            (should (= (length (plist-get item :source-fingerprints)) 1)))
          (should tool-result-entry)
          ;; The wire effect carries only model-facing labels and text.  Core
          ;; resolves its trusted provenance from the live frame at commit.
          (let ((wire (e-context-lifetime-e2e--json-body
                       curation-wire-arguments)))
            (should (equal (plist-get wire :keep)
                           (list curation-canvas-label)))
            (should (null (plist-get wire :summaries)))
            (if erase-p
                (should (equal (plist-get wire :erase)
                               (list curation-tool-label)))
              (should-not (plist-member wire :erase)))
            (should-not (plist-member wire :drop))
            (should-not (plist-member wire :frame))
            (should-not (plist-member wire :observation))
            (should-not (plist-member wire :ref))
            (should-not (plist-member wire :fingerprint)))
          (dolist (field '("source-refs" "source_refs" "fingerprints"
                           "source-fingerprints" "source_fingerprints"))
            (should-not (string-match-p field curation-wire-arguments)))
          ;; The immediate response id and the contaminated B descendant are
          ;; current-follow-up artifacts, never durable anchors.
          (should-not
           (seq-some
            (lambda (anchor)
              (member (plist-get (plist-get anchor :metadata) :response-id)
                      '("resp-A" "resp-B")))
            anchors))))))

(ert-deftest e-context-lifetime-e2e-test-tool-observe-curate-omission-preserves-receipt ()
  "Omitting a tool source drops raw content while preserving its receipt."
  (e-context-lifetime-e2e--run-tool-observe-curate))

(ert-deftest e-context-lifetime-e2e-test-tool-observe-curate-erase-suppresses-receipt ()
  "Explicitly erasing a tool source suppresses its receipt projection."
  (e-context-lifetime-e2e--run-tool-observe-curate t))

(ert-deftest e-context-lifetime-e2e-test-canvas-replacement-profiles ()
  "Exercise canvas replacement and anchor safety through real OpenAI profiles.

The four profiles deliberately differ only in the semantic continuation and
observation-delivery claims resolved by the OpenAI adapter.  The transport is
stateful so the assertions inspect effective model context, including
inherited Responses items, rather than only the literal request body."
  (let ((process-environment
         (cons "E_CONTEXT_LIFETIME_TOKEN=credential-free-test"
               process-environment))
        (e-harness-auto-compaction-enabled nil)
        (e-context-lifetime-shadow-projection-enabled t)
        (profiles
         '((none nil nil none)
           (linear t nil linear)
           (branchable branchable nil branchable)
           (replaceable t request-local-replaceable linear)))
        (baseline-semantic-signature nil))
    (dolist (spec profiles)
      (pcase-let ((`(,label ,continuation ,delivery ,expected-mode) spec))
        (let* ((provider-id (intern (format "canvas-%s-e2e" label)))
               (session-id (format "canvas-%s" label))
               (profile
                (append
                 (list :name (format "Canvas %s E2E" label)
                       :base-url (format "https://canvas-%s.example.test/v1"
                                         label)
                       :env-key "E_CONTEXT_LIFETIME_TOKEN"
                       :wire-api 'responses
                       :responses-transport 'http
                       :response-store t
                       :responses-context-layout 'developer-input
                       :include-encrypted-reasoning t
                       :continuation continuation
                       :requires-openai-auth nil)
                 (when delivery
                   (list :observation-delivery delivery))))
               (e-openai-model-providers
                (list (cons provider-id profile)))
               (stable-value "STABLE-C-ORIGINAL")
               (canvas nil)
               (requests nil)
               (response-items (make-hash-table :test #'equal))
               (request-projections nil)
               (request-count 0)
               (stable-provider
                (e-context-provider-create
                 :name (intern (format "e2e-stable-%s" label))
                 :cache-placement 'stable-context
                 :build (lambda (&rest _)
                          (list (list :role 'system :content stable-value)))))
               (canvas-provider
                (e-context-provider-create
                 :name (intern (format "e2e-canvas-%s" label))
                 :cache-placement 'dynamic-context
                 :build (lambda (&rest _)
                          (when canvas
                            (list (list :role 'system :content canvas))))))
               (capability
                (e-capability-create
                 :id (intern (format "canvas-capability-%s" label))
                 :instructions "C-MATRIX"
                 :context-providers (list stable-provider canvas-provider)))
               (request-function
                (cl-function
                 (lambda (&key url headers body)
                   (ignore headers)
                   (let* ((parsed (e-context-lifetime-e2e--json-body body))
                          (previous-response-id
                           (plist-get parsed :previous_response_id))
                          (literal-input
                           (copy-tree (plist-get parsed :input)))
                          (inherited-items
                           (copy-tree
                            (and previous-response-id
                                 (gethash previous-response-id
                                          response-items))))
                          (effective-context
                           (list :instructions (plist-get parsed :instructions)
                                 :literal-input literal-input
                                 :inherited-items inherited-items
                                 :input (append inherited-items
                                                literal-input)))
                          (response-id
                           (format "resp-%s-%d" label
                                   (1+ request-count))))
                     (setq requests
                           (append requests
                                   (list (list :url url
                                               :body body
                                               :parsed parsed
                                               :literal-input literal-input
                                               :inherited-items inherited-items
                                               :effective-context
                                               effective-context))))
                     (setq request-count (1+ request-count))
                     (e-context-lifetime-e2e--store-provider-chain
                      response-items
                      response-id
                      inherited-items
                      literal-input
                      (list
                       (list :type "message"
                             :role "assistant"
                             :content
                             (list (list :type "output_text"
                                         :text
                                         (format "ANSWER-MATRIX-%d"
                                                 request-count))))))
                     (e-context-lifetime-e2e--sse
                      (list (cons 'type "response.output_text.done")
                            (cons 'text
                                  (format "ANSWER-MATRIX-%d"
                                          request-count)))
                      (list (cons 'type "response.completed")
                            (cons 'response
                                  (list (cons 'id response-id)
                                     (cons 'status "completed")))))))))
               (harness
                (e-openai-create-harness
                 :provider provider-id
                 :model "gpt-e2e"
                 :request-function request-function)))
          (e-harness-activate-capability harness capability)
          (let ((original-body
                 (symbol-function 'e-openai-codex-request-body)))
            (cl-letf (((symbol-function 'e-openai-codex-request-body)
                       (lambda (&rest args)
                         (let ((body (apply original-body args)))
                           (setq request-projections
                                 (append request-projections
                                         (list (list
                                                :messages
                                                (copy-tree
                                                 (plist-get args :messages))
                                                :options
                                                (copy-tree
                                                 (plist-get args :options))
                                                :body body))))
                           body))))
              (e-board-e2e-create-session harness :id session-id)
              ;; Establish a clean anchor before any canvas observation.
              (e-context-lifetime-e2e--prompt-batch
               harness session-id "MATRIX-ONE")
              (setq canvas "CANVAS-ONE")
              (e-context-lifetime-e2e--prompt-batch
               harness session-id "MATRIX-TWO")
              (setq canvas "CANVAS-TWO")
              (e-context-lifetime-e2e--prompt-batch
               harness session-id "MATRIX-THREE")
              (setq stable-value "STABLE-C-CHANGED")
              (e-context-lifetime-e2e--prompt-batch
               harness session-id "MATRIX-FOUR")))
          (let* ((ordered requests)
                 (projections request-projections)
                 (canvas-one-request (nth 1 ordered))
                 (canvas-two-request (nth 2 ordered))
                 (stable-change-request (nth 3 ordered))
                 (canvas-one-text
                  (e-context-lifetime-e2e--model-context-text
                   (plist-get canvas-one-request :effective-context)))
                 (canvas-two-text
                  (e-context-lifetime-e2e--model-context-text
                   (plist-get canvas-two-request :effective-context)))
                 (stable-change-text
                  (e-context-lifetime-e2e--model-context-text
                   (plist-get stable-change-request :effective-context)))
                 (canvas-one-options
                  (plist-get (nth 1 projections) :options))
                 (canvas-two-options
                  (plist-get (nth 2 projections) :options))
                 (stable-change-options
                  (plist-get (nth 3 projections) :options))
                 (capabilities (plist-get canvas-one-options
                                           :context-capabilities))
                 (semantic-signature
                  (list
                   (list (e-context-lifetime-e2e--count
                          "C-MATRIX" canvas-one-text)
                         (e-context-lifetime-e2e--count
                          "STABLE-C-ORIGINAL" canvas-one-text)
                         (e-context-lifetime-e2e--count
                          "CANVAS-ONE" canvas-one-text)
                         (e-context-lifetime-e2e--count
                          "CANVAS-TWO" canvas-one-text))
                   (list (e-context-lifetime-e2e--count
                          "C-MATRIX" canvas-two-text)
                         (e-context-lifetime-e2e--count
                          "STABLE-C-ORIGINAL" canvas-two-text)
                         (e-context-lifetime-e2e--count
                          "CANVAS-ONE" canvas-two-text)
                         (e-context-lifetime-e2e--count
                          "CANVAS-TWO" canvas-two-text))
                   (list (e-context-lifetime-e2e--count
                          "C-MATRIX" stable-change-text)
                         (e-context-lifetime-e2e--count
                          "STABLE-C-ORIGINAL" stable-change-text)
                         (e-context-lifetime-e2e--count
                          "STABLE-C-CHANGED" stable-change-text))))
                 (anchors
                  (e-session-provider-anchors
                   (e-harness-sessions harness) session-id))
                 (anchor-ids
                  (mapcar (lambda (anchor)
                            (plist-get (plist-get anchor :metadata)
                                       :response-id))
                          anchors))
                 (first-response-id (format "resp-%s-1" label))
                 (second-response-id (format "resp-%s-2" label)))
            (should (= request-count 4))
            (should (eq (plist-get capabilities :continuation)
                        expected-mode))
            ;; The provider contract permits a kind-scoped delivery map;
            ;; inspect the semantic kind used by this scenario rather than
            ;; depending on the representation of that map.
            (should (eq
                     (e-backend-observation-delivery-for-kind
                      capabilities 'current-state)
                     (if delivery
                         'request-local-replaceable
                       'inherited)))
            (if delivery
                (progn
                  (should (eq
                           (e-backend-observation-delivery-for-kind
                            capabilities 'current-state)
                           'request-local-replaceable))
                  (should (eq
                           (e-backend-observation-delivery-for-kind
                            capabilities 'dynamic-context)
                           'request-local-replaceable))
                  ;; A canvas claim never authorizes dropping an inherited
                  ;; tool observation.
                  (should (eq
                           (e-backend-observation-delivery-for-kind
                            capabilities 'tool-result)
                           'inherited)))
              (dolist (kind '(current-state dynamic-context tool-result))
                (should (eq
                         (e-backend-observation-delivery-for-kind
                          capabilities kind)
                         'inherited))))
            ;; The effective request always retains the stable semantic prefix
            ;; and the current prompt, while replacement changes only the
            ;; current-state value.
            (dolist (text (list canvas-one-text canvas-two-text
                                stable-change-text))
              (should (string-match-p "C-MATRIX" text))
              (should (string-match-p "MATRIX-" text)))
            (should (string-match-p "CANVAS-ONE" canvas-one-text))
            (should-not (string-match-p "CANVAS-TWO" canvas-one-text))
            (should (string-match-p "CANVAS-TWO" canvas-two-text))
            (should-not (string-match-p "CANVAS-ONE" canvas-two-text))
            (should (string-match-p "STABLE-C-CHANGED"
                                    stable-change-text))
            (should-not (string-match-p "STABLE-C-ORIGINAL"
                                        stable-change-text))
            (if baseline-semantic-signature
                (should (equal semantic-signature
                               baseline-semantic-signature))
              (setq baseline-semantic-signature semantic-signature))
            ;; The stable change fences every prior anchor.  In particular,
            ;; no profile may continue from a response whose stable prefix is
            ;; no longer the current one.
            (should-not (plist-get stable-change-options :provider-anchor))
            (should (plist-get stable-change-options
                               :provider-anchor-invalidation-reason))
            (pcase label
              ('none
               (should-not (plist-get canvas-one-options :provider-anchor))
               (should-not (plist-get canvas-two-options :provider-anchor))
               (should-not anchor-ids))
              ('linear
               (should-not (plist-get canvas-one-options :provider-anchor))
               (should-not (plist-get canvas-two-options :provider-anchor))
               (should (equal anchor-ids (list first-response-id))))
              ('branchable
               (should (equal
                        (plist-get
                         (plist-get
                          (plist-get canvas-one-options :provider-anchor)
                          :metadata)
                         :response-id)
                        first-response-id))
               (should (equal
                        (plist-get
                         (plist-get
                          (plist-get canvas-two-options :provider-anchor)
                          :metadata)
                         :response-id)
                        first-response-id))
               (should (equal anchor-ids (list first-response-id))))
              ('replaceable
               (should (equal
                        (plist-get
                         (plist-get
                          (plist-get canvas-one-options :provider-anchor)
                          :metadata)
                         :response-id)
                        first-response-id))
               (should (equal
                        (plist-get
                         (plist-get
                          (plist-get canvas-two-options :provider-anchor)
                          :metadata)
                         :response-id)
                        second-response-id))
              (should (equal anchor-ids
                              (list first-response-id second-response-id
                                    (format "resp-%s-3" label)
                                    (format "resp-%s-4" label))))))))))))

(defun e-context-lifetime-e2e--run-multi-tool-profile
    (label continuation delivery &optional compaction-request-function)
  "Run the real multi-tool frontier for profile LABEL.

CONTINUATION and DELIVERY are profile declarations, not capability overrides.
Return captured effective requests, projection options, tool arguments, and
persisted anchor response ids for the caller's semantic assertions."
  (let* ((process-environment
          (cons "E_CONTEXT_LIFETIME_TOKEN=credential-free-test"
                process-environment))
         (e-harness-auto-compaction-enabled nil)
         (e-context-lifetime-shadow-projection-enabled t)
         (provider-id (intern (format "multi-tool-%s-e2e" label)))
         (session-id (format "multi-tool-%s" label))
         (profile
          (append
           (list :name (format "Multi tool %s E2E" label)
                 :base-url
                 (if compaction-request-function
                     e-openai-api-default-base-url
                   (format "https://multi-tool-%s.example.test/v1"
                           label))
                 :env-key "E_CONTEXT_LIFETIME_TOKEN"
                 :wire-api 'responses
                 :responses-transport 'http
                 :response-store t
                 :responses-context-layout 'developer-input
                 :include-encrypted-reasoning t
                 :continuation continuation
                 :provider-compaction
                 (when compaction-request-function 'opaque)
                 :requires-openai-auth nil)
           (when delivery
             (list :observation-delivery delivery))))
         (e-openai-model-providers (list (cons provider-id profile)))
         (canvas nil)
         (requests nil)
         (response-items (make-hash-table :test #'equal))
         (request-projections nil)
         (request-count 0)
         (compaction-mode nil)
         (tool-calls nil)
         (harness nil)
         (final-context nil)
         (stable-provider
          (e-context-provider-create
           :name (intern (format "multi-tool-stable-%s" label))
           :cache-placement 'stable-context
           :build (lambda (&rest _)
                    (list (list :role 'system
                                :content "MULTI-TOOL-STABLE")))))
         (canvas-provider
          (e-context-provider-create
           :name (intern (format "multi-tool-canvas-%s" label))
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (when canvas
                      (list (list :role 'system :content canvas))))))
         (tool-register
          (lambda (registry name result marker)
            (e-tools-register
             registry
             :name name
             :description (format "Return %s for Feature 88 E2E." marker)
             :parameters '(:type "object"
                           :properties (:target (:type "string"))
                           :required ["target"])
             :work
              (e-tools-cheap-work
              (format "e2e.context-lifetime.%s" name)
              (lambda (arguments)
                (setq tool-calls
                      (append tool-calls
                              (list (list :name name :arguments arguments))))
                ;; Exercise the existing atomic refresh-context path from an
                ;; ordinary streamed tool result.  Only the proven
                ;; request-local replacement profile changes the canvas; the
                ;; other matrix profiles retain their normal tool result
                ;; lifecycle.
                (if (and (eq label 'replaceable)
                         (equal name "inspect-one"))
                    (progn
                      (setq canvas "CANVAS-TWO")
                      (e-tools-result-create
                       (plist-get (e-tools-current-context) :tool-call)
                       'ok
                       result
                       '(:refresh-context t)))
                  result))))))
         (capability
          (e-capability-create
           :id (intern (format "multi-tool-capability-%s" label))
           :instructions "MULTI-TOOL-C"
           :context-providers (list stable-provider canvas-provider)
           :tools
           (list
            (lambda (registry)
              (funcall tool-register registry
                       "inspect-one" "TOOL-RESULT-ONE" "TOOL-RESULT-ONE")
              (funcall tool-register registry
                       "inspect-two" "TOOL-RESULT-TWO" "TOOL-RESULT-TWO")))))
         (request-function
          (cl-function
           (lambda (&key url headers body)
             (ignore headers)
             (let* ((parsed (e-context-lifetime-e2e--json-body body))
                    (previous-response-id
                     (plist-get parsed :previous_response_id))
                    (literal-input (copy-tree (plist-get parsed :input)))
                    (inherited-items
                     (copy-tree
                      (and previous-response-id
                           (gethash previous-response-id response-items))))
                    (effective-context
                     (list :instructions (plist-get parsed :instructions)
                           :literal-input literal-input
                           :inherited-items inherited-items
                           :input (append inherited-items literal-input)))
                    (next-response-id
                     (pcase (1+ request-count)
                       (1 (format "resp-%s-seed" label))
                       (2 (format "resp-%s-A" label))
                       (3 (format "resp-%s-B" label))
                       (_ (format "resp-%s-later" label)))))
               (setq requests
                     (append requests
                             (list (list :url url
                                         :body body
                                         :parsed parsed
                                         :literal-input literal-input
                                         :inherited-items inherited-items
                                         :effective-context
                                         effective-context
                                         :compaction compaction-mode))))
               (setq request-count (1+ request-count))
               (if compaction-mode
                   (e-context-lifetime-e2e--sse
                    '((type . "response.output_text.done")
                      (text . "PERF-COMPACTION-SUMMARY"))
                    '((type . "response.completed")
                      (response . ((id . "perf-compaction")
                                   (status . "completed"))))))
                 (pcase request-count
                 (1
                  (e-context-lifetime-e2e--store-provider-chain
                   response-items
                   next-response-id
                   inherited-items
                   literal-input
                   (list
                    (list :type "message"
                          :role "assistant"
                          :content
                          (list (list :type "output_text"
                                      :text "SEED-MULTI")))))
                  (e-context-lifetime-e2e--sse
                   '((type . "response.output_text.done")
                     (text . "SEED-MULTI"))
                   (list (cons 'type "response.completed")
                         (cons 'response
                               (list (cons 'id next-response-id)
                                     (cons 'status "completed"))))))
                 (2
                  ;; A contains the whole provider-side call/replay prefix.
                  ;; Its id is eligible only for the immediate result request.
                  (e-context-lifetime-e2e--store-provider-chain
                   response-items
                   next-response-id
                   inherited-items
                   literal-input
                   (list
                    (list :type "reasoning"
                          :id "reasoning-multi"
                          :encrypted_content "REPLAY-MULTI"
                          :summary [])
                    (list :type "function_call"
                          :call_id "call-one"
                          :name "inspect-one"
                          :arguments "{\"stated_purpose\":\"Inspect the first value.\",\"target\":\"ONE\"}")
                    (list :type "function_call"
                          :call_id "call-two"
                          :name "inspect-two"
                          :arguments "{\"stated_purpose\":\"Inspect the second value.\",\"target\":\"TWO\"}")))
                  (e-context-lifetime-e2e--sse
                   '((type . "response.output_item.done")
                     (item . ((type . "reasoning")
                              (id . "reasoning-multi")
                              (encrypted_content . "REPLAY-MULTI")
                              (summary . []))))
                   '((type . "response.output_item.done")
                     (item . ((type . "function_call")
                              (call_id . "call-one")
                              (name . "inspect-one")
                              (arguments . "{\"stated_purpose\":\"Inspect the first value.\",\"target\":\"ONE\"}"))))
                   '((type . "response.output_item.done")
                     (item . ((type . "function_call")
                              (call_id . "call-two")
                              (name . "inspect-two")
                              (arguments . "{\"stated_purpose\":\"Inspect the second value.\",\"target\":\"TWO\"}"))))
                   (list (cons 'type "response.completed")
                         (cons 'response
                               (list (cons 'id next-response-id)
                                     (cons 'status "completed"))))))
                 (3
                  ;; The ordinary streamed tool lifecycle has completed both
                  ;; calls before this matching response is admitted.
                  (e-context-lifetime-e2e--store-provider-chain
                   response-items
                   next-response-id
                   inherited-items
                   literal-input
                   (list
                    (list :type "message"
                          :role "assistant"
                          :content
                          (list (list :type "output_text"
                                      :text "B-MULTI-FINAL")))))
                  (e-context-lifetime-e2e--sse
                   '((type . "response.output_text.done")
                     (text . "B-MULTI-FINAL"))
                   (list (cons 'type "response.completed")
                         (cons 'response
                               (list (cons 'id next-response-id)
                                     (cons 'status "completed"))))))
                 (_
                  (e-context-lifetime-e2e--store-provider-chain
                   response-items
                   next-response-id
                   inherited-items
                   literal-input
                   (list
                    (list :type "message"
                          :role "assistant"
                          :content
                          (list (list :type "output_text"
                                      :text "LATER-MULTI")))))
                  (e-context-lifetime-e2e--sse
                   '((type . "response.output_text.done")
                     (text . "LATER-MULTI"))
                   (list (cons 'type "response.completed")
                         (cons 'response
                               (list (cons 'id next-response-id)
                                     (cons 'status "completed")))))))))))
         (harness
          (e-openai-create-harness
           :provider provider-id
           :model "gpt-e2e"
           :request-function request-function
           :compaction-request-function compaction-request-function)))
    (e-harness-activate-capability harness capability)
    (let ((original-body (symbol-function 'e-openai-codex-request-body)))
      (cl-letf (((symbol-function 'e-openai-codex-request-body)
                 (lambda (&rest args)
                   (let ((body (apply original-body args)))
                     (setq request-projections
                           (append request-projections
                                   (list (list
                                          :messages
                                          (copy-tree (plist-get args :messages))
                                          :options
                                          (copy-tree (plist-get args :options))
                                          :body body))))
                     body))))
        (e-board-e2e-create-session harness :id session-id)
        (e-context-lifetime-e2e--prompt-batch
         harness session-id "SEED-MULTI-PROMPT")
        (setq canvas "CANVAS-ONE")
        (e-context-lifetime-e2e--prompt-batch
         harness session-id "MULTI-TOOL-PROMPT")
        (setq canvas "CANVAS-TWO")
        (e-context-lifetime-e2e--prompt-batch
         harness session-id "AFTER-MULTI-PROMPT")))
    (setq final-context
          (e-harness-turn-context harness session-id "multi-tool-final-context"))
    (list
     :requests requests
     :projections request-projections
     :tool-calls tool-calls
     :request-log (lambda () requests)
     :projection-log (lambda () request-projections)
     :set-compaction-mode (lambda (value) (setq compaction-mode value))
     :harness harness
     :session-id session-id
     :final-context final-context
     :anchors
     (mapcar (lambda (anchor)
               (plist-get (plist-get anchor :metadata) :response-id))
              (e-session-provider-anchors
              (e-harness-sessions harness) session-id)))))

(ert-deftest e-context-lifetime-e2e-test-two-frame-curation-responses-compose ()
  "Real Responses transport curates two tool frames and publishes both activities."
  (dolist (mode '(stateless anchored))
    (let* ((process-environment
            (cons "E_CONTEXT_LIFETIME_TOKEN=credential-free-test"
                  process-environment))
           (e-harness-auto-compaction-enabled nil)
           (e-context-lifetime-shadow-projection-enabled t)
           (provider-id (intern (format "two-frame-%s-e2e" mode)))
           (session-id (format "two-frame-%s" mode))
           (e-openai-model-providers
            (list
             (list provider-id
                   :name (format "Two frame %s E2E" mode)
                   :base-url "https://two-frame.example.test/v1"
                   :env-key "E_CONTEXT_LIFETIME_TOKEN"
                   :wire-api 'responses
                   :responses-transport 'http
                   :response-store t
                   :responses-context-layout 'developer-input
                   :observation-delivery 'request-local-replaceable
                   :continuation (eq mode 'anchored)
                   :requires-openai-auth nil)))
           (requests nil)
           (request-count 0)
           (tool-calls nil)
           (captured-frames nil)
           (register-tool
            (lambda (registry name result)
              (e-tools-register
               registry
               :name name
               :description (format "Return %s." result)
               :parameters '(:type "object"
                             :properties (:target (:type "string"))
                             :required ["target"])
               :work
               (e-tools-cheap-work
                (format "e2e.two-frame.%s" name)
                (lambda (_arguments)
                  (setq tool-calls (append tool-calls (list name)))
                  result)))))
           (capability
            (e-capability-create
             :id (intern (format "two-frame-%s-capability" mode))
             :instructions "Use both inspection tools in order."
             :tools
             (list
              (lambda (registry)
                (funcall register-tool registry "inspect-a" "FRAME-A-SOURCE")
                (funcall register-tool registry "inspect-b" "FRAME-B-SOURCE")))))
           (transport
            (cl-function
             (lambda (&key body &allow-other-keys)
               (let ((parsed (e-context-lifetime-e2e--json-body body)))
                 (setq requests (append requests (list parsed))
                       request-count (1+ request-count))
                 (pcase request-count
                   (1
                    (e-context-lifetime-e2e--sse
                     '((type . "response.output_item.done")
                       (item . ((type . "function_call")
                                (call_id . "call-a")
                                (name . "inspect-a")
                                (arguments . "{\"stated_purpose\":\"Inspect A.\",\"target\":\"A\"}"))))
                     '((type . "response.completed")
                       (response . ((id . "two-frame-tool-a")
                                    (status . "completed"))))))
                   (2
                    (e-context-lifetime-e2e--sse
                     '((type . "response.output_item.done")
                       (item . ((type . "function_call")
                                (call_id . "curate-a")
                                (name . "context-curate")
                                (arguments . "{\"keep\":[1],\"summaries\":[],\"erase\":[]}"))))
                     '((type . "response.completed")
                       (response . ((id . "two-frame-curate-a")
                                    (status . "completed"))))))
                   (3
                    (e-context-lifetime-e2e--sse
                     '((type . "response.output_item.done")
                       (item . ((type . "function_call")
                                (call_id . "call-b")
                                (name . "inspect-b")
                                (arguments . "{\"stated_purpose\":\"Inspect B.\",\"target\":\"B\"}"))))
                     '((type . "response.completed")
                       (response . ((id . "two-frame-tool-b")
                                    (status . "completed"))))))
                   (4
                    (e-context-lifetime-e2e--sse
                     '((type . "response.output_item.done")
                       (item . ((type . "function_call")
                                (call_id . "curate-b")
                                (name . "context-curate")
                                (arguments . "{\"keep\":[1],\"summaries\":[],\"erase\":[]}"))))
                     '((type . "response.completed")
                       (response . ((id . "two-frame-curate-b")
                                    (status . "completed"))))))
                   (5
                    (e-context-lifetime-e2e--sse
                     '((type . "response.output_text.done")
                       (text . "TWO-FRAME-ANSWER"))
                     '((type . "response.completed")
                       (response . ((id . "two-frame-answer")
                                    (status . "completed"))))))
                   (_ (error "Unexpected two-frame request %d" request-count)))))))
           (harness
            (e-openai-create-harness
             :provider provider-id :model "gpt-e2e"
             :request-function transport)))
      (dolist (base-capability
               (e-layer-capabilities (e-harness-base-layer-create)))
        (e-harness-activate-capability harness base-capability))
      (e-harness-activate-capability harness capability)
      (let ((make-frame
             (symbol-function
              'e-harness-context-lifetime-tool-observation-frame)))
        (cl-letf (((symbol-function
                    'e-harness-context-lifetime-tool-observation-frame)
                   (lambda (&rest arguments)
                     (let ((frame (apply make-frame arguments)))
                       (setq captured-frames
                             (append captured-frames (list frame)))
                       frame))))
          (e-board-e2e-create-session harness :id session-id)
          (e-context-lifetime-e2e--prompt-batch
           harness session-id "Inspect A, curate it, then inspect and curate B.")))
      (let* ((store (e-harness-sessions harness))
             (curations (e-session-context-curations store session-id))
             (activities
              (seq-filter
               (lambda (event)
                 (eq (plist-get event :event-type) 'context-curated))
               (e-chat-service-activity-events harness session-id)))
             (messages (e-session-messages store session-id)))
        (should (= request-count 5))
        (should (equal tool-calls '("inspect-a" "inspect-b")))
        (should (= (length captured-frames) 2))
        (should (= (length curations) 2))
        (should (= (length activities) 2))
        (should
         (equal (mapcar (lambda (curation)
                          (plist-get curation :frame-id))
                        curations)
                (mapcar #'e-context-lifetime-frame-id captured-frames)))
        (should
         (equal (mapcar (lambda (activity)
                          (plist-get (plist-get activity :payload)
                                     :kept-source-count))
                        activities)
                '(1 1)))
        (should
         (equal (plist-get (car (last messages)) :content)
                "TWO-FRAME-ANSWER"))
        (should
         (equal
          (mapcar
           (lambda (request)
             (if (seq-find
                  (lambda (tool)
                    (equal (plist-get tool :name) "context-curate"))
                  (append (plist-get request :tools) nil))
                 t nil))
           requests)
          ;; The initial user request owns its own live frame.  Each consumed
          ;; curation acknowledgement is carrier-free, and tool B opens the
          ;; next opportunity.
          '(t t nil t nil)))
        (dolist (spec '((3 "curate-a") (5 "curate-b")))
          (let* ((request (nth (1- (car spec)) requests))
                 (call-id (cadr spec)))
            (should (= (length
                        (seq-filter
                         (lambda (item)
                           (and (equal (plist-get item :type)
                                       "function_call_output")
                                (equal (plist-get item :call_id) call-id)))
                         (append (plist-get request :input) nil)))
                       1))))))))

(ert-deftest e-context-lifetime-e2e-test-multi-tool-bundle-is-one-shot ()
  "Pair multiple streamed calls/replay/results and forget them as one bundle.

Each profile uses the real OpenAI capability resolver.  The branchable and
request-local profiles prove that an immediate response id may be used for
the matching in-memory result request, but neither that response nor its
contaminated descendant can become a later durable anchor."
  (dolist (spec '((stateless nil nil)
                  (clean-branch branchable nil)
                  (replaceable t request-local-replaceable)))
    (pcase-let ((`(,label ,continuation ,delivery) spec))
      (let* ((result
              (e-context-lifetime-e2e--run-multi-tool-profile
               label continuation delivery))
             (requests (plist-get result :requests))
             (projections (plist-get result :projections))
             (tool-calls (plist-get result :tool-calls))
             (anchors (plist-get result :anchors))
             (seed-request (nth 0 requests))
             (tool-request (nth 1 requests))
             (follow-up-request (nth 2 requests))
             (later-request (nth 3 requests))
             (tool-options (plist-get (nth 1 projections) :options))
             (follow-up-options (plist-get (nth 2 projections) :options))
             (later-options (plist-get (nth 3 projections) :options))
             (follow-up-effective
              (plist-get follow-up-request :effective-context))
             (later-effective
              (plist-get later-request :effective-context))
             (follow-up-text
              (e-context-lifetime-e2e--model-context-text
               follow-up-effective))
             (later-text
              (e-context-lifetime-e2e--model-context-text later-effective))
             (follow-up-input
              (plist-get follow-up-effective :input))
             (later-parsed (plist-get later-request :parsed)))
        (should (= (length requests) 4))
        (should (equal (mapcar (lambda (call) (plist-get call :name))
                               tool-calls)
                       '("inspect-one" "inspect-two")))
        ;; Both calls ran through the ordinary registry exactly once, with
        ;; provider arguments preserved at the real loop/tool boundary.
        (should (equal (mapcar (lambda (call)
                                 (plist-get (plist-get call :arguments)
                                            :target))
                               tool-calls)
                       '("ONE" "TWO")))
        (should (string-match-p "SEED-MULTI"
                                (e-context-lifetime-e2e--model-context-text
                                 (plist-get seed-request :effective-context))))
        ;; A's inherited chain includes the original durable seed prompt.
        (should (string-match-p "SEED-MULTI-PROMPT" follow-up-text))
        ;; A's opaque response id is used at most on its matching follow-up.
        (let ((follow-up-id
               (plist-get (plist-get follow-up-request :parsed)
                          :previous_response_id)))
          (if (eq label 'stateless)
              (should-not follow-up-id)
            (should (equal follow-up-id
                           (format "resp-%s-A" label)))))
        ;; The consuming invocation contains the paired replay/call/output
        ;; bundle, including both exact call identities and arguments.
        (let ((call-one
               (seq-find
                (lambda (item)
                  (and (equal (plist-get item :type) "function_call")
                       (equal (plist-get item :call_id) "call-one")))
                follow-up-input))
              (call-two
               (seq-find
                (lambda (item)
                  (and (equal (plist-get item :type) "function_call")
                       (equal (plist-get item :call_id) "call-two")))
                follow-up-input))
              (output-one
               (seq-find
                (lambda (item)
                  (and (equal (plist-get item :type)
                              "function_call_output")
                       (equal (plist-get item :call_id) "call-one")))
                follow-up-input))
              (output-two
               (seq-find
                (lambda (item)
                  (and (equal (plist-get item :type)
                              "function_call_output")
                       (equal (plist-get item :call_id) "call-two")))
                follow-up-input)))
          (should call-one)
          (should call-two)
          (should output-one)
          (should output-two)
          ;; Stateless replay reconstructs the prepared provider-neutral call,
          ;; whose operation arguments no longer contain envelope metadata.
          ;; Anchored continuations inherit the provider's original function
          ;; call, including the provider-visible stated purpose.
          (should (equal (plist-get call-one :arguments)
                         (if (eq label 'stateless)
                             "{\"target\":\"ONE\"}"
                           "{\"stated_purpose\":\"Inspect the first value.\",\"target\":\"ONE\"}")))
          (should (equal (plist-get call-two :arguments)
                         (if (eq label 'stateless)
                             "{\"target\":\"TWO\"}"
                           "{\"stated_purpose\":\"Inspect the second value.\",\"target\":\"TWO\"}")))
          (should (equal (plist-get output-one :output)
                         "TOOL-RESULT-ONE"))
          (should (equal (plist-get output-two :output)
                         "TOOL-RESULT-TWO")))
        (dolist (marker '("REPLAY-MULTI" "call-one" "inspect-one"
                          "call-two" "inspect-two"
                          "TOOL-RESULT-ONE" "TOOL-RESULT-TWO"))
          (should (string-match-p marker follow-up-text)))
        (should (= (e-context-lifetime-e2e--count
                    "function_call_output" follow-up-text)
                   2))
        ;; The mixed frontier is contaminated by the inherited tool bundle,
        ;; even when the canvas itself is request-local replaceable.
        (when (eq label 'replaceable)
          (should (= (e-context-lifetime-e2e--count
                      "CANVAS-TWO" follow-up-text)
                     1))
          (should-not (string-match-p "CANVAS-ONE" follow-up-text))
          (should-not (plist-get follow-up-options
                                 :lifetime-ephemerals-clean-p)))
        ;; The next invocation has CANVAS-TWO but no stale CANVAS-ONE or any
        ;; member of A/B's consumed bundle.  There are no orphan replay/call
        ;; items after the consuming response.
        (should (string-match-p "CANVAS-TWO" later-text))
        (should-not (string-match-p "CANVAS-ONE" later-text))
        (dolist (marker '("REPLAY-MULTI" "call-one" "inspect-one"
                          "TOOL-RESULT-ONE" "call-two" "inspect-two"
                          "TOOL-RESULT-TWO"
                          "resp-multi-"))
          (should-not (string-match-p marker later-text)))
        ;; A and B never enter the durable anchor set.  A clean branch may
        ;; reuse the seed; stateless and mixed replacement fall back cleanly.
        (should-not (member (format "resp-%s-A" label) anchors))
        (should-not (member (format "resp-%s-B" label) anchors))
        (pcase label
          ('stateless
           (should-not (plist-get later-parsed :previous_response_id))
           (should-not (plist-get later-options :provider-anchor)))
          ('clean-branch
           (should (equal
                    (plist-get
                     (plist-get (plist-get later-options :provider-anchor)
                                :metadata)
                     :response-id)
                    (format "resp-%s-seed" label)))
           (should (equal
                    (plist-get (plist-get later-request :parsed)
                               :previous_response_id)
                    (format "resp-%s-seed" label))))
          ('replaceable
           (should (equal
                    (plist-get
                     (plist-get (plist-get later-options :provider-anchor)
                                :metadata)
                     :response-id)
                    (format "resp-%s-seed" label)))
           (should (equal
                    (plist-get later-parsed :previous_response_id)
                    (format "resp-%s-seed" label)))))))))

(ert-deftest e-context-lifetime-e2e-test-performance-evidence-report ()
  "Emit deterministic scalar performance evidence for one capable profile.

The report is test output, not runtime state.  It records literal/effective
input sizes, inherited items, and continuation reuse around an ordinary
multi-tool turn and a deliberate portable generation boundary."
  (let* ((process-environment
          (cons "E_CONTEXT_LIFETIME_TOKEN=credential-free-test"
                process-environment))
         (e-context-lifetime-shadow-projection-enabled t)
         (e-openai-model-providers
          '((multi-tool-replaceable-e2e
             :name "Multi tool replaceable E2E"
             :base-url "https://api.openai.com/v1"
             :env-key "E_CONTEXT_LIFETIME_TOKEN"
             :wire-api responses
             :responses-transport http
             :response-store t
             :responses-context-layout developer-input
             :include-encrypted-reasoning t
             :continuation branchable
             :observation-delivery request-local-replaceable
             :provider-compaction opaque
             :requires-openai-auth nil)))
         (provider-output
          '((:type "encrypted"
             :payload "OPAQUE-COMPACT-ONE")))
         (provider-compaction-requests nil)
         (compaction-request-function
          (cl-function
           (lambda (&key url headers body)
             (let* ((parsed (e-context-lifetime-e2e--json-body body))
                    (literal-input (copy-tree (plist-get parsed :input)))
                    (effective-context
                     (list :instructions nil
                           :literal-input literal-input
                           :inherited-items nil
                           :input literal-input)))
               (setq provider-compaction-requests
                     (append provider-compaction-requests
                             (list (list :url url
                                         :headers (copy-tree headers)
                                         :body body
                                         :parsed parsed
                                         :literal-input literal-input
                                         :inherited-items nil
                                         :effective-context
                                         effective-context))))
               (format
                "{\"object\":\"response.compaction\",\"output\":%s,\"usage\":{\"input_tokens\":7,\"total_tokens\":11}}"
                (json-encode (vconcat provider-output)))))))
         (result
          (e-context-lifetime-e2e--run-multi-tool-profile
           'replaceable 'branchable 'request-local-replaceable
           compaction-request-function))
         (harness (plist-get result :harness))
         (session-id (plist-get result :session-id))
         (store (e-harness-sessions harness))
         (set-compaction-mode (plist-get result :set-compaction-mode))
         (request-log (plist-get result :request-log))
         (projection-log (plist-get result :projection-log))
         (marker-set '("TOOL-RESULT-ONE" "TOOL-RESULT-TWO"
                       "REPLAY-MULTI" "call-one" "call-two"))
         (canvas-stable-unchanged nil)
         (canvas-current-changed nil)
         (initial-requests (funcall request-log))
         (initial-projections (funcall projection-log)))
    ;; The helper has already completed the ordinary first, observation,
    ;; matching-follow-up, and canvas-change turns.  The next request is the
    ;; deliberate portable generation-boundary request.
    (should (= (length initial-requests) 4))
    (should-not (seq-some (lambda (request)
                            (plist-get request :compaction))
                          initial-requests))
    (let* ((before-context (plist-get result :final-context))
           (seed-anchor
            (seq-find
             (lambda (anchor)
               (equal (plist-get (plist-get anchor :metadata) :response-id)
                      "resp-replaceable-seed"))
             (e-session-provider-anchors store session-id)))
           (before-stable
            (e-context-lifetime-e2e--provider-anchor-fingerprints before-context))
           (before-current
            (plist-get
             (plist-get (nth 1 initial-projections) :options)
             :current-state-fingerprint))
           (after-current
            (plist-get
             (plist-get (nth 3 initial-projections) :options)
             :current-state-fingerprint)))
      ;; Request-local canvas replacement changes only the frontier value.
      ;; The seed anchor remains stable-compatible with the post-canvas
      ;; context, while the current-state fingerprint changes.
      (should seed-anchor)
      (should (equal (plist-get seed-anchor :fingerprints) before-stable))
      (should (stringp before-current))
      (should (stringp after-current))
      (setq canvas-stable-unchanged
            (equal (plist-get seed-anchor :fingerprints) before-stable)
            canvas-current-changed (not (equal before-current after-current)))
      (should canvas-stable-unchanged)
      (should canvas-current-changed))
    (let ((compaction-record
           (progn
             (funcall set-compaction-mode t)
               (unwind-protect
                   (e-harness-compact-session-batch
                    harness session-id :keep-recent-tokens 1)
               (funcall set-compaction-mode nil)))))
      (should compaction-record)
      ;; Drive the post-boundary request through the attached harness, loop,
      ;; and OpenAI backend.  This consumes the runtime provider candidate;
      ;; it is not a manually rendered request body.
      (e-context-lifetime-e2e--prompt-through-harness
       harness session-id "PERF-POST-COMPACTION")
      (let* ((requests (funcall request-log))
           (projections (funcall projection-log))
           (first (nth 0 requests))
           (observation (nth 1 requests))
           (follow-up (nth 2 requests))
           (warm (nth 3 requests))
           (post (car (last
                       (seq-remove (lambda (request)
                                     (plist-get request :compaction))
                                   requests))))
           (summary (seq-find (lambda (request)
                               (plist-get request :compaction))
                             requests))
           (provider-compaction-request
            (car provider-compaction-requests))
           (summary-count
            (length (seq-filter (lambda (request)
                                  (plist-get request :compaction))
                                requests)))
           (initial-summary-count
            (length (seq-filter (lambda (request)
                                  (plist-get request :compaction))
                                initial-requests)))
           (summary-metrics
            (and summary
                 (e-context-lifetime-e2e--request-metrics
                  summary marker-set)))
           (provider-compaction-metrics
            (and provider-compaction-request
                 (e-context-lifetime-e2e--request-metrics
                  provider-compaction-request marker-set)))
           (post-metrics
            (e-context-lifetime-e2e--request-metrics post marker-set))
           (provider-input
            (and provider-compaction-request
                 (plist-get provider-compaction-request :literal-input)))
           (post-input (plist-get (plist-get post :parsed) :input))
           (provider-output-first (car provider-output))
           (expected-post-prompt
            '(:type "message"
              :role "user"
              :content ((:type "input_text"
                         :text "PERF-POST-COMPACTION"))))
           (provider-output-selected
            (equal (car post-input) provider-output-first))
           (scenario-response-ids
            (delete-dups
             (delq nil
                   (append
                    (mapcar
                     (lambda (request)
                       (plist-get (plist-get request :parsed)
                                  :previous_response_id))
                     requests)
                    (plist-get result :anchors)))))
           (post-prompt-items
            (seq-filter
             (lambda (item)
               (string-match-p
                "PERF-POST-COMPACTION"
                (json-encode item)))
             post-input))
           (provider-compaction-count
            (length provider-compaction-requests))
           (warm-continuation-reused
            (and (plist-get (plist-get warm :parsed)
                           :previous_response_id)
                 t))
           (portable-compaction-observed
            (and summary (= summary-count 1)))
           (no-summary-per-observation
            (and (= initial-summary-count 0)
                 portable-compaction-observed))
           (portable-fallback-at-boundary
            (and (not provider-output-selected)
                 (null (plist-get (plist-get post :parsed)
                                  :previous_response_id))
                 (= provider-compaction-count 1)))
           (consumed-observation-retransmitted
            (> (alist-get 'raw_marker_bytes post-metrics) 0))
           (report
            (list
             (cons 'profile "branchable/request-local-replaceable")
             (cons 'summary_requests summary-count)
             (cons 'ordinary_tool_calls (length
                                         (plist-get result :tool-calls)))
             (cons 'consumed_observation_retransmitted
                   (if consumed-observation-retransmitted t :json-false))
             (cons 'canvas_stable_fingerprint_unchanged
                   (if canvas-stable-unchanged t :json-false))
             (cons 'canvas_current_fingerprint_changed
                   (if canvas-current-changed t :json-false))
             (cons 'warm_continuation_reused
                   (if warm-continuation-reused t :json-false))
             (cons 'no_summary_per_observation
                   (if no-summary-per-observation t :json-false))
             (cons 'portable_compaction_observed
                   (if portable-compaction-observed t :json-false))
             (cons 'provider_compaction
                   (list
                    (cons 'requests provider-compaction-count)
                    (cons 'input provider-compaction-metrics)
                    (cons 'output_items (length provider-output))
                    (cons 'output_item_zero_selected
                          (if provider-output-selected
                              t
                            :json-false))
                    (cons 'post_coverage_delta_items
                          (length post-prompt-items))))
             (cons 'portable_fallback_at_generation_boundary
                   (if portable-fallback-at-boundary t :json-false))
             (cons 'turns
                   (list
                    (cons 'first_turn
                          (e-context-lifetime-e2e--request-metrics
                           first marker-set))
                    (cons 'warm_turn
                          (e-context-lifetime-e2e--request-metrics
                           warm marker-set))
                    (cons 'observation_turn
                          (e-context-lifetime-e2e--request-metrics
                           observation marker-set))
                    (cons 'observation_follow_up
                          (e-context-lifetime-e2e--request-metrics
                           follow-up marker-set))
                    (cons 'portable_summary summary-metrics)
                    (cons 'post_compaction
                          (e-context-lifetime-e2e--request-metrics
                           post marker-set)))))))
      (should summary)
      (should provider-compaction-request)
      (should (= provider-compaction-count 1))
      (should (= (length (seq-filter
                          (lambda (request)
                            (plist-get request :compaction))
                          requests))
                 1))
      (should (= (length requests) 6))
      ;; The summary backend is a separate compaction stream and does not
      ;; pass through the ordinary Responses request-body projection hook;
      ;; the helper's projection wrapper also ends before this post-boundary
      ;; harness turn.
      (should (= (length projections) 4))
      ;; The normal warm/canvas turn reuses a clean anchor.  The post-boundary
      ;; request starts a fresh chain from the durable portable projection when
      ;; the opaque candidate is no longer compatible with the new generation.
      (should (plist-get (plist-get warm :parsed)
                         :previous_response_id))
      (should-not (plist-get (plist-get post :parsed)
                             :previous_response_id))
      (should-not provider-output-selected)
      (should portable-fallback-at-boundary)
      (should (= (length post-prompt-items) 1))
      (should (equal
               (plist-get (car post-prompt-items) :content)
               '((:type "input_text" :text "PERF-POST-COMPACTION"))))
      ;; The fresh request retains only the durable tail followed by the one
      ;; post-coverage prompt.  This proves order and absence of hidden
      ;; covered/stale items even when the opaque candidate is discarded.
      (should (= (length post-input) 3))
      (should (equal (car (last post-input)) expected-post-prompt))
      (should (= (length provider-output) 1))
      (should-not (equal (car post-input) provider-output-first))
      (should (= (alist-get 'raw_marker_bytes
                            provider-compaction-metrics)
                 0))
      ;; The observation bundle is visible to its consuming follow-up only.
      (let ((follow-up-text (prin1-to-string follow-up)))
        (should
         (seq-some
          (lambda (marker)
            (string-match-p (regexp-quote marker) follow-up-text))
          '("TOOL-RESULT-ONE" "REPLAY-MULTI" "call-one"))))
      ;; The portable summary input and the post-generation provider input
      ;; must both be durable-only.  Check literal and effective JSON
      ;; separately so a duplicated or hidden raw bundle cannot pass merely
      ;; because one representation happened to omit it.
      (should (= (alist-get 'raw_marker_bytes summary-metrics) 0))
      (should (= (alist-get 'raw_marker_bytes post-metrics) 0))
      (dolist (request (list summary provider-compaction-request post))
        (dolist (text (list
                       (json-encode (plist-get request :literal-input))
                       (json-encode
                        (plist-get (plist-get request :effective-context)
                                   :input))))
          (dolist (marker marker-set)
            (should-not (string-match-p (regexp-quote marker) text)))))
      (dolist (text (list (json-encode provider-input)
                          (json-encode post-input)))
        (dolist (marker '("ANCHOR" "provider-anchor"))
          (should-not (string-match-p (regexp-quote marker) text))))
      ;; Every response id actually observed in the pre-boundary chain is
      ;; absent from both the compact request and the fresh post-boundary
      ;; request, including their anchor/continuation fields.
      (dolist (request (list provider-compaction-request post))
        (let ((text (prin1-to-string request)))
          (dolist (response-id scenario-response-ids)
            (should-not (string-match-p (regexp-quote response-id) text)))))
      (let ((provider-body (plist-get provider-compaction-request :parsed)))
        (should (equal (plist-get provider-compaction-request :url)
                       (concat e-openai-api-default-base-url
                               "/responses/compact")))
        (should (equal
                 (sort (mapcar #'symbol-name
                               (cl-loop for (key _value) on provider-body
                                        by #'cddr
                                        collect key))
                       #'string<)
                 '(":input" ":model")))
        (should-not (plist-member provider-body :previous_response_id)))
      (should (equal (plist-get (plist-get provider-compaction-request
                                           :parsed)
                                :model)
                     "gpt-e2e"))
      (message "FEATURE88_PERFORMANCE %s" (json-encode report))))))

(provide 'e-context-lifetime-e2e-test)

;;; e-context-lifetime-e2e-test.el ends here

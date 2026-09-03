;;; e-openai-compaction-composition-test.el --- Tests for e OpenAI/Codex backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for OpenAI context curation and compaction composition.

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
    ;; Internal frame/source identities do not belong in the provider schema.
    (should
     (equal
      (plist-get (aref enabled-tools 0) :description)
      "After using presented ephemeral context sources, call this once to decide what remains available in later turns. Use keep for exact retention and summaries for compact durable replacements. Use erase only for labels whose source marker says erase-eligible; never erase a label marked erase-ineligible. Any presented label you omit loses its exact content; separately owned derived context such as receipts may remain."))
    (let* ((parameters (plist-get (aref enabled-tools 0) :parameters))
           (properties (plist-get parameters :properties))
           (summary-schema (plist-get properties :summaries))
           (summary-properties
           (plist-get (plist-get summary-schema :items) :properties))
           (source-schema (plist-get summary-properties :sources))
           (text-schema (plist-get summary-properties :text))
           (erase-schema (plist-get properties :erase)))
      (should (equal (sort (copy-sequence
                            (cl-loop for (key value) on properties by #'cddr
                                     collect key))
                           (lambda (left right)
                             (string< (symbol-name left)
                                      (symbol-name right))))
                     '(:erase :keep :summaries)))
      (should-not (plist-member parameters :required))
      (should (equal (plist-get (plist-get properties :keep) :type)
                     "array"))
      (should (equal (plist-get (plist-get properties :summaries) :type)
                     "array"))
      (should (equal (plist-get erase-schema :type) "array"))
      (should (equal (plist-get erase-schema :maxItems) 16))
      (should (equal (plist-get (plist-get erase-schema :items) :type)
                     "integer"))
      (should (equal (plist-get (plist-get erase-schema :items) :minimum)
                     1))
      (should (equal (plist-get (plist-get properties :keep) :maxItems)
                     16))
      (should (equal (plist-get (plist-get properties :summaries) :maxItems)
                     16))
      (should (equal (plist-get source-schema :minItems) 1))
      (should (equal (plist-get source-schema :maxItems) 16))
      (should (equal (plist-get (plist-get summary-schema :items) :required)
                     ["sources" "text"]))
      (should (equal (plist-get text-schema :minLength) 1))
      (should-not (plist-member properties :frame-id))
      (should-not (plist-member properties :source-observation-ids))
      (should-not (string-match-p
                   "frame-id\\|source-observation-ids\\|schema-version"
                   (json-encode enabled))))
    (should-not (plist-member disabled :tools))))

(ert-deftest e-openai-test-context-curation-schema-core-shape-agrees-on-transports ()
  "HTTP and WebSocket requests expose the same optional core shape."
  (dolist (transport '(http websocket))
    (let* ((body
            (e-openai-codex-request-body
             :messages '((:role user :content "prompt"))
             :options
             (list :model "gpt-test"
                   :responses-transport transport
                   :context-lifetime-enabled t
                   :reserved-effect-carrier 'context-curate-wire)
             :tools nil))
           (tool (aref (plist-get body :tools) 0))
           (parameters (plist-get tool :parameters))
           (properties (plist-get parameters :properties))
           (erase (plist-get properties :erase)))
      (should-not (plist-member parameters :required))
      (should (eq (plist-get parameters :additionalProperties) :json-false))
      (should (equal (sort (copy-sequence
                            (cl-loop for (key value) on properties by #'cddr
                                     collect key))
                           (lambda (left right)
                             (string< (symbol-name left)
                                      (symbol-name right))))
                     '(:erase :keep :summaries)))
      (should (equal (plist-get erase :type) "array"))
      (should (equal (plist-get erase :maxItems) 16))
      (should (equal (plist-get (plist-get erase :items) :type)
                     "integer"))
      (should (equal (plist-get (plist-get erase :items) :minimum) 1))
      (should (equal
               (e-context-lifetime-normalize-curation-disposition
                '(:keep (1) :summaries nil :erase (2)))
               '(:keep (1) :summaries nil :erase (2))))
      (should-error
       (e-context-lifetime-normalize-curation-disposition
        '(:keep nil :summaries nil))
       :type 'e-context-lifetime-invalid-record))))

(ert-deftest e-openai-test-context-curation-schema-is-captured-by-both-transports ()
  "The native HTTP and WebSocket starts capture the same optional carrier."
  (let* ((process-environment
          (cons "E_OPENAI_TEST_TOKEN=test-token" process-environment)))
    (dolist (transport '(http websocket))
      (let* ((e-openai-model-providers
              `((curation-transport-fixture
                 :name "Curation transport fixture"
                 :base-url "https://gateway.example.test/v1"
                 :env-key "E_OPENAI_TEST_TOKEN"
                 :wire-api responses
                 :responses-transport ,transport
                 :continuation nil
                 :requires-openai-auth nil)))
             captured-http captured-websocket request)
        (cl-letf (((symbol-function 'e-openai-http-request-start)
                   (lambda (&rest arguments)
                     (setq captured-http (plist-get arguments :body))
                     (e-backend-request-create :cancel (lambda () t))))
                  ((symbol-function 'e-openai-websocket-request-start)
                   (lambda (&rest arguments)
                     (setq captured-websocket
                           (plist-get arguments :body-data))
                     (e-backend-request-create :cancel (lambda () t)))))
          (let ((backend (e-openai-backend-create
                          :provider 'curation-transport-fixture)))
            (e-backend-start
             backend
             :messages '((:role user :content "prompt"))
             :options (list :model "gpt-test"
                            :context-lifetime-enabled t)
             :on-item #'ignore
             :on-done #'ignore
             :on-error #'ignore
             :on-request-start (lambda (value) (setq request value)))
            (should (e-openai-test--wait-until
                     (lambda () (or captured-http captured-websocket))
                     0.2))
            (let* ((body (if (eq transport 'http)
                             (json-parse-string
                              captured-http
                              :object-type 'plist
                              :array-type 'list
                              :null-object nil
                              :false-object :json-false)
                           captured-websocket))
                   (tool (car (append (plist-get body :tools) nil)))
                   (parameters (plist-get tool :parameters))
                   (properties (plist-get parameters :properties))
                   (erase (plist-get properties :erase)))
              (should body)
              (should
               (equal
                (plist-get tool :description)
                "After using presented ephemeral context sources, call this once to decide what remains available in later turns. Use keep for exact retention and summaries for compact durable replacements. Use erase only for labels whose source marker says erase-eligible; never erase a label marked erase-ineligible. Any presented label you omit loses its exact content; separately owned derived context such as receipts may remain."))
              (should-not (plist-member parameters :required))
              (should (eq (plist-get parameters :additionalProperties)
                          :json-false))
              (should (equal (sort (copy-sequence
                                    (cl-loop for (key value) on properties by #'cddr
                                             collect key))
                                   (lambda (left right)
                                     (string< (symbol-name left)
                                              (symbol-name right))))
                             '(:erase :keep :summaries)))
              (should (equal (plist-get erase :type) "array"))
              (should (equal (plist-get erase :maxItems) 16))
              (should (equal (plist-get (plist-get erase :items) :type)
                             "integer"))
              (should (equal (plist-get (plist-get erase :items) :minimum)
                             1))
              (should (equal (plist-get (plist-get properties :keep)
                                        :maxItems)
                             16))
              (should (equal (plist-get (plist-get properties :summaries)
                                        :maxItems)
                             16))
              (should (equal
                       (plist-get
                        (plist-get (plist-get properties :summaries) :items)
                        :required)
                       (if (eq transport 'http)
                           '("sources" "text")
                         ["sources" "text"])))
              (should-error
               (e-context-lifetime-normalize-curation-disposition
                '(:keep nil :summaries nil))
               :type 'e-context-lifetime-invalid-record)
              (when request
                (e-backend-cancel-request request)))))))))

(ert-deftest e-openai-test-context-curation-stream-carries-function-output-ack ()
  "The streaming Responses parser preserves the reserved call identity."
  (let* ((items
          (e-openai-codex-parse-stream
           "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"curation-call\",\"name\":\"context-curate\",\"arguments\":\"{\\\"keep\\\":[1],\\\"summaries\\\":[],\\\"erase\\\":[]}\"}}\n\n"))
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
             (:provider-id openai
              :metadata (:response-id "resp-promotion"
                         :reasoning-identity
                         (:effort "high" :summary "auto")))
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

(ert-deftest e-openai-test-loop-late-curation-ack-joins-tool-followup ()
  "A late reserved curation ack joins the ordinary tool result on the wire."
  (let* ((request-count 0)
         (requests nil)
         (durable-messages nil)
         (started-tools nil)
         (first-items nil)
         (curation-arguments
          (json-encode
           '(:keep [1] :summaries [] :erase [])))
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
                              :call_id "call-second"
                              :name "inspect-second"
                              :arguments (json-encode
                                          '(:target "other-state"))))
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
                        second-response)
                      nil
                      '(:effort "high" :summary "auto"))))
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
             input))
           (second-output-position
            (cl-position-if
             (lambda (item)
               (and (equal (plist-get item :type) "function_call_output")
                    (equal (plist-get item :call_id) "call-second")))
             input)))
      ;; The actual adapter stream order is ordinary call, reserved control,
      ;; then completion; the reserved control never enters ordinary tools.
      (should (< (cl-position 'tool-call first-types)
                 (cl-position 'context-curate first-types)))
      (should (member 'done first-types))
      (should (equal started-tools '("inspect-second" "inspect")))
      (should (= (length outputs) 3))
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
      (should (integerp second-output-position))
      (should (integerp curation-output-position))
      (should (< ordinary-output-position curation-output-position))
      (should (< second-output-position curation-output-position))
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
             :metadata (:response-id "fresh-anchor"
                        :reasoning-identity
                        (:effort "high" :summary "auto")))))
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

(provide 'e-openai-compaction-composition-test)

;;; e-openai-compaction-composition-test.el ends here

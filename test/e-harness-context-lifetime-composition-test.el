;;; e-harness-context-lifetime-composition-test.el --- Public harness context-lifetime composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Context-lifetime and provider-compaction restart/composition scenarios.

;;; Code:

(require 'ert)
(require 'e-anthropic)
(require 'e-json)
(require 'e-openai-decoder)
(require 'e-openai-responses)
(require 'e-sqlite-test-store-support
         (expand-file-name
          "e-sqlite-test-store-support.el"
          (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-harness-context-lifetime-composition-test--anthropic-stream
    (blocks stop-reason)
  "Return a complete SSE response for native content BLOCKS and STOP-REASON."
  (let ((events
         (list '(:type "message_start"
                :message (:role "assistant" :content []))))
        (index 0))
    (dolist (block blocks)
      (let ((type (plist-get block :type)))
        (setq events
              (append
               events
               (pcase type
                 ("thinking"
                  (append
                   (list (list :type "content_block_start"
                               :index index
                               :content_block
                               (list :type "thinking" :thinking ""))
                         (list :type "content_block_delta"
                               :index index
                               :delta (list :type "thinking_delta"
                                            :thinking
                                            (plist-get block :thinking))))
                   (when-let* ((signature (plist-get block :signature)))
                     (list (list :type "content_block_delta"
                                 :index index
                                 :delta (list :type "signature_delta"
                                              :signature signature))))
                   (list (list :type "content_block_stop" :index index))))
                 ("text"
                  (list (list :type "content_block_start"
                              :index index
                              :content_block '(:type "text" :text ""))
                        (list :type "content_block_delta"
                              :index index
                              :delta (list :type "text_delta"
                                           :text (plist-get block :text)))
                        (list :type "content_block_stop" :index index)))
                 (_
                  (list (list :type "content_block_start"
                              :index index :content_block block)
                        (list :type "content_block_stop" :index index))))))
        (setq index (1+ index))))
    (setq events
          (append events
                  (list (list :type "message_delta"
                              :delta (list :stop_reason stop-reason))
                        '(:type "message_stop"))))
    (mapconcat (lambda (event)
                 (format "event: %s\ndata: %s\n\n"
                         (plist-get event :type)
                         (e-json-serialize event)))
               events "")))

(defun e-harness-context-lifetime-composition-test--anthropic-tool-result-marker
    (body tool-use-id)
  "Return BODY's text marker immediately before TOOL-USE-ID's result."
  (seq-some
   (lambda (message)
     (let* ((blocks (append (plist-get message :content) nil))
            (result-index
             (cl-position-if
              (lambda (block)
                (and (equal (plist-get block :type) "tool_result")
                     (equal (plist-get block :tool_use_id) tool-use-id)))
              blocks)))
       (when (and result-index (> result-index 0))
         (let ((marker (nth (1- result-index) blocks)))
           (and (equal (plist-get marker :type) "text")
                (string-match-p
                 "\\[ephemeral context source [0-9]+,"
                 (plist-get marker :text))
                (plist-get marker :text))))))
   (append (plist-get body :messages) nil)))

(defun e-harness-context-lifetime-composition-test--source-markers (body)
  "Return current-state source marker blocks rendered in BODY's system text."
  (let ((printed (format "%s" (plist-get body :system)))
        (offset 0)
        markers)
    (while (string-match
            "\\[ephemeral context source [0-9]+, ~[0-9]+ tokens, erase-ineligible\\]"
            printed offset)
      (push (list :type "text" :text (match-string 0 printed)) markers)
      (setq offset (match-end 0)))
    (nreverse markers)))

(defun e-harness-context-lifetime-composition-test--openai-anchor-candidate
    (options response-id)
  "Return an OpenAI continuation candidate matching request OPTIONS."
  (list :type 'provider-anchor-candidate
        :provider-id 'openai
        :metadata
        (list :response-id response-id
              :prompt-layout-revision
              (e-openai-responses-prompt-layout-revision options)
              :reasoning-identity
              (e-openai-responses-reasoning-identity options))))

(ert-deftest e-harness-test-context-lifetime-tool-bundle-curates-and-forgets ()
  "A normal opted-in tool turn exposes a paired bundle once and keeps its curation."
  (e-harness-test--with-empty-layer-registry
    (let* ((request-count 0)
           (requests nil)
           (curation-input nil)
           (backend
            (e-backend-create
             :name "context-lifetime-tool"
             :context-capabilities
             '(:continuation none
               :observation-delivery request-local-replaceable
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (setq request-count (1+ request-count))
                (if (= request-count 1)
                    (progn
                      ;; The ordinary call remains an ordinary streamed tool
                      ;; item; the reserved effect is carried beside it.
                      (funcall
                       on-item
                       '(:type tool-call
                         :id "call-context-lifetime"
                         :name "remember-fact"
                         :arguments (:value "canvas marker")))
                      (setq curation-input
                            (e-openai-decoder--context-curation-effect
                             '(:keep []
                               :summaries
                               [(:sources [1]
                                 :text "selected durable fact")])
                             "curation-call-tool-bundle"))
                      (funcall on-item curation-input)
                      (funcall on-item '(:type done :reason tool-use)))
                  (funcall on-item
                           '(:type assistant-message
                             :content "follow-up answer"))
                  (funcall on-item '(:type done :reason stop)))))))
           (provider
            (e-context-provider-create
             :name 'context-lifetime-canvas
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system
                         :content "CANVAS-OBSERVATION")))))
           (capability
            (e-capability-create
             :id 'context-lifetime-tool-capability
             :context-providers (list provider)
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "remember-fact"
                 :description "Return the selected marker."
                 :handler (lambda (_arguments)
                            "BULKY-TOOL-RESULT"))))))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability))))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-create-session harness :id "session-1")
        (let ((probe (e-harness-turn-context harness "session-1" "probe")))
          (should (e-context-lifetime-frame-p
                   (plist-get probe :lifetime-frame))))
        (e-harness-test-prompt-batch harness "session-1" "remember this"))
      (should (= request-count 2))
      (should curation-input)
      (let* ((ordered-requests (reverse requests))
             (first-request (car ordered-requests))
             (second-request (cadr ordered-requests))
             (first-messages (plist-get first-request :messages))
             (first-options (plist-get first-request :options))
             (second-messages (plist-get second-request :messages))
             (tool-message
              (seq-find (lambda (message)
                          (eq (plist-get message :role) 'tool))
                        second-messages))
             (tool-replay-items
              (plist-get (plist-get tool-message :metadata)
                         :provider-replay-items))
             (roles (mapcar (lambda (message) (plist-get message :role))
                            second-messages)))
        (should (plist-get first-options :context-lifetime-enabled))
        (should (equal (mapcar (lambda (message) (plist-get message :role))
                               first-messages)
                       '(user system system)))
        (should (equal (plist-get (nth 1 first-messages) :content)
                       "[ephemeral context source 1, ~5 tokens, erase-ineligible]"))
        (should (equal (plist-get (nth 2 first-messages) :content)
                       "CANVAS-OBSERVATION"))
        (should (= (cl-count "CANVAS-OBSERVATION"
                             (mapcar (lambda (message)
                                       (plist-get message :content))
                                     first-messages)
                             :test #'equal)
                   1))
        (should (member 'tool-call roles))
        (should (member 'tool roles))
        (should
         (equal tool-replay-items
                (plist-get curation-input :provider-replay-items)))
        (should (equal
                 (plist-get
                  (plist-get tool-message :content)
                  :tool-call-id)
                 "call-context-lifetime")))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (let* ((store (e-harness-sessions harness))
               (projection (e-session-local-context-lifetime-projection
                            store "session-1"))
               (curations (plist-get projection :curations))
               (context (e-harness-turn-context
                         harness "session-1" "next-consumer"))
               (messages (plist-get context :messages))
               (message-entry-ids (plist-get context :message-entry-ids))
               (printed (prin1-to-string messages))
               (next-frame (plist-get context :lifetime-frame))
               (next-roles
                (mapcar (lambda (message) (plist-get message :role))
                        messages)))
          (should (= (length curations) 1))
          (should (vectorp message-entry-ids))
          (should (= (length message-entry-ids) (length messages)))
          (should
           (equal (delq nil (append (copy-sequence message-entry-ids) nil))
                  (plist-get projection :durable-tail-entry-ids)))
          (should-not (plist-member (plist-get context :options)
                                    :message-entry-ids))
          (let ((item (car (plist-get (car curations) :items))))
            (should (eq (plist-get item :kind) 'summary))
            (should (equal (plist-get item :text)
                           "selected durable fact")))
          (should
           (equal (plist-get projection :promotion-messages)
                  '((:role system :content "selected durable fact"))))
          (should (string-match-p "selected durable fact" printed))
          (should-not (string-match-p "BULKY-TOOL-RESULT" printed))
          (should-not (member 'tool-call next-roles))
          (should-not (member 'tool next-roles))
          (should (e-context-lifetime-frame-p next-frame))
          (should (seq-find
                   (lambda (message)
                     (equal (plist-get message :content)
                            "selected durable fact"))
                   messages))
        (should-not (string-match-p
                       "frame:consumer\|observation:"
                       printed)))))))

(ert-deftest e-harness-test-detached-context-lifetime-entry-identities-align ()
  "Detached durable bodies omit IDs while the request projection retains them."
  (let* ((path
          '(:messages
            ((:id "intent-entry" :type message :role user
              :content "durable intent")
             (:id "call-entry" :type message :role tool-call
              :content (:id "call-1" :name "inspect" :arguments ()))
             (:id "result-entry" :type message :role tool
              :content (:tool-call-id "call-1" :content "tool result"))
             (:id "answer-entry" :type message :role assistant
              :content "durable answer"))
            :message-path-indexes (0 1 2 3)
            :context-records nil
            :current-head-id "answer-entry"))
         (projection
          (e-harness-context-runtime--detached-lifetime-projection path))
         (generation
          (e-context-lifetime-generation-create
           :id "generation:detached-identity-map"
           :checkpoint nil
           :covered-session-boundary "root"))
         (projection (plist-put projection :generation generation))
         (path-messages (plist-get path :messages))
         (context
          (list :messages path-messages
                :segments (list (list :kind 'history
                                      :messages path-messages))
                :options nil))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (projected
          (e-harness-context-lifetime-apply-projection
           harness "detached-identity-map" "turn-1" context nil projection))
         (tail (plist-get projection :durable-tail))
         (tail-ids (plist-get projection :durable-tail-entry-ids))
         (messages (plist-get projected :messages))
         (message-entry-ids (plist-get projected :message-entry-ids)))
    (should (equal tail-ids '("intent-entry" "answer-entry")))
    (should (= (length tail) (length tail-ids)))
    (should-not (seq-some (lambda (message) (plist-member message :id)) tail))
    (should (equal messages tail))
    (should (equal (append message-entry-ids nil) tail-ids))
    (should (= (length messages) (length message-entry-ids)))
    (should-not (plist-member (plist-get projected :options)
                              :message-entry-ids))))

(ert-deftest e-harness-test-turn-context-message-identities-reach-loop-and-refresh ()
  "The turn owner forwards identity vectors outside backend options."
  (e-harness-test--with-empty-layer-registry
    (let* ((harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)))
           (initial
            '(:messages ((:role user :content "initial"))
              :message-entry-ids ["initial-entry"]
              :options nil
              :segments nil))
           (fresh
            '(:messages ((:role user :content "fresh")
                         (:role assistant :content "answer"))
              :message-entry-ids ["fresh-entry" "answer-entry"]
              :options nil
              :segments nil))
           captured-arguments)
      (e-harness-create-session harness :id "message-entry-map")
      (cl-letf (((symbol-function 'e-harness-turn-context)
                 (lambda (_harness _session-id _turn-id) fresh))
                ((symbol-function 'e-loop-start-turn)
                 (lambda (&rest arguments)
                   (setq captured-arguments arguments)
                   'started)))
        (should
         (eq (e-harness-turn--run-prompt-turn-async
              harness "message-entry-map" "turn-1" :context initial)
             'started))
        (should (equal (plist-get captured-arguments :message-entry-ids)
                       (plist-get initial :message-entry-ids)))
        (should-not (plist-member (plist-get captured-arguments :options)
                                  :message-entry-ids))
        (let* ((refresh (plist-get captured-arguments :refresh-context))
               (refreshed (funcall refresh)))
          (should (equal (plist-get refreshed :message-entry-ids)
                         (plist-get fresh :message-entry-ids)))
          (should (= (length (plist-get refreshed :messages))
                     (length (plist-get refreshed :message-entry-ids)))))))))

(ert-deftest e-harness-test-context-lifetime-presents-multiple-sources-at-late-frontier ()
  "Multiple live sources are marked once behind the stable canonical prefix."
  (e-harness-test--with-empty-layer-registry
    (let* ((requests nil)
           (source-one "FIRST-LATE-SOURCE")
           (source-two '(:kind "structured" :value 42 :items (alpha beta)))
           (backend
            (e-backend-create
             :name "context-lifetime-presentation"
             :context-capabilities
             '(:continuation none
               :observation-delivery inherited
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key messages options on-item &allow-other-keys)
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (funcall on-item
                         '(:type assistant-message :content "answer"))
                (funcall on-item '(:type done :reason stop))))))
           (stable-provider
            (e-context-provider-create
             :name 'stable-guidance
             :cache-placement 'stable-context
             :build (lambda (&rest _)
                      '((:role system :content "STABLE-GUIDANCE")))))
           (dynamic-provider
            (e-context-provider-create
             :name 'late-sources
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      (list (list :role 'system :content source-one)
                            (list :role 'system :content source-two)))))
           (capability
            (e-capability-create
             :id 'context-lifetime-presentation-capability
             :instructions "STATIC-POLICY"
             :context-providers (list stable-provider dynamic-provider)))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability))))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-create-session harness :id "session-1")
        (e-harness-test-prompt-batch harness "session-1" "durable prompt"))
      (let* ((request (car (last requests)))
             (messages (plist-get request :messages))
             (contents (mapcar (lambda (message)
                                 (plist-get message :content))
                               messages))
             (printed (prin1-to-string messages))
             (first-source-index
              (cl-position source-one contents :test #'equal))
             (second-source-index
              (cl-position source-two contents :test #'equal)))
        (should (equal contents
                       (list "STATIC-POLICY"
                             "STABLE-GUIDANCE"
                             "durable prompt"
                             "[ephemeral context source 1, ~5 tokens, erase-ineligible]"
                             source-one
                             "[ephemeral context source 2, ~14 tokens, erase-ineligible]"
                             source-two)))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :role))
                               messages)
                       '(system system user system system system system)))
        (should (< (cl-position "durable prompt" contents :test #'equal)
                   (cl-position "[ephemeral context source 1, ~5 tokens, erase-ineligible]" contents :test #'equal)))
        (should (< (cl-position "[ephemeral context source 1, ~5 tokens, erase-ineligible]" contents :test #'equal)
                   first-source-index))
        (should (< first-source-index
                   (cl-position "[ephemeral context source 2, ~14 tokens, erase-ineligible]" contents :test #'equal)))
        (should (< (cl-position "[ephemeral context source 2, ~14 tokens, erase-ineligible]" contents :test #'equal)
                   second-source-index))
        (should (= (cl-count source-one contents :test #'equal) 1))
        (should (= (cl-count source-two contents :test #'equal) 1))
        (should-not (string-match-p
                     "frame\|generation\|observation\|backing\|replay"
                     printed))))))

(ert-deftest e-harness-test-consumed-frame-labels-retire-before-new-frame ()
  "A stateless follow-up presents labels from only its current live frame."
  (e-harness-test--with-empty-layer-registry
    (let* ((request-count 0)
           (requests nil)
           (backend
            (e-backend-create
             :name "context-lifetime-marker-retirement"
             :context-capabilities
             '(:continuation none
               :observation-delivery inherited
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key messages options on-item &allow-other-keys)
                ;; Exercise the real adapter partition check as well as the
                ;; loop's provider-neutral message projection.
                (e-openai-codex-request-body
                 :messages messages :options options :tools nil)
                (setq requests
                      (append requests
                              (list (list :messages (copy-tree messages)
                                          :options (copy-tree options)))))
                (setq request-count (1+ request-count))
                (pcase request-count
                  (1
                   (funcall
                    on-item
                    (e-openai-decoder--context-curation-effect
                     '(:keep [] :summaries [] :erase [])
                     "curation-initial"))
                   (funcall on-item '(:type done :reason stop)))
                  (2
                   (dotimes (index 3)
                     (funcall
                      on-item
                      (list :type 'tool-call
                            :id (format "fresh-source-%d" (1+ index))
                            :name "read-source"
                            :arguments (list :index (1+ index)))))
                   (funcall on-item '(:type done :reason tool-use)))
                  (3
                   (funcall
                    on-item
                    (e-openai-decoder--context-curation-effect
                     '(:keep [3] :summaries [] :erase [])
                     "curation-fresh"))
                   (funcall on-item '(:type done :reason stop)))
                  (4
                   (funcall on-item
                            '(:type assistant-message
                              :content "LABELS-RETIRED"))
                   (funcall on-item '(:type done :reason stop)))
                  (_ (error "Unexpected marker-retirement request %d"
                            request-count)))))))
           (provider
            (e-context-provider-create
             :name 'marker-retirement-sources
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system :content "INITIAL-SOURCE-ONE")
                        (:role system :content "INITIAL-SOURCE-TWO")))))
           (capability
            (e-capability-create
             :id 'marker-retirement-capability
             :context-providers (list provider)
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "read-source"
                 :description "Return one fresh frame source."
                 :handler
                 (lambda (arguments)
                   (format "FRESH-SOURCE-%d"
                           (plist-get arguments :index))))))))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability))))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-create-session harness :id "marker-retirement")
        (e-harness-test-prompt-batch
         harness "marker-retirement" "inspect fresh sources"))
      (cl-labels
          ((source-markers
            (request)
            (seq-filter
             (lambda (message)
               (let ((content (plist-get message :content)))
                 (and (eq (plist-get message :role) 'system)
                      (stringp content)
                      (string-prefix-p "[ephemeral context source "
                                       content))))
             (plist-get request :messages))))
        (should (= request-count 4))
        (should (= (length (source-markers (nth 0 requests))) 2))
        (should-not (source-markers (nth 1 requests)))
        (let ((fresh-markers (source-markers (nth 2 requests))))
          (should (= (length fresh-markers) 3))
          (cl-loop for marker in fresh-markers
                   for label from 1
                   do (should
                       (string-prefix-p
                        (format "[ephemeral context source %d," label)
                        (plist-get marker :content)))))
        (should-not (source-markers (nth 3 requests))))
      (let* ((store (e-harness-sessions harness))
             (messages (e-session-local-messages store "marker-retirement"))
             (assistant
              (car (last (seq-filter
                          (lambda (message)
                            (eq (plist-get message :role) 'assistant))
                          messages)))))
        (should (equal (plist-get assistant :content) "LABELS-RETIRED"))
        (should (= (length (e-session-local-context-curations
                            store "marker-retirement"))
                   1))))))

(ert-deftest e-harness-test-context-lifetime-tool-marker-follows-all-prior-sources ()
  "A descendant tool source follows every prior presented source."
  (let* ((prior-observations
          (list
           (list :observation-id "observation:prior-one"
                 :kind "dynamic-context"
                 :source-entry-ref "context-source:prior-one"
                 :source-fingerprint "fingerprint:prior-one"
                 :effective-delivery "inherited"
                 :body "PRIOR-SOURCE-ONE")
           (list :observation-id "observation:prior-two"
                 :kind "current-state"
                 :source-entry-ref "context-source:prior-two"
                 :source-fingerprint "fingerprint:prior-two"
                 :effective-delivery "inherited"
                 :body "PRIOR-SOURCE-TWO")))
         (previous-frame
          (e-context-lifetime-frame-create
           :id "frame:prior"
           :generation-id "generation:prior"
           :consumer-request-id "consumer:prior"
           :observations prior-observations))
         (tool-message
          '(:role tool
            :content (:tool-call-id "call-descendant"
                      :name "inspect"
                      :status ok
                      :content "DESCENDANT-TOOL-SOURCE")))
         (frame
          (e-context-lifetime-frame-create
           :id "frame:descendant"
           :generation-id "generation:prior"
           :consumer-request-id "consumer:descendant"
           :observations
           (append prior-observations
                   (list
                    (list :observation-id "observation:descendant"
                          :kind "tool-result"
                          :source-entry-ref "context-source:tool"
                          :source-fingerprint "fingerprint:tool"
                          :effective-delivery "inherited"
                          :body tool-message)))))
         (messages (list (list :role 'system :content "STABLE") tool-message))
         (projection
          (e-harness-context-lifetime-present-tool-observation
           (list :frame frame
                 :previous-frame previous-frame
                 :message tool-message
                 :turn-messages messages
                 :provider-followup-messages (list tool-message))))
         (turn-messages (plist-get projection :turn-messages))
         (followup-messages (plist-get projection
                                      :provider-followup-messages)))
    (should (equal
             (mapcar (lambda (source) (plist-get source :label))
                     (e-context-lifetime-frame-curation-presentation frame))
             '(1 2 3)))
    (should (= (length turn-messages) 3))
    (should (= (length followup-messages) 2))
    (should (equal (plist-get (car turn-messages) :content) "STABLE"))
    (let ((turn-marker (cadr turn-messages))
          (followup-marker (car followup-messages))
          (turn-result (caddr turn-messages))
          (followup-result (cadr followup-messages)))
      (dolist (marker (list turn-marker followup-marker))
        (should
         (string-match-p
          "\\[ephemeral context source 3, ~[0-9]+ tokens, erase-eligible\\]"
          (plist-get marker :content)))
        (should
         (equal
          (plist-get (plist-get marker :metadata)
                     e-context-lifetime--request-local-source-marker-key)
          (list :kind
                e-context-lifetime--request-local-tool-result-marker-kind
                :tool-call-id "call-descendant"))))
      (dolist (result (list turn-result followup-result))
        (should
         (equal (plist-get (plist-get result :content) :content)
                "DESCENDANT-TOOL-SOURCE"))
        (should (= (cl-count result turn-messages :test #'equal) 1)))
      (should (equal turn-result followup-result))
      (should (= (cl-count followup-result followup-messages :test #'equal)
                 1)))
    (should-not
     (plist-member (plist-get tool-message :metadata)
                   e-context-lifetime--request-local-source-marker-key))))

(ert-deftest e-harness-test-context-lifetime-tool-result-curates-on-follow-up ()
  "A tool result is observed by B, curated there, then forgotten afterward."
  (e-harness-test--with-empty-layer-registry
    (let* ((directory (make-temp-file "e-harness-reserved-curation-" t))
           (store (e-session-persistent-store-create directory))
           (request-count 0)
           (requests nil)
           (curation-input nil)
           (events nil)
           (consumed-frames nil)
           (raw-result-marker "UNIQUE-RAW-TOOL-RESULT")
           (raw-result
            `(:capability "web.fetch"
              :headers [(:name "server" :value "nginx")
                        (:name "content-type" :value "text/html")]
              :text ,raw-result-marker))
           (backend
            (e-backend-create
             :name "context-lifetime-tool-result"
             :context-capabilities
             '(:continuation none
               :observation-delivery request-local-replaceable
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (setq request-count (1+ request-count))
                (pcase request-count
                  (1
                   ;; Response A only requests the ordinary tool.  The
                   ;; curation is deliberately withheld until B sees the
                   ;; paired call/result bundle.
                   (funcall
                    on-item
                    '(:type tool-call
                      :id "call-tool-result"
                      :name "inspect-result"
                      :arguments (
                                  :target "raw")))
                   (funcall on-item '(:type done :reason tool-use)))
                  (2
                   (setq curation-input
                         (e-openai-decoder--context-curation-effect
                          '(:keep []
                            :summaries
                            [(:sources [1]
                              :text "selected from tool result")])
                          "curation-call"))
                   (funcall on-item curation-input)
                   (funcall on-item '(:type done :reason stop)))
                  (3
                   (funcall on-item
                            '(:type assistant-message
                              :content "follow-up selected"))
                   (funcall on-item '(:type done :reason stop))))))))
           (provider
            (e-context-provider-create
             :name 'context-lifetime-tool-result-canvas
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system :content "CANVAS-A")))))
           (capability
            (e-capability-create
             :id 'context-lifetime-tool-result-capability
             :context-providers (list provider)
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "inspect-result"
                 :description "Return a uniquely identifiable result."
                 :handler (lambda (_arguments) raw-result))))))
           (harness
            (e-harness-create
             :backend backend
             :sessions store
             :intrinsic-capabilities (list capability))))
      (unwind-protect
          (progn
            (let ((e-context-lifetime-shadow-projection-enabled t)
                  (complete-frame
                   (symbol-function
                    'e-context-lifetime-frame-complete-for-consumer)))
              (e-harness-create-session harness :id "session-1")
              (e-harness-activity-subscribe
               harness (lambda (event) (push event events))
               :session-id "session-1")
              (cl-letf (((symbol-function
                          'e-context-lifetime-frame-complete-for-consumer)
                         (lambda (&rest arguments)
                           (let ((frame (apply complete-frame arguments)))
                             (push (list :frame frame
                                         :response-id (nth 2 arguments))
                                   consumed-frames)
                             frame))))
                (e-harness-test-prompt-batch harness "session-1" "inspect")))
            (e-session-flush-write-queue store)
            (should (= request-count 3))
            (let* ((ordered (reverse requests))
                   (request-b (cadr ordered))
                   (request-c (caddr ordered))
                   (messages-b (plist-get request-b :messages))
                   (roles-b (mapcar (lambda (message) (plist-get message :role))
                                    messages-b))
                   (tool-message-c
                    (seq-find
                     (lambda (message)
                       (and (eq (plist-get message :role) 'tool)
                            (equal (plist-get (plist-get message :content)
                                              :tool-call-id)
                                   "call-tool-result")))
                     (plist-get request-c :messages)))
                   (replay-items-c
                    (plist-get (plist-get tool-message-c :metadata)
                               :provider-replay-items))
                   (curation-call-position
                    (cl-position-if
                     (lambda (item)
                       (equal (plist-get (plist-get item :item) :type)
                              "function_call"))
                     replay-items-c))
                   (curation-ack-position
                    (cl-position-if
                     (lambda (item)
                       (and (equal (plist-get (plist-get item :item) :type)
                                   "function_call_output")
                            (equal (plist-get (plist-get item :item) :output)
                                   "")))
                     replay-items-c))
                   (curation-arguments (plist-get curation-input :arguments)))
              (should (equal (mapcar (lambda (message)
                                       (plist-get message :role))
                                     messages-b)
                             '(user system system tool-call system tool)))
              (should (string-match-p raw-result-marker
                                      (prin1-to-string messages-b)))
              (let ((tool-position
                     (cl-position 'tool roles-b :from-end t)))
                (should (equal (plist-get (nth (1- tool-position) messages-b)
                                          :role)
                               'system))
                (should (string-match-p
                         "\\[ephemeral context source 1, ~[0-9]+ tokens, erase-eligible\\]"
                         (plist-get (nth (1- tool-position) messages-b)
                                    :content))))
              (should (equal curation-arguments
                             '(:keep []
                               :summaries
                               [(:sources [1]
                                 :text "selected from tool result")])))
              (should tool-message-c)
              (should (equal (mapcar (lambda (item)
                                       (plist-get (plist-get item :item) :type))
                                     replay-items-c)
                             '("function_call" "function_call_output")))
              (should (integerp curation-call-position))
              (should (integerp curation-ack-position))
              (should (< curation-call-position curation-ack-position))
              (should (equal replay-items-c
                             (plist-get curation-input
                                        :provider-replay-items))))
            (should (equal (mapcar (lambda (message) (plist-get message :role))
                                   (e-harness-messages harness "session-1"))
                           '(user tool-call tool assistant)))
            (let* ((projection (e-session-local-context-lifetime-projection
                                store "session-1"))
                   (curations (plist-get projection :curations))
                   (tool-call
                    (seq-find (lambda (message)
                                (eq (plist-get message :role) 'tool-call))
                              (e-harness-messages harness "session-1")))
                   (assistant
                    (car (last
                          (seq-filter
                           (lambda (message)
                             (eq (plist-get message :role) 'assistant))
                           (e-harness-messages harness "session-1")))))
                   (next-context
                    (let ((e-context-lifetime-shadow-projection-enabled t))
                      (e-harness-turn-context
                       harness "session-1" "next-consumer")))
                   (next-messages (plist-get next-context :messages))
                   (printed (prin1-to-string next-messages))
                   (control-events
                    (seq-filter
                     (lambda (event)
                       (eq (plist-get event :event-type)
                           'context-curation-response))
                     (e-session-local-activity-events store "session-1")))
                   (control (car control-events))
                   (control-id (and control (plist-get control :id)))
                   (consumed-event
                    (seq-find
                     (lambda (event)
                       (eq (plist-get event :type) 'context-frame-consumed))
                     events))
                   (consumed-binding
                    (seq-find
                     (lambda (binding)
                       (equal (plist-get binding :response-id) control-id))
                     consumed-frames))
                   (reopened (e-session-persistent-store-create directory)))
              (should (= (length curations) 1))
              (should (= (length control-events) 1))
              (should control)
              (should (equal control-id
                             (plist-get (car curations) :response-entry-id)))
              (should (equal control-id
                             (plist-get (plist-get control :payload)
                                        :response-entry-id)))
              (should (equal control-id
                             (plist-get (plist-get consumed-event :payload)
                                        :response-entry-id)))
              (should
               (equal
                (plist-get (plist-get consumed-event :payload) :curation)
                '(:kept-source-count 0
                  :summary-count 1
                  :summarized-source-count 1
                  :erased-source-count 0
                  :source-stubs
                  ((:disposition summarized :source-kind "tool-result"
                    :tool-name "inspect-result")))))
              (should consumed-binding)
              (should (equal control-id
                             (e-context-lifetime-frame-consuming-response-entry-id
                              (plist-get consumed-binding :frame))))
              (should-not (equal control-id (plist-get tool-call :id)))
              (should-not (equal control-id (plist-get assistant :id)))
              (should (equal (plist-get control :event-type)
                             'context-curation-response))
              (should (equal (plist-get control :payload)
                             (list :response-entry-id control-id)))
              (should-not (string-match-p
                           "curation-call\\|function_call_output\\|frame-id\\|retry"
                           (prin1-to-string control)))
              (should-not (seq-find
                           (lambda (message)
                             (equal (plist-get message :id) control-id))
                           (e-session-local-messages store "session-1")))
              (should-not (string-match-p control-id printed))
              (should
               (equal (plist-get projection :promotion-messages)
                      '((:role system :content "selected from tool result"))))
              (should (string-match-p "selected from tool result" printed))
              (should-not (string-match-p raw-result-marker printed))
              (should-not (string-match-p "call-tool-result" printed))
              (should-not (member 'tool-call
                                  (mapcar (lambda (message)
                                            (plist-get message :role))
                                          next-messages)))
              (should-not (member 'tool
                                  (mapcar (lambda (message)
                                            (plist-get message :role))
                                          next-messages)))
              (let* ((reopened-record
                      (car (e-session-local-context-curations reopened "session-1")))
                     (reopened-control
                      (e-session-local-entry-by-id reopened "session-1" control-id))
                     (fork (e-session-fork reopened "session-1"))
                     (fork-id (plist-get fork :id))
                     (fork-projection
                      (e-session-local-context-lifetime-projection reopened fork-id))
                     (fork-generation (plist-get fork-projection :generation))
                     (fork-text
                      (prin1-to-string
                       (e-context-lifetime-generation-checkpoint
                        fork-generation))))
                (should (equal control-id
                               (plist-get reopened-record :response-entry-id)))
                (should (equal (e-session-local-entry-by-id reopened
                                                      "session-1" control-id)
                               reopened-control))
                (should (string-match-p "selected from tool result" fork-text))
                (should-not (string-match-p raw-result-marker fork-text))
                (should-not (string-match-p control-id fork-text))
                (should-not (e-session-local-entry-by-id reopened fork-id control-id)))))
      (delete-directory directory t)))))

(defun e-harness-context-lifetime-composition-test--anthropic-mixed-curation
    (compact-assistant-p)
  "Exercise mixed Messages curation, optionally compacting its assistant."
  (e-harness-test--with-empty-layer-registry
    (let* ((request-count 0)
           (requests nil)
           (events nil)
           (source-phase 'frame-a)
           (store (e-session-store-create))
           (session-id "anthropic-composed-curation")
           (frame-b-label nil)
           (frame-b-marker nil)
           (process-environment
            (cons "ANTHROPIC_GATEWAY_KEY=test-token" process-environment))
           (e-anthropic-model-providers
            '((test-gateway
               :name "Test Anthropic gateway"
               :base-url "https://gateway.example.test/v1"
               :auth bearer
               :env-key "ANTHROPIC_GATEWAY_KEY")))
           (backend
            (e-anthropic-backend-create
             :provider 'test-gateway
             :request-function
             (cl-function
              (lambda (&key body &allow-other-keys)
                (let* ((request-body (e-json-parse-string body))
                       (ordinal (cl-incf request-count)))
                  (setq requests (append requests (list request-body)))
                  (pcase ordinal
                    (1
                     (e-harness-context-lifetime-composition-test--anthropic-stream
                      '((:type "thinking" :thinking "Inspect frame A."
                         :signature "sig-frame-a")
                        (:type "text"
                         :text "Inspect the fresh result, then curate frame A.")
                        (:type "tool_use" :id "toolu-inspect-frame-b"
                         :name "inspect-source" :input (:path "frame-b"))
                        (:type "tool_use" :id "toolu-curate-frame-a"
                         :name "context-curate"
                         :input
                         (:keep [1]
                          :summaries
                          [(:sources [2] :text "FRAME-A-CURATED-SUMMARY")]
                          :erase [])))
                      "tool_use"))
                    (2
                     (setq frame-b-marker
                           (e-harness-context-lifetime-composition-test--anthropic-tool-result-marker
                            request-body "toolu-inspect-frame-b"))
                     (setq frame-b-label
                           (and frame-b-marker
                                (string-match
                                 "\\[ephemeral context source \\([0-9]+\\),"
                                 frame-b-marker)
                                (string-to-number
                                 (match-string 1 frame-b-marker))))
                     (unless frame-b-label
                       (error "Frame B tool result has no parsed source label: %S"
                              request-body))
                     (e-harness-context-lifetime-composition-test--anthropic-stream
                      (list
                       '(:type "thinking" :thinking "Inspect frame B."
                         :signature "sig-frame-b")
                       (list :type "tool_use"
                             :id "toolu-curate-frame-b"
                             :name "context-curate"
                             :input (list :keep []
                                          :summaries []
                                          :erase (vector frame-b-label))))
                      "tool_use"))
                    (3
                     (e-harness-context-lifetime-composition-test--anthropic-stream
                      '((:type "text" :text "FRAME-B-ANSWER")) "end_turn"))
                    (4
                     (e-harness-context-lifetime-composition-test--anthropic-stream
                      '((:type "text" :text "FRESH-TURN-ANSWER")) "end_turn"))
                    (_ (error "Unexpected Anthropic request %d" ordinal))))))))
           (provider
            (e-context-provider-create
             :name 'anthropic-frame-a-sources
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      (when (eq source-phase 'frame-a)
                        '((:role system :content "FRAME-A-KEPT-RAW")
                          (:role system :content "FRAME-A-SUMMARIZED-RAW")
                          (:role system :content "FRAME-A-OMITTED-RAW"))))))
           (capability
            (e-capability-create
             :id 'anthropic-composed-curation-capability
             :context-providers (list provider)
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "inspect-source"
                 :description "Return frame B's raw result."
                 :parameters '(:type "object"
                               :properties (:path (:type "string")))
                 :handler
                 (lambda (_arguments)
                   (setq source-phase 'after-frame-a)
                   (let ((result
                          (e-tools-result-create
                           (plist-get (e-tools-current-context) :tool-call)
                           'ok
                           (concat "FRAME-B-RAW-TOOL-RESULT "
                                   (make-string 240 ?b))
                           '(:refresh-context t))))
                     (when compact-assistant-p
                       (let ((entry
                              (seq-find
                               (lambda (candidate)
                                 (and
                                  (eq (plist-get candidate :role) 'tool-call)
                                  (equal
                                   (plist-get
                                    (plist-get candidate :content) :id)
                                   "toolu-inspect-frame-b")))
                               (reverse
                                (e-session-local-current-path
                                 store session-id)))))
                         (unless entry
                           (error "Expected current inspect-source call"))
                         (e-session-append-compaction
                          store session-id "Compacted before the tool result"
                          :first-kept-entry-id (plist-get entry :id))))
                     result)))))))
           (harness (e-harness-create
                     :backend backend
                     :sessions store
                     :intrinsic-capabilities (list capability))))
      (e-harness-create-session harness :id session-id)
      (e-harness-activity-subscribe
       harness (lambda (event) (setq events (append events (list event))))
       :session-id session-id)
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-test-prompt-batch harness session-id "Inspect frame A.")
        (e-harness-test-prompt-batch harness session-id "Fresh user turn."))
      (let* ((body-a (car requests))
             (body-b (cadr requests))
             (body-after-b-curation (nth 2 requests))
             (body-fresh (nth 3 requests))
             (wire-b (append (plist-get body-b :messages) nil))
             (mixed-response-text
              "Inspect the fresh result, then curate frame A.")
             (all-assistant-messages
              (seq-filter
               (lambda (message)
                 (equal (plist-get message :role) "assistant"))
               wire-b))
             (a-markers
              (e-harness-context-lifetime-composition-test--source-markers
               body-a))
             (a-marker-strings (mapcar (lambda (marker)
                                         (plist-get marker :text))
                                       a-markers))
             (body-b-text (prin1-to-string body-b))
             (body-after-b-text (prin1-to-string body-after-b-curation))
             (body-fresh-text (prin1-to-string body-fresh))
             (mixed-assistants
              (seq-filter
               (lambda (message)
                 (and (equal (plist-get message :role) "assistant")
                      (let ((ids
                             (mapcar
                              (lambda (block) (plist-get block :id))
                              (seq-filter
                               (lambda (block)
                                 (equal (plist-get block :type) "tool_use"))
                               (append (plist-get message :content) nil)))))
                        (and (member "toolu-inspect-frame-b" ids)
                             (member "toolu-curate-frame-a" ids)))))
               wire-b))
             (mixed-results
              (seq-filter
               (lambda (message)
                 (and (equal (plist-get message :role) "user")
                      (let ((ids
                             (mapcar
                              (lambda (block)
                                (plist-get block :tool_use_id))
                              (seq-filter
                               (lambda (block)
                                 (equal (plist-get block :type) "tool_result"))
                               (append (plist-get message :content) nil)))))
                       (and (member "toolu-inspect-frame-b" ids)
                             (member "toolu-curate-frame-a" ids)))))
               wire-b))
             (wire-mixed-text-occurrences
              (cl-loop for message in all-assistant-messages
                       for content = (plist-get message :content)
                       sum (if (stringp content)
                               (if (equal content mixed-response-text) 1 0)
                             (cl-count-if
                              (lambda (block)
                                (and (equal (plist-get block :type) "text")
                                     (equal (plist-get block :text)
                                            mixed-response-text)))
                              (append content nil)))))
             (durable-messages
              (e-session-local-messages
               (e-harness-sessions harness) session-id))
             (durable-mixed-text-occurrences
              (cl-count-if
               (lambda (message)
                 (and (eq (plist-get message :role) 'assistant)
                      (equal (plist-get message :content)
                             mixed-response-text)))
               durable-messages))
             (durable-message-text (prin1-to-string durable-messages))
             (b-curation-assistant
              (seq-find
               (lambda (message)
                 (and (equal (plist-get message :role) "assistant")
                      (seq-some
                       (lambda (block)
                         (equal (plist-get block :id) "toolu-curate-frame-b"))
                       (append (plist-get message :content) nil))))
               (append (plist-get body-after-b-curation :messages) nil)))
             (b-curation-result
              (seq-find
               (lambda (message)
                 (and (equal (plist-get message :role) "user")
                      (seq-some
                       (lambda (block)
                         (and (equal (plist-get block :type) "tool_result")
                              (equal (plist-get block :tool_use_id)
                                     "toolu-curate-frame-b")))
                       (append (plist-get message :content) nil))))
               (append (plist-get body-after-b-curation :messages) nil)))
             (consumed-events
              (seq-filter
               (lambda (event)
                 (eq (plist-get event :type) 'context-frame-consumed))
               events))
             (curation-events
              (seq-filter
               (lambda (event)
                 (plist-get (plist-get event :payload) :curation))
               consumed-events))
             (curation-a
              (plist-get (plist-get (car curation-events) :payload) :curation))
             (curation-b
              (plist-get (plist-get (cadr curation-events) :payload) :curation)))
        (should (= request-count 4))
        (should (= (length a-markers) 3))
        (should (string-match-p "FRAME-A-KEPT-RAW"
                                (prin1-to-string body-a)))
        (should (string-match-p "FRAME-A-SUMMARIZED-RAW"
                                (prin1-to-string body-a)))
        (should (string-match-p "FRAME-A-OMITTED-RAW"
                                (prin1-to-string body-a)))
        (should (stringp frame-b-marker))
        (should (integerp frame-b-label))
        (should-not (member frame-b-marker a-marker-strings))
        (should (= (length all-assistant-messages) 1))
        (should (= (length mixed-assistants) 1))
        (should (= (length mixed-results) 1))
        (should (= wire-mixed-text-occurrences 1))
        (should (= durable-mixed-text-occurrences 1))
        (should-not (string-match-p ":provider-replay-response-id"
                                    durable-message-text))
        (should-not (string-match-p "message-entry-ids" body-b-text))
        (should
         (equal (append (plist-get (car mixed-assistants) :content) nil)
                '((:type "thinking" :thinking "Inspect frame A."
                   :signature "sig-frame-a")
                  (:type "text"
                   :text "Inspect the fresh result, then curate frame A.")
                  (:type "tool_use" :id "toolu-inspect-frame-b"
                   :name "inspect-source" :input (:path "frame-b"))
                  (:type "tool_use" :id "toolu-curate-frame-a"
                   :name "context-curate"
                   :input
                   (:keep [1]
                    :summaries
                    [(:sources [2] :text "FRAME-A-CURATED-SUMMARY")]
                    :erase [])))))
        (let ((result-blocks
               (seq-filter
                (lambda (block)
                  (equal (plist-get block :type) "tool_result"))
                (append (plist-get (car mixed-results) :content) nil))))
          (should
           (equal (mapcar (lambda (block)
                            (plist-get block :tool_use_id))
                          result-blocks)
                  '("toolu-inspect-frame-b" "toolu-curate-frame-a")))
          (should
           (equal (plist-get (car result-blocks) :content)
                  (concat "FRAME-B-RAW-TOOL-RESULT "
                          (make-string 240 ?b))))
          (should (equal (plist-get (cadr result-blocks) :content)
                         "Curation applied.")))
        (should b-curation-assistant)
        (should b-curation-result)
        (should
         (equal
          (append (plist-get b-curation-assistant :content) nil)
          (list '(:type "thinking" :thinking "Inspect frame B."
                  :signature "sig-frame-b")
                (list :type "tool_use"
                      :id "toolu-curate-frame-b"
                      :name "context-curate"
                      :input (list :keep []
                                   :summaries []
                                   :erase (vector frame-b-label))))))
        (should
         (equal
          (seq-filter
           (lambda (block)
             (and (equal (plist-get block :type) "tool_result")
                  (equal (plist-get block :tool_use_id)
                         "toolu-curate-frame-b")))
           (append (plist-get b-curation-result :content) nil))
          '((:type "tool_result" :tool_use_id "toolu-curate-frame-b"
             :content "Curation applied."))))
        (dolist (marker a-marker-strings)
          (should-not (string-match-p (regexp-quote marker) body-b-text))
          (should-not
           (string-match-p (regexp-quote marker) body-after-b-text))
          (should-not (string-match-p (regexp-quote marker) body-fresh-text)))
        (should-not (string-match-p (regexp-quote frame-b-marker)
                                    body-after-b-text))
        (should-not (string-match-p (regexp-quote frame-b-marker)
                                    body-fresh-text))
        (should (= (length curation-events) 2))
        (should
         (equal curation-a
                '(:kept-source-count 1
                  :summary-count 1
                  :summarized-source-count 1
                  :erased-source-count 0
                  :source-stubs
                  ((:disposition kept :source-kind "current-state")
                   (:disposition summarized :source-kind "current-state")))))
        (should
         (equal curation-b
                '(:kept-source-count 0
                  :summary-count 0
                  :summarized-source-count 0
                  :erased-source-count 1
                  :source-stubs
                  ((:disposition erased :source-kind "tool-result"
                    :tool-name "inspect-source")))))
        (unless compact-assistant-p
          (should (string-match-p "FRAME-A-KEPT-RAW" body-fresh-text))
          (should (string-match-p "FRAME-A-CURATED-SUMMARY" body-fresh-text)))
        (should-not (string-match-p "FRAME-A-OMITTED-RAW" body-fresh-text))
        (should-not (string-match-p "FRAME-B-RAW-TOOL-RESULT" body-fresh-text))))))

(ert-deftest e-harness-test-anthropic-mixed-curation-composes-across-frames ()
  "Messages composes mixed frame-A curation, frame-B curation, and a fresh turn."
  (e-harness-context-lifetime-composition-test--anthropic-mixed-curation nil))

(ert-deftest e-harness-test-anthropic-mixed-curation-after-compaction ()
  "Messages replays a mixed response after compaction removes its assistant."
  (e-harness-context-lifetime-composition-test--anthropic-mixed-curation t))

(ert-deftest e-harness-test-invalid-curation-returns-provider-error-before-append ()
  "Rejected mixed calls recover without entering session or branch history."
  (e-harness-test--with-empty-layer-registry
    (dolist (case '((unknown-label . (:keep [2] :summaries []))
                    (oversized-record . (:keep [1] :summaries []))
                    (commentary . (:keep [2] :summaries []))))
      (let* ((response-phase
              (and (eq (car case) 'commentary) "commentary"))
             (directory (make-temp-file "e-harness-invalid-curation-" t))
             (store (e-session-persistent-store-create directory))
             reopened
             (request-count 0)
             (requests nil)
             (started-tools nil)
             (source-value "PREFLIGHT-SOURCE")
             (captured-frame nil)
             (prepared-bytes nil)
             (events nil)
             (backend
              (e-backend-create
               :name "context-lifetime-completion-preflight"
               :context-capabilities
               '(:continuation none
                 :observation-delivery request-local-replaceable
                 :reserved-effect-carrier context-curate-wire)
               :stream
               (cl-function
                (lambda (&key options on-item &allow-other-keys)
                  (cl-incf request-count)
                  (setq requests (append requests (list (copy-tree options))))
                  (pcase request-count
                    (1
                     (let ((arguments
                            (if (eq (car case) 'oversized-record)
                                (let ((text
                                       (make-string
                                        (1+ e-context-lifetime-curation-max-record-bytes)
                                        ?x)))
                                  ;; Keep the size assertion in the test while
                                  ;; leaving canonical record construction to
                                  ;; the context-lifetime owner.
                                  (setq prepared-bytes
                                        (string-bytes
                                         (encode-coding-string text 'utf-8 t)))
                                  (list :keep []
                                        :summaries
                                        (vector (list :sources [1]
                                                      :text text))))
                              (cdr case))))
                       (funcall on-item
                                '(:type tool-call
                                  :id "skipped-before-invalid"
                                  :name "skip-before"
                                  :arguments (:path "before")))
                       (funcall
                        on-item
                        (e-openai-decoder--context-curation-effect
                         arguments "invalid-preflight-curation")))
                     (funcall on-item
                              '(:type tool-call
                                :id "skipped-after-invalid"
                                :name "skip-after"
                                :arguments (:path "after")))
                     (funcall on-item
                              (list :type 'assistant-message
                                    :content "MUST-NOT-PERSIST"
                                    :phase response-phase))
                     (funcall on-item '(:type done :reason tool-use)))
                    (2
                     (funcall on-item
                              '(:type assistant-message
                                :content "RECOVERED-ANSWER"))
                     (funcall on-item '(:type done :reason stop)))
                    (_ (error "Unexpected request %d" request-count)))))))
             (provider
              (e-context-provider-create
               :name 'completion-preflight-source
               :cache-placement 'dynamic-context
               :build (lambda (&rest _)
                        (list (list :role 'system :content source-value)))))
             (capability
              (e-capability-create
               :id 'completion-preflight-capability
               :context-providers (list provider)
               :tools
               (list
                (lambda (registry)
                  (dolist (name '("skip-before" "skip-after"))
                    (let ((tool-name name))
                      (e-tools-test-register
                       registry
                       :name tool-name
                       :description "A tool that must remain unstarted."
                       :parameters '(:type "object"
                                     :properties (:path (:type "string"))
                                     :required ["path"])
                       :handler
                       (lambda (_arguments)
                         (push tool-name started-tools)
                         "unexpected"))))))))
             (harness
              (e-harness-create
               :backend backend
               :sessions store
               :intrinsic-capabilities (list capability)))
             (make-frame (symbol-function
                          'e-context-lifetime-frame-create-from-segments)))
        (cl-letf (((symbol-function
                    'e-context-lifetime-frame-create-from-segments)
                   (lambda (&rest arguments)
                     (setq captured-frame (apply make-frame arguments))
                     captured-frame)))
          (let ((e-context-lifetime-shadow-projection-enabled t))
            (e-harness-create-session harness :id "completion-preflight")
            (e-harness-activity-subscribe
             harness (lambda (event) (setq events (append events (list event))))
             :session-id "completion-preflight")
            (e-harness-test-prompt-batch
             harness "completion-preflight" "trigger curation")))
        (e-session-flush-write-queue store)
        (should (= request-count 2))
        (should-not started-tools)
        (when (eq (car case) 'oversized-record)
          (should (> prepared-bytes
                     e-context-lifetime-curation-max-record-bytes)))
        (should (e-context-lifetime-frame-p captured-frame))
        (should-not (e-session-local-context-curations
                     (e-harness-sessions harness) "completion-preflight"))
        (should-not (seq-find (lambda (event)
                                (eq (plist-get event :type) 'turn-failed))
                              events))
        (should
         (eq (or (plist-get (nth 1 requests) :reserved-effect-carrier)
                 (plist-get (plist-get (nth 1 requests)
                                       :context-capabilities)
                            :reserved-effect-carrier))
             'context-curate-wire))
        (let* ((skipped-calls
                (plist-get (nth 1 requests)
                           :provider-request-skipped-calls))
               (correction-items
                (plist-get (nth 1 requests) :provider-request-replay-items))
               (correction-output
                (seq-find
                 (lambda (item)
                   (equal (plist-get (plist-get item :item) :type)
                          "function_call_output"))
                 correction-items))
               (assistant-messages
                (seq-filter
                 (lambda (message)
                   (eq (plist-get message :role) 'assistant))
                 (e-harness-messages harness "completion-preflight"))))
          (should correction-output)
          (should
           (equal skipped-calls
                  '((:id "skipped-before-invalid" :name "skip-before"
                     :arguments (:path "before"))
                    (:id "skipped-after-invalid" :name "skip-after"
                     :arguments (:path "after")))))
          (should
           (equal
            (plist-get (plist-get correction-output :item) :output)
            e-openai-decoder--context-curation-invalid-correction))
          (should (equal (mapcar (lambda (message)
                                   (plist-get message :content))
                                 assistant-messages)
                         '("RECOVERED-ANSWER"))))
        (setq reopened (e-session-persistent-store-create directory))
        (let* ((root-messages
                (e-session-local-messages reopened "completion-preflight"))
               (root-records
                (e-session-storage-read-session-records
                 reopened "completion-preflight"))
               (fork (e-session-fork reopened "completion-preflight"))
               (fork-id (plist-get fork :id))
               (fork-messages (e-session-local-messages reopened fork-id))
               (fork-records
                (e-session-storage-read-session-records reopened fork-id)))
          (dolist (printed (list (prin1-to-string root-messages)
                                 (prin1-to-string root-records)
                                 (prin1-to-string fork-messages)
                                 (prin1-to-string fork-records)))
            (should-not
             (string-match-p
              "provider-request-skipped-calls\\|skipped-before-invalid\\|skipped-after-invalid\\|MUST-NOT-PERSIST"
              printed)))
          (should (equal (mapcar (lambda (message)
                                  (plist-get message :role))
                                root-messages)
                         '(user assistant)))
          (should
           (equal (mapcar (lambda (message)
                            (list (plist-get message :role)
                                  (plist-get message :content)))
                          root-messages)
                  (mapcar (lambda (message)
                            (list (plist-get message :role)
                                  (plist-get message :content)))
                          fork-messages))))
        (e-session-storage-close store)
        (e-session-storage-close reopened)
        (delete-directory directory t)))))

(ert-deftest e-harness-test-duplicate-curation-recovers-and-next-turn-is-usable ()
  "One duplicate correction settles normally and does not poison a fresh turn."
  (e-harness-test--with-empty-layer-registry
    (let* ((request-count 0)
           (requests nil)
           (events nil)
           (backend
            (e-backend-create
             :name "curation-recovery-fresh-turn"
             :context-capabilities
             '(:continuation none
               :observation-delivery request-local-replaceable
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key options on-item &allow-other-keys)
                (setq requests (append requests (list (copy-tree options)))
                      request-count (1+ request-count))
                (pcase request-count
                  ((or 1 2)
                   (funcall
                    on-item
                    (e-openai-decoder--context-curation-effect
                     '(:keep [1] :summaries [] :erase [])
                     (format "recovery-curation-%d" request-count)))
                   (funcall on-item '(:type done :reason stop)))
                  (3
                   (funcall on-item
                            '(:type assistant-message
                              :content "RECOVERED-ANSWER"))
                   (funcall on-item '(:type done :reason stop)))
                  (4
                   (funcall on-item
                            '(:type assistant-message
                              :content "FRESH-TURN-ANSWER"))
                   (funcall on-item '(:type done :reason stop)))
                  (_ (error "Unexpected recovery request %d" request-count)))))))
           (provider
            (e-context-provider-create
             :name 'curation-recovery-source
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system :content "RECOVERY-SOURCE")))))
           (capability
            (e-capability-create
             :id 'curation-recovery-capability
             :context-providers (list provider)))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability))))
      (e-harness-create-session harness :id "curation-recovery")
      (e-harness-activity-subscribe
       harness (lambda (event) (setq events (append events (list event))))
       :session-id "curation-recovery")
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-test-prompt-batch
         harness "curation-recovery" "curate, recover, and answer")
        (e-harness-test-prompt-batch
         harness "curation-recovery" "fresh question"))
      (let* ((store (e-harness-sessions harness))
             (messages (e-session-local-messages store "curation-recovery"))
             (correction-items
              (plist-get (nth 2 requests) :provider-request-replay-items))
             (correction-output
              (seq-find
               (lambda (item)
                 (equal (plist-get (plist-get item :item) :type)
                        "function_call_output"))
               correction-items)))
        (should (= request-count 4))
        (should (= (length (e-session-local-context-curations
                            store "curation-recovery"))
                   1))
        (should (= (seq-count
                    (lambda (event)
                      (eq (plist-get event :type)
                          'context-curation-duplicate-ignored))
                    events)
                   1))
        (should-not
         (string-match-p "e-context-lifetime-invalid-record"
                         (prin1-to-string events)))
        (should correction-output)
        (should
         (equal (plist-get (plist-get correction-output :item) :output)
                e-openai-decoder--context-curation-duplicate-correction))
        (should
         (equal (mapcar (lambda (message)
                          (plist-get message :content))
                        (seq-filter
                         (lambda (message)
                           (eq (plist-get message :role) 'assistant))
                         messages))
                '("RECOVERED-ANSWER" "FRESH-TURN-ANSWER")))
        (should-not (plist-get (nth 1 requests) :reserved-effect-carrier))
        (should-not (plist-get (nth 2 requests) :reserved-effect-carrier))))))

(ert-deftest e-harness-test-context-lifetime-assistant-curation-preserves-response-entry-id ()
  "A prepared curation and its assistant share one durable response identity."
  (e-harness-test--with-empty-layer-registry
    (let* ((directory (make-temp-file "e-harness-response-id-" t))
           (store (e-session-persistent-store-create directory))
           (request-count 0)
           (requests nil)
           (curation-input nil)
           (captured-frame nil)
           (consumed-frame nil)
           (events nil)
           (backend
            (e-backend-create
             :name "context-lifetime-assistant-response-id"
             :context-capabilities
             '(:continuation none
               :observation-delivery request-local-replaceable
               :reserved-effect-carrier context-curate-wire)
             :stream
             (cl-function
              (lambda (&key messages options on-item &allow-other-keys)
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (setq request-count (1+ request-count))
                (pcase request-count
                  (1
                   (setq curation-input
                         (e-openai-decoder--context-curation-effect
                          '(:keep [1] :summaries [])
                          "curation-call-response-id"))
                   (funcall on-item curation-input)
                   (funcall on-item
                            '(:type assistant-message :content "selected"))
                   (funcall on-item '(:type done :reason stop)))
                  (2
                   (funcall on-item
                            '(:type assistant-message :content "continued"))
                   (funcall on-item '(:type done :reason stop)))
                  (_ (error "Unexpected request %d" request-count)))))))
           (provider
            (e-context-provider-create
             :name 'assistant-response-id-source
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system :content "SOURCE-FOR-RESPONSE-ID")))))
           (capability
            (e-capability-create
             :id 'assistant-response-id-capability
             :context-providers (list provider)))
           (harness
            (e-harness-create
             :backend backend
             :sessions store
             :intrinsic-capabilities (list capability)))
           (make-frame
            (symbol-function 'e-context-lifetime-frame-create-from-segments))
           (complete-frame
             (symbol-function
              'e-context-lifetime-frame-complete-for-consumer)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'e-context-lifetime-frame-create-from-segments)
                       (lambda (&rest arguments)
                         (setq captured-frame (apply make-frame arguments))
                         captured-frame))
                      ((symbol-function 'e-context-lifetime-frame-complete-for-consumer)
                       (lambda (&rest arguments)
                         (setq consumed-frame (apply complete-frame arguments)))))
              (e-harness-activity-subscribe
               harness (lambda (event) (push event events))
               :session-id "assistant-response-id")
              (let ((e-context-lifetime-shadow-projection-enabled t))
                (e-harness-create-session harness :id "assistant-response-id")
                (e-harness-test-prompt-batch
                 harness "assistant-response-id" "curate this source")))
            (e-session-flush-write-queue store)
            (let* ((messages (e-harness-messages harness "assistant-response-id"))
                   (assistant
                    (seq-find
                     (lambda (message)
                       (and (eq (plist-get message :role) 'assistant)
                            (equal (plist-get message :content) "selected")))
                     messages))
                   (ack-request (cadr (reverse requests)))
                   (ack-message
                    (seq-find
                     (lambda (message)
                       (and (eq (plist-get message :role) 'assistant)
                            (equal (plist-get message :content) "selected")))
                     (plist-get ack-request :messages)))
                   (ack-items
                    (plist-get (plist-get ack-message :metadata)
                               :provider-replay-items))
                   (record (car (e-session-local-context-curations
                                 store "assistant-response-id")))
                   (event (seq-find
                           (lambda (entry)
                             (eq (plist-get entry :type)
                                 'context-frame-consumed))
                           events))
                   (response-id (plist-get assistant :id))
                   (reopened (e-session-persistent-store-create directory)))
              (should (= request-count 2))
              (should ack-message)
              (should (equal ack-items
                             (plist-get curation-input
                                        :provider-replay-items)))
              (should
               (equal (mapcar (lambda (item)
                                (plist-get (plist-get item :item) :type))
                              ack-items)
                      '("function_call" "function_call_output")))
              (should (stringp response-id))
              (should (equal response-id (plist-get record :response-entry-id)))
              (should (equal response-id
                             (e-context-lifetime-frame-consuming-response-entry-id
                              consumed-frame)))
              (should (equal response-id
                             (plist-get (plist-get event :payload)
                                        :response-entry-id)))
              (should captured-frame)
              (should consumed-frame)
              (should (e-context-lifetime-frame-consumed-p consumed-frame))
              (let* ((reopened-record
                      (car (e-session-local-context-curations
                            reopened "assistant-response-id")))
                     (reopened-assistant
                      (seq-find (lambda (message)
                                  (equal (plist-get message :id) response-id))
                                (e-session-local-messages reopened
                                                     "assistant-response-id")))
                     (fork (e-session-fork reopened "assistant-response-id"))
                     (fork-id (plist-get fork :id))
                     (fork-projection
                      (e-session-local-context-lifetime-projection reopened fork-id))
                     (fork-generation (plist-get fork-projection :generation))
                     (source-record-after-fork
                      (car (e-session-local-context-curations
                            reopened "assistant-response-id")))
                     (source-assistant-after-fork
                      (seq-find (lambda (message)
                                  (equal (plist-get message :id) response-id))
                                (e-session-local-messages reopened
                                                     "assistant-response-id"))))
                (should (equal response-id
                               (plist-get reopened-record :response-entry-id)))
                (should (equal response-id (plist-get reopened-assistant :id)))
                (should (equal response-id
                               (plist-get source-assistant-after-fork :id)))
                (should (equal response-id
                               (plist-get source-record-after-fork
                                          :response-entry-id)))
                (should (member '(:role system :content "SOURCE-FOR-RESPONSE-ID")
                                (e-context-lifetime-generation-checkpoint
                                 fork-generation)))
                (should-not
                 (string-match-p "\\[ephemeral context source 1, ~"
                                 (prin1-to-string
                                  (e-context-lifetime-generation-checkpoint
                                   fork-generation))))
                (should (stringp fork-id)))))
        (delete-directory directory t)))))

(ert-deftest e-harness-test-context-lifetime-zero-curation-consumes-frame ()
  "A response with no curation drops its live frame without a durable record."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         frame entry
         events)
    (e-harness-create-session harness :id "session-1")
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (setq frame (e-harness-test--curation-frame))
    (setq entry (list :status 'running :context-frame frame))
    (let ((e-context-lifetime-shadow-projection-enabled t)
          consumed)
      (setq consumed
            (e-harness-context-lifetime-commit-response
             harness "session-1" "turn-1" entry
             (list :frame frame
                   :provider-request-id "response-1"
                   :response-entry-id "response-1"
                   :curation-effects nil)))
      (should (e-context-lifetime-frame-consumed-p consumed))
      (e-harness-turn-state-set-context-frame entry consumed))
    (should (e-context-lifetime-frame-consumed-p
             (plist-get entry :context-frame)))
    (should (e-context-lifetime-frame-observations frame))
    (should-not (e-session-local-context-curations store "session-1"))
    (let ((consumed-event
           (seq-find (lambda (event)
                       (eq (plist-get event :type) 'context-frame-consumed))
                     events)))
      (should consumed-event)
      (should-not
       (plist-get (plist-get consumed-event :payload) :curation)))))

(ert-deftest e-harness-test-context-lifetime-invalid-curation-keeps-live-frame ()
  "Preparation failure leaves both the live source body and session untouched."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         (frame (e-harness-test--curation-frame))
         (entry (list :status 'running :context-frame frame))
         events)
    (e-harness-create-session harness :id "session-1")
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (should-error
       (e-harness-context-lifetime-commit-response
        harness "session-1" "turn-1" entry
        (list :frame frame
               :provider-request-id "response-1"
               :curation-effects
               (list (list :type 'context-curate
                           :arguments
                           '(:keep [2] :summaries [])))))
       :type 'e-context-lifetime-invalid-record))
    (should (eq (plist-get entry :context-frame) frame))
    (should-not (e-context-lifetime-frame-consumed-p frame))
    (should-not
     (seq-find (lambda (event)
                 (eq (plist-get event :type) 'context-frame-consumed))
               events))
    (should (equal (plist-get (car (e-context-lifetime-frame-observations frame))
                              :body)
                   '(:role system :content "HARNESS-EXACT-VALUE")))
    (should-not (e-session-local-context-curations store "session-1"))))

(ert-deftest e-harness-test-context-lifetime-steering-rebuilds-after-follow-up ()
  "Steering after curation acknowledgement uses the fresh projection once."
  (e-harness-test--with-empty-layer-registry
    (let (backend provider capability harness turn-id
          (request-count 0) requests finish-b raw-result curation-input)
      (setq raw-result "RAW-STEERING-RESULT")
      (setq backend
            (e-backend-create
             :name "context-lifetime-steering-refresh"
             :context-capabilities
             '(:continuation linear
               :observation-delivery inherited
               :reserved-effect-carrier context-curate-wire)
             :start
             (cl-function
              (lambda (&key messages options on-item on-done on-request-start
                             &allow-other-keys)
                (setq request-count (1+ request-count))
                (push (list :messages (copy-tree messages)
                            :options (copy-tree options))
                      requests)
                (let ((request (e-backend-request-create))
                      (ordinal request-count))
                  (funcall on-request-start request)
                  (cond
                   ((= ordinal 1)
                    (funcall on-item
                             '(:type provider-replay-item
                               :provider-id openai
                               :full-replay-only t
                               :item (:type "function_call"
                                      :call_id "call-steering"
                                      :name "inspect-steering"
                                      :arguments
                                      "{\"marker\":\"REPLAY-STEERING\"}")))
                    (funcall on-item
                             '(:type tool-call
                               :id "call-steering"
                               :name "inspect-steering"
                               :arguments ()))
                    (funcall on-item
                             (e-harness-context-lifetime-composition-test--openai-anchor-candidate
                              options "resp-A"))
                    (funcall on-item '(:type done :reason tool-use))
                    (funcall on-done '(:status done)))
                   ((= ordinal 2)
                    ;; Leave B in flight so steering is queued while its
                    ;; consumed frame has not yet been finalized.
                    (setq finish-b
                          (lambda ()
                            (setq curation-input
                                  (e-openai-decoder--context-curation-effect
                                   '(:keep []
                                     :summaries
                                     [(:sources [1]
                                       :text "selected after B")])
                                   "curation-call-steering"))
                            (funcall on-item curation-input)
                            (funcall on-item
                                     '(:type assistant-message
                                       :content "B"))
                            (funcall on-item
                                     (e-harness-context-lifetime-composition-test--openai-anchor-candidate
                                      options "resp-B"))
                            (funcall on-item '(:type done :reason stop))
                            (funcall on-done '(:status done)))))
                   ((= ordinal 3)
                    (funcall on-item
                             '(:type assistant-message :content "C"))
                    (funcall on-item
                             (e-harness-context-lifetime-composition-test--openai-anchor-candidate
                              options "resp-C"))
                    (funcall on-item '(:type done :reason stop))
                    (funcall on-done '(:status done)))
                   ((= ordinal 4)
                    (funcall on-item
                             '(:type assistant-message :content "D"))
                    (funcall on-item '(:type done :reason stop))
                    (funcall on-done '(:status done)))
                   (t (error "unexpected request %S" ordinal)))
                  )))))
      (setq provider
            (e-context-provider-create
             :name 'context-lifetime-steering-canvas
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      '((:role system :content "CANVAS-STEERING")))))
      (setq capability
            (e-capability-create
             :id 'context-lifetime-steering-capability
             :context-providers (list provider)
             :tools
             (list
              (lambda (registry)
                (e-tools-test-register
                 registry
                 :name "inspect-steering"
                 :description "Produce one ephemeral steering result."
                 :handler (lambda (_arguments) raw-result))))))
      (setq harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability)
             :default-options '(:model "gpt-test"
                                :reasoning-effort "high"
                                :reasoning-summary "auto"
                                :responses-transport http
                                :response-store t
                                :provider-continuation t
                                :provider-anchor-provider-id openai
                                :reserved-effect-carrier context-curate-wire)))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-harness-create-session harness :id "session-1")
        (setq turn-id
              (e-harness-test-prompt-async harness "session-1" "inspect"))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (< request-count 2)
                      (not finish-b)
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should (= request-count 2))
        (should finish-b)
        (should (equal
                 (e-harness-test-steer-active-turn
                  harness "session-1" "steer now")
                 turn-id))
        (funcall finish-b)
        (let ((entry (e-harness-wait-batch harness "session-1" 1.0)))
          (should (eq (plist-get entry :status) 'done))))
      (let* ((ordered (nreverse requests))
             (request-b (nth 1 ordered))
             (request-c (nth 2 ordered))
             (request-d (nth 3 ordered))
             (messages-b (plist-get request-b :messages))
             (messages-c (plist-get request-c :messages))
             (messages-d (plist-get request-d :messages))
             (curation-message
              (seq-find
               (lambda (message)
                 (and (eq (plist-get message :role) 'assistant)
                      (equal (plist-get message :content) "B")))
               messages-c))
             (curation-replay-items
              (plist-get (plist-get curation-message :metadata)
                         :provider-replay-items))
             (body-b-anchored
              (e-openai-codex-request-body
               :messages messages-b
               :options (plist-get request-b :options)
               :tools nil))
             (body-c
              (e-openai-codex-request-body
               :messages messages-c
               :options (plist-get request-c :options)
               :tools nil))
             (body-d
              (e-openai-codex-request-body
               :messages messages-d
               :options (plist-get request-d :options)
               :tools nil))
             (input-b (append (plist-get body-b-anchored :input) nil))
             (input-c (append (plist-get body-c :input) nil))
             (input-d (append (plist-get body-d :input) nil))
             (tool-follow-up-outputs
              (seq-filter
               (lambda (item)
                 (and (equal (plist-get item :type) "function_call_output")
                      (equal (plist-get item :call_id) "call-steering")))
               input-b))
             (curation-calls
              (seq-filter
               (lambda (item)
                 (and (equal (plist-get item :type) "function_call")
                      (equal (plist-get item :call_id)
                             "curation-call-steering")))
               input-c))
             (curation-acks
              (seq-filter
               (lambda (item)
                 (and (equal (plist-get item :type) "function_call_output")
                      (equal (plist-get item :call_id)
                             "curation-call-steering")))
               input-c))
             (request-c-anchor
              (plist-get (plist-get request-c :options) :provider-anchor))
             (request-c-anchor-layout
              (plist-get (plist-get request-c-anchor :metadata)
                         :prompt-layout-revision))
             (request-c-layout
              (e-openai-responses-prompt-layout-revision
               (plist-get request-c :options)))
             (printed-b (prin1-to-string messages-b))
             (printed-c (prin1-to-string messages-c))
             (printed-d (prin1-to-string messages-d)))
        (should (= request-count 4))
        (should (string-match-p raw-result printed-b))
        (should (string-match-p "call-steering" printed-b))
        (should (string-match-p "REPLAY-STEERING" printed-b))
        (should (string-match-p "selected after B" printed-c))
        (should-not (string-match-p "steer now" printed-c))
        (should (string-match-p "selected after B" printed-d))
        (should (string-match-p "steer now" printed-d))
        (should-not (string-match-p raw-result printed-d))
        (should curation-message)
        (should (equal curation-replay-items
                       (plist-get curation-input :provider-replay-items)))
        (should
         (equal (mapcar (lambda (item)
                          (plist-get (plist-get item :item) :type))
                        curation-replay-items)
                '("function_call" "function_call_output")))
        (should (string-match-p "curation-call-steering" printed-c))
        (should (equal (plist-get body-b-anchored :previous_response_id)
                       "resp-A"))
        (should (= (length tool-follow-up-outputs) 1))
        (should (equal (plist-get (car tool-follow-up-outputs) :output)
                       raw-result))
        (should-not (string-match-p "REPLAY-STEERING"
                                    (prin1-to-string input-b)))
        (should request-c-anchor)
        (should-not (equal request-c-anchor-layout request-c-layout))
        (should-not (plist-get body-c :previous_response_id))
        (should (= (length curation-calls) 1))
        (should (= (length curation-acks) 1))
        (should (equal (plist-get (car curation-acks) :output) ""))
        (should-not (plist-get (plist-get request-d :options) :provider-anchor))
        (should-not (plist-get (plist-get request-d :options)
                               :provider-request-replay-items))
        (dolist (marker (list raw-result "call-steering" "REPLAY-STEERING"
                               "resp-A" "resp-B" "resp-C"
                               "curation-call-steering"))
          (should-not (string-match-p marker printed-d)))
        (should-not (plist-get body-d :previous_response_id))
        (should (string-match-p raw-result (prin1-to-string body-c)))
        (should-not (string-match-p "steer now"
                                    (prin1-to-string body-c)))
        (should (string-match-p "steer now" (prin1-to-string body-d)))
        (should (string-match-p "selected after B" (prin1-to-string body-d)))
        (should-not (string-match-p raw-result (prin1-to-string body-d)))
        (should-not
         (seq-some
          (lambda (item)
            (and (member (plist-get item :type)
                         '("function_call" "function_call_output"))
                 (equal (plist-get item :call_id) "curation-call-steering")))
          input-d))
        (should-not
         (e-session-local-provider-anchors
          (e-harness-sessions harness) "session-1"))))))

(ert-deftest e-harness-test-provider-compaction-sync-installs-runtime-candidate ()
  "A successful portable boundary may install only a runtime provider candidate."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (provider-input nil)
         (backend
          (e-backend-create
           :name 'provider-compaction-fake
           :context-capabilities
           '(:continuation none
             :provider-compaction opaque)
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall on-item
                       '(:type assistant-message :content "portable C1"))))
           :provider-compaction
           (cl-function
            (lambda (&key messages options &allow-other-keys)
              (setq provider-input (list :messages (copy-tree messages)
                                         :options (copy-tree options)))
              '(:output ((:type "opaque" :marker "provider-state"))
                :usage (:total-tokens 3))))))
         (harness
          (e-harness-create
           :backend backend
           :default-options '(:model "fake-model"
                              :provider-anchor-provider-id fake
                              :context-lifetime-enabled t))))
    (e-harness-create-session harness :id "provider-session")
    ;; Establish the opt-in v2 generation before the ordinary transcript.
    (e-harness-turn-context harness "provider-session" "seed-turn")
    (e-session-append-message
     (e-harness-sessions harness) "provider-session"
     '(:role user :content "durable intent"))
    (e-session-append-message
     (e-harness-sessions harness) "provider-session"
     '(:role assistant :content "durable answer"))
    (e-session-append-message
     (e-harness-sessions harness) "provider-session"
     '(:role user :content "keep this"))
    (e-harness-compact-session-batch
     harness "provider-session" :keep-recent-tokens 1)
    (should provider-input)
    (should (string-match-p "portable C1"
                            (prin1-to-string (plist-get provider-input :messages))))
    (should-not (string-match-p "provider-state"
                                (prin1-to-string provider-input)))
    (let* ((candidate
            (gethash "provider-session"
                     (e-harness-provider-compaction-candidates harness)))
           (context (e-harness-turn-context
                     harness "provider-session" "candidate-turn"))
           (options (plist-get context :options)))
      (should candidate)
      (should-not (gethash "provider-session"
                           (e-harness-provider-compaction-candidates harness)))
      (should (equal (plist-get options :provider-compaction-output)
                     '((:type "opaque" :marker "provider-state"))))
      (should (eq (plist-get options :context-rendering-strategy)
                  'opaque-provider-compaction))
      ;; Provider state is a runtime candidate only, never a session record.
      (should-not
       (string-match-p
        "provider-state"
        (prin1-to-string
         (list (e-session-local-messages (e-harness-sessions harness)
                                   "provider-session")
               (e-session-local-activity-events
                (e-harness-sessions harness) "provider-session")
               (e-session-local-context-generations
               (e-harness-sessions harness) "provider-session"))))))))

(ert-deftest e-harness-test-provider-compaction-async-captures-fixed-boundary ()
  "An async result uses its start-time boundary, leaving later durable input in delta."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (done-callback nil)
         (backend
          (e-backend-fake-create
           :items nil
           :context-capabilities
           '(:continuation none :provider-compaction opaque)
           :provider-compaction
           (cl-function
            (lambda (&key on-done &allow-other-keys)
              (setq done-callback on-done)
              (e-backend-request-create :metadata '(:async t))))))
         (harness
          (e-harness-create
           :backend backend
           :default-options '(:model "async-candidate-model"
                              :provider-anchor-provider-id fake)))
         (session-id "async-candidate"))
    (e-harness-create-session harness :id session-id)
    (let* ((session (e-session-local-state (e-harness-sessions harness) session-id))
           (generation-entry
           (e-session-append-context-generation
             (e-harness-sessions harness) session-id
             (e-context-lifetime-generation-create
              :id "generation:async-candidate"
              :checkpoint '((:role system :content "C0"))
              :covered-session-boundary (plist-get session :root-event-id)))))
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role user :content "async before"))
      (e-harness-context-provider-compaction-start
       harness session-id generation-entry)
      (should done-callback)
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role assistant :content "async late"))
      (funcall done-callback '(:output ((:type "opaque")) :usage nil))
      (let* ((context (e-harness-turn-context harness session-id "async-after"))
             (options (plist-get context :options))
             (delta (plist-get options :provider-compaction-delta-messages)))
        (should (equal (plist-get options :provider-compaction-output)
                       '((:type "opaque"))))
        (should (= (cl-count "async late" delta
                             :key (lambda (message)
                                    (plist-get message :content))
                             :test #'equal)
                   1))))))

(ert-deftest e-harness-test-provider-compaction-async-generation-order-is-fenced ()
  "An older async compact result cannot replace a newer generation candidate."
  (let* ((callbacks nil)
         (backend
          (e-backend-fake-create
           :items nil
           :context-capabilities
           '(:continuation none :provider-compaction opaque)
           :provider-compaction
           (cl-function
            (lambda (&key on-done &allow-other-keys)
              (push on-done callbacks)
              (e-backend-request-create :metadata '(:async t))))))
         (store (e-session-store-create))
         (harness (e-harness-create :backend backend :sessions store
                                    :default-options
                                    '(:model "generation-order-model"
                                      :provider-anchor-provider-id fake)))
         (session-id "provider-generation-order"))
    (e-harness-create-session harness :id session-id)
    (let* ((session (e-session-local-state store session-id))
           (root (plist-get session :root-event-id))
           (generation-a
            (e-session-append-context-generation
             store session-id
             (e-context-lifetime-generation-create
              :id "generation:async-a"
              :checkpoint '((:role system :content "C-A"))
              :covered-session-boundary root))))
      (e-harness-context-provider-compaction-start
       harness session-id generation-a)
      (let* ((head-a (plist-get
                      (car (last (e-session-local-current-path store session-id)))
                      :id))
             (generation-b
              (e-session-append-context-generation
               store session-id
               (e-context-lifetime-generation-create
                :id "generation:async-b"
                :checkpoint '((:role system :content "C-B"))
                :covered-session-boundary head-a))))
        (e-harness-context-provider-compaction-start
         harness session-id generation-b)
        (should (= (length callbacks) 2))
        ;; PUSH stores B first.  A's callback arrives late and is fenced by
        ;; the current session generation check before any puthash.
        (funcall (car callbacks)
                 '(:output ((:type "opaque" :marker "RESULT-B"))
                   :usage nil))
        (funcall (cadr callbacks)
                 '(:output ((:type "opaque" :marker "RESULT-A"))
                   :usage nil))
        (should (equal
                 (plist-get
                  (gethash session-id
                           (e-harness-provider-compaction-candidates harness))
                  :output)
                 '((:type "opaque" :marker "RESULT-B"))))))))

(provide 'e-harness-context-lifetime-composition-test)

;;; e-harness-context-lifetime-composition-test.el ends here

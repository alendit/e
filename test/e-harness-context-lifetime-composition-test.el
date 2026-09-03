;;; e-harness-context-lifetime-composition-test.el --- Public harness context-lifetime composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Context-lifetime and provider-compaction restart/composition scenarios.

;;; Code:

(require 'ert)
(require 'e-sqlite-test-store-support
         (expand-file-name
          "e-sqlite-test-store-support.el"
          (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

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
                            (list :type 'context-curate
                                  :arguments
                                  '(:keep nil
                                    :summaries
                                    ((:sources (1)
                                      :text "selected durable fact")))))
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
             (roles (mapcar (lambda (message) (plist-get message :role))
                            second-messages)))
        (should (plist-get first-options :context-lifetime-enabled))
        (should (equal (mapcar (lambda (message) (plist-get message :role))
                               first-messages)
                       '(user system system)))
        (should (equal (plist-get (nth 1 first-messages) :content)
                       "[ephemeral context source 1, ~5 tokens]"))
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
        (should (equal
                 (plist-get
                  (plist-get
                   (seq-find
                    (lambda (message)
                      (eq (plist-get message :role) 'tool))
                    second-messages)
                   :content)
                  :tool-call-id)
                 "call-context-lifetime")))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (let* ((store (e-harness-sessions harness))
               (projection (e-session-context-lifetime-projection
                            store "session-1"))
               (curations (plist-get projection :curations))
               (context (e-harness-turn-context
                         harness "session-1" "next-consumer"))
               (messages (plist-get context :messages))
               (printed (prin1-to-string messages))
               (next-frame (plist-get context :lifetime-frame))
               (next-roles
                (mapcar (lambda (message) (plist-get message :role))
                        messages)))
          (should (= (length curations) 1))
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
                             "[ephemeral context source 1, ~5 tokens]"
                             source-one
                             "[ephemeral context source 2, ~14 tokens]"
                             source-two)))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :role))
                               messages)
                       '(system system user system system system system)))
        (should (< (cl-position "durable prompt" contents :test #'equal)
                   (cl-position "[ephemeral context source 1, ~5 tokens]" contents :test #'equal)))
        (should (< (cl-position "[ephemeral context source 1, ~5 tokens]" contents :test #'equal)
                   first-source-index))
        (should (< first-source-index
                   (cl-position "[ephemeral context source 2, ~14 tokens]" contents :test #'equal)))
        (should (< (cl-position "[ephemeral context source 2, ~14 tokens]" contents :test #'equal)
                   second-source-index))
        (should (= (cl-count source-one contents :test #'equal) 1))
        (should (= (cl-count source-two contents :test #'equal) 1))
        (should-not (string-match-p
                     "frame\|generation\|observation\|backing\|replay"
                     printed))))))

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
                 :provider-followup-messages (copy-tree messages))))
         (turn-messages (plist-get projection :turn-messages))
         (followup-messages (plist-get projection
                                      :provider-followup-messages)))
    (should (equal
             (mapcar (lambda (source) (plist-get source :label))
                     (e-context-lifetime-frame-curation-presentation frame))
             '(1 2 3)))
    (dolist (projected (list turn-messages followup-messages))
      (let ((contents (mapcar (lambda (message)
                                (plist-get message :content))
                              projected)))
        (should (equal (car contents) "STABLE"))
        (should (string-match-p "\\[ephemeral context source 3, ~[0-9]+ tokens\\]"
                                (cadr contents)))
        (should (equal
                 (plist-get (plist-get (caddr projected) :content) :content)
                 "DESCENDANT-TOOL-SOURCE"))
        (should (= (cl-count (caddr projected) projected :test #'equal)
                   1))))))

(ert-deftest e-harness-test-context-lifetime-disabled-keeps-default-path ()
  "The opt-in boundary leaves the existing path without a runtime frame."
  "The opt-in boundary leaves the existing path without a runtime frame."
  (let* ((captured-options nil)
         (backend
          (e-backend-create
           :name "context-lifetime-disabled"
           :stream
           (cl-function
            (lambda (&key messages options on-item &allow-other-keys)
              (ignore messages)
              (setq captured-options (copy-tree options))
              (funcall on-item
                       '(:type assistant-message :content "ordinary answer"))
              (funcall on-item '(:type done :reason stop))))))
         (harness (e-harness-create :backend backend)))
    (let ((e-context-lifetime-shadow-projection-enabled nil))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-batch harness "session-1" "ordinary"))
    (should-not (plist-get captured-options :context-lifetime-enabled))
    (should-not (e-session-context-generations
                 (e-harness-sessions harness) "session-1"))
    (should-not (e-session-context-promotions
                 (e-harness-sessions harness) "session-1"))))

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
              :headers ((:name "server" :value "nginx")
                        (:name "content-type" :value "text/html"))
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
                      :arguments (:stated_purpose "Inspect the raw result."
                                  :target "raw")))
                   (funcall on-item '(:type done :reason tool-use)))
                  (2
                   (setq curation-input
                         (list :type 'context-curate
                               :arguments
                               '(:keep nil
                                 :summaries
                                 ((:sources (2)
                                   :text "selected from tool result")))
                               :provider-replay-items
                               '((:type provider-replay-item
                                 :provider-id openai
                                 :item (:type "function_call"
                                        :call_id "curation-call"
                                        :name "context-curate"
                                        :arguments "{}"))
                                (:type provider-replay-item
                                 :provider-id openai
                                 :item (:type "function_call_output"
                                        :call_id "curation-call"
                                        :output "")))))
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
                         "\\[ephemeral context source 2, ~[0-9]+ tokens\\]"
                         (plist-get (nth (1- tool-position) messages-b)
                                    :content))))
              (should (equal curation-arguments
                             '(:keep nil
                               :summaries
                               ((:sources (2)
                                 :text "selected from tool result")))))
              (should tool-message-c)
              (should (equal (mapcar (lambda (item)
                                       (plist-get (plist-get item :item) :type))
                                     replay-items-c)
                             '("function_call" "function_call_output")))
              (should (integerp curation-call-position))
              (should (integerp curation-ack-position))
              (should (< curation-call-position curation-ack-position)))
            (should (equal (mapcar (lambda (message) (plist-get message :role))
                                   (e-harness-messages harness "session-1"))
                           '(user tool-call tool assistant)))
            (let* ((projection (e-session-context-lifetime-projection
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
                     (e-session-activity-events store "session-1")))
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
                           (e-session-messages store "session-1")))
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
                      (car (e-session-context-curations reopened "session-1")))
                     (reopened-control
                      (e-session-entry-by-id reopened "session-1" control-id))
                     (fork (e-session-fork reopened "session-1"))
                     (fork-id (plist-get fork :id))
                     (fork-projection
                      (e-session-context-lifetime-projection reopened fork-id))
                     (fork-generation (plist-get fork-projection :generation))
                     (fork-text
                      (prin1-to-string
                       (e-context-lifetime-generation-checkpoint
                        fork-generation))))
                (should (equal control-id
                               (plist-get reopened-record :response-entry-id)))
                (should (equal (e-session-entry-by-id reopened
                                                      "session-1" control-id)
                               reopened-control))
                (should (string-match-p "selected from tool result" fork-text))
                (should-not (string-match-p raw-result-marker fork-text))
                (should-not (string-match-p control-id fork-text))
                (should-not (e-session-entry-by-id reopened fork-id control-id)))))
        (delete-directory directory t)))))

(ert-deftest e-harness-test-context-lifetime-preflights-before-assistant-append ()
  "Completion-only curation failures do not append an assistant or consume its frame."
  (e-harness-test--with-empty-layer-registry
             (dolist (case '((unknown-label . (:keep (2) :summaries nil))
                    (oversized-record . (:keep (1) :summaries nil))))
      (let* ((request-count 0)
             (source-value "PREFLIGHT-SOURCE")
             (captured-frame nil)
             (prepared-bytes nil)
             (backend
              (e-backend-create
               :name "context-lifetime-completion-preflight"
               :context-capabilities
               '(:continuation none
                 :observation-delivery request-local-replaceable
                 :reserved-effect-carrier context-curate-wire)
               :stream
               (cl-function
                (lambda (&key on-item &allow-other-keys)
                  (cl-incf request-count)
                  (let ((arguments
                         (if (eq (car case) 'oversized-record)
                             (let ((text (make-string 9000 ?x)))
                               ;; Keep the size assertion in the test while
                               ;; leaving canonical record construction to the
                               ;; context-lifetime owner.
                               (setq prepared-bytes
                                     (string-bytes
                                      (encode-coding-string text 'utf-8 t)))
                               (list :keep nil
                                     :summaries
                                     (list (list :sources '(1)
                                                 :text text))))
                           (cdr case))))
                    (funcall on-item
                             (list :type 'context-curate
                                   :arguments arguments)))
                  (funcall on-item
                           '(:type assistant-message
                             :content "MUST-NOT-PERSIST"))
                  (funcall on-item '(:type done :reason stop))))))
             (provider
              (e-context-provider-create
               :name 'completion-preflight-source
               :cache-placement 'dynamic-context
               :build (lambda (&rest _)
                        (list (list :role 'system :content source-value)))))
             (capability
              (e-capability-create
               :id 'completion-preflight-capability
               :context-providers (list provider)))
             (harness
              (e-harness-create
               :backend backend
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
            (should-error
             (e-harness-test-prompt-batch
              harness "completion-preflight" "trigger curation")
             :type 'e-context-lifetime-invalid-record)))
        (should (= request-count 1))
        (when (eq (car case) 'oversized-record)
          (should (> prepared-bytes
                     e-context-lifetime-curation-max-record-bytes)))
        (should (e-context-lifetime-frame-p captured-frame))
        (should-not (e-context-lifetime-frame-consumed-p captured-frame))
        (should (equal
                 (plist-get
                  (car (e-context-lifetime-frame-observations captured-frame))
                  :body)
                 (list :role 'system :content source-value)))
        (should-not (e-session-context-curations
                     (e-harness-sessions harness) "completion-preflight"))
        (should-not
         (seq-find (lambda (message)
                     (eq (plist-get message :role) 'assistant))
                   (e-harness-messages harness "completion-preflight")))))))

(ert-deftest e-harness-test-context-lifetime-assistant-curation-preserves-response-entry-id ()
  "A prepared curation and its assistant share one durable response identity."
  (e-harness-test--with-empty-layer-registry
    (let* ((directory (make-temp-file "e-harness-response-id-" t))
           (store (e-session-persistent-store-create directory))
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
              (lambda (&key on-item &allow-other-keys)
                (funcall on-item
                         '(:type context-curate
                           :arguments (:keep (1) :summaries nil)))
                (funcall on-item
                         '(:type assistant-message :content "selected"))
                (funcall on-item '(:type done :reason stop))))))
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
                   (assistant (car (last (seq-filter
                                          (lambda (message)
                                            (eq (plist-get message :role) 'assistant))
                                          messages))))
                   (record (car (e-session-context-curations
                                 store "assistant-response-id")))
                   (event (seq-find
                           (lambda (entry)
                             (eq (plist-get entry :type)
                                 'context-frame-consumed))
                           events))
                   (response-id (plist-get assistant :id))
                   (reopened (e-session-persistent-store-create directory)))
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
                      (car (e-session-context-curations
                            reopened "assistant-response-id")))
                     (reopened-assistant
                      (seq-find (lambda (message)
                                  (equal (plist-get message :id) response-id))
                                (e-session-messages reopened
                                                     "assistant-response-id")))
                     (fork (e-session-fork reopened "assistant-response-id"))
                     (fork-id (plist-get fork :id))
                     (fork-projection
                      (e-session-context-lifetime-projection reopened fork-id))
                     (fork-generation (plist-get fork-projection :generation))
                     (source-record-after-fork
                      (car (e-session-context-curations
                            reopened "assistant-response-id")))
                     (source-assistant-after-fork
                      (seq-find (lambda (message)
                                  (equal (plist-get message :id) response-id))
                                (e-session-messages reopened
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
         frame entry)
    (e-harness-create-session harness :id "session-1")
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
    (should-not (e-session-context-curations store "session-1"))))

(ert-deftest e-harness-test-context-lifetime-invalid-curation-keeps-live-frame ()
  "Preparation failure leaves both the live source body and session untouched."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         (frame (e-harness-test--curation-frame))
         (entry (list :status 'running :context-frame frame)))
    (e-harness-create-session harness :id "session-1")
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (should-error
       (e-harness-context-lifetime-commit-response
        harness "session-1" "turn-1" entry
        (list :frame frame
               :provider-request-id "response-1"
               :curation-effects
               (list (list :type 'context-curate
                           :arguments
                           '(:keep (2) :summaries nil)))))
       :type 'e-context-lifetime-invalid-record))
    (should (eq (plist-get entry :context-frame) frame))
    (should-not (e-context-lifetime-frame-consumed-p frame))
    (should (equal (plist-get (car (e-context-lifetime-frame-observations frame))
                              :body)
                   '(:role system :content "HARNESS-EXACT-VALUE")))
    (should-not (e-session-context-curations store "session-1"))))

(ert-deftest e-harness-test-context-lifetime-steering-rebuilds-after-follow-up ()
  "Steering after a tool follow-up uses the fresh projection exactly once."
  (e-harness-test--with-empty-layer-registry
    (let (backend provider capability harness turn-id
          (request-count 0) requests finish-b raw-result)
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
                               :provider-id fake
                               :item (:type "steering-replay"
                                      :id "REPLAY-STEERING")))
                    (funcall on-item
                             '(:type tool-call
                               :id "call-steering"
                               :name "inspect-steering"
                               :arguments (:stated_purpose "Inspect steering.")))
                    (funcall on-item
                             '(:type provider-anchor-candidate
                               :provider-id fake
                               :metadata (:response-id "resp-A")))
                    (funcall on-item '(:type done :reason tool-use))
                    (funcall on-done '(:status done)))
                   ((= ordinal 2)
                    ;; Leave B in flight so steering is queued while its
                    ;; consumed frame has not yet been finalized.
                    (setq finish-b
                          (lambda ()
                            (funcall
                             on-item
                             (list :type 'context-curate
                                   :arguments
                                   '(:keep nil
                                     :summaries
                                     ((:sources (2)
                                       :text "selected after B")))))
                              (funcall on-item
                                       '(:type assistant-message
                                         :content "B"))
                              (funcall on-item
                                       '(:type provider-anchor-candidate
                                         :provider-id fake
                                         :metadata (:response-id "resp-B")))
                              (funcall on-item '(:type done :reason stop))
                              (funcall on-done '(:status done)))))
                   ((= ordinal 3)
                    (funcall on-item
                             '(:type assistant-message :content "C"))
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
             :default-options '(:provider-continuation t
                                :provider-anchor-provider-id fake)))
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
             (messages-b (plist-get request-b :messages))
             (messages-c (plist-get request-c :messages))
             (printed-b (prin1-to-string messages-b))
             (printed-c (prin1-to-string messages-c)))
        (should (= request-count 3))
        (should (string-match-p raw-result printed-b))
        (should (string-match-p "call-steering" printed-b))
        (should (string-match-p "REPLAY-STEERING" printed-b))
        (should (string-match-p "selected after B" printed-c))
        (should (string-match-p "steer now" printed-c))
        (dolist (marker (list raw-result "call-steering" "REPLAY-STEERING"
                               "resp-A" "resp-B"))
          (should-not (string-match-p marker printed-c)))
        (should-not
         (e-session-provider-anchors
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
         (list (e-session-messages (e-harness-sessions harness)
                                   "provider-session")
               (e-session-activity-events
                (e-harness-sessions harness) "provider-session")
               (e-session-context-generations
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
    (let* ((session (e-session-get (e-harness-sessions harness) session-id))
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
    (let* ((session (e-session-get store session-id))
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
                      (car (last (e-session-current-path store session-id)))
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

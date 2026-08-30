;;; e-harness-tool-composition-test.el --- Public harness tool/activity composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tool lifecycle, activity durability, provider telemetry, and board-adapter
;; composition scenarios.

;;; Code:

(require 'ert)
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-harness-test-durable-activity-provenance-reaches-subscribers ()
  "A public durable event names the activity entry written before it."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (events nil))
    (e-harness-create-session harness :id "session-1")
    (e-harness-activity-subscribe harness (lambda (event) (push event events))
                         :session-id "session-1")
    (e-harness-activity-emit-turn-event
     harness "session-1" "turn-1" 'tool-started '(:name "read"))
    (let* ((event (car events))
           (activity (car (e-harness-session-activity-events harness "session-1"))))
      (should (equal (plist-get event :payload) '(:name "read")))
      (should (equal (plist-get event :activity-entry-id)
                     (plist-get activity :id)))
      (should (= (plist-get event :board-activity-sequence)
                 (plist-get activity :board-activity-sequence))))))

(ert-deftest e-harness-test-abort-cancels-active-provider-request ()
  "Aborting an active async provider call cancels its request handle."
  (let* ((cancelled nil)
         (backend
          (e-backend-create
           :name "cancellable"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (e-backend-note-request-started
               (e-backend-request-create
                :cancel (lambda ()
                          (setq cancelled t)
                          t)))
              (while (not cancelled)
                (accept-process-output nil 0.01))
              (funcall on-item '(:type done :reason cancelled))))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (run-at-time 0.01 nil (lambda ()
                            (e-harness-test-abort harness "session-1")))
    (should (equal (plist-get (e-harness-wait-batch harness "session-1" 1.0)
                              :status)
                   'cancelled))
    (should cancelled)
    (should (member 'turn-cancelled
                    (mapcar (lambda (event) (plist-get event :type))
                            events)))))

(ert-deftest e-harness-test-tool-finished-activity-drops-unknown-metadata ()
  "Invalid purpose status is durable without persisting invalid text."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (let ((e-harness-activity-trusted-tool-details-uri "tmp://safe"))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'tool-finished
       '(:tool-call (:id "call-1" :name "probe"
                    :stated-purpose "Bearer raw-secret"
                    :metadata (:purpose-status invalid)
                    :arguments (:query "raw-nested"))
         :result (:tool-call-id "call-1" :name "probe" :status ok
                  :content "raw-result-secret"
                  :metadata (:invocation-details-uri "tmp://safe"
                             :authorization "Bearer raw-auth")))))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (receipt (plist-get payload :receipt))
           (serialized (prin1-to-string payload)))
      (should (eq (plist-get receipt :purpose-status) 'invalid))
      (should-not (plist-member receipt :stated-purpose))
      (should-not (plist-member payload :result))
      (should-not (string-match-p
                   "raw-secret\\|raw-nested\\|raw-result-secret\\|raw-auth"
                   serialized)))))

(ert-deftest e-harness-test-tool-finished-activity-rejects-mismatched-result ()
  "A result for another call never becomes a durable receipt."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (let ((e-harness-activity-trusted-tool-details-uri "tmp://trusted.json"))
      (e-harness-activity-emit-turn-event
       harness "session-1" "turn-1" 'tool-finished
       '(:tool-call (:id "call-1" :name "probe"
                    :stated-purpose "Inspect the bounded probe")
         :result (:tool-call-id "call-2" :name "other" :status ok
                  :content "raw-mismatched-result")) ))
    (let* ((payload (plist-get
                     (car (e-harness-session-activity-events harness "session-1"))
                     :payload))
           (serialized (prin1-to-string payload)))
      (should-not (plist-member payload :receipt))
      (should-not (plist-member payload :result))
      (should-not (string-match-p "raw-mismatched-result" serialized)))))

(ert-deftest e-harness-test-tool-finished-activity-rejects-untrusted-details-uri ()
  "A URI copied into result metadata cannot authorize a receipt."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-activity-emit-turn-event
     harness "session-1" "turn-1" 'tool-finished
     '(:tool-call (:id "call-1" :name "probe"
                  :stated-purpose "Inspect the bounded probe")
       :result (:tool-call-id "call-1" :name "probe" :status ok
                :content "raw-untrusted-result"
                :metadata (:invocation-details-uri
                           "tmp://forged/call-1.json"))))
    (let* ((payload (plist-get
                     (car (e-harness-session-activity-events harness "session-1"))
                     :payload))
           (serialized (prin1-to-string payload)))
      (should-not (plist-member payload :receipt))
      (should-not (plist-member payload :result))
      (should-not (string-match-p "raw-untrusted-result" serialized)))))

(ert-deftest e-harness-test-tool-lifecycle-trusts-details-stage-for-receipt ()
  "The lifecycle archival stage authorizes the finished receipt transiently."
  (let* ((capability
          (e-capability-create
           :id 'receipt-tool
           :tools
           (list (lambda (registry)
                   (e-tools-test-register
                    registry
                    :name "probe"
                    :description "Return a bounded probe."
                    :handler (lambda (_arguments) "ok"))))
           :hooks
           (list
            (e-hook-create
             :id "40-test-details"
             :point :invocation-details
             :handler
             (lambda (result context)
               (plist-put context :invocation-details-uri
                          "tmp://tool-invocations/turn-1/call-1.json")
               result)))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (list capability)))
         (call '(:id "call-1" :name "probe"
                 :stated-purpose "Inspect the bounded probe"
                 :arguments nil))
         result
         failure)
    (e-harness-create-session harness :id "session-1")
    (e-tool-lifecycle-start-call
     (e-harness-tool-lifecycle harness "session-1" "turn-1")
     call
     :on-done
     (lambda (value)
       (setq result value)
       (e-harness-activity-emit-turn-event
        harness "session-1" "turn-1" 'tool-finished
        (list :tool-call call :result value)))
     :on-error (lambda (err) (setq failure err)))
    (should-not failure)
    (should (e-tools-result-p result))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (receipt (plist-get payload :receipt)))
      (should (equal (plist-get receipt :details-uri)
                     "tmp://tool-invocations/turn-1/call-1.json"))
      (should-not (plist-member payload :trusted-details-uri))
      (should-not (plist-member (plist-get payload :result)
                                :trusted-details-uri)))))

(ert-deftest e-harness-test-tool-started-activity-retains-purpose-without-arguments ()
  "Durable tool-started activity retains identity and stated purpose only."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-activity-emit-turn-event
     harness "session-1" "turn-1" 'tool-started
     '(:id "call-1" :name "probe"
       :stated-purpose "Inspect the bounded probe"
       :arguments (:query "raw-query")))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (serialized (prin1-to-string payload)))
      (should (equal payload
                     '(:id "call-1"
                       :name "probe"
                       :stated-purpose "Inspect the bounded probe")))
      (should-not (string-match-p "raw-query" serialized)))))

(ert-deftest e-harness-test-tool-receipt-survives-persistent-reopen-without-preview ()
  "Reopened durable activity keeps receipt identity without raw content."
  (let* ((directory (make-temp-file "e-harness-tool-receipt-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store)))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-harness-activity-emit-turn-event
           harness "session-1" "turn-1" 'tool-started
           '(:id "call-1" :name "bash"
             :stated-purpose "Run the bounded command"
             :arguments (:command "raw-command-secret")))
          (let ((e-harness-activity-trusted-tool-details-uri
                 "tmp://tool-invocations/turn-1/call-1.json"))
            (e-harness-activity-emit-turn-event
             harness "session-1" "turn-1" 'tool-finished
             '(:tool-call (:id "call-1" :name "bash"
                          :stated-purpose "Run the bounded command"
                          :arguments (:command "raw-command-secret"))
               :result (:tool-call-id "call-1"
                        :name "bash"
                        :status ok
                        :content "raw-result-secret"
                        :metadata (:invocation-details-uri
                                   "tmp://tool-invocations/turn-1/call-1.json")))))
          (e-session-flush-write-queue store)
          (let* ((reopened (e-session-persistent-store-create directory))
                 (events (e-session-activity-events reopened "session-1"))
                 (started (car events))
                 (finished (cadr events))
                 (started-payload (plist-get started :payload))
                 (finished-payload (plist-get finished :payload))
                 (receipt (plist-get finished-payload :receipt))
                 (serialized (prin1-to-string events)))
            (should (equal started-payload
                           '(:id "call-1"
                             :name "bash"
                             :stated-purpose "Run the bounded command")))
            (should (equal receipt
                           '(:tool-call-id "call-1"
                             :tool "bash"
                             :status "ok"
                             :stated-purpose "Run the bounded command"
                             :details-uri
                             "tmp://tool-invocations/turn-1/call-1.json"
                             :details-lifetime "session-tmp")))
            (should-not (string-match-p
                         "raw-command-secret\\|raw-result-secret"
                         serialized))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-malformed-tool-finished-activity-never-falls-back-raw ()
  "Malformed legacy payloads retain identity, not arbitrary raw values."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-activity-emit-turn-event
     harness "session-1" "turn-1" 'tool-finished
     '(:tool-call (:id "call-1" :name "legacy")
       :result (:content "token=raw-secret")
       :authorization "Bearer raw-auth"
       :unknown (:secret "raw-nested")))
    (let* ((event (car (e-harness-session-activity-events
                        harness "session-1")))
           (payload (plist-get event :payload))
           (serialized (prin1-to-string payload)))
      (should (equal (plist-get (plist-get payload :tool-call) :id) "call-1"))
      (should-not (plist-member payload :result))
      (should-not
       (string-match-p "raw-secret\\|raw-auth\\|raw-nested" serialized)))))

(ert-deftest e-harness-test-activity-events-declare-persistence-class ()
  "Every durable activity event declares why it is persisted."
  (dolist (type e-harness-activity-durable-event-types)
    (should (memq (e-harness-activity-event-class type)
                  '(audit replay presentation-log)))))

(ert-deftest e-harness-test-action-events-are-durable ()
  "Action dispatch emits durable action activity events."
  (let ((harness (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-create-session harness :id "session-1")
    (e-actions-call
     'chat-session
     :rename
     '(:name "Action telemetry")
     (list :harness harness :session-id "session-1" :turn-id "turn-1"))
    (let ((types (mapcar (lambda (event)
                           (plist-get event :event-type))
                         (e-harness-session-activity-events
                          harness "session-1"))))
      (should (equal types '(action-started action-finished))))))

(ert-deftest e-harness-test-token-usage-events-are-durable ()
  "Backend token usage events are retained in session activity."
  (let* ((backend (e-backend-fake-create
                   :items
                   '((:type assistant-message :content "answer")
                     (:type token-usage
                      :usage (:input-tokens 202598
                              :cached-input-tokens 7552
                              :cache-creation-input-tokens 4096
                              :output-tokens 419
                              :reasoning-output-tokens 139
                              :total-tokens 203017))
                     (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (let* ((events (e-harness-session-activity-events harness "session-1"))
           (usage-event
            (seq-find (lambda (event)
                        (eq (plist-get event :event-type) 'token-usage))
                      events)))
      (should usage-event)
      (let ((payload (plist-get usage-event :payload)))
        (should (equal (plist-get payload :input-tokens) 202598))
        (should (equal (plist-get payload :cached-input-tokens) 7552))
        (should (equal (plist-get payload :cache-creation-input-tokens) 4096))
        (should (equal (plist-get payload :output-tokens) 419))
        (should (equal (plist-get payload :reasoning-output-tokens) 139))
        (should (equal (plist-get payload :total-tokens) 203017))
        (should (stringp (plist-get payload :provider-request-id)))
        (should (= (plist-get payload :provider-request-ordinal) 1))))))

(ert-deftest e-harness-test-provider-telemetry-is-narrow-on-disk-and-reload ()
  "Provider lifecycle and usage cross a narrow redacted durable boundary."
  (let* ((directory (make-temp-file "e-harness-provider-telemetry-" t))
         (secret "Bearer disk-provider-secret")
         (cache-key "cache-key-disk-secret")
         (backend
          (e-backend-create
           :name "durable-provider-projection"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata
                        (list :provider secret
                              :transport 'websocket
                              :url-host "https://user:password@example.test"
                              :url-path "/responses?token=disk-path-secret"
                              :diagnostics
                              (list :model "api_key=disk-model-secret"
                                    :reasoning-effort "high"
                                    :unknown "disk-diagnostic-secret"))))
              (funcall on-item '(:type assistant-message :content "answer"))
              (funcall on-item
                       '(:type token-usage
                         :usage (:input-tokens 12 :output-tokens 3
                                 :total-tokens 15
                                 :unknown "disk-usage-secret")))
              (funcall on-item '(:type done :reason stop))
              (funcall on-done '(:status done))
              nil))))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend backend :sessions store
                   :default-options (list :prompt-cache-key cache-key))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-harness-test-prompt-batch harness "session-1" "question")
          (e-session-flush-write-queue store)
          (let ((disk
                 (with-temp-buffer
                   (insert-file-contents
                    (e-session-storage-session-reference store "session-1"))
                   (buffer-string))))
            (should (string-match-p "REDACTED" disk))
            (should-not
             (string-match-p
              "disk-provider-secret\\|password@example\\|disk-path-secret\\|disk-model-secret\\|disk-diagnostic-secret\\|disk-usage-secret\\|cache-key-disk-secret"
              disk)))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (activity (e-session-activity-events loaded "session-1"))
                 (started (seq-find
                           (lambda (event)
                             (eq (plist-get event :event-type)
                                 'provider-request-started))
                           activity))
                 (usage (seq-find
                         (lambda (event)
                           (eq (plist-get event :event-type) 'token-usage))
                         activity))
                 (started-payload (plist-get started :payload))
                 (usage-payload (plist-get usage :payload)))
            (should (equal (plist-get started-payload :provider)
                           "Bearer [REDACTED]"))
            (should-not (plist-member started-payload :request-shape))
            (should-not (plist-member
                         (plist-get started-payload :diagnostics) :unknown))
            (should (= (plist-get usage-payload :input-tokens) 12))
            (should-not (plist-member usage-payload :unknown))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-provider-anchor-candidates-are-persisted ()
  "Provider anchor candidates persist with covered entry and context metadata."
  (e-harness-test--with-empty-layer-registry
    (let* ((dynamic-provider
            (e-context-provider-create
             :name 'visible-buffer
             :build (lambda (&rest _)
                      '((:role system :content "current state")))
             :cache-placement 'dynamic-context))
           (capability
            (e-capability-create
             :id 'context-anchor-capability
             :instructions "stable instructions"
             :context-providers (list dynamic-provider)))
           (backend (e-backend-fake-create
                     :context-capabilities
                     '(:continuation linear
                       :observation-delivery request-local-replaceable)
                     :items '((:type assistant-message :content "answer")
                              (:type provider-anchor-candidate
                               :provider-id openai
                               :metadata (:response-id "resp-1"))
                              (:type done :reason stop))))
           (harness (e-harness-create
                     :backend backend
                     :enabled-layer-ids '(context-anchor-layer)
                     :default-options '(:model "gpt-test"
                                        :provider-continuation t
                                        :provider-anchor-provider-id openai))))
      (e-layer-register
       (e-layer-spec-create
        :id 'context-anchor-layer
        :name "Context Anchor Layer"
        :factory (lambda ()
                   (e-layer-create
                    :id 'context-anchor-layer
                    :name "Context Anchor Layer"
                    :capabilities (list capability)))))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-batch harness "session-1" "question")
      (let* ((messages (e-harness-messages harness "session-1"))
             (assistant (cl-find 'assistant messages
                                 :key (lambda (message)
                                        (plist-get message :role))))
             (anchors (e-session-provider-anchors
                       (e-harness-sessions harness)
                       "session-1"))
             (anchor (car anchors))
             (fingerprints (plist-get anchor :fingerprints)))
        (should (= (length anchors) 1))
        (should (eq (plist-get anchor :provider-id) 'openai))
        (should (equal (plist-get anchor :model) "gpt-test"))
        (should (equal (plist-get anchor :covered-entry-id)
                       (plist-get assistant :id)))
        (should (equal (plist-get anchor :metadata)
                       '(:response-id "resp-1")))
        (should (equal (mapcar (lambda (fingerprint)
                                 (plist-get fingerprint :kind))
                               (plist-get fingerprints :segments))
                       '("static-prefix")))
        (should-not (plist-member fingerprints :current-state-fingerprint))
        (should (equal (plist-get fingerprints :active-layer-ids)
                       '("context-anchor-layer")))
        (should (equal (plist-get fingerprints :reasoning)
                       '(:reasoning nil :reasoning-effort nil :effort nil)))
        (dolist (fingerprint (plist-get fingerprints :segments))
          (should (stringp (plist-get fingerprint :id)))
          (should (stringp (plist-get fingerprint :fingerprint))))))))

(ert-deftest e-harness-test-provider-anchor-persistence-keeps-latest-candidate ()
  "Provider anchor persistence keeps only the final candidate for a provider."
  (e-harness-test--with-empty-layer-registry
    (let* ((backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items '((:type assistant-message :content "answer")
                              (:type provider-anchor-candidate
                               :provider-id openai
                               :metadata (:response-id "resp-old"))
                              (:type provider-anchor-candidate
                               :provider-id openai
                               :metadata (:response-id "resp-new"))
                              (:type done :reason stop))))
           (harness (e-harness-create
                     :backend backend
                     :default-options '(:model "gpt-test"
                                        :provider-continuation t
                                        :provider-anchor-provider-id openai))))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-batch harness "session-1" "question")
      (let ((anchors (e-session-provider-anchors
                      (e-harness-sessions harness)
                      "session-1")))
        (should (= (length anchors) 1))
        (should (equal (plist-get (plist-get (car anchors) :metadata)
                                  :response-id)
                       "resp-new"))))))

(defun e-harness-test--run-final-refresh-anchor-scenario
    (kind second-candidate-p)
  "Run a real harness refresh scenario for identity KIND.
Return request options, persisted anchors, and the final context."
  (let* ((stable-content "STABLE-A")
         (current-state "STATE-A")
         (tool-version "TOOL-A")
         (request-count 0)
         (requests nil)
         (harness nil)
         (backend
          (e-backend-create
           :name (format "anchor-refresh-%s" kind)
           :context-capabilities
           '(:continuation linear
             :observation-delivery request-local-replaceable)
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages)
              (push (copy-tree options) requests)
              (cl-incf request-count)
              (if (= request-count 1)
                  (progn
                    (funcall
                     on-item
                     '(:type tool-call
                       :id "refresh-1"
                       :name "refresh-anchor"
                       :arguments (:stated_purpose "Refresh the anchor.")))
                    (funcall
                     on-item
                     '(:type provider-anchor-candidate
                       :provider-id openai
                       :metadata (:response-id "resp-A")))
                    (funcall on-item '(:type done :reason stop)))
                (funcall on-item
                         '(:type assistant-message :content "answer-B"))
                (when second-candidate-p
                  (funcall
                   on-item
                   '(:type provider-anchor-candidate
                     :provider-id openai
                     :metadata (:response-id "resp-B"))))
                (funcall on-item '(:type done :reason stop)))))))
         (stable-provider
          (e-context-provider-create
           :name 'anchor-refresh-stable
           :cache-placement 'stable-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content stable-content)))))
         (dynamic-provider
          (e-context-provider-create
           :name 'anchor-refresh-current
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (capability
          (e-capability-create
           :id 'anchor-refresh-capability
           :instructions "stable policy"
           :context-providers (list stable-provider dynamic-provider)
           :tools
           (list
            (lambda (registry)
              (e-tools-register
               registry
               :name "refresh-anchor"
               :description (format "Refresh %s" tool-version)
               :work
               (e-tools-cheap-work
                "test.anchor-refresh"
                (lambda (_arguments)
                  (pcase kind
                    ('stable
                     (setq stable-content "STABLE-B"))
                    ('current-state
                     (setq current-state "STATE-B"))
                    ('tool-schema
                     (setq tool-version "TOOL-B"))
                    ('provider-option
                     (setf (e-harness-default-options harness)
                           (plist-put
                            (copy-sequence
                             (e-harness-default-options harness))
                            :max-tokens
                            123)))
                    ('compaction
                     (let* ((store (e-harness-sessions harness))
                            (first-entry
                             (car (e-session-current-path
                                   store "session-1"))))
                       (e-session-append-compaction
                        store "session-1" "refresh summary"
                        :first-kept-entry-id
                        (plist-get first-entry :id)))))
                  (e-tools-result-create
                   (plist-get (e-tools-current-context) :tool-call)
                   'ok
                   "refreshed"
                   '(:refresh-context t)))))))))
         )
    (setq harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list capability)
           :default-options
           '(:model "gpt-test"
             :provider-continuation t
             :provider-anchor-provider-id openai)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "refresh")
    (list :requests (nreverse requests)
          :anchors (e-session-provider-anchors
                    (e-harness-sessions harness) "session-1")
          :context (e-harness-turn-context
                    harness "session-1" "after-refresh"))))

(ert-deftest e-harness-test-provider-anchor-reversion-selects-matching-old-fingerprint ()
  "A later reversion selects an old compatible anchor, never a newer mismatch."
  (e-harness-test--with-empty-layer-registry
    (let* ((stable-content "STABLE-A")
           (request-count 0)
           (requests nil)
           (backend
            (e-backend-create
             :name "anchor-reversion"
             :context-capabilities
             '(:continuation linear
               :observation-delivery request-local-replaceable)
             :stream
             (cl-function
              (lambda (&key messages options on-item)
                (ignore messages)
                (push (copy-tree options) requests)
                (cl-incf request-count)
                (funcall on-item
                         (list :type 'assistant-message
                               :content (format "answer-%s" request-count)))
                (funcall on-item
                         (list :type 'provider-anchor-candidate
                               :provider-id 'openai
                               :metadata
                               (list :response-id
                                     (format "resp-%s" request-count))))
                (funcall on-item '(:type done :reason stop))))))
           (stable-provider
            (e-context-provider-create
             :name 'anchor-reversion-stable
             :cache-placement 'stable-context
             :build (lambda (&rest _)
                      (list (list :role 'system :content stable-content)))))
           (capability
            (e-capability-create
             :id 'anchor-reversion-capability
             :instructions "stable policy"
             :context-providers (list stable-provider)))
           (harness
            (e-harness-create
             :backend backend
             :intrinsic-capabilities (list capability)
             :default-options
             '(:model "gpt-test"
               :provider-continuation t
               :provider-anchor-provider-id openai))))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-batch harness "session-1" "one")
      (setq stable-content "STABLE-B")
      (e-harness-test-prompt-batch harness "session-1" "two")
      (setq stable-content "STABLE-A")
      (e-harness-test-prompt-batch harness "session-1" "three")
      (let* ((ordered (nreverse requests))
             (third (nth 2 ordered))
             (anchors (e-session-provider-anchors
                       (e-harness-sessions harness) "session-1")))
        (should (= (length ordered) 3))
        (should (equal
                 (plist-get (plist-get third :provider-anchor) :metadata)
                 '(:response-id "resp-1")))
        (should (= (length anchors) 3))
        (should (equal (mapcar (lambda (anchor)
                                (plist-get (plist-get anchor :metadata)
                                           :response-id))
                              anchors)
                       '("resp-1" "resp-2" "resp-3")))))))

(ert-deftest e-harness-test-provider-request-lifecycle-events-are-durable ()
  "Provider request lifecycle events are retained in ordered session activity."
  (let* ((backend
          (e-backend-create
           :name "durable-lifecycle"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata '(:provider codex
                                    :transport url-retrieve
                                    :url-host "example.test"
                                    :url-path "/codex/responses"
                                    :timeout-seconds 180)))
              (funcall on-item '(:type assistant-message :content "answer"))
              (funcall on-item '(:type done :reason stop))
              (funcall on-done '(:status done))
              nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "question")
    (let* ((activity (e-harness-session-activity-events harness "session-1"))
           (types (mapcar (lambda (event)
                            (plist-get event :event-type))
                          activity))
           (started (seq-find
                     (lambda (event)
                       (eq (plist-get event :event-type)
                           'provider-request-started))
                     activity))
           (finished (seq-find
                      (lambda (event)
                        (eq (plist-get event :event-type)
                            'provider-request-finished))
                      activity)))
      (should (equal types
                     '(turn-started
                       provider-request-started
                       provider-request-finished
                       turn-finished)))
      (should (equal (plist-get (plist-get started :payload) :provider)
                     'codex))
      (should (equal (plist-get (plist-get finished :payload) :status)
                     'done))
      (should (numberp (plist-get (plist-get finished :payload)
                                  :elapsed-seconds))))))

(ert-deftest e-harness-test-provider-deadline-default-settles-stalled-turn ()
  "A provider request with no callbacks fails visibly through the Work deadline."
  (let* ((e-harness-provider-request-deadline-seconds 0.02)
         (cancelled nil)
         (backend
          (e-backend-create
           :name "stalled-harness-provider"
           :start
           (cl-function
            (lambda (&key on-request-start &allow-other-keys)
              (let ((request
                     (e-backend-request-create
                      :cancel (lambda ()
                                (setq cancelled t)
                                t)
                      :metadata '(:provider fake :transport timer))))
                (funcall on-request-start request)
                request)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'error))
      (should (string-match-p "deadline" (plist-get settled :error)))
      (should (eq (plist-get (plist-get settled :error-details) :execution)
                  'backend)))
    (should cancelled)
    (let* ((types (mapcar (lambda (event) (plist-get event :type))
                          events))
           (failed (seq-find
                    (lambda (event)
                      (eq (plist-get event :type) 'turn-failed))
                    events))
           (provider-finished
            (seq-find
             (lambda (event)
               (eq (plist-get event :type) 'provider-request-finished))
             events)))
      (should (member 'provider-request-started types))
      (should (member 'provider-request-finished types))
      (should (member 'turn-failed types))
      (should (eq (plist-get (plist-get provider-finished :payload) :status)
                  'error))
      (should (eq (plist-get (plist-get (plist-get failed :payload)
                                        :details)
                             :execution)
                  'backend)))))

(ert-deftest e-harness-test-provider-timeout-retries-then-succeeds ()
  "Provider timeout errors retry with backoff instead of failing immediately."
  (let* ((e-harness-retry-initial-backoff-seconds 0.02)
         (e-harness-retry-backoff-multiplier 1.0)
         (e-harness-retry-max-backoff-seconds 0.02)
         (attempts 0)
         (events nil)
         (backend
          (e-backend-create
           :name "timeout"
           :normalize-error-details
           (lambda (_message details condition)
             (append details
                     (when (eq (car-safe condition) 'e-openai-request-timeout)
                       '(:retryable t :retry-reason timeout))))
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options)
              (cl-incf attempts)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata '(:provider codex
                                    :transport url-retrieve
                                    :url-host "example.test"
                                    :url-path "/codex/responses"
                                    :timeout-seconds 60)))
              (run-at-time
               0 nil
               (lambda ()
                 (if (= attempts 1)
                     (funcall on-error
                              '(e-openai-request-timeout
                                "request timed out"))
                   (funcall on-item
                            '(:type assistant-message :content "recovered"))
                   (funcall on-item '(:type done :reason stop))
                   (funcall on-done '(:status done)))))
              nil))))
         (harness (e-harness-create :backend backend)))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (let ((settled (e-harness-wait-batch harness "session-1" 1.0)))
      (should (equal (plist-get settled :status) 'done))
      (should (= attempts 2)))
    (let ((types (mapcar (lambda (event) (plist-get event :type)) events)))
      (should (member 'turn-retrying types))
      (should (member 'turn-finished types))
      (should-not (member 'turn-failed types))
      (should-not (member 'turn-cancelled types)))))

(ert-deftest e-harness-test-abort-ignores-stale-provider-callbacks ()
  "Provider callbacks that arrive after abort do not mutate session state."
  (let* ((callbacks nil)
         (cancelled nil)
         (backend (e-backend-create
                   :name "stale-callback"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore messages options on-error)
                      (setq callbacks (list :on-item on-item
                                            :on-done on-done))
                      (let ((request
                             (e-backend-request-create
                              :cancel (lambda ()
                                        (setq cancelled t)
                                        t))))
                        (funcall on-request-start request)
                        request)))))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-async harness "session-1" "question")
    (e-harness-test-abort harness "session-1")
    (funcall (plist-get callbacks :on-item)
             '(:type assistant-message :content "late answer"))
    (funcall (plist-get callbacks :on-item)
             '(:type done :reason stop))
    (funcall (plist-get callbacks :on-done) '(:status done))
    (should (equal (plist-get (e-harness-wait-batch harness "session-1" 0.1)
                              :status)
                   'cancelled))
    (should cancelled)
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user)))
    (should-not (member 'turn-finished
                        (mapcar (lambda (event) (plist-get event :type))
                                events)))))

(ert-deftest e-harness-test-abort-cancels-active-tool-request ()
  "Aborting during async tool execution cancels the active tool request."
  (let* ((tool-callbacks nil)
         (tool-cancelled nil)
         (backend (e-backend-create
                   :name "tool-abort"
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done on-error
                                   on-request-start)
                      (ignore messages options on-error on-request-start)
                      (funcall on-item
                               '(:type tool-call
                                 :id "call-1"
                                 :name "held-tool"
                                 :arguments (:stated_purpose "Hold the request."
                                             :text "hi")))
                      (funcall on-item '(:type done :reason tool-use))
                      (funcall on-done '(:status done))
                      nil))))
         (tools (e-tools-registry-create))
         (harness (e-harness-create :backend backend))
         (events nil))
    (e-tools-test-register tools
                      :name "held-tool"
                      :description "Hold."
                      :start
                      (cl-function
                       (lambda (&key arguments on-done on-error
                                      on-request-start)
                         (ignore arguments on-error)
                         (setq tool-callbacks (list :on-done on-done))
                         (let ((request
                                (e-tools-request-create
                                 :cancel (lambda ()
                                           (setq tool-cancelled t)
                                           t))))
                           (funcall on-request-start request)
                           request))))
    (cl-letf (((symbol-function 'e-harness-tools)
               (lambda (_harness &optional _session-id _turn-id) tools)))
      (e-harness-activity-subscribe harness (lambda (event) (push event events)))
      (e-harness-create-session harness :id "session-1")
      (e-harness-test-prompt-async harness "session-1" "question")
      (should tool-callbacks)
      (e-harness-test-abort harness "session-1")
      (funcall (plist-get tool-callbacks :on-done) "late result")
      (should (equal (plist-get (e-harness-wait-batch harness "session-1" 0.1)
                                :status)
                     'cancelled))
      (should tool-cancelled)
      (should (member 'turn-cancelled
                      (mapcar (lambda (event) (plist-get event :type))
                              events)))
      (should (member 'tool-finished
                      (mapcar (lambda (event) (plist-get event :type))
                              events)))
      (let* ((messages (e-harness-messages harness "session-1"))
             (tool-result (plist-get (nth 2 messages) :content)))
        (should (equal (mapcar (lambda (message) (plist-get message :role))
                               messages)
                       '(user tool-call tool)))
        (should (equal (plist-get tool-result :tool-call-id) "call-1"))
        (should (eq (plist-get tool-result :status) 'error))
        (should (equal (plist-get tool-result :content) "Cancelled"))))))

(ert-deftest e-harness-test-follow-up-appends-user-message ()
  "Follow-up submits another turn against the same session."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "first")
    (e-harness-test-prompt-batch harness "session-1" "second")
    (should (equal (mapcar (lambda (message) (plist-get message :role))
                           (e-harness-messages harness "session-1"))
                   '(user assistant user assistant)))))

(ert-deftest e-harness-test-board-consumption-receipt-carries-endpoint-fence ()
  "A consumed board input reports the accepted token and generation."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (token [endpoint :live 7 "store" "session"])
         events)
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session")
    (e-harness-test-prompt-batch
     harness "session" "board input"
     :metadata
     (list :input-origin 'board
           :board-delivery-id '("board" "message" "participant")
           :board-id "board" :board-participant-id "participant"
           :board-endpoint-token token :board-endpoint-generation '(3 7)))
    (let* ((event (cl-find-if
                   (lambda (candidate)
                     (eq (e-events-type candidate) 'input-consumed))
                   events))
           (payload (plist-get event :payload)))
      (should event)
      (should (equal (plist-get payload :endpoint-token) token))
      (should-not (eq (plist-get payload :endpoint-token) token))
      (should (equal (plist-get payload :endpoint-generation) '(3 7))))))

(ert-deftest e-harness-test-board-endpoint-token-is-not-message-metadata ()
  "A live endpoint fence is excluded from durable transcript metadata."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message :content "answer")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         (token [endpoint :live 7 "store" "session"]))
    (e-harness-create-session harness :id "session")
    (e-harness-test-prompt-batch
     harness "session" "board input"
     :metadata (list :input-origin 'board :board-endpoint-token token))
    (let ((metadata (plist-get (car (e-harness-messages harness "session"))
                               :metadata)))
      (should (eq (plist-get metadata :input-origin) 'board))
      (should-not (plist-member metadata :board-endpoint-token)))))

(ert-deftest e-harness-test-discards-only-fenced-queued-board-head ()
  "Queued board discard preserves FIFO and acknowledges the exact endpoint."
  (let* ((harness (e-harness-create))
         (token [endpoint :live 7 "store" "session"])
         (delivery-id '("board" "message" "participant"))
         events)
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session")
    (puthash "session"
             (list :id "settling" :status 'done
                   :endpoint-token token)
             (e-harness-active-turns harness))
    (e-harness-test-request-follow-up
     harness "session" "queued"
     :metadata
     (list :input-origin 'board :board-delivery-id delivery-id
           :board-endpoint-token token :board-endpoint-generation '(3 7)))
    (should-not
     (e-harness-discard-queued-board-input
      harness "session" delivery-id token '(3 8) 'participant-removed))
    (should (= (length (e-harness-queued-prompts harness "session")) 1))
    (should
     (e-harness-discard-queued-board-input
      harness "session" delivery-id token '(3 7) 'participant-removed))
    (should-not (e-harness-queued-prompts harness "session"))
    (let* ((event (cl-find-if
                   (lambda (candidate)
                     (eq (e-events-type candidate) 'input-discarded))
                   events))
           (payload (plist-get event :payload)))
      (should event)
      (should (equal (plist-get payload :delivery-id) delivery-id))
      (should (equal (plist-get payload :endpoint-token) token))
      (should-not (eq (plist-get payload :endpoint-token) token))
      (should (eq (plist-get payload :reason) 'participant-removed)))))

(ert-deftest e-harness-test-tool-lifecycle-runs-pre-and-post-hooks ()
  "Harness tool lifecycle applies active pre and post tool hooks."
  (should (require 'e-hooks nil t))
  (let* ((calls 0)
         (backend
          (e-backend-create
           :name "fake-harness-hooks"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore messages options)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item
                             '(:type tool-call
                               :id "call-1"
                               :name "echo"
                               :arguments (:stated_purpose "Echo the text."
                                           :text "raw")))
                    (funcall on-item '(:type done :reason tool-use)))
                (funcall on-item
                         '(:type assistant-message :content "done"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'echo-tool
           :tools
           (list (lambda (registry)
                   (e-tools-test-register
                    registry
                    :name "echo"
                    :description "Echo text."
                    :handler (lambda (arguments)
                               (plist-get arguments :text)))))))
         (harness nil)
         (hooks-capability
          (e-capability-create
           :id 'tool-hooks
           :hooks
           (list
            (e-hook-create
             :id "10-prepare"
             :point :pre-tool-call
             :handler (lambda (tool-call context)
                        (should (eq (plist-get context :harness) harness))
                        (let ((prepared (copy-sequence tool-call)))
                          (plist-put
                           prepared
                           :arguments
                           (list :stated_purpose
                                 (plist-get (plist-get tool-call :arguments)
                                            :stated_purpose)
                                 :text "prepared")))))
            (e-hook-create
             :id "50-shape-result"
             :point :post-tool-call
             :handler (lambda (result context)
                        (should (equal (plist-get context :session-id)
                                       "session-1"))
                        (plist-put (copy-sequence result)
                                   :content
                                   (concat (plist-get result :content)
                                           "-post"))))))))
    (setq harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list tools-capability hooks-capability)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "use tool")
    (let* ((messages (e-harness-messages harness "session-1"))
           (tool-call (cl-find 'tool-call messages
                               :key (lambda (message)
                                      (plist-get message :role))))
           (tool-result (cl-find 'tool messages
                                 :key (lambda (message)
                                        (plist-get message :role)))))
      (should (equal (plist-get (plist-get (plist-get tool-call :content)
                                           :arguments)
                                :text)
                     "prepared"))
      (should (equal (plist-get (plist-get tool-result :content) :content)
                     "prepared-post")))))

(ert-deftest e-harness-test-nested-tool-calls-use-harness-lifecycle ()
  "Nested tool calls run hooks, emit activity, and do not append messages."
  (should (require 'e-hooks nil t))
  (let* ((events nil)
         (harness nil)
         (tools-capability
          (e-capability-create
           :id 'nested-tools
           :tools
           (list
            (lambda (registry)
              (e-tools-test-register
               registry
               :name "outer"
               :description "Call inner."
               :handler (lambda (_arguments)
                          (e-tools-call
                           "inner" '(:text "raw")
                           '(:metadata (:purpose "chain-test")))))
              (e-tools-test-register
               registry
               :name "inner"
               :description "Return text."
               :handler (lambda (arguments)
                          (plist-get arguments :text)))))))
         (hooks-capability
          (e-capability-create
           :id 'nested-hooks
           :hooks
           (list
            (e-hook-create
             :id "10-nested-prepare"
             :point :pre-tool-call
             :handler
             (lambda (tool-call context)
               (should (eq (plist-get context :harness) harness))
               (if (equal (plist-get tool-call :name) "inner")
                   (plist-put (copy-sequence tool-call)
                              :arguments '(:text "prepared"))
                 tool-call)))
            (e-hook-create
             :id "50-nested-result"
             :point :post-tool-call
             :handler
             (lambda (result context)
               (should (equal (plist-get context :turn-id) "turn-1"))
               (if (equal (plist-get result :name) "inner")
                   (plist-put (copy-sequence result)
                              :content
                              (concat (plist-get result :content) "-post"))
                 result))))))
         result
         failure)
    (setq harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :intrinsic-capabilities (list tools-capability hooks-capability)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-tool-lifecycle-start-call
     (e-harness-tool-lifecycle harness "session-1" "turn-1")
     '(:id "outer-1" :name "outer" :arguments nil)
     :on-done (lambda (value) (setq result value))
     :on-error (lambda (err) (setq failure err)))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (not (or result failure))
                  (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should (or result failure))
    (when failure
      (signal (car failure) (cdr failure)))
    (should
     (equal result
            '(:tool-call-id "outer-1"
              :name "outer"
              :status ok
              :content (:tool-call-id "outer-1/nested-1"
                        :name "inner"
                        :status ok
                        :content "prepared-post"
                        :metadata nil)
              :metadata nil)))
    (let* ((ordered-events (nreverse events))
           (started (cl-find 'tool-started ordered-events
                             :key (lambda (event)
                                    (plist-get event :type))))
           (finished (cl-find 'tool-finished ordered-events
                              :key (lambda (event)
                                     (plist-get event :type))))
           (started-payload (plist-get started :payload))
           (finished-payload (plist-get finished :payload)))
      (should (equal (mapcar (lambda (event) (plist-get event :type))
                             ordered-events)
                     '(tool-started tool-finished)))
      (should (equal (plist-get started-payload :nested) t))
      (should (equal (plist-get started-payload :parent-tool-call-id)
                     "outer-1"))
      (should (equal (plist-get started-payload :depth) 1))
      (should (equal (plist-get (plist-get started-payload :tool-call)
                                :arguments)
                     '(:text "prepared")))
      (should (equal (plist-get started-payload :purpose)
                     "chain-test"))
      (should (equal (plist-get finished-payload :nested) t))
      (should (equal (plist-get finished-payload :parent-tool-call-id)
                     "outer-1"))
      (should (equal (plist-get (plist-get finished-payload :result)
                                :content)
                     "prepared-post"))
      (should (equal (plist-get finished-payload :purpose)
                     "chain-test")))
    (let* ((activity (e-harness-session-activity-events harness "session-1"))
           (nested-finished
            (cl-find 'tool-finished activity
                     :key (lambda (event)
                            (plist-get event :event-type)))))
      (should nested-finished)
      (should (equal (plist-get (plist-get nested-finished :payload)
                                :nested)
                     t)))
    (should (equal (e-harness-messages harness "session-1") nil))))

(ert-deftest e-harness-test-run-elisp-can-catch-nested-tool-errors ()
  "Evaluated Lisp can catch nested tool failures and return data."
  (let* ((code
          "(condition-case err
     (e-tools-call! \"fail_tool\" nil)
   (e-tools-nested-tool-error
    (let ((result (cadr err)))
      (list :caught t
            :tool (plist-get result :name)
            :status (plist-get result :status)
            :content (plist-get result :content)))))")
         (calls 0)
         (second-request-messages nil)
         (backend
          (e-backend-create
           :name "fake-run-elisp-caught-nested-error"
           :stream
           (cl-function
            (lambda (&key messages options on-item)
              (ignore options)
              (setq calls (1+ calls))
              (if (= calls 1)
                  (progn
                    (funcall on-item
                             (list :type 'tool-call
                                   :id "run-error"
                                   :name "run_elisp"
                                   :arguments (list :stated_purpose
                                                     "Run the requested code."
                                                     :code code)))
                    (funcall on-item '(:type done :reason tool-use)))
                (setq second-request-messages messages)
                (funcall on-item
                         '(:type assistant-message
                           :content "handled"))
                (funcall on-item '(:type done :reason stop)))))))
         (tools-capability
          (e-capability-create
           :id 'run-elisp-error-tools
           :tools
           (list
            (lambda (registry)
              (e-emacs-tools-register-run-elisp registry)
              (e-tools-test-register
               registry
               :name "fail_tool"
               :description "Fail."
               :handler (lambda (_arguments)
                          (error "nested boom")))))))
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list tools-capability))))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "catch nested error")
    (should (equal calls 2))
    (let* ((tool-message (cl-find 'tool second-request-messages
                                  :key (lambda (message)
                                         (plist-get message :role))))
           (content (plist-get (plist-get tool-message :content)
                               :content)))
      (should (string-match-p ":caught t" (format "%S" content)))
      (should (string-match-p "fail_tool" (format "%S" content)))
      (should (string-match-p "nested boom" (format "%S" content))))))

(ert-deftest e-harness-test-discovery-tool-descriptions-stay-lean ()
  "Discovery tools omit active scheme details from their prompt schemas."
  (let* ((capability
          (e-capability-create
           :id 'discovery-description-capability
           :resource-methods
           (list (lambda (registry)
                   (dolist (operation (list e-operation-glob
                                            e-operation-search))
                     (e-resources-register
                      registry
                      (e-resource-method-create
                       :scheme "described"
                       :operation operation
                       :description "Verbose scheme detail."
                       :uri-patterns '("described://<id>")
                       :handler #'ignore)))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (list capability))))
    (dolist (name '("glob" "search"))
      (let* ((tool (seq-find (lambda (definition)
                               (equal (plist-get definition :name) name))
                             (e-tools-definitions (e-harness-tools harness))))
             (description (plist-get tool :description)))
        (should tool)
        (should-not (string-match-p "described://" description))
        (should-not (string-match-p "Verbose scheme detail" description))))))

(ert-deftest e-harness-test-write-tool-description-states-create-invariant ()
  "Generated write descriptions state the shared create/overwrite contract."
  (let* ((capability
          (e-capability-create
           :id 'write-description-capability
           :resource-methods
           (list (lambda (registry)
                   (e-resources-register
                    registry
                    (e-resource-method-create
                     :scheme "writable"
                     :operation e-operation-write
                     :description "Writable resources."
                     :uri-patterns '("writable://<id>")
                     :handler (lambda (_uri _content) "ok")))))))
         (layer (e-layer-create
                 :id 'write-description-layer
                 :name "Write Description Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (e-layer-capabilities layer)))
         (write-tool (seq-find (lambda (definition)
                                 (equal (plist-get definition :name) "write"))
                               (e-tools-definitions (e-harness-tools harness))))
         (description (plist-get write-tool :description)))
    (should write-tool)
    (should
     (string-match-p
      "write creates missing parent paths and the target resource, or overwrites existing content"
      description))
    (should (string-match-p "writable://<id>" description))))

(ert-deftest e-harness-test-persists-activity-events-and-tags_turn_messages ()
  "Harness turn events persist as activity, and messages keep their turn id."
  (let* ((backend (e-backend-fake-create
                   :items '((:type reasoning-delta :content "thinking")
                            (:type reasoning-raw-delta :content "raw thinking")
                            (:type assistant-message :content "done")
                            (:type done :reason stop))))
         (store (e-session-store-create))
         (harness (e-harness-create :backend backend :sessions store)))
    (e-harness-create-session harness :id "session-1")
    (e-harness-test-prompt-batch harness "session-1" "hello")
    (let ((messages (e-session-messages store "session-1"))
          (events (e-session-activity-events store "session-1")))
      (should (equal (length (delete-dups
                              (mapcar (lambda (message)
                                        (plist-get message :turn-id))
                                      messages)))
                     1))
      (should (equal (mapcar (lambda (event)
                               (plist-get event :event-type))
                             events)
                     '(turn-started
                       provider-request-started
                       reasoning-delta
                       reasoning-raw-delta
                       provider-request-finished
                       turn-finished))))))

(ert-deftest e-harness-test-activity-index-write-is-coalesced ()
  "Harness activity events flush the session index once at turn settlement."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (write-count 0))
    (e-harness-create-session harness :id "session-1")
    (cl-letf (((symbol-function 'e-session-storage-publish-projections)
               (lambda (&rest _)
                 (setq write-count (1+ write-count)))))
      (e-harness-activity-emit-turn-event harness "session-1" "turn-1"
                                  'provider-request-started
                                  '(:status started))
      (e-harness-activity-emit-turn-event harness "session-1" "turn-1"
                                  'reasoning-delta
                                  '(:type reasoning-delta
                                    :content "thinking"))
      (e-harness-activity-emit-turn-event harness "session-1" "turn-1"
                                  'tool-started
                                  '(:name "read" :arguments nil))
      (e-harness-activity-emit-turn-event harness "session-1" "turn-1"
                                  'tool-finished
                                  '(:tool-call (:name "read")
                                    :result "ok"))
      (e-harness-activity-emit-turn-event harness "session-1" "turn-1"
                                  'turn-finished
                                  '(:reason done)))
    (should (= write-count 1))
    (should (equal '(provider-request-started
                     reasoning-delta
                     tool-started
                     tool-finished
                     turn-finished)
                   (mapcar (lambda (event)
                             (plist-get event :event-type))
                           (e-session-activity-events store "session-1"))))))

(provide 'e-harness-tool-composition-test)

;;; e-harness-tool-composition-test.el ends here

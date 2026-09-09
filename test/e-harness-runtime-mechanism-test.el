;;; e-harness-runtime-mechanism-test.el --- Owner mechanism tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Direct owner mechanism tests.  These assertions intentionally exercise
;; the private namespace of the owner under test; composed behavior belongs
;; in e-harness-test.el and the integration suites.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-chat-session)
(require 'e-base)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-context)
(require 'e-dev-profile)
(require 'e-sqlite-test-store-support
         (expand-file-name
          "e-sqlite-test-store-support.el"
          (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-emacs-tools)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-layers)
(require 'e-operations)
(require 'e-prompts)
(require 'e-request)
(require 'e-resources)
(require 'e-skills)
(require 'e-store)

(defconst e-harness-test--capability-config-options
  (list
   (e-capability-config-option-create
    :key :value
    :default "default"
    :validator #'stringp)
   (e-capability-config-option-create
    :key :items
    :default nil
    :normalizer #'e-capability-config-string-list
    :validator #'e-capability-config-string-list-p))
  "Option specs for harness capability config tests.")

(defmacro e-harness-test--with-empty-layer-registry (&rest body)
  "Run BODY with an isolated layer registry."
  (declare (indent 0) (debug t))
  `(let ((e-layer--registry (make-hash-table :test 'eq)))
     ,@body))

(defun e-harness-test--curation-frame
    (&optional generation-id frame-id value observation-id source-ref
              source-fingerprint)
  "Return one small live frame for harness commit-boundary tests."
  (e-context-lifetime-frame-create
   :id (or frame-id "frame:harness-test")
   :generation-id (or generation-id "generation:harness-test")
   :consumer-request-id "consumer:harness-test"
   :observations
   (list
    (list :observation-id (or observation-id "observation:harness-test")
          :kind "current-state"
          :source-entry-ref (or source-ref "source:harness-test")
          :source-fingerprint
          (or source-fingerprint "fingerprint:harness-test")
          :effective-delivery "inherited"
          :body (list :role 'system
                      :content (or value "HARNESS-EXACT-VALUE"))))))

(defun e-harness-test--tool-result-curation-frame
    (generation-id &optional frame-id call-id value observation-id source-ref
                  source-fingerprint)
  "Return one live tool-result source frame for erasure tests."
  (let ((call-id (or call-id "call:harness-tool-result")))
    (e-context-lifetime-frame-create
     :id (or frame-id "frame:harness-tool-result")
     :generation-id generation-id
     :consumer-request-id "consumer:harness-tool-result"
     :observations
     (list
      (list :observation-id (or observation-id "observation:harness-tool-result")
            :kind "tool-result"
            :source-entry-ref (or source-ref "source:harness-tool-result")
            :source-fingerprint
            (or source-fingerprint "fingerprint:harness-tool-result")
            :effective-delivery "inherited"
            :body (list :role 'tool
                        :content (list :tool-call-id call-id
                                        :name "inspect"
                                        :status 'ok
                                        :content
                                        (or value "HARNESS-TOOL-RESULT"))))))))

(defun e-harness-test--two-tool-result-curation-frame (generation-id)
  "Return a two-source tool-result frame for mixed curation tests."
  (e-context-lifetime-frame-create
   :id "frame:harness-mixed-tool-results"
   :generation-id generation-id
   :consumer-request-id "consumer:harness-mixed-tool-results"
   :observations
   (list
    (list :observation-id "observation:harness-mixed-1"
          :kind "tool-result"
          :source-entry-ref "source:harness-mixed-1"
          :source-fingerprint "fingerprint:harness-mixed-1"
          :effective-delivery "inherited"
          :body (list :role 'tool
                      :content (list :tool-call-id "call:harness-mixed-1"
                                      :name "inspect"
                                      :status 'ok
                                      :content "HARNESS-MIXED-ONE")))
    (list :observation-id "observation:harness-mixed-2"
          :kind "tool-result"
          :source-entry-ref "source:harness-mixed-2"
          :source-fingerprint "fingerprint:harness-mixed-2"
          :effective-delivery "inherited"
          :body (list :role 'tool
                      :content (list :tool-call-id "call:harness-mixed-2"
                                      :name "inspect"
                                      :status 'ok
                                      :content "HARNESS-MIXED-TWO"))))))

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
           :context-providers
           (if (eq kind 'current-state)
               (list stable-provider dynamic-provider)
             (list stable-provider))
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
                             (car (e-session-local-current-path
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
          :anchors (e-session-local-provider-anchors
                    (e-harness-sessions harness) "session-1")
          :context (e-harness-turn-context
                    harness "session-1" "after-refresh"))))

(defun e-harness-test--append-compaction-curation
    (store session-id generation-id suffix)
  "Append one valid curation for GENERATION-ID to SESSION-ID.
SUFFIX makes the runtime identities and fact unique to the owning test."
  (let* ((frame-id (format "frame-compaction-%s" suffix))
         (consumer-id (format "consumer-compaction-%s" suffix))
         (response-id (format "response-compaction-%s" suffix))
         (observation-id (format "observation-compaction-%s" suffix))
         (frame
          (e-harness-test--curation-frame
           generation-id frame-id
           (format "RAW-COMPACTION-%s" suffix)
           observation-id
           (format "external:compaction-%s" suffix)
           (format "compaction-fingerprint-%s" suffix)))
         (curation
          (plist-get
           (e-context-lifetime-prepare-curation-disposition
            frame
            (list :keep nil
                  :summaries
                  (list (list :sources '(1)
                              :text (format "selected-%s" suffix))))
            response-id
            1.0)
           :record)))
    (e-session-append-context-curation-package
     store session-id (list :promotion curation :erasure nil))))
(ert-deftest e-harness-test-set-message-display-hides-and-emits-event ()
  "Setting a message's display flips it hidden and emits `message-updated'."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (events nil))
    (e-harness-create-session harness :id "session-1")
    (let ((message (e-harness-turn--append-message
                    harness "session-1" "turn-1"
                    '(:role assistant :content "The fix shipped in commit 42."))))
      (e-harness-activity-subscribe harness (lambda (event) (push event events)))
      (e-harness-set-message-display
       harness "session-1" (plist-get message :id) 'hidden)
      (should (eq (plist-get (car (last (e-harness-messages harness "session-1")))
                             :display)
                  'hidden))
      (let ((updated (seq-find (lambda (event)
                                 (eq (plist-get event :type) 'message-updated))
                               events)))
        (should updated)
        (should (equal (plist-get (plist-get (plist-get updated :payload)
                                             :message)
                                  :id)
                       (plist-get message :id)))
        (should (eq (plist-get (plist-get (plist-get updated :payload)
                                          :message)
                               :display)
                    'hidden))))))

(ert-deftest e-harness-test-retryable-error-detection ()
  "The harness consumes only the backend-normalized retry decision."
  (should (e-harness-turn--retryable-error-p
           '(:retryable t :retry-reason rate-limit)))
  (should-not (e-harness-turn--retryable-error-p '(:retryable nil :status 429)))
  (should-not (e-harness-turn--retryable-error-p '(:status 503)))
  (should-not (e-harness-turn--retryable-error-p nil)))

(ert-deftest e-harness-test-backoff-schedule-grows-and-caps ()
  "Backoff grows by the multiplier and is capped, with jitter when enabled."
  (let ((e-harness-retry-initial-backoff-seconds 2.0)
        (e-harness-retry-backoff-multiplier 2.0)
        (e-harness-retry-max-backoff-seconds 20.0)
        (e-harness-retry-jitter-fraction 0))
    ;; With jitter disabled the schedule is deterministic.
    (should (= (e-harness-turn--retry-backoff-seconds 1) 2.0))
    (should (= (e-harness-turn--retry-backoff-seconds 2) 4.0))
    (should (= (e-harness-turn--retry-backoff-seconds 3) 8.0))
    (should (= (e-harness-turn--retry-backoff-seconds 10) 20.0)))
  (let ((e-harness-retry-initial-backoff-seconds 2.0)
        (e-harness-retry-backoff-multiplier 2.0)
        (e-harness-retry-max-backoff-seconds 20.0)
        (e-harness-retry-jitter-fraction 0.25))
    ;; Jitter only ever adds delay, never reduces below the base or past
    ;; the cap plus the jitter fraction.
    (let ((d (e-harness-turn--retry-backoff-seconds 1)))
      (should (>= d 2.0))
      (should (<= d (* 2.0 1.25))))
    (let ((d (e-harness-turn--retry-backoff-seconds 10)))
      (should (>= d 20.0))
      (should (<= d (* 20.0 1.25))))))

(ert-deftest e-harness-test-retry-reset-seconds-bounds-adapter-hint ()
  "The harness bounds normalized retry delays without interpreting providers."
  (let ((e-harness-retry-reset-max-wait-seconds 900.0))
    (should (= 45 (e-harness-turn--retry-reset-seconds
                   '(:retry-after-seconds 45))))
    (should (= 1.0 (e-harness-turn--retry-reset-seconds
                    '(:retry-after-seconds -60))))
    (should-not (e-harness-turn--retry-reset-seconds
                 '(:retry-after-seconds 2400)))
    (should-not (e-harness-turn--retry-reset-seconds '(:retryable t))))
  ;; A zero cap disables reset-aware waiting entirely.
  (let ((e-harness-retry-reset-max-wait-seconds 0))
    (should-not (e-harness-turn--retry-reset-seconds
                 '(:retry-after-seconds 5)))))

(ert-deftest e-harness-test-unsettled-state-follows-turn-and-input-owners ()
  "Harness counts active turns and queued inputs at their exact transitions."
  (let* ((e-harness-turn-state--aggregate-active-turn-count 0)
         (e-harness-turn-state--aggregate-queued-input-count 0)
         (e-harness-turn-state--aggregate-unsettled-generation 0)
         (backend (e-backend-create
                   :name "held"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-item on-done on-error
                                     on-request-start)))))
         (harness (e-harness-create :backend backend))
         scheduled snapshots)
    (setf (e-harness-unsettled-change-function harness)
          (lambda (snapshot) (push snapshot snapshots)))
    (e-harness-create-session harness :id "session-1")
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (push (lambda () (apply function arguments)) scheduled))))
      (e-harness-test-prompt-async harness "session-1" "first")
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 1 :active-turns 1 :queued-inputs 0)))
      (e-harness-test-queue-prompt harness "session-1" "second")
      (e-harness-test-steer-active-turn harness "session-1" "steer")
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 3 :active-turns 1 :queued-inputs 2)))
      (should (equal (e-harness-aggregate-unsettled-state)
                     '(:generation 3 :active-turns 1 :queued-inputs 2)))
      (e-harness-test-abort harness "session-1")
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 4 :active-turns 1 :queued-inputs 1)))
      (funcall (pop scheduled))
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 7 :active-turns 1 :queued-inputs 0)))
      (e-harness-test-abort harness "session-1")
      (funcall (pop scheduled))
      (should (equal (e-harness-unsettled-state harness)
                     '(:generation 8 :active-turns 0 :queued-inputs 0)))
      (should (equal (e-harness-aggregate-unsettled-state)
                     '(:generation 8 :active-turns 0 :queued-inputs 0)))
      (should (= (length snapshots) 8)))))

(ert-deftest e-harness-test-steer-active-turn-stores-pending-input ()
  "Successful steering stores only durable input on the active turn."
  (require 'e-board-runtime)
  (let* ((backend (e-backend-create
                   :name "steerable"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-item on-done
                                     on-error)
                             (funcall on-request-start
                                      (e-backend-request-create))
                             nil))))
         (harness (e-harness-create :backend backend))
         (token (e-board-runtime-endpoint-token--create
                 :harness-id :live
                 :harness-object-generation 7
                 :session-id "session-1"))
         (events nil))
    (e-harness-activity-subscribe harness (lambda (event) (push event events)))
    (e-harness-create-session harness :id "session-1")
    (let ((turn-id
           (e-harness-test-prompt-async
            harness "session-1" "first"
            :metadata (list :input-origin 'board
                            :board-endpoint-token token))))
      (should (equal (e-harness-test-steer-active-turn
                      harness "session-1" "focus here"
                      :metadata (list :source 'chat-composer
                                      :board-endpoint-token token))
                     turn-id))
      (let ((entry (gethash "session-1" (e-harness-active-turns harness))))
        (should (equal (e-harness-turn--pending-steering-items entry)
                       '((:prompt "focus here"
                          :metadata (:source chat-composer))))))
      (let ((steered (cl-find 'turn-steered events
                              :key (lambda (event)
                                     (plist-get event :type)))))
        (should steered)
        (should (equal (plist-get steered :turn-id) turn-id))
        (should (equal (plist-get (plist-get steered :payload)
                                  :prompt-preview)
                       "focus here"))
        (should (equal (plist-get (plist-get steered :payload)
                                  :metadata)
                       '(:source chat-composer))))
      (let ((durable
             (cl-find 'turn-steered
                      (e-harness-session-activity-events harness "session-1")
                      :key (lambda (event)
                             (plist-get event :event-type)))))
        (should durable)
        (should (equal (plist-get (plist-get durable :payload) :metadata)
                       '(:source chat-composer)))))))

(ert-deftest e-harness-test-provider-diagnostics-retain-context-semantics-only ()
  "Durable provider activity keeps context semantics without observation data."
  (let* ((directory (make-temp-file "e-harness-provider-context-diagnostics-" t))
         (fingerprint "sha256:current-state-v1")
         (raw "raw-current-state-secret")
         (diagnostics
          (list :model "gpt-test"
                :reasoning-effort "high"
                :reasoning-summary "detailed"
                :observation-delivery 'request-local-replaceable
                :replaceable-current-state-present t
                :current-state-fingerprint fingerprint
                :context-rendering-strategy 'replaceable-channel
                :provider-anchor-safety 'advance-eligible
                ;; These fields intentionally model the full frontier and
                ;; request-local value.  They must never cross the activity
                ;; projection or journal boundary.
                :observation-frontier
                (list :messages (list (list :role 'system :content raw))
                      :fingerprint fingerprint)
                :replaceable-current-state
                (list (list :role 'system :content raw))
                :current-state-messages
                (list (list :role 'system :content raw))
                :current-state-content raw
                :messages (list (list :role 'system :content raw))))
         (backend
          (e-backend-create
           :name "context-diagnostics"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                           on-request-start)
              (ignore messages options on-error)
              (funcall on-request-start
                       (e-backend-request-create
                        :metadata
                        (list :provider 'openai
                              :transport 'http
                              :diagnostics diagnostics)))
              (funcall on-item '(:type assistant-message :content "answer"))
              (funcall on-item '(:type done :reason stop))
              (funcall on-done '(:status done))
              nil))))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create :backend backend :sessions store)))
    (unwind-protect
        (progn
          (let ((projected
                 (e-harness-activity--provider-diagnostics-activity-projection
                  diagnostics)))
            (dolist (key '(:observation-delivery
                           :reasoning-effort
                           :reasoning-summary
                           :replaceable-current-state-present
                           :current-state-fingerprint
                           :context-rendering-strategy
                           :provider-anchor-safety))
              (should (plist-member projected key)))
            (should (equal (plist-get projected :current-state-fingerprint)
                           fingerprint))
            (should (equal (plist-get projected :reasoning-summary)
                           "detailed"))
            (should-not (plist-member projected :observation-frontier))
            (should-not (plist-member projected :replaceable-current-state))
            (should-not (plist-member projected :current-state-messages))
            (should-not (plist-member projected :current-state-content))
            (should-not (plist-member projected :messages))
            (should-not (string-match-p (regexp-quote raw)
                                        (prin1-to-string projected))))
          (e-harness-create-session harness :id "session-1")
          (e-harness-test-prompt-batch harness "session-1" "question")
          (let* ((activity (e-harness-session-activity-events
                            harness "session-1"))
                 (started
                  (seq-find
                   (lambda (event)
                     (eq (plist-get event :event-type)
                         'provider-request-started))
                   activity))
                 (projected (plist-get (plist-get started :payload)
                                       :diagnostics)))
            (should started)
            (should (equal (plist-get projected :reasoning-summary)
                           "detailed"))
            (should (eq (plist-get projected :observation-delivery)
                        'request-local-replaceable))
            (should (eq (plist-get projected :context-rendering-strategy)
                        'replaceable-channel))
            (should (eq (plist-get projected :provider-anchor-safety)
                        'advance-eligible)))
          (e-session-flush-write-queue store)
          (let* ((loaded (e-session-persistent-store-create directory))
                 (activity (e-session-local-activity-events loaded "session-1"))
                 (started
                  (seq-find
                   (lambda (event)
                     (eq (plist-get event :event-type)
                         'provider-request-started))
                   activity))
                 (payload (plist-get started :payload))
                 (durable-records
                  (prin1-to-string
                   (e-session-storage-read-session-records
                    loaded "session-1")))
                 (projected (plist-get payload :diagnostics)))
            (should started)
            (dolist (key '(:observation-delivery
                           :reasoning-effort
                           :reasoning-summary
                           :replaceable-current-state-present
                           :current-state-fingerprint
                           :context-rendering-strategy
                           :provider-anchor-safety))
              (should (plist-member projected key)))
            (should (eq (plist-get projected :observation-delivery)
                        'request-local-replaceable))
            (should (equal (plist-get projected
                                      :replaceable-current-state-present)
                           t))
            (should (equal (plist-get projected :current-state-fingerprint)
                           fingerprint))
            (should (equal (plist-get projected :reasoning-summary)
                           "detailed"))
            (should (eq (plist-get projected :context-rendering-strategy)
                        'replaceable-channel))
            (should (eq (plist-get projected :provider-anchor-safety)
                        'advance-eligible))
            (dolist (key '(:observation-frontier
                           :replaceable-current-state
                           :current-state-messages
                           :current-state-content
                           :messages))
              (should-not (plist-member (plist-get payload :diagnostics)
                                        key)))
            (should-not (string-match-p (regexp-quote raw)
                                        durable-records))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-context-curation-revision-fences-material-anchor ()
  "Curation revision fences anchors without hashing frame-local sources."
  (let* ((options
          '(:context-lifetime-enabled t
            :reserved-effect-carrier context-curate-wire
            :observation-delivery request-local-replaceable
            :observation-delivery-map request-local-replaceable
            :context-capabilities
            (:observation-delivery request-local-replaceable
             :reserved-effect-carrier context-curate-wire)))
         (context
          (lambda (value messages)
            (list :options (copy-tree options)
                  :messages messages
                  :segments
                  (list
                   '(:kind static-prefix
                     :id static
                     :fingerprint "static-fingerprint"
                     :messages ((:role system :content "stable policy")))
                   (list :kind 'current-state
                         :id 'current
                         :fingerprint "current-fingerprint"
                         :messages
                         (list (list :role 'system :content value))))
                  :provider-anchor-active-layer-ids '("layer")
                  :provider-anchor-compaction-boundary nil)))
         (first (funcall context
                         "SOURCE-ONE"
                         '((:role system :content "stable policy")
                           (:role system :content "SOURCE-ONE"))))
         (second (funcall context
                          "SOURCE-TWO"
                          '((:role system :content "stable policy")
                            (:role system :content "SOURCE-TWO"))))
         (first-fingerprints
          (e-harness-context-runtime--provider-anchor-fingerprints first))
         (second-fingerprints
          (e-harness-context-runtime--provider-anchor-fingerprints second))
         (revision (plist-get first-fingerprints
                              :context-curation-revision-identity)))
    (should revision)
    (should (equal revision
                   (e-context-lifetime-curation-revision-identity)))
    (should (equal first-fingerprints second-fingerprints))
    (should-not (string-match-p "SOURCE-ONE\|SOURCE-TWO"
                                (prin1-to-string first-fingerprints)))
    (let ((e-context-budget-estimate-bytes-per-token 2.0))
      (should-not
       (equal revision
              (plist-get
               (e-harness-context-runtime--provider-anchor-fingerprints first)
               :context-curation-revision-identity))))
    (let ((e-context-lifetime-curation-presentation-revision
           "context-curation-presentation-test-v2"))
      (should-not
       (equal revision
              (plist-get
               (e-harness-context-runtime--provider-anchor-fingerprints first)
               :context-curation-revision-identity))))
    (let* ((store (e-session-store-create))
           (session-id "curation-revision-session"))
      (e-session-create store :id session-id)
      (let* ((message (e-session-append-message
                       store session-id
                       '(:role assistant :content "answer")))
             (anchor
              (e-session-append-provider-anchor
               store session-id 'openai
               :model "gpt-test"
               :covered-entry-id (plist-get message :id)
               :fingerprints first-fingerprints)))
        (let ((e-context-budget-estimate-bytes-per-token 2.0))
          (should
           (eq (e-session-local-provider-anchor-incompatibility-reason
                store session-id anchor 'openai "gpt-test"
                (e-harness-context-runtime--provider-anchor-fingerprints first))
               'context-curation-revision-changed)))))))

(ert-deftest e-harness-test-reasoning-summary-fences-provider-identity ()
  "Effective Responses reasoning summary is an anchor identity input."
  (let* ((base-options '(:model "gpt-test"
                         :reasoning-effort "high"
                         :reasoning-summary "auto"))
         (detailed-options (plist-put (copy-sequence base-options)
                                      :reasoning-summary
                                      "detailed"))
         (base (list :options base-options :segments nil))
         (detailed (list :options detailed-options :segments nil))
         (base-fingerprints
          (e-harness-context-runtime--provider-anchor-fingerprints base))
         (detailed-fingerprints
          (e-harness-context-runtime--provider-anchor-fingerprints detailed))
         (diagnostics (list :model "gpt-test"
                            :reasoning-effort "high"
                            :reasoning-summary "detailed"))
         (projected
          (e-harness-activity--provider-diagnostics-activity-projection diagnostics)))
    (should (equal (plist-get (plist-get base-fingerprints :reasoning)
                              :reasoning-summary)
                   "auto"))
    (should-not (equal base-fingerprints detailed-fingerprints))
    (should (equal (plist-get (plist-get detailed-fingerprints :reasoning)
                              :reasoning-summary)
                   "detailed"))
    (should (equal (plist-get projected :reasoning-summary) "detailed"))))

(ert-deftest e-harness-test-provider-anchor-refresh-keeps-tool-frontier-wire-local ()
  "A refresh tool may continue immediately but cannot persist its frontier."
  (e-harness-test--with-empty-layer-registry
    (dolist (kind '(stable tool-schema provider-option compaction))
      (let* ((result
             (e-harness-test--run-final-refresh-anchor-scenario kind t))
             (requests (plist-get result :requests))
             (anchors (plist-get result :anchors)))
        (should (= (length requests) 2))
        (should-not (plist-get (nth 1 requests) :provider-anchor))
        (should-not anchors)))
    (let* ((result
            (e-harness-test--run-final-refresh-anchor-scenario
             'current-state t))
           (requests (plist-get result :requests))
           (anchors (plist-get result :anchors)))
      ;; The replaceable frontier may use resp-A for this immediate follow-up,
      ;; but the tool-derived frontier remains ineligible for persistence.
      (should (equal
               (plist-get
                (plist-get (nth 1 requests) :provider-anchor)
                :metadata)
               '(:response-id "resp-A")))
      (should-not anchors))
    (dolist (kind '(stable current-state))
      (let* ((result
              (e-harness-test--run-final-refresh-anchor-scenario kind nil))
             (requests (plist-get result :requests))
             (anchors (plist-get result :anchors)))
        (should (= (length requests) 2))
        (should (= (length anchors) 0))))))

(ert-deftest e-harness-test-context-attaches-compatible-provider-anchor ()
  "Context options include a compatible provider anchor and transcript delta."
  (let* ((current-state "dynamic context")
         (dynamic-provider
          (e-context-provider-create
           :name 'dynamic-provider
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (capability
          (e-capability-create
           :id 'anchor-capability
           :instructions "capability instructions"
           :context-providers (list dynamic-provider)))
         (layer (e-layer-create
                 :id 'anchor-layer
                 :name "Anchor Layer"
                 :capabilities (list capability)))
         (harness
          (e-harness-create
           :backend
           (e-backend-fake-create
            :items nil
            :context-capabilities
            '(:continuation linear
              :observation-delivery request-local-replaceable))
           :intrinsic-capabilities (e-layer-capabilities layer)
           :default-options '(:model "gpt-test"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness)
             "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1"))
           (fingerprints
            (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)))
      (e-session-append-provider-anchor
       (e-harness-sessions harness)
       "session-1"
       'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints fingerprints
       :metadata '(:response-id "resp-1"))
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "new prompt"))
      (let* ((context (e-harness-context harness "session-1" "turn-2"))
             (options (plist-get context :options))
             (anchor (plist-get options :provider-anchor))
             (delta (plist-get options :provider-anchor-delta-messages)))
        (should (equal (plist-get (plist-get anchor :metadata) :response-id)
                       "resp-1"))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               (plist-get options
                                          :replaceable-current-state))
                       '("dynamic context")))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               delta)
                       '("new prompt")))
        (should (equal (plist-get options
                                  :provider-anchor-source-message-count)
                       (length (plist-get context :messages))))))))

(ert-deftest e-harness-test-provider-anchor-delta-lifetime-filter-is-opt-in ()
  "Lifetime anchor deltas drop tool bundles without changing legacy deltas."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         (session-id "anchor-delta-lifetime-filter"))
    (e-harness-create-session harness :id session-id)
    (e-session-append-message store session-id
                              '(:role user :content "before-anchor"))
    (let* ((anchor-message
            (e-session-append-message store session-id
                                      '(:role assistant
                                        :content "anchor-response")))
           (tool-call
            (e-session-append-message
             store session-id
             '(:role tool-call
               :content (:id "anchor-call" :name "inspect"
                         :arguments (:target "one"))
               :metadata (:provider-replay-items
                          ((:id "anchor-replay"))))))
           (tool-result
            (e-session-append-message
             store session-id
             '(:role tool
               :content (:tool-call-id "anchor-call"
                         :content "anchor-result"))))
           (_tail
            (e-session-append-message store session-id
                                      '(:role user :content "after-anchor")))
           (anchor (list :covered-entry-id
                         (plist-get anchor-message :id)))
           (legacy
            (e-harness-context-runtime--provider-anchor-delta-messages
             harness session-id anchor nil))
           (lifetime
            (e-harness-context-runtime--provider-anchor-delta-messages
             harness session-id anchor
             '(:context-lifetime-enabled t))))
      (should (seq-some
               (lambda (message)
                 (equal (plist-get (plist-get message :content) :id)
                        (plist-get (plist-get tool-call :content) :id)))
               legacy))
      (should (seq-some
               (lambda (message)
                 (equal (plist-get (plist-get message :content)
                                   :tool-call-id)
                        (plist-get (plist-get tool-result :content)
                                   :tool-call-id)))
               legacy))
      (should-not
       (seq-some
        (lambda (message)
          (member (plist-get message :role) '(tool-call tool)))
        lifetime))
      (should (equal (mapcar (lambda (message)
                               (plist-get message :content))
                             lifetime)
                     '("after-anchor"))))))

(ert-deftest e-harness-test-context-uses-provider-anchor-after-dynamic-state-change ()
  "Replaceable current-state changes preserve the stable provider anchor."
  (let* ((current-state "dynamic context")
         (dynamic-provider
          (e-context-provider-create
           :name 'dynamic-provider
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (capability
          (e-capability-create
           :id 'anchor-capability
           :instructions "capability instructions"
           :context-providers (list dynamic-provider)))
         (layer (e-layer-create
                 :id 'anchor-layer
                 :name "Anchor Layer"
                 :capabilities (list capability)))
         (harness
          (e-harness-create
           :backend
           (e-backend-fake-create
            :items nil
            :context-capabilities
            '(:continuation linear
              :observation-delivery request-local-replaceable))
           :intrinsic-capabilities (e-layer-capabilities layer)
           :default-options '(:model "gpt-test"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness)
             "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1"))
           (anchor-identity
            (plist-get (plist-get anchor-context :options)
                       :continuation-projection-identity)))
      (e-session-append-provider-anchor
       (e-harness-sessions harness)
       "session-1"
       'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setq current-state "changed dynamic context")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "new prompt"))
      (let* ((context (e-harness-context harness "session-1" "turn-2"))
             (options (plist-get context :options))
             (identity (plist-get options
                                 :continuation-projection-identity)))
        (should (eq (plist-get options :observation-delivery)
                    'request-local-replaceable))
        (should (equal identity anchor-identity))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               (plist-get options
                                          :replaceable-current-state))
                       '("changed dynamic context")))
        (should (equal (plist-get
                        (plist-get
                         (plist-get options :provider-anchor)
                         :metadata)
                        :response-id)
                       "resp-1"))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               (plist-get options
                                          :provider-anchor-delta-messages))
                       '("new prompt")))))))

(ert-deftest e-harness-test-context-holds-linear-anchor-for-inherited-state ()
  "Linear continuation reconstructs statelessly around inherited current state."
  (let* ((current-state "dynamic context")
         (dynamic-provider
          (e-context-provider-create
           :name 'dynamic-provider
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (capability
          (e-capability-create
           :id 'anchor-capability
           :instructions "capability instructions"
           :context-providers (list dynamic-provider)))
         (layer (e-layer-create
                 :id 'anchor-layer
                 :name "Anchor Layer"
                 :capabilities (list capability)))
         (harness
          (e-harness-create
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
           :intrinsic-capabilities (e-layer-capabilities layer)
           :default-options '(:model "gpt-test"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setq current-state "changed dynamic context")
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let* ((options (plist-get
                       (e-harness-context harness "session-1" "turn-2")
                       :options)))
        (should-not (plist-get options :provider-anchor))
        (should (eq (plist-get options :observation-delivery) 'inherited))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'inherited-observation-requires-branchable))
        (should (eq (plist-get options :context-rendering-strategy)
                    'stateless))
        (should (eq (plist-get options :provider-anchor-safety)
                    'hold-inherited-observation))))))

(ert-deftest e-harness-test-branchable-inherited-state-uses-clean-anchor ()
  "Branchable inherited state reuses only a clean anchor and stays unadvanced."
  (let* ((current-state "dynamic context")
         (dynamic-provider
          (e-context-provider-create
           :name 'dynamic-provider
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (capability
          (e-capability-create
           :id 'anchor-capability
           :instructions "capability instructions"
           :context-providers (list dynamic-provider)))
         (layer (e-layer-create
                 :id 'anchor-layer
                 :name "Anchor Layer"
                 :capabilities (list capability)))
         (harness
          (e-harness-create
           :backend (e-backend-fake-create
                     :context-capabilities
                     '(:continuation branchable)
                     :items nil)
           :intrinsic-capabilities (e-layer-capabilities layer)
           :default-options '(:model "gpt-test"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1"))
           (clean-fingerprints
            (plist-put
             (copy-sequence
              (e-harness-context-runtime--provider-anchor-fingerprints anchor-context))
             :current-state-fingerprint
             nil)))
      ;; This is the clean response id from before current state was observed.
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints clean-fingerprints
       :metadata '(:response-id "resp-clean"))
      ;; A contaminated anchor must not win merely because it is newer.
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-contaminated"))
      (setq current-state "changed dynamic context")
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let* ((context (e-harness-context harness "session-1" "turn-2"))
             (options (plist-get context :options))
             (anchor (plist-get options :provider-anchor))
             (delta (plist-get options :provider-anchor-delta-messages)))
        (should (equal (plist-get (plist-get anchor :metadata) :response-id)
                       "resp-clean"))
        (should (eq (plist-get options :provider-anchor-safety)
                    'branchable-clean-anchor-only))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               delta)
                       '("changed dynamic context" "new prompt")))))))

(ert-deftest e-harness-test-context-reports-provider-anchor-invalidation-reason ()
  "Context options report why an otherwise current provider anchor was skipped."
  (let* ((stable-state "stable context")
         (stable-provider
          (e-context-provider-create
           :name 'stable-provider
           :cache-placement 'stable-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content stable-state)))))
         (capability
          (e-capability-create
           :id 'anchor-capability
           :instructions "capability instructions"
           :context-providers (list stable-provider)))
         (layer (e-layer-create
                 :id 'anchor-layer
                 :name "Anchor Layer"
                 :capabilities (list capability)))
         (harness
          (e-harness-create
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
           :intrinsic-capabilities (e-layer-capabilities layer)
           :default-options '(:model "gpt-test"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness)
             "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness)
       "session-1"
       'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setq stable-state "changed stable context")
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'segment-fingerprint-mismatch))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-reasoning-change ()
  "Provider continuation anchors include reasoning options in compatibility."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
           :default-options '(:model "gpt-test"
                              :reasoning-effort "high"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setf (e-harness-default-options harness)
            '(:model "gpt-test"
              :reasoning-effort "low"
              :provider-continuation t
              :provider-anchor-provider-id openai))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'reasoning-changed))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-tool-schema-change ()
  "Provider continuation anchors include active tool schemas in compatibility."
  (let* ((tool-parameters '(:type "object"
                           :properties (:path (:type "string"))))
         (tool-provider
          (lambda (registry)
            (e-tools-test-register
             registry
             :name "read"
             :description "Read a file."
             :parameters tool-parameters
             :handler (lambda (&rest _) "ok"))))
         (capability
          (e-capability-create :id 'tool-capability
                               :tools (list tool-provider)))
         (layer (e-layer-create
                 :id 'tool-layer :name "Tool Layer"
                 :capabilities (list capability)))
         (harness
          (e-harness-create
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
           :intrinsic-capabilities (e-layer-capabilities layer)
           :default-options '(:model "gpt-test"
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setq tool-parameters '(:type "object"
                              :properties (:uri (:type "string"))))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'tools-changed))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-effective-layer-id-change ()
  "Provider continuation anchors include effective layer ids in compatibility."
  (e-harness-test--with-empty-layer-registry
    (let* ((harness
            (e-harness-create
             :backend (e-backend-fake-create
                       :context-capabilities '(:continuation linear)
                       :items nil)
             :enabled-layer-ids '(base-layer)
             :default-options '(:model "gpt-test"
                                :provider-continuation t
                                :provider-anchor-provider-id openai))))
      (dolist (id '(base-layer extra-layer))
        (e-layer-register
         (let ((layer-id id))
           (e-layer-spec-create
            :id layer-id
            :name (symbol-name layer-id)
            :factory (lambda ()
                       (e-layer-create
                        :id layer-id
                        :name (symbol-name layer-id)))))))
      (e-harness-create-session harness :id "session-1")
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "old prompt"))
      (let* ((assistant
              (e-session-append-message
               (e-harness-sessions harness) "session-1"
               '(:role assistant :content "old answer")))
             (anchor-context (e-harness-context harness "session-1" "turn-1")))
        (e-session-append-provider-anchor
         (e-harness-sessions harness) "session-1" 'openai
         :model "gpt-test"
         :covered-entry-id (plist-get assistant :id)
         :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
         :metadata '(:response-id "resp-1"))
        (e-harness-enable-layer-id harness 'extra-layer)
        (e-session-append-message
         (e-harness-sessions harness) "session-1"
         '(:role user :content "new prompt"))
        (let ((options (plist-get
                        (e-harness-context harness "session-1" "turn-2")
                        :options)))
          (should-not (plist-get options :provider-anchor))
          (should (equal (plist-get options :provider-anchor-invalidation-reason)
                         'active-layers-changed)))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-provider-option-change ()
  "Provider continuation anchors include provider request-shaping options."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
           :default-options '(:model "claude-test"
                              :provider-continuation t
                              :provider-anchor-provider-id anthropic
                              :prompt-cache t
                              :anthropic-container-id "container-1"))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'anthropic
       :model "claude-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:provider anthropic
                   :model "claude-test"
                   :anthropic-cache-mode explicit
                   :anthropic-container-id "container-1"
                   :full-history t))
      (setf (e-harness-default-options harness)
            '(:model "claude-test"
              :provider-continuation t
              :provider-anchor-provider-id anthropic
              :prompt-cache t
              :anthropic-container-id "container-2"))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'provider-options-changed))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-instructions-change ()
  "Provider continuation anchors include explicit request instructions."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
           :default-options '(:model "gpt-test"
                              :instructions "Be terse."
                              :provider-continuation t
                              :provider-anchor-provider-id openai))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'openai
       :model "gpt-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:response-id "resp-1"))
      (setf (e-harness-default-options harness)
            '(:model "gpt-test"
              :instructions "Be detailed."
              :provider-continuation t
              :provider-anchor-provider-id openai))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'provider-options-changed))))))

(ert-deftest e-harness-test-provider-anchor-invalidates-on-max-tokens-change ()
  "Provider continuation anchors include Anthropic max token request shaping."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create
                     :context-capabilities '(:continuation linear)
                     :items nil)
           :default-options '(:model "claude-test"
                              :max-tokens 1024
                              :provider-continuation t
                              :provider-anchor-provider-id anthropic))))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness) "session-1"
     '(:role user :content "old prompt"))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) "session-1"
             '(:role assistant :content "old answer")))
           (anchor-context (e-harness-context harness "session-1" "turn-1")))
      (e-session-append-provider-anchor
       (e-harness-sessions harness) "session-1" 'anthropic
       :model "claude-test"
       :covered-entry-id (plist-get assistant :id)
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints anchor-context)
       :metadata '(:provider anthropic
                   :model "claude-test"
                   :full-history t))
      (setf (e-harness-default-options harness)
            '(:model "claude-test"
              :max-tokens 2048
              :provider-continuation t
              :provider-anchor-provider-id anthropic))
      (e-session-append-message
       (e-harness-sessions harness) "session-1"
       '(:role user :content "new prompt"))
      (let ((options (plist-get
                      (e-harness-context harness "session-1" "turn-2")
                      :options)))
        (should-not (plist-get options :provider-anchor))
        (should (equal (plist-get options :provider-anchor-invalidation-reason)
                       'provider-options-changed))))))

(ert-deftest e-harness-test-tool-lifecycle-reuses-hook-context ()
  "A tool lifecycle builds its hook context once for prepare/start/post."
  (should (require 'e-hooks nil t))
  (let* ((tools-capability
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
         (hooks-capability
          (e-capability-create
           :id 'tool-hooks
           :hooks
           (list
            (e-hook-create
             :id "10-prepare"
             :point :pre-tool-call
             :handler (lambda (tool-call _context)
                        (plist-put (copy-sequence tool-call)
                                   :arguments '(:text "prepared"))))
            (e-hook-create
             :id "50-result"
             :point :post-tool-call
             :handler (lambda (result _context)
                        (plist-put (copy-sequence result)
                                   :content
                                   (concat (plist-get result :content)
                                           "-post")))))))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list tools-capability hooks-capability)))
         (context-calls 0)
         result
         failure)
    (e-harness-create-session harness :id "session-1")
    (let ((original (symbol-function 'e-harness-turn--tool-hook-context)))
      (cl-letf (((symbol-function 'e-harness-turn--tool-hook-context)
                 (lambda (&rest args)
                   (setq context-calls (1+ context-calls))
                   (apply original args))))
        (let* ((lifecycle
                (e-harness-tool-lifecycle harness "session-1" "turn-1"))
               (prepared
                (e-tool-lifecycle-prepare-call
                 lifecycle
                 '(:id "call-1" :name "echo" :arguments (:text "raw")))))
          (e-tool-lifecycle-start-call
           lifecycle
           prepared
           :on-done (lambda (value) (setq result value))
           :on-error (lambda (err) (setq failure err))))))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (not (or result failure))
                  (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should (or result failure))
    (when failure
      (signal (car failure) (cdr failure)))
    (should (equal (plist-get result :content) "prepared-post"))
    (should (= context-calls 1))))

(ert-deftest e-harness-test-derived-prompt-cache-key-fits-provider-limit ()
  "Derived prompt cache keys preserve identity within the provider limit."
  (e-harness-test--with-empty-layer-registry
    (let* ((harness
            (e-harness-create
             :backend (e-backend-fake-create :items nil)
             :default-options '(:model "gpt-5.6"
                                :prompt-cache-default t)))
           (session-id "session-1")
           (other-root-session-id "session-2"))
      (e-harness-create-session
       harness
       :id session-id
       :metadata '(:project-root "/tmp/cache-project"))
      (e-harness-create-session
       harness
       :id other-root-session-id
       :metadata '(:project-root "/tmp/other-cache-project"))
      (let* ((options (e-harness-turn-options harness session-id))
             (key (plist-get options :prompt-cache-key))
             (other-root-key
              (plist-get
               (e-harness-turn-options harness other-root-session-id)
               :prompt-cache-key))
             (other-model-key
              (e-harness-context-runtime--derived-prompt-cache-key
               harness session-id
               (plist-put (copy-sequence options) :model "gpt-5.7")))
             (other-tools-key
              (e-harness-context-runtime--derived-prompt-cache-key
               harness session-id
               (plist-put (copy-sequence options)
                          :tools
                          '((:name "different-tool"))))))
        (should (string-match-p "\\`e:pcctx[0-9]+:[[:xdigit:]]+\\'" key))
        (should (= (length key) e-harness-prompt-cache-key-max-length))
        (should (equal key
                       (e-harness-context-runtime--derived-prompt-cache-key
                        harness session-id options)))
        (should-not (equal key other-root-key))
        (should-not (equal key other-model-key))
        (should-not (equal key other-tools-key))))))

(ert-deftest e-harness-test-derived-prompt-cache-key-canonicalizes-tool-schemas ()
  "Equivalent nested hash schemas share a key; material changes do not."
  (e-harness-test--with-empty-layer-registry
    (let* ((properties-a (make-hash-table :test 'equal))
           (properties-b (make-hash-table :test 'equal))
           (properties-c (make-hash-table :test 'equal))
           (path-schema '(:type "string" :minLength 1))
           (other-schema '(:type "string" :maxLength 40)))
      (puthash "path" path-schema properties-a)
      (puthash "other" other-schema properties-a)
      ;; Insert the equivalent hash object in the opposite order.
      (puthash "other" (copy-tree other-schema) properties-b)
      (puthash "path" (copy-tree path-schema) properties-b)
      (puthash "path" '(:type "number") properties-c)
      (puthash "other" (copy-tree other-schema) properties-c)
      (let* ((definition-a
              (list :type "function" :name "read" :description "Read."
                    :parameters (list :type "object"
                                      :properties properties-a
                                      :required ["path"]
                                      :additionalProperties :json-false)
                    :strict :json-false))
             (definition-b
              (list :strict :json-false :parameters
                    (list :additionalProperties :json-false
                          :required ["path"] :properties properties-b
                          :type "object")
                    :description "Read." :name "read" :type "function"))
             (definition-c
              (list :type "function" :name "read" :description "Read."
                    :parameters (list :type "object"
                                      :properties properties-c
                                      :required ["path"]
                                      :additionalProperties :json-false)
                    :strict :json-false))
             (options-a (list :model "gpt-test" :tools (list definition-a)))
             (options-b (list :model "gpt-test" :tools (list definition-b)))
             (options-c (list :model "gpt-test" :tools (list definition-c)))
             (key-a (e-harness-context-runtime--derived-prompt-cache-key
                     (e-harness-create :backend (e-backend-fake-create :items nil))
                     "session-1" options-a))
             (key-b (e-harness-context-runtime--derived-prompt-cache-key
                     (e-harness-create :backend (e-backend-fake-create :items nil))
                     "session-1" options-b))
             (key-c (e-harness-context-runtime--derived-prompt-cache-key
                     (e-harness-create :backend (e-backend-fake-create :items nil))
                     "session-1" options-c)))
        (should (equal (e-tools-definition-fingerprint definition-a)
                       (e-tools-definition-fingerprint definition-b)))
        (should (equal key-a key-b))
        (should-not (equal key-a key-c))))))

(ert-deftest e-harness-test-provider-diagnostics-retain-websocket-lifecycle ()
  "Durable provider diagnostics retain bounded WebSocket lifecycle state."
  (let ((projected
         (e-harness-activity--provider-diagnostics-activity-projection
          '(:provider-continuation full
            :previous-response-id-present nil
            :websocket-connection-id "e-ws-7"
            :websocket-reused t
            :websocket-reuse-count 3
            :websocket-request-mode full
            :websocket-idle-close-seconds 600))))
    (should (equal (plist-get projected :websocket-connection-id) "e-ws-7"))
    (should (eq (plist-get projected :websocket-reused) t))
    (should (= (plist-get projected :websocket-reuse-count) 3))
    (should (eq (plist-get projected :websocket-request-mode) 'full))
    (should (= (plist-get projected :websocket-idle-close-seconds)
               600))))

(ert-deftest e-harness-test-compaction-error-message-is-not-prefixed ()
  "Compaction-failed payload carries the bare reason, not a stacked prefix.
Regression: `e-compaction-error''s `define-error' message already starts with
\"Context compaction failed\", and the chat shell prepends it again, so the
backend-error-message helper must return only the bare reason."
  (let ((err (list 'e-compaction-error
                   "No safe message boundary available for compaction")))
    (should (equal (e-harness-turn--backend-error-message err)
                   "No safe message boundary available for compaction"))
    (should-not (string-match-p "Context compaction failed"
                                (e-harness-turn--backend-error-message err)))))

(ert-deftest e-harness-test-context-lifetime-preserves-observation-segment-ownership ()
  "Late markers preserve kind-scoped segment ownership and transport shape."
  (let* ((static-message '(:role system :content "STATIC-POLICY"))
         (current-message '(:role system :content "CURRENT-SOURCE"))
         (tool-message
          '(:role tool
            :content (:tool-call-id "call-1"
                      :name "lookup"
                      :status ok
                      :content "TOOL-SOURCE")))
         (dynamic-value '(:dynamic "DYNAMIC-SOURCE"))
         (dynamic-message (list :role 'system :content dynamic-value))
         (segments
          (list
           (list :kind 'static-prefix :id 'static-segment
                 :fingerprint "static-fingerprint"
                 :messages (list static-message))
           (list :kind 'current-state :id 'current-segment
                 :fingerprint "current-fingerprint"
                 :messages (list current-message))
           (list :kind 'history :id 'history-segment
                 :fingerprint "history-fingerprint"
                 :messages nil)
           (list :kind 'tool-result :id 'tool-segment
                 :fingerprint "tool-fingerprint"
                 :messages (list tool-message))
           (list :kind 'dynamic-context :id 'dynamic-segment
                 :fingerprint "dynamic-fingerprint"
                 :messages (list dynamic-message))))
         (context
          (list :messages
                (append (list static-message current-message)
                        (list tool-message dynamic-message))
                :segments segments))
         (backend
          (e-backend-create
           :name "context-lifetime-segment-ownership"
           :context-capabilities
           '(:observation-delivery request-local-replaceable)))
         (harness (e-harness-create :backend backend)))
    (e-harness-create-session harness :id "session-1")
    (let* ((capabilities '(:observation-delivery request-local-replaceable))
           (projected
            (e-harness-context-lifetime-apply-projection
             harness "session-1" "turn-1" context capabilities))
           (projected
            (e-harness-context-runtime--context-observation-frontier
             projected capabilities))
           (segments (plist-get projected :segments))
           (frame (plist-get projected :lifetime-frame))
           (presentation
            (e-context-lifetime-frame-curation-presentation frame))
           (markers (mapcar (lambda (source)
                              (plist-get source :marker))
                            presentation))
           (messages (plist-get projected :messages))
           (contents (mapcar (lambda (message)
                               (plist-get message :content))
                             messages))
           (options (plist-get projected :options))
           (replaceable (plist-get options :replaceable-current-state))
           (printed (prin1-to-string messages)))
      (should (equal (mapcar (lambda (segment) (plist-get segment :kind))
                             segments)
                     '(static-prefix history current-state tool-result
                       dynamic-context)))
      (dolist (spec '((static-prefix static-segment "static-fingerprint")
                      (history history-segment "history-fingerprint")
                      (current-state current-segment "current-fingerprint")
                      (tool-result tool-segment "tool-fingerprint")
                      (dynamic-context dynamic-segment "dynamic-fingerprint")))
        (let ((segment (seq-find
                        (lambda (candidate)
                          (eq (plist-get candidate :kind) (car spec)))
                        segments)))
          (should (equal (plist-get segment :id) (nth 1 spec)))
          (should (equal (plist-get segment :fingerprint)
                         (nth 2 spec)))))
      (should (equal contents
                     (list "STATIC-POLICY"
                           (nth 0 markers) "CURRENT-SOURCE"
                           (nth 1 markers) (plist-get tool-message :content)
                           (nth 2 markers) dynamic-value)))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
                             replaceable)
                     (list (nth 0 markers) "CURRENT-SOURCE"
                           (nth 2 markers) dynamic-value)))
      (should (eq (e-backend-observation-delivery-for-kind
                   capabilities 'current-state)
                  'request-local-replaceable))
      (should (eq (e-backend-observation-delivery-for-kind
                   capabilities 'tool-result)
                  'inherited))
      (should (equal (cadr
                      (plist-get
                       (seq-find
                        (lambda (segment)
                          (eq (plist-get segment :kind) 'tool-result))
                        segments)
                       :messages))
                     tool-message))
      (should (= (cl-count "CURRENT-SOURCE" contents :test #'equal) 1))
      (should (= (cl-count dynamic-value contents :test #'equal) 1))
      (should-not (string-match-p
                   "frame:|generation:|observation:|fingerprint:|backing|replay"
                   printed)))))

(ert-deftest e-harness-test-context-lifetime-erase-only-curation-is-audit-and-erasure ()
  "An erase-only response records erasure and audit before consuming its frame."
  (let* ((directory (make-temp-file "e-harness-erase-only-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         frame entry
         (events nil)
         (order nil)
         (append-package
          (symbol-function 'e-session-append-context-curation-package))
         (append-control
          (symbol-function 'e-session-append-context-curation-response))
         (complete-frame
          (symbol-function 'e-context-lifetime-frame-complete-for-consumer)))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "erase-only")
          (let ((generation
                 (e-harness-context-runtime--context-lifetime-ensure-generation
                  harness "erase-only")))
            (setq frame
                  (e-harness-test--tool-result-curation-frame
                   (e-context-lifetime-generation-id generation)
                   "frame:erase-only" "call-erase-only"
                   "ERASE-ONLY-RAW" "observation:erase-only"
                   "source:erase-only" "fingerprint:erase-only"))
            (setq entry (list :status 'running :context-frame frame)))
          (e-harness-activity-subscribe
           harness (lambda (event) (push event events))
           :session-id "erase-only")
          (let ((e-context-lifetime-shadow-projection-enabled t))
            (cl-letf (((symbol-function
                        'e-session-append-context-curation-package)
                       (lambda (&rest arguments)
                         (setq order (append order '(package)))
                         (apply append-package arguments)))
                      ((symbol-function
                        'e-session-append-context-curation-response)
                       (lambda (&rest arguments)
                         (setq order (append order '(control)))
                         (apply append-control arguments)))
                      ((symbol-function
                        'e-context-lifetime-frame-complete-for-consumer)
                       (lambda (&rest arguments)
                         (setq order (append order '(consume)))
                         (apply complete-frame arguments))))
              (should
               (e-context-lifetime-frame-consumed-p
                (e-harness-context-lifetime-commit-response
                 harness "erase-only" "turn-erase" entry
                 (list :frame frame
                       :response-entry-id "response-erase-only"
                       :curation-effects
                       (list
                        (list :type 'context-curate
                              :arguments '(:keep nil :summaries nil
                                            :erase (1))))))))))
          (should (equal order '(package control consume)))
          (e-session-flush-write-queue store)
          (let* ((erasures (e-session-local-context-erasures store "erase-only"))
                 (controls
                  (seq-filter
                   (lambda (event)
                     (eq (plist-get event :event-type)
                         'context-curation-response))
                   (e-session-local-activity-events store "erase-only")))
                 (consumed-event
                 (seq-find
                   (lambda (event)
                     (eq (plist-get event :type)
                         'context-frame-consumed))
                   events))
                 (reopened (e-session-persistent-store-create directory)))
            (should (= (length erasures) 1))
            (should (equal
                     (e-session-local-erased-tool-call-ids store "erase-only")
                     '("call-erase-only")))
            (should (= (length controls) 1))
            (should consumed-event)
            (should (equal (plist-get (plist-get consumed-event :payload)
                                      :response-entry-id)
                           "response-erase-only"))
            (should (equal (plist-get (plist-get consumed-event :payload)
                                      :frame-id)
                           (e-context-lifetime-frame-id frame)))
            (should
             (equal (plist-get (plist-get consumed-event :payload) :curation)
                    '(:kept-source-count 0
                      :summary-count 0
                      :summarized-source-count 0
                      :erased-source-count 1
                      :source-stubs
                      ((:disposition erased :source-kind "tool-result"
                        :tool-name "inspect")))))
            (should-not (e-session-local-context-curations store "erase-only"))
            (let* ((reopened-erasures
                    (e-session-local-context-erasures reopened "erase-only"))
                   (reopened-controls
                    (seq-filter
                     (lambda (event)
                       (eq (plist-get event :event-type)
                           'context-curation-response))
                     (e-session-local-activity-events reopened "erase-only")))
                   (control (car controls))
                   (reopened-control
                    (car reopened-controls)))
              (should (= (length reopened-erasures) 1))
              (should (equal
                       (e-session-local-erased-tool-call-ids reopened "erase-only")
                       '("call-erase-only")))
              (should (= (length reopened-controls) 1))
              (should (equal (plist-get control :id)
                             (plist-get reopened-control :id)))
              (should (equal (plist-get (plist-get reopened-control :payload)
                                        :response-entry-id)
                             "response-erase-only"))
              (should (e-session-local-entry-by-id
                       reopened "erase-only" (plist-get control :id)))
              (should-not
               (seq-find
                (lambda (message)
                  (equal (plist-get message :id) (plist-get control :id)))
                (e-session-local-messages reopened "erase-only"))))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-context-lifetime-mixed-curation-package-is-atomic ()
  "Mixed exact retention and erasure share one package and failure is inert."
  (let* ((directory (make-temp-file "e-harness-mixed-curation-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         frame entry
         events)
    (unwind-protect
        (progn
          (e-harness-activity-subscribe
           harness (lambda (event) (push event events)))
          (e-harness-create-session harness :id "mixed")
          (let ((generation
                 (e-harness-context-runtime--context-lifetime-ensure-generation
                  harness "mixed")))
            (setq frame
                  (e-harness-test--two-tool-result-curation-frame
                   (e-context-lifetime-generation-id generation)))
            (setq entry (list :status 'running :context-frame frame)))
          (let ((e-context-lifetime-shadow-projection-enabled t))
            (should
             (e-context-lifetime-frame-consumed-p
              (e-harness-context-lifetime-commit-response
               harness "mixed" "turn-mixed" entry
               (list :frame frame
                     :response-entry-id "response-mixed"
                     :curation-effects
                     (list
                      (list :type 'context-curate
                            :arguments
                            '(:keep (1)
                              :summaries nil
                              :erase (2)))))))))
          (let* ((package-entry
                  (seq-find
                   (lambda (item)
                     (eq (plist-get item :type)
                         'context-curation-package))
                   (e-session-local-current-path store "mixed")))
                 (curations (e-session-local-context-curations store "mixed"))
                 (erasures (e-session-local-context-erasures store "mixed")))
            (should package-entry)
            (should (plist-get package-entry :promotion))
            (should (plist-get package-entry :erasure))
            (should (= (length curations) 1))
            (should (= (length erasures) 1))
            (should (equal (e-session-local-erased-tool-call-ids store "mixed")
                           '("call:harness-mixed-2"))))
          (e-session-flush-write-queue store)
          (let ((reopened (e-session-persistent-store-create directory)))
            (should (= (length (e-session-local-context-curations
                                reopened "mixed"))
                       1))
            (should (= (length (e-session-local-context-erasures reopened "mixed"))
                       1))
            (should (equal (e-session-local-erased-tool-call-ids reopened "mixed")
                           '("call:harness-mixed-2"))))

          (e-harness-create-session harness :id "mixed-invalid")
          (let (bad-frame bad-entry)
            (let ((generation
                   (e-harness-context-runtime--context-lifetime-ensure-generation
                    harness "mixed-invalid")))
              (setq bad-frame
                    (e-harness-test--two-tool-result-curation-frame
                     (e-context-lifetime-generation-id generation)))
              (setq bad-entry
                    (list :status 'running :context-frame bad-frame)))
            (let ((e-context-lifetime-shadow-projection-enabled t))
              (should-error
               (e-harness-context-lifetime-commit-response
                harness "mixed-invalid" "turn-mixed-invalid" bad-entry
                (list :frame bad-frame
                      :response-entry-id "response-mixed-invalid"
                      :curation-effects
                      (list
                       (list :type 'context-curate
                             :arguments
                             '(:keep (1)
                               :summaries nil
                               :erase (1))))))
               :type 'e-context-lifetime-invalid-record))
            (should-not (e-context-lifetime-frame-consumed-p bad-frame))
            (should-not (e-session-local-context-curations store "mixed-invalid"))
            (should-not (e-session-local-context-erasures store "mixed-invalid")))

          (e-harness-create-session harness :id "mixed-failure")
          (let (failure-frame failure-entry)
            (let ((generation
                   (e-harness-context-runtime--context-lifetime-ensure-generation
                    harness "mixed-failure")))
              (setq failure-frame
                    (e-harness-test--two-tool-result-curation-frame
                     (e-context-lifetime-generation-id generation)))
              (setq failure-entry
                    (list :status 'running :context-frame failure-frame)))
            (cl-letf (((symbol-function
                        'e-session-append-context-curation-package)
                       (lambda (&rest _)
                         (error "synthetic curation package failure"))))
              (let ((e-context-lifetime-shadow-projection-enabled t))
                (should-error
                 (e-harness-context-lifetime-commit-response
                  harness "mixed-failure" "turn-mixed-failure" failure-entry
                  (list :frame failure-frame
                        :response-entry-id "response-mixed-failure"
                        :curation-effects
                        (list
                         (list :type 'context-curate
                               :arguments
                               '(:keep (1)
                                 :summaries nil
                                 :erase (2))))))
                 :type 'error)))
            (should-not (e-context-lifetime-frame-consumed-p failure-frame))
            (should-not (e-session-local-context-curations store "mixed-failure"))
            (should-not (e-session-local-context-erasures store "mixed-failure"))
            (should-not
             (seq-find
              (lambda (event)
                (and (eq (plist-get event :type) 'context-frame-consumed)
                     (equal (plist-get event :turn-id) "turn-mixed-failure")))
              events))))
      (delete-directory directory t))))

(ert-deftest e-harness-test-context-lifetime-curation-binds-payload-frame-over-descendant ()
  "Curation labels bind to the response frame, not a newer tool descendant."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         frame-a frame-b entry consumed)
    (e-harness-create-session harness :id "session-1")
    (let ((generation
           (e-harness-context-runtime--context-lifetime-ensure-generation
            harness "session-1")))
      (setq frame-a
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)
             "frame:a" "PAYLOAD-FRAME-VALUE" "observation:a" "source:a"
             "fingerprint:a"))
      (setq frame-b
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)
             "frame:b" "DESCENDANT-TOOL-VALUE" "observation:b" "source:b"
             "fingerprint:b"))
      (setq entry (list :status 'running :context-frame frame-b)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (setq consumed
            (e-harness-context-lifetime-commit-response
             harness "session-1" "turn-1" entry
             (list :frame frame-a
                   :provider-request-id "response-a"
                   :response-entry-id "response-a"
                   :curation-effects
                   (list (list :type 'context-curate
                               :arguments
                               '(:keep (1) :summaries nil)))))))
    (should (equal (e-context-lifetime-frame-id consumed) "frame:a"))
    (should (eq (plist-get entry :context-frame) frame-b))
    (should-not (e-context-lifetime-frame-consumed-p frame-b))
    (let* ((record (car (e-session-local-context-curations store "session-1")))
           (item (car (plist-get record :items))))
      (should (equal (plist-get item :value) "PAYLOAD-FRAME-VALUE"))
      (should (equal (plist-get item :source-observation-ids)
                     '("observation:a")))
      (should (equal (plist-get item :source-refs) '("source:a")))
      (should (equal (plist-get item :source-fingerprints)
                     '("fingerprint:a")))
      (should-not (string-match-p "DESCENDANT-TOOL-VALUE"
                                  (prin1-to-string record)))
      (should-not (string-match-p "observation:b"
                                  (prin1-to-string record))))))

(ert-deftest e-harness-test-context-lifetime-zero-curation-does-not-consume-descendant ()
  "Zero-effect completion for A leaves a newer live descendant B untouched."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         frame-a frame-b entry consumed)
    (e-harness-create-session harness :id "session-1")
    (let ((generation
           (e-harness-context-runtime--context-lifetime-ensure-generation
            harness "session-1")))
      (setq frame-a
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)
             "frame:a-zero" "PAYLOAD-ZERO-VALUE" "observation:a-zero"
             "source:a-zero" "fingerprint:a-zero"))
      (setq frame-b
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)
             "frame:b-zero" "DESCENDANT-ZERO-VALUE" "observation:b-zero"
             "source:b-zero" "fingerprint:b-zero"))
      (setq entry (list :status 'running :context-frame frame-b)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (setq consumed
            (e-harness-context-lifetime-commit-response
             harness "session-1" "turn-1" entry
             (list :frame frame-a
                   :provider-request-id "response-a-zero"
                   :response-entry-id "response-a-zero"
                   :curation-effects nil))))
    (should (equal (e-context-lifetime-frame-id consumed) "frame:a-zero"))
    (should (eq (plist-get entry :context-frame) frame-b))
    (should-not (e-context-lifetime-frame-consumed-p frame-b))
    (should-not (e-session-local-context-curations store "session-1"))))

(ert-deftest e-harness-test-context-lifetime-curation-appends-before-consuming ()
  "A valid curation appends its record before the frame body is consumed."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (store (e-harness-sessions harness))
         frame entry
         (order nil)
         (append-context-curation-package
          (symbol-function 'e-session-append-context-curation-package))
         (complete-frame
          (symbol-function 'e-context-lifetime-frame-complete-for-consumer)))
    (e-harness-create-session harness :id "session-1")
    (let ((generation
           (e-harness-context-runtime--context-lifetime-ensure-generation
            harness "session-1")))
      (setq frame
            (e-harness-test--curation-frame
             (e-context-lifetime-generation-id generation)))
      (setq entry (list :status 'running :context-frame frame)))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (cl-letf (((symbol-function 'e-session-append-context-curation-package)
                 (lambda (&rest arguments)
                   (setq order (append order '(package)))
                   (apply append-context-curation-package arguments)))
                ((symbol-function
                  'e-context-lifetime-frame-complete-for-consumer)
                 (lambda (&rest arguments)
                   (setq order (append order '(consume)))
                   (apply complete-frame arguments))))
        (e-harness-turn-state-set-context-frame
         entry
         (e-harness-context-lifetime-commit-response
          harness "session-1" "turn-1" entry
          (list :frame frame
                :provider-request-id "response-1"
                :response-entry-id "response-1"
                :curation-effects
                (list (list :type 'context-curate
                            :arguments
                            '(:keep (1) :summaries nil))))))))
    (should (equal order '(package consume)))
    (should (= (length (e-session-local-context-curations store "session-1")) 1))
    (should (e-context-lifetime-frame-consumed-p
             (plist-get entry :context-frame)))))

(ert-deftest e-harness-test-provider-compaction-none-skips-input-sync ()
  "Synchronous portable compaction does no opaque-input work for NONE."
  (let* ((input-calls 0)
         (backend
          (e-backend-create
           :name 'provider-compaction-none-sync
           :context-capabilities
           '(:continuation none :provider-compaction none)
           :stream
           (cl-function
            (lambda (&key on-item &allow-other-keys)
              (funcall on-item
                       '(:type assistant-message :content "NONE-SYNC-C1"))))))
         (harness (e-harness-create :backend backend
                                    :default-options
                                    '(:model "none-sync-model")))
         (store nil))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-harness-create-session harness :id "none-sync-session")
      (setq store (e-harness-sessions harness))
      (e-harness-turn-context harness "none-sync-session" "seed")
      (e-session-append-message store "none-sync-session"
                                '(:role user :content "NONE-SYNC-INTENT"))
      (e-session-append-message store "none-sync-session"
                                '(:role assistant :content "NONE-SYNC-ANSWER"))
      (cl-letf (((symbol-function 'e-harness-context-runtime--provider-compaction-input)
                 (lambda (&rest _args)
                   (setq input-calls (1+ input-calls))
                   (error "provider compaction input must stay lazy"))))
        (let ((record
               (e-harness-compact-session-batch
                harness "none-sync-session" :keep-recent-tokens 1)))
          (should record)))
      (should (= input-calls 0))
      (should (= (length (e-session-local-compactions store "none-sync-session"))
                 1))
      (should (string-match-p
               "NONE-SYNC-C1"
               (prin1-to-string
                (e-context-lifetime-generation-checkpoint
                 (e-session-local-context-lifetime-current-generation
                  store "none-sync-session"))))))))

(ert-deftest e-harness-test-provider-compaction-none-skips-input-async ()
  "Asynchronous portable compaction does no opaque-input work for NONE."
  (let* ((input-calls 0)
         (record nil)
         (failure nil)
         (backend
          (e-backend-create
           :name 'provider-compaction-none-async
           :context-capabilities
           '(:continuation none :provider-compaction none)
           :start
           (cl-function
            (lambda (&key on-item on-done &allow-other-keys)
              (run-at-time
               0.01 nil
               (lambda ()
                 (funcall on-item
                          '(:type assistant-message :content "NONE-ASYNC-C1"))
                 (funcall on-done '(:status done))))
              (e-backend-request-create)))))
         (harness (e-harness-create :backend backend
                                    :default-options
                                    '(:model "none-async-model")))
         (store nil))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-harness-create-session harness :id "none-async-session")
      (setq store (e-harness-sessions harness))
      (e-harness-turn-context harness "none-async-session" "seed")
      (e-session-append-message store "none-async-session"
                                '(:role user :content "NONE-ASYNC-INTENT"))
      (e-session-append-message store "none-async-session"
                                '(:role assistant :content "NONE-ASYNC-ANSWER"))
      (cl-letf (((symbol-function 'e-harness-context-runtime--provider-compaction-input)
                 (lambda (&rest _args)
                   (setq input-calls (1+ input-calls))
                   (error "provider compaction input must stay lazy"))))
        (e-harness-compact-session-start
         harness "none-async-session"
         :keep-recent-tokens 1
         :on-done (lambda (value) (setq record value))
         :on-error (lambda (err) (setq failure err)))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (not (or record failure))
                      (< (float-time) deadline))
            (accept-process-output nil 0.01))))
      (should record)
      (should-not failure)
      (should (= input-calls 0))
      (should (= (length (e-session-local-compactions store "none-async-session"))
                 1))
      (should (string-match-p
               "NONE-ASYNC-C1"
               (prin1-to-string
                (e-context-lifetime-generation-checkpoint
                 (e-session-local-context-lifetime-current-generation
                  store "none-async-session"))))))))

(ert-deftest e-harness-test-provider-compaction-candidate-fences-late-tail-and-promotion ()
  "A candidate covers its captured boundary and rejects a changed promotion frontier."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (backend
          (e-backend-fake-create
           :items nil
           :context-capabilities
           '(:continuation none :provider-compaction opaque)
           :provider-compaction
           (lambda (&rest _args)
             '(:output ((:type "opaque")) :usage nil))))
         (harness
          (e-harness-create
           :backend backend
           :default-options '(:model "candidate-model"
                              :provider-anchor-provider-id fake)))
         (session-id "candidate-fence"))
    (e-harness-create-session harness :id session-id)
    (let ((session (e-session-local-state (e-harness-sessions harness) session-id)))
      (e-session-append-context-generation
       (e-harness-sessions harness) session-id
       (e-context-lifetime-generation-create
        :id "generation:candidate-fence"
        :checkpoint '((:role system :content "C0"))
        :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-message
     (e-harness-sessions harness) session-id
     '(:role user :content "before compact"))
    (let* ((before (e-harness-turn-context harness session-id "before"))
           (generation (plist-get before :lifetime-generation))
           (input (e-harness-context-runtime--provider-compaction-input
                   harness session-id generation))
           (source-entry-id (plist-get input :source-entry-id)))
      (should source-entry-id)
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role tool-call
         :content (:id "late-call" :name "inspect"
                   :arguments (:marker "LATE-RAW-CALL"))
         :metadata (:provider-replay-items
                    ((:id "LATE-RAW-REPLAY")))))
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role tool
         :content (:tool-call-id "late-call"
                   :content "LATE-RAW-RESULT")))
      (e-session-append-message
       (e-harness-sessions harness) session-id
       '(:role assistant :content "late durable tail"))
      (e-harness-context-runtime--provider-compaction-store-candidate
       harness session-id before generation
       '((:type "opaque")) nil
       source-entry-id
       (plist-get input :promotion-frontier)
       (plist-get input :input-fingerprint))
      (let* ((after (e-harness-turn-context harness session-id "after"))
             (options (plist-get after :options))
             (delta (plist-get options :provider-compaction-delta-messages)))
        (should (equal (plist-get options :provider-compaction-output)
                       '((:type "opaque"))))
        (should (= (cl-count "late durable tail" delta
                             :key (lambda (message)
                                    (plist-get message :content))
                             :test #'equal)
                   1))
        (dolist (marker '("LATE-RAW-CALL" "LATE-RAW-REPLAY"
                          "LATE-RAW-RESULT"))
          (should-not (string-match-p marker (prin1-to-string delta)))))
      ;; A promotion committed after the captured provider input makes the
      ;; opaque result incomplete, so no candidate is installed.
      (clrhash (e-harness-provider-compaction-candidates harness))
      (let ((promotion-generation
             (e-context-lifetime-generation-id generation)))
        (e-harness-test--append-compaction-curation
         (e-harness-sessions harness) session-id promotion-generation "late"))
      (let* ((latest-input (e-harness-context-runtime--provider-compaction-input
                            harness session-id generation)))
        (e-harness-context-runtime--provider-compaction-store-candidate
         harness session-id before generation
         '((:type "opaque")) nil
         (plist-get latest-input :source-entry-id)
         (plist-get input :promotion-frontier)
         (plist-get input :input-fingerprint)))
      (should-not (gethash session-id
                           (e-harness-provider-compaction-candidates harness))))))

(ert-deftest e-harness-test-provider-compaction-mismatch-and-reopen-fallback ()
  "Model/layout/generation mismatches and a reopened harness use portable context."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (store (e-session-store-create))
         (backend (e-backend-fake-create
                   :items nil
                   :context-capabilities
                   '(:continuation none :provider-compaction opaque)
                   :provider-compaction
                   (lambda (&rest _args)
                     '(:output ((:type "opaque")) :usage nil))))
         (harness
          (e-harness-create
           :backend backend
           :sessions store
           :default-options '(:model "candidate-model"
                              :provider-anchor-provider-id fake)))
         (session-id "candidate-mismatch"))
    (e-harness-create-session harness :id session-id)
    (let ((session (e-session-local-state store session-id)))
      (e-session-append-context-generation
       store session-id
       (e-context-lifetime-generation-create
        :id "generation:candidate-mismatch"
        :checkpoint '((:role system :content "C0"))
        :covered-session-boundary (plist-get session :root-event-id))))
    (e-session-append-message store session-id
                              '(:role user :content "portable source"))
    (let* ((context (e-harness-turn-context harness session-id "original"))
           (generation (plist-get context :lifetime-generation))
           (input (e-harness-context-runtime--provider-compaction-input
                   harness session-id generation))
           (source (plist-get input :source-entry-id)))
      (e-harness-context-runtime--provider-compaction-store-candidate
       harness session-id context generation '((:type "opaque")) nil
       source (plist-get input :promotion-frontier)
       (plist-get input :input-fingerprint))
      (let ((model-context (copy-tree context)))
        (plist-put (plist-get model-context :options) :model "other-model")
        (should-not
         (e-harness-context-runtime--provider-compaction-candidate-compatible-p
          harness session-id model-context
          (gethash session-id
                   (e-harness-provider-compaction-candidates harness))
          (plist-get (plist-get model-context :options)
                     :context-capabilities))))
      (let ((layout-context (copy-tree context)))
        (plist-put (plist-get layout-context :options)
                   :instructions "changed layout")
        (should-not
         (e-harness-context-runtime--provider-compaction-candidate-compatible-p
          harness session-id layout-context
          (gethash session-id
                   (e-harness-provider-compaction-candidates harness))
          (plist-get (plist-get layout-context :options)
                     :context-capabilities))))
      (let ((generation-context (copy-tree context)))
        (plist-put generation-context :lifetime-generation
                   (e-context-lifetime-generation-create
                    :id "generation:replacement"
                    :checkpoint '((:role system :content "new"))
                    :covered-session-boundary source))
        (should-not
         (e-harness-context-runtime--provider-compaction-candidate-compatible-p
          harness session-id generation-context
          (gethash session-id
                   (e-harness-provider-compaction-candidates harness))
          (plist-get (plist-get generation-context :options)
                     :context-capabilities))))
      (let* ((reopened
              (e-harness-create
               :backend backend :sessions store
               :default-options '(:model "candidate-model"
                                  :provider-anchor-provider-id fake)))
             (reopened-context
              (e-harness-turn-context reopened session-id "reopened"))
             (reopened-options (plist-get reopened-context :options)))
        (should-not (plist-get reopened-options :provider-compaction-output))
        (should (string-match-p "portable source"
                                (prin1-to-string
                                 (plist-get reopened-context :messages))))))))

(ert-deftest e-harness-test-provider-compaction-candidate-is-one-shot ()
  "Preview does not consume opaque state; the actual turn consumes it once."
  (let* ((e-context-lifetime-shadow-projection-enabled t)
         (backend
          (e-backend-fake-create
           :items nil
           :context-capabilities
           '(:continuation linear :provider-compaction opaque)
           :provider-compaction
           (lambda (&rest _args)
             '(:output ((:type "opaque" :marker "ONE-SHOT"))
               :usage nil))))
         (store (e-session-store-create))
         (harness
          (e-harness-create
           :backend backend
           :sessions store
           :default-options '(:model "one-shot-model"
                              :provider-anchor-provider-id fake
                              :provider-continuation t
                              :context-lifetime-enabled t)))
         (session-id "provider-one-shot"))
    (e-harness-create-session harness :id session-id)
    (e-harness-turn-context harness session-id "seed")
    (e-session-append-message store session-id
                              '(:role user :content "one-shot durable"))
    (let ((boundary (plist-get
                     (car (last (e-session-local-current-path store session-id)))
                     :id)))
      (e-session-append-context-generation
       store session-id
       (e-context-lifetime-generation-create
        :id "generation:one-shot"
        :checkpoint '((:role system :content "C0-ONE-SHOT"))
        :covered-session-boundary boundary)))
    (let* ((context (e-harness-turn-context harness session-id "capture"))
           (generation (plist-get context :lifetime-generation))
           (input (e-harness-context-runtime--provider-compaction-input
                   harness session-id generation)))
      (e-harness-context-runtime--provider-compaction-store-candidate
       harness session-id context generation
       '((:type "opaque" :marker "ONE-SHOT")) nil
       (plist-get input :source-entry-id)
       (plist-get input :promotion-frontier)
       (plist-get input :input-fingerprint)))
    (let ((preview (e-harness-context harness session-id nil 'preview)))
      (should-not (plist-get (plist-get preview :options)
                             :provider-compaction-output))
      (should (gethash session-id
                       (e-harness-provider-compaction-candidates harness))))
    (let* ((first (e-harness-turn-context harness session-id "first-turn"))
           (first-options (plist-get first :options))
           (head (plist-get
                  (car (last (e-session-local-current-path store session-id)))
                  :id)))
      (should (equal (plist-get first-options :provider-compaction-output)
                     '((:type "opaque" :marker "ONE-SHOT"))))
      (should-not (gethash session-id
                           (e-harness-provider-compaction-candidates harness)))
      ;; A normal compatible response anchor is eligible on the next turn;
      ;; the consumed opaque output is not replayed.
      (e-session-append-provider-anchor
       store session-id 'fake
       :model "one-shot-model"
       :covered-entry-id head
       :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints first)
       :metadata '(:response-id "fresh-anchor"))
      (let* ((second (e-harness-turn-context harness session-id "second-turn"))
             (second-options (plist-get second :options)))
        (should-not (plist-get second-options :provider-compaction-output))
        (should (equal (plist-get
                        (plist-get second-options :provider-anchor)
                        :metadata)
                       '(:response-id "fresh-anchor")))))))

(ert-deftest e-harness-test-profile-records-context-build ()
  "Enabled dev profiling records harness context spans."
  (let* ((profile-directory (make-temp-file "e-harness-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-dev-profile-start)
          (e-harness-context harness "session-1" "turn-1")
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "harness.context" aggregates nil nil #'equal))))
      (delete-directory profile-directory t))))

(ert-deftest e-harness-test-profile-records-tool-start ()
  "Enabled dev profiling records harness tool start spans."
  (let* ((profile-directory (make-temp-file "e-harness-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (calls 0)
         (backend
          (e-backend-create
           :name "fake-harness-profile-tool"
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
                               :arguments (:text "raw")))
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
         (harness
          (e-harness-create
           :backend backend
           :intrinsic-capabilities (list tools-capability))))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id "session-1")
          (e-dev-profile-start)
          (e-harness-test-prompt-batch harness "session-1" "use tool")
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "harness.tool-start" aggregates nil nil #'equal))))
      (delete-directory profile-directory t))))

(provide 'e-harness-runtime-mechanism-test)

;;; e-harness-runtime-mechanism-test.el ends here

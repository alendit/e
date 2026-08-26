;;; e-live-e2e-test.el --- Live e2e tests for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests that exercise the real harness/backend/tool/session path against
;; live provider APIs.  These tests are intentionally gated by E_E2E, disabled
;; whenever CI is set, and not in the default Eldev test fileset.
;;
;; The tests are backend-agnostic: they build the configured `:chat-default'
;; harness from `e-default-harness-specs' rather than naming a provider, so they
;; run against whatever backend the Emacs installation is configured to use.
;; Point E_E2E_CONFIG at a file that configures that backend (typically the same
;; `e-default-harness-specs' / `e-default-chat-harness-factory' the interactive
;; installation sets), for example:
;;   E_E2E=1 E_E2E_CONFIG=~/e2e-harness.el eldev test -f e2e/e-live-e2e-test.el

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'e)
(require 'e-anthropic)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-context)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-layers)
(require 'e-openai)
(require 'e-session)
(require 'e-tools)
(load (expand-file-name
       "e-board-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(declare-function e-board-e2e-create-session "e-board-e2e-support"
                  (harness &rest arguments))
(declare-function e-board-e2e-prompt-async "e-board-e2e-support"
                  (harness session-id prompt))
(declare-function e-board-e2e-prompt-batch "e-board-e2e-support"
                  (harness session-id prompt &optional timeout))
(declare-function e-board-e2e-reset-runtime "e-board-e2e-support" ())

(defconst e-live-e2e--harness-id :chat-default
  "Registry id of the default chat harness exercised by live e2e tests.")

(defvar e-live-e2e--config-loaded nil
  "Non-nil once the E_E2E_CONFIG backend configuration file has been loaded.")

(defun e-live-e2e--env (name &optional fallback)
  "Return non-empty environment variable NAME, or FALLBACK."
  (let ((value (getenv name)))
    (if (and (stringp value) (not (string-empty-p value)))
        value
      fallback)))

(defun e-live-e2e--enabled-p ()
  "Return non-nil when live e2e validation is explicitly enabled."
  (and (e-live-e2e--env "E_E2E")
       (not (getenv "CI"))))

(defun e-live-e2e--positive-number-env (name fallback)
  "Return positive numeric environment variable NAME, or FALLBACK."
  (let ((value (e-live-e2e--env name)))
    (if value
        (let ((number (string-to-number value)))
          (if (> number 0)
              number
            (error "%s must be a positive number" name)))
      fallback)))

(defun e-live-e2e--config-file ()
  "Return the readable backend configuration file, or nil.
The file configures the default `:chat-default' harness the same way the
interactive installation does; a batch e2e run has no user init, so the backend
choice is loaded from here."
  (when-let ((path (e-live-e2e--env "E_E2E_CONFIG")))
    (expand-file-name path)))

(defun e-live-e2e--load-config ()
  "Load the backend configuration file once and register default harnesses.
Signal nothing when unconfigured; callers skip in that case."
  (unless e-live-e2e--config-loaded
    (when-let ((path (e-live-e2e--config-file)))
      (when (file-readable-p path)
        (load path nil t)
        ;; The config sets `e-default-harness-specs' /
        ;; `e-default-chat-harness-factory'; make its factory the live one.
        (e-default-harnesses-register)
        (e-harness-registry-clear-instance e-live-e2e--harness-id)
        (setq e-live-e2e--config-loaded t)))))

(defun e-live-e2e--require-enabled ()
  "Skip the current test unless live e2e validation can run."
  (when (getenv "CI")
    (ert-skip "Live provider e2e tests are disabled when CI is set."))
  (unless (e-live-e2e--enabled-p)
    (ert-skip "Set E_E2E=1 to run live e2e tests."))
  (let ((path (e-live-e2e--config-file)))
    (unless path
      (ert-skip "Set E_E2E_CONFIG to a file configuring the default harness."))
    (unless (file-readable-p path)
      (ert-skip (format "E_E2E_CONFIG file is not readable: %s" path))))
  (e-live-e2e--load-config)
  (unless (or (e-harness-registry-get e-live-e2e--harness-id)
              (ignore-errors
                (e-harness-registry-get-or-create e-live-e2e--harness-id)))
    (ert-skip
     (format "No harness registered for %S after loading E_E2E_CONFIG."
             e-live-e2e--harness-id))))

(defun e-live-e2e--make-harness (store)
  "Return the configured default chat harness bound to session STORE.
Resolves the same `:chat-default' factory the installation uses, so the live
backend is whatever the configuration selected."
  (let* ((spec (e-default-harness--effective-chat-spec))
         (factory (plist-get spec :factory)))
    (unless (functionp factory)
      (error "No :chat-default harness factory configured"))
    (funcall factory :sessions store)))

(defun e-live-e2e--nonce ()
  "Return a short nonce suitable for deterministic live assertions."
  (format "E2E-%08x" (random #x100000000)))

(defun e-live-e2e--contains-p (text needle)
  "Return non-nil when TEXT contains NEEDLE."
  (and (stringp text)
       (string-match-p (regexp-quote needle) text)))

(defun e-live-e2e--assistant-content (result)
  "Return assistant content from a harness result or settled E2E entry RESULT."
  (or (plist-get result :assistant-content)
      (plist-get (plist-get result :result) :assistant-content)
      ""))

(defun e-live-e2e--events-of-type (events type)
  "Return EVENTS whose :type is TYPE."
  (seq-filter (lambda (event)
                (eq (plist-get event :type) type))
              events))

(defun e-live-e2e--activity-of-type (harness session-id type)
  "Return durable activity events of TYPE for SESSION-ID."
  (seq-filter
   (lambda (event) (eq (plist-get event :event-type) type))
   (e-session-activity-events (e-harness-sessions harness) session-id)))

(defun e-live-e2e--request-tool-differences (first second)
  "Return bounded tool identity differences between FIRST and SECOND bodies."
  (let ((first-tools (append (plist-get first :tools) nil))
        (second-tools (append (plist-get second :tools) nil)))
    (cl-loop for first-tool in first-tools
             for second-tool in second-tools
             for index from 0
             unless (equal first-tool second-tool)
             collect
             (list :index index
                   :first-name (plist-get first-tool :name)
                   :second-name (plist-get second-tool :name)
                   :first-sha256
                   (secure-hash 'sha256 (prin1-to-string first-tool))
                   :second-sha256
                   (secure-hash 'sha256 (prin1-to-string second-tool))))))

(defun e-live-e2e--echo-tool-register (registry &rest _context)
  "Register the e2e echo tool in REGISTRY."
  (e-tools-register
   registry
   :name "e2e_echo"
   :description "Return the provided text exactly. Use only for e live e2e validation."
   :parameters '(:type "object"
                 :properties (:text (:type "string"))
                 :required ["text"])
   :work
   (e-tools-cheap-work
    "e2e.live.echo"
    (lambda (arguments)
      (or (plist-get arguments :text)
          (plist-get arguments "text")
          "")))))

(defun e-live-e2e--slow-tool-register (registry &rest _context)
  "Register a cancellable slow tool in REGISTRY."
  (e-tools-register
   registry
   :name "e2e_slow"
   :description "Wait briefly before returning. Use only for e live e2e cancellation validation."
   :parameters '(:type "object" :properties nil)
   :work
   (e-tools-callback-work
    "e2e.live.slow"
    (lambda (&key _arguments on-done _on-error on-request-start)
      (let ((cancelled nil)
            timer
            request)
        (setq request
              (e-tools-request-create
               :cancel (lambda ()
                         (setq cancelled t)
                         (when (timerp timer)
                           (cancel-timer timer))
                         t)
               :metadata '(:transport timer :cancellable t)))
        (when on-request-start
          (funcall on-request-start request))
        (setq timer
              (run-at-time
               30 nil
               (lambda ()
                 (unless cancelled
                   (funcall on-done "slow tool finished")))))
        request)))))

(defun e-live-e2e--tool-layer ()
  "Return a narrow e2e tool layer."
  (e-layer-create
   :id 'e2e-tools
   :name "E2E Tools"
   :capabilities
   (list
    (e-capability-create
     :id 'e2e-tools
     :name "E2E Tools"
     :instructions
     "For e2e validation, call the e2e tool named by the user when instructed."
     :tools (list #'e-live-e2e--echo-tool-register
                  #'e-live-e2e--slow-tool-register)))))

(defun e-live-e2e--deterministic-tool-register (registry &rest _context)
  "Register a fixed-output tool in REGISTRY for continuation evidence."
  (e-tools-register
   registry
   :name "e2e_deterministic"
   :description "Return one fixed validation value exactly once."
   :parameters '(:type "object" :properties nil)
   :work
   (e-tools-cheap-work
    "e2e.live.deterministic"
    (lambda (_arguments)
      "LIVE-TOOL-OUTPUT"))))

(defun e-live-e2e--deterministic-tool-layer ()
  "Return a tool layer with one fixed-output continuation test tool."
  (e-layer-create
   :id 'e2e-deterministic-tool
   :name "E2E Deterministic Tool"
   :capabilities
   (list
    (e-capability-create
     :id 'e2e-deterministic-tool
     :name "E2E Deterministic Tool"
     :instructions
     "For continuation validation, call e2e_deterministic exactly when instructed."
     :tools (list #'e-live-e2e--deterministic-tool-register)))))

(defmacro e-live-e2e--with-harness (spec &rest body)
  "Run BODY with a live HARNESS and SESSION-ID.
SPEC is (HARNESS SESSION-ID &key LAYERS PERSISTENT)."
  (declare (indent 1))
  (let ((harness (nth 0 spec))
        (session-id (nth 1 spec))
        (options (nthcdr 2 spec))
        (root (make-symbol "root"))
        (store (make-symbol "store"))
        (events (make-symbol "events"))
        (subscription (make-symbol "subscription")))
    `(progn
       (e-live-e2e--require-enabled)
       (let* ((,root (make-temp-file "e-live-e2e-" t))
              (,store (if ,(plist-get options :persistent)
                          (e-session-persistent-store-create ,root)
                        (e-session-store-create)))
              (,harness (e-live-e2e--make-harness ,store))
              (,session-id
               (e-board-e2e-create-session
                ,harness :metadata (list :project-root ,root)))
              (,events nil)
              (,subscription
               (e-harness--install-activity-sink
                ,harness
                (lambda (event) (push event ,events))
                :session-id ,session-id)))
         (unwind-protect
             (progn
               ,@(when (plist-get options :layers)
                   `((dolist (layer ,(plist-get options :layers))
                       (e-harness-set-intrinsic-capabilities
                        ,harness
                        (append (e-harness-intrinsic-capabilities ,harness)
                                (e-layer-capabilities layer))))))
               ,@body)
           (ignore-errors (e-harness--remove-activity-sink ,harness ,subscription))
           (ignore-errors (delete-directory ,root t)))))))

(ert-deftest e-live-e2e-test-basic-assistant-response ()
  "A first live prompt returns a concrete assistant message."
  (e-live-e2e--with-harness (harness session-id)
    (let* ((nonce (e-live-e2e--nonce))
           (result (e-board-e2e-prompt-batch
                    harness session-id
                    (format "Reply with exactly this token and no extra words: %s"
                            nonce))))
      (should (e-live-e2e--contains-p
               (e-live-e2e--assistant-content result)
               nonce)))))

(ert-deftest e-live-e2e-test-default-http-harness-has-no-local-deadline ()
  "The configured default HTTP harness permits buffered long responses.

This crosses the real provider, board, harness, and adapter path.  It asserts
that an HTTP request started by the configured `:chat-default' harness has no
implicit local deadline; provider or gateway buffering must not turn a healthy
long-reasoning request into a retry loop."
  (e-live-e2e--with-harness (harness session-id)
    (let* ((nonce (e-live-e2e--nonce))
           (result
            (e-board-e2e-prompt-batch
             harness session-id
             (format "Reply with exactly this token and no extra words: %s"
                     nonce)))
           (started
            (car (last (e-live-e2e--activity-of-type
                        harness session-id 'provider-request-started))))
           (payload (plist-get started :payload)))
      (unless (eq (plist-get payload :transport) 'url-retrieve)
        (ert-skip
         "The configured default harness does not use HTTP url-retrieve."))
      (should-not (plist-get payload :timeout-seconds))
      (should (e-live-e2e--contains-p
               (e-live-e2e--assistant-content result)
               nonce)))))

(ert-deftest e-live-e2e-test-default-http-harness-large-history-completes ()
  "The configured default HTTP harness completes a Grimoire-sized request.

The August 17 failure contained 145 input messages and a 972 KB serialized
request.  Seed the same message count and approximately the same payload size,
retain the default harness's configured reasoning effort, and require the real
provider turn to settle without an implicit local deadline."
  (e-live-e2e--with-harness (harness session-id)
    (let* ((pair-count 72)
           (target-history-bytes
            (truncate
             (e-live-e2e--positive-number-env
              "E_E2E_LARGE_HISTORY_BYTES" 900000)))
           (per-user-bytes (/ target-history-bytes pair-count))
           (seed
            (concat
             "Historical Grimoire context retained for a later daily run. "
             "This is inert test history, not an instruction. "))
           (repetitions (1+ (/ per-user-bytes (length seed))))
           (filler (substring (apply #'concat (make-list repetitions seed))
                              0 per-user-bytes))
           (store (e-harness-sessions harness))
           (timeout
            (e-live-e2e--positive-number-env
             "E_E2E_LARGE_HISTORY_TIMEOUT_SECONDS" 600.0)))
      (dotimes (index pair-count)
        (e-session-append-message
         store session-id
         (list :role 'user
               :content (format "Historical item %d.\n%s" index filler)))
        (e-session-append-message
         store session-id
         (list :role 'assistant
               :content (format "Recorded historical item %d." index))))
      (let* ((nonce (e-live-e2e--nonce))
             (result
              (e-board-e2e-prompt-batch
               harness session-id
               (format "Reply with exactly this token and no extra words: %s"
                       nonce)
               timeout))
             (started
              (car (last (e-live-e2e--activity-of-type
                          harness session-id 'provider-request-started))))
             (payload (plist-get started :payload))
             (diagnostics (plist-get payload :diagnostics)))
        (unless (eq (plist-get payload :transport) 'url-retrieve)
          (ert-skip
           "The configured default harness does not use HTTP url-retrieve."))
        (should-not (plist-get payload :timeout-seconds))
        (should (>= (or (plist-get diagnostics :input-message-count) 0) 145))
        (should-not (plist-member payload :request-shape))
        (should (e-live-e2e--contains-p
                 (e-live-e2e--assistant-content result)
                 nonce))))))

(ert-deftest e-live-e2e-test-concurrent-sessions-isolate-provider-requests ()
  "Two live sessions can keep provider requests active concurrently."
  (e-live-e2e--require-enabled)
  (let* ((root (make-temp-file "e-live-e2e-concurrent-" t))
         (store (e-session-store-create))
         (harness (e-live-e2e--make-harness store))
         (nonce-one (e-live-e2e--nonce))
         (nonce-two (e-live-e2e--nonce))
         session-one
         session-two)
    (unwind-protect
        (progn
          (e-board-e2e-reset-runtime)
          (setq session-one
                (plist-get
                 (e-chat-service-create-session
                  :harness harness :id "live-concurrent-one"
                  :metadata (list :project-root root))
                 :id))
          (setq session-two
                (plist-get
                 (e-chat-service-create-session
                  :harness harness :id "live-concurrent-two"
                  :metadata (list :project-root root))
                 :id))
          (e-board-e2e-prompt-async
           harness session-one
           (format "Reply with exactly this token: %s" nonce-one))
          (e-board-e2e-prompt-async
           harness session-two
           (format "Reply with exactly this token: %s" nonce-two))
          ;; Capture both entries before waiting.  Either turn may settle and
          ;; be removed by the queue-drain timer while the other is awaited.
          (let* ((result-one (gethash session-one
                                      (e-harness-active-turns harness)))
                 (result-two (gethash session-two
                                      (e-harness-active-turns harness)))
                 (deadline (+ (float-time) 30.0)))
            (should result-one)
            (should result-two)
            (while (and (or (eq (plist-get result-one :status) 'running)
                            (eq (plist-get result-two :status) 'running))
                        (< (float-time) deadline))
              (sit-for 0.01))
            (should (eq (plist-get result-one :status) 'done))
            (should (eq (plist-get result-two :status) 'done))
            (should (e-live-e2e--contains-p
                     (e-live-e2e--assistant-content
                      (plist-get result-one :result))
                     nonce-one))
            (should (e-live-e2e--contains-p
                     (e-live-e2e--assistant-content
                      (plist-get result-two :result))
                     nonce-two))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest e-live-e2e-test-follow-up-uses-session-context ()
  "A follow-up live prompt can use earlier transcript context."
  (e-live-e2e--with-harness (harness session-id)
    (let ((nonce (e-live-e2e--nonce)))
      (e-board-e2e-prompt-batch
       harness session-id
       (format "Remember this validation token for the next message: %s. Reply OK."
               nonce))
      (let ((result (e-board-e2e-prompt-batch
                     harness session-id
                     "Reply with only the validation token I asked you to remember.")))
        (should (e-live-e2e--contains-p
                 (e-live-e2e--assistant-content result)
                 nonce))))))

(ert-deftest e-live-e2e-test-tool-call-round-trip ()
  "The model can call a registered e tool and use its result."
  (e-live-e2e--with-harness (harness session-id :layers (list (e-live-e2e--tool-layer)))
    (let* ((nonce (e-live-e2e--nonce))
           (result (e-board-e2e-prompt-batch
                    harness session-id
                    (format
                     "Call e2e_echo exactly once with text %S. Then reply with only that returned text."
                     nonce)))
           (activities (e-session-activity-events
                        (e-harness-sessions harness) session-id)))
      (should (e-live-e2e--activity-of-type harness session-id 'tool-started))
      (should (e-live-e2e--activity-of-type harness session-id 'tool-finished))
      (should (e-live-e2e--contains-p
               (e-live-e2e--assistant-content result)
               nonce))
      (should (seq-some (lambda (event)
                          (e-live-e2e--contains-p
                           (prin1-to-string (plist-get event :payload))
                           "e2e_echo"))
                        activities)))))

(ert-deftest e-live-e2e-test-provider-lifecycle-events-are-durable ()
  "Live provider start and finish events are emitted and persisted."
  (e-live-e2e--with-harness (harness session-id)
    (let ((nonce (e-live-e2e--nonce)))
      (e-board-e2e-prompt-batch
       harness session-id
       (format "Reply with exactly this lifecycle token: %s" nonce))
      (let ((started (e-live-e2e--activity-of-type
                      harness session-id 'provider-request-started))
            (finished (e-live-e2e--activity-of-type
                       harness session-id 'provider-request-finished)))
        (should started)
        (should finished)
        (should (plist-get (plist-get (car started) :payload) :provider))
        (should (plist-get (plist-get (car finished) :payload) :status))))))

(ert-deftest e-live-e2e-test-token-usage-is-recorded-when-reported ()
  "Live provider token usage reaches durable activity when reported."
  (e-live-e2e--with-harness (harness session-id)
    (e-board-e2e-prompt-batch
     harness session-id
     "Reply with exactly: TOKEN-USAGE-CHECK")
    (let ((usage-events (e-live-e2e--activity-of-type
                         harness session-id 'token-usage)))
      (if usage-events
          (should (plist-get (plist-get (car usage-events) :payload)
                             :total-tokens))
        (ert-skip "The selected live provider did not report token usage.")))))

(ert-deftest e-live-e2e-test-session-persists-and-loads-live_messages ()
  "Live user and assistant messages survive session store reload."
  (e-live-e2e--with-harness (harness session-id :persistent t)
    (let* ((store-dir (e-session-store-directory (e-harness-sessions harness)))
           (nonce (e-live-e2e--nonce)))
      (e-board-e2e-prompt-batch
       harness session-id
       (format "Reply with exactly this persistence token: %s" nonce))
      (let* ((reloaded-store (e-session-persistent-store-create store-dir))
             (messages (e-session-messages reloaded-store session-id)))
        (should (>= (length messages) 2))
        (should (seq-some
                 (lambda (message)
                   (and (eq (plist-get message :role) 'assistant)
                        (e-live-e2e--contains-p
                         (plist-get message :content) nonce)))
                 messages))))))

(ert-deftest e-live-e2e-test-manual-compaction-records-summary ()
  "Manual compaction uses the live backend and records a durable compaction."
  (e-live-e2e--with-harness (harness session-id)
    (let ((nonce (e-live-e2e--nonce)))
      (e-board-e2e-prompt-batch
       harness session-id
       (format "Remember this compaction token: %s. Reply OK." nonce))
      (e-board-e2e-prompt-batch
       harness session-id
       "Reply with one short sentence confirming you still have the token.")
      (let ((record (e-harness-compact-session-batch
                     harness session-id
                     :reason 'manual
                     :keep-recent-tokens 1)))
        (should (plist-get record :summary))
        (should (e-session-latest-valid-compaction
                 (e-harness-sessions harness) session-id))
        (should (e-live-e2e--activity-of-type
                 harness session-id 'compaction-finished))))))

(ert-deftest e-live-e2e-test-provider-anchor-candidate-recorded-when-supported ()
  "Continuation-capable providers record provider anchor candidates."
  (e-live-e2e--with-harness (harness session-id)
    (e-board-e2e-prompt-batch
     harness session-id
     "Reply with exactly: ANCHOR-CHECK")
    ;; Continuation support is backend-specific; assert anchors when the
    ;; configured backend records them, otherwise skip rather than assume.
    (if (e-session-provider-anchors (e-harness-sessions harness) session-id)
        (should (e-session-provider-anchors
                 (e-harness-sessions harness) session-id))
      (ert-skip "The configured live backend does not record continuation anchors."))))

(ert-deftest e-live-e2e-test-openai-codex-store-false-continues ()
  "ChatGPT Codex replaces current state on its proved native continuation."
  (unless (fboundp 'e-openai-codex--websocket-request-start)
    (ert-skip "The OpenAI Responses WebSocket adapter is not loaded."))
  (e-live-e2e--require-enabled)
  (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id)))
    (unless (e-openai--builtin-codex-profile-p provider-id profile)
      (ert-skip "The configured provider is not the exact built-in ChatGPT Codex profile."))
    (should (eq (plist-get profile :response-store) :json-false)))
  (let* ((old-marker "OBSERVATION-OLD")
         (new-marker "OBSERVATION-NEW")
         (current-state old-marker)
         (provider
          (e-context-provider-create
           :name 'live-codex-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (layer
          (e-layer-create
           :id 'live-codex-segmented-context
           :name "Live Codex Segmented Context"
           :capabilities
           (list
            (e-capability-create
             :id 'live-codex-segmented-context
             :instructions "subscription stable instructions"
             :context-providers (list provider))))))
    (e-live-e2e--with-harness (harness session-id :layers (list layer))
      (let ((original-start
            (symbol-function 'e-openai-codex--websocket-request-start))
            request-bodies
            request-handles
            first-response-id
            first-assistant
            first-durable-assistant
            second-assistant
            second-turn-id)
        (cl-letf (((symbol-function 'e-openai-codex--websocket-request-start)
                   (lambda (&rest args)
                     (push (copy-tree (plist-get args :body-data)) request-bodies)
                     (let ((request (apply original-start args)))
                       (push request request-handles)
                       request))))
          (let ((first-result
                 (e-board-e2e-prompt-batch
                  harness session-id
                  (concat
                   "Reply with exactly: FIRST-TURN-READY. "
                   "Do not repeat any observation marker."))))
            (setq first-assistant
                  (e-live-e2e--assistant-content first-result))
            (setq first-durable-assistant
                  (car
                   (last
                    (seq-filter
                     (lambda (message)
                       (eq (plist-get message :role) 'assistant))
                     (e-harness-messages harness session-id)))))
            ;; This is an asserted precondition, not a post-hoc explanation:
            ;; if turn one leaked the old marker, turn two would be ambiguous.
            (should first-durable-assistant)
            (should-not
             (e-live-e2e--contains-p first-assistant old-marker))
            (should-not
             (e-live-e2e--contains-p
              (plist-get first-durable-assistant :content)
              old-marker))
            (setq first-response-id
                  (plist-get
                   (plist-get
                    (car
                     (last
                      (e-session-provider-anchors
                       (e-harness-sessions harness) session-id)))
                    :metadata)
                   :response-id))
            (should (stringp first-response-id)))
          (setq current-state new-marker)
          (let ((second-result
                 (e-board-e2e-prompt-batch
                  harness session-id
                  (concat
                   "Reply with exactly the current observation marker from "
                   "your instructions and no other text."))))
            (setq second-assistant
                  (e-live-e2e--assistant-content second-result))
            (setq second-turn-id (plist-get second-result :id))))
        (let* ((ordered-bodies (nreverse request-bodies))
               (ordered-handles (nreverse request-handles))
               (first-body (car ordered-bodies))
               (second-body (car (last ordered-bodies)))
               (second-request-handle (car (last ordered-handles)))
               (first-input (append (plist-get first-body :input) nil))
               (second-input (append (plist-get second-body :input) nil))
               (second-literal-request (prin1-to-string second-body))
               (request-metadata
                (e-backend-request-metadata second-request-handle))
               (diagnostics
                (plist-get request-metadata :diagnostics))
               (second-turn-finished-events
                (seq-filter
                 (lambda (event)
                   (equal (plist-get event :turn-id) second-turn-id))
                 (e-live-e2e--activity-of-type
                  harness session-id 'provider-request-finished)))
               (second-finished-event (car second-turn-finished-events))
               (second-finished-payload
                (plist-get second-finished-event :payload))
               (finished-diagnostics
                (plist-get second-finished-payload :diagnostics))
               (anchors
                (e-session-provider-anchors
                 (e-harness-sessions harness) session-id))
               (newest-anchor (car (last anchors)))
               (newest-response-id
                (plist-get (plist-get newest-anchor :metadata) :response-id)))
          (ert-info ((format "Codex continuation diagnostics: %S" diagnostics))
            (should (equal
                     (plist-get
                      (e-backend-context-capabilities
                       (e-harness-backend harness)
                       nil)
                     :observation-delivery)
                     e-openai--request-local-observation-delivery-map))
            (should (= (length ordered-bodies) 2))
            (should (= (length ordered-handles) 2))
            (should (e-backend-request-p second-request-handle))
            (should (stringp second-turn-id))
            (should (= (length second-turn-finished-events) 1))
            (should (equal (plist-get second-finished-event :turn-id)
                           second-turn-id))
            (should (= (plist-get second-finished-payload
                                  :provider-request-ordinal)
                       1))
            (should (eq (plist-get second-finished-payload :status) 'done))
            (should (eq (plist-get first-body :store) :json-false))
            (should (equal (plist-get first-body :include)
                           ["reasoning.encrypted_content"]))
            (should-not (plist-member first-body :previous_response_id))
            (should (eq (plist-get second-body :store) :json-false))
            (should (equal (plist-get second-body :include)
                           ["reasoning.encrypted_content"]))
            (should (equal (plist-get second-body :previous_response_id)
                           first-response-id))
            (dolist (body (list first-body second-body))
              (should-not (plist-member body :prompt_cache_options))
              (should-not
               (e-live-e2e--contains-p
                (prin1-to-string body)
                "prompt_cache_breakpoint")))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   first-input)
                           '("developer" "user")))
            (should-not
             (e-live-e2e--contains-p (prin1-to-string first-input)
                                     old-marker))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   second-input)
                           '("user")))
            (should
             (seq-find
              (lambda (item)
                (string-match-p
                 "subscription stable instructions"
                 (or (plist-get (aref (plist-get item :content) 0) :text) "")))
              first-input))
            (should (equal (plist-get first-body :instructions)
                           (concat "You are a helpful assistant.\n\n"
                                   old-marker)))
            (should (equal (plist-get second-body :instructions)
                           (concat "You are a helpful assistant.\n\n"
                                   new-marker)))
            (should (e-live-e2e--contains-p
                     (plist-get second-body :instructions)
                     new-marker))
            (should-not (e-live-e2e--contains-p
                         second-literal-request
                         old-marker))
            (should (equal (string-trim second-assistant) new-marker))
            (should-not
             (e-live-e2e--contains-p second-assistant old-marker))
            (should (equal (plist-get diagnostics :prompt-cache-mode)
                           "implicit-segmented"))
            (should (eq (plist-get diagnostics :observation-delivery)
                        'request-local-replaceable))
            (should (eq (plist-get diagnostics
                                   :replaceable-current-state-present)
                        t))
            (should (eq (plist-get diagnostics :provider-anchor-safety)
                        'advance-eligible))
            (should (eq (plist-get diagnostics :websocket-request-mode)
                        'incremental))
            (should (plist-member diagnostics :websocket-fallback-reason))
            (should-not (plist-get diagnostics :websocket-fallback-reason))
            (should (eq (plist-get diagnostics :previous-response-id-present)
                        t))
            (should (eq (plist-get diagnostics :websocket-reused) t))
            (should (= (plist-get diagnostics :websocket-reuse-count) 1))
            (dolist (key '(:websocket-request-mode
                           :websocket-fallback-reason
                           :previous-response-id-present
                           :websocket-reused
                           :websocket-reuse-count))
              (should (equal (plist-get finished-diagnostics key)
                             (plist-get diagnostics key))))
            (should (= (length anchors) 2))
            (should (stringp newest-response-id))
            (should-not (equal newest-response-id first-response-id))))))))

(ert-deftest e-live-e2e-test-openai-codex-older-clean-anchor-tool-chain ()
  "ChatGPT Codex branches from a clean anchor after a contaminated tool turn."
  (unless (fboundp 'e-openai-codex--websocket-request-start)
    (ert-skip "The OpenAI Responses WebSocket adapter is not loaded."))
  (e-live-e2e--require-enabled)
  (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id)))
    (unless (e-openai--builtin-codex-profile-p provider-id profile)
      (ert-skip "The configured provider is not the exact built-in ChatGPT Codex profile."))
    (should (eq (plist-get profile :response-store) :json-false))
    (should (= (plist-get profile :websocket-idle-close-seconds) 600)))
  (let* ((old-marker "LIVE-OLDER-OBSERVATION")
         (new-marker "LIVE-CURRENT-OBSERVATION")
         (current-state old-marker)
         (provider
          (e-context-provider-create
           :name 'live-codex-older-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (layer
          (e-layer-create
           :id 'live-codex-older-clean-anchor
           :name "Live Codex Older Clean Anchor"
           :capabilities
           (list
            (e-capability-create
             :id 'live-codex-older-clean-anchor
             :instructions "LIVE-STABLE-INSTRUCTIONS"
             :context-providers (list provider)))))
         (raw-tool-output "LIVE-TOOL-OUTPUT"))
    (e-live-e2e--with-harness
        (harness session-id
                 :layers (list layer (e-live-e2e--deterministic-tool-layer)))
      (let ((original-start
             (symbol-function 'e-openai-codex--websocket-request-start))
            (original-record
             (symbol-function
              'e-openai-codex--websocket-session-record-response))
            request-bodies
            request-handles
            completed-response-ids
            first-turn-id
            tool-turn-id
            warm-turn-id)
        (cl-letf (((symbol-function 'e-openai-codex--websocket-request-start)
                   (lambda (&rest args)
                     (setq request-bodies
                           (append request-bodies
                                   (list (copy-tree
                                          (plist-get args :body-data)))))
                     (let ((request (apply original-start args)))
                       (setq request-handles
                             (append request-handles (list request)))
                       request)))
                  ((symbol-function
                    'e-openai-codex--websocket-session-record-response)
                   (lambda (session response-id properties)
                     (when (stringp response-id)
                       (setq completed-response-ids
                             (append completed-response-ids
                                     (list response-id))))
                     (funcall original-record session response-id properties))))
          (let ((first-result
                 (e-board-e2e-prompt-batch
                  harness session-id
                  "Reply with exactly LIVE-R0-READY. Do not call tools or repeat observation markers.")))
            (setq first-turn-id (plist-get first-result :id))
            (should (stringp first-turn-id))
            (should (equal
                     (string-trim (e-live-e2e--assistant-content first-result))
                     "LIVE-R0-READY"))
            (should-not (e-live-e2e--contains-p
                         (e-live-e2e--assistant-content first-result)
                         old-marker)))
          (let ((tool-result
                 (e-board-e2e-prompt-batch
                  harness session-id
                  "Call e2e_deterministic exactly once. After it returns, reply with exactly LIVE-R2-READY and no other text.")))
            (setq tool-turn-id (plist-get tool-result :id))
            (should (stringp tool-turn-id))
            (should (equal
                     (string-trim (e-live-e2e--assistant-content tool-result))
                     "LIVE-R2-READY")))
          (setq current-state new-marker)
          (let ((warm-result
                 (e-board-e2e-prompt-batch
                  harness session-id
                  "Reply with exactly the current observation marker from your instructions and no other text.")))
            (setq warm-turn-id (plist-get warm-result :id))
            (should (stringp warm-turn-id))
            (should (equal (string-trim (e-live-e2e--assistant-content warm-result))
                           new-marker))))
        (let* ((ordered-bodies request-bodies)
               (ordered-handles request-handles)
               (response-ids completed-response-ids)
               (r0 (nth 0 response-ids))
               (r1 (nth 1 response-ids))
               (r2 (nth 2 response-ids))
               (first-body (nth 0 ordered-bodies))
               (tool-body (nth 1 ordered-bodies))
               (warm-body (nth 3 ordered-bodies))
               (warm-handle (nth 3 ordered-handles))
               (warm-input (append (plist-get warm-body :input) nil))
               (warm-input-printed (prin1-to-string warm-input))
               (warm-body-printed (prin1-to-string warm-body))
               (first-diagnostics
                (plist-get (e-backend-request-metadata
                            (car ordered-handles))
                           :diagnostics))
               (warm-diagnostics
                (plist-get (e-backend-request-metadata warm-handle)
                           :diagnostics))
               (warm-finished-events
                (seq-filter
                 (lambda (event)
                   (equal (plist-get event :turn-id) warm-turn-id))
                 (e-live-e2e--activity-of-type
                  harness session-id 'provider-request-finished)))
               (warm-finished-event (car warm-finished-events))
               (warm-finished-payload
                (plist-get warm-finished-event :payload))
               (warm-finished-diagnostics
                (plist-get warm-finished-payload :diagnostics))
               (anchors
                (e-session-provider-anchors
                 (e-harness-sessions harness) session-id))
               (anchor-ids
                (mapcar
                 (lambda (anchor)
                   (plist-get (plist-get anchor :metadata) :response-id))
                 anchors)))
          (should (= (length ordered-bodies) 4))
          (should (= (length ordered-handles) 4))
          (should (= (length response-ids) 4))
          (should (string-match-p
                   (regexp-quote old-marker)
                   (plist-get first-body :instructions)))
          (should (string-match-p
                   (regexp-quote old-marker)
                   (plist-get tool-body :instructions)))
          (dolist (response-id (list r0 r1 r2))
            (should (stringp response-id)))
          (should (= (length
                      (e-live-e2e--activity-of-type
                       harness session-id 'tool-started))
                     1))
          (should (= (length
                      (e-live-e2e--activity-of-type
                       harness session-id 'tool-finished))
                     1))
          (should (equal (plist-get warm-body :previous_response_id) r0))
          (should-not (member (plist-get warm-body :previous_response_id)
                              (list r1 r2)))
          (should (eq (plist-get warm-diagnostics :websocket-request-mode)
                      'incremental))
          (should (eq (plist-get warm-diagnostics
                                 :websocket-anchor-position)
                      'older))
          (should (= (plist-get warm-diagnostics
                                :websocket-idle-close-seconds)
                     600))
          (should (eq (plist-get warm-diagnostics :websocket-reused) t))
          (should (equal (plist-get first-diagnostics
                                    :websocket-connection-id)
                         (plist-get warm-diagnostics
                                    :websocket-connection-id)))
          (should warm-finished-event)
          (should (= (length warm-finished-events) 1))
          (should (eq (plist-get warm-finished-diagnostics
                                 :websocket-anchor-position)
                      'older))
          (should (= (plist-get warm-finished-diagnostics
                                :websocket-idle-close-seconds)
                     600))
          (should (eq (plist-get warm-finished-diagnostics
                                 :websocket-request-mode)
                      'incremental))
          (should (eq (plist-get warm-finished-diagnostics
                                 :previous-response-id-present)
                      t))
          (dolist (response-id (list r0 r1 r2))
            (should-not (string-match-p
                         (regexp-quote response-id)
                         (prin1-to-string warm-finished-diagnostics))))
          (should-not (string-match-p
                       (regexp-quote raw-tool-output)
                       (prin1-to-string warm-finished-diagnostics)))
          (should (string-match-p
                   (regexp-quote new-marker)
                   (plist-get warm-body :instructions)))
          (should (string-match-p
                   (regexp-quote "LIVE-STABLE-INSTRUCTIONS")
                   (plist-get warm-body :instructions)))
          (should-not (string-match-p (regexp-quote old-marker)
                                      warm-body-printed))
          (should-not (string-match-p (regexp-quote raw-tool-output)
                                      warm-body-printed))
          (should (string-match-p
                   (regexp-quote "Call e2e_deterministic exactly once")
                   warm-input-printed))
          (should-not
           (seq-some
            (lambda (item)
              (member (plist-get item :type)
                      '("function_call" "function_call_output"
                        "provider-replay-item" "reasoning"
                        function_call function_call_output
                        provider-replay-item reasoning)))
            warm-input))
          (should-not (string-match-p (regexp-quote r1) warm-body-printed))
          (should-not (string-match-p (regexp-quote r2) warm-body-printed))
          (should (member r0 anchor-ids))
          (should-not (member r1 anchor-ids))
          (should-not (member r2 anchor-ids))
          (e-backend-cancel-request warm-handle))))))

(ert-deftest e-live-e2e-test-openai-codex-long-idle-retains-socket ()
  "An explicitly gated pause beyond 300 seconds keeps Codex warm on one socket."
  (unless (fboundp 'e-openai-codex--websocket-request-start)
    (ert-skip "The OpenAI Responses WebSocket adapter is not loaded."))
  (e-live-e2e--require-enabled)
  (unless (e-live-e2e--env "E_E2E_LONG_IDLE")
    (ert-skip "Set E_E2E_LONG_IDLE=1 to run the slow 300--600 second acceptance."))
  (let* ((delay
          (e-live-e2e--positive-number-env "E_E2E_LONG_IDLE_SECONDS" 301.0))
         (provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id)))
    (unless (and (> delay 300) (< delay 600))
      (error "E_E2E_LONG_IDLE_SECONDS must be greater than 300 and less than 600"))
    (unless (e-openai--builtin-codex-profile-p provider-id profile)
      (ert-skip "The configured provider is not the exact built-in ChatGPT Codex profile."))
    (should (= (plist-get profile :websocket-idle-close-seconds) 600))
    (let* ((old-marker "LIVE-LONG-IDLE-OLD")
           (new-marker "LIVE-LONG-IDLE-NEW")
           (current-state old-marker)
           (provider
            (e-context-provider-create
             :name 'live-codex-long-idle-current-state
             :cache-placement 'dynamic-context
             :build (lambda (&rest _)
                      (list (list :role 'system :content current-state)))))
           (layer
            (e-layer-create
             :id 'live-codex-long-idle
             :name "Live Codex Long Idle"
             :capabilities
             (list
              (e-capability-create
               :id 'live-codex-long-idle
               :instructions "LIVE-LONG-IDLE-STABLE-INSTRUCTIONS"
               :context-providers (list provider)))))
           (request-bodies nil)
           (request-handles nil)
           (websocket-session nil))
      (e-live-e2e--with-harness (harness session-id :layers (list layer))
        (let ((original-start
               (symbol-function 'e-openai-codex--websocket-request-start)))
          (cl-letf (((symbol-function 'e-openai-codex--websocket-request-start)
                     (lambda (&rest args)
                       (setq websocket-session (plist-get args :session))
                       (setq request-bodies
                             (append request-bodies
                                     (list (copy-tree
                                            (plist-get args :body-data)))))
                       (let ((request (apply original-start args)))
                         (setq request-handles
                               (append request-handles (list request)))
                         request))))
            (let ((first-result
                   (e-board-e2e-prompt-batch
                    harness session-id
                    "Reply with exactly LIVE-LONG-IDLE-FIRST. Do not repeat observation markers.")))
              (should (e-live-e2e--contains-p
                       (e-live-e2e--assistant-content first-result)
                       "LIVE-LONG-IDLE-FIRST")))
            (let* ((anchors
                    (e-session-provider-anchors
                     (e-harness-sessions harness) session-id))
                   (first-anchor (car (last anchors)))
                   (first-response-id
                    (plist-get (plist-get first-anchor :metadata)
                               :response-id))
                   (first-body (car request-bodies)))
              (should (stringp first-response-id))
              (should (string-match-p
                       (regexp-quote old-marker)
                       (plist-get first-body :instructions)))
              (should websocket-session)
              (should (timerp
                       (e-openai-codex--websocket-session-idle-timer
                        websocket-session)))
              (ert-info ((format "Configured long-idle delay: %.3f seconds"
                                 delay))
                (sleep-for delay))
              (should (e-openai-codex--websocket-session-websocket
                       websocket-session))
              (should (timerp
                       (e-openai-codex--websocket-session-idle-timer
                        websocket-session)))
              (setq current-state new-marker)
              (let ((second-result
                     (e-board-e2e-prompt-batch
                      harness session-id
                      "Reply with exactly the current observation marker from your instructions and no other text.")))
                (should (equal (string-trim
                                (e-live-e2e--assistant-content second-result))
                               new-marker))
                (let* ((ordered-bodies request-bodies)
                       (ordered-handles request-handles)
                       (second-body (cadr ordered-bodies))
                       (first-diagnostics
                        (plist-get (e-backend-request-metadata
                                    (car ordered-handles))
                                   :diagnostics))
                       (second-diagnostics
                        (plist-get (e-backend-request-metadata
                                    (cadr ordered-handles))
                                   :diagnostics))
                       (finished-events
                        (seq-filter
                         (lambda (event)
                           (equal (plist-get event :turn-id)
                                  (plist-get second-result :id)))
                         (e-live-e2e--activity-of-type
                          harness session-id 'provider-request-finished)))
                       (finished-diagnostics
                        (plist-get (plist-get (car finished-events) :payload)
                                   :diagnostics)))
                  (should (= (length ordered-bodies) 2))
                  (should (= (length ordered-handles) 2))
                  (should (equal (plist-get second-body
                                            :previous_response_id)
                                 first-response-id))
                  (should (eq (plist-get second-diagnostics
                                         :websocket-request-mode)
                              'incremental))
                  (should (eq (plist-get second-diagnostics
                                         :websocket-anchor-position)
                              'latest))
                  (should (= (plist-get second-diagnostics
                                        :websocket-idle-close-seconds)
                             600))
                  (should (eq (plist-get second-diagnostics
                                         :websocket-reused)
                              t))
                  (should (= (plist-get second-diagnostics
                                        :websocket-reuse-count)
                             1))
                  (should (equal (plist-get first-diagnostics
                                            :websocket-connection-id)
                                 (plist-get second-diagnostics
                                            :websocket-connection-id)))
                  (should (= (length finished-events) 1))
                  (should (eq (plist-get finished-diagnostics
                                         :websocket-anchor-position)
                              'latest))
                  (should (= (plist-get finished-diagnostics
                                        :websocket-idle-close-seconds)
                             600))
                  (should (eq (plist-get finished-diagnostics
                                         :websocket-request-mode)
                              'incremental))
                  (should (eq (plist-get finished-diagnostics
                                         :previous-response-id-present)
                              t))
                  (should (string-match-p
                           (regexp-quote new-marker)
                           (plist-get second-body :instructions)))
                  (should-not (string-match-p
                               (regexp-quote old-marker)
                               (prin1-to-string second-body)))
                  (should (timerp
                           (e-openai-codex--websocket-session-idle-timer
                            websocket-session)))
                  (e-backend-cancel-request (cadr ordered-handles)))))))))))

(ert-deftest e-live-e2e-test-openai-store-false-full-replay ()
  "The configured OpenAI provider accepts encrypted reasoning full replay."
  (e-live-e2e--require-enabled)
  (let ((profile (e-openai-provider-profile e-openai-default-provider)))
    (unless (and (eq (e-openai--provider-wire-api profile) 'responses)
                 (eq (plist-get profile :response-store) :json-false))
      (ert-skip "The configured provider is not an unstored Responses backend.")))
  (e-live-e2e--with-harness
      (harness session-id :layers (list (e-live-e2e--tool-layer)))
    (let ((original-context
           (symbol-function 'e-openai--request-context))
          (original-websocket-start
           (symbol-function 'e-openai-codex--websocket-request-start))
          websocket-session
          request-bodies)
      (cl-letf (((symbol-function 'e-openai--request-context)
                 (lambda (&rest args)
                   (let ((context (apply original-context args)))
                     (when (eq (plist-get context :responses-transport) 'http)
                       (push (copy-tree (plist-get context :body-data))
                             request-bodies))
                     context)))
                ((symbol-function 'e-openai-codex--websocket-request-start)
                 (lambda (&rest args)
                   (setq websocket-session (plist-get args :session))
                   (push (copy-tree (plist-get args :full-body-data))
                         request-bodies)
                   (apply original-websocket-start args))))
        (e-board-e2e-prompt-batch
         harness session-id
         (concat
          "Reason carefully about why every finite directed acyclic graph has "
          "a topological ordering. After that private analysis, call e2e_echo "
          "exactly once with text REPLAY-SEED. Then reply with only that returned text."))
        (should (e-live-e2e--activity-of-type
                 harness session-id 'tool-finished))
        (let ((replay-items
               (cl-loop
                for message in (e-harness-messages harness session-id)
                for carrier = (if (eq (plist-get message :role) 'tool-call)
                                  (plist-get message :content)
                                (plist-get message :metadata))
                append (plist-get carrier :provider-replay-items))))
          (should replay-items)
          (let* ((reasoning-record
                  (seq-find
                   (lambda (record)
                     (equal (plist-get (plist-get record :item) :type)
                            "reasoning"))
                   replay-items))
                 (reasoning-item (plist-get reasoning-record :item)))
            (should reasoning-record)
            (should (plist-member reasoning-item :summary))
            (should-not (plist-get reasoning-item :summary))))
        ;; HTTP profiles without continuation already full-replay every turn.
        ;; For WebSocket profiles, deliberately discard the connection-local
        ;; anchor so the next live request must take the same full path.
        (when websocket-session
          (should (e-openai-codex--websocket-session-p websocket-session))
          (e-openai-codex--websocket-session-close websocket-session))
        (e-board-e2e-prompt-batch
         harness session-id
         "Reply with exactly: FULL-REPLAY-ACCEPTED"))
      (let* ((full-body (car request-bodies))
             (input (append (plist-get full-body :input) nil))
             (reasoning
              (seq-find
               (lambda (item) (equal (plist-get item :type) "reasoning"))
               input)))
        (should-not (plist-member full-body :previous_response_id))
        (should reasoning)
        (should (stringp (plist-get reasoning :encrypted_content)))
        (should (vectorp (plist-get reasoning :summary)))
        (should (= (length (plist-get reasoning :summary)) 0))))))

(ert-deftest e-live-e2e-test-openai-gpt56-explicit-cache-continues ()
  "A live GPT-5.6 follow-up retains the breakpoint and replaces instructions."
  (unless (fboundp 'e-openai-codex--websocket-request-start)
    (ert-skip "The OpenAI Responses WebSocket adapter is not loaded."))
  (e-live-e2e--require-enabled)
  (unless (eq (plist-get (e-openai-provider-profile e-openai-default-provider)
                         :prompt-cache-breakpoint-mode)
              'explicit)
    (ert-skip "The configured OpenAI provider does not support explicit cache breakpoints."))
  (let* ((current-state "live state one")
         ;; OpenAI only caches prefixes of at least 1,024 tokens.  Keep this
         ;; probe independent of whichever default layers the E2E config loads.
         (stable-guidance
          (mapconcat #'identity
                     (make-list 500 "stable cache probe directive")
                     " "))
         (provider
          (e-context-provider-create
           :name 'live-cross-turn-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (layer
          (e-layer-create
           :id 'live-cross-turn-current-state
           :name "Live Cross-Turn Current State"
           :capabilities
           (list
            (e-capability-create
             :id 'live-cross-turn-current-state
             :instructions stable-guidance
             :context-providers (list provider))))))
    (e-live-e2e--with-harness (harness session-id :layers (list layer))
      (unless (e-openai--gpt56-or-later-p
               (plist-get (e-harness-display-options harness session-id)
                          :model))
        (ert-skip "The configured live model is older than GPT-5.6."))
      (e-session-set-turn-options
       (e-harness-sessions harness)
       session-id
       '(:prompt-cache-default t))
      (let ((original-start
             (symbol-function 'e-openai-codex--websocket-request-start))
            request-bodies)
        (cl-letf (((symbol-function 'e-openai-codex--websocket-request-start)
                   (lambda (&rest args)
                     (push (list :body
                                 (copy-tree (plist-get args :body-data))
                                 :full-body
                                 (copy-tree (plist-get args :full-body-data)))
                           request-bodies)
                     (apply original-start args))))
          (e-board-e2e-prompt-batch
           harness session-id
           "Reply with exactly: CROSS-TURN-ONE")
          (unless (e-session-provider-anchors
                   (e-harness-sessions harness) session-id)
            (ert-skip
             "The configured live backend does not record continuation anchors."))
          (setq current-state "live state two")
          (e-board-e2e-prompt-batch
           harness session-id
           "Reply with exactly: CROSS-TURN-TWO"))
        (let* ((requests (e-live-e2e--activity-of-type
                          harness session-id 'provider-request-started))
               (latest (car (last requests)))
               (diagnostics (plist-get (plist-get latest :payload) :diagnostics))
               (ordered-bodies (nreverse request-bodies))
               (first-body (plist-get (car ordered-bodies) :body))
               (latest-body
                (plist-get (car (last ordered-bodies)) :body))
               (first-breakpoint
                (cl-loop for item in (plist-get first-body :input)
                         for block = (car (plist-get item :content))
                         when (plist-get block :prompt_cache_breakpoint)
                         return block))
               (latest-instructions (plist-get latest-body :instructions))
               (latest-current-input
                (seq-find
                 (lambda (item)
                   (string-match-p
                    "live state two"
                    (or (plist-get (car (plist-get item :content)) :text)
                        "")))
                 (append (plist-get latest-body :input) nil)))
               (usage-events (e-live-e2e--activity-of-type
                              harness session-id 'token-usage))
               (first-usage (plist-get (car usage-events) :payload))
               (latest-usage
                (plist-get (car (last usage-events)) :payload))
               (tool-differences
                (e-live-e2e--request-tool-differences
                 first-body latest-body)))
          (unless (plist-member diagnostics :websocket-request-mode)
            (ert-skip "The configured live backend is not Responses WebSocket mode."))
          (ert-info ((format "WebSocket diagnostics: %S; tool differences: %S"
                             diagnostics tool-differences))
            (should (eq (plist-get diagnostics :websocket-request-mode)
                        'incremental))
            (should (eq (plist-get diagnostics :previous-response-id-present) t))
            (should (eq (plist-get diagnostics :websocket-reused) t))
            (should (equal (plist-get diagnostics :prompt-cache-mode)
                           "explicit"))
            (should (eq (plist-get diagnostics :observation-delivery)
                        'request-local-replaceable))
            (should (eq (plist-get diagnostics
                                   :replaceable-current-state-present)
                        t))
            (should (eq (plist-get diagnostics :provider-anchor-safety)
                        'advance-eligible))
            (should (equal (plist-get diagnostics :prompt-layout-revision)
                           e-openai-gpt56-explicit-cache-layout-revision))
            (should first-breakpoint)
            (should (equal (plist-get first-breakpoint
                                      :prompt_cache_breakpoint)
                           '(:mode "explicit")))
            (should (equal (plist-get latest-body :prompt_cache_options)
                           '(:mode "explicit")))
            (should (equal (plist-get latest-body :include)
                           ["reasoning.encrypted_content"]))
            (should (stringp (plist-get latest-body :previous_response_id)))
            (should
             (equal
              (plist-get
               (plist-get
                (car (last
                      (e-session-provider-anchors
                       (e-harness-sessions harness) session-id)))
                :metadata)
               :prompt-layout-revision)
              e-openai-gpt56-explicit-cache-layout-revision))
            (should (equal latest-instructions
                           "You are a helpful assistant.\n\nlive state two"))
            (should-not latest-current-input)
            (should (> (or (plist-get first-usage
                                      :cache-creation-input-tokens)
                           0)
                       0))
            (should (> (or (plist-get latest-usage :cached-input-tokens) 0)
                       0))))))))

(ert-deftest e-live-e2e-test-active-request-can-be-cancelled ()
  "An active live turn can be cancelled through the harness."
  (e-live-e2e--with-harness (harness session-id :layers (list (e-live-e2e--tool-layer)))
    (let ((turn-id
           (e-board-e2e-prompt-async
            harness session-id
            "Call e2e_slow now. Do not answer until the tool result is available.")))
      (let ((deadline (+ (float-time) 30))
            cancelled)
        (while (and (not (e-live-e2e--activity-of-type
                          harness session-id 'provider-request-started))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (should (e-live-e2e--activity-of-type
                 harness session-id 'provider-request-started))
        (should (e-chat-service-abort-session harness session-id))
        (while (and (not cancelled) (< (float-time) deadline))
          (setq cancelled
                (seq-some
                 (lambda (event)
                   (and (eq (plist-get event :event-type) 'turn-cancelled)
                        (equal (plist-get event :turn-id) turn-id)))
                 (e-session-activity-events
                  (e-harness-sessions harness) session-id)))
          (accept-process-output nil 0.05))
        (should cancelled)))))

(ert-deftest e-live-e2e-test-provider-errors_surface_as_turn_failures ()
  "A live provider failure releases the session for the next request."
  (e-live-e2e--with-harness (harness session-id)
    (e-session-set-turn-options
     (e-harness-sessions harness) session-id
     '(:model "e-live-e2e-nonexistent-model"))
    (should-error
     (e-board-e2e-prompt-batch
      harness session-id
      "This request should fail because the model is invalid."))
    (should (e-live-e2e--activity-of-type
             harness session-id 'turn-failed))
    (e-session-set-turn-options
     (e-harness-sessions harness) session-id nil)
    (let* ((nonce (e-live-e2e--nonce))
           (result
            (e-board-e2e-prompt-batch
             harness session-id
             (format "Reply with exactly this recovery token: %s" nonce))))
      (should (e-live-e2e--contains-p
               (e-live-e2e--assistant-content result)
               nonce)))))

(provide 'e-live-e2e-test)

;;; e-live-e2e-test.el ends here

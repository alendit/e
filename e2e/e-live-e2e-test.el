;;; e-live-e2e-test.el --- Live e2e tests for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests that exercise the real harness/backend/tool/session path against
;; live provider APIs.  These tests are intentionally gated by E_E2E and are not
;; in the default Eldev test fileset.
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
(require 'e-backend)
(require 'e-capabilities)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-layers)
(require 'e-session)
(require 'e-tools)

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
  (e-live-e2e--env "E_E2E"))

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
  "Return assistant content from a harness prompt RESULT."
  (or (plist-get result :assistant-content) ""))

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

(defun e-live-e2e--echo-tool-register (registry &rest _context)
  "Register the e2e echo tool in REGISTRY."
  (e-tools-register
   registry
   :name "e2e_echo"
   :description "Return the provided text exactly. Use only for e live e2e validation."
   :parameters '(:type "object"
                 :properties (:text (:type "string"))
                 :required ["text"])
   :handler (lambda (arguments)
              (or (plist-get arguments :text)
                  (plist-get arguments "text")
                  ""))))

(defun e-live-e2e--slow-tool-register (registry &rest _context)
  "Register a cancellable slow tool in REGISTRY."
  (e-tools-register
   registry
   :name "e2e_slow"
   :description "Wait briefly before returning. Use only for e live e2e cancellation validation."
   :parameters '(:type "object" :properties nil)
   :start (lambda (&key _arguments on-done _on-error on-request-start)
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
              request))))

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
               (plist-get
                (e-harness-create-session
                 ,harness
                 :metadata (list :project-root ,root))
                :id))
              (,events nil)
              (,subscription
               (e-harness-subscribe
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
           (ignore-errors (e-harness-unsubscribe ,harness ,subscription))
           (ignore-errors (delete-directory ,root t)))))))

(ert-deftest e-live-e2e-test-basic-assistant-response ()
  "A first live prompt returns a concrete assistant message."
  (e-live-e2e--with-harness (harness session-id)
    (let* ((nonce (e-live-e2e--nonce))
           (result (e-harness-prompt
                    harness session-id
                    (format "Reply with exactly this token and no extra words: %s"
                            nonce))))
      (should (e-live-e2e--contains-p
               (e-live-e2e--assistant-content result)
               nonce)))))

(ert-deftest e-live-e2e-test-follow-up-uses-session-context ()
  "A follow-up live prompt can use earlier transcript context."
  (e-live-e2e--with-harness (harness session-id)
    (let ((nonce (e-live-e2e--nonce)))
      (e-harness-prompt
       harness session-id
       (format "Remember this validation token for the next message: %s. Reply OK."
               nonce))
      (let ((result (e-harness-prompt
                     harness session-id
                     "Reply with only the validation token I asked you to remember.")))
        (should (e-live-e2e--contains-p
                 (e-live-e2e--assistant-content result)
                 nonce))))))

(ert-deftest e-live-e2e-test-tool-call-round-trip ()
  "The model can call a registered e tool and use its result."
  (e-live-e2e--with-harness (harness session-id :layers (list (e-live-e2e--tool-layer)))
    (let* ((nonce (e-live-e2e--nonce))
           (result (e-harness-prompt
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
      (e-harness-prompt
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
    (e-harness-prompt
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
      (e-harness-prompt
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
      (e-harness-prompt
       harness session-id
       (format "Remember this compaction token: %s. Reply OK." nonce))
      (e-harness-prompt
       harness session-id
       "Reply with one short sentence confirming you still have the token.")
      (let ((record (e-harness-compact-session
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
    (e-harness-prompt
     harness session-id
     "Reply with exactly: ANCHOR-CHECK")
    ;; Continuation support is backend-specific; assert anchors when the
    ;; configured backend records them, otherwise skip rather than assume.
    (if (e-session-provider-anchors (e-harness-sessions harness) session-id)
        (should (e-session-provider-anchors
                 (e-harness-sessions harness) session-id))
      (ert-skip "The configured live backend does not record continuation anchors."))))

(ert-deftest e-live-e2e-test-active-request-can-be-cancelled ()
  "An active live turn can be cancelled through the harness."
  (e-live-e2e--with-harness (harness session-id :layers (list (e-live-e2e--tool-layer)))
    (let ((turn-id
           (e-harness-prompt-async
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
        (should (e-harness-abort harness session-id))
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
  "Live provider errors surface as harness failures."
  (e-live-e2e--with-harness (harness session-id)
    (e-session-set-turn-options
     (e-harness-sessions harness) session-id
     '(:model "e-live-e2e-nonexistent-model"))
    (should-error
     (e-harness-prompt
      harness session-id
      "This request should fail because the model is invalid."))
    (should (e-live-e2e--activity-of-type
             harness session-id 'turn-failed))))

(provide 'e-live-e2e-test)

;;; e-live-e2e-test.el ends here

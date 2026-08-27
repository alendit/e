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
(require 'json)
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

(defconst e-live-e2e--cache-scenario-timeout-default 120.0
  "Default total wall-clock bound for an external cache scenario.

This is runner configuration, not a Feature 88 semantic or cache contract.")

(defconst e-live-e2e--external-evidence-schema-revision
  "e88-cache-evidence-v1"
  "Revision of the bounded machine-readable external evidence record.")

(defun e-live-e2e--profile-auth-available-p (profile)
  "Return non-nil when PROFILE's adapter auth source is available.
Codex-managed profiles use the auth file consumed by the adapter.  Other
profiles use their declared non-empty environment-variable key; the token is
never returned or recorded here."
  (if (plist-get profile :requires-openai-auth)
      (file-readable-p (e-openai-codex-auth-file))
    (let ((env-key (plist-get profile :env-key)))
      (and (stringp env-key)
           (not (string-empty-p env-key))
           (let ((token (getenv env-key)))
             (and (stringp token) (not (string-empty-p token))))))))

(defun e-live-e2e--responses-identity (profile)
  "Return the truthful Responses transport/requester pair for PROFILE."
  (if (eq (plist-get profile :responses-transport) 'websocket)
      '("responses-websocket" . "e-openai-codex--websocket-request-start")
    '("responses-http" . "e-openai-codex--http-request-start")))

(defun e-live-e2e--cache-scenario-timeout ()
  "Return the predeclared total bound for a cache scenario."
  (e-live-e2e--positive-number-env
   "E_E2E_CACHE_SCENARIO_TIMEOUT_SECONDS"
   e-live-e2e--cache-scenario-timeout-default))

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

(defun e-live-e2e--provider-metrics-record (finished-payload usage-payload)
  "Return bounded scalar metrics from FINISHED-PAYLOAD and USAGE-PAYLOAD.
The request latency comes from the loop-owned `provider-request-finished'
payload.  A missing cached-input field is represented explicitly as the
`unavailable' symbol; it is never inferred to be zero.  Semantic request
status is intentionally not part of this measurement record."
  (let ((diagnostics (plist-get finished-payload :diagnostics)))
    (list :provider-request-latency-seconds
          (plist-get finished-payload :elapsed-seconds)
          :input-tokens
          (plist-get usage-payload :input-tokens)
          :cached-input-tokens
          (if (plist-member usage-payload :cached-input-tokens)
              (plist-get usage-payload :cached-input-tokens)
            'unavailable)
          :connection-id
          (plist-get diagnostics :websocket-connection-id)
          :reuse-count
          (plist-get diagnostics :websocket-reuse-count))))

(defun e-live-e2e--report-provider-metrics (metrics)
  "Emit one bounded explicit success-path record for METRICS."
  (message
   "E2E provider metrics: provider-request-latency-seconds=%s input-tokens=%s cached-input-tokens=%s connection-id=%s reuse-count=%s"
   (plist-get metrics :provider-request-latency-seconds)
   (plist-get metrics :input-tokens)
   (plist-get metrics :cached-input-tokens)
   (plist-get metrics :connection-id)
   (plist-get metrics :reuse-count)))

(defun e-live-e2e--sha256 (value)
  "Return the SHA-256 digest of the UTF-8 representation of VALUE."
  (secure-hash 'sha256 (encode-coding-string (prin1-to-string value) 'utf-8)))

(defun e-live-e2e--json-value (value)
  "Return VALUE converted to JSON-compatible bounded data.
Keyword plists become JSON objects; ordinary lists remain JSON arrays."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((symbolp value) (symbol-name value))
   ((vectorp value)
    (vconcat (mapcar #'e-live-e2e--json-value (append value nil))))
   ((and (listp value)
         (or (null value) (keywordp (car value))))
    (e-live-e2e--json-plist value))
   ((and (consp value)
         (cl-every (lambda (item)
                     (and (listp item) (keywordp (car item))))
                   value))
    (mapcar #'e-live-e2e--json-value value))
   ((and (consp value)
         (consp (car value))
         (or (stringp (caar value))
             (symbolp (caar value))))
    (mapcar (lambda (pair)
              (cons (format "%s" (car pair))
                    (e-live-e2e--json-value (cdr pair))))
            value))
   ((consp value)
    (mapcar #'e-live-e2e--json-value value))
   (t value)))

(defun e-live-e2e--json-plist (plist)
  "Return keyword PLIST as a JSON object alist."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (push (cons (if (keywordp key)
                        (substring (symbol-name key) 1)
                      (format "%s" key))
                    (e-live-e2e--json-value value))
              result)))
    (nreverse result)))

(defun e-live-e2e--repository-revision ()
  "Return the current repository revision, or an explicit unavailable value."
  (with-temp-buffer
    (if (= (call-process "git" nil t nil "rev-parse" "HEAD") 0)
        (string-trim (buffer-string))
      "unavailable")))

(defun e-live-e2e--request-shape (body)
  "Return a bounded, content-free material shape for Responses BODY."
  (let* ((input (append (plist-get body :input) nil))
         (tools (append (plist-get body :tools) nil))
         (instructions (plist-get body :instructions)))
    (list :body-sha256 (e-live-e2e--sha256 body)
          :input-sha256 (e-live-e2e--sha256 input)
          :input-item-count (length input)
          :input-roles (mapcar (lambda (item) (plist-get item :role)) input)
          :input-types (mapcar (lambda (item) (plist-get item :type)) input)
          :instructions-utf8-bytes
          (and (stringp instructions)
               (string-bytes (encode-coding-string instructions 'utf-8)))
          :wire-keys
          (cl-loop for (key _value) on body by #'cddr
                   collect (if (keywordp key)
                               (substring (symbol-name key) 1)
                             (format "%s" key)))
          :tool-names (mapcar (lambda (tool) (plist-get tool :name)) tools)
          :tool-set-sha256 (and tools (e-live-e2e--sha256 tools))
          :store (format "%s" (plist-get body :store))
          :previous-response-id-present
          (and (plist-member body :previous_response_id) t)
          :prompt-cache-key-present
          (and (plist-member body :prompt_cache_key) t)
          :prompt-cache-options-present
          (and (plist-member body :prompt_cache_options) t)
          :prompt-cache-retention-present
          (and (plist-member body :prompt_cache_retention) t))))

(defun e-live-e2e--request-diagnostics (metadata)
  "Return diagnostics from request METADATA, or METADATA when already plain."
  (or (plist-get metadata :diagnostics) metadata))

(defun e-live-e2e--cache-result (usage-payloads)
  "Classify provider cache usage without treating missing data as a hit."
  (cond
   ((cl-some (lambda (payload)
               (and (plist-member payload :cached-input-tokens)
                    (numberp (plist-get payload :cached-input-tokens))
                    (= (plist-get payload :cached-input-tokens) 0)))
             usage-payloads)
    "product-contract-failure")
   ((and usage-payloads
         (cl-every (lambda (payload)
                     (and (plist-member payload :cached-input-tokens)
                          (numberp (plist-get payload :cached-input-tokens))
                          (> (plist-get payload :cached-input-tokens) 0)))
                   usage-payloads))
    "warm")
   (t "unavailable")))

(cl-defun e-live-e2e--external-evidence-record
    (&key scenario provider profile model request-bodies request-metadata
          usage-payloads timeout started-at ended-at semantic-result
          cache-result result)
  "Return one bounded identity-complete external evidence RECORD.
REQUEST-BODIES are hashed and reduced to shapes; no prompt, token, auth header,
or response body is emitted.  REQUEST-METADATA and USAGE-PAYLOADS are ordered
lists matching those requests where available.  Each body receives one shape
by request index; absent metadata leaves only its metadata-derived fields
unavailable."
  (let* ((request-bodies (or request-bodies nil))
         (request-metadata (or request-metadata nil))
         (diagnostics
          (mapcar #'e-live-e2e--request-diagnostics request-metadata))
         (responses-identity (e-live-e2e--responses-identity profile))
         (websocket-p
          (equal (cdr responses-identity)
                 "e-openai-codex--websocket-request-start"))
         (shapes
          (cl-loop for body in request-bodies
                   for index from 1
                   for metadata = (nth (1- index) diagnostics)
                   for shape = (e-live-e2e--request-shape body)
                   collect
                   (append shape
                           (list :request-index index
                                 :connection-id
                                 (and websocket-p
                                      (plist-get metadata
                                                 :websocket-connection-id))
                                 :websocket-reused
                                 (and websocket-p
                                      (plist-get metadata :websocket-reused)
                                      t)
                                 :websocket-reuse-count
                                 (and websocket-p
                                      (plist-get metadata
                                                 :websocket-reuse-count))
                                 :websocket-request-mode
                                 (and websocket-p
                                      (plist-get metadata
                                                 :websocket-request-mode))
                                 :prompt-layout-revision
                                 (plist-get metadata :prompt-layout-revision)))))
         (connection-ids
          (when websocket-p
            (delete-dups
             (delq nil (mapcar (lambda (metadata)
                                 (plist-get metadata :websocket-connection-id))
                               diagnostics)))))
         (input-token-values
          (delq nil (mapcar (lambda (payload)
                              (and (numberp (plist-get payload :input-tokens))
                                   (plist-get payload :input-tokens)))
                            usage-payloads)))
         (cached-token-values
          (and usage-payloads
               (cl-every (lambda (payload)
                           (and (plist-member payload :cached-input-tokens)
                                (numberp (plist-get payload :cached-input-tokens))))
                         usage-payloads)
               (mapcar (lambda (payload)
                         (plist-get payload :cached-input-tokens))
                       usage-payloads)))
         (timestamp
          (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                              (seconds-to-time (or started-at (float-time)))
                              t))
         (profile-name (or (plist-get profile :name)
                           (format "%s" provider)))
         (base-url
          (or (plist-get profile :base-url)
              (plist-get (car request-metadata) :url)
              "unavailable"))
         (prompt-layout-revision
          (or (seq-some (lambda (metadata)
                          (plist-get metadata :prompt-layout-revision))
                        diagnostics)
              "unavailable"))
         (elapsed (and started-at ended-at (- ended-at started-at))))
    (list :evidence-schema-revision e-live-e2e--external-evidence-schema-revision
          :scenario (format "%s" scenario)
          :provider-id (format "%s" provider)
          :profile-id profile-name
          :base-url-identity base-url
          :transport (car responses-identity)
          :store-mode (format "%s" (plist-get profile :response-store))
          :native-requester (cdr responses-identity)
          :model-id (or model (plist-get (car request-bodies) :model)
                        "unavailable")
          :prompt-layout-revision (format "%S" prompt-layout-revision)
          :prompt-cache-key-derivation-revision
          (format "e-harness-prompt-cache-key-%s"
                  e-harness-prompt-cache-key-version)
          :material-request-shape (or shapes [])
          :request-count (length request-bodies)
          :socket-connection-ids connection-ids
          :total-input-tokens
          (if input-token-values
              (apply #'+ input-token-values)
            "unavailable")
          :cached-input-tokens
          (if cached-token-values
              (apply #'+ cached-token-values)
            "unavailable")
          :scenario-timeout-seconds timeout
          :timestamp timestamp
          :repository-revision (e-live-e2e--repository-revision)
          :elapsed-seconds elapsed
          :semantic-result (or semantic-result "unavailable")
          :cache-result (or cache-result "unavailable")
          :result (or result "unavailable"))))

(defun e-live-e2e--report-external-evidence (record)
  "Emit one machine-readable bounded external evidence RECORD."
  (message "E88 external evidence: %s"
           (json-encode (e-live-e2e--json-plist record))))

(cl-defun e-live-e2e--run-external-scenario
    (&key scenario provider profile model timeout started-at capture thunk)
  "Run THUNK and emit exactly one bounded evidence record.
CAPTURE is called after THUNK settles and returns the best state captured so
far as a plist.  The original error or ERT skip is re-signalled after the
record is emitted; configuration, semantic, cache, and overall results stay
separate in that record."
  (let (condition value configuration-unavailable)
    (condition-case caught
        (setq value
              (progn
                (unless (e-live-e2e--profile-auth-available-p profile)
                  (setq configuration-unavailable t)
                  (ert-skip
                   (if (plist-get profile :requires-openai-auth)
                       "ChatGPT Codex auth.json is unavailable."
                     "Configured OpenAI token environment auth is unavailable.")))
                (funcall thunk)))
      (error (setq condition caught)))
    (let* ((state (or (ignore-errors (funcall capture)) nil))
           (condition-type (car condition))
           (cache-result (or (plist-get state :cache-result)
                             "unavailable"))
           (condition-message
            (and condition (ignore-errors (error-message-string condition))))
           (semantic-result
            (cond
             (configuration-unavailable "unavailable")
             ((and (eq condition-type 'ert-test-failed)
                   (equal cache-result "product-contract-failure"))
              (or (plist-get state :semantic-result) "pass"))
             ((eq condition-type 'ert-test-failed) "failure")
             (t (or (plist-get state :semantic-result) "unavailable"))))
           (result
            (cond
             (configuration-unavailable "configuration-unavailable")
             ((and condition-message
                   (string-match-p "inconclusive-timeout"
                                   condition-message))
              "inconclusive-timeout")
             ((and condition
                   (eq condition-type 'ert-test-failed)
                   (equal cache-result "product-contract-failure"))
              "product-contract-failure")
             ((and condition (eq condition-type 'ert-test-failed))
              "semantic-failure")
             ((and condition (eq condition-type 'ert-test-skipped))
              "unavailable")
             (condition "provider/infrastructure-failure")
             ((equal cache-result "warm") "pass")
             ((equal cache-result "product-contract-failure")
              "product-contract-failure")
             (t "unavailable"))))
      (ignore-errors
        (e-live-e2e--report-external-evidence
         (e-live-e2e--external-evidence-record
          :scenario scenario
          :provider (or provider e-openai-default-provider)
          :profile profile
          :model (or (plist-get state :model) model)
          :request-bodies (plist-get state :request-bodies)
          :request-metadata (plist-get state :request-metadata)
          :usage-payloads (plist-get state :usage-payloads)
          :timeout timeout
          :started-at started-at
          :ended-at (float-time)
          :semantic-result semantic-result
          :cache-result cache-result
          :result result))))
    (if condition
        (signal (car condition) (cdr condition))
      value)))

(defun e-live-e2e--prompt-batch-before-deadline
    (harness session-id prompt deadline)
  "Run PROMPT with the remaining portion of a scenario DEADLINE."
  (let ((remaining (- deadline (float-time))))
    (when (<= remaining 0)
      (error "inconclusive-timeout: external scenario deadline expired"))
    (e-board-e2e-prompt-batch harness session-id prompt remaining)))

(ert-deftest e-live-e2e-test-external-evidence-record-is-bounded-and-complete ()
  "The external record is machine-readable, identity-complete, and content-free."
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (push (apply #'format format-string arguments) messages))))
      (let ((record
             (e-live-e2e--external-evidence-record
              :scenario 'chatgpt-canonical-warm-prefix
              :provider 'codex
              :profile '(:name "ChatGPT Codex"
                         :base-url "https://chatgpt.example/codex"
                         :responses-transport websocket
                         :response-store :json-false)
              :model "gpt-5.6-sol"
              :request-bodies
              '((:model "gpt-5.6-sol" :store :json-false
                 :input [(:role "system" :content "secret")]
                 :prompt_cache_key "secret-key"))
              :request-metadata
              '((:url "https://chatgpt.example/codex"
                 :diagnostics (:websocket-connection-id "socket-1"
                               :websocket-reused nil
                               :websocket-reuse-count 0
                               :websocket-request-mode full
                               :prompt-layout-revision "layout-v1")))
              :usage-payloads '((:input-tokens 10 :cached-input-tokens 5))
              :timeout 120.0
              :started-at 100.0
              :ended-at 100.5
              :semantic-result "pass"
              :cache-result "warm"
              :result "pass")))
        (should (equal (plist-get record :scenario)
                       "chatgpt-canonical-warm-prefix"))
        (should (equal (plist-get record :provider-id) "codex"))
        (should (equal (plist-get record :native-requester)
                       "e-openai-codex--websocket-request-start"))
        (should (equal (plist-get record :prompt-cache-key-derivation-revision)
                       "e-harness-prompt-cache-key-pcctx2"))
        (should (equal (plist-get record :total-input-tokens) 10))
        (should (equal (plist-get record :cached-input-tokens) 5))
        (e-live-e2e--report-external-evidence record)
        (cl-letf (((symbol-function 'e-openai-codex-auth-file)
                   (lambda () "/private/tmp/e88-missing-auth.json")))
          (should-error
           (e-live-e2e--run-external-scenario
            :scenario 'chatgpt-canonical-warm-prefix
            :provider 'codex
            :profile '(:name "ChatGPT Codex"
                       :base-url "https://chatgpt.example/codex"
                       :responses-transport websocket
                       :requires-openai-auth t
                       :response-store :json-false)
            :model "gpt-5.6-sol"
            :timeout 120.0
            :started-at 100.0
            :capture (lambda () nil)
            :thunk (lambda () (ert-skip "auth unavailable")))
           :type 'ert-test-skipped))))
    (setq messages (nreverse messages))
    (should (= (length messages) 2))
    (let ((line (car messages))
          (missing-line (cadr messages)))
      (dolist (field '("evidence-schema-revision" "base-url-identity"
                       "transport" "store-mode" "material-request-shape"
                       "repository-revision" "scenario-timeout-seconds"
                       "semantic-result" "cache-result"))
        (should (string-match-p (regexp-quote field) line)))
      (should-not (string-match-p "secret-key" line))
      (should-not (string-match-p "secret" line))
      (dolist (field '("\"material-request-shape\":[]"
                       "\"semantic-result\":\"unavailable\""
                       "\"cache-result\":\"unavailable\""
                       "\"result\":\"configuration-unavailable\""))
        (should (string-match-p (regexp-quote field) missing-line))))))

(ert-deftest e-live-e2e-test-external-evidence-record-preserves-captured-body-on-provider-failure ()
  "A provider-start failure still records the captured request shape once."
  (let* ((body
          '(:model "gpt-5.6-sol"
            :store :json-false
            :input [(:type "message" :role "system"
                     :content "captured-provider-secret")]
            :tools [(:type "function" :name "e2e_echo"
                      :description "secret tool description")]))
         (records nil)
         (condition nil))
    (cl-letf (((symbol-function 'file-readable-p) (lambda (&rest _) t))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (record) (push record records))))
      (condition-case caught
          (e-live-e2e--run-external-scenario
           :scenario 'chatgpt-canonical-tool-heavy
           :provider 'codex
           :profile '(:name "ChatGPT Codex"
                      :base-url "https://chatgpt.example/codex"
                      :responses-transport websocket
                      :requires-openai-auth t
                      :response-store :json-false)
           :model "gpt-5.6-sol"
           :timeout 120.0
           :started-at 100.0
           :capture (lambda ()
                      (list :request-bodies (list body)
                            :request-metadata nil
                            :usage-payloads nil))
           :thunk (lambda () (error "websocket start failed")))
        (error (setq condition caught))))
    (should condition)
    (should (eq (car condition) 'error))
    (should (equal (cadr condition) "websocket start failed"))
    (should (= (length records) 1))
    (let* ((record (car records))
           (shapes (plist-get record :material-request-shape))
           (shape (car shapes))
           (encoded (json-encode (e-live-e2e--json-plist record))))
      (should (equal (plist-get record :result)
                     "provider/infrastructure-failure"))
      (should (= (plist-get record :request-count) 1))
      (should (= (length shapes) 1))
      (should (= (plist-get shape :request-index) 1))
      (should (stringp (plist-get shape :body-sha256)))
      (should (stringp (plist-get shape :input-sha256)))
      (should (= (plist-get shape :input-item-count) 1))
      (should (equal (plist-get shape :input-roles) '("system")))
      (should (equal (plist-get shape :input-types) '("message")))
      (should (equal (plist-get shape :wire-keys)
                     '("model" "store" "input" "tools")))
      (should (equal (plist-get shape :tool-names) '("e2e_echo")))
      (dolist (key '(:connection-id :websocket-reused
                     :websocket-reuse-count :websocket-request-mode
                     :prompt-layout-revision))
        (should (plist-member shape key))
        (should-not (plist-get shape key)))
      (should-not (string-match-p
                   (regexp-quote "captured-provider-secret") encoded))
      (should-not (string-match-p
                   (regexp-quote "secret tool description") encoded)))))

(ert-deftest e-live-e2e-test-external-evidence-record-joins-metadata-by-body-index ()
  "Every captured body gets a shape when metadata is shorter than the prefix."
  (let* ((bodies
          '((:model "gpt-5.6-sol" :input [] :tools [])
            (:model "gpt-5.6-sol" :input [(:type "message" :role "user")])
            (:model "gpt-5.6-sol" :input [(:type "message" :role "assistant")])))
         (metadata
          '((:diagnostics (:websocket-connection-id "socket-1"
                           :websocket-reused nil
                           :websocket-reuse-count 0
                           :websocket-request-mode full
                           :prompt-layout-revision "layout-v1"))
            (:diagnostics (:websocket-connection-id "socket-1"
                           :websocket-reused t
                           :websocket-reuse-count 1
                           :websocket-request-mode delta
                           :prompt-layout-revision "layout-v1"))))
         (record
          (e-live-e2e--external-evidence-record
           :scenario 'chatgpt-canonical-warm-prefix
           :provider 'codex
           :profile '(:name "ChatGPT Codex"
                      :base-url "https://chatgpt.example/codex"
                      :responses-transport websocket
                      :response-store :json-false)
           :model "gpt-5.6-sol"
           :request-bodies bodies
           :request-metadata metadata
           :timeout 120.0
           :started-at 100.0
           :ended-at 100.5))
         (shapes (plist-get record :material-request-shape)))
    (should (= (plist-get record :request-count) 3))
    (should (= (length shapes) 3))
    (should (equal (mapcar (lambda (shape) (plist-get shape :request-index))
                           shapes)
                   '(1 2 3)))
    (should (equal (plist-get (nth 0 shapes) :connection-id) "socket-1"))
    (should (equal (plist-get (nth 1 shapes) :websocket-reuse-count) 1))
    (dolist (key '(:connection-id :websocket-reused
                   :websocket-reuse-count :websocket-request-mode
                   :prompt-layout-revision))
      (should (plist-member (nth 2 shapes) key))
      (should-not (plist-get (nth 2 shapes) key)))
    (should (equal (plist-get record :socket-connection-ids)
                   '("socket-1")))))

(ert-deftest e-live-e2e-test-external-scenario-uses-profile-token-auth ()
  "A token-auth Responses profile does not require the Codex auth file."
  (let ((process-environment (copy-sequence process-environment))
        records
        ran)
    (setenv "ENG_AI_MODEL_GW_KEY" "gateway-test-secret")
    (cl-letf (((symbol-function 'file-readable-p) (lambda (&rest _) nil))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (record) (push record records))))
      (should
       (equal
        (e-live-e2e--run-external-scenario
         :scenario 'gateway-auth
         :provider 'eng-ai-gateway-gpt
         :profile '(:name "Engineering AI gateway"
                    :wire-api responses
                    :responses-transport http
                    :requires-openai-auth nil
                    :env-key "ENG_AI_MODEL_GW_KEY")
         :model "gpt-5.6-sol"
         :timeout 120.0
         :started-at 100.0
         :capture (lambda () nil)
         :thunk (lambda () (setq ran t) 'gateway-ran))
        'gateway-ran)))
    (should ran)
    (should (= (length records) 1))
    (should (equal (plist-get (car records) :result) "unavailable"))
    (should-not
     (string-match-p "gateway-test-secret"
                     (json-encode
                      (e-live-e2e--json-plist (car records)))))))

(ert-deftest e-live-e2e-test-external-scenario-missing-token-is-configuration-unavailable ()
  "A token profile with no usable declared environment key is unavailable."
  (let ((process-environment (copy-sequence process-environment))
        records
        ran
        condition)
    (setenv "ENG_AI_MODEL_GW_KEY" nil)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (&rest _) nil))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (record) (push record records))))
      (condition-case caught
          (e-live-e2e--run-external-scenario
           :scenario 'gateway-auth
           :provider 'eng-ai-gateway-gpt
           :profile '(:name "Engineering AI gateway"
                      :wire-api responses
                      :responses-transport http
                      :requires-openai-auth nil
                      :env-key "ENG_AI_MODEL_GW_KEY")
           :model "gpt-5.6-sol"
           :timeout 120.0
           :started-at 100.0
           :capture (lambda () nil)
           :thunk (lambda () (setq ran t)))
        (error (setq condition caught))))
    (should condition)
    (should (eq (car condition) 'ert-test-skipped))
    (should-not ran)
    (should (= (length records) 1))
    (should (equal (plist-get (car records) :result)
                   "configuration-unavailable"))))

(ert-deftest e-live-e2e-test-external-scenario-codex-auth-still-uses-file ()
  "Codex-managed profiles use their readable auth file, not token fallback."
  (let ((process-environment (copy-sequence process-environment))
        (file-readable nil)
        records
        ran
        condition)
    (setenv "ENG_AI_MODEL_GW_KEY" "must-not-be-used")
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (&rest _) file-readable))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (record) (push record records))))
      (condition-case caught
          (e-live-e2e--run-external-scenario
           :scenario 'codex-auth
           :provider 'codex
           :profile '(:name "ChatGPT Codex"
                      :wire-api responses
                      :responses-transport websocket
                      :requires-openai-auth t
                      :env-key "ENG_AI_MODEL_GW_KEY")
           :model "gpt-5.6-sol"
           :timeout 120.0
           :started-at 100.0
           :capture (lambda () nil)
           :thunk (lambda () (setq ran t) 'codex-ran))
        (error (setq condition caught)))
    (should condition)
    (should (eq (car condition) 'ert-test-skipped))
    (should-not ran)
    (should (equal (plist-get (car records) :result)
                   "configuration-unavailable"))
    (setq file-readable t)
    (setq records nil)
    (setq condition nil)
    (should
     (equal
      (e-live-e2e--run-external-scenario
       :scenario 'codex-auth
       :provider 'codex
       :profile '(:name "ChatGPT Codex"
                  :wire-api responses
                  :responses-transport websocket
                  :requires-openai-auth t
                  :env-key "ENG_AI_MODEL_GW_KEY")
       :model "gpt-5.6-sol"
       :timeout 120.0
       :started-at 100.0
       :capture (lambda () nil)
       :thunk (lambda () (setq ran t) 'codex-ran))
      'codex-ran))
    (should ran)
    (should (= (length records) 1)))))

(ert-deftest e-live-e2e-test-external-evidence-record-reports-profile-identity ()
  "External records distinguish Responses HTTP and WebSocket identities."
  (dolist (case
           '((http "responses-http" "e-openai-codex--http-request-start")
             (websocket "responses-websocket"
                         "e-openai-codex--websocket-request-start")))
    (let* ((transport (nth 0 case))
           (body `(:model "gpt-5.6-sol"
                    :input [(:type "message" :role "user"
                             :content "private captured content")]
                    :tools []))
           (metadata
            (if (eq transport 'websocket)
                '((:diagnostics (:websocket-connection-id "socket-1"
                               :websocket-reused t
                               :websocket-reuse-count 1
                               :websocket-request-mode full
                               :prompt-layout-revision "layout-ws")))
              '((:transport url-retrieve
                 :websocket-connection-id "not-an-http-socket"
                 :prompt-layout-revision "layout-http"))))
           (record
            (e-live-e2e--external-evidence-record
             :scenario 'profile-identity
             :provider 'profile-provider
             :profile (list :name "Profile"
                            :wire-api 'responses
                            :responses-transport transport
                            :base-url "https://provider.example"
                            :response-store :json-false)
             :model "gpt-5.6-sol"
             :request-bodies (list body)
             :request-metadata metadata
             :timeout 120.0
             :started-at 100.0
             :ended-at 100.5))
           (encoded (json-encode (e-live-e2e--json-plist record)))
           (shape (car (plist-get record :material-request-shape))))
      (should (equal (plist-get record :transport) (nth 1 case)))
      (should (equal (plist-get record :native-requester) (nth 2 case)))
      (if (eq transport 'websocket)
          (progn
            (should (equal (plist-get record :socket-connection-ids)
                           '("socket-1")))
            (should (equal (plist-get shape :connection-id) "socket-1")))
        (should-not (plist-get record :socket-connection-ids))
        (should (plist-member shape :connection-id))
        (should-not (plist-get shape :connection-id)))
      (should-not (string-match-p "private captured content" encoded)))))

(ert-deftest e-live-e2e-test-external-finalizer-classifies-injected-failures ()
  "The shared finalizer records failures before preserving their outcome."
  (let (records)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (&rest _) t))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (record) (push record records))))
      (dolist (case
               (list
                (list "provider/infrastructure-failure"
                      (lambda () (error "socket unavailable"))
                      'error)
                (list "inconclusive-timeout"
                      (lambda ()
                        (error "inconclusive-timeout: deadline expired"))
                      'error)
                (list "semantic-failure"
                      (lambda () (ert-fail "semantic assertion"))
                      'ert-test-failed)))
        (setq records nil)
        (let (condition)
          (condition-case caught
              (e-live-e2e--run-external-scenario
               :scenario 'chatgpt-canonical-tool-heavy
               :provider 'codex
               :profile '(:name "ChatGPT Codex"
                          :base-url "https://chatgpt.example/codex"
                          :responses-transport websocket
                          :requires-openai-auth t
                          :response-store :json-false)
               :model "gpt-5.6-sol"
               :timeout 120.0
               :started-at 100.0
               :capture (lambda () nil)
               :thunk (cadr case))
            (error (setq condition caught)))
          (should condition)
          (should (eq (car condition) (nth 2 case)))
          (should (= (length records) 1))
          (let ((record (car records)))
            (should (equal (plist-get record :result) (car case)))
            (should (equal (plist-get record :semantic-result)
                           (if (equal (car case) "semantic-failure")
                               "failure"
                             "unavailable")))
            (should (equal (plist-get record :cache-result)
                           "unavailable"))))))))

(ert-deftest e-live-e2e-test-provider-metrics-record-reports-bounded-scalars ()
  "Metric extraction preserves unavailable cache fields and reports once."
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (push (apply #'format format-string arguments) messages))))
      (let ((metrics
             (e-live-e2e--provider-metrics-record
              '(:status done
                :elapsed-seconds 0.417
                :diagnostics (:websocket-connection-id "conn-1"
                              :websocket-reuse-count 2))
              '(:input-tokens 321))))
        (should (= (plist-get metrics :provider-request-latency-seconds)
                   0.417))
        (should (= (plist-get metrics :input-tokens) 321))
        (should (eq (plist-get metrics :cached-input-tokens) 'unavailable))
        (should (equal (plist-get metrics :connection-id) "conn-1"))
        (should (= (plist-get metrics :reuse-count) 2))
        (should-not (plist-member metrics :status))
        (e-live-e2e--report-provider-metrics metrics)
        ;; An explicit provider-reported zero remains a numeric measurement.
        (should (= (plist-get
                    (e-live-e2e--provider-metrics-record
                     '(:elapsed-seconds 0.1 :diagnostics nil)
                     '(:input-tokens 1 :cached-input-tokens 0))
                    :cached-input-tokens)
                   0))))
    (setq messages (nreverse messages))
    (should (= (length messages) 1))
    (should (string-match-p
             "provider-request-latency-seconds=0.417"
             (car messages)))
    (should (string-match-p "input-tokens=321" (car messages)))
    (should (string-match-p "cached-input-tokens=unavailable"
                           (car messages)))
    (should (string-match-p "connection-id=conn-1" (car messages)))
    (should (string-match-p "reuse-count=2" (car messages)))
    (should-not (string-match-p "status=" (car messages)))))

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
  "ChatGPT Codex sends the inherited observation as a late developer frontier."
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
            (should-not
             (e-session-provider-anchors
              (e-harness-sessions harness) session-id)))
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
               (newest-anchor (car (last anchors))))
          (ert-info ((format "Codex continuation diagnostics: %S" diagnostics))
            (should (equal
                     (plist-get
                      (e-backend-context-capabilities
                       (e-harness-backend harness)
                       nil)
                     :observation-delivery)
                     'inherited))
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
            (should-not (plist-member second-body :previous_response_id))
            (dolist (body (list first-body second-body))
              (should-not (plist-member body :prompt_cache_options))
              (should-not
               (e-live-e2e--contains-p
                (prin1-to-string body)
                "prompt_cache_breakpoint")))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   first-input)
                           '("developer" "user" "developer")))
            (should-not
             (e-live-e2e--contains-p (prin1-to-string first-input)
                                     old-marker))
            (should (equal (mapcar (lambda (item) (plist-get item :role))
                                   second-input)
                           '("developer" "user" "assistant" "user"
                             "developer")))
            (should
             (seq-find
              (lambda (item)
                (string-match-p
                 "subscription stable instructions"
                 (or (plist-get (aref (plist-get item :content) 0) :text) "")))
              first-input))
            (should (equal (plist-get first-body :instructions)
                           "You are a helpful assistant."))
            (should (equal (plist-get second-body :instructions)
                           "You are a helpful assistant."))
            (should (e-live-e2e--contains-p
                     (prin1-to-string second-input)
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
                        'inherited))
            (should-not (plist-get diagnostics
                                   :replaceable-current-state-present))
            (should (eq (plist-get diagnostics :provider-anchor-safety)
                        'hold-inherited-observation))
            (should (eq (plist-get diagnostics :websocket-request-mode)
                        'full))
            (should-not (plist-get diagnostics :previous-response-id-present))
            (should (eq (plist-get diagnostics :websocket-reused) t))
            (should (= (plist-get diagnostics :websocket-reuse-count) 1))
            (dolist (key '(:websocket-request-mode
                           :previous-response-id-present
                           :websocket-reused
                           :websocket-reuse-count))
              (should (equal (plist-get finished-diagnostics key)
                             (plist-get diagnostics key))))
            (should-not newest-anchor)
            (should-not anchors)))))))

(ert-deftest e-live-e2e-test-chatgpt-canonical-tool-heavy ()
  "A tool turn has an immediate continuation and a canonical next request."
  (unless (fboundp 'e-openai-codex--websocket-request-start)
    (ert-skip "The OpenAI Responses WebSocket adapter is not loaded."))
  (e-live-e2e--require-enabled)
  (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id)))
    (unless (e-openai--builtin-codex-profile-p provider-id profile)
      (ert-skip "The configured provider is not the exact built-in ChatGPT Codex profile."))
    (should (eq (plist-get profile :response-store) :json-false))
    ;; The built-in profile uses the general configurable socket policy; it
    ;; does not carry a Codex-specific timeout override.
    (should-not (plist-member profile :websocket-idle-close-seconds)))
  (let* ((old-marker "LIVE-INITIAL-OBSERVATION")
         (new-marker "LIVE-CURRENT-OBSERVATION")
         (current-state old-marker)
         (provider
          (e-context-provider-create
           :name 'live-codex-tool-heavy-current-state
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    (list (list :role 'system :content current-state)))))
         (layer
          (e-layer-create
           :id 'live-codex-tool-heavy
           :name "Live Codex Tool Heavy"
           :capabilities
           (list
            (e-capability-create
             :id 'live-codex-tool-heavy
             :instructions "LIVE-STABLE-INSTRUCTIONS"
             :context-providers (list provider)))))
         (raw-tool-output "LIVE-TOOL-OUTPUT"))
    (let ((e-context-lifetime-shadow-projection-enabled t))
      (e-live-e2e--with-harness
        (harness session-id
                 :layers (list layer (e-live-e2e--deterministic-tool-layer)))
      (let* ((scenario-timeout (e-live-e2e--cache-scenario-timeout))
             (started-at (float-time))
             (deadline (+ started-at scenario-timeout))
             (original-start
              (symbol-function 'e-openai-codex--websocket-request-start))
             (original-frame
              (symbol-function 'e-harness--lifetime-tool-observation-frame))
             (original-record
              (symbol-function
               'e-openai-codex--websocket-session-record-response))
             request-bodies
             request-handles
             completed-response-ids
             captured-tool-source
             curation-record
             first-turn-id
             tool-turn-id
             warm-turn-id
             (cache-result nil)
             (semantic-result "unavailable"))
        (e-live-e2e--run-external-scenario
         :scenario 'chatgpt-canonical-tool-heavy
         :provider e-openai-default-provider
         :profile (e-openai-provider-profile e-openai-default-provider)
         :model e-openai-default-model
         :timeout scenario-timeout
         :started-at started-at
         :capture
         (lambda ()
           (list
            :request-bodies request-bodies
            :request-metadata
            (mapcar (lambda (handle)
                      (ignore-errors (e-backend-request-metadata handle)))
                    request-handles)
            :usage-payloads
            (mapcar (lambda (event) (plist-get event :payload))
                    (e-live-e2e--activity-of-type
                     harness session-id 'token-usage))
            :semantic-result semantic-result
            :cache-result cache-result))
         :thunk
         (lambda ()
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
                     (funcall original-record session response-id properties)))
                  ((symbol-function
                    'e-harness--lifetime-tool-observation-frame)
                   (lambda (&rest args)
                     (let ((frame (apply original-frame args)))
                       (setq captured-tool-source
                             (copy-tree
                              (seq-find
                               (lambda (source)
                                 (equal (plist-get source :value)
                                        raw-tool-output))
                               (e-context-lifetime-frame-curation-sources
                                frame))))
                       frame))))
          (let ((first-result
                 (e-live-e2e--prompt-batch-before-deadline
                  harness session-id
                  "Reply with exactly LIVE-R0-READY. Do not call tools, context-curate, or repeat observation markers."
                  deadline)))
            (setq first-turn-id (plist-get first-result :id))
            (should (stringp first-turn-id))
            (should (equal
                     (string-trim (e-live-e2e--assistant-content first-result))
                     "LIVE-R0-READY"))
            (should-not (e-live-e2e--contains-p
                         (e-live-e2e--assistant-content first-result)
                         old-marker)))
          (let ((tool-result
                 (e-live-e2e--prompt-batch-before-deadline
                  harness session-id
                  "Call e2e_deterministic exactly once. After its result arrives, call the reserved context-curate carrier exactly once. Select the numeric source label whose displayed exact value is the result returned by e2e_deterministic and summarize it with text LIVE-CURATED-FACT. Then reply with exactly LIVE-R2-READY and no other text. Do not call any other tool."
                  deadline)))
            (setq tool-turn-id (plist-get tool-result :id))
            (setq curation-record
                  (car (last (e-session-context-curations
                              (e-harness-sessions harness) session-id))))
            (should (stringp tool-turn-id))
            (should (equal
                     (string-trim (e-live-e2e--assistant-content tool-result))
                     "LIVE-R2-READY")))
          (setq current-state new-marker)
          (let ((warm-result
                 (e-live-e2e--prompt-batch-before-deadline
                  harness session-id
                  "Reply with exactly the current observation marker from your instructions and no other text; do not call context-curate."
                  deadline)))
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
               (tool-followup-body (nth 2 ordered-bodies))
               (warm-body (nth 3 ordered-bodies))
               (warm-handle (nth 3 ordered-handles))
               (tool-followup-input
                (append (plist-get tool-followup-body :input) nil))
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
               (tool-followup-diagnostics
                (plist-get (e-backend-request-metadata
                            (nth 2 ordered-handles))
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
               (warm-usage-events
                (seq-filter
                 (lambda (event)
                   (equal (plist-get event :turn-id) warm-turn-id))
                 (e-live-e2e--activity-of-type
                  harness session-id 'token-usage)))
               (warm-cache-result
                (e-live-e2e--cache-result
                 (mapcar (lambda (event) (plist-get event :payload))
                         warm-usage-events)))
               (warm-metrics nil))
          (setq cache-result warm-cache-result)
          (should (= (length ordered-bodies) 4))
          (should (= (length ordered-handles) 4))
          (should (= (length response-ids) 4))
          (should (string-match-p
                   (regexp-quote old-marker)
                   (prin1-to-string first-body)))
          (should (string-match-p
                   (regexp-quote old-marker)
                   (prin1-to-string tool-body)))
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
          (should (equal (plist-get tool-followup-body
                                    :previous_response_id)
                         r1))
          (should (eq (plist-get tool-followup-diagnostics
                                 :websocket-request-mode)
                      'incremental))
          (should
           (seq-some
            (lambda (item)
              (and (equal (plist-get item :type) "function_call_output")
                   (equal (plist-get item :output) raw-tool-output)))
            tool-followup-input))
          (should (seq-some
                   (lambda (tool)
                     (member (plist-get tool :name)
                             '("context-curate" context-curate)))
                   (append (plist-get first-body :tools) nil)))
          (should (seq-some
                   (lambda (tool)
                     (member (plist-get tool :name)
                             '("context-curate" context-curate)))
                   (append (plist-get warm-body :tools) nil)))
          (should (seq-some
                   (lambda (tool)
                     (member (plist-get tool :name)
                             '("context-curate" context-curate)))
                   (append (plist-get tool-followup-body :tools) nil)))
          (should (and curation-record
                       (= (plist-get curation-record :record-version) 3)))
          (let ((item (car (plist-get curation-record :items))))
            (should (eq (plist-get item :kind) 'summary))
            (should (equal (plist-get item :text) "LIVE-CURATED-FACT"))
            (should captured-tool-source)
            (should (member (plist-get captured-tool-source
                                       :source-fingerprint)
                            (plist-get item :source-fingerprints))))
          (should-not (plist-member warm-body :previous_response_id))
          (should (eq (plist-get warm-diagnostics :websocket-request-mode)
                      'full))
          (should (numberp (plist-get warm-diagnostics
                                      :websocket-idle-close-seconds)))
          (should (>= (plist-get warm-diagnostics
                                  :websocket-idle-close-seconds)
                       0))
          (should (eq (plist-get warm-diagnostics :websocket-reused) t))
          ;; Keep semantic endpoint status separate from the bounded metrics
          ;; record, whose latency comes from the loop-owned lifecycle event.
          (should (eq (plist-get warm-finished-payload :status) 'done))
          (when warm-usage-events
            (setq warm-metrics
                  (e-live-e2e--provider-metrics-record
                   warm-finished-payload
                   (plist-get (car warm-usage-events) :payload)))
            (should (numberp
                     (plist-get warm-metrics
                                :provider-request-latency-seconds)))
            (should (>= (plist-get warm-metrics
                                    :provider-request-latency-seconds)
                         0.0))
            (should (numberp (plist-get warm-metrics :input-tokens)))
            (should (or (eq (plist-get warm-metrics :cached-input-tokens)
                           'unavailable)
                        (numberp (plist-get warm-metrics
                                              :cached-input-tokens))))
            (should (stringp (plist-get warm-metrics :connection-id)))
          (should (numberp (plist-get warm-metrics :reuse-count))))
          (should (equal (plist-get first-diagnostics
                                    :websocket-connection-id)
                         (plist-get warm-diagnostics
                                    :websocket-connection-id)))
          (should warm-finished-event)
          (should (= (length warm-finished-events) 1))
          (should (eq (plist-get warm-finished-diagnostics
                                 :websocket-request-mode)
                      'full))
          (should (numberp (plist-get warm-finished-diagnostics
                                      :websocket-idle-close-seconds)))
          (should (>= (plist-get warm-finished-diagnostics
                                  :websocket-idle-close-seconds)
                       0))
          (should-not (plist-get warm-finished-diagnostics
                                 :previous-response-id-present))
          (dolist (response-id (list r0 r1 r2))
            (should-not (string-match-p
                         (regexp-quote response-id)
                         (prin1-to-string warm-finished-diagnostics))))
          (should-not (string-match-p
                       (regexp-quote raw-tool-output)
                       (prin1-to-string warm-finished-diagnostics)))
          (should (string-match-p
                   (regexp-quote new-marker)
                   warm-body-printed))
          (should (string-match-p
                   (regexp-quote "LIVE-STABLE-INSTRUCTIONS")
                   warm-body-printed))
          (should-not (string-match-p (regexp-quote old-marker)
                                      warm-body-printed))
          (should-not (string-match-p (regexp-quote raw-tool-output)
                                      warm-body-printed))
          (should (string-match-p
                   (regexp-quote "LIVE-CURATED-FACT")
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
          (dolist (response-id (list r0 r1 r2))
            (should-not (string-match-p
                         (regexp-quote response-id)
                         warm-body-printed)))
          (when warm-metrics
            (e-live-e2e--report-provider-metrics warm-metrics))
          (setq semantic-result "pass")
          (cond
           ((equal warm-cache-result "warm") nil)
           ((equal warm-cache-result "product-contract-failure")
            (ert-fail
             "The provider explicitly reported zero cached tokens for the canonical ordinary request."))
           (t
            (ert-skip
             "Cached-token usage was unavailable; external cache evidence is inconclusive.")))
          (e-backend-cancel-request warm-handle)))))))))

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

(ert-deftest e-live-e2e-test-chatgpt-canonical-warm-prefix ()
  "A canonical warm-prefix pair is measured on retained and replaced sockets."
  (unless (fboundp 'e-openai-codex--websocket-request-start)
    (ert-skip "The OpenAI Responses WebSocket adapter is not loaded."))
  (e-live-e2e--require-enabled)
  (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id)))
    (unless (e-openai--builtin-codex-profile-p provider-id profile)
      (ert-skip "The configured provider is not the exact built-in ChatGPT Codex profile."))
    (should (eq (plist-get profile :response-store) :json-false))
    (should (eq (plist-get profile :observation-delivery) 'inherited)))
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
      (let* ((scenario-timeout (e-live-e2e--cache-scenario-timeout))
             (started-at (float-time))
             (deadline (+ started-at scenario-timeout))
             (original-start
              (symbol-function 'e-openai-codex--websocket-request-start))
             request-bodies
             request-handles
             request-sessions
             (cache-result nil)
             (semantic-result "unavailable"))
        (e-live-e2e--run-external-scenario
         :scenario 'chatgpt-canonical-warm-prefix
         :provider e-openai-default-provider
         :profile (e-openai-provider-profile e-openai-default-provider)
         :model e-openai-default-model
         :timeout scenario-timeout
         :started-at started-at
         :capture
         (lambda ()
           (let* ((ordered-bodies (reverse request-bodies))
                  (ordered-handles (reverse request-handles))
                  (usage-events
                   (e-live-e2e--activity-of-type
                    harness session-id 'token-usage)))
             (list
              :model (plist-get (plist-get (car ordered-bodies) :body) :model)
              :request-bodies
              (mapcar (lambda (entry) (plist-get entry :body)) ordered-bodies)
              :request-metadata
              (mapcar (lambda (handle)
                        (ignore-errors (e-backend-request-metadata handle)))
                      ordered-handles)
              :usage-payloads
              (mapcar (lambda (event) (plist-get event :payload))
                      usage-events)
              :semantic-result semantic-result
              :cache-result cache-result)))
         :thunk
         (lambda ()
          (cl-letf (((symbol-function 'e-openai-codex--websocket-request-start)
                   (lambda (&rest args)
                     (push (list :body
                                 (copy-tree (plist-get args :body-data))
                                 :full-body
                                 (copy-tree (plist-get args :full-body-data)))
                           request-bodies)
                     (push (plist-get args :session) request-sessions)
                     (let ((request (apply original-start args)))
                       (push request request-handles)
                       request))))
          (e-live-e2e--prompt-batch-before-deadline
           harness session-id "Reply with exactly: CROSS-TURN-ONE" deadline)
          (setq current-state "live state two")
          (e-live-e2e--prompt-batch-before-deadline
           harness session-id "Reply with exactly: CROSS-TURN-TWO" deadline)
          ;; Deliberately replace the retained socket, then make the same
          ;; canonical warm-prefix shape observable on the new connection.
          (when-let ((session (car request-sessions)))
            (e-openai-codex--websocket-session-close session))
          (setq current-state "live state three")
          (e-live-e2e--prompt-batch-before-deadline
           harness session-id "Reply with exactly: CROSS-TURN-THREE" deadline))
        (let* ((requests (e-live-e2e--activity-of-type
                          harness session-id 'provider-request-started))
               (latest (car (last requests)))
               (diagnostics (plist-get (plist-get latest :payload) :diagnostics))
               (ordered-bodies (reverse request-bodies))
               (ordered-handles (reverse request-handles))
               (first-body (plist-get (car ordered-bodies) :body))
               (latest-body
                (plist-get (car (last ordered-bodies)) :body))
               (request-metadata
                (mapcar #'e-backend-request-metadata ordered-handles))
               (middle-diagnostics
                (plist-get (nth 1 request-metadata) :diagnostics))
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
                    "live state three"
                    (or (plist-get (car (plist-get item :content)) :text)
                        "")))
                 (append (plist-get latest-body :input) nil)))
               (usage-events (e-live-e2e--activity-of-type
                              harness session-id 'token-usage))
               (retained-usage (plist-get (nth 1 usage-events) :payload))
               (replacement-usage (plist-get (nth 2 usage-events) :payload))
               (retained-cache-result
                (e-live-e2e--cache-result (list retained-usage)))
               (replacement-cache-result
                (e-live-e2e--cache-result (list replacement-usage)))
               (computed-cache-result
                (e-live-e2e--cache-result
                 (list retained-usage replacement-usage)))
               (tool-differences
                (e-live-e2e--request-tool-differences
                 first-body latest-body)))
          (unless (plist-member diagnostics :websocket-request-mode)
            (ert-skip "The configured live backend is not Responses WebSocket mode."))
          (ert-info ((format "WebSocket diagnostics: %S; tool differences: %S"
                             diagnostics tool-differences))
            (should (eq (plist-get diagnostics :websocket-request-mode)
                        'full))
            (should-not (plist-get diagnostics :previous-response-id-present))
            (should-not (plist-get diagnostics :websocket-reused))
            (should (member (plist-get diagnostics :prompt-cache-mode)
                            '("explicit" "implicit-segmented")))
            (should (eq (plist-get diagnostics :observation-delivery)
                        'inherited))
            (should-not (plist-get diagnostics
                                   :replaceable-current-state-present))
            (should (eq (plist-get diagnostics :provider-anchor-safety)
                        'hold-inherited-observation))
            (should (or (equal (plist-get diagnostics :prompt-layout-revision)
                              e-openai-gpt56-explicit-cache-layout-revision)
                        (plist-get diagnostics :prompt-layout-revision)))
            (when (equal (plist-get diagnostics :prompt-cache-mode)
                         "explicit")
              (should first-breakpoint)
              (should (equal (plist-get first-breakpoint
                                        :prompt_cache_breakpoint)
                             '(:mode "explicit")))
              (should (equal (plist-get latest-body :prompt_cache_options)
                             '(:mode "explicit"))))
            (should (equal (plist-get latest-body :include)
                           ["reasoning.encrypted_content"]))
            (should-not (plist-member latest-body :previous_response_id))
            (should (equal latest-instructions
                           "You are a helpful assistant."))
            (should latest-current-input)
            (should (e-live-e2e--contains-p
                     (prin1-to-string latest-body)
                     "live state three"))
            (should (eq (plist-get middle-diagnostics :websocket-reused) t))
            (should-not (plist-get diagnostics :websocket-reused))
            (should-not (equal
                         (plist-get middle-diagnostics :websocket-connection-id)
                         (plist-get diagnostics :websocket-connection-id)))
            (should (= (length ordered-bodies) 3))
            (should (= (length ordered-handles) 3))
            (should (equal (plist-get first-body :prompt_cache_key)
                           (plist-get (plist-get (nth 1 ordered-bodies) :body)
                                      :prompt_cache_key)))
            (should (equal (plist-get first-body :prompt_cache_key)
                           (plist-get latest-body :prompt_cache_key)))
            (setq cache-result computed-cache-result)
            (setq semantic-result "pass")
            ;; Request one is the intentionally cold prefill.  Require both
            ;; the retained and replacement targets to report warm usage only
            ;; after their bounded evidence record has been emitted.
            (cond
             ((equal computed-cache-result "warm")
              (should (equal retained-cache-result "warm"))
              (should (equal replacement-cache-result "warm")))
             ((equal computed-cache-result "product-contract-failure")
              (ert-fail
               "The provider explicitly reported zero cached tokens for a warm target."))
             (t
             (ert-skip
               "Cached-token usage was unavailable; external cache evidence is inconclusive.")))))))))))

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

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
(require 'e-context-lifetime)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-base)
(require 'e-harness-registry)
(require 'e-layers)
(require 'e-openai)
(require 'e-session)
(require 'e-tools)
(load (expand-file-name
       "e-chat-sql-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(declare-function e-chat-sql-e2e-create-session "e-chat-sql-e2e-support"
                  (harness &rest arguments))
(declare-function e-chat-sql-e2e-prompt-async "e-chat-sql-e2e-support"
                  (harness session-id prompt))
(declare-function e-chat-sql-e2e-prompt-batch "e-chat-sql-e2e-support"
                  (harness session-id prompt &optional timeout))
(declare-function e-chat-sql-e2e-reset "e-chat-sql-e2e-support" ())

(defconst e-live-e2e--harness-id :chat-default
  "Registry id of the default chat harness exercised by live e2e tests.")

(defconst e-live-e2e--checked-in-anthropic-config-file
  (expand-file-name
   "e-e2e-config-anthropic.el"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Checked-in Anthropic E2E configuration used by profile selection tests.")

(defvar e-live-e2e--config-loaded nil
  "Non-nil once the E_E2E_CONFIG backend configuration file has been loaded.")

(defconst e-live-e2e--cache-scenario-timeout-default 120.0
  "Default total wall-clock bound for an external cache scenario.

This is runner configuration, not a Feature 88 semantic or cache contract.")

(define-error 'e-live-e2e-scenario-timeout
  "External E2E scenario deadline expired")

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
      '("responses-websocket" . "e-openai-websocket-request-start")
    '("responses-http" . "e-openai-http-request-start")))

(defun e-live-e2e--metadata-value (metadata key)
  "Return (PRESENT VALUE) for KEY in request METADATA or its diagnostics."
  (let ((diagnostics (plist-get metadata :diagnostics)))
    (cond
     ((plist-member metadata key)
      (list t (plist-get metadata key)))
     ((plist-member diagnostics key)
      (list t (plist-get diagnostics key)))
     (t (list nil nil)))))

(defun e-live-e2e--identity-url (url)
  "Return URL without query/fragment material, or nil when URL is absent."
  (when (stringp url)
    (car (split-string url "[?#]" t))))

(defun e-live-e2e--identity-base-url (endpoint)
  "Return the base identity for Responses ENDPOINT."
  (when-let* ((endpoint (e-live-e2e--identity-url endpoint)))
    (let ((base
           (replace-regexp-in-string
            "/responses\\(?:/[^/?#]+\\)?\\'" "" endpoint)))
      ;; The adapter uses the WebSocket scheme for the wire endpoint while a
      ;; profile's base URL is scheme-neutral.  Compare their HTTP identities,
      ;; but retain the exact endpoint separately in the evidence record.
      (replace-regexp-in-string
       "\\`wss://" "https://"
       (replace-regexp-in-string "\\`ws://" "http://"
                                (replace-regexp-in-string "/+\\'" "" base))))))

(defun e-live-e2e--identity-transport (value)
  "Normalize captured transport VALUE to a Responses transport label."
  (let ((value (downcase (format "%s" value))))
    (cond
     ((member value '("websocket" "responses-websocket"))
      "responses-websocket")
     ((member value '("http" "responses-http" "url-retrieve"
                      "sync-wrapper" "injected-request-function"))
      "responses-http")
     (t nil))))

(defun e-live-e2e--captured-request-identity
    (profile body metadata fallback-model)
  "Return the effective identity of captured Responses BODY and METADATA.
The body and actual request metadata are authoritative.  Profile values only
fill fields that the wire/adapter legitimately leaves implicit, so a record
cannot silently report a configured identity different from the request that
crossed the native requester."
  (let* ((metadata (or metadata nil))
         (transport-value
          (or (and (plist-member metadata :transport)
                   (plist-get metadata :transport))
              (cadr (e-live-e2e--metadata-value metadata
                                                :responses-transport))))
         (profile-transport (plist-get profile :responses-transport))
         (transport (or (e-live-e2e--identity-transport transport-value)
                        (e-live-e2e--identity-transport profile-transport)
                        ;; Responses profiles default to HTTP when transport
                        ;; is omitted; captured metadata still wins above.
                        "responses-http"))
         (requester-value
          (or (cadr (e-live-e2e--metadata-value metadata :native-requester))
              (cadr (e-live-e2e--metadata-value metadata :requester))))
         (requester
          (or (and requester-value (format "%s" requester-value))
              (if (equal transport "responses-websocket")
                  "e-openai-websocket-request-start"
                (when (equal transport "responses-http")
                  "e-openai-http-request-start"))))
         (endpoint-value
          (cadr (e-live-e2e--metadata-value metadata :url)))
         (endpoint (or (e-live-e2e--identity-url endpoint-value)
                       (e-live-e2e--identity-url
                        (plist-get profile :base-url))))
         (endpoint-captured-p (stringp (e-live-e2e--identity-url endpoint-value)))
         (metadata-model (e-live-e2e--metadata-value metadata :model))
         (model-captured-p
          (or (plist-member body :model) (car metadata-model)))
         (model
          (cond
           ((plist-member body :model) (plist-get body :model))
           ((car metadata-model) (cadr metadata-model))
           (t fallback-model)))
         (store
          (cond
           ((plist-member body :store)
            (list t (plist-get body :store) 'wire))
           ((car (e-live-e2e--metadata-value metadata :response-store))
            (let ((value (e-live-e2e--metadata-value metadata :response-store)))
              (list t (cadr value) 'metadata)))
           ((and (equal transport "responses-websocket"))
            ;; WebSocket Responses stores the response implicitly when the
            ;; wire omits `store'; this is the adapter's documented default.
            (list t t 'implicit-websocket))
           (t (list nil nil nil)))))
    (list :endpoint endpoint
          :endpoint-captured-p endpoint-captured-p
          :base-url (e-live-e2e--identity-base-url endpoint)
          :model model
          :model-captured-p model-captured-p
          :store-present (car store)
          :store (cadr store)
          :store-source (caddr store)
          :transport transport
          :native-requester requester)))

(defun e-live-e2e--identity-mismatches (profile identities)
  "Return bounded identity mismatches in PROFILE and captured IDENTITIES."
  (let (mismatches)
    (cl-loop for identity in identities
             for index from 1
             do (dolist (spec '((:base-url :base-url)
                                (:default-model :model)
                                (:model :model)
                                (:response-store :store)
                                (:responses-transport :transport)))
                  (let ((declared-key (car spec))
                        (effective-key (cadr spec)))
                    (when (and (plist-member profile declared-key)
                               (not (equal
                                     (if (eq effective-key :base-url)
                                         (e-live-e2e--identity-base-url
                                          (plist-get profile declared-key))
                                       (if (eq effective-key :transport)
                                           (e-live-e2e--identity-transport
                                            (plist-get profile declared-key))
                                         (plist-get profile declared-key)))
                                     (plist-get identity effective-key))))
                      (push (list :request-index index
                                  :field effective-key
                                  :declared (plist-get profile declared-key)
                                  :effective (plist-get identity effective-key))
                            mismatches)))))
    (nreverse mismatches)))

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
  (when-let* ((path (e-live-e2e--env "E_E2E_CONFIG")))
    (expand-file-name path)))

(defun e-live-e2e--load-config ()
  "Load the backend configuration file once and register default harnesses.
Signal nothing when unconfigured; callers skip in that case."
  (unless e-live-e2e--config-loaded
    (when-let* ((path (e-live-e2e--config-file)))
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
  (let* ((spec (e-default-chat-harness-spec))
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

(defun e-live-e2e--input-block-with-property (input property)
  "Return the first INPUT content block carrying PROPERTY.
Responses content is a list or vector, and a single block may itself be
represented as a plist.  Keep the scan at the encoded body boundary so the
assertion does not accidentally accept a top-level prompt-cache option."
  (seq-some
   (lambda (item)
     (let* ((content (plist-get item :content))
            (blocks
             (cond
              ((vectorp content) (append content nil))
              ((and (listp content) (keywordp (car content)))
               (list content))
              ((listp content) content))))
       (seq-find
        (lambda (block)
          (and (listp block)
               (plist-member block property)
               block))
        blocks)))
   (if (vectorp input) (append input nil) input)))

(ert-deftest e-live-e2e-test-input-breakpoint-scan-handles-content-sequences ()
  "The explicit breakpoint assertion scans list and vector content blocks."
  (let ((input
         [(:type "message"
           :content [(:type "input_text" :text "without-marker")])
          (:type "message"
           :content ((:type "input_text" :text "before")
                     (:type "input_text"
                      :prompt_cache_breakpoint (:mode "explicit"))))]))
    (should
     (equal
      (e-live-e2e--input-block-with-property input :prompt_cache_breakpoint)
      '(:type "input_text"
        :prompt_cache_breakpoint (:mode "explicit"))))))

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
   (e-session-local-activity-events (e-harness-sessions harness) session-id)))

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

(defconst e-live-e2e--adoption-dependency-cone-files
  '("lisp/core/e-context-lifetime.el"
    "lisp/core/e-harness.el"
    "lisp/core/e-loop.el"
    "lisp/core/e-session.el"
    "lisp/layers/harness/e-harness-base.el"
    "lisp/layers/harness/e-tool-invocation-details.el"
    "lisp/layers/harness/e-session-tmp-resources.el"
    "lisp/adapters/openai/e-openai.el"
    "e2e/e-live-e2e-test.el")
  "Source owners whose changes invalidate adoption evidence reuse.")

(defconst e-live-e2e--reasoning-summary-dependency-cone-files
  '("lisp/adapters/openai/e-openai.el"
    "e2e/e-live-e2e-test.el")
  "Source owners whose changes invalidate reasoning-summary probe evidence.")

(defun e-live-e2e--dependency-identity (files &optional explicit-root)
  "Return a content-free digest of dependency-cone FILES.
The digest is evidence identity, not a replacement for repository provenance;
missing owner files remain explicit in the digest rather than silently falling
back to the repository revision.  EXPLICIT-ROOT is a test seam for exercising
the material file set without changing the checkout."
  (let* ((source-file (or load-file-name
                          buffer-file-name
                          (locate-library "e-live-e2e-test")
                          (expand-file-name "e2e/e-live-e2e-test.el"
                                            default-directory)))
         (root (or explicit-root
                   (expand-file-name ".." (file-name-directory source-file)))))
    (secure-hash
     'sha256
     (mapconcat
      (lambda (relative)
        (let ((path (expand-file-name relative root)))
          (format "%s:%s" relative
                  (if (file-readable-p path)
                      (with-temp-buffer
                        (insert-file-contents-literally path)
                        (secure-hash 'sha256 (current-buffer)))
                    "unavailable"))))
      files
      "\n"))))

(defun e-live-e2e--adoption-dependency-identity ()
  "Return a content-free digest of the adoption dependency cone."
  (e-live-e2e--dependency-identity
   e-live-e2e--adoption-dependency-cone-files))

(defun e-live-e2e--reasoning-summary-dependency-identity ()
  "Return a content-free digest of the reasoning-summary probe cone."
  (e-live-e2e--dependency-identity
   e-live-e2e--reasoning-summary-dependency-cone-files))

(defconst e-live-e2e--reasoning-summary-evidence-schema-revision
  "e88-reasoning-summary-evidence-v1"
  "Revision of the bounded reasoning-summary capability evidence record.")

(defconst e-live-e2e--reasoning-summary-freshness-seconds
  (* 7 24 60 60)
  "Seven-calendar-day freshness window for reasoning-summary probe evidence.")

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

(defun e-live-e2e--keyword-plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist."
  (condition-case nil
      (and (listp value)
           (cl-evenp (length value))
           (cl-loop for (key _value) on value by #'cddr
                    always (keywordp key)))
    (error nil)))

(defun e-live-e2e--reasoning-identity-from-body (body)
  "Return the bounded effective reasoning identity from Responses BODY."
  (let* ((reasoning (and (listp body) (plist-get body :reasoning)))
         (valid-p (e-live-e2e--keyword-plist-p reasoning))
         (effort (and valid-p (plist-get reasoning :effort)))
         (summary (and valid-p (plist-get reasoning :summary))))
    (when (and (stringp effort)
               (not (string-empty-p effort))
               (member summary '("auto" "detailed")))
      (list :effort effort :summary summary))))

(defun e-live-e2e--captured-reasoning-state (request-bodies)
  "Return bounded reasoning identity state for captured REQUEST-BODIES."
  (let* ((identities (mapcar #'e-live-e2e--reasoning-identity-from-body
                             request-bodies))
         (first (car identities)))
    (list :valid-p
          (and identities
               first
               (cl-every (lambda (identity)
                           (and identity (equal identity first)))
                         identities))
          :effort (plist-get first :effort)
          :summary (plist-get first :summary)
          :probe-request-identity
          (and request-bodies
               (plist-get (e-live-e2e--request-shape (car request-bodies))
                          :body-sha256)))))

(defun e-live-e2e--reasoning-summary-presence (events &optional messages)
  "Return bounded summary presence from provider-neutral EVENTS/MESSAGES.
Only the presence category is retained; summary text is never returned."
  (let ((empty-p nil)
        (present-p nil))
    (dolist (event events)
      (let* ((payload (plist-get event :payload))
             (item (if (eq (plist-get event :type) 'reasoning-delta)
                       event
                     payload))
             (content (plist-get item :content)))
        (when (and (member (plist-get item :type)
                           '(reasoning-delta "reasoning-delta"))
                   (member (plist-get item :stream-kind)
                           '(summary "summary"))
                   (stringp content))
          (if (string-empty-p content)
              (setq empty-p t)
            (setq present-p t)))))
    ;; The parser keeps the provider's reasoning item on the assistant message
    ;; as opaque replay metadata.  Its summary member distinguishes an explicit
    ;; empty/null array from a response that omitted the member entirely.
    (dolist (message messages)
      (dolist (record (plist-get (plist-get message :metadata)
                                 :provider-replay-items))
        (let ((item (plist-get record :item)))
          (when (member (plist-get item :type) '("reasoning" reasoning))
            (if (plist-member item :summary)
                (let ((summary (plist-get item :summary)))
                  (if (and (or (listp summary) (vectorp summary))
                           (= (length summary) 0))
                      (setq empty-p t)
                    (setq present-p t))))))))
    (cond
     (present-p "present")
     (empty-p "empty")
     (t "absent"))))

(cl-defun e-live-e2e--classify-reasoning-summary-capability
    (&key identity-result request-valid-p completed-p endpoint-rejected-p
          provider-failure-p configuration-unavailable-p timeout-p)
  "Classify one reasoning-summary capability probe outcome.
Identity and request-shape failures are semantic; endpoint rejection is bounded
unavailability and never triggers a fallback request."
  (cond
   (configuration-unavailable-p "configuration-unavailable")
   (timeout-p "inconclusive-timeout")
   ((not (equal identity-result "pass")) "semantic-failure")
   ((not request-valid-p) "semantic-failure")
   (endpoint-rejected-p "unavailable")
   (provider-failure-p "provider/infrastructure-failure")
   ((not completed-p) "provider/infrastructure-failure")
   (t "pass")))

(defun e-live-e2e--reasoning-summary-endpoint-rejection-p (condition)
  "Return non-nil when CONDITION is an explicit summary-field rejection.
This is only a bounded classification aid for the capability probe; the
condition itself is never emitted as evidence."
  (let ((text (downcase (prin1-to-string condition))))
    (and (string-match-p "reasoning" text)
         (string-match-p "summary" text)
         (string-match-p
          "\\(?:unsupported\\|unknown\\|unrecognized\\|invalid\\|not[[:space:]]+support\\)"
          text))))

(defun e-live-e2e--reasoning-summary-captured-identity-result
    (profile request-bodies request-metadata model)
  "Classify the effective identity of captured reasoning probe requests.
The wire endpoint, model, and store must be captured where the adapter emits
them; only transport/requester defaults that are implicit in the adapter may
  be inferred."
  (let* ((identities
          (cl-loop for body in request-bodies
                   for index from 0
                   collect
                   (e-live-e2e--captured-request-identity
                    profile body (nth index request-metadata) model)))
         (required-fields '(:base-url :model :store :transport
                            :native-requester))
         (complete-p
          (and identities
               (cl-every
                (lambda (identity)
                  (and (cl-every (lambda (field)
                                   (plist-get identity field))
                                 required-fields)
                       (plist-get identity :endpoint-captured-p)
                       (plist-get identity :model-captured-p)
                       (plist-get identity :store-present)))
                identities)))
         (first (car identities))
         (drift-p
          (and first
               (seq-some
                (lambda (identity)
                  (seq-some
                   (lambda (field)
                     (not (equal (plist-get first field)
                                 (plist-get identity field))))
                   required-fields))
                (cdr identities)))))
    (cond
     ((not complete-p) "unavailable")
     ((or drift-p (e-live-e2e--identity-mismatches profile identities))
      "invalid")
     (t "pass"))))

(defun e-live-e2e--reasoning-summary-evidence-reusable-p
    (previous current &optional now)
  "Return non-nil when CURRENT can reuse bounded PREVIOUS probe evidence."
  (let* ((previous-time (e-live-e2e--adoption-time
                         (plist-get previous :timestamp)))
         (current-time (e-live-e2e--adoption-time
                        (plist-get current :timestamp)))
         (now-time (e-live-e2e--adoption-time (or now (float-time))))
         (identity-fields
          '(:evidence-schema-revision :scenario :provider-id :profile-id
            :base-url-identity :endpoint-identity :transport :store-mode
            :native-requester :model-id :reasoning-effort :reasoning-summary
            :material-request-shape :probe-request-identity
            :prompt-layout-revision :prompt-cache-key-derivation-revision
            :dependency-identity)))
    (and previous current previous-time current-time now-time
         (equal (plist-get previous :identity-result) "pass")
         (equal (plist-get current :identity-result) "pass")
         (equal (plist-get previous :result) "pass")
         (equal (plist-get current :result) "pass")
         (stringp (plist-get previous :dependency-identity))
         (stringp (plist-get current :dependency-identity))
         (not (string-empty-p (plist-get previous :dependency-identity)))
         (not (string-empty-p (plist-get current :dependency-identity)))
         (>= (- current-time previous-time) 0)
         (>= (- now-time current-time) 0)
         (<= (- now-time previous-time)
             e-live-e2e--reasoning-summary-freshness-seconds)
         (cl-every (lambda (field)
                     (equal (plist-get previous field)
                            (plist-get current field)))
                   identity-fields))))

(defun e-live-e2e--captured-body-at-boundary (request-bodies prior-count)
  "Return the first chronological BODY captured after PRIOR-COUNT bodies.
REQUEST-BODIES is newest-first, as maintained by the native capture helpers;
the boundary therefore identifies the first request of the next turn even when
that turn later emits continuation requests."
  (nth prior-count
       (mapcar (lambda (entry) (plist-get entry :body))
               (reverse request-bodies))))

(defun e-live-e2e--captured-bodies-between
    (request-bodies start-count end-count)
  "Return chronological captured bodies in the half-open COUNT range.
REQUEST-BODIES is newest-first.  The range is deliberately count-based: the
first body is the request that starts a user turn, while later bodies must be
validated as bounded same-turn continuations by the caller."
  (seq-subseq
   (mapcar (lambda (entry) (plist-get entry :body))
           (reverse request-bodies))
   start-count end-count))

(defun e-live-e2e--context-curation-ack-body-p (body)
  "Return non-nil when BODY contains one linked context-curate acknowledgement.
Other already-replayed items may be present in the request, but the reserved
call must have exactly one matching empty output in causal order."
  (let* ((input (append (plist-get body :input) nil))
         (curation-calls
          (seq-filter
           (lambda (item)
             (and (member (plist-get item :type)
                          '("function_call" function_call))
                  (member (plist-get item :name)
                          '("context-curate" context-curate))))
           input))
         (curation-call (car curation-calls))
         (call-id (and curation-call
                       (plist-get curation-call :call_id)))
         (matching-outputs
          (and call-id
               (seq-filter
                (lambda (item)
                  (and (member (plist-get item :type)
                               '("function_call_output"
                                 function_call_output))
                       (equal (plist-get item :call_id) call-id)
                       (equal (plist-get item :output) "")))
                input)))
         (call-index (and curation-call
                          (cl-position-if
                           (lambda (item) (eq item curation-call))
                           input)))
         (output-index (and (= (length matching-outputs) 1)
                            (cl-position-if
                             (lambda (item)
                               (eq item (car matching-outputs)))
                             input))))
    (and (= (length curation-calls) 1)
         (= (length matching-outputs) 1)
         (stringp call-id)
         (integerp call-index)
         (integerp output-index)
         (< call-index output-index))))

(defconst e-live-e2e--context-curation-description
  "Call context-curate at most once for the currently presented set of labeled ephemeral sources. After its acknowledgement, continue with ordinary tools or a normal answer. Call it again only after later tool work or context refresh presents a new set of labeled ephemeral sources; labels belong only to the currently presented frame and cannot be reused for an earlier frame. Use keep for exact retention and summaries for compact durable replacements. Use erase only for labels whose source marker says erase-eligible; never erase a label marked erase-ineligible. Any presented label you omit loses its exact content; it is valid to omit every label when none should be retained or erased. Separately owned derived context such as receipts may remain."
  "Expected lifecycle affordance in the reserved curation carrier.")

(defun e-live-e2e--context-curation-carrier-p (body)
  "Return non-nil when BODY carries the exact curation schema and affordance."
  (seq-some
   (lambda (tool)
     (let* ((parameters (plist-get tool :parameters))
            (properties (plist-get parameters :properties))
            (keep (plist-get properties :keep))
            (summaries (plist-get properties :summaries))
            (summary-item (plist-get summaries :items))
            (summary-properties (plist-get summary-item :properties))
            (sources (plist-get summary-properties :sources))
            (text (plist-get summary-properties :text))
            (erase (plist-get properties :erase))
            (keys (cl-loop for (key _value) on properties by #'cddr
                           collect key)))
       (and (equal (plist-get tool :type) "function")
            (equal (plist-get tool :name) "context-curate")
            (equal (plist-get tool :description)
                   e-live-e2e--context-curation-description)
            (equal (plist-get parameters :type) "object")
            (eq (plist-get parameters :additionalProperties) :json-false)
            (not (plist-member parameters :required))
            (equal (sort (copy-sequence keys)
                         (lambda (left right)
                           (string< (symbol-name left)
                                    (symbol-name right))))
                   '(:erase :keep :summaries))
            (equal (plist-get keep :type) "array")
            (equal (plist-get keep :maxItems) 16)
            (equal (plist-get summaries :type) "array")
            (equal (plist-get summaries :maxItems) 16)
            (equal (plist-get sources :type) "array")
            (equal (plist-get sources :minItems) 1)
            (equal (plist-get sources :maxItems) 16)
            (equal (plist-get text :type) "string")
            (equal (plist-get text :minLength) 1)
            (equal (plist-get erase :type) "array")
            (equal (plist-get erase :maxItems) 16))))
   (append (plist-get body :tools) nil)))

(defun e-live-e2e--responses-reasoning-auto-p (body)
  "Return non-nil when Responses BODY carries effective reasoning auto."
  (let ((reasoning (plist-get body :reasoning)))
    (and (listp reasoning)
         (stringp (plist-get reasoning :effort))
         (equal (plist-get reasoning :summary) "auto"))))

(defun e-live-e2e--adoption-source-bearing-request-index (bodies sentinel)
  "Return the unique source-bearing continuation index in chronological BODIES.
SENTINEL is the exact ordinary-tool result.  A candidate must be after an
ordinary e2e_deterministic call, contain its matching output, and place an
ephemeral source marker immediately before that output.  Later bodies that
replay a context-curate call are acknowledgements, not new candidates.  Return
nil for an absent or ambiguous candidate; the caller then cannot turn model
selection into a product-contract observation."
  (let (candidates)
    (cl-labels
        ((input-items (body)
           (append (plist-get body :input) nil))
         (ordinary-call-p (item)
           (and (member (plist-get item :type)
                        '("function_call" function_call
                          "tool_call" tool_call))
                (equal (format "%s" (plist-get item :name))
                       "e2e_deterministic")
                (stringp (plist-get item :call_id))))
         (source-marker-p (item)
           (let* ((content (plist-get item :content))
                  (blocks
                   (cond
                    ((vectorp content) (append content nil))
                    ((and (listp content) (keywordp (car content)))
                     (list content))
                    ((listp content) content))))
             (and (equal (plist-get item :type) "message")
                  (seq-some
                   (lambda (block)
                     (and (stringp (plist-get block :text))
                          (string-match-p
                           "^\\[ephemeral context source [0-9]+, ~[0-9]+ tokens, erase-eligible\\]$"
                           (plist-get block :text))))
                   blocks))))
         (curation-replay-p (body)
           (seq-some
            (lambda (item)
              (and (member (plist-get item :type)
                           '("function_call" function_call
                             "tool_call" tool_call))
                   (equal (format "%s" (plist-get item :name))
                          "context-curate")))
            (input-items body)))
         (source-bearing-p (body prior-call-ids)
           (let* ((input (input-items body))
                  (ordinary-calls
                   (cl-loop for item in input
                            for position from 0
                            when (ordinary-call-p item)
                            collect (cons (plist-get item :call_id)
                                          position)))
                  (matching-output-count 0))
             (cl-loop for item in input
                      for position from 0
                      when (and (member (plist-get item :type)
                                        '("function_call_output"
                                          function_call_output))
                                (equal (plist-get item :output) sentinel)
                                (stringp (plist-get item :call_id)))
                      do (let ((call-id (plist-get item :call_id)))
                           (when (and (> position 0)
                                      (or (member call-id prior-call-ids)
                                          (seq-some
                                           (lambda (call)
                                             (and (equal (car call) call-id)
                                                  (< (cdr call) position)))
                                           ordinary-calls))
                                      (source-marker-p
                                       (nth (1- position) input)))
                             (setq matching-output-count
                                   (1+ matching-output-count))))
                      finally return (= matching-output-count 1)))))
      (cl-loop for body in bodies
               for index from 0
               do (let ((prior-call-ids
                         (cl-loop for prior-body in (seq-take bodies index)
                                  append
                                  (cl-loop for item in (input-items prior-body)
                                           when (ordinary-call-p item)
                                           collect (plist-get item :call_id)))))
                    (when (and (> index 0)
                               (not (curation-replay-p body))
                               (source-bearing-p body prior-call-ids))
                      (push index candidates))))
      (when (= (length candidates) 1)
        (car candidates)))))

(defun e-live-e2e--turn-retrying-events (harness session-id turn-id)
  "Return durable retry events for TURN-ID in SESSION-ID."
  (seq-filter
   (lambda (event) (equal (plist-get event :turn-id) turn-id))
   (e-live-e2e--activity-of-type harness session-id 'turn-retrying)))

(defun e-live-e2e--retry-event-stage (event)
  "Return an optional causal stage marker from retry EVENT.
Current production retry events do not need to expose a stage.  Deterministic
fixtures may provide one at the event or payload boundary so the validator can
reject a retry attributed to a different stage."
  (let ((payload (plist-get event :payload)))
    (or (plist-get event :stage)
        (plist-get event :retry-stage)
        (and payload (plist-get payload :stage))
        (and payload (plist-get payload :retry-stage)))))

(defun e-live-e2e--stage-key (stage)
  "Return a comparable string key for causal STAGE."
  (and stage (format "%s" stage)))

(defun e-live-e2e--captured-turn-stage-cardinality
    (bodies stages retry-events)
  "Validate chronological BODIES against logical STAGES and RETRY-EVENTS.
Each stage has one logical request body followed by zero or more exact retry
duplicates.  A retry event with an explicit stage must match that stage;
untagged production retry events are assigned only to otherwise unmatched
duplicate bodies.  The result reports logical and duplicate counts so a
reserved acknowledgement retry remains one logical acknowledgement."
  (catch 'invalid
    (unless stages
      (throw 'invalid nil))
    (let ((remaining (copy-sequence bodies))
          stage-results)
      (dolist (stage stages)
        (let* ((name (plist-get stage :stage))
               (expected (plist-get stage :body))
               (predicate (plist-get stage :predicate)))
          (unless (and remaining
                       (equal (car remaining) expected)
                       (or (not predicate)
                           (funcall predicate (car remaining))))
            (throw 'invalid nil))
          (pop remaining)
          (let (duplicates)
            (while (and remaining (equal (car remaining) expected))
              (push (pop remaining) duplicates))
            (push (list :stage name
                        :body expected
                        :duplicate-count (length duplicates)
                        :duplicates (nreverse duplicates))
                  stage-results))))
      (when remaining
        (throw 'invalid nil))
      (setq stage-results (nreverse stage-results))
      (let ((stage-event-counts (make-hash-table :test #'equal))
            (untagged-events 0)
            (retry-valid-p t)
            (duplicate-count
             (apply #'+ (or (mapcar (lambda (stage)
                                      (plist-get stage :duplicate-count))
                                    stage-results)
                            '(0)))))
        (dolist (event retry-events)
          (let ((stage-key
                 (e-live-e2e--stage-key
                  (e-live-e2e--retry-event-stage event))))
            (if stage-key
                (if (seq-some
                     (lambda (stage)
                       (equal stage-key
                              (e-live-e2e--stage-key
                               (plist-get stage :stage))))
                     stage-results)
                    (puthash stage-key
                             (1+ (gethash stage-key stage-event-counts 0))
                             stage-event-counts)
                  (setq retry-valid-p nil))
              (setq untagged-events (1+ untagged-events)))))
        ;; Every captured retry duplicate must have exactly one retry event;
        ;; neither one-over events nor unaccounted duplicates are acceptable.
        (unless (= duplicate-count (length retry-events))
          (setq retry-valid-p nil))
        (dolist (stage stage-results)
          (let* ((stage-key
                  (e-live-e2e--stage-key (plist-get stage :stage)))
                 (duplicates (plist-get stage :duplicate-count))
                 (tagged (gethash stage-key stage-event-counts 0))
                 (needed (- duplicates tagged)))
            (when (or (> tagged duplicates)
                      (> (max 0 needed) untagged-events))
              (setq retry-valid-p nil))
            (when (> needed 0)
              (setq untagged-events (- untagged-events needed)))))
        (when (and retry-valid-p (= untagged-events 0))
          (list :valid-p t
                :stages stage-results
                :logical-stage-count (length stage-results)
                :logical-curation-count
                (cl-count-if
                 (lambda (stage)
                   (equal (e-live-e2e--stage-key (plist-get stage :stage))
                          "curation-ack"))
                 stage-results)
                :duplicate-count duplicate-count
                :retry-count (length retry-events)))))))

(defun e-live-e2e--captured-turn-extras-valid-p
    (initial-body extra-bodies retry-events)
  "Validate same-turn EXTRA-BODIES after INITIAL-BODY.
The wrapper retains the legacy initial-or-ack shape while applying the
stage-aware logical retry validator.  Full tool turns provide every stage
directly so an acknowledgement cannot stand in for an ordinary continuation."
  (let ((ack-bodies
         (seq-filter #'e-live-e2e--context-curation-ack-body-p extra-bodies)))
    (plist-get
     (e-live-e2e--captured-turn-stage-cardinality
      (cons initial-body extra-bodies)
      (if ack-bodies
          (list (list :stage 'initial :body initial-body)
                (list :stage 'curation-ack :body (car ack-bodies)
                      :predicate
                      #'e-live-e2e--context-curation-ack-body-p))
        (list (list :stage 'initial :body initial-body)))
      retry-events)
     :valid-p)))

(ert-deftest e-live-e2e-test-adoption-composition-uses-turn-boundary ()
  "Select the composed request before a later continuation body."
  (let* ((durable "DURABLE-ADOPTION")
         (first-body '(:input ((:role user :content "first turn"))))
         (composed-body
          '(:input ((:role system :content "DURABLE-ADOPTION"))))
         (continuation-body
          '(:input ((:type "function_call" :name "context-curate"
                     :arguments "RAW-EPHEMERAL")
                    (:type "function_call_output"
                     :output "RAW-EPHEMERAL")
                    (:type "message"
                     :content "[ephemeral context source 1]"))))
         (request-bodies
          (list (list :body continuation-body)
                (list :body composed-body)
                (list :body first-body)
                (list :body '(:input ((:role assistant :content "first"))))))
         (body (e-live-e2e--captured-body-at-boundary request-bodies 2))
         (input (plist-get body :input))
         (printed (prin1-to-string input)))
    (should (equal body composed-body))
    (should (string-match-p (regexp-quote durable) printed))
    (should-not (string-match-p "\\[ephemeral context source [0-9]+" printed))
    (should-not (string-match-p
                 "tool-call-id\\|function_call\\|provider-replay-item"
                 printed))))

(ert-deftest e-live-e2e-test-cache-turn-boundaries-allow-only-curation-acks ()
  "Turn starts remain selectable when a reserved curation ack follows them."
  (let* ((first-body '(:input ((:role user :content "first"))))
         (second-body '(:input ((:role user :content "second"))))
         (ack-body
          '(:input ((:type "function_call" :name "context-curate"
                     :call_id "curation-call")
                    (:type "function_call_output" :call_id "curation-call"
                     :output ""))))
         (unrelated-body
          '(:input ((:type "function_call" :name "e2e_deterministic"
                     :call_id "unrelated-call"))))
         ;; Capture is newest-first, while turn ranges are chronological.
         (request-bodies
          (list (list :body ack-body)
                (list :body second-body)
                (list :body first-body)))
         (second-turn-bodies
          (e-live-e2e--captured-bodies-between request-bodies 1 3)))
    (should (equal
             (car (e-live-e2e--captured-bodies-between request-bodies 0 1))
             first-body))
    (should (equal (car second-turn-bodies) second-body))
    (should (equal (cadr second-turn-bodies) ack-body))
    (should (e-live-e2e--context-curation-ack-body-p ack-body))
    (should-not (e-live-e2e--context-curation-ack-body-p unrelated-body))))

(ert-deftest e-live-e2e-test-cache-turn-boundaries-require-retry-evidence ()
  "Identical extra bodies require a matching durable retry event."
  (let* ((initial-body '(:input ((:role user :content "same"))))
         (duplicate-body (copy-tree initial-body))
         (mutated-body '(:input ((:role user :content "changed"))))
         (retry-event '(:turn-id "turn-1" :event-type turn-retrying)))
    (should (e-live-e2e--captured-turn-extras-valid-p
             initial-body (list duplicate-body) (list retry-event)))
    (should-not (e-live-e2e--captured-turn-extras-valid-p
                 initial-body (list duplicate-body) nil))
    (should-not (e-live-e2e--captured-turn-extras-valid-p
                 initial-body (list mutated-body) (list retry-event)))))

(ert-deftest e-live-e2e-test-captured-turn-stage-cardinality-is-logical ()
  "Retries preserve one logical request or acknowledgement per causal stage."
  (let* ((initial-body '(:input ((:role user :content "initial"))))
         (followup-body
          '(:input ((:type "function_call_output"
                     :call_id "ordinary-call" :output "TOOL"))))
         (ack-body
          '(:input ((:type "function_call" :name "context-curate"
                     :call_id "curation-call")
                    (:type "function_call_output" :call_id "curation-call"
                     :output ""))))
         (different-ack-body
          '(:input ((:type "function_call" :name "context-curate"
                     :call_id "other-curation")
                    (:type "function_call_output" :call_id "other-curation"
                     :output ""))))
         (stages
          (list (list :stage 'initial :body initial-body)
                (list :stage 'tool-followup :body followup-body)
                (list :stage 'curation-ack :body ack-body
                      :predicate #'e-live-e2e--context-curation-ack-body-p)))
         (retry-event '(:stage curation-ack)))
    (let ((result
           (e-live-e2e--captured-turn-stage-cardinality
            (list initial-body followup-body ack-body ack-body)
            stages
            (list retry-event))))
      (should (plist-get result :valid-p))
      (should (= (plist-get result :logical-curation-count) 1))
      (should (= (plist-get result :duplicate-count) 1))
      (should (= (plist-get result :retry-count) 1)))
    ;; Production retry events are currently untagged; exact body equality and
    ;; one-for-one cardinality still make the duplicate bounded and logical.
    (should (e-live-e2e--captured-turn-stage-cardinality
             (list initial-body initial-body followup-body ack-body)
             stages
             (list '(:turn-id "turn-1"))))
    ;; Two copied acknowledgements with one retry event are one-over and fail.
    (should-not
     (e-live-e2e--captured-turn-stage-cardinality
      (list initial-body followup-body ack-body ack-body ack-body)
      stages
      (list retry-event)))
    ;; An explicit retry attributed to another stage cannot authorize the ack.
    (should-not
     (e-live-e2e--captured-turn-stage-cardinality
      (list initial-body followup-body ack-body ack-body)
      stages
      (list '(:stage tool-followup))))
    ;; A second distinct curation call is not a retry duplicate.
    (should-not
     (e-live-e2e--captured-turn-stage-cardinality
      (list initial-body followup-body ack-body different-ack-body)
      stages
      nil))
    ;; A reserved acknowledgement cannot occupy the ordinary-tool stage or a
    ;; no-curation first/warm turn.
    (should-not
     (e-live-e2e--captured-turn-stage-cardinality
      (list initial-body ack-body)
      (list (list :stage 'initial :body initial-body))
      nil))))

(ert-deftest e-live-e2e-test-adoption-carrier-localizes-to-source-continuation ()
  "Only the linked ordinary result continuation can satisfy the carrier gate."
  (let* ((sentinel "ADOPTION-SOURCE")
         (carrier-tool (e-openai-responses-context-curation-tool-definition))
         (call (list :type "function_call" :name "e2e_deterministic"
                     :call_id "ordinary-call"))
         (marker
          (list :type "message" :role "developer"
                :content
                (vector
                 (list :type "input_text"
                       :text "[ephemeral context source 1, ~5 tokens, erase-eligible]"))))
         (output
          (list :type "function_call_output"
                :call_id "ordinary-call" :output sentinel))
         (initial
          (list :tools (list carrier-tool)
                :input (list call)))
         (source-without-carrier
          (list :tools nil :input (list marker output)))
         (source-with-carrier
          (list :reasoning '(:effort "medium" :summary "auto")
                :tools (list carrier-tool)
                :input (list marker output)))
         (curation-call
          (list :type "function_call" :name "context-curate"
                :call_id "curation-call"))
         (curation-output
          (list :type "function_call_output" :call_id "curation-call"
                :output ""))
         (reserved-ack
          (list :tools (list carrier-tool)
                :input (list call marker output curation-call curation-output)))
         (other-with-carrier
          (list :tools (list carrier-tool)
                :input '((:type "message"
                          :content [(:type "input_text"
                                     :text "ordinary continuation")])))))
    ;; A carrier on the initial request is irrelevant when the source-bearing
    ;; continuation does not carry it.
    (should (equal
             (e-live-e2e--adoption-source-bearing-request-index
              (list initial source-without-carrier) sentinel)
             1))
    (should-not (seq-some (lambda (tool)
                            (equal (plist-get tool :name) "context-curate"))
                          (append (plist-get source-without-carrier :tools)
                                  nil)))
    ;; The same causal continuation clears the carrier gate when the carrier
    ;; is present there, regardless of an unrelated later declaration.
    (should (equal
             (e-live-e2e--adoption-source-bearing-request-index
              (list initial source-with-carrier other-with-carrier) sentinel)
             1))
    (should
     (and (e-live-e2e--context-curation-carrier-p source-with-carrier)
          (e-live-e2e--responses-reasoning-auto-p source-with-carrier)))
    (let ((wrong-affordance (copy-tree source-with-carrier)))
      (plist-put (car (plist-get wrong-affordance :tools))
                 :description
                 "Partition every presented source label exactly once.")
      (should-not (e-live-e2e--context-curation-carrier-p wrong-affordance)))
    ;; A later reserved acknowledgement replays the ordinary bundle, but is
    ;; not another source-bearing continuation.
    (should (equal
             (e-live-e2e--adoption-source-bearing-request-index
              (list initial source-with-carrier reserved-ack) sentinel)
             1))
    ;; Wrong linkage and two source-bearing continuations are not safe
    ;; evidence for model selection.
    (should-not
     (e-live-e2e--adoption-source-bearing-request-index
      (list initial
            (list :tools (list (list :name "context-curate"))
                  :input (list marker
                                (plist-put (copy-sequence output)
                                           :call_id "wrong-call"))))
      sentinel))
    (should-not
     (e-live-e2e--adoption-source-bearing-request-index
      (list initial source-with-carrier source-with-carrier)
      sentinel))))

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
          cache-result result adoption-result composition-result failure-stage
          adoption-disposition scenario-prompt-identity affordance-revision
          presentation-revision adoption-gates adoption-dependency-identity
          erase-adoption-gates
          evidence-schema-revision reasoning-effort reasoning-summary
          probe-request-identity dependency-identity returned-summary-presence)
  "Return one bounded identity-complete external evidence RECORD.
REQUEST-BODIES are hashed and reduced to shapes; no prompt, token, auth header,
or response body is emitted.  REQUEST-METADATA and USAGE-PAYLOADS are ordered
lists matching those requests where available.  Each body receives one shape
by request index; absent metadata leaves only its metadata-derived fields
unavailable.  ERASE-ADOPTION-GATES selects the autonomous-erasure classifier;
otherwise ADOPTION-GATES retains the positive-adoption classifier."
  (let* ((request-bodies (or request-bodies nil))
         (request-metadata (or request-metadata nil))
         (diagnostics
          (mapcar #'e-live-e2e--request-diagnostics request-metadata))
         (identities
          (cl-loop for body in request-bodies
                   for index from 1
                   collect
                   (e-live-e2e--captured-request-identity
                    profile body (nth (1- index) request-metadata) model)))
         (identity-fields '(:base-url :model :store :transport
                            :native-requester))
         (identity-complete-p
          (and identities
               (cl-every
                (lambda (identity)
                  (and (cl-every (lambda (field)
                                   (plist-get identity field))
                                 identity-fields)
                       ;; A declared/profile URL is useful context, but it
                       ;; cannot make a body-only partial capture complete.
                       (plist-get identity :endpoint-captured-p)
                       ;; Likewise, the caller's expected model is not wire
                       ;; evidence when both body and metadata omit it.
                       (plist-get identity :model-captured-p)
                       (plist-get identity :store-present)))
                identities)))
         (identity-drift
          (when (and identity-complete-p (cdr identities))
            (let ((first (car identities)))
              (cl-loop for identity in (cdr identities)
                       for index from 2
                       append
                       (cl-loop for field in identity-fields
                                unless (equal (plist-get first field)
                                              (plist-get identity field))
                                collect (list :request-index index
                                              :field field
                                              :first (plist-get first field)
                                              :effective (plist-get identity field)))))))
         (declaration-mismatches
          (and identities
               (e-live-e2e--identity-mismatches profile identities)))
         (identity-mismatches (append identity-drift declaration-mismatches))
         (identity-result
          (cond
           ((not identity-complete-p) "unavailable")
           (identity-mismatches "invalid")
           (t "pass")))
         (adoption-classification
          (cond
           (erase-adoption-gates
            (apply #'e-live-e2e--classify-autonomous-erase
                   (append (list :identity-result identity-result)
                           erase-adoption-gates)))
           (adoption-gates
            (apply #'e-live-e2e--classify-autonomous-adoption
                   (append (list :identity-result identity-result)
                           adoption-gates)))))
         (first-identity (car identities))
         (effective-base-url
          (or (plist-get first-identity :base-url) "unavailable"))
         (effective-endpoint
          (or (plist-get first-identity :endpoint) "unavailable"))
         (effective-model
          (or (plist-get first-identity :model) "unavailable"))
         (effective-store
          (and (plist-get first-identity :store-present)
               (format "%s" (plist-get first-identity :store))))
         (effective-transport
          (or (plist-get first-identity :transport) "unavailable"))
         (effective-requester
          (or (plist-get first-identity :native-requester) "unavailable"))
         (shapes
          (cl-loop for body in request-bodies
                   for index from 1
                   for metadata =
                   (e-live-e2e--request-diagnostics
                    (nth (1- index) request-metadata))
                   for identity = (nth (1- index) identities)
                   for websocket-p
                   = (equal (plist-get identity :transport)
                            "responses-websocket")
                   for shape = (e-live-e2e--request-shape body)
                   collect
                   (append shape
                           (list :request-index index
                                 :effective-endpoint
                                 (or (plist-get identity :endpoint)
                                     "unavailable")
                                 :effective-base-url
                                 (or (plist-get identity :base-url)
                                     "unavailable")
                                 :effective-model
                                 (or (plist-get identity :model)
                                     "unavailable")
                                 :effective-store-mode
                                 (and (plist-get identity :store-present)
                                      (format "%s"
                                              (plist-get identity :store)))
                                 :effective-transport
                                 (or (plist-get identity :transport)
                                     "unavailable")
                                 :effective-native-requester
                                 (or (plist-get identity :native-requester)
                                     "unavailable")
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
          (when (cl-some (lambda (identity)
                          (equal (plist-get identity :transport)
                                 "responses-websocket"))
                        identities)
            (delete-dups
             (delq nil
                   (cl-loop for identity in identities
                            for metadata in diagnostics
                            when (equal (plist-get identity :transport)
                                        "responses-websocket")
                            collect (plist-get metadata
                                                :websocket-connection-id))))))
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
         (prompt-layout-revision
          (or (seq-some (lambda (metadata)
                          (plist-get metadata :prompt-layout-revision))
                        diagnostics)
              "unavailable"))
         (elapsed (and started-at ended-at (- ended-at started-at)))
         (scenario-result
          (or (and (equal identity-result "pass")
                   adoption-classification
                   (plist-get adoption-classification :result))
              result
              "unavailable"))
         (record-result
          (if (and adoption-classification
                   (not (equal identity-result "pass")))
              (if (equal identity-result "invalid")
                  "identity-invalid"
                "identity-unavailable")
            (if (and (equal scenario-result "pass")
                     (not (equal identity-result "pass")))
                (if (equal identity-result "invalid")
                    "identity-invalid"
                  "identity-unavailable")
              scenario-result))))
    (let ((record
           (list :evidence-schema-revision
                 (or evidence-schema-revision
                     e-live-e2e--external-evidence-schema-revision)
                 :scenario (format "%s" scenario)
                 :provider-id (format "%s" provider)
                 :profile-id profile-name
                 :base-url-identity effective-base-url
                 :endpoint-identity effective-endpoint
                 :transport effective-transport
                 :store-mode (or effective-store "unavailable")
                 :native-requester effective-requester
                 :model-id effective-model
                 :declared-base-url (or (plist-get profile :base-url)
                                        "unavailable")
                 :declared-model
                 (or (plist-get profile :model)
                     (plist-get profile :default-model)
                     "unavailable")
                 :declared-store-mode
                 (if (plist-member profile :response-store)
                     (format "%s" (plist-get profile :response-store))
                   "unavailable")
                 :identity-result identity-result
                 :identity-mismatches identity-mismatches
                 :scenario-result scenario-result
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
                 :result record-result)))
      (setq record
         (if (or evidence-schema-revision reasoning-effort reasoning-summary
                    probe-request-identity dependency-identity
                    returned-summary-presence)
                (append record
                        (list :reasoning-effort
                              (or reasoning-effort "unavailable")
                              :reasoning-summary
                              (or reasoning-summary "unavailable")
                              :probe-request-identity
                              (or probe-request-identity "unavailable")
                              :dependency-identity
                              (or dependency-identity "unavailable")
                              :returned-summary-presence
                              (or returned-summary-presence "unavailable")))
              record))
      (if (or adoption-result composition-result failure-stage
              adoption-gates erase-adoption-gates
              adoption-disposition
              scenario-prompt-identity
              affordance-revision presentation-revision
              adoption-dependency-identity)
          (append record
                  (list :adoption-result
                        (or (plist-get adoption-classification
                                       :adoption-result)
                            adoption-result
                            "unavailable")
                        :composition-result
                        (or (plist-get adoption-classification
                                       :composition-result)
                            composition-result
                            "unavailable")
                        :failure-stage
                        (or (plist-get adoption-classification :failure-stage)
                            failure-stage
                            "none")
                        :adoption-disposition
                        (or (plist-get adoption-classification
                                       :adoption-disposition)
                            adoption-disposition
                            "unavailable")
                        :scenario-prompt-identity
                        (or scenario-prompt-identity "unavailable")
                        :affordance-revision
                        (or affordance-revision "unavailable")
                        :presentation-revision
                        (or presentation-revision "unavailable")
                        :adoption-dependency-identity
                        (or adoption-dependency-identity
                            (e-live-e2e--adoption-dependency-identity)
                            "unavailable")))
        record))))

(defun e-live-e2e--report-external-evidence (record)
  "Emit one machine-readable bounded external evidence RECORD."
  (message "E88 external evidence: %s"
           (json-encode (e-live-e2e--json-plist record))))

(defun e-live-e2e--cancel-newest-request (request-handles)
  "Cancel the newest captured request in newest-first REQUEST-HANDLES."
  (when-let* ((request (car request-handles)))
    (e-backend-cancel-request request)))

(cl-defun e-live-e2e--run-external-scenario
    (&key scenario provider profile model timeout started-at capture thunk cancel
          evidence-schema-revision profile-function)
  "Run THUNK and emit exactly one bounded evidence record.
CAPTURE is called after THUNK settles and returns the best state captured so
far as a plist.  The original error or ERT skip is re-signalled after the
record is emitted; configuration, semantic, cache, and overall results stay
separate in that record.  CANCEL runs in an outer cleanup boundary after
finalization, including when the original condition is re-signalled."
  (let (condition value configuration-unavailable)
    (unwind-protect
        (progn
          (condition-case caught
              (setq value
                    (progn
                      (when profile-function
                        (condition-case profile-caught
                            (setq profile (funcall profile-function))
                          (error
                           (setq configuration-unavailable t)
                           (signal (car profile-caught)
                                   (cdr profile-caught)))))
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
                 (scenario-result-override
                  (and (or (null condition)
                           (eq condition-type 'ert-test-failed))
                       (plist-get state :scenario-result)))
                 (cache-result (or (plist-get state :cache-result)
                                   "unavailable"))
                 (terminal-result (plist-get state :terminal-result))
                 (semantic-result
                  (cond
                   (configuration-unavailable "unavailable")
                   ((and (eq condition-type 'ert-test-failed)
                         (plist-member state :composition-result))
                    (or (plist-get state :semantic-result) "failure"))
                   ((and (eq condition-type 'ert-test-failed)
                         (equal cache-result "product-contract-failure"))
                    (or (plist-get state :semantic-result) "pass"))
                   ((eq condition-type 'ert-test-failed) "failure")
                   (t (or (plist-get state :semantic-result) "unavailable"))))
                 (result
                 (cond
                   (configuration-unavailable "configuration-unavailable")
                   (terminal-result terminal-result)
                   (scenario-result-override scenario-result-override)
                   ((eq condition-type 'e-live-e2e-scenario-timeout)
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
                :evidence-schema-revision
                (or (plist-get state :evidence-schema-revision)
                    evidence-schema-revision)
                :reasoning-effort (plist-get state :reasoning-effort)
                :reasoning-summary (plist-get state :reasoning-summary)
                :probe-request-identity
                (plist-get state :probe-request-identity)
                :dependency-identity (plist-get state :dependency-identity)
                :returned-summary-presence
                (plist-get state :returned-summary-presence)
                :request-bodies (plist-get state :request-bodies)
                :request-metadata (plist-get state :request-metadata)
                :usage-payloads (plist-get state :usage-payloads)
                :timeout timeout
                :started-at started-at
                :ended-at (float-time)
                :semantic-result semantic-result
                :cache-result cache-result
                :result result
                :adoption-result (plist-get state :adoption-result)
                :composition-result (plist-get state :composition-result)
                :failure-stage (plist-get state :failure-stage)
                :adoption-disposition (plist-get state :adoption-disposition)
                :scenario-prompt-identity
                (plist-get state :scenario-prompt-identity)
                :affordance-revision (plist-get state :affordance-revision)
                :presentation-revision
                (plist-get state :presentation-revision)
                :adoption-gates
                (plist-get state :adoption-gates)
                :erase-adoption-gates
                (plist-get state :erase-adoption-gates)))))
          (if condition
              (signal (car condition) (cdr condition))
            value))
      (when cancel
        (ignore-errors (funcall cancel))))))

(defun e-live-e2e--prompt-batch-before-deadline
    (harness session-id prompt deadline)
  "Run PROMPT with the remaining portion of a scenario DEADLINE."
  (let ((remaining (- deadline (float-time))))
    (when (<= remaining 0)
      (signal 'e-live-e2e-scenario-timeout (list :deadline deadline)))
    (condition-case caught
        (e-chat-sql-e2e-prompt-batch harness session-id prompt remaining)
      (error
       (let ((entry (gethash session-id (e-harness-active-turns harness))))
         (if (and (>= (float-time) deadline)
                  (eq (plist-get entry :status) 'running))
             (signal 'e-live-e2e-scenario-timeout
                     (list :deadline deadline :cause caught))
           (signal (car caught) (cdr caught))))))))

(defconst e-live-e2e--adoption-negative-terms
  '("context-curate" "curate" "keep" "summary" "summaries"
    "curation" "drop" "erase" "retain" "source" "label" "numeric-label"
    "carrier" "reasoning" "schema" "argument")
  "Case-insensitive words forbidden in the autonomous-adoption prompt.")

(defconst e-live-e2e--adoption-freshness-seconds (* 7 24 60 60)
  "Freshness window inherited by autonomous-adoption evidence.")

(defun e-live-e2e--autonomous-adoption-prompt (&optional tool-name)
  "Return the naturalistic adoption prompt naming ordinary TOOL-NAME only."
  (format
   "Use the %s lookup tool exactly once to obtain the current adoption sentinel. I will ask about that value in my next message. Do not reveal it yet; reply only READY."
   (or tool-name "e2e_deterministic")))

(defun e-live-e2e--adoption-prompt-violations (prompt)
  "Return forbidden adoption-prompt terms found in substituted PROMPT."
  (let ((case-fold-search t)
        violations)
    (dolist (term e-live-e2e--adoption-negative-terms)
      (when (string-match-p
             (regexp-quote term)
             (or prompt ""))
        (push term violations)))
    ;; A numeric source reference is a label-shaped reference, not a digit in
    ;; the ordinary tool's name.  Keep the check strict enough for markers,
    ;; "source 1", and "label:1" without rejecting the tool name.
    (when (string-match-p
           "\\(?:\\_<source\\_>\\|\\_<label\\_>\\)[[:space:]#:=/-]*[0-9]+\\|\\[[[:space:]]*[0-9]+[[:space:]]*[,\\]]"
           (or prompt ""))
      (push "numeric-source-reference" violations))
    (nreverse violations)))

(defun e-live-e2e--adoption-record-preserves-sentinel-p (record sentinel)
  "Return non-nil when one curation ITEM in RECORD preserves SENTINEL."
  (and (stringp sentinel)
       (seq-some
        (lambda (item)
          (pcase (plist-get item :kind)
            ((or 'exact "exact")
             (equal (plist-get item :value) sentinel))
            ((or 'summary "summary")
             (and (stringp (plist-get item :text))
                  (string-match-p (regexp-quote sentinel)
                                  (plist-get item :text))))))
        (append (plist-get record :items) nil))))

(defun e-live-e2e--adoption-record-disposition (record sentinel)
  "Return the preserving disposition in RECORD for SENTINEL, or nil."
  (when-let* ((item
              (seq-find
               (lambda (item)
                 (pcase (plist-get item :kind)
                   ((or 'exact "exact")
                    (equal (plist-get item :value) sentinel))
                   ((or 'summary "summary")
                    (and (stringp (plist-get item :text))
                         (string-match-p (regexp-quote sentinel)
                                         (plist-get item :text))))))
               (append (plist-get record :items) nil))))
    (format "%s" (plist-get item :kind))))

(defun e-live-e2e--adoption-positive-effect-valid-p
    (arguments source-count record sentinel)
  "Return non-nil for an explicit positive adoption disposition.
The disposition must retain SENTINEL by an exact or summary item.  Other
presented labels may be omitted under the optional carrier contract.  Core
remains the authority for strict shape and label validation; this predicate
only joins that result to the content-free evidence record."
  (condition-case nil
      (let* ((normalized
              (e-context-lifetime-normalize-curation-disposition
               arguments source-count))
             (retained-p
              (or (> (length (plist-get normalized :keep)) 0)
                  (> (length (plist-get normalized :summaries)) 0)))
             (disposition
              (and record
                   (e-live-e2e--adoption-record-disposition
                    record sentinel))))
        (and (integerp source-count)
             (> source-count 0)
             retained-p
             (member disposition '("exact" "summary"))
             (e-live-e2e--adoption-record-preserves-sentinel-p
              record sentinel)))
    (error nil)))

(defun e-live-e2e--adoption-erase-effect-valid-p (arguments source-count)
  "Return non-nil for one explicit erase-only disposition.
Core validates the carrier shape and source membership; this predicate joins
that result to the negative-adoption contract, which accepts exactly one
submitted erase label and no retained exact or summary labels."
  (condition-case nil
      (let* ((normalized
              (e-context-lifetime-normalize-curation-disposition
               arguments source-count))
             (erase (plist-get normalized :erase))
             (label (and (= (length erase) 1) (aref erase 0))))
        (and (integerp source-count)
             (> source-count 0)
             (= (length (plist-get normalized :keep)) 0)
             (= (length (plist-get normalized :summaries)) 0)
             (= (length erase) 1)
             (integerp label)
             (<= 1 label source-count)))
    (error nil)))

(defun e-live-e2e--autonomous-erase-prompt (&optional tool-name)
  "Return the naturalistic negative-adoption prompt for TOOL-NAME.
The prompt describes current-answer use plus no future lookup reminder, and
deliberately contains no carrier, curation, source-label, reasoning, schema,
or argument vocabulary."
  (format
   "Call the %s lookup tool exactly once. The returned sentinel is useful only for this answer, and after this answer I do not want any reminder that the lookup happened carried into later turns. Reverse every character of the sentinel exactly and reply with only that transformed value and no extra text."
   (or tool-name "e2e_deterministic")))

(defun e-live-e2e--receipt-field-keys (receipt)
  "Return the sorted keyword field set of RECEIPT, or nil when malformed.
The field set is kept separate from values so an omitted optional field cannot
compare equal to a present nil field after JSON replay."
  (let ((tail receipt)
        keys
        (valid t))
    (while (and valid tail)
      (if (and (consp tail)
               (keywordp (car tail))
               (consp (cdr tail)))
          (progn
            (push (car tail) keys)
            (setq tail (cddr tail)))
        (setq valid nil)))
    (when (and valid (null tail))
      (sort keys (lambda (left right)
                   (string< (symbol-name left) (symbol-name right)))))))

(defun e-live-e2e--normalize-reopened-receipt (receipt)
  "Normalize only persisted enum symbols in RECEIPT for JSON replay.
All other values remain exact, including identities, purpose, URI, and any
future receipt fields.  Nil enum values remain nil so field presence is tested
separately by the equality predicate."
  (let ((normalized (copy-tree receipt)))
    (dolist (key '(:status :details-lifetime))
      (when (plist-member normalized key)
        (let ((value (plist-get normalized key)))
          (when (and value (symbolp value))
            (setq normalized
                  (plist-put normalized key (symbol-name value)))))))
    normalized))

(defun e-live-e2e--reopened-receipt-equal-p (original-event reopened-event)
  "Return non-nil when REOPENED-EVENT preserves ORIGINAL-EVENT's receipt.
Compare durable payload receipts directly, allowing only the three enum
values that JSON replay represents as strings to differ from live symbols.
Every key, value, identity, optional-field presence, purpose, and URI remains
part of the comparison."
  (let* ((original-receipt
          (plist-get (plist-get original-event :payload) :receipt))
         (reopened-receipt
          (plist-get (plist-get reopened-event :payload) :receipt))
         (original-keys (e-live-e2e--receipt-field-keys original-receipt))
         (reopened-keys (e-live-e2e--receipt-field-keys reopened-receipt))
         (original-normalized
          (e-live-e2e--normalize-reopened-receipt original-receipt))
         (reopened-normalized
          (e-live-e2e--normalize-reopened-receipt reopened-receipt)))
    (and original-keys
         reopened-keys
         (equal original-keys reopened-keys)
         (equal
          (mapcar (lambda (key)
                    (list key (plist-get original-normalized key)))
                  original-keys)
          (mapcar (lambda (key)
                    (list key (plist-get reopened-normalized key)))
                  reopened-keys)))))

(defun e-live-e2e--ordinary-tool-receipt-prerequisite
    (harness session-id tool-finishes)
  "Return the direct receipt prerequisite for TOOL-FINISHES, or nil.
The ordinary fixture must have exactly one durable tool-finished event with a
string call identity, a live details URI, and a matching receipt already
visible through the harness projection.  This prerequisite is deliberately
separate from the later erasure-persistence gate."
  (when (= (length tool-finishes) 1)
    (let* ((event (car tool-finishes))
           (payload (plist-get event :payload))
           (receipt (plist-get payload :receipt))
           (tool-call-id (and (listp receipt)
                              (plist-get receipt :tool-call-id)))
           (details-uri (and (listp receipt)
                             (plist-get receipt :details-uri)))
           (projection
            (e-harness-base-receipt-projection harness session-id)))
      (when (and (stringp tool-call-id)
                 (stringp details-uri)
                 (e-session-tmp-reference-available-p
                  harness session-id details-uri)
                 (= (plist-get projection :selected-count) 1)
                 (= (plist-get projection :total-count) 1)
                 (= (plist-get projection :omitted-count) 0)
                 (seq-some
                  (lambda (view)
                    (equal (plist-get view :tool-call-id) tool-call-id))
                  (plist-get projection :receipts)))
        (list :event event
              :receipt receipt
              :tool-call-id tool-call-id
              :details-uri details-uri
              :projection projection)))))

(cl-defun e-live-e2e--classify-autonomous-erase
    (&key identity-result ordinary-tool-p prompt-control-p carrier-p
          current-answer-p effect-present-p explicit-erasure-p
          audit-linked-p consumed-p no-promotion-p erasure-persisted-p
          receipt-prerequisite-p receipt-erasure-id-p receipt-suppressed-p
          details-preserved-p reopen-audit-p reopen-erasure-p
          reopen-no-promotion-p reopen-receipt-p)
  "Return bounded classification for explicit negative adoption.
An absent effect is a product-contract observation, while a wrong current
answer is semantic failure.  A valid explicit erasure requires the audit,
  consumption, persistence, receipt-suppression, details-preservation, and
  reopen gates before composition can pass."
  (if (not (equal identity-result "pass"))
      (list :adoption-result "unavailable"
            :composition-result "unavailable"
            :failure-stage "none"
            :adoption-disposition "unavailable"
            :result "unavailable")
    (cond
     ((not ordinary-tool-p)
      (list :adoption-result "unavailable"
            :composition-result "failure"
            :failure-stage "ordinary-tool"
            :adoption-disposition "unavailable"
            :result "semantic-failure"))
     ((not prompt-control-p)
      (list :adoption-result "unavailable"
            :composition-result "failure"
            :failure-stage "prompt-control"
            :adoption-disposition "unavailable"
            :result "semantic-failure"))
     ((not carrier-p)
      (list :adoption-result "unavailable"
            :composition-result "failure"
            :failure-stage "carrier"
            :adoption-disposition "unavailable"
            :result "semantic-failure"))
     ((not current-answer-p)
      (list :adoption-result "unavailable"
            :composition-result "failure"
            :failure-stage "current-answer"
            :adoption-disposition "unavailable"
            :result "semantic-failure"))
     ((not effect-present-p)
      (list :adoption-result "product-contract-failure"
            :composition-result "pass"
            :failure-stage "model-selection"
            :adoption-disposition "unavailable"
            :result "product-contract-failure"))
     ((not explicit-erasure-p)
      (list :adoption-result "product-contract-failure"
            :composition-result "pass"
            :failure-stage "model-selection"
            :adoption-disposition "unavailable"
            :result "product-contract-failure"))
     ((not (and audit-linked-p consumed-p))
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "commit"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not no-promotion-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "no-promotion"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not receipt-prerequisite-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "receipt-prerequisite"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not erasure-persisted-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "erasure-persisted"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not receipt-erasure-id-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "receipt-erasure-id"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not receipt-suppressed-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "receipt-suppressed"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not details-preserved-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "details-preserved"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not reopen-audit-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "reopen-audit"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not reopen-erasure-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "reopen-erasure"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not reopen-no-promotion-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "reopen-no-promotion"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     ((not reopen-receipt-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "reopen-receipt"
            :adoption-disposition "erase"
            :result "semantic-failure"))
     (t
      (list :adoption-result "pass"
            :composition-result "pass"
            :failure-stage "none"
            :adoption-disposition "erase"
            :result "pass")))))

(defun e-live-e2e--adoption-audit-linked-p
    (store session-id record sink-events)
  "Return non-nil when RECORD has one exact response/control link.
The context-curation response control and entry are durable; the consumed-frame
event is a public sink event and is therefore checked in SINK-EVENTS, not the
session activity ledger."
  (let* ((response-entry-id (plist-get record :response-entry-id))
         (curation-id (plist-get record :id))
         (durable-events (e-session-local-activity-events store session-id))
         (controls
          (seq-filter
           (lambda (event)
             (eq (plist-get event :event-type)
                 'context-curation-response))
           durable-events))
         (control (car controls))
         (control-payload (and control (plist-get control :payload)))
         (consumed
          (seq-find
           (lambda (event)
             (and (eq (plist-get event :type)
                      'context-frame-consumed)
                  (equal (plist-get (plist-get event :payload)
                                    :response-entry-id)
                         response-entry-id)
                  (equal (plist-get (plist-get event :payload)
                                    :curation-ids)
                         (list curation-id))))
           sink-events)))
    (and (stringp response-entry-id)
         (stringp curation-id)
         (= (length controls) 1)
         (equal (plist-get control :id) response-entry-id)
         (equal (plist-get control-payload :response-entry-id)
                response-entry-id)
         (e-session-local-entry-by-id store session-id response-entry-id)
         consumed)))

(defun e-live-e2e--autonomous-erase-audit-links (store session-id)
  "Return bounded control/consumption identities for an erase response.
The curation response control and consumed-frame event are durable activity;
the nil curation-id list is intentional for an erase disposition.  Return
only opaque identities so callers cannot accidentally put event content into
external evidence."
  (let* ((events (e-session-local-activity-events store session-id))
         (controls
          (seq-filter
           (lambda (event)
             (eq (plist-get event :event-type)
                 'context-curation-response))
           events))
         (control (car controls))
         (control-payload (and control (plist-get control :payload)))
         (response-entry-id (and control (plist-get control :id)))
         (consumed-events
          (seq-filter
           (lambda (event)
             (let ((payload (plist-get event :payload)))
               (and (eq (plist-get event :event-type)
                        'context-frame-consumed)
                    (equal (plist-get payload :response-entry-id)
                           response-entry-id)
                    (null (plist-get payload :curation-ids))
                    (equal (plist-get event :turn-id)
                           (and control (plist-get control :turn-id))))))
           events))
         (consumed (car consumed-events))
         (consumed-payload (and consumed (plist-get consumed :payload)))
         (frame-id (and consumed (plist-get consumed-payload :frame-id))))
    (when (and (= (length controls) 1)
               (= (length consumed-events) 1)
               (stringp response-entry-id)
               (equal (plist-get control-payload :response-entry-id)
                      response-entry-id)
               (stringp frame-id)
               (equal (plist-get consumed-payload :response-entry-id)
                      response-entry-id)
               (null (plist-get consumed-payload :curation-ids))
               (equal (plist-get control :turn-id)
                      (plist-get consumed :turn-id))
               (condition-case nil
                   (e-session-local-entry-by-id store session-id response-entry-id)
                 (error nil)))
      (list :response-entry-id response-entry-id
            :frame-id frame-id
            :control-p t
            :consumed-p t))))

(cl-defun e-live-e2e--classify-autonomous-adoption
    (&key identity-result ordinary-tool-p prompt-control-p carrier-p
          valid-effect-p replacement-preserved-p audit-linked-p projection-p
          raw-excluded-p follow-up-p)
  "Return bounded adoption and composition results for the supplied gates.

The result is a plist with only the content-free subresults and one accepted
failure stage.  A valid identity with no model-selected effect is a product
contract observation; every repository/composition gate is semantic."
  (if (not (equal identity-result "pass"))
      (list :adoption-result "unavailable"
            :composition-result "unavailable"
            :failure-stage "none"
            :result "unavailable")
    (cond
     ((not ordinary-tool-p)
      (list :adoption-result "unavailable"
            :composition-result "failure"
            :failure-stage "ordinary-tool"
            :result "semantic-failure"))
     ((not prompt-control-p)
      (list :adoption-result "unavailable"
            :composition-result "failure"
            :failure-stage "prompt-control"
            :result "semantic-failure"))
     ((not carrier-p)
      (list :adoption-result "unavailable"
            :composition-result "failure"
            :failure-stage "carrier"
            :result "semantic-failure"))
     ((or (not valid-effect-p) (not replacement-preserved-p))
      (list :adoption-result "product-contract-failure"
            :composition-result "pass"
            :failure-stage "model-selection"
            :result "product-contract-failure"))
     ((not audit-linked-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "commit"
            :result "semantic-failure"))
     ((not projection-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "projection"
            :result "semantic-failure"))
     ((not raw-excluded-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "raw-exclusion"
            :result "semantic-failure"))
     ((not follow-up-p)
      (list :adoption-result "pass"
            :composition-result "failure"
            :failure-stage "follow-up-answer"
            :result "semantic-failure"))
     (t
      (list :adoption-result "pass"
            :composition-result "pass"
            :failure-stage "none"
            :result "pass")))))

(defun e-live-e2e--adoption-time (value)
  "Return numeric time for VALUE, or nil when it is not a timestamp."
  (cond
   ((numberp value) value)
   ((stringp value)
    (ignore-errors (float-time (date-to-time value))))
   (t nil)))

(defun e-live-e2e--adoption-evidence-reusable-p
    (previous current &optional now)
  "Return non-nil when CURRENT can reuse bounded PREVIOUS evidence.
NOW is a numeric or ISO timestamp used by deterministic owner tests."
  (let* ((previous-time (e-live-e2e--adoption-time
                         (plist-get previous :timestamp)))
         (current-time (e-live-e2e--adoption-time
                        (plist-get current :timestamp)))
         (now-time (e-live-e2e--adoption-time (or now (float-time))))
         (identity-fields
          '(:scenario :provider-id :profile-id :base-url-identity
            :endpoint-identity :transport :store-mode :native-requester
            :model-id :material-request-shape :prompt-layout-revision
            :prompt-cache-key-derivation-revision :scenario-prompt-identity
            :affordance-revision :presentation-revision
            :adoption-dependency-identity)))
    (and previous current previous-time current-time now-time
         (stringp (plist-get previous :adoption-dependency-identity))
         (stringp (plist-get current :adoption-dependency-identity))
         (not (string-empty-p
               (plist-get previous :adoption-dependency-identity)))
         (not (string-empty-p
               (plist-get current :adoption-dependency-identity)))
         (>= (- current-time previous-time) 0)
         (>= (- now-time current-time) 0)
         (<= (- now-time previous-time)
             e-live-e2e--adoption-freshness-seconds)
         (cl-every (lambda (field)
                     (equal (plist-get previous field)
                            (plist-get current field)))
                   identity-fields))))

(ert-deftest e-live-e2e-test-autonomous-adoption-prompt-negative-inventory ()
  "The substituted adoption prompt names only the ordinary deterministic tool."
  (let ((prompt (e-live-e2e--autonomous-adoption-prompt "e2e_deterministic")))
    (should-not (e-live-e2e--adoption-prompt-violations prompt))
    (should (member "keep"
                    (e-live-e2e--adoption-prompt-violations
                     (concat prompt " KEEP"))))
    (should (member "numeric-source-reference"
                    (e-live-e2e--adoption-prompt-violations
                     "Use source 2 next.")))
    (should (member "context-curate"
                    (e-live-e2e--adoption-prompt-violations
                     "CONTEXT-CURATE is forbidden.")))))

(ert-deftest e-live-e2e-test-autonomous-erase-prompt-negative-inventory ()
  "The negative prompt asks for current use and no later lookup reminder."
  (let ((prompt (e-live-e2e--autonomous-erase-prompt "e2e_deterministic")))
    (should (string-match-p (regexp-quote "e2e_deterministic") prompt))
    (should (= (length (split-string prompt "e2e_deterministic")) 2))
    (should (string-match-p (regexp-quote "exactly once") prompt))
    (should (string-match-p (regexp-quote "useful only for this answer")
                            prompt))
    (should (string-match-p (regexp-quote "after this answer") prompt))
    (should (string-match-p (regexp-quote "do not want any reminder") prompt))
    (should (string-match-p (regexp-quote "the lookup happened") prompt))
    (should (string-match-p (regexp-quote "carried into later turns") prompt))
    (should (string-match-p (regexp-quote "Reverse every character of the sentinel")
                            prompt))
    (should (string-match-p (regexp-quote "only that transformed value")
                            prompt))
    (should (string-match-p (regexp-quote "no extra text") prompt))
    (should-not (e-live-e2e--adoption-prompt-violations prompt))
    (dolist (term '("context-curate" "curation" "keep" "drop" "erase" "retain"
                    "summary" "summaries" "reasoning" "schema" "source"
                    "label" "numeric-label" "carrier" "argument"))
      (should (member term
                      (e-live-e2e--adoption-prompt-violations
                       (concat prompt " " term)))))))

(ert-deftest e-live-e2e-test-autonomous-adoption-positive-effect-validation ()
  "Positive adoption accepts explicit exact or summary retention."
  (let* ((sentinel "LIVE-ADOPTION-SENTINEL")
         (exact-record (list :items
                             (list (list :kind 'exact :value sentinel))))
         (summary-record
          (list :items
                (list (list :kind 'summary
                            :text (concat "Remember " sentinel)))))
         (other-record
          '(:items ((:kind exact :value "OTHER-SENTINEL"))))
         (exact-arguments '(:keep [1]))
         (summary-arguments
          '(:summaries
            [(:sources [1] :text "Remember LIVE-ADOPTION-SENTINEL")])))
    (should (e-live-e2e--adoption-positive-effect-valid-p
             exact-arguments 2 exact-record sentinel))
    (should (e-live-e2e--adoption-positive-effect-valid-p
             summary-arguments 2 summary-record sentinel))
    (should-not (e-live-e2e--adoption-positive-effect-valid-p
                 exact-arguments 2 other-record sentinel))
    (should-not (e-live-e2e--adoption-positive-effect-valid-p
                 summary-arguments 2 nil sentinel))
    (dolist (bad
             '((:keep [] :summaries [])
               (:keep [1] :erase [1])
               (:erase [1 2])))
      (should-not (e-live-e2e--adoption-positive-effect-valid-p
                   bad 2 exact-record sentinel)))
    ;; The independent erasure disposition has no positive adoption evidence.
    (should-not (e-live-e2e--adoption-positive-effect-valid-p
                 '(:erase [1 2])
                 2 exact-record sentinel))))

(ert-deftest e-live-e2e-test-autonomous-adoption-erase-effect-validation ()
  "Erase validation accepts exactly one submitted source label."
  (should (e-live-e2e--adoption-erase-effect-valid-p
           '(:erase [1]) 2))
  (should (e-live-e2e--adoption-erase-effect-valid-p
           '(:keep [] :summaries [] :erase [2]) 2))
  (dolist (bad
           '((:keep [1] :summaries [] :erase [2])
             (:keep [] :summaries [(:sources [1] :text "fact")] :erase [2])
             (:keep [] :summaries [] :erase [])
             (:keep [] :summaries [] :erase [1 2])
             (:keep [] :summaries [])
             (:erase [0])
             (:erase [3])
             (:erase ["1"])
             (:unknown [1])))
    (should-not (e-live-e2e--adoption-erase-effect-valid-p bad 2)))
  ;; Explicit erasure is one submitted label even when the source frontier is
  ;; larger; source eligibility is checked by the core frame preparation.
  (should (e-live-e2e--adoption-erase-effect-valid-p
           '(:erase [17]) 17)))

(ert-deftest e-live-e2e-test-autonomous-erase-classification-partitions ()
  "Erase adoption and composition failures remain independently classified."
  (let ((base '(:identity-result "pass"
                :ordinary-tool-p t :prompt-control-p t :carrier-p t
                :current-answer-p t :effect-present-p t
                :explicit-erasure-p t
                :audit-linked-p t :consumed-p t :no-promotion-p t
                :receipt-prerequisite-p t :receipt-erasure-id-p t
                :erasure-persisted-p t :receipt-suppressed-p t
                :details-preserved-p t
                :reopen-audit-p t :reopen-erasure-p t
                :reopen-no-promotion-p t :reopen-receipt-p t)))
    (let ((passing (apply #'e-live-e2e--classify-autonomous-erase base)))
      (should (equal passing
                     '(:adoption-result "pass"
                       :composition-result "pass"
                       :failure-stage "none"
                       :adoption-disposition "erase"
                       :result "pass"))))
    (dolist
        (case
         '((:ordinary-tool-p nil "unavailable" "failure" "ordinary-tool"
            "unavailable" "semantic-failure")
           (:prompt-control-p nil "unavailable" "failure" "prompt-control"
            "unavailable" "semantic-failure")
           (:carrier-p nil "unavailable" "failure" "carrier" "unavailable"
            "semantic-failure")
           (:current-answer-p nil "unavailable" "failure" "current-answer"
            "unavailable" "semantic-failure")
           (:effect-present-p nil "product-contract-failure" "pass"
            "model-selection" "unavailable" "product-contract-failure")
           (:explicit-erasure-p nil "product-contract-failure" "pass"
            "model-selection" "unavailable" "product-contract-failure")
           (:audit-linked-p nil "pass" "failure" "commit" "erase"
            "semantic-failure")
           (:consumed-p nil "pass" "failure" "commit" "erase"
            "semantic-failure")
           (:no-promotion-p nil "pass" "failure" "no-promotion" "erase"
            "semantic-failure")
           (:receipt-prerequisite-p nil "pass" "failure"
            "receipt-prerequisite" "erase" "semantic-failure")
           (:erasure-persisted-p nil "pass" "failure" "erasure-persisted" "erase"
            "semantic-failure")
           (:receipt-erasure-id-p nil "pass" "failure" "receipt-erasure-id"
            "erase" "semantic-failure")
           (:receipt-suppressed-p nil "pass" "failure" "receipt-suppressed" "erase"
            "semantic-failure")
           (:details-preserved-p nil "pass" "failure" "details-preserved" "erase"
            "semantic-failure")
           (:reopen-audit-p nil "pass" "failure" "reopen-audit" "erase"
            "semantic-failure")
           (:reopen-erasure-p nil "pass" "failure" "reopen-erasure" "erase"
            "semantic-failure")
           (:reopen-no-promotion-p nil "pass" "failure"
            "reopen-no-promotion" "erase" "semantic-failure")
           (:reopen-receipt-p nil "pass" "failure" "reopen-receipt" "erase"
            "semantic-failure")))
      (let* ((arguments
              (plist-put (copy-sequence base) (nth 0 case) (nth 1 case)))
             (result (apply #'e-live-e2e--classify-autonomous-erase arguments)))
        (should (equal (plist-get result :adoption-result) (nth 2 case)))
        (should (equal (plist-get result :composition-result) (nth 3 case)))
        (should (equal (plist-get result :failure-stage) (nth 4 case)))
        (should (equal (plist-get result :adoption-disposition)
                       (nth 5 case)))
        (should (equal (plist-get result :result) (nth 6 case)))))
    (let ((unavailable
           (apply #'e-live-e2e--classify-autonomous-erase
                  (plist-put (copy-sequence base)
                             :identity-result "unavailable"))))
      (should (equal (plist-get unavailable :adoption-result) "unavailable"))
      (should (equal (plist-get unavailable :composition-result) "unavailable"))
      (should (equal (plist-get unavailable :result) "unavailable")))))

(ert-deftest e-live-e2e-test-autonomous-adoption-accepts-exact-or-summary ()
  "The adopted sentinel may be preserved by either supported disposition."
  (let ((sentinel "LIVE-ADOPTION-SENTINEL"))
    (dolist (case
             `(((:kind exact :value ,sentinel) "exact")
               ((:kind summary :text ,(concat "Remember " sentinel))
                "summary")))
      (let ((record (list :items (list (car case)))))
        (should (e-live-e2e--adoption-record-preserves-sentinel-p
                 record sentinel))
        (should (equal (e-live-e2e--adoption-record-disposition
                        record sentinel)
                       (cadr case)))))
    (should-not
     (e-live-e2e--adoption-record-preserves-sentinel-p
      '(:items ((:kind exact :value "OTHER"))) sentinel))))

(ert-deftest e-live-e2e-test-autonomous-adoption-classification-partitions ()
  "Adoption/model choice and composition failures remain independently bounded."
  (let ((base '(:identity-result "pass"
                :ordinary-tool-p t :prompt-control-p t :carrier-p t
                :valid-effect-p t :replacement-preserved-p t
                :audit-linked-p t :projection-p t :raw-excluded-p t
                :follow-up-p t)))
    (dolist (case
             '((:valid-effect-p nil "product-contract-failure"
                "pass" "model-selection" "product-contract-failure")
               (:replacement-preserved-p nil "product-contract-failure"
                "pass" "model-selection" "product-contract-failure")
               (:carrier-p nil "unavailable" "failure" "carrier"
                "semantic-failure")
               (:projection-p nil "pass" "failure" "projection"
                "semantic-failure")
               (:raw-excluded-p nil "pass" "failure" "raw-exclusion"
                "semantic-failure")
               (:follow-up-p nil "pass" "failure" "follow-up-answer"
                "semantic-failure")))
      (let* ((key (nth 0 case))
             (arguments (plist-put (copy-sequence base) key (nth 1 case)))
             (result (apply #'e-live-e2e--classify-autonomous-adoption
                            arguments)))
        (should (equal (plist-get result :adoption-result) (nth 2 case)))
        (should (equal (plist-get result :composition-result) (nth 3 case)))
        (should (equal (plist-get result :failure-stage) (nth 4 case)))
        (should (equal (plist-get result :result) (nth 5 case)))))
    (let ((unavailable
           (e-live-e2e--classify-autonomous-adoption
            :identity-result "unavailable")))
      (should (equal (plist-get unavailable :result) "unavailable")))
    (let ((passing (apply #'e-live-e2e--classify-autonomous-adoption base)))
      (should (equal passing
                     '(:adoption-result "pass"
                       :composition-result "pass"
                       :failure-stage "none"
                       :result "pass"))))))

(ert-deftest e-live-e2e-test-autonomous-adoption-evidence-freshness-and-reuse ()
  "Adoption evidence reuses only within seven days and equal material identity."
  (let* ((base
          '(:scenario "responses-autonomous-curation-adoption"
            :provider-id "gateway" :profile-id "Responses"
            :base-url-identity "https://gateway.example"
            :endpoint-identity "https://gateway.example/responses"
            :transport "responses-http" :store-mode "json-false"
            :native-requester "e-openai-http-request-start"
            :model-id "gpt-5.6-sol"
            :material-request-shape ((:body-sha256 "body-1"))
            :prompt-layout-revision "layout-1"
            :prompt-cache-key-derivation-revision "cache-1"
            :scenario-prompt-identity "prompt-1"
            :affordance-revision "affordance-1"
            :presentation-revision "presentation-1"
            :adoption-dependency-identity "cone-1"
            :repository-revision "repo-1"
            :timestamp "2026-08-27T00:00:00Z"))
         (current (copy-sequence base)))
    (should
     (e-live-e2e--adoption-evidence-reusable-p
      base current "2026-08-30T00:00:00Z"))
    (should
     (e-live-e2e--adoption-evidence-reusable-p
      base (plist-put (copy-sequence current) :repository-revision "repo-2")
      "2026-08-30T00:00:00Z"))
    (should-not
     (e-live-e2e--adoption-evidence-reusable-p
      base (plist-put (copy-sequence current)
                      :adoption-dependency-identity "cone-2")
      "2026-08-30T00:00:00Z"))
    (should-not
     (e-live-e2e--adoption-evidence-reusable-p
      base current "2026-09-04T00:00:01Z"))
    (should-not
     (e-live-e2e--adoption-evidence-reusable-p
      base (plist-put (copy-sequence current) :scenario-prompt-identity
                      "prompt-2")
      "2026-08-30T00:00:00Z"))
    (should-not
     (e-live-e2e--adoption-evidence-reusable-p
      base (plist-put (copy-sequence current) :material-request-shape
                      '((:body-sha256 "body-2")))
      "2026-08-30T00:00:00Z"))))

(ert-deftest e-live-e2e-test-adoption-dependency-cone-covers-evidence-owners ()
  "Adoption reuse is fenced by every owner that can change its evidence."
  (let* ((owners
          '("lisp/layers/harness/e-harness-base.el"
            "lisp/layers/harness/e-tool-invocation-details.el"
            "lisp/layers/harness/e-session-tmp-resources.el"))
         (root (make-temp-file "e-live-dependency-cone-" t)))
    (unwind-protect
        (progn
          (dolist (relative e-live-e2e--adoption-dependency-cone-files)
            (let ((path (expand-file-name relative root)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path
                (insert "baseline:" relative))))
          (let ((baseline
                 (e-live-e2e--dependency-identity
                  e-live-e2e--adoption-dependency-cone-files root)))
            (should (equal
                     baseline
                     (e-live-e2e--dependency-identity
                      e-live-e2e--adoption-dependency-cone-files root)))
            (dolist (owner owners)
              (should (member owner e-live-e2e--adoption-dependency-cone-files))
              (let ((path (expand-file-name owner root)))
                (with-temp-file path
                  (insert "changed:" owner))
                (should-not
                 (equal
                  baseline
                  (e-live-e2e--dependency-identity
                   e-live-e2e--adoption-dependency-cone-files root)))
                (with-temp-file path
                  (insert "baseline:" owner))))))
      (delete-directory root t))))

(ert-deftest e-live-e2e-test-autonomous-erase-evidence-freshness-and-reuse ()
  "Erase evidence reuses only within seven days and equal material identity."
  (let* ((base
          '(:scenario "responses-autonomous-curation-erase"
            :provider-id "gateway" :profile-id "Responses"
            :base-url-identity "https://gateway.example"
            :endpoint-identity "https://gateway.example/responses"
            :transport "responses-http" :store-mode "json-false"
            :native-requester "e-openai-http-request-start"
            :model-id "gpt-5.6-sol"
            :material-request-shape ((:body-sha256 "erase-body-1"))
            :prompt-layout-revision "layout-1"
            :prompt-cache-key-derivation-revision "cache-1"
            :scenario-prompt-identity "erase-prompt-1"
            :affordance-revision "context-curate-v5"
            :presentation-revision "context-curation-presentation-v2"
            :adoption-dependency-identity "erase-cone-1"
            :repository-revision "repo-1"
            :timestamp "2026-08-27T00:00:00Z"))
         (current (copy-sequence base)))
    (should
     (e-live-e2e--adoption-evidence-reusable-p
      base current "2026-08-30T00:00:00Z"))
    ;; Repository provenance is retained but does not invalidate the material
    ;; erase result by itself.
    (should
     (e-live-e2e--adoption-evidence-reusable-p
      base (plist-put (copy-sequence current) :repository-revision "repo-2")
      "2026-08-30T00:00:00Z"))
    (should-not
     (e-live-e2e--adoption-evidence-reusable-p
      base (plist-put (copy-sequence current)
                      :adoption-dependency-identity "erase-cone-2")
      "2026-08-30T00:00:00Z"))
    (should-not
     (e-live-e2e--adoption-evidence-reusable-p
      base current "2026-09-04T00:00:01Z"))))

(ert-deftest e-live-e2e-test-reasoning-summary-capability-classification-partitions ()
  "Reasoning-summary capability outcomes have one bounded classification."
  (dolist (case
           '((:completed-p t "pass")
             (:endpoint-rejected-p t "unavailable")
             (:provider-failure-p t "provider/infrastructure-failure")
             (:timeout-p t "inconclusive-timeout")
             (:configuration-unavailable-p t "configuration-unavailable")))
    (let ((arguments (list :identity-result "pass"
                           :request-valid-p t
                           :completed-p nil)))
      (setq arguments (plist-put arguments (car case) (nth 1 case)))
      (should (equal
               (apply #'e-live-e2e--classify-reasoning-summary-capability
                      arguments)
               (nth 2 case)))))
  (should (equal
           (e-live-e2e--classify-reasoning-summary-capability
            :identity-result "invalid" :request-valid-p t :completed-p t)
           "semantic-failure"))
  (should (equal
           (e-live-e2e--classify-reasoning-summary-capability
            :identity-result "pass" :request-valid-p nil :completed-p t)
           "semantic-failure")))

(ert-deftest e-live-e2e-test-reasoning-summary-presence-is-bounded-and-content-free ()
  "Summary presence distinguishes present, empty, and absent without text."
  (should (equal
           (e-live-e2e--reasoning-summary-presence
            '((:type reasoning-delta :stream-kind summary
               :content "PRIVATE-SUMMARY")))
           "present"))
  (should (equal
           (e-live-e2e--reasoning-summary-presence
            '((:type reasoning-delta :stream-kind summary :content "")))
           "empty"))
  (should (equal (e-live-e2e--reasoning-summary-presence nil) "absent"))
  (should (equal
           (e-live-e2e--reasoning-summary-presence
            nil
            '((:metadata
               (:provider-replay-items
                ((:item (:type "reasoning" :summary [])))))))
           "empty"))
  (should (equal
           (e-live-e2e--reasoning-summary-presence
            nil
            '((:metadata
               (:provider-replay-items
                ((:item (:type "reasoning")))))))
           "absent")))

(ert-deftest e-live-e2e-test-reasoning-summary-capability-evidence-is-content-free ()
  "The probe record carries identity fields but no provider content."
  (let* ((record
          (e-live-e2e--external-evidence-record
           :evidence-schema-revision
           e-live-e2e--reasoning-summary-evidence-schema-revision
           :scenario 'responses-reasoning-summary-capability
           :provider 'configured-provider
           :profile '(:name "Configured Responses"
                      :base-url "https://gateway.example/v1"
                      :responses-transport http
                      :response-store :json-false)
           :model "gpt-5.6-sol"
           :request-bodies
           '((:model "gpt-5.6-sol" :store :json-false
              :reasoning (:effort "high" :summary "auto")
              :input [(:role "user" :content "PRIVATE-PROMPT")]))
           :request-metadata
           '((:url "https://gateway.example/v1/responses"
              :transport url-retrieve))
           :reasoning-effort "high"
           :reasoning-summary "auto"
           :probe-request-identity "probe-hash"
           :dependency-identity "dependency-hash"
           :returned-summary-presence "present"
           :timeout 120.0
           :started-at 100.0
           :ended-at 100.5
           :semantic-result "pass"
           :cache-result "unavailable"
           :result "pass"))
         (encoded (json-encode (e-live-e2e--json-plist record))))
    (should (equal (plist-get record :scenario)
                   "responses-reasoning-summary-capability"))
    (should (equal (plist-get record :reasoning-effort) "high"))
    (should (equal (plist-get record :reasoning-summary) "auto"))
    (should (equal (plist-get record :returned-summary-presence) "present"))
    (should (equal (plist-get record :probe-request-identity) "probe-hash"))
    (should-not (string-match-p "PRIVATE-PROMPT" encoded))))

(ert-deftest e-live-e2e-test-reasoning-summary-capability-evidence-freshness-and-reuse ()
  "Probe evidence reuses for seven days only with exact identity equality."
  (let* ((base
          '(:evidence-schema-revision "e88-reasoning-summary-evidence-v1"
            :scenario "responses-reasoning-summary-capability"
            :provider-id "gateway" :profile-id "Responses"
            :base-url-identity "https://gateway.example"
            :endpoint-identity "https://gateway.example/responses"
            :transport "responses-http" :store-mode "json-false"
            :native-requester "e-openai-http-request-start"
            :model-id "gpt-5.6-sol"
            :reasoning-effort "high" :reasoning-summary "auto"
            :material-request-shape ((:body-sha256 "body-1"))
            :probe-request-identity "probe-1"
            :prompt-layout-revision "layout-1"
            :prompt-cache-key-derivation-revision "cache-1"
            :dependency-identity "cone-1"
            :identity-result "pass" :result "pass"
            :repository-revision "repo-1"
            :timestamp "2026-08-27T00:00:00Z"))
         (current (copy-sequence base)))
    (should
     (e-live-e2e--reasoning-summary-evidence-reusable-p
      base current "2026-09-03T00:00:00Z"))
    ;; Repository provenance may change without changing the material probe.
    (should
     (e-live-e2e--reasoning-summary-evidence-reusable-p
      base (plist-put (copy-sequence current) :repository-revision "repo-2")
      "2026-09-03T00:00:00Z"))
    (dolist (field '(:reasoning-effort :reasoning-summary :probe-request-identity
                     :material-request-shape :dependency-identity))
      (let ((changed (copy-sequence current)))
        (plist-put changed field (format "%s-2" (plist-get changed field)))
        (should-not
         (e-live-e2e--reasoning-summary-evidence-reusable-p
          base changed "2026-09-03T00:00:00Z"))))
    (should-not
     (e-live-e2e--reasoning-summary-evidence-reusable-p
      base current "2026-09-03T00:00:01Z"))
    (should (string-match-p
             "\\`[[:xdigit:]]\\{64\\}\\'"
             (e-live-e2e--reasoning-summary-dependency-identity)))))

(ert-deftest e-live-e2e-test-reasoning-summary-capability-finalizes-terminal-paths ()
  "The capability boundary records success, rejection, config, and timeout once."
  (let* ((profile '(:name "Configured Responses"
                    :wire-api responses
                    :responses-transport http
                    :base-url "https://gateway.example/v1"
                    :response-store :json-false))
         (body '(:model "gpt-5.6-sol" :store :json-false
                 :reasoning (:effort "medium" :summary "auto")
                 :input [(:type "message" :role "user")]))
         (metadata '((:url "https://gateway.example/v1/responses"
                      :transport url-retrieve)))
         (cases (list
                 (list "pass" t
                       (lambda () :completed))
                 (list "unavailable" t
                       (lambda () (ert-skip "summary rejected")))
                 (list "configuration-unavailable" nil
                       (lambda () :must-not-run))
                 (list "inconclusive-timeout" t
                       (lambda ()
                         (signal 'e-live-e2e-scenario-timeout
                                 (list :deadline 120.0)))))))
    (dolist (case cases)
      (let (records condition ran)
        (cl-letf (((symbol-function
                    'e-live-e2e--profile-auth-available-p)
                   (lambda (&rest _) (nth 1 case)))
                  ((symbol-function 'e-live-e2e--report-external-evidence)
                   (lambda (record) (push record records))))
          (condition-case caught
              (e-live-e2e--run-external-scenario
               :scenario 'responses-reasoning-summary-capability
               :provider 'configured-provider
               :profile profile
               :model "gpt-5.6-sol"
               :timeout 120.0
               :started-at 100.0
               :evidence-schema-revision
               e-live-e2e--reasoning-summary-evidence-schema-revision
               :capture
               (lambda ()
                 (list :request-bodies (list body)
                       :request-metadata metadata
                       :reasoning-effort "medium"
                       :reasoning-summary "auto"
                       :probe-request-identity "probe"
                       :dependency-identity "dependency"
                       :returned-summary-presence "absent"
                       :semantic-result "pass"
                       :scenario-result
                       (and (equal (car case) "pass") "pass")))
               :thunk (lambda ()
                        (setq ran t)
                        (funcall (nth 2 case))))
            (error (setq condition caught))))
        (should (= (length records) 1))
        (should (if (equal (car case) "pass")
                    (null condition)
                  condition))
        (when condition
          (should
           (eq (car condition)
               (if (equal (car case) "inconclusive-timeout")
                   'e-live-e2e-scenario-timeout
                 'ert-test-skipped))))
        (should (eq ran (nth 1 case)))
        (should (equal (plist-get (car records) :result)
                       (car case)))))))

(ert-deftest e-live-e2e-test-autonomous-adoption-record-is-content-free ()
  "Adoption subresults add bounded fields without exposing captured content."
  (let* ((record
          (e-live-e2e--external-evidence-record
           :scenario 'responses-autonomous-curation-adoption
           :provider 'configured-provider
           :profile '(:name "Configured Responses"
                      :base-url "https://gateway.example/v1"
                      :responses-transport http
                      :response-store :json-false)
           :model "gpt-5.6-sol"
           :request-bodies
           '((:model "gpt-5.6-sol" :store :json-false
              :input [(:type "message" :role "system"
                       :content "PRIVATE-LIVE-ADOPTION-SENTINEL")]))
           :request-metadata
           '((:url "https://gateway.example/v1/responses"
              :transport url-retrieve))
           :timeout 120.0
           :started-at 100.0
           :ended-at 100.5
           :semantic-result "pass"
           :cache-result "unavailable"
           :result "pass"
           :adoption-result "pass"
           :composition-result "pass"
           :failure-stage "none"
           :adoption-disposition "summary"
           :scenario-prompt-identity "prompt-hash"
           :affordance-revision "context-curate-v5"
           :presentation-revision "context-curation-presentation-v2"
           :adoption-gates
           '(:ordinary-tool-p t :prompt-control-p t :carrier-p t
             :valid-effect-p t :replacement-preserved-p t
             :audit-linked-p t :projection-p t :raw-excluded-p t
             :follow-up-p t)))
         (encoded (json-encode (e-live-e2e--json-plist record))))
    (should (equal (plist-get record :identity-result) "pass"))
    (should (equal (plist-get record :adoption-result) "pass"))
    (should (equal (plist-get record :composition-result) "pass"))
    (should (equal (plist-get record :failure-stage) "none"))
    (should (equal (plist-get record :adoption-disposition) "summary"))
    (should (string-match-p
             "\\`[[:xdigit:]]\\{64\\}\\'"
             (plist-get record :adoption-dependency-identity)))
    (should-not (string-match-p "PRIVATE-LIVE-ADOPTION-SENTINEL" encoded))))

(ert-deftest e-live-e2e-test-autonomous-adoption-record-gates-identity ()
  "Identity failure downgrades adoption subresults through the record path."
  (let ((gates
         '(:ordinary-tool-p t :prompt-control-p t :carrier-p t
           :valid-effect-p nil :replacement-preserved-p t
           :audit-linked-p t :projection-p t :raw-excluded-p t
           :follow-up-p t)))
    (dolist (case
             `(("unavailable" "identity-unavailable"
                (:model "caller-model" :input []))
               ("invalid" "identity-invalid"
                (:model "wire-model" :store :json-false :input []))))
      (let* ((identity-result (nth 0 case))
             (record
              (e-live-e2e--external-evidence-record
               :scenario 'responses-autonomous-curation-adoption
               :provider 'configured-provider
               :profile (if (equal identity-result "invalid")
                            '(:name "Declared"
                              :wire-api responses
                              :responses-transport http
                              :base-url "https://declared.example/v1"
                              :response-store :json-false)
                          '(:name "Declared"
                            :wire-api responses
                            :responses-transport http
                            :base-url "https://gateway.example/v1"
                            :response-store :json-false))
               :model "caller-model"
               :request-bodies (list (nth 2 case))
               :request-metadata
               (list (list :url
                           (if (equal identity-result "invalid")
                               "https://effective.example/v1/responses"
                             "https://gateway.example/v1/responses")
                           :transport 'url-retrieve))
               :semantic-result "pass"
               :cache-result "warm"
               :result "pass"
               :adoption-result "pass"
               :composition-result "pass"
               :failure-stage "none"
               :adoption-gates gates
               :adoption-dependency-identity "cone-1")))
        (should (equal (plist-get record :identity-result) identity-result))
        (should (equal (plist-get record :adoption-result) "unavailable"))
        (should (equal (plist-get record :composition-result) "unavailable"))
        (should (equal (plist-get record :failure-stage) "none"))
        (should (equal (plist-get record :result) (nth 1 case)))))))

(ert-deftest e-live-e2e-test-autonomous-adoption-audit-linkage-is-exact ()
  "The curation response, durable entry, and consumed frame share one link."
  (let ((durable-events
         '((:id "response-1" :event-type context-curation-response
            :payload (:response-entry-id "response-1"))))
        (sink-events
         '((:id "consumed-1" :type context-frame-consumed
            :payload (:response-entry-id "response-1"
                      :curation-ids ("curation-1")))))
        (record '(:id "curation-1" :response-entry-id "response-1")))
    (cl-letf (((symbol-function 'e-session-local-activity-events)
               (lambda (&rest _) durable-events))
              ((symbol-function 'e-session-local-entry-by-id)
               (lambda (_store _session entry-id)
                 (and (equal entry-id "response-1")
                      (list :id entry-id)))))
      (should (e-live-e2e--adoption-audit-linked-p
               'store "session-1" record sink-events))
      (let ((bad-events (copy-tree sink-events)))
        (plist-put (car bad-events) :payload
                   '(:response-entry-id "response-1"
                     :curation-ids ("other-curation")))
        (setq sink-events bad-events)
        (should-not (e-live-e2e--adoption-audit-linked-p
                     'store "session-1" record sink-events))))))

(ert-deftest e-live-e2e-test-autonomous-erase-audit-links-filter-unrelated-events ()
  "Erase audit linkage ignores other consumed frames but rejects duplicates."
  (let* ((control
          '(:id "response-1" :turn-id "turn-2"
            :event-type context-curation-response
            :payload (:response-entry-id "response-1")))
         (matching
          '(:id "consumed-1" :turn-id "turn-2"
            :event-type context-frame-consumed
            :payload (:response-entry-id "response-1"
                      :frame-id "frame-1" :curation-ids nil)))
         (unrelated
          '(:id "consumed-old" :turn-id "turn-1"
            :event-type context-frame-consumed
            :payload (:response-entry-id "response-old"
                      :frame-id "frame-old" :curation-ids nil)))
         (events (list unrelated control matching)))
    (cl-letf (((symbol-function 'e-session-local-activity-events)
               (lambda (&rest _) events))
              ((symbol-function 'e-session-local-entry-by-id)
               (lambda (_store _session entry-id)
                 (and (equal entry-id "response-1")
                      (list :id entry-id)))))
      (should (e-live-e2e--autonomous-erase-audit-links
               'store "session-1"))
      (setq events (append events (list (copy-tree matching))))
      (should-not (e-live-e2e--autonomous-erase-audit-links
                   'store "session-1")))))

(ert-deftest e-live-e2e-test-external-finalizer-preserves-adoption-partitions ()
  "The shared finalizer retains a model-selection product observation."
  (let ((process-environment (copy-sequence process-environment))
        records
        condition)
    (setenv "E_E2E_ADOPTION_TEST_TOKEN" "adoption-test-secret")
    (cl-letf (((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (record) (push record records))))
      (condition-case caught
          (e-live-e2e--run-external-scenario
           :scenario 'responses-autonomous-curation-adoption
           :provider 'configured-provider
           :profile '(:name "Configured Responses"
                      :base-url "https://gateway.example/v1"
                      :responses-transport http
                      :response-store :json-false
                      :requires-openai-auth nil
                      :env-key "E_E2E_ADOPTION_TEST_TOKEN")
           :model "gpt-5.6-sol"
           :timeout 120.0
           :started-at 100.0
           :capture
           (lambda ()
             (list :model "gpt-5.6-sol"
                   :request-bodies
                   '((:model "gpt-5.6-sol" :store :json-false :input []))
                   :request-metadata
                   '((:url "https://gateway.example/v1/responses"
                      :transport url-retrieve))
                   :adoption-result "product-contract-failure"
                   :composition-result "pass"
                   :failure-stage "model-selection"
                   :adoption-gates
                   '(:ordinary-tool-p t :prompt-control-p t :carrier-p t
                     :valid-effect-p nil :replacement-preserved-p t
                     :audit-linked-p t :projection-p t :raw-excluded-p t
                     :follow-up-p t)
                   :semantic-result "pass"
                   :cache-result "unavailable"
                   :scenario-result "product-contract-failure"))
           :thunk (lambda () (ert-fail "model did not autonomously curate")))
        (error (setq condition caught))))
    (should condition)
    (should (eq (car condition) 'ert-test-failed))
    (should (= (length records) 1))
    (let ((record (car records)))
      (should (equal (plist-get record :result)
                     "product-contract-failure"))
      (should (equal (plist-get record :adoption-result)
                     "product-contract-failure"))
      (should (equal (plist-get record :composition-result) "pass"))
      (should (equal (plist-get record :failure-stage)
                     "model-selection"))
      (should (equal (plist-get record :semantic-result) "pass")))))

(ert-deftest e-live-e2e-test-external-finalizer-classifies-autonomous-erase-partitions ()
  "The shared finalizer records autonomous-erasure outcomes without content."
  (let* ((profile '(:name "Configured Responses"
                    :wire-api responses
                    :responses-transport http
                    :base-url "https://gateway.example/v1"
                    :response-store :json-false))
         (body '(:model "gpt-5.6-sol" :store :json-false
                 :input [(:type "message" :role "user"
                          :content "PRIVATE-ERASE-SENTINEL")]))
         (metadata '((:url "https://gateway.example/v1/responses"
                      :transport url-retrieve)))
         (base-gates '(:ordinary-tool-p t :prompt-control-p t :carrier-p t
                       :current-answer-p t :effect-present-p t
                       :explicit-erasure-p t
                       :audit-linked-p t :consumed-p t :no-promotion-p t
                       :receipt-prerequisite-p t :receipt-erasure-id-p t
                       :erasure-persisted-p t :receipt-suppressed-p t
                       :details-preserved-p t
                       :reopen-audit-p t :reopen-erasure-p t
                       :reopen-no-promotion-p t :reopen-receipt-p t))
         (cases
          (list
           (list :gates base-gates :bodies (list body)
                 :condition nil :result "pass" :adoption "pass"
                 :composition "pass" :stage "none" :disposition "erase")
           (list :gates (plist-put (copy-sequence base-gates)
                                   :effect-present-p nil)
                 :bodies (list body) :condition 'ert-test-failed
                 :result "product-contract-failure"
                 :adoption "product-contract-failure" :composition "pass"
                 :stage "model-selection" :disposition "unavailable")
           (list :gates (plist-put (copy-sequence base-gates)
                                   :current-answer-p nil)
                 :bodies (list body) :condition 'ert-test-failed
                 :result "semantic-failure" :adoption "unavailable"
                 :composition "failure" :stage "current-answer"
                 :disposition "unavailable")
           (list :gates (plist-put (copy-sequence base-gates)
                                   :receipt-prerequisite-p nil)
                 :bodies (list body) :condition 'ert-test-failed
                 :result "semantic-failure" :adoption "pass"
                 :composition "failure" :stage "receipt-prerequisite"
                 :disposition "erase")
           (list :gates (plist-put (copy-sequence base-gates)
                                   :receipt-erasure-id-p nil)
                 :bodies (list body) :condition 'ert-test-failed
                 :result "semantic-failure" :adoption "pass"
                 :composition "failure" :stage "receipt-erasure-id"
                 :disposition "erase")
           (list :gates (plist-put (copy-sequence base-gates)
                                   :reopen-audit-p nil)
                 :bodies (list body) :condition 'ert-test-failed
                 :result "semantic-failure" :adoption "pass"
                 :composition "failure" :stage "reopen-audit"
                 :disposition "erase")
           (list :gates (plist-put (copy-sequence base-gates)
                                   :reopen-erasure-p nil)
                 :bodies (list body) :condition 'ert-test-failed
                 :result "semantic-failure" :adoption "pass"
                 :composition "failure" :stage "reopen-erasure"
                 :disposition "erase")
           (list :gates (plist-put (copy-sequence base-gates)
                                   :reopen-no-promotion-p nil)
                 :bodies (list body) :condition 'ert-test-failed
                 :result "semantic-failure" :adoption "pass"
                 :composition "failure" :stage "reopen-no-promotion"
                 :disposition "erase")
           (list :gates (plist-put (copy-sequence base-gates)
                                   :reopen-receipt-p nil)
                 :bodies (list body) :condition 'ert-test-failed
                 :result "semantic-failure" :adoption "pass"
                 :composition "failure" :stage "reopen-receipt"
                 :disposition "erase")
           (list :gates base-gates :bodies nil :condition nil
                 :result "identity-unavailable" :adoption "unavailable"
                 :composition "unavailable" :stage "none"
                 :disposition "unavailable"))))
    (cl-letf (((symbol-function 'e-live-e2e--profile-auth-available-p)
               (lambda (&rest _) t)))
      (dolist (case cases)
        (let (records condition)
          (cl-letf (((symbol-function
                      'e-live-e2e--report-external-evidence)
                     (lambda (record) (push record records))))
            (condition-case caught
                (e-live-e2e--run-external-scenario
                 :scenario 'responses-autonomous-curation-erase
                 :provider 'configured-provider
                 :profile profile
                 :model "gpt-5.6-sol"
                 :timeout 120.0
                 :started-at 100.0
                 :capture
                 (lambda ()
                   (list :request-bodies (plist-get case :bodies)
                         :request-metadata
                         (and (plist-get case :bodies) metadata)
                         :erase-adoption-gates (plist-get case :gates)
                         :adoption-disposition "erase"
                         :semantic-result "pass"
                         :cache-result "unavailable"))
                 :thunk
                 (lambda ()
                   (if (plist-get case :condition)
                       (ert-fail "autonomous erase gate failed")
                     :erase-pass)))
              (error (setq condition caught))))
          (should (= (length records) 1))
          (should (if (plist-get case :condition)
                      (and condition
                           (eq (car condition) 'ert-test-failed))
                    (null condition)))
          (let* ((record (car records))
                 (encoded (json-encode (e-live-e2e--json-plist record))))
            (should (equal (plist-get record :scenario)
                           "responses-autonomous-curation-erase"))
            (should (equal (plist-get record :result)
                           (plist-get case :result)))
            (should (equal (plist-get record :adoption-result)
                           (plist-get case :adoption)))
            (should (equal (plist-get record :composition-result)
                           (plist-get case :composition)))
            (should (equal (plist-get record :failure-stage)
                           (plist-get case :stage)))
            (should (equal (plist-get record :adoption-disposition)
                           (plist-get case :disposition)))
            (should-not (string-match-p
                         (regexp-quote "PRIVATE-ERASE-SENTINEL") encoded))))))))

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
                       "e-openai-websocket-request-start"))
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
           '((http "responses-http" "e-openai-http-request-start")
             (websocket "responses-websocket"
                         "e-openai-websocket-request-start")))
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

(ert-deftest e-live-e2e-test-external-evidence-record-uses-captured-http-and-websocket-identity ()
  "Captured endpoint/model/store identity wins over omitted profile settings."
  (dolist (case
           '((http url-retrieve "https://backend.example/v1/responses"
                   "wire-http-model" :json-false
                   "responses-http"
                   "e-openai-http-request-start")
             (websocket websocket "wss://backend.example/v1/responses"
                         "wire-websocket-model" t
                         "responses-websocket"
                         "e-openai-websocket-request-start")))
    (pcase-let ((`(,profile-transport ,native-transport ,endpoint ,wire-model
                     ,wire-store ,transport ,requester)
                  case))
      (let* ((record
              (e-live-e2e--external-evidence-record
               :scenario 'captured-identity
               :provider 'configured-provider
               :profile (list :name "Configured Responses"
                              :wire-api 'responses
                              :responses-transport profile-transport
                              :base-url "https://backend.example/v1/")
               :model "caller-default-model"
               :request-bodies
               (list (list :model wire-model
                           :input []))
               :request-metadata
               (list (list :url endpoint
                           :transport native-transport
                           :diagnostics
                           (list :model wire-model
                                 :response-store wire-store)))
               :usage-payloads '((:input-tokens 10
                                  :cached-input-tokens 4))
               :timeout 120.0
               :started-at 100.0
               :ended-at 100.5
               :semantic-result "pass"
               :cache-result "warm"
               :result "pass")))
        (should (equal (plist-get record :identity-result) "pass"))
        (should (equal (plist-get record :result) "pass"))
        (should (equal (plist-get record :endpoint-identity) endpoint))
        (should (equal (plist-get record :base-url-identity)
                       "https://backend.example/v1"))
        (should (equal (plist-get record :model-id) wire-model))
        (should (equal (plist-get record :store-mode)
                       (format "%s" wire-store)))
        (should (equal (plist-get record :transport) transport))
        (should (equal (plist-get record :native-requester) requester))))))

(ert-deftest e-live-e2e-test-external-evidence-record-does-not-promote-uncaptured-defaults ()
  "Profile URL/store/model defaults do not complete a partial capture."
  (let ((record
         (e-live-e2e--external-evidence-record
          :scenario 'partial-identity
          :provider 'configured-provider
          :profile '(:name "Configured Responses"
                     :wire-api responses
                     :responses-transport http
                     :base-url "https://backend.example/v1/"
                     :default-model "declared-model"
                     :response-store :json-false)
          :model "caller-model"
          :request-bodies '((:input []))
          :request-metadata
          '((:url "https://backend.example/v1/responses"
             :transport url-retrieve))
          :semantic-result "pass"
          :cache-result "warm"
          :result "pass")))
    (should (equal (plist-get record :identity-result) "unavailable"))
    (should (equal (plist-get record :result) "identity-unavailable"))
    (should (equal (plist-get record :model-id) "caller-model"))
    (should (equal (plist-get record :store-mode) "unavailable"))
    (should (equal (plist-get record :base-url-identity)
                   "https://backend.example/v1"))))

(ert-deftest e-live-e2e-test-external-evidence-record-rejects-declared-identity-mismatch ()
  "A profile declaration cannot make a different captured request pass."
  (let ((record
         (e-live-e2e--external-evidence-record
          :scenario 'declared-identity-mismatch
          :provider 'configured-provider
          :profile '(:name "Declared profile"
                     :wire-api responses
                     :responses-transport http
                     :base-url "https://declared.example/v1"
                     :default-model "declared-model")
          :request-bodies
          '((:model "effective-model" :store :json-false :input []))
          :request-metadata
          '((:url "https://effective.example/v1/responses"
             :transport websocket
             :diagnostics (:response-store :json-false)))
          :timeout 120.0
          :started-at 100.0
          :ended-at 100.5
          :semantic-result "pass"
          :cache-result "warm"
          :result "pass")))
    (should (equal (plist-get record :identity-result) "invalid"))
    (should (equal (plist-get record :result) "identity-invalid"))
    (should (equal (plist-get record :scenario-result) "pass"))
    (should (equal (plist-get record :base-url-identity)
                   "https://effective.example/v1"))
    (should (equal (plist-get record :model-id) "effective-model"))
    (should (equal (plist-get record :transport) "responses-websocket"))
    (should (seq-some (lambda (m) (eq (plist-get m :field) :base-url))
                      (plist-get record :identity-mismatches)))
    (should (seq-some (lambda (m) (eq (plist-get m :field) :model))
                      (plist-get record :identity-mismatches)))
    (should (seq-some (lambda (m) (eq (plist-get m :field) :transport))
                      (plist-get record :identity-mismatches)))))

(ert-deftest e-live-e2e-test-external-evidence-record-rejects-identity-drift ()
  "A material identity change in a later captured request is not cache proof."
  (let ((record
         (e-live-e2e--external-evidence-record
          :scenario 'identity-drift
          :provider 'configured-provider
          :profile '(:name "Configured Responses"
                     :wire-api responses
                     :responses-transport http)
          :model "fallback-model"
          :request-bodies
          '((:model "wire-model" :store :json-false :input [])
            (:model "changed-model" :store :json-false :input []))
          :request-metadata
          '((:url "https://backend.example/v1/responses"
             :transport url-retrieve)
            (:url "https://backend.example/v1/responses"
             :transport url-retrieve))
          :timeout 120.0
          :started-at 100.0
          :ended-at 100.5
          :semantic-result "pass"
          :cache-result "warm"
          :result "pass")))
    (should (equal (plist-get record :request-count) 2))
    (should (equal (plist-get record :identity-result) "invalid"))
    (should (equal (plist-get record :result) "identity-invalid"))
    (should (seq-some (lambda (m)
                        (and (= (plist-get m :request-index) 2)
                             (eq (plist-get m :field) :model)))
                      (plist-get record :identity-mismatches)))))

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
                        (signal 'e-live-e2e-scenario-timeout
                                (list :deadline 120.0)))
                      'e-live-e2e-scenario-timeout)
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

(ert-deftest e-live-e2e-test-prompt-batch-deadline-signals-typed-condition ()
  "A running turn that consumes the deadline becomes a typed timeout."
  (let* ((active-turns (make-hash-table :test #'equal))
         (harness (e-harness-state-create :active-turns active-turns))
        (clock '(0.0 11.0))
        condition)
    (puthash "session" '(:status running) active-turns)
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _)
                 (prog1 (car clock)
                   (setq clock (cdr clock)))))
              ((symbol-function 'e-chat-sql-e2e-prompt-batch)
               (lambda (&rest _)
                 (error "E2E turn did not settle within 10.0 seconds"))))
      (condition-case caught
          (e-live-e2e--prompt-batch-before-deadline
           harness "session" "prompt" 10.0)
        (error (setq condition caught))))
    (should (eq (car condition) 'e-live-e2e-scenario-timeout))
    (should (equal (plist-get (cdr condition) :deadline) 10.0))
    (should (eq (car (plist-get (cdr condition) :cause)) 'error))))

(ert-deftest e-live-e2e-test-external-finalizer-types-timeout-and-cancels-newest ()
  "Typed timeout and provider error each finalize once and cancel newest."
  (let (records)
    (cl-letf (((symbol-function 'e-live-e2e--profile-auth-available-p)
               (lambda (&rest _) t))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (record) (push record records))))
      (dolist (case
               (list
                (list 'e-live-e2e-scenario-timeout
                      "inconclusive-timeout"
                      (lambda ()
                        (signal 'e-live-e2e-scenario-timeout
                                (list :deadline 120.0))))
                (list 'error
                      "provider/infrastructure-failure"
                      (lambda ()
                        (error "provider reported inconclusive-timeout")))))
        (setq records nil)
        (let (cancelled condition)
          (let* ((old (e-backend-request-create
                       :cancel (lambda ()
                                 (push 'old cancelled)
                                 t)))
                 (new (e-backend-request-create
                       :cancel (lambda ()
                                 (push 'new cancelled)
                                 t))))
            (condition-case caught
                (e-live-e2e--run-external-scenario
                 :scenario 'responses-canonical-tool-heavy
                 :provider 'configured-provider
                 :profile '(:name "Configured Responses"
                            :responses-transport http
                            :requires-openai-auth t)
                 :model "gpt-5.6-sol"
                 :timeout 120.0
                 :started-at 100.0
                 :capture (lambda ()
                            '(:semantic-result "unavailable"
                              :cache-result "unavailable"))
                 :cancel (lambda ()
                           (e-live-e2e--cancel-newest-request
                            (list new old)))
                 :thunk (nth 2 case))
              (error (setq condition caught))))
          (should (eq (car condition) (nth 0 case)))
          (should (= (length records) 1))
          (should (equal cancelled '(new)))
          (should (equal (plist-get (car records) :result)
                         (nth 1 case))))))))

(ert-deftest e-live-e2e-test-external-finalizer-preserves-adoption-checkpoint-after-late-failure ()
  "A late failure preserves an already established exact or summary adoption."
  (let (records)
    (cl-letf (((symbol-function 'e-live-e2e--profile-auth-available-p)
               (lambda (&rest _) t))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (record) (push record records))))
      (dolist (case
               (list
                (list "exact" 'error "provider/infrastructure-failure"
                      (lambda () (error "second-turn provider failure")))
                (list "summary" 'e-live-e2e-scenario-timeout
                      "inconclusive-timeout"
                      (lambda ()
                        (signal 'e-live-e2e-scenario-timeout
                                (list :deadline 120.0))))))
        (setq records nil)
        (let (condition)
          (condition-case caught
              (e-live-e2e--run-external-scenario
               :scenario 'responses-autonomous-curation-adoption
               :provider 'configured-provider
               :profile '(:name "Configured Responses"
                          :responses-transport http
                          :requires-openai-auth t)
               :model "gpt-5.6-sol"
               :timeout 120.0
               :started-at 100.0
               :capture (lambda ()
                          (list :semantic-result "unavailable"
                                :cache-result "unavailable"
                                :adoption-result "pass"
                                :composition-result "unavailable"
                                :failure-stage "none"
                                :adoption-disposition (nth 0 case)))
               :thunk (nth 3 case))
            (error (setq condition caught)))
          (should (eq (car condition) (nth 1 case)))
          (should (= (length records) 1))
          (let ((record (car records)))
            (should (equal (plist-get record :result) (nth 2 case)))
            (should (equal (plist-get record :adoption-result) "pass"))
            (should (equal (plist-get record :composition-result)
                           "unavailable"))
            (should (equal (plist-get record :failure-stage) "none"))
            (should (equal (plist-get record :adoption-disposition)
                           (nth 0 case)))))))))

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

(defun e-live-e2e--callback-work (id start)
  "Return a test-local cooperative Work fixture for callback START.
START is an e2e-only producer seam; production tools register canonical
`e-work-spec' values directly."
  (e-work-spec-create
   :id id
   :description (format "Run callback-backed e2e tool %s." id)
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'e2e
   :runner
   (lambda (handle arguments _context)
     (cl-labels
         ((adopt-request
           (request)
           (when request
             (setf (e-work-handle-metadata handle)
                   (append (e-work-handle-metadata handle)
                           (list :request request)))
             (when (e-tools-request-p request)
               (setf (e-tools-request-metadata request)
                     (append (e-tools-request-metadata request)
                             (list :work-id (e-work-handle-id handle)
                                   :work-handle handle))))
             (setf (e-work-handle-cancel-function handle)
                   (lambda (_handle)
                     (e-tools-cancel-request request)
                     t)))))
       (let ((request
              (e-tools--apply-start-with-optional-event
               start
               (list :arguments arguments
                     :on-done (lambda (value) (e-work-finish handle value))
                     :on-error (lambda (err) (e-work-fail handle err))
                     :on-request-start #'adopt-request)
               (lambda (_type payload) (e-work-progress handle payload)))))
         (adopt-request request)
         :deferred)))))

(defun e-live-e2e--slow-tool-register (registry &rest _context)
  "Register a cancellable slow tool in REGISTRY."
  (e-tools-register
   registry
   :name "e2e_slow"
   :description "Wait briefly before returning. Use only for e live e2e cancellation validation."
   :parameters '(:type "object" :properties nil)
   :work
   (e-live-e2e--callback-work
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

(defvar e-live-e2e--deterministic-tool-output "LIVE-TOOL-OUTPUT"
  "Output returned by the deterministic tool; adoption runs bind a nonce.")

(defun e-live-e2e--deterministic-tool-register (registry &rest _context)
  "Register a bounded-output tool in REGISTRY for continuation evidence."
  (e-tools-register
   registry
   :name "e2e_deterministic"
   :description "Return one bounded validation value exactly once."
   :parameters '(:type "object" :properties nil)
   :work
   (e-tools-cheap-work
    "e2e.live.deterministic"
    (lambda (_arguments)
      e-live-e2e--deterministic-tool-output))))

(defun e-live-e2e--deterministic-tool-layer ()
  "Return a tool layer with one bounded-output continuation test tool."
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

(ert-deftest e-live-e2e-test-deterministic-tool-output-can-be-bound-per-run ()
  "Adoption binds a nonce while existing deterministic callers keep the default."
  (let ((registry (e-tools-registry-create)))
    (e-live-e2e--deterministic-tool-register registry)
    (should (equal (plist-get
                    (e-tools-execute-batch
                     registry
                     '(:id "default" :name "e2e_deterministic"
                       :arguments nil))
                    :content)
                   "LIVE-TOOL-OUTPUT"))
    (let ((e-live-e2e--deterministic-tool-output "RUN-SENTINEL"))
      (should (equal (plist-get
                      (e-tools-execute-batch
                       registry
                       '(:id "adoption" :name "e2e_deterministic"
                         :arguments nil))
                      :content)
                     "RUN-SENTINEL")))))

(defmacro e-live-e2e--with-harness (spec &rest body)
  "Run BODY with a live HARNESS and SESSION-ID.
SPEC is (HARNESS SESSION-ID &key LAYERS EVENTS-VAR EVENTS-HOLDER).
When EVENTS-VAR is supplied, bind it to the newest-first public activity sink
events collected during BODY.  When EVENTS-HOLDER is supplied, its first cell
is updated through BODY so an outer finalizer can observe later events."
  (declare (indent 1))
  (let* ((harness (nth 0 spec))
        (session-id (nth 1 spec))
        (options (nthcdr 2 spec))
        (root (make-symbol "root"))
        (store (make-symbol "store"))
        (events (make-symbol "events"))
        (events-var (plist-get options :events-var))
        (events-holder (plist-get options :events-holder))
        (subscription (make-symbol "subscription")))
    `(progn
       (e-live-e2e--require-enabled)
       (let* ((,root (make-temp-file "e-live-e2e-" t))
              (,store (e-session-persistent-store-create ,root))
              (,harness (e-live-e2e--make-harness ,store))
              (,session-id
               (e-chat-sql-e2e-create-session
                ,harness :metadata (list :project-root ,root)))
              (,events nil)
              ,@(when events-var
                  `((,events-var nil)))
              (,subscription
               (e-harness-activity-subscribe
                ,harness
                (lambda (event)
                  (push event ,events)
                  ,@(when events-var
                      `((push event ,events-var)))
                  ,@(when events-holder
                      `((push event (car ,events-holder)))))
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
           (ignore-errors (e-harness-activity-unsubscribe ,harness ,subscription))
           (ignore-errors (e-session-sqlite-store-close ,store))
           (ignore-errors (delete-directory ,root t)))))))

(ert-deftest e-live-e2e-test-anthropic-profile-selection ()
  "The checked-in Anthropic config selects exact Opus 5.5 in isolated state."
  (let ((config-path (e-live-e2e--config-file)))
    (unless (and config-path
                 (file-readable-p config-path)
                 (equal (file-truename config-path)
                        (file-truename
                         e-live-e2e--checked-in-anthropic-config-file)))
      (ert-skip
       "Set E_E2E_CONFIG=e2e/e-e2e-config-anthropic.el for this selection test.")))
  (let ((e-live-e2e--config-loaded nil))
    (cl-letf (((symbol-function 'e-live-e2e--require-enabled)
               (lambda () (e-live-e2e--load-config))))
      (e-live-e2e--with-harness (harness session-id)
        (let* ((profile
                (e-anthropic-provider-profile e-anthropic-default-provider))
               (options (e-harness-display-options harness session-id)))
          (should (eq e-anthropic-default-provider 'eng-ai-gateway-opus-5-5))
          (should (equal e-anthropic-default-model "claude-opus-5-5"))
          (should (equal (plist-get profile :default-model)
                         "claude-opus-5-5"))
          (should (equal (plist-get options :model) "claude-opus-5-5"))
          (should (= (plist-get options :max-tokens)
                     e-anthropic-default-max-tokens))
          (should (equal (plist-get options :effort)
                         e-anthropic-default-effort))
          (should-not (plist-member profile :context-window))
          (should-not (plist-member profile :max-input-tokens)))))))

(defmacro e-live-e2e--with-responses-request-capture
    (profile request-bodies request-handles &rest body)
  "Run BODY while capturing native Responses request bodies and handles.
PROFILE selects the transport.  HTTP bodies are captured at the adapter
request-context boundary, while both transports capture handles from their
native request starter.  The supplied capture lists are newest-first."
  (declare (indent 3))
  (let ((profile-var (make-symbol "profile"))
        (original-http-start (make-symbol "original-http-start"))
        (original-websocket-start (make-symbol "original-websocket-start")))
    `(let ((,profile-var ,profile))
       (if (eq (plist-get ,profile-var :responses-transport) 'websocket)
         (let ((,original-websocket-start
                  (symbol-function 'e-openai-websocket-request-start)))
             (cl-letf
                 (((symbol-function 'e-openai-websocket-request-start)
                   (lambda (&rest args)
                     (let* ((entry
                             (list :body
                                   (copy-tree (plist-get args :body-data))
                                   :full-body
                                   (copy-tree (plist-get args :full-body-data))
                                   :session (plist-get args :session)
                                   :response-id nil))
                            (original-on-complete
                             (plist-get args :on-complete))
                            (args (plist-put
                                   (copy-sequence args)
                                   :on-complete
                                   (lambda (status)
                                     (setf (plist-get entry :response-id)
                                           (plist-get status :response-id))
                                     (when original-on-complete
                                       (funcall original-on-complete status))))))
                       (push entry ,request-bodies)
                       (let ((request
                              (apply ,original-websocket-start args)))
                       (push request ,request-handles)
                         request)))))
               ,@body))
         (let ((,original-http-start
                (symbol-function 'e-openai-http-request-start)))
           (cl-letf
               (((symbol-function 'e-openai-http-request-start)
                 (lambda (&rest args)
                   (push (list :body
                               (json-parse-string
                                (or (plist-get args :body) "{}")
                                :object-type 'plist
                                :array-type 'vector
                                :null-object nil
                                :false-object :json-false)
                               :full-body nil
                               :session nil)
                         ,request-bodies)
                   (let ((request (apply ,original-http-start args)))
                     (push request ,request-handles)
                     request))))
             ,@body))))))

(defun e-live-e2e--run-reasoning-summary-capability ()
  "Run one bounded configured Responses reasoning-summary capability probe.
The probe sends a single ordinary request with `reasoning.summary' set to
`auto'.  Its capture is content-free; the shared scenario boundary emits one
evidence record on every terminal path."
  (let* ((scenario-timeout (e-live-e2e--cache-scenario-timeout))
         (started-at (float-time))
         (provider-id e-openai-default-provider)
         (model e-openai-default-model)
         profile harness session-id (events-holder (list nil))
         request-bodies request-handles
         (semantic-result "unavailable")
         (scenario-result nil)
         (terminal-result nil))
    (e-live-e2e--run-external-scenario
     :scenario 'responses-reasoning-summary-capability
     :provider nil
     :profile profile
     :model model
     :timeout scenario-timeout
     :started-at started-at
     :evidence-schema-revision
     e-live-e2e--reasoning-summary-evidence-schema-revision
     :profile-function
     (lambda ()
       (e-live-e2e--require-enabled)
       (setq provider-id e-openai-default-provider
             model e-openai-default-model
             profile (e-openai-provider-profile provider-id))
       profile)
     :cancel (lambda ()
               (e-live-e2e--cancel-newest-request request-handles))
     :capture
     (lambda ()
       (let* ((ordered-entries (reverse request-bodies))
              (ordered-handles (reverse request-handles))
              (bodies
               (mapcar (lambda (entry)
                         (or (plist-get entry :full-body)
                             (plist-get entry :body)))
                       ordered-entries))
              (metadata
               (mapcar (lambda (handle)
                         (ignore-errors
                           (e-backend-request-metadata handle)))
                       ordered-handles))
              (usage-payloads
               (and harness session-id
                    (mapcar (lambda (event) (plist-get event :payload))
                            (e-live-e2e--activity-of-type
                             harness session-id 'token-usage))))
              (reasoning-state
               (e-live-e2e--captured-reasoning-state bodies)))
         (list
          :model (or (plist-get (car bodies) :model)
                     e-openai-default-model)
          :request-bodies bodies
          :request-metadata metadata
          :usage-payloads usage-payloads
          :reasoning-effort (plist-get reasoning-state :effort)
          :reasoning-summary (plist-get reasoning-state :summary)
          :probe-request-identity
          (plist-get reasoning-state :probe-request-identity)
          :dependency-identity
          (e-live-e2e--reasoning-summary-dependency-identity)
          :returned-summary-presence
          (e-live-e2e--reasoning-summary-presence
           (car events-holder)
           (and harness session-id
                (e-harness-messages harness session-id)))
          :evidence-schema-revision
          e-live-e2e--reasoning-summary-evidence-schema-revision
          :semantic-result semantic-result
          :scenario-result scenario-result
          :terminal-result terminal-result)))
     :thunk
     (lambda ()
       (unless (eq (e-openai-provider-wire-api profile) 'responses)
         (setq terminal-result "configuration-unavailable")
         (ert-skip "The configured provider is not a Responses profile."))
       (let ((transport (or (plist-get profile :responses-transport) 'http)))
         (unless (fboundp (if (eq transport 'websocket)
                              'e-openai-websocket-request-start
                            'e-openai-http-request-start))
           (setq terminal-result "configuration-unavailable")
           (ert-skip
            "The configured Responses transport starter is not loaded."))
         ;; Leave this set until the harness macro enters its body: if its
         ;; configuration gate skips before then, the finalizer keeps the
         ;; capability-specific configuration classification.
         (setq terminal-result "configuration-unavailable")
         (e-live-e2e--with-harness
             (live-harness live-session :events-holder events-holder)
           (setq terminal-result nil)
           (setq harness live-harness
                 session-id live-session)
           (setf (e-harness-default-options harness)
                 (plist-put
                  (copy-sequence (e-harness-default-options harness))
                  :reasoning-summary "auto"))
           (let* ((deadline (+ started-at scenario-timeout))
                  response)
             (e-live-e2e--with-responses-request-capture
                 profile request-bodies request-handles
               (setq response
                     (condition-case caught
                         (e-live-e2e--prompt-batch-before-deadline
                          harness session-id
                          "Reply with exactly CAPABILITY-PROBE-OK and no other words."
                          deadline)
                       (error
                        (if (e-live-e2e--reasoning-summary-endpoint-rejection-p
                             caught)
                            (progn
                              (setq terminal-result "unavailable")
                              (ert-skip
                               "The Responses endpoint rejected reasoning.summary."))
                          (signal (car caught) (cdr caught)))))))
             ;; Any normally settled response is a capability success.  The
             ;; returned answer and summary prose are deliberately irrelevant.
             (ignore response)
             (let* ((ordered-entries (reverse request-bodies))
                    (ordered-handles (reverse request-handles))
                    (bodies
                     (mapcar (lambda (entry)
                               (or (plist-get entry :full-body)
                                   (plist-get entry :body)))
                             ordered-entries))
                    (metadata
                     (mapcar (lambda (handle)
                               (ignore-errors
                                 (e-backend-request-metadata handle)))
                             ordered-handles))
                    (reasoning-state
                     (e-live-e2e--captured-reasoning-state bodies))
                    (identity-result
                     (e-live-e2e--reasoning-summary-captured-identity-result
                      profile bodies metadata e-openai-default-model))
                    (request-valid-p
                     (and (plist-get reasoning-state :valid-p)
                          (equal (plist-get reasoning-state :summary) "auto")))
                    (classification
                     (e-live-e2e--classify-reasoning-summary-capability
                      :identity-result identity-result
                      :request-valid-p request-valid-p
                      :completed-p t)))
               (setq semantic-result
                     (if (equal classification "pass") "pass" "failure")
                     scenario-result classification)
               (unless (equal classification "pass")
                 (ert-fail
                  "The captured Responses reasoning probe was invalid."))))))))))

(ert-deftest e-live-e2e-test-responses-reasoning-summary-capability ()
  "Probe the configured Responses backend for reasoning-summary support."
  (e-live-e2e--run-reasoning-summary-capability))

(ert-deftest e-live-e2e-test-responses-request-capture-selects-native-starter ()
  "The shared capture boundary follows each configured Responses transport."
  (dolist (transport '(http websocket))
    (let ((profile (list :responses-transport transport))
          (body '(:model "capture-test" :input []))
          request-bodies
          request-handles)
      (if (eq transport 'websocket)
          (cl-letf (((symbol-function
                      'e-openai-websocket-request-start)
                     (lambda (&rest _args)
                       (e-backend-request-create
                        :metadata '(:transport websocket)))))
            (e-live-e2e--with-responses-request-capture
                profile request-bodies request-handles
              (e-openai-websocket-request-start
               :body-data body :full-body-data body :session 'session)))
        (cl-letf (((symbol-function 'e-openai-http-request-start)
                   (lambda (&rest _args)
                     (e-backend-request-create
                      :metadata '(:transport url-retrieve)))))
          (e-live-e2e--with-responses-request-capture
              profile request-bodies request-handles
            (e-openai-http-request-start
             :url "https://capture.test"
             :body (json-encode body))))
      (should (= (length request-bodies) 1))
      (should (equal (plist-get (car request-bodies) :body) body))
      (should (= (length request-handles) 1))))))

(ert-deftest e-live-e2e-test-basic-assistant-response ()
  "A first live prompt returns a concrete assistant message."
  (e-live-e2e--with-harness (harness session-id)
    (let* ((nonce (e-live-e2e--nonce))
           (result (e-chat-sql-e2e-prompt-batch
                    harness session-id
                    (format "Reply with exactly this token and no extra words: %s"
                            nonce))))
      (unless (eq (plist-get result :status) 'done)
        (ert-fail (format "Live provider turn failed: %s"
                          (plist-get result :error))))
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
            (e-chat-sql-e2e-prompt-batch
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
              (e-chat-sql-e2e-prompt-batch
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

(ert-deftest e-live-e2e-test-follow-up-uses-session-context ()
  "A follow-up live prompt can use earlier transcript context."
  (e-live-e2e--with-harness (harness session-id)
    (let ((nonce (e-live-e2e--nonce)))
      (e-chat-sql-e2e-prompt-batch
       harness session-id
       (format "Remember this validation token for the next message: %s. Reply OK."
               nonce))
      (let ((result (e-chat-sql-e2e-prompt-batch
                     harness session-id
                     "Reply with only the validation token I asked you to remember.")))
        (should (e-live-e2e--contains-p
                 (e-live-e2e--assistant-content result)
                 nonce))))))

(ert-deftest e-live-e2e-test-tool-call-round-trip ()
  "The model can call a registered e tool and use its result."
  (e-live-e2e--with-harness (harness session-id :layers (list (e-live-e2e--tool-layer)))
    (let* ((nonce (e-live-e2e--nonce))
           (result (e-chat-sql-e2e-prompt-batch
                    harness session-id
                    (format
                     "Call e2e_echo exactly once with text %S. Then reply with only that returned text."
                     nonce)))
           (activities (e-session-local-activity-events
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

(defun e-live-e2e--run-autonomous-curation-adoption ()
  "Run the isolated naturalistic Responses curation-adoption scenario."
  (e-live-e2e--require-enabled)
  (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id)))
    (unless (eq (e-openai-provider-wire-api profile) 'responses)
      (ert-skip "The configured provider is not a Responses profile."))
    (let* ((tool-name "e2e_deterministic")
           (raw-tool-output (format "LIVE-ADOPTION-%s"
                                   (e-live-e2e--nonce)))
           (prompt (e-live-e2e--autonomous-adoption-prompt tool-name))
           (follow-up-prompt
            "What was the adoption sentinel from the lookup? Reply with exactly that value and no extra words.")
           (prompt-identity (e-live-e2e--sha256 prompt))
           (affordance-revision
            (format "%s" e-context-lifetime-curation-schema-revision))
           (presentation-revision
            (format "%s" e-context-lifetime-curation-presentation-revision)))
      (let ((e-context-lifetime-shadow-projection-enabled t)
            (e-live-e2e--deterministic-tool-output raw-tool-output))
        (e-live-e2e--with-harness
            (harness session-id
                     :layers (list (e-live-e2e--deterministic-tool-layer))
                     :events-var events)
          (let* ((scenario-timeout (e-live-e2e--cache-scenario-timeout))
                 (started-at (float-time))
                 (deadline (+ started-at scenario-timeout))
                 request-bodies
                 request-handles
                 first-result
                 second-result
                 second-request-count
                 curation-record
                 curation-arguments
                 curation-source-count
                 (curation-preparation-count 0)
                 adoption-disposition
                 (adoption-result "unavailable")
                 (composition-result "unavailable")
                 (failure-stage "none")
                 adoption-gates
                 (scenario-result nil)
                 (semantic-result "unavailable")
                 (cache-result "unavailable"))
            (e-live-e2e--run-external-scenario
             :scenario 'responses-autonomous-curation-adoption
             :provider provider-id
             :profile profile
             :model e-openai-default-model
             :timeout scenario-timeout
             :started-at started-at
             :cancel (lambda ()
                       (e-live-e2e--cancel-newest-request request-handles))
             :capture
             (lambda ()
               (let* ((ordered-entries (reverse request-bodies))
                      (ordered-handles (reverse request-handles))
                      (bodies (mapcar (lambda (entry) (plist-get entry :body))
                                     ordered-entries))
                      (metadata
                       (mapcar (lambda (handle)
                                 (ignore-errors
                                   (e-backend-request-metadata handle)))
                               ordered-handles))
                      (usage-payloads
                       (mapcar (lambda (event) (plist-get event :payload))
                               (e-live-e2e--activity-of-type
                                harness session-id 'token-usage))))
                 (setq cache-result
                       (if usage-payloads
                           (e-live-e2e--cache-result usage-payloads)
                         "unavailable"))
                 (list :model (or (plist-get (car (last bodies)) :model)
                                  e-openai-default-model)
                       :request-bodies bodies
                       :request-metadata metadata
                       :usage-payloads usage-payloads
                       :semantic-result semantic-result
                       :cache-result cache-result
                       :adoption-result adoption-result
                       :composition-result composition-result
                       :failure-stage failure-stage
                       :adoption-gates adoption-gates
                       :adoption-disposition adoption-disposition
                       :scenario-prompt-identity prompt-identity
                       :affordance-revision affordance-revision
                       :presentation-revision presentation-revision
                       :scenario-result scenario-result)))
             :thunk
             (lambda ()
               (cl-labels
                   ((classify (gates)
                      (setq adoption-gates gates)
                      (let ((classification
                             (apply
                              #'e-live-e2e--classify-autonomous-adoption
                              (append (list :identity-result "pass")
                                      gates))))
                        (setq adoption-result
                              (plist-get classification :adoption-result)
                              composition-result
                              (plist-get classification :composition-result)
                              failure-stage
                              (plist-get classification :failure-stage)
                              scenario-result
                              (plist-get classification :result)
                              semantic-result
                              (if (member (plist-get classification :result)
                                          '("pass"
                                            "product-contract-failure"))
                                  "pass"
                                "failure"))
                        classification))
                    (gates-for-stage (stage)
                      (let ((gates
                             (list :ordinary-tool-p t
                                   :prompt-control-p t
                                   :carrier-p t
                                   :valid-effect-p t
                                   :replacement-preserved-p t
                                   :audit-linked-p t
                                   :projection-p t
                                   :raw-excluded-p t
                                   :follow-up-p t)))
                        (plist-put
                         gates
                         (pcase stage
                           ("ordinary-tool" :ordinary-tool-p)
                           ("prompt-control" :prompt-control-p)
                           ("carrier" :carrier-p)
                           ("commit" :audit-linked-p)
                           ("projection" :projection-p)
                           ("raw-exclusion" :raw-excluded-p)
                           ("follow-up-answer" :follow-up-p))
                         nil)))
                    (fail (stage message)
                      (classify (gates-for-stage stage))
                      (ert-fail message))
                    (model-selection-failure (message)
                      (classify
                       '(:ordinary-tool-p t :prompt-control-p t
                         :carrier-p t :valid-effect-p nil
                         :replacement-preserved-p t :audit-linked-p t
                         :projection-p t :raw-excluded-p t :follow-up-p t))
                      (ert-fail message))
                    (require-gate (condition stage message)
                      (unless condition
                        (fail stage message)))
                    (input-has-role-p (input roles)
                      (seq-some
                       (lambda (item)
                         (member (plist-get item :role) roles))
                       (append input nil))))
                 (require-gate
                  (null (e-live-e2e--adoption-prompt-violations prompt))
                  "prompt-control"
                  "The naturalistic adoption prompt contains reserved vocabulary.")
                 (require-gate
                  (string-match-p (regexp-quote tool-name) prompt)
                  "prompt-control"
                  "The adoption prompt did not name the ordinary deterministic tool.")
                 (require-gate
                  (null (e-live-e2e--adoption-prompt-violations follow-up-prompt))
                  "prompt-control"
                  "The follow-up prompt contains reserved vocabulary.")
                 (let ((original-prepare
                        (symbol-function
                         'e-context-lifetime-prepare-curation-disposition)))
                   (cl-letf
                       (((symbol-function
                          'e-context-lifetime-prepare-curation-disposition)
                         (lambda (frame arguments response-entry-id
                                  &optional bytes-per-token)
                           (setq curation-preparation-count
                                 (1+ curation-preparation-count)
                                 curation-arguments (copy-tree arguments)
                                 curation-source-count
                                 (length
                                  (e-context-lifetime-frame-curation-sources
                                   frame bytes-per-token)))
                           (funcall original-prepare
                                    frame arguments response-entry-id
                                    bytes-per-token))))
                     (e-live-e2e--with-responses-request-capture
                         profile request-bodies request-handles
                       (setq first-result
                             (e-live-e2e--prompt-batch-before-deadline
                              harness session-id prompt deadline)))))
                 (let ((tool-starts
                        (e-live-e2e--activity-of-type
                         harness session-id 'tool-started))
                       (tool-finishes
                        (e-live-e2e--activity-of-type
                         harness session-id 'tool-finished))
                       (assistant (e-live-e2e--assistant-content first-result)))
                   (require-gate (= (length tool-starts) 1)
                                 "ordinary-tool"
                                 "The ordinary deterministic tool was not called exactly once.")
                   (require-gate (= (length tool-finishes) 1)
                                 "ordinary-tool"
                                 "The ordinary deterministic tool did not finish exactly once.")
                   (require-gate (equal (string-trim assistant) "READY")
                                 "prompt-control"
                                 "The first response did not answer READY.")
                   (require-gate (not (e-live-e2e--contains-p assistant raw-tool-output))
                                 "prompt-control"
                                 "The first response disclosed the deterministic sentinel."))
                 (let* ((ordered-bodies
                         (mapcar (lambda (entry) (plist-get entry :body))
                                 (reverse request-bodies)))
                        (source-bearing-index
                         (e-live-e2e--adoption-source-bearing-request-index
                          ordered-bodies raw-tool-output))
                        (source-bearing-body
                         (and (integerp source-bearing-index)
                              (nth source-bearing-index ordered-bodies)))
                        (carrier-present-p
                         (and source-bearing-body
                              (e-live-e2e--context-curation-carrier-p
                               source-bearing-body)
                              (e-live-e2e--responses-reasoning-auto-p
                               source-bearing-body))))
                   (require-gate carrier-present-p
                                 "carrier"
                                 "The reserved curation carrier was absent from the source-bearing continuation."))
                 (let* ((curations
                         (e-session-local-context-curations
                          (e-harness-sessions harness) session-id))
                        (record-count (length curations)))
                   (unless (= record-count 1)
                     (model-selection-failure
                      (format "Expected one valid autonomous curation effect, got %d."
                              record-count)))
                   (setq curation-record (car curations))
                   (unless (and (= (plist-get curation-record :record-version) 3)
                                (= curation-preparation-count 1)
                                (e-live-e2e--adoption-positive-effect-valid-p
                                 curation-arguments curation-source-count
                                 curation-record raw-tool-output))
                     (model-selection-failure
                      "The autonomous curation effect did not validate against its presented sources."))
                   (setq adoption-disposition
                         (e-live-e2e--adoption-record-disposition
                          curation-record raw-tool-output)))
                 (require-gate
                  (e-live-e2e--adoption-audit-linked-p
                   (e-harness-sessions harness) session-id curation-record
                   events)
                  "commit"
                  "The autonomous curation audit/control linkage was incomplete.")
                 ;; Adoption is independently established once the preserving
                 ;; effect and its exact audit linkage have been verified.  A
                 ;; later follow-up only establishes composition.
                 (setq adoption-result "pass")
                 (setq second-request-count (length request-bodies))
                 (e-live-e2e--with-responses-request-capture
                     profile request-bodies request-handles
                   (setq second-result
                         (e-live-e2e--prompt-batch-before-deadline
                          harness session-id follow-up-prompt deadline)))
                 (let* ((projection
                         (e-session-local-context-lifetime-projection
                          (e-harness-sessions harness) session-id))
                        (item (car (plist-get curation-record :items)))
                        (expected
                         (if (eq (plist-get item :kind) 'exact)
                             (plist-get item :value)
                           (plist-get item :text)))
                        (promotion-messages
                         (plist-get projection :promotion-messages))
                        (composed-body
                         (e-live-e2e--captured-body-at-boundary
                          request-bodies second-request-count))
                        (composed-input (and composed-body
                                             (append (plist-get composed-body :input)
                                                  nil)))
                        (composed-printed (prin1-to-string composed-input)))
                   (require-gate
                    (and (stringp expected)
                         (seq-some
                          (lambda (message)
                            (equal (plist-get message :content) expected))
                          promotion-messages)
                         (e-live-e2e--contains-p composed-printed expected))
                    "projection"
                    "The next request did not contain the selected durable replacement." )
                   (require-gate
                    (and composed-input
                         (not (string-match-p
                               "\\[ephemeral context source [0-9]+"
                               composed-printed))
                         (not (input-has-role-p composed-input '(tool tool-call
                                                                  "tool" "tool-call")))
                         (not (string-match-p
                               "tool-call-id\\|function_call\\|provider-replay-item"
                               composed-printed)))
                    "raw-exclusion"
                    "The next request replayed the consumed raw source or marker." )
                   (require-gate
                    (equal (string-trim
                            (e-live-e2e--assistant-content second-result))
                           raw-tool-output)
                    "follow-up-answer"
                    "The ordinary follow-up did not return the retained sentinel."))
                 (classify
                  '(:ordinary-tool-p t :prompt-control-p t :carrier-p t
                    :valid-effect-p t :replacement-preserved-p t
                    :audit-linked-p t :projection-p t :raw-excluded-p t
                    :follow-up-p t))))
            )))))))

(ert-deftest e-live-e2e-test-responses-autonomous-curation-adoption ()
  "A configured Responses model autonomously curates a future-turn tool result."
  (e-live-e2e--run-autonomous-curation-adoption))

(defun e-live-e2e--run-autonomous-curation-erase ()
  "Run the isolated naturalistic Responses explicit-erasure scenario.
The scenario makes one ordinary tool request, then verifies the curation-only
response as an audit/consumption operation through persistence and the
receipt projection.  Its evidence remains content-free; all source values
and provider arguments stay local to the scenario gates."
  (e-live-e2e--require-enabled)
  (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id)))
    (unless (eq (e-openai-provider-wire-api profile) 'responses)
      (ert-skip "The configured provider is not a Responses profile."))
    (let* ((tool-name "e2e_deterministic")
           (raw-tool-output (format "LIVE-ERASE-%s"
                                   (e-live-e2e--nonce)))
           (prompt (e-live-e2e--autonomous-erase-prompt tool-name))
           (prompt-identity (e-live-e2e--sha256 prompt))
           (affordance-revision
            (format "%s" e-context-lifetime-curation-schema-revision))
           (presentation-revision
            (format "%s" e-context-lifetime-curation-presentation-revision)))
      (let ((e-context-lifetime-shadow-projection-enabled t)
            (e-live-e2e--deterministic-tool-output raw-tool-output))
        (e-live-e2e--with-harness
            (harness session-id
                     :persistent t
                     :layers (list (e-harness-base-layer-create)
                                   (e-live-e2e--deterministic-tool-layer))
                     :events-var events)
          (let* ((scenario-timeout (e-live-e2e--cache-scenario-timeout))
                 (started-at (float-time))
                 (deadline (+ started-at scenario-timeout))
                 request-bodies
                 request-handles
                 first-result
                 tool-finished-before
                 receipt-prerequisite
                 curation-preparation
                 curation-arguments
                 curation-source-count
                 (curation-preparation-count 0)
                 ordered-bodies
                 source-bearing-index
                 source-bearing-body
                 ack-index
                 ack-body
                 stage-cardinality
                 response-entry-id
                 (adoption-result "unavailable")
                 (composition-result "unavailable")
                 (failure-stage "none")
                 (adoption-disposition "unavailable")
                 (semantic-result "unavailable")
                 (cache-result "unavailable")
                 (scenario-result nil)
                 erase-adoption-gates)
            (e-live-e2e--run-external-scenario
             :scenario 'responses-autonomous-curation-erase
             :provider provider-id
             :profile profile
             :model e-openai-default-model
             :timeout scenario-timeout
             :started-at started-at
             :cancel (lambda ()
                       (e-live-e2e--cancel-newest-request request-handles))
             :capture
             (lambda ()
               (let* ((entries (reverse request-bodies))
                      (bodies
                       (mapcar (lambda (entry)
                                 (or (plist-get entry :full-body)
                                     (plist-get entry :body)))
                               entries))
                      (handles (reverse request-handles))
                      (metadata
                       (mapcar (lambda (handle)
                                 (ignore-errors
                                   (e-backend-request-metadata handle)))
                               handles))
                      (usage-payloads
                       (mapcar (lambda (event) (plist-get event :payload))
                               (e-live-e2e--activity-of-type
                                harness session-id 'token-usage))))
                 (setq cache-result
                       (if usage-payloads
                           (e-live-e2e--cache-result usage-payloads)
                         "unavailable"))
                 (list :model (or (plist-get (car bodies) :model)
                                  e-openai-default-model)
                       :request-bodies bodies
                       :request-metadata metadata
                       :usage-payloads usage-payloads
                       :semantic-result semantic-result
                       :cache-result cache-result
                       :adoption-result adoption-result
                       :composition-result composition-result
                       :failure-stage failure-stage
                       :adoption-disposition adoption-disposition
                       :erase-adoption-gates erase-adoption-gates
                       :scenario-prompt-identity prompt-identity
                       :affordance-revision affordance-revision
                       :presentation-revision presentation-revision
                       :scenario-result scenario-result)))
             :thunk
             (lambda ()
               (cl-labels
                   ((classify (gates)
                      (setq erase-adoption-gates gates)
                      (let ((classification
                             (apply #'e-live-e2e--classify-autonomous-erase
                                    (append (list :identity-result "pass")
                                            gates))))
                        (setq adoption-result
                              (plist-get classification :adoption-result)
                              composition-result
                              (plist-get classification :composition-result)
                              failure-stage
                              (plist-get classification :failure-stage)
                              adoption-disposition
                              (plist-get classification :adoption-disposition)
                              scenario-result
                              (plist-get classification :result)
                              semantic-result
                              (if (member (plist-get classification :result)
                                          '("pass"
                                            "product-contract-failure"))
                                  "pass"
                                "failure"))
                        classification))
                    (gates-for-stage (stage)
                      (let ((gates
                             (list :ordinary-tool-p t
                                   :prompt-control-p t
                                   :carrier-p t
                                   :current-answer-p t
                                   :effect-present-p t
                                   :explicit-erasure-p t
                                   :audit-linked-p t
                                   :consumed-p t
                                   :no-promotion-p t
                                   :receipt-prerequisite-p t
                                   :receipt-erasure-id-p t
                                   :erasure-persisted-p t
                                   :receipt-suppressed-p t
                                   :details-preserved-p t
                                   :reopen-audit-p t
                                   :reopen-erasure-p t
                                   :reopen-no-promotion-p t
                                   :reopen-receipt-p t
                                   )))
                        (plist-put
                         gates
                         (pcase stage
                           ("ordinary-tool" :ordinary-tool-p)
                           ("prompt-control" :prompt-control-p)
                           ("carrier" :carrier-p)
                           ("current-answer" :current-answer-p)
                           ("model-selection" :explicit-erasure-p)
                           ("no-promotion" :no-promotion-p)
                           ("commit" :audit-linked-p)
                           ("receipt-prerequisite" :receipt-prerequisite-p)
                           ("erasure-persisted" :erasure-persisted-p)
                           ("receipt-erasure-id" :receipt-erasure-id-p)
                           ("receipt-suppressed" :receipt-suppressed-p)
                           ("details-preserved" :details-preserved-p)
                           ("reopen-audit" :reopen-audit-p)
                           ("reopen-erasure" :reopen-erasure-p)
                           ("reopen-no-promotion" :reopen-no-promotion-p)
                           ("reopen-receipt" :reopen-receipt-p)
                           )
                         nil)))
                    (fail (stage message)
                      (classify (gates-for-stage stage))
                      (ert-fail message))
                    (model-selection-failure (effect-p explicit-p message)
                      (classify
                       (list :ordinary-tool-p t :prompt-control-p t
                             :carrier-p t :current-answer-p t
                             :effect-present-p effect-p
                             :explicit-erasure-p explicit-p
                             :audit-linked-p t :consumed-p t
                             :no-promotion-p t :erasure-persisted-p t
                             :receipt-prerequisite-p t
                             :receipt-erasure-id-p t
                             :receipt-suppressed-p t :details-preserved-p t
                             :reopen-audit-p t :reopen-erasure-p t
                             :reopen-no-promotion-p t :reopen-receipt-p t))
                      (ert-fail message))
                    (require-gate (condition stage message)
                      (unless condition
                        (fail stage message))))
                 (require-gate
                  (null (e-live-e2e--adoption-prompt-violations prompt))
                  "prompt-control"
                  "The naturalistic erase prompt contains reserved vocabulary.")
                 (require-gate
                  (and (string-match-p (regexp-quote tool-name) prompt)
                       (= (length (split-string prompt tool-name t)) 2))
                  "prompt-control"
                  "The erase prompt did not name the ordinary tool exactly once.")
                 (let ((original-prepare
                        (symbol-function
                         'e-context-lifetime-prepare-curation-disposition)))
                   (condition-case caught
                       (cl-letf
                           (((symbol-function
                              'e-context-lifetime-prepare-curation-disposition)
                             (lambda (frame arguments response-entry-id
                                      &optional bytes-per-token)
                               (setq curation-preparation-count
                                     (1+ curation-preparation-count)
                                     curation-arguments (copy-tree arguments)
                                     curation-source-count
                                     (length
                                      (e-context-lifetime-frame-curation-sources
                                       frame bytes-per-token)))
                               (let ((prepared
                                      (funcall original-prepare
                                               frame arguments response-entry-id
                                               bytes-per-token)))
                                 (setq curation-preparation
                                       (copy-tree prepared))
                                 prepared))))
                         (e-live-e2e--with-responses-request-capture
                             profile request-bodies request-handles
                           (setq first-result
                                 (e-live-e2e--prompt-batch-before-deadline
                                  harness session-id prompt deadline))))
                     (e-context-lifetime-invalid-record
                      ;; Convert the typed core failure into classified ERT
                      ;; evidence while preserving its details in the message.
                     (classify
                       '(:ordinary-tool-p t :prompt-control-p t :carrier-p t
                         :current-answer-p t :effect-present-p t
                         :explicit-erasure-p nil
                         :audit-linked-p t :consumed-p t :no-promotion-p t
                         :receipt-prerequisite-p t :receipt-erasure-id-p t
                         :erasure-persisted-p t :receipt-suppressed-p t
                         :details-preserved-p t
                         :reopen-audit-p t :reopen-erasure-p t
                         :reopen-no-promotion-p t :reopen-receipt-p t))
                      (ert-fail
                       (format "Invalid curation disposition: %S" caught)))
                     (error (signal (car caught) (cdr caught)))))
                 (let ((tool-starts
                        (e-live-e2e--activity-of-type
                         harness session-id 'tool-started))
                       (tool-finishes
                        (e-live-e2e--activity-of-type
                         harness session-id 'tool-finished))
                       (assistant (e-live-e2e--assistant-content first-result)))
                   (require-gate (= (length tool-starts) 1)
                                 "ordinary-tool"
                                 "The ordinary deterministic tool was not called once.")
                   (require-gate (= (length tool-finishes) 1)
                                 "ordinary-tool"
                                 "The ordinary deterministic tool did not finish once.")
                   (setq tool-finished-before (car tool-finishes))
                   (setq receipt-prerequisite
                         (e-live-e2e--ordinary-tool-receipt-prerequisite
                          harness session-id tool-finishes))
                   (require-gate
                    receipt-prerequisite
                    "receipt-prerequisite"
                    "The ordinary tool did not produce one projected live receipt with details.")
                   (require-gate
                    (equal (string-trim assistant)
                           (concat (reverse raw-tool-output)))
                    "current-answer"
                    "The first answer was not the exact reversed sentinel."))
                 (setq ordered-bodies
                       (mapcar (lambda (entry) (plist-get entry :body))
                               (reverse request-bodies)))
                 (let* ((unique-bodies
                         (cl-remove-duplicates ordered-bodies :test #'equal))
                        (unique-index
                         (e-live-e2e--adoption-source-bearing-request-index
                          unique-bodies raw-tool-output)))
                   (setq source-bearing-body
                         (and (integerp unique-index)
                              (nth unique-index unique-bodies))
                         source-bearing-index
                         (and source-bearing-body
                              (cl-position source-bearing-body ordered-bodies
                                           :test #'equal))))
                 (let ((carrier-present-p
                        (and source-bearing-body
                             (e-live-e2e--context-curation-carrier-p
                              source-bearing-body)
                             (e-live-e2e--responses-reasoning-auto-p
                              source-bearing-body))))
                   (require-gate carrier-present-p
                                 "carrier"
                                 "The source continuation lacked the strict carrier or reasoning auto."))
                 (let* ((effect-present-p
                         (= curation-preparation-count 1))
                        (explicit-erasure-p
                         (and effect-present-p
                              (e-live-e2e--adoption-erase-effect-valid-p
                               curation-arguments curation-source-count))))
                   (unless effect-present-p
                     (model-selection-failure
                      nil nil "The model did not select one curation effect."))
                   (unless explicit-erasure-p
                     (model-selection-failure
                      t nil
                      "The selected effect was not one explicit erase of a single source."))
                   (setq ack-index
                         (cl-loop for body in
                                  (nthcdr (1+ source-bearing-index)
                                          ordered-bodies)
                                  for index from (1+ source-bearing-index)
                                  when (e-live-e2e--context-curation-ack-body-p
                                        body)
                                  return index))
                   (setq ack-body (and ack-index (nth ack-index ordered-bodies)))
                   (let* ((turn-id (plist-get first-result :id))
                          (retry-events
                           (and turn-id
                                (e-live-e2e--turn-retrying-events
                                 harness session-id turn-id))))
                     (setq stage-cardinality
                           (and source-bearing-index ack-body
                                (e-live-e2e--captured-turn-stage-cardinality
                                 ordered-bodies
                                 (list
                                  (list :stage 'initial
                                        :body (car ordered-bodies))
                                  (list :stage 'source-bearing
                                        :body source-bearing-body)
                                  (list :stage 'curation-ack :body ack-body
                                        :predicate
                                        #'e-live-e2e--context-curation-ack-body-p))
                                 retry-events)))
                     (require-gate
                      (and (plist-get stage-cardinality :valid-p)
                           (= (plist-get stage-cardinality
                                         :logical-curation-count)
                              1))
                                   "carrier"
                                   "The curation effect/ack was not one bounded causal stage."))
                   (let* ((store (e-harness-sessions harness))
                          (links
                           (e-live-e2e--autonomous-erase-audit-links
                            store session-id))
                          (tool-finished
                           (plist-get receipt-prerequisite :event))
                          (receipt
                           (and tool-finished
                                (plist-get (plist-get tool-finished :payload)
                                           :receipt)))
                          (tool-call-id
                           (and receipt (plist-get receipt :tool-call-id)))
                          (details-uri
                           (and receipt (plist-get receipt :details-uri)))
                          (erasure-record
                          (plist-get curation-preparation :erasure-record))
                          (package
                           (plist-get curation-preparation :package))
                          (actual-erasures
                           (e-session-local-context-erasures store session-id))
                          (actual-erasure (car actual-erasures))
                          (expected-erased-tool-call-ids
                           (and erasure-record
                                (e-context-lifetime-curation-erasure-tool-call-ids
                                 erasure-record)))
                          (actual-session-erased-tool-call-ids
                           (cl-mapcan
                            #'e-context-lifetime-curation-erasure-tool-call-ids
                            actual-erasures))
                          (package-entry
                           (seq-find
                            (lambda (entry)
                              (eq (plist-get entry :type)
                                  'context-curation-package))
                            (e-session-local-current-path store session-id)))
                          (erased-tool-call-ids
                           (e-session-local-erased-tool-call-ids store session-id))
                          (tool-finishes-after
                           (e-live-e2e--activity-of-type
                            harness session-id 'tool-finished))
                          (receipt-projection
                           (e-harness-base-receipt-projection
                            harness session-id
                            :erased-tool-call-ids erased-tool-call-ids)))
                     (setq response-entry-id
                           (plist-get links :response-entry-id))
                     (require-gate (and links (plist-get links :control-p))
                                   "commit"
                                   "The erase response control was not linked exactly once.")
                     (require-gate (and links (plist-get links :consumed-p))
                                   "commit"
                                   "The erase response did not consume its live frame.")
                     (require-gate
                      (and curation-preparation
                           erasure-record
                           package
                           (null (plist-get package :promotion))
                           (equal (plist-get package :erasure)
                                  erasure-record)
                           (null (plist-get curation-preparation :record))
                           package-entry
                           (null (plist-get package-entry :promotion))
                           (null (e-session-local-context-promotions store session-id))
                           (null (e-session-local-context-curations store session-id)))
                      "no-promotion"
                      "Erase-only curation persisted a promotion component.")
                     (require-gate
                      (and (= (length actual-erasures) 1)
                           (equal actual-erasure erasure-record)
                           (equal (plist-get erasure-record
                                             :response-entry-id)
                                  response-entry-id)
                           (equal
                            (e-context-lifetime-curation-erasure-tool-call-ids
                             erasure-record)
                            expected-erased-tool-call-ids)
                           (equal actual-session-erased-tool-call-ids
                                  expected-erased-tool-call-ids)
                           (equal erased-tool-call-ids
                                  expected-erased-tool-call-ids))
                      "erasure-persisted"
                      "The selected path did not expose one durable tool erasure.")
                     (require-gate
                      (and (= (length expected-erased-tool-call-ids) 1)
                           (equal tool-call-id
                                  (car expected-erased-tool-call-ids)))
                      "receipt-erasure-id"
                      "The durable receipt did not identify the erased tool call.")
                     (require-gate
                      (and (null (plist-get receipt-projection :receipts))
                           (= (plist-get receipt-projection :selected-count) 0)
                           (= (plist-get receipt-projection :total-count) 0)
                           (= (plist-get receipt-projection :omitted-count) 0)
                           (null (plist-get receipt-projection :messages)))
                      "receipt-suppressed"
                      "The erased receipt or an aggregate mark remained projected.")
                     (require-gate
                      (and (= (length tool-finishes-after) 1)
                           (equal (car tool-finishes-after)
                                  tool-finished-before)
                           (listp receipt)
                           (stringp details-uri)
                           (e-session-tmp-reference-available-p
                            harness session-id details-uri))
                      "details-preserved"
                      "Erasure changed durable activity or temporary details.")
                     (e-session-flush-write-queue store)
                            (let* ((reopened
                             (e-session-persistent-store-create
                              (e-session-store-directory store)))
                            (reopened-links
                             (e-live-e2e--autonomous-erase-audit-links
                              reopened session-id))
                            (reopened-erasures
                             (e-session-local-context-erasures reopened session-id))
                            (reopened-erasure (car reopened-erasures))
                            (reopened-erased-tool-call-ids
                             (e-session-local-erased-tool-call-ids
                              reopened session-id))
                            (reopened-receipt-event
                             (seq-find
                              (lambda (event)
                                (let ((candidate
                                       (plist-get
                                        (plist-get event :payload) :receipt)))
                                  (and (eq (plist-get event :event-type)
                                           'tool-finished)
                                       (equal (plist-get candidate :tool-call-id)
                                              tool-call-id))))
                              (e-session-local-activity-events reopened session-id))))
                       (require-gate
                        (and reopened-links
                             (equal (plist-get reopened-links :response-entry-id)
                                    response-entry-id))
                        "reopen-audit"
                        "The curation audit control did not replay with its response identity.")
                       (require-gate
                        (and (= (length reopened-erasures) 1)
                             (equal reopened-erasure erasure-record)
                             (equal reopened-erased-tool-call-ids
                                    expected-erased-tool-call-ids))
                        "reopen-erasure"
                        "The erasure record or selected erased IDs failed replay.")
                       (require-gate
                        (and (null (e-session-local-context-curations
                                   reopened session-id))
                             (null (e-session-local-context-promotions
                                    reopened session-id)))
                        "reopen-no-promotion"
                        "Replay exposed a promotion or curation projection for erase-only state.")
                       (require-gate
                        (and reopened-receipt-event
                             (e-live-e2e--reopened-receipt-equal-p
                              tool-finished-before reopened-receipt-event))
                        "reopen-receipt"
                        "The durable tool receipt did not replay with equivalent semantics.")
                       (classify
                        '(:ordinary-tool-p t :prompt-control-p t :carrier-p t
                          :current-answer-p t :effect-present-p t
                          :explicit-erasure-p t :audit-linked-p t :consumed-p t
                          :no-promotion-p t :erasure-persisted-p t
                          :receipt-prerequisite-p t :receipt-erasure-id-p t
                          :receipt-suppressed-p t :details-preserved-p t
                          :reopen-audit-p t :reopen-erasure-p t
                          :reopen-no-promotion-p t :reopen-receipt-p t))))))))))))))

(ert-deftest e-live-e2e-test-responses-autonomous-curation-erase ()
  "A configured Responses model autonomously erases a current-turn source."
  (e-live-e2e--run-autonomous-curation-erase))

(ert-deftest e-live-e2e-test-reopened-receipt-equality-is-direct ()
  "A replayed receipt normalizes only persisted enum spellings.
The fixture mirrors live activity, where enum values are symbols, and JSON
replay, where those same values are strings."
  (let* ((receipt '(:tool-call-id "call-1"
                    :tool "tool-1"
                    :status ok
                    :details-uri "tmp://tool-invocations/s/c.json"
                    :details-lifetime session-tmp))
         (original (list :event-type 'tool-finished
                         :payload (list :receipt receipt)))
         (replayed-receipt '(:details-lifetime "session-tmp"
                             :details-uri "tmp://tool-invocations/s/c.json"
                             :status "ok"
                             :tool "tool-1"
                             :tool-call-id "call-1"))
         (reopened (list :event-type 'tool-finished
                         :payload (list :receipt replayed-receipt))))
    (should (e-live-e2e--reopened-receipt-equal-p original reopened))
    (let ((wrong-call (copy-tree reopened)))
      (plist-put (plist-get wrong-call :payload)
                 :receipt
                 (plist-put (copy-tree replayed-receipt)
                            :tool-call-id "call-other"))
      (should-not (e-live-e2e--reopened-receipt-equal-p original wrong-call)))
    (let ((wrong-uri (copy-tree reopened)))
      (plist-put (plist-get wrong-uri :payload)
                 :receipt
                 (plist-put (copy-tree replayed-receipt)
                            :details-uri "tmp://tool-invocations/s/other.json"))
      (should-not (e-live-e2e--reopened-receipt-equal-p original wrong-uri)))
    ))

(ert-deftest e-live-e2e-test-autonomous-erase-scenario-layers-provide-support ()
  "The configured bare factory exposes receipt/details owners from its layers."
  (let (factory-store)
    (cl-letf (((symbol-function 'e-live-e2e--require-enabled)
               (lambda () t))
              ((symbol-function 'e-default-chat-harness-spec)
               (lambda ()
                 (list :factory
                       (lambda (&rest arguments)
                         (setq factory-store
                               (plist-get arguments :sessions))
                         (e-harness-create
                          :backend (e-backend-fake-create :items nil)
                          :sessions factory-store)))))
              ((symbol-function 'e-chat-sql-e2e-create-session)
               (lambda (harness &rest _arguments)
                 (e-harness-create-session harness :id "test-session")
                 "test-session")))
      (e-live-e2e--with-harness
          (harness session-id
                   :layers (list (e-harness-base-layer-create)
                                 (e-live-e2e--deterministic-tool-layer)))
        (let* ((capabilities
                (e-harness-effective-capabilities harness session-id))
               (capability-ids (mapcar #'e-capability-id capabilities))
               (base-context
                (seq-find (lambda (capability)
                            (eq (e-capability-id capability)
                                'harness-base-context))
                          capabilities)))
          (should factory-store)
          (should (memq 'harness-base-context capability-ids))
          (should (memq 'tool-invocation-details capability-ids))
          (should base-context)
          (should
           (seq-some
            (lambda (provider)
              (eq (e-context-provider-name provider)
                  'tool-invocation-receipts))
            (e-capability-context-providers base-context)))
          (should
           (seq-some
            (lambda (hook)
              (equal (e-hook-id hook) "40-tool-invocation-details"))
            (e-hooks-for-point
             (e-harness-hooks harness)
             :invocation-details)))
          (let ((e-harness-activity-trusted-tool-details-uri
                 "tmp://tool-invocations/test/call-1.json"))
            (cl-letf (((symbol-function 'e-session-tmp-reference-available-p)
                       (lambda (&rest _) t)))
              (e-harness-activity-emit-turn-event
               harness session-id "turn-1" 'tool-finished
               '(:tool-call (:id "call-1" :name "probe")
                 :result (:tool-call-id "call-1" :name "probe" :status ok
                         :content "ok")))
              (let* ((provider
                      (car (e-capability-context-providers base-context)))
                     (messages
                      (e-context-provider-build
                       provider :harness harness :session-id session-id
                       :turn-id "turn-1"))
                     (printed (prin1-to-string messages)))
                (should messages)
                (should (string-match-p "call-1" printed))))))))))

(ert-deftest e-live-e2e-test-autonomous-erase-receipt-prerequisite-is-direct ()
  "The erase prerequisite requires one projected direct live receipt."
  (let ((event '(:event-type tool-finished
                 :payload (:receipt (:tool-call-id "call-1"
                                      :details-uri
                                      "tmp://tool-invocations/t/c.json"))))
        (projection '(:receipts ((:tool-call-id "call-1"))
                      :selected-count 1 :total-count 1 :omitted-count 0))
        available-arguments)
    (cl-letf (((symbol-function 'e-session-tmp-reference-available-p)
               (lambda (&rest arguments)
                 (setq available-arguments arguments)
                 t))
              ((symbol-function 'e-harness-base-receipt-projection)
               (lambda (&rest _) projection)))
      (let ((prerequisite
             (e-live-e2e--ordinary-tool-receipt-prerequisite
              'harness "session-1" (list event))))
        (should prerequisite)
        (should (equal (plist-get prerequisite :tool-call-id) "call-1"))
        (should (equal (plist-get prerequisite :details-uri)
                       "tmp://tool-invocations/t/c.json"))
        (should available-arguments)
        (should-not
         (e-live-e2e--ordinary-tool-receipt-prerequisite
          'harness "session-1"
          (list (list :event-type 'tool-finished :payload nil))))))))

(ert-deftest e-live-e2e-test-autonomous-adoption-runner-keeps-cleanup-outside-call ()
  "The adoption runner passes only its declared keywords before cleanup."
  (let (received request-attempted)
    (cl-letf (((symbol-function 'e-live-e2e--require-enabled)
               (lambda () t))
              ((symbol-function 'e-openai-provider-profile)
               (lambda (&rest _)
                 '(:name "Responses test" :wire-api responses
                   :responses-transport http)))
              ((symbol-function 'e-openai-provider-wire-api)
               (lambda (&rest _) 'responses))
              ((symbol-function 'e-live-e2e--make-harness)
               (lambda (&rest _)
                 (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
              ((symbol-function 'e-chat-sql-e2e-create-session)
               (lambda (&rest _) "test-session"))
              ((symbol-function 'e-harness-activity-subscribe)
               (lambda (&rest _) 'test-subscription))
              ((symbol-function 'e-harness-set-intrinsic-capabilities)
               (lambda (&rest _) nil))
              ((symbol-function 'e-harness-activity-unsubscribe)
               (lambda (&rest _) nil))
              ((symbol-function 'make-temp-file)
               (lambda (&rest _) "/private/tmp/e-adoption-runner-test"))
              ((symbol-function 'delete-directory)
               (lambda (&rest _) nil))
              ((symbol-function 'e-live-e2e--run-external-scenario)
               (lambda (&rest arguments)
                 (setq received arguments)
                 'runner-stubbed))
              ((symbol-function 'e-chat-sql-e2e-prompt-batch)
               (lambda (&rest _)
                 (setq request-attempted t)
                 (error "provider request should not run")))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (&rest _) nil)))
      (let ((e-openai-default-provider 'test-provider))
        (e-live-e2e--run-autonomous-curation-adoption))
      (should (equal
               (cl-loop for (key _value) on received by #'cddr collect key)
               '(:scenario :provider :profile :model :timeout :started-at
                 :cancel :capture :thunk)))
      (should (functionp (plist-get received :capture)))
      (should (functionp (plist-get received :thunk)))
      (should (functionp (plist-get received :cancel)))
      (should-not request-attempted))))

(ert-deftest e-live-e2e-test-autonomous-erase-runner-keeps-cleanup-outside-call ()
  "The erase runner passes only its declared keywords before cleanup."
  (let (received request-attempted)
    (cl-letf (((symbol-function 'e-live-e2e--require-enabled)
               (lambda () t))
              ((symbol-function 'e-openai-provider-profile)
               (lambda (&rest _)
                 '(:name "Responses test" :wire-api responses
                   :responses-transport http)))
              ((symbol-function 'e-openai-provider-wire-api)
               (lambda (&rest _) 'responses))
              ((symbol-function 'e-live-e2e--make-harness)
               (lambda (&rest _)
                 (e-harness-create
                  :backend (e-backend-fake-create :items nil))))
              ((symbol-function 'e-session-persistent-store-create)
               (lambda (&rest _) (e-session-store-create)))
              ((symbol-function 'e-chat-sql-e2e-create-session)
               (lambda (&rest _) "test-session"))
              ((symbol-function 'e-harness-activity-subscribe)
               (lambda (&rest _) 'test-subscription))
              ((symbol-function 'e-harness-set-intrinsic-capabilities)
               (lambda (&rest _) nil))
              ((symbol-function 'e-harness-activity-unsubscribe)
               (lambda (&rest _) nil))
              ((symbol-function 'make-temp-file)
               (lambda (&rest _) "/private/tmp/e-erase-runner-test"))
              ((symbol-function 'delete-directory)
               (lambda (&rest _) nil))
              ((symbol-function 'e-live-e2e--run-external-scenario)
               (lambda (&rest arguments)
                 (setq received arguments)
                 'runner-stubbed))
              ((symbol-function 'e-chat-sql-e2e-prompt-batch)
               (lambda (&rest _)
                 (setq request-attempted t)
                 (error "provider request should not run")))
              ((symbol-function 'e-live-e2e--report-external-evidence)
               (lambda (&rest _) nil)))
      (let ((e-openai-default-provider 'test-provider))
        (e-live-e2e--run-autonomous-curation-erase))
      (should (equal
               (cl-loop for (key _value) on received by #'cddr collect key)
               '(:scenario :provider :profile :model :timeout :started-at
                 :cancel :capture :thunk)))
      (should (functionp (plist-get received :capture)))
      (should (functionp (plist-get received :thunk)))
      (should (functionp (plist-get received :cancel)))
      (should-not request-attempted))))

(ert-deftest e-live-e2e-test-provider-lifecycle-events-are-durable ()
  "Live provider start and finish events are emitted and persisted."
  (e-live-e2e--with-harness (harness session-id)
    (let ((nonce (e-live-e2e--nonce)))
      (e-chat-sql-e2e-prompt-batch
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
    (e-chat-sql-e2e-prompt-batch
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
  (e-live-e2e--with-harness (harness session-id)
    (let* ((store-dir (e-session-store-directory (e-harness-sessions harness)))
           (nonce (e-live-e2e--nonce)))
      (e-chat-sql-e2e-prompt-batch
       harness session-id
       (format "Reply with exactly this persistence token: %s" nonce))
      (let* ((reloaded-store (e-session-persistent-store-create store-dir))
             (messages (e-session-local-messages reloaded-store session-id)))
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
      (e-chat-sql-e2e-prompt-batch
       harness session-id
       (format "Remember this compaction token: %s. Reply OK." nonce))
      (e-chat-sql-e2e-prompt-batch
       harness session-id
       "Reply with one short sentence confirming you still have the token.")
      (let ((record (e-harness-compact-session-batch
                     harness session-id
                     :reason 'manual
                     :keep-recent-tokens 1)))
        (should (plist-get record :summary))
        (should (e-session-local-latest-valid-compaction
                 (e-harness-sessions harness) session-id))
        (should (e-live-e2e--activity-of-type
                 harness session-id 'compaction-finished))))))

(ert-deftest e-live-e2e-test-provider-anchor-candidate-recorded-when-supported ()
  "Continuation-capable providers record provider anchor candidates."
  (e-live-e2e--with-harness (harness session-id)
    (e-chat-sql-e2e-prompt-batch
     harness session-id
     "Reply with exactly: ANCHOR-CHECK")
    ;; Continuation support is backend-specific; assert anchors when the
    ;; configured backend records them, otherwise skip rather than assume.
    (if (e-session-local-provider-anchors (e-harness-sessions harness) session-id)
        (should (e-session-local-provider-anchors
                 (e-harness-sessions harness) session-id))
      (ert-skip "The configured live backend does not record continuation anchors."))))

(ert-deftest e-live-e2e-test-openai-codex-store-false-continues ()
  "ChatGPT Codex sends the inherited observation as a late developer frontier."
  (unless (fboundp 'e-openai-websocket-request-start)
    (ert-skip "The OpenAI Responses WebSocket adapter is not loaded."))
  (e-live-e2e--require-enabled)
    (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id)))
    (unless (e-openai-profile-builtin-codex-p provider-id profile)
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
            (symbol-function 'e-openai-websocket-request-start))
            request-bodies
            request-handles
            first-assistant
            first-durable-assistant
            second-assistant
            second-turn-id)
        (cl-letf (((symbol-function 'e-openai-websocket-request-start)
                   (lambda (&rest args)
                     (push (copy-tree (plist-get args :body-data)) request-bodies)
                     (let ((request (apply original-start args)))
                       (push request request-handles)
                       request))))
          (let ((first-result
                 (e-chat-sql-e2e-prompt-batch
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
             (e-session-local-provider-anchors
              (e-harness-sessions harness) session-id)))
          (setq current-state new-marker)
          (let ((second-result
                 (e-chat-sql-e2e-prompt-batch
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
                (e-session-local-provider-anchors
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

(defun e-live-e2e--run-canonical-tool-heavy (&optional chatgpt-only)
  "Run the canonical tool-heavy scenario for the configured Responses profile.
When CHATGPT-ONLY is non-nil, retain the strict built-in Codex WebSocket
continuation and socket assertions used by the compatibility selector."
  (e-live-e2e--require-enabled)
  (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id))
         (transport (or (plist-get profile :responses-transport) 'http)))
    (unless (eq (e-openai-provider-wire-api profile) 'responses)
      (ert-skip "The configured provider is not a Responses profile."))
    (when chatgpt-only
      (unless (e-openai-profile-builtin-codex-p provider-id profile)
        (ert-skip "The configured provider is not the exact built-in ChatGPT Codex profile."))
      (should (eq (plist-get profile :response-store) :json-false))
      (should-not (plist-member profile :websocket-idle-close-seconds)))
    (unless (fboundp (if (eq transport 'websocket)
                         'e-openai-websocket-request-start
                       'e-openai-http-request-start))
      (ert-skip "The configured Responses transport starter is not loaded."))
    (let* ((old-marker "LIVE-INITIAL-OBSERVATION")
           (new-marker "LIVE-CURRENT-OBSERVATION")
           (current-state old-marker)
           (stable-guidance
            (concat
             "LIVE-STABLE-INSTRUCTIONS "
             (mapconcat #'identity
                        (make-list 500 "stable tool cache probe directive")
                        " ")))
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
               :instructions stable-guidance
               :context-providers (list provider)))))
           (raw-tool-output "LIVE-TOOL-OUTPUT"))
      (let ((e-context-lifetime-shadow-projection-enabled t))
        (e-live-e2e--with-harness
          (harness session-id
                   :layers (list layer (e-live-e2e--deterministic-tool-layer)))
          (let* ((scenario-timeout (e-live-e2e--cache-scenario-timeout))
                 (started-at (float-time))
                 (deadline (+ started-at scenario-timeout))
                 request-bodies
                 request-handles
                 captured-tool-sources
                 captured-curation-arguments
                 curation-record
                 first-turn-id
                 tool-turn-id
                 warm-turn-id
                 tool-turn-start-count
                 warm-turn-start-count
                 (cache-result nil)
                 (semantic-result "unavailable"))
            (e-live-e2e--run-external-scenario
             :scenario (if chatgpt-only
                           'chatgpt-canonical-tool-heavy
                         'responses-canonical-tool-heavy)
             :provider provider-id
             :profile profile
             :model e-openai-default-model
             :timeout scenario-timeout
             :started-at started-at
             :cancel (lambda ()
                       (e-live-e2e--cancel-newest-request request-handles))
             :capture
             (lambda ()
               (let ((ordered-entries (reverse request-bodies))
                     (ordered-handles (reverse request-handles)))
                 (list
                  :model (plist-get (plist-get (car ordered-entries) :body)
                                    :model)
                  :request-bodies
                  (mapcar (lambda (entry) (plist-get entry :body))
                          ordered-entries)
                  :request-metadata
                  (mapcar (lambda (handle)
                            (ignore-errors
                              (e-backend-request-metadata handle)))
                          ordered-handles)
                  :usage-payloads
                  (mapcar (lambda (event) (plist-get event :payload))
                          (e-live-e2e--activity-of-type
                           harness session-id 'token-usage))
                  :semantic-result semantic-result
                  :cache-result cache-result)))
             :thunk
             (lambda ()
               (let ((original-prepare
                      (symbol-function
                       'e-context-lifetime-prepare-curation-disposition)))
                 (cl-labels
                     ((run-prompts ()
                        (let ((first-result
                               (e-live-e2e--prompt-batch-before-deadline
                                harness session-id
                                "Reply with exactly LIVE-R0-READY. Do not call tools, context-curate, or repeat observation markers."
                                deadline)))
                          (setq first-turn-id (plist-get first-result :id))
                          (should (stringp first-turn-id))
                          (should (equal
                                   (string-trim
                                    (e-live-e2e--assistant-content first-result))
                                   "LIVE-R0-READY"))
                          (should-not
                           (e-live-e2e--contains-p
                            (e-live-e2e--assistant-content first-result)
                            old-marker)))
                        (setq tool-turn-start-count (length request-bodies))
                        (let ((tool-result
                               (e-live-e2e--prompt-batch-before-deadline
                                harness session-id
                                "Call e2e_deterministic exactly once. After its result arrives, call the reserved context-curate carrier exactly once. For context-curate, send exactly one summary object: its sources array must contain only the one numeric source label whose displayed exact value is the result returned by e2e_deterministic, and its text must be exactly LIVE-CURATED-FACT. Omit keep and erase, omit every other displayed source label, do not add another summary, and do not call any other tool. Then reply with exactly LIVE-R2-READY and no other text."
                                deadline)))
                          (setq tool-turn-id (plist-get tool-result :id))
                          (setq curation-record
                                (car (last (e-session-local-context-curations
                                            (e-harness-sessions harness)
                                            session-id))))
                          (should (stringp tool-turn-id))
                          (should (equal
                                   (string-trim
                                    (e-live-e2e--assistant-content tool-result))
                                   "LIVE-R2-READY")))
                        (setq warm-turn-start-count (length request-bodies))
                        (setq current-state new-marker)
                        (let ((warm-result
                               (e-live-e2e--prompt-batch-before-deadline
                                harness session-id
                                "Reply with exactly the current observation marker from your instructions and no other text; do not call context-curate."
                                deadline)))
                          (setq warm-turn-id (plist-get warm-result :id))
                          (should (stringp warm-turn-id))
                          (should (equal
                                   (string-trim
                                    (e-live-e2e--assistant-content warm-result))
                                   new-marker)))))
                   (cl-letf
                       (((symbol-function
                          'e-context-lifetime-prepare-curation-disposition)
                         (lambda (frame arguments response-entry-id
                                  &optional bytes-per-token)
                           (setq captured-curation-arguments
                                 (copy-tree arguments))
                           (let ((matching-sources
                                  (seq-filter
                                   (lambda (source)
                                     (equal (plist-get source :value)
                                            raw-tool-output))
                                   (e-context-lifetime-frame-curation-sources
                                    frame bytes-per-token))))
                             (setq captured-tool-sources
                                   (cl-remove-duplicates
                                    (append captured-tool-sources
                                            (copy-tree matching-sources))
                                    :test (lambda (left right)
                                            (equal
                                             (plist-get left :source-fingerprint)
                                             (plist-get right :source-fingerprint)))))
                             (funcall original-prepare
                                      frame arguments response-entry-id
                                      bytes-per-token)))))
                     (e-live-e2e--with-responses-request-capture
                         profile request-bodies request-handles
                       (run-prompts))))
            (let* ((ordered-entries (reverse request-bodies))
                   (ordered-bodies
                    (mapcar (lambda (entry) (plist-get entry :body))
                            ordered-entries))
                   (ordered-handles (reverse request-handles))
                   (response-ids
                    (delq nil
                          (mapcar (lambda (entry)
                                    (plist-get entry :response-id))
                                  ordered-entries)))
                   (first-turn-bodies
                    (e-live-e2e--captured-bodies-between
                     request-bodies 0 tool-turn-start-count))
                   (tool-turn-bodies
                    (e-live-e2e--captured-bodies-between
                     request-bodies tool-turn-start-count
                     warm-turn-start-count))
                   (warm-turn-bodies
                    (e-live-e2e--captured-bodies-between
                     request-bodies warm-turn-start-count
                     (length ordered-bodies)))
                   (first-turn-retry-events
                    (e-live-e2e--turn-retrying-events
                     harness session-id first-turn-id))
                   (tool-turn-retry-events
                    (e-live-e2e--turn-retrying-events
                     harness session-id tool-turn-id))
                   (warm-turn-retry-events
                    (e-live-e2e--turn-retrying-events
                     harness session-id warm-turn-id))
                   (tool-followup-index
                    (cl-loop for body in (cdr tool-turn-bodies)
                             for index from 1
                             unless (or (equal body (car tool-turn-bodies))
                                        (e-live-e2e--context-curation-ack-body-p
                                         body))
                             return index))
                   (first-body (car first-turn-bodies))
                   (tool-body (car tool-turn-bodies))
                   (tool-followup-body
                    (and tool-followup-index
                         (nth tool-followup-index tool-turn-bodies)))
                   (curation-ack-index
                    (and tool-followup-index
                         (cl-loop for body in
                                  (nthcdr (1+ tool-followup-index)
                                          tool-turn-bodies)
                                  for index from (1+ tool-followup-index)
                                  when (e-live-e2e--context-curation-ack-body-p
                                        body)
                                  return index)))
                   (curation-ack-bodies
                    (and curation-ack-index
                         (nthcdr curation-ack-index tool-turn-bodies)))
                   (curation-ack-body (car curation-ack-bodies))
                   (tool-stage-cardinality
                    (and tool-body tool-followup-body curation-ack-body
                         (e-live-e2e--captured-turn-stage-cardinality
                          tool-turn-bodies
                          (list
                           (list :stage 'initial :body tool-body)
                           (list :stage 'tool-followup
                                 :body tool-followup-body)
                           (list :stage 'curation-ack
                                 :body curation-ack-body
                                 :predicate
                                 #'e-live-e2e--context-curation-ack-body-p))
                          tool-turn-retry-events)))
                   (warm-body (car warm-turn-bodies))
                   (tool-followup-input
                    (append (plist-get tool-followup-body :input) nil))
                   (curation-ack-input
                    (and curation-ack-body
                         (append (plist-get curation-ack-body :input) nil)))
                   (warm-input (append (plist-get warm-body :input) nil))
                   (warm-input-printed (prin1-to-string warm-input))
                   (warm-body-printed (prin1-to-string warm-body))
                   (request-metadata
                    (mapcar (lambda (handle)
                              (e-backend-request-metadata handle))
                            ordered-handles))
                   (first-diagnostics
                    (e-live-e2e--request-diagnostics
                     (nth 0 request-metadata)))
                   (tool-followup-diagnostics
                    (e-live-e2e--request-diagnostics
                     (nth (1+ tool-turn-start-count) request-metadata)))
                   (warm-diagnostics
                    (e-live-e2e--request-diagnostics
                     (nth warm-turn-start-count
                          request-metadata)))
                   (warm-finished-events
                    (seq-filter
                     (lambda (event)
                       (equal (plist-get event :turn-id) warm-turn-id))
                     (e-live-e2e--activity-of-type
                      harness session-id 'provider-request-finished)))
                   (warm-finished-event (car warm-finished-events))
                   (warm-finished-payload
                    (plist-get warm-finished-event :payload))
                   (warm-usage-events
                    (seq-filter
                     (lambda (event)
                       (equal (plist-get event :turn-id) warm-turn-id))
                     (e-live-e2e--activity-of-type
                      harness session-id 'token-usage)))
                   (warm-cache-result
                    (e-live-e2e--cache-result
                     (when warm-usage-events
                       (list (plist-get (car warm-usage-events) :payload)))))
                   (warm-metrics nil))
              (setq cache-result warm-cache-result)
              (should first-body)
              (should tool-body)
              (should tool-followup-body)
              (should warm-body)
              (should (= (length ordered-handles)
                         (length ordered-bodies)))
              (when (eq transport 'websocket)
                (should (= (length response-ids)
                           (length ordered-bodies))))
              ;; The tool turn has one ordinary tool continuation.  Any
              ;; further request in that same turn, and any extra request in
              ;; another turn, must be an exact retry of its causal stage or
              ;; the one logical reserved acknowledgement.
              (should tool-stage-cardinality)
              (should (plist-get tool-stage-cardinality :valid-p))
              (should (= (plist-get tool-stage-cardinality
                                    :logical-curation-count)
                         1))
              (should
               (plist-get
                (e-live-e2e--captured-turn-stage-cardinality
                 first-turn-bodies
                 (list (list :stage 'initial :body first-body))
                 first-turn-retry-events)
                :valid-p))
              (should
               (plist-get
                (e-live-e2e--captured-turn-stage-cardinality
                 warm-turn-bodies
                 (list (list :stage 'initial :body warm-body))
                 warm-turn-retry-events)
                :valid-p))
              (dolist (body curation-ack-bodies)
                (should (e-live-e2e--context-curation-ack-body-p body)))
              (when (eq transport 'http)
                (should curation-ack-body))
              (should (string-match-p (regexp-quote old-marker)
                                      (prin1-to-string first-body)))
              (should (string-match-p (regexp-quote old-marker)
                                      (prin1-to-string tool-body)))
              (should (= (length
                          (e-live-e2e--activity-of-type
                           harness session-id 'tool-started))
                         1))
              (should (= (length
                          (e-live-e2e--activity-of-type
                           harness session-id 'tool-finished))
                         1))
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
                       (append (plist-get tool-followup-body :tools) nil)))
              (should (seq-some
                       (lambda (tool)
                         (member (plist-get tool :name)
                                 '("context-curate" context-curate)))
                       (append (plist-get warm-body :tools) nil)))
              (should (and curation-record
                           (= (plist-get curation-record :record-version) 3)))
              (let ((item (car (plist-get curation-record :items))))
                (should (eq (plist-get item :kind) 'summary))
                (should (equal (plist-get item :text) "LIVE-CURATED-FACT"))
                (should captured-tool-sources)
                (unless
                    (seq-some
                     (lambda (source)
                       (member (plist-get source :source-fingerprint)
                               (plist-get item :source-fingerprints)))
                     captured-tool-sources)
                  (ert-fail
                   (format "curation arguments %S with captured tool sources %S do not match curation item %S"
                           captured-curation-arguments
                           captured-tool-sources item))))
              (should (= (length (e-session-local-context-curations
                                  (e-harness-sessions harness)
                                  session-id))
                         1))
              (should (seq-some
                       (lambda (item)
                         (and (equal (plist-get item :type)
                                     "function_call_output")
                              (equal (plist-get item :output)
                                     raw-tool-output)))
                       tool-followup-input))
              (let* ((curation-call-index
                       (and curation-ack-input
                            (cl-position-if
                             (lambda (item)
                               (and (equal (plist-get item :type)
                                           "function_call")
                                    (member (plist-get item :name)
                                            '("context-curate" context-curate))))
                             curation-ack-input)))
                     (curation-call
                      (and curation-call-index
                           (nth curation-call-index curation-ack-input)))
                     (curation-output-index
                      (and curation-call
                           (cl-position-if
                            (lambda (item)
                              (and (equal (plist-get item :type)
                                          "function_call_output")
                                   (equal (plist-get item :output) "")
                                   (equal (plist-get item :call_id)
                                          (plist-get curation-call :call_id))))
                            curation-ack-input)))
                     (curation-output
                      (and curation-output-index
                           (nth curation-output-index curation-ack-input))))
                (when curation-ack-bodies
                  (should (integerp curation-call-index))
                  (should (integerp curation-output-index))
                  (should (< curation-call-index curation-output-index))
                  (should (equal (plist-get curation-call :name)
                                 "context-curate"))
                  (should (equal (plist-get curation-output
                                            :output)
                                 ""))))
              (if (eq transport 'websocket)
                  (let ((r1 (nth 1 response-ids)))
                    (dolist (response-id response-ids)
                      (should (stringp response-id)))
                    (should (equal (plist-get tool-followup-body
                                              :previous_response_id)
                                   r1))
                    (should (eq (plist-get tool-followup-diagnostics
                                           :websocket-request-mode)
                                'incremental)))
                (dolist (body ordered-bodies)
                  (should-not (plist-member body :previous_response_id)))
                (should-not response-ids)
                (should (eq (plist-get first-diagnostics
                                       :responses-transport)
                            'http)))
              (should-not (plist-member warm-body :previous_response_id))
              (if (eq transport 'websocket)
                  (progn
                    (should (eq (plist-get warm-diagnostics
                                           :websocket-request-mode)
                                'full))
                    (should (numberp (plist-get warm-diagnostics
                                                :websocket-idle-close-seconds)))
                    (should (>= (plist-get warm-diagnostics
                                            :websocket-idle-close-seconds)
                                 0))
                    (should (eq (plist-get warm-diagnostics
                                           :websocket-reused)
                                t)))
                (should (eq (plist-get warm-diagnostics
                                       :responses-transport)
                            'http))
                (should-not (plist-get warm-diagnostics
                                       :websocket-connection-id)))
              ;; Keep semantic endpoint status separate from bounded metrics.
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
                (should (or (eq (plist-get warm-metrics
                                           :cached-input-tokens)
                                'unavailable)
                            (numberp (plist-get warm-metrics
                                              :cached-input-tokens))))
                (if (eq transport 'websocket)
                    (should (stringp (plist-get warm-metrics :connection-id)))
                  (should-not (plist-get warm-metrics :connection-id))))
              (when (eq transport 'websocket)
                (should (equal (plist-get first-diagnostics
                                          :websocket-connection-id)
                               (plist-get warm-diagnostics
                                          :websocket-connection-id))))
              (should warm-finished-event)
              (should (= (length warm-finished-events) 1))
              (when (eq transport 'websocket)
                (let ((finished-diagnostics
                       (plist-get warm-finished-payload :diagnostics)))
                  (should (eq (plist-get finished-diagnostics
                                         :websocket-request-mode)
                              'full))
                  (should (numberp
                           (plist-get finished-diagnostics
                                      :websocket-idle-close-seconds)))
                  (should (>= (plist-get finished-diagnostics
                                          :websocket-idle-close-seconds)
                               0))
                  (should-not (plist-get finished-diagnostics
                                         :previous-response-id-present))
                  (dolist (response-id response-ids)
                    (should-not
                     (string-match-p
                      (regexp-quote response-id)
                      (prin1-to-string finished-diagnostics))))
                  (should-not
                   (string-match-p
                    (regexp-quote raw-tool-output)
                    (prin1-to-string finished-diagnostics)))))
              (should-not (string-match-p (regexp-quote old-marker)
                                          warm-body-printed))
              (should-not (string-match-p (regexp-quote raw-tool-output)
                                          warm-body-printed))
              (should (string-match-p (regexp-quote new-marker)
                                      warm-body-printed))
              (should (string-match-p
                       (regexp-quote "LIVE-STABLE-INSTRUCTIONS")
                       warm-body-printed))
              (should (string-match-p (regexp-quote "LIVE-CURATED-FACT")
                                      warm-body-printed))
              (should (string-match-p
                       (regexp-quote "Call e2e_deterministic exactly once")
                       warm-input-printed))
              ;; The consumed tool-result frame's presentation marker must
              ;; not survive into the next canonical request.  The new
              ;; frontier may legitimately have its own marker.
              (should captured-tool-sources)
              (dolist (source captured-tool-sources)
                (let ((consumed-marker (plist-get source :marker)))
                (should-not
                 (seq-some
                  (lambda (item)
                    (let ((content (plist-get item :content)))
                      (and (vectorp content)
                           (= (length content) 1)
                           (equal (plist-get (aref content 0) :text)
                                  consumed-marker))))
                  warm-input))))
              ;; This is the provider-only empty acknowledgement for the
              ;; reserved curation call, not durable canonical context.
              (should-not
               (seq-some
                (lambda (item)
                  (and (equal (plist-get item :type)
                              "function_call_output")
                       (equal (plist-get item :output) "")))
                warm-input))
              (should-not
               (seq-some
                (lambda (item)
                  (member (plist-get item :type)
                          '("function_call" "function_call_output"
                            "provider-replay-item" "reasoning"
                            function_call function_call_output
                            provider-replay-item reasoning)))
                warm-input))
              (dolist (response-id response-ids)
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
              ))))))))))

(ert-deftest e-live-e2e-test-chatgpt-canonical-tool-heavy ()
  "A tool turn has an immediate continuation and a canonical next request."
  (e-live-e2e--run-canonical-tool-heavy t))

(ert-deftest e-live-e2e-test-responses-canonical-tool-heavy ()
  "A configured Responses profile completes the canonical tool scenario."
  (e-live-e2e--run-canonical-tool-heavy nil))

(ert-deftest e-live-e2e-test-openai-store-false-full-replay ()
  "The configured OpenAI provider accepts encrypted reasoning full replay."
  (e-live-e2e--require-enabled)
  (let ((profile (e-openai-provider-profile e-openai-default-provider)))
    (unless (and (eq (e-openai-provider-wire-api profile) 'responses)
                 (eq (plist-get profile :response-store) :json-false))
      (ert-skip "The configured provider is not an unstored Responses backend.")))
  (e-live-e2e--with-harness
      (harness session-id :layers (list (e-live-e2e--tool-layer)))
    (let (request-bodies request-handles)
      (e-live-e2e--with-responses-request-capture
          profile request-bodies request-handles
        (e-chat-sql-e2e-prompt-batch
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
        (when-let* ((websocket-session
                    (plist-get (car (last request-bodies)) :session)))
          (e-openai-websocket-session-close websocket-session))
        (e-chat-sql-e2e-prompt-batch
         harness session-id
         "Reply with exactly: FULL-REPLAY-ACCEPTED"))
      (let* ((first-entry (car (last request-bodies)))
             (full-body (or (plist-get first-entry :full-body)
                            (plist-get first-entry :body)))
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

(defun e-live-e2e--run-canonical-warm-prefix (&optional chatgpt-only)
  "Run the canonical warm-prefix scenario for a Responses profile.
CHATGPT-ONLY preserves the compatibility selector's strict built-in Codex
WebSocket and socket-replacement assertions."
  (e-live-e2e--require-enabled)
  (let* ((provider-id e-openai-default-provider)
         (profile (e-openai-provider-profile provider-id))
         (transport (or (plist-get profile :responses-transport) 'http)))
    (unless (eq (e-openai-provider-wire-api profile) 'responses)
      (ert-skip "The configured provider is not a Responses profile."))
    (when chatgpt-only
      (unless (e-openai-profile-builtin-codex-p provider-id profile)
        (ert-skip "The configured provider is not the exact built-in ChatGPT Codex profile."))
      (should (eq (plist-get profile :response-store) :json-false))
      (should (eq (plist-get profile :observation-delivery) 'inherited)))
    (unless (fboundp (if (eq transport 'websocket)
                         'e-openai-websocket-request-start
                       'e-openai-http-request-start))
      (ert-skip "The configured Responses transport starter is not loaded."))
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
        (unless (e-openai-profile-model-gpt56-or-later-p
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
               request-bodies
               request-handles
               first-turn-id
               middle-turn-id
               latest-turn-id
               middle-turn-start-count
               latest-turn-start-count
               (cache-result nil)
               (semantic-result "unavailable"))
          (e-live-e2e--run-external-scenario
           :scenario (if chatgpt-only
                         'chatgpt-canonical-warm-prefix
                       'responses-canonical-warm-prefix)
           :provider provider-id
           :profile profile
           :model e-openai-default-model
           :timeout scenario-timeout
           :started-at started-at
           :cancel (lambda ()
                     (e-live-e2e--cancel-newest-request request-handles))
           :capture
           (lambda ()
             (let* ((ordered-entries (reverse request-bodies))
                    (ordered-handles (reverse request-handles))
                    (usage-events
                     (e-live-e2e--activity-of-type
                      harness session-id 'token-usage)))
               (list
                :model (plist-get (plist-get (car ordered-entries) :body)
                                  :model)
                :request-bodies
                (mapcar (lambda (entry) (plist-get entry :body))
                        ordered-entries)
                :request-metadata
                (mapcar (lambda (handle)
                          (ignore-errors
                            (e-backend-request-metadata handle)))
                        ordered-handles)
                :usage-payloads
                (mapcar (lambda (event) (plist-get event :payload))
                        usage-events)
                :semantic-result semantic-result
                :cache-result cache-result)))
           :thunk
           (lambda ()
             (e-live-e2e--with-responses-request-capture
                 profile request-bodies request-handles
               (let ((first-result
                      (e-live-e2e--prompt-batch-before-deadline
                       harness session-id
                       "Reply with exactly: CROSS-TURN-ONE" deadline)))
                 (setq first-turn-id (plist-get first-result :id)))
               (setq middle-turn-start-count (length request-bodies))
               (setq current-state "live state two")
               (let ((middle-result
                      (e-live-e2e--prompt-batch-before-deadline
                       harness session-id
                       "Reply with exactly: CROSS-TURN-TWO" deadline)))
                 (setq middle-turn-id (plist-get middle-result :id)))
               ;; Only WebSocket profiles have a retained connection to
               ;; replace.  Each HTTP request is already an independent pair.
               (when (eq transport 'websocket)
                 (when-let* ((session
                             (plist-get (car (last request-bodies)) :session)))
                   (e-openai-websocket-session-close session)))
               (setq latest-turn-start-count (length request-bodies))
               (setq current-state "live state three")
               (let ((latest-result
                      (e-live-e2e--prompt-batch-before-deadline
                       harness session-id
                       "Reply with exactly: CROSS-TURN-THREE" deadline)))
                 (setq latest-turn-id (plist-get latest-result :id))))
          (let* ((requests (e-live-e2e--activity-of-type
                            harness session-id 'provider-request-started))
                 (latest (nth latest-turn-start-count requests))
                 (diagnostics
                  (plist-get (plist-get latest :payload) :diagnostics))
                 (ordered-entries (reverse request-bodies))
                 (ordered-bodies
                  (mapcar (lambda (entry) (plist-get entry :body))
                          ordered-entries))
                 (ordered-handles (reverse request-handles))
                 (first-turn-bodies
                  (e-live-e2e--captured-bodies-between
                   request-bodies 0 middle-turn-start-count))
                 (middle-turn-bodies
                  (e-live-e2e--captured-bodies-between
                   request-bodies middle-turn-start-count
                   latest-turn-start-count))
                 (latest-turn-bodies
                  (e-live-e2e--captured-bodies-between
                   request-bodies latest-turn-start-count
                   (length ordered-bodies)))
                 (first-turn-retry-events
                  (e-live-e2e--turn-retrying-events
                   harness session-id first-turn-id))
                 (middle-turn-retry-events
                  (e-live-e2e--turn-retrying-events
                   harness session-id middle-turn-id))
                 (latest-turn-retry-events
                  (e-live-e2e--turn-retrying-events
                   harness session-id latest-turn-id))
                 (first-body (car first-turn-bodies))
                 (middle-body (car middle-turn-bodies))
                 (latest-body (car latest-turn-bodies))
                 (request-metadata
                  (mapcar #'e-backend-request-metadata ordered-handles))
                 (first-diagnostics
                  (e-live-e2e--request-diagnostics
                   (nth 0 request-metadata)))
                 (middle-diagnostics
                  (e-live-e2e--request-diagnostics
                   (nth middle-turn-start-count request-metadata)))
                 (latest-diagnostics
                  (e-live-e2e--request-diagnostics
                   (nth latest-turn-start-count request-metadata)))
                 (first-breakpoint
                  (e-live-e2e--input-block-with-property
                   (plist-get first-body :input)
                   :prompt_cache_breakpoint))
                 (latest-instructions (plist-get latest-body :instructions))
                 (latest-current-input
                  (seq-find
                   (lambda (item)
                     (string-match-p "live state three"
                                     (prin1-to-string item)))
                   (append (plist-get latest-body :input) nil)))
                 (stable-first
                  (seq-find
                   (lambda (item)
                     (e-live-e2e--contains-p
                      (prin1-to-string item) stable-guidance))
                   (append (plist-get first-body :input) nil)))
                 (stable-latest
                  (seq-find
                   (lambda (item)
                     (e-live-e2e--contains-p
                      (prin1-to-string item) stable-guidance))
                   (append (plist-get latest-body :input) nil)))
                 (usage-events (e-live-e2e--activity-of-type
                                harness session-id 'token-usage))
                 (retained-usage-events
                  (seq-filter
                   (lambda (event)
                     (equal (plist-get event :turn-id) middle-turn-id))
                   usage-events))
                 (replacement-usage-events
                  (seq-filter
                   (lambda (event)
                     (equal (plist-get event :turn-id) latest-turn-id))
                   usage-events))
                 (retained-usage
                  (plist-get (car retained-usage-events) :payload))
                 (replacement-usage
                  (plist-get (car replacement-usage-events) :payload))
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
            (ert-info ((format "Responses diagnostics: %S; tool differences: %S"
                               diagnostics tool-differences))
              (should first-body)
              (should middle-body)
              (should latest-body)
              (should (= (length ordered-handles)
                         (length ordered-bodies)))
              (should
               (plist-get
                (e-live-e2e--captured-turn-stage-cardinality
                 first-turn-bodies
                 (list (list :stage 'initial :body first-body))
                 first-turn-retry-events)
                :valid-p))
              (should
               (plist-get
                (e-live-e2e--captured-turn-stage-cardinality
                 middle-turn-bodies
                 (list (list :stage 'initial :body middle-body))
                 middle-turn-retry-events)
                :valid-p))
              (should
               (plist-get
                (e-live-e2e--captured-turn-stage-cardinality
                 latest-turn-bodies
                 (list (list :stage 'initial :body latest-body))
                 latest-turn-retry-events)
                :valid-p))
              (should (stringp (plist-get first-body :prompt_cache_key)))
              (should (equal (plist-get first-body :prompt_cache_key)
                             (plist-get middle-body :prompt_cache_key)))
              (should (equal (plist-get first-body :prompt_cache_key)
                             (plist-get latest-body :prompt_cache_key)))
              (should (equal (plist-get latest-body :include)
                             ["reasoning.encrypted_content"]))
              (dolist (body ordered-bodies)
                (should-not (plist-member body :previous_response_id)))
              (should (equal latest-instructions
                             "You are a helpful assistant."))
              (should latest-current-input)
              (should (e-live-e2e--contains-p
                       (prin1-to-string latest-body)
                       "live state three"))
              (should stable-first)
              (should (equal stable-first stable-latest))
              (should (member (plist-get diagnostics :prompt-cache-mode)
                              '("explicit" "implicit-segmented")))
              (should (eq (plist-get diagnostics :observation-delivery)
                          'inherited))
              (should-not (plist-get diagnostics
                                     :replaceable-current-state-present))
              (if (eq transport 'websocket)
                  (should (eq (plist-get diagnostics :provider-anchor-safety)
                              'hold-inherited-observation))
                (should (eq (plist-get diagnostics :provider-anchor-safety)
                            'hold-unavailable-capability)))
              (when (equal (plist-get diagnostics :prompt-cache-mode)
                           "explicit")
                (should first-breakpoint)
                (should (equal (plist-get first-breakpoint
                                          :prompt_cache_breakpoint)
                               '(:mode "explicit")))
                (should (equal (plist-get latest-body :prompt_cache_options)
                               '(:mode "explicit"))))
              (if (eq transport 'websocket)
                  (progn
                    (should (eq (plist-get diagnostics
                                           :websocket-request-mode)
                                'full))
                    (should-not (plist-get diagnostics :websocket-reused))
                    (should (eq (plist-get middle-diagnostics
                                           :websocket-reused)
                                t))
                    (should-not (plist-get latest-diagnostics
                                           :websocket-reused))
                    (should-not
                     (equal (plist-get middle-diagnostics
                                       :websocket-connection-id)
                            (plist-get latest-diagnostics
                                       :websocket-connection-id))))
                (dolist (metadata request-metadata)
                  (should (eq (plist-get metadata :transport) 'url-retrieve))
                  (should-not (plist-get metadata :websocket-connection-id))
                  (should-not (plist-get metadata :websocket-request-mode)))
                (should (eq (plist-get first-diagnostics
                                       :responses-transport)
                            'http)))
              (setq cache-result computed-cache-result)
              (setq semantic-result "pass")
              ;; Request one is the intentionally cold prefill.  Require both
              ;; measured warm targets, including HTTP's independent request 2
              ;; and request 3 rather than inventing socket replacement.
              (cond
               ((equal computed-cache-result "warm")
                (should (equal retained-cache-result "warm"))
                (should (equal replacement-cache-result "warm")))
               ((equal computed-cache-result "product-contract-failure")
                (ert-fail
                 "The provider explicitly reported zero cached tokens for a warm target."))
               (t
                (ert-skip
                 "Cached-token usage was unavailable; external cache evidence is inconclusive."))))))))))))

(ert-deftest e-live-e2e-test-chatgpt-canonical-warm-prefix ()
  "A canonical warm-prefix pair is measured on retained and replaced sockets."
  (e-live-e2e--run-canonical-warm-prefix t))

(ert-deftest e-live-e2e-test-responses-canonical-warm-prefix ()
  "A configured Responses profile supplies a canonical warm-prefix pair."
  (e-live-e2e--run-canonical-warm-prefix nil))

(ert-deftest e-live-e2e-test-active-request-can-be-cancelled ()
  "An active live turn can be cancelled through the harness."
  (e-live-e2e--with-harness
      (harness session-id :layers (list (e-live-e2e--tool-layer))
               :events-var events)
    (e-chat-sql-e2e-prompt-async
     harness session-id
     "Call e2e_slow now. Do not answer until the tool result is available.")
    (let ((deadline (+ (float-time) 30))
          cancelled
          turn-id)
        (while (and (not (e-live-e2e--events-of-type
                          events 'provider-request-started))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (setq turn-id
              (plist-get
               (car (e-live-e2e--events-of-type
                     events 'provider-request-started))
               :turn-id))
        (should (stringp turn-id))
        (should (e-chat-service-abort-session harness session-id))
        (while (and (not cancelled) (< (float-time) deadline))
          (setq cancelled
                (seq-some
                 (lambda (event)
                   (and (eq (plist-get event :type) 'turn-cancelled)
                        (equal (plist-get event :turn-id) turn-id)))
                 events))
          (accept-process-output nil 0.05))
        (should cancelled))))

(ert-deftest e-live-e2e-test-provider-errors_surface_as_turn_failures ()
  "A live provider failure releases the session for the next request."
  (e-live-e2e--with-harness (harness session-id)
    (e-session-set-turn-options
     (e-harness-sessions harness) session-id
     '(:model "e-live-e2e-nonexistent-model"))
    (should-error
     (e-chat-sql-e2e-prompt-batch
      harness session-id
      "This request should fail because the model is invalid."))
    (should (e-live-e2e--activity-of-type
             harness session-id 'turn-failed))
    (e-session-set-turn-options
     (e-harness-sessions harness) session-id nil)
    (let* ((nonce (e-live-e2e--nonce))
           (result
            (e-chat-sql-e2e-prompt-batch
             harness session-id
             (format "Reply with exactly this recovery token: %s" nonce))))
      (should (e-live-e2e--contains-p
               (e-live-e2e--assistant-content result)
               nonce)))))

(provide 'e-live-e2e-test)

;;; e-live-e2e-test.el ends here

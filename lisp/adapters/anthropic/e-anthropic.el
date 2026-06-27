;;; e-anthropic.el --- Anthropic Messages backend adapter for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Anthropic Messages API adapter.  Provider auth, endpoint shape, request
;; mapping, and SSE parsing stay here instead of leaking into the harness.
;;
;; The harness, turn loop, tools, and session layers are provider-neutral; this
;; adapter builds an `e-backend' the same way `e-openai.el' does, but speaks the
;; native Messages wire shape (`/v1/messages') rather than the OpenAI-compatible
;; chat/completions shim.  Bearer-auth gateways are supported now; SigV4 (Amazon
;; Bedrock, Claude Platform on AWS) is reserved as a provider `:auth' variant.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'e-backend)
(require 'e-harness)
(require 'e-tools)

(define-error 'e-anthropic-auth-missing "Anthropic auth is missing")
(define-error 'e-anthropic-auth-invalid "Anthropic auth is invalid")
(define-error 'e-anthropic-provider-invalid "Anthropic provider profile is invalid")
(define-error 'e-anthropic-unsupported "Anthropic adapter feature is not supported")
(define-error 'e-anthropic-request-timeout "Anthropic request timed out")
(define-error 'e-anthropic-backend-error "Anthropic backend request failed")

(defgroup e-anthropic nil
  "Anthropic Messages backend adapter for e."
  :group 'e
  :prefix "e-anthropic-")

(defcustom e-anthropic-default-model "claude-opus-4-8"
  "Default model id for Anthropic Messages requests."
  :type 'string
  :group 'e-anthropic)

(defcustom e-anthropic-default-max-tokens 32000
  "Default `max_tokens' for Anthropic Messages requests.

The Messages API requires an explicit output-token ceiling.  Sending it from
the client keeps the budget under our control rather than at the mercy of a
gateway default, which is what silently truncated tool calls under the
OpenAI-compatible chat/completions path."
  :type 'integer
  :group 'e-anthropic)

(defcustom e-anthropic-default-effort "high"
  "Default `output_config.effort' for Anthropic Messages requests."
  :type '(choice (const "low") (const "medium") (const "high")
                 (const "xhigh") (const "max"))
  :group 'e-anthropic)

(defcustom e-anthropic-default-provider 'gateway
  "Default provider profile used by generic Anthropic harness helpers."
  :type 'symbol
  :group 'e-anthropic)

(defcustom e-anthropic-request-timeout-seconds 180
  "Seconds before Anthropic HTTP requests fail.
Set this to nil to deliberately disable provider HTTP request timeouts."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'e-anthropic)

(defcustom e-anthropic-version "2023-06-01"
  "Value sent in the `anthropic-version' request header."
  :type 'string
  :group 'e-anthropic)

(defvar e-anthropic--context-window-cache (make-hash-table :test 'equal)
  "In-memory cache of provider model context windows.
Keyed by provider symbol; each value is a model-name -> max-input-tokens hash
populated from the gateway's `/v1/model/info' (LiteLLM) catalog.  Cleared by
`e-anthropic-reset-context-window-cache'.  There is no static fallback: when
the gateway is unavailable, context-window lookups return nil.")

(defcustom e-anthropic-model-providers
  '((gateway
     :name "Anthropic via gateway"
     :base-url "https://api.anthropic.com/v1"
     :auth bearer
     :env-key "ANTHROPIC_API_KEY"
     :model-prefix ""))
  "Anthropic provider profiles keyed by provider symbol.

Each profile is plist data.  `:auth' supports `bearer' (read a token from
`:env-key', sent as `x-api-key') and `sigv4' (reserved for Amazon Bedrock and
Claude Platform on AWS; not implemented yet).  `:model-prefix' is prepended to
model ids (Amazon Bedrock uses `anthropic.')."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'e-anthropic)

(defun e-anthropic-provider-profile (&optional provider)
  "Return configured profile for PROVIDER.
When PROVIDER is nil, use `e-anthropic-default-provider'."
  (let* ((provider (or provider e-anthropic-default-provider))
         (entry (assq provider e-anthropic-model-providers)))
    (unless entry
      (signal 'e-anthropic-provider-invalid
              (list (format "Unknown provider profile: %S" provider))))
    (cdr entry)))

(defun e-anthropic-provider-name (&optional provider)
  "Return display name for PROVIDER."
  (or (plist-get (e-anthropic-provider-profile provider) :name)
      (symbol-name (or provider e-anthropic-default-provider))))

(defun e-anthropic--provider-base-url (profile)
  "Return PROFILE's base URL or signal a provider configuration error."
  (let ((base-url (plist-get profile :base-url)))
    (unless (and (stringp base-url) (not (string-empty-p base-url)))
      (signal 'e-anthropic-provider-invalid
              '("Provider profile is missing :base-url")))
    base-url))

(defun e-anthropic--provider-model (profile explicit-model)
  "Return the prefixed model id for PROFILE, preferring EXPLICIT-MODEL."
  (let ((model (or explicit-model
                   (plist-get profile :default-model)
                   e-anthropic-default-model))
        (prefix (or (plist-get profile :model-prefix) "")))
    (if (string-empty-p prefix)
        model
      (concat prefix model))))

(defun e-anthropic--env-token (env-key)
  "Return bearer token from ENV-KEY or signal an auth error."
  (unless (and (stringp env-key) (not (string-empty-p env-key)))
    (signal 'e-anthropic-auth-invalid
            '("Bearer provider is missing :env-key")))
  (let ((token (getenv env-key)))
    (unless (and (stringp token) (not (string-empty-p token)))
      (signal 'e-anthropic-auth-missing
              (list (format "Environment variable %s is missing" env-key))))
    token))

(defun e-anthropic--model-info-url (base-url)
  "Return the LiteLLM `/model/info' catalog URL for BASE-URL.
The gateway exposes per-model deployment metadata (including the real context
window) at `<base>/model/info', sibling to `/messages'."
  (let ((normalized (string-remove-suffix "/" base-url)))
    (concat normalized "/model/info")))

(defun e-anthropic--http-get (url headers)
  "GET URL with HEADERS and return the response body text, or signal.
Synchronous; bounded by `e-anthropic-request-timeout-seconds'."
  (let ((url-request-method "GET")
        (url-request-extra-headers (e-anthropic--http-header-list headers))
        (timeout e-anthropic-request-timeout-seconds))
    (let ((buffer (url-retrieve-synchronously url t t timeout)))
      (unless buffer
        (signal 'e-anthropic-backend-error
                (list (format "No response from %s" url))))
      (unwind-protect
          (e-anthropic--http-response-text buffer)
        (e-anthropic--kill-request-buffer buffer)))))

(defun e-anthropic--fetch-context-windows (provider)
  "Fetch a model-name -> max-input-tokens hash for PROVIDER from the gateway.
Queries `/model/info' and reads each entry's `model_info.max_input_tokens'.
Signals on transport, auth, or parse failure -- callers decide how to degrade."
  (let* ((profile (e-anthropic-provider-profile provider))
         (base-url (e-anthropic--provider-base-url profile))
         (headers (e-anthropic--headers profile))
         (text (e-anthropic--http-get
                (e-anthropic--model-info-url base-url) headers))
         (payload (json-parse-string text :object-type 'alist
                                     :array-type 'list :null-object nil))
         (data (alist-get 'data payload))
         (table (make-hash-table :test 'equal)))
    (dolist (entry data)
      (let ((name (alist-get 'model_name entry))
            (limit (alist-get 'max_input_tokens (alist-get 'model_info entry))))
        (when (and (stringp name) (integerp limit))
          (puthash name limit table))))
    table))

(defun e-anthropic--context-window-table (provider)
  "Return the cached context-window hash for PROVIDER, fetching once.
Returns nil when the gateway query fails (no static fallback)."
  (let ((key (or provider e-anthropic-default-provider)))
    (or (gethash key e-anthropic--context-window-cache)
        (when-let ((table (ignore-errors
                            (e-anthropic--fetch-context-windows provider))))
          (puthash key table e-anthropic--context-window-cache)
          table))))

;;;###autoload
(defun e-anthropic-context-window (model &optional provider)
  "Return PROVIDER's context window (max input tokens) for MODEL, or nil.
The value comes from the gateway's live `/model/info' catalog, cached
in-memory for the session.  Returns nil when the gateway is unavailable or
does not list MODEL -- there is no static fallback."
  (when (stringp model)
    (when-let ((table (e-anthropic--context-window-table provider)))
      (gethash model table))))

;;;###autoload
(defun e-anthropic-reset-context-window-cache ()
  "Clear the in-memory provider context-window cache.
The next `e-anthropic-context-window' call re-queries the gateway."
  (interactive)
  (clrhash e-anthropic--context-window-cache))

(defun e-anthropic-messages-url (base-url)
  "Return the Messages endpoint URL for BASE-URL."
  (let ((normalized (string-remove-suffix "/" base-url)))
    (if (string-suffix-p "/messages" normalized)
        normalized
      (concat normalized "/messages"))))

(defun e-anthropic--bearer-auth-header (profile token)
  "Return the auth header cons for PROFILE carrying TOKEN.
`:auth-header' selects `x-api-key' (default, first-party Anthropic) or
`authorization' (`Authorization: Bearer', for gateways that expect it)."
  (pcase (or (plist-get profile :auth-header) 'x-api-key)
    ('authorization (cons "Authorization" (concat "Bearer " token)))
    ('x-api-key (cons "x-api-key" token))
    (header
     (signal 'e-anthropic-provider-invalid
             (list (format "Unknown provider auth-header: %S" header))))))

(defun e-anthropic--headers (profile)
  "Return request headers for PROFILE.
Bearer providers send `x-api-key' (or `Authorization: Bearer' via
`:auth-header'); SigV4 providers are not yet supported."
  (pcase (or (plist-get profile :auth) 'bearer)
    ('bearer
     `(,(e-anthropic--bearer-auth-header
         profile (e-anthropic--env-token (plist-get profile :env-key)))
       ("anthropic-version" . ,e-anthropic-version)
       ("Accept" . "text/event-stream")
       ("Content-Type" . "application/json")))
    ('sigv4
     (signal 'e-anthropic-unsupported
             '("SigV4 auth (Amazon Bedrock / Claude Platform on AWS) is not yet implemented")))
    (auth
     (signal 'e-anthropic-provider-invalid
             (list (format "Unknown provider auth: %S" auth))))))

(defun e-anthropic--text-block (text)
  "Return a Messages text content block for TEXT."
  (list :type "text" :text (or text "")))

(defun e-anthropic--system-message-p (message)
  "Return non-nil when MESSAGE is a backend-neutral system message."
  (eq (plist-get message :role) 'system))

(defun e-anthropic--system (messages options)
  "Return the top-level system prompt string from MESSAGES and OPTIONS.
Return nil when neither an instructions option nor a system message is present."
  (let* ((instructions (plist-get options :instructions))
         (parts (delq nil
                      (append
                       (when (and (stringp instructions)
                                  (not (string-empty-p instructions)))
                         (list instructions))
                       (mapcar (lambda (message)
                                 (plist-get message :content))
                               (seq-filter #'e-anthropic--system-message-p
                                           messages))))))
    (when parts
      (string-join parts "\n\n"))))

(defun e-anthropic--tool-input (arguments)
  "Return Messages tool input from backend-neutral ARGUMENTS.
Anthropic requires an object, so nil arguments become an empty JSON object."
  (or arguments (make-hash-table :test 'equal)))

(defun e-anthropic--message (message)
  "Map backend-neutral MESSAGE to a Messages turn."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('tool-call
       (list :role "assistant"
             :content (vector
                       (list :type "tool_use"
                             :id (plist-get content :id)
                             :name (plist-get content :name)
                             :input (e-anthropic--tool-input
                                     (plist-get content :arguments))))))
      ('tool
       (list :role "user"
             :content (vector
                       (list :type "tool_result"
                             :tool_use_id (plist-get content :tool-call-id)
                             :content (e-tools-result-content-text
                                       (plist-get content :content))))))
      (_
       (list :role (symbol-name role)
             :content (vector (e-anthropic--text-block content)))))))

(defun e-anthropic--tool-definition (tool)
  "Map backend-neutral TOOL to a Messages tool definition."
  (list :name (plist-get tool :name)
        :description (plist-get tool :description)
        :input_schema (plist-get tool :parameters)))

(defun e-anthropic--tool-definitions (tools)
  "Map backend-neutral TOOLS to Messages tool definitions."
  (vconcat (mapcar #'e-anthropic--tool-definition tools)))

(defun e-anthropic--cache-control (ttl)
  "Return an ephemeral cache_control block, including TTL when non-nil."
  (if (and (stringp ttl) (not (string-empty-p ttl)))
      (list :type "ephemeral" :ttl ttl)
    (list :type "ephemeral")))

(defun e-anthropic--top-level-cache-mode-p (options)
  "Return non-nil when OPTIONS request provider-managed cache control."
  (eq (plist-get options :prompt-cache-mode) 'top-level))

(defun e-anthropic--stable-cache-segment-p (segment)
  "Return non-nil when SEGMENT belongs to the cacheable stable prefix."
  (memq (plist-get segment :kind) '(static-prefix stable-context)))

(defun e-anthropic--cache-breakpoint-segment (options)
  "Return the stable segment that receives Anthropic cache control."
  (let (candidate)
    (dolist (segment (plist-get options :segments))
      (when (and (e-anthropic--stable-cache-segment-p segment)
                 (cl-some #'e-anthropic--system-message-p
                          (plist-get segment :messages)))
        (setq candidate segment)))
    candidate))

(defun e-anthropic--system-blocks-from-segments (options cache-control)
  "Return segment-aware system blocks from OPTIONS.
When CACHE-CONTROL is non-nil, attach it to the last stable system block before
volatile current-state/history/delta segments."
  (let ((blocks nil)
        (stable-index nil)
        (instructions (plist-get options :instructions)))
    (when (and (stringp instructions) (not (string-empty-p instructions)))
      (push (e-anthropic--text-block instructions) blocks)
      (setq stable-index 0))
    (dolist (segment (plist-get options :segments))
      (let ((stable-p (e-anthropic--stable-cache-segment-p segment)))
        (dolist (message (plist-get segment :messages))
          (when (e-anthropic--system-message-p message)
            (push (e-anthropic--text-block (plist-get message :content))
                  blocks)
            (when stable-p
              (setq stable-index (1- (length blocks))))))))
    (when blocks
      (let ((ordered (nreverse blocks)))
        (when (and cache-control stable-index)
          (let ((block (nth stable-index ordered)))
            (setcar (nthcdr stable-index ordered)
                    (append block (list :cache_control cache-control)))))
        (vconcat ordered)))))

(cl-defun e-anthropic-request-body (&key messages options tools)
  "Build an Anthropic Messages request body from MESSAGES, OPTIONS, and TOOLS.

When OPTIONS enables `:prompt-cache', a `cache_control' breakpoint is attached
to the end of the stable prefix (the system prompt, or the last tool when there
is no system prompt) so Anthropic caches tools + system on the prefix match.
`:prompt-cache-ttl' selects the cache TTL (Anthropic supports `5m' and `1h')."
  (let* ((effort (or (plist-get options :effort) e-anthropic-default-effort))
         (cache-p (plist-get options :prompt-cache))
         (cache-control (and cache-p
                             (e-anthropic--cache-control
                              (plist-get options :prompt-cache-ttl))))
         (top-level-cache-p (and cache-control
                                 (e-anthropic--top-level-cache-mode-p options)))
         (system (e-anthropic--system messages options))
         (system-blocks (and cache-control
                             (not top-level-cache-p)
                             (e-anthropic--system-blocks-from-segments
                              options cache-control)))
         (turns (seq-remove #'e-anthropic--system-message-p messages))
         (tool-defs (and tools (e-anthropic--tool-definitions tools)))
         (body (list :model (or (plist-get options :model)
                                e-anthropic-default-model)
                     :max_tokens (or (plist-get options :max-tokens)
                                     e-anthropic-default-max-tokens)
                     :stream t)))
    ;; The cache breakpoint goes on the last block of the stable prefix.  When a
    ;; system prompt is present it caches tools + system together; otherwise it
    ;; falls back to the last tool definition.
    (when system
      (setq body
            (append body
                    (list :system
                          (if system-blocks
                              system-blocks
                            (if (and cache-control (not top-level-cache-p))
                              (vector (append (e-anthropic--text-block system)
                                              (list :cache_control
                                                    cache-control)))
                              system))))))
    (when top-level-cache-p
      (setq body (append body (list :cache_control cache-control))))
    (when (and cache-control (not top-level-cache-p)
               (not system) (> (length tool-defs) 0))
      (let ((last (1- (length tool-defs))))
        (aset tool-defs last
              (append (aref tool-defs last)
                      (list :cache_control cache-control)))))
    (setq body (append body
                        (list :messages (vconcat
                                         (mapcar #'e-anthropic--message turns))
                              :thinking (list :type "adaptive")
                              :output_config (list :effort effort))))
    (when-let ((container (plist-get options :anthropic-container-id)))
      (when (and (stringp container) (not (string-empty-p container)))
        (setq body (append body (list :container container)))))
    (when-let ((context-management
                (plist-get options :anthropic-context-management)))
      (setq body
            (append body (list :context_management context-management))))
    (when tool-defs
      (setq body (append body (list :tools tool-defs))))
    body))

(defun e-anthropic--request-metadata (options body)
  "Return sanitized Anthropic request metadata for OPTIONS and BODY."
  (let ((metadata (list :provider 'anthropic
                        :model (plist-get options :model))))
    (when (plist-get options :prompt-cache)
      (let ((mode (cond
                   ((e-anthropic--top-level-cache-mode-p options)
                    'top-level)
                   ((plist-member body :system)
                    'explicit)
                   ((plist-member body :tools)
                    'explicit)
                   (t 'off)))
            (breakpoint (cond
                         ((plist-member body :cache_control)
                          'provider-managed)
                         ((and (plist-member body :system)
                               (vectorp (plist-get body :system)))
                          'system-stable-prefix)
                         ((plist-member body :tools)
                          'tools)
                         (t 'none))))
        (setq metadata
              (append metadata
                      (list :anthropic-cache-mode mode
                            :anthropic-cache-breakpoint breakpoint
                            :full-history t)))
        (when-let ((breakpoint-segment
                    (and (eq breakpoint 'system-stable-prefix)
                         (e-anthropic--cache-breakpoint-segment options))))
          (setq metadata
                (append metadata
                        (list :anthropic-breakpoint-segment-id
                              (prin1-to-string
                               (plist-get breakpoint-segment :id))
                              :anthropic-breakpoint-fingerprint
                              (plist-get breakpoint-segment :fingerprint)))))
        (when-let ((ttl (plist-get options :prompt-cache-ttl)))
          (setq metadata
                (append metadata
                        (list :anthropic-cache-ttl ttl))))))
    (when-let ((container (plist-get options :anthropic-container-id)))
      (when (and (stringp container) (not (string-empty-p container)))
        (setq metadata
              (append metadata
                      (list :anthropic-container-id container)))))
    (when (plist-get body :context_management)
      (setq metadata
            (append metadata
                    (list :anthropic-context-management 'requested))))
    (when (plist-get body :context_management)
      (setq metadata
            (append metadata
                    (list :anthropic-beta-headers
                          (plist-get options :anthropic-beta-headers)))))
    (when-let ((segments (plist-get options :segments)))
      (setq metadata
            (append metadata
                    (list :segment-fingerprints
                          (mapcar (lambda (segment)
                                    (plist-get segment :fingerprint))
                                  segments)
                          :segment-fingerprint-count (length segments)))))
    (setq metadata
          (append metadata
                  (list :diagnostics
                        (list :model (plist-get body :model)
                              :effort (plist-get (plist-get body
                                                             :output_config)
                                                 :effort)
                              :max-tokens (plist-get body :max_tokens)
                              :prompt-cache
                              (and (plist-get options :prompt-cache) t)
                              :anthropic-cache-mode
                              (plist-get metadata :anthropic-cache-mode)
                              :anthropic-cache-breakpoint
                              (plist-get metadata :anthropic-cache-breakpoint)
                              :anthropic-cache-ttl
                              (plist-get metadata :anthropic-cache-ttl)
                              :anthropic-container-id-present
                              (not (null (plist-get metadata
                                                     :anthropic-container-id)))
                              :input-message-count
                              (length (plist-get body :messages))
                              :tool-count
                              (length (plist-get body :tools))))))
    metadata))

(defun e-anthropic--beta-headers (value)
  "Return normalized Anthropic beta header names from VALUE."
  (cond
   ((and (stringp value) (not (string-empty-p value)))
    (list value))
   ((listp value)
    (cl-remove-if-not
     (lambda (item)
       (and (stringp item) (not (string-empty-p item))))
     value))
   (t nil)))

(defun e-anthropic--headers-with-betas (headers beta-headers)
  "Return HEADERS with BETA-HEADERS appended as `anthropic-beta'."
  (if beta-headers
      (append headers
              (list (cons "anthropic-beta"
                          (string-join beta-headers ","))))
    headers))

(defun e-anthropic--parse-json (value)
  "Parse VALUE as JSON into plist data."
  (json-parse-string value
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun e-anthropic--number-or-nil (value)
  "Return VALUE when it is numeric, otherwise nil."
  (when (numberp value) value))

(defun e-anthropic--stop-reason-symbol (reason)
  "Return provider-neutral done reason for Messages stop REASON."
  (cond
   ((or (null reason) (equal reason "end_turn") (equal reason "stop_sequence"))
    'stop)
   ((equal reason "tool_use") 'tool-use)
   ((equal reason "max_tokens") 'max-tokens)
   ((equal reason "refusal") 'refusal)
   ((equal reason "pause_turn") 'pause-turn)
   (t (intern (replace-regexp-in-string "_" "-" (format "%s" reason))))))

(defun e-anthropic--usage-item
    (input-tokens output-tokens cached-tokens created-tokens)
  "Return a provider-neutral token usage item.
INPUT-TOKENS, OUTPUT-TOKENS, CACHED-TOKENS (cache reads), and CREATED-TOKENS
\(cache writes) come from Messages usage fields.  On the Messages API
`input_tokens' excludes both cache reads and cache writes, so a downstream total
must sum all three."
  (list :type 'token-usage
        :usage (list :input-tokens (e-anthropic--number-or-nil input-tokens)
                     :cached-input-tokens (e-anthropic--number-or-nil
                                           cached-tokens)
                     :cache-creation-input-tokens (e-anthropic--number-or-nil
                                                   created-tokens)
                     :output-tokens (e-anthropic--number-or-nil output-tokens)
                     :reasoning-output-tokens nil
                     :total-tokens nil)))

(defun e-anthropic--sse-data (stream-text)
  "Return parsed JSON events from Messages SSE STREAM-TEXT, in order."
  (let ((events nil))
    (dolist (chunk (split-string stream-text "\n\n" t))
      (let ((data-lines nil))
        (dolist (line (split-string chunk "\n"))
          (when (string-prefix-p "data:" line)
            (push (string-trim (substring line 5)) data-lines)))
        (when data-lines
          (let ((data (string-join (nreverse data-lines) "\n")))
            (unless (or (string-empty-p data) (equal data "[DONE]"))
              (push (e-anthropic--parse-json data) events))))))
    (nreverse events)))

(defun e-anthropic--parse-tool-input (partial-json)
  "Parse accumulated PARTIAL-JSON from a tool_use block into a plist."
  (if (and (stringp partial-json) (not (string-empty-p partial-json)))
      (e-anthropic--parse-json partial-json)
    nil))

(defun e-anthropic--text-preview (text &optional limit)
  "Return a compact single-line preview of TEXT.
LIMIT defaults to 240 characters."
  (let* ((limit (or limit 240))
         (preview (string-trim
                   (replace-regexp-in-string
                    "[[:space:]\n\r\t]+" " " (or text "")))))
    (if (> (length preview) limit)
        (concat (substring preview 0 limit) "...")
      preview)))

(defun e-anthropic--html-text-preview (html &optional limit)
  "Return a compact text preview for HTML.
LIMIT defaults to 240 characters."
  (e-anthropic--text-preview
   (replace-regexp-in-string "<[^>]+>" " " (or html ""))
   limit))

(defun e-anthropic--non-stream-json-error-item (stream-text)
  "Return a backend error item when STREAM-TEXT is a non-stream JSON body.
The Anthropic error object carries the failure kind in `error.type'
\(e.g. `rate_limit_error', `overloaded_error'); that type is folded into both
the surfaced content and the payload so the harness retry classifier can act on
it even when the human-readable message does not name the kind."
  (condition-case nil
      (let* ((payload (e-anthropic--parse-json stream-text))
             (error (plist-get payload :error))
             (error-type (and (listp error) (plist-get error :type)))
             (message (cond
                       ((listp error) (plist-get error :message))
                       ((stringp error) error))))
        (when message
          (list :type 'backend-error
                ;; Prefix the kind so an `overloaded_error' message of just
                ;; "Overloaded" is unambiguous, and so the retry classifier
                ;; matches on the type substring.
                :content (if error-type
                             (format "%s: %s" error-type message)
                           message)
                :payload (if error-type
                             (plist-put (copy-sequence payload)
                                        :error-type error-type)
                           payload))))
    (error nil)))

(defun e-anthropic--non-stream-error-item (stream-text)
  "Return a backend error item for a non-empty, non-SSE STREAM-TEXT.
JSON error bodies surface their `:error' message verbatim.  HTML or other
non-JSON bodies (gateway/transport failures returning an error page instead of
a Messages stream) are compacted into a single-line preview so the failure is
visible rather than masked as empty assistant output."
  (let ((trimmed (string-trim-left (or stream-text ""))))
    (cond
     ((string-empty-p (string-trim trimmed)) nil)
     ((string-prefix-p "{" trimmed)
      (or (e-anthropic--non-stream-json-error-item stream-text)
          (let ((preview (e-anthropic--text-preview stream-text)))
            (list :type 'backend-error
                  :content (format "Provider returned non-stream JSON instead of a Messages stream: %s"
                                   preview)
                  :payload (list :response-kind 'json :preview preview)))))
     ((string-prefix-p "<" trimmed)
      (let ((preview (e-anthropic--html-text-preview stream-text)))
        (when (not (string-empty-p preview))
          (list :type 'backend-error
                :content (format "Provider returned HTML instead of a Messages stream: %s"
                                 preview)
                :payload (list :response-kind 'html :preview preview)))))
     (t
      (let ((preview (e-anthropic--text-preview stream-text)))
        (when (not (string-empty-p preview))
          (list :type 'backend-error
                :content (format "Provider returned non-stream text instead of a Messages stream: %s"
                                 preview)
                :payload (list :response-kind 'text :preview preview))))))))

(defun e-anthropic-parse-stream (stream-text)
  "Parse Anthropic Messages STREAM-TEXT into backend-neutral items.

Text deltas across every content block are concatenated into a single
`assistant-message'; the turn loop keeps only the last assistant message, so
emitting one merged message (rather than one per block) is what it expects.
Tool calls are emitted per block index as their blocks complete.

When STREAM-TEXT is a non-stream JSON error body (no SSE events parsed), it is
surfaced as a single `backend-error' item — the gateway can return such a body
instead of a stream (the failure mode this adapter was built to make visible)."
  (let ((items nil)
        (text-parts nil)
        (blocks nil)
        (input-tokens nil)
        (cached-tokens nil)
        (created-tokens nil)
        (output-tokens nil)
        (stop-reason nil)
        (usage-seen nil)
        (terminal-seen nil)
        (event-count 0))
    (cl-labels
        ((emit-tool-call
          (block)
          (when (equal (plist-get block :type) "tool_use")
            (push (list :type 'tool-call
                        :id (plist-get block :id)
                        :name (plist-get block :name)
                        :arguments (e-anthropic--parse-tool-input
                                    (plist-get block :partial-json)))
                  items)))
         (absorb-usage
          (usage)
          ;; Usage is cumulative and re-stated on message_delta; take the latest
          ;; non-nil value for each field (last writer wins).
          (when usage
            (setq usage-seen t)
            (when (plist-member usage :input_tokens)
              (setq input-tokens (plist-get usage :input_tokens)))
            (when (plist-member usage :cache_read_input_tokens)
              (setq cached-tokens (plist-get usage :cache_read_input_tokens)))
            (when (plist-member usage :cache_creation_input_tokens)
              (setq created-tokens
                    (plist-get usage :cache_creation_input_tokens)))
            (when (plist-member usage :output_tokens)
              (setq output-tokens (plist-get usage :output_tokens))))))
      (dolist (event (e-anthropic--sse-data stream-text))
      (setq event-count (1+ event-count))
      (pcase (plist-get event :type)
        ("message_start"
         (absorb-usage (plist-get (plist-get event :message) :usage)))
        ("content_block_start"
         (let* ((index (plist-get event :index))
                (block (plist-get event :content_block)))
           (when (equal (plist-get block :type) "tool_use")
             ;; :partial-json is seeded here so the input_json_delta `plist-put'
             ;; mutates an existing key in place (a `plist-put' on a plist
             ;; lacking the key would not persist through the alist cdr).
             (push (cons index (list :type "tool_use"
                                     :id (plist-get block :id)
                                     :name (plist-get block :name)
                                     :partial-json ""))
                   blocks))))
        ("content_block_delta"
         (let* ((delta (plist-get event :delta))
                (delta-type (plist-get delta :type)))
           (pcase delta-type
             ("text_delta"
              (let ((text (plist-get delta :text)))
                (when text
                  (push text text-parts)
                  (push (list :type 'assistant-delta :content text) items))))
             ("input_json_delta"
              (let* ((index (plist-get event :index))
                     (entry (assoc index blocks)))
                (when entry
                  (let ((acc (cdr entry)))
                    (plist-put acc :partial-json
                               (concat (or (plist-get acc :partial-json) "")
                                       (or (plist-get delta :partial_json)
                                           ""))))))))))
        ("content_block_stop"
         (let ((entry (assoc (plist-get event :index) blocks)))
           (when entry
             (emit-tool-call (cdr entry)))))
        ("message_delta"
         (let ((delta (plist-get event :delta)))
           (when (plist-member delta :stop_reason)
             (setq stop-reason (plist-get delta :stop_reason)))
           (absorb-usage (plist-get event :usage))))
        ("message_stop"
         (setq terminal-seen t))
        ("error"
         (let ((err (plist-get event :error)))
           (push (list :type 'backend-error
                       :content (or (plist-get err :message) (format "%S" event))
                       :payload event)
                 items)))))
      (when text-parts
        (push (list :type 'assistant-message
                    :content (apply #'concat (nreverse text-parts)))
              items))
      (when usage-seen
        (push (e-anthropic--usage-item input-tokens output-tokens
                                       cached-tokens created-tokens)
              items))
      (when (or terminal-seen stop-reason)
        (push (list :type 'done
                    :reason (e-anthropic--stop-reason-symbol stop-reason))
              items))
      (when (zerop event-count)
        (when-let ((error-item (e-anthropic--non-stream-error-item stream-text)))
          (push error-item items)))
      (nreverse items))))

(defun e-anthropic--http-header-bytes (value)
  "Return VALUE as an ASCII byte string for `url-request-extra-headers'."
  (encode-coding-string (format "%s" value) 'us-ascii))

(defun e-anthropic--http-header-list (headers)
  "Return HEADERS with names and values normalized to byte strings."
  (mapcar (lambda (header)
            (cons (e-anthropic--http-header-bytes (car header))
                  (e-anthropic--http-header-bytes (cdr header))))
          headers))

(defun e-anthropic--http-response-text (buffer)
  "Return response body text from url.el BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\n\n" nil 'move)
    (buffer-substring-no-properties (point) (point-max))))

(defun e-anthropic--url-metadata (url)
  "Return sanitized diagnostic metadata for URL."
  (let* ((parsed (url-generic-parse-url url))
         (path (or (url-filename parsed) "/")))
    (when (string-match "\\`\\([^?#]*\\)" path)
      (setq path (match-string 1 path)))
    (when (string-empty-p path)
      (setq path "/"))
    (list :url-host (url-host parsed)
          :url-path path)))

(defun e-anthropic--kill-request-buffer (buffer)
  "Cancel any live request process attached to BUFFER and kill BUFFER."
  (when (buffer-live-p buffer)
    (when-let ((process (get-buffer-process buffer)))
      (when (process-live-p process)
        (delete-process process)))
    (kill-buffer buffer)))

(cl-defun e-anthropic--http-request-start
    (&key url headers body on-complete on-error)
  "POST BODY to URL with HEADERS asynchronously.
ON-COMPLETE receives the response body text.  ON-ERROR receives an Emacs
condition list.  Return a cancellable `e-backend-request' handle."
  (let ((url-request-method "POST")
        (url-request-extra-headers (e-anthropic--http-header-list headers))
        (url-request-data (encode-coding-string body 'utf-8))
        (timeout e-anthropic-request-timeout-seconds)
        request-buffer
        timeout-timer
        settled)
    (cl-labels
        ((cancel-timeout ()
           (when (timerp timeout-timer)
             (cancel-timer timeout-timer))
           (setq timeout-timer nil))
         (cleanup (buffer)
           (cancel-timeout)
           (e-anthropic--kill-request-buffer buffer))
         (settle-timeout ()
           (unless settled
             (setq settled t)
             (cleanup request-buffer)
             (when on-error
               (funcall
                on-error
                (list 'e-anthropic-request-timeout
                      (format "Anthropic request timed out after %s seconds"
                              timeout))))))
         (handle-callback (status)
           (unless settled
             (setq settled t)
             (let ((buffer (current-buffer)))
               (unwind-protect
                   (condition-case err
                       (let ((url-error (plist-get status :error)))
                         (if url-error
                             (let ((response-text
                                    (e-anthropic--http-response-text buffer)))
                               (if (not (string-empty-p
                                         (string-trim response-text)))
                                   (when on-complete
                                     (funcall on-complete response-text))
                                 (when on-error
                                   (funcall
                                    on-error
                                    (list 'error
                                          (format "Anthropic request failed: %S"
                                                  url-error))))))
                           (when on-complete
                             (funcall
                              on-complete
                              (e-anthropic--http-response-text buffer)))))
                     (error
                      (when on-error
                        (funcall on-error err))))
                 (cleanup buffer))))))
      (setq request-buffer
            (url-retrieve url
                          (lambda (status) (handle-callback status))
                          nil 'silent nil))
      (when (and timeout (not settled))
        (setq timeout-timer
              (run-at-time timeout nil #'settle-timeout))))
    (e-backend-request-create
     :cancel (lambda ()
               (unless settled (setq settled t))
               (when (timerp timeout-timer)
                 (cancel-timer timeout-timer))
               (setq timeout-timer nil)
               (e-anthropic--kill-request-buffer request-buffer)
               t)
     :metadata (append
                (list :transport 'url-retrieve
                      :url url
                      :timeout-seconds timeout
                      :cancellable t)
                (e-anthropic--url-metadata url)))))

(cl-defun e-anthropic--http-request (&key url headers body)
  "POST BODY to URL with HEADERS and return response text synchronously."
  (let ((response nil) (failure nil) (done nil))
    (e-anthropic--http-request-start
     :url url :headers headers :body body
     :on-complete (lambda (value) (setq response value) (setq done t))
     :on-error (lambda (err) (setq failure err) (setq done t)))
    (while (not done)
      (accept-process-output nil 0.01))
    (when failure
      (signal (car failure) (cdr failure)))
    response))

(defun e-anthropic--emit-response-items (response on-item)
  "Parse RESPONSE and emit backend-neutral items through ON-ITEM."
  (e-anthropic--emit-response-items-with-context response nil on-item))

(defun e-anthropic--anchor-candidate-item (context)
  "Return provider anchor candidate item for successful CONTEXT, when useful."
  (when-let ((metadata (plist-get context :metadata)))
    (when (or (plist-get metadata :anthropic-cache-mode)
              (plist-get metadata :anthropic-container-id)
              (plist-get metadata :anthropic-context-management)
              (plist-get metadata :anthropic-beta-headers))
      (list :type 'provider-anchor-candidate
            :provider-id 'anthropic
            :metadata metadata))))

(defun e-anthropic--emit-response-items-with-context (response context on-item)
  "Parse RESPONSE and emit backend-neutral items through ON-ITEM.
When CONTEXT has provider cache metadata, emit a provider anchor candidate
before the terminal success item so the harness can persist the cache state."
  (let ((candidate (e-anthropic--anchor-candidate-item context))
        emitted-candidate)
    (dolist (item (e-anthropic-parse-stream response))
      (when (and candidate
                 (not emitted-candidate)
                 (eq (plist-get item :type) 'done))
        (funcall on-item candidate)
        (setq emitted-candidate t))
      (funcall on-item item))))

(cl-defun e-anthropic--request-context
    (&key provider base-url model messages options)
  "Return adapter-local request context for PROVIDER request data.
BASE-URL, MODEL, MESSAGES, and OPTIONS contribute to the encoded Messages
request and backend-neutral context."
  (let* ((profile (e-anthropic-provider-profile provider))
         (effective-options (copy-sequence options))
         (resolved-model (e-anthropic--provider-model
                          profile
                          (or (plist-get effective-options :model) model)))
         (context-management
          (or (plist-get effective-options :anthropic-context-management)
              (plist-get profile :context-management)))
         (configured-beta-headers
          (e-anthropic--beta-headers
           (or (plist-get effective-options :anthropic-beta-headers)
               (plist-get profile :beta-headers))))
         (beta-headers (and context-management configured-beta-headers))
         (headers (e-anthropic--headers-with-betas
                   (e-anthropic--headers profile)
                   beta-headers)))
    (setq effective-options
          (plist-put effective-options :model resolved-model))
    (when (and context-management beta-headers)
      (setq effective-options
            (plist-put effective-options
                       :anthropic-context-management
                       context-management)))
    (when beta-headers
      (setq effective-options
            (plist-put effective-options
                       :anthropic-beta-headers
                       beta-headers)))
    (let* ((body-data (e-anthropic-request-body
                       :messages messages
                       :options effective-options
                       :tools (plist-get effective-options :tools)))
           (metadata (e-anthropic--request-metadata
                      effective-options body-data)))
      (list :provider provider
            :url (e-anthropic-messages-url
                  (or base-url (e-anthropic--provider-base-url profile)))
            :headers headers
            :metadata metadata
            :body (json-encode body-data)))))

(cl-defun e-anthropic-backend-create
    (&key provider base-url request-function name model)
  "Create an Anthropic Messages backend named NAME.
PROVIDER selects a profile from `e-anthropic-model-providers'.  BASE-URL
overrides the profile base URL.  REQUEST-FUNCTION is injectable for tests.
MODEL is the backend-local default when turn options omit `:model'."
  (let ((provider (or provider e-anthropic-default-provider)))
    (e-backend-create
     :name (or name (e-anthropic-provider-name provider))
     :stream
     (cl-function
      (lambda (&key messages options on-item)
        (let* ((context (e-anthropic--request-context
                         :provider provider
                         :base-url base-url
                         :model model
                         :messages messages
                         :options options))
               (requester (or request-function #'e-anthropic--http-request))
               (response nil))
          (e-backend-note-request-started
           (e-backend-request-create
            :metadata
            (append
             (list :provider provider
                   :url (plist-get context :url)
                   :cancellable nil
                   :transport 'sync-wrapper)
             (plist-get context :metadata)
             (e-anthropic--url-metadata (plist-get context :url)))))
          (setq response
                (funcall requester
                         :url (plist-get context :url)
                         :headers (plist-get context :headers)
                         :body (plist-get context :body)))
          (e-anthropic--emit-response-items-with-context
           response context on-item))))
     :start
     (cl-function
      (lambda (&key messages options on-item on-done on-error on-request-start)
        (let ((context (e-anthropic--request-context
                        :provider provider
                        :base-url base-url
                        :model model
                        :messages messages
                        :options options)))
          (if request-function
              (let ((cancelled nil) (timer nil) request)
                (setq request
                      (e-backend-request-create
                       :cancel (lambda ()
                                 (setq cancelled t)
                                 (when (timerp timer) (cancel-timer timer))
                                 t)
                       :metadata
                       (append
                        (list :provider provider
                              :url (plist-get context :url)
                              :cancellable 'queued-only
                              :transport 'injected-request-function)
                        (plist-get context :metadata)
                        (e-anthropic--url-metadata (plist-get context :url)))))
                (when on-request-start
                  (funcall on-request-start request))
                (setq timer
                      (run-at-time
                       0 nil
                       (lambda ()
                         (unless cancelled
                           (condition-case err
                               (let ((response
                                      (funcall
                                       request-function
                                       :url (plist-get context :url)
                                       :headers (plist-get context :headers)
                                       :body (plist-get context :body))))
                                 (e-anthropic--emit-response-items-with-context
                                  response context on-item)
                                 (when on-done
                                   (funcall on-done '(:status done))))
                             (error
                              (when on-error (funcall on-error err))))))))
                request)
            (let ((request
                   (e-anthropic--http-request-start
                    :url (plist-get context :url)
                    :headers (plist-get context :headers)
                    :body (plist-get context :body)
                    :on-complete
                    (lambda (response)
                      (condition-case err
                          (progn
                            (e-anthropic--emit-response-items-with-context
                             response context on-item)
                            (when on-done (funcall on-done '(:status done))))
                        (error
                         (when on-error (funcall on-error err)))))
                    :on-error on-error)))
              (setf (e-backend-request-metadata request)
                    (append (list :provider provider)
                            (plist-get context :metadata)
                            (e-backend-request-metadata request)))
              (when on-request-start
                (funcall on-request-start request))
              request))))))))

(cl-defun e-anthropic-create-harness
    (&key provider base-url request-function model sessions)
  "Create a harness configured for an Anthropic Messages provider.
PROVIDER selects `e-anthropic-default-provider' when nil.  BASE-URL and
REQUEST-FUNCTION configure the backend adapter.  MODEL is written into
backend-neutral turn options.  SESSIONS supplies an existing session store."
  (let* ((provider (or provider e-anthropic-default-provider))
         (profile (e-anthropic-provider-profile provider))
         (model (or model (plist-get profile :default-model)
                    e-anthropic-default-model)))
    (e-harness-create
     :backend (e-anthropic-backend-create
               :provider provider
               :base-url base-url
               :request-function request-function
               :model model)
     :default-options (list :model model
                            :max-tokens e-anthropic-default-max-tokens
                            :effort e-anthropic-default-effort)
     :sessions sessions)))

(provide 'e-anthropic)

;;; e-anthropic.el ends here

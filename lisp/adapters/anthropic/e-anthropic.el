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
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'e-backend)
(require 'e-harness)
(require 'e-request)
(require 'e-json)
(require 'e-context-lifetime)
(require 'e-tools)
(require 'e-work)

(define-error 'e-anthropic-auth-missing "Anthropic auth is missing")
(define-error 'e-anthropic-auth-invalid "Anthropic auth is invalid")
(define-error 'e-anthropic-provider-invalid "Anthropic provider profile is invalid")
(define-error 'e-anthropic-unsupported "Anthropic adapter feature is not supported")
(define-error 'e-anthropic-request-timeout "Anthropic request timed out")
(define-error 'e-anthropic-backend-error "Anthropic backend request failed")
(define-error 'e-anthropic-response-invalid
  "Anthropic Messages response is invalid" 'e-anthropic-backend-error)

(defconst e-anthropic--retryable-error-patterns
  '("rate limit" "rate_limit_error" "too many requests"
    "overloaded" "overloaded_error" "api_error"
    "internal_server_error" "server_error" "service unavailable"
    "bad gateway" "gateway time" "request timed out" "idle timed out"
    "connection termination" "connection reset" "reset by peer"
    "connect error" "before headers" "disconnect" "broken pipe"
    "premature")
  "Anthropic and gateway error fragments that identify transient failures.")

(defun e-anthropic--retryable-status-p (status)
  "Return non-nil when Anthropic HTTP STATUS permits a retry."
  (and (numberp status)
       (or (memq status '(408 409 429)) (>= status 500))))

(defun e-anthropic--retry-after-from-text (message &optional now)
  "Return provider retry delay parsed from MESSAGE, or nil.
NOW defaults to the current time and is injectable for tests."
  (let ((text (downcase (or message "")))
        (now (or now (float-time))))
    (cond
     ((string-match
       "\\(?:try again in\\|retry after\\|retry in\\)[^0-9]*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)[[:space:]]*\\(m\\|min\\|s\\|sec\\|seconds?\\|minutes?\\)?"
       text)
      (let ((number (string-to-number (match-string 1 text)))
            (unit (match-string 2 text)))
        (if (and unit (string-prefix-p "m" unit))
            (* number 60.0)
          number)))
     ((string-match
       "resets? at[:[:space:]]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[T[:space:]][0-9]\\{2\\}:[0-9]\\{2\\}\\(?::[0-9]\\{2\\}\\)?\\)"
       text)
      (let* ((stamp (replace-regexp-in-string
                     "T" " " (match-string 1 text)))
             (parsed (ignore-errors
                       (float-time
                        (encode-time
                         (parse-time-string (concat stamp " +0000")))))))
        (and parsed (- parsed now)))))))

(defun e-anthropic--error-type (details)
  "Return an Anthropic error type from DETAILS, when present."
  (let ((error (and (listp details) (plist-get details :error))))
    (or (and (listp details) (plist-get details :error-type))
        (and (listp error) (or (plist-get error :type)
                               (plist-get error :code))))))

(defun e-anthropic--normalize-error-details (message details condition)
  "Return Anthropic-owned normalized retry metadata for an error.
MESSAGE, DETAILS, and CONDITION are the backend error surfaces received by the
provider-neutral backend contract."
  (let* ((normalized (if (listp details) (copy-tree details) nil))
         (text (downcase (or message "")))
         (status (or (plist-get normalized :status)
                     (plist-get normalized :status-code)))
         (error-type (downcase
                      (format "%s" (or (e-anthropic--error-type normalized)
                                          ""))))
         (timeout-p (eq (car-safe condition) 'e-anthropic-request-timeout))
         (pattern (seq-find (lambda (candidate)
                              (string-match-p (regexp-quote candidate) text))
                            e-anthropic--retryable-error-patterns))
         (reason
          (cond
           ((or (equal status 429)
                (string-match-p "rate[_ -]?limit" error-type)
                (member pattern '("rate limit" "rate_limit_error"
                                  "too many requests")))
            'rate-limit)
           ((or timeout-p (equal status 408)
                (member pattern '("request timed out" "idle timed out")))
            'timeout)
           ((equal pattern "premature") 'premature-stream)
           ((equal status 409) 'conflict)
           ((or (e-anthropic--retryable-status-p status)
                (string-match-p
                 "\\(?:overloaded\\|api_error\\|server_error\\)" error-type)
                (member pattern '("overloaded" "overloaded_error" "api_error"
                                  "internal_server_error" "server_error"
                                  "service unavailable" "bad gateway"
                                  "gateway time")))
            'provider-unavailable)
           (pattern 'transport)))
         (retry-after
          (or (plist-get normalized :retry-after-seconds)
              (plist-get normalized :retry-after)
              (e-anthropic--retry-after-from-text message))))
    (setq normalized (plist-put normalized :retryable (and reason t)))
    (when reason
      (setq normalized (plist-put normalized :retry-reason reason)))
    (when (numberp retry-after)
      (setq normalized
            (plist-put normalized :retry-after-seconds retry-after)))
    normalized))

(defun e-anthropic--normalize-backend-error-item (item)
  "Return backend error ITEM with Anthropic-normalized payload details."
  (if (not (eq (plist-get item :type) 'backend-error))
      item
    (let ((normalized (copy-tree item)))
      (plist-put
       normalized :payload
       (e-anthropic--normalize-error-details
        (plist-get normalized :content)
        (plist-get normalized :payload)
        nil))
      normalized)))

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
  "Seconds of inactivity before an Anthropic HTTP request fails.
This is an idle deadline, not a total wall-clock budget: the timer is re-armed
whenever the response buffer receives data, so a long but healthy streamed
generation is never killed mid-flight, while a genuinely stalled connection
still fails after this many seconds without any bytes.
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
Keyed by provider symbol; each value maps model names to a limit or nil.
Entries come from the gateway's `/models' catalog.  Cleared by
`e-anthropic-reset-context-window-cache'.  There is no static fallback: when
the gateway is unavailable, context-window lookups return nil.")

(defvar e-anthropic--context-window-refresh-requests (make-hash-table :test 'equal)
  "In-flight async context-window catalog refresh requests keyed by provider.")

(defvar e-anthropic-context-window-cache-updated-hook nil
  "Hook run after a provider context-window catalog is cached.
Each function receives the provider key whose cache changed.")

(defvar e-anthropic--context-window-failure-cache (make-hash-table :test 'equal)
  "In-memory record of failed context-window fetches.
Keyed by provider symbol; each value is the `float-time' of the last failed
async gateway query.  A recent failure suppresses another refresh until
`e-anthropic-context-window-retry-cooldown' elapses.  Cleared by
`e-anthropic-reset-context-window-cache'.")

(defcustom e-anthropic-context-window-retry-cooldown 300
  "Seconds to wait before re-querying the gateway after a failed catalog fetch.
A failed asynchronous `/models' query is negative-cached for this many seconds
so repeated mode-line renders do not start a request each time.  Context-window
lookups themselves are always cache-only.  Set to 0 to retry every refresh."
  :type 'natnum
  :group 'e-anthropic)

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

(defun e-anthropic--reject-sync-in-hot-path (operation)
  "Reject synchronous Anthropic OPERATION from marked interactive hot paths."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error operation)))

(defun e-anthropic--models-url (base-url)
  "Return the provider model-catalog URL for BASE-URL.
The Messages endpoint and model catalog are siblings below the same base URL."
  (let ((normalized (string-remove-suffix "/" base-url)))
    (concat normalized "/models")))

(cl-defun e-anthropic--http-get-start
    (&key url headers on-complete on-error)
  "GET URL with HEADERS asynchronously.
ON-COMPLETE receives the response body text.  ON-ERROR receives an Emacs
condition list.  Return a cancellable `e-backend-request' handle."
  (e-anthropic--http-request-start
   :method "GET"
   :url url
   :headers headers
   :on-complete on-complete
   :on-error on-error))

(defun e-anthropic--context-window-table-from-json (text)
  "Return a model-name -> max-input-tokens-or-nil hash parsed from TEXT."
  (let* ((payload (e-json-parse-string text))
         (data (plist-get payload :data))
         (table (make-hash-table :test 'equal)))
    (unless (vectorp data)
      (signal 'e-anthropic-backend-error
              (list "Model catalog data is not a JSON array" data)))
    ;; The catalog is an application-owned hash table.  This is the explicit
    ;; provider-domain projection from canonical array/object values.
    (dolist (entry (append data nil))
      (let ((name (plist-get entry :id))
            (limit (plist-get entry :max_input_tokens)))
        (when (and (stringp name) (not (string-empty-p name)))
          (puthash name (and (integerp limit) (>= limit 0) limit) table))))
    (when (zerop (hash-table-count table))
      (signal 'e-anthropic-backend-error
              '("Model catalog contains no valid model IDs")))
    table))

(defun e-anthropic--context-window-failure-fresh-p (key)
  "Return non-nil when KEY has a failed fetch still within the retry cooldown."
  (and (> e-anthropic-context-window-retry-cooldown 0)
       (when-let* ((failed-at (gethash key
                                      e-anthropic--context-window-failure-cache)))
         (< (- (float-time) failed-at)
            e-anthropic-context-window-retry-cooldown))))

;;;###autoload
(cl-defun e-anthropic-refresh-context-window-cache
    (&key provider on-done on-error)
  "Asynchronously refresh PROVIDER's model context-window cache.
Return an in-flight request handle.  ON-DONE receives the parsed table after
the cache is populated.  ON-ERROR receives an Emacs condition list."
  (let* ((key (or provider e-anthropic-default-provider))
         (existing (gethash key e-anthropic--context-window-refresh-requests)))
    (or existing
        (unless (e-anthropic--context-window-failure-fresh-p key)
          (let* ((profile (e-anthropic-provider-profile provider))
                 (base-url (e-anthropic--provider-base-url profile))
                 (headers (e-anthropic--headers profile))
                 request
                 completed)
            (setq request
                  (e-anthropic--http-get-start
                   :url (e-anthropic--models-url base-url)
                   :headers headers
                   :on-complete
                   (lambda (text)
                     (setq completed t)
                     (remhash key e-anthropic--context-window-refresh-requests)
                     (condition-case err
                         (let ((table
                                (e-anthropic--context-window-table-from-json
                                 text)))
                           (remhash key e-anthropic--context-window-failure-cache)
                           (puthash key table e-anthropic--context-window-cache)
                           (run-hook-with-args
                            'e-anthropic-context-window-cache-updated-hook key)
                           (when on-done
                             (funcall on-done table)))
                       (error
                        (puthash key (float-time)
                                 e-anthropic--context-window-failure-cache)
                        (when on-error
                          (funcall on-error err)))))
                   :on-error
                   (lambda (err)
                     (setq completed t)
                     (remhash key e-anthropic--context-window-refresh-requests)
                     (puthash key (float-time)
                              e-anthropic--context-window-failure-cache)
                     (when on-error
                       (funcall on-error err)))))
            (unless completed
              (puthash key request e-anthropic--context-window-refresh-requests))
            request)))))

;;;###autoload
(defun e-anthropic-context-window (model &optional provider)
  "Return PROVIDER's context window (max input tokens) for MODEL, or nil.
The value comes from the in-memory `/models' catalog cache.  Use
`e-anthropic-refresh-context-window-cache' to refresh that cache asynchronously.
Returns nil when no cached catalog is available or MODEL is not listed."
  (when (stringp model)
    (when-let* ((table (gethash (or provider e-anthropic-default-provider)
                              e-anthropic--context-window-cache)))
      (gethash model table))))

;;;###autoload
(defun e-anthropic-reset-context-window-cache ()
  "Clear the in-memory provider context-window cache.
Cancel in-flight refreshes and clear the negative cache of failed fetches.
The next explicit `e-anthropic-refresh-context-window-cache' call can
repopulate the cache; ordinary context-window lookups remain cache-only."
  (interactive)
  (maphash
   (lambda (_key request)
     (ignore-errors
       (e-backend-cancel-request request)))
   e-anthropic--context-window-refresh-requests)
  (clrhash e-anthropic--context-window-refresh-requests)
  (clrhash e-anthropic--context-window-cache)
  (clrhash e-anthropic--context-window-failure-cache))

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
  (and (eq (plist-get message :role) 'system)
       (not (e-anthropic--request-local-source-marker message))))

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
Anthropic requires an object; canonical nil is its empty object."
  arguments)

(defun e-anthropic--tool-result-block (message)
  "Return the Messages `tool_result' block for backend-neutral MESSAGE."
  (let ((content (plist-get message :content)))
    (list :type "tool_result"
          :tool_use_id (plist-get content :tool-call-id)
          :content (e-tools-result-content-text (plist-get content :content)))))

(defun e-anthropic--tool-result-presentation-blocks
    (messages result-markers)
  "Return marker and tool_result blocks for result MESSAGES.
RESULT-MARKERS maps each marked tool result to its request-local marker."
  (let (blocks)
    (dolist (message messages)
      (when-let* ((marker (gethash message result-markers)))
        (push (e-anthropic--text-block (plist-get marker :content)) blocks))
      (push (e-anthropic--tool-result-block message) blocks))
    (nreverse blocks)))

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

(defun e-anthropic--message-replay-items (message)
  "Return provider replay records carried by tool-result MESSAGE metadata."
  (let ((records
         (and (eq (plist-get message :role) 'tool)
              (plist-get (plist-get message :metadata)
                         :provider-replay-items))))
    (cond
     ((null records) nil)
     ((and (vectorp records) (not (stringp records)))
      (append records nil))
     ((proper-list-p records) records)
     (t
      (signal 'e-anthropic-response-invalid
              '("malformed provider replay bundle"))))))

(defun e-anthropic--anthropic-replay-blocks (message)
  "Return Anthropic native content blocks carried by result MESSAGE.
Ignore provider replay records belonging to another adapter."
  (when (eq (plist-get message :role) 'tool)
    (let ((records
           (seq-filter
            (lambda (record)
              (member (plist-get record :provider-id) '(anthropic "anthropic")))
            (e-anthropic--message-replay-items message))))
      (when records
        (mapcar
         (lambda (record)
           (let ((block (plist-get record :item)))
             (unless (and (member (plist-get record :type)
                                  '(provider-replay-item "provider-replay-item"))
                          (listp block)
                          (stringp (plist-get block :type)))
               (signal 'e-anthropic-response-invalid
                       '("malformed Anthropic native replay block")))
             (copy-tree block)))
         records)))))

(defun e-anthropic--json-object-p (value)
  "Return non-nil when VALUE is a canonical JSON object plist."
  (and (proper-list-p value)
       (zerop (% (length value) 2))
       (let ((tail value)
             (valid t))
         (while tail
           (unless (keywordp (car tail))
             (setq valid nil))
           (setq tail (cddr tail)))
         valid)))

(defun e-anthropic--request-local-source-marker (message)
  "Return MESSAGE's supported typed request-local source marker, or nil.
Malformed reserved marker metadata fails instead of being sent as a system
message or silently detached from its source."
  (let ((metadata (plist-get message :metadata)))
    (when (and metadata (not (e-anthropic--json-object-p metadata)))
      (signal 'e-anthropic-response-invalid
              '("malformed message metadata")))
    (when (plist-member metadata
                        e-context-lifetime--request-local-source-marker-key)
      (let ((marker (plist-get metadata
                               e-context-lifetime--request-local-source-marker-key)))
        (unless (and (e-anthropic--json-object-p marker)
                     (= (length marker) 4)
                     (plist-member marker :kind)
                     (eq (plist-get marker :kind)
                         e-context-lifetime--request-local-tool-result-marker-kind)
                     (plist-member marker :tool-call-id)
                     (stringp (plist-get marker :tool-call-id))
                     (not (string-empty-p (plist-get marker :tool-call-id))))
          (signal 'e-anthropic-response-invalid
                  '("malformed request-local tool-result marker")))
        marker))))

(defun e-anthropic--tool-result-marker-indexes (messages)
  "Return typed source-marker pair indexes for MESSAGES.
Every marker must immediately precede its exact tool result."
  (let ((marker-results (make-hash-table :test 'eq))
        (result-markers (make-hash-table :test 'eq))
        (seen-call-ids (make-hash-table :test 'equal)))
    (cl-loop for tail on messages
             for marker-message = (car tail)
             for marker = (e-anthropic--request-local-source-marker
                           marker-message)
             when marker
             do
             (let* ((result (cadr tail))
                    (result-content (and result (plist-get result :content)))
                    (call-id (plist-get marker :tool-call-id)))
               (unless (and (eq (plist-get marker-message :role) 'system)
                            (stringp (plist-get marker-message :content))
                            (not (string-empty-p
                                  (plist-get marker-message :content)))
                            (eq (plist-get result :role) 'tool)
                            (e-anthropic--json-object-p result-content)
                            (equal call-id
                                   (plist-get result-content :tool-call-id)))
                 (signal 'e-anthropic-response-invalid
                         '("request-local tool-result marker is detached from its result")))
               (when (or (gethash call-id seen-call-ids)
                         (gethash result result-markers))
                 (signal 'e-anthropic-response-invalid
                         '("duplicate request-local tool-result marker")))
               (puthash call-id t seen-call-ids)
               (puthash marker-message result marker-results)
               (puthash result marker-message result-markers)))
    (list :marker-results marker-results
          :result-markers result-markers)))

(defun e-anthropic--validate-native-replay-blocks (blocks)
  "Validate required native fields in Anthropic replay BLOCKS.
Return the tool-use ids in native order."
  (let ((seen-ids (make-hash-table :test 'equal))
        tool-use-ids)
    (dolist (block blocks)
      (pcase (plist-get block :type)
        ("thinking"
         (unless (and (stringp (plist-get block :thinking))
                      (not (string-empty-p (plist-get block :thinking)))
                      (stringp (plist-get block :signature))
                      (not (string-empty-p (plist-get block :signature))))
           (signal 'e-anthropic-response-invalid
                   '("thinking block is missing signed replay material"))))
        ("redacted_thinking"
         (unless (and (stringp (plist-get block :data))
                      (not (string-empty-p (plist-get block :data))))
           (signal 'e-anthropic-response-invalid
                   '("redacted thinking block is missing replay material"))))
        ("tool_use"
         (let ((id (plist-get block :id)))
           (unless (and (stringp id) (not (string-empty-p id))
                        (stringp (plist-get block :name))
                        (not (string-empty-p (plist-get block :name)))
                        (e-anthropic--json-object-p (plist-get block :input)))
             (signal 'e-anthropic-response-invalid
                     '("tool_use block has malformed id, name, or input")))
           (when (gethash id seen-ids)
             (signal 'e-anthropic-response-invalid
                     '("duplicate tool_use id in native replay")))
           (puthash id t seen-ids)
           (push id tool-use-ids)))))
    (nreverse tool-use-ids)))

(defun e-anthropic--grouped-tool-followup (indexes blocks)
  "Return native assistant and user messages for replay BLOCKS.
INDEXES contains request-local call, result, and message-position indexes.
Every native tool-use id must match one neutral call and result.  The returned
plist also lists message objects to suppress from ordinary per-call rendering."
  (let ((tool-use-ids (e-anthropic--validate-native-replay-blocks blocks))
        (calls-by-id (plist-get indexes :calls))
        (results-by-id (plist-get indexes :results))
        (result-markers (plist-get indexes :result-markers))
        (positions (plist-get indexes :positions))
        (call-messages nil)
        (result-messages nil)
        marker-messages
        first-call
        first-call-position)
    (unless tool-use-ids
      (signal 'e-anthropic-response-invalid
              '("native replay bundle has no tool_use block")))
    (dolist (id tool-use-ids)
      (let* ((calls (gethash id calls-by-id))
             (results (gethash id results-by-id))
             (call (car calls))
             (result (car results))
             (call-position (gethash call positions))
             (result-position (gethash result positions)))
        (unless (= (length calls) 1)
          (signal 'e-anthropic-response-invalid
                  '("native tool_use does not have exactly one tool-call record")))
        (unless (= (length results) 1)
          (signal 'e-anthropic-response-invalid
                  '("native tool_use does not have exactly one tool result")))
        (when (<= result-position call-position)
          (signal 'e-anthropic-response-invalid
                  '("tool result precedes its tool-call record")))
        (when (or (null first-call-position)
                  (< call-position first-call-position))
          (setq first-call call
                first-call-position call-position))
        (push call call-messages)
        (push result result-messages)))
    (setq call-messages (nreverse call-messages)
          result-messages (nreverse result-messages))
    (setq marker-messages
          (delq nil (mapcar (lambda (result)
                              (gethash result result-markers))
                            result-messages)))
    (list :first-call first-call
          :messages
          (list (list :role "assistant" :content (vconcat blocks))
                (list :role "user"
                      :content
                      (vconcat
                       (e-anthropic--tool-result-presentation-blocks
                        result-messages result-markers))))
          :consumed (append call-messages marker-messages result-messages))))

(defun e-anthropic--messages (messages)
  "Map backend-neutral MESSAGES to grouped native Messages turns.
Pre-scan each immediate tool-result carrier so its native response replaces,
rather than follows, the individual call/result entries in transcript order."
  (let ((positions (make-hash-table :test 'eq))
        (calls-by-id (make-hash-table :test 'equal))
        (results-by-id (make-hash-table :test 'equal))
        (marker-pairs (e-anthropic--tool-result-marker-indexes messages))
        (consumed (make-hash-table :test 'eq))
        (replay-groups (make-hash-table :test 'eq))
        (wire-messages-reversed nil)
        (position 0))
    (dolist (message messages)
      (unless (gethash message positions)
        (puthash message position positions))
      (setq position (1+ position))
      (pcase (plist-get message :role)
        ('tool-call
         (let* ((id (plist-get (plist-get message :content) :id))
                (matches (gethash id calls-by-id)))
           (puthash id (cons message matches) calls-by-id)))
        ('tool
         (let* ((id (plist-get (plist-get message :content) :tool-call-id))
                (matches (gethash id results-by-id)))
           (puthash id (cons message matches) results-by-id)))))
    (let ((indexes (list :positions positions
                         :calls calls-by-id
                         :results results-by-id
                         :result-markers
                         (plist-get marker-pairs :result-markers))))
      (dolist (message messages)
        (when-let* ((blocks (e-anthropic--anthropic-replay-blocks message)))
          (let* ((group (e-anthropic--grouped-tool-followup indexes blocks))
                 (members (plist-get group :consumed))
                 (first-call (plist-get group :first-call)))
            (unless first-call
              (signal 'e-anthropic-response-invalid
                      '("native replay bundle has no matching tool-call")))
            (when (or (gethash first-call replay-groups)
                      (seq-some (lambda (member-message)
                                  (gethash member-message consumed))
                                members))
              (signal 'e-anthropic-response-invalid
                      '("overlapping Anthropic replay bundles in one request")))
            (puthash first-call group replay-groups)
            (dolist (member-message members)
              (puthash member-message t consumed)))))
      (dolist (message messages)
        (let ((group (gethash message replay-groups)))
          (cond
           (group
            (dolist (wire-message (plist-get group :messages))
              (push wire-message wire-messages-reversed)))
           ((and (not (gethash message consumed))
                 (gethash message (plist-get marker-pairs :marker-results)))
            (let ((result (gethash message
                                    (plist-get marker-pairs :marker-results))))
              (push (list :role "user"
                          :content
                          (vconcat
                           (e-anthropic--tool-result-presentation-blocks
                            (list result)
                            (plist-get marker-pairs :result-markers))))
                    wire-messages-reversed)
              (puthash message t consumed)
              (puthash result t consumed)))
           ((not (gethash message consumed))
            (push (e-anthropic--message message) wire-messages-reversed)))))
      (nreverse wire-messages-reversed))))

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

(defun e-anthropic--cache-breakpoint-selection (messages options)
  "Return the stable breakpoint in current MESSAGES described by OPTIONS.
The result contains a system-block :index and, when selected by a segment, its
:segment.  Segment messages locate the boundary; they never supply wire text."
  (let* ((instructions (plist-get options :instructions))
         (instruction-p (and (stringp instructions)
                             (not (string-empty-p instructions))))
         (system-messages (seq-filter #'e-anthropic--system-message-p messages))
         (segments (plist-get options :segments))
         (message-cursor 0)
         (selection (and instruction-p (list :index 0))))
    (if segments
        (dolist (segment segments)
          (dolist (segment-message (plist-get segment :messages))
            (when (e-anthropic--system-message-p segment-message)
              (when-let* ((position
                           (cl-position
                            segment-message system-messages
                            :start message-cursor
                            :test #'equal)))
                (setq message-cursor (1+ position))
                (when (e-anthropic--stable-cache-segment-p segment)
                  (setq selection
                        (list :index (+ (if instruction-p 1 0) position)
                              :segment segment)))))))
      (when (and (not instruction-p) system-messages)
        (setq selection (list :index (1- (length system-messages))))))
    selection))

(defun e-anthropic--cache-breakpoint-segment (options messages)
  "Return the stable segment selected for current MESSAGES, or nil."
  (plist-get (e-anthropic--cache-breakpoint-selection messages options)
             :segment))

(defun e-anthropic--system-blocks (messages options cache-control)
  "Return cached system blocks from the current MESSAGES projection.
OPTIONS segments choose the eligible stable CACHE-CONTROL position only."
  (let* ((instructions (plist-get options :instructions))
         (instruction-p (and (stringp instructions)
                             (not (string-empty-p instructions))))
         (system-messages (seq-filter #'e-anthropic--system-message-p messages))
         (system-contents
          (delq nil (mapcar (lambda (message)
                              (plist-get message :content))
                            system-messages))))
    (if (and (null (plist-get options :segments)) (not instruction-p))
        (vector
         (append (e-anthropic--text-block
                  (e-anthropic--system messages options))
                 (list :cache_control cache-control)))
      (let* ((blocks
              (append (when instruction-p
                        (list (e-anthropic--text-block instructions)))
                      (mapcar #'e-anthropic--text-block system-contents)))
             (selection (e-anthropic--cache-breakpoint-selection
                         messages options)))
        (when selection
          (let* ((index (plist-get selection :index))
                 (block (nth index blocks)))
            (when block
              (setcar (nthcdr index blocks)
                      (append block (list :cache_control cache-control))))))
        (when blocks
          (vconcat blocks))))))

(defun e-anthropic--thinking-type (options)
  "Return the `thinking.type' value for OPTIONS, or nil to omit thinking.
`:anthropic-thinking' controls extended thinking per request/provider: when the
key is absent, default to `adaptive' so ordinary chat is unchanged; a nil value
omits the thinking and effort knobs (for models that reject them, e.g. Haiku);
a string overrides the type."
  (if (plist-member options :anthropic-thinking)
      (let ((value (plist-get options :anthropic-thinking)))
        (cond
         ((null value) nil)
         ((stringp value) value)
         (t "adaptive")))
    "adaptive"))

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
                             (e-anthropic--system-blocks
                              messages options cache-control)))
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
                                         (e-anthropic--messages turns)))))
    ;; Extended thinking is opt-outable per request: some gateway models (e.g.
    ;; Haiku) reject `adaptive' thinking outright.  `:anthropic-thinking' nil
    ;; omits the thinking + effort knobs entirely; absent it, keep the adaptive
    ;; default so ordinary chat is unchanged.
    (when-let* ((thinking-type (e-anthropic--thinking-type options)))
      (setq body (append body
                         (list :thinking (list :type thinking-type)
                               :output_config (list :effort effort)))))
    (when-let* ((container (plist-get options :anthropic-container-id)))
      (when (and (stringp container) (not (string-empty-p container)))
        (setq body (append body (list :container container)))))
    (when-let* ((context-management
                (plist-get options :anthropic-context-management)))
      (setq body
            (append body (list :context_management context-management))))
    (when tool-defs
      (setq body (append body (list :tools tool-defs))))
    body))

(defun e-anthropic--request-metadata (options body &optional messages)
  "Return sanitized Anthropic request metadata for OPTIONS and BODY.
MESSAGES is the current neutral projection used to identify a stable segment."
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
                               (vectorp (plist-get body :system))
                               (cl-some
                                (lambda (block)
                                  (plist-member block :cache_control))
                                (append (plist-get body :system) nil)))
                          'system-stable-prefix)
                         ((and (plist-member body :tools)
                               (cl-some
                                (lambda (tool)
                                  (plist-member tool :cache_control))
                                (append (plist-get body :tools) nil)))
                          'tools)
                         (t 'none))))
        (setq metadata
              (append metadata
                      (list :anthropic-cache-mode mode
                            :anthropic-cache-breakpoint breakpoint
                            :full-history t)))
        (when-let* ((breakpoint-segment
                    (and (eq breakpoint 'system-stable-prefix)
                         (e-anthropic--cache-breakpoint-segment
                          options messages))))
          (setq metadata
                (append metadata
                        (list :anthropic-breakpoint-segment-id
                              (prin1-to-string
                               (plist-get breakpoint-segment :id))
                              :anthropic-breakpoint-fingerprint
                              (plist-get breakpoint-segment :fingerprint)))))
        (when-let* ((ttl (plist-get options :prompt-cache-ttl)))
          (setq metadata
                (append metadata
                        (list :anthropic-cache-ttl ttl))))))
    (when-let* ((container (plist-get options :anthropic-container-id)))
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
    (when-let* ((segments (plist-get options :segments)))
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
  (e-json-parse-string value))

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
`input_tokens' excludes both cache reads and cache writes, so the total sums all
three plus output.  The context-input count carries that sum for budgeting.
Messages does not report a total itself; derive it here so
downstream consumers see the same `:total-tokens' field the other adapters
provide."
  (let* ((input (e-anthropic--number-or-nil input-tokens))
         (cached (e-anthropic--number-or-nil cached-tokens))
         (created (e-anthropic--number-or-nil created-tokens))
         (output (e-anthropic--number-or-nil output-tokens))
         (context-input (and input (+ input (or cached 0) (or created 0))))
         (parts (delq nil (list input cached created output)))
         (total (and parts (apply #'+ parts))))
    (list :type 'token-usage
          :usage (list :context-input-tokens context-input
                       :input-tokens input
                       :cached-input-tokens cached
                       :cache-creation-input-tokens created
                       :output-tokens output
                       :reasoning-output-tokens nil
                       :total-tokens total))))

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

(defun e-anthropic--invalid-response-item (reason &optional provider-event)
  "Return a backend error item for an invalid response REASON."
  (list :type 'backend-error
        :content (format "Invalid Anthropic Messages response: %s" reason)
        :payload
        (append '(:response-kind invalid-messages-stream
                  :error-type "invalid_messages_response")
                (when provider-event
                  (list :provider-event provider-event)))))

(defun e-anthropic-parse-stream (stream-text)
  "Parse Anthropic Messages STREAM-TEXT into validated backend-neutral items.

Content blocks are collected by native index.  A complete terminal response
must be validated before any tool call or opaque native replay item is exposed.
Text deltas across blocks remain a single `assistant-message'.  Non-stream JSON
errors are surfaced as `backend-error' items because gateways can return those
instead of an SSE stream."
  (condition-case err
      (let ((blocks (make-hash-table :test 'eql))
            (block-order nil)
            (next-block-index 0)
            (open-block-count 0)
            (progress-items nil)
            (input-tokens nil)
            (cached-tokens nil)
            (created-tokens nil)
            (output-tokens nil)
            (stop-reason nil)
            (usage-seen nil)
            (message-start-seen nil)
            (message-delta-seen nil)
            (terminal-seen nil)
            (event-count 0))
        (cl-labels
          ((reject
            (reason &optional provider-event)
            (signal 'e-anthropic-response-invalid
                    (list reason provider-event)))
           (block-entry (index)
            (gethash index blocks))
           (entry-block (entry)
            (plist-get entry :block))
           (entry-set (index entry key value)
            (puthash index (plist-put entry key value) blocks))
           (absorb-usage
            (usage)
            (when usage
              (unless (e-anthropic--json-object-p usage)
                (reject "malformed usage object"))
              ;; Usage is cumulative and re-stated on message_delta; take the
              ;; latest non-nil value for each field (last writer wins).
              (setq usage-seen t)
              (when (plist-member usage :input_tokens)
                (setq input-tokens (plist-get usage :input_tokens)))
              (when (plist-member usage :cache_read_input_tokens)
                (setq cached-tokens
                      (plist-get usage :cache_read_input_tokens)))
              (when (plist-member usage :cache_creation_input_tokens)
                (setq created-tokens
                      (plist-get usage :cache_creation_input_tokens)))
              (when (plist-member usage :output_tokens)
                (setq output-tokens (plist-get usage :output_tokens))))))
          (dolist (event (e-anthropic--sse-data stream-text))
            (setq event-count (1+ event-count))
            (when terminal-seen
              (reject "event received after message_stop" event))
            (let ((event-type (plist-get event :type)))
              (unless (stringp event-type)
                (reject "event is missing its type" event))
              (pcase event-type
                ("message_start"
                 (when (or message-start-seen message-delta-seen
                           (> next-block-index 0))
                   (reject "message_start is out of order" event))
                 (setq message-start-seen t)
                 (let ((message (plist-get event :message)))
                   (when (and message
                              (not (e-anthropic--json-object-p message)))
                     (reject "malformed message_start object" event))
                   (absorb-usage (plist-get message :usage))))
                ("content_block_start"
                 (when message-delta-seen
                   (reject "content block started after message_delta" event))
                 (let ((index (plist-get event :index))
                       (block (plist-get event :content_block)))
                   (unless (and (integerp index)
                                (= index next-block-index)
                                (e-anthropic--json-object-p block)
                                (stringp (plist-get block :type)))
                     (reject "content block start has malformed index or block"
                             event))
                   (puthash index
                            (list :block (copy-tree block)
                                  :text-chunks nil
                                  :thinking-chunks nil
                                  :partial-json-chunks nil
                                  :stopped nil)
                            blocks)
                   (push index block-order)
                   (setq next-block-index (1+ next-block-index)
                         open-block-count (1+ open-block-count))))
                ("content_block_delta"
                 (when message-delta-seen
                   (reject "content block delta followed message_delta" event))
                 (let* ((index (plist-get event :index))
                        (entry (and (integerp index)
                                    (block-entry index)))
                        (delta (plist-get event :delta))
                        (delta-type (plist-get delta :type)))
                   (unless (and entry
                                (not (plist-get entry :stopped))
                                (e-anthropic--json-object-p delta)
                                (stringp delta-type))
                     (reject "content block delta has invalid order or shape"
                             event))
                   (let* ((block (entry-block entry))
                          (block-type (plist-get block :type)))
                     (pcase delta-type
                       ("text_delta"
                        (let ((text (plist-get delta :text)))
                          (unless (and (equal block-type "text")
                                       (stringp text))
                            (reject "text delta does not match its content block"
                                    event))
                          (entry-set
                           index entry :text-chunks
                           (cons text (plist-get entry :text-chunks)))
                          (push (list :type 'assistant-delta :content text)
                                progress-items)))
                       ("thinking_delta"
                        (let ((thinking (plist-get delta :thinking)))
                          (unless (and (equal block-type "thinking")
                                       (stringp thinking)
                                       (not (plist-member block :signature)))
                            (reject
                             "thinking delta does not match its content block"
                             event))
                          (entry-set
                           index entry :thinking-chunks
                           (cons thinking (plist-get entry :thinking-chunks)))
                          (push (list :type 'reasoning-raw-delta
                                      :stream-kind 'raw
                                      :content thinking
                                      :content-index index)
                                progress-items)))
                       ("signature_delta"
                        (let* ((signature (plist-get delta :signature))
                               (thinking
                                (concat (or (plist-get block :thinking) "")
                                        (mapconcat
                                         #'identity
                                         (nreverse
                                          (plist-get entry :thinking-chunks))
                                         ""))))
                          (unless (and (equal block-type "thinking")
                                       (stringp thinking)
                                       (not (string-empty-p thinking))
                                       (stringp signature)
                                       (not (string-empty-p signature))
                                       (not (plist-member block :signature)))
                            (reject "signature delta is malformed or misplaced"
                                    event))
                          (entry-set index entry :block
                                     (plist-put block :thinking thinking))
                          (entry-set index entry :thinking-chunks nil)
                          (entry-set index entry :block
                                     (plist-put (entry-block entry)
                                                :signature signature))))
                       ("input_json_delta"
                        (let ((partial-json (plist-get delta :partial_json)))
                          (unless (and (equal block-type "tool_use")
                                       (stringp partial-json))
                            (reject
                             "input JSON delta does not match a tool_use block"
                             event))
                          (entry-set
                           index entry :partial-json-chunks
                           (cons partial-json
                                 (plist-get entry :partial-json-chunks)))))
                       ("citations_delta"
                        (let ((citation (plist-get delta :citation)))
                          (unless (and (equal block-type "text")
                                       (e-anthropic--json-object-p citation))
                            (reject
                             "citation delta does not match its text block"
                             event))
                          (let* ((citations (plist-get block :citations))
                                 (citations
                                  (if (vectorp citations)
                                      (append citations nil)
                                    citations)))
                            (unless (or (null citations)
                                        (proper-list-p citations))
                              (reject "malformed text citations" event))
                            (entry-set
                             index entry :block
                             (plist-put
                              block :citations
                              (vconcat (append citations (list citation))))))))
                       (_
                        (reject
                         (format "unsupported content block delta %s"
                                 delta-type)
                         event))))))
                ("content_block_stop"
                 (when message-delta-seen
                   (reject "content block stopped after message_delta" event))
                 (let* ((index (plist-get event :index))
                        (entry (and (integerp index)
                                    (block-entry index))))
                   (unless (and entry
                                (not (plist-get entry :stopped)))
                     (reject "content block stop has invalid order" event))
                   (let* ((block (entry-block entry))
                          (block-type (plist-get block :type)))
                     (cond
                      ((equal block-type "tool_use")
                       (let* ((partial-json
                               (mapconcat
                                #'identity
                                (nreverse
                                 (plist-get entry :partial-json-chunks))
                                ""))
                              (input
                               (if (string-empty-p partial-json)
                                   (plist-get block :input)
                                 (e-anthropic--parse-tool-input partial-json))))
                         (unless (e-anthropic--json-object-p input)
                           (reject "tool_use input is not a JSON object" event))
                         (entry-set index entry :block
                                    (plist-put block :input input))))
                      ((equal block-type "text")
                       (when (plist-get entry :text-chunks)
                         (entry-set
                          index entry :block
                          (plist-put
                           block :text
                           (concat (or (plist-get block :text) "")
                                   (mapconcat
                                    #'identity
                                    (nreverse (plist-get entry :text-chunks))
                                    ""))))
                         (entry-set index entry :text-chunks nil)))
                      ((equal block-type "thinking")
                       (when (plist-get entry :thinking-chunks)
                         (entry-set
                          index entry :block
                          (plist-put
                           block :thinking
                           (concat (or (plist-get block :thinking) "")
                                   (mapconcat
                                    #'identity
                                    (nreverse
                                     (plist-get entry :thinking-chunks))
                                    ""))))
                         (entry-set index entry :thinking-chunks nil))))
                   (entry-set index entry :stopped t)
                   (setq open-block-count (1- open-block-count)))))
                ("message_delta"
                 (when message-delta-seen
                   (reject "duplicate message_delta event" event))
                 (when (> open-block-count 0)
                   (reject "message_delta arrived before content blocks stopped"
                           event))
                 (let ((delta (plist-get event :delta)))
                   (unless (e-anthropic--json-object-p delta)
                     (reject "malformed message_delta object" event))
                   (when (plist-member delta :stop_reason)
                     (unless (stringp (plist-get delta :stop_reason))
                       (reject "malformed stop_reason" event))
                     (setq stop-reason (plist-get delta :stop_reason))))
                 (absorb-usage (plist-get event :usage))
                 (setq message-delta-seen t))
                ("message_stop"
                 (when (> open-block-count 0)
                   (reject "message_stop arrived before content blocks stopped"
                           event))
                 (setq terminal-seen t))
                ("error"
                 (let ((provider-error (plist-get event :error)))
                   (reject (or (plist-get provider-error :message)
                               "provider error event")
                           event)))
                ("ping" nil)
                (_
                 (reject (format "unsupported event type %s" event-type)
                         event)))))
          (cond
           ((zerop event-count)
            (if (string-match-p "\\`[[:space:]]*\\'" stream-text)
                nil
              (when-let* ((error-item
                           (e-anthropic--non-stream-error-item stream-text)))
                (list error-item))))
           ((not terminal-seen)
            (reject "stream ended before message_stop"))
           (t
            (let* ((ordered-blocks
                    (mapcar (lambda (index) (entry-block
                                             (block-entry index)))
                            (nreverse block-order)))
                   (tool-use-blocks
                    (seq-filter
                     (lambda (block)
                       (equal (plist-get block :type) "tool_use"))
                     ordered-blocks))
                   (tool-use-ids
                    (when tool-use-blocks
                      (e-anthropic--validate-native-replay-blocks
                       ordered-blocks)))
                   (assistant-text
                    (apply #'concat
                           (mapcar (lambda (block)
                                     (if (equal (plist-get block :type) "text")
                                         (or (plist-get block :text) "")
                                       ""))
                                   ordered-blocks)))
                   (replay-items
                    (when tool-use-ids
                      (mapcar (lambda (block)
                                (list :type 'provider-replay-item
                                      :provider-id 'anthropic
                                      :item block))
                              ordered-blocks)))
                   (tool-call-items
                    (mapcar (lambda (block)
                              (list :type 'tool-call
                                    :id (plist-get block :id)
                                    :name (plist-get block :name)
                                    :arguments (plist-get block :input)))
                            tool-use-blocks)))
              (when (and tool-use-ids
                         (equal stop-reason "max_tokens"))
                (reject "tool_use response was truncated at max_tokens"))
              (when (and tool-use-ids
                         (not (equal stop-reason "tool_use")))
                (reject "tool_use blocks require a tool_use stop_reason"))
              (when (and (equal stop-reason "tool_use")
                         (not tool-use-ids))
                (reject "tool_use stop_reason has no tool_use block"))
              (append
               (nreverse progress-items)
               (when (not (string-empty-p assistant-text))
                 (list (list :type 'assistant-message
                             :content assistant-text)))
               tool-call-items
               replay-items
               (when usage-seen
                 (list (e-anthropic--usage-item
                        input-tokens output-tokens cached-tokens created-tokens)))
               (list (list :type 'done
                           :reason
                           (e-anthropic--stop-reason-symbol stop-reason)))))))))
    (e-anthropic-response-invalid
     (list (e-anthropic--invalid-response-item
            (or (cadr err) "malformed response") (caddr err))))
    (e-json-error
     (list (e-anthropic--invalid-response-item
            "invalid JSON event or tool input")))))

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
  "Cancel any live request process attached to BUFFER and kill BUFFER.
Delegates to `e-kill-buffer-quietly' so a still-live gateway connection can
never raise the blocking \"has a running process; kill it?\" prompt."
  (e-kill-buffer-quietly buffer))

(cl-defun e-anthropic--http-request-start
    (&key url headers body on-complete on-error (method "POST"))
  "Send METHOD request to URL with HEADERS and optional BODY asynchronously.
ON-COMPLETE receives the response body text.  ON-ERROR receives an Emacs
condition list.  Return a cancellable `e-backend-request' handle."
  (let ((url-request-method method)
        (url-request-extra-headers (e-anthropic--http-header-list headers))
        (url-request-data (and body (encode-coding-string body 'utf-8)))
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
                      (format "Anthropic request timed out after %s seconds without response data"
                              timeout))))))
         (arm-timeout ()
           ;; Idle deadline: re-arm on every chunk of response data so a long
           ;; but healthy streamed generation is never killed mid-flight; only
           ;; a connection silent for TIMEOUT seconds fails.
           (cancel-timeout)
           (when (and timeout (not settled))
             (setq timeout-timer
                   (run-at-time timeout nil #'settle-timeout))))
         (note-activity (&rest _)
           (unless settled
             (arm-timeout)))
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
                                          (e-format-safe
                                           "Anthropic request failed: %S"
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
      ;; Re-arm the idle timer on each chunk of received data.  `url-retrieve'
      ;; inserts the response into REQUEST-BUFFER incrementally, so a
      ;; buffer-local `after-change-functions' entry fires on every chunk
      ;; without depending on url internals.
      (when (buffer-live-p request-buffer)
        (with-current-buffer request-buffer
          (add-hook 'after-change-functions #'note-activity nil t)))
      (arm-timeout))
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
  (e-anthropic--reject-sync-in-hot-path 'e-anthropic--http-request)
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
  (when-let* ((metadata (plist-get context :metadata)))
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
      (setq item (e-anthropic--normalize-backend-error-item item))
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
                      effective-options body-data messages)))
      (list :provider provider
            :url (e-anthropic-messages-url
                  (or base-url (e-anthropic--provider-base-url profile)))
            :headers headers
            :metadata metadata
            :body (e-json-serialize body-data)))))

(cl-defun e-anthropic-backend-create
    (&key provider base-url request-function name model)
  "Create an Anthropic Messages backend named NAME.
PROVIDER selects a profile from `e-anthropic-model-providers'.  BASE-URL
overrides the profile base URL.  REQUEST-FUNCTION is injectable for tests.
MODEL is the backend-local default when turn options omit `:model'."
  (let ((provider (or provider e-anthropic-default-provider)))
    (e-backend-create
     :name (or name (e-anthropic-provider-name provider))
     :normalize-error-details #'e-anthropic--normalize-error-details
     :stream
     (cl-function
      (lambda (&key messages options on-item)
        (e-anthropic--reject-sync-in-hot-path 'e-anthropic-backend-stream)
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

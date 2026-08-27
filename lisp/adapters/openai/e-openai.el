;;; e-openai.el --- OpenAI/Codex backend adapter for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; OpenAI Responses/Codex adapter.  Provider auth, endpoint shape, request
;; mapping, and SSE parsing stay here instead of leaking into the harness.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'websocket)
(require 'e-backend)
(require 'e-harness)
(require 'e-request)
(require 'e-tools)
(require 'e-work)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-openai-auth-missing "OpenAI/Codex auth is missing")
(define-error 'e-openai-auth-invalid "OpenAI/Codex auth is invalid")
(define-error 'e-openai-provider-invalid "OpenAI provider profile is invalid")
(define-error 'e-openai-context-projection-invalid
  "OpenAI context projection is ambiguous")
(define-error 'e-openai-request-timeout "OpenAI/Codex request timed out")

(defconst e-openai--retryable-error-patterns
  '("rate limit" "rate_limit_error" "too many requests"
    "overloaded" "overloaded_error" "api_error"
    "internal_server_error" "server_error" "service unavailable"
    "bad gateway" "gateway time" "request timed out" "idle timed out"
    "connection termination" "connection reset" "reset by peer"
    "connect error" "before headers" "disconnect" "broken pipe"
    "premature")
  "OpenAI and gateway error fragments that identify transient failures.")

(defun e-openai--retryable-status-p (status)
  "Return non-nil when OpenAI HTTP STATUS permits a retry."
  (and (numberp status)
       (or (memq status '(408 409 429)) (>= status 500))))

(defun e-openai--retry-after-from-text (message &optional now)
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

(defun e-openai--error-code (details)
  "Return an OpenAI error code or type from DETAILS, when present."
  (let* ((error (and (listp details) (plist-get details :error)))
         (response (and (listp details) (plist-get details :response)))
         (response-error (and (listp response) (plist-get response :error))))
    (or (and (listp details) (plist-get details :error-type))
        (and (listp error) (or (plist-get error :code)
                               (plist-get error :type)))
        (and (listp response-error)
             (or (plist-get response-error :code)
                 (plist-get response-error :type))))))

(defun e-openai--normalize-error-details (message details condition)
  "Return OpenAI-owned normalized retry metadata for an error.
MESSAGE, DETAILS, and CONDITION are the backend error surfaces received by the
provider-neutral backend contract."
  (let* ((normalized (if (listp details) (copy-tree details) nil))
         (text (downcase (or message "")))
         (status (or (plist-get normalized :status)
                     (plist-get normalized :status-code)))
         (code (e-openai--error-code normalized))
         (code-text (downcase (format "%s" (or code ""))))
         (timeout-p (eq (car-safe condition) 'e-openai-request-timeout))
         (pattern (seq-find (lambda (candidate)
                              (string-match-p (regexp-quote candidate) text))
                            e-openai--retryable-error-patterns))
         (reason
          (cond
           ((or (equal status 429)
                (string-match-p "rate[_ -]?limit" code-text)
                (member pattern '("rate limit" "rate_limit_error"
                                  "too many requests")))
            'rate-limit)
           ((or timeout-p (equal status 408)
                (member pattern '("request timed out" "idle timed out")))
            'timeout)
           ((equal pattern "premature") 'premature-stream)
           ((equal status 409) 'conflict)
           ((or (e-openai--retryable-status-p status)
                (string-match-p
                 "\\(?:overloaded\\|api_error\\|server_error\\)" code-text)
                (member pattern '("overloaded" "overloaded_error" "api_error"
                                  "internal_server_error" "server_error"
                                  "service unavailable" "bad gateway"
                                  "gateway time")))
            'provider-unavailable)
           (pattern 'transport)))
         (retry-after
          (or (plist-get normalized :retry-after-seconds)
              (plist-get normalized :retry-after)
              (e-openai--retry-after-from-text message))))
    (setq normalized (plist-put normalized :retryable (and reason t)))
    (when reason
      (setq normalized (plist-put normalized :retry-reason reason)))
    (when (numberp retry-after)
      (setq normalized
            (plist-put normalized :retry-after-seconds retry-after)))
    normalized))

(defun e-openai--normalize-backend-error-item (item)
  "Return backend error ITEM with OpenAI-normalized payload details."
  (if (not (eq (plist-get item :type) 'backend-error))
      item
    (let ((normalized (copy-tree item)))
      (plist-put
       normalized :payload
       (e-openai--normalize-error-details
        (plist-get normalized :content)
        (plist-get normalized :payload)
        nil))
      normalized)))

(defun e-openai--profile-enabled-p ()
  "Return non-nil when developer profiling is available and enabled."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-openai--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when dev profiling is enabled."
  (if (e-openai--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-openai--reject-sync-in-hot-path (operation)
  "Reject synchronous OpenAI OPERATION from marked interactive hot paths."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error operation)))

(defconst e-openai-codex-default-base-url
  "https://chatgpt.com/backend-api"
  "Default base URL for ChatGPT-backed Codex Responses access.")

(defconst e-openai-api-default-base-url
  "https://api.openai.com/v1"
  "Default base URL for first-party OpenAI API access.")

(defconst e-openai-codex-account-claim
  "https://api.openai.com/auth"
  "JWT claim namespace containing the ChatGPT account id.")

(defgroup e-openai nil
  "OpenAI/Codex backend adapter for e."
  :group 'e
  :prefix "e-openai-")

(defvaralias 'e-openai-codex-default-model 'e-openai-default-model)

(defcustom e-openai-default-model "gpt-5.5"
  "Default model for OpenAI-like Responses requests."
  :type 'string
  :group 'e-openai)

(defcustom e-openai-default-reasoning-effort "high"
  "Default reasoning effort for OpenAI-like Responses requests."
  :type '(choice (const :tag "Unset" nil)
                 (string :tag "Effort"))
  :group 'e-openai)

(defcustom e-openai-default-reasoning-summary "auto"
  "Default Responses reasoning summary mode.

Responses requests always include this field.  `auto' is the provider's
ordinary summary mode; `detailed' is an explicit diagnostic choice."
  :type '(choice (const "auto") (const "detailed"))
  :group 'e-openai)

(defun e-openai--validate-reasoning-summary (value)
  "Return valid Responses reasoning summary VALUE, or signal an error."
  (unless (and (stringp value)
               (member value '("auto" "detailed")))
    (signal 'e-openai-provider-invalid
            (list (format "Invalid Responses reasoning summary %S" value))))
  value)

(defun e-openai--validate-reasoning-effort (value)
  "Return non-empty reasoning effort VALUE, or signal an error."
  (unless (and (stringp value)
               (not (string-empty-p value)))
    (signal 'e-openai-provider-invalid
            (list (format "Invalid Responses reasoning effort %S" value))))
  value)

(defun e-openai--keyword-plist-p (value)
  "Return non-nil when VALUE is a proper plist with keyword keys."
  (let ((tail value)
        (valid t))
    (while (and valid tail)
      (if (and (consp tail)
               (keywordp (car tail))
               (consp (cdr tail)))
          (setq tail (cddr tail))
        (setq valid nil)))
    (and valid (null tail))))

(defun e-openai--effective-reasoning (options)
  "Return validated Responses reasoning for OPTIONS.
An explicit `:reasoning' plist supplies the highest-precedence fields; absent
effort and summary fields are filled from the corresponding effective options.
The returned plist is detached from OPTIONS and always contains both required
fields."
  (let* ((explicit-p (plist-member options :reasoning))
         (explicit (and explicit-p (plist-get options :reasoning)))
         (option-summary
          (if (plist-member options :reasoning-summary)
              (plist-get options :reasoning-summary)
            e-openai-default-reasoning-summary)))
    (when explicit-p
      (unless (e-openai--keyword-plist-p explicit)
        (signal 'e-openai-provider-invalid
                (list (format "Invalid Responses reasoning %S" explicit))))
      (when (null explicit)
        (signal 'e-openai-provider-invalid
              '("Responses :reasoning cannot be nil"))))
    (let* ((reasoning (copy-tree (or explicit nil)))
           (effort
            (if (and explicit-p (plist-member explicit :effort))
                (plist-get explicit :effort)
              (if (plist-member options :reasoning-effort)
                  (plist-get options :reasoning-effort)
                e-openai-default-reasoning-effort)))
           (summary
            (if (and explicit-p (plist-member explicit :summary))
                (plist-get explicit :summary)
              option-summary)))
      (setq effort (e-openai--validate-reasoning-effort effort)
            summary (e-openai--validate-reasoning-summary summary))
      ;; Keep the required fields in a stable order while retaining any
      ;; provider-specific fields carried by an explicit reasoning plist.
      (cl-remf reasoning :effort)
      (cl-remf reasoning :summary)
      (append (list :effort effort) reasoning (list :summary summary)))))

(defcustom e-openai-default-text-verbosity "low"
  "Default GPT-5 Responses text verbosity for OpenAI-like requests."
  :type '(choice (const :tag "Unset" nil)
                 (string :tag "Verbosity"))
  :group 'e-openai)

(defcustom e-openai-default-provider 'codex
  "Default provider profile used by generic OpenAI harness helpers."
  :type 'symbol
  :group 'e-openai)

(defcustom e-openai-request-timeout-seconds nil
  "Optional idle timeout for OpenAI-like HTTP requests, in seconds.

The default is nil because an HTTP provider or gateway may buffer the response
while a long reasoning request is still healthy.  A local deadline cannot
distinguish that state from a stalled request.  Set a number only when the
selected HTTP provider guarantees response progress within that interval."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'e-openai)

(defcustom e-openai-websocket-idle-timeout-seconds 60
  "Seconds without Responses WebSocket events before a request fails.
When nil, Responses WebSocket requests do not time out locally."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'e-openai)

(defcustom e-openai-websocket-connection-idle-seconds 300
  "Seconds to retain an idle Responses WebSocket connection.
The connection stays open across compatible model/tool round trips so
`previous_response_id' can use the provider's connection-local cache.  Set
this to nil to retain idle connections until cancellation, failure, or backend
replacement."
  :type '(choice (const :tag "Retain until teardown" nil)
                 (number :tag "Seconds"))
  :group 'e-openai)

(defcustom e-openai-diagnostic-print-length 50
  "Maximum list/vector/hash entries printed in OpenAI diagnostic fallbacks."
  :type 'integer
  :group 'e-openai)

(defcustom e-openai-diagnostic-print-level 6
  "Maximum nested depth printed in OpenAI diagnostic fallbacks."
  :type 'integer
  :group 'e-openai)

(defcustom e-openai-diagnostic-string-max-bytes 4096
  "Maximum bytes shown for one string in OpenAI diagnostic fallbacks."
  :type 'integer
  :group 'e-openai)

(defcustom e-openai-diagnostic-result-max-bytes (* 8 1024)
  "Maximum bytes shown for a full OpenAI diagnostic fallback."
  :type 'integer
  :group 'e-openai)

(defun e-openai--custom-override-p (symbol)
  "Return non-nil when SYMBOL has a Custom override."
  (or (get symbol 'saved-value)
      (get symbol 'customized-value)
      (get symbol 'theme-value)))

(defun e-openai--migrate-websocket-idle-timeout-default ()
  "Adopt the current WebSocket idle timeout default across live reloads.

Reloading this file with an older live value leaves the defcustom variable
bound to the old default.  Preserve real Custom/theme overrides, but move
uncustomized old defaults to the current bounded value."
  (when (and (memq e-openai-websocket-idle-timeout-seconds '(nil 180))
             (not (e-openai--custom-override-p
                   'e-openai-websocket-idle-timeout-seconds)))
    (setq e-openai-websocket-idle-timeout-seconds 60)))

(defun e-openai--migrate-http-timeout-default ()
  "Disable the old implicit HTTP timeout across live reloads.

Reloading this file leaves the prior 180-second defcustom value bound.  Keep a
real Custom or theme override, but migrate the uncustomized old default to the
new provider-safe nil default."
  (when (and (equal e-openai-request-timeout-seconds 180)
             (not (e-openai--custom-override-p
                   'e-openai-request-timeout-seconds)))
    (setq e-openai-request-timeout-seconds nil)))

(e-openai--migrate-http-timeout-default)
(e-openai--migrate-websocket-idle-timeout-default)

(defun e-openai--plist-without (plist key)
  "Return PLIST without KEY."
  (let (result)
    (while plist
      (let ((current-key (pop plist))
            (value (pop plist)))
        (unless (eq current-key key)
          (setq result (append result (list current-key value))))))
    result))

(defun e-openai--diagnostic-byte-prefix (text max-bytes)
  "Return a prefix of TEXT no longer than MAX-BYTES."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (<= (+ bytes
                       (string-bytes (substring text index (1+ index))))
                    max-bytes))
      (setq bytes (+ bytes (string-bytes (substring text index (1+ index)))))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-openai--bounded-diagnostic-text (text)
  "Return TEXT bounded for provider diagnostics."
  (let* ((text (or text ""))
         (max-bytes (max 0 e-openai-diagnostic-string-max-bytes))
         (original-bytes (string-bytes text)))
    (if (<= original-bytes max-bytes)
        text
      (let* ((preview (e-openai--diagnostic-byte-prefix text max-bytes))
             (shown-bytes (string-bytes preview)))
        (format
         "%s\n[OpenAI diagnostic string truncated: showing first %d of %d bytes]"
         preview shown-bytes original-bytes)))))

(defun e-openai--diagnostic-preview-value (value depth seen)
  "Return a bounded preview copy of VALUE for provider diagnostics.
DEPTH limits recursive descent.  SEEN tracks container identity."
  (cond
   ((stringp value)
    (e-openai--bounded-diagnostic-text value))
   ((or (not value) (symbolp value) (numberp value) (characterp value))
    value)
   ((<= depth 0)
    '...)
   ((or (consp value) (vectorp value) (hash-table-p value))
    (if (gethash value seen)
        "#<cycle>"
      (puthash value t seen)
      (cond
       ((consp value)
        (let ((tail value)
              (items nil)
              (count 0)
              (limit (max 0 e-openai-diagnostic-print-length)))
          (while (and (consp tail) (< count limit))
            (push (e-openai--diagnostic-preview-value
                   (car tail) (1- depth) seen)
                  items)
            (setq tail (cdr tail))
            (setq count (1+ count)))
          (cond
           ((consp tail)
            (append (nreverse items) '(...)))
           ((null tail)
            (nreverse items))
           (t
            (append (nreverse items)
                    (list :dotted-tail
                          (e-openai--diagnostic-preview-value
                           tail (1- depth) seen)))))))
       ((vectorp value)
        (let* ((limit (max 0 e-openai-diagnostic-print-length))
               (count (min (length value) limit))
               (items nil))
          (dotimes (index count)
            (push (e-openai--diagnostic-preview-value
                   (aref value index) (1- depth) seen)
                  items))
          (apply #'vector
                 (nreverse
                  (if (< count (length value))
                      (cons '... items)
                    items)))))
       ((hash-table-p value)
        (let ((pairs nil)
              (count 0)
              (limit (max 0 e-openai-diagnostic-print-length))
              (truncated nil))
          (catch 'done
            (maphash
             (lambda (key entry)
               (if (>= count limit)
                   (progn
                     (setq truncated t)
                     (throw 'done nil))
                 (push
                  (cons
                   (e-openai--diagnostic-preview-value key (1- depth) seen)
                   (e-openai--diagnostic-preview-value entry (1- depth) seen))
                  pairs)
                 (setq count (1+ count))))
             value))
          (list :hash-table-preview (nreverse pairs)
                :truncated truncated
                :test (hash-table-test value)))))))
   (t value)))

(defun e-openai--truncate-diagnostic-string (text)
  "Return TEXT capped to `e-openai-diagnostic-result-max-bytes'."
  (let* ((max-bytes (max 0 e-openai-diagnostic-result-max-bytes))
         (original-bytes (string-bytes text)))
    (if (<= original-bytes max-bytes)
        text
      (let* ((preview (e-openai--diagnostic-byte-prefix text max-bytes))
             (shown-bytes (string-bytes preview)))
        (format
         "%s\n\n[OpenAI diagnostic truncated: showing first %d of %d bytes]"
         preview shown-bytes original-bytes)))))

(defun e-openai--bounded-diagnostic-string (value)
  "Return a bounded printed representation of VALUE for provider diagnostics."
  (let* ((preview
          (e-openai--diagnostic-preview-value
           value
           (max 0 e-openai-diagnostic-print-level)
           (make-hash-table :test 'eq)))
         (print-length (max 0 e-openai-diagnostic-print-length))
         (print-level (max 0 e-openai-diagnostic-print-level)))
    (e-openai--truncate-diagnostic-string
     (prin1-to-string preview))))

(defun e-openai--builtin-codex-profile-p (provider-id profile)
  "Return non-nil when PROFILE describes ChatGPT's built-in Codex endpoint."
  (and (eq provider-id 'codex)
       (equal (plist-get profile :name) "ChatGPT Codex")
       (equal (plist-get profile :base-url)
              (concat e-openai-codex-default-base-url "/codex"))
       (eq (plist-get profile :wire-api) 'responses)
       (eq (plist-get profile :responses-transport) 'websocket)
       (plist-get profile :requires-openai-auth)))

(defconst e-openai--request-local-observation-delivery-map
  '((:kind current-state :mode request-local-replaceable)
    (:kind dynamic-context :mode request-local-replaceable)
    (:kind tool-result :mode inherited)
    (:kind trace :mode inherited)
    (:kind retrieved-excerpt :mode inherited))
  "Kind-scoped delivery map for statically proven identity-compatible
Responses profiles.")

(defun e-openai--builtin-openai-profile ()
  "Return the canonical first-party OpenAI API provider profile."
  (list :name "OpenAI API"
        :base-url e-openai-api-default-base-url
        :wire-api 'responses
        :responses-transport 'websocket
        :response-store :json-false
        :responses-context-layout 'developer-input
        :prompt-cache-breakpoint-mode 'explicit
        ;; The canonical layout keeps the changing observation frontier in
        ;; late developer-role input.  It is therefore inherited by a
        ;; continuation; only profiles that deliberately retain the complete
        ;; observation in top-level `instructions' may opt into the older
        ;; request-local replacement contract.
        :observation-delivery 'inherited
        :include-encrypted-reasoning t
        :continuation t
        :requires-openai-auth nil
        :env-key "OPENAI_API_KEY"
        :default-model "gpt-5.6"))

(defun e-openai--profile-websocket-idle-close-seconds (profile)
  "Return the effective WebSocket idle-close policy for PROFILE.
An explicitly declared profile value must be a non-negative number.  Profiles
without the adapter-private field retain the existing global fallback,
including its nil no-timer behavior."
  (if (plist-member profile :websocket-idle-close-seconds)
      (let ((value (plist-get profile :websocket-idle-close-seconds)))
        (unless (and (numberp value) (>= value 0))
          (signal 'e-openai-provider-invalid
                  (list
                   (format
                    "Invalid :websocket-idle-close-seconds %S"
                    value))))
        value)
    e-openai-websocket-connection-idle-seconds))

(defun e-openai--normalize-model-providers (providers)
  "Return PROVIDERS with current built-in OpenAI requirements applied."
  (let ((normalized
         (mapcar (lambda (entry)
                   (let ((provider-id (car entry))
                         (profile (cdr entry)))
                     (if (e-openai--builtin-codex-profile-p
                          provider-id
                          profile)
                         (let ((normalized
                                (plist-put
                                 (plist-put
                                  (plist-put
                                   (plist-put (copy-sequence profile)
                                              :response-store :json-false)
                                   :prompt-cache-breakpoint-mode nil)
                                  :responses-context-layout 'developer-input)
                                 :include-encrypted-reasoning t)))
                           (unless (plist-member normalized
                                                 :observation-delivery)
                             (setq normalized
                                   (plist-put
                                    normalized
                                    :observation-delivery
                                    'inherited)))
                           (cons provider-id normalized))
                       entry)))
                 providers)))
    (if (assq 'openai normalized)
        normalized
      (append normalized
              (list (cons 'openai (e-openai--builtin-openai-profile)))))))

(defcustom e-openai-model-providers
  `((codex
     :name "ChatGPT Codex"
     :base-url ,(concat e-openai-codex-default-base-url "/codex")
     :wire-api responses
     :responses-transport websocket
     :response-store :json-false
     :prompt-cache-breakpoint-mode nil
     :responses-context-layout developer-input
     :observation-delivery inherited
     :include-encrypted-reasoning t
     :continuation t
     :requires-openai-auth t)
    (openai
     :name "OpenAI API"
     :base-url ,e-openai-api-default-base-url
     :wire-api responses
     :responses-transport websocket
     :response-store :json-false
     :responses-context-layout developer-input
     :prompt-cache-breakpoint-mode explicit
     :observation-delivery inherited
     :include-encrypted-reasoning t
     :continuation t
     :requires-openai-auth nil
     :env-key "OPENAI_API_KEY"
     :default-model "gpt-5.6"))
  "OpenAI-like model provider profiles keyed by provider symbol.

Each profile is plist data.  `:wire-api' supports `responses' and
`chat-completion' (`chat-completions' is accepted as a compatibility alias).
Profiles with `:requires-openai-auth' non-nil use Codex-managed ChatGPT auth.
Profiles with `:requires-openai-auth' nil read a bearer token from `:env-key'.
Profiles can set `:prompt-cache-retention' to nil to suppress that request
field, or non-nil to force it on.  Responses profiles can set
`:responses-transport' to `http' or `websocket'.  WebSocket Responses requests
store responses by default; set `:response-store' to explicitly override that
value.  Responses profiles can opt into provider continuation anchors with
`:continuation' non-nil.  GPT-5.6 Responses profiles set
`:prompt-cache-breakpoint-mode' to `explicit' when they accept explicit
breakpoints and `prompt_cache_options', or leave it nil when unsupported.
Profiles can independently set `:responses-context-layout' to
`developer-input' when system segments should remain distinct even without an
explicit breakpoint.  Set `:include-encrypted-reasoning' when a provider
supports returning stateless reasoning items for complete replay.  A Responses
profile may set `:reasoning-summary' to the diagnostic value \"detailed\";
when absent, the adapter sends \"auto\".  The request-level option can
override the profile, but the field cannot be disabled.  A Responses
profile may set `:observation-delivery' to `request-local-replaceable' only
when that profile and transport have proven that top-level `instructions' are
not inherited by its continuation response; otherwise the conservative
default is `inherited'.  Responses WebSocket profiles may set the adapter-
private `:websocket-idle-close-seconds' to a non-negative number; when absent,
the existing `e-openai-websocket-connection-idle-seconds' fallback applies."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'e-openai)

(setq e-openai-model-providers
      (e-openai--normalize-model-providers e-openai-model-providers))

(defcustom e-openai-codex-debug nil
  "When non-nil, retain the last raw Codex response and event summaries."
  :type 'boolean
  :group 'e-openai)

(defcustom e-openai-codex-raw-responses-max-bytes (* 256 1024)
  "Maximum raw provider diagnostic payload retained in bytes.
The bound applies to both `e-openai-codex--last-diagnostics' and the hidden
raw response buffer.  Set this to zero to keep event summaries without raw
provider payloads."
  :type '(integer :tag "Bytes")
  :group 'e-openai)

(defcustom e-openai-codex-raw-responses-buffer-name
  " *e-openai-codex-raw-responses*"
  "Hidden buffer used to retain raw Codex provider responses."
  :type 'string
  :group 'e-openai)

(defvar e-openai-codex--last-diagnostics nil
  "Last OpenAI/Codex diagnostics captured when debug mode is enabled.")

(defun e-openai-provider-profile (&optional provider)
  "Return configured profile for PROVIDER.
When PROVIDER is nil, use `e-openai-default-provider'."
  (setq e-openai-model-providers
        (e-openai--normalize-model-providers e-openai-model-providers))
  (let* ((provider (or provider e-openai-default-provider))
         (entry (assq provider e-openai-model-providers)))
    (unless entry
      (signal 'e-openai-provider-invalid
              (list (format "Unknown provider profile: %S" provider))))
    (let ((profile (cdr entry)))
      (unless (and (listp profile)
                   (memq (plist-get profile :wire-api)
                         '(responses chat-completion chat-completions)))
        (signal 'e-openai-provider-invalid
                (list (format "Provider %S must use :wire-api responses or chat-completion"
                              provider))))
      profile)))

(defun e-openai-provider-name (&optional provider)
  "Return display name for PROVIDER."
  (or (plist-get (e-openai-provider-profile provider) :name)
      (symbol-name (or provider e-openai-default-provider))))

(defun e-openai--provider-base-url (profile)
  "Return PROFILE's base URL or signal a provider configuration error."
  (let ((base-url (plist-get profile :base-url)))
    (unless (and (stringp base-url) (not (string-empty-p base-url)))
      (signal 'e-openai-provider-invalid
              '("Provider profile is missing :base-url")))
    base-url))

(defun e-openai--provider-model (profile explicit-model)
  "Return model for PROFILE, preferring EXPLICIT-MODEL."
  (or explicit-model
      (plist-get profile :default-model)
      e-openai-default-model))

(defun e-openai--provider-wire-api (profile)
  "Return PROFILE's normalized wire API."
  (pcase (plist-get profile :wire-api)
    ((or 'nil 'responses) 'responses)
    ((or 'chat-completion 'chat-completions) 'chat-completion)
    (other (signal 'e-openai-provider-invalid
                   (list (format "Unsupported :wire-api %S" other))))))

(defun e-openai--profile-reasoning-summary (profile)
  "Return the validated Responses summary mode declared by PROFILE."
  (e-openai--validate-reasoning-summary
   (if (plist-member profile :reasoning-summary)
       (plist-get profile :reasoning-summary)
     e-openai-default-reasoning-summary)))

(defun e-openai--profile-responses-transport (profile)
  "Return normalized Responses transport for PROFILE."
  (let ((transport (or (plist-get profile :responses-transport) 'http)))
    (unless (memq transport '(http websocket))
      (signal 'e-openai-provider-invalid
              (list (format "Unsupported :responses-transport %S"
                            transport))))
    transport))

(defun e-openai--response-store-value (options)
  "Return Responses store value for OPTIONS."
  (cond
   ((plist-member options :response-store)
    (plist-get options :response-store))
   ((eq (plist-get options :responses-transport) 'websocket)
    t)
   ((plist-get options :provider-continuation)
    t)
   (t :json-false)))

(defun e-openai--implicit-websocket-store-p (options)
  "Return non-nil when OPTIONS use WebSocket's stored-response default."
  (and (eq (plist-get options :responses-transport) 'websocket)
       (not (eq (e-openai--response-store-value options) :json-false))))

(defun e-openai--websocket-request-p (options)
  "Return non-nil when OPTIONS describe a Responses WebSocket request."
  (eq (plist-get options :responses-transport) 'websocket))

(defun e-openai--gpt56-or-later-p (model)
  "Return non-nil when MODEL names GPT-5.6 or a later numbered GPT model."
  (and (stringp model)
       (string-match "\\`gpt-\\([0-9]+\\)\\.\\([0-9]+\\)" model)
       (let ((major (string-to-number (match-string 1 model)))
             (minor (string-to-number (match-string 2 model))))
         (or (> major 5)
             (and (= major 5) (>= minor 6))))))

(defconst e-openai-gpt56-explicit-cache-layout-revision
  "responses-explicit-cache-v1"
  "Anchor revision for the GPT-5.6 explicit prompt-cache input layout.")

(defconst e-openai-gpt56-segmented-context-layout-revision
  "responses-segmented-context-v1"
  "Anchor revision for GPT-5.6 segmented input without explicit caching.")

(defun e-openai-codex--prompt-cache-breakpoint-mode (options)
  "Return the provider-supported GPT-5.6 breakpoint mode from OPTIONS.
Direct request-renderer callers default to `explicit' for compatibility; real
provider requests always materialize this option from the provider profile."
  (if (plist-member options :prompt-cache-breakpoint-mode)
      (plist-get options :prompt-cache-breakpoint-mode)
    'explicit))

(defun e-openai-codex--stable-cache-segment-p (segment)
  "Return non-nil when SEGMENT belongs to the stable prompt prefix."
  (memq (plist-get segment :kind) '(static-prefix stable-context)))

(defun e-openai-codex--stable-system-message-count (options)
  "Return the number of stable system messages described by OPTIONS."
  (cl-loop for segment in (plist-get options :segments)
           when (e-openai-codex--stable-cache-segment-p segment)
           sum (cl-count-if
                (lambda (message)
                  (eq (plist-get message :role) 'system))
                (plist-get segment :messages))))

(defun e-openai-codex--segmented-prompt-layout-p (options)
  "Return non-nil when OPTIONS use the semantic segmented input layout.

This is a wire-layout decision, not a cache-breakpoint decision.  The
developer-input profile declaration keeps stable and changing system messages
in input even when no prompt-cache key is available when the profile declares
the inherited delivery contract.  An explicit GPT-5.6 breakpoint declaration
with a key retains the direct renderer's segmented shape for legacy/direct
callers."
  (or (and (eq (e-openai-codex--observation-delivery options)
              'inherited)
           (eq (plist-get options :responses-context-layout)
               'developer-input))
      (and (e-openai--gpt56-or-later-p (plist-get options :model))
           (eq (e-openai-codex--prompt-cache-breakpoint-mode options)
               'explicit)
           (let ((key (plist-get options :prompt-cache-key)))
             (and (stringp key)
                  (not (string-empty-p key)))))))

(defun e-openai-codex--wire-prompt-layout-revision (options)
  "Return the provider wire-layout revision for OPTIONS, or nil.

This is deliberately separate from the material continuation identity below:
the curation revision must fence anchors even when no prompt-cache key exists,
while breakpoint emission remains key-dependent."
  (let ((key (plist-get options :prompt-cache-key))
        (mode (e-openai-codex--prompt-cache-breakpoint-mode options)))
    (when (and (e-openai--gpt56-or-later-p
                (plist-get options :model))
               (e-openai-codex--segmented-prompt-layout-p options)
               (stringp key)
               (not (string-empty-p key))
               (> (e-openai-codex--stable-system-message-count options) 0))
      (if (eq mode 'explicit)
          e-openai-gpt56-explicit-cache-layout-revision
        e-openai-gpt56-segmented-context-layout-revision))))

(defun e-openai-codex--prompt-layout-revision (options)
  "Return the material prompt-layout identity for OPTIONS, or nil.

The reserved curation carrier adds the complete provider-neutral curation
revision identity.  Per-frame labels, estimates, values, and provenance are
not options and therefore cannot enter this identity."
  (let ((wire-revision
         (e-openai-codex--wire-prompt-layout-revision options)))
    (if (eq (plist-get options :reserved-effect-carrier)
            'context-curate-wire)
        (list :prompt-layout-revision wire-revision
              :context-curation-revision-identity
              (e-context-lifetime-curation-revision-identity))
      wire-revision)))

(defun e-openai-codex--reasoning-identity (options)
  "Return the effective Responses reasoning identity for OPTIONS."
  (let ((reasoning (e-openai--effective-reasoning options)))
    (list :effort (plist-get reasoning :effort)
          :summary (plist-get reasoning :summary))))

(defun e-openai-codex--prompt-cache-mode-label (options)
  "Return the diagnostic cache mode label for segmented OPTIONS."
  (if (eq (e-openai-codex--prompt-cache-breakpoint-mode options) 'explicit)
      "explicit"
    "implicit-segmented"))

(defun e-openai--profile-prompt-cache-retention-supported-p (profile model)
  "Return non-nil when PROFILE and MODEL accept `prompt_cache_retention'.
GPT-5.6 and later use `prompt_cache_options.ttl' instead; their only current
TTL is the default, so the legacy retention option must not reach the wire."
  (and (not (e-openai--gpt56-or-later-p model))
       (if (plist-member profile :prompt-cache-retention)
           (plist-get profile :prompt-cache-retention)
         (not (plist-get profile :requires-openai-auth)))))

(defun e-openai--profile-continuation-supported-p (profile)
  "Return non-nil when PROFILE should use Responses continuation anchors."
  (and (eq (e-openai--provider-wire-api profile) 'responses)
       (plist-get profile :continuation)))

(defun e-openai--profile-continuation-mode (profile)
  "Return the proven continuation mode declared by PROFILE.

Boolean continuation declarations retain the historical linear mode.  A
named profile may explicitly declare `branchable' when it has independently
proved that inherited observations can branch from a clean anchor."
  (when (e-openai--profile-continuation-supported-p profile)
    (if (eq (plist-get profile :continuation) 'branchable)
        'branchable
      'linear)))

(defun e-openai--profile-observation-delivery (profile)
  "Return the validated semantic observation delivery for PROFILE.
An absent profile field is the conservative inherited default.  An explicitly
unknown value is a provider configuration error rather than an implicit
fallback, because selecting replacement changes continuation safety."
  (if (not (plist-member profile :observation-delivery))
      'inherited
    (let ((delivery (plist-get profile :observation-delivery)))
      (unless (memq delivery '(inherited request-local-replaceable))
        (signal 'e-openai-provider-invalid
                (list (format "Unsupported :observation-delivery %S"
                              delivery))))
      delivery)))

(defun e-openai--context-base-url-equal-p (left right)
  "Return non-nil when provider base URLs LEFT and RIGHT identify one endpoint."
  (and (stringp left)
       (stringp right)
       (equal (string-remove-suffix "/" left)
              (string-remove-suffix "/" right))))

(cl-defun e-openai--profile-context-capabilities
    (profile options &key provider base-url request-function
             provider-compaction-supported)
  "Return provider-neutral context capabilities for PROFILE and OPTIONS.

Only explicit profile evidence can select a request-local replaceable
observation channel.  The adapter owns this profile/transport decision; the
harness sees only the normalized semantic values.  BASE-URL and
REQUEST-FUNCTION describe the effective backend identity.  A built-in
first-party claim is not carried across an endpoint or transport override;
named custom profiles may retain their own explicit proof while using an
injected requester for conformance tests."
  (let* ((wire-api (e-openai--provider-wire-api profile))
         (declared-delivery (e-openai--profile-observation-delivery profile))
         (profile-base-url (plist-get profile :base-url))
         (profile-transport
          (and (eq wire-api 'responses)
               (e-openai--profile-responses-transport profile)))
         (requested-transport
          (and (plist-member options :responses-transport)
               (plist-get options :responses-transport)))
         (endpoint-compatible
          (or (null base-url)
              (e-openai--context-base-url-equal-p
               base-url profile-base-url)))
         (transport-compatible
          (or (null requested-transport)
              (eq requested-transport profile-transport)))
         (first-party-provider-p (memq provider '(codex openai)))
         ;; Built-in proof is tied to the first-party Responses endpoint and
         ;; native WebSocket requester.  A named custom profile may still use
         ;; an injected requester in its own conformance tests.
         (first-party-transport-compatible
          (or (not first-party-provider-p)
              (and (not request-function)
                   (eq profile-transport 'websocket))))
         (first-party-profile-compatible
          (pcase provider
            ('codex (e-openai--builtin-codex-profile-p provider profile))
            ('openai
             (e-openai--context-base-url-equal-p
              profile-base-url e-openai-api-default-base-url))
            (_ t)))
         (identity-compatible
          (and endpoint-compatible
               transport-compatible
               first-party-transport-compatible
               first-party-profile-compatible))
         (observation-delivery
          (if (and identity-compatible
                   (eq wire-api 'responses)
                   (eq declared-delivery 'request-local-replaceable))
              (copy-tree e-openai--request-local-observation-delivery-map)
            'inherited))
         (prefix-cache
          (cond
           ((and (eq wire-api 'responses)
                 (eq (plist-get profile :prompt-cache-breakpoint-mode)
                     'explicit))
            'explicit)
           ((plist-get options :prompt-cache-key) 'implicit)
           (t 'none))))
    (list :continuation
          (if identity-compatible
              (or (e-openai--profile-continuation-mode profile) 'none)
            'none)
          :observation-delivery observation-delivery
          :prefix-cache prefix-cache
          :provider-compaction
          (if provider-compaction-supported 'opaque 'none)
          :reasoning-state
          (if (plist-get profile :include-encrypted-reasoning)
              'replayable
            'none)
          :reserved-effect-carrier
          (if (eq wire-api 'responses)
              'context-curate-wire
            'none))))

(defun e-openai--harness-default-options (profile model)
  "Return backend-neutral harness options for PROFILE and MODEL."
  (let ((options (list :model model
                       :reasoning-effort e-openai-default-reasoning-effort)))
    (when (eq (e-openai--provider-wire-api profile) 'responses)
      (setq options
            (append options
                    (list :reasoning-summary
                          (e-openai--profile-reasoning-summary profile)))))
    (if (e-openai--profile-continuation-supported-p profile)
        (append options
                (list :provider-continuation t
                      :provider-anchor-provider-id 'openai))
      options)))

(defun e-openai--env-token (env-key)
  "Return bearer token from ENV-KEY or signal an auth error."
  (unless (and (stringp env-key) (not (string-empty-p env-key)))
    (signal 'e-openai-auth-invalid
            '("Token-auth provider is missing :env-key")))
  (let ((token (getenv env-key)))
    (unless (and (stringp token) (not (string-empty-p token)))
      (signal 'e-openai-auth-missing
              (list (format "Environment variable %s is missing" env-key))))
    token))

(defun e-openai-codex-last-diagnostics ()
  "Return the last captured OpenAI/Codex diagnostics.
When called interactively, display diagnostics in a temporary buffer.
Diagnostics are captured only when `e-openai-codex-debug' is non-nil."
  (interactive)
  (if (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*e-openai-codex-diagnostics*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (pp-to-string e-openai-codex--last-diagnostics))
          (goto-char (point-min))
          (special-mode))
        (display-buffer (current-buffer)))
    e-openai-codex--last-diagnostics))

(defun e-openai-codex--raw-response-tail (stream-text)
  "Return a UTF-8-safe bounded tail of STREAM-TEXT for debug diagnostics."
  (let* ((text (or stream-text ""))
         (limit (max 0 e-openai-codex-raw-responses-max-bytes)))
    (cond
     ((zerop limit) "")
     ((<= (string-bytes text) limit) text)
     (t
      ;; Work in UTF-8 bytes so the configured budget means the same thing for
      ;; ASCII and multibyte provider text.  Move right over continuation bytes
      ;; before decoding so the retained tail starts on a character boundary.
      (let* ((encoded (encode-coding-string text 'utf-8))
             (start (- (length encoded) limit)))
        (while (and (< start (length encoded))
                    (let ((byte (aref encoded start)))
                      (and (>= byte #x80) (<= byte #xBF))))
          (setq start (1+ start)))
        (decode-coding-string (substring encoded start) 'utf-8 t))))))

(defun e-openai-codex--trim-raw-response-buffer (buffer)
  "Trim BUFFER to `e-openai-codex-raw-responses-max-bytes'."
  (with-current-buffer buffer
    (let ((limit (max 0 e-openai-codex-raw-responses-max-bytes)))
      (when (> (string-bytes (buffer-string)) limit)
        (let ((tail (e-openai-codex--raw-response-tail (buffer-string))))
          (erase-buffer)
          (insert tail))))))

(defun e-openai-codex--append-raw-response (stream-text)
  "Append a bounded debug tail of STREAM-TEXT to the hidden response buffer."
  (when (and e-openai-codex-debug
             (not (string-empty-p (or stream-text "")))
             (> e-openai-codex-raw-responses-max-bytes 0))
    (with-current-buffer (get-buffer-create
                          e-openai-codex-raw-responses-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n"))
        (insert ";;; " (current-time-string) "\n")
        (insert (e-openai-codex--raw-response-tail stream-text))
        (unless (bolp)
          (insert "\n"))
        (e-openai-codex--trim-raw-response-buffer (current-buffer))))))

(defun e-openai-codex-auth-file (&optional codex-home)
  "Return the Codex auth file path for CODEX-HOME.
When CODEX-HOME is nil, use the CODEX_HOME environment variable or
`~/.codex'."
  (expand-file-name
   "auth.json"
   (or codex-home
       (getenv "CODEX_HOME")
       (expand-file-name "~/.codex"))))

(defun e-openai-codex-read-auth (&optional auth-file)
  "Read Codex auth from AUTH-FILE and return a plist."
  (let ((file (or auth-file (e-openai-codex-auth-file))))
    (unless (file-readable-p file)
      (signal 'e-openai-auth-missing (list file)))
    (json-parse-string (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))
                       :object-type 'plist
                       :array-type 'list
                       :null-object nil
                       :false-object :json-false)))

(defun e-openai-codex-auth-access-token (auth)
  "Return AUTH's access token."
  (or (plist-get (plist-get auth :tokens) :access_token)
      (plist-get auth :access_token)
      (signal 'e-openai-auth-invalid '("Missing access_token"))))

(defun e-openai-codex--base64url-decode (value)
  "Decode base64url VALUE."
  (let* ((normalized (replace-regexp-in-string "-" "+" value))
         (normalized (replace-regexp-in-string "_" "/" normalized))
         (padding (mod (- 4 (mod (length normalized) 4)) 4)))
    (base64-decode-string
     (concat normalized (make-string padding ?=)))))

(defun e-openai-codex--json-key (key)
  "Return plist keyword for JSON object KEY."
  (intern (concat ":" key)))

(defun e-openai-codex-auth-account-id (auth)
  "Extract the ChatGPT account id from AUTH's access token."
  (let* ((token (e-openai-codex-auth-access-token auth))
         (parts (split-string token "\\."))
         (payload (nth 1 parts)))
    (unless (= (length parts) 3)
      (signal 'e-openai-auth-invalid '("Access token is not a JWT")))
    (let* ((claims (json-parse-string
                    (e-openai-codex--base64url-decode payload)
                    :object-type 'plist
                    :array-type 'list
                    :null-object nil
                    :false-object :json-false))
           (account-claims (plist-get
                            claims
                            (e-openai-codex--json-key
                             e-openai-codex-account-claim)))
           (account-id (plist-get account-claims :chatgpt_account_id)))
      (or account-id
          (signal 'e-openai-auth-invalid
                  '("Missing ChatGPT account id claim"))))))

(defun e-openai-codex--message-content
    (role content &optional cache-breakpoint-p)
  "Return Responses content item for ROLE and CONTENT.
When CACHE-BREAKPOINT-P is non-nil, mark the input block as the end of the
explicitly cacheable stable prefix.

Provider-neutral context may carry a structured literal value (for example a
curation source presentation).  Responses text blocks are strings, so encode
such values as JSON at this wire boundary rather than using a Lisp printed
representation or dropping their structure."
  (let ((type (if (eq role 'assistant) "output_text" "input_text"))
        (text (cond
               ((null content) "")
               ((stringp content) content)
               (t (json-encode content)))))
    (vector
     (append
      (list :type type :text text)
      (when cache-breakpoint-p
        (list :prompt_cache_breakpoint (list :mode "explicit")))))))

(defun e-openai-codex--input-message
    (message &optional cache-breakpoint-p)
  "Map backend-neutral MESSAGE to a Responses input item.
CACHE-BREAKPOINT-P marks this message's content as the stable-prefix end."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('tool-call
       (list :type "function_call"
             :call_id (plist-get content :id)
             :name (plist-get content :name)
             :arguments (json-encode
                         (or (plist-get content :arguments)
                             (make-hash-table :test 'equal)))))
      ('tool
       (let ((result content))
         (list :type "function_call_output"
               :call_id (plist-get result :tool-call-id)
               :output (e-tools-result-content-text
                        (plist-get result :content)))))
      (_
       (list :type "message"
             :role (if (eq role 'system) "developer" (symbol-name role))
             :content (e-openai-codex--message-content
                       role content cache-breakpoint-p))))))

(defun e-openai-codex--input-replay-item (item)
  "Return an input-safe copy of opaque OpenAI replay ITEM.
OpenAI Responses output may represent an empty reasoning summary as JSON null,
while Responses input requires the field to contain an array."
  (let ((normalized (copy-tree item)))
    (when (and (member (plist-get normalized :type) '("reasoning" reasoning))
               (null (plist-get normalized :summary)))
      (setq normalized (plist-put normalized :summary [])))
    normalized))

(defun e-openai-codex--message-replay-items (message &optional immediate-followup-p)
  "Return input-safe OpenAI opaque replay items attached to MESSAGE.

When IMMEDIATE-FOLLOWUP-P is non-nil, omit replay records marked
`:full-replay-only'.  Such records are needed to reconstruct an unanchored
Responses request, but an anchored response already contains them."
  (let* ((role (plist-get message :role))
         (carrier (if (eq role 'tool-call)
                      (plist-get message :content)
                    (plist-get message :metadata)))
         (records (plist-get carrier :provider-replay-items)))
    (cl-loop for record in records
             when (and (member (plist-get record :provider-id)
                               '(openai "openai"))
                       (or (not immediate-followup-p)
                           (not (plist-get record :full-replay-only))))
             collect (e-openai-codex--input-replay-item
                      (plist-get record :item)))))

(defun e-openai-codex--system-message-p (message)
  "Return non-nil when MESSAGE is a backend-neutral system message."
  (eq (plist-get message :role) 'system))

(defun e-openai-codex--observation-delivery (options)
  "Return semantic observation delivery declared by OPTIONS."
  (or (plist-get options :observation-delivery)
      (plist-get (plist-get options :context-capabilities)
                 :observation-delivery)))

(defun e-openai-codex--replaceable-current-state-messages (options)
  "Return complete request-local current-state messages from OPTIONS.

The harness supplies the canonical frontier explicitly.  Direct adapter
callers may instead provide semantic segments, which keeps request-body tests
and other provider-neutral callers honest without making the adapter infer a
replaceable channel from a raw system-message role."
  (when (eq (e-openai-codex--observation-delivery options)
            'request-local-replaceable)
    (copy-tree
     (or (plist-get options :replaceable-current-state)
         (cl-loop for segment in (plist-get options :segments)
                  when (memq (plist-get segment :kind)
                             '(current-state dynamic-context))
                  append (copy-tree (plist-get segment :messages)))))))

(defun e-openai-codex--replaceable-current-state-content (options)
  "Return complete current-state value suitable for Responses instructions."
  (let ((messages (e-openai-codex--replaceable-current-state-messages options)))
    (when messages
      (string-join
       (delq nil
             (mapcar
              (lambda (message)
                (let ((content (plist-get message :content)))
                  (cond
                   ((stringp content) content)
                   ((null content) nil)
                   (t (json-encode content)))))
              messages))
       "\n\n"))))

(defun e-openai-codex--provider-compaction-stable-messages (options)
  "Return trusted stable semantic prefix messages from OPTIONS.

Opaque provider output replaces covered session history, but it does not carry
the harness capability prefix.  Only the named static/stable segments may be
resent here; deriving this from the full request would duplicate the portable
checkpoint represented by the opaque output."
  (cl-loop for segment in (plist-get options :segments)
           when (memq (plist-get segment :kind)
                      '(static-prefix stable-context))
           append (copy-tree (plist-get segment :messages))))

(defun e-openai-codex--remove-replaceable-current-state
    (messages options)
  "Return MESSAGES without the request-local current-state frontier.

The semantic segment layout, rather than structural message equality, identifies
which positions belong to the frontier.  This is important when a durable
history/delta message has the same role and content as the current observation.
Callers without segments cannot prove the partition and therefore signal an
explicit projection error instead of deleting an equal durable message by
guesswork or copying the frontier into explicit input.  The harness owns the
derived partition boundary; callers cannot forge an already-partitioned
escape."
  (let* ((replaceable-p
          (eq (e-openai-codex--observation-delivery options)
              'request-local-replaceable))
         (frontier-messages
          (e-openai-codex--replaceable-current-state-messages options))
         (segments (plist-get options :segments)))
    (cond
     ((not replaceable-p) messages)
     ((null frontier-messages) messages)
     ((null segments)
      (signal 'e-openai-context-projection-invalid
              '("A non-empty replaceable frontier has no segment partition")))
     (t
      (let ((index 0)
            (frontier-indices nil)
            (segment-message-count
             (plist-get options :context-segment-message-count))
            (segment-messages nil)
            (frontier-segment-messages nil))
        (dolist (segment segments)
          (dolist (message (plist-get segment :messages))
            (push message segment-messages)
            (when (memq (plist-get segment :kind)
                        '(current-state dynamic-context))
              (push index frontier-indices)
              (push message frontier-segment-messages))
            (setq index (1+ index))))
        (setq segment-messages (nreverse segment-messages)
              frontier-segment-messages (nreverse frontier-segment-messages))
        ;; The segment list is the sole origin evidence.  It may cover a
        ;; complete request or an exact prefix followed by later in-turn
        ;; messages, but its contents must be that exact prefix.  The reserved
        ;; count is checked when present, never used as an authority, and the
        ;; frontier value must agree with the frontier-bearing segments.  In
        ;; particular, a caller cannot forge a continuation-delta plist value
        ;; to turn an absent or mismatched partition into a safe projection.
        (unless (and (<= index (length messages))
                     (equal segment-messages
                            (cl-subseq messages 0 index))
                     (or (null segment-message-count)
                         (and (integerp segment-message-count)
                              (= index segment-message-count)))
                     (equal frontier-messages frontier-segment-messages))
          (signal 'e-openai-context-projection-invalid
                  '("Replaceable frontier segments do not cover the request prefix")))
        (cl-loop for message in messages
                 for message-index from 0
                 unless (memq message-index frontier-indices)
                 collect message))))))

(defun e-openai-codex--instructions (messages options)
  "Return top-level Codex instructions from MESSAGES and OPTIONS."
  (let* ((base (or (plist-get options :instructions)
                   "You are a helpful assistant."))
         (provider-compaction-p
          (plist-member options :provider-compaction-output))
         (current
          (e-openai-codex--replaceable-current-state-content options))
         (stable-messages
          (and provider-compaction-p
               (e-openai-codex--provider-compaction-stable-messages
                options)))
         (layout (e-openai-codex--segmented-prompt-layout-p options)))
    ;; Validate the semantic partition even when the segmented instruction
    ;; branch can otherwise derive stable system text without filtering input.
    ;; This prevents an absent/partial segment list from silently duplicating a
    ;; non-empty request-local frontier in explicit input.
    (when current
      (e-openai-codex--remove-replaceable-current-state messages options))
    (cond
     ;; In segmented Responses layouts stable system guidance remains a
     ;; developer-input prefix.  The current value is the only changing
     ;; request-local part and is resent in full here.
     ((and current layout)
      (string-join (delq nil (list base current)) "\n\n"))
     ;; Older/flattened Responses layouts have no separate stable developer
     ;; input.  Preserve their existing stable system guidance while replacing
     ;; the old current-state suffix with the complete current value.
     (current
      (let ((stable-messages
             (if provider-compaction-p
                 stable-messages
               (if (plist-get options :segments)
                 (cl-loop for segment in (plist-get options :segments)
                          unless (memq (plist-get segment :kind)
                                       '(current-state dynamic-context))
                          append (seq-filter
                                  #'e-openai-codex--system-message-p
                                  (plist-get segment :messages)))
               (seq-filter
                #'e-openai-codex--system-message-p
                (e-openai-codex--remove-replaceable-current-state
                 messages options))))))
        (string-join
         (delq nil
               (append
                (list base)
                (mapcar (lambda (message)
                          (plist-get message :content))
                        stable-messages)
                (list current)))
         "\n\n")))
     ;; A flattened provider-compaction request has no separate input channel
     ;; for stable capability context, so keep only the trusted stable prefix
     ;; in instructions.  Covered checkpoint/history messages stay exclusively
     ;; in opaque provider output.
     ((and provider-compaction-p (not layout))
      (string-join
       (delq nil
             (append (list base)
                     (mapcar (lambda (message)
                               (plist-get message :content))
                             stable-messages)))
       "\n\n"))
     (layout base)
     (t
      (string-join
       (delq nil
             (append (list base)
                     (mapcar (lambda (message) (plist-get message :content))
                             (seq-filter #'e-openai-codex--system-message-p
                                         messages))))
       "\n\n")))))

(defun e-openai-codex--continuation-response-id (options)
  "Return previous Responses id from OPTIONS when continuation is enabled."
  (when (and (not (plist-member options :provider-compaction-output))
             (plist-get options :provider-continuation)
             ;; WebSocket mode retains the latest response in connection-local
             ;; memory even with store=false.  HTTP continuation still needs a
             ;; stored response.
             (or (e-openai--websocket-request-p options)
                 (not (eq (e-openai--response-store-value options)
                          :json-false))))
    (let* ((anchor (plist-get options :provider-anchor))
           (metadata (plist-get anchor :metadata))
           (response-id (plist-get metadata :response-id)))
      (when (and (eq (plist-get anchor :provider-id) 'openai)
                 (stringp response-id)
                 (not (string-empty-p response-id))
                 (equal (plist-get metadata :prompt-layout-revision)
                        (e-openai-codex--prompt-layout-revision options))
                 ;; The effective reasoning pair is part of continuation
                 ;; safety.  An anchor without it is legacy/incomplete and
                 ;; must not authorize a material Responses continuation.
                 (and (plist-member metadata :reasoning-identity)
                      (equal (plist-get metadata :reasoning-identity)
                             (e-openai-codex--reasoning-identity options))))
        response-id))))

(defun e-openai-codex--move-inherited-frontier-to-end (messages options)
  "Move inherited observation segments after the canonical durable prefix.

The harness supplies semantic segments for a complete canonical request.  When
those segments cover MESSAGES exactly, current-state and dynamic-context
messages are emitted after every other message, preserving the order within
each group.  A canonical request may carry a harness-owned segment-message
count when same-turn messages follow that semantic prefix; that exact prefix
is accepted and the suffix is retained.  A continuation delta,
provider-compaction projection, or direct caller without segments is already
owned by another projection and is returned unchanged.  A canonical semantic
segment list that names a frontier but matches neither the full request nor
its explicitly counted prefix is invalid and signals instead of silently
guessing from message shape."
  (let ((segments (plist-get options :segments))
        (segment-message-count
         (plist-get options :context-segment-message-count))
        (continuation-delta-p
         (and (e-openai-codex--continuation-response-id options)
              (plist-member options :provider-anchor-delta-messages)
              (listp (plist-get options :provider-anchor-delta-messages))))
        (provider-compaction-p
         (plist-member options :provider-compaction-output)))
    (if (or (not (eq (e-openai-codex--observation-delivery options)
                     'inherited))
            (not (e-openai-codex--segmented-prompt-layout-p options))
            (null segments)
            continuation-delta-p
            provider-compaction-p)
        messages
      (let ((index 0)
            (frontier-indices nil)
            (segment-messages nil))
        (dolist (segment segments)
          (dolist (message (plist-get segment :messages))
            (push message segment-messages)
            (when (memq (plist-get segment :kind)
                        '(current-state dynamic-context))
              (push index frontier-indices))
            (setq index (1+ index))))
        (setq segment-messages (nreverse segment-messages)
              frontier-indices (nreverse frontier-indices))
        (cond
         ((null frontier-indices)
          messages)
         ((not (or (equal segment-messages messages)
                   (and (integerp segment-message-count)
                        (= index segment-message-count)
                        (<= index (length messages))
                        (equal segment-messages
                               (cl-subseq messages 0 index)))))
          (signal
           'e-openai-context-projection-invalid
           '("Inherited frontier segments do not cover the canonical request")))
         (t
          (append
           (cl-loop for message in messages
                    for message-index from 0
                    unless (memq message-index frontier-indices)
                    collect message)
           (mapcar (lambda (message-index)
                     (nth message-index messages))
                   frontier-indices))))))))

(defun e-openai-codex--request-input-messages (messages options)
  "Return Responses input messages from MESSAGES and OPTIONS."
  (let* ((provider-compaction-p
          (plist-member options :provider-compaction-output))
         (provider-compaction-output
          (and provider-compaction-p
               (plist-get options :provider-compaction-output)))
         (provider-compaction-delta
          (plist-get options :provider-compaction-delta-messages))
         (response-id (e-openai-codex--continuation-response-id options))
         (delta-messages (plist-get options :provider-anchor-delta-messages))
         (source-count
          (plist-get options :provider-anchor-source-message-count))
         (in-turn-messages
          (when (and (integerp source-count)
                     (>= source-count 0)
                     (<= source-count (length messages)))
            (nthcdr source-count messages)))
         (source (cond
                  (provider-compaction-p
                   (append (if (vectorp provider-compaction-output)
                               (append provider-compaction-output nil)
                             provider-compaction-output)
                           (mapcar #'e-openai-codex--input-message
                                   provider-compaction-delta)))
                  ((and response-id (listp delta-messages))
                     (append delta-messages in-turn-messages)
                   )
                  (t messages)))
         ;; A harness-built continuation delta is already partitioned at the
         ;; semantic boundary and contains no request-local replacement.  Do
         ;; not structurally re-filter it: equal durable deltas must survive.
         (source (if (or provider-compaction-p
                         (and response-id (listp delta-messages)))
                     source
                   (e-openai-codex--remove-replaceable-current-state
                    source options)))
         (source (e-openai-codex--move-inherited-frontier-to-end
                  source options)))
    (if (or provider-compaction-p
            (e-openai-codex--segmented-prompt-layout-p options))
        source
      (seq-remove #'e-openai-codex--system-message-p source))))

(defun e-openai-codex--input-items
    (messages options continuation-response-id)
  "Return Responses input items for MESSAGES under OPTIONS.
CONTINUATION-RESPONSE-ID suppresses a new explicit breakpoint because the
retained response already carries the stable segment and its earlier marker."
  (let ((stable-left
         (if (and (e-openai-codex--wire-prompt-layout-revision options)
                  (eq (e-openai-codex--prompt-cache-breakpoint-mode options)
                      'explicit)
                  (null continuation-response-id))
             (e-openai-codex--stable-system-message-count options)
           0))
        items)
    (dolist (message messages (vconcat (nreverse items)))
      (let ((breakpoint-p nil))
        (when (and (> stable-left 0)
                   (e-openai-codex--system-message-p message))
          (setq stable-left (1- stable-left))
          (setq breakpoint-p (= stable-left 0)))
        (let ((input-message
               (e-openai-codex--input-message message breakpoint-p)))
          (if (eq (plist-get message :role) 'tool)
              (progn
                ;; A tool result is the causal predecessor of replay items
                ;; attached by a later reserved curation effect.  Full replay
                ;; must therefore render the result before that call/output
                ;; pair.  Assistant and tool-call carriers retain the
                ;; established replay-before-carrier ordering.
                (push input-message items)
                (dolist (replay-item
                         (e-openai-codex--message-replay-items
                          message continuation-response-id))
                  (push replay-item items)))
            (dolist (replay-item
                     (e-openai-codex--message-replay-items
                      message continuation-response-id))
              (push replay-item items))
            (push input-message items)))))))

(defun e-openai-codex--request-input-items
    (messages options continuation-response-id)
  "Return provider input items, including opaque compact output when selected."
  (if (plist-member options :provider-compaction-output)
      (let* ((output (plist-get options :provider-compaction-output))
             (output (if (vectorp output) (append output nil) output))
             (stable-messages
              (if (e-openai-codex--segmented-prompt-layout-p options)
                  (e-openai-codex--provider-compaction-stable-messages
                   options)))
             (stable-items
              (if stable-messages
                  (append
                   (e-openai-codex--input-items
                    stable-messages options nil)
                   nil)))
             (delta (plist-get options :provider-compaction-delta-messages)))
        (vconcat (append output
                         stable-items
                         (mapcar #'e-openai-codex--input-message delta))))
    (e-openai-codex--input-items
     (e-openai-codex--request-input-messages messages options)
     options
     continuation-response-id)))

(defun e-openai-codex--without-provider-anchor (options)
  "Return OPTIONS without provider-anchor continuation state."
  (let ((options (copy-sequence options)))
    (dolist (key '(:provider-anchor
                   :provider-anchor-delta-messages
                   :provider-anchor-source-message-count))
      (cl-remf options key))
    options))

(defun e-openai-codex--context-curation-tool-definition ()
  "Return the wire carrier for the core-owned curation effect."
  (list :type "function"
        :name "context-curate"
        :description
        "For ephemeral context sources shown with labels, call this after using a source when it may be needed later: use keep with a label for exact retention or summaries with labels for a compact durable replacement; omitted sources are dropped after a successful call, and make no call when nothing should remain."
        :parameters
        (list :type "object"
              :additionalProperties :json-false
              :properties
              (list
               :keep
               (list :type "array" :maxItems 16
                     :items (list :type "integer" :minimum 1))
               :summaries
               (list :type "array" :maxItems 16
                     :items
                     (list :type "object"
                           :additionalProperties :json-false
                           :required ["sources" "text"]
                           :properties
                           (list :sources
                                 (list :type "array" :minItems 1 :maxItems 16
                                       :items
                                       (list :type "integer" :minimum 1))
                                 :text (list :type "string"
                                              :minLength 1))))))))

(defun e-openai-codex--text-verbosity (model options)
  "Return Responses text verbosity for MODEL under OPTIONS."
  (or (plist-get options :text-verbosity)
      (plist-get options :model-verbosity)
      (when (and (stringp model)
                 (string-prefix-p "gpt-5" model))
        e-openai-default-text-verbosity)))

(cl-defun e-openai-codex-request-body (&key messages options tools)
  "Build a Codex Responses request body from MESSAGES, OPTIONS, and TOOLS."
  (let* ((model (or (plist-get options :model)
                    e-openai-default-model))
         (text-verbosity (e-openai-codex--text-verbosity model options))
         (continuation-response-id
          (e-openai-codex--continuation-response-id options))
         (reasoning (e-openai--effective-reasoning options))
         (store-value (e-openai--response-store-value options))
         (body (append
                (list :model model)
                (unless (and (e-openai--implicit-websocket-store-p options)
                             (eq store-value t))
                  (list :store store-value))
                (unless (e-openai--websocket-request-p options)
                  (list :stream t))
                (list :instructions (e-openai-codex--instructions
                                     messages
                                     options)
                      :input (e-openai-codex--request-input-items
                              messages options continuation-response-id)
                      :tool_choice "auto"
                      :parallel_tool_calls t))))
    (when (or tools
              (eq (plist-get options :reserved-effect-carrier)
                  'context-curate-wire))
      (setq body
            (append body
                    (list :tools
                          (vconcat
                           (append (copy-tree tools)
                                   (when (eq (plist-get options
                                                       :reserved-effect-carrier)
                                             'context-curate-wire)
                                     (list
                                      (e-openai-codex--context-curation-tool-definition)))))))))
    (when text-verbosity
      (setq body (append body (list :text (list :verbosity text-verbosity)))))
    (when reasoning
      (setq body (append body (list :reasoning reasoning))))
    (when (plist-get options :include-encrypted-reasoning)
      (setq body
            (append body
                    (list :include ["reasoning.encrypted_content"]))))
    (when continuation-response-id
      (setq body
            (append body
                    (list :previous_response_id
                          continuation-response-id))))
    (when (plist-member options :prompt-cache-key)
      (setq body
            (append body
                    (list :prompt_cache_key
                          (plist-get options :prompt-cache-key)))))
    (when (and (e-openai-codex--wire-prompt-layout-revision options)
               (eq (e-openai-codex--prompt-cache-breakpoint-mode options)
                   'explicit))
      (setq body
            (append body
                    (list :prompt_cache_options (list :mode "explicit")))))
    (when (plist-member options :prompt-cache-retention)
      (setq body
            (append body
                    (list :prompt_cache_retention
                          (plist-get options :prompt-cache-retention)))))
    body))


(defun e-openai--request-metadata (wire-api options body)
  "Return sanitized request metadata for WIRE-API, OPTIONS, and BODY."
  (when (eq wire-api 'responses)
    (let* ((anchor (plist-get options :provider-anchor))
           (anchor-metadata (plist-get anchor :metadata))
           (response-id (plist-get anchor-metadata :response-id))
           (delta-messages (plist-get options :provider-anchor-delta-messages))
           (responses-transport
            (or (plist-get options :responses-transport) 'http))
           (continuation-state
            (cond
             ((not (plist-get options :provider-continuation)) 'disabled)
             ((plist-member body :previous_response_id) 'used)
             (t 'full)))
           (reasoning (plist-get body :reasoning))
           (diagnostics
            (list :model (plist-get body :model)
                  :reasoning-effort (plist-get reasoning :effort)
                  :reasoning-summary (plist-get reasoning :summary)
                  :response-store (e-openai--response-store-value options)
                  :prompt-cache-key-present
                  (not (null (plist-member body :prompt_cache_key)))
                  :prompt-cache-retention-present
                  (not (null (plist-member body :prompt_cache_retention)))
                  :provider-continuation continuation-state
                  :previous-response-id-present
                  (not (null (plist-member body :previous_response_id)))
                  :provider-anchor-present
                  (and (plist-get options :provider-anchor) t)
                  :provider-compaction-selected
                  (plist-member options :provider-compaction-output)
                  :provider-compaction-source-entry-id
                  (plist-get options :provider-compaction-source-entry-id)
                  :input-message-count (length (plist-get body :input))
                  ;; The reserved curation carrier is an adapter wire
                  ;; detail, not a user-dispatchable tool.  Keep diagnostics
                  ;; compatible with the ordinary tool count.
                  :tool-count
                  (cl-count-if
                   (lambda (tool)
                     (not (member (plist-get tool :name)
                                  '("context-curate" context-curate))))
                   (plist-get body :tools))
                  :responses-transport responses-transport))
           (metadata (list :provider-continuation continuation-state
                           :responses-transport responses-transport
                           :reasoning-identity
                           (list :effort (plist-get reasoning :effort)
                                 :summary (plist-get reasoning :summary))
                           :diagnostics diagnostics)))
      (when (or (plist-member options :observation-delivery)
                (plist-member options :context-capabilities)
                (plist-member options :replaceable-current-state)
                (plist-member options :current-state-fingerprint))
        (setq diagnostics
              (append diagnostics
                      (list :observation-delivery
                            (or (plist-get options :observation-delivery)
                                (plist-get
                                 (plist-get options :context-capabilities)
                                 :observation-delivery))
                            :replaceable-current-state-present
                            (and (e-openai-codex--replaceable-current-state-messages
                                  options)
                                 t)
                            :current-state-fingerprint
                            (plist-get options :current-state-fingerprint)
                            :context-rendering-strategy
                            (plist-get options :context-rendering-strategy)
                            :provider-anchor-safety
                            (plist-get options :provider-anchor-safety))))
        (setq metadata (plist-put metadata :diagnostics diagnostics)))
      (when-let ((revision (e-openai-codex--prompt-layout-revision options)))
        (setq diagnostics
              (append diagnostics
                      (list :prompt-cache-mode
                            (e-openai-codex--prompt-cache-mode-label options)
                            :prompt-layout-revision revision)))
        (setq metadata (plist-put metadata :diagnostics diagnostics))
        (setq metadata
              (append metadata
                      (list :openai-prompt-layout-revision revision))))
      (when (and (eq continuation-state 'used)
                 (stringp response-id))
        (setq metadata
              (append metadata
                      (list :provider-anchor-response-id response-id))))
      (when-let ((covered-entry-id (plist-get anchor :covered-entry-id)))
        (setq metadata
              (append metadata
                      (list :provider-anchor-covered-entry-id
                            covered-entry-id))))
      (when (listp delta-messages)
        (setq metadata
              (append metadata
                      (list :provider-continuation-delta-count
                            (length delta-messages)))))
      (when (integerp (plist-get options
                                 :provider-anchor-source-message-count))
        (setq metadata
              (append metadata
                      (list :provider-anchor-source-message-count
                            (plist-get
                             options
                             :provider-anchor-source-message-count)))))
      (when-let ((reason
                  (plist-get options :provider-anchor-invalidation-reason)))
        (setq metadata
              (append metadata
                      (list :provider-anchor-invalidation-reason reason))))
      metadata)))

(defun e-openai-chat-completion--message-content (content)
  "Return Chat Completions message content for CONTENT."
  (if (stringp content) content (or content "")))

(defun e-openai-chat-completion--tool-definition (tool)
  "Map backend-neutral TOOL to a Chat Completions tool definition."
  (let ((function (list :name (plist-get tool :name)
                        :description (plist-get tool :description)
                        :parameters (plist-get tool :parameters))))
    (when (plist-get tool :strict)
      (setq function (append function (list :strict (plist-get tool :strict)))))
    (list :type "function" :function function)))

(defun e-openai-chat-completion--tool-definitions (tools)
  "Map backend-neutral TOOLS to Chat Completions tool definitions."
  (vconcat (mapcar #'e-openai-chat-completion--tool-definition tools)))

(defun e-openai-chat-completion--message (message)
  "Map backend-neutral MESSAGE to a Chat Completions message."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('tool-call
       (let ((arguments (json-encode
                         (or (plist-get content :arguments)
                             (make-hash-table :test 'equal)))))
         (list :role "assistant"
               :content nil
               :tool_calls
               (vector
                (list :id (plist-get content :id)
                      :type "function"
                      :function (list :name (plist-get content :name)
                                      :arguments arguments))))))
      ('tool
       (let ((result content))
         (list :role "tool"
               :tool_call_id (plist-get result :tool-call-id)
               :content (e-tools-result-content-text
                         (plist-get result :content)))))
      (_
       (list :role (symbol-name role)
             :content (e-openai-chat-completion--message-content content))))))

(defun e-openai-chat-completion--messages (messages options)
  "Return Chat Completions messages from backend-neutral MESSAGES and OPTIONS."
  (let ((instructions (or (plist-get options :instructions)
                          "You are a helpful assistant.")))
    (vconcat
     (append
      (when (and (stringp instructions)
                 (not (string-empty-p instructions)))
        (list (list :role "system" :content instructions)))
      (mapcar #'e-openai-chat-completion--message messages)))))

(cl-defun e-openai-chat-completion-request-body (&key messages options tools)
  "Build a Chat Completions request body from MESSAGES, OPTIONS, and TOOLS."
  (let ((body (list :model (or (plist-get options :model)
                               e-openai-default-model)
                    :stream t
                    :messages (e-openai-chat-completion--messages
                               messages
                               options))))
    (when tools
      (setq body (append body
                         (list :tools
                               (e-openai-chat-completion--tool-definitions tools)
                               :tool_choice "auto"))))
    body))

(defun e-openai-responses-url (base-url)
  "Return the Responses endpoint URL for BASE-URL."
  (let ((normalized (string-remove-suffix "/" base-url)))
    (if (string-suffix-p "/responses" normalized)
        normalized
      (concat normalized "/responses"))))

(defun e-openai-responses-websocket-url (base-url)
  "Return the Responses WebSocket URL for BASE-URL."
  (let ((url (e-openai-responses-url base-url)))
    (cond
     ((string-prefix-p "https://" url)
      (concat "wss://" (substring url (length "https://"))))
     ((string-prefix-p "http://" url)
      (concat "ws://" (substring url (length "http://"))))
     ((or (string-prefix-p "wss://" url)
          (string-prefix-p "ws://" url))
      url)
     (t
      (signal 'e-openai-provider-invalid
              (list (format "Unsupported WebSocket base URL %S" base-url)))))))

(defun e-openai-chat-completion-url (base-url)
  "Return the Chat Completions endpoint URL for BASE-URL."
  (let ((normalized (string-remove-suffix "/" base-url)))
    (if (string-suffix-p "/chat/completions" normalized)
        normalized
      (concat normalized "/chat/completions"))))

(defun e-openai-codex-url (&optional base-url)
  "Return the Codex Responses URL for BASE-URL."
  (let ((normalized (string-remove-suffix
                     "/"
                     (or base-url e-openai-codex-default-base-url))))
    (cond
     ((string-suffix-p "/responses" normalized) normalized)
     ((string-suffix-p "/codex" normalized)
      (e-openai-responses-url normalized))
     (t (e-openai-responses-url (concat normalized "/codex"))))))

(defun e-openai-codex--headers (auth &optional session-id)
  "Return Codex request headers for AUTH and SESSION-ID."
  (let* ((token (e-openai-codex-auth-access-token auth))
         (headers `(("Authorization" . ,(concat "Bearer " token))
                    ("chatgpt-account-id" . ,(e-openai-codex-auth-account-id auth))
                    ("originator" . "e")
                    ("OpenAI-Beta" . "responses=experimental")
                    ("Accept" . "text/event-stream")
                    ("Content-Type" . "application/json"))))
    (if session-id
        (append headers
                `(("session_id" . ,session-id)
                  ("x-client-request-id" . ,session-id)))
      headers)))

(defun e-openai--token-headers (token)
  "Return standard Responses headers using bearer TOKEN."
  `(("Authorization" . ,(concat "Bearer " token))
    ("Accept" . "text/event-stream")
    ("Content-Type" . "application/json")))

(cl-defun e-openai--headers (&key profile auth-file session-id)
  "Return request headers for PROFILE.
AUTH-FILE and SESSION-ID are used only for Codex-managed OpenAI auth
profiles."
  (if (plist-get profile :requires-openai-auth)
      (e-openai-codex--headers (e-openai-codex-read-auth auth-file)
                               session-id)
    (e-openai--token-headers
     (e-openai--env-token (plist-get profile :env-key)))))

(defun e-openai--responses-websocket-headers (headers)
  "Return HEADERS adjusted for the Responses WebSocket handshake."
  (append (seq-remove (lambda (header)
                        (member (car header) '("Accept" "OpenAI-Beta")))
                      headers)
          '(("OpenAI-Beta" . "responses_websockets=2026-02-06"))))

(defun e-openai-codex--http-header-bytes (value)
  "Return VALUE as an ASCII byte string suitable for `url-request-extra-headers'."
  (encode-coding-string (format "%s" value) 'us-ascii))

(defun e-openai-codex--http-header-list (headers)
  "Return HEADERS with names and values normalized to byte strings."
  (mapcar (lambda (header)
            (cons (e-openai-codex--http-header-bytes (car header))
                  (e-openai-codex--http-header-bytes (cdr header))))
          headers))

(cl-defstruct
    (e-openai--http-response
     (:constructor e-openai--http-response-create))
  body
  status
  retry-after)

(cl-defun e-openai-codex--http-request (&key url headers body)
  "POST BODY to URL with HEADERS and return its complete response.
Successful responses are returned as body text.  HTTP errors carry their body,
status, and retry metadata in an `e-openai--http-response'."
  (e-openai--reject-sync-in-hot-path 'e-openai-codex--http-request)
  (let ((response nil)
        (failure nil)
        (done nil))
    (e-openai-codex--http-request-start
     :url url
     :headers headers
     :body body
     :on-complete (lambda (value)
                    (setq response value)
                    (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.01))
    (when failure
      (signal (car failure) (cdr failure)))
    response))

(defun e-openai-codex--http-response-text (buffer)
  "Return response body text from url.el BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n" nil 'move)
    (buffer-substring-no-properties (point) (point-max))))

(defun e-openai-codex--http-response-status (callback-status)
  "Return numeric HTTP status from the current buffer or CALLBACK-STATUS."
  (or (and (boundp 'url-http-response-status)
           (numberp url-http-response-status)
           url-http-response-status)
      (let* ((url-error (plist-get callback-status :error))
             (http-tail (and (listp url-error) (memq 'http url-error))))
        (and (numberp (cadr http-tail)) (cadr http-tail)))))

(defun e-openai-codex--http-response-header (name)
  "Return response header NAME from the current url.el buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (limit (or (and (re-search-forward "\r?\n\r?\n" nil t)
                          (match-beginning 0))
                     (point-max))))
      (goto-char (point-min))
      (when (re-search-forward
             (format "^%s:[ \t]*\\([^\r\n]*\\)" (regexp-quote name))
             limit t)
        (string-trim (match-string-no-properties 1))))))

(defun e-openai-codex--http-retry-after ()
  "Return a numeric Retry-After response delay from the current buffer."
  (when-let ((value (e-openai-codex--http-response-header "Retry-After")))
    (when (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value)
      (string-to-number value))))

(defun e-openai-codex--http-response (body callback-status)
  "Return BODY with HTTP metadata from CALLBACK-STATUS when material."
  (let ((status (e-openai-codex--http-response-status callback-status))
        (retry-after (e-openai-codex--http-retry-after)))
    (if (and (numberp status) (>= status 400))
        (e-openai--http-response-create
         :body body :status status :retry-after retry-after)
      body)))

(defun e-openai-codex--url-metadata (url)
  "Return sanitized diagnostic metadata for URL."
  (let* ((parsed (url-generic-parse-url url))
         (path (or (url-filename parsed) "/")))
    (when (string-match "\\`\\([^?#]*\\)" path)
      (setq path (match-string 1 path)))
    (when (string-empty-p path)
      (setq path "/"))
    (list :url-host (url-host parsed)
          :url-path path)))

(defun e-openai-codex--kill-request-buffer (buffer)
  "Cancel any live request process attached to BUFFER and kill BUFFER.
Real/pipe helper processes are force-killed; network processes are deleted.
The exit query is disabled and `kill-buffer-query-functions' is bound off so a
still-live process can never raise the blocking \"has a running process; kill
it?\" prompt that stalls a headless agent."
  (when (buffer-live-p buffer)
    (when-let ((process (get-buffer-process buffer)))
      (when (process-live-p process)
        (set-process-query-on-exit-flag process nil)
        (if (memq (process-type process) '(real pipe))
            (kill-process process)
          (delete-process process))))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(cl-defun e-openai-codex--http-request-start
    (&key url headers body on-complete on-error)
  "POST BODY to URL with HEADERS asynchronously.
ON-COMPLETE receives body text or a structured HTTP error response.  ON-ERROR
receives an Emacs condition list.  Return a cancellable `e-backend-request'
handle."
  (let ((url-request-method "POST")
        (url-request-extra-headers (e-openai-codex--http-header-list headers))
        (url-request-data (encode-coding-string body 'utf-8))
        (timeout e-openai-request-timeout-seconds)
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
           (e-openai-codex--kill-request-buffer buffer))
         (settle-timeout ()
           (unless settled
             (setq settled t)
             (cleanup request-buffer)
             (when on-error
               (funcall
                on-error
                (list 'e-openai-request-timeout
                      (format "OpenAI request timed out after %s seconds"
                              timeout))))))
         (rearm-timeout ()
           (when (and timeout (not settled))
             (cancel-timeout)
             (setq timeout-timer
                   (run-at-time timeout nil #'settle-timeout))))
         (track-response-progress (&rest _)
           ;; `url-retrieve' calls its completion callback only after the full
           ;; body arrives.  Its private response buffer changes on each
           ;; network chunk, which is the HTTP transport's idle-progress edge.
           (rearm-timeout))
         (handle-callback (status)
           (unless settled
             (setq settled t)
             (let ((buffer (current-buffer)))
               (unwind-protect
                   (condition-case err
                       (let* ((url-error (plist-get status :error))
                              (response-text
                               (e-openai-codex--http-response-text buffer))
                              (response
                               (e-openai-codex--http-response
                                response-text status)))
                         (if url-error
                             (if (or (e-openai--http-response-p response)
                                     (not (string-empty-p
                                           (string-trim response-text))))
                                   (when on-complete
                                     (funcall on-complete response))
                               (when on-error
                                 (funcall
                                  on-error
                                  (list 'error
                                        (e-format-safe
                                         "OpenAI request failed: %S"
                                         url-error)))))
                           (when on-complete
                             (funcall on-complete response))))
                     (error
                      (when on-error
                        (funcall on-error err))))
                 (cleanup buffer))))))
      (setq request-buffer
            (url-retrieve
             url
             (lambda (status)
               (handle-callback status))
             nil
             'silent
             nil))
      (when (buffer-live-p request-buffer)
        (with-current-buffer request-buffer
          (add-hook 'after-change-functions #'track-response-progress nil t)))
      (rearm-timeout))
    (e-backend-request-create
     :cancel (lambda ()
               (unless settled
                 (setq settled t))
               (when (timerp timeout-timer)
                 (cancel-timer timeout-timer))
               (setq timeout-timer nil)
               (e-openai-codex--kill-request-buffer request-buffer)
               t)
     :metadata (append
                (list :transport 'url-retrieve
                      :url url
                      :timeout-seconds timeout
                      :cancellable t)
                (e-openai-codex--url-metadata url)))))

(defun e-openai-codex--websocket-frame-text (frame)
  "Return text payload from WebSocket FRAME."
  (cond
   ((stringp frame) frame)
   ((websocket-frame-payload frame))
   (t "")))

(defun e-openai-codex--websocket-event-items
    (event emit-anchor &optional prompt-layout-revision reasoning-identity)
  "Return backend-neutral items for WebSocket EVENT.
When EMIT-ANCHOR is nil, completed response ids stay transport-local.
PROMPT-LAYOUT-REVISION and REASONING-IDENTITY are persisted with an emitted
continuation anchor."
  (let* ((completed-event-p
          (member (plist-get event :type)
                  '("response.completed" "response.done")))
         (response (plist-get event :response))
         (usage-item
          (when completed-event-p
            (e-openai-codex--usage-item
             (plist-get response :usage))))
         (anchor-candidate-item
          (when (and emit-anchor completed-event-p)
            (e-openai-codex--anchor-candidate-item
             response prompt-layout-revision reasoning-identity)))
         (event-item (e-openai-codex--event-item event)))
    (delq nil (list usage-item anchor-candidate-item event-item))))

(cl-defstruct
    (e-openai-codex--websocket-session
     (:constructor e-openai-codex--websocket-session-create))
  websocket
  url
  headers
  close-function
  connection-id
  reuse-count
  latest-response-id
  latest-response-properties
  active-request
  idle-timer)

(defvar e-openai-codex--websocket-connection-sequence 0
  "Process-local sequence for bounded WebSocket connection diagnostics.")

(define-error 'e-openai-websocket-busy
  "Responses WebSocket already has an active request")

(defun e-openai-codex--json-value-copy (value)
  "Return a detached copy of JSON-like VALUE.
Hash tables are copied by contents so a response's immediate continuation
snapshot does not share mutable request-property objects with a later request."
  (cond
   ((stringp value)
    (copy-sequence value))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash (lambda (key entry)
                 (puthash (e-openai-codex--json-value-copy key)
                          (e-openai-codex--json-value-copy entry)
                          copy))
               value)
      copy))
   ((vectorp value)
    (apply #'vector
           (mapcar #'e-openai-codex--json-value-copy value)))
   ((consp value)
    (cons (e-openai-codex--json-value-copy (car value))
          (e-openai-codex--json-value-copy (cdr value))))
   (t value)))

(defun e-openai-codex--websocket-session-clear-response-state (session)
  "Clear the immediate continuation state owned by SESSION."
  (setf (e-openai-codex--websocket-session-latest-response-id session) nil)
  (setf (e-openai-codex--websocket-session-latest-response-properties session)
        nil)
  session)

(defun e-openai-codex--websocket-session-record-response
    (session response-id properties)
  "Record only the latest completed RESPONSE-ID and PROPERTIES for SESSION.

The state is sufficient for the matching immediate function-call-output
continuation.  Older response identities are deliberately forgotten rather
than retained as a connection-local response graph."
  (e-openai-codex--websocket-session-clear-response-state session)
  (when (and (stringp response-id) (not (string-empty-p response-id)))
    (setf (e-openai-codex--websocket-session-latest-response-id session)
          response-id)
    (setf (e-openai-codex--websocket-session-latest-response-properties session)
          (e-openai-codex--json-value-copy properties)))
  session)

(defun e-openai-codex--websocket-cancel-idle-close (session)
  "Cancel SESSION's pending idle close timer."
  (when-let ((timer (e-openai-codex--websocket-session-idle-timer session)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (e-openai-codex--websocket-session-idle-timer session) nil)))

(defun e-openai-codex--websocket-session-close (session)
  "Close SESSION's connection and discard its warm continuation state."
  (e-openai-codex--websocket-cancel-idle-close session)
  (e-openai-codex--websocket-session-clear-response-state session)
  (let ((websocket (e-openai-codex--websocket-session-websocket session))
        (close-function
         (e-openai-codex--websocket-session-close-function session)))
    ;; Clear identity first so the library's synchronous on-close callback is
    ;; recognized as intentional cleanup rather than a transport failure.
    (setf (e-openai-codex--websocket-session-websocket session) nil)
    (setf (e-openai-codex--websocket-session-url session) nil)
    (setf (e-openai-codex--websocket-session-headers session) nil)
    (setf (e-openai-codex--websocket-session-close-function session) nil)
    (setf (e-openai-codex--websocket-session-reuse-count session) 0)
    (when (and websocket close-function)
      (funcall close-function websocket))))

(defun e-openai-codex--websocket-active-handler (session key &rest arguments)
  "Call KEY handler for SESSION's active request with ARGUMENTS."
  (when-let* ((active
               (e-openai-codex--websocket-session-active-request session))
              (handler (plist-get active key)))
    (apply handler arguments)))

(defun e-openai-codex--websocket-session-open (session url headers)
  "Open SESSION for URL and HEADERS and return the connection."
  (e-openai-codex--websocket-session-close session)
  (let* ((close-function (symbol-function 'websocket-close))
         (connection-id
          (format "e-ws-%d"
                  (cl-incf e-openai-codex--websocket-connection-sequence)))
         websocket)
    (setq websocket
          (websocket-open
           url
           :custom-header-alist headers
           :on-message
           (lambda (candidate frame)
             (when (eq candidate
                       (e-openai-codex--websocket-session-websocket session))
               (e-openai-codex--websocket-active-handler
                session :on-message candidate frame)))
           :on-close
           (lambda (candidate &rest _args)
             (when (eq candidate
                       (e-openai-codex--websocket-session-websocket session))
               (let ((active
                      (e-openai-codex--websocket-session-active-request
                       session)))
                 (e-openai-codex--websocket-cancel-idle-close session)
                 (setf (e-openai-codex--websocket-session-websocket session) nil)
                 (setf (e-openai-codex--websocket-session-url session) nil)
                 (setf (e-openai-codex--websocket-session-headers session) nil)
                 (setf (e-openai-codex--websocket-session-close-function
                        session)
                       nil)
                 (e-openai-codex--websocket-session-clear-response-state
                  session)
                 (setf (e-openai-codex--websocket-session-active-request session)
                       nil)
                 (when-let ((handler (plist-get active :on-close)))
                   (funcall handler candidate)))))
           :on-error
           (lambda (candidate &rest args)
             (when (eq candidate
                       (e-openai-codex--websocket-session-websocket session))
               (e-openai-codex--websocket-active-handler
                session :on-error candidate args)))))
    (setf (e-openai-codex--websocket-session-websocket session) websocket)
    (setf (e-openai-codex--websocket-session-url session) url)
    (setf (e-openai-codex--websocket-session-headers session)
          (copy-tree headers))
    (setf (e-openai-codex--websocket-session-close-function session)
          close-function)
    (setf (e-openai-codex--websocket-session-connection-id session)
          connection-id)
    (setf (e-openai-codex--websocket-session-reuse-count session) 0)
    websocket))

(defun e-openai-codex--websocket-schedule-idle-close
    (session idle-close-seconds)
  "Schedule SESSION's idle close using IDLE-CLOSE-SECONDS.
The effective policy is resolved by the request context before the WebSocket
request starts; this owner never reads the global fallback directly."
  (e-openai-codex--websocket-cancel-idle-close session)
  (when (and (numberp idle-close-seconds)
             (>= idle-close-seconds 0)
             (e-openai-codex--websocket-session-websocket session))
    (let ((connection-id
           (e-openai-codex--websocket-session-connection-id session)))
      (setf
       (e-openai-codex--websocket-session-idle-timer session)
       (run-at-time
        idle-close-seconds nil
        (lambda ()
          (setf (e-openai-codex--websocket-session-idle-timer session) nil)
          (when (and
                 (null (e-openai-codex--websocket-session-active-request session))
                 (equal connection-id
                        (e-openai-codex--websocket-session-connection-id
                         session)))
            (e-openai-codex--websocket-session-close session))))))))

(defun e-openai-codex--websocket-request-properties (body-data)
  "Return continuation-invariant request properties from BODY-DATA.
The Responses API replaces top-level `instructions' when a request supplies
`previous_response_id', so changed instructions do not invalidate the warm
response chain."
  (e-openai--plist-without
   (e-openai--plist-without
    (e-openai--plist-without body-data :input)
    :previous_response_id)
   :instructions))

(defun e-openai-codex--json-value-equal-p (first second)
  "Return non-nil when JSON-like values FIRST and SECOND are equivalent.
Hash tables are compared by contents rather than object identity.  Vector and
list order remains significant because those values encode JSON arrays and
ordered request plists in adapter-local request data."
  (cond
   ((and (hash-table-p first) (hash-table-p second))
    (and (= (hash-table-count first) (hash-table-count second))
         (let ((missing (make-symbol "missing"))
               (equivalent t))
           (maphash
            (lambda (key value)
              (let ((other (gethash key second missing)))
                (unless (and (not (eq other missing))
                             (e-openai-codex--json-value-equal-p value other))
                  (setq equivalent nil))))
            first)
           equivalent)))
   ((and (vectorp first) (vectorp second))
    (and (= (length first) (length second))
         (cl-loop for index below (length first)
                  always
                  (e-openai-codex--json-value-equal-p
                   (aref first index) (aref second index)))))
   ((and (consp first) (consp second))
    (and (e-openai-codex--json-value-equal-p (car first) (car second))
         (e-openai-codex--json-value-equal-p (cdr first) (cdr second))))
   (t (equal first second))))

(defun e-openai-codex--websocket-unresolved-response-p (event)
  "Return non-nil when EVENT rejects an unavailable previous response id."
  ;; The Responses WebSocket protocol has emitted this rejection both as a
  ;; response-scoped failure and as a top-level error event.  They have the
  ;; same causal meaning: the immediate connection-local response cannot be
  ;; continued, so the one bounded recovery is a complete canonical request.
  (when (member (plist-get event :type) '("response.failed" "error"))
    (let* ((response (plist-get event :response))
           (error (or (plist-get response :error)
                      (plist-get event :error)))
           (code (or (and (listp error) (plist-get error :code))
                     (plist-get event :code)))
           (param (or (and (listp error) (plist-get error :param))
                      (plist-get event :param)))
           (message (downcase
                     (or (and (listp error) (plist-get error :message))
                         (and (stringp error) error)
                         (plist-get event :message)
                         ""))))
      (or (equal param "previous_response_id")
          (member code '("previous_response_not_found"
                         "response_not_found"
                         "unknown_previous_response_id"))
          (and (string-match-p "previous.response" message)
               (string-match-p
                "not found\\|not cached\\|uncached\\|unknown\\|expired"
                message))))))

(defun e-openai-codex--websocket-actual-metadata
    (metadata body-data connection-id reused reuse-count mode idle-close-seconds)
  "Return METADATA updated for the actual WebSocket BODY-DATA sent.
CONNECTION-ID identifies the socket, REUSED and REUSE-COUNT describe its
lifecycle, MODE describes the selected request shape, and IDLE-CLOSE-SECONDS
is the request-resolved local policy."
  (let* ((metadata (copy-tree metadata))
         (diagnostics (copy-sequence (plist-get metadata :diagnostics)))
         (previous-present
          (not (null (plist-member body-data :previous_response_id))))
         (continuation
          (if previous-present
              'used
            (if (eq (plist-get metadata :provider-continuation) 'disabled)
                'disabled
              'full))))
    (setq diagnostics
          (plist-put diagnostics :provider-continuation continuation))
    (setq diagnostics
          (plist-put diagnostics :previous-response-id-present
                     previous-present))
    (setq diagnostics
          (plist-put diagnostics :input-message-count
                     (length (plist-get body-data :input))))
    (setq diagnostics
          (plist-put diagnostics :websocket-connection-id connection-id))
    (setq diagnostics
          (plist-put diagnostics :websocket-reused (and reused t)))
    (setq diagnostics
          (plist-put diagnostics :websocket-reuse-count reuse-count))
    (setq diagnostics
          (plist-put diagnostics :websocket-request-mode mode))
    (setq diagnostics
          (plist-put diagnostics :websocket-idle-close-seconds
                     idle-close-seconds))
    (setq metadata (plist-put metadata :provider-continuation continuation))
    (setq metadata (plist-put metadata :diagnostics diagnostics))
    (unless previous-present
      (cl-remf metadata :provider-anchor-response-id)
      (cl-remf metadata :provider-continuation-delta-count)
      (cl-remf metadata :provider-anchor-source-message-count))
    metadata))

(cl-defun e-openai-codex--websocket-request-start
    (&key session url headers body-data full-body-data request-metadata
          prompt-layout-revision reasoning-identity idle-close-seconds
          on-item on-complete on-error)
  "Send BODY-DATA as a Responses WebSocket request to URL with HEADERS.
SESSION owns a connection reusable by compatible sequential requests.
FULL-BODY-DATA is the safe request without provider continuation.
ON-ITEM receives backend-neutral stream items.  ON-COMPLETE receives a status
plist when a completed event arrives.  ON-ERROR receives an Emacs condition
list.  Return a cancellable `e-backend-request' handle."
  (let* ((session (or session
                      (e-openai-codex--websocket-session-create)))
         (timeout e-openai-websocket-idle-timeout-seconds)
         (full-body-data (or full-body-data body-data))
         (properties
          ;; Snapshot all continuation properties before the first send.  A
          ;; caller may reuse and mutate its request tree while the response
          ;; is still in flight; the later latest-response admission must compare the
          ;; request that was actually started, not that mutable tree.
          (e-openai-codex--json-value-copy
           (e-openai-codex--websocket-request-properties full-body-data)))
         (requested-response-id (plist-get body-data :previous_response_id))
         (existing-websocket
          (e-openai-codex--websocket-session-websocket session))
         (connection-compatible
          (and existing-websocket
               (equal url (e-openai-codex--websocket-session-url session))
               (equal headers
                      (e-openai-codex--websocket-session-headers session))))
         (response-known-p
          (and existing-websocket
               (stringp requested-response-id)
               (equal requested-response-id
                      (e-openai-codex--websocket-session-latest-response-id
                       session))))
         (properties-compatible
          (and response-known-p
               (e-openai-codex--json-value-equal-p
                properties
                (e-openai-codex--websocket-session-latest-response-properties
                 session))))
         (incremental-p
          (and connection-compatible response-known-p properties-compatible))
         (actual-body-data (if incremental-p body-data full-body-data))
         (reused connection-compatible)
        timeout-timer
        settled
        retried-full
        completed-response-id
        request
        request-token
        assistant-message-candidate
        assistant-message-seen)
    (when (e-openai-codex--websocket-session-active-request session)
      (signal 'e-openai-websocket-busy (list url)))
    (e-openai-codex--websocket-cancel-idle-close session)
    (unless connection-compatible
      (e-openai-codex--websocket-session-open session url headers)
      (setq reused nil))
    (when reused
      (cl-incf (e-openai-codex--websocket-session-reuse-count session)))
    (setq request-token (list :websocket-request))
    (cl-labels
        ((cancel-timeout ()
           (when (timerp timeout-timer)
             (cancel-timer timeout-timer))
           (setq timeout-timer nil))
         (arm-timeout ()
           (cancel-timeout)
           (when (and timeout (not settled))
             (setq timeout-timer
                   (run-at-time
                    timeout nil
                    (lambda ()
                      (settle-error
                       (list 'e-openai-request-timeout
                             (format "OpenAI WebSocket idle timed out after %s seconds"
                                     timeout))))))))
         (active-request-p ()
           (eq request-token
               (plist-get
                (e-openai-codex--websocket-session-active-request session)
                :token)))
         (clear-active-request ()
           (when (active-request-p)
             (setf (e-openai-codex--websocket-session-active-request session)
                   nil)))
         (refresh-request-metadata (mode)
           (let ((actual
                  (e-openai-codex--websocket-actual-metadata
                   request-metadata
                   actual-body-data
                   (e-openai-codex--websocket-session-connection-id session)
                   reused
                   (e-openai-codex--websocket-session-reuse-count session)
                   mode
                   idle-close-seconds)))
             (setf (e-backend-request-metadata request)
                   (append
                    (list :transport 'websocket
                          :url url
                          :timeout-seconds timeout
                          :cancellable t)
                    actual
                    (e-openai-codex--url-metadata url)))))
         (settle-error (err)
           (unless settled
             (setq settled t)
             (cancel-timeout)
             (clear-active-request)
             (e-openai-codex--websocket-session-close session)
             (when on-error
               (funcall on-error err))))
         (settle-complete (&optional response-id)
           (unless settled
             (setq settled t)
             (cancel-timeout)
             (clear-active-request)
             (when (stringp response-id)
               (e-openai-codex--websocket-session-record-response
                session response-id properties))
             (e-openai-codex--websocket-schedule-idle-close
              session idle-close-seconds)
             (when on-complete
               (funcall on-complete '(:status done)))))
         (settle-backend-error (item)
           ;; `response.failed' is a terminal Responses event.  Preserve its
           ;; backend-neutral error item for the loop/harness while releasing
           ;; the transport first, so a retry cannot collide with this request.
           (unless settled
             (setq settled t)
             (cancel-timeout)
             (clear-active-request)
             (e-openai-codex--websocket-session-close session)
             (when on-item
               (funcall on-item
                        (e-openai--normalize-backend-error-item item)))))
         (emit-item (item)
           (pcase (plist-get item :type)
             ('assistant-message
              (setq assistant-message-seen t)
              (when on-item
                (funcall on-item item)))
             ('assistant-message-candidate
              (unless assistant-message-candidate
                (setq assistant-message-candidate
                      (list :type 'assistant-message
                            :content (plist-get item :content)))))
             ('done
              (unless assistant-message-seen
                (when assistant-message-candidate
                  (setq assistant-message-seen t)
                  (when on-item
                    (funcall on-item assistant-message-candidate))))
              (when on-item
                (funcall on-item item)))
             ('backend-error
              (settle-backend-error item))
             (_
              (when on-item
                (funcall on-item item)))))
         (send-current-body ()
           (websocket-send-text
            (e-openai-codex--websocket-session-websocket session)
            (json-encode (append (list :type "response.create")
                                 actual-body-data))))
         (retry-full-request ()
           (setq retried-full t)
           ;; The provider rejected the only retained response identity.  Do
           ;; not preserve it while the complete canonical retry is in flight.
           (e-openai-codex--websocket-session-clear-response-state session)
           (setq actual-body-data full-body-data)
           (setq completed-response-id nil)
           (setq assistant-message-candidate nil)
           (setq assistant-message-seen nil)
           (refresh-request-metadata 'full-retry)
           (arm-timeout)
           (condition-case err
               (send-current-body)
             (error (settle-error err))))
         (handle-message (_websocket frame)
           (unless settled
             (condition-case err
                 (let* ((text (e-openai-codex--websocket-frame-text frame))
                        (event (e-openai-codex--parse-json text))
                        (completed-event-p
                         (member (plist-get event :type)
                                 '("response.completed" "response.done")))
                        (incomplete-event-p
                         (equal (plist-get event :type)
                                "response.incomplete"))
                        (terminal-event-p
                         (or completed-event-p incomplete-event-p))
                        ;; WebSocket responses are valid connection-local
                        ;; anchors even with store=false.  If the connection is
                        ;; later lost, previous_response_not_found already
                        ;; triggers a complete replay.
                        (emit-anchor t))
                   (arm-timeout)
                   (if (and incremental-p
                            (not retried-full)
                            (e-openai-codex--websocket-unresolved-response-p
                             event))
                       (retry-full-request)
                     (when completed-event-p
                       (setq completed-response-id
                             (plist-get (plist-get event :response) :id)))
                     (dolist (item (e-openai-codex--websocket-event-items
                                    event
                                    emit-anchor
                                    prompt-layout-revision
                                    reasoning-identity))
                       (emit-item item))
                     (when terminal-event-p
                       ;; An incomplete response is a successful terminal
                       ;; lifecycle event, but its response id is not a
                       ;; confirmed connection-local continuation anchor.
                       (settle-complete
                        (unless incomplete-event-p completed-response-id)))))
               (error
                (settle-error err)))))
         (handle-close (&rest _args)
           (unless settled
             (settle-error '(error "Responses WebSocket closed before completion"))))
         (handle-error (&rest args)
           (settle-error (list 'error
                               (format "Responses WebSocket error: %s"
                                       (e-openai--bounded-diagnostic-string
                                       args))))))
      (setq request
            (e-backend-request-create
             :cancel
             (lambda ()
               (unless settled
                 (setq settled t))
               (cancel-timeout)
               (clear-active-request)
               (e-openai-codex--websocket-session-close session)
               t)))
      (setf
       (e-openai-codex--websocket-session-active-request session)
       (list :token request-token
             :on-message #'handle-message
             :on-close #'handle-close
             :on-error #'handle-error))
      (refresh-request-metadata (if incremental-p 'incremental 'full))
      (arm-timeout)
      (condition-case err
          (send-current-body)
        (error
         (if reused
             (condition-case retry-error
                 (progn
                   (e-openai-codex--websocket-session-open session url headers)
                   (setq reused nil)
                   (setq actual-body-data full-body-data)
                   (refresh-request-metadata 'full-retry)
                   (send-current-body))
               (error (settle-error retry-error)))
           (settle-error err))))
      request)))

(defun e-openai-codex--parse-json (value)
  "Parse VALUE as JSON into plist data."
  (json-parse-string value
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun e-openai-codex--function-call-item-p (item)
  "Return non-nil when ITEM is a Responses function-call item."
  (and (listp item)
       (member (plist-get item :type)
               '("function_call" "tool_call" function_call tool_call))))

(defun e-openai-codex--context-curation-name-p (name)
  "Return non-nil when NAME is the reserved curation carrier."
  (member name '("context-curate" context-curate)))

(defun e-openai-codex--encrypted-reasoning-item-p (item)
  "Return non-nil when ITEM is replayable encrypted OpenAI reasoning."
  (and (listp item)
       (member (plist-get item :type) '("reasoning" reasoning))
       (stringp (plist-get item :encrypted_content))))

(defun e-openai-codex--parse-function-arguments (arguments)
  "Parse JSON ARGUMENTS from a Responses function call."
  (cond
   ((and (stringp arguments) (not (string-empty-p arguments)))
    (e-openai-codex--parse-json arguments))
   ((listp arguments) arguments)
   (t nil)))

(defun e-openai-codex--context-curation-effect (arguments &optional call-id)
  "Return the core-owned curation effect decoded from wire ARGUMENTS.

The wire object is deliberately passed through without adding frame or
provider identity.  Core binds its labels to the live frame at completion."
  (let* ((arguments (e-openai-codex--parse-function-arguments arguments))
         (effect (list :type 'context-curate
                       :arguments arguments)))
    ;; Responses requires a function_call_output for every function_call when
    ;; a subsequent request continues from its response id.  Keep that wire
    ;; acknowledgement opaque and paired with the in-memory response; the
    ;; core curation effect remains exact and provider-neutral, while the
    ;; session projection removes this replay metadata from later durable
    ;; context.
    (when (and (stringp call-id) (not (string-empty-p call-id)))
      (let ((output-replay
             (list :type 'provider-replay-item
                   :provider-id 'openai
                   :item (list :type "function_call_output"
                               :call_id call-id
                               :output "")))
            (call-replay
             (list :type 'provider-replay-item
                   :provider-id 'openai
                   ;; An anchored continuation already has this function call
                   ;; in the provider response.  It is needed only when the
                   ;; complete causal exchange is replayed statelessly.
                   :full-replay-only t
                   :item (list :type "function_call"
                               :call_id call-id
                               :name "context-curate"
                               :arguments
                               (json-encode
                                (or arguments
                                    (make-hash-table :test 'equal)))))))
        ;; Retain the singular output field for existing consumers while the
        ;; plural field carries the complete call/output pair for full replay.
        (setq effect
              (plist-put effect :provider-replay-item output-replay))
        (setq effect
              (plist-put effect :provider-replay-items
                         (list call-replay output-replay)))))
    effect))

(defun e-openai-codex--sequence-list (value)
  "Return VALUE as a list when it is a JSON array sequence."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun e-openai-codex--content-text (content)
  "Return concatenated output text from Responses CONTENT."
  (string-join
   (delq nil
         (mapcar
          (lambda (part)
            (pcase (plist-get part :type)
              ((or "output_text" "text") (plist-get part :text))
              ("refusal" (plist-get part :refusal))))
          (e-openai-codex--sequence-list content)))
   ""))

(defun e-openai-codex--message-item-text (item)
  "Return assistant text from a Responses message ITEM."
  (when (equal (plist-get item :type) "message")
    (let ((text (e-openai-codex--content-text (plist-get item :content))))
      (unless (string-empty-p text)
        text))))

(defun e-openai-codex--event-summary (event item)
  "Return a compact diagnostics summary for provider EVENT and parsed ITEM."
  (let ((provider-item (plist-get event :item))
        (provider-part (plist-get event :part)))
    (list :event-type (plist-get event :type)
          :item-type (or (plist-get provider-item :type)
                         (plist-get provider-part :type))
          :parsed-type (plist-get item :type))))

(defun e-openai-codex--response-error-message (event)
  "Return a readable error message for a Responses failure EVENT."
  (let* ((response (plist-get event :response))
         (error (or (plist-get response :error)
                    (plist-get event :error)))
         (code (or (and (listp error) (plist-get error :code))
                   (plist-get event :code)))
         (message
          (or (and (listp error) (plist-get error :message))
              (and (stringp error) error)
              (plist-get event :message))))
    (if (stringp message)
        (let ((message (e-openai--bounded-diagnostic-text message)))
          (if (stringp code)
              (format "%s: %s" code message)
            message))
      (e-openai--bounded-diagnostic-string event))))

(defun e-openai-codex--number-or-nil (value)
  "Return VALUE when it is numeric, otherwise nil."
  (when (numberp value)
    value))

(defun e-openai-codex--usage-item (usage)
  "Return provider-neutral token usage item for Responses USAGE."
  (when (consp usage)
    (let* ((input-details (plist-get usage :input_tokens_details))
           (output-details (plist-get usage :output_tokens_details))
           (normalized
            (list
             :input-tokens
             (e-openai-codex--number-or-nil
              (plist-get usage :input_tokens))
             :cached-input-tokens
             (e-openai-codex--number-or-nil
              (plist-get input-details :cached_tokens))
             :cache-creation-input-tokens
             (e-openai-codex--number-or-nil
              (plist-get input-details :cache_write_tokens))
             :output-tokens
             (e-openai-codex--number-or-nil
              (plist-get usage :output_tokens))
             :reasoning-output-tokens
             (e-openai-codex--number-or-nil
              (plist-get output-details :reasoning_tokens))
             :total-tokens
             (e-openai-codex--number-or-nil
              (plist-get usage :total_tokens)))))
      (list :type 'token-usage :usage normalized))))

(defun e-openai-codex--anchor-candidate-item
    (response &optional prompt-layout-revision reasoning-identity)
  "Return provider anchor candidate item from completed RESPONSE.
PROMPT-LAYOUT-REVISION records the request layout carried by the response;
REASONING-IDENTITY fences the effective effort and summary pair."
  (when-let ((response-id (and (consp response)
                               (plist-get response :id))))
    (when (stringp response-id)
      (list :type 'provider-anchor-candidate
            :provider-id 'openai
            :metadata
            (append
             (list :response-id response-id)
             (when prompt-layout-revision
               (list :prompt-layout-revision prompt-layout-revision))
             (when reasoning-identity
               (list :reasoning-identity reasoning-identity)))))))

(defun e-openai-codex--json-error-item (stream-text)
  "Return a backend error item when STREAM-TEXT is a JSON error response."
  (when (string-prefix-p "{" (string-trim-left stream-text))
    (let* ((payload (e-openai-codex--parse-json stream-text))
           (error (plist-get payload :error))
           (detail (plist-get payload :detail))
           (code (and (listp error)
                      (or (plist-get error :code)
                          (plist-get error :type))))
           (message (cond
                     ((listp error) (plist-get error :message))
                     ((stringp error) error)
                     ((stringp detail) detail))))
      (when message
        (list :type 'backend-error
              :content (if (stringp code)
                           (format "%s: %s" code message)
                         message)
              :payload payload)))))

(defun e-openai-codex--text-preview (text &optional limit)
  "Return a compact single-line preview of TEXT.
LIMIT defaults to 240 characters."
  (let* ((limit (or limit 240))
         (preview (string-trim
                   (replace-regexp-in-string
                    "[[:space:]\n\r\t]+" " " (or text "")))))
    (if (> (length preview) limit)
        (concat (substring preview 0 limit) "...")
      preview)))

(defun e-openai-codex--html-text-preview (html &optional limit)
  "Return a compact text preview for HTML.
LIMIT defaults to 240 characters."
  (e-openai-codex--text-preview
   (replace-regexp-in-string "<[^>]+>" " " (or html ""))
   limit))

(defun e-openai-codex--non-stream-error-item (stream-text &optional wire-api)
  "Return a backend error item for non-empty non-SSE STREAM-TEXT.
WIRE-API identifies the expected OpenAI streaming protocol."
  (let* ((raw (or stream-text ""))
         (trimmed (string-trim-left raw))
         (html (string-prefix-p "<" trimmed))
         (preview (if html
                      (e-openai-codex--html-text-preview stream-text)
                    (e-openai-codex--text-preview stream-text)))
         (kind (if html 'html 'text)))
    (unless (string-empty-p (string-trim raw))
      (list :type 'backend-error
            :content (format "Provider returned %s instead of a %s stream: %s"
                             (if html "HTML" "non-stream text")
                             (if (eq wire-api 'chat-completion)
                                 "Chat Completions"
                               "Responses")
                             (if (string-empty-p preview)
                                 "(no text content)"
                               preview))
            :payload (list :response-kind kind
                           :preview preview)))))

(defun e-openai--sse-response-p (text)
  "Return non-nil when TEXT contains at least one SSE data field."
  (and (stringp text)
       (string-match-p "\\(?:\\`\\|\n\\)data:" text)))

(defun e-openai--sse-chunks (text)
  "Return SSE event chunks from TEXT with LF or CRLF framing."
  (split-string text "\r?\n\r?\n" t))

(defun e-openai--sse-lines (chunk)
  "Return lines from SSE CHUNK with LF or CRLF framing."
  (split-string chunk "\r?\n"))

(defun e-openai-codex--incomplete-reason (event)
  "Return the backend-neutral terminal reason for incomplete EVENT."
  (let ((reason (plist-get
                 (plist-get (plist-get event :response) :incomplete_details)
                 :reason)))
    (cond
     ((member reason '("max_output_tokens" "max_tokens")) 'length)
     ((equal reason "content_filter") 'content-filter)
     ((stringp reason)
      (intern (replace-regexp-in-string "_" "-" reason)))
     (t 'incomplete))))

(defun e-openai-codex--event-item (event)
  "Map parsed Responses EVENT to one backend-neutral item, or nil."
  (let ((type (plist-get event :type)))
    (cond
     ((equal type "response.output_text.delta")
      (list :type 'assistant-delta
            :content (plist-get event :delta)))
     ((equal type "response.output_text.done")
      (list :type 'assistant-message
            :content (plist-get event :text)))
     ((equal type "response.refusal.delta")
      (list :type 'assistant-delta
            :content (plist-get event :delta)))
     ((equal type "response.refusal.done")
      (list :type 'assistant-message
            :content (plist-get event :refusal)))
     ((equal type "response.reasoning_summary_text.delta")
      (list :type 'reasoning-delta
            :stream-kind 'summary
            :content (or (plist-get event :delta)
                         (plist-get event :text))))
     ((equal type "response.reasoning_text.delta")
      (list :type 'reasoning-raw-delta
            :stream-kind 'raw
            :content (or (plist-get event :delta)
                         (plist-get event :text))))
     ((and (equal type "response.output_item.done")
           (e-openai-codex--encrypted-reasoning-item-p
            (plist-get event :item)))
      (list :type 'provider-replay-item
            :provider-id 'openai
            :item (copy-tree (plist-get event :item))))
     ((and (equal type "response.output_item.done")
           (e-openai-codex--function-call-item-p (plist-get event :item)))
      (let ((item (plist-get event :item)))
        (if (e-openai-codex--context-curation-name-p
             (plist-get item :name))
            (e-openai-codex--context-curation-effect
             (plist-get item :arguments)
             (or (plist-get item :call_id)
                 (plist-get item :call-id)))
          (list :type 'tool-call
                :id (or (plist-get item :call_id)
                        (plist-get item :id))
                :name (plist-get item :name)
                :arguments (e-openai-codex--parse-function-arguments
                            (plist-get item :arguments))))))
     ((and (equal type "response.output_item.done")
           (e-openai-codex--message-item-text (plist-get event :item)))
      (list :type 'assistant-message-candidate
            :content (e-openai-codex--message-item-text
                      (plist-get event :item))
            :source 'output-item))
     ((and (equal type "response.content_part.done")
           (member (plist-get (plist-get event :part) :type)
                   '("output_text" "text")))
      (list :type 'assistant-message-candidate
            :content (plist-get (plist-get event :part) :text)
            :source 'content-part))
     ((member type '("response.completed" "response.done"))
      (list :type 'done :reason 'stop))
     ((equal type "response.incomplete")
      (list :type 'done :reason (e-openai-codex--incomplete-reason event)))
     ((equal type "response.failed")
      (list :type 'backend-error
            :content (e-openai-codex--response-error-message event)
            :payload event))
     ((equal type "error")
      (list :type 'backend-error
            :content (e-openai-codex--response-error-message event)
            :payload event))
     (t nil))))

(defun e-openai-codex-parse-stream
    (stream-text &optional prompt-layout-revision reasoning-identity)
  "Parse Codex Responses STREAM-TEXT into backend-neutral items.
PROMPT-LAYOUT-REVISION and REASONING-IDENTITY are stored on emitted
continuation anchors."
  (e-openai-codex--append-raw-response stream-text)
  (let ((items nil)
        (event-summaries nil)
        (assistant-message-seen nil)
        (assistant-message-candidate nil))
    (cl-labels
        ((handle-item
          (item)
          (pcase (plist-get item :type)
            ('assistant-message
             (setq assistant-message-seen t)
             (push item items))
            ('assistant-message-candidate
             (unless assistant-message-candidate
               (setq assistant-message-candidate
                     (list :type 'assistant-message
                           :content (plist-get item :content)))))
            ('done
             (unless assistant-message-seen
               (when assistant-message-candidate
                 (push assistant-message-candidate items)
                 (setq assistant-message-seen t)))
             (push item items))
            (_
             (push item items)))))
      (dolist (chunk (e-openai--sse-chunks stream-text))
        (let ((data-lines nil))
          (dolist (line (e-openai--sse-lines chunk))
            (when (string-prefix-p "data:" line)
              (push (string-trim (substring line 5)) data-lines)))
          (when data-lines
            (let ((data (string-join (nreverse data-lines) "\n")))
              (unless (or (string-empty-p data) (equal data "[DONE]"))
                (let* ((event (e-openai-codex--parse-json data))
                       (item (e-openai-codex--event-item event))
                       (completed-event-p
                        (member (plist-get event :type)
                                '("response.completed" "response.done")))
                       (response (plist-get event :response))
                       (anchor-candidate-item
                        (when completed-event-p
                          (e-openai-codex--anchor-candidate-item
                           response prompt-layout-revision
                           reasoning-identity)))
                       (usage-item
                        (when completed-event-p
                          (e-openai-codex--usage-item
                           (plist-get response :usage)))))
                  (push (e-openai-codex--event-summary event item)
                        event-summaries)
                  (when usage-item
                    (handle-item usage-item))
                  (when anchor-candidate-item
                    (handle-item anchor-candidate-item))
                  (when item
                    (handle-item item)))))))))
    (unless assistant-message-seen
      (when assistant-message-candidate
        (push assistant-message-candidate items)))
    (unless items
      (when-let ((error-item (e-openai-codex--json-error-item stream-text)))
        (push error-item items)))
    (unless (or items (e-openai--sse-response-p stream-text))
      (when-let ((error-item
                  (e-openai-codex--non-stream-error-item stream-text)))
        (push error-item items)))
    (when e-openai-codex-debug
      (setq e-openai-codex--last-diagnostics
            (list :raw-response (e-openai-codex--raw-response-tail stream-text)
                  :events (nreverse event-summaries))))
    (nreverse items)))


(defun e-openai-chat-completion--choice-delta (choice)
  "Return CHOICE delta plist from a Chat Completions chunk."
  (plist-get choice :delta))

(defun e-openai-chat-completion--delta-content (delta)
  "Return text content from Chat Completions DELTA."
  (let ((content (or (plist-get delta :content)
                     (plist-get delta :refusal))))
    (when (stringp content) content)))

(defun e-openai-chat-completion--delta-tool-calls (delta)
  "Return tool-call deltas from Chat Completions DELTA."
  (e-openai-codex--sequence-list (plist-get delta :tool_calls)))

(defun e-openai-chat-completion--usage-item (usage)
  "Return provider-neutral token usage item for Chat Completions USAGE."
  (when (consp usage)
    (let* ((prompt-details (plist-get usage :prompt_tokens_details))
           (completion-details (plist-get usage :completion_tokens_details))
           (normalized
            (list
             :input-tokens
             (e-openai-codex--number-or-nil
              (plist-get usage :prompt_tokens))
             :cached-input-tokens
             (e-openai-codex--number-or-nil
              (plist-get prompt-details :cached_tokens))
             :cache-creation-input-tokens
             (e-openai-codex--number-or-nil
              (plist-get prompt-details :cache_write_tokens))
             :output-tokens
             (e-openai-codex--number-or-nil
              (plist-get usage :completion_tokens))
             :reasoning-output-tokens
             (e-openai-codex--number-or-nil
              (plist-get completion-details :reasoning_tokens))
             :total-tokens
             (e-openai-codex--number-or-nil
              (plist-get usage :total_tokens)))))
      (list :type 'token-usage :usage normalized))))

(defun e-openai-chat-completion--tool-call-key (tool-call fallback-index)
  "Return stable accumulator key for TOOL-CALL with FALLBACK-INDEX."
  (or (plist-get tool-call :index)
      (plist-get tool-call :id)
      fallback-index))

(defun e-openai-chat-completion--merge-tool-call-delta
    (state tool-call fallback-index)
  "Merge one TOOL-CALL delta into STATE and return its accumulator."
  (let* ((key (e-openai-chat-completion--tool-call-key tool-call fallback-index))
         (existing (or (assoc key state)
                       (let ((entry (cons key (list :arguments ""))))
                         (push entry state)
                         entry)))
         (acc (cdr existing))
         (function (plist-get tool-call :function)))
    (when (plist-get tool-call :id)
      (setq acc (plist-put acc :id (plist-get tool-call :id))))
    (when (plist-get function :name)
      (setq acc (plist-put acc :name (plist-get function :name))))
    (when (plist-member function :arguments)
      (setq acc (plist-put acc :arguments
                           (concat (or (plist-get acc :arguments) "")
                                   (or (plist-get function :arguments) "")))))
    (setcdr existing acc)
    (cons state acc)))

(defun e-openai-chat-completion--finish-reason-symbol (reason)
  "Return provider-neutral done reason for Chat Completions REASON."
  (cond
   ((or (null reason) (equal reason "stop")) 'stop)
   ((equal reason "length") 'length)
   ((equal reason "tool_calls") 'tool-calls)
   ((equal reason "content_filter") 'content-filter)
   (t (intern (replace-regexp-in-string "_" "-" (format "%s" reason))))))

(defun e-openai-chat-completion--tool-call-finish-p (reason)
  "Return non-nil when REASON means accumulated tool calls are complete."
  (equal reason "tool_calls"))

(defun e-openai-chat-completion-parse-stream (stream-text)
  "Parse Chat Completions STREAM-TEXT into backend-neutral items."
  (e-openai-codex--append-raw-response stream-text)
  (let ((items nil)
        (text-parts nil)
        (tool-state nil)
        (done-seen nil))
    (cl-labels
        ((emit-tool-calls
          ()
          (dolist (entry (nreverse tool-state))
            (let* ((acc (cdr entry))
                   (arguments (plist-get acc :arguments)))
              (when (and (plist-get acc :id)
                         (plist-get acc :name))
                (if (e-openai-codex--context-curation-name-p
                     (plist-get acc :name))
                    (push (e-openai-codex--context-curation-effect
                           arguments
                           (plist-get acc :id))
                          items)
                  (push (list :type 'tool-call
                              :id (plist-get acc :id)
                              :name (plist-get acc :name)
                              :arguments
                              (e-openai-codex--parse-function-arguments
                               arguments))
                        items)))))))
      (dolist (chunk (e-openai--sse-chunks stream-text))
        (let ((data-lines nil))
          (dolist (line (e-openai--sse-lines chunk))
            (when (string-prefix-p "data:" line)
              (push (string-trim (substring line 5)) data-lines)))
          (when data-lines
            (let ((data (string-join (nreverse data-lines) "\n")))
              (cond
               ((or (string-empty-p data) (equal data "[DONE]")) nil)
               (t
                (let* ((event (e-openai-codex--parse-json data))
                       (usage-item
                        (e-openai-chat-completion--usage-item
                         (plist-get event :usage))))
                  (when usage-item
                    (push usage-item items))
                  (dolist (choice (e-openai-codex--sequence-list
                                   (plist-get event :choices)))
                    (let* ((delta (e-openai-chat-completion--choice-delta
                                   choice))
                           (content
                            (e-openai-chat-completion--delta-content delta))
                           (tool-calls
                            (e-openai-chat-completion--delta-tool-calls delta))
                           (finish-reason (plist-get choice :finish_reason)))
                      (when content
                        (push content text-parts)
                        (push (list :type 'assistant-delta
                                    :content content)
                              items))
                      (cl-loop for tool-call in tool-calls
                               for index from 0
                               do (let ((merged
                                         (e-openai-chat-completion--merge-tool-call-delta
                                          tool-state tool-call index)))
                                    (setq tool-state (car merged))))
                      (when finish-reason
                        (unless done-seen
                          (if (e-openai-chat-completion--tool-call-finish-p
                               finish-reason)
                              (emit-tool-calls)
                            (when text-parts
                              (push (list :type 'assistant-message
                                          :content
                                          (apply #'concat
                                                 (nreverse text-parts)))
                                    items)))
                          (push (list :type 'done
                                      :reason
                                      (e-openai-chat-completion--finish-reason-symbol
                                       finish-reason))
                                items)
                          (setq done-seen t)))))))))))))
    (unless items
      (when-let ((error-item (e-openai-codex--json-error-item stream-text)))
        (push error-item items)))
    (nreverse items)))

(cl-defun e-openai--request-context
    (&key provider auth-file base-url model messages options)
  "Return adapter-local request context for PROVIDER request data.
AUTH-FILE, BASE-URL, MODEL, MESSAGES, and OPTIONS contribute to the encoded
OpenAI request and backend-neutral context."
  (e-openai--profile-call
   'openai.request-context
   (list :metadata (list :provider provider
                         :message-count (length messages)
                         :tool-count (length (plist-get options :tools))))
   (lambda ()
     (let* ((profile (e-openai-provider-profile provider))
            (wire-api (e-openai--provider-wire-api profile))
            (responses-transport (e-openai--profile-responses-transport profile))
            (websocket-idle-close-seconds
             (when (eq responses-transport 'websocket)
               (e-openai--profile-websocket-idle-close-seconds profile)))
            (effective-options (copy-sequence options))
            (_ (when (and (eq responses-transport 'websocket)
                          (not (eq wire-api 'responses)))
                 (signal 'e-openai-provider-invalid
                         '("WebSocket transport is only supported for Responses providers"))))
            (_ (unless (plist-get effective-options :model)
                 (setq effective-options
                       (plist-put effective-options
                                  :model
                                  (e-openai--provider-model profile model)))))
            (_ (when (eq wire-api 'responses)
                 (setq effective-options
                       (plist-put effective-options
                                  :responses-transport
                                  responses-transport))
                 (setq effective-options
                       (plist-put
                        effective-options
                        :prompt-cache-breakpoint-mode
                        (plist-get profile :prompt-cache-breakpoint-mode)))
                 (setq effective-options
                       (plist-put
                        effective-options
                        :responses-context-layout
                        (plist-get profile :responses-context-layout)))
                 ;; Responses always carries a summary choice.  Materialize
                 ;; the profile value here for direct adapter callers while
                 ;; preserving a caller-provided request override.
                 (unless (plist-member effective-options :reasoning-summary)
                   (setq effective-options
                         (plist-put
                          effective-options
                          :reasoning-summary
                          (e-openai--profile-reasoning-summary profile))))
                 ;; Direct adapter callers do not pass through the harness
                 ;; capability projection.  Materialize the profile's
                 ;; semantic delivery contract here so the input layout is
                 ;; stable with or without a prompt-cache key.  Preserve an
                 ;; explicit harness value when one is already present.
                 (unless (plist-member effective-options
                                        :observation-delivery)
                   (setq effective-options
                         (plist-put
                          effective-options
                          :observation-delivery
                          (e-openai--profile-observation-delivery profile))))
                 (setq effective-options
                       (plist-put
                        effective-options
                        :include-encrypted-reasoning
                        (plist-get profile :include-encrypted-reasoning)))
                 (if (plist-get effective-options :context-lifetime-enabled)
                     (setq effective-options
                           (plist-put effective-options
                                      :reserved-effect-carrier
                                      'context-curate-wire))
                   (cl-remf effective-options :reserved-effect-carrier))
                 (when (plist-member profile :response-store)
                   (setq effective-options
                         (plist-put effective-options
                                    :response-store
                                    (plist-get profile :response-store))))))
            (_ (when (and (eq wire-api 'responses)
                          (plist-member effective-options :prompt-cache-retention)
                          (not (e-openai--profile-prompt-cache-retention-supported-p
                                profile
                                (plist-get effective-options :model))))
                 (cl-remf effective-options :prompt-cache-retention)))
            (reasoning-identity
             (when (eq wire-api 'responses)
               (e-openai-codex--reasoning-identity effective-options)))
            (body-data
             (e-openai--profile-call
              'openai.request-body
              (list :metadata (list :provider provider
                                    :wire-api wire-api
                                    :message-count (length messages)
                                    :tool-count
                                    (length (plist-get effective-options :tools))))
              (lambda ()
                (pcase wire-api
                  ('responses
                   (e-openai-codex-request-body
                    :messages messages
                    :options effective-options
                    :tools (plist-get effective-options :tools)))
                  ('chat-completion
                   (e-openai-chat-completion-request-body
                    :messages messages
                    :options effective-options
                    :tools (plist-get effective-options :tools)))))))
            (full-body-data
             (when (and (eq wire-api 'responses)
                        (eq responses-transport 'websocket))
               (e-openai-codex-request-body
                :messages messages
                :options
                (e-openai-codex--without-provider-anchor effective-options)
                :tools (plist-get effective-options :tools))))
            (metadata (e-openai--request-metadata
                       wire-api effective-options body-data))
            (body
             (e-openai--profile-call
              'openai.request-json
              (list :metadata (list :provider provider
                                    :wire-api wire-api))
              (lambda ()
                (json-encode body-data))))
            (session-id (plist-get effective-options :session-id))
            (url (pcase wire-api
                   ('responses
                    (let ((resolved-base-url
                           (or base-url
                               (e-openai--provider-base-url profile))))
                      (if (eq responses-transport 'websocket)
                          (e-openai-responses-websocket-url resolved-base-url)
                        (e-openai-responses-url resolved-base-url))))
                   ('chat-completion
                    (e-openai-chat-completion-url
                     (or base-url
                         (e-openai--provider-base-url profile))))))
            (headers
             (e-openai--profile-call
              'openai.request-headers
              (list :metadata (list :provider provider
                                    :wire-api wire-api
                                    :transport responses-transport))
              (lambda ()
                (e-openai--headers
                 :profile profile
                 :auth-file auth-file
                 :session-id session-id))))
            (headers (if (and (eq wire-api 'responses)
                              (eq responses-transport 'websocket))
                         (e-openai--responses-websocket-headers headers)
                       headers)))
       (list :provider provider
             :wire-api wire-api
             :responses-transport responses-transport
             :websocket-idle-close-seconds websocket-idle-close-seconds
             :prompt-layout-revision
             (plist-get metadata :openai-prompt-layout-revision)
             :reasoning-identity reasoning-identity
             :session-id session-id
             :url url
             :headers headers
             :metadata metadata
             :body-data body-data
             :full-body-data full-body-data
             :body body)))))

(defun e-openai--premature-stream-error-item (wire-api)
  "Return a retryable premature-stream error item for WIRE-API."
  (list :type 'backend-error
        :content
        (format "%s stream ended prematurely before a terminal event"
                (if (eq wire-api 'responses)
                    "Responses"
                  "Chat Completions"))
        :payload (list :response-kind 'sse :wire-api wire-api)))

(defun e-openai--response-body-text (response)
  "Return body text from string or structured HTTP RESPONSE."
  (if (e-openai--http-response-p response)
      (e-openai--http-response-body response)
    response))

(defun e-openai--http-response-error-details (response)
  "Return backend error details carried by structured HTTP RESPONSE."
  (when (e-openai--http-response-p response)
    (append
     (when-let ((status (e-openai--http-response-status response)))
       (list :status status))
     (when-let ((retry-after (e-openai--http-response-retry-after response)))
       (list :retry-after retry-after)))))

(defun e-openai--http-error-status-p (response)
  "Return non-nil when structured HTTP RESPONSE has an error status."
  (and (e-openai--http-response-p response)
       (numberp (e-openai--http-response-status response))
       (>= (e-openai--http-response-status response) 400)))

(defun e-openai--http-error-item (response items)
  "Return one backend error for HTTP RESPONSE, preserving parsed ITEMS."
  (let* ((body (e-openai--response-body-text response))
         (details (e-openai--http-response-error-details response))
         (parsed (seq-find (lambda (item)
                             (eq (plist-get item :type) 'backend-error))
                           items))
         (item
          (or (and parsed (copy-tree parsed))
              (let ((preview (e-openai-codex--text-preview body)))
                (list
                 :type 'backend-error
                 :content
                 (if (string-empty-p preview)
                     (format "OpenAI HTTP request failed with status %s"
                             (e-openai--http-response-status response))
                   (format "OpenAI HTTP request failed with status %s: %s"
                           (e-openai--http-response-status response)
                           preview)))))))
    (plist-put item :payload
               (append details (copy-tree (plist-get item :payload))))
    item))

(defun e-openai--terminal-response-item-p (item)
  "Return non-nil when ITEM settles a complete provider response."
  (memq (plist-get item :type) '(done backend-error)))

(defun e-openai--complete-response-items (response context)
  "Parse complete HTTP RESPONSE for CONTEXT and validate stream settlement."
  (let* ((wire-api (plist-get context :wire-api))
         (body (e-openai--response-body-text response))
         items
         parse-error)
    (condition-case err
        (setq items
              (pcase wire-api
                ('responses
                 (e-openai-codex-parse-stream
                  body
                  (plist-get context :prompt-layout-revision)
                  (plist-get context :reasoning-identity)))
                ('chat-completion
                 (e-openai-chat-completion-parse-stream body))))
      (json-error (setq parse-error err)))
    (cond
     ((e-openai--http-error-status-p response)
      (list (e-openai--http-error-item response items)))
     ((and parse-error
           (eq (car parse-error) 'json-end-of-file)
           (e-openai--sse-response-p body))
      (list (e-openai--premature-stream-error-item wire-api)))
     (parse-error
      (signal (car parse-error) (cdr parse-error)))
     ((seq-some #'e-openai--terminal-response-item-p items)
      items)
     ((string-empty-p (string-trim (or body "")))
      (list (e-openai--premature-stream-error-item wire-api)))
     ((e-openai--sse-response-p body)
      (list (e-openai--premature-stream-error-item wire-api)))
     (t
      (list (e-openai-codex--non-stream-error-item body wire-api))))))

(defun e-openai--emit-response-items (response context on-item)
  "Parse complete RESPONSE for CONTEXT and emit items through ON-ITEM."
  (dolist (item (e-openai--complete-response-items response context))
    (funcall on-item (e-openai--normalize-backend-error-item item))))

(defun e-openai--provider-compaction-eligible-p
    (provider profile base-url request-function compaction-request-function)
  "Return non-nil when PROFILE proves the public compact endpoint.

The compact operation is deliberately narrower than ordinary Responses
continuation.  It requires the first-party API base URL and Responses wire
format.  A separate injected compaction requester is a test seam for that
known endpoint; an arbitrary ordinary request override is not evidence for
the compact capability."
  (and (eq (e-openai--provider-wire-api profile) 'responses)
       (or (eq provider 'openai)
           (eq (plist-get profile :provider-compaction) 'opaque))
       (e-openai--context-base-url-equal-p
        (or base-url (plist-get profile :base-url))
        e-openai-api-default-base-url)
       (or (null request-function)
           compaction-request-function)))

(defun e-openai--provider-compaction-input (messages)
  "Return Responses input items for portable MESSAGES.

MESSAGES have already crossed the provider-neutral portable projection.  This
adapter mapping adds no replay, anchor, diagnostic, or current-state fields."
  (vconcat (mapcar (lambda (message)
                     (e-openai-codex--input-message message))
                   messages)))

(defun e-openai--provider-compaction-headers (profile auth-file session-id)
  "Return JSON response headers for PROFILE's compact endpoint."
  (let ((headers (e-openai--headers :profile profile
                                    :auth-file auth-file
                                    :session-id session-id)))
    (cons '("Accept" . "application/json")
          (seq-remove (lambda (header)
                        (equal (car header) "Accept"))
                      headers))))

(defun e-openai--provider-compaction-usage (usage)
  "Return bounded generic usage from OpenAI compact USAGE."
  (when (listp usage)
    (let (result)
      (dolist (mapping '((:input_tokens . :input-tokens)
                         (:output_tokens . :output-tokens)
                         (:total_tokens . :total-tokens)))
        (when-let ((value (plist-get usage (car mapping))))
          (unless (and (integerp value) (>= value 0))
            (signal 'e-openai-provider-invalid
                    (list "Invalid compact usage" usage)))
          (setq result (append result (list (cdr mapping) value)))))
      result)))

(defun e-openai--provider-compaction-decode (response)
  "Decode one complete OpenAI compact RESPONSE into the generic result."
  (let* ((body (e-openai--response-body-text response))
         ;; `json-parse-string' represents both an empty object and an empty
         ;; plist as nil.  Reject an object-valued output before that loss of
         ;; shape so only the documented array is accepted.
         (object-output-p
          (and (stringp body)
               (string-match-p
                "\\\"output\\\"[[:space:]]*:[[:space:]]*{"
                body)))
         (parsed
          (if (and (listp body)
                   (or (null body) (keywordp (car body))))
            body
            (json-parse-string body
                               :object-type 'plist
                               :array-type 'list
                               :null-object :json-null
                               :false-object :json-false)))
         (object (plist-get parsed :object))
         (output (plist-get parsed :output)))
    (when (e-openai--http-error-status-p response)
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact request failed" response)))
    (when object-output-p
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact output is an object" parsed)))
    (unless (member object '("response.compaction" response.compaction))
      (signal 'e-openai-provider-invalid
              (list "Unexpected OpenAI compact object" object)))
    (unless (plist-member parsed :output)
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact response has no output field" parsed)))
    (when (eq output :json-null)
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact response has no output" parsed)))
    (unless (or (vectorp output)
                (and (listp output)
                     (or (null output)
                         (not (keywordp (car output))))))
      (signal 'e-openai-provider-invalid
              (list "OpenAI compact output is not an array" output)))
    (list :output output
          :usage (e-openai--provider-compaction-usage
                  (plist-get parsed :usage)))))

(cl-defun e-openai--provider-compaction
    (profile auth-file base-url model compaction-request-function
             &key messages options on-done on-error)
  "Run the public OpenAI compact endpoint for portable MESSAGES."
  (let* ((url (concat (string-remove-suffix "/"
                                           (or base-url
                                               (plist-get profile :base-url)))
                      "/responses/compact"))
         (body-data (list :model (or (plist-get options :model)
                                     model
                                     (plist-get profile :default-model)
                                     e-openai-default-model)
                          :input (e-openai--provider-compaction-input messages)))
         (body (json-encode body-data))
         (headers (e-openai--provider-compaction-headers
                   profile auth-file (plist-get options :session-id)))
         (requester (or compaction-request-function
                        #'e-openai-codex--http-request)))
    (if (or on-done on-error)
        (if compaction-request-function
            (let ((cancelled nil)
                  (timer nil)
                  request)
              (setq request
                    (e-backend-request-create
                     :cancel (lambda ()
                               (setq cancelled t)
                               (when (timerp timer)
                                 (cancel-timer timer))
                               t)
                     :metadata (list :transport 'injected-compaction
                                     :url url)))
              (setq timer
                    (run-at-time
                     0 nil
                     (lambda ()
                       (unless cancelled
                         (condition-case err
                             (funcall on-done
                                      (e-openai--provider-compaction-decode
                                       (funcall requester
                                                :url url
                                                :headers headers
                                                :body body)))
                           (error
                            (when on-error
                              (funcall on-error err))))))))
              request)
          (e-openai-codex--http-request-start
           :url url
           :headers headers
           :body body
           :on-complete
           (lambda (response)
             (condition-case err
                 (funcall on-done
                          (e-openai--provider-compaction-decode response))
               (error
                (when on-error (funcall on-error err)))))
           :on-error on-error))
      (e-openai--provider-compaction-decode
       (funcall requester :url url :headers headers :body body)))))

(cl-defun e-openai-backend-create
    (&key provider auth-file base-url request-function
          compaction-request-function name model)
  "Create an OpenAI-like backend named NAME.
PROVIDER selects a profile from `e-openai-model-providers'.  AUTH-FILE is used
for Codex-managed OpenAI auth profiles.  BASE-URL overrides the profile base
URL.  REQUEST-FUNCTION is injectable for tests.  MODEL is the backend-local
default when turn options do not include `:model'.  The provider profile's
`:wire-api' chooses the Responses or Chat Completions request/stream mapping."
  (let* ((provider (or provider e-openai-default-provider))
         (profile (e-openai-provider-profile provider))
         (provider-compaction-supported
          (e-openai--provider-compaction-eligible-p
           provider profile base-url request-function
           compaction-request-function))
         (provider-compaction
          (when provider-compaction-supported
            (cl-function
             (lambda (&key messages options on-done on-error)
               (e-openai--provider-compaction
                profile auth-file base-url model compaction-request-function
                :messages messages
                :options options
                :on-done on-done
                :on-error on-error)))))
         (websocket-sessions (make-hash-table :test 'equal)))
    (cl-labels
        ((request-metadata (context transport cancellable)
           (append
            (list :provider (plist-get context :provider)
                  :wire-api (plist-get context :wire-api)
                  :url (plist-get context :url)
                  :cancellable cancellable
                  :transport transport)
            (plist-get context :metadata)
            (e-openai-codex--url-metadata
             (plist-get context :url))))
         (websocket-session (context)
           (let ((key (or (plist-get context :session-id)
                          :backend-default)))
             (or (gethash key websocket-sessions)
                 (puthash
                  key
                  (e-openai-codex--websocket-session-create)
                  websocket-sessions))))
         (websocket-request-metadata (context)
           (append
            (list :provider (plist-get context :provider)
                  :wire-api (plist-get context :wire-api))
            (plist-get context :metadata))))
      (e-backend-create
       :name (or name (e-openai-provider-name provider))
       :normalize-error-details #'e-openai--normalize-error-details
       :context-capabilities
       (lambda (options)
         (e-openai--profile-context-capabilities
          (e-openai-provider-profile provider)
          options
          :provider provider
          :base-url base-url
          :request-function request-function
          :provider-compaction-supported
          (and provider-compaction-supported
               (functionp provider-compaction))))
       :provider-compaction provider-compaction
       :stream
       (cl-function
        (lambda (&key messages options on-item)
          (e-openai--reject-sync-in-hot-path 'e-openai-backend-stream)
          (let* ((context (e-openai--request-context
                           :provider provider
                           :auth-file auth-file
                           :base-url base-url
                           :model model
                           :messages messages
                           :options options))
                 (websocket-p (and (not request-function)
                                   (eq (plist-get context :responses-transport)
                                       'websocket))))
            (if websocket-p
                (let (done failure)
                  (e-backend-note-request-started
                   (e-openai-codex--websocket-request-start
                    :session (websocket-session context)
                    :url (plist-get context :url)
                    :headers (plist-get context :headers)
                    :body-data (plist-get context :body-data)
                    :full-body-data (plist-get context :full-body-data)
                    :request-metadata (websocket-request-metadata context)
                    :prompt-layout-revision
                    (plist-get context :prompt-layout-revision)
                    :reasoning-identity
                    (plist-get context :reasoning-identity)
                    :idle-close-seconds
                    (plist-get context :websocket-idle-close-seconds)
                    :on-item on-item
                    :on-complete (lambda (_status)
                                   (setq done t))
                    :on-error (lambda (err)
                                (setq failure err)
                                (setq done t))))
                  (while (not done)
                    (accept-process-output nil 0.01))
                  (when failure
                    (signal (car failure) (cdr failure))))
              (let* ((requester (or request-function
                                    #'e-openai-codex--http-request))
                     (response nil))
                (e-backend-note-request-started
                 (e-backend-request-create
                  :metadata
                  (request-metadata context 'sync-wrapper nil)))
                (setq response
                      (funcall requester
                               :url (plist-get context :url)
                               :headers (plist-get context :headers)
                               :body (plist-get context :body)))
                (e-openai--emit-response-items response context on-item))))))
       :start
       (cl-function
        (lambda (&key messages options on-item on-done on-error
                      on-request-start)
          (let* ((context (e-openai--request-context
                           :provider provider
                           :auth-file auth-file
                           :base-url base-url
                           :model model
                           :messages messages
                           :options options)))
            (cond
             (request-function
              (let ((cancelled nil)
                    (timer nil)
                    request)
                (setq request
                      (e-backend-request-create
                       :cancel (lambda ()
                                 (setq cancelled t)
                                 (when (timerp timer)
                                   (cancel-timer timer))
                                 t)
                       :metadata
                       (request-metadata
                        context
                        'injected-request-function
                        'queued-only)))
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
                                 (e-openai--emit-response-items
                                  response context on-item)
                                 (when on-done
                                   (funcall on-done '(:status done))))
                             (error
                              (when on-error
                                (funcall on-error err))))))))
                request))
             ((eq (plist-get context :responses-transport) 'websocket)
              (let ((request
                     (e-openai-codex--websocket-request-start
                      :session (websocket-session context)
                      :url (plist-get context :url)
                      :headers (plist-get context :headers)
                      :body-data (plist-get context :body-data)
                      :full-body-data (plist-get context :full-body-data)
                      :request-metadata (websocket-request-metadata context)
                      :prompt-layout-revision
                      (plist-get context :prompt-layout-revision)
                      :reasoning-identity
                      (plist-get context :reasoning-identity)
                      :idle-close-seconds
                      (plist-get context :websocket-idle-close-seconds)
                      :on-item on-item
                      :on-complete on-done
                      :on-error on-error)))
                (when on-request-start
                  (funcall on-request-start request))
                request))
             (t
              (let ((request
                     (e-openai-codex--http-request-start
                      :url (plist-get context :url)
                      :headers (plist-get context :headers)
                      :body (plist-get context :body)
                      :on-complete
                      (lambda (response)
                        (condition-case err
                            (progn
                              (e-openai--emit-response-items response context on-item)
                              (when on-done
                                (funcall on-done '(:status done))))
                          (error
                           (when on-error
                             (funcall on-error err)))))
                      :on-error on-error)))
                (setf (e-backend-request-metadata request)
                      (append
                       (list :provider (plist-get context :provider)
                             :wire-api (plist-get context :wire-api))
                       (plist-get context :metadata)
                       (e-backend-request-metadata request)))
                (when on-request-start
                  (funcall on-request-start request))
                request))))))))))

(cl-defun e-openai-create-harness
    (&key provider auth-file base-url request-function
          compaction-request-function model sessions)
  "Create a harness configured for an OpenAI-like provider.
PROVIDER selects `e-openai-default-provider' when nil.  AUTH-FILE, BASE-URL,
and REQUEST-FUNCTION configure the backend adapter.  MODEL is written into
backend-neutral turn options by the default context strategy used by harness
turn paths.  SESSIONS supplies an existing session store."
  (let* ((provider (or provider e-openai-default-provider))
         (profile (e-openai-provider-profile provider))
         (model (e-openai--provider-model profile model)))
    (e-harness-create
     :backend (e-openai-backend-create
               :provider provider
               :auth-file auth-file
               :base-url base-url
               :request-function request-function
               :compaction-request-function compaction-request-function
               :model model)
     :default-options (e-openai--harness-default-options profile model)
     :sessions sessions)))

(cl-defun e-openai-codex-backend-create
    (&key auth-file base-url request-function name model)
  "Create an OpenAI/Codex backend named NAME.
AUTH-FILE points at Codex-managed auth.  BASE-URL defaults to ChatGPT's Codex
backend.  REQUEST-FUNCTION is injectable for tests."
  (e-openai-backend-create
   :provider 'codex
   :auth-file auth-file
   :base-url (when base-url (e-openai-codex-url base-url))
   :request-function request-function
   :name (or name "openai-codex")
   :model model))

(cl-defun e-openai-codex-create-harness
    (&key auth-file base-url request-function model)
  "Create a harness configured for ChatGPT-backed Codex.
AUTH-FILE, BASE-URL, and REQUEST-FUNCTION configure the backend adapter.  MODEL
is written into backend-neutral turn options by the default harness context
strategy."
  (e-openai-create-harness
   :provider 'codex
   :auth-file auth-file
   :base-url (when base-url (e-openai-codex-url base-url))
   :request-function request-function
   :model model))

(provide 'e-openai)

;;; e-openai.el ends here

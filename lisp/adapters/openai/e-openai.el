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
(require 'e-backend)
(require 'e-harness)
(require 'e-tools)

(define-error 'e-openai-auth-missing "OpenAI/Codex auth is missing")
(define-error 'e-openai-auth-invalid "OpenAI/Codex auth is invalid")
(define-error 'e-openai-provider-invalid "OpenAI provider profile is invalid")
(define-error 'e-openai-request-timeout "OpenAI/Codex request timed out")

(defconst e-openai-codex-default-base-url
  "https://chatgpt.com/backend-api"
  "Default base URL for ChatGPT-backed Codex Responses access.")

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

(defcustom e-openai-default-provider 'codex
  "Default provider profile used by generic OpenAI harness helpers."
  :type 'symbol
  :group 'e-openai)

(defcustom e-openai-request-timeout-seconds 180
  "Seconds before OpenAI-like HTTP requests fail.
Set this to nil to deliberately disable provider HTTP request timeouts."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'e-openai)

(defcustom e-openai-model-providers
  `((codex
     :name "ChatGPT Codex"
     :base-url ,(concat e-openai-codex-default-base-url "/codex")
     :wire-api responses
     :continuation t
     :requires-openai-auth t))
  "OpenAI-like model provider profiles keyed by provider symbol.

Each profile is plist data.  `:wire-api' supports `responses' and
`chat-completion' (`chat-completions' is accepted as a compatibility alias).
Profiles with `:requires-openai-auth' non-nil use Codex-managed ChatGPT auth.
Profiles with `:requires-openai-auth' nil read a bearer token from `:env-key'.
Profiles can set `:prompt-cache-retention' to nil to suppress that request
field, or non-nil to force it on.  Responses profiles can opt into provider
continuation anchors with `:continuation' non-nil."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'e-openai)

(defcustom e-openai-codex-debug nil
  "When non-nil, retain the last raw Codex response and event summaries."
  :type 'boolean
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
  (let ((wire-api (plist-get profile :wire-api)))
    (if (eq wire-api 'chat-completions)
        'chat-completion
      wire-api)))

(defun e-openai--profile-prompt-cache-retention-supported-p (profile)
  "Return non-nil when PROFILE accepts `prompt_cache_retention'."
  (if (plist-member profile :prompt-cache-retention)
      (plist-get profile :prompt-cache-retention)
    (not (plist-get profile :requires-openai-auth))))

(defun e-openai--profile-continuation-supported-p (profile)
  "Return non-nil when PROFILE should use Responses continuation anchors."
  (and (eq (e-openai--provider-wire-api profile) 'responses)
       (plist-get profile :continuation)))

(defun e-openai--harness-default-options (profile model)
  "Return backend-neutral harness options for PROFILE and MODEL."
  (let ((options (list :model model
                       :reasoning-effort e-openai-default-reasoning-effort)))
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

(defun e-openai-codex--append-raw-response (stream-text)
  "Append STREAM-TEXT to the hidden raw provider response buffer."
  (unless (string-empty-p (or stream-text ""))
    (with-current-buffer (get-buffer-create
                          e-openai-codex-raw-responses-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n"))
        (insert ";;; " (current-time-string) "\n")
        (insert stream-text)
        (unless (bolp)
          (insert "\n"))))))

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

(defun e-openai-codex--message-content (role content)
  "Return Responses content item for ROLE and CONTENT."
  (let ((type (if (eq role 'assistant) "output_text" "input_text")))
    (vector (list :type type :text (or content "")))))

(defun e-openai-codex--input-message (message)
  "Map backend-neutral MESSAGE to a Responses input item."
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
             :role (symbol-name role)
             :content (e-openai-codex--message-content role content))))))

(defun e-openai-codex--system-message-p (message)
  "Return non-nil when MESSAGE is a backend-neutral system message."
  (eq (plist-get message :role) 'system))

(defun e-openai-codex--instructions (messages options)
  "Return top-level Codex instructions from MESSAGES and OPTIONS."
  (string-join
   (delq nil
         (append
          (list (or (plist-get options :instructions)
                    "You are a helpful assistant."))
          (mapcar (lambda (message)
                    (plist-get message :content))
                  (seq-filter #'e-openai-codex--system-message-p
                              messages))))
   "\n\n"))

(defun e-openai-codex--continuation-response-id (options)
  "Return previous Responses id from OPTIONS when continuation is enabled."
  (when (plist-get options :provider-continuation)
    (let* ((anchor (plist-get options :provider-anchor))
           (metadata (plist-get anchor :metadata))
           (response-id (plist-get metadata :response-id)))
      (when (and (eq (plist-get anchor :provider-id) 'openai)
                 (stringp response-id)
                 (not (string-empty-p response-id)))
        response-id))))

(defun e-openai-codex--request-input-messages (messages options)
  "Return Responses input messages from MESSAGES and OPTIONS."
  (let* ((response-id (e-openai-codex--continuation-response-id options))
         (delta-messages (plist-get options :provider-anchor-delta-messages))
         (source (if (and response-id (listp delta-messages))
                     delta-messages
                   messages)))
    (seq-remove #'e-openai-codex--system-message-p source)))

(cl-defun e-openai-codex-request-body (&key messages options tools)
  "Build a Codex Responses request body from MESSAGES, OPTIONS, and TOOLS."
  (let* ((input-messages (e-openai-codex--request-input-messages
                          messages
                          options))
         (continuation-response-id
          (e-openai-codex--continuation-response-id options))
         (reasoning (if (plist-member options :reasoning)
                        (plist-get options :reasoning)
                      (when-let ((effort (or (plist-get options :reasoning-effort)
                                             e-openai-default-reasoning-effort)))
                        (list :effort effort))))
         (body (list :model (or (plist-get options :model)
                                e-openai-default-model)
                     :store (if (plist-get options :provider-continuation)
                                t
                              :json-false)
                     :stream t
                     :instructions (e-openai-codex--instructions
                                    messages
                                    options)
                     :input (vconcat (mapcar #'e-openai-codex--input-message
                                             input-messages))
                     :tool_choice "auto"
                     :parallel_tool_calls t)))
    (when tools
      (setq body (append body (list :tools (vconcat tools)))))
    (when reasoning
      (setq body (append body (list :reasoning reasoning))))
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
           (continuation-state
            (cond
             ((not (plist-get options :provider-continuation)) 'disabled)
             ((plist-member body :previous_response_id) 'used)
             (t 'full)))
           (metadata (list :provider-continuation continuation-state)))
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

(defun e-openai-codex--http-header-bytes (value)
  "Return VALUE as an ASCII byte string suitable for `url-request-extra-headers'."
  (encode-coding-string (format "%s" value) 'us-ascii))

(defun e-openai-codex--http-header-list (headers)
  "Return HEADERS with names and values normalized to byte strings."
  (mapcar (lambda (header)
            (cons (e-openai-codex--http-header-bytes (car header))
                  (e-openai-codex--http-header-bytes (cdr header))))
          headers))

(cl-defun e-openai-codex--http-request (&key url headers body)
  "POST BODY to URL with HEADERS and return response text."
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
    (re-search-forward "\n\n" nil 'move)
    (buffer-substring-no-properties (point) (point-max))))

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
  "Cancel any live request process attached to BUFFER and kill BUFFER."
  (when (buffer-live-p buffer)
    (when-let ((process (get-buffer-process buffer)))
      (when (process-live-p process)
        (if (memq (process-type process) '(real pipe))
            (kill-process process)
          (delete-process process))))
    (kill-buffer buffer)))

(cl-defun e-openai-codex--http-request-start
    (&key url headers body on-complete on-error)
  "POST BODY to URL with HEADERS asynchronously.
ON-COMPLETE receives the response body text.  ON-ERROR receives an Emacs
condition list.  Return a cancellable `e-backend-request' handle."
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
         (handle-callback (status)
           (unless settled
             (setq settled t)
             (let ((buffer (current-buffer)))
               (unwind-protect
                   (condition-case err
                       (let ((url-error (plist-get status :error)))
                         (if url-error
                             (let ((response-text
                                    (e-openai-codex--http-response-text
                                     buffer)))
                               (if (not (string-empty-p
                                         (string-trim response-text)))
                                   (when on-complete
                                     (funcall on-complete response-text))
                                 (when on-error
                                   (funcall
                                    on-error
                                    (list 'error
                                          (format "OpenAI request failed: %S"
                                                  url-error))))))
                           (when on-complete
                             (funcall
                              on-complete
                              (e-openai-codex--http-response-text buffer)))))
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
      (when (and timeout (not settled))
        (setq timeout-timer
              (run-at-time timeout nil #'settle-timeout))))
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
       (equal (plist-get item :type) "function_call")))

(defun e-openai-codex--parse-function-arguments (arguments)
  "Parse JSON ARGUMENTS from a Responses function call."
  (if (and (stringp arguments) (not (string-empty-p arguments)))
      (e-openai-codex--parse-json arguments)
    nil))

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
            (when (member (plist-get part :type) '("output_text" "text"))
              (plist-get part :text)))
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
                    (plist-get event :error))))
    (or (and (listp error)
             (plist-get error :message))
        (and (stringp error) error)
        (format "%S" event))))

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

(defun e-openai-codex--anchor-candidate-item (response)
  "Return provider anchor candidate item from completed RESPONSE."
  (when-let ((response-id (and (consp response)
                               (plist-get response :id))))
    (when (stringp response-id)
      (list :type 'provider-anchor-candidate
            :provider-id 'openai
            :metadata (list :response-id response-id)))))

(defun e-openai-codex--json-error-item (stream-text)
  "Return a backend error item when STREAM-TEXT is a JSON error response."
  (when (string-prefix-p "{" (string-trim-left stream-text))
    (let* ((payload (e-openai-codex--parse-json stream-text))
           (error (plist-get payload :error))
           (detail (plist-get payload :detail))
           (message (cond
                     ((listp error) (plist-get error :message))
                     ((stringp error) error)
                     ((stringp detail) detail))))
      (when message
        (list :type 'backend-error
              :content message
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

(defun e-openai-codex--non-stream-error-item (stream-text)
  "Return a backend error item for non-empty non-SSE STREAM-TEXT."
  (let* ((trimmed (string-trim-left (or stream-text "")))
         (html (string-prefix-p "<" trimmed))
         (preview (if html
                      (e-openai-codex--html-text-preview stream-text)
                    (e-openai-codex--text-preview stream-text)))
         (kind (if html 'html 'text)))
    (when (not (string-empty-p preview))
      (list :type 'backend-error
            :content (format "Provider returned %s instead of a Responses stream: %s"
                             (if html "HTML" "non-stream text")
                             preview)
            :payload (list :response-kind kind
                           :preview preview)))))

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
     ((member type '("response.reasoning_summary_text.delta"
                     "response.reasoning_text.delta"))
      (list :type 'reasoning-delta
            :content (or (plist-get event :delta)
                         (plist-get event :text))))
     ((and (equal type "response.output_item.done")
           (e-openai-codex--function-call-item-p (plist-get event :item)))
      (let ((item (plist-get event :item)))
        (list :type 'tool-call
              :id (plist-get item :call_id)
              :name (plist-get item :name)
              :arguments (e-openai-codex--parse-function-arguments
                          (plist-get item :arguments)))))
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
     ((equal type "response.failed")
      (list :type 'backend-error
            :content (e-openai-codex--response-error-message event)
            :payload event))
     (t nil))))

(defun e-openai-codex-parse-stream (stream-text)
  "Parse Codex Responses STREAM-TEXT into backend-neutral items."
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
      (dolist (chunk (split-string stream-text "\n\n" t))
        (let ((data-lines nil))
          (dolist (line (split-string chunk "\n"))
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
                          (e-openai-codex--anchor-candidate-item response)))
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
    (unless items
      (when-let ((error-item
                  (e-openai-codex--non-stream-error-item stream-text)))
        (push error-item items)))
    (when e-openai-codex-debug
      (setq e-openai-codex--last-diagnostics
            (list :raw-response stream-text
                  :events (nreverse event-summaries))))
    (nreverse items)))


(defun e-openai-chat-completion--choice-delta (choice)
  "Return CHOICE delta plist from a Chat Completions chunk."
  (plist-get choice :delta))

(defun e-openai-chat-completion--delta-content (delta)
  "Return text content from Chat Completions DELTA."
  (let ((content (plist-get delta :content)))
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
                (push (list :type 'tool-call
                            :id (plist-get acc :id)
                            :name (plist-get acc :name)
                            :arguments
                            (e-openai-codex--parse-function-arguments
                             arguments))
                      items))))))
      (dolist (chunk (split-string stream-text "\n\n" t))
        (let ((data-lines nil))
          (dolist (line (split-string chunk "\n"))
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
                          (setq done-seen t))))))))))))
    (unless items
      (when-let ((error-item (e-openai-codex--json-error-item stream-text)))
        (push error-item items)))
    (nreverse items))))

(cl-defun e-openai--request-context
    (&key provider auth-file base-url model messages options)
  "Return adapter-local request context for PROVIDER request data.
AUTH-FILE, BASE-URL, MODEL, MESSAGES, and OPTIONS contribute to the encoded
OpenAI request and backend-neutral context."
  (let* ((profile (e-openai-provider-profile provider))
         (wire-api (e-openai--provider-wire-api profile))
         (effective-options (copy-sequence options))
         (_ (unless (plist-get effective-options :model)
              (setq effective-options
                    (plist-put effective-options
                               :model
                               (e-openai--provider-model profile model)))))
         (_ (when (and (eq wire-api 'responses)
                       (plist-member effective-options :prompt-cache-retention)
                       (not (e-openai--profile-prompt-cache-retention-supported-p
                             profile)))
              (cl-remf effective-options :prompt-cache-retention)))
         (body-data
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
              :tools (plist-get effective-options :tools)))))
         (metadata (e-openai--request-metadata
                    wire-api effective-options body-data))
         (body (json-encode body-data))
         (session-id (plist-get effective-options :session-id))
         (url (pcase wire-api
                ('responses
                 (e-openai-responses-url
                  (or base-url
                      (e-openai--provider-base-url profile))))
                ('chat-completion
                 (e-openai-chat-completion-url
                  (or base-url
                      (e-openai--provider-base-url profile))))))
         (headers (e-openai--headers
                   :profile profile
                   :auth-file auth-file
                   :session-id session-id)))
    (list :provider provider
          :wire-api wire-api
          :url url
          :headers headers
          :metadata metadata
          :body body)))

(defun e-openai--emit-response-items (response context on-item)
  "Parse RESPONSE for CONTEXT and emit backend-neutral items through ON-ITEM."
  (dolist (item (pcase (plist-get context :wire-api)
                  ('responses (e-openai-codex-parse-stream response))
                  ('chat-completion
                   (e-openai-chat-completion-parse-stream response))))
    (funcall on-item item)))

(cl-defun e-openai-backend-create
    (&key provider auth-file base-url request-function name model)
  "Create an OpenAI-like backend named NAME.
PROVIDER selects a profile from `e-openai-model-providers'.  AUTH-FILE is used
for Codex-managed OpenAI auth profiles.  BASE-URL overrides the profile base
URL.  REQUEST-FUNCTION is injectable for tests.  MODEL is the backend-local
default when turn options do not include `:model'.  The provider profile's
`:wire-api' chooses the Responses or Chat Completions request/stream mapping."
  (let ((provider (or provider e-openai-default-provider)))
    (e-backend-create
     :name (or name (e-openai-provider-name provider))
     :stream
     (cl-function
      (lambda (&key messages options on-item)
        (let* ((context (e-openai--request-context
                         :provider provider
                         :auth-file auth-file
                         :base-url base-url
                         :model model
                         :messages messages
                         :options options))
               (requester (or request-function
                              #'e-openai-codex--http-request))
               (response nil))
          (e-backend-note-request-started
           (e-backend-request-create
            :metadata
            (append
             (list :provider (plist-get context :provider)
                   :wire-api (plist-get context :wire-api)
                   :url (plist-get context :url)
                   :cancellable nil
                   :transport 'sync-wrapper)
             (plist-get context :metadata)
             (e-openai-codex--url-metadata
              (plist-get context :url)))))
          (setq response
                (funcall requester
                         :url (plist-get context :url)
                         :headers (plist-get context :headers)
                         :body (plist-get context :body)))
          (e-openai--emit-response-items response context on-item))))
     :start
     (cl-function
      (lambda (&key messages options on-item on-done on-error
                    on-request-start)
        (let ((context (e-openai--request-context
                        :provider provider
                        :auth-file auth-file
                        :base-url base-url
                        :model model
                        :messages messages
                        :options options)))
          (if request-function
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
                       (append
                        (list :provider (plist-get context :provider)
                              :wire-api (plist-get context :wire-api)
                              :url (plist-get context :url)
                              :cancellable 'queued-only
                              :transport 'injected-request-function)
                        (plist-get context :metadata)
                        (e-openai-codex--url-metadata
                         (plist-get context :url)))))
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
                request)
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
              request))))))))

(cl-defun e-openai-create-harness
    (&key provider auth-file base-url request-function model sessions)
  "Create a harness configured for an OpenAI-like provider.
PROVIDER selects `e-openai-default-provider' when nil.  AUTH-FILE, BASE-URL,
and REQUEST-FUNCTION configure the backend adapter.  MODEL is written into
backend-neutral turn options by the default context strategy path used by
`e-harness-prompt'.  SESSIONS supplies an existing session store."
  (let* ((provider (or provider e-openai-default-provider))
         (profile (e-openai-provider-profile provider))
         (model (e-openai--provider-model profile model)))
    (e-harness-create
     :backend (e-openai-backend-create
               :provider provider
               :auth-file auth-file
               :base-url base-url
               :request-function request-function
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
is written into backend-neutral turn options by the default context strategy
path used by `e-harness-prompt'."
  (e-openai-create-harness
   :provider 'codex
   :auth-file auth-file
   :base-url (when base-url (e-openai-codex-url base-url))
   :request-function request-function
   :model model))

(provide 'e-openai)

;;; e-openai.el ends here

;;; e-mcp.el --- MCP capability wrapper for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; MCP support is exposed as construction-time capability sugar.  The harness
;; sees ordinary capabilities and ordinary tools; this module owns the helper
;; process protocol and MCP-to-tool mapping.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-context)
(require 'e-store)
(require 'e-tools)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)

(declare-function e-harness-sessions "e-harness" (harness))
(declare-function e-harness-effective-capability-config "e-harness")
(declare-function e-session-get "e-session" (store session-id))
(declare-function e-session-set-metadata "e-session"
                  (store session-id metadata))

(define-error 'e-mcp-backend-error "MCP helper backend error")
(define-error 'e-mcp-backend-timeout "MCP helper backend timeout"
  'e-mcp-backend-error)
(define-error 'e-mcp-protocol-error "MCP helper protocol error"
  'e-mcp-backend-error)

(dolist (condition '(e-mcp-backend-error
                     e-mcp-backend-timeout
                     e-mcp-protocol-error))
  (put condition 'e-tools-infrastructure-error t))

(defcustom e-mcp-helper-timeout 10
  "Default number of seconds to wait for MCP helper responses."
  :type 'number
  :group 'e)

(defcustom e-mcp-node-executable "node"
  "Node executable used for the MCP helper."
  :type 'string
  :group 'e)

(cl-defstruct (e-mcp-server
               (:constructor e-mcp-server--create
                             (&key id command env timeout url http-headers)))
  id
  command
  env
  timeout
  ;; HTTP (Streamable HTTP) transport fields.  When URL is set the server is
  ;; reached over HTTP directly from Emacs and COMMAND/ENV are unused.
  url
  http-headers)

(cl-defstruct (e-mcp-tool
               (:constructor e-mcp-tool--create
                             (&key server-id name description input-schema
                                   metadata)))
  server-id
  name
  description
  input-schema
  metadata)

(defvar e-mcp--helper-process nil
  "Current MCP helper process.")

(defvar e-mcp--helper-stdout nil
  "Buffer collecting MCP helper stdout frames.")

(defvar e-mcp--helper-stderr nil
  "Buffer collecting MCP helper stderr diagnostics.")

(defvar e-mcp--helper-next-id 0
  "Next Emacs-to-helper request id.")

(defvar e-mcp--known-servers nil
  "MCP servers observed during capability construction.")

(defvar e-mcp--latest-diagnostics nil
  "Most recent diagnostics returned by the MCP helper.")

(defvar e-mcp--http-sessions nil
  "Alist of (server-id . session-plist) for HTTP MCP sessions.
Each session plist tracks :url, :headers, :session-id, :initialized.")

(defvar e-mcp--catalog-cache (make-hash-table :test 'equal)
  "Memoized tools/list catalogs keyed by sorted server id list.
Progressive disclosure touches each server catalog from several places per
turn (Tier-0 card, Tier-1 resources, Tier-2 activation, lazy tool
registration).  Memoizing the helper round trip keeps that fan-out from
re-listing tools on every turn.  Invalidated by `e-mcp-reset' and
`e-mcp-refresh'.")

(defvar e-mcp-helper-transport-function nil
  "Optional fake helper transport function for tests.
When non-nil, the function receives the helper request plist and returns a
helper response plist.")

(defun e-mcp--non-empty-string (value label)
  "Return VALUE when it is a non-empty string for LABEL."
  (unless (and (stringp value) (not (string-empty-p value)))
    (signal 'wrong-type-argument (list label value)))
  value)

(defun e-mcp--valid-command-p (command)
  "Return non-nil when COMMAND is a non-empty string list."
  (and (consp command)
       (cl-every (lambda (part)
                   (and (stringp part)
                        (not (string-empty-p part))))
                 command)))

(defun e-mcp--json-object-p (value)
  "Return non-nil when VALUE is an Emacs JSON object representation."
  (or (hash-table-p value)
      (and (listp value)
           (cl-evenp (length value))
           (cl-loop for (key _item) on value by #'cddr
                    always (keywordp key)))))

(defun e-mcp-server-create (&rest args)
  "Create an MCP server spec from keyword ARGS.
A server must specify either COMMAND (stdio transport) or URL (HTTP
transport), but not both."
  (let* ((server (apply #'e-mcp-server--create args))
         (id (e-mcp-server-id server))
         (command (e-mcp-server-command server))
         (url (e-mcp-server-url server)))
    (e-mcp--non-empty-string id 'mcp-server-id)
    (cond
     ((and url command)
      (signal 'wrong-type-argument
              (list 'mcp-server-transport
                    "specify either :command or :url, not both")))
     (url
      (unless (and (stringp url) (not (string-empty-p url)))
        (signal 'wrong-type-argument (list 'mcp-server-url url))))
     (t
      (unless (e-mcp--valid-command-p command)
        (signal 'wrong-type-argument (list 'mcp-server-command command)))))
    server))

(defun e-mcp-tool-create (&rest args)
  "Create an MCP tool catalog entry from keyword ARGS."
  (let* ((tool (apply #'e-mcp-tool--create args))
         (server-id (e-mcp-tool-server-id tool))
         (name (e-mcp-tool-name tool))
         (schema (e-mcp-tool-input-schema tool)))
    (e-mcp--non-empty-string server-id 'mcp-server-id)
    (e-mcp--non-empty-string name 'mcp-tool-name)
    (unless (e-mcp--json-object-p schema)
      (signal 'wrong-type-argument (list 'mcp-input-schema schema)))
    tool))

(defun e-mcp--directory ()
  "Return the directory containing this file."
  (file-name-directory
   (file-truename
    (or load-file-name
        buffer-file-name
        (locate-library "e-mcp")
        default-directory))))

(defun e-mcp--helper-script ()
  "Return the MCP helper script path."
  (expand-file-name "e-mcp-helper.mjs" (e-mcp--directory)))

(defun e-mcp--helper-command ()
  "Return the command used to start the MCP helper."
  (list e-mcp-node-executable (e-mcp--helper-script)))

(defun e-mcp--buffer-string (buffer)
  "Return BUFFER contents, or an empty string when BUFFER is not live."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun e-mcp--parse-json (text)
  "Parse TEXT as JSON into plist-compatible values."
  (let ((json-object-type 'plist)
        (json-array-type 'vector)
        (json-key-type 'keyword)
        (json-false :json-false)
        (json-null nil))
    (json-read-from-string text)))

(defun e-mcp-reset ()
  "Stop the MCP helper process and clear helper protocol state."
  (when (and e-mcp--helper-process
             (process-live-p e-mcp--helper-process))
    (kill-process e-mcp--helper-process))
  (when (buffer-live-p e-mcp--helper-stdout)
    (kill-buffer e-mcp--helper-stdout))
  (when (buffer-live-p e-mcp--helper-stderr)
    (kill-buffer e-mcp--helper-stderr))
  (setq e-mcp--helper-process nil)
  (setq e-mcp--helper-stdout nil)
  (setq e-mcp--helper-stderr nil)
  (setq e-mcp--helper-next-id 0)
  (setq e-mcp--latest-diagnostics nil)
  (setq e-mcp--http-sessions nil)
  (clrhash e-mcp--catalog-cache)
  t)

(defun e-mcp--catalog-key (servers)
  "Return a stable cache key for SERVERS."
  (sort (mapcar #'e-mcp-server-id servers) #'string<))

(defun e-mcp--invalidate-catalog (servers)
  "Drop any memoized catalog for SERVERS."
  (remhash (e-mcp--catalog-key servers) e-mcp--catalog-cache))

(defun e-mcp-diagnostics ()
  "Return diagnostics from the most recent MCP helper response."
  e-mcp--latest-diagnostics)

(defun e-mcp--helper-live-p ()
  "Return non-nil when the MCP helper process is live."
  (and e-mcp--helper-process
       (process-live-p e-mcp--helper-process)))

(defun e-mcp--helper-ensure ()
  "Start and return the MCP helper process."
  (unless (e-mcp--helper-live-p)
    (setq e-mcp--helper-stdout (generate-new-buffer " *e-mcp-stdout*"))
    (setq e-mcp--helper-stderr (generate-new-buffer " *e-mcp-stderr*"))
    (setq e-mcp--helper-process
          (make-process
           :name "e-mcp-helper"
           :buffer nil
           :stderr e-mcp--helper-stderr
           :command (e-mcp--helper-command)
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :filter
           (lambda (_process text)
             (when (buffer-live-p e-mcp--helper-stdout)
               (with-current-buffer e-mcp--helper-stdout
                 (goto-char (point-max))
                 (insert text))))))
    (set-process-query-on-exit-flag e-mcp--helper-process nil))
  e-mcp--helper-process)

(defun e-mcp--stderr-string ()
  "Return captured MCP helper stderr diagnostics."
  (string-trim (e-mcp--buffer-string e-mcp--helper-stderr)))

(defun e-mcp--next-id ()
  "Return next helper protocol request id."
  (setq e-mcp--helper-next-id (1+ e-mcp--helper-next-id)))

(defun e-mcp--response-for-id (id)
  "Return parsed MCP helper response for ID when available."
  (when (buffer-live-p e-mcp--helper-stdout)
    (with-current-buffer e-mcp--helper-stdout
      (save-excursion
        (goto-char (point-min))
        (let (response)
          (while (and (not response) (not (eobp)))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (unless (string-empty-p (string-trim line))
                (let ((payload (e-mcp--parse-json line)))
                  (when (equal (plist-get payload :id) id)
                    (setq response payload)))))
            (forward-line 1))
          response)))))

(defun e-mcp--truthy-p (value)
  "Return non-nil when VALUE is JSON truthy for helper protocol booleans."
  (and value (not (eq value :json-false))))

(defun e-mcp--env-entry (entry)
  "Return helper JSON shape for env ENTRY."
  (list :name (car entry) :value (cdr entry)))

(defun e-mcp--server-payload (server)
  "Return helper JSON shape for stdio SERVER."
  (append
   (list :id (e-mcp-server-id server)
         :command (vconcat (or (e-mcp-server-command server) nil))
         :env (vconcat (mapcar #'e-mcp--env-entry
                               (or (e-mcp-server-env server) nil))))
   (when (e-mcp-server-timeout server)
     (list :timeout (e-mcp-server-timeout server)))))

(defun e-mcp--servers-payload (servers)
  "Return helper JSON shape for SERVERS."
  (vconcat (mapcar #'e-mcp--server-payload servers)))

(defun e-mcp--helper-error (response fallback)
  "Signal an infrastructure error from helper RESPONSE and FALLBACK."
  (signal 'e-mcp-backend-error
          (list (or (plist-get response :error) fallback)
                (plist-get response :diagnostics))))

(defun e-mcp--helper-request (op servers &rest args)
  "Send OP for SERVERS to the helper with keyword ARGS."
  (let* ((id (e-mcp--next-id))
         (request (append (list :id id
                                :op op
                                :servers (e-mcp--servers-payload servers))
                          args))
         response)
    (setq response
          (if e-mcp-helper-transport-function
              (funcall e-mcp-helper-transport-function request)
            (let* ((process (e-mcp--helper-ensure))
                   (timeout (or (plist-get args :timeout)
                                e-mcp-helper-timeout))
                   (deadline (+ (float-time) timeout)))
              (process-send-string process (concat (json-encode request) "\n"))
              (while (and (not response)
                          (process-live-p process)
                          (< (float-time) deadline))
                (accept-process-output process 0.01)
                (setq response (e-mcp--response-for-id id)))
              (unless response
                (if (process-live-p process)
                    (signal 'e-mcp-backend-timeout
                            (list (format "MCP helper timed out after %s seconds"
                                          timeout)))
                  (signal 'e-mcp-backend-error
                          (list (string-trim
                                 (format "MCP helper exited%s"
                                         (if (string-empty-p
                                              (e-mcp--stderr-string))
                                             ""
                                           (concat ": "
                                                   (e-mcp--stderr-string)))))))))
              response)))
    (unless (and (listp response) (plist-member response :ok))
      (signal 'e-mcp-protocol-error
              (list "MCP helper returned an invalid response" response)))
    (setq e-mcp--latest-diagnostics (plist-get response :diagnostics))
    (unless (e-mcp--truthy-p (plist-get response :ok))
      (e-mcp--helper-error response "MCP helper returned an error"))
    (plist-get response :result)))

(defun e-mcp--tool-from-helper (server-id item)
  "Return an `e-mcp-tool' for SERVER-ID and helper catalog ITEM."
  (e-mcp-tool-create
   :server-id server-id
   :name (plist-get item :name)
   :description (or (plist-get item :description) "")
   :input-schema (or (plist-get item :inputSchema)
                     (plist-get item :input-schema))
   :metadata (plist-get item :metadata)))


;;; HTTP (Streamable HTTP) transport
;;
;; For servers with :url set, we talk MCP JSON-RPC directly from Emacs over
;; HTTP POST.  No helper process needed.

(defun e-mcp--http-session (server)
  "Return or create an HTTP session for SERVER."
  (let* ((id (e-mcp-server-id server))
         (existing (assoc id e-mcp--http-sessions)))
    (if existing
        (cdr existing)
      (let ((session (list :url (e-mcp-server-url server)
                           :headers (e-mcp-server-http-headers server)
                           :session-id nil
                           :initialized nil
                           :next-id 0)))
        (push (cons id session) e-mcp--http-sessions)
        session))))

(defun e-mcp--http-session-reset (server-id)
  "Remove cached HTTP session for SERVER-ID."
  (setq e-mcp--http-sessions
        (assoc-delete-all server-id e-mcp--http-sessions)))

(defun e-mcp--http-request-headers (session)
  "Return HTTP request headers for SESSION."
  (let ((headers (plist-get session :headers))
        (session-id (plist-get session :session-id))
        result)
    (push '("Content-Type" . "application/json") result)
    (push '("Accept" . "application/json, text/event-stream") result)
    (when session-id
      (push (cons "Mcp-Session-Id" session-id) result))
    (dolist (entry headers)
      (push entry result))
    (nreverse result)))

(defun e-mcp--http-next-id (session)
  "Return and increment the next JSON-RPC id for SESSION."
  (let ((id (1+ (plist-get session :next-id))))
    (plist-put session :next-id id)
    id))

(defun e-mcp--http-response-body (buffer)
  "Extract HTTP response body from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (buffer-substring-no-properties (point) (point-max)))))

(defun e-mcp--http-session-header (buffer)
  "Return the Mcp-Session-Id response header value in BUFFER, or nil.
Only the response header region (before the blank line that ends the
headers) is searched."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((header-end (save-excursion
                        (if (re-search-forward "\r?\n\r?\n" nil t)
                            (point)
                          (point-max))))
          (case-fold-search t))
      (when (re-search-forward
             "^mcp-session\\(?:-id\\)?:[ \t]*\\(.+?\\)[ \t\r]*$"
             header-end t)
        (match-string 1)))))

(defun e-mcp--http-post (session method params)
  "Send a JSON-RPC METHOD call with PARAMS to the MCP HTTP server in SESSION.
Return the parsed JSON-RPC result on success, signal on error."
  (let* ((id (e-mcp--http-next-id session))
         (url (plist-get session :url))
         (timeout (or e-mcp-helper-timeout 10))
         (payload (json-encode
                   (list :jsonrpc "2.0"
                         :id id
                         :method method
                         :params (or params (make-hash-table)))))
         (url-request-method "POST")
         (url-request-extra-headers (e-mcp--http-request-headers session))
         (url-request-data (encode-coding-string payload 'utf-8))
         (buffer (condition-case err
                     (url-retrieve-synchronously url 'silent nil timeout)
                   (error
                    (signal 'e-mcp-backend-error
                            (list (format "HTTP request to %s failed: %S" url err)))))))
    (unwind-protect
        (let* ((body (e-mcp--http-response-body buffer))
               (_ (unless (and body (not (string-empty-p (string-trim body))))
                    (signal 'e-mcp-backend-error
                            (list (format "Empty response from MCP HTTP server %s" url)))))
               (response (e-mcp--parse-json body))
               ;; Capture Mcp-Session-Id header so follow-up requests stay on
               ;; the same MCP session.
               (resp-session-id (e-mcp--http-session-header buffer)))
          (when resp-session-id
            (plist-put session :session-id resp-session-id))
          (when (plist-get response :error)
            (let ((err-obj (plist-get response :error)))
              (signal 'e-mcp-backend-error
                      (list (or (plist-get err-obj :message)
                                (format "%S" err-obj))))))
          (plist-get response :result))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun e-mcp--http-notify (session method params)
  "Send a JSON-RPC notification (no id, no response expected) to SESSION."
  (let* ((url (plist-get session :url))
         (payload (json-encode
                   (list :jsonrpc "2.0"
                         :method method
                         :params (or params (make-hash-table)))))
         (url-request-method "POST")
         (url-request-extra-headers (e-mcp--http-request-headers session))
         (url-request-data (encode-coding-string payload 'utf-8))
         (buffer (ignore-errors
                   (url-retrieve-synchronously url 'silent nil 5))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun e-mcp--http-initialize (session)
  "Send MCP initialize and initialized notification for SESSION."
  (unless (plist-get session :initialized)
    (e-mcp--http-post session "initialize"
                      (list :protocolVersion "2024-11-05"
                            :capabilities nil
                            :clientInfo (list :name "e-mcp" :version "0.1.0")))
    (e-mcp--http-notify session "notifications/initialized" nil)
    (plist-put session :initialized t)))

(defun e-mcp--http-list-tools (server)
  "Return MCP tools for HTTP SERVER."
  (let ((session (e-mcp--http-session server)))
    (e-mcp--http-initialize session)
    (let ((result (e-mcp--http-post session "tools/list" nil)))
      (mapcar
       (lambda (item)
         (e-mcp--tool-from-helper (e-mcp-server-id server) item))
       (append (plist-get result :tools) nil)))))

(defun e-mcp--http-call-tool (server tool-name arguments)
  "Call TOOL-NAME with ARGUMENTS on HTTP SERVER."
  (let ((session (e-mcp--http-session server)))
    (e-mcp--http-initialize session)
    (e-mcp--http-post session "tools/call"
                      (list :name tool-name
                            :arguments (or arguments nil)))))

(defun e-mcp--http-refresh (server)
  "Refresh tool catalog for HTTP SERVER."
  (e-mcp--http-session-reset (e-mcp-server-id server))
  (e-mcp--http-list-tools server))

(defun e-mcp--server-http-p (server)
  "Return non-nil when SERVER uses HTTP transport."
  (and (e-mcp-server-url server) t))

(defun e-mcp--list-tools-uncached (servers)
  "Return freshly discovered MCP tools for SERVERS, bypassing the cache.
HTTP servers are handled in-process; stdio servers use the helper."
  (let ((http-servers (cl-remove-if-not #'e-mcp--server-http-p servers))
        (stdio-servers (cl-remove-if #'e-mcp--server-http-p servers))
        tools)
    ;; HTTP transport: direct Elisp
    (dolist (server http-servers)
      (setq tools (append tools (e-mcp--http-list-tools server))))
    ;; stdio transport: Node helper
    (when stdio-servers
      (let* ((result (e-mcp--helper-request "list-tools" stdio-servers))
             (fallback-server-id (when (= (length stdio-servers) 1)
                                   (e-mcp-server-id (car stdio-servers)))))
        (dolist (item (append (plist-get result :tools) nil))
          (let ((server-id (or (plist-get item :serverId)
                               (plist-get item :server-id)
                               fallback-server-id)))
            (unless server-id
              (signal 'e-mcp-protocol-error
                      (list "MCP helper omitted server id from multi-server catalog"
                            item)))
            (push (e-mcp--tool-from-helper server-id item) tools)))))
    (nreverse tools)))

(defun e-mcp-list-tools (servers)
  "Return discovered MCP tools for SERVERS.
The catalog is memoized per server set; `e-mcp-refresh' or `e-mcp-reset'
invalidate it."
  (let ((key (e-mcp--catalog-key servers)))
    (if-let ((cached (gethash key e-mcp--catalog-cache)))
        cached
      (puthash key (e-mcp--list-tools-uncached servers) e-mcp--catalog-cache))))

(defun e-mcp-call-tool (servers server-id tool-name arguments)
  "Call TOOL-NAME on SERVER-ID through SERVERS with ARGUMENTS."
  (let ((server (cl-find server-id servers
                         :key #'e-mcp-server-id :test #'equal)))
    (unless server
      (signal 'e-mcp-backend-error
              (list (format "Unknown MCP server: %s" server-id))))
    (if (e-mcp--server-http-p server)
        (e-mcp--http-call-tool server tool-name arguments)
      (e-mcp--helper-request "call-tool"
                             (cl-remove-if #'e-mcp--server-http-p servers)
                             :server server-id
                             :tool tool-name
                             :arguments arguments))))

(defun e-mcp-refresh (&optional servers)
  "Refresh tool catalogs for SERVERS.
HTTP servers are refreshed in-process; stdio servers use the helper.
Interactively, refresh all servers seen during capability construction."
  (interactive)
  (let ((servers (or servers e-mcp--known-servers)))
    (unless servers
      (signal 'e-mcp-backend-error
              (list "No MCP servers are configured for refresh")))
    (e-mcp--invalidate-catalog servers)
    (let ((http-servers (cl-remove-if-not #'e-mcp--server-http-p servers))
          (stdio-servers (cl-remove-if #'e-mcp--server-http-p servers)))
      (dolist (server http-servers)
        (e-mcp--http-refresh server))
      (when stdio-servers
        (e-mcp--helper-request "refresh" stdio-servers)))))

(defun e-mcp--generated-tool-name (tool)
  "Return the generated e tool name for MCP TOOL.
Uses `__' separators (not dots) so the name satisfies provider tool-name
constraints such as Anthropic/Bedrock's `[a-zA-Z0-9_-]+' pattern.  The name
is only a dispatch key; MCP routing uses the tool metadata, never a parse of
this string."
  (format "mcp__%s__%s"
          (e-mcp-tool-server-id tool)
          (e-mcp-tool-name tool)))

(defun e-mcp--tool-metadata (tool)
  "Return metadata for generated e TOOL."
  (list :kind 'mcp-tool
        :server-id (e-mcp-tool-server-id tool)
        :tool-name (e-mcp-tool-name tool)))

(defun e-mcp--content-block-text (block)
  "Return model-visible text for MCP content BLOCK."
  (pcase (plist-get block :type)
    ("text"
     (or (plist-get block :text) ""))
    (type
     (format "[Unsupported MCP content block: %s]"
             (or type "unknown")))))

(defun e-mcp--content-text (content)
  "Return model-visible text for MCP CONTENT blocks."
  (string-join
   (delq nil
         (mapcar #'e-mcp--content-block-text (append content nil)))
   "\n"))

(defun e-mcp--result-content (mcp-result)
  "Return e tool content mapped from MCP-RESULT."
  (let* ((has-structured (plist-member mcp-result :structuredContent))
         (structured (plist-get mcp-result :structuredContent))
         (text (e-mcp--content-text (plist-get mcp-result :content))))
    (cond
     ((and has-structured (not (string-empty-p text)))
      (list :content text :structuredContent structured))
     (has-structured
      structured)
     (t text))))

(defun e-mcp--result-error-p (mcp-result)
  "Return non-nil when MCP-RESULT is an MCP execution error."
  (e-mcp--truthy-p (plist-get mcp-result :isError)))

(defun e-mcp--tool-handler (servers tool)
  "Return a generated handler for MCP TOOL through SERVERS."
  (lambda (arguments)
    (let* ((call (plist-get (e-tools-current-context) :tool-call))
           (mcp-result (e-mcp-call-tool
                        servers
                        (e-mcp-tool-server-id tool)
                        (e-mcp-tool-name tool)
                        arguments))
           (content (e-mcp--result-content mcp-result))
           (metadata (e-mcp--tool-metadata tool)))
      (if (e-mcp--result-error-p mcp-result)
          (e-tools-result-create
           call
           'error
           content
           (append metadata (list :error 'mcp-execution-error)))
        (e-tools-result-create call 'ok content metadata)))))

(defun e-mcp--register-tool (registry servers tool)
  "Register generated e TOOL in REGISTRY for SERVERS."
  (e-tools-register
   registry
   :name (e-mcp--generated-tool-name tool)
   :description (format "[MCP %s] %s"
                        (e-mcp-tool-server-id tool)
                        (e-mcp-tool-description tool))
   :parameters (e-mcp-tool-input-schema tool)
   :metadata (e-mcp--tool-metadata tool)
   :handler (e-mcp--tool-handler servers tool)))

(defun e-mcp--remember-servers (servers)
  "Remember SERVERS for interactive refresh without duplicates."
  (dolist (server servers)
    (unless (cl-find (e-mcp-server-id server)
                     e-mcp--known-servers
                     :key #'e-mcp-server-id
                     :test #'equal)
      (setq e-mcp--known-servers
            (append e-mcp--known-servers (list server))))))

(defun e-mcp--known-server (server-id)
  "Return the remembered `e-mcp-server' for SERVER-ID, or nil."
  (cl-find server-id e-mcp--known-servers
           :key #'e-mcp-server-id :test #'equal))

;;; Progressive disclosure (Tier 0/1/2)
;;
;; Eager mode (the default) registers every MCP tool's full schema into the
;; per-turn tool set, paying its context cost on every turn.  Progressive mode
;; splits "a tool family exists" from "the full contract for calling it":
;;
;;   Tier 0  one tiny capability card per server, injected into context.
;;   Tier 1  full schemas as on-demand e:// resources, read not injected.
;;   Tier 2  an mcp_activate meta-tool that returns schemas and promotes the
;;           requested tools into the session's active tool set.
;;
;; The three tiers are coherent only together: cards without lazy tools double
;; the cost, lazy tools without cards leave the model unable to discover the
;; family.  They are therefore gated by one per-capability `:progressive' flag
;; rather than three independent switches.  With the flag off, this module
;; behaves exactly as it did before progressive disclosure existed.

(defconst e-mcp--progressive-config-options
  (list (e-capability-config-option-create
         :key :progressive
         :type 'boolean
         :default nil
         :documentation
         "When non-nil, expose MCP tools through progressive disclosure: a tiny per-server capability card in context (Tier 0), full schemas as on-demand e:// resources (Tier 1), and an mcp_activate meta-tool that promotes tools into the active set (Tier 2).  When nil, every tool schema is injected eagerly."))
  "Config option specs every MCP-wrapping capability owns.")

(cl-defun e-mcp--config
    (capability-id config-options &key harness session-id directory overrides)
  "Resolve CAPABILITY-ID config against CONFIG-OPTIONS.
HARNESS, when present, contributes session-scoped runtime config."
  (if harness
      (e-harness-effective-capability-config
       harness capability-id config-options
       :session-id session-id :directory directory :overrides overrides)
    (e-capability-config-resolve
     capability-id config-options :directory directory :overrides overrides)))

(cl-defun e-mcp--progressive-p
    (capability-id config-options &key harness session-id)
  "Return non-nil when CAPABILITY-ID is configured for progressive disclosure."
  (plist-get (e-mcp--config capability-id config-options
                            :harness harness :session-id session-id)
             :progressive))

;;; Tier 2 — active tool set (persisted in session metadata)

(defun e-mcp--active-set (harness session-id)
  "Return the activated-MCP-tools alist for HARNESS SESSION-ID.
Each entry is (SERVER-ID . TOOLS) where TOOLS is t (all tools) or a list of
tool-name strings."
  (when (and harness session-id)
    (when-let ((session (ignore-errors
                          (e-session-get (e-harness-sessions harness)
                                         session-id))))
      (plist-get (plist-get session :metadata) :mcp-active))))

(defun e-mcp--tool-activated-p (active server-id tool-name)
  "Return non-nil when TOOL-NAME of SERVER-ID is activated in ACTIVE."
  (let ((tools (cdr (assoc server-id active))))
    (or (eq tools t)
        (and (listp tools) (member tool-name tools) t))))

(defun e-mcp--merge-active-tools (existing tool-names)
  "Merge TOOL-NAMES into EXISTING activation for one server.
Nil or empty TOOL-NAMES means \"all tools\"."
  (cond
   ((eq existing t) t)
   ((null tool-names) t)
   (t (cl-remove-duplicates
       (append (and (listp existing) existing) tool-names)
       :test #'equal))))

(defun e-mcp--activate (harness session-id server-id tool-names)
  "Promote TOOL-NAMES of SERVER-ID into HARNESS SESSION-ID active set.
TOOL-NAMES nil or empty activates the whole server.  Returns the merged value."
  (let* ((store (e-harness-sessions harness))
         (metadata (copy-sequence
                    (plist-get (e-session-get store session-id) :metadata)))
         (active (copy-alist (plist-get metadata :mcp-active)))
         (existing (cdr (assoc server-id active)))
         (merged (e-mcp--merge-active-tools existing (append tool-names nil))))
    (setf (alist-get server-id active nil nil #'equal) merged)
    (e-session-set-metadata store session-id
                            (plist-put metadata :mcp-active active))
    merged))

;;; Tier 1 — schema text and on-demand resources

(defun e-mcp--tool-schema-text (tool)
  "Return the full on-demand schema document for TOOL.
The callable tool name is the generated name the model invokes later."
  (string-join
   (list (format "# %s" (e-mcp--generated-tool-name tool))
         (e-mcp-tool-description tool)
         ""
         "Input schema (JSON):"
         (json-encode (e-mcp-tool-input-schema tool)))
   "\n"))

(defun e-mcp--family-index-text (server-id catalog)
  "Return the Tier-1 family index document for SERVER-ID CATALOG."
  (string-join
   (cons
    (format "# MCP %s — %d tools" server-id (length catalog))
    (mapcar
     (lambda (tool)
       (format "- %s: %s"
               (e-mcp-tool-name tool)
               (e-mcp-tool-description tool)))
     catalog))
   "\n"))

(defun e-mcp--register-resources (store capability servers)
  "Register Tier-1 MCP schema resources for SERVERS in STORE under CAPABILITY."
  (let ((capability-id (e-capability-id capability)))
    (dolist (server servers)
      (let* ((server-id (e-mcp-server-id server))
             (catalog (e-mcp-list-tools (list server))))
        (e-store-register
         store capability-id
         (format "mcp/%s/tools" server-id)
         :description (format "MCP %s tool index." server-id)
         :content (e-mcp--family-index-text server-id catalog)
         :metadata (list :kind 'mcp-tool-index :server-id server-id))
        (dolist (tool catalog)
          (e-store-register
           store capability-id
           (format "mcp/%s/tools/%s" server-id (e-mcp-tool-name tool))
           :description (format "MCP %s/%s full schema."
                                server-id (e-mcp-tool-name tool))
           :content (e-mcp--tool-schema-text tool)
           :metadata (e-mcp--tool-metadata tool)))))))

(defun e-mcp--resource-provider (servers capability-id all-options)
  "Return a resource provider registering Tier-1 resources for SERVERS.
Resources are registered only when CAPABILITY-ID resolves to progressive mode
against ALL-OPTIONS."
  (cl-function
   (lambda (store capability &key harness session-id &allow-other-keys)
     (when (e-mcp--progressive-p capability-id all-options
                                 :harness harness :session-id session-id)
       (e-mcp--register-resources store capability servers)))))

;;; Tier 0 — capability cards

(defun e-mcp--server-card-text (capability-id server catalog)
  "Return the Tier-0 capability card for SERVER CATALOG under CAPABILITY-ID."
  (let ((server-id (e-mcp-server-id server)))
    (string-join
     (list
      (format "%s (MCP, %d tools)" server-id (length catalog))
      (format "Load before use: read e://%s/mcp/%s/tools  (or call mcp_activate server=\"%s\")"
              capability-id server-id server-id)
      (format "Tools: %s"
              (string-join (mapcar #'e-mcp-tool-name catalog) ", ")))
     "\n")))

(defun e-mcp--cards-message (capability-id servers)
  "Return a single Tier-0 context message describing SERVERS for CAPABILITY-ID."
  (let ((cards (mapcar
                (lambda (server)
                  (e-mcp--server-card-text
                   capability-id server (e-mcp-list-tools (list server))))
                servers)))
    (when cards
      (list
       (list :role 'system
             :content
             (string-join
              (cons
               "MCP tool families available this session (schemas load on demand):"
               cards)
              "\n\n"))))))

(defun e-mcp--context-provider (servers capability-id all-options)
  "Return a Tier-0 card context provider for SERVERS.
Cards are emitted only when CAPABILITY-ID resolves to progressive mode."
  (e-context-provider-create
   :name (intern (format "mcp-cards-%s" capability-id))
   :priority 210
   :cache-placement 'stable-context
   :build
   (cl-function
    (lambda (&key harness session-id _turn-id)
      (when (e-mcp--progressive-p capability-id all-options
                                  :harness harness :session-id session-id)
        (e-mcp--cards-message capability-id servers))))))

;;; Tier 2 — mcp_activate meta-tool

(defconst e-mcp--activate-tool-parameters
  '(:type "object"
    :properties (:server (:type "string"
                          :description "MCP server id to activate.")
                 :tools (:type "array"
                         :items (:type "string")
                         :description "Tool names to activate; omit for all.")
                 :invoke (:type "object"
                          :description "Optional single tool to call immediately."
                          :properties (:tool (:type "string")
                                       :arguments (:type "object"))))
    :required ["server"])
  "Schema for the always-present mcp_activate meta-tool.")

(defun e-mcp--catalog-for-server (server-id)
  "Return (SERVER . CATALOG) for SERVER-ID from remembered servers, or nil."
  (when-let ((server (e-mcp--known-server server-id)))
    (cons server (e-mcp-list-tools (list server)))))

(defun e-mcp--select-tools (catalog tool-names)
  "Return CATALOG entries whose names are in TOOL-NAMES, or all when empty."
  (let ((names (append tool-names nil)))
    (if (null names)
        catalog
      (cl-remove-if-not
       (lambda (tool) (member (e-mcp-tool-name tool) names))
       catalog))))

(defun e-mcp--activate-handler (arguments)
  "Handle an mcp_activate call described by ARGUMENTS."
  (let* ((context (e-tools-current-context))
         (call (plist-get context :tool-call))
         (harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (server-id (plist-get arguments :server))
         (requested (plist-get arguments :tools))
         (invoke (plist-get arguments :invoke))
         (server+catalog (and server-id (e-mcp--catalog-for-server server-id))))
    (unless server+catalog
      (signal 'e-mcp-protocol-error
              (list (format "Unknown MCP server: %s" server-id))))
    (let* ((server (car server+catalog))
           (catalog (cdr server+catalog))
           (selected (e-mcp--select-tools catalog requested))
           (schema-text (string-join
                         (mapcar #'e-mcp--tool-schema-text selected)
                         "\n\n"))
           (sections (list schema-text)))
      (when (and harness session-id)
        (e-mcp--activate harness session-id server-id
                         (mapcar #'e-mcp-tool-name selected))
        (push "Activated for this session; the tools above are now callable."
              sections))
      (when invoke
        (let* ((tool-name (plist-get invoke :tool))
               (invoke-args (plist-get invoke :arguments))
               (result (e-mcp-call-tool (list server) server-id
                                        tool-name invoke-args)))
          (push (format "Invoke %s result:\n%s"
                        tool-name
                        (e-tools-result-content-text
                         (e-mcp--result-content result)))
                sections)))
      (let ((content (string-join (nreverse sections) "\n\n")))
        (if call
            (e-tools-result-create call 'ok content (list :kind 'mcp-activate))
          content)))))

(defun e-mcp--register-meta-tool (registry)
  "Register the always-present mcp_activate meta-tool in REGISTRY."
  (e-tools-register
   registry
   :name "mcp_activate"
   :description
   "Load full schemas for MCP tools and make them callable for the rest of the session. Pass `server' and optionally `tools' (omit for all). The result returns the full schemas; the named tools become callable on the next turn. Optionally pass `invoke' {tool, arguments} to also call one tool immediately."
   :parameters e-mcp--activate-tool-parameters
   :handler #'e-mcp--activate-handler
   :metadata '(:kind mcp-activate)))

(defun e-mcp--tools-provider (servers capability-id all-options)
  "Return a tools provider for SERVERS.
In eager mode it registers every tool.  In progressive mode it registers the
mcp_activate meta-tool plus only the tools the session has activated."
  (cl-function
   (lambda (registry &key harness session-id &allow-other-keys)
     (if (e-mcp--progressive-p capability-id all-options
                               :harness harness :session-id session-id)
         (progn
           (e-mcp--register-meta-tool registry)
           (let ((active (e-mcp--active-set harness session-id)))
             (dolist (tool (e-mcp-list-tools servers))
               (when (e-mcp--tool-activated-p
                      active (e-mcp-tool-server-id tool) (e-mcp-tool-name tool))
                 (e-mcp--register-tool registry servers tool)))))
       (dolist (tool (e-mcp-list-tools servers))
         (e-mcp--register-tool registry servers tool))))))

(cl-defun e-capability-with-mcp-create
    (&key id name instructions mcp-servers tools resource-methods resources
          context-providers actions instruction-priority config-options config)
  "Create an ordinary capability that wraps configured MCP servers.
MCP-SERVERS are construction-time `e-mcp-server' values.  Discovered MCP tools
are registered as ordinary e tools under deterministic names."
  (dolist (server mcp-servers)
    (unless (e-mcp-server-p server)
      (signal 'wrong-type-argument (list 'e-mcp-server-p server))))
  (e-mcp--remember-servers mcp-servers)
  (let* ((all-options (append e-mcp--progressive-config-options config-options))
         (mcp-tools (when mcp-servers
                      (e-mcp--tools-provider mcp-servers id all-options)))
         (mcp-resources (when mcp-servers
                          (e-mcp--resource-provider mcp-servers id all-options)))
         (mcp-cards (when mcp-servers
                      (e-mcp--context-provider mcp-servers id all-options))))
    (when mcp-servers
      (e-capability-config-register-options id all-options))
    (e-capability-create
     :id id
     :name name
     :instructions instructions
     :tools (append tools (when mcp-tools (list mcp-tools)))
     :resource-methods resource-methods
     :resources (append resources (when mcp-resources (list mcp-resources)))
     :context-providers (append context-providers
                                (when mcp-cards (list mcp-cards)))
     :instruction-priority instruction-priority
     :actions actions
     :config-options all-options
     :config config)))

(provide 'e-mcp)

;;; e-mcp.el ends here

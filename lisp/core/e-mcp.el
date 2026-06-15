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
(require 'e-tools)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)

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
  t)

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

(defvar e-mcp--http-sessions nil
  "Alist of (server-id . session-plist) for HTTP MCP sessions.
Each session plist tracks :url, :headers, :session-id, :initialized.")

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

(defun e-mcp-list-tools (servers)
  "Return discovered MCP tools for SERVERS.
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
  (let ((mcp-tools
         (lambda (registry)
           (dolist (tool (e-mcp-list-tools mcp-servers))
             (e-mcp--register-tool registry mcp-servers tool)))))
    (e-capability-create
     :id id
     :name name
     :instructions instructions
     :tools (append tools (when mcp-servers (list mcp-tools)))
     :resource-methods resource-methods
     :resources resources
     :context-providers context-providers
     :instruction-priority instruction-priority
     :actions actions
     :config-options config-options
     :config config)))

(provide 'e-mcp)

;;; e-mcp.el ends here

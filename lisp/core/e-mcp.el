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
                             (&key id command env timeout)))
  id
  command
  env
  timeout)

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
  "Create an MCP server spec from keyword ARGS."
  (let* ((server (apply #'e-mcp-server--create args))
         (id (e-mcp-server-id server))
         (command (e-mcp-server-command server)))
    (e-mcp--non-empty-string id 'mcp-server-id)
    (unless (e-mcp--valid-command-p command)
      (signal 'wrong-type-argument (list 'mcp-server-command command)))
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
  "Return helper JSON shape for SERVER."
  (append
   (list :id (e-mcp-server-id server)
         :command (vconcat (e-mcp-server-command server))
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

(defun e-mcp-list-tools (servers)
  "Return discovered MCP tools for SERVERS."
  (let* ((result (e-mcp--helper-request "list-tools" servers))
         (fallback-server-id (when (= (length servers) 1)
                               (e-mcp-server-id (car servers)))))
    (mapcar
     (lambda (item)
       (let ((server-id (or (plist-get item :serverId)
                            (plist-get item :server-id)
                            fallback-server-id)))
         (unless server-id
           (signal 'e-mcp-protocol-error
                   (list "MCP helper omitted server id from multi-server catalog"
                         item)))
         (e-mcp--tool-from-helper server-id item)))
     (append (plist-get result :tools) nil))))

(defun e-mcp-call-tool (servers server-id tool-name arguments)
  "Call TOOL-NAME on SERVER-ID through SERVERS with ARGUMENTS."
  (e-mcp--helper-request "call-tool"
                         servers
                         :server server-id
                         :tool tool-name
                         :arguments arguments))

(defun e-mcp-refresh (&optional servers)
  "Ask the MCP helper to refresh tool catalogs for SERVERS.
Interactively, refresh all servers seen during capability construction."
  (interactive)
  (let ((servers (or servers e-mcp--known-servers)))
    (unless servers
      (signal 'e-mcp-backend-error
              (list "No MCP servers are configured for refresh")))
    (e-mcp--helper-request "refresh" servers)))

(defun e-mcp--generated-tool-name (tool)
  "Return the generated e tool name for MCP TOOL."
  (format "mcp.%s.%s"
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

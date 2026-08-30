;;; e-mcp-protocol.el --- Stable MCP values and validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This small domain contract contains only the typed server/tool values and
;; protocol errors shared by the two supported transports.  Transport state
;; and capability/session state live in their respective owners.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(define-error 'e-mcp-backend-error "MCP helper backend error")
(define-error 'e-mcp-backend-timeout "MCP helper backend timeout"
  'e-mcp-backend-error)
(define-error 'e-mcp-protocol-error "MCP helper protocol error"
  'e-mcp-backend-error)
(define-error 'e-mcp-backpressure "Too many concurrent MCP requests"
  'e-mcp-backend-error)

(dolist (condition '(e-mcp-backend-error
                     e-mcp-backend-timeout
                     e-mcp-protocol-error
                     e-mcp-backpressure))
  (put condition 'e-tools-infrastructure-error t))

(cl-defstruct (e-mcp-server
               (:constructor e-mcp-protocol--server-create
                             (&key id command env timeout url http-headers)))
  id command env timeout
  ;; URL selects the streamable HTTP transport; COMMAND/ENV select stdio.
  url http-headers)

(cl-defstruct (e-mcp-tool
               (:constructor e-mcp-protocol--tool-create
                             (&key server-id name description input-schema
                                   metadata)))
  server-id name description input-schema metadata)

(defun e-mcp-protocol--non-empty-string (value label)
  "Return VALUE when it is a non-empty string for LABEL."
  (unless (and (stringp value) (not (string-empty-p value)))
    (signal 'wrong-type-argument (list label value)))
  value)

(defun e-mcp-protocol--valid-command-p (command)
  "Return non-nil when COMMAND is a non-empty string list."
  (and (consp command)
       (cl-every (lambda (part)
                   (and (stringp part) (not (string-empty-p part))))
                 command)))

(defun e-mcp-protocol--json-object-p (value)
  "Return non-nil when VALUE is an Emacs JSON object representation."
  (or (hash-table-p value)
      (and (listp value)
           (cl-evenp (length value))
           (cl-loop for (key _item) on value by #'cddr
                    always (keywordp key)))))

(defun e-mcp-server-create (&rest args)
  "Create an MCP server spec from keyword ARGS.
A server must specify either COMMAND (stdio) or URL (HTTP), but not both."
  (let* ((server (apply #'e-mcp-protocol--server-create args))
         (id (e-mcp-server-id server))
         (command (e-mcp-server-command server))
         (url (e-mcp-server-url server)))
    (e-mcp-protocol--non-empty-string id 'mcp-server-id)
    (cond
     ((and url command)
      (signal 'wrong-type-argument
              (list 'mcp-server-transport
                    "specify either :command or :url, not both")))
     (url
      (unless (and (stringp url) (not (string-empty-p url)))
        (signal 'wrong-type-argument (list 'mcp-server-url url))))
     (t
      (unless (e-mcp-protocol--valid-command-p command)
        (signal 'wrong-type-argument
                (list 'mcp-server-command command)))))
    server))

(defun e-mcp-tool-create (&rest args)
  "Create an MCP tool catalog entry from keyword ARGS."
  (let* ((tool (apply #'e-mcp-protocol--tool-create args))
         (server-id (e-mcp-tool-server-id tool))
         (name (e-mcp-tool-name tool))
         (schema (e-mcp-tool-input-schema tool)))
    (e-mcp-protocol--non-empty-string server-id 'mcp-server-id)
    (e-mcp-protocol--non-empty-string name 'mcp-tool-name)
    (unless (e-mcp-protocol--json-object-p schema)
      (signal 'wrong-type-argument (list 'mcp-input-schema schema)))
    tool))

(defun e-mcp-protocol-tool-from-wire (server-id item)
  "Return an `e-mcp-tool' for SERVER-ID and wire catalog ITEM."
  (e-mcp-tool-create
   :server-id server-id
   :name (plist-get item :name)
   :description (or (plist-get item :description) "")
   :input-schema (or (plist-get item :inputSchema)
                     (plist-get item :input-schema))
   :metadata (plist-get item :metadata)))

(defun e-mcp-protocol-truthy-p (value)
  "Return non-nil when VALUE is JSON truthy for MCP protocol values."
  (and value (not (eq value :json-false))))

(defun e-mcp-protocol-http-server-p (server)
  "Return non-nil when SERVER's declared transport is streamable HTTP."
  (and (e-mcp-server-p server)
       (e-mcp-server-url server)
       t))

(provide 'e-mcp-protocol)

;;; e-mcp-protocol.el ends here

;;; e-mcp-layer.el --- Register MCP servers as e layers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Reusable glue for exposing MCP servers to the runtime as an ordinary layer.
;; `e-mcp-layer-register' wraps `e-capability-with-mcp-create' in a registered
;; layer spec so a single call gives a configured MCP server tools in the
;; default chat harness.  Configuration files only describe their servers; the
;; layer plumbing lives here.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-layers)
(require 'e-mcp)

(declare-function e-capability-with-mcp-create "e-mcp")

(defun e-mcp-layer--enable-by-default (id)
  "Append ID to `e-default-chat-layer-ids' so default chat harnesses load it."
  (require 'e-default-harnesses)
  (when (boundp 'e-default-chat-layer-ids)
    (add-to-list 'e-default-chat-layer-ids id t)))

(cl-defun e-mcp-layer-register (&key id name summary servers default)
  "Register a layer exposing MCP SERVERS as e tools and return its layer spec.

ID is a symbol naming the layer.  NAME is its display name (defaults to a
string form of ID).  SUMMARY is an optional one-line description.  SERVERS is a
list of `e-mcp-server' values; their discovered tools are registered as
ordinary e tools named `mcp__<server-id>__<tool>'.  When DEFAULT is non-nil,
ID is appended to `e-default-chat-layer-ids' so default chat harnesses activate
the layer.

The capability shares ID's symbol name, and SERVERS are captured by the layer
factory, so callers may read credentials at call time without leaking them into
the layer registry."
  (unless (symbolp id)
    (signal 'wrong-type-argument (list 'symbolp id)))
  (dolist (server servers)
    (unless (e-mcp-server-p server)
      (signal 'wrong-type-argument (list 'e-mcp-server-p server))))
  (let* ((display-name (or name (symbol-name id)))
         (capability-id (symbol-name id))
         (factory
          (lambda ()
            (e-layer-create
             :id id
             :name display-name
             :capabilities
             (list (e-capability-with-mcp-create
                    :id capability-id
                    :name display-name
                    :mcp-servers servers))))))
    (prog1
        (e-layer-register
         (e-layer-spec-create
          :id id
          :name display-name
          :summary summary
          :factory factory))
      (when default
        (e-mcp-layer--enable-by-default id)))))

(provide 'e-mcp-layer)

;;; e-mcp-layer.el ends here

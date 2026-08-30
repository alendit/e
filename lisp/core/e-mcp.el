;;; e-mcp.el --- MCP public facade and composition root -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public MCP entry point.  The protocol values, transports, client catalog,
;; and capability composition are loaded in dependency order; this root owns
;; only cross-owner reset/diagnostic composition.

;;; Code:

(require 'e-mcp-protocol)
(require 'e-mcp-transport)
(require 'e-mcp-stdio)
(require 'e-mcp-http)
(require 'e-mcp-client)
(require 'e-mcp-capability)

(defun e-mcp-reset ()
  "Reset MCP capability, catalog, and transport runtime state."
  (e-mcp-client-reset)
  (e-mcp-stdio-reset)
  (e-mcp-http-reset)
  (e-mcp-transport-reset)
  t)

(defun e-mcp-diagnostics ()
  "Return diagnostics from the most recent MCP helper response."
  (e-mcp-stdio-diagnostics))

(provide 'e-mcp)

;;; e-mcp.el ends here

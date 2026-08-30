;;; e-mcp-transport.el --- Shared MCP request admission -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The two MCP transports share one bounded in-flight request budget.  This is
;; the only shared transport state; protocol values, client catalogs, and
;; capability state remain in their own owners.

;;; Code:

(require 'cl-lib)
(require 'e-mcp-protocol)

(defcustom e-mcp-max-concurrent-requests 8
  "Maximum number of in-flight async MCP transport requests.
This cap covers stdio helper requests and HTTP JSON-RPC POST requests.  Set to
nil to disable the cap."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum in-flight requests"))
  :group 'e)

(defvar e-mcp-transport--active-request-count 0
  "Number of async MCP transport requests currently in flight.")

(defun e-mcp-transport-reserve (family)
  "Reserve one async MCP transport slot for FAMILY."
  (when (and (integerp e-mcp-max-concurrent-requests)
             (>= e-mcp-transport--active-request-count
                 e-mcp-max-concurrent-requests))
    (signal 'e-mcp-backpressure
            (list (format "Too many concurrent MCP requests: %s"
                          e-mcp-transport--active-request-count)
                  family e-mcp-max-concurrent-requests)))
  (cl-incf e-mcp-transport--active-request-count)
  family)

(defun e-mcp-transport-release (&optional _family)
  "Release one async MCP transport slot."
  (setq e-mcp-transport--active-request-count
        (max 0 (1- e-mcp-transport--active-request-count))))

(defun e-mcp-transport-reset ()
  "Reset the shared MCP transport request budget."
  (setq e-mcp-transport--active-request-count 0)
  t)

(provide 'e-mcp-transport)

;;; e-mcp-transport.el ends here

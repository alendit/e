;;; e-mcp-codex.el --- Read MCP servers from a Codex config.toml -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Build `e-mcp-server' specs from an existing Codex `config.toml'.  This lets a
;; user point e at the same MCP servers Codex already uses without copying
;; credentials into Emacs configuration: the token stays in `~/.codex' and is
;; read at call time.
;;
;; Only the small slice of TOML that Codex MCP server blocks use is parsed:
;;
;;   [mcp_servers.NAME]            url = "..."  (HTTP transport)
;;   [mcp_servers.NAME.http_headers]            Header = "Value"
;;   [mcp_servers.NAME]            command = "..."  (stdio transport)
;;   [mcp_servers.NAME]            args = ["..."]
;;   [mcp_servers.NAME.env]        KEY = "Value"

;;; Code:

(require 'cl-lib)
(require 'e-mcp)
(require 'subr-x)

(defcustom e-mcp-codex-config-file "~/.codex/config.toml"
  "Path to the Codex `config.toml' read by `e-mcp-codex-server'."
  :type 'file
  :group 'e)

(defun e-mcp-codex--unquote (value)
  "Return TOML string VALUE without surrounding quotes.
Handles both basic (\") and literal (') TOML strings."
  (let ((trimmed (string-trim value)))
    (if (and (>= (length trimmed) 2)
             (or (and (string-prefix-p "\"" trimmed)
                      (string-suffix-p "\"" trimmed))
                 (and (string-prefix-p "'" trimmed)
                      (string-suffix-p "'" trimmed))))
        (substring trimmed 1 -1)
      trimmed)))

(defun e-mcp-codex--parse-array (value)
  "Return the strings of a single-line TOML array VALUE like [\"a\", \"b\"]."
  (let ((inner (string-trim value)))
    (when (and (string-prefix-p "[" inner) (string-suffix-p "]" inner))
      (setq inner (string-trim (substring inner 1 -1))))
    (if (string-empty-p inner)
        nil
      (mapcar (lambda (item) (e-mcp-codex--unquote item))
              (split-string inner "," t "[ \t]+")))))

(defun e-mcp-codex--section-bounds (section)
  "Return (START . END) buffer positions for TOML SECTION header, or nil.
START is the line after the header; END is the start of the next `[' header
or `point-max'.  Point is searched from `point-min'."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^[ \t]*\\[" (regexp-quote section) "\\][ \t]*$")
         nil t)
    (forward-line 1)
    (let ((start (point))
          (end (if (re-search-forward "^[ \t]*\\[" nil t)
                   (line-beginning-position)
                 (point-max))))
      (cons start end))))

(defun e-mcp-codex--section-pairs (section)
  "Return an alist of KEY . RAW-VALUE strings under TOML SECTION, or nil."
  (when-let ((bounds (e-mcp-codex--section-bounds section)))
    (let (pairs)
      (save-excursion
        (goto-char (car bounds))
        (while (< (point) (cdr bounds))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (string-match "^[ \t]*\\([^#=]+?\\)[ \t]*=[ \t]*\\(.*\\)$" line)
              (push (cons (string-trim (match-string 1 line))
                          (string-trim (match-string 2 line)))
                    pairs)))
          (forward-line 1)))
      (nreverse pairs))))

(defun e-mcp-codex--header-pairs (section)
  "Return SECTION pairs with quotes stripped from each value."
  (mapcar (lambda (pair)
            (cons (car pair) (e-mcp-codex--unquote (cdr pair))))
          (e-mcp-codex--section-pairs section)))

(cl-defun e-mcp-codex-server (name &key id timeout)
  "Return an `e-mcp-server' built from Codex config block NAME, or nil.

NAME is the Codex server name (the part after `mcp_servers.').  ID is the
e-side server id and defaults to NAME.  TIMEOUT, when non-nil, sets the stdio
helper timeout.  Returns nil when `e-mcp-codex-config-file' is unreadable or
has no such block, so callers can degrade gracefully.

HTTP (`url') and stdio (`command') transports are both recognized; an `url'
block carries its `http_headers', a `command' block its `args' and `env'."
  (let ((file (expand-file-name e-mcp-codex-config-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let* ((server-section (concat "mcp_servers." name))
               (pairs (e-mcp-codex--section-pairs server-section)))
          (when pairs
            (let ((url (cdr (assoc "url" pairs)))
                  (command-raw (cdr (assoc "command" pairs)))
                  (args-raw (cdr (assoc "args" pairs)))
                  (server-id (or id name)))
              (cond
               (url
                (e-mcp-server-create
                 :id server-id
                 :url (e-mcp-codex--unquote url)
                 :http-headers
                 (e-mcp-codex--header-pairs
                  (concat server-section ".http_headers"))))
               (command-raw
                (e-mcp-server-create
                 :id server-id
                 :command (cons (e-mcp-codex--unquote command-raw)
                                (e-mcp-codex--parse-array (or args-raw "[]")))
                 :env (e-mcp-codex--header-pairs
                       (concat server-section ".env"))
                 :timeout timeout))
               (t nil)))))))))

(provide 'e-mcp-codex)

;;; e-mcp-codex.el ends here

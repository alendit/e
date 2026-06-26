;;; e-mcp-codex-test.el --- Tests for Codex MCP config reader -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for reading `e-mcp-server' specs from a Codex `config.toml'.

;;; Code:

(require 'ert)
(require 'e-mcp)
(require 'e-mcp-codex)

(defconst e-mcp-codex-test--config "\
[mcp_servers.slack]
url = \"https://mcp.slack.com/mcp\"

[mcp_servers.slack.http_headers]
Authorization = \"Bearer xoxp-secret\"

[mcp_servers.docs]
url = \"https://developers.example.com/mcp\"

[mcp_servers.local]
command = \"node\"
args = [\"server.mjs\", \"--flag\"]

[mcp_servers.local.env]
TOKEN = \"abc123\"
"
  "Sample Codex config covering HTTP, headerless HTTP, and stdio servers.")

(defmacro e-mcp-codex-test--with-config (contents &rest body)
  "Run BODY with `e-mcp-codex-config-file' pointing at a temp file of CONTENTS."
  (declare (indent 1))
  `(let ((file (make-temp-file "e-mcp-codex-test" nil ".toml")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,contents))
           (let ((e-mcp-codex-config-file file))
             ,@body))
       (delete-file file))))

(ert-deftest e-mcp-codex-test-reads-http-server-with-headers ()
  "An HTTP server block yields a url server with parsed http headers."
  (e-mcp-codex-test--with-config e-mcp-codex-test--config
    (let ((server (e-mcp-codex-server "slack")))
      (should (e-mcp-server-p server))
      (should (equal (e-mcp-server-id server) "slack"))
      (should (equal (e-mcp-server-url server) "https://mcp.slack.com/mcp"))
      (should (null (e-mcp-server-command server)))
      (should (equal (e-mcp-server-http-headers server)
                     '(("Authorization" . "Bearer xoxp-secret")))))))

(ert-deftest e-mcp-codex-test-honors-explicit-id ()
  "The :id keyword overrides the e-side server id."
  (e-mcp-codex-test--with-config e-mcp-codex-test--config
    (should (equal (e-mcp-server-id (e-mcp-codex-server "slack" :id "slk"))
                   "slk"))))

(ert-deftest e-mcp-codex-test-reads-http-server-without-headers ()
  "An HTTP server block with no header section yields empty headers."
  (e-mcp-codex-test--with-config e-mcp-codex-test--config
    (let ((server (e-mcp-codex-server "docs")))
      (should (equal (e-mcp-server-url server) "https://developers.example.com/mcp"))
      (should (null (e-mcp-server-http-headers server))))))

(ert-deftest e-mcp-codex-test-reads-stdio-server ()
  "A command server block yields a command server with args, env, and timeout."
  (e-mcp-codex-test--with-config e-mcp-codex-test--config
    (let ((server (e-mcp-codex-server "local" :timeout 5)))
      (should (equal (e-mcp-server-command server)
                     '("node" "server.mjs" "--flag")))
      (should (equal (e-mcp-server-env server) '(("TOKEN" . "abc123"))))
      (should (null (e-mcp-server-url server)))
      (should (= (e-mcp-server-timeout server) 5)))))

(defconst e-mcp-codex-test--literal-config "\
[mcp_servers.slack]
url = 'https://mcp.slack.com/mcp'

[mcp_servers.slack.http_headers]
Authorization = 'Bearer xoxp-secret'

[mcp_servers.local]
command = 'node'
args = ['server.mjs', '--flag']

[mcp_servers.local.env]
TOKEN = 'abc123'
"
  "Codex config using TOML literal (single-quoted) strings throughout.")

(ert-deftest e-mcp-codex-test-reads-literal-single-quoted-strings ()
  "TOML literal (single-quoted) values are unquoted for url, headers, args, env."
  (e-mcp-codex-test--with-config e-mcp-codex-test--literal-config
    (let ((http (e-mcp-codex-server "slack"))
          (stdio (e-mcp-codex-server "local")))
      (should (equal (e-mcp-server-url http) "https://mcp.slack.com/mcp"))
      (should (equal (e-mcp-server-http-headers http)
                     '(("Authorization" . "Bearer xoxp-secret"))))
      (should (equal (e-mcp-server-command stdio) '("node" "server.mjs" "--flag")))
      (should (equal (e-mcp-server-env stdio) '(("TOKEN" . "abc123")))))))

(ert-deftest e-mcp-codex-test-missing-block-returns-nil ()
  "An absent server block yields nil rather than signaling."
  (e-mcp-codex-test--with-config e-mcp-codex-test--config
    (should (null (e-mcp-codex-server "nope")))))

(ert-deftest e-mcp-codex-test-missing-file-returns-nil ()
  "An unreadable config file yields nil."
  (let ((e-mcp-codex-config-file "/nonexistent/path/config.toml"))
    (should (null (e-mcp-codex-server "slack")))))

(provide 'e-mcp-codex-test)

;;; e-mcp-codex-test.el ends here

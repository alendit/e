;;; e-mcp-layer-test.el --- Tests for MCP layer registration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for `e-mcp-layer-register'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'e)
(require 'e-default-harnesses)
(require 'e-layers)
(require 'e-mcp)
(require 'e-mcp-layer)

(defun e-mcp-layer-test--server ()
  "Return an HTTP MCP server spec for tests."
  (e-mcp-server-create :id "slack" :url "https://mcp.slack.com/mcp"))

(defmacro e-mcp-layer-test--isolated (&rest body)
  "Run BODY with a fresh layer registry and default chat layer id list."
  (declare (indent 0))
  `(let ((e-layer--registry (make-hash-table :test 'eq))
         (e-default-chat-layer-ids (copy-sequence e-default-chat-layer-ids)))
     ,@body))

(ert-deftest e-mcp-layer-test-registers-spec-and-builds-layer ()
  "Registration stores a spec whose factory builds a one-capability layer."
  (e-mcp-layer-test--isolated
    (let ((spec (e-mcp-layer-register
                 :id 'slack-mcp
                 :name "Slack MCP"
                 :summary "Slack tools."
                 :servers (list (e-mcp-layer-test--server)))))
      (should (e-layer-spec-p spec))
      (should (eq (e-layer-get 'slack-mcp) spec))
      (should (equal (e-layer-spec-summary spec) "Slack tools."))
      (let ((layer (e-layer-create-registered 'slack-mcp)))
        (should (eq (e-layer-id layer) 'slack-mcp))
        (should (equal (e-layer-name layer) "Slack MCP"))
        (should (= (length (e-layer-capabilities layer)) 1))))))

(ert-deftest e-mcp-layer-test-default-appends-to-chat-layer-ids ()
  "A non-nil :default appends the id to `e-default-chat-layer-ids' once."
  (e-mcp-layer-test--isolated
    (should-not (memq 'slack-mcp e-default-chat-layer-ids))
    (e-mcp-layer-register
     :id 'slack-mcp
     :name "Slack MCP"
     :servers (list (e-mcp-layer-test--server))
     :default t)
    (should (memq 'slack-mcp e-default-chat-layer-ids))
    ;; Re-registering must not duplicate the id.
    (e-mcp-layer-register
     :id 'slack-mcp
     :name "Slack MCP"
     :servers (list (e-mcp-layer-test--server))
     :default t)
    (should (= (cl-count 'slack-mcp e-default-chat-layer-ids) 1))))

(ert-deftest e-mcp-layer-test-without-default-leaves-chat-layer-ids ()
  "Omitting :default does not touch `e-default-chat-layer-ids'."
  (e-mcp-layer-test--isolated
    (e-mcp-layer-register
     :id 'slack-mcp
     :name "Slack MCP"
     :servers (list (e-mcp-layer-test--server)))
    (should-not (memq 'slack-mcp e-default-chat-layer-ids))))

(ert-deftest e-mcp-layer-test-rejects-non-server-values ()
  "Non-`e-mcp-server' entries in :servers signal a type error."
  (e-mcp-layer-test--isolated
    (should-error (e-mcp-layer-register
                   :id 'bad
                   :servers (list "not-a-server"))
                  :type 'wrong-type-argument)))

(provide 'e-mcp-layer-test)

;;; e-mcp-layer-test.el ends here

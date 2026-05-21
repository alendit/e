;;; e-dev-test.el --- Tests for e development reload -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for interactive development reload behavior.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-dev)
(require 'e-openai)

(ert-deftest e-dev-test-reload-restores-mvp-entrypoints ()
  "Reload loads the MVP modules and restores their entry points."
  (fmakunbound 'e-chat-new)
  (fmakunbound 'e-chat-resume)
  (fmakunbound 'e-chat-rename)
  (fmakunbound 'e-chat-set-model)
  (fmakunbound 'e-chat-set-effort)
  (fmakunbound 'e-chat-open)
  (fmakunbound 'e-base-layer-create)
  (fmakunbound 'e-base-tools-register-defaults)
  (fmakunbound 'e-emacs-base-layer-create)
  (fmakunbound 'e-layer-create)
  (e-dev-reload default-directory)
  (should (commandp 'e-chat-new))
  (should (commandp 'e-chat-resume))
  (should (commandp 'e-chat-rename))
  (should (commandp 'e-chat-set-model))
  (should (commandp 'e-chat-set-effort))
  (should (fboundp 'e-chat-open))
  (should (fboundp 'e-base-layer-create))
  (should (fboundp 'e-base-tools-register-defaults))
  (should (fboundp 'e-emacs-base-layer-create))
  (should (fboundp 'e-layer-create)))

(ert-deftest e-dev-test-reload-clears-obsolete-entrypoints-and-refreshes-defaults ()
  "Reload removes stale functions and reapplies changed default options."
  (fset 'e-chat (lambda () (interactive)))
  (setq e-openai-default-model "gpt-5.4")
  (e-dev-reload default-directory)
  (should-not (fboundp 'e-chat))
  (should (equal e-openai-default-model "gpt-5.5")))

(provide 'e-dev-test)

;;; e-dev-test.el ends here

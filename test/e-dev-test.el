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

(ert-deftest e-dev-test-reload-restores-mvp-entrypoints ()
  "Reload loads the MVP modules and restores their entry points."
  (fmakunbound 'e-chat)
  (fmakunbound 'e-chat-new)
  (fmakunbound 'e-chat-resume)
  (fmakunbound 'e-chat-rename)
  (fmakunbound 'e-chat-open)
  (fmakunbound 'e-emacs-base-layer-create)
  (fmakunbound 'e-layer-create)
  (e-dev-reload default-directory)
  (should (commandp 'e-chat))
  (should (commandp 'e-chat-new))
  (should (commandp 'e-chat-resume))
  (should (commandp 'e-chat-rename))
  (should (fboundp 'e-chat-open))
  (should (fboundp 'e-emacs-base-layer-create))
  (should (fboundp 'e-layer-create)))

(provide 'e-dev-test)

;;; e-dev-test.el ends here

;;; e-dev-test.el --- Tests for e development reload -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for interactive development reload behavior.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-default-harnesses)
(require 'e-dev)
(require 'e-openai)

(ert-deftest e-dev-test-reload-restores-mvp-entrypoints ()
  "Reload loads the MVP modules and restores their entry points."
  (fmakunbound 'e-chat)
  (fmakunbound 'e-chat-new)
  (fmakunbound 'e-chat-resume)
  (fmakunbound 'e-chat-rename)
  (fmakunbound 'e-chat-set-model)
  (fmakunbound 'e-chat-set-effort)
  (fmakunbound 'e-chat-open)
  (fmakunbound 'e-base-layer-create)
  (fmakunbound 'e-emacs-base-layer-create)
  (fmakunbound 'e-layer-create)
  (fmakunbound 'e-operation-create)
  (fmakunbound 'e-resource-method-create)
  (fmakunbound 'e-resources-call)
  (fmakunbound 'e-shell-create)
  (fmakunbound 'e-shell-command-create)
  (fmakunbound 'e-chat-shell)
  (e-dev-reload default-directory)
  (should (commandp 'e-chat))
  (should (commandp 'e-chat-new))
  (should (commandp 'e-chat-resume))
  (should (commandp 'e-chat-rename))
  (should (commandp 'e-chat-set-model))
  (should (commandp 'e-chat-set-effort))
  (should (fboundp 'e-chat-open))
  (should (fboundp 'e-base-layer-create))
  (should (fboundp 'e-emacs-base-layer-create))
  (should (fboundp 'e-layer-create))
  (should (fboundp 'e-operation-create))
  (should (fboundp 'e-resource-method-create))
  (should (fboundp 'e-resources-call))
  (should (fboundp 'e-shell-create))
  (should (fboundp 'e-shell-command-create))
  (should (fboundp 'e-chat-shell))
  (should (eq (e-shell-id (e-shell-get 'chat)) 'chat)))

(ert-deftest e-dev-test-reload-clears-obsolete-entrypoints-and-refreshes-defaults ()
  "Reload removes stale functions and reapplies changed default options."
  (setq e-openai-default-model "gpt-5.4")
  (setq e-startup-capability-hook '(stale))
  (fset 'e-resource-handler-create #'ignore)
  (fset 'e-capability-resource-handlers #'ignore)
  (fset 'e-capabilities-register-resource-handlers #'ignore)
  (fset 'e-skill-create #'ignore)
  (fset 'e-skills-register #'ignore)
  (setq e-default-chat-layer-functions
        '(e-base-layer-create e-emacs-base-layer-create))
  (let ((e-startup-shell-hook
         (cons (lambda ()
                 (e-harness-registry-get-or-create :chat-default))
               e-startup-shell-hook)))
    (e-dev-reload default-directory))
  (should-not (boundp 'e-startup-capability-hook))
  (should-not (fboundp 'e-resource-handler-create))
  (should-not (fboundp 'e-capability-resource-handlers))
  (should-not (fboundp 'e-capabilities-register-resource-handlers))
  (should-not (fboundp 'e-skill-create))
  (should-not (fboundp 'e-skills-register))
  (should (equal e-openai-default-model "gpt-5.5"))
  (should (equal e-default-chat-layer-functions
                 '(e-layer-selection-layer-create
                   e-base-layer-create
                   e-emacs-base-layer-create)))
  (should (equal (mapcar #'e-layer-id
                         (e-harness-active-layers
                          (e-harness-registry-get-or-create :chat-default)))
                 '(chat-session e base emacs-base))))

(provide 'e-dev-test)

;;; e-dev-test.el ends here

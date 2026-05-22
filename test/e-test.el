;;; e-test.el --- Tests for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the minimal public package surface.

;;; Code:

(require 'autoload)
(require 'cl-lib)
(require 'ert)

(defconst e-test--autoload-commands
  '(e-chat-new
    e-chat-resume
    e-chat-rename
    e-chat-set-model
    e-chat-set-effort
    e-chat-show-context
    e-chat-submit
    e-chat-abort
    e-chat-reset
    e-dev-reload)
  "Interactive commands expected to exist from package autoloads.")

(defconst e-test--autoload-functions
  '(e-chat-shell)
  "Non-command functions expected to exist from package autoloads.")

(defconst e-test--core-features
  '(e-backend
    e-capabilities
    e-context
    e-events
    e-session
    e-tools
    e-loop
    e-layers
    e-harness
    e-core)
  "Features expected after requiring the pure core runtime.")

(defconst e-test--non-core-features
  '(e-chat
    e-openai
    e-emacs-tools
    e-base-tools)
  "Concrete shell, provider, and side-effect features excluded from e-core.")

(ert-deftest e-test-loads-feature ()
  "The package feature can be required from the project load path."
  (should (require 'e nil t)))

(ert-deftest e-test-core-loads-only-pure-runtime-features ()
  "Requiring e-core does not load shells, providers, or concrete tools."
  (let ((original-features features))
    (unwind-protect
        (progn
          (setq features
                (cl-set-difference features
                                   (append e-test--core-features
                                           e-test--non-core-features)))
          (should (require 'e-core nil t))
          (dolist (feature e-test--core-features)
            (should (featurep feature)))
          (dolist (feature e-test--non-core-features)
            (should-not (featurep feature))))
      (setq features original-features))))

(ert-deftest e-test-core-status-reports-only-pure-runtime-components ()
  "The core status plist describes the pure runtime boundary."
  (let ((original-features features))
    (unwind-protect
        (progn
          (setq features
                (cl-set-difference features
                                   (append e-test--core-features
                                           e-test--non-core-features)))
          (require 'e-core)
          (let ((status (e-core-status)))
            (should (eq (plist-get status :state) 'ready))
            (dolist (key '(:backends
                           :capabilities
                           :context
                           :events
                           :sessions
                           :tools
                           :loop
                           :layers
                           :harness))
              (should (plist-get status key)))
            (dolist (key '(:chat
                           :openai
                           :emacs-tools
                           :base-tools))
              (should-not (plist-member status key)))))
      (setq features original-features))))

(ert-deftest e-test-exposes-version ()
  "The package exposes its scaffold version."
  (require 'e)
  (should (string= e-version "0.1.0")))

(ert-deftest e-test-adds-source-subdirectories-to-load-path ()
  "The package makes nested source directories available for require/autoload."
  (require 'e)
  (dolist (directory '("lisp/core"
                       "lisp/layers"
                       "lisp/layers/base"
                       "lisp/layers/emacs"
                       "lisp/layers/evidence"
                       "lisp/layers/chat"
                       "lisp/shells"
                       "lisp/shells/chat"
                       "lisp/adapters/openai"
                       "lisp/dev"))
    (should (member (expand-file-name directory default-directory)
                    load-path))))

(ert-deftest e-test-exposes-interactive-entrypoints ()
  "The package exposes status and live-reload commands."
  (require 'e)
  (should (commandp 'e-status))
  (should (commandp 'e-chat-new))
  (should (commandp 'e-chat-resume))
  (should (commandp 'e-chat-rename))
  (should (commandp 'e-chat-set-model))
  (should (commandp 'e-chat-set-effort))
  (should (commandp 'e-chat-show-context))
  (should (commandp 'e-chat-submit))
  (should (commandp 'e-chat-abort))
  (should (commandp 'e-chat-reset))
  (should (commandp 'e-dev-reload)))

(ert-deftest e-test-autoloads-expose-chat-commands-at-startup ()
  "Generated package autoloads expose chat commands before reload."
  (let ((generated-autoload-file
         (make-temp-file
          (expand-file-name "e-generated-autoloads-" default-directory)
          nil
          ".el")))
    (unwind-protect
        (progn
          (dolist (command e-test--autoload-commands)
            (when (fboundp command)
              (fmakunbound command)))
          (dolist (function e-test--autoload-functions)
            (when (fboundp function)
              (fmakunbound function)))
          (update-directory-autoloads default-directory)
          (let ((load-path (cons default-directory load-path)))
            (load generated-autoload-file nil 'nomessage)
            (dolist (command e-test--autoload-commands)
              (should (commandp command)))
            (dolist (function e-test--autoload-functions)
              (should (fboundp function)))
            (autoload-do-load (symbol-function 'e-chat-new) 'e-chat-new)
            (autoload-do-load (symbol-function 'e-dev-reload) 'e-dev-reload))
          (dolist (command e-test--autoload-commands)
            (should (commandp command)))
          (dolist (function e-test--autoload-functions)
            (should (fboundp function))))
      (when (file-exists-p generated-autoload-file)
        (delete-file generated-autoload-file))
      (when (file-exists-p (concat generated-autoload-file "~"))
        (delete-file (concat generated-autoload-file "~")))
      (load (expand-file-name "e.el" default-directory) nil 'nomessage)
      (load (expand-file-name "lisp/shells/chat/e-chat.el" default-directory)
            nil
            'nomessage)
      (load (expand-file-name "lisp/dev/e-dev.el" default-directory)
            nil
            'nomessage))))

(ert-deftest e-test-exposes-shell-manifest-api ()
  "The package exposes generic shell manifest constructors and registry."
  (require 'e)
  (should (fboundp 'e-shell-create))
  (should (fboundp 'e-shell-command-create))
  (should (fboundp 'e-shell-register))
  (should (fboundp 'e-shell-get))
  (should (fboundp 'e-shell-list))
  (should (fboundp 'e-shell-command-by-id)))

(ert-deftest e-test-exposes-core-harness-api ()
  "The package exposes the core harness API after requiring e."
  (require 'e)
  (should (fboundp 'e-harness-create))
  (should (fboundp 'e-harness-create-session))
  (should (fboundp 'e-harness-subscribe))
  (should (fboundp 'e-harness-activate-layer))
  (should (fboundp 'e-harness-activate-capability))
  (should (fboundp 'e-harness-prompt))
  (should (fboundp 'e-harness-prompt-async))
  (should (fboundp 'e-harness-wait))
  (should (fboundp 'e-harness-follow-up))
  (should (fboundp 'e-harness-abort))
  (should (fboundp 'e-harness-reset))
  (should (fboundp 'e-harness-state))
  (should (fboundp 'e-harness-messages))
  (should (fboundp 'e-capability-create))
  (should (fboundp 'e-capabilities-context-messages)))

(provide 'e-test)

;;; e-test.el ends here

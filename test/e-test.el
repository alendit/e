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
  '(e-chat
    e-chat-new
    e-chat-resume
    e-chat-start-here
    e-chat-rename
    e-chat-set-model
    e-chat-set-effort
    e-chat-show-context
    e-chat-submit
    e-chat-abort
    e-chat-reset
    e-canvas-open-for-current-buffer
    e-canvas-new-buffer
    e-canvas-new-file
    e-canvas-attach-current-buffer
    e-canvas-attach-file
    e-layers-toggle
    e-layers-enable
    e-layers-disable
    e-dev-reload)
  "Interactive commands expected to exist from package autoloads.")

(defconst e-test--autoload-functions
  '(e-chat-shell
    e-chat-starter-shell
    e-canvas-shell
    e-layers-shell)
  "Non-command functions expected to exist from package autoloads.")

(defconst e-test--core-features
  '(e-backend
    e-capabilities
    e-hooks
    e-context
    e-events
    e-store
    e-skills
    e-startup
    e-session
    e-tools
    e-loop
    e-layers
    e-harness
    e-harness-registry
    e-core)
  "Features expected after requiring the pure core runtime.")

(defconst e-test--non-core-features
  '(e-chat
    e-chat-starter
    e-canvas
    e-default-layers
    e-default-harnesses
    e-layer-selection
    e-harness-base
    e-session-tmp-resources
    e-tool-output-truncation
    e-layers-shell
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
                           :hooks
                           :context
                           :events
                           :store
                           :skills
                           :sessions
                           :tools
                           :loop
                           :layers
                           :harness
                           :harness-registry))
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
                       "lisp/layers/agents"
                       "lisp/layers/base"
                       "lisp/layers/harness"
                       "lisp/layers/emacs"
                       "lisp/layers/evidence"
                       "lisp/layers/web"
                       "lisp/layers/chat"
                       "lisp/defaults"
                       "lisp/shells"
                       "lisp/shells/chat"
                       "lisp/adapters/openai"
                       "lisp/dev"))
    (should (member (expand-file-name directory default-directory)
                    load-path))))

(ert-deftest e-test-symlinked-package-load-resolves-real-source-directory ()
  "Loading a straight-style symlinked package should use the real source root."
  (let ((build-directory (make-temp-file "e-build-" t))
        (original-features features))
    (unwind-protect
        (progn
          (make-symbolic-link
           (expand-file-name "e.el" default-directory)
           (expand-file-name "e.el" build-directory)
           t)
          (should-not
           (file-exists-p (expand-file-name "e-operations.el"
                                            build-directory)))
          (setq features
                (cl-remove-if
                 (lambda (feature)
                   (string-prefix-p "e" (symbol-name feature)))
                 features))
          (let ((load-path (cons build-directory load-path)))
            (load (expand-file-name "e.el" build-directory) nil 'nomessage))
          (should (equal (file-name-as-directory
                          (file-truename default-directory))
                         (e-source-directory)))
          (should (featurep 'e-operations)))
      (setq features original-features)
      (delete-directory build-directory t)
      (load (expand-file-name "e.el" default-directory) nil 'nomessage))))

(ert-deftest e-test-exposes-interactive-entrypoints ()
  "The package exposes status and live-reload commands."
  (require 'e)
  (should (commandp 'e-status))
  (should (commandp 'e-chat))
  (should (commandp 'e-chat-new))
  (should (commandp 'e-chat-resume))
  (should (commandp 'e-chat-rename))
  (should (commandp 'e-chat-set-model))
  (should (commandp 'e-chat-set-effort))
  (should (commandp 'e-chat-show-context))
  (should (commandp 'e-chat-context-mode))
  (should (commandp 'e-chat-submit))
  (should (commandp 'e-chat-abort))
  (should (commandp 'e-chat-reset))
  (should (commandp 'e-canvas-open-for-current-buffer))
  (should (commandp 'e-canvas-new-buffer))
  (should (commandp 'e-canvas-new-file))
  (should (commandp 'e-canvas-attach-current-buffer))
  (should (commandp 'e-canvas-attach-file))
  (should (commandp 'e-layers-toggle))
  (should (commandp 'e-layers-enable))
  (should (commandp 'e-layers-disable))
  (should (commandp 'e-dev-reload)))

(ert-deftest e-test-startup-loads-shell-providers ()
  "Startup loads shell command providers instead of hand-autoloading commands."
  (let ((original-features features))
    (unwind-protect
        (progn
          (setq features (cl-set-difference features
                                            '(e e-chat e-chat-starter e-canvas
                                              e-layers-shell)))
          (load (expand-file-name "e.el" default-directory) nil 'nomessage)
          (should (featurep 'e-chat))
          (should (featurep 'e-chat-starter))
          (should (featurep 'e-canvas))
          (should (featurep 'e-layers-shell))
          (should (eq (e-shell-id (e-shell-get 'chat)) 'chat))
          (should (eq (e-shell-id (e-shell-get 'global-session-starter))
                      'global-session-starter))
          (should (eq (e-shell-id (e-shell-get 'canvas)) 'canvas))
          (should (eq (e-shell-id (e-shell-get 'layers)) 'layers)))
      (setq features original-features)
      (load (expand-file-name "e.el" default-directory) nil 'nomessage))))

(ert-deftest e-test-startup-runs-provider-load-hooks-in-order ()
  "Startup hooks run layer, then shell providers."
  (require 'e-startup)
  (let ((events nil)
        (e-startup-layer-hook nil)
        (e-startup-shell-hook nil))
    (add-hook 'e-startup-layer-hook
              (lambda () (push 'layer events)))
    (add-hook 'e-startup-shell-hook
              (lambda () (push 'shell events)))
    (e-startup-run)
    (should (equal (nreverse events)
                   '(layer shell)))))

(ert-deftest e-test-startup-refreshes-chat-shell-keymaps ()
  "Package startup uses the chat shell hook to refresh initial keymaps."
  (require 'e)
  (let ((e-chat-mode-map (make-sparse-keymap))
        (e-chat-response-navigation-mode-map (make-sparse-keymap)))
    (e-startup-run)
    (should (eq (lookup-key e-chat-mode-map (kbd "<escape>"))
                'e-chat-enter-response-navigation))
    (should (eq (lookup-key e-chat-response-navigation-mode-map (kbd "j"))
                'e-chat-response-navigation-next))))

(ert-deftest e-test-autoloads-expose-chat-commands-at-startup ()
  "Generated provider autoloads expose chat commands before reload."
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
          (when (fboundp 'e--add-source-directories)
            (fmakunbound 'e--add-source-directories))
          (dolist (directory '("." "lisp/shells" "lisp/shells/chat"
                               "lisp/dev"))
            (update-directory-autoloads
             (expand-file-name directory default-directory)))
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
      (load (expand-file-name "lisp/shells/e-layers-shell.el"
                              default-directory)
            nil
            'nomessage)
      (load (expand-file-name "lisp/shells/chat/e-chat.el" default-directory)
            nil
            'nomessage)
      (load (expand-file-name "lisp/shells/chat/e-chat-starter.el"
                              default-directory)
            nil
            'nomessage)
      (load (expand-file-name "lisp/shells/e-canvas.el" default-directory)
            nil
            'nomessage)
      (load (expand-file-name "lisp/dev/e-dev.el" default-directory)
            nil
            'nomessage)
      (e-chat--refresh-keymaps))))

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
  (should (fboundp 'e-harness-unsubscribe))
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
  (should (fboundp 'e-harness-registry-register-factory))
  (should (fboundp 'e-harness-registry-get-or-create))
  (should (fboundp 'e-capability-create))
  (should (fboundp 'e-capabilities-context-messages)))

(ert-deftest e-test-requiring-e-registers-chat-default-factory ()
  "Requiring e registers the startup default chat harness factory lazily."
  (require 'e)
  (require 'e-harness-registry)
  (should (member :chat-default (e-harness-registry-list))))

(provide 'e-test)

;;; e-test.el ends here

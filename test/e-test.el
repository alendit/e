;;; e-test.el --- Tests for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the minimal public package surface.

;;; Code:

(require 'autoload)
(require 'ert)

(defconst e-test--autoload-commands
  '(e-chat-new
    e-chat-resume
    e-chat-rename
    e-chat-set-model
    e-chat-set-effort
    e-chat-submit
    e-chat-abort
    e-chat-reset
    e-dev-reload)
  "Interactive commands expected to exist from package autoloads.")

(ert-deftest e-test-loads-feature ()
  "The package feature can be required from the project load path."
  (should (require 'e nil t)))

(ert-deftest e-test-exposes-version ()
  "The package exposes its scaffold version."
  (require 'e)
  (should (string= e-version "0.1.0")))

(ert-deftest e-test-exposes-interactive-entrypoints ()
  "The package exposes status and live-reload commands."
  (require 'e)
  (should (commandp 'e-status))
  (should (commandp 'e-chat-new))
  (should (commandp 'e-chat-resume))
  (should (commandp 'e-chat-rename))
  (should (commandp 'e-chat-set-model))
  (should (commandp 'e-chat-set-effort))
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
          (update-directory-autoloads default-directory)
          (let ((load-path (list default-directory)))
            (load generated-autoload-file nil 'nomessage)
            (dolist (command e-test--autoload-commands)
              (should (commandp command)))
            (autoload-do-load (symbol-function 'e-chat-new) 'e-chat-new)
            (autoload-do-load (symbol-function 'e-dev-reload) 'e-dev-reload))
          (dolist (command e-test--autoload-commands)
            (should (commandp command))))
      (when (file-exists-p generated-autoload-file)
        (delete-file generated-autoload-file))
      (when (file-exists-p (concat generated-autoload-file "~"))
        (delete-file (concat generated-autoload-file "~")))
      (load (expand-file-name "e.el" default-directory) nil 'nomessage)
      (load (expand-file-name "lisp/e-chat.el" default-directory) nil 'nomessage)
      (load (expand-file-name "lisp/e-dev.el" default-directory) nil 'nomessage))))

(ert-deftest e-test-exposes-core-harness-api ()
  "The package exposes the core harness API after requiring e."
  (require 'e)
  (should (fboundp 'e-harness-create))
  (should (fboundp 'e-harness-prompt))
  (should (fboundp 'e-harness-prompt-async))
  (should (fboundp 'e-harness-wait))
  (should (fboundp 'e-harness-messages)))

(provide 'e-test)

;;; e-test.el ends here

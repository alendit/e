;;; e.el --- Emacs-hosted agent runtime scaffold -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools
;; URL: https://github.com/dimitrivorona/e
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Minimal package entry point for e, an Emacs-hosted agent runtime.
;; This file intentionally exposes only a small scaffold surface.  Core
;; behavior lives in focused runtime modules, while development helpers live
;; outside the runtime boundary.

;;; Code:

(declare-function e-core-status "e-core")
(declare-function e-startup-run "e-startup")

(eval-and-compile
  (defconst e--directory
    (file-name-directory
     (file-truename
      (expand-file-name (or load-file-name buffer-file-name default-directory))))
    "Directory containing the e package entry point.")
  (defconst e--source-subdirectories
    '("lisp/core"
      "lisp/layers"
      "lisp/layers/agents"
      "lisp/layers/base"
      "lisp/layers/harness"
      "lisp/layers/emacs"
      "lisp/layers/evidence"
      "lisp/layers/web"
      "lisp/layers/text-editing"
      "lisp/layers/org-canvas"
      "lisp/layers/project-local"
      "lisp/layers/chat"
      "lisp/defaults"
      "lisp/shells"
      "lisp/shells/chat"
      "lisp/adapters/openai"
      "lisp/adapters/anthropic"
      "lisp/dev")
    "Source directories containing e libraries.")
  (defun e--add-source-directories (directory)
    "Add e source subdirectories rooted at DIRECTORY to `load-path'."
    (dolist (subdirectory (reverse e--source-subdirectories))
      (add-to-list 'load-path (expand-file-name subdirectory directory))))
  (e--add-source-directories e--directory))

;;;###autoload
(let ((directory
       (file-name-directory
        (file-truename
         (expand-file-name
          (or (locate-library "e")
              load-file-name
              buffer-file-name
              default-directory))))))
  (dolist (subdirectory (reverse
                         '("lisp/core"
                           "lisp/layers"
                           "lisp/layers/agents"
                           "lisp/layers/base"
                           "lisp/layers/harness"
                           "lisp/layers/emacs"
                           "lisp/layers/evidence"
                           "lisp/layers/web"
                           "lisp/layers/text-editing"
                           "lisp/layers/org-canvas"
                           "lisp/layers/project-local"
                           "lisp/layers/chat"
                           "lisp/defaults"
                           "lisp/shells"
                           "lisp/shells/chat"
                           "lisp/adapters/openai"
                           "lisp/dev")))
    (add-to-list 'load-path (expand-file-name subdirectory directory))))

(let ((load-prefer-newer t))
  (require 'e-core)
  (require 'e-context-budget)
  (require 'e-default-layers)
  (require 'e-project-local)
  (require 'e-default-harnesses)
  (require 'e-shells)
  (require 'e-layers-shell)
  (require 'e-context-status)
  (require 'e-chat)
  (require 'e-chat-starter)
  (require 'e-canvas)
  (require 'e-org-canvas)
  (require 'e-debug)
  (require 'e-background-session))
(e-startup-run)

(defgroup e nil
  "Emacs-hosted agent runtime."
  :group 'applications
  :prefix "e-")

(defconst e-version "0.1.0"
  "Version of the e package.")

(defun e-source-directory ()
  "Return the directory containing the loaded e entry point."
  e--directory)

;;;###autoload
(defun e-status ()
  "Display and return the current e scaffold status."
  (interactive)
  (let* ((core-status (e-core-status))
         (summary
          (format "e %s loaded from %s (%s)"
                  e-version
                  (abbreviate-file-name (e-source-directory))
                  (plist-get core-status :state))))
    (when (called-interactively-p 'interactive)
      (message "%s" summary))
    summary))

;;;###autoload
(autoload 'e-dev-reload "e-dev" "Reload e package files during development." t)
;;;###autoload
(autoload 'e-project-local-byte-compile-project
  "e-project-local"
  "Byte-compile allowlisted project-local Elisp under DIRECTORY."
  t)
;;;###autoload
(autoload 'e-dev-profile-start "e-dev-profile" "Start an e developer profiling trace." t)
;;;###autoload
(autoload 'e-dev-profile-stop "e-dev-profile" "Stop an e developer profiling trace." t)
;;;###autoload
(autoload 'e-dev-profile-report "e-dev-profile" "Open an e developer profiling report." t)
;;;###autoload
(autoload 'e-dev-profile-open-latest "e-dev-profile" "Open the latest e profiling trace." t)

(provide 'e)

;;; e.el ends here

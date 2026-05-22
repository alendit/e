;;; e-dev.el --- Interactive development helpers for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Development-only helpers for interactive package work inside Emacs.

;;; Code:

(require 'e)

(defgroup e-dev nil
  "Interactive development helpers for e."
  :group 'e
  :prefix "e-dev-")

(defconst e-dev--directory
  (file-name-directory
   (file-truename (or load-file-name buffer-file-name default-directory)))
  "Directory containing this development helper file.")

(defcustom e-dev-source-directory
  (expand-file-name "../.." e-dev--directory)
  "Root directory of the local e checkout used for live reloading."
  :type 'directory
  :group 'e-dev)

(defconst e-dev--obsolete-functions
  '(e-chat)
  "Functions removed from the public surface that reload should unbind.")

(defconst e-dev--reevaluated-defaults
  '(e-openai-default-model
    e-openai-default-reasoning-effort)
  "Uncustomized options whose changed defaults should apply after reload.")

(defun e-dev--clear-obsolete-functions ()
  "Remove stale obsolete function bindings after a live reload."
  (dolist (symbol e-dev--obsolete-functions)
    (when (fboundp symbol)
      (fmakunbound symbol))))

(defun e-dev--reevaluate-uncustomized-defaults ()
  "Reapply changed defcustom defaults unless the user customized them."
  (dolist (symbol e-dev--reevaluated-defaults)
    (when (and (boundp symbol)
               (not (get symbol 'customized-value)))
      (custom-reevaluate-setting symbol))))

;;;###autoload
(defun e-dev-reload (&optional directory)
  "Reload e package files from DIRECTORY or `e-dev-source-directory'."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name (or directory e-dev-source-directory))))
         (files '("lisp/core/e-events.el"
                  "lisp/core/e-session.el"
                  "lisp/core/e-context.el"
                  "lisp/core/e-tools.el"
                  "lisp/core/e-capabilities.el"
                  "lisp/layers/e-layers.el"
                  "lisp/core/e-backend.el"
                  "lisp/core/e-loop.el"
                  "lisp/core/e-harness.el"
                  "lisp/layers/base/e-base-tools.el"
                  "lisp/layers/base/e-file-capabilities.el"
                  "lisp/layers/base/e-base.el"
                  "lisp/layers/emacs/e-emacs-tools.el"
                  "lisp/layers/emacs/e-emacs-capabilities.el"
                  "lisp/layers/emacs/e-emacs-base.el"
                  "lisp/layers/evidence/e-evidence-tools.el"
                  "lisp/layers/chat/e-chat-session.el"
                  "lisp/adapters/openai/e-openai.el"
                  "lisp/shells/chat/e-chat.el"
                  "lisp/core/e-core.el"
                  "e.el"
                  "lisp/dev/e-dev.el")))
    (dolist (file files)
      (load (expand-file-name file root) nil 'nomessage))
    (e-dev--clear-obsolete-functions)
    (e-dev--reevaluate-uncustomized-defaults)
    (let ((refreshed (if (fboundp 'e-chat-reload-buffers)
                         (e-chat-reload-buffers)
                       0)))
      (message "Reloaded e from %s; refreshed %d chat buffer%s"
               (abbreviate-file-name root)
               refreshed
               (if (= refreshed 1) "" "s")))
    root))

(provide 'e-dev)

;;; e-dev.el ends here

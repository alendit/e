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
  (expand-file-name ".." e-dev--directory)
  "Root directory of the local e checkout used for live reloading."
  :type 'directory
  :group 'e-dev)

;;;###autoload
(defun e-dev-reload (&optional directory)
  "Reload e package files from DIRECTORY or `e-dev-source-directory'."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name (or directory e-dev-source-directory))))
         (files '("lisp/e-events.el"
                  "lisp/e-session.el"
                  "lisp/e-context.el"
                  "lisp/e-backend.el"
                  "lisp/e-tools.el"
                  "lisp/e-loop.el"
                  "lisp/e-harness.el"
                  "lisp/e-core.el"
                  "e.el"
                  "lisp/e-dev.el")))
    (dolist (file files)
      (load (expand-file-name file root) nil 'nomessage))
    (message "Reloaded e from %s" (abbreviate-file-name root))
    root))

(provide 'e-dev)

;;; e-dev.el ends here

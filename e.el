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

(eval-and-compile
  (defconst e--directory
    (file-name-directory
     (file-truename (or load-file-name buffer-file-name default-directory)))
    "Directory containing the e package entry point.")
  (add-to-list 'load-path (expand-file-name "lisp" e--directory)))

(require 'e-core)

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

(provide 'e)

;;; e.el ends here

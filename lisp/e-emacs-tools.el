;;; e-emacs-tools.el --- Harmless Emacs tools for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Low-risk concrete tools.  M2 intentionally avoids file writes, process
;; execution, elisp evaluation, buffer edits, and harness mutation tools.

;;; Code:

(require 'cl-lib)
(require 'e-tools)

(defun e-emacs-tools-register-current-time (registry)
  "Register a harmless `current-time' tool in REGISTRY."
  (e-tools-register
   registry
   :name "current_time"
   :description "Return the current local time as a readable string."
   :parameters '(:type "object"
                 :properties nil)
   :handler (lambda (_arguments)
              (current-time-string))))

(defun e-emacs-tools-register-defaults (registry)
  "Register M2-safe concrete tools in REGISTRY."
  (e-emacs-tools-register-current-time registry)
  registry)

(provide 'e-emacs-tools)

;;; e-emacs-tools.el ends here

;;; e-startup.el --- Startup hooks for e providers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider startup hooks keep package load and development reload on the same
;; initialization path.  Concrete providers own their hooks; the entry point
;; only runs the hook groups in dependency order.

;;; Code:

(defvar e-startup-layer-hook nil
  "Hook run for layer/default provider startup.")

(defvar e-startup-shell-hook nil
  "Hook run for presentation shell provider startup.")

(defun e-startup-run ()
  "Run e provider startup hooks in dependency order."
  (run-hooks 'e-startup-layer-hook)
  (run-hooks 'e-startup-shell-hook))

(provide 'e-startup)

;;; e-startup.el ends here

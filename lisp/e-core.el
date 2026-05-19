;;; e-core.el --- Core runtime scaffold for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure core scaffold for e.  This module must stay independent from
;; presentation buffers, keymaps, provider adapters, and concrete side effects.

;;; Code:

(defconst e-core-scaffold-state 'ready
  "Minimal state marker for the core runtime scaffold.")

(defun e-core-status ()
  "Return a plist describing the current core scaffold state."
  (list :state e-core-scaffold-state))

(provide 'e-core)

;;; e-core.el ends here

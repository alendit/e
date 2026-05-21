;;; e-core.el --- Core runtime for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure core runtime for e.  This module must stay independent from
;; presentation buffers, keymaps, provider adapters, and concrete side effects.

;;; Code:

(require 'e-backend)
(require 'e-base)
(require 'e-base-tools)
(require 'e-chat)
(require 'e-context)
(require 'e-emacs-base)
(require 'e-emacs-tools)
(require 'e-events)
(require 'e-harness)
(require 'e-layers)
(require 'e-loop)
(require 'e-openai)
(require 'e-session)
(require 'e-tools)

(defconst e-core-scaffold-state 'ready
  "Minimal state marker for the core runtime scaffold.")

(defun e-core-status ()
  "Return a plist describing the current core state."
  (list :state e-core-scaffold-state
        :events t
        :sessions t
        :session-persistence t
        :chat t
        :layers t
        :base t
        :base-tools t
        :backends t
        :context t
        :emacs-base t
        :emacs-tools t
        :openai t
        :tools t
        :loop t
        :harness t))

(provide 'e-core)

;;; e-core.el ends here

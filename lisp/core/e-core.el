;;; e-core.el --- Core runtime for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure core runtime for e.  This module must stay independent from
;; presentation buffers, keymaps, provider adapters, and concrete side effects.

;;; Code:

(require 'e-backend)
(require 'e-compaction)
(require 'e-context)
(require 'e-events)
(require 'e-operations)
(require 'e-resources)
(require 'e-store)
(require 'e-capability-config)
(require 'e-capabilities)
(require 'e-hooks)
(require 'e-mcp)
(require 'e-skills)
(require 'e-startup)
(require 'e-session)
(require 'e-tools)
(require 'e-loop)
(require 'e-layers)
(require 'e-harness)
(require 'e-harness-registry)

(defconst e-core-scaffold-state 'ready
  "Minimal state marker for the core runtime scaffold.")

(defun e-core-status ()
  "Return a plist describing the current core state."
  (list :state e-core-scaffold-state
        :backends t
        :compaction t
        :capabilities t
        :hooks t
        :mcp t
        :context t
        :events t
        :operations t
        :resources t
        :store t
        :skills t
        :startup t
        :sessions t
        :session-persistence t
        :tools t
        :loop t
        :layers t
        :harness t
        :harness-registry t))

(provide 'e-core)

;;; e-core.el ends here

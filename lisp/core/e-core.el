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
(require 'e-resource-patterns)
(require 'e-resource-query)
(require 'e-resources)
(require 'e-resource-coherence)
(require 'e-request)
(require 'e-work)
(require 'e-store)
(require 'e-capability-config)
(require 'e-capabilities)
(require 'e-prompts)
(require 'e-hooks)
(require 'e-mcp)
(require 'e-skills)
(require 'e-startup)
(require 'e-session)
(require 'e-tools)
(require 'e-loop)
(require 'e-layers)
(require 'e-harness)
(require 'e-actions)
(require 'e-usage-report)
(require 'e-harness-registry)
(require 'e-harness-instances)
(require 'e-task-queue)

(defconst e-core-scaffold-state 'ready
  "Minimal state marker for the core runtime scaffold.")

(defun e-core-status ()
  "Return a plist describing the current core state."
  (list :state e-core-scaffold-state
        :backends t
        :compaction t
        :capabilities t
        :prompts t
        :hooks t
        :mcp t
        :context t
        :events t
        :operations t
        :resources t
        :resource-coherence t
        :request-lifecycle t
        :work t
        :store t
        :skills t
        :startup t
        :sessions t
        :session-persistence t
        :tools t
        :loop t
        :layers t
        :harness t
        :actions t
        :usage-report t
        :harness-registry t
        :harness-instances t))

(provide 'e-core)

;;; e-core.el ends here

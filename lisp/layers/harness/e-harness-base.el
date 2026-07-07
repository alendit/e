;;; e-harness-base.el --- Harness support layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Built-in harness support layer.  This layer contributes resources and
;; lifecycle hooks owned by the harness rather than by OS or editor tool sets.

;;; Code:

(require 'e-capabilities)
(require 'e-layers)
(require 'e-raw-results)
(require 'e-session-tmp-resources)
(require 'e-session-resources)
(require 'e-raw-result-cleanup)
(require 'e-tool-output-truncation)
(require 'e-await-tool)

(defconst e-harness-base-instructions
  "Communicate reasoning explicitly and concretely, without unnecessary detail. Surface concise reasoning when it changes what the user can understand about the turn: a distinct phase begins, new evidence narrows the work, a decision or tradeoff is made, a blocker appears, the approach changes, or a non-obvious next action is about to happen. Do not send an update for every command or tool call, repeat the same reason for similar commands, restate visible plans, or narrate obvious continuation."
  "Base model-facing instructions contributed by the harness-base layer.")

(defun e-harness-base-context-capability-create ()
  "Create the harness-base context guidance capability."
  (e-capability-create
   :id 'harness-base-context
   :name "Harness Base Context"
   :instruction-priority 240
   :instructions e-harness-base-instructions))

(defun e-await-capability-create ()
  "Create the capability contributing the model-facing await tool.
Await is generic over `e-work' handles, so it lives in the base layer every
harness carries rather than in any one subsystem."
  (e-capability-create
   :id 'await
   :name "Await"
   :tools (list #'e-await-tool-register)))

(defun e-harness-base-layer-create ()
  "Create the harness-base support layer."
  (e-layer-create
   :id 'harness-base
   :name "Harness Base"
   :capabilities (list (e-harness-base-context-capability-create)
                       (e-await-capability-create)
                       (e-raw-results-capability-create)
                       (e-session-tmp-capability-create)
                       (e-session-resources-capability-create)
                       (e-tool-output-truncation-capability-create))))

(provide 'e-harness-base)

;;; e-harness-base.el ends here

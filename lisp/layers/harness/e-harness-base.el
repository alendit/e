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
(require 'e-session-tmp-resources)
(require 'e-tool-output-truncation)

(defconst e-harness-base-instructions
  "Communicate reasoning explicitly and concretely: emit a concise reasoning message only when it conveys a genuinely new thought, such as a new decision, tradeoff, uncertainty, blocker, or course correction. Do not repeat prior reasoning summaries or restate routine progress; otherwise continue without a reasoning message."
  "Base model-facing instructions contributed by the harness-base layer.")

(defun e-harness-base-context-capability-create ()
  "Create the harness-base context guidance capability."
  (e-capability-create
   :id 'harness-base-context
   :name "Harness Base Context"
   :instruction-priority 240
   :instructions e-harness-base-instructions))

(defun e-harness-base-layer-create ()
  "Create the harness-base support layer."
  (e-layer-create
   :id 'harness-base
   :name "Harness Base"
   :capabilities (list (e-harness-base-context-capability-create)
                       (e-session-tmp-capability-create)
                       (e-tool-output-truncation-capability-create))))

(provide 'e-harness-base)

;;; e-harness-base.el ends here

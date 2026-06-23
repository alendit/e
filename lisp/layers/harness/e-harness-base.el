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
  "Communicate reasoning explicitly and concretely: Use brief reasoning updates when they help the user follow meaningful progress or new context, such as what you learned, a decision, tradeoff, uncertainty, blocker, course correction, or why a non-obvious next action matters. Keep each update to one short sentence. Suppress only boilerplate updates that merely say you will continue, restate the same plan or status, name an obvious next step without new context, or repeat prior reasoning."
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

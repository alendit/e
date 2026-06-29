;;; e-harness-advanced.el --- Advanced harness capabilities for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Optional-by-layer but default-enabled advanced harness capabilities.  These
;; capabilities help the agent manage its own work process without belonging to
;; OS, editor, project, or presentation layers.

;;; Code:

(require 'e-goal)
(require 'e-layers)

(defun e-harness-advanced-layer-create ()
  "Create the harness-advanced support layer."
  (e-layer-create
   :id 'harness-advanced
   :name "Harness Advanced"
   :requires '(harness-base)
   :capabilities (list (e-goal-capability-create))))

(provide 'e-harness-advanced)

;;; e-harness-advanced.el ends here

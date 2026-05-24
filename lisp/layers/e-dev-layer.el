;;; e-dev-layer.el --- e development layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Development layer packaging context-inspection tools.

;;; Code:

(require 'e-context-inspection)
(require 'e-layers)

(defun e-dev-layer-create ()
  "Create the e-dev layer."
  (e-layer-create
   :id 'e-dev
   :name "e Dev"
   :capabilities (list (e-context-inspection-capability-create))))

(provide 'e-dev-layer)

;;; e-dev-layer.el ends here

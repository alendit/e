;;; e-layer.el --- e self-management layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Runtime self-management layer.

;;; Code:

(require 'e-context-inspection)
(require 'e-layer-selection)
(require 'e-layers)

(defun e-core-layer-create ()
  "Create the e self-management layer."
  (e-layer-create
   :id 'e
   :name "e"
   :capabilities (list (e-layer-selection-capability-create)
                       (e-context-inspection-capability-create))))

(provide 'e-layer)

;;; e-layer.el ends here

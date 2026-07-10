;;; e-async-control.el --- Cross-domain async work control for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the model-facing join primitive shared by async capability actions.
;; Domain capabilities start and control their work through actions; this layer
;; contributes the one top-level async tool required to hold a model turn open
;; without blocking Emacs.

;;; Code:

(require 'e-await-tool)
(require 'e-capabilities)
(require 'e-layers)

(defun e-async-control-capability-create ()
  "Create the capability contributing the cross-domain await tool."
  (e-capability-create
   :id 'async-control
   :name "Async Control"
   :tools (list #'e-await-tool-register)))

(defun e-async-control-layer-create ()
  "Create the async-control layer."
  (e-layer-create
   :id 'async-control
   :name "Async Control"
   :capabilities (list (e-async-control-capability-create))))

(provide 'e-async-control)

;;; e-async-control.el ends here

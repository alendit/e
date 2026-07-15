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
(require 'e-waitable)
(require 'e-work)

(defun e-async-control-register-work-resolver ()
  "Register the generic `work' waitable scheme against the detached registry.
A reference of the form work:HANDLE-ID resolves to whichever detachable tool
call outlived its `wait_for' window, without the await tool knowing which tool
produced it.  This is the entire integration between detached work and `await'."
  (e-waitable-register-resolver
   "work"
   (lambda (id) (e-work-detached-handle id))))

(defun e-async-control-capability-create ()
  "Create the capability contributing the cross-domain await tool."
  (e-capability-create
   :id 'async-control
   :name "Async Control"
   :tools (list #'e-await-tool-register)))

(defun e-async-control-layer-create ()
  "Create the async-control layer."
  (e-async-control-register-work-resolver)
  (e-layer-create
   :id 'async-control
   :name "Async Control"
   :capabilities (list (e-async-control-capability-create))))

(provide 'e-async-control)

;;; e-async-control.el ends here

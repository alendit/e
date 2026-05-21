;;; e-emacs-base.el --- Default Emacs layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The MVP Emacs base layer contributes Emacs-aware instructions, automatic
;; visible-buffer context, and concrete Emacs tools.

;;; Code:

(require 'e-emacs-capabilities)
(require 'e-layers)

(defun e-emacs-base--visible-buffer-context ()
  "Return a readable summary of visible Emacs buffers."
  (e-emacs-capabilities-visible-buffer-context))

(defun e-emacs-base-visible-buffers-context-provider ()
  "Return the visible-buffer context provider for the emacs-base layer."
  (e-emacs-visible-buffers-context-provider))

(defun e-emacs-base-layer-create ()
  "Create the MVP emacs-base layer."
  (e-layer-create
   :id 'emacs-base
   :name "Emacs Base"
   :capabilities (list (e-emacs-awareness-capability-create)
                       (e-buffer-read-capability-create)
                       (e-selection-context-capability-create)
                       (e-buffer-edit-capability-create)
                       (e-elisp-eval-capability-create))
   :instructions nil
   :tools nil
   :context-providers nil
   :skills nil
   :prompts nil))

(provide 'e-emacs-base)

;;; e-emacs-base.el ends here

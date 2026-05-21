;;; e-base.el --- Default base layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Base layer for workspace file and shell tools.

;;; Code:

(require 'e-file-capabilities)
(require 'e-layers)

(defconst e-base-instructions
  "Use base file and shell tools for workspace files and shell commands. Use Emacs buffer tools for live buffer state."
  "Default instructions contributed by the base layer.")

(defun e-base-layer-create (&optional directory)
  "Create the base layer rooted at DIRECTORY or `default-directory'."
  (let ((root (file-name-as-directory
               (expand-file-name (or directory default-directory)))))
    (e-layer-create
     :id 'base
     :name "Base"
     :capabilities (list (e-base-guidance-capability-create e-base-instructions)
                         (e-file-inspection-capability-create root)
                         (e-file-mutation-capability-create root)
                         (e-shell-process-capability-create root))
     :instructions nil
     :tools nil
     :context-providers nil
     :skills nil
     :prompts nil)))

(provide 'e-base)

;;; e-base.el ends here

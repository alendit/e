;;; e-base.el --- OS base layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; OS base layer for workspace file and shell tools.

;;; Code:

(require 'e-file-capabilities)
(require 'e-layers)

(defconst e-base-instructions
  "Use OS base file and shell tools for workspace files and shell commands."
  "Default instructions contributed by the OS base guidance capability.")

(defun e-base-layer-create (&optional directory)
  "Create the OS base layer rooted at DIRECTORY or `default-directory'."
  (let ((root (file-name-as-directory
               (expand-file-name (or directory default-directory)))))
    (e-layer-create
     :id 'os-base
     :name "OS Base"
     :capabilities (list (e-base-guidance-capability-create
                          e-base-instructions
                          :instruction-priority 230)
                         (e-file-inspection-capability-create root)
                         (e-file-mutation-capability-create root)
                         (e-shell-process-capability-create root)))))

(provide 'e-base)

;;; e-base.el ends here

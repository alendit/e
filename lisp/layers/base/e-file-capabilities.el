;;; e-file-capabilities.el --- File and process capabilities for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability constructors for workspace file inspection, file mutation, and
;; shell/process execution.

;;; Code:

(require 'e-base-tools)
(require 'e-capabilities)

(defun e-file-capabilities--directory (directory)
  "Return normalized capability root DIRECTORY."
  (file-name-as-directory
   (expand-file-name (or directory default-directory))))

(defun e-base-guidance-capability-create (instructions)
  "Create a base guidance capability carrying INSTRUCTIONS."
  (e-capability-create
   :id 'base-guidance
   :name "Base Guidance"
   :instructions instructions))

(defun e-file-inspection-capability-create (&optional directory)
  "Create a read-only file inspection capability rooted at DIRECTORY."
  (let ((root (e-file-capabilities--directory directory)))
    (e-capability-create
     :id 'file-inspection
     :name "File Inspection"
     :tools (list (lambda (registry)
                    (e-base-tools-register-read registry root))))))

(defun e-file-mutation-capability-create (&optional directory)
  "Create a file mutation capability rooted at DIRECTORY."
  (let ((root (e-file-capabilities--directory directory)))
    (e-capability-create
     :id 'file-mutation
     :name "File Mutation"
     :tools (list (lambda (registry)
                    (e-base-tools-register-write registry root)
                    (e-base-tools-register-edit registry root))))))

(defun e-shell-process-capability-create (&optional directory)
  "Create a shell process capability rooted at DIRECTORY."
  (let ((root (e-file-capabilities--directory directory)))
    (e-capability-create
     :id 'shell-process
     :name "Shell Process"
     :tools (list (lambda (registry)
                    (e-base-tools-register-bash registry root))))))

(provide 'e-file-capabilities)

;;; e-file-capabilities.el ends here

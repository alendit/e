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
(require 'e-harness)

(defun e-file-capabilities--directory (directory)
  "Return normalized capability root DIRECTORY."
  (file-name-as-directory
   (expand-file-name (or directory default-directory))))

(defun e-file-capabilities--context-directory (fallback context)
  "Return session project root from CONTEXT, or FALLBACK."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (turn-id (plist-get context :turn-id))
         (root (and (e-harness-p harness)
                    session-id
                    (e-harness-project-root harness session-id turn-id))))
    (e-file-capabilities--directory (or root fallback))))

(defun e-file-capabilities--context-roots (fallback context)
  "Return the active workspace roots for CONTEXT as a list.
The first element is the primary project root (used for relative paths); the
rest are configured extra workspace roots.  Falls back to FALLBACK as the sole
root when no session roots are resolvable."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (turn-id (plist-get context :turn-id))
         (roots (and (e-harness-p harness)
                     session-id
                     (e-harness-workspace-roots harness session-id turn-id))))
    (or roots (list (e-file-capabilities--directory fallback)))))

(cl-defun e-base-guidance-capability-create
    (instructions &key instruction-priority)
  "Create a base guidance capability carrying INSTRUCTIONS."
  (e-capability-create
   :id 'base-guidance
   :name "Base Guidance"
   :instruction-priority instruction-priority
   :instructions instructions))

(defun e-file-handling-capability-create (&optional directory)
  "Create a file handling capability rooted at DIRECTORY.

Registers read/write/edit `file://' resource methods plus the
`resource_sync_status' tool, scoped to the workspace roots."
  (let ((root (e-file-capabilities--directory directory)))
    (e-capability-create
     :id 'file-handling
     :name "File Handling"
     :tools (list (lambda (registry &rest context)
                    (e-base-tools-register-resource-sync-status
                     registry
                     (e-file-capabilities--context-roots root context))))
     :resource-methods
     (list (e-capability-resource-method-provider-create
            :handler
            (lambda (registry &rest context)
              (e-base-tools-register-file-resource
               registry
               (e-file-capabilities--context-roots root context))))))))

(defun e-shell-process-capability-create (&optional directory)
  "Create a shell process capability rooted at DIRECTORY."
  (let ((root (e-file-capabilities--directory directory)))
    (e-capability-create
     :id 'shell-process
     :name "Shell Process"
     :tools (list (lambda (registry &rest context)
                    (e-base-tools-register-bash
                     registry
                     (e-file-capabilities--context-directory root context)))))))

(provide 'e-file-capabilities)

;;; e-file-capabilities.el ends here

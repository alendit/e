;;; e-emacs-capabilities.el --- Emacs capabilities for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability constructors for Emacs awareness, buffer access, buffer mutation,
;; elisp evaluation, and future selection context.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-context)
(require 'e-emacs-tools)
(require 'e-layers)

(defconst e-emacs-base-instructions
  "You are running inside Emacs. Use buffer tools for buffer inspection and live buffer edits. Buffer edits do not save files; call save_buffer when persistence is required.

When editing a file that may be open in Emacs, prefer live buffer tools over direct file writes. If you write a file directly and a live file-backed buffer exists for it, sync or reload the buffer before reporting completion. If you edit a live file-backed buffer and the change should persist, save the buffer. Before finalizing, check visible buffers that correspond to modified resources."
  "Default instructions contributed by Emacs awareness capabilities.")

(defun e-emacs-capabilities-visible-buffer-context ()
  "Return a readable summary of visible Emacs buffers."
  (let ((buffers (e-emacs-tools-buffer-metadata-list t)))
    (concat
     "Visible Emacs buffers:\n"
     (if buffers
         (mapconcat
          (lambda (buffer)
            (format "- %s mode=%s file=%s modified=%s visible=%s"
                    (plist-get buffer :name)
                    (plist-get buffer :mode)
                    (or (plist-get buffer :file) "nil")
                    (if (plist-get buffer :modified) "true" "false")
                    (if (plist-get buffer :visible) "true" "false")))
          buffers
          "\n")
       "- none"))))

(defun e-emacs-visible-buffers-context-provider ()
  "Return the visible-buffer context provider for Emacs awareness."
  (e-context-provider-create
   :name 'visible-buffers
   :priority 320
   :build (cl-function
           (lambda (&key harness session-id turn-id)
             (ignore harness session-id turn-id)
             (list (list :role 'system
                         :content
                         (e-emacs-capabilities-visible-buffer-context)))))))

(defun e-emacs-awareness-capability-create ()
  "Create an Emacs awareness capability."
  (e-capability-create
   :id 'emacs-awareness
   :name "Emacs Awareness"
   :instruction-priority 300
   :instructions e-emacs-base-instructions
   :context-providers (list (e-emacs-visible-buffers-context-provider))))

(defun e-buffer-read-capability-create ()
  "Create a capability for reading live Emacs buffers."
  (e-capability-create
   :id 'buffer-read
   :name "Buffer Read"
   :tools (list #'e-emacs-tools-register-list-buffers)
   :resource-methods (list #'e-emacs-tools-register-buffer-read-resource)))

(defun e-buffer-edit-capability-create ()
  "Create a capability for mutating live Emacs buffers."
  (e-capability-create
   :id 'buffer-edit
   :name "Buffer Edit"
   :tools (list #'e-emacs-tools-register-save-buffer)
   :resource-methods (list #'e-emacs-tools-register-buffer-resource)))

(defun e-elisp-eval-capability-create ()
  "Create a capability for explicit Emacs Lisp evaluation."
  (e-capability-create
   :id 'elisp-eval
   :name "Elisp Eval"
   :tools (list #'e-emacs-tools-register-elisp-eval)))

(defun e-selection-context-capability-create ()
  "Create a placeholder capability for future selection context."
  (e-capability-create
   :id 'selection-context
   :name "Selection Context"))

(defun e-emacs-layer-create ()
  "Create the conservative Emacs layer preset."
  (e-layer-create
   :id 'emacs
   :name "Emacs"
   :capabilities (list (e-emacs-awareness-capability-create)
                       (e-buffer-read-capability-create)
                       (e-selection-context-capability-create))))

(defun e-emacs-operator-layer-create ()
  "Create the Emacs operator layer preset."
  (let ((emacs-layer (e-emacs-layer-create)))
    (e-layer-create
     :id 'emacs-operator
     :name "Emacs Operator"
     :capabilities (append
                    (e-layer-capabilities emacs-layer)
                    (list (e-buffer-edit-capability-create)
                          (e-elisp-eval-capability-create))))))

(provide 'e-emacs-capabilities)

;;; e-emacs-capabilities.el ends here

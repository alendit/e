;;; e-emacs-base.el --- Default Emacs layer for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The MVP Emacs base layer contributes Emacs-aware instructions, automatic
;; visible-buffer context, and concrete Emacs tools.

;;; Code:

(require 'cl-lib)
(require 'e-context)
(require 'e-emacs-tools)
(require 'e-layers)

(defconst e-emacs-base-instructions
  "You are running inside Emacs. Use buffer tools for buffer inspection and live buffer edits. Buffer edits do not save files; call save_buffer when persistence is required."
  "Default instructions contributed by the emacs-base layer.")

(defun e-emacs-base--visible-buffer-context ()
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

(defun e-emacs-base-visible-buffers-context-provider ()
  "Return the visible-buffer context provider for the emacs-base layer."
  (e-context-provider-create
   :name 'visible-buffers
   :build (cl-function
           (lambda (&key harness session-id turn-id)
             (ignore harness session-id turn-id)
             (list (list :role 'system
                         :content (e-emacs-base--visible-buffer-context)))))))

(defun e-emacs-base-layer-create ()
  "Create the MVP emacs-base layer."
  (e-layer-create
   :id 'emacs-base
   :name "Emacs Base"
   :instructions e-emacs-base-instructions
   :tools (list #'e-emacs-tools-register-defaults)
   :context-providers (list (e-emacs-base-visible-buffers-context-provider))
   :skills nil
   :prompts nil))

(provide 'e-emacs-base)

;;; e-emacs-base.el ends here

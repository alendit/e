;;; e-emacs-tools.el --- Harmless Emacs tools for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Low-risk concrete tools.  M2 intentionally avoids file writes, process
;; execution, elisp evaluation, buffer edits, and harness mutation tools.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-tools)

(define-error 'e-emacs-tools-buffer-missing "Emacs buffer is missing")
(define-error 'e-emacs-tools-edit-invalid "Emacs buffer edit is invalid")
(define-error 'e-emacs-tools-save-invalid "Emacs buffer cannot be saved")

(defun e-emacs-tools--buffer (name)
  "Return live buffer NAME or signal an explicit tool error."
  (or (get-buffer name)
      (signal 'e-emacs-tools-buffer-missing
              (list (format "No buffer named %s" name)))))

(defun e-emacs-tools--buffer-visible-p (buffer)
  "Return non-nil when BUFFER is visible in a live window."
  (and (get-buffer-window buffer t) t))

(defun e-emacs-tools--buffer-metadata (buffer)
  "Return metadata for BUFFER."
  (with-current-buffer buffer
    (list :name (buffer-name buffer)
          :mode (symbol-name major-mode)
          :file buffer-file-name
          :file-backed (and buffer-file-name t)
          :modified (buffer-modified-p buffer)
          :visible (e-emacs-tools--buffer-visible-p buffer))))

(defun e-emacs-tools-buffer-metadata-list (&optional visible-only)
  "Return metadata for live buffers.
When VISIBLE-ONLY is non-nil, include only buffers visible in windows."
  (let ((buffers nil))
    (dolist (buffer (buffer-list))
      (when (or (not visible-only)
                (e-emacs-tools--buffer-visible-p buffer))
        (push (e-emacs-tools--buffer-metadata buffer) buffers)))
    (nreverse buffers)))

(defun e-emacs-tools--argument-string (arguments key)
  "Return required string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp value)))
    value))

(defun e-emacs-tools--range-start (arguments)
  "Return requested buffer range start from ARGUMENTS."
  (or (plist-get arguments :start) 1))

(defun e-emacs-tools--range-end (arguments)
  "Return requested buffer range end from ARGUMENTS or nil."
  (plist-get arguments :end))

(defun e-emacs-tools--read-range (arguments)
  "Return validated buffer range from ARGUMENTS."
  (let ((start (e-emacs-tools--range-start arguments))
        (requested-end (e-emacs-tools--range-end arguments)))
    (let ((exclusive-end (if requested-end
                             (1+ requested-end)
                           (point-max))))
      (unless (and (integerp start)
                   (integerp exclusive-end)
                   (<= (point-min) start exclusive-end (point-max)))
        (signal 'args-out-of-range (list start requested-end)))
      (list :start start
            :exclusive-end exclusive-end
            :reported-end (or requested-end (1- exclusive-end))))))

(defun e-emacs-tools--replacement-count (old-text)
  "Return match positions for OLD-TEXT in current buffer."
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward old-text nil t)
        (push (cons (match-beginning 0) (match-end 0)) matches)))
    (nreverse matches)))

(defun e-emacs-tools--read-forms (code)
  "Read all elisp forms from CODE."
  (let ((position 0)
        (forms nil)
        read-result)
    (while (< position (length code))
      (setq read-result (read-from-string code position))
      (push (car read-result) forms)
      (setq position (cdr read-result))
      (while (and (< position (length code))
                  (memq (aref code position) '(?\s ?\t ?\n ?\r)))
        (setq position (1+ position))))
    (nreverse forms)))

(defun e-emacs-tools-register-current-time (registry)
  "Register a harmless `current-time' tool in REGISTRY."
  (e-tools-register
   registry
   :name "current_time"
   :description "Return the current local time as a readable string."
   :parameters '(:type "object"
                 :properties nil)
   :handler (lambda (_arguments)
              (current-time-string))))

(defun e-emacs-tools-register-list-buffers (registry)
  "Register a tool that lists live Emacs buffers in REGISTRY."
  (e-tools-register
   registry
   :name "list_buffers"
   :description "Return live Emacs buffer names and metadata."
   :parameters '(:type "object"
                 :properties (:visible_only (:type "boolean")))
   :handler (lambda (arguments)
              (list :buffers
                    (e-emacs-tools-buffer-metadata-list
                     (plist-get arguments :visible_only))))))

(defun e-emacs-tools-register-read-buffer (registry)
  "Register a tool that reads live Emacs buffer text in REGISTRY."
  (e-tools-register
   registry
   :name "read_buffer"
   :description "Read full or ranged contents from a named live Emacs buffer."
   :parameters '(:type "object"
                 :properties (:name (:type "string")
                              :start (:type "integer")
                              :end (:type "integer"))
                 :required ["name"])
   :handler
   (lambda (arguments)
     (let ((name (e-emacs-tools--argument-string arguments :name)))
       (with-current-buffer (e-emacs-tools--buffer name)
         (let* ((range (e-emacs-tools--read-range arguments))
                (start (plist-get range :start))
                (exclusive-end (plist-get range :exclusive-end))
                (reported-end (plist-get range :reported-end)))
           (list :name name
                 :content (buffer-substring-no-properties start exclusive-end)
                 :start start
                 :end reported-end)))))))

(defun e-emacs-tools-register-write-buffer (registry)
  "Register a tool that replaces live Emacs buffer text in REGISTRY."
  (e-tools-register
   registry
   :name "write_buffer"
   :description "Replace the live contents of a named Emacs buffer without saving."
   :parameters '(:type "object"
                 :properties (:name (:type "string")
                              :content (:type "string"))
                 :required ["name" "content"])
   :handler
   (lambda (arguments)
     (let ((name (e-emacs-tools--argument-string arguments :name))
           (content (e-emacs-tools--argument-string arguments :content)))
       (with-current-buffer (e-emacs-tools--buffer name)
         (erase-buffer)
         (insert content)
         (list :name name
               :chars (length content)
               :saved nil))))))

(defun e-emacs-tools-register-edit-buffer (registry)
  "Register an exact replacement tool for live Emacs buffers in REGISTRY."
  (e-tools-register
   registry
   :name "edit_buffer"
   :description "Replace one exact old-text match in a named Emacs buffer without saving."
   :parameters '(:type "object"
                 :properties (:name (:type "string")
                              :old_text (:type "string")
                              :new_text (:type "string"))
                 :required ["name" "old_text" "new_text"])
   :handler
   (lambda (arguments)
     (let ((name (e-emacs-tools--argument-string arguments :name))
           (old-text (e-emacs-tools--argument-string arguments :old_text))
           (new-text (e-emacs-tools--argument-string arguments :new_text)))
       (when (string-empty-p old-text)
         (signal 'e-emacs-tools-edit-invalid '("old_text must not be empty")))
       (when (equal old-text new-text)
         (signal 'e-emacs-tools-edit-invalid '("old_text and new_text are identical")))
       (with-current-buffer (e-emacs-tools--buffer name)
         (let ((matches (e-emacs-tools--replacement-count old-text)))
           (pcase (length matches)
             (0 (signal 'e-emacs-tools-edit-invalid '("old_text was not found")))
             (1 (let ((match (car matches)))
                  (goto-char (car match))
                  (delete-region (car match) (cdr match))
                  (insert new-text)
                  (list :name name
                        :replacements 1
                        :saved nil)))
             (_ (signal 'e-emacs-tools-edit-invalid
                        '("old_text matched more than once"))))))))))

(defun e-emacs-tools-register-save-buffer (registry)
  "Register a tool that saves file-backed Emacs buffers in REGISTRY."
  (e-tools-register
   registry
   :name "save_buffer"
   :description "Save a named file-backed Emacs buffer using its existing file path."
   :parameters '(:type "object"
                 :properties (:name (:type "string"))
                 :required ["name"])
   :handler
   (lambda (arguments)
     (let ((name (e-emacs-tools--argument-string arguments :name)))
       (with-current-buffer (e-emacs-tools--buffer name)
         (unless buffer-file-name
           (signal 'e-emacs-tools-save-invalid
                   (list (format "Buffer %s does not visit a file" name))))
         (save-buffer)
         (list :name name
               :file buffer-file-name
               :saved t))))))

(defun e-emacs-tools-register-run-elisp (registry)
  "Register a tool that evaluates explicit Emacs Lisp in REGISTRY."
  (e-tools-register
   registry
   :name "run_elisp"
   :description "Evaluate explicit Emacs Lisp in Emacs and return the printed result."
   :parameters '(:type "object"
                 :properties (:code (:type "string"))
                 :required ["code"])
   :handler
   (lambda (arguments)
     (let* ((code (e-emacs-tools--argument-string arguments :code))
            (forms (e-emacs-tools--read-forms code))
            result)
       (dolist (form forms)
         (setq result (eval form t)))
       (list :result (prin1-to-string result))))))

(defun e-emacs-tools-register-defaults (registry)
  "Register default concrete Emacs tools in REGISTRY."
  (e-emacs-tools-register-current-time registry)
  (e-emacs-tools-register-list-buffers registry)
  (e-emacs-tools-register-read-buffer registry)
  (e-emacs-tools-register-write-buffer registry)
  (e-emacs-tools-register-edit-buffer registry)
  (e-emacs-tools-register-save-buffer registry)
  (e-emacs-tools-register-run-elisp registry)
  registry)

(provide 'e-emacs-tools)

;;; e-emacs-tools.el ends here

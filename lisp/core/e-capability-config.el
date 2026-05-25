;;; e-capability-config.el --- Capability-scoped configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability-owned configuration resolution.  Layers may pass construction
;; defaults, but option contracts are declared by the capability owner.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function e-harness-effective-capability-config "e-harness"
                  (harness capability-id options &key session-id directory
                           overrides))

(define-error 'e-capability-config-unknown-option
  "Unknown capability config option")
(define-error 'e-capability-config-invalid-value
  "Invalid capability config value")

(defgroup e-capability-config nil
  "Capability-scoped configuration for e."
  :group 'e
  :prefix "e-capability-config-")

(defcustom e-capability-config nil
  "Global capability configuration.
Each entry is shaped as (CAPABILITY-ID . PLIST), where CAPABILITY-ID is the
capability symbol and PLIST contains keyword options owned by that capability."
  :type '(alist :key-type symbol :value-type plist)
  :group 'e-capability-config)

(cl-defstruct (e-capability-config-option
               (:constructor e-capability-config-option-create))
  key
  type
  default
  documentation
  normalizer
  validator)

(defvar e-capability-config-known-options nil
  "Alist mapping capability ids to declared option specs.")

(defun e-capability-config-register-options (capability-id options)
  "Register declared OPTIONS for CAPABILITY-ID."
  (setf (alist-get capability-id e-capability-config-known-options)
        options)
  options)

(defun e-capability-config-registered-options (capability-id)
  "Return registered option specs for CAPABILITY-ID."
  (alist-get capability-id e-capability-config-known-options))

(defun e-capability-config--value-safe-p (value)
  "Return non-nil when VALUE is safe in directory-local config."
  (cond
   ((or (null value) (stringp value) (symbolp value)
        (numberp value) (eq value t))
    t)
   ((listp value)
    (cl-every #'e-capability-config--value-safe-p value))
   (t nil)))

(defun e-capability-config-safe-local-value-p (value)
  "Return non-nil when VALUE is a safe `e-capability-config' value."
  (and
   (listp value)
   (cl-every
    (lambda (entry)
      (and (consp entry)
           (symbolp (car entry))
           (let ((plist (cdr entry)))
             (and (listp plist)
                  (zerop (% (length plist) 2))
                  (cl-loop for (key val) on plist by #'cddr
                           always (and (keywordp key)
                                       (e-capability-config--value-safe-p
                                        val)))))))
    value)))

(put 'e-capability-config
     'safe-local-variable
     #'e-capability-config-safe-local-value-p)

(defun e-capability-config-directory-source (directory)
  "Return directory-local `e-capability-config' source for DIRECTORY.
The returned plist has `:source', `:directory', and `:value' keys.  Return nil
when DIRECTORY has no actual directory-local `e-capability-config' binding."
  (when directory
    (let ((root (file-name-as-directory (expand-file-name directory))))
      (when (file-directory-p root)
        (with-temp-buffer
          (let ((default-directory root))
            (hack-dir-local-variables-non-file-buffer)
            (when (local-variable-p 'e-capability-config (current-buffer))
              (list :source 'directory-local
                    :directory root
                    :value e-capability-config))))))))

(defun e-capability-config--directory-value (directory)
  "Return directory-local `e-capability-config' for DIRECTORY, or nil."
  (plist-get (e-capability-config-directory-source directory) :value))

(defun e-capability-config--plist-for (capability-id config)
  "Return CAPABILITY-ID plist from CONFIG."
  (let ((entry (assq capability-id config)))
    (and entry (cdr entry))))

(defun e-capability-config--spec-key (spec)
  "Return option key for SPEC."
  (e-capability-config-option-key spec))

(defun e-capability-config--default-plist (options)
  "Return defaults plist for option SPECS."
  (let (plist)
    (dolist (spec options)
      (setq plist
            (plist-put plist
                       (e-capability-config-option-key spec)
                       (e-capability-config-option-default spec))))
    plist))

(defun e-capability-config--validate-known-keys
    (capability-id plist options)
  "Signal if PLIST contains unknown keys for CAPABILITY-ID."
  (let ((known (mapcar #'e-capability-config--spec-key options)))
    (unless (zerop (% (length plist) 2))
      (signal 'e-capability-config-invalid-value
              (list capability-id plist)))
    (cl-loop for (key _value) on plist by #'cddr
             unless (memq key known)
             do (signal 'e-capability-config-unknown-option
                        (list capability-id key)))))

(defun e-capability-config--merge-plist (left right)
  "Return LEFT with RIGHT key/value pairs overriding earlier values."
  (let ((result left))
    (cl-loop for (key value) on right by #'cddr
             do (setq result (plist-put result key value)))
    result))

(defun e-capability-config--normalize-value (capability-id spec value)
  "Normalize and validate VALUE for CAPABILITY-ID option SPEC."
  (let* ((normalizer (e-capability-config-option-normalizer spec))
         (validator (e-capability-config-option-validator spec))
         (normalized (if normalizer (funcall normalizer value) value)))
    (when (and validator (not (funcall validator normalized)))
      (signal 'e-capability-config-invalid-value
              (list capability-id
                    (e-capability-config-option-key spec)
                    normalized)))
    normalized))

(defun e-capability-config--normalize-plist
    (capability-id plist options)
  "Normalize PLIST values for CAPABILITY-ID according to OPTIONS."
  (let ((result plist))
    (dolist (spec options)
      (let ((key (e-capability-config-option-key spec)))
        (when (plist-member result key)
          (setq result
                (plist-put
                 result
                 key
                 (e-capability-config--normalize-value
                  capability-id spec (plist-get result key)))))))
    result))

(cl-defun e-capability-config-resolve
    (capability-id options &key directory runtime-config overrides)
  "Resolve CAPABILITY-ID config for OPTIONS.
Precedence is option defaults, global `e-capability-config', directory-local
config under DIRECTORY, runtime config, then explicit OVERRIDES."
  (let* ((global (e-capability-config--plist-for
                  capability-id e-capability-config))
         (project (e-capability-config--plist-for
                   capability-id
                   (e-capability-config--directory-value directory)))
         (parts (list (e-capability-config--default-plist options)
                      global
                      project
                      runtime-config
                      overrides))
         (resolved nil))
    (dolist (part parts)
      (when part
        (e-capability-config--validate-known-keys
         capability-id part options)
        (setq resolved (e-capability-config--merge-plist resolved part))))
    (e-capability-config--normalize-plist capability-id resolved options)))

(defun e-capability-config-string-list (value)
  "Normalize VALUE to a list of strings."
  (cond
   ((null value) nil)
   ((and (listp value) (cl-every #'stringp value)) value)
   ((stringp value) (list value))
   (t value)))

(defun e-capability-config-string-list-p (value)
  "Return non-nil when VALUE is nil or a string list."
  (or (null value)
      (and (listp value) (cl-every #'stringp value))))

(defun e-capability-config-customize ()
  "Open Customize for `e-capability-config'."
  (interactive)
  (customize-variable 'e-capability-config))

(defun e-capability-config-format (capability-id config)
  "Return a human-readable description for CAPABILITY-ID CONFIG."
  (format "%s\n\n%S"
          (symbol-name capability-id)
          config))

(defun e-capability-config--buffer-harness ()
  "Return the active buffer harness when one is bound."
  (and (boundp 'e-current-harness) e-current-harness))

(defun e-capability-config--buffer-session-id ()
  "Return the active buffer session id when one is bound."
  (and (boundp 'e-chat-session-id) e-chat-session-id))

(defun e-capability-config-describe
    (&optional capability-id directory options harness session-id)
  "Describe effective config for CAPABILITY-ID in DIRECTORY.
When OPTIONS is nil, use registered option specs for CAPABILITY-ID.
When HARNESS is non-nil, include harness-local runtime config for SESSION-ID."
  (interactive
   (let* ((ids (mapcar (lambda (entry) (symbol-name (car entry)))
                       e-capability-config-known-options))
          (id (intern
               (completing-read "Capability: " ids nil t nil nil
                                (car ids)))))
     (list id
           nil
           nil
           (e-capability-config--buffer-harness)
           (e-capability-config--buffer-session-id))))
  (let* ((id (or capability-id
                 (user-error "Capability id is required")))
         (specs (or options (e-capability-config-registered-options id)))
         (active-harness
          (or harness (e-capability-config--buffer-harness)))
         (active-session-id
          (or session-id (e-capability-config--buffer-session-id)))
         (config
          (if (and active-harness
                   (fboundp 'e-harness-effective-capability-config))
              (e-harness-effective-capability-config
               active-harness
               id
               specs
               :session-id active-session-id
               :directory directory)
            (e-capability-config-resolve
             id specs :directory (or directory default-directory))))
         (text (e-capability-config-format id config)))
    (if (called-interactively-p 'interactive)
        (with-help-window "*e-capability-config*"
          (princ text))
      text)))

(provide 'e-capability-config)

;;; e-capability-config.el ends here

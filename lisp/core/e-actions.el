;;; e-actions.el --- Context-bound action dispatch for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capability actions are shell-facing semantic operations.  This module gives
;; Elisp one ergonomic dispatcher over the active harness/session action
;; surface.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-tools)

(define-error 'e-actions-error "e action dispatch error")
(define-error 'e-actions-no-active-harness
  "e action dispatch requires an active harness" 'e-actions-error)
(define-error 'e-actions-no-active-session
  "e action dispatch requires an active session" 'e-actions-error)
(define-error 'e-actions-unknown-capability
  "e action capability is not active" 'e-actions-error)
(define-error 'e-actions-unknown-action
  "e capability action is not available" 'e-actions-error)
(define-error 'e-actions-invalid-arguments
  "e action arguments are invalid" 'e-actions-error)

(defun e-actions--plist-p (value)
  "Return non-nil when VALUE is a keyword plist."
  (and (listp value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun e-actions--argument-key (key)
  "Return KEY as a keyword for action arguments."
  (cond
   ((keywordp key) key)
   ((symbolp key) (intern (concat ":" (symbol-name key))))
   ((stringp key) (intern (concat ":" (string-remove-prefix ":" key))))
   (t (signal 'e-actions-invalid-arguments
              (list (format "Unsupported action argument key: %S" key))))))

(defun e-actions--arguments-plist (value)
  "Return VALUE normalized to an action argument plist."
  (cond
   ((null value) nil)
   ((e-actions--plist-p value)
    (let (result)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (setq result
                (append result
                        (list key (e-actions--argument-value item))))))
      result))
   ((hash-table-p value)
    (let (result)
      (maphash (lambda (key item)
                 (setq result
                       (append result
                               (list (e-actions--argument-key key)
                                     (e-actions--argument-value item)))))
               value)
      result))
   ((and (listp value)
         (cl-every #'consp value))
    (let (result)
      (dolist (entry value)
        (setq result
              (append result
                      (list (e-actions--argument-key (car entry))
                            (e-actions--argument-value (cdr entry))))))
      result))
   (t
    (signal 'e-actions-invalid-arguments
            (list (format "Action arguments must be an object/plist, got %S"
                          value))))))

(defun e-actions--argument-value (value)
  "Return VALUE with nested JSON objects normalized to plists."
  (cond
   ((hash-table-p value)
    (e-actions--arguments-plist value))
   ((and (listp value)
         (not (e-actions--plist-p value))
         (cl-every #'consp value))
    (e-actions--arguments-plist value))
   ((vectorp value)
    (vconcat (mapcar #'e-actions--argument-value value)))
   (t value)))

(defun e-actions--capability-id (value)
  "Return VALUE as a capability id symbol."
  (cond
   ((and (symbolp value) (not (keywordp value))) value)
   ((stringp value) (intern value))
   (t (signal 'e-actions-invalid-arguments
              (list (format "Capability must be a symbol or string, got %S"
                            value))))))

(defun e-actions--action-key (value)
  "Return VALUE as an action keyword."
  (cond
   ((keywordp value) value)
   ((symbolp value) (intern (concat ":" (symbol-name value))))
   ((stringp value) (intern (concat ":" (string-remove-prefix ":" value))))
   (t (signal 'e-actions-invalid-arguments
              (list (format "Action must be a keyword/symbol/string, got %S"
                            value))))))

(defun e-actions--context (options)
  "Return dispatcher context from OPTIONS and `e-tools-current-context'."
  (or (plist-get options :context)
      (e-tools-current-context)
      nil))

(defun e-actions--harness (context options)
  "Return active harness from CONTEXT or OPTIONS."
  (let ((harness (or (plist-get options :harness)
                     (plist-get context :harness))))
    (unless (e-harness-p harness)
      (signal 'e-actions-no-active-harness
              (list "Action dispatch requires :harness or active tool context")))
    harness))

(defun e-actions--session-id (context options)
  "Return active session id from CONTEXT or OPTIONS."
  (or (plist-get options :session-id)
      (plist-get context :session-id)))

(defun e-actions--turn-id (context options)
  "Return active turn id from CONTEXT or OPTIONS."
  (or (plist-get options :turn-id)
      (plist-get context :turn-id)))

(defun e-actions--find-capability (harness session-id turn-id capability-id)
  "Return active CAPABILITY-ID from HARNESS SESSION-ID TURN-ID."
  (cl-find-if (lambda (capability)
                (eq (e-capability-id capability) capability-id))
              (e-harness-effective-capabilities harness session-id turn-id)))

(defun e-actions--schema-required (parameters)
  "Return required keys from action PARAMETERS schema."
  (let ((required (plist-get parameters :required)))
    (cond
     ((vectorp required) (append required nil))
     ((listp required) required)
     (t nil))))

(defun e-actions--validate-arguments (action arguments)
  "Validate ARGUMENTS against ACTION's compact parameter schema."
  (when (e-action-p action)
    (let ((parameters (e-action-parameters action)))
      (dolist (name (e-actions--schema-required parameters))
        (let ((key (e-actions--argument-key name)))
          (unless (plist-member arguments key)
            (signal 'e-actions-invalid-arguments
                    (list (format "Missing required action argument: %s"
                                  (if (stringp name) name (symbol-name name)))))))))))

(defun e-actions--default-action (handler)
  "Return legacy action descriptor for HANDLER."
  (e-action-create
   :handler handler
   :caller (lambda (_action-context arguments)
             (funcall handler arguments))))

(defun e-actions-dispatch (capability action &optional arguments options)
  "Dispatch CAPABILITY ACTION with ARGUMENTS and return a dispatch plist.
OPTIONS may include `:harness', `:session-id', `:turn-id', or `:context'."
  (let* ((options (or options nil))
         (context (e-actions--context options))
         (harness (e-actions--harness context options))
         (session-id (e-actions--session-id context options))
         (turn-id (e-actions--turn-id context options))
         (capability-id (e-actions--capability-id capability))
         (action-key (e-actions--action-key action))
         (capability-object
          (e-actions--find-capability harness session-id turn-id capability-id)))
    (unless capability-object
      (signal 'e-actions-unknown-capability
              (list (format "Capability %S is not active" capability-id))))
    (let* ((entry (e-capabilities-action-spec capability-object action-key))
           (action-spec
            (cond
             ((e-action-p entry) entry)
             ((functionp entry) (e-actions--default-action entry))
             (t nil))))
      (unless action-spec
        (signal 'e-actions-unknown-action
                (list (format "Capability %S has no action %S"
                              capability-id action-key))))
      (when (and (e-action-requires-session action-spec)
                 (not (stringp session-id)))
        (signal 'e-actions-no-active-session
                (list (format "Action %S/%S requires an active session"
                              capability-id action-key))))
      (let* ((arguments (e-actions--arguments-plist arguments))
             (action-context (list :harness harness
                                   :session-id session-id
                                   :turn-id turn-id
                                   :capability capability-object
                                   :capability-id capability-id
                                   :action action-key
                                   :context context))
             result)
        (e-actions--validate-arguments action-spec arguments)
        (setq result
              (funcall (or (e-action-caller action-spec)
                           (lambda (_action-context args)
                             (funcall (e-action-handler action-spec) args)))
                       action-context
                       arguments))
        (list :capability capability-object
              :capability-id capability-id
              :action action-key
              :spec action-spec
              :result result)))))

(defun e-actions-call (capability action &optional arguments options)
  "Call active CAPABILITY ACTION with ARGUMENTS.
When OPTIONS omits `:harness' and `:session-id', dispatch uses the current
`e-tools-current-context'.  Return the raw action result."
  (plist-get
   (e-actions-dispatch capability action arguments options)
   :result))

(provide 'e-actions)

;;; e-actions.el ends here

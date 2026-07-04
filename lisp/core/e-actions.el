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
(require 'e-session)
(require 'e-tools)
(require 'e-work)

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
                                  (if (stringp name)
                                      name
                                    (symbol-name name)))))))))))

(defun e-actions--default-action (handler)
  "Return legacy action descriptor for HANDLER."
  (e-action-create
   :handler handler
   :caller (lambda (_action-context arguments)
             (funcall handler arguments))))

(defun e-actions--action-start (action)
  "Return ACTION's async start function, tolerating older live records."
  (and (e-action-p action)
       (>= (length action) 8)
       (e-action-start action)))

(defun e-actions--action-work (action)
  "Return ACTION's work spec, tolerating older live records."
  (and (e-action-p action)
       (>= (length action) 9)
       (e-action-work action)))

(defun e-actions--preview (value)
  "Return a compact printable preview for VALUE."
  (let* ((text (prin1-to-string value))
         (max-bytes 4096)
         (original-bytes (string-bytes text))
         (truncated (> original-bytes max-bytes))
         (preview
          (if truncated
              (let ((bytes 0)
                    (index 0)
                    (length (length text)))
                (while (and (< index length)
                            (let ((next-bytes
                                   (string-bytes
                                    (substring text index (1+ index)))))
                              (when (<= (+ bytes next-bytes) max-bytes)
                                (setq bytes (+ bytes next-bytes))
                                t)))
                  (setq index (1+ index)))
                (substring text 0 index))
            text)))
    (list :content preview
          :truncated truncated
          :original-bytes original-bytes
          :shown-bytes (string-bytes preview))))

(defun e-actions--parent-tool-call-id (context)
  "Return parent tool call id from CONTEXT, if any."
  (when-let ((tool-call (plist-get context :tool-call)))
    (plist-get tool-call :id)))

(defun e-actions--activity-payload
    (call-id capability-id action-key arguments context &rest fields)
  "Return action activity payload."
  (append
   (list :action-call-id call-id
         :capability-id capability-id
         :action action-key
         :parent-tool-call-id (e-actions--parent-tool-call-id context)
         :arguments (e-actions--preview arguments))
   fields))

(defun e-actions--emit-activity
    (harness session-id turn-id type payload)
  "Emit action activity TYPE with PAYLOAD when session context is durable."
  (when (and (e-harness-p harness)
             (stringp session-id)
             (stringp turn-id)
             (fboundp 'e-harness--emit-turn-event)
             (ignore-errors
               (e-session-get (e-harness-sessions harness) session-id)))
    (e-harness--emit-turn-event harness session-id turn-id type payload)))

(defun e-actions--error-payload-fields (err)
  "Return payload fields for ERR."
  (list :status 'error
        :error-class (car err)
        :message (error-message-string err)))

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
         (call-id (e-session-generate-ulid))
         (started-at (float-time))
         (capability-object
          (e-actions--find-capability harness session-id turn-id capability-id)))
    (e-actions--emit-activity
     harness session-id turn-id 'action-started
     (e-actions--activity-payload
      call-id capability-id action-key arguments context
      :status 'started))
    (condition-case err
        (progn
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
                                         :action-call-id call-id
                                         :context context))
                   (started-result
                    (list :status 'started
                          :action-call-id call-id
                          :capability capability-id
                          :action action-key))
                   result)
              (e-actions--validate-arguments action-spec arguments)
              (cl-labels
                  ((make-finish
                    (settled)
                    (lambda (value)
                      (unless (car settled)
                        (setcar settled t)
                        (e-actions--emit-activity
                         harness session-id turn-id 'action-finished
                         (e-actions--activity-payload
                          call-id capability-id action-key arguments context
                          :status 'ok
                          :elapsed-seconds (- (float-time) started-at)
                          :result (e-actions--preview value))))))
                   (make-fail
                    (settled)
                    (lambda (err)
                      (unless (car settled)
                        (setcar settled t)
                        (e-actions--emit-activity
                         harness session-id turn-id 'action-failed
                         (apply #'e-actions--activity-payload
                                call-id capability-id action-key
                                arguments context
                                (append
                                 (list :elapsed-seconds
                                       (- (float-time) started-at))
                                 (e-actions--error-payload-fields err))))))))
                (cond
                 ((e-work-spec-p (e-actions--action-work action-spec))
                  (let* ((settled (cons nil nil))
                         (work (e-actions--action-work action-spec))
                         (request
                          (e-work-start work arguments
                                        :context action-context
                                        :on-done (make-finish settled)
                                        :on-error (make-fail settled)))
                         (dispatch-result
                          (if (eq (plist-get (e-work-status request) :state)
                                  'finished)
                              (e-work-handle-result request)
                            started-result)))
                    (list :capability capability-object
                          :capability-id capability-id
                          :action action-key
                          :spec action-spec
                          :request request
                          :result dispatch-result)))
                 ((functionp (e-actions--action-start action-spec))
                  (let* ((settled (cons nil nil))
                         (start (e-actions--action-start action-spec))
                         (request
                          (funcall start action-context arguments
                                   :on-done (make-finish settled)
                                   :on-error (make-fail settled))))
                    (list :capability capability-object
                          :capability-id capability-id
                          :action action-key
                          :spec action-spec
                          :request request
                          :result started-result)))
                 (t
                  (setq result
                        (funcall (or (e-action-caller action-spec)
                                     (lambda (_action-context args)
                                       (funcall (e-action-handler action-spec)
                                                args)))
                                 action-context
                                 arguments))
                  (e-actions--emit-activity
                   harness session-id turn-id 'action-finished
                   (e-actions--activity-payload
                    call-id capability-id action-key arguments context
                    :status 'ok
                    :elapsed-seconds (- (float-time) started-at)
                    :result (e-actions--preview result)))
                  (list :capability capability-object
                        :capability-id capability-id
                        :action action-key
                        :spec action-spec
                        :result result)))))))
      (error
       (e-actions--emit-activity
        harness session-id turn-id 'action-failed
        (apply #'e-actions--activity-payload
               call-id capability-id action-key arguments context
               (append
                (list :elapsed-seconds (- (float-time) started-at))
                (e-actions--error-payload-fields err))))
       (signal (car err) (cdr err))))))

(defun e-actions-call (capability action &optional arguments options)
  "Call active CAPABILITY ACTION with ARGUMENTS.
When OPTIONS omits `:harness' and `:session-id', dispatch uses the current
`e-tools-current-context'.  Return the raw action result."
  (plist-get
   (e-actions-dispatch capability action arguments options)
   :result))

(provide 'e-actions)

;;; e-actions.el ends here

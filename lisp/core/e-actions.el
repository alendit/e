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
(require 'e-harness-activity)
(require 'e-session)
(require 'e-telemetry)
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
(define-error 'e-actions-invalid-spec
  "e action descriptor is invalid" 'e-actions-error)

(defun e-actions--proper-list-p (value)
  "Return non-nil when VALUE is a proper list.

An alist uses dotted pairs for entries, so `listp' alone is not enough to
distinguish an object from a JSON array represented as a Lisp list."
  (let ((tail value))
    (while (consp tail)
      (setq tail (cdr tail)))
    (null tail)))

(defun e-actions--plist-p (value)
  "Return non-nil when VALUE is a keyword plist."
  (and (e-actions--proper-list-p value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun e-actions--plist-array-p (value)
  "Return non-nil when VALUE is a non-empty list of keyword plists."
  (and (e-actions--proper-list-p value)
       (consp value)
       (cl-every #'e-actions--plist-p value)))

(defun e-actions--alist-p (value &optional allow-plist-elements)
  "Return non-nil when VALUE is an alist with action-compatible keys.

When ALLOW-PLIST-ELEMENTS is non-nil, a proper pair such as \=`(:name VALUE)
is accepted even though the same shape can also be one object in a list of
plists.  A declared object schema supplies that disambiguation."
  (and (e-actions--proper-list-p value)
       (or allow-plist-elements
           (not (e-actions--plist-array-p value)))
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (let ((key (car entry)))
                          (or (keywordp key)
                              (symbolp key)
                              (stringp key)))))
                 value)))

(defun e-actions--argument-key (key)
  "Return KEY as a keyword for action arguments."
  (cond
   ((keywordp key) key)
   ((symbolp key) (intern (concat ":" (symbol-name key))))
   ((stringp key) (intern (concat ":" (string-remove-prefix ":" key))))
   (t (signal 'e-actions-invalid-arguments
              (list (format "Unsupported action argument key: %S" key))))))

(defun e-actions--schema-type (schema)
  "Return the JSON type declared by SCHEMA, or nil."
  (and (e-actions--proper-list-p schema)
       (plist-get schema :type)))

(defun e-actions--schema-property-name (name)
  "Return the JSON property name represented by NAME."
  (cond
   ((keywordp name) (substring (symbol-name name) 1))
   ((symbolp name) (symbol-name name))
   ((stringp name) (string-remove-prefix ":" name))
   (t nil)))

(defun e-actions--alist-entry-value (entry &optional schema)
  "Return the value carried by alist ENTRY under SCHEMA.

Both dotted entries, such as \=`(NAME . VALUE), and two-element list entries,
such as \=`(NAME VALUE), are accepted.  A one-element list value remains a
list when SCHEMA declares an array; this preserves the distinction between a
scalar two-element alist entry and an array-valued property."
  (let ((tail (cdr entry)))
    (if (and (consp tail)
             (null (cdr tail))
             (not (equal (e-actions--schema-type schema) "array")))
        (car tail)
      tail)))

(defun e-actions--schema-property (schema key)
  "Return the child schema for KEY in object SCHEMA, when declared.

Provider adapters may represent `:properties' as a plist, alist, or hash
table.  Compare their JSON names rather than relying on one concrete Elisp
container or key spelling."
  (let* ((properties (and (e-actions--proper-list-p schema)
                          (plist-get schema :properties)))
         (target (e-actions--schema-property-name key)))
    (cond
     ((hash-table-p properties)
      (catch 'found
        (maphash
         (lambda (property child-schema)
           (when (equal (e-actions--schema-property-name property) target)
             (throw 'found child-schema)))
         properties)
        nil))
     ((e-actions--plist-p properties)
      (let ((rest properties)
            found)
        (while rest
          (let ((property (pop rest))
                (child-schema (pop rest)))
            (when (equal (e-actions--schema-property-name property) target)
              (setq found child-schema)
              (setq rest nil))))
        found))
     ((e-actions--alist-p properties)
      (let ((entry
             (cl-find-if
              (lambda (candidate)
                (equal (e-actions--schema-property-name (car candidate))
                       target))
              properties)))
        (when entry
          (e-actions--alist-entry-value entry)))))))

(defun e-actions--arguments-plist (value &optional schema)
  "Return VALUE normalized to an action argument plist using SCHEMA."
  (cond
   ((null value) nil)
   ((e-actions--plist-p value)
    (let ((rest value)
          result)
      (while rest
        (let* ((key (pop rest))
               (item (pop rest))
               (child-schema (e-actions--schema-property schema key)))
          (push key result)
          (push (e-actions--argument-value item child-schema) result)))
      (nreverse result)))
   ((hash-table-p value)
    (let (result)
      (maphash (lambda (key item)
                 (let* ((argument-key (e-actions--argument-key key))
                        (child-schema
                         (e-actions--schema-property schema argument-key)))
                   (push argument-key result)
                   (push (e-actions--argument-value item child-schema)
                         result)))
               value)
      (nreverse result)))
   ((e-actions--alist-p value
                         (equal (e-actions--schema-type schema) "object"))
    (let (result)
      (dolist (entry value)
        (let* ((argument-key (e-actions--argument-key (car entry)))
               (child-schema
                (e-actions--schema-property schema argument-key))
               (item (e-actions--alist-entry-value entry child-schema)))
          (push argument-key result)
          (push (e-actions--argument-value item child-schema) result)))
      (nreverse result)))
   (t
    (signal 'e-actions-invalid-arguments
            (list (format "Action arguments must be an object/plist, got %S"
                          value))))))

(defun e-actions--argument-value (value &optional schema)
  "Return VALUE normalized according to JSON SCHEMA.

Objects become keyword plists, while arrays retain their input list/vector
shape and recursively normalize each item.  When no schema is available,
hash tables, plists, and alists still identify objects; a list of such objects
therefore remains an array instead of being mistaken for an alist."
  (let ((type (e-actions--schema-type schema)))
    (cond
     ((null value) nil)
     ((hash-table-p value)
      (e-actions--arguments-plist value schema))
     ((equal type "object")
      (if (or (e-actions--plist-p value)
              (e-actions--alist-p value t))
          (e-actions--arguments-plist value schema)
        value))
     ((equal type "array")
      (let ((items-schema
             (and (e-actions--proper-list-p schema)
                  (plist-get schema :items))))
        (cond
         ((vectorp value)
          (vconcat
           (mapcar (lambda (item)
                     (e-actions--argument-value item items-schema))
                   (append value nil))))
         ((e-actions--proper-list-p value)
          (mapcar (lambda (item)
                    (e-actions--argument-value item items-schema))
                  value))
         (t value))))
     ((vectorp value)
      (vconcat
       (mapcar (lambda (item) (e-actions--argument-value item))
               (append value nil))))
     ((e-actions--plist-p value)
      (e-actions--arguments-plist value))
     ((e-actions--alist-p value)
      (e-actions--arguments-plist value))
     ((e-actions--proper-list-p value)
      (mapcar (lambda (item) (e-actions--argument-value item)) value))
     (t value))))

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
              (e-harness-effective-action-capabilities
               harness session-id turn-id
               :requested-capability-id capability-id)))

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
                                    (symbol-name name))))))))
      (when (eq (plist-get parameters :additionalProperties) :json-false)
        (let ((properties (plist-get parameters :properties)))
          (cl-loop for key in arguments by #'cddr do
                   (unless (plist-member properties key)
                     (signal 'e-actions-invalid-arguments
                             (list "Action arguments contain undeclared fields")))))))))

(defun e-actions--preview (value)
  "Return a compact redacted printable preview for VALUE."
  (e-telemetry-preview value))

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
             (fboundp 'e-harness-activity-emit-turn-event))
    (let ((store (e-harness-sessions harness)))
      (when (or (e-session-async-enabled-p store)
                (ignore-errors (e-session-local-state store session-id)))
        (e-harness-activity-emit-turn-event
         harness session-id turn-id type payload)))))

(defun e-actions--error-payload-fields (err)
  "Return payload fields for ERR."
  (list :status 'error
        :error-class (car err)
        :message-preview (e-telemetry-preview (e-work-error-message err))))

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
          (e-actions--find-capability harness session-id turn-id capability-id))
         activity-arguments
         failure-emitted)
    (condition-case err
        (progn
          (unless capability-object
            (signal 'e-actions-unknown-capability
                    (list (format "Capability %S is not active" capability-id))))
          (let ((action-spec
                 (e-capabilities-action-spec capability-object action-key)))
            (unless action-spec
              (signal 'e-actions-unknown-action
                      (list (format "Capability %S has no action %S"
                                    capability-id action-key))))
            (unless (e-action-p action-spec)
              (signal 'e-actions-invalid-spec
                      (list (format "Capability %S action %S is not an e-action descriptor"
                                    capability-id action-key))))
            (unless (e-work-spec-p (e-action-work action-spec))
              (signal 'e-actions-invalid-spec
                      (list (format "Capability %S action %S does not declare work"
                                    capability-id action-key))))
            (when (and (e-action-requires-session action-spec)
                       (not (stringp session-id)))
              (signal 'e-actions-no-active-session
                      (list (format "Action %S/%S requires an active session"
                                    capability-id action-key))))
            (let* ((arguments
                    (e-actions--arguments-plist
                     arguments (e-action-parameters action-spec)))
                   (action-context (list :harness harness
                                         :session-id session-id
                                         :turn-id turn-id
                                         :capability capability-object
                                         :capability-id capability-id
                                         :action action-key
                                         :action-call-id call-id
                                         :context context)))
              (e-actions--validate-arguments action-spec arguments)
              ;; Rejected arguments never cross the durable activity boundary.
              ;; Only a schema-valid payload is eligible for started/finished
              ;; telemetry; failures before this point retain no caller values.
              (setq activity-arguments arguments)
              (e-actions--emit-activity
               harness session-id turn-id 'action-started
               (e-actions--activity-payload
                call-id capability-id action-key activity-arguments context
                :status 'started))
              (cl-labels
                  ((make-finish
                    (settled)
                    (lambda (value)
                      (unless (car settled)
                        (setcar settled t)
                        (e-actions--emit-activity
                         harness session-id turn-id 'action-finished
                         (e-actions--activity-payload
                          call-id capability-id action-key activity-arguments context
                          :status 'ok
                          :elapsed-seconds (- (float-time) started-at)
                          :result (e-actions--preview value))))))
                   (make-fail
                    (settled)
                    (lambda (err)
                      (unless (car settled)
                        (setcar settled t)
                        (setq failure-emitted t)
                        (e-actions--emit-activity
                         harness session-id turn-id 'action-failed
                         (apply #'e-actions--activity-payload
                                call-id capability-id action-key
                                activity-arguments context
                                (append
                                 (list :elapsed-seconds
                                       (- (float-time) started-at))
                                 (e-actions--error-payload-fields err))))))))
                (let* ((settled (cons nil nil))
                       (work (e-action-work action-spec))
                       (request
                        (e-work-start work arguments
                                      :context action-context
                                      :on-done (make-finish settled)
                                      :on-error (make-fail settled)))
                       (dispatch-result
                        (pcase (plist-get (e-work-status request) :state)
                          ('finished (e-work-handle-result request))
                          ('failed
                           (let ((err (e-work-handle-error request)))
                             (signal (car err) (cdr err))))
                          ('cancelled
                           (signal 'e-work-cancelled (list request)))
                          (_
                           ;; Pending action work crosses the shell/model
                           ;; boundary by reference.  The generic detached
                           ;; registry is process-local coordination only; the
                           ;; action's eventual result remains owned by WORK.
                           (e-work-detach-register request)
                           (format "work:%s" (e-work-handle-id request))))))
                  (list :capability capability-object
                        :capability-id capability-id
                        :action action-key
                        :spec action-spec
                        :request request
                        :result dispatch-result))))))
      (error
       (unless failure-emitted
         (e-actions--emit-activity
          harness session-id turn-id 'action-failed
          (apply #'e-actions--activity-payload
                 call-id capability-id action-key activity-arguments context
                 (append
                  (list :elapsed-seconds (- (float-time) started-at))
                  (e-actions--error-payload-fields err)))))
       (signal (car err) (cdr err))))))

(defun e-actions-call (capability action &optional arguments options)
  "Call active CAPABILITY ACTION with ARGUMENTS.
When OPTIONS omits `:harness' and `:session-id', dispatch uses the current
`e-tools-current-context'.  Return the raw result when the action settles
immediately.  A still-pending action returns a generic `work:' reference whose
settlement and bounded inline result are observed through the top-level
`await' tool."
  (plist-get
   (e-actions-dispatch capability action arguments options)
   :result))

(provide 'e-actions)

;;; e-actions.el ends here

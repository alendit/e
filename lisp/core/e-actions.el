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
(require 'e-json)
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
(define-error 'e-actions-invalid-result
  "e action result is not canonical JSON" 'e-actions-error)
(define-error 'e-actions-invalid-spec
  "e action descriptor is invalid" 'e-actions-error)

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

(defun e-actions--validate-arguments (action arguments)
  "Validate canonical ARGUMENTS against ACTION's canonical schema.
Return ARGUMENTS unchanged.  The action boundary accepts only the shared
canonical JSON representation; it never reparses or reshapes Lisp containers."
  (unless (e-action-p action)
    (signal 'e-actions-invalid-spec (list "Not an e-action descriptor")))
  (condition-case error-data
      (e-json-schema-assert arguments (e-action-parameters action))
    ((e-json-error e-json-schema-error)
     (signal 'e-actions-invalid-arguments
             (list (error-message-string error-data))))))

(defun e-actions--validate-result (value)
  "Validate immediate structured action VALUE without reshaping it.
Action adapters own any mapping from domain records into canonical JSON.  The
dispatcher only enforces that the value crossing the model-facing boundary is
already canonical; strings, numbers, vectors, objects, and explicit sentinels
are returned unchanged.  A domain adapter may return an existing Work handle;
pending handles use the established detached `work:' reference contract, while
settled handles are validated recursively."
  (cond
   ((e-work-handle-p value)
    (pcase (plist-get (e-work-status value) :state)
      ('finished (e-actions--validate-result (e-work-handle-result value)))
      ('failed
       (let ((error-data (e-work-handle-error value)))
         (signal (car error-data) (cdr error-data))))
      ('cancelled (signal 'e-work-cancelled (list value)))
      (_
       (e-work-detach-register value)
       (format "work:%s" (e-work-handle-id value)))))
   (t
    (condition-case error-data
        (e-json-assert-value value)
      (e-json-error
       (signal 'e-actions-invalid-result
               (list (error-message-string error-data))))))))

(defun e-actions--preview (value)
  "Return a compact redacted printable preview for VALUE."
  (e-telemetry-preview value))

(defun e-actions--parent-tool-call-id (context)
  "Return parent tool call id from CONTEXT, if any."
  (when-let* ((tool-call (plist-get context :tool-call)))
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
            (let* ((arguments (e-actions--validate-arguments
                               action-spec arguments))
                   (action-context (list :harness harness
                                         :session-id session-id
                                         :turn-id turn-id
                                         :capability capability-object
                                         :capability-id capability-id
                                         :action action-key
                                         :action-call-id call-id
                                         :context context)))
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
                          ('finished
                           (e-actions--validate-result
                            (e-work-handle-result request)))
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

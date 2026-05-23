;;; e-tools.el --- Tool registry for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure tool registry and dispatch for core tool-call handling.

;;; Code:

(require 'cl-lib)

(cl-defstruct (e-tools-registry (:constructor e-tools-registry-create))
  (tools (make-hash-table :test 'equal))
  (order nil))

(cl-defstruct (e-tools-request (:constructor e-tools-request-create))
  cancel
  metadata)

(defun e-tools-cancel-request (request)
  "Cancel REQUEST when it has a tool cancellation function."
  (when-let ((cancel (and (e-tools-request-p request)
                          (e-tools-request-cancel request))))
    (funcall cancel)))

(cl-defun e-tools-register
    (registry &key name description parameters handler start)
  "Register tool NAME in REGISTRY with DESCRIPTION, PARAMETERS, HANDLER, and START.
HANDLER is a synchronous implementation.  START is a callback-driven async
implementation."
  (unless (or (functionp handler) (functionp start))
    (signal 'wrong-type-argument (list 'functionp (or handler start))))
  (unless (gethash name (e-tools-registry-tools registry))
    (setf (e-tools-registry-order registry)
          (append (e-tools-registry-order registry) (list name))))
  (puthash name
           (list :name name
                 :description description
                 :parameters parameters
                 :handler handler
                 :start start)
           (e-tools-registry-tools registry)))

(defun e-tools--empty-json-object ()
  "Return an empty object suitable for `json-encode'."
  (make-hash-table :test 'equal))

(defun e-tools--normalize-parameters (parameters)
  "Return tool PARAMETERS with valid JSON object defaults."
  (let ((normalized (copy-sequence
                     (or parameters
                         (list :type "object"
                               :properties (e-tools--empty-json-object))))))
    (when (and (equal (plist-get normalized :type) "object")
               (null (plist-get normalized :properties)))
      (plist-put normalized :properties (e-tools--empty-json-object)))
    normalized))

(defun e-tools-definitions (registry)
  "Return backend-neutral tool definitions for REGISTRY."
  (let ((definitions nil))
    (dolist (name (e-tools-registry-order registry))
      (let ((tool (gethash name (e-tools-registry-tools registry))))
        (push (list :type "function"
                    :name (plist-get tool :name)
                    :description (plist-get tool :description)
                    :parameters (e-tools--normalize-parameters
                                 (plist-get tool :parameters))
                    :strict :json-false)
              definitions)))
    (nreverse definitions)))

(defun e-tools--result (call status content &optional metadata)
  "Return a structured tool result for CALL with STATUS, CONTENT, and METADATA."
  (list :tool-call-id (plist-get call :id)
        :name (plist-get call :name)
        :status status
        :content content
        :metadata metadata))

(defun e-tools-execute (registry call)
  "Execute CALL against REGISTRY and return a structured tool result."
  (let ((done nil)
        (result nil)
        (failure nil))
    (e-tools-start
     registry
     call
     :on-done (lambda (value)
                (setq result value)
                (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.01))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(cl-defun e-tools-start
    (registry call &key on-done on-error on-request-start)
  "Start CALL against REGISTRY and report a structured result asynchronously.
ON-DONE receives the structured result.  ON-ERROR receives unexpected Emacs
condition lists.  ON-REQUEST-START receives an optional `e-tools-request'."
  (let* ((name (plist-get call :name))
         (tool (gethash name (e-tools-registry-tools registry))))
    (if (not tool)
        (let ((result (e-tools--result
                       call
                       'error
                       (format "Unknown tool: %s" name)
                       '(:error e-tool-missing))))
          (when on-done
            (funcall on-done result))
          nil)
      (let ((start (plist-get tool :start))
            (handler (plist-get tool :handler)))
        (cl-labels
            ((finish-ok
              (content)
              (when on-done
                (funcall on-done
                         (e-tools--result call 'ok content nil))))
             (finish-error
              (err)
              (when on-done
                (funcall on-done
                         (e-tools--result
                          call
                          'error
                          (error-message-string err)
                          (list :error (car err))))))
             (publish-request
              (request)
              (when (and request on-request-start)
                (funcall on-request-start request))))
          (condition-case err
              (if (functionp start)
                  (let ((reported-request nil))
                    (let ((request
                           (funcall
                            start
                            :arguments (plist-get call :arguments)
                            :on-done #'finish-ok
                            :on-error #'finish-error
                            :on-request-start
                            (lambda (request)
                              (setq reported-request request)
                              (publish-request request)))))
                      (when (and request (not (eq request reported-request)))
                        (publish-request request))
                      request))
                (let ((cancelled nil)
                      (timer nil)
                      request)
                  (setq request
                        (e-tools-request-create
                         :cancel (lambda ()
                                   (setq cancelled t)
                                   (when (timerp timer)
                                     (cancel-timer timer))
                                   t)
                         :metadata '(:transport timer
                                     :cancellable queued-only)))
                  (publish-request request)
                  (setq timer
                        (run-at-time
                         0 nil
                         (lambda ()
                           (unless cancelled
                             (condition-case err
                                 (finish-ok
                                  (funcall handler
                                           (plist-get call :arguments)))
                               (quit
                                (finish-error err))
                               (error
                                (finish-error err)))))))
                  request))
            (quit
             (finish-error err)
             nil)
            (error
             (finish-error err)
             nil)))))))

(provide 'e-tools)

;;; e-tools.el ends here

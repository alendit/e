;;; e-tools.el --- Tool registry for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure tool registry and dispatch for core tool-call handling.

;;; Code:

(require 'cl-lib)

(cl-defstruct (e-tools-registry (:constructor e-tools-registry-create))
  (tools (make-hash-table :test 'equal)))

(cl-defun e-tools-register (registry &key name description parameters handler)
  "Register tool NAME with DESCRIPTION, PARAMETERS, and HANDLER in REGISTRY."
  (unless (functionp handler)
    (signal 'wrong-type-argument (list 'functionp handler)))
  (puthash name
           (list :name name
                 :description description
                 :parameters parameters
                 :handler handler)
           (e-tools-registry-tools registry)))

(defun e-tools-definitions (registry)
  "Return backend-neutral tool definitions for REGISTRY."
  (let ((definitions nil))
    (maphash
     (lambda (_name tool)
       (push (list :type "function"
                   :name (plist-get tool :name)
                   :description (plist-get tool :description)
                   :parameters (or (plist-get tool :parameters)
                                   '(:type "object"
                                     :properties nil))
                   :strict :json-false)
             definitions))
     (e-tools-registry-tools registry))
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
  (let* ((name (plist-get call :name))
         (tool (gethash name (e-tools-registry-tools registry))))
    (if (not tool)
        (e-tools--result call
                         'error
                         (format "Unknown tool: %s" name)
                         '(:error e-tool-missing))
      (condition-case err
          (e-tools--result call
                           'ok
                           (funcall (plist-get tool :handler)
                                    (plist-get call :arguments))
                           nil)
        (error
         (e-tools--result call
                          'error
                          (error-message-string err)
                          (list :error (car err))))))))

(provide 'e-tools)

;;; e-tools.el ends here

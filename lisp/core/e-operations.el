;;; e-operations.el --- Standard resource operation contracts for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Operation contracts define shared model-facing semantics.  Resource methods
;; implement these contracts for URI schemes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (e-operation (:constructor e-operation-create))
  id
  tool-name
  description
  parameters
  dispatch)

(defun e-operations--argument-string (arguments key)
  "Return required string argument KEY from ARGUMENTS."
  (let ((value (plist-get arguments key)))
    (unless (stringp value)
      (signal 'wrong-type-argument (list 'stringp key)))
    value))

(defconst e-operation-read
  (e-operation-create
   :id 'read
   :tool-name "read"
   :description "Read a URI-addressed resource."
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Resource URI to read, such as file://README.md or buffer://*scratch*.")
                              :range (:type "object"
                                      :description "Optional resource-specific range. Supported units depend on the URI scheme."
                                      :properties (:unit (:type "string"
                                                         :description "Range unit, such as line or offset.")
                                                   :start (:type "number"
                                                           :description "1-based start position in the selected unit.")
                                                   :end (:type "number"
                                                         :description "Inclusive 1-based end position, when supported.")
                                                   :limit (:type "number"
                                                           :description "Maximum number of units to return, when supported."))
                                      :required ["unit" "start"]))
                 :required ["uri"])
   :dispatch (lambda (call arguments)
               (funcall call
                        (e-operations--argument-string arguments :uri)
                        (plist-get arguments :range)))))

(defconst e-operation-write
  (e-operation-create
   :id 'write
   :tool-name "write"
   :description "Write content to a URI-addressed resource."
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Resource URI to write, such as file://README.md or buffer://*scratch*.")
                              :content (:type "string"
                                        :description "Complete replacement content."))
                 :required ["uri" "content"])
   :dispatch (lambda (call arguments)
               (funcall call
                        (e-operations--argument-string arguments :uri)
                        (e-operations--argument-string arguments :content)))))

(defconst e-operation-edit
  (e-operation-create
   :id 'edit
   :tool-name "edit"
   :description "Edit a URI-addressed text resource using exact text replacements."
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Resource URI to edit, such as file://README.md or buffer://*scratch*.")
                              :edits (:type "array"
                                      :description "Exact text replacements to apply to the resource."
                                      :items (:type "object"
                                              :properties (:oldText (:type "string"
                                                                   :description "Exact current text to replace. Must match exactly once.")
                                                           :newText (:type "string"
                                                                   :description "Replacement text."))
                                              :required ["oldText" "newText"])))
                 :required ["uri" "edits"])
   :dispatch (lambda (call arguments)
               (funcall call
                        (e-operations--argument-string arguments :uri)
                        (plist-get arguments :edits)))))

(defconst e-operations-standard
  (list e-operation-read e-operation-write e-operation-edit)
  "Standard resource operation contracts exposed by the harness when active.")

(defun e-operation-id-of (operation)
  "Return OPERATION id."
  (cond
   ((e-operation-p operation) (e-operation-id operation))
   ((symbolp operation) operation)
   (t (signal 'wrong-type-argument (list 'e-operation-p operation)))))

(provide 'e-operations)

;;; e-operations.el ends here

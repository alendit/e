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

(defun e-operations--present-options (arguments keys)
  "Return plist from ARGUMENTS containing only present KEYS."
  (let (options)
    (dolist (key keys (nreverse options))
      (when (plist-member arguments key)
        (setq options (cons (plist-get arguments key)
                            (cons key options)))))))

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
   :description (concat
                 "Write complete content to a URI-addressed resource. "
                 "For every URI scheme listed below, write creates missing parent paths "
                 "and the target resource, or overwrites existing content.")
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

(defconst e-operation-glob
  (e-operation-create
   :id 'glob
   :tool-name "glob"
   :description "List URI-addressed resources matching an optional glob pattern."
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Resource URI root to list, such as file://lisp/ or buffer://.")
                              :pattern (:type "string"
                                        :description "Optional glob pattern to match beneath the resource root, such as *.el.")
                              :case-sensitive (:type "boolean"
                                               :description "When non-nil or omitted, match the glob pattern case-sensitively.")
                              :limit (:type "number"
                                      :description "Maximum number of resources to return."))
                 :required ["uri"])
   :dispatch (lambda (call arguments)
               (funcall call
                        (e-operations--argument-string arguments :uri)
                        (plist-get arguments :pattern)
                        (plist-get arguments :limit)
                        (plist-get arguments :case-sensitive)))))

(defconst e-operation-search
  (e-operation-create
   :id 'search
   :tool-name "search"
   :description "Search URI-addressed resources for text matches."
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Resource URI root to search, such as file://lisp/ or buffer://.")
                              :query (:type "string"
                                      :description "Facade search query. Literal characters match literally; * is a non-whitespace wildcard.")
                              :glob (:type "string"
                                     :description "Optional glob pattern limiting resources to search.")
                              :case-sensitive (:type "boolean"
                                               :description "When non-nil, match query case-sensitively.")
                              :whole-word (:type "boolean"
                                           :description "When non-nil, matches must start and end at word boundaries around the full query.")
                              :multiline (:type "boolean"
                                          :description "When non-nil, whitespace gaps may cross line boundaries.")
                              :limit (:type "number"
                                      :description "Maximum number of matches to return."))
                 :required ["uri" "query"])
   :dispatch (lambda (call arguments)
               (funcall call
                        (e-operations--argument-string arguments :uri)
                        (e-operations--argument-string arguments :query)
                        (e-operations--present-options
                         arguments
                         '(:glob :case-sensitive :whole-word
                           :multiline :limit))))))

(defconst e-operations-standard
  (list e-operation-read
        e-operation-write
        e-operation-edit
        e-operation-glob
        e-operation-search)
  "Standard resource operation contracts exposed by the harness when active.")

(defun e-operation-id-of (operation)
  "Return OPERATION id."
  (cond
   ((e-operation-p operation) (e-operation-id operation))
   ((symbolp operation) operation)
   (t (signal 'wrong-type-argument (list 'e-operation-p operation)))))

(provide 'e-operations)

;;; e-operations.el ends here

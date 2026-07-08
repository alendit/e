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

(defun e-operations--coerce-edits (edits)
  "Return EDITS normalized to a list of edit plists.

Some models send a single edit object instead of a one-element array; wrap a
lone edit object (a plist bearing `:oldText') into a single-item list.  A
well-formed list is returned unchanged.

Stringified arguments are not handled here.  Providers that JSON-stringify
nested tool arguments (notably Bedrock) are reparsed against the tool schema in
`e-tools--coerce-arguments' before dispatch, so `edits' arrives as data."
  (if (and (listp edits) (plist-member edits :oldText))
      (list edits)
    edits))

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
   :description (concat
                 "Edit a URI-addressed text resource using exact text replacements. "
                 "`edits' is always a JSON array of {oldText, newText} objects, even "
                 "for a single change: pass [{\"oldText\": ..., \"newText\": ...}]. "
                 "Do not send `edits' as a JSON-encoded string and do not send a bare "
                 "object; it must be a real array.")
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Resource URI to edit, such as file://README.md or buffer://*scratch*.")
                              :edits (:type "array"
                                      :description "Array of exact text replacements to apply, in order. Always an array of objects, even for one edit: [{\"oldText\": \"...\", \"newText\": \"...\"}]. Never a JSON-encoded string and never a bare object."
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
                        (e-operations--coerce-edits
                         (plist-get arguments :edits))))))

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
                                      :description "Maximum number of resources to return.")
                              :sort-by (:type "string"
                                        :description "Optional resource metadata field to sort by, such as default, name, uri, created-at, updated-at, or a scheme-specific field.")
                              :sort-order (:type "string"
                                           :enum ["desc" "asc"]
                                           :description "Optional sort order. Defaults to desc for explicit sort fields.")
                              :created-after (:type "string"
                                              :description "Inclusive ISO 8601 created-at lower bound, when the scheme supports it.")
                              :created-before (:type "string"
                                               :description "Inclusive ISO 8601 created-at upper bound, when the scheme supports it.")
                              :updated-after (:type "string"
                                              :description "Inclusive ISO 8601 updated-at lower bound, when the scheme supports it.")
                              :updated-before (:type "string"
                                               :description "Inclusive ISO 8601 updated-at upper bound, when the scheme supports it."))
                 :required ["uri"])
   :dispatch (lambda (call arguments)
               (funcall call
                        (e-operations--argument-string arguments :uri)
                        (plist-get arguments :pattern)
                        (plist-get arguments :limit)
                        (plist-get arguments :case-sensitive)
                        (plist-get arguments :sort-by)
                        (plist-get arguments :sort-order)
                        (plist-get arguments :created-after)
                        (plist-get arguments :created-before)
                        (plist-get arguments :updated-after)
                        (plist-get arguments :updated-before)))))

(defconst e-operation-search
  (e-operation-create
   :id 'search
   :tool-name "search"
   :description "Search URI-addressed resources for text matches."
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Resource URI root to search, such as file://lisp/ or buffer://.")
                              :query (:type "string"
                                      :description "Ranked lexical query. All whitespace-separated terms must match; literal characters match literally; * is a non-whitespace wildcard.")
                              :glob (:type "string"
                                     :description "Optional glob pattern limiting resources to search.")
                              :case-sensitive (:type "boolean"
                                               :description "When non-nil, match query case-sensitively.")
                              :whole-word (:type "boolean"
                                           :description "When non-nil, matches must start and end at word boundaries around the full query.")
                              :multiline (:type "boolean"
                                          :description "When non-nil, whitespace gaps may cross line boundaries.")
                              :limit (:type "number"
                                      :description "Maximum number of text matches to return.")
                              :resource-sort-by (:type "string"
                                                 :description "Optional resource metadata field used to order resource candidates before search.")
                              :resource-sort-order (:type "string"
                                                    :enum ["desc" "asc"]
                                                    :description "Optional resource candidate sort order.")
                              :resource-limit (:type "number"
                                               :description "Optional maximum number of resource candidates to search before returning text matches.")
                              :created-after (:type "string"
                                              :description "Inclusive ISO 8601 created-at lower bound for resource candidates, when the scheme supports it.")
                              :created-before (:type "string"
                                               :description "Inclusive ISO 8601 created-at upper bound for resource candidates, when the scheme supports it.")
                              :updated-after (:type "string"
                                              :description "Inclusive ISO 8601 updated-at lower bound for resource candidates, when the scheme supports it.")
                              :updated-before (:type "string"
                                               :description "Inclusive ISO 8601 updated-at upper bound for resource candidates, when the scheme supports it."))
                 :required ["uri" "query"])
   :dispatch (lambda (call arguments)
               (funcall call
                        (e-operations--argument-string arguments :uri)
                        (e-operations--argument-string arguments :query)
                        (e-operations--present-options
                         arguments
                         '(:glob :case-sensitive :whole-word
                           :multiline :limit :resource-sort-by
                           :resource-sort-order :resource-limit
                           :created-after :created-before
                           :updated-after :updated-before))))))

(defconst e-operation-table-of-content
  (e-operation-create
   :id 'table-of-content
   :tool-name "table_of_content"
   :description (concat
                 "Table of content for a URI-addressed resource using wot. "
                 "For file-backed resources this calls wot on the backing file when safe. "
                 "For in-memory resources this pipes resource text to wot --stdin. "
                 "session:// is not supported.")
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Resource URI to outline, such as file://README.org, buffer://*scratch*, or e://capability/refs/name.md.")
                              :max-depth (:type "number"
                                          :description "Optional wot --max-depth value.")
                              :max-items (:type "number"
                                          :description "Optional wot --max-items value.")
                              :min-lines (:type "number"
                                          :description "Optional wot --min-lines value.")
                              :format (:type "string"
                                       :enum ["markdown" "json"]
                                       :description "Optional wot output format. Defaults to markdown.")
                              :language (:type "string"
                                         :description "Optional wot language, useful for stdin-backed resources when inference is ambiguous.")
                              :lenient (:type "boolean"
                                        :description "When non-nil, pass --lenient to wot."))
                 :required ["uri"])
   :dispatch (lambda (call arguments)
               (funcall call
                        (e-operations--argument-string arguments :uri)
                        (e-operations--present-options
                         arguments
                         '(:max-depth :max-items :min-lines
                           :format :language :lenient))))))

(defconst e-operations-standard
  (list e-operation-read
        e-operation-write
        e-operation-edit
        e-operation-glob
        e-operation-search
        e-operation-table-of-content)
  "Standard resource operation contracts exposed by the harness when active.")

(defun e-operation-id-of (operation)
  "Return OPERATION id."
  (cond
   ((e-operation-p operation) (e-operation-id operation))
   ((symbolp operation) operation)
   (t (signal 'wrong-type-argument (list 'e-operation-p operation)))))

(provide 'e-operations)

;;; e-operations.el ends here

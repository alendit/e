;;; e-json.el --- Canonical JSON values for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the one JSON-like value representation shared by provider,
;; model-facing, and persistence boundaries.  It deliberately does not map
;; arbitrary Elisp containers into that representation: objects are keyword
;; plists, arrays are vectors, and the two explicit sentinels preserve JSON
;; false and null.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(define-error 'e-json-error "Canonical JSON value error")
(define-error 'e-json-schema-error
  "Canonical JSON Schema error"
  'e-json-error)

(defconst e-json-false :json-false
  "Canonical Elisp value for the JSON false value.")

(defconst e-json-null :json-null
  "Canonical Elisp value for the JSON null value.")

(defconst e-json--supported-schema-keywords
  '(:type :properties :required :additionalProperties :items :enum
    :description :title :format :minLength :maxLength :nonBlank :singleLine
    :pattern :minimum :maximum :minItems :maxItems)
  "JSON Schema keywords supported by the canonical tool boundary.")

(defun e-json--error (format-string &rest arguments)
  "Signal `e-json-error' with FORMAT-STRING and ARGUMENTS."
  (signal 'e-json-error (list (apply #'format format-string arguments))))

(defun e-json--finite-number-p (value)
  "Return non-nil when VALUE is a finite canonical JSON number."
  (or (integerp value)
      (and (floatp value)
           (= value value)
           (not (= (abs value) 1.0e+INF)))))

(defun e-json--text-string-p (value)
  "Return non-nil when VALUE is JSON character text rather than raw bytes.

ASCII-only unibyte strings are unambiguous character text.  Non-ASCII byte
strings must be decoded by their owning ingress boundary before entering the
canonical JSON value model."
  (and (stringp value)
       (if (multibyte-string-p value)
           (cl-loop for character across value
                    always
                    (or (< character #xd800)
                        (<= #xe000 character #x10ffff)))
         (not (string-match-p "[^\0-\177]" value)))))

(defun e-json--validate (value active)
  "Validate canonical JSON VALUE using ACTIVE compound values."
  (cond
   ((or (null value)
        (eq value t)
        (eq value e-json-false)
        (eq value e-json-null))
    value)
   ((stringp value)
    (unless (e-json--text-string-p value)
      (e-json--error
       "JSON strings must contain Unicode character text, not raw bytes"))
    value)
   ((numberp value)
    (unless (e-json--finite-number-p value)
      (e-json--error "JSON numbers must be finite"))
    value)
   ((vectorp value)
    (when (gethash value active)
      (e-json--error "Canonical JSON values cannot contain cycles"))
    (puthash value t active)
    (unwind-protect
        (let ((index 0))
          (while (< index (length value))
            (e-json--validate (aref value index) active)
            (setq index (1+ index)))
          value)
      (remhash value active)))
   ((consp value)
    (let ((cursor value)
          (keys nil)
          (nodes nil))
      (unwind-protect
          (progn
            (while cursor
              (unless (consp cursor)
                (e-json--error
                 "JSON objects must be proper even keyword plists"))
              (when (gethash cursor active)
                (e-json--error "Canonical JSON values cannot contain cycles"))
              (puthash cursor t active)
              (push cursor nodes)
              (let ((key (car cursor))
                    (rest (cdr cursor)))
                (unless (keywordp key)
                  (e-json--error
                   "JSON object keys must be interned keywords"))
                (unless (consp rest)
                  (e-json--error
                   "JSON objects must be proper even keyword plists"))
                (when (memq key keys)
                  (e-json--error "JSON objects cannot contain duplicate keys"))
                (push key keys)
                (e-json--validate (car rest) active)
                (setq cursor (cdr rest))))
            value)
        (dolist (node nodes)
          (remhash node active)))))
   (t
    (e-json--error "Unsupported canonical JSON value"))))

;;;###autoload
(defun e-json-value-p (value)
  "Return non-nil when VALUE has the exact canonical JSON shape."
  (condition-case nil
      (progn
        (e-json-assert-value value)
        t)
    (e-json-error nil)))

;;;###autoload
(defun e-json-assert-value (value)
  "Return VALUE, or signal `e-json-error' when it is not canonical JSON."
  (e-json--validate value (make-hash-table :test 'eq)))

;;;###autoload
(defun e-json-parse-string (string)
  "Parse JSON STRING into the canonical JSON value representation."
  (unless (stringp string)
    (e-json--error "JSON input must be a string"))
  (condition-case error-data
      (let ((value
             (json-parse-string string
                                :object-type 'plist
                                :array-type 'array
                                :null-object e-json-null
                                :false-object e-json-false)))
        (e-json-assert-value value))
    (e-json-error
     (signal (car error-data) (cdr error-data)))
    (error
     (e-json--error "Invalid JSON text: %s"
                    (error-message-string error-data)))))

;;;###autoload
(defun e-json-serialize (value)
  "Serialize canonical JSON VALUE into composable JSON character text.

Emacs's `json-serialize' returns UTF-8 wire bytes for non-ASCII content.  This
canonical boundary decodes those bytes immediately so its result may safely
become a string value inside another canonical JSON value.  Transport adapters
remain responsible for encoding the final text to wire bytes."
  (e-json-assert-value value)
  (condition-case error-data
      (let ((encoded
             (json-serialize value
                             :null-object e-json-null
                             :false-object e-json-false)))
        (if (multibyte-string-p encoded)
            encoded
          (decode-coding-string encoded 'utf-8-unix t)))
    (error
     (e-json--error "Cannot serialize canonical JSON value: %s"
                    (error-message-string error-data)))))

(defun e-json--schema-error (path format-string &rest arguments)
  "Signal `e-json-schema-error' for PATH with FORMAT-STRING and ARGUMENTS."
  (signal 'e-json-schema-error
          (list (format "Schema at %s %s"
                        (or path "<root>")
                        (apply #'format format-string arguments)))))

(defun e-json--schema-plist-p (value)
  "Return non-nil when VALUE is a canonical JSON object plist."
  (or (null value)
      (and (listp value)
           (cl-evenp (length value))
           (cl-loop for (key _item) on value by #'cddr
                    always (keywordp key)))))

(defun e-json--schema-key-name (key)
  "Return the JSON text name for canonical schema property KEY."
  (substring (symbol-name key) 1))

(defun e-json--schema-keyword (name)
  "Return canonical keyword for JSON property NAME, or nil."
  (when (stringp name)
    (intern (concat ":" name))))

(defun e-json--schema-keywords (schema)
  "Return schema keys from canonical SCHEMA."
  (let (keys)
    (while schema
      (push (pop schema) keys)
      (pop schema))
    keys))

(defun e-json--schema-check-bound (value keyword path)
  "Check non-negative numeric schema VALUE for KEYWORD at PATH."
  (when (plist-member value keyword)
    (let ((bound (plist-get value keyword)))
      (unless (and (integerp bound) (>= bound 0))
        (e-json--schema-error
         path "%s must be a non-negative integer" keyword)))))

(defun e-json--schema-check-object (schema path)
  "Validate object-valued keywords in SCHEMA at PATH."
  (when (plist-member schema :properties)
    (let ((properties (plist-get schema :properties)))
      (unless (e-json--schema-plist-p properties)
        (e-json--schema-error path ":properties must be a keyword plist"))
      (let ((rest properties))
        (while rest
          (let ((key (pop rest))
                (child (pop rest)))
            (unless (keywordp key)
              (e-json--schema-error path ":properties keys must be keywords"))
            (e-json--schema-validate child
                                      (format "%s.properties.%s"
                                              path
                                              (e-json--schema-key-name key))))))))
  (when (plist-member schema :required)
    (let ((required (plist-get schema :required)))
      (unless (vectorp required)
        (e-json--schema-error path ":required must be a vector"))
      (dotimes (index (length required))
        (unless (stringp (aref required index))
          (e-json--schema-error
           path ":required entries must be strings")))))
  (when (plist-member schema :additionalProperties)
    (let ((additional (plist-get schema :additionalProperties)))
      (unless (memq additional (list t e-json-false))
        (e-json--schema-error
         path ":additionalProperties must be true or :json-false")))))

(defun e-json--schema-check-array (schema path)
  "Validate array-valued keywords in SCHEMA at PATH."
  (when (plist-member schema :items)
    (e-json--schema-validate (plist-get schema :items)
                             (format "%s.items" path)))
  (e-json--schema-check-bound schema :minItems path)
  (e-json--schema-check-bound schema :maxItems path))

(defun e-json--schema-check-string (schema path)
  "Validate string-valued keywords in SCHEMA at PATH."
  (dolist (keyword '(:minLength :maxLength))
    (e-json--schema-check-bound schema keyword path))
  (dolist (keyword '(:nonBlank :singleLine))
    (when (plist-member schema keyword)
      (unless (memq (plist-get schema keyword) (list t e-json-false))
        (e-json--schema-error
         path "%s must be true or :json-false" keyword))))
  (when (plist-member schema :pattern)
    (unless (stringp (plist-get schema :pattern))
      (e-json--schema-error path ":pattern must be a string"))))

(defun e-json--schema-check-number (schema path)
  "Validate numeric bounds in SCHEMA at PATH."
  (dolist (keyword '(:minimum :maximum))
    (when (plist-member schema keyword)
      (let ((bound (plist-get schema keyword)))
        (unless (and (numberp bound) (e-json--finite-number-p bound))
          (e-json--schema-error
           path "%s must be a finite number" keyword))))))

(defun e-json--schema-validate (schema path)
  "Validate canonical SCHEMA recursively at PATH.
Nil is the canonical empty JSON Schema, which accepts every canonical value."
  (when schema
    (condition-case error-data
        (e-json-assert-value schema)
      (e-json-error
       (signal (car error-data) (cdr error-data))))
    (unless (e-json--schema-plist-p schema)
      (e-json--schema-error path "must be a keyword plist"))
    (dolist (keyword (e-json--schema-keywords schema))
      (unless (memq keyword e-json--supported-schema-keywords)
        (e-json--schema-error path "has unsupported keyword %s" keyword)))
    (let ((type (and (plist-member schema :type)
                     (plist-get schema :type))))
      (when (and type
                 (not (member type '("string" "number" "integer"
                                     "boolean" "object" "array" "null"))))
        (e-json--schema-error path "has unsupported :type %S" type))
      (when (and (plist-member schema :type) (not (stringp type)))
        (e-json--schema-error path ":type must be a string")))
    (when (plist-member schema :enum)
      (unless (vectorp (plist-get schema :enum))
        (e-json--schema-error path ":enum must be a vector")))
    (dolist (keyword '(:description :title :format))
      (when (plist-member schema keyword)
        (unless (stringp (plist-get schema keyword))
          (e-json--schema-error path "%s must be a string" keyword))))
    (e-json--schema-check-object schema path)
    (e-json--schema-check-array schema path)
    (e-json--schema-check-string schema path)
    (e-json--schema-check-number schema path)))

(defun e-json--schema-value-type-p (value type)
  "Return non-nil when canonical VALUE has JSON Schema TYPE."
  (pcase type
    ("string" (stringp value))
    ("number" (and (numberp value) (e-json--finite-number-p value)))
    ("integer" (integerp value))
    ("boolean" (memq value (list t e-json-false)))
    ("object" (e-json--schema-plist-p value))
    ("array" (vectorp value))
    ("null" (eq value e-json-null))
    (_ t)))

(defun e-json--schema-value-error (path format-string &rest arguments)
  "Signal a value/schema mismatch at PATH."
  (signal 'e-json-schema-error
          (list (format "Value at %s %s"
                        (or path "<root>")
                        (apply #'format format-string arguments)))))

(defun e-json--schema-value-validate (value schema path)
  "Validate canonical VALUE against canonical SCHEMA at PATH."
  (when schema
    (let ((type (and (plist-member schema :type)
                     (plist-get schema :type))))
      (unless (e-json--schema-value-type-p value type)
        (e-json--schema-value-error path "does not match type %s" type))
      (when (plist-member schema :enum)
        (unless (cl-some (lambda (allowed) (equal allowed value))
                         (append (plist-get schema :enum) nil))
          (e-json--schema-value-error path "is not an allowed enum value")))
      (when (and (stringp value)
                 (plist-member schema :minLength)
                 (< (length value) (plist-get schema :minLength)))
        (e-json--schema-value-error path "is shorter than :minLength"))
      (when (and (stringp value)
                 (plist-member schema :maxLength)
                 (> (length value) (plist-get schema :maxLength)))
        (e-json--schema-value-error path "is longer than :maxLength"))
      (when (and (stringp value)
                 (plist-get schema :nonBlank)
                 (string-empty-p (string-trim value)))
        (e-json--schema-value-error path "must not be blank"))
      (when (and (stringp value)
                 (plist-get schema :singleLine)
                 (string-match-p "[\n\r]" value))
        (e-json--schema-value-error path "must be one line"))
      (when (and (stringp value)
                 (plist-member schema :pattern)
                 (not (string-match-p (plist-get schema :pattern) value)))
        (e-json--schema-value-error path "does not match :pattern"))
      (when (and (numberp value)
                 (plist-member schema :minimum)
                 (< value (plist-get schema :minimum)))
        (e-json--schema-value-error path "is below :minimum"))
      (when (and (numberp value)
                 (plist-member schema :maximum)
                 (> value (plist-get schema :maximum)))
        (e-json--schema-value-error path "is above :maximum"))
      (when (and (vectorp value)
                 (plist-member schema :minItems)
                 (< (length value) (plist-get schema :minItems)))
        (e-json--schema-value-error path "has fewer than :minItems items"))
      (when (and (vectorp value)
                 (plist-member schema :maxItems)
                 (> (length value) (plist-get schema :maxItems)))
        (e-json--schema-value-error path "has more than :maxItems items"))
      (when (and (e-json--schema-plist-p value)
                 (equal type "object"))
        (let ((properties (plist-get schema :properties))
              (additional (if (plist-member schema :additionalProperties)
                              (plist-get schema :additionalProperties)
                            t)))
          (dolist (name (append (or (plist-get schema :required) []) nil))
            (let ((key (e-json--schema-keyword name)))
              (unless (and key (plist-member value key))
                (e-json--schema-value-error
                 path "is missing required property %s" name))))
          (let ((rest value))
            (while rest
              (let* ((key (pop rest))
                     (item (pop rest))
                     (child (and properties (plist-get properties key))))
                (if (and properties (plist-member properties key))
                    (e-json--schema-value-validate
                     item child
                     (format "%s.%s" (or path "<root>")
                             (e-json--schema-key-name key)))
                  (when (eq additional e-json-false)
                    (e-json--schema-value-error
                     path "contains undeclared property %s"
                     (e-json--schema-key-name key)))))))))
      (when (and (vectorp value) (plist-member schema :items))
        (dotimes (index (length value))
          (e-json--schema-value-validate
           (aref value index)
           (plist-get schema :items)
           (format "%s[%d]" (or path "<root>") index)))))))

;;;###autoload
(defun e-json-schema-assert-schema (schema)
  "Return canonical SCHEMA, or signal for an unsupported schema shape.
Nil is the canonical empty schema and accepts every canonical JSON value."
  (when schema
    (e-json-assert-value schema)
    (e-json--schema-validate schema "<root>"))
  schema)

;;;###autoload
(defun e-json-schema-assert (value schema)
  "Return canonical VALUE after validating it against canonical SCHEMA.
SCHEMA is the deliberately small JSON Schema subset used by model-facing
tools.  Neither SCHEMA nor VALUE is reshaped.  Signal `e-json-error' for a
noncanonical input and `e-json-schema-error' for an invalid schema or mismatch."
  (e-json-assert-value value)
  (e-json-schema-assert-schema schema)
  (e-json--schema-value-validate value schema "<root>")
  value)

;;;###autoload
(defun e-json-schema-value-p (value schema)
  "Return non-nil when canonical VALUE satisfies canonical SCHEMA."
  (condition-case nil
      (progn
        (e-json-schema-assert value schema)
        t)
    (e-json-error nil)
    (e-json-schema-error nil)))

(provide 'e-json)

;;; e-json.el ends here

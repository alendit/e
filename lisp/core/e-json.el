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

(require 'json)

(define-error 'e-json-error "Canonical JSON value error")

(defconst e-json-false :json-false
  "Canonical Elisp value for the JSON false value.")

(defconst e-json-null :json-null
  "Canonical Elisp value for the JSON null value.")

(defun e-json--error (format-string &rest arguments)
  "Signal `e-json-error' with FORMAT-STRING and ARGUMENTS."
  (signal 'e-json-error (list (apply #'format format-string arguments))))

(defun e-json--finite-number-p (value)
  "Return non-nil when VALUE is a finite canonical JSON number."
  (or (integerp value)
      (and (floatp value)
           (= value value)
           (not (= (abs value) 1.0e+INF)))))

(defun e-json--validate (value active)
  "Validate canonical JSON VALUE using ACTIVE compound values."
  (cond
   ((or (null value)
        (eq value t)
        (eq value e-json-false)
        (eq value e-json-null)
        (stringp value))
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
  "Serialize canonical JSON VALUE into JSON text."
  (e-json-assert-value value)
  (condition-case error-data
      (json-serialize value
                      :null-object e-json-null
                      :false-object e-json-false)
    (error
     (e-json--error "Cannot serialize canonical JSON value: %s"
                    (error-message-string error-data)))))

(provide 'e-json)

;;; e-json.el ends here

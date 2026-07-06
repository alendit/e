;;; e-resources.el --- URI-addressed resource method registry for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Resource methods implement operation contracts for URI schemes.

;;; Code:

(require 'cl-lib)
(require 'e-operations)
(require 'seq)

(define-error 'e-resources-invalid-uri "Resource URI is invalid")
(define-error 'e-resources-unknown-scheme "Resource URI scheme is not registered")
(define-error 'e-resources-unsupported-operation "Resource operation is not supported")

(cl-defstruct (e-resource-method (:constructor e-resource-method-create))
  scheme
  operation
  handler
  start
  description
  uri-patterns
  range-modes
  examples
  metadata
  work)

(cl-defstruct (e-resources-registry (:constructor e-resources-registry-create))
  (methods (make-hash-table :test 'equal))
  (schemes nil)
  (operations nil))

(defun e-resources--normalize-scheme (scheme)
  "Return normalized SCHEME."
  (unless (stringp scheme)
    (signal 'wrong-type-argument (list 'stringp scheme)))
  (downcase scheme))

(defun e-resources-parse-uri (uri)
  "Parse URI into a plist with `:scheme', `:address', and `:uri'."
  (unless (stringp uri)
    (signal 'wrong-type-argument (list 'stringp uri)))
  (unless (string-match "\\`\\([A-Za-z][A-Za-z0-9+.-]*\\)://\\(.*\\)\\'" uri)
    (signal 'e-resources-invalid-uri (list (format "Invalid resource URI: %s" uri))))
  (list :scheme (e-resources--normalize-scheme (match-string 1 uri))
        :address (match-string 2 uri)
        :uri uri))

(defun e-resources--method-key (operation-id scheme)
  "Return registry key for OPERATION-ID and SCHEME."
  (cons operation-id scheme))

(defun e-resources--remember-scheme (registry scheme)
  "Remember SCHEME ordering in REGISTRY."
  (unless (member scheme (e-resources-registry-schemes registry))
    (setf (e-resources-registry-schemes registry)
          (append (e-resources-registry-schemes registry) (list scheme)))))

(defun e-resources--remember-operation (registry operation)
  "Remember OPERATION ordering in REGISTRY."
  (unless (cl-find (e-operation-id-of operation)
                   (e-resources-registry-operations registry)
                   :key #'e-operation-id-of
                   :test #'eq)
    (setf (e-resources-registry-operations registry)
          (append (e-resources-registry-operations registry) (list operation)))))

(defun e-resources-register (registry method-or-register)
  "Register METHOD-OR-REGISTER in REGISTRY.
METHOD-OR-REGISTER may be an `e-resource-method' object or a function that
registers one or more methods in REGISTRY."
  (cond
   ((e-resource-method-p method-or-register)
    (let* ((scheme (e-resources--normalize-scheme
                    (e-resource-method-scheme method-or-register)))
           (operation (e-resource-method-operation method-or-register))
           (operation-id (e-operation-id-of operation)))
      (setf (e-resource-method-scheme method-or-register) scheme)
      (e-resources--remember-scheme registry scheme)
      (e-resources--remember-operation registry operation)
      (puthash (e-resources--method-key operation-id scheme)
               method-or-register
               (e-resources-registry-methods registry))
      method-or-register))
   ((functionp method-or-register)
    (funcall method-or-register registry))
   (t
    (signal 'wrong-type-argument (list 'e-resource-method-p method-or-register)))))

(defun e-resources--known-scheme-p (registry scheme)
  "Return non-nil when SCHEME is registered in REGISTRY."
  (member scheme (e-resources-registry-schemes registry)))

(defun e-resources--method (registry operation parsed-uri)
  "Return registered method for OPERATION and PARSED-URI in REGISTRY."
  (let* ((scheme (plist-get parsed-uri :scheme))
         (operation-id (e-operation-id-of operation))
         (method (gethash (e-resources--method-key operation-id scheme)
                          (e-resources-registry-methods registry))))
    (unless method
      (if (e-resources--known-scheme-p registry scheme)
          (signal 'e-resources-unsupported-operation
                  (list (format "Resource scheme %s does not support %s"
                                scheme operation-id)))
        (signal 'e-resources-unknown-scheme
                (list (format "Unknown resource URI scheme: %s" scheme)))))
    method))

(defun e-resources--function-max-args (function)
  "Return FUNCTION's maximum arity, or t when unbounded or unknown."
  (let ((arity (ignore-errors (func-arity function))))
    (cond
     ((consp arity) (cdr arity))
     ((integerp arity) arity)
     (t t))))

(defun e-resources--compatible-arguments (method operation handler arguments)
  "Return ARGUMENTS adjusted for source-compatible older METHOD handlers."
  (if (not (eq (e-operation-id-of operation) 'glob))
      arguments
    (let ((extra (nthcdr 3 arguments))
          (max-args (e-resources--function-max-args handler)))
      (cond
       ((or (eq max-args t) (>= max-args 10)) arguments)
       ((seq-some #'identity extra)
        (signal 'e-resources-unsupported-operation
                (list (format "Resource scheme %s does not support advanced glob query controls"
                              (e-resource-method-scheme method)))))
       (t (seq-take arguments 3))))))

(defun e-resources-call (registry operation uri &rest arguments)
  "Call OPERATION for URI in REGISTRY with ARGUMENTS."
  (let* ((parsed-uri (e-resources-parse-uri uri))
         (method (e-resources--method registry operation parsed-uri))
         (handler (e-resource-method-handler method)))
    (unless (functionp handler)
      (signal 'e-resources-unsupported-operation
              (list (format "Resource scheme %s does not support %s"
                            (e-resource-method-scheme method)
                            (e-operation-id-of operation)))))
    (apply handler parsed-uri
           (e-resources--compatible-arguments
            method operation handler arguments))))

(defun e-resources-read (registry uri &optional range)
  "Read URI from REGISTRY with optional RANGE."
  (e-resources-call registry e-operation-read uri range))

(defun e-resources-write (registry uri content)
  "Write CONTENT to URI through REGISTRY."
  (e-resources-call registry e-operation-write uri content))

(defun e-resources-edit (registry uri edits)
  "Apply EDITS to URI through REGISTRY."
  (e-resources-call registry e-operation-edit uri edits))

(defun e-resources-glob
    (registry uri &optional pattern limit case-sensitive sort-by sort-order
              created-after created-before updated-after updated-before)
  "List resources under URI through REGISTRY.
PATTERN and LIMIT are optional.  CASE-SENSITIVE defaults to non-nil.
SORT-BY, SORT-ORDER, CREATED-AFTER, CREATED-BEFORE, UPDATED-AFTER, and
UPDATED-BEFORE are optional scheme-supported query controls."
  (e-resources-call registry e-operation-glob uri pattern limit case-sensitive
                    sort-by sort-order created-after created-before
                    updated-after updated-before))

(defun e-resources-search (registry uri query &optional options)
  "Search URI through REGISTRY for QUERY with optional OPTIONS."
  (e-resources-call registry e-operation-search uri query options))

(defun e-resources-operations (registry)
  "Return operations implemented by REGISTRY."
  (copy-sequence (e-resources-registry-operations registry)))

(defun e-resources-methods-for-operation (registry operation)
  "Return methods in REGISTRY that implement OPERATION."
  (let ((operation-id (e-operation-id-of operation))
        (methods nil))
    (dolist (scheme (e-resources-registry-schemes registry))
      (let ((method (gethash (e-resources--method-key operation-id scheme)
                             (e-resources-registry-methods registry))))
        (when method
          (push method methods))))
    (nreverse methods)))

(provide 'e-resources)

;;; e-resources.el ends here

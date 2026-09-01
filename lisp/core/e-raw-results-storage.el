;;; e-raw-results-storage.el --- Raw-result durable storage port -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)

(define-error 'e-raw-results-storage-error "Raw-result storage error")
(define-error 'e-raw-results-storage-conflict "Raw-result content conflict"
  'e-raw-results-storage-error)
(define-error 'e-raw-results-storage-too-large "Raw-result is too large"
  'e-raw-results-storage-error)

(cl-defstruct (e-raw-results-storage
               (:constructor e-raw-results-storage--create)
               (:conc-name e-raw-results-storage--))
  runtime call-operation)

(defun e-raw-results-storage-runtime (storage)
  "Return STORAGE's shared runtime-store identity."
  (e-raw-results-storage--runtime storage))

(defun e-raw-results-storage--call (storage operation &rest arguments)
  "Invoke STORAGE OPERATION with ARGUMENTS."
  (unless (e-raw-results-storage-p storage)
    (signal 'wrong-type-argument (list 'e-raw-results-storage-p storage)))
  (condition-case err
      (apply (e-raw-results-storage--call-operation storage)
             operation arguments)
    (e-runtime-store-raw-conflict
     (signal 'e-raw-results-storage-conflict (cdr err)))
    (e-runtime-store-resource-too-large
     (signal 'e-raw-results-storage-too-large (cdr err)))))

(defun e-raw-results-storage-put
    (storage uri content metadata created-at expires-at)
  "Commit immutable URI CONTENT and fixed EXPIRES-AT."
  (e-raw-results-storage--call storage 'put uri content metadata created-at
                               expires-at))

(defun e-raw-results-storage-read (storage uri &optional now)
  "Return exact unexpired URI content at NOW."
  (e-raw-results-storage--call storage 'read uri (or now (float-time))))

(defun e-raw-results-storage-delete (storage uri)
  "Physically delete URI."
  (e-raw-results-storage--call storage 'delete uri))

(defun e-raw-results-storage-expire (storage &optional now limit)
  "Physically delete up to LIMIT expired resources at NOW."
  (e-raw-results-storage--call storage 'expire (or now (float-time))
                               (or limit 256)))

(provide 'e-raw-results-storage)

;;; e-raw-results-storage.el ends here

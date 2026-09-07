;;; e-goodnite-storage.el --- Goodnite demand-event storage port -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Goodnite knowledge remains external/read-only.  This port owns only ordered
;; e demand events and the one named consumer checkpoint used by offline cleanup.

;;; Code:

(require 'cl-lib)

(define-error 'e-goodnite-storage-error "Goodnite demand storage error")
(define-error 'e-goodnite-storage-conflict "Goodnite demand storage conflict"
  'e-goodnite-storage-error)

(defconst e-goodnite-demand-consumer "goodnite-demand-consumer"
  "Stable identity of the single Goodnite demand-event consumer.")

(cl-defstruct (e-goodnite-storage
               (:constructor e-goodnite-storage--create)
               (:conc-name e-goodnite-storage--))
  runtime call-operation submit-operation)

(defun e-goodnite-storage-runtime (storage)
  "Return STORAGE's shared runtime-store identity."
  (e-goodnite-storage--runtime storage))

(defun e-goodnite-storage--call (storage operation &rest arguments)
  "Invoke explicit blocking STORAGE OPERATION with ARGUMENTS.
This compatibility boundary is reserved for offline migration and tests."
  (unless (e-goodnite-storage-p storage)
    (signal 'wrong-type-argument (list 'e-goodnite-storage-p storage)))
  (condition-case err
      (apply (e-goodnite-storage--call-operation storage) operation arguments)
    (e-runtime-store-goodnite-conflict
     (signal 'e-goodnite-storage-conflict (cdr err)))))

(defun e-goodnite-storage-submit
    (storage kind operation arguments on-settle)
  "Submit KIND OPERATION with ARGUMENTS and invoke ON-SETTLE asynchronously."
  (unless (e-goodnite-storage-p storage)
    (signal 'wrong-type-argument (list 'e-goodnite-storage-p storage)))
  (unless (functionp (e-goodnite-storage--submit-operation storage))
    (signal 'e-goodnite-storage-error
            (list "Goodnite storage has no asynchronous submission port")))
  (funcall (e-goodnite-storage--submit-operation storage)
           kind operation arguments on-settle))

(defun e-goodnite-storage-append (storage event-id event)
  "Append immutable EVENT with stable EVENT-ID."
  (e-goodnite-storage--call storage 'append event-id event))

(defun e-goodnite-storage-page
    (storage &optional after limit consumer)
  "Return bounded demand events after AFTER for CONSUMER."
  (e-goodnite-storage--call storage 'page (or after 0) (or limit 256)
                            (or consumer e-goodnite-demand-consumer)))

(defun e-goodnite-storage-ack
    (storage position &optional consumer)
  "Advance CONSUMER checkpoint through POSITION."
  (e-goodnite-storage--call storage 'ack
                            (or consumer e-goodnite-demand-consumer) position))

(defun e-goodnite-storage-cleanup
    (storage &optional limit consumer)
  "Delete up to LIMIT events at/before acknowledged CONSUMER checkpoint."
  (e-goodnite-storage--call storage 'cleanup
                            (or consumer e-goodnite-demand-consumer)
                            (or limit 256)))

(provide 'e-goodnite-storage)

;;; e-goodnite-storage.el ends here

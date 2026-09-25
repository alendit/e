;;; e-task-storage.el --- Task-owned durable queue port -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Consumer-shaped persistence used by `e-task-queue'.  Task policy owns queue
;; and attempt meaning; adapters own only durable projections and arbitration.

;;; Code:

(require 'cl-lib)

(define-error 'e-task-storage-error "Task storage error")
(define-error 'e-task-storage-conflict "Task storage conflict"
  'e-task-storage-error)

(cl-defstruct (e-task-storage
               (:constructor e-task-storage--create)
               (:conc-name e-task-storage--))
  runtime call-operation submit-operation)

(defun e-task-storage-runtime (storage)
  "Return STORAGE's shared runtime-store identity."
  (e-task-storage--runtime storage))

(defun e-task-storage--call (storage operation &rest arguments)
  "Invoke explicit blocking STORAGE OPERATION with ARGUMENTS.
This compatibility boundary is for offline migration, operator actions, and
tests.  Interactive task scheduling uses `e-task-storage-submit'."
  (unless (e-task-storage-p storage)
    (signal 'wrong-type-argument (list 'e-task-storage-p storage)))
  (condition-case err
      (apply (e-task-storage--call-operation storage) operation arguments)
    (e-runtime-store-task-conflict
     (signal 'e-task-storage-conflict (cdr err)))))

(defun e-task-storage-submit
    (storage kind operation arguments on-settle)
  "Submit KIND OPERATION with ARGUMENTS and call ON-SETTLE asynchronously.
ON-SETTLE receives RESULT and ERROR.  Return an adapter-private request handle;
ordinary task policy does not wait for or inspect it."
  (unless (e-task-storage-p storage)
    (signal 'wrong-type-argument (list 'e-task-storage-p storage)))
  (unless (functionp (e-task-storage--submit-operation storage))
    (signal 'e-task-storage-error
            (list "Task storage has no asynchronous submission port")))
  (funcall (e-task-storage--submit-operation storage)
           kind operation arguments on-settle))

(defun e-task-storage-open-queue (storage queue-id)
  "Create or return durable QUEUE-ID root."
  (e-task-storage--call storage 'open-queue queue-id))

(defun e-task-storage-enqueue
    (storage queue-id position record)
  "Request RECORD at POSITION in QUEUE-ID.
Storage owns durable ordering.  Repeating the same task id with the same
immutable assignment returns its canonical record; changed assignment content
signals `e-task-storage-conflict'."
  (e-task-storage--call storage 'enqueue queue-id position record))

(defun e-task-storage-claim
    (storage queue-id task-id attempt-id started-at instance-id)
  "Claim TASK-ID as immutable ATTEMPT-ID before its runner starts."
  (e-task-storage--call storage 'claim queue-id task-id
                        attempt-id started-at instance-id))

(defun e-task-storage-transition
    (storage queue-id task-id expected-status record)
  "Commit TASK-ID RECORD from EXPECTED-STATUS."
  (e-task-storage--call storage 'transition queue-id task-id
                        expected-status record))

(defun e-task-storage-set-paused
    (storage queue-id paused-p)
  "Commit QUEUE-ID PAUSED-P gate."
  (e-task-storage--call storage 'set-paused queue-id paused-p))

(defun e-task-storage-snapshot (storage queue-id &optional limit)
  "Return bounded current QUEUE-ID projection and attempt history."
  (e-task-storage--call storage 'snapshot queue-id (or limit 1024)))

(defun e-task-storage-delete-history (storage queue-id)
  "Delete QUEUE-ID records and attempts."
  (e-task-storage--call storage 'delete-history queue-id))

(defun e-task-storage-import-legacy-snapshot (storage queue-id snapshot)
  "Import one validated legacy SNAPSHOT into empty durable QUEUE-ID.
This operation exists only for the explicit offline migration application."
  (e-task-storage--call storage 'import-legacy-snapshot queue-id snapshot))

(provide 'e-task-storage)

;;; e-task-storage.el ends here

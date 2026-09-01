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
  runtime call-operation)

(defun e-task-storage-runtime (storage)
  "Return STORAGE's shared runtime-store identity."
  (e-task-storage--runtime storage))

(defun e-task-storage--call (storage operation &rest arguments)
  "Invoke STORAGE OPERATION with ARGUMENTS."
  (unless (e-task-storage-p storage)
    (signal 'wrong-type-argument (list 'e-task-storage-p storage)))
  (condition-case err
      (apply (e-task-storage--call-operation storage) operation arguments)
    (e-runtime-store-task-conflict
     (signal 'e-task-storage-conflict (cdr err)))
    (e-runtime-store-revision-conflict
     (signal 'e-task-storage-conflict (cdr err)))))

(defun e-task-storage-open-queue (storage queue-id)
  "Create or return durable QUEUE-ID root."
  (e-task-storage--call storage 'open-queue queue-id))

(defun e-task-storage-enqueue
    (storage queue-id expected-revision position record)
  "Append RECORD at POSITION after EXPECTED-REVISION."
  (e-task-storage--call storage 'enqueue queue-id expected-revision position
                        record))

(defun e-task-storage-claim
    (storage queue-id expected-revision task-id attempt-id started-at instance-id)
  "Claim TASK-ID as immutable ATTEMPT-ID before its runner starts."
  (e-task-storage--call storage 'claim queue-id expected-revision task-id
                        attempt-id started-at instance-id))

(defun e-task-storage-transition
    (storage queue-id expected-revision task-id expected-status event-id record)
  "Commit TASK-ID RECORD transition identified by EVENT-ID."
  (e-task-storage--call storage 'transition queue-id expected-revision task-id
                        expected-status event-id record))

(defun e-task-storage-set-paused
    (storage queue-id expected-revision paused-p)
  "Commit QUEUE-ID PAUSED-P gate after EXPECTED-REVISION."
  (e-task-storage--call storage 'set-paused queue-id expected-revision paused-p))

(defun e-task-storage-snapshot (storage queue-id &optional limit)
  "Return bounded current QUEUE-ID projection and attempt history."
  (e-task-storage--call storage 'snapshot queue-id (or limit 1024)))

(defun e-task-storage-delete-history (storage queue-id expected-revision)
  "Delete QUEUE-ID records and attempts after EXPECTED-REVISION."
  (e-task-storage--call storage 'delete-history queue-id expected-revision))

(provide 'e-task-storage)

;;; e-task-storage.el ends here

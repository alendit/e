;;; e-cron-storage.el --- Cron-owned durable cadence port -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Names the durable cadence and immutable firing operations consumed by cron.
;; Definitions, guards, actions, and timers stay live.

;;; Code:

(require 'cl-lib)

(define-error 'e-cron-storage-error "Cron storage error")
(define-error 'e-cron-storage-conflict "Cron storage conflict"
  'e-cron-storage-error)

(cl-defstruct (e-cron-storage
               (:constructor e-cron-storage--create)
               (:conc-name e-cron-storage--))
  runtime call-operation)

(defun e-cron-storage-runtime (storage)
  "Return STORAGE's shared runtime-store identity."
  (e-cron-storage--runtime storage))

(defun e-cron-storage--call (storage operation &rest arguments)
  "Invoke STORAGE OPERATION with ARGUMENTS."
  (unless (e-cron-storage-p storage)
    (signal 'wrong-type-argument (list 'e-cron-storage-p storage)))
  (condition-case err
      (apply (e-cron-storage--call-operation storage) operation arguments)
    (e-runtime-store-cron-conflict
     (signal 'e-cron-storage-conflict (cdr err)))))

(defun e-cron-storage-register
    (storage schedule-id definition anchor)
  "Register SCHEDULE-ID definition and return its durable cadence."
  (e-cron-storage--call storage 'register schedule-id definition anchor))

(defun e-cron-storage-claim
    (storage schedule-id firing-id due-at fire-at next-fire)
  "Claim FIRING-ID and advance cadence before executable work."
  (e-cron-storage--call storage 'claim schedule-id firing-id
                        due-at fire-at next-fire))

(defun e-cron-storage-settle
    (storage schedule-id firing-id expected-state state result)
  "Settle FIRING-ID from EXPECTED-STATE to STATE with RESULT."
  (e-cron-storage--call storage 'settle schedule-id firing-id expected-state
                        state result))

(defun e-cron-storage-cadence (storage schedule-id)
  "Return SCHEDULE-ID cadence and unresolved firing projections."
  (e-cron-storage--call storage 'cadence schedule-id))

(defun e-cron-storage-delete-history (storage schedule-id)
  "Delete SCHEDULE-ID firing history."
  (e-cron-storage--call storage 'delete-history schedule-id))

(provide 'e-cron-storage)

;;; e-cron-storage.el ends here

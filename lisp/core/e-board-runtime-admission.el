;;; e-board-runtime-admission.el --- Runtime admission lifetime owner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the small process-local lifetime index used while the
;; runtime opens a Board work or aggregation relation.  It is deliberately
;; not a transaction engine: the Board admission owns Board mutation and this
;; owner only records the exact attachment and Board objects that must be
;; fenced when an attachment is retired.  Production callbacks use ordinary
;; idempotent cleanup and generation/object identity.  Synthetic faults in
;; individual hash primitives are not part of this contract.

;;; Code:

(require 'cl-lib)
(require 'e-board-admission)
(require 'e-board-runtime-error)

(cl-defstruct (e-board-runtime-admission-record
               (:constructor e-board-runtime-admission-record--create)
               (:conc-name e-board-runtime-admission-record-))
  "Exact runtime lifetime for one open Board admission.

The admission and attachment objects are the authority.  The two indexes are
bounded lookup aids for normal attachment retirement and explicit Board-wide
cleanup; they never transfer ownership to an equal id or a replacement object."
  board attachment admission generation in-flight-p cancel-requested-p
  board-bucket attachment-bucket)

(defvar e-board-runtime-admission--pending-admissions
  (make-hash-table :test 'eq)
  "Open admissions keyed by their opaque Board admission object.")

(defvar e-board-runtime-admission--pending-by-board
  (make-hash-table :test 'eq)
  "Open admissions grouped by source Board object.")

(defvar e-board-runtime-admission--pending-by-attachment
  (make-hash-table :test 'eq)
  "Open admissions grouped by runtime attachment object.")

(defun e-board-runtime-admission--admission-p (value)
  "Return non-nil when VALUE is a Board admission token."
  (or (e-board-work-admission-p value)
      (e-board-aggregation-admission-p value)))

(defun e-board-runtime-admission--record (admission)
  "Return the exact runtime record for ADMISSION, if it is open."
  (and (e-board-runtime-admission--admission-p admission)
       (gethash admission e-board-runtime-admission--pending-admissions)))

(defun e-board-runtime-admission--bucket (table key &optional create)
  "Return the exact TABLE bucket for KEY, optionally creating it."
  (or (gethash key table)
      (when create
        (let ((bucket (make-hash-table :test 'eq)))
          (puthash key bucket table)
          bucket))))

(defun e-board-runtime-admission--add
    (table key admission record)
  "Add exact ADMISSION/RECORD to TABLE's object-local KEY bucket."
  (when key
    (let ((bucket (e-board-runtime-admission--bucket table key t)))
      (let ((current (gethash admission bucket)))
        (unless (or (null current) (eq current record))
          (signal 'e-board-runtime-error
                  (list "Runtime admission index has replacement authority"
                        admission)))
        (puthash admission record bucket))
      bucket)))

(defun e-board-runtime-admission--remove
    (table key admission record expected-bucket)
  "Remove only exact ADMISSION/RECORD from TABLE's KEY bucket."
  (when key
    (let ((bucket (gethash key table)))
      (when bucket
        (let ((current (gethash admission bucket)))
          (cond
           ((eq current record) (remhash admission bucket))
           ((null current) nil)
           (t
            (signal 'e-board-runtime-error
                    (list "Runtime admission index was replaced" admission))))
        (when (and (= (hash-table-count bucket) 0)
                   (eq (gethash key table) bucket)
                   (eq bucket expected-bucket))
          (remhash key table)))))))

(defun e-board-runtime-admission--forget (record)
  "Forget RECORD from all exact runtime indexes after Board cleanup."
  (let ((admission (e-board-runtime-admission-record-admission record))
        (board (e-board-runtime-admission-record-board record))
        (attachment (e-board-runtime-admission-record-attachment record)))
    (when (eq (gethash admission e-board-runtime-admission--pending-admissions)
              record)
      (remhash admission e-board-runtime-admission--pending-admissions))
    (e-board-runtime-admission--remove
     e-board-runtime-admission--pending-by-board board admission record
     (e-board-runtime-admission-record-board-bucket record))
    (e-board-runtime-admission--remove
     e-board-runtime-admission--pending-by-attachment attachment admission record
     (e-board-runtime-admission-record-attachment-bucket record))
    t))

(defun e-board-runtime-admission-remember
    (attachment board admission &optional in-flight-p generation)
  "Remember exact ADMISSION for ATTACHMENT and BOARD.

The record is published in the primary admission index and in exact
object-local indexes.  These indexes support production retirement and retry;
they intentionally do not promise recovery from arbitrary primitive faults."
  (unless (and board (e-board-runtime-admission--admission-p admission))
    (signal 'wrong-type-argument
            (list 'e-board-runtime-admission-p admission)))
  (let ((record (e-board-runtime-admission--record admission)))
    (unless record
      (setq record
            (e-board-runtime-admission-record--create
             :board board :attachment attachment :admission admission
             :generation generation :in-flight-p in-flight-p))
      (puthash admission record e-board-runtime-admission--pending-admissions))
    (unless (and (eq board (e-board-runtime-admission-record-board record))
                 (eq attachment
                     (e-board-runtime-admission-record-attachment record)))
      (signal 'e-board-runtime-error
              (list "Runtime admission identity changed" admission)))
    (setf (e-board-runtime-admission-record-in-flight-p record) in-flight-p
          (e-board-runtime-admission-record-generation record) generation)
    (setf (e-board-runtime-admission-record-board-bucket record)
          (e-board-runtime-admission--add
           e-board-runtime-admission--pending-by-board board admission record)
          (e-board-runtime-admission-record-attachment-bucket record)
          (e-board-runtime-admission--add
           e-board-runtime-admission--pending-by-attachment attachment admission
           record))
    record))

(defun e-board-runtime-admission-finish (admission)
  "Mark ADMISSION's current runtime stack frame complete."
  (when-let ((record (e-board-runtime-admission--record admission)))
    (setf (e-board-runtime-admission-record-in-flight-p record) nil)
    record))

(defun e-board-runtime-admission-fence (attachment &optional board)
  "Fence open admissions for exact ATTACHMENT, or BOARD when ATTACHMENT is nil.

Attachment retirement never walks a Board bucket: a same-Board sibling
attachment remains valid.  BOARD is an explicit whole-Board operation for the
few callers that own the complete Board lifetime."
  (let ((bucket (if attachment
                   (gethash attachment
                            e-board-runtime-admission--pending-by-attachment)
                 (and board
                      (gethash board
                               e-board-runtime-admission--pending-by-board)))))
    (when bucket
      (maphash
       (lambda (_admission record)
         (setf (e-board-runtime-admission-record-cancel-requested-p record) t))
       bucket)))
  attachment)

(defun e-board-runtime-admission--abort (record)
  "Abort RECORD's exact Board admission and forget it after success."
  (let ((admission (e-board-runtime-admission-record-admission record))
        (board (e-board-runtime-admission-record-board record)))
    (when (e-board-runtime-admission-record-in-flight-p record)
      (setf (e-board-runtime-admission-record-cancel-requested-p record) t)
      (signal 'e-board-admission-pending
              (list "Runtime admission is still in flight" admission)))
    (e-board-admission-abort board admission)
    (e-board-runtime-admission--forget record)))

(defun e-board-runtime-admission-abort (board admission)
  "Abort exact BOARD ADMISSION and remove its runtime lifetime record."
  (when-let ((record (e-board-runtime-admission--record admission)))
    (unless (eq board (e-board-runtime-admission-record-board record))
      (signal 'e-board-runtime-error
              (list "Runtime admission belongs to another Board" admission)))
    (e-board-runtime-admission--abort record))
  t)

(defun e-board-runtime-admission-complete (admission)
  "Complete ADMISSION's Board projection, then remove its runtime record.

If Board completion signals, the exact runtime record remains indexed and the
initiating error is visible.  Normal production callers retry through this
same exact object identity; arbitrary primitive-fault recovery is out of scope."
  (when-let ((record (e-board-runtime-admission--record admission)))
    (let ((board (e-board-runtime-admission-record-board record)))
      (e-board-admission-complete board admission)
      (e-board-runtime-admission--forget record)))
  admission)

(defun e-board-runtime-admission-retry
    (&optional attachment board allow-fenced-in-flight)
  "Retry exact open admissions for ATTACHMENT or explicit whole BOARD.

When ATTACHMENT is non-nil, only its object-local bucket is inspected.  With a
nil attachment, BOARD is an explicit owner-wide operation.  No descriptive-id
lookup or process-wide scan is used."
  (let ((bucket (if attachment
                   (gethash attachment
                            e-board-runtime-admission--pending-by-attachment)
                 (and board
                      (gethash board
                               e-board-runtime-admission--pending-by-board))))
        records)
    (when bucket
      (maphash (lambda (_admission record) (push record records)) bucket))
    (dolist (record records)
      (if (and (e-board-runtime-admission-record-in-flight-p record)
               allow-fenced-in-flight
               (e-board-runtime-admission-record-cancel-requested-p record))
          nil
        (e-board-runtime-admission--abort record)))
    records))

(provide 'e-board-runtime-admission)

;;; e-board-runtime-admission.el ends here

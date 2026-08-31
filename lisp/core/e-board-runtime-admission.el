;;; e-board-runtime-admission.el --- Runtime admission catalog owner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the concrete, process-local catalog used while a runtime
;; attachment stages a Board work or aggregation admission.  It is deliberately
;; Board-runtime-specific: the record and its six exact indexes are not a
;; generic transaction framework and do not own attachment delivery, producer,
;; pickup, or activity state.  The runtime facade supplies the exact Board and
;; attachment values and uses the semantic operations below to remember,
;; fence, retry, complete, and remove one admission.

;;; Code:

(require 'cl-lib)
(require 'e-board-admission)

(cl-defstruct (e-board-runtime-admission-record
               (:constructor e-board-runtime-admission-record--create)
               (:conc-name e-board-runtime-admission-record-))
  "Exact runtime-owned lifetime for one cross-owner Board admission.

The admission object is the primary authority.  Board and attachment buckets
are secondary exact indexes used only to fence a captured lifetime without a
process-wide scan.  Bucket fields are recorded before publication so a partial
hash-table mutation remains retryable even when its adapter signals afterward."
  board attachment admission generation in-flight-p cancel-requested-p
  publication-stage cleanup-stage
  recovery-installed-p recovery-board-bucket recovery-board-installed-p
  recovery-attachment-bucket recovery-attachment-installed-p
  primary-installed-p board-bucket board-installed-p
  attachment-bucket attachment-installed-p)

(defvar e-board-runtime-admission--pending-admissions
  (make-hash-table :test 'eq)
  "Exact primary runtime admissions awaiting commit or inverse cleanup.")

(defvar e-board-runtime-admission--pending-by-board
  (make-hash-table :test 'eq)
  "Exact pending-admission records bucketed by source Board object.")

(defvar e-board-runtime-admission--pending-by-attachment
  (make-hash-table :test 'eq)
  "Exact pending-admission records bucketed by captured attachment object.")

(defvar e-board-runtime-admission--recovery
  (make-hash-table :test 'eq)
  "Last-removal recovery catalog keyed by opaque Board admission object.")

(defvar e-board-runtime-admission--recovery-by-board
  (make-hash-table :test 'eq)
  "Exact recovery records bucketed by source Board object.")

(defvar e-board-runtime-admission--recovery-by-attachment
  (make-hash-table :test 'eq)
  "Exact recovery records bucketed by captured attachment object.")

(defun e-board-runtime-admission--admission-p (value)
  "Return non-nil when VALUE is a Board admission token."
  (or (e-board-work-admission-p value)
      (e-board-aggregation-admission-p value)))

(defun e-board-runtime-admission--record (admission)
  "Return exact runtime record for ADMISSION, if retained."
  (or (gethash admission e-board-runtime-admission--pending-admissions)
      (gethash admission e-board-runtime-admission--recovery)))

(defun e-board-runtime-admission--record-bucket (record slot)
  "Return RECORD's exact bucket named by SLOT."
  (pcase slot
    ('board (e-board-runtime-admission-record-board-bucket record))
    ('attachment (e-board-runtime-admission-record-attachment-bucket record))
    ('recovery-board
     (e-board-runtime-admission-record-recovery-board-bucket record))
    ('recovery-attachment
     (e-board-runtime-admission-record-recovery-attachment-bucket record))))

(defun e-board-runtime-admission--set-record-bucket (record slot bucket)
  "Set RECORD's exact bucket named by SLOT to BUCKET."
  (pcase slot
    ('board (setf (e-board-runtime-admission-record-board-bucket record) bucket))
    ('attachment
     (setf (e-board-runtime-admission-record-attachment-bucket record) bucket))
    ('recovery-board
     (setf (e-board-runtime-admission-record-recovery-board-bucket record)
           bucket))
    ('recovery-attachment
     (setf (e-board-runtime-admission-record-recovery-attachment-bucket record)
           bucket)))
  bucket)

(defun e-board-runtime-admission--publish-bucket
    (table key record slot)
  "Return and publish RECORD's exact bucket SLOT for TABLE KEY.

The record slot is set before the first `puthash'.  Thus both a pre-mutation
and post-mutation hash adapter error leave an exact bucket object available to
the later owner retry.  A different existing bucket is replacement authority
and is never overwritten."
  (let* ((known (e-board-runtime-admission--record-bucket record slot))
         (current (gethash key table))
         (bucket
          (cond
           ((and current known (not (eq current known)))
            (signal 'e-board-runtime-error
                    (list "Runtime admission bucket was replaced" key)))
           ((and current (not (hash-table-p current)))
            (signal 'e-board-runtime-error
                    (list "Runtime admission bucket is not a hash table" key)))
           (current current)
           (known known)
           (t (make-hash-table :test 'eq)))))
    (unless (hash-table-p bucket)
      (signal 'e-board-runtime-error
              (list "Runtime admission bucket is not a hash table" key)))
    ;; Record authority before publication; this is intentionally before the
    ;; fallible outer table mutation, not after its acknowledgement.
    (e-board-runtime-admission--set-record-bucket record slot bucket)
    (unless current
      (condition-case err
          (puthash key bucket table)
        (error
         ;; Do not hide the adapter error.  The record still owns BUCKET and a
         ;; subsequent retry can prove or repair its exact outer membership.
         (signal (car err) (cdr err)))))
    (let ((published (gethash key table)))
      (unless (eq published bucket)
        (signal 'e-board-runtime-error
                (list "Runtime admission bucket publication did not stick" key)))
      bucket)))

(defun e-board-runtime-admission--ensure-record
    (table admission record)
  "Install exact ADMISSION RECORD in primary TABLE, with postcondition."
  (let ((current (gethash admission table)))
    (cond
     ((null current)
      (condition-case err
          (puthash admission record table)
        (error
         (signal (car err) (cdr err))))
      (unless (eq (gethash admission table) record)
        (signal 'e-board-runtime-error
                (list "Runtime admission record publication did not stick"
                      admission))))
     ((eq current record) nil)
     (t
      (signal 'e-board-runtime-error
              (list "Runtime admission record has replacement authority"
                    admission))))
  record))

(defun e-board-runtime-admission--put-member (bucket admission record)
  "Install exact ADMISSION RECORD in BUCKET, with postcondition."
  (let ((current (gethash admission bucket)))
    (cond
     ((null current)
      (condition-case err
          (puthash admission record bucket)
        (error
         (signal (car err) (cdr err)))))
     ((eq current record) nil)
     (t
      (signal 'e-board-runtime-error
              (list "Runtime admission bucket has replacement authority"
                    admission))))
    (unless (eq (gethash admission bucket) record)
      (signal 'e-board-runtime-error
              (list "Runtime admission bucket publication did not stick"
                    admission)))
    record))

(defun e-board-runtime-admission--remove-member
    (table key bucket admission record)
  "Remove exact ADMISSION from BUCKET and its empty outer TABLE bucket.

Only object identity authorizes each inverse.  If a hash primitive signals
after mutation, the error remains visible and the next retry sees the exact
postcondition instead of touching a replacement."
  (when bucket
    (let ((current (gethash admission bucket)))
      (cond
       ((eq current record)
        (condition-case err
            (remhash admission bucket)
          (error
           (signal (car err) (cdr err))))
        (unless (null (gethash admission bucket))
          (signal 'e-board-runtime-error
                  (list "Runtime admission member remains" admission))))
       ((null current) nil)
       (t
        (signal 'e-board-runtime-error
                (list "Runtime admission member was replaced" admission)))))
    (when (and (= (hash-table-count bucket) 0)
               (eq (gethash key table) bucket))
      (condition-case err
          (remhash key table)
        (error
         (signal (car err) (cdr err))))
      (when (eq (gethash key table) bucket)
        (signal 'e-board-runtime-error
                (list "Runtime admission empty bucket remains" key)))))
  t)

(defun e-board-runtime-admission--abort-board (board admission)
  "Abort ADMISSION, retrying one lower-owner inverse fault.

The runtime owner preserves the original initiating condition; this bounded
retry only handles a transient lower-owner inverse boundary and never
reconstructs or rediscoveres authority by an id."
  (condition-case _first-error
      (progn
        (if (e-board-work-admission-p admission)
            (e-board-admission-abort board admission)
          (e-board-admission-abort board admission))
        t)
    (error
     (condition-case _retry-error
         (progn
           (if (e-board-work-admission-p admission)
               (e-board-admission-abort board admission)
             (e-board-admission-abort board admission))
           t)
       (error nil)))))

(defun e-board-runtime-admission-abort (board admission)
  "Abort exact BOARD ADMISSION through the runtime-admission owner.

This is the narrow semantic inverse used by the runtime facade.  It does not
expose the catalog record or any of its secondary indexes."
  (e-board-runtime-admission--abort-board board admission))

(defun e-board-runtime-admission-remember
    (attachment board admission &optional in-flight-p generation)
  "Retain exact BOARD ADMISSION in the runtime recovery catalog.

BOARD must be the source Board, and ATTACHMENT is the captured runtime object
whose lifetime this admission belongs to.  The recovery primary record is
published first.  Each of the four secondary buckets is then recorded in the
same record before its outer-table publication, so every partial path remains
repairable without a process-wide scan."
  (unless (and board (e-board-runtime-admission--admission-p admission))
    (signal 'wrong-type-argument
            (list 'e-board-runtime-admission-admission-p admission)))
  (let ((record (e-board-runtime-admission--record admission)))
    (unless record
      (setq record
            (e-board-runtime-admission-record--create
             :board board :attachment attachment :admission admission
             :generation generation :in-flight-p in-flight-p
             :publication-stage 'new)))
    ;; The exact Board and attachment object are immutable provenance for this
    ;; admission.  Equal ids never transfer a record to a replacement.
    (unless (and (eq (e-board-runtime-admission-record-board record) board)
                 (eq (e-board-runtime-admission-record-attachment record)
                     attachment))
      (signal 'e-board-runtime-error
              (list "Runtime admission identity changed" admission)))
    (setf (e-board-runtime-admission-record-in-flight-p record) in-flight-p)
    ;; The recovery primary is the first exact authority.  If this hash
    ;; primitive signals after mutation, a later call finds the same record.
    (e-board-runtime-admission--ensure-record
     e-board-runtime-admission--recovery admission record)
    (setf (e-board-runtime-admission-record-recovery-installed-p record) t
          (e-board-runtime-admission-record-publication-stage record)
          'recovery-record)
    (let ((bucket
           (e-board-runtime-admission--publish-bucket
            e-board-runtime-admission--recovery-by-board board record
            'recovery-board)))
      (e-board-runtime-admission--put-member bucket admission record)
      (setf (e-board-runtime-admission-record-recovery-board-installed-p record)
            t
            (e-board-runtime-admission-record-publication-stage record)
            'recovery-board))
    (let ((bucket
           (e-board-runtime-admission--publish-bucket
            e-board-runtime-admission--recovery-by-attachment attachment record
            'recovery-attachment)))
      (e-board-runtime-admission--put-member bucket admission record)
      (setf
       (e-board-runtime-admission-record-recovery-attachment-installed-p record)
       t
       (e-board-runtime-admission-record-publication-stage record)
       'recovery-attachment))
    (e-board-runtime-admission--ensure-record
     e-board-runtime-admission--pending-admissions admission record)
    (setf (e-board-runtime-admission-record-primary-installed-p record) t
          (e-board-runtime-admission-record-publication-stage record) 'primary)
    (let ((bucket
           (e-board-runtime-admission--publish-bucket
            e-board-runtime-admission--pending-by-board board record 'board)))
      (e-board-runtime-admission--put-member bucket admission record)
      (setf (e-board-runtime-admission-record-board-installed-p record) t
            (e-board-runtime-admission-record-publication-stage record) 'board))
    (let ((bucket
           (e-board-runtime-admission--publish-bucket
            e-board-runtime-admission--pending-by-attachment attachment record
            'attachment)))
      (e-board-runtime-admission--put-member bucket admission record)
      (setf (e-board-runtime-admission-record-attachment-installed-p record) t
            (e-board-runtime-admission-record-publication-stage record)
            'published))
    record))

(defun e-board-runtime-admission-complete (admission)
  "Complete exact ADMISSION and remove all runtime catalog memberships.

Board completion is acknowledged before catalog removal, while the recovery
record remains the last authority.  Repeated calls therefore repair an
interrupted cleanup without rediscovering a replacement by descriptive id."
  (when-let ((record (e-board-runtime-admission--record admission)))
    (let* ((board (e-board-runtime-admission-record-board record))
           (attachment (e-board-runtime-admission-record-attachment record))
           (board-bucket
            (or (e-board-runtime-admission-record-board-bucket record)
                (gethash board e-board-runtime-admission--pending-by-board)))
           (attachment-bucket
            (or (e-board-runtime-admission-record-attachment-bucket record)
                (gethash attachment
                         e-board-runtime-admission--pending-by-attachment)))
           (recovery-board-bucket
            (or (e-board-runtime-admission-record-recovery-board-bucket record)
                (gethash board
                         e-board-runtime-admission--recovery-by-board)))
           (recovery-attachment-bucket
            (or
             (e-board-runtime-admission-record-recovery-attachment-bucket
              record)
             (gethash attachment
                      e-board-runtime-admission--recovery-by-attachment))))
      ;; Semantic Board completion is intentionally outside the runtime
      ;; catalogs.  It is idempotent and precedes every runtime inverse.
      (e-board-admission-complete board admission)
      (setf (e-board-runtime-admission-record-cleanup-stage record)
            'board-complete)
      ;; A bucket may have been created and published before its member
      ;; insertion signalled.  Its recorded identity is therefore cleanup
      ;; authority even when the membership flag is false; remove-member is
      ;; careful to leave a replacement outer bucket untouched.
      (when attachment-bucket
        (e-board-runtime-admission--remove-member
         e-board-runtime-admission--pending-by-attachment attachment
         attachment-bucket admission record)
        (setf (e-board-runtime-admission-record-attachment-installed-p record)
              nil
              (e-board-runtime-admission-record-cleanup-stage record)
              'attachment-removed))
      (when board-bucket
        (e-board-runtime-admission--remove-member
         e-board-runtime-admission--pending-by-board board board-bucket
         admission record)
        (setf (e-board-runtime-admission-record-board-installed-p record) nil
              (e-board-runtime-admission-record-cleanup-stage record)
              'board-removed))
      (when (or (e-board-runtime-admission-record-primary-installed-p record)
                (eq (gethash admission e-board-runtime-admission--pending-admissions)
                    record))
        (when (eq (gethash admission e-board-runtime-admission--pending-admissions)
                  record)
          (condition-case err
              (remhash admission e-board-runtime-admission--pending-admissions)
            (error (signal (car err) (cdr err)))))
        (unless (eq (gethash admission e-board-runtime-admission--pending-admissions)
                    record)
          (setf (e-board-runtime-admission-record-primary-installed-p record)
                nil))
        (setf (e-board-runtime-admission-record-cleanup-stage record)
              'primary-removed))
      (when recovery-attachment-bucket
        (e-board-runtime-admission--remove-member
         e-board-runtime-admission--recovery-by-attachment attachment
         recovery-attachment-bucket admission record)
        (setf
         (e-board-runtime-admission-record-recovery-attachment-installed-p
          record)
         nil))
      (when recovery-board-bucket
        (e-board-runtime-admission--remove-member
         e-board-runtime-admission--recovery-by-board board
         recovery-board-bucket admission record)
        (setf (e-board-runtime-admission-record-recovery-board-installed-p record)
              nil))
      (when (or (e-board-runtime-admission-record-recovery-installed-p record)
                (eq (gethash admission e-board-runtime-admission--recovery)
                    record))
        (when (eq (gethash admission e-board-runtime-admission--recovery)
                  record)
          (condition-case err
              (remhash admission e-board-runtime-admission--recovery)
            (error (signal (car err) (cdr err)))))
        (unless (eq (gethash admission e-board-runtime-admission--recovery)
                    record)
          (setf
           (e-board-runtime-admission-record-recovery-installed-p record) nil))
        (when (eq (gethash admission e-board-runtime-admission--recovery)
                  record)
          (signal 'e-board-runtime-error
                  (list "Runtime admission recovery record remains" admission))))
      (setf (e-board-runtime-admission-record-cleanup-stage record) 'done)))
  admission)

(defun e-board-runtime-admission-finish (admission)
  "Mark ADMISSION's current runtime stack frame complete without forgetting it."
  (when-let ((record (e-board-runtime-admission--record admission)))
    (setf (e-board-runtime-admission-record-in-flight-p record) nil)
    record))

(defun e-board-runtime-admission-fence (attachment &optional board)
  "Fence in-flight admissions owned by exact ATTACHMENT or BOARD.

Retirement may be reentrant with a lower Board callback.  Marking the captured
record is safe on that stack; the outer operation later performs its exact
inverse.  BOARD is optional for callers that already hold an attachment, but
is used by the runtime retirement path so a record whose attachment bucket was
only partially published is still fenced through its exact source-Board index."
  (let (buckets)
    (dolist (bucket
             (list (gethash attachment
                            e-board-runtime-admission--pending-by-attachment)
                   (gethash attachment
                            e-board-runtime-admission--recovery-by-attachment)
                   (and board
                        (gethash board
                                 e-board-runtime-admission--pending-by-board))
                   (and board
                        (gethash board
                                 e-board-runtime-admission--recovery-by-board))))
      (when (and bucket (not (memq bucket buckets)))
        (push bucket buckets)))
    (dolist (bucket buckets)
      (maphash
       (lambda (_admission record)
         (setf (e-board-runtime-admission-record-cancel-requested-p record) t))
       bucket)))
  attachment)

(defun e-board-runtime-admission-retry
    (&optional attachment board allow-fenced-in-flight)
  "Retry exact pending Board admissions for ATTACHMENT or BOARD.

Only the supplied object-local bucket is consulted.  An in-flight admission is
either fenced for its owning stack or reported as still active; no replacement
is reconstructed from an id."
  (let* ((bucket (and board
                      (gethash board e-board-runtime-admission--pending-by-board)))
         (recovery-bucket
          (and board
               (gethash board e-board-runtime-admission--recovery-by-board)))
         (attachment-bucket
          (and attachment
               (gethash attachment
                        e-board-runtime-admission--pending-by-attachment)))
         (recovery-attachment-bucket
          (and attachment
               (gethash attachment
                        e-board-runtime-admission--recovery-by-attachment)))
         records)
    ;; A partially published record can be present only in the Board bucket
    ;; (for example when attachment-bucket publication signalled before its
    ;; outer table changed).  When both identities are supplied, inspect their
    ;; four exact secondary buckets; this remains object-local and never
    ;; becomes a process-wide catalog scan.
    (dolist (candidate-bucket
             (if attachment
                 (list attachment-bucket recovery-attachment-bucket
                       bucket recovery-bucket)
               (list bucket recovery-bucket)))
      (when candidate-bucket
        (maphash
         (lambda (_admission record)
           (cl-pushnew record records :test #'eq))
         candidate-bucket)))
    (dolist (record records)
      (cond
       ((and (e-board-runtime-admission-record-in-flight-p record)
             allow-fenced-in-flight
             (e-board-runtime-admission-record-cancel-requested-p record))
        nil)
       ((e-board-runtime-admission-record-in-flight-p record)
        (signal 'e-board-runtime-error
                (list "Board admission is still in flight" record)))
       ((not (e-board-runtime-admission--abort-board
              (e-board-runtime-admission-record-board record)
              (e-board-runtime-admission-record-admission record)))
        (signal 'e-board-runtime-error
                (list "Pending Board admission cleanup remains incomplete" record)))
       (t
        (e-board-runtime-admission-complete
         (e-board-runtime-admission-record-admission record)))))))

(provide 'e-board-runtime-admission)

;;; e-board-runtime-admission.el ends here

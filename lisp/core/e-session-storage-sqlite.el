;;; e-session-storage-sqlite.el --- SQLite session physical adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Implements the SQLite side of the existing session-storage seam.  It maps
;; consumer-shaped session operations to the generic runtime-store protocol;
;; schema, SQL, worker lifecycle, aggregate replay, and catalog policy remain
;; with their existing owners.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'e-runtime-store)
(require 'e-session-storage-limits)

(defvar e-session-storage-sqlite--backends
  (make-hash-table :test 'eq :weakness 'key))
(defvar e-session-storage-sqlite--runtimes
  (make-hash-table :test 'eq :weakness 'key))
(defvar e-session-storage-sqlite--owned-runtimes
  (make-hash-table :test 'eq :weakness 'key))
(cl-defstruct (e-session-storage-sqlite--async-operation
               (:constructor e-session-storage-sqlite--async-operation-create))
  "Opaque consumer-facing session operation; its request remains adapter-private."
  request)

(defun e-session-storage-sqlite-register (store backend runtime owns-runtime)
  "Register STORE's physical BACKEND and optional RUNTIME."
  (puthash store backend e-session-storage-sqlite--backends)
  (if runtime
      (puthash store runtime e-session-storage-sqlite--runtimes)
    (remhash store e-session-storage-sqlite--runtimes))
  (if (and runtime owns-runtime)
      (puthash store t e-session-storage-sqlite--owned-runtimes)
    (remhash store e-session-storage-sqlite--owned-runtimes)))

(defun e-session-storage-sqlite-store-p (store)
  "Return non-nil when STORE uses the opt-in SQLite adapter."
  (eq (gethash store e-session-storage-sqlite--backends 'legacy) 'sqlite))

(defun e-session-storage-sqlite-runtime (store)
  "Return STORE's runtime-store adapter, or nil."
  (gethash store e-session-storage-sqlite--runtimes))

(defun e-session-storage-sqlite--call (store kind body)
  "Call STORE's typed runtime KIND BODY operation."
  (let ((runtime (e-session-storage-sqlite-runtime store)))
    (unless runtime
      (signal 'e-session-storage-error (list "SQLite runtime is unavailable")))
    (when (eq kind 'write)
      (e-session-storage-sqlite--preflight-session-body body))
    (e-runtime-store-call runtime kind body)))

(defun e-session-storage-sqlite--preflight-session-body (body)
  "Enforce practical record and batch limits for session mutation BODY."
  (pcase (plist-get body :op)
    ((or 'session-append 'session-append-with-tool-transition)
     (e-runtime-store-codec-measure-bounded
      (plist-get body :record) e-session-storage-record-byte-limit))
    ('session-append-batch
     (let ((records (plist-get body :records)))
       (unless (or (listp records) (vectorp records))
         (signal 'wrong-type-argument (list '(or list vector) records)))
       (when (> (length records) e-session-storage-batch-record-limit)
         (signal 'e-runtime-store-request-too-large
                 (list "Session batch exceeds record-count limit"
                       :limit e-session-storage-batch-record-limit)))
       (mapc (lambda (record)
               (e-runtime-store-codec-measure-bounded
                record e-session-storage-record-byte-limit))
             (append records nil))
       (e-runtime-store-codec-measure-bounded
        body e-session-storage-batch-byte-limit))))
  body)

(defun e-session-storage-sqlite-validate-operation-body (store body)
  "Validate STORE's session mutation BODY without submitting it."
  (unless (e-session-storage-sqlite-runtime store)
    (signal 'e-session-storage-error (list "SQLite runtime is unavailable")))
  (e-session-storage-sqlite--preflight-session-body body))

(defun e-session-storage-sqlite-submit-owned
    (store kind body owner-key on-settle &optional escrow)
  "Submit STORE's typed KIND BODY for OWNER-KEY and observe its settlement.

The session application service owns any resulting work handle and publishes
aggregate state only from this consumer-shaped terminal observation.  The
runtime request, OWNER-KEY, and protocol fields stay inside this adapter."
  (let ((runtime (e-session-storage-sqlite-runtime store)))
    (unless runtime
      (signal 'e-session-storage-error (list "SQLite runtime is unavailable")))
    (e-session-storage-sqlite--preflight-session-body body)
    (let* ((operation (e-session-storage-sqlite--async-operation-create))
           (observer
            (lambda (settled)
              (if (eq (e-runtime-store-request--state settled) 'committed)
                  (funcall on-settle
                           (e-runtime-store-request--result settled) nil)
                (funcall on-settle nil
                         (or (e-runtime-store-request--error settled)
                             '(e-session-storage-error
                               "Runtime request did not commit"))))))
           request)
      (condition-case observation-error
          (progn
            (setq request (e-runtime-store--submit-owned
                           runtime kind body owner-key escrow))
            ;; The opaque adapter operation is allocated before admission.  Its
            ;; sole request pointer is transferred without a quit window before
            ;; observer installation can fault.
            (let ((inhibit-quit t))
              (setf (e-session-storage-sqlite--async-operation-request operation)
                    request))
            (e-runtime-store--observe request observer)
            operation)
        ((error quit)
         (if (null request)
             (signal (car observation-error) (cdr observation-error))
           (let ((disposition
                  (condition-case cancel-error
                      (e-runtime-store-cancel runtime request)
                    ((error quit) cancel-error))))
             (if (eq disposition 'dropped)
                 (signal (car observation-error) (cdr observation-error))
               ;; A request that cannot be proven dropped still needs its one
               ;; authoritative terminal observer.  Both objects were already
               ;; allocated, so this quit-inhibited recovery retains only
               ;; bounded pointers and performs no semantic publication.
               (let ((inhibit-quit t))
                 (unless (e-runtime-store-request--observer request)
                   (setf (e-runtime-store-request--observer request) observer)))
               (signal 'e-session-storage-admission-ambiguous
                       (list "Runtime observer installation failed after admission"
                             :storage-operation operation
                             :disposition disposition
                             :cause observation-error))))))))))

(defun e-session-storage-sqlite-submit (store kind body on-settle &optional escrow)
  "Submit STORE's typed KIND BODY and call ON-SETTLE with RESULT and ERROR."
  (e-session-storage-sqlite-submit-owned
   store kind body nil on-settle escrow))

(defun e-session-storage-sqlite-cancel-operation (store operation)
  "Cancel queued opaque session OPERATION in STORE exactly once.

The adapter keeps the runtime request private while exposing only the runtime's
finite cancellation disposition to the session application service."
  (unless (e-session-storage-sqlite--async-operation-p operation)
    (signal 'wrong-type-argument
            (list 'e-session-storage-sqlite--async-operation-p operation)))
  (let ((runtime (e-session-storage-sqlite-runtime store)))
    (unless runtime
      (signal 'e-session-storage-error (list "SQLite runtime is unavailable")))
    (e-runtime-store-cancel
     runtime (e-session-storage-sqlite--async-operation-request operation))))

(defun e-session-storage-sqlite-reference (session-id)
  "Return the opaque catalog reference for SESSION-ID."
  (format "sqlite:session:%s" session-id))

(defun e-session-storage-sqlite-header (store session-id)
  "Return STORE's physical header for SESSION-ID."
  (let ((header (e-session-storage-sqlite--call
                 store 'read (list :op 'session-header :session-id session-id))))
    (plist-put header :stored-bytes (plist-get header :byte-size))
    ;; The facade's legacy cursor is named byte-size; SQLite uses position.
    (plist-put header :byte-size (plist-get header :revision))
    header))

(defun e-session-storage-sqlite-read-checkpoint (store session-id)
  "Return SESSION-ID's exact checkpoint or signal when absent."
  ;; Call the worker's guarded read directly.  It performs its own metadata
  ;; check before selecting a value, while the session loader has already used
  ;; `checkpoint-get' to choose checkpoint versus full replay.  Avoiding a
  ;; second metadata round-trip keeps normal checkpoint resume at two worker
  ;; requests: presence then guarded value.
  (let ((result (e-session-storage-sqlite--call
                 store 'read
                 (list :op 'checkpoint-read :session-id session-id))))
    (if result
        (let ((value (plist-get result :value)))
          (when (vectorp (plist-get value :records))
            (plist-put value :records (append (plist-get value :records) nil)))
          value)
      (signal 'file-missing (list "SQLite checkpoint" session-id)))))

(defun e-session-storage-sqlite-checkpoint-present-p (store session-id)
  "Return non-nil when SESSION-ID has a usable bounded checkpoint."
  (let ((status (e-session-storage-sqlite--call
                 store 'read (list :op 'checkpoint-get :session-id session-id))))
    (and status (plist-get status :usable))))

(defun e-session-storage-sqlite-write-checkpoint (store session-id value)
  "Persist bounded SESSION-ID checkpoint VALUE, or omit an oversized one.

Checkpoint values are rebuildable projections.  Their size is checked before
the parent submits a worker request, so an oversized current projection cannot
freeze or poison the authoritative session-record path."
  (condition-case err
      (progn
        (e-runtime-store-codec-encode-bounded
         value e-runtime-store-codec-checkpoint-canonical-byte-limit)
        (e-session-storage-sqlite--call
         store 'write (list :op 'checkpoint-put :session-id session-id
                            :value value)))
    (e-runtime-store-codec-too-large
     (list :omitted t :session-id session-id
           :canonical-limit e-runtime-store-codec-checkpoint-canonical-byte-limit
           :cause err))))

(defun e-session-storage-sqlite-read-records (store session-id &optional after)
  "Return all semantic records for SESSION-ID after AFTER."
  (let ((position (or after 0)) records next)
    (while
        (progn
          (let ((page (e-session-storage-sqlite-read-page
                       store session-id position 512)))
            (setq records
                  (nconc records
                         (mapcar (lambda (entry) (plist-get entry :value))
                                 (plist-get page :records)))
                  next (plist-get page :next)
                  position (or next position)))
          next))
    records))

(defun e-session-storage-sqlite-read-page (store session-id after limit)
  "Return one bounded semantic page for SESSION-ID."
  (e-session-storage-sqlite--call
   store 'read (list :op 'session-record-page :session-id session-id
                     :after (or after 0) :limit (or limit 256))))

(defun e-session-storage-sqlite-session-ids (store)
  "Return STORE's durable session identities through bounded cursor pages."
  (let (cursor ids next)
    (while
        (progn
          (let ((page (e-session-storage-sqlite--call
                       store 'read
                       (append '(:op session-id-page :limit 256)
                               (when cursor (list :cursor cursor))))))
            (let ((page-ids (plist-get page :ids)))
              (unless (and (listp page-ids)
                           (seq-every-p #'stringp page-ids))
                (signal 'e-runtime-store-error
                        (list "Invalid session identity page" page)))
              (setq ids (nconc ids page-ids)
                    next (plist-get page :next)))
            (when (and next
                       (or (not (stringp next))
                           (equal next cursor)))
              (signal 'e-runtime-store-error
                      (list "Session identity page did not advance" page)))
            (setq cursor next))
          next))
    ids))

(defun e-session-storage-sqlite-read-catalog (store)
  "Return STORE's catalog projection, or nil."
  (when-let* ((result (e-session-storage-sqlite--call
                       store 'read '(:op catalog-get))))
    (plist-get result :value)))

(defun e-session-storage-sqlite-write-catalog (store value)
  "Persist bounded STORE catalog projection VALUE.

The catalog is derived from authoritative session records.  Its private cap
therefore reports an explicit projection failure after the primary commit; it
does not truncate semantic content or freeze the shared runtime."
  (condition-case err
      (progn
        (e-runtime-store-codec-encode-bounded
         value e-runtime-store-codec-catalog-canonical-byte-limit)
        (e-session-storage-sqlite--call
         store 'write (list :op 'catalog-put :value value)))
    (e-runtime-store-codec-too-large
     (signal 'e-runtime-store-projection-too-large
             (list "Catalog projection exceeds canonical byte limit"
                   :projection 'catalog
                   :limit e-runtime-store-codec-catalog-canonical-byte-limit
                   :cause err)))))

(defun e-session-storage-sqlite-status (store)
  "Return bounded adapter and runtime status for STORE."
  (append (list :backend 'sqlite :unsettled-write-count 0)
          (e-runtime-store-status (e-session-storage-sqlite-runtime store))))

(defun e-session-storage-sqlite-append (store session-id record)
  "Append one exact RECORD to SESSION-ID."
  (e-session-storage-sqlite--call
   store 'write (list :op 'session-append :session-id session-id
                      :record record)))

(defun e-session-storage-sqlite-append-batch (store session-id records)
  "Append exact RECORDS atomically to SESSION-ID."
  (e-session-storage-sqlite--call
   store 'write (list :op 'session-append-batch :session-id session-id
                      :records (vconcat records))))

(defun e-session-storage-sqlite-prepare-append-batch (session-id records)
  "Return detached RECORDS after preflighting one complete batch frame."
  (let* ((records (mapcar #'copy-tree records))
         (body (list :op 'session-append-batch :session-id session-id
                     :records (vconcat records))))
    ;; This admission seam must not itself materialize an unbounded canonical
    ;; body before the scheduler can perform its complete-frame preflight.
    ;; Submission adds the generated id and kind envelope, then repeats the
    ;; bounded check before queue ownership changes.
    (e-runtime-store-codec-encode-bounded
     body e-runtime-store-codec-protocol-canonical-byte-limit)
    records))

(defun e-session-storage-sqlite-delete (store session-id)
  "Delete SESSION-ID and its physically subordinate private state."
  (e-session-storage-sqlite--call
   store 'write (list :op 'session-delete :session-id session-id)))

(defun e-session-storage-sqlite-ordered-barrier (store)
  "Return after STORE acknowledges all previously submitted effects.

The runtime store serializes writes and gives queued commits priority over
bounded reads.  Its status query is therefore the narrow ordered barrier: an
acknowledged status response cannot overtake an earlier submitted write.  The
barrier deliberately performs no database integrity scan; callers request
that separately through `e-runtime-store-integrity'."
  (e-session-storage-sqlite--call store 'read '(:op status)))

(defun e-session-storage-sqlite-healthy-p (store)
  "Return non-nil after STORE acknowledges its ordered health barrier."
  (e-session-storage-sqlite-ordered-barrier store))

(defun e-session-storage-sqlite-tool-transition
    (store session-id call-id state payload)
  "Persist one tool continuity transition."
  (e-session-storage-sqlite--call
   store 'write
   (list :op 'tool-transition :session-id session-id
         :call-id call-id :state state :payload payload)))

(defun e-session-storage-sqlite-tool-classifications (store session-id)
  "Return SESSION-ID's bounded tool continuity classifications."
  (e-session-storage-sqlite--call
   store 'read (list :op 'tool-list :session-id session-id :limit 512)))

(defun e-session-storage-sqlite-close (store)
  "Close STORE's subordinate runtime worker."
  (when-let* ((runtime (and (gethash store
                                    e-session-storage-sqlite--owned-runtimes)
                            (e-session-storage-sqlite-runtime store))))
    (e-runtime-store-close runtime))
  (remhash store e-session-storage-sqlite--owned-runtimes)
  (remhash store e-session-storage-sqlite--runtimes)
  (remhash store e-session-storage-sqlite--backends)
  t)

(provide 'e-session-storage-sqlite)

;;; e-session-storage-sqlite.el ends here

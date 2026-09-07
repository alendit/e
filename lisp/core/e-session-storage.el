;;; e-session-storage.el --- Current session storage port -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session-owned physical port.  Persistent stores are SQLite-backed and borrow
;; the one runtime-store worker injected by composition.  Explicitly ephemeral
;; stores perform no durable I/O.  Retired JSONL decoding lives separately in
;; `e-session-legacy' and is reachable only from offline migration.

;;; Code:

(require 'cl-lib)
(require 'e-session-query)
(require 'e-session-storage-sqlite)

(define-error 'e-session-persistence-unavailable
  "Session persistence is unavailable")
(define-error 'e-session-storage-error "Session persistence error")
(define-error 'e-session-storage-command-error
  "Invalid session persistence command" 'e-session-storage-error)
(define-error 'e-session-storage-admission-ambiguous
  "Session persistence admission could not be proven cancelled"
  'e-session-storage-error)
(define-error 'e-session-storage-migration-required
  "Offline session migration is required" 'e-session-storage-error)

(cl-defstruct (e-session-storage-state
               (:constructor e-session-storage--state-create)
               (:predicate e-session-storage--state-p)
               (:conc-name e-session-storage--state-))
  owner directory persistent
  (checkpoint-dirty-session-ids (make-hash-table :test 'equal))
  (unsettled-write-count 0) (unsettled-generation 0)
  index-projection checkpoint-projection-operation projection-last-error)

(defvar e-session-storage--states
  (make-hash-table :test 'eq :weakness 'key)
  "Current storage state keyed by opaque session owner.")

(defvar e-session-storage--unsettled-write-count 0)
(defvar e-session-storage--unsettled-generation 0)
(defvar e-session-storage--unsettled-change-function nil)
(defvar e-session-storage-unsettled-change-hook nil
  "Hook run after session durability state changes.")

(defconst e-session-storage--projection-error-message-limit 512)

(defun e-session-storage--state (owner)
  "Return storage state for opaque OWNER, creating an ephemeral state."
  (if (e-session-storage--state-p owner)
      owner
    (or (gethash owner e-session-storage--states)
        (let ((state (e-session-storage--state-create :owner owner)))
          (puthash owner state e-session-storage--states)
          state))))

(cl-defun e-session-storage-register
    (owner &key directory sessions-directory index-file persistent write-mode
           (backend 'legacy) runtime-store (owns-runtime-store t) reservation)
  "Register OWNER's current physical adapter and return its state.

Persistent legacy registration is rejected with migration guidance.  DIRECTORY,
RUNTIME-STORE, and OWNS-RUNTIME-STORE describe the SQLite adapter selected by
the composition root.  Retired path and mode arguments are accepted only so a
caller receives the targeted error instead of an opaque keyword failure."
  (ignore sessions-directory index-file write-mode)
  (when (and persistent (not (eq backend 'sqlite)))
    (signal 'e-session-storage-error
            (list "Legacy session persistence was retired; run offline migration")))
  (let ((state
         (e-session-storage--state-create
          :owner owner
          :directory (and directory
                          (file-name-as-directory (expand-file-name directory)))
          :persistent (and persistent t))))
    (puthash owner state e-session-storage--states)
    ;; RESERVATION is retained as a compatibility keyword for callers that
    ;; compose the runtime.  The runtime itself owns that ledger.
    (ignore reservation)
    (e-session-storage-sqlite-register owner backend runtime-store
                                       owns-runtime-store)
    state))

(defun e-session-storage--profile-call (event options thunk)
  "Measure storage EVENT with OPTIONS when developer profiling is active."
  (if (and (fboundp 'e-dev-profile-enabled-p)
           (fboundp 'e-dev-profile-measure-thunk)
           (e-dev-profile-enabled-p))
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-session-storage-unsettled-state ()
  "Return constant-time aggregate session durability state."
  (list :generation e-session-storage--unsettled-generation
        :writes e-session-storage--unsettled-write-count))

(defun e-session-storage--adjust-unsettled-writes (store delta)
  "Adjust STORE and process-wide durability counters by DELTA."
  (let* ((state (e-session-storage--state store))
         (store-next (+ (e-session-storage--state-unsettled-write-count state)
                        delta))
         (global-next (+ e-session-storage--unsettled-write-count delta)))
    (when (or (< store-next 0) (< global-next 0))
      (signal 'e-session-storage-error
              (list "Session unsettled count would become negative")))
    (setf (e-session-storage--state-unsettled-write-count state) store-next)
    (cl-incf (e-session-storage--state-unsettled-generation state))
    (setq e-session-storage--unsettled-write-count global-next)
    (cl-incf e-session-storage--unsettled-generation)
    (when e-session-storage--unsettled-change-function
      (funcall e-session-storage--unsettled-change-function
               (e-session-storage-unsettled-state)))
    (run-hook-with-args 'e-session-storage-unsettled-change-hook
                        (e-session-storage-unsettled-state))
    store-next))

(defun e-session-storage-sqlite-p (store)
  "Return non-nil when STORE uses the current SQLite adapter."
  (e-session-storage-sqlite-store-p store))

(defun e-session-storage-persistent-p (store)
  "Return non-nil when STORE has current persistent authority."
  (and (e-session-storage-sqlite-p store)
       (e-session-storage--state-persistent
        (e-session-storage--state store))))

(defun e-session-storage-runtime-store (store)
  "Return STORE's injected runtime-store adapter, or nil when ephemeral."
  (e-session-storage-sqlite-runtime store))

(defun e-session-storage-submit (store kind body on-settle &optional escrow)
  "Submit current SQLite KIND BODY and report `(RESULT ERROR)' asynchronously.

This narrow physical operation is for the session application's FIFO
coordinator.  It does not expose DP5A request details to session callers."
  (e-session-storage--require-sqlite store "Asynchronous session operation")
  (e-session-storage-sqlite-submit store kind body on-settle escrow))

(defun e-session-storage-validate-operation-body (store body)
  "Preflight STORE's one physical session mutation BODY."
  (e-session-storage--require-sqlite store "Session operation validation")
  (e-session-storage-sqlite-validate-operation-body store body))

(defun e-session-storage-submit-owned
    (store session-id body on-settle &optional escrow)
  "Submit one SESSION-ID write and report its physical settlement.

The private owner key never crosses the session storage port.  Session-domain
pending and suspect policy belongs to the application service."
  (e-session-storage--require-sqlite store "Owned session operation")
  (e-session-storage-sqlite-submit-owned
   store 'write body (cons 'session session-id) on-settle escrow))

(defun e-session-storage-cancel-operation (store operation)
  "Cancel queued opaque session OPERATION through STORE's adapter."
  (e-session-storage--require-sqlite store "Session operation cancellation")
  (e-session-storage-sqlite-cancel-operation store operation))

(defun e-session-storage--require-sqlite (store operation)
  "Require current persistent STORE for OPERATION."
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-session-storage-error
            (list (format "%s requires migrated SQLite session state" operation))))
  store)

(defun e-session-storage-session-reference (store session-id)
  "Return current adapter reference for SESSION-ID in STORE."
  (and (e-session-storage-sqlite-p store)
       (e-session-storage-sqlite-reference session-id)))

(defun e-session-storage-session-header (store session-id)
  "Return bounded current resume metadata for SESSION-ID."
  (e-session-storage--require-sqlite store "Session header")
  (e-session-storage-sqlite-header store session-id))

(defun e-session-storage-read-resume-checkpoint (store session-id)
  "Read SESSION-ID's bounded current resume checkpoint."
  (e-session-storage--require-sqlite store "Resume checkpoint read")
  (e-session-storage-sqlite-read-checkpoint store session-id))

(defun e-session-storage-resume-checkpoint-present-p (store session-id)
  "Return non-nil when SESSION-ID has a current resume checkpoint."
  (and (e-session-storage-sqlite-p store)
       (e-session-storage-sqlite-checkpoint-present-p store session-id)))

(defun e-session-storage-persist-resume-checkpoint (store session-id value)
  "Persist SESSION-ID's bounded resume VALUE."
  (e-session-storage--require-sqlite store "Resume checkpoint write")
  (e-session-storage-sqlite-write-checkpoint store session-id value))

(defun e-session-storage-read-session-records (store session-id &optional offset)
  "Return detached records for SESSION-ID from OFFSET."
  (e-session-storage--require-sqlite store "Session record read")
  (e-session-storage-sqlite-read-records store session-id offset))

(defun e-session-storage-read-session-page
    (store session-id &optional after limit)
  "Return a bounded current record page after AFTER for SESSION-ID."
  (e-session-storage--require-sqlite store "Session page read")
  (e-session-storage-sqlite-read-page store session-id after limit))

(defun e-session-storage-session-ids (store)
  "Return durable session ids in current STORE."
  (e-session-storage--require-sqlite store "Session listing")
  (e-session-storage-sqlite-session-ids store))

(defun e-session-storage-read-catalog-projection (store)
  "Read STORE's detached current catalog projection."
  (e-session-storage--require-sqlite store "Session catalog read")
  (e-session-storage-sqlite-read-catalog store))

(defun e-session-storage--mark-checkpoint-dirty (store session-id)
  "Mark SESSION-ID's rebuildable resume projection dirty."
  (when (e-session-storage-sqlite-p store)
    (puthash session-id t
             (e-session-storage--state-checkpoint-dirty-session-ids
              (e-session-storage--state store)))))

(defun e-session-storage-checkpoint-dirty-session-ids (store)
  "Return STORE session ids needing a resume projection."
  (let (ids)
    (maphash (lambda (session-id _value) (push session-id ids))
             (e-session-storage--state-checkpoint-dirty-session-ids
              (e-session-storage--state store)))
    (nreverse ids)))

(defun e-session-storage-checkpoint-mark-clean (store session-ids)
  "Mark SESSION-IDS' submitted resume projections clean."
  (dolist (session-id session-ids)
    (remhash session-id
             (e-session-storage--state-checkpoint-dirty-session-ids
              (e-session-storage--state store)))))

(defun e-session-storage-note-projection-error (store error)
  "Record bounded derived-projection ERROR in STORE status."
  (let ((message (condition-case nil
                     (error-message-string error)
                   (error "Unknown derived projection failure"))))
    (setf (e-session-storage--state-projection-last-error
           (e-session-storage--state store))
          (list :symbol (if (symbolp (car-safe error)) (car error) 'error)
                :message (substring
                          message 0
                          (min (length message)
                               e-session-storage--projection-error-message-limit))))))

(defun e-session-storage-clear-projection-error (store)
  "Clear STORE's last derived-projection diagnostic."
  (setf (e-session-storage--state-projection-last-error
         (e-session-storage--state store)) nil))

(defun e-session-storage-durability-status (store)
  "Return bounded current durability status for STORE."
  (let* ((state (e-session-storage--state store))
         (dirty (hash-table-count
                 (e-session-storage--state-checkpoint-dirty-session-ids state)))
         (error (e-session-storage--state-projection-last-error state)))
    (if (e-session-storage-sqlite-p store)
        (append (list :checkpoint-dirty-count dirty
                      :index-write-pending (and (or (> dirty 0) error) t)
                      :projection-last-error (copy-tree error))
                (e-session-storage-sqlite-status store))
      (list :checkpoint-dirty-count 0 :index-write-pending nil
            :projection-last-error nil :unsettled-write-count 0))))

(defun e-session-storage-admission-controller-enabled-p (store)
  "Return non-nil when STORE can atomically admit session records."
  (e-session-storage-sqlite-p store))

(defun e-session-storage-prepare-mutation (store session-id record)
  "Preflight detached semantic RECORD for SESSION-ID in STORE."
  (if (e-session-storage-sqlite-p store)
      (progn
        (e-session-storage-sqlite-prepare-append-batch
         session-id (list (copy-tree record)))
        (copy-tree record))
    (copy-tree record)))

(defun e-session-storage-prepare-mutation-batch (store session-id records)
  "Preflight one complete current session mutation batch."
  (e-session-storage--require-sqlite store "Session mutation batch")
  (unless records
    (signal 'e-session-storage-error (list "Mutation batch is empty" session-id)))
  (e-session-storage-sqlite-prepare-append-batch
   session-id (mapcar #'copy-tree records)))

(defun e-session-storage-validate-admission-for-store
    (store session-id records)
  "Preflight SESSION-ID admission RECORDS for current STORE."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-prepare-mutation-batch store session-id records)
    (mapcar #'copy-tree records)))

(defun e-session-storage--blocking-query-delta (store session-id records)
  "Derive RECORDS' final query row at an explicit synchronous boundary.

This compatibility seam is restricted to already-blocking offline, operator,
and test callers.  Interactive v6 code derives commands through
`e-session-async' and never reaches this function."
  (let ((state
         (e-session-storage-sqlite-query-state-blocking store session-id))
        (position 0))
    (setq position (or (plist-get state :journal-position) 0))
    (dolist (record records)
      (setq position (1+ position))
      (let ((positioned (copy-tree record t)))
        (plist-put positioned :journal-position position)
        (setq state (e-session-query-state-apply-record state positioned))))
    state))

(defun e-session-storage-commit-mutation (store session-id record)
  "Commit semantic RECORD at an explicit synchronous compatibility boundary."
  (e-session-storage--profile-call
   'session.append-record
   (list :session-id session-id
         :metadata (list :record-type (plist-get record :type)))
   (lambda ()
     (when (e-session-storage-persistent-p store)
       (e-session-storage--mark-checkpoint-dirty store session-id)
       (e-session-storage-sqlite-append
        store session-id record
        (e-session-storage--blocking-query-delta
         store session-id (list record)))))))

(defun e-session-storage-commit-mutation-batch (store session-id records)
  "Commit RECORDS atomically at a synchronous compatibility boundary."
  (e-session-storage--require-sqlite store "Session mutation batch")
  (e-session-storage--mark-checkpoint-dirty store session-id)
  (e-session-storage-sqlite-append-batch-with-query-delta
   store session-id records
   (e-session-storage--blocking-query-delta store session-id records)))

(defun e-session-storage-commit-mutation-batch-with-query-delta
    (store session-id records query-delta)
  "Commit RECORDS and complete domain-owned QUERY-DELTA atomically.

This narrow migration/application seam keeps record interpretation in the
session domain while allowing the SQLite adapter to validate and persist the
final current row together with the canonical journal batch."
  (e-session-storage--require-sqlite store "Session mutation batch")
  (e-session-storage--mark-checkpoint-dirty store session-id)
  (e-session-storage-sqlite-append-batch-with-query-delta
   store session-id records query-delta))

(defun e-session-storage-publish-admission
    (store session-id records &optional write-index)
  "Commit admission RECORDS atomically, optionally publishing projections."
  (when (e-session-storage-persistent-p store)
    (e-session-storage-commit-mutation-batch store session-id records)
    (when write-index (e-session-storage--write-index store))))

(defun e-session-storage-publish-projections
    (store index-projection checkpoint-projection-operation)
  "Publish rebuildable catalog and resume projections through STORE."
  (when (e-session-storage-persistent-p store)
    (let ((state (e-session-storage--state store)))
      (setf (e-session-storage--state-index-projection state)
            (copy-tree index-projection)
            (e-session-storage--state-checkpoint-projection-operation state)
            checkpoint-projection-operation)
      (e-session-storage-sqlite-write-catalog store index-projection)
      (let ((session-ids (e-session-storage-checkpoint-dirty-session-ids store)))
        (dolist (session-id session-ids)
          (e-session-storage-persist-resume-checkpoint
           store session-id (funcall checkpoint-projection-operation session-id)))
        (e-session-storage-checkpoint-mark-clean store session-ids))
      (e-session-storage-clear-projection-error store))))

(defun e-session-storage-publish-catalog-projection (store index-projection)
  "Publish only STORE's rebuildable catalog INDEX-PROJECTION.

Offline migration uses this narrow operation after translated checkpoints are
already authoritative.  It deliberately neither writes nor cleans checkpoints."
  (when (e-session-storage-persistent-p store)
    (let* ((state (e-session-storage--state store))
           (projection (copy-tree index-projection)))
      (e-session-storage-sqlite-write-catalog store projection)
      (setf (e-session-storage--state-index-projection state) projection)
      (e-session-storage-clear-projection-error store)))
  index-projection)

(defun e-session-storage--write-index (store)
  "Publish STORE's current rebuildable session projections."
  (when (e-session-storage-persistent-p store)
    (let ((state (e-session-storage--state store)))
      (e-session-storage-publish-projections
       store
       (e-session-storage--state-index-projection state)
       (e-session-storage--state-checkpoint-projection-operation state)))))

(defun e-session-storage-abort-session (store session-id)
  "Remove unpublished SESSION-ID physical state from STORE."
  (when (e-session-storage-sqlite-p store)
    (e-session-storage-sqlite-delete store session-id))
  (remhash session-id
           (e-session-storage--state-checkpoint-dirty-session-ids
            (e-session-storage--state store)))
  t)

(defun e-session-storage-finalize-store (store on-done on-error)
  "Finalize STORE's ordered durability boundary."
  (if (not (e-session-storage-persistent-p store))
      (funcall on-done t)
    (condition-case err
        (progn
          (let* ((state (e-session-storage--state store))
                 (dirty (hash-table-count
                         (e-session-storage--state-checkpoint-dirty-session-ids
                          state))))
            (when (or (> dirty 0)
                      (e-session-storage--state-projection-last-error state))
              (condition-case projection-error
                  (e-session-storage--write-index store)
                (error
                 (e-session-storage-note-projection-error store projection-error)
                 (signal (car projection-error) (cdr projection-error))))))
          (e-session-storage-sqlite-ordered-barrier store)
          (funcall on-done t))
      (error (funcall on-error err)))))

(defun e-session-storage-flush-write-queue (store)
  "Retain the historical barrier name over current STORE ordering."
  (when (e-session-storage-persistent-p store)
    (e-session-storage-sqlite-ordered-barrier store))
  store)

(defun e-session-storage-enable (store)
  "Return STORE's injected current runtime adapter."
  (e-session-storage--require-sqlite store "Persistent session runtime")
  (e-session-storage-runtime-store store))

(defun e-session-storage-close (store)
  "Close STORE when it owns a runtime worker.

When the optimistic session service is loaded, close first releases its
process-local pending and suspect status.  Runtime close remains responsible
for settling or fencing physical requests."
  (unwind-protect
      (when (fboundp 'e-session-async-teardown)
        (e-session-async-teardown store))
    (when (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-close store))))

(provide 'e-session-storage)

;;; e-session-storage.el ends here

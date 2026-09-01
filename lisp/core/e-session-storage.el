;;; e-session-storage.el --- Session storage owner and JSONL adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the current physical session-storage adapter: JSONL append ordering,
;; queued writes, the Node writer outbox, checkpoint/index write scheduling,
;; and unsettled durability state.  The adapter is intentionally replaceable
;; by Feature 87 without changing the aggregate or durable record contracts.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'e-session-codec)

(define-error 'e-session-persistence-unavailable
  "Session persistence controller is unavailable")
(define-error 'e-session-storage-error "Session persistence error")
(define-error 'e-session-storage-command-error
  "Invalid session persistence command"
  'e-session-storage-error)

(require 'e-session-storage-sqlite)

;; The aggregate is intentionally opaque to this module.  One storage state
;; object owns all physical-adapter lifetime, queue, checkpoint, and outbox
;; fields.  The owner key is only an identity token supplied by the
;; composition root; storage never reads its representation.
(cl-defstruct (e-session-storage-state
               (:constructor e-session-storage--state-create)
               (:predicate e-session-storage--state-p)
               (:conc-name e-session-storage--state-))
  owner directory sessions-directory index-file persistent write-mode
  write-queue write-queue-timer index-write-pending
  (checkpoint-dirty-session-ids (make-hash-table :test 'equal))
  (write-queue-generation 0) (write-queue-sequence 0)
  (unsettled-write-count 0) (unsettled-generation 0)
  index-projection checkpoint-projection-operation projection-last-error
  controller)

(defvar e-session-storage--states
  (make-hash-table :test 'eq :weakness 'key)
  "Storage state objects keyed by opaque application owners.")

(defun e-session-storage--state (owner)
  "Return storage state for opaque OWNER, creating an ephemeral one if needed."
  (if (e-session-storage--state-p owner)
      owner
    (or (gethash owner e-session-storage--states)
        (let ((state (e-session-storage--state-create :owner owner)))
          (puthash owner state e-session-storage--states)
          state))))

(cl-defun e-session-storage-register
    (owner &key directory sessions-directory index-file persistent write-mode
           (backend 'legacy) runtime-store)
  "Register a storage state for opaque OWNER and return that state.
DIRECTORY and the derived paths are supplied by the composition root; this
owner does not inspect the aggregate's path/configuration slots."
  (let* ((directory (and directory
                         (file-name-as-directory (expand-file-name directory))))
         (sessions-directory
          (or sessions-directory
              (and directory (expand-file-name "sessions" directory))))
         (index-file
          (or index-file
              (and directory (expand-file-name "index.json" directory))))
         (state (e-session-storage--state-create
                 :owner owner :directory directory
                 :sessions-directory sessions-directory :index-file index-file
                 :persistent (and persistent directory)
                 :write-mode write-mode)))
    (puthash owner state e-session-storage--states)
    (e-session-storage-sqlite-register owner backend runtime-store)
    state))

(defun e-session-storage--state-for (owner)
  "Return the storage state associated with opaque OWNER, if registered."
  (if (e-session-storage--state-p owner)
      owner
    (gethash owner e-session-storage--states)))

(defun e-session-storage--controller-state (controller)
  "Return the storage state owned by CONTROLLER.

Controllers created by older callers keep an opaque application token in their
`store' slot.  Resolving that token here keeps the transport layer independent
of the aggregate representation while allowing a controller to retain its
existing public lifecycle."
  (e-session-storage--state (e-session-storage--controller-store controller)))

(defcustom e-session-write-queue-delay 0.05
  "Seconds to wait before flushing queued persistent session writes."
  :type 'number :group 'e-session)

(defvar e-session-storage--unsettled-write-count 0)
(defvar e-session-storage--unsettled-generation 0)
(defvar e-session-storage--unsettled-change-function nil)
(defvar e-session-storage-unsettled-change-hook nil
  "Hook run after storage durability state changes.")

(defun e-session-storage--profile-call (event options thunk)
  "Measure storage EVENT when the optional developer profiler is loaded."
  (if (and (fboundp 'e-dev-profile-enabled-p)
           (fboundp 'e-dev-profile-measure-thunk)
           (e-dev-profile-enabled-p))
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-session-storage-unsettled-state ()
  "Return constant-time aggregate durability state."
  (list :generation e-session-storage--unsettled-generation
        :writes e-session-storage--unsettled-write-count))

(defun e-session-storage--adjust-unsettled-writes (store delta)
  "Adjust STORE and the process-wide storage durability projection by DELTA.
The transition is published only after both owner counters have been updated,
so quiescence observers never see a false zero between related writes."
  (let* ((state (e-session-storage--state store))
         (store-next (+ (e-session-storage--state-unsettled-write-count state)
                        delta))
        (global-next (+ e-session-storage--unsettled-write-count delta)))
    (when (or (< store-next 0) (< global-next 0))
      (signal 'e-session-error
              (list "Storage unsettled count would become negative"
                    store-next global-next)))
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

(defun e-session-storage-persistent-p (store)
  "Return non-nil when STORE writes to the current durable adapter."
  (let ((state (e-session-storage--state store)))
    (and (e-session-storage--state-persistent state)
         (e-session-storage--state-directory state))))

(defun e-session-storage-sqlite-p (store)
  "Return non-nil when STORE uses the opt-in SQLite physical adapter."
  (e-session-storage-sqlite-store-p store))

(defun e-session-storage-runtime-store (store)
  "Return STORE's runtime-store adapter, or nil for the legacy backend."
  (e-session-storage-sqlite-runtime store))

(defun e-session-storage--ensure-directories (store)
  "Ensure persistent directories for STORE exist."
  (when (e-session-storage-persistent-p store)
    (make-directory
     (e-session-storage--state-sessions-directory
      (e-session-storage--state store)) t)))

(defun e-session-storage--session-file (store session-id)
  "Return JSONL file path for SESSION-ID in STORE."
  (expand-file-name (concat session-id ".jsonl")
                    (e-session-storage--state-sessions-directory
                     (e-session-storage--state store))))

(defun e-session-storage-session-reference (store session-id)
  "Return the durable file reference for SESSION-ID.

The reference is a catalog-facing value, not an invitation for callers to
manage the journal.  Reads, writes, and temporary-file handling remain inside
this owner."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-reference session-id)
    (e-session-storage--session-file store session-id)))

(defun e-session-storage--checkpoint-file (store session-id)
  "Return resume-checkpoint file path for SESSION-ID in STORE."
  (expand-file-name (concat session-id ".checkpoint.json")
                    (e-session-storage--state-sessions-directory
                     (e-session-storage--state store))))

(defun e-session-storage--journal-size (store session-id)
  "Return byte size of SESSION-ID's journal, or zero when it is absent."
  (let ((file (e-session-storage--session-file store session-id)))
    (if (file-readable-p file)
        (file-attribute-size (file-attributes file))
      0)))

(defun e-session-storage-session-header (store session-id)
  "Return bounded resume metadata for SESSION-ID.

The result contains only the existence bit, durable byte size, and a private
adapter reference used by the application service to identify the session.
Callers do not need to derive paths or inspect storage state themselves."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-header store session-id)
    (let* ((file (e-session-storage--session-file store session-id))
         (attributes (and (file-readable-p file) (file-attributes file))))
      (list :session-id session-id
            :present (and attributes t)
            :byte-size (if attributes (file-attribute-size attributes) 0)
            :reference file))))

(defun e-session-storage--read-file-value (store file-name)
  "Read JSON FILE-NAME below STORE's directory as a detached Lisp value.

This is a physical adapter operation.  It intentionally performs no schema or
aggregate validation; callers pass the returned value to the codec owner."
  (let ((file (expand-file-name file-name
                                (e-session-storage--state-directory
                                 (e-session-storage--state store)))))
    (unless (file-readable-p file)
      (signal 'file-missing (list "Opening file" file)))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (json-parse-string (buffer-string)
                         :object-type 'plist
                         :array-type 'list
                         :null-object nil
                         :false-object :json-false))))

(defun e-session-storage--read-checkpoint (store session-id)
  "Read SESSION-ID's physical checkpoint value."
  (e-session-storage--read-file-value
   store
   (file-relative-name
    (e-session-storage--checkpoint-file store session-id)
    (e-session-storage--state-directory (e-session-storage--state store)))))

(defun e-session-storage-read-resume-checkpoint (store session-id)
  "Read SESSION-ID's bounded resume checkpoint value.

This is the semantic resume operation exposed to the application service;
checkpoint file naming and JSON parsing remain storage implementation details."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-read-checkpoint store session-id)
    (e-session-storage--read-checkpoint store session-id)))

(defun e-session-storage-resume-checkpoint-present-p (store session-id)
  "Return non-nil when SESSION-ID has a readable resume checkpoint."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-checkpoint-present-p store session-id)
    (file-readable-p (e-session-storage--checkpoint-file store session-id))))

(defun e-session-storage--write-checkpoint (store session-id value)
  "Atomically write detached checkpoint VALUE for SESSION-ID.

The storage adapter owns the temporary-file and rename boundary.  VALUE is
already a catalog/codec projection and is not interpreted here."
  (when (e-session-storage-persistent-p store)
    (e-session-storage--ensure-directories store)
    (let* ((target (e-session-storage--checkpoint-file store session-id))
           (temporary (make-temp-file (concat target ".") nil ".tmp"))
           (coding-system-for-write 'utf-8))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (insert (json-encode value) "\n"))
            (rename-file temporary target t))
        (when (file-exists-p temporary)
          (delete-file temporary)))
      target)))

(defun e-session-storage-persist-resume-checkpoint (store session-id value)
  "Atomically persist the bounded resume VALUE for SESSION-ID."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-write-checkpoint store session-id value)
    (e-session-storage--write-checkpoint store session-id value)))

(defun e-session-storage--read-journal (store session-id &optional offset)
  "Return physical JSONL lines from SESSION-ID beginning at byte OFFSET.

The returned values are parsed plists and remain detached.  The storage owner
does not apply or normalize them; replay belongs to the application service and
the codec/aggregate owners."
  (let ((file (e-session-storage--session-file store session-id))
        (offset (or offset 0)))
    (unless (file-readable-p file)
      (signal 'file-missing (list "Opening session journal" file)))
    (with-temp-buffer
      (let ((coding-system-for-read 'no-conversion))
        (insert-file-contents-literally file nil offset))
      (goto-char (point-min))
      (let (records)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p line)
              (push (e-session-codec-json-read-line
                     (decode-coding-string line 'utf-8))
                    records)))
          (forward-line 1))
        (nreverse records)))))

(defun e-session-storage-read-session-records (store session-id &optional offset)
  "Return detached semantic-input records for SESSION-ID from OFFSET.

The adapter owns line framing and file reads; the application service owns
codec decoding and aggregate replay application."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-read-records store session-id offset)
    (e-session-storage--read-journal store session-id offset)))

(defun e-session-storage-read-session-page
    (store session-id &optional after limit)
  "Return a bounded semantic record page after AFTER for SESSION-ID."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-read-page store session-id after limit)
    (let* ((records (e-session-storage-read-session-records store session-id))
           (after (or after 0))
           (limit (or limit 256))
           (slice (seq-take (nthcdr after records) limit))
           (next (+ after (length slice))))
      (list :records (cl-loop for value in slice
                              for position from (1+ after)
                              collect (list :position position :value value))
            :next (and (= (length slice) limit) next)))))

(defun e-session-storage-read-session-chunk
    (store session-id position next-position)
  "Return one bounded raw-text resume chunk for SESSION-ID.

POSITION and NEXT-POSITION are byte offsets supplied by the cooperative load
operation.  The returned value contains text and the resulting byte position;
the caller still owns line-to-semantic replay ordering, while storage owns the
physical file reference and coding setup."
  (let ((file (e-session-storage--session-file store session-id)))
    (unless (file-readable-p file)
      (signal 'file-missing (list "Opening session journal" file)))
    (with-temp-buffer
      (let ((coding-system-for-read 'no-conversion))
        (insert-file-contents-literally file nil position next-position))
      (list :session-id session-id
            :position position
            :next-position next-position
            :text (buffer-string)))))

(defun e-session-storage-session-ids (store)
  "Return session ids for JSONL files currently present in STORE."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-session-ids store)
    (let ((directory (e-session-storage--state-sessions-directory
                    (e-session-storage--state store))))
      (if (file-directory-p directory)
          (mapcar #'file-name-base
                  (directory-files directory t "\\.jsonl\\'"))
        nil))))

(defun e-session-storage--read-index (store)
  "Return the physical session index value, or nil when unavailable."
  (let ((file (e-session-storage--state-index-file
               (e-session-storage--state store))))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (json-parse-string (buffer-string)
                           :object-type 'plist
                           :array-type 'list
                           :null-object e-session-codec-json-null
                           :false-object :json-false)))))

(defun e-session-storage-read-catalog-projection (store)
  "Read the detached durable catalog projection for STORE."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-read-catalog store)
    (e-session-storage--read-index store)))

(defun e-session-storage--mark-checkpoint-dirty (store session-id)
  "Mark SESSION-ID's resume state dirty in STORE."
  (puthash session-id t
           (e-session-storage--state-checkpoint-dirty-session-ids
            (e-session-storage--state store))))

(defun e-session-storage-checkpoint-dirty-session-ids (store)
  "Return STORE session ids needing a resume checkpoint."
  (let (ids)
    (maphash (lambda (session-id _value) (push session-id ids))
             (e-session-storage--state-checkpoint-dirty-session-ids
              (e-session-storage--state store)))
    (nreverse ids)))

(defun e-session-storage-checkpoint-mark-clean (store session-ids)
  "Mark SESSION-IDS' submitted resume state clean in STORE."
  (dolist (session-id session-ids)
    (remhash session-id
             (e-session-storage--state-checkpoint-dirty-session-ids
              (e-session-storage--state store)))))

(defun e-session-storage--queued-writes-p (store)
  "Return non-nil when STORE batches persistent writes through a timer."
  (eq (e-session-storage--state-write-mode
       (e-session-storage--state store)) 'queued))

(defun e-session-storage--controller (store)
  "Return STORE's asynchronous persistence controller, if configured."
  (e-session-storage--state-controller (e-session-storage--state store)))

(defun e-session-storage-admission-controller-enabled-p (store)
  "Return non-nil when STORE has an asynchronous admission adapter."
  (or (e-session-storage-sqlite-p store)
      (and (e-session-storage--controller store) t)))

(defun e-session-storage--index-write-pending-p (store)
  "Return non-nil when STORE has a derived-index write outstanding.

This is the semantic durability query used by the session application service;
the queued entry, generation, and timer representations remain private to the
storage owner."
  (and (e-session-storage--state-index-write-pending
        (e-session-storage--state store))
       t))

(defun e-session-storage-durability-status (store)
  "Return bounded durability status for STORE.

The result intentionally contains counts and lifecycle booleans only.  It does
not expose queued records, timers, generations, or the storage state object to
callers that need to preserve an application-level rollback invariant."
  (let ((state (e-session-storage--state store)))
    (if (e-session-storage-sqlite-p store)
        (let ((dirty-count
               (hash-table-count
                (e-session-storage--state-checkpoint-dirty-session-ids state)))
              (error (e-session-storage--state-projection-last-error state)))
          (append
           (list :checkpoint-dirty-count dirty-count
                 :index-write-pending (and (or (> dirty-count 0) error) t)
                 :projection-last-error (copy-tree error))
           (e-session-storage-sqlite-status store)))
      (list :queued-write-count
          (length (e-session-storage--state-write-queue state))
          :write-timer-active
          (and (timerp (e-session-storage--state-write-queue-timer state)) t)
          :index-write-pending
          (e-session-storage--index-write-pending-p store)
          :unsettled-write-count
          (e-session-storage--state-unsettled-write-count state)
          :checkpoint-dirty-count
            (hash-table-count
             (e-session-storage--state-checkpoint-dirty-session-ids state))))))

(defconst e-session-storage--projection-error-message-limit 512
  "Maximum diagnostic message length retained for derived projections.")

(defun e-session-storage-note-projection-error (store error)
  "Record bounded derived-projection ERROR for STORE durability status."
  (let* ((symbol (and (consp error) (car error)))
         (message
          (condition-case nil
              (error-message-string error)
            (error "Unknown derived projection failure"))))
    (setf (e-session-storage--state-projection-last-error
           (e-session-storage--state store))
          (list :symbol (if (symbolp symbol) symbol 'error)
                :message
                (substring message 0
                           (min (length message)
                                e-session-storage--projection-error-message-limit))))))

(defun e-session-storage-clear-projection-error (store)
  "Clear STORE's last derived-projection diagnostic after successful retry."
  (setf (e-session-storage--state-projection-last-error
         (e-session-storage--state store))
        nil))

(defun e-session-storage--append-record-now (store session-id record)
  "Immediately append encoded RECORD for SESSION-ID in persistent STORE.
The public append/preflight boundary performs semantic-to-wire normalization;
keeping this physical primitive wire-shaped avoids encoding the same record a
second time on direct, queued, and retry paths."
  (when (e-session-storage-persistent-p store)
    (e-session-storage--ensure-directories store)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-buffer
        (insert (json-encode record) "\n")
        (write-region (point-min) (point-max)
                      (e-session-storage--session-file store session-id)
                      t 'silent)))))

(defun e-session-storage--record-already-appended-p (store session-id record)
  "Return non-nil when encoded RECORD already occupies SESSION-ID's journal."
  (let ((journal (e-session-storage--session-file store session-id))
        (encoded (json-encode record)))
    (and (file-readable-p journal)
         (with-temp-buffer
           (insert-file-contents journal)
           (goto-char (point-min))
           (catch 'found
             (while (not (eobp))
               (when (string= encoded
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))
                 (throw 'found t))
               (forward-line 1)))))))

(defun e-session-storage--index-json (store)
  "Return the current explicitly supplied index projection as JSON."
  (let ((projection (e-session-storage--state-index-projection
                     (e-session-storage--state store))))
    (concat (json-encode
             (vconcat (mapcar #'e-session-codec-index-entry-for-json
                              (or projection nil))))
            "\n")))

(defun e-session-storage--set-index-projection (store projection)
  "Install detached catalog PROJECTION for the next index write.
The application service supplies this value after composing aggregate and
catalog owners; storage only serializes it."
  (setf (e-session-storage--state-index-projection
         (e-session-storage--state store))
        (copy-tree projection))
  projection)

(defun e-session-storage--set-checkpoint-projection-operation (store operation)
  "Install application-produced checkpoint projection OPERATION for STORE."
  (setf (e-session-storage--state-checkpoint-projection-operation
         (e-session-storage--state store))
        operation)
  operation)

(defun e-session-storage-publish-projections
    (store index-projection checkpoint-projection-operation)
  "Publish catalog projections through STORE's durability mode.

The application service supplies the detached index value and an operation
that produces a current semantic checkpoint manifest for one session.  This
single boundary retains that operation only across the existing debounce and
checkpoint batch; storage never inspects aggregate or catalog representation."
  (e-session-storage--set-index-projection store index-projection)
  (e-session-storage--set-checkpoint-projection-operation
   store checkpoint-projection-operation)
  (if (e-session-storage-sqlite-p store)
      (progn
        (e-session-storage-sqlite-write-catalog store index-projection)
        ;; The current session mutation marks its owner dirty before this
        ;; derived publication.  Persist each bounded owner checkpoint after
        ;; its record commit, then clear precisely that captured set.
        (let ((session-ids
               (e-session-storage-checkpoint-dirty-session-ids store)))
          (dolist (session-id session-ids)
            (e-session-storage-persist-resume-checkpoint
             store session-id
             (funcall checkpoint-projection-operation session-id)))
          (e-session-storage-checkpoint-mark-clean store session-ids))
        (e-session-storage-clear-projection-error store))
    (e-session-storage--write-index store)))

(defun e-session-storage--write-index-now (store)
  "Immediately write STORE's persistent session index."
  (when (e-session-storage-persistent-p store)
    (e-session-storage--ensure-directories store)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file
          (e-session-storage--state-index-file (e-session-storage--state store))
        (insert (e-session-storage--index-json store))))))

(defconst e-session-storage--critical-record-types
  '("session"
    "session-info"
    "message"
    "board-message"
    "message-display"
    "activity-event"
    "branch-summary"
    "compaction"
    "provider-anchor"
    "process-report"
    "context-generation"
    "context-promotion"
    "context-curation-package"
    "current-branch"
    "messages-cleared")
  "Persistent record types that must flush before derived queued records.")

(defun e-session-storage--queued-record-criticality (record)
  "Return the queued-write criticality class for RECORD."
  (if (member (plist-get record :type) e-session-storage--critical-record-types)
      'critical
    'derived))

(defun e-session-storage--queued-record-dependencies (session-id record)
  "Return dependency metadata for queued RECORD in SESSION-ID."
  (list :session-id session-id
        :record-id (or (plist-get record :id)
                       (plist-get record :entry_id))
        :parent-id (or (plist-get record :parent_id)
                       (plist-get record :previous_entry_id))))

(defun e-session-storage--queued-write-entry (store session-id record)
  "Return a metadata-bearing queued write entry for RECORD."
  (list :session-id session-id
        :record record
        :generation (e-session-storage--state-write-queue-generation
                     (e-session-storage--state store))
        :sequence (cl-incf (e-session-storage--state-write-queue-sequence
                            (e-session-storage--state store)))
        :criticality (e-session-storage--queued-record-criticality record)
        :dependencies (e-session-storage--queued-record-dependencies session-id record)))

(defun e-session-storage--queued-admission-entry (store session-id records)
  "Return one queued transaction entry for admission RECORDS.
The records stay together until the direct writer has published their complete
batch, so a queue flush cannot expose a root without its board association."
  (list :session-id session-id
        :records records
        :generation (e-session-storage--state-write-queue-generation
                     (e-session-storage--state store))
        :sequence (cl-incf (e-session-storage--state-write-queue-sequence
                            (e-session-storage--state store)))
        :criticality (e-session-storage--queued-record-criticality (car records))
        :dependencies (e-session-storage--queued-record-dependencies
                       session-id (car records))))

(defun e-session-storage--queued-index-entry (store)
  "Return a metadata-bearing queued derived-index write entry."
  (list :generation (e-session-storage--state-write-queue-generation
                     (e-session-storage--state store))
        :sequence (cl-incf (e-session-storage--state-write-queue-sequence
                            (e-session-storage--state store)))
        :criticality 'derived
        :dependencies '(:source queued-records)))

(defun e-session-storage--queued-entry-session-id (entry)
  "Return queued ENTRY's session id."
  (if (and (consp entry)
           (keywordp (car entry)))
      (plist-get entry :session-id)
    (car entry)))

(defun e-session-storage--queued-entry-record (entry)
  "Return queued ENTRY's persistent record."
  (if (and (consp entry)
           (keywordp (car entry))
           (plist-member entry :record))
      (plist-get entry :record)
    (cdr entry)))

(defun e-session-storage--queued-entry-records (entry)
  "Return the persistent records represented by queued ENTRY."
  (if (and (consp entry)
           (keywordp (car entry))
           (plist-member entry :records))
      (plist-get entry :records)
    (list (e-session-storage--queued-entry-record entry))))

(defun e-session-storage--queued-entry-current-p (store entry)
  "Return non-nil when queued ENTRY belongs to STORE's current generation."
  (let ((generation (plist-get entry :generation)))
    (or (null generation)
        (= generation (e-session-storage--state-write-queue-generation
                       (e-session-storage--state store))))))

(defun e-session-storage--queued-index-current-p (store entry)
  "Return non-nil when queued index ENTRY belongs to STORE's current generation."
  (or (eq entry t)
      (and (consp entry)
           (keywordp (car entry))
           (= (plist-get entry :generation)
              (e-session-storage--state-write-queue-generation
               (e-session-storage--state store))))))

(defun e-session-storage--clear-write-queue-timer (store)
  "Clear STORE's queued write timer slot."
  (let ((timer (e-session-storage--state-write-queue-timer
                (e-session-storage--state store))))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (e-session-storage--state-write-queue-timer
         (e-session-storage--state store)) nil))

(defun e-session-storage--drop-queued-write-entry (store entry)
  "Drop acknowledged queued write ENTRY from STORE."
  (when (memq entry (e-session-storage--state-write-queue
                     (e-session-storage--state store)))
    (setf (e-session-storage--state-write-queue
           (e-session-storage--state store))
          (delq entry (e-session-storage--state-write-queue
                       (e-session-storage--state store))))
    (e-session-storage--adjust-unsettled-writes store -1)))

(defun e-session-storage--drop-stale-queued-write-entries (store)
  "Remove stale-generation queued writes from STORE."
  (let* ((old (e-session-storage--state-write-queue
               (e-session-storage--state store)))
         (current (cl-remove-if-not
                   (lambda (entry)
                     (e-session-storage--queued-entry-current-p store entry))
                   old))
         (dropped (- (length old) (length current))))
    (setf (e-session-storage--state-write-queue
           (e-session-storage--state store)) current)
    (when (> dropped 0)
      (e-session-storage--adjust-unsettled-writes store (- dropped)))))

(defun e-session-storage--queued-entry-critical-p (entry)
  "Return non-nil when queued ENTRY must flush before derived records."
  (not (and (consp entry)
            (keywordp (car entry))
            (eq (plist-get entry :criticality) 'derived))))

(defun e-session-storage--order-queued-records-for-flush (entries)
  "Return ENTRIES with critical records before derived records.
Ordering remains stable within each criticality class."
  (let (critical derived)
    (dolist (entry entries)
      (if (e-session-storage--queued-entry-critical-p entry)
          (push entry critical)
        (push entry derived)))
    (nconc (nreverse critical) (nreverse derived))))

(defun e-session-storage--flush-queued-records (store entries)
  "Append queued ENTRIES, acknowledging each durable record write."
  (dolist (entry entries)
    (let ((session-id (e-session-storage--queued-entry-session-id entry))
          (records (e-session-storage--queued-entry-records entry)))
      (condition-case error
          (if (cdr records)
              (e-session-storage--append-admission-records-now store session-id records)
            (e-session-storage--append-record-now store session-id (car records)))
        (error
         (unless (and (= (length records) 1)
                      (e-session-storage--record-already-appended-p
                       store session-id (car records)))
           (signal (car error) (cdr error)))))
      (e-session-storage--drop-queued-write-entry store entry))))

(defun e-session-storage-flush-write-queue (store)
  "Synchronously flush queued persistent writes for STORE.
Return STORE."
  (e-session-storage--clear-write-queue-timer store)
  (let* ((raw-entries (reverse (e-session-storage--state-write-queue
                               (e-session-storage--state store))))
         (entries (cl-remove-if-not
                   (lambda (entry)
                     (e-session-storage--queued-entry-current-p store entry))
                   raw-entries))
         (ordered-entries (e-session-storage--order-queued-records-for-flush entries))
         (write-index-entry (e-session-storage--state-index-write-pending
                             (e-session-storage--state store)))
         (write-index (and write-index-entry
                           (e-session-storage--queued-index-current-p
                            store write-index-entry)))
         (stale-index (and write-index-entry (not write-index)))
         (rebuild-index (and stale-index entries))
         (stale-count (- (length raw-entries) (length entries))))
    (e-session-storage--profile-call
     'session.flush-write-queue
     (list :metadata (list :persistent (and (e-session-storage-persistent-p store) t)
                           :record-count (length entries)
                           :stale-record-count stale-count
                           :stale-index (and stale-index t)
                           :write-index (and (or write-index rebuild-index) t)))
     (lambda ()
       (e-session-storage--drop-stale-queued-write-entries store)
       (e-session-storage--flush-queued-records store ordered-entries)
       (when rebuild-index
         (setf (e-session-storage--state-index-write-pending
                (e-session-storage--state store))
               (e-session-storage--queued-index-entry store)))
       (when (or write-index rebuild-index)
         (e-session-storage--write-index-now store))))
    (setf (e-session-storage--state-write-queue
           (e-session-storage--state store)) nil)
    (when (e-session-storage--state-index-write-pending
           (e-session-storage--state store))
      (setf (e-session-storage--state-index-write-pending
             (e-session-storage--state store)) nil)
      (e-session-storage--adjust-unsettled-writes store -1)))
  store)

(defun e-session-storage--schedule-write-queue (store)
  "Schedule STORE's queued persistent writes."
  (when (and (e-session-storage-persistent-p store)
             (e-session-storage--queued-writes-p store)
             (not (timerp (e-session-storage--state-write-queue-timer
                           (e-session-storage--state store)))))
    (let ((generation
           (cl-incf (e-session-storage--state-write-queue-generation
                     (e-session-storage--state store))))
          (delay (max 0 (or e-session-write-queue-delay 0))))
      (setf (e-session-storage--state-write-queue-timer
             (e-session-storage--state store))
            (run-at-time
             delay nil
             (lambda ()
               (when (and (e-session-storage--state-p
                           (e-session-storage--state store))
                          (= generation
                             (e-session-storage--state-write-queue-generation
                              (e-session-storage--state store))))
                 (e-session-storage-flush-write-queue store))))))))

(defun e-session-storage-commit-mutation (store session-id record)
  "Commit semantic durable RECORD for SESSION-ID in persistent STORE.

The caller supplies one typed session mutation.  JSONL encoding, queueing,
controller submission, and durability bookkeeping stay inside this owner."
  (let ((record (if (e-session-storage-sqlite-p store)
                    record
                  (e-session-codec-record-for-json record))))
    (e-session-storage--profile-call
     'session.append-record
     (list :session-id session-id
           :metadata (list :record-type (plist-get record :type)))
     (lambda ()
       (when (e-session-storage-persistent-p store)
         (e-session-storage--mark-checkpoint-dirty store session-id)
         (if (e-session-storage-sqlite-p store)
             (e-session-storage-sqlite-append store session-id record)
           (if-let* ((controller (e-session-storage--controller store)))
             (e-session-storage--submit-record controller session-id record)
           (if (e-session-storage--queued-writes-p store)
             (progn
               (e-session-storage--schedule-write-queue store)
               (push (e-session-storage--queued-write-entry store session-id record)
                     (e-session-storage--state-write-queue
                      (e-session-storage--state store)))
               (e-session-storage--adjust-unsettled-writes store 1))
             (e-session-storage--append-record-now store session-id record)))))))))

(defun e-session-storage-prepare-mutation (store session-id record)
  "Validate semantic durable RECORD before any STORE state is published.
Persistent controller commands are prepared on a detached request so command
shape, JSON encodability, and byte limits fail before the owning session or
association is made visible.  Direct and queued stores still use their normal
write paths after this pure preflight."
  (let ((record (if (e-session-storage-sqlite-p store)
                    (copy-tree record)
                  (e-session-codec-record-for-json record))))
    (unless (e-session-storage-sqlite-p store)
      (json-encode record))
    (when-let* ((controller (e-session-storage--controller store)))
      (e-session-storage--validate-record controller session-id record))
    record))

(defun e-session-storage-prepare-mutation-batch (store session-id records)
  "Preflight one complete SQLite session-owner mutation batch."
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-session-storage-error
            (list "Mutation batches require SQLite" session-id)))
  (unless records
    (signal 'e-session-storage-error
            (list "Mutation batch is empty" session-id)))
  (e-session-storage-sqlite-prepare-append-batch
   session-id
   (mapcar (lambda (record)
             (e-session-storage-prepare-mutation store session-id record))
           records)))

(defun e-session-storage-commit-mutation-batch (store session-id records)
  "Commit preflighted RECORDS atomically for SQLite SESSION-ID."
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-session-storage-error
            (list "Mutation batches require SQLite" session-id)))
  (e-session-storage--mark-checkpoint-dirty store session-id)
  (e-session-storage-sqlite-append-batch store session-id records))



(defun e-session-storage--write-index (store)
  "Write STORE's persistent session index."
  (e-session-storage--profile-call
   'session.write-index
   (list :metadata (list :persistent (and (e-session-storage-persistent-p store) t)))
   (lambda ()
     (when (e-session-storage-persistent-p store)
       (if (e-session-storage-sqlite-p store)
           (e-session-storage-publish-projections
            store
            (e-session-storage--state-index-projection
             (e-session-storage--state store))
            (e-session-storage--state-checkpoint-projection-operation
             (e-session-storage--state store)))
         (if-let* ((controller (e-session-storage--controller store)))
           (e-session-storage--request-checkpoint controller)
         (if (e-session-storage--queued-writes-p store)
           (progn
             (e-session-storage--schedule-write-queue store)
             (unless (e-session-storage--state-index-write-pending
                      (e-session-storage--state store))
               (e-session-storage--adjust-unsettled-writes store 1))
             (setf (e-session-storage--state-index-write-pending
                    (e-session-storage--state store))
                   (e-session-storage--queued-index-entry store)))
             (e-session-storage--write-index-now store))))))))

(defun e-session-storage-finalize-store (store on-done on-error)
  "Asynchronously finalize STORE's current durability boundary.
STORE must own the production persistence controller.  Call ON-DONE after its
checkpoint is acknowledged, or ON-ERROR when the writer rejects it."
  (if (e-session-storage-sqlite-p store)
      (condition-case err
          (progn
            ;; A primary mutation may already have acknowledged while its
            ;; rebuildable catalog/checkpoint publication failed.  Retry that
            ;; retained projection before the explicit ordered barrier.
            (let* ((state (e-session-storage--state store))
                   (dirty
                    (hash-table-count
                     (e-session-storage--state-checkpoint-dirty-session-ids
                      state))))
              (when (or (> dirty 0)
                        (e-session-storage--state-projection-last-error state))
                (condition-case projection-error
                    (e-session-storage--write-index store)
                  (error
                   (e-session-storage-note-projection-error
                    store projection-error)
                   (signal (car projection-error)
                           (cdr projection-error))))))
            ;; The bounded status read is also SQLite's explicit ordered
            ;; barrier: runtime write priority prevents it from overtaking
            ;; any effect submitted before this finalize call.
            (e-session-storage-sqlite-ordered-barrier store)
            (funcall on-done t))
        (error (funcall on-error err)))
    (if-let* ((controller (e-session-storage--controller store)))
      (e-session-storage-finalize controller on-done on-error)
      (signal 'e-session-persistence-unavailable
              (list "Session store has no asynchronous persistence controller")))))



(defun e-session-storage--append-admission-records-now (store session-id records)
  "Publish prepared admission RECORDS as one direct journal transaction.
The replacement file is renamed only after every record has been encoded and
written, so a direct-store failure cannot publish a partial admission pair."
  (unless records
    (signal 'e-session-storage-error
            (list "Invalid admission journal batch" session-id)))
  (when (e-session-storage-persistent-p store)
    (e-session-storage--ensure-directories store)
    (let* ((file (e-session-storage--session-file store session-id))
           (temporary (make-temp-name (concat file ".admission-"))))
      (unwind-protect
          (with-temp-buffer
            (when (file-readable-p file)
              (insert-file-contents file))
            (goto-char (point-max))
            (dolist (record records)
              (insert (json-encode record) "\n"))
            (write-region (point-min) (point-max) temporary nil 'silent)
            (rename-file temporary file t))
        (when (file-exists-p temporary)
          (delete-file temporary))))))


(defgroup e-session-storage nil
  "Asynchronous persistent session storage."
  :group 'e-session)

(defcustom e-session-storage-node-executable "node"
  "Node executable used by the bundled session writer."
  :type 'string
  :group 'e-session-storage)

(defcustom e-session-storage-checkpoint-delay 2.0
  "Seconds of quiet before requesting resume and catalog checkpoints."
  :type 'number
  :group 'e-session-storage)

(defcustom e-session-storage-retry-delay 1.0
  "Seconds before restarting a failed writer with unacknowledged commands."
  :type 'number
  :group 'e-session-storage)

(defcustom e-session-storage-retry-page-size 32
  "Maximum writer requests resent by one retry callback."
  :type 'integer
  :group 'e-session-storage)

(defcustom e-session-storage-command-byte-limit (* 256 1024)
  "Maximum encoded size of one writer command."
  :type 'integer
  :group 'e-session-storage)

(defcustom e-session-storage-command-node-limit 32768
  "Maximum Lisp value nodes inspected before encoding one writer command.
This remains a structural allocation guard while allowing valid checkpoint
manifests to reach the independent encoded-byte limit."
  :type 'integer
  :group 'e-session-storage)

(cl-defstruct (e-session-storage
               (:constructor e-session-storage--create)
               (:predicate e-session-storage--controller-p)
               (:conc-name e-session-storage--controller-))
  store process stderr-buffer input-fragment
  instance-id (next-sequence 0) (outbox (make-hash-table :test 'equal))
  outbox-head outbox-tail retry-cursor
  (callbacks (make-hash-table :test 'equal))
  checkpoint-timer retry-timer last-error)

(cl-defstruct (e-session-storage-command
               (:constructor e-session-storage-command--create)
               (:predicate e-session-storage--command-p)
               (:conc-name e-session-storage--command-))
  "One validated writer request and its immutable wire representation."
  request wire)

(defconst e-session-storage--library-directory
  (file-name-directory
   (file-truename
    (or load-file-name
        (locate-library "e-session-storage")
        (signal 'e-session-storage-error
                (list "Cannot locate e-session-storage library")))))
  "Directory containing the loaded session persistence library.")

(defun e-session-storage--directory ()
  "Return the directory containing this library."
  e-session-storage--library-directory)

(defun e-session-storage--writer-script ()
  "Return the bundled writer program path."
  (expand-file-name "e-session-writer.mjs" (e-session-storage--directory)))

(defun e-session-storage--command ()
  "Return argv for a persistent session writer."
  (let ((node (executable-find e-session-storage-node-executable)))
    (unless node
      (signal 'e-session-storage-error
              (list (format "Cannot find Node executable %s"
                            e-session-storage-node-executable))))
    (list node (e-session-storage--writer-script))))

(defun e-session-storage--live-p (controller)
  "Return non-nil when CONTROLLER's writer is live."
  (let ((process (e-session-storage--controller-process controller)))
    (and process (process-live-p process))))

(defun e-session-storage--command-within-budget-p (request)
  "Return non-nil when REQUEST fits fixed pre-encoding budgets."
  (let ((pending (list request))
        (nodes 0)
        (string-bytes 0)
        valid)
    (setq valid t)
    (while (and pending valid)
      (let ((value (pop pending)))
        (setq nodes (1+ nodes))
        (when (> nodes e-session-storage-command-node-limit)
          (setq valid nil))
        (cond
         ((stringp value)
          (setq string-bytes (+ string-bytes (string-bytes value)))
          (when (> string-bytes e-session-storage-command-byte-limit)
            (setq valid nil)))
         ((consp value)
          (push (car value) pending)
          (push (cdr value) pending))
         ((vectorp value)
          (dotimes (index (length value))
            (push (aref value index) pending))))))
    valid))

(defun e-session-storage--encode-command (request)
  "Return REQUEST as one bounded newline-terminated writer command."
  (unless (e-session-storage--command-within-budget-p request)
    (signal 'e-session-storage-command-error
            (list "Writer command exceeds pre-encoding budget")))
  (let ((encoded
         (condition-case err
             (concat (json-encode request) "\n")
           (json-error
            (signal 'e-session-storage-command-error
                    (list "Writer command is not JSON-encodable" err))))))
    (when (> (string-bytes encoded)
             e-session-storage-command-byte-limit)
      (signal 'e-session-storage-command-error
              (list "Writer command exceeds byte budget")))
    encoded))

(defun e-session-storage--prepare-command (request)
  "Return a validated immutable writer command for REQUEST."
  (e-session-storage-command--create
   :request request
   :wire (e-session-storage--encode-command request)))

(defun e-session-storage--next-command-id (controller)
  "Return the command id CONTROLLER will assign to its next submission.
Admission preflight uses the same id shape as the eventual outbox command so
the generated instance/sequence suffix cannot turn a successful preflight
into a later command-size rejection.  The caller remains on one synchronous
stack until submission, so no command can consume this sequence in between."
  (format "%s:%d"
          (e-session-storage--controller-instance-id controller)
          (1+ (e-session-storage--controller-next-sequence controller))))

(defun e-session-storage--validate-record (controller session-id record)
  "Validate RECORD for SESSION-ID without adding it to CONTROLLER's outbox.
The returned command is detached and deliberately discarded; this is the
session admission preflight used before a newly created root is published."
  (e-session-storage--prepare-command
   (list :id (e-session-storage--next-command-id controller)
         :directory (e-session-storage--state-directory
                     (e-session-storage--controller-state controller))
         :op "append" :session-id session-id :record record)))

(defun e-session-storage--validate-admission (controller session-id records)
  "Validate an admission RECORDS batch without entering CONTROLLER's outbox.
The batch command is the controller-side atomic publication unit used by a
new session plus its board association."
  (e-session-storage--prepare-command
   (list :id (e-session-storage--next-command-id controller)
         :directory (e-session-storage--state-directory
                     (e-session-storage--controller-state controller))
         :op "append-batch" :session-id session-id
         :records (vconcat records))))

(defun e-session-storage-validate-admission-for-store
    (store session-id records)
  "Validate one semantic admission batch for STORE without publishing it."
  (when-let ((controller (e-session-storage--controller store)))
    (e-session-storage--validate-admission controller session-id records)))

(defun e-session-storage--send (controller command)
  "Send one prepared COMMAND to CONTROLLER's live writer.
Raw request plists remain accepted for controllers created before a live
reload; new submissions always prepare once before entering the outbox."
  (process-send-string (e-session-storage--controller-process controller)
                       (if (e-session-storage--command-p command)
                           (e-session-storage--command-wire command)
                         (e-session-storage--encode-command command))))

(defun e-session-storage--append-outbox-id (controller id)
  "Append ID to CONTROLLER's O(1) retry-order queue."
  (let ((cell (list id)))
    (if-let ((tail (e-session-storage--controller-outbox-tail controller)))
        (setcdr tail cell)
      (setf (e-session-storage--controller-outbox-head controller) cell))
    (setf (e-session-storage--controller-outbox-tail controller) cell)))

(defun e-session-storage--trim-outbox-order (controller)
  "Drop acknowledged ids from the front of CONTROLLER's retry queue."
  (let ((head (e-session-storage--controller-outbox-head controller))
        (outbox (e-session-storage--controller-outbox controller)))
    (while (and head (not (gethash (car head) outbox)))
      (setq head (cdr head)))
    (setf (e-session-storage--controller-outbox-head controller) head)
    (unless head
      (setf (e-session-storage--controller-outbox-tail controller) nil))))

(defun e-session-storage--resend-page (controller)
  "Resend one fixed retry page for CONTROLLER and yield between pages."
  (let ((cursor (e-session-storage--controller-retry-cursor controller))
        (scanned 0))
    (while (and cursor (< scanned e-session-storage-retry-page-size))
      (when-let ((request (gethash (car cursor)
                                  (e-session-storage--controller-outbox controller))))
        (e-session-storage--send controller request))
      (setq cursor (cdr cursor)
            scanned (1+ scanned)))
    (setf (e-session-storage--controller-retry-cursor controller) cursor)
    (when cursor
      (run-at-time 0 nil
                   (lambda ()
                     (when (e-session-storage--live-p controller)
                       (e-session-storage--resend-page controller)))))))

(defun e-session-storage--restart-later (controller)
  "Retry CONTROLLER's writer when it still has work."
  (when (and (> (hash-table-count (e-session-storage--controller-outbox controller)) 0)
             (not (timerp (e-session-storage--controller-retry-timer controller))))
    (setf (e-session-storage--controller-retry-timer controller)
          (run-at-time
           e-session-storage-retry-delay nil
           (lambda ()
             (setf (e-session-storage--controller-retry-timer controller) nil)
             (condition-case err
                 (progn
                   (e-session-storage--ensure controller)
                   (setf (e-session-storage--controller-retry-cursor controller)
                         (e-session-storage--controller-outbox-head controller))
                   (e-session-storage--resend-page controller))
               (error
                (setf (e-session-storage--controller-last-error controller) err)
                (e-session-storage--restart-later controller))))))))

(defun e-session-storage--handle-response (controller response)
  "Apply one writer RESPONSE to CONTROLLER."
  (let* ((id (plist-get response :id))
         (callbacks (and (stringp id)
                         (gethash id (e-session-storage--controller-callbacks controller)))))
    (if (eq (plist-get response :ok) :json-false)
        (let ((err (list 'e-session-storage-error
                         (or (plist-get response :error)
                             "Writer rejected command"))))
          (setf (e-session-storage--controller-last-error controller) err)
          (if (eq (plist-get response :retryable) :json-false)
            (progn
              (when (and (stringp id)
                         (gethash id (e-session-storage--controller-outbox controller)))
                (remhash id (e-session-storage--controller-outbox controller))
                (e-session-storage--trim-outbox-order controller)
                (e-session-storage--adjust-unsettled-writes
                 (e-session-storage--controller-store controller) -1))
                (remhash id (e-session-storage--controller-callbacks controller))
                (if-let ((on-error (cdr callbacks)))
                    (funcall on-error err)
                  (display-warning 'e-session-storage
                                   (error-message-string err)
                                   :error)))
            ;; Older writers omit `retryable'; preserve their retry behavior.
            (e-session-storage--restart-later controller)))
      (when (stringp id)
        (when (gethash id (e-session-storage--controller-outbox controller))
          (remhash id (e-session-storage--controller-outbox controller))
          (e-session-storage--trim-outbox-order controller)
          (e-session-storage--adjust-unsettled-writes
           (e-session-storage--controller-store controller) -1))
        (remhash id (e-session-storage--controller-callbacks controller))
        (when-let ((on-done (car callbacks)))
          (funcall on-done (plist-get response :result)))
        (setf (e-session-storage--controller-last-error controller) nil)))))

(defun e-session-storage--status (controller)
  "Return bounded operational status for persistence CONTROLLER."
  (let ((err (e-session-storage--controller-last-error controller)))
    (list :writer-live (and (e-session-storage--live-p controller) t)
          :outbox-count (hash-table-count
                         (e-session-storage--controller-outbox controller))
          :retry-pending (and (timerp
                               (e-session-storage--controller-retry-timer controller))
                              t)
          :last-error (and err
                           (condition-case nil
                               (error-message-string err)
                             (error (format "%S" err)))))))

(defun e-session-storage--consume-output (controller text)
  "Consume newline-delimited writer protocol TEXT."
  (let ((input (concat (or (e-session-storage--controller-input-fragment controller) "") text)))
    (while (string-match "\n" input)
      (let ((line (substring input 0 (match-beginning 0))))
        (setq input (substring input (match-end 0)))
        (unless (string-empty-p line)
          (condition-case err
              (e-session-storage--handle-response
               controller
               (json-parse-string line :object-type 'plist :array-type 'list
                                  :null-object nil :false-object :json-false))
            (error
             (setf (e-session-storage--controller-last-error controller) err))))))
    (setf (e-session-storage--controller-input-fragment controller) input)))

(defun e-session-storage--ensure (controller)
  "Start and return CONTROLLER's writer process."
  (unless (e-session-storage--live-p controller)
    (let ((stderr (generate-new-buffer " *e-session-writer-stderr*")))
      (setf (e-session-storage--controller-stderr-buffer controller) stderr
            (e-session-storage--controller-process controller)
            (make-process
             :name "e-session-writer"
             :buffer nil :stderr stderr :command (e-session-storage--command)
             :connection-type 'pipe :coding 'utf-8-unix :noquery t
             :filter (lambda (_process text)
                       (e-session-storage--consume-output controller text))
             :sentinel (lambda (_process _event)
                         (unless (e-session-storage--live-p controller)
                           (e-session-storage--restart-later controller)))))
      (set-process-query-on-exit-flag (e-session-storage--controller-process controller) nil)))
  (e-session-storage--controller-process controller))

(defun e-session-storage--send-submitted-command (controller command)
  "Send COMMAND without overtaking work queued before a writer replacement."
  (let ((previous-process (e-session-storage--controller-process controller)))
    (e-session-storage--ensure controller)
    (cond
     ((not (eq previous-process
               (e-session-storage--controller-process controller)))
      ;; A new pipe has no knowledge of the old pipe's unacknowledged writes.
      ;; Replay the complete ordered outbox, including COMMAND at its tail.
      (setf (e-session-storage--controller-retry-cursor controller)
            (e-session-storage--controller-outbox-head controller))
      (e-session-storage--resend-page controller))
     ;; A paged replay already owns every command appended at its tail.
     ((e-session-storage--controller-retry-cursor controller))
     (t
      (e-session-storage--send controller command)))))

(defun e-session-storage--submit
    (controller operation &optional on-done on-error)
  "Queue OPERATION for CONTROLLER and return its stable command id.
ON-DONE and ON-ERROR are the one callback pair for this generic command."
  (let* ((sequence (cl-incf (e-session-storage--controller-next-sequence controller)))
         ;; The writer deduplicates this value after an Emacs restart.  A local
         ;; counter would collide with a prior controller's acknowledged work.
         (id (format "%s:%d" (e-session-storage--controller-instance-id controller)
                     sequence))
         (request (append (list :id id
                                :directory
                                (e-session-storage--state-directory
                                 (e-session-storage--controller-state controller)))
                          operation))
         (command
          (condition-case err
              (e-session-storage--prepare-command request)
            (e-session-storage-command-error
             (setf (e-session-storage--controller-last-error controller) err)
             (when on-error
               (funcall on-error err))
             (signal (car err) (cdr err))))))
    ;; Only transport-ready commands become durable outbox obligations.
    (puthash id command (e-session-storage--controller-outbox controller))
    (e-session-storage--append-outbox-id controller id)
    (when (or on-done on-error)
      (puthash id (cons on-done on-error)
               (e-session-storage--controller-callbacks controller)))
    (e-session-storage--adjust-unsettled-writes
     (e-session-storage--controller-store controller) 1)
    (condition-case err
        (e-session-storage--send-submitted-command controller command)
      (error
       (setf (e-session-storage--controller-last-error controller) err)
       (e-session-storage--restart-later controller)))
    id))

(cl-defun e-session-storage--submit-record
    (controller session-id record &optional on-done on-error)
  "Submit durable RECORD for SESSION-ID through CONTROLLER.

Optional callbacks are attached to the command's existing outbox lifecycle;
they do not introduce a checkpoint or a second write."
  (e-session-storage--submit
   controller (list :op "append" :session-id session-id :record record)
   on-done on-error))

(defun e-session-storage--submit-admission (controller session-id records)
  "Submit one atomic session-admission RECORDS batch to CONTROLLER.
The controller retains one retry identity for the whole root/association
publication rather than exposing separate commands that can be split."
  (e-session-storage--submit
   controller (list :op "append-batch" :session-id session-id
                    :records (vconcat records))))

(cl-defun e-session-storage-publish-admission
    (store session-id records &optional (write-index t))
  "Publish validated admission RECORDS for SESSION-ID through STORE's mode.
The storage owner chooses the direct, queued, or controller path and owns all
queue, checkpoint-dirty, and unsettled-write transitions.  RECORDS must have
already passed the aggregate's semantic validation and storage preflight.
WRITE-INDEX controls the optional derived-index publication.  The application
composition root can defer it until after the aggregate has applied the
semantic admission, while direct callers retain the historical default."
  (cond
   ((e-session-storage-sqlite-p store)
    (e-session-storage-sqlite-append-batch store session-id records)
    (e-session-storage--mark-checkpoint-dirty store session-id)
    (when write-index (e-session-storage--write-index store)))
   ((e-session-storage--controller store)
    (e-session-storage--submit-admission
     (e-session-storage--controller store) session-id records)
    ;; The batch writer owns journal atomicity; the normal checkpoint request
    ;; publishes the derived index only after that batch has reached the
    ;; writer's serial queue.
    (e-session-storage--mark-checkpoint-dirty store session-id)
    (when write-index
      (e-session-storage--write-index store)))
   ((e-session-storage--queued-writes-p store)
    (e-session-storage--mark-checkpoint-dirty store session-id)
    (e-session-storage--schedule-write-queue store)
    (push (e-session-storage--queued-admission-entry store session-id records)
          (e-session-storage--state-write-queue
           (e-session-storage--state store)))
    (e-session-storage--adjust-unsettled-writes store 1)
    (when write-index
      (e-session-storage--write-index store)))
   (t
    (e-session-storage--append-admission-records-now store session-id records)
    (e-session-storage--mark-checkpoint-dirty store session-id)
    (when write-index
      (e-session-storage--write-index store)))))

(defun e-session-storage-abort-session (store session-id)
  "Discard unpublished storage work and files for SESSION-ID.
This rollback boundary is used only for a session reservation that never
completed admission.  It never retracts an acknowledged writer command or
touches another session's pending work."
  (if (e-session-storage-sqlite-p store)
      (e-session-storage-sqlite-delete store session-id)
    (dolist (entry
           (copy-sequence
            (e-session-storage--state-write-queue
             (e-session-storage--state store))))
    (when (equal (e-session-storage--queued-entry-session-id entry) session-id)
      (e-session-storage--drop-queued-write-entry store entry)))
  (remhash session-id
           (e-session-storage--state-checkpoint-dirty-session-ids
            (e-session-storage--state store)))
  (when (e-session-storage-persistent-p store)
    (dolist (file (list (e-session-storage--session-file store session-id)
                        (e-session-storage--checkpoint-file store session-id)))
      (when (file-exists-p file)
        (delete-file file)))
    ;; Commands already acknowledged by the writer cannot be retracted here;
    ;; their session files are removed above and any late acknowledgement
    ;; remains visible to the storage owner rather than being swallowed.
    (when-let ((controller (e-session-storage--controller store)))
      (e-session-storage--discard-retained-admission-commands
       controller session-id))
    ;; The aggregate removes its in-memory session after this physical cleanup.
    ;; It then decides whether the direct catalog must be rewritten; queued
    ;; catalog work remains pending until its normal timer boundary.
      nil))
  t)

(defun e-session-storage--discard-retained-admission-commands
    (controller session-id)
  "Remove retained, not-yet-acknowledged append work for SESSION-ID.
This private admission cleanup boundary only drops commands still owned by
CONTROLLER's local outbox, preserves unrelated sessions, and never pretends to
retract a command already acknowledged by the writer."
  (let (cancelled)
    (maphash
     (lambda (id command)
       (let* ((request (and (e-session-storage--command-p command)
                            (e-session-storage--command-request command)))
              (operation (plist-get request :op)))
         (when (and (member operation '("append" "append-batch"))
                    (equal (plist-get request :session-id) session-id))
           (push id cancelled))))
     (e-session-storage--controller-outbox controller))
    (dolist (id cancelled)
      (remhash id (e-session-storage--controller-outbox controller))
      (remhash id (e-session-storage--controller-callbacks controller))
      (e-session-storage--adjust-unsettled-writes
       (e-session-storage--controller-store controller) -1))
    (e-session-storage--trim-outbox-order controller)
    cancelled))

(defun e-session-storage--checkpoint-operation
    (controller session-id &optional projection-operation)
  "Return one bounded writer checkpoint operation for CONTROLLER SESSION-ID.
PROJECTION-OPERATION, when supplied, is the application-owned projection
captured for the current durability batch."
  (let* ((state (e-session-storage--controller-state controller))
         (projection-operation
          (or projection-operation
              (e-session-storage--state-checkpoint-projection-operation state)))
         ;; The application service owns catalog selection.  Invoke its narrow
         ;; operation at the durability boundary, then detach the result before
         ;; command preparation reaches the physical writer adapter.
         (manifest (copy-tree
                    (or (and projection-operation
                             (funcall projection-operation session-id))
                        (list :session-id session-id :entry-ids [])))))
    (when (plist-member manifest :board-state)
      (plist-put manifest :board-state
                 (e-session-codec-board-association-for-json
                  (plist-get manifest :board-state))))
    (list :op "checkpoint"
          :sessions (vector manifest))))

(defun e-session-storage--reindex-operation ()
  "Return the writer operation used as a checkpoint-batch index barrier."
  (list :op "reindex"))

(defun e-session-storage--submit-checkpoint-batch
    (controller on-done on-error)
  "Submit dirty checkpoints, then one reindex barrier, for CONTROLLER.

Each session manifest travels in its own bounded command.  The writer handles
commands serially, so acknowledging the final reindex means every preceding
checkpoint is durable.  Call ON-DONE after that barrier or ON-ERROR once on the
first terminal failure.  The caller owns one unsettled-write slot spanning the
whole batch; each submitted writer command owns its ordinary outbox slot."
  (let* ((store (e-session-storage--controller-store controller))
         (state (e-session-storage--controller-state controller))
         (projection-operation
          (e-session-storage--state-checkpoint-projection-operation state))
         (session-ids (e-session-storage-checkpoint-dirty-session-ids store))
         (remaining (copy-sequence session-ids))
         (settled nil)
         first-command-id)
    (cl-labels
        ((fail (err)
           (unless settled
             (setq settled t)
             ;; A failed checkpoint or reindex leaves the batch retryable.
             ;; Mutations made after an earlier manifest was submitted already
             ;; re-added their ids; `puthash' safely coalesces both cases.
             (dolist (session-id session-ids)
               (e-session-storage--mark-checkpoint-dirty store session-id))
             (funcall on-error err)))
         (finish (value)
           (unless settled
             (setq settled t)
             ;; Do not retain an aggregate-capturing operation after its batch.
             ;; A mutation during this batch installs a distinct operation for
             ;; the next debounce and must remain pending.
             (when (eq projection-operation
                       (e-session-storage--state-checkpoint-projection-operation
                        state))
               (setf (e-session-storage--state-checkpoint-projection-operation
                      state)
                     nil))
             (funcall on-done value)))
         (submit-operation (operation success)
           (condition-case err
               (let ((command-id
                      (e-session-storage--submit
                       controller operation success #'fail)))
                 (unless first-command-id
                   (setq first-command-id command-id))
                 command-id)
             ;; Preflight invokes FAIL before re-signalling.  The settlement
             ;; guard makes this catch idempotent and keeps timer callbacks from
             ;; leaking their batch-level unsettled slot.
             (error
              (fail err)
              nil)))
         (submit-next (&optional _value)
           (unless settled
             (condition-case err
                 (if-let ((session-id (pop remaining)))
                     (when (submit-operation
                            (e-session-storage--checkpoint-operation
                             controller session-id projection-operation)
                            #'submit-next)
                       ;; Transfer this snapshot out of the dirty set before the
                       ;; event loop can observe its acknowledgement.  A later
                       ;; mutation marks it dirty again; a failure re-marks the
                       ;; whole captured batch in FAIL.
                       (e-session-storage-checkpoint-mark-clean
                        store (list session-id)))
                   (submit-operation
                    (e-session-storage--reindex-operation) #'finish))
               (error (fail err))))))
      (submit-next)
      first-command-id)))

(defun e-session-storage--request-checkpoint (controller)
  "Debounce a derived session-index checkpoint for CONTROLLER."
  (if-let ((timer (e-session-storage--controller-checkpoint-timer controller)))
      (cancel-timer timer)
    (e-session-storage--adjust-unsettled-writes
     (e-session-storage--controller-store controller) 1))
  (setf (e-session-storage--controller-checkpoint-timer controller)
        (run-at-time
         (max 0 e-session-storage-checkpoint-delay) nil
         (lambda ()
           ;; Keep the timer's ownership slot across the full command series so
           ;; acknowledgements cannot expose false quiescent edges between
           ;; checkpoints and the final reindex barrier.
           (setf (e-session-storage--controller-checkpoint-timer controller) nil)
           (e-session-storage--submit-checkpoint-batch
            controller
            (lambda (_value)
              (e-session-storage--adjust-unsettled-writes
               (e-session-storage--controller-store controller) -1))
            (lambda (err)
              (e-session-storage--adjust-unsettled-writes
               (e-session-storage--controller-store controller) -1)
              (display-warning 'e-session-storage
                               (error-message-string err)
                               :error)))))))

(defun e-session-storage-finalize (controller on-done on-error)
  "Asynchronously finalize CONTROLLER's current durability boundary.
Call ON-DONE after the writer acknowledges every dirty session checkpoint and
the final reindex barrier, or ON-ERROR if the writer rejects one.  Return the
first stable command id in that batch."
  (let ((timer (e-session-storage--controller-checkpoint-timer controller)))
    (when timer (cancel-timer timer))
    ;; A scheduled checkpoint already owns one slot.  Otherwise establish the
    ;; batch slot before submitting its first outbox command.
    (unless timer
      (e-session-storage--adjust-unsettled-writes
       (e-session-storage--controller-store controller) 1))
    (setf (e-session-storage--controller-checkpoint-timer controller) nil)
    (e-session-storage--submit-checkpoint-batch
     controller
     (lambda (value)
       (e-session-storage--adjust-unsettled-writes
        (e-session-storage--controller-store controller) -1)
       (funcall on-done value))
     (lambda (err)
       (e-session-storage--adjust-unsettled-writes
        (e-session-storage--controller-store controller) -1)
       (funcall on-error err)))))

(defun e-session-storage-enable (store)
  "Attach and return an asynchronous persistence controller for STORE."
  (let ((state (e-session-storage--state store)))
    (unless (e-session-storage--state-persistent state)
    (signal 'e-session-storage-error (list "Store is not persistent")))
    (if (e-session-storage-sqlite-p store)
        (e-session-storage-runtime-store store)
      (or (e-session-storage--state-controller state)
        (let ((controller
               (e-session-storage--create
                :store state
                :instance-id
                (format "e-session-%x-%x"
                        (truncate (float-time))
                        (random most-positive-fixnum)))))
          (setf (e-session-storage--state-controller state) controller)
          controller)))))

(defun e-session-storage-close (store)
  "Close STORE's opt-in runtime worker; legacy stores need no close."
  (e-session-storage-sqlite-close store))


(provide 'e-session-storage)

;;; e-session-storage.el ends here

;;; e-runtime-store-offline-worker.el --- Explicit offline SQLite operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Runs only in a disposable batch Emacs launched by the operator application
;; service.  This module owns the narrow schema-upgrade operation that cannot
;; run during ordinary runtime startup.  It never loads a harness or domain
;; owner and does not expose arbitrary SQL.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'e-runtime-store-codec)
(require 'e-runtime-store-worker)
(require 'e-runtime-store-session-worker)
(require 'e-session-process-report-projection)
(require 'e-session-query)
(require 'e-runtime-store-ownership)

(define-error 'e-runtime-store-offline-error "Offline runtime-store operation failed")

(defconst e-runtime-store-offline-worker--legacy-session-page-size 128
  "Maximum session identities visited by one bounded migration page.")
(defconst e-runtime-store-offline-worker--legacy-record-page-size 128
  "Maximum v5 journal rows read by one bounded migration page.")
(defconst e-runtime-store-offline-worker--v5-migration-checksum
  "feature92-schema-v5"
  "Logical checksum for recognized v5 provenance rows.")
(defconst e-runtime-store-offline-worker--v6-migration-identity
  "feature92-v5-to-v6-explicit-upgrade"
  "Durable identity installed for the explicit v5-to-v6 migration.")
(defconst e-runtime-store-offline-worker--v6-migration-checksum
  "feature92-schema-v6"
  "Logical checksum for the v5-to-v6 schema boundary.")
(defconst e-runtime-store-offline-worker--v7-migration-identity
  "feature92-v6-to-v7-process-report-projection"
  "Durable identity installed for the explicit v6-to-v7 migration.")
(defconst e-runtime-store-offline-worker--v7-migration-checksum
  "feature92-schema-v7-process-report-projection"
  "Logical checksum for the v7 process-report projection boundary.")
(defconst e-runtime-store-offline-worker--v8-migration-identity
  "feature95-v7-to-v8-normalized-communication"
  "Durable identity installed for the explicit v7-to-v8 normalization.")
(defconst e-runtime-store-offline-worker--v8-migration-checksum
  "feature95-schema-v8-normalized-communication"
  "Logical checksum for the v7-to-v8 normalized communication boundary.")

(defun e-runtime-store-offline-worker--error (message &rest data)
  "Signal a bounded offline error with MESSAGE and DATA."
  (signal 'e-runtime-store-offline-error (cons message data)))

(defun e-runtime-store-offline-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-runtime-store-offline-worker--check (database)
  "Signal unless DATABASE passes a full integrity check."
  (let ((rows (sqlite-select database "PRAGMA integrity_check")))
    (unless (and (= (length rows) 1)
                 (equal (e-runtime-store-offline-worker--column
                         (car rows) 0)
                        "ok"))
      (signal 'e-runtime-store-offline-error
              (list "SQLite integrity check failed" rows)))))

(defun e-runtime-store-offline-worker--version (database)
  "Return DATABASE schema version or signal with migration guidance."
  (unless (car (sqlite-select
                database
                "SELECT 1 FROM sqlite_master WHERE type='table' AND name='store_meta'"))
    (signal 'e-runtime-store-offline-error
            (list "Not an e SQLite runtime store; run legacy migration")))
  (let ((row (car (sqlite-select
                   database
                   "SELECT value FROM store_meta WHERE key='schema_version'"))))
    (unless row
      (signal 'e-runtime-store-offline-error
              (list "Missing SQLite schema version; run legacy migration")))
    (string-to-number
     (e-runtime-store-offline-worker--column row 0))))

(defun e-runtime-store-offline-worker--table-exists-p (database table)
  "Return non-nil when TABLE exists in DATABASE."
  (and (stringp table)
       (car (sqlite-select
             database
             "SELECT 1 FROM sqlite_master WHERE type='table' AND name=?"
             (vector table)))))

(defun e-runtime-store-offline-worker--decode-value
    (text limit kind session-id)
  "Decode bounded base64 TEXT as a durable KIND witness for SESSION-ID."
  (unless (and (stringp text)
               (<= (string-bytes text)
                   (1+ (* 4 (ceiling limit 3)))))
    (e-runtime-store-offline-worker--error
     "Offline witness payload exceeds its bound"
     :kind kind :session-id session-id :limit limit))
  (condition-case err
      (let ((value
             (e-runtime-store-codec-decode (base64-decode-string text))))
        ;; The codec decoder intentionally preserves the exact durable value;
        ;; this second bounded measurement is the migration's witness-side
        ;; guard against a legal codec value that is too large for the retired
        ;; projection's contract.
        (e-runtime-store-codec-measure-bounded value limit)
        value)
    (error
     (e-runtime-store-offline-worker--error
      "Offline witness payload cannot be decoded"
      :kind kind :session-id session-id :cause err))))

(defun e-runtime-store-offline-worker--record-at-position
    (record session-id position)
  "Return detached RECORD with its adapter-supplied POSITION.

The physical v5 payload is not changed.  A copied replay value receives a
stable position only when the old payload did not carry one; a conflicting
embedded value is a corruption witness rather than something migration may
silently repair.  Root records from the v5 writer historically used
`:created-at' in place of `:timestamp', so the replay copy derives that one
canonical timestamp without rewriting durable bytes."
  (unless (and (listp record)
               (stringp (plist-get record :session-id))
               (equal session-id (plist-get record :session-id)))
    (e-runtime-store-offline-worker--error
     "Session journal record identity is invalid"
     :session-id session-id :position position))
  ;; RECORD is a fresh value decoded for this one migration row and is already
  ;; bounded by `e-session-storage-record-byte-limit'.  Do not run the whole
  ;; durable record through the much smaller query-row scalar bounds: message,
  ;; tool, and context payloads are journal facts, not current-row values.
  ;; A shallow spine copy is sufficient because the derivation seam treats the
  ;; record as read-only and detaches each value it actually projects.
  (let ((copy (copy-sequence record)))
    (when (plist-member copy :journal-position)
      (unless (equal (plist-get copy :journal-position) position)
        (e-runtime-store-offline-worker--error
         "Session journal position disagrees with SQLite"
         :session-id session-id :position position
         :record-position (plist-get copy :journal-position))))
    (unless (plist-member copy :journal-position)
      (setq copy (append copy (list :journal-position position))))
    (when (and (equal (plist-get copy :type) "session")
               (not (plist-member copy :timestamp)))
      (let ((created-at (plist-get copy :created-at)))
        (when (stringp created-at)
          (setq copy (append copy (list :timestamp created-at))))))
    copy))

(defun e-runtime-store-offline-worker--decode-record
    (payload session-id position)
  "Decode one bounded v5 journal PAYLOAD for SESSION-ID at POSITION."
  (unless (and (stringp payload)
               (> (string-bytes payload) 0)
               (<= (string-bytes payload)
                   (1+ (* 4 (ceiling e-session-storage-record-byte-limit 3)))))
    (e-runtime-store-offline-worker--error
     "Session journal payload exceeds its bound"
     :session-id session-id :position position))
  (condition-case err
      (e-runtime-store-offline-worker--record-at-position
       (e-runtime-store-codec-decode (base64-decode-string payload))
       session-id position)
    (e-runtime-store-offline-error (signal (car err) (cdr err)))
    (error
     (e-runtime-store-offline-worker--error
      "Session journal payload cannot be decoded"
      :session-id session-id :position position :cause err))))

(defun e-runtime-store-offline-worker--visit-session
    (database table session-id function)
  "Visit v5 TABLE rows for SESSION-ID through FUNCTION, one row at a time.

FUNCTION receives `(STATE POSITION PAYLOAD RECORD)' and returns the next
STATE.  The iterator validates monotonic positions and returns a small summary
containing no retained record collection.  TABLE is one of the two internal
constants used by the upgrader, never caller SQL."
  (unless (member table '("session_records" "session_records_v5"))
    (e-runtime-store-offline-worker--error "Invalid legacy session table" table))
  (let ((after 0)
        (state nil)
        (count 0)
        (bytes 0)
        (last-position 0)
        done)
    (while (not done)
      (let ((rows
             (sqlite-select
              database
              (format "SELECT position,payload FROM %s WHERE session_id=? AND position>? ORDER BY position LIMIT ?"
                      table)
              (vector session-id after
                      (1+ e-runtime-store-offline-worker--legacy-record-page-size)))))
        (if (null rows)
            (setq done t)
          (dolist (row rows)
            (let ((position
                   (e-runtime-store-offline-worker--column row 0))
                  (payload
                   (e-runtime-store-offline-worker--column row 1)))
              (when (or (not (integerp position))
                        (/= position (1+ last-position)))
                (e-runtime-store-offline-worker--error
                 "Session journal positions are not contiguous"
                 :session-id session-id :position position
                 :previous last-position))
              (unless (stringp payload)
                (e-runtime-store-offline-worker--error
                 "Session journal payload is not text"
                 :session-id session-id :position position))
              (setq last-position position
                    after position
                    count (1+ count)
                    bytes (+ bytes (string-bytes payload)))
              (let ((record
                     (e-runtime-store-offline-worker--decode-record
                      payload session-id position)))
                (setq state (funcall function state position payload record)))))
          (when (< (length rows)
                   (1+ e-runtime-store-offline-worker--legacy-record-page-size))
            (setq done t)))))
    (unless (> count 0)
      (e-runtime-store-offline-worker--error
       "Session has no canonical journal records" :session-id session-id))
    (list :state state :record-count count :payload-bytes bytes
          :last-position last-position)))

(defun e-runtime-store-offline-worker--visit-sessions
    (database table function)
  "Visit distinct session IDs in bounded pages, invoking FUNCTION per session.

FUNCTION receives SESSION-ID.  No complete identity list or aggregate mirror
is retained by this walker."
  (unless (member table '("session_records" "session_records_v5"))
    (e-runtime-store-offline-worker--error "Invalid legacy session table" table))
  (let ((after nil)
        (done nil)
        (count 0))
    (while (not done)
      (let* ((rows
              (if after
                  (sqlite-select
                   database
                   (format "SELECT DISTINCT session_id FROM %s WHERE session_id>? ORDER BY session_id LIMIT ?" table)
                   (vector after
                           (1+ e-runtime-store-offline-worker--legacy-session-page-size)))
                (sqlite-select
                 database
                 (format "SELECT DISTINCT session_id FROM %s ORDER BY session_id LIMIT ?" table)
                 (vector (1+ e-runtime-store-offline-worker--legacy-session-page-size)))))
             (processed 0))
        (if (null rows)
            (setq done t)
          (catch 'legacy-session-page-full
            (dolist (row rows)
              (when (>= processed
                        e-runtime-store-offline-worker--legacy-session-page-size)
                (throw 'legacy-session-page-full nil))
              (let ((session-id
                     (e-runtime-store-offline-worker--column row 0)))
                (unless (and (stringp session-id) (> (string-bytes session-id) 0))
                  (e-runtime-store-offline-worker--error
                   "Legacy session identity is invalid" session-id))
                (funcall function session-id)
                (setq after session-id
                      processed (1+ processed)
                      count (1+ count)))))
          (when (< (length rows)
                   (1+ e-runtime-store-offline-worker--legacy-session-page-size))
            (setq done t)))))
    count))

(defun e-runtime-store-offline-worker--fault-mode ()
  "Return the explicit test-only migration fault mode, when requested."
  (let ((mode (getenv "E_RUNTIME_STORE_TEST_MIGRATION_FAULT")))
    (cond
     ((member mode '("1" "before" "before-schema-transaction")) 'before)
     ((member mode '("after" "after-schema-transaction")) 'after)
     ((member mode '("after-v5" "v5")) 'after-v5)
     ((member mode '("after-v6" "v6")) 'after-v6)
     ((member mode '("after-v7-schema" "v7-schema")) 'after-v7-schema)
     ((member mode '("after-v7-populate" "v7-populate")) 'after-v7-populate)
     ((member mode '("after-v7-parity" "v7-parity")) 'after-v7-parity)
     ((member mode '("after-v8-schema" "v8-schema")) 'after-v8-schema)
     ((member mode '("after-v8-populate" "v8-populate")) 'after-v8-populate)
     ((member mode '("after-v8-parity" "v8-parity")) 'after-v8-parity)
     (t nil))))

(defun e-runtime-store-offline-worker--fault (point)
  "Signal the explicit test fault at POINT."
  (when (eq (e-runtime-store-offline-worker--fault-mode) point)
    (e-runtime-store-offline-worker--error
     "Forced migration failure" :stage point)))

(defun e-runtime-store-offline-worker--apply-v5-record (state record)
  "Apply canonical v5 RECORD to query STATE for offline migration.

Current record families use the domain derivation.  A historical family that
the old aggregate ignored is preserved verbatim in the v6 journal and advances
only its physical high-water mark.  This compatibility rule is deliberately
private to the offline v5 boundary; ordinary v6 writes retain the closed
current command grammar."
  (let ((type (e-session-query--record-type record)))
    (cond
     ((equal type "session")
      ;; The retired replay path treated a later root as replacement state.
      ;; Preserve that observable compatibility without weakening the current
      ;; v6 command grammar, where a second root remains invalid.
      (if state
          (e-session-query--new-state record)
        (e-session-query-state-apply-record state record)))
     ;; The retired replay path ignored every non-root record until a root had
     ;; established aggregate state.  Keep the durable row, but do not invent
     ;; a session query row for a rootless historical journal.
     ((null state) nil)
     ((member type e-session-query-supported-record-types)
      (e-session-query-state-apply-record state record))
     (t
      (unless (and (e-session-query--record-shape-p record)
                   (stringp type)
                   (> (string-bytes type) 0)
                   (stringp (plist-get record :session-id)))
        (e-runtime-store-offline-worker--error
         "Historical session record is malformed"
         :type type :session-id (plist-get record :session-id)))
      (cond
       ((null state)
        (e-runtime-store-offline-worker--error
         "Historical session record precedes its root"
         :type type :session-id (plist-get record :session-id)))
       ((or (plist-get state :deleted) (plist-get state :noop)) state)
       (t
        (unless (equal (plist-get state :session-id)
                       (plist-get record :session-id))
          (e-runtime-store-offline-worker--error
           "Historical session record identity does not match query state"
           :type type :session-id (plist-get record :session-id)))
        (let ((next (e-session-query--copy-value state)))
          (plist-put next :journal-position
                     (e-session-query--record-position
                      record (plist-get state :journal-position)))
          (e-session-query-state-validate next))))))))

(defun e-runtime-store-offline-worker--verify-v5-session
    (database session-id)
  "Derive and validate one v5 SESSION-ID from its canonical journal."
  (let* ((summary
          (e-runtime-store-offline-worker--visit-session
           database "session_records" session-id
           (lambda (state _position _payload record)
             (e-runtime-store-offline-worker--apply-v5-record state record))))
         (state (plist-get summary :state)))
    (when (and state (not (plist-get state :deleted)))
      (e-session-query-state-validate state))
    (list :session-id session-id :state state
          :record-count (plist-get summary :record-count)
          :payload-bytes (plist-get summary :payload-bytes)
          :last-position (plist-get summary :last-position))))

(defun e-runtime-store-offline-worker--preflight-v5 (database)
  "Replay and validate every canonical v5 session in bounded order."
  (let ((sessions 0)
        (records 0)
        (bytes 0))
    (e-runtime-store-offline-worker--visit-sessions
     database "session_records"
     (lambda (session-id)
       (let ((summary
              (e-runtime-store-offline-worker--verify-v5-session
               database session-id)))
         (setq sessions (1+ sessions)
               records (+ records (plist-get summary :record-count))
               bytes (+ bytes (plist-get summary :payload-bytes))))))
    (list :sessions sessions :records records :payload-bytes bytes)))

(defun e-runtime-store-offline-worker--copy-v5-session
    (database session-id)
  "Copy and derive one renamed v5 SESSION-ID into the v6 relations.

The old payload string is inserted verbatim.  Typed columns come only from
the detached decoded record and the complete query row comes only from the
session-domain replay; neither retired projection is consulted here."
  (let* ((summary
          (e-runtime-store-offline-worker--visit-session
           database "session_records_v5" session-id
           (lambda (state position payload record)
             (let* ((columns
                    (e-runtime-store-session-worker--record-columns record))
                    (next-state
                     (e-runtime-store-offline-worker--apply-v5-record
                      state record)))
               (sqlite-execute
                database
                "INSERT INTO session_records(session_id,position,payload,record_type,record_id,record_identity,parent_id,timestamp) VALUES(?,?,?,?,?,?,?,?)"
                (vector session-id position payload
                        (nth 0 columns) (nth 1 columns) (nth 2 columns)
                        (nth 3 columns) (nth 4 columns)))
               ;; A per-row digest comparison proves byte identity without
               ;; retaining a complete session or store payload in Emacs.
               (let ((copied
                      (car (sqlite-select
                            database
                            "SELECT payload FROM session_records WHERE session_id=? AND position=?"
                            (vector session-id position)))))
                 (unless (and copied
                              (equal payload
                                     (e-runtime-store-offline-worker--column
                                      copied 0))
                              (equal (secure-hash 'sha256 payload)
                                     (secure-hash
                                      'sha256
                                      (e-runtime-store-offline-worker--column
                                       copied 0))))
                   (e-runtime-store-offline-worker--error
                    "Migrated journal payload failed identity verification"
                    :session-id session-id :position position)))
               next-state))))
         (state (plist-get summary :state)))
    (unless (or (null state)
                (and (listp state) (plist-get state :deleted))
                (and (listp state) (not (plist-get state :noop))))
      (e-runtime-store-offline-worker--error
       "Migrated session did not produce a complete query state"
       :session-id session-id))
    (when (and state (not (plist-get state :deleted)))
      (sqlite-execute
       database
       (concat
        "INSERT INTO session_query_state(session_id,name,summary,metadata,created_at,updated_at,last_message_at,latest_assistant_marker,message_count,current_branch,turn_options,current_head_id,root_event_id,current_context_generation_id,root_p,board_output_sequence,board_activity_sequence,journal_position) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
       (e-runtime-store-session-worker--state-values state)))
    summary))

(defun e-runtime-store-offline-worker--copy-v5-sessions (database)
  "Copy all renamed v5 sessions through bounded replay and return counts."
  (let ((sessions 0)
        (records 0)
        (bytes 0))
    (e-runtime-store-offline-worker--visit-sessions
     database "session_records_v5"
     (lambda (session-id)
       (let ((summary
              (e-runtime-store-offline-worker--copy-v5-session
               database session-id)))
         (setq sessions (1+ sessions)
               records (+ records (plist-get summary :record-count))
               bytes (+ bytes (plist-get summary :payload-bytes))))))
    (list :sessions sessions :records records :payload-bytes bytes)))

(defun e-runtime-store-offline-worker--parity-check
    (database source-table destination-table)
  "Verify row count and exact payload byte count between two tables."
  (let* ((source
          (car (sqlite-select
                database
                (format "SELECT COUNT(*),COALESCE(SUM(LENGTH(payload)),0) FROM %s"
                        source-table))))
         (destination
          (car (sqlite-select
                database
                (format "SELECT COUNT(*),COALESCE(SUM(LENGTH(payload)),0) FROM %s"
                        destination-table)))))
    (unless (and (= (e-runtime-store-offline-worker--column source 0)
                    (e-runtime-store-offline-worker--column destination 0))
                 (= (e-runtime-store-offline-worker--column source 1)
                    (e-runtime-store-offline-worker--column destination 1)))
      (e-runtime-store-offline-worker--error
       "Migrated journal row parity failed"
       :source source :destination destination))
    (list :records (e-runtime-store-offline-worker--column source 0)
          :payload-bytes (e-runtime-store-offline-worker--column source 1))))

(defun e-runtime-store-offline-worker--install-v6 (database)
  "Install v6 generic and domain relations on a renamed v5 DATABASE."
  ;; The rename is inside the caller's BEGIN IMMEDIATE transaction.  No
  ;; current relation can observe the temporary name, and the old canonical
  ;; payload remains available until derivation, parity, and witness checks
  ;; have all completed.
  ;; A downgraded test image or an interrupted earlier install may still carry
  ;; a stale derived row/table.  Query state is rebuildable, and its indexes
  ;; must not shadow the freshly created v6 relation after the canonical table
  ;; is renamed below.
  (sqlite-execute database "DROP TABLE IF EXISTS session_query_state")
  (sqlite-execute database
                  "ALTER TABLE session_records RENAME TO session_records_v5")
  (dolist (index '("session_records_identity"
                   "session_records_session_page"
                   "session_records_type_page"
                   "session_records_id_page"
                   "session_records_record_identity_page"
                   "session_records_parent_page"
                   "session_records_position"))
    (sqlite-execute database (format "DROP INDEX IF EXISTS %s" index)))
  (e-runtime-store-worker--initialize-common-schema database)
  (e-runtime-store-worker--initialize-domain-schema database 6))

(defun e-runtime-store-offline-worker--ensure-migration-row
    (database version identity checksum)
  "Validate or insert VERSION migration IDENTITY with logical CHECKSUM."
  (let* ((digest (secure-hash 'sha256 checksum))
         (row (car (sqlite-select
                    database
                    "SELECT identity,checksum FROM schema_migrations WHERE version=?"
                    (vector version)))))
    (when (and row
               (or (not (equal (e-runtime-store-offline-worker--column row 0)
                               identity))
                   (not (equal (e-runtime-store-offline-worker--column row 1)
                               digest))))
      (e-runtime-store-offline-worker--error
       "Existing migration identity is malformed" :version version))
    (unless row
      (sqlite-execute
       database
       "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(?,?,?,?)"
       (vector version identity digest (float-time))))))

(defun e-runtime-store-offline-worker--migration-row (database version)
  "Return DATABASE migration VERSION's identity/checksum pair, or nil."
  (let ((row (car (sqlite-select
                   database
                   "SELECT identity,checksum FROM schema_migrations WHERE version=?"
                   (vector version)))))
    (and row
         (list (e-runtime-store-offline-worker--column row 0)
               (e-runtime-store-offline-worker--column row 1)))))

(defun e-runtime-store-offline-worker--recognized-v5-row-p (row)
  "Return non-nil when ROW is normalized v5 source provenance."
  (and
   (member (car row)
           '("feature92-direct-v5-source"
             "feature92-v4-to-v5-explicit-upgrade"))
   (equal (cadr row)
          (secure-hash
           'sha256 e-runtime-store-offline-worker--v5-migration-checksum))))

(defun e-runtime-store-offline-worker--historical-fresh-v5-row-p (row)
  "Return non-nil when ROW is the exact historical fresh-v5 identity."
  (equal
   row
   (list "new-current-schema"
         (secure-hash
          'sha256 e-runtime-store-offline-worker--v5-migration-checksum))))

(defun e-runtime-store-offline-worker--ensure-v5-provenance (database)
  "Install or normalize direct-v5 provenance inside the upgrade transaction."
  (let ((row (e-runtime-store-offline-worker--migration-row database 5)))
    (cond
     ((null row)
      (e-runtime-store-offline-worker--ensure-migration-row
       database 5 "feature92-direct-v5-source"
       e-runtime-store-offline-worker--v5-migration-checksum))
     ((e-runtime-store-offline-worker--historical-fresh-v5-row-p row)
      (sqlite-execute
       database
       "UPDATE schema_migrations SET identity='feature92-direct-v5-source' WHERE version=5"))
     ((e-runtime-store-offline-worker--recognized-v5-row-p row) t)
     (t
      (e-runtime-store-offline-worker--error
       "Source v5 migration provenance is invalid")))))

(defun e-runtime-store-offline-worker--verify-v6-lineage (database)
  "Verify DATABASE has one recognized v6 predecessor lineage."
  (let* ((v5 (e-runtime-store-offline-worker--migration-row database 5))
         (v6 (e-runtime-store-offline-worker--migration-row database 6))
         (v6-checksum
          (secure-hash
           'sha256 e-runtime-store-offline-worker--v6-migration-checksum)))
    (cond
     ((equal v6 (list "new-current-schema" v6-checksum))
      (when v5
        (e-runtime-store-offline-worker--error
         "Fresh v6 source has unexpected v5 provenance")))
     ((equal v6
             (list e-runtime-store-offline-worker--v6-migration-identity
                   v6-checksum))
      (unless (or
               (e-runtime-store-offline-worker--recognized-v5-row-p v5)
               (e-runtime-store-offline-worker--historical-fresh-v5-row-p v5))
        (e-runtime-store-offline-worker--error
         "Migrated v6 source has invalid v5 provenance")))
     (t
      (e-runtime-store-offline-worker--error
       "Source v6 migration lineage is invalid"))))
  t)

(defun e-runtime-store-offline-worker--verify-v7-lineage (database)
  "Verify DATABASE's complete recognized v7 predecessor lineage.

The runtime worker intentionally exposes only the current v8 lineage check;
the stopped-store operator owns this pre-install v7 boundary and therefore
checks its migration rows directly without adding a runtime compatibility
helper."
  (let* ((v5 (e-runtime-store-offline-worker--migration-row database 5))
         (v6 (e-runtime-store-offline-worker--migration-row database 6))
         (v7 (e-runtime-store-offline-worker--migration-row database 7))
         (v6-checksum
          (secure-hash 'sha256
                       e-runtime-store-offline-worker--v6-migration-checksum))
         (v7-checksum
          (secure-hash 'sha256
                       e-runtime-store-offline-worker--v7-migration-checksum)))
    (unless (equal v7
                   (list e-runtime-store-offline-worker--v7-migration-identity
                         v7-checksum))
      (e-runtime-store-offline-worker--error
       "Schema v7 migration lineage is invalid" :version 7))
    (cond
     ((equal v6 (list "new-current-schema" v6-checksum))
      (when v5
        (e-runtime-store-offline-worker--error
         "Fresh v6 predecessor has unexpected v5 provenance" :version 5)))
     ((equal v6
            (list e-runtime-store-offline-worker--v6-migration-identity
                  v6-checksum))
      (unless (or
               (e-runtime-store-offline-worker--recognized-v5-row-p v5)
               (e-runtime-store-offline-worker--historical-fresh-v5-row-p v5))
        (e-runtime-store-offline-worker--error
         "Migrated v6 predecessor has invalid v5 provenance" :version 5)))
     (t
      (e-runtime-store-offline-worker--error
       "Schema v7 predecessor lineage is invalid" :version 6))))
  t)

(defun e-runtime-store-offline-worker--verify-v8-lineage (database)
  "Verify DATABASE's complete v8 lineage using the runtime contract."
  (let ((e-runtime-store-worker--database database))
    (condition-case err
        (e-runtime-store-worker--verify-v8-lineage)
      (e-runtime-store-schema-too-old
       (e-runtime-store-offline-worker--error
        "Schema v8 migration lineage is invalid"
        :reason (plist-get (cdr err) :reason))))))

(defun e-runtime-store-offline-worker--install-v7 (database)
  "Install only the schema-v7 additions on DATABASE."
  ;; This column existed in the final v6 definition, but older v6 stores may
  ;; lack it.  Only the explicit stopped-store operator may repair that
  ;; physical boundary; ordinary startup checks the version before DDL.
  (unless (seq-some
           (lambda (row)
             (equal (e-runtime-store-offline-worker--column row 1)
                    "current_context_generation_id"))
           (sqlite-select database "PRAGMA table_info(session_query_state)"))
    (sqlite-execute
     database
     "ALTER TABLE session_query_state ADD COLUMN current_context_generation_id TEXT"))
  ;; The projection is derived in full below.  Recreate it so an interrupted
  ;; operator attempt or a deliberately downgraded fixture cannot retain a
  ;; foreign key that SQLite rewrote when the v5 journal was renamed.
  (sqlite-execute database "DROP TABLE IF EXISTS session_process_report_index")
  (e-runtime-store-session-worker-initialize-process-report-projection database))

(defun e-runtime-store-offline-worker--rename-table (database table)
  "Rename TABLE to its v7 preservation name when it exists."
  (when (e-runtime-store-offline-worker--table-exists-p database table)
    (sqlite-execute database
                    (format "ALTER TABLE %s RENAME TO %s_v7" table table))))

(defun e-runtime-store-offline-worker--decode-text (text)
  "Decode one legacy SQLite payload TEXT."
  (and text (e-runtime-store-codec-decode (base64-decode-string text))))

(defun e-runtime-store-offline-worker--v7-conflict
    (message &rest data)
  "Reject one contradictory v7 duplicate with bounded DATA."
  (apply #'e-runtime-store-offline-worker--error
         (cons message data)))

(defun e-runtime-store-offline-worker--v7-decode
    (text kind &rest data)
  "Decode one v7 KIND payload and identify it with DATA."
  (condition-case nil
      (let ((value (e-runtime-store-offline-worker--decode-text text)))
        (unless (listp value)
          (apply #'e-runtime-store-offline-worker--v7-conflict
                 (append (list (format "Malformed v7 %s payload" kind))
                         data)))
        value)
    (error
     (apply #'e-runtime-store-offline-worker--v7-conflict
            (append (list (format "Malformed v7 %s payload" kind))
                    data)))))

(defun e-runtime-store-offline-worker--v7-value
    (text kind &rest data)
  "Decode one scalar v7 KIND value and identify it with DATA."
  (condition-case _err
      (e-runtime-store-offline-worker--decode-text text)
    (error
     (apply #'e-runtime-store-offline-worker--v7-conflict
            (append (list (format "Malformed v7 %s value" kind))
                    data)))))

(defun e-runtime-store-offline-worker--v7-name (value)
  "Return VALUE's comparable symbol/string name, or nil."
  (cond ((symbolp value) (symbol-name value))
        ((stringp value) value)
        (t nil)))

(defun e-runtime-store-offline-worker--v7-assert-equal
    (message expected actual &rest data)
  "Signal MESSAGE when v7 EXPECTED and ACTUAL facts disagree."
  (unless (equal expected actual)
    (apply #'e-runtime-store-offline-worker--v7-conflict
           (append (list message :expected expected :actual actual) data))))

(defun e-runtime-store-offline-worker--v7-sorted-values (values)
  "Return VALUES in a deterministic comparison order."
  (sort (mapcar (lambda (value) (e-runtime-store-codec-encode value))
                (copy-tree values t))
        #'string<))

(defun e-runtime-store-offline-worker--v7-record-kind
    (record &rest data)
  "Return RECORD's one agreed kind, accepting one retired alias."
  (let ((canonical (and (plist-member record :record-kind)
                        (e-runtime-store-offline-worker--v7-name
                         (plist-get record :record-kind))))
        (legacy (and (plist-member record :kind)
                     (e-runtime-store-offline-worker--v7-name
                      (plist-get record :kind)))))
    (when (and canonical legacy)
      (apply #'e-runtime-store-offline-worker--v7-assert-equal
             (append (list "v7 record kind aliases disagree"
                           canonical legacy)
                     data)))
    (or canonical legacy)))

(defun e-runtime-store-offline-worker--v7-record-position
    (record &rest data)
  "Return RECORD's one agreed position, accepting one retired alias."
  (let ((canonical (and (plist-member record :seq)
                        (plist-get record :seq)))
        (legacy (and (plist-member record :durable-position)
                     (plist-get record :durable-position))))
    (when (and canonical legacy)
      (apply #'e-runtime-store-offline-worker--v7-assert-equal
             (append (list "v7 record position aliases disagree"
                           canonical legacy)
                     data)))
    (or canonical legacy)))

(defun e-runtime-store-offline-worker--v7-record-tags
    (record &rest data)
  "Return RECORD's one agreed tag list, accepting one retired alias."
  (let ((canonical (and (plist-member record :tags)
                        (plist-get record :tags)))
        (legacy (and (plist-member record :selector-tags)
                     (plist-get record :selector-tags))))
    (when (and canonical legacy)
      (apply #'e-runtime-store-offline-worker--v7-assert-equal
             (append
              (list "v7 record tag aliases disagree"
                    (e-runtime-store-offline-worker--v7-sorted-values canonical)
                    (e-runtime-store-offline-worker--v7-sorted-values legacy))
              data)))
    (or canonical legacy)))

(defun e-runtime-store-offline-worker--v7-record-attributes
    (record &rest data)
  "Return RECORD's one agreed attribute list, accepting one retired alias."
  (let ((canonical (and (plist-member record :attributes)
                        (plist-get record :attributes)))
        (legacy (and (plist-member record :selector-attributes)
                     (plist-get record :selector-attributes))))
    (when (and canonical legacy)
      (apply #'e-runtime-store-offline-worker--v7-assert-equal
             (append
              (list "v7 record attribute aliases disagree"
                    (e-runtime-store-offline-worker--v7-sorted-values
                     (e-runtime-store-offline-worker--v7-attribute-pairs
                      canonical))
                    (e-runtime-store-offline-worker--v7-sorted-values
                     (e-runtime-store-offline-worker--v7-attribute-pairs
                      legacy)))
              data)))
    (or canonical legacy)))

(defun e-runtime-store-offline-worker--v7-attribute-pairs (values)
  "Return alternating attribute VALUES as comparable key/value pairs."
  (let (pairs)
    (while values
      (push (list (car values) (cadr values)) pairs)
      (setq values (cddr values)))
    (nreverse pairs)))

(defun e-runtime-store-offline-worker--copy-v7-session-state (database)
  "Copy v7 query rows after association fields leave the session relation."
  (dolist (row (sqlite-select
                database
                (concat "SELECT session_id,name,summary,metadata,created_at,updated_at,"
                        "last_message_at,latest_assistant_marker,message_count,"
                        "current_branch,turn_options,current_head_id,root_event_id,"
                        "current_context_generation_id,root_p,board_output_sequence,"
                        "board_activity_sequence,journal_position,board_id,principal,"
                        "association_role,routing_policy FROM session_query_state_v7")))
    (sqlite-execute
     database
     "INSERT INTO session_query_state(session_id,name,summary,metadata,created_at,updated_at,last_message_at,latest_assistant_marker,message_count,current_branch,turn_options,current_head_id,root_event_id,current_context_generation_id,root_p,board_output_sequence,board_activity_sequence,journal_position) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
     (apply #'vector (cl-subseq (append row nil) 0 18)))))

(defun e-runtime-store-offline-worker--copy-v7-participants (database)
  "Normalize v7 participant payloads into relational participant columns."
  (let ((e-board-sqlite-worker--database database))
    (dolist (row (sqlite-select database
                                "SELECT board_id,generation,participant_id,payload,revision FROM board_participants_v7"))
      (let* ((board-id (e-runtime-store-offline-worker--column row 0))
             (generation (e-runtime-store-offline-worker--column row 1))
             (participant-id (e-runtime-store-offline-worker--column row 2))
             (participant (e-runtime-store-offline-worker--v7-decode
                           (e-runtime-store-offline-worker--column row 3)
                           "participant"
                           :board-id board-id :generation generation
                           :participant-id participant-id))
             (content (e-board-sqlite-worker--participant-content participant)))
        (when (plist-member participant :id)
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 participant identity disagrees"
           participant-id (plist-get participant :id)
           :board-id board-id :generation generation
           :participant-id participant-id))
        (sqlite-execute
         database
         "INSERT INTO board_participants(board_id,generation,participant_id,principal,author,controller,role,state,name,subscription_id,publication_pending,payload,revision) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)"
         (vector board-id generation participant-id
                 (plist-get participant :principal)
                 (plist-get participant :author)
                 (plist-get participant :controller)
                 (symbol-name (or (plist-get participant :role) 'participant))
                 (symbol-name (or (plist-get participant :state) 'active))
                 (plist-get participant :name)
                 (plist-get participant :subscription-id)
                 (if (plist-get participant :publication-pending) 1 0)
                 (e-board-sqlite-worker--sql-value content)
                 (e-runtime-store-offline-worker--column row 4)))))))

(defun e-runtime-store-offline-worker--copy-v7-board (database)
  "Normalize v7 Board records, selectors, routing, and pickups."
  (let ((e-board-sqlite-worker--database database))
    (dolist (row (sqlite-select
                  database
                  "SELECT board_id,trusted_principal,generation,revision,next_position,root_payload FROM boards_v7"))
      (sqlite-execute
       database
       "INSERT INTO boards(board_id,trusted_principal,generation,revision,next_position,root_payload) VALUES(?,?,?,?,?,?)"
       (apply #'vector (append row nil))))
    (e-runtime-store-offline-worker--copy-v7-participants database)
    (dolist (row (sqlite-select database
                                "SELECT board_id,generation,position,record_kind,record_id,source_kind,source_key,source_hash,payload FROM board_records_v7"))
      (let* ((board-id (e-runtime-store-offline-worker--column row 0))
             (generation (e-runtime-store-offline-worker--column row 1))
             (position (e-runtime-store-offline-worker--column row 2))
             (record-id (e-runtime-store-offline-worker--column row 4))
             (record (e-runtime-store-offline-worker--v7-decode
                      (e-runtime-store-offline-worker--column row 8)
                      "record" :board-id board-id :generation generation
                      :position position :record-id record-id))
             (record-kind
              (e-runtime-store-offline-worker--v7-record-kind
               record :board-id board-id :generation generation
               :position position :record-id record-id))
             (record-position
              (e-runtime-store-offline-worker--v7-record-position
               record :board-id board-id :generation generation
               :position position :record-id record-id))
             (record-tags
              (e-runtime-store-offline-worker--v7-record-tags
               record :board-id board-id :generation generation
               :position position :record-id record-id))
             (record-attributes
              (e-runtime-store-offline-worker--v7-record-attributes
               record :board-id board-id :generation generation
               :position position :record-id record-id))
             (tag-rows
              (sqlite-select
               database
               "SELECT tag FROM board_record_tags_v7 WHERE board_id=? AND generation=? AND position=? ORDER BY rowid"
               (vector board-id generation position)))
             (attribute-rows
              (sqlite-select
               database
               "SELECT attribute_key,attribute_value FROM board_record_attributes_v7 WHERE board_id=? AND generation=? AND position=? ORDER BY rowid"
               (vector board-id generation position)))
             (stored-tags
              (mapcar (lambda (tag-row)
                        (e-runtime-store-offline-worker--v7-value
                         (e-runtime-store-offline-worker--column tag-row 0)
                         "record tag" :board-id board-id :position position))
                      tag-rows))
             (stored-attributes
              (let (values)
                (dolist (attribute-row attribute-rows)
                  (setq values
                        (append values
                                (list
                                 (e-runtime-store-offline-worker--v7-value
                                  (e-runtime-store-offline-worker--column
                                   attribute-row 0)
                                  "record attribute key"
                                  :board-id board-id :position position)
                                 (e-runtime-store-offline-worker--v7-value
                                  (e-runtime-store-offline-worker--column
                                   attribute-row 1)
                                  "record attribute value"
                                  :board-id board-id :position position)))))
                values))
             (content (e-board-sqlite-worker--record-content record)))
        (e-runtime-store-offline-worker--v7-assert-equal
         "v7 record key disagrees" record-id (plist-get record :id)
         :board-id board-id :generation generation :position position)
        (e-runtime-store-offline-worker--v7-assert-equal
         "v7 record kind disagrees"
         (e-runtime-store-offline-worker--column row 3) record-kind
         :board-id board-id :generation generation :position position)
        (when record-position
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 record position disagrees" position record-position
           :board-id board-id :generation generation :record-id record-id))
        (when record-tags
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 record tags disagree"
           (e-runtime-store-offline-worker--v7-sorted-values stored-tags)
           (e-runtime-store-offline-worker--v7-sorted-values record-tags)
           :board-id board-id :generation generation :position position))
        (when record-attributes
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 record attributes disagree"
           (e-runtime-store-offline-worker--v7-sorted-values
            (e-runtime-store-offline-worker--v7-attribute-pairs
             stored-attributes))
           (e-runtime-store-offline-worker--v7-sorted-values
            (e-runtime-store-offline-worker--v7-attribute-pairs
             record-attributes))
           :board-id board-id :generation generation :position position))
        (let ((source (plist-get record :source)))
          (when source
            (when (plist-get source :kind)
              (e-runtime-store-offline-worker--v7-assert-equal
               "v7 record source kind disagrees"
               (symbol-name (plist-get source :kind))
               (e-runtime-store-offline-worker--column row 5)
               :board-id board-id :position position))
            (when (plist-member source :key)
              (e-runtime-store-offline-worker--v7-assert-equal
               "v7 record source key disagrees"
               (e-runtime-store-offline-worker--decode-text
                (e-runtime-store-offline-worker--column row 6))
               (plist-get source :key)
               :board-id board-id :position position))))
        (unless (plist-get record :created-at)
          (e-runtime-store-offline-worker--v7-conflict
           "v7 record has no creation time"
           :board-id board-id :generation generation :position position
           :record-id record-id))
        (sqlite-execute
         database
         "INSERT INTO board_records(board_id,generation,position,record_kind,record_id,source_kind,source_key,source_hash,created_at,author,subject_participant_id,payload) VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"
         (vector board-id generation position
                 (e-runtime-store-offline-worker--column row 3)
                 record-id
                 (e-runtime-store-offline-worker--column row 5)
                 (e-runtime-store-offline-worker--column row 6)
                 (e-runtime-store-offline-worker--column row 7)
                 (plist-get record :created-at)
                 (e-board-sqlite-worker--sql-value
                  (plist-get record :author))
                 (plist-get record :subject-participant-id)
                 (e-board-sqlite-worker--sql-value content)))))
    (dolist (row (sqlite-select database
                                "SELECT board_id,generation,position,tag FROM board_record_tags_v7"))
      (sqlite-execute database
                      "INSERT INTO board_record_tags(board_id,generation,position,tag) VALUES(?,?,?,?)"
                      (apply #'vector (append row nil))))
    (dolist (row (sqlite-select database
                                "SELECT board_id,generation,position,attribute_key,attribute_value FROM board_record_attributes_v7"))
      (sqlite-execute database
                      "INSERT INTO board_record_attributes(board_id,generation,position,attribute_key,attribute_value) VALUES(?,?,?,?,?)"
                      (apply #'vector (append row nil))))
    (dolist (row (sqlite-select database
                                "SELECT board_id,generation,message_id,outcome,payload,revision FROM board_routing_v7"))
      (let* ((board-id (e-runtime-store-offline-worker--column row 0))
             (generation (e-runtime-store-offline-worker--column row 1))
             (message-id (e-runtime-store-offline-worker--column row 2))
             (outcome (e-runtime-store-offline-worker--v7-decode
                       (e-runtime-store-offline-worker--column row 4)
                       "routing" :board-id board-id :generation generation
                       :message-id message-id))
             (pickup-rows
              (sqlite-select
               database
               "SELECT delivery_key,participant_id FROM board_pickups_v7 WHERE board_id=? AND generation=? AND message_id=? ORDER BY participant_id,fifo_position"
               (vector board-id generation message-id)))
             (participant-ids
              (delete-dups
               (mapcar (lambda (pickup-row)
                         (e-runtime-store-offline-worker--column pickup-row 1))
                       pickup-rows)))
             (pickup-ids
              (mapcar (lambda (pickup-row)
                        (e-runtime-store-offline-worker--v7-value
                         (e-runtime-store-offline-worker--column pickup-row 0)
                         "pickup identity" :board-id board-id
                         :generation generation :message-id message-id))
                      pickup-rows)))
        (when (plist-member outcome :state)
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 routing state disagrees"
           (e-runtime-store-offline-worker--column row 3)
           (e-runtime-store-offline-worker--v7-name
            (plist-get outcome :state))
           :board-id board-id :generation generation :message-id message-id))
        (when (plist-member outcome :participant-ids)
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 routing participants disagree"
           (e-runtime-store-offline-worker--v7-sorted-values participant-ids)
           (e-runtime-store-offline-worker--v7-sorted-values
            (plist-get outcome :participant-ids))
           :board-id board-id :generation generation :message-id message-id))
        (when (plist-member outcome :pickup-ids)
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 routing pickups disagree"
           (e-runtime-store-offline-worker--v7-sorted-values pickup-ids)
           (e-runtime-store-offline-worker--v7-sorted-values
            (plist-get outcome :pickup-ids))
           :board-id board-id :generation generation :message-id message-id))
        (sqlite-execute
         database
         "INSERT INTO board_routing(board_id,generation,message_id,outcome,reason,revision,payload) VALUES(?,?,?,?,?,?,?)"
         (vector board-id generation message-id
                 (e-runtime-store-offline-worker--column row 3)
                 (and (plist-get outcome :reason)
                      (e-runtime-store-offline-worker--v7-name
                       (plist-get outcome :reason)))
                 (e-runtime-store-offline-worker--column row 5)
                 (e-board-sqlite-worker--sql-value
                  (e-board-sqlite-worker--without-keys
                   outcome '(:state :reason :participant-ids :pickup-ids)))))))
    (dolist (row (sqlite-select database
                                "SELECT delivery_key,board_id,generation,participant_id,fifo_position,message_id,state,revision,attempt,payload FROM board_pickups_v7"))
      (let* ((delivery-key (e-runtime-store-offline-worker--column row 0))
             (board-id (e-runtime-store-offline-worker--column row 1))
             (generation (e-runtime-store-offline-worker--column row 2))
             (participant-id (e-runtime-store-offline-worker--column row 3))
             (fifo-position (e-runtime-store-offline-worker--column row 4))
             (message-id (e-runtime-store-offline-worker--column row 5))
             (state (e-runtime-store-offline-worker--column row 6))
             (revision (e-runtime-store-offline-worker--column row 7))
             (attempt (e-runtime-store-offline-worker--column row 8))
             (pickup (e-runtime-store-offline-worker--v7-decode
                      (e-runtime-store-offline-worker--column row 9)
                      "pickup" :board-id board-id :generation generation
                      :participant-id participant-id :message-id message-id)))
        (dolist (field (list (list :delivery-id
                                    (e-runtime-store-offline-worker--v7-value
                                     delivery-key "pickup identity"
                                     :board-id board-id :message-id message-id)
                                    (plist-get pickup :delivery-id))
                             (list :board-id board-id (plist-get pickup :board-id))
                             (list :participant-id participant-id
                                   (plist-get pickup :participant-id))
                             (list :fifo-position fifo-position
                                   (plist-get pickup :fifo-position))
                             (list :message-id message-id
                                   (plist-get pickup :message-id))
                             (list :state state
                                   (e-runtime-store-offline-worker--v7-name
                                    (plist-get pickup :state)))
                             (list :revision revision
                                   (plist-get pickup :revision))
                             (list :attempt attempt
                                   (plist-get pickup :attempt))))
          (when (plist-member pickup (car field))
            (e-runtime-store-offline-worker--v7-assert-equal
             (format "v7 pickup %s disagrees" (car field))
             (cadr field) (caddr field)
             :board-id board-id :generation generation
             :participant-id participant-id :message-id message-id)))
        (sqlite-execute
         database
         "INSERT INTO board_pickups(delivery_key,board_id,generation,participant_id,fifo_position,message_id,state,revision,attempt,payload) VALUES(?,?,?,?,?,?,?,?,?,?)"
         (vector delivery-key board-id generation participant-id fifo-position
                 message-id state revision attempt
                 (e-board-sqlite-worker--sql-value
                  (e-board-sqlite-worker--without-keys
                   pickup '(:delivery-id :board-id :participant-id
                            :fifo-position :message-id :state :revision :attempt)))))))
    (sqlite-execute database
                    "INSERT INTO board_pickup_events(delivery_key,event_position,state,payload,created_at) SELECT delivery_key,event_position,state,payload,created_at FROM board_pickup_events_v7")
    (sqlite-execute database
                    "INSERT INTO board_replay_progress(board_id,generation,subscription_id,position,revision) SELECT board_id,generation,subscription_id,position,revision FROM board_replay_progress_v7")))

(defun e-runtime-store-offline-worker--copy-v7-associations (database)
  "Build constrained session Board associations from v7 state rows."
  (dolist (row (sqlite-select database
                              "SELECT session_id,board_id,principal,association_role,routing_policy,name,root_p FROM session_query_state_v7 WHERE board_id IS NOT NULL"))
    (let* ((session-id (e-runtime-store-offline-worker--column row 0))
           (board-id (e-runtime-store-offline-worker--column row 1))
           ;; Session query scalars were stored as plain TEXT in v7.  Only
           ;; structured values such as routing policy used the store codec.
           (principal (e-runtime-store-offline-worker--column row 2))
           (association-role (e-runtime-store-offline-worker--column row 3))
           (policy (e-runtime-store-offline-worker--v7-decode
                    (e-runtime-store-offline-worker--column row 4)
                    "association" :session-id session-id :board-id board-id))
           (session-name (e-runtime-store-offline-worker--column row 5))
           (root-p (= 1 (e-runtime-store-offline-worker--column row 6)))
           (policy-participant-id (plist-get policy :participant-id))
           (participant-id
            (or policy-participant-id
                (concat
                 "ptc_"
                 (substring
                  (secure-hash 'sha256
                               (format "v7-session-association:%s" session-id))
                  0 32))))
           (board-row
            (car (sqlite-select database
                                "SELECT generation,trusted_principal FROM boards WHERE board_id=?"
                                (vector board-id))))
           (generation (and board-row
                            (e-runtime-store-offline-worker--column
                             board-row 0)))
           (board-principal
            (and board-row
                 (e-runtime-store-offline-worker--v7-value
                  (e-runtime-store-offline-worker--column board-row 1)
                  "Board trusted principal"
                  :session-id session-id :board-id board-id)))
           (participant-row
            (and generation participant-id
                 (car (sqlite-select
                       database
                       "SELECT principal,role FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
                       (vector board-id generation participant-id))))))
      (unless (and (stringp principal) (not (string-empty-p principal)))
        (e-runtime-store-offline-worker--error
         "Malformed v7 association principal value"
         :session-id session-id :board-id board-id
         :principal principal))
      (unless (and (stringp participant-id)
                   (not (string-empty-p participant-id)))
        (e-runtime-store-offline-worker--error
         "Malformed v7 association participant identity"
         :session-id session-id :board-id board-id
         :participant-id participant-id))
      (e-runtime-store-offline-worker--v7-assert-equal
       "v7 association and Board principal disagree"
       principal board-principal
       :session-id session-id :board-id board-id
       :participant-id participant-id)
      (if participant-row
          (progn
            (e-runtime-store-offline-worker--v7-assert-equal
             "v7 association principal disagrees"
             principal (e-runtime-store-offline-worker--column participant-row 0)
             :session-id session-id :board-id board-id
             :participant-id participant-id)
            (when association-role
              (e-runtime-store-offline-worker--v7-assert-equal
               "v7 association role disagrees"
               association-role
               (e-runtime-store-offline-worker--v7-name
                (e-runtime-store-offline-worker--column participant-row 1))
               :session-id session-id :board-id board-id
               :participant-id participant-id)))
        ;; Early v7 sessions could carry a durable Board association before
        ;; participant projections were introduced.  Materialize the missing
        ;; normalized identity entirely from those session and Board facts.
        (let* ((role (or association-role (if root-p "owner" "participant")))
               (name (or (plist-get policy :participant-name)
                         session-name
                         (and (equal role "owner") "Main"))))
          (sqlite-execute
           database
           "INSERT INTO board_participants(board_id,generation,participant_id,principal,author,controller,role,state,name,subscription_id,publication_pending,payload,revision) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,1)"
           (vector board-id generation participant-id principal nil principal
                   role "active" name nil 0 nil))))
      (sqlite-execute
       database
       "INSERT INTO board_session_associations(session_id,board_id,generation,participant_id,routing_policy,revision) VALUES(?,?,?,?,?,1)"
       (vector session-id board-id generation participant-id
               (e-board-sqlite-worker--sql-value
                (e-board-sqlite-worker--without-keys
                 policy '(:session-id :board-id :generation :participant-id
                          :association-role :principal
                          :participant-name))))))))

(defun e-runtime-store-offline-worker--copy-v7-tasks (database)
  "Normalize v7 task/attempt rows and map destructive queued attempts."
  (let* ((queue-rows
          (sqlite-select database
                         "SELECT queue_id,revision,sequence,paused FROM task_queues_v7"))
         (task-rows
          (sqlite-select database
                         "SELECT queue_id,task_id,position,status,revision,payload FROM task_records_v7"))
         (attempt-rows
          (sqlite-select database
                         "SELECT queue_id,task_id,attempt_id,attempt_number,state,started_at,settled_at,payload FROM task_attempts_v7"))
         (queue-sequences (make-hash-table :test 'equal))
         (tasks (make-hash-table :test 'equal))
         (attempts (make-hash-table :test 'equal)))
    (dolist (queue-row queue-rows)
      (let ((queue-id (e-runtime-store-offline-worker--column queue-row 0)))
        (puthash queue-id
                 (e-runtime-store-offline-worker--column queue-row 2)
                 queue-sequences)
        (sqlite-execute
         database
         "INSERT INTO task_queues(queue_id,revision,sequence,paused) VALUES(?,?,?,?)"
         (apply #'vector (append queue-row nil)))))
    (dolist (row task-rows)
      (let* ((queue-id (e-runtime-store-offline-worker--column row 0))
             (task-id (e-runtime-store-offline-worker--column row 1))
             (position (e-runtime-store-offline-worker--column row 2))
             (status (e-runtime-store-offline-worker--column row 3))
             (record (e-runtime-store-offline-worker--v7-decode
                      (e-runtime-store-offline-worker--column row 5)
                      "task" :queue-id queue-id :task-id task-id))
             (key (cons queue-id task-id)))
        (unless (gethash queue-id queue-sequences)
          (e-runtime-store-offline-worker--v7-conflict
           "v7 task references unknown queue" :queue-id queue-id :task-id task-id))
        (when (gethash key tasks)
          (e-runtime-store-offline-worker--v7-conflict
           "v7 task identity is duplicated" :queue-id queue-id :task-id task-id))
        (when (plist-member record :task-id)
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 task identity disagrees" task-id (plist-get record :task-id)
           :queue-id queue-id :task-id task-id))
        (when (plist-member record :status)
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 task status disagrees" status
           (e-runtime-store-offline-worker--v7-name (plist-get record :status))
           :queue-id queue-id :task-id task-id))
        (when (and (plist-member record :harness-instance-id)
                   (plist-member record :harness-selector))
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 task harness selector aliases disagree"
           (plist-get record :harness-instance-id)
           (plist-get record :harness-selector)
           :queue-id queue-id :task-id task-id))
        (when (> position (gethash queue-id queue-sequences))
          (e-runtime-store-offline-worker--v7-conflict
           "v7 task position exceeds queue sequence" :queue-id queue-id
           :task-id task-id :position position))
        (puthash key
                 (list :row row :record record
                       :attempt-id (and (plist-member record :attempt-id)
                                        (plist-get record :attempt-id)))
                 tasks)))
    (dolist (row attempt-rows)
      (let* ((queue-id (e-runtime-store-offline-worker--column row 0))
             (task-id (e-runtime-store-offline-worker--column row 1))
             (attempt-id (e-runtime-store-offline-worker--column row 2))
             (attempt-number (e-runtime-store-offline-worker--column row 3))
             (state (e-runtime-store-offline-worker--column row 4))
             (started-at (e-runtime-store-offline-worker--column row 5))
             (settled-at (e-runtime-store-offline-worker--column row 6))
             (legacy (e-runtime-store-offline-worker--v7-decode
                      (e-runtime-store-offline-worker--column row 7)
                      "attempt" :queue-id queue-id :task-id task-id
                      :attempt-id attempt-id))
             (key (cons queue-id task-id))
             (number-value (and (plist-member legacy :attempt-number)
                                (plist-get legacy :attempt-number)))
             (number-alias (and (plist-member legacy :number)
                                (plist-get legacy :number))))
        (unless (gethash key tasks)
          (e-runtime-store-offline-worker--v7-conflict
           "v7 attempt references unknown task" :queue-id queue-id
           :task-id task-id :attempt-id attempt-id))
        (when (and number-value number-alias)
          (e-runtime-store-offline-worker--v7-assert-equal
           "v7 attempt number aliases disagree" number-value number-alias
           :queue-id queue-id :task-id task-id :attempt-id attempt-id))
        (dolist (field
                 (list (list :task-id task-id (plist-get legacy :task-id))
                       (list :attempt-id attempt-id (plist-get legacy :attempt-id))
                       (list :attempt-number attempt-number
                             (or number-value number-alias))
                       (list :state state
                             (e-runtime-store-offline-worker--v7-name
                              (plist-get legacy :state)))
                       (list :started-at started-at (plist-get legacy :started-at))
                       (list :settled-at settled-at (plist-get legacy :settled-at))))
          (when (plist-member legacy (car field))
            (e-runtime-store-offline-worker--v7-assert-equal
             (format "v7 attempt %s disagrees" (car field))
             (cadr field) (caddr field)
             :queue-id queue-id :task-id task-id :attempt-id attempt-id)))
        (when (and (equal state "queued") settled-at)
          (e-runtime-store-offline-worker--v7-conflict
           "Contradictory v7 queued attempt facts"
           :queue-id queue-id :task-id task-id :attempt-id attempt-id))
        (when (or (not (integerp attempt-number)) (<= attempt-number 0))
          (e-runtime-store-offline-worker--v7-conflict
           "Invalid v7 attempt number" :queue-id queue-id :task-id task-id
           :attempt-id attempt-id :attempt-number attempt-number))
        (let ((prior (gethash key attempts)))
          (when (seq-find
                 (lambda (existing)
                   (= attempt-number
                      (e-runtime-store-offline-worker--column existing 3)))
                 prior)
            (e-runtime-store-offline-worker--v7-conflict
             "Duplicate v7 attempt number" :queue-id queue-id :task-id task-id
             :attempt-number attempt-number))
          (puthash key (cons row prior) attempts))))
    (maphash
     (lambda (key task)
       (let* ((queue-id (car key))
              (task-id (cdr key))
              (row (plist-get task :row))
              (record (plist-get task :record))
              (task-status (e-runtime-store-offline-worker--column row 3))
              (task-attempt-id (plist-get task :attempt-id))
              (task-attempts
               (sort (copy-sequence (gethash key attempts))
                     (lambda (left right)
                       (< (e-runtime-store-offline-worker--column left 3)
                          (e-runtime-store-offline-worker--column right 3)))))
              (latest-row (car (last task-attempts)))
              (latest-id (and latest-row
                              (e-runtime-store-offline-worker--column latest-row 2)))
              (latest-state (and latest-row
                                  (e-runtime-store-offline-worker--column latest-row 4))))
         (when (and task-attempt-id
                    (not (equal task-attempt-id latest-id)))
           (e-runtime-store-offline-worker--v7-conflict
            "v7 task latest attempt disagrees" :queue-id queue-id
            :task-id task-id :expected latest-id :actual task-attempt-id))
         (when (and (member task-status '("running" "pausing"))
                    (null latest-row))
           (e-runtime-store-offline-worker--v7-conflict
            "v7 active task has no attempt"
            :queue-id queue-id :task-id task-id :task-status task-status))
         (when (and latest-state
                    (not (or (and (member task-status '("queued" "paused"))
                                  (equal latest-state "queued"))
                             (and (equal task-status "running")
                                  (member latest-state '("claimed" "running")))
                             (equal task-status latest-state))))
           (e-runtime-store-offline-worker--v7-conflict
            "v7 task and latest attempt topology disagrees"
            :queue-id queue-id :task-id task-id
            :task-status task-status :attempt-state latest-state))
         (sqlite-execute
          database
          "INSERT INTO task_records(queue_id,task_id,position,status,revision,enqueued_at,harness_selector,retry_count,latest_attempt_id,payload) VALUES(?,?,?,?,?,?,?,?,?,?)"
          (vector queue-id task-id
                  (e-runtime-store-offline-worker--column row 2)
                  task-status
                  (e-runtime-store-offline-worker--column row 4)
                  (plist-get record :enqueued-at)
                  (e-task-storage-sqlite-worker--pack
                   (or (plist-get record :harness-instance-id)
                       (plist-get record :harness-selector)))
                  (or (plist-get record :retries) 0)
                  latest-id
                  (e-task-storage-sqlite-worker--pack
                   (e-task-storage-sqlite-worker--task-content record))))))
     tasks)
    (dolist (row attempt-rows)
      (let* ((queue-id (e-runtime-store-offline-worker--column row 0))
             (task-id (e-runtime-store-offline-worker--column row 1))
             (attempt-id (e-runtime-store-offline-worker--column row 2))
             (state (e-runtime-store-offline-worker--column row 4))
             (legacy (e-runtime-store-offline-worker--v7-decode
                      (e-runtime-store-offline-worker--column row 7)
                      "attempt" :queue-id queue-id :task-id task-id
                      :attempt-id attempt-id)))
        (sqlite-execute
         database
         "INSERT INTO task_attempts(queue_id,task_id,attempt_id,attempt_number,state,harness_instance_id,session_id,started_at,settled_at,outputs,error) VALUES(?,?,?,?,?,?,?,?,?,?,?)"
         (vector queue-id task-id attempt-id
                 (e-runtime-store-offline-worker--column row 3)
                 (if (equal state "queued") "legacy-requeued" state)
                 (plist-get legacy :harness-instance-id)
                 (plist-get legacy :session-id)
                 (e-runtime-store-offline-worker--column row 5)
                 (e-runtime-store-offline-worker--column row 6)
                 (and (plist-get legacy :outputs)
                      (e-task-storage-sqlite-worker--pack
                       (plist-get legacy :outputs)))
                 (and (plist-get legacy :error)
                      (e-task-storage-sqlite-worker--pack
                       (plist-get legacy :error)))))))))

(defun e-runtime-store-offline-worker--install-v8 (database)
  "Rebuild normalized v8 communication relations from stopped v7 tables."
  (sqlite-execute database "DROP TABLE IF EXISTS session_process_report_index")
  (dolist (index '("session_query_state_board" "session_records_position"
                   "task_records_dispatch" "board_records_selector"
                   "board_record_tags_selector" "board_record_attributes_selector"
                   "board_pickups_unresolved"))
    (sqlite-execute database (format "DROP INDEX IF EXISTS %s" index)))
  (dolist (table '("session_query_state" "boards" "board_records" "board_record_tags"
                   "board_record_attributes" "board_routing" "board_pickups"
                   "board_pickup_events" "board_participants" "board_replay_progress"
                   "task_queues" "task_records" "task_attempts"
                   "board_session_admissions"))
    (e-runtime-store-offline-worker--rename-table database table))
  (e-runtime-store-worker--initialize-domain-schema database)
  (e-runtime-store-offline-worker--copy-v7-session-state database)
  (e-runtime-store-offline-worker--copy-v7-board database)
  (e-runtime-store-offline-worker--copy-v7-associations database)
  (e-runtime-store-offline-worker--copy-v7-tasks database)
  (e-runtime-store-session-worker-initialize-process-report-projection database)
  ;; The process-report projection remains derived state.  Rebuild it from
  ;; the copied canonical journal before parity verification; carrying the old
  ;; rows would retain the retired foreign-key boundary.
  (e-runtime-store-offline-worker--populate-v7 database)
  (dolist (table '("session_query_state_v7" "board_session_admissions_v7"
                   "board_pickup_events_v7" "board_replay_progress_v7"
                   "board_routing_v7" "board_pickups_v7"
                   "board_record_tags_v7" "board_record_attributes_v7"
                   "board_records_v7" "board_participants_v7" "boards_v7"
                   "task_attempts_v7" "task_records_v7" "task_queues_v7"))
    (when (e-runtime-store-offline-worker--table-exists-p database table)
      (sqlite-execute database (format "DROP TABLE %s" table)))))

(defun e-runtime-store-offline-worker--projection-error
    (error session-id position report)
  "Raise bounded migration ERROR for REPORT at SESSION-ID POSITION."
  (e-runtime-store-offline-worker--error
   "Process-report projection migration failed"
   :session-id session-id :position position
   :report-type
   (let ((type (and (listp report) (plist-get report :report-type))))
     (cond ((stringp type) type) ((symbolp type) (symbol-name type)) (t nil)))
   :field (plist-get (cddr error) :field)
   :limit (plist-get (cddr error) :limit)))

(defun e-runtime-store-offline-worker--projection-rows
    (record session-id position)
  "Derive bounded projection rows from RECORD for diagnostics ownership."
  (condition-case err
      (e-session-process-report-projection-rows record)
    (e-session-process-report-projection-error
     (e-runtime-store-offline-worker--projection-error
      err session-id position (plist-get record :report)))))

(defun e-runtime-store-offline-worker--insert-projection
    (database session-id position record)
  "Insert RECORD's projection for SESSION-ID POSITION."
  (dolist (row (e-runtime-store-offline-worker--projection-rows
                record session-id position))
    (sqlite-execute
     database
     "INSERT INTO session_process_report_index(session_id,position,association_ordinal,report_type,marker_id,provider_request_id,triage_status) VALUES(?,?,?,?,?,?,?)"
     (vector session-id position
             (plist-get row :association-ordinal)
             (plist-get row :report-type)
             (plist-get row :marker-id)
             (plist-get row :provider-request-id)
             (plist-get row :triage-status)))))

(defun e-runtime-store-offline-worker--populate-v7 (database)
  "Populate the v7 projection in bounded journal pages."
  (let ((base-count 0) (association-count 0) (record-count 0))
    (e-runtime-store-offline-worker--visit-sessions
     database "session_records"
     (lambda (session-id)
       (e-runtime-store-offline-worker--visit-session
        database "session_records" session-id
        (lambda (state position _payload record)
          (let ((rows (e-runtime-store-offline-worker--projection-rows
                       record session-id position)))
            (when rows
              (setq base-count (1+ base-count)
                    association-count
                    (+ association-count (1- (length rows))))
              (dolist (row rows)
                (sqlite-execute
                 database
                 "INSERT INTO session_process_report_index(session_id,position,association_ordinal,report_type,marker_id,provider_request_id,triage_status) VALUES(?,?,?,?,?,?,?)"
                 (vector session-id position
                         (plist-get row :association-ordinal)
                         (plist-get row :report-type)
                         (plist-get row :marker-id)
                         (plist-get row :provider-request-id)
                         (plist-get row :triage-status)))))
            (setq record-count (1+ record-count))
            state)))))
    (list :records record-count :base-rows base-count
          :association-rows association-count)))

(defun e-runtime-store-offline-worker--preflight-v7 (database)
  "Validate every canonical record needed by v7 without writing."
  (let ((records 0) (base-rows 0) (association-rows 0))
    (e-runtime-store-offline-worker--visit-sessions
     database "session_records"
     (lambda (session-id)
       (e-runtime-store-offline-worker--visit-session
        database "session_records" session-id
        (lambda (state position _payload record)
          (let ((rows (e-runtime-store-offline-worker--projection-rows
                       record session-id position)))
            (when rows
              (setq base-rows (1+ base-rows)
                    association-rows (+ association-rows (1- (length rows)))))
            (setq records (1+ records))
            state)))))
    (list :records records :base-rows base-rows
          :association-rows association-rows)))

(defun e-runtime-store-offline-worker--verify-v7-projection (database)
  "Verify exact v7 projection parity from bounded canonical reads."
  (let ((expected-base 0) (expected-associations 0) (canonical-records 0))
    (e-runtime-store-offline-worker--visit-sessions
     database "session_records"
     (lambda (session-id)
       (e-runtime-store-offline-worker--visit-session
        database "session_records" session-id
        (lambda (state position _payload record)
          (let* ((expected
                  (e-runtime-store-offline-worker--projection-rows
                   record session-id position))
                 (actual
                  (mapcar
                   (lambda (row)
                     (list :association-ordinal
                           (e-runtime-store-offline-worker--column row 0)
                           :report-type
                           (e-runtime-store-offline-worker--column row 1)
                           :marker-id
                           (e-runtime-store-offline-worker--column row 2)
                           :provider-request-id
                           (e-runtime-store-offline-worker--column row 3)
                           :triage-status
                           (e-runtime-store-offline-worker--column row 4)))
                   (sqlite-select
                    database
                    (concat
                     "SELECT association_ordinal,report_type,marker_id,"
                     "provider_request_id,triage_status"
                     " FROM session_process_report_index"
                     " WHERE session_id=? AND position=?"
                     " ORDER BY association_ordinal")
                    (vector session-id position)))))
            (unless (equal expected actual)
              (e-runtime-store-offline-worker--error
               "Process-report projection parity failed"
               :session-id session-id :position position
               :report-type
               (and expected (plist-get (car expected) :report-type))))
            (when expected
              (setq expected-base (1+ expected-base)
                    expected-associations
                    (+ expected-associations (1- (length expected)))))
            (setq canonical-records (1+ canonical-records))
            state)))))
    (let* ((counts
            (car (sqlite-select
                  database
                  (concat
                   "SELECT SUM(CASE WHEN association_ordinal=0 THEN 1 ELSE 0 END),"
                   "SUM(CASE WHEN association_ordinal>0 THEN 1 ELSE 0 END),"
                   "COUNT(*) FROM session_process_report_index"))))
           (base (or (e-runtime-store-offline-worker--column counts 0) 0))
           (associations
            (or (e-runtime-store-offline-worker--column counts 1) 0))
           (total (e-runtime-store-offline-worker--column counts 2))
           (orphans
            (e-runtime-store-offline-worker--column
             (car (sqlite-select
                   database
                   (concat
                    "SELECT COUNT(*) FROM session_process_report_index p"
                    " LEFT JOIN session_records r ON r.session_id=p.session_id"
                    " AND r.position=p.position WHERE r.session_id IS NULL")))
             0)))
      (unless (and (= base expected-base)
                   (= associations expected-associations)
                   (= total (+ expected-base expected-associations))
                   (= orphans 0))
        (e-runtime-store-offline-worker--error
         "Process-report projection aggregate parity failed"
         :base base :expected-base expected-base
         :associations associations
         :expected-associations expected-associations :orphans orphans))
      (list :records canonical-records :base-rows base
            :association-rows associations :orphans orphans))))

(defun e-runtime-store-offline-worker--verify-v7-schema (database)
  "Verify the exact v7 relation, indexes, and projection parity."
  (unless (e-runtime-store-offline-worker--table-exists-p
           database "session_process_report_index")
    (e-runtime-store-offline-worker--error
     "Schema v7 process-report projection is missing"))
  (condition-case err
      (e-runtime-store-session-worker-verify-process-report-projection-schema
       database)
    (e-runtime-store-worker-error
     (e-runtime-store-offline-worker--error
      "Schema v7 process-report shape is invalid"
      :reason (car (cdr err))
      :index (plist-get (cddr err) :index))))
  (e-runtime-store-offline-worker--verify-v7-projection database))

(defun e-runtime-store-offline-worker--verify-v8-schema (database)
  "Verify normalized v8 relations and projection parity."
  (dolist (table '("board_session_associations" "board_records"
                   "board_record_tags" "board_record_attributes"
                   "board_routing" "board_pickups" "board_participants"
                   "task_records" "task_attempts" "session_query_state"))
    (unless (e-runtime-store-offline-worker--table-exists-p database table)
      (e-runtime-store-offline-worker--error
       "Schema v8 normalized relation is missing" :table table)))
  (dolist (table '("board_session_admissions" "session_query_state_v7"
                   "board_records_v7" "board_pickups_v7" "task_records_v7"
                   "task_attempts_v7"))
    (when (e-runtime-store-offline-worker--table-exists-p database table)
      (e-runtime-store-offline-worker--error
       "Schema v8 retains obsolete relation" :table table)))
  (let ((e-runtime-store-worker--database database))
    (condition-case err
        (e-runtime-store-worker--verify-v8-normalized-schema)
      (e-runtime-store-schema-too-old
       (e-runtime-store-offline-worker--error
        "Schema v8 normalized relation shape is invalid"
        :reason (plist-get (cdr err) :reason)))))
  (condition-case err
      (e-runtime-store-session-worker-verify-process-report-projection-schema
       database)
    (e-runtime-store-worker-error
     (e-runtime-store-offline-worker--error
      "Schema v8 process-report shape is invalid"
      :reason (car (cdr err)))))
  (e-runtime-store-offline-worker--verify-v7-projection database))

(defun e-runtime-store-offline-worker--remove-sqlite-sidecars (database-file)
  "Remove SQLite WAL sidecars for DATABASE-FILE after its connection closes."
  (dolist (file (list (concat database-file "-wal")
                      (concat database-file "-shm")))
    (when (file-exists-p file)
      (delete-file file))))

(defun e-runtime-store-offline-worker--install-v5-envelope (database)
  "Install the retained v4-to-v5 generic envelope on DATABASE.

This is deliberately a small compatibility stage.  It does not derive or
rewrite any domain relation; the enclosing transaction immediately continues
to the v6 session cutover.  Keeping the stage explicit preserves the durable
v4 migration identity and lets one verified operator invocation roll back to
the original v4 image if the later v6 install fails."
  (e-runtime-store-worker--initialize-common-schema database)
  (let* ((identity "feature92-v4-to-v5-explicit-upgrade")
         (checksum (secure-hash 'sha256 "feature92-schema-v5"))
         (row (car (sqlite-select
                    database
                    "SELECT identity,checksum FROM schema_migrations WHERE version=5"))))
    (when (and row
               (or (not (equal (e-runtime-store-offline-worker--column row 0)
                               identity))
                   (not (equal (e-runtime-store-offline-worker--column row 1)
                               checksum))))
      (e-runtime-store-offline-worker--error
       "Existing v5 migration row is malformed" row))
    (unless row
      (sqlite-execute
       database
       "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(?,?,?,?)"
       (vector 5 identity checksum (float-time))))
    (sqlite-execute database
                    "UPDATE store_meta SET value='5' WHERE key='schema_version'")))

(defun e-runtime-store-offline-worker--restore-source
    (database-file backup-file &optional expected-version)
  "Restore DATABASE-FILE from verified BACKUP-FILE and recheck its version.

EXPECTED-VERSION defaults to 5 for callers retaining the historical helper
name.  The v4-to-v5-to-v6 path passes its original version so a post-install
fault restores the exact pre-upgrade schema boundary."
  (e-runtime-store-offline-worker--remove-sqlite-sidecars database-file)
  (copy-file backup-file database-file t)
  (set-file-modes database-file #o600)
  (let ((restored (sqlite-open database-file)))
    (unwind-protect
        (progn
          (e-runtime-store-offline-worker--check restored)
          (unless (= (e-runtime-store-offline-worker--version restored)
                     (or expected-version 5))
            (e-runtime-store-offline-worker--error
             "Verified backup did not restore the original schema"
             :expected (or expected-version 5)))
          t)
      (sqlite-close restored))))

(defun e-runtime-store-offline-worker--upgrade (database-file backup-file)
  "Upgrade stopped schema v4, v5, v6, or v7 DATABASE-FILE explicitly to v8.

The complete supported chain uses one verified backup and one install
transaction.  A verified v7 source is a read-only no-op and creates no backup."
  (unless (file-readable-p database-file)
    (e-runtime-store-offline-worker--error "Store must exist" database-file))
  (let ((claim
         (e-runtime-store-ownership-acquire
          database-file (format "offline:%d" (emacs-pid)) 'offline))
        (database nil)
        (committed nil)
        (version nil)
        (copied nil)
        (projection nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq database (sqlite-open database-file)
                    version (e-runtime-store-offline-worker--version database))
              (sqlite-execute database "PRAGMA foreign_keys=ON")
              (let ((current e-runtime-store-worker-schema-version))
                (cond
                 ((> version current)
                  (signal 'e-runtime-store-schema-too-new
                          (list :actual version :supported current)))
                 ((= version current)
                  (e-runtime-store-offline-worker--check database)
                  (e-runtime-store-offline-worker--verify-v8-lineage database)
                  (setq projection
                        (e-runtime-store-offline-worker--verify-v8-schema
                         database))
                  (list :from version :to current :noop t :backup nil
                        :records (plist-get projection :records)
                        :projection-base-rows
                        (plist-get projection :base-rows)
                        :projection-association-rows
                        (plist-get projection :association-rows)
                        :integrity "ok"))
                 ((not (memq version '(4 5 6 7)))
                  (e-runtime-store-offline-worker--error
                   "No supported direct upgrade path" version current))
                 (t
                  (unless (e-runtime-store-offline-worker--table-exists-p
                           database "session_records")
                    (e-runtime-store-offline-worker--error
                     "Source store has no canonical session journal"))
                  (e-runtime-store-offline-worker--check database)
                  (when (>= version 6)
                    (e-runtime-store-offline-worker--verify-v6-lineage database))
                  ;; A v7 lineage failure is a read-only source diagnostic;
                  ;; reject it before creating the operator backup.
                  (when (= version 7)
                    (e-runtime-store-offline-worker--verify-v7-lineage database))
                  (if (< version 6)
                      (e-runtime-store-offline-worker--preflight-v5 database)
                    (e-runtime-store-offline-worker--preflight-v7 database))
                  (when (file-exists-p backup-file)
                    (signal 'file-already-exists (list backup-file)))
                  (make-directory (file-name-directory backup-file) t)
                  (set-file-modes (file-name-directory backup-file) #o700)
                  (sqlite-execute database "VACUUM INTO ?" (vector backup-file))
                  (set-file-modes backup-file #o600)
                  (let ((backup (sqlite-open backup-file)))
                    (unwind-protect
                        (progn
                          (e-runtime-store-offline-worker--check backup)
                          (unless (= (e-runtime-store-offline-worker--version backup)
                                     version)
                            (e-runtime-store-offline-worker--error
                             "Backup schema verification failed"
                             :actual
                             (e-runtime-store-offline-worker--version backup))))
                      (sqlite-close backup)))
                  (e-runtime-store-offline-worker--fault 'before)
                  (sqlite-execute database "BEGIN IMMEDIATE")
                  (condition-case transaction-error
                      (progn
                        (when (= version 4)
                          (e-runtime-store-offline-worker--install-v5-envelope
                           database)
                          (e-runtime-store-offline-worker--fault 'after-v5))
                        (when (< version 6)
                          (e-runtime-store-offline-worker--install-v6 database)
                          (when (= version 5)
                            (e-runtime-store-offline-worker--ensure-v5-provenance
                             database))
                          (setq copied
                                (e-runtime-store-offline-worker--copy-v5-sessions
                                 database))
                          (e-runtime-store-offline-worker--parity-check
                           database "session_records_v5" "session_records")
                          (sqlite-execute
                           database "DROP TABLE IF EXISTS session_records_v5")
                          (sqlite-execute
                           database "DROP TABLE IF EXISTS catalog_projection")
                          (sqlite-execute
                           database "DROP TABLE IF EXISTS session_checkpoints")
                          (e-runtime-store-offline-worker--ensure-migration-row
                           database 6
                           e-runtime-store-offline-worker--v6-migration-identity
                           e-runtime-store-offline-worker--v6-migration-checksum))
                        (when
                            (and
                             (= version 6)
                             (equal
                              (car
                               (e-runtime-store-offline-worker--migration-row
                                database 6))
                              e-runtime-store-offline-worker--v6-migration-identity))
                          (e-runtime-store-offline-worker--ensure-v5-provenance
                           database))
                        (e-runtime-store-offline-worker--fault 'after-v6)
                        (when (< version 7)
                          (e-runtime-store-offline-worker--install-v7 database)
                          (e-runtime-store-offline-worker--fault 'after-v7-schema)
                          (setq projection
                                (e-runtime-store-offline-worker--populate-v7
                                 database))
                          (e-runtime-store-offline-worker--fault
                           'after-v7-populate)
                          (setq projection
                                (e-runtime-store-offline-worker--verify-v7-schema
                                 database))
                          (e-runtime-store-offline-worker--fault 'after-v7-parity)
                          (e-runtime-store-offline-worker--ensure-migration-row
                           database 7
                           e-runtime-store-offline-worker--v7-migration-identity
                           e-runtime-store-offline-worker--v7-migration-checksum)
                          (e-runtime-store-offline-worker--verify-v7-lineage
                           database))
                        (e-runtime-store-offline-worker--install-v8 database)
                        (e-runtime-store-offline-worker--fault 'after-v8-schema)
                        (e-runtime-store-offline-worker--fault 'after-v8-populate)
                        (setq projection
                              (e-runtime-store-offline-worker--verify-v8-schema
                               database))
                        (e-runtime-store-offline-worker--fault 'after-v8-parity)
                        (e-runtime-store-offline-worker--ensure-migration-row
                         database 8
                         e-runtime-store-offline-worker--v8-migration-identity
                         e-runtime-store-offline-worker--v8-migration-checksum)
                        (e-runtime-store-offline-worker--verify-v8-lineage
                         database)
                        (sqlite-execute
                         database
                         "UPDATE store_meta SET value='8' WHERE key='schema_version'")
                        (sqlite-execute database "COMMIT")
                        (setq committed t))
                    (error
                     (ignore-errors (sqlite-execute database "ROLLBACK"))
                     (signal (car transaction-error) (cdr transaction-error))))
                  (e-runtime-store-offline-worker--fault 'after)
                  (e-runtime-store-offline-worker--check database)
                  (e-runtime-store-offline-worker--verify-v8-lineage database)
                  (setq projection
                        (e-runtime-store-offline-worker--verify-v8-schema
                         database))
                  (set-file-modes database-file #o600)
                  (list :from version :to current :backup backup-file
                        :backup-bytes
                        (file-attribute-size (file-attributes backup-file))
                        :sessions (plist-get copied :sessions)
                        :records (or (plist-get copied :records)
                                     (plist-get projection :records))
                        :payload-bytes (plist-get copied :payload-bytes)
                        :projection-base-rows
                        (plist-get projection :base-rows)
                        :projection-association-rows
                        (plist-get projection :association-rows)
                        :integrity "ok")))))
          (error
           (when database
             (ignore-errors (sqlite-close database))
             (setq database nil))
           (when committed
             (e-runtime-store-offline-worker--restore-source
              database-file backup-file version)
             (setq committed nil))
           (signal (car err) (cdr err))))
      (when database (sqlite-close database))
      (e-runtime-store-ownership-release claim))))

(defun e-runtime-store-offline-worker-main ()
  "Execute one narrow operation from `command-line-args-left'."
  (let* ((operation (pop command-line-args-left))
         (database-file (pop command-line-args-left))
         (output-file (pop command-line-args-left))
         (argument (pop command-line-args-left))
         result)
    (unless (and operation database-file output-file)
      (signal 'e-runtime-store-offline-error (list "Missing offline arguments")))
    (setq result
          (pcase operation
            ("upgrade"
             (e-runtime-store-offline-worker--upgrade
              (expand-file-name database-file) (expand-file-name argument)))
            (_ (signal 'e-runtime-store-offline-error
                       (list "Unknown offline operation" operation)))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (e-runtime-store-codec-encode result)
                    nil output-file nil 'silent))
    (set-file-modes output-file #o600)))

(provide 'e-runtime-store-offline-worker)

;;; e-runtime-store-offline-worker.el ends here

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
(require 'e-session-query)
(require 'e-runtime-store-ownership)

(define-error 'e-runtime-store-offline-error "Offline runtime-store operation failed")

(defconst e-runtime-store-offline-worker--legacy-session-page-size 128
  "Maximum session identities visited by one bounded migration page.")
(defconst e-runtime-store-offline-worker--legacy-record-page-size 128
  "Maximum v5 journal rows read by one bounded migration page.")
(defconst e-runtime-store-offline-worker--migration-identity
  "feature92-v5-to-v6-explicit-upgrade"
  "Durable identity installed for the explicit v5-to-v6 migration.")
(defconst e-runtime-store-offline-worker--migration-checksum
  "feature92-schema-v6"
  "Logical checksum for the v5-to-v6 schema boundary.")

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
     (t nil))))

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
        "INSERT INTO session_query_state(session_id,name,summary,metadata,created_at,updated_at,last_message_at,latest_assistant_marker,message_count,current_branch,turn_options,current_head_id,root_event_id,current_context_generation_id,board_id,principal,association_role,routing_policy,root_p,board_output_sequence,board_activity_sequence,journal_position) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
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
  (e-runtime-store-worker--initialize-domain-schema database))

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

(defun e-runtime-store-offline-worker--restore-v5
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
  "Upgrade a stopped v4 or v5 DATABASE-FILE to v6 after verified BACKUP-FILE.

Only this explicit operator operation may cross the v4/v5/v6 schema boundary;
ordinary startup remains reject-only.  A v4 source receives the retained
generic v5 envelope inside the same atomic install before the v6 session
relations are derived.  All derivation is bounded, one session at a time, and
uses the canonical journal rather than retired projections."
  (unless (file-readable-p database-file)
    (e-runtime-store-offline-worker--error
     "Store must exist" database-file))
  (when (file-exists-p backup-file)
    (signal 'file-already-exists (list backup-file)))
  (make-directory (file-name-directory backup-file) t)
  (set-file-modes (file-name-directory backup-file) #o700)
  ;; Claim before the first SQLite open.  The ordinary worker and this explicit
  ;; offline operation deliberately share the same process-lifetime authority.
  (let ((claim
         (e-runtime-store-ownership-acquire
          database-file (format "offline:%d" (emacs-pid)) 'offline)))
      (let ((database nil)
          (committed nil)
          (version nil)
          (copied nil))
      (unwind-protect
          (condition-case err
              (progn
                (setq database (sqlite-open database-file)
                      version (e-runtime-store-offline-worker--version database))
                (let ((current e-runtime-store-worker-schema-version))
                  (cond
                   ((> version current)
                    (signal 'e-runtime-store-schema-too-new
                            (list :actual version :supported current)))
                   ((= version current)
                    (e-runtime-store-offline-worker--error
                     "Store already uses the current schema" current))
                   ((not (memq version '(4 5)))
                    (e-runtime-store-offline-worker--error
                     "No supported direct upgrade path" version current)))
                  (unless (e-runtime-store-offline-worker--table-exists-p
                           database "session_records")
                    (e-runtime-store-offline-worker--error
                     "v5 store has no canonical session journal"))
                  (e-runtime-store-offline-worker--check database)
                  ;; Validate every canonical journal before allocating a
                  ;; multi-gigabyte backup.  This pass is read-only; a failure
                  ;; cannot leave schema state to restore.
                  (e-runtime-store-offline-worker--preflight-v5 database)
                  ;; VACUUM INTO is the sole backup mechanism.  It is done
                  ;; before any schema write and the copy is independently
                  ;; opened, fully checked, and version-verified.
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
                             :actual (e-runtime-store-offline-worker--version
                                      backup))))
                      (sqlite-close backup)))
                  (when (eq (e-runtime-store-offline-worker--fault-mode) 'before)
                    (e-runtime-store-offline-worker--error
                     "Forced migration failure before schema transaction"))
                  (sqlite-execute database "BEGIN IMMEDIATE")
                  (condition-case transaction-error
                      (progn
                        (when (= version 4)
                          (e-runtime-store-offline-worker--install-v5-envelope
                           database))
                        (e-runtime-store-offline-worker--install-v6 database)
                        (setq copied
                              (e-runtime-store-offline-worker--copy-v5-sessions
                               database))
                        (e-runtime-store-offline-worker--parity-check
                         database "session_records_v5" "session_records")
                        ;; Retired projections are witnesses only.  Remove
                        ;; them after canonical replay and parity, still in
                        ;; the same atomic transaction.
                        (sqlite-execute database
                                        "DROP TABLE IF EXISTS session_records_v5")
                        (sqlite-execute database
                                        "DROP TABLE IF EXISTS catalog_projection")
                        (sqlite-execute database
                                        "DROP TABLE IF EXISTS session_checkpoints")
                        (sqlite-execute
                         database
                         "INSERT OR REPLACE INTO schema_migrations(version,identity,checksum,applied_at) VALUES(?,?,?,?)"
                         (vector current e-runtime-store-offline-worker--migration-identity
                                 (secure-hash
                                  'sha256
                                  e-runtime-store-offline-worker--migration-checksum)
                                 (float-time)))
                        (sqlite-execute
                         database
                         "UPDATE store_meta SET value=? WHERE key='schema_version'"
                         (vector (number-to-string current)))
                        (sqlite-execute database "COMMIT")
                        (setq committed t))
                    (error
                     (ignore-errors (sqlite-execute database "ROLLBACK"))
                     (signal (car transaction-error)
                             (cdr transaction-error))))
                  (when (eq (e-runtime-store-offline-worker--fault-mode) 'after)
                    (e-runtime-store-offline-worker--error
                     "Forced migration failure after schema transaction"))
                  (e-runtime-store-offline-worker--check database)
                  (set-file-modes database-file #o600)
                  (list :from version :to current :backup backup-file
                        :backup-bytes
                        (file-attribute-size (file-attributes backup-file))
                        :sessions (plist-get copied :sessions)
                        :records (plist-get copied :records)
                        :payload-bytes (plist-get copied :payload-bytes)
                        :integrity "ok")))
            (error
             ;; A post-COMMIT fault is deliberately stronger than an ordinary
             ;; transaction rollback: close the v6 handle and restore the
             ;; verified v5 image, then recheck the restored store before
             ;; returning the injected failure to the operator.
             (when database
               (ignore-errors (sqlite-close database))
               (setq database nil))
             (when committed
               (e-runtime-store-offline-worker--restore-v5
                database-file backup-file version)
               (setq committed nil))
             (signal (car err) (cdr err))))
        (when database
          (sqlite-close database))
        ;; Both the ordinary and offline paths close SQLite before releasing
        ;; their shared process-lifetime ownership claim.
        (e-runtime-store-ownership-release claim)))))

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

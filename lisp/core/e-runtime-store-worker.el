;;; e-runtime-store-worker.el --- Subordinate SQLite runtime-store worker -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Loaded only by a private batch Emacs.  All SQL and schema knowledge for the
;; runtime store stays on this side of the typed process protocol.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'subr-x)
(require 'e-runtime-store-codec)

(define-error 'e-runtime-store-worker-error "Runtime store worker error")
(define-error 'e-runtime-store-owner-active "Runtime store already has a live owner"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-resource-too-large "Runtime store resource is too large"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-schema-too-old "Runtime store schema requires explicit upgrade"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-schema-too-new "Runtime store schema is newer than this runtime"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-board-conflict "Runtime store Board conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-task-conflict "Runtime store task conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-cron-conflict "Runtime store cron conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-goodnite-conflict "Runtime store Goodnite conflict"
  'e-runtime-store-worker-error)
(define-error 'e-runtime-store-raw-conflict "Runtime store raw-result conflict"
  'e-runtime-store-worker-error)

(require 'e-board-storage-sqlite-worker)
(require 'e-cron-storage-sqlite-worker)
(require 'e-goodnite-storage-sqlite-worker)
(require 'e-raw-results-storage-sqlite-worker)
(require 'e-task-storage-sqlite-worker)
(require 'e-voice-storage-sqlite-worker)

(defconst e-runtime-store-worker-schema-version 4)
(defconst e-runtime-store-worker-resource-byte-limit (* 16 1024 1024)
  "Private one-BLOB resource limit; deliberately above ordinary tool details.")
(defconst e-runtime-store-worker-session-page-byte-limit (* 1024 1024)
  "Private encoded-payload budget for one session page result.")
(defconst e-runtime-store-worker-session-record-byte-limit (* 16 1024 1024)
  "Private encoded limit for one exact session record.")

(defvar e-runtime-store-worker--database nil)
(defvar e-runtime-store-worker--database-file nil)
(defvar e-runtime-store-worker--owner-file nil)
(defvar e-runtime-store-worker--runtime-id nil)
(defvar e-runtime-store-worker--owns-owner-file nil)

(defun e-runtime-store-worker--column (row index)
  "Return INDEX from SQLite ROW across supported Emacs return shapes."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-runtime-store-worker--pack (value)
  "Return VALUE as ASCII transport text."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-runtime-store-worker--unpack (text)
  "Return exact value encoded by ASCII TEXT."
  (e-runtime-store-codec-decode (base64-decode-string text)))

(defun e-runtime-store-worker--sql-value (value)
  "Return VALUE encoded for a SQLite TEXT field."
  (e-runtime-store-worker--pack value))

(defun e-runtime-store-worker--value (text)
  "Return exact value stored in SQLite TEXT."
  (and text (e-runtime-store-worker--unpack text)))

(defun e-runtime-store-worker--pid-live-p (pid)
  "Return non-nil when PID identifies a live process."
  (and (integerp pid) (> pid 0) (process-attributes pid)))

(defun e-runtime-store-worker--read-owner ()
  "Return the existing owner record, or nil when unreadable."
  (when (file-readable-p e-runtime-store-worker--owner-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents e-runtime-store-worker--owner-file)
          (read (current-buffer)))
      (error nil))))

(defun e-runtime-store-worker--claim-owner ()
  "Claim the runtime ownership file or reject a live owner."
  (when-let* ((owner (e-runtime-store-worker--read-owner)))
    (when (e-runtime-store-worker--pid-live-p (plist-get owner :pid))
      (signal 'e-runtime-store-owner-active
              (list :runtime-id (plist-get owner :runtime-id)
                    :pid (plist-get owner :pid)))))
  (when (file-exists-p e-runtime-store-worker--owner-file)
    (delete-file e-runtime-store-worker--owner-file))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region
     (prin1-to-string (list :runtime-id e-runtime-store-worker--runtime-id
                            :pid (emacs-pid) :started-at (float-time)))
     nil e-runtime-store-worker--owner-file nil 'silent nil 'excl))
  (set-file-modes e-runtime-store-worker--owner-file #o600)
  (setq e-runtime-store-worker--owns-owner-file t))

(defun e-runtime-store-worker--permissions ()
  "Apply restrictive modes to current SQLite and ownership files."
  (dolist (file (list e-runtime-store-worker--database-file
                      (concat e-runtime-store-worker--database-file "-wal")
                      (concat e-runtime-store-worker--database-file "-shm")
                      e-runtime-store-worker--owner-file))
    (when (file-exists-p file)
      (set-file-modes file #o600))))

(defun e-runtime-store-worker--release-owner ()
  "Release only this worker's current runtime ownership file."
  (when (and e-runtime-store-worker--owns-owner-file
             e-runtime-store-worker--owner-file
             (file-exists-p e-runtime-store-worker--owner-file))
    (let ((owner (e-runtime-store-worker--read-owner)))
      (when (and (equal (plist-get owner :runtime-id)
                        e-runtime-store-worker--runtime-id)
                 (= (or (plist-get owner :pid) -1) (emacs-pid)))
        (delete-file e-runtime-store-worker--owner-file)))))

(defun e-runtime-store-worker--schema (new-store-p)
  "Create the current schema when NEW-STORE-P, otherwise verify it."
  ;; Inspect an existing store before creating current-version relations.  P4
  ;; owns explicit offline upgrades; ordinary startup must not mutate an older
  ;; P1 database and then report that it is unsupported.
  (unless (or new-store-p
              (car (sqlite-select
                    e-runtime-store-worker--database
                    "SELECT 1 FROM sqlite_master WHERE type='table' AND name='store_meta'")))
    (signal 'e-runtime-store-schema-too-old
            (list :actual 'legacy-or-unversioned
                  :required e-runtime-store-worker-schema-version
                  :operation 'e-runtime-migration-run)))
  (sqlite-execute
   e-runtime-store-worker--database
   "CREATE TABLE IF NOT EXISTS store_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
  (let ((row (car (sqlite-select
                   e-runtime-store-worker--database
                   "SELECT value FROM store_meta WHERE key='schema_version'"))))
    (when row
      (let ((version
             (string-to-number (e-runtime-store-worker--column row 0))))
        (cond
         ((< version e-runtime-store-worker-schema-version)
          (signal 'e-runtime-store-schema-too-old
                  (list :actual version
                        :required e-runtime-store-worker-schema-version
                        :operation 'e-runtime-store-offline-upgrade)))
         ((> version e-runtime-store-worker-schema-version)
          (signal 'e-runtime-store-schema-too-new
                  (list :actual version
                        :supported e-runtime-store-worker-schema-version)))))))
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY, identity TEXT NOT NULL, checksum TEXT NOT NULL, applied_at REAL NOT NULL)"
         "CREATE TABLE IF NOT EXISTS session_records (session_id TEXT NOT NULL, position INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(session_id, position))"
         "CREATE INDEX IF NOT EXISTS session_records_position ON session_records(session_id, position)"
         "CREATE TABLE IF NOT EXISTS session_checkpoints (session_id TEXT PRIMARY KEY, payload TEXT NOT NULL, revision INTEGER NOT NULL)"
         "CREATE TABLE IF NOT EXISTS catalog_projection (singleton INTEGER PRIMARY KEY CHECK(singleton = 1), payload TEXT NOT NULL, revision INTEGER NOT NULL)"
         "CREATE TABLE IF NOT EXISTS tool_followups (session_id TEXT NOT NULL, call_id TEXT NOT NULL, state TEXT NOT NULL, payload TEXT, revision INTEGER NOT NULL, PRIMARY KEY(session_id, call_id))"
         "CREATE INDEX IF NOT EXISTS tool_followups_session ON tool_followups(session_id, state)"
         "CREATE TABLE IF NOT EXISTS resources (lineage_id TEXT NOT NULL, resource_path TEXT NOT NULL, session_id TEXT NOT NULL, content BLOB NOT NULL, metadata TEXT, created_at REAL NOT NULL, updated_at REAL NOT NULL, expires_at REAL, PRIMARY KEY(lineage_id, resource_path))"
         "CREATE INDEX IF NOT EXISTS resources_session ON resources(session_id)"
         "CREATE INDEX IF NOT EXISTS resources_expiry ON resources(expires_at)"))
    (sqlite-execute e-runtime-store-worker--database statement))
  (e-board-storage-sqlite-worker-initialize
   e-runtime-store-worker--database)
  (e-task-storage-sqlite-worker-initialize
   e-runtime-store-worker--database)
  (e-cron-storage-sqlite-worker-initialize
   e-runtime-store-worker--database)
  (e-voice-storage-sqlite-worker-initialize
   e-runtime-store-worker--database)
  (e-goodnite-storage-sqlite-worker-initialize
   e-runtime-store-worker--database)
  (e-raw-results-storage-sqlite-worker-initialize
   e-runtime-store-worker--database)
  (let ((row (car (sqlite-select e-runtime-store-worker--database
                                 "SELECT value FROM store_meta WHERE key='schema_version'"))))
    (unless row
      (sqlite-execute e-runtime-store-worker--database
                      "INSERT INTO store_meta(key,value) VALUES('schema_version',?)"
                      (vector (number-to-string
                               e-runtime-store-worker-schema-version)))
      (sqlite-execute
       e-runtime-store-worker--database
       "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(?,?,?,?)"
       (vector e-runtime-store-worker-schema-version "new-current-schema"
               (secure-hash 'sha256 "feature87-schema-v4") (float-time))))))

(defun e-runtime-store-worker--open (directory runtime-id)
  "Open DIRECTORY for RUNTIME-ID and return startup status."
  (unless (sqlite-available-p)
    (signal 'e-runtime-store-worker-error (list "SQLite is unavailable")))
  (setq directory (file-name-as-directory (expand-file-name directory))
        e-runtime-store-worker--runtime-id runtime-id
        e-runtime-store-worker--database-file
        (expand-file-name "store.sqlite3" directory)
        e-runtime-store-worker--owner-file
        (expand-file-name "store.sqlite3.owner" directory))
  (let ((new-store-p (not (file-exists-p e-runtime-store-worker--database-file))))
  (make-directory directory t)
  (set-file-modes directory #o700)
  (e-runtime-store-worker--claim-owner)
  (setq e-runtime-store-worker--database
        (sqlite-open e-runtime-store-worker--database-file))
  (sqlite-execute e-runtime-store-worker--database "PRAGMA foreign_keys=ON")
  (sqlite-select e-runtime-store-worker--database "PRAGMA journal_mode=WAL")
  (sqlite-execute e-runtime-store-worker--database "PRAGMA synchronous=NORMAL")
  (sqlite-execute e-runtime-store-worker--database "PRAGMA busy_timeout=2500")
  (e-runtime-store-worker--schema new-store-p)
  (e-runtime-store-worker--permissions)
  (list :schema-version e-runtime-store-worker-schema-version
        :database-file e-runtime-store-worker--database-file
        :runtime-id runtime-id :pid (emacs-pid)
        :journal-mode "wal" :synchronous "normal")))

(defun e-runtime-store-worker--session-position (session-id)
  "Return SESSION-ID's current monotonic record position."
  (e-runtime-store-worker--column
   (car (sqlite-select
         e-runtime-store-worker--database
         "SELECT COALESCE(MAX(position),0) FROM session_records WHERE session_id=?"
         (vector session-id)))
   0))

(defun e-runtime-store-worker--session-append (body)
  "Append BODY's one session record."
  (let* ((session-id (plist-get body :session-id))
         (revision (e-runtime-store-worker--session-position session-id))
         (next (1+ revision))
         (payload (e-runtime-store-worker--sql-value
                   (plist-get body :record))))
    (when (> (string-bytes payload)
             e-runtime-store-worker-session-record-byte-limit)
      (signal 'e-runtime-store-worker-error
              (list "Session record exceeds private limit"
                    (string-bytes payload)
                    e-runtime-store-worker-session-record-byte-limit)))
    (sqlite-execute
     e-runtime-store-worker--database
     "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
     (vector session-id next payload))
    (list :session-id session-id :revision next :position next)))

(defun e-runtime-store-worker--session-append-batch (body)
  "Append BODY's session record batch atomically."
  (let* ((session-id (plist-get body :session-id))
         (revision (e-runtime-store-worker--session-position session-id))
         (position revision))
    (dolist (record (append (plist-get body :records) nil))
      (let ((payload (e-runtime-store-worker--sql-value record)))
        (when (> (string-bytes payload)
                 e-runtime-store-worker-session-record-byte-limit)
          (signal 'e-runtime-store-worker-error
                  (list "Session record exceeds private limit"
                        (string-bytes payload)
                        e-runtime-store-worker-session-record-byte-limit)))
        (cl-incf position)
        (sqlite-execute
         e-runtime-store-worker--database
         "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
         (vector session-id position payload))))
    (list :session-id session-id :revision position
          :first-position (and (> position revision) (1+ revision))
          :last-position position)))

(defun e-runtime-store-worker--checkpoint-put (body)
  "Persist a bounded session checkpoint from BODY."
  (let* ((session-id (plist-get body :session-id))
         (revision (e-runtime-store-worker--session-position session-id)))
    (sqlite-execute
     e-runtime-store-worker--database
     "INSERT INTO session_checkpoints(session_id,payload,revision) VALUES(?,?,?) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload,revision=excluded.revision"
     (vector session-id
             (e-runtime-store-worker--sql-value (plist-get body :value))
             revision))
    (list :session-id session-id :revision revision)))

(defun e-runtime-store-worker--catalog-put (body)
  "Persist catalog projection from BODY."
  (let* ((row (car (sqlite-select
                    e-runtime-store-worker--database
                    "SELECT revision FROM catalog_projection WHERE singleton=1")))
         (revision (1+ (if row (e-runtime-store-worker--column row 0) 0))))
    (sqlite-execute
     e-runtime-store-worker--database
     "INSERT INTO catalog_projection(singleton,payload,revision) VALUES(1,?,?) ON CONFLICT(singleton) DO UPDATE SET payload=excluded.payload,revision=excluded.revision"
     (vector (e-runtime-store-worker--sql-value (plist-get body :value)) revision))
    (list :revision revision)))

(defun e-runtime-store-worker--session-delete (body)
  "Delete one session and its private resources."
  (let ((session-id (plist-get body :session-id)))
    (dolist (table '("session_records" "session_checkpoints" "tool_followups" "resources"))
      (sqlite-execute e-runtime-store-worker--database
                      (format "DELETE FROM %s WHERE session_id=?" table)
                      (vector session-id)))
    (list :session-id session-id :deleted t)))

(defun e-runtime-store-worker--tool-transition (body)
  "Commit one typed tool follow-up transition from BODY."
  (let* ((session-id (plist-get body :session-id))
         (call-id (plist-get body :call-id))
         (state (plist-get body :state))
         (prior (car (sqlite-select
                      e-runtime-store-worker--database
                      "SELECT revision FROM tool_followups WHERE session_id=? AND call_id=?"
                      (vector session-id call-id))))
         (revision (1+ (if prior (e-runtime-store-worker--column prior 0) 0))))
    (sqlite-execute
     e-runtime-store-worker--database
     "INSERT INTO tool_followups(session_id,call_id,state,payload,revision) VALUES(?,?,?,?,?) ON CONFLICT(session_id,call_id) DO UPDATE SET state=excluded.state,payload=excluded.payload,revision=excluded.revision"
     (vector session-id call-id (symbol-name state)
             (and (plist-member body :payload)
                  (e-runtime-store-worker--sql-value (plist-get body :payload)))
             revision))
    (list :session-id session-id :call-id call-id :state state
          :revision revision)))

(defun e-runtime-store-worker--resource-put (body)
  "Commit one resource value from BODY."
  (let* ((content (plist-get body :content))
         (bytes (string-bytes content))
         (limit e-runtime-store-worker-resource-byte-limit)
         (now (float-time)))
    (when (> bytes limit)
      (signal 'e-runtime-store-resource-too-large (list bytes limit)))
    (sqlite-execute
     e-runtime-store-worker--database
     "INSERT INTO resources(lineage_id,resource_path,session_id,content,metadata,created_at,updated_at,expires_at) VALUES(?,?,?,CAST(? AS BLOB),?,?,?,?) ON CONFLICT(lineage_id,resource_path) DO UPDATE SET session_id=excluded.session_id,content=excluded.content,metadata=excluded.metadata,updated_at=excluded.updated_at,expires_at=excluded.expires_at"
     (vector (plist-get body :lineage-id) (plist-get body :path)
             (plist-get body :session-id)
             content
             (and (plist-member body :metadata)
                  (e-runtime-store-worker--sql-value (plist-get body :metadata)))
             now now (plist-get body :expires-at)))
    (list :uri (concat "tmp://" (plist-get body :path)) :bytes bytes
          :limit limit :updated-at now)))

(defun e-runtime-store-worker--resource-delete (body)
  "Delete one resource selected by BODY."
  (let ((count (sqlite-execute
                e-runtime-store-worker--database
                "DELETE FROM resources WHERE lineage_id=? AND resource_path=?"
                (vector (plist-get body :lineage-id) (plist-get body :path)))))
    (list :deleted (> count 0))))

(defun e-runtime-store-worker--resource-delete-lineage (body)
  "Delete BODY's resource lineage."
  (let ((count (sqlite-execute
                e-runtime-store-worker--database
                "DELETE FROM resources WHERE lineage_id=?"
                (vector (plist-get body :lineage-id)))))
    (list :deleted count)))

(defun e-runtime-store-worker--resource-expire (body)
  "Delete resources expired by BODY's timestamp."
  (let ((count (sqlite-execute
                e-runtime-store-worker--database
                "DELETE FROM resources WHERE expires_at IS NOT NULL AND expires_at < ?"
                (vector (or (plist-get body :now) (float-time))))))
    (list :deleted count)))


(defun e-runtime-store-worker--write-dispatch (body)
  "Execute typed write BODY inside the current transaction."
  (pcase (plist-get body :op)
    ('session-append (e-runtime-store-worker--session-append body))
    ('session-append-batch (e-runtime-store-worker--session-append-batch body))
    ('checkpoint-put (e-runtime-store-worker--checkpoint-put body))
    ('catalog-put (e-runtime-store-worker--catalog-put body))
    ('session-delete (e-runtime-store-worker--session-delete body))
    ('tool-transition (e-runtime-store-worker--tool-transition body))
    ('resource-put (e-runtime-store-worker--resource-put body))
    ('resource-delete (e-runtime-store-worker--resource-delete body))
    ('resource-delete-lineage
     (e-runtime-store-worker--resource-delete-lineage body))
    ('resource-expire (e-runtime-store-worker--resource-expire body))
    ((or 'board-create 'board-clear 'board-record-put 'board-routing-put
         'board-pickup-transition 'board-participant-put
         'board-participant-delete 'board-participant-publish
         'board-replay-progress-put 'board-pickup-session-admit)
     (e-board-storage-sqlite-worker-write
      e-runtime-store-worker--database body))
    ((or 'task-queue-open 'task-enqueue 'task-claim 'task-transition
         'task-queue-pause 'task-history-delete 'task-import-legacy-snapshot)
     (e-task-storage-sqlite-worker-write
      e-runtime-store-worker--database body))
    ((or 'cron-register 'cron-claim 'cron-settle 'cron-history-delete)
     (e-cron-storage-sqlite-worker-write
      e-runtime-store-worker--database body))
    ((or 'voice-record 'voice-clear)
     (e-voice-storage-sqlite-worker-write
      e-runtime-store-worker--database body))
    ((or 'goodnite-event-append 'goodnite-checkpoint-ack
         'goodnite-event-cleanup)
     (e-goodnite-storage-sqlite-worker-write
      e-runtime-store-worker--database body))
    ((or 'raw-result-put 'raw-result-delete 'raw-result-expire)
     (e-raw-results-storage-sqlite-worker-write
      e-runtime-store-worker--database body))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown write operation" (plist-get body :op))))))

(defun e-runtime-store-worker--write (request)
  "Execute one transactional write REQUEST and return its committed result."
  (let ((body (plist-get request :body)))
    ;; Modes are a precondition for mutation.  Once COMMIT succeeds, response
    ;; formation is deliberately in-memory and non-fallible.  Transport loss
    ;; fails the client and never causes automatic resubmission.
    (e-runtime-store-worker--permissions)
    (sqlite-execute e-runtime-store-worker--database "BEGIN IMMEDIATE")
    (condition-case err
        (let ((result (e-runtime-store-worker--write-dispatch body)))
          (sqlite-execute e-runtime-store-worker--database "COMMIT")
          result)
      (error
       (ignore-errors
         (sqlite-execute e-runtime-store-worker--database "ROLLBACK"))
       (signal (car err) (cdr err))))))

(defun e-runtime-store-worker--read (body)
  "Execute bounded typed query BODY."
  (pcase (plist-get body :op)
    ('status
     (list :schema-version e-runtime-store-worker-schema-version
           :database-file e-runtime-store-worker--database-file
           :runtime-id e-runtime-store-worker--runtime-id
           :pid (emacs-pid)))
    ('store-integrity
     (let* ((full (and (plist-get body :full) t))
            (pragma (if full "PRAGMA integrity_check" "PRAGMA quick_check"))
            (rows (sqlite-select e-runtime-store-worker--database pragma)))
       (list :kind (if full 'integrity-check 'quick-check)
             :ok (and (= (length rows) 1)
                      (equal (e-runtime-store-worker--column (car rows) 0)
                             "ok"))
             :rows (mapcar (lambda (row)
                             (e-runtime-store-worker--column row 0))
                           rows))))
    ('store-metrics
     (let ((page-size
            (e-runtime-store-worker--column
             (car (sqlite-select e-runtime-store-worker--database
                                 "PRAGMA page_size")) 0))
           (page-count
            (e-runtime-store-worker--column
             (car (sqlite-select e-runtime-store-worker--database
                                 "PRAGMA page_count")) 0)))
       (list :schema-version e-runtime-store-worker-schema-version
             :page-size page-size :page-count page-count
             :database-bytes (* page-size page-count)
             :wal-bytes
             (let ((wal (concat e-runtime-store-worker--database-file "-wal")))
               (if (file-exists-p wal)
                   (file-attribute-size (file-attributes wal))
                 0)))))
    ('store-backup
     (let ((destination (expand-file-name (plist-get body :destination))))
       (when (file-exists-p destination)
         (signal 'file-already-exists (list destination)))
       (make-directory (file-name-directory destination) t)
       (set-file-modes (file-name-directory destination) #o700)
       (sqlite-execute e-runtime-store-worker--database
                       "VACUUM INTO ?" (vector destination))
       (set-file-modes destination #o600)
       (let ((backup (sqlite-open destination)))
         (unwind-protect
             (let ((check
                    (e-runtime-store-worker--column
                     (car (sqlite-select backup "PRAGMA quick_check")) 0)))
               (unless (equal check "ok")
                 (signal 'e-runtime-store-worker-error
                         (list "Backup verification failed" check))))
           (sqlite-close backup)))
       (list :destination destination
             :bytes (file-attribute-size (file-attributes destination))
             :verified t)))
    ('session-header
     (let* ((session-id (plist-get body :session-id))
            (row (car (sqlite-select
                       e-runtime-store-worker--database
                       "SELECT COUNT(*),COALESCE(SUM(LENGTH(payload)),0),COALESCE(MAX(position),0) FROM session_records WHERE session_id=?"
                       (vector session-id)))))
       (list :session-id session-id :present (> (e-runtime-store-worker--column row 0) 0)
             :record-count (e-runtime-store-worker--column row 0) :byte-size (e-runtime-store-worker--column row 1)
             :revision (e-runtime-store-worker--column row 2) :reference session-id)))
    ('session-ids
     (mapcar (lambda (row) (e-runtime-store-worker--column row 0))
             (sqlite-select e-runtime-store-worker--database
                            "SELECT DISTINCT session_id FROM session_records ORDER BY session_id")))
    ('session-record-page
     (let* ((limit (min 1024 (max 1 (or (plist-get body :limit) 256))))
            (rows (sqlite-select
                   e-runtime-store-worker--database
                   "SELECT position,payload,LENGTH(payload) FROM session_records WHERE session_id=? AND position>? ORDER BY position LIMIT ?"
                   (vector (plist-get body :session-id)
                           (or (plist-get body :after) 0) limit)))
            (bytes 0) selected truncated)
       (catch 'full
         (dolist (row rows)
           (let ((row-bytes (e-runtime-store-worker--column row 2)))
             (when (> row-bytes e-runtime-store-worker-session-record-byte-limit)
               (signal 'e-runtime-store-worker-error
                       (list "Session record exceeds page result budget"
                             (e-runtime-store-worker--column row 0)
                             row-bytes)))
             (when (and selected
                        (> (+ bytes row-bytes)
                           e-runtime-store-worker-session-page-byte-limit))
               (setq truncated t)
               (throw 'full nil))
             (cl-incf bytes row-bytes)
             (push (list :position
                         (e-runtime-store-worker--column row 0)
                         :value
                         (e-runtime-store-worker--value
                          (e-runtime-store-worker--column row 1)))
                   selected))))
       (setq selected (nreverse selected))
       (list :records selected
             :next (and selected
                        (or truncated (= (length rows) limit))
                        (plist-get (car (last selected)) :position)))))
    ('checkpoint-get
     (when-let* ((row (car (sqlite-select
                            e-runtime-store-worker--database
                            "SELECT payload,revision FROM session_checkpoints WHERE session_id=?"
                            (vector (plist-get body :session-id))))))
       (list :value (e-runtime-store-worker--value (e-runtime-store-worker--column row 0))
             :revision (e-runtime-store-worker--column row 1))))
    ('catalog-get
     (when-let* ((row (car (sqlite-select e-runtime-store-worker--database
                                          "SELECT payload,revision FROM catalog_projection WHERE singleton=1"))))
       (list :value (e-runtime-store-worker--value (e-runtime-store-worker--column row 0))
             :revision (e-runtime-store-worker--column row 1))))
    ('tool-list
     (mapcar
      (lambda (row)
        (list :call-id (e-runtime-store-worker--column row 0) :state (intern (e-runtime-store-worker--column row 1))
              :payload (e-runtime-store-worker--value (e-runtime-store-worker--column row 2))
              :revision (e-runtime-store-worker--column row 3)))
      (sqlite-select
       e-runtime-store-worker--database
       "SELECT call_id,state,payload,revision FROM tool_followups WHERE session_id=? ORDER BY call_id LIMIT ?"
       (vector (plist-get body :session-id)
               (min 1024 (max 1 (or (plist-get body :limit) 256)))))))
    ((or 'board-get 'board-list 'board-record-page 'board-routing-get
         'board-pickup-list 'board-participant-list
         'board-replay-progress-get)
     (e-board-storage-sqlite-worker-read
      e-runtime-store-worker--database body))
    ('task-snapshot
     (e-task-storage-sqlite-worker-read
      e-runtime-store-worker--database body))
    ('cron-cadence
     (e-cron-storage-sqlite-worker-read
      e-runtime-store-worker--database body))
    ('voice-list
     (e-voice-storage-sqlite-worker-read
      e-runtime-store-worker--database body))
    ('goodnite-event-page
     (e-goodnite-storage-sqlite-worker-read
      e-runtime-store-worker--database body))
    ('raw-result-read
     (e-raw-results-storage-sqlite-worker-read
      e-runtime-store-worker--database body))
    ('resource-get
     (when-let* ((row (car (sqlite-select
                            e-runtime-store-worker--database
                            "SELECT LENGTH(CAST(content AS TEXT)),LENGTH(content),metadata,updated_at,expires_at FROM resources WHERE lineage_id=? AND resource_path=? AND (expires_at IS NULL OR expires_at>=?)"
                            (vector (plist-get body :lineage-id)
                                    (plist-get body :path)
                                    (or (plist-get body :now) (float-time)))))))
       (list :characters (e-runtime-store-worker--column row 0)
             :bytes (e-runtime-store-worker--column row 1)
             :metadata (e-runtime-store-worker--value
                        (e-runtime-store-worker--column row 2))
             :updated-at (e-runtime-store-worker--column row 3)
             :expires-at (e-runtime-store-worker--column row 4))))
    ('resource-read
     (when-let* ((row (car (sqlite-select
                            e-runtime-store-worker--database
                            "SELECT SUBSTR(CAST(content AS TEXT),?,?),LENGTH(CAST(content AS TEXT)),LENGTH(content) FROM resources WHERE lineage_id=? AND resource_path=? AND (expires_at IS NULL OR expires_at>=?)"
                            (vector (1+ (max 0 (or (plist-get body :offset) 0)))
                                    (min 4096
                                         (max 1 (or (plist-get body :limit)
                                                    4096)))
                                    (plist-get body :lineage-id)
                                    (plist-get body :path)
                                    (or (plist-get body :now) (float-time)))))))
       (let* ((content (e-runtime-store-worker--column row 0))
              (total (e-runtime-store-worker--column row 1))
              (offset (max 0 (or (plist-get body :offset) 0)))
              (limit (min 4096
                          (max 1 (or (plist-get body :limit) (* 256 1024)))))
              (end (min total (+ offset limit))))
         (list :content content
               :next (and (< end total) end)
               :characters total
               :bytes (e-runtime-store-worker--column row 2)))))
    ('resource-list
     (let ((rows (sqlite-select
                  e-runtime-store-worker--database
                  "SELECT resource_path,LENGTH(CAST(content AS BLOB)),metadata,updated_at,expires_at FROM resources WHERE lineage_id=? AND (expires_at IS NULL OR expires_at>=?) ORDER BY resource_path LIMIT ?"
                  (vector (plist-get body :lineage-id)
                          (or (plist-get body :now) (float-time))
                          (min 4096 (max 1 (or (plist-get body :limit) 1024)))))))
       (mapcar (lambda (row)
                 (list :path (e-runtime-store-worker--column row 0)
                       :bytes (e-runtime-store-worker--column row 1)
                       :metadata (e-runtime-store-worker--value
                                  (e-runtime-store-worker--column row 2))
                       :updated-at (e-runtime-store-worker--column row 3)
                       :expires-at (e-runtime-store-worker--column row 4)))
               rows)))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown read operation" (plist-get body :op))))))

(defun e-runtime-store-worker--handle (request)
  "Handle one decoded REQUEST."
  (pcase (plist-get request :kind)
    ('open (e-runtime-store-worker--open (plist-get request :directory)
                                         (plist-get request :runtime-id)))
    ('write (e-runtime-store-worker--write request))
    ('read (e-runtime-store-worker--read (plist-get request :body)))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown request kind" (plist-get request :kind))))))

(defun e-runtime-store-worker-main ()
  "Run the newline-framed worker protocol on standard input/output."
  (let ((standard-output t)
        line)
    (unwind-protect
        (while (setq line (condition-case nil (read-string "")
                            (end-of-file nil)))
          (unless (string-empty-p line)
            (let* ((request (condition-case err
                                (e-runtime-store-worker--unpack line)
                              (error (list :decode-error err))))
                   (response
                    (condition-case err
                        (list :id (plist-get request :id) :ok t
                              :result (e-runtime-store-worker--handle request))
                      (error
                       (list :id (plist-get request :id) :ok nil
                             :error-symbol (car err)
                             :error-data (cdr err))))))
              (princ (e-runtime-store-worker--pack response))
              (terpri)
              (flush-standard-output))))
      (when e-runtime-store-worker--database
        (sqlite-close e-runtime-store-worker--database))
      (e-runtime-store-worker--release-owner))))

(provide 'e-runtime-store-worker)

;;; e-runtime-store-worker.el ends here

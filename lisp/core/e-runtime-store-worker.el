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

(unless (get 'e-runtime-store-worker-error 'error-conditions)
  (define-error 'e-runtime-store-worker-error "Runtime store worker error"))
(unless (get 'e-runtime-store-response-too-large 'error-conditions)
  (define-error 'e-runtime-store-response-too-large
    "Runtime store read response exceeds its transport limit"
    'e-runtime-store-worker-error))
(require 'e-runtime-store-ownership)
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
(defconst e-runtime-store-worker-checkpoint-canonical-byte-limit
  e-runtime-store-codec-checkpoint-canonical-byte-limit
  "Maximum canonical bytes in one rebuildable checkpoint value.")
(defconst e-runtime-store-worker-session-id-page-row-limit 256
  "Maximum durable identities returned by one session-id page.")
(defconst e-runtime-store-worker-session-id-page-byte-limit (* 64 1024)
  "Maximum raw UTF-8 identity bytes returned by one session-id page.")

(defvar e-runtime-store-worker--database nil)
(defvar e-runtime-store-worker--database-file nil)
(defvar e-runtime-store-worker--runtime-id nil)
(defvar e-runtime-store-worker--ownership nil)

(defun e-runtime-store-worker--column (row index)
  "Return INDEX from SQLite ROW across supported Emacs return shapes."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-runtime-store-worker--pack (value)
  "Return bounded VALUE as ASCII transport text without its delimiter."
  (let* ((canonical
          (e-runtime-store-codec-encode-bounded
           value e-runtime-store-codec-protocol-canonical-byte-limit))
         (wire-bytes (e-runtime-store-codec-wire-byte-count
                      (string-bytes canonical))))
    (when (> wire-bytes e-runtime-store-codec-protocol-wire-byte-limit)
      (signal 'e-runtime-store-codec-too-large
              (list "Protocol wire frame exceeds byte limit"
                    :domain 'wire
                    :limit e-runtime-store-codec-protocol-wire-byte-limit
                    :canonical-bytes (string-bytes canonical))))
    (base64-encode-string canonical t)))

(defun e-runtime-store-worker--unpack (text)
  "Return exact value encoded by ASCII TEXT."
  (when (> (1+ (string-bytes text))
           e-runtime-store-codec-protocol-wire-byte-limit)
    (signal 'e-runtime-store-codec-too-large
            (list "Protocol wire frame exceeds byte limit"
                  :domain 'wire
                  :limit e-runtime-store-codec-protocol-wire-byte-limit
                  :wire-bytes (1+ (string-bytes text)))))
  (let ((canonical (base64-decode-string text)))
    (when (> (string-bytes canonical)
             e-runtime-store-codec-protocol-canonical-byte-limit)
      (signal 'e-runtime-store-codec-too-large
              (list "Protocol canonical frame exceeds byte limit"
                    :domain 'canonical
                    :limit e-runtime-store-codec-protocol-canonical-byte-limit
                    :canonical-bytes (string-bytes canonical))))
    (e-runtime-store-codec-decode canonical)))

(defun e-runtime-store-worker--sql-value (value)
  "Return VALUE encoded for a SQLite TEXT field."
  ;; SQLite payloads are not protocol frames.  Their operation-specific
  ;; storage limits are checked by the caller before this exact encoding.
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-runtime-store-worker--value (text)
  "Return exact value stored in SQLite TEXT."
  (and text (e-runtime-store-worker--unpack text)))

(defun e-runtime-store-worker--permissions ()
  "Apply restrictive modes to current SQLite and ownership files."
  (dolist (file (list e-runtime-store-worker--database-file
                      (concat e-runtime-store-worker--database-file "-wal")
                      (concat e-runtime-store-worker--database-file "-shm")
                      (and e-runtime-store-worker--ownership
                           (e-runtime-store-ownership-claim--metadata-file
                            e-runtime-store-worker--ownership))))
    (when (file-exists-p file)
      (set-file-modes file #o600))))

(defun e-runtime-store-worker--close ()
  "Close SQLite before releasing this worker's runtime-directory claim."
  (unwind-protect
      (when e-runtime-store-worker--database
        (sqlite-close e-runtime-store-worker--database))
    (setq e-runtime-store-worker--database nil)
    (when e-runtime-store-worker--ownership
      (unwind-protect
          (e-runtime-store-ownership-release e-runtime-store-worker--ownership)
        (setq e-runtime-store-worker--ownership nil)))))

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
        (expand-file-name "store.sqlite3" directory))
  (make-directory directory t)
  (set-file-modes directory #o700)
  ;; The file lock is the authority.  Do not inspect SQLite or its old owner
  ;; metadata until this process holds the shared ordinary/offline claim.
  (setq e-runtime-store-worker--ownership
        (e-runtime-store-ownership-acquire
         e-runtime-store-worker--database-file runtime-id 'ordinary))
  (let ((opened nil))
    (unwind-protect
        (let ((new-store-p
               (not (file-exists-p e-runtime-store-worker--database-file))))
          (setq e-runtime-store-worker--database
                (sqlite-open e-runtime-store-worker--database-file))
          (sqlite-execute e-runtime-store-worker--database "PRAGMA foreign_keys=ON")
          (sqlite-select e-runtime-store-worker--database "PRAGMA journal_mode=WAL")
          (sqlite-execute e-runtime-store-worker--database "PRAGMA synchronous=NORMAL")
          (sqlite-execute e-runtime-store-worker--database "PRAGMA busy_timeout=2500")
          (e-runtime-store-worker--schema new-store-p)
          (e-runtime-store-worker--permissions)
          (setq opened t)
          (list :schema-version e-runtime-store-worker-schema-version
                :database-file e-runtime-store-worker--database-file
                :runtime-id runtime-id :pid (emacs-pid)
                :journal-mode "wal" :synchronous "normal"))
      (unless opened
        (e-runtime-store-worker--close)))))

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
         (revision (e-runtime-store-worker--session-position session-id))
         ;; This physical backstop protects direct protocol callers.  The
         ;; session adapter preflights the same value before queue admission so
         ;; ordinary oversized checkpoints are omitted rather than submitted.
         (canonical (e-runtime-store-codec-encode-bounded
                     (plist-get body :value)
                     e-runtime-store-worker-checkpoint-canonical-byte-limit)))
    (sqlite-execute
     e-runtime-store-worker--database
     "INSERT INTO session_checkpoints(session_id,payload,revision) VALUES(?,?,?) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload,revision=excluded.revision"
     (vector session-id
             (base64-encode-string canonical t)
             revision))
    (list :session-id session-id :revision revision)))

(defun e-runtime-store-worker--catalog-put (body)
  "Persist catalog projection from BODY."
  (let* ((row (car (sqlite-select
                    e-runtime-store-worker--database
                    "SELECT revision FROM catalog_projection WHERE singleton=1")))
         (revision (1+ (if row (e-runtime-store-worker--column row 0) 0)))
         ;; Catalog is a rebuildable projection.  The session adapter checks
         ;; this before transport; the worker repeats the cap for direct
         ;; protocol callers so no oversized catalog payload reaches SQLite.
         (canonical (e-runtime-store-codec-encode-bounded
                     (plist-get body :value)
                     e-runtime-store-codec-catalog-canonical-byte-limit)))
    (sqlite-execute
     e-runtime-store-worker--database
     "INSERT INTO catalog_projection(singleton,payload,revision) VALUES(1,?,?) ON CONFLICT(singleton) DO UPDATE SET payload=excluded.payload,revision=excluded.revision"
     (vector (base64-encode-string canonical t) revision))
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

(defun e-runtime-store-worker--base64-canonical-byte-count
    (sqlite-text-bytes suffix)
  "Return decoded canonical bytes for base64 SQLite TEXT metadata.

SUFFIX contains at most the final two ASCII characters.  Nil means malformed
or non-base64-sized metadata and is deliberately not considered usable for a
checkpoint payload."
  (when (and (integerp sqlite-text-bytes)
             (> sqlite-text-bytes 0)
             (= (% sqlite-text-bytes 4) 0)
             (stringp suffix))
    (let ((padding (cond
                    ((string-suffix-p "==" suffix) 2)
                    ((string-suffix-p "=" suffix) 1)
                    (t 0))))
      (- (* 3 (/ sqlite-text-bytes 4)) padding))))

(defun e-runtime-store-worker--checkpoint-status (session-id)
  "Return metadata-only bounded checkpoint status for SESSION-ID."
  (when-let* ((row (car (sqlite-select
                          e-runtime-store-worker--database
                          "SELECT revision,LENGTH(CAST(payload AS BLOB)),SUBSTR(payload,-2) FROM session_checkpoints WHERE session_id=?"
                          (vector session-id)))))
    (let* ((revision (e-runtime-store-worker--column row 0))
           (sqlite-text-bytes (e-runtime-store-worker--column row 1))
           (canonical-bytes
            (e-runtime-store-worker--base64-canonical-byte-count
             sqlite-text-bytes (e-runtime-store-worker--column row 2))))
      (list :present t :revision revision
            :sqlite-text-bytes sqlite-text-bytes
            :canonical-bytes canonical-bytes
            :usable (and canonical-bytes
                         (<= canonical-bytes
                             e-runtime-store-worker-checkpoint-canonical-byte-limit))))))

(defun e-runtime-store-worker--checkpoint-read (session-id)
  "Return SESSION-ID's safe checkpoint payload after metadata preflight.

No oversized legacy payload is selected into worker memory or crosses the
protocol.  Its canonical journal remains available for replay from zero."
  (when-let* ((status (e-runtime-store-worker--checkpoint-status session-id)))
    (when (plist-get status :usable)
      (when-let* ((row (car (sqlite-select
                             e-runtime-store-worker--database
                             "SELECT payload FROM session_checkpoints WHERE session_id=?"
                             (vector session-id)))))
        (list :value (e-runtime-store-worker--value
                      (e-runtime-store-worker--column row 0))
              :revision (plist-get status :revision))))))

(defun e-runtime-store-worker--session-id-page (body)
  "Return one cursor, row, and byte bounded durable session identity page."
  (let* ((cursor (plist-get body :cursor))
         (limit (min e-runtime-store-worker-session-id-page-row-limit
                     (max 1 (or (plist-get body :limit)
                               e-runtime-store-worker-session-id-page-row-limit))))
         (query-limit (1+ limit)))
    (unless (or (null cursor) (stringp cursor))
      (signal 'e-runtime-store-worker-error
              (list "Session identity cursor must be a string" cursor)))
    (let* ((rows
            (if cursor
                (sqlite-select
                 e-runtime-store-worker--database
                 "SELECT DISTINCT session_id FROM session_records WHERE session_id>? ORDER BY session_id LIMIT ?"
                 (vector cursor query-limit))
              (sqlite-select
               e-runtime-store-worker--database
               "SELECT DISTINCT session_id FROM session_records ORDER BY session_id LIMIT ?"
               (vector query-limit))))
           (bytes 0) selected truncated (row-count 0))
      (catch 'full
        (dolist (row rows)
          ;; The extra query row is lookahead only.  It establishes whether a
          ;; full row-limited page has a successor without becoming a 257th
          ;; result in a declared 256-row page.
          (when (>= row-count limit)
            (setq truncated t)
            (throw 'full nil))
          (let* ((session-id (e-runtime-store-worker--column row 0))
                 (row-bytes (string-bytes session-id)))
            (when (> row-bytes e-runtime-store-worker-session-id-page-byte-limit)
              (signal 'e-runtime-store-worker-error
                      (list "Session identity exceeds page byte budget"
                            session-id row-bytes
                            e-runtime-store-worker-session-id-page-byte-limit)))
            (when (and selected
                       (> (+ bytes row-bytes)
                          e-runtime-store-worker-session-id-page-byte-limit))
              (setq truncated t)
              (throw 'full nil))
            (cl-incf bytes row-bytes)
            (cl-incf row-count)
            (push session-id selected))))
      (setq selected (nreverse selected))
      (list :ids selected
            :next (and selected
                       (or truncated (= (length rows) query-limit))
                       (car (last selected)))
            :byte-count bytes
            :row-limit limit
            :byte-limit e-runtime-store-worker-session-id-page-byte-limit))))

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
    ('session-id-page
     (e-runtime-store-worker--session-id-page body))
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
     (e-runtime-store-worker--checkpoint-status (plist-get body :session-id)))
    ('checkpoint-read
     (e-runtime-store-worker--checkpoint-read (plist-get body :session-id)))
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

(defun e-runtime-store-worker--response (request)
  "Return one correlated success or typed-error response for REQUEST."
  (condition-case err
      (list :id (plist-get request :id) :ok t
            :result (e-runtime-store-worker--handle request))
    (error
     (list :id (plist-get request :id) :ok nil
           :error-symbol (car err) :error-data (cdr err)))))

(defun e-runtime-store-worker--oversized-read-response (request cause)
  "Return the small correlated read-overflow response for REQUEST and CAUSE."
  (list :id (plist-get request :id) :ok nil
        :error-symbol 'e-runtime-store-response-too-large
        :error-data
        (list "Runtime-store read response exceeds transport limit"
              :operation (plist-get (plist-get request :body) :op)
              :kind 'read :request-id (plist-get request :id)
              :cause (car-safe cause)
              :canonical-limit e-runtime-store-codec-protocol-canonical-byte-limit
              :wire-limit e-runtime-store-codec-protocol-wire-byte-limit)))

(defun e-runtime-store-worker--emit-response (request response)
  "Emit RESPONSE for REQUEST while preserving write acknowledgement ambiguity.

An oversized ordinary read has no durable ambiguity, so its result is replaced
with a small correlated typed error.  A write may have committed before its
acknowledgement became unencodable; terminate the worker instead of making it
look retry-safe to the parent."
  (condition-case err
      (progn
        (princ (e-runtime-store-worker--pack response))
        (terpri)
        (flush-standard-output))
    (e-runtime-store-codec-too-large
     (if (eq (plist-get request :kind) 'read)
         (let ((fallback
                (e-runtime-store-worker--oversized-read-response request err)))
           ;; The fallback contains only fixed fields plus the parent-generated
           ;; request identity, so a second overflow is a protocol fatal error.
           (princ (e-runtime-store-worker--pack fallback))
           (terpri)
           (flush-standard-output))
       (signal (car err) (cdr err))))))

(defun e-runtime-store-worker-main ()
  "Run the newline-framed worker protocol on standard input/output."
  (let ((standard-output t)
        line)
    (unwind-protect
        (while (setq line (condition-case nil (read-string "")
                            (end-of-file nil)))
          ;; Every delimiter terminates one request.  A blank frame is corrupt,
          ;; not an idle keepalive: let the normal decode-error response reach
          ;; the parent, which then freezes with its active request as cause.
          (let* ((request (condition-case err
                              (e-runtime-store-worker--unpack line)
                            (error (list :decode-error err))))
                 (response (e-runtime-store-worker--response request)))
            (e-runtime-store-worker--emit-response request response)))
      (e-runtime-store-worker--close))))

(provide 'e-runtime-store-worker)

;;; e-runtime-store-worker.el ends here

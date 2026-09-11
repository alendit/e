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
(define-error 'e-runtime-store-parent-active
  "Runtime store predecessor parent is still live"
  'e-runtime-store-worker-error)

(require 'e-runtime-store-session-worker)
(require 'e-board-sqlite-worker)
(require 'e-cron-storage-sqlite-worker)
(require 'e-goodnite-storage-sqlite-worker)
(require 'e-raw-results-storage-sqlite-worker)
(require 'e-task-storage-sqlite-worker)
(require 'e-voice-storage-sqlite-worker)

(defconst e-runtime-store-worker-schema-version 7)
(defconst e-runtime-store-worker--v5-schema-checksum "feature92-schema-v5")
(defconst e-runtime-store-worker--v6-schema-checksum "feature92-schema-v6")
(defconst e-runtime-store-worker--v7-schema-checksum
  "feature92-schema-v7-process-report-projection")
(defconst e-runtime-store-worker-resource-byte-limit (* 16 1024 1024)
  "Private one-BLOB resource limit; deliberately above ordinary tool details.")

(defvar e-runtime-store-worker--database nil)
(defvar e-runtime-store-worker--database-file nil)
(defvar e-runtime-store-worker--runtime-id nil)
(defvar e-runtime-store-worker--ownership nil)
(defvar e-runtime-store-worker--access-mode 'read-write)
(defvar e-runtime-store-worker--borrow-authorization nil
  "One private parent-pipe authorization awaiting one borrowed open.")

(defun e-runtime-store-worker--test-fault (point &optional request)
  "Terminate at private test POINT when the one-shot worker seam requests it.
The seam is inert without its explicitly named environment variables and is
consumed through a marker file shared with the replacement subprocess."
  (when (and (equal (getenv "E_RUNTIME_STORE_TEST_FAULT") point)
             (or (not request)
                 (pcase point
                   ("after-response-formation"
                    (memq (plist-get request :kind) '(write close)))
                   (_ t)))
             (let ((operation (getenv "E_RUNTIME_STORE_TEST_FAULT_OPERATION")))
               (or (not operation)
                   (and request
                        (equal operation
                               (format "%s"
                                       (plist-get (plist-get request :body) :op)))))))
    (let ((marker (getenv "E_RUNTIME_STORE_TEST_FAULT_ONCE_FILE")))
      (when (or (not marker) (not (file-exists-p marker)))
        (when marker (write-region "used" nil marker nil 'silent))
        (kill-emacs 70)))))

(defun e-runtime-store-worker--test-stall (request)
  "Wait at REQUEST's private file-controlled graphical test seam.
The seam is inert unless `E_RUNTIME_STORE_TEST_STALL_DIRECTORY' names a
directory containing OPERATION.hold.  It writes OPERATION.ready, then waits
until OPERATION.release exists.  Only this disposable worker is blocked."
  (when-let* ((directory (getenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY"))
              (operation (or (and (eq (plist-get request :kind) 'open) 'open)
                             (plist-get (plist-get request :body) :op))))
    (let* ((name (format "%s" operation))
           (hold (expand-file-name (concat name ".hold") directory))
           (ready (expand-file-name (concat name ".ready") directory))
           (release (expand-file-name (concat name ".release") directory)))
      (when (file-exists-p hold)
        (write-region "ready" nil ready nil 'silent)
        (while (not (file-exists-p release))
          (sleep-for 0.01))))))

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

(defun e-runtime-store-worker--bounded-receipt-result (request value)
  "Encode VALUE only when REQUEST's complete success response fits pre-COMMIT.

Receipt storage holds just VALUE, but the parent can acknowledge it only as
the correlated `:id', `:ok', and `:result' protocol response.  Validate that
complete wire authority before the domain transaction commits, then retain the
bounded result payload for idempotent replay."
  (let ((limit e-runtime-store-codec-protocol-canonical-byte-limit))
    ;; Use the exact production packer, not a parallel estimate: it proves
    ;; both canonical and base64 wire limits for this request's success frame.
    (e-runtime-store-worker--pack
     (list :id (plist-get request :id) :ok t :result value))
    (base64-encode-string
     (e-runtime-store-codec-encode-bounded value limit)
     t)))

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
    (when (and file (file-exists-p file))
      (set-file-modes file #o600))))

(defun e-runtime-store-worker--close ()
  "Close SQLite before releasing this worker's runtime-directory claim."
  (unwind-protect
      (when e-runtime-store-worker--database
        (sqlite-close e-runtime-store-worker--database))
    (setq e-runtime-store-worker--database nil
          e-runtime-store-worker--borrow-authorization nil)
    (when e-runtime-store-worker--ownership
      (unwind-protect
          (e-runtime-store-ownership-release e-runtime-store-worker--ownership)
        (setq e-runtime-store-worker--ownership nil)))))

(defun e-runtime-store-worker--initialize-common-schema (database)
  "Create the current generic runtime-store relations on DATABASE.

The offline upgrader uses this after it has installed the v6 session
relations.  Keeping the generic envelope here prevents the operator-only
migration from acquiring a second, subtly different copy of the transport
schema; this helper does not inspect or change `store_meta'."
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY, identity TEXT NOT NULL, checksum TEXT NOT NULL, applied_at REAL NOT NULL)"
         "CREATE TABLE IF NOT EXISTS tool_followups (session_id TEXT NOT NULL, call_id TEXT NOT NULL, state TEXT NOT NULL, payload TEXT, revision INTEGER NOT NULL, PRIMARY KEY(session_id, call_id))"
         "CREATE INDEX IF NOT EXISTS tool_followups_session ON tool_followups(session_id, state)"
         "CREATE TABLE IF NOT EXISTS resources (lineage_id TEXT NOT NULL, resource_path TEXT NOT NULL, session_id TEXT NOT NULL, content BLOB NOT NULL, metadata TEXT, created_at REAL NOT NULL, updated_at REAL NOT NULL, expires_at REAL, PRIMARY KEY(lineage_id, resource_path))"
         "CREATE INDEX IF NOT EXISTS resources_session ON resources(session_id)"
         "CREATE INDEX IF NOT EXISTS resources_expiry ON resources(expires_at)"
         ;; A receipt is deliberately generic: domain writers remain unaware
         ;; of transport acknowledgement loss.  Its row is committed with the
         ;; domain mutation, so a replacement worker can distinguish no commit
         ;; from a committed-but-unobserved response.
         "CREATE TABLE IF NOT EXISTS runtime_store_receipts (runtime_id TEXT NOT NULL, request_id TEXT NOT NULL, fingerprint TEXT NOT NULL, result TEXT NOT NULL, write_prefix INTEGER NOT NULL, PRIMARY KEY(runtime_id, request_id))"
         "CREATE INDEX IF NOT EXISTS runtime_store_receipts_watermark ON runtime_store_receipts(runtime_id, write_prefix)"
         "CREATE TABLE IF NOT EXISTS runtime_store_state (singleton INTEGER PRIMARY KEY CHECK(singleton = 1), runtime_id TEXT NOT NULL, parent_boot TEXT NOT NULL, parent_pid INTEGER NOT NULL, parent_process_start TEXT NOT NULL, acknowledged_prefix INTEGER NOT NULL DEFAULT 0, retired INTEGER NOT NULL DEFAULT 0, retirement_request_id TEXT, retirement_fingerprint TEXT, retirement_result TEXT)"))
    (sqlite-execute database statement)))

(defun e-runtime-store-worker--initialize-domain-schema
    (database &optional schema-version)
  "Create all current domain relations on DATABASE.

Each domain worker owns its own physical mapping; this function only keeps
the generic worker's current composition order in one reusable seam for the
normal worker and the stopped-store upgrader."
  (if (and schema-version (< schema-version 7))
      (e-runtime-store-session-worker-initialize-v6 database)
    (e-runtime-store-session-worker-initialize database))
  (e-board-sqlite-worker-initialize database)
  (e-task-storage-sqlite-worker-initialize database)
  (e-cron-storage-sqlite-worker-initialize database)
  (e-voice-storage-sqlite-worker-initialize database)
  (e-goodnite-storage-sqlite-worker-initialize database)
  (e-raw-results-storage-sqlite-worker-initialize database))

(defun e-runtime-store-worker--invalid-current-schema (reason &rest data)
  "Reject the current schema for explicit offline repair due to REASON."
  (signal 'e-runtime-store-schema-too-old
          (append (list :actual 'malformed-v7
                        :required e-runtime-store-worker-schema-version
                        :operation 'e-runtime-store-offline-upgrade
                        :reason reason)
                  data)))

(defun e-runtime-store-worker--migration-row (version)
  "Return VERSION's migration identity/checksum pair, or nil."
  (let ((row
         (car (sqlite-select
               e-runtime-store-worker--database
               "SELECT identity,checksum FROM schema_migrations WHERE version=?"
               (vector version)))))
    (and row
         (list (e-runtime-store-worker--column row 0)
               (e-runtime-store-worker--column row 1)))))

(defun e-runtime-store-worker--verify-v7-lineage ()
  "Verify the exact recognized migration lineage for current schema v7."
  (condition-case err
      (let* ((v5 (e-runtime-store-worker--migration-row 5))
             (v6 (e-runtime-store-worker--migration-row 6))
             (v7 (e-runtime-store-worker--migration-row 7))
             (v5-checksum
              (secure-hash 'sha256 e-runtime-store-worker--v5-schema-checksum))
             (v6-checksum
              (secure-hash 'sha256 e-runtime-store-worker--v6-schema-checksum))
             (v7-checksum
              (secure-hash 'sha256 e-runtime-store-worker--v7-schema-checksum)))
        (cond
         ((equal v7 (list "new-current-schema" v7-checksum))
          (when (or v5 v6)
            (e-runtime-store-worker--invalid-current-schema
             'unexpected-fresh-v7-predecessor)))
         ((equal v7
                 (list "feature92-v6-to-v7-process-report-projection"
                       v7-checksum))
          (cond
           ((equal v6 (list "new-current-schema" v6-checksum))
            (when v5
              (e-runtime-store-worker--invalid-current-schema
               'unexpected-fresh-v6-predecessor)))
           ((equal v6
                   (list "feature92-v5-to-v6-explicit-upgrade" v6-checksum))
            (unless (or
                     (equal v5 (list "feature92-direct-v5-source" v5-checksum))
                     (equal v5
                            (list "feature92-v4-to-v5-explicit-upgrade"
                                  v5-checksum)))
              (e-runtime-store-worker--invalid-current-schema
               'invalid-v5-predecessor)))
           (t
            (e-runtime-store-worker--invalid-current-schema
             'invalid-v6-predecessor))))
         (t
          (e-runtime-store-worker--invalid-current-schema
           'invalid-v7-lineage))))
    (sqlite-error
     (e-runtime-store-worker--invalid-current-schema
      'missing-migration-relation :cause (car (cdr err)))))
  t)

(defun e-runtime-store-worker--verify-current-schema ()
  "Verify existing current storage without executing schema initializers."
  (unless (car (sqlite-select
                e-runtime-store-worker--database
                "SELECT 1 FROM sqlite_master WHERE type='table' AND name='store_meta'"))
    (signal 'e-runtime-store-schema-too-old
            (list :actual 'legacy-or-unversioned
                  :required e-runtime-store-worker-schema-version
                  :operation 'e-runtime-migration-run)))
  (let* ((row (car (sqlite-select
                    e-runtime-store-worker--database
                    "SELECT value FROM store_meta WHERE key='schema_version'")))
         (version (and row
                       (string-to-number
                        (e-runtime-store-worker--column row 0)))))
    (cond
     ((null version)
      (signal 'e-runtime-store-schema-too-old
              (list :actual 'unversioned
                    :required e-runtime-store-worker-schema-version
                    :operation 'e-runtime-store-offline-upgrade)))
     ((< version e-runtime-store-worker-schema-version)
      (signal 'e-runtime-store-schema-too-old
              (list :actual version
                    :required e-runtime-store-worker-schema-version
                    :operation 'e-runtime-store-offline-upgrade)))
     ((> version e-runtime-store-worker-schema-version)
      (signal 'e-runtime-store-schema-too-new
              (list :actual version
                    :supported e-runtime-store-worker-schema-version)))))
  (e-runtime-store-worker--verify-v7-lineage)
  (condition-case err
      (e-runtime-store-session-worker-verify-process-report-projection-schema
       e-runtime-store-worker--database)
    (e-runtime-store-worker-error
     (e-runtime-store-worker--invalid-current-schema
      'invalid-process-report-projection
      :detail (car (cdr err)) :index (plist-get (cddr err) :index))))
  t)

(defun e-runtime-store-worker--schema (new-store-p)
  "Initialize a genuinely new store; verify existing stores read-only."
  (if (not new-store-p)
      (e-runtime-store-worker--verify-current-schema)
    (sqlite-execute
     e-runtime-store-worker--database
     "CREATE TABLE store_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
    (e-runtime-store-worker--initialize-common-schema
     e-runtime-store-worker--database)
    (e-runtime-store-worker--initialize-domain-schema
     e-runtime-store-worker--database)
    (sqlite-execute e-runtime-store-worker--database
                    "INSERT INTO store_meta(key,value) VALUES('schema_version',?)"
                    (vector (number-to-string
                             e-runtime-store-worker-schema-version)))
    (sqlite-execute
     e-runtime-store-worker--database
     "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(?,?,?,?)"
     (vector e-runtime-store-worker-schema-version "new-current-schema"
             (secure-hash 'sha256 e-runtime-store-worker--v7-schema-checksum)
             (float-time))))
  t)

(defun e-runtime-store-worker--verify-schema-read-only ()
  "Verify that the read-only connection targets the current schema."
  (e-runtime-store-worker--verify-current-schema))

(defun e-runtime-store-worker--default-parent-identity ()
  "Return a bounded identity for direct worker-owner calls.
The scheduler always supplies its own identity.  This fallback keeps focused
worker fixtures faithful without widening the process protocol."
  (list :boot (e-runtime-store-ownership--host-boot-id)
        :pid (emacs-pid)
        :process-start (e-runtime-store-ownership--current-process-start)))

(defun e-runtime-store-worker--parent-process-start (value)
  "Encode bounded parent process-start VALUE for singleton comparison."
  (base64-encode-string
   (e-runtime-store-codec-encode-bounded
    value e-runtime-store-ownership-metadata-max-bytes)
   t))

(defun e-runtime-store-worker--parent-identity (identity)
  "Validate and normalize bounded scheduler parent IDENTITY."
  (let ((boot (plist-get identity :boot))
        (pid (plist-get identity :pid))
        (start (plist-get identity :process-start)))
    (unless (and (stringp boot) (<= (string-bytes boot) 256)
                 (integerp pid) (> pid 0) start)
      (signal 'e-runtime-store-worker-error
              (list "Malformed runtime-store parent identity" identity)))
    (list :boot boot :pid pid
          :process-start (e-runtime-store-worker--parent-process-start start))))

(defun e-runtime-store-worker--parent-live-p (pid encoded-start)
  "Return non-nil when PID still has exactly ENCODED-START identity."
  (when-let* ((attributes
              (e-runtime-store-ownership--process-attributes pid)))
    (equal encoded-start
           (e-runtime-store-worker--parent-process-start
            (e-runtime-store-ownership--process-start attributes)))))

(defun e-runtime-store-worker--install-runtime-state (runtime-id parent)
  "Preserve or safely replace the singleton state for RUNTIME-ID and PARENT.

An unretired predecessor remains replay authority until its recorded parent is
dead, has reused its PID, or comes from a prior parent boot.  Replacement and
receipt reclamation share one SQLite transaction so no receipt is orphaned."
  (let ((parent-boot (plist-get parent :boot))
        (parent-pid (plist-get parent :pid))
        (parent-start (plist-get parent :process-start)))
    (sqlite-execute e-runtime-store-worker--database "BEGIN IMMEDIATE")
    (condition-case err
        (let ((row (car (sqlite-select
                         e-runtime-store-worker--database
                         "SELECT runtime_id,parent_boot,parent_pid,parent_process_start,retired FROM runtime_store_state WHERE singleton=1"))))
          (cond
           ((not row)
            (sqlite-execute
             e-runtime-store-worker--database
             "INSERT INTO runtime_store_state(singleton,runtime_id,parent_boot,parent_pid,parent_process_start,acknowledged_prefix,retired) VALUES(1,?,?,?,?,0,0)"
             (vector runtime-id parent-boot parent-pid parent-start)))
           ((and (equal runtime-id (e-runtime-store-worker--column row 0))
                 (equal parent-boot (e-runtime-store-worker--column row 1))
                 (= parent-pid (e-runtime-store-worker--column row 2))
                 (equal parent-start (e-runtime-store-worker--column row 3)))
            ;; Same scheduler identity is a replacement worker, not a new
            ;; owner.  In particular retain retirement identity for a lost
            ;; close acknowledgement.
            nil)
           ((or (= 1 (e-runtime-store-worker--column row 4))
                (not (equal parent-boot (e-runtime-store-worker--column row 1)))
                (not (e-runtime-store-worker--parent-live-p
                      (e-runtime-store-worker--column row 2)
                      (e-runtime-store-worker--column row 3))))
            (let ((predecessor (e-runtime-store-worker--column row 0)))
              ;; Once this branch is safe, the predecessor can no longer
              ;; replay: remove all of its receipts before replacing the only
              ;; state row that names it.
              (sqlite-execute e-runtime-store-worker--database
                              "DELETE FROM runtime_store_receipts WHERE runtime_id=?"
                              (vector predecessor))
              (sqlite-execute
               e-runtime-store-worker--database
               "UPDATE runtime_store_state SET runtime_id=?,parent_boot=?,parent_pid=?,parent_process_start=?,acknowledged_prefix=0,retired=0,retirement_request_id=NULL,retirement_fingerprint=NULL,retirement_result=NULL WHERE singleton=1"
               (vector runtime-id parent-boot parent-pid parent-start))))
           (t
            (signal 'e-runtime-store-parent-active
                    (list "Runtime-store predecessor parent remains live"
                          :runtime-id (e-runtime-store-worker--column row 0)
                          :parent-boot (e-runtime-store-worker--column row 1)
                          :parent-pid (e-runtime-store-worker--column row 2)
                          :parent-process-start
                          (e-runtime-store-worker--value
                           (e-runtime-store-worker--column row 3))))))
          (sqlite-execute e-runtime-store-worker--database "COMMIT"))
      (error
       (ignore-errors (sqlite-execute e-runtime-store-worker--database "ROLLBACK"))
       (signal (car err) (cdr err))))))

(defun e-runtime-store-worker--install-borrow-authorization (request)
  "Install REQUEST's private one-use borrowed-open authorization.

The worker main loop calls this only for the control frame arriving on its
private standard-input pipe.  It deliberately produces no protocol response:
the following ordered open frame is the sole observable acknowledgement."
  (let ((keys nil)
        (cursor request))
    (while (consp cursor)
      (push (pop cursor) keys)
      (pop cursor))
    (unless (and (null cursor)
                 (equal (sort keys
                              (lambda (left right)
                                (string< (symbol-name left)
                                         (symbol-name right))))
                        '(:authorization :kind))
                 (eq (plist-get request :kind) 'borrow-authorize)
                 (null e-runtime-store-worker--database)
                 (null e-runtime-store-worker--borrow-authorization))
      (signal 'e-runtime-store-owner-identity-conflict
              (list "Borrow authorization control is invalid or repeated")))
    (let* ((authorization (plist-get request :authorization))
           (database-file (plist-get authorization :database-file)))
      (unless (stringp database-file)
        (signal 'e-runtime-store-owner-identity-conflict
                (list "Borrow authorization has no database identity")))
      (e-runtime-store-ownership--verify-borrow-authorization
       database-file authorization)
      (setq e-runtime-store-worker--borrow-authorization authorization))))

(defun e-runtime-store-worker--consume-borrow-authorization (database-file)
  "Consume and verify this worker's authorization for DATABASE-FILE once."
  (let ((authorization e-runtime-store-worker--borrow-authorization))
    ;; Clear before verification so neither success nor a caught failure can
    ;; replay the private control in this process.
    (setq e-runtime-store-worker--borrow-authorization nil)
    (unless authorization
      (signal 'e-runtime-store-owner-identity-conflict
              (list "Borrowed open has no private parent authorization")))
    (e-runtime-store-ownership--verify-borrow-authorization
     database-file authorization)))

(defun e-runtime-store-worker--open
    (directory runtime-id &optional parent-identity access-mode
               borrowed-authorized)
  "Open DIRECTORY for RUNTIME-ID under scheduler PARENT-IDENTITY."
  (unless (sqlite-available-p)
    (signal 'e-runtime-store-worker-error (list "SQLite is unavailable")))
  (setq access-mode (or access-mode 'read-write))
  (unless (memq access-mode '(read-write read-only))
    (signal 'wrong-type-argument
            (list '(member read-write read-only) access-mode)))
  (setq directory (file-name-as-directory (expand-file-name directory))
        e-runtime-store-worker--runtime-id runtime-id
        e-runtime-store-worker--access-mode access-mode
        e-runtime-store-worker--database-file
        (expand-file-name "store.sqlite3" directory))
  (when (eq access-mode 'read-write)
    (make-directory directory t)
    (set-file-modes directory #o700)
    (if borrowed-authorized
        ;; An explicit offline parent already owns the one process-lifetime
        ;; lock.  Consume the private control-pipe authorization before SQLite
        ;; opens; this child never releases the borrowed claim.
        (e-runtime-store-worker--consume-borrow-authorization
         e-runtime-store-worker--database-file)
      ;; The writer owns runtime lifecycle.  Its read-only sibling is subordinate
      ;; to the same parent and never competes for this exclusive claim.
      (setq e-runtime-store-worker--ownership
            (e-runtime-store-ownership-acquire
             e-runtime-store-worker--database-file runtime-id 'ordinary))))
  (let ((opened nil))
    (unwind-protect
        (let ((new-store-p
               (not (file-exists-p e-runtime-store-worker--database-file))))
          (when (and (eq access-mode 'read-only) new-store-p)
            (signal 'e-runtime-store-worker-error
                    (list "Read-only worker requires an existing database")))
          (setq e-runtime-store-worker--database
                (sqlite-open e-runtime-store-worker--database-file
                             (eq access-mode 'read-only)))
          (sqlite-execute e-runtime-store-worker--database "PRAGMA foreign_keys=ON")
          ;; An existing store crosses a verification-only boundary before
          ;; WAL mode or any schema/domain initializer can mutate it.
          (unless new-store-p
            (e-runtime-store-worker--verify-current-schema))
          (when (eq access-mode 'read-write)
            (sqlite-select e-runtime-store-worker--database "PRAGMA journal_mode=WAL")
            (sqlite-execute e-runtime-store-worker--database "PRAGMA synchronous=NORMAL"))
          (sqlite-execute e-runtime-store-worker--database "PRAGMA busy_timeout=2500")
          (if (eq access-mode 'read-only)
              t
            (when new-store-p
              (e-runtime-store-worker--schema t))
            (e-runtime-store-worker--install-runtime-state
             runtime-id
             (e-runtime-store-worker--parent-identity
              (or parent-identity
                  (e-runtime-store-worker--default-parent-identity))))
            (e-runtime-store-worker--permissions))
          (setq opened t)
          (list :schema-version e-runtime-store-worker-schema-version
                :database-file e-runtime-store-worker--database-file
                :runtime-id runtime-id :pid (emacs-pid) :access-mode access-mode
                :journal-mode "wal" :synchronous "normal"))
      (unless opened
        (e-runtime-store-worker--close)))))

(defun e-runtime-store-worker--session-append (body)
  "Append BODY's one session record through the session worker module."
  (e-runtime-store-session-worker-write
   e-runtime-store-worker--database body))

(defun e-runtime-store-worker--session-append-batch (body)
  "Append BODY's session record batch through the session worker module."
  (e-runtime-store-session-worker-write
   e-runtime-store-worker--database body))

(defun e-runtime-store-worker--session-board-participant-admit (body)
  "Atomically admit BODY's session, Board participant, and optional pickup."
  (let* ((board-id (plist-get body :board-id))
         (board-row
          (car
           (sqlite-select
            e-runtime-store-worker--database
            "SELECT trusted_principal,generation,revision,next_position,root_payload FROM boards WHERE board_id=?"
            (vector board-id))))
         (_
          (unless board-row
            (signal 'e-runtime-store-board-conflict
                    (list "Unknown Board" board-id))))
         (generation (e-runtime-store-worker--column board-row 1))
         (_
          (when (and (plist-get body :generation)
                     (/= (plist-get body :generation) generation))
            (signal 'e-runtime-store-board-conflict
                    (list "Stale Board generation" board-id
                          (plist-get body :generation) generation))))
         (session-result
          (e-runtime-store-session-worker-write
           e-runtime-store-worker--database
           (list :op 'session-append-batch
                 :session-id (plist-get body :session-id)
                 :records (plist-get body :records)
                 :query-delta (plist-get body :query-delta))))
         (participant (plist-get body :participant))
         (_
          (unless (eq (plist-get participant :role) 'participant)
            (signal 'e-runtime-store-board-conflict
                    (list "Child admission requires participant role"
                          (plist-get participant :role)))))
         (participant-result
          (e-board-sqlite-worker-write
           e-runtime-store-worker--database
           (list :op 'board-participant-put
                 :board-id (plist-get body :board-id)
                 :generation generation
                 :participant participant)))
         (pickup (plist-get body :pickup))
         (pickup-result
          (when pickup
            (e-board-sqlite-worker-write
             e-runtime-store-worker--database
             (list :op 'board-pickup-session-admit
                   :board-id (plist-get body :board-id)
                   :generation generation
                   :delivery-id (plist-get pickup :delivery-id)
                   :session-id (plist-get body :session-id)
                   :record (plist-get pickup :record)
                   :lane (plist-get pickup :lane))))))
    (list :session session-result
          :participant participant-result
          :pickup pickup-result
          :association
          (list :board-id board-id
                :principal (plist-get (plist-get body :query-delta) :principal)
                :association-role
                (plist-get (plist-get body :query-delta) :association-role)
                :routing-policy
                (copy-tree
                 (plist-get (plist-get body :query-delta) :routing-policy) t))
          :board-revision
          (or (plist-get pickup-result :board-revision)
              (plist-get participant-result :revision)))))

(defun e-runtime-store-worker--chat-session-owner-admit (body)
  "Atomically admit one chat owner session without fabricating input."
  (let* ((session-id (plist-get body :session-id))
         (board-id (plist-get body :board-id))
         (principal (plist-get body :principal))
         (existing
          (car (sqlite-select
                e-runtime-store-worker--database
                "SELECT board_id,principal,association_role,routing_policy FROM session_query_state WHERE session_id=?"
                (vector session-id))))
         session-result participant-result)
    (if existing
        (unless (and (equal board-id
                            (e-runtime-store-worker--column existing 0))
                     (equal principal
                            (e-runtime-store-worker--column existing 1))
                     (equal "owner"
                            (e-runtime-store-worker--column existing 2)))
          (signal 'e-runtime-store-board-conflict
                  (list "Chat admission identity conflicts" session-id)))
      (e-board-sqlite-worker-write
       e-runtime-store-worker--database
       (list :op 'board-create :board-id board-id
             :trusted-principal principal :root (list :board-id board-id)))
      (setq session-result
            (e-runtime-store-session-worker-write
             e-runtime-store-worker--database
             (list :op 'session-append-batch :session-id session-id
                   :records (plist-get body :records)
                   :query-delta (plist-get body :query-delta))))
      (setq participant-result
            (e-board-sqlite-worker-write
             e-runtime-store-worker--database
             (list :op 'board-participant-put :board-id board-id
                   :generation 1 :participant
                   (plist-get body :participant)))))
    (list :status (if existing 'existing 'created)
          :session session-result
          :participant participant-result
          :session-id session-id
          :association
          (list :board-id board-id :principal principal
                :association-role "owner"
                :routing-policy
                (if existing
                    (e-runtime-store-worker--value
                     (e-runtime-store-worker--column existing 3))
                  (copy-tree
                   (plist-get (plist-get body :query-delta) :routing-policy)
                   t))))))

(defun e-runtime-store-worker--chat-session-input-admit (body)
  "Atomically admit one new chat session and its first routed input."
  (let* ((owner-result
          (e-runtime-store-worker--chat-session-owner-admit body))
         (session-id (plist-get body :session-id))
         (board-id (plist-get body :board-id))
         (principal (plist-get body :principal)))
    (let* ((append-body (copy-sequence body))
           (_ (setq append-body (plist-put append-body :op 'board-append-route)))
           (append-result
            (e-board-sqlite-worker-write
             e-runtime-store-worker--database append-body))
           (policy
            (plist-get (plist-get owner-result :association)
                       :routing-policy)))
      ;; APPEND-RESULT carries a request-local association slot for callers
      ;; that resolve a Board from SESSION-ID.  Composite admission already
      ;; owns the exact association and must replace that slot, not append a
      ;; duplicate plist key whose nil value shadows the committed policy.
      (let ((result (copy-sequence append-result)))
        (setq result (plist-put result :session
                                (plist-get owner-result :session))
              result (plist-put result :participant
                                (plist-get owner-result :participant))
              result (plist-put result :session-id session-id)
              result
              (plist-put
               result :association
               (list :board-id board-id :principal principal
                     :association-role "owner" :routing-policy policy)))
        result))))

(defun e-runtime-store-worker--session-delete (body)
  "Delete one session through the session worker module."
  (e-runtime-store-session-worker-write
   e-runtime-store-worker--database body))

(defun e-runtime-store-worker--session-command (body)
  "Commit BODY's sealed session command and optional tool transition."
  (let ((result
         (e-runtime-store-session-worker-write
          e-runtime-store-worker--database body))
        (continuity (plist-get body :continuity)))
    (when continuity
      (e-runtime-store-worker--tool-transition
       (append (list :session-id (plist-get body :session-id)) continuity)))
    result))

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

(defun e-runtime-store-worker--session-append-with-tool-transition (body)
  "Atomically append BODY's session record and optional tool transition."
  (let ((result (e-runtime-store-worker--session-append body))
        (continuity (plist-get body :continuity)))
    (when continuity
      (e-runtime-store-worker--tool-transition
       (append (list :session-id (plist-get body :session-id)) continuity)))
    result))

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
    ('session-command (e-runtime-store-worker--session-command body))
    ('session-append (e-runtime-store-worker--session-append body))
    ('session-append-with-tool-transition
     (e-runtime-store-worker--session-append-with-tool-transition body))
    ('session-append-batch (e-runtime-store-worker--session-append-batch body))
    ('session-board-participant-admit
     (e-runtime-store-worker--session-board-participant-admit body))
    ('chat-session-input-admit
     (e-runtime-store-worker--chat-session-input-admit body))
    ('chat-session-owner-admit
     (e-runtime-store-worker--chat-session-owner-admit body))
    ('session-delete (e-runtime-store-worker--session-delete body))
    ('tool-transition (e-runtime-store-worker--tool-transition body))
    ('resource-put (e-runtime-store-worker--resource-put body))
    ('resource-delete (e-runtime-store-worker--resource-delete body))
    ('resource-delete-lineage
     (e-runtime-store-worker--resource-delete-lineage body))
    ('resource-expire (e-runtime-store-worker--resource-expire body))
    ((or 'board-create 'board-clear 'board-record-put 'board-append-route
         'board-record-append
         'board-routing-put
         'board-pickup-transition 'board-participant-put
         'board-participant-delete 'board-participant-publish
         'board-replay-progress-put 'board-pickup-session-admit)
     (e-board-sqlite-worker-write
      e-runtime-store-worker--database body))
    ((or 'task-queue-open 'task-enqueue 'task-claim 'task-runnable-claim
         'task-transition
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

(defun e-runtime-store-worker--request-fingerprint (request)
  "Return the stable canonical fingerprint for write REQUEST.
The request id is the receipt key; every other replay-semantic frame field is
bound into the fingerprint so an id cannot silently adopt a changed kind or
acknowledgement prefix."
  (secure-hash
   'sha256
   (e-runtime-store-codec-encode
    (list :kind (plist-get request :kind)
          :body (plist-get request :body)
          :write-prefix (plist-get request :write-prefix)
          :ack-prefix (plist-get request :ack-prefix)))))

(defun e-runtime-store-worker--receipt-result (row)
  "Return the bounded decoded result held by receipt ROW."
  (e-runtime-store-worker--value (e-runtime-store-worker--column row 1)))

(defun e-runtime-store-worker--write (request)
  "Execute one idempotent transactional write REQUEST and return its result."
  (let* ((body (plist-get request :body))
         (request-id (plist-get request :id))
         (fingerprint (e-runtime-store-worker--request-fingerprint request))
         (prefix (or (plist-get request :write-prefix) 0))
         (ack-prefix (or (plist-get request :ack-prefix) 0)))
    (e-runtime-store-worker--permissions)
    (sqlite-execute e-runtime-store-worker--database "BEGIN IMMEDIATE")
    (condition-case err
        (let ((prior
               (car (sqlite-select
                     e-runtime-store-worker--database
                     "SELECT fingerprint,result FROM runtime_store_receipts WHERE runtime_id=? AND request_id=?"
                     (vector e-runtime-store-worker--runtime-id request-id)))))
          (if prior
              (progn
                (unless (equal fingerprint (e-runtime-store-worker--column prior 0))
                  (signal 'e-runtime-store-worker-error
                          (list "Runtime-store receipt fingerprint collision"
                                :runtime-id e-runtime-store-worker--runtime-id
                                :request-id request-id)))
                (sqlite-execute e-runtime-store-worker--database "COMMIT")
                (e-runtime-store-worker--receipt-result prior))
            ;; Advance only through writes already observed by the parent.  The
            ;; active request is never among these receipts, so an uncertain
            ;; commit remains available to its replacement worker.
            (sqlite-execute e-runtime-store-worker--database
                            "DELETE FROM runtime_store_receipts WHERE runtime_id=? AND write_prefix<=?"
                            (vector e-runtime-store-worker--runtime-id ack-prefix))
            (sqlite-execute e-runtime-store-worker--database
                            "UPDATE runtime_store_state SET acknowledged_prefix=MAX(acknowledged_prefix,?) WHERE singleton=1 AND runtime_id=?"
                            (vector ack-prefix e-runtime-store-worker--runtime-id))
            (let* ((result (e-runtime-store-worker--write-dispatch body))
                   ;; Encode before COMMIT so an unrepresentable result aborts
                   ;; the mutation instead of creating an unacknowledgeable one.
                   (encoded-result
                    (e-runtime-store-worker--bounded-receipt-result
                     request result)))
              (sqlite-execute
               e-runtime-store-worker--database
               "INSERT INTO runtime_store_receipts(runtime_id,request_id,fingerprint,result,write_prefix) VALUES(?,?,?,?,?)"
               (vector e-runtime-store-worker--runtime-id request-id fingerprint
                       encoded-result prefix))
              (e-runtime-store-worker--test-fault "before-commit" request)
              (sqlite-execute e-runtime-store-worker--database "COMMIT")
              (e-runtime-store-worker--test-fault "after-commit" request)
              result)))
      (error
       (ignore-errors (sqlite-execute e-runtime-store-worker--database "ROLLBACK"))
       (signal (car err) (cdr err))))))

(defun e-runtime-store-worker--retire (request)
  "Transactionally retire the current runtime for idempotent close REQUEST."
  (let* ((request-id (plist-get request :id))
         (fingerprint (e-runtime-store-worker--request-fingerprint request))
         (result (list :runtime-id e-runtime-store-worker--runtime-id :retired t))
         (encoded-result (e-runtime-store-worker--bounded-receipt-result
                          request result)))
    (sqlite-execute e-runtime-store-worker--database "BEGIN IMMEDIATE")
    (condition-case err
        (let ((row (car (sqlite-select
                         e-runtime-store-worker--database
                         "SELECT retirement_request_id,retirement_fingerprint,retirement_result FROM runtime_store_state WHERE singleton=1 AND runtime_id=?"
                         (vector e-runtime-store-worker--runtime-id)))))
          (unless row
            (signal 'e-runtime-store-worker-error (list "Missing runtime state at close")))
          (let ((prior-id (e-runtime-store-worker--column row 0)))
            (if prior-id
                (progn
                  (unless (and (equal prior-id request-id)
                               (equal (e-runtime-store-worker--column row 1) fingerprint))
                    (signal 'e-runtime-store-worker-error
                            (list "Runtime-store retirement fingerprint collision"
                                  :runtime-id e-runtime-store-worker--runtime-id
                                  :request-id request-id)))
                  (sqlite-execute e-runtime-store-worker--database "COMMIT")
                  (e-runtime-store-worker--value
                   (e-runtime-store-worker--column row 2)))
              (sqlite-execute e-runtime-store-worker--database
                              "DELETE FROM runtime_store_receipts WHERE runtime_id=?"
                              (vector e-runtime-store-worker--runtime-id))
              (sqlite-execute
               e-runtime-store-worker--database
               "UPDATE runtime_store_state SET retired=1, retirement_request_id=?, retirement_fingerprint=?, retirement_result=? WHERE singleton=1"
               (vector request-id fingerprint encoded-result))
              (e-runtime-store-worker--test-fault "before-retirement-commit")
              (sqlite-execute e-runtime-store-worker--database "COMMIT")
              (e-runtime-store-worker--test-fault "after-retirement-commit")
              result)))
      (error
       (ignore-errors (sqlite-execute e-runtime-store-worker--database "ROLLBACK"))
       (signal (car err) (cdr err))))))

(defun e-runtime-store-worker--chat-session-view (body)
  "Read one bounded chat DTO and Board change boundary in this SQL snapshot."
  (let* ((session-id (plist-get body :session-id))
         (limit (min 64 (max 1 (or (plist-get body :limit) 32))))
         (metadata
          (e-runtime-store-session-worker-read
           e-runtime-store-worker--database
           (list :op 'session-metadata :session-id session-id)))
         (association
          (e-runtime-store-session-worker-read
           e-runtime-store-worker--database
           (list :op 'session-board-association :session-id session-id)))
         (board-id (and association (plist-get association :board-id))))
    (unless (and metadata association (stringp board-id))
      (signal 'e-runtime-store-board-conflict
              (list "Session has no complete Board chat view" session-id)))
    (let* ((presentation-metadata
            (append
             (copy-tree (plist-get metadata :metadata) t)
             (list :session-id session-id
                   :name (plist-get metadata :name)
                   :summary (plist-get metadata :summary)
                   :latest-assistant-marker
                   (plist-get metadata :latest-assistant-marker)
                   :turn-options
                   (copy-tree (plist-get metadata :turn-options) t))))
           (window
            (e-board-sqlite-worker-read
             e-runtime-store-worker--database
             (list :op 'board-visible-window :board-id board-id :limit limit)))
           (messages
            (mapcar
             (lambda (row)
               (let* ((record (copy-tree (plist-get row :record) t))
                      (kind (or (plist-get record :kind)
                                (plist-get record :record-kind))))
                 (plist-put record :role
                            (if (eq kind 'input) 'user 'assistant))
                 (plist-put record :metadata
                            (copy-tree (plist-get record :attributes) t))
                 (plist-put record :references
                            (copy-tree (plist-get record :reference) t))
                 record))
             (plist-get window :records))))
      (list :session-id session-id :metadata presentation-metadata
            :association association :messages messages
            :cursor (plist-get window :cursor)
            :through (plist-get window :through)
            :truncated (and (plist-get window :truncated) t)))))

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
    ('chat-session-view
     (e-runtime-store-worker--chat-session-view body))
    ((or 'session-query-state 'session-state-get
         'session-query-state-get
         'session-metadata 'session-metadata-get
         'session-board-association 'session-board-association-get
         'session-query-page 'session-state-page 'session-id-page
         'session-recent-page 'session-root-page
         'session-record-page 'session-history-page
         'session-recent-failures 'session-turn-inspection
         'session-visible-message-page 'session-visible-messages
         'session-context-path
         'session-process-report-marker-page
         'session-process-report-marker
         'session-process-report-triage-page
         'session-process-report-extraction-page
         'session-process-report-request-shapes
         'session-process-report-marker-count
         'session-header)
     (e-runtime-store-session-worker-read
      e-runtime-store-worker--database body))
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
    ((or 'board-get 'board-list 'board-record-page 'board-visible-window
         'board-orchestration-run 'board-orchestration-runs
         'board-routing-get
         'board-pickup-list 'board-participant-list
         'board-replay-progress-get)
     (e-board-sqlite-worker-read
      e-runtime-store-worker--database body))
    ((or 'task-snapshot 'task-queue-status)
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
                                    (min (* 256 1024)
                                         (max 1 (or (plist-get body :limit)
                                                    (* 256 1024))))
                                    (plist-get body :lineage-id)
                                    (plist-get body :path)
                                    (or (plist-get body :now) (float-time)))))))
       (let* ((content (e-runtime-store-worker--column row 0))
              (total (e-runtime-store-worker--column row 1))
              (offset (max 0 (or (plist-get body :offset) 0)))
              (limit (min (* 256 1024)
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
    ('resource-search-source
     ;; Search policy (literal/regexp/case/ranking) remains in the resource
     ;; domain.  SQLite supplies one bounded detached source page so the
     ;; caller never performs an N+1 series of resource reads or reconstructs
     ;; a process-local resource catalog.
     (let ((rows
            (sqlite-select
             e-runtime-store-worker--database
             "SELECT resource_path,SUBSTR(CAST(content AS TEXT),1,4096),LENGTH(CAST(content AS BLOB)),metadata,updated_at,expires_at FROM resources WHERE lineage_id=? AND (expires_at IS NULL OR expires_at>=?) ORDER BY resource_path LIMIT ?"
             (vector (plist-get body :lineage-id)
                     (or (plist-get body :now) (float-time))
                     (min 256 (max 1 (or (plist-get body :limit) 64)))))))
       (mapcar
        (lambda (row)
          (list :path (e-runtime-store-worker--column row 0)
                :content (e-runtime-store-worker--column row 1)
                :bytes (e-runtime-store-worker--column row 2)
                :metadata (e-runtime-store-worker--value
                           (e-runtime-store-worker--column row 3))
                :updated-at (e-runtime-store-worker--column row 4)
                :expires-at (e-runtime-store-worker--column row 5)))
        rows)))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown read operation" (plist-get body :op))))))

(defun e-runtime-store-worker--read-snapshot (body)
  "Execute BODY against one SQLite read snapshot.

`store-backup' uses SQLite's own `VACUUM INTO' snapshot and cannot run inside a
transaction.  Every ordinary bounded query is fenced by one read transaction so
multi-statement consumer-shaped adapters return one database-issued boundary."
  (if (eq (plist-get body :op) 'store-backup)
      (e-runtime-store-worker--read body)
    (sqlite-execute e-runtime-store-worker--database "BEGIN")
    (condition-case err
        (prog1 (e-runtime-store-worker--read body)
          (sqlite-execute e-runtime-store-worker--database "COMMIT"))
      (error
       (ignore-errors
         (sqlite-execute e-runtime-store-worker--database "ROLLBACK"))
       (signal (car err) (cdr err))))))

(defun e-runtime-store-worker--handle (request)
  "Handle one decoded REQUEST."
  (pcase (plist-get request :kind)
    ('open (e-runtime-store-worker--open
            (plist-get request :directory) (plist-get request :runtime-id)
            (plist-get request :parent-identity)
            (plist-get request :access-mode)
            (plist-get request :borrowed-authorized)))
    ('close (if (eq e-runtime-store-worker--access-mode 'read-only)
                (list :runtime-id e-runtime-store-worker--runtime-id
                      :retired t :read-only t)
              (e-runtime-store-worker--retire request)))
    ('write (if (eq e-runtime-store-worker--access-mode 'read-only)
                (signal 'e-runtime-store-worker-error
                        (list "Read-only worker rejected a mutation"))
              (e-runtime-store-worker--write request)))
    ('read (e-runtime-store-worker--read-snapshot
            (plist-get request :body)))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown request kind" (plist-get request :kind))))))

(defun e-runtime-store-worker--response (request)
  "Return one correlated success or typed-error response for REQUEST."
  (condition-case err
      (progn
        (e-runtime-store-worker--test-stall request)
        (list :id (plist-get request :id) :ok t
              :result (e-runtime-store-worker--handle request)))
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
        (e-runtime-store-worker--test-fault "after-response-formation" request)
        (e-runtime-store-worker--test-fault "after-open-response-formation" request)
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
          (let ((request (condition-case err
                             (e-runtime-store-worker--unpack line)
                           (error (list :decode-error err)))))
            (if (eq (plist-get request :kind) 'borrow-authorize)
                (e-runtime-store-worker--install-borrow-authorization request)
              (e-runtime-store-worker--emit-response
               request (e-runtime-store-worker--response request)))))
      (e-runtime-store-worker--close))))

(provide 'e-runtime-store-worker)

;;; e-runtime-store-worker.el ends here

;;; e-runtime-store-offline-test.el --- explicit schema upgrades -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'sqlite)
(require 'e-runtime-store-codec)
(require 'e-runtime-store-offline)
(require 'e-runtime-store)
(require 'e-runtime-store-worker)
(require 'e-runtime-store-session-worker)
(require 'e-session-query)

(defconst e-runtime-store-offline-test--source-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this focused offline-store test source.")

(defun e-runtime-store-offline-test--payload (value)
  "Encode VALUE exactly as a legacy v5 session payload."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-runtime-store-offline-test--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-runtime-store-offline-test--file-hash (file)
  "Return SHA-256 of FILE bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun e-runtime-store-offline-test--make-v7 ()
  "Create and close one disposable current v7 runtime store."
  (let* ((directory (make-temp-file "e-runtime-offline-v7-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (runtime (e-runtime-store-open directory)))
    (unwind-protect
        (e-runtime-store-call runtime 'read '(:op store-metrics))
      (e-runtime-store-close runtime))
    (list directory database-file)))

(defun e-runtime-store-offline-test--migration-tuples (database)
  "Return DATABASE migration rows as detached version/identity/checksum lists."
  (mapcar
   (lambda (row)
     (list (e-runtime-store-offline-test--column row 0)
           (e-runtime-store-offline-test--column row 1)
           (e-runtime-store-offline-test--column row 2)))
   (sqlite-select
    database
    "SELECT version,identity,checksum FROM schema_migrations ORDER BY version")))

(defun e-runtime-store-offline-test--root (session-id)
  "Return a minimal canonical root for SESSION-ID."
  (list :type "session" :session-id session-id :id "root"
        :timestamp "2026-09-06T00:00:00Z"
        :created-at "2026-09-06T00:00:00Z"
        :updated-at "2026-09-06T00:00:00Z"
        :metadata (list :name (concat "Name " session-id))))

(defun e-runtime-store-offline-test--message (session-id)
  "Return a canonical user message for SESSION-ID."
  (list :type "message" :session-id session-id :id "message"
        :parent-id "root" :timestamp "2026-09-06T00:00:01Z"
        :message (list :id "message" :role 'user :content "Prompt"
                       :created-at "2026-09-06T00:00:01Z")))

(defun e-runtime-store-offline-test--record
    (session-id type id second &rest fields)
  "Build one bounded replay RECORD for SESSION-ID."
  (append (list :type type :session-id session-id :id id
                :timestamp (format "2026-09-06T00:00:%02dZ" second))
          fields))

(defun e-runtime-store-offline-test--all-family-records (session-id)
  "Return one valid v5 journal record for every query family."
  (list
   (append (e-runtime-store-offline-test--root session-id)
           (list :delta-id "root-delta"))
   (e-runtime-store-offline-test--record
    session-id "message" "user" 1
    :message '(:id "user" :role user :content "Prompt"
               :created-at "2026-09-06T00:00:01Z"))
   (e-runtime-store-offline-test--record
    session-id "message" "assistant" 2
    :message '(:id "assistant" :role assistant :content "Answer"
               :created-at "2026-09-06T00:00:02Z"
               :board-output-sequence 4))
   (e-runtime-store-offline-test--record
    session-id "activity-event" "activity" 3
    :board-activity-sequence 5 :semantic-event '(:event-type tool-finished))
   (e-runtime-store-offline-test--record
    session-id "message-display" "user" 4 :display "hidden")
   (e-runtime-store-offline-test--record
    session-id "process-report" "report" 5 :report '(:status done))
   (e-runtime-store-offline-test--record
    session-id "branch-summary" "branch-summary" 6
    :branch-id "main" :summary "Summary")
   (e-runtime-store-offline-test--record
    session-id "compaction" "compaction" 7
    :branch-id "main" :summary "Compacted" :first-kept-entry-id "root")
   (e-runtime-store-offline-test--record
    session-id "provider-anchor" "anchor" 8
    :provider-id "openai" :model "gpt")
   (e-runtime-store-offline-test--record
    session-id "context-generation" "legacy-generation" 9
    :context-record '(:record-version 1 :type context-generation))
   (e-runtime-store-offline-test--record
    session-id "context-promotion" "legacy-promotion" 10
    :context-record '(:record-version 1 :type context-promotion))
   (e-runtime-store-offline-test--record
    session-id "context-frame" "frame" 11
    :context-record '(:record-version 2 :type context-frame))
   (e-runtime-store-offline-test--record
    session-id "context-frame-settlement" "settlement" 12
    :context-record '(:record-version 2 :type context-frame-settlement))
   (e-runtime-store-offline-test--record
    session-id "context-generation" "generation" 13
    :context-record '(:record-version 2 :type context-generation))
   (e-runtime-store-offline-test--record
    session-id "context-promotion" "promotion" 14
    :context-record '(:record-version 3 :type context-promotion))
   (e-runtime-store-offline-test--record
    session-id "context-curation-package" "curation" 15
    :promotion '(:record-version 3 :type context-promotion) :erasure nil)
   (e-runtime-store-offline-test--record
    session-id "messages-cleared" "clear" 16)
   (e-runtime-store-offline-test--record
    session-id "current-branch" "branch" 20 :branch-id "feature")
   (e-runtime-store-offline-test--record
    session-id "session-info" "info-metadata" 21
    :field 'metadata :value '(:name "Meta" :model "meta"))
   (e-runtime-store-offline-test--record
    session-id "session-info" "info-config" 22
    :field 'config :value '(:model "config"))
   (e-runtime-store-offline-test--record
    session-id "session-info" "info-reference" 23
    :field 'context-reference :key :source-reference :value "source")
   (e-runtime-store-offline-test--record
    session-id "session-info" "info-references" 24
    :field 'context-references :owner "owner"
    :value '(:source "reference"))
   (e-runtime-store-offline-test--record
    session-id "session-info" "info-capability" 25
    :field 'capability-state :capability-id "capability" :version 1
    :value '(:enabled t))
   (e-runtime-store-offline-test--record
    session-id "session-info" "info-options" 26
    :field 'turn-options :value '(:model "model"))
   (e-runtime-store-offline-test--record
    session-id "session-info" "info-name" 27
    :field 'name :value "Final")))

(defun e-runtime-store-offline-test--catalog (state)
  "Return a retired catalog witness matching STATE."
  (list (list :id (plist-get state :session-id)
              :name (plist-get state :name)
              :summary (plist-get state :summary)
              :metadata (plist-get state :metadata)
              :message-count (plist-get state :message-count)
              :created-at (plist-get state :created-at)
              :updated-at (plist-get state :updated-at)
              :last-message-at (plist-get state :last-message-at)
              :latest-assistant-marker
              (plist-get state :latest-assistant-marker)
              :board-id (plist-get state :board-id)
              :principal (plist-get state :principal))))

(defun e-runtime-store-offline-test--make-v5
    (records &optional catalog checkpoint)
  "Create a disposable v5 database containing RECORDS.

The fixture writes the retired tables directly.  It never opens the user's
runtime directory or uses the v6 initializer to manufacture a v5 database."
  (let* ((directory (make-temp-file "e-runtime-offline-v5-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (database (sqlite-open database-file))
         (session-id (plist-get (car records) :session-id)))
    (unwind-protect
        (progn
          (dolist
              (statement
               '("CREATE TABLE store_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
                 "CREATE TABLE session_records (session_id TEXT NOT NULL, position INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(session_id, position))"
                 "CREATE TABLE session_checkpoints (session_id TEXT PRIMARY KEY, payload TEXT NOT NULL, revision INTEGER NOT NULL)"
                 "CREATE TABLE catalog_projection (singleton INTEGER PRIMARY KEY CHECK(singleton = 1), payload TEXT NOT NULL, revision INTEGER NOT NULL)"))
            (sqlite-execute database statement))
          (sqlite-execute database
                          "INSERT INTO store_meta(key,value) VALUES('schema_version','5')")
          (let ((position 0))
            (dolist (record records)
              (setq position (1+ position))
              (sqlite-execute
               database
               "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
               (vector session-id position
                       (e-runtime-store-offline-test--payload record)))))
          (when catalog
            (sqlite-execute
             database
             "INSERT INTO catalog_projection(singleton,payload,revision) VALUES(1,?,1)"
             (vector (e-runtime-store-offline-test--payload catalog))))
          (when checkpoint
            (sqlite-execute
             database
             "INSERT INTO session_checkpoints(session_id,payload,revision) VALUES(?,?,1)"
             (vector session-id
                     (e-runtime-store-offline-test--payload checkpoint))))
          (sqlite-close database)
          (setq database nil)
          (set-file-modes database-file #o600)
          (list directory database-file))
      (when database (sqlite-close database)))))

(defun e-runtime-store-offline-test--make-v6 (records)
  "Create a disposable physical v6 database containing canonical RECORDS."
  (let* ((directory (make-temp-file "e-runtime-offline-v6-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (database (sqlite-open database-file))
         (session-id (plist-get (car records) :session-id)))
    (unwind-protect
        (progn
          (sqlite-execute database "PRAGMA foreign_keys=ON")
          (sqlite-execute
           database
           "CREATE TABLE store_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
          (e-runtime-store-worker--initialize-common-schema database)
          (e-runtime-store-worker--initialize-domain-schema database 6)
          (sqlite-execute
           database
           "INSERT INTO store_meta(key,value) VALUES('schema_version','6')")
          (sqlite-execute
           database
           "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(6,'new-current-schema',?,0)"
           (vector (secure-hash 'sha256 "feature92-schema-v6")))
          (let ((position 0))
            (dolist (record records)
              (setq position (1+ position))
              (let ((columns
                     (e-runtime-store-session-worker--record-columns record)))
                (sqlite-execute
                 database
                 "INSERT INTO session_records(session_id,position,payload,record_type,record_id,record_identity,parent_id,timestamp) VALUES(?,?,?,?,?,?,?,?)"
                 (vector
                  session-id position
                  (e-runtime-store-offline-test--payload record)
                  (nth 0 columns) (nth 1 columns) (nth 2 columns)
                  (nth 3 columns) (nth 4 columns))))))
          (let ((state (e-session-query-derive records)))
            (when state
              (plist-put state :journal-position (length records))
              (sqlite-execute
               database
               (concat
                "INSERT INTO session_query_state(session_id,name,summary,metadata,created_at,updated_at,last_message_at,latest_assistant_marker,message_count,current_branch,turn_options,current_head_id,root_event_id,current_context_generation_id,board_id,principal,association_role,routing_policy,root_p,board_output_sequence,board_activity_sequence,journal_position) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
               (e-runtime-store-session-worker--state-values state))))
          (sqlite-close database)
          (setq database nil)
          (set-file-modes database-file #o600)
          (list directory database-file))
      (when database (sqlite-close database)))))

(defun e-runtime-store-offline-test--make-v4
    (records &optional catalog checkpoint)
  "Create a disposable v4 database with the retained session relations.

This is a direct old-shape fixture rather than a current v6 database with its
version label edited.  The v4-to-v5 envelope is therefore exercised as a real
compatibility stage before the v6 session cutover."
  (let* ((directory (make-temp-file "e-runtime-offline-v4-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (database (sqlite-open database-file))
         (session-id (plist-get (car records) :session-id)))
    (unwind-protect
        (progn
          (dolist
              (statement
               '("CREATE TABLE store_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
                 "CREATE TABLE session_records (session_id TEXT NOT NULL, position INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(session_id, position))"
                 "CREATE TABLE session_checkpoints (session_id TEXT PRIMARY KEY, payload TEXT NOT NULL, revision INTEGER NOT NULL)"
                 "CREATE TABLE catalog_projection (singleton INTEGER PRIMARY KEY CHECK(singleton = 1), payload TEXT NOT NULL, revision INTEGER NOT NULL)"))
            (sqlite-execute database statement))
          (sqlite-execute database
                          "INSERT INTO store_meta(key,value) VALUES('schema_version','4')")
          (let ((position 0))
            (dolist (record records)
              (setq position (1+ position))
              (sqlite-execute
               database
               "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
               (vector session-id position
                       (e-runtime-store-offline-test--payload record)))))
          (when catalog
            (sqlite-execute
             database
             "INSERT INTO catalog_projection(singleton,payload,revision) VALUES(1,?,1)"
             (vector (e-runtime-store-offline-test--payload catalog))))
          (when checkpoint
            (sqlite-execute
             database
             "INSERT INTO session_checkpoints(session_id,payload,revision) VALUES(?,?,1)"
             (vector session-id
                     (e-runtime-store-offline-test--payload checkpoint))))
          (sqlite-close database)
          (setq database nil)
          (set-file-modes database-file #o600)
          (list directory database-file))
      (when database (sqlite-close database)))))

(defun e-runtime-store-offline-test--version (database-file)
  "Return the schema version in DATABASE-FILE."
  (let ((database (sqlite-open database-file)))
    (unwind-protect
        (string-to-number
         (e-runtime-store-offline-test--column
          (car (sqlite-select database
                              "SELECT value FROM store_meta WHERE key='schema_version'"))
          0))
      (sqlite-close database))))

(defun e-runtime-store-offline-test--table-p (database-file table)
  "Return whether TABLE exists in DATABASE-FILE."
  (let ((database (sqlite-open database-file)))
    (unwind-protect
        (not (null (sqlite-select database
                                  "SELECT 1 FROM sqlite_master WHERE type='table' AND name=?"
                                  (vector table))))
      (sqlite-close database))))

(ert-deftest e-runtime-store-offline-v5-oversized-checkpoint-is-ignored ()
  "A retired checkpoint over the current write bound is skipped as unusable."
  (let* ((session-id "oversized-checkpoint")
         (records (list (e-runtime-store-offline-test--root session-id)))
         (checkpoint
          (list :version 1 :session-id session-id
                :root (list :id "root"
                            :created-at "2026-09-06T00:00:00Z")
                ;; This is intentionally larger than the current one-MiB
                ;; checkpoint projection bound, as are known old stores.
                :legacy-payload
                (make-string (+ e-runtime-store-codec-checkpoint-canonical-byte-limit
                                1024)
                             ?x)))
         (fixture (e-runtime-store-offline-test--make-v5
                   records nil checkpoint))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (let ((result (e-runtime-store-offline-upgrade directory backup)))
          (should (= (plist-get result :from) 5))
          (should (= (plist-get result :to) 7))
          (should-not (e-runtime-store-offline-test--table-p
                       database-file "session_checkpoints")))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v4-sequential-compatibility-upgrade ()
  "A real v4 source crosses each retained boundary before v7 install."
  (let* ((session-id "v4-session")
         (records (list (e-runtime-store-offline-test--root session-id)
                        (e-runtime-store-offline-test--message session-id)))
         (state (e-session-query-derive records))
         (catalog (e-runtime-store-offline-test--catalog state))
         (fixture (e-runtime-store-offline-test--make-v4 records catalog nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v4.sqlite3" directory))
         (payloads (mapcar #'e-runtime-store-offline-test--payload records)))
    (unwind-protect
        (let ((result (e-runtime-store-offline-upgrade directory backup)))
          (should (= (plist-get result :from) 4))
          (should (= (plist-get result :to) 7))
          (should (= (e-runtime-store-offline-test--version backup) 4))
          (should (= (e-runtime-store-offline-test--version database-file) 7))
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (should (equal
                           (mapcar (lambda (row)
                                     (e-runtime-store-offline-test--column row 0))
                                   (sqlite-select
                                    database
                                    "SELECT payload FROM session_records WHERE session_id=? ORDER BY position"
                                    (vector session-id)))
                           payloads))
                  (should (equal
                           (mapcar (lambda (row)
                                     (e-runtime-store-offline-test--column row 0))
                                   (sqlite-select
                                    database
                                    "SELECT identity FROM schema_migrations WHERE version IN (5,6,7) ORDER BY version"))
                           '("feature92-v4-to-v5-explicit-upgrade"
                             "feature92-v5-to-v6-explicit-upgrade"
                             "feature92-v6-to-v7-process-report-projection"))))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v4-post-transaction-fault-restores-source ()
  "A v4 post-install fault restores the original v4 image and payloads."
  (let* ((session-id "v4-fault")
         (records (list (e-runtime-store-offline-test--root session-id)))
         (fixture (e-runtime-store-offline-test--make-v4 records nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v4.sqlite3" directory))
         (payload (e-runtime-store-offline-test--payload (car records)))
         (process-environment
          (cons "E_RUNTIME_STORE_TEST_MIGRATION_FAULT=after-schema-transaction"
                process-environment)))
    (unwind-protect
        (progn
          (should-error (e-runtime-store-offline-upgrade directory backup))
          (should (= (e-runtime-store-offline-test--version database-file) 4))
          (should (= (e-runtime-store-offline-test--version backup) 4))
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (should (equal
                         (e-runtime-store-offline-test--column
                          (car (sqlite-select
                                database
                                "SELECT payload FROM session_records WHERE session_id=? AND position=1"
                                (vector session-id)))
                          0)
                         payload))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-context-erasure-fails-by-session ()
  "A standalone context-erasure record is a named non-derivable state."
  (let* ((session-id "context-erasure-session")
         (records
          (list (e-runtime-store-offline-test--root session-id)
                (e-runtime-store-offline-test--record
                 session-id "context-erasure" "erasure" 1
                 :context-record '(:record-version 2 :type context-erasure))))
         (fixture (e-runtime-store-offline-test--make-v5 records nil nil))
         (directory (nth 0 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (let ((err (should-error
                    (e-runtime-store-offline-upgrade directory backup))))
          (should (string-match-p (regexp-quote session-id)
                                  (error-message-string err)))
          (should (= (e-runtime-store-offline-test--version
                      (nth 1 fixture))
                     5)))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-position-gap-fails-by-session ()
  "A v5 journal gap is corruption, not a position the adapter may repair."
  (let* ((session-id "position-gap-session")
         (records (list (e-runtime-store-offline-test--root session-id)
                        (e-runtime-store-offline-test--message session-id)))
         (fixture (e-runtime-store-offline-test--make-v5 records nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (sqlite-execute
                 database
                 "UPDATE session_records SET position=3 WHERE session_id=? AND position=2"
                 (vector session-id))
              (sqlite-close database)))
          (let ((err (should-error
                      (e-runtime-store-offline-upgrade directory backup))))
            (should (string-match-p (regexp-quote session-id)
                                    (error-message-string err)))
            (should (= (e-runtime-store-offline-test--version database-file)
                       5))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-upgrade-installs-derived-v7-schema ()
  "A stopped v5 journal installs exact bounded v7 query state."
  (let* ((session-id "offline-session")
         (records (list (e-runtime-store-offline-test--root session-id)
                        (e-runtime-store-offline-test--message session-id)))
         (state (e-session-query-derive records))
         (catalog (e-runtime-store-offline-test--catalog state))
         (checkpoint
          (list :version 1 :session-id session-id
                :root (list :id "root"
                             :created-at "2026-09-06T00:00:00Z"
                             :updated-at "2026-09-06T00:00:00Z")
                :records nil))
         (fixture (e-runtime-store-offline-test--make-v5
                   records catalog checkpoint))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory))
         (payloads (mapcar #'e-runtime-store-offline-test--payload records)))
    (unwind-protect
        (let ((result (e-runtime-store-offline-upgrade directory backup)))
          (should (= (plist-get result :from) 5))
          (should (= (plist-get result :to) 7))
          (should (= (plist-get result :sessions) 1))
          (should (= (plist-get result :records) 2))
          (should (equal (plist-get result :integrity) "ok"))
          (should (= (e-runtime-store-offline-test--version database-file) 7))
          (should (= (e-runtime-store-offline-test--version backup) 5))
          (should (= (logand (file-modes backup) #o777) #o600))
          (should-not (e-runtime-store-offline-test--table-p
                       database-file "catalog_projection"))
          (should-not (e-runtime-store-offline-test--table-p
                       database-file "session_checkpoints"))
          (should-not (e-runtime-store-offline-test--table-p
                       database-file "session_records_v5"))
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (should (equal
                           (mapcar (lambda (row)
                                     (e-runtime-store-offline-test--column row 0))
                                   (sqlite-select
                                    database
                                    "SELECT payload FROM session_records WHERE session_id=? ORDER BY position"
                                    (vector session-id)))
                           payloads))
                  (should (equal
                           (e-runtime-store-offline-test--column
                            (car (sqlite-select
                                  database
                                  "SELECT record_type,record_id,parent_id,timestamp FROM session_records WHERE session_id=? AND position=2"
                                  (vector session-id)))
                            0)
                           "message"))
                  (should (equal
                           (e-runtime-store-offline-test--column
                            (car (sqlite-select
                                  database
                                  "SELECT name,summary,message_count,journal_position FROM session_query_state WHERE session_id=?"
                                  (vector session-id)))
                            0)
                           "Name offline-session"))
                  (should (=
                           (e-runtime-store-offline-test--column
                            (car (sqlite-select
                                  database
                                  "SELECT message_count FROM session_query_state WHERE session_id=?"
                                  (vector session-id)))
                            0)
                           1))
                  (should
                   (equal
                    (e-runtime-store-offline-test--migration-tuples database)
                    (list
                     (list 5 "feature92-direct-v5-source"
                           (secure-hash 'sha256 "feature92-schema-v5"))
                     (list 6 "feature92-v5-to-v6-explicit-upgrade"
                           (secure-hash 'sha256 "feature92-schema-v6"))
                     (list 7 "feature92-v6-to-v7-process-report-projection"
                           (secure-hash 'sha256
                                        "feature92-schema-v7-process-report-projection"))))))
              (sqlite-close database))))
          ;; The production worker may reopen the installed v7 image.  This
          ;; is deliberately a fresh owner, after the operator subprocess has
          ;; released its offline claim.
          (let ((store (e-runtime-store-open directory)))
            (unwind-protect
                (progn
                  (when-let* ((request
                               (e-runtime-store--open-control-request store)))
                    (e-runtime-store-await store request 5.0))
                  (should (= (plist-get (e-runtime-store-metrics store)
                                        :schema-version)
                             7)))
              (ignore-errors (e-runtime-store-close store))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-upgrade-preserves-large-journal-values ()
  "Migration projects scalars without applying row bounds to journal facts."
  (let* ((session-id "offline-large-session")
         (content (make-string 9000 ?m))
         (records
          (list
           (e-runtime-store-offline-test--root session-id)
           (e-runtime-store-offline-test--record
            session-id "message" "large-message" 1
            :message (list :id "large-message" :role 'user
                           :created-at "2026-09-06T00:00:01Z"
                           :content content
                           :provider-payload (make-string 12000 ?p)))))
         (fixture (e-runtime-store-offline-test--make-v5 records nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (e-runtime-store-offline-upgrade directory backup)
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (let* ((row (car (sqlite-select
                                  database
                                  "SELECT summary FROM session_query_state WHERE session_id=?"
                                  (vector session-id))))
                       (summary
                        (e-runtime-store-offline-test--column row 0))
                       (payload-row
                        (car (sqlite-select
                              database
                              "SELECT payload FROM session_records WHERE session_id=? AND position=2"
                              (vector session-id))))
                       (record
                        (e-runtime-store-codec-decode
                         (base64-decode-string
                          (e-runtime-store-offline-test--column payload-row 0)))))
                  (should (= (string-bytes summary)
                             e-session-query-state-string-byte-limit))
                  (should (string-prefix-p summary content))
                  (should (equal (plist-get (plist-get record :message) :content)
                                 content)))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-preserves-ignored-historical-family ()
  "An old ignored family remains query-neutral but advances journal order."
  (let* ((session-id "offline-historical-family")
         (records
          (list
           (e-runtime-store-offline-test--root session-id)
           (e-runtime-store-offline-test--record
            session-id "probe" "historical-probe" 1
            :payload (make-string 9000 ?p))))
         (fixture (e-runtime-store-offline-test--make-v5 records nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (e-runtime-store-offline-upgrade directory backup)
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (should (= (e-runtime-store-offline-test--column
                              (car (sqlite-select
                                    database
                                    "SELECT journal_position FROM session_query_state WHERE session_id=?"
                                    (vector session-id)))
                              0)
                             2))
                  (should (equal
                           (e-runtime-store-offline-test--column
                            (car (sqlite-select
                                  database
                                  "SELECT record_type FROM session_records WHERE session_id=? AND position=2"
                                  (vector session-id)))
                            0)
                           "probe")))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-later-root-replaces-query-state ()
  "A duplicate legacy root retains the old replay replacement semantics."
  (let* ((session-id "offline-repeated-root")
         (first-root (e-runtime-store-offline-test--root session-id))
         (message (e-runtime-store-offline-test--message session-id))
         (second-root
          (plist-put
           (plist-put (copy-sequence first-root) :id "replacement-root")
           :name "Replacement"))
         (fixture (e-runtime-store-offline-test--make-v5
                   (list first-root message second-root) nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (e-runtime-store-offline-upgrade directory backup)
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (let ((row (car (sqlite-select
                                 database
                                 "SELECT name,message_count,root_event_id,journal_position FROM session_query_state WHERE session_id=?"
                                 (vector session-id)))))
                  (should (equal (e-runtime-store-offline-test--column row 0)
                                 "Replacement"))
                  (should (= (e-runtime-store-offline-test--column row 1) 0))
                  (should (equal (e-runtime-store-offline-test--column row 2)
                                 "replacement-root"))
                  (should (= (e-runtime-store-offline-test--column row 3) 3)))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-preserves-rootless-historical-journal ()
  "Rootless legacy rows survive without fabricating session query state."
  (let* ((session-id "offline-rootless")
         (record
          (e-runtime-store-offline-test--record
           session-id "retired-legacy-note" "legacy-note" 0
           :value '(:author "client" :content "historical")))
         (fixture (e-runtime-store-offline-test--make-v5
                   (list record) nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (e-runtime-store-offline-upgrade directory backup)
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (should-not
                   (sqlite-select
                    database
                    "SELECT 1 FROM session_query_state WHERE session_id=?"
                    (vector session-id)))
                  (should (equal
                           (e-runtime-store-offline-test--column
                            (car (sqlite-select
                                  database
                                  "SELECT record_type FROM session_records WHERE session_id=? AND position=1"
                                  (vector session-id)))
                            0)
                           "retired-legacy-note")))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-fault-before-transaction-preserves-source ()
  "The pre-transaction fault leaves v5 untouched and keeps its backup."
  (let* ((session-id "before-fault")
         (records (list (e-runtime-store-offline-test--root session-id)))
         (fixture (e-runtime-store-offline-test--make-v5 records nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory))
         (process-environment
          (cons "E_RUNTIME_STORE_TEST_MIGRATION_FAULT=before-schema-transaction"
                process-environment)))
    (unwind-protect
        (progn
          (should-error (e-runtime-store-offline-upgrade directory backup))
          (should (= (e-runtime-store-offline-test--version database-file) 5))
          (should (= (e-runtime-store-offline-test--version backup) 5))
          (should (e-runtime-store-offline-test--table-p
                   database-file "catalog_projection")))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-replays-all-families-and-deletion ()
  "The migration replays every supported family and preserves delete control."
  (let* ((session-id "all-families")
         (records (e-runtime-store-offline-test--all-family-records session-id))
         (fixture (e-runtime-store-offline-test--make-v5 records nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory))
         (expected (e-session-query-derive records)))
    (unwind-protect
        (progn
          (e-runtime-store-offline-upgrade directory backup)
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (let ((row
                       (car (sqlite-select
                             database
                             "SELECT name,summary,message_count,current_branch,board_id,principal,association_role,journal_position FROM session_query_state WHERE session_id=?"
                             (vector session-id)))))
                  (should row)
                  (should (equal (e-runtime-store-offline-test--column row 0)
                                 (plist-get expected :name)))
                  (should (equal (e-runtime-store-offline-test--column row 1)
                                 (plist-get expected :summary)))
                  (should (= (e-runtime-store-offline-test--column row 2)
                             (plist-get expected :message-count)))
                  (should (equal (e-runtime-store-offline-test--column row 4)
                                 (plist-get expected :board-id)))
                  (should (equal (e-runtime-store-offline-test--column row 6)
                                 (plist-get expected :association-role)))
                  (should (= (e-runtime-store-offline-test--column row 7)
                             (length records))))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))
(ert-deftest e-runtime-store-offline-v5-deletion-removes-query-row ()
  "A canonical deletion removes only the derived query row."
  (let* ((session-id "deleted-session")
         (records (append
                   (list (e-runtime-store-offline-test--root session-id))
                   (list (e-runtime-store-offline-test--record
                          session-id "session-deleted" "deleted" 1))))
         (fixture (e-runtime-store-offline-test--make-v5 records nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (e-runtime-store-offline-upgrade directory backup)
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (should-not
                   (sqlite-select database
                                  "SELECT 1 FROM session_query_state WHERE session_id=?"
                                  (vector session-id)))
                  (should (= (e-runtime-store-offline-test--column
                              (car (sqlite-select
                                    database
                                    "SELECT COUNT(*) FROM session_records WHERE session_id=?"
                                    (vector session-id)))
                             2))))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-retired-projections-do-not-gate-journal ()
  "Missing or stale retired projections cannot reject canonical journals."
  (dolist (kind '(omission conflict))
    (let* ((session-a (format "witness-a-%s" kind))
           (session-b (format "witness-b-%s" kind))
           (records-a (list (e-runtime-store-offline-test--root session-a)))
           (records-b (list (e-runtime-store-offline-test--root session-b)))
           (state-a (e-session-query-derive records-a))
           (catalog
            (if (eq kind 'omission)
                (e-runtime-store-offline-test--catalog state-a)
              (list (list :id session-a :name "wrong-name"))))
           (fixture
            (e-runtime-store-offline-test--make-v5
             records-a catalog nil))
           (directory (nth 0 fixture))
           (database-file (nth 1 fixture))
           (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
      (unwind-protect
          (progn
            ;; Add the second journal only after fixture construction so this
            ;; test remains explicit about the physical v5 shape.
            (when (eq kind 'omission)
              (let ((database (sqlite-open database-file)))
                (unwind-protect
                    (sqlite-execute
                     database
                     "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
                     (vector session-b 1
                             (e-runtime-store-offline-test--payload
                              (car records-b))))
                  (sqlite-close database))))
            (let ((result
                   (e-runtime-store-offline-upgrade directory backup)))
              (should (= (plist-get result :to) 7))
              (let ((database (sqlite-open database-file)))
                (unwind-protect
                    (progn
                      (should (= (e-runtime-store-offline-test--column
                                  (car (sqlite-select
                                        database
                                        "SELECT COUNT(*) FROM session_query_state"))
                                  0)
                                 (if (eq kind 'omission) 2 1)))
                      (should-not (e-runtime-store-offline-test--table-p
                                   database-file "catalog_projection"))
                      (should-not (e-runtime-store-offline-test--table-p
                                   database-file "session_checkpoints")))
                  (sqlite-close database)))))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest e-runtime-store-offline-v5-noop-is-not-a-journal-family ()
  "A semantic no-op has no migrated journal row or invented family."
  (let* ((session-id "noop-session")
         (records (list (e-runtime-store-offline-test--root session-id)))
         (state (e-session-query-derive records))
         (noop (e-session-query-delta-noop state session-id)))
    (should (equal noop (list :session-id session-id :noop t)))
    (let* ((fixture (e-runtime-store-offline-test--make-v5 records nil nil))
           (directory (nth 0 fixture))
           (database-file (nth 1 fixture))
           (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
      (unwind-protect
          (progn
            (e-runtime-store-offline-upgrade directory backup)
            (let ((database (sqlite-open database-file)))
              (unwind-protect
                  (should-not
                   (sqlite-select database
                                  "SELECT 1 FROM session_records WHERE record_type='noop'"))
                (sqlite-close database))))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest e-runtime-store-offline-v5-scale-1277-sessions-is-bounded ()
  "Migration pages a generated 1,277-session shape without an Emacs mirror."
  (let* ((directory (make-temp-file "e-runtime-offline-scale-" t))
         (database-file (expand-file-name "store.sqlite3" directory))
         (database (sqlite-open database-file))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (dolist
              (statement
               '("CREATE TABLE store_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
                 "CREATE TABLE session_records (session_id TEXT NOT NULL, position INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(session_id, position))"
                 "CREATE TABLE session_checkpoints (session_id TEXT PRIMARY KEY, payload TEXT NOT NULL, revision INTEGER NOT NULL)"
                 "CREATE TABLE catalog_projection (singleton INTEGER PRIMARY KEY CHECK(singleton = 1), payload TEXT NOT NULL, revision INTEGER NOT NULL)"))
            (sqlite-execute database statement))
          (sqlite-execute database
                          "INSERT INTO store_meta(key,value) VALUES('schema_version','5')")
          (dotimes (index 1277)
            (let* ((session-id (format "scale-%04d" index))
                   (root (e-runtime-store-offline-test--root session-id)))
              (sqlite-execute
               database
               "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
               (vector session-id 1
                       (e-runtime-store-offline-test--payload root)))))
          (sqlite-close database)
          (setq database nil)
          (should (= (plist-get (e-runtime-store-offline-upgrade
                                 directory backup) :sessions)
                     1277))
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (should (= (e-runtime-store-offline-test--column
                            (car (sqlite-select
                                  database
                                  "SELECT COUNT(*) FROM session_query_state"))
                            0)
                           1277))
              (sqlite-close database))))
      (when database (sqlite-close database))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v5-fault-after-transaction-restores-source ()
  "The post-transaction fault restores the verified original v5 image."
  (let* ((session-id "after-fault")
         (records (list (e-runtime-store-offline-test--root session-id)))
         (fixture (e-runtime-store-offline-test--make-v5 records nil nil))
         (directory (nth 0 fixture))
         (database-file (nth 1 fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory))
         (payload (e-runtime-store-offline-test--payload (car records)))
         (process-environment
          (cons "E_RUNTIME_STORE_TEST_MIGRATION_FAULT=after-schema-transaction"
                process-environment)))
    (unwind-protect
        (progn
          (should-error (e-runtime-store-offline-upgrade directory backup))
          (should (= (e-runtime-store-offline-test--version database-file) 5))
          (should (e-runtime-store-offline-test--table-p
                   database-file "catalog_projection"))
          (should (= (e-runtime-store-offline-test--version backup) 5))
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (should (equal
                         (e-runtime-store-offline-test--column
                          (car (sqlite-select
                                database
                                "SELECT payload FROM session_records WHERE session_id=? AND position=1"
                                (vector session-id)))
                          0)
                         payload))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v6-to-v7-projects-mixed-history ()
  "The v6 upgrade projects semantic report facts before query bounds."
  (let* ((session-id "v6-process-reports")
         (root (e-runtime-store-offline-test--root session-id))
         (reports
          (list
           (list :type "process-report" :session-id session-id :id "marker-a"
                 :delta-id "marker-delta" :parent-id "root"
                 :timestamp "2026-09-06T00:00:01Z"
                 :report '(:report-type "marker" :id "marker-a"
                           :marker-id "marker-a" :evidence-id "evidence-a"))
           (list :type "process-report" :session-id session-id
                 :id "legacy-triage" :delta-id "triage-delta"
                 :parent-id "transcript-head" :timestamp "2026-09-06T00:00:02Z"
                 :report '(:report-type "triage" :id "legacy-triage"
                           :parent-id "transcript-head" :marker-id "marker-a"
                           :status "closed" :outcome "actionable"))
           (list :type "process-report" :session-id session-id :id "extraction"
                 :delta-id "extraction-delta" :parent-id "root"
                 :timestamp "2026-09-06T00:00:03Z"
                 :report '(:report-type "extraction" :id "extraction"
                           :marker-ids ["marker-a" "marker-a"]))))
         (unrelated
          (mapcar
           (lambda (index)
             (list :type "process-report" :session-id session-id
                   :id (format "other-%d" index)
                   :delta-id (format "other-delta-%d" index) :parent-id "root"
                   :timestamp "2026-09-06T00:00:04Z"
                   :report (list :report-type "other"
                                 :id (format "other-%d" index))))
           (number-sequence 0 299)))
         (shapes
          (list
           (list :type "process-report" :session-id session-id :id "shape-old"
                 :delta-id "shape-old-delta" :parent-id "root"
                 :timestamp "2026-09-06T00:00:05Z"
                 :report '(:report-type "request-shape" :id "shape-old"
                           :provider-request-id "request-a"))
           (list :type "process-report" :session-id session-id :id "shape-new"
                 :delta-id "shape-new-delta" :parent-id "root"
                 :timestamp "2026-09-06T00:00:06Z"
                 :report '(:report-type "request-shape" :id "shape-new"
                           :provider-request-id "request-a"))))
         (records (append (list root) reports unrelated shapes))
         (fixture (e-runtime-store-offline-test--make-v6 records))
         (directory (car fixture))
         (database-file (cadr fixture))
         (backup (expand-file-name "operator/pre-v6.sqlite3" directory)))
    (unwind-protect
        (let ((result (e-runtime-store-offline-upgrade directory backup)))
          (should (= (plist-get result :from) 6))
          (should (= (plist-get result :to) 7))
          (should (= (plist-get result :projection-base-rows) 305))
          (should (= (plist-get result :projection-association-rows) 3))
          (let ((runtime (e-runtime-store-open directory)))
            (unwind-protect
                (progn
                  (should
                   (equal
                    (plist-get
                     (plist-get
                      (e-runtime-store-call
                       runtime 'read
                       (list :op 'session-process-report-marker
                             :session-id session-id :marker-id "marker-a"))
                      :marker)
                     :id)
                    "marker-a"))
                  (should
                   (equal
                    (plist-get
                     (car
                      (plist-get
                       (e-runtime-store-call
                        runtime 'read
                        (list :op 'session-process-report-triage-page
                              :session-id session-id :marker-id "marker-a"
                              :limit 1))
                       :triage))
                     :id)
                    "legacy-triage"))
                  (should
                   (equal
                    (plist-get
                     (plist-get
                      (car
                       (plist-get
                        (e-runtime-store-call
                         runtime 'read
                         (list :op 'session-process-report-marker-page
                               :session-id session-id :status "closed"
                               :limit 1))
                        :markers))
                      :marker)
                     :id)
                    "marker-a"))
                  (should
                   (equal
                    (plist-get
                     (car
                      (plist-get
                       (e-runtime-store-call
                        runtime 'read
                        (list :op 'session-process-report-extraction-page
                              :session-id session-id :marker-id "marker-a"
                              :limit 1))
                       :extractions))
                     :id)
                    "extraction"))
                  (should
                   (equal
                    (plist-get
                     (car
                      (plist-get
                       (e-runtime-store-call
                        runtime 'read
                        (list :op 'session-process-report-request-shapes
                              :session-id session-id
                              :provider-request-ids ["request-a"]))
                       :request-shapes))
                     :id)
                    "shape-new"))
                  (should
                   (= (plist-get
                       (e-runtime-store-call
                        runtime 'read
                        (list :op 'session-process-report-marker-count
                              :session-id session-id))
                       :marker-count)
                      1)))
              (e-runtime-store-close runtime)))
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (should
                   (equal
                    (mapcar (lambda (row)
                              (e-runtime-store-offline-test--column row 1))
                            (sqlite-select
                             database
                             "PRAGMA table_info(session_process_report_index)"))
                    '("session_id" "position" "association_ordinal"
                      "report_type" "marker_id" "provider_request_id"
                      "triage_status")))
                  (should (= (e-runtime-store-offline-test--column
                              (car (sqlite-select
                                    database
                                    "SELECT COUNT(*) FROM pragma_foreign_key_list('session_process_report_index')"))
                              0)
                             2)))
              (sqlite-close database))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-historical-fresh-v5-lineage-normalizes ()
  "The exact historical fresh-v5 identity upgrades to normalized provenance."
  (let* ((session-id "historical-v5")
         (fixture
          (e-runtime-store-offline-test--make-v5
           (list (e-runtime-store-offline-test--root session-id))))
         (directory (car fixture))
         (database-file (cadr fixture))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (sqlite-execute
                   database
                   "CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, identity TEXT NOT NULL, checksum TEXT NOT NULL, applied_at REAL NOT NULL)")
                  (sqlite-execute
                   database
                   "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(5,'new-current-schema',?,0)"
                   (vector (secure-hash 'sha256 "feature92-schema-v5"))))
              (sqlite-close database)))
          (let ((result (e-runtime-store-offline-upgrade directory backup)))
            (should (= (plist-get result :from) 5))
            (should (= (plist-get result :to) 7)))
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (should
                 (equal
                  (e-runtime-store-offline-test--column
                   (car
                    (sqlite-select
                     database
                     "SELECT identity FROM schema_migrations WHERE version=5"))
                   0)
                  "feature92-direct-v5-source"))
              (sqlite-close database)))
          (let ((runtime (e-runtime-store-open directory)))
            (unwind-protect
                (should (= (plist-get
                            (e-runtime-store-call
                             runtime 'read '(:op store-metrics))
                            :schema-version)
                           7))
              (e-runtime-store-close runtime))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-historical-v6-predecessor-normalizes ()
  "A migrated v6 with exact historical v5 provenance normalizes and reopens."
  (let* ((session-id "historical-v6")
         (fixture
          (e-runtime-store-offline-test--make-v6
           (list (e-runtime-store-offline-test--root session-id))))
         (directory (car fixture))
         (database-file (cadr fixture))
         (backup (expand-file-name "operator/pre-v6.sqlite3" directory)))
    (unwind-protect
        (progn
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (progn
                  (sqlite-execute
                   database
                   "UPDATE schema_migrations SET identity='feature92-v5-to-v6-explicit-upgrade' WHERE version=6")
                  (sqlite-execute
                   database
                   "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(5,'new-current-schema',?,0)"
                   (vector (secure-hash 'sha256 "feature92-schema-v5"))))
              (sqlite-close database)))
          (let ((result (e-runtime-store-offline-upgrade directory backup)))
            (should (= (plist-get result :from) 6))
            (should (= (plist-get result :to) 7)))
          (let ((database (sqlite-open database-file)))
            (unwind-protect
                (should
                 (equal
                  (e-runtime-store-offline-test--column
                   (car
                    (sqlite-select
                     database
                     "SELECT identity FROM schema_migrations WHERE version=5"))
                   0)
                  "feature92-direct-v5-source"))
              (sqlite-close database)))
          (let ((runtime (e-runtime-store-open directory)))
            (unwind-protect
                (should (= (plist-get
                            (e-runtime-store-call
                             runtime 'read '(:op store-metrics))
                            :schema-version)
                           7))
              (e-runtime-store-close runtime))))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v7-is-verified-read-only-noop ()
  "A verified v7 store creates no backup and changes no migration rows."
  (let* ((directory (make-temp-file "e-runtime-offline-v7-" t))
         (runtime (e-runtime-store-open directory))
         (backup (expand-file-name "operator/unused.sqlite3" directory)))
    (unwind-protect
        (progn
          (e-runtime-store-call runtime 'read '(:op store-metrics))
          (e-runtime-store-close runtime)
          (setq runtime nil)
          (let* ((database-file (expand-file-name "store.sqlite3" directory))
                 (before (let ((database (sqlite-open database-file)))
                           (unwind-protect
                               (sqlite-select
                                database
                                "SELECT version,identity,checksum FROM schema_migrations ORDER BY version")
                             (sqlite-close database))))
                 (hash-before
                  (e-runtime-store-offline-test--file-hash database-file))
                 (result (e-runtime-store-offline-upgrade directory backup))
                 (after (let ((database (sqlite-open database-file)))
                          (unwind-protect
                              (sqlite-select
                               database
                               "SELECT version,identity,checksum FROM schema_migrations ORDER BY version")
                            (sqlite-close database)))))
            (should (plist-get result :noop))
            (should-not (plist-get result :backup))
            (should-not (file-exists-p backup))
            (should (equal before after))
            (should
             (equal
              before
              (list
               (list 7 "new-current-schema"
                     (secure-hash
                      'sha256
                      "feature92-schema-v7-process-report-projection")))))
            (should (equal hash-before
                           (e-runtime-store-offline-test--file-hash
                            database-file)))))
      (when runtime (e-runtime-store-close runtime))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v7-malformed-shape-is-not-repaired ()
  "Ordinary startup rejects malformed current shape without running DDL repair."
  (dolist (case '(missing-table malformed-index split-foreign-keys))
    (let* ((fixture (e-runtime-store-offline-test--make-v7))
           (directory (car fixture))
           (database-file (cadr fixture))
           (runtime nil)
           expected-index-sql
           expected-foreign-keys)
      (unwind-protect
          (progn
            (let ((database (sqlite-open database-file)))
              (unwind-protect
                  (pcase case
                    ('missing-table
                     (sqlite-execute
                      database "DROP TABLE session_process_report_index"))
                    ('malformed-index
                     (sqlite-execute
                      database "DROP INDEX session_process_report_marker_page")
                     (sqlite-execute
                      database
                      (concat
                       "CREATE INDEX session_process_report_marker_page "
                       "ON session_process_report_index(session_id,position ASC) "
                       "WHERE association_ordinal=0"))
                     (setq expected-index-sql
                           (e-runtime-store-offline-test--column
                            (car
                             (sqlite-select
                              database
                             "SELECT sql FROM sqlite_master WHERE name='session_process_report_marker_page'"))
                            0)))
                    ('split-foreign-keys
                     (sqlite-execute
                      database "DROP TABLE session_process_report_index")
                     (sqlite-execute
                      database
                      (concat
                       "CREATE TABLE session_process_report_index ("
                       "session_id TEXT NOT NULL, position INTEGER NOT NULL, "
                       "association_ordinal INTEGER NOT NULL, "
                       "report_type TEXT NOT NULL, marker_id TEXT, "
                       "provider_request_id TEXT, triage_status TEXT, "
                       "PRIMARY KEY(session_id,position,association_ordinal), "
                       "FOREIGN KEY(session_id) REFERENCES session_records(session_id) ON DELETE CASCADE, "
                       "FOREIGN KEY(position) REFERENCES session_records(position) ON DELETE CASCADE)"))
                     (e-runtime-store-session-worker-initialize-process-report-projection
                      database)
                     (setq expected-foreign-keys
                           (sqlite-select
                            database
                            "PRAGMA foreign_key_list(session_process_report_index)"))))
                (sqlite-close database)))
            (setq runtime (e-runtime-store-open directory))
            (should-error
             (e-runtime-store-call runtime 'read '(:op store-metrics))
             :type 'e-runtime-store-schema-too-old)
            (ignore-errors (e-runtime-store-close runtime))
            (setq runtime nil)
            (let ((database (sqlite-open database-file)))
              (unwind-protect
                  (pcase case
                    ('missing-table
                     (should-not
                      (car
                       (sqlite-select
                        database
                        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_process_report_index'"))))
                    ('malformed-index
                     (should
                      (equal
                       (e-runtime-store-offline-test--column
                        (car
                         (sqlite-select
                          database
                          "SELECT sql FROM sqlite_master WHERE name='session_process_report_marker_page'"))
                        0)
                       expected-index-sql)))
                    ('split-foreign-keys
                     (let ((actual
                            (sqlite-select
                             database
                             "PRAGMA foreign_key_list(session_process_report_index)")))
                       (should (equal actual expected-foreign-keys))
                       (should
                        (= (length
                            (delete-dups
                             (mapcar
                              (lambda (row)
                                (e-runtime-store-offline-test--column row 0))
                              actual)))
                           2)))))
                (sqlite-close database))))
        (when runtime (ignore-errors (e-runtime-store-close runtime)))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest e-runtime-store-offline-v7-invalid-lineage-is-read-only-failure ()
  "Missing or altered v7 provenance rejects without backup or database writes."
  (dolist (mutation
           '("DELETE FROM schema_migrations WHERE version=7"
             "UPDATE schema_migrations SET checksum='bad' WHERE version=7"
             "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(6,'new-current-schema','bad',0)"))
    (let* ((fixture (e-runtime-store-offline-test--make-v7))
           (directory (car fixture))
           (database-file (cadr fixture))
           (backup (expand-file-name "operator/unused.sqlite3" directory)))
      (unwind-protect
          (progn
            (let ((database (sqlite-open database-file)))
              (unwind-protect
                  (sqlite-execute database mutation)
                (sqlite-close database)))
            (let ((before
                   (e-runtime-store-offline-test--file-hash database-file)))
              (should-error
               (e-runtime-store-offline-upgrade directory backup))
              (should-not (file-exists-p backup))
              (should
               (equal before
                      (e-runtime-store-offline-test--file-hash
                       database-file)))))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest e-runtime-store-offline-migrated-v6-requires-v5-provenance ()
  "A migrated v6 source without recognized v5 provenance fails pre-backup."
  (dolist (v5-row
           '(nil
             (5 "feature92-direct-v5-source" "bad")
             (5 "unknown-v5-source" "bad")))
    (let* ((fixture
            (e-runtime-store-offline-test--make-v6
             (list (e-runtime-store-offline-test--root "lineage-v6"))))
           (directory (car fixture))
           (database-file (cadr fixture))
           (backup (expand-file-name "operator/unused.sqlite3" directory)))
      (unwind-protect
          (progn
            (let ((database (sqlite-open database-file)))
              (unwind-protect
                  (progn
                    (sqlite-execute
                     database
                     "UPDATE schema_migrations SET identity='feature92-v5-to-v6-explicit-upgrade' WHERE version=6")
                    (when v5-row
                      (sqlite-execute
                       database
                       "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(?,?,?,0)"
                       (vconcat v5-row))))
                (sqlite-close database)))
            (should-error
             (e-runtime-store-offline-upgrade directory backup))
            (should-not (file-exists-p backup)))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest e-runtime-store-offline-worker-symlink-build-resolves-real-core-directory ()
  "The offline launcher resolves sibling dependencies from the real checkout."
  (let* ((fixture (e-runtime-store-offline-test--make-v7))
         (directory (car fixture))
         (core-directory
          (expand-file-name "../lisp/core"
                            e-runtime-store-offline-test--source-directory))
         (build-directory (make-temp-file "e-runtime-store-offline-build-" t))
         (worker-file
          (expand-file-name "e-runtime-store-offline-worker.el"
                            build-directory))
         (backup (expand-file-name "unused.sqlite3" directory)))
    (unwind-protect
        (progn
          (make-symbolic-link
           (expand-file-name "e-runtime-store-offline-worker.el" core-directory)
           worker-file)
          (should-not (file-exists-p (expand-file-name "e-json.el"
                                                        build-directory)))
          (cl-letf (((symbol-function 'e-runtime-store-offline--worker-file)
                     (lambda () worker-file)))
            (let ((result (e-runtime-store-offline-upgrade directory backup)))
              (should (plist-get result :noop)))))
      (when (file-directory-p build-directory)
        (delete-directory build-directory t))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-worker-pre-main-failure-is-bounded ()
  "Offline startup failures retain status and bounded stderr diagnostics."
  (let* ((fixture (e-runtime-store-offline-test--make-v7))
         (directory (car fixture))
         (worker-file (make-temp-file "e-runtime-store-offline-bad-" nil ".el"))
         (marker "synthetic offline startup failure")
         (payload (concat marker " "
                         (make-string
                          (+ e-runtime-store-offline-startup-diagnostic-byte-limit 512)
                          ?x)))
         (backup (expand-file-name "unused.sqlite3" directory)))
    (unwind-protect
        (progn
          (write-region (format "(error %S)\n" payload)
                        nil worker-file nil 'silent)
          (cl-letf (((symbol-function 'e-runtime-store-offline--worker-file)
                     (lambda () worker-file)))
            (let* ((failure
                    (should-error
                     (e-runtime-store-offline-upgrade directory backup)
                     :type 'e-runtime-store-offline-error))
                   (data (cddr failure))
                   (stderr (plist-get data :stderr)))
              (should (= (plist-get data :exit-status) 1))
              (should (string-match-p (regexp-quote marker) stderr))
              (should (plist-get data :stderr-truncated))
              (should (<= (string-bytes stderr)
                          e-runtime-store-offline-startup-diagnostic-byte-limit)))))
      (when (file-exists-p worker-file) (delete-file worker-file))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-v7-stage-faults-preserve-source ()
  "Every injected precommit stage rolls back and postcommit restores backup."
  (dolist (spec '((4 "after-v5")
                  (5 "after-v6")
                  (6 "after-v7-schema")
                  (6 "after-v7-populate")
                  (6 "after-v7-parity")
                  (6 "after-schema-transaction")))
    (let* ((version (car spec))
           (mode (cadr spec))
           (session-id (format "fault-%d-%s" version mode))
           (records (list (e-runtime-store-offline-test--root session-id)))
           (fixture (pcase version
                      (4 (e-runtime-store-offline-test--make-v4 records))
                      (5 (e-runtime-store-offline-test--make-v5 records))
                      (6 (e-runtime-store-offline-test--make-v6 records))))
           (directory (car fixture))
           (database-file (cadr fixture))
           (backup (expand-file-name "operator/source.sqlite3" directory))
           (process-environment
            (cons (concat "E_RUNTIME_STORE_TEST_MIGRATION_FAULT=" mode)
                  process-environment)))
      (unwind-protect
          (progn
            (should-error
             (e-runtime-store-offline-upgrade directory backup))
            (should (= (e-runtime-store-offline-test--version database-file)
                       version))
            (should (= (e-runtime-store-offline-test--version backup) version)))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest e-runtime-store-offline-ordinary-startup-rejects-v6 ()
  "Ordinary startup rejects v6 before creating the v7 projection."
  (let* ((session-id "reject-v6")
         (fixture
          (e-runtime-store-offline-test--make-v6
           (list (e-runtime-store-offline-test--root session-id))))
         (directory (car fixture))
         (database-file (cadr fixture))
         (runtime nil))
    (unwind-protect
        (progn
          (setq runtime (e-runtime-store-open directory))
          (should-error
           (e-runtime-store-call runtime 'read '(:op store-metrics))
           :type 'e-runtime-store-schema-too-old)
          (should-not
           (e-runtime-store-offline-test--table-p
            database-file "session_process_report_index")))
      (when runtime (ignore-errors (e-runtime-store-close runtime)))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-offline-malformed-projection-diagnostic-is-bounded ()
  "Malformed v6 projection input fails before backup without payload disclosure."
  (let* ((session-id "malformed-v6")
         (secret "PAYLOAD-MUST-NOT-APPEAR")
         (root (e-runtime-store-offline-test--root session-id))
         (bad
          (list :type "process-report" :session-id session-id :id "bad"
                :delta-id "bad-delta" :parent-id "root"
                :timestamp "2026-09-06T00:00:01Z"
                :report
                (list :report-type "extraction" :secret secret
                      :marker-ids
                      (vconcat
                       (mapcar (lambda (index) (format "marker-%d" index))
                               (number-sequence 0 64))))))
         (fixture (e-runtime-store-offline-test--make-v6 (list root bad)))
         (directory (car fixture))
         (backup (expand-file-name "operator/source.sqlite3" directory)))
    (unwind-protect
        (let* ((error (should-error
                       (e-runtime-store-offline-upgrade directory backup)))
               (message (error-message-string error)))
          (should (string-match-p (regexp-quote session-id) message))
          (should (string-match-p "marker-ids" message))
          (should (string-match-p "64" message))
          (should-not (string-match-p secret message))
          (should-not (file-exists-p backup)))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(provide 'e-runtime-store-offline-test)

;;; e-runtime-store-offline-test.el ends here

;;; e-runtime-store-session-worker.el --- Session SQLite schema and operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module is the worker-side physical boundary for session history and
;; current query state.  The generic runtime worker owns process framing,
;; lifecycle, and receipt transactions.  A sealed semantic command is derived
;; against the authoritative current row inside that transaction, then the
;; journal record and relational query row are updated atomically.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'sqlite)
(require 'e-context-lifetime)
(require 'e-runtime-store-codec)
(require 'e-session-query)
(require 'e-session-query-command)
(require 'e-session-process-report-projection)
(require 'e-session-storage-limits)

(unless (get 'e-runtime-store-worker-error 'error-conditions)
  (define-error 'e-runtime-store-worker-error "Runtime store worker error"))

(defconst e-runtime-store-session-worker-page-row-limit 256
  "Maximum rows returned by one session query page.")
(defconst e-runtime-store-session-worker-record-page-row-limit 256
  "Maximum records returned by one bounded forward page.")
(defconst e-runtime-store-session-worker-failure-scan-row-limit 1024
  "Maximum recent activity rows inspected for failure navigation.")
(defconst e-runtime-store-session-worker-turn-inspection-row-limit 512
  "Maximum recent message/activity rows inspected for one failed turn.")
(defconst e-runtime-store-session-worker-visible-message-row-limit 64
  "Maximum messages returned by one visible chat window read.")
(defconst e-runtime-store-session-worker-context-path-row-limit 4096
  "Maximum canonical path records inspected for one provider request.")
(defconst e-runtime-store-session-worker-context-path-byte-limit (* 8 1024 1024)
  "Maximum detached message bytes returned for one provider request.")
(defconst e-runtime-store-session-worker-context-receipt-limit 8
  "Maximum non-erased tool receipts returned for one provider request.")
(defconst e-runtime-store-session-worker-context-receipt-byte-limit 4096
  "Maximum encoded bytes returned in the selected tool-receipt tail.")
(defconst e-runtime-store-session-worker-id-page-byte-limit (* 64 1024)
  "Maximum UTF-8 identity bytes returned by one session-id page.")
(defconst e-runtime-store-session-worker-page-byte-limit (* 1024 1024)
  "Maximum encoded payload bytes returned by one session page.")
(defconst e-runtime-store-session-worker-record-byte-limit (* 16 1024 1024)
  "Maximum legacy payload bytes accepted while reading one record.")

(defconst e-runtime-store-session-worker--state-columns
  '(session_id name summary metadata created_at updated_at last_message_at
    latest_assistant_marker message_count current_branch turn_options
    current_head_id root_event_id current_context_generation_id
    board_id principal association_role
    routing_policy root_p board_output_sequence board_activity_sequence
    journal_position)
  "Physical `session_query_state' columns in row-ABI order.")

(defvar e-runtime-store-session-worker--database nil)

(defun e-runtime-store-session-worker--error (message &rest data)
  "Signal a bounded worker error with MESSAGE and DATA."
  (signal 'e-runtime-store-worker-error (cons message data)))

(defun e-runtime-store-session-worker--column (row index)
  "Return INDEX from SQLite ROW across supported Emacs return shapes."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-runtime-store-session-worker--detach-query-content (value)
  "Return a detached copy of decoded consumer content VALUE.
Callers charge VALUE against their consumer-shaped byte budget before calling
this function.  Query-row scalar limits do not apply to message bodies or
selected context records."
  (copy-tree value t))

(defun e-runtime-store-session-worker--proper-plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist with unique keys."
  (and (proper-list-p value)
       (let ((tail value)
             (seen nil)
             valid)
         (setq valid t)
         (while (and valid tail)
           (let ((key (pop tail)))
             (setq valid
                   (and (keywordp key)
                        (consp tail)
                        (not (memq key seen))))
             (when valid
               (push key seen)
               (pop tail))))
         valid)))

(defun e-runtime-store-session-worker--scalar (value field)
  "Validate bounded scalar VALUE for physical FIELD and return VALUE."
  (when (and value
             (not (and (stringp value)
                       (<= (string-bytes value)
                           e-session-query-state-string-byte-limit))))
    (e-runtime-store-session-worker--error
     "Session physical scalar is invalid" field value))
  value)

(defun e-runtime-store-session-worker--required-scalar (value field)
  "Validate nonempty bounded scalar VALUE for physical FIELD.

Current query rows are page-sortable physical facts.  In particular their
creation and update timestamps cannot be absent, otherwise a stable cursor
could not identify the row it just returned."
  (unless (and (stringp value)
               (> (string-bytes value) 0)
               (<= (string-bytes value)
                   e-session-query-state-string-byte-limit))
    (e-runtime-store-session-worker--error
     "Session physical required scalar is invalid" field value))
  value)

(defun e-runtime-store-session-worker--session-id (value)
  "Validate and return a bounded SESSION-ID."
  (unless (and (stringp value)
               (> (string-bytes value) 0)
               (<= (string-bytes value)
                   e-session-query-state-string-byte-limit))
    (e-runtime-store-session-worker--error
     "Session identity is invalid" value))
  value)

(defun e-runtime-store-session-worker--nonnegative-integer (value field)
  "Validate nonnegative integer VALUE for FIELD."
  (unless (and (integerp value) (>= value 0))
    (e-runtime-store-session-worker--error
     "Session physical integer is invalid" field value))
  value)

(defun e-runtime-store-session-worker--copy-value (value field)
  "Validate, detach, and return bounded semantic VALUE for FIELD."
  (condition-case err
      (e-session-query--copy-value value)
    (error
     (e-runtime-store-session-worker--error
      "Session physical value is invalid" field (car err)))))

(defun e-runtime-store-session-worker--encode-value (value field)
  "Encode bounded semantic VALUE for nullable physical FIELD."
  (when value
    (condition-case err
        (base64-encode-string
         (e-runtime-store-codec-encode-bounded
          value e-session-query-state-value-byte-limit)
         t)
      (error
       (e-runtime-store-session-worker--error
        "Session physical value cannot be encoded" field (car err))))))

(defun e-runtime-store-session-worker--decode-value (text field)
  "Decode and detach nullable physical TEXT for FIELD."
  (when text
    (condition-case err
        (e-session-query--copy-value
         (e-runtime-store-codec-decode (base64-decode-string text)))
      (error
       (e-runtime-store-session-worker--error
       "Session physical value cannot be decoded" field (car err))))))

(defun e-runtime-store-session-worker--message-with-display
    (message encoded-disposition)
  "Return detached MESSAGE with its latest ENCODED-DISPOSITION applied.

The disposition row is selected in the same set query as the bounded message
consumer.  A nil payload means no disposition was ever recorded."
  (let ((result (e-runtime-store-session-worker--detach-query-content message)))
    (when encoded-disposition
      (let* ((record
              (condition-case err
                  (e-runtime-store-codec-decode
                   (base64-decode-string encoded-disposition))
                (error
                 (e-runtime-store-session-worker--error
                  "Session message disposition cannot be decoded" (car err)))))
             (display (plist-get record :display)))
        (unless (and (equal (plist-get record :type) "message-display")
                     (equal (plist-get record :id) (plist-get result :id))
                     (or (null display) (stringp display) (symbolp display)))
          (e-runtime-store-session-worker--error
           "Session message disposition has an invalid shape" record))
        (if display
            (plist-put result :display display)
          (cl-remf result :display))))
    result))

(defun e-runtime-store-session-worker--record-columns (record)
  "Return typed physical columns for canonical RECORD.

Only structural identity and ordering fields are extracted here.  Record
meaning remains owned by the session domain's query derivation."
  (unless (e-runtime-store-session-worker--proper-plist-p record)
    (e-runtime-store-session-worker--error
     "Session record is not a proper unique plist" record))
    (let* ((record-session-id (plist-get record :session-id))
         (type (plist-get record :type))
         (record-type (cond ((stringp type) type)
                            ((symbolp type) (symbol-name type))
                            (t nil)))
         (record-id (plist-get record :id))
         (delta-id (and (plist-member record :delta-id)
                        (plist-get record :delta-id)))
         (parent-id (plist-get record :parent-id))
         (timestamp (or (plist-get record :timestamp)
                        (plist-get record :created-at))))
    (unless (and (stringp record-session-id)
                 (> (string-bytes record-session-id) 0)
                 (<= (string-bytes record-session-id)
                     e-session-query-state-string-byte-limit)
                 (stringp record-type)
                 (> (string-bytes record-type) 0)
                 (<= (string-bytes record-type)
                     e-session-query-state-string-byte-limit))
      (e-runtime-store-session-worker--error
       "Session record identity or type is invalid" record))
    (when (and (equal record-type "session")
               (not (and (stringp timestamp) (> (string-bytes timestamp) 0))))
      (e-runtime-store-session-worker--error
       "Session root record requires a timestamp" record))
    (when delta-id
      (unless (and (stringp delta-id)
                   (> (string-bytes delta-id) 0)
                   (<= (string-bytes delta-id)
                       e-session-query-state-string-byte-limit))
        (e-runtime-store-session-worker--error
         "Session record delta identity is invalid" delta-id)))
    (dolist (entry (list (cons :id record-id)
                         (cons :parent-id parent-id)
                         (cons :timestamp timestamp)))
      (when (cdr entry)
        (unless (and (stringp (cdr entry))
                     (<= (string-bytes (cdr entry))
                         e-session-query-state-string-byte-limit))
          (e-runtime-store-session-worker--error
           "Session record typed field is invalid" (car entry) (cdr entry)))))
    ;; A missing/nil delta id is deliberately legacy-compatible and has no
    ;; uniqueness claim.  When present it is the durable structural identity
    ;; used for idempotent record admission, independent of semantic target
    ;; ids such as a message-display's :id.
    (list record-type record-id delta-id parent-id timestamp)))

(defun e-runtime-store-session-worker--validate-state (state)
  "Validate and detach a complete domain-owned query STATE."
  (condition-case err
      (progn
        (e-session-query-state-validate state)
        (e-session-query--copy-value state))
    (error
     (e-runtime-store-session-worker--error
      "Session query delta is not a complete row" (car err)))))

(defun e-runtime-store-session-worker--validate-delta (delta)
  "Validate and detach complete row or exact control DELTA."
  (cond
   ((and (e-runtime-store-session-worker--proper-plist-p delta)
         (plist-member delta :deleted))
    (condition-case err
        (progn
          (e-session-query-control-delta-validate delta)
          (e-session-query--copy-value delta))
      (error
       (e-runtime-store-session-worker--error
        "Session deletion delta is invalid" (car err)))))
   ((and (e-runtime-store-session-worker--proper-plist-p delta)
         (plist-member delta :noop))
    (condition-case err
        (progn
          (e-session-query-control-delta-validate delta)
          (e-session-query--copy-value delta))
      (error
       (e-runtime-store-session-worker--error
        "Session no-op delta is invalid" (car err)))))
   (t (e-runtime-store-session-worker--validate-state delta))))

(defun e-runtime-store-session-worker--state-values (state)
  "Return STATE's encoded SQL values in physical column order."
  (let ((state (e-runtime-store-session-worker--validate-state state)))
    (vector
     (e-runtime-store-session-worker--session-id
      (plist-get state :session-id))
     (e-runtime-store-session-worker--scalar (plist-get state :name) :name)
     (e-runtime-store-session-worker--scalar (plist-get state :summary) :summary)
     (e-runtime-store-session-worker--encode-value
      (plist-get state :metadata) :metadata)
     (e-runtime-store-session-worker--required-scalar
      (plist-get state :created-at) :created-at)
     (e-runtime-store-session-worker--required-scalar
      (plist-get state :updated-at) :updated-at)
     (e-runtime-store-session-worker--scalar
      (plist-get state :last-message-at) :last-message-at)
     (e-runtime-store-session-worker--scalar
      (plist-get state :latest-assistant-marker)
      :latest-assistant-marker)
     (e-runtime-store-session-worker--nonnegative-integer
      (plist-get state :message-count) :message-count)
     (e-runtime-store-session-worker--scalar
      (plist-get state :current-branch) :current-branch)
     (e-runtime-store-session-worker--encode-value
      (plist-get state :turn-options) :turn-options)
     (e-runtime-store-session-worker--scalar
      (plist-get state :current-head-id) :current-head-id)
     (e-runtime-store-session-worker--scalar
      (plist-get state :root-event-id) :root-event-id)
     (e-runtime-store-session-worker--scalar
      (plist-get state :current-context-generation-id)
      :current-context-generation-id)
     (e-runtime-store-session-worker--scalar (plist-get state :board-id) :board-id)
     (e-runtime-store-session-worker--scalar (plist-get state :principal) :principal)
     (e-runtime-store-session-worker--scalar
      (plist-get state :association-role) :association-role)
     (e-runtime-store-session-worker--encode-value
      (plist-get state :routing-policy) :routing-policy)
     (if (plist-get state :root-p) 1 0)
     (e-runtime-store-session-worker--nonnegative-integer
      (plist-get state :board-output-sequence) :board-output-sequence)
     (e-runtime-store-session-worker--nonnegative-integer
      (plist-get state :board-activity-sequence) :board-activity-sequence)
     (e-runtime-store-session-worker--nonnegative-integer
      (plist-get state :journal-position) :journal-position))))

(defun e-runtime-store-session-worker--state-from-row (row)
  "Return detached logical state represented by physical SQLite ROW."
  (when row
    (unless (= (length row) (length e-runtime-store-session-worker--state-columns))
      (e-runtime-store-session-worker--error
       "Session query row has an invalid physical shape" row))
    (let ((root-p (e-runtime-store-session-worker--column row 18)))
      (unless (memq root-p '(0 1))
        (e-runtime-store-session-worker--error
         "Session query root flag is invalid" root-p)))
    (e-runtime-store-session-worker--required-scalar
     (e-runtime-store-session-worker--column row 4) :created-at)
    (e-runtime-store-session-worker--required-scalar
     (e-runtime-store-session-worker--column row 5) :updated-at)
    (let ((state
           (list
            :session-id (e-runtime-store-session-worker--column row 0)
            :name (e-runtime-store-session-worker--column row 1)
            :summary (e-runtime-store-session-worker--column row 2)
            :metadata (e-runtime-store-session-worker--decode-value
                       (e-runtime-store-session-worker--column row 3) :metadata)
            :created-at (e-runtime-store-session-worker--column row 4)
            :updated-at (e-runtime-store-session-worker--column row 5)
            :last-message-at (e-runtime-store-session-worker--column row 6)
            :latest-assistant-marker (e-runtime-store-session-worker--column row 7)
            :message-count (e-runtime-store-session-worker--column row 8)
            :current-branch (e-runtime-store-session-worker--column row 9)
            :turn-options (e-runtime-store-session-worker--decode-value
                           (e-runtime-store-session-worker--column row 10)
                           :turn-options)
            :current-head-id (e-runtime-store-session-worker--column row 11)
            :root-event-id (e-runtime-store-session-worker--column row 12)
            :current-context-generation-id
            (e-runtime-store-session-worker--column row 13)
            :board-id (e-runtime-store-session-worker--column row 14)
            :principal (e-runtime-store-session-worker--column row 15)
            :association-role (e-runtime-store-session-worker--column row 16)
            :routing-policy (e-runtime-store-session-worker--decode-value
                             (e-runtime-store-session-worker--column row 17)
                             :routing-policy)
            :root-p (= 1 (e-runtime-store-session-worker--column row 18))
            :board-output-sequence (e-runtime-store-session-worker--column row 19)
            :board-activity-sequence (e-runtime-store-session-worker--column row 20)
            :journal-position (e-runtime-store-session-worker--column row 21))))
      (e-runtime-store-session-worker--validate-state state))))

(defun e-runtime-store-session-worker-initialize-process-report-projection (database)
  "Create the schema-v7 process-report projection on DATABASE."
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS session_process_report_index (session_id TEXT NOT NULL, position INTEGER NOT NULL, association_ordinal INTEGER NOT NULL, report_type TEXT NOT NULL, marker_id TEXT, provider_request_id TEXT, triage_status TEXT, PRIMARY KEY(session_id, position, association_ordinal), FOREIGN KEY(session_id, position) REFERENCES session_records(session_id, position) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS session_process_report_base_page ON session_process_report_index(session_id, report_type, position DESC) WHERE association_ordinal=0"
         "CREATE INDEX IF NOT EXISTS session_process_report_marker_page ON session_process_report_index(session_id, marker_id, report_type, position DESC) WHERE association_ordinal>0"
         "CREATE INDEX IF NOT EXISTS session_process_report_triage_status ON session_process_report_index(session_id, marker_id, position DESC, triage_status) WHERE association_ordinal > 0 AND report_type = 'triage' AND triage_status IS NOT NULL"
         "CREATE INDEX IF NOT EXISTS session_process_report_request_page ON session_process_report_index(session_id, provider_request_id, report_type, position DESC) WHERE association_ordinal=0 AND provider_request_id IS NOT NULL"))
    (sqlite-execute database statement)))

(defun e-runtime-store-session-worker--normalized-sql (value)
  "Return VALUE normalized for exact generated-schema comparisons."
  (and (stringp value)
       (replace-regexp-in-string "[[:space:]]+" "" (downcase value))))

(defun e-runtime-store-session-worker--process-report-index-shape
    (database name)
  "Return declared key column/order pairs for projection index NAME."
  (mapcar
   (lambda (row)
     (list (e-runtime-store-session-worker--column row 2)
           (e-runtime-store-session-worker--column row 3)))
   (seq-filter
    (lambda (row)
      (= (e-runtime-store-session-worker--column row 5) 1))
    (sqlite-select database (format "PRAGMA index_xinfo(%s)" name)))))

(defun e-runtime-store-session-worker-verify-process-report-projection-schema
    (database)
  "Verify DATABASE's exact schema-v7 process-report projection shape."
  (let* ((table-info
          (sqlite-select database
                         "PRAGMA table_info(session_process_report_index)"))
         (columns
          (mapcar
           (lambda (row)
             (list (e-runtime-store-session-worker--column row 1)
                   (e-runtime-store-session-worker--column row 2)
                   (e-runtime-store-session-worker--column row 3)
                   (e-runtime-store-session-worker--column row 4)
                   (e-runtime-store-session-worker--column row 5)))
           table-info))
         (foreign-key
          (sort
           (mapcar
            (lambda (row)
              (list (e-runtime-store-session-worker--column row 0)
                    (e-runtime-store-session-worker--column row 1)
                    (e-runtime-store-session-worker--column row 2)
                    (e-runtime-store-session-worker--column row 3)
                    (e-runtime-store-session-worker--column row 4)
                    (e-runtime-store-session-worker--column row 5)
                    (e-runtime-store-session-worker--column row 6)
                    (e-runtime-store-session-worker--column row 7)))
            (sqlite-select
             database
             "PRAGMA foreign_key_list(session_process_report_index)"))
           (lambda (left right)
             (or (< (nth 0 left) (nth 0 right))
                 (and (= (nth 0 left) (nth 0 right))
                      (< (nth 1 left) (nth 1 right)))))))
         (index-list
          (sqlite-select database
                         "PRAGMA index_list(session_process_report_index)"))
         (expectations
          '(("session_process_report_base_page"
             (("session_id" 0) ("report_type" 0) ("position" 1))
             "association_ordinal=0")
            ("session_process_report_marker_page"
             (("session_id" 0) ("marker_id" 0) ("report_type" 0)
              ("position" 1))
             "association_ordinal>0")
            ("session_process_report_triage_status"
             (("session_id" 0) ("marker_id" 0) ("position" 1)
              ("triage_status" 0))
             "association_ordinal > 0 AND report_type = 'triage' AND triage_status IS NOT NULL")
            ("session_process_report_request_page"
             (("session_id" 0) ("provider_request_id" 0)
              ("report_type" 0) ("position" 1))
             "association_ordinal=0 AND provider_request_id IS NOT NULL"))))
    (unless
        (equal columns
               '(("session_id" "TEXT" 1 nil 1)
                 ("position" "INTEGER" 1 nil 2)
                 ("association_ordinal" "INTEGER" 1 nil 3)
                 ("report_type" "TEXT" 1 nil 0)
                 ("marker_id" "TEXT" 0 nil 0)
                 ("provider_request_id" "TEXT" 0 nil 0)
                 ("triage_status" "TEXT" 0 nil 0)))
      (e-runtime-store-session-worker--error
       "Schema v7 process-report table shape is invalid"))
    (unless
        (equal foreign-key
               '((0 0 "session_records" "session_id" "session_id"
                    "NO ACTION" "CASCADE" "NONE")
                 (0 1 "session_records" "position" "position"
                    "NO ACTION" "CASCADE" "NONE")))
      (e-runtime-store-session-worker--error
       "Schema v7 process-report foreign key is invalid"))
    (dolist (expectation expectations)
      (let* ((name (car expectation))
             (listed
              (seq-find
               (lambda (row)
                 (equal (e-runtime-store-session-worker--column row 1) name))
               index-list))
             (sql-row
              (car (sqlite-select
                    database
                    "SELECT sql FROM sqlite_master WHERE type='index' AND name=?"
                    (vector name))))
             (sql (and sql-row
                       (e-runtime-store-session-worker--column sql-row 0)))
             (where (and sql (string-match "[[:space:]]WHERE[[:space:]]+\\(.+\\)\\'" sql)
                         (match-string 1 sql))))
        (unless (and listed
                     (= (e-runtime-store-session-worker--column listed 4) 1)
                     (equal
                      (e-runtime-store-session-worker--process-report-index-shape
                       database name)
                      (cadr expectation))
                     (equal
                      (e-runtime-store-session-worker--normalized-sql where)
                      (e-runtime-store-session-worker--normalized-sql
                       (caddr expectation))))
          (e-runtime-store-session-worker--error
           "Schema v7 process-report index shape is invalid" :index name))))
    t))

(defun e-runtime-store-session-worker-initialize-v6 (database)
  "Create the schema-v6 session relations and indexes on DATABASE.

This function is called from the generic worker's one schema transaction
boundary.  It intentionally creates neither the retired catalog projection
nor resume checkpoints."
  (let ((e-runtime-store-session-worker--database database))
    (dolist
        (statement
         '("CREATE TABLE IF NOT EXISTS session_records (session_id TEXT NOT NULL, position INTEGER NOT NULL, payload TEXT NOT NULL, record_type TEXT NOT NULL DEFAULT '', record_id TEXT, record_identity TEXT, parent_id TEXT, timestamp TEXT, PRIMARY KEY(session_id, position))"
           "CREATE UNIQUE INDEX IF NOT EXISTS session_records_identity ON session_records(session_id, record_identity) WHERE record_identity IS NOT NULL"
           "CREATE INDEX IF NOT EXISTS session_records_session_page ON session_records(session_id, position)"
           "CREATE INDEX IF NOT EXISTS session_records_type_page ON session_records(session_id, record_type, position)"
           "CREATE INDEX IF NOT EXISTS session_records_id_page ON session_records(session_id, record_id, position)"
           "CREATE INDEX IF NOT EXISTS session_records_record_identity_page ON session_records(session_id, record_identity, position)"
           "CREATE INDEX IF NOT EXISTS session_records_parent_page ON session_records(session_id, parent_id, position)"
           "CREATE TABLE IF NOT EXISTS session_query_state (session_id TEXT PRIMARY KEY, name TEXT, summary TEXT, metadata TEXT, created_at TEXT, updated_at TEXT, last_message_at TEXT, latest_assistant_marker TEXT, message_count INTEGER NOT NULL, current_branch TEXT, turn_options TEXT, current_head_id TEXT, root_event_id TEXT, current_context_generation_id TEXT, board_id TEXT, principal TEXT, association_role TEXT, routing_policy TEXT, root_p INTEGER NOT NULL, board_output_sequence INTEGER NOT NULL, board_activity_sequence INTEGER NOT NULL, journal_position INTEGER NOT NULL)"
           "CREATE INDEX IF NOT EXISTS session_query_state_recent ON session_query_state(updated_at DESC, session_id DESC)"
           "CREATE INDEX IF NOT EXISTS session_query_state_root ON session_query_state(root_p, updated_at DESC, session_id DESC)"
           "CREATE INDEX IF NOT EXISTS session_query_state_board ON session_query_state(board_id, principal, updated_at DESC, session_id DESC)"
           "CREATE INDEX IF NOT EXISTS session_query_state_cursor ON session_query_state(journal_position, session_id)"
           ;; This redundant v5 physical index has no logical meaning.  It is
           ;; safe to remove while opening an already-current store and keeps
           ;; the v6 schema from carrying duplicate position coverage.
           "DROP INDEX IF EXISTS session_records_position"))
      (sqlite-execute database statement))))

(defun e-runtime-store-session-worker-initialize (database)
  "Create the schema-v7 session relations and indexes on DATABASE."
  (e-runtime-store-session-worker-initialize-v6 database)
  (e-runtime-store-session-worker-initialize-process-report-projection database))

(defun e-runtime-store-session-worker--process-report-index-insert
    (database session-id position record)
  "Insert RECORD's derived process-report rows at POSITION."
  (condition-case err
      (dolist (row (e-session-process-report-projection-rows record))
        (sqlite-execute
         database
         "INSERT INTO session_process_report_index(session_id,position,association_ordinal,report_type,marker_id,provider_request_id,triage_status) VALUES(?,?,?,?,?,?,?)"
         (vector session-id position
                 (plist-get row :association-ordinal)
                 (plist-get row :report-type)
                 (plist-get row :marker-id)
                 (plist-get row :provider-request-id)
                 (plist-get row :triage-status))))
    (e-session-process-report-projection-error
     (e-runtime-store-session-worker--error
      "Process-report query projection is invalid"
      :session-id session-id :position position
      :field (plist-get (cddr err) :field)
      :limit (plist-get (cddr err) :limit)))))

(defun e-runtime-store-session-worker--record-insert
    (database session-id position record)
  "Insert RECORD at POSITION for SESSION-ID into DATABASE."
  (let* ((record-session-id (plist-get record :session-id))
         (columns (e-runtime-store-session-worker--record-columns record))
         (record-type (nth 0 columns))
         (record-id (nth 1 columns))
         (record-identity (nth 2 columns))
         (parent-id (nth 3 columns))
         (timestamp (nth 4 columns))
         (payload
          (condition-case err
              (e-runtime-store-codec-encode-bounded
               record e-session-storage-record-byte-limit)
            (error
             (e-runtime-store-session-worker--error
              "Session record payload is too large or invalid" (car err))))))
    (unless (equal session-id record-session-id)
      (e-runtime-store-session-worker--error
       "Session record identity does not match append owner"
       session-id record-session-id))
    (condition-case err
        (progn
          (sqlite-execute
           database
           "INSERT INTO session_records(session_id,position,payload,record_type,record_id,record_identity,parent_id,timestamp) VALUES(?,?,?,?,?,?,?,?)"
           (vector session-id position (base64-encode-string payload t)
                   record-type record-id record-identity parent-id timestamp))
          (e-runtime-store-session-worker--process-report-index-insert
           database session-id position record))
      (sqlite-error
       ;; Keep physical uniqueness failures inside the worker's typed error
       ;; vocabulary; the generic transaction then rolls back every prior
       ;; insert in the request.
       (e-runtime-store-session-worker--error
        "Session record structural identity already exists"
        :session-id session-id :record-identity record-identity
        :sqlite-error (car err))))))

(defun e-runtime-store-session-worker--position (database session-id)
  "Return current durable high-water POSITION for SESSION-ID."
  (or (let ((row (car (sqlite-select
                       database
                       "SELECT MAX(position) FROM session_records WHERE session_id=?"
                       (vector session-id)))))
        (and row (e-runtime-store-session-worker--column row 0)))
      0))

(defun e-runtime-store-session-worker--assert-delta-position
    (delta expected-position)
  "Require non-control DELTA's journal position to equal EXPECTED-POSITION.

The journal position is supplied by the application/service adapter after it
has assigned the actual durable position.  It is not a replay counter the
worker may silently repair."
  (unless (or (plist-get delta :deleted) (plist-get delta :noop))
    (unless (= (plist-get delta :journal-position) expected-position)
      (e-runtime-store-session-worker--error
       "Session query delta position does not match journal append"
       :expected expected-position
       :actual (plist-get delta :journal-position)))))

(defun e-runtime-store-session-worker--write-delta
    (database session-id delta &optional expected-position)
  "Apply validated query DELTA for SESSION-ID to DATABASE.

When EXPECTED-POSITION is supplied, a non-control delta must name exactly
that position before its row is written."
  (let ((delta (e-runtime-store-session-worker--validate-delta delta)))
    (unless (equal session-id (plist-get delta :session-id))
      (e-runtime-store-session-worker--error
       "Session delta identity does not match record" session-id delta))
    (when expected-position
      (e-runtime-store-session-worker--assert-delta-position
       delta expected-position))
    (cond
     ((plist-get delta :deleted)
      (sqlite-execute database
                      "DELETE FROM session_query_state WHERE session_id=?"
                      (vector session-id))
      (list :session-id session-id :deleted t))
     ((plist-get delta :noop)
      (list :session-id session-id :noop t))
     (t
      (sqlite-execute
       database
       (concat
        "INSERT INTO session_query_state(session_id,name,summary,metadata,created_at,updated_at,last_message_at,latest_assistant_marker,message_count,current_branch,turn_options,current_head_id,root_event_id,current_context_generation_id,board_id,principal,association_role,routing_policy,root_p,board_output_sequence,board_activity_sequence,journal_position) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
        "ON CONFLICT(session_id) DO UPDATE SET name=excluded.name,summary=excluded.summary,metadata=excluded.metadata,created_at=excluded.created_at,updated_at=excluded.updated_at,last_message_at=excluded.last_message_at,latest_assistant_marker=excluded.latest_assistant_marker,message_count=excluded.message_count,current_branch=excluded.current_branch,turn_options=excluded.turn_options,current_head_id=excluded.current_head_id,root_event_id=excluded.root_event_id,current_context_generation_id=excluded.current_context_generation_id,board_id=excluded.board_id,principal=excluded.principal,association_role=excluded.association_role,routing_policy=excluded.routing_policy,root_p=excluded.root_p,board_output_sequence=excluded.board_output_sequence,board_activity_sequence=excluded.board_activity_sequence,journal_position=excluded.journal_position")
       (e-runtime-store-session-worker--state-values delta))
      delta))))

(defun e-runtime-store-session-worker--append (database body)
  "Append one session record and its complete query DELTA."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (record (plist-get body :record))
         (delta (or (plist-get body :query-delta)
                    (plist-get body :delta)))
         (position (1+ (e-runtime-store-session-worker--position
                       database session-id))))
    (unless delta
      (e-runtime-store-session-worker--error
       "Session append requires a query delta" session-id))
    (setq delta (e-runtime-store-session-worker--validate-delta delta))
    (when (plist-get delta :noop)
      (e-runtime-store-session-worker--error
       "Semantic no-op has no durable session append" session-id))
    (e-runtime-store-session-worker--assert-delta-position delta position)
    (e-runtime-store-session-worker--record-insert
     database session-id position record)
    (e-runtime-store-session-worker--write-delta
     database session-id delta position)
    (let ((result
           (list :session-id session-id :position position
                 :high-water position)))
      result)))

(defun e-runtime-store-session-worker--append-batch (database body)
  "Append BODY's records and final complete query DELTA atomically."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (records (plist-get body :records))
         (delta (or (plist-get body :query-delta)
                    (plist-get body :delta)))
         (position (e-runtime-store-session-worker--position
                    database session-id))
         (first nil)
         (last position))
    (unless (or (listp records) (vectorp records))
      (e-runtime-store-session-worker--error
       "Session batch records must be a list or vector"))
    (setq records (append records nil))
    (when (null records)
      (e-runtime-store-session-worker--error
       "Session batch must contain at least one durable record" session-id))
    (when (> (length records) e-session-storage-batch-record-limit)
      (e-runtime-store-session-worker--error
       "Session batch exceeds record-count limit" (length records)))
    (condition-case err
        (e-runtime-store-codec-measure-bounded
         body e-session-storage-batch-byte-limit)
      (error
       (e-runtime-store-session-worker--error
        "Session batch exceeds byte limit" (car err))))
    (unless delta
      (e-runtime-store-session-worker--error
       "Session batch requires a query delta" session-id))
    (setq delta (e-runtime-store-session-worker--validate-delta delta))
    (when (plist-get delta :noop)
      (e-runtime-store-session-worker--error
       "Semantic no-op has no durable session batch" session-id))
    ;; The batch delta describes the final row after the final inserted
    ;; journal position, not the number of records in this request.
    (e-runtime-store-session-worker--assert-delta-position
     delta (+ position (length records)))
    (dolist (record records)
      (setq last (1+ last)
            first (or first last))
      (e-runtime-store-session-worker--record-insert
       database session-id last record))
    (e-runtime-store-session-worker--write-delta
     database session-id delta (+ position (length records)))
    (let ((result
           (list :session-id session-id :first-position (and first first)
                 :last-position last :high-water last)))
      result)))

(defun e-runtime-store-session-worker--delete (database body)
  "Apply exact delete control BODY and remove its session records."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (delta (or (plist-get body :query-delta)
                    (plist-get body :delta)
                    (list :session-id session-id :deleted t))))
    (setq delta (e-runtime-store-session-worker--validate-delta delta))
    (unless (plist-get delta :deleted)
      (e-runtime-store-session-worker--error
       "Session delete requires a deleted control delta" delta))
    (e-runtime-store-session-worker--write-delta database session-id delta)
    (sqlite-execute database "DELETE FROM session_records WHERE session_id=?"
                    (vector session-id))
    (dolist (table '("tool_followups" "resources"))
      (sqlite-execute database
                      (format "DELETE FROM %s WHERE session_id=?" table)
                      (vector session-id)))
    (list :session-id session-id :deleted t)))

(defun e-runtime-store-session-worker--command (database body)
  "Interpret and commit BODY's sealed session command transactionally."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (command (e-session-query-command-from-wire
                   (plist-get body :command)))
         (command-session-id
          (e-session-aggregate-command-session-id command))
         (state (e-runtime-store-session-worker--query-state
                 database (list :session-id session-id)))
         (delta nil))
    (unless (equal session-id command-session-id)
      (e-runtime-store-session-worker--error
       "Session command identity mismatch" session-id command-session-id))
    ;; This read and the writes below run in one SQLite transaction.  Callers
    ;; need no Emacs-side read/derive FIFO.  Submission order is not a semantic
    ;; guarantee; acknowledged commits and explicit multi-change transactions
    ;; are the ordering boundaries.
    (setq delta (e-session-query-command-interpret state command))
    (if (eq (e-session-aggregate-command-tag command) 'delete)
        (e-runtime-store-session-worker--delete
         database
         (list :session-id session-id
               :query-delta (plist-get delta :query-delta)))
      (e-runtime-store-session-worker--append
       database
       (list :session-id session-id
             :record (plist-get delta :record)
             :query-delta (plist-get delta :query-delta))))
    (copy-tree (plist-get delta :result) t)))

(defun e-runtime-store-session-worker-write (database body)
  "Execute a typed session write BODY on transaction-scoped DATABASE."
  (let ((e-runtime-store-session-worker--database database))
    (pcase (plist-get body :op)
      ('session-command
       (e-runtime-store-session-worker--command database body))
      ('session-append (e-runtime-store-session-worker--append database body))
      ('session-append-with-tool-transition
       (e-runtime-store-session-worker--append database body))
      ('session-append-batch
       (e-runtime-store-session-worker--append-batch database body))
      ('session-delete (e-runtime-store-session-worker--delete database body))
      (_ (e-runtime-store-session-worker--error
          "Unknown session write operation" (plist-get body :op))))))

(defun e-runtime-store-session-worker--limit (value cap)
  "Validate positive requested VALUE against CAP and return it."
  (let ((limit (or value cap)))
    (unless (and (integerp limit) (> limit 0) (<= limit cap))
      (e-runtime-store-session-worker--error
       "Session page limit is outside its bound" value cap))
    limit))

(defun e-runtime-store-session-worker--page-cursor (cursor)
  "Validate detached stable page CURSOR or return nil."
  (when cursor
    (let ((keys nil)
          (tail cursor))
      (while tail
        (push (pop tail) keys)
        (pop tail))
      (unless (and (e-runtime-store-session-worker--proper-plist-p cursor)
                   (= (length keys) 2)
                   (memq :session-id keys)
                   (memq :updated-at keys))
        (e-runtime-store-session-worker--error
         "Session page cursor is invalid" cursor)))
    (let ((session-id (plist-get cursor :session-id))
          (updated-at (plist-get cursor :updated-at)))
      (unless (and (stringp session-id)
                   (stringp updated-at))
        (e-runtime-store-session-worker--error
         "Session page cursor values are invalid" cursor))
      (list :session-id session-id :updated-at updated-at))))

(defun e-runtime-store-session-worker--query-state (database body)
  "Read one exact detached query state."
  (let ((session-id (e-runtime-store-session-worker--session-id
                     (plist-get body :session-id))))
    (e-runtime-store-session-worker--state-from-row
     (car (sqlite-select
           database
           (concat "SELECT "
                   (mapconcat #'symbol-name
                              e-runtime-store-session-worker--state-columns ",")
                   " FROM session_query_state WHERE session_id=?")
           (vector session-id))))))

(defun e-runtime-store-session-worker--metadata (database body)
  "Read exact detached metadata for one session."
  (let ((state (e-runtime-store-session-worker--query-state database body)))
    (when state
      (list :session-id (plist-get state :session-id)
            :name (plist-get state :name)
            :summary (plist-get state :summary)
            :latest-assistant-marker
            (plist-get state :latest-assistant-marker)
            :metadata (e-session-query--copy-value
                       (plist-get state :metadata))
            :created-at (plist-get state :created-at)
            :updated-at (plist-get state :updated-at)
            :current-branch (plist-get state :current-branch)
            :turn-options (e-session-query--copy-value
                           (plist-get state :turn-options))
            :current-head-id (plist-get state :current-head-id)
            :root-event-id (plist-get state :root-event-id)
            :root-p (plist-get state :root-p)
            :journal-position (plist-get state :journal-position)))))

(defun e-runtime-store-session-worker--association (database body)
  "Read exact detached Board association for one session."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         ;; Keep this projection physically narrow.  In particular, an
         ;; unreadable metadata/options payload must not prevent a Board
         ;; owner from discovering its association.
         (row (car (sqlite-select
                    database
                    (concat
                     "SELECT session_id,board_id,principal,association_role,"
                     "routing_policy,board_output_sequence,"
                     "board_activity_sequence FROM session_query_state "
                     "WHERE session_id=?")
                    (vector session-id)))))
    (when row
      (let ((row-session-id
             (e-runtime-store-session-worker--session-id
              (e-runtime-store-session-worker--column row 0)))
            (board-id (e-runtime-store-session-worker--scalar
                       (e-runtime-store-session-worker--column row 1)
                       :board-id))
            (principal (e-runtime-store-session-worker--scalar
                        (e-runtime-store-session-worker--column row 2)
                        :principal))
            (association-role
             (e-runtime-store-session-worker--scalar
              (e-runtime-store-session-worker--column row 3)
              :association-role))
            (routing-policy
             (e-runtime-store-session-worker--decode-value
              (e-runtime-store-session-worker--column row 4)
              :routing-policy))
            (board-output-sequence
             (e-runtime-store-session-worker--column row 5))
            (board-activity-sequence
             (e-runtime-store-session-worker--column row 6)))
        (list :session-id row-session-id :board-id board-id
              :principal principal :association-role association-role
              :routing-policy routing-policy
              :board-output-sequence board-output-sequence
              :board-activity-sequence board-activity-sequence)))))

(defun e-runtime-store-session-worker--query-page (database body)
  "Read a stable newest/root bounded query-state page."
  (let* ((limit (e-runtime-store-session-worker--limit
                 (plist-get body :limit)
                 e-runtime-store-session-worker-page-row-limit))
         (cursor (e-runtime-store-session-worker--page-cursor
                  (plist-get body :cursor)))
         (root-p (if (eq (plist-get body :op) 'session-root-page)
                     t
                   (plist-get body :root-p)))
         (board-id (and (plist-member body :board-id)
                        (plist-get body :board-id)))
         (principal (and (plist-member body :principal)
                         (plist-get body :principal)))
         (where nil)
         (params nil))
    (unless (or (null root-p) (eq root-p t))
      (e-runtime-store-session-worker--error
       "Session root filter is invalid" root-p))
    (when root-p
      (setq where (append where (list "root_p=1"))))
    (when (plist-member body :board-id)
      (if (null board-id)
          (setq where (append where (list "board_id IS NULL")))
        (unless (and (stringp board-id)
                     (<= (string-bytes board-id)
                         e-session-query-state-string-byte-limit))
          (e-runtime-store-session-worker--error
           "Session Board filter is invalid" board-id))
        (setq where (append where (list "board_id=?"))
              params (append params (list board-id)))))
    (when (plist-member body :principal)
      (if (null principal)
          (setq where (append where (list "principal IS NULL")))
        (unless (and (stringp principal)
                     (<= (string-bytes principal)
                         e-session-query-state-string-byte-limit))
          (e-runtime-store-session-worker--error
           "Session principal filter is invalid" principal))
        (setq where (append where (list "principal=?"))
              params (append params (list principal)))))
    (when cursor
      (setq where
            (append where
                    (list "(updated_at < ? OR (updated_at = ? AND session_id < ?))")))
      (setq params
            (append params
                    (list (plist-get cursor :updated-at)
                          (plist-get cursor :updated-at)
                          (plist-get cursor :session-id)))))
    (setq params (vconcat params (vector (1+ limit))))
    (let* ((sql (concat "SELECT "
                        (mapconcat #'symbol-name
                                   e-runtime-store-session-worker--state-columns ",")
                        " FROM session_query_state"
                        (when where
                          (concat " WHERE "
                                  (mapconcat #'identity where " AND ")))
                        " ORDER BY updated_at DESC, session_id DESC LIMIT ?"))
           (rows (sqlite-select database sql params))
           (states nil)
           (bytes 0)
           (truncated (> (length rows) limit)))
      ;; Decode only the bounded page.  A lookahead row proves that a next
      ;; cursor exists, while the byte budget prevents one page of large
      ;; semantic values from becoming an oversized protocol response.
      (catch 'page-full
        (dolist (row rows)
          (when (>= (length states) limit)
            (setq truncated t)
            (throw 'page-full nil))
          (let* ((state (e-runtime-store-session-worker--state-from-row row))
                 (state-bytes
                  (e-runtime-store-codec-measure-bounded
                   state e-runtime-store-session-worker-page-byte-limit)))
            (when (and states
                       (> (+ bytes state-bytes)
                          e-runtime-store-session-worker-page-byte-limit))
              (setq truncated t)
              (throw 'page-full nil))
            (setq bytes (+ bytes state-bytes))
            (push state states))))
      (setq states (nreverse states))
      (let ((last-state (car (last states))))
        (list :rows (mapcar #'e-session-query--copy-value states)
              :next (and truncated last-state
                         (list :updated-at (plist-get last-state :updated-at)
                               :session-id (plist-get last-state :session-id)))
              :limit limit :byte-count bytes
              :byte-limit e-runtime-store-session-worker-page-byte-limit)))))

(defun e-runtime-store-session-worker--id-page (database body)
  "Return one bounded cursor page of durable session identities."
  (let* ((cursor (plist-get body :cursor))
         (limit (e-runtime-store-session-worker--limit
                 (plist-get body :limit)
                 e-runtime-store-session-worker-page-row-limit)))
    (when cursor
      (unless (and (stringp cursor)
                   (<= (string-bytes cursor)
                       e-session-query-state-string-byte-limit))
        (e-runtime-store-session-worker--error
         "Session identity cursor is invalid" cursor)))
    (let* ((params (if cursor (vector cursor (1+ limit))
                   (vector (1+ limit))))
           (sql (if cursor
                    "SELECT session_id FROM session_query_state WHERE session_id>? ORDER BY session_id ASC LIMIT ?"
                  "SELECT session_id FROM session_query_state ORDER BY session_id ASC LIMIT ?"))
           (rows (sqlite-select database sql params))
           (truncated (> (length rows) limit))
           (ids nil)
           (bytes 0))
      (catch 'page-full
        (dolist (row rows)
          (when (>= (length ids) limit)
            (setq truncated t)
            (throw 'page-full nil))
          (let* ((session-id
                  (e-runtime-store-session-worker--session-id
                   (e-runtime-store-session-worker--column row 0)))
                 (id-bytes (string-bytes session-id)))
            (when (and ids
                       (> (+ bytes id-bytes)
                          e-runtime-store-session-worker-id-page-byte-limit))
              (setq truncated t)
              (throw 'page-full nil))
            (when (> id-bytes e-runtime-store-session-worker-id-page-byte-limit)
              (e-runtime-store-session-worker--error
               "Session identity page exceeds byte bound" id-bytes))
            (setq bytes (+ bytes id-bytes))
            (push session-id ids))))
      (setq ids (nreverse ids))
      (list :ids ids
            :next (and truncated (car (last ids)))
            :limit limit :byte-count bytes
            :byte-limit e-runtime-store-session-worker-id-page-byte-limit))))

(defun e-runtime-store-session-worker--decode-record-row (row)
  "Return detached bounded record result from physical ROW."
  (unless (= (length row) 8)
    (e-runtime-store-session-worker--error
     "Session record row has an invalid physical shape" row))
  (let ((position (e-runtime-store-session-worker--column row 0))
        (record-type (e-runtime-store-session-worker--column row 1))
        (record-id (e-runtime-store-session-worker--column row 2))
        (record-identity (e-runtime-store-session-worker--column row 3))
        (parent-id (e-runtime-store-session-worker--column row 4))
        (timestamp (e-runtime-store-session-worker--column row 5))
        (payload-bytes (e-runtime-store-session-worker--column row 6))
        (payload (e-runtime-store-session-worker--column row 7)))
    (unless (and (integerp position) (>= position 0)
                 (stringp record-type) (> (string-bytes record-type) 0)
                 (<= (string-bytes record-type)
                     e-session-query-state-string-byte-limit)
                 (or (null record-id)
                     (and (stringp record-id)
                          (<= (string-bytes record-id)
                              e-session-query-state-string-byte-limit)))
                 (or (null record-identity)
                     (and (stringp record-identity)
                          (> (string-bytes record-identity) 0)
                          (<= (string-bytes record-identity)
                              e-session-query-state-string-byte-limit)))
                 (or (null parent-id)
                     (and (stringp parent-id)
                          (<= (string-bytes parent-id)
                              e-session-query-state-string-byte-limit)))
                 (or (null timestamp)
                     (and (stringp timestamp)
                          (<= (string-bytes timestamp)
                              e-session-query-state-string-byte-limit)))
                 (integerp payload-bytes) (>= payload-bytes 0)
                 (stringp payload)
                 (= payload-bytes (string-bytes payload)))
      (e-runtime-store-session-worker--error
       "Session record row has invalid physical fields" row))
    (when (> payload-bytes e-runtime-store-session-worker-record-byte-limit)
      (e-runtime-store-session-worker--error
       "Session record exceeds read bound" payload-bytes))
    (list :position position :record-type record-type :record-id record-id
          :record-identity record-identity
          :parent-id parent-id :timestamp timestamp
          ;; The codec produces a fresh tree and PAYLOAD-BYTES was bounded
          ;; above.  Query-row scalar limits apply to indexed current state,
          ;; not to durable journal values such as message bodies.
          :value (e-runtime-store-codec-decode
                  (base64-decode-string payload)))))

(defun e-runtime-store-session-worker--record-page (database body)
  "Read one bounded ordered record page with typed predicates."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (order (or (plist-get body :order) 'oldest))
         (after (plist-get body :after))
         (before (plist-get body :before))
         (limit (e-runtime-store-session-worker--limit
                 (plist-get body :limit)
                 e-runtime-store-session-worker-record-page-row-limit))
         (where (list "session_id=?"))
         (params (list session-id)))
    (unless (memq order '(oldest newest))
      (e-runtime-store-session-worker--error
       "Session record order is invalid" order))
    (when (and after before)
      (e-runtime-store-session-worker--error
       "Session record page has conflicting cursors" after before))
    (pcase order
      ('oldest
       (when before
         (e-runtime-store-session-worker--error
          "Oldest-first session page cannot use a before cursor" before))
       (setq after (or after 0))
       (unless (and (integerp after) (>= after 0))
         (e-runtime-store-session-worker--error
          "Session record cursor is invalid" after))
       (setq where (append where (list "position>?"))
             params (append params (list after))))
      ('newest
       (when after
         (e-runtime-store-session-worker--error
          "Newest-first session page cannot use an after cursor" after))
       (when before
         (unless (and (integerp before) (> before 0))
           (e-runtime-store-session-worker--error
            "Session record cursor is invalid" before))
         (setq where (append where (list "position<?"))
               params (append params (list before))))))
    (dolist (field '(record-type record-id record-identity parent-id))
      (when (plist-member body (intern (concat ":" (symbol-name field))))
        (let* ((key (intern (concat ":" (symbol-name field))))
               (value (plist-get body key))
               (column (pcase field
                         ('record-type "record_type")
                         ('record-id "record_id")
                         ('record-identity "record_identity")
                         ('parent-id "parent_id"))))
          (unless (or (null value)
                      (and (stringp value)
                           (<= (string-bytes value)
                               e-session-query-state-string-byte-limit)))
            (e-runtime-store-session-worker--error
             "Session record filter is invalid" field value))
          (if (null value)
              (setq where (append where (list (concat column " IS NULL"))))
            (setq where (append where (list (concat column "=?")))
                  params (append params (list value)))))))
    (dolist (spec '((:record-ids . "record_id")
                    (:parent-ids . "parent_id")))
      (when (plist-member body (car spec))
        (let* ((raw (plist-get body (car spec)))
               (values (cond ((vectorp raw) (append raw nil))
                             ((proper-list-p raw) raw)
                             (t nil))))
          (unless (and values
                       (<= (length values) 64)
                       (= (length values)
                          (length (delete-dups (copy-sequence values))))
                       (cl-every
                        (lambda (value)
                          (and (stringp value)
                               (> (string-bytes value) 0)
                               (<= (string-bytes value)
                                   e-session-query-state-string-byte-limit)))
                        values))
            (e-runtime-store-session-worker--error
             "Session record identity set is invalid" (car spec)))
          (setq where
                (append
                 where
                 (list
                  (format "%s IN (%s)" (cdr spec)
                          (mapconcat (lambda (_value) "?") values ","))))
                params (append params values)))))
    (let* ((sql (concat "SELECT position,record_type,record_id,record_identity,parent_id,timestamp,LENGTH(payload),payload FROM session_records WHERE "
                        (mapconcat #'identity where " AND ")
                        " ORDER BY position "
                        (if (eq order 'newest) "DESC" "ASC")
                        " LIMIT ?"))
           (rows (sqlite-select
                  database sql
                  (vconcat params (vector (1+ limit)))))
           (truncated (> (length rows) limit))
           (selected (if truncated (butlast rows) rows))
           (records nil)
           (bytes 0))
      (catch 'page-full
        (dolist (row selected)
          (let* ((record (e-runtime-store-session-worker--decode-record-row row))
                 (record-bytes (e-runtime-store-codec-measure-bounded
                                record e-runtime-store-session-worker-page-byte-limit)))
            (when (and records
                       (> (+ bytes record-bytes)
                          e-runtime-store-session-worker-page-byte-limit))
              (setq truncated t)
              (throw 'page-full nil))
            (when (> record-bytes e-runtime-store-session-worker-page-byte-limit)
              (e-runtime-store-session-worker--error
               "Session record exceeds page byte bound" record-bytes))
            (setq bytes (+ bytes record-bytes))
            (push record records))))
      (setq records (nreverse records))
      (let ((last-record (car (last records))))
        (list :records records
              :next (and truncated last-record
                         (plist-get last-record :position))
              :limit limit :byte-count bytes
              :byte-limit e-runtime-store-session-worker-page-byte-limit
          :high-water (e-runtime-store-session-worker--position
                           database session-id))))))

(defun e-runtime-store-session-worker--process-report (row expected-type)
  "Decode canonical process report ROW and require EXPECTED-TYPE."
  (let* ((record (e-runtime-store-session-worker--decode-record-row row))
         (value (plist-get record :value))
         (report (plist-get value :report)))
    (unless (and (equal (plist-get value :type) "process-report")
                 (listp report)
                 (equal (plist-get report :report-type) expected-type))
      (e-runtime-store-session-worker--error
       "Process-report index disagrees with canonical journal"
       :position (plist-get record :position) :report-type expected-type))
    (copy-tree report t)))

(defun e-runtime-store-session-worker--process-report-cursor (value)
  "Validate newest-first process-report position cursor VALUE."
  (when value
    (unless (and (integerp value) (> value 0))
      (e-runtime-store-session-worker--error
       "Process-report page cursor is invalid" value)))
  value)

(defun e-runtime-store-session-worker--process-report-marker-page
    (database body)
  "Return a bounded newest marker page with each latest triage in one query."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (limit (e-runtime-store-session-worker--limit
                 (plist-get body :limit)
                 e-runtime-store-session-worker-record-page-row-limit))
         (before (e-runtime-store-session-worker--process-report-cursor
                  (plist-get body :before)))
         (raw-status (plist-get body :status))
         (status
          (and raw-status
               (downcase
                (e-runtime-store-session-worker--required-scalar
                 raw-status :status))))
         (rows
          (sqlite-select
           database
           (concat
            "WITH marker_status AS ("
            " SELECT p.position,p.marker_id,"
            " (SELECT ti.position FROM session_process_report_index ti"
            " WHERE ti.session_id=p.session_id AND ti.association_ordinal>0"
            " AND ti.report_type='triage' AND ti.marker_id=p.marker_id"
            " ORDER BY ti.position DESC LIMIT 1) AS triage_position"
            " FROM session_process_report_index p"
            " WHERE p.session_id=? AND p.association_ordinal>0"
            " AND p.report_type='marker'"
            (if before " AND p.position<?" "")
            "), marker_page AS ("
            " SELECT ms.position,ms.marker_id,ms.triage_position"
            " FROM marker_status ms LEFT JOIN session_process_report_index ti"
            " ON ti.session_id=? AND ti.position=ms.triage_position"
            " AND ti.association_ordinal>0 AND ti.report_type='triage'"
            " AND ti.marker_id=ms.marker_id"
            " WHERE (? IS NULL OR COALESCE(ti.triage_status,'open')=?)"
            " ORDER BY ms.position DESC LIMIT ?"
            ") SELECT m.position,m.record_type,m.record_id,m.record_identity,"
            "m.parent_id,m.timestamp,LENGTH(m.payload),m.payload,"
            "t.position,t.record_type,t.record_id,t.record_identity,"
            "t.parent_id,t.timestamp,LENGTH(t.payload),t.payload "
            "FROM marker_page p JOIN session_records m"
            " ON m.session_id=? AND m.position=p.position "
            "LEFT JOIN session_records t ON t.session_id=m.session_id"
            " AND t.position=p.triage_position"
            " ORDER BY p.position DESC")
           (if before
               (vector session-id before session-id status status
                       (1+ limit) session-id)
             (vector session-id session-id status status
                     (1+ limit) session-id))))
         (truncated (> (length rows) limit))
         (selected (if truncated (cl-subseq rows 0 limit) rows))
         markers (bytes 0) last-position)
    (catch 'page-full
      (dolist (row selected)
        (let* ((marker
                (e-runtime-store-session-worker--process-report
                 (cl-subseq row 0 8) "marker"))
               (triage
                (when (e-runtime-store-session-worker--column row 8)
                  (e-runtime-store-session-worker--process-report
                   (cl-subseq row 8 16) "triage")))
               (item (list :marker marker :latest-triage triage))
               (item-bytes
                (e-runtime-store-codec-measure-bounded
                 item e-runtime-store-session-worker-page-byte-limit)))
          (when (and markers
                     (> (+ bytes item-bytes)
                        e-runtime-store-session-worker-page-byte-limit))
            (setq truncated t)
            (throw 'page-full nil))
          (when (> item-bytes e-runtime-store-session-worker-page-byte-limit)
            (e-runtime-store-session-worker--error
             "Process-report marker exceeds page byte bound" item-bytes))
          (setq bytes (+ bytes item-bytes)
                last-position
                (e-runtime-store-session-worker--column row 0))
          (push item markers))))
    (setq markers (nreverse markers))
    (list :markers markers :truncated truncated
          :next (and truncated last-position)
          :limit limit :byte-count bytes
          :byte-limit e-runtime-store-session-worker-page-byte-limit)))

(defun e-runtime-store-session-worker--process-report-marker
    (database body)
  "Return the exact canonical marker selected by BODY."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (marker-id (e-runtime-store-session-worker--required-scalar
                     (plist-get body :marker-id) :marker-id))
         (row
          (car
           (sqlite-select
            database
            (concat
             "SELECT r.position,r.record_type,r.record_id,r.record_identity,"
             "r.parent_id,r.timestamp,LENGTH(r.payload),r.payload "
             "FROM session_process_report_index p JOIN session_records r"
             " ON r.session_id=p.session_id AND r.position=p.position"
             " WHERE p.session_id=? AND p.association_ordinal>0"
             " AND p.report_type='marker' AND p.marker_id=?"
             " ORDER BY p.position ASC LIMIT 1")
            (vector session-id marker-id)))))
    (let ((marker
           (and row
                (e-runtime-store-session-worker--process-report row "marker"))))
      (when marker
        (e-runtime-store-codec-measure-bounded
         marker e-runtime-store-session-worker-page-byte-limit))
      (list :marker marker))))

(defun e-runtime-store-session-worker--process-report-association-page
    (database body report-type result-key)
  "Return bounded REPORT-TYPE rows associated with BODY's marker."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (marker-id (e-runtime-store-session-worker--required-scalar
                     (plist-get body :marker-id) :marker-id))
         (limit (e-runtime-store-session-worker--limit
                 (plist-get body :limit)
                 e-runtime-store-session-worker-record-page-row-limit))
         (before (e-runtime-store-session-worker--process-report-cursor
                  (plist-get body :before)))
         (rows
          (sqlite-select
           database
           (concat
            "SELECT r.position,r.record_type,r.record_id,r.record_identity,"
            "r.parent_id,r.timestamp,LENGTH(r.payload),r.payload "
            "FROM session_process_report_index p JOIN session_records r"
            " ON r.session_id=p.session_id AND r.position=p.position"
            " WHERE p.session_id=? AND p.association_ordinal>0"
            " AND p.report_type=? AND p.marker_id=?"
            (if before " AND p.position<?" "")
            " ORDER BY p.position DESC LIMIT ?")
           (if before
               (vector session-id report-type marker-id before (1+ limit))
             (vector session-id report-type marker-id (1+ limit)))))
         (truncated (> (length rows) limit))
         (selected (if truncated (cl-subseq rows 0 limit) rows))
         reports (bytes 0) last-position)
    (catch 'page-full
      (dolist (row selected)
        (let* ((report
                (e-runtime-store-session-worker--process-report row report-type))
               (report-bytes
                (e-runtime-store-codec-measure-bounded
                 report e-runtime-store-session-worker-page-byte-limit)))
          (when (and reports
                     (> (+ bytes report-bytes)
                        e-runtime-store-session-worker-page-byte-limit))
            (setq truncated t)
            (throw 'page-full nil))
          (when (> report-bytes e-runtime-store-session-worker-page-byte-limit)
            (e-runtime-store-session-worker--error
             "Process-report association exceeds page byte bound" report-bytes))
          (setq bytes (+ bytes report-bytes)
                last-position
                (e-runtime-store-session-worker--column row 0))
          (push report reports))))
    (setq reports (nreverse reports))
    (list result-key reports :truncated truncated
          :next (and truncated last-position)
          :limit limit :byte-count bytes
          :byte-limit e-runtime-store-session-worker-page-byte-limit)))

(defun e-runtime-store-session-worker--process-report-request-shapes
    (database body)
  "Return the latest exact request-shape for each bounded request id in BODY."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (raw (plist-get body :provider-request-ids))
         (ids (cond ((vectorp raw) (append raw nil))
                    ((proper-list-p raw) raw)
                    (t nil))))
    (unless (and ids (<= (length ids) 64)
                 (= (length ids) (length (delete-dups (copy-sequence ids))))
                 (cl-every
                  (lambda (id)
                    (and (stringp id) (> (string-bytes id) 0)
                         (<= (string-bytes id)
                             e-session-query-state-string-byte-limit)))
                  ids))
      (e-runtime-store-session-worker--error
       "Process-report request identity set is invalid"))
    (let* ((marks (mapconcat (lambda (_id) "?") ids ","))
           (rows
            (sqlite-select
             database
             (concat
              "SELECT r.position,r.record_type,r.record_id,r.record_identity,"
              "r.parent_id,r.timestamp,LENGTH(r.payload),r.payload "
              "FROM session_process_report_index p JOIN session_records r"
              " ON r.session_id=p.session_id AND r.position=p.position"
              " WHERE p.session_id=? AND p.association_ordinal=0"
              " AND p.report_type='request-shape'"
              " AND p.provider_request_id IN (" marks ")"
              " AND p.position=(SELECT MAX(p2.position)"
              " FROM session_process_report_index p2"
              " WHERE p2.session_id=p.session_id AND p2.association_ordinal=0"
              " AND p2.report_type='request-shape'"
              " AND p2.provider_request_id=p.provider_request_id)"
              " ORDER BY p.position ASC LIMIT 65")
             (vconcat (list session-id) ids)))
           reports (bytes 0))
      (when (> (length rows) 64)
        (e-runtime-store-session-worker--error
         "Process-report request result exceeds its bound"))
      (dolist (row rows)
        (let* ((report
                (e-runtime-store-session-worker--process-report
                 row "request-shape"))
               (report-bytes
                (e-runtime-store-codec-measure-bounded
                 report e-runtime-store-session-worker-page-byte-limit)))
          (when (> (+ bytes report-bytes)
                   e-runtime-store-session-worker-page-byte-limit)
            (e-runtime-store-session-worker--error
             "Process-report request result exceeds byte bound"
             e-runtime-store-session-worker-page-byte-limit))
          (setq bytes (+ bytes report-bytes))
          (push report reports)))
      (list :request-shapes (nreverse reports)
            :truncated nil :next nil :limit 64 :byte-count bytes
            :byte-limit e-runtime-store-session-worker-page-byte-limit))))

(defun e-runtime-store-session-worker--process-report-marker-count
    (database body)
  "Return exact durable marker count for BODY's session."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (row (car (sqlite-select
                    database
                    (concat
                     "SELECT COUNT(*) FROM session_process_report_index"
                     " WHERE session_id=? AND association_ordinal=0"
                     " AND report_type='marker'")
                    (vector session-id)))))
    (list :marker-count
          (e-runtime-store-session-worker--column row 0))))

(defun e-runtime-store-session-worker--activity-event (record)
  "Return RECORD's detached semantic activity event, or nil.

Current relational commands store the event envelope directly on the
`activity-event' record.  Offline-migrated v5 rows may additionally carry the
former nested `:semantic-event' spelling.  Normalize both at this bounded read
boundary; neither shape is installed as process-local session state."
  (when (equal (plist-get record :type) "activity-event")
    (let ((event
           (or (when-let* ((nested (plist-get record :semantic-event)))
                 (e-session-query--copy-value nested))
               (list :id (plist-get record :id)
                     :parent-id (plist-get record :parent-id)
                     :turn-id (plist-get record :turn-id)
                     :event-type (plist-get record :event-type)
                     :payload (e-session-query--copy-value
                               (plist-get record :payload))
                     :created-at (plist-get record :timestamp)))))
      (when-let* ((event-type (plist-get event :event-type)))
        (when (stringp event-type)
          (plist-put event :event-type (intern event-type))))
      (when (eq (plist-get event :event-type) 'hook-audit)
        (when-let* ((payload (plist-get event :payload)))
          (dolist (key '(:owner :outcome :truth-status))
            (when-let* ((value (plist-get payload key)))
              (when (stringp value)
                (plist-put payload key (intern value)))))))
      event)))

(defun e-runtime-store-session-worker--recent-failures (database body)
  "Return a bounded newest-first failed-turn page from DATABASE."
  (let* ((limit (e-runtime-store-session-worker--limit
                 (plist-get body :limit) 32))
         (rows
          (sqlite-select
           database
           (concat
            "SELECT r.session_id,r.position,r.record_type,r.record_id,"
            "r.record_identity,r.parent_id,r.timestamp,LENGTH(r.payload),"
            "r.payload,q.name,q.summary "
            "FROM session_records r JOIN session_query_state q "
            "ON q.session_id=r.session_id "
            "WHERE r.record_type='activity-event' "
            "ORDER BY COALESCE(r.timestamp,'') DESC,r.session_id DESC,"
            "r.position DESC LIMIT ?")
           (vector e-runtime-store-session-worker-failure-scan-row-limit)))
         failures)
    (catch 'full
      (dolist (row rows)
        (let* ((session-id
                (e-runtime-store-session-worker--column row 0))
               (record
                (e-runtime-store-session-worker--decode-record-row
                 (seq-subseq row 1 9)))
               (event
                (e-runtime-store-session-worker--activity-event
                 (plist-get record :value))))
          (when (and event (eq (plist-get event :event-type) 'turn-failed))
            (let ((payload (plist-get event :payload)))
              (push
               (list :session-id session-id
                     :turn-id (plist-get event :turn-id)
                     :created-at (plist-get event :created-at)
                     :event-id (plist-get event :id)
                     :error (plist-get payload :error)
                     :details (e-session-query--copy-value
                               (plist-get payload :details))
                     :session-title
                     (or (e-runtime-store-session-worker--column row 9)
                         (e-runtime-store-session-worker--column row 10)
                         session-id))
               failures))
            (when (>= (length failures) limit) (throw 'full nil))))))
    (list :failures (nreverse failures)
          :limit limit
          :scanned (length rows)
          :scan-limit e-runtime-store-session-worker-failure-scan-row-limit)))

(defun e-runtime-store-session-worker--turn-inspection (database body)
  "Return one bounded failed-turn timeline from DATABASE."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (turn-id (e-runtime-store-session-worker--session-id
                   (plist-get body :turn-id)))
         (state (e-runtime-store-session-worker--query-state
                 database (list :session-id session-id)))
         (rows
          (and state
               (sqlite-select
                database
                (concat
                 "SELECT position,record_type,record_id,record_identity,"
                 "parent_id,timestamp,LENGTH(payload),payload "
                 "FROM session_records WHERE session_id=? "
                 "AND record_type IN ('message','activity-event') "
                 "ORDER BY position DESC LIMIT ?")
                (vector
                 session-id
                 (1+ e-runtime-store-session-worker-turn-inspection-row-limit)))))
         (truncated
          (and rows
               (> (length rows)
                  e-runtime-store-session-worker-turn-inspection-row-limit)))
         messages events)
    (dolist (row (if truncated (butlast rows) rows))
      (let* ((record
              (e-runtime-store-session-worker--decode-record-row row))
             (value (plist-get record :value))
             (record-type (plist-get record :record-type)))
        (cond
         ((equal record-type "message")
          (when-let* ((message (plist-get value :message))
                      ((equal (plist-get message :turn-id) turn-id)))
            (push (e-session-query--copy-value message) messages)))
         ((equal record-type "activity-event")
          (when-let* ((event
                       (e-runtime-store-session-worker--activity-event value))
                      ((equal (plist-get event :turn-id) turn-id)))
            (push (e-session-query--copy-value event) events))))))
    (list
     :present (and state t)
     :session
     (and state
          (list :id session-id
                :name (plist-get state :name)
                :summary (plist-get state :summary)
                :metadata (e-session-query--copy-value
                           (plist-get state :metadata))))
     :turn-id turn-id
     :events events
     :messages messages
     :truncated truncated
     :row-limit e-runtime-store-session-worker-turn-inspection-row-limit)))

(defun e-runtime-store-session-worker--visible-message-page (database body)
  "Read the newest bounded message window for one chat session.

This is deliberately a consumer-shaped read rather than a second spelling of
the general journal-page API.  SQL filters on the typed `message' record
family before decoding payloads, and the result contains only detached
message values needed by the visible transcript.  The newest rows are
returned in presentation order (oldest to newest within the window)."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (limit (e-runtime-store-session-worker--limit
                 (plist-get body :limit)
                 e-runtime-store-session-worker-visible-message-row-limit))
         (rows
          (sqlite-select
           database
           (concat
            "SELECT m.position,m.record_type,m.record_id,m.record_identity,m.parent_id,m.timestamp,LENGTH(m.payload),m.payload,"
            "(SELECT d.payload FROM session_records d "
            " WHERE d.session_id=m.session_id AND d.record_type='message-display' "
            " AND d.record_id=m.record_id ORDER BY d.position DESC LIMIT 1) "
            "FROM session_records m WHERE m.session_id=? AND m.record_type='message' "
            "ORDER BY m.position DESC LIMIT ?")
           (vector session-id (1+ limit))))
         (truncated (> (length rows) limit))
         (messages nil)
         (bytes 0))
    (catch 'visible-page-full
      (dolist (row (if truncated (cl-subseq rows 0 limit) rows))
        (let* ((record
                (e-runtime-store-session-worker--decode-record-row
                 (cl-subseq row 0 8)))
               (message
                (e-runtime-store-session-worker--message-with-display
                 (plist-get (plist-get record :value) :message)
                 (e-runtime-store-session-worker--column row 8)))
               (message-bytes
                (e-runtime-store-codec-measure-bounded
                 message e-runtime-store-session-worker-page-byte-limit)))
          (unless (and (listp message)
                       (plist-member message :id)
                       (plist-member message :role))
            (e-runtime-store-session-worker--error
             "Session visible message has an invalid shape" message))
          (when (and messages
                     (> (+ bytes message-bytes)
                        e-runtime-store-session-worker-page-byte-limit))
            (setq truncated t)
            (throw 'visible-page-full nil))
          (when (> message-bytes e-runtime-store-session-worker-page-byte-limit)
            (e-runtime-store-session-worker--error
             "Session visible message exceeds page byte bound" message-bytes))
          (setq bytes (+ bytes message-bytes))
          (push message messages))))
    (list :session-id session-id
          ;; SQL visits newest first; PUSH restores presentation order, so do
          ;; not reverse this list a second time.
          :messages messages
          :limit limit
          :truncated truncated
          :byte-count bytes
          :byte-limit e-runtime-store-session-worker-page-byte-limit
          :high-water
          (e-runtime-store-session-worker--position database session-id))))

(defun e-runtime-store-session-worker--context-path (database body)
  "Read exactly one selected parent path and apply its latest compaction.

The recursive relation follows `session_query_state.current_head_id' toward
the root.  Only message and compaction payloads cross the worker boundary;
unselected branch rows and unrelated journal families are never returned."
  (let* ((session-id (e-runtime-store-session-worker--session-id
                      (plist-get body :session-id)))
         (state-row
          (car (sqlite-select
                database
                (concat
                 "SELECT current_head_id,current_branch,turn_options,metadata "
                 "FROM session_query_state WHERE session_id=?")
                (vector session-id))))
         (_ (unless state-row
              (e-runtime-store-session-worker--error
               "Missing session query state for context" session-id)))
         (head-id (e-runtime-store-session-worker--column state-row 0))
         (branch (e-runtime-store-session-worker--column state-row 1))
         (turn-options
          (e-runtime-store-session-worker--decode-value
           (e-runtime-store-session-worker--column state-row 2)
           :turn-options))
         (metadata
          (e-runtime-store-session-worker--decode-value
           (e-runtime-store-session-worker--column state-row 3)
           :metadata))
         (rows
          (if (null head-id)
              nil
            (sqlite-select
             database
             (concat
              "WITH RECURSIVE selected(position,record_id,parent_id,record_type,payload,depth) AS ("
              "SELECT position,record_id,parent_id,record_type,payload,0 "
              "FROM session_records WHERE session_id=? AND record_id=? "
              "UNION ALL "
              "SELECT r.position,r.record_id,r.parent_id,r.record_type,r.payload,s.depth+1 "
              "FROM session_records r JOIN selected s ON r.record_id=s.parent_id "
              "WHERE r.session_id=? AND s.depth<?) "
              "SELECT position,record_id,parent_id,record_type,payload,depth,"
              "(SELECT d.payload FROM session_records d "
              " WHERE d.session_id=? AND d.record_type='message-display' "
              " AND d.record_id=selected.record_id "
              " ORDER BY d.position DESC LIMIT 1) "
              "FROM selected ORDER BY depth DESC")
             (vector session-id head-id session-id
                     e-runtime-store-session-worker-context-path-row-limit
                     session-id))))
         (truncated
          (and rows
               (= (length rows)
                  (1+ e-runtime-store-session-worker-context-path-row-limit))))
         (path-ids (make-hash-table :test 'equal))
         entries latest-compaction boundary messages message-path-indexes
         context-records tool-receipts (tool-receipt-total-count 0)
         (tool-receipt-bytes 0)
         (erased-tool-call-ids (make-hash-table :test #'equal))
         (bytes 0))
    (when truncated
      (e-runtime-store-session-worker--error
       "Selected session context path exceeds record limit"
       session-id e-runtime-store-session-worker-context-path-row-limit))
    (cl-loop for row in rows
             for path-index from 0
             do
      (let* ((record-id (e-runtime-store-session-worker--column row 1))
             (record-type (e-runtime-store-session-worker--column row 3))
             (record
              (e-runtime-store-codec-decode
               (base64-decode-string
                (e-runtime-store-session-worker--column row 4)))))
        (when record-id (puthash record-id path-index path-ids))
        (push (list :path-index path-index :record-type record-type
                    :record record
                    :display-payload
                    (e-runtime-store-session-worker--column row 6))
              entries)))
    (setq entries (nreverse entries))
    ;; Receipt context is part of this consumer-shaped selected-path query.
    ;; Resolve durable erasures here and return only the small visible tail;
    ;; Emacs never receives or retains the complete receipt history.
    (dolist (entry entries)
      (when (equal (plist-get entry :record-type)
                   "context-curation-package")
        (when-let* ((erasure (plist-get (plist-get entry :record) :erasure)))
          (dolist (tool-call-id
                   (e-context-lifetime-curation-erasure-tool-call-ids erasure))
            (puthash tool-call-id t erased-tool-call-ids)))))
    (dolist (entry entries)
      (when (equal (plist-get entry :record-type) "activity-event")
        (let* ((record (plist-get entry :record))
               (payload (plist-get record :payload))
               (receipt (and (eq (plist-get record :event-type) 'tool-finished)
                             (plist-get payload :receipt)))
               (tool-call-id (and (listp receipt)
                                  (plist-get receipt :tool-call-id))))
          (when (and (listp receipt)
                     (stringp tool-call-id)
                     (not (gethash tool-call-id erased-tool-call-ids)))
            (let* ((projected
                    (list :tool-call-id tool-call-id
                          :tool (plist-get receipt :tool)
                          :status (plist-get receipt :status)
                          :details-uri (plist-get receipt :details-uri)))
                   (receipt-bytes
                    (e-runtime-store-codec-measure-bounded
                     projected
                     e-runtime-store-session-worker-context-receipt-byte-limit)))
              (setq tool-receipt-total-count
                    (1+ tool-receipt-total-count))
              (push (cons receipt-bytes
                          (e-session-query--copy-value projected))
                    tool-receipts)
              (setq tool-receipt-bytes (+ tool-receipt-bytes receipt-bytes))
              (while (or (> (length tool-receipts)
                            e-runtime-store-session-worker-context-receipt-limit)
                         (> tool-receipt-bytes
                            e-runtime-store-session-worker-context-receipt-byte-limit))
                (let ((oldest (car (last tool-receipts))))
                  (setq tool-receipt-bytes
                        (- tool-receipt-bytes (car oldest))
                        tool-receipts (butlast tool-receipts)))))))))
    (dolist (entry entries)
      (when (equal (plist-get entry :record-type) "compaction")
        (let* ((record (plist-get entry :record))
               (candidate (plist-get record :first-kept-entry-id)))
          (when (and (stringp candidate) (gethash candidate path-ids))
            (setq latest-compaction record boundary candidate)))))
    (let ((inside (null boundary)))
      (dolist (entry entries)
        (let* ((record (plist-get entry :record))
               (record-id (plist-get record :id))
               (record-type (plist-get entry :record-type))
               (path-index (plist-get entry :path-index)))
          (when (equal record-id boundary) (setq inside t))
          (when (and inside (equal record-type "message"))
            (let* ((message
                    (e-runtime-store-session-worker--message-with-display
                     (plist-get record :message)
                     (plist-get entry :display-payload)))
                   (message-bytes
                    (e-runtime-store-codec-measure-bounded
                     message
                     e-runtime-store-session-worker-context-path-byte-limit)))
              (when (> (+ bytes message-bytes)
                       e-runtime-store-session-worker-context-path-byte-limit)
                (e-runtime-store-session-worker--error
                 "Selected session context exceeds byte limit"
                 session-id
                 e-runtime-store-session-worker-context-path-byte-limit))
              (setq bytes (+ bytes message-bytes))
              (push message messages)
              (push path-index message-path-indexes)))
          (when (and inside
                     (member record-type
                             '("context-generation" "context-promotion"
                               "context-curation-package")))
            (let ((record-bytes
                   (e-runtime-store-codec-measure-bounded
                    record
                    e-runtime-store-session-worker-context-path-byte-limit)))
              (when (> (+ bytes record-bytes)
                       e-runtime-store-session-worker-context-path-byte-limit)
                (e-runtime-store-session-worker--error
                 "Selected session context exceeds byte limit"
                 session-id
                 e-runtime-store-session-worker-context-path-byte-limit))
              (setq bytes (+ bytes record-bytes))
              (push
               (append
                (list :path-index path-index :record-type record-type
                      :record
                      (e-runtime-store-session-worker--detach-query-content
                       record))
                (when (equal record-type "context-generation")
                  (let* ((context-record (plist-get record :context-record))
                         (covered
                          (plist-get context-record
                                     :covered-session-boundary)))
                    (list :covered-boundary-index
                          (and covered (gethash covered path-ids))))))
               context-records))))))
    (list :session-id session-id :current-branch branch
          :current-head-id head-id
          :current-head-path-index (and entries (1- (length entries)))
          :metadata (e-session-query--copy-value metadata)
          :turn-options (e-session-query--copy-value turn-options)
          :compaction
          (and latest-compaction
               (list :id (plist-get latest-compaction :id)
                     :summary (plist-get latest-compaction :summary)
                     :first-kept-entry-id boundary))
          :messages (nreverse messages)
          :message-path-indexes (nreverse message-path-indexes)
          :context-records (nreverse context-records)
          :tool-receipts (mapcar #'cdr (nreverse tool-receipts))
          :tool-receipt-total-count tool-receipt-total-count
          :tool-receipt-byte-count tool-receipt-bytes
          :tool-receipt-byte-limit
          e-runtime-store-session-worker-context-receipt-byte-limit
          :record-limit e-runtime-store-session-worker-context-path-row-limit
          :byte-count bytes
          :byte-limit e-runtime-store-session-worker-context-path-byte-limit
          :high-water
          (e-runtime-store-session-worker--position database session-id))))

(defun e-runtime-store-session-worker--header (database body)
  "Return bounded metadata for SESSION-ID's journal."
  (let ((session-id (e-runtime-store-session-worker--session-id
                     (plist-get body :session-id))))
    (let ((row (car (sqlite-select
                    database
                    "SELECT COUNT(*),COALESCE(SUM(LENGTH(payload)),0),COALESCE(MAX(position),0) FROM session_records WHERE session_id=?"
                    (vector session-id)))))
      (list :session-id session-id
            :present (> (e-runtime-store-session-worker--column row 0) 0)
            :record-count (e-runtime-store-session-worker--column row 0)
            :byte-size (e-runtime-store-session-worker--column row 1)
            :revision (e-runtime-store-session-worker--column row 2)
            :reference session-id))))

(defun e-runtime-store-session-worker-read (database body)
  "Execute one typed bounded session read BODY."
  (let ((e-runtime-store-session-worker--database database))
    (pcase (plist-get body :op)
      ((or 'session-query-state 'session-query-state-get 'session-state-get)
       (e-runtime-store-session-worker--query-state database body))
      ((or 'session-metadata 'session-metadata-get)
       (e-runtime-store-session-worker--metadata database body))
      ((or 'session-board-association 'session-board-association-get)
       (e-runtime-store-session-worker--association database body))
      ((or 'session-query-page 'session-state-page
           'session-recent-page 'session-root-page)
       (e-runtime-store-session-worker--query-page database body))
      ('session-id-page
       (e-runtime-store-session-worker--id-page database body))
      ((or 'session-record-page 'session-history-page)
       (e-runtime-store-session-worker--record-page database body))
      ('session-process-report-marker-page
       (e-runtime-store-session-worker--process-report-marker-page
        database body))
      ('session-process-report-marker
       (e-runtime-store-session-worker--process-report-marker database body))
      ('session-process-report-triage-page
       (e-runtime-store-session-worker--process-report-association-page
        database body "triage" :triage))
      ('session-process-report-extraction-page
       (e-runtime-store-session-worker--process-report-association-page
        database body "extraction" :extractions))
      ('session-process-report-request-shapes
       (e-runtime-store-session-worker--process-report-request-shapes
        database body))
      ('session-process-report-marker-count
       (e-runtime-store-session-worker--process-report-marker-count
        database body))
      ('session-recent-failures
       (e-runtime-store-session-worker--recent-failures database body))
      ('session-turn-inspection
       (e-runtime-store-session-worker--turn-inspection database body))
      ((or 'session-visible-message-page 'session-visible-messages)
       (e-runtime-store-session-worker--visible-message-page database body))
      ('session-context-path
       (e-runtime-store-session-worker--context-path database body))
      ('session-header (e-runtime-store-session-worker--header database body))
      (_ (e-runtime-store-session-worker--error
          "Unknown session read operation" (plist-get body :op))))))

(provide 'e-runtime-store-session-worker)

;;; e-runtime-store-session-worker.el ends here

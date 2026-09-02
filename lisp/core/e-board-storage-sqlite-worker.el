;;; e-board-storage-sqlite-worker.el --- Worker-side durable Board SQL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the private SQLite schema and typed commands for durable Boards.  The
;; generic runtime worker supplies one transaction-scoped connection and calls
;; the narrow initialize/write/read dispatch operations.  This module owns no
;; process transport or transaction acknowledgement.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'e-runtime-store-codec)

(defconst e-board-storage-sqlite-worker-page-byte-limit (* 1024 1024)
  "Private encoded-payload budget for one Board record page.")

(defconst e-board-storage-sqlite-worker-session-record-byte-limit
  (* 16 1024 1024)
  "Private session-record bound shared by the one composite admission.")

(defvar e-board-storage-sqlite-worker--database nil)

(defun e-board-storage-sqlite-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-board-storage-sqlite-worker--sql-value (value)
  "Encode VALUE for a SQLite TEXT field."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-board-storage-sqlite-worker--value (text)
  "Decode exact value from SQLite TEXT."
  (and text
       (e-runtime-store-codec-decode (base64-decode-string text))))

(defun e-board-storage-sqlite-worker-initialize (database)
  "Create private Board tables on DATABASE."
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS boards (board_id TEXT PRIMARY KEY, trusted_principal TEXT, generation INTEGER NOT NULL, revision INTEGER NOT NULL, next_position INTEGER NOT NULL, root_payload TEXT)"
         "CREATE TABLE IF NOT EXISTS board_records (board_id TEXT NOT NULL, generation INTEGER NOT NULL, position INTEGER NOT NULL, record_kind TEXT NOT NULL, record_id TEXT NOT NULL, source_kind TEXT, source_key TEXT, source_hash TEXT, payload TEXT NOT NULL, PRIMARY KEY(board_id,generation,position), UNIQUE(board_id,generation,record_id), UNIQUE(board_id,generation,source_kind,source_key), FOREIGN KEY(board_id) REFERENCES boards(board_id) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS board_records_selector ON board_records(board_id,generation,record_kind,position)"
         "CREATE TABLE IF NOT EXISTS board_record_tags (board_id TEXT NOT NULL, generation INTEGER NOT NULL, position INTEGER NOT NULL, tag TEXT NOT NULL, PRIMARY KEY(board_id,generation,position,tag), FOREIGN KEY(board_id,generation,position) REFERENCES board_records(board_id,generation,position) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS board_record_tags_selector ON board_record_tags(board_id,generation,tag,position)"
         "CREATE TABLE IF NOT EXISTS board_record_attributes (board_id TEXT NOT NULL, generation INTEGER NOT NULL, position INTEGER NOT NULL, attribute_key TEXT NOT NULL, attribute_value TEXT NOT NULL, PRIMARY KEY(board_id,generation,position,attribute_key), FOREIGN KEY(board_id,generation,position) REFERENCES board_records(board_id,generation,position) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS board_record_attributes_selector ON board_record_attributes(board_id,generation,attribute_key,attribute_value,position)"
         "CREATE TABLE IF NOT EXISTS board_routing (board_id TEXT NOT NULL, generation INTEGER NOT NULL, message_id TEXT NOT NULL, outcome TEXT NOT NULL, payload TEXT NOT NULL, revision INTEGER NOT NULL, PRIMARY KEY(board_id,generation,message_id), FOREIGN KEY(board_id) REFERENCES boards(board_id) ON DELETE CASCADE)"
         "CREATE TABLE IF NOT EXISTS board_pickups (delivery_key TEXT PRIMARY KEY, board_id TEXT NOT NULL, generation INTEGER NOT NULL, participant_id TEXT NOT NULL, fifo_position INTEGER NOT NULL, message_id TEXT NOT NULL, state TEXT NOT NULL, revision INTEGER NOT NULL, attempt INTEGER NOT NULL, payload TEXT NOT NULL, UNIQUE(board_id,generation,participant_id,fifo_position), FOREIGN KEY(board_id) REFERENCES boards(board_id) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS board_pickups_unresolved ON board_pickups(board_id,generation,participant_id,state,fifo_position)"
         "CREATE TABLE IF NOT EXISTS board_pickup_events (delivery_key TEXT NOT NULL, event_position INTEGER NOT NULL, state TEXT NOT NULL, payload TEXT, created_at REAL NOT NULL, PRIMARY KEY(delivery_key,event_position), FOREIGN KEY(delivery_key) REFERENCES board_pickups(delivery_key) ON DELETE CASCADE)"
         "CREATE TABLE IF NOT EXISTS board_participants (board_id TEXT NOT NULL, generation INTEGER NOT NULL, participant_id TEXT NOT NULL, payload TEXT NOT NULL, revision INTEGER NOT NULL, PRIMARY KEY(board_id,generation,participant_id), FOREIGN KEY(board_id) REFERENCES boards(board_id) ON DELETE CASCADE)"
         "CREATE TABLE IF NOT EXISTS board_replay_progress (board_id TEXT NOT NULL, generation INTEGER NOT NULL, subscription_id TEXT NOT NULL, position INTEGER NOT NULL, revision INTEGER NOT NULL, PRIMARY KEY(board_id,generation,subscription_id), FOREIGN KEY(board_id) REFERENCES boards(board_id) ON DELETE CASCADE)"
         "CREATE TABLE IF NOT EXISTS board_session_admissions (delivery_key TEXT PRIMARY KEY, session_id TEXT NOT NULL, session_position INTEGER NOT NULL, lane TEXT NOT NULL, payload TEXT NOT NULL, FOREIGN KEY(delivery_key) REFERENCES board_pickups(delivery_key) ON DELETE CASCADE)"))
    (sqlite-execute database statement)))

(defun e-board-storage-sqlite-worker--board-row (board-id)
  "Return BOARD-ID's root row or signal."
  (or (car (sqlite-select
            e-board-storage-sqlite-worker--database
            "SELECT trusted_principal,generation,revision,next_position,root_payload FROM boards WHERE board_id=?"
            (vector board-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Unknown Board" board-id))))

(defun e-board-storage-sqlite-worker--board-check (body)
  "Return BODY's current Board row after its generation fence."
  (let* ((board-id (plist-get body :board-id))
         (row (e-board-storage-sqlite-worker--board-row board-id))
         (generation (e-board-storage-sqlite-worker--column row 1))
         (expected-generation (plist-get body :generation)))
    (when (and expected-generation (/= expected-generation generation))
      (signal 'e-runtime-store-board-conflict
              (list "Stale Board generation" board-id
                    expected-generation generation)))
    row))

(defun e-board-storage-sqlite-worker--board-create (body)
  "Create BODY's durable Board root."
  (let* ((board-id (plist-get body :board-id))
         (existing (car (sqlite-select
                         e-board-storage-sqlite-worker--database
                         "SELECT trusted_principal,generation,revision,root_payload FROM boards WHERE board_id=?"
                         (vector board-id))))
         (principal (e-board-storage-sqlite-worker--sql-value
                     (plist-get body :trusted-principal)))
         (root (e-board-storage-sqlite-worker--sql-value (plist-get body :root))))
    (if existing
        (if (and (equal principal (e-board-storage-sqlite-worker--column existing 0))
                 (equal root (e-board-storage-sqlite-worker--column existing 3)))
            (list :board-id board-id
                  :trusted-principal
                  (e-board-storage-sqlite-worker--value
                   (e-board-storage-sqlite-worker--column existing 0))
                  :generation (e-board-storage-sqlite-worker--column existing 1)
                  :revision (e-board-storage-sqlite-worker--column existing 2)
                  :status 'existing)
          (signal 'e-runtime-store-board-conflict
                  (list "Board identity conflicts" board-id)))
      (sqlite-execute
       e-board-storage-sqlite-worker--database
       "INSERT INTO boards(board_id,trusted_principal,generation,revision,next_position,root_payload) VALUES(?,?,1,1,0,?)"
       (vector board-id principal root))
      (list :board-id board-id
            :trusted-principal (plist-get body :trusted-principal)
            :generation 1 :revision 1 :status 'created))))

(defun e-board-storage-sqlite-worker--board-clear (body)
  "Advance BODY's Board generation without deleting prior audit."
  (let* ((row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (previous-generation (e-board-storage-sqlite-worker--column row 1))
         (generation (1+ previous-generation))
         (revision (1+ (e-board-storage-sqlite-worker--column row 2))))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "UPDATE boards SET generation=?,revision=?,next_position=0 WHERE board_id=?"
     (vector generation revision board-id))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "INSERT INTO board_participants(board_id,generation,participant_id,payload,revision) SELECT board_id,?,participant_id,payload,1 FROM board_participants WHERE board_id=? AND generation=?"
     (vector generation board-id previous-generation))
    (list :board-id board-id :generation generation :revision revision)))

(defun e-board-storage-sqlite-worker--board-record-put (body)
  "Append one canonical Board record from BODY."
  (catch 'e-board-storage-sqlite-worker--board-record-result
    (let* ((row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-storage-sqlite-worker--column row 1))
         (revision (e-board-storage-sqlite-worker--column row 2))
         (position (1+ (e-board-storage-sqlite-worker--column row 3)))
         (record (plist-get body :record))
         (record-id (plist-get record :id))
         (record-kind (symbol-name (plist-get record :record-kind)))
         (source (plist-get body :source))
         (source-kind (and source (symbol-name (plist-get source :kind))))
         (source-key (and source
                          (e-board-storage-sqlite-worker--sql-value
                           (plist-get source :key))))
         (source-hash (and source (plist-get source :hash)))
         (payload (e-board-storage-sqlite-worker--sql-value record)))
    (when source-key
      (when-let* ((existing
                   (car (sqlite-select
                         e-board-storage-sqlite-worker--database
                         "SELECT source_hash,payload,position FROM board_records WHERE board_id=? AND generation=? AND source_kind=? AND source_key=?"
                         (vector board-id generation source-kind source-key)))))
        (unless (equal source-hash
                       (e-board-storage-sqlite-worker--column existing 0))
          (signal 'e-runtime-store-board-conflict
                  (list "Board source key conflicts" board-id
                        (plist-get source :key))))
        (throw 'e-board-storage-sqlite-worker--board-record-result
          (list :board-id board-id :generation generation :revision revision
                :position (e-board-storage-sqlite-worker--column existing 2)
                :status 'duplicate
                :record (e-board-storage-sqlite-worker--value
                         (e-board-storage-sqlite-worker--column existing 1))))))
    (when (car (sqlite-select
                e-board-storage-sqlite-worker--database
                "SELECT 1 FROM board_records WHERE board_id=? AND generation=? AND record_id=?"
                (vector board-id generation record-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Board record id conflicts" board-id record-id)))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "INSERT INTO board_records(board_id,generation,position,record_kind,record_id,source_kind,source_key,source_hash,payload) VALUES(?,?,?,?,?,?,?,?,?)"
     (vector board-id generation position record-kind record-id source-kind
             source-key source-hash payload))
    (dolist (tag (plist-get record :selector-tags))
      (sqlite-execute
       e-board-storage-sqlite-worker--database
       "INSERT INTO board_record_tags(board_id,generation,position,tag) VALUES(?,?,?,?)"
       (vector board-id generation position
               (e-board-storage-sqlite-worker--sql-value tag))))
    (let ((attributes (plist-get record :selector-attributes)))
      (while attributes
        (sqlite-execute
         e-board-storage-sqlite-worker--database
         "INSERT INTO board_record_attributes(board_id,generation,position,attribute_key,attribute_value) VALUES(?,?,?,?,?)"
         (vector board-id generation position
                 (e-board-storage-sqlite-worker--sql-value (car attributes))
                 (e-board-storage-sqlite-worker--sql-value (cadr attributes))))
        (setq attributes (cddr attributes))))
    (cl-incf revision)
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "UPDATE boards SET revision=?,next_position=? WHERE board_id=?"
     (vector revision position board-id))
      (list :board-id board-id :generation generation :revision revision
            :position position :status 'posted :record record))))

(defun e-board-storage-sqlite-worker--pickup-key (delivery-id)
  "Return the exact durable key for DELIVERY-ID."
  (e-board-storage-sqlite-worker--sql-value delivery-id))

(defun e-board-storage-sqlite-worker--pickup-event
    (delivery-key state &optional payload)
  "Append one immutable pickup STATE event for DELIVERY-KEY."
  (let* ((row (car (sqlite-select
                    e-board-storage-sqlite-worker--database
                    "SELECT COALESCE(MAX(event_position),0) FROM board_pickup_events WHERE delivery_key=?"
                    (vector delivery-key))))
         (position (1+ (e-board-storage-sqlite-worker--column row 0))))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "INSERT INTO board_pickup_events(delivery_key,event_position,state,payload,created_at) VALUES(?,?,?,?,?)"
     (vector delivery-key position (symbol-name state)
             (and payload (e-board-storage-sqlite-worker--sql-value payload))
             (float-time)))
    position))

(defun e-board-storage-sqlite-worker--board-routing-put (body)
  "Commit one final routing outcome and immutable pickup set from BODY."
  (let* ((row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-storage-sqlite-worker--column row 1))
         (revision (e-board-storage-sqlite-worker--column row 2))
         (message-id (plist-get body :message-id))
         (outcome (plist-get body :outcome))
         (payload (e-board-storage-sqlite-worker--sql-value outcome)))
    (when (car (sqlite-select
                e-board-storage-sqlite-worker--database
                "SELECT 1 FROM board_routing WHERE board_id=? AND generation=? AND message_id=?"
                (vector board-id generation message-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Board routing outcome is final" board-id message-id)))
    (cl-incf revision)
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "INSERT INTO board_routing(board_id,generation,message_id,outcome,payload,revision) VALUES(?,?,?,?,?,?)"
     (vector board-id generation message-id
             (symbol-name (plist-get outcome :state)) payload revision))
    (let (committed-pickups)
      (dolist (pickup (append (plist-get body :pickups) nil))
        (let* ((delivery-id (plist-get pickup :delivery-id))
               (delivery-key (e-board-storage-sqlite-worker--pickup-key delivery-id))
               (participant-id (plist-get pickup :participant-id))
               (fifo-row
                (car (sqlite-select
                      e-board-storage-sqlite-worker--database
                      "SELECT COALESCE(MAX(fifo_position),0) FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=?"
                      (vector board-id generation participant-id))))
               (fifo-position (1+ (e-board-storage-sqlite-worker--column fifo-row 0)))
               (active
                (car (sqlite-select
                      e-board-storage-sqlite-worker--database
                      "SELECT 1 FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=? AND state IN ('pending','ready','claimed','accepted','cancelling') LIMIT 1"
                      (vector board-id generation participant-id))))
               (state (if active 'pending 'ready))
               (stored (append pickup
                               (list :fifo-position fifo-position :state state
                                     :revision 1))))
          (sqlite-execute
           e-board-storage-sqlite-worker--database
           "INSERT INTO board_pickups(delivery_key,board_id,generation,participant_id,fifo_position,message_id,state,revision,attempt,payload) VALUES(?,?,?,?,?,?,?,?,0,?)"
           (vector delivery-key board-id generation participant-id fifo-position
                   message-id (symbol-name state) 1
                   (e-board-storage-sqlite-worker--sql-value stored)))
          (e-board-storage-sqlite-worker--pickup-event delivery-key state)
          (push stored committed-pickups)))
      (sqlite-execute
       e-board-storage-sqlite-worker--database
       "UPDATE boards SET revision=? WHERE board_id=?"
       (vector revision board-id))
      (list :board-id board-id :generation generation :revision revision
            :message-id message-id :outcome outcome
            :pickups (nreverse committed-pickups)))))

(defun e-board-storage-sqlite-worker--pickup-row (board-id generation delivery-id)
  "Return DELIVERY-ID row for BOARD-ID GENERATION or signal."
  (or (car (sqlite-select
            e-board-storage-sqlite-worker--database
            "SELECT delivery_key,participant_id,fifo_position,state,revision,attempt,payload FROM board_pickups WHERE delivery_key=? AND board_id=? AND generation=?"
            (vector (e-board-storage-sqlite-worker--pickup-key delivery-id)
                    board-id generation)))
      (signal 'e-runtime-store-board-conflict
              (list "Unknown Board pickup" board-id delivery-id))))

(defun e-board-storage-sqlite-worker--pickup-promote-next
    (board-id generation participant-id fifo-position)
  "Promote PARTICIPANT-ID's next FIFO pickup after FIFO-POSITION."
  (when-let* ((next
               (car (sqlite-select
                     e-board-storage-sqlite-worker--database
                     "SELECT delivery_key,payload,revision FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=? AND fifo_position>? AND state='pending' ORDER BY fifo_position LIMIT 1"
                     (vector board-id generation participant-id fifo-position)))))
    (let* ((delivery-key (e-board-storage-sqlite-worker--column next 0))
           (payload (e-board-storage-sqlite-worker--value
                     (e-board-storage-sqlite-worker--column next 1)))
           (revision (1+ (e-board-storage-sqlite-worker--column next 2))))
      (setq payload (plist-put payload :state 'ready))
      (setq payload (plist-put payload :revision revision))
      (sqlite-execute
       e-board-storage-sqlite-worker--database
       "UPDATE board_pickups SET state='ready',revision=?,payload=? WHERE delivery_key=?"
       (vector revision (e-board-storage-sqlite-worker--sql-value payload) delivery-key))
      (e-board-storage-sqlite-worker--pickup-event delivery-key 'ready)
      payload)))

(defun e-board-storage-sqlite-worker--board-pickup-transition (body)
  "Commit one typed Board pickup transition from BODY."
  (let* ((board-row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-storage-sqlite-worker--column board-row 1))
         (board-revision (e-board-storage-sqlite-worker--column board-row 2))
         (delivery-id (plist-get body :delivery-id))
         (row (e-board-storage-sqlite-worker--pickup-row board-id generation delivery-id))
         (delivery-key (e-board-storage-sqlite-worker--column row 0))
         (participant-id (e-board-storage-sqlite-worker--column row 1))
         (fifo-position (e-board-storage-sqlite-worker--column row 2))
         (state (intern (e-board-storage-sqlite-worker--column row 3)))
         (revision (e-board-storage-sqlite-worker--column row 4))
         (attempt (e-board-storage-sqlite-worker--column row 5))
         (payload (e-board-storage-sqlite-worker--value
                   (e-board-storage-sqlite-worker--column row 6)))
         (transition (plist-get body :transition))
         (data (plist-get body :data))
         next-state terminal-p)
    (pcase transition
      ('claim
       (unless (eq state 'ready)
         (signal 'e-runtime-store-board-conflict
                 (list "Pickup is not ready" delivery-id state)))
       (setq next-state 'claimed attempt (1+ attempt)))
      ('accept
       (unless (eq state 'claimed)
         (signal 'e-runtime-store-board-conflict
                 (list "Pickup is not claimed" delivery-id state)))
       (setq next-state 'accepted))
      ('consume
       (unless (memq state '(claimed accepted cancelling))
         (signal 'e-runtime-store-board-conflict
                 (list "Pickup cannot be consumed" delivery-id state)))
       (setq next-state 'consumed terminal-p t))
      ('retry
       (unless (eq state 'claimed)
         (signal 'e-runtime-store-board-conflict
                 (list "Pickup cannot be retried" delivery-id state)))
       (setq next-state 'ready))
      ('cancel
       (unless (memq state '(pending ready claimed accepted cancelling))
         (signal 'e-runtime-store-board-conflict
                 (list "Pickup cannot be cancelled" delivery-id state)))
       (if (memq state '(claimed accepted))
           (setq next-state 'cancelling)
         (setq next-state 'cancelled terminal-p t)))
      ((or 'discard 'fail 'expire 'uncertain 'tombstone)
       (setq next-state
             (pcase transition
               ('discard 'discarded) ('fail 'failed) ('expire 'expired)
               ('uncertain 'uncertain) (_ 'tombstoned))
             terminal-p t))
      (_ (signal 'e-runtime-store-board-conflict
                 (list "Unknown pickup transition" transition))))
    (cl-incf revision)
    (cl-incf board-revision)
    (setq payload (plist-put payload :state next-state))
    (setq payload (plist-put payload :revision revision))
    (setq payload (plist-put payload :attempt attempt))
    (when data (setq payload (plist-put payload :transition-data data)))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "UPDATE board_pickups SET state=?,revision=?,attempt=?,payload=? WHERE delivery_key=?"
     (vector (symbol-name next-state) revision attempt
             (e-board-storage-sqlite-worker--sql-value payload) delivery-key))
    (e-board-storage-sqlite-worker--pickup-event delivery-key next-state data)
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "UPDATE boards SET revision=? WHERE board_id=?"
     (vector board-revision board-id))
    (list :board-id board-id :generation generation :revision board-revision
          :pickup payload
          :next (and terminal-p
                     (e-board-storage-sqlite-worker--pickup-promote-next
                      board-id generation participant-id fifo-position)))))

(defun e-board-storage-sqlite-worker--board-participant-put (body)
  "Persist one logical Board participant projection from BODY."
  (let* ((row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-storage-sqlite-worker--column row 1))
         (revision (1+ (e-board-storage-sqlite-worker--column row 2)))
         (participant (plist-get body :participant))
         (participant-id (plist-get participant :id)))
    (when (car (sqlite-select
                e-board-storage-sqlite-worker--database
                "SELECT 1 FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
                (vector board-id generation participant-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Board participant conflicts" board-id participant-id)))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "INSERT INTO board_participants(board_id,generation,participant_id,payload,revision) VALUES(?,?,?,?,1)"
     (vector board-id generation participant-id
             (e-board-storage-sqlite-worker--sql-value participant)))
    (sqlite-execute e-board-storage-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector revision board-id))
    (list :board-id board-id :generation generation :revision revision
          :participant participant)))

(defun e-board-storage-sqlite-worker--board-participant-delete (body)
  "Delete one unpublished participant projection from BODY."
  (let* ((row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-storage-sqlite-worker--column row 1))
         (revision (1+ (e-board-storage-sqlite-worker--column row 2)))
         (participant-id (plist-get body :participant-id))
         (participant-row
          (car (sqlite-select
                e-board-storage-sqlite-worker--database
                "SELECT payload FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
                (vector board-id generation participant-id))))
         (participant
          (and participant-row
               (e-board-storage-sqlite-worker--value
                (e-board-storage-sqlite-worker--column participant-row 0))))
         (subscription-id (plist-get participant :subscription-id)))
    (unless participant-row
      (signal 'e-runtime-store-board-conflict
              (list "Unknown Board participant" board-id participant-id)))
    (when (car (sqlite-select
                e-board-storage-sqlite-worker--database
                "SELECT 1 FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=? LIMIT 1"
                (vector board-id generation participant-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Board participant has durable pickups"
                    board-id participant-id)))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "DELETE FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
     (vector board-id generation participant-id))
    (when subscription-id
      (sqlite-execute
       e-board-storage-sqlite-worker--database
       "DELETE FROM board_replay_progress WHERE board_id=? AND generation=? AND subscription_id=?"
       (vector board-id generation subscription-id)))
    (sqlite-execute e-board-storage-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector revision board-id))
    (list :board-id board-id :generation generation :revision revision
          :participant-id participant-id :deleted t)))

(defun e-board-storage-sqlite-worker--board-participant-publish (body)
  "Publish one provisional durable participant from BODY."
  (let* ((row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-storage-sqlite-worker--column row 1))
         (revision (1+ (e-board-storage-sqlite-worker--column row 2)))
         (participant-id (plist-get body :participant-id))
         (participant-row
          (car (sqlite-select
                e-board-storage-sqlite-worker--database
                "SELECT payload,revision FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
                (vector board-id generation participant-id))))
         (participant
          (and participant-row
               (e-board-storage-sqlite-worker--value
                (e-board-storage-sqlite-worker--column participant-row 0))))
         (participant-revision
          (and participant-row
               (1+ (e-board-storage-sqlite-worker--column participant-row 1)))))
    (unless participant-row
      (signal 'e-runtime-store-board-conflict
              (list "Unknown Board participant" board-id participant-id)))
    (unless (plist-get participant :publication-pending)
      (signal 'e-runtime-store-board-conflict
              (list "Board participant is already published"
                    board-id participant-id)))
    (setq participant (plist-put participant :publication-pending nil))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "UPDATE board_participants SET payload=?,revision=? WHERE board_id=? AND generation=? AND participant_id=?"
     (vector (e-board-storage-sqlite-worker--sql-value participant)
             participant-revision board-id generation participant-id))
    (sqlite-execute e-board-storage-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector revision board-id))
    (list :board-id board-id :generation generation :revision revision
          :participant participant)))

(defun e-board-storage-sqlite-worker--board-replay-progress-put (body)
  "Persist stable subscription replay progress from BODY."
  (let* ((row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-storage-sqlite-worker--column row 1))
         (revision (1+ (e-board-storage-sqlite-worker--column row 2)))
         (subscription-id (plist-get body :subscription-id))
         (position (plist-get body :position)))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "INSERT INTO board_replay_progress(board_id,generation,subscription_id,position,revision) VALUES(?,?,?,?,1) ON CONFLICT(board_id,generation,subscription_id) DO UPDATE SET position=MAX(position,excluded.position),revision=board_replay_progress.revision+1"
     (vector board-id generation subscription-id position))
    (sqlite-execute e-board-storage-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector revision board-id))
    (list :board-id board-id :generation generation :revision revision
          :subscription-id subscription-id :position position)))

(defun e-board-storage-sqlite-worker--board-pickup-session-admit (body)
  "Atomically accept a claimed pickup and append its session admission record."
  (let* ((board-row (e-board-storage-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-storage-sqlite-worker--column board-row 1))
         (board-revision (e-board-storage-sqlite-worker--column board-row 2))
         (delivery-id (plist-get body :delivery-id))
         (pickup-row
          (e-board-storage-sqlite-worker--pickup-row board-id generation delivery-id))
         (delivery-key (e-board-storage-sqlite-worker--column pickup-row 0))
         (state (intern (e-board-storage-sqlite-worker--column pickup-row 3)))
         (pickup-revision (e-board-storage-sqlite-worker--column pickup-row 4))
         (pickup-payload
          (e-board-storage-sqlite-worker--value
           (e-board-storage-sqlite-worker--column pickup-row 6)))
         (session-id (plist-get body :session-id))
         (session-revision
          (or (caar (sqlite-select
                     e-board-storage-sqlite-worker--database
                     "SELECT MAX(position) FROM session_records WHERE session_id=?"
                     (vector session-id)))
              0))
         (session-position (1+ session-revision))
         (record (plist-get body :record))
         (record-payload (e-board-storage-sqlite-worker--sql-value record))
         (lane (plist-get body :lane)))
    (unless (eq state 'claimed)
      (signal 'e-runtime-store-board-conflict
              (list "Pickup admission requires a claim" delivery-id state)))
    (when (> (string-bytes record-payload)
             e-board-storage-sqlite-worker-session-record-byte-limit)
      (signal 'e-runtime-store-worker-error
              (list "Session record exceeds private limit"
                    (string-bytes record-payload)
                    e-board-storage-sqlite-worker-session-record-byte-limit)))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
     (vector session-id session-position record-payload))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "INSERT INTO board_session_admissions(delivery_key,session_id,session_position,lane,payload) VALUES(?,?,?,?,?)"
     (vector delivery-key session-id session-position (symbol-name lane)
             record-payload))
    (cl-incf pickup-revision)
    (cl-incf board-revision)
    (setq pickup-payload (plist-put pickup-payload :state 'accepted))
    (setq pickup-payload (plist-put pickup-payload :revision pickup-revision))
    (sqlite-execute
     e-board-storage-sqlite-worker--database
     "UPDATE board_pickups SET state='accepted',revision=?,payload=? WHERE delivery_key=?"
     (vector pickup-revision
             (e-board-storage-sqlite-worker--sql-value pickup-payload) delivery-key))
    (e-board-storage-sqlite-worker--pickup-event
     delivery-key 'accepted (list :session-id session-id :lane lane))
    (sqlite-execute e-board-storage-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector board-revision board-id))
    (list :board-id board-id :generation generation
          :board-revision board-revision :pickup-revision pickup-revision
          :delivery-id delivery-id :session-id session-id
          :session-revision session-position :lane lane :record record)))


(defun e-board-storage-sqlite-worker-write (database body)
  "Execute one typed Board write BODY on transaction-scoped DATABASE."
  (let ((e-board-storage-sqlite-worker--database database))
    (pcase (plist-get body :op)
      ('board-create (e-board-storage-sqlite-worker--board-create body))
      ('board-clear (e-board-storage-sqlite-worker--board-clear body))
      ('board-record-put (e-board-storage-sqlite-worker--board-record-put body))
      ('board-routing-put (e-board-storage-sqlite-worker--board-routing-put body))
      ('board-pickup-transition
       (e-board-storage-sqlite-worker--board-pickup-transition body))
      ('board-participant-put
       (e-board-storage-sqlite-worker--board-participant-put body))
      ('board-participant-delete
       (e-board-storage-sqlite-worker--board-participant-delete body))
      ('board-participant-publish
       (e-board-storage-sqlite-worker--board-participant-publish body))
      ('board-replay-progress-put
       (e-board-storage-sqlite-worker--board-replay-progress-put body))
      ('board-pickup-session-admit
       (e-board-storage-sqlite-worker--board-pickup-session-admit body))
      (_ (signal 'e-runtime-store-worker-error
                 (list "Unknown Board write operation"
                       (plist-get body :op)))))))

(defun e-board-storage-sqlite-worker-read (database body)
  "Execute one typed Board read BODY on DATABASE."
  (let ((e-board-storage-sqlite-worker--database database))
    (pcase (plist-get body :op)
    ('board-get
     (when-let* ((row (car (sqlite-select
                            e-board-storage-sqlite-worker--database
                            "SELECT board_id,trusted_principal,generation,revision,next_position,root_payload FROM boards WHERE board_id=?"
                            (vector (plist-get body :board-id))))))
       (list :board-id (e-board-storage-sqlite-worker--column row 0)
             :trusted-principal
             (e-board-storage-sqlite-worker--value
              (e-board-storage-sqlite-worker--column row 1))
             :generation (e-board-storage-sqlite-worker--column row 2)
             :revision (e-board-storage-sqlite-worker--column row 3)
             :next-position (e-board-storage-sqlite-worker--column row 4)
             :root (e-board-storage-sqlite-worker--value
                    (e-board-storage-sqlite-worker--column row 5)))))
    ('board-list
     (let* ((limit (min 1024 (max 1 (or (plist-get body :limit) 64))))
            (after (or (plist-get body :after) ""))
            (rows (sqlite-select
                   e-board-storage-sqlite-worker--database
                   "SELECT board_id,trusted_principal,generation,revision,next_position,root_payload FROM boards WHERE board_id>? ORDER BY board_id LIMIT ?"
                   (vector after (1+ limit))))
            (more (> (length rows) limit))
            (selected (if more (butlast rows) rows)))
       (list
        :boards
        (mapcar
         (lambda (row)
           (list :board-id (e-board-storage-sqlite-worker--column row 0)
                 :trusted-principal
                 (e-board-storage-sqlite-worker--value
                  (e-board-storage-sqlite-worker--column row 1))
                 :generation (e-board-storage-sqlite-worker--column row 2)
                 :revision (e-board-storage-sqlite-worker--column row 3)
                 :next-position (e-board-storage-sqlite-worker--column row 4)
                 :root (e-board-storage-sqlite-worker--value
                        (e-board-storage-sqlite-worker--column row 5))))
         selected)
        :next (and more (e-board-storage-sqlite-worker--column (car (last selected)) 0)))))
    ('board-record-page
     (let* ((board-id (plist-get body :board-id))
            (generation (plist-get body :generation))
            (after (or (plist-get body :after) 0))
            (limit (min 1024 (max 1 (or (plist-get body :limit) 256))))
            (selector (plist-get body :selector))
            (kinds (plist-get selector :kinds))
            (tags (or (plist-get selector :tags-all)
                      (plist-get selector :tags)))
            (attributes (plist-get selector :attributes))
            ;; Clauses are accumulated with `push' and reversed at query time.
            (clauses
             (list "r.position>?" "r.generation=?" "r.board_id=?"))
            (parameters (list board-id generation after)))
       (when kinds
         (push (format "r.record_kind IN (%s)"
                       (mapconcat (lambda (_kind) "?") kinds ","))
               clauses)
         (setq parameters
               (append parameters (mapcar #'symbol-name kinds))))
       (dolist (tag tags)
         (push "EXISTS (SELECT 1 FROM board_record_tags t WHERE t.board_id=r.board_id AND t.generation=r.generation AND t.position=r.position AND t.tag=?)"
               clauses)
         (setq parameters
               (append parameters
                       (list (e-board-storage-sqlite-worker--sql-value tag)))))
       (dolist (attribute attributes)
         (push "EXISTS (SELECT 1 FROM board_record_attributes a WHERE a.board_id=r.board_id AND a.generation=r.generation AND a.position=r.position AND a.attribute_key=? AND a.attribute_value=?)"
               clauses)
         (setq parameters
               (append parameters
                       (list
                        (e-board-storage-sqlite-worker--sql-value (car attribute))
                        (e-board-storage-sqlite-worker--sql-value (cdr attribute))))))
       (let* ((sql
               (concat
                "SELECT r.position,r.payload,LENGTH(r.payload),r.source_kind,r.source_key,r.source_hash FROM board_records r WHERE "
                (mapconcat #'identity (nreverse clauses) " AND ")
                " ORDER BY r.position LIMIT ?"))
              (rows (sqlite-select
                     e-board-storage-sqlite-worker--database sql
                     (vconcat (append parameters (list limit)))))
              (bytes 0) selected truncated)
         (catch 'full
           (dolist (row rows)
             (let ((row-bytes (e-board-storage-sqlite-worker--column row 2)))
               (when (and selected
                          (> (+ bytes row-bytes)
                             e-board-storage-sqlite-worker-page-byte-limit))
                 (setq truncated t)
                 (throw 'full nil))
               (cl-incf bytes row-bytes)
               (push
                (list :position (e-board-storage-sqlite-worker--column row 0)
                      :record
                      (e-board-storage-sqlite-worker--value
                       (e-board-storage-sqlite-worker--column row 1))
                      :source
                      (and (e-board-storage-sqlite-worker--column row 3)
                           (list
                            :kind (intern (e-board-storage-sqlite-worker--column row 3))
                            :key
                            (e-board-storage-sqlite-worker--value
                             (e-board-storage-sqlite-worker--column row 4))
                            :hash (e-board-storage-sqlite-worker--column row 5))))
                selected))))
         (setq selected (nreverse selected))
         (list :records selected :bytes bytes
               :next (and selected
                          (or truncated (= (length rows) limit))
                          (plist-get (car (last selected)) :position))))))
    ('board-routing-get
     (when-let* ((row (car (sqlite-select
                            e-board-storage-sqlite-worker--database
                            "SELECT payload,revision FROM board_routing WHERE board_id=? AND generation=? AND message_id=?"
                            (vector (plist-get body :board-id)
                                    (plist-get body :generation)
                                    (plist-get body :message-id))))))
       (list :outcome
             (e-board-storage-sqlite-worker--value
              (e-board-storage-sqlite-worker--column row 0))
             :revision (e-board-storage-sqlite-worker--column row 1))))
    ('board-pickup-list
     (let* ((participant-id (plist-get body :participant-id))
            (limit (min 4096 (max 1 (or (plist-get body :limit) 512))))
            (sql
             (concat
              "SELECT payload,state,revision,attempt FROM board_pickups WHERE board_id=? AND generation=? AND state IN ('pending','ready','claimed','accepted','cancelling')"
              (if participant-id " AND participant_id=?" "")
              " ORDER BY participant_id,fifo_position LIMIT ?"))
            (parameters
             (append (list (plist-get body :board-id)
                           (plist-get body :generation))
                     (when participant-id (list participant-id))
                     (list limit))))
       (mapcar
        (lambda (row)
          (let ((payload (e-board-storage-sqlite-worker--value
                          (e-board-storage-sqlite-worker--column row 0))))
            (setq payload
                  (plist-put payload :state
                             (intern (e-board-storage-sqlite-worker--column row 1))))
            (setq payload
                  (plist-put payload :revision
                             (e-board-storage-sqlite-worker--column row 2)))
            (plist-put payload :attempt
                       (e-board-storage-sqlite-worker--column row 3))))
        (sqlite-select e-board-storage-sqlite-worker--database sql
                       (vconcat parameters)))))
    ('board-participant-list
     (mapcar
      (lambda (row)
        (e-board-storage-sqlite-worker--value
         (e-board-storage-sqlite-worker--column row 0)))
      (sqlite-select
       e-board-storage-sqlite-worker--database
       "SELECT payload FROM board_participants WHERE board_id=? AND generation=? ORDER BY participant_id LIMIT ?"
       (vector (plist-get body :board-id) (plist-get body :generation)
               (min 4096 (max 1 (or (plist-get body :limit) 512)))))))
    ('board-replay-progress-get
     (when-let* ((row (car (sqlite-select
                            e-board-storage-sqlite-worker--database
                            "SELECT position,revision FROM board_replay_progress WHERE board_id=? AND generation=? AND subscription_id=?"
                            (vector (plist-get body :board-id)
                                    (plist-get body :generation)
                                    (plist-get body :subscription-id))))))
       (list :position (e-board-storage-sqlite-worker--column row 0)
             :revision (e-board-storage-sqlite-worker--column row 1))))
      (_ (signal 'e-runtime-store-worker-error
                 (list "Unknown Board read operation"
                       (plist-get body :op)))))))

(provide 'e-board-storage-sqlite-worker)

;;; e-board-storage-sqlite-worker.el ends here

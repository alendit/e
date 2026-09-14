;;; e-board-sqlite-worker.el --- Worker-side durable Board SQL -*- lexical-binding: t; -*-

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
(require 'e-board-orchestration)
(require 'e-board-sqlite-contract)
(require 'e-runtime-store-codec)

(defconst e-board-sqlite-worker-page-byte-limit (* 1024 1024)
  "Private encoded-payload budget for one Board record page.")

(defconst e-board-sqlite-worker-session-record-byte-limit
  (* 16 1024 1024)
  "Private session-record bound shared by the one composite admission.")

(defvar e-board-sqlite-worker--database nil)

(defconst e-board-sqlite-worker-bounded-set-limit 512
  "Maximum participant or pickup rows returned to one bounded request.")

(defconst e-board-sqlite-worker-routing-set-limit 4096
  "Maximum unaddressed routing candidates accepted by one append.

Addressed delivery performs an exact participant lookup and is not subject to
this bound.  Unaddressed fan-out fails explicitly beyond the practical bound;
it must never silently omit a participant because an internal page ended.")

(defun e-board-sqlite-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-board-sqlite-worker--sql-value (value)
  "Encode VALUE for a SQLite TEXT field."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-board-sqlite-worker--value (text)
  "Decode exact value from SQLite TEXT."
  (and text
       (e-runtime-store-codec-decode (base64-decode-string text))))

(defun e-board-sqlite-worker-initialize (database)
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

(defun e-board-sqlite-worker--board-row (board-id)
  "Return BOARD-ID's root row or signal."
  (or (car (sqlite-select
            e-board-sqlite-worker--database
            "SELECT trusted_principal,generation,revision,next_position,root_payload FROM boards WHERE board_id=?"
            (vector board-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Unknown Board" board-id))))

(defun e-board-sqlite-worker--board-check (body)
  "Return BODY's current Board row after its generation fence."
  (let* ((board-id (plist-get body :board-id))
         (row (e-board-sqlite-worker--board-row board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (expected-generation (plist-get body :generation)))
    (when (and expected-generation (/= expected-generation generation))
      (signal 'e-runtime-store-board-conflict
              (list "Stale Board generation" board-id
                    expected-generation generation)))
    row))

(defun e-board-sqlite-worker--board-create (body)
  "Create BODY's durable Board root."
  (let* ((board-id (plist-get body :board-id))
         (existing (car (sqlite-select
                         e-board-sqlite-worker--database
                         "SELECT trusted_principal,generation,revision,root_payload FROM boards WHERE board_id=?"
                         (vector board-id))))
         (principal (e-board-sqlite-worker--sql-value
                     (plist-get body :trusted-principal)))
         (root (e-board-sqlite-worker--sql-value (plist-get body :root))))
    (if existing
        (if (and (equal principal (e-board-sqlite-worker--column existing 0))
                 (equal root (e-board-sqlite-worker--column existing 3)))
            (list :board-id board-id
                  :trusted-principal
                  (e-board-sqlite-worker--value
                   (e-board-sqlite-worker--column existing 0))
                  :generation (e-board-sqlite-worker--column existing 1)
                  :revision (e-board-sqlite-worker--column existing 2)
                  :status 'existing)
          (signal 'e-runtime-store-board-conflict
                  (list "Board identity conflicts" board-id)))
      (sqlite-execute
       e-board-sqlite-worker--database
       "INSERT INTO boards(board_id,trusted_principal,generation,revision,next_position,root_payload) VALUES(?,?,1,1,0,?)"
       (vector board-id principal root))
      (list :board-id board-id
            :trusted-principal (plist-get body :trusted-principal)
            :generation 1 :revision 1 :status 'created))))

(defun e-board-sqlite-worker--board-clear (body)
  "Advance BODY's Board generation without deleting prior audit."
  (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (previous-generation (e-board-sqlite-worker--column row 1))
         (generation (1+ previous-generation))
         (revision (1+ (e-board-sqlite-worker--column row 2))))
    (sqlite-execute
     e-board-sqlite-worker--database
     "UPDATE boards SET generation=?,revision=?,next_position=0 WHERE board_id=?"
     (vector generation revision board-id))
    (sqlite-execute
     e-board-sqlite-worker--database
     "INSERT INTO board_participants(board_id,generation,participant_id,payload,revision) SELECT board_id,?,participant_id,payload,1 FROM board_participants WHERE board_id=? AND generation=?"
     (vector generation board-id previous-generation))
    (list :board-id board-id :generation generation :revision revision)))

(defun e-board-sqlite-worker--board-record-put (body)
  "Append one canonical Board record from BODY."
  (catch 'e-board-sqlite-worker--board-record-result
    (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (revision (e-board-sqlite-worker--column row 2))
         (position (1+ (e-board-sqlite-worker--column row 3)))
         (record (plist-get body :record))
         (record-id (plist-get record :id))
         (record-kind (symbol-name (plist-get record :record-kind)))
         (source (plist-get body :source))
         (source-kind (and source (symbol-name (plist-get source :kind))))
         (source-key (and source
                          (e-board-sqlite-worker--sql-value
                           (plist-get source :key))))
         (source-hash (and source (plist-get source :hash)))
         (payload (e-board-sqlite-worker--sql-value record)))
    (when source-key
      (when-let* ((existing
                   (car (sqlite-select
                         e-board-sqlite-worker--database
                         "SELECT source_hash,payload,position FROM board_records WHERE board_id=? AND generation=? AND source_kind=? AND source_key=?"
                         (vector board-id generation source-kind source-key)))))
        (unless (equal source-hash
                       (e-board-sqlite-worker--column existing 0))
          (signal 'e-runtime-store-board-conflict
                  (list "Board source key conflicts" board-id
                        (plist-get source :key))))
        (throw 'e-board-sqlite-worker--board-record-result
          (list :board-id board-id :generation generation :revision revision
                :position (e-board-sqlite-worker--column existing 2)
                :status 'duplicate
                :record (e-board-sqlite-worker--value
                         (e-board-sqlite-worker--column existing 1))))))
    (when (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT 1 FROM board_records WHERE board_id=? AND generation=? AND record_id=?"
                (vector board-id generation record-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Board record id conflicts" board-id record-id)))
    (sqlite-execute
     e-board-sqlite-worker--database
     "INSERT INTO board_records(board_id,generation,position,record_kind,record_id,source_kind,source_key,source_hash,payload) VALUES(?,?,?,?,?,?,?,?,?)"
     (vector board-id generation position record-kind record-id source-kind
             source-key source-hash payload))
    (dolist (tag (or (plist-get record :selector-tags)
                     (plist-get record :tags)))
      (sqlite-execute
       e-board-sqlite-worker--database
       "INSERT INTO board_record_tags(board_id,generation,position,tag) VALUES(?,?,?,?)"
       (vector board-id generation position
               (e-board-sqlite-worker--sql-value tag))))
    (let ((attributes (or (plist-get record :selector-attributes)
                          (plist-get record :attributes))))
      (while attributes
        (sqlite-execute
         e-board-sqlite-worker--database
         "INSERT INTO board_record_attributes(board_id,generation,position,attribute_key,attribute_value) VALUES(?,?,?,?,?)"
         (vector board-id generation position
                 (e-board-sqlite-worker--sql-value (car attributes))
                 (e-board-sqlite-worker--sql-value (cadr attributes))))
        (setq attributes (cddr attributes))))
    (cl-incf revision)
    (sqlite-execute
     e-board-sqlite-worker--database
     "UPDATE boards SET revision=?,next_position=? WHERE board_id=?"
     (vector revision position board-id))
      (list :board-id board-id :generation generation :revision revision
            :position position :status 'posted :record record))))

(defun e-board-sqlite-worker--selector-matches-p
    (selector kind tags attributes to author subject-participant-id)
  "Return non-nil when detached SELECTOR matches one candidate record."
  (let ((all (or (plist-get selector :tags-all)
                 (plist-get selector :tags)))
        (any (plist-get selector :tags-any)))
    (and (or (not (plist-member selector :kind))
             (equal (plist-get selector :kind) kind))
         (or (not (plist-member selector :to))
             (equal (plist-get selector :to) to))
         (or (not (plist-member selector :author))
             (equal (plist-get selector :author) author))
         (or (not (plist-member selector :subject-participant-id))
             (equal (plist-get selector :subject-participant-id)
                    subject-participant-id))
         (cl-every (lambda (tag) (member tag tags)) all)
         (or (null any) (cl-some (lambda (tag) (member tag tags)) any))
         (cl-every
          (lambda (clause)
            (equal (plist-get attributes (car clause)) (cdr clause)))
          (plist-get selector :attributes)))))

(defun e-board-sqlite-worker--routing-policies
    (board-id generation &optional addressed-participant-id)
  "Return exact durable routing policies for BOARD-ID GENERATION.

When ADDRESSED-PARTICIPANT-ID is non-nil, query that participant exactly and
scan association policies in bounded internal pages until its policy is found.
Unaddressed fan-out pages both relations and signals at the practical routing
bound instead of silently truncating the eligible set."
  (let ((participants (make-hash-table :test 'equal)) policies
        (participant-cursor "") (association-cursor "")
        (participant-count 0) participant-rows association-rows)
    (if addressed-participant-id
        (setq participant-rows
              (sqlite-select
               e-board-sqlite-worker--database
               "SELECT participant_id,payload FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
               (vector board-id generation addressed-participant-id)))
      (let ((more t))
        (while more
          (setq participant-rows
                (sqlite-select
                 e-board-sqlite-worker--database
                 "SELECT participant_id,payload FROM board_participants WHERE board_id=? AND generation=? AND participant_id>? ORDER BY participant_id LIMIT ?"
                 (vector board-id generation participant-cursor
                         e-board-sqlite-worker-bounded-set-limit)))
          (dolist (row participant-rows)
            (setq participant-cursor
                  (e-board-sqlite-worker--column row 0))
            (cl-incf participant-count)
            (when (> participant-count
                     e-board-sqlite-worker-routing-set-limit)
              (signal 'e-runtime-store-board-conflict
                      (list "Board routing candidate limit exceeded"
                            board-id generation
                            e-board-sqlite-worker-routing-set-limit)))
            (let ((payload
                   (e-board-sqlite-worker--value
                    (e-board-sqlite-worker--column row 1))))
              (when (memq (plist-get payload :state) '(active dormant stale))
                (puthash participant-cursor payload participants))))
          (setq more
                (= (length participant-rows)
                   e-board-sqlite-worker-bounded-set-limit)))))
    (when addressed-participant-id
      (dolist (row participant-rows)
        (let ((payload
               (e-board-sqlite-worker--value
                (e-board-sqlite-worker--column row 1))))
          (when (memq (plist-get payload :state) '(active dormant stale))
            (puthash (e-board-sqlite-worker--column row 0)
                     payload participants)))))
    ;; Association query rows are the durable home of each participant's
    ;; selectors.  Page the set within the worker transaction; never install
    ;; it in the parent process or issue one query per participant.
    (let ((more t))
      (while more
        (setq association-rows
              (sqlite-select
               e-board-sqlite-worker--database
               "SELECT session_id,routing_policy FROM session_query_state WHERE board_id=? AND routing_policy IS NOT NULL AND session_id>? ORDER BY session_id LIMIT ?"
               (vector board-id association-cursor
                       e-board-sqlite-worker-bounded-set-limit)))
        (dolist (row association-rows)
          (setq association-cursor
                (e-board-sqlite-worker--column row 0))
          (let* ((policy
                  (e-board-sqlite-worker--value
                   (e-board-sqlite-worker--column row 1)))
                 (participant-id (plist-get policy :participant-id)))
            (when (gethash participant-id participants)
              (push (list :session-id association-cursor
                          :participant-id participant-id
                          :participant (gethash participant-id participants)
                          :policy policy)
                    policies))))
        (setq more
              (and (= (length association-rows)
                      e-board-sqlite-worker-bounded-set-limit)
                   (or (null addressed-participant-id)
                       (null policies))))))
    (nreverse policies)))

(defun e-board-sqlite-worker--canonical-message-id
    (board-id generation kind source-key)
  "Return the SQLite-operation-owned stable KIND identity for SOURCE-KEY."
  (concat
   "msg_"
   (substring
    (secure-hash
     'sha256
     (e-runtime-store-codec-encode
      (list :board-id board-id :generation generation
            :kind kind :source-key source-key)))
    0 40)))

(defun e-board-sqlite-worker--canonical-append-route-result
    (board-id generation message-id status)
  "Read MESSAGE-ID's canonical append/routing result with STATUS."
  (let* ((record-row
          (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT position,payload FROM board_records WHERE board_id=? AND generation=? AND record_id=?"
                (vector board-id generation message-id))))
         (routing-row
          (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT payload,revision FROM board_routing WHERE board_id=? AND generation=? AND message_id=?"
                (vector board-id generation message-id))))
         (pickups
          (mapcar
           (lambda (row)
             (e-board-sqlite-worker--value
              (e-board-sqlite-worker--column row 0)))
           (sqlite-select
            e-board-sqlite-worker--database
            "SELECT payload FROM board_pickups WHERE board_id=? AND generation=? AND message_id=? ORDER BY participant_id,fifo_position"
            (vector board-id generation message-id))))
         (board-row (e-board-sqlite-worker--board-row board-id)))
    (unless (and record-row routing-row)
      (signal 'e-runtime-store-board-conflict
              (list "Incomplete canonical Board append" board-id message-id)))
    (list :board-id board-id :generation generation
          :revision (e-board-sqlite-worker--column board-row 2)
          :position (e-board-sqlite-worker--column record-row 0)
          :status status
          :message
          (e-board-sqlite-worker--value
           (e-board-sqlite-worker--column record-row 1))
          :routing
          (e-board-sqlite-worker--value
           (e-board-sqlite-worker--column routing-row 0))
          :pickups pickups)))

(defun e-board-sqlite-worker--append-route-association (body result)
  "Attach BODY's request-local association facts to canonical RESULT."
  (if (not (plist-get body :session-id))
      result
    (append
     result
     (list :session-id (plist-get body :session-id)
           :association
           (list :board-id (plist-get body :board-id)
                 :principal (plist-get body :principal)
                 :routing-policy
                 (copy-tree (plist-get body :routing-policy) t))))))

(defun e-board-sqlite-worker--resolve-board-input (body)
  "Return BODY plus exact Board association when it supplies only a session."
  (if (plist-get body :board-id)
      body
    (let* ((session-id (plist-get body :session-id))
           (row
            (and session-id
                 (car
                  (sqlite-select
                   e-board-sqlite-worker--database
                   "SELECT board_id,routing_policy,principal FROM session_query_state WHERE session_id=?"
                   (vector session-id)))))
           (board-id (and row
                          (e-board-sqlite-worker--column row 0))))
      (unless board-id
        (signal 'e-runtime-store-board-conflict
                (list "Session has no Board association" session-id)))
      (let ((resolved (copy-sequence body)))
        (setq resolved (plist-put resolved :board-id board-id))
        (setq resolved
              (plist-put resolved :routing-policy
                         (e-board-sqlite-worker--value
                          (e-board-sqlite-worker--column row 1))))
        (plist-put resolved :principal
                   (e-board-sqlite-worker--column row 2))))))

(defun e-board-sqlite-worker--board-append-route (body)
  "Atomically append and route one canonical Board input from BODY."
  (setq body (e-board-sqlite-worker--resolve-board-input body))
  (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (source-key (plist-get body :source-input-key))
         (source-hash (plist-get body :source-hash))
         (source-key-sql
          (e-board-sqlite-worker--sql-value source-key))
         (existing
          (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT record_id,source_hash FROM board_records WHERE board_id=? AND generation=? AND source_kind='input' AND source_key=?"
                (vector board-id generation source-key-sql)))))
    (unless source-key
      (signal 'e-runtime-store-board-conflict
              (list "Board append requires a stable source identity" board-id)))
    (if existing
        (progn
          (unless (equal source-hash
                         (e-board-sqlite-worker--column existing 1))
            (signal 'e-runtime-store-board-conflict
                    (list "Board source key conflicts" board-id source-key)))
          (e-board-sqlite-worker--append-route-association
           body
           (e-board-sqlite-worker--canonical-append-route-result
            board-id generation
            (e-board-sqlite-worker--column existing 0) 'duplicate)))
      (let* ((position (1+ (e-board-sqlite-worker--column row 3)))
             (message-id
              (e-board-sqlite-worker--canonical-message-id
               board-id generation 'input source-key))
             (tags (copy-tree (plist-get body :tags)))
             (attributes (copy-tree (plist-get body :attributes)))
             (to (plist-get body :to))
             (author (plist-get body :author))
             (mode (or (plist-get body :mode) 'inject))
             (content (plist-get body :content))
             (reference (copy-tree (plist-get body :reference)))
             (message
              (list :id message-id :board-id board-id :seq position
                    :record-kind 'input :kind 'input :author author
                    :requester-actor (copy-tree (plist-get body :requester-actor))
                    :tags tags :selector-tags tags
                    :attributes attributes :selector-attributes attributes
                    :to to :mode mode :content content :reference reference
                    :source-input-key (copy-tree source-key)
                    :created-at (or (plist-get body :created-at) (float-time))
                    :routing-state 'routed :durable-position position))
             (policies
              (e-board-sqlite-worker--routing-policies
               board-id generation to))
             participants pickups)
        (dolist (entry policies)
          (let* ((participant-id (plist-get entry :participant-id))
                 (policy (plist-get entry :policy))
                 (selector (plist-get policy :pickup-selector)))
            (when (if to
                      (equal participant-id to)
                    (e-board-sqlite-worker--selector-matches-p
                     selector 'input tags attributes to author nil))
              (push participant-id participants)
              (push
               (list :delivery-id (list board-id message-id participant-id)
                     :board-id board-id :participant-id participant-id
                     :message-id message-id
                     :subscription-ids
                     (list (plist-get (plist-get entry :participant)
                                      :subscription-id))
                     :event-seq-range (list position position)
                     :mode mode :requester-actor
                     (copy-tree (plist-get body :requester-actor))
                     :addressed-p (and to t)
                     :cause-metadata
                     (list :source-input-key (copy-tree source-key)
                           :routing-tags tags :input-attributes attributes)
                     :content content :reference reference)
               pickups))))
        (setq participants (nreverse participants)
              pickups (nreverse pickups))
        (let* ((state (if participants 'routed 'unrouted))
               (reason (and (null participants)
                            (if to 'target-unavailable
                              'no-matching-subscription)))
               (outcome
                (list :state state :reason reason
                      :participant-ids participants
                      :pickup-ids
                      (mapcar (lambda (pickup)
                                (copy-tree (plist-get pickup :delivery-id)))
                              pickups))))
          (e-board-sqlite-worker--board-record-put
           (list :op 'board-record-put :board-id board-id
                 :generation generation :record message
                 :source (list :kind 'input :key source-key
                               :hash source-hash)))
          (e-board-sqlite-worker--board-routing-put
           (list :op 'board-routing-put :board-id board-id
                 :generation generation :message-id message-id
                 :outcome outcome :pickups (vconcat pickups)))
          (e-board-sqlite-worker--append-route-association
           body
           (e-board-sqlite-worker--canonical-append-route-result
            board-id generation message-id 'posted)))))))

(defun e-board-sqlite-worker--board-record-append (body)
  "Append one non-routed canonical Board record from BODY."
  (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (kind (plist-get body :record-kind))
         (source-kind (plist-get body :source-kind))
         (source-key (plist-get body :source-key))
         (source-hash (plist-get body :source-hash))
         (existing
          (car
           (sqlite-select
            e-board-sqlite-worker--database
            "SELECT record_id,source_hash FROM board_records WHERE board_id=? AND generation=? AND source_kind=? AND source_key=?"
            (vector board-id generation (symbol-name source-kind)
                    (e-board-sqlite-worker--sql-value source-key))))))
    (unless (and (memq kind '(output activity fact)) source-kind source-key)
      (signal 'e-runtime-store-board-conflict
              (list "Board record append requires stable kind/source" body)))
    (if existing
        (progn
          (unless (equal source-hash
                         (e-board-sqlite-worker--column existing 1))
            (signal 'e-runtime-store-board-conflict
                    (list "Board record source conflicts" board-id source-key)))
          (let* ((message-id
                  (e-board-sqlite-worker--column existing 0))
                 (record-row
                  (car
                   (sqlite-select
                    e-board-sqlite-worker--database
                    "SELECT position,payload FROM board_records WHERE board_id=? AND generation=? AND record_id=?"
                    (vector board-id generation message-id)))))
            (list :board-id board-id :generation generation :status 'duplicate
                  :position
                  (e-board-sqlite-worker--column record-row 0)
                  :message
                  (e-board-sqlite-worker--value
                   (e-board-sqlite-worker--column record-row 1)))))
      (let* ((position (1+ (e-board-sqlite-worker--column row 3)))
             (message-id
              (e-board-sqlite-worker--canonical-message-id
               board-id generation kind source-key))
             (record
              (append
               (list :id message-id :board-id board-id :seq position
                     :record-kind kind :kind kind
                     :created-at (or (plist-get body :created-at) (float-time))
                     :durable-position position)
               (copy-tree (plist-get body :record-fields) t))))
        (let ((stored
               (e-board-sqlite-worker--board-record-put
                (list :op 'board-record-put :board-id board-id
                      :generation generation :record record
                      :source (list :kind source-kind :key source-key
                                    :hash source-hash)))))
          (list :board-id board-id :generation generation :status 'posted
                :revision (plist-get stored :revision)
                :position (plist-get stored :position) :message record))))))

(defun e-board-sqlite-worker--pickup-key (delivery-id)
  "Return the exact durable key for DELIVERY-ID."
  (e-board-sqlite-worker--sql-value delivery-id))

(defun e-board-sqlite-worker--pickup-event
    (delivery-key state &optional payload)
  "Append one immutable pickup STATE event for DELIVERY-KEY."
  (let* ((row (car (sqlite-select
                    e-board-sqlite-worker--database
                    "SELECT COALESCE(MAX(event_position),0) FROM board_pickup_events WHERE delivery_key=?"
                    (vector delivery-key))))
         (position (1+ (e-board-sqlite-worker--column row 0))))
    (sqlite-execute
     e-board-sqlite-worker--database
     "INSERT INTO board_pickup_events(delivery_key,event_position,state,payload,created_at) VALUES(?,?,?,?,?)"
     (vector delivery-key position (symbol-name state)
             (and payload (e-board-sqlite-worker--sql-value payload))
             (float-time)))
    position))

(defun e-board-sqlite-worker--board-routing-put (body)
  "Commit one final routing outcome and immutable pickup set from BODY."
  (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (revision (e-board-sqlite-worker--column row 2))
         (message-id (plist-get body :message-id))
         (outcome (plist-get body :outcome))
         (payload (e-board-sqlite-worker--sql-value outcome)))
    (when (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT 1 FROM board_routing WHERE board_id=? AND generation=? AND message_id=?"
                (vector board-id generation message-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Board routing outcome is final" board-id message-id)))
    (cl-incf revision)
    (sqlite-execute
     e-board-sqlite-worker--database
     "INSERT INTO board_routing(board_id,generation,message_id,outcome,payload,revision) VALUES(?,?,?,?,?,?)"
     (vector board-id generation message-id
             (symbol-name (plist-get outcome :state)) payload revision))
    (let (committed-pickups)
      (dolist (pickup (append (plist-get body :pickups) nil))
        (let* ((delivery-id (plist-get pickup :delivery-id))
               (delivery-key (e-board-sqlite-worker--pickup-key delivery-id))
               (participant-id (plist-get pickup :participant-id))
               (fifo-row
                (car (sqlite-select
                      e-board-sqlite-worker--database
                      "SELECT COALESCE(MAX(fifo_position),0) FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=?"
                      (vector board-id generation participant-id))))
               (fifo-position (1+ (e-board-sqlite-worker--column fifo-row 0)))
               (active
                (car (sqlite-select
                      e-board-sqlite-worker--database
                      "SELECT 1 FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=? AND state IN ('pending','ready','claimed','accepted','cancelling') LIMIT 1"
                      (vector board-id generation participant-id))))
               (state (if active 'pending 'ready))
               (stored (append pickup
                               (list :fifo-position fifo-position :state state
                                     :revision 1))))
          (sqlite-execute
           e-board-sqlite-worker--database
           "INSERT INTO board_pickups(delivery_key,board_id,generation,participant_id,fifo_position,message_id,state,revision,attempt,payload) VALUES(?,?,?,?,?,?,?,?,0,?)"
           (vector delivery-key board-id generation participant-id fifo-position
                   message-id (symbol-name state) 1
                   (e-board-sqlite-worker--sql-value stored)))
          (e-board-sqlite-worker--pickup-event delivery-key state)
          (push stored committed-pickups)))
      (sqlite-execute
       e-board-sqlite-worker--database
       "UPDATE boards SET revision=? WHERE board_id=?"
       (vector revision board-id))
      (list :board-id board-id :generation generation :revision revision
            :message-id message-id :outcome outcome
            :pickups (nreverse committed-pickups)))))

(defun e-board-sqlite-worker--pickup-row (board-id generation delivery-id)
  "Return DELIVERY-ID row for BOARD-ID GENERATION or signal."
  (or (car (sqlite-select
            e-board-sqlite-worker--database
            "SELECT delivery_key,participant_id,fifo_position,state,revision,attempt,payload FROM board_pickups WHERE delivery_key=? AND board_id=? AND generation=?"
            (vector (e-board-sqlite-worker--pickup-key delivery-id)
                    board-id generation)))
      (signal 'e-runtime-store-board-conflict
              (list "Unknown Board pickup" board-id delivery-id))))

(defun e-board-sqlite-worker--pickup-promote-next
    (board-id generation participant-id fifo-position)
  "Promote PARTICIPANT-ID's next FIFO pickup after FIFO-POSITION."
  (when-let* ((next
               (car (sqlite-select
                     e-board-sqlite-worker--database
                     "SELECT delivery_key,payload,revision FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=? AND fifo_position>? AND state='pending' ORDER BY fifo_position LIMIT 1"
                     (vector board-id generation participant-id fifo-position)))))
    (let* ((delivery-key (e-board-sqlite-worker--column next 0))
           (payload (e-board-sqlite-worker--value
                     (e-board-sqlite-worker--column next 1)))
           (revision (1+ (e-board-sqlite-worker--column next 2))))
      (setq payload (plist-put payload :state 'ready))
      (setq payload (plist-put payload :revision revision))
      (sqlite-execute
       e-board-sqlite-worker--database
       "UPDATE board_pickups SET state='ready',revision=?,payload=? WHERE delivery_key=?"
       (vector revision (e-board-sqlite-worker--sql-value payload) delivery-key))
      (e-board-sqlite-worker--pickup-event delivery-key 'ready)
      payload)))

(defun e-board-sqlite-worker--board-pickup-transition (body)
  "Commit one typed Board pickup transition from BODY."
  (let* ((board-row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column board-row 1))
         (board-revision (e-board-sqlite-worker--column board-row 2))
         (delivery-id (plist-get body :delivery-id))
         (row (e-board-sqlite-worker--pickup-row board-id generation delivery-id))
         (delivery-key (e-board-sqlite-worker--column row 0))
         (participant-id (e-board-sqlite-worker--column row 1))
         (fifo-position (e-board-sqlite-worker--column row 2))
         (state (intern (e-board-sqlite-worker--column row 3)))
         (revision (e-board-sqlite-worker--column row 4))
         (attempt (e-board-sqlite-worker--column row 5))
         (payload (e-board-sqlite-worker--value
                   (e-board-sqlite-worker--column row 6)))
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
     e-board-sqlite-worker--database
     "UPDATE board_pickups SET state=?,revision=?,attempt=?,payload=? WHERE delivery_key=?"
     (vector (symbol-name next-state) revision attempt
             (e-board-sqlite-worker--sql-value payload) delivery-key))
    (e-board-sqlite-worker--pickup-event delivery-key next-state data)
    (sqlite-execute
     e-board-sqlite-worker--database
     "UPDATE boards SET revision=? WHERE board_id=?"
     (vector board-revision board-id))
    (list :board-id board-id :generation generation :revision board-revision
          :pickup payload
          :next (and terminal-p
                     (e-board-sqlite-worker--pickup-promote-next
                      board-id generation participant-id fifo-position)))))

(defun e-board-sqlite-worker--board-participant-put (body)
  "Persist one logical Board participant projection from BODY."
  (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (revision (1+ (e-board-sqlite-worker--column row 2)))
         (participant (plist-get body :participant))
         (participant-id (plist-get participant :id)))
    (when (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT 1 FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
                (vector board-id generation participant-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Board participant conflicts" board-id participant-id)))
    (sqlite-execute
     e-board-sqlite-worker--database
     "INSERT INTO board_participants(board_id,generation,participant_id,payload,revision) VALUES(?,?,?,?,1)"
     (vector board-id generation participant-id
             (e-board-sqlite-worker--sql-value participant)))
    (sqlite-execute e-board-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector revision board-id))
    (list :board-id board-id :generation generation :revision revision
          :participant participant)))

(defun e-board-sqlite-worker--board-participant-delete (body)
  "Delete one unpublished participant projection from BODY."
  (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (revision (1+ (e-board-sqlite-worker--column row 2)))
         (participant-id (plist-get body :participant-id))
         (participant-row
          (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT payload FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
                (vector board-id generation participant-id))))
         (participant
          (and participant-row
               (e-board-sqlite-worker--value
                (e-board-sqlite-worker--column participant-row 0))))
         (subscription-id (plist-get participant :subscription-id)))
    (unless participant-row
      (signal 'e-runtime-store-board-conflict
              (list "Unknown Board participant" board-id participant-id)))
    (when (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT 1 FROM board_pickups WHERE board_id=? AND generation=? AND participant_id=? LIMIT 1"
                (vector board-id generation participant-id)))
      (signal 'e-runtime-store-board-conflict
              (list "Board participant has durable pickups"
                    board-id participant-id)))
    (sqlite-execute
     e-board-sqlite-worker--database
     "DELETE FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
     (vector board-id generation participant-id))
    (when subscription-id
      (sqlite-execute
       e-board-sqlite-worker--database
       "DELETE FROM board_replay_progress WHERE board_id=? AND generation=? AND subscription_id=?"
       (vector board-id generation subscription-id)))
    (sqlite-execute e-board-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector revision board-id))
    (list :board-id board-id :generation generation :revision revision
          :participant-id participant-id :deleted t)))

(defun e-board-sqlite-worker--board-participant-publish (body)
  "Publish one provisional durable participant from BODY."
  (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (revision (1+ (e-board-sqlite-worker--column row 2)))
         (participant-id (plist-get body :participant-id))
         (participant-row
          (car (sqlite-select
                e-board-sqlite-worker--database
                "SELECT payload,revision FROM board_participants WHERE board_id=? AND generation=? AND participant_id=?"
                (vector board-id generation participant-id))))
         (participant
          (and participant-row
               (e-board-sqlite-worker--value
                (e-board-sqlite-worker--column participant-row 0))))
         (participant-revision
          (and participant-row
               (1+ (e-board-sqlite-worker--column participant-row 1)))))
    (unless participant-row
      (signal 'e-runtime-store-board-conflict
              (list "Unknown Board participant" board-id participant-id)))
    (unless (plist-get participant :publication-pending)
      (signal 'e-runtime-store-board-conflict
              (list "Board participant is already published"
                    board-id participant-id)))
    (setq participant (plist-put participant :publication-pending nil))
    (sqlite-execute
     e-board-sqlite-worker--database
     "UPDATE board_participants SET payload=?,revision=? WHERE board_id=? AND generation=? AND participant_id=?"
     (vector (e-board-sqlite-worker--sql-value participant)
             participant-revision board-id generation participant-id))
    (sqlite-execute e-board-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector revision board-id))
    (list :board-id board-id :generation generation :revision revision
          :participant participant)))

(defun e-board-sqlite-worker--board-replay-progress-put (body)
  "Persist stable subscription replay progress from BODY."
  (let* ((row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column row 1))
         (revision (1+ (e-board-sqlite-worker--column row 2)))
         (subscription-id (plist-get body :subscription-id))
         (position (plist-get body :position)))
    (sqlite-execute
     e-board-sqlite-worker--database
     "INSERT INTO board_replay_progress(board_id,generation,subscription_id,position,revision) VALUES(?,?,?,?,1) ON CONFLICT(board_id,generation,subscription_id) DO UPDATE SET position=MAX(position,excluded.position),revision=board_replay_progress.revision+1"
     (vector board-id generation subscription-id position))
    (sqlite-execute e-board-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector revision board-id))
    (list :board-id board-id :generation generation :revision revision
          :subscription-id subscription-id :position position)))

(defun e-board-sqlite-worker--board-pickup-session-admit (body)
  "Atomically accept a claimed pickup and record its session association."
  (let* ((board-row (e-board-sqlite-worker--board-check body))
         (board-id (plist-get body :board-id))
         (generation (e-board-sqlite-worker--column board-row 1))
         (board-revision (e-board-sqlite-worker--column board-row 2))
         (delivery-id (plist-get body :delivery-id))
         (pickup-row
          (e-board-sqlite-worker--pickup-row board-id generation delivery-id))
         (delivery-key (e-board-sqlite-worker--column pickup-row 0))
         (state (intern (e-board-sqlite-worker--column pickup-row 3)))
         (pickup-revision (e-board-sqlite-worker--column pickup-row 4))
         (pickup-payload
          (e-board-sqlite-worker--value
           (e-board-sqlite-worker--column pickup-row 6)))
         (session-id (plist-get body :session-id))
         (session-revision
          (or (caar (sqlite-select
                     e-board-sqlite-worker--database
                     "SELECT MAX(position) FROM session_records WHERE session_id=?"
                     (vector session-id)))
              0))
         ;; This is the session boundary observed by the Board association,
         ;; not a new session-journal position.  Session messages use their
         ;; own application FIFO and update journal plus query row atomically.
         (session-position session-revision)
         (record (plist-get body :record))
         (record-payload (e-board-sqlite-worker--sql-value record))
         (lane (plist-get body :lane)))
    (unless (eq state 'claimed)
      (signal 'e-runtime-store-board-conflict
              (list "Pickup admission requires a claim" delivery-id state)))
    (when (> (string-bytes record-payload)
             e-board-sqlite-worker-session-record-byte-limit)
      (signal 'e-runtime-store-worker-error
              (list "Session record exceeds private limit"
                    (string-bytes record-payload)
                    e-board-sqlite-worker-session-record-byte-limit)))
    (sqlite-execute
     e-board-sqlite-worker--database
     "INSERT INTO board_session_admissions(delivery_key,session_id,session_position,lane,payload) VALUES(?,?,?,?,?)"
     (vector delivery-key session-id session-position (symbol-name lane)
             record-payload))
    (cl-incf pickup-revision)
    (cl-incf board-revision)
    (setq pickup-payload (plist-put pickup-payload :state 'accepted))
    (setq pickup-payload (plist-put pickup-payload :revision pickup-revision))
    (sqlite-execute
     e-board-sqlite-worker--database
     "UPDATE board_pickups SET state='accepted',revision=?,payload=? WHERE delivery_key=?"
     (vector pickup-revision
             (e-board-sqlite-worker--sql-value pickup-payload) delivery-key))
    (e-board-sqlite-worker--pickup-event
     delivery-key 'accepted (list :session-id session-id :lane lane))
    (sqlite-execute e-board-sqlite-worker--database
                    "UPDATE boards SET revision=? WHERE board_id=?"
                    (vector board-revision board-id))
    (list :board-id board-id :generation generation
          :board-revision board-revision :pickup-revision pickup-revision
          :delivery-id delivery-id :session-id session-id
          :session-revision session-position :lane lane :record record)))


(defun e-board-sqlite-worker-write (database body)
  "Execute one typed Board write BODY on transaction-scoped DATABASE."
  (let ((e-board-sqlite-worker--database database))
    (pcase (plist-get body :op)
      ('board-create (e-board-sqlite-worker--board-create body))
      ('board-clear (e-board-sqlite-worker--board-clear body))
      ('board-record-put (e-board-sqlite-worker--board-record-put body))
      ('board-append-route
       (e-board-sqlite-worker--board-append-route body))
      ('board-record-append
       (e-board-sqlite-worker--board-record-append body))
      ('board-routing-put (e-board-sqlite-worker--board-routing-put body))
      ('board-pickup-transition
       (e-board-sqlite-worker--board-pickup-transition body))
      ('board-participant-put
       (e-board-sqlite-worker--board-participant-put body))
      ('board-participant-delete
       (e-board-sqlite-worker--board-participant-delete body))
      ('board-participant-publish
       (e-board-sqlite-worker--board-participant-publish body))
      ('board-replay-progress-put
       (e-board-sqlite-worker--board-replay-progress-put body))
      ('board-pickup-session-admit
       (e-board-sqlite-worker--board-pickup-session-admit body))
      (_ (signal 'e-runtime-store-worker-error
                 (list "Unknown Board write operation"
                       (plist-get body :op)))))))

(defun e-board-sqlite-worker--activity-symbol (value)
  "Normalize an activity enum VALUE stored as a symbol or wire string."
  (if (stringp value) (intern value) value))

(defun e-board-sqlite-worker--activity-public-participant (participant)
  "Return PARTICIPANT without process-local execution identity or handles."
  (let ((copy (copy-tree participant t)))
    (dolist (key '(:subagent-id :work-handle :cancel :child-harness
                   :publication-target :publication-function))
      (setq copy (cl-loop for (entry value) on copy by #'cddr
                          unless (eq entry key)
                          append (list entry value))))
    copy))

(defun e-board-sqlite-worker--activity-lifecycle-source-p
    (source-key session-id status)
  "Return non-nil when SOURCE-KEY identifies a runner lifecycle for SESSION-ID.
The source identity is the durable discriminator published by the runner.  It
keeps unrelated same-session facts from being interpreted as lifecycle state
without making this Board-owned worker depend on the runner implementation."
  (and (listp source-key)
       (= (length source-key) 3)
       (eq (nth 0 source-key) 'subagent-lifecycle)
       (equal (nth 1 source-key) session-id)
       (equal (e-board-sqlite-worker--activity-symbol (nth 2 source-key))
              status)))

(defun e-board-sqlite-worker--activity-lifecycle-outcome
    (position record source-key)
  "Return one lifecycle OUTCOME from FACT RECORD, or nil.
Lifecycle facts are intentionally reduced to the consumer outcome shape.  The
process-local subagent display id, if present in the durable attributes, never
crosses this observation boundary."
  (let* ((attributes (plist-get record :attributes))
         (session-id (plist-get attributes :session-id))
         (status (e-board-sqlite-worker--activity-symbol
                  (plist-get attributes :status))))
    (when (and (stringp session-id) status
               (e-board-sqlite-worker--activity-lifecycle-source-p
                source-key session-id status))
      (append
       (list :source 'lifecycle :position position :session-id session-id
             :status status)
       (when (plist-member attributes :summary)
         (list :summary (plist-get attributes :summary)))
       (when (plist-member attributes :error)
         (list :error (plist-get attributes :error)))
       (when (plist-member attributes :result)
         (list :result (copy-tree (plist-get attributes :result) t)))
       (when (plist-member attributes :outputs)
         (list :outputs (copy-tree (plist-get attributes :outputs) t)))
       (when (plist-member attributes :finished-at)
         (list :finished-at (plist-get attributes :finished-at)))))))

(defun e-board-sqlite-worker--activity-report-outcome (position record)
  "Return one orchestration terminal OUTCOME from FACT RECORD, or nil."
  (let ((fact (e-board-orchestration-fact-from-record record)))
    (when (and fact (eq (plist-get fact :type) 'terminal-report))
      (let ((payload (plist-get fact :payload)))
        (append
         (list :source 'orchestration :position position
               :session-id (plist-get payload :participant-session-id)
               :run-id (plist-get payload :run-id)
               :task-key (plist-get payload :task-key)
               :attempt (plist-get payload :attempt)
               :status (plist-get payload :status))
         (when (plist-member payload :summary)
           (list :summary (plist-get payload :summary)))
         (when (plist-member payload :error)
           (list :error (plist-get payload :error)))
         (when (plist-member payload :result)
           (list :result (copy-tree (plist-get payload :result) t)))
         (when (plist-member payload :outputs)
           (list :outputs (copy-tree (plist-get payload :outputs) t))))))))

(defun e-board-sqlite-worker--activity-lifecycle-source-keys (session-ids)
  "Return exact durable lifecycle source keys for SESSION-IDS.

The runner's lifecycle source key is the Board-owned durable discriminator.
Selecting those keys directly keeps unrelated same-session facts out of the
bounded candidate set before any reduction or limit is applied."
  (cl-loop for session-id in session-ids
           append
           (mapcar (lambda (status)
                     (list 'subagent-lifecycle session-id status))
                   e-board-sqlite-activity-lifecycle-statuses)))

(defun e-board-sqlite-worker--activity-report-source-key (assignment)
  "Return the durable terminal-report source key for ASSIGNMENT.

Terminal reports use the stable assignment idempotency key as their Board
source identity.  This mapping is deliberately local to the Board SQL query
boundary; it does not require the orchestration action or a live runner."
  (let ((run-id (plist-get assignment :run-id))
        (task-key (plist-get assignment :task-key))
        (attempt (plist-get assignment :attempt)))
    (list (format "orchestration:%s:terminal-report" run-id)
          (format "terminal:%s:%s:%d" run-id task-key attempt)
          0)))

(defun e-board-sqlite-worker--activity-lifecycle-rows
    (board-id generation session-ids)
  "Return bounded lifecycle rows for exact selected SESSION-IDS.

The source-key set contains the durable lifecycle identity (session plus the
current status domain), so newer unrelated facts carrying the same session and
status attributes cannot consume the activity bound.  There is one set query
regardless of participant count; a no-candidate request still performs it for
fixed query-count behavior."
  (let ((source-keys
         (e-board-sqlite-worker--activity-lifecycle-source-keys session-ids)))
    (if (null source-keys)
        (sqlite-select e-board-sqlite-worker--database
                       "SELECT position,payload,source_key FROM board_records WHERE 1=0")
      (sqlite-select
       e-board-sqlite-worker--database
       (format
        (concat
         "SELECT position,payload,source_key FROM board_records"
         " WHERE board_id=? AND generation=?"
         " AND record_kind='fact' AND source_kind='fact'"
         " AND source_key IN (%s) ORDER BY position DESC LIMIT ?")
        (mapconcat (lambda (_source-key) "?") source-keys ","))
       (vconcat
        (list board-id generation)
        (mapcar #'e-board-sqlite-worker--sql-value source-keys)
        (list (min e-board-sqlite-activity-fact-row-limit
                   (length source-keys))))))))

(defun e-board-sqlite-worker--activity-report-rows
    (board-id generation assignments)
  "Return bounded terminal-report rows for exact ASSIGNMENTS.

Each assignment maps to one stable terminal-report source key.  Selecting the
exact run/task/attempt key before the bounded read prevents newer reports for
other assignments in the same run from displacing the selected report."
  (let ((source-keys
         (mapcar #'e-board-sqlite-worker--activity-report-source-key
                 assignments)))
    (if (null source-keys)
        (sqlite-select e-board-sqlite-worker--database
                       "SELECT position,payload,source_key FROM board_records WHERE 1=0")
      (sqlite-select
       e-board-sqlite-worker--database
       (format
        (concat
         "SELECT position,payload,source_key FROM board_records"
         " WHERE board_id=? AND generation=?"
         " AND record_kind='fact' AND source_kind='fact'"
         " AND source_key IN (%s) ORDER BY position DESC LIMIT ?")
        (mapconcat (lambda (_source-key) "?") source-keys ","))
       (vconcat
        (list board-id generation)
        (mapcar #'e-board-sqlite-worker--sql-value source-keys)
        (list (min e-board-sqlite-activity-fact-row-limit
                   (length source-keys))))))))

(defun e-board-sqlite-worker--activity-identities
    (participants by-participant by-principal)
  "Return relevant session and exact report identities for PARTICIPANTS."
  (let (session-ids assignments)
    (dolist (entry participants)
      (let* ((participant-id (plist-get entry :participant-id))
             (participant (plist-get entry :participant))
             (principal (plist-get participant :principal))
             (context (or (gethash participant-id by-participant)
                          (and principal
                               (gethash principal by-principal))))
             (metadata (plist-get context :metadata))
             (session-id (or (plist-get context :session-id)
                             (plist-get participant :session-id)))
             (run-id (plist-get metadata :board-run-id))
             (task-key (plist-get metadata :board-task-key))
             (attempt (plist-get metadata :board-attempt)))
        (when (stringp session-id)
          (push session-id session-ids))
        (when (and (stringp run-id) (stringp task-key) (integerp attempt))
          (push (list :run-id run-id :task-key task-key :attempt attempt
                      :session-id session-id)
                assignments))))
    (list (delete-dups session-ids) (delete-dups assignments))))

(defun e-board-sqlite-worker--activity-page-finalize (page byte-limit)
  "Return PAGE with exact final encoded :bytes under BYTE-LIMIT.
The byte count is part of the returned representation, so measuring before
inserting it is insufficient when the count changes its encoded width."
  (let ((candidate (plist-put (copy-tree page t) :bytes 0)))
    (catch 'settled
      (while t
        (let ((bytes
               (e-runtime-store-codec-measure-bounded candidate byte-limit)))
          (if (= bytes (plist-get candidate :bytes))
              (throw 'settled candidate)
            (setq candidate (plist-put candidate :bytes bytes))))))))

(defun e-board-sqlite-worker--activity-session-context
    (board-id participants)
  "Return bounded session context maps for BOARD-ID and PARTICIPANTS.
The worker reads the current query rows as one set.  It maps each durable
routing participant to its admitted session and metadata without issuing
participant-specific session reads."
  (let ((by-participant (make-hash-table :test 'equal))
        (by-principal (make-hash-table :test 'equal)))
    (let* ((session-ids
            (delete-dups
             (delq nil
                   (mapcar
                    (lambda (entry)
                      (let ((participant (plist-get entry :participant)))
                        (or (and (stringp (plist-get participant :session-id))
                                 (plist-get participant :session-id))
                            (plist-get entry :participant-id))))
                    participants))))
           (principals
            (delete-dups
             (delq nil
                   (mapcar
                    (lambda (entry)
                      (let ((principal
                             (plist-get (plist-get entry :participant)
                                        :principal)))
                        (and (stringp principal) principal)))
                    participants))))
           (clauses nil)
           (parameters (list board-id)))
      ;; The selected participant page supplies the exact session/principal
      ;; candidates.  This keeps the one context read bounded without
      ;; accidentally dropping a page row behind an unrelated session prefix.
      (when session-ids
        (setq clauses
              (append clauses
                      (list
                       (format "session_id IN (%s)"
                               (mapconcat (lambda (_id) "?")
                                          session-ids ",")))))
        (setq parameters (append parameters session-ids)))
      (when principals
        (setq clauses
              (append clauses
                      (list
                       (format "principal IN (%s)"
                               (mapconcat (lambda (_principal) "?")
                                          principals ",")))))
        (setq parameters (append parameters principals)))
      (dolist
          (row
           (if clauses
               (sqlite-select
                e-board-sqlite-worker--database
                (format
                 "SELECT session_id,name,metadata,principal,association_role,routing_policy FROM session_query_state WHERE board_id=? AND (%s) ORDER BY session_id LIMIT ?"
                 (mapconcat #'identity clauses " OR "))
                (vconcat
                 (append parameters
                         (list
                          (min e-board-sqlite-activity-session-row-limit
                               (max 1 (* 2 (length participants))))))))
             (sqlite-select
              e-board-sqlite-worker--database
              "SELECT session_id,name,metadata,principal,association_role,routing_policy FROM session_query_state WHERE 1=0")))
      (let* ((session-id (e-board-sqlite-worker--column row 0))
             (policy (e-board-sqlite-worker--value
                      (e-board-sqlite-worker--column row 5)))
             (entry (list :session-id session-id
                          :name (e-board-sqlite-worker--column row 1)
                          :metadata
                          (e-board-sqlite-worker--value
                           (e-board-sqlite-worker--column row 2))
                          :principal (e-board-sqlite-worker--column row 3)
                          :association-role
                          (e-board-sqlite-worker--column row 4)
                          :participant-id
                          (plist-get policy :participant-id))))
        (when-let ((participant-id (plist-get entry :participant-id)))
          (puthash participant-id entry by-participant))
        (when-let ((principal (plist-get entry :principal)))
          (puthash principal entry by-principal))))
    (list by-participant by-principal))))

(defun e-board-sqlite-worker--activity-page (body)
  "Return one bounded detached participant/activity page from BODY.
Participant, current session, and newest fact relations are selected as sets
inside this worker transaction.  The parent process receives only the reduced
page and never reconstructs a Board aggregate or performs follow-up reads."
  (let* ((board-id (plist-get body :board-id))
         (board-row (e-board-sqlite-worker--board-row board-id))
         (generation (e-board-sqlite-worker--column board-row 1))
         (revision (e-board-sqlite-worker--column board-row 2))
         (after (or (plist-get body :after) ""))
         (limit (plist-get body :limit))
         (byte-limit (plist-get body :byte-limit)))
    (unless (and (integerp limit) (> limit 0)
                 (<= limit e-board-sqlite-activity-page-count-limit))
      (signal 'e-runtime-store-worker-error
              (list "Board activity participant count is out of bounds"
                    limit e-board-sqlite-activity-page-count-limit)))
    (unless (and (integerp byte-limit) (> byte-limit 0)
                 (<= byte-limit e-board-sqlite-activity-page-byte-limit))
      (signal 'e-runtime-store-worker-error
              (list "Board activity page byte bound is out of bounds"
                    byte-limit e-board-sqlite-activity-page-byte-limit)))
    (let* ((rows
            (sqlite-select
             e-board-sqlite-worker--database
             "SELECT participant_id,payload FROM board_participants WHERE board_id=? AND generation=? AND participant_id>? ORDER BY participant_id LIMIT ?"
             (vector board-id generation after (1+ limit))))
           (more (> (length rows) limit))
           (selected-rows (if more (cl-subseq rows 0 limit) rows))
           (participant-values
            (mapcar
             (lambda (row)
               (list :participant-id
                     (e-board-sqlite-worker--column row 0)
                     :participant
                     (e-board-sqlite-worker--value
                      (e-board-sqlite-worker--column row 1))))
             selected-rows))
           (contexts
            (e-board-sqlite-worker--activity-session-context
             board-id participant-values))
           (by-participant (nth 0 contexts))
           (by-principal (nth 1 contexts))
           (identities
            (e-board-sqlite-worker--activity-identities
             participant-values by-participant by-principal))
           (session-ids (nth 0 identities))
           (report-assignments (nth 1 identities))
           (lifecycle-by-session (make-hash-table :test 'equal))
           (report-by-session-assignment (make-hash-table :test 'equal))
           (report-by-assignment (make-hash-table :test 'equal))
           (report-by-session (make-hash-table :test 'equal)))
      ;; Each candidate set is selected by the page's durable identities before
      ;; reduction.  Unrelated Board history therefore cannot displace a
      ;; relevant lifecycle or terminal report behind a global newest-row cap.
      (dolist
          (row
           (e-board-sqlite-worker--activity-lifecycle-rows
            board-id generation session-ids))
        (let* ((position (e-board-sqlite-worker--column row 0))
               (record (e-board-sqlite-worker--value
                        (e-board-sqlite-worker--column row 1)))
               (source-key (e-board-sqlite-worker--value
                            (e-board-sqlite-worker--column row 2)))
               (lifecycle
                (e-board-sqlite-worker--activity-lifecycle-outcome
                 position record source-key)))
          (when (and lifecycle
                     (not (gethash (plist-get lifecycle :session-id)
                                   lifecycle-by-session)))
            (puthash (plist-get lifecycle :session-id) lifecycle
                     lifecycle-by-session))))
      (dolist
          (row
           (e-board-sqlite-worker--activity-report-rows
            board-id generation report-assignments))
        (let* ((position (e-board-sqlite-worker--column row 0))
               (record (e-board-sqlite-worker--value
                        (e-board-sqlite-worker--column row 1)))
               (report
                (e-board-sqlite-worker--activity-report-outcome
                 position record)))
          (when report
            (let ((assignment
                   (list (plist-get report :run-id)
                         (plist-get report :task-key)
                         (plist-get report :attempt)))
                  (session-id (plist-get report :session-id)))
              (unless (gethash assignment report-by-assignment)
                (puthash assignment report report-by-assignment))
              (when session-id
                (unless (gethash (cons session-id assignment)
                                 report-by-session-assignment)
                  (puthash (cons session-id assignment) report
                           report-by-session-assignment))
                (unless (gethash session-id report-by-session)
                  (puthash session-id report report-by-session)))))))
      (let ((participant-rows nil))
        (dolist (entry participant-values)
          (let* ((participant-id (plist-get entry :participant-id))
                 (participant (plist-get entry :participant))
                 (principal (plist-get participant :principal))
                 (context (or (gethash participant-id by-participant)
                              (and principal
                                   (gethash principal by-principal))))
                 (metadata (plist-get context :metadata))
                 (session-id (or (plist-get context :session-id)
                                 (plist-get participant :session-id)))
                 (run-id (plist-get metadata :board-run-id))
                 (task-key (plist-get metadata :board-task-key))
                 (attempt (plist-get metadata :board-attempt))
                 (assignment (and run-id task-key (integerp attempt)
                                  (list run-id task-key attempt)))
                 (report
                  (or (and assignment
                           (if session-id
                               (or (gethash (cons session-id assignment)
                                            report-by-session-assignment)
                                   (let ((candidate
                                          (gethash assignment
                                                   report-by-assignment)))
                                     (and candidate
                                          (or (null (plist-get candidate
                                                              :session-id))
                                              (equal session-id
                                                     (plist-get candidate
                                                                :session-id)))
                                          candidate)))
                             (gethash assignment report-by-assignment)))
                      (and (null assignment) session-id
                           (gethash session-id report-by-session))))
                 (lifecycle (and session-id
                                 (gethash session-id lifecycle-by-session)))
                 (outcome (or report lifecycle)))
            (when (and report (null run-id))
              (setq run-id (plist-get report :run-id)
                    task-key (plist-get report :task-key)
                    attempt (plist-get report :attempt)))
            (push
             (append
              (list :participant-id participant-id
                    :session-id session-id
                    :name (or (plist-get participant :name)
                              (plist-get context :name)
                              session-id participant-id)
                    :principal principal
                    :role (plist-get participant :role)
                    :state (or (plist-get participant :state) 'active)
                    :participant
                    (e-board-sqlite-worker--activity-public-participant
                     participant))
              (when run-id (list :run-id run-id))
              (when task-key (list :task-key task-key))
              (when (integerp attempt) (list :attempt attempt))
              (when outcome
                (list :outcome
                      (append
                       (list :source (plist-get outcome :source)
                             :status (plist-get outcome :status))
                       (when (plist-member outcome :summary)
                         (list :summary (plist-get outcome :summary)))
                       (when (plist-member outcome :error)
                         (list :error (plist-get outcome :error)))
                       (when (plist-member outcome :result)
                         (list :result
                               (copy-tree (plist-get outcome :result) t)))
                       (when (plist-member outcome :outputs)
                         (list :outputs
                               (copy-tree (plist-get outcome :outputs) t)))
                       (when (plist-member outcome :finished-at)
                         (list :finished-at
                               (plist-get outcome :finished-at)))))))
             participant-rows)))
        (setq participant-rows (nreverse participant-rows))
        (let ((kept participant-rows)
              page)
          ;; Build the largest prefix that satisfies the exact transport codec
          ;; bound.  If a single detached row cannot fit, fail this request
          ;; locally instead of returning an unbounded or silently empty page.
          (catch 'page
            (while t
              (let* ((last (car (last kept)))
                     (last-id (plist-get last :participant-id))
                     (truncated-prefix-p
                      (or more
                          (< (length kept) (length participant-rows)))))
                (setq page
                      (list :board-id board-id
                            :generation generation
                            :revision revision
                            :after after
                            :participants kept
                            :next (and truncated-prefix-p last-id)
                            :cursor (or last-id after))))
              (condition-case _error
                  (throw 'page
                         (e-board-sqlite-worker--activity-page-finalize
                          page byte-limit))
                (e-runtime-store-codec-too-large
                 (if (cdr kept)
                     (setq kept (butlast kept))
                   (signal 'e-runtime-store-worker-error
                           (list
                            "One Board activity participant exceeds page byte bound"
                            board-id byte-limit))))))))))))

(defun e-board-sqlite-worker-read (database body)
  "Execute one typed Board read BODY on DATABASE."
  (let ((e-board-sqlite-worker--database database))
    (pcase (plist-get body :op)
    ('board-get
     (when-let* ((row (car (sqlite-select
                            e-board-sqlite-worker--database
                            "SELECT board_id,trusted_principal,generation,revision,next_position,root_payload FROM boards WHERE board_id=?"
                            (vector (plist-get body :board-id))))))
       (list :board-id (e-board-sqlite-worker--column row 0)
             :trusted-principal
             (e-board-sqlite-worker--value
              (e-board-sqlite-worker--column row 1))
             :generation (e-board-sqlite-worker--column row 2)
             :revision (e-board-sqlite-worker--column row 3)
             :next-position (e-board-sqlite-worker--column row 4)
             :root (e-board-sqlite-worker--value
                    (e-board-sqlite-worker--column row 5)))))
    ('board-list
     (let* ((limit (min 1024 (max 1 (or (plist-get body :limit) 64))))
            (after (or (plist-get body :after) ""))
            (rows (sqlite-select
                   e-board-sqlite-worker--database
                   "SELECT board_id,trusted_principal,generation,revision,next_position,root_payload FROM boards WHERE board_id>? ORDER BY board_id LIMIT ?"
                   (vector after (1+ limit))))
            (more (> (length rows) limit))
            (selected (if more (butlast rows) rows)))
       (list
        :boards
        (mapcar
         (lambda (row)
           (list :board-id (e-board-sqlite-worker--column row 0)
                 :trusted-principal
                 (e-board-sqlite-worker--value
                  (e-board-sqlite-worker--column row 1))
                 :generation (e-board-sqlite-worker--column row 2)
                 :revision (e-board-sqlite-worker--column row 3)
                 :next-position (e-board-sqlite-worker--column row 4)
                 :root (e-board-sqlite-worker--value
                        (e-board-sqlite-worker--column row 5))))
         selected)
        :next (and more (e-board-sqlite-worker--column (car (last selected)) 0)))))
    ('board-record-page
     (let* ((board-id (plist-get body :board-id))
            (board-row
             (e-board-sqlite-worker--board-row board-id))
            (generation (or (plist-get body :generation)
                            (e-board-sqlite-worker--column
                             board-row 1)))
            (after (or (plist-get body :after) 0))
            (through
             (or
              (plist-get body :through)
              (if (= generation
                     (e-board-sqlite-worker--column board-row 1))
                  (e-board-sqlite-worker--column board-row 3)
                (e-board-sqlite-worker--column
                 (car
                  (sqlite-select
                   e-board-sqlite-worker--database
                   "SELECT COALESCE(MAX(position),0) FROM board_records WHERE board_id=? AND generation=?"
                   (vector board-id generation)))
                 0))))
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
       (unless (integerp through)
         (signal 'e-runtime-store-board-conflict
                 (list "Unknown Board page boundary" board-id generation)))
       (push "r.position<=?" clauses)
       (setq parameters (append parameters (list through)))
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
                       (list (e-board-sqlite-worker--sql-value tag)))))
       (dolist (attribute attributes)
         (push "EXISTS (SELECT 1 FROM board_record_attributes a WHERE a.board_id=r.board_id AND a.generation=r.generation AND a.position=r.position AND a.attribute_key=? AND a.attribute_value=?)"
               clauses)
         (setq parameters
               (append parameters
                       (list
                        (e-board-sqlite-worker--sql-value (car attribute))
                        (e-board-sqlite-worker--sql-value (cdr attribute))))))
       (let* ((sql
               (concat
                "SELECT r.position,r.payload,LENGTH(r.payload),r.source_kind,r.source_key,r.source_hash FROM board_records r WHERE "
                (mapconcat #'identity (nreverse clauses) " AND ")
                " ORDER BY r.position LIMIT ?"))
              (rows (sqlite-select
                     e-board-sqlite-worker--database sql
                     (vconcat (append parameters (list limit)))))
              (bytes 0) selected truncated)
         (catch 'full
           (dolist (row rows)
             (let ((row-bytes (e-board-sqlite-worker--column row 2)))
               (when (and selected
                          (> (+ bytes row-bytes)
                             e-board-sqlite-worker-page-byte-limit))
                 (setq truncated t)
                 (throw 'full nil))
               (cl-incf bytes row-bytes)
               (push
                (list :position (e-board-sqlite-worker--column row 0)
                      :record
                      (e-board-sqlite-worker--value
                       (e-board-sqlite-worker--column row 1))
                      :source
                      (and (e-board-sqlite-worker--column row 3)
                           (list
                            :kind (intern (e-board-sqlite-worker--column row 3))
                            :key
                            (e-board-sqlite-worker--value
                             (e-board-sqlite-worker--column row 4))
                            :hash (e-board-sqlite-worker--column row 5))))
                selected))))
         (setq selected (nreverse selected))
         (list :records selected :bytes bytes :through through
               :cursor (or (and selected
                                (plist-get (car (last selected)) :position))
                           after)
               :next (and selected
                          (or truncated (= (length rows) limit))
                          (plist-get (car (last selected)) :position))))))
    ('board-visible-window
     (let* ((board-id (plist-get body :board-id))
            (row (e-board-sqlite-worker--board-row board-id))
            (generation (or (plist-get body :generation)
                            (e-board-sqlite-worker--column row 1)))
            (through (e-board-sqlite-worker--column row 3))
            (limit (min 1024 (max 1 (or (plist-get body :limit) 64))))
            (rows
             (sqlite-select
              e-board-sqlite-worker--database
              "SELECT position,payload,LENGTH(payload),source_kind,source_key,source_hash FROM board_records WHERE board_id=? AND generation=? AND position<=? AND (record_kind IN ('input','output') OR (record_kind='activity' AND source_kind='context-curation')) ORDER BY position DESC LIMIT ?"
              (vector board-id generation through limit)))
            (bytes 0) selected truncated)
       ;; Prefer the newest complete presentation rows when the byte bound is
       ;; tighter than the count bound, then restore canonical ascending order.
       (catch 'full
         (dolist (current rows)
           (let ((row-bytes
                  (e-board-sqlite-worker--column current 2)))
             (when (and selected
                        (> (+ bytes row-bytes)
                           e-board-sqlite-worker-page-byte-limit))
               (setq truncated t)
               (throw 'full nil))
             (cl-incf bytes row-bytes)
             (push
              (list :position
                    (e-board-sqlite-worker--column current 0)
                    :record
                    (e-board-sqlite-worker--value
                     (e-board-sqlite-worker--column current 1))
                    :source
                    (and (e-board-sqlite-worker--column current 3)
                         (list
                          :kind
                          (intern (e-board-sqlite-worker--column
                                   current 3))
                          :key
                          (e-board-sqlite-worker--value
                           (e-board-sqlite-worker--column current 4))
                          :hash
                          (e-board-sqlite-worker--column current 5))))
              selected))))
       (list :records selected :bytes bytes :generation generation
             :through through :cursor through
             :truncated (and truncated t))))
    ('board-orchestration-run
     (let* ((board-id (plist-get body :board-id))
            (run-id (plist-get body :run-id))
            (limit (min 1024 (max 1 (or (plist-get body :limit) 256))))
            (rows
             (sqlite-select
              e-board-sqlite-worker--database
              (concat
               "SELECT r.position,r.payload FROM board_records r "
               "JOIN board_record_attributes a ON a.board_id=r.board_id "
               "AND a.generation=r.generation AND a.position=r.position "
               "WHERE r.board_id=? AND r.generation=(SELECT generation FROM boards WHERE board_id=?) "
               "AND r.record_kind='fact' AND a.attribute_key=? AND a.attribute_value=? "
               "ORDER BY r.position LIMIT ?")
              (vector board-id board-id
                      (e-board-sqlite-worker--sql-value
                       :orchestration-run-id)
                      (e-board-sqlite-worker--sql-value run-id)
                      (1+ limit))))
            (truncated (> (length rows) limit)))
       (list :run-id run-id
             :records
             (mapcar
              (lambda (row)
                (list :position
                      (e-board-sqlite-worker--column row 0)
                      :record
                      (e-board-sqlite-worker--value
                       (e-board-sqlite-worker--column row 1))))
              (if truncated (cl-subseq rows 0 limit) rows))
             :truncated truncated)))
    ('board-orchestration-runs
     (let* ((board-id (plist-get body :board-id))
            (run-limit (min 32 (max 1 (or (plist-get body :limit) 32))))
            (row-limit 4096)
            (rows
             (sqlite-select
              e-board-sqlite-worker--database
              (concat
               "SELECT r.position,r.payload FROM board_records r "
               "WHERE r.board_id=? AND r.generation=(SELECT generation FROM boards WHERE board_id=?) "
               "AND r.record_kind='fact' AND EXISTS "
               "(SELECT 1 FROM board_record_tags t WHERE t.board_id=r.board_id "
               "AND t.generation=r.generation AND t.position=r.position AND t.tag=?) "
               "ORDER BY r.position DESC LIMIT ?")
              (vector board-id board-id
                      (e-board-sqlite-worker--sql-value 'orchestration)
                      row-limit)))
            (selected-run-ids (make-hash-table :test 'equal))
            selected (run-count 0))
       ;; Scan newest-first until the requested number of manifest boundaries
       ;; is complete, then restore canonical order for the pure reducer.
       (catch 'complete
         (dolist (row rows)
           (let* ((record
                   (e-board-sqlite-worker--value
                    (e-board-sqlite-worker--column row 1)))
                  (attributes (plist-get record :attributes))
                  (run-id (plist-get attributes :orchestration-run-id))
                  (type (plist-get attributes :orchestration-type)))
             (when (or (gethash run-id selected-run-ids)
                       (< run-count run-limit))
               (puthash run-id t selected-run-ids)
               (push (list :position
                           (e-board-sqlite-worker--column row 0)
                           :record record)
                     selected))
             (when (equal type "manifest")
               (cl-incf run-count)
               (when (>= run-count run-limit)
                 (throw 'complete nil))))))
       (list :records selected
             :run-count run-count
             :truncated (and (= (length rows) row-limit)
                             (< run-count run-limit)))))
    ('board-activity-page
     (e-board-sqlite-worker--activity-page body))
    ('board-routing-get
     (when-let* ((row (car (sqlite-select
                            e-board-sqlite-worker--database
                            "SELECT payload,revision FROM board_routing WHERE board_id=? AND generation=? AND message_id=?"
                            (vector (plist-get body :board-id)
                                    (plist-get body :generation)
                                    (plist-get body :message-id))))))
       (list :outcome
             (e-board-sqlite-worker--value
              (e-board-sqlite-worker--column row 0))
             :revision (e-board-sqlite-worker--column row 1))))
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
          (let ((payload (e-board-sqlite-worker--value
                          (e-board-sqlite-worker--column row 0))))
            (setq payload
                  (plist-put payload :state
                             (intern (e-board-sqlite-worker--column row 1))))
            (setq payload
                  (plist-put payload :revision
                             (e-board-sqlite-worker--column row 2)))
            (plist-put payload :attempt
                       (e-board-sqlite-worker--column row 3))))
        (sqlite-select e-board-sqlite-worker--database sql
                       (vconcat parameters)))))
    ('board-participant-list
     (mapcar
      (lambda (row)
        (e-board-sqlite-worker--value
         (e-board-sqlite-worker--column row 0)))
      (sqlite-select
       e-board-sqlite-worker--database
       "SELECT payload FROM board_participants WHERE board_id=? AND generation=? ORDER BY participant_id LIMIT ?"
       (vector (plist-get body :board-id) (plist-get body :generation)
               (min 4096 (max 1 (or (plist-get body :limit) 512)))))))
    ('board-replay-progress-get
     (when-let* ((row (car (sqlite-select
                            e-board-sqlite-worker--database
                            "SELECT position,revision FROM board_replay_progress WHERE board_id=? AND generation=? AND subscription_id=?"
                            (vector (plist-get body :board-id)
                                    (plist-get body :generation)
                                    (plist-get body :subscription-id))))))
       (list :position (e-board-sqlite-worker--column row 0)
             :revision (e-board-sqlite-worker--column row 1))))
      (_ (signal 'e-runtime-store-worker-error
                 (list "Unknown Board read operation"
                       (plist-get body :op)))))))

(provide 'e-board-sqlite-worker)

;;; e-board-sqlite-worker.el ends here

;;; e-task-storage-sqlite-worker.el --- Worker-side task SQL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns only the private task schema and typed task command handlers.  The
;; generic worker supplies the transaction, dispatch, and codec.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'subr-x)
(require 'e-runtime-store-codec)

(defvar e-task-storage-sqlite-worker--database nil)

(defun e-task-storage-sqlite-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-task-storage-sqlite-worker--pack (value)
  "Encode VALUE for a task payload column."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-task-storage-sqlite-worker--unpack (value)
  "Decode task payload VALUE."
  (e-runtime-store-codec-decode (base64-decode-string value)))

(defun e-task-storage-sqlite-worker--without-keys (value keys)
  "Return detached VALUE without relational task authority KEYS."
  (let ((tail (copy-tree value t)) result)
    (while tail
      (let ((key (pop tail)) (entry-value (pop tail)))
        (unless (memq key keys)
          (setq result (append result (list key entry-value))))))
    result))

(defun e-task-storage-sqlite-worker--task-content (record)
  "Return opaque task content without queue or execution fields."
  (e-task-storage-sqlite-worker--without-keys
   record '(:queue-id :task-id :position :status :revision :enqueued-at
            :started-at :finished-at :settled-at :harness-instance-id
            :harness-selector :session-id :retries :retry-count
            :latest-attempt-id :attempt-id :attempt-number :outputs :error)))

(defun e-task-storage-sqlite-worker--attempt-dto (row)
  "Reconstruct one attempt DTO from normalized ROW."
  (list :task-id (e-task-storage-sqlite-worker--column row 1)
        :attempt-id (e-task-storage-sqlite-worker--column row 2)
        :attempt-number (e-task-storage-sqlite-worker--column row 3)
        :state (intern (e-task-storage-sqlite-worker--column row 4))
        :harness-instance-id (e-task-storage-sqlite-worker--column row 5)
        :session-id (e-task-storage-sqlite-worker--column row 6)
        :started-at (e-task-storage-sqlite-worker--column row 7)
        :finished-at (e-task-storage-sqlite-worker--column row 8)
        :outputs (and (e-task-storage-sqlite-worker--column row 9)
                      (e-task-storage-sqlite-worker--unpack
                       (e-task-storage-sqlite-worker--column row 9)))
        :error (and (e-task-storage-sqlite-worker--column row 10)
                    (e-task-storage-sqlite-worker--unpack
                     (e-task-storage-sqlite-worker--column row 10)))))

(defun e-task-storage-sqlite-worker--task-dto (row attempt-row)
  "Reconstruct one public task DTO from task ROW and latest ATTEMPT-ROW."
  (let* ((content (e-task-storage-sqlite-worker--unpack
                   (e-task-storage-sqlite-worker--column row 9)))
         (attempt (and attempt-row
                       (e-task-storage-sqlite-worker--attempt-dto attempt-row))))
    (append
     (list :task-id (e-task-storage-sqlite-worker--column row 1)
           :status (intern (e-task-storage-sqlite-worker--column row 3))
           :revision (e-task-storage-sqlite-worker--column row 4)
           :enqueued-at (e-task-storage-sqlite-worker--column row 5)
           :harness-instance-id
           (if attempt
               (plist-get attempt :harness-instance-id)
             (and (e-task-storage-sqlite-worker--column row 6)
                  (e-task-storage-sqlite-worker--unpack
                   (e-task-storage-sqlite-worker--column row 6))))
           :retries (e-task-storage-sqlite-worker--column row 7)
           :attempt-id (e-task-storage-sqlite-worker--column row 8)
           :attempt-number (and attempt (plist-get attempt :attempt-number))
           :started-at (and attempt (plist-get attempt :started-at))
           :finished-at (and attempt (plist-get attempt :finished-at))
           :session-id (and attempt (plist-get attempt :session-id))
           :outputs (and attempt (plist-get attempt :outputs))
           :error (and attempt (plist-get attempt :error)))
     content)))

(defun e-task-storage-sqlite-worker-initialize-v7 (database)
  "Create the historical schema-v7 task relations on DATABASE."
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS task_queues (queue_id TEXT PRIMARY KEY, revision INTEGER NOT NULL, sequence INTEGER NOT NULL, paused INTEGER NOT NULL)"
         "CREATE TABLE IF NOT EXISTS task_records (queue_id TEXT NOT NULL, task_id TEXT NOT NULL, position INTEGER NOT NULL, status TEXT NOT NULL, revision INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(queue_id,task_id), UNIQUE(queue_id,position), FOREIGN KEY(queue_id) REFERENCES task_queues(queue_id) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS task_records_dispatch ON task_records(queue_id,status,position)"
         "CREATE TABLE IF NOT EXISTS task_attempts (queue_id TEXT NOT NULL, task_id TEXT NOT NULL, attempt_id TEXT NOT NULL, attempt_number INTEGER NOT NULL, state TEXT NOT NULL, started_at TEXT, settled_at TEXT, payload TEXT NOT NULL, PRIMARY KEY(queue_id,attempt_id), FOREIGN KEY(queue_id,task_id) REFERENCES task_records(queue_id,task_id) ON DELETE CASCADE)"))
    (sqlite-execute database statement)))

(defun e-task-storage-sqlite-worker-initialize (database)
  "Initialize current normalized task relations in DATABASE."
  (setq e-task-storage-sqlite-worker--database database)
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS task_queues (queue_id TEXT PRIMARY KEY, revision INTEGER NOT NULL, sequence INTEGER NOT NULL, paused INTEGER NOT NULL)"
         "CREATE TABLE IF NOT EXISTS task_records (queue_id TEXT NOT NULL, task_id TEXT NOT NULL, position INTEGER NOT NULL, status TEXT NOT NULL, revision INTEGER NOT NULL, enqueued_at TEXT, harness_selector TEXT, retry_count INTEGER NOT NULL DEFAULT 0, latest_attempt_id TEXT, payload TEXT NOT NULL, PRIMARY KEY(queue_id,task_id), UNIQUE(queue_id,position), FOREIGN KEY(queue_id) REFERENCES task_queues(queue_id) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS task_records_dispatch ON task_records(queue_id,status,position)"
         "CREATE TABLE IF NOT EXISTS task_attempts (queue_id TEXT NOT NULL, task_id TEXT NOT NULL, attempt_id TEXT NOT NULL, attempt_number INTEGER NOT NULL, state TEXT NOT NULL, harness_instance_id TEXT, session_id TEXT, started_at TEXT, settled_at TEXT, outputs TEXT, error TEXT, PRIMARY KEY(queue_id,attempt_id), FOREIGN KEY(queue_id,task_id) REFERENCES task_records(queue_id,task_id) ON DELETE CASCADE)"))
    (sqlite-execute database statement)))

(defun e-task-storage-sqlite-worker--queue (queue-id)
  "Return QUEUE-ID row or signal."
  (or (car (sqlite-select
            e-task-storage-sqlite-worker--database
            "SELECT revision,sequence,paused FROM task_queues WHERE queue_id=?"
            (vector queue-id)))
      (signal 'e-runtime-store-task-conflict
              (list "Unknown task queue" queue-id))))

(defun e-task-storage-sqlite-worker--set-root
    (queue-id revision sequence paused-p)
  "Update QUEUE-ID root fields."
  (sqlite-execute
   e-task-storage-sqlite-worker--database
   "UPDATE task_queues SET revision=?,sequence=?,paused=? WHERE queue_id=?"
   (vector revision sequence (if paused-p 1 0) queue-id)))

(defun e-task-storage-sqlite-worker--open (body)
  "Open or create a task queue from BODY."
  (let ((queue-id (plist-get body :queue-id)))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "INSERT OR IGNORE INTO task_queues(queue_id,revision,sequence,paused) VALUES(?,0,0,0)"
     (vector queue-id))
    (let ((row (e-task-storage-sqlite-worker--queue queue-id)))
      (list :queue-id queue-id
            :revision (e-task-storage-sqlite-worker--column row 0)
            :sequence (e-task-storage-sqlite-worker--column row 1)
            :paused-p (= (e-task-storage-sqlite-worker--column row 2) 1)))))

(defun e-task-storage-sqlite-worker--enqueue (body)
  "Append one queued task from BODY."
  (let* ((queue-id (plist-get body :queue-id))
         (_root (e-task-storage-sqlite-worker--open body))
         (row (e-task-storage-sqlite-worker--queue queue-id))
         (record (plist-get body :record))
         (task-id (plist-get record :task-id))
         ;; SQLite owns durable ordering.  A caller may optimistically name a
         ;; task, but it never reserves or reconstructs the queue sequence.
         (position (1+ (e-task-storage-sqlite-worker--column row 1)))
         (revision (1+ (e-task-storage-sqlite-worker--column row 0))))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "INSERT INTO task_records(queue_id,task_id,position,status,revision,enqueued_at,harness_selector,retry_count,latest_attempt_id,payload) VALUES(?,?,?,?,1,?,?,?,?,?)"
     (vector queue-id task-id position "queued"
             (plist-get record :enqueued-at)
             (e-task-storage-sqlite-worker--pack
              (plist-get record :harness-instance-id))
             (or (plist-get record :retries) 0) nil
             (e-task-storage-sqlite-worker--pack
              (e-task-storage-sqlite-worker--task-content record))))
    (e-task-storage-sqlite-worker--set-root
     queue-id revision (max position
                            (e-task-storage-sqlite-worker--column row 1))
     (= (e-task-storage-sqlite-worker--column row 2) 1))
    (list :queue-id queue-id :task-id task-id :revision revision
          :sequence position :task-revision 1
          :record (e-task-storage-sqlite-worker--task-dto
                   (vector queue-id task-id position "queued" 1
                           (plist-get record :enqueued-at)
                           (e-task-storage-sqlite-worker--pack
                            (plist-get record :harness-instance-id))
                           (or (plist-get record :retries) 0) nil
                           (e-task-storage-sqlite-worker--pack
                            (e-task-storage-sqlite-worker--task-content record)))
                   nil))))

(defun e-task-storage-sqlite-worker--task-row (queue-id task-id)
  "Return QUEUE-ID TASK-ID row or signal."
  (or (car (sqlite-select
            e-task-storage-sqlite-worker--database
            "SELECT queue_id,task_id,position,status,revision,enqueued_at,harness_selector,retry_count,latest_attempt_id,payload FROM task_records WHERE queue_id=? AND task_id=?"
            (vector queue-id task-id)))
      (signal 'e-runtime-store-task-conflict
              (list "Unknown task" queue-id task-id))))

(defun e-task-storage-sqlite-worker--attempt-row (queue-id attempt-id)
  "Return one normalized attempt row or nil for ATTEMPT-ID."
  (when attempt-id
    (car (sqlite-select
          e-task-storage-sqlite-worker--database
          "SELECT queue_id,task_id,attempt_id,attempt_number,state,harness_instance_id,session_id,started_at,settled_at,outputs,error FROM task_attempts WHERE queue_id=? AND attempt_id=?"
          (vector queue-id attempt-id)))))

(defun e-task-storage-sqlite-worker--claim (body)
  "Claim one task before runner invocation."
  (let* ((queue-id (plist-get body :queue-id))
         (queue-row (e-task-storage-sqlite-worker--queue queue-id))
         (task-id (plist-get body :task-id))
         (attempt-id (plist-get body :attempt-id))
         (task-row (e-task-storage-sqlite-worker--task-row queue-id task-id))
         (status (intern (e-task-storage-sqlite-worker--column task-row 3)))
         (record (e-task-storage-sqlite-worker--task-dto
                  task-row
                  (e-task-storage-sqlite-worker--attempt-row
                   queue-id (e-task-storage-sqlite-worker--column task-row 8))))
         (revision (1+ (e-task-storage-sqlite-worker--column queue-row 0)))
         (task-revision (1+ (e-task-storage-sqlite-worker--column task-row 4)))
         (number (1+ (or (plist-get record :attempt-number) 0))))
    (unless (eq status 'queued)
      (signal 'e-runtime-store-task-conflict
              (list "Task is not queued" task-id status)))
    (setq record (plist-put record :status 'running)
          record (plist-put record :started-at (plist-get body :started-at))
          record (plist-put record :attempt-id attempt-id)
          record (plist-put record :attempt-number number)
          record (plist-put record :harness-instance-id
                            (plist-get body :harness-instance-id)))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "INSERT INTO task_attempts(queue_id,task_id,attempt_id,attempt_number,state,harness_instance_id,session_id,started_at,settled_at,outputs,error) VALUES(?,?,?,?,?,?,?,?,?,?,?)"
     (vector queue-id task-id attempt-id number "claimed"
             (plist-get body :harness-instance-id) nil
             (plist-get body :started-at) nil nil nil))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "UPDATE task_records SET status='running',revision=?,harness_selector=?,latest_attempt_id=?,payload=? WHERE queue_id=? AND task_id=?"
     (vector task-revision
             (e-task-storage-sqlite-worker--column task-row 6)
             attempt-id
             (e-task-storage-sqlite-worker--column task-row 9)
             queue-id task-id))
    (e-task-storage-sqlite-worker--set-root
     queue-id revision (e-task-storage-sqlite-worker--column queue-row 1)
     (= (e-task-storage-sqlite-worker--column queue-row 2) 1))
    (list :queue-id queue-id :task-id task-id :attempt-id attempt-id
          :revision revision :task-revision task-revision :record record)))

(defun e-task-storage-sqlite-worker--claim-runnable (body)
  "Atomically claim the oldest runnable task described by BODY.

The generic runtime worker wraps this handler in one SQLite transaction, so
selection, state transition, and attempt creation have one commit boundary."
  (let* ((queue-id (plist-get body :queue-id))
         (_root (e-task-storage-sqlite-worker--open body))
         (queue-row (e-task-storage-sqlite-worker--queue queue-id)))
    (if (= (e-task-storage-sqlite-worker--column queue-row 2) 1)
        (list :queue-id queue-id :claimed-p nil :paused-p t)
      (if-let* ((row
                 (car
                  (sqlite-select
                   e-task-storage-sqlite-worker--database
                   (concat
                    "SELECT queue_id,task_id,position,status,revision,enqueued_at,harness_selector,retry_count,latest_attempt_id,payload FROM task_records "
                    "WHERE queue_id=? AND status='queued' "
                    "ORDER BY position LIMIT 1")
                   (vector queue-id)))))
          (let* ((task-id (e-task-storage-sqlite-worker--column row 1))
                 (task-revision
                  (1+ (e-task-storage-sqlite-worker--column row 4)))
                 (record
                  (e-task-storage-sqlite-worker--task-dto
                   row
                   (e-task-storage-sqlite-worker--attempt-row
                    queue-id (e-task-storage-sqlite-worker--column row 8))))
                 (number (1+ (or (plist-get record :attempt-number) 0)))
                 (attempt-id (format "%s:a:%d" task-id number))
                 (started-at (plist-get body :started-at))
                 (instance-id
                  (or (plist-get record :harness-instance-id)
                      (plist-get body :harness-instance-id)))
                 (revision
                  (1+ (e-task-storage-sqlite-worker--column queue-row 0))))
            (setq record (plist-put record :status 'running)
                  record (plist-put record :started-at started-at)
                  record (plist-put record :attempt-id attempt-id)
                  record (plist-put record :attempt-number number)
                  record (plist-put record :harness-instance-id instance-id))
            (sqlite-execute
             e-task-storage-sqlite-worker--database
             (concat
              "INSERT INTO task_attempts(queue_id,task_id,attempt_id,"
              "attempt_number,state,harness_instance_id,session_id,started_at,settled_at,outputs,error) VALUES(?,?,?,?,?,?,?,?,?,?,?)")
             (vector queue-id task-id attempt-id number "claimed"
                     instance-id nil started-at nil nil nil))
            (sqlite-execute
             e-task-storage-sqlite-worker--database
             (concat
              "UPDATE task_records SET status='running',revision=?,harness_selector=?,latest_attempt_id=?,payload=? "
              "WHERE queue_id=? AND task_id=? AND status='queued'")
             (vector task-revision
                     (e-task-storage-sqlite-worker--column row 6)
                     attempt-id
                     (e-task-storage-sqlite-worker--column row 9)
                     queue-id task-id))
            (e-task-storage-sqlite-worker--set-root
             queue-id revision
             (e-task-storage-sqlite-worker--column queue-row 1) nil)
            (list :queue-id queue-id :claimed-p t :task-id task-id
                  :attempt-id attempt-id :revision revision
                  :task-revision task-revision :record record))
        (list :queue-id queue-id :claimed-p nil :paused-p nil)))))

(defun e-task-storage-sqlite-worker--transition (body)
  "Commit one task transition from BODY."
  (let* ((queue-id (plist-get body :queue-id))
         (queue-row (e-task-storage-sqlite-worker--queue queue-id))
         (task-id (plist-get body :task-id))
         (task-row (e-task-storage-sqlite-worker--task-row queue-id task-id))
         (status (intern (e-task-storage-sqlite-worker--column task-row 3)))
         (expected-status (plist-get body :expected-status))
         (record (plist-get body :record))
         (next-status (plist-get record :status))
         (revision (1+ (e-task-storage-sqlite-worker--column queue-row 0)))
         (task-revision (1+ (e-task-storage-sqlite-worker--column task-row 4))))
    (unless (eq status expected-status)
      (signal 'e-runtime-store-task-conflict
              (list "Task status conflict" task-id expected-status status)))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "UPDATE task_records SET status=?,revision=?,retry_count=?,latest_attempt_id=?,payload=? WHERE queue_id=? AND task_id=?"
     (vector (symbol-name next-status) task-revision
             (or (plist-get record :retries)
                 (e-task-storage-sqlite-worker--column task-row 7))
             (plist-get record :attempt-id)
             (e-task-storage-sqlite-worker--pack
              (e-task-storage-sqlite-worker--task-content record))
             queue-id task-id))
    ;; Re-queueing normally preserves the completed or paused attempt as
    ;; history.  Auto-retry supplies an explicit prior-attempt transition;
    ;; ordinary resume supplies none and must not fabricate a failure.
    (let* ((attempt-id (plist-get record :attempt-id))
           (attempt-transition (plist-get body :attempt-transition))
           (attempt-state
            (or (plist-get attempt-transition :state)
                (and (not (eq next-status 'queued)) next-status))))
      (when (and attempt-id attempt-state)
        (sqlite-execute
         e-task-storage-sqlite-worker--database
         "UPDATE task_attempts SET state=?,harness_instance_id=?,session_id=?,settled_at=?,outputs=?,error=? WHERE queue_id=? AND attempt_id=?"
         (vector (symbol-name attempt-state)
                 (plist-get record :harness-instance-id)
                 (plist-get record :session-id)
                 (or (plist-get attempt-transition :settled-at)
                     (plist-get record :finished-at))
                 (and (plist-get record :outputs)
                      (e-task-storage-sqlite-worker--pack
                       (plist-get record :outputs)))
                 (and (plist-get record :error)
                      (e-task-storage-sqlite-worker--pack
                       (plist-get record :error)))
                 queue-id attempt-id))))
    (e-task-storage-sqlite-worker--set-root
     queue-id revision (e-task-storage-sqlite-worker--column queue-row 1)
     (= (e-task-storage-sqlite-worker--column queue-row 2) 1))
    (list :queue-id queue-id :task-id task-id :revision revision
          :task-revision task-revision
          :record
          (let* ((current-row
                  (e-task-storage-sqlite-worker--task-row queue-id task-id))
                 (current-attempt-id
                  (e-task-storage-sqlite-worker--column current-row 8)))
            (e-task-storage-sqlite-worker--task-dto
             current-row
             (e-task-storage-sqlite-worker--attempt-row
              queue-id current-attempt-id))))))

(defun e-task-storage-sqlite-worker--pause (body)
  "Commit queue pause gate from BODY."
  (let* ((queue-id (plist-get body :queue-id))
         (_root (e-task-storage-sqlite-worker--open body))
         (row (e-task-storage-sqlite-worker--queue queue-id))
         (revision (1+ (e-task-storage-sqlite-worker--column row 0)))
         (paused-p (and (plist-get body :paused-p) t)))
    (e-task-storage-sqlite-worker--set-root
     queue-id revision (e-task-storage-sqlite-worker--column row 1) paused-p)
    (list :queue-id queue-id :revision revision :paused-p paused-p)))

(defun e-task-storage-sqlite-worker--delete-history (body)
  "Delete explicit task history from BODY."
  (let* ((queue-id (plist-get body :queue-id))
         (row (e-task-storage-sqlite-worker--queue queue-id))
         (revision (1+ (e-task-storage-sqlite-worker--column row 0)))
         (sequence (e-task-storage-sqlite-worker--column row 1)))
    (sqlite-execute e-task-storage-sqlite-worker--database
                    "DELETE FROM task_records WHERE queue_id=?"
                    (vector queue-id))
    ;; Keep the monotonic task sequence across history deletion so task
    ;; identities never repeat within this durable queue.
    (e-task-storage-sqlite-worker--set-root queue-id revision sequence nil)
    (list :queue-id queue-id :revision revision :sequence sequence
          :deleted t)))

(defconst e-task-storage-sqlite-worker--import-statuses
  '(queued paused done failed cancelled interrupted)
  "Terminal and resumable task states accepted by offline migration.")

(defun e-task-storage-sqlite-worker--import-legacy-snapshot (body)
  "Import BODY's complete legacy snapshot into an empty task queue."
  (let* ((queue-id (plist-get body :queue-id))
         (snapshot (plist-get body :snapshot))
         (root (e-task-storage-sqlite-worker--queue queue-id))
         (records (or (plist-get snapshot :records) nil))
         (order (or (plist-get snapshot :order)
                    (mapcar (lambda (record) (plist-get record :task-id))
                            records)))
         (by-id (make-hash-table :test 'equal))
         (sequence (or (plist-get snapshot :sequence) (length records)))
         (position 0))
    (unless (and (= (e-task-storage-sqlite-worker--column root 0) 0)
                 (null (sqlite-select
                        e-task-storage-sqlite-worker--database
                        "SELECT 1 FROM task_records WHERE queue_id=? LIMIT 1"
                        (vector queue-id))))
      (signal 'e-runtime-store-task-conflict
              (list "Legacy task import requires an empty queue" queue-id)))
    (dolist (record records)
      (let ((task-id (plist-get record :task-id)))
        (unless (and (stringp task-id) (not (string-empty-p task-id))
                     (not (gethash task-id by-id)))
          (signal 'e-runtime-store-task-conflict
                  (list "Malformed or duplicate legacy task" task-id)))
        (puthash task-id (copy-tree record) by-id)))
    (unless (= (length order) (hash-table-count by-id))
      (signal 'e-runtime-store-task-conflict
              (list "Legacy task order does not match records")))
    (dolist (task-id order)
      (let* ((record (gethash task-id by-id))
             (status (and record (plist-get record :status))))
        (unless record
          (signal 'e-runtime-store-task-conflict
                  (list "Legacy task order names an unknown task" task-id)))
        ;; A legacy live process cannot survive an offline migration.  Preserve
        ;; ambiguity instead of turning a possibly irreversible effect into a
        ;; fresh queued attempt.
        (when (memq status '(running pausing))
          (setq status 'interrupted
                record (plist-put record :status 'interrupted)
                record (plist-put record :error
                                  "Legacy in-flight task was interrupted during migration")))
        (unless (memq status e-task-storage-sqlite-worker--import-statuses)
          (signal 'e-runtime-store-task-conflict
                  (list "Unsupported legacy task status" task-id status)))
        (let* ((attempt-p
                (or (memq status '(done failed cancelled interrupted))
                    (cl-some (lambda (key) (plist-member record key))
                             '(:attempt-id :attempt-number :session-id
                               :started-at :finished-at :settled-at
                               :outputs :error))))
               (attempt-id
                (and attempt-p
                     (or (plist-get record :attempt-id)
                         (format "legacy-attempt:%s" task-id))))
               (attempt-number
                (and attempt-p (or (plist-get record :attempt-number) 1))))
          (setq position (1+ position))
          (sqlite-execute
           e-task-storage-sqlite-worker--database
           "INSERT INTO task_records(queue_id,task_id,position,status,revision,enqueued_at,harness_selector,retry_count,latest_attempt_id,payload) VALUES(?,?,?,?,1,?,?,?,?,?)"
           (vector queue-id task-id position (symbol-name status)
                   (plist-get record :enqueued-at)
                   (e-task-storage-sqlite-worker--pack
                    (plist-get record :harness-instance-id))
                   (or (plist-get record :retries) 0)
                   attempt-id
                   (e-task-storage-sqlite-worker--pack
                    (e-task-storage-sqlite-worker--task-content record))))
          (when attempt-p
            (sqlite-execute
             e-task-storage-sqlite-worker--database
             "INSERT INTO task_attempts(queue_id,task_id,attempt_id,attempt_number,state,harness_instance_id,session_id,started_at,settled_at,outputs,error) VALUES(?,?,?,?,?,?,?,?,?,?,?)"
             (vector queue-id task-id attempt-id attempt-number
                     (symbol-name status)
                     (plist-get record :harness-instance-id)
                     (plist-get record :session-id)
                     (plist-get record :started-at)
                     (or (plist-get record :finished-at)
                         (plist-get record :settled-at))
                     (and (plist-member record :outputs)
                          (e-task-storage-sqlite-worker--pack
                           (plist-get record :outputs)))
                     (and (plist-member record :error)
                          (e-task-storage-sqlite-worker--pack
                           (plist-get record :error)))))))))
    (e-task-storage-sqlite-worker--set-root
     queue-id (if records 1 0) (max sequence position)
     (and (plist-get snapshot :paused-p) t))
    (list :queue-id queue-id :revision (if records 1 0)
          :sequence (max sequence position)
          :paused-p (and (plist-get snapshot :paused-p) t)
          :records position)))

(defun e-task-storage-sqlite-worker-write (database body)
  "Execute one typed task write BODY using DATABASE."
  (setq e-task-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('task-queue-open (e-task-storage-sqlite-worker--open body))
    ('task-enqueue (e-task-storage-sqlite-worker--enqueue body))
    ('task-claim (e-task-storage-sqlite-worker--claim body))
    ('task-runnable-claim
     (e-task-storage-sqlite-worker--claim-runnable body))
    ('task-transition (e-task-storage-sqlite-worker--transition body))
    ('task-queue-pause (e-task-storage-sqlite-worker--pause body))
    ('task-history-delete (e-task-storage-sqlite-worker--delete-history body))
    ('task-import-legacy-snapshot
     (e-task-storage-sqlite-worker--import-legacy-snapshot body))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown task write" (plist-get body :op))))))

(defun e-task-storage-sqlite-worker-read (database body)
  "Execute one bounded task read BODY using DATABASE."
  (setq e-task-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('task-queue-status
     (let* ((queue-id (plist-get body :queue-id))
            (root (e-task-storage-sqlite-worker--queue queue-id)))
       (list :queue-id queue-id
             :revision (e-task-storage-sqlite-worker--column root 0)
             :sequence (e-task-storage-sqlite-worker--column root 1)
             :paused-p (= (e-task-storage-sqlite-worker--column root 2) 1))))
    ('task-snapshot
     (let* ((queue-id (plist-get body :queue-id))
            (root (e-task-storage-sqlite-worker--queue queue-id))
            (limit (min 4096 (max 1 (or (plist-get body :limit) 1024))))
            (task-rows
             (sqlite-select
              database
              (concat
               "SELECT t.queue_id,t.task_id,t.position,t.status,t.revision,"
               "t.enqueued_at,t.harness_selector,t.retry_count,"
               "t.latest_attempt_id,t.payload,"
               "a.queue_id,a.task_id,a.attempt_id,a.attempt_number,a.state,"
               "a.harness_instance_id,a.session_id,a.started_at,a.settled_at,"
               "a.outputs,a.error "
               "FROM task_records t LEFT JOIN task_attempts a "
               "ON a.queue_id=t.queue_id AND a.attempt_id=t.latest_attempt_id "
               "WHERE t.queue_id=? ORDER BY t.position LIMIT ?")
              (vector queue-id limit)))
            (records
             (mapcar
              (lambda (row)
                (let ((values (append row nil)))
                  (e-task-storage-sqlite-worker--task-dto
                   (cl-subseq values 0 10)
                   (and (nth 12 values) (cl-subseq values 10 21)))))
              task-rows))
            (attempts
             (mapcar
              #'e-task-storage-sqlite-worker--attempt-dto
              (sqlite-select
               database
               "SELECT queue_id,task_id,attempt_id,attempt_number,state,harness_instance_id,session_id,started_at,settled_at,outputs,error FROM task_attempts WHERE queue_id=? ORDER BY rowid LIMIT ?"
               (vector queue-id limit)))))
       (list :queue-id queue-id
             :revision (e-task-storage-sqlite-worker--column root 0)
             :sequence (e-task-storage-sqlite-worker--column root 1)
             :paused-p (= (e-task-storage-sqlite-worker--column root 2) 1)
             :records records :attempts attempts)))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown task read" (plist-get body :op))))))

(provide 'e-task-storage-sqlite-worker)

;;; e-task-storage-sqlite-worker.el ends here

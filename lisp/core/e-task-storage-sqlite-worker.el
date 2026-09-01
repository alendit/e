;;; e-task-storage-sqlite-worker.el --- Worker-side task SQL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns only the private task schema and typed task command handlers.  The
;; generic worker supplies the transaction, command deduplication, and codec.

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

(defun e-task-storage-sqlite-worker-initialize (database)
  "Initialize task relations in DATABASE."
  (setq e-task-storage-sqlite-worker--database database)
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS task_queues (queue_id TEXT PRIMARY KEY, revision INTEGER NOT NULL, sequence INTEGER NOT NULL, paused INTEGER NOT NULL)"
         "CREATE TABLE IF NOT EXISTS task_records (queue_id TEXT NOT NULL, task_id TEXT NOT NULL, position INTEGER NOT NULL, status TEXT NOT NULL, revision INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(queue_id,task_id), UNIQUE(queue_id,position), FOREIGN KEY(queue_id) REFERENCES task_queues(queue_id) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS task_records_dispatch ON task_records(queue_id,status,position)"
         "CREATE TABLE IF NOT EXISTS task_attempts (queue_id TEXT NOT NULL, task_id TEXT NOT NULL, attempt_id TEXT NOT NULL, attempt_number INTEGER NOT NULL, state TEXT NOT NULL, started_at TEXT, settled_at TEXT, payload TEXT NOT NULL, PRIMARY KEY(queue_id,attempt_id), FOREIGN KEY(queue_id,task_id) REFERENCES task_records(queue_id,task_id) ON DELETE CASCADE)"
         "CREATE TABLE IF NOT EXISTS task_events (queue_id TEXT NOT NULL, event_id TEXT NOT NULL, task_id TEXT NOT NULL, position INTEGER PRIMARY KEY AUTOINCREMENT, payload TEXT NOT NULL, UNIQUE(queue_id,event_id), FOREIGN KEY(queue_id,task_id) REFERENCES task_records(queue_id,task_id) ON DELETE CASCADE)"))
    (sqlite-execute database statement)))

(defun e-task-storage-sqlite-worker--queue (queue-id)
  "Return QUEUE-ID row or signal."
  (or (car (sqlite-select
            e-task-storage-sqlite-worker--database
            "SELECT revision,sequence,paused FROM task_queues WHERE queue_id=?"
            (vector queue-id)))
      (signal 'e-runtime-store-task-conflict
              (list "Unknown task queue" queue-id))))

(defun e-task-storage-sqlite-worker--check (body)
  "Return BODY queue row after revision validation."
  (let* ((queue-id (plist-get body :queue-id))
         (row (e-task-storage-sqlite-worker--queue queue-id))
         (expected (plist-get body :expected-revision))
         (revision (e-task-storage-sqlite-worker--column row 0)))
    (when (and expected (/= expected revision))
      (signal 'e-runtime-store-revision-conflict
              (list (format "task:%s" queue-id) expected revision)))
    row))

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
  (let* ((row (e-task-storage-sqlite-worker--check body))
         (queue-id (plist-get body :queue-id))
         (record (plist-get body :record))
         (task-id (plist-get record :task-id))
         (position (plist-get body :position))
         (revision (1+ (e-task-storage-sqlite-worker--column row 0))))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "INSERT INTO task_records(queue_id,task_id,position,status,revision,payload) VALUES(?,?,?,?,1,?)"
     (vector queue-id task-id position "queued"
             (e-task-storage-sqlite-worker--pack record)))
    (e-task-storage-sqlite-worker--set-root
     queue-id revision (max position
                            (e-task-storage-sqlite-worker--column row 1))
     (= (e-task-storage-sqlite-worker--column row 2) 1))
    (list :queue-id queue-id :task-id task-id :revision revision
          :task-revision 1 :record record)))

(defun e-task-storage-sqlite-worker--task-row (queue-id task-id)
  "Return QUEUE-ID TASK-ID row or signal."
  (or (car (sqlite-select
            e-task-storage-sqlite-worker--database
            "SELECT status,revision,payload FROM task_records WHERE queue_id=? AND task_id=?"
            (vector queue-id task-id)))
      (signal 'e-runtime-store-task-conflict
              (list "Unknown task" queue-id task-id))))

(defun e-task-storage-sqlite-worker--claim (body)
  "Claim one task before runner invocation."
  (let* ((queue-row (e-task-storage-sqlite-worker--check body))
         (queue-id (plist-get body :queue-id))
         (task-id (plist-get body :task-id))
         (attempt-id (plist-get body :attempt-id))
         (task-row (e-task-storage-sqlite-worker--task-row queue-id task-id))
         (status (intern (e-task-storage-sqlite-worker--column task-row 0)))
         (record (e-task-storage-sqlite-worker--unpack
                  (e-task-storage-sqlite-worker--column task-row 2)))
         (revision (1+ (e-task-storage-sqlite-worker--column queue-row 0)))
         (task-revision (1+ (e-task-storage-sqlite-worker--column task-row 1)))
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
     "INSERT INTO task_attempts(queue_id,task_id,attempt_id,attempt_number,state,started_at,payload) VALUES(?,?,?,?,?,?,?)"
     (vector queue-id task-id attempt-id number "claimed"
             (plist-get body :started-at)
             (e-task-storage-sqlite-worker--pack
              (list :attempt-id attempt-id :number number :state 'claimed))))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "UPDATE task_records SET status='running',revision=?,payload=? WHERE queue_id=? AND task_id=?"
     (vector task-revision (e-task-storage-sqlite-worker--pack record)
             queue-id task-id))
    (e-task-storage-sqlite-worker--set-root
     queue-id revision (e-task-storage-sqlite-worker--column queue-row 1)
     (= (e-task-storage-sqlite-worker--column queue-row 2) 1))
    (list :queue-id queue-id :task-id task-id :attempt-id attempt-id
          :revision revision :task-revision task-revision :record record)))

(defun e-task-storage-sqlite-worker--transition (body)
  "Commit one task transition from BODY."
  (let* ((queue-row (e-task-storage-sqlite-worker--check body))
         (queue-id (plist-get body :queue-id))
         (task-id (plist-get body :task-id))
         (task-row (e-task-storage-sqlite-worker--task-row queue-id task-id))
         (status (intern (e-task-storage-sqlite-worker--column task-row 0)))
         (expected-status (plist-get body :expected-status))
         (record (plist-get body :record))
         (next-status (plist-get record :status))
         (revision (1+ (e-task-storage-sqlite-worker--column queue-row 0)))
         (task-revision (1+ (e-task-storage-sqlite-worker--column task-row 1)))
         (event-id (plist-get body :event-id)))
    (unless (eq status expected-status)
      (signal 'e-runtime-store-task-conflict
              (list "Task status conflict" task-id expected-status status)))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "INSERT INTO task_events(queue_id,event_id,task_id,payload) VALUES(?,?,?,?)"
     (vector queue-id event-id task-id
             (e-task-storage-sqlite-worker--pack
              (list :from status :to next-status :record record))))
    (sqlite-execute
     e-task-storage-sqlite-worker--database
     "UPDATE task_records SET status=?,revision=?,payload=? WHERE queue_id=? AND task_id=?"
     (vector (symbol-name next-status) task-revision
             (e-task-storage-sqlite-worker--pack record) queue-id task-id))
    (when-let* ((attempt-id (plist-get record :attempt-id)))
      (sqlite-execute
       e-task-storage-sqlite-worker--database
       "UPDATE task_attempts SET state=?,settled_at=?,payload=? WHERE queue_id=? AND attempt_id=?"
       (vector (symbol-name next-status) (plist-get record :finished-at)
               (e-task-storage-sqlite-worker--pack
                (list :attempt-id attempt-id :state next-status
                      :outputs (plist-get record :outputs)
                      :error (plist-get record :error)))
               queue-id attempt-id)))
    (e-task-storage-sqlite-worker--set-root
     queue-id revision (e-task-storage-sqlite-worker--column queue-row 1)
     (= (e-task-storage-sqlite-worker--column queue-row 2) 1))
    (list :queue-id queue-id :task-id task-id :revision revision
          :task-revision task-revision :record record)))

(defun e-task-storage-sqlite-worker--pause (body)
  "Commit queue pause gate from BODY."
  (let* ((row (e-task-storage-sqlite-worker--check body))
         (queue-id (plist-get body :queue-id))
         (revision (1+ (e-task-storage-sqlite-worker--column row 0)))
         (paused-p (and (plist-get body :paused-p) t)))
    (e-task-storage-sqlite-worker--set-root
     queue-id revision (e-task-storage-sqlite-worker--column row 1) paused-p)
    (list :queue-id queue-id :revision revision :paused-p paused-p)))

(defun e-task-storage-sqlite-worker--delete-history (body)
  "Delete explicit task history from BODY."
  (let* ((row (e-task-storage-sqlite-worker--check body))
         (queue-id (plist-get body :queue-id))
         (revision (1+ (e-task-storage-sqlite-worker--column row 0)))
         (sequence (e-task-storage-sqlite-worker--column row 1)))
    (sqlite-execute e-task-storage-sqlite-worker--database
                    "DELETE FROM task_records WHERE queue_id=?"
                    (vector queue-id))
    ;; Keep the monotonic task sequence across history deletion.  Reusing an
    ;; old task identity would collide with the runtime command-dedup ledger.
    (e-task-storage-sqlite-worker--set-root queue-id revision sequence nil)
    (list :queue-id queue-id :revision revision :sequence sequence
          :deleted t)))

(defun e-task-storage-sqlite-worker-write (database body)
  "Execute one typed task write BODY using DATABASE."
  (setq e-task-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('task-queue-open (e-task-storage-sqlite-worker--open body))
    ('task-enqueue (e-task-storage-sqlite-worker--enqueue body))
    ('task-claim (e-task-storage-sqlite-worker--claim body))
    ('task-transition (e-task-storage-sqlite-worker--transition body))
    ('task-queue-pause (e-task-storage-sqlite-worker--pause body))
    ('task-history-delete (e-task-storage-sqlite-worker--delete-history body))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown task write" (plist-get body :op))))))

(defun e-task-storage-sqlite-worker-read (database body)
  "Execute one bounded task read BODY using DATABASE."
  (setq e-task-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('task-snapshot
     (let* ((queue-id (plist-get body :queue-id))
            (root (e-task-storage-sqlite-worker--queue queue-id))
            (limit (min 4096 (max 1 (or (plist-get body :limit) 1024))))
            (records
             (mapcar
              (lambda (row)
                (e-task-storage-sqlite-worker--unpack
                 (e-task-storage-sqlite-worker--column row 0)))
              (sqlite-select
               database
               "SELECT payload FROM task_records WHERE queue_id=? ORDER BY position LIMIT ?"
               (vector queue-id limit))))
            (attempts
             (mapcar
              (lambda (row)
                (list :task-id (e-task-storage-sqlite-worker--column row 0)
                      :attempt-id (e-task-storage-sqlite-worker--column row 1)
                      :number (e-task-storage-sqlite-worker--column row 2)
                      :state (intern (e-task-storage-sqlite-worker--column row 3))
                      :payload
                      (e-task-storage-sqlite-worker--unpack
                       (e-task-storage-sqlite-worker--column row 4))))
              (sqlite-select
               database
               "SELECT task_id,attempt_id,attempt_number,state,payload FROM task_attempts WHERE queue_id=? ORDER BY rowid LIMIT ?"
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

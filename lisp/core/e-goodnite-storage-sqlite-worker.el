;;; e-goodnite-storage-sqlite-worker.el --- Worker-side Goodnite SQL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'sqlite)
(require 'e-runtime-store-codec)

(defvar e-goodnite-storage-sqlite-worker--database nil)

(defun e-goodnite-storage-sqlite-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-goodnite-storage-sqlite-worker--pack (value)
  "Encode VALUE for SQLite."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-goodnite-storage-sqlite-worker--unpack (value)
  "Decode VALUE from SQLite."
  (e-runtime-store-codec-decode (base64-decode-string value)))

(defun e-goodnite-storage-sqlite-worker-initialize (database)
  "Initialize Goodnite demand relations in DATABASE."
  (setq e-goodnite-storage-sqlite-worker--database database)
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS goodnite_demand_events (position INTEGER PRIMARY KEY AUTOINCREMENT, event_id TEXT NOT NULL UNIQUE, payload TEXT NOT NULL, created_at REAL NOT NULL)"
         "CREATE TABLE IF NOT EXISTS goodnite_demand_checkpoints (consumer TEXT PRIMARY KEY, position INTEGER NOT NULL)"))
    (sqlite-execute database statement)))

(defun e-goodnite-storage-sqlite-worker--append (body)
  "Append or deduplicate one demand event from BODY."
  (let* ((event-id (plist-get body :event-id))
         (payload (e-goodnite-storage-sqlite-worker--pack
                   (plist-get body :event)))
         (prior (car (sqlite-select
                      e-goodnite-storage-sqlite-worker--database
                      "SELECT position FROM goodnite_demand_events WHERE event_id=?"
                      (vector event-id)))))
    (if prior
        ;; EVENT-ID is a canonical digest of the semantic demand fields.  The
        ;; observation timestamp is deliberately excluded from that identity,
        ;; so retries and repeated observations deduplicate to the first row.
        (list :event-id event-id
              :position (e-goodnite-storage-sqlite-worker--column prior 0)
              :duplicate t)
      (sqlite-execute
       e-goodnite-storage-sqlite-worker--database
       "INSERT INTO goodnite_demand_events(event_id,payload,created_at) VALUES(?,?,?)"
       (vector event-id payload (float-time)))
      (let ((position
             (e-goodnite-storage-sqlite-worker--column
              (car (sqlite-select
                    e-goodnite-storage-sqlite-worker--database
                    "SELECT position FROM goodnite_demand_events WHERE event_id=?"
                    (vector event-id))) 0)))
        (list :event-id event-id :position position :duplicate nil)))))

(defun e-goodnite-storage-sqlite-worker--ack (body)
  "Advance one named consumer checkpoint from BODY."
  (let* ((consumer (plist-get body :consumer))
         (position (max 0 (or (plist-get body :position) 0)))
         (maximum
          (e-goodnite-storage-sqlite-worker--column
           (car (sqlite-select
                 e-goodnite-storage-sqlite-worker--database
                 "SELECT COALESCE(MAX(position),0) FROM goodnite_demand_events")) 0))
         (prior (car (sqlite-select
                      e-goodnite-storage-sqlite-worker--database
                      "SELECT position FROM goodnite_demand_checkpoints WHERE consumer=?"
                      (vector consumer))))
         (current (if prior
                      (e-goodnite-storage-sqlite-worker--column prior 0)
                    0)))
    (when (> position maximum)
      (signal 'e-runtime-store-goodnite-conflict
              (list "Checkpoint exceeds event tail" position maximum)))
    (when (< position current)
      (signal 'e-runtime-store-goodnite-conflict
              (list "Checkpoint regression" consumer position current)))
    (sqlite-execute
     e-goodnite-storage-sqlite-worker--database
     "INSERT INTO goodnite_demand_checkpoints(consumer,position) VALUES(?,?) ON CONFLICT(consumer) DO UPDATE SET position=excluded.position"
     (vector consumer position))
    (list :consumer consumer :position position)))

(defun e-goodnite-storage-sqlite-worker--cleanup (body)
  "Delete one bounded acknowledged demand prefix from BODY."
  (let* ((consumer (plist-get body :consumer))
         (limit (min 4096 (max 1 (or (plist-get body :limit) 256))))
         (checkpoint-row
          (car (sqlite-select
                e-goodnite-storage-sqlite-worker--database
                "SELECT position FROM goodnite_demand_checkpoints WHERE consumer=?"
                (vector consumer)))))
    (unless checkpoint-row
      (signal 'e-runtime-store-goodnite-conflict
              (list "Cleanup requires acknowledged checkpoint" consumer)))
    (let* ((checkpoint
            (e-goodnite-storage-sqlite-worker--column checkpoint-row 0))
           (positions
            (mapcar
             (lambda (row)
               (e-goodnite-storage-sqlite-worker--column row 0))
             (sqlite-select
              e-goodnite-storage-sqlite-worker--database
              "SELECT position FROM goodnite_demand_events WHERE position<=? ORDER BY position LIMIT ?"
              (vector checkpoint limit)))))
      (dolist (position positions)
        (sqlite-execute e-goodnite-storage-sqlite-worker--database
                        "DELETE FROM goodnite_demand_events WHERE position=?"
                        (vector position)))
      (list :consumer consumer :checkpoint checkpoint
            :deleted (length positions)
            :more (= (length positions) limit)))))

(defun e-goodnite-storage-sqlite-worker-write (database body)
  "Execute typed Goodnite write BODY using DATABASE."
  (setq e-goodnite-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('goodnite-event-append
     (e-goodnite-storage-sqlite-worker--append body))
    ('goodnite-checkpoint-ack
     (e-goodnite-storage-sqlite-worker--ack body))
    ('goodnite-event-cleanup
     (e-goodnite-storage-sqlite-worker--cleanup body))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown Goodnite write" (plist-get body :op))))))

(defun e-goodnite-storage-sqlite-worker-read (database body)
  "Execute bounded Goodnite read BODY using DATABASE."
  (setq e-goodnite-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('goodnite-event-page
     (let* ((consumer (plist-get body :consumer))
            (checkpoint-row
             (car (sqlite-select
                   database
                   "SELECT position FROM goodnite_demand_checkpoints WHERE consumer=?"
                   (vector consumer))))
            (checkpoint (if checkpoint-row
                            (e-goodnite-storage-sqlite-worker--column
                             checkpoint-row 0)
                          0))
            (after (max checkpoint (or (plist-get body :after) 0)))
            (limit (min 1024 (max 1 (or (plist-get body :limit) 256))))
            (rows (sqlite-select
                   database
                   "SELECT position,event_id,payload FROM goodnite_demand_events WHERE position>? ORDER BY position LIMIT ?"
                   (vector after limit)))
            (events
             (mapcar
              (lambda (row)
                (list :position
                      (e-goodnite-storage-sqlite-worker--column row 0)
                      :event-id
                      (e-goodnite-storage-sqlite-worker--column row 1)
                      :event
                      (e-goodnite-storage-sqlite-worker--unpack
                       (e-goodnite-storage-sqlite-worker--column row 2))))
              rows)))
       (list :consumer consumer :checkpoint checkpoint :events events
             :next (and (= (length events) limit)
                        (plist-get (car (last events)) :position)))))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown Goodnite read" (plist-get body :op))))))

(provide 'e-goodnite-storage-sqlite-worker)

;;; e-goodnite-storage-sqlite-worker.el ends here

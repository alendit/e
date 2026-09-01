;;; e-cron-storage-sqlite-worker.el --- Worker-side cron SQL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'sqlite)
(require 'e-runtime-store-codec)

(defvar e-cron-storage-sqlite-worker--database nil)

(defun e-cron-storage-sqlite-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-cron-storage-sqlite-worker--pack (value)
  "Encode VALUE for SQLite."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-cron-storage-sqlite-worker--unpack (value)
  "Decode VALUE from SQLite."
  (and value
       (e-runtime-store-codec-decode (base64-decode-string value))))

(defun e-cron-storage-sqlite-worker-initialize (database)
  "Initialize cron relations in DATABASE."
  (setq e-cron-storage-sqlite-worker--database database)
  (dolist
      (statement
       '("CREATE TABLE IF NOT EXISTS cron_cadence (schedule_id TEXT PRIMARY KEY, definition_hash TEXT NOT NULL, definition_revision INTEGER NOT NULL, revision INTEGER NOT NULL, anchor REAL NOT NULL, last_fire REAL, next_fire REAL)"
         "CREATE TABLE IF NOT EXISTS cron_firings (schedule_id TEXT NOT NULL, firing_id TEXT NOT NULL, definition_revision INTEGER NOT NULL, due_at REAL NOT NULL, fire_at REAL NOT NULL, state TEXT NOT NULL, result TEXT, PRIMARY KEY(schedule_id,firing_id), FOREIGN KEY(schedule_id) REFERENCES cron_cadence(schedule_id) ON DELETE CASCADE)"
         "CREATE INDEX IF NOT EXISTS cron_firings_unresolved ON cron_firings(schedule_id,state,due_at)"))
    (sqlite-execute database statement)))

(defun e-cron-storage-sqlite-worker--row (id)
  "Return cadence row for ID or signal."
  (or (car (sqlite-select
            e-cron-storage-sqlite-worker--database
            "SELECT definition_hash,definition_revision,revision,anchor,last_fire,next_fire FROM cron_cadence WHERE schedule_id=?"
            (vector (format "%s" id))))
      (signal 'e-runtime-store-cron-conflict
              (list "Unknown cron schedule" id))))

(defun e-cron-storage-sqlite-worker--register (body)
  "Register or reconcile one schedule definition from BODY."
  (let* ((id (format "%s" (plist-get body :schedule-id)))
         (hash (plist-get body :definition-hash))
         (anchor (plist-get body :anchor))
         (row (car (sqlite-select
                    e-cron-storage-sqlite-worker--database
                    "SELECT definition_hash,definition_revision,revision,anchor,last_fire,next_fire FROM cron_cadence WHERE schedule_id=?"
                    (vector id)))))
    (if (null row)
        (sqlite-execute
         e-cron-storage-sqlite-worker--database
         "INSERT INTO cron_cadence(schedule_id,definition_hash,definition_revision,revision,anchor) VALUES(?,?,1,0,?)"
         (vector id hash anchor))
      (unless (equal hash (e-cron-storage-sqlite-worker--column row 0))
        (let ((definition-revision
               (1+ (e-cron-storage-sqlite-worker--column row 1)))
              (revision (1+ (e-cron-storage-sqlite-worker--column row 2))))
          (sqlite-execute
           e-cron-storage-sqlite-worker--database
           "UPDATE cron_cadence SET definition_hash=?,definition_revision=?,revision=?,anchor=?,last_fire=NULL,next_fire=NULL WHERE schedule_id=?"
           (vector hash definition-revision revision anchor id)))))
    (let ((current (e-cron-storage-sqlite-worker--row id)))
      (list :schedule-id id
            :definition-revision
            (e-cron-storage-sqlite-worker--column current 1)
            :revision (e-cron-storage-sqlite-worker--column current 2)
            :anchor (e-cron-storage-sqlite-worker--column current 3)
            :last-fire (e-cron-storage-sqlite-worker--column current 4)
            :next-fire (e-cron-storage-sqlite-worker--column current 5)))))

(defun e-cron-storage-sqlite-worker--claim (body)
  "Claim one firing and advance cadence from BODY."
  (let* ((id (format "%s" (plist-get body :schedule-id)))
         (row (e-cron-storage-sqlite-worker--row id))
         (expected (plist-get body :expected-revision))
         (actual (e-cron-storage-sqlite-worker--column row 2))
         (revision (1+ actual))
         (firing-id (plist-get body :firing-id)))
    (unless (= expected actual)
      (signal 'e-runtime-store-revision-conflict
              (list (format "cron:%s" id) expected actual)))
    (sqlite-execute
     e-cron-storage-sqlite-worker--database
     "INSERT INTO cron_firings(schedule_id,firing_id,definition_revision,due_at,fire_at,state) VALUES(?,?,?,?,?,'claimed')"
     (vector id firing-id
             (e-cron-storage-sqlite-worker--column row 1)
             (plist-get body :due-at) (plist-get body :fire-at)))
    (sqlite-execute
     e-cron-storage-sqlite-worker--database
     "UPDATE cron_cadence SET revision=?,last_fire=?,next_fire=? WHERE schedule_id=?"
     (vector revision (plist-get body :fire-at)
             (plist-get body :next-fire) id))
    (list :schedule-id id :firing-id firing-id :state 'claimed
          :revision revision
          :definition-revision
          (e-cron-storage-sqlite-worker--column row 1)
          :last-fire (plist-get body :fire-at)
          :next-fire (plist-get body :next-fire))))

(defun e-cron-storage-sqlite-worker--settle (body)
  "Settle one firing from BODY."
  (let* ((id (format "%s" (plist-get body :schedule-id)))
         (firing-id (plist-get body :firing-id))
         (row (car (sqlite-select
                    e-cron-storage-sqlite-worker--database
                    "SELECT state FROM cron_firings WHERE schedule_id=? AND firing_id=?"
                    (vector id firing-id))))
         (expected (plist-get body :expected-state))
         (state (plist-get body :state)))
    (unless row
      (signal 'e-runtime-store-cron-conflict
              (list "Unknown cron firing" id firing-id)))
    (unless (eq expected
                (intern (e-cron-storage-sqlite-worker--column row 0)))
      (signal 'e-runtime-store-cron-conflict
              (list "Cron firing state conflict" id firing-id expected
                    (e-cron-storage-sqlite-worker--column row 0))))
    (sqlite-execute
     e-cron-storage-sqlite-worker--database
     "UPDATE cron_firings SET state=?,result=? WHERE schedule_id=? AND firing_id=?"
     (vector (symbol-name state)
             (e-cron-storage-sqlite-worker--pack (plist-get body :result))
             id firing-id))
    (list :schedule-id id :firing-id firing-id :state state)))

(defun e-cron-storage-sqlite-worker--delete-history (body)
  "Delete firing history after validating cadence revision."
  (let* ((id (format "%s" (plist-get body :schedule-id)))
         (row (e-cron-storage-sqlite-worker--row id))
         (expected (plist-get body :expected-revision))
         (actual (e-cron-storage-sqlite-worker--column row 2)))
    (unless (= expected actual)
      (signal 'e-runtime-store-revision-conflict
              (list (format "cron:%s" id) expected actual)))
    (sqlite-execute e-cron-storage-sqlite-worker--database
                    "DELETE FROM cron_firings WHERE schedule_id=?" (vector id))
    (list :schedule-id id :revision actual :deleted t)))

(defun e-cron-storage-sqlite-worker-write (database body)
  "Execute typed cron write BODY using DATABASE."
  (setq e-cron-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('cron-register (e-cron-storage-sqlite-worker--register body))
    ('cron-claim (e-cron-storage-sqlite-worker--claim body))
    ('cron-settle (e-cron-storage-sqlite-worker--settle body))
    ('cron-history-delete
     (e-cron-storage-sqlite-worker--delete-history body))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown cron write" (plist-get body :op))))))

(defun e-cron-storage-sqlite-worker-read (database body)
  "Execute bounded cron read BODY using DATABASE."
  (setq e-cron-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('cron-cadence
     (let* ((id (format "%s" (plist-get body :schedule-id)))
            (row (e-cron-storage-sqlite-worker--row id))
            (unresolved
             (mapcar
              (lambda (firing)
                (list :firing-id
                      (e-cron-storage-sqlite-worker--column firing 0)
                      :definition-revision
                      (e-cron-storage-sqlite-worker--column firing 1)
                      :due-at (e-cron-storage-sqlite-worker--column firing 2)
                      :fire-at (e-cron-storage-sqlite-worker--column firing 3)
                      :state
                      (intern (e-cron-storage-sqlite-worker--column firing 4))))
              (sqlite-select
               database
               "SELECT firing_id,definition_revision,due_at,fire_at,state FROM cron_firings WHERE schedule_id=? AND state IN ('claimed','started','unsafe') ORDER BY due_at LIMIT 256"
               (vector id)))))
       (list :schedule-id id
             :definition-revision
             (e-cron-storage-sqlite-worker--column row 1)
             :revision (e-cron-storage-sqlite-worker--column row 2)
             :anchor (e-cron-storage-sqlite-worker--column row 3)
             :last-fire (e-cron-storage-sqlite-worker--column row 4)
             :next-fire (e-cron-storage-sqlite-worker--column row 5)
             :unresolved unresolved)))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown cron read" (plist-get body :op))))))

(provide 'e-cron-storage-sqlite-worker)

;;; e-cron-storage-sqlite-worker.el ends here

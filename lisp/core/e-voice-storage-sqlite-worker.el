;;; e-voice-storage-sqlite-worker.el --- Worker-side voice SQL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'sqlite)

(defvar e-voice-storage-sqlite-worker--database nil)

(defun e-voice-storage-sqlite-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-voice-storage-sqlite-worker-initialize (database)
  "Initialize voice relations in DATABASE."
  (setq e-voice-storage-sqlite-worker--database database)
  (sqlite-execute
   database
   "CREATE TABLE IF NOT EXISTS voice_tells (tell_key TEXT PRIMARY KEY, label TEXT NOT NULL, description TEXT, use_count INTEGER NOT NULL, last_use TEXT NOT NULL, lru_position INTEGER NOT NULL)"))

(defun e-voice-storage-sqlite-worker--list (&optional limit)
  "Return bounded voice tells in LRU order."
  (mapcar
   (lambda (row)
     (list :key (e-voice-storage-sqlite-worker--column row 0)
           :label (e-voice-storage-sqlite-worker--column row 1)
           :description (e-voice-storage-sqlite-worker--column row 2)
           :count (e-voice-storage-sqlite-worker--column row 3)
           :last (e-voice-storage-sqlite-worker--column row 4)))
   (sqlite-select
    e-voice-storage-sqlite-worker--database
    "SELECT tell_key,label,description,use_count,last_use FROM voice_tells ORDER BY lru_position LIMIT ?"
    (vector (min 4096 (max 1 (or limit 1024)))))))

(defun e-voice-storage-sqlite-worker--record (body)
  "Atomically update one tell and enforce its cap."
  (let* ((key (plist-get body :key))
         (cap (max 1 (min 4096 (or (plist-get body :cap) 128))))
         (existing
          (car (sqlite-select
                e-voice-storage-sqlite-worker--database
                "SELECT description,use_count FROM voice_tells WHERE tell_key=?"
                (vector key))))
         (description
          (or (plist-get body :description)
              (and existing
                   (e-voice-storage-sqlite-worker--column existing 0))))
         (count (1+ (if existing
                        (e-voice-storage-sqlite-worker--column existing 1)
                      0))))
    (sqlite-execute e-voice-storage-sqlite-worker--database
                    "UPDATE voice_tells SET lru_position=lru_position+1")
    (sqlite-execute
     e-voice-storage-sqlite-worker--database
     "INSERT INTO voice_tells(tell_key,label,description,use_count,last_use,lru_position) VALUES(?,?,?,?,?,0) ON CONFLICT(tell_key) DO UPDATE SET label=excluded.label,description=excluded.description,use_count=excluded.use_count,last_use=excluded.last_use,lru_position=0"
     (vector key (plist-get body :label) description count
             (plist-get body :last)))
    (sqlite-execute
     e-voice-storage-sqlite-worker--database
     "DELETE FROM voice_tells WHERE tell_key IN (SELECT tell_key FROM voice_tells ORDER BY lru_position LIMIT -1 OFFSET ?)"
     (vector cap))
    (list :entry (car (e-voice-storage-sqlite-worker--list 1))
          :tells (e-voice-storage-sqlite-worker--list cap))))

(defun e-voice-storage-sqlite-worker-write (database body)
  "Execute typed voice write BODY using DATABASE."
  (setq e-voice-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('voice-record (e-voice-storage-sqlite-worker--record body))
    ('voice-clear
     (let ((deleted (sqlite-execute database "DELETE FROM voice_tells")))
       (list :count 0 :deleted deleted)))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown voice write" (plist-get body :op))))))

(defun e-voice-storage-sqlite-worker-read (database body)
  "Execute typed voice read BODY using DATABASE."
  (setq e-voice-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('voice-list
     (let ((tells (e-voice-storage-sqlite-worker--list
                   (plist-get body :limit))))
       (list :count (length tells) :tells tells)))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown voice read" (plist-get body :op))))))

(provide 'e-voice-storage-sqlite-worker)

;;; e-voice-storage-sqlite-worker.el ends here

;;; e-raw-results-storage-sqlite-worker.el --- Worker-side raw-result SQL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'sqlite)
(require 'subr-x)
(require 'e-runtime-store-codec)

(defconst e-raw-results-storage-sqlite-worker-byte-limit (* 16 1024 1024)
  "Private one-BLOB raw-result limit.")

(defvar e-raw-results-storage-sqlite-worker--database nil)

(defun e-raw-results-storage-sqlite-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-raw-results-storage-sqlite-worker--pack (value)
  "Encode VALUE for SQLite."
  (base64-encode-string (e-runtime-store-codec-encode value) t))

(defun e-raw-results-storage-sqlite-worker--unpack (value)
  "Decode VALUE from SQLite."
  (and value
       (e-runtime-store-codec-decode (base64-decode-string value))))

(defun e-raw-results-storage-sqlite-worker-initialize (database)
  "Initialize raw-result relation in DATABASE."
  (setq e-raw-results-storage-sqlite-worker--database database)
  (sqlite-execute
   database
   "CREATE TABLE IF NOT EXISTS raw_results (uri TEXT PRIMARY KEY, content BLOB NOT NULL, content_hash TEXT NOT NULL, metadata TEXT, created_at REAL NOT NULL, expires_at REAL NOT NULL)"))

(defun e-raw-results-storage-sqlite-worker--put (body)
  "Commit immutable raw-result content from BODY."
  (let* ((uri (plist-get body :uri))
         (content (plist-get body :content))
         (bytes (string-bytes content))
         (hash (secure-hash 'sha256 content))
         (prior (car (sqlite-select
                      e-raw-results-storage-sqlite-worker--database
                      "SELECT content_hash,LENGTH(content),created_at,expires_at,metadata FROM raw_results WHERE uri=?"
                      (vector uri)))))
    (when (> bytes e-raw-results-storage-sqlite-worker-byte-limit)
      (signal 'e-runtime-store-resource-too-large
              (list bytes e-raw-results-storage-sqlite-worker-byte-limit)))
    (if prior
        (progn
          (unless (equal hash
                         (e-raw-results-storage-sqlite-worker--column prior 0))
            (signal 'e-runtime-store-raw-conflict
                    (list uri "different content")))
          (list :uri uri :bytes
                (e-raw-results-storage-sqlite-worker--column prior 1)
                :created-at
                (e-raw-results-storage-sqlite-worker--column prior 2)
                :expires-at
                (e-raw-results-storage-sqlite-worker--column prior 3)
                :metadata
                (e-raw-results-storage-sqlite-worker--unpack
                 (e-raw-results-storage-sqlite-worker--column prior 4))
                :duplicate t))
      (sqlite-execute
       e-raw-results-storage-sqlite-worker--database
       "INSERT INTO raw_results(uri,content,content_hash,metadata,created_at,expires_at) VALUES(?,?,?,?,?,?)"
       (vector uri content hash
               (e-raw-results-storage-sqlite-worker--pack
                (plist-get body :metadata))
               (plist-get body :created-at) (plist-get body :expires-at)))
      (list :uri uri :bytes bytes :created-at (plist-get body :created-at)
            :expires-at (plist-get body :expires-at)
            :metadata (plist-get body :metadata) :duplicate nil))))

(defun e-raw-results-storage-sqlite-worker--delete (body)
  "Delete one raw URI from BODY."
  (let ((uri (plist-get body :uri)))
    (list :uri uri
          :deleted
          (> (sqlite-execute
              e-raw-results-storage-sqlite-worker--database
              "DELETE FROM raw_results WHERE uri=?" (vector uri)) 0))))

(defun e-raw-results-storage-sqlite-worker--expire (body)
  "Delete one bounded expired raw-result page."
  (let* ((now (or (plist-get body :now) (float-time)))
         (limit (min 4096 (max 1 (or (plist-get body :limit) 256))))
         (uris
          (mapcar
           (lambda (row)
             (e-raw-results-storage-sqlite-worker--column row 0))
           (sqlite-select
            e-raw-results-storage-sqlite-worker--database
            "SELECT uri FROM raw_results WHERE expires_at<=? ORDER BY expires_at,uri LIMIT ?"
            (vector now limit)))))
    (dolist (uri uris)
      (sqlite-execute e-raw-results-storage-sqlite-worker--database
                      "DELETE FROM raw_results WHERE uri=?" (vector uri)))
    (list :deleted uris :more (= (length uris) limit))))

(defun e-raw-results-storage-sqlite-worker-write (database body)
  "Execute typed raw-result write BODY using DATABASE."
  (setq e-raw-results-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('raw-result-put (e-raw-results-storage-sqlite-worker--put body))
    ('raw-result-delete (e-raw-results-storage-sqlite-worker--delete body))
    ('raw-result-expire (e-raw-results-storage-sqlite-worker--expire body))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown raw-result write" (plist-get body :op))))))

(defun e-raw-results-storage-sqlite-worker-read (database body)
  "Execute bounded raw-result read BODY using DATABASE."
  (setq e-raw-results-storage-sqlite-worker--database database)
  (pcase (plist-get body :op)
    ('raw-result-read
     (when-let* ((row (car (sqlite-select
                            database
                            "SELECT content,metadata,created_at,expires_at FROM raw_results WHERE uri=? AND expires_at>?"
                            (vector (plist-get body :uri)
                                    (or (plist-get body :now) (float-time)))))))
       (list :content
             (e-raw-results-storage-sqlite-worker--column row 0)
             :metadata
             (e-raw-results-storage-sqlite-worker--unpack
              (e-raw-results-storage-sqlite-worker--column row 1))
             :created-at
             (e-raw-results-storage-sqlite-worker--column row 2)
             :expires-at
             (e-raw-results-storage-sqlite-worker--column row 3))))
    (_ (signal 'e-runtime-store-worker-error
               (list "Unknown raw-result read" (plist-get body :op))))))

(provide 'e-raw-results-storage-sqlite-worker)

;;; e-raw-results-storage-sqlite-worker.el ends here

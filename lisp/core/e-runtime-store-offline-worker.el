;;; e-runtime-store-offline-worker.el --- Explicit offline SQLite operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Runs only in a disposable batch Emacs launched by the operator application
;; service.  This module owns the narrow schema-upgrade operation that cannot
;; run during ordinary runtime startup.  It never loads a harness or domain
;; owner and does not expose arbitrary SQL.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'e-runtime-store-codec)
(require 'e-runtime-store-worker)

(define-error 'e-runtime-store-offline-error "Offline runtime-store operation failed")

(defun e-runtime-store-offline-worker--column (row index)
  "Return INDEX from SQLite ROW."
  (if (vectorp row) (aref row index) (nth index row)))

(defun e-runtime-store-offline-worker--check (database)
  "Signal unless DATABASE passes a full integrity check."
  (let ((rows (sqlite-select database "PRAGMA integrity_check")))
    (unless (and (= (length rows) 1)
                 (equal (e-runtime-store-offline-worker--column
                         (car rows) 0)
                        "ok"))
      (signal 'e-runtime-store-offline-error
              (list "SQLite integrity check failed" rows)))))

(defun e-runtime-store-offline-worker--version (database)
  "Return DATABASE schema version or signal with migration guidance."
  (unless (car (sqlite-select
                database
                "SELECT 1 FROM sqlite_master WHERE type='table' AND name='store_meta'"))
    (signal 'e-runtime-store-offline-error
            (list "Not an e SQLite runtime store; run legacy migration")))
  (let ((row (car (sqlite-select
                   database
                   "SELECT value FROM store_meta WHERE key='schema_version'"))))
    (unless row
      (signal 'e-runtime-store-offline-error
              (list "Missing SQLite schema version; run legacy migration")))
    (string-to-number
     (e-runtime-store-offline-worker--column row 0))))

(defun e-runtime-store-offline-worker--assert-unowned (database-file)
  "Reject a live owner for DATABASE-FILE and remove a stale crash marker."
  (let ((owner-file (concat database-file ".owner")))
    (when (file-exists-p owner-file)
      (let ((owner
             (condition-case nil
                 (with-temp-buffer
                   (insert-file-contents owner-file)
                   (read (current-buffer)))
               (error nil))))
        (when (and owner
                   (integerp (plist-get owner :pid))
                   (process-attributes (plist-get owner :pid)))
          (signal 'e-runtime-store-offline-error
                  (list "Store has a live runtime owner" owner)))
        (delete-file owner-file)))))

(defun e-runtime-store-offline-worker--upgrade (database-file backup-file)
  "Upgrade closed DATABASE-FILE after verified BACKUP-FILE creation."
  (unless (file-readable-p database-file)
    (signal 'e-runtime-store-offline-error
            (list "Store must exist" database-file)))
  (e-runtime-store-offline-worker--assert-unowned database-file)
  (when (file-exists-p backup-file)
    (signal 'file-already-exists (list backup-file)))
  (make-directory (file-name-directory backup-file) t)
  (set-file-modes (file-name-directory backup-file) #o700)
  (let ((database (sqlite-open database-file)))
    (unwind-protect
        (let ((version (e-runtime-store-offline-worker--version database))
              (current e-runtime-store-worker-schema-version))
          (cond
           ((> version current)
            (signal 'e-runtime-store-schema-too-new
                    (list :actual version :supported current)))
           ((= version current)
            (signal 'e-runtime-store-offline-error
                    (list "Store already uses the current schema" current)))
           ((/= version (1- current))
            (signal 'e-runtime-store-offline-error
                    (list "No supported direct upgrade path" version current))))
          (e-runtime-store-offline-worker--check database)
          (sqlite-execute database "VACUUM INTO ?" (vector backup-file))
          (set-file-modes backup-file #o600)
          (let ((backup (sqlite-open backup-file)))
            (unwind-protect
                (progn
                  (e-runtime-store-offline-worker--check backup)
                  (unless (= (e-runtime-store-offline-worker--version backup)
                             version)
                    (signal 'e-runtime-store-offline-error
                            (list "Backup schema verification failed"))))
              (sqlite-close backup)))
          (sqlite-execute database "BEGIN IMMEDIATE")
          (condition-case err
              (progn
                (sqlite-execute
                 database
                 "CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY, identity TEXT NOT NULL, checksum TEXT NOT NULL, applied_at REAL NOT NULL)")
                (sqlite-execute
                 database
                 "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(?,?,?,?)"
                 (vector current "feature87-p4-explicit-upgrade"
                         (secure-hash 'sha256 "feature87-schema-v4")
                         (float-time)))
                (sqlite-execute
                 database
                 "UPDATE store_meta SET value=? WHERE key='schema_version'"
                 (vector (number-to-string current)))
                (sqlite-execute database "COMMIT"))
            (error
             (ignore-errors (sqlite-execute database "ROLLBACK"))
             (signal (car err) (cdr err))))
          (e-runtime-store-offline-worker--check database)
          (set-file-modes database-file #o600)
          (list :from version :to current :backup backup-file
                :backup-bytes
                (file-attribute-size (file-attributes backup-file))
                :integrity "ok"))
      (sqlite-close database))))

(defun e-runtime-store-offline-worker-main ()
  "Execute one narrow operation from `command-line-args-left'."
  (let* ((operation (pop command-line-args-left))
         (database-file (pop command-line-args-left))
         (output-file (pop command-line-args-left))
         (argument (pop command-line-args-left))
         result)
    (unless (and operation database-file output-file)
      (signal 'e-runtime-store-offline-error (list "Missing offline arguments")))
    (setq result
          (pcase operation
            ("upgrade"
             (e-runtime-store-offline-worker--upgrade
              (expand-file-name database-file) (expand-file-name argument)))
            (_ (signal 'e-runtime-store-offline-error
                       (list "Unknown offline operation" operation)))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (e-runtime-store-codec-encode result)
                    nil output-file nil 'silent))
    (set-file-modes output-file #o600)))

(provide 'e-runtime-store-offline-worker)

;;; e-runtime-store-offline-worker.el ends here

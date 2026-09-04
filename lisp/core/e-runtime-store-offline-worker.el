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
(require 'e-runtime-store-ownership)

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

(defun e-runtime-store-offline-worker--upgrade (database-file backup-file)
  "Upgrade closed DATABASE-FILE after verified BACKUP-FILE creation."
  (unless (file-readable-p database-file)
    (signal 'e-runtime-store-offline-error
            (list "Store must exist" database-file)))
  (when (file-exists-p backup-file)
    (signal 'file-already-exists (list backup-file)))
  (make-directory (file-name-directory backup-file) t)
  (set-file-modes (file-name-directory backup-file) #o700)
  ;; Claim before the first SQLite open.  The ordinary worker and this explicit
  ;; offline operation deliberately share the same process-lifetime authority.
  (let ((claim
         (e-runtime-store-ownership-acquire
          database-file (format "offline:%d" (emacs-pid)) 'offline)))
    (unwind-protect
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
                      ;; v5's generic receipt relation is additive.  Ordinary
                      ;; startup rejects v4 before it reaches this point;
                      ;; only this verified, operator-selected transaction can
                      ;; install the recovery schema.
                      (sqlite-execute
                       database
                       "CREATE TABLE runtime_store_receipts (runtime_id TEXT NOT NULL, request_id TEXT NOT NULL, fingerprint TEXT NOT NULL, result TEXT NOT NULL, write_prefix INTEGER NOT NULL, PRIMARY KEY(runtime_id, request_id))")
                      (sqlite-execute
                       database
                       "CREATE INDEX runtime_store_receipts_watermark ON runtime_store_receipts(runtime_id, write_prefix)")
                      (sqlite-execute
                       database
                       "CREATE TABLE runtime_store_state (singleton INTEGER PRIMARY KEY CHECK(singleton = 1), runtime_id TEXT NOT NULL, parent_boot TEXT NOT NULL, parent_pid INTEGER NOT NULL, parent_process_start TEXT NOT NULL, acknowledged_prefix INTEGER NOT NULL DEFAULT 0, retired INTEGER NOT NULL DEFAULT 0, retirement_request_id TEXT, retirement_fingerprint TEXT, retirement_result TEXT)")
                      (when (getenv "E_RUNTIME_STORE_TEST_MIGRATION_FAULT")
                        ;; Explicitly test-only, evaluated inside the upgrade
                        ;; transaction so the rollback witness is meaningful.
                        (error "Forced runtime-store migration rollback"))
                      (sqlite-execute
                       database
                       "INSERT INTO schema_migrations(version,identity,checksum,applied_at) VALUES(?,?,?,?)"
                       (vector current "feature92-v4-to-v5-explicit-upgrade"
                               (secure-hash 'sha256 "feature92-schema-v5")
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
            (sqlite-close database)))
      ;; Both the ordinary and offline paths close their SQLite connection(s)
      ;; before their shared claim becomes available to the other worker.
      (e-runtime-store-ownership-release claim))))

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

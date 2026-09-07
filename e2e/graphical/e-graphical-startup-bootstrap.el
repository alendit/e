;;; e-graphical-startup-bootstrap.el --- Prepare the startup E2E fixture -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The startup-prewarm scenario must arrange its disposable store and worker
;; hold before the daemon loads `e'.  The shell supplies fresh paths; this file
;; creates an honest nonempty v5 fixture directly, arms the hold, and leaves a
;; bounded setup report for the real default prewarm to replace.

;;; Code:

(require 'cl-lib)
(require 'sqlite)

(defconst e-graphical-startup-bootstrap--project-root
  (expand-file-name
   "../.."
   (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the e checkout containing this bootstrap file.")

(let ((core (expand-file-name "lisp/core"
                             e-graphical-startup-bootstrap--project-root)))
  (add-to-list 'load-path core)
  ;; This bootstrap runs before the shared source bootstrap.  Prefer source so
  ;; the disposable fixture worker and client are from the same checkout.
  (setq load-prefer-newer nil
        load-suffixes (cons ".el" (delete ".el" load-suffixes))))

(require 'e-runtime-store)

(defun e-graphical-startup-bootstrap--make-v5-database (directory)
  "Create an honest disposable legacy v5 database in DIRECTORY.

This fixture is intentionally assembled without the current v6 initializer:
it contains the old schema marker, retired catalog/checkpoint relations, and a
nonempty legacy journal row.  Production startup then observes v5 and reports
the explicit-upgrade diagnostic before it can create any v6 relation."
  (let* ((database-file (expand-file-name "store.sqlite3" directory))
         (database (sqlite-open database-file)))
    (unwind-protect
        (progn
          (sqlite-execute
           database
           "CREATE TABLE store_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
          (sqlite-execute
           database
           "CREATE TABLE session_records (session_id TEXT NOT NULL, position INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(session_id, position))")
          (sqlite-execute
           database
           "CREATE TABLE session_checkpoints (session_id TEXT PRIMARY KEY, payload TEXT NOT NULL, revision INTEGER NOT NULL)")
          (sqlite-execute
           database
           "CREATE TABLE catalog_projection (singleton INTEGER PRIMARY KEY CHECK(singleton = 1), payload TEXT NOT NULL, revision INTEGER NOT NULL)")
          (sqlite-execute database
                         "INSERT INTO store_meta(key,value) VALUES('schema_version','5')")
          (sqlite-execute database
                         "INSERT INTO session_records(session_id,position,payload) VALUES('startup-fixture',1,'legacy-record')")
          (sqlite-execute database
                         "INSERT INTO session_checkpoints(session_id,payload,revision) VALUES('startup-fixture','legacy-checkpoint',1)")
          (sqlite-execute database
                         "INSERT INTO catalog_projection(singleton,payload,revision) VALUES(1,'legacy-catalog',1)"))
      (sqlite-close database))))

(defun e-graphical-startup-bootstrap--write-report (path phase directory)
  "Write bounded setup report for PHASE in DIRECTORY to PATH."
  (with-temp-file path
    (prin1 (list :phase phase
                 :directory directory
                 :fixture-nonempty t
                 :worker-schema 5
                 :hold 'open)
           (current-buffer))
    (insert "\n")))

(when (and (equal (getenv "E_GRAPHICAL_E2E_SELECTOR") "startup-prewarm")
           (equal (getenv "E_E2E_EMACS_CONFIG") "isolated"))
  (let* ((directory (getenv "E_RUNTIME_STATE_DIRECTORY"))
         (stall-directory (getenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY"))
         (report-file (getenv "E_GRAPHICAL_E2E_STARTUP_REPORT"))
         (old-stall-directory (getenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY")))
    (unless (and directory stall-directory report-file)
      (error "Startup E2E fixture paths were not supplied by the runner"))
    (make-directory directory t)
    (make-directory stall-directory t)
    ;; The fixture is assembled directly with old physical relations.  The
    ;; later daemon uses the production v6 worker and therefore reports the
    ;; explicit-upgrade diagnostic without mutating the fixture.
    (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" nil)
    (e-graphical-startup-bootstrap--make-v5-database directory)
    (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" old-stall-directory)
    ;; Source compilation happens in this isolated daemon before the real
    ;; prewarm request reaches its held-open observation.  Keep this test-only
    ;; deadline above that setup cost; the product default remains 60 seconds.
    (setq e-runtime-store-request-timeout 180.0)
    (with-temp-file
      (expand-file-name "open.hold" stall-directory)
      (insert "hold"))
    ;; Keep the setup report channel present before the daemon loads `e'.  The
    ;; default prewarm overwrites it once its startup-owned open is submitted.
    (e-graphical-startup-bootstrap--write-report
     report-file 'prepared directory)
    ;; The later daemon uses the production v6 worker; leave only the private
    ;; stall seam enabled for its startup-owned open.
    (setenv "E_RUNTIME_STORE_TEST_WORKER_FILE" nil)
    (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" stall-directory)))

(provide 'e-graphical-startup-bootstrap)

;;; e-graphical-startup-bootstrap.el ends here

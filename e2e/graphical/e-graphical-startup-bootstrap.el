;;; e-graphical-startup-bootstrap.el --- Prepare the startup E2E fixture -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The startup-prewarm scenario must arrange its disposable store and worker
;; hold before the daemon loads `e'.  The shell supplies fresh paths; this file
;; creates the nonempty v5 fixture, writes the v6 test worker, arms the hold,
;; and leaves a bounded setup report for the real default prewarm to replace.

;;; Code:

(require 'cl-lib)

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

(defun e-graphical-startup-bootstrap--make-v6-worker (target)
  "Write the test-only v6 worker copy to TARGET.

The production worker remains v5 until the explicit DP2 schema package.  This
copy changes exactly its schema declaration and nothing else."
  (let ((source
         (expand-file-name "lisp/core/e-runtime-store-worker.el"
                           e-graphical-startup-bootstrap--project-root)))
    (with-temp-buffer
      (insert-file-contents source)
      (goto-char (point-min))
      (unless (search-forward
               "(defconst e-runtime-store-worker-schema-version 5)" nil t)
        (error "Could not locate the v5 worker schema declaration"))
      (replace-match
       "(defconst e-runtime-store-worker-schema-version 6)")
      (write-region (point-min) (point-max) target nil 'silent))))

(defun e-graphical-startup-bootstrap--write-report (path phase directory)
  "Write bounded setup report for PHASE in DIRECTORY to PATH."
  (with-temp-file path
    (prin1 (list :phase phase
                 :directory directory
                 :fixture-nonempty t
                 :worker-schema 6
                 :hold 'open)
           (current-buffer))
    (insert "\n")))

(when (and (equal (getenv "E_GRAPHICAL_E2E_SELECTOR") "startup-prewarm")
           (equal (getenv "E_E2E_EMACS_CONFIG") "isolated"))
  (let* ((directory (getenv "E_RUNTIME_STATE_DIRECTORY"))
         (stall-directory (getenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY"))
         (worker-file (getenv "E_RUNTIME_STORE_TEST_WORKER_FILE"))
         (report-file (getenv "E_GRAPHICAL_E2E_STARTUP_REPORT"))
         (old-worker-file (getenv "E_RUNTIME_STORE_TEST_WORKER_FILE"))
         (old-stall-directory (getenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY"))
         fixture)
    (unless (and directory stall-directory worker-file report-file)
      (error "Startup E2E fixture paths were not supplied by the runner"))
    (make-directory directory t)
    (make-directory stall-directory t)
    ;; The fixture is intentionally created with the production v5 worker,
    ;; before the test-only worker override is visible to the later daemon.
    (setenv "E_RUNTIME_STORE_TEST_WORKER_FILE" nil)
    (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" nil)
    (unwind-protect
        (progn
          (setq fixture (e-runtime-store-open directory))
          (e-runtime-store-call
           fixture 'write
           '(:op session-append-batch :session-id "startup-fixture"
             :records [(:type "session" :session-id "startup-fixture"
                        :id "startup-root"
                        :timestamp "2026-09-05T00:00:00Z")]))
          (e-runtime-store-close fixture)
          (setq fixture nil))
      (when (and fixture
                 (not (e-runtime-store--closed fixture)))
        (ignore-errors (e-runtime-store-close fixture)))
      (setenv "E_RUNTIME_STORE_TEST_WORKER_FILE" old-worker-file)
      (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" old-stall-directory))
    (e-graphical-startup-bootstrap--make-v6-worker worker-file)
    (with-temp-file
      (expand-file-name "open.hold" stall-directory)
      (insert "hold"))
    ;; Keep the setup report channel present before the daemon loads `e'.  The
    ;; default prewarm overwrites it once its startup-owned open is submitted.
    (e-graphical-startup-bootstrap--write-report
     report-file 'prepared directory)
    (setenv "E_RUNTIME_STORE_TEST_WORKER_FILE" worker-file)
    (setenv "E_RUNTIME_STORE_TEST_STALL_DIRECTORY" stall-directory)))

(provide 'e-graphical-startup-bootstrap)

;;; e-graphical-startup-bootstrap.el ends here

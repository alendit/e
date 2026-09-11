;;; e-sqlite-test-store-support.el --- Disposable current-store fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Older semantic tests call the public persistent-session constructors and
;; often model restart by opening the same directory again.  Those constructors
;; now open SQLite, whose exclusive owner must be closed before restart.  This
;; test-only lifecycle fixture performs that ownership handoff and closes every
;; disposable worker after its ERT test.  It never selects a legacy backend and
;; is not loaded by production.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-session)

(defvar e-sqlite-test-store-support--stores (make-hash-table :test 'equal)
  "Disposable standalone session store lists keyed by canonical directory.")

(defvar e-sqlite-test-store-support--active-p nil
  "Non-nil while a test that explicitly requested this fixture is running.

This support file is loaded into the shared broad-suite Emacs.  Its constructor
advice must therefore be dynamically scoped to the older semantic suites that
require the blocking aggregate fixture; otherwise unrelated tests silently
receive a different public-store contract based only on test execution order.")

(defun e-sqlite-test-store-support--fixture-test-p (test)
  "Return non-nil when ERT TEST explicitly uses this semantic fixture."
  (let ((name (symbol-name (ert-test-name test))))
    (string-match-p
     (concat "\\`\\(?:e-compaction-\\|e-dev-test-\\|"
             "e-harness-base-\\|e-harness-test-\\|e-hook-audit-\\|"
             "e-process-reporting-\\|e-session-test-\\)")
     name)))

(defun e-sqlite-test-store-support--cancel-project-timers ()
  "Cancel deferred chat callbacks before disposable stores are closed."
  (dolist (function '(e-chat-service--subscription-drain-callback
                      e-chat-service--observer-drain-callback))
    (cancel-function-timers function)))

(defun e-sqlite-test-store-support--directory (directory)
  "Return canonical DIRECTORY used by standalone session fixtures."
  (file-name-as-directory
   (expand-file-name (or directory e-session-directory))))

(defun e-sqlite-test-store-support--open
    (load-all operation &optional directory &rest arguments)
  "Call OPERATION for DIRECTORY and ARGUMENTS, sharing one physical owner.

LOAD-ALL selects eager replay for a second semantic facade over the same
test-owned runtime worker."
  (if (not e-sqlite-test-store-support--active-p)
      (apply operation directory arguments)
    (let* ((key (e-sqlite-test-store-support--directory directory))
           (stores (gethash key e-sqlite-test-store-support--stores))
           (live-store
            (cl-find-if
             (lambda (candidate)
               (let ((runtime (e-session-storage-runtime-store candidate)))
                 (and runtime (not (e-runtime-store--closed runtime)))))
             stores))
           (store
            (if live-store
                (e-session-sqlite-store-create
                 directory :load-all load-all
                 :runtime-store
                 (e-session-storage-runtime-store live-store))
              ;; These older aggregate semantics tests explicitly request their
              ;; blocking batch fixture.  Do not call the public constructor:
              ;; ordinary v6 constructors are intentionally asynchronous and
              ;; refuse aggregate reconstruction.
              (progn
                (ignore operation arguments)
                (e-session-sqlite-store-create directory :load-all load-all)))))
      (puthash key (cons store stores) e-sqlite-test-store-support--stores)
      store)))

(defun e-sqlite-test-store-support--close-all ()
  "Close every test-owned standalone SQLite session store."
  (e-sqlite-test-store-support--cancel-project-timers)
  (maphash (lambda (_directory stores)
             (dolist (store stores)
               (ignore-errors (e-session-sqlite-store-close store))))
           e-sqlite-test-store-support--stores)
  (clrhash e-sqlite-test-store-support--stores))

(defun e-sqlite-test-store-support--run-test (operation &rest arguments)
  "Call ERT OPERATION with ARGUMENTS and release disposable stores."
  (let ((e-sqlite-test-store-support--active-p
         (e-sqlite-test-store-support--fixture-test-p (car arguments))))
    (if e-sqlite-test-store-support--active-p
        (unwind-protect (apply operation arguments)
          (e-sqlite-test-store-support--close-all))
      (apply operation arguments))))

(advice-add 'e-session-persistent-store-create :around
            (apply-partially #'e-sqlite-test-store-support--open t))
(advice-add 'e-session-persistent-index-store-create :around
            (apply-partially #'e-sqlite-test-store-support--open nil))
(advice-add 'ert-run-test :around #'e-sqlite-test-store-support--run-test)

(provide 'e-sqlite-test-store-support)

;;; e-sqlite-test-store-support.el ends here

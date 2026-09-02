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

(defun e-sqlite-test-store-support--cancel-project-timers ()
  "Cancel deferred e callbacks before their disposable stores are closed.
Semantic tests often leave presentation/Board drains pending after their final
assertion.  Running those callbacks in the following ERT case would mutate the
following case's dynamically isolated aggregate counters."
  (if (fboundp 'e-board-e2e--cancel-runtime-timers)
      (e-board-e2e--cancel-runtime-timers)
    (dolist (function '(e-chat-service--subscription-drain-callback
                        e-chat-service--observer-drain-callback
                        e-board-runtime--drain-deferred-hooks))
      (cancel-function-timers function))))

(defun e-sqlite-test-store-support--directory (directory)
  "Return canonical DIRECTORY used by standalone session fixtures."
  (file-name-as-directory
   (expand-file-name (or directory e-session-directory))))

(defun e-sqlite-test-store-support--open
    (load-all operation &optional directory &rest arguments)
  "Call OPERATION for DIRECTORY and ARGUMENTS, sharing one physical owner.

LOAD-ALL selects eager replay for a second semantic facade over the same
test-owned runtime worker."
  (let* ((key (e-sqlite-test-store-support--directory directory))
         (stores (gethash key e-sqlite-test-store-support--stores))
         (store
          (if stores
              (e-session-sqlite-store-create
               directory :load-all load-all
               :runtime-store
               (e-session-storage-runtime-store (car (last stores))))
            (apply operation directory arguments))))
    (puthash key (cons store stores) e-sqlite-test-store-support--stores)
    store))

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
  (unwind-protect (apply operation arguments)
    (e-sqlite-test-store-support--close-all)))

(advice-add 'e-session-persistent-store-create :around
            (apply-partially #'e-sqlite-test-store-support--open t))
(advice-add 'e-session-persistent-index-store-create :around
            (apply-partially #'e-sqlite-test-store-support--open nil))
(advice-add 'ert-run-test :around #'e-sqlite-test-store-support--run-test)

(provide 'e-sqlite-test-store-support)

;;; e-sqlite-test-store-support.el ends here

;;; e-board-engine-removal-structure-test.el --- No alternate Board engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; DP7C clean-cut proof.  SQLite Board services are the only durable Board
;; implementation; the former aggregate engine, registry, runtime, admission,
;; and storage-polymorphism modules must not remain available by load path or
;; symbol.

;;; Code:

(require 'ert)

(defconst e-board-engine-removal-structure-test--retired-modules
  '(e-board
    e-board-state
    e-board-admission
    e-board-durability
    e-board-registry
    e-board-runtime
    e-board-runtime-admission
    e-board-runtime-error
    e-board-pickup-admission
    e-board-session-association
    e-board-sqlite
    e-board-storage
    e-board-storage-sqlite
    e-board-storage-sqlite-worker
    e-board-orchestration-engine)
  "Feature names deleted by the DP7C Board-engine clean cut.")

(defconst e-board-engine-removal-structure-test--retired-files
  (mapcar (lambda (feature)
            (format "lisp/core/%s.el" feature))
          e-board-engine-removal-structure-test--retired-modules)
  "Source paths deleted with the alternate Board engine.")

(defconst e-board-engine-removal-structure-test--retired-symbols
  '(e-board-create
    e-board-get
    e-board-list
    e-board-register
    e-board-post-input
    e-board-post-output
    e-board-post-activity
    e-board-post-fact
    e-board-add-participant
    e-board-subscribe
    e-board-observer-subscribe
    e-board-runtime-create
    e-board-runtime-producer-bind
    e-board-registry-create
    e-board-admission-create
    e-board-pickup-admission-create
    e-board-session-association-create
    e-board-durability-create
    e-board-sqlite-create-registry-board
    e-board-storage
    e-board-storage-create-board
    e-board-storage-sqlite-create
    e-board-storage-sqlite-create-async
    e-board-orchestration-publish-fact
    e-board-orchestration-run-projection
    e-session-board-admission-records
    e-session-create-board-admission
    e-session-commit-board-admission
    e-session-append-board-message
    e-session-clear-board-messages
    e-session-declare-board-state
    e-session-board-association
    e-session-board-association-invalid-p
    e-session-board-association-policy-present-p
    e-session-board-routing-policy
    e-session-local-board-messages
    e-session--board-state-record
    e-session--persist-board-state
    e-session-codec--board-routing-selector-for-json
    e-session-codec--board-routing-selector-from-json
    e-session-codec--board-routing-policy-from-json
    e-session-codec--normalize-board-message
    e-session-codec-board-routing-policy-for-json
    e-session-codec-board-association-for-json
    e-session-codec-board-association-from-json
    e-session-catalog--board-messages)
  "Representative public constructors and operations deleted by DP7C.")

(defconst e-board-engine-removal-structure-test--retired-session-symbols
  '(e-session-checkpoint-board-message-limit
    e-session-checkpoint-board-fact-limit)
  "Retired session-owned Board projection variables.")

(defconst e-board-engine-removal-structure-test--ordinary-session-files
  '("lisp/core/e-session.el"
    "lisp/core/e-session-aggregate.el"
    "lisp/core/e-session-catalog.el"
    "lisp/core/e-session-query.el"
    "lisp/core/e-session-query-command.el")
  "Ordinary session sources forbidden from retaining Board journal families.")

(defconst e-board-engine-removal-structure-test--retired-session-record-types
  '("board-message" "board-messages-cleared" "board-session-state")
  "Board journal families accepted only by explicit offline migration.")

(defconst e-board-engine-removal-structure-test--source-roots
  '("lisp" "e2e")
  "Runtime and E2E source roots that may not depend on retired modules.")

(defun e-board-engine-removal-structure-test--source-files ()
  "Return current Lisp source files covered by the clean-cut scan."
  (apply #'append
         (mapcar (lambda (root)
                   (directory-files-recursively root "\\.el\\'"))
                 e-board-engine-removal-structure-test--source-roots)))

(defun e-board-engine-removal-structure-test--module-reference-hits ()
  "Return retired feature references remaining in current source."
  (let (hits)
    (dolist (file (e-board-engine-removal-structure-test--source-files))
      (unless (equal file "lisp/defaults/e-runtime-migration.el")
        (with-temp-buffer
          (insert-file-contents file)
          (dolist (feature e-board-engine-removal-structure-test--retired-modules)
            (goto-char (point-min))
            (when (re-search-forward
                   (format "(\\(?:require\\|provide\\) '%s)"
                           (regexp-quote (symbol-name feature)))
                   nil t)
              (push (list file feature) hits))))))
    (nreverse hits)))

(ert-deftest e-board-engine-removal-structure-test-retired-files-are-gone ()
  "No alternate Board implementation remains as a source file or library."
  (dolist (file e-board-engine-removal-structure-test--retired-files)
    (should-not (file-exists-p file)))
  (dolist (feature e-board-engine-removal-structure-test--retired-modules)
    (should-not (locate-library (symbol-name feature)))))

(ert-deftest e-board-engine-removal-structure-test-retired-apis-are-unbound ()
  "No callable compatibility shim survives for the alternate Board engine."
  (dolist (symbol e-board-engine-removal-structure-test--retired-symbols)
    (should-not (fboundp symbol)))
  (dolist (symbol e-board-engine-removal-structure-test--retired-session-symbols)
    (should-not (boundp symbol))))

(ert-deftest e-board-engine-removal-structure-test-no-module-dependencies ()
  "Runtime and E2E sources do not require or provide retired Board modules."
  (should-not
   (e-board-engine-removal-structure-test--module-reference-hits)))

(ert-deftest e-board-engine-removal-structure-test-session-has-no-board-journal ()
  "Ordinary session code has no reachable retired Board record family."
  (dolist (file e-board-engine-removal-structure-test--ordinary-session-files)
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (record-type
               e-board-engine-removal-structure-test--retired-session-record-types)
        (goto-char (point-min))
        (should-not (search-forward record-type nil t))))))

(provide 'e-board-engine-removal-structure-test)

;;; e-board-engine-removal-structure-test.el ends here

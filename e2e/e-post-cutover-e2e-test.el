;;; e-post-cutover-e2e-test.el --- SQLite-authoritative post-cutover E2E -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Cross-boundary coverage for the ordinary post-migration startup path.  The
;; scenario owns a disposable legacy tree, performs the real same-root offline
;; cutover, and then reaches its data through bounded asynchronous SQLite
;; queries.  Neither initial open nor reopen may reconstruct a session catalog
;; or durable session aggregate in Emacs.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-runtime-migration)
(require 'e-session)
(require 'e-session-async)
(require 'e-work)
(load (expand-file-name
       "e-post-cutover-e2e-support.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil nil t)

(defun e-post-cutover-e2e--query (work)
  "Return WORK's value at this explicit E2E batch boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 8)))

(defun e-post-cutover-e2e--assert-detached-state (store representative)
  "Assert STORE exposes REPRESENTATIVE only through bounded detached queries."
  (let* ((page
          (e-post-cutover-e2e--query
           (e-session-async-query-page
            store :limit e-post-cutover-e2e--session-count)))
         (row
          (seq-find
           (lambda (candidate)
             (equal (plist-get candidate :session-id) representative))
           (plist-get page :rows)))
         (visible
          (e-post-cutover-e2e--query
           (e-session-async-visible-message-page store representative 1))))
    (should (= (length (plist-get page :rows))
               e-post-cutover-e2e--session-count))
    (should row)
    (should (= (plist-get page :limit) e-post-cutover-e2e--session-count))
    (should (= (length (plist-get visible :messages)) 1))
    (should (equal
             (plist-get (car (plist-get visible :messages)) :content)
             (e-post-cutover-e2e--message-content 17)))
    (should (= (hash-table-count (e-session-store-sessions store)) 0))))

(ert-deftest e-post-cutover-e2e-test-same-root-cutover-queries-without-replay ()
  "Cutover feeds bounded SQLite queries without reconstructing durable state."
  (let* ((base (make-temp-file "e-post-cutover-e2e-" t))
         (root (expand-file-name "e" base))
         (source (expand-file-name "e-copy" base))
         (backup (expand-file-name "e.backup" base))
         (representative (e-post-cutover-e2e--session-id 17))
         (process-environment (copy-sequence process-environment))
         (e-session-directory (expand-file-name "sessions" root))
         (e-default--runtime nil)
         (e-default--chat-sessions nil)
         (e-runtime-sqlite--live-composition nil)
         (e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-registry--generations (make-hash-table :test 'equal))
         (e-harness-registry--invalidation-events
          (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-harness-instance--generation 0))
    (setenv "E_RUNTIME_STATE_DIRECTORY" nil)
    (unwind-protect
        (progn
          (e-post-cutover-e2e--make-legacy-root root)
          (copy-directory root source nil nil nil)
          (let* ((legacy-inventory (e-runtime-migration-inventory root))
                 (report (e-runtime-migration-cutover source root backup)))
            (should (eq (plist-get report :operation) 'cutover))
            (should (equal legacy-inventory
                           (e-runtime-migration-inventory source)))
            (should (equal legacy-inventory
                           (e-runtime-migration-inventory backup)))
            (should (file-regular-p (expand-file-name "store.sqlite3" root)))
            (should-not (file-exists-p (expand-file-name "sessions" root))))
          (e-default-harnesses-register
           '((:id :chat-default
              :name "Default Chat"
              :kind chat
              :default t
              :factory e-default-chat-harness-create
              :sync e-default-chat-harness-sync)))
          (let* ((harness
                  (e-harness-registry-get-or-create :chat-default))
                 (store (e-harness-sessions harness)))
            ;; Harness construction is transport-only.  No session row or
            ;; journal record is copied into the aggregate table.
            (should (= (hash-table-count (e-session-store-sessions store)) 0))
            (e-post-cutover-e2e--assert-detached-state store representative))
          (e-default-runtime-close)
          (e-harness-registry-clear-instance :chat-default)
          (let* ((harness
                  (e-harness-registry-get-or-create :chat-default))
                 (store (e-harness-sessions harness)))
            (should (= (hash-table-count (e-session-store-sessions store)) 0))
            (e-post-cutover-e2e--assert-detached-state store representative)))
      (e-default-runtime-close)
      (when (file-directory-p base)
        (delete-directory base t)))))

(provide 'e-post-cutover-e2e-test)

;;; e-post-cutover-e2e-test.el ends here

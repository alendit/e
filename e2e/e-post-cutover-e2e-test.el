;;; e-post-cutover-e2e-test.el --- Deterministic post-cutover E2E -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Cross-boundary coverage for the ordinary post-migration startup path.  The
;; scenario owns a disposable legacy tree, performs the real same-root offline
;; cutover, and then reaches the SQLite session store through the normal
;; :chat-default registry factory.  Provider I/O is outside this contract.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-runtime-migration)
(require 'e-session)
(require 'e-session-codec)

(defconst e-post-cutover-e2e--session-count 64
  "Number of legacy sessions in the structural lazy-loading fixture.")

(defun e-post-cutover-e2e--write (file text)
  "Write TEXT to test-owned FILE using the retired store encoding."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)))

(defun e-post-cutover-e2e--session-id (index)
  "Return the deterministic legacy session id for INDEX."
  (format "legacy-%03d" index))

(defun e-post-cutover-e2e--message-content (index)
  "Return the exact fixture message content for INDEX."
  (format "post-cutover message %03d" index))

(defun e-post-cutover-e2e--timestamp (index seconds)
  "Return a deterministic explicit-zone timestamp for INDEX and SECONDS."
  (format "2026-01-%02dT00:00:%02dZ" (1+ (% index 28)) seconds))

(defun e-post-cutover-e2e--write-legacy-session (root index)
  "Write one retired session journal below test-owned ROOT for INDEX."
  (let* ((session-id (e-post-cutover-e2e--session-id index))
         (created-at (e-post-cutover-e2e--timestamp index 0))
         (updated-at (e-post-cutover-e2e--timestamp index 1))
         (root-id (format "root-%03d" index))
         (message-id (format "message-%03d" index))
         (records
          (list
           (list :type "session" :session-id session-id :id root-id
                 :created-at created-at :updated-at updated-at :metadata nil)
           (list :type "message" :session-id session-id :id message-id
                 :parent-id root-id :timestamp updated-at
                 :message
                 (list :role 'user
                       :content (e-post-cutover-e2e--message-content index)
                       :created-at updated-at :type 'message
                       :id message-id :parent-id root-id)))))
    (e-post-cutover-e2e--write
     (expand-file-name
      (format "sessions/sessions/%s.jsonl" session-id) root)
     (concat
      (mapconcat
       (lambda (record)
         (json-encode (e-session-codec-record-for-json record)))
       records "\n")
      "\n"))
    (list :id session-id :created-at created-at :updated-at updated-at
          :message-count 1 :last-message-at updated-at
          :name (format "Legacy session %03d" index))))

(defun e-post-cutover-e2e--make-legacy-root (root)
  "Create the deterministic retired session tree at test-owned ROOT."
  (make-directory root t)
  (let (catalog)
    (dotimes (index e-post-cutover-e2e--session-count)
      (push (e-post-cutover-e2e--write-legacy-session root index) catalog))
    (e-post-cutover-e2e--write
     (expand-file-name "sessions/index.json" root)
     (concat (json-encode (vconcat (nreverse catalog))) "\n")))
  root)

(defun e-post-cutover-e2e--loaded-p (store session-id)
  "Return non-nil when SESSION-ID is loaded in STORE."
  (and (plist-get
        (e-session-aggregate-peek-session store session-id)
        :loaded)
       t))

(defun e-post-cutover-e2e--assert-stubs (store loaded-id unloaded-id)
  "Assert STORE has the complete catalog with selected load state.

LOADED-ID is nil before any semantic access.  UNLOADED-ID must remain an
unloaded catalog stub throughout the scenario."
  (should (= (length (e-session-list store))
             e-post-cutover-e2e--session-count))
  (dotimes (index e-post-cutover-e2e--session-count)
    (let ((session-id (e-post-cutover-e2e--session-id index)))
      (should (eq (e-post-cutover-e2e--loaded-p store session-id)
                  (and loaded-id (equal loaded-id session-id))))))
  (should-not (e-post-cutover-e2e--loaded-p store unloaded-id)))

(ert-deftest e-post-cutover-e2e-test-same-root-cutover-starts-lazy ()
  "Cutover feeds the ordinary lazy :chat-default composition exactly."
  (let* ((base (make-temp-file "e-post-cutover-e2e-" t))
         (root (expand-file-name "e" base))
         (source (expand-file-name "e-copy" base))
         (backup (expand-file-name "e.backup" base))
         (representative (e-post-cutover-e2e--session-id 17))
         (untouched (e-post-cutover-e2e--session-id 42))
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
         (e-harness-instance--generation 0)
         (read-page (symbol-function 'e-session-storage-read-session-page))
         (read-records
          (symbol-function 'e-session-storage-read-session-records))
         page-session-ids
         record-session-ids)
    (setenv "E_RUNTIME_STATE_DIRECTORY" nil)
    (unwind-protect
        (progn
          (e-post-cutover-e2e--make-legacy-root root)
          (copy-directory root source nil nil nil)
          (let* ((legacy-inventory (e-runtime-migration-inventory root))
                 (cutover-start (float-time))
                 (report (e-runtime-migration-cutover source root backup))
                 (cutover-seconds (- (float-time) cutover-start)))
            (should (eq (plist-get report :operation) 'cutover))
            (should (equal (directory-file-name (plist-get report :installed))
                           (directory-file-name root)))
            (should (equal legacy-inventory
                           (e-runtime-migration-inventory source)))
            (should (equal legacy-inventory
                           (e-runtime-migration-inventory backup)))
            (should (file-regular-p (expand-file-name "store.sqlite3" root)))
            (should-not (file-exists-p (expand-file-name "sessions" root)))
            (cl-letf
                (((symbol-function 'e-session-storage-read-session-page)
                  (lambda (store session-id &optional after limit)
                    (push session-id page-session-ids)
                    (funcall read-page store session-id after limit)))
                 ((symbol-function 'e-session-storage-read-session-records)
                  (lambda (store session-id &optional offset)
                    (push session-id record-session-ids)
                    (funcall read-records store session-id offset))))
              (e-default-harnesses-register
               '((:id :chat-default
                  :name "Default Chat"
                  :kind chat
                  :default t
                  :factory e-default-chat-harness-create
                  :sync e-default-chat-harness-sync)))
              (let* ((open-start (float-time))
                     (harness
                      (e-harness-registry-get-or-create :chat-default))
                     (store (e-harness-sessions harness))
                     (open-seconds (- (float-time) open-start)))
                (should (equal (e-default-runtime-directory)
                               (file-name-as-directory root)))
                (should (equal (e-session-store-directory store)
                               (file-name-as-directory root)))
                (should-not page-session-ids)
                (should-not record-session-ids)
                (e-post-cutover-e2e--assert-stubs store nil untouched)
                (let ((load-start (float-time)))
                  (should
                   (equal
                    (mapcar (lambda (message) (plist-get message :content))
                            (e-session-messages store representative))
                    (list (e-post-cutover-e2e--message-content 17))))
                  (message
                   (concat
                    "E87 post-cutover E2E observational seconds: "
                    "cutover=%.3f open=%.3f first-load=%.3f")
                   cutover-seconds open-seconds
                   (- (float-time) load-start)))
                (should (equal (delete-dups (copy-sequence record-session-ids))
                               (list representative)))
                (e-post-cutover-e2e--assert-stubs
                 store representative untouched))
              (e-default-runtime-close)
              (e-harness-registry-clear-instance :chat-default)
              (setq page-session-ids nil record-session-ids nil)
              (let* ((reopen-start (float-time))
                     (harness
                      (e-harness-registry-get-or-create :chat-default))
                     (store (e-harness-sessions harness))
                     (reopen-seconds (- (float-time) reopen-start)))
                (should-not page-session-ids)
                (should-not record-session-ids)
                (e-post-cutover-e2e--assert-stubs store nil untouched)
                (should
                 (equal
                  (mapcar (lambda (message) (plist-get message :content))
                          (e-session-messages store representative))
                  (list (e-post-cutover-e2e--message-content 17))))
                (should (equal
                         (delete-dups (copy-sequence record-session-ids))
                         (list representative)))
                (e-post-cutover-e2e--assert-stubs
                 store representative untouched)
                (message
                 "E87 post-cutover E2E observational reopen seconds: %.3f"
                 reopen-seconds)))))
      (e-default-runtime-close)
      (when (file-directory-p base)
        (delete-directory base t)))))

(provide 'e-post-cutover-e2e-test)

;;; e-post-cutover-e2e-test.el ends here

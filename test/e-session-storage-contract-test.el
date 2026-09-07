;;; e-session-storage-contract-test.el --- Direct storage owner contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests use an opaque application token.  The storage owner must be
;; able to own its physical state and commit a typed mutation without loading
;; or inspecting the session aggregate or application facade.

;;; Code:

(require 'ert)
(require 'e-runtime-store)
(require 'e-session-storage)

(defun e-session-storage-contract--source-requires-no-siblings-p ()
  "Return non-nil when storage source has no upward owner dependencies.

Fresh-load behavior is exercised by the batch owner-load gate; keeping this
ERT assertion source-oriented makes it safe to run alongside composed tests in
one Emacs process."
  (let ((source-file (or (locate-library "e-session-storage")
                         (expand-file-name "lisp/core/e-session-storage.el"
                                           default-directory))))
    (with-temp-buffer
      (insert-file-contents source-file)
      (not (re-search-forward
            "(require ['\"]e-session-\\(aggregate\\|catalog\\|session\\)"
            nil t)))))

(ert-deftest e-session-storage-contract-loads-without-aggregate-or-facade ()
  "The storage adapter loads without aggregate or application state."
  (should (e-session-storage-contract--source-requires-no-siblings-p)))

(ert-deftest e-session-storage-contract-state-is-explicit-and-owned ()
  "Current projection and durability state lives in the storage owner."
  (let* ((owner (make-symbol "opaque-session-owner"))
         (state (e-session-storage-register owner)))
    (should (e-session-storage--state-p state))
    (should (eq (e-session-storage--state-owner state) owner))
    (should (= (e-session-storage--state-unsettled-write-count state) 0))
    (should (= (hash-table-count
                (e-session-storage--state-checkpoint-dirty-session-ids state))
               0))
    (should-not (e-session-storage--state-projection-last-error state))))

(ert-deftest e-session-storage-contract-commits-typed-mutation-for-opaque-owner ()
  (ert-skip "Retired record-only mutation contract; v6 requires a domain query delta")
  "The semantic commit operation uses SQLite without aggregate knowledge."
  (let* ((directory (make-temp-file "e-session-storage-contract-" t))
         (owner (make-symbol "opaque-session-owner"))
         (session-id "opaque-session")
         (runtime (e-runtime-store-open directory))
         (state (e-session-storage-register
                 owner :directory directory :persistent t :backend 'sqlite
                 :runtime-store runtime :owns-runtime-store t)))
    (unwind-protect
        (progn
          (should (e-session-storage--state-p state))
          (e-session-storage-commit-mutation
           owner session-id
           (list :type "session" :session-id session-id
                 :id "opaque-root" :created-at "2026-08-30T00:00:00Z"
                 :metadata nil))
          (should (equal
                   (plist-get
                    (car (e-session-storage-read-session-records
                          owner session-id))
                    :id)
                   "opaque-root"))
          (should (equal (e-session-storage-session-ids owner)
                         (list session-id))))
      (e-session-storage-close owner)
      (delete-directory directory t))))

(ert-deftest e-session-storage-contract-checkpoint-write-is-atomic-value-operation ()
  (ert-skip "Retired checkpoint projection contract")
  "Checkpoint persistence accepts detached values, not aggregate records."
  (let* ((directory (make-temp-file "e-session-storage-checkpoint-" t))
         (owner (make-symbol "opaque-session-owner"))
         (runtime (e-runtime-store-open directory)))
    (unwind-protect
        (progn
          (e-session-storage-register
           owner :directory directory :persistent t :backend 'sqlite
           :runtime-store runtime :owns-runtime-store t)
          (e-session-storage-commit-mutation
           owner "checkpoint-session"
           '(:type "session" :session-id "checkpoint-session"
             :id "root" :created-at "2026-08-30T00:00:00Z"))
          (e-session-storage-persist-resume-checkpoint
           owner "checkpoint-session"
           '(:version 1 :entry-ids ["root" "message-1"]))
          (should (equal
                   (plist-get
                    (e-session-storage-read-resume-checkpoint
                     owner "checkpoint-session")
                    :version)
                   1)))
      (e-session-storage-close owner)
      (delete-directory directory t))))

(ert-deftest e-session-storage-contract-c04-bounds-rebuildable-projections-locally ()
  (ert-skip "Retired catalog/checkpoint projection contract")
  "Checkpoint omission and catalog overflow preserve primary session authority."
  (let* ((directory (make-temp-file "e-session-storage-c04-" t))
         (owner (make-symbol "opaque-session-owner"))
         (runtime (e-runtime-store-open directory))
         (session-id "projection-session")
         (checkpoint '(:version 1 :payload "x"))
         (checkpoint-limit
          (string-bytes (e-runtime-store-codec-encode checkpoint)))
         (catalog '((:id "projection-session" :title "x")))
         (catalog-limit
          (string-bytes (e-runtime-store-codec-encode catalog))))
    (unwind-protect
        (progn
          (e-session-storage-register
           owner :directory directory :persistent t :backend 'sqlite
           :runtime-store runtime :owns-runtime-store t)
          (e-session-storage-commit-mutation
           owner session-id
           '(:type "session" :session-id "projection-session"
             :id "root" :created-at "2026-09-04T00:00:00Z"))
          (should (= (string-bytes
                      (e-runtime-store-codec-encode
                       '(:version 1 :payload "xx")))
                     (1+ checkpoint-limit)))
          (let ((e-runtime-store-codec-checkpoint-canonical-byte-limit
                 checkpoint-limit))
            (should (plist-get
                     (e-session-storage-persist-resume-checkpoint
                      owner session-id checkpoint)
                     :revision))
            (let ((omitted
                   (e-session-storage-persist-resume-checkpoint
                    owner session-id '(:version 1 :payload "xx"))))
              (should (plist-get omitted :omitted)))
            (should (equal (e-session-storage-read-resume-checkpoint
                            owner session-id)
                           checkpoint)))
          (should (= (string-bytes
                      (e-runtime-store-codec-encode
                       '((:id "projection-session" :title "xx"))))
                     (1+ catalog-limit)))
          (let ((e-runtime-store-codec-catalog-canonical-byte-limit
                 catalog-limit))
            (e-session-storage-publish-catalog-projection owner catalog)
            (should-error
             (e-session-storage-publish-catalog-projection
              owner '((:id "projection-session" :title "xx")))
             :type 'e-runtime-store-projection-too-large))
          ;; The authoritative record and the last admitted projection survive
          ;; a local derived-projection failure; another owner can keep using
          ;; the shared runtime without a close/reopen cycle.
          (should (equal
                   (e-session-storage-read-catalog-projection owner) catalog))
          (should (= (length (e-session-storage-read-session-records
                              owner session-id))
                     1))
          (should (e-runtime-store-live-p runtime))
          (should (plist-get
                   (e-session-storage-commit-mutation
                    owner session-id
                    '(:type "message" :session-id "projection-session"
                      :id "after-overflow" :parent-id "root"
                      :created-at "2026-09-04T00:00:01Z"))
                   :revision)))
      (e-session-storage-close owner)
      (delete-directory directory t))))

(ert-deftest e-session-storage-contract-c04-cooperatively-assembles-identity-pages ()
  (ert-skip "Retired session-id enumeration contract")
  "The adapter assembles real cursor pages without an unbounded worker result."
  (let* ((directory (make-temp-file "e-session-storage-identities-" t))
         (initializer (e-runtime-store-open directory))
         (payload (base64-encode-string
                   (e-runtime-store-codec-encode '(:type "session")) t))
         owner runtime)
    (unwind-protect
        (progn
          ;; Cold open is asynchronous.  Observe a worker-owned read before
          ;; closing so the schema exists before this fixture seeds it with
          ;; direct SQL.
          (e-runtime-store-call initializer 'read '(:op store-metrics))
          (e-runtime-store-close initializer)
          (let ((database (sqlite-open (expand-file-name "store.sqlite3" directory))))
            (unwind-protect
                (dotimes (index 257)
                  (let ((session-id (format "page-%03d" index)))
                    (sqlite-execute
                     database
                     "INSERT INTO session_records(session_id,position,payload) VALUES(?,?,?)"
                     (vector session-id 1 payload))))
              (sqlite-close database)))
          (setq owner (make-symbol "opaque-session-owner")
                runtime (e-runtime-store-open directory))
          (e-session-storage-register
           owner :directory directory :persistent t :backend 'sqlite
           :runtime-store runtime :owns-runtime-store t)
          (let ((call (symbol-function 'e-runtime-store-call))
                calls ids)
            (cl-letf
                (((symbol-function 'e-runtime-store-call)
                  (lambda (candidate kind body)
                    (when (eq (plist-get body :op) 'session-id-page)
                      (push body calls))
                    (funcall call candidate kind body))))
              (setq ids (e-session-storage-session-ids owner)))
            (should (= (length ids) 257))
            (should (equal (car ids) "page-000"))
            (should (equal (car (last ids)) "page-256"))
            (should (= (length calls) 2))))
      (when owner (e-session-storage-close owner))
      (when (and runtime (not owner)) (e-runtime-store-close runtime))
      (delete-directory directory t))))

(ert-deftest e-session-storage-contract-rejects-retired-persistent-registration ()
  "Persistent callers cannot select the removed sidecar backend."
  (should-error
   (e-session-storage-register
    (make-symbol "legacy-owner") :directory temporary-file-directory
    :persistent t)
   :type 'e-session-storage-error))

(provide 'e-session-storage-contract-test)

;;; e-session-storage-contract-test.el ends here

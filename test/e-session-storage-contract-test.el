;;; e-session-storage-contract-test.el --- Direct storage owner contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests use an opaque application token.  The storage owner must be
;; able to own its physical state and commit a typed mutation without loading
;; or inspecting the session aggregate or application facade.

;;; Code:

(require 'ert)
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
  "Queue, checkpoint, and controller fields live in storage state."
  (let* ((owner (make-symbol "opaque-session-owner"))
         (state (e-session-storage-register owner :write-mode 'queued)))
    (should (e-session-storage--state-p state))
    (should (eq (e-session-storage--state-owner state) owner))
    (should (null (e-session-storage--state-write-queue state)))
    (should (= (e-session-storage--state-unsettled-write-count state) 0))
    (should-not (e-session-storage--state-controller state))))

(ert-deftest e-session-storage-contract-commits-typed-mutation-for-opaque-owner ()
  "The semantic commit operation writes JSONL without aggregate knowledge."
  (let* ((directory (make-temp-file "e-session-storage-contract-" t))
         (owner (make-symbol "opaque-session-owner"))
         (session-id "opaque-session")
         (state (e-session-storage-register
                 owner :directory directory :persistent t :write-mode nil)))
    (unwind-protect
        (progn
          (should (e-session-storage--state-p state))
          (e-session-storage-commit-mutation
           owner session-id
           (list :type "session" :session-id session-id
                 :id "opaque-root" :created-at "2026-08-30T00:00:00Z"
                 :metadata nil))
          (let ((journal (e-session-storage--session-file owner session-id)))
            (should (file-readable-p journal))
            (with-temp-buffer
              (insert-file-contents journal)
              (should (string-match-p "opaque-root" (buffer-string)))))
          (should (equal (e-session-storage-session-ids owner)
                         (list session-id))))
      (delete-directory directory t))))

(ert-deftest e-session-storage-contract-checkpoint-write-is-atomic-value-operation ()
  "Checkpoint persistence accepts detached values, not aggregate records."
  (let* ((directory (make-temp-file "e-session-storage-checkpoint-" t))
         (owner (make-symbol "opaque-session-owner")))
    (unwind-protect
        (progn
          (e-session-storage-register owner :directory directory :persistent t)
          (e-session-storage--write-checkpoint
           owner "checkpoint-session"
           '(:version 1 :entry-ids ["root" "message-1"]))
          (should (equal
                   (plist-get
                    (e-session-storage--read-checkpoint owner
                                                        "checkpoint-session")
                    :version)
                   1)))
      (delete-directory directory t))))

(provide 'e-session-storage-contract-test)

;;; e-session-storage-contract-test.el ends here

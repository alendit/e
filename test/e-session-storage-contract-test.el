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





(ert-deftest e-session-storage-contract-rejects-retired-persistent-registration ()
  "Persistent callers cannot select the removed sidecar backend."
  (should-error
   (e-session-storage-register
    (make-symbol "legacy-owner") :directory temporary-file-directory
    :persistent t)
   :type 'e-session-storage-error))

(provide 'e-session-storage-contract-test)

;;; e-session-storage-contract-test.el ends here

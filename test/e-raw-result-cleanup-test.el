;;; e-raw-result-cleanup-test.el --- Tests for mixed raw-result cleanup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for cleanup of mixed raw-result reference owners.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-raw-result-cleanup)
(require 'e-raw-results)
(require 'e-session-tmp-resources)

(ert-deftest e-raw-result-cleanup-test-deletes-mixed-reference-list ()
  "Mixed cleanup deletes both session tmp and generic raw-result references."
  (should (require 'e-raw-result-cleanup nil t))
  (let* ((directory (make-temp-file "e-raw-result-cleanup-test-" t))
         (e-raw-results-directory directory)
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (list (e-session-tmp-capability-create))))
         (session-reference
          (e-session-tmp-write-raw-result
           harness "session-1" "raw-results/session.txt" "session"))
         (generic-reference
          (e-raw-results-write
           :id "generic.txt"
           :content "generic"))
         (root (e-session-tmp-directory harness "session-1"))
         (session-path (expand-file-name "raw-results/session.txt" root))
         (generic-path (expand-file-name "generic.txt" directory)))
    (unwind-protect
        (let ((deleted
               (e-raw-result-cleanup-references
                harness
                "session-1"
                (list session-reference
                      '(:uri "file://not-raw-result" :storage file)
                      generic-reference))))
          (should (equal (sort (mapcar #'file-name-nondirectory deleted)
                               #'string<)
                         '("generic.txt" "session.txt")))
          (should-not (file-exists-p session-path))
          (should-not (file-exists-p generic-path))
          (should (file-directory-p root)))
      (when (file-directory-p directory)
        (delete-directory directory t))
      (e-session-tmp-cleanup-harness harness))))

(ert-deftest e-raw-result-cleanup-test-generic-reference-needs-no-session ()
  "Generic raw-result cleanup does not require harness session ownership."
  (should (require 'e-raw-result-cleanup nil t))
  (let* ((directory (make-temp-file "e-raw-result-cleanup-test-" t))
         (e-raw-results-directory directory)
         (reference (e-raw-results-write
                     :id "orphan.txt"
                     :content "orphan"))
         (path (expand-file-name "orphan.txt" directory)))
    (unwind-protect
        (progn
          (should (file-exists-p path))
          (should (equal (e-raw-result-cleanup-reference
                          nil nil reference)
                         path))
          (should-not (file-exists-p path)))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(provide 'e-raw-result-cleanup-test)

;;; e-raw-result-cleanup-test.el ends here

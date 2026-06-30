;;; e-raw-results-test.el --- Tests for raw-result resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for generic raw-result resources.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-capabilities)
(require 'e-resources)
(require 'e-raw-results)

(ert-deftest e-raw-results-test-write-and-read-reference ()
  "Raw-result references preserve full content behind a bounded preview."
  (should (require 'e-raw-results nil t))
  (let* ((directory (make-temp-file "e-raw-results-test-" t))
         (e-raw-results-directory directory)
         (reference (e-raw-results-write
                     :id "tool-output.txt"
                     :content "abcdefghijklmnopqrstuvwxyz"
                     :owner '(:kind tool-result :tool-name "bash")
                     :preview "abcdefghijkl"
                     :preview-bytes 10)))
    (unwind-protect
        (progn
          (should (equal (plist-get reference :uri)
                         "raw-result://tool-output.txt"))
          (should (eq (plist-get reference :storage) 'raw-result-store))
          (should (equal (plist-get reference :cleanup-lifetime)
                         'raw-result-store))
          (should (equal (plist-get reference :original-bytes) 26))
          (should (equal (plist-get reference :preview) "abcdefghij"))
          (should (equal (e-raw-results-read (plist-get reference :uri))
                         "abcdefghijklmnopqrstuvwxyz")))
      (delete-directory directory t))))

(ert-deftest e-raw-results-test-resource-method-reads-reference ()
  "The raw-result capability exposes stored output through the read operation."
  (should (require 'e-raw-results nil t))
  (let* ((directory (make-temp-file "e-raw-results-test-" t))
         (e-raw-results-directory directory)
         (registry (e-resources-registry-create))
         (reference (e-raw-results-write
                     :id "full.txt"
                     :content "full content")))
    (unwind-protect
        (progn
          (e-capabilities-register-resource-methods
           (e-raw-results-capability-create)
           registry)
          (should (equal (e-resources-read registry
                                           (plist-get reference :uri)
                                           nil)
                         "full content")))
      (delete-directory directory t))))

(ert-deftest e-raw-results-test-cleanup-reference-deletes-file ()
  "Raw-result references can be explicitly cleaned up by owner paths."
  (should (require 'e-raw-results nil t))
  (let* ((directory (make-temp-file "e-raw-results-test-" t))
         (e-raw-results-directory directory)
         (reference (e-raw-results-write
                     :id "cleanup.txt"
                     :content "discard me"))
         (path (expand-file-name "cleanup.txt" directory)))
    (unwind-protect
        (progn
          (should (file-exists-p path))
          (should (equal (e-raw-results-cleanup-reference reference) path))
          (should-not (file-exists-p path))
          (should-not (e-raw-results-cleanup-reference reference)))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(provide 'e-raw-results-test)

;;; e-raw-results-test.el ends here

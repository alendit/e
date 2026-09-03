;;; e-base-tools-owner-test.el --- Direct base-tool owner contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; File/resource and bash/process owners are tested directly.  Full registry
;; composition and live-buffer behavior remains covered by e-base-tools-test.

;;; Code:

(require 'ert)
(require 'e-base-tools-bash)
(require 'e-base-tools-file)
(load (expand-file-name
       "e-test-environment-support.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-base-tools-owner-test-loads-without-facade ()
  "The file and bash owners load without the base-tools facade."
  (e-test-run-fresh-owner-isolation
   "base-tool owner isolation"
   '(e-base-tools-bash e-base-tools-file)
   '(e-base-tools)
   '((fboundp 'e-base-tools-file-resource-path)
     (fboundp 'e-base-tools-file-buffer-coherence-group)
     (fboundp 'e-base-tools-file--argument-string)
     (fboundp 'e-base-tools-bash--argument-string)
     (fboundp 'e-base-tools-bash--truncate-tail-lines))))

(ert-deftest e-base-tools-owner-test-file-path-stays-inside-root ()
  "File owner resolves a parsed resource address within its root."
  (let ((root (file-name-as-directory (make-temp-file "e-base-owner-" t))))
    (unwind-protect
        (let ((path (e-base-tools-file-resource-path
                     '(:address "note.txt") root)))
          (should (equal path (expand-file-name "note.txt" root)))
          (should-error
           (e-base-tools-file-resource-path '(:address "../escape") root)
           :type 'e-base-tools-path-outside-root))
      (delete-directory root t))))

(ert-deftest e-base-tools-owner-test-bash-bounds-tail-output ()
  "Bash owner truncates output through its own bounded collector policy."
  (let ((result
         (e-base-tools-bash--truncate-tail-lines
          (concat
           (mapconcat #'number-to-string (number-sequence 1 1001) "\n")
           "\n"))))
    (should (plist-get result :truncated))
    (should (eq (plist-get result :truncated-by) 'lines))
    (should (= (plist-get result :total-lines) 1001))
    (should (= (plist-get result :output-lines) 1000))))

(provide 'e-base-tools-owner-test)

;;; e-base-tools-owner-test.el ends here

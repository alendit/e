;;; e-base-tools-test.el --- Tests for base filesystem and shell tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for Pi-like base tools.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-base-tools)
(require 'e-tools)

(defun e-base-tools-test--execute (registry name arguments)
  "Execute NAME with ARGUMENTS against REGISTRY."
  (e-tools-execute
   registry
   (list :id "call-1" :name name :arguments arguments)))

(ert-deftest e-base-tools-test-read-file-full-and-range ()
  "The read tool reads full and ranged text files."
  (let* ((directory (make-temp-file "e-base-read-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (write-region "one\ntwo\nthree\n" nil file nil 'silent)
          (e-base-tools-register-read registry directory)
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry "read" '(:path "sample.txt"))
                   :content)
                  "one\ntwo\nthree\n"))
          (should
           (equal (plist-get
                   (e-base-tools-test--execute
                    registry "read" '(:path "sample.txt" :offset 2 :limit 1))
                   :content)
                  "two\n\n[2 more lines in file. Use offset=3 to continue.]")))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-read-file-errors-for-missing-and-binary ()
  "The read tool fails clearly for missing and binary files."
  (let* ((directory (make-temp-file "e-base-read-errors-" t))
         (binary-file (expand-file-name "image.png" directory))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (write-region (unibyte-string 137 80 78 71 13 10 26 10 0)
                        nil binary-file nil 'silent)
          (e-base-tools-register-read registry directory)
          (let ((missing (e-base-tools-test--execute
                          registry "read" '(:path "missing.txt")))
                (binary (e-base-tools-test--execute
                         registry "read" '(:path "image.png"))))
            (should (equal (plist-get missing :status) 'error))
            (should (string-match-p "File is not readable"
                                    (plist-get missing :content)))
            (should (equal (plist-get binary :status) 'error))
            (should (string-match-p "text-only"
                                    (plist-get binary :content)))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-read-file-truncates-with-continuation ()
  "The read tool truncates large text files with a continuation hint."
  (let* ((directory (make-temp-file "e-base-read-truncate-" t))
         (file (expand-file-name "large.txt" directory))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (write-region
           (mapconcat (lambda (index) (format "line-%04d" index))
                      (number-sequence 1 2105)
                      "\n")
           nil file nil 'silent)
          (e-base-tools-register-read registry directory)
          (let ((content (plist-get
                          (e-base-tools-test--execute
                           registry "read" '(:path "large.txt"))
                          :content)))
            (should (string-match-p "line-0001" content))
            (should-not (string-match-p "line-2105" content))
            (should (string-match-p
                     "\\[Showing lines 1-2000 of 2105\\. Use offset=2001 to continue\\.\\]"
                     content))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-write-file-creates-parents-and-overwrites ()
  "The write tool creates parent directories and overwrites files."
  (let* ((directory (make-temp-file "e-base-write-" t))
         (target (expand-file-name "nested/file.txt" directory))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (e-base-tools-register-write registry directory)
          (let ((result (e-base-tools-test--execute
                         registry
                         "write"
                         '(:path "nested/file.txt" :content "new text"))))
            (should (equal (plist-get result :status) 'ok))
            (should (string-match-p "Successfully wrote 8 bytes"
                                    (plist-get result :content)))
            (should (equal (with-temp-buffer
                             (insert-file-contents target)
                             (buffer-string))
                           "new text")))
          (e-base-tools-test--execute
           registry
           "write"
           '(:path "nested/file.txt" :content "replacement"))
          (should (equal (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))
                         "replacement")))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-edit-file-applies-disjoint-edits-and-preserves-crlf ()
  "The edit tool applies multiple exact edits and preserves CRLF endings."
  (let* ((directory (make-temp-file "e-base-edit-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (write-region "alpha\r\nbeta\r\ngamma\r\n" nil file nil 'silent)
          (e-base-tools-register-edit registry directory)
          (let ((result (e-base-tools-test--execute
                         registry
                         "edit"
                         '(:path "sample.txt"
                           :edits ((:oldText "alpha" :newText "ALPHA")
                                   (:oldText "gamma" :newText "GAMMA"))))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (plist-get (plist-get result :content) :replacements)
                           2))
            (should (string-match-p "^-alpha" (plist-get (plist-get result :content) :diff)))
            (should (equal (with-temp-buffer
                             (insert-file-contents-literally file)
                             (buffer-string))
                           "ALPHA\r\nbeta\r\nGAMMA\r\n"))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-edit-file-rejects-invalid-replacements ()
  "The edit tool rejects missing, duplicate, empty, overlapping, and no-op edits."
  (let* ((directory (make-temp-file "e-base-edit-errors-" t))
         (file (expand-file-name "sample.txt" directory))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (write-region "alpha beta beta gamma" nil file nil 'silent)
          (e-base-tools-register-edit registry directory)
          (dolist (case '(("missing" (:path "sample.txt"
                                     :edits ((:oldText "missing" :newText "x"))))
                          ("unique" (:path "sample.txt"
                                    :edits ((:oldText "beta" :newText "x"))))
                          ("empty" (:path "sample.txt"
                                   :edits ((:oldText "" :newText "x"))))
                          ("overlap" (:path "sample.txt"
                                     :edits ((:oldText "alpha beta" :newText "x")
                                             (:oldText "beta beta" :newText "y"))))
                          ("No changes" (:path "sample.txt"
                                        :edits ((:oldText "alpha" :newText "alpha"))))))
            (let ((result (e-base-tools-test--execute registry "edit" (cadr case))))
              (should (equal (plist-get result :status) 'error))
              (should (string-match-p (car case) (plist-get result :content))))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-captures-output-and-errors ()
  "The bash tool captures stdout/stderr and reports nonzero exits."
  (let* ((directory (make-temp-file "e-base-bash-" t))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (e-base-tools-register-bash registry directory)
          (let ((result (e-base-tools-test--execute
                         registry
                         "bash"
                         '(:command "printf out; printf err >&2"))))
            (should (equal (plist-get result :status) 'ok))
            (should (equal (plist-get result :content) "outerr")))
          (let ((result (e-base-tools-test--execute
                         registry
                         "bash"
                         '(:command "printf fail; exit 7"))))
            (should (equal (plist-get result :status) 'error))
            (should (string-match-p "fail" (plist-get result :content)))
            (should (string-match-p "Command exited with code 7"
                                    (plist-get result :content)))))
      (delete-directory directory t))))

(ert-deftest e-base-tools-test-bash-times-out-and-truncates-output ()
  "The bash tool honors timeouts and saves truncated output."
  (let* ((directory (make-temp-file "e-base-bash-limits-" t))
         (registry (e-tools-registry-create)))
    (unwind-protect
        (progn
          (e-base-tools-register-bash registry directory)
          (let ((timeout-result (e-base-tools-test--execute
                                 registry
                                 "bash"
                                 '(:command "sleep 2" :timeout 0.1))))
            (should (equal (plist-get timeout-result :status) 'error))
            (should (string-match-p "Command timed out after 0.1 seconds"
                                    (plist-get timeout-result :content))))
          (let* ((result (e-base-tools-test--execute
                          registry
                          "bash"
                          '(:command "yes line | head -n 2105")))
                 (content (plist-get result :content)))
            (should (equal (plist-get result :status) 'ok))
            (should (string-match-p "Full output:" content))
            (should (string-match-p "\\[Showing lines 106-2105 of 2105\\."
                                    content))
            (when (string-match "Full output: \\([^]\n]+\\)" content)
              (should (file-readable-p (match-string 1 content))))))
      (delete-directory directory t))))

(provide 'e-base-tools-test)

;;; e-base-tools-test.el ends here

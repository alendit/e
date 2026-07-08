;;; e-resource-toc-test.el --- Tests for resource table-of-content helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for wot-backed resource table-of-content helpers.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-resource-toc)

(defun e-resource-toc-test--fake-wot (directory)
  "Create a fake wot executable in DIRECTORY."
  (let ((file (expand-file-name "wot" directory)))
    (write-region
     (concat "#!/bin/sh\n"
             "printf 'args:'\n"
             "for arg in \"$@\"; do printf '<%s>' \"$arg\"; done\n"
             "printf '\\nstdin:'\n"
             "cat\n")
     nil
     file
     nil
     'silent)
    (set-file-modes file #o755)
    file))

(ert-deftest e-resource-toc-test-discovers-wot-only-on-exec-path ()
  "Availability follows `exec-path'."
  (let* ((directory (make-temp-file "e-resource-toc-bin-" t))
         (_wot (e-resource-toc-test--fake-wot directory)))
    (unwind-protect
        (let ((exec-path (list directory)))
          (should (e-resource-toc-available-p))
          (should (string-suffix-p "/wot" (e-resource-toc-wot-executable))))
      (delete-directory directory t)))
  (let ((exec-path (list (make-temp-file "e-resource-toc-empty-" t))))
    (unwind-protect
        (should-not (e-resource-toc-available-p))
      (delete-directory (car exec-path) t))))

(ert-deftest e-resource-toc-test-normalizes-options-and-infers-language ()
  "Options and language inference match wot's supported names."
  (should (equal (e-resource-toc-normalize-options
                  '(:max-depth 2 :max-items 4 :min-lines 0
                    :format "json" :language "elisp" :lenient t))
                 '(:max-depth 2 :max-items 4 :min-lines 0
                   :format "json" :language "elisp" :lenient t)))
  (should (equal (e-resource-toc-infer-language "file.el") "elisp"))
  (should (equal (e-resource-toc-infer-language "README.org") "org"))
  (should (equal (e-resource-toc-infer-language "Dockerfile") "dockerfile"))
  (should-error (e-resource-toc-normalize-options '(:format "xmlish"))
                :type 'e-resource-toc-invalid-option)
  (should-error (e-resource-toc-require-language nil "unknown.resource")
                :type 'e-resource-toc-language-required))

(ert-deftest e-resource-toc-test-run-content-pipes-stdin-to-wot ()
  "Stdin-backed table-of-content passes content and inferred language to wot."
  (let* ((directory (make-temp-file "e-resource-toc-bin-" t))
         (_wot (e-resource-toc-test--fake-wot directory)))
    (unwind-protect
        (let* ((exec-path (list directory))
               (result (e-resource-toc-run-content
                        "buffer://sample.el"
                        "sample.el"
                        "(defun sample () nil)\n"
                        '(:max-depth 2 :min-lines 0))))
          (should (string-match-p
                   "args:<--max-depth><2><--min-lines><0><--format><markdown><--stdin><--language><elisp>"
                   (plist-get result :content)))
          (should (string-match-p "stdin:(defun sample () nil)"
                                  (plist-get result :content)))
          (should (equal (plist-get (plist-get result :metadata) :language)
                         "elisp")))
      (delete-directory directory t))))

(ert-deftest e-resource-toc-test-run-file-calls-wot-with-file-path ()
  "File-backed table-of-content passes a file path to wot."
  (let* ((bin-directory (make-temp-file "e-resource-toc-bin-" t))
         (file-directory (make-temp-file "e-resource-toc-file-" t))
         (_wot (e-resource-toc-test--fake-wot bin-directory))
         (file (expand-file-name "sample.org" file-directory)))
    (unwind-protect
        (progn
          (write-region "* Heading\n" nil file nil 'silent)
          (let* ((exec-path (list bin-directory))
                 (result (e-resource-toc-run-file
                          "tmp://sample.org"
                          file
                          '(:format "json"))))
            (should (string-match-p
                     (regexp-quote (format "args:<--format><json><%s>" file))
                     (plist-get result :content)))
            (should (equal (plist-get (plist-get result :metadata) :format)
                           "json"))))
      (delete-directory bin-directory t)
      (delete-directory file-directory t))))

(provide 'e-resource-toc-test)

;;; e-resource-toc-test.el ends here

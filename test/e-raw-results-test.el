;;; e-raw-results-test.el --- Raw-result cutover tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-raw-results)

(ert-deftest e-raw-results-test-requires-runtime-storage ()
  "Ordinary raw-result operations never fall back to filesystem sidecars."
  (let ((e-raw-results-storage nil))
    (should-error (e-raw-results-write :id "result.txt" :content "value")
                  :type 'e-raw-results-storage-error)
    (should-error (e-raw-results-read "raw-result://result.txt")
                  :type 'e-raw-results-storage-error)))

(ert-deftest e-raw-results-test-file-import-preflights-size-before-read ()
  "An oversized staging file is rejected before reading its contents."
  (let ((source (make-temp-file "e-raw-result-large-")))
    (unwind-protect
        (let ((e-raw-results-max-content-bytes 4))
          (with-temp-file source (insert "12345"))
          (cl-letf (((symbol-function 'insert-file-contents)
                     (lambda (&rest _)
                       (ert-fail "oversized file contents were read"))))
            (should-error (e-raw-results-import-file source)
                          :type 'e-raw-results-too-large)))
      (when (file-exists-p source) (delete-file source)))))

(ert-deftest e-raw-results-test-direct-write-preflights-content-size ()
  "A direct oversized value is rejected before storage submission."
  (let ((e-raw-results-max-content-bytes 4)
        (e-raw-results-storage 'unused))
    (cl-letf (((symbol-function 'e-raw-results-storage-submit)
               (lambda (&rest _)
                 (ert-fail "oversized value reached storage"))))
      (should-error
       (e-raw-results-write :id "result.txt" :content "12345")
       :type 'e-raw-results-too-large))))

(provide 'e-raw-results-test)

;;; e-raw-results-test.el ends here

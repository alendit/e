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

(provide 'e-raw-results-test)

;;; e-raw-results-test.el ends here

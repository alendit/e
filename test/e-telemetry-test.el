;;; e-telemetry-test.el --- Tests for telemetry previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-telemetry)

(ert-deftest e-telemetry-test-preview-redacts-keyed-and-inline-secrets ()
  (let* ((preview (e-telemetry-preview
                   '(:authorization "Bearer abc123"
                     :nested (:password "hunter2")
                     :command "curl -H 'Authorization: Bearer xyz' x")))
         (content (plist-get preview :content)))
    (should (plist-get preview :redacted))
    (should (string-match-p "REDACTED" content))
    (should-not (string-match-p "abc123\\|hunter2\\|xyz" content))
    (should (> (plist-get preview :original-bytes) 0))))

(ert-deftest e-telemetry-test-preview-bounds-redacted-content ()
  (let ((preview (e-telemetry-preview (make-string 100 ?x) 12)))
    (should (plist-get preview :truncated))
    (should (<= (plist-get preview :shown-bytes) 12))))

(provide 'e-telemetry-test)

;;; e-telemetry-test.el ends here

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

(ert-deftest e-telemetry-test-redacts-provider-keys-and-signed-urls ()
  (let* ((github "ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
         (openai "sk-proj-ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
         (aws "AKIAABCDEFGHIJKLMNOP")
         (text (format
                "https://user:password@example.test/path?X-Amz-Credential=%s&X-Amz-Signature=signed-secret github=%s openai=%s"
                aws github openai))
         (redacted (e-telemetry-redact-string text)))
    (should (string-match-p "REDACTED" redacted))
    (dolist (secret (list "password" "signed-secret" github openai aws))
      (should-not (string-match-p (regexp-quote secret) redacted)))))

(ert-deftest e-telemetry-test-preview-bounds-redacted-content ()
  (let ((preview (e-telemetry-preview (make-string 100 ?x) 12)))
    (should (plist-get preview :truncated))
    (should (<= (plist-get preview :shown-bytes) 12))))

(ert-deftest e-telemetry-test-preview-bounds-wide-value ()
  "A wide VALUE must not materialize an unbounded string before truncation.
Tool results are parsed JSON -- large but finite -- so an unbounded printer
here would build a multi-gigabyte string before the 4096-byte truncation."
  (let* ((value (vconcat (mapcar (lambda (i) (format "item-%d" i))
                                 (number-sequence 1 100000))))
         (preview (e-telemetry-preview value 4096)))
    (should (<= (plist-get preview :shown-bytes) 4096))
    (should (plist-get preview :truncated))))

(provide 'e-telemetry-test)

;;; e-telemetry-test.el ends here

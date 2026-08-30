;;; e-context-estimate-test.el --- Direct value-estimate contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The estimator is intentionally tested without loading context-budget or
;; context-lifetime.  Those owners consume this small value contract.

;;; Code:

(require 'ert)
(require 'e-context-estimate)

(ert-deftest e-context-estimate-test-uses-utf8-prin1-and-rounds-up ()
  "The estimator uses canonical printed UTF-8 bytes and upward rounding."
  (let ((bytes (string-bytes (prin1-to-string "é"))))
    (should (= (e-context-budget-value-token-estimate "é" 2.0)
               (ceiling (/ bytes 2.0))))))

(ert-deftest e-context-estimate-test-uses-configured-ratio ()
  "The established setting supplies the ratio when no override is given."
  (let ((e-context-budget-estimate-bytes-per-token 2.0)
        (bytes (string-bytes (prin1-to-string "value"))))
    (should (= (e-context-budget-value-token-estimate "value")
               (ceiling (/ bytes 2.0))))))

(ert-deftest e-context-estimate-test-invalid-ratios-use-four-byte-fallback ()
  "Invalid and non-positive ratios retain the historical 4.0 fallback."
  (let ((bytes (string-bytes (prin1-to-string "é"))))
    (dolist (ratio '(nil 0 -1 "invalid"))
      (should (= (e-context-budget-value-token-estimate "é" ratio)
                 (ceiling (/ bytes 4.0)))))))

(ert-deftest e-context-estimate-test-effective-ratio-is-single-owner-value ()
  "The effective ratio operation exposes one deterministic pure setting."
  (let ((e-context-budget-estimate-bytes-per-token 3.0))
    (should (= (e-context-estimate-effective-bytes-per-token) 3.0))
    (should (= (e-context-estimate-effective-bytes-per-token -2) 4.0))))

(provide 'e-context-estimate-test)

;;; e-context-estimate-test.el ends here

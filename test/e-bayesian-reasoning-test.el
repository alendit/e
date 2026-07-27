;;; e-bayesian-reasoning-test.el --- Tests for Bayesian Reasoning capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the harness-advanced Bayesian Reasoning capability.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-bayesian-reasoning)
(require 'e-harness-advanced)
(require 'e-capabilities)
(require 'e-default-harnesses)
(require 'e-store)

(ert-deftest e-bayesian-reasoning-test-instructions-present ()
  "Instructions carry the compact disposition and point at the reference."
  (should (stringp e-bayesian-reasoning-instructions))
  (should (string-match-p "confidence" e-bayesian-reasoning-instructions))
  (should (string-match-p "insufficient evidence" e-bayesian-reasoning-instructions))
  (should (string-match-p "e://bayesian-reasoning/refs/tenets.md"
                          e-bayesian-reasoning-instructions)))

(ert-deftest e-bayesian-reasoning-test-capability-created ()
  "Capability exposes id, name, and instructions."
  (let ((capability (e-bayesian-reasoning-capability-create)))
    (should (eq (e-capability-id capability) 'bayesian-reasoning))
    (should (equal (e-capability-name capability) "Bayesian Reasoning"))
    (should (equal (e-capability-instructions capability)
                   e-bayesian-reasoning-instructions))))

(ert-deftest e-bayesian-reasoning-test-reference-resource-registered ()
  "The tenets reference registers at the expected URI and is readable."
  (let ((capability (e-bayesian-reasoning-capability-create))
        (store (e-store-create)))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://bayesian-reasoning/refs/tenets.md")))
    (let ((content (e-store-read store "e://bayesian-reasoning/refs/tenets.md" nil)))
      (should (string-match-p "Start from an explicit prior" content))
      (should (string-match-p "Calibrate and state uncertainty honestly" content))
      ;; All eight tenets must be present, not just the first and last.
      (dotimes (n 8)
        (should (string-match-p (format "## %d\\." (1+ n)) content))))))

(ert-deftest e-bayesian-reasoning-test-harness-advanced-includes-capability ()
  "Harness advanced layer includes the Bayesian Reasoning capability."
  (let* ((layer (e-harness-advanced-layer-create))
         (ids (mapcar #'e-capability-id (e-layer-capabilities layer))))
    (should (memq 'bayesian-reasoning ids))
    (should (memq 'goal ids))
    (should (equal (e-layer-requires layer) '(harness-base)))))

(ert-deftest e-bayesian-reasoning-test-default-chat-layer-includes-harness-advanced ()
  "The harness-advanced layer, which packages this capability, loads by default."
  (should (memq 'harness-advanced e-default-chat-layer-ids)))

(provide 'e-bayesian-reasoning-test)

;;; e-bayesian-reasoning-test.el ends here

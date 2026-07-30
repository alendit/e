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
(require 'e-backend)
(require 'e-harness)
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
                          e-bayesian-reasoning-instructions))
  (should (string-match-p "not answer content"
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
                   '("e://bayesian-reasoning/refs/tenets.md"
                     "e://bayesian-reasoning/claim-audits")))
    (let ((content (e-store-read store "e://bayesian-reasoning/refs/tenets.md" nil)))
      (should (string-match-p "Start from an explicit prior" content))
      (should (string-match-p "Calibrate and state uncertainty honestly" content))
      ;; All eight tenets must be present, not just the first and last.
      (dotimes (n 8)
        (should (string-match-p (format "## %d\\." (1+ n)) content)))
      (should (string-match-p
               "No session is selected"
               (e-store-read store "e://bayesian-reasoning/claim-audits" nil))))))

(ert-deftest e-bayesian-reasoning-test-claim-audit-resource-renders-session-records ()
  "The audit resource uses its harness/session registration context."
  (let* ((capability (e-bayesian-reasoning-capability-create))
         (harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "session-1")
    (e-harness-record-hook-audit
     harness "session-1" "turn-1"
     :owner 'bayesian-reasoning
     :hook-id "60-bayesian-reasoning-turn-finished"
     :outcome 'references-resolved
     :details '(:correction none :resolved ((:reference "ev:01KTEST"))))
    (let ((content (e-store-read (e-harness-store harness "session-1")
                                 "e://bayesian-reasoning/claim-audits" nil)))
      (should (string-match-p "outcome: references-resolved" content))
      (should (string-match-p "truth status: not-evaluated" content))
      (should (string-match-p "claims: 0" content))
      (should (string-match-p "ev:01KTEST" content)))))

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

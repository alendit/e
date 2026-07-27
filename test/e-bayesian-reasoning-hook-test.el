;;; e-bayesian-reasoning-hook-test.el --- Tests for the reasoning stop-hook -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for Slice 3: the conditional turn-finished stop-hook and the
;; harness follow-up affordance it depends on.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-bayesian-reasoning)
(require 'e-harness)
(require 'e-backend)

;;;; The harness follow-up affordance

(ert-deftest e-bayesian-reasoning-hook-test-request-follow-up-needs-no-active-turn ()
  "`e-harness-request-follow-up' queues a prompt with no running turn.
This is the core affordance the stop-hook relies on: at turn settlement there
is no active turn, so the guarded `e-harness-queue-prompt' would signal."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    ;; The guarded path refuses without a running turn.
    (should-error (e-harness-queue-prompt harness "session-1" "guarded")
                  :type 'e-harness-no-active-turn)
    ;; The settlement-valid path accepts it.
    (e-harness-request-follow-up harness "session-1" "corrective")
    (should (equal (mapcar (lambda (item) (plist-get item :prompt))
                           (e-harness-queued-prompts harness "session-1"))
                   '("corrective")))))

;;;; Deterministic mark-completeness checks

(ert-deftest e-bayesian-reasoning-hook-test-complete-mark-has-no-gap ()
  "A fully specified mark passes the completeness check."
  (should-not
   (e-bayesian-reasoning--mark-gap
    '(:claim "X" :confidence "high" :alternatives "Y" :evidence "tool:1"))))

(ert-deftest e-bayesian-reasoning-hook-test-missing-confidence-is-a-gap ()
  "A mark without a known confidence band fails."
  (should
   (e-bayesian-reasoning--mark-gap
    '(:claim "X" :confidence "pretty sure" :alternatives "Y" :evidence "e"))))

(ert-deftest e-bayesian-reasoning-hook-test-missing-alternative-is-a-gap ()
  "A mark with no alternative fails."
  (should
   (e-bayesian-reasoning--mark-gap
    '(:claim "X" :confidence "high" :alternatives "" :evidence "e"))))

(ert-deftest e-bayesian-reasoning-hook-test-bare-high-claim-without-evidence-is-a-gap ()
  "A high-confidence claim with a rival but no evidence fails."
  (should
   (e-bayesian-reasoning--mark-gap
    '(:claim "X" :confidence "high" :alternatives "Y" :evidence ""))))

(ert-deftest e-bayesian-reasoning-hook-test-low-confidence-without-evidence-passes ()
  "Low confidence excuses missing evidence: the claim is already hedged."
  (should-not
   (e-bayesian-reasoning--mark-gap
    '(:claim "X" :confidence "low" :alternatives "Y" :evidence ""))))

(ert-deftest e-bayesian-reasoning-hook-test-abstention-passes ()
  "Explicit `insufficient-evidence' abstention passes without evidence."
  (should-not
   (e-bayesian-reasoning--mark-gap
    '(:claim "X" :confidence "high"
      :alternatives "insufficient-evidence" :evidence ""))))

;;;; Turn-level gating

(defun e-bayesian-reasoning-hook-test--context (content &optional metadata)
  "Return a synthetic turn-finished CONTEXT wrapping assistant CONTENT."
  (list :assistant-message (list :role 'assistant
                                 :content content
                                 :metadata metadata)))

(ert-deftest e-bayesian-reasoning-hook-test-trivial-turn-has-no-gap ()
  "A short hedged reply with no concrete assertion does not fire."
  (should-not
   (e-bayesian-reasoning--turn-gap
    (e-bayesian-reasoning-hook-test--context
     "That is possible, but I am not sure and would need to check."))))

(ert-deftest e-bayesian-reasoning-hook-test-unmarked-concrete-turn-fires ()
  "An unmarked reply asserting a concrete specific trips the coarse filter."
  (should
   (e-bayesian-reasoning--turn-gap
    (e-bayesian-reasoning-hook-test--context
     "The regression landed in commit 42 and cut latency by 37%."))))

(ert-deftest e-bayesian-reasoning-hook-test-well-marked-turn-passes ()
  "A concrete reply carrying a complete markdown reasoning mark passes."
  (should-not
   (e-bayesian-reasoning--turn-gap
    (e-bayesian-reasoning-hook-test--context
     (concat "The rollout raised errors by 5%.\n\n"
             "```reasoning\n"
             "claim: the rollout raised errors\n"
             "confidence: high\n"
             "alternatives: coincidental upstream incident\n"
             "evidence: tool:dashboard-1\n"
             "```\n")))))

(ert-deftest e-bayesian-reasoning-hook-test-incomplete-mark-fires ()
  "A concrete reply whose mark omits evidence fails on that gap."
  (should
   (e-bayesian-reasoning--turn-gap
    (e-bayesian-reasoning-hook-test--context
     (concat "The rollout raised errors by 5%.\n\n"
             "```reasoning\n"
             "claim: the rollout raised errors\n"
             "confidence: high\n"
             "alternatives: coincidental upstream incident\n"
             "evidence:\n"
             "```\n")))))

(ert-deftest e-bayesian-reasoning-hook-test-follow-up-turn-never-refires ()
  "A turn tagged as the corrective follow-up is exempt, preventing oscillation.
The exemption lives in the hook, not the gate: a follow-up turn may still show
a gap, but the hook must not queue another follow-up for it."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-bayesian-reasoning--turn-finished-hook
     '(:status done)
     (list :harness harness
           :session-id "session-1"
           :assistant-message
           (list :role 'assistant
                 :content "The regression landed in commit 42."
                 :metadata (list :bayesian-reasoning
                                 e-bayesian-reasoning--follow-up-marker))))
    (should-not (e-harness-queued-prompts harness "session-1"))))

;;;; The hook: requests exactly one follow-up, never rewrites the value

(ert-deftest e-bayesian-reasoning-hook-test-hook-requests-one-follow-up ()
  "On a gated failure the hook queues exactly one follow-up and returns VALUE."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (value '(:status done)))
    (e-harness-create-session harness :id "session-1")
    (let ((result
           (e-bayesian-reasoning--turn-finished-hook
            value
            (list :harness harness
                  :session-id "session-1"
                  :assistant-message
                  (list :role 'assistant
                        :content "The fix shipped in commit 42.")))))
      (should (eq result value))
      (let ((queued (e-harness-queued-prompts harness "session-1")))
        (should (= (length queued) 1))
        (should (equal (plist-get (car queued) :metadata)
                       (list :bayesian-reasoning
                             e-bayesian-reasoning--follow-up-marker)))))))

(ert-deftest e-bayesian-reasoning-hook-test-hook-noop-on-clean-turn ()
  "A trivial turn queues no follow-up."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-bayesian-reasoning--turn-finished-hook
     '(:status done)
     (list :harness harness
           :session-id "session-1"
           :assistant-message
           (list :role 'assistant
                 :content "Maybe; I would need to check before saying.")))
    (should-not (e-harness-queued-prompts harness "session-1"))))

(ert-deftest e-bayesian-reasoning-hook-test-capability-registers-turn-finished-hook ()
  "The capability contributes a turn-finished hook."
  (let* ((capability (e-bayesian-reasoning-capability-create))
         (points (mapcar #'e-hook-point (e-capability-hooks capability))))
    (should (memq :turn-finished points))))

(provide 'e-bayesian-reasoning-hook-test)

;;; e-bayesian-reasoning-hook-test.el ends here

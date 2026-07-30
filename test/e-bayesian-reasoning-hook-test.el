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

(ert-deftest e-bayesian-reasoning-hook-test-low-confidence-without-evidence-is-a-gap ()
  "A hedged factual claim still needs provenance or an abstention."
  (should
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

(ert-deftest e-bayesian-reasoning-hook-test-unmarked-concrete-turn-quiet-by-default ()
  "By default an unmarked concrete reply does not fire.
Ordinary prose is full of numbers, dates, and CamelCase names, so the coarse
bare-assertion tripwire is off unless explicitly enabled -- otherwise every
normal answer would draw a correction."
  (should-not
   (e-bayesian-reasoning--turn-gap
    (e-bayesian-reasoning-hook-test--context
     "The regression landed in commit 42 and cut latency by 37%."))))

(ert-deftest e-bayesian-reasoning-hook-test-bare-assertion-gate-opt-in-fires ()
  "With the bare-assertion gate enabled, an unmarked concrete reply fires."
  (let ((e-bayesian-reasoning-enable-bare-assertion-gate t))
    (should
     (e-bayesian-reasoning--turn-gap
      (e-bayesian-reasoning-hook-test--context
       "The regression landed in commit 42 and cut latency by 37%.")))))

(ert-deftest e-bayesian-reasoning-hook-test-opaque-v1-evidence-is-a-gap ()
  "A historical opaque evidence string cannot pass v2 enforcement."
  (should
   (e-bayesian-reasoning--turn-gap
    (e-bayesian-reasoning-hook-test--context
     (concat "The rollout raised errors by 5%.\n\n"
             "```reasoning\n"
             "claim: the rollout raised errors\n"
             "confidence: high\n"
             "alternatives: coincidental upstream incident\n"
             "evidence: tool:dashboard-1\n"
             "```\n")))))

(ert-deftest e-bayesian-reasoning-hook-test-resolves-earlier-successful-tool-handle ()
  "A v2 `ev:' handle resolves only to an earlier successful tool result."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (session-id "session-1")
         (turn-id "turn-1")
         tool
         assistant)
    (e-harness-create-session harness :id session-id)
    (e-session-append-message
     (e-harness-sessions harness) session-id
     '(:id "01KUSER" :role user :turn-id "turn-1" :content "Why is it slow?"))
    (setq tool
          (e-session-append-message
           (e-harness-sessions harness) session-id
           '(:role tool :turn-id "turn-1"
             :content (:status ok :tool-call-id "call-1" :content "Measured result"))))
    (setq assistant
          (e-session-append-message
           (e-harness-sessions harness) session-id
           (list :id "01KASSIST" :role 'assistant :turn-id turn-id
                 :content
                 (concat "The query is slow.\n\n```reasoning\n"
                         "claim: the query is slow\nconfidence: medium\n"
                         "alternatives: cache miss\n"
                         "evidence: ev:" (plist-get tool :id) "\n```\n"))))
    (let ((check (e-bayesian-reasoning--turn-check
                  (list :harness harness :session-id session-id :turn-id turn-id
                        :assistant-message assistant))))
      (should (eq (plist-get check :outcome) 'references-resolved))
      (should (= (plist-get (plist-get check :details) :claim-count) 1))
      (should (equal (plist-get (car (plist-get (plist-get check :details)
                                                 :resolved))
                                 :source-kind)
                     'tool-result)))
    (let* ((messages (e-bayesian-reasoning--current-turn-evidence-context
                      :harness harness :session-id session-id :turn-id turn-id))
           (content (plist-get (car messages) :content)))
      (should (string-match-p (regexp-quote (concat "ev:" (plist-get tool :id)))
                              content)))))

(ert-deftest e-bayesian-reasoning-hook-test-high-risk-unmarked-turn-is-a-gap ()
  "A diagnostic request requires a mark or explicit abstention."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (session-id "session-1")
         (turn-id "turn-1"))
    (e-harness-create-session harness :id session-id)
    (e-session-append-message
     (e-harness-sessions harness) session-id
     '(:role user :turn-id "turn-1" :content "Why is the query slow?"))
    (let ((assistant
           (e-session-append-message
            (e-harness-sessions harness) session-id
            '(:role assistant :turn-id "turn-1" :content "The query is slow."))))
      (should (equal
               (plist-get
                (e-bayesian-reasoning--turn-check
                 (list :harness harness :session-id session-id :turn-id turn-id
                       :assistant-message assistant))
                :outcome)
               'format-gap)))))

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
  "A turn whose prompt carries the follow-up marker is exempt.
The marker rides the user (prompt) message of the corrective turn, so the
hook must recognize it there and not queue a second follow-up -- otherwise a
repeatedly-uncalibrated model would oscillate.  A concrete assistant reply is
appended for the same turn so the gate itself would otherwise fire."
  (let* ((backend (e-backend-fake-create
                   :items '((:type assistant-message
                             :content "The regression landed in commit 42.")
                            (:type done :reason stop))))
         (harness (e-harness-create :backend backend))
         ;; Enable the bare-assertion gate so the concrete unmarked reply would
         ;; otherwise fire; the point of the test is that the follow-up marker
         ;; suppresses it regardless.
         (e-bayesian-reasoning-enable-bare-assertion-gate t)
         (queued nil))
    (e-harness-create-session harness :id "session-1")
    ;; Run a real turn whose prompt carries the follow-up marker, then fire the
    ;; hook against that turn.
    (e-harness-prompt-batch
     harness "session-1" "corrective"
     :metadata (list :bayesian-reasoning e-bayesian-reasoning--follow-up-marker))
    (let* ((messages (e-harness-messages harness "session-1"))
           (turn-id (plist-get (car (last messages)) :turn-id))
           (assistant (seq-find (lambda (m) (eq (plist-get m :role) 'assistant))
                                messages)))
      (e-bayesian-reasoning--turn-finished-hook
       '(:status done)
       (list :harness harness
             :session-id "session-1"
             :turn-id turn-id
             :assistant-message assistant))
      (setq queued (e-harness-queued-prompts harness "session-1")))
    (should-not queued)))

;;;; The corrective follow-up prompt

(ert-deftest e-bayesian-reasoning-hook-test-follow-up-prompt-improves-not-restates ()
  "The corrective prompt improves the answer instead of replacing it.
The goal is a better answer, not a terser one: the prompt must name the
specific GAP, tell the model to keep the detail the evidence supports, drop or
soften only the unsupported claims, add a complete reasoning block, offer the
`insufficient-evidence' abstention, and forbid both fabrication and discarding
supported detail merely to shorten the reply."
  (let ((prompt (e-bayesian-reasoning--follow-up-prompt "cite the evidence")))
    (should (string-match-p "cite the evidence" prompt))
    (should (string-match-p "keep the detail" prompt))
    (should (string-match-p "unsupported claims" prompt))
    (should (string-match-p "reasoning block" prompt))
    (should (string-match-p "insufficient-evidence" prompt))
    (should (string-match-p "fabricate" prompt))
    (should (string-match-p "supported detail" prompt))))

;;;; The hook: requests exactly one follow-up, never rewrites the value

(defconst e-bayesian-reasoning-hook-test--incomplete-mark-reply
  (concat "The rollout raised errors by 5%.\n\n"
          "```reasoning\n"
          "claim: the rollout raised errors\n"
          "confidence: high\n"
          "alternatives: coincidental upstream incident\n"
          "evidence:\n"
          "```\n")
  "An assistant reply carrying a mark whose evidence field is empty.
This trips gate 1 (the self-emitted mark path), which is on by default, so it
is the reply used to exercise the failure path without the opt-in gate.")

(ert-deftest e-bayesian-reasoning-hook-test-hook-requests-one-hidden-follow-up ()
  "On a gated failure the hook queues exactly one hidden follow-up.
The follow-up prompt rides the metadata `:display' `hidden' channel so the
shell never shows the machine-authored instruction; the hook returns VALUE
unchanged."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
        (value '(:status done)))
    (e-harness-create-session harness :id "session-1")
    (let* ((message (e-harness--append-message
                     harness "session-1" "turn-1"
                     (list :role 'assistant
                           :content
                           e-bayesian-reasoning-hook-test--incomplete-mark-reply)))
           (result
            (e-bayesian-reasoning--turn-finished-hook
             value
             (list :harness harness
                   :session-id "session-1"
                   :turn-id "turn-1"
                   :assistant-message message))))
      (should (eq result value))
      (let ((queued (e-harness-queued-prompts harness "session-1")))
        (should (= (length queued) 1))
        (should (equal (plist-get (car queued) :metadata)
                       (list :bayesian-reasoning
                             e-bayesian-reasoning--follow-up-marker
                             :display 'hidden))))
      (let ((audit (car (e-harness-turn-hook-audits
                         harness "session-1" "turn-1" 'bayesian-reasoning))))
        (should (eq (plist-get (plist-get audit :payload) :outcome) 'format-gap))
        (should (eq (plist-get (plist-get (plist-get audit :payload) :details)
                               :correction)
                    'queued))
        (should (eq (plist-get (plist-get audit :payload) :truth-status)
                    'not-evaluated))))))

(ert-deftest e-bayesian-reasoning-hook-test-hook-hides-superseded-first-attempt ()
  "On a gated failure the hook hides the finished reply it is correcting.
The first attempt stays in the transcript for audit but is flagged hidden so
only the model-authored correction is shown."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (let ((message (e-harness--append-message
                    harness "session-1" "turn-1"
                    (list :role 'assistant
                          :content
                          e-bayesian-reasoning-hook-test--incomplete-mark-reply))))
      (e-bayesian-reasoning--turn-finished-hook
       '(:status done)
       (list :harness harness
             :session-id "session-1"
             :turn-id "turn-1"
             :assistant-message message))
      (should (e-harness-message-hidden-p
               (car (last (e-harness-messages harness "session-1"))))))))

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

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
(require 'e-chat-session)
(require 'e-file-capabilities)
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
    (let ((queued (car (e-harness-queued-prompts harness "session-1"))))
      (should (equal (plist-get queued :prompt) "corrective"))
      (should (eq (plist-get (plist-get queued :metadata) :input-origin)
                  'harness)))))

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

(ert-deftest e-bayesian-reasoning-hook-test-missing-claim-is-a-gap ()
  "A mark must state the substantive claim, not just its other fields."
  (should
   (e-bayesian-reasoning--mark-gap
    '(:claim "" :confidence "high" :alternatives "Y" :evidence "in:01KUSER"))))

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
     '(:id "01KASKED" :role user :turn-id "turn-1" :content "Why is it slow?"))
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

(ert-deftest e-bayesian-reasoning-hook-test-resolves-request-time-source-handle ()
  "A `src:' handle resolves against the exact captured model context."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (session-id "session-1")
         (turn-id "turn-1")
         (source
          (e-context-source-create
           :uri "file:///tmp/spec.org"
           :label "spec.org"
           :content "The feature uses native execution."
           :source-kind 'attachment
           :provider 'chat-session))
         (handle (e-context-source-handle source)))
    (e-harness-create-session harness :id session-id)
    (e-session-append-message
     (e-harness-sessions harness) session-id
     '(:id "01KASKED" :role user :origin human :turn-id "turn-1"
       :content "Summarize the design."))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) session-id
             (list :id "01KASSIST" :role 'assistant :turn-id turn-id
                   :content
                   (concat
                    "It uses native execution.\n\n```reasoning\n"
                    "claim: the feature uses native execution\n"
                    "confidence: high\nalternatives: external execution\n"
                    "evidence: " handle "\n```\n"))))
           (check
            (e-bayesian-reasoning--turn-check
             (list :harness harness :session-id session-id :turn-id turn-id
                   :assistant-message assistant
                   :model-context
                   (list :segments
                         (list
                          (list e-context-evidence-sources-key
                                (list source))))))))
      (should (eq (plist-get check :outcome) 'references-resolved))
      (let ((resolved
             (car (plist-get (plist-get check :details) :resolved))))
        (should (equal (plist-get resolved :reference) handle))
        (should (equal (plist-get resolved :uri) "file:///tmp/spec.org"))
        (should (equal (plist-get resolved :content-sha256)
                       (plist-get source :content-sha256)))))))

(ert-deftest e-bayesian-reasoning-hook-test-harness-prompt-is-not-input-evidence ()
  "A hidden repair prompt cannot validate itself through an `in:' handle."
  (let* ((harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (session-id "session-1")
         (turn-id "turn-2"))
    (e-harness-create-session harness :id session-id)
    (e-session-append-message
     (e-harness-sessions harness) session-id
     '(:id "01KRETRY" :role user :origin harness :turn-id "turn-2"
       :content "Repair the claim."))
    (let* ((assistant
            (e-session-append-message
             (e-harness-sessions harness) session-id
             '(:id "01KASSIST" :role assistant :turn-id "turn-2"
               :content
               "Repaired.\n\n```reasoning\nclaim: repaired\nconfidence: high\nalternatives: not repaired\nevidence: in:01KRETRY\n```\n")))
           (context
            (list :harness harness :session-id session-id :turn-id turn-id
                  :assistant-message assistant))
           (check (e-bayesian-reasoning--turn-check context)))
      (should (eq (plist-get check :outcome) 'evidence-gap))
      (should (eq (plist-get
                   (car (plist-get (plist-get check :details) :rejected))
                   :reason)
                  'wrong-source))
      (should-not
       (e-bayesian-reasoning--current-turn-evidence-context
        :harness harness :session-id session-id :turn-id turn-id)))))

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

(ert-deftest e-bayesian-reasoning-hook-test-follow-up-prompt-carries-originating-handles ()
  "A repair prompt distinguishes a bad citation from a rejected claim."
  (let ((prompt
         (e-bayesian-reasoning--follow-up-prompt
          "replace unresolved evidence"
          '("src:0123456789ABCDEF" "in:01KASKED")
          '((:reference "in:01KBAD" :reason unknown)))))
    (should (string-match-p "claim was not rejected" prompt))
    (should (string-match-p "src:0123456789ABCDEF" prompt))
    (should (string-match-p "in:01KASKED" prompt))
    (should (string-match-p "in:01KBAD" prompt))
    (should (string-match-p "Repair an invalid citation" prompt))))

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
                             :display 'hidden
                             :supersedes-message-id (plist-get message :id)
                             :pending-summary
                             e-bayesian-reasoning--follow-up-pending-summary
                             :input-origin 'harness))))
      (let ((audit (car (e-harness-turn-hook-audits
                         harness "session-1" "turn-1" 'bayesian-reasoning))))
        (should (eq (plist-get (plist-get audit :payload) :outcome) 'format-gap))
        (should (eq (plist-get (plist-get (plist-get audit :payload) :details)
                               :correction)
                    'queued))
        (should (eq (plist-get (plist-get audit :payload) :truth-status)
                    'not-evaluated))))))

(ert-deftest e-bayesian-reasoning-hook-test-invalid-replacement-keeps-original-visible ()
  "An invalid corrective reply is hidden and cannot erase the original."
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
      (should-not (e-harness-message-hidden-p message))
      (let* ((metadata (plist-get
                        (car (e-harness-queued-prompts harness "session-1"))
                        :metadata))
             (_prompt (e-harness--append-message
                       harness "session-1" "turn-2"
                       (list :role 'user :content "corrective" :metadata metadata)))
             (replacement (e-harness--append-message
                           harness "session-1" "turn-2"
                           (list :role 'assistant :content "revised reply"))))
        (e-bayesian-reasoning--turn-finished-hook
         '(:status done)
         (list :harness harness
               :session-id "session-1"
               :turn-id "turn-2"
               :assistant-message replacement))
        (should-not (e-harness-message-hidden-p message))
        (should (e-harness-message-hidden-p replacement))
        (let* ((audits
                (e-harness-turn-hook-audits
                 harness "session-1" "turn-1" 'bayesian-reasoning))
               (payload (plist-get (car (last audits)) :payload)))
          (should (eq (plist-get payload :outcome) 'correction-unresolved))
          (should (eq (plist-get (plist-get payload :details) :correction)
                      'failed)))))))

(ert-deftest e-bayesian-reasoning-hook-test-valid-replacement-hides-original ()
  "A validated corrective reply replaces the original and closes its audit."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-create-session harness :id "session-1")
    (e-harness--append-message
     harness "session-1" "turn-1"
     '(:id "01KASKED" :role user :origin human :content "Why did errors rise?"))
    (let ((message
           (e-harness--append-message
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
      (let* ((metadata (plist-get
                        (car (e-harness-queued-prompts harness "session-1"))
                        :metadata))
             (_prompt
              (e-harness--append-message
               harness "session-1" "turn-2"
               (list :role 'user :origin 'harness
                     :content "corrective" :metadata metadata)))
             (replacement
              (e-harness--append-message
               harness "session-1" "turn-2"
               (list
                :role 'assistant
                :content
                (concat
                 "The measured rise needs another explanation.\n\n"
                 "```reasoning\nclaim: the measured error rise needs another explanation\n"
                 "confidence: medium\nalternatives: rollout caused the rise\n"
                 "evidence: in:01KASKED\n```\n")))))
        (e-bayesian-reasoning--turn-finished-hook
         '(:status done)
         (list :harness harness
               :session-id "session-1"
               :turn-id "turn-2"
               :assistant-message replacement))
        (should (e-harness-message-hidden-p message))
        (should-not (e-harness-message-hidden-p replacement))
        (let* ((audits
                (e-harness-turn-hook-audits
                 harness "session-1" "turn-1" 'bayesian-reasoning))
               (payload (plist-get (car (last audits)) :payload)))
          (should (eq (plist-get payload :outcome) 'references-resolved))
          (should (eq (plist-get (plist-get payload :details) :correction)
                      'completed)))))))

(ert-deftest e-bayesian-reasoning-hook-test-attachment-repair-regression ()
  "An opaque citation cannot turn a source-backed answer into `I don't know'."
  (let* ((calls 0)
         (request-messages nil)
         (backend
          (e-backend-create
           :name "claim-repair-regression"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done
                           on-error on-request-start)
              (ignore options on-error)
              (setq calls (1+ calls))
              (push (copy-tree messages) request-messages)
              (when on-request-start
                (funcall on-request-start (e-backend-request-create)))
              (funcall
               on-item
               (list
                :type 'assistant-message
                :content
                (if (= calls 1)
                    (concat
                     "Release 266 uses native PageRank; later work moves "
                     "execution outside Hyper.\n\n"
                     "```reasoning\n"
                     "claim: Release 266 uses native PageRank before external execution\n"
                     "confidence: high\nalternatives: external execution ships first\n"
                     "evidence: in:01\n```\n")
                  "I don't know.")))
              (funcall on-item '(:type done :reason stop))
              (funcall on-done '(:status done))
              (e-backend-request-create)))))
         (harness (e-harness-create :backend backend)))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-harness-activate-capability
     harness (e-bayesian-reasoning-capability-create))
    (e-harness-create-session harness :id "session-1")
    (with-temp-buffer
      (rename-buffer "e-claim-repair-regression-source" t)
      (insert
       "Release 266 uses native PageRank in Hyper. Later work targets external execution.")
      (e-chat-session-attach-context
       harness "session-1"
       (list :uri (concat "buffer://" (buffer-name))
             :label "graph analytics spec"
             :buffer-name (buffer-name)))
      (e-harness-prompt-batch
       harness "session-1" "What does the graph analytics spec propose?")
      (let ((deadline (+ (float-time) 1)))
        (while (and (< calls 2) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (= calls 2))
      (let* ((messages (e-harness-messages harness "session-1"))
             (assistants
              (seq-filter
               (lambda (message)
                 (eq (plist-get message :role) 'assistant))
               messages))
             (repair-prompt
              (seq-find
               (lambda (message)
                 (eq (e-bayesian-reasoning--message-input-origin message)
                     'harness))
               messages))
             (original (car assistants))
             (replacement (cadr assistants))
             (first-request (car (last request-messages)))
             (source-message
              (seq-find
               (lambda (message)
                 (and (eq (plist-get message :role) 'system)
                      (string-match-p "<attachment"
                                      (plist-get message :content))))
               first-request)))
        (should source-message)
        (should (string-match-p "evidence=\"src:[0-9A-F]\\{16\\}\""
                                (plist-get source-message :content)))
        (should (string-match-p "src:[0-9A-F]\\{16\\}"
                                (plist-get repair-prompt :content)))
        (should-not (e-harness-message-hidden-p original))
        (should (e-harness-message-hidden-p replacement))
        (let* ((audits
                (e-harness-turn-hook-audits
                 harness "session-1" (plist-get original :turn-id)
                 'bayesian-reasoning))
               (payload (plist-get (car (last audits)) :payload)))
          (should (eq (plist-get payload :outcome)
                      'correction-unresolved)))))))

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

(ert-deftest e-bayesian-reasoning-hook-test-strips-blocks-from-write-content ()
  "The pre-tool boundary removes transcript metadata from complete writes."
  (let* ((block
          "#+begin_reasoning\nclaim: internal\nconfidence: high\nalternatives: none\nevidence: in:01KASKED\n#+end_reasoning")
         (prepared
          (e-bayesian-reasoning--sanitize-artifact-tool-call
           (list :id "call-1" :name "write"
                 :arguments (list :uri "file://plan.org"
                                  :content (concat "Before\n\n" block
                                                   "\n\nAfter\n")))
           nil))
         (content (plist-get (plist-get prepared :arguments) :content)))
    (should-not (string-match-p "begin_reasoning" content))
    (should-not (string-match-p "claim: internal" content))
    (should (string-match-p "Before" content))
    (should (string-match-p "After" content))))

(ert-deftest e-bayesian-reasoning-hook-test-edit-preserves-old-block-to-remove-it ()
  "Edit cleanup can match an old block but cannot add one in replacement text."
  (let* ((block
          "```reasoning\nclaim: internal\nconfidence: high\nalternatives: none\nevidence: in:01KASKED\n```")
         (prepared
          (e-bayesian-reasoning--sanitize-artifact-tool-call
           (list :id "call-1" :name "edit"
                 :arguments
                 (list :uri "file://plan.org"
                       :edits
                       (list
                        (list :oldText block
                              :newText (concat "Useful prose.\n" block)))))
           nil))
         (edit (car (plist-get (plist-get prepared :arguments) :edits))))
    (should (equal (plist-get edit :oldText) block))
    (should (equal (plist-get edit :newText) "Useful prose.\n"))))

(ert-deftest e-bayesian-reasoning-hook-test-leaves-non-mutating-call-unchanged ()
  "The artifact boundary does not rewrite unrelated or non-mutating calls."
  (let ((call
         (list :id "call-1" :name "read"
               :arguments
               (list :uri "file://plan.org"
                     :query "#+begin_reasoning"))))
    (should
     (equal (e-bayesian-reasoning--sanitize-artifact-tool-call call nil)
            call))))

(ert-deftest e-bayesian-reasoning-hook-test-lifecycle-sanitizes-artifact-edit ()
  "The registered Bayesian pre-tool hook enforces the artifact boundary."
  (let* ((harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :intrinsic-capabilities
           (list (e-bayesian-reasoning-capability-create))))
         (block
          "#+begin_reasoning\nclaim: internal\nconfidence: high\nalternatives: none\nevidence: in:01KASKED\n#+end_reasoning"))
    (e-harness-create-session harness :id "session-1")
    (let* ((lifecycle (e-harness-tool-lifecycle
                       harness "session-1" "turn-1"))
           (prepared
            (e-tool-lifecycle-prepare-call
             lifecycle
             (list :id "call-1" :name "edit"
                   :arguments
                   (list :uri "buffer://plan.org"
                         :edits
                         (list (list :oldText "Old"
                                     :newText (concat "New\n" block)))))))
           (new-text
            (plist-get
             (car (plist-get (plist-get prepared :arguments) :edits))
             :newText)))
      (should (equal new-text "New\n")))))

(ert-deftest e-bayesian-reasoning-hook-test-resource-edit-cannot-persist-block ()
  "A real resource edit receives cleaned replacement content."
  (let* ((directory (make-temp-file "e-reasoning-artifact-" t))
         (file (expand-file-name "plan.org" directory))
         (harness
          (e-harness-create
           :backend (e-backend-fake-create :items nil)
           :intrinsic-capabilities
           (list (e-file-handling-capability-create directory)
                 (e-bayesian-reasoning-capability-create))))
         (block
          "#+begin_reasoning\nclaim: internal\nconfidence: high\nalternatives: none\nevidence: in:01KASKED\n#+end_reasoning")
         done result failure)
    (unwind-protect
        (progn
          (write-region "Old\n" nil file nil 'silent)
          (e-harness-create-session harness :id "session-1")
          (let* ((lifecycle (e-harness-tool-lifecycle
                             harness "session-1" "turn-1"))
                 (prepared
                  (e-tool-lifecycle-prepare-call
                   lifecycle
                   (list :id "call-1" :name "edit"
                         :arguments
                         (list :uri (concat "file://" file)
                               ;; Reproduce the provider's accepted one-edit
                               ;; object shape from the reported session.
                               :edits
                               (list :oldText "Old\n"
                                     :newText (concat "New\n" block)))))))
            (e-tool-lifecycle-start-call
             lifecycle prepared
             :on-done (lambda (value) (setq result value done t))
             :on-error (lambda (err) (setq failure err done t))))
          (let ((deadline (+ (float-time) 1)))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (when failure
            (signal (car failure) (cdr failure)))
          (should done)
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "New\n"))))
      (delete-directory directory t))))

(ert-deftest e-bayesian-reasoning-hook-test-capability-registers-turn-finished-hook ()
  "The capability contributes transcript checking and artifact-boundary hooks."
  (let* ((capability (e-bayesian-reasoning-capability-create))
         (points (mapcar #'e-hook-point (e-capability-hooks capability))))
    (should (memq :turn-finished points))
    (should (memq :pre-tool-call points))))

(provide 'e-bayesian-reasoning-hook-test)

;;; e-bayesian-reasoning-hook-test.el ends here

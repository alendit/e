;;; e-context-budget-test.el --- Tests for core context budget -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for UI-free context budget accounting.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-context-budget)
(require 'e-harness)
(require 'e-session)

(ert-deftest e-context-budget-test-value-estimate-rounds-up-and-falls-back ()
  "Value estimates use UTF-8 bytes, upward rounding, and the 4.0 fallback."
  (let ((bytes (string-bytes (prin1-to-string "é"))))
    (should (= (e-context-budget-value-token-estimate "é" 2.0)
               (ceiling (/ bytes 2.0))))
    (should (= (e-context-budget-value-token-estimate "é" 0)
               (ceiling (/ bytes 4.0))))
    (should (= (e-context-budget-value-token-estimate "é" "invalid")
               (ceiling (/ bytes 4.0))))))

(ert-deftest e-context-budget-test-model-window-uses-table ()
  "Model windows are read from the supplied or default budget table."
  (should (equal (e-context-budget-model-window "claude-sonnet-5") 364000))
  (dolist (model '("gpt-5.6"
                   "gpt-5.6-sol"
                   "gpt-5.6-terra"
                   "gpt-5.6-luna"))
    (should (equal (e-context-budget-model-window model) 353400)))
  (should (equal (e-context-budget-model-window "gpt-5.5") 258400))
  (should (equal (e-context-budget-model-window
                  "custom" '(("custom" . 1234)))
                 1234))
  (should-not (e-context-budget-model-window "missing" '())))

(ert-deftest e-context-budget-test-sonnet-5-window-enables-auto-compaction ()
  "Sonnet 5 usage can cross the manually configured compaction threshold."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :default-options '(:model "claude-sonnet-5")))
         (store (e-harness-sessions harness))
         (e-harness-auto-compaction-reserve-tokens 16384))
    (e-session-create store :id "sonnet-5-budget")
    (e-session-append-message
     store "sonnet-5-budget" '(:role user :content "large context"))
    (e-session-append-activity-event
     store "sonnet-5-budget" "turn-1" 'token-usage
     '(:input-tokens 350000 :total-tokens 350100))
    (let ((status (e-context-budget-status
                   harness "sonnet-5-budget"
                   :prefer-token-usage t
                   :estimate-context nil)))
      (should (> (plist-get status :used-tokens)
                 (- (plist-get status :window)
                    e-harness-auto-compaction-reserve-tokens))))))

(ert-deftest e-context-budget-test-gpt-56-sol-window-enables-auto-compaction ()
  "GPT-5.6 Sol provider usage can cross the auto-compaction threshold."
  (let* ((harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :default-options '(:model "gpt-5.6-sol")))
         (store (e-harness-sessions harness))
         (e-harness-auto-compaction-reserve-tokens 16384))
    (e-session-create store :id "gpt-56-budget")
    (e-session-append-message
     store "gpt-56-budget" '(:role user :content "large context"))
    (e-session-append-activity-event
     store "gpt-56-budget" "turn-1" 'token-usage
     '(:input-tokens 340000 :total-tokens 340100))
    (let ((status (e-context-budget-status
                   harness "gpt-56-budget"
                   :prefer-token-usage t
                   :estimate-context nil)))
      (should (> (plist-get status :used-tokens)
                 (- (plist-get status :window)
                    e-harness-auto-compaction-reserve-tokens))))))

(ert-deftest e-context-budget-test-used-tokens-prefers-fresh-provider-usage ()
  "Fresh provider token usage is used before estimating context."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (e-session-create store :id "budget-usage")
    (e-session-append-message
     store "budget-usage" '(:role user :content "context question"))
    (e-session-append-activity-event
     store "budget-usage" "turn-1" 'token-usage
     '(:input-tokens 202598 :total-tokens 203017))
    (should (equal (e-context-budget-used-tokens harness "budget-usage")
                   202598))))

(ert-deftest e-context-budget-test-used-tokens-ignores-usage-before-compaction ()
  "Provider usage before the latest valid compaction is treated as stale."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (e-session-create store :id "budget-compacted")
    (let ((kept (e-session-append-message
                 store "budget-compacted"
                 '(:role user :content "kept suffix"))))
      (e-session-append-activity-event
       store "budget-compacted" "turn-1" 'token-usage
       '(:input-tokens 202598 :total-tokens 203017))
      (e-session-append-compaction
       store "budget-compacted" "Summary"
       :first-kept-entry-id (plist-get kept :id))
      (let ((status (e-context-budget-status
                     harness "budget-compacted"
                     :token-limits '(("gpt-5.5" . 100))
                     :bytes-per-token 1.0)))
        (should (integerp (plist-get status :used-tokens)))
        (should-not (equal (plist-get status :used-tokens) 202598))
        (should (plist-get status :approximate))))))

(ert-deftest e-context-budget-test-status-includes_model_effort_window ()
  "Budget status reports model, effort, used tokens, and window."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (e-session-create store :id "budget-status")
    (e-session-append-message
     store "budget-status" '(:role user :content "context question"))
    (let ((status (e-context-budget-status
                   harness "budget-status"
                   :token-limits '(("gpt-5.5" . 100))
                   :bytes-per-token 1.0)))
      (should (equal (plist-get status :model) "gpt-5.5"))
      (should (equal (plist-get status :reasoning-effort) "high"))
      (should (equal (plist-get status :window) 100))
      (should (integerp (plist-get status :used-tokens)))
      (should (plist-get status :approximate)))))

(ert-deftest e-context-budget-test-fresh-estimate-cache-skips-context ()
  "Fresh estimate cache hits avoid materializing full harness context."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         (cache (cons 321 (float-time)))
         (context-calls 0))
    (e-session-create store :id "budget-cache")
    (cl-letf (((symbol-function 'e-harness-context)
               (lambda (&rest _args)
                 (setq context-calls (1+ context-calls))
                 (error "fresh estimate cache should skip context"))))
      (let ((status (e-context-budget-status
                     harness "budget-cache"
                     :estimate-cache cache
                     :estimate-cache-seconds 100
                     :token-limits '(("gpt-5.5" . 1000)))))
        (should (equal (plist-get status :used-tokens) 321))
        (should (plist-get status :approximate)))
      (should (equal (e-context-budget-used-tokens
                      harness "budget-cache"
                      :estimate-cache cache
                      :estimate-cache-seconds 100)
                     321))
      (should (= context-calls 0)))))

(ert-deftest e-context-budget-test-estimate-context-passes_context_purpose ()
  "Budget estimates pass optional context purpose to harness context."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high")))
         seen-purpose)
    (e-session-create store :id "budget-purpose")
    (cl-letf (((symbol-function 'e-harness-context)
               (lambda (_harness _session-id _turn-id context-purpose)
                 (setq seen-purpose context-purpose)
                 '(:messages ((:role user :content "purpose context"))
                   :options (:model "gpt-5.5" :reasoning-effort "high")))))
      (let ((status (e-context-budget-status
                     harness "budget-purpose"
                     :context-purpose 'status
                     :token-limits '(("gpt-5.5" . 1000)))))
        (should (eq seen-purpose 'status))
        (should (plist-get status :approximate))))))

(ert-deftest e-context-budget-test-estimate-cache-key-mismatch-recomputes ()
  "Estimate cache keys force recomputation even when TTL is fresh."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options '(:model "gpt-5.5")))
         (cache (cons nil nil))
         (calls 0)
         (context-calls 0))
    (e-session-create store :id "budget-key-cache")
    (e-session-append-message
     store "budget-key-cache" '(:role user :content "first"))
    (cl-letf* ((orig (symbol-function 'e-context-budget-context-token-estimate))
               ((symbol-function 'e-context-budget-context-token-estimate)
                (lambda (&rest args)
                  (setq calls (1+ calls))
                  (apply orig args)))
               (orig-context (symbol-function 'e-harness-context))
               ((symbol-function 'e-harness-context)
                (lambda (&rest args)
                  (setq context-calls (1+ context-calls))
                  (apply orig-context args))))
      (should (integerp
               (e-context-budget-used-tokens
                harness "budget-key-cache"
                :estimate-cache cache
                :estimate-cache-seconds 100
                :estimate-cache-key '(:messages 1))))
      (should (integerp
               (e-context-budget-used-tokens
                harness "budget-key-cache"
                :estimate-cache cache
                :estimate-cache-seconds 100
                :estimate-cache-key '(:messages 1))))
      (e-session-append-message
       store "budget-key-cache" '(:role assistant :content "second"))
      (should (integerp
               (e-context-budget-used-tokens
                harness "budget-key-cache"
                :estimate-cache cache
                :estimate-cache-seconds 100
                :estimate-cache-key '(:messages 2)))))
    (should (equal calls 2))
    (should (equal context-calls 2))))

(provide 'e-context-budget-test)

;;; e-context-budget-test.el ends here

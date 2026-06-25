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

(ert-deftest e-context-budget-test-model-window-uses-table ()
  "Model windows are read from the supplied or default budget table."
  (should (equal (e-context-budget-model-window "gpt-5.5") 258400))
  (should (equal (e-context-budget-model-window
                  "custom" '(("custom" . 1234)))
                 1234))
  (should-not (e-context-budget-model-window "missing" '())))

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

(provide 'e-context-budget-test)

;;; e-context-budget-test.el ends here

;;; e-context-status-test.el --- Tests for shared context status -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the shell-agnostic context-state status computation.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-context-status)
(require 'e-dev-profile)
(require 'e-events)
(require 'e-harness)
(require 'e-session)

(ert-deftest e-context-status-test-format-prefix-and-usage ()
  "The formatter renders prefix, model, effort, and token usage."
  (should (equal (e-context-status-format
                  "Org Canvas" "gpt-5.5" "high" 18000 400000 t)
                 "Org Canvas gpt-5.5/high ~5% (~18k/400k tok)"))
  (should (equal (e-context-status-format
                  "e-chat" "gpt-5.5" "high" 40000 258400 nil)
                 "e-chat gpt-5.5/high 15% (40k/258k tok)"))
  (should (equal (e-context-status-format "Org Canvas" nil nil nil nil)
                 "Org Canvas model unset/effort unset")))

(ert-deftest e-context-status-test-model-token-limit-uses-table ()
  "Model limits are read from the supplied or default table."
  (should (equal (e-context-status-model-token-limit "gpt-5.5") 258400))
  (should (equal (e-context-status-model-token-limit
                  "custom" '(("custom" . 1234)))
                 1234))
  (should-not (e-context-status-model-token-limit "unknown" '())))

(ert-deftest e-context-status-test-text-without-session-returns-prefix ()
  "With no harness/session, the status text is just the prefix."
  (should (equal (e-context-status-text nil nil :prefix "Org Canvas")
                 "Org Canvas")))

(ert-deftest e-context-status-test-text-missing-session-returns-prefix ()
  "A missing session id does not render unset model/effort placeholders."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (should (equal (e-context-status-text harness "missing"
                                          :prefix "Org Canvas")
                   "Org Canvas"))))

(ert-deftest e-context-status-test-text-estimates-context ()
  "Without provider usage, the status text estimates context tokens."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (e-session-create store :id "status-estimate")
    (e-session-append-message
     store "status-estimate" '(:role user :content "context question"))
    (let ((text (e-context-status-text
                 harness "status-estimate"
                 :prefix "Org Canvas"
                 :token-limits '(("gpt-5.5" . 100))
                 :bytes-per-token 1.0)))
      (should (string-match-p "\\`Org Canvas gpt-5.5/high " text))
      (should (string-match-p "~[0-9]+%" text))
      (should (string-match-p "/100 tok" text)))))

(ert-deftest e-context-status-test-text-prefers-provider-usage ()
  "Fresh provider token usage is used before estimating context."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (e-session-create store :id "status-usage")
    (e-session-append-activity-event
     store "status-usage" "turn-1" 'token-usage
     '(:input-tokens 202598 :total-tokens 203017))
    (should (equal (e-context-status-text harness "status-usage" :prefix "e-chat")
                   "e-chat gpt-5.5/high 78% (203k/258k tok)"))))

(ert-deftest e-context-status-test-provider-usage-does-not-scan-activity-events ()
  "Fresh provider usage is read from session-derived state."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (e-session-create store :id "status-usage-fast")
    (e-session-append-activity-event
     store "status-usage-fast" "turn-1" 'token-usage
     '(:input-tokens 202598 :total-tokens 203017))
    (cl-letf (((symbol-function 'e-harness-session-activity-events)
               (lambda (&rest _args)
                 (error "activity scan should be skipped"))))
      (should (equal (e-context-status-text
                      harness "status-usage-fast" :prefix "e-chat")
                     "e-chat gpt-5.5/high 78% (203k/258k tok)")))))

(ert-deftest e-context-status-test-text-ignores-usage-before-compaction ()
  "Provider usage before the latest valid compaction is treated as stale."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options
                   '(:model "gpt-5.5" :reasoning-effort "high"))))
    (e-session-create store :id "status-compacted")
    (let ((kept (e-session-append-message
                 store "status-compacted"
                 '(:role user :content "small kept context"))))
      (e-session-append-activity-event
       store "status-compacted" "turn-1" 'token-usage
       '(:input-tokens 202598 :total-tokens 203017))
      (e-session-append-compaction
       store "status-compacted" "Summary"
       :first-kept-entry-id (plist-get kept :id))
      (let ((text (e-context-status-text
                   harness "status-compacted"
                   :prefix "e-chat"
                   :token-limits '(("gpt-5.5" . 100))
                   :bytes-per-token 1.0)))
        (should (string-match-p "\\`e-chat gpt-5.5/high " text))
        (should (string-match-p "~" text))
        (should-not (string-match-p "203k" text))))))

(ert-deftest e-context-status-test-estimate-cache-reuses-value ()
  "A caller-owned cache cell reuses the last estimate within the TTL."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options '(:model "gpt-5.5")))
         (cache (cons nil nil))
         (calls 0)
         (context-calls 0))
    (e-session-create store :id "status-cache")
    (e-session-append-message
     store "status-cache" '(:role user :content "first"))
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
      (let ((e-context-status-estimate-cache-seconds 100))
        (e-context-status-text harness "status-cache"
                               :prefix "e-chat" :estimate-cache cache)
        (e-context-status-text harness "status-cache"
                               :prefix "e-chat" :estimate-cache cache)))
    (should (equal calls 1))
    (should (equal context-calls 1))))

(ert-deftest e-context-status-test-estimate-cache-key-mismatch-recomputes ()
  "Status estimate cache keys force recomputation before TTL expiry."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options '(:model "gpt-5.5")))
         (cache (cons nil nil))
         (calls 0)
         (context-calls 0))
    (e-session-create store :id "status-key-cache")
    (e-session-append-message
     store "status-key-cache" '(:role user :content "first"))
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
      (let ((e-context-status-estimate-cache-seconds 100))
        (e-context-status-text harness "status-key-cache"
                               :prefix "e-chat"
                               :estimate-cache cache
                               :estimate-cache-key '(:messages 1))
        (e-context-status-text harness "status-key-cache"
                               :prefix "e-chat"
                               :estimate-cache cache
                               :estimate-cache-key '(:messages 1))
        (e-session-append-message
         store "status-key-cache" '(:role assistant :content "second"))
        (e-context-status-text harness "status-key-cache"
                               :prefix "e-chat"
                               :estimate-cache cache
                               :estimate-cache-key '(:messages 2))))
    (should (equal calls 2))
    (should (equal context-calls 2))))

(ert-deftest e-context-status-test-snapshot-cache-reuses-text ()
  "A caller-owned status snapshot cache skips rebuilding budget status."
  (let ((cache (cons nil nil))
        (calls 0))
    (cl-letf (((symbol-function 'e-context-budget-status)
               (lambda (&rest _args)
                 (setq calls (1+ calls))
                 '(:model "gpt-5.5"
                   :reasoning-effort "high"
                   :used-tokens 123
                   :window 1000
                   :approximate t))))
      (let ((e-context-status-estimate-cache-seconds 100))
        (should (equal
                 (e-context-status-text
                  'harness "snapshot"
                 :prefix "e-chat"
                 :snapshot-cache cache
                 :snapshot-cache-key '(:state 1))
                 "e-chat gpt-5.5/high ~13% (~123/1k tok)"))
        (should (equal
                 (e-context-status-text
                  'harness "snapshot"
                  :prefix "e-chat"
                  :snapshot-cache cache
                  :snapshot-cache-key '(:state 1))
                 "e-chat gpt-5.5/high ~13% (~123/1k tok)"))))
    (should (= calls 1))))

(ert-deftest e-context-status-test-profile-records-status-text ()
  "Enabled dev profiling records context status text computation."
  (let* ((profile-directory (make-temp-file "e-context-status-profile-" t))
         (e-dev-profile-directory profile-directory)
         (e-dev-profile--enabled nil)
         (e-dev-profile--current-file nil)
         (e-dev-profile--latest-file nil)
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store
                   :default-options '(:model "gpt-5.5"))))
    (unwind-protect
        (progn
          (e-session-create store :id "status-profile")
          (e-dev-profile-start)
          (e-context-status-text harness "status-profile"
                                 :prefix "e-chat"
                                 :estimate-context nil)
          (e-dev-profile-stop)
          (let* ((report (e-dev-profile-report-data e-dev-profile--latest-file))
                 (aggregates (plist-get report :aggregates)))
            (should (alist-get "context-status.text" aggregates nil nil #'equal))))
      (delete-directory profile-directory t))))

(provide 'e-context-status-test)

;;; e-context-status-test.el ends here

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
         (calls 0))
    (e-session-create store :id "status-cache")
    (e-session-append-message
     store "status-cache" '(:role user :content "first"))
    (cl-letf* ((orig (symbol-function 'e-context-status-context-token-estimate))
               ((symbol-function 'e-context-status-context-token-estimate)
                (lambda (&rest args)
                  (setq calls (1+ calls))
                  (apply orig args))))
      (let ((e-context-status-estimate-cache-seconds 100))
        (e-context-status-text harness "status-cache"
                               :prefix "e-chat" :estimate-cache cache)
        (e-context-status-text harness "status-cache"
                               :prefix "e-chat" :estimate-cache cache)))
    (should (equal calls 1))))

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

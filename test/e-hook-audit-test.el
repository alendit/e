;;; e-hook-audit-test.el --- Tests for durable generic hook audits -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Proves the core audit envelope without importing any capability policy.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-session)

(ert-deftest e-hook-audit-test-persists-and-reloads ()
  "A generic hook audit survives session-store reload with its opaque details."
  (let* ((directory (make-temp-file "e-hook-audit-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create :backend (e-backend-fake-create :items nil)
                                    :sessions store))
         (session-id "session-1"))
    (unwind-protect
        (progn
          (e-harness-create-session harness :id session-id)
          (e-harness-record-hook-audit
           harness session-id "turn-1"
           :owner 'test-capability :hook-id "test-hook"
           :outcome 'references-resolved :summary "References resolved"
           :details '(:reference "ev:01KTEST"))
          (e-session-flush-write-queue store)
          (let* ((reloaded-harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions (e-session-persistent-store-create directory)))
                 (audit (car (e-harness-turn-hook-audits
                              reloaded-harness session-id "turn-1" 'test-capability))))
            (should (eq (plist-get audit :event-type) 'hook-audit))
            (should (equal (plist-get (plist-get audit :payload) :summary)
                           "References resolved"))
            (should (equal (plist-get (plist-get audit :payload) :details)
                           '(:reference "ev:01KTEST")))
            (should (eq (plist-get (plist-get audit :payload) :truth-status)
                        'not-evaluated))))
      (delete-directory directory t))))

(provide 'e-hook-audit-test)

;;; e-hook-audit-test.el ends here

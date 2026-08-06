;;; e-message-details-test.el --- Tests for capability message details -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Focused tests for the shell-neutral message-details contribution contract.

;;; Code:

(require 'ert)
(require 'e-message-details)

(ert-deftest e-message-details-test-collects-provider-contributions-in-order ()
  "Providers contribute validated details without shell-specific knowledge."
  (let* ((first (e-message-detail-create
                 :id 'claims :summary "1 claim" :body "Claims\n- one"))
         (second (e-message-detail-create
                  :id 'sources :summary "1 source" :body "Sources\n- src:1"))
         (message '(:role assistant :content "answer"))
         (context '(:session-id "session")))
    (should (equal
             (e-message-details-collect
              (list (lambda (actual-message actual-context)
                      (should (eq actual-message message))
                      (should (eq actual-context context))
                      first)
                    (lambda (_message _context) (list second)))
              message context)
             (list first second)))))

(ert-deftest e-message-details-test-rejects-invalid-provider-output ()
  "Malformed semantic output surfaces at the capability boundary."
  (should-error
   (e-message-details-collect
    (list (lambda (_message _context) '(:summary "not a record")))
    '(:role assistant) nil)
   :type 'wrong-type-argument))

(provide 'e-message-details-test)

;;; e-message-details-test.el ends here

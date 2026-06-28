;;; e-request-test.el --- Tests for e request lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Deterministic tests for the request/job lifecycle contract used by slow
;; interactive operations.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-request)

(ert-deftest e-request-test-terminal-settlement-happens-once ()
  "A request records exactly one terminal state."
  (let ((request (e-request-lifecycle-create
                  :id "req-1"
                  :owner 'test
                  :generation 1)))
    (should (eq (e-request-lifecycle-state request) 'created))
    (should (e-request-start request '(:phase started)))
    (should (eq (e-request-lifecycle-state request) 'started))
    (should (e-request-progress request '(:items 1)))
    (should (eq (e-request-lifecycle-state request) 'progress))
    (should (e-request-finish request '(:ok t)))
    (should (eq (e-request-lifecycle-state request) 'finished))
    (should-not (e-request-fail request '(:late t)))
    (should (eq (e-request-lifecycle-state request) 'finished))
    (should (equal (e-request-lifecycle-terminal-payload request)
                   '(:ok t)))
    (should (= (length (seq-filter
                        (lambda (event)
                          (memq (plist-get event :state)
                                e-request-terminal-states))
                        (e-request-lifecycle-events request)))
               1))
    (should (equal (plist-get (car (last (e-request-lifecycle-diagnostics request)))
                              :kind)
                   'late-terminal))))

(ert-deftest e-request-test-cancellation-settles-and-calls-cancel-function ()
  "Cancellation invokes the underlying cancel function and settles visibly."
  (let ((cancelled nil)
        (cleaned nil))
    (let ((request (e-request-lifecycle-create
                    :id "req-2"
                    :owner 'test
                    :cancel-function (lambda (_request)
                                       (setq cancelled t))
                    :cleanup-trigger (lambda (_request)
                                       (setq cleaned t)))))
      (should (e-request-cancel request '(:reason user)))
      (should cancelled)
      (should cleaned)
      (should (eq (e-request-lifecycle-state request) 'cancelled))
      (should (equal (e-request-lifecycle-terminal-payload request)
                     '(:reason user)))
      (should-not (e-request-progress request '(:late t)))
      (should (eq (plist-get (car (last (e-request-lifecycle-diagnostics request)))
                             :kind)
                  'late-progress)))))

(ert-deftest e-request-test-generation-staleness-is-explicit ()
  "Generation checks are explicit and deterministic."
  (let ((request (e-request-lifecycle-create
                  :id "req-3"
                  :owner 'test
                  :generation 7)))
    (should-not (e-request-stale-generation-p request 7))
    (should (e-request-stale-generation-p request 8))
    (should-not (e-request-stale-generation-p request nil))))

(ert-deftest e-request-test-hot-path-blocking-guard-rejects-known-waits ()
  "Marked hot paths can reject known blocking primitives in tests."
  (should-error
   (e-request-with-blocking-primitive-guard
     (e-request-with-hot-path 'interactive-submit
       (process-file "true")))
   :type 'e-request-blocking-call-in-hot-path)
  (should-error
   (e-request-with-blocking-primitive-guard
     (e-request-with-hot-path 'interactive-submit
       (accept-process-output nil 0.01)))
   :type 'e-request-blocking-call-in-hot-path)
  (e-request-with-blocking-primitive-guard
    (should (= (process-file "true") 0))))

(ert-deftest e-request-test-controlled-delayed-callback-contract ()
  "A controlled delayed operation proves start returns before callback release."
  (let ((release nil)
        (result nil)
        (request (e-request-lifecycle-create
                  :id "req-4"
                  :owner 'test)))
    (e-request-start request)
    (run-at-time 0 nil
                 (lambda ()
                   (when release
                     (setq result (e-request-finish request '(:done t))))))
    (should-not result)
    (setq release t)
    (accept-process-output nil 0.05)
    (should result)
    (should (eq (e-request-lifecycle-state request) 'finished))))

(provide 'e-request-test)

;;; e-request-test.el ends here

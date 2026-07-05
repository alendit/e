;;; e-usage-report-test.el --- Tests for durable activity usage reports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for tool/action usage aggregation.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-tools)
(require 'e-usage-report)

(defun e-usage-report-test--tool-provider (registry)
  "Register sample usage test tools in REGISTRY."
  (e-tools-register
   registry
   :name "sample_tool"
   :description "Sample counted tool."
   :parameters '(:type "object" :properties nil)
   :handler (lambda (_arguments) "ok"))
  (e-tools-register
   registry
   :name "idle_tool"
   :description "Sample zero-use tool."
   :parameters '(:type "object" :properties nil)
   :handler (lambda (_arguments) "idle")))

(defun e-usage-report-test--capability ()
  "Return a capability with tools and actions for usage report tests."
  (e-capability-create
   :id 'usage-sample
   :name "Usage Sample"
   :tools (list #'e-usage-report-test--tool-provider)
   :actions
	   (list :run
	         (e-action-cheap-create
	          :runner (lambda (_arguments _context) 'ran)
	          :description "Sample counted action.")
	         :idle
	         (e-action-cheap-create
	          :runner (lambda (_arguments _context) 'idle)
	          :description "Sample zero-use action."))))

(ert-deftest e-usage-report-test-splits-tools-and-actions ()
  "Usage reports aggregate tool and action starts in separate sections."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-usage-report-test--capability))
    (e-harness-create-session harness :id "session-1")
    (e-harness--emit-turn-event
     harness "session-1" "turn-1" 'tool-started
     '(:id "tool-call-1" :name "sample_tool"))
    (e-actions-call
     'usage-sample :run nil
     (list :harness harness :session-id "session-1" :turn-id "turn-1"))
    (let ((report (e-usage-report-session harness "session-1")))
      (should (equal (plist-get report :tools)
                     '((:name "sample_tool" :count 1))))
      (should (equal (plist-get report :actions)
                     '((:name "usage-sample/run" :count 1)))))))

(ert-deftest e-usage-report-test-full-surface-includes-zero-rows ()
  "Full-surface reports include active tools and actions with zero counts."
  (let ((harness (e-harness-create :backend (e-backend-fake-create :items nil))))
    (e-harness-activate-capability harness (e-usage-report-test--capability))
    (e-harness-create-session harness :id "session-1")
    (e-harness--emit-turn-event
     harness "session-1" "turn-1" 'tool-started
     '(:id "tool-call-1" :name "sample_tool"))
    (e-actions-call
     'usage-sample :run nil
     (list :harness harness :session-id "session-1" :turn-id "turn-1"))
    (let* ((report (e-usage-report-session
                    harness "session-1"
                    :turn-id "turn-1"
                    :include-zero-rows t))
           (tools (plist-get report :tools))
           (actions (plist-get report :actions)))
      (should (member '(:name "sample_tool" :count 1) tools))
      (should (member '(:name "idle_tool" :count 0) tools))
      (should (member '(:name "usage-sample/run" :count 1) actions))
      (should (member '(:name "usage-sample/idle" :count 0) actions)))))

(provide 'e-usage-report-test)

;;; e-usage-report-test.el ends here

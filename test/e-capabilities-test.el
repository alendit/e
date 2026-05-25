;;; e-capabilities-test.el --- Tests for e capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for capability contribution contracts.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-capabilities)
(require 'e-operations)
(require 'e-resources)
(require 'e-store)

(ert-deftest e-capabilities-test-create-capability ()
  "A capability carries independent contribution types."
  (let ((capability
         (e-capability-create
          :id 'buffer-read
          :name "Buffer Read"
          :instructions "Read buffers."
          :tools (list #'ignore)
          :resource-methods (list #'ignore)
          :resources (list #'ignore)
          :context-providers nil
          :hooks (list #'ignore)
          :actions '(:read-buffer ignore))))
    (should (eq (e-capability-id capability) 'buffer-read))
    (should (equal (e-capability-name capability) "Buffer Read"))
    (should (equal (e-capability-instructions capability) "Read buffers."))
    (should (= (length (e-capability-tools capability)) 1))
    (should (= (length (e-capability-resource-methods capability)) 1))
    (should (= (length (e-capability-resources capability)) 1))
    (should (= (length (e-capability-hooks capability)) 1))
    (should (plist-member (e-capability-actions capability) :read-buffer))))

(ert-deftest e-capabilities-test-register-tools ()
  "Capability tool providers register against the given registry."
  (let ((called nil)
        (registry (list :registry t)))
    (e-capabilities-register-tools
     (e-capability-create
      :id 'tools
      :tools (list (lambda (actual-registry)
                     (setq called actual-registry))))
     registry)
    (should (eq called registry))))

(ert-deftest e-capabilities-test-register-resource-methods ()
  "Capability resource method providers register against the given registry."
  (let ((registry (e-resources-registry-create)))
    (e-capabilities-register-resource-methods
     (e-capability-create
      :id 'resources
      :resource-methods
      (list (lambda (actual-registry)
              (e-resources-register
               actual-registry
               (e-resource-method-create
                :scheme "cap"
                :operation e-operation-read
                :handler (lambda (_uri _range) "resource"))))))
     registry)
    (should (equal (e-resources-read registry "cap://value" nil)
                   "resource"))))

(ert-deftest e-capabilities-test-register-resources ()
  "Capability resource providers register under the capability e:// namespace."
  (let ((store (e-store-create)))
    (e-capabilities-register-resources
     (e-capability-create
      :id 'capability-resources
      :resources
      (list (lambda (actual-store capability)
              (e-store-register
               actual-store
               (e-capability-id capability)
               "refs/capability.md"
               :description "Capability reference."
               :content "Capability reference content."))))
     store)
    (should (equal (e-store-read store
                                 "e://capability-resources/refs/capability.md"
                                 nil)
                   "Capability reference content."))))

(ert-deftest e-capabilities-test-derived-accessors-tolerate-stale-records ()
  "Capability accessors tolerate stale records compiled before hooks existed."
  (let ((legacy (vector 'cl-struct-e-capability
                        'legacy
                        "Legacy"
                        "Instructions."
                        nil
                        nil
                        nil
                        '(:legacy ignore))))
    (should-not (e-capability-resources legacy))
    (should-not (e-capability-hooks legacy))
    (should (equal (e-capability-actions legacy)
                   '(:legacy ignore)))))

(ert-deftest e-capabilities-test-register-hooks ()
  "Capability hook providers register against the given registry."
  (should (require 'e-hooks nil t))
  (let ((registry (e-hooks-registry-create)))
    (e-capabilities-register-hooks
     (e-capability-create
      :id 'hooks
      :hooks
      (list (e-hook-create
             :id "50-capability-hook"
             :point :post-tool-call
             :handler (lambda (value _context)
                        (concat value "-hooked")))))
     registry)
    (should (equal (e-hooks-run-reduce registry :post-tool-call "value" nil)
                   "value-hooked"))))

(ert-deftest e-capabilities-test-context-messages ()
  "Context messages include instructions before provider messages."
  (let* ((provider
          (lambda (&rest args)
            (list (list :role 'system
                        :content (format "provider:%s"
                                         (plist-get args :turn-id))))))
         (capability
          (e-capability-create
           :id 'context
           :instructions "Capability instructions."
           :context-providers (list provider)))
         (messages
          (e-capabilities-context-messages
           (list capability)
           :harness :harness
           :session-id "session"
           :turn-id "turn-1")))
    (should (equal messages
                   '((:role system :content "Capability instructions.")
                     (:role system :content "provider:turn-1"))))))

(ert-deftest e-capabilities-test-context-priority-orders_fragments ()
  "Context aggregation orders provider and instruction fragments by priority."
  (let* ((early-provider
          (e-context-provider-create
           :name 'early
           :priority 100
           :build (lambda (&rest _)
                    '((:role system :content "early provider")))))
         (late-provider
          (e-context-provider-create
           :name 'late
           :priority 300
           :build (lambda (&rest _)
                    '((:role system :content "late provider")))))
         (first
          (e-capability-create
           :id 'first
           :instructions "first instructions"
           :context-providers (list late-provider)))
         (second
          (e-capability-create
           :id 'second
           :instructions "second instructions"
           :context-providers (list early-provider)))
         (messages (e-capabilities-context-messages (list first second))))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           messages)
                   '("early provider"
                     "first instructions"
                     "second instructions"
                     "late provider")))))

(ert-deftest e-capabilities-test-context-priority_preserves_tie_order ()
  "Equal priorities preserve capability traversal and provider message order."
  (let* ((provider
          (e-context-provider-create
           :name 'default
           :build (lambda (&rest _)
                    '((:role system :content "provider one")
                      (:role system :content "provider two")))))
         (first (e-capability-create :id 'first
                                     :instructions "first instructions"))
         (second (e-capability-create :id 'second
                                      :context-providers (list provider)))
         (messages (e-capabilities-context-messages (list first second))))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           messages)
                   '("first instructions" "provider one" "provider two")))))

(ert-deftest e-capabilities-test-action ()
  "Capability actions are looked up by keyword."
  (let* ((action #'ignore)
         (capability
          (e-capability-create
           :id 'actions
           :actions (list :submit action))))
    (should (eq (e-capabilities-action capability :submit) action))
    (should-not (e-capabilities-action capability :missing))))

(provide 'e-capabilities-test)

;;; e-capabilities-test.el ends here

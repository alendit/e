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
(require 'e-prompts)
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
          :config-options '(:option-specs)
          :config '(:option "value")
          :prompts (list (e-prompt-spec-create
                          :name "explain"
                          :description "Explain."
                          :template "Explain this."))
          :actions '(:read-buffer ignore))))
    (should (eq (e-capability-id capability) 'buffer-read))
    (should (equal (e-capability-name capability) "Buffer Read"))
    (should (equal (e-capability-instructions capability) "Read buffers."))
    (should (= (length (e-capability-tools capability)) 1))
    (should (= (length (e-capability-resource-methods capability)) 1))
    (should (= (length (e-capability-resources capability)) 1))
    (should (= (length (e-capability-hooks capability)) 1))
    (should (equal (e-capability-config-options capability) '(:option-specs)))
    (should (equal (e-capability-config capability) '(:option "value")))
    (should (= (length (e-capability-prompts capability)) 1))
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
    (should-not (e-capability-config-options legacy))
    (should-not (e-capability-config legacy))
    (should-not (e-capability-prompts legacy))
    (should (equal (e-capability-actions legacy)
                   '(:legacy ignore)))))

(ert-deftest e-capabilities-test-instruction-priority-rejects-stale-records ()
  "Priority slot shape changes require capability rebuild, not vector fallback."
  (let ((legacy (vector 'cl-struct-e-capability
                        'legacy
                        "Legacy"
                        "Instructions."
                        nil
                        nil
                        nil
                        '(:legacy ignore))))
    (should-error (e-capability-instruction-priority legacy))))

(ert-deftest e-capabilities-test-public-accessors-do-not-keep-struct-metadata ()
  "Public capability accessors stay reload-safe plain functions."
  (let ((symbols '(e-capability-id
                   e-capability-name
                   e-capability-instructions
                   e-capability-tools
                   e-capability-resource-methods
                   e-capability-resources
                   e-capability-context-providers
                   e-capability-actions
                   e-capability-hooks
                   e-capability-instruction-priority
                   e-capability-config-options
                   e-capability-config
                   e-capability-prompts)))
    (dolist (symbol symbols)
      (put symbol 'compiler-macro 'stale)
      (put symbol 'side-effect-free 'stale)
      (put symbol 'gv-expander 'stale))
    (load-file (locate-library "e-capabilities.el"))
    (dolist (symbol symbols)
      (should-not (get symbol 'compiler-macro))
      (should-not (get symbol 'side-effect-free))
      (should-not (get symbol 'gv-expander)))))

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
  "Context aggregation keeps static instructions before provider fragments."
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
                   '("first instructions"
                     "second instructions"
                     "early provider"
                     "late provider")))))

(ert-deftest e-capabilities-test-context-cache-placement-orders_fragments ()
  "Context aggregation keeps stable provider context before dynamic context."
  (let* ((stable-provider
          (e-context-provider-create
           :name 'stable
           :priority 300
           :cache-placement 'stable-context
           :build (lambda (&rest _)
                    '((:role system :content "stable provider")))))
         (dynamic-provider
          (e-context-provider-create
           :name 'dynamic
           :priority 100
           :cache-placement 'dynamic-context
           :build (lambda (&rest _)
                    '((:role system :content "dynamic provider")))))
         (capability
          (e-capability-create
           :id 'context
           :instructions "static instructions"
           :context-providers (list dynamic-provider stable-provider)))
         (messages (e-capabilities-context-messages (list capability))))
    (should (equal (mapcar (lambda (message) (plist-get message :content))
                           messages)
                   '("static instructions"
                     "stable provider"
                     "dynamic provider")))))

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

(ert-deftest e-capabilities-test-action-descriptor-preserves-function-lookup ()
  "Action descriptors keep direct function lookup backward compatible."
  (let* ((handler #'ignore)
         (descriptor (e-action-create
                      :handler handler
                      :description "Submit."
                      :parameters '(:type "object")))
         (capability
          (e-capability-create
           :id 'actions
           :actions (list :submit descriptor))))
    (should (eq (e-capabilities-action capability :submit) handler))
    (should (eq (e-capabilities-action-spec capability :submit) descriptor))))

(provide 'e-capabilities-test)

;;; e-capabilities-test.el ends here

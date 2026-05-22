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

(ert-deftest e-capabilities-test-create-capability ()
  "A capability carries independent contribution types."
  (let ((capability
         (e-capability-create
          :id 'buffer-read
          :name "Buffer Read"
          :instructions "Read buffers."
          :tools (list #'ignore)
          :resource-methods (list #'ignore)
          :context-providers nil
          :actions '(:read-buffer ignore))))
    (should (eq (e-capability-id capability) 'buffer-read))
    (should (equal (e-capability-name capability) "Buffer Read"))
    (should (equal (e-capability-instructions capability) "Read buffers."))
    (should (= (length (e-capability-tools capability)) 1))
    (should (= (length (e-capability-resource-methods capability)) 1))
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

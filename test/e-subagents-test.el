;;; e-subagents-test.el --- Tests for the subagents capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for subagent type discovery: harness-instance registration flags, the
;; types context provider, and the read-only type catalog resource.

;;; Code:

(require 'ert)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-store)
(require 'e-subagents)

(defmacro e-subagents-test--with-empty-registries (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     ,@body))

(defun e-subagents-test--fake-factory ()
  "Return a fake harness factory."
  (lambda () (e-harness-create :backend (e-backend-fake-create :items nil))))

(defun e-subagents-test--register-types ()
  "Register a representative set of subagent and non-subagent instances."
  (e-harness-instance-register
   :id :reviewer
   :name "Reviewer"
   :kind 'reviewer
   :subagent t
   :description "Use for focused review."
   :context-visibility 'always
   :factory (e-subagents-test--fake-factory))
  (e-harness-instance-register
   :id :secret-tool-user
   :name "Secret tool user"
   :kind 'tool-user
   :subagent t
   :description "Rarely used specialized tool runner."
   :context-visibility 'hidden
   :factory (e-subagents-test--fake-factory))
  (e-harness-instance-register
   :id :chat-default
   :name "Chat"
   :kind 'chat
   :factory (e-subagents-test--fake-factory)))

(ert-deftest e-subagents-test-lists-spawnable-instances-by-flag ()
  "Only instances registered with :subagent are enumerated as spawnable."
  (e-subagents-test--with-empty-registries
    (e-subagents-test--register-types)
    (should (equal (mapcar #'e-harness-instance-id
                           (e-harness-instance-list-subagents))
                   '(:reviewer :secret-tool-user)))
    (should (equal (mapcar #'e-harness-instance-id
                           (e-harness-instance-list-subagents :visibility 'always))
                   '(:reviewer)))
    (should (equal (mapcar #'e-harness-instance-id
                           (e-harness-instance-list-subagents :visibility 'hidden))
                   '(:secret-tool-user)))))

(ert-deftest e-subagents-test-register-rejects-bad-visibility ()
  "Registration validates the context-visibility value."
  (e-subagents-test--with-empty-registries
    (should-error
     (e-harness-instance-register
      :id :bad
      :name "Bad"
      :kind 'reviewer
      :subagent t
      :context-visibility 'sometimes
      :factory (e-subagents-test--fake-factory))
     :type 'wrong-type-argument)))

(ert-deftest e-subagents-test-context-block-honors-visibility ()
  "The context block includes `always' types and omits `hidden' ones."
  (e-subagents-test--with-empty-registries
    (e-subagents-test--register-types)
    (let* ((messages
            (e-capabilities-context-messages
             (list (e-subagents-capability-create))))
           (content (mapconcat (lambda (message) (plist-get message :content))
                               messages "\n")))
      (should (string-match-p ":reviewer -- Reviewer -- Use for focused review." content))
      (should-not (string-match-p ":secret-tool-user" content))
      (should-not (string-match-p ":chat-default" content)))))

(ert-deftest e-subagents-test-context-block-empty-without-types ()
  "No subagent context block is contributed when nothing is spawnable."
  (e-subagents-test--with-empty-registries
    (e-harness-instance-register
     :id :chat-default
     :name "Chat"
     :kind 'chat
     :factory (e-subagents-test--fake-factory))
    (should-not (e-subagents--context-messages))))

(ert-deftest e-subagents-test-catalog-resource-includes-hidden ()
  "The refs/types.md resource lists every spawnable type, hidden included."
  (e-subagents-test--with-empty-registries
    (e-subagents-test--register-types)
    (let* ((capability (e-subagents-capability-create))
           (store (e-store-create)))
      (e-capabilities-register-resources capability store)
      (should (member "e://subagents/refs/types.md"
                      (mapcar #'e-store-entry-uri (e-store-list store))))
      (let ((catalog (e-store-read store "e://subagents/refs/types.md" nil)))
        (should (string-match-p "## :reviewer" catalog))
        (should (string-match-p "## :secret-tool-user" catalog))
        (should (string-match-p "Rarely used specialized tool runner." catalog))
        (should-not (string-match-p ":chat-default" catalog))))))

(provide 'e-subagents-test)

;;; e-subagents-test.el ends here

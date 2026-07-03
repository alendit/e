;;; e-chat-output-mode-test.el --- Tests for chat output mode capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the chat-output-mode capability: mode resolution across
;; global/project/session precedence, the Org-output system instruction
;; presence gating, and the per-session set/clear helper.

;;; Code:

(require 'ert)
(require 'seq)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-chat-output-mode)
(require 'e-harness)
(require 'e-session)

;;;; Mode resolution

(ert-deftest e-chat-output-mode-test-default-is-markdown ()
  "With no configuration the resolved mode is `markdown'."
  (let ((e-capability-config nil)
        (directory (make-temp-file "e-chat-output-mode-" t)))
    (unwind-protect
        (should (eq (e-chat-output-mode-resolve nil nil directory) 'markdown))
      (delete-directory directory t))))

(ert-deftest e-chat-output-mode-test-global-config-selects-org ()
  "A global config value selects the Org mode."
  (let ((e-capability-config '((chat-output-mode :mode org)))
        (directory (make-temp-file "e-chat-output-mode-" t)))
    (unwind-protect
        (should (eq (e-chat-output-mode-resolve nil nil directory) 'org))
      (delete-directory directory t))))

(ert-deftest e-chat-output-mode-test-rejects-unknown-mode-option ()
  "An unknown mode value signals the capability-config invalid-value error."
  (let ((e-capability-config '((chat-output-mode :mode sideways)))
        (directory (make-temp-file "e-chat-output-mode-" t)))
    (unwind-protect
        (should-error
         (e-chat-output-mode-resolve nil nil directory)
         :type 'e-capability-config-invalid-value)
      (delete-directory directory t))))

(ert-deftest e-chat-output-mode-test-session-override-wins ()
  "A per-session override wins over global config resolution."
  (let ((e-capability-config '((chat-output-mode :mode markdown)))
        (harness (e-harness-create :backend (e-backend-create :name "noop"))))
    (e-harness-create-session harness :id "session-1")
    (should (eq (e-chat-output-mode-resolve harness "session-1") 'markdown))
    (e-chat-output-mode-session-set harness "session-1" 'org)
    (should (eq (e-chat-output-mode-session-get harness "session-1") 'org))
    (should (eq (e-chat-output-mode-resolve harness "session-1") 'org))
    (e-chat-output-mode-session-set harness "session-1" nil)
    (should (null (e-chat-output-mode-session-get harness "session-1")))
    (should (eq (e-chat-output-mode-resolve harness "session-1") 'markdown))))

(ert-deftest e-chat-output-mode-test-session-set-rejects-unknown ()
  "Setting an unknown per-session mode signals an error."
  (let ((harness (e-harness-create :backend (e-backend-create :name "noop"))))
    (e-harness-create-session harness :id "session-1")
    (should-error
     (e-chat-output-mode-session-set harness "session-1" 'diagonal))))

;;;; Instruction gating

(ert-deftest e-chat-output-mode-test-markdown-contributes-no-instruction ()
  "With markdown resolved the capability contributes no context message."
  (let ((e-capability-config nil)
        (directory (make-temp-file "e-chat-output-mode-" t)))
    (unwind-protect
        (let* ((capability (e-chat-output-mode-capability-create directory))
               (messages (e-capabilities-context-messages (list capability))))
          (should (eq (e-capability-id capability) 'chat-output-mode))
          (should (null messages)))
      (delete-directory directory t))))

(ert-deftest e-chat-output-mode-test-org-contributes-system-instruction ()
  "With org resolved the capability contributes the Org-output system message."
  (let ((e-capability-config '((chat-output-mode :mode org)))
        (directory (make-temp-file "e-chat-output-mode-" t)))
    (unwind-protect
        (let* ((capability (e-chat-output-mode-capability-create directory))
               (messages (e-capabilities-context-messages (list capability)))
               (system (seq-find
                        (lambda (message)
                          (eq (plist-get message :role) 'system))
                        messages)))
          (should system)
          (should (equal (plist-get system :content)
                         e-chat-output-mode--org-instructions))
          (should (string-match-p "Org markup"
                                  (plist-get system :content)))
          (should (string-match-p "\\[\\[target\\]\\[description\\]\\]"
                                  (plist-get system :content))))
      (delete-directory directory t))))

(ert-deftest e-chat-output-mode-test-set-config-writes-mode ()
  "The global set helper writes the mode without touching other capabilities."
  (let ((e-capability-config '((other-cap :flag t))))
    (e-chat-output-mode--set-config 'org)
    (should (equal (cdr (assq 'chat-output-mode e-capability-config))
                   '(:mode org)))
    (should (equal (cdr (assq 'other-cap e-capability-config))
                   '(:flag t)))))

(provide 'e-chat-output-mode-test)

;;; e-chat-output-mode-test.el ends here

;;; e-shells-test.el --- Tests for shell manifests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for first-class presentation shell manifests.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-shells)

(ert-deftest e-shells-test-command-struct-preserves-fields ()
  "Shell command descriptors preserve their manifest fields."
  (let ((command (e-shell-command-create
                  :id 'open
                  :summary "Open a shell."
                  :interactive 'e-example-open-command
                  :function 'e-example-open
                  :scope 'global
                  :requires '(:shell chat)
                  :metadata '(:label "Open"))))
    (should (eq (e-shell-command-id command) 'open))
    (should (equal (e-shell-command-summary command) "Open a shell."))
    (should (eq (e-shell-command-interactive command)
                'e-example-open-command))
    (should (eq (e-shell-command-function command) 'e-example-open))
    (should (eq (e-shell-command-scope command) 'global))
    (should (equal (e-shell-command-requires command) '(:shell chat)))
    (should (equal (e-shell-command-metadata command) '(:label "Open")))))

(ert-deftest e-shells-test-shell-struct-preserves-fields ()
  "Shell descriptors preserve their manifest fields."
  (let* ((command (e-shell-command-create :id 'open))
         (shell (e-shell-create
                 :id 'example
                 :name "Example"
                 :summary "Example shell."
                 :required-capabilities '(chat-session)
                 :optional-capabilities '(selection-context)
                 :commands (list command)
                 :keymaps '((:id example-mode :keymap example-mode-map))
                 :metadata '(:status experimental))))
    (should (eq (e-shell-id shell) 'example))
    (should (equal (e-shell-name shell) "Example"))
    (should (equal (e-shell-summary shell) "Example shell."))
    (should (equal (e-shell-required-capabilities shell) '(chat-session)))
    (should (equal (e-shell-optional-capabilities shell)
                   '(selection-context)))
    (should (equal (e-shell-commands shell) (list command)))
    (should (equal (e-shell-keymaps shell)
                   '((:id example-mode :keymap example-mode-map))))
    (should (equal (e-shell-metadata shell) '(:status experimental)))))

(ert-deftest e-shells-test-register-get-list-and-replace ()
  "The shell registry stores manifests by id and replaces by id."
  (let* ((first (e-shell-create :id 'registry-test :name "First"))
         (second (e-shell-create :id 'registry-test :name "Second")))
    (e-shell-register first)
    (should (eq (e-shell-get 'registry-test) first))
    (should (memq first (e-shell-list)))
    (e-shell-register second)
    (should (eq (e-shell-get 'registry-test) second))
    (should-not (memq first (e-shell-list)))))

(ert-deftest e-shells-test-rejects-missing-shell-id ()
  "Shell manifests must have explicit non-nil ids."
  (should-error
   (e-shell-register (e-shell-create :id nil :name "Missing"))
   :type 'wrong-type-argument))

(ert-deftest e-shells-test-rejects-missing-command-id ()
  "Shell commands must have explicit non-nil ids."
  (should-error
   (e-shell-validate
    (e-shell-create
     :id 'missing-command-id
     :commands (list (e-shell-command-create :id nil))))
   :type 'wrong-type-argument))

(ert-deftest e-shells-test-active-shells-include-harness-layer-shells ()
  "Active shell discovery includes layer-owned shells for the target harness."
  (let ((e-shell--registry (make-hash-table :test 'eq))
        (e-shell--scoped-registry (make-hash-table :test 'eq)))
    (let* ((harness-a (cons 'harness 'a))
           (harness-b (cons 'harness 'b))
           (global (e-shell-create :id 'chat :name "Chat"))
           (project (e-shell-create :id 'topic :name "Topic"))
           (collision (e-shell-create :id 'chat :name "Project Chat")))
      (e-shell-register global)
      (e-shell-register-layer-shells
       harness-a 'project-local (list project collision)
       :project-root "/tmp/example/")
      (should (eq (e-shell-get-active 'chat harness-a) global))
      (should (eq (e-shell-get-active 'topic harness-a) project))
      (should-not (e-shell-get-active 'topic harness-b))
      (should (memq project (e-shell-list-active harness-a)))
      (should-not (memq collision (e-shell-list-active harness-a)))
      (e-shell-unregister-layer-shells harness-a 'project-local)
      (should-not (e-shell-get-active 'topic harness-a)))))

(ert-deftest e-shells-test-command-lookup-finds-command-by-id ()
  "Command lookup returns the shell command descriptor matching id."
  (let* ((open (e-shell-command-create :id 'open))
         (close (e-shell-command-create :id 'close))
         (shell (e-shell-create :id 'lookup-test
                                :commands (list open close))))
    (should (eq (e-shell-command-by-id shell 'open) open))
    (should (eq (e-shell-command-by-id shell 'close) close))
    (should-not (e-shell-command-by-id shell 'missing))))

(provide 'e-shells-test)

;;; e-shells-test.el ends here

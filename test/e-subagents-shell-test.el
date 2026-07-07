;;; e-subagents-shell-test.el --- Tests for the subagents list shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell smoke tests for `e-subagents-shell': the list buffer renders one row
;; per child, scopes to the parent session, and refreshes on registry change.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-subagent-registry)
(require 'e-subagent-runner)
(require 'e-subagents-shell)

(defmacro e-subagents-shell-test--with-instances (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     (e-harness-instance-register
      :id :reviewer
      :name "Reviewer"
      :kind 'reviewer
      :subagent t
      :description "Use for review."
      :factory (lambda () (e-harness-create
                           :backend (e-backend-fake-create :items nil))))
     ,@body))

(defun e-subagents-shell-test--spawn (registry parent parent-session-id label)
  "Spawn a non-settling reviewer child under PARENT with LABEL."
  (e-subagent-spawn registry parent parent-session-id
                    :type :reviewer :prompt "go" :label label
                    :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))

(ert-deftest e-subagents-shell-test-renders-children-scoped-to-parent ()
  "The list buffer renders one row per child of the parent session."
  (e-subagents-shell-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-create-session parent :id "parent-1")
      (e-harness-create-session parent :id "parent-2")
      (e-subagents-shell-test--spawn registry parent "parent-1" "child a")
      (e-subagents-shell-test--spawn registry parent "parent-2" "child b")
      (let ((buffer (e-subagents-list-buffer
                     :registry registry :parent-session-id "parent-1")))
        (unwind-protect
            (with-current-buffer buffer
              (should (derived-mode-p 'e-subagents-shell-mode))
              (should (= 1 (length tabulated-list-entries)))
              (should (equal (aref (cadr (car tabulated-list-entries)) 0)
                             "child a")))
          (kill-buffer buffer)
          (remove-hook 'e-subagent-registry-change-functions
                       #'e-subagents-shell--refresh-buffers))))))

(ert-deftest e-subagents-shell-test-refreshes-on-registry-change ()
  "A spawn after opening the buffer is reflected by the change hook."
  (e-subagents-shell-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-create-session parent :id "parent-1")
      (let ((buffer (e-subagents-list-buffer
                     :registry registry :parent-session-id "parent-1")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (should (null tabulated-list-entries)))
              (e-subagents-shell-test--spawn registry parent "parent-1" "late")
              (with-current-buffer buffer
                (should (= 1 (length tabulated-list-entries)))))
          (kill-buffer buffer)
          (remove-hook 'e-subagent-registry-change-functions
                       #'e-subagents-shell--refresh-buffers))))))

(ert-deftest e-subagents-shell-test-row-actions-are-commands ()
  "The documented row-action keys are bound to interactive commands."
  (e-subagents-shell-test--with-instances
    (let ((buffer (e-subagents-list-buffer
                   :registry (e-subagent-registry-create))))
      (unwind-protect
          (with-current-buffer buffer
            (dolist (cell '(("RET" . e-subagents-shell-open-chat)
                            ("i" . e-subagents-shell-interrupt)
                            ("k" . e-subagents-shell-shutdown)
                            ("g" . e-subagents-shell-refresh)))
              (let ((binding (keymap-lookup e-subagents-shell-mode-map (car cell))))
                (should (eq binding (cdr cell)))
                (should (commandp binding)))))
        (kill-buffer buffer)
        (remove-hook 'e-subagent-registry-change-functions
                     #'e-subagents-shell--refresh-buffers)))))

(provide 'e-subagents-shell-test)

;;; e-subagents-shell-test.el ends here

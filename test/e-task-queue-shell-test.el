;;; e-task-queue-shell-test.el --- Tests for the task queue list shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell smoke tests for `e-task-queue-shell': the list buffer renders rows and
;; refreshes when the backing queue changes.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-task-queue)
(require 'e-task-queue-shell)

(defmacro e-task-queue-shell-test--with-instances (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     (e-harness-instance-register
      :id :chat-test
      :name "Test"
      :kind 'chat
      :default t
      :factory (lambda () (e-harness-create
                           :backend (e-backend-fake-create :items nil))))
     ,@body))

(defun e-task-queue-shell-test--queue ()
  "Return a queue whose fake runner never auto-settles."
  (e-task-queue-create
   :runner (lambda (_task _harness _on-settle) (list :cancel #'ignore))))

(ert-deftest e-task-queue-shell-test-renders-rows-newest-first ()
  "The list buffer renders one row per task, newest-first."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (a (e-task-queue-enqueue queue :prompt "first"))
           (b (e-task-queue-enqueue queue :prompt "second"))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'e-task-queue-shell-mode))
            (should (equal (mapcar #'car tabulated-list-entries)
                           (list (plist-get b :task-id)
                                 (plist-get a :task-id)))))
        (kill-buffer buffer)))))

(ert-deftest e-task-queue-shell-test-refreshes-on-queue-change ()
  "An enqueue after opening the buffer is reflected by the change hook."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (should (null tabulated-list-entries)))
            (e-task-queue-enqueue queue :prompt "late arrival")
            (with-current-buffer buffer
              (should (= 1 (length tabulated-list-entries)))))
        (kill-buffer buffer)
        (remove-hook 'e-task-queue-change-functions
                     #'e-task-queue-shell--refresh-buffers)))))

(ert-deftest e-task-queue-shell-test-refresh-key-is-command ()
  "The `g' refresh keybinding is bound to an interactive command."
  (e-task-queue-shell-test--with-instances
    (let ((buffer (e-task-queue-list-buffer
                   :queue (e-task-queue-shell-test--queue))))
      (unwind-protect
          (with-current-buffer buffer
            (let ((binding (key-binding (kbd "g"))))
              (should (eq binding #'e-task-queue-shell-refresh))
              (should (commandp binding))))
        (kill-buffer buffer)
        (remove-hook 'e-task-queue-change-functions
                     #'e-task-queue-shell--refresh-buffers)))))

(provide 'e-task-queue-shell-test)

;;; e-task-queue-shell-test.el ends here

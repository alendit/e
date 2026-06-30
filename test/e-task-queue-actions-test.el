;;; e-task-queue-actions-test.el --- Tests for task queue capability actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Task Queue capability actions: the skill-backed surface
;; over the Slice 1 registry, and the assertion that the queue stays out of
;; model-facing tool definitions (actions only), mirroring `elisp-job'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-capabilities)
(require 'e-store)
(require 'e-tools)
(require 'e-task-queue)
(require 'e-task-queue-actions)

(defmacro e-task-queue-actions-test--with-instances (&rest body)
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

(defun e-task-queue-actions-test--queue ()
  "Return a queue whose fake runner never auto-settles."
  (e-task-queue-create
   :runner (lambda (_task _harness _on-settle) (list :cancel #'ignore))))

(ert-deftest e-task-queue-actions-test-capability-actions-and-resource ()
  "The capability exposes the queue actions and a readable reference resource."
  (let* ((capability (e-task-queue-capability-create
                      :queue (e-task-queue-actions-test--queue)))
         (store (e-store-create)))
    (should (eq (e-capability-id capability) 'task-queue))
    (dolist (action '(:enqueue :list-tasks :task-status :read-task :cancel-task))
      (should (e-capabilities-action-spec capability action)))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://task-queue/skills/task-queue")))
    (should (string-match-p
             "enqueue"
             (e-store-read store "e://task-queue/skills/task-queue" nil)))))

(ert-deftest e-task-queue-actions-test-round-trip ()
  "Enqueue, list, status, read, and cancel round-trip through the actions."
  (e-task-queue-actions-test--with-instances
    (let* ((queue (e-task-queue-actions-test--queue))
           (capability (e-task-queue-capability-create :queue queue))
           (enqueue (e-capabilities-action capability :enqueue))
           (list-tasks (e-capabilities-action capability :list-tasks))
           (task-status (e-capabilities-action capability :task-status))
           (read-task (e-capabilities-action capability :read-task))
           (cancel-task (e-capabilities-action capability :cancel-task))
           (record (funcall enqueue (list :prompt "research X"
                                          :metadata '(:source "grimoire:inbox"))))
           (task-id (plist-get record :task-id)))
      (should (stringp task-id))
      (should (equal (plist-get record :metadata) '(:source "grimoire:inbox")))
      (should (= 1 (length (funcall list-tasks nil))))
      (should (equal (plist-get (funcall task-status (list :task-id task-id))
                                :task-id)
                     task-id))
      (should (plist-member (funcall read-task (list :task-id task-id)) :outputs))
      (should (eq (plist-get (funcall cancel-task (list :task-id task-id))
                             :status)
                  'cancelled)))))

(ert-deftest e-task-queue-actions-test-string-instance-id-resolves ()
  "A string `harness-instance-id', as the action schema advertises, resolves."
  (e-task-queue-actions-test--with-instances
    (let* ((settled-harness nil)
           (queue (e-task-queue-create
                   :runner (lambda (_task harness _on-settle)
                             (setq settled-harness harness)
                             (list :cancel #'ignore))))
           (capability (e-task-queue-capability-create :queue queue))
           (enqueue (e-capabilities-action capability :enqueue))
           (expected (e-harness-instance-get-or-create :chat-test))
           (record (funcall enqueue (list :prompt "go"
                                          :harness-instance-id "chat-test"))))
      (should (eq (plist-get record :status) 'running))
      (should (eq settled-harness expected)))))

(ert-deftest e-task-queue-actions-test-is-action-not-tool ()
  "The task queue capability exposes actions but no model-facing tools."
  (let* ((capability (e-task-queue-capability-create
                      :queue (e-task-queue-actions-test--queue)))
         (registry (e-tools-registry-create)))
    (e-capabilities-register-tools capability registry)
    (let ((names (mapcar (lambda (tool) (plist-get tool :name))
                         (e-tools-definitions registry))))
      (should-not (member "task_queue" names))
      (should-not (member "enqueue" names)))
    (should (e-capabilities-action-spec capability :enqueue))
    (should (e-capabilities-action-spec capability :list-tasks))
    (should (e-capabilities-action-spec capability :task-status))
    (should (e-capabilities-action-spec capability :read-task))
    (should (e-capabilities-action-spec capability :cancel-task))))

(provide 'e-task-queue-actions-test)

;;; e-task-queue-actions-test.el ends here

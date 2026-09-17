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
(require 'e-work)

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

(defun e-task-queue-actions-test--call (capability action arguments)
  "Start CAPABILITY ACTION work with ARGUMENTS and return its immediate result."
  (let* ((spec (e-capabilities-action-spec capability action))
         (handle (e-work-start (e-action-work spec) arguments)))
    (e-work-handle-result handle)))

(ert-deftest e-task-queue-actions-test-only-public-queue-registers-task-resolver ()
  "Only the public action queue may claim the process-global task scheme."
  (let ((e-task-queue-actions-default-queue
         (e-task-queue-actions-test--queue)))
    (setf (e-task-queue-expose-await-references-p
           e-task-queue-actions-default-queue) t)
    (should (e-task-queue-expose-await-references-p
             e-task-queue-actions-default-queue))
    (should-error
     (e-task-queue-actions-register-waitable-resolver
      (e-task-queue-actions-test--queue))
     :type 'user-error)))

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

(ert-deftest e-task-queue-actions-test-enqueue-action-has-agent-task-work ()
  "The enqueue action exposes the generic agent-task work carrier."
  (let* ((queue (e-task-queue-actions-test--queue))
         (capability (e-task-queue-capability-create :queue queue))
         (spec (e-capabilities-action-spec capability :enqueue))
         (work (e-action-work spec)))
    (should (e-work-spec-p work))
    (should (eq (e-work-spec-execution work) 'agent-task))))

(ert-deftest e-task-queue-actions-test-round-trip ()
  "Enqueue, list, status, read, and cancel round-trip through the actions."
  (e-task-queue-actions-test--with-instances
    (let* ((queue (e-task-queue-actions-test--queue))
           (capability (e-task-queue-capability-create :queue queue))
           (record (e-task-queue-actions-test--call
                    capability :enqueue
                    (list :prompt "research X"
                          :metadata '(:source "grimoire:inbox"))))
           (task-id (plist-get record :task-id)))
      (should (stringp task-id))
      (should-not (plist-member record :await-ref))
      (should (equal (plist-get record :metadata) '(:source "grimoire:inbox")))
      (should (= 1 (length (e-task-queue-actions-test--call
                            capability :list-tasks nil))))
      (should (equal (plist-get (e-task-queue-actions-test--call
                                 capability :task-status
                                 (list :task-id task-id))
                                :task-id)
                     task-id))
      (should (plist-member (e-task-queue-actions-test--call
                             capability :read-task
                             (list :task-id task-id))
                            :outputs))
      (should (equal (plist-get (e-task-queue-actions-test--call
                              capability :cancel-task
                              (list :task-id task-id))
                             :status)
                  "cancelled")))))

(ert-deftest e-task-queue-actions-test-string-instance-id-resolves ()
  "A string `harness-instance-id', as the action schema advertises, resolves."
  (e-task-queue-actions-test--with-instances
    (let* ((settled-harness nil)
           (queue (e-task-queue-create
                   :runner (lambda (_task harness _on-settle)
                             (setq settled-harness harness)
                             (list :cancel #'ignore))))
           (capability (e-task-queue-capability-create :queue queue))
           (expected (e-harness-instance-get-or-create :chat-test))
           (record (e-task-queue-actions-test--call
                    capability :enqueue
                    (list :prompt "go"
                          :harness-instance-id "chat-test"))))
      (should (equal (plist-get record :status) "running"))
      (should (eq settled-harness expected)))))

(ert-deftest e-task-queue-actions-test-pause-resume-round-trip ()
  "Pause and resume actions move a task through `paused' and back.
The runner's cancel settles like the real harness runner does on abort, so
pausing a running task lands it `paused' rather than leaving it running."
  (e-task-queue-actions-test--with-instances
    (let* ((queue (e-task-queue-create
                   :runner (lambda (_task _harness on-settle)
                             (list :cancel
                                   (lambda () (funcall on-settle
                                                       :status 'cancelled))))))
           (capability (e-task-queue-capability-create :queue queue))
           (record (e-task-queue-actions-test--call
                    capability :enqueue (list :prompt "work")))
           (task-id (plist-get record :task-id)))
      (should (equal (plist-get (e-task-queue-actions-test--call
                              capability :pause-task
                              (list :task-id task-id))
                             :status)
                  "paused"))
      (should (equal (plist-get (e-task-queue-actions-test--call
                              capability :resume-task
                              (list :task-id task-id))
                             :status)
                  "running")))))

(ert-deftest e-task-queue-actions-test-pause-all-gates-queue ()
  "The pause-all action gates the queue so enqueued work stays queued."
  (e-task-queue-actions-test--with-instances
    (let* ((queue (e-task-queue-actions-test--queue))
           (capability (e-task-queue-capability-create :queue queue)))
      (e-task-queue-actions-test--call capability :pause-all nil)
      (let* ((record (e-task-queue-actions-test--call
                      capability :enqueue (list :prompt "held")))
             (task-id (plist-get record :task-id)))
        (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                    'queued))
        (e-task-queue-actions-test--call capability :resume-all nil)
        (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                    'running))))))

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
    (should (e-capabilities-action-spec capability :cancel-task))
    (should (e-capabilities-action-spec capability :pause-task))
    (should (e-capabilities-action-spec capability :resume-task))
    (should (e-capabilities-action-spec capability :pause-all))
    (should (e-capabilities-action-spec capability :resume-all))))


(ert-deftest e-task-queue-actions-test-ensure-initialized-is-idempotent ()
  "Ensuring initialized process-local state is an idempotent no-op."
  (let ((e-task-queue-actions-default-queue
         (e-task-queue-actions-test--queue)))
    (setf (e-task-queue-loaded-p e-task-queue-actions-default-queue) t)
    (unwind-protect
        (cl-letf (((symbol-function 'e-task-queue-initialize)
                   (lambda (&rest _args)
                     (ert-fail "An initialized queue must not initialize again"))))
          (should (eq (e-task-queue-actions-ensure-initialized)
                      e-task-queue-actions-default-queue)))
      (setf (e-task-queue-loaded-p e-task-queue-actions-default-queue) nil))))

(ert-deftest e-task-queue-actions-test-ensure-initialized-issues-no-storage-work ()
  "Process-local initialization neither queries nor mutates durable state."
  (let ((e-task-queue-actions-default-queue
         (e-task-queue-actions-test--queue)))
    (cl-letf (((symbol-function 'e-task-storage-submit)
               (lambda (&rest _args)
                 (ert-fail "Initialization must issue no SQLite operation"))))
      (should (eq (e-task-queue-actions-ensure-initialized)
                  e-task-queue-actions-default-queue))
      (should (e-task-queue-loaded-p e-task-queue-actions-default-queue)))))

(provide 'e-task-queue-actions-test)

;;; e-task-queue-actions-test.el ends here

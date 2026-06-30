;;; e-task-queue-test.el --- Tests for the agent task queue -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for `e-task-queue'.  A fake runner stands in for the harness-turn
;; runner: it records the harness it was handed and settles only when the test
;; calls the stored settle thunk, so admission control, ordering, settle-driven
;; dispatch, and cancellation are all driven deterministically without real
;; turns.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-task-queue)

(defmacro e-task-queue-test--with-instances (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     ,@body))

(defun e-task-queue-test--register-instance (id &optional default)
  "Register a fake chat harness instance ID, optionally as DEFAULT."
  (e-harness-instance-register
   :id id
   :name (symbol-name id)
   :kind 'chat
   :default default
   :factory (lambda () (e-harness-create
                        :backend (e-backend-fake-create :items nil)))))

(cl-defstruct e-task-queue-test--recorder
  "Captured runner invocations for a fake runner."
  (calls nil))

(defun e-task-queue-test--fake-runner (recorder)
  "Return a runner that records calls into RECORDER and never auto-settles.
Each call appends a plist of `:task', `:harness', and `:settle' (the settle
thunk) so the test settles tasks explicitly."
  (lambda (task harness on-settle)
    (push (list :task task :harness harness :settle on-settle)
          (e-task-queue-test--recorder-calls recorder))
    (list :cancel (lambda () (funcall on-settle :status 'cancelled)))))

(defun e-task-queue-test--running-ids (queue)
  "Return task ids in QUEUE whose status is running."
  (mapcar (lambda (r) (plist-get r :task-id))
          (cl-remove-if-not
           (lambda (r) (eq (plist-get r :status) 'running))
           (e-task-queue-list queue))))

(ert-deftest e-task-queue-test-enqueue-returns-record ()
  "Enqueue returns a record and the task is admitted under the cap."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder)))
           (record (e-task-queue-enqueue queue :prompt "do thing")))
      (should (stringp (plist-get record :task-id)))
      (should (equal (plist-get record :prompt) "do thing"))
      ;; Cap is 2 by default and nothing else is running, so it dispatched.
      (should (eq (plist-get (e-task-queue-get queue (plist-get record :task-id))
                             :status)
                  'running)))))

(ert-deftest e-task-queue-test-admission-control-under-cap ()
  "With cap 2, a third enqueue waits until a running task settles."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-parallel 2
                   :runner (e-task-queue-test--fake-runner recorder)))
           (a (e-task-queue-enqueue queue :prompt "a"))
           (b (e-task-queue-enqueue queue :prompt "b"))
           (c (e-task-queue-enqueue queue :prompt "c")))
      (should (equal (sort (e-task-queue-test--running-ids queue) #'string<)
                     (sort (list (plist-get a :task-id)
                                 (plist-get b :task-id))
                           #'string<)))
      (should (eq (plist-get (e-task-queue-get queue (plist-get c :task-id))
                             :status)
                  'queued))
      ;; Settle a; the queued c should now start.
      (let ((settle (plist-get (car (last (e-task-queue-test--recorder-calls
                                           recorder)))
                               :settle)))
        ;; Calls are pushed newest-first; the oldest (a) is the last element.
        (funcall settle :status 'done))
      (should (eq (plist-get (e-task-queue-get queue (plist-get c :task-id))
                             :status)
                  'running)))))

(ert-deftest e-task-queue-test-status-transitions-to-done ()
  "A settled task records done, outputs, and a finished timestamp."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder)))
           (task (e-task-queue-enqueue queue :prompt "a"))
           (task-id (plist-get task :task-id))
           (settle (plist-get (car (e-task-queue-test--recorder-calls recorder))
                              :settle)))
      (funcall settle :status 'done
               :outputs (list (list :kind 'text :value "result")))
      (let ((record (e-task-queue-get queue task-id)))
        (should (eq (plist-get record :status) 'done))
        (should (equal (e-task-queue-outputs queue task-id)
                       (list (list :kind 'text :value "result"))))
        (should (plist-get record :finished-at))))))

(ert-deftest e-task-queue-test-failure-does-not-block-dispatch ()
  "A failed task records its error and frees a slot for the next task."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-parallel 1
                   :runner (e-task-queue-test--fake-runner recorder)))
           (a (e-task-queue-enqueue queue :prompt "a"))
           (b (e-task-queue-enqueue queue :prompt "b")))
      (should (eq (plist-get (e-task-queue-get queue (plist-get b :task-id))
                             :status)
                  'queued))
      (let ((settle (plist-get (car (e-task-queue-test--recorder-calls recorder))
                               :settle)))
        (funcall settle :status 'failed :error "boom"))
      (let ((record (e-task-queue-get queue (plist-get a :task-id))))
        (should (eq (plist-get record :status) 'failed))
        (should (equal (plist-get record :error) "boom")))
      (should (eq (plist-get (e-task-queue-get queue (plist-get b :task-id))
                             :status)
                  'running)))))

(ert-deftest e-task-queue-test-cancel-queued-task ()
  "Cancelling a queued task marks it cancelled without ever running it."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-parallel 1
                   :runner (e-task-queue-test--fake-runner recorder)))
           (_a (e-task-queue-enqueue queue :prompt "a"))
           (b (e-task-queue-enqueue queue :prompt "b"))
           (before (length (e-task-queue-test--recorder-calls recorder))))
      (e-task-queue-cancel queue (plist-get b :task-id))
      (should (eq (plist-get (e-task-queue-get queue (plist-get b :task-id))
                             :status)
                  'cancelled))
      ;; The runner was never invoked for b.
      (should (= before (length (e-task-queue-test--recorder-calls recorder)))))))

(ert-deftest e-task-queue-test-cancel-running-task ()
  "Cancelling a running task invokes its handle cancel and drops the result."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder)))
           (a (e-task-queue-enqueue queue :prompt "a"))
           (task-id (plist-get a :task-id))
           (settle (plist-get (car (e-task-queue-test--recorder-calls recorder))
                              :settle)))
      (e-task-queue-cancel queue task-id)
      (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                  'cancelled))
      ;; A late settle from the runner must not resurrect a cancelled task.
      (funcall settle :status 'done :outputs (list (list :kind 'text :value "x")))
      (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                  'cancelled))
      (should-not (e-task-queue-outputs queue task-id)))))

(ert-deftest e-task-queue-test-list-is-newest-first ()
  "Listing tasks returns them newest-first."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-parallel 1
                   :runner (e-task-queue-test--fake-runner recorder)))
           (a (e-task-queue-enqueue queue :prompt "a"))
           (b (e-task-queue-enqueue queue :prompt "b"))
           (c (e-task-queue-enqueue queue :prompt "c")))
      (should (equal (mapcar (lambda (r) (plist-get r :task-id))
                             (e-task-queue-list queue))
                     (list (plist-get c :task-id)
                           (plist-get b :task-id)
                           (plist-get a :task-id)))))))

(ert-deftest e-task-queue-test-per-task-instance-wins ()
  "A task's explicit harness instance is handed to the runner."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-default t)
    (e-task-queue-test--register-instance :chat-special)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder)))
           (special (e-harness-instance-get-or-create :chat-special)))
      (e-task-queue-enqueue queue :prompt "a" :harness-instance-id :chat-special)
      (should (eq (plist-get (car (e-task-queue-test--recorder-calls recorder))
                             :harness)
                  special)))))

(ert-deftest e-task-queue-test-default-instance-applies ()
  "A task without an instance runs on the queue default."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-default t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :default-harness-instance-id :chat-default
                   :runner (e-task-queue-test--fake-runner recorder)))
           (default (e-harness-instance-get-or-create :chat-default)))
      (e-task-queue-enqueue queue :prompt "a")
      (should (eq (plist-get (car (e-task-queue-test--recorder-calls recorder))
                             :harness)
                  default)))))

(ert-deftest e-task-queue-test-unregistered-instance-fails-task ()
  "An unregistered instance id fails just that task without stalling dispatch."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-default t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-parallel 1
                   :runner (e-task-queue-test--fake-runner recorder)))
           (bad (e-task-queue-enqueue queue :prompt "bad"
                                      :harness-instance-id :chat-missing))
           (good (e-task-queue-enqueue queue :prompt "good")))
      (let ((record (e-task-queue-get queue (plist-get bad :task-id))))
        (should (eq (plist-get record :status) 'failed))
        (should (string-match-p "chat-missing" (plist-get record :error))))
      ;; The dispatcher kept serving: the good task is now running.
      (should (eq (plist-get (e-task-queue-get queue (plist-get good :task-id))
                             :status)
                  'running)))))

(ert-deftest e-task-queue-test-change-hook-fires ()
  "Enqueue and settle run the change hook."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder)))
           (changes 0))
      (let ((e-task-queue-change-functions
             (list (lambda (_queue) (cl-incf changes)))))
        (let* ((task (e-task-queue-enqueue queue :prompt "a"))
               (settle (plist-get (car (e-task-queue-test--recorder-calls
                                        recorder))
                                  :settle)))
          (ignore task)
          (should (> changes 0))
          (let ((before changes))
            (funcall settle :status 'done)
            (should (> changes before))))))))

(ert-deftest e-task-queue-test-default-runner-runs-real-turn ()
  "The default runner submits a turn and settles done with assistant output."
  (e-task-queue-test--with-instances
    (e-harness-instance-register
     :id :chat-real
     :name "Real"
     :kind 'chat
     :default t
     :factory (lambda ()
                (e-harness-create
                 :backend (e-backend-fake-create
                           :items '((:type assistant-message :content "worked")
                                    (:type done :reason stop))))))
    (let* ((queue (e-task-queue-create))
           (task (e-task-queue-enqueue queue :prompt "please work"))
           (task-id (plist-get task :task-id))
           (deadline (+ (float-time) 2.0)))
      (while (and (eq (plist-get (e-task-queue-get queue task-id) :status)
                      'running)
                  (< (float-time) deadline))
        (accept-process-output nil 0.01))
      (let ((record (e-task-queue-get queue task-id)))
        (should (eq (plist-get record :status) 'done))
        (should (plist-get record :session-id))
        (should (cl-find 'text (plist-get record :outputs)
                         :key (lambda (o) (plist-get o :kind))))))))

(ert-deftest e-task-queue-test-synchronous-settle-clears-handle ()
  "A runner that settles inside its own call leaves no stale handle."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((queue (e-task-queue-create
                   :runner (lambda (_task _harness on-settle)
                             (funcall on-settle :status 'done)
                             (list :session-id "s"
                                   :cancel (lambda () (error "must not run"))))))
           (task (e-task-queue-enqueue queue :prompt "go"))
           (task-id (plist-get task :task-id))
           (internal (gethash task-id (e-task-queue-records queue))))
      (should (eq (plist-get internal :status) 'done))
      ;; The settle nilled the handle; the runner-return path must not
      ;; resurrect it on the now-terminal record.
      (should (null (plist-get internal :handle)))
      ;; The session id the handle carried is still recorded.
      (should (equal (plist-get internal :session-id) "s")))))

(provide 'e-task-queue-test)

;;; e-task-queue-test.el ends here

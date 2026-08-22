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
(require 'e-board-orchestration)
(require 'e-board-orchestration-actions)

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

(defun e-task-queue-test--fake-runner-with-session (recorder)
  "Like `e-task-queue-test--fake-runner' but returns a `:session-id' handle.
Auto-retry only arms for a task that produced a session to reference, so retry
tests need a runner whose handle carries one."
  (lambda (task harness on-settle)
    (push (list :task task :harness harness :settle on-settle)
          (e-task-queue-test--recorder-calls recorder))
    (list :session-id "sess-1"
          :cancel (lambda () (funcall on-settle :status 'cancelled)))))

(defun e-task-queue-test--running-ids (queue)
  "Return task ids in QUEUE whose status is running."
  (mapcar (lambda (r) (plist-get r :task-id))
          (cl-remove-if-not
           (lambda (r) (eq (plist-get r :status) 'running))
           (e-task-queue-list queue))))

(defun e-task-queue-test--await-durable (queue)
  "Wait in this test process for QUEUE's asynchronous durability boundary."
  (let ((deadline (+ (float-time) 5.0)) done failure)
    (e-task-queue-finalize
     queue (lambda (_queue) (setq done t)) (lambda (err) (setq failure err)))
    (while (and (not done) (not failure) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should-not failure)
    (should done)
    queue))

(ert-deftest e-task-queue-test-enqueue-returns-record ()
  "Enqueue returns a record and the task is admitted under the cap."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder)))
           (record (e-task-queue-enqueue queue :prompt "do thing")))
      (should (stringp (plist-get record :task-id)))
      (should-not (plist-member record :await-ref))
      (should (equal (plist-get record :prompt) "do thing"))
      (should (eq (plist-get record :harness-instance-id) :chat-a))
      ;; Cap is 2 by default and nothing else is running, so it dispatched.
      (should (eq (plist-get (e-task-queue-get queue (plist-get record :task-id))
                             :status)
                  'running)))))

(ert-deftest e-task-queue-test-public-queue-returns-await-reference ()
  "A queue explicitly exposed to agents returns resolvable task references."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :expose-await-references-p t
                   :runner (e-task-queue-test--fake-runner recorder)))
           (record (e-task-queue-enqueue queue :prompt "do thing")))
      (should (equal (plist-get record :await-ref)
                     (format "task:%s" (plist-get record :task-id)))))))

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

(ert-deftest e-task-queue-test-default-runner-requires-board-binding ()
  "The bundled queue cannot fall back to a standalone harness session."
  (should-error (e-task-queue-enqueue (e-task-queue-create) :prompt "work")
                :type 'e-board-runtime-producer-disabled))

(ert-deftest e-task-queue-test-default-runner-records-unrouted-board-work ()
  "The bundled queue cannot claim completion when no participant picks work up."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
        (e-board-runtime--producer-bindings (make-hash-table :test 'equal))
        (e-board-runtime--producer-inputs (make-hash-table :test 'equal))
        (e-board-runtime--producer-deliveries (make-hash-table :test 'equal))
        (e-board-runtime--producer-turns (make-hash-table :test 'equal))
        (e-board-runtime--producer-epoch 0)
        (e-board-runtime--producer-head nil)
        (e-board-runtime--producer-tail nil)
        (e-board-runtime--producer-drain-scheduled nil)
        (e-board-runtime--producer-scheduler (lambda (_callback)))
        (e-board-runtime--admission-open-p t)
        (e-board-runtime--unsettled-producer-count 0)
        (e-board-runtime--unsettled-generation 0))
    (let* ((board (e-board-registry-create :id "task-board"))
           (binding (e-board-runtime-producer-bind 'tasks board))
           (queue (e-task-queue-create :producer-binding binding))
           (record (e-task-queue-enqueue queue :prompt "please work")))
      (should (eq (plist-get record :status) 'running))
      (should-not (plist-get record :session-id))
      (e-board-runtime-drain-producers)
      (e-board-runtime--drain-input-routing
       board
       (lambda ()
         (e-board-drain-input-classifications
          (e-board-registry-board-source-board board))))
      (setq record (e-task-queue-get queue (plist-get record :task-id)))
      (should (eq (plist-get record :status) 'unrouted))
      (should (string-match-p "no-matching-subscription"
                              (plist-get record :error)))
      (let ((message (car (e-board-messages
                           (e-board-registry-board-source-board board)))))
        (should (eq (e-board-message-kind message) 'input))
        (should (eq (e-board-message-routing-state message) 'unrouted))
        (should (equal (e-board-message-content message) "please work")))
      (should (= (hash-table-count (e-board-registry-board-participants board)) 0)))))

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

(ert-deftest e-task-queue-test-pause-queued-task-holds ()
  "Pausing a queued task holds it in `paused' and frees no running slot."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-parallel 1
                   :runner (e-task-queue-test--fake-runner recorder)))
           (_a (e-task-queue-enqueue queue :prompt "a"))
           (b (e-task-queue-enqueue queue :prompt "b"))
           (before (length (e-task-queue-test--recorder-calls recorder))))
      (e-task-queue-pause queue (plist-get b :task-id))
      (should (eq (plist-get (e-task-queue-get queue (plist-get b :task-id))
                             :status)
                  'paused))
      ;; A paused queued task never starts, so the runner was not called.
      (should (= before (length (e-task-queue-test--recorder-calls recorder)))))))

(ert-deftest e-task-queue-test-pause-running-task-frees-slot ()
  "Pausing a running task aborts at the boundary, lands `paused', frees its slot."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-parallel 1
                   :runner (e-task-queue-test--fake-runner recorder)))
           (a (e-task-queue-enqueue queue :prompt "a"))
           (b (e-task-queue-enqueue queue :prompt "b")))
      ;; a runs, b waits under the cap of 1.
      (should (eq (plist-get (e-task-queue-get queue (plist-get b :task-id))
                             :status)
                  'queued))
      ;; The fake runner's :cancel calls on-settle with `cancelled', which the
      ;; pause intent reinterprets as a pause.
      (e-task-queue-pause queue (plist-get a :task-id))
      (should (eq (plist-get (e-task-queue-get queue (plist-get a :task-id))
                             :status)
                  'paused))
      ;; The freed slot let the queued task b start.
      (should (eq (plist-get (e-task-queue-get queue (plist-get b :task-id))
                             :status)
                  'running)))))

(ert-deftest e-task-queue-test-resume-requeues-and-reruns ()
  "Resuming a paused task returns it to queued and re-runs it."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder)))
           (a (e-task-queue-enqueue queue :prompt "a"))
           (task-id (plist-get a :task-id)))
      (e-task-queue-pause queue task-id)
      (should (eq (plist-get (e-task-queue-get queue task-id) :status) 'paused))
      (let ((before (length (e-task-queue-test--recorder-calls recorder))))
        (e-task-queue-resume queue task-id)
        (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                    'running))
        (should (> (length (e-task-queue-test--recorder-calls recorder))
                   before))))))

(ert-deftest e-task-queue-test-queue-gate-blocks-dispatch ()
  "The queue-level pause gate stops the dispatcher from starting new work."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder))))
      (e-task-queue-pause-all queue)
      (let ((task (e-task-queue-enqueue queue :prompt "held")))
        ;; The gate is set, so an enqueued task stays queued.
        (should (eq (plist-get (e-task-queue-get queue (plist-get task :task-id))
                               :status)
                    'queued))
        (e-task-queue-resume-all queue)
        (should (eq (plist-get (e-task-queue-get queue (plist-get task :task-id))
                               :status)
                    'running))))))

(ert-deftest e-task-queue-test-persistence-round-trip ()
  "Records round-trip through a directory, preserving order and statuses."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let ((dir (make-temp-file "e-task-queue-test" t)))
      (unwind-protect
          (let* ((recorder (make-e-task-queue-test--recorder))
                 (queue (e-task-queue-create
                         :max-parallel 1
                         :directory dir
                         :runner (e-task-queue-test--fake-runner recorder)))
                 (a (e-task-queue-enqueue queue :prompt "first"))
                 (b (e-task-queue-enqueue queue :prompt "second")))
            ;; a is running under the cap, b is queued.
            (e-task-queue-test--await-durable queue)
            (let ((reloaded (e-task-queue-create
                             :max-parallel 1
                             :directory dir
                             ;; A runner that never settles keeps re-queued
                             ;; work observable as running after load.
                             :runner (e-task-queue-test--fake-runner
                                      (make-e-task-queue-test--recorder)))))
              (e-task-queue-load reloaded)
              (should (equal (mapcar (lambda (r) (plist-get r :task-id))
                                     (e-task-queue-list reloaded))
                             (list (plist-get b :task-id)
                                   (plist-get a :task-id))))
              ;; The task that was running at "shutdown" re-runs; the queued
              ;; one waits under the cap of 1.
              (should (eq (plist-get (e-task-queue-get reloaded
                                                       (plist-get a :task-id))
                                     :status)
                          'running))
              (should (eq (plist-get (e-task-queue-get reloaded
                                                       (plist-get b :task-id))
                                     :status)
                          'queued))))
        (delete-directory dir t)))))

(ert-deftest e-task-queue-test-persistence-preserves-terminal-and-paused ()
  "Terminal tasks keep their status on load; paused tasks stay paused."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let ((dir (make-temp-file "e-task-queue-test" t)))
      (unwind-protect
          (let* ((recorder (make-e-task-queue-test--recorder))
                 (queue (e-task-queue-create
                         :directory dir
                         :runner (e-task-queue-test--fake-runner recorder)))
                 (done (e-task-queue-enqueue queue :prompt "done"))
                 (paused (e-task-queue-enqueue queue :prompt "paused")))
            (funcall (plist-get (car (last (e-task-queue-test--recorder-calls
                                            recorder)))
                                :settle)
                     :status 'done)
            (e-task-queue-pause queue (plist-get paused :task-id))
            (e-task-queue-test--await-durable queue)
            (let ((reloaded (e-task-queue-create :directory dir)))
              ;; Keep the gate set so paused/done tasks are not re-dispatched.
              (setf (e-task-queue-paused-p reloaded) t)
              (e-task-queue-load reloaded)
              (should (eq (plist-get (e-task-queue-get
                                      reloaded (plist-get done :task-id))
                                     :status)
                          'done))
              (should (eq (plist-get (e-task-queue-get
                                      reloaded (plist-get paused :task-id))
                                     :status)
                          'paused))))
        (delete-directory dir t)))))

(ert-deftest e-task-queue-test-in-memory-queue-writes-nothing ()
  "A queue with no directory persists nothing."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder))))
      (e-task-queue-enqueue queue :prompt "a")
      (should (null (e-task-queue-directory queue)))
      (should (null (e-task-queue--record-file queue)))
      (should-error (e-task-queue-finalize queue #'ignore #'ignore)
                    :type 'e-task-queue-error))))

(ert-deftest e-task-queue-test-enqueue-enforces-record-and-byte-caps ()
  "Enqueue rejects work beyond fixed record and retained-byte limits."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((e-task-queue-max-records 1)
           (e-task-queue-record-byte-limit 8)
           (recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder))))
      (should-error (e-task-queue-enqueue queue :prompt "123456789")
                    :type 'e-task-queue-error)
      (e-task-queue-enqueue queue :prompt "short")
      (should-error (e-task-queue-enqueue queue :prompt "next")
                    :type 'e-task-queue-error))))

(ert-deftest e-task-queue-test-snapshot-budget-precedes-serialization ()
  "An oversized snapshot is rejected before printer allocation."
  (let* ((queue (e-task-queue-create))
         (e-task-queue-snapshot-byte-limit 8)
         printed)
    (puthash "one" (list :task-id "one" :prompt "123456789")
             (e-task-queue-records queue))
    (setf (e-task-queue-order queue) '("one"))
    (cl-letf (((symbol-function 'prin1-to-string)
               (lambda (_value) (setq printed t) "ignored")))
      (should-error (e-task-queue--snapshot-string queue)
                    :type 'e-task-queue-error)
      (should-not printed))))

(ert-deftest e-task-queue-test-failed-task-auto-retries ()
  "A failed task with retries left is re-armed as a fresh queued attempt.
The retry references the failed session, carries an analyze-and-continue
prompt, preserves the original prompt in `:origin-prompt', and bumps the retry
counter."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-retries 1
                   :runner (e-task-queue-test--fake-runner-with-session
                            recorder)))
           (task (e-task-queue-enqueue queue :prompt "do the thing"))
           (task-id (plist-get task :task-id))
           (settle (plist-get (car (e-task-queue-test--recorder-calls recorder))
                              :settle)))
      (funcall settle :status 'failed :error "boom")
      (let ((record (e-task-queue-get queue task-id)))
        ;; Re-armed rather than terminated.
        (should (eq (plist-get record :status) 'running))
        (should (= (plist-get record :retries) 1))
        (should (equal (plist-get record :origin-prompt) "do the thing"))
        ;; The retry prompt references the failed session and original task.
        (should (string-match-p "sess-1" (plist-get record :prompt)))
        (should (string-match-p "do the thing" (plist-get record :prompt)))
        (should (string-match-p "boom" (plist-get record :prompt))))
      ;; The retry was actually dispatched: the runner ran a second time.
      (should (= (length (e-task-queue-test--recorder-calls recorder)) 2)))))

(ert-deftest e-task-queue-test-retries-exhaust-to-failed ()
  "Once retries are exhausted, a further failure lands terminal `failed'."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-retries 1
                   :runner (e-task-queue-test--fake-runner-with-session
                            recorder)))
           (task (e-task-queue-enqueue queue :prompt "do the thing"))
           (task-id (plist-get task :task-id)))
      ;; First failure arms retry 1.
      (funcall (plist-get (car (e-task-queue-test--recorder-calls recorder))
                          :settle)
               :status 'failed :error "boom-1")
      (should (eq (plist-get (e-task-queue-get queue task-id) :status) 'running))
      ;; Second failure has no retries left: terminal failed, error retained.
      (funcall (plist-get (car (e-task-queue-test--recorder-calls recorder))
                          :settle)
               :status 'failed :error "boom-2")
      (let ((record (e-task-queue-get queue task-id)))
        (should (eq (plist-get record :status) 'failed))
        (should (= (plist-get record :retries) 1))
        (should (equal (plist-get record :error) "boom-2"))
        (should (plist-get record :finished-at))))))

(ert-deftest e-task-queue-test-no-retry-when-disabled ()
  "With retries disabled, a failure terminates immediately."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-retries 0
                   :runner (e-task-queue-test--fake-runner-with-session
                            recorder)))
           (task (e-task-queue-enqueue queue :prompt "do the thing"))
           (task-id (plist-get task :task-id))
           (settle (plist-get (car (e-task-queue-test--recorder-calls recorder))
                              :settle)))
      (funcall settle :status 'failed :error "boom")
      (let ((record (e-task-queue-get queue task-id)))
        (should (eq (plist-get record :status) 'failed))
        (should (= (plist-get record :retries) 0))))))

(ert-deftest e-task-queue-test-no-retry-without-session ()
  "A failed task with no session to reference is not retried.
The retry prompt must be able to point the new attempt at the failed session;
without one there is nothing to analyze, so the task terminates."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :max-retries 2
                   ;; The plain fake runner returns no `:session-id'.
                   :runner (e-task-queue-test--fake-runner recorder)))
           (task (e-task-queue-enqueue queue :prompt "do the thing"))
           (task-id (plist-get task :task-id))
           (settle (plist-get (car (e-task-queue-test--recorder-calls recorder))
                              :settle)))
      (funcall settle :status 'failed :error "boom")
      (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                  'failed)))))

(ert-deftest e-task-queue-test-retry-persists-fields ()
  "Retry counter and origin prompt round-trip through persistence."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let ((dir (make-temp-file "e-task-queue-test" t)))
      (unwind-protect
          (let* ((recorder (make-e-task-queue-test--recorder))
                 (queue (e-task-queue-create
                         :max-retries 1
                         :directory dir
                         :runner (e-task-queue-test--fake-runner-with-session
                                  recorder)))
                 (task (e-task-queue-enqueue queue :prompt "do the thing"))
                 (task-id (plist-get task :task-id))
                 (settle (plist-get (car (e-task-queue-test--recorder-calls
                                          recorder))
                                    :settle)))
            (funcall settle :status 'failed :error "boom")
            (e-task-queue-test--await-durable queue)
            (let ((reloaded (e-task-queue-create
                             :max-retries 1
                             :directory dir
                             :runner (e-task-queue-test--fake-runner-with-session
                                      (make-e-task-queue-test--recorder)))))
              (e-task-queue-load reloaded)
              (let ((record (e-task-queue-get reloaded task-id)))
                (should (= (plist-get record :retries) 1))
                (should (equal (plist-get record :origin-prompt)
                               "do the thing")))))
        (delete-directory dir t)))))

(ert-deftest e-task-queue-test-write-timer-owns-quiescence-slot ()
  "A coalesced domain write remains visible until its callback returns."
  (let ((e-task-queue--unsettled-write-count 0)
        (e-task-queue--failed-write-count 0)
        (e-task-queue--unsettled-generation 0)
        callback
        wrote)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq callback (lambda () (apply function arguments)))
                 (timer-create)))
              ((symbol-function 'e-task-queue--start-async-write)
               (lambda (_queue)
                 (setq wrote t)
                 (e-task-queue--adjust-writer-state 'writes -1))))
      (let ((queue (e-task-queue-create :directory "/tmp/task-writer-test")))
        (e-task-queue--schedule-write queue)
        (should (equal (e-task-queue-unsettled-state)
                       '(:generation 1 :writes 1 :failures 0)))
        (funcall callback)
        (should wrote)
        (should (= (plist-get (e-task-queue-unsettled-state) :writes) 0))))))

(ert-deftest e-task-queue-test-dirty-writer-keeps-one-quiescence-slot ()
  "A dirty worker handoff never exposes a false quiescent edge."
  (let* ((queue (e-task-queue-create :directory "/tmp/task-writer-test"))
         (e-task-queue--unsettled-write-count 1)
         (e-task-queue--failed-write-count 0)
         (e-task-queue--unsettled-generation 1)
         (first 'first)
         (second 'second)
         started)
    (setf (e-task-queue-write-process queue) first
          (e-task-queue-write-dirty-p queue) t)
    (cl-letf (((symbol-function 'process-status) (lambda (_process) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_process) 0))
              ((symbol-function 'e-task-queue--start-async-write)
               (lambda (target)
                 (setq started t)
                 (setf (e-task-queue-write-process target) second))))
      (e-task-queue--writer-finished queue first)
      (should started)
      (should (= (plist-get (e-task-queue-unsettled-state) :writes) 1))
      (e-task-queue--writer-finished queue second)
      (should (= (plist-get (e-task-queue-unsettled-state) :writes) 0)))))

(ert-deftest e-task-queue-test-writer-uses-global-process-environment ()
  "Writer discovery and launch ignore unrelated buffer-local process state."
  (let ((directory (make-temp-file "e-task-queue-writer-environment-" t))
        (global-exec-path (default-value 'exec-path))
        (global-process-environment (default-value 'process-environment))
        discovery-environment
        launch-environment
        sent)
    (unwind-protect
        (with-temp-buffer
          (setq-local exec-path '("/buffer-only"))
          (setq-local process-environment '("PATH=/buffer-only"))
          (let ((queue (e-task-queue-create :directory directory)))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (_executable)
                         (setq discovery-environment
                               (list exec-path process-environment))
                         "/global/node"))
                      ((symbol-function 'make-process)
                       (lambda (&rest _arguments)
                         (setq launch-environment
                               (list exec-path process-environment))
                         'writer))
                      ((symbol-function 'process-send-string)
                       (lambda (_process snapshot) (setq sent snapshot)))
                      ((symbol-function 'process-send-eof) #'ignore))
              (e-task-queue--start-async-write queue)))
          (should (equal discovery-environment
                         (list global-exec-path global-process-environment)))
          (should (equal launch-environment
                         (list global-exec-path global-process-environment)))
          (should (stringp sent)))
      (delete-directory directory t))))

(provide 'e-task-queue-test)

;;; e-task-queue-test.el ends here

(defun e-task-queue-test--orchestration-manifest (board &optional attempt)
  "Publish a one-task manifest selecting ATTEMPT to BOARD."
  (e-board-orchestration-publish-fact
   board
   (list :version 1 :type 'manifest :idempotency-key "manifest"
         :payload (list :run-id "run-1"
                        :tasks (list (list :task-key "task" :required t
                                           :accepted-attempt (or attempt 0)))
                        :deadline '(:kind none)))))

(ert-deftest e-task-queue-test-orchestration-bridge-is-idempotent ()
  "One selected manifest attempt maps to one durable queue task and report."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((board (e-board-create :id "queue-orchestration"))
           (recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create :runner (e-task-queue-test--fake-runner recorder))))
      (e-task-queue-test--orchestration-manifest board)
      (let* ((first (e-board-orchestration-actions-dispatch-queue-task
                     queue board :run-id "run-1" :task-key "task" :attempt 0 :prompt "do"))
             (again (e-board-orchestration-actions-dispatch-queue-task
                     queue board :run-id "run-1" :task-key "task" :attempt 0 :prompt "do"))
             (settle (plist-get (car (e-task-queue-test--recorder-calls recorder)) :settle)))
        (should (equal (plist-get first :task-id) (plist-get again :task-id)))
        (should-not (plist-member first :await-ref))
        (should (equal (plist-get (plist-get first :metadata) :board-run-id) "run-1"))
        (funcall settle :status 'done :outputs '((:kind text :value "ok")))
        (let ((projection (e-board-orchestration-project-board board)))
          (should (eq (plist-get projection :terminal-status) 'done))
          (should (= (length (plist-get projection :reports)) 1)))))))

(ert-deftest e-task-queue-test-orchestration-retry-uses-new-attempt ()
  "Queue retry changes durable attempt metadata without changing the manifest."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((board (e-board-create :id "queue-retry"))
           (recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create :max-retries 1
                                       :runner (e-task-queue-test--fake-runner-with-session recorder))))
      (e-task-queue-test--orchestration-manifest board)
      (let* ((record (e-board-orchestration-actions-dispatch-queue-task
                      queue board :run-id "run-1" :task-key "task" :attempt 0 :prompt "do"))
             (first-settle (plist-get (car (e-task-queue-test--recorder-calls recorder)) :settle)))
        (funcall first-settle :status 'failed :error "retry")
        (let ((retried (e-task-queue-get queue (plist-get record :task-id))))
          (should (eq (plist-get retried :status) 'running))
          (should (= (plist-get (plist-get retried :metadata) :board-attempt) 1)))))))

(ert-deftest e-task-queue-test-orchestration-metadata-survives-save-load ()
  "Durable queue snapshots retain run, task, and attempt bridge metadata."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let ((dir (make-temp-file "e-task-queue-orchestration" t)))
      (unwind-protect
          (let* ((recorder (make-e-task-queue-test--recorder))
                 (queue (e-task-queue-create
                         :directory dir :runner (e-task-queue-test--fake-runner recorder)))
                 (task (e-task-queue-enqueue
                        queue :prompt "do" :metadata '(:board-run-id "run-1"
                                                        :board-task-key "task"
                                                        :board-attempt 0)))
                 (task-id (plist-get task :task-id)))
            (e-task-queue-test--await-durable queue)
            (let ((reloaded (e-task-queue-create
                             :directory dir :runner (e-task-queue-test--fake-runner recorder))))
              (e-task-queue-load reloaded)
              (should (equal (plist-get (plist-get (e-task-queue-get reloaded task-id)
                                                  :metadata)
                                        :board-task-key)
                             "task"))))
        (delete-directory dir t)))))

(ert-deftest e-task-queue-test-orchestration-restart-restores-required-and-optional-work ()
  "A restored journal and queue accept one report per task and reconcile once."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let ((directory (make-temp-file "e-task-queue-run-restart" t))
          (e-board--registry (make-hash-table :test 'equal))
          (e-board-registry--boards (make-hash-table :test 'equal))
          (e-board-registry--board-index nil)
          (e-board-orchestration-actions--queue-boards
           (make-hash-table :test 'equal)))
      (unwind-protect
          (let* ((source-runtime (e-board-registry-create :id "restart-run"))
                 (source (e-board-registry-board-source-board source-runtime))
                 (initial-recorder (make-e-task-queue-test--recorder))
                 (queue (e-task-queue-create
                         :directory directory
                         :runner (e-task-queue-test--fake-runner initial-recorder))))
            (e-board-orchestration-publish-fact
             source
             (list :version 1 :type 'manifest :idempotency-key "manifest"
                   :payload
                   '(:run-id "run-1"
                     :tasks ((:task-key "required" :required t :accepted-attempt 0)
                             (:task-key "optional" :required nil :accepted-attempt 0))
                     :deadline (:kind none)
                     :continuation (:session-id "coordinator" :prompt "reconcile"
                                    :publication-key "continuation-1"))))
            (dolist (task '(("required" . "required work")
                            ("optional" . "optional work")))
              (e-board-orchestration-actions-dispatch-queue-task
               queue source :run-id "run-1" :task-key (car task) :attempt 0
               :prompt (cdr task)))
            (e-task-queue-test--await-durable queue)
            (let ((journal (mapcar #'e-chat-service--board-envelope
                                   (e-board-messages source))))
              ;; A fresh process reconstructs only durable journal and queue state.
              (setq e-board--registry (make-hash-table :test 'equal)
                    e-board-registry--boards (make-hash-table :test 'equal)
                    e-board-registry--board-index nil)
              (let* ((restored-runtime (e-board-registry-create :id "restart-run"))
                     (restored (e-board-registry-board-source-board restored-runtime))
                     (restored-recorder (make-e-task-queue-test--recorder))
                     (reloaded (e-task-queue-create
                                :directory directory
                                :runner (e-task-queue-test--fake-runner restored-recorder))))
                (e-board-orchestration-mark-restoring restored)
                (dolist (envelope journal)
                  (e-board-import-message restored envelope))
                (e-board-orchestration-mark-restored restored)
                (e-task-queue-load reloaded)
                (dolist (record (e-task-queue-list reloaded))
                  (puthash (plist-get record :task-id) restored
                           e-board-orchestration-actions--queue-boards))
                ;; Load starts each restored task with the fake runner.  Settle
                ;; each once, independent of call order.
                (dolist (task-key '("required" "optional"))
                  (let* ((call (cl-find task-key
                                        (e-task-queue-test--recorder-calls restored-recorder)
                                        :key (lambda (item)
                                               (plist-get (plist-get item :task) :metadata))
                                        :test (lambda (key metadata)
                                                (equal key (plist-get metadata :board-task-key)))))
                         (settle (plist-get call :settle)))
                    (should settle)
                    (funcall settle :status 'done
                             :outputs (list (list :kind 'text :value task-key)))))
                (let ((projection (e-board-orchestration-run-projection restored "run-1")))
                  (should (eq (plist-get projection :terminal-status) 'done))
                  (should (= (length (plist-get projection :reports)) 2)))
                (let ((binding (e-chat-service--binding-create
                                :harness nil :session-id "coordinator"
                                :board restored-runtime))
                      queued)
                  (cl-letf (((symbol-function 'e-chat-service-queue-session)
                             (lambda (&rest arguments) (push arguments queued))))
                    (e-chat-service--reconcile-board-continuation binding)
                    (e-chat-service--reconcile-board-continuation binding))
                  (should (= (length queued) 1))
                  (should (equal (plist-get (nthcdr 3 (car queued)) :source-input-key)
                                 '("orchestration-continuation" "continuation-1" 0))))))
        (delete-directory directory t))))))

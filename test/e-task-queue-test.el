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
(require 'e-chat-service)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-task-queue)
(require 'e-runtime-store)
(require 'e-session-sqlite)
(require 'e-task-storage-sqlite)
(load (expand-file-name "e-board-producer-test-support.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

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

(defun e-task-queue-test--wait-until (predicate &optional timeout)
  "Wait at this explicit test boundary until PREDICATE succeeds."
  (let ((deadline (+ (float-time) (or timeout 5.0))) value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

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

(ert-deftest e-task-queue-test-default-runner-requires-sql-target ()
  "The bundled queue cannot fall back to a standalone harness session."
  (should-error (e-task-queue-enqueue (e-task-queue-create) :prompt "work")
                :type 'e-task-queue-error))

(ert-deftest e-task-queue-test-default-runner-zero-match-stays-unrouted ()
  "A committed Board append without a pickup is visibly unrouted, not done."
  (e-board-producer-test-with-target (target)
    (let* ((queue (e-task-queue-create :publication-target target))
           (task '(:task-id "task-unrouted" :prompt "please work"
                   :summary "please work" :attempt-id "attempt-1"))
           settlement
           (runner-handle
            (e-task-queue-default-runner
             task queue (lambda (&rest result) (setq settlement result)))))
      (should-not settlement)
      (e-board-producer-test-await (plist-get runner-handle :publication))
      (should (eq (plist-get settlement :status) 'unrouted))
      (should (string-match-p "no-matching-subscription"
                              (plist-get settlement :error)))
      (let ((message (car (e-board-producer-test-records target))))
        (should (eq (plist-get message :kind) 'input))
        (should (equal (plist-get message :content) "please work"))
        (should (equal (plist-get message :tags) '(task-queue task)))))))

(ert-deftest e-task-queue-test-default-runner-rejected-commit-fails-task ()
  "A rejected Board commit fails its task instead of acknowledging execution."
  (e-board-producer-test-with-target (_target service)
    (let* ((missing-target
            (e-board-sqlite-publication-target-create
             service "missing-task-board" :author "task-test"))
           (queue (e-task-queue-create :publication-target missing-target))
           (task (e-task-queue-enqueue queue :prompt "cannot route"))
           (task-id (plist-get task :task-id)))
      (should
       (e-task-queue-test--wait-until
        (lambda ()
          (eq (plist-get (e-task-queue-get queue task-id) :status) 'failed))))
      (should
       (string-match-p
        "Unknown Board"
        (plist-get (e-task-queue-get queue task-id) :error))))))

(ert-deftest e-task-queue-test-default-runner-waits-for-delivery-outcome ()
  "A matched task stays running past COMMIT and settles from its real turn."
  (e-board-producer-test-with-target (target service board-id runtime)
    (e-board-producer-test-admit-participant
     service board-id "task-worker-session" "task-worker" '(:tags (task)))
    (let* ((store-directory (make-temp-file "e-task-live-sql-" t))
           (store
            (e-session-sqlite-store-create
             store-directory :runtime-store runtime :asynchronous t))
           (harness
            (e-harness-create
             :sessions store
             :backend
             (e-backend-fake-create
              :items '((:type assistant-message :content "task complete")
                       (:type done :reason stop)))))
           binding)
      (unwind-protect
          (progn
            (setq binding
                  (e-board-producer-test-await
                   (e-chat-service-binding-start
                    harness "task-worker-session")))
            (let* ((queue
                    (e-task-queue-create
                     ;; TARGET and BINDING intentionally use separate service
                     ;; values over the same runtime.  Live pickup wakeup is
                     ;; transport-scoped, not dependent on object identity.
                     :publication-target target))
                   (task
                    (e-task-queue-enqueue queue :prompt "please work"))
                   (task-id (plist-get task :task-id)))
              ;; COMMIT creates a pickup, but execution is still live work.
              (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                          'running))
              (should
               (e-task-queue-test--wait-until
                (lambda ()
                  (not
                   (eq (plist-get (e-task-queue-get queue task-id) :status)
                       'running)))))
              (let ((settled (e-task-queue-get queue task-id)))
                (should (eq (plist-get settled :status) 'done))
                (should
                 (equal
                  (plist-get
                   (car
                    (plist-get
                     (plist-get (car (plist-get settled :outputs)) :value)
                     :deliveries))
                   :status)
                  'done)))
              (should
               (= (hash-table-count
                   (e-board-sqlite-service--delivery-observer-table
                    (e-chat-service-binding-sqlite-service binding)))
                  0))
              (e-chat-service-close-board binding)
              (should
               (= (hash-table-count
                   (e-board-sqlite-service--pickup-observer-table
                    (e-chat-service-binding-sqlite-service binding)))
                  0))
              (setq binding nil)))
        (when binding
          (e-chat-service-close-board binding))
        (ignore-errors (e-session-sqlite-store-close store))
        (delete-directory store-directory t)))))

(ert-deftest e-task-queue-test-controller-close-cancels-live-delivery-work ()
  "Retiring a routed controller cannot strand its producer task running."
  (e-board-producer-test-with-target (target service board-id runtime)
    (e-board-producer-test-admit-participant
     service board-id "task-close-session" "task-close-worker" '(:tags (task)))
    (let* ((store-directory (make-temp-file "e-task-close-sql-" t))
           (store
            (e-session-sqlite-store-create
             store-directory :runtime-store runtime :asynchronous t))
           (harness
            (e-harness-create
             :sessions store
             :backend
             (e-backend-fake-create
              :delay 2.0
              :items '((:type assistant-message :content "too late")
                       (:type done :reason stop)))))
           binding)
      (unwind-protect
          (progn
            (setq binding
                  (e-board-producer-test-await
                   (e-chat-service-binding-start
                    harness "task-close-session")))
            (let* ((queue (e-task-queue-create :publication-target target))
                   (task (e-task-queue-enqueue queue :prompt "close during work"))
                   (task-id (plist-get task :task-id))
                   (observer-table
                    (e-board-sqlite-service--delivery-observer-table service)))
              (should
               (e-task-queue-test--wait-until
                (lambda ()
                  (> (hash-table-count
                      (e-chat-service-binding-executing-turns binding))
                     0))))
              (let (delivery-id)
                (maphash
                 (lambda (candidate _turn-id)
                   (setq delivery-id candidate))
                 (e-chat-service-binding-executing-turns binding))
                ;; A defective observer must not prevent the task observer for
                ;; the same canonical delivery from being settled on retire.
                (e-board-sqlite-publication-target-observe-delivery-outcome
                 target delivery-id
                 (lambda (_status _payload)
                   (error "defective retirement observer"))))
              (should (> (hash-table-count observer-table) 0))
              (e-chat-service-close-board binding)
              (should (eq (plist-get (e-task-queue-get queue task-id) :status)
                          'cancelled))
              (should (zerop (hash-table-count observer-table)))
              (should
               (zerop
                (hash-table-count
                 (e-chat-service-binding-executing-turns binding))))
              (should-not (e-chat-service-binding harness "task-close-session"))))
        (when binding
          (ignore-errors
            (e-harness-attached-turn-port-abort
             (e-chat-service-binding-turn-port binding)))
          (when (e-chat-service--binding-live-p binding)
            (e-chat-service-close-board binding)))
        (ignore-errors (e-session-sqlite-store-close store))
        (delete-directory store-directory t)))))

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

(ert-deftest e-task-queue-test-in-memory-queue-writes-nothing ()
  "A queue with no directory persists nothing."
  (e-task-queue-test--with-instances
    (e-task-queue-test--register-instance :chat-a t)
    (let* ((recorder (make-e-task-queue-test--recorder))
           (queue (e-task-queue-create
                   :runner (e-task-queue-test--fake-runner recorder))))
      (e-task-queue-enqueue queue :prompt "a")
      (should (null (e-task-queue-directory queue)))
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

(ert-deftest e-task-queue-test-file-construction-requires-offline-migration ()
  "The retired file-backed constructor is never an ordinary fallback."
  (should-error (e-task-queue-create :directory "/tmp/retired-task-store")
                :type 'e-task-queue-error))

(ert-deftest e-task-queue-test-startup-load-issues-no-sql-and-start-claims ()
  "Startup initializes no durable mirror; explicit scheduler start claims."
  (let* ((calls nil)
         (storage
          (e-task-storage--create
           :runtime :test-runtime
           :call-operation
           (lambda (&rest arguments)
             (error "Unexpected blocking task storage operation: %S"
                    arguments))
           :submit-operation
           (lambda (kind operation arguments on-settle)
             (push (list kind operation arguments) calls)
             (funcall on-settle '(:claimed-p nil :paused-p nil) nil)
             :request)))
         (queue (e-task-queue-create :storage storage :id "first-use")))
    (should-not calls)
    (e-task-queue-load queue)
    (should-not calls)
    (should (e-task-queue-loaded-p queue))
    (e-task-queue-start queue)
    (should (= (length calls) 1))
    (should (equal (caar calls) 'write))
    (should (equal (cadar calls) 'claim-runnable))
    (should (= (hash-table-count (e-task-queue-records queue)) 0))))

(ert-deftest e-task-queue-test-durable-enqueue-releases-nonexecuting-copy ()
  "A committed queued task remains only in SQLite until atomically claimed."
  (let* ((calls nil)
         (storage
          (e-task-storage--create
           :runtime :test-runtime
           :call-operation
           (lambda (&rest arguments)
             (error "Unexpected blocking task storage operation: %S"
                    arguments))
           :submit-operation
           (lambda (kind operation arguments on-settle)
             (setq calls (append calls (list (list kind operation arguments))))
             (funcall on-settle
                      (if (eq operation 'enqueue)
                          '(:revision 1)
                        '(:claimed-p nil :paused-p nil))
                      nil)
             :request)))
         (queue
          (e-task-queue-create
           :storage storage :id "release-copy"
           :runner (lambda (&rest _) (ert-fail "No task was claimed"))))
         (record (e-task-queue-enqueue queue :prompt "persist only")))
    (should (eq (plist-get record :status) 'queued))
    (should (= (hash-table-count (e-task-queue-records queue)) 0))
    (should-not (e-task-queue-order queue))
    (should-not (e-task-queue-work-handle
                 queue (plist-get record :task-id)))
    (should (equal (mapcar #'cadr calls) '(enqueue claim-runnable)))))

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

(provide 'e-task-queue-test)

;;; e-task-queue-test.el ends here

;;; e-task-storage-sqlite.el --- SQLite task storage adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Maps the task-owned port to typed runtime-store commands.  It owns no queue
;; policy, runner, Board publication, SQL, or live work handles.

;;; Code:

(require 'e-runtime-store)
(require 'e-task-storage)

(defun e-task-storage-sqlite--body (operation arguments)
  "Return the typed worker body for OPERATION and ARGUMENTS."
  (pcase operation
    ('open-queue
     (list :op 'task-queue-open :queue-id (car arguments)))
    ('enqueue
     (pcase-let ((`(,queue-id ,record) arguments))
       (list :op 'task-enqueue :queue-id queue-id :record record)))
    ('claim-runnable
     (pcase-let ((`(,queue-id ,started-at ,instance-id) arguments))
       (list :op 'task-runnable-claim :queue-id queue-id
             :started-at started-at :harness-instance-id instance-id)))
    ('transition
     (pcase-let ((`(,queue-id ,task-id ,expected-status ,record . ,rest)
                  arguments))
       (list :op 'task-transition :queue-id queue-id :task-id task-id
             :expected-status expected-status :record record
             :attempt-transition (car rest))))
    ('set-paused
     (pcase-let ((`(,queue-id ,paused-p) arguments))
       (list :op 'task-queue-pause :queue-id queue-id :paused-p paused-p)))
    ('status
     (list :op 'task-queue-status :queue-id (car arguments)))
    (_ (signal 'e-task-storage-error
               (list "Unknown asynchronous task storage operation"
                     operation)))))

(defun e-task-storage-sqlite--submit
    (runtime kind operation arguments on-settle)
  "Submit typed task OPERATION through RUNTIME and observe settlement."
  (let* ((body (e-task-storage-sqlite--body operation arguments))
         (queue-id (plist-get body :queue-id))
         (request
          (e-runtime-store--submit-owned
           runtime kind body
           (and (eq kind 'write) (cons 'task queue-id)))))
    (e-runtime-store--observe
     request
     (lambda (settled)
       (if (eq (e-runtime-store-request--state settled) 'committed)
           (funcall on-settle (e-runtime-store-request--result settled) nil)
         (funcall on-settle nil
                  (or (e-runtime-store-request--error settled)
                      '(e-task-storage-error
                        "Runtime request did not commit"))))))
    request))

(defun e-task-storage-sqlite--call (runtime operation arguments)
  "Dispatch task OPERATION ARGUMENTS through RUNTIME."
  (pcase operation
    ('open-queue
     (e-runtime-store-call
      runtime 'write (list :op 'task-queue-open :queue-id (car arguments))))
    ('enqueue
     (pcase-let ((`(,queue-id ,position ,record) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'task-enqueue :queue-id queue-id
              :position position :record record))))
    ('claim
     (pcase-let
         ((`(,queue-id ,task-id ,attempt-id ,started-at ,instance-id)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'task-claim :queue-id queue-id
              :task-id task-id :attempt-id attempt-id :started-at started-at
              :harness-instance-id instance-id))))
    ('transition
     (pcase-let
         ((`(,queue-id ,task-id ,expected-status ,record . ,rest)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'task-transition :queue-id queue-id
              :task-id task-id :expected-status expected-status
              :record record :attempt-transition (car rest)))))
    ('set-paused
     (pcase-let ((`(,queue-id ,paused-p) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'task-queue-pause :queue-id queue-id :paused-p paused-p))))
    ('snapshot
     (pcase-let ((`(,queue-id ,limit) arguments))
       (e-runtime-store-call
        runtime 'read
        (list :op 'task-snapshot :queue-id queue-id :limit limit))))
    ('delete-history
     (pcase-let ((`(,queue-id) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'task-history-delete :queue-id queue-id))))
    ('import-legacy-snapshot
     (pcase-let ((`(,queue-id ,snapshot) arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'task-import-legacy-snapshot :queue-id queue-id
              :snapshot snapshot))))
    (_ (signal 'e-task-storage-error
               (list "Unknown task storage operation" operation)))))

(defun e-task-storage-sqlite-create (runtime)
  "Return a task storage port backed by shared RUNTIME."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (e-task-storage--create
   :runtime runtime
   :call-operation
   (lambda (operation &rest arguments)
     (e-task-storage-sqlite--call runtime operation arguments))
   :submit-operation
   (lambda (kind operation arguments on-settle)
     (e-task-storage-sqlite--submit
      runtime kind operation arguments on-settle))))

(provide 'e-task-storage-sqlite)

;;; e-task-storage-sqlite.el ends here

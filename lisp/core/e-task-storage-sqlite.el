;;; e-task-storage-sqlite.el --- SQLite task storage adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Maps the task-owned port to typed runtime-store commands.  It owns no queue
;; policy, runner, Board publication, SQL, or live work handles.

;;; Code:

(require 'e-runtime-store)
(require 'e-task-storage)

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
         ((`(,queue-id ,task-id ,expected-status ,record)
           arguments))
       (e-runtime-store-call
        runtime 'write
        (list :op 'task-transition :queue-id queue-id
              :task-id task-id :expected-status expected-status
              :record record))))
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
     (e-task-storage-sqlite--call runtime operation arguments))))

(provide 'e-task-storage-sqlite)

;;; e-task-storage-sqlite.el ends here

;;; e-board-orchestration-actions.el --- Durable run action helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These helpers resolve a durable assignment from session metadata.  They do
;; not consult the live subagent registry, so a child report remains meaningful
;; after the registry is rebuilt or absent.

;;; Code:

(require 'e-board-orchestration)
(require 'e-chat-service)
(require 'e-task-queue)

(defun e-board-orchestration-actions-assignment (harness session-id)
  "Return SESSION-ID's durable orchestration assignment on HARNESS, or nil."
  (when-let* ((session (e-chat-service-session harness session-id))
              (metadata (plist-get session :metadata))
              (run-id (plist-get metadata :board-run-id))
              (task-key (plist-get metadata :board-task-key))
              (attempt (plist-get metadata :board-attempt)))
    (list :run-id run-id :task-key task-key :attempt attempt)))


(defun e-board-orchestration-actions--source-board (board)
  "Return the core board owned by runtime BOARD."
  (if (e-board-p board)
      board
    (e-board-registry-board-source-board board)))

(defun e-board-orchestration-actions-terminal-key (assignment)
  "Return the stable terminal-report idempotency key for ASSIGNMENT."
  (format "terminal:%s:%s:%d"
          (plist-get assignment :run-id)
          (plist-get assignment :task-key)
          (plist-get assignment :attempt)))

(cl-defun e-board-orchestration-actions-publish-terminal
    (board assignment status &key summary outputs error author)
  "Publish ASSIGNMENT's bounded terminal STATUS report to BOARD.
The stable assignment key makes callback retries no-ops at the board boundary."
  (e-board-orchestration-publish-fact
   (e-board-orchestration-actions--source-board board)
   (list :version e-board-orchestration-fact-version
         :type 'terminal-report
         :idempotency-key (e-board-orchestration-actions-terminal-key assignment)
         :payload (append (copy-tree assignment)
                          (list :status status :summary (or summary "")
                                :outputs (or outputs []) :error error)))
   :author author))

(cl-defun e-board-orchestration-actions-report-from-context
    (context &key summary outputs)
  "Publish a structured terminal report using CONTEXT's persisted assignment.
Return nil for ordinary children without a durable assignment."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (assignment (and harness session-id
                          (e-board-orchestration-actions-assignment harness session-id))))
    (when assignment
      (let ((board (e-chat-service-binding-board
                    (e-chat-service-ensure-binding harness session-id))))
        (e-board-orchestration-actions-publish-terminal
         board assignment 'done :summary summary :outputs outputs
         :author (list :session-id session-id))))))

(provide 'e-board-orchestration-actions)

;;; e-board-orchestration-actions.el ends here

(defvar e-board-orchestration-actions--queue-boards (make-hash-table :test 'equal)
  "Live core boards keyed by queue task id for durable bridge callbacks.")

(defun e-board-orchestration-actions--queue-assignment (record)
  "Return durable orchestration assignment stored in queue RECORD, or nil."
  (let ((metadata (plist-get record :metadata)))
    (when-let ((run-id (plist-get metadata :board-run-id)))
      (list :run-id run-id
            :task-key (plist-get metadata :board-task-key)
            :attempt (plist-get metadata :board-attempt)))))

(defun e-board-orchestration-actions--queue-board (record)
  "Resolve RECORD's core board from its live bridge or durable board id."
  (or (gethash (plist-get record :task-id) e-board-orchestration-actions--queue-boards)
      (when-let ((board-id (plist-get (plist-get record :metadata) :board-id)))
        (when-let ((board (e-board-registry-get board-id)))
          (e-board-orchestration-actions--source-board board)))))

(defun e-board-orchestration-actions--attempt-key (assignment status)
  "Return the stable task-attempt fact key for ASSIGNMENT and STATUS."
  (format "attempt:%s:%s:%d:%s"
          (plist-get assignment :run-id) (plist-get assignment :task-key)
          (plist-get assignment :attempt) status))

(defun e-board-orchestration-actions--publish-attempt (board assignment status)
  "Publish one idempotent durable task ATTEMPT state."
  (e-board-orchestration-publish-fact
   board
   (list :version e-board-orchestration-fact-version :type 'task-attempt
         :idempotency-key (e-board-orchestration-actions--attempt-key assignment status)
         :payload (append (copy-tree assignment) (list :status status)))))

(defun e-board-orchestration-actions--queue-terminal (queue record)
  "Turn a terminal QUEUE RECORD into its assigned durable terminal report."
  (ignore queue)
  (when-let* ((assignment (e-board-orchestration-actions--queue-assignment record))
              (board (e-board-orchestration-actions--queue-board record)))
    (e-board-orchestration-actions-publish-terminal
     board assignment (plist-get record :status)
     :summary (e-task-queue-record-display-summary record)
     :outputs (plist-get record :outputs) :error (plist-get record :error))))

(defun e-board-orchestration-actions--queue-rehydrate (queue record persisted-status)
  "Reconcile a restored queue RECORD's durable assignment before dispatch."
  (ignore queue)
  (when (and (eq persisted-status 'running)
             (e-board-orchestration-actions--queue-assignment record))
    (when-let ((board (e-board-orchestration-actions--queue-board record)))
      (e-board-orchestration-actions--publish-attempt
       board (e-board-orchestration-actions--queue-assignment record) 'queued))))

(add-hook 'e-task-queue-terminal-functions #'e-board-orchestration-actions--queue-terminal)
(add-hook 'e-task-queue-rehydrate-functions #'e-board-orchestration-actions--queue-rehydrate)

(cl-defun e-board-orchestration-actions-dispatch-queue-task
    (queue board &key run-id task-key attempt prompt summary harness-instance-id)
  "Dispatch one manifest-selected attempt through durable QUEUE.
A duplicate dispatch returns the existing task.  Queue retries use new attempt
metadata but never alter the manifest's accepted-attempt selection."
  (let* ((board (e-board-orchestration-actions--source-board board))
         (projection (e-board-orchestration-project-board board))
         (task (cl-find task-key (plist-get projection :tasks)
                        :key (lambda (item) (plist-get item :task-key)) :test #'equal)))
    (unless (and (equal run-id (plist-get projection :run-id)) task
                 (= attempt (plist-get task :accepted-attempt)))
      (signal 'e-board-orchestration-error
              (list "Task attempt is not selected by the manifest" run-id task-key attempt)))
    (or (cl-find-if (lambda (record)
                      (equal (e-board-orchestration-actions--queue-assignment record)
                             (list :run-id run-id :task-key task-key :attempt attempt)))
                    (e-task-queue-list queue))
        (let* ((metadata (list :board-run-id run-id :board-task-key task-key
                               :board-attempt attempt :board-id (e-board-id board)))
               (record (e-task-queue-enqueue
                        queue :prompt prompt :summary summary :metadata metadata
                        :harness-instance-id harness-instance-id)))
          (puthash (plist-get record :task-id) board e-board-orchestration-actions--queue-boards)
          (e-board-orchestration-actions--publish-attempt
           board (e-board-orchestration-actions--queue-assignment record)
           (plist-get record :status))
          record))))

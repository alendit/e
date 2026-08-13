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

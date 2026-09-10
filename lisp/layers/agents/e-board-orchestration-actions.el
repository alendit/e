;;; e-board-orchestration-actions.el --- Durable run action helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These helpers publish and query durable orchestration facts through one
;; explicit SQL Board target.  The in-memory subagent registry may retain live
;; execution handles, but it is never consulted for durable run state.

;;; Code:

(require 'e-board-orchestration)
(require 'e-board-sqlite-service)
(require 'e-chat-service)
(require 'e-work)

(defun e-board-orchestration-actions-assignment-from-metadata (metadata)
  "Return the durable orchestration assignment in detached METADATA, or nil."
  (when-let* ((run-id (plist-get metadata :board-run-id))
              (task-key (plist-get metadata :board-task-key))
              (attempt (plist-get metadata :board-attempt)))
    (list :run-id run-id :task-key task-key :attempt attempt)))


(defun e-board-orchestration-actions--sqlite-target-p (target)
  "Return non-nil when TARGET is an explicit SQL publication address."
  (e-board-sqlite-publication-target-valid-p target))

(defun e-board-orchestration-actions-terminal-key (assignment)
  "Return the stable terminal-report idempotency key for ASSIGNMENT."
  (format "terminal:%s:%s:%d"
          (plist-get assignment :run-id)
          (plist-get assignment :task-key)
          (plist-get assignment :attempt)))

(cl-defun e-board-orchestration-actions-publish-terminal
    (target assignment status &key summary outputs error author)
  "Publish ASSIGNMENT's bounded terminal STATUS report to TARGET.
The stable assignment key makes callback retries no-ops at the board boundary."
  (let ((fact
         (list :version e-board-orchestration-fact-version
               :type 'terminal-report
               :idempotency-key
               (e-board-orchestration-actions-terminal-key assignment)
               :payload (append (copy-tree assignment)
                                (list :status status :summary (or summary "")
                                      :outputs (or outputs []) :error error
                                      :participant-session-id
                                      (plist-get author :session-id))))))
    (e-board-sqlite-publication-target-orchestration-fact-start
     target fact :author author)))

(defconst e-board-orchestration-actions-run-limit 32
  "Maximum durable runs returned by one observation action.")

(defconst e-board-orchestration-actions-task-limit 64
  "Maximum tasks, reports, and conflicts retained in one run observation.")

(defun e-board-orchestration-actions--take (items limit)
  "Return at most LIMIT ITEMS as a fresh list."
  (cl-subseq items 0 (min (length items) limit)))

(defun e-board-orchestration-actions--bounded-items (items limit)
  "Return ITEMS clipped to LIMIT with truncation evidence."
  (let ((items (or items nil)))
    (list :items (copy-tree (e-board-orchestration-actions--take items limit))
          :truncated (> (length items) limit))))

(defun e-board-orchestration-actions--bounded-projection (projection)
  "Return the bounded observation form of durable run PROJECTION."
  (if (memq (plist-get projection :state) '(missing not-restored-yet))
      (copy-tree projection)
    (let* ((tasks (e-board-orchestration-actions--bounded-items
                   (plist-get projection :tasks) e-board-orchestration-actions-task-limit))
           (reports (e-board-orchestration-actions--bounded-items
                     (delq nil (mapcar (lambda (task)
                                         (plist-get task :accepted-report))
                                       (plist-get projection :tasks)))
                     e-board-orchestration-actions-task-limit))
           (conflicts (e-board-orchestration-actions--bounded-items
                       (plist-get projection :conflicts) e-board-orchestration-actions-task-limit))
           (manifest (copy-tree (plist-get projection :manifest))))
      (plist-put manifest :tasks (plist-get tasks :items))
      (append
       (list :run-id (plist-get projection :run-id)
             :manifest manifest
             :tasks (plist-get tasks :items)
             :accepted-reports (plist-get reports :items)
             :conflicts (plist-get conflicts :items)
             :deadline (copy-tree (plist-get projection :deadline))
             :continuation (copy-tree (plist-get projection :continuation))
             :terminal-status (plist-get projection :terminal-status))
       (when (plist-get tasks :truncated) (list :tasks-truncated t))
       (when (plist-get reports :truncated) (list :accepted-reports-truncated t))
       (when (plist-get conflicts :truncated) (list :conflicts-truncated t))))))

(defconst e-board-orchestration-actions--mapped-work-spec
  (e-work-spec-create
   :id "board-orchestration-query" :execution 'cooperative
   :interactive-policy 'async :owner 'subagents
   :runner
   (lambda (parent arguments _context)
     (let ((child (plist-get arguments :child))
           (mapper (plist-get arguments :mapper)))
       (setf (e-work-handle-cancel-function parent)
             (lambda (_handle) (e-work-cancel child)))
       (e-work-on-settle
        child
        (lambda (settled)
          (pcase (plist-get (e-work-status settled) :state)
            ('finished
             (condition-case err
                 (e-work-finish parent
                                (funcall mapper
                                         (e-work-handle-result settled)))
               (error (e-work-fail parent err))))
            ('failed (e-work-fail parent (e-work-handle-error settled)))
            ('cancelled (e-work-cancel parent)))))
       :deferred)))
  "Work contract for detached SQL query result mapping.")

(defun e-board-orchestration-actions--map-work (child mapper)
  "Return request-scoped work mapping CHILD through MAPPER."
  (e-work-start e-board-orchestration-actions--mapped-work-spec
                (list :child child :mapper mapper)))

(defun e-board-orchestration-actions--records (page)
  "Return detached Board records from SQL PAGE."
  (mapcar (lambda (row) (copy-tree (plist-get row :record) t))
          (plist-get page :records)))

(defun e-board-orchestration-actions--sql-run-projection (page now)
  "Reduce one detached SQL orchestration PAGE at NOW."
  (when (plist-get page :truncated)
    (signal 'e-board-orchestration-error
            (list "Run fact page exceeds bounded query" (plist-get page :run-id))))
  (let ((records (e-board-orchestration-actions--records page)))
    (if records
        (e-board-orchestration-actions--bounded-projection
         (e-board-orchestration-reduce records now))
      (list :run-id (plist-get page :run-id) :state 'missing))))

(defun e-board-orchestration-actions-run-projection (target run-id &optional now)
  "Return request-scoped work for TARGET's bounded RUN-ID projection."
  (unless (e-board-orchestration-actions--sqlite-target-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (e-board-orchestration-actions--map-work
   (e-board-sqlite-publication-target-orchestration-run-start target run-id)
   (lambda (page)
     (e-board-orchestration-actions--sql-run-projection page now))))

(defun e-board-orchestration-actions--sql-run-list (page now)
  "Reduce SQL PAGE into a bounded newest-first run list at NOW."
  (when (plist-get page :truncated)
    (signal 'e-board-orchestration-error
            (list "Run list exceeds bounded query")))
  (let ((groups (make-hash-table :test 'equal)) order)
    (dolist (record (e-board-orchestration-actions--records page))
      (when-let* ((fact (e-board-orchestration-fact-from-record record))
                  (run-id (plist-get (plist-get fact :payload) :run-id)))
        (puthash run-id (append (gethash run-id groups) (list record)) groups)
        (when (eq (plist-get fact :type) 'manifest)
          (push run-id order))))
    (mapcar
     (lambda (run-id)
       (e-board-orchestration-actions--bounded-projection
        (e-board-orchestration-reduce (gethash run-id groups) now)))
     order)))

(defun e-board-orchestration-actions-list-runs (target &optional now)
  "Return request-scoped work for TARGET's bounded durable run list."
  (unless (e-board-orchestration-actions--sqlite-target-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (e-board-orchestration-actions--map-work
   (e-board-sqlite-publication-target-orchestration-runs-start
    target e-board-orchestration-actions-run-limit)
   (lambda (page)
     (e-board-orchestration-actions--sql-run-list page now))))

(defun e-board-orchestration-actions--context-target (context)
  "Return an explicit SQL target for CONTEXT's live presentation binding."
  (let ((harness (plist-get context :harness))
        (session-id (plist-get context :session-id)))
    (unless (and harness session-id)
      (signal 'wrong-type-argument (list 'e-board-context context)))
    (let ((binding (e-chat-service-binding harness session-id)))
      (unless binding
        (signal 'e-board-orchestration-error
                (list "Board binding is not ready" session-id)))
      (e-chat-service-publication-target binding))))

(defun e-board-orchestration-actions--run-id (arguments)
  "Return the required run id from action ARGUMENTS."
  (let ((run-id (plist-get arguments :run-id)))
    (unless (stringp run-id)
      (signal 'wrong-type-argument (list 'stringp :run-id)))
    run-id))

(defun e-board-orchestration-actions--status (context arguments)
  "Return CONTEXT board's bounded projection for the requested run."
  (e-board-orchestration-actions-run-projection
   (e-board-orchestration-actions--context-target context)
   (e-board-orchestration-actions--run-id arguments)))

(defun e-board-orchestration-actions--list (context _arguments)
  "Return bounded durable run projections for CONTEXT's board."
  (e-board-orchestration-actions-list-runs
   (e-board-orchestration-actions--context-target context)))

(defconst e-board-orchestration-actions--run-id-parameters
  '(:type "object"
    :properties
    (:run-id (:type "string" :description "Durable board run id from its manifest."))
    :required ["run-id"])
  "Action parameters for one durable run lookup.")

(defun e-board-orchestration-actions--action (handler parameters)
  "Return one async-capable durable run observation action for HANDLER."
  (e-action-create
   :parameters parameters
   :work
   (e-work-spec-create
    :id "board-orchestration-action" :execution 'cooperative
    :interactive-policy 'async :owner 'subagents
    :runner
    (lambda (parent arguments context)
      (let ((result (funcall handler context arguments)))
        (if (not (e-work-handle-p result))
            result
          (setf (e-work-handle-cancel-function parent)
                (lambda (_handle) (e-work-cancel result)))
          (e-work-on-settle
           result
           (lambda (settled)
             (pcase (plist-get (e-work-status settled) :state)
               ('finished
                (e-work-finish parent (e-work-handle-result settled)))
               ('failed (e-work-fail parent (e-work-handle-error settled)))
               ('cancelled (e-work-cancel parent)))))
          :deferred))))))

(defun e-board-orchestration-actions-parent-alist ()
  "Return parent actions that expose durable board run observations."
  (list :list-runs
        (e-board-orchestration-actions--action
         #'e-board-orchestration-actions--list nil)
        :run-status
        (e-board-orchestration-actions--action
         #'e-board-orchestration-actions--status
         e-board-orchestration-actions--run-id-parameters)))

(provide 'e-board-orchestration-actions)

;;; e-board-orchestration-actions.el ends here

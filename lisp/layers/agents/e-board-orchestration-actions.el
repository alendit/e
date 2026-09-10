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
(require 'e-work)

(defun e-board-orchestration-actions-assignment-from-metadata (metadata)
  "Return the durable orchestration assignment in detached METADATA, or nil."
  (when-let* ((run-id (plist-get metadata :board-run-id))
              (task-key (plist-get metadata :board-task-key))
              (attempt (plist-get metadata :board-attempt)))
    (list :run-id run-id :task-key task-key :attempt attempt)))


(defun e-board-orchestration-actions--source-board (board)
  "Return the core board owned by runtime BOARD."
  (if (e-board-p board)
      board
    (e-board-registry-board-source-board board)))

(defun e-board-orchestration-actions--sqlite-target-p (target)
  "Return non-nil when TARGET is an ordinary SQL chat binding."
  (and (e-chat-service-binding-p target)
       (e-board-sqlite-service-p
        (e-chat-service-binding-sqlite-service target))))

(defun e-board-orchestration-actions--target (harness session-id)
  "Return SESSION-ID's live SQL binding or explicit ephemeral Board."
  (let ((binding (e-chat-service-binding harness session-id)))
    (unless binding
      (signal 'e-board-orchestration-error
              (list "Board binding is not ready" session-id)))
    (if (e-board-orchestration-actions--sqlite-target-p binding)
        binding
      (e-board-orchestration-actions--source-board
       (e-chat-service-binding-board binding)))))

(defun e-board-orchestration-actions-terminal-key (assignment)
  "Return the stable terminal-report idempotency key for ASSIGNMENT."
  (format "terminal:%s:%s:%d"
          (plist-get assignment :run-id)
          (plist-get assignment :task-key)
          (plist-get assignment :attempt)))

(defconst e-board-orchestration-actions--terminal-publication-work-spec
  (e-work-spec-create
   :id "board-terminal-publication" :execution 'cooperative
   :interactive-policy 'async :owner 'subagents
   :runner (lambda (_handle _arguments _context) :deferred))
  "Work contract for terminal publication and its durable continuation.")

(defun e-board-orchestration-actions--finish-from-work
    (outer child &optional result)
  "Settle OUTER from CHILD, returning RESULT after CHILD succeeds."
  (e-work-on-settle
   child
   (lambda (settled)
     (pcase (plist-get (e-work-status settled) :state)
       ('finished (e-work-finish outer
                                 (if result result
                                   (e-work-handle-result settled))))
       ('failed (e-work-fail outer (e-work-handle-error settled)))
       ('cancelled (e-work-cancel outer))))))

(defun e-board-orchestration-actions--publish-sql-continuation
    (target outer terminal-result projection)
  "Publish PROJECTION's pending continuation through SQL TARGET."
  (let ((continuation (plist-get projection :continuation)))
    (if (not (eq (plist-get continuation :state) 'pending))
        (e-work-finish outer terminal-result)
      (let* ((run-id (plist-get projection :run-id))
             (publication-key (plist-get continuation :publication-key))
             (queue-work
              (e-chat-service-queue-session
               (e-chat-service-binding-harness target)
               (plist-get continuation :session-id)
               (plist-get continuation :prompt)
               :metadata (list :board-run-id run-id)
               :source-input-key (list 'board-continuation publication-key))))
        (e-work-on-settle
         queue-work
         (lambda (settled)
           (pcase (plist-get (e-work-status settled) :state)
             ('finished
              (let ((claim-work
                     (e-board-sqlite-service-orchestration-fact-start
                      (e-chat-service-binding-sqlite-service target)
                      (e-chat-service-binding-board-id target)
                      (list
                       :version e-board-orchestration-fact-version
                       :type 'continuation-claim
                       :idempotency-key
                       (e-board-orchestration-continuation-claim-key
                        publication-key 'published)
                       :payload
                       (list :run-id run-id :publication-key publication-key
                             :status 'published)))))
                (e-board-orchestration-actions--finish-from-work
                 outer claim-work terminal-result)))
             ('failed (e-work-fail outer (e-work-handle-error settled)))
             ('cancelled (e-work-cancel outer)))))))))

(defun e-board-orchestration-actions--publish-sql-terminal
    (target fact author)
  "Publish terminal FACT and its ready continuation through SQL TARGET."
  (let* ((outer
          (e-work-start
           e-board-orchestration-actions--terminal-publication-work-spec nil))
         (publication
          (e-board-sqlite-service-orchestration-fact-start
           (e-chat-service-binding-sqlite-service target)
           (e-chat-service-binding-board-id target) fact :author author)))
    (e-work-on-settle
     publication
     (lambda (settled)
       (pcase (plist-get (e-work-status settled) :state)
         ('finished
          (let* ((terminal-result (e-work-handle-result settled))
                 (run-id (plist-get (plist-get fact :payload) :run-id))
                 (query
                  (e-board-orchestration-actions-run-projection target run-id)))
            (e-work-on-settle
             query
             (lambda (queried)
               (pcase (plist-get (e-work-status queried) :state)
                 ('finished
                  (e-board-orchestration-actions--publish-sql-continuation
                   target outer terminal-result
                   (e-work-handle-result queried)))
                 ('failed (e-work-fail outer (e-work-handle-error queried)))
                 ('cancelled (e-work-cancel outer)))))))
         ('failed (e-work-fail outer (e-work-handle-error settled)))
         ('cancelled (e-work-cancel outer)))))
    outer))

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
    (if (e-board-orchestration-actions--sqlite-target-p target)
        (e-board-orchestration-actions--publish-sql-terminal
         target fact author)
      (e-board-orchestration-publish-fact
       (e-board-orchestration-actions--source-board target) fact
       :author author))))

(cl-defun e-board-orchestration-actions-report-from-context
    (context &key summary outputs)
  "Publish a structured terminal report using CONTEXT's persisted assignment.
Return nil for ordinary children without a durable assignment."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (assignment
          (and harness session-id
               (e-board-orchestration-actions-assignment-from-metadata
                (plist-get context :session-metadata)))))
    (when assignment
      (e-board-orchestration-actions-publish-terminal
       (e-board-orchestration-actions--target harness session-id)
       assignment 'done :summary summary :outputs outputs
       :author (list :session-id session-id)))))

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

(defun e-board-orchestration-actions--publish-attempt (target assignment status)
  "Publish one idempotent durable task ATTEMPT state."
  (let ((fact
         (list :version e-board-orchestration-fact-version :type 'task-attempt
               :idempotency-key
               (e-board-orchestration-actions--attempt-key assignment status)
               :payload
               (append (copy-tree assignment) (list :status status)))))
    (if (e-board-orchestration-actions--sqlite-target-p target)
        (e-board-sqlite-service-orchestration-fact-start
         (e-chat-service-binding-sqlite-service target)
         (e-chat-service-binding-board-id target) fact)
      (e-board-orchestration-publish-fact
       (e-board-orchestration-actions--source-board target) fact))))

(defun e-board-orchestration-actions-select-next-attempt
    (board run-id task-key accepted-attempt)
  "Durably select and return the successor to ACCEPTED-ATTEMPT.
The current projection must still select the exact named attempt.  Repeating
the same selection is an idempotent success; skipping an attempt is rejected."
  (let* ((board (e-board-orchestration-actions--source-board board))
         (projection (e-board-orchestration-run-projection board run-id))
         (task (cl-find task-key (plist-get projection :tasks)
                        :key (lambda (item) (plist-get item :task-key))
                        :test #'equal))
         (current (and task (plist-get task :accepted-attempt)))
         (next (1+ accepted-attempt)))
    (cond
     ((and task (= current next)) next)
     ((not (and task (= current accepted-attempt)
                (not (memq (plist-get task :state)
                           '(done failed cancelled)))))
      (signal 'e-board-orchestration-error
              (list "Task attempt is not retry-selectable"
                    run-id task-key accepted-attempt current)))
     (t
      (e-board-orchestration-publish-fact
       board
       (list :version e-board-orchestration-fact-version
             :type 'attempt-selection
             :idempotency-key
             (format "attempt-selection:%s:%s:%d" run-id task-key next)
             :payload (list :run-id run-id :task-key task-key :attempt next)))
      next))))

(defun e-board-orchestration-actions--queue-terminal (queue record)
  "Turn a terminal QUEUE RECORD into its assigned durable terminal report."
  (ignore queue)
  (when-let* ((assignment (e-board-orchestration-actions--queue-assignment record))
              (board (e-board-orchestration-actions--queue-board record)))
    (e-board-orchestration-actions-publish-terminal
     board assignment (plist-get record :status)
     :summary (e-task-queue-record-display-summary record)
     :outputs (plist-get record :outputs) :error (plist-get record :error))))

(add-hook 'e-task-queue-terminal-functions #'e-board-orchestration-actions--queue-terminal)

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

(defconst e-board-orchestration-actions-run-limit 32
  "Maximum durable runs returned by one observation action.")

(defconst e-board-orchestration-actions-task-limit 64
  "Maximum tasks, reports, and conflicts retained in one run observation.")

(defvar e-board-orchestration-actions-projection-change-functions nil
  "Functions called after a durable run projection changes.
Each function receives the core board, run id, and bounded projection.  This
notification path observes board facts and never reads child sessions.")

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
  "Return TARGET's bounded RUN-ID projection or SQL query work."
  (if (e-board-orchestration-actions--sqlite-target-p target)
      (let ((work
             (e-board-sqlite-service-orchestration-run-start
              (e-chat-service-binding-sqlite-service target)
              (e-chat-service-binding-board-id target) run-id)))
        (e-board-orchestration-actions--map-work
         work
         (lambda (page)
           (e-board-orchestration-actions--sql-run-projection page now))))
    (e-board-orchestration-actions--bounded-projection
     (e-board-orchestration-run-projection
      (e-board-orchestration-actions--source-board target) run-id now))))

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
  "Return TARGET's bounded run list or SQL query work."
  (if (e-board-orchestration-actions--sqlite-target-p target)
      (let ((work
             (e-board-sqlite-service-orchestration-runs-start
              (e-chat-service-binding-sqlite-service target)
              (e-chat-service-binding-board-id target)
              e-board-orchestration-actions-run-limit)))
        (e-board-orchestration-actions--map-work
         work
         (lambda (page)
           (e-board-orchestration-actions--sql-run-list page now))))
    (let* ((board (e-board-orchestration-actions--source-board target))
           (run-ids (e-board-orchestration-actions--take
                     (e-board-orchestration-run-ids board)
                     e-board-orchestration-actions-run-limit)))
      (mapcar (lambda (run-id)
                (e-board-orchestration-actions-run-projection board run-id now))
              run-ids))))

(defun e-board-orchestration-actions--notify-projection (original board fact &rest arguments)
  "Publish a bounded projection update when ORIGINAL posts durable FACT."
  (let ((publication (apply original board fact arguments)))
    (when (eq (e-board-publication-status publication) 'posted)
      (let* ((board (e-board-orchestration-actions--source-board board))
             (payload (plist-get fact :payload))
             (run-id (plist-get payload :run-id))
             (report-before-manifest
              (and (eq (plist-get fact :type) 'terminal-report)
                   (not (member run-id (e-board-orchestration-run-ids board))))))
        ;; The persisted report is replayed when its manifest becomes visible.
        (unless report-before-manifest
          (run-hook-with-args 'e-board-orchestration-actions-projection-change-functions
                              board run-id
                              (e-board-orchestration-actions-run-projection board run-id)))))
    publication))

(unless (advice-member-p #'e-board-orchestration-actions--notify-projection
                         'e-board-orchestration-publish-fact)
  (advice-add 'e-board-orchestration-publish-fact :around
              #'e-board-orchestration-actions--notify-projection))

(defun e-board-orchestration-actions--context-target (context)
  "Return CONTEXT's already-live SQL binding or ephemeral Board."
  (let ((harness (plist-get context :harness))
        (session-id (plist-get context :session-id)))
    (unless (and harness session-id)
      (signal 'wrong-type-argument (list 'e-board-context context)))
    (e-board-orchestration-actions--target harness session-id)))

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

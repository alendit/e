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
(require 'e-json)
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
    (target assignment status &key summary result outputs error author generation)
  "Publish ASSIGNMENT's bounded terminal STATUS report to TARGET.
The stable assignment key makes callback retries no-ops at the board boundary.
When GENERATION is non-nil, require that it is still current."
  (let ((fact
         (list :version e-board-orchestration-fact-version
               :type 'terminal-report
               :idempotency-key
               (e-board-orchestration-actions-terminal-key assignment)
               :payload (append (copy-tree assignment)
                                (list :status status :summary (or summary "")
                                      :outputs (or outputs []) :error error)
                                (when result (list :result result))
                                (list
                                      :participant-session-id
                                      (plist-get author :session-id))))))
    (let ((work
           (e-board-sqlite-publication-target-orchestration-fact-start
            target fact :author author :generation generation)))
      (e-work-on-settle
       work
       (lambda (settled)
         (when (eq (plist-get (e-work-status settled) :state) 'finished)
           (e-chat-service-reconcile-sqlite-continuation-target target))))
      work)))

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
             :continuation-outcome
             (copy-tree (plist-get projection :continuation-outcome) t)
             :terminal-status (plist-get projection :terminal-status))
       (when (plist-get tasks :truncated) (list :tasks-truncated t))
       (when (plist-get reports :truncated) (list :accepted-reports-truncated t))
       (when (plist-get conflicts :truncated) (list :conflicts-truncated t))))))

(defconst e-board-orchestration-actions--mapped-work-spec
  (e-work-spec-create
   :id "board-orchestration-query" :execution 'cooperative
   :interactive-policy 'async :owner 'board
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

(defun e-board-orchestration-actions-run-projection
    (target run-id &optional now generation)
  "Return request-scoped work for TARGET's bounded RUN-ID projection.
When GENERATION is non-nil, fence the exact run read to that Board generation."
  (unless (e-board-orchestration-actions--sqlite-target-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (e-board-orchestration-actions--map-work
   (e-board-sqlite-publication-target-orchestration-run-start
    target run-id nil generation)
   (lambda (page)
     (e-board-orchestration-actions--sql-run-projection page now))))

(defun e-board-orchestration-actions--sql-run-list (page now)
  "Reduce SQL PAGE into a bounded newest-first run list at NOW."
  (when (plist-get page :truncated)
    (signal 'e-board-orchestration-error
            (list "Run list exceeds bounded query")))
  (let ((groups (make-hash-table :test 'equal)) order)
    (dolist (row (plist-get page :records))
      (let ((record (copy-tree (plist-get row :record) t)))
        (when-let* ((fact (e-board-orchestration-fact-from-record record))
                  (run-id (plist-get (plist-get fact :payload) :run-id)))
          (puthash run-id
                   (append (gethash run-id groups)
                           (list (list :record record
                                       :position (plist-get row :position))))
                   groups)
          (when (eq (plist-get fact :type) 'manifest)
            (push run-id order)))))
    (mapcar
     (lambda (run-id)
       (let* ((rows (gethash run-id groups))
              (projection
               (e-board-orchestration-actions--bounded-projection
                (e-board-orchestration-reduce
                 (mapcar (lambda (row) (plist-get row :record)) rows)
                 now)))
              (latest (car (last rows))))
         (plist-put
          (plist-put projection :latest-event-position
                     (or (plist-get latest :position) 0))
          :latest-event-at
          (plist-get (plist-get latest :record) :created-at))))
     order)))

(defconst e-board-orchestration-actions--run-list-after-reconciliation-spec
  (e-work-spec-create
   :id "board-orchestration-run-list-after-reconciliation"
   :execution 'cooperative :interactive-policy 'async :owner 'board
   :runner
   (lambda (parent arguments _context)
     (let* ((prerequisite (plist-get arguments :prerequisite))
            (target (plist-get arguments :target))
            (now (plist-get arguments :now))
            query)
       (setf (e-work-handle-cancel-function parent)
             (lambda (_handle)
               (when (and query
                          (not (memq (plist-get (e-work-status query) :state)
                                     '(finished failed cancelled))))
                 (e-work-cancel query))
               (when (and prerequisite
                          (not (memq (plist-get (e-work-status prerequisite) :state)
                                     '(finished failed cancelled))))
                 (e-work-cancel prerequisite))))
       (e-work-on-settle
        prerequisite
        (lambda (settled)
          (pcase (plist-get (e-work-status settled) :state)
            ('finished
             (setq query
                   (e-board-sqlite-publication-target-orchestration-runs-start
                    target e-board-orchestration-actions-run-limit))
             (e-work-on-settle
              query
              (lambda (query-settled)
                (pcase (plist-get (e-work-status query-settled) :state)
                  ('finished
                   (condition-case query-error
                       (e-work-finish
                        parent
                        (e-board-orchestration-actions--sql-run-list
                         (e-work-handle-result query-settled) now))
                     (error (e-work-fail parent query-error))))
                  ('failed
                   (e-work-fail parent
                                (e-work-handle-error query-settled)))
                  ('cancelled (e-work-cancel parent))))))
            ('failed (e-work-fail parent (e-work-handle-error settled)))
            ('cancelled (e-work-cancel parent))))))
       :deferred))
  "Work contract for a run-list query behind outcome reconciliation.")

(defun e-board-orchestration-actions-list-runs (target &optional now)
  "Return request-scoped work for TARGET's bounded durable run list."
  (unless (e-board-orchestration-actions--sqlite-target-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (let ((reconciliation
         (e-chat-service-reconcile-sqlite-continuation-backfill-target target)))
    (if reconciliation
        (e-work-start
         e-board-orchestration-actions--run-list-after-reconciliation-spec
         (list :prerequisite reconciliation :target target :now now))
      (e-board-orchestration-actions--map-work
       (e-board-sqlite-publication-target-orchestration-runs-start
        target e-board-orchestration-actions-run-limit)
       (lambda (page)
         (e-board-orchestration-actions--sql-run-list page now))))))

(defun e-board-orchestration-actions--sql-run-set
    (page now &optional record-limit byte-limit)
  "Map indexed SQL PAGE into the consumer-shaped run set at NOW."
  (unless (and (integerp (plist-get page :active-count))
               (listp (plist-get page :runs)))
    (signal 'e-board-orchestration-error
            (list "Active run query returned an invalid index page")))
  (e-board-orchestration-run-set-projection
   (plist-get page :runs)
   :board-id (plist-get page :board-id)
   :more-p (plist-get page :more-p)
   :active-count (plist-get page :active-count)
   :now (or now (plist-get page :as-of))
   :record-limit (or record-limit
                     e-board-orchestration-run-set-default-record-limit)
   :byte-limit (or byte-limit
                   e-board-orchestration-run-set-default-byte-limit)))

(cl-defun e-board-orchestration-actions-run-set
    (target &optional now &key
            (limit e-board-orchestration-actions-run-limit)
            (byte-limit e-board-orchestration-run-set-default-byte-limit))
  "Return request-scoped work for TARGET's bounded active run-set.
LIMIT bounds the durable run query and returned projection; BYTE-LIMIT bounds
the detached result.  Neither may exceed the corresponding run-set maximum."
  (unless (e-board-orchestration-actions--sqlite-target-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (unless (and (integerp limit) (> limit 0)
               (<= limit e-board-orchestration-run-set-max-record-limit))
    (signal 'e-board-orchestration-error
            (list "Run-set query limit is out of bounds"
                  limit e-board-orchestration-run-set-max-record-limit)))
  (unless (and (integerp byte-limit) (> byte-limit 0)
               (<= byte-limit e-board-orchestration-run-set-max-byte-limit))
    (signal 'e-board-orchestration-error
            (list "Run-set byte limit is out of bounds"
                  byte-limit e-board-orchestration-run-set-max-byte-limit)))
  (e-board-orchestration-actions--map-work
   (e-board-sqlite-publication-target-orchestration-active-runs-start
    target limit now)
   (lambda (page)
     (let ((value (e-board-orchestration-actions--sql-run-set
                   page now limit byte-limit)))
       (plist-put value :board-id
                  (e-board-sqlite-publication-target-board-id target))))))

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

(defun e-board-orchestration-actions--run-set-list (context _arguments)
  "Return the consumer-shaped active run-set for CONTEXT's Board."
  (e-board-orchestration-actions-run-set
   (e-board-orchestration-actions--context-target context)))

(defun e-board-orchestration-actions--canonical-string (value)
  "Return VALUE as a canonical string or explicit JSON null."
  (cond
   ((stringp value) value)
   ((null value) e-json-null)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun e-board-orchestration-actions--canonical-number (value)
  "Return finite numeric VALUE or explicit JSON null."
  (if (and (numberp value) (e-json-value-p value)) value e-json-null))

(defun e-board-orchestration-actions--canonical-bool (value)
  "Return VALUE as a canonical JSON boolean."
  (if (eq value t) t e-json-false))

(defun e-board-orchestration-actions--canonical-value (value)
  "Return canonical VALUE, or explicit null for a domain-owned value."
  (if (e-json-value-p value) value e-json-null))

(defun e-board-orchestration-actions--canonical-output (value)
  "Project one orchestration output VALUE into canonical JSON."
  (list :kind (e-board-orchestration-actions--canonical-string
               (plist-get value :kind))
        :uri (e-board-orchestration-actions--canonical-string
              (plist-get value :uri))
        :value (e-board-orchestration-actions--canonical-string
                (plist-get value :value))
        :label (e-board-orchestration-actions--canonical-string
                (plist-get value :label))))

(defun e-board-orchestration-actions--canonical-report (value)
  "Project one terminal REPORT VALUE into canonical JSON."
  (list :run-id (e-board-orchestration-actions--canonical-string
                 (plist-get value :run-id))
        :task-key (e-board-orchestration-actions--canonical-string
                   (plist-get value :task-key))
        :attempt (e-board-orchestration-actions--canonical-number
                  (plist-get value :attempt))
        :status (e-board-orchestration-actions--canonical-string
                 (plist-get value :status))
        :summary (e-board-orchestration-actions--canonical-string
                  (plist-get value :summary))
        :error (e-board-orchestration-actions--canonical-string
                (plist-get value :error))
        :participant-session-id
        (e-board-orchestration-actions--canonical-string
         (plist-get value :participant-session-id))
        :outputs
        (vconcat (mapcar #'e-board-orchestration-actions--canonical-output
                         (or (plist-get value :outputs) nil)))
        :result (e-board-orchestration-actions--canonical-value
                 (plist-get value :result))))

(defun e-board-orchestration-actions--canonical-task (value)
  "Project one reduced orchestration TASK into canonical JSON."
  (list :task-key (e-board-orchestration-actions--canonical-string
                   (plist-get value :task-key))
        :required (e-board-orchestration-actions--canonical-bool
                   (plist-get value :required))
        :accepted-attempt
        (e-board-orchestration-actions--canonical-number
         (plist-get value :accepted-attempt))
        :state (e-board-orchestration-actions--canonical-string
                (plist-get value :state))
        :accepted-report
        (if (plist-get value :accepted-report)
            (e-board-orchestration-actions--canonical-report
             (plist-get value :accepted-report))
          e-json-null)))

(defun e-board-orchestration-actions--canonical-conflict (value)
  "Project one orchestration CONFLICT VALUE into canonical JSON."
  (list :run-id (e-board-orchestration-actions--canonical-string
                 (plist-get value :run-id))
        :task-key (e-board-orchestration-actions--canonical-string
                   (plist-get value :task-key))
        :attempt (e-board-orchestration-actions--canonical-number
                  (plist-get value :attempt))
        :reason (e-board-orchestration-actions--canonical-string
                 (plist-get value :reason))))

(defun e-board-orchestration-actions--canonical-deadline (value)
  "Project an orchestration DEADLINE VALUE into canonical JSON."
  (list :kind (e-board-orchestration-actions--canonical-string
               (plist-get value :kind))
        :at (e-board-orchestration-actions--canonical-number
             (plist-get value :at))
        :expired (e-board-orchestration-actions--canonical-bool
                  (plist-get value :expired))))

(defun e-board-orchestration-actions--canonical-continuation (value)
  "Project a durable CONTINUATION VALUE into canonical JSON."
  (list :session-id (e-board-orchestration-actions--canonical-string
                     (plist-get value :session-id))
        :prompt (e-board-orchestration-actions--canonical-string
                 (plist-get value :prompt))
        :publication-key (e-board-orchestration-actions--canonical-string
                          (plist-get value :publication-key))
        :state (e-board-orchestration-actions--canonical-string
                (plist-get value :state))
        :claims
        (vconcat
         (mapcar
          (lambda (claim)
            (list :run-id (e-board-orchestration-actions--canonical-string
                           (plist-get claim :run-id))
                  :publication-key
                  (e-board-orchestration-actions--canonical-string
                   (plist-get claim :publication-key))
                  :status
                  (e-board-orchestration-actions--canonical-string
                   (plist-get claim :status))
                  :error
                  (e-board-orchestration-actions--canonical-string
                   (plist-get claim :error))))
          (or (plist-get value :claims) nil)))
        :execution-outcome
        (if (plist-get value :execution-outcome)
            (e-board-orchestration-actions--canonical-continuation-outcome
             (plist-get value :execution-outcome))
          e-json-null)))

(defun e-board-orchestration-actions--canonical-continuation-outcome (value)
  "Project one generic continuation execution OUTCOME into canonical JSON."
  (list :run-id (e-board-orchestration-actions--canonical-string
                 (plist-get value :run-id))
        :publication-key
        (e-board-orchestration-actions--canonical-string
         (plist-get value :publication-key))
        :status (e-board-orchestration-actions--canonical-string
                 (plist-get value :status))
        :turn-id (e-board-orchestration-actions--canonical-string
                  (plist-get value :turn-id))
        :error (e-board-orchestration-actions--canonical-string
                (plist-get value :error))
        :conflict (e-board-orchestration-actions--canonical-bool
                   (plist-get value :conflict))))

(defun e-board-orchestration-actions--canonical-manifest (value)
  "Project a durable orchestration MANIFEST into canonical JSON."
  (list :run-id (e-board-orchestration-actions--canonical-string
                 (plist-get value :run-id))
        :tasks (vconcat (mapcar #'e-board-orchestration-actions--canonical-task
                                (or (plist-get value :tasks) nil)))
        :deadline (if (plist-get value :deadline)
                      (e-board-orchestration-actions--canonical-deadline
                       (plist-get value :deadline))
                    e-json-null)
        :continuation (if (plist-get value :continuation)
                          (e-board-orchestration-actions--canonical-continuation
                           (plist-get value :continuation))
                        e-json-null)
        :descriptor (e-board-orchestration-actions--canonical-value
                     (plist-get value :descriptor))))

(defun e-board-orchestration-actions--canonical-projection (value)
  "Project one reduced orchestration PROJECTION into canonical JSON."
  (list :run-id (e-board-orchestration-actions--canonical-string
                 (plist-get value :run-id))
        :state (e-board-orchestration-actions--canonical-string
                (plist-get value :state))
        :manifest (if (plist-get value :manifest)
                      (e-board-orchestration-actions--canonical-manifest
                       (plist-get value :manifest))
                    e-json-null)
        :tasks (vconcat (mapcar #'e-board-orchestration-actions--canonical-task
                                (or (plist-get value :tasks) nil)))
        :accepted-reports
        (vconcat (mapcar #'e-board-orchestration-actions--canonical-report
                         (or (plist-get value :accepted-reports) nil)))
        :conflicts
        (vconcat (mapcar #'e-board-orchestration-actions--canonical-conflict
                         (or (plist-get value :conflicts) nil)))
        :deadline (if (plist-get value :deadline)
                      (e-board-orchestration-actions--canonical-deadline
                       (plist-get value :deadline))
                    e-json-null)
        :continuation (if (plist-get value :continuation)
                          (e-board-orchestration-actions--canonical-continuation
                           (plist-get value :continuation))
                        e-json-null)
        :continuation-outcome
        (if (plist-get value :continuation-outcome)
            (e-board-orchestration-actions--canonical-continuation-outcome
             (plist-get value :continuation-outcome))
          e-json-null)
        :terminal-status
        (e-board-orchestration-actions--canonical-string
         (plist-get value :terminal-status))
        :tasks-truncated (e-board-orchestration-actions--canonical-bool
                          (plist-get value :tasks-truncated))
        :accepted-reports-truncated
        (e-board-orchestration-actions--canonical-bool
         (plist-get value :accepted-reports-truncated))
        :conflicts-truncated
        (e-board-orchestration-actions--canonical-bool
         (plist-get value :conflicts-truncated))))

(defun e-board-orchestration-actions--canonical-counts (value)
  "Project one bounded state-count object VALUE into canonical JSON."
  (list :total (or (plist-get value :total) 0)
        :pending (or (plist-get value :pending) 0)
        :running (or (plist-get value :running) 0)
        :done (or (plist-get value :done) 0)
        :failed (or (plist-get value :failed) 0)
        :cancelled (or (plist-get value :cancelled) 0)
        :other (or (plist-get value :other) 0)))

(defun e-board-orchestration-actions--canonical-run-entry (value)
  "Project one consumer-shaped RUN entry into canonical JSON."
  (list :board-id (e-board-orchestration-actions--canonical-string
                   (plist-get value :board-id))
        :run-id (e-board-orchestration-actions--canonical-string
                 (plist-get value :run-id))
        :label (e-board-orchestration-actions--canonical-string
                (plist-get value :label))
        :lifecycle (e-board-orchestration-actions--canonical-string
                    (plist-get value :lifecycle))
        :active-p (e-board-orchestration-actions--canonical-bool
                   (plist-get value :active-p))
        :actionable-rank (or (plist-get value :actionable-rank) 0)
        :required-count (or (plist-get value :required-count) 0)
        :optional-count (or (plist-get value :optional-count) 0)
        :required-total (or (plist-get value :required-total) 0)
        :optional-total (or (plist-get value :optional-total) 0)
        :required-state-counts
        (e-board-orchestration-actions--canonical-counts
         (plist-get value :required-state-counts))
        :optional-state-counts
        (e-board-orchestration-actions--canonical-counts
         (plist-get value :optional-state-counts))
        :required-complete (or (plist-get value :required-complete) 0)
        :optional-active (e-board-orchestration-actions--canonical-bool
                          (plist-get value :optional-active))
        :participant-count (or (plist-get value :participant-count) 0)
        :participant-total (or (plist-get value :participant-total) 0)
        :admission-count (or (plist-get value :admission-count) 0)
        :admission-total (or (plist-get value :admission-total) 0)
        :latest-event-at
        (e-board-orchestration-actions--canonical-number
         (plist-get value :latest-event-at))
        :latest-event-position (or (plist-get value :latest-event-position) 0)
        :conflicts
        (vconcat (mapcar #'e-board-orchestration-actions--canonical-conflict
                         (or (plist-get value :conflicts) nil)))
        :deadline
        (if (plist-get value :deadline)
            (e-board-orchestration-actions--canonical-deadline
             (plist-get value :deadline))
          e-json-null)
        :failure (e-board-orchestration-actions--canonical-string
                  (plist-get value :failure))
        :restore-state (e-board-orchestration-actions--canonical-string
                        (plist-get value :restore-state))
        :attention-p (e-board-orchestration-actions--canonical-bool
                      (plist-get value :attention-p))
        :completion-state
        (e-board-orchestration-actions--canonical-string
         (plist-get value :completion-state))
        :completion-delivery-state
        (e-board-orchestration-actions--canonical-string
         (plist-get value :completion-delivery-state))
        :completion-execution-state
        (e-board-orchestration-actions--canonical-string
         (plist-get value :completion-execution-state))
        :continuation-outcome
        (if (plist-get value :continuation-outcome)
            (e-board-orchestration-actions--canonical-continuation-outcome
             (plist-get value :continuation-outcome))
          e-json-null)
        :continuation-state
        (e-board-orchestration-actions--canonical-string
         (plist-get value :continuation-state))))

(defun e-board-orchestration-actions--canonical-run-set (value)
  "Project a consumer-shaped RUN-SET into canonical JSON."
  (list :board-id (e-board-orchestration-actions--canonical-string
                   (plist-get value :board-id))
        :restore-state (e-board-orchestration-actions--canonical-string
                        (plist-get value :restore-state))
        :ready-p (e-board-orchestration-actions--canonical-bool
                  (plist-get value :ready-p))
        :status (e-board-orchestration-actions--canonical-string
                 (plist-get value :status))
        :runs (vconcat (mapcar #'e-board-orchestration-actions--canonical-run-entry
                               (or (plist-get value :runs) nil)))
        :active-count (or (plist-get value :active-count) 0)
        :active-run-count (or (plist-get value :active-run-count) 0)
        :omitted-count (or (plist-get value :omitted-count) 0)
        :more-p (e-board-orchestration-actions--canonical-bool
                 (plist-get value :more-p))
        :bytes (or (plist-get value :bytes) 0)))

(defun e-board-orchestration-actions--canonical-result (value)
  "Project one Board orchestration action VALUE into canonical JSON."
  (cond
   ((and (listp value) (plist-member value :runs))
    (e-board-orchestration-actions--canonical-run-set value))
   ((and (listp value) (plist-member value :manifest))
    (e-board-orchestration-actions--canonical-projection value))
   ((and (listp value) (plist-member value :run-id))
    (e-board-orchestration-actions--canonical-projection value))
   ((listp value)
    (vconcat (mapcar #'e-board-orchestration-actions--canonical-projection value)))
   ((e-json-value-p value) value)
   (t (signal 'e-json-error
              (list "Board orchestration returned a noncanonical result")))))

(defconst e-board-orchestration-actions--run-id-parameters
  '(:type "object"
    :properties
    (:run-id (:type "string" :description "Durable board run id from its manifest."))
    :required ["run-id"]
    :additionalProperties :json-false)
  "Action parameters for one durable run lookup.")

(defun e-board-orchestration-actions--action (handler parameters description)
  "Return one async-capable durable run observation action for HANDLER."
  (e-action-create
   :description description
   :parameters parameters
   :work
   (e-work-spec-create
    :id "board-orchestration-action" :execution 'cooperative
    :interactive-policy 'async :owner 'board
    :runner
    (lambda (parent arguments context)
      (let ((result (funcall handler context arguments)))
        (if (not (e-work-handle-p result))
            (e-board-orchestration-actions--canonical-result result)
          (setf (e-work-handle-cancel-function parent)
                (lambda (_handle) (e-work-cancel result)))
          (e-work-on-settle
           result
           (lambda (settled)
             (pcase (plist-get (e-work-status settled) :state)
               ('finished
                (e-work-finish
                 parent
                 (e-board-orchestration-actions--canonical-result
                  (e-work-handle-result settled))))
               ('failed (e-work-fail parent (e-work-handle-error settled)))
               ('cancelled (e-work-cancel parent)))))
          :deferred))))))

(defun e-board-orchestration-actions-parent-alist (&optional run-set-p)
  "Return Board actions that expose durable run observations.
When RUN-SET-P is non-nil, `:list-runs' returns the Feature 89 consumer-shaped
run-set value.  The default preserves the lower-level bounded list helper for
callers that consume its historical list shape directly."
  (list :list-runs
        (e-board-orchestration-actions--action
         (if run-set-p
             #'e-board-orchestration-actions--run-set-list
           #'e-board-orchestration-actions--list)
         nil
         "List the Board's bounded active run-set.")
        :run-status
        (e-board-orchestration-actions--action
         #'e-board-orchestration-actions--status
         e-board-orchestration-actions--run-id-parameters
         "Read one bounded durable Board run projection.")))

(provide 'e-board-orchestration-actions)

;;; e-board-orchestration-actions.el ends here

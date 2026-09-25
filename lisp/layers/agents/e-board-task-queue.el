;;; e-board-task-queue.el --- Reconcile Board assignments with durable tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Bridges durable Board assignment identities to the generic SQLite task
;; queue.  Board policy stays in orchestration and execution stays in the
;; configured queue runner.

;;; Code:

(require 'e-board-sqlite-service)
(require 'e-subagent-runner)
(require 'e-task-queue)
(require 'e-task-storage)
(require 'subr-x)
(require 'e-work)

(define-error 'e-board-task-queue-error "Board task queue error")

(defconst e-board-task-queue-snapshot-limit 4096
  "Maximum durable task records inspected by one reconciliation request.")

(defconst e-board-task-queue--terminal-statuses
  '(done failed cancelled interrupted unrouted)
  "Terminal task queue states returned as a Board assignment disposition.")

(defconst e-board-task-queue--active-statuses '(queued running paused pausing)
  "Nonterminal task queue states that already own a Board assignment.")

(defun e-board-task-queue-task-id (target run-id task-key attempt)
  "Return a bounded deterministic task id for TARGET's assignment.
The exact Board, RUN-ID, TASK-KEY, and ATTEMPT coordinates are hashed as one
printed tuple so delimiters in any coordinate cannot alias another tuple."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (unless (and (stringp run-id) (not (string-empty-p run-id))
               (stringp task-key) (not (string-empty-p task-key))
               (integerp attempt) (>= attempt 0))
    (signal 'e-board-task-queue-error
            (list "Invalid Board task assignment" run-id task-key attempt)))
  (format "btask_%s"
          (secure-hash
           'sha256
           (concat "e-board-task-queue-v1\n"
                   (prin1-to-string
                    (list (e-board-sqlite-publication-target-board-id target)
                          run-id task-key attempt))))))

(defun e-board-task-queue--valid-metadata-p (metadata)
  "Return non-nil when METADATA is a proper plist with unique keyword keys."
  (let ((tail metadata)
        (seen (make-hash-table :test 'eq))
        valid)
    (setq valid (or (null metadata) (proper-list-p metadata)))
    (while (and valid tail)
      (let ((key (pop tail)))
        (if (or (not (keywordp key))
                (null tail)
                (gethash key seen))
            (setq valid nil)
          (puthash key t seen)
          (pop tail))))
    (and valid (null tail))))

(defun e-board-task-queue--metadata (metadata board-id assignment)
  "Return detached METADATA with canonical BOARD-ID and ASSIGNMENT fields."
  (unless (e-board-task-queue--valid-metadata-p metadata)
    (signal 'e-board-task-queue-error
            (list "Board task metadata must be a plist with unique keyword keys")))
  (let ((copy (copy-tree metadata t)))
    (setq copy (plist-put copy :board-id board-id))
    (setq copy (plist-put copy :board-run-id (plist-get assignment :run-id)))
    (setq copy (plist-put copy :board-task-key (plist-get assignment :task-key)))
    (plist-put copy :board-attempt (plist-get assignment :attempt))))

(defun e-board-task-queue--active-p (work)
  "Return non-nil while WORK can still settle."
  (not (memq (plist-get (e-work-status work) :state)
             '(finished failed cancelled))))

(defun e-board-task-queue--fail (work condition)
  "Fail WORK with CONDITION unless it has already settled."
  (when (e-board-task-queue--active-p work)
    (e-work-fail work condition)))

(defun e-board-task-queue--snapshot-complete-p (snapshot)
  "Return non-nil when SNAPSHOT contains an untruncated task record page."
  (let ((records (plist-get snapshot :records)))
    (and (listp records)
         (not (plist-get snapshot :truncated))
         (< (length records) e-board-task-queue-snapshot-limit))))

(defun e-board-task-queue--classify (target assignment expected-task-id result)
  "Classify assignment from canonical stable-enqueue RESULT."
  (let* ((record (plist-get result :record))
         (task-id (plist-get result :task-id))
         (status (plist-get record :status))
         (created-p (plist-get result :created-p))
         (board-id (e-board-sqlite-publication-target-board-id target)))
    (unless (and (equal expected-task-id task-id)
                 (listp record)
                 (equal task-id (plist-get record :task-id)))
      (signal 'e-board-task-queue-error
              (list "Task enqueue returned no canonical assignment record" result)))
    (list
     :status
     (cond
      (created-p 'created)
      ((memq status e-board-task-queue--terminal-statuses) 'terminal)
      ((eq status 'running)
       (if (e-subagent-runner-assignment-state
            board-id
            (plist-get assignment :run-id)
            (plist-get assignment :task-key)
            (plist-get assignment :attempt))
           'existing
         'orphan))
      ((memq status e-board-task-queue--active-statuses) 'existing)
      (t
       (signal 'e-board-task-queue-error
               (list "Unknown task queue state for Board assignment" status))))
     :task-id task-id
     :assignment (copy-tree assignment t)
     :record (copy-tree record t))))

(defun e-board-task-queue--enqueue
    (work queue target assignment task-id prompt summary metadata
          harness-instance-id)
  "Submit stable TASK-ID after WORK's complete bounded snapshot."
  (when (e-board-task-queue--active-p work)
    (condition-case error
        (e-task-queue-enqueue
         queue :task-id task-id :prompt prompt :summary summary
         :metadata metadata :harness-instance-id harness-instance-id
         :on-settle
         (lambda (result settle-error)
           (when (e-board-task-queue--active-p work)
             (if settle-error
                 (e-board-task-queue--fail work settle-error)
               (condition-case error
                 (e-work-finish
                    work (e-board-task-queue--classify
                          target assignment task-id result))
                 ((error quit) (e-board-task-queue--fail work error)))))))
      ((error quit) (e-board-task-queue--fail work error)))))

(defun e-board-task-queue--snapshot
    (work queue target assignment task-id prompt summary metadata
          harness-instance-id)
  "Read QUEUE's bounded task snapshot before classifying ASSIGNMENT."
  (when (e-board-task-queue--active-p work)
    (condition-case error
        (e-task-storage-submit
         (e-task-queue-storage queue) 'read 'snapshot
         (list (e-task-queue-id queue)
               e-board-task-queue-snapshot-limit)
         (lambda (snapshot settle-error)
           (when (e-board-task-queue--active-p work)
             (if settle-error
                 (e-board-task-queue--fail work settle-error)
               (condition-case error
                   (progn
                     (unless (and (equal (plist-get snapshot :queue-id)
                                         (e-task-queue-id queue))
                                  (e-board-task-queue--snapshot-complete-p snapshot))
                       (signal 'e-board-task-queue-error
                               (list "Task queue snapshot is incomplete"
                                     (e-task-queue-id queue)
                                     e-board-task-queue-snapshot-limit)))
                     (e-board-task-queue--enqueue
                      work queue target assignment task-id prompt summary
                      metadata harness-instance-id))
                 ((error quit) (e-board-task-queue--fail work error)))))))
      ((error quit) (e-board-task-queue--fail work error)))))

(defconst e-board-task-queue--reconcile-work-spec
  (e-work-spec-create
   :id "board-task-queue-reconcile"
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'board
   :runner
   (lambda (work arguments _context)
     (let ((queue (plist-get arguments :queue))
           (target (plist-get arguments :target))
           (assignment (plist-get arguments :assignment))
           (task-id (plist-get arguments :task-id))
           (prompt (plist-get arguments :prompt))
           (summary (plist-get arguments :summary))
           (metadata (plist-get arguments :metadata))
           (harness-instance-id
            (plist-get arguments :harness-instance-id)))
       (condition-case error
           (e-task-storage-submit
            (e-task-queue-storage queue) 'write 'open-queue
            (list (e-task-queue-id queue))
            (lambda (_opened settle-error)
              (when (e-board-task-queue--active-p work)
                (if settle-error
                    (e-board-task-queue--fail work settle-error)
                  (e-board-task-queue--snapshot
                   work queue target assignment task-id prompt summary
                   metadata harness-instance-id)))))
         ((error quit) (e-board-task-queue--fail work error)))
       :deferred)))
  "Cooperative work that opens, snapshots, and reconciles one Board task.")

(cl-defun e-board-task-queue-reconcile
    (queue target run-id task-key attempt
           &key prompt summary metadata harness-instance-id)
  "Reconcile one Board assignment with QUEUE and return request-scoped WORK.
WORK finishes with `:status' `created', `existing', `terminal', or `orphan'
and includes the stable task id and canonical task record.  A persisted
running task without an exact process-local subagent assignment is reported
as an orphan and is never retried here.

The queue must use durable storage and have automatic retries disabled; any
retry must advance the Board attempt before it is enqueued.  Snapshot or
enqueue errors fail WORK."
  (unless (and (e-task-queue-p queue)
               (e-task-queue-storage-backed-p queue))
    (signal 'e-board-task-queue-error
            (list "Board task reconciliation requires a durable task queue")))
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (unless (zerop (or (e-task-queue-max-retries queue)
                     e-task-queue-max-retries))
    (signal 'e-board-task-queue-error
            (list "Board task queues must disable automatic retries")))
  (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
    (signal 'wrong-type-argument (list 'stringp :prompt)))
  (let* ((board-id (e-board-sqlite-publication-target-board-id target))
         (assignment (list :run-id run-id :task-key task-key :attempt attempt))
         (task-id (e-board-task-queue-task-id
                   target run-id task-key attempt))
         (metadata (e-board-task-queue--metadata
                    metadata board-id assignment)))
    (e-work-start
     e-board-task-queue--reconcile-work-spec
     (list :queue queue :target target :assignment assignment :task-id task-id
           :prompt (copy-sequence prompt) :summary (copy-tree summary t)
           :metadata metadata :harness-instance-id harness-instance-id)
     :context (list :board-id board-id :run-id run-id :task-key task-key
                    :attempt attempt))))

(provide 'e-board-task-queue)

;;; e-board-task-queue.el ends here

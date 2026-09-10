;;; e-task-queue.el --- Bounded-concurrency agent task queue for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A durable, bounded-concurrency queue of agent work items.  Each task is a
;; prompt plus metadata with an explicit status lifecycle.  The queue runs at most
;; `e-task-queue-max-parallel' tasks concurrently and dispatches FIFO by
;; enqueue time.
;;
;; A custom runner remains an application-owned extension seam.  The bundled
;; default runner requires an explicit SQLite publication target and publishes
;; one Board fact; it never creates or prompts a harness session.
;;
;; This module depends only on the core harness and the harness-instance
;; catalog, never on a UI shell, so the queue runs headless.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-board-sqlite-service)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-task-storage)
(require 'e-work)

(defgroup e-task-queue nil
  "Bounded-concurrency agent task queue."
  :group 'e
  :prefix "e-task-queue-")

(defcustom e-task-queue-max-parallel 2
  "Maximum number of tasks a queue runs concurrently."
  :type 'integer
  :group 'e-task-queue)

(defcustom e-task-queue-default-harness-instance-id nil
  "Harness instance id used for tasks that supply none.
When nil, the queue falls back to the default `chat' harness instance, the same
default a new chat uses."
  :type '(choice (const :tag "Default chat instance" nil)
                 (symbol :tag "Harness instance id"))
  :group 'e-task-queue)

(defcustom e-task-queue-max-records 128
  "Maximum retained records in one task queue."
  :type 'integer
  :group 'e-task-queue)

(defcustom e-task-queue-record-byte-limit (* 16 1024)
  "Maximum string bytes retained by one durable task record."
  :type 'integer
  :group 'e-task-queue)

(defcustom e-task-queue-record-node-limit 512
  "Maximum Lisp value nodes retained by one durable task record."
  :type 'integer
  :group 'e-task-queue)

(defcustom e-task-queue-max-retries 1
  "How many times a `failed' task is automatically retried.
A retry runs a fresh session that references the failed one and is prompted to
analyze the failure and continue the original task.  0 disables auto-retry.
A queue may override this via its `max-retries' slot."
  :type 'integer
  :group 'e-task-queue)

(defvar e-task-queue-retry-prompt-function #'e-task-queue-default-retry-prompt
  "Function building the prompt for an auto-retry of a failed task.
Called with the failed task's normalized record and must return the prompt
string the retry submits.  The record carries `:origin-prompt' (the true
original task), `:session-id' (the failed session to reference), and `:error'.")

(defvar e-task-queue-change-functions nil
  "Abnormal hook run after a queue's task set or a task record changes.
Each function is called with the queue.  Intended for observation (shells,
tests); handlers must not mutate the queue.")

(defvar e-task-queue-terminal-functions nil
  "Abnormal hook run after one task reaches a terminal queue state.
Each function receives QUEUE and the normalized terminal task record.  Queue
retry remains queue policy: this hook runs only after retries are exhausted.")

(defvar e-task-queue--unsettled-write-count 0)
(defvar e-task-queue--failed-write-count 0)
(defvar e-task-queue--unsettled-generation 0)
(defvar e-task-queue--unsettled-change-functions nil)

(defun e-task-queue-unsettled-state ()
  "Return constant-time task writer state for runtime quiescence."
  (list :generation e-task-queue--unsettled-generation
        :writes e-task-queue--unsettled-write-count
        :failures e-task-queue--failed-write-count))

(defun e-task-queue--adjust-writer-state (class delta)
  "Adjust task writer CLASS by DELTA and notify quiescence observers."
  (pcase class
    ('writes (cl-incf e-task-queue--unsettled-write-count delta))
    ('failures (cl-incf e-task-queue--failed-write-count delta)))
  (cl-incf e-task-queue--unsettled-generation)
  (run-hook-with-args 'e-task-queue--unsettled-change-functions
                      (e-task-queue-unsettled-state)))

(define-error 'e-task-queue-error "Task queue error")
(define-error 'e-task-queue-unknown-task "Unknown task id" 'e-task-queue-error)

(defun e-task-queue--value-within-budget-p (value node-limit byte-limit)
  "Return non-nil when VALUE fits NODE-LIMIT and BYTE-LIMIT."
  (let ((pending (list value)) (nodes 0) (bytes 0) valid)
    (setq valid t)
    (while (and pending valid)
      (let ((item (pop pending)))
        (setq nodes (1+ nodes))
        (when (> nodes node-limit) (setq valid nil))
        (cond
         ((stringp item)
          (setq bytes (+ bytes (string-bytes item)))
          (when (> bytes byte-limit) (setq valid nil)))
         ((consp item)
          (push (car item) pending)
          (push (cdr item) pending))
         ((vectorp item)
          (dotimes (index (length item))
            (push (aref item index) pending))))))
    valid))

(cl-defstruct (e-task-queue (:constructor e-task-queue--create))
  "A live task scheduler with a bounded dispatcher.
RECORDS maps task ids to mutable task plists.  ORDER lists task ids in enqueue
order (oldest first).  MAX-PARALLEL, DEFAULT-HARNESS-INSTANCE-ID, and RUNNER
override the module defaults when non-nil.  EXPOSE-AWAIT-REFERENCES-P marks the
one public queue whose task ids the global =task:= resolver can resolve.
DISPATCHING guards dispatch re-entrancy so a synchronous runner settle does
not recurse.  PAUSED-P is the queue-level gate that stops the dispatcher from
starting new work.  STORAGE is the only durable authority; LOADED-P records
that scheduler startup was requested without implying durable replay."
  (records (make-hash-table :test 'equal))
  (order nil)
  (sequence 0)
  max-parallel
  default-harness-instance-id
  runner
  publication-target
  dispatching
  paused-p
  max-retries
  expose-await-references-p
  ;; Retained as an always-nil compatibility accessor; file-backed
  ;; construction is rejected above and no runtime path reads this slot.
  directory
  id
  storage
  storage-root-opened-p
  claim-pending-p
  persistence-suspect
  (revision 0)
  loaded-p)

(cl-defun e-task-queue-create (&key max-parallel default-harness-instance-id
                                    runner publication-target max-retries directory
                                    expose-await-references-p storage
                                    (id "default"))
  "Return a new task queue.
MAX-PARALLEL, DEFAULT-HARNESS-INSTANCE-ID, MAX-RETRIES, and RUNNER override the
module defaults for this queue when non-nil.  Without RUNNER,
PUBLICATION-TARGET must name the explicit SQLite Board receiving queued task
facts.
EXPOSE-AWAIT-REFERENCES-P is reserved for a queue whose task ids are registered
with the global waitable resolver; private scheduler queues must leave it nil.
DIRECTORY is retired and signals with migration guidance.  STORAGE is the only
durable port; omitting both creates an explicitly in-memory queue."
  (when directory
    (signal 'e-task-queue-error
            (list "Task file persistence was retired; run offline migration")))
  ;; Storage-backed construction intentionally performs no domain request.
  ;; The durable queue root is opened at the first explicit queue operation
  ;; instead, so composing the default runtime cannot wait for worker open or
  ;; create a queue row as a constructor side effect.
  (e-task-queue--create
   :id id
   :storage storage
   :max-parallel max-parallel
   :default-harness-instance-id default-harness-instance-id
   :runner runner
   :publication-target publication-target
   :max-retries max-retries
   :expose-await-references-p expose-await-references-p))

(defun e-task-queue--ensure-storage-root (queue)
  "Synchronously open QUEUE's durable root for an operator operation.

This named blocking compatibility helper is used only by explicit history
deletion.  Ordinary scheduling uses atomic asynchronous claims and never calls
it."
  (when (and (e-task-queue-storage-backed-p queue)
             (not (e-task-queue-storage-root-opened-p queue)))
    (let ((root (e-task-storage-open-queue
                 (e-task-queue-storage queue) (e-task-queue-id queue))))
      (setf (e-task-queue-storage-root-opened-p queue) t
            (e-task-queue-revision queue) (or (plist-get root :revision) 0)
            (e-task-queue-sequence queue)
            (max (or (e-task-queue-sequence queue) 0)
                 (or (plist-get root :sequence) 0))
            (e-task-queue-paused-p queue) (and (plist-get root :paused-p) t))))
  queue)

(defun e-task-queue-storage-backed-p (queue)
  "Return non-nil when QUEUE uses its typed SQLite storage port."
  (and (e-task-storage-p (e-task-queue-storage queue)) t))

;; --- configuration accessors ------------------------------------------------

(defun e-task-queue--max-parallel (queue)
  "Return the effective parallelism cap for QUEUE."
  (or (e-task-queue-max-parallel queue) e-task-queue-max-parallel))

(defun e-task-queue--max-retries (queue)
  "Return the effective auto-retry cap for QUEUE."
  (or (e-task-queue-max-retries queue) e-task-queue-max-retries 0))

(defun e-task-queue--runner (queue)
  "Return the effective runner for QUEUE."
  (or (e-task-queue-runner queue) #'e-task-queue-default-runner))

(defun e-task-queue--default-instance-id (queue)
  "Return the harness instance id for tasks in QUEUE that supply none.
Falls back to the default `chat' instance when no explicit default is set."
  (or (e-task-queue-default-harness-instance-id queue)
      e-task-queue-default-harness-instance-id
      (when-let ((instance (e-harness-instance-default :kind 'chat)))
        (e-harness-instance-id instance))))

;; --- records ----------------------------------------------------------------

(defun e-task-queue--next-id (queue)
  "Return the next stable task id for QUEUE."
  (setf (e-task-queue-sequence queue) (1+ (e-task-queue-sequence queue)))
  (format "tsk_%06d" (e-task-queue-sequence queue)))

(defun e-task-queue--timestamp ()
  "Return an ISO-8601 timestamp for task lifecycle stamps."
  (format-time-string "%FT%T%z"))

(defun e-task-queue--prompt-summary (prompt)
  "Return a compact one-line summary for PROMPT."
  (when (stringp prompt)
    (truncate-string-to-width
     (string-trim (replace-regexp-in-string "[\n\t ]+" " " prompt))
     80 nil nil t)))

(defun e-task-queue-record-display-summary (record)
  "Return the best short display label for RECORD.
Prefers an explicit agent-authored `:summary' stub, falling back to the
truncated prompt prefix in `:prompt-summary'."
  (or (plist-get record :summary)
      (plist-get record :prompt-summary)
      ""))

(defun e-task-queue--record (queue task-id)
  "Return the mutable internal record for TASK-ID in QUEUE, or signal."
  (or (gethash task-id (e-task-queue-records queue))
      (signal 'e-task-queue-unknown-task (list task-id))))

(defun e-task-queue--normalize (queue record)
  "Return a model-facing copy of QUEUE's RECORD without runtime-only fields."
  (let ((task-id (plist-get record :task-id)))
    (append
     (list :task-id task-id)
     (when (e-task-queue-expose-await-references-p queue)
       (list :await-ref (format "task:%s" task-id)))
     (list :status (plist-get record :status)
           :prompt (plist-get record :prompt)
           :origin-prompt (plist-get record :origin-prompt)
           :summary (plist-get record :summary)
           :prompt-summary (plist-get record :prompt-summary)
           :metadata (plist-get record :metadata)
           :harness-instance-id (plist-get record :harness-instance-id)
           :enqueued-at (plist-get record :enqueued-at)
           :started-at (plist-get record :started-at)
           :finished-at (plist-get record :finished-at)
           :session-id (plist-get record :session-id)
           :retries (or (plist-get record :retries) 0)
           :attempt-id (plist-get record :attempt-id)
           :attempt-number (or (plist-get record :attempt-number) 0)
           :outputs (plist-get record :outputs)
           :error (plist-get record :error)))))

(defun e-task-queue--notify (queue)
  "Run change hooks for QUEUE."
  (run-hook-with-args 'e-task-queue-change-functions queue))

(defconst e-task-queue--durable-fields
  '(:task-id :status :prompt :origin-prompt :summary :prompt-summary :metadata
    :harness-instance-id :enqueued-at :started-at :finished-at :session-id
    :retries :outputs :error :attempt-id :attempt-number :revision)
  "Task fields owned by the durable record projection.")

(defun e-task-queue--durable-record (record)
  "Return a detached durable projection of RECORD."
  (let (durable)
    (dolist (key e-task-queue--durable-fields)
      (setq durable (plist-put durable key (copy-tree (plist-get record key)))))
    durable))

(defun e-task-queue--publish-durable-record (target durable)
  "Publish committed DURABLE fields into live TARGET."
  (dolist (key e-task-queue--durable-fields)
    (setq target (plist-put target key (copy-tree (plist-get durable key)))))
  target)

(defun e-task-queue--commit-record
    (queue current expected-status staged)
  "Publish STAGED into CURRENT and enqueue its durable transition.
Persistent callers never wait for SQLite.  CURRENT is retained only while the
mutation is in flight or the task is executing."
  (if (not (e-task-queue-storage-backed-p queue))
      (e-task-queue--publish-durable-record current staged)
    (let* ((task-id (plist-get current :task-id))
           (durable (e-task-queue--durable-record staged))
           (terminal-p (memq (plist-get durable :status)
                             '(done failed cancelled interrupted unrouted))))
      (e-task-queue--publish-durable-record current durable)
      (plist-put current :persistence-pending t)
      (e-task-storage-submit
       (e-task-queue-storage queue) 'write 'transition
       (list (e-task-queue-id queue) task-id expected-status durable)
       (lambda (result error)
         (plist-put current :persistence-pending nil)
         (if error
             (progn
               (plist-put current :persistence-suspect t)
               (unless (e-task-queue-persistence-suspect queue)
                 (setf (e-task-queue-persistence-suspect queue)
                       (copy-tree error t))))
           (setf (e-task-queue-revision queue)
                 (or (plist-get result :revision)
                     (e-task-queue-revision queue)))
           (when terminal-p
             (remhash task-id (e-task-queue-records queue))
             (setf (e-task-queue-order queue)
                   (delete task-id (e-task-queue-order queue)))))
         (e-task-queue--notify queue)))
      current)))

;; --- public reads -----------------------------------------------------------

(defun e-task-queue-get (queue task-id)
  "Return the normalized record for TASK-ID in QUEUE."
  (e-task-queue--normalize queue (e-task-queue--record queue task-id)))

(defun e-task-queue-list (queue)
  "Return normalized task records in QUEUE, newest-first."
  (mapcar (lambda (task-id)
            (e-task-queue--normalize queue (gethash task-id (e-task-queue-records queue))))
          (reverse (e-task-queue-order queue))))

(defun e-task-queue-outputs (queue task-id)
  "Return the outputs collected for TASK-ID in QUEUE."
  (plist-get (e-task-queue--record queue task-id) :outputs))

(defun e-task-queue-work-handle (queue task-id)
  "Return TASK-ID's live `e-work' handle, or nil when it is unknown."
  (when-let ((record (gethash task-id (e-task-queue-records queue))))
    (plist-get record :work-handle)))

(defun e-task-queue--work-spec ()
  "Return the cooperative work spec tracking one queued task."
  (e-work-spec-create
   :id "task-queue-task"
   :description "Track a queued agent task."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'task-queue
   :runner (lambda (_handle _arguments _context) :deferred)))

(defun e-task-queue--work-handle-for-status (record)
  "Return a work handle mirroring RECORD's current status."
  (let ((handle (e-work-start (e-task-queue--work-spec) nil)))
    (pcase (plist-get record :status)
      ('done
       (e-work-finish handle
                      (list :summary (e-task-queue-record-display-summary record)
                            :outputs (plist-get record :outputs))))
      ('failed
       (e-work-fail handle
                    (list 'e-task-queue-error
                          (or (plist-get record :error) "Task failed"))))
      ('unrouted
       (e-work-fail handle
                    (list 'e-task-queue-error
                          (or (plist-get record :error) "Task was unrouted"))))
      ('interrupted
       (e-work-fail handle
                    (list 'e-task-queue-error
                          (or (plist-get record :error)
                              "Task effect is uncertain after restart"))))
      ('cancelled (e-work-cancel handle)))
    handle))

(defun e-task-queue--settle-work-handle (record status)
  "Settle RECORD's work handle for terminal STATUS."
  (when-let ((handle (plist-get record :work-handle)))
    (pcase status
      ('done
       (e-work-finish handle
                      (list :summary (e-task-queue-record-display-summary record)
                            :outputs (plist-get record :outputs))))
      ('failed
       (e-work-fail handle
                    (list 'e-task-queue-error
                          (or (plist-get record :error) "Task failed"))))
      ('unrouted
       (e-work-fail handle
                    (list 'e-task-queue-error
                          (or (plist-get record :error) "Task was unrouted"))))
      ('interrupted
       (e-work-fail handle
                    (list 'e-task-queue-error
                          (or (plist-get record :error)
                              "Task effect is uncertain after restart"))))
      ('cancelled (e-work-cancel handle)))))

;; --- dispatch helpers -------------------------------------------------------

(defun e-task-queue--running-count (queue)
  "Return the number of active or cancellation-pending tasks in QUEUE."
  (let ((count 0))
    (maphash (lambda (_id record)
               (when (memq (plist-get record :status) '(running pausing))
                 (setq count (1+ count))))
             (e-task-queue-records queue))
    count))

(defun e-task-queue--oldest-queued (queue)
  "Return the oldest queued task id in QUEUE, or nil."
  (cl-find-if (lambda (task-id)
                (eq (plist-get (gethash task-id (e-task-queue-records queue))
                               :status)
                    'queued))
              (e-task-queue-order queue)))

(defun e-task-queue-default-retry-prompt (record)
  "Return the default auto-retry prompt for a failed task RECORD.
References the failed session and asks the agent to analyze the failure before
resuming the original task."
  (let ((origin (or (plist-get record :origin-prompt)
                    (plist-get record :prompt)))
        (session (plist-get record :session-id))
        (error (plist-get record :error)))
    (concat
     "A previous attempt at this task failed and is being auto-retried.\n\n"
     (when session
       (format "Failed session: %s (inspect it for its transcript and partial progress).\n"
               session))
     (when error
       (format "Reported failure: %s\n" error))
     "\nFirst analyze what went wrong in that attempt, then continue and complete "
     "the original task below.  Do not repeat the failing step blindly; adjust your "
     "approach based on the failure.\n\n"
     "Original task:\n" origin)))

(defun e-task-queue--build-retry-prompt (record)
  "Build the retry prompt for RECORD via `e-task-queue-retry-prompt-function'."
  (condition-case nil
      (funcall e-task-queue-retry-prompt-function record)
    (error (e-task-queue-default-retry-prompt record))))

(defun e-task-queue--maybe-retry (queue record)
  "Auto-retry a just-failed RECORD in QUEUE when retries remain.
Rewrites the failed RECORD in place into a fresh `queued' attempt: its prompt
becomes an analyze-the-failure-and-continue prompt that references the failed
session, the retry counter increments, and lifecycle stamps reset.  Returns
non-nil when a retry was armed.  The original prompt is preserved in
`:origin-prompt' so successive retries always reference the true task."
  (when (and (< (or (plist-get record :retries) 0)
                (e-task-queue--max-retries queue))
             ;; A retry needs a failed session to reference and analyze.
             (plist-get record :session-id))
    (let ((retry-prompt (e-task-queue--build-retry-prompt
                         (e-task-queue--normalize queue record))))
      (plist-put record :retries (1+ (or (plist-get record :retries) 0)))
      (plist-put record :prompt retry-prompt)
      (plist-put record :status 'queued)
      (plist-put record :started-at nil)
      (plist-put record :finished-at nil)
      (plist-put record :handle nil)
      ;; A bridged retry is a fresh durable attempt.  The group can still
      ;; select a different accepted attempt without changing queue policy.
      (when-let ((run-id (plist-get (plist-get record :metadata) :board-run-id)))
        (ignore run-id)
        (let ((metadata (copy-tree (plist-get record :metadata))))
          (plist-put metadata :board-attempt
                     (1+ (or (plist-get metadata :board-attempt) 0)))
          (plist-put record :metadata metadata)))
      ;; Keep the failing error visible until the retry starts; the display
      ;; still shows the last failure reason while the task waits to re-run.
      t)))

(defun e-task-queue--settle (queue task-id status &rest args)
  "Settle an active TASK-ID in QUEUE.
ARGS may carry `:outputs' and `:error'.  No-op unless the task is still running
or durably pausing.  A confirmed `cancelled' settlement for a pause request
lands `paused'; another terminal result records that known outcome without
retry.  Re-dispatches QUEUE after a real transition."
  (let ((record (gethash task-id (e-task-queue-records queue))))
    (when (and record
               (memq (plist-get record :status) '(running pausing))
               ;; Each runner closure carries the immutable attempt it was
               ;; created for.  A late settle from a cancelled/paused attempt
               ;; must not settle a later retry or resume of the same task.
               (or (not (plist-member args :owner-attempt-id))
                   (equal (plist-get args :owner-attempt-id)
                          (plist-get record :attempt-id))))
      (unless (e-task-queue--value-within-budget-p
               args e-task-queue-record-node-limit e-task-queue-record-byte-limit)
        (setq status 'failed
              args (list :error "Task result exceeds retention budget")))
      (let ((staged (copy-tree record))
            (expected-status (plist-get record :status))
            terminal-status)
        (when (plist-member args :outputs)
          (plist-put staged :outputs (plist-get args :outputs)))
        (when (plist-member args :error)
          (plist-put staged :error (plist-get args :error)))
        (cond
         ((and (eq expected-status 'pausing) (eq status 'cancelled))
          (plist-put staged :status 'paused)
          (plist-put staged :started-at nil)
          (plist-put staged :finished-at nil))
         ((eq expected-status 'pausing)
          ;; The owned runner produced a known non-cancellation terminal
          ;; result after the pause request.  Preserve that result and never
          ;; auto-retry it as though cancellation had been confirmed.
          (plist-put staged :status status)
          (plist-put staged :finished-at (e-task-queue--timestamp))
          (setq terminal-status status))
         ((and (plist-get staged :pausing) (eq status 'cancelled))
          ;; Legacy queues retain their historical in-memory pause marker.
          (plist-put staged :pausing nil)
          (plist-put staged :status 'paused)
          (plist-put staged :started-at nil))
         ((and (eq status 'failed)
               (e-task-queue--maybe-retry queue staged)) nil)
         (t
          (plist-put staged :status status)
          (plist-put staged :finished-at (e-task-queue--timestamp))
          (setq terminal-status status)))
        (e-task-queue--commit-record queue record expected-status staged)
        (plist-put record :handle nil)
        (when terminal-status
          (e-task-queue--settle-work-handle record terminal-status)
          (run-hook-with-args 'e-task-queue-terminal-functions
                              queue (e-task-queue--normalize queue record))))
      (e-task-queue--notify queue)
      (e-task-queue--dispatch queue))))

(defun e-task-queue--interrupt-pausing (queue record message)
  "Commit an uncertain pause cancellation for RECORD with MESSAGE."
  (when (eq (plist-get record :status) 'pausing)
    (let ((staged (copy-tree record)))
      (plist-put staged :status 'interrupted)
      (plist-put staged :finished-at (e-task-queue--timestamp))
      (plist-put staged :error message)
      (e-task-queue--commit-record queue record 'pausing staged)
      (plist-put record :handle nil)
      (e-task-queue--settle-work-handle record 'interrupted)
      (run-hook-with-args 'e-task-queue-terminal-functions
                          queue (e-task-queue--normalize queue record))
      (e-task-queue--notify queue)
      (e-task-queue--dispatch queue)))
  record)

(defun e-task-queue--invoke-claimed-runner
    (queue task-id record instance-id)
  "Invoke TASK-ID's runner after RECORD's durable claim is live."
  (let ((harness
         (if (null (e-task-queue-runner queue))
             :board-producer
           (condition-case err
               (if instance-id
                   (e-harness-instance-get-or-create instance-id)
                 (signal 'e-task-queue-unknown-task
                         (list "No harness instance for task")))
             (error
              (e-task-queue--settle
               queue task-id 'failed
               :error (format "Cannot resolve harness instance %s: %s"
                              instance-id (e-work-error-message err)))
              nil)))))
    (when harness
      (let* ((owner-attempt-id (plist-get record :attempt-id))
             (handle
              (condition-case err
                  (funcall (e-task-queue--runner queue)
                           (e-task-queue--normalize queue record)
                           (if (e-task-queue-runner queue) harness queue)
                           (lambda (&rest settle-args)
                             (apply #'e-task-queue--settle queue task-id
                                    (or (plist-get settle-args :status) 'done)
                                    :owner-attempt-id owner-attempt-id
                                    settle-args)))
                (error
                 ;; The claim is authoritative and the runner may have begun
                 ;; an irreversible effect before signalling.  Without a
                 ;; returned handle there is no safe cancellation or retry
                 ;; boundary, so preserve uncertainty before surfacing the
                 ;; original synchronous error.
                 (when (eq (plist-get record :status) 'running)
                   (e-task-queue--settle
                    queue task-id 'interrupted
                    :owner-attempt-id owner-attempt-id
                    :error
                    (format "Task runner signalled after durable claim; external effect is uncertain: %s"
                            (error-message-string err))))
                 (signal (car err) (cdr err))))))
        (when-let* ((session-id (and (listp handle)
                                     (plist-get handle :session-id))))
          (plist-put record :session-id session-id))
        ;; A synchronous runner may already have settled the task and cleared
        ;; its handle; only a still-running task keeps a live handle.
        (when (eq (plist-get record :status) 'running)
          (plist-put record :handle handle))))))

(defun e-task-queue--start (queue task-id)
  "Start process-local TASK-ID and invoke the runner.
Resolves the task's harness instance at this moment.  A task whose instance id
is missing or unresolvable settles `failed' without stalling the dispatcher.
SQLite-backed queues use `e-task-queue--request-runnable-claim' instead."
  (let* ((record (gethash task-id (e-task-queue-records queue)))
         (instance-id (or (plist-get record :harness-instance-id)
                          (e-task-queue--default-instance-id queue))))
    (when (e-task-queue-storage-backed-p queue)
      (signal 'e-task-queue-error
              (list "SQLite task starts require an atomic asynchronous claim"
                    task-id)))
    (when instance-id
      (plist-put record :harness-instance-id instance-id))
    (plist-put record :status 'running)
    (plist-put record :started-at (e-task-queue--timestamp))
    (e-task-queue--notify queue)
    (e-task-queue--invoke-claimed-runner queue task-id record instance-id)))

(defun e-task-queue--claimed-record (queue durable)
  "Install DURABLE as one executing live record in QUEUE."
  (let* ((task-id (plist-get durable :task-id))
         (record (or (gethash task-id (e-task-queue-records queue))
                     (copy-tree durable t))))
    (e-task-queue--publish-durable-record record durable)
    (plist-put record :persistence-pending nil)
    (unless (plist-get record :work-handle)
      (plist-put record :work-handle
                 (e-task-queue--work-handle-for-status record)))
    (puthash task-id record (e-task-queue-records queue))
    (cl-pushnew task-id (e-task-queue-order queue) :test #'equal)
    record))

(defun e-task-queue--claim-settled (queue result error)
  "Continue QUEUE after one request-local runnable claim settles."
  (setf (e-task-queue-claim-pending-p queue) nil)
  (let ((claimed-p nil))
    (cond
   (error
    (unless (e-task-queue-persistence-suspect queue)
      (setf (e-task-queue-persistence-suspect queue) (copy-tree error t))))
   ((plist-get result :claimed-p)
    (setq claimed-p t)
    (setf (e-task-queue-revision queue)
          (or (plist-get result :revision) (e-task-queue-revision queue)))
    (let* ((record
            (e-task-queue--claimed-record queue (plist-get result :record)))
           (task-id (plist-get record :task-id))
           (instance-id (plist-get record :harness-instance-id)))
      (e-task-queue--notify queue)
      (e-task-queue--invoke-claimed-runner
       queue task-id record instance-id)))
   ((plist-get result :paused-p)
    (setf (e-task-queue-paused-p queue) t)))
    ;; A miss ends this scheduler tick.  A claim may have left additional
    ;; capacity, so continue only after actual progress and never spin on an
    ;; empty durable queue.
    (when (and claimed-p (not error))
      (e-task-queue--dispatch queue))))

(defun e-task-queue--request-runnable-claim (queue)
  "Enqueue one atomic SQLite runnable claim for QUEUE."
  (unless (e-task-queue-claim-pending-p queue)
    (setf (e-task-queue-claim-pending-p queue) t)
    (condition-case err
        (e-task-storage-submit
         (e-task-queue-storage queue) 'write 'claim-runnable
         (list (e-task-queue-id queue) (e-task-queue--timestamp)
               (e-task-queue--default-instance-id queue))
         (lambda (result error)
           (e-task-queue--claim-settled queue result error)))
      (error
       (setf (e-task-queue-claim-pending-p queue) nil)
       (unless (e-task-queue-persistence-suspect queue)
         (setf (e-task-queue-persistence-suspect queue) (copy-tree err t)))))))

(defun e-task-queue--dispatch (queue)
  "Start queued tasks in QUEUE up to the parallelism cap.
The queue-level pause gate blocks all new starts while set.  Guards against
re-entrancy so a synchronous runner settle does not recurse; the loop picks up
any task the settle frees."
  (unless (or (e-task-queue-dispatching queue)
              (e-task-queue-paused-p queue))
    (if (e-task-queue-storage-backed-p queue)
        (when (< (e-task-queue--running-count queue)
                 (e-task-queue--max-parallel queue))
          (e-task-queue--request-runnable-claim queue))
      (setf (e-task-queue-dispatching queue) t)
      (unwind-protect
          (let (next)
            (while (and (< (e-task-queue--running-count queue)
                           (e-task-queue--max-parallel queue))
                        (setq next (e-task-queue--oldest-queued queue)))
              (e-task-queue--start queue next)))
        (setf (e-task-queue-dispatching queue) nil)))))

;; --- public mutations -------------------------------------------------------

(cl-defun e-task-queue-enqueue (queue &key prompt summary metadata harness-instance-id)
  "Enqueue PROMPT on QUEUE and return its normalized record.
SUMMARY is an optional short, human-worded stub describing the task, the way a
new topic is auto-titled; when nil, a truncated prefix of PROMPT is used for
display.  METADATA is an opaque plist the enqueuer owns.  HARNESS-INSTANCE-ID
selects the configured harness instance the task runs on; nil uses the queue
default, resolved at dispatch time.  Dispatch runs before returning, so a task
may already be running when this returns."
  (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
    (signal 'wrong-type-argument (list 'stringp :prompt)))
  (unless (e-task-queue--value-within-budget-p
           (list prompt summary metadata)
           e-task-queue-record-node-limit e-task-queue-record-byte-limit)
    (signal 'e-task-queue-error (list "Task record exceeds retention budget")))
  (when (>= (hash-table-count (e-task-queue-records queue))
            e-task-queue-max-records)
    (signal 'e-task-queue-error (list "Task queue record limit reached")))
  (when (and (null (e-task-queue-runner queue))
             (not (e-board-sqlite-publication-target-valid-p
                   (e-task-queue-publication-target queue))))
    (signal 'e-task-queue-error
            (list "Default runner requires a SQLite publication target")))
  (let* ((task-id (if (e-task-queue-storage-backed-p queue)
                      (make-temp-name "tsk_")
                    (e-task-queue--next-id queue)))
         (record (list :task-id task-id
                       :status 'queued
                       :prompt prompt
                       :origin-prompt prompt
                       :summary (and (stringp summary)
                                     (not (string-empty-p (string-trim summary)))
                                     (string-trim summary))
                       :prompt-summary (e-task-queue--prompt-summary prompt)
                       :metadata metadata
                       :harness-instance-id harness-instance-id
                       :enqueued-at (e-task-queue--timestamp)
                       :started-at nil
                       :finished-at nil
                       :session-id nil
                       :retries 0
                       :outputs nil
                       :error nil
                       :attempt-id nil
                       :attempt-number 0
                       :revision 0
                       :handle nil
                       :work-handle nil)))
    (if (e-task-queue-storage-backed-p queue)
        ;; A durable queued task is SQLite state, not executing Emacs work.
        ;; Keep this optimistic record only until enqueue settlement; the
        ;; atomic claim path creates the first live work handle.
        (plist-put record :persistence-pending t)
      (plist-put record :work-handle
                 (e-task-queue--work-handle-for-status record)))
    (puthash task-id record (e-task-queue-records queue))
    (setf (e-task-queue-order queue)
          (append (e-task-queue-order queue) (list task-id)))
    (e-task-queue--notify queue)
    (let ((public (e-task-queue--normalize queue record)))
      (if (not (e-task-queue-storage-backed-p queue))
          (e-task-queue--dispatch queue)
        (condition-case err
            (e-task-storage-submit
             (e-task-queue-storage queue) 'write 'enqueue
             (list (e-task-queue-id queue)
                   (e-task-queue--durable-record record))
             (lambda (result error)
               (plist-put record :persistence-pending nil)
               (if error
                   (unless (e-task-queue-persistence-suspect queue)
                     (setf (e-task-queue-persistence-suspect queue)
                           (copy-tree error t)))
                 (setf (e-task-queue-revision queue)
                       (or (plist-get result :revision)
                           (e-task-queue-revision queue))))
               ;; Queued durability belongs only to SQLite.  The scheduler
               ;; reintroduces this task into Emacs iff an atomic claim makes
               ;; it executing work.
               (remhash task-id (e-task-queue-records queue))
               (setf (e-task-queue-order queue)
                     (delete task-id (e-task-queue-order queue)))
               (unless error (e-task-queue--dispatch queue))
               (e-task-queue--notify queue)))
          (error
           (plist-put record :persistence-pending nil)
           (setf (e-task-queue-persistence-suspect queue) (copy-tree err t))
           (remhash task-id (e-task-queue-records queue))
           (setf (e-task-queue-order queue)
                 (delete task-id (e-task-queue-order queue))))))
      (if (e-task-queue-storage-backed-p queue)
          public
        (e-task-queue--normalize
         queue (e-task-queue--record queue task-id))))))

(defun e-task-queue-cancel (queue task-id)
  "Cancel TASK-ID in QUEUE and return its normalized record.
A queued or paused task becomes `cancelled' without ever running.  A running
task is interrupted through its runner handle and its result is dropped on
settle.  Terminal tasks are returned unchanged."
  (let ((record (e-task-queue--record queue task-id)))
    (if (and (e-task-queue-storage-backed-p queue)
             (plist-get record :claiming))
      (plist-put record :claim-cancel-requested t)
      (pcase (plist-get record :status)
        ((or 'queued 'paused)
         (let ((staged (copy-tree record))
               (expected (plist-get record :status)))
           (plist-put staged :status 'cancelled)
           (plist-put staged :pausing nil)
           (plist-put staged :finished-at (e-task-queue--timestamp))
           (e-task-queue--commit-record queue record expected staged))
         (e-task-queue--settle-work-handle record 'cancelled)
         (e-task-queue--notify queue))
        ('running
         (let ((handle (plist-get record :handle)))
           (let ((staged (copy-tree record)))
             (plist-put staged :status 'cancelled)
             (plist-put staged :pausing nil)
             (plist-put staged :finished-at (e-task-queue--timestamp))
             (e-task-queue--commit-record queue record 'running staged))
           (plist-put record :handle nil)
           (when (and (listp handle) (functionp (plist-get handle :cancel)))
             (ignore-errors (funcall (plist-get handle :cancel))))
           (e-task-queue--settle-work-handle record 'cancelled)
           (e-task-queue--notify queue)
           (e-task-queue--dispatch queue)))))
    (e-task-queue--normalize queue record)))

(defun e-task-queue-pause (queue task-id)
  "Pause TASK-ID in QUEUE and return its normalized record.
A queued task moves to `paused' in place.  A running task is asked to stop at
its next turn boundary: the runner handle aborts the active turn and the
`cancelled' settle that follows is interpreted as a pause, landing the record
in `paused' with any partial outputs preserved.  A paused or terminal task is
returned unchanged."
  (let ((record (e-task-queue--record queue task-id)))
    (if (and (e-task-queue-storage-backed-p queue)
             (plist-get record :claiming))
        (plist-put record :claim-pause-requested t)
      (pcase (plist-get record :status)
        ('queued
         (let ((staged (copy-tree record)))
           (plist-put staged :status 'paused)
           (e-task-queue--commit-record queue record 'queued staged))
         (e-task-queue--notify queue))
        ('running
         (let ((handle (plist-get record :handle)))
           (if (e-task-queue-storage-backed-p queue)
               (progn
                 (let ((staged (copy-tree record)))
                   (plist-put staged :status 'pausing)
                   (e-task-queue--commit-record queue record 'running staged))
                 (unless (and (listp handle)
                              (functionp (plist-get handle :cancel)))
                   (e-task-queue--interrupt-pausing
                    queue record
                    "Task pause could not cancel the claimed runner; external effect is uncertain")
                   (signal 'e-task-queue-error
                           (list "Running task has no cancellation capability"
                                 task-id)))
                 (condition-case err
                     (funcall (plist-get handle :cancel))
                   (error
                    (e-task-queue--interrupt-pausing
                     queue record
                     (format "Task pause cancellation failed; external effect is uncertain: %s"
                             (error-message-string err)))
                    (signal (car err) (cdr err))))
                 ;; An asynchronous cancellation remains durably `pausing' and
                 ;; retains its active slot until its owned settle callback.
                 (e-task-queue--notify queue))
             ;; Preserve the legacy in-memory cancellation ordering.
             (plist-put record :pausing t)
             (if (and (listp handle) (functionp (plist-get handle :cancel)))
                 (ignore-errors (funcall (plist-get handle :cancel)))
               (plist-put record :pausing nil)
               (plist-put record :status 'paused)
               (plist-put record :handle nil)
               (plist-put record :started-at nil)
               (e-task-queue--notify queue)
               (e-task-queue--dispatch queue)))))))
    (e-task-queue--normalize queue record)))

(defun e-task-queue-resume (queue task-id)
  "Resume a paused TASK-ID in QUEUE and return its normalized record.
The task returns to `queued', where the dispatcher re-runs it from its prompt
under the normal cap.  A non-paused task is returned unchanged."
  (let ((record (e-task-queue--record queue task-id)))
    (when (eq (plist-get record :status) 'paused)
      (let ((staged (copy-tree record)))
        (plist-put staged :status 'queued)
        (plist-put staged :started-at nil)
        (plist-put staged :finished-at nil)
        (e-task-queue--commit-record queue record 'paused staged))
      (e-task-queue--notify queue)
      (e-task-queue--dispatch queue))
    (e-task-queue--normalize queue record)))

(defun e-task-queue-pause-all (queue)
  "Set QUEUE's pause gate and pause every non-terminal task.
While the gate is set the dispatcher starts no new work.  Returns QUEUE."
  (when (e-task-queue-storage-backed-p queue)
    (e-task-storage-submit
     (e-task-queue-storage queue) 'write 'set-paused
     (list (e-task-queue-id queue) t)
     (lambda (result error)
       (if error
           (unless (e-task-queue-persistence-suspect queue)
             (setf (e-task-queue-persistence-suspect queue)
                   (copy-tree error t)))
         (setf (e-task-queue-revision queue)
               (or (plist-get result :revision)
                   (e-task-queue-revision queue)))))))
  (setf (e-task-queue-paused-p queue) t)
  (dolist (task-id (copy-sequence (e-task-queue-order queue)))
    (let ((record (gethash task-id (e-task-queue-records queue))))
      (when (memq (plist-get record :status) '(queued running))
        (e-task-queue-pause queue task-id))))
  (e-task-queue--notify queue)
  queue)

(defun e-task-queue-resume-all (queue)
  "Clear QUEUE's pause gate, resume every paused task, and re-dispatch.
Returns QUEUE."
  (when (e-task-queue-storage-backed-p queue)
    (e-task-storage-submit
     (e-task-queue-storage queue) 'write 'set-paused
     (list (e-task-queue-id queue) nil)
     (lambda (result error)
       (if error
           (unless (e-task-queue-persistence-suspect queue)
             (setf (e-task-queue-persistence-suspect queue)
                   (copy-tree error t)))
         (setf (e-task-queue-revision queue)
               (or (plist-get result :revision)
                   (e-task-queue-revision queue)))))))
  (setf (e-task-queue-paused-p queue) nil)
  (dolist (task-id (copy-sequence (e-task-queue-order queue)))
    (let ((record (gethash task-id (e-task-queue-records queue))))
      (when (eq (plist-get record :status) 'paused)
        (if (e-task-queue-storage-backed-p queue)
            (let ((staged (copy-tree record)))
              (plist-put staged :status 'queued)
              (plist-put staged :started-at nil)
              (plist-put staged :finished-at nil)
              (e-task-queue--commit-record queue record 'paused staged))
          (plist-put record :status 'queued)
          (plist-put record :started-at nil)
          (plist-put record :finished-at nil)))))
  (e-task-queue--notify queue)
  (e-task-queue--dispatch queue)
  queue)

;; --- default runner ---------------------------------------------------------

(defun e-task-queue-default-runner (task harness on-settle)
  "Publish TASK as Board work through QUEUE's explicit SQLite target.
HARNESS is the queue sentinel carrying QUEUE for the default path.  Custom test
and application runners retain the historical runner signature."
  (unless (e-task-queue-p harness)
    (signal 'e-task-queue-error (list "Default runner requires its queue")))
  (let ((target (e-task-queue-publication-target harness)))
    (unless (e-board-sqlite-publication-target-valid-p target)
      (signal 'e-task-queue-error
              (list "Default runner requires a SQLite publication target")))
    (let ((publication
           (e-board-sqlite-publication-target-append-route-start
            target
            (plist-get task :prompt)
            (list 'task-queue (e-task-queue-id harness)
                  (plist-get task :task-id)
                  (or (plist-get task :attempt-id)
                      (plist-get task :retries) 0))
            :tags '(task-queue task)
            :attributes
            (list :task-id (plist-get task :task-id)
                  :task-attempt-id (plist-get task :attempt-id)
                  :summary (plist-get task :summary)
                  :metadata (copy-tree (plist-get task :metadata)))
            :reference (format "task:%s" (plist-get task :task-id))))
          observations
          delivery-outcomes
          pending-delivery-ids
          canonical-publication
          settled-p)
      (cl-labels
          ((cancel-observations
            ()
            (dolist (observation observations)
              (e-board-sqlite-delivery-observation-cancel observation))
            (setq observations nil))
           (settle-once
            (status &rest args)
            (unless settled-p
              (setq settled-p t)
              (cancel-observations)
              (apply on-settle :status status args)))
           (delivery-settled
            (delivery-id status payload)
            (when (member delivery-id pending-delivery-ids)
              (setq pending-delivery-ids
                    (delete delivery-id pending-delivery-ids))
              (push (list :delivery-id (copy-tree delivery-id t)
                          :status status :payload (copy-tree payload t))
                    delivery-outcomes)
              (unless pending-delivery-ids
                (let ((terminal-status
                       (cond
                        ((seq-some (lambda (outcome)
                                     (eq (plist-get outcome :status) 'failed))
                                   delivery-outcomes)
                         'failed)
                        ((seq-some (lambda (outcome)
                                     (eq (plist-get outcome :status) 'cancelled))
                                   delivery-outcomes)
                         'cancelled)
                        (t 'done))))
                  (if (eq terminal-status 'done)
                      (settle-once
                       'done :outputs
                       (list
                        (list :kind 'board-work
                              :value
                              (list :publication
                                    (copy-tree canonical-publication t)
                                    :deliveries
                                    (nreverse (copy-tree delivery-outcomes t))))))
                    (settle-once
                     terminal-status
                     :error
                     (or (seq-some
                          (lambda (outcome)
                            (when-let* ((error
                                        (plist-get
                                         (plist-get outcome :payload) :error)))
                              (e-work-error-message error)))
                          delivery-outcomes)
                         (format "Board task delivery %s" terminal-status)))))))))
      (e-work-on-settle
       publication
       (lambda (settled)
         (let* ((status (e-work-status settled))
                (state (plist-get status :state)))
           (pcase state
             ('finished
              (setq canonical-publication
                    (copy-tree (plist-get status :result) t))
              (let* ((routing (plist-get canonical-publication :routing))
                     (routing-state (plist-get routing :state))
                     (pickups (append
                               (plist-get canonical-publication :pickups) nil)))
                (if (or (eq routing-state 'unrouted) (null pickups))
                    (settle-once
                     'unrouted
                     :error
                     (format "Board task was unrouted: %s"
                             (or (plist-get routing :reason) 'no-pickup)))
                  (setq pending-delivery-ids
                        (mapcar (lambda (pickup)
                                  (copy-tree
                                   (plist-get pickup :delivery-id)))
                                pickups))
                  (dolist (delivery-id pending-delivery-ids)
                    (let ((observed-id (copy-tree delivery-id t)))
                      (push
                       (e-board-sqlite-publication-target-observe-delivery-outcome
                        target observed-id
                        (lambda (delivery-status payload)
                          (delivery-settled
                           observed-id delivery-status payload)))
                       observations))))))
             ('cancelled (settle-once 'cancelled))
             (_ (settle-once
                 'failed
                 :error (e-work-error-message
                         (plist-get status :error))))))))
      (list :publication publication
            :cancel
            (lambda ()
              (cancel-observations)
              (unless (memq (plist-get (e-work-status publication) :state)
                            '(finished failed cancelled))
                (e-work-cancel publication))))))))

;; --- persistence ------------------------------------------------------------

(defun e-task-queue-finalize (queue on-done on-error)
  "Asynchronously finalize QUEUE's current durability boundary.
Call ON-DONE with QUEUE after the worker commits the current snapshot, or call
ON-ERROR with the writer error.  Return QUEUE immediately."
  (unless (e-task-queue-storage-backed-p queue)
    (signal 'e-task-queue-error (list "In-memory task queue is not durable")))
  (condition-case err
      (e-task-storage-submit
       (e-task-queue-storage queue) 'read 'status
       (list (e-task-queue-id queue))
       (lambda (_result error)
         (if error
             (when on-error (funcall on-error error))
           (when on-done (funcall on-done queue)))))
    (error (when on-error (funcall on-error err))))
  queue)

(defun e-task-queue-initialize (queue)
  "Initialize QUEUE's process-local scheduler state without issuing SQL.
Call `e-task-queue-start' from the explicit scheduler boundary to query and
atomically claim runnable work."
  (setf (e-task-queue-loaded-p queue) t)
  queue)

(define-obsolete-function-alias 'e-task-queue-load
  #'e-task-queue-initialize "2026-09-06")

(defun e-task-queue-start (queue)
  "Start QUEUE's live scheduler without restoring durable history.
Persistent queues submit one atomic runnable claim; in-memory queues dispatch
their process-local records."
  (setf (e-task-queue-loaded-p queue) t)
  (e-task-queue--dispatch queue)
  queue)

(defun e-task-queue-delete-history (queue)
  "Explicitly delete QUEUE's durable records and attempt history."
  (unless (e-task-queue-storage-backed-p queue)
    (signal 'e-task-queue-error
            (list "History deletion requires SQLite task storage")))
  (e-task-queue--ensure-storage-root queue)
  (let ((result
         (e-task-storage-delete-history
          (e-task-queue-storage queue) (e-task-queue-id queue))))
    (setf (e-task-queue-revision queue) (plist-get result :revision)
          (e-task-queue-sequence queue) (plist-get result :sequence)
          (e-task-queue-order queue) nil)
    (clrhash (e-task-queue-records queue))
    (e-task-queue--notify queue)
    result))

(provide 'e-task-queue)

;;; e-task-queue.el ends here

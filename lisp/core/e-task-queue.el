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
;; default runner requires explicit live producer authority and publishes one
;; board fact; it never creates or prompts a harness session.
;;
;; This module depends only on the core harness and the harness-instance
;; catalog, never on a UI shell, so the queue runs headless.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-board-runtime)
(require 'e-harness)
(require 'e-harness-instances)
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

(defcustom e-task-queue-directory (locate-user-emacs-file "e/task-queue/")
  "Default directory the persistent task queue writes its records to.
Mirrors `e-session-directory'.  A queue constructed without a directory stays
in-memory."
  :type 'directory
  :group 'e-task-queue)

(defcustom e-task-queue-write-delay 0.1
  "Seconds to coalesce persistent task-queue writes before flushing."
  :type 'number
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

(defcustom e-task-queue-snapshot-byte-limit (* 2 1024 1024)
  "Maximum encoded size of one durable task snapshot."
  :type 'integer
  :group 'e-task-queue)

(defcustom e-task-queue-snapshot-node-limit 32768
  "Maximum Lisp value nodes inspected before encoding a task snapshot."
  :type 'integer
  :group 'e-task-queue)

(defcustom e-task-queue-node-executable "node"
  "Node executable used by the asynchronous task snapshot writer."
  :type 'string
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

(defvar e-task-queue-rehydrate-functions nil
  "Abnormal hook run for a durable record before a restored queue dispatches.
Each function receives QUEUE, the live RECORD, and its persisted status.")

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
  "An in-memory task queue with a bounded dispatcher.
RECORDS maps task ids to mutable task plists.  ORDER lists task ids in enqueue
order (oldest first).  MAX-PARALLEL, DEFAULT-HARNESS-INSTANCE-ID, and RUNNER
override the module defaults when non-nil.  EXPOSE-AWAIT-REFERENCES-P marks the
one public queue whose task ids the global =task:= resolver can resolve.
DISPATCHING guards dispatch
re-entrancy so a synchronous runner settle does not recurse.  PAUSED-P is the
queue-level gate that stops the dispatcher from starting new work.  DIRECTORY,
when non-nil, makes the queue durable and names where records are written;
LOADED-P records successful rehydration of that queue instance; WRITE-TIMER
coalesces durable writes."
  (records (make-hash-table :test 'equal))
  (order nil)
  (sequence 0)
  max-parallel
  default-harness-instance-id
  runner
  producer-binding
  dispatching
  paused-p
  max-retries
  expose-await-references-p
  directory
  loaded-p
  write-timer
  write-process
  write-dirty-p
  write-failed-p
  write-callbacks)

(cl-defun e-task-queue-create (&key max-parallel default-harness-instance-id
                                    runner producer-binding max-retries directory
                                    expose-await-references-p)
  "Return a new task queue.
MAX-PARALLEL, DEFAULT-HARNESS-INSTANCE-ID, MAX-RETRIES, and RUNNER override the
module defaults for this queue when non-nil.  Without RUNNER, PRODUCER-BINDING
must name current process-local board authority and queued tasks publish facts.
EXPOSE-AWAIT-REFERENCES-P is reserved for a queue whose task ids are registered
with the global waitable resolver; private scheduler queues must leave it nil.
DIRECTORY, when non-nil, makes the queue durable and stores its records there;
without it the queue stays in-memory."
  (e-task-queue--create
   :max-parallel max-parallel
   :default-harness-instance-id default-harness-instance-id
   :runner runner
   :producer-binding producer-binding
   :max-retries max-retries
   :expose-await-references-p expose-await-references-p
   :directory directory))

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
           :outputs (plist-get record :outputs)
           :error (plist-get record :error)))))

(defun e-task-queue--notify (queue)
  "Run change hooks for QUEUE."
  (run-hook-with-args 'e-task-queue-change-functions queue))

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
      ('cancelled (e-work-cancel handle)))))

;; --- dispatch helpers -------------------------------------------------------

(defun e-task-queue--running-count (queue)
  "Return the number of running tasks in QUEUE."
  (let ((count 0))
    (maphash (lambda (_id record)
               (when (eq (plist-get record :status) 'running)
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
  "Settle a running TASK-ID in QUEUE.
ARGS may carry `:outputs' and `:error'.  No-op unless the task is still
running, so a runner that settles after the dispatcher cancelled the task is
dropped.  A `cancelled' settle for a task the operator asked to pause lands
`paused' (non-terminal) instead, preserving any partial outputs.  Re-dispatches
QUEUE after a real transition."
  (let ((record (gethash task-id (e-task-queue-records queue))))
    (when (and record (eq (plist-get record :status) 'running))
      (unless (e-task-queue--value-within-budget-p
               args e-task-queue-record-node-limit e-task-queue-record-byte-limit)
        (setq status 'failed
              args (list :error "Task result exceeds retention budget")))
      (when (plist-member args :outputs)
        (plist-put record :outputs (plist-get args :outputs)))
      (when (plist-member args :error)
        (plist-put record :error (plist-get args :error)))
      (plist-put record :handle nil)
      (cond
       ((and (plist-get record :pausing) (eq status 'cancelled))
        ;; The abort came from a pause request: hold, do not terminate.
        (plist-put record :pausing nil)
        (plist-put record :status 'paused)
        (plist-put record :started-at nil))
       ((and (eq status 'failed)
             (e-task-queue--maybe-retry queue record))
        ;; A failed task with retries left was re-armed as `queued'; the
        ;; dispatcher below will start the analyze-and-continue retry.
        nil)
       (t
        (plist-put record :status status)
        (plist-put record :finished-at (e-task-queue--timestamp))
        (e-task-queue--settle-work-handle record status)
        (run-hook-with-args 'e-task-queue-terminal-functions
                            queue (e-task-queue--normalize queue record))))
      (e-task-queue--notify queue)
      (e-task-queue--dispatch queue))))

(defun e-task-queue--start (queue task-id)
  "Transition TASK-ID in QUEUE to running and invoke the runner.
Resolves the task's harness instance at this moment.  A task whose instance id
is missing or unresolvable settles `failed' without stalling the dispatcher."
  (let* ((record (gethash task-id (e-task-queue-records queue)))
         (instance-id (or (plist-get record :harness-instance-id)
                          (e-task-queue--default-instance-id queue))))
    ;; A nil instance id is a dispatch-time choice.  Once the task starts,
    ;; retain the resolved target so its completed session can be identified
    ;; after a restart even if the queue default later changes.
    (when instance-id
      (plist-put record :harness-instance-id instance-id))
    (plist-put record :status 'running)
    (plist-put record :started-at (e-task-queue--timestamp))
    (e-task-queue--notify queue)
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
        (let ((handle
               (funcall (e-task-queue--runner queue)
                        (e-task-queue--normalize queue record)
                        (if (e-task-queue-runner queue) harness queue)
                        (lambda (&rest settle-args)
                          (apply #'e-task-queue--settle queue task-id
                                 (or (plist-get settle-args :status) 'done)
                                 settle-args)))))
          (when-let ((session-id (and (listp handle)
                                      (plist-get handle :session-id))))
            (plist-put record :session-id session-id))
          ;; A synchronous runner may already have settled the task and
          ;; cleared its handle; only a still-running task keeps a live handle.
          (when (eq (plist-get record :status) 'running)
            (plist-put record :handle handle)))))))

(defun e-task-queue--dispatch (queue)
  "Start queued tasks in QUEUE up to the parallelism cap.
The queue-level pause gate blocks all new starts while set.  Guards against
re-entrancy so a synchronous runner settle does not recurse; the loop picks up
any task the settle frees."
  (unless (or (e-task-queue-dispatching queue)
              (e-task-queue-paused-p queue))
    (setf (e-task-queue-dispatching queue) t)
    (unwind-protect
        (let (next)
          (while (and (< (e-task-queue--running-count queue)
                         (e-task-queue--max-parallel queue))
                      (setq next (e-task-queue--oldest-queued queue)))
            (e-task-queue--start queue next)))
      (setf (e-task-queue-dispatching queue) nil))))

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
             (not (e-board-runtime-producer-binding-live-p
                   (e-task-queue-producer-binding queue))))
    (signal 'e-board-runtime-producer-disabled
            (list 'task-queue 'missing-live-binding)))
  (let* ((task-id (e-task-queue--next-id queue))
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
                       :handle nil
                       :work-handle nil)))
    (plist-put record :work-handle
               (e-task-queue--work-handle-for-status record))
    (puthash task-id record (e-task-queue-records queue))
    (setf (e-task-queue-order queue)
          (append (e-task-queue-order queue) (list task-id)))
    (e-task-queue--notify queue)
    (e-task-queue--dispatch queue)
    (e-task-queue--normalize queue (e-task-queue--record queue task-id))))

(defun e-task-queue-cancel (queue task-id)
  "Cancel TASK-ID in QUEUE and return its normalized record.
A queued or paused task becomes `cancelled' without ever running.  A running
task is interrupted through its runner handle and its result is dropped on
settle.  Terminal tasks are returned unchanged."
  (let ((record (e-task-queue--record queue task-id)))
    (pcase (plist-get record :status)
      ((or 'queued 'paused)
       (plist-put record :status 'cancelled)
       (plist-put record :pausing nil)
       (plist-put record :finished-at (e-task-queue--timestamp))
       (e-task-queue--settle-work-handle record 'cancelled)
       (e-task-queue--notify queue))
      ('running
       (let ((handle (plist-get record :handle)))
         (plist-put record :status 'cancelled)
         (plist-put record :pausing nil)
         (plist-put record :finished-at (e-task-queue--timestamp))
         (plist-put record :handle nil)
         (when (and (listp handle) (functionp (plist-get handle :cancel)))
           (ignore-errors (funcall (plist-get handle :cancel))))
         (e-task-queue--settle-work-handle record 'cancelled)
         (e-task-queue--notify queue)
         (e-task-queue--dispatch queue))))
    (e-task-queue--normalize queue record)))

(defun e-task-queue-pause (queue task-id)
  "Pause TASK-ID in QUEUE and return its normalized record.
A queued task moves to `paused' in place.  A running task is asked to stop at
its next turn boundary: the runner handle aborts the active turn and the
`cancelled' settle that follows is interpreted as a pause, landing the record
in `paused' with any partial outputs preserved.  A paused or terminal task is
returned unchanged."
  (let ((record (e-task-queue--record queue task-id)))
    (pcase (plist-get record :status)
      ('queued
       (plist-put record :status 'paused)
       (e-task-queue--notify queue))
      ('running
       (let ((handle (plist-get record :handle)))
         ;; Mark the pause intent so the abort's `cancelled' settle lands
         ;; `paused' rather than terminating the task.
         (plist-put record :pausing t)
         (if (and (listp handle) (functionp (plist-get handle :cancel)))
             (ignore-errors (funcall (plist-get handle :cancel)))
           ;; No live handle to abort; hold the task directly.
           (plist-put record :pausing nil)
           (plist-put record :status 'paused)
           (plist-put record :handle nil)
           (plist-put record :started-at nil)
           (e-task-queue--notify queue)
           (e-task-queue--dispatch queue)))))
    (e-task-queue--normalize queue record)))

(defun e-task-queue-resume (queue task-id)
  "Resume a paused TASK-ID in QUEUE and return its normalized record.
The task returns to `queued', where the dispatcher re-runs it from its prompt
under the normal cap.  A non-paused task is returned unchanged."
  (let ((record (e-task-queue--record queue task-id)))
    (when (eq (plist-get record :status) 'paused)
      (plist-put record :status 'queued)
      (plist-put record :started-at nil)
      (plist-put record :finished-at nil)
      (e-task-queue--notify queue)
      (e-task-queue--dispatch queue))
    (e-task-queue--normalize queue record)))

(defun e-task-queue-pause-all (queue)
  "Set QUEUE's pause gate and pause every non-terminal task.
While the gate is set the dispatcher starts no new work.  Returns QUEUE."
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
  (setf (e-task-queue-paused-p queue) nil)
  (dolist (task-id (copy-sequence (e-task-queue-order queue)))
    (let ((record (gethash task-id (e-task-queue-records queue))))
      (when (eq (plist-get record :status) 'paused)
        (plist-put record :status 'queued)
        (plist-put record :started-at nil)
        (plist-put record :finished-at nil))))
  (e-task-queue--notify queue)
  (e-task-queue--dispatch queue)
  queue)

;; --- default runner ---------------------------------------------------------

(defun e-task-queue-default-runner (task harness on-settle)
  "Publish TASK as board work through QUEUE-owned process authority.
HARNESS is the queue sentinel carrying QUEUE for the default path.  Custom test
and application runners retain the historical runner signature."
  (unless (e-task-queue-p harness)
    (signal 'e-task-queue-error (list "Default runner requires its queue")))
  (let ((binding (e-task-queue-producer-binding harness)))
    (unless (e-board-runtime-producer-binding-live-p binding)
      (signal 'e-board-runtime-producer-disabled
              (list (plist-get task :task-id) 'missing-live-binding)))
    (let ((publication
           (e-board-runtime-producer-publish-input
            binding
            :tags '(task-queue task)
            :attributes
            (list :task-id (plist-get task :task-id)
                  :summary (plist-get task :summary)
                  :metadata (copy-tree (plist-get task :metadata)))
            :content (plist-get task :prompt)
            :reference (format "task:%s" (plist-get task :task-id))
            :on-settle
            (lambda (&rest result)
              (let ((status (plist-get result :status)))
                (apply on-settle
                       :status status
                       :outputs
                       (list (list :kind 'board-work
                                   :value result))
                       (when (eq status 'unrouted)
                         (list :error
                               (format "Board work unrouted: %s"
                                       (plist-get result :reason))))))))))
      (list :publication publication
            :cancel
            (lambda ()
              (e-board-runtime-producer-cancel publication))))))

;; --- persistence ------------------------------------------------------------

(defconst e-task-queue--persist-fields
  '(:task-id :status :prompt :origin-prompt :summary :prompt-summary :metadata
    :harness-instance-id :enqueued-at :started-at :finished-at :session-id
    :retries :outputs :error)
  "Durable task record fields.
The transient `:handle', `:pausing', and the live harness are never persisted.")

(defun e-task-queue--record-file (queue)
  "Return the on-disk records file for QUEUE, or nil when in-memory."
  (when-let ((directory (e-task-queue-directory queue)))
    (expand-file-name "records.eld" directory)))

(defun e-task-queue--persistable-record (record)
  "Return a durable copy of RECORD carrying only persisted fields."
  (let (durable)
    (dolist (key e-task-queue--persist-fields)
      (setq durable (plist-put durable key (plist-get record key))))
    durable))

(defun e-task-queue--serialize (queue)
  "Return QUEUE's durable state as a plist."
  (list :sequence (e-task-queue-sequence queue)
        :order (e-task-queue-order queue)
        :records (mapcar (lambda (task-id)
                           (e-task-queue--persistable-record
                            (gethash task-id (e-task-queue-records queue))))
                         (e-task-queue-order queue))))

(defun e-task-queue--snapshot-string (queue)
  "Return QUEUE's bounded durable snapshot string."
  (let ((value (e-task-queue--serialize queue)))
    (unless (e-task-queue--value-within-budget-p
             value e-task-queue-snapshot-node-limit
             e-task-queue-snapshot-byte-limit)
      (signal 'e-task-queue-error
              (list "Task queue snapshot exceeds pre-encoding budget")))
    (let ((snapshot (let ((print-length nil) (print-level nil))
                      (prin1-to-string value))))
      (when (> (string-bytes snapshot) e-task-queue-snapshot-byte-limit)
        (signal 'e-task-queue-error
                (list "Task queue snapshot exceeds byte limit")))
      snapshot)))

(defun e-task-queue--writer-script ()
  "Return the bundled asynchronous task writer path."
  (expand-file-name
   "e-task-queue-writer.mjs"
   (file-name-directory
    (file-truename (or load-file-name buffer-file-name
                       (locate-library "e-task-queue") default-directory)))))

(defun e-task-queue--set-write-failure (queue failed)
  "Set QUEUE's FAILED writer state and its quiescence counter."
  (unless (eq failed (e-task-queue-write-failed-p queue))
    (setf (e-task-queue-write-failed-p queue) failed)
    (e-task-queue--adjust-writer-state 'failures (if failed 1 -1))))

(defun e-task-queue--finish-callbacks (queue succeeded error-value)
  "Finish QUEUE callbacks with SUCCEEDED and ERROR-VALUE."
  (let ((callbacks (nreverse (e-task-queue-write-callbacks queue))))
    (setf (e-task-queue-write-callbacks queue) nil)
    (dolist (callback callbacks)
      (if succeeded
          (when (car callback) (funcall (car callback) queue))
        (when (cdr callback) (funcall (cdr callback) error-value))))))

(defun e-task-queue--writer-finished (queue process)
  "Handle terminal PROCESS state for QUEUE's current writer."
  (when (and (memq (process-status process) '(exit signal))
             (eq process (e-task-queue-write-process queue)))
    (setf (e-task-queue-write-process queue) nil)
    (let ((failed (not (zerop (process-exit-status process)))))
      (when failed
        (setf (e-task-queue-write-dirty-p queue) nil))
      (e-task-queue--set-write-failure queue failed)
      (if (and (not failed) (e-task-queue-write-dirty-p queue))
          (progn
            (setf (e-task-queue-write-dirty-p queue) nil)
            (condition-case err
                (e-task-queue--start-async-write queue)
              (error
               (e-task-queue--set-write-failure queue t)
               (e-task-queue--adjust-writer-state 'writes -1)
               (e-task-queue--finish-callbacks queue nil err))))
        (e-task-queue--adjust-writer-state 'writes -1)
        (e-task-queue--finish-callbacks
         queue (not failed)
         (and failed
              (list 'e-task-queue-error "Task queue writer failed")))))))

(defun e-task-queue--start-async-write (queue)
  "Send one bounded QUEUE snapshot to the external writer."
  (let* ((file (e-task-queue--record-file queue))
         (node (executable-find e-task-queue-node-executable))
         (snapshot (e-task-queue--snapshot-string queue)))
    (unless node
      (signal 'e-task-queue-error
              (list "Cannot find task queue Node writer")))
    (make-directory (file-name-directory file) t)
    (let ((process
           (make-process
            :name "e-task-queue-writer" :buffer nil :noquery t
            :command (list node (e-task-queue--writer-script) file)
            :connection-type 'pipe :coding 'utf-8-unix
            :sentinel (lambda (process _event)
                        (e-task-queue--writer-finished queue process)))))
      (setf (e-task-queue-write-process queue) process)
      (condition-case err
          (progn
            (process-send-string process snapshot)
            (process-send-eof process))
        (error
         (setf (e-task-queue-write-process queue) nil)
         (when (process-live-p process) (delete-process process))
         (signal (car err) (cdr err)))))))

(defun e-task-queue--schedule-write (queue)
  "Schedule a coalesced durable write for QUEUE.
No-op for an in-memory queue.  Reuses a pending timer so a burst of mutations
collapses into one write off the hot enqueue/settle path."
  (when (and (e-task-queue-directory queue)
             (not (timerp (e-task-queue-write-timer queue))))
    (if (process-live-p (e-task-queue-write-process queue))
        (setf (e-task-queue-write-dirty-p queue) t)
      (e-task-queue--adjust-writer-state 'writes 1)
      (setf (e-task-queue-write-timer queue)
            (run-at-time
             (max 0 (or e-task-queue-write-delay 0)) nil
             (lambda ()
               (when (e-task-queue-p queue)
                 (setf (e-task-queue-write-timer queue) nil)
                 (condition-case err
                     (e-task-queue--start-async-write queue)
                   (error
                    (e-task-queue--set-write-failure queue t)
                    (e-task-queue--adjust-writer-state 'writes -1)
                    (signal (car err) (cdr err)))))))))))

(defun e-task-queue-finalize (queue on-done on-error)
  "Asynchronously finalize QUEUE's current durability boundary.
Call ON-DONE with QUEUE after the worker commits the current snapshot, or call
ON-ERROR with the writer error.  Return QUEUE immediately."
  (unless (e-task-queue-directory queue)
    (signal 'e-task-queue-error (list "In-memory task queue is not durable")))
  (push (cons on-done on-error) (e-task-queue-write-callbacks queue))
  (cond
   ((process-live-p (e-task-queue-write-process queue)) nil)
   ((timerp (e-task-queue-write-timer queue))
    (cancel-timer (e-task-queue-write-timer queue))
    (setf (e-task-queue-write-timer queue) nil)
    (condition-case err
        (e-task-queue--start-async-write queue)
      (error
       (e-task-queue--set-write-failure queue t)
       (e-task-queue--adjust-writer-state 'writes -1)
       (e-task-queue--finish-callbacks queue nil err))))
   (t
    (e-task-queue--adjust-writer-state 'writes 1)
    (condition-case err
        (e-task-queue--start-async-write queue)
      (error
       (e-task-queue--set-write-failure queue t)
       (e-task-queue--adjust-writer-state 'writes -1)
       (e-task-queue--finish-callbacks queue nil err)))))
  queue)

(defun e-task-queue--persist-on-change (queue)
  "Persist QUEUE on a change event.
Bound to `e-task-queue-change-functions' so durable queues write off every
task mutation without the core knowing about disk on its hot path."
  (when (e-task-queue-directory queue)
    (e-task-queue--schedule-write queue)))

(add-hook 'e-task-queue-change-functions #'e-task-queue--persist-on-change)

(defun e-task-queue--load-record (queue durable)
  "Return a live internal record from a DURABLE persisted record.
A task that was `running' at shutdown could not have survived its turn, so it
is normalized to `queued' for a best-effort re-run."
  (let ((record (copy-sequence durable))
        (persisted-status (plist-get durable :status)))
    (when (eq (plist-get record :status) 'running)
      (setq record (plist-put record :status 'queued))
      (setq record (plist-put record :started-at nil))
      (setq record (plist-put record :finished-at nil)))
    (setq record (plist-put record :handle nil))
    (setq record (plist-put record :pausing nil))
    (setq record
          (plist-put record :work-handle
                     (e-task-queue--work-handle-for-status record)))
    (run-hook-with-args 'e-task-queue-rehydrate-functions queue record persisted-status)
    record))

(defun e-task-queue-load (queue)
  "Load QUEUE's persisted records from disk and re-dispatch.  Return QUEUE.
A `running' record loads as `queued'; `paused', terminal, and `queued' states
load unchanged.  A queue with no directory or no records file is left empty.
`e-task-queue-loaded-p' becomes non-nil only after the complete load succeeds."
  (when-let* ((file (e-task-queue--record-file queue))
              ((file-exists-p file)))
    (let ((state (with-temp-buffer
                   (let ((coding-system-for-read 'utf-8))
                     (insert-file-contents file))
                   (goto-char (point-min))
                   (read (current-buffer)))))
      (clrhash (e-task-queue-records queue))
      (setf (e-task-queue-order queue) (plist-get state :order))
      (setf (e-task-queue-sequence queue) (or (plist-get state :sequence) 0))
      (dolist (durable (plist-get state :records))
        (let ((record (e-task-queue--load-record queue durable)))
          (puthash (plist-get record :task-id) record
                   (e-task-queue-records queue))))
      (e-task-queue--notify queue)
      (e-task-queue--dispatch queue)))
  (setf (e-task-queue-loaded-p queue) t)
  queue)

(provide 'e-task-queue)

;;; e-task-queue.el ends here

;;; e-task-queue.el --- Bounded-concurrency agent task queue for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A durable, bounded-concurrency queue of agent work items.  Each task is a
;; prompt plus metadata that runs as an ordinary harness turn, carries an
;; explicit status lifecycle, and collects its outputs.  The queue runs at most
;; `e-task-queue-max-parallel' tasks concurrently and dispatches FIFO by
;; enqueue time.
;;
;; The harness stays unaware of the queue: a task runs through a pluggable
;; runner that, by default, submits the prompt through `e-harness-prompt-async'
;; the same way a background session would.  The harness instance a task runs on
;; is chosen per task and resolved at dispatch time, mirroring how a chat buffer
;; picks its instance; re-pointing the default or fixing a bad instance id still
;; affects tasks that are merely queued.
;;
;; This module depends only on the core harness and the harness-instance
;; catalog, never on a UI shell, so the queue runs headless.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-harness)
(require 'e-harness-instances)

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

(defvar e-task-queue-change-functions nil
  "Abnormal hook run after a queue's task set or a task record changes.
Each function is called with the queue.  Intended for observation (shells,
tests); handlers must not mutate the queue.")

(define-error 'e-task-queue-unknown-task "Unknown task id")

(cl-defstruct (e-task-queue (:constructor e-task-queue--create))
  "An in-memory task queue with a bounded dispatcher.
RECORDS maps task ids to mutable task plists.  ORDER lists task ids in enqueue
order (oldest first).  MAX-PARALLEL, DEFAULT-HARNESS-INSTANCE-ID, and RUNNER
override the module defaults when non-nil.  DISPATCHING guards dispatch
re-entrancy so a synchronous runner settle does not recurse.  PAUSED-P is the
queue-level gate that stops the dispatcher from starting new work.  DIRECTORY,
when non-nil, makes the queue durable and names where records are written;
WRITE-TIMER coalesces those writes."
  (records (make-hash-table :test 'equal))
  (order nil)
  (sequence 0)
  max-parallel
  default-harness-instance-id
  runner
  dispatching
  paused-p
  directory
  write-timer)

(cl-defun e-task-queue-create (&key max-parallel default-harness-instance-id
                                    runner directory)
  "Return a new task queue.
MAX-PARALLEL, DEFAULT-HARNESS-INSTANCE-ID, and RUNNER override the module
defaults for this queue when non-nil.  DIRECTORY, when non-nil, makes the queue
durable and stores its records there; without it the queue stays in-memory."
  (e-task-queue--create
   :max-parallel max-parallel
   :default-harness-instance-id default-harness-instance-id
   :runner runner
   :directory directory))

;; --- configuration accessors ------------------------------------------------

(defun e-task-queue--max-parallel (queue)
  "Return the effective parallelism cap for QUEUE."
  (or (e-task-queue-max-parallel queue) e-task-queue-max-parallel))

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

(defun e-task-queue--record (queue task-id)
  "Return the mutable internal record for TASK-ID in QUEUE, or signal."
  (or (gethash task-id (e-task-queue-records queue))
      (signal 'e-task-queue-unknown-task (list task-id))))

(defun e-task-queue--normalize (record)
  "Return a model-facing copy of RECORD without runtime-only fields."
  (list :task-id (plist-get record :task-id)
        :status (plist-get record :status)
        :prompt (plist-get record :prompt)
        :prompt-summary (plist-get record :prompt-summary)
        :metadata (plist-get record :metadata)
        :harness-instance-id (plist-get record :harness-instance-id)
        :enqueued-at (plist-get record :enqueued-at)
        :started-at (plist-get record :started-at)
        :finished-at (plist-get record :finished-at)
        :session-id (plist-get record :session-id)
        :outputs (plist-get record :outputs)
        :error (plist-get record :error)))

(defun e-task-queue--notify (queue)
  "Run change hooks for QUEUE."
  (run-hook-with-args 'e-task-queue-change-functions queue))

;; --- public reads -----------------------------------------------------------

(defun e-task-queue-get (queue task-id)
  "Return the normalized record for TASK-ID in QUEUE."
  (e-task-queue--normalize (e-task-queue--record queue task-id)))

(defun e-task-queue-list (queue)
  "Return normalized task records in QUEUE, newest-first."
  (mapcar (lambda (task-id)
            (e-task-queue--normalize (gethash task-id (e-task-queue-records queue))))
          (reverse (e-task-queue-order queue))))

(defun e-task-queue-outputs (queue task-id)
  "Return the outputs collected for TASK-ID in QUEUE."
  (plist-get (e-task-queue--record queue task-id) :outputs))

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

(defun e-task-queue--settle (queue task-id status &rest args)
  "Settle a running TASK-ID in QUEUE.
ARGS may carry `:outputs' and `:error'.  No-op unless the task is still
running, so a runner that settles after the dispatcher cancelled the task is
dropped.  A `cancelled' settle for a task the operator asked to pause lands
`paused' (non-terminal) instead, preserving any partial outputs.  Re-dispatches
QUEUE after a real transition."
  (let ((record (gethash task-id (e-task-queue-records queue))))
    (when (and record (eq (plist-get record :status) 'running))
      (when (plist-member args :outputs)
        (plist-put record :outputs (plist-get args :outputs)))
      (when (plist-member args :error)
        (plist-put record :error (plist-get args :error)))
      (plist-put record :handle nil)
      (if (and (plist-get record :pausing) (eq status 'cancelled))
          ;; The abort came from a pause request: hold, do not terminate.
          (progn
            (plist-put record :pausing nil)
            (plist-put record :status 'paused)
            (plist-put record :started-at nil))
        (plist-put record :status status)
        (plist-put record :finished-at (e-task-queue--timestamp)))
      (e-task-queue--notify queue)
      (e-task-queue--dispatch queue))))

(defun e-task-queue--start (queue task-id)
  "Transition TASK-ID in QUEUE to running and invoke the runner.
Resolves the task's harness instance at this moment.  A task whose instance id
is missing or unresolvable settles `failed' without stalling the dispatcher."
  (let* ((record (gethash task-id (e-task-queue-records queue)))
         (instance-id (or (plist-get record :harness-instance-id)
                          (e-task-queue--default-instance-id queue))))
    (plist-put record :status 'running)
    (plist-put record :started-at (e-task-queue--timestamp))
    (e-task-queue--notify queue)
    (let ((harness
           (condition-case err
               (if instance-id
                   (e-harness-instance-get-or-create instance-id)
                 (signal 'e-task-queue-unknown-task
                         (list "No harness instance for task")))
             (error
              (e-task-queue--settle
               queue task-id 'failed
               :error (format "Cannot resolve harness instance %s: %s"
                              instance-id (error-message-string err)))
              nil))))
      (when harness
        (let ((handle
               (funcall (e-task-queue--runner queue)
                        (e-task-queue--normalize record)
                        harness
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

(cl-defun e-task-queue-enqueue (queue &key prompt metadata harness-instance-id)
  "Enqueue PROMPT on QUEUE and return its normalized record.
METADATA is an opaque plist the enqueuer owns.  HARNESS-INSTANCE-ID selects the
configured harness instance the task runs on; nil uses the queue default,
resolved at dispatch time.  Dispatch runs before returning, so a task may
already be running when this returns."
  (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
    (signal 'wrong-type-argument (list 'stringp :prompt)))
  (let* ((task-id (e-task-queue--next-id queue))
         (record (list :task-id task-id
                       :status 'queued
                       :prompt prompt
                       :prompt-summary (e-task-queue--prompt-summary prompt)
                       :metadata metadata
                       :harness-instance-id harness-instance-id
                       :enqueued-at (e-task-queue--timestamp)
                       :started-at nil
                       :finished-at nil
                       :session-id nil
                       :outputs nil
                       :error nil
                       :handle nil)))
    (puthash task-id record (e-task-queue-records queue))
    (setf (e-task-queue-order queue)
          (append (e-task-queue-order queue) (list task-id)))
    (e-task-queue--notify queue)
    (e-task-queue--dispatch queue)
    (e-task-queue--normalize (e-task-queue--record queue task-id))))

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
       (e-task-queue--notify queue))
      ('running
       (let ((handle (plist-get record :handle)))
         (plist-put record :status 'cancelled)
         (plist-put record :pausing nil)
         (plist-put record :finished-at (e-task-queue--timestamp))
         (plist-put record :handle nil)
         (when (and (listp handle) (functionp (plist-get handle :cancel)))
           (ignore-errors (funcall (plist-get handle :cancel))))
         (e-task-queue--notify queue)
         (e-task-queue--dispatch queue))))
    (e-task-queue--normalize record)))

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
    (e-task-queue--normalize record)))

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
    (e-task-queue--normalize record)))

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

(defun e-task-queue--last-assistant-text (harness session-id)
  "Return the last assistant message text for SESSION-ID in HARNESS, or nil."
  (let ((content
         (cl-some (lambda (message)
                    (and (eq (plist-get message :role) 'assistant)
                         (plist-get message :content)))
                  (reverse (e-harness-messages harness session-id)))))
    (and (stringp content) content)))

(defun e-task-queue-default-runner (task harness on-settle)
  "Run TASK on HARNESS by submitting its prompt as one harness turn.
Settles through ON-SETTLE when the turn finishes, fails, or is cancelled.
Returns a handle plist carrying the created `:session-id' and a `:cancel'
function that aborts the active turn."
  (let* ((session (e-harness-create-session
                   harness :metadata (plist-get task :metadata)))
         (session-id (plist-get session :id))
         (settled nil)
         subscription)
    (cl-labels
        ((finish
          (status &rest args)
          (unless settled
            (setq settled t)
            (when subscription
              (e-harness-unsubscribe harness subscription))
            (apply on-settle :status status args))))
      (setq subscription
            (e-harness-subscribe
             harness
             (lambda (event)
               (pcase (plist-get event :type)
                 ('turn-finished
                  (finish 'done
                          :outputs
                          (when-let ((text (e-task-queue--last-assistant-text
                                            harness session-id)))
                            (list (list :kind 'text :value text
                                        :label "assistant")))))
                 ('turn-failed
                  (finish 'failed
                          :error (or (plist-get (plist-get event :payload) :error)
                                     "Task turn failed")))
                 ('turn-cancelled
                  (finish 'cancelled))))
             :session-id session-id))
      (condition-case err
          (e-harness-prompt-async harness session-id (plist-get task :prompt))
        (error
         (finish 'failed :error (error-message-string err))))
      (list :session-id session-id
            :cancel (lambda () (ignore-errors (e-harness-abort harness session-id)))))))

;; --- persistence ------------------------------------------------------------

(defconst e-task-queue--persist-fields
  '(:task-id :status :prompt :prompt-summary :metadata :harness-instance-id
    :enqueued-at :started-at :finished-at :session-id :outputs :error)
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

(defun e-task-queue--write-now (queue)
  "Write QUEUE's durable state to disk immediately."
  (when-let ((file (e-task-queue--record-file queue)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file
        (let ((print-length nil)
              (print-level nil))
          (prin1 (e-task-queue--serialize queue) (current-buffer)))))))

(defun e-task-queue--schedule-write (queue)
  "Schedule a coalesced durable write for QUEUE.
No-op for an in-memory queue.  Reuses a pending timer so a burst of mutations
collapses into one write off the hot enqueue/settle path."
  (when (and (e-task-queue-directory queue)
             (not (timerp (e-task-queue-write-timer queue))))
    (setf (e-task-queue-write-timer queue)
          (run-at-time
           (max 0 (or e-task-queue-write-delay 0)) nil
           (lambda ()
             (when (e-task-queue-p queue)
               (setf (e-task-queue-write-timer queue) nil)
               (e-task-queue--write-now queue)))))))

(defun e-task-queue-flush (queue)
  "Flush any pending durable write for QUEUE synchronously.  Return QUEUE."
  (when-let ((timer (e-task-queue-write-timer queue)))
    (cancel-timer timer)
    (setf (e-task-queue-write-timer queue) nil))
  (when (e-task-queue-directory queue)
    (e-task-queue--write-now queue))
  queue)

(defun e-task-queue--persist-on-change (queue)
  "Persist QUEUE on a change event.
Bound to `e-task-queue-change-functions' so durable queues write off every
task mutation without the core knowing about disk on its hot path."
  (when (e-task-queue-directory queue)
    (e-task-queue--schedule-write queue)))

(add-hook 'e-task-queue-change-functions #'e-task-queue--persist-on-change)

(defun e-task-queue--load-record (durable)
  "Return a live internal record from a DURABLE persisted record.
A task that was `running' at shutdown could not have survived its turn, so it
is normalized to `queued' for a best-effort re-run."
  (let ((record (copy-sequence durable)))
    (when (eq (plist-get record :status) 'running)
      (setq record (plist-put record :status 'queued))
      (setq record (plist-put record :started-at nil))
      (setq record (plist-put record :finished-at nil)))
    (setq record (plist-put record :handle nil))
    (setq record (plist-put record :pausing nil))
    record))

(defun e-task-queue-load (queue)
  "Load QUEUE's persisted records from disk and re-dispatch.  Return QUEUE.
A `running' record loads as `queued'; `paused', terminal, and `queued' states
load unchanged.  A queue with no directory or no records file is left empty."
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
        (let ((record (e-task-queue--load-record durable)))
          (puthash (plist-get record :task-id) record
                   (e-task-queue-records queue))))
      (e-task-queue--notify queue)
      (e-task-queue--dispatch queue)))
  queue)

(provide 'e-task-queue)

;;; e-task-queue.el ends here

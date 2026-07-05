;;; e-work.el --- Uniform non-blocking work substrate for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A small lifecycle and carrier substrate for work that may take time.  The
;; important contract is API-shaped: callers start work and receive a handle.
;; Waiting is explicit batch/test behavior, not an interactive default.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-backend)
(require 'url)
(require 'e-request)

(declare-function e-task-queue-enqueue "e-task-queue")

(define-error 'e-work-error "e work error")
(define-error 'e-work-invalid-spec "Invalid e work spec" 'e-work-error)
(define-error 'e-work-unsupported-execution "Unsupported e work execution carrier" 'e-work-error)
(define-error 'e-work-await-not-allowed "e work batch await requires an explicit batch/test scope" 'e-work-error)
(define-error 'e-work-await-in-hot-path "e work batch await rejected in interactive hot path" 'e-work-error)
(define-error 'e-work-await-timeout "e work batch await timed out" 'e-work-error)
(define-error 'e-work-cancelled "e work was cancelled" 'e-work-error)
(define-error 'e-work-deadline-exceeded "e work deadline exceeded" 'e-work-error)
(define-error 'e-work-process-failed "e work process failed" 'e-work-error)
(define-error 'e-work-url-failed "e work URL request failed" 'e-work-error)

(defconst e-work-execution-carriers
  '(cheap process url cooperative render agent-task backend)
  "Built-in work execution carriers.")

(defconst e-work-interactive-policies
  '(cheap async batch-only)
  "Known interactive policies for work specs.")

(defvar e-work--sequence 0
  "Monotonic fallback sequence for work handles.")

(defvar e-work--batch-await-allowed nil
  "Non-nil while explicit batch/test code may block on work handles.")

(defmacro e-work-with-batch-await (&rest body)
  "Run BODY in an explicit batch/test scope that may await work handles."
  (declare (indent 0) (debug t))
  `(let ((e-work--batch-await-allowed t))
     ,@body))

(cl-defstruct (e-work-spec
               (:constructor e-work-spec--create)
               (:conc-name e-work-spec-))
  id
  description
  parameters
  execution
  interactive-policy
  owner
  metadata
  concurrency
  coalesce-key
  result-shaper
  setup
  runner
  command
  url
  backend
  messages
  options
  request-handler
  item-handler
  timeout
  deadline
  task-queue
  prompt
  summary)

(cl-defstruct (e-work-handle
               (:constructor e-work-handle--create)
               (:conc-name e-work-handle-))
  id
  spec
  lifecycle
  metadata
  callbacks
  cancel-function
  cleanup-function
  result
  error)

(defun e-work--next-id (spec)
  "Return a fresh work id for SPEC."
  (format "%s/%d"
          (or (e-work-spec-id spec) "work")
          (cl-incf e-work--sequence)))

(defun e-work--validate-spec (spec)
  "Signal unless SPEC declares the explicit work policy contract."
  (unless (e-work-spec-p spec)
    (signal 'wrong-type-argument (list 'e-work-spec-p spec)))
  (unless (e-work-spec-execution spec)
    (signal 'e-work-invalid-spec
            (list "Work spec requires :execution" spec)))
  (unless (memq (e-work-spec-execution spec) e-work-execution-carriers)
    (signal 'e-work-unsupported-execution
            (list (e-work-spec-execution spec))))
  (unless (e-work-spec-interactive-policy spec)
    (signal 'e-work-invalid-spec
            (list "Work spec requires :interactive-policy" spec)))
  (unless (memq (e-work-spec-interactive-policy spec)
                e-work-interactive-policies)
    (signal 'e-work-invalid-spec
            (list "Unsupported interactive policy"
                  (e-work-spec-interactive-policy spec))))
  spec)

(cl-defun e-work-spec-create
    (&rest args
           &key id description parameters execution interactive-policy owner
           metadata concurrency coalesce-key result-shaper setup runner command
           url backend messages options request-handler item-handler timeout
           deadline task-queue prompt summary
           &allow-other-keys)
  "Create a validated work spec.
Every spec must declare explicit :execution and :interactive-policy values."
  (ignore id description parameters execution interactive-policy owner
          metadata concurrency coalesce-key result-shaper setup runner command
          url backend messages options request-handler item-handler timeout
          deadline task-queue prompt summary)
  (e-work--validate-spec (apply #'e-work-spec--create args)))

(put 'e-work-spec-create 'compiler-macro nil)

(defun e-work--call (value arguments context)
  "Resolve VALUE with ARGUMENTS and CONTEXT when it is a function."
  (if (functionp value)
      (funcall value arguments context)
    value))

(defun e-work--shape-result (spec raw arguments context)
  "Return RAW shaped through SPEC's result shaper when present."
  (if-let ((shaper (e-work-spec-result-shaper spec)))
      (funcall shaper raw arguments context)
    raw))

(defun e-work--callback (handle key &rest args)
  "Call HANDLE callback KEY with ARGS when present."
  (when-let ((callback (plist-get (e-work-handle-callbacks handle) key)))
    (apply callback args)))

(defun e-work--cleanup (handle)
  "Run HANDLE cleanup exactly once."
  (when-let ((cleanup (e-work-handle-cleanup-function handle)))
    (setf (e-work-handle-cleanup-function handle) nil)
    (funcall cleanup handle)))

(defun e-work--add-cleanup (handle cleanup)
  "Add CLEANUP to HANDLE's terminal cleanup chain."
  (when cleanup
    (let ((previous (e-work-handle-cleanup-function handle)))
      (setf (e-work-handle-cleanup-function handle)
            (if previous
                (lambda (current-handle)
                  (funcall cleanup current-handle)
	                  (funcall previous current-handle))
	              cleanup)))))

(defun e-work--remember-cancel-error (handle err)
  "Record underlying cancellation ERR on HANDLE metadata."
  (setf (e-work-handle-metadata handle)
        (append (e-work-handle-metadata handle)
                (list :cancel-error err)))
  err)

(defun e-work--cancel-underlying (handle)
  "Cancel HANDLE's underlying carrier and return any cancellation error."
  (when-let ((cancel (e-work-handle-cancel-function handle)))
    (condition-case err
        (progn
          (funcall cancel handle)
          nil)
      (error
       (e-work--remember-cancel-error handle err)))))

(defun e-work--valid-deadline-p (deadline)
  "Return non-nil when DEADLINE is a valid absolute timestamp."
  (or (null deadline)
      (and (numberp deadline)
           (not (< deadline 0)))))

(defun e-work--effective-deadline (spec arguments context)
  "Return SPEC's effective absolute deadline for ARGUMENTS and CONTEXT."
  (let ((deadlines
         (delq nil
               (list
                (e-work--call (e-work-spec-deadline spec) arguments context)
                (plist-get context :deadline)))))
    (dolist (deadline deadlines)
      (unless (e-work--valid-deadline-p deadline)
        (signal 'e-work-invalid-spec
                (list "Work deadline must be an absolute float-time timestamp"
                      deadline))))
    (when deadlines
      (apply #'min deadlines))))

(defun e-work--deadline-condition (handle deadline)
  "Return a visible timeout condition for HANDLE and DEADLINE."
  (let* ((spec (e-work-handle-spec handle))
         (now (float-time))
         (details (list :deadline deadline
                        :now now
                        :overdue-seconds (max 0.0 (- now deadline))
                        :work-id (e-work-handle-id handle)
                        :spec-id (e-work-spec-id spec)
                        :execution (e-work-spec-execution spec))))
    (when-let ((cancel-error (plist-get (e-work-handle-metadata handle)
                                        :cancel-error)))
      (plist-put details :cancel-error cancel-error))
    (list 'e-work-deadline-exceeded
          (format "Work %s exceeded its deadline" (e-work-handle-id handle))
          details)))

(defun e-work--install-deadline (handle arguments context)
  "Install HANDLE's effective deadline timer for ARGUMENTS and CONTEXT."
  (when-let ((deadline
              (e-work--effective-deadline
               (e-work-handle-spec handle) arguments context)))
    (let ((timer nil))
      (setf (e-work-handle-metadata handle)
            (append (e-work-handle-metadata handle)
                    (list :deadline deadline)))
      (setq timer
            (run-at-time
             (max 0 (- deadline (float-time))) nil
             (lambda ()
               (unless (e-request-terminal-p (e-work-handle-lifecycle handle))
                 (e-work--cancel-underlying handle)
                 (e-work-fail
                  handle
                  (e-work--deadline-condition handle deadline))))))
      (e-work--add-cleanup
       handle
       (lambda (_handle)
         (when (timerp timer)
           (cancel-timer timer)))))))

(defun e-work-add-cleanup (handle cleanup)
  "Add CLEANUP to HANDLE's terminal cleanup chain."
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (e-work--add-cleanup handle cleanup)
  handle)

(defun e-work--terminal-event (handle state payload)
  "Emit terminal STATE for HANDLE with PAYLOAD."
  (e-work--cleanup handle)
  (e-work--callback handle :on-event state payload))

(defun e-work-progress (handle payload)
  "Record progress PAYLOAD for HANDLE."
  (when (and (e-work-handle-p handle)
             (e-request-progress (e-work-handle-lifecycle handle) payload))
    (e-work--callback handle :on-progress payload)
    (e-work--callback handle :on-event 'progress payload)
    handle))

(defun e-work-finish (handle payload)
  "Finish HANDLE with PAYLOAD, ignoring stale late callbacks."
  (when (and (e-work-handle-p handle)
             (e-request-finish (e-work-handle-lifecycle handle) payload))
    (setf (e-work-handle-result handle) payload)
    (e-work--terminal-event handle 'finished payload)
    (e-work--callback handle :on-done payload)
    handle))

(defun e-work-fail (handle condition)
  "Fail HANDLE with CONDITION, ignoring stale late callbacks."
  (when (and (e-work-handle-p handle)
             (e-request-fail (e-work-handle-lifecycle handle) condition))
    (setf (e-work-handle-error handle) condition)
    (e-work--terminal-event handle 'failed condition)
    (e-work--callback handle :on-error condition)
    handle))

(defun e-work-cancel (handle)
  "Cancel HANDLE and its underlying carrier, if any."
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (unless (e-request-terminal-p (e-work-handle-lifecycle handle))
    (let* ((cancel-error (e-work--cancel-underlying handle))
           (payload (if cancel-error
                        (list :status 'cancelled
                              :cancel-error cancel-error)
                      '(:status cancelled))))
      (when (e-request-cancel (e-work-handle-lifecycle handle) payload)
        (setf (e-work-handle-error handle) payload)
        (e-work--terminal-event handle 'cancelled payload))))
  handle)

(defun e-work-status (handle)
  "Return a stable status plist for HANDLE."
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (let ((lifecycle (e-work-handle-lifecycle handle)))
    (list :id (e-work-handle-id handle)
          :spec-id (e-work-spec-id (e-work-handle-spec handle))
          :state (e-request-lifecycle-state lifecycle)
          :progress (e-request-lifecycle-progress lifecycle)
          :result (e-work-handle-result handle)
          :error (e-work-handle-error handle)
          :metadata (e-work-handle-metadata handle))))

(cl-defun e-work-await-batch (handle &key timeout)
  "Wait for HANDLE in batch/test code and return its result.
This function is rejected from interactive hot paths."
  (unless e-work--batch-await-allowed
    (signal 'e-work-await-not-allowed
            (list "Wrap batch/test waits in e-work-with-batch-await")))
  (when (e-request-hot-path-active-p)
    (signal 'e-work-await-in-hot-path
            (list "Use callbacks/status in interactive code")))
  (let ((deadline (and timeout (+ (float-time) timeout))))
    (while (not (e-request-terminal-p (e-work-handle-lifecycle handle)))
      (when (and deadline (> (float-time) deadline))
        (signal 'e-work-await-timeout (list handle timeout)))
      (accept-process-output nil 0.01)))
  (pcase (e-request-lifecycle-state (e-work-handle-lifecycle handle))
    ('finished (e-work-handle-result handle))
    ('failed (signal (car (e-work-handle-error handle))
                     (cdr (e-work-handle-error handle))))
    ('cancelled (signal 'e-work-cancelled (list handle)))
    (state (signal 'e-work-error (list "Unexpected terminal state" state)))))

(defun e-work--setup (handle arguments context)
  "Run HANDLE setup and install cleanup/metadata."
  (when-let ((setup (e-work-spec-setup (e-work-handle-spec handle))))
    (let ((state (funcall setup arguments context)))
      (when (plist-get state :cleanup)
        (e-work--add-cleanup handle (plist-get state :cleanup)))
      (when (plist-get state :metadata)
        (setf (e-work-handle-metadata handle)
              (append (e-work-handle-metadata handle)
                      (plist-get state :metadata)))))))

(defun e-work--start-cheap (handle arguments context)
  "Start HANDLE on the cheap inline carrier."
  (let ((runner (e-work-spec-runner (e-work-handle-spec handle))))
    (unless (functionp runner)
      (signal 'e-work-invalid-spec (list "Cheap work requires :runner")))
    (e-work-finish
     handle
     (e-work--shape-result
      (e-work-handle-spec handle)
      (funcall runner arguments context)
      arguments context))))

(defun e-work--process-command-value (spec arguments context)
  "Return process command plist for SPEC."
  (let ((command (e-work--call (e-work-spec-command spec) arguments context)))
    (cond
     ((plist-get command :immediate)
      command)
     ((and (consp command) (stringp (car command)))
      (list :program (car command) :args (cdr command)))
     ((plist-get command :program)
      command)
     (t
      (signal 'e-work-invalid-spec
              (list "Process work requires :command returning a command"))))))

(defun e-work--buffer-string (buffer)
  "Return BUFFER contents, or an empty string when BUFFER is not live."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun e-work--start-process (handle arguments context)
  "Start HANDLE on the process carrier."
  (let* ((spec (e-work-handle-spec handle))
         (command (e-work--process-command-value spec arguments context))
         (immediate (plist-get command :immediate)))
    (when (plist-get command :metadata)
      (setf (e-work-handle-metadata handle)
            (append (e-work-handle-metadata handle)
                    (plist-get command :metadata))))
    (if immediate
        (e-work-finish
         handle (e-work--shape-result spec immediate arguments context))
      (let* ((program (plist-get command :program))
             (args (plist-get command :args))
             (directory (or (plist-get command :directory) default-directory))
             (ok-statuses (or (plist-get command :ok-statuses) '(0)))
             (name (or (plist-get command :name) "e-work-process"))
             (on-output (plist-get command :on-output))
             (progress (plist-get command :progress))
             (progress-interval (or (plist-get command :progress-interval) 0))
             (timeout (or (plist-get command :timeout)
                          (e-work--call (e-work-spec-timeout spec)
                                        arguments context)))
             (finish-on-nonzero (plist-get command :finish-on-nonzero))
             (finish-on-timeout (plist-get command :finish-on-timeout))
             (capture-output
              (if (plist-member command :capture-output)
                  (plist-get command :capture-output)
                (not on-output)))
             (state (plist-get command :state))
             (stdout (generate-new-buffer " *e-work-process-stdout*"))
             (stderr (generate-new-buffer " *e-work-process-stderr*"))
             process
             progress-timer
             timeout-timer
             last-progress-time
             progress-pending)
        (unless (stringp program)
          (signal 'e-work-invalid-spec
                  (list "Process work requires a string :program")))
        (cl-labels
            ((cleanup (_handle)
               (when (timerp progress-timer)
                 (cancel-timer progress-timer))
               (when (timerp timeout-timer)
                 (cancel-timer timeout-timer))
               (when (buffer-live-p stdout)
                 (kill-buffer stdout))
               (when (buffer-live-p stderr)
                 (kill-buffer stderr)))
             (terminal-p ()
               (e-request-terminal-p (e-work-handle-lifecycle handle)))
             (ok-status-p (status)
               (or (eq ok-statuses t)
                   (member status ok-statuses)))
             (insert-output (chunk)
               (when (and capture-output (buffer-live-p stdout))
                 (with-current-buffer stdout
                   (insert chunk))))
             (progress-payload ()
               (when progress
                 (funcall progress handle process state)))
             (emit-progress ()
               (when-let ((payload (progress-payload)))
                 (when (timerp progress-timer)
                   (cancel-timer progress-timer))
                 (setq progress-timer nil)
                 (setq progress-pending nil)
                 (setq last-progress-time (float-time))
                 (e-work-progress handle payload)))
             (request-progress ()
               (when progress
                 (setq progress-pending t)
                 (let* ((interval (max 0 progress-interval))
                        (now (float-time))
                        (elapsed (and last-progress-time
                                      (- now last-progress-time))))
                   (cond
                    ((or (zerop interval)
                         (not last-progress-time)
                         (and elapsed (>= elapsed interval)))
                     (emit-progress))
                    ((not (timerp progress-timer))
                     (setq progress-timer
                           (run-at-time
                            (max 0 (- interval (or elapsed 0)))
                            nil
                            (lambda ()
                              (setq progress-timer nil)
                              (when (and progress-pending
                                         (not (terminal-p)))
                                (emit-progress))))))))))
             (raw-result (status reason &optional suffix exit-code)
               (let ((stdout-text (e-work--buffer-string stdout))
                     (stderr-text (e-work--buffer-string stderr)))
                 (list :status status
                       :reason reason
                       :exit-code exit-code
                       :stdout stdout-text
                       :stderr stderr-text
                       :lines (split-string stdout-text "\n" t)
                       :process process
                       :command (cons program args)
                       :state state
                       :suffix suffix)))
             (finish-with-raw (raw)
               (condition-case err
                   (e-work-finish
                    handle
                    (e-work--shape-result spec raw arguments context))
                 (error
                  (e-work-fail handle err))))
             (finish-process ()
               (unless (terminal-p)
                 (when progress
                   (emit-progress))
                 (let* ((status (process-exit-status process))
                        (tool-status (if (ok-status-p status) 'ok 'error))
                        (stderr-text (e-work--buffer-string stderr)))
                   (cond
                    ((ok-status-p status)
                     (finish-with-raw
                      (raw-result tool-status 'exit nil status)))
                    (finish-on-nonzero
                     (finish-with-raw
                      (raw-result
                       tool-status
                       'exit
                       (format "%s exited with status %s" program status)
                       status)))
                    (t
                     (e-work-fail
                      handle
                      (list 'e-work-process-failed
                            (format "%s failed with exit status %s: %s"
                                    program status
                                    (string-trim stderr-text)))))))))
             (finish-signal ()
               (unless (terminal-p)
                 (when progress
                   (emit-progress))
                 (if finish-on-nonzero
                     (finish-with-raw
                      (raw-result
                       'error
                       'signal
                       (format "%s was interrupted" program)
                       (process-exit-status process)))
                   (e-work-fail
                    handle
                    (list 'e-work-process-failed
                          (format "%s was interrupted" program))))))
             (finish-timeout ()
               (unless (terminal-p)
                 (when progress
                   (emit-progress))
                 (let ((suffix (or (plist-get command :timeout-message)
                                   (format "%s timed out after %s seconds"
                                           program timeout))))
                   (if finish-on-timeout
                       (finish-with-raw
                        (raw-result 'error 'timeout suffix nil))
                     (e-work-fail
                      handle
                      (list 'e-work-process-failed suffix))))
                 (when (and process (process-live-p process))
                   (kill-process process))))
             (record-output (_proc chunk)
               (unless (terminal-p)
                 (condition-case err
                     (progn
                       (insert-output chunk)
                       (when on-output
                         (funcall on-output handle process chunk state))
                       (request-progress))
                   (error
                    (when (and process (process-live-p process))
                      (kill-process process))
                    (e-work-fail handle err))))))
          (e-work--add-cleanup handle #'cleanup)
          (setf (e-work-handle-cancel-function handle)
                (lambda (_handle)
                  (when (and process (process-live-p process))
                    (kill-process process))))
          (let ((started nil))
            (unwind-protect
                (let ((default-directory directory))
                  (setq process
                        (make-process
                         :name name
                         :buffer stdout
                         :stderr stderr
                         :command (cons program args)
                         :connection-type
                         (or (plist-get command :connection-type) 'pipe)
                         :coding (or (plist-get command :coding)
                                     'utf-8-unix)
                         :noquery t
                         :filter (when (or on-output progress)
                                   #'record-output)
                         :sentinel
                         (lambda (proc _event)
                           (when (and (eq proc process)
                                      (memq (process-status proc)
                                            '(exit signal)))
                             (if (eq (process-status proc) 'signal)
                                 (finish-signal)
                               (finish-process)))))))
                  (set-process-query-on-exit-flag process nil)
                  (setf (e-work-handle-metadata handle)
                        (append (e-work-handle-metadata handle)
                                (list :process process
                                      :transport 'process)))
                  (when timeout
                    (setq timeout-timer
                          (run-at-time timeout nil #'finish-timeout)))
                  (setq started t)
                  handle)
              (unless started
                (e-work--cleanup handle))))))))

(defun e-work--start-url (handle arguments context)
  "Start HANDLE on the URL carrier."
  (let* ((spec (e-work-handle-spec handle))
         (url (e-work--call (e-work-spec-url spec) arguments context))
         (timeout (or (e-work--call (e-work-spec-timeout spec)
                                    arguments context)
                      20))
         timer
         response-buffer)
    (unless (stringp url)
      (signal 'e-work-invalid-spec
              (list "URL work requires :url resolving to a string")))
    (cl-labels
        ((cleanup (_handle)
           (when (timerp timer)
             (cancel-timer timer))
           (when (buffer-live-p response-buffer)
             (kill-buffer response-buffer))))
      (e-work--add-cleanup handle #'cleanup)
      (setf (e-work-handle-cancel-function handle)
            (lambda (_handle)
              (when (buffer-live-p response-buffer)
                (when-let ((process (get-buffer-process response-buffer)))
                  (when (process-live-p process)
                    (kill-process process)))))))
      (setf (e-work-handle-metadata handle)
            (append (e-work-handle-metadata handle)
                    (list :transport 'url :url url)))
      (e-work-progress handle (list :message (format "Fetching %s" url)))
      (setq timer
            (run-at-time
             timeout nil
             (lambda ()
               (e-work-fail
                handle
                (list 'e-work-url-failed
                      (format "URL request timed out after %s seconds"
                              timeout))))))
      (setq response-buffer
            (url-retrieve
             url
             (lambda (status)
               (setq response-buffer (current-buffer))
               (if-let ((err (plist-get status :error)))
                   (e-work-fail handle (if (consp err)
                                           err
                                         (list 'e-work-url-failed err)))
                 (condition-case condition
                     (e-work-finish
                      handle
                      (e-work--shape-result
                       spec
                       (list :url url
                             :status status
                             :buffer (current-buffer))
                       arguments context))
                   (error
                    (e-work-fail handle condition)))))
             nil
             t
             nil))
      handle))

(defun e-work--start-timer-runner (handle arguments context)
  "Start HANDLE on a cooperative timer carrier."
  (let* ((spec (e-work-handle-spec handle))
         (runner (e-work-spec-runner spec))
         (delay (or (plist-get arguments :delay) 0))
         timer)
    (unless (functionp runner)
      (signal 'e-work-invalid-spec
              (list "Timer work requires :runner")))
    (setf (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (when (timerp timer)
              (cancel-timer timer))))
    (setq timer
          (run-at-time
           delay nil
           (lambda ()
             (condition-case err
                 (e-work-finish
                  handle
                  (e-work--shape-result
                   spec
                   (funcall runner arguments context)
                   arguments context))
               (error
                (e-work-fail handle err))))))
	    (setf (e-work-handle-metadata handle)
	          (append (e-work-handle-metadata handle)
	                  (list :transport (e-work-spec-execution spec)
	                        :timer timer)))
	    handle))

(defun e-work--start-cooperative (handle arguments context)
  "Start HANDLE on the cooperative self-settling carrier."
  (let ((runner (e-work-spec-runner (e-work-handle-spec handle))))
    (unless (functionp runner)
      (signal 'e-work-invalid-spec
              (list "Cooperative work requires :runner")))
    (setf (e-work-handle-metadata handle)
          (append (e-work-handle-metadata handle)
                  '(:transport cooperative)))
    (let ((result (funcall runner handle arguments context)))
      (unless (eq result :deferred)
        (e-work-finish
         handle
         (e-work--shape-result
          (e-work-handle-spec handle)
          result
          arguments context))))
    handle))

(defun e-work--start-backend (handle arguments context)
  "Start HANDLE on the backend carrier."
  (let* ((spec (e-work-handle-spec handle))
         (backend (e-work--call (e-work-spec-backend spec) arguments context))
         (messages (e-work--call (e-work-spec-messages spec) arguments context))
         (options (e-work--call (e-work-spec-options spec) arguments context))
         (request-handler (e-work-spec-request-handler spec))
         (item-handler (e-work-spec-item-handler spec))
         backend-request)
    (unless (e-backend-p backend)
      (signal 'e-work-invalid-spec
              (list "Backend work requires :backend resolving to an e-backend")))
    (cl-labels
        ((terminal-p ()
           (e-request-terminal-p (e-work-handle-lifecycle handle)))
         (remember-request
          (request)
          (when (and request (not (eq request backend-request)))
            (setq backend-request request)
            (setf (e-work-handle-metadata handle)
                  (append (e-work-handle-metadata handle)
                          (list :backend-request request
                                :backend-request-metadata
                                (and (e-backend-request-p request)
                                     (e-backend-request-metadata request)))))
            (when request-handler
              (condition-case err
                  (funcall request-handler handle request arguments context)
                (error
                 (fail err))))))
         (cancel-backend ()
          (when (and backend-request
                     (e-backend-request-p backend-request))
            (e-backend-cancel-request backend-request)))
         (cancel-backend-best-effort ()
          (condition-case err
              (cancel-backend)
            (error
             (e-work--remember-cancel-error handle err))))
         (fail (err)
          (unless (terminal-p)
            (cancel-backend-best-effort)
            (e-work-fail handle err))))
      (setf (e-work-handle-cancel-function handle)
            (lambda (_handle)
              (cancel-backend)
              t))
      (setf (e-work-handle-metadata handle)
            (append (e-work-handle-metadata handle)
                    (list :transport 'backend
                          :backend (e-backend--name backend))))
      (remember-request
       (e-backend-start
        backend
        :messages messages
        :options options
        :on-request-start #'remember-request
        :on-item
        (lambda (item)
          (unless (terminal-p)
            (condition-case err
                (progn
                  (when item-handler
                    (funcall item-handler handle item arguments context))
                  (e-work-progress handle (list :item item)))
              (error
               (fail err)))))
        :on-done
        (lambda (result)
          (unless (terminal-p)
            (condition-case err
                (e-work-finish
                 handle
                 (e-work--shape-result spec result arguments context))
              (error
               (fail err)))))
        :on-error #'fail))
      handle)))

(defun e-work--start-agent-task (handle arguments context)
  "Start HANDLE on the agent task queue carrier."
  (let* ((spec (e-work-handle-spec handle))
         (queue (e-work--call (e-work-spec-task-queue spec) arguments context))
         (prompt (e-work--call (e-work-spec-prompt spec) arguments context))
         (summary (e-work--call (e-work-spec-summary spec) arguments context))
         (metadata (plist-get arguments :metadata))
         (instance-id (plist-get arguments :harness-instance-id)))
    (unless (fboundp 'e-task-queue-enqueue)
      (signal 'e-work-invalid-spec
              (list "Agent-task work requires e-task-queue-enqueue")))
    (e-work-finish
     handle
     (e-work--shape-result
      spec
      (e-task-queue-enqueue queue
                            :prompt prompt
                            :summary summary
                            :metadata metadata
                            :harness-instance-id instance-id)
      arguments context))))

(cl-defun e-work-start
    (spec arguments &key context on-done on-error on-progress on-event)
  "Start SPEC with ARGUMENTS and return an `e-work-handle'.
The handle is returned for every carrier.  Cheap work may finish before this
function returns, but still records the same lifecycle."
  (setq spec (e-work--validate-spec spec))
  (let* ((id (e-work--next-id spec))
         (metadata (copy-sequence (e-work--call (e-work-spec-metadata spec)
                                                arguments context)))
         (lifecycle
          (e-request-lifecycle-create
           :id id
           :owner (e-work-spec-owner spec)
           :parent-id (plist-get context :turn-id)))
         (handle
          (e-work-handle--create
           :id id
           :spec spec
           :lifecycle lifecycle
           :metadata metadata
           :callbacks (list :on-done on-done
                            :on-error on-error
                            :on-progress on-progress
                            :on-event on-event))))
    (condition-case err
        (progn
          (e-request-start lifecycle
                           (list :execution (e-work-spec-execution spec)
                                 :interactive-policy
                                 (e-work-spec-interactive-policy spec)))
          (e-work--install-deadline handle arguments context)
          (e-work--setup handle arguments context)
          (pcase (e-work-spec-execution spec)
            ('cheap (e-work--start-cheap handle arguments context))
            ('process (e-work--start-process handle arguments context))
            ('url (e-work--start-url handle arguments context))
            ('cooperative (e-work--start-cooperative handle arguments context))
            ('render (e-work--start-timer-runner handle arguments context))
            ('backend (e-work--start-backend handle arguments context))
            ('agent-task (e-work--start-agent-task handle arguments context))
            (_ (signal 'e-work-unsupported-execution
                       (list (e-work-spec-execution spec)))))
          handle)
      (error
       (e-work-fail handle err)
       handle))))

(provide 'e-work)

;;; e-work.el ends here

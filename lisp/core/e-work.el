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

(defun e-work-error-message (err)
  "Return `error-message-string' for ERR with the printer bounded.
`error-message-string' prints the condition's data with the caller's print
settings.  A condition whose data is cyclic or huge (harness state, a buffer, a
process object) then makes the printer loop without end, spinning Emacs at 100%
CPU while memory climbs until it crashes.  Bind the printer to detect cycles and
cap size so formatting a hostile error always terminates."
  (let ((print-circle t)
        (print-length 100)
        (print-level 8))
    (error-message-string err)))

(defun e-format-safe (format-string &rest args)
  "Like `format' with FORMAT-STRING and ARGS, but with the printer bounded.
`format' renders a `%S' (or `%s' on a non-string) by calling the Lisp printer
with the caller's print settings.  When an argument is cyclic or huge -- a
harness struct, a streaming event, a buffer or process object, a backend error
carrying that state -- the printer loops without end, spinning Emacs at 100%
CPU while memory climbs until the process is killed.  This is the same failure
`e-work-error-message' guards for errors; use this for any async or timer
callback that formats an arbitrary value for display or logging.  Bind the
printer to detect cycles and cap size so formatting always terminates.

Do NOT use this where the printed text is an identity or is read back
\(`secure-hash' keys, `prin1' to a file, saved state): the length and level
caps truncate and would corrupt the result."
  (let ((print-circle t)
        (print-length 100)
        (print-level 8))
    (apply #'format format-string args)))

(defun e-prin1-safe (value)
  "Return `prin1-to-string' of VALUE with the printer bounded.
See `e-format-safe' for why and when: a cyclic or huge VALUE otherwise makes
the printer spin forever.  Never use this for a hash input, on-disk `prin1', or
any text that must read back -- the caps truncate."
  (let ((print-circle t)
        (print-length 100)
        (print-level 8))
    (prin1-to-string value)))

(defun e-kill-buffer-quietly (buffer)
  "Kill BUFFER without ever prompting about a running process.
BUFFER is a transient HTTP/`url-retrieve' or subprocess buffer that e owns.
Emacs's `process-kill-buffer-query-function' asks \"Buffer ... has a running
process; kill it? (y or n)\" whenever `kill-buffer' runs on a buffer whose
process is still live -- an in-flight request connection, say.  In a headless
agent that prompt blocks the turn forever, since nothing answers it.

Tear the process down first with its exit query disabled, then kill with
`kill-buffer-query-functions' bound off.  The two guards are redundant on
purpose: even if the process outlives `delete-process' for a moment, the
prompt can never fire."
  (when (buffer-live-p buffer)
    (when-let ((process (get-buffer-process buffer)))
      (when (process-live-p process)
        (set-process-query-on-exit-flag process nil)
        (delete-process process)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

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

(defun e-work-on-settle (handle callback)
  "Call CALLBACK with HANDLE once HANDLE reaches a terminal state.
When HANDLE is already terminal, call CALLBACK now; otherwise register a
terminal-event subscription that fires exactly once when it settles.  This is
the event-driven, non-blocking counterpart to reading `e-work-status' in a
loop.  CALLBACK runs with HANDLE already in a terminal state, so
`e-work-status' reports the final result or error."
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (if (e-request-terminal-p (e-work-handle-lifecycle handle))
      (funcall callback handle)
    (e-work-add-cleanup handle (lambda (settled-handle)
                                 (funcall callback settled-handle))))
  handle)

(cl-defun e-work-await-set (handles &key (mode 'all) timeout on-settle)
  "Wait for HANDLES event-driven and call ON-SETTLE once when the set settles.
This is the non-blocking, set-oriented sibling of `e-work-await-batch': it never
calls `accept-process-output' and never blocks the thread.  It subscribes to
each handle's terminal event and arms at most one timeout timer.

MODE is `all' (default: settle when every handle is terminal) or `any' (settle
when the first handle is terminal).  TIMEOUT, when non-nil, is seconds after
which the wait settles regardless of the handles.

ON-SETTLE is called exactly once with a plist:
  (:reason complete|timed-out :mode MODE :done DONE :pending PENDING)
where DONE lists the terminal handles and PENDING the non-terminal ones.  A
handle already terminal when this is called counts immediately, so awaiting
finished work settles at once.

Returns a canceller thunk that detaches the wait without invoking ON-SETTLE;
late terminal callbacks then no-op."
  (unless (and (listp handles) handles (cl-every #'e-work-handle-p handles))
    (signal 'wrong-type-argument (list 'e-work-handle-list handles)))
  (unless (memq mode '(all any))
    (signal 'wrong-type-argument (list '(member all any) mode)))
  (let (settled timer)
    (cl-labels
        ((terminal-p (handle)
           (e-request-terminal-p (e-work-handle-lifecycle handle)))
         (finish (reason)
           (unless settled
             (setq settled t)
             (when (timerp timer)
               (cancel-timer timer)
               (setq timer nil))
             (when on-settle
               (funcall on-settle
                        (list :reason reason
                              :mode mode
                              :done (cl-remove-if-not #'terminal-p handles)
                              :pending (cl-remove-if #'terminal-p handles))))))
         (check ()
           (unless settled
             (let ((terminal (cl-count-if #'terminal-p handles)))
               (pcase mode
                 ('any (when (> terminal 0) (finish 'complete)))
                 ('all (when (= terminal (length handles))
                         (finish 'complete))))))))
      (dolist (handle handles)
        (e-work-on-settle handle (lambda (_settled-handle) (check))))
      ;; Only arm the timeout if no already-terminal handle settled the set
      ;; during subscription above.
      (when (and (not settled) timeout)
        (setq timer (run-at-time timeout nil (lambda () (finish 'timed-out)))))
      (lambda ()
        (unless settled
          (setq settled t)
          (when (timerp timer)
            (cancel-timer timer)
            (setq timer nil)))))))

(defvar e-work--detached-handles (make-hash-table :test 'equal)
  "Process-global map of `e-work' handle id to a live detached handle.
Runtime-only and rebuildable: it holds no durable facts.  The race coordinator
inserts a handle here when a raced tool call outlives its `wait_for' window, so
the generic `work:' waitable scheme can resolve it later.  Entries key on the
handle's own id, so nothing tool-specific is stored: a detached bash, glob, or
fetch handle is just an `e-work' that kept running.")

(defun e-work-detach-register (handle)
  "Register HANDLE in the detached-work registry and return it.
Keyed on HANDLE's own id.  A terminal handle stays registered so a later
`await' can still resolve its result; the registry is discarded on restart."
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (puthash (e-work-handle-id handle) handle e-work--detached-handles)
  handle)

(defun e-work-detached-handle (id)
  "Return the live detached-work handle registered under ID, or nil."
  (gethash id e-work--detached-handles))

(defun e-work-detached-handle-ids ()
  "Return the ids of currently registered detached-work handles."
  (hash-table-keys e-work--detached-handles))

(cl-defun e-work-race-or-detach (child &key wait-for on-inline on-detach)
  "Race CHILD against a WAIT-FOR deadline and settle exactly one way.
CHILD is a live `e-work-handle'.  WAIT-FOR is seconds:

  <= 0   detach immediately without arming any timer (pure fire-and-forget),
  > 0    race CHILD against a WAIT-FOR-second timer,
  nil    hold until CHILD is terminal, bounded only by CHILD's own timeout.

When CHILD reaches a terminal state inside the window, ON-INLINE is called with
CHILD.  When the window expires with CHILD still running, ON-DETACH is called
with CHILD.  Exactly one of the two runs.  This is `e-work-await-set' with
`:mode any' over the single CHILD plus a WAIT-FOR timeout, so the wait is
event-driven and never blocks.

Returns a canceller thunk that tears down the race without invoking either
callback, so a cancelled parent can detach the wait before settling CHILD
itself."
  (unless (e-work-handle-p child)
    (signal 'wrong-type-argument (list 'e-work-handle-p child)))
  (cond
   ((and (numberp wait-for) (<= wait-for 0))
    (when on-detach (funcall on-detach child))
    (lambda () nil))
   (t
    (e-work-await-set
     (list child)
     :mode 'any
     :timeout wait-for
     :on-settle
     (lambda (report)
       (pcase (plist-get report :reason)
         ('complete (when on-inline (funcall on-inline child)))
         ('timed-out (when on-detach (funcall on-detach child)))))))))

(defconst e-work-detachable-wait-for-parameter
  '(:wait_for
    (:type "number"
     :description "Seconds to hold the turn before detaching. The work runs inline for up to this long; if it finishes first, its result is returned inline. If the window expires while it is still running, the work detaches and the call returns a `work:<id>' reference to pass to `await', plus an `output_uri' for partial output. 0 detaches immediately (fire-and-forget); omit for the tool's default hold."))
  "JSON Schema property injected into every detachable tool's parameters.
One concept, one parameter: `wait_for' subsumes any foreground/background flag.")

(defun e-work-detachable-merge-parameters (parameters)
  "Return PARAMETERS with the shared `wait_for' property merged in.
PARAMETERS is a tool's JSON Schema object plist.  The `wait_for' property is
declared once here so every detachable tool exposes an identical control."
  (let* ((properties (plist-get parameters :properties))
         (merged (append properties e-work-detachable-wait-for-parameter)))
    (plist-put (copy-sequence parameters) :properties merged)))

(defun e-work-detachable-wait-for (arguments default)
  "Return the effective `wait_for' seconds from ARGUMENTS, or DEFAULT.
A non-numeric `wait_for' is a client error and signals."
  (let ((value (plist-get arguments :wait_for)))
    (cond
     ((null value) default)
     ((numberp value) value)
     (t (signal 'wrong-type-argument (list 'numberp value))))))

(defun e-work--detach-ack (child scheme extra)
  "Return the detach acknowledgment plist for detached CHILD under SCHEME.
EXTRA is a per-tool plist merged after the generic fields.  The reference is
CHILD's own `e-work' id under SCHEME, so `await' and `e-work-cancel' resolve it
with no tool-specific knowledge."
  (append
   (list :reference (format "%s:%s" scheme (e-work-handle-id child))
         :state "running")
   (when-let ((uri (plist-get (e-work-handle-metadata child) :output-uri)))
     (list :output_uri uri))
   extra))

(cl-defun e-work-detachable-spec
    (child-spec &key id description owner default-wait-for
                (reference-scheme "work") ack-extra)
  "Return a cooperative parent spec that races CHILD-SPEC against `wait_for'.
CHILD-SPEC is the tool's underlying work spec (process, url, or cooperative).
The parent reads `wait_for' from its arguments, starts CHILD-SPEC as a
standalone child handle, and delegates to `e-work-race-or-detach':

  inline  finish the parent with CHILD's own terminal result (or failure),
          indistinguishable from running CHILD-SPEC directly;
  detach  register the still-running child and finish with a detach ack.

DEFAULT-WAIT-FOR is the hold applied when a call omits `wait_for'.  ACK-EXTRA,
when non-nil, is called with the tool arguments and returns a plist merged into
the detach acknowledgment (for example a `:command' or `:query' echo).  The
parent never touches the detached-work registry beyond the single insert on the
detach branch, and the child stays ignorant of detachment entirely."
  (e-work-spec-create
   :id (or id (format "%s.detachable" (or (e-work-spec-id child-spec) "work")))
   :description (or description (e-work-spec-description child-spec))
   :execution 'cooperative
   :interactive-policy 'async
   :owner (or owner (e-work-spec-owner child-spec))
   :runner
   (lambda (parent arguments context)
     (let ((wait-for (e-work-detachable-wait-for arguments default-wait-for))
           (race-cancel nil)
           child)
       (setf (e-work-handle-cancel-function parent)
             (lambda (_handle)
               (when race-cancel (funcall race-cancel))
               (when (e-work-handle-p child) (e-work-cancel child))
               t))
       (setq child
             (e-work-start
              child-spec arguments
              :context context
              :on-progress (lambda (payload) (e-work-progress parent payload))))
       ;; Surface the child's early metadata (streaming output uri, transport)
       ;; on the parent so a detach ack and progress reads see it at once.
       (setf (e-work-handle-metadata parent)
             (append (e-work-handle-metadata parent)
                     (e-work-handle-metadata child)))
       (setq race-cancel
             (e-work-race-or-detach
              child
              :wait-for wait-for
              :on-inline
              (lambda (settled-child)
                (pcase (e-request-lifecycle-state
                        (e-work-handle-lifecycle settled-child))
                  ('finished
                   (e-work-finish parent (e-work-handle-result settled-child)))
                  ('failed
                   (e-work-fail parent (e-work-handle-error settled-child)))
                  ('cancelled (e-work-cancel parent))))
              :on-detach
              (lambda (running-child)
                (e-work-detach-register running-child)
                (e-work-finish
                 parent
                 (e-work--detach-ack
                  running-child reference-scheme
                  (and (functionp ack-extra)
                       (funcall ack-extra arguments)))))))
       :deferred))))

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
               (e-kill-buffer-quietly stdout)
               (e-kill-buffer-quietly stderr))
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
           (e-kill-buffer-quietly response-buffer)))
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
               (unless (e-request-terminal-p
                        (e-work-handle-lifecycle handle))
                 (e-work--cancel-underlying handle)
                 (e-work-fail
                  handle
                  (list 'e-work-url-failed
                        (format "URL request timed out after %s seconds"
                                timeout)))))))
      (setq response-buffer
            (url-retrieve
             url
             (lambda (status)
               (let ((buffer (current-buffer)))
                 (if (e-request-terminal-p
                      (e-work-handle-lifecycle handle))
                     (e-kill-buffer-quietly buffer)
                   (setq response-buffer buffer)
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
                                 :buffer buffer)
                           arguments context))
                       (error
                        (e-work-fail handle condition)))))))
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
                   (let ((raw (funcall runner arguments context)))
                     (unless (eq raw :deferred)
                       (e-work-finish
                        handle
                        (e-work--shape-result
                         spec
                         raw
                         arguments context))))
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
          (when (and request
                     (not (terminal-p))
                     (not (eq request backend-request)))
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

(defun e-work--agent-task-instance-id (value)
  "Normalize agent-task harness instance id VALUE from JSON-like arguments."
  (cond
   ((null value) nil)
   ((keywordp value) value)
   ((stringp value)
    (intern (concat ":" (string-remove-prefix ":" value))))
   (t (signal 'wrong-type-argument
              (list 'stringp :harness-instance-id)))))

(defun e-work--start-agent-task (handle arguments context)
  "Start HANDLE on the agent task queue carrier."
  (let* ((spec (e-work-handle-spec handle))
         (queue (e-work--call (e-work-spec-task-queue spec) arguments context))
         (prompt (e-work--call (e-work-spec-prompt spec) arguments context))
         (summary (e-work--call (e-work-spec-summary spec) arguments context))
         (metadata (plist-get arguments :metadata))
         (instance-id (e-work--agent-task-instance-id
                       (plist-get arguments :harness-instance-id))))
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

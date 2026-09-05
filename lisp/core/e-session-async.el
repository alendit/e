;;; e-session-async.el --- Session durable-operation coordinator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The session application service owns ordered durable operations above the
;; DP5A runtime transport.  It keeps session publication out of worker filters:
;; a lane retains the bounded immutable operation until the runtime request has
;; one terminal outcome, then asks the aggregate to apply the acknowledged
;; delta exactly once.  Only aggregate-owned sealed commands enter a lane;
;; caller mutation closures and whole-session projections never do.

;;; Code:

(require 'cl-lib)
(require 'e-session-aggregate)
(require 'e-session-codec)
(require 'e-session-storage)
(require 'e-work)

(define-error 'e-session-async-capacity-exhausted
  "Session durable-operation capacity is exhausted"
  'e-session-storage-error)
(define-error 'e-session-async-reconciliation-required
  "Session requires canonical reconciliation before another mutation"
  'e-session-storage-error)

(defconst e-session-async-lane-operation-capacity 32)
(defconst e-session-async-lane-byte-capacity
  (let* ((spec (cdr (e-session-aggregate-command-family-spec 'create))))
    (+ (plist-get spec :producer-max)
       (e-session-aggregate-command-delta-bytes 'create)
       (e-session-aggregate-command-reference-bytes 'create)))
  "Maximum local P+D+R retained by one session lane.")
(defconst e-session-async-coordinator-operation-capacity 64)
(defconst e-session-async-coordinator-byte-capacity
  (* 2 e-session-async-lane-byte-capacity)
  "Maximum local P+D+R retained by all session lanes.")
(defconst e-session-async-publication-drain-limit 16)
(defconst e-session-async-reconciliation-diagnostic-limit 1024)
(defconst e-session-async-reconciliation-barrier-bytes 2048)
(defconst e-session-async-reconciliation-attempt-limit 3)
(defconst e-session-async-reconciliation-cause-kinds
  '(aggregate-invariant storage-unavailable capacity cancelled quit error))

(defconst e-session-async--completion-actions
  '(apply-after-return token-allocation projection result shared-release
    lane-detach schedule terminal-enqueue)
  "Exhaustive fault-injectable actions in acknowledged completion.")

(defvar e-session-async--completion-fault-function nil
  "Optional test-only function called with completion ACTION and EDGE.")

(cl-defstruct (e-session-async--lane
               (:constructor e-session-async--lane-create))
  session-id queue active reconciliation (count 0) (bytes 0))

(cl-defstruct (e-session-async--coordinator
               (:constructor e-session-async--coordinator-create))
  store lanes (count 0) (bytes 0) publication-outbox publication-timer
  terminal-outbox terminal-outbox-tail terminal-timer terminal-draining
  scheduler-timer (generation 0) closing closed
  (reconciliation-count 0) (reconciliation-bytes 0))

(cl-defstruct (e-session-async--operation
               (:constructor e-session-async--operation-create))
  coordinator lane session-id command delta body bytes work before-submit
  write-index result state generation charged producer-bytes
  frame-escrow composition-bytes family storage-operation
  (apply-attempts 0) first-apply-cause applied completion-phase public-result
  terminal-token)

(cl-defstruct (e-session-async--terminal-token
               (:constructor e-session-async--terminal-token-create))
  coordinator work state payload bytes enqueued)

(defvar e-session-async--coordinators
  (make-hash-table :test 'eq :weakness 'key)
  "Session durable-operation coordinators keyed by session storage owner.")

(defvar e-session-async--enabled-stores
  (make-hash-table :test 'eq :weakness 'key)
  "Persistent stores whose facade uses C07 work handles.")

(defun e-session-async--completion-fault (action edge)
  "Invoke the test fault seam for real completion ACTION at EDGE."
  (unless (memq action e-session-async--completion-actions)
    (signal 'e-session-storage-error
            (list "Unknown session completion action" action)))
  (when e-session-async--completion-fault-function
    (funcall e-session-async--completion-fault-function action edge)))

(defun e-session-async-enable (store)
  "Enable asynchronous session application operations for STORE."
  (puthash store t e-session-async--enabled-stores)
  store)

(defun e-session-async-enabled-p (store)
  "Return non-nil when STORE's facade returns C07 operation handles."
  (gethash store e-session-async--enabled-stores))

(defun e-session-async-pending-p (store session-id)
  "Return non-nil when SESSION-ID has unacknowledged private lane intent.

The sealed commands are strictly coordinator-private: this predicate never
returns one to a session facade or caller."
  (when-let* ((coordinator (gethash store e-session-async--coordinators))
              (lane (gethash session-id (e-session-async--coordinator-lanes coordinator))))
    (and (or (e-session-async--lane-active lane)
             (e-session-async--lane-queue lane))
         t)))

(defun e-session-async-reconciliation-required-p (store session-id)
  "Return non-nil when SESSION-ID is blocked pending canonical reconciliation."
  (when-let* ((coordinator (gethash store e-session-async--coordinators))
              (lane (gethash session-id
                             (e-session-async--coordinator-lanes coordinator))))
    (and (e-session-async--lane-reconciliation lane) t)))

(defconst e-session-async--operation-spec
  (e-work-spec-create
   :id "session-durable" :execution 'cooperative :interactive-policy 'async
   :owner 'e-session-async
   :runner (lambda (_handle operation _context)
             (e-session-async--start-operation operation)
             :deferred)))

(defun e-session-async-coordinator (store)
  "Return STORE's one session durable-operation coordinator.

Persistent session stores share the injected DP5A runtime.  Ephemeral stores
have no coordinator because they have no durable-before-visible boundary."
  (or (gethash store e-session-async--coordinators)
      (progn
        (unless (e-session-storage-runtime-store store)
          (signal 'e-session-storage-error
                  (list "Persistent session runtime is unavailable" store)))
        (let ((coordinator
               (e-session-async--coordinator-create
                :store store :lanes (make-hash-table :test 'equal))))
          (puthash store coordinator e-session-async--coordinators)
          coordinator))))

(defun e-session-async--lane (coordinator session-id)
  "Return COORDINATOR's FIFO lane for SESSION-ID."
  (or (gethash session-id (e-session-async--coordinator-lanes coordinator))
      (let ((lane (e-session-async--lane-create :session-id session-id)))
        (puthash session-id lane (e-session-async--coordinator-lanes coordinator))
        lane)))

(defun e-session-async--reconciliation-error (lane)
  "Return LANE's bounded persistent reconciliation failure."
  (let ((state (e-session-async--lane-reconciliation lane)))
    (list 'e-session-async-reconciliation-required
          "Committed session state requires canonical reconciliation"
          :session-id (e-session-async--lane-session-id lane)
          :cause-kind (plist-get state :cause-kind)
          :diagnostic (plist-get state :diagnostic)
          :request-id (plist-get state :request-id)
          :delta-id (plist-get state :delta-id)
          :revision (plist-get state :revision)
          :operation (plist-get state :operation))))

(defun e-session-async--utf8-prefix (string byte-limit)
  "Return STRING's longest prefix occupying at most BYTE-LIMIT UTF-8 bytes."
  ;; A UTF-8 byte is never shorter than one character, so no candidate beyond
  ;; BYTE-LIMIT characters can fit.  Keeping HIGH there also bounds every
  ;; temporary substring made by this search.
  (let ((low 0) (high (min (length string) byte-limit)))
    (while (< low high)
      (let ((middle (ceiling (+ low high) 2)))
        (if (<= (string-bytes (substring string 0 middle)) byte-limit)
            (setq low middle)
          (setq high (1- middle)))))
    (substring string 0 low)))

(defun e-session-async--reconciliation-cause-kind (cause)
  "Map arbitrary first CAUSE to the finite reconciliation cause enum."
  (let ((type (car-safe cause)))
    (cond
     ((memq type '(e-session-error e-session-missing e-session-duplicate))
      'aggregate-invariant)
     ((memq type '(e-session-storage-error e-runtime-store-unavailable))
      'storage-unavailable)
     ((memq type '(e-session-async-capacity-exhausted
                   e-runtime-store-capacity-exhausted)) 'capacity)
     ((memq type '(e-runtime-store-cancelled e-work-cancelled)) 'cancelled)
     ((eq type 'quit) 'quit)
     (t 'error))))

(defun e-session-async--reconciliation-shape (operation cause)
  "Return OPERATION's exact finite exhausted-reconciliation proof."
  (let* ((command (e-session-async--operation-command operation))
         (result (e-session-async--operation-result operation))
         ;; Never render the full condition: arbitrary error data may retain a
         ;; producer leaf.  The conventional first string is sufficient for a
         ;; finite diagnostic, otherwise the finite cause kind is explicit.
         (message (if (stringp (cadr cause))
                      (cadr cause)
                    "Authoritative publication failed")))
    (list :session-id (e-session-async--operation-session-id operation)
          :request-id (e-session-aggregate-command-request-id command)
          :delta-id (e-session-aggregate-command-delta-id command)
          :revision (and (listp result) (plist-get result :revision))
          :operation (e-session-aggregate-command-tag command)
          :cause-kind (e-session-async--reconciliation-cause-kind cause)
          :diagnostic
          (e-session-async--utf8-prefix
           message e-session-async-reconciliation-diagnostic-limit)
          :bytes e-session-async-reconciliation-barrier-bytes
          :charged t)))

(defun e-session-async-reconciliation-match-p
    (store session-id request-id delta-id revision)
  "Return non-nil when all durable proof fields match SESSION-ID's barrier."
  (when-let* ((coordinator (gethash store e-session-async--coordinators))
              (lane (gethash session-id
                             (e-session-async--coordinator-lanes coordinator)))
              (state (e-session-async--lane-reconciliation lane)))
    (and (equal request-id (plist-get state :request-id))
         (equal delta-id (plist-get state :delta-id))
         (equal revision (plist-get state :revision)))))

(defun e-session-async--failed-work (session-id cause)
  "Return a terminal session work handle carrying typed CAUSE."
  (let ((work (e-work-prepare e-session-async--operation-spec nil
                              :context (list :domain-ref session-id
                                             :work-kind 'session-durable))))
    (e-work-fail work cause)
    work))

(defun e-session-async--capacity-error (coordinator lane bytes)
  "Return the typed local capacity cause for BYTES at COORDINATOR/LANE."
  (list 'e-session-async-capacity-exhausted
        "Session durable-operation capacity is exhausted"
        :session-id (e-session-async--lane-session-id lane)
        :lane-count (e-session-async--lane-count lane)
        :lane-bytes (e-session-async--lane-bytes lane)
        :coordinator-count (e-session-async--coordinator-count coordinator)
        :coordinator-bytes (e-session-async--coordinator-bytes coordinator)
        :requested-bytes bytes))

(defun e-session-async--admit-p (coordinator lane bytes)
  "Return non-nil when COORDINATOR may admit BYTES in LANE."
  (and (< (e-session-async--lane-count lane)
          e-session-async-lane-operation-capacity)
       (<= (+ (e-session-async--lane-bytes lane) bytes)
           e-session-async-lane-byte-capacity)
       (< (e-session-async--coordinator-count coordinator)
          e-session-async-coordinator-operation-capacity)
       (<= (+ (e-session-async--coordinator-bytes coordinator) bytes)
           e-session-async-coordinator-byte-capacity)))

(defun e-session-async--charge (coordinator lane bytes)
  "Charge one admitted operation with BYTES exactly once."
  (cl-incf (e-session-async--lane-count lane))
  (cl-incf (e-session-async--lane-bytes lane) bytes)
  (cl-incf (e-session-async--coordinator-count coordinator))
  (cl-incf (e-session-async--coordinator-bytes coordinator) bytes))

(defun e-session-async--uncharge (coordinator lane bytes)
  "Undo one not-yet-enqueued admission charge of BYTES exactly once."
  (setf (e-session-async--lane-count lane)
        (max 0 (1- (e-session-async--lane-count lane)))
        (e-session-async--lane-bytes lane)
        (max 0 (- (e-session-async--lane-bytes lane) bytes))
        (e-session-async--coordinator-count coordinator)
        (max 0 (1- (e-session-async--coordinator-count coordinator)))
        (e-session-async--coordinator-bytes coordinator)
        (max 0 (- (e-session-async--coordinator-bytes coordinator) bytes))))

(defun e-session-async--release (operation)
  "Release OPERATION's lane and coordinator retention exactly once."
  (when (e-session-async--operation-charged operation)
    (let ((bytes (e-session-async--operation-bytes operation))
          (lane (e-session-async--operation-lane operation))
          (coordinator (e-session-async--operation-coordinator operation)))
      (setf (e-session-async--operation-charged operation) nil
            (e-session-async--operation-bytes operation) nil
            (e-session-async--lane-count lane)
            (1- (e-session-async--lane-count lane))
            (e-session-async--lane-bytes lane)
            (- (e-session-async--lane-bytes lane) bytes)
            (e-session-async--coordinator-count coordinator)
            (1- (e-session-async--coordinator-count coordinator))
            (e-session-async--coordinator-bytes coordinator)
            (- (e-session-async--coordinator-bytes coordinator) bytes))))
  ;; Handles retain their terminal result/error.  The private operation must
  ;; not retain the handle back through its arguments or closures afterwards.
  (when-let* ((work (e-session-async--operation-work operation)))
    (setf (e-work-handle-arguments work) nil))
  (when-let* ((escrow (e-session-async--operation-frame-escrow operation))
              (coordinator (e-session-async--operation-coordinator operation)))
    ;; A successful adapter submit clears this field after transferring the
    ;; reservation into DP5A.  Every earlier failure returns the exact escrow.
    (e-session-storage-release-frame-escrow
     (e-session-async--coordinator-store coordinator) escrow))
  (when-let* ((shared (e-session-async--operation-composition-bytes operation))
              (coordinator (e-session-async--operation-coordinator operation)))
    ;; P+D+R is session-owned rather than a runtime frame.  It shares the
    ;; composition ledger but must survive the frame escrow transfer until the
    ;; operation reaches a definitive session terminal state.
    (e-session-storage-release-frame-escrow
     (e-session-async--coordinator-store coordinator) shared))
  (setf (e-session-async--operation-frame-escrow operation) nil
        (e-session-async--operation-composition-bytes operation) nil
        (e-session-async--operation-command operation) nil
        (e-session-async--operation-delta operation) nil
        (e-session-async--operation-body operation) nil
        (e-session-async--operation-before-submit operation) nil
        (e-session-async--operation-result operation) nil
        (e-session-async--operation-storage-operation operation) nil
        (e-session-async--operation-first-apply-cause operation) nil
        (e-session-async--operation-work operation) nil))

(defun e-session-async--schedule (coordinator)
  "Schedule one bounded coordinator dispatch turn."
  (unless (or (e-session-async--coordinator-closed coordinator)
              (timerp (e-session-async--coordinator-scheduler-timer coordinator)))
    (let ((generation (cl-incf (e-session-async--coordinator-generation coordinator))))
      (setf (e-session-async--coordinator-scheduler-timer coordinator)
            (run-at-time 0 nil #'e-session-async--dispatch coordinator generation)))))

(defun e-session-async--enqueue-terminal-token (token)
  "Idempotently append TOKEN to its coordinator's terminal outbox in O(1)."
  (let* ((coordinator (e-session-async--terminal-token-coordinator token))
         (tail (e-session-async--coordinator-terminal-outbox-tail coordinator)))
    (unless (e-session-async--terminal-token-enqueued token)
      (let ((cell (list token)))
        (if tail
            (setcdr tail cell)
          (setf (e-session-async--coordinator-terminal-outbox coordinator) cell))
        ;; Mark enqueue ownership before arming the timer.  A timer-allocation
        ;; failure can retry without appending the same token a second time.
        (setf (e-session-async--coordinator-terminal-outbox-tail coordinator) cell
              (e-session-async--terminal-token-enqueued token) t)))
    (unless (timerp (e-session-async--coordinator-terminal-timer coordinator))
      (setf (e-session-async--coordinator-terminal-timer coordinator)
            (run-at-time 0 nil #'e-session-async--drain-terminals coordinator)))))

(defun e-session-async--pop-terminal-token (coordinator)
  "Pop one terminal token from COORDINATOR without traversing its outbox."
  (let* ((head (e-session-async--coordinator-terminal-outbox coordinator))
         (token (car head))
         (next (cdr head)))
    (setcdr head nil)
    (setf (e-session-async--coordinator-terminal-outbox coordinator) next)
    (unless next
      (setf (e-session-async--coordinator-terminal-outbox-tail coordinator) nil))
    token))

(defun e-session-async--notify-terminal-token (token)
  "Settle TOKEN once and release its pre-reserved notification references."
  (let ((coordinator (e-session-async--terminal-token-coordinator token))
        (work (e-session-async--terminal-token-work token)))
    (unwind-protect
        (condition-case _observer-exit
            (pcase (e-session-async--terminal-token-state token)
              ('finished
               (e-work-finish work (e-session-async--terminal-token-payload token)))
              ('failed
               (e-work-fail work (e-session-async--terminal-token-payload token)))
              (_ (signal 'e-session-storage-error
                         (list "Unsupported session terminal token state"))))
          (quit nil)
          (error nil))
      ;; A composition-ledger defect must not retain the already-detached
      ;; terminal token or prevent later notifications from draining.
      (condition-case nil
          (e-session-storage-release-frame-escrow
           (e-session-async--coordinator-store coordinator)
           (e-session-async--terminal-token-bytes token))
        ((error quit) nil))
      (setf (e-session-async--terminal-token-work token) nil
            (e-session-async--terminal-token-payload token) nil
            (e-session-async--terminal-token-coordinator token) nil
            (e-session-async--terminal-token-enqueued token) nil))))

(defun e-session-async--drain-terminals (coordinator)
  "Drain at most 16 isolated terminal notifications from COORDINATOR.

The unwind cleanup arms the next timer even when an observer quits, throws to
an enclosing catch, errors, or reenters, so the detached tail cannot be
stranded or reacquire lane scheduler ownership."
  (unless (e-session-async--coordinator-terminal-draining coordinator)
    (setf (e-session-async--coordinator-terminal-timer coordinator) nil
          (e-session-async--coordinator-terminal-draining coordinator) t)
    (unwind-protect
        (let ((remaining e-session-async-publication-drain-limit))
          (while (and (> remaining 0)
                      (e-session-async--coordinator-terminal-outbox coordinator))
            (cl-decf remaining)
            (let ((token (e-session-async--pop-terminal-token coordinator)))
              (e-session-async--notify-terminal-token token))))
      (setf (e-session-async--coordinator-terminal-draining coordinator) nil)
      ;; This cleanup also runs for a nonlocal observer exit.  Remaining
      ;; detached tokens are therefore never stranded, while an ordinary full
      ;; page schedules exactly one following page and no empty timer.
      (when (and (e-session-async--coordinator-terminal-outbox coordinator)
                 (not (timerp (e-session-async--coordinator-terminal-timer
                               coordinator))))
        (setf (e-session-async--coordinator-terminal-timer coordinator)
              (run-at-time 0 nil #'e-session-async--drain-terminals
                           coordinator))))))

(defun e-session-async--transfer-terminal-token
    (operation state payload &optional retained-bytes)
  "Transfer OPERATION's R and optional RETAINED-BYTES to its prepared token."
  (let* ((coordinator (e-session-async--operation-coordinator operation))
         (shared (e-session-async--operation-composition-bytes operation))
         (token (e-session-async--operation-terminal-token operation))
         (references
          (plist-get
           (e-session-aggregate-command-account
            (e-session-async--operation-command operation))
           :reference-bytes)))
    ;; Validate every reference and amount before the shared ledger changes.
    (unless (and token
                 (eq coordinator
                     (e-session-async--terminal-token-coordinator token))
                 (eq (e-session-async--operation-work operation)
                     (e-session-async--terminal-token-work token))
                 (= references (e-session-async--terminal-token-bytes token))
                 shared
                 (>= shared (+ references (or retained-bytes 0))))
      (signal 'e-session-storage-error
              (list "Session operation lost terminal reference reservation")))
    (let ((inhibit-quit t))
      (e-session-storage-release-frame-escrow
       (e-session-async--coordinator-store coordinator)
       (- shared references (or retained-bytes 0)))
      (setf (e-session-async--operation-composition-bytes operation) nil
            (e-session-async--terminal-token-state token) state
            (e-session-async--terminal-token-payload token) payload))
    token))

(defun e-session-async--advance-completion
    (operation expected next action function)
  "Run one real completion ACTION and atomically advance EXPECTED to NEXT."
  (when (eq (e-session-async--operation-completion-phase operation) expected)
    (e-session-async--completion-fault action 'before)
    (let (result (inhibit-quit t))
      (setq result (funcall function))
      (setf (e-session-async--operation-completion-phase operation) next)
      (e-session-async--completion-fault action 'after)
      result)))

(defun e-session-async--dispatch (coordinator generation)
  "Start at most 16 queued operations from distinct idle session lanes.

Different sessions may submit to the shared worker in the same turn; DP5A
retains physical write ordering, while each lane remains FIFO."
  (when (= generation (e-session-async--coordinator-generation coordinator))
    (setf (e-session-async--coordinator-scheduler-timer coordinator) nil)
    (unless (or (e-session-async--coordinator-closing coordinator)
                (e-session-async--coordinator-closed coordinator))
      (let (lanes (started 0))
        (maphash (lambda (_ lane) (push lane lanes))
                 (e-session-async--coordinator-lanes coordinator))
        (dolist (lane lanes)
          (when (and (< started e-session-async-publication-drain-limit)
                     (not (e-session-async--lane-active lane))
                     (e-session-async--lane-queue lane))
            (cl-incf started)
            (let ((operation (pop (e-session-async--lane-queue lane))))
              (setf (e-session-async--lane-active lane) operation
                    (e-session-async--operation-state operation) 'starting)
              (e-work-start-prepared (e-session-async--operation-work operation)))))
        ;; Never let a wide fan-out monopolize an editor timer turn.
        (when (cl-some (lambda (lane)
                         (and (not (e-session-async--lane-active lane))
                              (e-session-async--lane-queue lane)))
                       lanes)
          (e-session-async--schedule coordinator))))))

(defun e-session-async--enqueue-publication (operation)
  "Queue OPERATION's post-ack publication outside runtime callbacks."
  (let ((coordinator (e-session-async--operation-coordinator operation)))
    (setf (e-session-async--coordinator-publication-outbox coordinator)
          (nconc (e-session-async--coordinator-publication-outbox coordinator)
                 (list operation)))
    (unless (timerp (e-session-async--coordinator-publication-timer coordinator))
      (setf (e-session-async--coordinator-publication-timer coordinator)
            (run-at-time 0 nil #'e-session-async--drain-publications coordinator)))))

(defun e-session-async--mark-reconciliation-required (operation cause)
  "Transfer OPERATION ownership to one exact barrier plus terminal token."
  (let* ((lane (e-session-async--operation-lane operation))
         (coordinator (e-session-async--operation-coordinator operation))
         (barrier-bytes e-session-async-reconciliation-barrier-bytes)
         (references
          (plist-get
           (e-session-aggregate-command-account
            (e-session-async--operation-command operation))
           :reference-bytes))
         (shared (e-session-async--operation-composition-bytes operation)))
    (unless (e-session-async--operation-charged operation)
      (signal 'e-session-storage-error
              (list "Acknowledged operation lost its reconciliation reservation")))
    (unless (and shared (>= shared (+ barrier-bytes references)))
      (signal 'e-session-storage-error
              (list "Reconciliation barrier lacks shared reservation")))
    (let ((released (- (e-session-async--operation-bytes operation)
                       barrier-bytes))
          token
          (inhibit-quit t))
      (setq token
            (e-session-async--transfer-terminal-token
             operation 'failed cause barrier-bytes))
      ;; Preserve the active count slot, replacing all local operation bytes
      ;; with exactly one fixed barrier.
      (setf (e-session-async--operation-charged operation) nil
            (e-session-async--operation-bytes operation) nil
            (e-session-async--lane-bytes lane)
            (- (e-session-async--lane-bytes lane) released)
            (e-session-async--coordinator-bytes coordinator)
            (- (e-session-async--coordinator-bytes coordinator) released))
      (cl-incf (e-session-async--coordinator-reconciliation-count coordinator))
      (cl-incf (e-session-async--coordinator-reconciliation-bytes coordinator)
               barrier-bytes)
      (setf (e-session-async--lane-reconciliation lane)
            (e-session-async--reconciliation-shape operation cause))
      token)))

(defun e-session-async--retire-lane-failure (operation cause &optional barrier)
  "Atomically detach OPERATION and its lane tail before failure callbacks.

When BARRIER is non-nil, OPERATION's acknowledged proof replaces its charge
with the exact reconciliation barrier.  Every tail operation releases first;
only then are the O(1) terminal tokens made observable."
  (let* ((lane (e-session-async--operation-lane operation))
         (coordinator (e-session-async--operation-coordinator operation))
         (tail (e-session-async--lane-queue lane))
         (active-token
          (if barrier
              (e-session-async--mark-reconciliation-required operation cause)
            (e-session-async--transfer-terminal-token
             operation 'failed cause)))
         (tokens (list active-token)))
    ;; Detach the complete lane before releasing a single payload or notifying
    ;; a public observer.  Reentrant admission sees either the barrier or a new
    ;; independent lane, never the failed tail.
    (setf (e-session-async--lane-active lane) nil
          (e-session-async--lane-queue lane) nil)
    (dolist (dependent tail)
      (push (e-session-async--transfer-terminal-token
             dependent 'failed cause)
            tokens))
    (e-session-async--release operation)
    (dolist (dependent tail)
      (e-session-async--release dependent))
    (unless barrier
      (remhash (e-session-async--lane-session-id lane)
               (e-session-async--coordinator-lanes coordinator)))
    (e-session-async--schedule coordinator)
    (cl-mapc
     (lambda (owned token)
       (e-session-async--enqueue-terminal-token token)
       (e-session-async--clear-operation-links owned))
     (cons operation tail) (nreverse tokens))))

(defun e-session-async--attempt-authoritative-apply (operation)
  "Apply OPERATION once, or retain its rolled-back delta for reconciliation."
  (condition-case err
      (let* ((coordinator (e-session-async--operation-coordinator operation))
             (store (e-session-async--coordinator-store coordinator))
             (delta (e-session-async--operation-delta operation))
             (inhibit-quit t))
        (e-session-aggregate-apply-committed-record
         store (plist-get delta :record))
        ;; Aggregate return and this marker are one quit-inhibited transition.
        ;; The injected after-return fault runs only after the marker, and the
        ;; handler below checks it before considering reconciliation.
        (setf (e-session-async--operation-applied operation) t
              (e-session-async--operation-state operation) 'applied
              (e-session-async--operation-completion-phase operation) 'applied)
        (e-session-async--completion-fault 'apply-after-return 'after)
        'applied)
    ((error quit)
     (if (e-session-async--operation-applied operation)
         'retry
       (unless (e-session-async--operation-first-apply-cause operation)
         (setf (e-session-async--operation-first-apply-cause operation) err))
       (cl-incf (e-session-async--operation-apply-attempts operation))
       (if (< (e-session-async--operation-apply-attempts operation)
              e-session-async-reconciliation-attempt-limit)
           'retry
         (e-session-async--retire-lane-failure
          operation (e-session-async--operation-first-apply-cause operation) t)
         'failed)))))

(defun e-session-async--finish-applied-operation (operation)
  "Idempotently advance already-applied OPERATION through terminal enqueue."
  (let* ((coordinator (e-session-async--operation-coordinator operation))
         (store (e-session-async--coordinator-store coordinator))
         (delta (e-session-async--operation-delta operation))
         (command (e-session-async--operation-command operation))
         (lane (e-session-async--operation-lane operation)))
    (while (not (eq (e-session-async--operation-completion-phase operation)
                    'terminal-enqueued))
      (pcase (e-session-async--operation-completion-phase operation)
        ('applied
         (e-session-async--advance-completion
          operation 'applied 'projected 'projection
          (lambda ()
            (when (e-session-async--operation-write-index operation)
              (condition-case projection-error
                  (e-session-storage--mark-checkpoint-dirty
                   store (e-session-async--operation-session-id operation))
                ((error quit)
                 ;; Projection bookkeeping is non-gating.  Make one bounded
                 ;; diagnostic attempt, isolate its exit independently, and
                 ;; retire this phase regardless.  A future retry, if useful,
                 ;; belongs to a separate bounded projection owner rather than
                 ;; retaining or re-running authoritative session completion.
                 (condition-case nil
                     (e-session-storage-note-projection-error
                      store projection-error)
                   ((error quit) nil))))))))
        ('projected
         (e-session-async--advance-completion
          operation 'projected 'result-ready 'result
          (lambda ()
            (setf (e-session-async--operation-public-result operation)
                  (e-session-aggregate-command-result store command delta)))))
        ('result-ready
         (e-session-async--advance-completion
          operation 'result-ready 'token-transferred 'shared-release
          (lambda ()
            (e-session-async--transfer-terminal-token
             operation 'finished
             (e-session-async--operation-public-result operation)))))
        ('token-transferred
         (e-session-async--advance-completion
          operation 'token-transferred 'lane-detached 'lane-detach
          (lambda ()
            ;; Validate all owner links before the atomic local release.  The
            ;; operation/token association remains until terminal enqueue.
            (unless (and (eq (e-session-async--lane-active lane) operation)
                         (e-session-async--operation-charged operation)
                         (null (e-session-async--operation-composition-bytes
                                operation)))
              (signal 'e-session-storage-error
                      (list "Invalid session completion detach state")))
            (let ((inhibit-quit t))
              (setf (e-session-async--lane-active lane) nil)
              (e-session-async--release operation)
              (when (and (null (e-session-async--lane-queue lane))
                         (null (e-session-async--lane-reconciliation lane)))
                (remhash (e-session-async--lane-session-id lane)
                         (e-session-async--coordinator-lanes coordinator)))))))
        ('lane-detached
         (e-session-async--advance-completion
          operation 'lane-detached 'scheduled 'schedule
          (lambda () (e-session-async--schedule coordinator))))
        ('scheduled
         (e-session-async--advance-completion
          operation 'scheduled 'terminal-enqueued 'terminal-enqueue
          (lambda ()
            (e-session-async--enqueue-terminal-token
             (e-session-async--operation-terminal-token operation)))))
        (_ (signal 'e-session-storage-error
                   (list "Invalid session completion phase"
                         (e-session-async--operation-completion-phase
                          operation))))))
    ;; Only a durably linked terminal outbox token allows the publication
    ;; owner to forget the operation/token association.
    (e-session-async--clear-operation-links operation)
    'finished))

(defun e-session-async--finish-operation (operation)
  "Advance OPERATION's split apply and post-apply completion state machine."
  (let ((state (if (e-session-async--operation-applied operation)
                   'applied
                 (e-session-async--attempt-authoritative-apply operation))))
    (if (eq state 'applied)
        (condition-case _cleanup-exit
            (e-session-async--finish-applied-operation operation)
          ((error quit) 'retry))
      state)))

(defun e-session-async--release-reconciliation (coordinator lane)
  "Release LANE's retained reconciliation reservation exactly once."
  (when-let* ((state (e-session-async--lane-reconciliation lane)))
    (when (plist-get state :charged)
      (let ((bytes (plist-get state :bytes)))
        (setf (e-session-async--lane-count lane)
              (1- (e-session-async--lane-count lane))
              (e-session-async--lane-bytes lane)
              (- (e-session-async--lane-bytes lane) bytes)
              (e-session-async--coordinator-count coordinator)
              (1- (e-session-async--coordinator-count coordinator))
              (e-session-async--coordinator-bytes coordinator)
              (- (e-session-async--coordinator-bytes coordinator) bytes))
        (cl-decf (e-session-async--coordinator-reconciliation-count coordinator))
        (cl-decf (e-session-async--coordinator-reconciliation-bytes coordinator)
                 bytes)
        ;; `mark-reconciliation-required' retained this exact residual in the
        ;; composition ledger.  The lane barrier is its sole later releaser.
        (e-session-storage-release-frame-escrow
         (e-session-async--coordinator-store coordinator) bytes)))
    (setf (e-session-async--lane-reconciliation lane) nil)))

(defun e-session-async-teardown (store)
  "Release idle coordinator barriers during owner reset or settled teardown.

This is intentionally not asynchronous session close orchestration.  It is a
small ownership cleanup primitive for already-settled lanes and their timers."
  (when-let* ((coordinator (gethash store e-session-async--coordinators)))
    (when (or (e-session-async--coordinator-publication-outbox coordinator)
              (e-session-async--coordinator-terminal-outbox coordinator))
      (signal 'e-session-storage-error
              (list "Cannot tear down unsettled session publications")))
    (maphash
     (lambda (_session-id lane)
       (when (or (e-session-async--lane-active lane)
                 (e-session-async--lane-queue lane))
         (signal 'e-session-storage-error
                 (list "Cannot tear down an unsettled session coordinator"))))
     (e-session-async--coordinator-lanes coordinator))
    (dolist (timer (list (e-session-async--coordinator-scheduler-timer coordinator)
                         (e-session-async--coordinator-publication-timer coordinator)
                         (e-session-async--coordinator-terminal-timer coordinator)))
      (when (timerp timer) (cancel-timer timer)))
    (maphash
     (lambda (_session-id lane)
       (e-session-async--release-reconciliation coordinator lane))
     (e-session-async--coordinator-lanes coordinator))
    (remhash store e-session-async--coordinators)
    t))

(defun e-session-async--clear-operation-links (operation)
  "Break OPERATION's remaining private owner links after terminal cleanup."
  (setf (e-session-async--operation-lane operation) nil
        (e-session-async--operation-coordinator operation) nil
        (e-session-async--operation-session-id operation) nil
        (e-session-async--operation-terminal-token operation) nil
        (e-session-async--operation-public-result operation) nil))

(defun e-session-async--drain-publications (coordinator)
  "Advance at most `e-session-async-publication-drain-limit' retained owners.

The head is removed only after terminal enqueue or definitive apply failure;
an error or quit in any completion action therefore cannot lose publication."
  (setf (e-session-async--coordinator-publication-timer coordinator) nil)
  (let ((remaining e-session-async-publication-drain-limit))
    (while (and (> remaining 0)
                (e-session-async--coordinator-publication-outbox coordinator))
      (cl-decf remaining)
      (let* ((operation
              (car (e-session-async--coordinator-publication-outbox coordinator)))
             (outcome
              (condition-case nil
                  (e-session-async--finish-operation operation)
                ((error quit) 'retry))))
        (if (memq outcome '(finished failed))
            (pop (e-session-async--coordinator-publication-outbox coordinator))
          ;; Apply reconciliation and post-apply completion retries each get a
          ;; separate bounded timer turn while retaining the outbox head.
          (setq remaining 0))))
    (when (e-session-async--coordinator-publication-outbox coordinator)
      (setf (e-session-async--coordinator-publication-timer coordinator)
            (run-at-time 0 nil #'e-session-async--drain-publications coordinator)))))

(defun e-session-async--storage-settled (operation result error)
  "Translate adapter RESULT or ERROR into publication or terminal failure."
  (if (null error)
      ;; DP5A has released the immutable transport frame before this callback;
      ;; retain only the definitive bounded runtime result until publication.
      (progn
        (setf (e-session-async--operation-result operation)
              result
              (e-session-async--operation-state operation) 'acknowledged)
        (e-session-async--enqueue-publication operation))
    (let ((cause error))
      (e-session-async--retire-lane-failure operation cause))))

(defun e-session-async--start-operation (operation)
  "Interpret and submit OPERATION's sealed command to DP5A without awaiting."
  (condition-case err
      (let* ((coordinator (e-session-async--operation-coordinator operation))
             (store (e-session-async--coordinator-store coordinator))
             (delta (e-session-aggregate-command-interpret
                     store (e-session-async--operation-command operation)))
             (record (plist-get delta :record))
             ;; The body and delta share RECORD.  No whole-session stage or
             ;; second durable payload survives this lane-head turn.
             (continuity
              (when-let* ((augment
                           (e-session-async--operation-before-submit operation)))
                (funcall augment delta)))
             (body (if continuity
                       (list :op 'session-append-with-tool-transition
                             :session-id
                             (e-session-async--operation-session-id operation)
                             :record record :continuity continuity)
                     (list :op 'session-append
                           :session-id
                           (e-session-async--operation-session-id operation)
                           :record record)))
             (actual-frame (e-session-storage-measure-frame-escrow
                            store 'write body))
             (reserved-frame (e-session-async--operation-frame-escrow operation)))
        (when (> actual-frame reserved-frame)
          (signal 'e-session-async-capacity-exhausted
                  (list "Interpreted session delta exceeded its frame escrow"
                        :actual actual-frame :reserved reserved-frame
                        :family (e-session-async--operation-family operation))))
        (setf (e-session-async--operation-delta operation) delta
              (e-session-async--operation-body operation) body)
        (setf (e-session-async--operation-state operation) 'submitted)
        ;; The storage contract returns a non-nil opaque submission only
        ;; after DP5A accepted the composition escrow.  Until that point the
        ;; operation remains its sole owner and `--release' rolls it back.
        ;; This keeps an adapter/preflight exception from silently losing or
        ;; double-releasing shared frame bytes.
        (let ((submitted
               (e-session-storage-submit
                store 'write body
                (lambda (result error)
                  (e-session-async--storage-settled operation result error))
                (e-session-async--operation-frame-escrow operation))))
          (unless submitted
            (signal 'e-session-storage-error
                    (list "Storage did not accept composition frame escrow")))
          (setf (e-session-async--operation-storage-operation operation)
                submitted))
        ;; The runtime owns the surviving actual frame now.  Its terminal
        ;; release is the only release path after this transfer.
        (setf (e-session-async--operation-frame-escrow operation) nil))
    ((error quit)
     (e-session-async--retire-lane-failure operation err))))

(cl-defun e-session-async--submit-command
    (store session-id tag arguments &key before-submit write-index)
  "Admit one sealed aggregate TAG with ARGUMENTS and return its `e-work'.

Validation and exact allocation-free measurement precede every retained
representation.  The coordinator then atomically reserves the lane slot and
P+D+R+Freserve, seals the command, and queues it.  Interpretation occurs only
  at the lane head against acknowledged live state."
  (e-session-aggregate-command-validate tag session-id arguments)
  (let* ((frame-measurer
          (lambda (body)
            (e-session-storage-measure-frame-escrow store 'write body)))
         ;; The aggregate's schema calculator is the only accounting pass over
         ;; the caller graph.  It returns exact unique-leaf P, generated D/R,
         ;; and the measured maximum transport envelope before detachment.
         (accounting (e-session-aggregate-command-accounting
                      tag session-id arguments frame-measurer))
         (family (plist-get accounting :family))
         (producer-bytes (plist-get accounting :producer-bytes))
         (local-bytes (plist-get accounting :local-bytes))
         (frame-escrow (plist-get accounting :frame-reserve))
         (coordinator (e-session-async-coordinator store))
         (generated-id-p (null session-id))
         (existing (and session-id (gethash session-id
                            (e-session-async--coordinator-lanes coordinator)))))
    (if (and existing (e-session-async--lane-reconciliation existing))
        (e-session-async--failed-work
         session-id (e-session-async--reconciliation-error existing))
      (let* ((lane (or existing
                       (e-session-async--lane-create :session-id session-id)))
             (shared-bytes (+ local-bytes frame-escrow)))
        (if (not (e-session-async--admit-p coordinator lane local-bytes))
            (e-session-async--failed-work
             session-id
             (e-session-async--capacity-error coordinator lane local-bytes))
          (let (charged shared-reserved success work operation command)
            (unwind-protect
                (progn
                  ;; Both ledgers become owner-visible before command identity,
                  ;; time, detachment, or command allocation can occur.
                  (e-session-async--charge coordinator lane local-bytes)
                  (setq charged t)
                  (e-session-storage-reserve-frame-escrow store shared-bytes)
                  (setq shared-reserved t)
                  (setq command
                        (e-session-aggregate-command-seal
                         tag session-id arguments accounting frame-measurer)
                        session-id
                        (e-session-aggregate-command-session-id command))
                  ;; Generated create identities are intentionally allocated
                  ;; only inside the post-reservation sealed constructor.
                  ;; Resolve the lane key afterwards, before anything can be
                  ;; enqueued or published.
                  (when (and generated-id-p
                             (or (gethash session-id
                                          (e-session-async--coordinator-lanes
                                           coordinator))
                                 (e-session-aggregate-session-present-p
                                  store session-id)))
                    (signal 'e-session-duplicate (list session-id)))
                  (setf (e-session-async--lane-session-id lane) session-id)
                  (setq
                        work
                        (e-work-prepare
                         e-session-async--operation-spec nil
                         :context (list :domain-ref session-id
                                        :work-kind 'session-durable))
                        operation
                        (e-session-async--operation-create
                         :coordinator coordinator :lane lane
                         :session-id session-id
                         :command command :bytes local-bytes
                         :producer-bytes producer-bytes
                         :frame-escrow frame-escrow
                         :composition-bytes local-bytes :family family
                         :work work :before-submit before-submit
                         :write-index write-index :state 'queued
                         :generation
                         (e-session-async--coordinator-generation coordinator)))
                  ;; R pays for this one fixed token at admission.  Every
                  ;; authoritative completion therefore owns a terminal object
                  ;; before live apply and never allocates one while retiring.
                  (e-session-async--completion-fault 'token-allocation 'before)
                  (setf (e-session-async--operation-terminal-token operation)
                        (e-session-async--terminal-token-create
                         :coordinator coordinator :work work
                         :bytes (plist-get accounting :reference-bytes)))
                  (e-session-async--completion-fault 'token-allocation 'after)
                  (setf (e-work-handle-arguments work) operation
                        (e-session-async--operation-charged operation) t)
                  (unless existing
                    (puthash session-id lane
                             (e-session-async--coordinator-lanes coordinator)))
                  (setf (e-session-async--lane-queue lane)
                        (nconc (e-session-async--lane-queue lane)
                               (list operation)))
                  (e-session-async--schedule coordinator)
                  (setq success t)
                  work)
              (unless success
                (when shared-reserved
                  (e-session-storage-release-frame-escrow store shared-bytes))
                (when charged
                  (e-session-async--uncharge coordinator lane local-bytes))))))))))

(cl-defun e-session-async-submit-command
    (store session-id tag arguments &key before-submit write-index)
  "Admit one sealed session command or return a terminal capacity work.

Expected producer/composition exhaustion is part of the asynchronous facade:
it returns an already-failed `e-work' and owns no lane, payload, or escrow.
Invariant, validation, freeze, parity, and constructor bugs still surface."
  (condition-case err
      (e-session-async--submit-command
       store session-id tag arguments
       :before-submit before-submit :write-index write-index)
    ((e-session-command-too-large e-runtime-store-codec-too-large)
     (e-session-async--failed-work
      session-id
      (list 'e-session-async-capacity-exhausted
            "Session command producer exceeds its byte capacity"
            :cause err)))
    (e-session-async-capacity-exhausted
     (e-session-async--failed-work session-id err))))

(defun e-session-async-unsupported-command (session-id name)
  "Return a terminal typed work for unsupported asynchronous command NAME."
  (e-session-async--failed-work
   session-id
   (list 'e-session-storage-command-error
         "Unsupported asynchronous session command" name)))

(provide 'e-session-async)

;;; e-session-async.el ends here

;;; e-board.el --- Process-local board routing core for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This is the pure, process-local board model.  It owns no harness endpoint
;; and performs no delivery; consumers inspect the frozen pickup envelopes.

;;; Code:

(require 'cl-lib)
(require 'e-work)

(define-error 'e-board-error "e board error")
(define-error 'e-board-id-conflict "e board id conflict" 'e-board-error)
(define-error 'e-board-missing "e board is not registered" 'e-board-error)
(define-error 'e-board-invalid-source-key "Invalid board source key" 'e-board-error)
(define-error 'e-board-invalid-activity "Invalid board activity message" 'e-board-error)
(define-error 'e-board-observer-missing "Unknown board observer" 'e-board-error)

(defvar e-board--id-sequence 0
  "Process-local fallback sequence for board identities.")

(defvar e-board--registry (make-hash-table :test 'equal)
  "Live process-local boards keyed by board id.")

(cl-defstruct (e-board-message
               (:constructor e-board-message--create)
               (:conc-name e-board-message-))
  id board-id seq kind author tags attributes to mode content reference
  source-input-key source-output-key reply-to-message-ids caused-by-delivery-ids
  source-activity-key source-fact-key subject-participant-id source-turn-id
  activity-kind
  matching-participant-ids pickup-ids unrouted-reason routing-state)

(cl-defstruct (e-board-event
               (:constructor e-board-event--create)
               (:conc-name e-board-event-))
  seq type data)

(cl-defstruct (e-board-participant
               (:constructor e-board-participant--create)
               (:conc-name e-board-participant-))
  id board-id state create-pickup-subscription-id)

(cl-defstruct (e-board-subscription
               (:constructor e-board-subscription--create)
               (:conc-name e-board-subscription-))
  id board-id participant-id selector effect state built-in-p)

(defconst e-board--ordinary-subscription-states
  '(active muted completed faulted cancelled expired)
  "States available to ordinary orchestration subscriptions.")

(defconst e-board--observer-states
  '(active muted faulted cancelled expired)
  "States available to effect-free board observers.")

(cl-defstruct (e-board-observer
               (:constructor e-board-observer--create)
               (:conc-name e-board-observer-))
  id board-id client-id selector state next-seq)

(defconst e-board-max-derived-hops 8
  "Maximum subscription lineage depth for derived board inputs.")

(defconst e-board-terminal-classification-drain-limit 16
  "Maximum indexed terminal subscription clauses classified per drain.")

(defconst e-board-input-classification-drain-limit 32
  "Maximum frozen input subscription clauses classified per drain.")

(defconst e-board-aggregation-deadline-drain-limit 16
  "Maximum aggregation deadline transitions committed per drain.")

(cl-defstruct (e-board-pickup
               (:constructor e-board-pickup--create)
               (:conc-name e-board-pickup-))
  delivery-id board-id message-id participant-id subscription-ids mode state)

(cl-defstruct (e-board-publication
               (:constructor e-board-publication--create)
               (:conc-name e-board-publication-))
  status message pickup-ids)

(cl-defstruct (e-board-work
                (:constructor e-board-work--create)
                (:conc-name e-board-work-))
  id handle metadata state terminal-seq terminal-payload)

(cl-defstruct (e-board-invocation
                (:constructor e-board-invocation--create)
                (:conc-name e-board-invocation-))
  id work-id state effect-target activation-id)

(cl-defstruct (e-board-aggregation
                (:constructor e-board-aggregation--create)
                (:conc-name e-board-aggregation-))
  id work-ids mode state effect-target timer activation-id)

(cl-defstruct (e-board-terminal-classification
               (:constructor e-board-terminal-classification--create)
               (:conc-name e-board-terminal-classification-))
  work-id invocation-ids aggregation-ids invocation-index aggregation-index)

(cl-defstruct (e-board-input-classification
               (:constructor e-board-input-classification--create)
               (:conc-name e-board-input-classification-))
  message publication subscriptions index matches post-subscriptions post-index)

(cl-defstruct (e-board
               (:constructor e-board--create)
               (:conc-name e-board-))
  id id-function next-seq events messages message-table participants subscriptions
  observers pickups source-high-watermarks source-recent work-table invocations aggregations pending-effects
  effect-scheduler invocation-effect-dispatcher invocation-work-index aggregation-work-index
  terminal-classifications terminal-classification-scheduled terminal-classification-scheduler
  input-classifications input-classification-scheduled input-classification-scheduler
  routed-pickup-results
  aggregation-deadlines aggregation-deadline-scheduled aggregation-deadline-scheduler)

(defun e-board--next-id (board kind)
  "Return BOARD's next identity for KIND.
An injected id function receives KIND.  The fallback is only process-local and
exists so callers need not supply ids outside deterministic tests."
  (let ((id (if-let ((function (e-board-id-function board)))
                (funcall function kind)
              (format "%s%d"
                      (pcase kind
                        ('board "brd_")
                        ('client "cli_")
                        ('participant "ptc_")
                        ('subscription "sub_")
                        ('invocation "inv_")
                        ('message "msg_")
                        (_ (format "%s_" kind)))
                      (cl-incf e-board--id-sequence)))))
    (unless id
      (signal 'e-board-error (list "Id generator returned nil" kind)))
    id))

(defun e-board--require-id (id name)
  "Return ID or signal that required identity NAME is absent."
  (unless id
    (signal 'wrong-type-argument (list name id)))
  id)

(defun e-board-register (board)
  "Register BOARD in the process-local board registry and return it."
  (unless (e-board-p board)
    (signal 'wrong-type-argument (list 'e-board-p board)))
  (let ((id (e-board-id board)))
    (when-let ((existing (gethash id e-board--registry)))
      (unless (eq existing board)
        (signal 'e-board-id-conflict (list id))))
    (puthash id board e-board--registry))
  board)

(defun e-board-get (id)
  "Return the registered board ID, or signal `e-board-missing'."
  (or (gethash id e-board--registry)
      (signal 'e-board-missing (list id))))

(defun e-board-unregister (board-or-id)
  "Remove BOARD-OR-ID from the process-local registry.
The board object remains valid for inspection by its holder."
  (let ((id (if (e-board-p board-or-id)
                (e-board-id board-or-id)
              board-or-id)))
    (remhash id e-board--registry))
  nil)

(defun e-board-list ()
  "Return registered boards sorted by printable identity."
  (let (boards)
    (maphash (lambda (_id board) (push board boards)) e-board--registry)
    (sort boards (lambda (left right)
                   (string< (format "%s" (e-board-id left))
                            (format "%s" (e-board-id right)))))))

(cl-defun e-board-create
    (&key id id-function effect-scheduler invocation-effect-dispatcher
          terminal-classification-scheduler input-classification-scheduler
          aggregation-deadline-scheduler
          (register t))
  "Create a process-local board with ID and optional ID-FUNCTION.
ID-FUNCTION receives a symbol such as `message' or `subscription'.  Passing
explicit ids to individual operations takes precedence over this generator."
  (let* ((board (e-board--create
                  :id (or id (format "brd_%d" (cl-incf e-board--id-sequence)))
                 :id-function id-function
                 :next-seq 0
                 :events nil
                 :messages nil
                 :message-table (make-hash-table :test 'equal)
                 :participants (make-hash-table :test 'equal)
                 :subscriptions nil
                 :observers (make-hash-table :test 'equal)
                  :pickups (make-hash-table :test 'equal)
                  :source-high-watermarks (make-hash-table :test 'equal)
                  :source-recent (make-hash-table :test 'equal)
                   :work-table (make-hash-table :test 'equal)
                   :invocations (make-hash-table :test 'equal)
                   :aggregations (make-hash-table :test 'equal)
                  :pending-effects nil
                  :effect-scheduler effect-scheduler
                  :invocation-effect-dispatcher invocation-effect-dispatcher
                  :invocation-work-index (make-hash-table :test 'equal)
                  :aggregation-work-index (make-hash-table :test 'equal)
                  :terminal-classifications nil
                  :terminal-classification-scheduled nil
                  :terminal-classification-scheduler terminal-classification-scheduler
                  :input-classifications nil
                  :input-classification-scheduled nil
                  :input-classification-scheduler input-classification-scheduler
                  :routed-pickup-results nil
                  :aggregation-deadlines nil
                  :aggregation-deadline-scheduled nil
                  :aggregation-deadline-scheduler aggregation-deadline-scheduler)))
    (when register (e-board-register board))
    board))

(defun e-board--append-event (board type data)
  "Append TYPE with DATA to BOARD's ordered event log and return the event."
  (let ((event (e-board-event--create
                :seq (cl-incf (e-board-next-seq board))
                :type type
                :data data)))
    (setf (e-board-events board) (append (e-board-events board) (list event)))
    event))

(defun e-board-events-after (board seq)
  "Return BOARD events whose sequence is strictly greater than SEQ."
  (cl-remove-if (lambda (event) (<= (e-board-event-seq event) seq))
                (e-board-events board)))

(defun e-board-message (board message-id)
  "Return BOARD message MESSAGE-ID, or nil when it is not retained."
  (gethash message-id (e-board-message-table board)))

(defun e-board-participant (board participant-id)
  "Return BOARD participant PARTICIPANT-ID, or nil."
  (gethash participant-id (e-board-participants board)))

(defun e-board-pickup (board delivery-id)
  "Return BOARD pickup DELIVERY-ID, or nil."
  (gethash delivery-id (e-board-pickups board)))

(defun e-board-observed-work (board work-id)
  "Return BOARD's observed work record for WORK-ID, or nil."
  (gethash work-id (e-board-work-table board)))

(defun e-board-invocation (board invocation-id)
  "Return BOARD's exact invocation relation for INVOCATION-ID, or nil."
  (gethash invocation-id (e-board-invocations board)))

(defun e-board-aggregation (board aggregation-id)
  "Return BOARD's aggregation subscription for AGGREGATION-ID, or nil."
  (gethash aggregation-id (e-board-aggregations board)))

(defun e-board-observer (board observer-id)
  "Return BOARD's client observer cursor OBSERVER-ID, or nil."
  (gethash observer-id (e-board-observers board)))

(defun e-board--schedule-effect (board effect)
  "Schedule BOARD EFFECT after the initiating work-start stack unwinds."
  (setf (e-board-pending-effects board)
        (append (e-board-pending-effects board) (list effect)))
  (if-let ((scheduler (e-board-effect-scheduler board)))
      (funcall scheduler (lambda () (e-board-drain-effects board)))
    (run-at-time 0 nil (lambda () (e-board-drain-effects board)))))

(defun e-board--schedule-aggregation-deadline (board)
  "Schedule one later deadline drain for BOARD."
  (unless (e-board-aggregation-deadline-scheduled board)
    (setf (e-board-aggregation-deadline-scheduled board) t)
    (if-let ((scheduler (e-board-aggregation-deadline-scheduler board)))
        (funcall scheduler (lambda () (e-board-drain-aggregation-deadlines board)))
      (run-at-time 0 nil (lambda () (e-board-drain-aggregation-deadlines board))))))

(defun e-board--queue-aggregation-deadline (board aggregation-id)
  "Record an elapsed aggregation deadline without settling on the timer stack."
  (setf (e-board-aggregation-deadlines board)
        (append (e-board-aggregation-deadlines board) (list aggregation-id)))
  (e-board--schedule-aggregation-deadline board))

(defun e-board-drain-aggregation-deadlines (board)
  "Commit a bounded page of elapsed aggregation deadlines in board order."
  (setf (e-board-aggregation-deadline-scheduled board) nil)
  (let ((remaining e-board-aggregation-deadline-drain-limit))
    (while (and (> remaining 0) (e-board-aggregation-deadlines board))
      (let ((aggregation-id (pop (e-board-aggregation-deadlines board))))
        (when-let ((aggregation (e-board-aggregation board aggregation-id)))
          (when (eq (e-board-aggregation-state aggregation) 'open)
            (e-board--settle-aggregation board aggregation 'timed-out)))
        (cl-decf remaining)))
    (when (e-board-aggregation-deadlines board)
      (e-board--schedule-aggregation-deadline board))))

(defun e-board-drain-effects (board)
  "Apply BOARD's frozen effects once, in publication order.
The runtime invokes this through the injected scheduler; reducers only append
effect records and never synchronously enter a tool or harness callback."
  (let ((effects (e-board-pending-effects board)))
    (setf (e-board-pending-effects board) nil)
    (dolist (effect effects)
      (funcall effect))))

(defun e-board--index-work-subscription (index work-id subscription-id)
  "Add SUBSCRIPTION-ID to WORK-ID's exact INDEX without scanning its peers."
  (puthash work-id (append (gethash work-id index) (list subscription-id)) index))

(defun e-board--schedule-terminal-classification (board)
  "Schedule BOARD's bounded terminal classifier once after settlement returns."
  (unless (e-board-terminal-classification-scheduled board)
    (setf (e-board-terminal-classification-scheduled board) t)
    (if-let ((scheduler (e-board-terminal-classification-scheduler board)))
        (funcall scheduler (lambda () (e-board-drain-terminal-classifications board)))
      (run-at-time 0 nil (lambda () (e-board-drain-terminal-classifications board))))))

(defun e-board--queue-terminal-classification
    (board work-id &optional invocation-ids aggregation-ids)
  "Freeze indexed terminal candidates for WORK-ID and schedule their classifier."
  (let ((record (e-board-terminal-classification--create
                 :work-id work-id
                 :invocation-ids (copy-sequence
                                  (or invocation-ids
                                      (gethash work-id (e-board-invocation-work-index board))))
                 :aggregation-ids (copy-sequence
                                   (or aggregation-ids
                                       (gethash work-id (e-board-aggregation-work-index board))))
                 :invocation-index 0 :aggregation-index 0)))
    (setf (e-board-terminal-classifications board)
          (append (e-board-terminal-classifications board) (list record)))
    (e-board--schedule-terminal-classification board)))

(defun e-board-drain-terminal-classifications (board)
  "Classify one bounded page of frozen terminal subscription candidates."
  (setf (e-board-terminal-classification-scheduled board) nil)
  (let ((remaining e-board-terminal-classification-drain-limit))
    (while (and remaining (> remaining 0) (e-board-terminal-classifications board))
      (let* ((record (car (e-board-terminal-classifications board)))
             (work (e-board-observed-work board
                                          (e-board-terminal-classification-work-id record)))
             (state (and work (e-board-work-state work)))
             (payload (and work (e-board-work-terminal-payload work))))
        (cond
         ((< (e-board-terminal-classification-invocation-index record)
             (length (e-board-terminal-classification-invocation-ids record)))
          (let* ((index (e-board-terminal-classification-invocation-index record))
                 (id (nth index (e-board-terminal-classification-invocation-ids record))))
            (setf (e-board-terminal-classification-invocation-index record) (1+ index))
            (when-let ((invocation (e-board-invocation board id)))
              (e-board--settle-invocation board invocation state payload))))
         ((< (e-board-terminal-classification-aggregation-index record)
             (length (e-board-terminal-classification-aggregation-ids record)))
          (let* ((index (e-board-terminal-classification-aggregation-index record))
                 (id (nth index (e-board-terminal-classification-aggregation-ids record))))
            (setf (e-board-terminal-classification-aggregation-index record) (1+ index))
            (when-let ((aggregation (e-board-aggregation board id)))
              (when (and (eq (e-board-aggregation-state aggregation) 'open)
                         (e-board--aggregation-ready-p board aggregation))
                (e-board--settle-aggregation board aggregation 'complete)))))
         (t
          (setf (e-board-terminal-classifications board)
                (cdr (e-board-terminal-classifications board)))))
        (cl-decf remaining)))
    (when (e-board-terminal-classifications board)
      (e-board--schedule-terminal-classification board))))

(defun e-board--settle-invocation (board invocation state payload)
  "Commit INVOCATION's exact reply effect for terminal STATE and PAYLOAD."
  (when (eq (e-board-invocation-state invocation) 'open)
    (setf (e-board-invocation-state invocation) 'prepared)
    (let ((activation-id
           (list (e-board-id board) (e-board-invocation-id invocation) 1)))
      (setf (e-board-invocation-activation-id invocation) activation-id)
      (e-board--append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :work-id (e-board-invocation-work-id invocation)
             :effect 'reply-to-invocation))
      (e-board--schedule-effect
       board
       (lambda ()
         (when (eq (e-board-invocation-state invocation) 'prepared)
           (setf (e-board-invocation-state invocation) 'applying)
           (condition-case err
               (progn
                 (let ((dispatcher (e-board-invocation-effect-dispatcher board)))
                   (unless dispatcher
                     (signal 'e-board-error
                             (list "No invocation effect dispatcher" activation-id)))
                   (funcall dispatcher board
                            (e-board-invocation-effect-target invocation)
                            state payload))
                 (setf (e-board-invocation-state invocation) 'committed)
                 (e-board--append-event
                  board 'effect-committed
                  (list :activation-id activation-id
                        :effect 'reply-to-invocation)))
             (error
              (setf (e-board-invocation-state invocation) 'failed)
              (e-board--append-event
               board 'effect-failed
                (list :activation-id activation-id :error err))))))))))

(defun e-board--aggregation-ready-p (board aggregation)
  "Return non-nil when AGGREGATION's observed work has reached its policy."
  (let ((work-ids (e-board-aggregation-work-ids aggregation)))
    (pcase (e-board-aggregation-mode aggregation)
      ('all (cl-every (lambda (id)
                        (e-board-work-terminal-seq (e-board-observed-work board id)))
                      work-ids))
      ('any (cl-some (lambda (id)
                       (e-board-work-terminal-seq (e-board-observed-work board id)))
                     work-ids))
      (_ (signal 'e-board-error
                 (list "Unknown aggregation mode" (e-board-aggregation-mode aggregation)))))))

(defun e-board--settle-aggregation (board aggregation reason)
  "Commit AGGREGATION's deferred reply effect with terminal REASON."
  (when (eq (e-board-aggregation-state aggregation) 'open)
    (setf (e-board-aggregation-state aggregation) 'prepared)
    (when-let ((timer (e-board-aggregation-timer aggregation)))
      (cancel-timer timer)
      (setf (e-board-aggregation-timer aggregation) nil))
    (let ((activation-id
           (list (e-board-id board) (e-board-aggregation-id aggregation) 1)))
      (setf (e-board-aggregation-activation-id aggregation) activation-id)
      (e-board--append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :work-ids (copy-sequence (e-board-aggregation-work-ids aggregation))
             :effect 'reply-to-invocation))
      (e-board--schedule-effect
       board
       (lambda ()
         (when (eq (e-board-aggregation-state aggregation) 'prepared)
           (setf (e-board-aggregation-state aggregation) 'applying)
           (condition-case err
               (progn
                 (let ((dispatcher (e-board-invocation-effect-dispatcher board)))
                   (unless dispatcher
                     (signal 'e-board-error
                             (list "No invocation effect dispatcher" activation-id)))
                   (funcall dispatcher board
                            (e-board-aggregation-effect-target aggregation)
                            'aggregation reason))
                 (setf (e-board-aggregation-state aggregation) 'committed)
                 (e-board--append-event
                  board 'effect-committed
                  (list :activation-id activation-id :effect 'reply-to-invocation)))
             (error
              (setf (e-board-aggregation-state aggregation) 'failed)
              (e-board--append-event
               board 'effect-failed
               (list :activation-id activation-id :error err))))))))))

(cl-defun e-board-subscribe-aggregation
    (board work-ids mode effect-target &key id timeout)
  "Install an ordered work aggregation reply subscription on BOARD.
WORK-IDS must name currently observed work.  MODE is `all' or `any'.
EFFECT-TARGET remains opaque to the board and receives a later frozen reason
through the injected invocation effect dispatcher."
  (unless (and (listp work-ids) work-ids)
    (signal 'e-board-error (list "Aggregation requires at least one work id")))
  (unless (memq mode '(all any))
    (signal 'e-board-error (list "Unknown aggregation mode" mode)))
  (unless effect-target
    (signal 'e-board-error (list "Aggregation effect target is required")))
  (dolist (work-id work-ids)
    (unless (e-board-observed-work board work-id)
      (signal 'e-board-error (list "Unknown board work" work-id))))
  (let ((id (or id (e-board--next-id board 'invocation))))
    (when (e-board-aggregation board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((aggregation (e-board-aggregation--create
                        :id id :work-ids (copy-sequence work-ids) :mode mode
                        :state 'open :effect-target effect-target)))
      (puthash id aggregation (e-board-aggregations board))
      (dolist (work-id work-ids)
        (e-board--index-work-subscription (e-board-aggregation-work-index board)
                                          work-id id))
      (e-board--append-event
       board 'subscription-added
       (list :subscription-id id :work-ids (copy-sequence work-ids)
             :readiness (if (eq mode 'all) 'all-terminal 'first-terminal)
             :effect 'reply-to-invocation))
      (when timeout
        (setf (e-board-aggregation-timer aggregation)
              (run-at-time timeout nil
                           (lambda ()
                             (e-board--queue-aggregation-deadline
                              board (e-board-aggregation-id aggregation))))))
      (when (e-board--aggregation-ready-p board aggregation)
        ;; Keep an already-ready subscription on the same later classifier
        ;; path as a fresh terminal publication.
        (e-board--queue-terminal-classification
         board (car work-ids) nil (list id)))
      aggregation)))

(defun e-board-cancel-aggregation (board aggregation-id)
  "Cancel open AGGREGATION-ID on BOARD without affecting watched work."
  (when-let ((aggregation (e-board-aggregation board aggregation-id)))
    (when (eq (e-board-aggregation-state aggregation) 'open)
      (when-let ((timer (e-board-aggregation-timer aggregation)))
        (cancel-timer timer))
      (setf (e-board-aggregation-timer aggregation) nil
            (e-board-aggregation-state aggregation) 'cancelled)
      (e-board--append-event
       board 'subscription-cancelled
       (list :subscription-id aggregation-id))))
  t)

(defun e-board--observe-work-terminal (board work state payload)
  "Append WORK's terminal fact and queue its frozen indexed classifier."
  (unless (e-board-work-terminal-seq work)
    (let ((event (e-board--append-event
                  board state
                  (list :work-id (e-board-work-id work)
                        :state state :payload payload))))
      (setf (e-board-work-state work) state
            (e-board-work-terminal-seq work) (e-board-event-seq event)
            (e-board-work-terminal-payload work) payload)
      (e-board--queue-terminal-classification board (e-board-work-id work)))))

(cl-defun e-board-enroll-work (board handle &key metadata)
  "Enroll prepared HANDLE in BOARD before its runner may start.
The canonical work id is the handle id.  The dedicated observer is installed
before runner entry so synchronous carriers cannot settle outside the log."
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (when (e-work-handle-started-p handle)
    (signal 'e-board-error (list "Cannot enroll started work" handle)))
  (let ((id (e-work-handle-id handle)))
    (when (e-board-observed-work board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((work (e-board-work--create
                 :id id :handle handle :metadata (copy-tree metadata)
                 :state 'posted)))
      (puthash id work (e-board-work-table board))
      (e-board--append-event board 'posted
                             (list :work-id id :metadata (copy-tree metadata)))
      (e-work-install-publication-observer
       handle
       (lambda (_handle state payload)
         (e-board--observe-work-terminal board work state payload)))
      work)))

(cl-defun e-board-subscribe-invocation (board work-id effect-target &key id)
  "Install one exact reply relation for BOARD WORK-ID.
EFFECT-TARGET is an opaque exact invocation identity owned by the runtime
effect adapter.  The board never retains or invokes a loop callback; it only
commits the terminal event, then asks its injected dispatcher to apply this
target after the start stack unwinds."
  (unless effect-target
    (signal 'e-board-error (list "Invocation effect target is required")))
  (unless (e-board-observed-work board work-id)
    (signal 'e-board-error (list "Unknown board work" work-id)))
  (let ((id (or id (e-board--next-id board 'invocation))))
    (when (e-board-invocation board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((invocation (e-board-invocation--create
                       :id id :work-id work-id :state 'open
                       :effect-target effect-target)))
      (puthash id invocation (e-board-invocations board))
      (e-board--index-work-subscription (e-board-invocation-work-index board)
                                        work-id id)
      (e-board--append-event board 'subscription-added
                             (list :subscription-id id :work-id work-id
                                   :effect 'reply-to-invocation))
      ;; Enrolling and subscribing can be separated by a caller transaction.
      ;; If an already-terminal handle is intentionally subscribed, publish one
      ;; frozen activation without scanning unrelated history.
      (let ((work (e-board-observed-work board work-id)))
        (when (e-board-work-terminal-seq work)
          (e-board--queue-terminal-classification board work-id (list id))))
      invocation)))

(cl-defun e-board-enroll-invocation-work
    (board handle invocation-id effect-target &key metadata)
  "Atomically enroll prepared HANDLE and its exact INVOCATION-ID relation.
EFFECT-TARGET is owned by an injected runtime invocation service.  This
convenience keeps required pre-run ordering at one application boundary without
making `e-work' depend on board state or making the board retain loop closures."
  ;; Validate every relation that can reject before `e-board-enroll-work'
  ;; appends the operation record.  A cheap runner may settle immediately, so
  ;; callers must never have to roll a visible enrollment back afterwards.
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (when (e-work-handle-started-p handle)
    (signal 'e-board-error (list "Cannot enroll started work" handle)))
  (when (e-board-observed-work board (e-work-handle-id handle))
    (signal 'e-board-id-conflict (list (e-work-handle-id handle))))
  (unless effect-target
    (signal 'e-board-error (list "Invocation effect target is required")))
  (when (e-board-invocation board invocation-id)
    (signal 'e-board-id-conflict (list invocation-id)))
  (e-board-enroll-work board handle :metadata metadata)
  (e-board-subscribe-invocation board (e-work-handle-id handle) effect-target
                                 :id invocation-id)
  handle)

(defun e-board--active-participant-p (participant)
  "Return non-nil when PARTICIPANT can receive a new pickup."
  (memq (e-board-participant-state participant) '(active dormant stale)))

(cl-defun e-board-add-participant
    (board &key id (state 'active) create-pickup-subscription-id)
  "Add participant ID to BOARD and install its built-in exact pickup route.
The identity subscription is membership-owned: ordinary subscriptions cannot
replace it, and exact input ignores descriptive tags and other subscriptions."
  (let* ((id (or id (e-board--next-id board 'participant)))
         (subscription-id
          (or create-pickup-subscription-id
              (e-board--next-id board 'subscription))))
    (e-board--require-id id 'e-board-participant-id)
    (when (e-board-participant board id)
      (signal 'e-board-id-conflict (list id)))
    (when (cl-find subscription-id (e-board-subscriptions board)
                   :key #'e-board-subscription-id :test #'equal)
      (signal 'e-board-id-conflict (list subscription-id)))
    (let ((participant
           (e-board-participant--create
            :id id :board-id (e-board-id board) :state state
            :create-pickup-subscription-id subscription-id)))
      (puthash id participant (e-board-participants board))
      (setf (e-board-subscriptions board)
            (append (e-board-subscriptions board)
                    (list (e-board-subscription--create
                           :id subscription-id
                           :board-id (e-board-id board)
                           :participant-id id
                           :selector (list :to id)
                           :effect 'create-pickup
                           :state 'active
                           :built-in-p t))))
      (e-board--append-event board 'participant-added
                             (list :participant-id id
                                   :subscription-id subscription-id))
      participant)))

(cl-defun e-board-subscribe
    (board participant-id selector &key id (state 'active) (effect 'create-pickup))
  "Install an ordinary immutable input subscription for PARTICIPANT-ID.
SELECTOR supports `:to', `:tags', `:tags-all', and `:tags-any'.  Slice 1's
   effects are `create-pickup' and declarative `(:post-input ...)'.  New
 subscriptions only inspect future inputs."
  (unless (e-board-participant board participant-id)
    (signal 'e-board-error (list "Unknown participant" participant-id)))
  (unless (or (eq effect 'create-pickup)
              (and (listp effect) (eq (car effect) :post-input)))
    (signal 'e-board-error (list "Unsupported board effect" effect)))
  (unless (listp selector)
    (signal 'wrong-type-argument (list 'listp selector)))
  (unless (memq state '(active muted))
    (signal 'wrong-type-argument (list '(member active muted) state)))
  (let ((id (or id (e-board--next-id board 'subscription))))
    (when (cl-find id (e-board-subscriptions board)
                   :key #'e-board-subscription-id :test #'equal)
      (signal 'e-board-id-conflict (list id)))
    (let ((subscription
           (e-board-subscription--create
            :id id :board-id (e-board-id board)
            :participant-id participant-id
            ;; The matcher is immutable even if the caller later mutates its plist.
            :selector (copy-tree selector)
            :effect effect :state state :built-in-p nil)))
      (setf (e-board-subscriptions board)
            (append (e-board-subscriptions board) (list subscription)))
      (e-board--append-event board 'subscription-added
                             (list :subscription-id id
                                   :participant-id participant-id))
      subscription)))

(defun e-board--tags-match-p (selector message)
  "Return non-nil when SELECTOR's tag clauses match MESSAGE."
  (let ((tags (e-board-message-tags message))
        (all (or (plist-get selector :tags-all)
                 (plist-get selector :tags)))
        (any (plist-get selector :tags-any)))
    (and (cl-every (lambda (tag) (member tag tags)) all)
         (or (null any) (cl-some (lambda (tag) (member tag tags)) any)))))

(defun e-board--selector-attributes-match-p (selector message)
  "Return non-nil when SELECTOR's bounded attribute clauses match MESSAGE."
  (cl-every (lambda (pair)
              (equal (plist-get (e-board-message-attributes message) (car pair))
                     (cdr pair)))
            (let ((attributes (plist-get selector :attributes)))
              (cond ((null attributes) nil)
                    ((and (listp attributes) (keywordp (car attributes)))
                     (cl-loop for (key value) on attributes by #'cddr
                              collect (cons key value)))
                    ((listp attributes) attributes)
                    (t (signal 'wrong-type-argument
                               (list 'listp attributes)))))))

(defun e-board--fault-subscription (board subscription err)
  "Record trusted predicate ERR without reviving a changed subscription view."
  (setf (e-board-subscription-state subscription) 'faulted)
  (when-let ((current (e-board-find-subscription
                       board (e-board-subscription-id subscription))))
    (when (eq (e-board-subscription-state current) 'active)
      (e-board--transition-subscription board current 'faulted)
      (e-board--append-event
       board 'subscription-faulted
       (list :subscription-id (e-board-subscription-id current)
             :error err)))))

(defun e-board--selector-matches-p (board subscription message)
  "Return non-nil when SUBSCRIPTION's immutable selector matches MESSAGE."
  (let ((selector (e-board-subscription-selector subscription)))
    (and (eq (e-board-message-kind message) 'input)
          (or (not (plist-member selector :to))
              (equal (plist-get selector :to) (e-board-message-to message)))
          (or (not (plist-member selector :author))
              (equal (plist-get selector :author) (e-board-message-author message)))
          (e-board--selector-attributes-match-p selector message)
          (e-board--tags-match-p selector message)
          (if-let ((predicate (plist-get selector :predicate)))
              (condition-case err
                  (funcall predicate message)
                (error
                 (e-board--fault-subscription board subscription err)
                 nil))
            t))))

(cl-defun e-board-observer-subscribe
    (board client-id selector &key id (state 'active) (start-seq 0))
  "Create an effect-free client observer cursor over BOARD's message sequence.
Observers deliberately share selector fields with participant subscriptions,
but they cannot activate effects, create pickups, alter routedness, or consume
messages.  START-SEQ is an explicit retained-history cursor; live callers use
the returned cursor's advancing `next-seq' for later bounded pages."
  (unless (listp selector)
    (signal 'wrong-type-argument (list 'listp selector)))
  (unless (memq state e-board--observer-states)
    (signal 'wrong-type-argument
            (list e-board--observer-states state)))
  (unless (and (integerp start-seq) (>= start-seq 0))
    (signal 'wrong-type-argument (list 'natnump start-seq)))
  (let ((id (or id (e-board--next-id board 'observer))))
    (when (e-board-observer board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((observer (e-board-observer--create
                     :id id :board-id (e-board-id board) :client-id client-id
                     :selector (copy-tree selector) :state state
                     :next-seq start-seq)))
      (puthash id observer (e-board-observers board))
      (e-board--append-event board 'observer-added
                             (list :observer-id id :client-id client-id
                                   :start-seq start-seq))
      observer)))

(defun e-board--observer-matches-p (board observer message)
  "Return non-nil when OBSERVER can observe MESSAGE, faulting only itself."
  (let ((selector (e-board-observer-selector observer)))
    (and (or (not (plist-member selector :kind))
             (equal (plist-get selector :kind) (e-board-message-kind message)))
         (or (not (plist-member selector :to))
             (equal (plist-get selector :to) (e-board-message-to message)))
         (or (not (plist-member selector :author))
             (equal (plist-get selector :author) (e-board-message-author message)))
         (or (not (plist-member selector :subject-participant-id))
             (equal (plist-get selector :subject-participant-id)
                    (e-board-message-subject-participant-id message)))
         (e-board--selector-attributes-match-p selector message)
         (e-board--tags-match-p selector message)
         (if-let ((predicate (plist-get selector :predicate)))
             (condition-case err
                 (funcall predicate message)
               (error
                (e-board--fault-observer board observer err)
                nil))
           t))))

(defun e-board--observer-transition-allowed-p (from to)
  "Return non-nil when observer state FROM may transition to TO."
  (pcase from
    ('active (memq to '(muted faulted cancelled expired)))
    ('muted (memq to '(active cancelled expired)))
    (_ nil)))

(defun e-board--transition-observer (board observer state)
  "Commit OBSERVER's effect-free lifecycle transition to STATE on BOARD."
  (let ((from (e-board-observer-state observer)))
    (unless (memq state e-board--observer-states)
      (signal 'wrong-type-argument (list e-board--observer-states state)))
    (unless (e-board--observer-transition-allowed-p from state)
      (signal 'e-board-error
              (list "Illegal observer transition" from state
                    (e-board-observer-id observer))))
    (setf (e-board-observer-state observer) state)
    (e-board--append-event board 'observer-transition
                           (list :observer-id (e-board-observer-id observer)
                                 :from from :state state))
    observer))

(defun e-board--fault-observer (board observer err)
  "Record trusted observer predicate ERR without reviving a replaced cursor."
  (when (eq (e-board-observer-state observer) 'active)
    (e-board--transition-observer board observer 'faulted)
    (e-board--append-event
     board 'observer-faulted
     (list :observer-id (e-board-observer-id observer) :error err))))

(defun e-board-set-observer-state (board observer-id state)
  "Transition effect-free OBSERVER-ID to STATE on BOARD."
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (e-board--transition-observer board observer state)))

(cl-defun e-board-replace-observer
    (board observer-id selector &key id (state 'active) start-seq)
  "Cancel OBSERVER-ID and install a fresh client-local observer cursor.
Omitted START-SEQ keeps replacement future-only at the old cursor; callers
request retained backfill explicitly with a lower START-SEQ."
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (unless (listp selector)
      (signal 'wrong-type-argument (list 'listp selector)))
    (unless (memq state '(active muted))
      (signal 'wrong-type-argument (list '(member active muted) state)))
    (let ((replacement-id (or id (e-board--next-id board 'observer))))
      (when (e-board-observer board replacement-id)
        (signal 'e-board-id-conflict (list replacement-id)))
      (unless (memq (e-board-observer-state observer)
                    '(faulted cancelled expired))
        (e-board--transition-observer board observer 'cancelled))
      (let ((replacement
             (e-board-observer-subscribe
              board (e-board-observer-client-id observer) selector
              :id replacement-id :state state
              :start-seq (or start-seq (e-board-observer-next-seq observer)))))
        (e-board--append-event board 'observer-replaced
                               (list :observer-id observer-id
                                     :replacement-id replacement-id))
        replacement))))

(cl-defun e-board-observer-read-page (board observer-id &key (limit 32))
  "Advance OBSERVER-ID through at most LIMIT records and return matches.
This is a non-consuming read cursor.  The caller chooses scheduling/paging;
the board mutates only the cursor after each inspected record, so filtered gaps
are preserved and retrying a later page cannot produce an earlier record.
Trusted predicates run only here, never from message append or work settlement."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'plusp limit)))
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id))))
        (inspected 0)
        matches)
    (when (eq (e-board-observer-state observer) 'active)
      (dolist (message (e-board-messages board))
        (when (and (< inspected limit)
                   (> (e-board-message-seq message)
                      (e-board-observer-next-seq observer)))
          (cl-incf inspected)
          (setf (e-board-observer-next-seq observer)
                (e-board-message-seq message))
          (when (e-board--observer-matches-p board observer message)
            (push message matches)))))
    (nreverse matches)))

(defun e-board--eligible-subscription-p (board subscription)
  "Return non-nil when SUBSCRIPTION is active and its participant can receive."
  (and (eq (e-board-subscription-state subscription) 'active)
       (eq (e-board-subscription-effect subscription) 'create-pickup)
       (when-let ((participant
                   (e-board-participant board
                                        (e-board-subscription-participant-id
                                         subscription))))
          (e-board--active-participant-p participant))))

(defun e-board-find-subscription (board subscription-id)
  "Return BOARD's subscription SUBSCRIPTION-ID, or nil."
  (cl-find subscription-id (e-board-subscriptions board)
           :key #'e-board-subscription-id :test #'equal))

(defun e-board--subscription-transition-allowed-p (from to)
  "Return non-nil when ordinary subscription state FROM may move to TO."
  (pcase from
    ('active (memq to '(muted completed faulted cancelled expired)))
    ('muted (memq to '(active cancelled expired)))
    (_ nil)))

(defun e-board--transition-subscription (board subscription state)
  "Commit SUBSCRIPTION's ordinary lifecycle transition to STATE on BOARD."
  (let ((from (e-board-subscription-state subscription)))
    (unless (memq state e-board--ordinary-subscription-states)
      (signal 'wrong-type-argument
              (list e-board--ordinary-subscription-states state)))
    (unless (e-board--subscription-transition-allowed-p from state)
      (signal 'e-board-error
              (list "Illegal subscription transition" from state
                    (e-board-subscription-id subscription))))
    (setf (e-board-subscription-state subscription) state)
    (e-board--append-event board 'subscription-transition
                           (list :subscription-id (e-board-subscription-id subscription)
                                 :from from :state state))
    subscription))

(defun e-board-set-subscription-state (board subscription-id state)
  "Transition an ordinary BOARD subscription to STATE.
The membership-owned exact address route is not mutable through this API; its
lifetime belongs to participant membership."
  (let ((subscription (e-board-find-subscription board subscription-id)))
    (unless subscription
      (signal 'e-board-error (list "Unknown subscription" subscription-id)))
    (when (e-board-subscription-built-in-p subscription)
      (signal 'e-board-error (list "Membership-owned subscription" subscription-id)))
    (e-board--transition-subscription board subscription state)))

(cl-defun e-board-replace-subscription
    (board subscription-id selector &key id (effect nil effect-supplied-p)
           (state 'active))
  "Cancel ordinary SUBSCRIPTION-ID and install a future-only replacement.
The replacement receives a fresh id by default, so captured classifier views
continue to name the old immutable subscription.  Replacing a terminal
subscription records the relationship without rewriting its terminal state."
  (let ((subscription (or (e-board-find-subscription board subscription-id)
                          (signal 'e-board-error
                                  (list "Unknown subscription" subscription-id)))))
    (when (e-board-subscription-built-in-p subscription)
      (signal 'e-board-error (list "Membership-owned subscription" subscription-id)))
    (let ((replacement-id (or id (e-board--next-id board 'subscription))))
      (when (e-board-find-subscription board replacement-id)
        (signal 'e-board-id-conflict (list replacement-id)))
      ;; Validate the replacement before changing the old subscription.
      (unless (listp selector)
        (signal 'wrong-type-argument (list 'listp selector)))
      (unless (memq state '(active muted))
        (signal 'wrong-type-argument (list '(member active muted) state)))
      (let ((replacement-effect
             (if effect-supplied-p effect (e-board-subscription-effect subscription))))
        (unless (or (eq replacement-effect 'create-pickup)
                    (and (listp replacement-effect)
                         (eq (car replacement-effect) :post-input)))
          (signal 'e-board-error
                  (list "Unsupported board effect" replacement-effect)))
        (unless (memq (e-board-subscription-state subscription)
                      '(completed faulted cancelled expired))
          (e-board--transition-subscription board subscription 'cancelled))
        (let ((replacement
               (e-board-subscribe
                board (e-board-subscription-participant-id subscription) selector
                :id replacement-id :state state :effect replacement-effect)))
          (e-board--append-event
           board 'subscription-replaced
           (list :subscription-id subscription-id
                 :replacement-id replacement-id))
          replacement)))))

(defun e-board--matching-subscriptions (board message)
  "Return eligible subscriptions for MESSAGE using its exact/tag route rule."
  (if-let ((to (e-board-message-to message)))
      ;; Addressed input deliberately bypasses ordinary subscriptions and tags.
      (cl-remove-if-not
       (lambda (subscription)
         (and (e-board-subscription-built-in-p subscription)
              (equal (e-board-subscription-participant-id subscription) to)
              (e-board--eligible-subscription-p board subscription)))
       (e-board-subscriptions board))
    (cl-remove-if-not
     (lambda (subscription)
       (and (not (e-board-subscription-built-in-p subscription))
            (e-board--eligible-subscription-p board subscription)
             (e-board--selector-matches-p board subscription message)))
      (e-board-subscriptions board))))

(defun e-board--post-input-subscriptions (board message)
  "Return ordinary post-input subscriptions eligible for MESSAGE.
Derived messages cannot activate a subscription already in their lineage.
This gives post effects a bounded, visible cycle stop without special routing."
  (unless (e-board-message-to message)
    (let* ((attributes (e-board-message-attributes message))
           (lineage (plist-get attributes :board-subscription-lineage)))
      (cl-remove-if-not
       (lambda (subscription)
         (and (eq (e-board-subscription-state subscription) 'active)
              (not (e-board-subscription-built-in-p subscription))
              (listp (e-board-subscription-effect subscription))
              (eq (car (e-board-subscription-effect subscription)) :post-input)
              (not (member (e-board-subscription-id subscription) lineage))
               (e-board--selector-matches-p board subscription message)))
       (e-board-subscriptions board)))))

(defun e-board--schedule-post-input (board subscription message)
  "Freeze and schedule SUBSCRIPTION's declarative post from MESSAGE."
  (let* ((effect (cdr (e-board-subscription-effect subscription)))
         (attributes (copy-tree (plist-get effect :attributes)))
         (lineage (append (copy-sequence
                           (plist-get (e-board-message-attributes message)
                                      :board-subscription-lineage))
                          (list (e-board-subscription-id subscription))))
         (activation-id (list (e-board-id board) (e-board-subscription-id subscription)
                              (e-board-message-id message))))
    (if (> (length lineage) e-board-max-derived-hops)
        (e-board--append-event
         board 'effect-stopped
         (list :activation-id activation-id :reason 'causal-hop-limit))
      (e-board--append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :subscription-id (e-board-subscription-id subscription)
             :effect 'post-input))
      (e-board--schedule-effect
       board
       (lambda ()
         (condition-case err
             (let ((publication
                    (e-board-post-input
                     board
                     :author (or (plist-get effect :author)
                                 (format "participant:%s"
                                         (e-board-subscription-participant-id subscription)))
                     :tags (copy-tree (plist-get effect :tags))
                     :attributes
                     (append attributes
                             (list :board-subscription-lineage lineage))
                     :to (plist-get effect :to)
                     :mode (or (plist-get effect :mode) 'inject)
                     :content (plist-get effect :content)
                     :reference (plist-get effect :reference)
                     :source-input-key
                     (list (e-board-subscription-id subscription) 1
                           (e-board-message-seq message)))))
               (e-board--append-event
                board 'effect-committed
                (list :activation-id activation-id :effect 'post-input
                      :message-id (and (e-board-publication-message publication)
                                       (e-board-message-id
                                        (e-board-publication-message publication))))))
           (error
            (e-board--append-event
             board 'effect-failed
             (list :activation-id activation-id :error err)))))))))

(defun e-board--source-key-parts (source-key)
  "Return SOURCE-KEY as (PRODUCER GENERATION SEQ), or signal.
Source identities are board-scoped producer/generation/monotonic-sequence
tuples.  Lists and vectors are accepted to keep adapters representation-neutral."
  (let ((parts (cond ((listp source-key) source-key)
                     ((vectorp source-key) (append source-key nil)))))
    (unless (and (= (length parts) 3)
                 (nth 0 parts) (nth 1 parts)
                 (integerp (nth 2 parts)) (>= (nth 2 parts) 0))
      (signal 'e-board-invalid-source-key (list source-key)))
    parts))

(defun e-board--source-publication (board kind source-key)
  "Return existing or expired publication status for BOARD KIND SOURCE-KEY.
Return nil when the key is new and may be appended."
  (when source-key
    (pcase-let* ((`(,producer ,generation ,sequence)
                  (e-board--source-key-parts source-key))
                 (recent-key (list kind producer generation sequence))
                 (watermark-key (list kind producer generation))
                 (existing (gethash recent-key (e-board-source-recent board)))
                 (watermark (gethash watermark-key
                                     (e-board-source-high-watermarks board))))
      (cond
       (existing
        (e-board-publication--create
         :status 'duplicate :message existing
         :pickup-ids (e-board-message-pickup-ids existing)))
       ((and watermark (<= sequence watermark))
        (e-board-publication--create :status 'source-history-expired))
       (t nil)))))

(defun e-board--remember-source (board kind source-key message)
  "Atomically retain SOURCE-KEY's MESSAGE and advance its high watermark."
  (when source-key
    (pcase-let ((`(,producer ,generation ,sequence)
                 (e-board--source-key-parts source-key)))
      (puthash (list kind producer generation sequence) message
               (e-board-source-recent board))
      (puthash (list kind producer generation) sequence
               (e-board-source-high-watermarks board)))))

(defun e-board--make-message (board kind id author tags attributes to mode content reference
                                     source-input-key source-output-key
                                     reply-to-message-ids caused-by-delivery-ids
                                     &optional source-activity-key source-fact-key
                                     subject-participant-id source-turn-id activity-kind)
  "Create and record one immutable BOARD message, returning it."
  (when (e-board-message board id)
    (signal 'e-board-id-conflict (list id)))
  (let* ((event (e-board--append-event
                 board (intern (format "%s-posted" kind))
                 (list :message-id id)))
         (message
          (e-board-message--create
           :id id :board-id (e-board-id board) :seq (e-board-event-seq event)
            :kind kind :author author :tags (copy-tree tags)
            :attributes (copy-tree attributes) :to to :mode mode
           :content content :reference reference
           :source-input-key (copy-tree source-input-key)
           :source-output-key (copy-tree source-output-key)
           :reply-to-message-ids (copy-tree reply-to-message-ids)
           :caused-by-delivery-ids (copy-tree caused-by-delivery-ids)
           :source-activity-key (copy-tree source-activity-key)
           :source-fact-key (copy-tree source-fact-key)
           :subject-participant-id subject-participant-id
           :source-turn-id source-turn-id
           :activity-kind activity-kind
           :routing-state (and (eq kind 'input) 'routing))))
    (puthash id message (e-board-message-table board))
    (setf (e-board-messages board)
          (append (e-board-messages board) (list message)))
    message))

(defun e-board--input-subscription-matches-p (board subscription message)
  "Classify one frozen SUBSCRIPTION against MESSAGE in a later router turn."
  (let ((matches
         (cond
          ((eq (e-board-subscription-effect subscription) 'create-pickup)
           (and (e-board--eligible-subscription-p board subscription)
                (if-let ((to (e-board-message-to message)))
                    (and (e-board-subscription-built-in-p subscription)
                         (equal (e-board-subscription-participant-id subscription) to))
                  (and (not (e-board-subscription-built-in-p subscription))
                       (e-board--selector-matches-p board subscription message)))))
          ((and (not (e-board-message-to message))
                (eq (e-board-subscription-state subscription) 'active)
                (not (e-board-subscription-built-in-p subscription))
                (listp (e-board-subscription-effect subscription))
                (eq (car (e-board-subscription-effect subscription)) :post-input))
           (let ((lineage (plist-get (e-board-message-attributes message)
                                     :board-subscription-lineage)))
             (and (not (member (e-board-subscription-id subscription) lineage))
                  (e-board--selector-matches-p board subscription message)))))))
    matches))

(defun e-board--finalize-input-classification
    (board message publication subscriptions post-subscriptions)
  "Commit MESSAGE's complete pickup fan-out from frozen SUBSCRIPTIONS."
  (let ((subscriptions (nreverse subscriptions))
        (post-subscriptions (nreverse post-subscriptions))
        (by-participant (make-hash-table :test 'equal))
        participant-ids pickup-ids)
    ;; Group before allocating pickups so duplicate subscriptions cannot fan out.
    (dolist (subscription subscriptions)
      (let ((participant-id (e-board-subscription-participant-id subscription)))
        (puthash participant-id
                 (append (gethash participant-id by-participant)
                         (list (e-board-subscription-id subscription)))
                 by-participant)
        (unless (member participant-id participant-ids)
          (setq participant-ids (append participant-ids (list participant-id))))))
    (if (null participant-ids)
        (let ((reason (if (e-board-message-to message)
                          'target-unavailable
                        'no-matching-subscription)))
          (setf (e-board-message-unrouted-reason message) reason
                (e-board-message-routing-state message) 'unrouted)
          (e-board--append-event board 'input-unrouted
                                 (list :message-id (e-board-message-id message)
                                       :reason reason)))
      (dolist (participant-id participant-ids)
        (let* ((delivery-id (list (e-board-id board)
                                  (e-board-message-id message)
                                  participant-id))
               (pickup
                (e-board-pickup--create
                 :delivery-id delivery-id :board-id (e-board-id board)
                 :message-id (e-board-message-id message)
                 :participant-id participant-id
                 :subscription-ids (gethash participant-id by-participant)
                 :mode (e-board-message-mode message) :state 'pending)))
          (puthash delivery-id pickup (e-board-pickups board))
           (setq pickup-ids (append pickup-ids (list delivery-id)))))
       (setf (e-board-message-matching-participant-ids message) participant-ids
             (e-board-message-pickup-ids message) pickup-ids
             (e-board-message-routing-state message) 'routed)
        (e-board--append-event board 'input-routed
                               (list :message-id (e-board-message-id message)
                                     :participant-ids participant-ids
                                     :pickup-ids pickup-ids)))
    (dolist (subscription post-subscriptions)
      (e-board--schedule-post-input board subscription message))
    (setf (e-board-routed-pickup-results board)
          (append (e-board-routed-pickup-results board)
                  (list (list (e-board-message-id message) pickup-ids)))
          (e-board-publication-pickup-ids publication) pickup-ids)))

(defun e-board--schedule-input-classification (board)
  "Schedule BOARD's frozen input classifier once after append returns."
  (unless (e-board-input-classification-scheduled board)
    (setf (e-board-input-classification-scheduled board) t)
    (if-let ((scheduler (e-board-input-classification-scheduler board)))
        (funcall scheduler (lambda () (e-board-drain-input-classifications board)))
      (run-at-time 0 nil (lambda () (e-board-drain-input-classifications board))))))

(defun e-board--queue-input-classification (board message publication)
  "Freeze BOARD's subscription view for MESSAGE without matching on append."
  (let ((subscriptions
         (mapcar #'copy-e-board-subscription (e-board-subscriptions board))))
    (setf (e-board-input-classifications board)
          (append (e-board-input-classifications board)
                  (list (e-board-input-classification--create
                         :message message :subscriptions subscriptions :index 0
                         :publication publication
                         :matches nil :post-subscriptions nil :post-index 0))))
    (e-board--schedule-input-classification board)))

(defun e-board-drain-input-classifications (board)
  "Classify a bounded page of frozen input subscriptions in board order."
  (setf (e-board-input-classification-scheduled board) nil)
  (let ((remaining e-board-input-classification-drain-limit))
    (while (and (> remaining 0) (e-board-input-classifications board))
      (let* ((record (car (e-board-input-classifications board)))
             (subscriptions (e-board-input-classification-subscriptions record))
             (index (e-board-input-classification-index record))
             (message (e-board-input-classification-message record)))
        (if (< index (length subscriptions))
            (let ((subscription (nth index subscriptions)))
              (setf (e-board-input-classification-index record) (1+ index))
              (when (e-board--input-subscription-matches-p board subscription message)
                (if (eq (e-board-subscription-effect subscription) 'create-pickup)
                    (push subscription (e-board-input-classification-matches record))
                  (push subscription
                        (e-board-input-classification-post-subscriptions record)))))
          (e-board--finalize-input-classification
           board message (e-board-input-classification-publication record)
           (e-board-input-classification-matches record)
           (e-board-input-classification-post-subscriptions record))
          (setf (e-board-input-classifications board)
                (cdr (e-board-input-classifications board))))
        (cl-decf remaining)))
    (when (e-board-input-classifications board)
      (e-board--schedule-input-classification board))))

(defun e-board-drain-routed-pickups (board)
  "Return and clear finalized pickup ids for separately scheduled delivery."
  (prog1 (e-board-routed-pickup-results board)
    (setf (e-board-routed-pickup-results board) nil)))

(cl-defun e-board-post-input
    (board &key id author tags attributes to (mode 'inject) content reference source-input-key)
  "Append one input message and queue its routing, returning a publication.
With TO, only its participant's built-in address subscription is considered.
Without TO, active ordinary tag subscriptions receive one frozen pickup each.
SOURCE-INPUT-KEY retries return the existing message; old or out-of-order keys
return status `source-history-expired' without appending or routing again."
  (unless (memq mode '(inject queue))
    (signal 'wrong-type-argument (list '(member inject queue) mode)))
  (or (e-board--source-publication board 'input source-input-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message (e-board--make-message
                        board 'input id author tags attributes to mode content reference
                       source-input-key nil nil nil))
             (publication (e-board-publication--create
                           :status 'posted :message message :pickup-ids nil)))
        (e-board--remember-source board 'input source-input-key message)
        (e-board--queue-input-classification board message publication)
        publication)))

(cl-defun e-board-post-output
    (board &key id author tags content reference source-output-key
           subject-participant-id source-turn-id
           reply-to-message-ids caused-by-delivery-ids)
  "Append one non-routable output message and return an `e-board-publication'.
SOURCE-OUTPUT-KEY is required because output publication retries must be
at-most-once.  Outputs never create participant pickups."
  (unless source-output-key
    (signal 'e-board-invalid-source-key (list source-output-key)))
  (when (or subject-participant-id source-turn-id)
    (unless (and (stringp subject-participant-id)
                 (equal author (format "participant:%s" subject-participant-id))
                 source-turn-id)
      (signal 'e-board-invalid-activity
              (list :author author :subject-participant-id subject-participant-id
                    :source-turn-id source-turn-id))))
  (or (e-board--source-publication board 'output source-output-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message (e-board--make-message
                        board 'output id author tags nil nil nil content reference nil
                       source-output-key reply-to-message-ids
                       caused-by-delivery-ids nil nil subject-participant-id
                       source-turn-id nil)))
        (e-board--remember-source board 'output source-output-key message)
        (e-board-publication--create :status 'posted :message message
                                     :pickup-ids nil))))

(cl-defun e-board-post-activity
    (board &key id author subject-participant-id source-turn-id activity-kind
           tags attributes content reference source-activity-key
           reply-to-message-ids caused-by-delivery-ids)
  "Append one source-keyed, observation-only participant activity message.
Activity is never pickup-eligible: a later continuation may react to it, but
an activity tag by itself cannot re-enter a participant inbox."
  (unless source-activity-key
    (signal 'e-board-invalid-source-key (list source-activity-key)))
  (unless (and (stringp subject-participant-id)
               (equal author (format "participant:%s" subject-participant-id))
               source-turn-id activity-kind)
    (signal 'e-board-invalid-activity
            (list :author author :subject-participant-id subject-participant-id
                  :source-turn-id source-turn-id :activity-kind activity-kind)))
  (or (e-board--source-publication board 'activity source-activity-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message
              (e-board--make-message
               board 'activity id author tags attributes nil nil content reference
               nil nil reply-to-message-ids caused-by-delivery-ids
               source-activity-key nil subject-participant-id source-turn-id
               activity-kind)))
        (e-board--remember-source board 'activity source-activity-key message)
        (e-board-publication--create :status 'posted :message message
                                     :pickup-ids nil))))

(cl-defun e-board-post-fact
    (board &key id author tags attributes content reference source-fact-key)
  "Append one source-keyed, observation-only board fact message."
  (unless source-fact-key
    (signal 'e-board-invalid-source-key (list source-fact-key)))
  (or (e-board--source-publication board 'fact source-fact-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message
              (e-board--make-message
               board 'fact id author tags attributes nil nil content reference
               nil nil nil nil nil source-fact-key nil nil nil)))
        (e-board--remember-source board 'fact source-fact-key message)
        (e-board-publication--create :status 'posted :message message
                                     :pickup-ids nil))))

(defun e-board-unrouted-inputs (board)
  "Return retained BOARD input messages that have a visible unrouted reason."
  (cl-remove-if-not
   (lambda (message)
     (and (eq (e-board-message-kind message) 'input)
          (e-board-message-unrouted-reason message)))
   (e-board-messages board)))

(provide 'e-board)

;;; e-board.el ends here

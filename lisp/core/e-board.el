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
(define-error 'e-board-envelope-too-large
  "Board message envelope field exceeds its byte budget" 'e-board-error)
(define-error 'e-board-invalid-envelope
  "Board message envelope contains unsupported mutable structure" 'e-board-error)

(defvar e-board--id-sequence 0
  "Process-local fallback sequence for board identities.")

(defvar e-board--registry (make-hash-table :test 'equal)
  "Live process-local boards keyed by board id.")

(cl-defstruct (e-board-message
               (:constructor e-board-message--create)
               (:conc-name e-board-message-))
  id board-id seq kind author requester-actor tags attributes to mode content reference
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
  id board-id participant-id selector effect state built-in-p
  readiness accumulator readiness-timer readiness-generation firing-number
  firing-limit lifetime lifetime-timer lifetime-generation)

(defconst e-board--ordinary-subscription-states
  '(active muted completed faulted cancelled expired)
  "States available to ordinary orchestration subscriptions.")

(defconst e-board--observer-states
  '(active muted faulted cancelled expired)
  "States available to effect-free board observers.")

(cl-defstruct (e-board-observer
               (:constructor e-board-observer--create)
               (:conc-name e-board-observer-))
  id board-id client-id client-generation selector state next-seq next-index
  history-before-seq history-before-index history-floor
  prepared-page last-accepted-page-receipt
  prepared-history-page last-accepted-history-receipt)

(defconst e-board-max-derived-hops 8
  "Maximum subscription lineage depth for derived board inputs.")

(defconst e-board-terminal-classification-drain-limit 16
  "Maximum indexed terminal subscription clauses classified per drain.")

(defconst e-board-input-classification-drain-limit 32
  "Maximum frozen input subscription clauses classified per drain.")

(defconst e-board-subscription-replay-drain-limit 32
  "Maximum retained records classified for explicit continuation replay.")

(defconst e-board-aggregation-deadline-drain-limit 16
  "Maximum aggregation deadline transitions committed per drain.")

(defconst e-board-effect-drain-limit 16
  "Maximum frozen effects applied in one scheduler turn.")

(defconst e-board-default-pickup-pending-limit 16
  "Maximum FIFO pickups allowed behind one participant's active head.")

(defconst e-board-message-content-byte-limit (* 256 1024)
  "Maximum retained byte budget for one board message content value.")

(defconst e-board-message-reference-byte-limit (* 64 1024)
  "Maximum retained byte budget for one board message reference value.")

(defconst e-board-message-attributes-byte-limit (* 64 1024)
  "Maximum retained byte budget for one board message attributes value.")

(defconst e-board-message-metadata-byte-limit (* 32 1024)
  "Maximum retained byte budget for one board message causal metadata field.")

(defconst e-board-message-tags-byte-limit (* 8 1024)
  "Maximum retained byte budget for one board message tag collection.")

(defconst e-board--aggregation-modes
  '(all any all-terminal first-terminal on-success on-failure on-terminal)
  "Accepted private aggregation readiness modes.")

(cl-defstruct (e-board-delivery-attempt
               (:constructor e-board-delivery-attempt--create)
               (:conc-name e-board-delivery-attempt-))
  number endpoint-token composite-generation state receipt reason)

(cl-defstruct (e-board-pickup
               (:constructor e-board-pickup--create)
               (:conc-name e-board-pickup-))
  delivery-id board-id participant-id message-id subscription-ids
  event-seq-range mode requester-actor addressed-p cause-metadata content reference state
  attempt)

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

(cl-defstruct (e-board-activation
               (:constructor e-board-activation--create)
               (:conc-name e-board-activation-))
  id subscription-id message-id effect state)

(cl-defstruct (e-board-open-activity
               (:constructor e-board-open-activity--create)
               (:conc-name e-board-open-activity-))
  participant-id turn-id message-id seq activity-kind)

(cl-defstruct (e-board-terminal-classification
               (:constructor e-board-terminal-classification--create)
               (:conc-name e-board-terminal-classification-))
  work-id invocation-ids aggregation-ids)

(cl-defstruct (e-board-id-queue
               (:constructor e-board-id-queue--create)
               (:conc-name e-board-id-queue-))
  head tail)

(cl-defstruct (e-board-input-classification
               (:constructor e-board-input-classification--create)
               (:conc-name e-board-input-classification-))
  message publication subscription-count index matches post-subscriptions post-index)

(cl-defstruct (e-board-subscription-replay
               (:constructor e-board-subscription-replay--create)
               (:conc-name e-board-subscription-replay-))
  subscription next-seq through-seq)

(cl-defstruct (e-board
               (:constructor e-board--create)
               (:conc-name e-board-))
  id id-function next-seq events events-tail messages messages-tail message-count
  message-table message-seq-table message-index-table event-message-count
  participants subscriptions subscriptions-tail subscription-count
  subscription-index-table subscription-id-table
  observers pickups source-high-watermarks source-recent work-table invocations aggregations
  pending-effects pending-effects-tail effects-scheduled
  effect-scheduler invocation-effect-dispatcher invocation-work-index aggregation-work-index
  terminal-classifications terminal-classification-tail
  terminal-classification-scheduled terminal-classification-scheduler
  input-classifications input-classification-tail
  input-classification-scheduled input-classification-scheduler
  subscription-replays subscription-replay-tail subscription-replay-scheduled
  routed-pickup-results routed-pickup-results-tail
  aggregation-deadlines aggregation-deadline-tail
  aggregation-deadline-scheduled aggregation-deadline-scheduler
  continuation-timer-scheduler subscription-timer-scheduler
  activations activation-subscription-index pickup-queues pickup-pending-limit
  open-activities closed-activities retention-floor classification-authorizer
  message-notification-function
  unsettled-pickup-count unsettled-effect-count unsettled-routing-count
  unsettled-generation unsettled-change-function)

(defun e-board-unsettled-state (board)
  "Return BOARD's constant-time nonterminal owner projection."
  (list :generation (e-board-unsettled-generation board)
        :pickups (e-board-unsettled-pickup-count board)
        :effects (e-board-unsettled-effect-count board)
        :routing (e-board-unsettled-routing-count board)))

(defun e-board--adjust-unsettled (board class delta)
  "Adjust BOARD's unsettled CLASS by DELTA at its owning transition."
  (let* ((current (pcase class
                    ('pickups (e-board-unsettled-pickup-count board))
                    ('effects (e-board-unsettled-effect-count board))
                    ('routing (e-board-unsettled-routing-count board))))
         (next (+ current delta)))
    (when (< next 0)
      (signal 'e-board-error (list "Negative board unsettled count" class next)))
    (pcase class
      ('pickups (setf (e-board-unsettled-pickup-count board) next))
      ('effects (setf (e-board-unsettled-effect-count board) next))
      ('routing (setf (e-board-unsettled-routing-count board) next)))
    (cl-incf (e-board-unsettled-generation board))
    (when-let ((notify (e-board-unsettled-change-function board)))
      (funcall notify board class delta (e-board-unsettled-state board)))
    next))

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
          classification-authorizer message-notification-function
          unsettled-change-function
          terminal-classification-scheduler input-classification-scheduler
          aggregation-deadline-scheduler continuation-timer-scheduler
          subscription-timer-scheduler
          (pickup-pending-limit e-board-default-pickup-pending-limit)
          (retention-floor 0)
          (register t))
  "Create a process-local board with ID and optional ID-FUNCTION.
ID-FUNCTION receives a symbol such as `message' or `subscription'.  Passing
explicit ids to individual operations takes precedence over this generator.
PICKUP-PENDING-LIMIT bounds records queued behind a participant's active head."
  (unless (and (integerp pickup-pending-limit) (>= pickup-pending-limit 0))
    (signal 'wrong-type-argument (list 'natnump pickup-pending-limit)))
  (unless (and (integerp retention-floor) (>= retention-floor 0))
    (signal 'wrong-type-argument (list 'natnump retention-floor)))
  (unless (or (null classification-authorizer)
              (functionp classification-authorizer))
    (signal 'wrong-type-argument (list 'functionp classification-authorizer)))
  (unless (or (null unsettled-change-function)
              (functionp unsettled-change-function))
    (signal 'wrong-type-argument (list 'functionp unsettled-change-function)))
  (unless (or (null message-notification-function)
              (functionp message-notification-function))
    (signal 'wrong-type-argument
            (list 'functionp message-notification-function)))
  (let* ((board (e-board--create
                  :id (or id (format "brd_%d" (cl-incf e-board--id-sequence)))
                 :id-function id-function
                 :next-seq 0
                 :events nil
                  :events-tail nil
                  :messages nil
                  :messages-tail nil
                  :message-count 0
                  :message-table (make-hash-table :test 'equal)
                  :message-seq-table (make-hash-table :test 'eql)
                  :message-index-table (make-hash-table :test 'eql)
                  :event-message-count
                  (let ((table (make-hash-table :test 'eql)))
                    (puthash 0 0 table)
                    table)
                 :participants (make-hash-table :test 'equal)
                 :subscriptions nil
                 :subscriptions-tail nil
                 :subscription-count 0
                 :subscription-index-table (make-hash-table :test 'eql)
                 :subscription-id-table (make-hash-table :test 'equal)
                 :observers (make-hash-table :test 'equal)
                  :pickups (make-hash-table :test 'equal)
                  :source-high-watermarks (make-hash-table :test 'equal)
                  :source-recent (make-hash-table :test 'equal)
                   :work-table (make-hash-table :test 'equal)
                   :invocations (make-hash-table :test 'equal)
                   :aggregations (make-hash-table :test 'equal)
                  :activations (make-hash-table :test 'equal)
                  :activation-subscription-index (make-hash-table :test 'equal)
                  :pickup-queues (make-hash-table :test 'equal)
                  :pickup-pending-limit pickup-pending-limit
                  :open-activities (make-hash-table :test 'equal)
                  :closed-activities (make-hash-table :test 'equal)
                 :retention-floor retention-floor
                  :pending-effects nil
                  :pending-effects-tail nil
                  :effects-scheduled nil
                  :effect-scheduler effect-scheduler
                  :invocation-effect-dispatcher invocation-effect-dispatcher
                  :classification-authorizer classification-authorizer
                  :message-notification-function message-notification-function
                  :unsettled-pickup-count 0
                  :unsettled-effect-count 0
                  :unsettled-routing-count 0
                  :unsettled-generation 0
                  :unsettled-change-function unsettled-change-function
                  :invocation-work-index (make-hash-table :test 'equal)
                  :aggregation-work-index (make-hash-table :test 'equal)
                  :terminal-classifications nil
                  :terminal-classification-tail nil
                  :terminal-classification-scheduled nil
                  :terminal-classification-scheduler terminal-classification-scheduler
                  :input-classifications nil
                  :input-classification-tail nil
                  :input-classification-scheduled nil
                  :input-classification-scheduler input-classification-scheduler
                  :subscription-replays nil
                  :subscription-replay-tail nil
                  :subscription-replay-scheduled nil
                  :routed-pickup-results nil
                  :routed-pickup-results-tail nil
                  :aggregation-deadlines nil
                  :aggregation-deadline-tail nil
                  :aggregation-deadline-scheduled nil
                  :aggregation-deadline-scheduler aggregation-deadline-scheduler
                  :continuation-timer-scheduler continuation-timer-scheduler
                  :subscription-timer-scheduler subscription-timer-scheduler)))
    (when register (e-board-register board))
    board))

(defun e-board--append-event (board type data)
  "Append TYPE with DATA to BOARD's ordered event log and return the event."
  (let ((event (e-board-event--create
                :seq (cl-incf (e-board-next-seq board))
                :type type
                :data data)))
    (let ((cell (list event)))
      (if (e-board-events-tail board)
          (setcdr (e-board-events-tail board) cell)
        (setf (e-board-events board) cell))
      (setf (e-board-events-tail board) cell))
    (puthash (e-board-event-seq event) (e-board-message-count board)
             (e-board-event-message-count board))
    event))

(defun e-board-events-after (board seq)
  "Return BOARD events whose sequence is strictly greater than SEQ."
  (cl-remove-if (lambda (event) (<= (e-board-event-seq event) seq))
                (e-board-events board)))

(defun e-board-advance-retention-floor (board floor)
  "Advance BOARD's logical retained-message floor to FLOOR.
The reducer never scans observers here.  A live observer whose cursor is now
behind this floor transitions to `expired' only when it next requests a page,
at which point that page requires a fresh snapshot."
  (unless (and (integerp floor) (>= floor 0))
    (signal 'wrong-type-argument (list 'natnump floor)))
  (when (> floor (e-board-next-seq board))
    (signal 'e-board-error (list "Retention floor exceeds board sequence" floor)))
  (when (< floor (e-board-retention-floor board))
    (signal 'e-board-error (list "Retention floor cannot move backwards" floor)))
  (when (> floor (e-board-retention-floor board))
    (setf (e-board-retention-floor board) floor)
    (e-board--append-event board 'retention-floor-advanced (list :floor floor)))
  board)

(defun e-board-message (board message-id)
  "Return BOARD message MESSAGE-ID, or nil when it is not retained."
  (gethash message-id (e-board-message-table board)))

(defun e-board-participant (board participant-id)
  "Return BOARD participant PARTICIPANT-ID, or nil."
  (gethash participant-id (e-board-participants board)))

(defun e-board-pickup (board delivery-id)
  "Return BOARD pickup DELIVERY-ID, or nil."
  (gethash delivery-id (e-board-pickups board)))

(defun e-board--pickup-queue (board participant-id)
  "Return PARTICIPANT-ID's ordered pickup identities on BOARD."
  (gethash participant-id (e-board-pickup-queues board)))

(defun e-board--enqueue-pickup (board pickup)
  "Append PICKUP to its participant FIFO and return its initial state."
  (let* ((participant-id (e-board-pickup-participant-id pickup))
         (queue (e-board--pickup-queue board participant-id)))
    (if (and queue
             (>= (length (cdr queue)) (e-board-pickup-pending-limit board)))
        (progn
          (setf (e-board-pickup-state pickup) 'overflowed)
          (e-board--append-event
           board 'pickup-overflowed
           (list :delivery-id (e-board-pickup-delivery-id pickup)
                 :pending-limit (e-board-pickup-pending-limit board))))
      (puthash participant-id
               (append queue (list (e-board-pickup-delivery-id pickup)))
               (e-board-pickup-queues board))
      (setf (e-board-pickup-state pickup) (if queue 'pending 'ready))
      (e-board--adjust-unsettled board 'pickups 1))
    (e-board-pickup-state pickup)))

(defun e-board--set-pickup-attempt-state (pickup state &optional reason)
  "Move PICKUP's physical attempt to STATE and optionally retain REASON."
  (when-let ((attempt (e-board-pickup-attempt pickup)))
    (setf (e-board-delivery-attempt-state attempt) state)
    (when reason
      (setf (e-board-delivery-attempt-reason attempt)
            (e-board--copy-envelope-value reason))))
  pickup)

(defun e-board-pickup-start-delivery
    (board delivery-id &optional endpoint-token composite-generation)
  "Bind and fence ready DELIVERY-ID as one physical delivery attempt.
ENDPOINT-TOKEN is opaque to the board.  COMPOSITE-GENERATION identifies the
selected logical-instance and concrete-harness generations.  A later attempt
may replace this binding only after the previous call was proven uncommitted."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (and (eq (e-board-pickup-state pickup) 'ready)
                 (equal (car (e-board--pickup-queue
                              board (e-board-pickup-participant-id pickup)))
                        delivery-id))
      (signal 'e-board-error (list "Pickup is not ready head" delivery-id)))
    (let* ((previous (e-board-pickup-attempt pickup))
           (number (if previous
                       (1+ (e-board-delivery-attempt-number previous))
                     1)))
      (when (and previous
                 (not (eq (e-board-delivery-attempt-state previous)
                          'proven-uncommitted)))
        (signal 'e-board-error
                (list "Pickup attempt is not replaceable" delivery-id)))
      (setf (e-board-pickup-attempt pickup)
            (e-board-delivery-attempt--create
             :number number
             :endpoint-token (e-board--copy-envelope-value endpoint-token)
             :composite-generation
             (e-board--copy-envelope-value composite-generation)
             :state 'delivering)))
    (setf (e-board-pickup-state pickup) 'delivering)
    (e-board--append-event board 'pickup-delivering
                           (list :delivery-id delivery-id
                                 :attempt-number
                                 (e-board-delivery-attempt-number
                                  (e-board-pickup-attempt pickup))
                                 :endpoint-token
                                 (e-board--copy-envelope-value endpoint-token)
                                 :composite-generation
                                 (e-board--copy-envelope-value
                                  composite-generation)))
    pickup))

(defun e-board-pickup-complete-delivery (board delivery-id)
  "Consume DELIVERY-ID and promote its participant's next FIFO pickup.
Return the newly ready pickup identity, if any."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(delivering accepted cancelling))
      (signal 'e-board-error
              (list "Pickup is not delivering, accepted, or cancelling" delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id)))
      (unless (equal (car queue) delivery-id)
        (signal 'e-board-error (list "Pickup lost FIFO ownership" delivery-id)))
      (setf (e-board-pickup-state pickup) 'consumed)
      (e-board--adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state pickup 'consumed)
      (e-board--append-event board 'pickup-consumed
                             (list :delivery-id delivery-id))
      (setq queue (cdr queue))
      (puthash participant-id queue (e-board-pickup-queues board))
      (when-let ((next-id (car queue)))
        (let ((next (e-board-pickup board next-id)))
          (setf (e-board-pickup-state next) 'ready)
          (e-board--append-event board 'pickup-ready
                                 (list :delivery-id next-id))
          next-id)))))

(defun e-board-pickup-accept-delivery (board delivery-id &optional receipt)
  "Record DELIVERY-ID as harness-owned with optional acceptance RECEIPT."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (eq (e-board-pickup-state pickup) 'delivering)
      (signal 'e-board-error (list "Pickup is not delivering" delivery-id)))
    (setf (e-board-pickup-state pickup) 'accepted)
    (e-board--set-pickup-attempt-state pickup 'accepted)
    (when receipt
      (setf (e-board-delivery-attempt-receipt (e-board-pickup-attempt pickup))
            (e-board--copy-envelope-value receipt)))
    (e-board--append-event board 'pickup-accepted (list :delivery-id delivery-id))
    pickup))

(defun e-board-pickup-discard-delivery (board delivery-id reason)
  "Record accepted DELIVERY-ID as discarded and promote its FIFO successor."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(accepted cancelling))
      (signal 'e-board-error (list "Pickup is not accepted or cancelling" delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id))
           (cancelled-p (eq (e-board-pickup-state pickup) 'cancelling)))
      (unless (equal (car queue) delivery-id)
        (signal 'e-board-error (list "Pickup lost FIFO ownership" delivery-id)))
      (setf (e-board-pickup-state pickup) (if cancelled-p 'cancelled 'discarded))
      (e-board--adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state
       pickup (if cancelled-p 'cancelled 'discarded) reason)
      (e-board--append-event board (if cancelled-p 'pickup-cancelled 'pickup-discarded)
                             (list :delivery-id delivery-id :reason reason))
      (setq queue (cdr queue))
      (puthash participant-id queue (e-board-pickup-queues board))
      (when-let ((next-id (car queue)))
        (let ((next (e-board-pickup board next-id)))
          (setf (e-board-pickup-state next) 'ready)
          (e-board--append-event board 'pickup-ready
                                 (list :delivery-id next-id))
          next-id)))))

(defun e-board-pickup-mark-uncertain (board delivery-id reason)
  "Tombstone ambiguous DELIVERY-ID and promote its FIFO successor.
An uncertain physical attempt is never retried as though it had not reached the
original endpoint.  REASON records the reconciliation gap for later inspection."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(delivering accepted cancelling))
      (signal 'e-board-error
              (list "Pickup uncertainty requires delivering or accepted state" delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id)))
      (unless (equal (car queue) delivery-id)
        (signal 'e-board-error (list "Pickup lost FIFO ownership" delivery-id)))
      (setf (e-board-pickup-state pickup) 'uncertain)
      (e-board--adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state pickup 'uncertain reason)
      (e-board--append-event board 'pickup-uncertain
                             (list :delivery-id delivery-id :reason reason))
      (setq queue (cdr queue))
      (puthash participant-id queue (e-board-pickup-queues board))
      (when-let ((next-id (car queue)))
        (let ((next (e-board-pickup board next-id)))
          (setf (e-board-pickup-state next) 'ready)
          (e-board--append-event board 'pickup-ready
                                 (list :delivery-id next-id))
          next-id)))))

(defun e-board-cancel-pickup (board delivery-id &optional reason)
  "Cancel DELIVERY-ID without affecting watched work.
Pending and ready pickups become terminal immediately.  In-flight and accepted
pickups become `cancelling' until a consumption or discard receipt resolves the
same immutable delivery id.  Return a newly ready successor only when this
call releases the FIFO head."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup)
                  '(pending ready delivering accepted))
      (signal 'e-board-error
              (list "Pickup cancellation requires a nonterminal state" delivery-id)))
    (if (memq (e-board-pickup-state pickup) '(delivering accepted))
        (progn
          (setf (e-board-pickup-state pickup) 'cancelling)
          (e-board--set-pickup-attempt-state pickup 'cancelling reason)
          (e-board--append-event board 'pickup-cancelling
                                 (list :delivery-id delivery-id :reason reason))
          nil)
      (let* ((participant-id (e-board-pickup-participant-id pickup))
             (queue (e-board--pickup-queue board participant-id))
             (head-p (equal (car queue) delivery-id)))
        (setf (e-board-pickup-state pickup) 'cancelled)
        (e-board--adjust-unsettled board 'pickups -1)
        (e-board--set-pickup-attempt-state pickup 'cancelled reason)
        (puthash participant-id (delete delivery-id queue) (e-board-pickup-queues board))
        (e-board--append-event board 'pickup-cancelled
                               (list :delivery-id delivery-id :reason reason))
        (when head-p
          (when-let ((next-id (car (e-board--pickup-queue board participant-id))))
            (let ((next (e-board-pickup board next-id)))
              (setf (e-board-pickup-state next) 'ready)
              (e-board--append-event board 'pickup-ready
                                     (list :delivery-id next-id))
              next-id)))))))

(defun e-board-expire-pickup (board delivery-id &optional reason)
  "Expire pending or ready DELIVERY-ID and release its FIFO successor.
Expiry is a visible terminal tombstone.  It never retries or silently drops a
stalled logical pickup, and an expired head releases only that participant's
next FIFO record."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(pending ready))
      (signal 'e-board-error
              (list "Pickup expiry requires pending or ready state" delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id))
           (head-p (equal (car queue) delivery-id)))
      (setf (e-board-pickup-state pickup) 'expired)
      (e-board--adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state pickup 'expired reason)
      (puthash participant-id (delete delivery-id queue) (e-board-pickup-queues board))
      (e-board--append-event board 'pickup-expired
                             (list :delivery-id delivery-id :reason reason))
      (when head-p
        (when-let ((next-id (car (e-board--pickup-queue board participant-id))))
          (let ((next (e-board-pickup board next-id)))
            (setf (e-board-pickup-state next) 'ready)
            (e-board--append-event board 'pickup-ready
                                   (list :delivery-id next-id))
            next-id))))))

(defun e-board-fail-pickup (board delivery-id reason)
  "Record a permanent failure for DELIVERY-ID and release its FIFO successor.
The delivery adapter may use this only when it proves the logical pickup cannot
be delivered through the selected endpoint.  A transient uncommitted failure
uses `e-board-pickup-return-ready' instead."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(pending ready delivering))
      (signal 'e-board-error
              (list "Pickup failure requires pending, ready, or delivering state"
                    delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id))
           (head-p (equal (car queue) delivery-id)))
      (setf (e-board-pickup-state pickup) 'failed)
      (e-board--adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state pickup 'failed reason)
      (puthash participant-id (delete delivery-id queue) (e-board-pickup-queues board))
      (e-board--append-event board 'pickup-failed
                             (list :delivery-id delivery-id :reason reason))
      (when head-p
        (when-let ((next-id (car (e-board--pickup-queue board participant-id))))
          (let ((next (e-board-pickup board next-id)))
            (setf (e-board-pickup-state next) 'ready)
            (e-board--append-event board 'pickup-ready
                                   (list :delivery-id next-id))
            next-id))))))

(defun e-board-pickup-return-ready (board delivery-id err)
  "Return uncommitted delivering DELIVERY-ID to its FIFO head after ERR.
When a cancellation already fenced the in-flight attempt, a proven
uncommitted failure settles that cancellation instead of retrying it."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(delivering cancelling))
      (signal 'e-board-error (list "Pickup is not delivering or cancelling" delivery-id)))
    (if (eq (e-board-pickup-state pickup) 'cancelling)
        (let* ((participant-id (e-board-pickup-participant-id pickup))
               (queue (e-board--pickup-queue board participant-id)))
          (unless (equal (car queue) delivery-id)
            (signal 'e-board-error (list "Pickup lost FIFO ownership" delivery-id)))
          (setf (e-board-pickup-state pickup) 'cancelled)
          (e-board--adjust-unsettled board 'pickups -1)
          (e-board--set-pickup-attempt-state pickup 'cancelled err)
          (e-board--append-event board 'pickup-cancelled
                                 (list :delivery-id delivery-id :reason err))
          (setq queue (cdr queue))
          (puthash participant-id queue (e-board-pickup-queues board))
          (when-let ((next-id (car queue)))
            (let ((next (e-board-pickup board next-id)))
              (setf (e-board-pickup-state next) 'ready)
              (e-board--append-event board 'pickup-ready
                                     (list :delivery-id next-id))
              next-id)))
      (setf (e-board-pickup-state pickup) 'ready)
      (e-board--set-pickup-attempt-state pickup 'proven-uncommitted)
      (e-board--append-event board 'pickup-delivery-failed
                             (list :delivery-id delivery-id :error err))
      pickup)))

(defun e-board-observed-work (board work-id)
  "Return BOARD's observed work record for WORK-ID, or nil."
  (gethash work-id (e-board-work-table board)))

(defun e-board-invocation (board invocation-id)
  "Return BOARD's exact invocation relation for INVOCATION-ID, or nil."
  (gethash invocation-id (e-board-invocations board)))

(defun e-board-aggregation (board aggregation-id)
  "Return BOARD's aggregation subscription for AGGREGATION-ID, or nil."
  (gethash aggregation-id (e-board-aggregations board)))

(defun e-board-activation (board activation-id)
  "Return BOARD's frozen effect activation for ACTIVATION-ID, or nil."
  (gethash activation-id (e-board-activations board)))

(defun e-board--open-activity-key (participant-id turn-id)
  "Return the board-owned open-activity projection key."
  (list participant-id turn-id))

(defun e-board-open-activity (board participant-id turn-id)
  "Return BOARD's current open activity for PARTICIPANT-ID and TURN-ID."
  (gethash (e-board--open-activity-key participant-id turn-id)
           (e-board-open-activities board)))

(defun e-board--terminal-activity-kind-p (activity-kind)
  "Return non-nil when ACTIVITY-KIND closes a visible turn projection."
  (memq activity-kind '(turn-finished turn-failed turn-cancelled turn-summary)))

(defun e-board--record-open-activity (board message)
  "Update BOARD's derived open-activity projection from activity MESSAGE."
  (let* ((participant-id (e-board-message-subject-participant-id message))
         (turn-id (e-board-message-source-turn-id message))
         (key (e-board--open-activity-key participant-id turn-id)))
    (if (e-board--terminal-activity-kind-p (e-board-message-activity-kind message))
        (e-board--close-open-activity board participant-id turn-id message)
      (unless (gethash key (e-board-closed-activities board))
        (puthash key
                 (e-board-open-activity--create
                  :participant-id participant-id :turn-id turn-id
                  :message-id (e-board-message-id message)
                  :seq (e-board-message-seq message)
                  :activity-kind (e-board-message-activity-kind message))
                 (e-board-open-activities board))))))

(defun e-board--close-open-activity (board participant-id turn-id message)
  "Close BOARD's activity projection for PARTICIPANT-ID and TURN-ID at MESSAGE."
  (when (and participant-id turn-id)
    (let ((key (e-board--open-activity-key participant-id turn-id)))
      (remhash key (e-board-open-activities board))
      (puthash key (e-board-message-id message) (e-board-closed-activities board)))))

(defun e-board-observer (board observer-id)
  "Return BOARD's client observer cursor OBSERVER-ID, or nil."
  (gethash observer-id (e-board-observers board)))

(defun e-board--schedule-effect-drain (board)
  "Schedule at most one later drain for BOARD's frozen effect FIFO."
  (unless (e-board-effects-scheduled board)
    (setf (e-board-effects-scheduled board) t)
    (if-let ((scheduler (e-board-effect-scheduler board)))
        (funcall scheduler (lambda () (e-board-drain-effects board)))
      (run-at-time 0 nil (lambda () (e-board-drain-effects board))))))

(defun e-board--schedule-effect (board effect)
  "Schedule BOARD EFFECT after the initiating work-start stack unwinds."
  (let ((cell (list effect)))
    (if-let ((tail (e-board-pending-effects-tail board)))
        (setcdr tail cell)
      (setf (e-board-pending-effects board) cell))
    (setf (e-board-pending-effects-tail board) cell))
  (e-board--adjust-unsettled board 'effects 1)
  (e-board--schedule-effect-drain board))

(defun e-board--schedule-aggregation-deadline (board)
  "Schedule one later deadline drain for BOARD."
  (unless (e-board-aggregation-deadline-scheduled board)
    (setf (e-board-aggregation-deadline-scheduled board) t)
    (if-let ((scheduler (e-board-aggregation-deadline-scheduler board)))
        (funcall scheduler (lambda () (e-board-drain-aggregation-deadlines board)))
      (run-at-time 0 nil (lambda () (e-board-drain-aggregation-deadlines board))))))

(defun e-board--queue-aggregation-deadline (board aggregation-id)
  "Record an elapsed aggregation deadline without settling on the timer stack."
  (let ((cell (list aggregation-id)))
    (if-let ((tail (e-board-aggregation-deadline-tail board)))
        (setcdr tail cell)
      (setf (e-board-aggregation-deadlines board) cell))
    (setf (e-board-aggregation-deadline-tail board) cell))
  (e-board--schedule-aggregation-deadline board))

(defun e-board-drain-aggregation-deadlines (board)
  "Commit a bounded page of elapsed aggregation deadlines in board order."
  (setf (e-board-aggregation-deadline-scheduled board) nil)
  (let ((remaining e-board-aggregation-deadline-drain-limit))
    (while (and (> remaining 0) (e-board-aggregation-deadlines board))
      (let ((aggregation-id (pop (e-board-aggregation-deadlines board))))
        (unless (e-board-aggregation-deadlines board)
          (setf (e-board-aggregation-deadline-tail board) nil))
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
  (setf (e-board-effects-scheduled board) nil)
  (let ((remaining e-board-effect-drain-limit))
    (while (and (> remaining 0) (e-board-pending-effects board))
      (let ((effect (pop (e-board-pending-effects board))))
        (unless (e-board-pending-effects board)
          (setf (e-board-pending-effects-tail board) nil))
      ;; Concrete effect closures record their own domain failure state.  This
      ;; outer boundary also contains an unexpected stale or malformed queued
      ;; callback, so one record cannot wedge later FIFO effects.
      (unwind-protect
          (condition-case err
              (funcall effect)
            (error
             (e-board--append-event board 'effect-drain-failed
                                    (list :error err))))
        (e-board--adjust-unsettled board 'effects -1)))
      (cl-decf remaining))
    (when (e-board-pending-effects board)
      (e-board--schedule-effect-drain board))))

(defun e-board--index-work-subscription (index work-id subscription-id)
  "Add SUBSCRIPTION-ID to WORK-ID's exact INDEX without scanning its peers."
  (let* ((queue (or (gethash work-id index)
                    (puthash work-id (e-board-id-queue--create) index)))
         (cell (list subscription-id)))
    (if-let ((tail (e-board-id-queue-tail queue)))
        (setcdr tail cell)
      (setf (e-board-id-queue-head queue) cell))
    (setf (e-board-id-queue-tail queue) cell)))

(defun e-board--indexed-work-subscriptions (index work-id)
  "Return WORK-ID's stable subscription-id sequence from INDEX."
  (when-let ((queue (gethash work-id index)))
    (e-board-id-queue-head queue)))

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
                 :invocation-ids
                 (or invocation-ids
                     (e-board--indexed-work-subscriptions
                      (e-board-invocation-work-index board) work-id))
                 :aggregation-ids
                 (or aggregation-ids
                     (e-board--indexed-work-subscriptions
                      (e-board-aggregation-work-index board) work-id)))))
    ;; Detach the exact frozen queues.  A later invalid enrollment cannot mutate
    ;; the terminal record's persistent list tail.
    (remhash work-id (e-board-invocation-work-index board))
    (remhash work-id (e-board-aggregation-work-index board))
    (let ((cell (list record)))
      (if-let ((tail (e-board-terminal-classification-tail board)))
          (setcdr tail cell)
        (setf (e-board-terminal-classifications board) cell))
      (setf (e-board-terminal-classification-tail board) cell))
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
         ((e-board-terminal-classification-invocation-ids record)
          (let ((id (pop (e-board-terminal-classification-invocation-ids record))))
            (when-let ((invocation (e-board-invocation board id)))
              (e-board--settle-invocation board invocation state payload))))
         ((e-board-terminal-classification-aggregation-ids record)
          (let ((id (pop (e-board-terminal-classification-aggregation-ids record))))
            (when-let ((aggregation (e-board-aggregation board id)))
              (when (and (eq (e-board-aggregation-state aggregation) 'open)
                         (e-board--aggregation-ready-p board aggregation))
                (e-board--settle-aggregation board aggregation 'complete)))))
         (t
          (setf (e-board-terminal-classifications board)
                (cdr (e-board-terminal-classifications board)))
          (unless (e-board-terminal-classifications board)
            (setf (e-board-terminal-classification-tail board) nil))))
        (cl-decf remaining)))
    (when (e-board-terminal-classifications board)
      (e-board--schedule-terminal-classification board))))

(defun e-board--settle-invocation (board invocation state payload)
  "Commit INVOCATION's exact reply effect for terminal STATE and PAYLOAD."
  (when (eq (e-board-invocation-state invocation) 'open)
    (setf (e-board-invocation-state invocation) 'prepared)
    (let ((activation-id
           (list (e-board-id board) (e-board-invocation-id invocation) 1))
          (activation nil))
      (setf (e-board-invocation-activation-id invocation) activation-id)
      (setq activation
            (e-board-activation--create
             :id activation-id
             :subscription-id (e-board-invocation-id invocation)
             :message-id (e-board-invocation-work-id invocation)
             :effect 'reply-to-invocation :state 'prepared))
      (puthash activation-id activation (e-board-activations board))
      (e-board--append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :work-id (e-board-invocation-work-id invocation)
             :effect 'reply-to-invocation))
      (e-board--schedule-effect
       board
       (lambda ()
         (when (and (eq (e-board-invocation-state invocation) 'prepared)
                    (eq (e-board-activation-state activation) 'prepared))
           (setf (e-board-invocation-state invocation) 'applying)
           (setf (e-board-activation-state activation) 'applying)
           (e-board--append-event
            board 'activation-applying (list :activation-id activation-id))
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
                 (setf (e-board-activation-state activation) 'committed)
                 (e-board--append-event
                  board 'effect-committed
                  (list :activation-id activation-id
                        :effect 'reply-to-invocation)))
             (error
              (setf (e-board-invocation-state invocation) 'failed)
              (setf (e-board-activation-state activation) 'failed)
              (e-board--append-event
               board 'effect-failed
                (list :activation-id activation-id :error err))))))))))

(defun e-board--aggregation-ready-p (board aggregation)
  "Return non-nil when AGGREGATION's observed work has reached its policy."
  (let ((work-ids (e-board-aggregation-work-ids aggregation)))
    (pcase (e-board-aggregation-mode aggregation)
      ((or 'all 'all-terminal)
       (cl-every (lambda (id)
                   (e-board-work-terminal-seq (e-board-observed-work board id)))
                 work-ids))
      ((or 'any 'first-terminal)
       (cl-some (lambda (id)
                  (e-board-work-terminal-seq (e-board-observed-work board id)))
                work-ids))
      ('on-success
       (eq (e-board-work-state (e-board-observed-work board (car work-ids)))
           'finished))
      ('on-failure
       (eq (e-board-work-state (e-board-observed-work board (car work-ids)))
           'failed))
      ('on-terminal
       (e-board-work-terminal-seq
        (e-board-observed-work board (car work-ids))))
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
           (list (e-board-id board) (e-board-aggregation-id aggregation) 1))
          (activation nil))
      (setf (e-board-aggregation-activation-id aggregation) activation-id)
      (setq activation
            (e-board-activation--create
             :id activation-id
             :subscription-id (e-board-aggregation-id aggregation)
             :message-id nil :effect 'reply-to-invocation :state 'prepared))
      (puthash activation-id activation (e-board-activations board))
      (e-board--append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :work-ids (copy-sequence (e-board-aggregation-work-ids aggregation))
             :effect 'reply-to-invocation))
      (e-board--schedule-effect
       board
       (lambda ()
         (when (and (eq (e-board-aggregation-state aggregation) 'prepared)
                    (eq (e-board-activation-state activation) 'prepared))
           (setf (e-board-aggregation-state aggregation) 'applying)
           (setf (e-board-activation-state activation) 'applying)
           (e-board--append-event
            board 'activation-applying (list :activation-id activation-id))
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
                 (setf (e-board-activation-state activation) 'committed)
                 (e-board--append-event
                  board 'effect-committed
                  (list :activation-id activation-id :effect 'reply-to-invocation)))
             (error
              (setf (e-board-aggregation-state aggregation) 'failed)
              (setf (e-board-activation-state activation) 'failed)
              (e-board--append-event
               board 'effect-failed
               (list :activation-id activation-id :error err))))))))))

(cl-defun e-board-subscribe-aggregation
    (board work-ids mode effect-target &key id timeout)
  "Install an ordered work aggregation reply subscription on BOARD.
WORK-IDS must name currently observed work.  MODE is `all'/`all-terminal',
`any'/`first-terminal', `on-success', `on-failure', or `on-terminal'.
EFFECT-TARGET remains opaque to the board and receives a later frozen reason
through the injected invocation effect dispatcher."
  (unless (listp work-ids)
    (signal 'e-board-error (list "Aggregation work ids must be a list")))
  (unless (memq mode e-board--aggregation-modes)
    (signal 'e-board-error (list "Unknown aggregation mode" mode)))
  (when (and (memq mode '(any first-terminal on-success on-failure on-terminal))
             (null work-ids))
    (signal 'e-board-error (list "Aggregation requires at least one work id")))
  (when (and (memq mode '(on-success on-failure on-terminal))
             (/= (length work-ids) 1))
    (signal 'e-board-error
            (list "Exact readiness requires one work id" mode work-ids)))
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
             :readiness (pcase mode
                          ('all 'all-terminal)
                          ('any 'first-terminal)
                          (_ mode))
             :effect 'reply-to-invocation))
      (when timeout
        (setf (e-board-aggregation-timer aggregation)
              (run-at-time timeout nil
                           (lambda ()
                             (e-board--queue-aggregation-deadline
                              board (e-board-aggregation-id aggregation))))))
      (when (e-board--aggregation-ready-p board aggregation)
        ;; Keep an already-ready subscription on the same later classifier
        ;; path as a fresh terminal publication, except an empty all-terminal
        ;; set which has no source terminal record to classify.
        (if work-ids
            (e-board--queue-terminal-classification
             board (car work-ids) nil (list id))
          (e-board--settle-aggregation board aggregation 'complete)))
      aggregation)))

(defun e-board-cancel-aggregation (board aggregation-id)
  "Cancel open or prepared AGGREGATION-ID without affecting watched work.
A prepared reply activation is fenced before its later effect callback can
reach the runtime; an already-applying effect remains outside this local
cancellation boundary because its commit is no longer provably absent."
  (when-let ((aggregation (e-board-aggregation board aggregation-id)))
    (when (memq (e-board-aggregation-state aggregation) '(open prepared))
      (when-let ((timer (e-board-aggregation-timer aggregation)))
        (cancel-timer timer))
      (when (eq (e-board-aggregation-state aggregation) 'prepared)
        (when-let ((activation
                    (e-board-activation board
                                        (e-board-aggregation-activation-id aggregation))))
          (when (eq (e-board-activation-state activation) 'prepared)
            (setf (e-board-activation-state activation) 'cancelled)
            (e-board--append-event
             board 'activation-cancelled
             (list :activation-id (e-board-activation-id activation)
                   :reason 'aggregation-cancelled)))))
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

(defun e-board--append-subscription (board subscription)
  "Append SUBSCRIPTION to BOARD's ordered list and constant-time indexes."
  (let* ((index (cl-incf (e-board-subscription-count board)))
         (cell (list subscription)))
    (if (e-board-subscriptions-tail board)
        (setcdr (e-board-subscriptions-tail board) cell)
      (setf (e-board-subscriptions board) cell))
    (setf (e-board-subscriptions-tail board) cell)
    (puthash index subscription (e-board-subscription-index-table board))
    (puthash (e-board-subscription-id subscription) subscription
             (e-board-subscription-id-table board))
    subscription))

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
    (when (e-board-find-subscription board subscription-id)
      (signal 'e-board-id-conflict (list subscription-id)))
    (let ((participant
           (e-board-participant--create
            :id id :board-id (e-board-id board) :state state
            :create-pickup-subscription-id subscription-id)))
      (puthash id participant (e-board-participants board))
      (e-board--append-subscription
       board
       (e-board-subscription--create
        :id subscription-id
        :board-id (e-board-id board)
        :participant-id id
        :selector (list :to id)
        :effect 'create-pickup
        :state 'active
        :built-in-p t))
      (e-board--append-event board 'participant-added
                             (list :participant-id id
                                   :subscription-id subscription-id))
      participant)))

(defun e-board--valid-continuation-readiness-p (readiness)
  "Return non-nil when READINESS is a closed continuation accumulator policy."
  (or (null readiness)
      (and (listp readiness)
           (cond
             ((eq (plist-get readiness :policy) 'batch)
              (let ((count (plist-get readiness :count))
                    (max-delay (plist-get readiness :max-delay)))
                (and (integerp count) (> count 0)
                     (or (null max-delay)
                         (and (numberp max-delay) (> max-delay 0))))))
             ((eq (plist-get readiness :policy) 'latest-after-quiet)
              (let ((quiet-period (plist-get readiness :quiet-period)))
                (and (numberp quiet-period) (> quiet-period 0))))))))

(defun e-board--validate-continuation-readiness (effect readiness)
  "Reject a READINESS declaration that cannot belong to EFFECT."
  (when (and readiness (not (and (listp effect) (eq (car effect) :post-input))))
    (signal 'e-board-error (list "Readiness requires post-input effect" readiness)))
  (unless (e-board--valid-continuation-readiness-p readiness)
    (signal 'e-board-error (list "Invalid continuation readiness" readiness))))

(cl-defun e-board-subscribe
    (board participant-id selector &key id (state 'active) (effect 'create-pickup)
           readiness firing-limit lifetime start-seq)
  "Install an ordinary immutable subscription for PARTICIPANT-ID.
SELECTOR supports kind, activity-kind, identity, attribute, and tag clauses.
Effects are `create-pickup' and declarative `(:post-input ...)'.  A post-input
continuation may declare READINESS as `(:policy batch :count N :max-delay
SECONDS)' or `(:policy latest-after-quiet :quiet-period SECONDS)'; otherwise
every match fires.  FIRING-LIMIT, when non-nil, is the positive number of
post-input activations permitted before the subscription completes.  LIFETIME,
when non-nil, is a positive number of seconds before the subscription expires.
START-SEQ is an explicit retained-history replay cursor for a post-input
continuation; it never reroutes an existing input or creates a pickup.
New subscriptions inspect future board records; only `create-pickup' is
restricted to input records."
  (unless (e-board-participant board participant-id)
    (signal 'e-board-error (list "Unknown participant" participant-id)))
  (unless (or (eq effect 'create-pickup)
              (and (listp effect) (eq (car effect) :post-input)))
    (signal 'e-board-error (list "Unsupported board effect" effect)))
  (e-board--validate-continuation-readiness effect readiness)
  (when firing-limit
    (unless (and (integerp firing-limit) (> firing-limit 0))
      (signal 'wrong-type-argument (list 'plusp firing-limit)))
    (unless (and (listp effect) (eq (car effect) :post-input))
      (signal 'e-board-error
              (list "Firing limit requires post-input effect" firing-limit))))
  (when lifetime
    (unless (and (numberp lifetime) (> lifetime 0))
      (signal 'wrong-type-argument (list 'plusp lifetime))))
  (when start-seq
    (unless (and (integerp start-seq)
                 (>= start-seq (1- (e-board-retention-floor board))))
      (signal 'e-board-error
              (list "Replay start is outside retained board history" start-seq)))
    (unless (and (listp effect) (eq (car effect) :post-input))
      (signal 'e-board-error
              (list "Replay requires post-input effect" start-seq))))
  (unless (listp selector)
    (signal 'wrong-type-argument (list 'listp selector)))
  (unless (memq state '(active muted))
    (signal 'wrong-type-argument (list '(member active muted) state)))
  (let ((id (or id (e-board--next-id board 'subscription))))
    (when (e-board-find-subscription board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((subscription
           (e-board-subscription--create
            :id id :board-id (e-board-id board)
            :participant-id participant-id
            ;; The matcher is immutable even if the caller later mutates its plist.
            :selector (copy-tree selector)
            :effect (copy-tree effect) :state state :built-in-p nil
            :readiness (copy-tree readiness) :accumulator nil
            :readiness-generation 0 :firing-number 0
            :firing-limit firing-limit :lifetime lifetime
            :lifetime-generation 0)))
      (e-board--append-subscription board subscription)
      (e-board--append-event board 'subscription-added
                             (list :subscription-id id
                                   :participant-id participant-id
                                   :firing-limit firing-limit :lifetime lifetime))
      (when lifetime
        (setf (e-board-subscription-lifetime-timer subscription)
              (e-board--schedule-subscription-timer
               board lifetime
               (lambda ()
                 (e-board--queue-subscription-expiry
                 board id (e-board-subscription-lifetime-generation subscription))))))
      (when start-seq
        (e-board--queue-subscription-replay board subscription start-seq))
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
    (and (or (not (plist-member selector :kind))
             (equal (plist-get selector :kind) (e-board-message-kind message)))
         (or (not (plist-member selector :activity-kind))
             (equal (plist-get selector :activity-kind)
                    (e-board-message-activity-kind message)))
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
                 (e-board--fault-subscription board subscription err)
                 nil))
            t))))

(defun e-board--message-count-through-seq (board seq)
  "Return BOARD's number of messages whose event sequence is at most SEQ."
  (cond
   ((<= seq 0) 0)
   ((>= seq (e-board-next-seq board)) (e-board-message-count board))
   (t (or (gethash seq (e-board-event-message-count board)) 0))))

(cl-defun e-board-observer-subscribe
    (board client-id selector &key id client-generation (state 'active) start-seq
           history-before-seq (history-floor 0))
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
  (setq start-seq (or start-seq (e-board-next-seq board)))
  (unless (and (integerp start-seq) (>= start-seq 0))
    (signal 'wrong-type-argument (list 'natnump start-seq)))
  (unless (and (integerp history-floor) (>= history-floor 0))
    (signal 'wrong-type-argument (list 'natnump history-floor)))
  (when client-generation
    (unless (and (integerp client-generation) (> client-generation 0))
      (signal 'wrong-type-argument (list 'plusp client-generation))))
  (when history-before-seq
    (unless (and (integerp history-before-seq)
                 (>= history-before-seq history-floor))
      (signal 'wrong-type-argument (list 'natnump history-before-seq))))
  (let ((id (or id (e-board--next-id board 'observer))))
    (when (e-board-observer board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((observer (e-board-observer--create
                     :id id :board-id (e-board-id board) :client-id client-id
                     :client-generation client-generation
                     :selector (copy-tree selector) :state state
                     :next-seq start-seq
                     :next-index (e-board--message-count-through-seq
                                  board start-seq)
                     :history-before-seq history-before-seq
                     :history-before-index nil
                     :history-floor history-floor)))
      (puthash id observer (e-board-observers board))
      (e-board--append-event board 'observer-added
                             (list :observer-id id :client-id client-id
                                   :start-seq start-seq))
      observer)))

(cl-defun e-board-observer-prepare-history-page (board observer-id &key (limit 32))
  "Prepare one bounded ascending history page without moving its cursor.
Return =:messages= plus an opaque =:receipt= for
`e-board-observer-accept-history-page'."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'plusp limit)))
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (if-let ((prepared (e-board-observer-prepared-history-page observer)))
        (copy-tree prepared)
        (let ((next-before nil)
              (next-before-index nil)
              (index
               (or (e-board-observer-history-before-index observer)
                   (and (e-board-observer-history-before-seq observer)
                        (e-board--message-count-through-seq
                         board
                         (1- (e-board-observer-history-before-seq observer))))))
              (inspected 0)
              matches)
          (when (and (eq (e-board-observer-state observer) 'active)
                     (e-board-observer-history-before-seq observer))
            (let ((floor (e-board-observer-history-floor observer)))
              (while (and index (> index 0) (< inspected limit))
                (let ((message (gethash index (e-board-message-index-table board))))
                  (if (< (e-board-message-seq message) floor)
                      (setq index 0)
                    (cl-incf inspected)
                    (setq next-before (e-board-message-seq message)
                          next-before-index (1- index))
                    (when (e-board--observer-matches-p board observer message)
                      (push message matches))
                    (cl-decf index))))))
          (let* ((receipt
                  (and next-before
                       (list (e-board-id board) observer-id 'history
                             (e-board-observer-history-before-seq observer)
                             next-before)))
                 (page (list :messages matches :before-seq next-before
                             :before-index next-before-index
                             :receipt receipt)))
            (when receipt
              (setf (e-board-observer-prepared-history-page observer) page))
            (copy-tree page))))))

(defun e-board-observer-accept-history-page (board observer-id receipt)
  "Advance OBSERVER-ID's history cursor after its exact prepared RECEIPT."
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (if (and receipt
             (equal receipt
                    (e-board-observer-last-accepted-history-receipt observer)))
        observer
      (let* ((page (e-board-observer-prepared-history-page observer))
             (expected (and page (plist-get page :receipt)))
             (before-seq (and page (plist-get page :before-seq)))
             (before-index (and page (plist-get page :before-index))))
        (unless (and expected (equal receipt expected))
          (signal 'e-board-error
                  (list "Invalid observer history acceptance" observer-id receipt)))
        (unless (and (eq (e-board-observer-state observer) 'active)
                     (integerp before-seq)
                     (e-board-observer-history-before-seq observer)
                     (< before-seq (e-board-observer-history-before-seq observer))
                     (>= before-seq (e-board-observer-history-floor observer)))
          (signal 'e-board-error
                  (list "Stale observer history acceptance" observer-id receipt)))
        (setf (e-board-observer-history-before-seq observer) before-seq
              (e-board-observer-history-before-index observer) before-index
              (e-board-observer-prepared-history-page observer) nil
              (e-board-observer-last-accepted-history-receipt observer)
              (copy-tree receipt))
        (e-board--append-event board 'observer-history-page-accepted
                               (list :observer-id observer-id :before-seq before-seq))
        observer))))

(cl-defun e-board-observer-read-history-page (board observer-id &key (limit 32))
  "Synchronously prepare and accept one bounded ascending history page."
  (let* ((page (e-board-observer-prepare-history-page board observer-id :limit limit))
         (receipt (plist-get page :receipt)))
    (when receipt
      (e-board-observer-accept-history-page board observer-id receipt))
    (plist-get page :messages)))

(defun e-board--observer-matches-p (board observer message)
  "Return non-nil when OBSERVER can observe MESSAGE, faulting only itself."
  (let ((selector (e-board-observer-selector observer)))
    (and (or (not (plist-member selector :kind))
             (equal (plist-get selector :kind) (e-board-message-kind message)))
         (or (not (plist-member selector :activity-kind))
             (equal (plist-get selector :activity-kind)
                    (e-board-message-activity-kind message)))
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
    (setf (e-board-observer-state observer) state
          (e-board-observer-prepared-page observer) nil
          (e-board-observer-prepared-history-page observer) nil)
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
              :client-generation (e-board-observer-client-generation observer)
              :start-seq (or start-seq (e-board-observer-next-seq observer)))))
        (e-board--append-event board 'observer-replaced
                               (list :observer-id observer-id
                                     :replacement-id replacement-id))
        replacement))))

(cl-defun e-board-observer-prepare-page (board observer-id &key (limit 32))
  "Prepare one bounded observer page without advancing its live cursor.
Return a plist with =:messages=, =:through-seq=, and an opaque =:receipt=
receipt.  A client queue must call `e-board-observer-accept-page' only after
it accepts this page.  Trusted predicates run here, never from append."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'plusp limit)))
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id))))
        (resnapshot-required nil))
    (when (and (eq (e-board-observer-state observer) 'active)
               (< (e-board-observer-next-seq observer)
                  (1- (e-board-retention-floor board))))
      (e-board--transition-observer board observer 'expired)
      (setq resnapshot-required t))
    (when (eq (e-board-observer-state observer) 'expired)
      (setq resnapshot-required t))
    (if-let ((prepared (e-board-observer-prepared-page observer)))
        (copy-tree prepared)
        (let ((inspected 0)
              (index (1+ (e-board-observer-next-index observer)))
              (through-seq nil)
              (through-index nil)
              matches)
          (when (eq (e-board-observer-state observer) 'active)
            (while (and (<= index (e-board-message-count board))
                        (< inspected limit))
              (let ((message (gethash index (e-board-message-index-table board))))
                (when (>= (e-board-message-seq message)
                          (e-board-retention-floor board))
                  (cl-incf inspected)
                  (setq through-seq (e-board-message-seq message)
                        through-index index)
                  (when (e-board--observer-matches-p board observer message)
                    (push message matches)))
                (cl-incf index))))
          (let* ((receipt
                  (and through-seq
                       (list (e-board-id board) observer-id 'live
                             (e-board-observer-next-seq observer) through-seq)))
                 (page (list :messages (nreverse matches)
                             :through-seq through-seq
                             :through-index through-index
                             :receipt receipt
                             :resnapshot-required resnapshot-required)))
            (when receipt
              (setf (e-board-observer-prepared-page observer) page))
            (copy-tree page))))))

(defun e-board-observer-accept-page (board observer-id receipt)
  "Advance OBSERVER-ID through its exact prepared page RECEIPT.
Only the pinned page currently owned by the observer may advance its cursor;
retrying the last committed receipt is idempotent."
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (if (and receipt
             (equal receipt (e-board-observer-last-accepted-page-receipt observer)))
        observer
      (let* ((page (e-board-observer-prepared-page observer))
             (expected (and page (plist-get page :receipt)))
             (through-seq (and page (plist-get page :through-seq)))
             (through-index (and page (plist-get page :through-index))))
        (unless (and expected (equal receipt expected))
          (signal 'e-board-error
                  (list "Invalid observer page acceptance" observer-id receipt)))
        (unless (and (eq (e-board-observer-state observer) 'active)
                     (integerp through-seq)
                     (> through-seq (e-board-observer-next-seq observer))
                     (<= through-seq (e-board-next-seq board)))
          (signal 'e-board-error
                  (list "Stale observer page acceptance" observer-id receipt)))
        (setf (e-board-observer-next-seq observer) through-seq
              (e-board-observer-next-index observer) through-index
              (e-board-observer-prepared-page observer) nil
              (e-board-observer-last-accepted-page-receipt observer)
              (copy-tree receipt))
        (e-board--append-event board 'observer-page-accepted
                               (list :observer-id observer-id
                                     :through-seq through-seq))
        observer))))

(cl-defun e-board-observer-read-page (board observer-id &key (limit 32))
  "Synchronously prepare and accept one bounded page for OBSERVER-ID.
Asynchronous client adapters should instead use `e-board-observer-prepare-page'
and acknowledge only after their queue accepts the returned page."
  (let* ((page (e-board-observer-prepare-page board observer-id :limit limit))
         (receipt (plist-get page :receipt)))
    (when receipt
      (e-board-observer-accept-page board observer-id receipt))
    (plist-get page :messages)))

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
  (gethash subscription-id (e-board-subscription-id-table board)))

(defun e-board--classification-subscription-current-p (board snapshot)
  "Return the active current subscription represented by frozen SNAPSHOT.
The snapshot keeps historical selector bytes stable, while this lookup fences
later mute, terminal, cancellation, expiry, and replacement transitions."
  (when-let ((current
              (e-board-find-subscription
               board (e-board-subscription-id snapshot))))
    (and (eq (e-board-subscription-state current) 'active)
         current)))

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
    (when (memq state '(muted completed faulted cancelled expired))
      (when-let ((timer (e-board-subscription-readiness-timer subscription)))
        (cancel-timer timer))
      (setf (e-board-subscription-readiness-timer subscription) nil
            (e-board-subscription-accumulator subscription) nil
            (e-board-subscription-readiness-generation subscription)
            (1+ (e-board-subscription-readiness-generation subscription))))
    (when (memq state '(completed faulted cancelled expired))
      (when-let ((timer (e-board-subscription-lifetime-timer subscription)))
        (cancel-timer timer))
      (setf (e-board-subscription-lifetime-timer subscription) nil
            (e-board-subscription-lifetime-generation subscription)
            (1+ (e-board-subscription-lifetime-generation subscription))))
    (e-board--append-event board 'subscription-transition
                           (list :subscription-id (e-board-subscription-id subscription)
                                 :from from :state state))
    (when (memq state '(muted cancelled))
      (e-board--cancel-prepared-activations
       board (e-board-subscription-id subscription) state))
    subscription))

(defun e-board--cancel-prepared-activations (board subscription-id reason)
  "Fence prepared effects owned by SUBSCRIPTION-ID before they can apply."
  (dolist (activation-id
           (gethash subscription-id (e-board-activation-subscription-index board)))
    (when-let ((activation (e-board-activation board activation-id)))
      (when (eq (e-board-activation-state activation) 'prepared)
        (setf (e-board-activation-state activation) 'cancelled)
        (e-board--append-event board 'activation-cancelled
                               (list :activation-id activation-id :reason reason))))))

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
           (state 'active) (readiness nil readiness-supplied-p)
           (firing-limit nil firing-limit-supplied-p)
           (lifetime nil lifetime-supplied-p))
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
      (let* ((replacement-effect
              (if effect-supplied-p effect (e-board-subscription-effect subscription)))
             (replacement-readiness
             (if readiness-supplied-p readiness
                (e-board-subscription-readiness subscription)))
             (replacement-firing-limit
              (if firing-limit-supplied-p firing-limit
                (e-board-subscription-firing-limit subscription)))
             (replacement-lifetime
              (if lifetime-supplied-p lifetime
                (e-board-subscription-lifetime subscription))))
        (unless (or (eq replacement-effect 'create-pickup)
                    (and (listp replacement-effect)
                         (eq (car replacement-effect) :post-input)))
          (signal 'e-board-error
                  (list "Unsupported board effect" replacement-effect)))
        (e-board--validate-continuation-readiness
         replacement-effect replacement-readiness)
        (when replacement-firing-limit
          (unless (and (integerp replacement-firing-limit)
                       (> replacement-firing-limit 0))
            (signal 'wrong-type-argument
                    (list 'plusp replacement-firing-limit)))
          (unless (and (listp replacement-effect)
                       (eq (car replacement-effect) :post-input))
            (signal 'e-board-error
                    (list "Firing limit requires post-input effect"
                          replacement-firing-limit))))
        (when replacement-lifetime
          (unless (and (numberp replacement-lifetime)
                       (> replacement-lifetime 0))
            (signal 'wrong-type-argument
                    (list 'plusp replacement-lifetime))))
        (unless (memq (e-board-subscription-state subscription)
                      '(completed faulted cancelled expired))
          (e-board--transition-subscription board subscription 'cancelled))
        (let ((replacement
               (e-board-subscribe
                board (e-board-subscription-participant-id subscription) selector
                :id replacement-id :state state :effect replacement-effect
                :readiness replacement-readiness
                :firing-limit replacement-firing-limit
                :lifetime replacement-lifetime)))
          (e-board--append-event
           board 'subscription-replaced
           (list :subscription-id subscription-id
                 :replacement-id replacement-id))
          replacement)))))

(defun e-board--schedule-continuation-timer (board seconds callback)
  "Schedule CALLBACK after SECONDS without letting it apply a continuation."
  (if-let ((scheduler (e-board-continuation-timer-scheduler board)))
      (funcall scheduler seconds callback)
    (run-at-time seconds nil callback)))

(defun e-board--schedule-subscription-timer (board seconds callback)
  "Schedule a subscription lifecycle CALLBACK after SECONDS."
  (if-let ((scheduler (e-board-subscription-timer-scheduler board)))
      (funcall scheduler seconds callback)
    (run-at-time seconds nil callback)))

(defun e-board--schedule-subscription-replay (board)
  "Schedule one bounded retained-continuation replay drain for BOARD."
  (unless (e-board-subscription-replay-scheduled board)
    (setf (e-board-subscription-replay-scheduled board) t)
    (e-board--schedule-effect
     board (lambda () (e-board-drain-subscription-replays board)))))

(defun e-board--queue-subscription-replay (board subscription start-seq)
  "Freeze SUBSCRIPTION and queue its explicit retained post-input replay.
START-SEQ is exclusive.  The captured high watermark isolates the replay from
ordinary future routing, which retains its existing append-time classifier."
  (let ((record (e-board-subscription-replay--create
                 :subscription (copy-e-board-subscription subscription)
                 :next-seq (1+ start-seq)
                 :through-seq (e-board-next-seq board))))
    (setf (e-board-subscription-replays board)
          (or (e-board-subscription-replays board) (list record)))
    (if-let ((tail (e-board-subscription-replay-tail board)))
        (let ((cell (list record)))
          (setcdr tail cell)
          (setf (e-board-subscription-replay-tail board) cell))
      (setf (e-board-subscription-replay-tail board)
            (e-board-subscription-replays board)))
    (e-board--append-event
     board 'subscription-replay-requested
     (list :subscription-id (e-board-subscription-id subscription)
           :start-seq start-seq :through-seq (e-board-subscription-replay-through-seq record)))
    (e-board--schedule-subscription-replay board)))

(defun e-board-drain-subscription-replays (board)
  "Classify one bounded page of explicit retained post-input replays."
  (setf (e-board-subscription-replay-scheduled board) nil)
  (let ((remaining e-board-subscription-replay-drain-limit))
    (while (and (> remaining 0) (e-board-subscription-replays board))
      (let* ((record (car (e-board-subscription-replays board)))
             (next-seq (e-board-subscription-replay-next-seq record)))
        (if (> next-seq (e-board-subscription-replay-through-seq record))
            (progn
              (setf (e-board-subscription-replays board)
                    (cdr (e-board-subscription-replays board)))
              (unless (e-board-subscription-replays board)
                (setf (e-board-subscription-replay-tail board) nil))
              (e-board--append-event
               board 'subscription-replay-complete
               (list :subscription-id
                     (e-board-subscription-id
                      (e-board-subscription-replay-subscription record)))))
          (setf (e-board-subscription-replay-next-seq record) (1+ next-seq))
          (when-let ((message (gethash next-seq (e-board-message-seq-table board))))
            (condition-case err
                (when (e-board--message-subscription-matches-p
                       board (e-board-subscription-replay-subscription record) message)
                  (e-board--accept-post-input-match
                   board (e-board-subscription-replay-subscription record) message))
              (error
               (e-board--fault-subscription
                board (e-board-subscription-replay-subscription record) err))))))
        (cl-decf remaining)))
    (when (e-board-subscription-replays board)
      (e-board--schedule-subscription-replay board)))

(defun e-board--queue-subscription-expiry (board subscription-id generation)
  "Queue one generation-fenced expiry transition outside the timer callback."
  (e-board--schedule-effect
   board
   (lambda ()
     (when-let ((subscription (e-board-find-subscription board subscription-id)))
       (when (and (= generation
                     (e-board-subscription-lifetime-generation subscription))
                  (memq (e-board-subscription-state subscription) '(active muted)))
         (e-board--transition-subscription board subscription 'expired))))))

(defun e-board--fire-post-input (board subscription message-or-messages)
  "Reserve one post-input firing and schedule it from frozen matched records.
The firing reservation is part of the board reducer, so a bounded
FIRING-LIMIT fences later classifier work before it can create another effect."
  (when (eq (e-board-subscription-state subscription) 'active)
    (let ((firing-number (1+ (e-board-subscription-firing-number subscription))))
      (when (or (null (e-board-subscription-firing-limit subscription))
                (<= firing-number
                    (e-board-subscription-firing-limit subscription)))
        (setf (e-board-subscription-firing-number subscription) firing-number)
        (e-board--schedule-post-input board subscription message-or-messages
                                      firing-number)
        (when (and (e-board-subscription-firing-limit subscription)
                   (= firing-number
                      (e-board-subscription-firing-limit subscription)))
          (e-board--transition-subscription board subscription 'completed))
        firing-number))))

(defun e-board--flush-continuation-accumulator (board subscription-id generation)
  "Freeze the current matching source ids for one deferred continuation effect."
  (when-let ((subscription (e-board-find-subscription board subscription-id)))
    (when (and (eq (e-board-subscription-state subscription) 'active)
               (= generation (e-board-subscription-readiness-generation subscription))
               (e-board-subscription-accumulator subscription))
      (when-let ((timer (e-board-subscription-readiness-timer subscription)))
        (cancel-timer timer))
      (let ((message-ids (e-board-subscription-accumulator subscription)))
        (setf (e-board-subscription-readiness-timer subscription) nil
              (e-board-subscription-accumulator subscription) nil
              (e-board-subscription-readiness-generation subscription)
              (1+ (e-board-subscription-readiness-generation subscription)))
        (e-board--fire-post-input
         board subscription
         (mapcar (lambda (message-id) (e-board-message board message-id)) message-ids))))))

(defun e-board--queue-continuation-accumulator-flush
    (board subscription-id generation)
  "Queue a fenced continuation accumulator flush through BOARD's effect drain."
  (e-board--schedule-effect
   board
   (lambda ()
     (e-board--flush-continuation-accumulator board subscription-id generation))))

(defun e-board--accept-post-input-match (board frozen-subscription message)
  "Record MESSAGE for FROZEN-SUBSCRIPTION's current continuation policy.
Classifier snapshots decide matching, while the current subscription fences a
later mute, replacement, or cancellation before any accumulator mutation."
  (when-let ((subscription
              (e-board-find-subscription board
                                         (e-board-subscription-id frozen-subscription))))
    (when (and (eq (e-board-subscription-state subscription) 'active)
               (e-board--authorize-classification
                board subscription message 'effect-preparation))
      (let ((readiness (e-board-subscription-readiness subscription)))
        (if (null readiness)
            (e-board--fire-post-input board subscription message)
          (pcase (plist-get readiness :policy)
            ('batch
             (let* ((generation (if (e-board-subscription-accumulator subscription)
                                    (e-board-subscription-readiness-generation subscription)
                                  (1+ (e-board-subscription-readiness-generation subscription))))
                    (accumulator
                       (append (e-board-subscription-accumulator subscription)
                               (list (e-board-message-id message))))
                      (count (plist-get readiness :count))
                      (max-delay (plist-get readiness :max-delay)))
               (setf (e-board-subscription-readiness-generation subscription) generation
                     (e-board-subscription-accumulator subscription) accumulator)
                 (cond
                  ((>= (length accumulator) count)
                   (e-board--flush-continuation-accumulator
                    board (e-board-subscription-id subscription) generation))
                  ((and max-delay
                        (null (e-board-subscription-readiness-timer subscription)))
                   (setf (e-board-subscription-readiness-timer subscription)
                         (e-board--schedule-continuation-timer
                          board max-delay
                          (lambda ()
                            (e-board--queue-continuation-accumulator-flush
                             board (e-board-subscription-id subscription) generation))))))))
            ('latest-after-quiet
             (let ((generation (1+ (e-board-subscription-readiness-generation subscription))))
               (when-let ((timer (e-board-subscription-readiness-timer subscription)))
                 (cancel-timer timer))
               (setf (e-board-subscription-readiness-generation subscription) generation
                     (e-board-subscription-accumulator subscription)
                     (list (e-board-message-id message))
                     (e-board-subscription-readiness-timer subscription)
                     (e-board--schedule-continuation-timer
                      board (plist-get readiness :quiet-period)
                      (lambda ()
                        (e-board--queue-continuation-accumulator-flush
                         board (e-board-subscription-id subscription) generation))))))))))))

(defun e-board--schedule-post-input (board subscription message-or-messages &optional firing-number)
  "Freeze and schedule SUBSCRIPTION's declarative post from matched records."
  (let* ((messages (if (listp message-or-messages)
                       message-or-messages
                     (list message-or-messages)))
         (message (car messages))
         (message-ids (mapcar #'e-board-message-id messages))
         (effect (cdr (e-board-subscription-effect subscription)))
         (attributes (copy-tree (plist-get effect :attributes)))
         (lineage (append (copy-sequence
                           (plist-get (e-board-message-attributes message)
                                      :board-subscription-lineage))
                          (list (e-board-subscription-id subscription))))
         (activation-id (if firing-number
                            (list (e-board-id board)
                                  (e-board-subscription-id subscription)
                                  'continuation firing-number)
                          (list (e-board-id board) (e-board-subscription-id subscription)
                                (e-board-message-id message))))
         (activation (e-board-activation--create
                      :id activation-id
                      :subscription-id (e-board-subscription-id subscription)
                      :message-id (e-board-message-id message)
                      :effect 'post-input :state 'prepared)))
    (if (> (length lineage) e-board-max-derived-hops)
        (e-board--append-event
         board 'effect-stopped
         (list :activation-id activation-id :reason 'causal-hop-limit))
      (puthash activation-id activation (e-board-activations board))
      (puthash (e-board-subscription-id subscription)
               (append (gethash (e-board-subscription-id subscription)
                                (e-board-activation-subscription-index board))
                       (list activation-id))
               (e-board-activation-subscription-index board))
      (e-board--append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :subscription-id (e-board-subscription-id subscription)
             :effect 'post-input))
      (e-board--schedule-effect
       board
       (lambda ()
         (when (eq (e-board-activation-state activation) 'prepared)
           (setf (e-board-activation-state activation) 'applying)
           (e-board--append-event
            board 'activation-applying (list :activation-id activation-id))
           (condition-case err
               (let ((publication
                      (e-board-post-input
                       board
                       :author (or (plist-get effect :author)
                                   (format "participant:%s"
                                           (e-board-subscription-participant-id subscription)))
                       :requester-actor
                       (list 'participant
                             (e-board-subscription-participant-id subscription))
                       :tags (copy-tree (plist-get effect :tags))
                       :attributes
                       (append attributes
                               (list :board-subscription-lineage lineage
                                     :board-subscription-source-message-ids message-ids))
                       :to (plist-get effect :to)
                       :mode (or (plist-get effect :mode) 'inject)
                       :content (plist-get effect :content)
                       :reference (plist-get effect :reference)
                       :source-input-key
                       (list (e-board-subscription-id subscription)
                             (or firing-number 1)
                             (e-board-message-seq message)))))
                 (setf (e-board-activation-state activation) 'committed)
                 (e-board--append-event
                  board 'effect-committed
                  (list :activation-id activation-id :effect 'post-input
                        :message-id (and (e-board-publication-message publication)
                                         (e-board-message-id
                                          (e-board-publication-message publication))))))
             (error
              (setf (e-board-activation-state activation) 'failed)
              (e-board--append-event
               board 'effect-failed
               (list :activation-id activation-id :error err))))))))))

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

(defun e-board--freeze-envelope-value (value field byte-limit)
  "Deep-copy VALUE for retained FIELD while enforcing BYTE-LIMIT.
The walk charges atomic payload bytes and one byte per container cell.  It
stops as soon as the field exceeds its budget, rejects cyclic structures, and
copies strings, conses, vectors, and hash tables so later caller mutation
cannot rewrite retained board state."
  (let ((remaining byte-limit)
        (visiting (make-hash-table :test 'eq)))
    (cl-labels
        ((charge (amount)
           (setq remaining (- remaining amount))
           (when (< remaining 0)
             (signal 'e-board-envelope-too-large
                     (list field byte-limit))))
         (copy-value (current)
           (cond
            ((stringp current)
             (charge (string-bytes current))
             (copy-sequence current))
            ((symbolp current)
             (charge (length (symbol-name current)))
             current)
            ((numberp current)
             (charge 16)
             current)
            ((consp current)
             (when (gethash current visiting)
               (signal 'e-board-invalid-envelope (list field 'cyclic-cons)))
             (charge 1)
             (puthash current t visiting)
             (unwind-protect
                 (cons (copy-value (car current))
                       (copy-value (cdr current)))
               (remhash current visiting)))
            ((vectorp current)
             (when (gethash current visiting)
               (signal 'e-board-invalid-envelope (list field 'cyclic-vector)))
             (charge (length current))
             (puthash current t visiting)
             (unwind-protect
                 (let ((copy (make-vector (length current) nil)))
                   (dotimes (index (length current))
                     (aset copy index (copy-value (aref current index))))
                   copy)
               (remhash current visiting)))
            ((hash-table-p current)
             (when (gethash current visiting)
               (signal 'e-board-invalid-envelope (list field 'cyclic-hash-table)))
             (charge (hash-table-count current))
             (puthash current t visiting)
             (unwind-protect
                 (let ((copy (make-hash-table :test (hash-table-test current)
                                              :size (hash-table-count current))))
                   (maphash (lambda (key item)
                              (puthash (copy-value key) (copy-value item) copy))
                            current)
                   copy)
               (remhash current visiting)))
            (t
             (signal 'e-board-invalid-envelope
                     (list field (type-of current)))))))
      (copy-value value))))

(defun e-board--make-message (board kind id author requester-actor
                                     tags attributes to mode content reference
                                     source-input-key source-output-key
                                     reply-to-message-ids caused-by-delivery-ids
                                     &optional source-activity-key source-fact-key
                                     subject-participant-id source-turn-id activity-kind)
  "Create and record one immutable BOARD message, returning it."
  (when (e-board-message board id)
    (signal 'e-board-id-conflict (list id)))
  (let* ((frozen-author
          (e-board--freeze-envelope-value
           author 'author e-board-message-metadata-byte-limit))
         (frozen-requester
          (e-board--freeze-envelope-value
           requester-actor 'requester-actor e-board-message-metadata-byte-limit))
         (frozen-tags
          (e-board--freeze-envelope-value
           tags 'tags e-board-message-tags-byte-limit))
         (frozen-attributes
          (e-board--freeze-envelope-value
           attributes 'attributes e-board-message-attributes-byte-limit))
         (frozen-to
          (e-board--freeze-envelope-value
           to 'to e-board-message-metadata-byte-limit))
         (frozen-content
          (e-board--freeze-envelope-value
           content 'content e-board-message-content-byte-limit))
         (frozen-reference
          (e-board--freeze-envelope-value
           reference 'reference e-board-message-reference-byte-limit))
         (frozen-source-input-key
          (e-board--freeze-envelope-value
           source-input-key 'source-input-key e-board-message-metadata-byte-limit))
         (frozen-source-output-key
          (e-board--freeze-envelope-value
           source-output-key 'source-output-key e-board-message-metadata-byte-limit))
         (frozen-reply-to-message-ids
          (e-board--freeze-envelope-value
           reply-to-message-ids 'reply-to-message-ids
           e-board-message-metadata-byte-limit))
         (frozen-caused-by-delivery-ids
          (e-board--freeze-envelope-value
           caused-by-delivery-ids 'caused-by-delivery-ids
           e-board-message-metadata-byte-limit))
         (frozen-source-activity-key
          (e-board--freeze-envelope-value
           source-activity-key 'source-activity-key
           e-board-message-metadata-byte-limit))
         (frozen-source-fact-key
          (e-board--freeze-envelope-value
           source-fact-key 'source-fact-key e-board-message-metadata-byte-limit))
         (frozen-subject-participant-id
          (e-board--freeze-envelope-value
           subject-participant-id 'subject-participant-id
           e-board-message-metadata-byte-limit))
         (frozen-source-turn-id
          (e-board--freeze-envelope-value
           source-turn-id 'source-turn-id e-board-message-metadata-byte-limit))
         (event (e-board--append-event
                 board (intern (format "%s-posted" kind))
                 (list :message-id id)))
         (message
          (e-board-message--create
           :id id :board-id (e-board-id board) :seq (e-board-event-seq event)
           :kind kind :author frozen-author :requester-actor frozen-requester
           :tags frozen-tags
           :attributes frozen-attributes :to frozen-to :mode mode
           :content frozen-content :reference frozen-reference
           :source-input-key frozen-source-input-key
           :source-output-key frozen-source-output-key
           :reply-to-message-ids frozen-reply-to-message-ids
           :caused-by-delivery-ids frozen-caused-by-delivery-ids
           :source-activity-key frozen-source-activity-key
           :source-fact-key frozen-source-fact-key
           :subject-participant-id frozen-subject-participant-id
           :source-turn-id frozen-source-turn-id
           :activity-kind activity-kind
           :routing-state (and (eq kind 'input) 'routing))))
    (puthash id message (e-board-message-table board))
    (puthash (e-board-message-seq message) message (e-board-message-seq-table board))
    (let* ((index (cl-incf (e-board-message-count board)))
           (cell (list message)))
      (if (e-board-messages-tail board)
          (setcdr (e-board-messages-tail board) cell)
        (setf (e-board-messages board) cell))
      (setf (e-board-messages-tail board) cell)
      (puthash index message (e-board-message-index-table board))
      (puthash (e-board-message-seq message) index
               (e-board-event-message-count board)))
    ;; Notification runs only after the immutable append.  Adapters must do no
    ;; more than enqueue one bounded wake token; they must not re-enter board
    ;; mutation here.
    (when-let ((notify (e-board-message-notification-function board)))
      (condition-case err
          (funcall notify board message)
        (error
         (e-board--append-event
          board 'message-notification-failed
          (list :message-id (e-board-message-id message) :error err)))))
    message))

(defun e-board--authorize-classification (board subscription message phase)
  "Authorize SUBSCRIPTION's access to MESSAGE at deferred reducer PHASE."
  (if-let ((authorizer (e-board-classification-authorizer board)))
      (if (funcall authorizer subscription message phase)
          t
        (e-board--append-event
         board 'authorization-revoked
         (list :subscription-id (e-board-subscription-id subscription)
               :message-id (e-board-message-id message) :phase phase))
        nil)
    t))

(defun e-board--message-subscription-matches-p (board subscription message)
  "Classify one frozen SUBSCRIPTION against MESSAGE in a later router turn.
Only input records may create pickups.  An explicit `:post-input' continuation
may instead match any unaddressed board record, including output, activity, and
fact publications."
  (cond
   ((eq (e-board-subscription-effect subscription) 'create-pickup)
    (and (eq (e-board-message-kind message) 'input)
         (e-board--eligible-subscription-p board subscription)
         (e-board--authorize-classification
          board subscription message 'selector)
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
    (and (e-board--authorize-classification
          board subscription message 'selector)
         (let ((lineage (plist-get (e-board-message-attributes message)
                                   :board-subscription-lineage)))
           (and (not (member (e-board-subscription-id subscription) lineage))
                (e-board--selector-matches-p board subscription message)))))))

(defun e-board--pickup-cause-metadata (message)
  "Return MESSAGE's bounded causal fields for one logical pickup envelope."
  (let ((attributes (e-board-message-attributes message)))
    (list :reply-to-message-ids
          (copy-tree (e-board-message-reply-to-message-ids message))
          :caused-by-delivery-ids
          (copy-tree (e-board-message-caused-by-delivery-ids message))
          :source-input-key
          (copy-tree (e-board-message-source-input-key message))
          :routing-tags
          (copy-tree (e-board-message-tags message))
          :input-attributes
          (copy-tree attributes)
          :subscription-lineage
          (copy-tree (plist-get attributes :board-subscription-lineage))
          :source-message-ids
          (copy-tree
           (plist-get attributes :board-subscription-source-message-ids)))))

(defun e-board--copy-envelope-value (value)
  "Recursively copy mutable sequence storage in logical envelope VALUE."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value)
    (cons (e-board--copy-envelope-value (car value))
          (e-board--copy-envelope-value (cdr value))))
   ((vectorp value)
    (apply #'vector (mapcar #'e-board--copy-envelope-value value)))
   (t value)))

(defun e-board--finalize-input-classification
    (board message publication subscriptions post-subscriptions)
  "Commit MESSAGE's deferred pickups and continuation effects.
Only input records receive a routing projection or create pickups; every
message kind may schedule its frozen explicit continuation matches."
  (let ((subscriptions (nreverse subscriptions))
        (post-subscriptions (nreverse post-subscriptions))
        (by-participant (make-hash-table :test 'equal))
        participant-ids pickup-ids)
    (setq subscriptions
          (cl-remove-if-not
           (lambda (subscription)
             (when-let ((current
                         (e-board--classification-subscription-current-p
                          board subscription)))
               (e-board--authorize-classification
                board current message 'pickup-finalization)))
           subscriptions))
    (when (eq (e-board-message-kind message) 'input)
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
                   :participant-id participant-id
                   :message-id (e-board-message-id message)
                   :subscription-ids
                   (copy-sequence (gethash participant-id by-participant))
                   :event-seq-range (list (e-board-message-seq message)
                                          (e-board-message-seq message))
                   :mode (e-board-message-mode message)
                   :requester-actor
                   (e-board--copy-envelope-value
                    (e-board-message-requester-actor message))
                   :addressed-p (and (e-board-message-to message) t)
                   :cause-metadata
                   (e-board--copy-envelope-value
                    (e-board--pickup-cause-metadata message))
                   :content
                   (e-board--copy-envelope-value
                    (e-board-message-content message))
                   :reference
                   (e-board--copy-envelope-value
                    (e-board-message-reference message)))))
            (puthash delivery-id pickup (e-board-pickups board))
            (e-board--enqueue-pickup board pickup)
            (setq pickup-ids (append pickup-ids (list delivery-id)))))
        (setf (e-board-message-matching-participant-ids message) participant-ids
              (e-board-message-pickup-ids message) pickup-ids
              (e-board-message-routing-state message) 'routed)
        (e-board--append-event board 'input-routed
                               (list :message-id (e-board-message-id message)
                                     :participant-ids participant-ids
                                     :pickup-ids pickup-ids))))
    (dolist (subscription post-subscriptions)
      (when-let ((current
                  (e-board--classification-subscription-current-p
                   board subscription)))
        (when (e-board--authorize-classification
               board current message 'effect-finalization)
          (e-board--accept-post-input-match board current message))))
    (when (eq (e-board-message-kind message) 'input)
      (let ((cell (list (list (e-board-message-id message) pickup-ids))))
        (if (e-board-routed-pickup-results-tail board)
            (setcdr (e-board-routed-pickup-results-tail board) cell)
          (setf (e-board-routed-pickup-results board) cell))
        (setf (e-board-routed-pickup-results-tail board) cell))
      (setf (e-board-publication-pickup-ids publication) pickup-ids))))

(defun e-board--schedule-input-classification (board)
  "Schedule BOARD's frozen input classifier once after append returns."
  (unless (e-board-input-classification-scheduled board)
    (setf (e-board-input-classification-scheduled board) t)
    (if-let ((scheduler (e-board-input-classification-scheduler board)))
        (funcall scheduler (lambda () (e-board-drain-input-classifications board)))
      (run-at-time 0 nil (lambda () (e-board-drain-input-classifications board))))))

(defun e-board--queue-input-classification (board message publication)
  "Freeze BOARD's subscription view for MESSAGE without matching on append.
The legacy name reflects its original pickup-only caller; non-input records use
the same bounded queue solely to classify explicit continuation subscriptions."
  (let ((cell
         (list (e-board-input-classification--create
                :message message
                :subscription-count (e-board-subscription-count board)
                :index 0 :publication publication
                :matches nil :post-subscriptions nil :post-index 0))))
    (if (e-board-input-classification-tail board)
        (setcdr (e-board-input-classification-tail board) cell)
      (setf (e-board-input-classifications board) cell))
    (setf (e-board-input-classification-tail board) cell))
  (e-board--adjust-unsettled board 'routing 1)
  (e-board--schedule-input-classification board))

(defun e-board--fail-input-classification (board record err)
  "Stop RECORD before pickup commit after a core classifier ERR."
  (let ((message (e-board-input-classification-message record)))
    (if (eq (e-board-message-kind message) 'input)
        (progn
          (setf (e-board-message-routing-state message) 'routing-failed)
          (e-board--append-event
           board 'input-routing-failed
           (list :message-id (e-board-message-id message) :error err)))
      (e-board--append-event
       board 'continuation-classification-failed
       (list :message-id (e-board-message-id message) :error err)))
    (setf (e-board-input-classifications board)
          (cdr (e-board-input-classifications board)))
    (e-board--adjust-unsettled board 'routing -1)
    (unless (e-board-input-classifications board)
      (setf (e-board-input-classification-tail board) nil))))

(defun e-board-drain-input-classifications (board)
  "Classify a bounded page of frozen input subscriptions in board order."
  (setf (e-board-input-classification-scheduled board) nil)
  (let ((remaining e-board-input-classification-drain-limit))
    (while (and (> remaining 0) (e-board-input-classifications board))
      (let* ((record (car (e-board-input-classifications board)))
             (subscription-count
              (e-board-input-classification-subscription-count record))
             (index (e-board-input-classification-index record))
             (message (e-board-input-classification-message record)))
        (if (< index subscription-count)
            (let ((subscription
                   (copy-e-board-subscription
                    (gethash (1+ index)
                             (e-board-subscription-index-table board)))))
              (setf (e-board-input-classification-index record) (1+ index))
              (condition-case err
                  (when (e-board--message-subscription-matches-p board subscription message)
                    (if (eq (e-board-subscription-effect subscription) 'create-pickup)
                        (push subscription (e-board-input-classification-matches record))
                      (push subscription
                            (e-board-input-classification-post-subscriptions record))))
                (error
                 (e-board--fail-input-classification board record err))))
          (e-board--finalize-input-classification
           board message (e-board-input-classification-publication record)
           (e-board-input-classification-matches record)
           (e-board-input-classification-post-subscriptions record))
          (setf (e-board-input-classifications board)
                (cdr (e-board-input-classifications board)))
          (e-board--adjust-unsettled board 'routing -1)
          (unless (e-board-input-classifications board)
            (setf (e-board-input-classification-tail board) nil)))
        (cl-decf remaining)))
    (when (e-board-input-classifications board)
      (e-board--schedule-input-classification board))))

(defun e-board-drain-routed-pickups (board)
  "Return and clear finalized pickup ids for separately scheduled delivery."
  (prog1 (e-board-routed-pickup-results board)
    (setf (e-board-routed-pickup-results board) nil
          (e-board-routed-pickup-results-tail board) nil)))

(cl-defun e-board-post-input
    (board &key id author requester-actor tags attributes to (mode 'inject)
           content reference source-input-key)
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
                        board 'input id author requester-actor
                        tags attributes to mode content reference
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
                        board 'output id author nil tags nil nil nil content reference nil
                       source-output-key reply-to-message-ids
                       caused-by-delivery-ids nil nil subject-participant-id
                       source-turn-id nil)))
        (e-board--remember-source board 'output source-output-key message)
        (e-board--close-open-activity
         board subject-participant-id source-turn-id message)
        (let ((publication (e-board-publication--create
                            :status 'posted :message message :pickup-ids nil)))
          (e-board--queue-input-classification board message publication)
          publication))))

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
               board 'activity id author nil tags attributes nil nil content reference
               nil nil reply-to-message-ids caused-by-delivery-ids
               source-activity-key nil subject-participant-id source-turn-id
               activity-kind)))
        (e-board--remember-source board 'activity source-activity-key message)
        (e-board--record-open-activity board message)
        (let ((publication (e-board-publication--create
                            :status 'posted :message message :pickup-ids nil)))
          (e-board--queue-input-classification board message publication)
          publication))))

(cl-defun e-board-post-fact
    (board &key id author tags attributes content reference source-fact-key)
  "Append one source-keyed, observation-only board fact message."
  (unless source-fact-key
    (signal 'e-board-invalid-source-key (list source-fact-key)))
  (or (e-board--source-publication board 'fact source-fact-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message
              (e-board--make-message
               board 'fact id author nil tags attributes nil nil content reference
               nil nil nil nil nil source-fact-key nil nil nil)))
        (e-board--remember-source board 'fact source-fact-key message)
        (let ((publication (e-board-publication--create
                            :status 'posted :message message :pickup-ids nil)))
          (e-board--queue-input-classification board message publication)
          publication))))

(defun e-board-unrouted-inputs (board)
  "Return retained BOARD input messages that have a visible unrouted reason."
  (cl-remove-if-not
   (lambda (message)
     (and (eq (e-board-message-kind message) 'input)
          (e-board-message-unrouted-reason message)))
   (e-board-messages board)))

(provide 'e-board)

;;; e-board.el ends here

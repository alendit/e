;;; e-board-state.el --- Stable Board state/value contract for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains only the Board domain's stable state and value shapes.
;; Admission receipts and their mutation protocol live in `e-board-admission'.
;; Keeping these definitions below both modules gives the admission owner a
;; cycle-free, independently loadable contract without moving semantic state
;; out of the Board domain.

;;; Code:

(require 'cl-lib)

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
(define-error 'e-board-admission-pending
  "Board admission cleanup remains pending" 'e-board-error)
(define-error 'e-board-mutation-frozen
  "Board durable commit is in progress" 'e-board-error)

(cl-defstruct (e-board-message
               (:constructor e-board-message--create)
               (:conc-name e-board-message-))
  id board-id seq kind author requester-actor tags attributes to mode content reference
  source-input-key source-output-key reply-to-message-ids caused-by-delivery-ids
  source-activity-key source-fact-key subject-participant-id source-turn-id
  activity-kind matching-participant-ids pickup-ids unrouted-reason routing-state
  created-at durable-position)

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
  delivery priority self-delivery failure-policy
  readiness accumulator readiness-timer readiness-generation firing-number
  firing-limit lifetime lifetime-timer lifetime-generation lifetime-token)

(cl-defstruct (e-board-processing-chain
               (:constructor e-board-processing-chain--create)
               (:conc-name e-board-processing-chain-))
  id board-id root-message-id candidate-message-id caused-by-message-id
  processor-history processing-depth created-at)

(cl-defstruct (e-board-processing-result
               (:constructor e-board-processing-result--create)
               (:conc-name e-board-processing-result-))
  id board-id chain-id subscription-id participant-id candidate-message-id
  outcome replacement-message-id failure-policy failure created-at)

(cl-defstruct (e-board-observer
               (:constructor e-board-observer--create)
               (:conc-name e-board-observer-))
  id board-id client-id client-generation selector state next-seq next-index
  history-before-seq history-before-index history-floor
  prepared-page last-accepted-page-receipt
  prepared-history-page last-accepted-history-receipt)

(cl-defstruct (e-board-delivery-attempt
               (:constructor e-board-delivery-attempt--create)
               (:conc-name e-board-delivery-attempt-))
  number endpoint-token composite-generation state receipt reason)

(cl-defstruct (e-board-pickup
               (:constructor e-board-pickup--create)
               (:conc-name e-board-pickup-))
  delivery-id board-id participant-id message-id subscription-ids
  participant-lifetime event-seq-range mode requester-actor addressed-p
  cause-metadata content reference fifo-position revision state attempt)

(cl-defstruct (e-board-publication
               (:constructor e-board-publication--create)
               (:conc-name e-board-publication-))
  status message pickup-ids)

(cl-defstruct (e-board-work
                (:constructor e-board-work--create)
                (:conc-name e-board-work-))
  id handle metadata state terminal-seq terminal-payload
  publication-observer posted-event)

(cl-defstruct (e-board-invocation
               (:constructor e-board-invocation--create)
               (:conc-name e-board-invocation-))
  id work-id state effect-target activation-id subscription-event
  settlement-admission)

(cl-defstruct (e-board-aggregation
                (:constructor e-board-aggregation--create)
                (:conc-name e-board-aggregation-))
  id work-ids mode state effect-target timer activation-id admission
  settlement-admission)

(cl-defstruct (e-board-activation
               (:constructor e-board-activation--create)
               (:conc-name e-board-activation-))
  id subscription-id subscription-token message-id effect state
  event-receipt effect-receipt)

(cl-defstruct (e-board-open-activity
               (:constructor e-board-open-activity--create)
               (:conc-name e-board-open-activity-))
  participant-id turn-id message-id seq activity-kind)

(cl-defstruct (e-board-terminal-classification
               (:constructor e-board-terminal-classification--create)
               (:conc-name e-board-terminal-classification-))
  work-id work invocation-ids invocation-objects aggregation-ids aggregation-objects)

(cl-defstruct (e-board-id-queue
               (:constructor e-board-id-queue--create)
               (:conc-name e-board-id-queue-))
  head tail node-index)

(cl-defstruct (e-board-input-classification
               (:constructor e-board-input-classification--create)
               (:conc-name e-board-input-classification-))
  message publication subscription-count index
  matches matches-tail post-subscriptions post-subscriptions-tail
  phase cursor by-participant participant-ids participant-ids-tail participant-count
  authorized-routes prepared-pickups prepared-pickups-tail pickup-ids pickup-ids-tail
  overflow-reason)

(cl-defstruct (e-board-subscription-bucket
               (:constructor e-board-subscription-bucket--create)
               (:conc-name e-board-subscription-bucket-))
  ids tail count routes routes-tail)

(cl-defstruct (e-board-classification-route
               (:constructor e-board-classification-route--create)
               (:conc-name e-board-classification-route-))
  subscription subscription-token participant)

(cl-defstruct (e-board-subscription-replay
               (:constructor e-board-subscription-replay--create)
               (:conc-name e-board-subscription-replay-))
  subscription next-seq through-seq next-position through-position)

(cl-defstruct (e-board
               (:constructor e-board-state-create)
               (:conc-name e-board-))
  id id-function storage trusted-principal generation revision mutation-frozen-p
  next-seq events events-tail messages messages-tail message-count
  message-table message-seq-table message-index-table event-message-count
  event-message-prefix-high-watermark event-node-index
  pending-admissions
  message-kind-newest-table message-kind-tag-newest-table
  participants subscriptions subscriptions-tail subscription-count
  subscription-index-table subscription-id-table subscription-lifetime-sequence
  processing-chains-internal processing-chains-tail-internal processing-chain-table-internal
  processing-chain-reservations
  processing-results-internal processing-results-tail-internal processing-result-table-internal
  processing-result-reservations processing-record-notification-function
  observers pickups source-high-watermarks source-recent source-signatures
  work-table invocations aggregations
  pending-effects pending-effects-tail effect-node-index effects-scheduled
  effect-schedule-generation effect-draining-p
  effect-callback-generation effect-scheduler invocation-effect-dispatcher
  aggregation-work-index invocation-work-index
  terminal-classifications terminal-classification-tail
  terminal-classification-node-index terminal-classification-scheduled
  terminal-classification-generation terminal-classification-callback-generation
  terminal-classification-scheduler input-classifications input-classification-tail
  input-classification-scheduled input-classification-scheduler subscription-replays
  subscription-replay-tail subscription-replay-scheduled routed-pickup-results
  routed-pickup-results-tail aggregation-deadlines aggregation-deadline-tail
  aggregation-deadline-node-index aggregation-deadline-scheduled
  aggregation-deadline-generation aggregation-deadline-callback-generation
  aggregation-deadline-scheduler continuation-timer-scheduler subscription-timer-scheduler
  activations activation-subscription-index pickup-queues pickup-pending-limit
  open-activities closed-activities retention-floor classification-authorizer
  message-notification-function unsettled-pickup-count unsettled-effect-count
  unsettled-routing-count unsettled-generation unsettled-change-function)

(defun e-board-state-unsettled-state (board)
  "Return BOARD's constant-time unsettled-count projection."
  (list :generation (e-board-unsettled-generation board)
        :pickups (e-board-unsettled-pickup-count board)
        :effects (e-board-unsettled-effect-count board)
        :routing (e-board-unsettled-routing-count board)))

(defun e-board-state-adjust-unsettled (board class delta)
  "Apply one BOARD unsettled-count transition and notify its observer.

This is the low-level Board state contract used by both semantic Board policy
and the admission receipt owner.  It intentionally owns no queue or admission
logic."
  (let* ((current (pcase class
                    ('pickups (e-board-unsettled-pickup-count board))
                    ('effects (e-board-unsettled-effect-count board))
                    ('routing (e-board-unsettled-routing-count board))
                    (_ (signal 'e-board-error
                               (list "Unknown unsettled class" class)))))
         (next (+ current delta)))
    (when (< next 0)
      (signal 'e-board-error (list "Negative board unsettled count" class next)))
    (pcase class
      ('pickups (setf (e-board-unsettled-pickup-count board) next))
      ('effects (setf (e-board-unsettled-effect-count board) next))
      ('routing (setf (e-board-unsettled-routing-count board) next)))
    (cl-incf (e-board-unsettled-generation board))
    (when-let* ((notify (e-board-unsettled-change-function board)))
      (funcall notify board class delta (e-board-state-unsettled-state board)))
    next))

(provide 'e-board-state)

;;; e-board-state.el ends here

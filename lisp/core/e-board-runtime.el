;;; e-board-runtime.el --- Harness delivery adapter for board pickups -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This composition adapter connects registry participants to existing harness
;; sessions.  Board routing remains pure: this module receives only the frozen
;; pending pickups produced by the source board and delivers them through an
;; explicit port.

;;; Code:

(require 'cl-lib)
(require 'e-board-registry)
(require 'e-harness)

(define-error 'e-board-runtime-error "e board runtime error")
(define-error 'e-board-runtime-attachment-exists
  "e board runtime participant is already attached"
  'e-board-runtime-error)
(define-error 'e-board-runtime-session-busy
  "e board runtime session is busy"
  'e-board-runtime-error)
(define-error 'e-board-runtime-session-missing
  "e board runtime session is missing"
  'e-board-runtime-error)

(defvar e-board-runtime--attachments (make-hash-table :test 'equal)
  "Live runtime attachments keyed by board and participant identity.")

(cl-defstruct (e-board-runtime-attachment
               (:constructor e-board-runtime-attachment--create)
               (:conc-name e-board-runtime-attachment-))
  board participant harness session-id delivery-function)

(defun e-board-runtime--attachment-key (board participant)
  "Return the attachment lookup key for BOARD and PARTICIPANT."
  (list (e-board-registry-board-id board)
        (e-board-registry-participant-id participant)))

(defun e-board-runtime--active-board (board-or-id)
  "Return active registry BOARD-OR-ID."
  (let ((board (e-board-registry-get board-or-id)))
    (unless (eq (e-board-registry-board-state board) 'active)
      (signal 'e-board-registry-closed
              (list (e-board-registry-board-id board))))
    board))

(defun e-board-runtime--require-live-session (harness session-id)
  "Return HARNESS's existing SESSION-ID, or signal a runtime-specific error."
  (condition-case nil
      (e-session-get (e-harness-sessions harness) session-id)
    (error (signal 'e-board-runtime-session-missing (list session-id)))))

(defun e-board-runtime--delivery-metadata (pickup)
  "Return harness metadata that identifies the frozen PICKUP."
  (list :input-origin 'board
        :board-delivery-id (copy-tree (e-board-pickup-delivery-id pickup))
        :board-id (e-board-pickup-board-id pickup)
        :board-participant-id (e-board-pickup-participant-id pickup)))

(defun e-board-runtime--deliver-to-harness (attachment pickup message)
  "Deliver PICKUP's MESSAGE through ATTACHMENT's idle harness session.
Queue-mode messages enter the harness follow-up queue.  Inject-mode messages
start an asynchronous prompt only while no active turn owns the session; a
busy session leaves its pickup pending for an explicit later retry."
  (let* ((harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (prompt (e-board-message-content message))
         (metadata (e-board-runtime--delivery-metadata pickup)))
    (unless (and (stringp prompt) (not (string-empty-p prompt)))
      (user-error "Board input content must be a non-empty string"))
    (when (plist-get (e-harness-state harness session-id) :active-turn)
      (signal 'e-board-runtime-session-busy (list session-id)))
    (pcase (e-board-pickup-mode pickup)
      ('queue
       (e-harness-request-follow-up harness session-id prompt :metadata metadata))
      ('inject
       (e-harness-prompt-async harness session-id prompt :metadata metadata)))))

(cl-defun e-board-runtime-attach
    (board-or-id harness session-id
                 &key participant-id author principal delivery-function)
  "Attach existing live HARNESS SESSION-ID to BOARD-OR-ID as one participant.

DELIVERY-FUNCTION is called as (FUNCTION ATTACHMENT PICKUP MESSAGE) for each
frozen pending pickup addressed to the participant.  It must perform one
delivery or signal; normal return marks that pickup delivered.  When omitted,
the conservative idle-only harness delivery port is used."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (or (null delivery-function) (functionp delivery-function))
    (signal 'wrong-type-argument (list 'functionp delivery-function)))
  (e-board-runtime--require-live-session harness session-id)
  (let* ((board (e-board-runtime--active-board board-or-id))
         (participant (e-board-registry-add-participant
                       board :id participant-id :author author :principal principal))
         (key (e-board-runtime--attachment-key board participant)))
    (when (gethash key e-board-runtime--attachments)
      (signal 'e-board-runtime-attachment-exists (list key)))
    (let ((attachment
           (e-board-runtime-attachment--create
            :board board
            :participant participant
            :harness harness
            :session-id session-id
            :delivery-function (or delivery-function
                                   #'e-board-runtime--deliver-to-harness))))
      (puthash key attachment e-board-runtime--attachments)
      attachment)))

(defun e-board-runtime--deliver-pickups (board pickup-ids)
  "Deliver BOARD's frozen pending PICKUP-IDS through their attachments."
  (let ((source-board (e-board-registry-board-source-board board)))
    (dolist (delivery-id pickup-ids)
      (when-let* ((pickup (e-board-pickup source-board delivery-id))
                  ((eq (e-board-pickup-state pickup) 'pending))
                  (participant (e-board-registry-participant
                                board (e-board-pickup-participant-id pickup)))
                  (attachment
                   (gethash (e-board-runtime--attachment-key board participant)
                            e-board-runtime--attachments))
                  (message (e-board-message source-board
                                            (e-board-pickup-message-id pickup))))
        (funcall (e-board-runtime-attachment-delivery-function attachment)
                 attachment pickup message)
        (setf (e-board-pickup-state pickup) 'delivered)))))

(cl-defun e-board-runtime-post-input
    (board-or-id &key id author tags to (mode 'inject) content reference source-input-key)
  "Post one input to BOARD-OR-ID's source board, then deliver its frozen pickups.
The returned value is the source board's `e-board-publication'.  Duplicate
publications only retry pickups that remain pending."
  (let* ((board (e-board-runtime--active-board board-or-id))
         (publication
          (e-board-post-input
           (e-board-registry-board-source-board board)
           :id id :author author :tags tags :to to :mode mode :content content
           :reference reference :source-input-key source-input-key)))
    (e-board-runtime--deliver-pickups board
                                      (e-board-publication-pickup-ids publication))
    publication))

(provide 'e-board-runtime)

;;; e-board-runtime.el ends here

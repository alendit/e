;;; e-board-registry.el --- Process-local board lifecycle registry for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns the process-local board lifecycle and its local identities.
;; `e-board' remains the source-board model for routing and subscriptions.

;;; Code:

(require 'cl-lib)
(require 'e-board)

(define-error 'e-board-registry-error "e board registry error")
(define-error 'e-board-registry-id-conflict "e board registry id conflict"
  'e-board-registry-error)
(define-error 'e-board-registry-missing "e board registry board is missing"
  'e-board-registry-error)
(define-error 'e-board-registry-closed "e board registry board is closed"
  'e-board-registry-error)
(define-error 'e-board-registry-participant-board-local
  "e board registry participant belongs to another board"
  'e-board-registry-error)
(define-error 'e-board-registry-participant-missing
  "e board registry participant is missing"
  'e-board-registry-error)
(define-error 'e-board-registry-client-missing
  "e board registry client is missing"
  'e-board-registry-error)

(defvar e-board-registry--id-sequence 0
  "Process-local fallback sequence for registry-owned identities.")

(defvar e-board-registry--boards (make-hash-table :test 'equal)
  "Live and closed process-local boards keyed by board id.")

(cl-defstruct (e-board-registry-board
                (:constructor e-board-registry-board--create)
                (:conc-name e-board-registry-board-))
  id source-board state author principal id-function clients participants)

(cl-defstruct (e-board-registry-client
                (:constructor e-board-registry-client--create)
                (:conc-name e-board-registry-client-))
  id board-id author principal)

(cl-defstruct (e-board-registry-participant
                (:constructor e-board-registry-participant--create)
                (:conc-name e-board-registry-participant-))
  id board-id author principal source-participant)

(defun e-board-registry--next-id (id-function kind)
  "Return an identity for KIND from ID-FUNCTION or the local fallback."
  (let ((id (if id-function
                 (funcall id-function kind)
              (format "%s%d"
                      (pcase kind
                        ('board "brd_")
                        ('client "cli_")
                        ('participant "ptc_")
                        ('subscription "sub_")
                        (_ (format "%s_" kind)))
                      (cl-incf e-board-registry--id-sequence)))))
    (unless id
      (signal 'e-board-registry-error
              (list "Id generator returned nil" kind)))
    id))

(defun e-board-registry--resolve (board-or-id)
  "Return registered BOARD-OR-ID, or signal `e-board-registry-missing'."
  (let* ((id (if (e-board-registry-board-p board-or-id)
                 (e-board-registry-board-id board-or-id)
               board-or-id))
         (board (gethash id e-board-registry--boards)))
    (unless (and board
                 (or (not (e-board-registry-board-p board-or-id))
                     (eq board board-or-id)))
      (signal 'e-board-registry-missing (list id)))
    board))

(defun e-board-registry--require-active (board-or-id)
  "Return active BOARD-OR-ID, or signal `e-board-registry-closed'."
  (let ((board (e-board-registry--resolve board-or-id)))
    (unless (eq (e-board-registry-board-state board) 'active)
      (signal 'e-board-registry-closed
              (list (e-board-registry-board-id board))))
    board))

(defun e-board-registry--participant (board participant-or-id)
  "Return BOARD's canonical PARTICIPANT-OR-ID record.
Participant records must be the record created for BOARD; this prevents one
board's participant identity from being used to mutate another board."
  (let* ((id (if (e-board-registry-participant-p participant-or-id)
                 (e-board-registry-participant-id participant-or-id)
               participant-or-id))
         (participant (gethash id (e-board-registry-board-participants board))))
    (when (and (e-board-registry-participant-p participant-or-id)
               (not (equal (e-board-registry-participant-board-id participant-or-id)
                           (e-board-registry-board-id board))))
      (signal 'e-board-registry-participant-board-local (list id)))
    (unless participant
      (signal 'e-board-registry-participant-missing (list id)))
    (when (and (e-board-registry-participant-p participant-or-id)
               (not (eq participant participant-or-id)))
      (signal 'e-board-registry-participant-board-local (list id)))
    participant))

(cl-defun e-board-registry-create (&key id id-function author principal)
  "Create and register an active board with stored AUTHOR and PRINCIPAL.
ID-FUNCTION receives an identity kind and supplies all registry-owned ids.
The source board is registered with `e-board' under the same board identity."
  (let* ((id (or id (e-board-registry--next-id id-function 'board))))
    (when (gethash id e-board-registry--boards)
      (signal 'e-board-registry-id-conflict (list id)))
    (let ((board (e-board-registry-board--create
                  :id id
                  :source-board (e-board-create :id id)
                  :state 'active
                  :author author
                  :principal principal
                  :id-function id-function
                  :clients (make-hash-table :test 'equal)
                  :participants (make-hash-table :test 'equal))))
      (puthash id board e-board-registry--boards)
      board)))

(defun e-board-registry-get (id)
  "Return process-local board ID, or signal `e-board-registry-missing'."
  (e-board-registry--resolve id))

(defun e-board-registry-list ()
  "Return all process-local boards sorted by printable identity."
  (let (boards)
    (maphash (lambda (_id board) (push board boards)) e-board-registry--boards)
    (sort boards (lambda (left right)
                   (string< (format "%s" (e-board-registry-board-id left))
                            (format "%s" (e-board-registry-board-id right)))))))

(defun e-board-registry-participant (board-or-id participant-or-id)
  "Return BOARD-OR-ID's canonical board-local participant record."
  (e-board-registry--participant (e-board-registry--resolve board-or-id)
                                 participant-or-id))

(cl-defun e-board-registry-attach-client
    (board-or-id &key id author principal)
  "Attach a client to active BOARD-OR-ID and return its local record."
  (let* ((board (e-board-registry--require-active board-or-id))
         (id (or id (e-board-registry--next-id
                     (e-board-registry-board-id-function board) 'client))))
    (when (gethash id (e-board-registry-board-clients board))
      (signal 'e-board-registry-id-conflict (list id)))
    (let ((client (e-board-registry-client--create
                   :id id :board-id (e-board-registry-board-id board)
                   :author author :principal principal)))
      (puthash id client (e-board-registry-board-clients board))
      client)))

(defun e-board-registry-detach-client (board-or-id client-id)
  "Detach CLIENT-ID from active BOARD-OR-ID and release its observers."
  (let* ((board (e-board-registry--require-active board-or-id))
         (clients (e-board-registry-board-clients board))
         (client (gethash client-id clients)))
    (when client
      (maphash
       (lambda (_observer-id observer)
         (when (and (equal (e-board-observer-client-id observer) client-id)
                    (memq (e-board-observer-state observer) '(active muted)))
           (e-board-set-observer-state
            (e-board-registry-board-source-board board)
            (e-board-observer-id observer) 'cancelled)))
       (e-board-observers (e-board-registry-board-source-board board)))
      (remhash client-id clients))
    client))

(cl-defun e-board-registry-install-observer
    (board-or-id client-id selector &key id (start-seq 0)
                 history-before-seq (history-floor 0))
  "Install an effect-free board observer owned by attached CLIENT-ID.
The registry validates board-local client ownership; the source board retains
the cursor and selector because it owns ordered message observation."
  (let* ((board (e-board-registry--require-active board-or-id))
         (client (gethash client-id (e-board-registry-board-clients board))))
    (unless client
      (signal 'e-board-registry-client-missing (list client-id)))
    (e-board-observer-subscribe
     (e-board-registry-board-source-board board) client-id selector
     :id (or id (e-board-registry--next-id
                 (e-board-registry-board-id-function board) 'observer))
     :start-seq start-seq
     :history-before-seq history-before-seq
     :history-floor history-floor)))

(defun e-board-registry--observer-for-client (board client-id observer-id)
  "Return BOARD OBSERVER-ID after validating its attached owning CLIENT-ID."
  (unless (gethash client-id (e-board-registry-board-clients board))
    (signal 'e-board-registry-client-missing (list client-id)))
  (let ((observer (or (e-board-observer
                       (e-board-registry-board-source-board board) observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (unless (equal (e-board-observer-client-id observer) client-id)
      (signal 'e-board-registry-error
              (list "Observer belongs to another client" observer-id client-id)))
    observer))

(cl-defun e-board-registry-prepare-observer-page
    (board-or-id client-id observer-id &key (limit 32))
  "Prepare OBSERVER-ID's page for attached CLIENT-ID without cursor advance."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-observer-prepare-page
     (e-board-registry-board-source-board board) observer-id :limit limit)))

(defun e-board-registry-accept-observer-page
    (board-or-id client-id observer-id through-seq)
  "Record attached CLIENT-ID's accepted observer page receipt THROUGH-SEQ."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-observer-accept-page
     (e-board-registry-board-source-board board) observer-id through-seq)))

(cl-defun e-board-registry-prepare-observer-history-page
    (board-or-id client-id observer-id &key (limit 32))
  "Prepare an attached CLIENT-ID's reverse observer page without advance."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-observer-prepare-history-page
     (e-board-registry-board-source-board board) observer-id :limit limit)))

(defun e-board-registry-accept-observer-history-page
    (board-or-id client-id observer-id before-seq)
  "Record attached CLIENT-ID's accepted history receipt BEFORE-SEQ."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-registry--observer-for-client board client-id observer-id)
    (e-board-observer-accept-history-page
     (e-board-registry-board-source-board board) observer-id before-seq)))

(cl-defun e-board-registry-add-participant
    (board-or-id &key id author principal (state 'active))
  "Add a board-local participant to active BOARD-OR-ID.
The participant's built-in exact address subscription is created by the source
board, with its identity supplied by this registry's id generator."
  (let* ((board (e-board-registry--require-active board-or-id))
         (id-function (e-board-registry-board-id-function board))
         (id (or id (e-board-registry--next-id id-function 'participant)))
         (participants (e-board-registry-board-participants board)))
    (when (gethash id participants)
      (signal 'e-board-registry-id-conflict (list id)))
    (let* ((source-participant
            (e-board-add-participant
             (e-board-registry-board-source-board board)
             :id id
             :state state
             :create-pickup-subscription-id
             (e-board-registry--next-id id-function 'subscription)))
           (participant
            (e-board-registry-participant--create
             :id id :board-id (e-board-registry-board-id board)
             :author author :principal principal
             :source-participant source-participant)))
      (puthash id participant participants)
      participant)))

(defun e-board-registry-remove-participant (board-or-id participant-or-id)
  "Remove PARTICIPANT-OR-ID from active BOARD-OR-ID and disable its routes."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (participant-id (e-board-registry-participant-id participant))
         (source-board (e-board-registry-board-source-board board)))
    (setf (e-board-participant-state
           (e-board-registry-participant-source-participant participant))
          'removed)
    (dolist (subscription (e-board-subscriptions source-board))
      (when (equal (e-board-subscription-participant-id subscription) participant-id)
        (setf (e-board-subscription-state subscription) 'inactive)))
    (remhash participant-id (e-board-registry-board-participants board))
    participant))

(defun e-board-registry-set-participant-state
    (board-or-id participant-or-id state)
  "Transition a local participant to active, dormant, or stale STATE.
These nonterminal membership states retain the participant's exact address
subscription so queued/recoverable delivery can be reconciled by the runtime.
Removal remains the terminal operation in `e-board-registry-remove-participant'."
  (unless (memq state '(active dormant stale))
    (signal 'wrong-type-argument
            (list '(member active dormant stale) state)))
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (source-board (e-board-registry-board-source-board board))
         (source-participant
          (e-board-registry-participant-source-participant participant))
         (from (e-board-participant-state source-participant)))
    (when (eq from state)
      (signal 'e-board-registry-error
              (list "Participant already in requested state" state)))
    (setf (e-board-participant-state source-participant) state)
    (e-board--append-event
     source-board
     (pcase state
       ('active 'participant-rebound)
       ('dormant 'participant-dormant)
       ('stale 'participant-stale))
     (list :participant-id (e-board-registry-participant-id participant)
           :from from :state state))
    participant))

(cl-defun e-board-registry-install-subscription
    (board-or-id participant-or-id selector &key id (state 'active)
                (effect 'create-pickup))
  "Install an ordinary source-board subscription for a local participant."
  (let* ((board (e-board-registry--require-active board-or-id))
         (participant (e-board-registry--participant board participant-or-id))
         (id (or id (e-board-registry--next-id
                     (e-board-registry-board-id-function board) 'subscription))))
    (e-board-subscribe (e-board-registry-board-source-board board)
                      (e-board-registry-participant-id participant)
                       selector :id id :state state :effect effect)))

(defun e-board-registry-mute-subscription (board-or-id subscription-id)
  "Mute ordinary SUBSCRIPTION-ID on active BOARD-OR-ID."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-set-subscription-state
     (e-board-registry-board-source-board board) subscription-id 'muted)))

(defun e-board-registry-resume-subscription (board-or-id subscription-id)
  "Resume ordinary SUBSCRIPTION-ID on active BOARD-OR-ID for future inputs."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-set-subscription-state
     (e-board-registry-board-source-board board) subscription-id 'active)))

(defun e-board-registry-cancel-subscription (board-or-id subscription-id)
  "Cancel ordinary SUBSCRIPTION-ID on active BOARD-OR-ID permanently."
  (let ((board (e-board-registry--require-active board-or-id)))
    (e-board-set-subscription-state
     (e-board-registry-board-source-board board) subscription-id 'cancelled)))

(defun e-board-registry-close (board-or-id)
  "Close BOARD-OR-ID, disable its routes, and unregister its source board."
  (let ((board (e-board-registry--require-active board-or-id)))
    (maphash
     (lambda (_id participant)
       (setf (e-board-participant-state
              (e-board-registry-participant-source-participant participant))
             'closed))
     (e-board-registry-board-participants board))
    (dolist (subscription
             (e-board-subscriptions (e-board-registry-board-source-board board)))
      (setf (e-board-subscription-state subscription) 'inactive))
    (setf (e-board-registry-board-state board) 'closed)
    (e-board-unregister (e-board-registry-board-source-board board))
    board))

(provide 'e-board-registry)

;;; e-board-registry.el ends here

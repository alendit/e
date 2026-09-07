;;; e-board-storage.el --- Consumer-shaped durable Board storage port -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module names the durable operations consumed by Board policy.  It owns
;; no SQL, routing policy, endpoint, subscription, or session behavior.  A
;; concrete adapter supplies the operations as one opaque value.

;;; Code:

(require 'cl-lib)
(require 'e-runtime-store-codec)

(define-error 'e-board-storage-error "Board storage error")
(define-error 'e-board-storage-conflict "Board storage conflict"
  'e-board-storage-error)
(define-error 'e-board-storage-unavailable "Board storage unavailable"
  'e-board-storage-error)

(cl-defstruct (e-board-storage
               (:constructor e-board-storage--create)
               (:conc-name e-board-storage--))
  runtime call-operation asynchronous pending-count first-error
  next-revision next-position next-generation settlement-function)

(defun e-board-storage-signature-hash (value)
  "Return the stable hash used for one canonical Board source VALUE."
  (secure-hash 'sha256 (e-runtime-store-codec-encode value)))

(defun e-board-storage-runtime (storage)
  "Return STORAGE's shared runtime-store identity."
  (e-board-storage--runtime storage))

(defun e-board-storage-asynchronous-p (storage)
  "Return non-nil when STORAGE admits writes without waiting for ACK."
  (and (e-board-storage-p storage)
       (e-board-storage--asynchronous storage)))

(defun e-board-storage-set-settlement-function (storage function)
  "Set STORAGE's bounded asynchronous settlement observer to FUNCTION."
  (unless (or (null function) (functionp function))
    (signal 'wrong-type-argument (list 'functionp function)))
  (setf (e-board-storage--settlement-function storage) function)
  storage)

(defun e-board-storage--call (storage operation &rest arguments)
  "Invoke STORAGE OPERATION with ARGUMENTS."
  (unless (e-board-storage-p storage)
    (signal 'wrong-type-argument (list 'e-board-storage-p storage)))
  (condition-case err
      (apply (e-board-storage--call-operation storage) operation arguments)
    (e-runtime-store-board-conflict
     (signal 'e-board-storage-conflict (cdr err)))))

(defun e-board-storage-create-board
    (storage board-id trusted-principal &optional root)
  "Create durable BOARD-ID owned by TRUSTED-PRINCIPAL."
  (e-board-storage--call storage 'create-board board-id trusted-principal root))

(defun e-board-storage-board (storage board-id)
  "Return durable root projection for BOARD-ID, or nil."
  (e-board-storage--call storage 'board board-id))

(defun e-board-storage-list-boards (storage &optional after limit)
  "Return a bounded durable Board-root page after AFTER."
  (e-board-storage--call storage 'list-boards after limit))

(defun e-board-storage-clear-board (storage board-id)
  "Advance BOARD-ID generation."
  (e-board-storage--call storage 'clear-board board-id))

(defun e-board-storage-publish-record
    (storage board-id generation record source)
  "Publish canonical RECORD and optional SOURCE identity to BOARD-ID."
  (e-board-storage--call storage 'publish-record board-id generation
                         record source))

(defun e-board-storage-record-page
    (storage board-id generation &optional after limit selector)
  "Return a bounded canonical record page for BOARD-ID GENERATION."
  (e-board-storage--call storage 'record-page board-id generation after limit
                         selector))

(defun e-board-storage-commit-routing
    (storage board-id generation message-id outcome pickups)
  "Commit MESSAGE-ID's final OUTCOME and immutable PICKUPS."
  (e-board-storage--call storage 'commit-routing board-id generation
                         message-id outcome pickups))

(defun e-board-storage-routing (storage board-id generation message-id)
  "Return MESSAGE-ID's final routing projection, or nil."
  (e-board-storage--call storage 'routing board-id generation message-id))

(defun e-board-storage-transition-pickup
    (storage board-id generation delivery-id transition data)
  "Commit DELIVERY-ID TRANSITION with bounded DATA."
  (e-board-storage--call storage 'transition-pickup board-id generation
                         delivery-id transition data))

(defun e-board-storage-unresolved-pickups
    (storage board-id generation &optional participant-id limit)
  "Return unresolved pickups in participant FIFO order."
  (e-board-storage--call storage 'unresolved-pickups board-id generation
                         participant-id limit))

(defun e-board-storage-put-participant
    (storage board-id generation participant)
  "Persist logical PARTICIPANT identity for BOARD-ID."
  (e-board-storage--call storage 'put-participant board-id generation
                         participant))

(defun e-board-storage-delete-participant
    (storage board-id generation participant-id)
  "Delete unpublished PARTICIPANT-ID."
  (e-board-storage--call storage 'delete-participant board-id generation
                         participant-id))

(defun e-board-storage-publish-participant
    (storage board-id generation participant-id)
  "Publish provisional PARTICIPANT-ID."
  (e-board-storage--call storage 'publish-participant board-id generation
                         participant-id))

(defun e-board-storage-participants (storage board-id generation &optional limit)
  "Return bounded logical participant projections for BOARD-ID."
  (e-board-storage--call storage 'participants board-id generation limit))

(defun e-board-storage-put-replay-progress
    (storage board-id generation subscription-id position)
  "Persist SUBSCRIPTION-ID replay POSITION for BOARD-ID GENERATION."
  (e-board-storage--call storage 'put-replay-progress board-id generation
                         subscription-id position))

(defun e-board-storage-replay-progress
    (storage board-id generation subscription-id)
  "Return durable replay progress for stable SUBSCRIPTION-ID."
  (e-board-storage--call storage 'replay-progress board-id generation
                         subscription-id))

(defun e-board-storage-admit-pickup
    (storage board-id generation delivery-id session-id record lane)
  "Atomically accept DELIVERY-ID and append session admission RECORD."
  (e-board-storage--call storage 'admit-pickup board-id generation delivery-id
                         session-id record lane))

(defun e-board-storage-status (storage)
  "Return bounded STORAGE health and runtime status."
  (e-board-storage--call storage 'status))

(provide 'e-board-storage)

;;; e-board-storage.el ends here

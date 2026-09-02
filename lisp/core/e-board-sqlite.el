;;; e-board-sqlite.el --- Durable Board composition and restore -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Composes the process-local Board/registry policy from the consumer-shaped
;; SQLite storage port.  It restores only durable identities and facts; live
;; clients, subscriptions, callbacks, endpoints, tokens, and timers are rebuilt
;; by their normal owners.

;;; Code:

(require 'cl-lib)
(require 'e-board)
(require 'e-board-registry)
(require 'e-board-storage-sqlite)

(cl-defun e-board-sqlite-create-registry-board
    (runtime &key id author principal id-function)
  "Create a durable registry Board on shared RUNTIME."
  (e-board-registry-create
   :id id :author author :principal principal :id-function id-function
   :storage (e-board-storage-sqlite-create runtime)))

(defun e-board-sqlite--restore-records (board storage generation)
  "Restore BOARD records from STORAGE GENERATION in bounded pages."
  (let ((after 0) page)
    (while
        (progn
          (setq page (e-board-storage-record-page
                      storage (e-board-id board) generation after 256 nil))
          (dolist (item (plist-get page :records))
            (let ((record (plist-get item :record)))
              (if (plist-get record :record-type)
                  (e-board-import-processing-record board record)
                (let ((message
                       (e-board-import-message
                        board record (plist-get item :position))))
                  (e-board-restore-source
                   board message (plist-get item :source))))))
          (setq after (plist-get page :next))))))

(defun e-board-sqlite--reconcile-pickups (board storage generation)
  "Return restorable unresolved pickups, tombstoning ambiguous effects."
  (let ((initial (e-board-storage-unresolved-pickups
                  storage (e-board-id board) generation nil 4096))
        uncertain)
    (dolist (pickup initial)
      (when (memq (plist-get pickup :state) '(claimed accepted cancelling))
        (let ((result
               (e-board-storage-transition-pickup
                storage (e-board-id board) generation
                (plist-get pickup :delivery-id) 'uncertain
                (list :reason 'restart-effect-ambiguous))))
          (setf (e-board-revision board) (plist-get result :revision))
          ;; Uncertainty is terminal but remains visible.  Re-read the active
          ;; set below because this transition may promote a FIFO successor.
          (push (plist-get result :pickup) uncertain))))
    (append
     (nreverse uncertain)
     (e-board-storage-unresolved-pickups
      storage (e-board-id board) generation nil 4096))))

(cl-defun e-board-sqlite-restore-registry-board
    (runtime board-id &key author id-function)
  "Restore durable BOARD-ID and its unresolved pickups from shared RUNTIME."
  (let* ((storage (e-board-storage-sqlite-create runtime))
         (root (or (e-board-storage-board storage board-id)
                   (signal 'e-board-storage-error
                           (list "Missing durable Board" board-id))))
         (generation (plist-get root :generation))
         (registry-board
          (e-board-registry-create
           :id board-id :author author
           :principal (plist-get root :trusted-principal)
           :id-function id-function :storage storage :restoring t
           :generation generation :revision (plist-get root :revision)))
         (board (e-board-registry-board-source-board registry-board)))
    (let ((e-board--storage-replay-p t))
      (dolist (participant
               (e-board-storage-participants storage board-id generation 4096))
        (if (plist-get participant :publication-pending)
            ;; A process loss before the enclosing session admission committed
            ;; leaves only this owner-specific provisional identity.  Remove
            ;; it so exact retry can recreate the participant from session
            ;; association intent; durable pickups would reject this cleanup.
            (let ((result
                   (e-board-storage-delete-participant
                    storage board-id generation
                    (plist-get participant :id))))
              (setf (e-board-revision board) (plist-get result :revision)))
          (let ((restored
                 (e-board-registry-add-participant
                  registry-board :id (plist-get participant :id)
                  :author (plist-get participant :author)
                  :principal (plist-get participant :principal)
                  :controller (plist-get participant :controller)
                  :subscription-id (plist-get participant :subscription-id)
                  :state 'dormant :publish-event nil)))
            ;; No process-local participant-added event is replayed, but the
            ;; durable identity was already published before this restart.
            (setf (e-board-registry-participant-publication-pending restored)
                  nil))))
      (e-board-sqlite--restore-records board storage generation))
    (let ((pickups
           (e-board-sqlite--reconcile-pickups board storage generation)))
      (dolist (message (e-board-messages board))
        (when (eq (e-board-message-kind message) 'input)
          (when-let* ((routing
                       (e-board-storage-routing
                        storage board-id generation
                        (e-board-message-id message))))
            (e-board-restore-routing
             board (e-board-message-id message) (plist-get routing :outcome)
             (cl-remove-if-not
              (lambda (pickup)
                (equal (plist-get pickup :message-id)
                       (e-board-message-id message)))
              pickups))))))
    registry-board))

(provide 'e-board-sqlite)

;;; e-board-sqlite.el ends here

;;; e-board-pickup-admission.el --- Atomic Board/session pickup admission -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Application service for the sole cross-owner P2 transaction.  It joins one
;; already claimed Board pickup with one staged immutable session input record.
;; It owns no turn execution, callback, provider request, or generic transaction.

;;; Code:

(require 'cl-lib)
(require 'e-board)
(require 'e-board-storage)
(require 'e-session-board-input-admission)
(require 'e-session-storage)

(define-error 'e-board-pickup-admission-error
  "Board pickup admission error" 'e-board-error)

(cl-defun e-board-pickup-admission-commit
    (board delivery-id session-store session-id lane &key metadata)
  "Atomically accept claimed DELIVERY-ID and admit it to SESSION-ID on LANE."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-pickup-admission-error
                            (list "Unknown pickup" delivery-id)))))
    (unless (and (e-board-storage-backed-p board)
                 (eq (e-board-pickup-state pickup) 'delivering))
      (signal 'e-board-pickup-admission-error
              (list "Pickup is not durably claimed" delivery-id)))
    (unless (eq (e-board-storage-runtime (e-board-storage board))
                (e-session-storage-runtime-store session-store))
      (signal 'e-board-pickup-admission-error
              (list "Board and session do not share one runtime store")))
    (let* ((admission
            (e-session-board-input-admission-prepare
             session-store session-id delivery-id lane
             (e-board-pickup-content pickup) :metadata metadata))
           (result
            (e-board-storage-admit-pickup
             (e-board-storage board) (e-board-id board)
             (e-board-generation board) delivery-id session-id
             (e-session-board-input-admission-record admission) lane)))
      ;; Both semantic owners publish only after the one worker ACK.
      (e-session-board-input-admission-publish admission)
      (let ((e-board--storage-replay-p t))
        (e-board-pickup-accept-delivery board delivery-id
                                        (list :session-id session-id
                                              :lane lane)))
      (setf (e-board-revision board) (plist-get result :board-revision)
            (e-board-pickup-revision pickup)
            (plist-get result :pickup-revision))
      (list :delivery-id (copy-tree delivery-id) :session-id session-id
            :lane lane :entry
            (e-session-board-input-admission-entry admission)))))

(provide 'e-board-pickup-admission)

;;; e-board-pickup-admission.el ends here

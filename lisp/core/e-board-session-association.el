;;; e-board-session-association.el --- Board/session durable association -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the application composition between a durable session association and
;; a Board root.  Chat presentation consumes the result but does not persist or
;; restore Board records.  Durable Boards always use their SQLite owner; an
;; explicitly ephemeral session may still compose a process-local Board.

;;; Code:

(require 'e-board-registry)
(require 'e-board-orchestration)
(require 'e-board-sqlite)
(require 'e-session)
(require 'e-session-storage)

(defun e-board-session-association-create-board (store principal)
  "Create a registry Board for PRINCIPAL using STORE's selected backend."
  (cond
   ((e-session-storage-sqlite-p store)
    (e-board-sqlite-create-registry-board
     (e-session-storage-runtime-store store) :principal principal))
   ((not (e-session-storage-persistent-p store))
    (e-board-registry-create :principal principal))
   (t
    (signal 'e-session-storage-error
            (list "Persistent Boards require migrated SQLite state")))))

(defun e-board-session-association-persist
    (store session-id principal board-id role &optional routing-policy)
  "Persist SESSION-ID's Board identity and ROUTING-POLICY."
  (e-session-declare-board-state
   store session-id principal board-id role routing-policy))

(defun e-board-session-association-reset-projection (_store _session-id)
  "Retain durable Board audit while the caller clears presentation state."
  nil)

(defun e-board-session-association-configure-notifications
    (store session-id board observer)
  "Configure BOARD's presentation notification composition.
OBSERVER receives BOARD and one already committed message.  Durable Board
records and processing facts are owned by the Board SQLite adapter before this
callback runs; this service never proxies them through session storage."
  (ignore store session-id)
  (setf (e-board-message-notification-function board)
        (lambda (source message) (funcall observer source message))
        (e-board-processing-record-notification-function board) nil))

(defun e-board-session-association-release (board)
  "Release BOARD's process-local presentation notification hooks."
  (setf (e-board-message-notification-function board) nil
        (e-board-processing-record-notification-function board) nil)
  t)

(defun e-board-session-association-restore (store session)
  "Restore SESSION's associated Board through its owning physical backend."
  (let* ((session-id (plist-get session :id))
         (state (plist-get session :board-session-state))
         (board-id (plist-get state :board-id))
         (principal (plist-get state :principal)))
    (or (condition-case nil
            (e-board-registry-get board-id)
          (e-board-registry-missing nil))
        (cond
         ((e-session-storage-sqlite-p store)
            ;; SQLite owns Board facts independently.  A missing root is a
            ;; visible ownership/corruption error, never a session-journal
            ;; compatibility fallback.
            (e-board-sqlite-restore-registry-board
             (e-session-storage-runtime-store store) board-id))
         ((not (e-session-storage-persistent-p store))
          (let ((board (e-board-registry-create :id board-id
                                                :principal principal)))
            (e-board-orchestration-mark-restoring
             (e-board-registry-board-source-board board))
            (unwind-protect
                (dolist (envelope (e-session-board-messages store session-id))
                  (if (plist-get envelope :record-type)
                      (e-board-import-processing-record
                       (e-board-registry-board-source-board board) envelope)
                    (e-board-import-message
                     (e-board-registry-board-source-board board) envelope)))
              (e-board-orchestration-mark-restored
               (e-board-registry-board-source-board board)))
            board))
         (t
          (signal 'e-session-storage-error
                  (list "Persistent Boards require migrated SQLite state"
                        board-id)))))))

(provide 'e-board-session-association)

;;; e-board-session-association.el ends here

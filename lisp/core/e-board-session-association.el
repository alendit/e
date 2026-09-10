;;; e-board-session-association.el --- Board/session durable association -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the application composition between a durable session association and
;; a lightweight live Board controller.  Chat presentation consumes the result
;; but never reconstructs Board history.  Durable Boards use a bounded detached
;; SQLite controller query; explicitly ephemeral sessions remain process-local.

;;; Code:

(require 'e-board-registry)
(require 'e-board-orchestration-engine)
(require 'e-board-sqlite)
(require 'e-session)
(require 'e-session-storage)
(require 'subr-x)

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

(defun e-board-session-association-compose-ephemeral (store session)
  "Compose SESSION's explicitly in-memory Board runtime from STORE.

This compatibility path is process-local by construction and is never used by
ordinary SQLite v6 sessions."
  (when (e-session-storage-persistent-p store)
    (signal 'e-session-storage-error
            (list "Durable Boards require bounded SQLite controller queries")))
  (let* ((session-id (plist-get session :id))
         (state (plist-get session :board-session-state))
         (board-id (plist-get state :board-id))
         (principal (plist-get state :principal))
         (board (e-board-registry-create :id board-id :principal principal)))
    (e-board-orchestration-mark-restoring
     (e-board-registry-board-source-board board))
    (unwind-protect
        (dolist (envelope (e-session-local-board-messages store session-id))
          (if (plist-get envelope :record-type)
              (e-board-import-processing-record
               (e-board-registry-board-source-board board) envelope)
            (e-board-import-message
             (e-board-registry-board-source-board board) envelope)))
      (e-board-orchestration-mark-restored
       (e-board-registry-board-source-board board)))
    board))

(provide 'e-board-session-association)

;;; e-board-session-association.el ends here

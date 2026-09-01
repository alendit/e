;;; e-board-session-association.el --- Board/session durable association -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the application composition between a durable session association and
;; a Board root.  Chat presentation consumes the result but does not persist or
;; restore Board records.  Legacy session Board journals remain a compatibility
;; input until the P4 cutover; new SQLite Boards use the Board store directly.

;;; Code:

(require 'e-board-registry)
(require 'e-board-orchestration)
(require 'e-board-sqlite)
(require 'e-session)
(require 'e-session-storage)

(defvar e-board-session-association--legacy-owners
  (make-hash-table :test 'equal)
  "Legacy Board log owners keyed by Board id.
Each value names the exact live Board object, session store, and root session.")

(defun e-board-session-association-create-board (store principal)
  "Create a registry Board for PRINCIPAL using STORE's selected backend."
  (if (e-session-storage-sqlite-p store)
      (e-board-sqlite-create-registry-board
       (e-session-storage-runtime-store store) :principal principal)
    (e-board-registry-create :principal principal)))

(defun e-board-session-association-persist
    (store session-id principal board-id role &optional routing-policy)
  "Persist SESSION-ID's Board identity and ROUTING-POLICY."
  (e-session-declare-board-state
   store session-id principal board-id role routing-policy))

(defun e-board-session-association-reset-legacy-projection (store session-id)
  "Clear SESSION-ID's legacy Board journal when that backend still owns it.
SQLite Boards keep their immutable audit and clear only presentation state."
  (unless (e-session-storage-sqlite-p store)
    (e-session-clear-board-messages store session-id)))

(defun e-board-session-association-configure-notifications
    (store session-id board observer)
  "Configure BOARD's persistence and presentation notification composition.
OBSERVER receives BOARD and one committed message.  The legacy session journal
keeps one stable root owner; SQLite records are already authoritative before
this callback runs."
  (let* ((board-id (e-board-id board))
         (owner (gethash board-id
                         e-board-session-association--legacy-owners)))
    (when (and (not (e-session-storage-sqlite-p store))
               (not (eq (car-safe owner) board)))
      (setq owner (list board store session-id))
      (puthash board-id owner e-board-session-association--legacy-owners))
    (setf
     (e-board-message-notification-function board)
     (lambda (source message)
       (when-let* ((legacy
                    (gethash (e-board-id source)
                             e-board-session-association--legacy-owners))
                   ((eq (car legacy) source)))
         (e-session-append-board-message
          (nth 1 legacy) (nth 2 legacy) (e-board-message-envelope message)))
       (funcall observer source message))
     (e-board-processing-record-notification-function board)
     (and (not (e-session-storage-sqlite-p store))
          (lambda (source record _type)
            (when-let* ((legacy
                         (gethash
                          (e-board-id source)
                          e-board-session-association--legacy-owners))
                        ((eq (car legacy) source)))
              (e-session-append-board-message
               (nth 1 legacy) (nth 2 legacy)
               (e-board-processing-record-envelope record))))))))

(defun e-board-session-association-release (board)
  "Release BOARD's exact process-local legacy association, if current."
  (let* ((board-id (e-board-id board))
         (owner (gethash board-id
                         e-board-session-association--legacy-owners)))
    (when (eq (car-safe owner) board)
      (remhash board-id e-board-session-association--legacy-owners)
      (setf (e-board-message-notification-function board) nil
            (e-board-processing-record-notification-function board) nil)
      t)))

(defun e-board-session-association-restore (store session)
  "Restore SESSION's associated Board through its owning physical backend."
  (let* ((session-id (plist-get session :id))
         (state (plist-get session :board-session-state))
         (board-id (plist-get state :board-id))
         (principal (plist-get state :principal)))
    (or (condition-case nil
            (e-board-registry-get board-id)
          (e-board-registry-missing nil))
        (if (e-session-storage-sqlite-p store)
            ;; SQLite owns Board facts independently.  A missing root is a
            ;; visible ownership/corruption error, never a session-journal
            ;; compatibility fallback.
            (e-board-sqlite-restore-registry-board
             (e-session-storage-runtime-store store) board-id)
          (let ((board (e-board-registry-create
                        :id board-id :principal principal)))
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
            board)))))

(provide 'e-board-session-association)

;;; e-board-session-association.el ends here

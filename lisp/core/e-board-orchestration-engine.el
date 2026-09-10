;;; e-board-orchestration-engine.el --- Transitional Board engine adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Isolate the in-memory Board adapters still used by producers awaiting DP7B.
;; Public chat depends only on the detached fact contract in
;; `e-board-orchestration'.  DP7B migrates these callers to SQL, after which
;; DP7C removes this file with the alternate engine.

;;; Code:

(require 'cl-lib)
(require 'e-board)
(require 'e-board-orchestration)

(defvar e-board-orchestration-engine--restoration-states
  (make-hash-table :test 'equal)
  "Board replay state retained only by the transitional engine adapter.")

(defun e-board-orchestration-mark-restoring (board)
  "Mark transitional engine BOARD as replaying durable facts."
  (puthash (e-board-id board) 'restoring
           e-board-orchestration-engine--restoration-states))

(defun e-board-orchestration-mark-restored (board)
  "Mark transitional engine BOARD's durable fact replay complete."
  (puthash (e-board-id board) 'restored
           e-board-orchestration-engine--restoration-states))

(defun e-board-orchestration-restoration-state (board)
  "Return transitional engine BOARD's replay state, or `ready'."
  (or (gethash (e-board-id board)
               e-board-orchestration-engine--restoration-states)
      'ready))

(cl-defun e-board-orchestration-publish-fact (board fact &key author)
  "Validate and idempotently publish durable orchestration FACT to BOARD."
  (let ((fields (e-board-orchestration-fact-record-fields fact :author author)))
    (e-board-post-fact
     board :author (plist-get fields :author)
     :tags (plist-get fields :tags)
     :attributes (plist-get fields :attributes)
     :content (plist-get fields :content)
     :source-fact-key (plist-get fields :source-key))))

(defun e-board-orchestration-fact-from-message (message)
  "Return normalized orchestration fact from engine MESSAGE, or nil."
  (when (and (eq (e-board-message-kind message) 'fact)
             (memq 'orchestration (e-board-message-tags message)))
    (e-board-orchestration-fact-from-record
     (list :kind (e-board-message-kind message)
           :tags (e-board-message-tags message)
           :attributes (e-board-message-attributes message)))))

(defun e-board-orchestration-engine--run-facts (board run-id)
  "Return transitional engine BOARD facts that belong to RUN-ID."
  (delq nil
        (mapcar
         (lambda (message)
           (when-let* ((fact
                        (e-board-orchestration-fact-from-message message))
                       ((equal run-id
                               (plist-get (plist-get fact :payload) :run-id))))
             fact))
         (e-board-messages board))))

(defun e-board-orchestration-run-projection (board run-id &optional now)
  "Return transitional engine BOARD's bounded projection for RUN-ID."
  (if (eq (e-board-orchestration-restoration-state board) 'restoring)
      (list :run-id run-id :state 'not-restored-yet)
    (let ((facts (e-board-orchestration-engine--run-facts board run-id)))
      (if facts
          (e-board-orchestration-reduce facts now)
        (list :run-id run-id :state 'missing)))))

(defun e-board-orchestration-run-ids (board)
  "Return manifest run ids visible on transitional engine BOARD."
  (unless (eq (e-board-orchestration-restoration-state board) 'restoring)
    (delete-dups
     (delq nil
           (mapcar
            (lambda (message)
              (when-let* ((fact
                           (e-board-orchestration-fact-from-message message))
                          ((eq (plist-get fact :type) 'manifest)))
                (plist-get (plist-get fact :payload) :run-id)))
            (e-board-messages board))))))

(defun e-board-orchestration-project-board (board &optional now)
  "Reduce transitional engine BOARD's orchestration facts."
  (e-board-orchestration-reduce
   (delq nil
         (mapcar #'e-board-orchestration-fact-from-message
                 (e-board-messages board)))
   now))

(provide 'e-board-orchestration-engine)

;;; e-board-orchestration-engine.el ends here

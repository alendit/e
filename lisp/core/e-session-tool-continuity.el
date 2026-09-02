;;; e-session-tool-continuity.el --- Durable tool restart classification -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the SQLite-only durable lifecycle which classifies interrupted tool
;; work.  Session journaling calls the two narrow composition helpers at its
;; execution fences; this owner knows no aggregate, provider, callback, handle,
;; timer, or live request representation.

;;; Code:

(require 'cl-lib)
(require 'e-session-aggregate)
(require 'e-session-storage)

(cl-defun e-session-tool-followup-transition
    (store session-id call-id state &optional payload)
  "Commit typed durable tool CALL-ID STATE for SESSION-ID."
  (unless (memq state '(admitted claimed started resulted follow-up-ready
                                 promoted settled attention-required cancelled))
    (signal 'e-session-error (list "Invalid tool follow-up state" state)))
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-session-storage-error
            (list "Tool follow-up storage requires SQLite")))
  (e-session-storage-sqlite-tool-transition
   store session-id call-id state (copy-tree payload)))

(defun e-session-tool-followup-classifications (store session-id)
  "Return bounded durable tool restart classifications for SESSION-ID."
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-session-storage-error
            (list "Tool follow-up storage requires SQLite")))
  (e-session-storage-sqlite-tool-classifications store session-id))

(defun e-session-tool-continuity-admit-message
    (store session-id message entry)
  "Persist MESSAGE's admission fence before ENTRY can become visible."
  (when (and (e-session-storage-sqlite-p store)
             (eq (plist-get message :role) 'tool-call))
    (let ((call (plist-get message :content)))
      (when-let* ((call-id (plist-get call :id)))
        (e-session-tool-followup-transition
         store session-id call-id 'admitted
         (list :tool-name (plist-get call :name)
               :entry-id (plist-get entry :id)))))))

(defun e-session-tool-continuity-record-activity
    (store session-id turn-id event-type payload entry)
  "Persist the tool classification fence represented by activity ENTRY."
  (when (and (e-session-storage-sqlite-p store)
             (memq event-type '(tool-started tool-finished)))
    (let* ((tool-call (plist-get payload :tool-call))
           (call-id (or (plist-get tool-call :id)
                        (plist-get payload :tool-call-id))))
      (when call-id
        (e-session-tool-followup-transition
         store session-id call-id
         (if (eq event-type 'tool-started) 'claimed 'resulted)
         (list :turn-id turn-id :event-id (plist-get entry :id)
               :tool-name (plist-get tool-call :name)))))))

(provide 'e-session-tool-continuity)

;;; e-session-tool-continuity.el ends here

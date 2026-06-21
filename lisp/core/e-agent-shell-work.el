;;; e-agent-shell-work.el --- Agent Shell work registry for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; In-memory normalized work registry for Agent Shell Fleet capability actions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (e-agent-shell-work-registry
               (:constructor e-agent-shell-work-registry-create))
  (records (make-hash-table :test 'equal))
  (order nil)
  (sequence 0))

(defun e-agent-shell-work--next-id (registry)
  "Return the next stable work id from REGISTRY."
  (setf (e-agent-shell-work-registry-sequence registry)
        (1+ (e-agent-shell-work-registry-sequence registry)))
  (format "asw_%06d" (e-agent-shell-work-registry-sequence registry)))

(defun e-agent-shell-work--prompt-summary (prompt)
  "Return compact summary for PROMPT."
  (when (stringp prompt)
    (truncate-string-to-width
     (replace-regexp-in-string "[\n\t ]+" " " prompt)
     80 nil nil t)))

(defun e-agent-shell-work--data-get (data key)
  "Return KEY from plist, alist, or hash table DATA."
  (cond
   ((hash-table-p data) (gethash key data))
   ((and (listp data) (plist-member data key)) (plist-get data key))
   ((listp data)
    (let ((cell (assoc key data)))
      (and cell (cdr cell))))
   (t nil)))

(defun e-agent-shell-work--event-data (event)
  "Return nested event data from EVENT, or nil."
  (e-agent-shell-work--data-get event :data))

(defun e-agent-shell-work--event-field (event key)
  "Return KEY from EVENT or its nested data payload."
  (or (e-agent-shell-work--data-get
       (e-agent-shell-work--event-data event)
       key)
      (e-agent-shell-work--data-get event key)))

(defun e-agent-shell-work--event-type (event)
  "Return normalized type for EVENT."
  (or (e-agent-shell-work--data-get event :type)
      (e-agent-shell-work--data-get event :event)
      event))

(defun e-agent-shell-work--record (registry work-id)
  "Return mutable internal record WORK-ID from REGISTRY."
  (or (gethash work-id (e-agent-shell-work-registry-records registry))
      (user-error "Unknown Agent Shell work id: %s" work-id)))

(defun e-agent-shell-work--normalize (record)
  "Return model-facing normalized RECORD."
  (let* ((buffer (plist-get record :shell-buffer))
         (live (buffer-live-p buffer))
         (status (if live
                     (plist-get record :status)
                   'dead)))
    (unless live
      (plist-put record :status 'dead))
    (list :work-id (plist-get record :work-id)
          :status status
          :agent-id (plist-get record :agent-id)
          :project-root (plist-get record :project-root)
          :shell-buffer (and live (buffer-name buffer))
          :agent-session-id (plist-get record :agent-session-id)
          :transcript-file (plist-get record :transcript-file)
          :origin (plist-get record :origin)
          :prompt-summary (plist-get record :prompt-summary)
          :last-event (plist-get record :last-event)
          :last-event-at (plist-get record :last-event-at)
          :latest-response-preview (plist-get record :latest-response-preview)
          :usage (plist-get record :usage)
          :changed-files (plist-get record :changed-files)
          :error (plist-get record :error))))

(cl-defun e-agent-shell-work-create
    (registry &key shell-buffer agent-id project-root agent-session-id
              transcript-file origin prompt status subscription)
  "Create and store a work record in REGISTRY."
  (unless (buffer-live-p shell-buffer)
    (user-error "Agent Shell work requires a live shell buffer"))
  (let* ((work-id (e-agent-shell-work--next-id registry))
         (record (list :work-id work-id
                       :status (or status 'ready)
                       :agent-id agent-id
                       :project-root project-root
                       :shell-buffer shell-buffer
                       :agent-session-id agent-session-id
                       :transcript-file transcript-file
                       :origin (or origin 'e-created)
                       :prompt-summary (e-agent-shell-work--prompt-summary prompt)
                       :subscription subscription)))
    (puthash work-id record (e-agent-shell-work-registry-records registry))
    (setf (e-agent-shell-work-registry-order registry)
          (append (e-agent-shell-work-registry-order registry)
                  (list work-id)))
    (e-agent-shell-work--normalize record)))

(defun e-agent-shell-work-set-subscription (registry work-id subscription)
  "Set WORK-ID SUBSCRIPTION in REGISTRY."
  (plist-put (e-agent-shell-work--record registry work-id)
             :subscription subscription))

(defun e-agent-shell-work-set-status (registry work-id status)
  "Set WORK-ID STATUS in REGISTRY and return its normalized record."
  (let ((record (e-agent-shell-work--record registry work-id)))
    (plist-put record :status status)
    (e-agent-shell-work--normalize record)))

(defun e-agent-shell-work-get (registry work-id)
  "Return normalized work record WORK-ID from REGISTRY."
  (e-agent-shell-work--normalize
   (e-agent-shell-work--record registry work-id)))

(defun e-agent-shell-work-buffer (registry work-id)
  "Return live shell buffer for WORK-ID."
  (let ((buffer (plist-get (e-agent-shell-work--record registry work-id)
                           :shell-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "Agent Shell work buffer is dead"))
    buffer))

(defun e-agent-shell-work-list (registry)
  "Return normalized work records in creation order."
  (mapcar (lambda (work-id)
            (e-agent-shell-work-get registry work-id))
          (e-agent-shell-work-registry-order registry)))

(defun e-agent-shell-work-ids (registry)
  "Return tracked work ids in creation order."
  (copy-sequence (e-agent-shell-work-registry-order registry)))

(defun e-agent-shell-work--stamp-event (record event type)
  "Record EVENT and TYPE on RECORD."
  (plist-put record :last-event type)
  (plist-put record :last-event-at (format-time-string "%FT%T%z"))
  (plist-put record :last-raw-event event))

(defun e-agent-shell-work-update-from-event (registry work-id event)
  "Apply Agent Shell EVENT to WORK-ID and return the normalized record."
  (let* ((record (e-agent-shell-work--record registry work-id))
         (type (e-agent-shell-work--event-type event)))
    (e-agent-shell-work--stamp-event record event type)
    (pcase type
      ('input-submitted
       (plist-put record :status 'busy))
      ('permission-request
       (plist-put record :status 'blocked)
       (plist-put record :blocked-detail
                  (e-agent-shell-work--event-field event :detail)))
      ('permission-response
       (when (eq (plist-get record :status) 'blocked)
         (plist-put record :status 'busy)))
      ('tool-call-update
       (plist-put record :latest-response-preview
                  (or (e-agent-shell-work--event-field event :preview)
                      (e-agent-shell-work--event-field event :message)
                      (plist-get record :latest-response-preview))))
      ('file-write
       (let ((file (or (e-agent-shell-work--event-field event :file)
                       (e-agent-shell-work--event-field event :path))))
         (when file
           (plist-put record :changed-files
                      (delete-dups
                       (append (plist-get record :changed-files)
                               (list file)))))))
      ('turn-complete
       (plist-put record :status 'finished)
       (plist-put record :latest-response-preview
                  (or (e-agent-shell-work--event-field event :response)
                      (e-agent-shell-work--event-field event :preview)
                      (plist-get record :latest-response-preview)))
       (plist-put record :usage
                  (e-agent-shell-work--event-field event :usage)))
      ('error
       (plist-put record :status 'failed)
       (plist-put record :error (or (e-agent-shell-work--event-field
                                     event :error)
                                    (e-agent-shell-work--event-field
                                     event :message)
                                    "Agent Shell error")))
      ('clean-up
       (unless (memq (plist-get record :status)
                     '(finished interrupted failed))
         (plist-put record :status 'dead))))
    (e-agent-shell-work--normalize record)))

(defun e-agent-shell-work-mark-interrupted (registry work-id)
  "Mark WORK-ID interrupted and return the normalized record."
  (let ((record (e-agent-shell-work--record registry work-id)))
    (plist-put record :status 'interrupted)
    (plist-put record :last-event 'interrupt-work)
    (plist-put record :last-event-at (format-time-string "%FT%T%z"))
    (e-agent-shell-work--normalize record)))

(provide 'e-agent-shell-work)

;;; e-agent-shell-work.el ends here

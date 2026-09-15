;;; e-board-observation.el --- Board-owned participant activity observation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The Board observation seam is the consumer-shaped contract shared by the
;; activity shell and agent actions.  It delegates detached bounded pages to
;; the SQLite application service and durable session query port.  It has no
;; knowledge of subagent runners, process-local handles, or live execution
;; registries.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-chat-service)
(require 'e-board-sqlite-service)
(require 'e-session-async)
(require 'e-session-storage)
(require 'e-work)

(define-error 'e-board-observation-error
  "Invalid Board observation request"
  'e-board-sqlite-error)

(defconst e-board-observation-default-page-limit 64
  "Default participant count returned by the observation action.")

(defconst e-board-observation-raw-page-limit 32
  "Maximum durable transcript messages returned by one raw observation.")

(defun e-board-observation-activity-page-start
    (target &rest arguments)
  "Return work for TARGET's bounded participant/activity page.
ARGUMENTS are keyword arguments accepted by
`e-board-sqlite-publication-target-activity-page-start'.  The returned page is
detached and request-owned; this observation service retains no page after the
consumer work handle settles."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (apply #'e-board-sqlite-publication-target-activity-page-start
         target arguments))

(defun e-board-observation-activity-participant-start
    (target participant-id)
  "Return work for TARGET's exact durable PARTICIPANT-ID projection.
The Board SQL worker applies the identity predicate before selecting any
bounded session or outcome candidates; a missing participant is reported as a
request-local observation error when the returned work settles."
  (unless (and (stringp participant-id) (not (string-empty-p participant-id)))
    (signal 'e-board-observation-error
            (list "Participant identity must be a non-empty string"
                  participant-id)))
  (let ((child
         (e-board-observation-activity-page-start
          target :participant-id participant-id :limit 1)))
    (e-work-start
     (e-work-spec-create
      :id "board-observation-participant"
      :execution 'cooperative :interactive-policy 'async
      :owner 'board-observation
      :runner
      (lambda (parent arguments _context)
        (let ((child (plist-get arguments :child)))
          (setf (e-work-handle-cancel-function parent)
                (lambda (_handle) (e-work-cancel child)))
          (e-work-on-settle
           child
           (lambda (settled)
             (pcase (plist-get (e-work-status settled) :state)
               ('finished
                (let* ((page (e-work-handle-result settled))
                       (row (car (plist-get page :participants))))
                  (if row
                      (e-work-finish parent (copy-tree row t))
                    (e-work-fail
                     parent
                     (list 'e-board-observation-error
                           "Durable participant is not present on Board"
                           participant-id)))))
               ('failed (e-work-fail parent (e-work-handle-error settled)))
               ('cancelled (e-work-cancel parent)))))
          :deferred)))
     (list :child child))))

(defun e-board-observation-session-page-start (store participant-id &optional limit)
  "Return bounded durable transcript work for PARTICIPANT-ID from STORE.
This is an explicit SQL session query and never consults process-local child
state or a live transcript cache."
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-board-observation-error
            (list "Durable raw participant read requires SQLite" store)))
  (unless (and (stringp participant-id) (not (string-empty-p participant-id)))
    (signal 'e-board-observation-error
            (list "Participant identity must be a non-empty string"
                  participant-id)))
  (let ((limit (or limit e-board-observation-raw-page-limit)))
    (unless (and (integerp limit) (> limit 0)
                 (<= limit e-board-observation-raw-page-limit))
      (signal 'e-board-observation-error
              (list "Raw participant read limit is out of bounds" limit)))
    (e-session-async-visible-message-page store participant-id limit)))

(defun e-board-observation--context-target (context)
  "Return CONTEXT's explicit SQL Board target."
  (let* ((harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (binding (and harness session-id
                       (e-chat-service-binding harness session-id))))
    (unless binding
      (signal 'e-board-observation-error
              (list "Board binding is not ready" session-id)))
    (e-chat-service-publication-target binding)))

(defun e-board-observation--participant-id (arguments)
  "Return the required durable participant id from ARGUMENTS."
  (let ((participant-id (plist-get arguments :participant-id)))
    (unless (and (stringp participant-id)
                 (not (string-empty-p participant-id)))
      (signal 'wrong-type-argument
              (list 'non-empty-string-p participant-id)))
    participant-id))

(defun e-board-observation--list (context arguments)
  "Return CONTEXT's detached Board participant/activity page."
  (e-board-observation-activity-page-start
   (e-board-observation--context-target context)
   :after (plist-get arguments :after)
   :limit (or (plist-get arguments :limit)
              e-board-observation-default-page-limit)))

(defun e-board-observation--status (context arguments)
  "Return one exact durable participant projection for CONTEXT."
  (e-board-observation-activity-participant-start
   (e-board-observation--context-target context)
   (e-board-observation--participant-id arguments)))

(defun e-board-observation--read (context arguments)
  "Return a durable participant projection or raw transcript page."
  (let ((participant-id (e-board-observation--participant-id arguments)))
    (if (plist-get arguments :raw)
        (e-board-observation-session-page-start
         (e-chat-service-session-store (plist-get context :harness))
         participant-id
         (or (plist-get arguments :limit)
             e-board-observation-raw-page-limit))
      (e-board-observation-activity-participant-start
       (e-board-observation--context-target context) participant-id))))

(defun e-board-observation--action (handler parameters)
  "Return an async observation action descriptor for HANDLER."
  (e-action-create
   :parameters parameters
   :work
   (e-work-spec-create
    :id "board-observation-action"
    :execution 'cooperative :interactive-policy 'async
    :owner 'board-observation
    :runner
    (lambda (parent arguments context)
      (let ((child (funcall handler context arguments)))
        (unless (e-work-handle-p child)
          (signal 'e-board-observation-error
                  (list "Board observation handler did not return work")))
        (setf (e-work-handle-cancel-function parent)
              (lambda (_handle) (e-work-cancel child)))
        (e-work-on-settle
         child
         (lambda (settled)
           (pcase (plist-get (e-work-status settled) :state)
             ('finished (e-work-finish parent
                                       (e-work-handle-result settled)))
             ('failed (e-work-fail parent (e-work-handle-error settled)))
             ('cancelled (e-work-cancel parent)))))
        :deferred)))))

(defconst e-board-observation--list-parameters
  '(:type "object"
    :properties
    (:after (:type "string" :description "Opaque Board activity cursor.")
     :limit (:type "integer" :description "Maximum participant rows."))
    :required [])
  "Action parameters for bounded Board participant listing.")

(defconst e-board-observation--participant-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string" :description "Durable Board participant/session id."))
    :required ["participant-id"])
  "Action parameters for one durable participant lookup.")

(defconst e-board-observation--read-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string" :description "Durable Board participant/session id.")
     :raw
     (:type "boolean"
      :description "Return a bounded durable transcript page.")
     :limit
     (:type "integer" :description "Maximum raw transcript messages."))
    :required ["participant-id"])
  "Action parameters for durable participant reads.")

(defun e-board-observation-parent-alist ()
  "Return Board-backed parent observation actions.
The three actions share the same explicit Board target and never consult the
private live execution owner."
  (list :list
        (e-board-observation--action
         #'e-board-observation--list e-board-observation--list-parameters)
        :status
        (e-board-observation--action
         #'e-board-observation--status
         e-board-observation--participant-parameters)
        :read
        (e-board-observation--action
         #'e-board-observation--read e-board-observation--read-parameters)))

(provide 'e-board-observation)

;;; e-board-observation.el ends here

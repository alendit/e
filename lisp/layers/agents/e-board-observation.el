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
(require 'e-json)
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

(defun e-board-observation-activity-detail-start
    (target record-id store)
  "Return work for authorized summary RECORD-ID on TARGET.
STORE is an internal session-owner dependency supplied by the admitted
observation context; the public action accepts only the Board target and
record identity."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-board-observation-error
            (list "Summary detail requires SQLite session storage")))
  (unless (and (stringp record-id) (not (string-empty-p record-id)))
    (signal 'e-board-observation-error
            (list "Summary record identity must be a non-empty string")))
  (let ((board-work
         (e-board-sqlite-publication-target-activity-detail-start
          target record-id)))
    (e-work-start
     (e-work-spec-create
      :id "board-observation-activity-detail"
      :execution 'cooperative :interactive-policy 'async
      :owner 'board-observation
      :runner
      (lambda (parent arguments _context)
        (let ((current (plist-get arguments :board-work))
              (store (plist-get arguments :store)))
          (cl-labels
              ((finish-session (settled)
                 (pcase (plist-get (e-work-status settled) :state)
                   ('finished
                    (e-work-finish
                     parent
                     (list :summary
                           (plist-get (e-work-handle-result settled)
                                      :summary))))
                   ('failed (e-work-fail parent
                                         (e-work-handle-error settled)))
                   ('cancelled (e-work-cancel parent))))
               (finish-board (settled)
                 (pcase (plist-get (e-work-status settled) :state)
                   ('finished
                    (let* ((source (e-work-handle-result settled))
                           (session-id (plist-get source :session-id))
                           (entry-id (plist-get source :activity-entry-id)))
                      (if (not (and (stringp session-id)
                                    (stringp entry-id)))
                          (e-work-fail
                           parent
                           (list 'e-board-observation-error
                                 "Board summary source is unavailable"))
                        (when (eq (plist-get (e-work-status parent) :state)
                                  'started)
                          (condition-case _error
                              (let ((session-work
                                     (e-session-async--reasoning-summary
                                      store session-id entry-id)))
                                (setf current session-work
                                      (e-work-handle-cancel-function parent)
                                      (lambda (_handle)
                                        (e-work-cancel session-work)))
                                (e-work-on-settle session-work #'finish-session))
                            (error
                             (e-work-fail
                              parent
                              (list 'e-board-observation-error
                                    "Unable to start summary detail"))))))))
                   ('failed (e-work-fail parent
                                         (e-work-handle-error settled)))
                   ('cancelled (e-work-cancel parent)))))
            (setf (e-work-handle-cancel-function parent)
                  (lambda (_handle) (e-work-cancel current)))
            (e-work-on-settle current #'finish-board)
            :deferred))))
     (list :board-work board-work :store store))))

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
   :run-id (plist-get arguments :run-id)
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
    (if (eq (plist-get arguments :raw) t)
        (e-board-observation-session-page-start
         (e-chat-service-session-store (plist-get context :harness))
         participant-id
         (or (plist-get arguments :limit)
             e-board-observation-raw-page-limit))
      (e-board-observation-activity-participant-start
       (e-board-observation--context-target context) participant-id))))

(defun e-board-observation--detail (context arguments)
  "Resolve one Board-authorized summary detail without exposing its source."
  (let* ((harness (plist-get context :harness))
         (record-id (plist-get arguments :record-id)))
    (unless harness
      (signal 'e-board-observation-error
              (list "Board summary detail requires a harness context")))
    (e-board-observation-activity-detail-start
     (e-board-observation--context-target context)
     record-id
     (e-chat-service-session-store harness))))

(defun e-board-observation--canonical-string (value)
  "Return VALUE as a canonical string or explicit JSON null."
  (cond
   ((stringp value) value)
   ((null value) e-json-null)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun e-board-observation--canonical-number (value)
  "Return finite numeric VALUE or explicit JSON null."
  (if (and (numberp value) (e-json-value-p value)) value e-json-null))

(defun e-board-observation--canonical-value (value)
  "Return canonical VALUE, or an explicit null for omitted domain data."
  (if (e-json-value-p value) value e-json-null))

(defun e-board-observation--canonical-participant (value)
  "Project durable participant VALUE into the observation JSON shape."
  (list :id (e-board-observation--canonical-string (plist-get value :id))
        :name (e-board-observation--canonical-string
               (plist-get value :name))
        :author (e-board-observation--canonical-string
                 (plist-get value :author))
        :principal (e-board-observation--canonical-string
                    (plist-get value :principal))
        :controller (e-board-observation--canonical-string
                     (plist-get value :controller))
        :role (e-board-observation--canonical-string (plist-get value :role))
        :state (e-board-observation--canonical-string
                (plist-get value :state))
        :subscription-id (e-board-observation--canonical-string
                          (plist-get value :subscription-id))
        :tags (vconcat (mapcar #'e-board-observation--canonical-string
                               (or (plist-get value :tags) nil)))
        :metadata (e-board-observation--canonical-value
                   (plist-get value :metadata))))

(defun e-board-observation--canonical-outcome (value)
  "Project participant OUTCOME VALUE into canonical JSON."
  (list :source (e-board-observation--canonical-string
                 (plist-get value :source))
        :status (e-board-observation--canonical-string
                 (plist-get value :status))
        :summary (e-board-observation--canonical-string
                  (plist-get value :summary))
        :error (e-board-observation--canonical-string (plist-get value :error))
        :result (e-board-observation--canonical-value
                 (plist-get value :result))
        :outputs (vconcat (mapcar #'e-board-observation--canonical-value
                                  (or (plist-get value :outputs) nil)))
        :finished-at (e-board-observation--canonical-number
                      (plist-get value :finished-at))))

(defun e-board-observation--canonical-row (value)
  "Project one participant activity ROW into canonical JSON."
  (list :participant-id (e-board-observation--canonical-string
                         (plist-get value :participant-id))
        :session-id (e-board-observation--canonical-string
                     (plist-get value :session-id))
        :name (e-board-observation--canonical-string (plist-get value :name))
        :principal (e-board-observation--canonical-string
                    (plist-get value :principal))
        :role (e-board-observation--canonical-string (plist-get value :role))
        :state (e-board-observation--canonical-string
                (plist-get value :state))
        :participant (e-board-observation--canonical-participant
                      (plist-get value :participant))
        :run-id (e-board-observation--canonical-string (plist-get value :run-id))
        :task-key (e-board-observation--canonical-string
                   (plist-get value :task-key))
        :attempt (e-board-observation--canonical-number
                  (plist-get value :attempt))
        :subagent-role (e-board-observation--canonical-string
                        (plist-get value :subagent-role))
        :reasoning-summary-preview
        (e-board-observation--canonical-string
         (plist-get value :reasoning-summary-preview))
        :reasoning-summary-record-id
        (e-board-observation--canonical-string
         (plist-get value :reasoning-summary-record-id))
        :outcome (if (plist-member value :outcome)
                     (e-board-observation--canonical-outcome
                      (plist-get value :outcome))
                   e-json-null)))

(defun e-board-observation--canonical-run (value)
  "Project selected-run VALUE into canonical JSON."
  (list :run-id (e-board-observation--canonical-string
                 (plist-get value :run-id))
        :label (e-board-observation--canonical-string
                (plist-get value :label))
        :terminal-status
        (e-board-observation--canonical-string
         (plist-get value :terminal-status))))

(defun e-board-observation--canonical-run-outcome (value)
  "Project selected task OUTCOME VALUE into canonical JSON."
  (list :status (e-board-observation--canonical-string
                 (plist-get value :status))
        :summary (e-board-observation--canonical-string
                  (plist-get value :summary))
        :error (e-board-observation--canonical-string
                (plist-get value :error))))

(defun e-board-observation--canonical-task (value)
  "Project selected run TASK VALUE into canonical JSON."
  (list :task-key (e-board-observation--canonical-string
                   (plist-get value :task-key))
        :required (if (eq (plist-get value :required) t) t e-json-false)
        :accepted-attempt
        (e-board-observation--canonical-number
         (plist-get value :accepted-attempt))
        :state (e-board-observation--canonical-string (plist-get value :state))
        :label (e-board-observation--canonical-string (plist-get value :label))
        :participant-id
        (e-board-observation--canonical-string
         (plist-get value :participant-id))
        :participant
        (if (plist-member value :participant-row)
            (e-board-observation--canonical-row
             (plist-get value :participant-row))
          e-json-null)
        :outcome
        (if (plist-member value :outcome)
            (e-board-observation--canonical-run-outcome
             (plist-get value :outcome))
          e-json-null)))

(defun e-board-observation--canonical-page (value)
  "Project an activity PAGE into canonical JSON."
  (append
   (list :board-id (e-board-observation--canonical-string
                    (plist-get value :board-id))
         :generation (e-board-observation--canonical-number
                      (plist-get value :generation))
         :revision (e-board-observation--canonical-number
                    (plist-get value :revision))
         :after (e-board-observation--canonical-string
                 (plist-get value :after)))
   (when (plist-member value :run)
     (list :run (e-board-observation--canonical-run
                 (plist-get value :run))))
   (when (plist-member value :tasks)
     (list :tasks
           (vconcat (mapcar #'e-board-observation--canonical-task
                            (plist-get value :tasks)))))
   (list :participants
         (vconcat (mapcar #'e-board-observation--canonical-row
                          (or (plist-get value :participants) nil)))
         :next (e-board-observation--canonical-string (plist-get value :next))
         :cursor (e-board-observation--canonical-string
                  (plist-get value :cursor))
         :bytes (e-board-observation--canonical-number
                 (plist-get value :bytes)))))

(defun e-board-observation--canonical-message (value)
  "Project one durable transcript MESSAGE into canonical JSON."
  (list :id (e-board-observation--canonical-string (plist-get value :id))
        :role (e-board-observation--canonical-string (plist-get value :role))
        :content (cond
                  ((stringp (plist-get value :content))
                   (plist-get value :content))
                  ((e-json-value-p (plist-get value :content))
                   (plist-get value :content))
                  ((null (plist-get value :content)) e-json-null)
                  (t (format "%s" (plist-get value :content))))
        :name (e-board-observation--canonical-string (plist-get value :name))
        :tool-call-id (e-board-observation--canonical-string
                       (plist-get value :tool-call-id))
        :turn-id (e-board-observation--canonical-string
                  (plist-get value :turn-id))
        :display (e-board-observation--canonical-string
                  (plist-get value :display))
        :details (e-board-observation--canonical-value
                  (plist-get value :details))))

(defun e-board-observation--canonical-message-page (value)
  "Project a raw durable message PAGE into canonical JSON."
  (list :session-id (e-board-observation--canonical-string
                     (plist-get value :session-id))
        :messages
        (vconcat (mapcar #'e-board-observation--canonical-message
                         (or (plist-get value :messages) nil)))
        :limit (e-board-observation--canonical-number (plist-get value :limit))
        :truncated (if (eq (plist-get value :truncated) t)
                       t e-json-false)
        :byte-count (e-board-observation--canonical-number
                     (plist-get value :byte-count))
        :byte-limit (e-board-observation--canonical-number
                     (plist-get value :byte-limit))
        :high-water (e-board-observation--canonical-number
                     (plist-get value :high-water))))

(defun e-board-observation--canonical-detail (value)
  "Project a summary-only detail DTO into canonical JSON."
  (list :summary (e-board-observation--canonical-string
                  (plist-get value :summary))))

(defun e-board-observation--canonical-result (value)
  "Project one Board observation result into canonical JSON."
  (cond
   ((and (listp value) (plist-member value :participants))
    (e-board-observation--canonical-page value))
   ((and (listp value) (plist-member value :messages))
    (e-board-observation--canonical-message-page value))
   ((and (listp value) (plist-member value :summary))
    (e-board-observation--canonical-detail value))
   ((and (listp value) (plist-member value :participant-id))
    (e-board-observation--canonical-row value))
   ((e-json-value-p value) value)
   (t (signal 'e-json-error
              (list "Board observation returned a noncanonical result")))))

(defun e-board-observation--action (handler parameters description)
  "Return an async observation action descriptor for HANDLER."
  (e-action-create
   :description description
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
             ('finished
              (e-work-finish
               parent
               (e-board-observation--canonical-result
                (e-work-handle-result settled))))
             ('failed (e-work-fail parent (e-work-handle-error settled)))
             ('cancelled (e-work-cancel parent)))))
        :deferred)))))

(defconst e-board-observation--list-parameters
  '(:type "object"
    :properties
    (:run-id
     (:type "string"
      :description "Optional durable run whose task dispositions should be included.")
     :after (:type "string" :description "Opaque Board activity cursor.")
     :limit (:type "integer" :description "Maximum participant rows."))
    :required []
    :additionalProperties :json-false)
  "Action parameters for bounded Board participant listing.")

(defconst e-board-observation--participant-parameters
  '(:type "object"
    :properties
    (:participant-id
     (:type "string" :description "Durable Board participant/session id."))
    :required ["participant-id"]
    :additionalProperties :json-false)
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
    :required ["participant-id"]
    :additionalProperties :json-false)
  "Action parameters for durable participant reads.")

(defconst e-board-observation--detail-parameters
  '(:type "object"
    :properties
    (:record-id
     (:type "string" :description "Board activity record identity."))
    :required ["record-id"]
    :additionalProperties :json-false)
  "Action parameters for one authorized summary detail read.")

(defun e-board-observation-parent-alist ()
  "Return Board-backed parent observation actions.
The actions share the same explicit Board target and never consult the
private live execution owner."
  (list :list
        (e-board-observation--action
         #'e-board-observation--list e-board-observation--list-parameters
         "List the current Board participant/activity page, optionally with one selected run's task dispositions.")
        :status
        (e-board-observation--action
         #'e-board-observation--status
         e-board-observation--participant-parameters
         "Read one committed participant outcome.")
        :read
        (e-board-observation--action
         #'e-board-observation--read e-board-observation--read-parameters
         "Read one participant projection or bounded durable transcript.")
        :detail
        (e-board-observation--action
         #'e-board-observation--detail e-board-observation--detail-parameters
         "Read one Board-authorized combined reasoning summary.")))

(provide 'e-board-observation)

;;; e-board-observation.el ends here

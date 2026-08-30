;;; e-session-catalog.el --- Pure session catalog/checkpoint projections -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The catalog is a value-oriented policy owner.  It selects bounded resume
;; state and produces index/checkpoint projections from explicit session and
;; board values.  It does not open files, mutate an aggregate, or call the
;; storage adapter.  The application root performs those side effects.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-session-codec)

(define-error 'e-session-catalog-error "Session catalog error")

(defun e-session-catalog--copy-value (value)
  "Return VALUE detached for a catalog projection.

`copy-tree' deliberately preserves strings, while catalog projections are
returned to callers and may outlive the aggregate that produced them.  Keep
this small copier local to the projection owner so no mutable aggregate value
can leak through a checkpoint or index boundary."
  (cond
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (vconcat (mapcar #'e-session-catalog--copy-value value)))
   ((consp value)
    (cons (e-session-catalog--copy-value (car value))
          (e-session-catalog--copy-value (cdr value))))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value)
                                 :size (hash-table-size value))))
      (maphash (lambda (key item)
                 (puthash (e-session-catalog--copy-value key)
                          (e-session-catalog--copy-value item) copy))
               value)
      copy))
   (t value)))

(defcustom e-session-checkpoint-activity-event-limit 64
  "Maximum recent activity entries retained in a resume checkpoint."
  :type 'integer :group 'e-session)

(defcustom e-session-checkpoint-marked-activity-event-limit 16
  "Maximum marked activity entries retained in a resume checkpoint."
  :type 'integer :group 'e-session)

(defcustom e-session-checkpoint-board-message-limit 256
  "Maximum recent board messages retained in a resume checkpoint."
  :type 'integer :group 'e-session)

(defcustom e-session-checkpoint-board-fact-limit 256
  "Maximum recent board facts retained in a resume checkpoint."
  :type 'integer :group 'e-session)

(defcustom e-session-checkpoint-process-report-limit 32
  "Maximum recent process reports retained in a resume checkpoint."
  :type 'integer :group 'e-session)

(defcustom e-session-load-chunk-bytes 65536
  "Number of bytes in one cooperative session-load step."
  :type 'integer :group 'e-session)

(defconst e-session-checkpoint-version 1
  "Current durable resume-checkpoint format version.")

(defun e-session-catalog-checkpoint-valid-p (checkpoint session-id)
  "Return non-nil when CHECKPOINT is a valid bounded resume value.

This is recovery policy over detached values only.  Physical file existence,
JSON parsing, and journal-offset comparison remain responsibilities of the
application service and storage adapter."
  (and (listp checkpoint)
       (= (or (plist-get checkpoint :version) -1)
          e-session-checkpoint-version)
       (equal (plist-get checkpoint :session-id) session-id)
       (integerp (plist-get checkpoint :journal-byte-offset))
       (>= (plist-get checkpoint :journal-byte-offset) 0)
       (listp (plist-get checkpoint :records))))

(defconst e-session-catalog--list-fields
  '(:session-events :messages :activity-events :branch-summaries
    :compactions :provider-anchors :process-reports
    :context-generations :context-promotions
    :context-curation-packages)
  "Append-only semantic fields represented in a session value.")

(defun e-session-catalog--session-entries (session)
  "Return all durable entries in SESSION order-independent list order."
  (apply #'append
         (mapcar (lambda (field) (copy-sequence (plist-get session field)))
                 e-session-catalog--list-fields)))

(defun e-session-catalog--entry-by-id (entries id)
  "Return entry ID from ENTRIES."
  (seq-find (lambda (entry) (equal (plist-get entry :id) id)) entries))

(defun e-session-catalog--path (session &optional head-id)
  "Return SESSION's canonical parent path ending at HEAD-ID."
  (let* ((entries (e-session-catalog--session-entries session))
         (head-id (or head-id (plist-get session :current-head-id)))
         result)
    (while head-id
      (when-let ((entry (e-session-catalog--entry-by-id entries head-id)))
        (push entry result)
        (setq head-id (plist-get entry :parent-id))))
    result))

(defun e-session-catalog--root-id (session)
  "Return SESSION's durable root event id."
  (or (plist-get session :root-event-id)
      (plist-get (car (plist-get session :session-events)) :id)))

(defun e-session-catalog--tail (items limit)
  "Return the last at most LIMIT ITEMS without changing their order."
  (let ((count (length items)))
    (copy-sequence (nthcdr (max 0 (- count limit)) items))))

(defun e-session-catalog--latest-valid-compaction (session)
  "Return latest compaction whose boundary occurs on SESSION's path."
  (let ((path (e-session-catalog--path session)))
    (seq-find
     (lambda (entry)
       (and (eq (plist-get entry :type) 'compaction)
            (let ((boundary (plist-get entry :first-kept-entry-id)))
              (and boundary
                   (e-session-catalog--entry-by-id path boundary)))))
     (reverse (plist-get session :compactions)))))

(defun e-session-catalog--checkpoint-path-suffix (session)
  "Return SESSION's resumable current-path suffix."
  (let ((path (e-session-catalog--path session)))
    (if-let* ((compaction (e-session-catalog--latest-valid-compaction session))
              (boundary (e-session-catalog--entry-by-id
                         path (plist-get compaction :first-kept-entry-id))))
        (member boundary path)
      path)))

(defun e-session-catalog--board-messages (messages)
  "Select the bounded board resume union from MESSAGES."
  (let* ((recent (e-session-catalog--tail
                  messages e-session-checkpoint-board-message-limit))
         (facts (e-session-catalog--tail
                 (cl-remove-if-not
                  (lambda (message)
                    (let ((kind (plist-get message :kind)))
                      (or (eq kind 'fact) (equal kind "fact"))))
                  messages)
                 e-session-checkpoint-board-fact-limit))
         (selected (make-hash-table :test 'eq)))
    (dolist (message recent) (puthash message t selected))
    (dolist (message facts) (puthash message t selected))
    (cl-remove-if-not (lambda (message) (gethash message selected)) messages)))

(defun e-session-catalog--context-components (entry)
  "Return context components carried by semantic ENTRY."
  (pcase (plist-get entry :type)
    ('context-promotion
     (when-let ((record (plist-get entry :context-record)))
       (list (cons 'context-promotion (copy-tree record)))))
    ('context-curation-package
     (delq nil
           (list (and (plist-get entry :promotion)
                      (cons 'context-promotion
                            (copy-tree (plist-get entry :promotion))))
                 (and (plist-get entry :erasure)
                      (cons 'context-erasure
                            (copy-tree (plist-get entry :erasure)))))))
    (_ nil)))

(defun e-session-catalog--context-state (path complete-path)
  "Return bounded context lifetime projection for PATH and COMPLETE-PATH."
  (let* ((generations
          (seq-filter (lambda (entry)
                        (eq (plist-get entry :type) 'context-generation))
                      complete-path))
         (generation-entry (car (last generations)))
         (generation (and generation-entry
                          (copy-tree (plist-get generation-entry :context-record))))
         (path-components
          (cl-mapcan
           (lambda (entry)
             (mapcar (lambda (component) (cons entry component))
                     (e-session-catalog--context-components entry)))
           path))
         (complete-components
          (cl-mapcan
           (lambda (entry)
             (mapcar (lambda (component) (cons entry component))
                     (e-session-catalog--context-components entry)))
           complete-path))
         promotions erasures promotion-entries erasure-entries)
    (dolist (pair path-components)
      (let* ((component (cdr pair))
             (record (cdr component)))
        (when (and generation
                   (eq (car component) 'context-promotion)
                   (equal (plist-get record :generation-id)
                          (plist-get generation :id)))
          (push (copy-tree record) promotions)
          (push (car pair) promotion-entries))))
    (dolist (pair complete-components)
      (when (eq (car (cdr pair)) 'context-erasure)
        (push (copy-tree (cdr (cdr pair))) erasures)
        (push (car pair) erasure-entries)))
    (let* ((erasures (nreverse erasures))
           (erasure-generation-ids
            (delete-dups (delq nil (mapcar (lambda (record)
                                             (plist-get record :generation-id))
                                           erasures))))
           (required-generations
            (seq-filter
             (lambda (entry)
               (or (eq entry generation-entry)
                   (member (plist-get (plist-get entry :context-record) :id)
                           erasure-generation-ids)))
             generations))
           (entries
            (seq-filter
             (lambda (entry)
               (or (memq entry required-generations)
                   (memq entry promotion-entries)
                   (memq entry erasure-entries)))
             complete-path)))
      (list :generation generation
            :generations
            (vconcat (mapcar (lambda (entry)
                               (copy-tree (plist-get entry :context-record)))
                             required-generations))
            :promotions (vconcat (mapcar #'copy-tree (nreverse promotions)))
            :erasures (vconcat erasures)
            :entry-ids (vconcat (mapcar (lambda (entry) (plist-get entry :id))
                                        entries))))))

(defun e-session-catalog--retained-entries (session)
  "Return ordered bounded durable entries needed to resume SESSION."
  (let* ((path (e-session-catalog--checkpoint-path-suffix session))
         (path-ids (mapcar (lambda (entry) (plist-get entry :id)) path))
         (complete-path (e-session-catalog--path session))
         (activity
          (e-session-catalog--tail
           (cl-remove-if-not
            (lambda (entry) (member (plist-get entry :id) path-ids))
            (plist-get session :activity-events))
           e-session-checkpoint-activity-event-limit))
         (marked-activity
          (e-session-catalog--tail
           (cl-remove-if-not
            (lambda (entry)
              (and (member (plist-get entry :id)
                           (mapcar (lambda (item) (plist-get item :id))
                                   complete-path))
                   (plist-get entry :checkpoint-retain)))
            (plist-get session :activity-events))
           e-session-checkpoint-marked-activity-event-limit))
         (reports
          (e-session-catalog--tail
           (cl-remove-if-not
            (lambda (entry) (member (plist-get entry :id) path-ids))
            (plist-get session :process-reports))
           e-session-checkpoint-process-report-limit))
         (anchors
          (cl-remove-if-not
           (lambda (anchor)
             (and (member (plist-get anchor :id) path-ids)
                  (member (plist-get anchor :covered-entry-id) path-ids)))
           (plist-get session :provider-anchors)))
         (curation-controls
          (cl-remove-if-not
           (lambda (entry)
             (and (eq (plist-get entry :type) 'activity-event)
                  (eq (plist-get entry :event-type)
                      'context-curation-response)))
           complete-path))
         (required-ids
          (delete-dups
           (append
            (mapcar (lambda (entry) (plist-get entry :id))
                    (seq-filter (lambda (entry)
                                  (memq (plist-get entry :type)
                                        '(message branch-summary compaction)))
                                path))
            (mapcar (lambda (entry) (plist-get entry :id)) activity)
            (mapcar (lambda (entry) (plist-get entry :id)) marked-activity)
            (when-let ((latest (plist-get session :latest-token-usage-event)))
              (list (plist-get latest :id)))
            (mapcar (lambda (entry) (plist-get entry :id)) reports)
            (mapcar (lambda (entry) (plist-get entry :id)) anchors)
            (mapcar (lambda (entry) (plist-get entry :id)) curation-controls)
            (append (append (plist-get (e-session-catalog--context-state
                                        path complete-path)
                                       :entry-ids)
                            nil)
                    nil)
            (list (plist-get session :current-head-id)
                  (and path (plist-get (car path) :id)))))))
    (cl-remove-if-not
     (lambda (entry)
       (and (not (equal (plist-get entry :id)
                        (e-session-catalog--root-id session)))
            (member (plist-get entry :id) required-ids)))
     complete-path)))

(defun e-session-catalog--root (session)
  "Return compact root state from SESSION."
  (list :id (e-session-catalog--root-id session)
        :created-at (plist-get session :created-at)
        :updated-at (plist-get session :updated-at)
        :metadata (copy-tree (plist-get session :metadata))
        :name (plist-get session :name)
        :turn-options (copy-tree (plist-get session :turn-options))
        :current-branch (plist-get session :current-branch)
        :board-output-sequence (or (plist-get session :board-output-sequence) 0)
        :board-activity-sequence (or (plist-get session :board-activity-sequence) 0)))

(defun e-session-catalog-checkpoint-manifest (session &optional board-messages)
  "Return semantic resume manifest for SESSION and BOARD-MESSAGES."
  (let* ((path (e-session-catalog--path session))
         (entries (e-session-catalog--retained-entries session))
         (context (e-session-catalog--context-state
                   (e-session-catalog--checkpoint-path-suffix session)
                   path))
         (board (e-session-catalog--board-messages (or board-messages nil))))
    (e-session-catalog--copy-value
     (list :session-id (plist-get session :id)
           :root (e-session-catalog--root session)
           :board-state (plist-get session :board-session-state)
           :context-lifetime context
           :entry-ids (vconcat (mapcar (lambda (entry) (plist-get entry :id)) entries))
           :board-message-identities
           (vconcat
            (mapcar (lambda (message)
                      (list :record-type
                            (or (plist-get message :record-type) 'board-message)
                            :id (plist-get message :id)))
                    board))))))

(defun e-session-catalog--checkpoint-entry-record (session-id entry parent-id)
  "Return replay record for ENTRY reparented to PARENT-ID."
  (let ((record (e-session-codec-record-for-entry session-id entry parent-id)))
    ;; The compact checkpoint root already carries the current session
    ;; metadata/name/options.  Historical checkpoints deliberately retained
    ;; only the identity-bearing session-info event here; repeating the full
    ;; metadata event would both enlarge the bounded projection and make a
    ;; legacy malformed root appear repaired by a later duplicate event.
    (when (and (eq (plist-get entry :type) 'session-event)
               (eq (plist-get entry :event-type) 'session-info))
      (setq record
            (list :type "session-info" :session-id session-id
                  :id (plist-get entry :id)
                  :parent-id parent-id
                  :timestamp (plist-get entry :created-at))))
    record))

(defun e-session-catalog--checkpoint-records (session &optional board-messages)
  "Return canonical replay records for SESSION's resume projection."
  (let* ((session-id (plist-get session :id))
         (root (e-session-catalog--root session))
         (records (list (append (list :type "session" :session-id session-id
                                      :timestamp (plist-get root :created-at))
                                root)))
         (parent-id (plist-get root :id)))
    (when-let ((state (plist-get session :board-session-state)))
      (push (list :type "board-session-state" :session-id session-id
                  :timestamp (plist-get root :updated-at)
                  :board-state (copy-tree state)
                  :board-id (plist-get state :board-id)
                  :principal (plist-get state :principal)
                  :board-output-sequence (plist-get root :board-output-sequence)
                  :board-activity-sequence (plist-get root :board-activity-sequence))
            records))
    (dolist (message (e-session-catalog--board-messages (or board-messages nil)))
      (push (list :type "board-message" :session-id session-id
                  :message (copy-tree message)) records))
    (dolist (entry (e-session-catalog--retained-entries session))
      (push (e-session-catalog--checkpoint-entry-record session-id entry parent-id)
            records)
      (setq parent-id (plist-get entry :id)))
    (e-session-catalog--copy-value (nreverse records))))

(defun e-session-catalog-checkpoint-json (session board-messages offset)
  "Return JSON-ready checkpoint value for SESSION at journal OFFSET."
  (e-session-catalog--copy-value
   (list :version e-session-checkpoint-version
         :session-id (plist-get session :id)
         :journal-byte-offset offset
         :records (vconcat (mapcar #'e-session-codec-record-for-json
                                   (e-session-catalog--checkpoint-records
                                    session board-messages)))
         :writer-high-watermarks nil)))

(defun e-session-catalog-index-entry (session &optional file)
  "Return the detached catalog projection for SESSION."
  (let ((state (plist-get session :board-session-state)))
    (e-session-catalog--copy-value
     (append
      (list :id (plist-get session :id)
            :name (plist-get session :name)
            :summary (plist-get session :summary)
            :metadata (plist-get session :metadata)
            :title (or (plist-get session :name)
                       (plist-get session :summary)
                       (format "Untitled %s" (plist-get session :id)))
            :message-count (or (plist-get session :message-count) 0)
            :created-at (plist-get session :created-at)
            :updated-at (plist-get session :updated-at)
            :updated-seq (plist-get session :updated-seq)
            :last-message-at (plist-get session :last-message-at)
            :latest-assistant-marker (plist-get session :latest-assistant-marker)
            :board-id (plist-get state :board-id)
            :principal (plist-get state :principal)
            :file file
            :loaded (plist-get session :loaded))
      (when (plist-member session :board-session-state)
        (list :board-state state))))))

(defun e-session-catalog-sort-index-entries (entries)
  "Return detached catalog ENTRIES in newest-message-first order.

The physical index preserves this semantic order so readers that inspect the
file directly retain the historical picker ordering.  Ties use the durable
update sequence, matching the aggregate's in-memory list operation."
  (sort (copy-sequence entries)
        (lambda (left right)
          (let ((left-time (or (plist-get left :last-message-at)
                               (plist-get left :created-at) ""))
                (right-time (or (plist-get right :last-message-at)
                                (plist-get right :created-at) ""))
                (left-seq (or (plist-get left :updated-seq) 0))
                (right-seq (or (plist-get right :updated-seq) 0)))
            (or (string> left-time right-time)
                (and (string= left-time right-time)
                     (> left-seq right-seq)))))))

(provide 'e-session-catalog)

;;; e-session-catalog.el ends here

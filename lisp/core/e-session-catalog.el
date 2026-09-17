;;; e-session-catalog.el --- Pure session catalog/checkpoint projections -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The catalog is a value-oriented policy owner.  It selects bounded resume
;; state and produces index/checkpoint projections from explicit session
;; values.  It does not open files, mutate an aggregate, or call the
;; storage adapter.  The application root performs those side effects.

;;; Code:

(require 'cl-lib)
(require 'e-json)
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

(defconst e-session-catalog--missing
  (make-symbol "e-session-catalog--missing")
  "Sentinel used when an equal-tested catalog lookup has no value.")

(cl-defstruct (e-session-catalog--analysis
              (:constructor e-session-catalog--analysis-create
                            (session entries path path-id-set path-id-index
                             suffix suffix-id-set)))
  "Invocation-local indexed analysis of one session projection.

The analysis is deliberately disposable.  It collects the supplied semantic
value once, preserves the first equal ID just as the historical linear lookup
did, and carries the path/set values needed by one manifest or checkpoint
projection.  It is not aggregate state and is never retained by storage."
  session entries path path-id-set path-id-index suffix suffix-id-set)

(defun e-session-catalog--analysis-entry (by-id id)
  "Resolve ID through the invocation-local indexed catalog BY-ID."
  (gethash id by-id e-session-catalog--missing))

(defun e-session-catalog--analysis-member-p (set key)
  "Return non-nil when KEY is present in the equal-tested indexed SET."
  (gethash key set))

(defun e-session-catalog--select-retained (items predicate)
  "Select ITEMS for one bounded retained-entry category using PREDICATE."
  (cl-remove-if-not predicate items))

(defun e-session-catalog--select-final-path (path predicate)
  "Select final retained entries from PATH using PREDICATE."
  (cl-remove-if-not predicate path))

(defun e-session-catalog--session-entries (session)
  "Return all durable entries in SESSION order-independent list order."
  (apply #'append
         (mapcar (lambda (field) (copy-sequence (plist-get session field)))
                 e-session-catalog--list-fields)))

(defun e-session-catalog--analysis-latest-valid-compaction
  (session path-id-set)
  "Return latest valid compaction for SESSION and indexed PATH-ID-SET."
  (seq-find
   (lambda (entry)
     (and (eq (plist-get entry :type) 'compaction)
          (let ((boundary (plist-get entry :first-kept-entry-id)))
            (and boundary
                 (e-session-catalog--analysis-member-p path-id-set boundary)))))
   (reverse (plist-get session :compactions))))

(defun e-session-catalog--analysis-build (session &optional head-id)
  "Build one indexed, invocation-local analysis for SESSION.

The equal hash index is populated in the exact order returned by
`e-session-catalog--session-entries`; an existing value is never overwritten.
This preserves the historical first-match behavior for duplicate IDs across
and within semantic catalog fields.  Missing parent IDs and repeated path IDs
are malformed catalog values and signal `e-session-catalog-error`."
  (let* ((entries (e-session-catalog--session-entries session))
         (by-id (make-hash-table :test #'equal
                                 :size (max 1 (length entries))))
         (cursor (or head-id (plist-get session :current-head-id)))
         (visited (make-hash-table :test #'equal
                                   :size (max 1 (length entries))))
         path)
    (dolist (entry entries)
      (let ((id (plist-get entry :id)))
        (when (eq (gethash id by-id e-session-catalog--missing)
                  e-session-catalog--missing)
          (puthash id entry by-id))))
    (while cursor
      (when (e-session-catalog--analysis-member-p visited cursor)
        (signal 'e-session-catalog-error
                (list (format "Session path repeats entry ID %S" cursor))))
      (puthash cursor t visited)
      (let ((entry (e-session-catalog--analysis-entry by-id cursor)))
        (if (eq entry e-session-catalog--missing)
            (signal 'e-session-catalog-error
                    (list (format "Session path cannot resolve entry ID %S"
                                  cursor)))
          (push entry path)
          (setq cursor (plist-get entry :parent-id)))))
    (let* ((path-id-set (make-hash-table :test #'equal
                                         :size (max 1 (length path))))
           (path-id-index (make-hash-table
                           :test #'equal :size (max 1 (length path)))))
      (dolist (entry path)
        (let ((id (plist-get entry :id)))
          (puthash id t path-id-set)
          (puthash id entry path-id-index)))
      (let* ((compaction
              (e-session-catalog--analysis-latest-valid-compaction
               session path-id-set))
           (boundary-id (and compaction
                               (plist-get compaction :first-kept-entry-id)))
             (boundary (and boundary-id
                            (e-session-catalog--analysis-entry
                             path-id-index boundary-id)))
             (suffix (if (and boundary
                              (e-session-catalog--analysis-member-p
                               path-id-set boundary-id))
                         ;; `by-id' and PATH share the same entry objects, so
                         ;; `memq' finds the exact historical first match without
                         ;; another equal linear membership scan.
                         (or (memq boundary path) path)
                       path))
             (suffix-id-set (make-hash-table :test #'equal
                                             :size (max 1 (length suffix)))))
        (dolist (entry suffix)
          (puthash (plist-get entry :id) t suffix-id-set))
        (e-session-catalog--analysis-create
         session entries path path-id-set path-id-index suffix suffix-id-set)))))

(defun e-session-catalog--path (session &optional head-id analysis)
  "Return SESSION's canonical parent path ending at HEAD-ID.

ANALYSIS, when supplied by a composed projection, reuses its indexed path.
Malformed unresolved or cyclic paths signal `e-session-catalog-error`."
  (e-session-catalog--analysis-path
   (or analysis (e-session-catalog--analysis-build session head-id))))

(defun e-session-catalog--root-id (session)
  "Return SESSION's durable root event id."
  (or (plist-get session :root-event-id)
      (plist-get (car (plist-get session :session-events)) :id)))

(defun e-session-catalog--tail (items limit)
  "Return the last at most LIMIT ITEMS without changing their order."
  (let ((count (length items)))
    (copy-sequence (nthcdr (max 0 (- count limit)) items))))

(defun e-session-catalog--latest-valid-compaction (session &optional analysis)
  "Return latest compaction whose boundary occurs on SESSION's path."
  (let ((analysis (or analysis (e-session-catalog--analysis-build session))))
    (e-session-catalog--analysis-latest-valid-compaction
     session (e-session-catalog--analysis-path-id-set analysis))))

(defun e-session-catalog--checkpoint-path-suffix (session &optional analysis)
  "Return SESSION's resumable current-path suffix."
  (e-session-catalog--analysis-suffix
   (or analysis (e-session-catalog--analysis-build session))))

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

(defun e-session-catalog--context-state (analysis)
  "Return bounded context lifetime projection from indexed ANALYSIS."
  (let* ((path (e-session-catalog--analysis-suffix analysis))
         (complete-path (e-session-catalog--analysis-path analysis))
         (generations
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
            (make-hash-table :test #'equal
                             :size (max 1 (length erasures))))
           (required-generations nil)
           (required-generation-set (make-hash-table :test #'eq))
           (promotion-entry-set (make-hash-table :test #'eq))
           (erasure-entry-set (make-hash-table :test #'eq)))
      (dolist (record erasures)
        (when-let ((generation-id (plist-get record :generation-id)))
          (puthash generation-id t erasure-generation-ids)))
      (dolist (entry generations)
        (when (or (eq entry generation-entry)
                  (e-session-catalog--analysis-member-p
                   erasure-generation-ids
                   (plist-get (plist-get entry :context-record) :id)))
          (push entry required-generations)))
      (setq required-generations (nreverse required-generations))
      (dolist (entry required-generations)
        (puthash entry t required-generation-set))
      (dolist (entry promotion-entries)
        (puthash entry t promotion-entry-set))
      (dolist (entry erasure-entries)
        (puthash entry t erasure-entry-set))
      (let ((entries
             (seq-filter
              (lambda (entry)
                (or (e-session-catalog--analysis-member-p
                     required-generation-set entry)
                    (e-session-catalog--analysis-member-p
                     promotion-entry-set entry)
                    (e-session-catalog--analysis-member-p
                     erasure-entry-set entry)))
              complete-path)))
        (list :generation generation
              :generations
              (vconcat (mapcar (lambda (entry)
                                 (copy-tree (plist-get entry :context-record)))
                               required-generations))
              :promotions (vconcat (mapcar #'copy-tree (nreverse promotions)))
              :erasures (vconcat erasures)
              :entry-ids (vconcat (mapcar (lambda (entry) (plist-get entry :id))
                                          entries)))))))

(defun e-session-catalog--retained-entries
    (session &optional analysis context)
  "Return ordered bounded durable entries needed to resume SESSION."
  (let* ((analysis (or analysis (e-session-catalog--analysis-build session)))
         (path (e-session-catalog--analysis-suffix analysis))
         (path-id-set (e-session-catalog--analysis-suffix-id-set analysis))
         (complete-path (e-session-catalog--analysis-path analysis))
         (complete-path-id-set
          (e-session-catalog--analysis-path-id-set analysis))
         (context (or context (e-session-catalog--context-state analysis)))
         (activity
          (e-session-catalog--tail
           (e-session-catalog--select-retained
            (plist-get session :activity-events)
            (lambda (entry)
              (e-session-catalog--analysis-member-p
               path-id-set (plist-get entry :id))))
           e-session-checkpoint-activity-event-limit))
         (marked-activity
          (e-session-catalog--tail
           (e-session-catalog--select-retained
            (plist-get session :activity-events)
            (lambda (entry)
              (and (e-session-catalog--analysis-member-p
                    complete-path-id-set (plist-get entry :id))
                   (plist-get entry :checkpoint-retain))))
           e-session-checkpoint-marked-activity-event-limit))
         (reports
          (e-session-catalog--tail
           (e-session-catalog--select-retained
            (plist-get session :process-reports)
            (lambda (entry)
              (e-session-catalog--analysis-member-p
               path-id-set (plist-get entry :id))))
           e-session-checkpoint-process-report-limit))
         (anchors
          (e-session-catalog--select-retained
           (plist-get session :provider-anchors)
           (lambda (anchor)
             (and (e-session-catalog--analysis-member-p
                  path-id-set (plist-get anchor :id))
                  (e-session-catalog--analysis-member-p
                   path-id-set (plist-get anchor :covered-entry-id))))))
         (curation-controls
          (e-session-catalog--select-retained
           complete-path
           (lambda (entry)
             (and (eq (plist-get entry :type) 'activity-event)
                  (eq (plist-get entry :event-type)
                      'context-curation-response)))))
         (required-ids (make-hash-table :test #'equal
                                        :size (max 1 (length path)))))
    (dolist (entry path)
      (when (memq (plist-get entry :type)
                  '(message branch-summary compaction))
        (puthash (plist-get entry :id) t required-ids)))
    (dolist (entries (list activity marked-activity reports anchors
                           curation-controls))
      (dolist (entry entries)
        (puthash (plist-get entry :id) t required-ids)))
    (when-let ((latest (plist-get session :latest-token-usage-event)))
      (puthash (plist-get latest :id) t required-ids))
    (let ((context-entry-ids (plist-get context :entry-ids)))
      (dotimes (index (length context-entry-ids))
        (puthash (aref context-entry-ids index) t required-ids)))
    (puthash (plist-get session :current-head-id) t required-ids)
    (puthash (and path (plist-get (car path) :id)) t required-ids)
    (e-session-catalog--select-final-path
     complete-path
     (lambda (entry)
       (and (not (equal (plist-get entry :id)
                        (e-session-catalog--root-id session)))
            (e-session-catalog--analysis-member-p
             required-ids (plist-get entry :id)))))))

(defun e-session-catalog--root (session)
  "Return compact root state from SESSION."
  (list :id (e-session-catalog--root-id session)
        :created-at (plist-get session :created-at)
        :updated-at (plist-get session :updated-at)
        :metadata (copy-tree (plist-get session :metadata))
        :name (plist-get session :name)
        :turn-options (copy-tree (plist-get session :turn-options))
        :current-branch (plist-get session :current-branch)))

(defun e-session-catalog-checkpoint-manifest (session)
  "Return semantic resume manifest for SESSION."
  (let* ((analysis (e-session-catalog--analysis-build session))
         (context (e-session-catalog--context-state analysis))
         (entries (e-session-catalog--retained-entries
                   session analysis context)))
    (e-session-catalog--copy-value
     (list :session-id (plist-get session :id)
           :root (e-session-catalog--root session)
           :context-lifetime context
           :entry-ids
           (vconcat (mapcar (lambda (entry) (plist-get entry :id)) entries))))))

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

(defun e-session-catalog--checkpoint-records (session)
  "Return canonical replay records for SESSION's resume projection."
  (let* ((analysis (e-session-catalog--analysis-build session))
         (context (e-session-catalog--context-state analysis))
         (retained (e-session-catalog--retained-entries
                    session analysis context))
         (session-id (plist-get session :id))
         (root (e-session-catalog--root session))
         (records (list (append (list :type "session" :session-id session-id
                                      :timestamp (plist-get root :created-at))
                                root)))
         (parent-id (plist-get root :id)))
    (dolist (entry retained)
      (push (e-session-catalog--checkpoint-entry-record session-id entry parent-id)
            records)
      (setq parent-id (plist-get entry :id)))
    (e-session-catalog--copy-value (nreverse records))))

(defun e-session-catalog-checkpoint-value (session offset)
  "Return exact semantic checkpoint value for SESSION at journal OFFSET."
  (e-session-catalog--copy-value
   (list :version e-session-checkpoint-version
         :session-id (plist-get session :id)
         :journal-byte-offset offset
         :records (vconcat (e-session-catalog--checkpoint-records session)))))

(defun e-session-catalog-checkpoint-json (session offset)
  "Return JSON-ready checkpoint value for SESSION at journal OFFSET."
  (let ((checkpoint
         (e-session-catalog-checkpoint-value session offset)))
    (setq checkpoint
          (plist-put
           checkpoint :records
           (vconcat (mapcar #'e-session-codec-record-for-json
                            (append (plist-get checkpoint :records) nil)))))
    (e-json-assert-value checkpoint)))

(defun e-session-catalog-index-entry (session &optional file)
  "Return the detached catalog projection for SESSION."
  (e-session-catalog--copy-value
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
            :file file
            :loaded (plist-get session :loaded))))

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

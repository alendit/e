;;; e-board-sqlite-service.el --- SQL-owned Board application service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Consumer-shaped asynchronous Board operations for ordinary SQLite
;; composition.  The service retains the runtime transport plus bounded live
;; pickup subscribers and executing-delivery outcome callbacks.  Canonical
;; identities, order, routing, pickup state, and read cursors are returned by
;; the worker transaction and are never predicted or mirrored here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-board-orchestration)
(require 'e-board-sqlite-contract)
(require 'e-runtime-store)
(require 'e-session)
(require 'e-session-board-policy)
(require 'e-session-query)
(require 'e-work)

(cl-defstruct (e-board-sqlite-live-hub
               (:constructor e-board-sqlite-live-hub--create))
  "Process-local callbacks shared by services over one runtime transport."
  (pickup-observers (make-hash-table :test 'equal))
  (delivery-outcome-observers (make-hash-table :test 'equal))
  (commit-observers (make-hash-table :test 'equal)))

(defvar e-board-sqlite-service--live-hubs
  (make-hash-table :test 'eq :weakness 'key-and-value)
  "Weak runtime-to-hub map for bounded live Board coordination.")

(defun e-board-sqlite-service--detached-copy (value)
  "Return a recursively detached copy of serializable VALUE.
Unlike `copy-tree', this copies mutable leaves such as strings, vectors, and
hash tables as well as cons structure.  Board application-service inputs are
bounded acyclic values suitable for the runtime-store codec; objects outside
that value language are immutable scalars and are returned unchanged."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value)
    (cons (e-board-sqlite-service--detached-copy (car value))
          (e-board-sqlite-service--detached-copy (cdr value))))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value)
                                 :size (max 1 (hash-table-count value)))))
      (maphash
       (lambda (key entry)
         (puthash (e-board-sqlite-service--detached-copy key)
                  (e-board-sqlite-service--detached-copy entry)
                  copy))
       value)
      copy))
   ((recordp value)
    (apply #'record
           (aref value 0)
           (cl-loop for index from 1 below (length value)
                    collect
                    (e-board-sqlite-service--detached-copy
                     (aref value index)))))
   ((vectorp value)
    (apply #'vector
           (mapcar #'e-board-sqlite-service--detached-copy
                   (append value nil))))
   ((bool-vector-p value) (copy-sequence value))
   (t value)))

(cl-defstruct (e-board-sqlite-service
               (:constructor e-board-sqlite-service--create))
  runtime
  live-hub)

(cl-defstruct (e-board-sqlite-publication-target
               (:constructor e-board-sqlite-publication-target--create)
               (:conc-name e-board-sqlite-publication-target--)
               (:copier nil))
  "A narrow immutable publication address for one durable Board.
SERVICE and BOARD-ID name the SQL authority.  The remaining fields are copied
defaults used only to form a requested transaction; the target contains no
Board aggregate, registry membership, or durable-state mirror."
  (service nil :read-only t)
  (board-id nil :read-only t)
  (author nil :read-only t)
  (requester-actor nil :read-only t)
  (tags nil :read-only t)
  (attributes nil :read-only t)
  (to nil :read-only t)
  (mode nil :read-only t))

(cl-defstruct (e-board-sqlite-delivery-observation
               (:constructor e-board-sqlite-delivery-observation--create)
               (:conc-name e-board-sqlite-delivery-observation--)
               (:copier nil))
  "One process-local callback awaiting a live delivery's terminal outcome."
  service delivery-id callback active-p)

(cl-defstruct (e-board-sqlite-pickup-observation
               (:constructor e-board-sqlite-pickup-observation--create)
               (:conc-name e-board-sqlite-pickup-observation--)
               (:copier nil))
  "One process-local subscriber for committed pickups on one live Board."
  service board-id callback active-p)

(cl-defstruct (e-board-sqlite-commit-observation
               (:constructor e-board-sqlite-commit-observation--create)
               (:conc-name e-board-sqlite-commit-observation--)
               (:copier nil))
  "One process-local subscriber for committed writes on one Board."
  service board-id callback active-p)

(cl-defstruct (e-board-sqlite-service-operation
               (:constructor e-board-sqlite-service--operation-create))
  service kind body owner-key work request settled)

(defun e-board-sqlite-service-create (runtime)
  "Return a Board application service over RUNTIME.
The service owns only transport and process-local live coordination; SQLite
owns every durable Board fact."
  (unless (e-runtime-store-p runtime)
    (signal 'wrong-type-argument (list 'e-runtime-store-p runtime)))
  (let ((hub
         (or (gethash runtime e-board-sqlite-service--live-hubs)
             (let ((created (e-board-sqlite-live-hub--create)))
               (puthash runtime created e-board-sqlite-service--live-hubs)
               created))))
    (e-board-sqlite-service--create :runtime runtime :live-hub hub)))

(cl-defun e-board-sqlite-service-session-admission
    (&key id metadata principal board-id association-role routing-policy)
  "Return root records and an SQL query delta for one Board-associated session.

The session journal contains only the session root.  The Board association is
installed directly into the relational current row in the same SQLite
transaction, so ordinary session replay never becomes a second Board store."
  (let* ((role (and association-role
                    (if (symbolp association-role)
                        (symbol-name association-role)
                      association-role)))
         (session (e-session-admission-records :id id :metadata metadata))
         (session-id (plist-get session :id)))
    (unless (and (stringp board-id) (not (string-empty-p board-id))
                 (stringp principal) (not (string-empty-p principal)))
      (signal 'e-board-sqlite-error
              (list "Invalid Board association identity"
                    session-id board-id principal)))
    (when (and role (not (member role '("owner" "participant"))))
      (signal 'e-board-sqlite-error
              (list "Invalid Board association role" role)))
    (when (and routing-policy
               (not (e-session-board-routing-policy-valid-p routing-policy)))
      (signal 'e-session-board-routing-invalid
              (list "Invalid Board routing policy" routing-policy)))
    (let* ((association
            (append
             (list :board-id (copy-sequence board-id)
                   :principal (copy-sequence principal))
             (when role (list :association-role role))
             (when routing-policy
               (list :routing-policy
                     (e-session-board-routing-policy-normalize
                      routing-policy)))))
           (records (plist-get session :admission-records))
           (root (copy-tree (car records) t))
           (query-delta nil))
      (plist-put root :journal-position 1)
      (setq query-delta (e-session-query-derive (list root)))
      (dolist (key '(:board-id :principal :association-role :routing-policy))
        (plist-put query-delta key
                   (e-board-sqlite-service--detached-copy
                    (plist-get association key))))
      (e-session-query-state-validate query-delta)
      (append session
              (list :association
                    (e-board-sqlite-service--detached-copy association)
                    :query-delta
                    (e-board-sqlite-service--detached-copy query-delta))))))

(defun e-board-sqlite-service--pickup-observer-table (service)
  "Return SERVICE's runtime-shared live pickup observer table."
  (e-board-sqlite-live-hub-pickup-observers
   (e-board-sqlite-service-live-hub service)))

(defun e-board-sqlite-service--delivery-observer-table (service)
  "Return SERVICE's runtime-shared live delivery observer table."
  (e-board-sqlite-live-hub-delivery-outcome-observers
   (e-board-sqlite-service-live-hub service)))

(defun e-board-sqlite-service--commit-observer-table (service)
  "Return SERVICE's runtime-shared committed-write observer table."
  (e-board-sqlite-live-hub-commit-observers
   (e-board-sqlite-service-live-hub service)))

(cl-defun e-board-sqlite-publication-target-create
    (service board-id &key author requester-actor tags attributes to
             (mode 'inject))
  "Return an explicit SQL publication target for BOARD-ID on SERVICE.
AUTHOR, REQUESTER-ACTOR, TAGS, ATTRIBUTES, TO, and MODE are immutable defaults
for later transactions.  Callers must still supply a stable source key for
each publication attempt."
  (unless (e-board-sqlite-service-p service)
    (signal 'wrong-type-argument (list 'e-board-sqlite-service-p service)))
  (unless (and (stringp board-id) (not (string-empty-p board-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p board-id)))
  (e-board-sqlite-publication-target--create
   :service service
   :board-id (e-board-sqlite-service--detached-copy board-id)
   :author (e-board-sqlite-service--detached-copy author)
   :requester-actor
   (e-board-sqlite-service--detached-copy requester-actor)
   :tags (e-board-sqlite-service--detached-copy tags)
   :attributes (e-board-sqlite-service--detached-copy attributes)
   :to (e-board-sqlite-service--detached-copy to) :mode mode))

(defun e-board-sqlite-publication-target-valid-p (target)
  "Return non-nil when TARGET names a usable SQL Board address."
  (and (e-board-sqlite-publication-target-p target)
       (e-board-sqlite-service-p
        (e-board-sqlite-publication-target--service target))
       (let ((board-id
              (e-board-sqlite-publication-target--board-id target)))
         (and (stringp board-id) (not (string-empty-p board-id))))))

(defun e-board-sqlite-publication-target-board-id (target)
  "Return a detached copy of TARGET's durable Board id."
  (e-board-sqlite-publication-target--require target)
  (e-board-sqlite-service--detached-copy
   (e-board-sqlite-publication-target--board-id target)))

(defun e-board-sqlite-pickup-observation-cancel (observation)
  "Cancel process-local pickup OBSERVATION and return non-nil when active."
  (when (and (e-board-sqlite-pickup-observation-p observation)
             (e-board-sqlite-pickup-observation--active-p observation))
    (let* ((service (e-board-sqlite-pickup-observation--service observation))
           (board-id (e-board-sqlite-pickup-observation--board-id observation))
           (table (e-board-sqlite-service--pickup-observer-table service))
           (remaining (delq observation (gethash board-id table))))
      (setf (e-board-sqlite-pickup-observation--active-p observation) nil)
      (if remaining
          (puthash board-id remaining table)
        (remhash board-id table))
      t)))

(defun e-board-sqlite-service-observe-pickups (service board-id callback)
  "Observe committed pickup batches for BOARD-ID through SERVICE.
CALLBACK receives a detached list of canonical pickup rows.  The observation
is process-local subscriber coordination only; durable pickup facts remain in
SQLite."
  (unless (e-board-sqlite-service-p service)
    (signal 'wrong-type-argument (list 'e-board-sqlite-service-p service)))
  (unless (and (stringp board-id) (not (string-empty-p board-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p board-id)))
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (let* ((table (e-board-sqlite-service--pickup-observer-table service))
         (key (copy-sequence board-id))
         (observation
          (e-board-sqlite-pickup-observation--create
           :service service :board-id key :callback callback :active-p t)))
    (puthash key (cons observation (gethash key table)) table)
    observation))

(defun e-board-sqlite-service--notify-pickups (service board-id pickups)
  "Notify SERVICE's live BOARD-ID subscribers of canonical PICKUPS."
  (let ((observations
         (copy-sequence
          (gethash board-id
                   (e-board-sqlite-service--pickup-observer-table service))))
        (detached (e-board-sqlite-service--detached-copy pickups)))
    (dolist (observation observations)
      (when (e-board-sqlite-pickup-observation--active-p observation)
        (funcall (e-board-sqlite-pickup-observation--callback observation)
                 (e-board-sqlite-service--detached-copy detached))))))

(defun e-board-sqlite-service-observe-commits (service board-id callback)
  "Observe committed writes for BOARD-ID through SERVICE.
CALLBACK receives no durable row and must issue its own bounded query.  The
observation is a wake-up path only; SQLite remains authoritative."
  (unless (e-board-sqlite-service-p service)
    (signal 'wrong-type-argument (list 'e-board-sqlite-service-p service)))
  (unless (and (stringp board-id) (not (string-empty-p board-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p board-id)))
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (let* ((table (e-board-sqlite-service--commit-observer-table service))
         (key (copy-sequence board-id))
         (observation
          (e-board-sqlite-commit-observation--create
           :service service :board-id key :callback callback :active-p t)))
    (puthash key (cons observation (gethash key table)) table)
    observation))

(defun e-board-sqlite-commit-observation-cancel (observation)
  "Cancel committed-write OBSERVATION and return non-nil when active."
  (when (and (e-board-sqlite-commit-observation-p observation)
             (e-board-sqlite-commit-observation--active-p observation))
    (let* ((service (e-board-sqlite-commit-observation--service observation))
           (board-id (e-board-sqlite-commit-observation--board-id observation))
           (table (e-board-sqlite-service--commit-observer-table service))
           (remaining (delq observation (gethash board-id table))))
      (setf (e-board-sqlite-commit-observation--active-p observation) nil)
      (if remaining
          (puthash board-id remaining table)
        (remhash board-id table))
      t)))

(defun e-board-sqlite-service--notify-commits (service board-id)
  "Wake committed-write subscribers for BOARD-ID."
  (dolist (observation
           (copy-sequence
            (gethash board-id
                     (e-board-sqlite-service--commit-observer-table service))))
    (when (e-board-sqlite-commit-observation--active-p observation)
      (condition-case error
          (funcall (e-board-sqlite-commit-observation--callback observation))
        (error
         ;; One request-owned wake-up must not suppress another subscriber.
         (message "e Board commit observer failed: %s"
                  (e-work-error-message error)))))))

(defun e-board-sqlite-service--publish-committed-pickups (work service)
  "Publish WORK's committed pickup result to live SERVICE subscribers."
  (let* ((status (e-work-status work))
         (result (and (eq (plist-get status :state) 'finished)
                      (plist-get status :result)))
         (board-id (plist-get result :board-id))
         (pickups (plist-get result :pickups)))
    (when (and board-id pickups)
      ;; Defer one event-loop turn.  Request-owned settlement callbacks must be
      ;; able to install delivery-outcome observations before a fast live
      ;; consumer can claim and finish the newly committed pickup.
      (run-at-time
       0 nil
       (lambda ()
         (e-board-sqlite-service--notify-pickups
          service board-id pickups))))))

(defun e-board-sqlite-delivery-observation-cancel (observation)
  "Cancel process-local delivery OBSERVATION and return non-nil when active."
  (when (and (e-board-sqlite-delivery-observation-p observation)
             (e-board-sqlite-delivery-observation--active-p observation))
    (let* ((service (e-board-sqlite-delivery-observation--service observation))
           (delivery-id
            (e-board-sqlite-delivery-observation--delivery-id observation))
           (table (e-board-sqlite-service--delivery-observer-table service))
           (remaining (delq observation (gethash delivery-id table))))
      (setf (e-board-sqlite-delivery-observation--active-p observation) nil)
      (if remaining
          (puthash delivery-id remaining table)
        (remhash delivery-id table))
      t)))

(defun e-board-sqlite-publication-target-observe-delivery-outcome
    (target delivery-id callback)
  "Observe DELIVERY-ID's live terminal outcome through TARGET.
CALLBACK receives (STATUS PAYLOAD).  The observation is process-local executing
coordination only; SQLite remains authoritative for durable pickup state."
  (e-board-sqlite-publication-target--require target)
  (unless delivery-id
    (signal 'wrong-type-argument (list 'non-nil-delivery-id delivery-id)))
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (let* ((service (e-board-sqlite-publication-target--service target))
         (table (e-board-sqlite-service--delivery-observer-table service))
         (key (e-board-sqlite-service--detached-copy delivery-id))
         (observation
          (e-board-sqlite-delivery-observation--create
           :service service :delivery-id key :callback callback :active-p t)))
    (puthash key (cons observation (gethash key table)) table)
    observation))

(defun e-board-sqlite-service-notify-delivery-outcome
    (service delivery-id status payload)
  "Notify SERVICE observers of DELIVERY-ID's live STATUS and PAYLOAD once.
This is the narrow bridge from the live chat controller's canonical turn event
to request-owned producer work.  It stores no terminal outcome."
  (unless (e-board-sqlite-service-p service)
    (signal 'wrong-type-argument (list 'e-board-sqlite-service-p service)))
  (let* ((table (e-board-sqlite-service--delivery-observer-table service))
         (observations (gethash delivery-id table))
         first-error)
    (remhash delivery-id table)
    (dolist (observation observations)
      (when (e-board-sqlite-delivery-observation--active-p observation)
        (setf (e-board-sqlite-delivery-observation--active-p observation) nil)
        (condition-case error
            (funcall
             (e-board-sqlite-delivery-observation--callback observation)
             status
             (e-board-sqlite-service--detached-copy payload))
          (error
           ;; One request-owned callback must not prevent settlement of the
           ;; other observers for the same canonical delivery.  Re-signal the
           ;; first defect only after every observer has been released.
           (unless first-error
             (setq first-error error))))))
    (when first-error
      (signal (car first-error) (cdr first-error)))
    (and observations t)))

(defun e-board-sqlite-publication-target--require (target)
  "Return TARGET after validating its SQL publication shape."
  (unless (e-board-sqlite-publication-target-valid-p target)
    (signal 'wrong-type-argument
            (list 'e-board-sqlite-publication-target-p target)))
  target)

(cl-defun e-board-sqlite-publication-target-append-route-start
    (target content source-input-key
            &key author requester-actor tags attributes to mode reference
            created-at)
  "Append and route CONTENT through TARGET using stable SOURCE-INPUT-KEY."
  (e-board-sqlite-publication-target--require target)
  (e-board-sqlite-service-append-route-start
   (e-board-sqlite-publication-target--service target)
   (e-board-sqlite-publication-target--board-id target)
   :author (or (e-board-sqlite-service--detached-copy author)
               (e-board-sqlite-service--detached-copy
                (e-board-sqlite-publication-target--author target)))
   :requester-actor
   (or (e-board-sqlite-service--detached-copy requester-actor)
       (e-board-sqlite-service--detached-copy
        (e-board-sqlite-publication-target--requester-actor target)))
   :tags (append
          (e-board-sqlite-service--detached-copy
           (e-board-sqlite-publication-target--tags target))
          (e-board-sqlite-service--detached-copy tags))
   :attributes
   (append
    (e-board-sqlite-service--detached-copy
     (e-board-sqlite-publication-target--attributes target))
    (e-board-sqlite-service--detached-copy attributes))
   :to (or (e-board-sqlite-service--detached-copy to)
           (e-board-sqlite-service--detached-copy
            (e-board-sqlite-publication-target--to target)))
   :mode (or mode (e-board-sqlite-publication-target--mode target) 'inject)
   :content (e-board-sqlite-service--detached-copy content)
   :reference (e-board-sqlite-service--detached-copy reference)
   :source-input-key (e-board-sqlite-service--detached-copy source-input-key)
   :created-at created-at))

(cl-defun e-board-sqlite-publication-target-record-append-start
    (target record-kind source-kind source-key &rest record-fields)
  "Append one canonical non-routed record through TARGET."
  (e-board-sqlite-publication-target--require target)
  (apply #'e-board-sqlite-service-record-append-start
         (e-board-sqlite-publication-target--service target)
         (e-board-sqlite-publication-target--board-id target)
         record-kind source-kind
         (e-board-sqlite-service--detached-copy source-key)
         (e-board-sqlite-service--detached-copy record-fields)))

(cl-defun e-board-sqlite-publication-target-fact-start
    (target content source-fact-key &key author tags attributes reference)
  "Append one observation-only fact through TARGET."
  (e-board-sqlite-publication-target-record-append-start
   target 'fact 'fact source-fact-key
   :author (or (e-board-sqlite-service--detached-copy author)
               (e-board-sqlite-service--detached-copy
                (e-board-sqlite-publication-target--author target)))
   :tags (append
          (e-board-sqlite-service--detached-copy
           (e-board-sqlite-publication-target--tags target))
          (e-board-sqlite-service--detached-copy tags))
   :attributes
   (append
    (e-board-sqlite-service--detached-copy
     (e-board-sqlite-publication-target--attributes target))
    (e-board-sqlite-service--detached-copy attributes))
   :content (e-board-sqlite-service--detached-copy content)
   :reference (e-board-sqlite-service--detached-copy reference)))

(cl-defun e-board-sqlite-publication-target-orchestration-fact-start
    (target fact &key author)
  "Validate and append orchestration FACT through TARGET."
  (e-board-sqlite-publication-target--require target)
  (e-board-sqlite-service-orchestration-fact-start
   (e-board-sqlite-publication-target--service target)
   (e-board-sqlite-publication-target--board-id target) fact
   :author (or (e-board-sqlite-service--detached-copy author)
               (e-board-sqlite-service--detached-copy
                (e-board-sqlite-publication-target--author target)))))

(defun e-board-sqlite-publication-target-orchestration-run-start
    (target run-id &optional limit)
  "Read TARGET's bounded durable RUN-ID fact set."
  (e-board-sqlite-publication-target--require target)
  (e-board-sqlite-service-orchestration-run-start
   (e-board-sqlite-publication-target--service target)
   (e-board-sqlite-publication-target--board-id target) run-id limit))

(defun e-board-sqlite-publication-target-orchestration-runs-start
    (target &optional limit)
  "Read facts for TARGET's newest bounded durable run set."
  (e-board-sqlite-publication-target--require target)
  (e-board-sqlite-service-orchestration-runs-start
   (e-board-sqlite-publication-target--service target)
   (e-board-sqlite-publication-target--board-id target) limit))

(defun e-board-sqlite-publication-target-orchestration-active-runs-start
    (target &optional limit now)
  "Read TARGET's bounded priority page of active run-index summaries."
  (e-board-sqlite-publication-target--require target)
  (e-board-sqlite-service-orchestration-active-runs-start
   (e-board-sqlite-publication-target--service target)
   (e-board-sqlite-publication-target--board-id target) limit now))

(cl-defun e-board-sqlite-publication-target-orchestration-run-index-page-start
    (target &key cursor active-only include-continuation-ref limit)
  "Read one stable current-generation run-index page from TARGET."
  (e-board-sqlite-publication-target--require target)
   (e-board-sqlite-service-orchestration-run-index-page-start
   (e-board-sqlite-publication-target--service target)
   (e-board-sqlite-publication-target--board-id target)
   :cursor cursor :active-only active-only
   :include-continuation-ref include-continuation-ref :limit limit))

(defun e-board-sqlite-publication-target-board-owner-resolve-start (target)
  "Read TARGET's bounded current owner candidates."
  (e-board-sqlite-publication-target--require target)
  (e-board-sqlite-service-board-owner-resolve-start
   (e-board-sqlite-publication-target--service target)
   (e-board-sqlite-publication-target--board-id target)))

(defun e-board-sqlite-publication-target-activity-page-start
    (target &rest arguments)
  "Read TARGET's bounded detached Board participant/activity page.
The request is owned by the Board SQL service; TARGET contributes only its
durable Board address and no aggregate or live execution state.  ARGUMENTS are
the keyword arguments accepted by `e-board-sqlite-service-activity-page-start'."
  (e-board-sqlite-publication-target--require target)
  (apply #'e-board-sqlite-service-activity-page-start
         (e-board-sqlite-publication-target--service target)
         (e-board-sqlite-publication-target--board-id target)
         arguments))

(defun e-board-sqlite-publication-target-activity-detail-start
    (target record-id)
  "Read authorized summary detail RECORD-ID from TARGET."
  (e-board-sqlite-publication-target--require target)
  (e-board-sqlite-service-activity-detail-start
   (e-board-sqlite-publication-target--service target)
   (e-board-sqlite-publication-target--board-id target) record-id))

(defun e-board-sqlite-publication-target-record-page-start
    (target &rest arguments)
  "Read a bounded canonical record page from TARGET using ARGUMENTS."
  (e-board-sqlite-publication-target--require target)
  (apply #'e-board-sqlite-service-record-page-start
         (e-board-sqlite-publication-target--service target)
         (e-board-sqlite-publication-target--board-id target)
         arguments))

(defun e-board-sqlite-service--settle (operation request)
  "Settle OPERATION exactly once from terminal runtime REQUEST."
  (unless (e-board-sqlite-service-operation-settled operation)
    (setf (e-board-sqlite-service-operation-settled operation) t
          (e-board-sqlite-service-operation-request operation) nil)
    (let* ((work (e-board-sqlite-service-operation-work operation))
           (committed-p (eq (e-runtime-store-request--state request)
                            'committed))
           (result (and committed-p
                        (e-board-sqlite-service--detached-copy
                         (e-runtime-store-request--result request))))
           (board-id (plist-get (e-board-sqlite-service-operation-body operation)
                                :board-id)))
      (if committed-p
          (progn
            ;; Wake Board-owned projection consumers before acknowledging the
            ;; write to their callers.  Otherwise a readiness callback can
            ;; observe the committed write while the shared run-set still
            ;; represents the previous SQL snapshot.
            (when (and (eq (e-board-sqlite-service-operation-kind operation)
                           'write)
                       board-id)
              (e-board-sqlite-service--notify-commits
               (e-board-sqlite-service-operation-service operation) board-id))
            (e-work-finish work result))
        (e-work-fail
         work
         (or (e-board-sqlite-service--detached-copy
              (e-runtime-store-request--error request))
             '(e-board-sqlite-error "Board operation did not commit")))))))

(defun e-board-sqlite-service--run (handle operation _context)
  "Submit OPERATION without waiting for worker open or acknowledgement."
  (let* ((service (e-board-sqlite-service-operation-service operation))
         (runtime (e-board-sqlite-service-runtime service))
         (kind (e-board-sqlite-service-operation-kind operation))
         (request
          (if-let* ((owner-key
                     (e-board-sqlite-service-operation-owner-key operation)))
              (e-runtime-store--submit-owned
               runtime kind (e-board-sqlite-service-operation-body operation)
               owner-key)
            (e-runtime-store-submit
             runtime kind (e-board-sqlite-service-operation-body operation)))))
    (setf (e-board-sqlite-service-operation-work operation) handle
          (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (when-let* ((pending
                         (e-board-sqlite-service-operation-request operation)))
              (e-runtime-store-cancel runtime pending))))
    (e-runtime-store--observe
     request
     (lambda (settled)
       (e-board-sqlite-service--settle operation settled)))
    (unless (e-board-sqlite-service-operation-settled operation)
      (setf (e-board-sqlite-service-operation-request operation) request))
    :deferred))

(defconst e-board-sqlite-service--work-spec
  (e-work-spec-create
   :id "board-sql-operation" :execution 'cooperative
   :interactive-policy 'async :owner 'e-board-sqlite-service
   :runner #'e-board-sqlite-service--run))

(defun e-board-sqlite-service--start
    (service kind body &optional owner-key)
  "Start one request-scoped SERVICE KIND BODY operation."
  (unless (e-board-sqlite-service-p service)
    (signal 'wrong-type-argument (list 'e-board-sqlite-service-p service)))
  (let* ((operation
          (e-board-sqlite-service--operation-create
           :service service :kind kind
           :body (e-board-sqlite-service--detached-copy body)
           :owner-key (e-board-sqlite-service--detached-copy owner-key)))
         (work
          (e-work-prepare
           e-board-sqlite-service--work-spec operation
           :context
           (list :domain-ref (plist-get body :board-id)
                 :work-kind
                 (if (eq kind 'read) 'board-query 'board-mutation)))))
    (setf (e-board-sqlite-service-operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    work))

(cl-defun e-board-sqlite-service-board-create-start
    (service board-id trusted-principal &optional root)
  "Create BOARD-ID through SERVICE and immediately return request work.
TRUSTED-PRINCIPAL owns the durable Board root.  ROOT defaults to the minimum
canonical identity payload and is copied before crossing the SQL boundary."
  (unless (and (stringp board-id) (not (string-empty-p board-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p board-id)))
  (e-board-sqlite-service--start
   service 'write
   (list :op 'board-create :board-id board-id
         :trusted-principal
         (e-board-sqlite-service--detached-copy trusted-principal)
         :root
         (e-board-sqlite-service--detached-copy
          (or root (list :board-id board-id))))
   (cons 'board board-id)))

(cl-defun e-board-sqlite-service-append-route-start
    (service board-id &key session-id author requester-actor tags attributes to
             (mode 'inject) content reference source-input-key created-at)
  "Append and route one canonical input, returning request-scoped work."
  (unless (or (and (stringp board-id) (not (string-empty-p board-id)))
              (and (stringp session-id) (not (string-empty-p session-id))))
    (signal 'wrong-type-argument (list 'non-empty-string-p board-id)))
  (unless (and (stringp content) (not (string-empty-p content)))
    (user-error "Prompt must not be empty"))
  (unless source-input-key
    (signal 'e-board-sqlite-error
            (list "Board append requires a stable source identity" board-id)))
  (let* ((signature
         (list :author author :requester-actor requester-actor
               :tags tags :attributes attributes :to to :mode mode
               :content content :reference reference))
         (work
          (e-board-sqlite-service--start
           service 'write
           (list :op 'board-append-route :board-id board-id :session-id session-id
                 :author author :requester-actor requester-actor
                 :tags (e-board-sqlite-service--detached-copy tags)
                 :attributes
                 (e-board-sqlite-service--detached-copy attributes)
                 :to (e-board-sqlite-service--detached-copy to)
                 :mode mode
                 :content (e-board-sqlite-service--detached-copy content)
                 :reference (e-board-sqlite-service--detached-copy reference)
                 :source-input-key
                 (e-board-sqlite-service--detached-copy source-input-key)
                 :source-hash (e-board-sqlite-signature-hash signature)
                 :created-at created-at)
           (if board-id (cons 'board board-id) (cons 'session session-id)))))
    (e-work-on-settle
     work
     (lambda (settled)
       (e-board-sqlite-service--publish-committed-pickups settled service)))
    work))

(defun e-board-sqlite-service--record-append-start
    (service board-id record-kind source-kind source-key record-fields
             &optional generation)
  "Append RECORD-FIELDS, fenced to GENERATION when it is supplied."
  (unless (or (null generation)
              (and (integerp generation) (> generation 0)))
    (signal 'e-board-sqlite-error
            (list "Board record generation is invalid" generation)))
  (let ((signature (list :record-kind record-kind
                         :record-fields record-fields)))
    (e-board-sqlite-service--start
     service 'write
     (append
      (list :op 'board-record-append :board-id board-id)
      (when generation (list :generation generation))
      (list :record-kind record-kind :source-kind source-kind
            :source-key (e-board-sqlite-service--detached-copy source-key)
            :source-hash (e-board-sqlite-signature-hash signature)
            :record-fields
            (e-board-sqlite-service--detached-copy record-fields)))
     (cons 'board board-id))))

(cl-defun e-board-sqlite-service-record-append-start
    (service board-id record-kind source-kind source-key &rest record-fields)
  "Append one canonical non-routed RECORD-KIND with detached RECORD-FIELDS."
  (e-board-sqlite-service--record-append-start
   service board-id record-kind source-kind source-key record-fields))

(cl-defun e-board-sqlite-service-orchestration-fact-start
    (service board-id fact &key author generation)
  "Validate and append one orchestration FACT to BOARD-ID."
  (let ((fields
         (e-board-orchestration-fact-record-fields fact :author author)))
    (e-board-sqlite-service--record-append-start
     service board-id 'fact 'fact (plist-get fields :source-key)
     (cl-loop for (key value) on fields by #'cddr
              unless (eq key :source-key)
              append (list key value))
     generation)))

(defun e-board-sqlite-service-orchestration-run-start
    (service board-id run-id &optional limit generation)
  "Read RUN-ID's bounded facts from BOARD-ID.
When GENERATION is non-nil, require that it is still the current generation."
  (unless (or (null generation)
              (and (integerp generation) (> generation 0)))
    (signal 'e-board-sqlite-error
            (list "Board run generation is invalid" generation)))
  (e-board-sqlite-service--start
   service 'read
   (append (list :op 'board-orchestration-run :board-id board-id
                 :run-id run-id :limit (or limit 256))
           (when generation (list :generation generation)))))

(defun e-board-sqlite-service-orchestration-runs-start
    (service board-id &optional limit)
  "Read facts for the newest bounded durable run set from BOARD-ID."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-orchestration-runs :board-id board-id
         :limit (or limit 32))))

(defun e-board-sqlite-service-orchestration-active-runs-start
    (service board-id &optional limit now)
  "Read BOARD-ID's bounded priority page and exact active-run count."
  (let ((limit (or limit e-board-orchestration-run-set-default-record-limit)))
    (unless (and (integerp limit) (> limit 0)
                 (<= limit e-board-orchestration-run-set-max-record-limit))
      (signal 'e-board-sqlite-error
              (list "Board active-run page limit is out of bounds" limit)))
    (e-board-sqlite-service--start
     service 'read
     (list :op 'board-orchestration-active-runs :board-id board-id
           :limit limit :now now))))

(cl-defun e-board-sqlite-service-orchestration-run-index-page-start
    (service board-id &key cursor active-only include-continuation-ref limit)
  "Read one bounded stable current-generation run-index page from BOARD-ID.

CURSOR is returned unchanged by the previous page.  ACTIVE-ONLY filters out
completed rows while preserving the same manifest-position cursor contract."
  (let ((limit (or limit e-board-orchestration-run-set-default-record-limit)))
    (unless (and (integerp limit) (> limit 0)
                 (<= limit e-board-orchestration-run-set-max-record-limit))
      (signal 'e-board-sqlite-error
              (list "Board run-index page limit is out of bounds" limit)))
    (unless (or (null active-only) (eq active-only t))
      (signal 'e-board-sqlite-error
              (list "Board run-index active filter is invalid" active-only)))
    (unless (or (null include-continuation-ref)
                (eq include-continuation-ref t))
      (signal 'e-board-sqlite-error
              (list "Board run-index continuation reference flag is invalid"
                    include-continuation-ref)))
    (e-board-sqlite-service--start
     service 'read
     (list :op 'board-orchestration-run-index-page :board-id board-id
           :cursor (e-board-sqlite-service--detached-copy cursor)
           :active-only active-only
           :include-continuation-ref include-continuation-ref
           :limit limit))))

(cl-defun e-board-sqlite-service-activity-page-start
    (service board-id &key after limit byte-limit participant-id run-id)
  "Read one bounded consumer-shaped Board participant/activity page.

The request returns immediately with an `e-work' handle.  COUNT and BYTE-LIMIT
are validated before admission and are repeated by the worker at the SQL
boundary.  The worker joins or reduces bounded participant, session, and fact
sets; it never issues a read per participant.  Neither this service nor its
operation retains the returned page after the consumer's work handle settles."
  (let ((limit (or limit e-board-sqlite-activity-page-count-limit))
        (byte-limit (or byte-limit e-board-sqlite-activity-page-byte-limit)))
    (unless (and (integerp limit) (> limit 0)
                 (<= limit e-board-sqlite-activity-page-count-limit))
      (signal 'e-board-sqlite-error
              (list "Board activity participant count is out of bounds"
                    limit e-board-sqlite-activity-page-count-limit)))
    (unless (and (integerp byte-limit) (> byte-limit 0)
                 (<= byte-limit e-board-sqlite-activity-page-byte-limit))
      (signal 'e-board-sqlite-error
              (list "Board activity page byte bound is out of bounds"
                    byte-limit e-board-sqlite-activity-page-byte-limit)))
    (unless (or (null after)
                (and (stringp after) (not (string-empty-p after))))
      (signal 'e-board-sqlite-error
              (list "Board activity cursor is invalid" after)))
    (unless (or (null participant-id)
                (and (stringp participant-id)
                     (not (string-empty-p participant-id))))
      (signal 'e-board-sqlite-error
              (list "Board activity participant identity is invalid"
                    participant-id)))
    (unless (or (null run-id)
                (and (stringp run-id) (not (string-empty-p run-id))))
      (signal 'e-board-sqlite-error
              (list "Board activity run identity is invalid" run-id)))
    (e-board-sqlite-service--start
     service 'read
     (list :op 'board-activity-page :board-id board-id
           :after (e-board-sqlite-service--detached-copy (or after ""))
           :limit limit :byte-limit byte-limit
           :participant-id
           (e-board-sqlite-service--detached-copy participant-id)
           :run-id (e-board-sqlite-service--detached-copy run-id)))))

(defun e-board-sqlite-service-activity-detail-start
    (service board-id record-id)
  "Read one Board-authorized summary detail by RECORD-ID.
The worker validates the current Board generation, participant association,
record kind, and opaque source identity before returning a detached mapping."
  (unless (and (stringp record-id) (not (string-empty-p record-id)))
    (signal 'e-board-sqlite-error
            (list "Board activity record identity is invalid")))
  (e-board-sqlite-service--start
   service 'read (list :op 'board-activity-detail
                       :board-id board-id :record-id record-id)))

(defun e-board-sqlite-service-pickup-page-start
    (service board-id generation participant-id &optional limit)
  "Read PARTICIPANT-ID's bounded unresolved pickup page."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-pickup-list :board-id board-id :generation generation
         :participant-id participant-id :limit (or limit 16))))

(defun e-board-sqlite-service-board-get-start (service board-id)
  "Read BOARD-ID's detached scalar coordination row."
  (e-board-sqlite-service--start
   service 'read (list :op 'board-get :board-id board-id)))

(defun e-board-sqlite-service-board-owner-resolve-start (service board-id)
  "Read BOARD-ID's bounded current owner candidates.

The result is an untouched detached candidate set.  `e-chat-service' owns the
semantic exact-one, identity, and live-binding validation at its application
boundary; this SQL service only supplies the fixed-cost Board query."
  (unless (and (stringp board-id) (not (string-empty-p board-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p board-id)))
  (e-board-sqlite-service--start
   service 'read (list :op 'board-owner-resolve :board-id board-id)))

(cl-defun e-board-sqlite-service-admit-session-input-start
    (service session-id board-id principal records query-delta participant
             &key author requester-actor tags attributes to (mode 'inject)
             content reference source-input-key)
  "Atomically admit a new chat session and its first routed input."
  (let ((signature
         (list :author author :requester-actor requester-actor
               :tags tags :attributes attributes :to to :mode mode
               :content content :reference reference)))
    (e-board-sqlite-service--start
     service 'write
     (list :op 'chat-session-input-admit :session-id session-id
           :board-id board-id :principal principal
           :records
           (vconcat (e-board-sqlite-service--detached-copy records))
           :query-delta (e-board-sqlite-service--detached-copy query-delta)
           :participant (e-board-sqlite-service--detached-copy participant)
           :author (e-board-sqlite-service--detached-copy author)
           :requester-actor
           (e-board-sqlite-service--detached-copy requester-actor)
           :tags (e-board-sqlite-service--detached-copy tags)
           :attributes (e-board-sqlite-service--detached-copy attributes)
           :to (e-board-sqlite-service--detached-copy to) :mode mode
           :content (e-board-sqlite-service--detached-copy content)
           :reference (e-board-sqlite-service--detached-copy reference)
           :source-input-key
           (e-board-sqlite-service--detached-copy source-input-key)
           :source-hash (e-board-sqlite-signature-hash signature))
     (cons 'session session-id))))

(defun e-board-sqlite-service-admit-session-owner-start
    (service session-id board-id principal records query-delta participant)
  "Atomically admit a chat owner without creating a synthetic input row."
  (e-board-sqlite-service--start
   service 'write
   (list :op 'chat-session-owner-admit :session-id session-id
         :board-id board-id :principal principal
         :records (vconcat (e-board-sqlite-service--detached-copy records))
         :query-delta (e-board-sqlite-service--detached-copy query-delta)
         :participant (e-board-sqlite-service--detached-copy participant))
   (cons 'session session-id)))

(cl-defun e-board-sqlite-service-admit-participant-start
    (service session-id board-id records query-delta participant &key pickup)
  "Atomically admit SESSION-ID as BOARD-ID PARTICIPANT.

The worker resolves the Board's current generation inside the transaction;
the caller supplies no reconstructed Board or locally retained generation."
  (e-board-sqlite-service--start
   service 'write
   (append
    (list :op 'session-board-participant-admit
          :session-id session-id :board-id board-id
          :records (vconcat (e-board-sqlite-service--detached-copy records))
          :query-delta (e-board-sqlite-service--detached-copy query-delta)
          :participant (e-board-sqlite-service--detached-copy participant))
    (when pickup
      (list :pickup (e-board-sqlite-service--detached-copy pickup))))
   (cons 'session session-id)))

(defun e-board-sqlite-service-transition-pickup-start
    (service board-id delivery-id transition &optional data)
  "Start DELIVERY-ID TRANSITION and return canonical settlement work."
  (e-board-sqlite-service--start
   service 'write
   (list :op 'board-pickup-transition :board-id board-id
         :delivery-id (e-board-sqlite-service--detached-copy delivery-id)
         :transition transition
         :data (e-board-sqlite-service--detached-copy data))
   (cons 'board board-id)))

(cl-defun e-board-sqlite-service-record-page-start
    (service board-id &key generation after limit selector through)
  "Read one bounded canonical page and its SQLite snapshot cursor."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-record-page :board-id board-id :generation generation
         :after after :limit limit
         :selector (e-board-sqlite-service--detached-copy selector)
         :through through)))

(cl-defun e-board-sqlite-service-visible-window-start
    (service board-id &key generation limit)
  "Read the recent visible Board window and its SQLite change boundary."
  (e-board-sqlite-service--start
   service 'read
   (list :op 'board-visible-window :board-id board-id
         :generation generation :limit (or limit 64))))

(provide 'e-board-sqlite-service)

;;; e-board-sqlite-service.el ends here

;;; e-session.el --- Public session facade and application service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `e-session' is the stable application boundary.  The aggregate owns live
;; semantic state, the codec owns JSONL value mapping, the catalog owns bounded
;; checkpoint/index projections, and storage owns all physical I/O and writer
;; lifecycle.  This module composes those owners and is the only place where a
;; semantic mutation is paired with its durable record.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'e-request)
(require 'e-session-aggregate)
(require 'e-session-codec)
(require 'e-session-catalog)
(require 'e-session-board-policy)
(require 'e-session-identity)
(require 'e-session-metadata)
(require 'e-session-provider-anchor)
(require 'e-session-storage)
(require 'e-session-sqlite)
(require 'e-session-tool-continuity)

(defvar e-session--load-in-progress nil
  "Non-nil while the application service is replaying a session journal.")

(defvar e-session--commit-in-progress (make-hash-table :test 'equal)
  "Session ids with an isolated mutation awaiting durable commit.

The live aggregate remains at its previously committed value while a key is
present.  Facade operations that need a loaded session fail explicitly at the
barrier; direct read-only aggregate projections continue to see committed-old
state.")

(defun e-session--timestamp (&optional time)
  "Return TIME as the compact UTC timestamp used by session records."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun e-session--persistent-p (store)
  "Return non-nil when STORE has a durable storage adapter."
  (e-session-storage-persistent-p store))

(defun e-session-persistent-p (store)
  "Return non-nil when STORE has a durable session adapter.

This is the application-facing health query; consumers do not need to inspect
the storage owner's state representation."
  (e-session--persistent-p store))

(defun e-session--profile-call (event options thunk)
  "Measure EVENT around THUNK when the optional developer profiler is active."
  (if (and (fboundp 'e-dev-profile-enabled-p)
           (fboundp 'e-dev-profile-measure-thunk)
           (e-dev-profile-enabled-p))
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-session--index-entry-session (store entry)
  "Return an unloaded semantic session stub from catalog ENTRY."
  (let ((id (plist-get entry :id))
        (association (e-session-aggregate-projected-board-association entry)))
    (when id
      (let ((session
             (list :id id
                   :metadata
                   (e-session-metadata-normalize-for-replay
                    (plist-get entry :metadata))
                   :session-events nil :messages nil :activity-events nil
                   :branch-summaries nil :current-branch nil :compactions nil
                   :provider-anchors nil :process-reports nil
                   :context-generations nil :context-promotions nil
                   :context-curation-packages nil :turn-options nil
                   :created-at (plist-get entry :created-at)
                   :updated-at (plist-get entry :updated-at)
                   :updated-seq (or (plist-get entry :updated-seq) 0)
                   :name (plist-get entry :name)
                   :summary (plist-get entry :summary)
                   :message-count (or (plist-get entry :message-count) 0)
                   :last-message-at (plist-get entry :last-message-at)
                   :latest-assistant-marker
                   (plist-get entry :latest-assistant-marker)
                   :board-id (plist-get entry :board-id)
                   :principal (plist-get entry :principal)
                   :file (or (plist-get entry :file)
                             (e-session-storage-session-reference store id))
                   :loaded nil)))
        ;; A present malformed association is significant: retaining the
        ;; bounded marker makes root pickers fail closed instead of treating a
        ;; partially populated legacy flat projection as an ordinary root.
        ;; The canonical all-null flat projection remains absent and therefore
        ;; is intentionally not installed on the stub.
        (when association
          (plist-put session :board-session-state association))
        (e-session-aggregate-initialize-list-state session)))))

(defun e-session--normalize-index-json-entry (entry)
  "Return physical index ENTRY in semantic detached form."
  (when (e-session-aggregate-keyword-plist-shape-p entry)
    (let ((result (copy-tree entry)))
      (let ((tail result))
        (while tail
          (let* ((key (pop tail))
                 (value (pop tail)))
            (plist-put result key
                       (if (eq key :board-state)
                           ;; Keep physical JSON null distinct from an empty
                           ;; object until the aggregate classifies the index
                           ;; projection.  Both parse as nil under plist object
                           ;; semantics, while the historical index contract
                           ;; treats null as absence and {} as malformed state.
                           (if (e-session-codec-json-null-p value)
                               value
                             (e-session-codec-board-association-from-json
                              value))
                         (e-session-codec-index-value-from-json value)))))
      result))))

(defun e-session--index-entries (value)
  "Return normalized catalog entries from physical index VALUE."
  (cond
   ((and (proper-list-p value)
         (seq-some (lambda (item)
                     (and (e-session-aggregate-keyword-plist-shape-p item)
                          (plist-member item :id)))
                   value))
    (delq nil (mapcar #'e-session--normalize-index-json-entry value)))
   ((e-session-aggregate-keyword-plist-shape-p value)
    (let (entries)
      (while value
        (let* ((key (pop value))
               (entry (pop value))
               (id (cond
                    ((keywordp key) (string-remove-prefix ":" (symbol-name key)))
                    ((symbolp key) (symbol-name key))
                    ((stringp key) key))))
          (when (e-session-aggregate-keyword-plist-shape-p entry)
            (let ((entry (e-session--normalize-index-json-entry entry)))
              (unless (plist-get entry :id)
                (plist-put entry :id id))
              (push entry entries)))))
      (nreverse entries)))))

(defun e-session--load-index (store)
  "Load detached session stubs from STORE's physical index."
  (when (e-session--persistent-p store)
    (when-let ((value (e-session-storage-read-catalog-projection store)))
      (let ((entries (e-session--index-entries value)))
        (when entries
          (e-session-aggregate-reset store)
          (dolist (entry entries)
            (when-let ((session (e-session--index-entry-session store entry)))
              (e-session-aggregate-install-index-session store session))))
        (and entries t)))))

(defun e-session--reconcile-journal-roots (store &optional only-missing)
  "Add session stubs for journals missing from STORE's catalog projection."
  (dolist (session-id (e-session-storage-session-ids store))
    (when (or (not only-missing)
              (not (e-session-aggregate-session-present-p store session-id)))
      (unless (e-session-aggregate-session-present-p store session-id)
        (condition-case nil
            (let ((root (car (e-session-storage-read-session-records
                              store session-id))))
              (when (equal (plist-get root :type) "session")
                (when-let ((session
                           (e-session--index-entry-session
                            store
                            (list :id session-id
                                  :created-at (or (plist-get root :created-at)
                                                  (plist-get root :timestamp))
                                  :updated-at (or (plist-get root :updated-at)
                                                  (plist-get root :timestamp))
                                  :message-count 0))))
                  (e-session-aggregate-install-index-session store session))))
          (file-error nil)
          (json-parse-error nil))))))

(defun e-session--checkpoint-projection-operation (store)
  "Return an operation that projects STORE's latest SESSION-ID checkpoint."
  (lambda (session-id)
    (e-session--checkpoint-json store session-id)))

(defun e-session--refresh-projections (store)
  "Compose STORE's index and deferred checkpoint projection operation."
  (let (index-projection)
    (dolist (session (e-session-aggregate-session-values store))
      (let* ((session-id (plist-get session :id))
             (file (e-session-storage-session-reference store session-id))
             (index-entry (e-session-catalog-index-entry session file)))
        (push index-entry index-projection)))
    (list (e-session-catalog-sort-index-entries (nreverse index-projection))
          (e-session--checkpoint-projection-operation store))))

(defun e-session--write-index (store)
  "Persist the composed index projection for STORE."
  (e-session--profile-call
   'session.write-index nil
   (lambda ()
     (pcase-let ((`(,index-projection ,checkpoint-projection-operation)
                  (e-session--refresh-projections store)))
       (e-session-storage-publish-projections
        store index-projection checkpoint-projection-operation)))))

(defun e-session--write-index-after-primary (store)
  "Publish STORE's rebuildable projections after primary acknowledgement.

Once the authoritative session record has committed, a derived publication
failure cannot turn that mutation back into a reported failure.  Storage keeps
the dirty owner set, retained projection operation, and bounded diagnostic for
the next mutation or explicit finalize barrier."
  (condition-case err
      (progn (e-session--write-index store) t)
    (error
     (e-session-storage-note-projection-error store err)
     nil)))

(defun e-session--root-record-from-store (store session-id)
  "Return the current semantic root record for SESSION-ID."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (root (car (e-session-aggregate-session-events store session-id))))
    (list :type "session" :session-id session-id
          :id (plist-get root :id)
          :timestamp (or (plist-get root :created-at)
                         (plist-get session :created-at))
          :created-at (plist-get session :created-at)
          :updated-at (plist-get session :updated-at)
          :metadata (copy-tree (plist-get session :metadata))
          :name (plist-get session :name)
          :turn-options (copy-tree (plist-get session :turn-options))
          :current-branch (plist-get session :current-branch)
          :board-output-sequence (or (plist-get session :board-output-sequence) 0)
          :board-activity-sequence
          (or (plist-get session :board-activity-sequence) 0))))

(defun e-session--persist-record (store session-id record &optional write-index)
  "Preflight and append semantic RECORD, then optionally refresh the index."
  (when (e-session--persistent-p store)
    (let ((prepared (e-session-storage-prepare-mutation
                     store session-id record)))
      (e-session-storage-commit-mutation store session-id prepared)))
  (when write-index
    (e-session--write-index store))
  record)

(defun e-session--call-with-commit-barrier (store session-id operation)
  "Call OPERATION while SESSION-ID rejects dependent facade work."
  (let ((key (cons store session-id)))
    (when (gethash key e-session--commit-in-progress)
      (signal 'e-session-persistence-unavailable
              (list "Session commit is in progress" session-id)))
    (puthash key t e-session--commit-in-progress)
    (unwind-protect (funcall operation)
      (remhash key e-session--commit-in-progress))))

(cl-defun e-session--commit-session-mutation
    (store session-id mutate make-record &key before-commit write-index)
  "Pair one semantic session MUTATE with its durable record.

MUTATE receives an aggregate-owned isolated stage and returns the semantic
result.  MAKE-RECORD receives that stage and result and returns its one durable
record, or nil for a semantic no-op.  BEFORE-COMMIT, when non-nil, receives the
same arguments after record preflight and may establish an owner-specific
execution fence.  SQLite stages and commits before live publication.  Legacy
stores retain their historical mutate-then-persist ordering.  This is a narrow
session application boundary, not a generic transaction builder."
  (if (not (e-session-storage-sqlite-p store))
      (let* ((result (funcall mutate store))
             (record (funcall make-record store result)))
        (when record
          (when before-commit
            (funcall before-commit store result))
          (e-session--persist-record store session-id record write-index))
        result)
    (e-session--call-with-commit-barrier
     store session-id
     (lambda ()
       (let* ((source-present
               (e-session-aggregate-session-present-p store session-id))
              (stage
               (e-session-aggregate-stage-session-mutation
                store (and source-present session-id)))
              (result (funcall mutate stage))
              (record (funcall make-record stage result))
              (prepared
               (and record
                    (e-session-storage-prepare-mutation
                     store session-id record))))
         (when record
           (when before-commit
             (funcall before-commit stage result))
           (e-session-storage-commit-mutation store session-id prepared)
           ;; The worker has acknowledged the durable record.  Only now may a
           ;; model-facing callback observe the new live value.
           (e-session-aggregate-publish-staged-session store stage session-id)
           (when write-index
             (e-session--write-index-after-primary store)))
         result)))))

(cl-defun e-session--commit-entry-mutation
    (store session-id mutate &key before-commit write-index)
  "Commit MUTATE's one entry record before publishing SESSION-ID."
  (e-session--commit-session-mutation
   store session-id mutate
   (lambda (_stage entry)
     (and entry (e-session-codec-record-for-entry session-id entry)))
   :before-commit before-commit :write-index write-index))

(cl-defun e-session--commit-session-event-mutation
    (store session-id mutate &key write-index)
  "Commit MUTATE's newest session event before publishing SESSION-ID."
  (e-session--commit-session-mutation
   store session-id mutate
   (lambda (stage _result)
     (e-session-codec-record-for-entry
      session-id (e-session--latest-session-event stage session-id)))
   :write-index write-index))

(defun e-session--persist-entry (store session-id entry &optional write-index)
  "Persist semantic ENTRY and optionally update the catalog."
  (e-session--persist-record
   store session-id
   (e-session-codec-record-for-entry session-id entry)
   write-index))

(defun e-session--latest-session-event (store session-id)
  "Return the newest semantic session event for SESSION-ID."
  (car (last (e-session-aggregate-session-events store session-id))))

(defun e-session--board-state-record (store session-id)
  "Return the aggregate's current board association record."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (state (e-session-aggregate-board-association session)))
    (when state
      (list :type "board-session-state" :session-id session-id
            :timestamp (e-session--timestamp)
            :board-state state
            :board-id (plist-get state :board-id)
            :principal (plist-get state :principal)
            :association-role (plist-get state :association-role)
            :board-output-sequence
            (or (plist-get session :board-output-sequence) 0)
            :board-activity-sequence
            (or (plist-get session :board-activity-sequence) 0)))))

(defun e-session--persist-board-state (store session-id)
  "Persist the aggregate's current board association projection."
  (let ((record (e-session--board-state-record store session-id)))
    (when record
      (e-session--persist-record store session-id record))))

(defun e-session--checkpoint-json (store session-id)
  "Return catalog-produced exact checkpoint value for SESSION-ID."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (offset (plist-get (e-session-storage-session-header store session-id)
                            :byte-size)))
    (e-session-catalog-checkpoint-value
     session (e-session-aggregate-board-messages store session-id) offset)))

(defun e-session--write-session-checkpoint-now (store session-id)
  "Atomically persist SESSION-ID's current bounded checkpoint."
  (e-session-storage-persist-resume-checkpoint
   store session-id (e-session--checkpoint-json store session-id)))

(defun e-session--read-checkpoint (store session-id)
  "Read and validate SESSION-ID's physical resume checkpoint."
  (unless (e-session-storage-resume-checkpoint-present-p store session-id)
    (signal 'e-session-checkpoint-missing (list session-id)))
  (let ((file (plist-get (e-session-storage-session-header store session-id)
                         :reference)))
    (let ((checkpoint
           (condition-case err
               (e-session-storage-read-resume-checkpoint store session-id)
             ((file-error file-missing json-parse-error)
              (signal 'e-session-checkpoint-invalid
                      (list session-id file err))))))
      (unless (e-session-catalog-checkpoint-valid-p checkpoint session-id)
        (signal 'e-session-checkpoint-invalid (list session-id file)))
      checkpoint)))

(defun e-session--apply-physical-record (store record)
  "Decode physical RECORD and apply its semantic value to STORE."
  (e-session-aggregate-apply-record
   store (e-session-codec-decode-record record)))

(defun e-session--begin-checkpoint-replay (store session-id checkpoint)
  "Install CHECKPOINT's semantic records as replay prefix for SESSION-ID."
  (e-session-aggregate-reset-session store session-id)
  (let ((e-session--load-in-progress t))
    (dolist (record (plist-get checkpoint :records))
      (e-session--apply-physical-record store record)))
  (unless (e-session-aggregate-session-present-p store session-id)
    (signal 'e-session-checkpoint-invalid
            (list session-id "Checkpoint has no session root"))))

(defun e-session--finish-replay (store session-id)
  "Finalize replayed SESSION-ID and return its loaded semantic value."
  (e-session-aggregate-finalize-replayed-session
   store (e-session-aggregate-get-live store session-id)))

(defun e-session-load-session (store session-id)
  "Load SESSION-ID's checkpoint and journal suffix from STORE."
  (unless (e-session--persistent-p store)
    (signal 'e-session-missing (list session-id)))
  (let* ((header (e-session-storage-session-header store session-id))
         (checkpoint (e-session--read-checkpoint store session-id))
         (offset (plist-get checkpoint :journal-byte-offset))
         (size (plist-get header :byte-size)))
    (unless (plist-get header :present)
      (signal 'e-session-missing (list session-id)))
    (when (> offset size)
      (signal 'e-session-checkpoint-invalid
              (list session-id "Checkpoint offset exceeds journal size"
                    offset size)))
    (e-session--begin-checkpoint-replay store session-id checkpoint)
    (let ((e-session--load-in-progress t))
      (dolist (record (e-session-storage-read-session-records
                       store session-id offset))
        (e-session--apply-physical-record store record)))
    (e-session--finish-replay store session-id)))

(cl-defun e-session-load-session-start
    (store session-id &key on-done on-error on-progress chunk-bytes)
  "Start cooperative indexed SQLite replay for SESSION-ID."
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-session-missing (list session-id)))
  (e-session-sqlite-load-session-start
   store session-id :on-done on-done :on-error on-error
   :on-progress on-progress :page-size
   (max 1 (min 1024 (or chunk-bytes 256)))))

(defun e-session--load-session-journal-fully (store session-id)
  "Replay all journal records for explicit checkpoint migration."
  (unless (plist-get (e-session-storage-session-header store session-id)
                     :present)
    (signal 'e-session-missing (list session-id)))
  (e-session-aggregate-reset-session store session-id)
  (let ((e-session--load-in-progress t))
    (dolist (record (e-session-storage-read-session-records store session-id))
      (e-session--apply-physical-record store record)))
  (e-session--finish-replay store session-id))

(defun e-session-migrate-session-checkpoint (store session-id)
  "Migrate SESSION-ID's complete journal to its bounded checkpoint."
  (e-session--load-session-journal-fully store session-id)
  (e-session--write-session-checkpoint-now store session-id))

(defun e-session-load (store)
  "Replay every durable journal in STORE.

The explicit `e-session-load-session' operation remains checkpoint-only.  The
store-construction path accepts a missing checkpoint as an offline migration
case so a freshly created direct JSONL store remains reopenable."
  (when (e-session--persistent-p store)
    (e-session-aggregate-reset store)
    (dolist (session-id (e-session-storage-session-ids store))
      (condition-case err
          (e-session-load-session store session-id)
        (e-session-checkpoint-missing
         (e-session--load-session-journal-fully store session-id))
        (error (signal (car err) (cdr err))))))
  store)

(cl-defun e-session-persistent-index-store-create
    (&optional directory &key write-mode)
  "Open the current SQLite session STORE without eager transcript replay.

WRITE-MODE is retained only to diagnose removed JSONL writer configurations;
nil and `sqlite' both select the sole current physical adapter."
  (unless (memq write-mode '(nil sqlite))
    (signal 'e-session-storage-migration-required
            (list "Legacy session write modes are retired; migrate offline"
                  write-mode)))
  (e-session-sqlite-store-create directory))

(cl-defun e-session-persistent-store-create (&optional directory &key write-mode)
  "Open the current SQLite session STORE and eagerly restore its sessions.

This historical facade name no longer selects or falls back to JSONL."
  (unless (memq write-mode '(nil sqlite))
    (signal 'e-session-storage-migration-required
            (list "Legacy session write modes are retired; migrate offline"
                  write-mode)))
  (e-session-sqlite-store-create directory :load-all t))

(defun e-session--ensure-loaded (store session-id)
  "Return loaded SESSION-ID, loading its checkpoint suffix on demand."
  (when (gethash (cons store session-id) e-session--commit-in-progress)
    (signal 'e-session-persistence-unavailable
            (list "Session commit is in progress" session-id)))
  (let ((session (e-session-aggregate-peek-session store session-id)))
    (if (plist-get session :loaded)
        session
      (condition-case err
          (e-session-load-session store session-id)
        ;; An index store may be opened between the first journal flush and
        ;; the asynchronous checkpoint barrier.  Ordinary facade reads retain
        ;; the historical reopen behavior by replaying that journal in full;
        ;; the explicit `e-session-load-session' API remains strict and still
        ;; reports a missing resume checkpoint to its caller.
        (e-session-checkpoint-missing
         (e-session--load-session-journal-fully store session-id))
        (error (signal (car err) (cdr err)))))))

(cl-defun e-session-create (store &key id metadata defer-persistence)
  "Create SESSION in STORE and publish its root unless deferred."
  (setq id (or id (e-session-generate-id)))
  (if (and (e-session-storage-sqlite-p store) (not defer-persistence))
      (e-session--commit-session-mutation
       store id
       (lambda (stage)
         (e-session-aggregate-create stage :id id :metadata metadata))
       (lambda (stage _session)
         (e-session--root-record-from-store stage id))
       :write-index t)
    (let ((session (e-session-aggregate-create
                    store :id id :metadata metadata
                    :defer-persistence defer-persistence)))
      (if defer-persistence
          session
        (condition-case err
            (progn
              (e-session--persist-record
               store (plist-get session :id)
               (e-session--root-record-from-store
                store (plist-get session :id)) t)
              session)
          (error
           (e-session-aggregate-abort-created store (plist-get session :id))
           (signal (car err) (cdr err))))))))

(cl-defun e-session-create-board-admission
    (store &key id metadata principal board-id association-role routing-policy)
  "Prepare a board-backed session admission without publishing it."
  (let* ((session (e-session-aggregate-create-board-admission
                   store :id (or id (e-session-generate-id)) :metadata metadata
                   :principal principal :board-id board-id
                   :association-role association-role
                   :routing-policy routing-policy))
         (session-id (plist-get session :id))
         (records (mapcar #'e-session-codec-record-for-json
                          (plist-get session :admission-records))))
    (when (e-session--persistent-p store)
      (e-session-storage-validate-admission-for-store store session-id records)
      (dolist (record records) (json-encode record)))
    (plist-put session :admission-records records)
    session))

(defun e-session-commit-board-admission (store session-id)
  "Publish a prepared board admission as one storage transaction."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (records (plist-get session :admission-records)))
    (if (not (e-session-storage-sqlite-p store))
        (condition-case err
            (pcase-let
                ((`(,index-projection ,checkpoint-projection-operation)
                  (e-session--refresh-projections store)))
              (e-session-storage-publish-admission
               store session-id records nil)
              (e-session-aggregate-commit-board-admission store session-id)
              (e-session-storage-publish-projections
               store index-projection checkpoint-projection-operation)
              session)
          (error
           (e-session-storage-abort-session store session-id)
           (e-session-aggregate-abort-created store session-id)
           (signal (car err) (cdr err))))
      (e-session--call-with-commit-barrier
       store session-id
       (lambda ()
         (condition-case err
             (e-session-storage-publish-admission
              store session-id records nil)
           (error
            (e-session-storage-abort-session store session-id)
            (e-session-aggregate-abort-created store session-id)
            (signal (car err) (cdr err))))
         ;; Primary admission is now authoritative.  Removing the private
         ;; reservation and rebuilding projections cannot turn it into a
         ;; reported uncommitted failure.
         (e-session-aggregate-commit-board-admission store session-id)
         (e-session--write-index-after-primary store)
         session)))))

(defun e-session-abort-created (store session-id)
  "Abort a not-yet-admitted session and its owned storage work."
  (let ((operation
         (lambda ()
           (e-session-storage-abort-session store session-id)
           (e-session-aggregate-abort-created store session-id)
           ;; Preserve an existing shared derived-index obligation instead of
           ;; appending a second command while rolling back one reservation.
           (when (and
                  (e-session--persistent-p store)
                  (not (e-session-storage-admission-controller-enabled-p
                        store))
                  (not (plist-get
                        (e-session-storage-durability-status store)
                        :index-write-pending)))
             (e-session--write-index store))
           t)))
    (if (not (e-session-storage-sqlite-p store))
        (funcall operation)
      (e-session--call-with-commit-barrier store session-id operation))))

(defun e-session-delete (store session-id)
  "Delete SESSION-ID and all of its private durable SQLite state."
  (unless (e-session-storage-sqlite-p store)
    (signal 'e-session-storage-error
            (list "Session deletion is available on SQLite stores")))
  (e-session--call-with-commit-barrier
   store session-id
   (lambda ()
     ;; Deletion is already storage-first.  The admission barrier keeps a
     ;; reentrant append from being ordered between the delete ACK and the live
     ;; aggregate removal.
     (e-session-storage-sqlite-delete store session-id)
     (e-session-aggregate-reset-session store session-id)
     (e-session--write-index-after-primary store)
     t)))

(defun e-session-append-message (store session-id message)
  "Append MESSAGE to SESSION-ID and persist its semantic entry."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-message aggregate session-id message))
   :before-commit
   (lambda (_aggregate entry)
     (e-session-tool-continuity-admit-message store session-id message entry))
   :write-index t))

(defun e-session-set-message-display (store session-id message-id display)
  "Set DISPLAY on one message and persist its display disposition."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-set-message-display
      aggregate session-id message-id display))
   (lambda (_aggregate message)
     (when message
       (list :type "message-display" :session-id session-id
             :timestamp (e-session--timestamp) :id message-id
             :display (and display (symbol-name display)))))
   :write-index t))

(cl-defun e-session-append-activity-event
    (store session-id turn-id event-type payload &key (write-index t)
           checkpoint-retain)
  "Append durable activity EVENT-TYPE and optionally refresh the index."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-activity-event
      aggregate session-id turn-id event-type payload
      :write-index write-index :checkpoint-retain checkpoint-retain))
   :before-commit
   (lambda (_aggregate entry)
     ;; This owner classification is the execution fence.  If the subsequent
     ;; session record fails, restart classification is conservative while
     ;; live session state remains old.
      (e-session-tool-continuity-record-activity
      store session-id turn-id event-type payload entry))
   :write-index write-index))

(cl-defun e-session-append-context-curation-response
    (store session-id turn-id response-entry-id &key (write-index t))
  "Append the audit-only context curation response control entry."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-context-curation-response
      aggregate session-id turn-id response-entry-id
      :write-index write-index))
   :write-index write-index))

(defun e-session-append-process-report (store session-id report)
  "Append an out-of-band process REPORT."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-process-report aggregate session-id report))
   :write-index t))

(cl-defun e-session-append-branch-summary
    (store session-id branch-id summary &key metadata)
  "Append BRANCH-ID SUMMARY."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-branch-summary
      aggregate session-id branch-id summary :metadata metadata))
   :write-index t))

(cl-defun e-session-append-compaction
    (store session-id summary &key branch-id range first-kept-entry-id
           tokens-before tokens-kept metadata)
  "Append compaction SUMMARY."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-compaction
      aggregate session-id summary :branch-id branch-id :range range
      :first-kept-entry-id first-kept-entry-id
      :tokens-before tokens-before :tokens-kept tokens-kept
      :metadata metadata))
   :write-index t))

(cl-defun e-session-append-provider-anchor
    (store session-id provider-id &key model covered-entry-id fingerprints metadata)
  "Append opaque PROVIDER-ID anchor state."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-provider-anchor
      aggregate session-id provider-id :model model
      :covered-entry-id covered-entry-id :fingerprints fingerprints
      :metadata metadata))
   :write-index t))

(cl-defun e-session-append-context-generation
    (store session-id generation &key (write-index t))
  "Append semantic context GENERATION."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-context-generation
      aggregate session-id generation :write-index write-index))
   :write-index write-index))

(cl-defun e-session-append-context-curation-package
    (store session-id package &key (write-index t))
  "Append one atomic semantic context curation PACKAGE."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-context-curation-package
      aggregate session-id package :write-index write-index))
   (lambda (_aggregate result) (plist-get result :record))
   :write-index write-index))

(defun e-session-set-metadata (store session-id metadata)
  "Replace durable session METADATA."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-event-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-set-metadata aggregate session-id metadata))
   :write-index t)
  metadata)

(defun e-session-set-session-config (store session-id config)
  "Merge durable session CONFIG."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-event-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-set-session-config aggregate session-id config))
   :write-index t))

(defun e-session-set-context-references (store session-id owner references)
  "Set current-state REFERENCES for OWNER."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-event-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-set-context-references
      aggregate session-id owner references))
   :write-index t))

(defun e-session-set-context-reference (store session-id key reference)
  "Set one durable current-state REFERENCE."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-event-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-set-context-reference
      aggregate session-id key reference))
   :write-index t))

(cl-defun e-session-set-capability-state
    (store session-id capability-id state &key version)
  "Set durable CAPABILITY-ID STATE."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-event-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-set-capability-state
      aggregate session-id capability-id state :version version))
   :write-index t))

(defun e-session-set-turn-options (store session-id options)
  "Replace session-scoped turn OPTIONS."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-event-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-set-turn-options aggregate session-id options))
   :write-index t))

(defun e-session-set-current-branch (store session-id branch-id)
  "Set the current branch cursor."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-event-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-set-current-branch
      aggregate session-id branch-id))
   :write-index t))

(defun e-session-clear-messages (store session-id)
  "Clear transcript-derived state with an append-only reset event."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-entry-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-clear-messages aggregate session-id))
   :write-index t))

(defun e-session-rename (store session-id name)
  "Rename SESSION-ID."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-event-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-rename aggregate session-id name))
   :write-index t))

(defun e-session-append-board-message (store session-id message)
  "Append one immutable board envelope."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-append-board-message
      aggregate session-id message))
   (lambda (_aggregate result)
     (list :type "board-message" :session-id session-id
           :timestamp (e-session--timestamp) :message result))
   :write-index t))

(defun e-session-clear-board-messages (store session-id)
  "Clear the independent board journal."
  (e-session--ensure-loaded store session-id)
  (e-session--commit-session-mutation
   store session-id
   (lambda (aggregate)
     (e-session-aggregate-clear-board-messages aggregate session-id))
   (lambda (_aggregate _result)
     (list :type "board-messages-cleared" :session-id session-id
           :id (e-session-generate-ulid) :timestamp (e-session--timestamp)))
   :write-index t))

(defun e-session-declare-board-state
    (store session-id principal board-id &optional association-role routing-policy)
  "Set and persist board identity and routing policy."
  (e-session--ensure-loaded store session-id)
  (let ((state
         (e-session--commit-session-mutation
          store session-id
          (lambda (aggregate)
            (e-session-aggregate-declare-board-state
             aggregate session-id principal board-id association-role
             routing-policy))
          (lambda (aggregate _result)
            (e-session--board-state-record aggregate session-id))
          :write-index t)))
    (copy-tree state)))

(cl-defun e-session-fork (store session-id &key at metadata name)
  "Fork SESSION-ID and publish the new aggregate through storage."
  (e-session--ensure-loaded store session-id)
  (if (not (e-session-storage-sqlite-p store))
      (let* ((fork (e-session-aggregate-fork
                    store session-id :at at :metadata metadata :name name))
             (fork-id (plist-get fork :id))
             (path (e-session-aggregate-current-path store fork-id)))
        (condition-case err
            (progn
              (e-session--persist-record store fork-id
                                         (e-session--root-record-from-store
                                          store fork-id) nil)
              (when (e-session-aggregate-board-association fork)
                (e-session--persist-board-state store fork-id))
              (dolist (entry (cdr path))
                (e-session--persist-entry store fork-id entry nil))
              (e-session--write-index store)
              fork)
          (error
           (e-session-abort-created store fork-id)
           (signal (car err) (cdr err)))))
    (e-session--call-with-commit-barrier
     store session-id
     (lambda ()
       (let (stage fork fork-id records prepared)
         (condition-case err
             (progn
               (setq stage
                     (e-session-aggregate-stage-session-mutation
                      store session-id)
                     fork
                     (e-session-aggregate-fork
                      stage session-id :at at :metadata metadata :name name)
                     fork-id (plist-get fork :id))
               ;; A fork is one bounded session-owner mutation.  Build and
               ;; preflight its complete record vector before submitting the
               ;; existing atomic append-batch command with one stable runtime
               ;; identity.  No partial fork can survive process loss.
               (setq records
                     (append
                      (list (e-session--root-record-from-store stage fork-id))
                      (when (e-session-aggregate-board-association fork)
                        (list (e-session--board-state-record stage fork-id)))
                      (mapcar
                       (lambda (entry)
                         (e-session-codec-record-for-entry fork-id entry))
                       (cdr (e-session-aggregate-current-path
                             stage fork-id))))
                     prepared
                     (e-session-storage-prepare-mutation-batch
                      store fork-id records))
               (e-session-storage-commit-mutation-batch
                store fork-id prepared))
           (error
            ;; Only a definitive pre-acknowledgement failure may remove the
            ;; prepared fork.  After the batch ACK it is authoritative and no
            ;; later projection failure may be reported as a failed fork.
            (when fork-id
              (e-session-storage-abort-session store fork-id))
            (signal (car err) (cdr err))))
         (e-session-aggregate-publish-staged-session store stage fork-id)
         (e-session--write-index-after-primary store)
         fork)))))

(defun e-session-finalize (store on-done on-error)
  "Finalize STORE's asynchronous storage durability boundary."
  (e-session-storage-finalize-store store on-done on-error))

(defun e-session-enable (store)
  "Attach STORE's asynchronous persistence adapter and return its handle."
  (e-session-storage-enable store))

(defun e-session-persistence-unsettled-state (&rest _args)
  "Return process-wide physical storage durability state."
  (e-session-storage-unsettled-state))

(defun e-session-checkpoint-dirty-session-ids (store)
  "Return STORE's sessions with pending physical checkpoints."
  (e-session-storage-checkpoint-dirty-session-ids store))

(defun e-session-checkpoint-mark-clean (store session-ids)
  "Mark SESSION-IDS' physical checkpoints clean."
  (e-session-storage-checkpoint-mark-clean store session-ids))

(defun e-session-flush-write-queue (store)
  "Synchronously flush STORE's physical write queue."
  (e-session-storage-flush-write-queue store))

(defun e-session-get (store session-id)
  "Return loaded SESSION-ID semantic state."
  (e-session--ensure-loaded store session-id))

(defun e-session-session-present-p (store session-id)
  "Return non-nil when STORE contains SESSION-ID without replaying it."
  (e-session-aggregate-session-present-p store session-id))

(defun e-session-messages (store session-id)
  "Return SESSION-ID transcript messages."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-messages store session-id))

(defun e-session-activity-events (store session-id)
  "Return SESSION-ID activity events."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-activity-events store session-id))

(defun e-session-latest-token-usage-event (store session-id)
  "Return latest token-usage activity event."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-latest-token-usage-event store session-id))

(defun e-session-session-events (store session-id)
  "Return SESSION-ID session events."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-session-events store session-id))

(defun e-session-compactions (store session-id)
  "Return SESSION-ID compaction records."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-compactions store session-id))

(defun e-session-provider-anchors (store session-id)
  "Return SESSION-ID provider anchors."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-provider-anchors store session-id))

(defun e-session-context-generations (store session-id)
  "Return SESSION-ID context generations."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-context-generations store session-id))

(defun e-session-context-promotions (store session-id)
  "Return SESSION-ID context promotions."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-context-promotions store session-id))

(defun e-session-context-erasures (store session-id)
  "Return SESSION-ID context erasures."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-context-erasures store session-id))

(defun e-session-erased-tool-call-ids (store session-id &optional head-id)
  "Return erased tool-call identities on SESSION-ID's selected path."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-erased-tool-call-ids store session-id head-id))

(defun e-session-context-curations (store session-id)
  "Return SESSION-ID context curation records."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-context-curations store session-id))

(defun e-session-context-lifetime-current-generation
    (store session-id &optional head-id)
  "Return current semantic context generation."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-context-lifetime-current-generation store session-id head-id))

(defun e-session-context-lifetime-durable-message (entry)
  "Return a portable durable message projection."
  (e-session-aggregate-context-lifetime-durable-message entry))

(defun e-session-context-lifetime-projection
    (store session-id &optional head-id)
  "Return the provider-neutral context projection."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-context-lifetime-projection store session-id head-id))

(defun e-session-process-reports (store session-id)
  "Return SESSION-ID process reports."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-process-reports store session-id))

(cl-defun e-session-latest-compatible-provider-anchor
    (store session-id provider-id &key model fingerprints)
  "Return the latest compatible provider anchor."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-latest-compatible-provider-anchor
   store session-id provider-id :model model :fingerprints fingerprints))

(defun e-session-turn-options (store session-id)
  "Return session-scoped turn options."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-turn-options store session-id))

(defun e-session-metadata-context-references (metadata owner)
  "Return OWNER's durable context references from METADATA."
  (e-session-metadata-context-references-value metadata owner))

(defun e-session-context-references (store session-id owner)
  "Return OWNER's durable context references."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-context-references store session-id owner))

(defun e-session-capability-state (store session-id capability-id)
  "Return durable CAPABILITY-ID state."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-capability-state store session-id capability-id))

(defun e-session-current-path (store session-id &optional head-id)
  "Return SESSION-ID's current parent path."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-current-path store session-id head-id))

(defun e-session-entries-in-turn (store session-id turn-id)
  "Return current-path entries for TURN-ID."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-entries-in-turn store session-id turn-id))

(defun e-session-entry-by-id (store session-id entry-id)
  "Return SESSION-ID entry ENTRY-ID."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-entry-by-id store session-id entry-id))

(defun e-session-entry-previous (store session-id entry-id)
  "Return current-path predecessor of ENTRY-ID."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-entry-previous store session-id entry-id))

(defun e-session-entry-next (store session-id entry-id)
  "Return current-path successor of ENTRY-ID."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-entry-next store session-id entry-id))

(defun e-session-latest-entry-of-type (store session-id type)
  "Return latest current-path entry of TYPE."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-latest-entry-of-type store session-id type))

(defun e-session-entries-from (store session-id first-entry-id)
  "Return current path from FIRST-ENTRY-ID."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-entries-from store session-id first-entry-id))

(defun e-session-entries-before (store session-id entry-id)
  "Return current path entries before ENTRY-ID."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-entries-before store session-id entry-id))

(defun e-session-compaction-boundary-valid-p (store session-id compaction)
  "Return non-nil when COMPACTION's boundary is on the current path."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-compaction-boundary-valid-p store session-id compaction))

(defun e-session-latest-valid-compaction (store session-id)
  "Return latest current-path compaction."
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-latest-valid-compaction store session-id))

(defun e-session-provider-anchor-incompatibility-reason
    (store session-id anchor provider-id model fingerprints)
  "Return incompatibility reason for ANCHOR."
  (e-session--ensure-loaded store session-id)
  (e-session-provider-anchor-policy-incompatibility-reason
   (e-session-aggregate-current-path store session-id)
   anchor provider-id model fingerprints))

(defun e-session-provider-anchor-compatible-p
    (store session-id anchor provider-id model fingerprints)
  "Return non-nil when ANCHOR is compatible."
  (e-session--ensure-loaded store session-id)
  (e-session-provider-anchor-policy-compatible-p
   (e-session-aggregate-current-path store session-id)
   anchor provider-id model fingerprints))

(defun e-session-board-association (session)
  "Return normalized board association from semantic SESSION."
  (e-session-aggregate-board-association session))

(defun e-session-board-association-invalid-p (association)
  "Return non-nil for malformed board ASSOCIATION."
  (e-session-aggregate-board-association-invalid-p association))

(defun e-session-board-routing-policy (session)
  "Return detached board routing policy from SESSION."
  (e-session-aggregate-board-routing-policy session))

(defun e-session-board-association-policy-present-p (association)
  "Return non-nil when ASSOCIATION carries routing policy."
  (e-session-aggregate-board-association-policy-present-p association))

(defun e-session-board-messages (store session-id)
  "Return detached board envelopes for SESSION-ID."
  ;; Board state is kept outside the generic session projection, but reading
  ;; it still has the facade's normal lazy-replay semantics for index stubs.
  (e-session--ensure-loaded store session-id)
  (e-session-aggregate-board-messages store session-id))

(defun e-session-generate-id ()
  "Return a new durable session id."
  (e-session-identity-generate-id))

(defun e-session-generate-ulid ()
  "Return a new ordered durable entry id."
  (e-session-identity-generate-ulid))

(defun e-session-metadata-key-state-class (key)
  "Return the state class declared for durable metadata KEY."
  (e-session-metadata-policy-key-state-class key))

(defun e-session-display-title (store session-id)
  "Return SESSION-ID's display title without replaying an index stub."
  (e-session-aggregate-display-title store session-id))

(defun e-session-root-p (session)
  "Return non-nil when SESSION belongs in root pickers."
  (e-session-aggregate-root-p session))

(defun e-session-list (store)
  "Return detached session index projections sorted newest first."
  (e-session-aggregate-list store))

(defun e-session-list-roots (store)
  "Return user-facing root session projections."
  (e-session-aggregate-list-roots store))

(defun e-session-index-entry (store session-id)
  "Return detached catalog metadata for SESSION-ID, when present.

This application-level projection is used by offline maintenance callers that
must preserve an unloaded index stub while replaying one journal.  It keeps
the aggregate record and storage reference behind their owner boundaries."
  (when-let ((session (e-session-aggregate-peek-session store session-id)))
    (e-session-catalog-index-entry
     session (e-session-storage-session-reference store session-id))))

(defun e-session-unload-session (store session-id &optional index-entry)
  "Replace loaded SESSION-ID with its detached index projection.

INDEX-ENTRY, when supplied, is the caller's pre-replay snapshot.  This
operation is intended for bounded offline replay/migration workflows; it
never touches durable files or another session's aggregate state."
  (when (e-session-aggregate-peek-session store session-id)
    (let ((entry (or index-entry (e-session-index-entry store session-id))))
      (e-session-aggregate-reset-session store session-id)
      (when entry
        (e-session-aggregate-install-index-session
         store (e-session--index-entry-session store entry)))
      (e-session-aggregate-peek-session store session-id))))

(defun e-session-checkpoint-manifest (store session-id)
  "Return semantic bounded checkpoint manifest for SESSION-ID."
  (e-session-catalog-checkpoint-manifest
   (e-session--ensure-loaded store session-id)
   (e-session-aggregate-board-messages store session-id)))

(defun e-session-refresh-index-metadata (store)
  "Refresh unloaded session metadata from the physical index."
  (when-let ((value (and (e-session--persistent-p store)
                         (e-session-storage-read-catalog-projection store))))
    (dolist (entry (e-session--index-entries value))
      (when-let ((replacement (e-session--index-entry-session store entry)))
        (e-session-aggregate-merge-index-session store replacement))))
  store)

(defun e-session-refresh-index (store)
  "Publish STORE's current index and deferred checkpoint projection.

The application service composes aggregate values with catalog policy and
hands the resulting index and checkpoint operation to the storage owner's one
publication boundary.  Consumers needing a derived-index barrier use this
operation instead of reaching into the physical writer."
  (e-session--write-index store))

(provide 'e-session)

;;; e-session.el ends here

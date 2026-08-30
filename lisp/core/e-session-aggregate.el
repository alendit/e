;;; e-session-aggregate.el --- Session aggregate and semantic mutations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the live session aggregate, identity/path semantics, board journal,
;; provider-neutral metadata, and semantic mutations.  Persistence is an
;; application-service concern: this module has no dependency on the catalog
;; or storage adapter.  It consumes the pure codec only for one canonical
;; routing-value size check; it never performs durable I/O or replay decoding.

;;; Code:

(require 'cl-lib)
(require 'e-context-lifetime)
(require 'e-session-codec)
(require 'e-board)
(require 'seq)
(require 'subr-x)

(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-session-missing "Session does not exist")
(define-error 'e-session-duplicate "Session already exists")
(define-error 'e-session-checkpoint-missing "Session resume checkpoint does not exist" 'e-session-missing)
(define-error 'e-session-checkpoint-invalid "Session resume checkpoint is invalid")
(define-error 'e-session-board-message-conflict "Conflicting board message envelope")
(define-error 'e-session-board-message-cycle "Cyclic board message envelope")
(define-error 'e-session-board-message-invalid-record-type "Invalid board message record type")
(define-error 'e-session-error "Session error")

(defgroup e-session nil "Session storage for e." :group 'e :prefix "e-session-")
(defcustom e-session-directory (locate-user-emacs-file "e/sessions/")
  "Default directory used for persisted e sessions."
  :type 'directory :group 'e-session)

;; The aggregate owns the deterministic set of context entry types.  The
;; codec validates the fixed durable record versions independently; no runtime
;; registry or load-order-sensitive registration hook is needed.
(defconst e-session-aggregate--context-lifetime-entry-types
  '(context-generation context-promotion)
  "Durable entry types owned by the generational context lifetime model.")

(cl-defstruct (e-session-store (:constructor e-session-store-create))
  (sessions (make-hash-table :test 'equal))
  (entry-indexes (make-hash-table :test 'equal))
  (board-journals (make-hash-table :test 'equal))
  directory
  sessions-directory
  index-file
  persistent
  write-mode
  (sequence 0))

(cl-defstruct (e-session-board-journal
               (:constructor e-session-aggregate--board-journal-create))
  messages tail (id-index (make-hash-table :test 'equal)))

(defun e-session-aggregate-reset (store)
  "Clear all loaded semantic state in STORE before a replay pass.

Replay/application code uses this operation instead of mutating the
aggregate's hash tables and sequence fields directly.  Physical storage state
and its queues are intentionally unaffected."
  (clrhash (e-session-store-sessions store))
  (clrhash (e-session-store-entry-indexes store))
  (clrhash (e-session-store-board-journals store))
  (setf (e-session-store-sequence store) 0)
  store)

(defun e-session-aggregate-reset-session (store session-id)
  "Remove one SESSION-ID's loaded state and replay indexes from STORE."
  (remhash session-id (e-session-store-sessions store))
  (remhash session-id (e-session-store-entry-indexes store))
  (remhash session-id (e-session-store-board-journals store))
  session-id)

(defun e-session-aggregate-session-present-p (store session-id)
  "Return non-nil when STORE has a loaded or indexed SESSION-ID."
  (and (gethash session-id (e-session-store-sessions store)) t))

(defun e-session-aggregate-install-index-session (store session)
  "Install an unloaded semantic index SESSION in STORE.

The aggregate owns the session map and sequence high-water mark; callers pass a
fully detached stub produced from the catalog projection."
  (let ((session-id (plist-get session :id)))
    (unless session-id
      (signal 'e-session-error (list "Indexed session has no id" session)))
    (puthash session-id session (e-session-store-sessions store))
    (setf (e-session-store-sequence store)
          (max (e-session-store-sequence store)
               (or (plist-get session :updated-seq) 0)))
    session))

(defun e-session-aggregate-session-values (store)
  "Return the current semantic session values for composition.

The returned list is a traversal snapshot; each session remains owned by the
aggregate and must be treated as read-only by projection consumers."
  (let (sessions)
    (maphash (lambda (_session-id session) (push session sessions))
             (e-session-store-sessions store))
    (nreverse sessions)))

(defun e-session-aggregate-merge-index-session (store replacement)
  "Merge detached index metadata into an unloaded aggregate stub.

Loaded transcripts remain authoritative.  This narrow operation is used by a
catalog refresh and avoids exposing the aggregate's session table to the
application service."
  (when-let ((session (gethash (plist-get replacement :id)
                               (e-session-store-sessions store))))
    (unless (plist-get session :loaded)
      (dolist (field '(:metadata :updated-at :updated-seq :name :summary
                       :message-count :last-message-at
                       :latest-assistant-marker :board-session-state :file))
        (when (plist-member replacement field)
          (plist-put session field (plist-get replacement field)))))
    session))


(defconst e-session-aggregate--replay-list-fields
  '(:session-events :messages :activity-events :branch-summaries
    :compactions :provider-anchors :process-reports
    :context-generations :context-promotions
    :context-curation-packages)
  "Session fields accumulated in reverse order while replaying JSONL.")

(defconst e-session-aggregate--list-tail-fields
  '((:messages . :messages-tail)
    (:activity-events . :activity-events-tail)
    (:branch-summaries . :branch-summaries-tail)
    (:compactions . :compactions-tail)
    (:provider-anchors . :provider-anchors-tail)
    (:process-reports . :process-reports-tail)
    (:context-generations . :context-generations-tail)
    (:context-promotions . :context-promotions-tail)
    (:context-curation-packages . :context-curation-packages-tail))
  "Internal append-only list fields and their cached tail cells.")

(defconst e-session-metadata-schema
  '((:name
     :owner session
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:model
     :owner session
     :state-class session-config
     :lifetime durable
     :indexed t
     :legacy t)
    (:project-root
     :owner session
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:harness-instance-id
     :owner chat
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:origin
     :owner shell
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:source
     :owner shell
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:source-reference
     :owner shell
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t)
    (:context-references
     :owner session
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t)
    (:org-canvas-ref
     :owner org-canvas
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t)
    (:org-canvas
     :owner org-canvas
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t
     :legacy t)
    (:parent-session-id
     :owner subagents
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:subagent-role
     :owner subagents
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:subagent-label
     :owner subagents
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:tmp-lineage-id
     :owner subagents
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:task-queue-task-id
     :owner task-queue
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:board-run-id
     :owner board-orchestration
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:board-task-key
     :owner board-orchestration
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:board-attempt
     :owner board-orchestration
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:mcp-active
     :owner mcp
     :state-class capability-state
     :lifetime durable
     :indexed t)
    (:capability-state
     :owner capabilities
     :state-class capability-state
     :lifetime durable
     :indexed t))
  "Allowed durable session metadata keys and their state ownership.")

(defconst e-session-aggregate--presentation-metadata-keys
  '(:e-chat-read-markers)
  "Presentation-only metadata keys rejected on write and removed on replay.")

(defconst e-session-aggregate--ulid-alphabet "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
  "Crockford Base32 alphabet used for ULID strings.")

(defvar e-session-aggregate--last-ulid-milliseconds nil
  "Last millisecond timestamp used by `e-session-aggregate-generate-ulid'.")

(defvar e-session-aggregate--last-ulid-random nil
  "Last 80-bit random suffix used by `e-session-aggregate-generate-ulid'.")

(defun e-session-aggregate--metadata-descriptor (key)
  "Return metadata schema descriptor for KEY."
  (seq-find (lambda (descriptor)
              (eq (car descriptor) key))
            e-session-metadata-schema))

(defun e-session-aggregate-metadata-key-state-class (key)
  "Return the declared state class for durable metadata KEY."
  (plist-get (cdr (e-session-aggregate--metadata-descriptor key)) :state-class))

(defun e-session-aggregate--plist-remove (plist key)
  "Return PLIST without KEY."
  (let (result)
    (while (consp plist)
      (let ((current-key (pop plist)))
        (when (consp plist)
          (let ((value (pop plist)))
            (unless (eq current-key key)
              (push current-key result)
              (push value result))))))
    (nreverse result)))

(defun e-session-aggregate-keyword-plist-shape-p (value)
  "Return non-nil when VALUE has keyword plist shape."
  (and (proper-list-p value)
       (let ((tail value)
             (valid t))
         (while (and valid tail)
           (setq valid
                 (and (consp tail)
                      (keywordp (car tail))
                      (consp (cdr tail))))
           (setq tail (cddr tail)))
         valid)))

(defun e-session-aggregate--metadata-owner-key (owner)
  "Return stable keyword key for metadata OWNER."
  (cond
   ((keywordp owner) owner)
   ((symbolp owner) (intern (concat ":" (symbol-name owner))))
   ((stringp owner) (intern (concat ":" owner)))
   (t (error "Metadata owner must be a keyword, symbol, or string: %S" owner))))

(defun e-session-aggregate--metadata-json-array-safe-value (value)
  "Return VALUE with reference arrays encoded unambiguously for JSON.
Keyword plists remain objects.  Other proper lists become vectors so the JSON
writer cannot reinterpret a list of plists as one object."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'e-session-aggregate--metadata-json-array-safe-value value)))
   ((e-session-aggregate-keyword-plist-shape-p value)
    (let (result)
      (while (consp value)
        (let ((key (pop value)))
          (when (consp value)
            (push key result)
            (push (e-session-aggregate--metadata-json-array-safe-value (pop value))
                  result))))
      (nreverse result)))
   ((proper-list-p value)
    (vconcat (mapcar #'e-session-aggregate--metadata-json-array-safe-value value)))
   (t value)))

(defun e-session-aggregate--metadata-public-value (value)
  "Return persisted metadata VALUE in caller-facing Elisp shape."
  (cond
   ((vectorp value)
    (mapcar #'e-session-aggregate--metadata-public-value value))
   ((e-session-aggregate-keyword-plist-shape-p value)
    (let (result)
      (while (consp value)
        (let ((key (pop value)))
          (when (consp value)
            (push key result)
            (push (e-session-aggregate--metadata-public-value (pop value)) result))))
      (nreverse result)))
   ((proper-list-p value)
    (mapcar #'e-session-aggregate--metadata-public-value value))
   (t value)))

(defun e-session-aggregate--metadata-validate-org-canvas-ref (value key)
  "Validate Org Canvas metadata VALUE under KEY."
  (unless (or (null value) (e-session-aggregate-keyword-plist-shape-p value))
    (error "Session metadata %S must be a keyword plist" key))
  (when (or (plist-member value :last-focus)
            (plist-member value :last-scope))
    (error "Session metadata %S must not contain volatile focus or scope" key)))

(defun e-session-aggregate--metadata-validate-context-references (value)
  "Validate durable current-state reference VALUE."
  (unless (or (null value) (e-session-aggregate-keyword-plist-shape-p value))
    (error "Session metadata :context-references must be an owner-keyed plist")))

(defun e-session-aggregate--metadata-validate-capability-state (value)
  "Validate durable capability-state VALUE."
  (unless (or (null value) (e-session-aggregate-keyword-plist-shape-p value))
    (error "Session metadata :capability-state must be an owner-keyed plist")))

(defun e-session-aggregate--validate-metadata-value (key value)
  "Validate durable session metadata KEY VALUE."
  (pcase key
    ((or :org-canvas :org-canvas-ref)
     (e-session-aggregate--metadata-validate-org-canvas-ref value key))
    (:context-references
     (e-session-aggregate--metadata-validate-context-references value))
    (:capability-state
     (e-session-aggregate--metadata-validate-capability-state value))
    (_ nil)))

(defun e-session-aggregate--validate-metadata-class (metadata expected-class)
  "Validate that METADATA only contains keys in EXPECTED-CLASS."
  (let ((tail metadata))
    (while (consp tail)
      (let ((key (pop tail)))
        (unless (consp tail)
          (error "Session metadata has key %S without value" key))
        (let* ((value (pop tail))
               (descriptor (e-session-aggregate--metadata-descriptor key))
               (state-class (plist-get (cdr descriptor) :state-class)))
          (unless descriptor
            (error "Session metadata key %S has no durable state schema" key))
          (unless (eq state-class expected-class)
            (error "Session metadata key %S is %S, not %S"
                   key state-class expected-class))
          (e-session-aggregate--validate-metadata-value key value)))))
  metadata)

(defun e-session-aggregate--validate-metadata (metadata)
  "Validate durable session METADATA and return it."
  (unless (or (null metadata) (e-session-aggregate-keyword-plist-shape-p metadata))
    (error "Session metadata must be a keyword plist"))
  (let ((tail metadata))
    (while (consp tail)
      (let* ((key (pop tail))
             (value (pop tail))
             (descriptor (e-session-aggregate--metadata-descriptor key)))
        (when (memq key e-session-aggregate--presentation-metadata-keys)
          (error "Session metadata key %S is presentation state" key))
        (unless descriptor
          (error "Session metadata key %S has no durable state schema" key))
        (e-session-aggregate--validate-metadata-value key value))))
  metadata)

(defun e-session-aggregate--normalize-org-canvas-ref-for-replay (value)
  "Return legacy Org Canvas VALUE without volatile focus fields."
  (when value
    (setq value (copy-sequence value))
    (setq value (e-session-aggregate--plist-remove value :last-focus))
    (setq value (e-session-aggregate--plist-remove value :last-scope)))
  value)

(defun e-session-aggregate--legacy-metadata-key (value)
  "Return schema metadata key named by legacy VALUE."
  (let ((name (cond
               ((keywordp value)
                (string-remove-prefix ":" (symbol-name value)))
               ((symbolp value) (symbol-name value))
               ((stringp value) (string-remove-prefix ":" value)))))
    (when name
      (seq-some (lambda (descriptor)
                  (let ((key (car descriptor)))
                    (and (string= name
                                  (string-remove-prefix
                                   ":" (symbol-name key)))
                         key)))
                e-session-metadata-schema))))

(defun e-session-aggregate--normalize-legacy-metadata-array (metadata)
  "Repair legacy JSON-array METADATA into a schema-keyed plist.
This is only for replaying old persisted records that encoded metadata as
arrays and sometimes inverted key/value pairs."
  (if (or (null metadata)
          (e-session-aggregate-keyword-plist-shape-p metadata)
          (not (proper-list-p metadata)))
      metadata
    (let ((tail metadata)
          result
          repaired)
      (while (consp tail)
        (let* ((first (pop tail))
               (second (and (consp tail) (pop tail)))
               (first-key (e-session-aggregate--legacy-metadata-key first))
               (second-key (e-session-aggregate--legacy-metadata-key second)))
          (cond
           ((and first-key (not second-key))
            (setq result (plist-put result first-key second)
                  repaired t))
           ((and second-key (not first-key))
            (setq result (plist-put result second-key first)
                  repaired t)))))
      (if repaired result metadata))))

(defun e-session-aggregate-normalize-metadata-for-replay (metadata &optional legacy)
  "Return replayed METADATA without known transient state."
  (when (and legacy
             (consp metadata)
             (not (keywordp (car metadata))))
    (setq metadata (e-session-aggregate--normalize-legacy-metadata-array metadata)))
  (when metadata
    (setq metadata (copy-sequence metadata))
    (dolist (key e-session-aggregate--presentation-metadata-keys)
      (setq metadata (e-session-aggregate--plist-remove metadata key)))
    (when (plist-member metadata :org-canvas)
      (setq metadata
            (plist-put
             metadata
             :org-canvas
             (e-session-aggregate--normalize-org-canvas-ref-for-replay
              (plist-get metadata :org-canvas)))))
    (when (plist-member metadata :org-canvas-ref)
      (setq metadata
            (plist-put
             metadata
             :org-canvas-ref
             (e-session-aggregate--normalize-org-canvas-ref-for-replay
              (plist-get metadata :org-canvas-ref))))))
    ;; JSON arrays are read as lists by the codec.  Re-establish the
    ;; aggregate's canonical vector representation for durable current-state
    ;; reference arrays before a checkpoint projection crosses the JSON
    ;; boundary again; otherwise a list of reference plists is encoded as one
    ;; object and loses the established array shape.
    (when (plist-member metadata :context-references)
      (setq metadata
            (plist-put
             metadata
             :context-references
             (e-session-aggregate--metadata-json-array-safe-value
              (plist-get metadata :context-references)))))
  metadata)

(defun e-session-aggregate--timestamp (&optional time)
  "Return TIME as a compact UTC timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun e-session-aggregate--id-timestamp (&optional time)
  "Return TIME as a session-id timestamp."
  (format-time-string "%Y%m%dT%H%M%S" time t))

(defun e-session-aggregate--generate-id ()
  "Generate a persistent session id."
  (let* ((seed (format "%S" (list (current-time) (random) (emacs-pid)
                                  (system-name))))
         (suffix (substring (secure-hash 'sha1 seed) 0 12)))
    (format "%s-%s" (e-session-aggregate--id-timestamp) suffix)))

(defun e-session-aggregate-generate-id ()
  "Return a fresh persistent session id without publishing it.
Application services use this for admission preflight before session creation."
  (e-session-aggregate--generate-id))

(defun e-session-aggregate--ulid-encode (number length)
  "Encode NUMBER as a Crockford Base32 string with LENGTH characters."
  (let ((chars (make-string length ?0))
        (index (1- length)))
    (while (>= index 0)
      (aset chars index (aref e-session-aggregate--ulid-alphabet (logand number 31)))
      (setq number (ash number -5))
      (setq index (1- index)))
    chars))

(defun e-session-aggregate--current-milliseconds ()
  "Return current Unix time in milliseconds."
  (floor (* 1000 (float-time))))

(defun e-session-aggregate--random-80-bit ()
  "Return a sufficiently random 80-bit integer."
  (let* ((seed (format "%S" (list (current-time) (random t) (emacs-pid)
                                  (system-name))))
         (hex (substring (secure-hash 'sha1 seed) 0 20)))
    (string-to-number hex 16)))

(defun e-session-aggregate--ulid-from-parts (milliseconds random)
  "Return a ULID from MILLISECONDS and 80-bit RANDOM suffix."
  (concat (e-session-aggregate--ulid-encode milliseconds 10)
          (e-session-aggregate--ulid-encode random 16)))

(defun e-session-aggregate-generate-ulid ()
  "Generate an opaque monotonic ULID string for durable session entries."
  (let* ((milliseconds (e-session-aggregate--current-milliseconds))
         (random (if (equal milliseconds e-session-aggregate--last-ulid-milliseconds)
                     (1+ (or e-session-aggregate--last-ulid-random 0))
                   (e-session-aggregate--random-80-bit)))
         (random-limit (expt 2 80)))
    (when (>= random random-limit)
      (setq milliseconds (1+ milliseconds))
      (setq random 0))
    (setq e-session-aggregate--last-ulid-milliseconds milliseconds
          e-session-aggregate--last-ulid-random random)
    (e-session-aggregate--ulid-from-parts milliseconds random)))

(defun e-session-aggregate--timestamp-milliseconds (timestamp)
  "Return TIMESTAMP parsed as Unix milliseconds, or current milliseconds."
  (condition-case nil
      (if (stringp timestamp)
          (floor (* 1000 (float-time (date-to-time timestamp))))
        (e-session-aggregate--current-milliseconds))
    (error (e-session-aggregate--current-milliseconds))))

(defun e-session-aggregate--legacy-entry-id (session type ordinal timestamp)
  "Return a stable backfilled id for legacy SESSION entry TYPE at ORDINAL."
  (let* ((session-id (plist-get session :id))
         (seed (format "%s:%s:%s:%s" session-id type ordinal timestamp))
         (random (string-to-number (substring (secure-hash 'sha1 seed) 0 20)
                                   16)))
    (e-session-aggregate--ulid-from-parts
     (e-session-aggregate--timestamp-milliseconds timestamp)
     random)))

(defun e-session-aggregate--next-sequence (store)
  "Return STORE's next mutation sequence."
  (setf (e-session-store-sequence store)
        (1+ (e-session-store-sequence store))))

(defun e-session-aggregate--touch (store session &optional timestamp)
  "Update SESSION's modification metadata in STORE."
  (plist-put session :updated-at (or timestamp (e-session-aggregate--timestamp)))
  (plist-put session :updated-seq (e-session-aggregate--next-sequence store))
  session)


(defun e-session-aggregate--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-session-aggregate--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when developer profiling is enabled."
  (if (e-session-aggregate--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-session-aggregate--entry-index (store session-id)
  "Return STORE's entry-id index for SESSION-ID."
  (or (gethash session-id (e-session-store-entry-indexes store))
      (puthash session-id
               (make-hash-table :test 'equal)
               (e-session-store-entry-indexes store))))

(defun e-session-aggregate--clear-entry-index (store session-id)
  "Clear STORE's entry-id index for SESSION-ID."
  (remhash session-id (e-session-store-entry-indexes store)))

(defun e-session-aggregate--index-entry (store session-id entry)
  "Index durable ENTRY for SESSION-ID in STORE."
  (when-let ((entry-id (plist-get entry :id)))
    (puthash entry-id entry (e-session-aggregate--entry-index store session-id)))
  entry)

(defun e-session-aggregate--index-session-entries (store session)
  "Rebuild STORE's entry-id index for SESSION."
  (let ((session-id (plist-get session :id)))
    (when session-id
      (e-session-aggregate--clear-entry-index store session-id)
      (dolist (entry (e-session-aggregate--entries store session-id))
        (e-session-aggregate--index-entry store session-id entry)))))

(defun e-session-aggregate--list-tail (items)
  "Return the tail cell for ITEMS, or nil."
  (when items
    (last items)))

(defun e-session-aggregate--tail-field (field)
  "Return the cached tail field for append-only FIELD."
  (alist-get field e-session-aggregate--list-tail-fields))

(defun e-session-aggregate-initialize-list-state (session)
  "Destructively initialize SESSION append-only list tail fields.
This repairs or resets the internal cached tail cells from the current
canonical list values.  Callers use this after creating or replaying a session,
or after constructing an unloaded index stub."
  (dolist (pair e-session-aggregate--list-tail-fields)
    (plist-put session (cdr pair) (e-session-aggregate--list-tail
                                   (plist-get session (car pair)))))
  session)

(defun e-session-aggregate--replace-list-field (session field items)
  "Destructively replace SESSION FIELD with ITEMS and update its tail."
  (plist-put session field items)
  (when-let ((tail-field (e-session-aggregate--tail-field field)))
    (plist-put session tail-field (e-session-aggregate--list-tail items)))
  items)

(defun e-session-aggregate--append-list-item (session field item)
  "Append ITEM to SESSION FIELD in O(1) and return ITEM.
The canonical list spine belongs to the session store.  If FIELD has legacy
contents but no cached tail cell, compute and cache the tail once before
appending."
  (let* ((tail-field (e-session-aggregate--tail-field field))
         (cell (list item))
         (tail (or (and tail-field (plist-get session tail-field))
                   (when-let ((items (plist-get session field)))
                     (e-session-aggregate--list-tail items)))))
    (if tail
        (setcdr tail cell)
      (plist-put session field cell))
    (when tail-field
      (plist-put session tail-field cell))
    item))

(defun e-session-aggregate--first-user-message (messages)
  "Return first user-authored content in MESSAGES."
  (catch 'found
    (dolist (message messages)
      (when (eq (plist-get message :role) 'user)
        (let ((content (plist-get message :content)))
          (when (stringp content)
            (throw 'found content)))))))

(defun e-session-aggregate--default-title (prompt)
  "Return PROMPT formatted as a default session title."
  (if (> (length prompt) 25)
      (concat (substring prompt 0 25) "...")
    prompt))

(defun e-session-aggregate--refresh-derived-fields (store session)
  "Refresh derived display fields for SESSION in STORE."
  (let ((messages (plist-get session :messages)))
    (plist-put session :summary (e-session-aggregate--first-user-message messages))
    (plist-put session :message-count (length messages))
    (plist-put session :last-message-at (e-session-aggregate--last-message-at session))
    (plist-put session :latest-assistant-marker
               (e-session-aggregate--latest-assistant-marker session))
    (when-let ((sessions-directory (e-session-store-sessions-directory store)))
      (plist-put session :file
                 (expand-file-name
                  (concat (plist-get session :id) ".jsonl")
                  sessions-directory))))
  session)

(defun e-session-aggregate--refresh-file-field (store session)
  "Refresh persistent file metadata for SESSION in STORE."
  (when-let ((sessions-directory (e-session-store-sessions-directory store)))
    (plist-put session :file
               (expand-file-name
                (concat (plist-get session :id) ".jsonl")
                sessions-directory)))
  session)

(defun e-session-aggregate--message-summary (message)
  "Return MESSAGE content when it should become a session summary."
  (when (eq (plist-get message :role) 'user)
    (let ((content (plist-get message :content)))
      (when (stringp content)
        content))))

(defun e-session-aggregate--update-message-derived-fields-on-append
    (store session message)
  "Update SESSION derived fields incrementally for appended MESSAGE."
  (let ((count (plist-get session :message-count)))
    (plist-put session
               :message-count
               (if (integerp count)
                   (1+ count)
                 (length (plist-get session :messages)))))
  (unless (plist-get session :summary)
    (when-let ((summary (e-session-aggregate--message-summary message)))
      (plist-put session :summary summary)))
  (plist-put session :last-message-at (plist-get message :created-at))
  (when (eq (plist-get message :role) 'assistant)
    (plist-put session :latest-assistant-marker
               (e-session-aggregate--message-assistant-marker message)))
  (e-session-aggregate--refresh-file-field store session))

(defun e-session-aggregate--clear-message-derived-fields (store session)
  "Reset message-derived fields for cleared SESSION."
  (plist-put session :message-count 0)
  (plist-put session :summary nil)
  (plist-put session :last-message-at nil)
  (plist-put session :latest-assistant-marker nil)
  (e-session-aggregate--refresh-file-field store session))

(defun e-session-aggregate--display-title-for-session (session)
  "Return a display title for SESSION."
  (or (plist-get session :name)
      (when-let ((summary (plist-get session :summary)))
        (e-session-aggregate--default-title summary))
      (when-let ((created-at (plist-get session :created-at)))
        (format "Untitled %s" created-at))
      (format "Untitled %s" (plist-get session :id))))

(defun e-session-aggregate--prepend-replayed-item (session field item)
  "Prepend replayed ITEM to SESSION FIELD."
  (plist-put session field (cons item (plist-get session field))))

(defun e-session-aggregate--next-entry-ordinal (session)
  "Return SESSION's next replay entry ordinal."
  (let ((ordinal (1+ (or (plist-get session :entry-count) 0))))
    (plist-put session :entry-count ordinal)
    ordinal))

(defun e-session-aggregate--entry-id-from-record (record entry)
  "Return durable id from RECORD or ENTRY."
  (or (plist-get entry :id)
      (plist-get record :id)))

(defun e-session-aggregate--entry-parent-id-from-record (record entry)
  "Return parent id from RECORD or ENTRY."
  (if (plist-member entry :parent-id)
      (plist-get entry :parent-id)
    (plist-get record :parent-id)))

(defun e-session-aggregate--entry-with-identity (session type entry timestamp &optional record)
  "Return ENTRY with durable identity fields for SESSION and TYPE.
TIMESTAMP is used for creation metadata and legacy deterministic backfill.
When RECORD is non-nil, identity fields may be replayed from the JSONL record."
  (let ((entry (copy-sequence entry)))
    ;; TYPE is the replay dispatch contract.  JSON turns a nested symbol into
    ;; a string, which must never leak into the symbol-based in-memory model.
    (plist-put entry :type type)
    (unless (plist-get entry :id)
      (plist-put
       entry :id
       (or (e-session-aggregate--entry-id-from-record record entry)
           (if record
               (e-session-aggregate--legacy-entry-id
                session type (e-session-aggregate--next-entry-ordinal session) timestamp)
             (e-session-aggregate-generate-ulid)))))
    (unless (plist-member entry :parent-id)
      (when-let ((parent-id
                  (or (e-session-aggregate--entry-parent-id-from-record record entry)
                      (plist-get session :current-head-id))))
        (plist-put entry :parent-id parent-id)))
    (unless (plist-member entry :created-at)
      (plist-put entry :created-at timestamp))
    ;; Replay is an explicit durability proof supplied by the journal or
    ;; checkpoint loader.  Live appends receive their lifecycle state from
    ;; the persistence owner instead of being inferred from entry presence.
    (when record
      (plist-put entry :durability-state 'replayed-durable))
    entry))

(defun e-session-aggregate--normalize-entry-from-record
    (session type entry timestamp &optional record)
  "Return normalized durable ENTRY for replay or append."
  (e-session-aggregate--advance-head
   session
   (e-session-aggregate--entry-with-identity session type entry timestamp record)))

(defun e-session-aggregate--advance-head (session entry)
  "Advance SESSION current head to ENTRY."
  (plist-put session :current-head-id (plist-get entry :id))
  entry)

(defun e-session-aggregate--root-event-id (session)
  "Return SESSION root event id, when available."
  (or (plist-get session :root-event-id)
      (plist-get (car (plist-get session :session-events)) :id)))

(defun e-session-aggregate--session-event
    (session event-type timestamp &optional fields record)
  "Return a normalized session EVENT-TYPE entry for SESSION.
TIMESTAMP is used as creation metadata.  FIELDS are copied onto the event,
and RECORD supplies persisted identity fields during replay."
  (let ((entry (append (list :event-type event-type
                             :created-at timestamp)
                       (copy-sequence fields))))
    (e-session-aggregate--normalize-entry-from-record
     session 'session-event entry timestamp record)))

(defun e-session-aggregate--append-session-event
    (session event-type timestamp &optional fields record)
  "Append a normalized session EVENT-TYPE entry to SESSION."
  (let ((event (e-session-aggregate--session-event
                session event-type timestamp fields record)))
    (plist-put session
               :session-events
               (append (plist-get session :session-events) (list event)))
    (when (eq event-type 'session-created)
      (plist-put session :root-event-id (plist-get event :id)))
    event))

(defun e-session-aggregate--prepend-replayed-session-event
    (session event-type timestamp &optional fields record)
  "Prepend a replayed session EVENT-TYPE entry to SESSION."
  (let ((event (e-session-aggregate--session-event
                session event-type timestamp fields record)))
    (e-session-aggregate--prepend-replayed-item session :session-events event)
    (when (eq event-type 'session-created)
      (plist-put session :root-event-id (plist-get event :id)))
    event))

(defun e-session-aggregate--entries (store session-id)
  "Return all durable entries for SESSION-ID in insertion order."
  (let ((session (e-session-aggregate-get-live store session-id)))
    (append (plist-get session :session-events)
            (plist-get session :messages)
            (plist-get session :activity-events)
            (plist-get session :branch-summaries)
            (plist-get session :compactions)
            (plist-get session :provider-anchors)
            (plist-get session :process-reports)
            (plist-get session :context-generations)
            (plist-get session :context-promotions)
            (plist-get session :context-curation-packages))))

(defun e-session-aggregate-entry-by-id (store session-id entry-id)
  "Return durable entry ENTRY-ID from SESSION-ID."
  (or (gethash entry-id (e-session-aggregate--entry-index store session-id))
      (seq-find (lambda (entry)
                  (equal (plist-get entry :id) entry-id))
                (e-session-aggregate--entries store session-id))))

(defun e-session-aggregate--entry-children (store session-id parent-id)
  "Return entries whose parent is PARENT-ID in SESSION-ID."
  (seq-filter (lambda (entry)
                (equal (plist-get entry :parent-id) parent-id))
              (e-session-aggregate--entries store session-id)))

(defun e-session-aggregate-keyword-plist-p (value)
  "Return non-nil when VALUE is a proper plist with keyword keys."
  (and (proper-list-p value)
       (let ((tail value)
             (valid t))
         (while (and valid tail)
           (if (and (consp tail)
                    (keywordp (car tail))
                    (consp (cdr tail)))
               (setq tail (cddr tail))
             (setq valid nil)))
         (and valid (null tail)))))

(defun e-session-aggregate-current-path (store session-id &optional head-id)
  "Return SESSION-ID current parent path ending at HEAD-ID or current head."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (head-id (or head-id (plist-get session :current-head-id)))
         path)
    (while head-id
      (let ((entry (e-session-aggregate-entry-by-id store session-id head-id)))
        (unless entry
          (setq head-id nil))
        (when entry
          (push entry path)
          (setq head-id (plist-get entry :parent-id)))))
    path))

(defun e-session-aggregate-entries-in-turn (store session-id turn-id)
  "Return entries in SESSION-ID that belong to TURN-ID."
  (seq-filter (lambda (entry)
                (equal (plist-get entry :turn-id) turn-id))
              (e-session-aggregate-current-path store session-id)))

(defun e-session-aggregate-entry-previous (store session-id entry-id)
  "Return the previous entry before ENTRY-ID on SESSION-ID current path."
  (when-let ((entry (e-session-aggregate-entry-by-id store session-id entry-id)))
    (when-let ((parent-id (plist-get entry :parent-id)))
      (e-session-aggregate-entry-by-id store session-id parent-id))))

(defun e-session-aggregate-entry-next (store session-id entry-id)
  "Return the next entry after ENTRY-ID on SESSION-ID current path."
  (let ((path (e-session-aggregate-current-path store session-id)))
    (cadr (member (e-session-aggregate-entry-by-id store session-id entry-id) path))))

(defun e-session-aggregate-latest-entry-of-type (store session-id type)
  "Return latest entry of TYPE on SESSION-ID current path."
  (seq-find (lambda (entry)
              (eq (plist-get entry :type) type))
            (reverse (e-session-aggregate-current-path store session-id))))

(defun e-session-aggregate-entries-from (store session-id first-entry-id)
  "Return current-path entries from FIRST-ENTRY-ID to the current head."
  (let ((path (e-session-aggregate-current-path store session-id)))
    (member (e-session-aggregate-entry-by-id store session-id first-entry-id) path)))

(defun e-session-aggregate-entries-before (store session-id entry-id)
  "Return current-path entries before ENTRY-ID."
  (let ((entries nil)
        (done nil))
    (dolist (entry (e-session-aggregate-current-path store session-id))
      (unless done
        (if (equal (plist-get entry :id) entry-id)
            (setq done t)
          (push entry entries))))
    (nreverse entries)))

(defun e-session-aggregate-compaction-boundary-valid-p (store session-id compaction)
  "Return non-nil when COMPACTION points at an entry on the current path."
  (let ((boundary (plist-get compaction :first-kept-entry-id)))
    (and (stringp boundary)
         (e-session-aggregate-entry-by-id store session-id boundary)
         (seq-some (lambda (entry)
                     (equal (plist-get entry :id) boundary))
                   (e-session-aggregate-current-path store session-id)))))

(defun e-session-aggregate-latest-valid-compaction (store session-id)
  "Return the latest compaction record with a valid current-path boundary."
  (seq-find
   (lambda (entry)
     (and (eq (plist-get entry :type) 'compaction)
          (e-session-aggregate-compaction-boundary-valid-p store session-id entry)))
   (reverse (e-session-aggregate-compactions store session-id))))


(defun e-session-aggregate--provider-anchor-dynamic-segment-p (segment)
  "Return non-nil when SEGMENT is volatile current-state context."
  (let ((kind (and (e-session-aggregate-keyword-plist-p segment)
                   (plist-get segment :kind))))
    (or (eq kind 'current-state)
        (eq kind 'dynamic-context)
        (equal kind "current-state")
        (equal kind "dynamic-context"))))

(defun e-session-aggregate--provider-anchor-segment-list-p (segments)
  "Return non-nil when SEGMENTS is a list of segment plists."
  (and (proper-list-p segments)
       (cl-every (lambda (segment)
                   (and (e-session-aggregate-keyword-plist-p segment)
                        (plist-member segment :kind)))
                 segments)))

(defun e-session-aggregate--provider-anchor-stable-segments (fingerprints)
  "Return provider-anchor hard-identity segments from FINGERPRINTS."
  (let ((segments (and (e-session-aggregate-keyword-plist-p fingerprints)
                       (plist-get fingerprints :segments))))
    (cond
     ((null segments) nil)
     ((e-session-aggregate--provider-anchor-segment-list-p segments)
      (cl-remove-if
       #'e-session-aggregate--provider-anchor-dynamic-segment-p
       segments))
     (t (list :invalid-provider-anchor-segments)))))

(defun e-session-aggregate-provider-anchor-incompatibility-reason
    (store session-id anchor provider-id model fingerprints)
  "Return why ANCHOR is not compatible, or nil when compatible."
  (let* ((path (e-session-aggregate-current-path store session-id))
         (path-ids (mapcar (lambda (entry) (plist-get entry :id)) path))
         (anchor-id (plist-get anchor :id))
         (covered-entry-id (plist-get anchor :covered-entry-id))
         (anchor-fingerprints (plist-get anchor :fingerprints)))
    (cond
     ((not (eq (plist-get anchor :type) 'provider-anchor))
      'invalid-anchor-type)
     ((not (eq (plist-get anchor :provider-id) provider-id))
      'provider-mismatch)
     ((not (equal (plist-get anchor :model) model))
      'model-mismatch)
     ((not (equal (e-session-aggregate--provider-anchor-stable-segments
                   anchor-fingerprints)
                  (e-session-aggregate--provider-anchor-stable-segments
                   fingerprints)))
      'segment-fingerprint-mismatch)
     ((and (or (plist-member anchor-fingerprints :observation-delivery)
               (plist-member fingerprints :observation-delivery))
           (not (equal (plist-get anchor-fingerprints :observation-delivery)
                       (plist-get fingerprints :observation-delivery))))
      'observation-delivery-changed)
     ((and (or (plist-member anchor-fingerprints :current-state-fingerprint)
               (plist-member fingerprints :current-state-fingerprint))
           (not (equal
                 (plist-get anchor-fingerprints :current-state-fingerprint)
                 (plist-get fingerprints :current-state-fingerprint))))
      'current-state-changed)
     ((not (equal (plist-get anchor-fingerprints :active-layer-ids)
                  (plist-get fingerprints :active-layer-ids)))
      'active-layers-changed)
     ((not (equal (plist-get anchor-fingerprints :tools)
                  (plist-get fingerprints :tools)))
      'tools-changed)
     ((not (equal (plist-get anchor-fingerprints :reasoning)
                  (plist-get fingerprints :reasoning)))
      'reasoning-changed)
     ((not (equal (plist-get anchor-fingerprints :provider-options)
                  (plist-get fingerprints :provider-options)))
      'provider-options-changed)
     ((not (equal (plist-get anchor-fingerprints :compaction-boundary)
                  (plist-get fingerprints :compaction-boundary)))
      'compaction-boundary-changed)
     ((not (equal (plist-get anchor-fingerprints :lifetime-generation)
                  (plist-get fingerprints :lifetime-generation)))
      'context-generation-changed)
     ((and (or (plist-member anchor-fingerprints
                            :context-curation-revision-identity)
               (plist-member fingerprints
                            :context-curation-revision-identity))
           (not (equal
                 (plist-get anchor-fingerprints
                            :context-curation-revision-identity)
                 (plist-get fingerprints
                            :context-curation-revision-identity))))
      'context-curation-revision-changed)
     ((and (not (or (plist-member anchor-fingerprints :segments)
                    (plist-member anchor-fingerprints :active-layer-ids)
                    (plist-member anchor-fingerprints :tools)
                    (plist-member anchor-fingerprints :reasoning)
                    (plist-member anchor-fingerprints :provider-options)
                    (plist-member anchor-fingerprints :compaction-boundary)
                    (plist-member anchor-fingerprints :lifetime-generation)))
           (not (equal anchor-fingerprints fingerprints)))
      'fingerprint-mismatch)
     ((not (member anchor-id path-ids))
      'anchor-not-on-current-path)
     ((not (member covered-entry-id path-ids))
      'covered-entry-not-on-current-path)
     (t nil))))

(defun e-session-aggregate-provider-anchor-compatible-p
    (store session-id anchor provider-id model fingerprints)
  "Return non-nil when ANCHOR is compatible with current SESSION-ID state."
  (null
   (e-session-aggregate-provider-anchor-incompatibility-reason
    store session-id anchor provider-id model fingerprints)))

(defun e-session-aggregate-finalize-replayed-session (store session)
  "Restore replayed SESSION field ordering and derived metadata."
  (dolist (field e-session-aggregate--replay-list-fields)
    (plist-put session field (nreverse (plist-get session field))))
  (let ((journal (e-session-aggregate--board-journal store (plist-get session :id))))
    ;; Board replay appends through the journal tail, so its physical order is
    ;; already forward (unlike the prepend-based aggregate lists above).
    (setf (e-session-board-journal-tail journal)
          (e-session-aggregate--list-tail (e-session-board-journal-messages journal))))
  (e-session-aggregate-initialize-list-state session)
  (cl-remf session :entry-count)
  (plist-put session :loaded t)
  (e-session-aggregate--refresh-derived-fields store session)
  (e-session-aggregate--index-session-entries store session)
  session)

(defun e-session-aggregate--last-message-at (session)
  "Return SESSION's latest message timestamp, when it has messages."
  (when-let ((message (car (last (plist-get session :messages)))))
    (plist-get message :created-at)))

(defun e-session-aggregate--message-assistant-marker (message)
  "Return MESSAGE's stable assistant read marker."
  (or (plist-get message :id)
      (plist-get message :created-at)))

(defun e-session-aggregate--latest-assistant-marker (session)
  "Return SESSION's latest assistant message marker."
  (let (marker)
    (dolist (message (reverse (plist-get session :messages)))
      (when (and (not marker)
                 (eq (plist-get message :role) 'assistant))
        (setq marker (e-session-aggregate--message-assistant-marker message))))
    marker))

(defconst e-session-aggregate--invalid-board-association
  '(:invalid-board-association t)
  "Bounded internal marker for a present malformed board association.")

(define-error 'e-session-board-routing-invalid
  "Invalid board routing policy value")

(defconst e-session-aggregate--board-routing-policy-keys
  '(:participant-id :pickup-selector :observer-selector :default-tags
    :default-to)
  "Complete durable fields for one board participant routing policy.")

(defconst e-session-aggregate--board-routing-selector-keys
  '(:kind :activity-kind :to :author :subject-participant-id :attributes
    :tags :tags-all :tags-any)
  "JSON-shaped declarative selector keys admitted to routing policy.")

(defconst e-session-aggregate--board-routing-policy-node-budget 8192
  "Maximum structural nodes admitted by a board routing policy.

This is a domain budget for the declarative policy, not a nesting-depth cap.
It is deliberately independent of `e-board' so session replay can account for
the same bounded policy before encoding or mutation.  The representative
board policies are far below this ceiling.")

(defconst e-session-aggregate--board-routing-policy-byte-budget (* 64 1024)
  "Maximum UTF-8 bytes accounted for by a board routing policy.

The value follows the existing board metadata/attribute scale while keeping
the session admission boundary independent of the board implementation.")

(defconst e-session-aggregate--board-routing-policy-minimum-byte-budget 64
  "Smallest useful encoded budget for a complete routing policy.

Below this structural floor the policy can be rejected from the cheap
pre-encoding walk without invoking the codec's JSON escaping path.")

(defvar e-session-aggregate--board-routing-budget-visit-count 0
  "Number of nodes visited by the most recent routing-policy budget walk.
This is an internal diagnostic hook used by bounded-admission tests; callers
must not use it as policy state.")

(defun e-session-aggregate--board-routing-value-budget-valid-p
    (value &optional preserve-counter ignore-byte-budget)
  "Return non-nil when VALUE fits the routing-policy admission budget.

Account iteratively so hostile deep or cyclic values are rejected before
`json-encode' or session mutation.  In addition to string payloads, account
symbol names, numeric spellings, and a small canonical structural overhead.
This is a preflight estimate; the encoded policy receives an exact canonical
UTF-8 byte check after its reversible attribute encoding."
  (unless preserve-counter
    (setq e-session-aggregate--board-routing-budget-visit-count 0))
  (let ((pending (list (list :value value)))
        (visiting (make-hash-table :test 'eq))
        (nodes 0)
        (bytes 0)
        (valid t))
    (while (and valid pending)
      (let ((task (pop pending)))
        (if (eq (car task) :leave)
            (remhash (cdr task) visiting)
          (let ((current (cadr task)))
            (setq nodes (1+ nodes))
            (cl-incf e-session-aggregate--board-routing-budget-visit-count)
            (when (> nodes e-session-aggregate--board-routing-policy-node-budget)
              (setq valid nil))
            (cond
             ((stringp current)
              ;; Quotes are part of the JSON spelling; escapes are charged by
              ;; the exact post-encoding check below.
              (setq bytes (+ bytes 2 (string-bytes current))))
             ((numberp current)
              (setq bytes (+ bytes (string-bytes
                                    (number-to-string current)))))
             ((symbolp current)
              (setq bytes (+ bytes 2 (string-bytes (symbol-name current)))))
             ((null current)
              (setq bytes (+ bytes 4)))
             ((eq current t)
              (setq bytes (+ bytes 4)))
             ((or (vectorp current) (consp current))
              (when (gethash current visiting)
                (setq valid nil))
              (unless (gethash current visiting)
                (puthash current t visiting)
                (push (cons :leave current) pending)
                ;; Every container contributes delimiters.  Individual
                ;; separators are charged by each child below conservatively
                ;; through the node count and the final exact check.
                (setq bytes (+ bytes 2))
                (if (vectorp current)
                    (let* ((count (length current))
                           (remaining
                            (- e-session-aggregate--board-routing-policy-node-budget
                               nodes
                               (length pending))))
                      ;; Do not enqueue a caller-controlled vector wider than
                      ;; the remaining structural allowance.  Reject before
                      ;; allocating a task per element.
                      (if (> count remaining)
                          (setq valid nil)
                        (let ((index (1- count)))
                          (while (>= index 0)
                            (push (list :value (aref current index)) pending)
                            (setq index (1- index))))))
                  (push (list :value (cdr current)) pending)
                  (push (list :value (car current)) pending))))
             (t
              ;; Function objects, hash tables, buffers, markers, and other
              ;; process-local objects are not durable selector data.
              (setq valid nil)))
            (when (and (not ignore-byte-budget)
                       (> bytes e-session-aggregate--board-routing-policy-byte-budget))
              (setq valid nil))))))
    valid))

(defun e-session-aggregate--board-routing-json-value-p (value &optional visiting)
  "Return non-nil when VALUE is a finite JSON-shaped Lisp value.
Functions, hash tables, and cyclic values are deliberately not durable board
policy.  VISITING is the active identity set used to reject cycles without
accepting an executable selector predicate by accident."
  (let ((visiting (or visiting (make-hash-table :test 'eq)))
        (leave-marker (make-symbol "routing-leave"))
        (pending (list value))
        (valid t))
    ;; Keep this admission walk iterative.  A structural budget is useful
    ;; only when a hostile but finite nested value cannot exhaust the Lisp
    ;; evaluator before the budget is consulted.
    (while (and valid pending)
      (let ((current (pop pending)))
        (if (and (consp current) (eq (car current) leave-marker))
            (remhash (cdr current) visiting)
          (cond
           ((or (null current) (eq current t) (numberp current)
                (stringp current)) nil)
           ;; Symbols are data in selectors, even when their names are also
           ;; callable functions.  Only executable objects/forms are rejected.
           ((and (symbolp current) (not (keywordp current))) nil)
           ((functionp current) (setq valid nil))
           ((and (consp current) (memq (car current) '(lambda function)))
            (setq valid nil))
           ((or (vectorp current) (consp current))
            (if (gethash current visiting)
                (setq valid nil)
              (puthash current t visiting)
              (push (cons leave-marker current) pending)
              (cond
               ((vectorp current)
                (let ((index (1- (length current))))
                  (while (>= index 0)
                    (push (aref current index) pending)
                    (setq index (1- index)))))
               ((e-session-aggregate-keyword-plist-shape-p current)
                (let ((tail current))
                  (while tail
                    (pop tail)
                    (push (pop tail) pending))))
               ((proper-list-p current)
                (dolist (item (reverse current))
                  (push item pending)))
               ((or (keywordp (car current))
                    (stringp (car current)))
                (push (cdr current) pending))
               (t
                (setq valid nil)))))
           (t (setq valid nil))))))
    valid))

(defun e-session-aggregate--board-routing-json-byte-size (value)
  "Return the canonical UTF-8 JSON byte size of finite VALUE.
Container traversal is iterative so a policy below the structural node budget
cannot overflow the Lisp evaluator merely while measuring its representation.
Scalar values use Emacs's canonical JSON escaping; unsupported dotted pairs
signal `e-session-board-routing-invalid'."
  (let ((pending (list (list :value value)))
        (visiting (make-hash-table :test 'eq))
        (leave-marker (make-symbol "routing-json-leave"))
        (bytes 0))
    (while pending
      (let ((task (pop pending)))
        (if (eq (car task) leave-marker)
            (remhash (cdr task) visiting)
          (let ((current (cadr task)))
            (cond
             ((or (null current) (eq current t) (numberp current)
                  (stringp current) (symbolp current))
              (setq bytes (+ bytes
                             (string-bytes (json-encode current)))))
             ((or (vectorp current) (consp current))
              (when (gethash current visiting)
                (signal 'e-session-board-routing-invalid
                        (list "Cyclic routing value" current)))
              (puthash current t visiting)
              (push (cons leave-marker current) pending)
              (cond
               ((vectorp current)
                (let ((count (length current))
                      (index (1- (length current))))
                  (setq bytes (+ bytes 2 (max 0 (1- count))))
                  (while (>= index 0)
                    (push (list :value (aref current index)) pending)
                    (setq index (1- index)))))
               ((e-session-aggregate-keyword-plist-shape-p current)
                (let ((tail current)
                      (count 0))
                  (while tail
                    (setq count (1+ count))
                    (push (list :value (pop tail)) pending)
                    (push (list :value (pop tail)) pending))
                  (setq bytes (+ bytes 2 count (max 0 (1- count))))))
               ((proper-list-p current)
                (let ((count (length current)))
                  (setq bytes (+ bytes 2 (max 0 (1- count))))
                  (dolist (item (reverse current))
                    (push (list :value item) pending))))
               (t
                (signal 'e-session-board-routing-invalid
                        (list "Unsupported dotted routing value" current)))))
             (t
              (signal 'e-session-board-routing-invalid
                      (list "Unsupported routing value" current))))))))
    bytes))

(defun e-session-aggregate--board-routing-json-value-valid-p (value &optional budgeted-p)
  "Return non-nil when VALUE is finite and encodable as JSON."
  (and (or budgeted-p
           (e-session-aggregate--board-routing-value-budget-valid-p value))
       (e-session-aggregate--board-routing-json-value-p value)
       (condition-case nil
           (progn
             ;; Measure the canonical spelling without recursively encoding
             ;; the whole value.  Scalar `json-encode' calls retain exact
             ;; escaping while containers are traversed iteratively.
             (e-session-aggregate--board-routing-json-byte-size value)
             t)
         (error nil))))

(defun e-session-aggregate--board-routing-tag-list-valid-p (value)
  "Return non-nil when VALUE is a list of declarative tag atoms."
  (and (proper-list-p value)
       (cl-every
        (lambda (tag)
          (and (or (symbolp tag) (stringp tag))
               (not (and (symbolp tag) (keywordp tag)))))
        value)))

(defun e-session-aggregate--board-routing-selector-valid-p
    (selector &optional budgeted-p)
  "Return non-nil when SELECTOR is declarative and JSON-shaped."
  (and (or budgeted-p
           (e-session-aggregate--board-routing-value-budget-valid-p selector))
       (e-session-aggregate-keyword-plist-shape-p selector)
       (let ((tail selector)
             seen
             (valid t))
         (while (and valid tail)
          (let ((key (pop tail))
                (value (pop tail)))
             (setq valid
                   (and (memq key e-session-aggregate--board-routing-selector-keys)
                        (not (memq key seen))
                        (cond
                         ((memq key '(:tags :tags-all :tags-any))
                          (e-session-aggregate--board-routing-tag-list-valid-p value))
                         ((memq key '(:kind :activity-kind))
                          (or (symbolp value) (stringp value)))
                         ((memq key '(:to :author :subject-participant-id))
                          (stringp value))
                         ((eq key :attributes)
                          (and (e-board-selector-attributes-valid-p value)
                               (e-session-aggregate--board-routing-json-value-valid-p
                                value t)))
                         (t nil))))
             (push key seen)))
         valid)))

(defun e-session-aggregate--board-routing-policy-valid-p (policy)
  "Return non-nil when POLICY has exactly the complete durable shape."
  ;; Budget the complete caller value before any plist, selector, tag, or
  ;; attribute grammar walk.  This is the admission cutoff for rejected input
  ;; as well as accepted policy.
  (and (>= e-session-aggregate--board-routing-policy-byte-budget
           e-session-aggregate--board-routing-policy-minimum-byte-budget)
       (e-session-aggregate--board-routing-value-budget-valid-p
        policy nil t)
       (e-session-aggregate-keyword-plist-shape-p policy)
       (let ((tail policy)
             seen
             (valid t))
         (while (and valid tail)
           (let ((key (pop tail))
                 (value (pop tail)))
             (setq valid
                   (and (memq key e-session-aggregate--board-routing-policy-keys)
                        (not (memq key seen))
                        (cond
                         ((eq key :participant-id)
                          (and (stringp value)
                               (not (string-empty-p value))))
                         ((memq key '(:pickup-selector :observer-selector))
                          (e-session-aggregate--board-routing-selector-valid-p
                           value t))
                         ((eq key :default-tags)
                          (e-session-aggregate--board-routing-tag-list-valid-p value))
                         ((eq key :default-to)
                          (or (null value) (stringp value)))
                         (t nil))))
             (push key seen)))
         (and valid
              (= (length seen) (length e-session-aggregate--board-routing-policy-keys))
              (e-session-aggregate--board-routing-json-value-p policy)
              ;; Attribute selectors are tagged reversibly for persistence;
              ;; enforce the byte ceiling on that actual canonical form too.
              (condition-case nil
                  (<=
                   (e-session-aggregate--board-routing-json-byte-size
                   (e-session-codec-board-routing-policy-for-json policy))
                   e-session-aggregate--board-routing-policy-byte-budget)
                (error nil))))))

(defun e-session-aggregate--normalize-board-routing-selector (selector)
  "Return SELECTOR in the in-memory symbol form used by board matchers."
  (let ((selector (e-session-aggregate-board-routing-copy-value selector)))
    (dolist (key '(:kind :activity-kind))
      (when (stringp (plist-get selector key))
        (plist-put selector key (intern (plist-get selector key)))))
    (dolist (key '(:tags :tags-all :tags-any))
      (when (plist-member selector key)
        (plist-put selector key
                   (mapcar (lambda (tag)
                             (if (stringp tag) (intern tag) tag))
                           (plist-get selector key)))))
      selector))

(defun e-session-aggregate-board-routing-copy-value (value)
  "Deep-copy JSON-shaped board routing VALUE, including strings.
Use an explicit task stack so an admitted finite policy does not consume the
Lisp call stack merely while detaching nested selector data.  Cycles signal the
same invalid-policy condition as the admission walk."
  (let ((pending (list (list :value value)))
        (results nil)
        (visiting (make-hash-table :test 'eq))
        (leave-marker (make-symbol "routing-copy-leave")))
    (while pending
      (let ((task (pop pending)))
        (pcase (car task)
          (:leave
           (remhash (cadr task) visiting))
          (:assemble-vector
           (let (items)
             (dotimes (_ (cadr task))
               (push (pop results) items))
             (push (vconcat items) results)))
          (:assemble-cons
           (let ((cdr-value (pop results))
                 (car-value (pop results)))
             (push (cons car-value cdr-value) results)))
          (:value
           (let ((current (cadr task)))
             (cond
              ((stringp current)
               (push (copy-sequence current) results))
              ((or (null current) (eq current t) (numberp current)
                   (symbolp current))
               (push current results))
              ((or (vectorp current) (consp current))
               (when (gethash current visiting)
                 (signal 'e-session-board-routing-invalid
                         (list "Cyclic routing value" current)))
               (puthash current t visiting)
               (push (cons leave-marker current) pending)
               (push (list :assemble-vector (length current)) pending)
               (if (vectorp current)
                   (let ((index (1- (length current))))
                     (while (>= index 0)
                       (push (list :value (aref current index)) pending)
                       (setq index (1- index))))
                 ;; A cons is always copied as its car/cdr pair.  This avoids
                 ;; calling `proper-list-p' while traversing a deep value.
                 (pop pending)
                 (push (list :assemble-cons) pending)
                 (push (list :value (cdr current)) pending)
                 (push (list :value (car current)) pending)))
              (t
              (signal 'e-session-board-routing-invalid
                       (list "Unsupported routing value" current)))))))))
    (car results)))

(defun e-session-aggregate--normalize-board-routing-policy (policy)
  "Return detached POLICY with replayed tag/kind values normalized."
  (when policy
    (let ((policy (e-session-aggregate-board-routing-copy-value policy)))
      (dolist (key '(:pickup-selector :observer-selector))
        (plist-put policy key
                   (e-session-aggregate--normalize-board-routing-selector
                    (plist-get policy key))))
      (when (plist-member policy :default-tags)
        (plist-put policy :default-tags
                   (mapcar (lambda (tag)
                             (if (stringp tag) (intern tag) tag))
                           (plist-get policy :default-tags))))
      policy)))

(defun e-session-aggregate--board-association-keys-valid-p (association)
  "Return non-nil when ASSOCIATION contains only its bounded unique keys."
  (let ((tail association)
        seen
        (valid t))
    (while (and valid tail)
      (let ((key (pop tail)))
        (setq valid (and (memq key '(:board-id :principal :association-role
                                     :routing-policy))
                         (not (memq key seen))))
        (push key seen)
        (pop tail)))
    valid))

(defun e-session-aggregate--valid-board-association-p (association)
  "Return non-nil when ASSOCIATION has the complete durable board shape."
  (and (e-session-aggregate-keyword-plist-shape-p association)
       (e-session-aggregate--board-association-keys-valid-p association)
       (stringp (plist-get association :board-id))
       (stringp (plist-get association :principal))
       (or (not (plist-member association :association-role))
           (member (plist-get association :association-role)
                   '("owner" "participant")))
       (or (not (plist-member association :routing-policy))
           (e-session-aggregate--board-routing-policy-valid-p
            (plist-get association :routing-policy)))))

(defun e-session-aggregate--normalize-board-association (association)
  "Return a bounded normalized representation of present ASSOCIATION."
  (if (e-session-aggregate--valid-board-association-p association)
      (let ((normalized (copy-tree association)))
        (when (plist-member normalized :routing-policy)
          (plist-put normalized :routing-policy
                     (e-session-aggregate--normalize-board-routing-policy
                      (plist-get normalized :routing-policy))))
        normalized)
    (copy-tree e-session-aggregate--invalid-board-association)))

(defun e-session-aggregate-board-routing-policy (session)
  "Return SESSION's detached complete routing policy, or nil when absent."
  (when-let ((association (e-session-aggregate-board-association session)))
    (unless (e-session-aggregate-board-association-invalid-p association)
      (when (plist-member association :routing-policy)
        (e-session-aggregate-board-routing-copy-value
         (plist-get association :routing-policy))))))

(defun e-session-aggregate-board-routing-policy-valid-p (policy)
  "Return non-nil when POLICY is a complete durable routing policy."
  (e-session-aggregate--board-routing-policy-valid-p policy))

(defun e-session-aggregate-board-association-policy-present-p (association)
  "Return non-nil when ASSOCIATION explicitly carries a routing policy."
  (and (not (e-session-aggregate-board-association-invalid-p association))
       (plist-member association :routing-policy)))

(defun e-session-aggregate-projected-board-association (projection)
  "Return normalized board association from persisted PROJECTION.
The nested representation is authoritative when its key is present.  Flat
identity mirrors reconstruct only the canonical legacy shape in its absence."
  (if (plist-member projection :board-state)
      (let ((state (plist-get projection :board-state))
            (board-id (plist-get projection :board-id))
            (principal (plist-get projection :principal))
            (json-null-p (e-session-codec-json-null-p
                          (plist-get projection :board-state))))
        ;; Historical indexes projected all three keys as JSON null for an
        ;; ordinary non-board session.  Preserve only that exact absence shape;
        ;; omitted or non-null flat mirrors make a null nested value malformed.
        (if (and (plist-member projection :board-id)
                 (plist-member projection :principal)
                 json-null-p
                 (or (null board-id)
                     (e-session-codec-json-null-p board-id))
                 (or (null principal)
                     (e-session-codec-json-null-p principal)))
            nil
          (e-session-aggregate--normalize-board-association
           (if json-null-p nil state))))
    (let ((board-id (plist-get projection :board-id))
          (principal (plist-get projection :principal)))
      (if (and (null board-id) (null principal))
          nil
        (e-session-aggregate--normalize-board-association
         (list :board-id board-id :principal principal))))))

(defun e-session-aggregate-board-association (session)
  "Return SESSION's normalized whole board association, or nil when absent."
  (cond
   ((plist-member session :board-session-state)
    (e-session-aggregate--normalize-board-association
     (plist-get session :board-session-state)))
   ((plist-member session :board-state)
    (e-session-aggregate--normalize-board-association
     (plist-get session :board-state)))
   (t nil)))

(defun e-session-aggregate-board-association-invalid-p (association)
  "Return non-nil when ASSOCIATION is the bounded malformed-state marker."
  (equal association e-session-aggregate--invalid-board-association))

(defun e-session-aggregate--session-index-entry (store session)
  "Return public index metadata for SESSION in STORE."
  (e-session-aggregate--refresh-file-field store session)
  (let* ((state (e-session-aggregate-board-association session))
         (entry
          (list :id (plist-get session :id)
                :name (plist-get session :name)
                :summary (plist-get session :summary)
                :metadata (plist-get session :metadata)
                :title (e-session-aggregate--display-title-for-session session)
                :message-count (or (plist-get session :message-count) 0)
                :created-at (plist-get session :created-at)
                :updated-at (plist-get session :updated-at)
                :updated-seq (plist-get session :updated-seq)
                :last-message-at (or (plist-get session :last-message-at)
                                     (e-session-aggregate--last-message-at session))
                :latest-assistant-marker
                (or (plist-get session :latest-assistant-marker)
                    (e-session-aggregate--latest-assistant-marker session))
                :board-id (plist-get state :board-id)
                :principal (plist-get state :principal)
                :file (plist-get session :file)
                :loaded (plist-get session :loaded))))
    (when (plist-member session :board-session-state)
      (setq entry (plist-put entry :board-state state)))
    entry))

(defun e-session-aggregate--normalize-turn-options (options)
  "Return canonical session turn OPTIONS."
  (let (normalized)
    (when-let ((model (plist-get options :model)))
      (when (and (stringp model) (not (string-empty-p (string-trim model))))
        (setq normalized
              (plist-put normalized :model (string-trim model)))))
    (when-let ((effort (plist-get options :reasoning-effort)))
      (when (and (stringp effort) (not (string-empty-p (string-trim effort))))
        (setq normalized
              (plist-put normalized :reasoning-effort (string-trim effort)))))
    (when (and (plist-member options :prompt-cache-default)
               (memq (plist-get options :prompt-cache-default) '(nil t)))
      (setq normalized
            (plist-put normalized
                       :prompt-cache-default
                       (plist-get options :prompt-cache-default))))
    (when-let ((cache-key (plist-get options :prompt-cache-key)))
      (when (and (stringp cache-key)
                 (not (string-empty-p (string-trim cache-key))))
        (setq normalized
              (plist-put normalized
                         :prompt-cache-key
                         (string-trim cache-key)))))
    (when-let ((retention (plist-get options :prompt-cache-retention)))
      (when (and (stringp retention)
                 (not (string-empty-p (string-trim retention))))
        (setq normalized
              (plist-put normalized
                         :prompt-cache-retention
                         (string-trim retention)))))
    normalized))

(defun e-session-aggregate--message-with-created-at (message timestamp)
  "Return semantic MESSAGE with TIMESTAMP when no creation time is present.
Wire-level role/display normalization belongs to `e-session-codec'; this
aggregate helper only supplies the domain's required timestamp default."
  (let ((normalized (copy-sequence message)))
    (unless (plist-member normalized :created-at)
      (plist-put normalized :created-at timestamp))
    normalized))

(defun e-session-aggregate-peek-session (store session-id)
  "Return SESSION-ID metadata without forcing transcript replay."
  (or (gethash session-id (e-session-store-sessions store))
      (signal 'e-session-missing (list session-id))))

(cl-defun e-session-aggregate-create (store &key id metadata defer-persistence)
  "Create an in-memory session aggregate in STORE with ID and METADATA.
The optional DEFER-PERSISTENCE flag marks a private admission reservation for
the application service.  This owner never publishes durable records; the
composition root supplies the storage adapter after the semantic mutation has
been accepted."
  (setq id (or id (e-session-aggregate--generate-id)))
  (when (gethash id (e-session-store-sessions store))
    (signal 'e-session-duplicate (list id)))
  (setq metadata (e-session-aggregate--validate-metadata
                  (e-session-aggregate-normalize-metadata-for-replay metadata)))
  (let* ((timestamp (e-session-aggregate--timestamp))
         (session (list :id id
                        :metadata metadata
                        :session-events nil
                        :messages nil
                        :board-output-sequence 0
                        :board-activity-sequence 0
                        :activity-events nil
                        :branch-summaries nil
                        :current-branch nil
                        :compactions nil
                        :provider-anchors nil
                        :process-reports nil
                        :context-generations nil
                        :context-promotions nil
                        :context-curation-packages nil
                        :turn-options nil
                        :created-at timestamp
                        :updated-at timestamp
                        :name (plist-get metadata :name)
                        :loaded t)))
    (e-session-aggregate-initialize-list-state session)
    (let ((root (e-session-aggregate--append-session-event
                 session
                 'session-created
                 timestamp
                 (list :metadata metadata))))
      (plist-put session :root-event-id (plist-get root :id)))
    (when defer-persistence
      (plist-put session :admission-pending t))
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    (puthash id session (e-session-store-sessions store))
    (e-session-aggregate--index-session-entries store session)
    session))

(cl-defun e-session-aggregate-create-board-admission
    (store &key id metadata principal board-id association-role routing-policy)
  "Reserve one board participant session before durable publication.
Validate the complete board association and all durable records before placing
the private reservation in STORE.  The owner must call
`e-session-aggregate-commit-board-admission' after runtime attachment
succeeds, or `e-session-aggregate-abort-created' on failure.  No journal,
queue, controller outbox,
or index entry is published by this function."
  (let ((session
         (e-session-aggregate-create store :id id :metadata metadata
                           :defer-persistence t)))
    (condition-case error
        (progn
          (unless (and (stringp board-id) (not (string-empty-p board-id))
                       (stringp principal) (not (string-empty-p principal)))
            (signal 'e-session-error
                    (list "Invalid board admission identity"
                          board-id principal)))
          (when (and association-role
                     (not (member association-role '("owner" "participant"
                                                     owner participant))))
            (signal 'e-session-error
                    (list "Invalid board association role" association-role)))
          (when (and routing-policy
                     (not (e-session-aggregate--board-routing-policy-valid-p
                           routing-policy)))
            (signal 'e-session-board-routing-invalid
                    (list "Invalid board routing policy" routing-policy)))
          (let ((board-state (list :board-id (copy-sequence board-id)
                                   :principal (copy-sequence principal))))
            (when association-role
              (plist-put board-state :association-role
                         (if (symbolp association-role)
                             (symbol-name association-role)
                           association-role)))
            (when routing-policy
              (plist-put
               board-state :routing-policy
               (e-session-aggregate--normalize-board-routing-policy routing-policy)))
            (plist-put session :board-session-state board-state)
            (let* ((session-id (plist-get session :id))
                   (state-record
                    (list :type "board-session-state"
                          :session-id session-id
                          :board-state board-state
                          :board-id board-id
                          :principal principal
                          :board-output-sequence
                          (or (plist-get session :board-output-sequence) 0)
                          :board-activity-sequence
                          (or (plist-get session :board-activity-sequence) 0)))
                   (records
                    (list
                     (list :type "session"
                           :session-id session-id
                           :id (e-session-aggregate--root-event-id session)
                           :timestamp (plist-get session :created-at)
                           :created-at (plist-get session :created-at)
                           :updated-at (plist-get session :updated-at)
                           :metadata (plist-get session :metadata))
                     state-record)))
              (plist-put session :admission-records records)
              (e-session-aggregate--index-session-entries store session)
              session)))
      (error
       (ignore-errors
         (e-session-aggregate-abort-created store (plist-get session :id)))
       (signal (car error) (cdr error))))))


(defun e-session-aggregate-commit-board-admission (store session-id)
  "Complete one previously reserved board admission in the aggregate.
The returned session retains its detached semantic admission records for the
application service to submit as one storage transaction.  No physical write
or queue mutation occurs here."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (records (and (plist-get session :admission-pending)
                       (plist-get session :admission-records))))
    (unless (and (plist-get session :admission-pending)
                 (listp records) (= (length records) 2))
      (signal 'e-session-error
              (list "Session has no pending board admission" session-id)))
    (cl-remf session :admission-pending)
    (cl-remf session :admission-records)
    (e-session-aggregate--index-session-entries store session)
    session))

(defun e-session-aggregate-abort-created (store session-id)
  "Remove a newly created SESSION-ID after an owning service failure.

This is intentionally limited to application-service rollback: callers must
  only use it for a session that has just been created and has not been exposed
as a restorable participant.  It removes the in-memory/index/journal state and
any queued direct-store writes, rather than appending a user-visible tombstone
  for an object that never completed admission."
  (when-let* ((session (gethash session-id (e-session-store-sessions store)))
              (_ (plist-get session :loaded)))
    (remhash session-id (e-session-store-entry-indexes store))
    (remhash session-id (e-session-store-board-journals store))
    (remhash session-id (e-session-store-sessions store))
    t))

(defun e-session-aggregate--board-journal (store session-id)
  "Return STORE's private board journal for SESSION-ID."
  (or (gethash session-id (e-session-store-board-journals store))
      (puthash session-id
               (e-session-aggregate--board-journal-create)
               (e-session-store-board-journals store))))

(defun e-session-aggregate--clear-board-journal (store session-id)
  "Remove STORE's private board journal for SESSION-ID."
  (remhash session-id (e-session-store-board-journals store)))

(defun e-session-aggregate--freeze-board-value (value)
  "Return VALUE detached from mutable board-journal input.
Signal `e-session-board-message-cycle' for cyclic conses, vectors, and hash
 tables.  The copy is deliberately iterative: board envelopes are bounded by
 policy, but their aggregate list can still be deep enough to exhaust the
 evaluator when a checkpoint is assembled."
  (let ((pending (list (list :value value)))
        (results nil)
        (visiting (make-hash-table :test 'eq)))
    (while pending
      (let ((task (pop pending)))
        (pcase (car task)
          (:leave
           (remhash (cadr task) visiting))
          (:assemble-cons
           (let ((cdr-value (pop results))
                 (car-value (pop results)))
             (push (cons car-value cdr-value) results)))
          (:assemble-vector
           (let (items)
             (dotimes (_ (cadr task))
               (push (pop results) items))
             (push (vconcat items) results)))
          (:assemble-hash
           (let ((copy (make-hash-table :test (cadr task)
                                        :size (caddr task))))
             (dotimes (_ (cadddr task))
               (let ((item (pop results))
                     (key (pop results)))
                 (puthash key item copy)))
             (push copy results)))
          (:value
           (let ((current (cadr task)))
             (cond
              ((stringp current)
               (push (copy-sequence current) results))
              ((or (null current) (eq current t) (numberp current)
                   (symbolp current))
               (push current results))
              ((consp current)
               (when (gethash current visiting)
                 (signal 'e-session-board-message-cycle (list 'cons)))
               (puthash current t visiting)
               (push (list :leave current) pending)
               (push (list :assemble-cons) pending)
               (push (list :value (cdr current)) pending)
               (push (list :value (car current)) pending))
              ((vectorp current)
               (when (gethash current visiting)
                 (signal 'e-session-board-message-cycle (list 'vector)))
               (puthash current t visiting)
               (push (list :leave current) pending)
               (push (list :assemble-vector (length current)) pending)
               (let ((index (1- (length current))))
                 (while (>= index 0)
                   (push (list :value (aref current index)) pending)
                   (setq index (1- index)))))
              ((hash-table-p current)
               (when (gethash current visiting)
                 (signal 'e-session-board-message-cycle (list 'hash-table)))
               (puthash current t visiting)
               (let (pairs)
                 (maphash (lambda (key item)
                            (push (cons key item) pairs))
                          current)
                 (push (list :leave current) pending)
                 (push (list :assemble-hash (hash-table-test current)
                             (hash-table-size current) (length pairs))
                       pending)
                 (dolist (pair pairs)
                   (push (list :value (cdr pair)) pending)
                   (push (list :value (car pair)) pending))))
              (t
               (push current results))))))))
    (car results)))

(defun e-session-aggregate-board-messages (store session-id)
  "Return SESSION-ID's durable board envelopes in board order."
  (e-session-aggregate-get-live store session-id)
  (e-session-aggregate--freeze-board-value
   (e-session-board-journal-messages
    (e-session-aggregate--board-journal store session-id))))

(defun e-session-aggregate--canonical-board-record-type (record-type)
  "Return supported RECORD-TYPE in the board journal's representation.
Nil means an ordinary board message."
  (pcase record-type
    (`nil nil)
    ((or 'processing-chain "processing-chain") 'processing-chain)
    ((or 'processing-result "processing-result") 'processing-result)
    (_
     (signal 'e-session-board-message-invalid-record-type
             (list record-type)))))

(defun e-session-aggregate--normalize-board-message (message)
  "Normalize durable board MESSAGE after input or JSONL replay."
  (dolist (field '(:kind :mode :activity-kind :routing-state
                   :unrouted-reason :record-type :outcome :failure-policy))
    (when-let ((value (plist-get message field)))
      (when (stringp value)
        (plist-put message field (intern value)))))
  ;; Keep the historical detached representation: callers can rely on the
  ;; tags slot being present even when the envelope carried no tags.
  (plist-put message :tags
             (mapcar (lambda (tag)
                       (if (stringp tag) (intern tag) tag))
                     (plist-get message :tags)))
  (when-let ((attributes (plist-get message :attributes)))
    (when-let ((status (plist-get attributes :status)))
      (when (stringp status)
        (plist-put attributes :status (intern status)))))
  message)

(defun e-session-aggregate-board-message-identity (message)
  "Return the durable journal identity for board MESSAGE.
Processing records have a record type, while ordinary board messages occupy the
untyped board-message namespace.  The pair prevents equal raw ids from
silently replacing records from another namespace."
  (cons (or (e-session-aggregate--canonical-board-record-type
             (plist-get message :record-type))
            'board-message)
        (plist-get message :id)))

(defun e-session-aggregate--existing-board-message (journal message)
  "Return MESSAGE's retained duplicate, or signal for a typed conflict."
  (let* ((identity (e-session-aggregate-board-message-identity message))
         (existing (gethash identity (e-session-board-journal-id-index journal))))
    (when (and existing
               (plist-get message :record-type)
               (not (equal existing message)))
      (signal 'e-session-board-message-conflict
              (list identity existing message)))
    existing))

(defun e-session-aggregate-append-board-message (store session-id message)
  "Append one immutable board MESSAGE envelope to SESSION-ID's board log."
  (e-session-aggregate-get-live store session-id)
  (let* ((journal (e-session-aggregate--board-journal store session-id))
         (message (e-session-aggregate--freeze-board-value message))
         (record-type
          (e-session-aggregate--canonical-board-record-type
           (plist-get message :record-type)))
         (_ (when record-type
              (plist-put message :record-type record-type)))
         (existing (e-session-aggregate--existing-board-message journal message)))
    (unless existing
      (puthash (e-session-aggregate-board-message-identity message) message
               (e-session-board-journal-id-index journal))
      (let ((cell (list message)))
        (if-let ((tail (e-session-board-journal-tail journal)))
            (setcdr tail cell)
          (setf (e-session-board-journal-messages journal) cell))
        (setf (e-session-board-journal-tail journal) cell))
      (let ((session (e-session-aggregate-get-live store session-id)))
        (e-session-aggregate--touch store session (e-session-aggregate--timestamp)))
      ;; The application service persists this detached envelope.
      nil)
    (e-session-aggregate--freeze-board-value (or existing message))))

(defun e-session-aggregate-clear-board-messages (store session-id)
  "Clear SESSION-ID's durable board log and derived identity index."
  (let ((journal (e-session-aggregate--board-journal store session-id))
        (session (e-session-aggregate-get-live store session-id)))
    (setf (e-session-board-journal-messages journal) nil
          (e-session-board-journal-tail journal) nil
          (e-session-board-journal-id-index journal) (make-hash-table :test 'equal))
    (e-session-aggregate--touch store session (e-session-aggregate--timestamp))
    nil))

(defun e-session-aggregate-declare-board-state
    (store session-id principal board-id &optional association-role
           routing-policy)
  "Persist SESSION-ID's board identity, role, and ROUTING-POLICY.
ASSOCIATION-ROLE is either `owner' or `participant'.  Nil omits the role for
replay-compatible callers that create the legacy board identity shape.
ROUTING-POLICY, when non-nil, must contain every key in
`e-session-aggregate--board-routing-policy-keys'; a present policy is never
partially
persisted."
  (unless (member association-role '(nil "owner" "participant"))
    (error "Invalid board association role: %S" association-role))
  (unless (and (stringp session-id) (stringp principal) (stringp board-id))
    (error "Board association identity must be strings: %S %S %S"
           session-id principal board-id))
  (when (and routing-policy
             (not (e-session-aggregate--board-routing-policy-valid-p routing-policy)))
    (error "Invalid board routing policy: %S" routing-policy))
  (let* ((session (e-session-aggregate-get-live store session-id))
         (had-state (plist-member session :board-session-state))
         (old-state (plist-get session :board-session-state))
         (board-state (list :board-id (copy-sequence board-id)
                            :principal (copy-sequence principal))))
    (when association-role
      (setq board-state
            (plist-put board-state :association-role association-role)))
    (when routing-policy
      (setq board-state
            (plist-put
             board-state :routing-policy
             (e-session-aggregate--normalize-board-routing-policy routing-policy))))
    (let ((record
           (list :type "board-session-state" :session-id session-id
                 :board-state board-state :board-id board-id
                 :principal principal
                 :board-output-sequence
                 (or (plist-get session :board-output-sequence) 0)
                 :board-activity-sequence
                 (or (plist-get session :board-activity-sequence) 0))))
      (ignore had-state old-state record)
      (plist-put session :board-session-state (copy-tree board-state))
      ;; Durable publication is coordinated by the application service after
      ;; this semantic transition succeeds.
      (copy-tree board-state))))

(defun e-session-aggregate--fork-message-seed (message)
  "Return MESSAGE stripped of source-session identity for fork replay.
The fork rebuilds a fresh linear parent chain, so durable identity fields
(`:id', `:parent-id') and the source turn grouping (`:turn-id') are dropped;
the re-append path mints new ones anchored on the fork's own head."
  (let ((seed (copy-sequence message)))
    (dolist (key '(:id :parent-id :turn-id))
      (setq seed (e-session-aggregate--plist-remove seed key)))
    seed))

(cl-defun e-session-aggregate-fork (store session-id &key at metadata name)
  "Fork SESSION-ID in STORE into a new independent session and return it.

The fork is seeded with a snapshot of the source's current-path messages up to
AT (a head entry id; defaults to the source's current head), re-appended in
order so the fork is a clean linear continuation.  Context-bearing durable
metadata (canvas attachment, project root, capability state) and the source's
turn options (model/effort) are copied so the fork resumes with the same
working context.  Provider anchors and compaction structure are intentionally
not copied: the fork starts without provider cache and re-compacts on its own.

The source session is left untouched; new turns append only to the fork.
METADATA overrides merge onto the copied metadata; NAME, when given, sets the
fork's session name (otherwise it inherits the source name)."
  (let* ((source (e-session-aggregate-get-live store session-id))
         (head-id (or at (plist-get source :current-head-id)))
         (path (e-session-aggregate-current-path store session-id head-id))
         (portable-projection
          (e-session-aggregate-context-lifetime-projection store session-id head-id))
         (source-generation (plist-get portable-projection :generation))
         (source-checkpoint
          (and source-generation
               (e-context-lifetime-generation-checkpoint source-generation)))
         (promotion-messages
          (plist-get portable-projection :promotion-messages))
         ;; A deliberate non-empty portable generation is the only fork seed
         ;; that may replace the ordinary message copy.  A generation with
         ;; active promotions but no checkpoint still needs a portable seed so
         ;; v2/v3 durable items are not lost when the branch is selected.
         (portable-checkpoint
          (when (and source-generation
                     (or source-checkpoint promotion-messages))
            (e-context-lifetime-portable-checkpoint
             (append
              (copy-tree source-checkpoint)
              (mapcar #'e-context-lifetime-portable-message
                      (plist-get portable-projection :durable-tail))
              (mapcar #'e-context-lifetime-portable-message
                      promotion-messages))
             t)))
         (messages (seq-filter (lambda (entry)
                                 (eq (plist-get entry :type) 'message))
                               path))
         (base-metadata (copy-sequence (plist-get source :metadata)))
         (merged-metadata (e-session-aggregate--merge-metadata base-metadata metadata))
         (merged-metadata (if name
                              (plist-put merged-metadata :name name)
                            merged-metadata))
         (turn-options (plist-get source :turn-options))
         (fork (e-session-aggregate-create store :metadata merged-metadata)))
    (if portable-checkpoint
        (let (last-seed)
          ;; Keep the portable projection as ordinary fork messages so a
          ;; lifetime-disabled reader still sees the same semantic context.
          ;; The fresh generation then covers those seed messages, avoiding a
          ;; duplicate enabled projection and preventing covered source history
          ;; from returning.
          (dolist (message portable-checkpoint)
            (setq last-seed
                  (e-session-aggregate-append-message
                   store
                   (plist-get fork :id)
                   (e-session-aggregate--fork-message-seed message))))
          (e-session-aggregate-append-context-generation
           store
           (plist-get fork :id)
           (e-context-lifetime-generation-create
            :id (format "generation:fork:%s" (e-session-aggregate-generate-ulid))
            :checkpoint portable-checkpoint
            :covered-session-boundary (plist-get last-seed :id))))
      (dolist (message messages)
        (e-session-aggregate-append-message store (plist-get fork :id)
                                  (e-session-aggregate--fork-message-seed message))))
    (when turn-options
      (e-session-aggregate-set-turn-options store (plist-get fork :id) turn-options))
    (e-session-aggregate-get store (plist-get fork :id))))

(defun e-session-aggregate-get-live (store session-id)
  "Return the mutable live SESSION-ID state from STORE."
  (e-session-aggregate-peek-session store session-id))

(defun e-session-aggregate-get (store session-id)
  "Return SESSION-ID's mutable generic session state from STORE.
Board journal state is owned privately by STORE and is available only through
its dedicated board journal accessors."
  (e-session-aggregate-get-live store session-id))

(defun e-session-aggregate-messages (store session-id)
  "Return messages for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :messages)))

(defun e-session-aggregate-activity-events (store session-id)
  "Return durable activity events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :activity-events)))

(defun e-session-aggregate-latest-token-usage-event (store session-id)
  "Return the latest durable token usage event for SESSION-ID in STORE."
  (plist-get (e-session-aggregate-get-live store session-id) :latest-token-usage-event))

(defun e-session-aggregate-session-events (store session-id)
  "Return durable session events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :session-events)))

(defun e-session-aggregate-compactions (store session-id)
  "Return compaction records for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :compactions)))

(defun e-session-aggregate-provider-anchors (store session-id)
  "Return provider anchor records for SESSION-ID in STORE in insertion order."
  (copy-sequence
   (plist-get (e-session-aggregate-get-live store session-id) :provider-anchors)))

(defun e-session-aggregate-context-generations (store session-id)
  "Return context generation entries for SESSION-ID in insertion order."
  (copy-tree
   (plist-get (e-session-aggregate-get-live store session-id) :context-generations)))

(defun e-session-aggregate-context-promotions (store session-id)
  "Return detached context promotion entries for SESSION-ID in path order.

Promotion components carried by a curation package are projected as virtual
entries so this compatibility accessor has the same :context-record shape as
standalone promotion entries.  The package remains the only persisted entry."
  (let (promotions)
    (dolist (entry (e-session-aggregate-current-path store session-id)
                   (nreverse promotions))
      (dolist (component (e-session-aggregate--context-entry-components entry))
        (when (eq (car component) 'context-promotion)
          (let ((record (cdr component)))
            (push
             (if (eq (plist-get entry :type) 'context-promotion)
                 (copy-tree entry)
               (list :type 'context-promotion
                     :id (plist-get record :id)
                     :parent-id (plist-get entry :parent-id)
                     :created-at (plist-get entry :created-at)
                     :context-record (copy-tree record)))
             promotions)))))))

(defun e-session-aggregate-context-erasures (store session-id)
  "Return detached version-1 erasure records for SESSION-ID in path order.

The returned records are detached and contain only identity/provenance.  This
accessor is an audit surface; model-facing projection uses the selected-head
query below instead of scanning every session entry itself."
  (let (records)
    (dolist (entry (e-session-aggregate-current-path store session-id)
                   (nreverse records))
      (dolist (component (e-session-aggregate--context-entry-components entry))
        (when (eq (car component) 'context-erasure)
          (push (e-context-lifetime-curation-erasure-record
                 (cdr component))
                records))))))

(defun e-session-aggregate-erased-tool-call-ids
    (store session-id &optional selected-head-id)
  "Return canonical erased tool-call IDs on SESSION-ID's selected head.

SELECTED-HEAD-ID is an optional durable entry id; nil selects the session's
current head.  The query walks only the parent path ending at that head,
preserves path/source order, and removes duplicate identities.  It is
intentionally read-only and does not inspect any Feature 90 receipt or
provider state."
  (unless (or (null selected-head-id) (stringp selected-head-id))
    (signal 'e-session-error
            (list "Invalid selected context head" selected-head-id)))
  (when (and selected-head-id
             (not (e-session-aggregate-entry-by-id store session-id selected-head-id)))
    (signal 'e-session-error
            (list "Unknown selected context head" selected-head-id)))
  (let ((head-id selected-head-id)
        (seen (make-hash-table :test 'equal))
        ids)
    (dolist (entry (e-session-aggregate-current-path store session-id head-id)
                   (nreverse ids))
      (dolist (component (e-session-aggregate--context-entry-components entry))
        (when (eq (car component) 'context-erasure)
          (dolist (tool-call-id
                   (e-context-lifetime-curation-erasure-tool-call-ids
                    (cdr component)))
            (unless (gethash tool-call-id seen)
              (puthash tool-call-id t seen)
              (push tool-call-id ids))))))))

(defun e-session-aggregate-context-curations (store session-id)
  "Return version-3 curation records for SESSION-ID in insertion order.

The persisted entry family remains `context-promotion' for compatibility, so
this accessor selects the new record version without exposing v2 promotion
records as if they were v3 records."
  (delq nil
        (mapcar
         (lambda (component)
           (let ((record (cdr component)))
             (when (and (eq (car component) 'context-promotion)
                        (equal (plist-get record :record-version)
                               e-context-lifetime-curation-record-version))
               (copy-tree record))))
         (cl-mapcan #'e-session-aggregate--context-entry-components
                    (e-session-aggregate-current-path store session-id)))))

(defun e-session-aggregate-context-lifetime-current-generation
    (store session-id &optional head-id)
  "Return the latest narrowed semantic generation on SESSION-ID's path.

Legacy frame/generation journal entries are intentionally not reconstructed as
runtime frames.  Only the current v2 generation codec participates in this
projection."
  (when-let ((entry (e-session-aggregate--context-active-generation
                     store session-id head-id)))
    (condition-case error
        (e-context-lifetime-generation-from-record
         (e-session-aggregate-context-record entry))
      (e-context-lifetime-invalid-record
       (signal 'e-session-error
               (list "Invalid current context generation" session-id error))))))

(defun e-session-aggregate--context-lifetime-durable-message (entry)
  "Return model-facing durable MESSAGE from canonical session ENTRY."
  (when (eq (plist-get entry :type) 'message)
    (let ((role (plist-get entry :role)))
      (unless (memq role '(tool-call tool))
        (let ((message (copy-tree entry)))
          ;; Provider replay items travel with a paired tool observation.  They
          ;; are not durable model context after that consuming request.
          (cl-remf message :id)
          (cl-remf message :parent-id)
          (cl-remf message :created-at)
          (cl-remf message :turn-id)
          (cl-remf message :type)
          (cl-remf message :durability-state)
          (when-let ((metadata (plist-get message :metadata)))
            (setq metadata (copy-tree metadata))
            (cl-remf metadata :provider-replay-items)
            (if metadata
                (plist-put message :metadata metadata)
              (cl-remf message :metadata)))
          message)))))

(defun e-session-aggregate-context-lifetime-durable-message (entry)
  "Return the portable durable projection of canonical message ENTRY.

This narrow consumer-facing wrapper keeps compaction and fork ownership from
duplicating the session transcript/body filtering rules."
  (e-session-aggregate--context-lifetime-durable-message entry))

(defun e-session-aggregate--checkpoint-path-suffix (store session-id)
  "Return the current path suffix selected by the latest valid compaction.
This is a domain path calculation; checkpoint serialization remains owned by
the catalog owner."
  (let ((path (e-session-aggregate-current-path store session-id)))
    (if-let* ((compaction
               (e-session-aggregate-latest-valid-compaction store session-id))
              (boundary-id (plist-get compaction :first-kept-entry-id))
              (boundary (e-session-aggregate-entry-by-id
                         store session-id boundary-id))
              (suffix (member boundary path)))
        suffix
      path)))

(defun e-session-aggregate--path-after-boundary (path boundary-id)
  "Return PATH strictly after BOUNDARY-ID, or PATH when it is absent."
  (if-let ((boundary (seq-find
                      (lambda (entry)
                        (equal (plist-get entry :id) boundary-id))
                      path)))
      (cdr (member boundary path))
    path))

(defun e-session-aggregate-context-lifetime-projection
    (store session-id &optional head-id)
  "Return canonical inputs for the semantic later-request projection.

The session transcript/current branch is the sole durable body source.  The
result contains no runtime frame or observation body; callers may add a fresh
consumer-bound frame at request construction time."
  (let* ((path (if head-id
                   (e-session-aggregate-current-path store session-id head-id)
                 (e-session-aggregate--checkpoint-path-suffix store session-id)))
         (generation (e-session-aggregate-context-lifetime-current-generation
                      store session-id head-id))
         (generation-path
          ;; The first opt-in generation is an identity boundary with no
          ;; checkpoint; keep the ordinary transcript until a deliberate
          ;; portable compaction supplies a real checkpoint.  A non-empty
          ;; checkpoint is the replacement boundary that covers its prefix.
          (if (and generation
                   (e-context-lifetime-generation-checkpoint generation))
              (e-session-aggregate--path-after-boundary
               path
               (e-context-lifetime-generation-covered-session-boundary
                generation))
            path))
         (messages (delq nil
                         (mapcar #'e-session-aggregate--context-lifetime-durable-message
                                 generation-path)))
         (generation-id (and generation
                             (e-context-lifetime-generation-id generation)))
         ;; Promotions before a deliberate portable boundary are absorbed into
         ;; its checkpoint.  Historical records remain in the audit journal but
         ;; only facts owned by the active generation are eligible here.
         (promotions nil)
         (curations nil)
         (promotion-message-entry-groups nil)
         (promotion-frontier nil))
    (dolist (entry path)
      (dolist (component (e-session-aggregate--context-entry-components entry))
        (when (and (eq (car component) 'context-promotion)
                   generation-id)
          (condition-case error
              (let ((record (cdr component)))
                (when (equal (plist-get record :generation-id)
                             generation-id)
                  (if (equal (plist-get record :record-version)
                             e-context-lifetime-curation-record-version)
                      (let ((curation
                             (e-context-lifetime-curation-from-record record)))
                        (push curation curations)
                        (push
                         (mapcar
                          (lambda (message)
                            (list :kind 'v3 :message
                                  (copy-tree message)))
                          (e-context-lifetime-curation-messages curation))
                         promotion-message-entry-groups))
                    (let ((promotion
                           (e-context-lifetime-promotion-from-record record)))
                      (push promotion promotions)
                      (push
                       (mapcar
                        (lambda (message)
                          (list :kind 'v2 :message
                                (copy-tree message)))
                        (e-context-lifetime-promotion-fact-messages
                         (list promotion)))
                       promotion-message-entry-groups)))
                  (push (plist-get record :id) promotion-frontier)))
            (e-context-lifetime-invalid-record
             (signal 'e-session-error
                     (list "Invalid current context promotion"
                           session-id error)))))))
    (let* ((promotion-message-entries
            (if promotion-message-entry-groups
                (apply #'append (nreverse promotion-message-entry-groups))
              nil))
           (promotion-messages
            (mapcar (lambda (entry) (plist-get entry :message))
                    promotion-message-entries)))
      (list :generation generation
            :durable-tail messages
            :promotions (nreverse promotions)
            :curations (nreverse curations)
            :promotion-messages promotion-messages
            :promotion-message-entries
            (copy-tree promotion-message-entries)
            :promotion-frontier (nreverse promotion-frontier)))))

(defun e-session-aggregate-process-reports (store session-id)
  "Return process reports for SESSION-ID in STORE in insertion order."
  (copy-sequence
   (plist-get (e-session-aggregate-get-live store session-id) :process-reports)))

(cl-defun e-session-aggregate-latest-compatible-provider-anchor
    (store session-id provider-id &key model fingerprints)
  "Return latest provider anchor compatible with SESSION-ID current path."
  (seq-find
   (lambda (anchor)
     (e-session-aggregate-provider-anchor-compatible-p
      store session-id anchor provider-id model fingerprints))
   (reverse (e-session-aggregate-provider-anchors store session-id))))

(defun e-session-aggregate-turn-options (store session-id)
  "Return session-scoped turn options for SESSION-ID in STORE."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :turn-options)))

(defun e-session-aggregate--replace-metadata (store session-id metadata)
  "Replace SESSION-ID METADATA in STORE after validation."
  (let* ((metadata (e-session-aggregate--validate-metadata metadata))
         (session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :metadata metadata))))
    (e-session-aggregate--index-entry store session-id event)
    (plist-put session :metadata metadata)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    metadata))

(defun e-session-aggregate-set-metadata (store session-id metadata)
  "Replace SESSION-ID METADATA in STORE.
This compatibility path validates that every key has a durable state schema.
New code should prefer the narrower typed metadata helpers."
  (e-session-aggregate--replace-metadata store session-id metadata))

(defun e-session-aggregate--merge-metadata (metadata updates)
  "Return METADATA with UPDATES applied."
  (let ((metadata (copy-sequence metadata)))
    (while (consp updates)
      (let ((key (pop updates)))
        (when (consp updates)
          (setq metadata (plist-put metadata key (pop updates))))))
    metadata))

(defun e-session-aggregate-set-session-config (store session-id config)
  "Merge durable session CONFIG into SESSION-ID metadata."
  (e-session-aggregate--validate-metadata-class config 'session-config)
  (let* ((session (e-session-aggregate-get-live store session-id))
         (metadata (e-session-aggregate--merge-metadata
                    (plist-get session :metadata)
                    config)))
    (e-session-aggregate--replace-metadata store session-id metadata)))

(defun e-session-aggregate-metadata-context-references (metadata owner)
  "Return current-state references for OWNER from session METADATA.
This transcript-free reader accepts metadata from either a live session or a
session catalog entry."
  (let* ((references (plist-get metadata :context-references))
         (owner-key (e-session-aggregate--metadata-owner-key owner)))
    (copy-tree
     (e-session-aggregate--metadata-public-value
      (plist-get references owner-key)))))

(defun e-session-aggregate-context-references (store session-id owner)
  "Return current-state references for OWNER in SESSION-ID."
  (e-session-aggregate-metadata-context-references
   (plist-get (e-session-aggregate-get-live store session-id) :metadata)
   owner))

(defun e-session-aggregate-set-context-references (store session-id owner references)
  "Set durable current-state REFERENCES for OWNER in SESSION-ID."
  (let* ((owner-key (e-session-aggregate--metadata-owner-key owner))
         (session (e-session-aggregate-get-live store session-id))
         (metadata (copy-sequence (plist-get session :metadata)))
         (all-references (copy-sequence
                          (plist-get metadata :context-references))))
    (setq all-references
          (plist-put all-references
                     owner-key
                     (e-session-aggregate--metadata-json-array-safe-value references)))
    (e-session-aggregate--replace-metadata
     store
     session-id
     (plist-put metadata :context-references all-references))
    references))

(defun e-session-aggregate-set-context-reference (store session-id key reference)
  "Set durable current-state REFERENCE metadata KEY for SESSION-ID."
  (e-session-aggregate--validate-metadata-class (list key reference)
                                      'current-state-reference)
  (let* ((session (e-session-aggregate-get-live store session-id))
         (metadata (e-session-aggregate--merge-metadata
                    (plist-get session :metadata)
                    (list key reference))))
    (e-session-aggregate--replace-metadata store session-id metadata)))

(defun e-session-aggregate-capability-state (store session-id capability-id)
  "Return durable capability state for CAPABILITY-ID in SESSION-ID."
  (let* ((metadata (plist-get (e-session-aggregate-get-live store session-id) :metadata))
         (state (plist-get metadata :capability-state))
         (owner-key (e-session-aggregate--metadata-owner-key capability-id)))
    (copy-tree
     (e-session-aggregate--metadata-public-value
      (plist-get state owner-key)))))

(cl-defun e-session-aggregate-set-capability-state
    (store session-id capability-id state &key version)
  "Set durable capability STATE for CAPABILITY-ID in SESSION-ID."
  (let* ((owner-key (e-session-aggregate--metadata-owner-key capability-id))
         (session (e-session-aggregate-get-live store session-id))
         (metadata (copy-sequence (plist-get session :metadata)))
         (all-state (copy-sequence (plist-get metadata :capability-state)))
         (entry (if version
                    (list :version version :state state)
                  state)))
    (setq all-state (plist-put all-state owner-key entry))
    (e-session-aggregate--replace-metadata
     store
     session-id
     (plist-put metadata :capability-state all-state))
    entry))

(defun e-session-aggregate-set-turn-options (store session-id options)
  "Replace SESSION-ID turn OPTIONS in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (turn-options (e-session-aggregate--normalize-turn-options options))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :turn-options turn-options))))
    (e-session-aggregate--index-entry store session-id event)
    (plist-put session :turn-options turn-options)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    turn-options))

(defun e-session-aggregate-append-message (store session-id message)
  "Append MESSAGE to SESSION-ID in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
          (message (e-session-aggregate--normalize-entry-from-record
                    session
                    'message
                    (e-session-aggregate--message-with-created-at
                     message timestamp)
                    timestamp)))
    (when (and (eq (plist-get message :role) 'assistant)
               (not (plist-member message :board-output-sequence)))
      (let ((sequence
             (1+ (or (plist-get session :board-output-sequence)
                     (cl-loop for entry in (plist-get session :messages)
                              maximize (or (plist-get entry :board-output-sequence) 0))
                     0))))
        (setq message (plist-put message :board-output-sequence sequence))
        (plist-put session :board-output-sequence sequence)))
    (e-session-aggregate--append-list-item session :messages message)
    (e-session-aggregate--index-entry store session-id message)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--update-message-derived-fields-on-append store session message)
    message))

(defun e-session-aggregate--message-by-id (store session-id message-id)
  "Return SESSION-ID's message with MESSAGE-ID in STORE, or nil.
Prefers the entry-id index; falls back to a scan of `:messages' so a message
appended before an index rebuild is still found."
  (or (let ((entry (gethash message-id (e-session-aggregate--entry-index store session-id))))
        (and entry (eq (plist-get entry :type) 'message) entry))
      (seq-find (lambda (message)
                  (equal (plist-get message :id) message-id))
                (plist-get (e-session-aggregate-get-live store session-id) :messages))))

(defun e-session-aggregate-set-message-display (store session-id message-id display)
  "Set DISPLAY on SESSION-ID's message MESSAGE-ID in STORE and persist it.
DISPLAY is a display disposition symbol (e.g. `hidden'); nil clears it back to
the default visible state.  Mutates the in-memory message in place and appends
a durable `message-display' record so the change replays on reload.  Returns
the updated message, or nil when no such message exists."
  (when-let ((message (e-session-aggregate--message-by-id store session-id message-id)))
    (let ((timestamp (e-session-aggregate--timestamp)))
      (if display
          (plist-put message :display display)
        (cl-remf message :display))
      (e-session-aggregate--touch store (e-session-aggregate-get-live store session-id) timestamp)
      message)))

(defun e-session-aggregate--append-activity-event-entry
    (store session-id turn-id event-type payload entry-id write-index
           checkpoint-retain)
  "Append activity EVENT-TYPE with optional durable ENTRY-ID.

ENTRY-ID is reserved for the one audit-only context-curation response control
entry whose identity must be shared with its prepared record.  Ordinary
activity events continue to mint their own ids."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--normalize-entry-from-record
                 session
                 'activity-event
                 (append (when entry-id (list :id entry-id))
                         (when checkpoint-retain
                           (list :checkpoint-retain t))
                         (list :turn-id turn-id
                               :event-type event-type
                               :payload (copy-tree payload)
                               :created-at timestamp))
                 timestamp)))
    (unless (plist-member event :board-activity-sequence)
      (let ((sequence
             (1+ (or (plist-get session :board-activity-sequence)
                     (cl-loop for entry in (plist-get session :activity-events)
                              maximize (or (plist-get entry :board-activity-sequence) 0))
                     0))))
        (setq event (plist-put event :board-activity-sequence sequence))
        (plist-put session :board-activity-sequence sequence)))
    (e-session-aggregate--append-list-item session :activity-events event)
    (e-session-aggregate--update-activity-derived-fields session event)
    (e-session-aggregate--index-entry store session-id event)
    (e-session-aggregate--touch store session timestamp)
    (ignore write-index)
    event))

(cl-defun e-session-aggregate-append-activity-event
    (store session-id turn-id event-type payload &key (write-index t)
           checkpoint-retain)
  "Append durable activity EVENT-TYPE to STORE for SESSION-ID and TURN-ID.

When CHECKPOINT-RETAIN is non-nil, persist a generic retention marker on the
activity entry.  Checkpoint construction may pin a bounded marked tail from
the complete selected path; it does not interpret PAYLOAD."
  (e-session-aggregate--append-activity-event-entry
   store session-id turn-id event-type payload nil write-index
   checkpoint-retain))

(cl-defun e-session-aggregate-append-context-curation-response
    (store session-id turn-id response-entry-id &key (write-index t))
  "Append the audit-only control entry for a reserved curation response.

RESPONSE-ENTRY-ID is the durable identity already allocated for the provider
response.  The entry is kept in activity history rather than transcript
messages, so it is resolvable by id but cannot enter ordinary model context."
  (unless (and (stringp response-entry-id)
               (not (string-empty-p response-entry-id)))
    (signal 'e-session-error
            (list "Context curation response requires an entry id"
                  response-entry-id)))
  (e-session-aggregate--append-activity-event-entry
   store session-id turn-id 'context-curation-response
   (list :response-entry-id response-entry-id)
   response-entry-id write-index nil))

(defun e-session-aggregate-append-process-report (store session-id report)
  "Append out-of-band process REPORT to SESSION-ID in STORE.
Process reports are durable session entries but are not transcript messages and
therefore never enter backend context."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (report (e-session-aggregate--normalize-entry-from-record
                  session
                  'process-report
                  (copy-sequence report)
                  timestamp)))
    (e-session-aggregate--append-list-item session :process-reports report)
    (e-session-aggregate--index-entry store session-id report)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-file-field store session)
    report))

(cl-defun e-session-aggregate-append-branch-summary
    (store session-id branch-id summary &key metadata)
  "Append BRANCH-ID SUMMARY metadata to SESSION-ID in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (record (e-session-aggregate--normalize-entry-from-record
                  session
                  'branch-summary
                  (list :branch-id branch-id
                        :summary summary
                        :metadata metadata
                        :created-at timestamp)
                  timestamp)))
    (e-session-aggregate--append-list-item session :branch-summaries record)
    (e-session-aggregate--index-entry store session-id record)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-file-field store session)
    record))

(cl-defun e-session-aggregate-append-compaction
    (store session-id summary &key branch-id range first-kept-entry-id
           tokens-before tokens-kept metadata)
  "Append compaction SUMMARY for SESSION-ID in STORE.
BRANCH-ID, RANGE, FIRST-KEPT-ENTRY-ID, and METADATA describe the compacted
source when available."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (record (e-session-aggregate--normalize-entry-from-record
                  session
                  'compaction
                  (list :summary summary
                        :branch-id branch-id
                        :range range
                        :first-kept-entry-id first-kept-entry-id
                        :tokens-before tokens-before
                        :tokens-kept tokens-kept
                        :metadata metadata
                        :created-at timestamp)
                  timestamp)))
    (e-session-aggregate--append-list-item session :compactions record)
    (e-session-aggregate--index-entry store session-id record)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-file-field store session)
    record))

(cl-defun e-session-aggregate-append-provider-anchor
    (store session-id provider-id &key model covered-entry-id fingerprints
           metadata)
  "Append opaque PROVIDER-ID anchor metadata to SESSION-ID in STORE.
COVERED-ENTRY-ID identifies the latest transcript entry covered by the
provider-owned anchor.  FINGERPRINTS and METADATA are opaque to session core."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (record (e-session-aggregate--normalize-entry-from-record
                  session
                  'provider-anchor
                  (list :provider-id provider-id
                        :model model
                        :covered-entry-id covered-entry-id
                        :fingerprints fingerprints
                        :metadata metadata
                        :created-at timestamp)
                  timestamp)))
    (e-session-aggregate--append-list-item session :provider-anchors record)
    (e-session-aggregate--index-entry store session-id record)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-file-field store session)
    record))


(defun e-session-aggregate--context-current-path (store session-id &optional head-id)
  "Return the current canonical path for context ownership validation."
  (let* ((session (gethash session-id (e-session-store-sessions store)))
         (entries (and session
                       (append (plist-get session :session-events)
                               (plist-get session :messages)
                               (plist-get session :activity-events)
                               (plist-get session :branch-summaries)
                               (plist-get session :compactions)
                               (plist-get session :provider-anchors)
                               (plist-get session :process-reports)
                               (plist-get session :context-generations)
                               (plist-get session :context-promotions)
                               (plist-get session :context-curation-packages))))
         (by-id (make-hash-table :test #'equal))
         (visited (make-hash-table :test #'equal))
         path
         (head-id (or head-id
                      (and session (plist-get session :current-head-id)))))
    (dolist (entry entries)
      (puthash (plist-get entry :id) entry by-id))
    (while head-id
      (when (gethash head-id visited)
        (signal 'e-session-error
                (list "Context session path contains a cycle"
                      session-id head-id)))
      (puthash head-id t visited)
      (let ((entry (gethash head-id by-id)))
        (unless entry
          (signal 'e-session-error
                  (list "Context session path has unresolved head or parent"
                        session-id head-id)))
        (push entry path)
        (setq head-id (plist-get entry :parent-id))))
  path))

(defun e-session-aggregate--context-record-duplicate-key-p (record)
  "Return non-nil when semantic context RECORD repeats a keyword."
  (when (e-session-aggregate-keyword-plist-p record)
    (let ((tail record)
          seen
          duplicate)
      (while tail
        (let ((key (pop tail)))
          (pop tail)
          (when (memq key seen)
            (setq duplicate t))
          (push key seen)))
      duplicate)))

(defun e-session-aggregate--normalize-context-record-for-replay (type record)
  "Normalize the semantic TYPE marker in decoded context RECORD."
  (let ((copy (copy-tree record)))
    (when (and (e-session-aggregate-keyword-plist-p copy)
               (stringp (plist-get copy :type))
               (equal (plist-get copy :type) (symbol-name type)))
      (plist-put copy :type type))
    copy))

(defun e-session-aggregate--normalize-context-record
    (type record &optional expected-record-version read-legacy-p)
  "Validate and normalize semantic context RECORD for TYPE.
The aggregate consumes semantic values only; JSON conversion and durable
record decoding remain in `e-session-codec'."
  (when (e-session-aggregate--context-record-duplicate-key-p record)
    (signal 'e-session-error
            (list "Context lifetime record has duplicate fields" type)))
  (condition-case error
      (let* ((record-version (and (e-session-aggregate-keyword-plist-p record)
                                  (plist-get record :record-version)))
             (decoded
              (cond
               ((eq type 'context-generation)
                (e-context-lifetime-generation-from-record record))
               ((eq type 'context-erasure)
                (e-context-lifetime-curation-erasure-from-record record))
               ((equal record-version
                       e-context-lifetime-curation-record-version)
                (e-context-lifetime-curation-from-record record))
               ;; Version-2 promotion records remain readable history.  The
               ;; strict compatibility decoder validates their old shape;
               ;; production writes continue to use the version-3 curation
               ;; record and never manufacture this representation.
               ((and read-legacy-p
                     (equal record-version e-context-lifetime-record-version))
                (e-context-lifetime-promotion-from-record record))
               (t
                (signal 'e-session-error
                        (list "Context lifetime writes require a supported version"
                              record-version)))))
             (normalized
              (cond
               ((eq type 'context-generation)
                (e-context-lifetime-generation-record decoded))
               ((eq type 'context-erasure)
                (e-context-lifetime-curation-erasure-record decoded))
               ((and read-legacy-p
                     (equal record-version e-context-lifetime-record-version))
                (copy-tree record))
               (t (e-context-lifetime-curation-record decoded)))))
        (when (and expected-record-version
                   (not (equal expected-record-version
                               (plist-get normalized :record-version))))
          (signal 'e-session-error
                  (list "Context record version is not accepted"
                        expected-record-version
                        (plist-get normalized :record-version))))
        normalized)
    (e-context-lifetime-invalid-record
     (signal 'e-session-error
             (list "Invalid context lifetime record" type error)))))

(defun e-session-aggregate--context-active-generation (store session-id &optional head-id)
  "Return the latest context generation on SESSION-ID's current path."
  (seq-find (lambda (entry)
              (eq (plist-get entry :type) 'context-generation))
            (reverse (e-session-aggregate--context-current-path store session-id head-id))))

(defun e-session-aggregate--validate-context-entry-ownership
    (store session-id type context-record)
  "Reject CONTEXT-RECORD when its generation owner is not active."
  (let* ((generation-entry
          (e-session-aggregate--context-active-generation store session-id))
         (generation-record
          (and generation-entry
               (e-session-aggregate-context-record generation-entry)))
         (generation-id (and generation-record
                             (plist-get generation-record :id))))
    (when (and (memq type '(context-promotion context-erasure))
               (not (equal (plist-get context-record :generation-id)
                           generation-id)))
      (signal 'e-session-error
              (list "Context lifetime record has no active generation owner"
                    session-id
                    (plist-get context-record :generation-id)
                    generation-id)))
    context-record))

(defun e-session-aggregate--normalize-context-curation-package (package)
  "Return detached validated semantic components from curation PACKAGE.

This is the one Feature 88 package shape owned by the session boundary.  It is
deliberately not a general transaction abstraction: the only allowed fields
are the optional version-3 promotion and version-1 erasure components."
  (unless (e-session-aggregate-keyword-plist-p package)
    (signal 'e-session-error
            (list "Context curation package must be a keyword plist" package)))
  (when (e-session-aggregate--context-record-duplicate-key-p package)
    (signal 'e-session-error
            (list "Context curation package has duplicate fields")))
  (let ((keys nil)
        (tail package))
    (while tail
      (push (pop tail) keys)
      (pop tail))
    (setq keys (nreverse keys))
    (unless (and (= (length keys) 2)
                 (memq :promotion keys)
                 (memq :erasure keys))
      (signal 'e-session-error
              (list "Context curation package has unsupported fields" keys))))
  (let* ((promotion-raw (plist-get package :promotion))
         (erasure-raw (plist-get package :erasure))
         (promotion
          (and promotion-raw
               (e-session-aggregate--normalize-context-record
                'context-promotion promotion-raw
                e-context-lifetime-curation-record-version)))
         (erasure
          (and erasure-raw
               (e-session-aggregate--normalize-context-record
                'context-erasure erasure-raw
                e-context-lifetime-curation-erasure-record-version))))
    (unless (or promotion erasure)
      (signal 'e-session-error
              (list "Context curation package has no semantic component")))
    (when (and promotion erasure)
      (dolist (field '(:frame-id :generation-id :consumer-request-id
                       :response-entry-id))
        (unless (equal (plist-get promotion field)
                       (plist-get erasure field))
          (signal 'e-session-error
                  (list "Context curation package component identity mismatch"
                        field)))))
    (list :promotion promotion :erasure erasure)))

(defun e-session-aggregate--context-curation-package-id (session-id package)
  "Return the deterministic identity for semantic curation PACKAGE."
  (format "context-curation-package:%s"
          (substring
           (secure-hash 'sha256
                        (prin1-to-string
                         (list session-id
                               (plist-get package :promotion)
                               (plist-get package :erasure))))
           0 32)))

(defun e-session-aggregate--context-curation-package-record
    (session-id package package-id parent-id timestamp)
  "Return one JSON-safe session record for curation PACKAGE."
  (list :type "context-curation-package"
        :session-id session-id
        :id package-id
        :parent-id parent-id
        :timestamp timestamp
        :promotion (and (plist-get package :promotion)
                        (copy-tree (plist-get package :promotion)))
        :erasure (and (plist-get package :erasure)
                      (copy-tree (plist-get package :erasure)))))

(defun e-session-aggregate--prepare-context-curation-package
    (store session-id package)
  "Prepare one persistent curation PACKAGE without mutating STORE.

The returned value contains detached normalized components and one indexed
session entry.  The package is the commit unit; its optional promotion and
erasure components are never represented as independently persisted entries."
  (let* ((package (e-session-aggregate--normalize-context-curation-package package))
         (session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (base-parent (plist-get session :current-head-id))
         (_promotion
          (when-let ((record (plist-get package :promotion)))
            (e-session-aggregate--validate-context-entry-ownership
             store session-id 'context-promotion record)))
         (_erasure
          (when-let ((record (plist-get package :erasure)))
            (e-session-aggregate--validate-context-entry-ownership
             store session-id 'context-erasure record)))
         (package-id (e-session-aggregate--context-curation-package-id
                      session-id package))
         (record (e-session-aggregate--context-curation-package-record
                  session-id package package-id base-parent timestamp))
         (entry
          ;; Preparation must not move the live head.  The normalized entry
          ;; receives the current parent, then the commit path advances the
          ;; head only after the durable package record is accepted.
          (e-session-aggregate--entry-with-identity
           session 'context-curation-package
           (list :promotion (plist-get package :promotion)
                 :erasure (plist-get package :erasure))
           timestamp record)))
    (list :package package
          :entry entry
          :package-id package-id
          :timestamp timestamp
          :record record)))

(defun e-session-aggregate--context-curation-package-existing-state
    (store session-id prepared)
  "Return an exact selected-path package for PREPARED, or signal a conflict.

An exact retry may run after the package advanced the session head (for
example, when its separate audit append failed), so the proposed parent is
not part of the idempotency comparison.  A package on an inactive sibling is
not a retry for the selected path and remains a conflict."
  (let* ((package-id (plist-get prepared :package-id))
         (existing (gethash package-id
                            (e-session-aggregate--entry-index store session-id)))
         (selected-path
          (and existing (e-session-aggregate-current-path store session-id)))
         (selected-p
          (and existing
               (seq-some
                (lambda (entry)
                  (equal (plist-get entry :id) package-id))
                selected-path))))
    (cond
     ((null existing) nil)
     ((and selected-p
           (eq (plist-get existing :type) 'context-curation-package)
           (equal (plist-get existing :promotion)
                  (plist-get (plist-get prepared :package) :promotion))
           (equal (plist-get existing :erasure)
                  (plist-get (plist-get prepared :package) :erasure)))
      existing)
     (t
      (signal 'e-session-error
              (list "Context curation package identity conflict"
                    package-id))))))

(defun e-session-aggregate--replay-context-curation-package
    (store session-id record)
  "Replay one validated curation PACKAGE RECORD atomically.

All semantic components are normalized and ownership-checked before the one
package entry is installed.  A repeated exact package is idempotent; an entry
with the same identity but different canonical components is rejected."
  (unless (and (e-session-aggregate-keyword-plist-p record)
               (not (e-session-aggregate--context-record-duplicate-key-p record)))
    (signal 'e-session-error
            (list "Invalid context curation package record")))
  (let ((keys nil)
        (tail record))
    (while tail
      (push (pop tail) keys)
      (pop tail))
    (setq keys (nreverse keys))
    (unless (equal keys '(:type :session-id :id :parent-id :timestamp
                          :promotion :erasure))
      (signal 'e-session-error
              (list "Invalid context curation package fields" keys))))
  ;; Replay is already inside the owning session's load transaction.  Calling
  ;; `e-session-aggregate-get-live' here would see the not-yet-finalized replay session
  ;; as unloaded and recursively restart the same journal replay.
  (let* ((session (gethash session-id (e-session-store-sessions store)))
         (package
          (e-session-aggregate--normalize-context-curation-package
           (list :promotion
                 (e-session-aggregate--normalize-context-record-for-replay
                  'context-promotion (plist-get record :promotion))
                 :erasure
                 (e-session-aggregate--normalize-context-record-for-replay
                  'context-erasure (plist-get record :erasure)))))
         (package-id (plist-get record :id))
         (expected-id (e-session-aggregate--context-curation-package-id
                       session-id package))
         ;; Do not use the public lazy-loading lookup while a session is being
         ;; replayed.  Its fallback walks the live entry lists through
         ;; `e-session-aggregate-get-live', which would recursively restart this same
         ;; journal load before the replay session has been finalized.
         (existing (gethash package-id
                            (e-session-aggregate--entry-index store session-id))))
    (unless (and (equal (plist-get record :type)
                        "context-curation-package")
                 (equal (plist-get record :session-id) session-id)
                 (stringp package-id)
                 (stringp (plist-get record :timestamp))
                 (equal package-id expected-id))
      (signal 'e-session-error
              (list "Invalid context curation package identity" package-id)))
    (dolist (component (list (cons 'context-promotion
                                   (plist-get package :promotion))
                             (cons 'context-erasure
                                   (plist-get package :erasure))))
      (when (cdr component)
        (e-session-aggregate--validate-context-entry-ownership
         store session-id (car component) (cdr component))))
    (cond
     (existing
      (unless (and (eq (plist-get existing :type)
                       'context-curation-package)
                   (equal (plist-get existing :parent-id)
                          (plist-get record :parent-id))
                   (equal (plist-get existing :promotion)
                          (plist-get package :promotion))
                   (equal (plist-get existing :erasure)
                          (plist-get package :erasure)))
        (signal 'e-session-error
                (list "Context curation package replay conflict" package-id)))
      nil)
     (t
      (let ((entry
             (e-session-aggregate--normalize-entry-from-record
              session 'context-curation-package
              (list :promotion (plist-get package :promotion)
                    :erasure (plist-get package :erasure))
              (plist-get record :timestamp)
              record)))
        (e-session-aggregate--prepend-replayed-item
         session :context-curation-packages entry)
        (e-session-aggregate--index-entry store session-id entry)
        (e-session-aggregate--touch store session (plist-get record :timestamp)))))))

(cl-defun e-session-aggregate--append-context-entry
    (store session-id type field context-record &key (write-index t)
           expected-record-version)
  "Append narrowed provider-neutral CONTEXT-RECORD under TYPE."
  (unless (memq type e-session-aggregate--context-lifetime-entry-types)
    (signal 'e-session-error (list "Unknown context lifetime entry" type)))
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (context-record (e-session-aggregate--normalize-context-record
                          type context-record expected-record-version))
         (_ownership
          (e-session-aggregate--validate-context-entry-ownership
           store session-id type context-record))
         (entry
          (e-session-aggregate--normalize-entry-from-record
           session type (list :context-record context-record) timestamp)))
    (e-session-aggregate--append-list-item session field entry)
    (e-session-aggregate--index-entry store session-id entry)
    (e-session-aggregate--touch store session timestamp)
    (ignore write-index)
    entry))

(cl-defun e-session-aggregate-append-context-generation
    (store session-id generation &key (write-index t))
  "Append semantic GENERATION and return its durable session entry."
  (e-session-aggregate--append-context-entry
   store session-id 'context-generation :context-generations
   (if (e-context-lifetime-generation-p generation)
       (e-context-lifetime-generation-record generation)
     generation)
   :write-index write-index))

(cl-defun e-session-aggregate-append-context-curation-package
    (store session-id package &key (write-index t))
  "Atomically append semantic curation PACKAGE for SESSION-ID.

PACKAGE is the narrow pure value produced by
`e-context-lifetime-prepare-curation-disposition'.  Its optional promotion and
erasure components are validated and staged on a detached session before one
`context-curation-package' persistence operation is submitted.  The package
is therefore one direct write, one queued record, or one controller outbox
command; it is never split into independent component appends.  A repeated
package with the same component identities is idempotent.  Audit response
controls remain a separate activity append owned by the harness.

The persistence operation precedes live-session mutation, so synchronous write,
queue, or controller submission errors leave both semantic projections absent
and leave frame consumption to the caller."
  (let* ((prepared (e-session-aggregate--prepare-context-curation-package
                    store session-id package))
         (existing (e-session-aggregate--context-curation-package-existing-state
                    store session-id prepared))
         (entry (plist-get prepared :entry))
         (session (e-session-aggregate-get-live store session-id))
         (timestamp (plist-get prepared :timestamp)))
    (if existing
        (list :id (plist-get prepared :package-id)
              :package (plist-get prepared :package)
              :entry existing
              :promotion (plist-get existing :promotion)
              :erasure (plist-get existing :erasure)
              :already-present t)
      (e-session-aggregate--append-list-item
       session :context-curation-packages entry)
      (e-session-aggregate--index-entry store session-id entry)
      (e-session-aggregate--advance-head session entry)
      (e-session-aggregate--touch store session timestamp)
      (ignore write-index)
      (list :id (plist-get prepared :package-id)
            :package (plist-get prepared :package)
            :record (plist-get prepared :record)
            :entry entry
            :promotion (plist-get entry :promotion)
            :erasure (plist-get entry :erasure)))))

(defun e-session-aggregate-set-current-branch (store session-id branch-id)
  "Set SESSION-ID current branch cursor to BRANCH-ID in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--append-session-event
                 session
                 'current-branch
                 timestamp
                 (list :branch-id branch-id))))
    (e-session-aggregate--index-entry store session-id event)
    (plist-put session :current-branch branch-id)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    branch-id))

(defun e-session-aggregate-clear-messages (store session-id)
  "Clear all messages for SESSION-ID in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (root-id (e-session-aggregate--root-event-id session))
         (event nil))
    (e-session-aggregate--replace-list-field session :messages nil)
    (e-session-aggregate--replace-list-field session :activity-events nil)
    (e-session-aggregate--replace-list-field session :provider-anchors nil)
    (plist-put session :latest-token-usage-event nil)
    (e-session-aggregate--clear-message-derived-fields store session)
    (e-session-aggregate--clear-entry-index store session-id)
    (dolist (entry (plist-get session :session-events))
      (e-session-aggregate--index-entry store session-id entry))
    (dolist (entry (plist-get session :branch-summaries))
      (e-session-aggregate--index-entry store session-id entry))
    (dolist (entry (plist-get session :compactions))
      (e-session-aggregate--index-entry store session-id entry))
    (dolist (entry (plist-get session :provider-anchors))
      (e-session-aggregate--index-entry store session-id entry))
    (plist-put session :current-head-id root-id)
    (setq event
          (e-session-aggregate--append-session-event
           session
           'messages-cleared
           timestamp
           (list :parent-id root-id)))
    (e-session-aggregate--index-entry store session-id event)
    (e-session-aggregate--touch store session timestamp)
    event))

(defun e-session-aggregate-rename (store session-id name)
  "Rename SESSION-ID in STORE to NAME."
  (when (string-empty-p (string-trim (or name "")))
    (user-error "Session name must not be empty"))
  (let* ((session (e-session-aggregate-get-live store session-id))
         (name (string-trim name))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :name name))))
    (e-session-aggregate--index-entry store session-id event)
    (plist-put session :name name)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    session))

(defun e-session-aggregate-display-title (store session-id)
  "Return display title for SESSION-ID in STORE."
  (e-session-aggregate--display-title-for-session
   (e-session-aggregate-peek-session store session-id)))

(defun e-session-aggregate-root-p (session)
  "Return non-nil when SESSION is a user-facing root session.
Subagent and task-queue sessions remain directly addressable through their own
surfaces, but do not belong in general session pickers."
  (let ((metadata (plist-get session :metadata)))
    (not (or (plist-get metadata :parent-session-id)
             (plist-get metadata :subagent-role)
             (plist-get metadata :task-queue-task-id)))))

(defun e-session-aggregate-list (store)
  "Return STORE sessions sorted by most recent message."
  (let (sessions)
    (maphash (lambda (_id session)
               (push (e-session-aggregate--session-index-entry store session) sessions))
             (e-session-store-sessions store))
    (sort sessions
          (lambda (left right)
            (let ((left-time (or (plist-get left :last-message-at)
                                 (plist-get left :created-at)
                                 ""))
                  (right-time (or (plist-get right :last-message-at)
                                  (plist-get right :created-at)
                                  ""))
                  (left-seq (or (plist-get left :updated-seq) 0))
                  (right-seq (or (plist-get right :updated-seq) 0)))
              (or (string> left-time right-time)
                  (and (string= left-time right-time)
                       (> left-seq right-seq))))))))

(defun e-session-aggregate-list-roots (store)
  "Return user-facing root sessions in STORE, newest first."
  (cl-remove-if-not #'e-session-aggregate-root-p (e-session-aggregate-list store)))


(defun e-session-aggregate-context-record (entry)
  "Return the provider-neutral context record carried by ENTRY."
  (copy-tree (plist-get entry :context-record)))

(defun e-session-aggregate--context-record-sequence (record key)
  "Return RECORD's KEY value as a detached logical-id sequence."
  (let ((value (plist-get record key)))
    (cond
     ((null value) nil)
     ((vectorp value) (append value nil))
     ((listp value) (copy-sequence value))
     (t (list value)))))

(defun e-session-aggregate--update-activity-derived-fields (session event)
  "Update derived SESSION fields for appended activity EVENT.

The replay application path calls this semantic aggregate operation after the
codec has detached the wire value; the codec does not know the aggregate's
plist representation."
  (when (eq (plist-get event :event-type) 'token-usage)
    (plist-put session :latest-token-usage-event event))
  event)

(defun e-session-aggregate--context-entry-components (entry)
  "Return semantic context components carried by durable ENTRY."
  (pcase (plist-get entry :type)
    ('context-promotion
     (list (cons 'context-promotion
                 (e-session-aggregate-context-record entry))))
    ('context-curation-package
     (delq nil
           (list (and (plist-get entry :promotion)
                      (cons 'context-promotion
                            (copy-tree (plist-get entry :promotion))))
                 (and (plist-get entry :erasure)
                      (cons 'context-erasure
                            (copy-tree (plist-get entry :erasure)))))))
    (_ nil)))

(defun e-session-aggregate-apply-record (store record)
  "Apply detached semantic RECORD to STORE during journal replay.

The durable codec owns only the wire-to-value mapping.  This function is the
aggregate's sole replay application boundary: it validates ownership, updates
the domain projections, and never performs a second persistence operation.
RECORD must already be detached by `e-session-codec-decode-record'."
  (let* ((type (plist-get record :type))
         (session-id (plist-get record :session-id))
         (timestamp (plist-get record :timestamp))
         (session (and session-id
                       (gethash session-id
                                (e-session-store-sessions store)))))
    (pcase type
      ("session"
       (e-session-aggregate--clear-board-journal store session-id)
       (let* ((metadata
               (e-session-aggregate--validate-metadata
                (e-session-aggregate-normalize-metadata-for-replay
                 (plist-get record :metadata) t)))
              (session
               (list :id session-id
                     :metadata metadata
                     :session-events nil
                     :messages nil
                     :board-output-sequence
                     (or (plist-get record :board-output-sequence) 0)
                     :board-activity-sequence
                     (or (plist-get record :board-activity-sequence) 0)
                     :activity-events nil
                     :branch-summaries nil
                     :current-branch (plist-get record :current-branch)
                     :compactions nil
                     :provider-anchors nil
                     :process-reports nil
                     :context-generations nil
                     :context-promotions nil
                     :context-curation-packages nil
                     :created-at (or (plist-get record :created-at) timestamp)
                     :updated-at (or (plist-get record :updated-at) timestamp)
                     :turn-options
                     (e-session-aggregate--normalize-turn-options
                      (plist-get record :turn-options))
                     :name (plist-get record :name))))
         (e-session-aggregate-initialize-list-state session)
         (e-session-aggregate--prepend-replayed-session-event
          session 'session-created
          (or (plist-get record :created-at) timestamp)
          (list :metadata metadata) record)
         (e-session-aggregate--touch store session
                                     (plist-get session :updated-at))
         (puthash session-id session (e-session-store-sessions store))))
      ("message"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :messages
          (e-session-aggregate--normalize-entry-from-record
           session 'message
           (e-session-aggregate--message-with-created-at
            (plist-get record :message) timestamp)
           timestamp record))
         (when-let ((sequence
                     (plist-get (car (plist-get session :messages))
                                :board-output-sequence)))
           (plist-put session :board-output-sequence
                      (max (or (plist-get session :board-output-sequence) 0)
                           sequence)))
         (e-session-aggregate--touch store session timestamp)))
      ("board-message"
       (when session
         (let* ((journal (e-session-aggregate--board-journal store session-id))
                (message
                 (e-session-aggregate--normalize-board-message
                  (e-session-aggregate--freeze-board-value
                   (copy-tree (plist-get record :message)))))
                (existing
                 (e-session-aggregate--existing-board-message journal message)))
           (unless existing
             (puthash (e-session-aggregate-board-message-identity message)
                      message (e-session-board-journal-id-index journal))
             (let ((cell (list message)))
               (if-let ((tail (e-session-board-journal-tail journal)))
                   (setcdr tail cell)
                 (setf (e-session-board-journal-messages journal) cell))
               (setf (e-session-board-journal-tail journal) cell))))
         (e-session-aggregate--touch store session timestamp)))
      ("board-session-state"
       (when session
         (plist-put session :board-session-state
                   (e-session-aggregate-projected-board-association record))
         (e-session-aggregate--touch store session timestamp)))
      ("board-messages-cleared"
       (when session
         (e-session-aggregate--clear-board-journal store session-id)
         (e-session-aggregate--touch store session timestamp)))
      ("message-display"
       (when session
         (when-let ((message
                     (seq-find
                      (lambda (message)
                        (equal (plist-get message :id)
                               (plist-get record :id)))
                      (plist-get session :messages))))
           (let ((display (plist-get record :display)))
             (if display
                 (plist-put message :display (if (stringp display)
                                                 (intern display) display))
               (cl-remf message :display))))
         (e-session-aggregate--touch store session timestamp)))
      ("activity-event"
       (when session
         (let* ((event
                 (e-session-aggregate--normalize-entry-from-record
                  session 'activity-event
                  (or (plist-get record :semantic-event)
                      (list :id (plist-get record :id)
                            :parent-id (plist-get record :parent-id)
                            :turn-id (plist-get record :turn-id)
                            :event-type (plist-get record :event-type)
                            :payload (plist-get record :payload)
                            :created-at timestamp))
                  timestamp record)))
           (when (plist-member record :checkpoint-retain)
             (plist-put event :checkpoint-retain
                        (plist-get record :checkpoint-retain)))
           (when (plist-member record :board-activity-sequence)
             (plist-put event :board-activity-sequence
                        (plist-get record :board-activity-sequence)))
           (e-session-aggregate--prepend-replayed-item
            session :activity-events event)
           (when-let ((sequence (plist-get event :board-activity-sequence)))
             (plist-put session :board-activity-sequence
                        (max (or (plist-get session :board-activity-sequence) 0)
                             sequence)))
           (e-session-aggregate--update-activity-derived-fields session event)))
       (when session
         (e-session-aggregate--touch store session timestamp)))
      ("branch-summary"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :branch-summaries
          (e-session-aggregate--normalize-entry-from-record
           session 'branch-summary
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :branch-id (plist-get record :branch-id)
                 :summary (plist-get record :summary)
                 :metadata (plist-get record :metadata)
                 :created-at timestamp)
           timestamp record))
         (e-session-aggregate--touch store session timestamp)))
      ("compaction"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :compactions
          (e-session-aggregate--normalize-entry-from-record
           session 'compaction
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :summary (plist-get record :summary)
                 :branch-id (plist-get record :branch-id)
                 :range (plist-get record :range)
                 :first-kept-entry-id (plist-get record :first-kept-entry-id)
                 :tokens-before (plist-get record :tokens-before)
                 :tokens-kept (plist-get record :tokens-kept)
                 :metadata (plist-get record :metadata)
                 :created-at timestamp)
           timestamp record))
         (e-session-aggregate--touch store session timestamp)))
      ("provider-anchor"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :provider-anchors
          (e-session-aggregate--normalize-entry-from-record
           session 'provider-anchor
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :provider-id (let ((value (plist-get record :provider-id)))
                                (if (stringp value) (intern value) value))
                 :model (plist-get record :model)
                 :covered-entry-id (plist-get record :covered-entry-id)
                 :fingerprints (plist-get record :fingerprints)
                 :metadata (plist-get record :metadata)
                 :created-at timestamp)
           timestamp record))
         (e-session-aggregate--touch store session timestamp)))
      ("process-report"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :process-reports
          (e-session-aggregate--normalize-entry-from-record
           session 'process-report
           (plist-get record :report) timestamp record))
         (e-session-aggregate--touch store session timestamp)))
      ((or "context-frame" "context-frame-settlement") nil)
      ("context-erasure"
       (signal 'e-session-error
               (list "Standalone context erasure records are unsupported")))
      ("context-curation-package"
       (when session
         (e-session-aggregate--replay-context-curation-package
          store session-id record)))
      ((or "context-generation" "context-promotion")
       (let* ((entry-type (intern type))
              (raw-context-record (plist-get record :context-record)))
         ;; Version 1 records remain readable history, but do not recreate the
         ;; retired runtime-frame model.  Version 3 records are the only ones
         ;; admitted to the narrowed semantic projection.
         (when (and session
                    (e-session-aggregate--context-record-duplicate-key-p
                     raw-context-record))
           (signal 'e-session-error
                   (list "Context lifetime replay has duplicate fields"
                         entry-type)))
         (when (and session
                    (not (equal (plist-get raw-context-record :record-version)
                                1)))
           (let* ((field (if (eq entry-type 'context-generation)
                             :context-generations
                           :context-promotions))
                  (context-record
                  (e-session-aggregate--normalize-context-record
                    entry-type
                    (e-session-aggregate--normalize-context-record-for-replay
                     entry-type raw-context-record)
                    nil t))
                  (_ownership
                   (e-session-aggregate--validate-context-entry-ownership
                    store session-id entry-type context-record))
                  (entry
                   (e-session-aggregate--normalize-entry-from-record
                    session entry-type
                    (list :context-record context-record)
                    timestamp record)))
             (e-session-aggregate--prepend-replayed-item session field entry)
             (e-session-aggregate--touch store session timestamp)))))
      ("current-branch"
       (when session
         (plist-put session :current-branch (plist-get record :branch-id))
         (e-session-aggregate--prepend-replayed-session-event
          session 'current-branch timestamp
          (list :branch-id (plist-get record :branch-id)) record)
         (e-session-aggregate--touch store session timestamp)))
      ("session-info"
       (when session
         (let (fields)
           (when (plist-member record :name)
             (setq fields (plist-put fields :name (plist-get record :name))))
           (when (plist-member record :metadata)
             (setq fields
                   (plist-put
                    fields :metadata
                    (e-session-aggregate-normalize-metadata-for-replay
                     (plist-get record :metadata) t))))
           (when (plist-member record :turn-options)
             (setq fields
                   (plist-put fields :turn-options
                              (e-session-aggregate--normalize-turn-options
                               (plist-get record :turn-options)))))
           (e-session-aggregate--prepend-replayed-session-event
            session 'session-info timestamp fields record))
         (when (plist-member record :name)
           (plist-put session :name (plist-get record :name)))
         (when (plist-member record :metadata)
           (plist-put session :metadata
                      (e-session-aggregate-normalize-metadata-for-replay
                       (plist-get record :metadata) t)))
         (when (plist-member record :turn-options)
           (plist-put session :turn-options
                      (e-session-aggregate--normalize-turn-options
                       (plist-get record :turn-options))))
         (e-session-aggregate--touch store session timestamp)))
      ("messages-cleared"
       (when session
         (e-session-aggregate--replace-list-field session :messages nil)
         (e-session-aggregate--replace-list-field session :activity-events nil)
         (e-session-aggregate--replace-list-field session :provider-anchors nil)
         (plist-put session :latest-token-usage-event nil)
         (e-session-aggregate--clear-message-derived-fields store session)
         (plist-put session :current-head-id
                    (e-session-aggregate--root-event-id session))
         (e-session-aggregate--prepend-replayed-session-event
          session 'messages-cleared timestamp
          (list :parent-id (e-session-aggregate--root-event-id session)) record)
         (e-session-aggregate--touch store session timestamp))))))


(provide 'e-session-aggregate)

;;; e-session-aggregate.el ends here

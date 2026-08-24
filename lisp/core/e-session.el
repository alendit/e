;;; e-session.el --- Session store for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session storage for the pure core runtime.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'e-request)
(require 'e-context-lifetime)
(require 'seq)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")
(declare-function e-session-persistence-submit-record "e-session-persistence")
(declare-function e-session-persistence-await-record-prefix
                  "e-session-persistence")
(declare-function e-session-persistence-request-checkpoint "e-session-persistence")
(declare-function e-session-persistence-finalize "e-session-persistence")

(define-error 'e-session-missing "Session does not exist")
(define-error 'e-session-duplicate "Session already exists")
(define-error 'e-session-checkpoint-missing
  "Session resume checkpoint does not exist"
  'e-session-missing)
(define-error 'e-session-checkpoint-invalid
  "Session resume checkpoint is invalid")
(define-error 'e-session-persistence-unavailable
  "Session persistence controller is unavailable")
(define-error 'e-session-board-message-conflict
  "Conflicting board message envelope")
(define-error 'e-session-board-message-cycle
  "Cyclic board message envelope")
(define-error 'e-session-board-message-invalid-record-type
  "Invalid board message record type")

(defgroup e-session nil
  "Session storage for e."
  :group 'e
  :prefix "e-session-")

(defcustom e-session-directory (locate-user-emacs-file "e/sessions/")
  "Default directory used for persisted e sessions."
  :type 'directory
  :group 'e-session)

(define-error 'e-session-error "Session error")

(cl-defstruct (e-session-store (:constructor e-session-store-create))
  (sessions (make-hash-table :test 'equal))
  (entry-indexes (make-hash-table :test 'equal))
  (board-journals (make-hash-table :test 'equal))
  directory
  sessions-directory
  index-file
  persistent
  write-mode
  write-queue
  write-queue-timer
  index-write-pending
  persistence-controller
  (checkpoint-dirty-session-ids (make-hash-table :test 'equal))
  (write-queue-generation 0)
  (write-queue-sequence 0)
  (unsettled-write-count 0)
  (unsettled-generation 0)
  (sequence 0))

(cl-defstruct (e-session-board-journal
               (:constructor e-session--board-journal-create))
  messages tail (id-index (make-hash-table :test 'equal)))

(defvar e-session--unsettled-write-count 0)
(defvar e-session--unsettled-generation 0)
(defvar e-session--unsettled-change-function nil)
(defvar e-session--unsettled-change-functions nil)

(defun e-session-persistence-unsettled-state ()
  "Return the constant-time aggregate persistence owner projection."
  (list :generation e-session--unsettled-generation
        :writes e-session--unsettled-write-count))

(defun e-session--adjust-unsettled-writes (store delta)
  "Adjust STORE and aggregate writer outbox ownership by DELTA."
  (let ((store-next (+ (e-session-store-unsettled-write-count store) delta))
        (global-next (+ e-session--unsettled-write-count delta)))
    (when (or (< store-next 0) (< global-next 0))
      (signal 'e-session-error
              (list "Negative persistence unsettled count" store-next global-next)))
    (setf (e-session-store-unsettled-write-count store) store-next)
    (cl-incf (e-session-store-unsettled-generation store))
    (setq e-session--unsettled-write-count global-next)
    (cl-incf e-session--unsettled-generation)
    (when e-session--unsettled-change-function
      (funcall e-session--unsettled-change-function
               (e-session-persistence-unsettled-state)))
    (run-hook-with-args 'e-session--unsettled-change-functions
                        (e-session-persistence-unsettled-state))
    store-next))

(defcustom e-session-write-queue-delay 0.05
  "Seconds to wait before flushing queued persistent session writes."
  :type 'number
  :group 'e)

(defcustom e-session-load-chunk-bytes 65536
  "Number of bytes to read per cooperative persistent session load step."
  :type 'integer
  :group 'e)

(defcustom e-session-checkpoint-activity-event-limit 64
  "Maximum recent activity entries retained in a resume checkpoint."
  :type 'integer
  :group 'e-session)

(defcustom e-session-checkpoint-board-message-limit 256
  "Maximum recent board messages retained in a resume checkpoint."
  :type 'integer
  :group 'e-session)

(defcustom e-session-checkpoint-board-fact-limit 256
  "Maximum recent board facts retained in a resume checkpoint.
Facts are selected independently from the recent message tail so high-volume
activity cannot evict the semantic state needed by board reducers."
  :type 'integer
  :group 'e-session)

(defcustom e-session-checkpoint-process-report-limit 32
  "Maximum recent process reports retained in a resume checkpoint."
  :type 'integer
  :group 'e-session)

(defconst e-session-checkpoint-version 1
  "Current durable resume-checkpoint format version.")

(defconst e-session--replay-list-fields
  '(:session-events :messages :activity-events :branch-summaries
    :compactions :provider-anchors :process-reports
    :context-generations :context-frames :context-promotions
    :context-frame-settlements)
  "Session fields accumulated in reverse order while replaying JSONL.")

(defconst e-session--list-tail-fields
  '((:messages . :messages-tail)
    (:activity-events . :activity-events-tail)
    (:branch-summaries . :branch-summaries-tail)
    (:compactions . :compactions-tail)
    (:provider-anchors . :provider-anchors-tail)
    (:process-reports . :process-reports-tail)
    (:context-generations . :context-generations-tail)
    (:context-frames . :context-frames-tail)
    (:context-promotions . :context-promotions-tail)
    (:context-frame-settlements . :context-frame-settlements-tail))
  "Internal append-only list fields and their cached tail cells.")

(defconst e-session--context-lifetime-entry-types
  '(context-generation context-frame context-promotion context-frame-settlement)
  "Durable entry types owned by the generational context lifetime model.")

(defconst e-session--context-frame-states
  '(open consuming consumed settled aborted)
  "Frame states accepted at the session persistence boundary.")

(defconst e-session--context-settlement-statuses
  '(acknowledged settled failed aborted)
  "Settlement statuses accepted at the session persistence boundary.")

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

(defconst e-session--presentation-metadata-keys
  '(:e-chat-read-markers)
  "Presentation-only metadata keys rejected on write and removed on replay.")

(defconst e-session--ulid-alphabet "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
  "Crockford Base32 alphabet used for ULID strings.")

(defvar e-session--last-ulid-milliseconds nil
  "Last millisecond timestamp used by `e-session-generate-ulid'.")

(defvar e-session--last-ulid-random nil
  "Last 80-bit random suffix used by `e-session-generate-ulid'.")

(defun e-session--metadata-descriptor (key)
  "Return metadata schema descriptor for KEY."
  (seq-find (lambda (descriptor)
              (eq (car descriptor) key))
            e-session-metadata-schema))

(defun e-session-metadata-key-state-class (key)
  "Return the declared state class for durable metadata KEY."
  (plist-get (cdr (e-session--metadata-descriptor key)) :state-class))

(defun e-session--plist-remove (plist key)
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

(defun e-session--keyword-plist-shape-p (value)
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

(defun e-session--metadata-owner-key (owner)
  "Return stable keyword key for metadata OWNER."
  (cond
   ((keywordp owner) owner)
   ((symbolp owner) (intern (concat ":" (symbol-name owner))))
   ((stringp owner) (intern (concat ":" owner)))
   (t (error "Metadata owner must be a keyword, symbol, or string: %S" owner))))

(defun e-session--metadata-json-array-safe-value (value)
  "Return VALUE with reference arrays encoded unambiguously for JSON.
Keyword plists remain objects.  Other proper lists become vectors so the JSON
writer cannot reinterpret a list of plists as one object."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'e-session--metadata-json-array-safe-value value)))
   ((e-session--keyword-plist-shape-p value)
    (let (result)
      (while (consp value)
        (let ((key (pop value)))
          (when (consp value)
            (push key result)
            (push (e-session--metadata-json-array-safe-value (pop value))
                  result))))
      (nreverse result)))
   ((proper-list-p value)
    (vconcat (mapcar #'e-session--metadata-json-array-safe-value value)))
   (t value)))

(defun e-session--metadata-public-value (value)
  "Return persisted metadata VALUE in caller-facing Elisp shape."
  (cond
   ((vectorp value)
    (mapcar #'e-session--metadata-public-value value))
   ((e-session--keyword-plist-shape-p value)
    (let (result)
      (while (consp value)
        (let ((key (pop value)))
          (when (consp value)
            (push key result)
            (push (e-session--metadata-public-value (pop value)) result))))
      (nreverse result)))
   ((proper-list-p value)
    (mapcar #'e-session--metadata-public-value value))
   (t value)))

(defun e-session--metadata-validate-org-canvas-ref (value key)
  "Validate Org Canvas metadata VALUE under KEY."
  (unless (or (null value) (e-session--keyword-plist-shape-p value))
    (error "Session metadata %S must be a keyword plist" key))
  (when (or (plist-member value :last-focus)
            (plist-member value :last-scope))
    (error "Session metadata %S must not contain volatile focus or scope" key)))

(defun e-session--metadata-validate-context-references (value)
  "Validate durable current-state reference VALUE."
  (unless (or (null value) (e-session--keyword-plist-shape-p value))
    (error "Session metadata :context-references must be an owner-keyed plist")))

(defun e-session--metadata-validate-capability-state (value)
  "Validate durable capability-state VALUE."
  (unless (or (null value) (e-session--keyword-plist-shape-p value))
    (error "Session metadata :capability-state must be an owner-keyed plist")))

(defun e-session--validate-metadata-value (key value)
  "Validate durable session metadata KEY VALUE."
  (pcase key
    ((or :org-canvas :org-canvas-ref)
     (e-session--metadata-validate-org-canvas-ref value key))
    (:context-references
     (e-session--metadata-validate-context-references value))
    (:capability-state
     (e-session--metadata-validate-capability-state value))
    (_ nil)))

(defun e-session--validate-metadata-class (metadata expected-class)
  "Validate that METADATA only contains keys in EXPECTED-CLASS."
  (let ((tail metadata))
    (while (consp tail)
      (let ((key (pop tail)))
        (unless (consp tail)
          (error "Session metadata has key %S without value" key))
        (let* ((value (pop tail))
               (descriptor (e-session--metadata-descriptor key))
               (state-class (plist-get (cdr descriptor) :state-class)))
          (unless descriptor
            (error "Session metadata key %S has no durable state schema" key))
          (unless (eq state-class expected-class)
            (error "Session metadata key %S is %S, not %S"
                   key state-class expected-class))
          (e-session--validate-metadata-value key value)))))
  metadata)

(defun e-session--validate-metadata (metadata)
  "Validate durable session METADATA and return it."
  (unless (or (null metadata) (e-session--keyword-plist-shape-p metadata))
    (error "Session metadata must be a keyword plist"))
  (let ((tail metadata))
    (while (consp tail)
      (let* ((key (pop tail))
             (value (pop tail))
             (descriptor (e-session--metadata-descriptor key)))
        (when (memq key e-session--presentation-metadata-keys)
          (error "Session metadata key %S is presentation state" key))
        (unless descriptor
          (error "Session metadata key %S has no durable state schema" key))
        (e-session--validate-metadata-value key value))))
  metadata)

(defun e-session--normalize-org-canvas-ref-for-replay (value)
  "Return legacy Org Canvas VALUE without volatile focus fields."
  (when value
    (setq value (copy-sequence value))
    (setq value (e-session--plist-remove value :last-focus))
    (setq value (e-session--plist-remove value :last-scope)))
  value)

(defun e-session--legacy-metadata-key (value)
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

(defun e-session--normalize-legacy-metadata-array (metadata)
  "Repair legacy JSON-array METADATA into a schema-keyed plist.
This is only for replaying old persisted records that encoded metadata as
arrays and sometimes inverted key/value pairs."
  (if (or (null metadata)
          (e-session--keyword-plist-shape-p metadata)
          (not (proper-list-p metadata)))
      metadata
    (let ((tail metadata)
          result
          repaired)
      (while (consp tail)
        (let* ((first (pop tail))
               (second (and (consp tail) (pop tail)))
               (first-key (e-session--legacy-metadata-key first))
               (second-key (e-session--legacy-metadata-key second)))
          (cond
           ((and first-key (not second-key))
            (setq result (plist-put result first-key second)
                  repaired t))
           ((and second-key (not first-key))
            (setq result (plist-put result second-key first)
                  repaired t)))))
      (if repaired result metadata))))

(defun e-session--normalize-metadata-for-replay (metadata &optional legacy)
  "Return replayed METADATA without known transient state."
  (when (and legacy
             (consp metadata)
             (not (keywordp (car metadata))))
    (setq metadata (e-session--normalize-legacy-metadata-array metadata)))
  (when metadata
    (setq metadata (copy-sequence metadata))
    (dolist (key e-session--presentation-metadata-keys)
      (setq metadata (e-session--plist-remove metadata key)))
    (when (plist-member metadata :org-canvas)
      (setq metadata
            (plist-put
             metadata
             :org-canvas
             (e-session--normalize-org-canvas-ref-for-replay
              (plist-get metadata :org-canvas)))))
    (when (plist-member metadata :org-canvas-ref)
      (setq metadata
            (plist-put
             metadata
             :org-canvas-ref
             (e-session--normalize-org-canvas-ref-for-replay
              (plist-get metadata :org-canvas-ref))))))
  metadata)

(defun e-session--timestamp (&optional time)
  "Return TIME as a compact UTC timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun e-session--id-timestamp (&optional time)
  "Return TIME as a session-id timestamp."
  (format-time-string "%Y%m%dT%H%M%S" time t))

(defun e-session--generate-id ()
  "Generate a persistent session id."
  (let* ((seed (format "%S" (list (current-time) (random) (emacs-pid)
                                  (system-name))))
         (suffix (substring (secure-hash 'sha1 seed) 0 12)))
    (format "%s-%s" (e-session--id-timestamp) suffix)))

(defun e-session--ulid-encode (number length)
  "Encode NUMBER as a Crockford Base32 string with LENGTH characters."
  (let ((chars (make-string length ?0))
        (index (1- length)))
    (while (>= index 0)
      (aset chars index (aref e-session--ulid-alphabet (logand number 31)))
      (setq number (ash number -5))
      (setq index (1- index)))
    chars))

(defun e-session--current-milliseconds ()
  "Return current Unix time in milliseconds."
  (floor (* 1000 (float-time))))

(defun e-session--random-80-bit ()
  "Return a sufficiently random 80-bit integer."
  (let* ((seed (format "%S" (list (current-time) (random t) (emacs-pid)
                                  (system-name))))
         (hex (substring (secure-hash 'sha1 seed) 0 20)))
    (string-to-number hex 16)))

(defun e-session--ulid-from-parts (milliseconds random)
  "Return a ULID from MILLISECONDS and 80-bit RANDOM suffix."
  (concat (e-session--ulid-encode milliseconds 10)
          (e-session--ulid-encode random 16)))

(defun e-session-generate-ulid ()
  "Generate an opaque monotonic ULID string for durable session entries."
  (let* ((milliseconds (e-session--current-milliseconds))
         (random (if (equal milliseconds e-session--last-ulid-milliseconds)
                     (1+ (or e-session--last-ulid-random 0))
                   (e-session--random-80-bit)))
         (random-limit (expt 2 80)))
    (when (>= random random-limit)
      (setq milliseconds (1+ milliseconds))
      (setq random 0))
    (setq e-session--last-ulid-milliseconds milliseconds
          e-session--last-ulid-random random)
    (e-session--ulid-from-parts milliseconds random)))

(defun e-session--timestamp-milliseconds (timestamp)
  "Return TIMESTAMP parsed as Unix milliseconds, or current milliseconds."
  (condition-case nil
      (if (stringp timestamp)
          (floor (* 1000 (float-time (date-to-time timestamp))))
        (e-session--current-milliseconds))
    (error (e-session--current-milliseconds))))

(defun e-session--legacy-entry-id (session type ordinal timestamp)
  "Return a stable backfilled id for legacy SESSION entry TYPE at ORDINAL."
  (let* ((session-id (plist-get session :id))
         (seed (format "%s:%s:%s:%s" session-id type ordinal timestamp))
         (random (string-to-number (substring (secure-hash 'sha1 seed) 0 20)
                                   16)))
    (e-session--ulid-from-parts
     (e-session--timestamp-milliseconds timestamp)
     random)))

(defun e-session--next-sequence (store)
  "Return STORE's next mutation sequence."
  (setf (e-session-store-sequence store)
        (1+ (e-session-store-sequence store))))

(defun e-session--touch (store session &optional timestamp)
  "Update SESSION's modification metadata in STORE."
  (plist-put session :updated-at (or timestamp (e-session--timestamp)))
  (plist-put session :updated-seq (e-session--next-sequence store))
  session)

(defun e-session--persistent-p (store)
  "Return non-nil when STORE writes to disk."
  (and (e-session-store-persistent store)
       (e-session-store-directory store)))

(defun e-session--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-session--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when developer profiling is enabled."
  (if (e-session--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-session--ensure-directories (store)
  "Ensure persistent directories for STORE exist."
  (when (e-session--persistent-p store)
    (make-directory (e-session-store-sessions-directory store) t)))

(defun e-session--session-file (store session-id)
  "Return JSONL file path for SESSION-ID in STORE."
  (expand-file-name (concat session-id ".jsonl")
                    (e-session-store-sessions-directory store)))

(defun e-session--checkpoint-file (store session-id)
  "Return resume-checkpoint file path for SESSION-ID in STORE."
  (expand-file-name (concat session-id ".checkpoint.json")
                    (e-session-store-sessions-directory store)))

(defun e-session--mark-checkpoint-dirty (store session-id)
  "Mark SESSION-ID's resume state dirty in STORE."
  (puthash session-id t (e-session-store-checkpoint-dirty-session-ids store)))

(defun e-session-checkpoint-dirty-session-ids (store)
  "Return STORE session ids needing a resume checkpoint."
  (let (ids)
    (maphash (lambda (session-id _value) (push session-id ids))
             (e-session-store-checkpoint-dirty-session-ids store))
    (nreverse ids)))

(defun e-session-checkpoint-mark-clean (store session-ids)
  "Mark SESSION-IDS' submitted resume state clean in STORE."
  (dolist (session-id session-ids)
    (remhash session-id
             (e-session-store-checkpoint-dirty-session-ids store))))

(defun e-session--queued-writes-p (store)
  "Return non-nil when STORE batches persistent writes through a timer."
  (eq (e-session-store-write-mode store) 'queued))

(defun e-session--persistence-controller (store)
  "Return STORE's asynchronous persistence controller, if configured."
  (e-session-store-persistence-controller store))

(defun e-session--append-record-now (store session-id record)
  "Immediately append RECORD for SESSION-ID in persistent STORE."
  (when (e-session--persistent-p store)
    (e-session--ensure-directories store)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-buffer
        (insert (json-encode record) "\n")
        (write-region (point-min) (point-max)
                      (e-session--session-file store session-id)
                      t 'silent)))))

(defun e-session--record-already-appended-p (store session-id record)
  "Return non-nil when RECORD already occupies SESSION-ID's journal."
  (let ((journal (e-session--session-file store session-id))
        (encoded (json-encode record)))
    (and (file-readable-p journal)
         (with-temp-buffer
           (insert-file-contents journal)
           (goto-char (point-min))
           (catch 'found
             (while (not (eobp))
               (when (string= encoded
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))
                 (throw 'found t))
               (forward-line 1)))))))

(defun e-session--index-json (store)
  "Return STORE's current index JSON line."
  (concat (json-encode (vconcat (e-session-list store))) "\n"))

(defun e-session--write-index-now (store)
  "Immediately write STORE's persistent session index."
  (when (e-session--persistent-p store)
    (e-session--ensure-directories store)
    (let ((dirty (e-session-checkpoint-dirty-session-ids store)))
      (dolist (session-id dirty)
        ;; Direct stores establish a root checkpoint once; later records are a
        ;; valid suffix.  Explicit migration and the asynchronous writer
        ;; compact that suffix at deliberate durability boundaries.
        (when (and (plist-get (e-session--peek-session store session-id) :loaded)
                   (not (file-exists-p
                         (e-session--checkpoint-file store session-id))))
          (e-session--write-session-checkpoint-now store session-id)))
      (e-session-checkpoint-mark-clean store dirty))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file (e-session-store-index-file store)
        (insert (e-session--index-json store))))))

(defconst e-session--critical-record-types
  '("session"
    "session-info"
    "message"
    "board-message"
    "message-display"
    "activity-event"
    "branch-summary"
    "compaction"
    "provider-anchor"
    "process-report"
    "context-generation"
    "context-frame"
    "context-promotion"
    "context-frame-settlement"
    "current-branch"
    "messages-cleared")
  "Persistent record types that must flush before derived queued records.")

(defun e-session--queued-record-criticality (record)
  "Return the queued-write criticality class for RECORD."
  (if (member (plist-get record :type) e-session--critical-record-types)
      'critical
    'derived))

(defun e-session--queued-record-dependencies (session-id record)
  "Return dependency metadata for queued RECORD in SESSION-ID."
  (list :session-id session-id
        :record-id (or (plist-get record :id)
                       (plist-get record :entry_id))
        :parent-id (or (plist-get record :parent_id)
                       (plist-get record :previous_entry_id))))

(defun e-session--queued-write-entry (store session-id record)
  "Return a metadata-bearing queued write entry for RECORD."
  (list :session-id session-id
        :record record
        :generation (e-session-store-write-queue-generation store)
        :sequence (cl-incf (e-session-store-write-queue-sequence store))
        :criticality (e-session--queued-record-criticality record)
        :dependencies (e-session--queued-record-dependencies session-id record)))

(defun e-session--queued-index-entry (store)
  "Return a metadata-bearing queued derived-index write entry."
  (list :generation (e-session-store-write-queue-generation store)
        :sequence (cl-incf (e-session-store-write-queue-sequence store))
        :criticality 'derived
        :dependencies '(:source queued-records)))

(defun e-session--queued-entry-session-id (entry)
  "Return queued ENTRY's session id."
  (if (and (consp entry)
           (keywordp (car entry)))
      (plist-get entry :session-id)
    (car entry)))

(defun e-session--queued-entry-record (entry)
  "Return queued ENTRY's persistent record."
  (if (and (consp entry)
           (keywordp (car entry))
           (plist-member entry :record))
      (plist-get entry :record)
    (cdr entry)))

(defun e-session--queued-entry-current-p (store entry)
  "Return non-nil when queued ENTRY belongs to STORE's current generation."
  (let ((generation (plist-get entry :generation)))
    (or (null generation)
        (= generation (e-session-store-write-queue-generation store)))))

(defun e-session--queued-index-current-p (store entry)
  "Return non-nil when queued index ENTRY belongs to STORE's current generation."
  (or (eq entry t)
      (and (consp entry)
           (keywordp (car entry))
           (= (plist-get entry :generation)
              (e-session-store-write-queue-generation store)))))

(defun e-session--clear-write-queue-timer (store)
  "Clear STORE's queued write timer slot."
  (let ((timer (e-session-store-write-queue-timer store)))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (e-session-store-write-queue-timer store) nil))

(defun e-session--drop-queued-write-entry (store entry)
  "Drop acknowledged queued write ENTRY from STORE."
  (when (memq entry (e-session-store-write-queue store))
    (setf (e-session-store-write-queue store)
          (delq entry (e-session-store-write-queue store)))
    (e-session--adjust-unsettled-writes store -1)))

(defun e-session--drop-stale-queued-write-entries (store)
  "Remove stale-generation queued writes from STORE."
  (let* ((old (e-session-store-write-queue store))
         (current (cl-remove-if-not
                   (lambda (entry)
                     (e-session--queued-entry-current-p store entry))
                   old))
         (dropped (- (length old) (length current))))
    (setf (e-session-store-write-queue store) current)
    (when (> dropped 0)
      (e-session--adjust-unsettled-writes store (- dropped)))))

(defun e-session--queued-entry-critical-p (entry)
  "Return non-nil when queued ENTRY must flush before derived records."
  (not (and (consp entry)
            (keywordp (car entry))
            (eq (plist-get entry :criticality) 'derived))))

(defun e-session--order-queued-records-for-flush (entries)
  "Return ENTRIES with critical records before derived records.
Ordering remains stable within each criticality class."
  (let (critical derived)
    (dolist (entry entries)
      (if (e-session--queued-entry-critical-p entry)
          (push entry critical)
        (push entry derived)))
    (nconc (nreverse critical) (nreverse derived))))

(defun e-session--flush-queued-records (store entries)
  "Append queued ENTRIES, acknowledging each durable record write."
  (dolist (entry entries)
    (let ((session-id (e-session--queued-entry-session-id entry))
          (record (e-session--queued-entry-record entry)))
      (condition-case error
          (e-session--append-record-now store session-id record)
        (error
         (unless (e-session--record-already-appended-p store session-id record)
           (signal (car error) (cdr error)))))
      (e-session--drop-queued-write-entry store entry))))

(defun e-session-flush-write-queue (store)
  "Synchronously flush queued persistent writes for STORE.
Return STORE."
  (e-session--clear-write-queue-timer store)
  (let* ((raw-entries (reverse (e-session-store-write-queue store)))
         (entries (cl-remove-if-not
                   (lambda (entry)
                     (e-session--queued-entry-current-p store entry))
                   raw-entries))
         (ordered-entries (e-session--order-queued-records-for-flush entries))
         (write-index-entry (e-session-store-index-write-pending store))
         (write-index (and write-index-entry
                           (e-session--queued-index-current-p
                            store write-index-entry)))
         (stale-index (and write-index-entry (not write-index)))
         (rebuild-index (and stale-index entries))
         (stale-count (- (length raw-entries) (length entries))))
    (e-session--profile-call
     'session.flush-write-queue
     (list :metadata (list :persistent (and (e-session--persistent-p store) t)
                           :record-count (length entries)
                           :stale-record-count stale-count
                           :stale-index (and stale-index t)
                           :write-index (and (or write-index rebuild-index) t)))
     (lambda ()
       (e-session--drop-stale-queued-write-entries store)
       (e-session--flush-queued-records store ordered-entries)
       (when rebuild-index
         (setf (e-session-store-index-write-pending store)
               (e-session--queued-index-entry store)))
       (when (or write-index rebuild-index)
         (e-session--write-index-now store))))
    (setf (e-session-store-write-queue store) nil)
    (when (e-session-store-index-write-pending store)
      (setf (e-session-store-index-write-pending store) nil)
      (e-session--adjust-unsettled-writes store -1)))
  store)

(defun e-session--schedule-write-queue (store)
  "Schedule STORE's queued persistent writes."
  (when (and (e-session--persistent-p store)
             (e-session--queued-writes-p store)
             (not (timerp (e-session-store-write-queue-timer store))))
    (let ((generation (cl-incf (e-session-store-write-queue-generation store)))
          (delay (max 0 (or e-session-write-queue-delay 0))))
      (setf (e-session-store-write-queue-timer store)
            (run-at-time
             delay nil
             (lambda ()
               (when (and (e-session-store-p store)
                          (= generation
                             (e-session-store-write-queue-generation store)))
                 (e-session-flush-write-queue store))))))))

(defun e-session--append-record (store session-id record)
  "Append RECORD for SESSION-ID in persistent STORE."
  (e-session--profile-call
   'session.append-record
   (list :session-id session-id
         :metadata (list :record-type (plist-get record :type)))
   (lambda ()
     (when (e-session--persistent-p store)
       (e-session--mark-checkpoint-dirty store session-id)
       (if-let ((controller (e-session--persistence-controller store)))
           (e-session-persistence-submit-record controller session-id record)
         (if (e-session--queued-writes-p store)
           (progn
             (e-session--schedule-write-queue store)
             (push (e-session--queued-write-entry store session-id record)
                   (e-session-store-write-queue store))
             (e-session--adjust-unsettled-writes store 1))
           (e-session--append-record-now store session-id record)))))))


(defun e-session--entry-index (store session-id)
  "Return STORE's entry-id index for SESSION-ID."
  (or (gethash session-id (e-session-store-entry-indexes store))
      (puthash session-id
               (make-hash-table :test 'equal)
               (e-session-store-entry-indexes store))))

(defun e-session--clear-entry-index (store session-id)
  "Clear STORE's entry-id index for SESSION-ID."
  (remhash session-id (e-session-store-entry-indexes store)))

(defun e-session--index-entry (store session-id entry)
  "Index durable ENTRY for SESSION-ID in STORE."
  (when-let ((entry-id (plist-get entry :id)))
    (puthash entry-id entry (e-session--entry-index store session-id)))
  entry)

(defun e-session--index-session-entries (store session)
  "Rebuild STORE's entry-id index for SESSION."
  (let ((session-id (plist-get session :id)))
    (when session-id
      (e-session--clear-entry-index store session-id)
      (dolist (entry (e-session--entries store session-id))
        (e-session--index-entry store session-id entry)))))

(defun e-session--list-tail (items)
  "Return the tail cell for ITEMS, or nil."
  (when items
    (last items)))

(defun e-session--tail-field (field)
  "Return the cached tail field for append-only FIELD."
  (alist-get field e-session--list-tail-fields))

(defun e-session--initialize-list-state (session)
  "Destructively initialize SESSION append-only list tail fields.
This repairs or resets the internal cached tail cells from the current
canonical list values.  Callers use this after creating or replaying a session,
or after constructing an unloaded index stub."
  (dolist (pair e-session--list-tail-fields)
    (plist-put session (cdr pair) (e-session--list-tail
                                   (plist-get session (car pair)))))
  session)

(defun e-session--replace-list-field (session field items)
  "Destructively replace SESSION FIELD with ITEMS and update its tail."
  (plist-put session field items)
  (when-let ((tail-field (e-session--tail-field field)))
    (plist-put session tail-field (e-session--list-tail items)))
  items)

(defun e-session--append-list-item (session field item)
  "Append ITEM to SESSION FIELD in O(1) and return ITEM.
The canonical list spine belongs to the session store.  If FIELD has legacy
contents but no cached tail cell, compute and cache the tail once before
appending."
  (let* ((tail-field (e-session--tail-field field))
         (cell (list item))
         (tail (or (and tail-field (plist-get session tail-field))
                   (when-let ((items (plist-get session field)))
                     (e-session--list-tail items)))))
    (if tail
        (setcdr tail cell)
      (plist-put session field cell))
    (when tail-field
      (plist-put session tail-field cell))
    item))

(defun e-session--first-user-message (messages)
  "Return first user-authored content in MESSAGES."
  (catch 'found
    (dolist (message messages)
      (when (eq (plist-get message :role) 'user)
        (let ((content (plist-get message :content)))
          (when (stringp content)
            (throw 'found content)))))))

(defun e-session--default-title (prompt)
  "Return PROMPT formatted as a default session title."
  (if (> (length prompt) 25)
      (concat (substring prompt 0 25) "...")
    prompt))

(defun e-session--refresh-derived-fields (store session)
  "Refresh derived display fields for SESSION in STORE."
  (let ((messages (plist-get session :messages)))
    (plist-put session :summary (e-session--first-user-message messages))
    (plist-put session :message-count (length messages))
    (plist-put session :last-message-at (e-session--last-message-at session))
    (plist-put session :latest-assistant-marker
               (e-session--latest-assistant-marker session))
    (when (e-session--persistent-p store)
      (plist-put session :file
                 (e-session--session-file store (plist-get session :id)))))
  session)

(defun e-session--refresh-file-field (store session)
  "Refresh persistent file metadata for SESSION in STORE."
  (when (e-session--persistent-p store)
    (plist-put session :file
               (e-session--session-file store (plist-get session :id))))
  session)

(defun e-session--message-summary (message)
  "Return MESSAGE content when it should become a session summary."
  (when (eq (plist-get message :role) 'user)
    (let ((content (plist-get message :content)))
      (when (stringp content)
        content))))

(defun e-session--update-message-derived-fields-on-append
    (store session message)
  "Update SESSION derived fields incrementally for appended MESSAGE."
  (let ((count (plist-get session :message-count)))
    (plist-put session
               :message-count
               (if (integerp count)
                   (1+ count)
                 (length (plist-get session :messages)))))
  (unless (plist-get session :summary)
    (when-let ((summary (e-session--message-summary message)))
      (plist-put session :summary summary)))
  (plist-put session :last-message-at (plist-get message :created-at))
  (when (eq (plist-get message :role) 'assistant)
    (plist-put session :latest-assistant-marker
               (e-session--message-assistant-marker message)))
  (e-session--refresh-file-field store session))

(defun e-session--clear-message-derived-fields (store session)
  "Reset message-derived fields for cleared SESSION."
  (plist-put session :message-count 0)
  (plist-put session :summary nil)
  (plist-put session :last-message-at nil)
  (plist-put session :latest-assistant-marker nil)
  (e-session--refresh-file-field store session))

(defun e-session--display-title-for-session (session)
  "Return a display title for SESSION."
  (or (plist-get session :name)
      (when-let ((summary (plist-get session :summary)))
        (e-session--default-title summary))
      (when-let ((created-at (plist-get session :created-at)))
        (format "Untitled %s" created-at))
      (format "Untitled %s" (plist-get session :id))))

(defun e-session--prepend-replayed-item (session field item)
  "Prepend replayed ITEM to SESSION FIELD."
  (plist-put session field (cons item (plist-get session field))))

(defun e-session--next-entry-ordinal (session)
  "Return SESSION's next replay entry ordinal."
  (let ((ordinal (1+ (or (plist-get session :entry-count) 0))))
    (plist-put session :entry-count ordinal)
    ordinal))

(defun e-session--entry-id-from-record (record entry)
  "Return durable id from RECORD or ENTRY."
  (or (plist-get entry :id)
      (plist-get record :id)))

(defun e-session--entry-parent-id-from-record (record entry)
  "Return parent id from RECORD or ENTRY."
  (if (plist-member entry :parent-id)
      (plist-get entry :parent-id)
    (plist-get record :parent-id)))

(defun e-session--entry-with-identity (session type entry timestamp &optional record)
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
       (or (e-session--entry-id-from-record record entry)
           (if record
               (e-session--legacy-entry-id
                session type (e-session--next-entry-ordinal session) timestamp)
             (e-session-generate-ulid)))))
    (unless (plist-member entry :parent-id)
      (when-let ((parent-id
                  (or (e-session--entry-parent-id-from-record record entry)
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

(defun e-session--normalize-entry-from-record
    (session type entry timestamp &optional record)
  "Return normalized durable ENTRY for replay or append."
  (e-session--advance-head
   session
   (e-session--entry-with-identity session type entry timestamp record)))

(defun e-session--advance-head (session entry)
  "Advance SESSION current head to ENTRY."
  (plist-put session :current-head-id (plist-get entry :id))
  entry)

(defun e-session--root-event-id (session)
  "Return SESSION root event id, when available."
  (or (plist-get session :root-event-id)
      (plist-get (car (plist-get session :session-events)) :id)))

(defun e-session--session-event
    (session event-type timestamp &optional fields record)
  "Return a normalized session EVENT-TYPE entry for SESSION.
TIMESTAMP is used as creation metadata.  FIELDS are copied onto the event,
and RECORD supplies persisted identity fields during replay."
  (let ((entry (append (list :event-type event-type
                             :created-at timestamp)
                       (copy-sequence fields))))
    (e-session--normalize-entry-from-record
     session 'session-event entry timestamp record)))

(defun e-session--append-session-event
    (session event-type timestamp &optional fields record)
  "Append a normalized session EVENT-TYPE entry to SESSION."
  (let ((event (e-session--session-event
                session event-type timestamp fields record)))
    (plist-put session
               :session-events
               (append (plist-get session :session-events) (list event)))
    (when (eq event-type 'session-created)
      (plist-put session :root-event-id (plist-get event :id)))
    event))

(defun e-session--prepend-replayed-session-event
    (session event-type timestamp &optional fields record)
  "Prepend a replayed session EVENT-TYPE entry to SESSION."
  (let ((event (e-session--session-event
                session event-type timestamp fields record)))
    (e-session--prepend-replayed-item session :session-events event)
    (when (eq event-type 'session-created)
      (plist-put session :root-event-id (plist-get event :id)))
    event))

(defun e-session--entries (store session-id)
  "Return all durable entries for SESSION-ID in insertion order."
  (let ((session (e-session--get-live store session-id)))
    (append (plist-get session :session-events)
            (plist-get session :messages)
            (plist-get session :activity-events)
            (plist-get session :branch-summaries)
            (plist-get session :compactions)
            (plist-get session :provider-anchors)
            (plist-get session :process-reports)
            (plist-get session :context-generations)
            (plist-get session :context-frames)
            (plist-get session :context-promotions)
            (plist-get session :context-frame-settlements))))

(defun e-session-entry-by-id (store session-id entry-id)
  "Return durable entry ENTRY-ID from SESSION-ID."
  (or (gethash entry-id (e-session--entry-index store session-id))
      (seq-find (lambda (entry)
                  (equal (plist-get entry :id) entry-id))
                (e-session--entries store session-id))))

(defun e-session--entry-children (store session-id parent-id)
  "Return entries whose parent is PARENT-ID in SESSION-ID."
  (seq-filter (lambda (entry)
                (equal (plist-get entry :parent-id) parent-id))
              (e-session--entries store session-id)))

(defun e-session--keyword-plist-p (value)
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

(defun e-session-current-path (store session-id &optional head-id)
  "Return SESSION-ID current parent path ending at HEAD-ID or current head."
  (let* ((session (e-session--get-live store session-id))
         (head-id (or head-id (plist-get session :current-head-id)))
         path)
    (while head-id
      (let ((entry (e-session-entry-by-id store session-id head-id)))
        (unless entry
          (setq head-id nil))
        (when entry
          (push entry path)
          (setq head-id (plist-get entry :parent-id)))))
    path))

(defun e-session-entries-in-turn (store session-id turn-id)
  "Return entries in SESSION-ID that belong to TURN-ID."
  (seq-filter (lambda (entry)
                (equal (plist-get entry :turn-id) turn-id))
              (e-session-current-path store session-id)))

(defun e-session-entry-previous (store session-id entry-id)
  "Return the previous entry before ENTRY-ID on SESSION-ID current path."
  (when-let ((entry (e-session-entry-by-id store session-id entry-id)))
    (when-let ((parent-id (plist-get entry :parent-id)))
      (e-session-entry-by-id store session-id parent-id))))

(defun e-session-entry-next (store session-id entry-id)
  "Return the next entry after ENTRY-ID on SESSION-ID current path."
  (let ((path (e-session-current-path store session-id)))
    (cadr (member (e-session-entry-by-id store session-id entry-id) path))))

(defun e-session-latest-entry-of-type (store session-id type)
  "Return latest entry of TYPE on SESSION-ID current path."
  (seq-find (lambda (entry)
              (eq (plist-get entry :type) type))
            (reverse (e-session-current-path store session-id))))

(defun e-session-entries-from (store session-id first-entry-id)
  "Return current-path entries from FIRST-ENTRY-ID to the current head."
  (let ((path (e-session-current-path store session-id)))
    (member (e-session-entry-by-id store session-id first-entry-id) path)))

(defun e-session-entries-before (store session-id entry-id)
  "Return current-path entries before ENTRY-ID."
  (let ((entries nil)
        (done nil))
    (dolist (entry (e-session-current-path store session-id))
      (unless done
        (if (equal (plist-get entry :id) entry-id)
            (setq done t)
          (push entry entries))))
    (nreverse entries)))

(defun e-session-compaction-boundary-valid-p (store session-id compaction)
  "Return non-nil when COMPACTION points at an entry on the current path."
  (let ((boundary (plist-get compaction :first-kept-entry-id)))
    (and (stringp boundary)
         (e-session-entry-by-id store session-id boundary)
         (seq-some (lambda (entry)
                     (equal (plist-get entry :id) boundary))
                   (e-session-current-path store session-id)))))

(defun e-session-latest-valid-compaction (store session-id)
  "Return the latest compaction record with a valid current-path boundary."
  (seq-find
   (lambda (entry)
     (and (eq (plist-get entry :type) 'compaction)
          (e-session-compaction-boundary-valid-p store session-id entry)))
   (reverse (e-session-compactions store session-id))))

(defun e-session--checkpoint-tail (items limit)
  "Return the last at most LIMIT ITEMS without changing their order."
  (let ((count (length items)))
    (copy-sequence (nthcdr (max 0 (- count limit)) items))))

(defun e-session--checkpoint-board-messages (store session-id)
  "Return bounded board resume state for SESSION-ID in STORE.
Retain a recent presentation/activity tail and a separately bounded fact tail,
then restore their original board order without duplicates."
  (let* ((messages
          (e-session-board-journal-messages
           (e-session--board-journal store session-id)))
         (recent (e-session--checkpoint-tail
                  messages e-session-checkpoint-board-message-limit))
         (facts (e-session--checkpoint-tail
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

(defun e-session--checkpoint-path-suffix (store session-id)
  "Return SESSION-ID's resumable current-path suffix.
The latest valid compaction boundary is the earliest retained entry.  Without
a compaction, the complete current path remains model context and is retained."
  (let ((path (e-session-current-path store session-id)))
    (if-let* ((compaction (e-session-latest-valid-compaction store session-id))
              (boundary-id (plist-get compaction :first-kept-entry-id))
              (boundary (e-session-entry-by-id store session-id boundary-id))
              (suffix (member boundary path)))
        suffix
      path)))

(defun e-session--context-record (entry)
  "Return the provider-neutral context record carried by ENTRY."
  (copy-tree (plist-get entry :context-record)))

(defun e-session--context-lifetime-latest-entries
    (entries type key)
  "Return the latest ordered ENTRY of TYPE for each nested KEY value.

Context records are append-only lifecycle snapshots.  `entries' is already in
journal order, so the final occurrence for a logical id is authoritative.  A
hash table finds those occurrences without scanning the journal once per
frame; the final filter preserves their original order for checkpoint output."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (when (eq (plist-get entry :type) type)
        (let ((record (e-session--context-record entry)))
          (when (plist-member record key)
            (puthash (plist-get record key) entry latest)))))
    (seq-filter
     (lambda (entry)
       (and (eq (plist-get entry :type) type)
            (eq entry
                (gethash (plist-get (e-session--context-record entry) key)
                         latest))))
     entries)))

(defun e-session--context-lifetime-marker-p (record values)
  "Return non-nil when RECORD's :status is one of VALUES.

Symbols are used by the in-memory API and the replay normalizer restores the
same symbols after JSON decoding.  Accepting their wire string spelling here
keeps the reducer conservative but tolerant of a directly supplied persisted
record."
  (let ((status (plist-get record :status)))
    (or (memq status values)
        (and (stringp status)
             (member status (mapcar #'symbol-name values))))))

(defun e-session--context-lifetime-frame-state-p (record state)
  "Return non-nil when FRAME RECORD has lifecycle STATE."
  (let ((value (plist-get record :state)))
    (or (eq value state)
        (and (stringp value)
             (equal value (symbol-name state))))))

(defun e-session--context-record-sequence (record key)
  "Return RECORD's KEY value as a detached logical-id sequence."
  (let ((value (plist-get record key)))
    (cond
     ((null value) nil)
     ((vectorp value) (append value nil))
     ((listp value) (copy-sequence value))
     (t (list value)))))

(defun e-session--context-frame-unsettled-p (entry)
  "Return non-nil when ENTRY is a frame snapshot candidate for reduction.

Closure is not inferred from the nested snapshot state.  Only the final
acknowledged settlement marker (or an explicit durable abort) closes a logical
frame; in particular, a `:state settled' snapshot without its marker remains
recoverable."
  (eq (plist-get entry :type) 'context-frame))

(defun e-session--checkpoint-context-lifetime-state (store session-id)
  "Return bounded active generation and unsettled-frame checkpoint state.

The state is derived from the current path.  Closed frames, unreferenced or
superseded promotions, and superseded settlement attempts are intentionally
omitted from the resume manifest; the append-only journal remains the audit
source for them.  Returned entry ids follow journal order."
  (let* ((path (e-session--checkpoint-path-suffix store session-id))
         (generations
          (seq-filter (lambda (entry)
                        (eq (plist-get entry :type) 'context-generation))
                      path))
         (generation (car (last generations)))
         (generation-record (and generation
                                 (e-session--context-record generation)))
         (generation-id (plist-get generation-record :id))
         (frame-candidates
          (e-session--context-lifetime-latest-entries
           path 'context-frame :id))
         (settlement-candidates
          (e-session--context-lifetime-latest-entries
           path 'context-frame-settlement :frame-id))
         (promotion-candidates
          (e-session--context-lifetime-latest-entries
           path 'context-promotion :id))
         (_validated-frame-ownership
          (progn
            (when (and frame-candidates (not generation-id))
              (signal 'e-session-error
                      (list "Context frames have no active generation"
                            session-id)))
            (dolist (entry frame-candidates)
              (let ((record (e-session--context-record entry)))
                (unless (equal (plist-get record :generation-id)
                               generation-id)
                  (signal 'e-session-error
                          (list "Context frame has no active matching generation"
                                session-id
                                (plist-get record :id)
                                (plist-get record :generation-id)
                                generation-id)))))))
         (settled-frame-ids
          (let ((ids (make-hash-table :test #'equal)))
            (dolist (entry settlement-candidates)
              (let ((record (e-session--context-record entry)))
                (when (e-session--context-lifetime-marker-p
                       record '(acknowledged settled))
                  (puthash (plist-get record :frame-id) t ids))))
            ids))
         (aborted-frame-ids
          (let ((ids (make-hash-table :test #'equal)))
            ;; An explicit frame-aborted snapshot is itself a durable terminal
            ;; decision.  A settlement marker carrying :status aborted is
            ;; accepted as the equivalent durable abort marker.
            (dolist (entry frame-candidates)
              (let ((record (e-session--context-record entry)))
                (when (e-session--context-lifetime-frame-state-p
                       record 'aborted)
                  (puthash (plist-get record :id) t ids))))
            (dolist (entry settlement-candidates)
              (let ((record (e-session--context-record entry)))
                (when (e-session--context-lifetime-marker-p record '(aborted))
                  (puthash (plist-get record :frame-id) t ids))))
            ids))
         (frames
          (cl-remove-if-not
           (lambda (entry)
             (let ((record (e-session--context-record entry)))
               (and (e-session--context-frame-unsettled-p entry)
                    (not (gethash (plist-get record :id)
                                  settled-frame-ids))
                    (not (gethash (plist-get record :id)
                                  aborted-frame-ids))
                    (equal (plist-get record :generation-id)
                           generation-id))))
           frame-candidates))
         (frame-ids
          (let ((ids (make-hash-table :test #'equal)))
            (dolist (entry frames)
              (puthash (plist-get (e-session--context-record entry) :id)
                       t ids))
            ids))
         (referenced-promotion-ids
          (let ((ids (make-hash-table :test #'equal)))
            (dolist (entry frames)
              (dolist (promotion-id
                       (e-session--context-record-sequence
                        (e-session--context-record entry) :promotion-ids))
                (puthash promotion-id t ids)))
            ids))
         (promotions
          (cl-remove-if-not
           (lambda (entry)
             (and (eq (plist-get entry :type) 'context-promotion)
                  (gethash (plist-get (e-session--context-record entry) :id)
                           referenced-promotion-ids)
                  (gethash (plist-get (e-session--context-record entry)
                                      :frame-id)
                           frame-ids)))
           promotion-candidates))
         (settlements
          (seq-filter
           (lambda (entry)
             (and (eq (plist-get entry :type) 'context-frame-settlement)
                  (memq entry settlement-candidates)
                  (gethash (plist-get (e-session--context-record entry)
                                      :frame-id)
                           frame-ids)))
           path))
         (entries
          (seq-filter
           (lambda (entry)
             (or (eq entry generation)
                 (memq entry frames)
                 (memq entry promotions)
                 (memq entry settlements)))
           path)))
    (list :generation (and generation-record
                           (copy-tree generation-record))
          :frames (vconcat (mapcar #'e-session--context-record frames))
          :promotions (vconcat (mapcar #'e-session--context-record promotions))
          :settlements (vconcat (mapcar #'e-session--context-record settlements))
          :entry-ids (vconcat (mapcar (lambda (entry) (plist-get entry :id))
                                      entries)))))

(defun e-session--checkpoint-retained-entries (store session-id)
  "Return ordered durable entries needed to resume SESSION-ID."
  (let* ((session (e-session--get-live store session-id))
         (path (e-session--checkpoint-path-suffix store session-id))
         (path-ids (mapcar (lambda (entry) (plist-get entry :id)) path))
         (activity
          (e-session--checkpoint-tail
           (cl-remove-if-not
            (lambda (entry) (member (plist-get entry :id) path-ids))
            (plist-get session :activity-events))
           e-session-checkpoint-activity-event-limit))
         (latest-token (plist-get session :latest-token-usage-event))
         (reports
          (e-session--checkpoint-tail
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
         (context-state
          (e-session--checkpoint-context-lifetime-state store session-id))
         (required-ids
          (delq nil
                (append
                 (mapcar
                  (lambda (entry)
                    (when (memq (plist-get entry :type)
                                '(message branch-summary compaction))
                      (plist-get entry :id)))
                  path)
                 (mapcar (lambda (entry) (plist-get entry :id)) activity)
                 (and latest-token (list (plist-get latest-token :id)))
                 (mapcar (lambda (entry) (plist-get entry :id)) reports)
                 (mapcar (lambda (entry) (plist-get entry :id)) anchors)
                 (append (append (plist-get context-state :entry-ids) nil)
                         nil)
                 (list (plist-get session :current-head-id)
                       (and path (plist-get (car path) :id)))))))
    (cl-remove-if-not
     (lambda (entry)
       (and (not (equal (plist-get entry :id)
                        (e-session--root-event-id session)))
            (member (plist-get entry :id) required-ids)))
     path)))

(defun e-session--checkpoint-root (session)
  "Return compact current root state for SESSION."
  (list :id (e-session--root-event-id session)
        :created-at (plist-get session :created-at)
        :updated-at (plist-get session :updated-at)
        :metadata (copy-tree (plist-get session :metadata))
        :name (plist-get session :name)
        :turn-options (copy-tree (plist-get session :turn-options))
        :current-branch (plist-get session :current-branch)
        :board-output-sequence (or (plist-get session :board-output-sequence) 0)
        :board-activity-sequence
        (or (plist-get session :board-activity-sequence) 0)))

(defun e-session-checkpoint-manifest (store session-id)
  "Return JSON-friendly semantic resume manifest for SESSION-ID in STORE."
  (let* ((session (e-session--get-live store session-id))
         (entries (e-session--checkpoint-retained-entries store session-id))
         (board-messages
          (e-session--checkpoint-board-messages store session-id)))
    (e-session--freeze-board-value
     (list :session-id session-id
           :root (e-session--checkpoint-root session)
           :board-state (plist-get session :board-session-state)
           :context-lifetime
           (e-session--checkpoint-context-lifetime-state store session-id)
           :entry-ids
           (vconcat (mapcar (lambda (entry) (plist-get entry :id)) entries))
           :board-message-identities
           (vconcat
            (mapcar
             (lambda (message)
               (list :record-type
                     (or (plist-get message :record-type) 'board-message)
                     :id (plist-get message :id)))
             board-messages))))))

(defun e-session--checkpoint-entry-record (session-id entry parent-id)
  "Return replay record for SESSION-ID ENTRY reparented to PARENT-ID."
  (let ((timestamp (plist-get entry :created-at))
        (id (plist-get entry :id)))
    (pcase (plist-get entry :type)
      ('message
       (let ((message (copy-tree entry)))
         ;; `:durability-state' is an in-memory replay proof, not part of the
         ;; provider-neutral journal payload.
         (cl-remf message :durability-state)
         (plist-put message :parent-id parent-id)
         (list :type "message" :session-id session-id :timestamp timestamp
               :id id :parent-id parent-id :message message)))
      ('activity-event
       (list :type "activity-event" :session-id session-id
             :id id :parent-id parent-id
             :turn-id (plist-get entry :turn-id)
             :board-activity-sequence
             (plist-get entry :board-activity-sequence)
             :timestamp timestamp :event-type (plist-get entry :event-type)
             :payload (copy-tree (plist-get entry :payload))))
      ('branch-summary
       (list :type "branch-summary" :session-id session-id
             :id id :parent-id parent-id :timestamp timestamp
             :branch-id (plist-get entry :branch-id)
             :summary (plist-get entry :summary)
             :metadata (copy-tree (plist-get entry :metadata))))
      ('compaction
       (list :type "compaction" :session-id session-id
             :id id :parent-id parent-id :timestamp timestamp
             :summary (plist-get entry :summary)
             :branch-id (plist-get entry :branch-id)
             :range (copy-tree (plist-get entry :range))
             :first-kept-entry-id (plist-get entry :first-kept-entry-id)
             :tokens-before (plist-get entry :tokens-before)
             :tokens-kept (plist-get entry :tokens-kept)
             :metadata (copy-tree (plist-get entry :metadata))))
      ('provider-anchor
       (list :type "provider-anchor" :session-id session-id
             :id id :parent-id parent-id :timestamp timestamp
             :provider-id (plist-get entry :provider-id)
             :model (plist-get entry :model)
             :covered-entry-id (plist-get entry :covered-entry-id)
             :fingerprints
             (e-session--provider-anchor-fingerprints-for-json
              (copy-tree (plist-get entry :fingerprints)))
             :metadata (copy-tree (plist-get entry :metadata))))
      ('process-report
       (let ((report (copy-tree entry)))
         (cl-remf report :durability-state)
         (plist-put report :parent-id parent-id)
         (list :type "process-report" :session-id session-id
               :id id :parent-id parent-id :timestamp timestamp
               :report report)))
      ((or 'context-generation 'context-frame 'context-promotion
           'context-frame-settlement)
       (list :type (symbol-name (plist-get entry :type))
             :session-id session-id
             :id id
             :parent-id parent-id
             :timestamp timestamp
             :context-record
             (e-session--context-record-for-json
              (plist-get entry :context-record))))
      ('session-event
       (pcase (plist-get entry :event-type)
         ('current-branch
          (list :type "current-branch" :session-id session-id
                :id id :parent-id parent-id :timestamp timestamp
                :branch-id (plist-get entry :branch-id)))
         (_
          (list :type "session-info" :session-id session-id
                :id id :parent-id parent-id :timestamp timestamp))))
      (_
       (signal 'e-session-checkpoint-invalid
               (list session-id "Unsupported checkpoint entry"
                     (plist-get entry :type)))))))

(defun e-session--checkpoint-records (store session-id)
  "Return canonical replay records for SESSION-ID's current resume state."
  (let* ((session (e-session--get-live store session-id))
         (root (e-session--checkpoint-root session))
         (root-id (plist-get root :id))
         (records
          (list
           (append (list :type "session" :session-id session-id
                         :timestamp (plist-get root :created-at))
                   root)))
         (parent-id root-id))
    (when-let ((board-state (plist-get session :board-session-state)))
      (setq records
            (append records
                    (list (list :type "board-session-state"
                                :session-id session-id
                                :timestamp (plist-get root :updated-at)
                                :board-state (copy-tree board-state)
                                :board-id (plist-get board-state :board-id)
                                :principal (plist-get board-state :principal)
                                :board-output-sequence
                                (plist-get root :board-output-sequence)
                                :board-activity-sequence
                                (plist-get root :board-activity-sequence))))))
    (dolist (message (e-session--checkpoint-board-messages store session-id))
      (setq records
            (append records
                    (list (list :type "board-message" :session-id session-id
                                :message (copy-tree message))))))
    (dolist (entry (e-session--checkpoint-retained-entries store session-id))
      (setq records
            (append records
                    (list (e-session--checkpoint-entry-record
                           session-id entry parent-id))))
      (setq parent-id (plist-get entry :id)))
    records))

(defun e-session--provider-anchor-dynamic-segment-p (segment)
  "Return non-nil when SEGMENT is volatile current-state context."
  (let ((kind (and (e-session--keyword-plist-p segment)
                   (plist-get segment :kind))))
    (or (eq kind 'current-state)
        (eq kind 'dynamic-context)
        (equal kind "current-state")
        (equal kind "dynamic-context"))))

(defun e-session--provider-anchor-segment-list-p (segments)
  "Return non-nil when SEGMENTS is a list of segment plists."
  (and (proper-list-p segments)
       (cl-every (lambda (segment)
                   (and (e-session--keyword-plist-p segment)
                        (plist-member segment :kind)))
                 segments)))

(defun e-session--provider-anchor-stable-segments (fingerprints)
  "Return provider-anchor hard-identity segments from FINGERPRINTS."
  (let ((segments (and (e-session--keyword-plist-p fingerprints)
                       (plist-get fingerprints :segments))))
    (cond
     ((null segments) nil)
     ((e-session--provider-anchor-segment-list-p segments)
      (cl-remove-if
       #'e-session--provider-anchor-dynamic-segment-p
       segments))
     (t (list :invalid-provider-anchor-segments)))))

(defun e-session--provider-anchor-fingerprints-for-json (fingerprints)
  "Return FINGERPRINTS with list-of-plist fields encoded as JSON arrays."
  (if (not (e-session--keyword-plist-p fingerprints))
      fingerprints
    (let ((copy (copy-sequence fingerprints)))
      (when (plist-member copy :segments)
        (setq copy (plist-put copy
                              :segments
                              (vconcat (plist-get copy :segments)))))
      (when (plist-member copy :tools)
        (setq copy (plist-put copy
                              :tools
                              (vconcat (plist-get copy :tools)))))
      copy)))

(defun e-session-provider-anchor-incompatibility-reason
    (store session-id anchor provider-id model fingerprints)
  "Return why ANCHOR is not compatible, or nil when compatible."
  (let* ((path (e-session-current-path store session-id))
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
     ((not (equal (e-session--provider-anchor-stable-segments
                   anchor-fingerprints)
                  (e-session--provider-anchor-stable-segments
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
     ((and (not (or (plist-member anchor-fingerprints :segments)
                    (plist-member anchor-fingerprints :active-layer-ids)
                    (plist-member anchor-fingerprints :tools)
                    (plist-member anchor-fingerprints :reasoning)
                    (plist-member anchor-fingerprints :provider-options)
                    (plist-member anchor-fingerprints :compaction-boundary)))
           (not (equal anchor-fingerprints fingerprints)))
      'fingerprint-mismatch)
     ((not (member anchor-id path-ids))
      'anchor-not-on-current-path)
     ((not (member covered-entry-id path-ids))
      'covered-entry-not-on-current-path)
     (t nil))))

(defun e-session-provider-anchor-compatible-p
    (store session-id anchor provider-id model fingerprints)
  "Return non-nil when ANCHOR is compatible with current SESSION-ID state."
  (null
   (e-session-provider-anchor-incompatibility-reason
    store session-id anchor provider-id model fingerprints)))

(defun e-session--finalize-replayed-session (store session)
  "Restore replayed SESSION field ordering and derived metadata."
  (dolist (field e-session--replay-list-fields)
    (plist-put session field (nreverse (plist-get session field))))
  (let ((journal (e-session--board-journal store (plist-get session :id))))
    (setf (e-session-board-journal-messages journal)
          (nreverse (e-session-board-journal-messages journal))
          (e-session-board-journal-tail journal)
          (e-session--list-tail (e-session-board-journal-messages journal))))
  (e-session--initialize-list-state session)
  (cl-remf session :entry-count)
  (plist-put session :loaded t)
  (e-session--refresh-derived-fields store session)
  (e-session--index-session-entries store session)
  session)

(defun e-session--last-message-at (session)
  "Return SESSION's latest message timestamp, when it has messages."
  (when-let ((message (car (last (plist-get session :messages)))))
    (plist-get message :created-at)))

(defun e-session--message-assistant-marker (message)
  "Return MESSAGE's stable assistant read marker."
  (or (plist-get message :id)
      (plist-get message :created-at)))

(defun e-session--latest-assistant-marker (session)
  "Return SESSION's latest assistant message marker."
  (let (marker)
    (dolist (message (reverse (plist-get session :messages)))
      (when (and (not marker)
                 (eq (plist-get message :role) 'assistant))
        (setq marker (e-session--message-assistant-marker message))))
    marker))

(defun e-session--session-index-entry (store session)
  "Return public index metadata for SESSION in STORE."
  (e-session--refresh-file-field store session)
  (list :id (plist-get session :id)
        :name (plist-get session :name)
        :summary (plist-get session :summary)
        :metadata (plist-get session :metadata)
        :title (e-session--display-title-for-session session)
        :message-count (or (plist-get session :message-count) 0)
        :created-at (plist-get session :created-at)
        :updated-at (plist-get session :updated-at)
        :updated-seq (plist-get session :updated-seq)
        :last-message-at (or (plist-get session :last-message-at)
                             (e-session--last-message-at session))
        :latest-assistant-marker
        (or (plist-get session :latest-assistant-marker)
            (e-session--latest-assistant-marker session))
        :board-id
        (plist-get (plist-get session :board-session-state) :board-id)
        :principal
        (plist-get (plist-get session :board-session-state) :principal)
        :file (plist-get session :file)
        :loaded (plist-get session :loaded)))

(defun e-session--normalize-turn-options (options)
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

(defun e-session--write-index (store)
  "Write STORE's persistent session index."
  (e-session--profile-call
   'session.write-index
   (list :metadata (list :persistent (and (e-session--persistent-p store) t)))
   (lambda ()
     (when (e-session--persistent-p store)
       (if-let ((controller (e-session--persistence-controller store)))
           (e-session-persistence-request-checkpoint controller)
         (if (e-session--queued-writes-p store)
           (progn
             (e-session--schedule-write-queue store)
             (unless (e-session-store-index-write-pending store)
               (e-session--adjust-unsettled-writes store 1))
             (setf (e-session-store-index-write-pending store)
                   (e-session--queued-index-entry store)))
           (e-session--write-index-now store)))))))

(defun e-session-finalize (store on-done on-error)
  "Asynchronously finalize STORE's current durability boundary.
STORE must own the production persistence controller.  Call ON-DONE after its
checkpoint is acknowledged, or ON-ERROR when the writer rejects it."
  (if-let ((controller (e-session--persistence-controller store)))
      (e-session-persistence-finalize controller on-done on-error)
    (signal 'e-session-persistence-unavailable
            (list "Session store has no asynchronous persistence controller"))))

(defun e-session--validate-context-lifetime-prefix
    (store session-id prefix-entry-ids)
  "Return validated PREFIX-ENTRY-IDS and their replayed durability proofs.

An absent outbox mapping is not by itself evidence of durability: it may mean
that a caller supplied an unknown identity.  The session boundary owns the
authoritative entry index, so validate every named prefix identity here before
delegating acknowledgement to the persistence controller."
  (let ((ids
         (cond
          ((null prefix-entry-ids) nil)
          ((vectorp prefix-entry-ids) (append prefix-entry-ids nil))
          ((and (listp prefix-entry-ids)
                (proper-list-p prefix-entry-ids))
           (copy-sequence prefix-entry-ids))
          (t
           (signal 'e-session-error
                   (list "Context lifetime prefix requires a sequence"
                         session-id prefix-entry-ids))))))
    (let (replayed-entry-ids)
      (when ids
        (e-session--get-live store session-id)
        (dolist (entry-id ids)
          (let ((entry (e-session-entry-by-id store session-id entry-id)))
            (unless entry
              (signal 'e-session-error
                      (list "Context lifetime prefix names no session entry"
                            session-id entry-id)))
            (when (eq (plist-get entry :durability-state) 'replayed-durable)
              (push entry-id replayed-entry-ids)))))
      (list :entry-ids ids
            :replayed-entry-ids (nreverse replayed-entry-ids)))))

(defun e-session-context-lifetime-durability-barrier
    (store session-id prefix-entry-ids on-done on-error)
  "Await durability of named PREFIX-ENTRY-IDS for SESSION-ID.

This is a narrow session/prefix barrier.  It never finalizes every dirty
session, requests a checkpoint, or rebuilds the global index.  The production
persistence controller owns the asynchronous outbox acknowledgement; the
in-memory store is already durable at this boundary."
  (let* ((validated
          (e-session--validate-context-lifetime-prefix
           store session-id prefix-entry-ids))
         (prefix-entry-ids (plist-get validated :entry-ids))
         (replayed-entry-ids (plist-get validated :replayed-entry-ids)))
    (cond
     ((not (e-session-store-persistent store))
      (funcall on-done (list :session-id session-id
                             :entry-ids (copy-sequence prefix-entry-ids)
                             :pending-count 0)))
     ((not (e-session--persistence-controller store))
      (signal 'e-session-persistence-unavailable
              (list "Session store has no asynchronous persistence controller"
                    session-id)))
     (t
      (e-session-persistence-await-record-prefix
       (e-session--persistence-controller store)
       session-id prefix-entry-ids on-done on-error replayed-entry-ids)))))

(defun e-session--json-read-line (line)
  "Parse one JSONL LINE as a plist."
  (json-parse-string line
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun e-session--known-role (role)
  "Return ROLE normalized for the in-memory transcript."
  (if (stringp role)
      (intern role)
    role))

(defun e-session--known-display (display)
  "Return DISPLAY disposition normalized for the in-memory transcript."
  (if (stringp display)
      (intern display)
    display))

(defun e-session--normalize-message (message)
  "Return MESSAGE normalized after JSON replay."
  (plist-put message :role (e-session--known-role (plist-get message :role)))
  (when-let ((origin (plist-get message :origin)))
    (when (stringp origin)
      (plist-put message :origin (intern origin))))
  (when (plist-member message :display)
    (plist-put message :display
               (e-session--known-display (plist-get message :display))))
  message)

(defun e-session--known-event-type (event-type)
  "Return EVENT-TYPE normalized for in-memory activity events."
  (if (stringp event-type)
      (intern event-type)
    event-type))

(defun e-session--known-provider-id (provider-id)
  "Return PROVIDER-ID normalized for in-memory provider anchor records."
  (if (stringp provider-id)
      (intern provider-id)
    provider-id))

(defun e-session--normalize-activity-event (event)
  "Return EVENT normalized after JSON replay."
  (plist-put event
             :event-type
             (e-session--known-event-type (plist-get event :event-type)))
  (when (eq (plist-get event :event-type) 'hook-audit)
    (let ((payload (copy-sequence (plist-get event :payload))))
      (dolist (key '(:owner :outcome :truth-status))
        (when-let ((value (plist-get payload key)))
          (when (stringp value)
            (plist-put payload key (intern value)))))
      (plist-put event :payload payload)))
  event)

(defun e-session--normalize-board-message (message)
  "Return durable board MESSAGE normalized after JSON replay."
  (dolist (field '(:kind :mode :activity-kind :routing-state
                   :unrouted-reason :record-type :outcome :failure-policy))
    (when-let ((value (plist-get message field)))
      (when (stringp value)
        (plist-put message field (intern value)))))
  (plist-put message :tags
             (mapcar (lambda (tag) (if (stringp tag) (intern tag) tag))
                     (plist-get message :tags)))
  (when-let ((attributes (plist-get message :attributes)))
    (when-let ((status (plist-get attributes :status)))
      (when (stringp status)
        (plist-put attributes :status (intern status)))))
  message)

(defun e-session--update-activity-derived-fields (session event)
  "Update derived SESSION fields for appended activity EVENT."
  (when (eq (plist-get event :event-type) 'token-usage)
    (plist-put session :latest-token-usage-event event))
  event)

(defun e-session--message-with-created-at (message timestamp)
  "Return normalized MESSAGE with TIMESTAMP as its creation time when missing."
  (let ((normalized (e-session--normalize-message (copy-sequence message))))
    (unless (plist-member normalized :created-at)
      (plist-put normalized :created-at timestamp))
    normalized))

(defun e-session--replay-record (store record)
  "Replay persistent RECORD into STORE without appending it again."
  (let* ((type (plist-get record :type))
         (session-id (plist-get record :session-id))
         (timestamp (plist-get record :timestamp))
         (session (and session-id
                       (gethash session-id
                                (e-session-store-sessions store)))))
    (pcase type
      ("session"
       (e-session--clear-board-journal store session-id)
       (let* ((metadata (e-session--normalize-metadata-for-replay
                         (plist-get record :metadata)
                         t))
              (session (list :id session-id
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
                            :context-frames nil
                            :context-promotions nil
                            :context-frame-settlements nil
                            :created-at (or (plist-get record :created-at)
                                            timestamp)
                            :updated-at (or (plist-get record :updated-at)
                                            timestamp)
                            :turn-options
                            (e-session--normalize-turn-options
                             (plist-get record :turn-options))
                            :name (plist-get record :name))))
         (e-session--initialize-list-state session)
         (e-session--prepend-replayed-session-event
          session
          'session-created
          (or (plist-get record :created-at) timestamp)
          (list :metadata metadata)
          record)
         (e-session--touch store session (plist-get session :updated-at))
         (puthash session-id session (e-session-store-sessions store))))
      ("message"
       (when session
         (e-session--prepend-replayed-item
          session
          :messages
          (e-session--normalize-entry-from-record
           session
           'message
           (e-session--message-with-created-at
            (plist-get record :message)
            timestamp)
           timestamp
           record))
          (when-let ((sequence
                      (plist-get (car (plist-get session :messages))
                                 :board-output-sequence)))
            (plist-put session :board-output-sequence
                       (max (or (plist-get session :board-output-sequence) 0)
                            sequence)))
          (e-session--touch store session timestamp)))
      ("board-message"
       (when session
         (let* ((journal (e-session--board-journal store session-id))
                (message
                 (e-session--freeze-board-value
                  (e-session--normalize-board-message
                   (copy-tree (plist-get record :message)))))
                (existing (e-session--existing-board-message journal message)))
           (unless existing
             (puthash (e-session--board-message-identity message) message
                      (e-session-board-journal-id-index journal))
             (setf (e-session-board-journal-messages journal)
                   (cons message (e-session-board-journal-messages journal)))))
         (e-session--touch store session timestamp)))
      ("board-session-state"
       (when session
         (plist-put session :board-session-state
                    (copy-tree (plist-get record :board-state)))
         (e-session--touch store session timestamp)))
      ("board-messages-cleared"
       (when session
         (e-session--clear-board-journal store session-id)
         (e-session--touch store session timestamp)))
      ("message-display"
       (when session
         (when-let ((message
                     (seq-find (lambda (message)
                                 (equal (plist-get message :id)
                                        (plist-get record :id)))
                               (plist-get session :messages))))
           (let ((display (plist-get record :display)))
             (if display
                 (plist-put message :display
                            (e-session--known-display display))
               (cl-remf message :display))))
         (e-session--touch store session timestamp)))
      ("activity-event"
       (when session
        (let* ((event-data
                (list :id (plist-get record :id)
                      :parent-id (plist-get record :parent-id)
                      :turn-id (plist-get record :turn-id)
                      :event-type (plist-get record :event-type)
                      :payload (plist-get record :payload)
                      :created-at timestamp))
               (_ (when (plist-member record :board-activity-sequence)
                    (plist-put event-data :board-activity-sequence
                               (plist-get record :board-activity-sequence))))
               (event
                (e-session--normalize-entry-from-record
                 session
                 'activity-event
                 (e-session--normalize-activity-event event-data)
                 timestamp
                 record)))
          (e-session--prepend-replayed-item session :activity-events event)
          (when-let ((sequence (plist-get event :board-activity-sequence)))
            (plist-put session :board-activity-sequence
                       (max (or (plist-get session :board-activity-sequence) 0)
                            sequence)))
          (e-session--update-activity-derived-fields session event))
         (e-session--touch store session timestamp)))
      ("branch-summary"
       (when session
         (e-session--prepend-replayed-item
          session
          :branch-summaries
          (e-session--normalize-entry-from-record
           session
           'branch-summary
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :branch-id
                 (plist-get record :branch-id)
                 :summary
                 (plist-get record :summary)
                 :metadata
                 (plist-get record :metadata)
                 :created-at timestamp)
           timestamp
           record))
         (e-session--touch store session timestamp)))
      ("compaction"
       (when session
         (e-session--prepend-replayed-item
          session
          :compactions
          (e-session--normalize-entry-from-record
           session
           'compaction
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :summary
                 (plist-get record :summary)
                 :branch-id
                 (plist-get record :branch-id)
                 :range
                 (plist-get record :range)
                 :first-kept-entry-id
                 (plist-get record :first-kept-entry-id)
                 :tokens-before
                 (plist-get record :tokens-before)
                 :tokens-kept
                 (plist-get record :tokens-kept)
                 :metadata
                 (plist-get record :metadata)
                 :created-at timestamp)
           timestamp
           record))
         (e-session--touch store session timestamp)))
      ("provider-anchor"
       (when session
         (e-session--prepend-replayed-item
          session
          :provider-anchors
          (e-session--normalize-entry-from-record
           session
           'provider-anchor
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :provider-id
                 (e-session--known-provider-id
                  (plist-get record :provider-id))
                 :model
                 (plist-get record :model)
                 :covered-entry-id
                 (plist-get record :covered-entry-id)
                 :fingerprints
                 (plist-get record :fingerprints)
                 :metadata
                 (plist-get record :metadata)
                 :created-at timestamp)
           timestamp
           record))
         (e-session--touch store session timestamp)))
      ("process-report"
       (when session
         (e-session--prepend-replayed-item
          session
          :process-reports
          (e-session--normalize-entry-from-record
           session
           'process-report
           (plist-get record :report)
           timestamp
           record))
         (e-session--touch store session timestamp)))
      ((or "context-generation" "context-frame" "context-promotion"
           "context-frame-settlement")
       (when session
         (let* ((entry-type (intern type))
                (field (pcase entry-type
                         ('context-generation :context-generations)
                         ('context-frame :context-frames)
                         ('context-promotion :context-promotions)
                         ('context-frame-settlement
                          :context-frame-settlements)))
                (context-record
                 (e-session--normalize-context-record
                  entry-type
                  (e-session--normalize-context-record-for-replay
                   entry-type
                   (plist-get record :context-record))))
                (_ownership
                 (e-session--validate-context-entry-ownership
                  store session-id entry-type context-record))
                (entry
                 (e-session--normalize-entry-from-record
                  session entry-type
                  (list :context-record context-record)
                  timestamp
                  record)))
           (e-session--prepend-replayed-item session field entry)
           (e-session--touch store session timestamp))))
      ("current-branch"
       (when session
         (plist-put session :current-branch
                    (plist-get record :branch-id))
         (e-session--prepend-replayed-session-event
          session
          'current-branch
          timestamp
          (list :branch-id (plist-get record :branch-id))
          record)
         (e-session--touch store session timestamp)))
      ("session-info"
       (when session
         (let (fields)
           (when (plist-member record :name)
             (setq fields (plist-put fields :name (plist-get record :name))))
           (when (plist-member record :metadata)
             (setq fields (plist-put fields
                                     :metadata
                                     (e-session--normalize-metadata-for-replay
                                      (plist-get record :metadata)
                                      t))))
           (when (plist-member record :turn-options)
             (setq fields
                   (plist-put fields
                              :turn-options
                              (e-session--normalize-turn-options
                               (plist-get record :turn-options)))))
           (e-session--prepend-replayed-session-event
            session 'session-info timestamp fields record))
         (when (plist-member record :name)
           (plist-put session :name (plist-get record :name)))
         (when (plist-member record :metadata)
           (plist-put session
                      :metadata
                      (e-session--normalize-metadata-for-replay
                       (plist-get record :metadata)
                       t)))
         (when (plist-member record :turn-options)
           (plist-put session
                      :turn-options
                      (e-session--normalize-turn-options
                       (plist-get record :turn-options))))
         (e-session--touch store session timestamp)))
      ("messages-cleared"
       (when session
         (e-session--replace-list-field session :messages nil)
         (e-session--replace-list-field session :activity-events nil)
         (e-session--replace-list-field session :provider-anchors nil)
         (plist-put session :latest-token-usage-event nil)
         (e-session--clear-message-derived-fields store session)
         (plist-put session :current-head-id (e-session--root-event-id session))
         (e-session--prepend-replayed-session-event
          session
          'messages-cleared
          timestamp
          (list :parent-id (e-session--root-event-id session))
          record)
         (e-session--touch store session timestamp))))))

(defun e-session--checkpoint-json (store session-id offset)
  "Return SESSION-ID checkpoint JSON value at journal byte OFFSET."
  (list :version e-session-checkpoint-version
        :session-id session-id
        :journal-byte-offset offset
        :records (vconcat (e-session--checkpoint-records store session-id))
        :writer-high-watermarks nil))

(defun e-session--write-session-checkpoint-now (store session-id)
  "Atomically write SESSION-ID's current resume checkpoint from STORE."
  (let* ((journal (e-session--session-file store session-id))
         (target (e-session--checkpoint-file store session-id)))
    (unless (file-readable-p journal)
      (signal 'e-session-missing (list session-id journal)))
    (let* ((offset (file-attribute-size (file-attributes journal)))
           (temporary (make-temp-file (concat target ".") nil ".tmp"))
           (coding-system-for-write 'utf-8))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (insert (json-encode
                       (e-session--checkpoint-json store session-id offset))
                      "\n"))
            (rename-file temporary target t))
        (when (file-exists-p temporary)
          (delete-file temporary))))
    target))

(defun e-session--read-checkpoint (store session-id)
  "Read and validate SESSION-ID's resume checkpoint from STORE."
  (let ((file (e-session--checkpoint-file store session-id)))
    (unless (file-readable-p file)
      (signal 'e-session-checkpoint-missing (list session-id file)))
    (let ((checkpoint
           (condition-case err
               (e-session--json-read-file file)
             ((file-error json-parse-error)
              (signal 'e-session-checkpoint-invalid
                      (list session-id file err))))))
      (unless (and (= (or (plist-get checkpoint :version) -1)
                          e-session-checkpoint-version)
                   (equal (plist-get checkpoint :session-id) session-id)
                   (integerp (plist-get checkpoint :journal-byte-offset))
                   (>= (plist-get checkpoint :journal-byte-offset) 0)
                   (consp (plist-get checkpoint :records)))
        (signal 'e-session-checkpoint-invalid (list session-id file)))
      checkpoint)))

(defun e-session--begin-checkpoint-replay (store session-id checkpoint)
  "Install CHECKPOINT records as the replay prefix for SESSION-ID in STORE."
  (remhash session-id (e-session-store-sessions store))
  (e-session--clear-board-journal store session-id)
  (e-session--clear-entry-index store session-id)
  (dolist (record (plist-get checkpoint :records))
    (e-session--replay-record store record))
  (unless (gethash session-id (e-session-store-sessions store))
    (signal 'e-session-checkpoint-invalid
            (list session-id "Checkpoint has no session root"))))

(defun e-session--replay-jsonl-buffer (store)
  "Replay newline-delimited JSON records in the current unibyte buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (unless (string-empty-p line)
        (e-session--replay-record
         store
         (e-session--json-read-line (decode-coding-string line 'utf-8)))))
    (forward-line 1)))

(defun e-session--load-session-journal-fully (store session-id)
  "Replay all of SESSION-ID's journal for explicit offline migration only."
  (let ((file (e-session--session-file store session-id)))
    (unless (file-readable-p file)
      (signal 'e-session-missing (list session-id)))
    (remhash session-id (e-session-store-sessions store))
    (e-session--clear-board-journal store session-id)
    (e-session--clear-entry-index store session-id)
    (with-temp-buffer
      (let ((coding-system-for-read 'no-conversion))
        (insert-file-contents-literally file))
      (e-session--replay-jsonl-buffer store))
    (if-let ((session (gethash session-id (e-session-store-sessions store))))
        (e-session--finalize-replayed-session store session)
      (signal 'e-session-missing (list session-id)))))

(defun e-session-migrate-session-checkpoint (store session-id)
  "Offline-migrate SESSION-ID's complete journal to a resume checkpoint.
This explicit operation is the only checkpoint-less full-journal replay path."
  (e-session--load-session-journal-fully store session-id)
  (e-session--write-session-checkpoint-now store session-id))

(defun e-session-load (store)
  "Replay STORE's checkpointed persistent sessions from disk."
  (when (e-session--persistent-p store)
    (clrhash (e-session-store-sessions store))
    (clrhash (e-session-store-entry-indexes store))
    (clrhash (e-session-store-board-journals store))
    (setf (e-session-store-sequence store) 0)
    (let ((sessions-directory (e-session-store-sessions-directory store)))
      (when (file-directory-p sessions-directory)
        (dolist (file (directory-files sessions-directory t "\\.jsonl\\'"))
          (e-session-load-session store (file-name-base file))))))
  store)

(defun e-session--index-entry-session (store entry)
  "Return an unloaded session stub from index ENTRY in STORE."
  (let ((id (plist-get entry :id)))
    (when id
      (e-session--initialize-list-state
       (list :id id
             :metadata
             (e-session--normalize-metadata-for-replay
              (plist-get entry :metadata))
             :session-events nil
             :messages nil
             :activity-events nil
             :branch-summaries nil
             :current-branch nil
             :compactions nil
             :provider-anchors nil
             :process-reports nil
             :turn-options nil
             :created-at (plist-get entry :created-at)
             :updated-at (plist-get entry :updated-at)
             :updated-seq (or (plist-get entry :updated-seq) 0)
             :name (plist-get entry :name)
             :summary (plist-get entry :summary)
             :message-count (or (plist-get entry :message-count) 0)
             :last-message-at (plist-get entry :last-message-at)
             :board-id (plist-get entry :board-id)
             :principal (plist-get entry :principal)
             :board-session-state
             (when (and (plist-get entry :board-id)
                        (plist-get entry :principal))
               (list :board-id (plist-get entry :board-id)
                     :principal (plist-get entry :principal)))
             :file (or (plist-get entry :file)
                       (e-session--session-file store id))
             :loaded nil)))))

(defun e-session--put-index-entry (store entry)
  "Add index ENTRY to STORE as an unloaded session."
  (when-let ((session (e-session--index-entry-session store entry)))
    (puthash (plist-get session :id)
             session
             (e-session-store-sessions store))
    (setf (e-session-store-sequence store)
          (max (e-session-store-sequence store)
               (or (plist-get session :updated-seq) 0)))
    session))

(defun e-session--json-read-file (file)
  "Parse JSON FILE as a plist/list value."
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (insert-file-contents file)
      (json-parse-string (buffer-string)
                         :object-type 'plist
                         :array-type 'list
                         :null-object nil
                         :false-object :json-false))))

(defun e-session--index-key-id (key)
  "Return a session id string for object-shaped index KEY."
  (cond
   ((keywordp key) (string-remove-prefix ":" (symbol-name key)))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)))

(defun e-session--normalize-index-entry (entry &optional fallback-id)
  "Return normalized index ENTRY, using FALLBACK-ID when needed."
  (when (listp entry)
    (let ((entry (copy-sequence entry)))
      (unless (plist-get entry :id)
        (when fallback-id
          (plist-put entry :id fallback-id)))
      (when (and (not (plist-get entry :name))
                 (not (plist-get entry :summary))
                 (plist-get entry :title))
        (plist-put entry :summary (plist-get entry :title)))
      entry)))

(defun e-session--index-entries (value)
  "Return normalized session index entries from parsed JSON VALUE."
  (cond
   ((and (consp value)
         (listp (car value))
         (plist-member (car value) :id))
    (delq nil (mapcar #'e-session--normalize-index-entry value)))
   ((and (consp value)
         (keywordp (car value)))
    (let (entries)
      (while value
        (let* ((key (pop value))
               (entry (pop value))
               (id (e-session--index-key-id key)))
          (when-let ((normalized
                      (e-session--normalize-index-entry entry id)))
            (push normalized entries))))
      (nreverse entries)))))

(defun e-session--read-index-entries (store)
  "Return STORE's parsed persistent index entries, or nil when unavailable."
  (when (and (e-session--persistent-p store)
             (file-readable-p (e-session-store-index-file store)))
    (let ((value (condition-case nil
                     (e-session--json-read-file
                      (e-session-store-index-file store))
                   (file-error nil)
                   (json-parse-error nil))))
      (e-session--index-entries value))))

(defun e-session--load-index (store)
  "Load STORE session metadata from its persistent index file."
  (when-let ((entries (e-session--read-index-entries store)))
    (clrhash (e-session-store-sessions store))
    (clrhash (e-session-store-entry-indexes store))
    (clrhash (e-session-store-board-journals store))
    (setf (e-session-store-sequence store) 0)
    (dolist (entry entries)
      (e-session--put-index-entry store entry))
    t))

(defun e-session-refresh-index-metadata (store)
  "Refresh unloaded session metadata in STORE from its persistent index.
Preserve session objects and all loaded mutable state.  This is the narrow
reload path for a retained index-backed store: it applies updated index-stub
interpretation without replaying journals or replacing lifecycle ownership."
  (when-let ((entries (e-session--read-index-entries store)))
    (dolist (entry entries)
      (when-let ((session
                  (gethash (plist-get entry :id)
                           (e-session-store-sessions store))))
        (unless (plist-get session :loaded)
          (plist-put session :metadata
                     (e-session--normalize-metadata-for-replay
                      (plist-get entry :metadata)))))))
  store)

(defun e-session--load-index-from-session-files (store &optional only-missing)
  "Populate STORE metadata from session root records.
When ONLY-MISSING is non-nil, preserve catalog entries already loaded from the
derived index.  This reconciles journals committed after the last catalog
checkpoint without making an older index hide a new session."
  (let ((sessions-directory (e-session-store-sessions-directory store)))
    (when (file-directory-p sessions-directory)
      (dolist (file (directory-files sessions-directory t "\\.jsonl\\'"))
        ;; A current catalog must make listing transcript-free.  File names
        ;; match session ids, so only a journal absent from the catalog needs
        ;; its root record read during reconciliation.
        (let ((session-id (file-name-base file)))
          (when (or (not only-missing)
                    (not (gethash session-id (e-session-store-sessions store))))
            (condition-case nil
                (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8))
                    (insert-file-contents file nil 0 65536))
                  (goto-char (point-min))
                  (let* ((line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                         (record (and (not (string-empty-p line))
                                      (e-session--json-read-line line))))
                    (when (equal (plist-get record :type) "session")
                      (e-session--put-index-entry
                       store
                       (list :id (plist-get record :session-id)
                             :created-at (or (plist-get record :created-at)
                                             (plist-get record :timestamp))
                             :updated-at (or (plist-get record :updated-at)
                                             (plist-get record :timestamp))
                             :message-count 0
                             :file file)))))
              (file-error nil)
              (json-parse-error nil))))))))

(cl-defun e-session-persistent-index-store-create (&optional directory
                                                             &key write-mode)
  "Create a persistent STORE with session metadata loaded from the index.
Transcript JSONL files are loaded on demand when a session's messages or mutable
state are requested."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory e-session-directory))))
         (sessions-directory (expand-file-name "sessions" directory))
         (store (e-session-store-create
                 :directory directory
                 :sessions-directory sessions-directory
                 :index-file (expand-file-name "index.json" directory)
                 :persistent t
                 :write-mode write-mode)))
    (if (e-session--load-index store)
        (e-session--load-index-from-session-files store t)
      (e-session--load-index-from-session-files store))
    store))

(defun e-session-load-session (store session-id)
  "Load SESSION-ID checkpoint and journal suffix from persistent STORE."
  (unless (e-session--persistent-p store)
    (signal 'e-session-missing (list session-id)))
  (let* ((file (e-session--session-file store session-id))
         (checkpoint (e-session--read-checkpoint store session-id))
         (offset (plist-get checkpoint :journal-byte-offset)))
    (unless (file-readable-p file)
      (signal 'e-session-missing (list session-id)))
    (let ((file-size (file-attribute-size (file-attributes file))))
      (when (> offset file-size)
        (signal 'e-session-checkpoint-invalid
                (list session-id "Checkpoint offset exceeds journal size"
                      offset file-size))))
    (e-session--begin-checkpoint-replay store session-id checkpoint)
    (with-temp-buffer
      (let ((coding-system-for-read 'no-conversion))
        (insert-file-contents-literally file nil offset))
      (e-session--replay-jsonl-buffer store))
    (if-let ((session (gethash session-id (e-session-store-sessions store))))
        (e-session--finalize-replayed-session store session)
      (signal 'e-session-missing (list session-id)))))

(cl-defun e-session-load-session-start
    (store session-id &key on-done on-error on-progress chunk-bytes)
  "Start cooperatively loading SESSION-ID checkpoint and journal suffix.
Return an `e-request-lifecycle' request.  ON-DONE receives the loaded session,
ON-ERROR receives a condition list, and ON-PROGRESS receives byte progress."
  (unless (e-session--persistent-p store)
    (signal 'e-session-missing (list session-id)))
  (let* ((file (e-session--session-file store session-id))
         (checkpoint (e-session--read-checkpoint store session-id))
         (checkpoint-offset (plist-get checkpoint :journal-byte-offset))
         (chunk-bytes (max 1 (or chunk-bytes e-session-load-chunk-bytes))))
    (unless (file-readable-p file)
      (signal 'e-session-missing (list session-id)))
    (e-session--begin-checkpoint-replay store session-id checkpoint)
    (let* ((file-size (file-attribute-size (file-attributes file)))
           (_ (when (> checkpoint-offset file-size)
                (signal 'e-session-checkpoint-invalid
                        (list session-id
                              "Checkpoint offset exceeds journal size"
                              checkpoint-offset file-size))))
           (position checkpoint-offset)
           (carry "")
           timer
           request)
      (cl-labels
          ((clear-timer ()
             (when (timerp timer)
               (cancel-timer timer))
             (setq timer nil))
           (progress ()
             (let ((payload (list :session-id session-id
                                  :bytes-read position
                                  :bytes-total file-size)))
               (e-request-progress request payload)
               (when on-progress
                 (funcall on-progress payload))))
           (fail (err)
             (unless (e-request-terminal-p request)
               (clear-timer)
               (e-request-fail request err)
               (when on-error
                 (funcall on-error err))))
           (finish ()
             (unless (e-request-terminal-p request)
               (clear-timer)
               (condition-case err
                   (progn
                     (unless (string-empty-p carry)
                       (e-session--replay-record
                        store
                        (e-session--json-read-line
                         (decode-coding-string carry 'utf-8))))
                     (if-let ((session
                               (gethash session-id
                                        (e-session-store-sessions store))))
                         (let ((session
                                (e-session--finalize-replayed-session
                                 store session)))
                           (e-request-finish request session)
                           (when on-done
                             (funcall on-done session)))
                       (signal 'e-session-missing (list session-id))))
                 (error
                  (fail err)))))
           (schedule ()
             (setq timer (run-at-time 0 nil #'step)))
           (process-lines (text final-newline)
             (let* ((joined (concat carry text))
                    (lines (split-string joined "\n")))
               (setq carry (if final-newline "" (car (last lines))))
               (dolist (line (if final-newline lines (butlast lines)))
                 (unless (string-empty-p line)
                   (e-session--replay-record
                    store
                    (e-session--json-read-line
                     (decode-coding-string line 'utf-8)))))))
           (step ()
             (unless (e-request-terminal-p request)
               (condition-case err
                   (if (>= position file-size)
                       (finish)
                     (let* ((next-position
                             (min file-size (+ position chunk-bytes)))
                            text)
                       (with-temp-buffer
                         (let ((coding-system-for-read 'no-conversion))
                           (insert-file-contents-literally
                            file nil position next-position))
                         (setq text (buffer-string)))
                       (setq position next-position)
                       (process-lines
                        text
                        (or (string-empty-p text)
                            (string-suffix-p "\n" text)))
                       (progress)
                       (schedule)))
                 (error
                  (fail err))))))
        (setq request
              (e-request-lifecycle-create
               :id (e-session-generate-ulid)
               :owner 'e-session-load
               :session-id session-id
               :state 'created
               :cancel-function (lambda (_request)
                                  (clear-timer))))
        (e-request-start request (list :session-id session-id
                                       :bytes-total file-size))
        (schedule)
        request))))

(defun e-session--peek-session (store session-id)
  "Return SESSION-ID metadata from STORE without forcing transcript replay."
  (or (gethash session-id (e-session-store-sessions store))
      (signal 'e-session-missing (list session-id))))

(cl-defun e-session-persistent-store-create (&optional directory
                                                       &key write-mode)
  "Create and load a persistent session store rooted at DIRECTORY."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory e-session-directory))))
         (sessions-directory (expand-file-name "sessions" directory))
         (store (e-session-store-create
                 :directory directory
                 :sessions-directory sessions-directory
                 :index-file (expand-file-name "index.json" directory)
                 :persistent t
                 :write-mode write-mode)))
    (e-session-load store)
    store))

(cl-defun e-session-create (store &key id metadata)
  "Create a session in STORE with ID and METADATA."
  (setq id (or id (e-session--generate-id)))
  (when (gethash id (e-session-store-sessions store))
    (signal 'e-session-duplicate (list id)))
  (setq metadata (e-session--validate-metadata
                  (e-session--normalize-metadata-for-replay metadata)))
  (let* ((timestamp (e-session--timestamp))
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
                        :context-frames nil
                        :context-promotions nil
                        :context-frame-settlements nil
                        :turn-options nil
                        :created-at timestamp
                        :updated-at timestamp
                        :name (plist-get metadata :name)
                        :loaded t)))
    (e-session--initialize-list-state session)
    (let ((root (e-session--append-session-event
                 session
                 'session-created
                 timestamp
                 (list :metadata metadata))))
      (plist-put session :root-event-id (plist-get root :id)))
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
    (puthash id session (e-session-store-sessions store))
    (e-session--index-session-entries store session)
    (e-session--append-record
     store id
     (list :type "session"
           :session-id id
           :id (e-session--root-event-id session)
           :timestamp timestamp
           :created-at timestamp
           :updated-at timestamp
           :metadata metadata))
    (e-session--write-index store)
    session))

(defun e-session--board-journal (store session-id)
  "Return STORE's private board journal for SESSION-ID."
  (or (gethash session-id (e-session-store-board-journals store))
      (puthash session-id
               (e-session--board-journal-create)
               (e-session-store-board-journals store))))

(defun e-session--clear-board-journal (store session-id)
  "Remove STORE's private board journal for SESSION-ID."
  (remhash session-id (e-session-store-board-journals store)))

(defun e-session--freeze-board-value (value)
  "Return VALUE detached from mutable board-journal input.
Signal `e-session-board-message-cycle' for cyclic conses, vectors, and hash
 tables."
  (let ((visiting (make-hash-table :test 'eq)))
    (cl-labels
        ((copy-value (current)
           (cond
            ((stringp current) (copy-sequence current))
            ((consp current)
             (when (gethash current visiting)
               (signal 'e-session-board-message-cycle (list 'cons)))
             (puthash current t visiting)
             (unwind-protect
                 (cons (copy-value (car current))
                       (copy-value (cdr current)))
               (remhash current visiting)))
            ((vectorp current)
             (when (gethash current visiting)
               (signal 'e-session-board-message-cycle (list 'vector)))
             (puthash current t visiting)
             (unwind-protect
                 (vconcat (mapcar #'copy-value current))
               (remhash current visiting)))
            ((hash-table-p current)
             (when (gethash current visiting)
               (signal 'e-session-board-message-cycle (list 'hash-table)))
             (puthash current t visiting)
             (unwind-protect
                 (let ((copy (make-hash-table :test (hash-table-test current)
                                              :size (hash-table-size current))))
                   (maphash (lambda (key item)
                              (puthash (copy-value key) (copy-value item) copy))
                            current)
                   copy)
               (remhash current visiting)))
            (t current))))
      (copy-value value))))

(defun e-session-board-messages (store session-id)
  "Return SESSION-ID's durable board envelopes in board order."
  (e-session--get-live store session-id)
  (e-session--freeze-board-value
   (e-session-board-journal-messages
    (e-session--board-journal store session-id))))

(defun e-session--canonical-board-record-type (record-type)
  "Return supported RECORD-TYPE in the board journal's representation.
Nil means an ordinary board message."
  (pcase record-type
    (`nil nil)
    ((or 'processing-chain "processing-chain") 'processing-chain)
    ((or 'processing-result "processing-result") 'processing-result)
    (_
     (signal 'e-session-board-message-invalid-record-type
             (list record-type)))))

(defun e-session--board-message-identity (message)
  "Return the durable journal identity for board MESSAGE.
Processing records have a record type, while ordinary board messages occupy the
untyped board-message namespace.  The pair prevents equal raw ids from
silently replacing records from another namespace."
  (cons (or (e-session--canonical-board-record-type
             (plist-get message :record-type))
            'board-message)
        (plist-get message :id)))

(defun e-session--existing-board-message (journal message)
  "Return MESSAGE's retained duplicate, or signal for a typed conflict."
  (let* ((identity (e-session--board-message-identity message))
         (existing (gethash identity (e-session-board-journal-id-index journal))))
    (when (and existing
               (plist-get message :record-type)
               (not (equal existing message)))
      (signal 'e-session-board-message-conflict
              (list identity existing message)))
    existing))

(defun e-session-append-board-message (store session-id message)
  "Append one immutable board MESSAGE envelope to SESSION-ID's board log."
  (e-session--get-live store session-id)
  (let* ((journal (e-session--board-journal store session-id))
         (message (e-session--freeze-board-value message))
         (record-type
          (e-session--canonical-board-record-type
           (plist-get message :record-type)))
         (_ (when record-type
              (plist-put message :record-type record-type)))
         (existing (e-session--existing-board-message journal message)))
    (unless existing
      (puthash (e-session--board-message-identity message) message
               (e-session-board-journal-id-index journal))
      (let ((cell (list message)))
        (if-let ((tail (e-session-board-journal-tail journal)))
            (setcdr tail cell)
          (setf (e-session-board-journal-messages journal) cell))
        (setf (e-session-board-journal-tail journal) cell))
      (let ((session (e-session--get-live store session-id)))
        (e-session--touch store session (e-session--timestamp)))
      (e-session--append-record
       store session-id
       (list :type "board-message" :session-id session-id
             :message (e-session--freeze-board-value message))))
    (e-session--freeze-board-value (or existing message))))

(defun e-session-clear-board-messages (store session-id)
  "Clear SESSION-ID's durable board log and derived identity index."
  (let ((journal (e-session--board-journal store session-id))
        (session (e-session--get-live store session-id)))
    (setf (e-session-board-journal-messages journal) nil
          (e-session-board-journal-tail journal) nil
          (e-session-board-journal-id-index journal) (make-hash-table :test 'equal))
    (e-session--touch store session (e-session--timestamp))
    (e-session--append-record
     store session-id
     (list :type "board-messages-cleared" :session-id session-id))
    nil))

(defun e-session-declare-board-state (store session-id principal board-id)
  "Persist SESSION-ID's board identity and PRINCIPAL."
  (let* ((session (e-session--get-live store session-id))
         (board-state (list :board-id board-id :principal principal)))
    (plist-put session :board-session-state (copy-tree board-state))
    (e-session--append-record
     store session-id
     (list :type "board-session-state" :session-id session-id
           :board-state board-state :board-id board-id :principal principal
           :board-output-sequence
           (or (plist-get session :board-output-sequence) 0)
           :board-activity-sequence
           (or (plist-get session :board-activity-sequence) 0)))
    (e-session--write-index store)
    board-state))

(defun e-session--fork-message-seed (message)
  "Return MESSAGE stripped of source-session identity for fork replay.
The fork rebuilds a fresh linear parent chain, so durable identity fields
(`:id', `:parent-id') and the source turn grouping (`:turn-id') are dropped;
the re-append path mints new ones anchored on the fork's own head."
  (let ((seed (copy-sequence message)))
    (dolist (key '(:id :parent-id :turn-id))
      (setq seed (e-session--plist-remove seed key)))
    seed))

(cl-defun e-session-fork (store session-id &key at metadata name)
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
  (let* ((source (e-session--get-live store session-id))
         (head-id (or at (plist-get source :current-head-id)))
         (path (e-session-current-path store session-id head-id))
         (messages (seq-filter (lambda (entry)
                                 (eq (plist-get entry :type) 'message))
                               path))
         (base-metadata (copy-sequence (plist-get source :metadata)))
         (merged-metadata (e-session--merge-metadata base-metadata metadata))
         (merged-metadata (if name
                              (plist-put merged-metadata :name name)
                            merged-metadata))
         (turn-options (plist-get source :turn-options))
         (fork (e-session-create store :metadata merged-metadata)))
    (dolist (message messages)
      (e-session-append-message store (plist-get fork :id)
                                (e-session--fork-message-seed message)))
    (when turn-options
      (e-session-set-turn-options store (plist-get fork :id) turn-options))
    (e-session-get store (plist-get fork :id))))

(defun e-session--get-live (store session-id)
  "Return the mutable live SESSION-ID state from STORE."
  (let ((session (e-session--peek-session store session-id)))
    (if (and (e-session--persistent-p store)
             (not (plist-get session :loaded)))
        (e-session-load-session store session-id)
      session)))

(defun e-session-get (store session-id)
  "Return SESSION-ID's mutable generic session state from STORE.
Board journal state is owned privately by STORE and is available only through
its dedicated board journal accessors."
  (e-session--get-live store session-id))

(defun e-session-messages (store session-id)
  "Return messages for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session--get-live store session-id) :messages)))

(defun e-session-activity-events (store session-id)
  "Return durable activity events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session--get-live store session-id) :activity-events)))

(defun e-session-latest-token-usage-event (store session-id)
  "Return the latest durable token usage event for SESSION-ID in STORE."
  (plist-get (e-session--get-live store session-id) :latest-token-usage-event))

(defun e-session-session-events (store session-id)
  "Return durable session events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session--get-live store session-id) :session-events)))

(defun e-session-compactions (store session-id)
  "Return compaction records for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session--get-live store session-id) :compactions)))

(defun e-session-provider-anchors (store session-id)
  "Return provider anchor records for SESSION-ID in STORE in insertion order."
  (copy-sequence
   (plist-get (e-session--get-live store session-id) :provider-anchors)))

(defun e-session-context-generations (store session-id)
  "Return context generation entries for SESSION-ID in insertion order."
  (copy-tree
   (plist-get (e-session--get-live store session-id) :context-generations)))

(defun e-session-context-frames (store session-id)
  "Return context frame entries for SESSION-ID in insertion order."
  (copy-tree
   (plist-get (e-session--get-live store session-id) :context-frames)))

(defun e-session-context-promotions (store session-id)
  "Return context promotion entries for SESSION-ID in insertion order."
  (copy-tree
   (plist-get (e-session--get-live store session-id) :context-promotions)))

(defun e-session-context-frame-settlements (store session-id)
  "Return frame settlement entries for SESSION-ID in insertion order."
  (copy-tree
   (plist-get (e-session--get-live store session-id)
              :context-frame-settlements)))

(defun e-session-context-lifetime-resume-state (store session-id)
  "Return bounded active generation and unsettled frame resume state."
  (copy-tree (e-session--checkpoint-context-lifetime-state store session-id)))

(defun e-session-process-reports (store session-id)
  "Return process reports for SESSION-ID in STORE in insertion order."
  (copy-sequence
   (plist-get (e-session--get-live store session-id) :process-reports)))

(cl-defun e-session-latest-compatible-provider-anchor
    (store session-id provider-id &key model fingerprints)
  "Return latest provider anchor compatible with SESSION-ID current path."
  (seq-find
   (lambda (anchor)
     (e-session-provider-anchor-compatible-p
      store session-id anchor provider-id model fingerprints))
   (reverse (e-session-provider-anchors store session-id))))

(defun e-session-turn-options (store session-id)
  "Return session-scoped turn options for SESSION-ID in STORE."
  (copy-sequence (plist-get (e-session--get-live store session-id) :turn-options)))

(defun e-session--replace-metadata (store session-id metadata)
  "Replace SESSION-ID METADATA in STORE after validation."
  (let* ((metadata (e-session--validate-metadata metadata))
         (session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (event (e-session--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :metadata metadata))))
    (e-session--index-entry store session-id event)
    (plist-put session :metadata metadata)
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "session-info"
           :session-id session-id
           :id (plist-get event :id)
           :parent-id (plist-get event :parent-id)
           :timestamp timestamp
           :metadata metadata))
    (e-session--write-index store)
    metadata))

(defun e-session-set-metadata (store session-id metadata)
  "Replace SESSION-ID METADATA in STORE.
This compatibility path validates that every key has a durable state schema.
New code should prefer the narrower typed metadata helpers."
  (e-session--replace-metadata store session-id metadata))

(defun e-session--merge-metadata (metadata updates)
  "Return METADATA with UPDATES applied."
  (let ((metadata (copy-sequence metadata)))
    (while (consp updates)
      (let ((key (pop updates)))
        (when (consp updates)
          (setq metadata (plist-put metadata key (pop updates))))))
    metadata))

(defun e-session-set-session-config (store session-id config)
  "Merge durable session CONFIG into SESSION-ID metadata."
  (e-session--validate-metadata-class config 'session-config)
  (let* ((session (e-session--get-live store session-id))
         (metadata (e-session--merge-metadata
                    (plist-get session :metadata)
                    config)))
    (e-session--replace-metadata store session-id metadata)))

(defun e-session-metadata-context-references (metadata owner)
  "Return current-state references for OWNER from session METADATA.
This transcript-free reader accepts metadata from either a live session or a
session catalog entry."
  (let* ((references (plist-get metadata :context-references))
         (owner-key (e-session--metadata-owner-key owner)))
    (copy-tree
     (e-session--metadata-public-value
      (plist-get references owner-key)))))

(defun e-session-context-references (store session-id owner)
  "Return current-state references for OWNER in SESSION-ID."
  (e-session-metadata-context-references
   (plist-get (e-session--get-live store session-id) :metadata)
   owner))

(defun e-session-set-context-references (store session-id owner references)
  "Set durable current-state REFERENCES for OWNER in SESSION-ID."
  (let* ((owner-key (e-session--metadata-owner-key owner))
         (session (e-session--get-live store session-id))
         (metadata (copy-sequence (plist-get session :metadata)))
         (all-references (copy-sequence
                          (plist-get metadata :context-references))))
    (setq all-references
          (plist-put all-references
                     owner-key
                     (e-session--metadata-json-array-safe-value references)))
    (e-session--replace-metadata
     store
     session-id
     (plist-put metadata :context-references all-references))
    references))

(defun e-session-set-context-reference (store session-id key reference)
  "Set durable current-state REFERENCE metadata KEY for SESSION-ID."
  (e-session--validate-metadata-class (list key reference)
                                      'current-state-reference)
  (let* ((session (e-session--get-live store session-id))
         (metadata (e-session--merge-metadata
                    (plist-get session :metadata)
                    (list key reference))))
    (e-session--replace-metadata store session-id metadata)))

(defun e-session-capability-state (store session-id capability-id)
  "Return durable capability state for CAPABILITY-ID in SESSION-ID."
  (let* ((metadata (plist-get (e-session--get-live store session-id) :metadata))
         (state (plist-get metadata :capability-state))
         (owner-key (e-session--metadata-owner-key capability-id)))
    (copy-tree
     (e-session--metadata-public-value
      (plist-get state owner-key)))))

(cl-defun e-session-set-capability-state
    (store session-id capability-id state &key version)
  "Set durable capability STATE for CAPABILITY-ID in SESSION-ID."
  (let* ((owner-key (e-session--metadata-owner-key capability-id))
         (session (e-session--get-live store session-id))
         (metadata (copy-sequence (plist-get session :metadata)))
         (all-state (copy-sequence (plist-get metadata :capability-state)))
         (entry (if version
                    (list :version version :state state)
                  state)))
    (setq all-state (plist-put all-state owner-key entry))
    (e-session--replace-metadata
     store
     session-id
     (plist-put metadata :capability-state all-state))
    entry))

(defun e-session-set-turn-options (store session-id options)
  "Replace SESSION-ID turn OPTIONS in STORE."
  (let* ((session (e-session--get-live store session-id))
         (turn-options (e-session--normalize-turn-options options))
         (timestamp (e-session--timestamp))
         (event (e-session--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :turn-options turn-options))))
    (e-session--index-entry store session-id event)
    (plist-put session :turn-options turn-options)
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "session-info"
           :session-id session-id
           :id (plist-get event :id)
           :parent-id (plist-get event :parent-id)
           :timestamp timestamp
           :turn-options turn-options))
    (e-session--write-index store)
    turn-options))

(defun e-session-append-message (store session-id message)
  "Append MESSAGE to SESSION-ID in STORE."
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
          (message (e-session--normalize-entry-from-record
                    session
                    'message
                    (e-session--message-with-created-at message timestamp)
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
    (e-session--append-list-item session :messages message)
    (e-session--index-entry store session-id message)
    (e-session--touch store session timestamp)
    (e-session--update-message-derived-fields-on-append store session message)
    (e-session--append-record
     store session-id
     (list :type "message"
           :session-id session-id
           :timestamp timestamp
           :id (plist-get message :id)
           :parent-id (plist-get message :parent-id)
           :message message))
    (e-session--write-index store)
    message))

(defun e-session--message-by-id (store session-id message-id)
  "Return SESSION-ID's message with MESSAGE-ID in STORE, or nil.
Prefers the entry-id index; falls back to a scan of `:messages' so a message
appended before an index rebuild is still found."
  (or (let ((entry (gethash message-id (e-session--entry-index store session-id))))
        (and entry (eq (plist-get entry :type) 'message) entry))
      (seq-find (lambda (message)
                  (equal (plist-get message :id) message-id))
                (plist-get (e-session--get-live store session-id) :messages))))

(defun e-session-set-message-display (store session-id message-id display)
  "Set DISPLAY on SESSION-ID's message MESSAGE-ID in STORE and persist it.
DISPLAY is a display disposition symbol (e.g. `hidden'); nil clears it back to
the default visible state.  Mutates the in-memory message in place and appends
a durable `message-display' record so the change replays on reload.  Returns
the updated message, or nil when no such message exists."
  (when-let ((message (e-session--message-by-id store session-id message-id)))
    (let ((timestamp (e-session--timestamp)))
      (if display
          (plist-put message :display display)
        (cl-remf message :display))
      (e-session--touch store (e-session--get-live store session-id) timestamp)
      (e-session--append-record
       store session-id
       (list :type "message-display"
             :session-id session-id
             :timestamp timestamp
             :id message-id
             :display (and display (symbol-name display))))
      message)))

(cl-defun e-session-append-activity-event
    (store session-id turn-id event-type payload &key (write-index t))
  "Append a durable activity EVENT-TYPE to STORE for SESSION-ID and TURN-ID."
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (event (e-session--normalize-entry-from-record
                 session
                 'activity-event
                 (list :turn-id turn-id
                       :event-type event-type
                       :payload payload
                       :created-at timestamp)
                 timestamp)))
    (unless (plist-member event :board-activity-sequence)
      (let ((sequence
             (1+ (or (plist-get session :board-activity-sequence)
                     (cl-loop for entry in (plist-get session :activity-events)
                              maximize (or (plist-get entry :board-activity-sequence) 0))
                     0))))
        (setq event (plist-put event :board-activity-sequence sequence))
        (plist-put session :board-activity-sequence sequence)))
    (e-session--append-list-item session :activity-events event)
    (e-session--update-activity-derived-fields session event)
    (e-session--index-entry store session-id event)
    (e-session--touch store session timestamp)
    (e-session--append-record
     store session-id
     (list :type "activity-event"
           :session-id session-id
           :id (plist-get event :id)
           :parent-id (plist-get event :parent-id)
           :turn-id turn-id
           :board-activity-sequence (plist-get event :board-activity-sequence)
           :timestamp timestamp
           :event-type event-type
           :payload payload))
    (when write-index
      (e-session--write-index store))
    event))

(defun e-session-append-process-report (store session-id report)
  "Append out-of-band process REPORT to SESSION-ID in STORE.
Process reports are durable session entries but are not transcript messages and
therefore never enter backend context."
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (report (e-session--normalize-entry-from-record
                  session
                  'process-report
                  (copy-sequence report)
                  timestamp)))
    (e-session--append-list-item session :process-reports report)
    (e-session--index-entry store session-id report)
    (e-session--touch store session timestamp)
    (e-session--refresh-file-field store session)
    (e-session--append-record
     store session-id
     (list :type "process-report"
           :session-id session-id
           :id (plist-get report :id)
           :parent-id (plist-get report :parent-id)
           :timestamp timestamp
           :report report))
    (e-session--write-index store)
    report))

(cl-defun e-session-append-branch-summary
    (store session-id branch-id summary &key metadata)
  "Append BRANCH-ID SUMMARY metadata to SESSION-ID in STORE."
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (record (e-session--normalize-entry-from-record
                  session
                  'branch-summary
                  (list :branch-id branch-id
                        :summary summary
                        :metadata metadata
                        :created-at timestamp)
                  timestamp)))
    (e-session--append-list-item session :branch-summaries record)
    (e-session--index-entry store session-id record)
    (e-session--touch store session timestamp)
    (e-session--refresh-file-field store session)
    (e-session--append-record
     store session-id
     (list :type "branch-summary"
           :session-id session-id
           :id (plist-get record :id)
           :parent-id (plist-get record :parent-id)
           :timestamp timestamp
           :branch-id branch-id
           :summary summary
           :metadata metadata))
    (e-session--write-index store)
    record))

(cl-defun e-session-append-compaction
    (store session-id summary &key branch-id range first-kept-entry-id
           tokens-before tokens-kept metadata)
  "Append compaction SUMMARY for SESSION-ID in STORE.
BRANCH-ID, RANGE, FIRST-KEPT-ENTRY-ID, and METADATA describe the compacted
source when available."
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (record (e-session--normalize-entry-from-record
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
    (e-session--append-list-item session :compactions record)
    (e-session--index-entry store session-id record)
    (e-session--touch store session timestamp)
    (e-session--refresh-file-field store session)
    (e-session--append-record
     store session-id
     (list :type "compaction"
           :session-id session-id
           :id (plist-get record :id)
           :parent-id (plist-get record :parent-id)
           :timestamp timestamp
           :summary summary
           :branch-id branch-id
           :range range
           :first-kept-entry-id first-kept-entry-id
           :tokens-before tokens-before
           :tokens-kept tokens-kept
           :metadata metadata))
    (e-session--write-index store)
    record))

(cl-defun e-session-append-provider-anchor
    (store session-id provider-id &key model covered-entry-id fingerprints
           metadata)
  "Append opaque PROVIDER-ID anchor metadata to SESSION-ID in STORE.
COVERED-ENTRY-ID identifies the latest transcript entry covered by the
provider-owned anchor.  FINGERPRINTS and METADATA are opaque to session core."
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (record (e-session--normalize-entry-from-record
                  session
                  'provider-anchor
                  (list :provider-id provider-id
                        :model model
                        :covered-entry-id covered-entry-id
                        :fingerprints fingerprints
                        :metadata metadata
                        :created-at timestamp)
                  timestamp)))
    (e-session--append-list-item session :provider-anchors record)
    (e-session--index-entry store session-id record)
    (e-session--touch store session timestamp)
    (e-session--refresh-file-field store session)
    (e-session--append-record
     store session-id
     (list :type "provider-anchor"
           :session-id session-id
           :id (plist-get record :id)
           :parent-id (plist-get record :parent-id)
           :timestamp timestamp
           :provider-id provider-id
           :model model
           :covered-entry-id covered-entry-id
           :fingerprints
           (e-session--provider-anchor-fingerprints-for-json fingerprints)
           :metadata metadata))
    (e-session--write-index store)
    record))

(defun e-session--context-value-for-json (value)
  "Return context VALUE with semantic sequences encoded as JSON arrays.

`json-encode' treats a list beginning with a keyword as an object.  Context
records legitimately contain lists of keyword plists (messages, observations,
facts), so those sequence boundaries must become vectors before persistence;
otherwise a list containing one message is flattened into one object."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'e-session--context-value-for-json (append value nil))))
   ((and (listp value)
         (e-session--keyword-plist-p value))
    (let (result)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (setq result
                (append result
                        (list key
                              (e-session--context-value-for-json item))))))
      result))
   ((consp value)
    (vconcat (mapcar #'e-session--context-value-for-json value)))
   (t value)))

(defun e-session--context-record-for-json (record)
  "Return provider-neutral context RECORD safe for JSON persistence."
  (e-session--context-value-for-json record))

(defun e-session--normalize-context-record-for-replay (type record)
  "Normalize JSON-decoded lifecycle fields in context RECORD.
TYPE selects the lifecycle vocabulary to restore.

JSON decodes symbols such as frame states and settlement statuses as strings.
Keep the nested semantic payload opaque, but restore the small control
vocabulary needed by checkpoint derivation and the pure lifetime projector.
Unknown values remain conservative strings and therefore cannot be mistaken for
a terminal state."
  (if (not (e-session--keyword-plist-p record))
      record
    (let ((copy (copy-tree record)))
      ;; Do not manufacture required fields at the replay boundary.  The shared
      ;; validator below must be able to distinguish a valid persisted record
      ;; from one that merely became convenient to reduce after normalization.
      (when-let ((record-type (plist-get copy :type)))
        (when (and (stringp record-type)
                   (equal record-type (symbol-name type)))
          (plist-put copy :type type)))
    (when (eq type 'context-frame)
      (when-let ((state (plist-get copy :state)))
        (when (stringp state)
          (plist-put copy :state
                     (pcase state
                       ("open" 'open)
                       ("consuming" 'consuming)
                       ("consumed" 'consumed)
                       ("settled" 'settled)
                       ("aborted" 'aborted)
                       (_ state))))))
    (when (eq type 'context-frame-settlement)
      (when-let ((status (plist-get copy :status)))
        (when (stringp status)
          (plist-put copy :status
                       (pcase status
                       ("acknowledged" 'acknowledged)
                       ("settled" 'settled)
                       ("failed" 'failed)
                       ("aborted" 'aborted)
                       (_ status))))))
      copy)))

(defun e-session--context-id-p (id)
  "Return non-nil when ID is a scalar non-empty logical identity."
  (and id
       (or (and (stringp id) (not (string-empty-p id)))
           (and (symbolp id) (not (string-empty-p (symbol-name id))))
           (numberp id))))

(defun e-session--context-canonical-id (id type field)
  "Validate and canonicalize logical identity FIELD for TYPE."
  (unless (e-session--context-id-p id)
    (signal 'e-session-error
            (list "Context lifetime record has an invalid identity"
                  type field id)))
  (if (symbolp id) (symbol-name id) id))

(defun e-session--context-control-symbol (value vocabulary type field)
  "Return canonical control symbol VALUE or reject it for TYPE FIELD."
  (let ((symbol (cond
                 ((memq value vocabulary) value)
                 ((and (stringp value)
                       (member value (mapcar #'symbol-name vocabulary)))
                  (intern value)))))
    (unless symbol
      (signal 'e-session-error
              (list "Context lifetime record has an invalid lifecycle value"
                    type field value)))
    symbol))

(defun e-session--normalize-context-record (type record)
  "Validate and return canonical context RECORD normalized for TYPE.

This is the one strict lifetime-record boundary.  Append and replay both call
it after their respective wire decoding, so malformed plists cannot enter the
resume reducer through a replay-only path."
  (unless (and (listp record)
               (e-session--keyword-plist-p record))
    (signal 'e-session-error
            (list "Context lifetime records require a keyword plist" type record)))
  (unless (memq type e-session--context-lifetime-entry-types)
    (signal 'e-session-error (list "Unknown context lifetime entry" type)))
  (unless (and (plist-member record :record-version)
               (integerp (plist-get record :record-version))
               (= (plist-get record :record-version)
                  e-context-lifetime-record-version))
    (signal 'e-session-error
            (list "Context lifetime record has an invalid version"
                  type (plist-get record :record-version))))
  (unless (and (plist-member record :type)
               (eq (plist-get record :type) type))
    (signal 'e-session-error
            (list "Context lifetime record has an invalid type"
                  type (plist-get record :type))))
  (let* ((required
          (pcase type
            ('context-generation '(:id))
            ('context-frame '(:id :generation-id :state))
            ('context-promotion '(:id :frame-id))
            ('context-frame-settlement '(:id :frame-id :status))))
         (copy (copy-tree record)))
    (dolist (key required)
      (unless (and (plist-member copy key)
                   (e-session--context-id-p (plist-get copy key)))
        (signal 'e-session-error
                (list "Context lifetime record is missing or has an invalid field"
                      type key (plist-get copy key)))))
    (dolist (key '(:id :generation-id :frame-id))
      (when (plist-member copy key)
        (plist-put copy key
                   (e-session--context-canonical-id
                    (plist-get copy key) type key))))
    (when (eq type 'context-frame)
      (plist-put copy :state
                 (e-session--context-control-symbol
                  (plist-get copy :state)
                  e-session--context-frame-states type :state)))
    (when (eq type 'context-frame-settlement)
      (plist-put copy :status
                 (e-session--context-control-symbol
                  (plist-get copy :status)
                  e-session--context-settlement-statuses type :status)))
    ;; Semantic payloads are canonicalized at the same boundary as control
    ;; fields.  This makes append and replay produce byte/fingerprint-equivalent
    ;; values even when JSON has changed nested symbols into strings.
    (pcase type
      ('context-generation
       (plist-put copy :checkpoint
                  (e-context-lifetime-canonicalize
                   (plist-get copy :checkpoint)))
       (plist-put copy :durable-tail
                  (e-context-lifetime-canonicalize
                   (plist-get copy :durable-tail))))
      ('context-frame
       (dolist (key '(:observations :source-fingerprints :observation-ids
                      :consumption-attempt-ids :consuming-response-ids
                      :promotion-ids :terminal-settlement))
         (when (plist-member copy key)
           (plist-put copy key
                      (e-context-lifetime-canonicalize
                       (plist-get copy key))))))
      ('context-promotion
       (dolist (key '(:facts :source-observation-ids))
         (when (plist-member copy key)
           (plist-put copy key
                      (e-context-lifetime-canonicalize
                       (plist-get copy key)))))))
    copy))

(defun e-session--context-current-path (store session-id)
  "Return the current path using only the raw replayed SESSION-ID state.

Ownership validation runs while a persistent session is being reconstructed.
At that point the normal live-session accessor would see the partially replayed
session as unloaded and recursively restart the load.  Build the small path
lookup directly from the already-installed session object instead; this also
keeps the lifetime policy independent of checkpoint derivation."
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
                               (plist-get session :context-frames)
                               (plist-get session :context-promotions)
                               (plist-get session
                                          :context-frame-settlements))))
         (by-id (make-hash-table :test #'equal))
         path
         (head-id (and session (plist-get session :current-head-id))))
    (dolist (entry entries)
      (puthash (plist-get entry :id) entry by-id))
    (while head-id
      (when-let ((entry (gethash head-id by-id)))
        (push entry path)
        (setq head-id (plist-get entry :parent-id))))
    path))

(defun e-session--context-active-generation (store session-id)
  "Return the latest context generation entry on SESSION-ID's current path.

This ownership lookup deliberately uses the raw current path rather than the
checkpoint suffix.  The latter derives lifetime state as part of checkpoint
construction, so using it while replaying or appending a lifetime record would
make ownership validation recursively depend on the checkpoint reducer."
  (seq-find (lambda (entry)
              (eq (plist-get entry :type) 'context-generation))
            (reverse (e-session--context-current-path store session-id))))

(defun e-session--context-frame-entry (store session-id frame-id)
  "Return the latest frame entry for logical FRAME-ID on SESSION-ID's path.

As with `e-session--context-active-generation', this is an ownership query,
not a resume-state query, and therefore must not enter checkpoint derivation."
  (seq-find
   (lambda (entry)
     (and (eq (plist-get entry :type) 'context-frame)
          (equal (plist-get (e-session--context-record entry) :id) frame-id)))
   (reverse (e-session--context-current-path store session-id))))

(defun e-session--validate-context-entry-ownership
    (store session-id type context-record)
  "Reject CONTEXT-RECORD when its generation/frame owner is not active."
  (let* ((generation-entry
          (e-session--context-active-generation store session-id))
         (generation-record
          (and generation-entry
               (e-session--context-record generation-entry)))
         (generation-id (plist-get generation-record :id)))
    (cond
     ((eq type 'context-frame)
      (unless (and generation-id
                   (equal (plist-get context-record :generation-id)
                          generation-id))
        (signal 'e-session-error
                (list "Context frame has no active matching generation"
                      session-id
                      (plist-get context-record :generation-id)
                      generation-id))))
     ((memq type '(context-promotion context-frame-settlement))
      (let* ((frame-id (plist-get context-record :frame-id))
             (frame-entry (e-session--context-frame-entry
                           store session-id frame-id))
             (frame-record (and frame-entry
                                (e-session--context-record frame-entry))))
        (unless (and frame-record
                     generation-id
                     (equal (plist-get frame-record :generation-id)
                            generation-id))
          (signal 'e-session-error
                  (list "Context record has no active frame owner"
                        session-id type frame-id generation-id)))))
     (t context-record))))

(cl-defun e-session--append-context-entry
    (store session-id type field context-record &key (write-index t))
  "Append provider-neutral CONTEXT-RECORD under TYPE and FIELD.

The session entry receives its own append-only identity and parent link while
the logical context record remains detached under `:context-record'."
  (unless (memq type e-session--context-lifetime-entry-types)
    (signal 'e-session-error (list "Unknown context lifetime entry" type)))
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (context-record (e-session--normalize-context-record
                          type context-record))
         (_ownership
          (e-session--validate-context-entry-ownership
           store session-id type context-record))
         (entry
          (e-session--normalize-entry-from-record
           session type (list :context-record context-record) timestamp)))
    (e-session--append-list-item session field entry)
    (e-session--index-entry store session-id entry)
    (e-session--touch store session timestamp)
    (e-session--append-record
     store session-id
     (list :type (symbol-name type)
           :session-id session-id
           :id (plist-get entry :id)
           :parent-id (plist-get entry :parent-id)
           :timestamp timestamp
           :context-record (e-session--context-record-for-json context-record)))
    (when write-index
      (e-session--write-index store))
    entry))

(cl-defun e-session-append-context-generation
    (store session-id generation &key (write-index t))
  "Append semantic GENERATION and return its durable session entry."
  (e-session--append-context-entry
   store session-id 'context-generation :context-generations generation
   :write-index write-index))

(cl-defun e-session-append-context-frame
    (store session-id frame &key (write-index t))
  "Append observation FRAME and return its durable session entry."
  (e-session--append-context-entry
   store session-id 'context-frame :context-frames frame
   :write-index write-index))

(cl-defun e-session-append-context-promotion
    (store session-id promotion &key (write-index t))
  "Append selected durable PROMOTION and return its durable session entry."
  (e-session--append-context-entry
   store session-id 'context-promotion :context-promotions promotion
   :write-index write-index))

(cl-defun e-session-append-context-frame-settlement
    (store session-id settlement &key (write-index t))
  "Append a frame SETTLEMENT acknowledgement marker."
  (e-session--append-context-entry
   store session-id 'context-frame-settlement :context-frame-settlements
   settlement :write-index write-index))

(defun e-session-set-current-branch (store session-id branch-id)
  "Set SESSION-ID current branch cursor to BRANCH-ID in STORE."
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (event (e-session--append-session-event
                 session
                 'current-branch
                 timestamp
                 (list :branch-id branch-id))))
    (e-session--index-entry store session-id event)
    (plist-put session :current-branch branch-id)
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "current-branch"
           :session-id session-id
           :id (plist-get event :id)
           :parent-id (plist-get event :parent-id)
           :timestamp timestamp
           :branch-id branch-id))
    (e-session--write-index store)
    branch-id))

(defun e-session-clear-messages (store session-id)
  "Clear all messages for SESSION-ID in STORE."
  (let* ((session (e-session--get-live store session-id))
         (timestamp (e-session--timestamp))
         (root-id (e-session--root-event-id session))
         (event nil))
    (e-session--replace-list-field session :messages nil)
    (e-session--replace-list-field session :activity-events nil)
    (e-session--replace-list-field session :provider-anchors nil)
    (plist-put session :latest-token-usage-event nil)
    (e-session--clear-message-derived-fields store session)
    (e-session--clear-entry-index store session-id)
    (dolist (entry (plist-get session :session-events))
      (e-session--index-entry store session-id entry))
    (dolist (entry (plist-get session :branch-summaries))
      (e-session--index-entry store session-id entry))
    (dolist (entry (plist-get session :compactions))
      (e-session--index-entry store session-id entry))
    (dolist (entry (plist-get session :provider-anchors))
      (e-session--index-entry store session-id entry))
    (plist-put session :current-head-id root-id)
    (setq event
          (e-session--append-session-event
           session
           'messages-cleared
           timestamp
           (list :parent-id root-id)))
    (e-session--index-entry store session-id event)
    (e-session--touch store session timestamp)
    (e-session--append-record
     store session-id
     (list :type "messages-cleared"
           :session-id session-id
           :id (plist-get event :id)
           :parent-id (plist-get event :parent-id)
           :timestamp timestamp))
    (e-session--write-index store)
    event))

(defun e-session-rename (store session-id name)
  "Rename SESSION-ID in STORE to NAME."
  (when (string-empty-p (string-trim (or name "")))
    (user-error "Session name must not be empty"))
  (let* ((session (e-session--get-live store session-id))
         (name (string-trim name))
         (timestamp (e-session--timestamp))
         (event (e-session--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :name name))))
    (e-session--index-entry store session-id event)
    (plist-put session :name name)
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "session-info"
           :session-id session-id
           :id (plist-get event :id)
           :parent-id (plist-get event :parent-id)
           :timestamp timestamp
           :name name))
    (e-session--write-index store)
    session))

(defun e-session-display-title (store session-id)
  "Return display title for SESSION-ID in STORE."
  (e-session--display-title-for-session
   (e-session--peek-session store session-id)))

(defun e-session-root-p (session)
  "Return non-nil when SESSION is a user-facing root session.
Subagent and task-queue sessions remain directly addressable through their own
surfaces, but do not belong in general session pickers."
  (let ((metadata (plist-get session :metadata)))
    (not (or (plist-get metadata :parent-session-id)
             (plist-get metadata :subagent-role)
             (plist-get metadata :task-queue-task-id)))))

(defun e-session-list (store)
  "Return STORE sessions sorted by most recent message."
  (let (sessions)
    (maphash (lambda (_id session)
               (push (e-session--session-index-entry store session) sessions))
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

(defun e-session-list-roots (store)
  "Return user-facing root sessions in STORE, newest first."
  (cl-remove-if-not #'e-session-root-p (e-session-list store)))

(provide 'e-session)

;;; e-session.el ends here

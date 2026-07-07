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
(require 'seq)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-session-missing "Session does not exist")
(define-error 'e-session-duplicate "Session already exists")

(defgroup e-session nil
  "Session storage for e."
  :group 'e
  :prefix "e-session-")

(defcustom e-session-directory (locate-user-emacs-file "e/sessions/")
  "Default directory used for persisted e sessions."
  :type 'directory
  :group 'e-session)

(cl-defstruct (e-session-store (:constructor e-session-store-create))
  (sessions (make-hash-table :test 'equal))
  (entry-indexes (make-hash-table :test 'equal))
  directory
  sessions-directory
  index-file
  persistent
  write-mode
  write-queue
  write-queue-timer
  index-write-pending
  (write-queue-generation 0)
  (write-queue-sequence 0)
  (sequence 0))

(defcustom e-session-write-queue-delay 0.05
  "Seconds to wait before flushing queued persistent session writes."
  :type 'number
  :group 'e)

(defcustom e-session-load-chunk-bytes 65536
  "Number of bytes to read per cooperative persistent session load step."
  :type 'integer
  :group 'e)

(defconst e-session--replay-list-fields
  '(:session-events :messages :activity-events :branch-summaries
    :compactions :provider-anchors)
  "Session fields accumulated in reverse order while replaying JSONL.")

(defconst e-session--list-tail-fields
  '((:messages . :messages-tail)
    (:activity-events . :activity-events-tail)
    (:branch-summaries . :branch-summaries-tail)
    (:compactions . :compactions-tail)
    (:provider-anchors . :provider-anchors-tail))
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
    (:context-attachments
     :owner chat-session
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t
     :legacy t)
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

(defun e-session--queued-writes-p (store)
  "Return non-nil when STORE batches persistent writes through a timer."
  (eq (e-session-store-write-mode store) 'queued))

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

(defun e-session--index-json (store)
  "Return STORE's current index JSON line."
  (concat (json-encode (vconcat (e-session-list store))) "\n"))

(defun e-session--write-index-now (store)
  "Immediately write STORE's persistent session index."
  (when (e-session--persistent-p store)
    (e-session--ensure-directories store)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file (e-session-store-index-file store)
        (insert (e-session--index-json store))))))

(defconst e-session--critical-record-types
  '("session"
    "session-info"
    "message"
    "activity-event"
    "branch-summary"
    "compaction"
    "provider-anchor"
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
  (setf (e-session-store-write-queue store)
        (delq entry (e-session-store-write-queue store))))

(defun e-session--drop-stale-queued-write-entries (store)
  "Remove stale-generation queued writes from STORE."
  (setf (e-session-store-write-queue store)
        (cl-remove-if-not
         (lambda (entry)
           (e-session--queued-entry-current-p store entry))
         (e-session-store-write-queue store))))

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
  "Append queued ENTRIES, acknowledging each successful record write."
  (dolist (entry entries)
    (e-session--append-record-now
     store
     (e-session--queued-entry-session-id entry)
     (e-session--queued-entry-record entry))
    (e-session--drop-queued-write-entry store entry)))

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
    (setf (e-session-store-index-write-pending store) nil))
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
       (if (e-session--queued-writes-p store)
           (progn
             (e-session--schedule-write-queue store)
             (push (e-session--queued-write-entry store session-id record)
                   (e-session-store-write-queue store)))
         (e-session--append-record-now store session-id record))))))


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
    (unless (plist-member entry :type)
      (plist-put entry :type type))
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
  (let ((session (e-session-get store session-id)))
    (append (plist-get session :session-events)
            (plist-get session :messages)
            (plist-get session :activity-events)
            (plist-get session :branch-summaries)
            (plist-get session :compactions)
            (plist-get session :provider-anchors))))

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
  (let* ((session (e-session-get store session-id))
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
       (if (e-session--queued-writes-p store)
           (progn
             (e-session--schedule-write-queue store)
             (setf (e-session-store-index-write-pending store)
                   (e-session--queued-index-entry store)))
         (e-session--write-index-now store))))))

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

(defun e-session--normalize-message (message)
  "Return MESSAGE normalized after JSON replay."
  (plist-put message :role (e-session--known-role (plist-get message :role)))
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
  event)

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
       (let* ((metadata (e-session--normalize-metadata-for-replay
                         (plist-get record :metadata)
                         t))
              (session (list :id session-id
                            :metadata metadata
                            :session-events nil
                            :messages nil
                            :activity-events nil
                            :branch-summaries nil
                            :current-branch nil
                            :compactions nil
                            :provider-anchors nil
                            :created-at (or (plist-get record :created-at)
                                            timestamp)
                            :updated-at (or (plist-get record :updated-at)
                                            timestamp)
                            :turn-options
                            (e-session--normalize-turn-options
                             (plist-get record :turn-options))
                            :name nil)))
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
         (e-session--touch store session timestamp)))
      ("activity-event"
       (when session
        (let ((event
               (e-session--normalize-entry-from-record
                session
                'activity-event
                (e-session--normalize-activity-event
                 (list :id (plist-get record :id)
                       :parent-id (plist-get record :parent-id)
                       :turn-id (plist-get record :turn-id)
                       :event-type (plist-get record :event-type)
                       :payload (plist-get record :payload)
                       :created-at timestamp))
                timestamp
                record)))
          (e-session--prepend-replayed-item session :activity-events event)
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

(defun e-session-load (store)
  "Replay STORE's persistent sessions from disk."
  (when (e-session--persistent-p store)
    (clrhash (e-session-store-sessions store))
    (clrhash (e-session-store-entry-indexes store))
    (setf (e-session-store-sequence store) 0)
    (let ((sessions-directory (e-session-store-sessions-directory store)))
      (when (file-directory-p sessions-directory)
        (dolist (file (directory-files sessions-directory t "\\.jsonl\\'"))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                (unless (string-empty-p line)
                  (e-session--replay-record
                   store
                   (e-session--json-read-line line))))
              (forward-line 1)))))
      (maphash (lambda (_id session)
                 (e-session--finalize-replayed-session store session))
               (e-session-store-sessions store))))
  store)

(defun e-session--index-entry-session (store entry)
  "Return an unloaded session stub from index ENTRY in STORE."
  (let ((id (plist-get entry :id)))
    (when id
      (e-session--initialize-list-state
       (list :id id
             :metadata nil
             :session-events nil
             :messages nil
             :activity-events nil
             :branch-summaries nil
             :current-branch nil
             :compactions nil
             :provider-anchors nil
             :turn-options nil
             :created-at (plist-get entry :created-at)
             :updated-at (plist-get entry :updated-at)
             :updated-seq (or (plist-get entry :updated-seq) 0)
             :name (plist-get entry :name)
             :summary (plist-get entry :summary)
             :message-count (or (plist-get entry :message-count) 0)
             :last-message-at (plist-get entry :last-message-at)
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

(defun e-session--load-index (store)
  "Load STORE session metadata from its persistent index file."
  (when (and (e-session--persistent-p store)
             (file-readable-p (e-session-store-index-file store)))
    (let* ((value (condition-case nil
                      (e-session--json-read-file
                       (e-session-store-index-file store))
                    (file-error nil)
                    (json-parse-error nil)))
           (entries (e-session--index-entries value)))
      (when entries
        (clrhash (e-session-store-sessions store))
        (clrhash (e-session-store-entry-indexes store))
        (setf (e-session-store-sequence store) 0)
        (dolist (entry entries)
          (e-session--put-index-entry store entry))
        t))))

(defun e-session--load-index-from-session-files (store)
  "Populate STORE metadata from session records when no index exists."
  (let ((sessions-directory (e-session-store-sessions-directory store)))
    (when (file-directory-p sessions-directory)
      (dolist (file (directory-files sessions-directory t "\\.jsonl\\'"))
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
          (json-parse-error nil))))))

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
    (unless (e-session--load-index store)
      (e-session--load-index-from-session-files store))
    store))

(defun e-session-load-session (store session-id)
  "Load SESSION-ID transcript from persistent STORE."
  (unless (e-session--persistent-p store)
    (signal 'e-session-missing (list session-id)))
  (let ((file (e-session--session-file store session-id)))
    (unless (file-readable-p file)
      (signal 'e-session-missing (list session-id)))
    (remhash session-id (e-session-store-sessions store))
    (e-session--clear-entry-index store session-id)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (string-empty-p line)
            (e-session--replay-record store (e-session--json-read-line line))))
        (forward-line 1)))
    (if-let ((session (gethash session-id (e-session-store-sessions store))))
        (e-session--finalize-replayed-session store session)
      (signal 'e-session-missing (list session-id)))))

(cl-defun e-session-load-session-start
    (store session-id &key on-done on-error on-progress chunk-bytes)
  "Start cooperatively loading SESSION-ID transcript from persistent STORE.
Return an `e-request-lifecycle' request.  ON-DONE receives the loaded session,
ON-ERROR receives a condition list, and ON-PROGRESS receives byte progress."
  (unless (e-session--persistent-p store)
    (signal 'e-session-missing (list session-id)))
  (let* ((file (e-session--session-file store session-id))
         (chunk-bytes (max 1 (or chunk-bytes e-session-load-chunk-bytes))))
    (unless (file-readable-p file)
      (signal 'e-session-missing (list session-id)))
    (remhash session-id (e-session-store-sessions store))
    (e-session--clear-entry-index store session-id)
    (let* ((file-size (file-attribute-size (file-attributes file)))
           (position 0)
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
                        :activity-events nil
                        :branch-summaries nil
                        :current-branch nil
                        :compactions nil
                        :provider-anchors nil
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
  (let* ((source (e-session-get store session-id))
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

(defun e-session-get (store session-id)
  "Return SESSION-ID from STORE."
  (let ((session (e-session--peek-session store session-id)))
    (if (and (e-session--persistent-p store)
             (not (plist-get session :loaded)))
        (e-session-load-session store session-id)
      session)))

(defun e-session-messages (store session-id)
  "Return messages for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :messages)))

(defun e-session-activity-events (store session-id)
  "Return durable activity events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :activity-events)))

(defun e-session-latest-token-usage-event (store session-id)
  "Return the latest durable token usage event for SESSION-ID in STORE."
  (plist-get (e-session-get store session-id) :latest-token-usage-event))

(defun e-session-session-events (store session-id)
  "Return durable session events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :session-events)))

(defun e-session-compactions (store session-id)
  "Return compaction records for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :compactions)))

(defun e-session-provider-anchors (store session-id)
  "Return provider anchor records for SESSION-ID in STORE in insertion order."
  (copy-sequence
   (plist-get (e-session-get store session-id) :provider-anchors)))

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
  (copy-sequence (plist-get (e-session-get store session-id) :turn-options)))

(defun e-session--replace-metadata (store session-id metadata)
  "Replace SESSION-ID METADATA in STORE after validation."
  (let* ((metadata (e-session--validate-metadata metadata))
         (session (e-session-get store session-id))
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
  (let* ((session (e-session-get store session-id))
         (metadata (e-session--merge-metadata
                    (plist-get session :metadata)
                    config)))
    (e-session--replace-metadata store session-id metadata)))

(defun e-session-context-references (store session-id owner)
  "Return current-state references for OWNER in SESSION-ID."
  (let* ((metadata (plist-get (e-session-get store session-id) :metadata))
         (references (plist-get metadata :context-references))
         (owner-key (e-session--metadata-owner-key owner)))
    (copy-tree
     (e-session--metadata-public-value
      (plist-get references owner-key)))))

(defun e-session-set-context-references (store session-id owner references)
  "Set durable current-state REFERENCES for OWNER in SESSION-ID."
  (let* ((owner-key (e-session--metadata-owner-key owner))
         (session (e-session-get store session-id))
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
  (let* ((session (e-session-get store session-id))
         (metadata (e-session--merge-metadata
                    (plist-get session :metadata)
                    (list key reference))))
    (e-session--replace-metadata store session-id metadata)))

(defun e-session-capability-state (store session-id capability-id)
  "Return durable capability state for CAPABILITY-ID in SESSION-ID."
  (let* ((metadata (plist-get (e-session-get store session-id) :metadata))
         (state (plist-get metadata :capability-state))
         (owner-key (e-session--metadata-owner-key capability-id)))
    (copy-tree
     (e-session--metadata-public-value
      (plist-get state owner-key)))))

(cl-defun e-session-set-capability-state
    (store session-id capability-id state &key version)
  "Set durable capability STATE for CAPABILITY-ID in SESSION-ID."
  (let* ((owner-key (e-session--metadata-owner-key capability-id))
         (session (e-session-get store session-id))
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
  (let* ((session (e-session-get store session-id))
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
  (let* ((session (e-session-get store session-id))
         (timestamp (e-session--timestamp))
         (message (e-session--normalize-entry-from-record
                   session
                   'message
                   (e-session--message-with-created-at message timestamp)
                   timestamp)))
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

(cl-defun e-session-append-activity-event
    (store session-id turn-id event-type payload &key (write-index t))
  "Append a durable activity EVENT-TYPE to STORE for SESSION-ID and TURN-ID."
  (let* ((session (e-session-get store session-id))
         (timestamp (e-session--timestamp))
         (event (e-session--normalize-entry-from-record
                 session
                 'activity-event
                 (list :turn-id turn-id
                       :event-type event-type
                       :payload payload
                       :created-at timestamp)
                 timestamp)))
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
           :timestamp timestamp
           :event-type event-type
           :payload payload))
    (when write-index
      (e-session--write-index store))
    event))

(cl-defun e-session-append-branch-summary
    (store session-id branch-id summary &key metadata)
  "Append BRANCH-ID SUMMARY metadata to SESSION-ID in STORE."
  (let* ((session (e-session-get store session-id))
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
  (let* ((session (e-session-get store session-id))
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
  (let* ((session (e-session-get store session-id))
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

(defun e-session-set-current-branch (store session-id branch-id)
  "Set SESSION-ID current branch cursor to BRANCH-ID in STORE."
  (let* ((session (e-session-get store session-id))
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
  (let* ((session (e-session-get store session-id))
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
  (let* ((session (e-session-get store session-id))
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

(provide 'e-session)

;;; e-session.el ends here

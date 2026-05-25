;;; e-session.el --- Session store for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session storage for the pure core runtime.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

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
  directory
  sessions-directory
  index-file
  persistent
  (sequence 0))

(defconst e-session--replay-list-fields
  '(:messages :activity-events :branch-summaries :compactions)
  "Session fields accumulated in reverse order while replaying JSONL.")

(defconst e-session--ulid-alphabet "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
  "Crockford Base32 alphabet used for ULID strings.")

(defvar e-session--last-ulid-milliseconds nil
  "Last millisecond timestamp used by `e-session-generate-ulid'.")

(defvar e-session--last-ulid-random nil
  "Last 80-bit random suffix used by `e-session-generate-ulid'.")

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
                                  (system-name) (garbage-collect))))
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

(defun e-session--ensure-directories (store)
  "Ensure persistent directories for STORE exist."
  (when (e-session--persistent-p store)
    (make-directory (e-session-store-sessions-directory store) t)))

(defun e-session--session-file (store session-id)
  "Return JSONL file path for SESSION-ID in STORE."
  (expand-file-name (concat session-id ".jsonl")
                    (e-session-store-sessions-directory store)))

(defun e-session--append-record (store session-id record)
  "Append RECORD for SESSION-ID in persistent STORE."
  (when (e-session--persistent-p store)
    (e-session--ensure-directories store)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-buffer
        (insert (json-encode record) "\n")
        (append-to-file (point-min) (point-max)
                        (e-session--session-file store session-id))))))

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
    (when (e-session--persistent-p store)
      (plist-put session :file
                 (e-session--session-file store (plist-get session :id)))))
  session)

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

(defun e-session--entries (store session-id)
  "Return all durable entries for SESSION-ID in insertion order."
  (let ((session (e-session-get store session-id)))
    (append (plist-get session :messages)
            (plist-get session :activity-events)
            (plist-get session :branch-summaries)
            (plist-get session :compactions))))

(defun e-session-entry-by-id (store session-id entry-id)
  "Return durable entry ENTRY-ID from SESSION-ID."
  (seq-find (lambda (entry)
              (equal (plist-get entry :id) entry-id))
            (e-session--entries store session-id)))

(defun e-session--entry-children (store session-id parent-id)
  "Return entries whose parent is PARENT-ID in SESSION-ID."
  (seq-filter (lambda (entry)
                (equal (plist-get entry :parent-id) parent-id))
              (e-session--entries store session-id)))

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

(defun e-session--finalize-replayed-session (store session)
  "Restore replayed SESSION field ordering and derived metadata."
  (dolist (field e-session--replay-list-fields)
    (plist-put session field (nreverse (plist-get session field))))
  (cl-remf session :entry-count)
  (plist-put session :loaded t)
  (e-session--refresh-derived-fields store session))

(defun e-session--last-message-at (session)
  "Return SESSION's latest message timestamp, when it has messages."
  (when-let ((message (car (last (plist-get session :messages)))))
    (plist-get message :created-at)))

(defun e-session--session-index-entry (store session)
  "Return public index metadata for SESSION in STORE."
  (when (plist-get session :loaded)
    (e-session--refresh-derived-fields store session))
  (list :id (plist-get session :id)
        :name (plist-get session :name)
        :summary (plist-get session :summary)
        :title (e-session--display-title-for-session session)
        :message-count (or (plist-get session :message-count) 0)
        :created-at (plist-get session :created-at)
        :updated-at (plist-get session :updated-at)
        :updated-seq (plist-get session :updated-seq)
        :last-message-at (or (plist-get session :last-message-at)
                             (e-session--last-message-at session))
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
    normalized))

(defun e-session--write-index (store)
  "Write STORE's persistent session index."
  (when (e-session--persistent-p store)
    (e-session--ensure-directories store)
    (let ((coding-system-for-write 'utf-8)
          (index (e-session-list store)))
      (with-temp-file (e-session-store-index-file store)
        (insert (json-encode (vconcat index)) "\n")))))

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

(defun e-session--normalize-activity-event (event)
  "Return EVENT normalized after JSON replay."
  (plist-put event
             :event-type
             (e-session--known-event-type (plist-get event :event-type)))
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
       (let ((session (list :id session-id
                            :metadata (plist-get record :metadata)
                            :messages nil
                            :activity-events nil
                            :branch-summaries nil
                            :current-branch nil
                            :compactions nil
                            :created-at (or (plist-get record :created-at)
                                            timestamp)
                            :updated-at (or (plist-get record :updated-at)
                                            timestamp)
                            :turn-options
                            (e-session--normalize-turn-options
                             (plist-get record :turn-options))
                            :name nil)))
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
         (e-session--prepend-replayed-item
          session
          :activity-events
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
           record))
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
      ("current-branch"
       (when session
         (plist-put session :current-branch
                    (plist-get record :branch-id))
         (e-session--touch store session timestamp)))
      ("session-info"
       (when session
         (when (plist-member record :name)
           (plist-put session :name (plist-get record :name)))
         (when (plist-member record :metadata)
           (plist-put session :metadata (plist-get record :metadata)))
         (when (plist-member record :turn-options)
           (plist-put session
                      :turn-options
                      (e-session--normalize-turn-options
                       (plist-get record :turn-options))))
         (e-session--touch store session timestamp)))
      ("messages-cleared"
       (when session
         (plist-put session :messages nil)
         (plist-put session :activity-events nil)
         (plist-put session :current-head-id nil)
         (e-session--touch store session timestamp))))))

(defun e-session-load (store)
  "Replay STORE's persistent sessions from disk."
  (when (e-session--persistent-p store)
    (clrhash (e-session-store-sessions store))
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
      (list :id id
            :metadata nil
            :messages nil
            :activity-events nil
            :branch-summaries nil
            :current-branch nil
            :compactions nil
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
            :loaded nil))))

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

(defun e-session-persistent-index-store-create (&optional directory)
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
                 :persistent t)))
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

(defun e-session--peek-session (store session-id)
  "Return SESSION-ID metadata from STORE without forcing transcript replay."
  (or (gethash session-id (e-session-store-sessions store))
      (signal 'e-session-missing (list session-id))))

(defun e-session-persistent-store-create (&optional directory)
  "Create and load a persistent session store rooted at DIRECTORY."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory e-session-directory))))
         (sessions-directory (expand-file-name "sessions" directory))
         (store (e-session-store-create
                 :directory directory
                 :sessions-directory sessions-directory
                 :index-file (expand-file-name "index.json" directory)
                 :persistent t)))
    (e-session-load store)
    store))

(cl-defun e-session-create (store &key id metadata)
  "Create a session in STORE with ID and METADATA."
  (setq id (or id (e-session--generate-id)))
  (when (gethash id (e-session-store-sessions store))
    (signal 'e-session-duplicate (list id)))
  (let* ((timestamp (e-session--timestamp))
         (session (list :id id
                        :metadata metadata
                        :messages nil
                        :activity-events nil
                        :branch-summaries nil
                        :current-branch nil
                        :compactions nil
                        :turn-options nil
                        :created-at timestamp
                        :updated-at timestamp
                        :name (plist-get metadata :name)
                        :loaded t)))
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
    (puthash id session (e-session-store-sessions store))
    (e-session--append-record
     store id
     (list :type "session"
           :session-id id
           :timestamp timestamp
           :created-at timestamp
           :updated-at timestamp
           :metadata metadata))
    (e-session--write-index store)
    session))

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

(defun e-session-compactions (store session-id)
  "Return compaction records for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :compactions)))

(defun e-session-turn-options (store session-id)
  "Return session-scoped turn options for SESSION-ID in STORE."
  (copy-sequence (plist-get (e-session-get store session-id) :turn-options)))

(defun e-session-set-metadata (store session-id metadata)
  "Replace SESSION-ID METADATA in STORE."
  (let ((session (e-session-get store session-id)))
    (plist-put session :metadata metadata)
    (e-session--touch store session)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "session-info"
           :session-id session-id
           :timestamp (plist-get session :updated-at)
           :metadata metadata))
    (e-session--write-index store)
    metadata))

(defun e-session-set-turn-options (store session-id options)
  "Replace SESSION-ID turn OPTIONS in STORE."
  (let* ((session (e-session-get store session-id))
         (turn-options (e-session--normalize-turn-options options)))
    (plist-put session :turn-options turn-options)
    (e-session--touch store session)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "session-info"
           :session-id session-id
           :timestamp (plist-get session :updated-at)
           :turn-options turn-options))
    (e-session--write-index store)
    turn-options))

(defun e-session-append-message (store session-id message)
  "Append MESSAGE to SESSION-ID in STORE."
  (let* ((session (e-session-get store session-id))
         (messages (plist-get session :messages))
         (timestamp (e-session--timestamp))
         (message (e-session--normalize-entry-from-record
                   session
                   'message
                   (e-session--message-with-created-at message timestamp)
                   timestamp)))
    (plist-put session :messages (append messages (list message)))
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
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

(defun e-session-append-activity-event
    (store session-id turn-id event-type payload)
  "Append a durable activity EVENT-TYPE to STORE for SESSION-ID and TURN-ID."
  (let* ((session (e-session-get store session-id))
         (events (plist-get session :activity-events))
         (timestamp (e-session--timestamp))
         (event (e-session--normalize-entry-from-record
                 session
                 'activity-event
                 (list :turn-id turn-id
                       :event-type event-type
                       :payload payload
                       :created-at timestamp)
                 timestamp)))
    (plist-put session :activity-events (append events (list event)))
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
    (e-session--write-index store)
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
    (plist-put session
               :branch-summaries
               (append (plist-get session :branch-summaries)
                       (list record)))
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
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
    (plist-put session
               :compactions
               (append (plist-get session :compactions)
                       (list record)))
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
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

(defun e-session-set-current-branch (store session-id branch-id)
  "Set SESSION-ID current branch cursor to BRANCH-ID in STORE."
  (let* ((session (e-session-get store session-id))
         (timestamp (e-session--timestamp)))
    (plist-put session :current-branch branch-id)
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "current-branch"
           :session-id session-id
           :timestamp timestamp
           :branch-id branch-id))
    (e-session--write-index store)
    branch-id))

(defun e-session-clear-messages (store session-id)
  "Clear all messages for SESSION-ID in STORE."
  (let ((session (e-session-get store session-id)))
    (plist-put session :messages nil)
    (plist-put session :activity-events nil)
    (e-session--touch store session)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "messages-cleared"
           :session-id session-id
           :timestamp (plist-get session :updated-at)))
    (e-session--write-index store)
    session))

(defun e-session-rename (store session-id name)
  "Rename SESSION-ID in STORE to NAME."
  (when (string-empty-p (string-trim (or name "")))
    (user-error "Session name must not be empty"))
  (let ((session (e-session-get store session-id))
        (name (string-trim name)))
    (plist-put session :name name)
    (e-session--touch store session)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "session-info"
           :session-id session-id
           :timestamp (plist-get session :updated-at)
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

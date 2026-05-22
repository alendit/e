;;; e-session.el --- Session store for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session storage for the pure core runtime.

;;; Code:

(require 'cl-lib)
(require 'json)
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

(defun e-session--last-message-at (session)
  "Return SESSION's latest message timestamp, when it has messages."
  (when-let ((message (car (last (plist-get session :messages)))))
    (plist-get message :created-at)))

(defun e-session--session-index-entry (store session)
  "Return public index metadata for SESSION in STORE."
  (e-session--refresh-derived-fields store session)
  (list :id (plist-get session :id)
        :name (plist-get session :name)
        :summary (plist-get session :summary)
        :title (e-session-display-title store (plist-get session :id))
        :message-count (or (plist-get session :message-count) 0)
        :created-at (plist-get session :created-at)
        :updated-at (plist-get session :updated-at)
        :updated-seq (plist-get session :updated-seq)
        :last-message-at (e-session--last-message-at session)
        :file (plist-get session :file)))

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
        (insert (json-encode index) "\n")))))

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
         (plist-put session
                    :messages
                    (append (plist-get session :messages)
                            (list (e-session--message-with-created-at
                                   (plist-get record :message)
                                   timestamp))))
         (e-session--touch store session timestamp)))
      ("activity-event"
       (when session
         (plist-put session
                    :activity-events
                    (append (plist-get session :activity-events)
                            (list (e-session--normalize-activity-event
                                   (list :turn-id (plist-get record :turn-id)
                                         :event-type (plist-get record :event-type)
                                         :payload (plist-get record :payload)
                                         :created-at timestamp)))))
         (e-session--touch store session timestamp)))
      ("branch-summary"
       (when session
         (plist-put session
                    :branch-summaries
                    (append (plist-get session :branch-summaries)
                            (list (list :branch-id
                                        (plist-get record :branch-id)
                                        :summary
                                        (plist-get record :summary)
                                        :metadata
                                        (plist-get record :metadata)
                                        :created-at timestamp))))
         (e-session--touch store session timestamp)))
      ("compaction"
       (when session
         (plist-put session
                    :compactions
                    (append (plist-get session :compactions)
                            (list (list :summary
                                        (plist-get record :summary)
                                        :branch-id
                                        (plist-get record :branch-id)
                                        :range
                                        (plist-get record :range)
                                        :metadata
                                        (plist-get record :metadata)
                                        :created-at timestamp))))
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
         (e-session--touch store session timestamp))))
    (when session
      (e-session--refresh-derived-fields store session))))

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
              (forward-line 1)))))))
  store)

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
                        :name (plist-get metadata :name))))
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
  (or (gethash session-id (e-session-store-sessions store))
      (signal 'e-session-missing (list session-id))))

(defun e-session-messages (store session-id)
  "Return messages for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :messages)))

(defun e-session-activity-events (store session-id)
  "Return durable activity events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-get store session-id) :activity-events)))

(defun e-session-turn-options (store session-id)
  "Return session-scoped turn options for SESSION-ID in STORE."
  (copy-sequence (plist-get (e-session-get store session-id) :turn-options)))

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
         (message (e-session--message-with-created-at message timestamp)))
    (plist-put session :messages (append messages (list message)))
    (e-session--touch store session timestamp)
    (e-session--refresh-derived-fields store session)
    (e-session--append-record
     store session-id
     (list :type "message"
           :session-id session-id
           :timestamp timestamp
           :message message))
    (e-session--write-index store)
    message))

(defun e-session-append-activity-event
    (store session-id turn-id event-type payload)
  "Append a durable activity EVENT-TYPE to STORE for SESSION-ID and TURN-ID."
  (let* ((session (e-session-get store session-id))
         (events (plist-get session :activity-events))
         (timestamp (e-session--timestamp))
         (event (list :turn-id turn-id
                      :event-type event-type
                      :payload payload
                      :created-at timestamp)))
    (plist-put session :activity-events (append events (list event)))
    (e-session--touch store session timestamp)
    (e-session--append-record
     store session-id
     (list :type "activity-event"
           :session-id session-id
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
         (record (list :branch-id branch-id
                       :summary summary
                       :metadata metadata
                       :created-at timestamp)))
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
           :timestamp timestamp
           :branch-id branch-id
           :summary summary
           :metadata metadata))
    (e-session--write-index store)
    record))

(cl-defun e-session-append-compaction
    (store session-id summary &key branch-id range metadata)
  "Append compaction SUMMARY for SESSION-ID in STORE.
BRANCH-ID, RANGE, and METADATA describe the compacted source when available."
  (let* ((session (e-session-get store session-id))
         (timestamp (e-session--timestamp))
         (record (list :summary summary
                       :branch-id branch-id
                       :range range
                       :metadata metadata
                       :created-at timestamp)))
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
           :timestamp timestamp
           :summary summary
           :branch-id branch-id
           :range range
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
  (let ((session (e-session-get store session-id)))
    (or (plist-get session :name)
        (when-let ((summary (plist-get session :summary)))
          (e-session--default-title summary))
        (when-let ((created-at (plist-get session :created-at)))
          (format "Untitled %s" created-at))
        (format "Untitled %s" session-id))))

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

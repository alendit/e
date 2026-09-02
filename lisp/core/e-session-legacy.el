;;; e-session-legacy.el --- Offline legacy session decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure decoder for the retired JSONL session representation.  This module is
;; loaded only by the explicit offline migration application.  It has no write
;; operation, runtime fallback, controller, timer, or session aggregate state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'parse-time)
(require 'seq)
(require 'subr-x)
(require 'e-session-codec)

(define-error 'e-session-legacy-error "Invalid legacy session store")

(defun e-session-legacy--journal-directory (root)
  "Return the retired journal directory below legacy ROOT."
  (expand-file-name "sessions" (file-name-as-directory root)))

(defun e-session-legacy-session-ids (root)
  "Return sorted session identifiers found below copied legacy ROOT."
  (let ((directory (e-session-legacy--journal-directory root)))
    (unless (file-directory-p directory)
      (signal 'e-session-legacy-error
              (list "Legacy session journal root is missing" directory)))
    (sort (mapcar #'file-name-base
                  (directory-files directory t "\\.jsonl\\'"))
          #'string<)))

(defun e-session-legacy-read-records (root session-id)
  "Decode SESSION-ID's complete retired JSONL journal below copied ROOT.

The legacy writer always terminated committed records with a newline.  A
nonempty unterminated tail is therefore an interrupted append and migration
rejects it rather than silently importing a prefix."
  (let ((file (expand-file-name
               (concat session-id ".jsonl")
               (e-session-legacy--journal-directory root))))
    (unless (file-readable-p file)
      (signal 'e-session-legacy-error
              (list "Legacy session journal is missing" file)))
    (with-temp-buffer
      (let ((coding-system-for-read 'no-conversion))
        (insert-file-contents-literally file))
      (when (and (> (buffer-size) 0)
                 (/= (char-before (point-max)) ?\n))
        (signal 'e-session-legacy-error
                (list "Legacy session journal has an incomplete tail" file)))
      (goto-char (point-min))
      (let (records)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p line)
              (condition-case err
                  (push (e-session-codec-json-read-line
                         (decode-coding-string line 'utf-8))
                        records)
                (error
                 (signal 'e-session-legacy-error
                         (list "Malformed legacy session record" file
                               (error-message-string err)))))))
          (forward-line 1))
        (nreverse records)))))

(defun e-session-legacy-read-catalog (root)
  "Decode ROOT's optional retired =index.json= value.

The catalog is a rebuildable projection, but a malformed present file is still
reported so an operator receives a complete source-quality assessment."
  (let ((file (expand-file-name "index.json" (file-name-as-directory root))))
    (when (file-exists-p file)
      (unless (file-readable-p file)
        (signal 'e-session-legacy-error
                (list "Legacy session catalog is unreadable" file)))
      (condition-case err
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8))
              (insert-file-contents file))
            (json-parse-buffer :object-type 'plist :array-type 'list
                               :null-object e-session-codec-json-null
                               :false-object :json-false))
        (error
         (signal 'e-session-legacy-error
                 (list "Malformed legacy session catalog" file
                       (error-message-string err))))))))

(defun e-session-legacy--catalog-key-id (key)
  "Return the session identifier represented by catalog KEY."
  (cond
   ((keywordp key) (string-remove-prefix ":" (symbol-name key)))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)))

(defun e-session-legacy--catalog-entry (catalog session-id)
  "Return SESSION-ID's unique detached entry from legacy CATALOG."
  (let (matches)
    (cond
     ((and (proper-list-p catalog) (listp (car catalog)))
      (dolist (entry catalog)
        (when (and (listp entry)
                   (equal (plist-get entry :id) session-id))
          (push entry matches))))
     ((proper-list-p catalog)
      (let ((tail catalog))
        (while tail
          (let* ((key (pop tail))
                 (entry (and tail (pop tail)))
                 (id (e-session-legacy--catalog-key-id key)))
            (when (and (equal id session-id) (listp entry))
              (let ((entry (copy-tree entry)))
                (unless (plist-get entry :id)
                  (plist-put entry :id id))
                (push entry matches))))))))
    (unless (= (length matches) 1)
      (signal 'e-session-legacy-error
              (list "Rootless legacy journal requires one catalog entry"
                    session-id (length matches))))
    (copy-tree (car matches))))

(defun e-session-legacy--timestamp-seconds (value)
  "Return VALUE as floating-point epoch seconds, or nil when invalid."
  (condition-case nil
      (cond
       ((numberp value) (float value))
       ((and (stringp value)
             (integerp (nth 8 (parse-time-string value))))
        (float-time (date-to-time value))))
    (error nil)))

(defun e-session-legacy--format-timestamp (value)
  "Return validated timestamp VALUE in a session-root-compatible spelling."
  (let ((seconds (e-session-legacy--timestamp-seconds value)))
    (when seconds
      (if (stringp value)
          value
        (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                            (seconds-to-time seconds) t)))))

(defun e-session-legacy--catalog-timestamp
    (catalog-entry key fallback session-id)
  "Return CATALOG-ENTRY's validated KEY, or FALLBACK for SESSION-ID."
  (let ((value (plist-get catalog-entry key)))
    (if (null value)
        fallback
      (or (e-session-legacy--format-timestamp value)
          (signal 'e-session-legacy-error
                  (list "Invalid rootless session catalog timestamp"
                        session-id key value))))))

(defun e-session-legacy--earliest-record-timestamp (records session-id)
  "Return the earliest timestamp represented by RECORDS for SESSION-ID."
  (let (earliest)
    (dolist (record records)
      (let ((message (plist-get record :message)))
        (dolist (value (list (plist-get record :created-at)
                             (plist-get record :timestamp)
                             (and (listp message)
                                  (plist-get message :created-at))
                             (and (listp message)
                                  (plist-get message :timestamp))))
          (when-let* ((seconds
                       (e-session-legacy--timestamp-seconds value)))
            (when (or (null earliest) (< seconds earliest))
              (setq earliest seconds))))))
    (unless earliest
      (signal 'e-session-legacy-error
              (list "Rootless legacy journal has no usable timestamp"
                    session-id)))
    (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                        (seconds-to-time earliest) t)))

(defun e-session-legacy--inferred-root-id (records session-id)
  "Return RECORDS' unique missing parent identity for SESSION-ID."
  (let ((ids (make-hash-table :test 'equal)) missing)
    (dolist (record records)
      (when-let* ((id (plist-get record :id)))
        (puthash id t ids)))
    (dolist (record records)
      (when-let* ((parent-id (plist-get record :parent-id)))
        (unless (gethash parent-id ids)
          (push parent-id missing))))
    (setq missing (delete-dups missing))
    (unless (= (length missing) 1)
      (signal 'e-session-legacy-error
              (list "Rootless legacy journal has no unique root identity"
                    session-id (length missing))))
    (car missing)))

(defun e-session-legacy--synthesize-root (session-id records catalog-entry)
  "Build SESSION-ID's missing root from RECORDS and CATALOG-ENTRY."
  (let* ((fallback
          (e-session-legacy--earliest-record-timestamp records session-id))
         (created-at
          (e-session-legacy--catalog-timestamp
           catalog-entry :created-at fallback session-id))
         (updated-at
          (e-session-legacy--catalog-timestamp
           catalog-entry :updated-at created-at session-id)))
    (list :type "session" :session-id session-id
          :id (e-session-legacy--inferred-root-id records session-id)
          :timestamp created-at :created-at created-at :updated-at updated-at
          :metadata (copy-tree (plist-get catalog-entry :metadata))
          :name (plist-get catalog-entry :name))))

(defun e-session-legacy--checkpoint-root (checkpoint session-id)
  "Return CHECKPOINT's authoritative root for SESSION-ID, or nil."
  (when checkpoint
    (let* ((value (plist-get checkpoint :value))
           (root (car (plist-get value :records))))
      (unless (and (listp root)
                   (equal (plist-get root :type) "session")
                   (equal (plist-get root :session-id) session-id))
        (signal 'e-session-legacy-error
                (list "Rootless legacy checkpoint has no matching root"
                      session-id)))
      (copy-tree root))))

(defun e-session-legacy--normalize-rootless-sessions
    (sessions checkpoints catalog)
  "Prepend canonical roots to nonempty rootless SESSIONS.

CHECKPOINTS and CATALOG are accepted historical projections.  A matching
checkpoint root is authoritative.  Without one, the catalog supplies session
metadata while the journal supplies the root identity and earliest timestamp.
The original journal records remain in their exact order after the root."
  (dolist (entry sessions)
    (let ((session-id (car entry))
          (records (cdr entry)))
      (when (and records
                 (not (seq-some
                       (lambda (record)
                         (equal (plist-get record :type) "session"))
                       records)))
        (unless (seq-every-p
                 (lambda (record)
                   (equal (plist-get record :session-id) session-id))
                 records)
          (signal 'e-session-legacy-error
                  (list "Rootless legacy journal mixes session identities"
                        session-id)))
        (let* ((catalog-entry
                (e-session-legacy--catalog-entry catalog session-id))
               (checkpoint
                (seq-find
                 (lambda (candidate)
                   (equal (plist-get candidate :session-id) session-id))
                 checkpoints))
               (root
                (or (e-session-legacy--checkpoint-root checkpoint session-id)
                    (e-session-legacy--synthesize-root
                     session-id records catalog-entry))))
          (setcdr entry (cons root records))
          (when checkpoint
            (let* ((value (plist-get checkpoint :value))
                   (position (plist-get value :journal-byte-offset)))
              (unless (and (integerp position) (>= position 0))
                (signal 'e-session-legacy-error
                        (list "Invalid rootless checkpoint position"
                              session-id position)))
              (plist-put value :journal-byte-offset (1+ position))))))))
  (list :sessions sessions :checkpoints checkpoints))

(defun e-session-legacy--checkpoint-file-session-id (file)
  "Return the session id encoded by legacy checkpoint FILE."
  (string-remove-suffix ".checkpoint.json" (file-name-nondirectory file)))

(defun e-session-legacy--journal-position-at-offset (root session-id offset)
  "Translate SESSION-ID's legacy byte OFFSET into a record position."
  (unless (and (integerp offset) (>= offset 0))
    (signal 'e-session-legacy-error
            (list "Invalid legacy checkpoint offset" session-id offset)))
  (let ((journal
         (expand-file-name
          (concat session-id ".jsonl")
          (e-session-legacy--journal-directory root))))
    (unless (file-readable-p journal)
      (signal 'e-session-legacy-error
              (list "Checkpoint has no legacy session journal" session-id)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally journal)
      (when (> offset (buffer-size))
        (signal 'e-session-legacy-error
                (list "Legacy checkpoint offset exceeds journal"
                      session-id offset (buffer-size))))
      (goto-char (1+ offset))
      (unless (or (= offset 0) (= (char-before) ?\n))
        (signal 'e-session-legacy-error
                (list "Legacy checkpoint offset is not a record boundary"
                      session-id offset)))
      (count-lines (point-min) (point)))))

(defun e-session-legacy-read-checkpoints (root)
  "Return sorted active legacy resume checkpoints below ROOT.

Each result retains the parsed source value and supplies a SQLite-ready value
whose physical JSONL byte cursor is translated to the equivalent record
position.  All other semantic fields are preserved exactly."
  (let ((directory (e-session-legacy--journal-directory root)) checkpoints)
    (dolist (file (directory-files directory t "\\.checkpoint\\.json\\'"))
      (let ((session-id (e-session-legacy--checkpoint-file-session-id file)))
        (condition-case err
            (let* ((source-value
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8))
                        (insert-file-contents file))
                      (json-parse-buffer
                       :object-type 'plist :array-type 'list
                       :null-object nil :false-object :json-false)))
                   (offset (plist-get source-value :journal-byte-offset))
                   (position
                    (e-session-legacy--journal-position-at-offset
                     root session-id offset))
                   (value (copy-tree source-value)))
              (plist-put value :journal-byte-offset position)
              (push (list :session-id session-id :source-value source-value
                          :value value)
                    checkpoints))
          (e-session-legacy-error (signal (car err) (cdr err)))
          (error
           (signal 'e-session-legacy-error
                   (list "Malformed legacy session checkpoint" file
                         (error-message-string err)))))))
    (sort checkpoints
          (lambda (left right)
            (string< (plist-get left :session-id)
                     (plist-get right :session-id))))))

(defun e-session-legacy-decode (root)
  "Return validated retired session facts from copied legacy ROOT."
  (let* ((ids (e-session-legacy-session-ids root))
         (sessions
          (mapcar (lambda (id)
                    (cons id (e-session-legacy-read-records root id)))
                  ids))
         (checkpoints (e-session-legacy-read-checkpoints root))
         (catalog (e-session-legacy-read-catalog root))
         (normalized
          (e-session-legacy--normalize-rootless-sessions
           sessions checkpoints catalog)))
    (list :sessions (plist-get normalized :sessions)
          :checkpoints (plist-get normalized :checkpoints)
          :catalog catalog)))

(provide 'e-session-legacy)

;;; e-session-legacy.el ends here

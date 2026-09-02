;;; e-runtime-migration.el --- Explicit legacy-to-SQLite migration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Offline operator application service.  It inventories a copied legacy tree,
;; decodes each retired owner representation, imports through owner-shaped
;; ports into a private new store, compares a deterministic semantic manifest,
;; and only then renames the complete directory into place.  Ordinary runtime
;; startup never calls this module.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-board-storage)
(require 'e-cron-legacy)
(require 'e-cron-storage)
(require 'e-goodnite-demand-legacy)
(require 'e-goodnite-storage)
(require 'e-raw-results-legacy)
(require 'e-raw-results-storage)
(require 'e-runtime-sqlite)
(require 'e-runtime-store-codec)
(require 'e-session-catalog)
(require 'e-session-legacy)
(require 'e-session-storage)
(require 'e-task-queue-legacy)
(require 'e-task-storage)
(require 'e-voice-adjustment-legacy)
(require 'e-voice-storage)

(define-error 'e-runtime-migration-error "Runtime legacy migration failed")
(define-error 'e-runtime-migration-conflict "Legacy facts conflict"
  'e-runtime-migration-error)
(define-error 'e-runtime-migration-target-exists "Migration target exists"
  'e-runtime-migration-error)

(defconst e-runtime-migration-manifest-version 1)
(defconst e-runtime-migration-raw-expiry-seconds (* 365 24 60 60))
(defconst e-runtime-migration-cli-error-max-chars 2048)

(defun e-runtime-migration--canonical-hash (value)
  "Return canonical exact SHA-256 for VALUE."
  (secure-hash 'sha256 (e-runtime-store-codec-encode value)))

(defun e-runtime-migration--file-hash (file)
  "Return SHA-256 of FILE's exact bytes."
  (with-temp-buffer
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally file))
    (secure-hash 'sha256 (current-buffer))))

(defun e-runtime-migration--files (root)
  "Return every sorted regular file below ROOT."
  (sort
   (seq-filter #'file-regular-p
               (directory-files-recursively root "." nil))
   #'string<))

(defun e-runtime-migration--inventory-disposition (relative)
  "Return the explicit migration disposition for RELATIVE, or signal."
  (cond
   ((string-match-p
     "\\`sessions/sessions/[^/]+\\.jsonl\\'" relative)
    (list :owner 'session :disposition 'import))
   ((string-match-p
     "\\`sessions/sessions/[^/]+\\.checkpoint\\.json\\'" relative)
    (list :owner 'session-checkpoint :disposition 'import))
   ((string-match-p
     "\\`sessions/sessions/[^/]+\\.\\(?:jsonl\\|checkpoint\\.json\\)\\.bak\\.[0-9]\\{8\\}T[0-9]\\{6\\}Z\\'"
     relative)
    (list :owner 'session :disposition 'source-preserved-backup
          :reason "timestamped pre-cutover backup; inactive rollback input"))
   ((equal relative "sessions/index.json")
    (list :owner 'session-catalog :disposition 'validate-and-rebuild))
   ((string-match-p
     "\\`sessions/index\\.json\\.bak\\.[0-9]\\{8\\}T[0-9]\\{6\\}Z\\'"
     relative)
    (list :owner 'session-catalog :disposition 'source-preserved-backup
          :reason "timestamped pre-cutover backup; inactive rollback input"))
   ((equal relative "sessions/chat-overview-state.json")
    ;; This retired sidecar held presentation read markers.  Commit e28957c1
    ;; moved its useful high-watermark into session metadata; current overview
    ;; read state is deliberately process-local presentation state.
    (list :owner 'chat-overview :disposition 'retired-derived
          :reason "superseded presentation read markers; source preserved"))
   ((string-match-p
     "\\`task-queue/\\(?:.*/\\)?records\\.eld\\'" relative)
    (list :owner 'task :disposition 'import))
   ((string-match-p "\\`task-queue/.+\\.org\\'" relative)
    (list :owner 'task-product :disposition 'source-preserved-file-product
          :reason "historical task output product; not runtime queue authority"))
   ((equal relative "cron-state.eld")
    (list :owner 'cron :disposition 'import))
   ((equal relative "voice-tells.eld")
    (list :owner 'voice :disposition 'import))
   ((equal relative "goodnite/state/daydream_access.jsonl")
    (list :owner 'goodnite :disposition 'import))
   ((string-match-p "\\`raw-results/.+" relative)
    (list :owner 'raw-result :disposition 'import))
   ((string-match-p "\\`session-tmp/.+" relative)
    (list :owner 'session-resource :disposition 'import))
   (t
    (signal 'e-runtime-migration-error
            (list "Unmapped legacy state file" relative)))))

(defun e-runtime-migration-inventory (source)
  "Return deterministic physical inventory for copied legacy SOURCE."
  (setq source (file-name-as-directory (expand-file-name source)))
  (unless (file-directory-p source)
    (signal 'e-runtime-migration-error (list "Legacy source is missing" source)))
  (let ((session-root (expand-file-name "sessions" source)))
    (unless (file-directory-p session-root)
      (signal 'e-runtime-migration-error
              (list "Legacy sessions root is missing" session-root))))
  (mapcar
   (lambda (file)
     (let* ((relative (file-relative-name file source))
            (disposition
             (e-runtime-migration--inventory-disposition relative)))
       (append
        (list :path relative
              :bytes (file-attribute-size (file-attributes file))
              :sha256 (e-runtime-migration--file-hash file))
        disposition)))
   (e-runtime-migration--files source)))

(defun e-runtime-migration--session-input (source)
  "Decode exact legacy session state from SOURCE."
  (e-session-legacy-decode (expand-file-name "sessions" source)))

(defun e-runtime-migration--task-queue-id (task-root file)
  "Return the stable owner queue id for FILE below TASK-ROOT."
  (let ((relative (file-relative-name file task-root)))
    (if (equal relative "records.eld")
        "default"
      (directory-file-name (file-name-directory relative)))))

(defun e-runtime-migration--task-input (source)
  "Decode every retired task queue snapshot below SOURCE."
  (let ((task-root (expand-file-name "task-queue" source))
        (seen (make-hash-table :test 'equal))
        queues)
    (when (file-directory-p task-root)
      (dolist (file (e-runtime-migration--files task-root))
        (when (equal (file-name-nondirectory file) "records.eld")
          (let ((queue-id (e-runtime-migration--task-queue-id task-root file)))
            (when (gethash queue-id seen)
              (signal 'e-runtime-migration-conflict
                      (list "Duplicate legacy task queue identity" queue-id)))
            (puthash queue-id t seen)
            (push (list :queue-id queue-id
                        :path (file-relative-name file source)
                        :snapshot (e-task-queue-legacy-decode-file file))
                  queues)))))
    (sort queues
          (lambda (left right)
            (string< (plist-get left :queue-id)
                     (plist-get right :queue-id))))))

(defun e-runtime-migration--board-input (sessions)
  "Extract and canonically deduplicate Board facts from SESSIONS."
  (let ((roots (make-hash-table :test 'equal))
        (records (make-hash-table :test 'equal)))
    (dolist (session sessions)
      (dolist (record (cdr session))
        (pcase (plist-get record :type)
          ("board-session-state"
           (let* ((state (or (plist-get record :board-state) record))
                  (role (plist-get state :association-role))
                  (board-id (or (plist-get state :board-id)
                                (plist-get record :board-id)))
                  (principal (or (plist-get state :principal)
                                 (plist-get record :principal))))
             (when (and board-id principal
                        (not (equal role "participant")))
               (let ((prior (gethash board-id roots))
                     (root (list :board-id board-id :principal principal)))
                 (when (and prior (not (equal prior root)))
                   (signal 'e-runtime-migration-conflict
                           (list "Conflicting Board root" board-id prior root)))
                 (puthash board-id root roots)))))
          ("board-message"
           (let* ((message (copy-tree (plist-get record :message)))
                  (board-id (or (plist-get message :board-id)
                                (plist-get record :board-id)))
                  (id (and message (plist-get message :id)))
                  (key (and board-id id (cons board-id id)))
                  (prior (and key (gethash key records))))
             (when key
               (when (and prior (not (equal prior message)))
                 (signal 'e-runtime-migration-conflict
                         (list "Conflicting Board record" board-id id)))
               (puthash key message records)))))))
    (list
     :roots (sort (hash-table-values roots)
                  (lambda (a b) (string< (plist-get a :board-id)
                                         (plist-get b :board-id))))
     :records
     (sort (hash-table-values records)
           (lambda (a b)
             (string< (format "%s/%s" (plist-get a :board-id)
                                      (plist-get a :id))
                      (format "%s/%s" (plist-get b :board-id)
                                      (plist-get b :id))))))))

(defun e-runtime-migration--decode (source)
  "Decode and validate every accepted legacy owner below SOURCE."
  (let* ((session-state (e-runtime-migration--session-input source))
         (sessions (plist-get session-state :sessions))
         (tasks (e-runtime-migration--task-input source))
         (cron (e-cron-legacy-decode-file
                (expand-file-name "cron-state.eld" source)))
         (voice (e-voice-adjustment-legacy-decode-file
                 (expand-file-name "voice-tells.eld" source)))
         (goodnite (e-goodnite-demand-legacy-decode-file
                    (expand-file-name "goodnite/state/daydream_access.jsonl"
                                      source)))
         (raw-root (expand-file-name "raw-results" source))
         (tmp-root (expand-file-name "session-tmp" source)))
    (dolist (task tasks)
      (let ((snapshot (plist-get task :snapshot)))
        (when (and snapshot
                   (not (and (listp snapshot)
                             (listp (plist-get snapshot :records)))))
          (signal 'e-runtime-migration-error
                  (list "Malformed task snapshot" (plist-get task :path))))))
    (dolist (checkpoint (plist-get session-state :checkpoints))
      (let ((session-id (plist-get checkpoint :session-id))
            (value (plist-get checkpoint :value)))
        (unless (assoc session-id sessions)
          (signal 'e-runtime-migration-error
                  (list "Checkpoint has no session root" session-id)))
        (unless (e-session-catalog-checkpoint-valid-p value session-id)
          (signal 'e-runtime-migration-error
                  (list "Invalid legacy session checkpoint" session-id)))))
    (list :sessions sessions :boards (e-runtime-migration--board-input sessions)
          :session-checkpoints (plist-get session-state :checkpoints)
          :session-catalog (plist-get session-state :catalog)
          :tasks tasks :cron cron :voice voice :goodnite goodnite
          :raw-files (and (file-directory-p raw-root)
                          (e-runtime-migration--files raw-root))
          :tmp-files (and (file-directory-p tmp-root)
                          (e-runtime-migration--files tmp-root)))))

(defun e-runtime-migration--import-sessions (runtime decoded)
  "Import session records from DECODED through RUNTIME."
  (let ((store (e-runtime-sqlite-session-store runtime)) (count 0))
    (dolist (entry (plist-get decoded :sessions))
      (when (cdr entry)
        (e-session-storage-commit-mutation-batch store (car entry) (cdr entry))
        (setq count (+ count (length (cdr entry))))))
    count))

(defun e-runtime-migration--import-session-checkpoints (runtime decoded)
  "Import validated session checkpoints from DECODED through RUNTIME."
  (let ((store (e-runtime-sqlite-session-store runtime)) (count 0))
    (dolist (checkpoint (plist-get decoded :session-checkpoints))
      (e-session-storage-persist-resume-checkpoint
       store (plist-get checkpoint :session-id)
       (plist-get checkpoint :value))
      (setq count (1+ count)))
    count))

(defun e-runtime-migration--import-boards (runtime decoded)
  "Import canonical Board roots and message facts from DECODED."
  (let ((storage (e-runtime-sqlite-board-storage runtime))
        (state (make-hash-table :test 'equal))
        (count 0))
    (dolist (root (plist-get (plist-get decoded :boards) :roots))
      (let ((created (e-board-storage-create-board
                      storage (plist-get root :board-id)
                      (plist-get root :principal) root)))
        (puthash (plist-get root :board-id) created state)))
    (dolist (message (plist-get (plist-get decoded :boards) :records))
      (let* ((board-id (plist-get message :board-id))
             (root (gethash board-id state)))
        (when root
          (setq message (plist-put (copy-tree message) :record-kind
                                   (or (plist-get message :record-type)
                                       'message)))
          (setq root
                (e-board-storage-publish-record
                 storage board-id (plist-get root :generation) message nil))
          (puthash board-id root state)
          (setq count (1+ count)))))
    (list :roots (hash-table-count state) :records count)))

(defun e-runtime-migration--import-tasks (runtime queues)
  "Import legacy task QUEUES as separate durable owner projections."
  (let ((storage (e-runtime-sqlite-task-storage runtime)) results)
    (dolist (queue queues)
      (let ((queue-id (plist-get queue :queue-id))
            (snapshot (plist-get queue :snapshot)))
        (e-task-storage-open-queue storage queue-id)
        (push (append
               (list :path (plist-get queue :path))
               (if snapshot
                   (e-task-storage-import-legacy-snapshot
                    storage queue-id snapshot)
                 (list :queue-id queue-id :revision 0 :sequence 0 :records 0)))
              results)))
    (nreverse results)))

(defun e-runtime-migration--import-cron (runtime table)
  "Import legacy cron cadence TABLE."
  (let ((storage (e-runtime-sqlite-cron-storage runtime)) (count 0))
    (when table
      (maphash
       (lambda (id state)
         (let* ((anchor (or (plist-get state :anchor) 0.0))
                (last (plist-get state :last-fire)))
           (e-cron-storage-register
            storage id (list :legacy-definition id) anchor)
           (when last
             (let ((firing (format "migration:%s:%s" id last)))
               (e-cron-storage-claim storage id firing last last nil)
               (e-cron-storage-settle storage id firing 'claimed 'completed
                                      '(:migrated t))))
           (setq count (1+ count))))
       table))
    count))

(defun e-runtime-migration--import-voice (runtime tells)
  "Import legacy voice TELLS preserving counts and LRU order."
  (let ((storage (e-runtime-sqlite-voice-storage runtime)) (count 0)
        (cap (max 128 (length tells))))
    ;; Oldest first so the legacy first element remains most recent.
    (dolist (tell (reverse tells))
      (dotimes (_ (max 1 (or (plist-get tell :count) 1)))
        (e-voice-storage-record storage (plist-get tell :key)
                                (plist-get tell :label)
                                (plist-get tell :description)
                                (or (plist-get tell :last) "migration") cap))
      (setq count (1+ count)))
    count))

(defun e-runtime-migration--import-goodnite (runtime events)
  "Import ordered Goodnite EVENTS with deterministic observation identities."
  (let ((storage (e-runtime-sqlite-goodnite-storage runtime)) (position 0))
    (dolist (event events)
      (setq position (1+ position))
      (e-goodnite-storage-append
       storage (format "migration:%08d:%s" position
                       (e-runtime-migration--canonical-hash event)) event))
    position))

(defun e-runtime-migration--import-raw (runtime source files)
  "Import legacy raw FILES from SOURCE."
  (let ((storage (e-runtime-sqlite-raw-results-storage runtime)) (count 0))
    (dolist (file files)
      (let* ((relative (file-relative-name file
                                          (expand-file-name "raw-results" source)))
             (content (e-raw-results-legacy-decode-file file))
             (created (float-time (file-attribute-modification-time
                                   (file-attributes file)))))
        (e-raw-results-storage-put
         storage (concat "raw-result://" relative) content
         (list :legacy-path relative) created
         (+ created e-runtime-migration-raw-expiry-seconds))
        (setq count (1+ count))))
    count))

(defun e-runtime-migration--import-tmp (runtime source files)
  "Import legacy session tmp FILES from SOURCE using lineage/path names."
  (let ((store (e-runtime-sqlite-runtime-store runtime)) (count 0)
        (root (expand-file-name "session-tmp" source)))
    (dolist (file files)
      (let* ((relative (file-relative-name file root))
             (parts (split-string relative "/" t))
             (lineage (car parts))
             (path (string-join (cdr parts) "/")))
        (unless (and lineage (not (string-empty-p path)))
          (signal 'e-runtime-migration-error
                  (list "Malformed session tmp path" relative)))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix))
            (insert-file-contents file))
          (e-runtime-store-call
           store 'write
           (list :op 'resource-put :lineage-id lineage :session-id lineage
                 :path path :content (buffer-string)
                 :metadata '(:migrated t)
                 :expires-at (+ (float-time) (* 24 60 60)))))
        (setq count (1+ count))))
    count))

(defun e-runtime-migration--semantic-manifest (inventory decoded imported)
  "Return deterministic semantic manifest for INVENTORY, DECODED and IMPORTED."
  (let ((facts
         (list :sessions (plist-get decoded :sessions)
               :session-checkpoints
               (plist-get decoded :session-checkpoints)
               :session-catalog (plist-get decoded :session-catalog)
               :boards (plist-get decoded :boards)
               :tasks (plist-get decoded :tasks)
               :cron (plist-get decoded :cron)
               :voice (plist-get decoded :voice)
               :goodnite (plist-get decoded :goodnite)
               :raw (mapcar (lambda (file)
                              (cons (file-name-nondirectory file)
                                    (e-runtime-migration--file-hash file)))
                            (plist-get decoded :raw-files))
               :tmp (mapcar (lambda (file)
                              (cons (file-name-nondirectory file)
                                    (e-runtime-migration--file-hash file)))
                            (plist-get decoded :tmp-files))
               :retired
               (seq-filter
                (lambda (entry)
                  (eq (plist-get entry :disposition) 'retired-derived))
                inventory)
               :source-preserved
               (seq-filter
                (lambda (entry)
                  (memq (plist-get entry :disposition)
                        '(source-preserved-backup
                          source-preserved-file-product)))
                inventory))))
    (list :version e-runtime-migration-manifest-version
          :source-hash (e-runtime-migration--canonical-hash inventory)
          :semantic-hash (e-runtime-migration--canonical-hash facts)
          :imported imported)))

(defun e-runtime-migration--write-report (directory report)
  "Write restrictive migration REPORT below DIRECTORY."
  (let ((file (expand-file-name "migration-report.eld" directory))
        (coding-system-for-write 'utf-8-unix))
    (write-region (e-runtime-store-codec-encode report) nil file nil 'silent)
    (set-file-modes file #o600)
    file))

(cl-defun e-runtime-migration-run (source target &key dry-run)
  "Migrate copied legacy SOURCE into new SQLite TARGET.

When DRY-RUN is non-nil, perform the complete import and proof in a private
temporary directory, return its deterministic manifest, then remove the
working store.  Install mode atomically renames a fully closed, verified work
directory to TARGET.  SOURCE is never written."
  (setq source (file-name-as-directory (expand-file-name source))
        target (directory-file-name (expand-file-name target)))
  (when (file-exists-p target)
    (signal 'e-runtime-migration-target-exists (list target)))
  (let* ((inventory (e-runtime-migration-inventory source))
         (decoded (e-runtime-migration--decode source))
         (parent (file-name-directory target))
         (work (make-temp-file
                (expand-file-name
                 (format ".%s.migration-" (file-name-nondirectory target))
                 parent) t))
         (e-runtime-sqlite--live-composition nil)
         runtime report success)
    (set-file-modes work #o700)
    (unwind-protect
        (progn
          (setq runtime (e-runtime-sqlite-open work))
          (let ((imported
                 (list
                  :session-records
                  (e-runtime-migration--import-sessions runtime decoded)
                  :session-checkpoints
                  (e-runtime-migration--import-session-checkpoints
                   runtime decoded)
                  :boards (e-runtime-migration--import-boards runtime decoded)
                  :tasks (e-runtime-migration--import-tasks
                          runtime (plist-get decoded :tasks))
                  :cron (e-runtime-migration--import-cron
                         runtime (plist-get decoded :cron))
                  :voice (e-runtime-migration--import-voice
                          runtime (plist-get decoded :voice))
                  :goodnite (e-runtime-migration--import-goodnite
                             runtime (plist-get decoded :goodnite))
                  :raw (e-runtime-migration--import-raw
                        runtime source (plist-get decoded :raw-files))
                  :tmp (e-runtime-migration--import-tmp
                        runtime source (plist-get decoded :tmp-files)))))
            (setq report
                  (list :operation (if dry-run 'dry-run 'install)
                        :manifest
                        (e-runtime-migration--semantic-manifest
                         inventory decoded imported)
                        :inventory inventory
                        :integrity
                        (e-runtime-store-integrity
                         (e-runtime-sqlite-runtime-store runtime) t))))
          (e-runtime-sqlite-close runtime)
          (setq runtime nil)
          (e-runtime-migration--write-report work report)
          (if dry-run
              (setq success t)
            (rename-file work target nil)
            (setq success t
                  report (plist-put report :installed target)))
          report)
      (when runtime (ignore-errors (e-runtime-sqlite-close runtime)))
      (when (and (file-directory-p work) (or dry-run (not success)))
        (delete-directory work t)))))

(defun e-runtime-migration--bounded-error-message (error-data)
  "Return a one-line bounded operator message for ERROR-DATA."
  (let ((message
         (replace-regexp-in-string
          "[\n\r\t ]+" " " (error-message-string error-data))))
    (if (> (length message) e-runtime-migration-cli-error-max-chars)
        (concat (substring message 0 e-runtime-migration-cli-error-max-chars)
                "...")
      message)))

(defun e-runtime-migration-cli-main ()
  "Run the environment-configured migration with bounded operator errors."
  (condition-case err
      (progn
        (prin1
         (e-runtime-migration-run
          (getenv "E_RUNTIME_MIGRATION_SOURCE")
          (getenv "E_RUNTIME_MIGRATION_TARGET")
          :dry-run (equal (getenv "E_RUNTIME_MIGRATION_DRY_RUN") "t")))
        (terpri))
    (error
     (princ
      (format "e-runtime-migrate: %s\n"
              (e-runtime-migration--bounded-error-message err))
      #'external-debugging-output)
     (kill-emacs 1))))

(provide 'e-runtime-migration)

;;; e-runtime-migration.el ends here

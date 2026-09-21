;;; e-runtime-migration.el --- Explicit legacy-to-SQLite migration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Offline operator application service.  It inventories a copied legacy tree,
;; decodes each retired owner representation, imports through the current
;; owner APIs or explicit SQL operations into a private new store, compares a
;; deterministic semantic manifest,
;; and only then renames the complete directory into place.  Ordinary runtime
;; startup never calls this module.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-board-orchestration)
(require 'e-board-sqlite-contract)
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
(require 'e-session-query)
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
(define-error 'e-runtime-migration-cutover-error "Runtime cutover failed"
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

(defun e-runtime-migration--legacy-board-object-array (value)
  "Decode pre-wire Board VALUE containing an array of plists.
This compatibility parser belongs exclusively to the stopped-v5 migration
boundary; ordinary Board reads require the current tagged wire format."
  (let ((items (if (vectorp value) (append value nil) value)))
    (cond
     ((null items) nil)
     ((cl-every #'listp items) (copy-tree items))
     (t
      (cl-labels
          ((restore-tail
            (tail)
            (let ((tail (if (vectorp tail) (append tail nil) tail))
                  result)
              (while tail
                (let* ((raw-key (pop tail))
                       (key (if (stringp raw-key)
                                (intern (concat ":" raw-key))
                              raw-key))
                       (item (pop tail)))
                  (when (and (eq key :kind) (stringp item))
                    (setq item (intern item)))
                  (when (eq key :outputs)
                    (setq item (restore-array item)))
                  (setq result (append result (list key item)))))
              result))
           (restore-array
            (array)
            (let ((array (if (vectorp array) (append array nil) array))
                  result)
              (while array
                (let ((key (pop array))
                      (tail (pop array)))
                  (unless (keywordp key)
                    (signal 'e-runtime-migration-error
                            (list "Malformed legacy Board object array"
                                  value)))
                  (setq tail (if (vectorp tail) (append tail nil) tail))
                  (unless tail
                    (signal 'e-runtime-migration-error
                            (list "Malformed legacy Board object array"
                                  value)))
                  (let ((first (pop tail)))
                    (when (and (eq key :kind) (stringp first))
                      (setq first (intern first)))
                    (push (cons key (cons first (restore-tail tail))) result))))
              (nreverse result))))
        (restore-array items))))))

(defun e-runtime-migration--legacy-board-enum (value)
  "Return pre-wire Board enum VALUE as a symbol."
  (if (stringp value) (intern value) value))

(defun e-runtime-migration--legacy-board-orchestration-payload (type payload)
  "Decode pre-wire orchestration PAYLOAD of TYPE for offline migration."
  (let ((payload (copy-tree payload)))
    (pcase type
      ('manifest
       (plist-put payload :tasks
                  (e-runtime-migration--legacy-board-object-array
                   (plist-get payload :tasks)))
       (when-let ((deadline (plist-get payload :deadline)))
         (plist-put deadline :kind
                    (e-runtime-migration--legacy-board-enum
                     (plist-get deadline :kind)))))
      ('continuation-claim
       (plist-put payload :status
                  (e-runtime-migration--legacy-board-enum
                   (plist-get payload :status))))
      ((or 'task-attempt 'terminal-report)
       (plist-put payload :status
                  (e-runtime-migration--legacy-board-enum
                   (plist-get payload :status)))
       (when (eq type 'terminal-report)
         (plist-put payload :outputs
                    (e-runtime-migration--legacy-board-object-array
                     (plist-get payload :outputs))))))
    payload))

(defun e-runtime-migration--normalize-board-orchestration-message (message)
  "Upgrade pre-wire orchestration MESSAGE to the current Board wire format."
  (let* ((attributes (plist-get message :attributes))
         (type-value (plist-get attributes :orchestration-type)))
    (if (and (memq 'orchestration (plist-get message :tags))
             type-value
             (null (plist-get attributes :orchestration-wire-version)))
        (let* ((type (e-runtime-migration--legacy-board-enum type-value))
               (fact
                (list
                 :version (plist-get attributes :orchestration-version)
                 :type type
                 :payload
                 (e-runtime-migration--legacy-board-orchestration-payload
                  type (plist-get attributes :orchestration-payload))
                 :idempotency-key
                 (plist-get attributes :orchestration-idempotency-key)))
               (fields (e-board-orchestration-fact-record-fields fact)))
          (plist-put (copy-tree message) :attributes
                     (plist-get fields :attributes)))
      message)))

(defconst e-runtime-migration--legacy-board-session-record-types
  '("board-message" "board-messages-cleared" "board-session-state")
  "Retired Board record families accepted only at this offline boundary.")

(defun e-runtime-migration--legacy-board-attribute-value (value)
  "Decode one legacy routing attribute VALUE."
  (let ((items (and (or (vectorp value) (proper-list-p value))
                    (append value nil))))
    (if (not (and (= (length items) 2) (stringp (car items))))
        (copy-tree value t)
      (pcase (car items)
        ("symbol" (intern (cadr items)))
        ("vector"
         (vconcat (mapcar #'e-runtime-migration--legacy-board-attribute-value
                          (append (cadr items) nil))))
        ("list"
         (mapcar #'e-runtime-migration--legacy-board-attribute-value
                 (append (cadr items) nil)))
        ("cons"
         (let ((pair (append (cadr items) nil)))
           (unless (= (length pair) 2)
             (signal 'e-runtime-migration-error
                     (list "Malformed legacy Board selector cons" value)))
           (cons (e-runtime-migration--legacy-board-attribute-value
                  (car pair))
                 (e-runtime-migration--legacy-board-attribute-value
                  (cadr pair)))))
        ("plist"
         (let (result)
           (dolist (pair (append (cadr items) nil))
             (let* ((pair (append pair nil))
                    (key (car pair)))
               (unless (and (= (length pair) 2) (stringp key))
                 (signal 'e-runtime-migration-error
                         (list "Malformed legacy Board selector plist" value)))
               (setq result
                     (plist-put
                      result
                      (intern (concat ":" (string-remove-prefix ":" key)))
                      (e-runtime-migration--legacy-board-attribute-value
                       (cadr pair))))))
           result))
        (_ (copy-tree value t))))))

(defun e-runtime-migration--legacy-board-selector (selector)
  "Decode legacy typed attributes in Board routing SELECTOR."
  (let ((copy (copy-tree selector t)))
    (when-let* ((encoded (plist-get copy :attributes))
                (items (and (or (vectorp encoded) (proper-list-p encoded))
                            (append encoded nil))))
      (when (and (= (length items) 2)
                 (equal (car items) "e-routing-attributes-v1"))
        (plist-put copy :attributes
                   (e-runtime-migration--legacy-board-attribute-value
                    (cadr items)))))
    copy))

(defun e-runtime-migration--legacy-board-conflict
    (reason session-id &optional record-id)
  "Signal a bounded legacy Board conflict for SESSION-ID.
REASON is a fixed operator-facing string.  RECORD-ID, when present, is the
small stable identity of the affected record; the legacy payload is never
included in the condition data."
  (signal 'e-runtime-migration-conflict
          (append (list reason :session-id session-id)
                  (when record-id (list :record-id record-id)))))

(defun e-runtime-migration--legacy-board-plist-p (value)
  "Return non-nil when VALUE is a proper keyword plist."
  (and (proper-list-p value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr always (keywordp key))))

(defun e-runtime-migration--legacy-board-record-session-valid-p
    (record session-id)
  "Return non-nil when legacy Board RECORD belongs to SESSION-ID."
  (and (e-runtime-migration--legacy-board-plist-p record)
       (equal (plist-get record :session-id) session-id)))

(defun e-runtime-migration--legacy-board-default-routing-policy
    (session-id board-id)
  "Return a complete deterministic routing policy for legacy SESSION-ID.
BOARD-ID is included in the derived participant identity so distinct Board
associations cannot collide merely because legacy session names were reused."
  (let ((participant-id
         (concat
          "ptc_"
          (substring
           (secure-hash
            'sha256
            (format "legacy-board-participant:%s:%s" board-id session-id))
           0 32))))
    (list :participant-id participant-id
          :pickup-selector '(:tags (main))
          :observer-selector '(:tags (main))
          :default-tags '(main)
          :default-to nil)))

(defun e-runtime-migration--legacy-board-canonical-root-p
    (session-id principal role)
  "Return non-nil when legacy association identity proves a canonical root.
Historical owners were explicit.  Before the role field existed, only the
canonical chat principal tied the association unambiguously to its session."
  (or (equal role "owner")
      (and (null role)
           (equal principal (concat "chat:" session-id)))))

(defun e-runtime-migration--legacy-board-state (record session-id)
  "Return RECORD's validated detached Board association for SESSION-ID."
  (unless (e-runtime-migration--legacy-board-record-session-valid-p
           record session-id)
    (e-runtime-migration--legacy-board-conflict
     "Malformed legacy Board association record" session-id))
  (let* ((raw-state (or (plist-get record :board-state) record))
         (state (and (e-runtime-migration--legacy-board-plist-p raw-state)
                     (copy-tree raw-state t)))
         (board-id (and state (plist-get state :board-id)))
         (principal (and state (plist-get state :principal)))
         (role (and state (plist-get state :association-role)))
         (policy (and state (plist-get state :routing-policy))))
    (unless (and (stringp board-id) (not (string-empty-p board-id))
                 (stringp principal) (not (string-empty-p principal)))
      (e-runtime-migration--legacy-board-conflict
       "Malformed legacy Board association identity" session-id))
    (when (and role (symbolp role))
      (setq role (symbol-name role)))
    (unless (member role '(nil "owner" "participant"))
      (e-runtime-migration--legacy-board-conflict
       "Malformed legacy Board association role" session-id))
    (unless policy
      (unless (e-runtime-migration--legacy-board-canonical-root-p
               session-id principal role)
        (e-runtime-migration--legacy-board-conflict
         "Legacy Board association lacks routing policy" session-id))
      (setq policy
            (e-runtime-migration--legacy-board-default-routing-policy
             session-id board-id)))
    ;; Before association roles were persisted, a roleless association was the
    ;; session's Board root.  Board import already maps that historical shape
    ;; to an owner participant; keep the durable session query row identical so
    ;; root-session navigation cannot silently omit the migrated session.
    (unless role
      (setq role "owner"))
    (condition-case err
        (progn
          (dolist (key '(:pickup-selector :observer-selector))
            (when (plist-member policy key)
              (plist-put policy key
                         (e-runtime-migration--legacy-board-selector
                          (plist-get policy key)))))
          (unless (e-session-board-routing-policy-valid-p policy)
            (e-runtime-migration--legacy-board-conflict
             "Malformed legacy Board routing policy" session-id)))
      (e-runtime-migration-conflict
       (signal (car err) (cdr err)))
      (error
       (e-runtime-migration--legacy-board-conflict
        "Malformed legacy Board routing policy" session-id)))
    (plist-put state :routing-policy
               (e-session-board-routing-policy-normalize policy))
    (plist-put state :association-role role)
    state))

(defun e-runtime-migration--legacy-board-association
    (records &optional session-id)
  "Return the latest decoded Board association in legacy RECORDS.
SESSION-ID names the owning source journal in bounded conflict diagnostics."
  (let (association)
    (dolist (record records)
      (when (equal (plist-get record :type) "board-session-state")
        (let* ((owner-id (or session-id (plist-get record :session-id)))
               (state
                (e-runtime-migration--legacy-board-state record owner-id)))
          (when (and association
                     (or (not (equal (plist-get association :board-id)
                                     (plist-get state :board-id)))
                         (not (equal (plist-get association :principal)
                                     (plist-get state :principal)))))
            (e-runtime-migration--legacy-board-conflict
             "Conflicting legacy Board association" owner-id))
          (setq association state))))
    association))

(defun e-runtime-migration--normalize-board-message (message)
  "Return legacy Board MESSAGE in current semantic value spelling."
  (let ((message (copy-tree message t)))
    (dolist (field '(:kind :mode :activity-kind :routing-state
                     :unrouted-reason :record-type :outcome :failure-policy))
      (when-let* ((value (plist-get message field)))
        (when (stringp value) (plist-put message field (intern value)))))
    (when (plist-member message :tags)
      (plist-put message :tags
                 (mapcar (lambda (tag) (if (stringp tag) (intern tag) tag))
                         (plist-get message :tags))))
    (when-let* ((attributes (plist-get message :attributes))
                (status (plist-get attributes :status)))
      (when (stringp status) (plist-put attributes :status (intern status))))
    message))

(defun e-runtime-migration--legacy-board-session-records
    (session-id records association)
  "Return SESSION-ID's surviving legacy Board RECORDS in append order.
ASSOCIATION supplies the durable Board identity.  A clear record discards all
earlier Board messages in this source session.  Duplicate message identities
retain the first equal envelope after the last clear."
  (let ((board-id (and association (plist-get association :board-id)))
        (seen (make-hash-table :test 'equal))
        survivors
        (position 0))
    (dolist (record records)
      (setq position (1+ position))
      (pcase (plist-get record :type)
        ((or "board-message" "board-messages-cleared" "board-session-state")
         (unless (e-runtime-migration--legacy-board-record-session-valid-p
                  record session-id)
           (e-runtime-migration--legacy-board-conflict
            "Malformed legacy Board journal record" session-id)))
        (_ nil))
      (pcase (plist-get record :type)
        ("board-messages-cleared"
         (let ((clear-id (plist-get record :id)))
           (unless (and (stringp clear-id) (not (string-empty-p clear-id)))
             (e-runtime-migration--legacy-board-conflict
              "Malformed legacy Board clear record" session-id)))
         (clrhash seen)
         (setq survivors nil))
        ("board-message"
         (unless association
           (e-runtime-migration--legacy-board-conflict
            "Orphan legacy Board message" session-id))
         (let* ((raw-message (plist-get record :message))
                (raw-id (and (e-runtime-migration--legacy-board-plist-p
                              raw-message)
                             (plist-get raw-message :id))))
           (unless (and (stringp raw-id) (not (string-empty-p raw-id)))
             (e-runtime-migration--legacy-board-conflict
              "Malformed legacy Board message" session-id))
           (let* ((message
                   (condition-case nil
                       (e-runtime-migration--normalize-board-orchestration-message
                        (e-runtime-migration--normalize-board-message raw-message))
                     (error
                      (e-runtime-migration--legacy-board-conflict
                       "Malformed legacy Board message" session-id raw-id))))
                  (kind (plist-get message :kind))
                  (embedded-board-id (plist-get message :board-id))
                  (prior (gethash raw-id seen)))
             (unless (memq kind '(input output activity fact))
               (e-runtime-migration--legacy-board-conflict
                "Malformed legacy Board message kind" session-id raw-id))
             (when (and embedded-board-id
                        (not (equal embedded-board-id board-id)))
               (e-runtime-migration--legacy-board-conflict
                "Legacy Board message association conflicts" session-id raw-id))
             (setq message (plist-put message :board-id board-id))
             (setq message (plist-put message :record-kind kind))
             (if prior
                 (unless (equal prior message)
                   (e-runtime-migration--legacy-board-conflict
                    "Conflicting legacy Board message" session-id raw-id))
               (puthash raw-id message seen)
               (push (list :board-id board-id
                           :source-session-id session-id
                           :source-position position
                           :message message)
                     survivors)))))
        (_ nil)))
    (nreverse survivors)))

(defun e-runtime-migration--board-input (sessions)
  "Extract legacy Board roots and surviving records from SESSIONS.
Each source session is reconstructed in physical append order.  Cross-session
deduplication retains the first surviving equal message for a Board without
sorting Board or message identities."
  (let ((roots-by-id (make-hash-table :test 'equal))
        (records-by-id (make-hash-table :test 'equal))
        (participant-owners (make-hash-table :test 'equal))
        roots records associations)
    (dolist (session sessions)
      (let* ((session-id (car session))
             (source-records (cdr session))
             (association
              (e-runtime-migration--legacy-board-association
               source-records session-id))
             (has-board-record
              (seq-some
               (lambda (record)
                 (member (plist-get record :type)
                         e-runtime-migration--legacy-board-session-record-types))
               source-records)))
        (when (and has-board-record (null association))
          (e-runtime-migration--legacy-board-conflict
           "Legacy Board journal has no association" session-id))
        (when association
          (let* ((board-id (plist-get association :board-id))
                 (principal (plist-get association :principal))
                 (role (plist-get association :association-role))
                 (participant-id
                  (plist-get (plist-get association :routing-policy)
                             :participant-id))
                 (participant-key (cons board-id participant-id))
                 (prior-participant-owner
                  (gethash participant-key participant-owners))
                 (prior-root (gethash board-id roots-by-id)))
            (when prior-participant-owner
              (e-runtime-migration--legacy-board-conflict
               "Duplicate legacy Board participant identity" session-id
               participant-id))
            (puthash participant-key session-id participant-owners)
            (push (list :session-id session-id :board-id board-id
                        :association (copy-tree association t))
                  associations)
            (unless (equal role "participant")
              (if prior-root
                  (unless (equal (plist-get prior-root :principal) principal)
                    (e-runtime-migration--legacy-board-conflict
                     "Conflicting legacy Board root" session-id))
                (let ((root (list :board-id board-id :principal principal
                                  :source-session-id session-id)))
                  (puthash board-id root roots-by-id)
                  (push root roots))))
            (dolist (entry
                     (e-runtime-migration--legacy-board-session-records
                      session-id source-records association))
              (let* ((message (plist-get entry :message))
                     (record-id (plist-get message :id))
                     (key (cons board-id record-id))
                     (prior (gethash key records-by-id)))
                (if prior
                    (unless (equal (plist-get prior :message) message)
                      (e-runtime-migration--legacy-board-conflict
                       "Conflicting legacy Board message" session-id record-id))
                  (puthash key entry records-by-id)
                  (push entry records))))))))
    (dolist (association associations)
      (let* ((board-id (plist-get association :board-id))
             (session-id (plist-get association :session-id))
             (state (plist-get association :association))
             (root (gethash board-id roots-by-id)))
        (unless root
          (e-runtime-migration--legacy-board-conflict
           "Legacy Board association has no Board root" session-id))
        (unless (equal (plist-get state :principal)
                       (plist-get root :principal))
          (e-runtime-migration--legacy-board-conflict
           "Legacy Board association principal conflicts with Board root"
           session-id))))
    (list :roots (nreverse roots)
          :associations (nreverse associations)
          :records (nreverse records))))

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
  "Import canonical session records and derived rows from DECODED.

The translated journal is the sole source of current session facts.  Retired
catalog and checkpoint values remain validation witnesses in the decode phase;
they are never published into the v6 store."
  (let ((store (e-runtime-sqlite-session-store runtime)) (count 0))
    (dolist (entry (plist-get decoded :sessions))
      (when (cdr entry)
        (let* ((session-id (car entry))
               (source-records (cdr entry))
               (association
                (e-runtime-migration--legacy-board-association
                 source-records session-id))
               (records
                (cl-remove-if
                 (lambda (record)
                   (member (plist-get record :type)
                           e-runtime-migration--legacy-board-session-record-types))
                 source-records))
               ;; The v6 row uses the durable append position as its stable
               ;; cursor.  Legacy JSONL has no physical position field, so
               ;; supply it only to the detached derivation input and leave
               ;; the translated record values otherwise intact.
               (position 0)
               (replay-records
                (mapcar
                 (lambda (record)
                   (setq position (1+ position))
                   (let ((copy (copy-tree record)))
                     (unless (plist-member copy :timestamp)
                       (when-let* ((created-at (plist-get copy :created-at)))
                         (setq copy (plist-put copy :timestamp created-at))))
                     (plist-put copy :journal-position position)))
                 records))
               (query-delta (e-session-query-derive replay-records)))
          (when association
            (dolist (key '(:board-id :principal :association-role
                           :routing-policy))
              (plist-put query-delta key (copy-tree (plist-get association key) t)))
            (e-session-query-state-validate query-delta))
          (unless query-delta
            (signal 'e-runtime-migration-error
                    (list "Session journal produced no query state"
                          session-id)))
          (e-session-storage-commit-mutation-batch-with-query-delta
           store session-id records query-delta)
          (setq count (+ count (length records))))))
    count))

(defun e-runtime-migration--import-boards (runtime decoded)
  "Import Board roots, participants, and ordered message facts from DECODED."
  (let ((store (e-runtime-sqlite-runtime-store runtime))
        (state (make-hash-table :test 'equal))
        (participant-count 0)
        (count 0))
    (dolist (root (plist-get (plist-get decoded :boards) :roots))
      (let ((created
             (e-runtime-store-call
              store 'write
              (list :op 'board-create :board-id (plist-get root :board-id)
                    :trusted-principal (plist-get root :principal)
                    :root root))))
        (puthash (plist-get root :board-id) created state)))
    (dolist (entry (plist-get (plist-get decoded :boards) :associations))
      (let* ((session-id (plist-get entry :session-id))
             (board-id (plist-get entry :board-id))
             (association (plist-get entry :association))
             (policy (plist-get association :routing-policy))
             (participant-id (plist-get policy :participant-id))
             (principal (plist-get association :principal))
             (role (if (equal (plist-get association :association-role)
                              "participant")
                       'participant
                     'owner))
             (participant
              (list :id participant-id :author "e-runtime-migration"
                    :principal principal :controller principal :role role
                    :state 'active
                    :subscription-id (concat "sub_" participant-id)
                    :publication-pending nil
                    :source-session-id session-id)))
        (unless (gethash board-id state)
          (e-runtime-migration--legacy-board-conflict
           "Legacy Board participant has no imported Board root" session-id))
        (let ((updated
               (e-runtime-store-call
                store 'write
                (list :op 'board-participant-put :board-id board-id
                      :participant participant))))
          (puthash board-id updated state))
        (e-runtime-store-call
         store 'write
         (list :op 'board-session-association-put
               :session-id session-id
               :board-id board-id
               :participant-id participant-id
               :association-role role
               :routing-policy policy))
        (setq participant-count (1+ participant-count))))
    (dolist (entry (plist-get (plist-get decoded :boards) :records))
      (let* ((board-id (plist-get entry :board-id))
             (source-session-id (plist-get entry :source-session-id))
             (source-position (plist-get entry :source-position))
             (message (copy-tree (plist-get entry :message) t))
             (root (gethash board-id state)))
        (unless root
          (e-runtime-migration--legacy-board-conflict
           "Legacy Board message has no imported Board root"
           source-session-id (plist-get message :id)))
        (plist-put message :source-session-id source-session-id)
        (setq message
              (plist-put message :record-kind (plist-get message :kind)))
        (setq root
              (e-runtime-store-call
               store 'write
               (list :op 'board-record-put :board-id board-id
                     :generation (plist-get root :generation)
                     :record message
                     :source
                     (list :kind 'legacy-session
                           :key (list :session-id source-session-id
                                      :position source-position)
                           :hash (e-runtime-migration--canonical-hash
                                  message)))))
        (puthash board-id root state)
        (setq count (1+ count))))
    (list :roots (hash-table-count state)
          :participants participant-count
          :records count)))

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
          (let* ((session-records
                  (e-runtime-migration--import-sessions runtime decoded)))
            (let ((imported
                   (list
                    :session-records session-records
                    ;; These values are retained in the deterministic source
                    ;; manifest as witnesses, not imported projections.
                    :session-checkpoint-witnesses
                    (length (plist-get decoded :session-checkpoints))
                    :session-catalog-witness t
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
                         (e-runtime-sqlite-runtime-store runtime) t)))))
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

(defun e-runtime-migration--cutover-paths (source root backup)
  "Validate and return normalized SOURCE, ROOT, and BACKUP cutover paths."
  (let* ((source (directory-file-name (expand-file-name source)))
         (root (directory-file-name (expand-file-name root)))
         (backup (directory-file-name (expand-file-name backup)))
         (root-parent (file-name-directory root))
         (backup-parent (file-name-directory backup)))
    (unless (file-directory-p source)
      (signal 'e-runtime-migration-cutover-error
              (list "Copied legacy source is missing" source)))
    (unless (file-directory-p root)
      (signal 'e-runtime-migration-cutover-error
              (list "Legacy runtime root is missing" root)))
    (when (or (file-symlink-p source) (file-symlink-p root))
      (signal 'e-runtime-migration-cutover-error
              (list "Cutover source and runtime root must be real directories"
                    source root)))
    (when (or (file-equal-p source root)
              (file-in-directory-p source root)
              (file-in-directory-p root source))
      (signal 'e-runtime-migration-cutover-error
              (list "Copied source must be outside the runtime root" source root)))
    (unless (file-equal-p root-parent backup-parent)
      (signal 'e-runtime-migration-cutover-error
              (list "Backup must be a sibling of the runtime root"
                    backup root)))
    (when (or (file-exists-p backup) (file-symlink-p backup))
      (signal 'e-runtime-migration-cutover-error
              (list "Cutover backup already exists" backup)))
    (when (or (file-exists-p (expand-file-name "store.sqlite3" root))
              (file-exists-p (expand-file-name "store.sqlite3.owner" root)))
      (signal 'e-runtime-migration-cutover-error
              (list "Runtime root already contains current or live SQLite state"
                    root)))
    (list source root backup)))

(defun e-runtime-migration-cutover (source root backup)
  "Replace legacy ROOT with verified SQLite state imported from SOURCE.

SOURCE must be an exact copied legacy tree outside ROOT.  BACKUP must be a
nonexistent sibling of ROOT.  The caller must stop Emacs first.  This offline
operation imports and verifies a sibling staging directory, rechecks that ROOT
still matches SOURCE, renames ROOT to BACKUP, and atomically installs the
staging directory at ROOT.  If installation fails after the first rename, the
original ROOT is restored.  Neither SOURCE nor BACKUP is deleted."
  (pcase-let* ((`(,source ,root ,backup)
                (e-runtime-migration--cutover-paths source root backup))
               (parent (file-name-directory root))
               (prefix (expand-file-name
                        (format ".%s.cutover-" (file-name-nondirectory root))
                        parent))
               (source-inventory (e-runtime-migration-inventory source))
               (root-inventory (e-runtime-migration-inventory root))
               (staging nil)
               (report nil)
               (installed nil))
    (unless (equal source-inventory root-inventory)
      (signal 'e-runtime-migration-cutover-error
              (list "Copied source does not exactly match the legacy root"
                    source root)))
    ;; Reserve a collision-free sibling name, then return it to the importer,
    ;; whose public install contract requires a nonexistent target.
    (setq staging (make-temp-file prefix t))
    (delete-directory staging)
    (unwind-protect
        (progn
          (setq report (e-runtime-migration-run source staging))
          (unless (and (equal source-inventory (plist-get report :inventory))
                       (equal source-inventory
                              (e-runtime-migration-inventory source)))
            (signal 'e-runtime-migration-cutover-error
                    (list "Copied source changed during offline verification"
                          source)))
          (setq report (plist-put report :operation 'cutover)
                report (plist-put report :installed root)
                report (plist-put report :backup backup))
          ;; Write the final operator paths before swapping, so no fallible
          ;; report mutation occurs after the new authority is installed.
          (e-runtime-migration--write-report staging report)
          (unless (equal source-inventory
                         (e-runtime-migration-inventory root))
            (signal 'e-runtime-migration-cutover-error
                    (list "Legacy root changed during offline verification"
                          root)))
          ;; A keyboard quit is deferred across the two same-parent renames.
          ;; Explicitly signalled quit still enters the same rollback path as
          ;; an install error, while a rename that already completed remains
          ;; canonical even if its caller did not observe acknowledgement.
          (let ((inhibit-quit t)
                (root-moved nil))
            (condition-case install-error
                (progn
                  (rename-file root backup nil)
                  (setq root-moved t)
                  (rename-file staging root nil)
                  (setq installed t)
                  report)
              ((error quit)
               (cond
                ((and (file-directory-p root)
                      (not (file-exists-p staging))
                      (file-regular-p
                       (expand-file-name "store.sqlite3" root)))
                 (setq installed t)
                 (signal (car install-error) (cdr install-error)))
                ((or root-moved
                     (and (file-directory-p backup)
                          (not (file-exists-p root))))
                 (let (restore-error)
                   (condition-case caught
                       (rename-file backup root nil)
                     ((error quit) (setq restore-error caught)))
                   (if restore-error
                       (signal
                        'e-runtime-migration-cutover-error
                        (list
                         "SQLite install and legacy-root restore both failed"
                         install-error restore-error root backup))
                     (signal (car install-error) (cdr install-error)))))
                (t
                 (signal (car install-error) (cdr install-error))))))))
      (when (and staging (file-directory-p staging) (not installed))
        (delete-directory staging t)))))

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
         (if (equal (getenv "E_RUNTIME_MIGRATION_OPERATION") "cutover")
             (e-runtime-migration-cutover
              (getenv "E_RUNTIME_MIGRATION_SOURCE")
              (getenv "E_RUNTIME_MIGRATION_TARGET")
              (getenv "E_RUNTIME_MIGRATION_BACKUP"))
           (e-runtime-migration-run
            (getenv "E_RUNTIME_MIGRATION_SOURCE")
            (getenv "E_RUNTIME_MIGRATION_TARGET")
            :dry-run (equal (getenv "E_RUNTIME_MIGRATION_DRY_RUN") "t"))))
        (terpri))
    (error
     (princ
      (format "e-runtime-migrate: %s\n"
              (e-runtime-migration--bounded-error-message err))
      #'external-debugging-output)
     (kill-emacs 1))))

(provide 'e-runtime-migration)

;;; e-runtime-migration.el ends here

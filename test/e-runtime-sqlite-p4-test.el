;;; e-runtime-sqlite-p4-test.el --- Feature 87 P4 completion scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-default-harnesses)
(require 'e-runtime-migration)
(require 'e-runtime-store-offline)

(defun e-runtime-sqlite-p4-test--mode (file)
  "Return FILE permission bits."
  (logand (file-modes file) #o777))

(defun e-runtime-sqlite-p4-test--wait-ready (store)
  "Observe STORE's asynchronous open before direct fixture access."
  (when-let* ((request (e-runtime-store--open-control-request store)))
    (e-runtime-store-await store request 5.0))
  store)

(defun e-runtime-sqlite-p4-test--write (file text)
  "Write TEXT to FILE for a disposable legacy fixture."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)))

(defun e-runtime-sqlite-p4-test--write-session-records
    (root session-id records)
  "Write disposable retired session RECORDS below legacy ROOT."
  (e-runtime-sqlite-p4-test--write
   (expand-file-name (format "sessions/sessions/%s.jsonl" session-id) root)
   (concat
    (mapconcat (lambda (record)
                 (json-encode (e-session-codec-record-for-json record)))
               records "\n")
    "\n")))

(defun e-runtime-sqlite-p4-test--write-session-checkpoint
    (root session-id root-record)
  "Write a retired checkpoint for SESSION-ID after ROOT-RECORD in ROOT."
  (let* ((physical (e-session-codec-record-for-json root-record))
         (first-line (json-encode physical))
         (checkpoint
          (list :version 1 :session-id session-id
                :journal-byte-offset (string-bytes (concat first-line "\n"))
                :records (vector physical)
                :writer-high-watermarks nil
                :legacy-extra (list :enabled :json-false :labels '("a" "b")))))
    (e-runtime-sqlite-p4-test--write
     (expand-file-name
      (format "sessions/sessions/%s.checkpoint.json" session-id) root)
     (concat (json-encode checkpoint) "\n"))))

(defun e-runtime-sqlite-p4-test--legacy-fixture ()
  "Return a representative disposable legacy source tree."
  (let* ((root (make-temp-file "e-runtime-p4-legacy-" t))
         (root-record
          '(:type "session" :session-id "restored-session" :id "legacy-root"
            :created-at "2026-01-01T00:00:00Z"
            :updated-at "2026-01-01T00:00:01Z" :metadata nil))
         (message-record
          '(:type "message" :session-id "restored-session"
            :id "legacy-message" :parent-id "legacy-root"
            :timestamp "2026-01-01T00:00:01Z"
            :message (:role user :content "exact legacy input"
                      :created-at "2026-01-01T00:00:01Z" :type message
                      :id "legacy-message" :parent-id "legacy-root"))))
    (e-runtime-sqlite-p4-test--write-session-records
     root "restored-session" (list root-record message-record))
    (e-runtime-sqlite-p4-test--write-session-checkpoint
     root "restored-session" root-record)
    (e-runtime-sqlite-p4-test--write
     (expand-file-name
      "sessions/sessions/restored-session.checkpoint.json.bak.20260810T173134Z"
      root)
     "preserved checkpoint backup")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name
      "sessions/sessions/restored-session.jsonl.bak.20260810T173134Z" root)
     "preserved journal backup")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name "sessions/index.json.bak.20260810T173134Z" root)
     "preserved index backup")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name "task-queue/records.eld" root)
     "(:sequence 4 :paused-p t :order (\"legacy-task\" \"done-task\" \"in-flight\") :records ((:task-id \"legacy-task\" :status paused :prompt \"do legacy work\") (:task-id \"done-task\" :status done :outputs (\"exact\")) (:task-id \"in-flight\" :status running :attempt-id \"old-attempt\")))")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name
      "task-queue/grimoire-daily/1e17e2718d8ad55d/records.eld" root)
     "(:sequence 7 :paused-p nil :order (\"daily-1\" \"daily-2\") :records ((:task-id \"daily-1\" :status done :outputs (\"first\")) (:task-id \"daily-2\" :status done :outputs (\"second\"))))")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name
      "task-queue/grimoire-daily/1e17e2718d8ad55d/daily-fragment.org" root)
     "* Historical task product\n")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name "sessions/chat-overview-state.json" root)
     "{\"restored-session\":\"legacy-message\"}")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name "cron-state.eld" root)
     "#s(hash-table size 2 test equal data (\"legacy-cron\" (:anchor 10.0 :last-fire 20.0)))")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name "voice-tells.eld" root)
     "((:key \"plain\" :label \"Plain\" :description \"Write plainly\" :count 2 :last \"2026-01-01T00:00:00Z\"))")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name "goodnite/state/daydream_access.jsonl" root)
     "{\"kind\":\"read\",\"entry_uri\":\"goodnite://one\",\"engine\":\"e\"}\n")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name "raw-results/result.txt" root) "raw exact")
    (e-runtime-sqlite-p4-test--write
     (expand-file-name "session-tmp/restored-session/note.txt" root)
     "tmp exact")
    root))

(ert-deftest e-runtime-sqlite-p4-s9-invalid-legacy-input-never-installs ()
  "Missing, incomplete, conflicting, or unmapped input leaves no target."
  (let* ((missing (make-temp-file "e-runtime-p4-missing-" t))
         (missing-target (concat missing "-target"))
         (incomplete (e-runtime-sqlite-p4-test--legacy-fixture))
         (incomplete-target (concat incomplete "-target"))
         (conflict (e-runtime-sqlite-p4-test--legacy-fixture))
         (conflict-target (concat conflict "-target"))
         (unmapped (e-runtime-sqlite-p4-test--legacy-fixture))
         (unmapped-target (concat unmapped "-target")))
    (unwind-protect
        (progn
          (should-error (e-runtime-migration-run missing missing-target)
                        :type 'e-runtime-migration-error)
          (should-not (file-exists-p missing-target))
          (let ((journal
                 (expand-file-name
                  "sessions/sessions/restored-session.jsonl" incomplete)))
            (with-temp-buffer
              (insert-file-contents journal)
              (goto-char (point-max))
              (delete-char -1)
              (write-region nil nil journal nil 'silent)))
          (should-error (e-runtime-migration-run incomplete incomplete-target)
                        :type 'e-session-legacy-error)
          (should-not (file-exists-p incomplete-target))
          (e-runtime-sqlite-p4-test--write-session-records
           conflict "board-a"
           '((:type "session" :session-id "board-a" :id "board-a-root")
             (:type "board-session-state" :session-id "board-a"
              :board-state (:board-id "shared-board" :principal "alice"
                            :association-role "owner"))))
          (e-runtime-sqlite-p4-test--write-session-records
           conflict "board-b"
           '((:type "session" :session-id "board-b" :id "board-b-root")
             (:type "board-session-state" :session-id "board-b"
              :board-state (:board-id "shared-board" :principal "bob"
                            :association-role "owner"))))
          (should-error (e-runtime-migration-run conflict conflict-target)
                        :type 'e-runtime-migration-conflict)
          (should-not (file-exists-p conflict-target))
          (e-runtime-sqlite-p4-test--write
           (expand-file-name "task-queue/unmapped.cache" unmapped) "state")
          (should-error (e-runtime-migration-run unmapped unmapped-target)
                        :type 'e-runtime-migration-error)
          (should-not (file-exists-p unmapped-target)))
      (dolist (directory
               (list missing missing-target incomplete incomplete-target
                     conflict conflict-target unmapped unmapped-target))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-s9-cli-errors-are-bounded ()
  "The migration CLI reports an operator error without a payload backtrace."
  (let* ((repo (locate-dominating-file default-directory "scripts"))
         (source (e-runtime-sqlite-p4-test--legacy-fixture))
         (target (concat source "-target"))
         (script (expand-file-name "scripts/e-runtime-migrate"
                                   repo))
         (cold-root (make-temp-file "e-runtime-p4-cold-cli-" t))
         (cold-script (expand-file-name "scripts/e-runtime-migrate"
                                        cold-root))
         (snapshot
          (format "(:sequence 1 :order (\"bad\") :records ((:task-id \"bad\" :status unknown :prompt %S)))"
                  (make-string (* 256 1024) ?x)))
         (stdout (generate-new-buffer " *e-runtime-migrate-cli-output*"))
         (stderr (make-temp-file "e-runtime-migrate-cli-stderr-"))
         (upgrade-script (expand-file-name "scripts/e-runtime-upgrade"
                                           repo))
         exit output upgrade-output)
    (unwind-protect
        (progn
          ;; Reproduce an archive checkout with no adjacent bytecode.  The
          ;; entrypoint itself must keep load-time warning chatter bounded.
          (copy-directory (expand-file-name "lisp" repo)
                          (expand-file-name "lisp" cold-root) nil nil t)
          (dolist (file
                   (directory-files-recursively cold-root "\\.elc\\'"))
            (delete-file file))
          (make-directory (file-name-directory cold-script) t)
          (copy-file script cold-script t)
          (set-file-modes cold-script #o755)
          (e-runtime-sqlite-p4-test--write
           (expand-file-name "task-queue/records.eld" source) snapshot)
          (setq exit
                (process-file cold-script nil (list stdout stderr) nil
                              "dry-run" source target))
          (setq output
                (concat
                 (with-current-buffer stdout (buffer-string))
                 (with-temp-buffer
                   (insert-file-contents stderr)
                   (buffer-string))))
          (should-not (zerop exit))
          (should (< (string-bytes output) 4096))
          (should (string-match-p "e-runtime-migrate:" output))
          (should-not (string-match-p "Debugger entered" output))
          (should-not (file-exists-p target))
          (with-current-buffer stdout (erase-buffer))
          (write-region "" nil stderr nil 'silent)
          (setq exit (process-file upgrade-script nil (list stdout stderr) nil
                                   source))
          (setq upgrade-output
                (concat
                 (with-current-buffer stdout (buffer-string))
                 (with-temp-buffer
                   (insert-file-contents stderr)
                   (buffer-string))))
          (should-not (zerop exit))
          (should (< (string-bytes upgrade-output) 4096))
          (should (string-match-p "e-runtime-upgrade:" upgrade-output))
          (should-not (string-match-p "Debugger entered" upgrade-output)))
      (kill-buffer stdout)
      (when (file-exists-p stderr) (delete-file stderr))
      (dolist (directory (list source target cold-root))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-s9-explicit-upgrade-backs-up-before-install ()
  "Ordinary startup rejects v4; explicit v5 upgrade verifies a backup."
  (let* ((directory (make-temp-file "e-runtime-p4-upgrade-" t))
         (store (e-runtime-store-open directory))
         (database (expand-file-name "store.sqlite3" directory))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (should (= (plist-get
                      (e-runtime-store-call
                       store 'write
                       '(:op session-append :session-id "upgrade-preserved"
                         :record (:value before-v5)))
                      :revision)
                     1))
          (e-runtime-store-close store)
          (setq store nil)
          ;; This fixture mutation occurs only in the isolated test process.
          (let ((db (sqlite-open database)))
            (sqlite-execute
             db "UPDATE store_meta SET value='4' WHERE key='schema_version'")
            (sqlite-execute db "DELETE FROM schema_migrations WHERE version=5")
            (sqlite-execute db "DROP TABLE runtime_store_receipts")
            (sqlite-execute db "DROP TABLE runtime_store_state")
            (sqlite-close db))
          (should-error
           (e-runtime-sqlite-p4-test--wait-ready
            (e-runtime-store-open directory))
                        :type 'e-runtime-store-schema-too-old)
          ;; Ordinary v4 open is a refusal, not an implicit partial upgrade.
          (let ((db (sqlite-open database)))
            (unwind-protect
                (progn
                  (should (equal (car (car (sqlite-select db "SELECT value FROM store_meta WHERE key='schema_version'"))) "4"))
                  (should-not (car (sqlite-select db "SELECT 1 FROM sqlite_master WHERE name='runtime_store_state'"))))
              (sqlite-close db)))
          (let ((result (e-runtime-store-offline-upgrade directory backup)))
            (should (= (plist-get result :from) 4))
            (should (= (plist-get result :to) 5))
            (should (equal (plist-get result :integrity) "ok"))
            (should (= (e-runtime-sqlite-p4-test--mode backup) #o600)))
          (setq store
                (e-runtime-sqlite-p4-test--wait-ready
                 (e-runtime-store-open directory)))
          (should (= (plist-get (e-runtime-store-metrics store)
                                :schema-version)
                     5))
          (should (equal
                   (plist-get
                    (car (plist-get
                          (e-runtime-store-call
                           store 'read
                           '(:op session-record-page :session-id "upgrade-preserved"))
                          :records))
                    :value)
                   '(:value before-v5)))
          ;; An upgraded v5 store uses the same receipt recovery path.
          (let* ((marker (make-temp-file "e-runtime-upgrade-recovery-"))
                 (process-environment
                  (cons "E_RUNTIME_STORE_TEST_FAULT=after-commit"
                        (cons (concat "E_RUNTIME_STORE_TEST_FAULT_ONCE_FILE=" marker)
                              process-environment))))
            (delete-file marker)
            ;; Restart under the fault environment, then recover one write.
            (e-runtime-store-close store)
            (setq store (e-runtime-store-open directory))
            (should (> (plist-get
                        (e-runtime-store-call
                         store 'write
                         '(:op session-append :session-id "upgrade-preserved"
                           :record (:value recovered-v5)))
                        :revision)
                       1))
            (should (file-exists-p marker))
            (delete-file marker)))
      (when store (ignore-errors (e-runtime-store-close store)))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p4-s92-v5-upgrade-fault-rolls-back-without-partial-schema ()
  "An injected v4-to-v5 upgrade fault leaves the old store untouched."
  (let* ((directory (make-temp-file "e-runtime-v5-rollback-" t))
         (store (e-runtime-store-open directory))
         (database (expand-file-name "store.sqlite3" directory))
         (backup (expand-file-name "operator/pre-v5.sqlite3" directory)))
    (unwind-protect
        (progn
          (e-runtime-sqlite-p4-test--wait-ready store)
          (e-runtime-store-close store)
          (setq store nil)
          (let ((db (sqlite-open database)))
            (sqlite-execute db "UPDATE store_meta SET value='4' WHERE key='schema_version'")
            (sqlite-execute db "DELETE FROM schema_migrations WHERE version=5")
            (sqlite-execute db "DROP TABLE runtime_store_receipts")
            (sqlite-execute db "DROP TABLE runtime_store_state")
            (sqlite-close db))
          (let ((process-environment
                 (cons "E_RUNTIME_STORE_TEST_MIGRATION_FAULT=1" process-environment)))
            (should-error (e-runtime-store-offline-upgrade directory backup)
                          :type 'e-runtime-store-offline-error))
          (should (file-exists-p backup))
          (let ((db (sqlite-open database)))
            (unwind-protect
                (progn
                  (should (equal (car
                                  (car (sqlite-select db "SELECT value FROM store_meta WHERE key='schema_version'")))
                                 "4"))
                  (should-not (car (sqlite-select db "SELECT 1 FROM sqlite_master WHERE name='runtime_store_receipts'")))
                  (should-not (car (sqlite-select db "SELECT 1 FROM sqlite_master WHERE name='runtime_store_state'"))))
              (sqlite-close db))))
      (when store (ignore-errors (e-runtime-store-close store)))
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p4-s9-backup-integrity-metrics-and-permissions ()
  "Typed maintenance operations remain bounded, verified, and restrictive."
  (let* ((directory (make-temp-file "e-runtime-p4-maintenance-" t))
         (backup (expand-file-name "backups/operator.sqlite3" directory))
         (store (e-runtime-store-open directory)))
    (unwind-protect
        (progn
          (should (plist-get (e-runtime-store-integrity store t) :ok))
          (let ((metrics (e-runtime-store-metrics store)))
            (should (= (plist-get metrics :schema-version) 5))
            (should (> (plist-get metrics :database-bytes) 0)))
          (should (plist-get (e-runtime-store-backup store backup) :verified))
          (should (= (e-runtime-sqlite-p4-test--mode backup) #o600))
          (should (= (e-runtime-sqlite-p4-test--mode directory) #o700)))
      (e-runtime-store-close store)
      (delete-directory directory t))))

(ert-deftest e-runtime-sqlite-p4-s1-migration-is-deterministic-and-restores ()
  "Dry runs agree, install is atomic, sources stay unchanged, and restart restores."
  (let* ((source (e-runtime-sqlite-p4-test--legacy-fixture))
         (first-target (concat source "-dry-one"))
         (second-target (concat source "-dry-two"))
         (target (concat source "-installed"))
         (before (e-runtime-migration-inventory source))
         (first (e-runtime-migration-run source first-target :dry-run t))
         (second (e-runtime-migration-run source second-target :dry-run t))
         runtime)
    (unwind-protect
        (progn
          (let ((overview
                 (seq-find
                  (lambda (entry)
                    (equal (plist-get entry :path)
                           "sessions/chat-overview-state.json"))
                  before)))
            (should (eq (plist-get overview :owner) 'chat-overview))
            (should (eq (plist-get overview :disposition)
                        'retired-derived)))
          (dolist (path
                   '("sessions/sessions/restored-session.checkpoint.json.bak.20260810T173134Z"
                     "sessions/sessions/restored-session.jsonl.bak.20260810T173134Z"
                     "sessions/index.json.bak.20260810T173134Z"))
            (should
             (eq (plist-get
                  (seq-find (lambda (entry)
                              (equal (plist-get entry :path) path))
                            before)
                  :disposition)
                 'source-preserved-backup)))
          (should
           (eq (plist-get
                (seq-find
                 (lambda (entry)
                   (equal (plist-get entry :path)
                          "task-queue/grimoire-daily/1e17e2718d8ad55d/daily-fragment.org"))
                 before)
                :disposition)
               'source-preserved-file-product))
          (should (equal (plist-get first :manifest)
                         (plist-get second :manifest)))
          (should (equal before (e-runtime-migration-inventory source)))
          (let ((installed (e-runtime-migration-run source target)))
            (should (equal (plist-get (plist-get installed :integrity) :ok) t))
            (should (file-regular-p (expand-file-name "store.sqlite3" target)))
            (should (file-regular-p
                     (expand-file-name "migration-report.eld" target))))
          (let ((e-runtime-sqlite--live-composition nil))
            (setq runtime (e-runtime-sqlite-open target :load-sessions t))
            (let ((checkpoint
                   (e-session-storage-read-resume-checkpoint
                    (e-runtime-sqlite-session-store runtime)
                    "restored-session")))
              (should (= (plist-get checkpoint :journal-byte-offset) 1))
              (should (equal (plist-get checkpoint :legacy-extra)
                             '(:enabled :json-false :labels ("a" "b"))))
              (should (equal (plist-get (car (plist-get checkpoint :records))
                                        :id)
                             "legacy-root")))
            (should
             (equal (plist-get
                     (car (e-session-messages
                           (e-runtime-sqlite-session-store runtime)
                           "restored-session"))
                     :content)
                    "exact legacy input"))
            (let ((tasks (e-task-storage-snapshot
                          (e-runtime-sqlite-task-storage runtime) "default" 10)))
              (should (plist-get tasks :paused-p))
              (should (= (plist-get tasks :sequence) 4))
              (should (equal (mapcar (lambda (record)
                                       (plist-get record :status))
                                     (plist-get tasks :records))
                             '(paused done interrupted)))
              (should (equal (plist-get
                              (nth 1 (plist-get tasks :records)) :outputs)
                             '("exact"))))
            (let ((daily
                   (e-task-storage-snapshot
                    (e-runtime-sqlite-task-storage runtime)
                    "grimoire-daily/1e17e2718d8ad55d" 10)))
              (should (= (plist-get daily :sequence) 7))
              (should (equal
                       (mapcar (lambda (record)
                                 (plist-get record :status))
                               (plist-get daily :records))
                       '(done done))))
            (should (= (length
                        (plist-get
                         (e-goodnite-storage-page
                          (e-runtime-sqlite-goodnite-storage runtime) 0 10)
                         :events))
                       1))
            (should (equal
                     (plist-get
                      (e-raw-results-storage-read
                       (e-runtime-sqlite-raw-results-storage runtime)
                       "raw-result://result.txt" 0)
                      :content)
                     "raw exact"))
            (e-runtime-sqlite-close runtime)
            (setq runtime nil)
            (setq runtime (e-runtime-sqlite-open target :load-sessions t))
            (should (= (length
                        (e-session-messages
                         (e-runtime-sqlite-session-store runtime)
                         "restored-session"))
                       1)))
          (should (equal before (e-runtime-migration-inventory source)))
          (should-error (e-runtime-migration-run source target)
                        :type 'e-runtime-migration-target-exists))
      (when runtime (ignore-errors (e-runtime-sqlite-close runtime)))
      (dolist (directory (list source target first-target second-target))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-rootless-session-default-restore ()
  "Migration normalizes a rootless journal before ordinary default startup."
  (let* ((source (e-runtime-sqlite-p4-test--legacy-fixture))
         (target (concat source "-rootless-installed"))
         (session-id "20260807T101346-90ba4ce0f030")
         (original
          `((:type "board-message" :session-id ,session-id
             :message (:id "board-1" :kind "input"
                       :created-at 1786097627.224162
                       :content "preserved board input"))
            (:type "message" :session-id ,session-id :id "message-1"
             :parent-id "legacy-root-id"
             :timestamp "2026-08-07T10:13:48Z"
             :message (:id "message-1" :parent-id "legacy-root-id"
                       :role "user" :content "preserved session input"))))
         (process-environment (copy-sequence process-environment))
         (e-session-directory (expand-file-name "custom-layout" source))
         (e-default--runtime nil)
         (e-default--chat-sessions nil)
         (e-runtime-sqlite--live-composition nil))
    (unwind-protect
        (progn
          (e-runtime-sqlite-p4-test--write-session-records
           source session-id original)
          (e-runtime-sqlite-p4-test--write
           (expand-file-name "sessions/index.json" source)
           (concat (json-encode
                    (vector (list :id session-id :message-count 0)))
                   "\n"))
          (e-runtime-migration-run source target)
          (setenv "E_RUNTIME_STATE_DIRECTORY" target)
          (let* ((store (e-default-session-store))
                 (physical
                  (e-session-storage-read-session-records store session-id))
                 (session (e-session-get store session-id)))
            (should-not
             (e-session-storage-resume-checkpoint-present-p store session-id))
            (should (= (length physical) (1+ (length original))))
            (should (equal (plist-get (car physical) :type) "session"))
            (should (equal (plist-get (car physical) :id) "legacy-root-id"))
            (should (equal (plist-get session :created-at)
                           "2026-08-07T10:13:47Z"))
            (should (equal
                     (plist-get (car (e-session-messages store session-id))
                                :content)
                     "preserved session input")))
          (e-default-runtime-close)
          (setq e-default--runtime nil e-default--chat-sessions nil)
          (let ((store (e-default-session-store)))
            (should (equal
                     (plist-get (car (e-session-messages store session-id))
                                :content)
                     "preserved session input"))))
      (e-default-runtime-close)
      (dolist (directory (list source target))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-catalog-failure-does-not-install ()
  "A catalog publication failure preserves source and leaves no target."
  (let* ((source (e-runtime-sqlite-p4-test--legacy-fixture))
         (target (concat source "-catalog-failure"))
         (before (e-runtime-migration-inventory source)))
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function
                 'e-session-storage-publish-catalog-projection)
                (lambda (&rest _args)
                  (signal 'e-session-storage-error
                          '("injected catalog publication failure")))))
            (should-error (e-runtime-migration-run source target)
                          :type 'e-session-storage-error))
          (should (equal before (e-runtime-migration-inventory source)))
          (should-not (file-exists-p target)))
      (dolist (directory (list source target))
        (when (file-directory-p directory)
          (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-cutover-preserves-root-backup-and-default-restores ()
  "Offline same-root cutover preserves legacy state and feeds ordinary startup."
  (let* ((root (e-runtime-sqlite-p4-test--legacy-fixture))
         (source (e-runtime-sqlite-p4-test--legacy-fixture))
         (backup (concat root ".backup"))
         (before (e-runtime-migration-inventory root))
         (process-environment (copy-sequence process-environment))
         (e-session-directory (expand-file-name "sessions" root))
         (e-default--runtime nil)
         (e-default--chat-sessions nil)
         (e-runtime-sqlite--live-composition nil))
    (setenv "E_RUNTIME_STATE_DIRECTORY" nil)
    (unwind-protect
        (progn
          (let ((err
                 (should-error (e-default-session-store)
                               :type 'e-runtime-store-migration-required)))
            (should (string-match-p (regexp-quote root)
                                    (error-message-string err)))
            (should (string-match-p "COPIED_LEGACY_SOURCE"
                                    (error-message-string err)))
            (should (string-match-p "SIBLING_BACKUP"
                                    (error-message-string err)))
            (should (string-match-p "restart Emacs"
                                    (error-message-string err))))
          (should-not (file-exists-p (expand-file-name "store.sqlite3" root)))
          (should (equal before (e-runtime-migration-inventory root)))
          (let ((result (e-runtime-migration-cutover source root backup)))
            (should (eq (plist-get result :operation) 'cutover))
            (should (equal (plist-get result :installed) root))
            (should (equal (plist-get result :backup) backup)))
          (should (equal before (e-runtime-migration-inventory backup)))
          (should (file-regular-p (expand-file-name "store.sqlite3" root)))
          (should-not (file-exists-p (expand-file-name "sessions" root)))
          (let ((sessions (e-default-session-store)))
            (should (equal (e-session-store-directory sessions)
                           (file-name-as-directory root)))
            (should (equal
                     (plist-get
                      (car (e-session-messages sessions "restored-session"))
                      :content)
                     "exact legacy input"))))
      (e-default-runtime-close)
      (dolist (directory (list root source backup))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-cutover-preflight-failure-leaves-root-unchanged ()
  "A stale copied source cannot move or mutate the legacy root."
  (let* ((root (e-runtime-sqlite-p4-test--legacy-fixture))
         (source (e-runtime-sqlite-p4-test--legacy-fixture))
         (backup (concat root ".backup")))
    (unwind-protect
        (progn
          (e-runtime-sqlite-p4-test--write
           (expand-file-name "voice-tells.eld" source)
           "((:key \"plain\" :label \"Plain\" :description \"changed copy\" :count 2 :last \"2026-01-01T00:00:00Z\"))")
          (let ((before (e-runtime-migration-inventory root)))
            (should-error (e-runtime-migration-cutover source root backup)
                          :type 'e-runtime-migration-cutover-error)
            (should (equal before (e-runtime-migration-inventory root)))
            (should-not (file-exists-p backup))
            (should-not
             (file-exists-p (expand-file-name "store.sqlite3" root)))))
      (dolist (directory (list root source backup))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-cutover-install-failure-restores-root ()
  "A failed post-move SQLite install restores the exact legacy root."
  (let* ((root (e-runtime-sqlite-p4-test--legacy-fixture))
         (source (e-runtime-sqlite-p4-test--legacy-fixture))
         (backup (concat root ".backup"))
         (before (e-runtime-migration-inventory root))
         (real-rename (symbol-function 'rename-file)))
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'rename-file)
                (lambda (old new &optional ok-if-already-exists)
                  (if (and (equal (directory-file-name (expand-file-name new))
                                  root)
                           (not (equal
                                 (directory-file-name (expand-file-name old))
                                 backup)))
                      (signal 'file-error '("simulated SQLite install failure"))
                    (funcall real-rename old new ok-if-already-exists)))))
            (should-error (e-runtime-migration-cutover source root backup)
                          :type 'file-error))
          (should (file-directory-p root))
          (should-not (file-exists-p backup))
          (should (equal before (e-runtime-migration-inventory root)))
          (should-not
           (file-exists-p (expand-file-name "store.sqlite3" root))))
      (dolist (directory (list root source backup))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-cutover-quit-before-install-restores-root ()
  "An ordinary quit before the install rename cannot strand the logical root."
  (let* ((root (e-runtime-sqlite-p4-test--legacy-fixture))
         (source (e-runtime-sqlite-p4-test--legacy-fixture))
         (backup (concat root ".backup"))
         (before (e-runtime-migration-inventory root))
         (real-rename (symbol-function 'rename-file))
         caught)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'rename-file)
                (lambda (old new &optional ok-if-already-exists)
                  (if (and (equal (directory-file-name (expand-file-name new))
                                  root)
                           (not (equal
                                 (directory-file-name (expand-file-name old))
                                 backup)))
                      (signal 'quit nil)
                    (funcall real-rename old new ok-if-already-exists)))))
            (condition-case err
                (e-runtime-migration-cutover source root backup)
              (quit (setq caught err))))
          (should (eq (car caught) 'quit))
          (should (file-directory-p root))
          (should-not (file-exists-p backup))
          (should (equal before (e-runtime-migration-inventory root)))
          (should-not
           (file-exists-p (expand-file-name "store.sqlite3" root))))
      (dolist (directory (list root source backup))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-cutover-rejects-non-sibling-and-current-store ()
  "Cutover refuses a non-atomic backup layout or an existing SQLite authority."
  (let* ((root (e-runtime-sqlite-p4-test--legacy-fixture))
         (source (e-runtime-sqlite-p4-test--legacy-fixture))
         (other-parent (make-temp-file "e-runtime-p4-other-" t))
         (non-sibling (expand-file-name "backup" other-parent))
         (sibling (concat root ".backup")))
    (unwind-protect
        (progn
          (should-error (e-runtime-migration-cutover source root non-sibling)
                        :type 'e-runtime-migration-cutover-error)
          (should-not (file-exists-p non-sibling))
          (e-runtime-sqlite-p4-test--write
           (expand-file-name "store.sqlite3" root) "already current")
          (should-error (e-runtime-migration-cutover source root sibling)
                        :type 'e-runtime-migration-cutover-error)
          (should-not (file-exists-p sibling)))
      (dolist (directory (list root source sibling other-parent))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-cutover-cli-dispatches-offline-swap ()
  "The operator script dispatches its explicit four-argument cutover surface."
  (let* ((root (e-runtime-sqlite-p4-test--legacy-fixture))
         (source (e-runtime-sqlite-p4-test--legacy-fixture))
         (backup (concat root ".backup"))
         (script (expand-file-name "scripts/e-runtime-migrate"
                                   (locate-dominating-file
                                    default-directory "scripts")))
         (output (generate-new-buffer " *e-runtime-cutover-cli-output*")))
    (unwind-protect
        (progn
          (should (zerop (process-file script nil output nil
                                       "cutover" source root backup)))
          (should (string-match-p ":operation cutover"
                                  (with-current-buffer output
                                    (buffer-string))))
          (should (file-regular-p (expand-file-name "store.sqlite3" root)))
          (should (file-directory-p backup)))
      (kill-buffer output)
      (dolist (directory (list root source backup))
        (when (file-directory-p directory) (delete-directory directory t))))))

(ert-deftest e-runtime-sqlite-p4-s10-default-runtime-is-one-sqlite-store ()
  "Ordinary defaults inject one SQLite composition and write no sidecars."
  (let* ((directory (make-temp-file "e-runtime-p4-default-" t))
         (process-environment (copy-sequence process-environment))
         (e-default--runtime nil)
         (e-default--chat-sessions nil)
         (e-runtime-sqlite--live-composition nil))
    (setenv "E_RUNTIME_STATE_DIRECTORY" directory)
    (unwind-protect
        (let* ((first (e-default-session-store))
               (second (e-default-session-store))
               (runtime (e-default-runtime)))
          (should (eq first second))
          (should (e-session-storage-sqlite-p first))
          (should (eq first (e-runtime-sqlite-session-store runtime)))
          (should (eq e-task-queue-actions-default-queue
                      (e-runtime-sqlite-task-queue runtime)))
          (should (e-task-queue-expose-await-references-p
                   e-task-queue-actions-default-queue))
          (e-session-create first :id "default-sqlite")
          (should (file-regular-p (expand-file-name "store.sqlite3" directory)))
          (dolist (sidecar '("records.eld" "cron-state.eld" "voice-tells.eld"
                             "daydream_access.jsonl" "index.json"))
            (should-not (file-exists-p (expand-file-name sidecar directory)))))
      (e-default-runtime-close)
      (delete-directory directory t))))

(provide 'e-runtime-sqlite-p4-test)

;;; e-runtime-sqlite-p4-test.el ends here

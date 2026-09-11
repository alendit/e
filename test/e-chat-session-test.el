;;; e-chat-session-test.el --- Tests for chat session capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for chat-session semantic actions without presentation rendering.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-chat-session)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-session)
(require 'e-session-async)
(require 'e-session-sqlite)
(require 'e-work)
(load (expand-file-name
       "e-test-environment-support.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-chat-session-test--await (work)
  "Observe request-scoped WORK at this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(defun e-chat-session-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE is non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) (or timeout 2.0))) value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(cl-defmacro e-chat-session-test--with-sqlite-harness
    ((harness store backend) &rest body)
  "Run BODY with a disposable async SQLite STORE and HARNESS."
  (declare (indent 1) (debug ((symbolp symbolp form) body)))
  `(let* ((store-directory (make-temp-file "e-chat-session-sql-" t))
          (,store (e-session-sqlite-store-create
                   store-directory :asynchronous t))
          (,harness (e-harness-create :backend ,backend :sessions ,store)))
     (unwind-protect
         (progn ,@body)
       ;; A held provider is intentional in queue/steer tests.  Release the
       ;; fixture's bounded live coordination before closing its SQLite worker
       ;; so a later failure callback cannot schedule work against a retired
       ;; attachment.
       (clrhash (e-harness-prompt-queues ,harness))
       (clrhash (e-harness-prompt-queue-counts ,harness))
       (clrhash (e-harness-active-turns ,harness))
       (when-let* ((bindings (gethash ,harness e-chat-service--bindings)))
         (maphash (lambda (_session-id binding)
                    (ignore-errors (e-chat-service--retire-binding binding)))
                  bindings))
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory store-directory t))))

(defun e-chat-session-test--create-session (harness session-id &optional metadata)
  "Create and passively admit SESSION-ID with METADATA in HARNESS."
  (let ((creation
         (e-chat-service-create-session-start
          :harness harness :id session-id :metadata metadata)))
    (e-chat-session-test--await
     (e-chat-service-binding-start harness session-id nil t))
    (e-chat-session-test--await creation)))

(defun e-chat-session-test--view (store session-id)
  "Return SESSION-ID's detached SQLite chat view from STORE."
  (e-chat-session-test--await
   (e-session-async-chat-view store session-id :limit 32)))

(ert-deftest e-chat-session-test-submit-validates-and-publishes-input ()
  "Submitting validates and persists through the public SQL chat service."
  (e-chat-session-test--with-sqlite-harness
      (harness store
               (e-backend-fake-create
                :items '((:type assistant-message :content "answer")
                         (:type done :reason stop))))
    (let ((creation
           (e-chat-service-create-session-start
            :harness harness :id "session-1")))
    (should-error
     (e-chat-session-submit harness "session-1" "")
     :type 'user-error)
    (let ((admission (e-chat-session-submit harness "session-1" "hello")))
      (should (e-work-handle-p admission))
      (e-chat-session-test--await admission)
      (e-chat-session-test--await creation)
      (should
       (e-chat-session-test--wait-until
        (lambda ()
          (equal (mapcar (lambda (message) (plist-get message :content))
                         (plist-get (e-chat-session-test--view
                                     store "session-1")
                                    :messages))
                 '("hello" "answer")))
        5.0))))))

(ert-deftest e-chat-session-test-submit-preserves-explicit-metadata ()
  "Submitting can record caller metadata beyond composer references."
  (let (seen-messages)
    (e-chat-session-test--with-sqlite-harness
        (harness _store
                 (e-backend-create
                  :name "capture"
                  :start
                  (cl-function
                   (lambda (&key messages on-request-start on-done
                                  &allow-other-keys)
                     (setq seen-messages (copy-tree messages t))
                     (let ((request (e-backend-request-create)))
                       (funcall on-request-start request)
                       (funcall on-done '(:status done))
                       request)))))
    (let* ((creation
            (e-chat-service-create-session-start
             :harness harness :id "session-1"))
           (admission
            (e-chat-session-submit
             harness "session-1" "hello"
             :metadata '(:org-canvas-scope thread)
             :references '((:uri "buffer://source")))))
      (e-chat-session-test--await admission)
      (e-chat-session-test--await creation)
      (should
       (e-chat-session-test--wait-until
        (lambda () seen-messages)
        5.0))
      (let* ((message (car (last seen-messages)))
             (metadata (plist-get message :metadata)))
        (should (equal (plist-get metadata :org-canvas-scope) 'thread))
        (should (equal (plist-get metadata :references)
                       '((:uri "buffer://source")))))))))

(ert-deftest e-chat-session-test-metadata-writes-drop-read-markers ()
  "Chat-session metadata writes do not retain presentation read markers."
  (let* ((project-root (file-name-as-directory
                        (make-temp-file "e-chat-session-root-" t))))
    (unwind-protect
        (e-chat-session-test--with-sqlite-harness
            (harness store (e-backend-create :name "noop"))
          (e-chat-session-test--create-session
           harness "session-1"
           (list :name "Chat" :project-root project-root
                 :e-chat-read-markers '(:chat-default "assistant-read")))
          (e-chat-session-attach-context
           harness "session-1" '(:uri "buffer://source")
           :current-attachments nil)
          (should
           (e-chat-session-test--wait-until
            (lambda ()
              (zerop (e-session-async-pending-count store "session-1")))))
          (let* ((metadata
                  (plist-get
                   (e-chat-session-test--await
                    (e-session-async-session-metadata store "session-1"))
                   :metadata))
                 (references (plist-get metadata :context-references))
                 (attachments
                  (e-chat-session-metadata-attachments metadata)))
            (should (equal (plist-get metadata :project-root) project-root))
            (should (equal (plist-get (car attachments) :uri)
                           "buffer://source"))
            (should (plist-member references :chat-session))
            (should-not (plist-member metadata :context-attachments))
            (should-not (plist-member metadata :e-chat-read-markers))))
      (delete-directory project-root t))))

(ert-deftest e-chat-session-test-queue-validates-and-routes-to-inbox ()
  "Queueing validates prompt text and reaches the active turn's inbox."
  (e-chat-session-test--with-sqlite-harness
      (harness _store
               (e-backend-create
                :name "held"
                :start
                (cl-function
                 (lambda (&key on-request-start &allow-other-keys)
                   (let ((request (e-backend-request-create)))
                     (funcall on-request-start request)
                     request)))))
    (e-chat-session-test--create-session harness "session-1")
    (e-chat-session-test--await
     (e-chat-session-submit harness "session-1" "running"))
    (should (e-chat-session-test--wait-until
             (lambda ()
               (e-chat-service-active-turn-p harness "session-1"))))
    (should-error
     (e-chat-session-queue harness "session-1" "")
     :type 'user-error)
    (let ((admission
           (e-chat-session-queue
            harness
            "session-1"
            "queued"
            :references '((:uri "buffer://source"))
            :metadata '(:source chat-composer))))
      (should (e-work-handle-p admission))
      (e-chat-session-test--await admission)
      (should
       (e-chat-session-test--wait-until
        (lambda () (e-harness-queued-prompts harness "session-1"))))
      (let ((item (car (e-harness-queued-prompts harness "session-1"))))
        (should (equal (plist-get item :prompt) "queued"))
        (should (equal (plist-get (plist-get item :metadata) :references)
                       '((:uri "buffer://source"))))
        (should (equal (plist-get (plist-get item :metadata) :source)
                       'chat-composer))))))

(ert-deftest e-chat-session-test-steer-validates-and-routes-to-active-turn ()
  "Steering validates prompt text and reaches the active steering lane."
  (let* ((backend (e-backend-create
                   :name "steerable"
                   :start (cl-function
                           (lambda (&key messages options on-item on-done
                                          on-error on-request-start)
                             (ignore messages options on-item on-done
                                     on-error)
                             (funcall on-request-start
                                      (e-backend-request-create))
                             nil))))
         )
    (e-chat-session-test--with-sqlite-harness (harness _store backend)
      (e-chat-session-test--create-session harness "session-1")
      (e-chat-session-test--await
       (e-chat-session-submit harness "session-1" "running"))
      (should (e-chat-session-test--wait-until
               (lambda ()
                 (e-chat-service-active-turn-p harness "session-1"))))
      (let (events)
        (e-chat-service-subscribe
         harness "session-1"
         (lambda (event) (push event events)))
        (should-error
         (e-chat-session-steer harness "session-1" "")
         :type 'user-error)
        (let ((admission
               (e-chat-session-steer
                harness "session-1" "focus here"
                :metadata '(:source chat-composer))))
          (should (e-work-handle-p admission))
          (e-chat-session-test--await admission))
        ;; Steering intent is a transient live-controller fact.  Observe it
        ;; through the public chat subscription instead of racing the harness
        ;; owner's internal pending-input queue, which may drain immediately.
        (should
         (e-chat-session-test--wait-until
          (lambda ()
            (seq-find
             (lambda (event)
               (and (eq (plist-get event :type) 'turn-steered)
                    (equal
                     (plist-get
                      (plist-get (plist-get event :payload) :metadata)
                      :source)
                     'chat-composer)))
             events)))))
      (e-chat-session-abort harness "session-1")
      (should
       (e-chat-session-test--wait-until
        (lambda ()
          (not (e-chat-service-active-turn-p harness "session-1"))))))))

(ert-deftest e-chat-session-test-abort-and-rename-use-live-and-sql-owners ()
  "Abort touches live work while rename persists through SQLite."
  (e-chat-session-test--with-sqlite-harness
      (harness store
               (e-backend-create
                :name "held"
                :start (lambda (&rest _args)
                         (e-backend-request-create))))
    (e-chat-session-test--create-session harness "session-1")
    (e-chat-session-test--await
     (e-chat-session-submit harness "session-1" "hello"))
    (should (e-chat-session-test--wait-until
             (lambda ()
               (e-chat-service-active-turn-p harness "session-1"))))
    (e-chat-session-abort harness "session-1")
    (should (e-chat-session-test--wait-until
             (lambda ()
               (not (e-chat-service-active-turn-p harness "session-1")))))
    (e-chat-session-test--await
     (e-chat-session-rename harness "session-1" "Renamed"))
    (let ((metadata
           (e-chat-session-test--await
            (e-session-async-session-metadata store "session-1"))))
      (should (equal (plist-get metadata :name) "Renamed")))))

(ert-deftest e-chat-session-test-compact-start-returns-before-summary ()
  "Chat-session compaction starts asynchronously and settles by callback."
  (let* ((backend (e-backend-create
                   :name 'delayed-summary
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done
                                   &allow-other-keys)
                      (ignore messages options)
                      (run-at-time
                       0.05 nil
                       (lambda ()
                         (funcall on-item
                                  '(:type assistant-message
                                    :content "Compacted summary."))
                         (funcall on-done '(:status done))))
                      (e-backend-request-create
                       :metadata '(:provider delayed-summary))))))
         record
         failure)
    (e-chat-session-test--with-sqlite-harness (harness _store backend)
      (e-chat-session-test--create-session harness "session-1")
      (dolist (message '((:role user :content "old")
                         (:role assistant :content "old answer")
                         (:role user :content "new")))
        (e-chat-session-test--await
         (e-chat-service-append-seed-message
          harness "session-1" message)))
      (let ((request
             (e-chat-session-compact-start
              harness "session-1"
              :on-done (lambda (value) (setq record value))
              :on-error (lambda (err) (setq failure err)))))
        (should (e-backend-request-p request))
        (should-not record)
        (should-not failure)
        (should
         (e-chat-session-test--wait-until (lambda () (or record failure))))
        (should-not failure)
        (should (equal (plist-get record :summary)
                       "Compacted summary."))))))

(ert-deftest e-chat-session-test-compact-action-starts-asynchronously ()
  "The chat-session compact action starts asynchronously through e-actions."
  (let* ((backend (e-backend-create
                   :name 'delayed-action-summary
                   :start
                   (cl-function
                    (lambda (&key messages options on-item on-done
                                   &allow-other-keys)
                      (ignore messages options)
                      (run-at-time
                       0.05 nil
                       (lambda ()
                         (funcall on-item
                                  '(:type assistant-message
                                    :content "Action summary."))
                         (funcall on-done '(:status done))))
                      (e-backend-request-create
                       :metadata '(:provider delayed-action-summary))))))
         )
    (let ((e-work--detached-handles (make-hash-table :test 'equal)))
      (e-chat-session-test--with-sqlite-harness (harness store backend)
        (e-harness-activate-capability harness (e-chat-session-capability-create))
        (e-chat-session-test--create-session harness "session-1")
        (dolist (message '((:role user :content "old")
                           (:role assistant :content "old answer")
                           (:role user :content "new")))
          (e-chat-session-test--await
           (e-chat-service-append-seed-message
            harness "session-1" message)))
        (let ((result
               (e-actions-call
                'chat-session
                :compact
                '(:keep_recent_tokens 1)
                (list :harness harness
                      :session-id "session-1"
                      :turn-id "turn-compact"))))
          (should (string-match-p "\\`work:" result)))
        (should
         (e-chat-session-test--wait-until
          (lambda ()
            (plist-get
             (e-chat-session-test--await
              (e-session-async-context-path store "session-1"))
             :compaction))))
        (should
         (equal
          (plist-get
           (plist-get
            (e-chat-session-test--await
             (e-session-async-context-path store "session-1"))
            :compaction)
           :summary)
          "Action summary."))))))

(ert-deftest e-chat-session-test-options-and-context ()
  "Chat-session options and context use detached SQLite operations."
  (e-chat-session-test--with-sqlite-harness
      (harness _store (e-backend-fake-create :items nil))
    (e-chat-session-test--create-session harness "session-1")
    (e-chat-session-test--await
     (e-chat-session-set-options
      harness "session-1"
      '(:model "gpt-test" :reasoning-effort "high")))
    (e-chat-session-test--await
     (e-chat-service-append-seed-message
      harness "session-1" '(:role user :content "context question")))
    (let ((work (e-chat-session-context harness "session-1")))
      (should (e-work-handle-p work))
      (let ((context (e-chat-session-test--await work)))
        (should (equal (plist-get (plist-get context :options) :model)
                       "gpt-test"))
        (should (equal
                 (plist-get (plist-get context :options) :reasoning-effort)
                 "high"))
        (should (equal (mapcar (lambda (message)
                                (plist-get message :content))
                              (plist-get context :messages))
                       '("context question")))))))

(ert-deftest e-chat-session-test-context-preview-uses-preview-purpose ()
  "Chat-session context preview keeps the explicit preview purpose."
  (let ((seen-purpose nil))
    (cl-letf (((symbol-function 'e-harness-context-preview-start)
               (lambda (_harness _session-id)
                 (setq seen-purpose 'preview)
                 'preview-work)))
      (should (eq (e-chat-session-context 'harness "session-1")
                  'preview-work))
      (should (eq seen-purpose 'preview)))))

(ert-deftest e-chat-session-test-capability-actions ()
  "The chat-session capability exposes stable shell action names."
  (let ((capability (e-chat-session-capability-create)))
    (should (eq (e-capability-id capability) 'chat-session))
    (dolist (action '(:submit :steer :queue :abort :compact :rename
                      :set-model :set-effort
                      :attach-context :detach-context :context))
      (should (e-action-p (e-capabilities-action-spec capability action))))
    (should (e-work-spec-p
             (e-action-work
              (e-capabilities-action-spec capability :compact))))
    (should (e-work-spec-p
             (e-action-work
              (e-capabilities-action-spec capability :context))))))

(ert-deftest e-chat-session-test-offline-migration-translates-checkpoint-offset ()
  "Rewriting a journal preserves the checkpoint's logical record boundary."
  (e-test-require-executable "python3")
  (let* ((directory (make-temp-file "e-chat-attachment-offset-" t))
         (sessions (expand-file-name "sessions" directory))
         (journal (expand-file-name "offset.jsonl" sessions))
         (checkpoint (expand-file-name "offset.checkpoint.json" sessions))
         (script
          (expand-file-name
           "docs/bugs/e-chat-resume-collapsed-attachment/migrate-chat-attachments.py"
           (locate-dominating-file default-directory "Eldev")))
         (first-line
          (concat
           "{\"type\":\"session\",\"session-id\":\"offset\",\"metadata\":"
           "{\"context-attachments\":{\"uri\":\"file://offset.org\","
           "\"label\":\"legacy\",\"canvas\":true}}}"))
         (second-line
          "{\"type\":\"activity-event\",\"session-id\":\"offset\"}")
         (old-offset (1+ (string-bytes first-line)))
         migrated-offset)
    (unwind-protect
        (progn
          (make-directory sessions t)
          (write-region (concat first-line "\n" second-line "\n")
                        nil journal nil 'silent)
          (write-region
           (format
            "{\"version\":1,\"session-id\":\"offset\",\"journal-byte-offset\":%d,\"records\":[]}\n"
            old-offset)
           nil checkpoint nil 'silent)
          (with-temp-buffer
            (should
             (zerop
              (call-process "python3" nil t nil script
                            "--session-root" directory)))
            (should (search-backward
                     "\"checkpoint-offsets-updated\": 1" nil t)))
          (with-temp-buffer
            (should
             (zerop
              (call-process "python3" nil t nil script
                            "--session-root" directory
                            "--apply" "--confirm-emacs-stopped"))))
          (setq migrated-offset
                (plist-get
                 (json-parse-string
                  (with-temp-buffer
                    (insert-file-contents checkpoint)
                    (buffer-string))
                  :object-type 'plist :array-type 'list)
                 :journal-byte-offset))
          (with-temp-buffer
            (insert-file-contents-literally journal)
            (goto-char (point-min))
            (forward-line 1)
            (should (= migrated-offset (1- (position-bytes (point)))))
            (should (equal (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))
                           second-line)))
          (should (/= migrated-offset old-offset)))
      (delete-directory directory t))))

(ert-deftest e-chat-session-test-offline-migration-covers-inventoried-shapes ()
  "The migrator rewrites every known shape in every session file format."
  (e-test-require-executable "python3")
  (let* ((directory (make-temp-file "e-chat-attachment-shapes-" t))
         (sessions (expand-file-name "sessions" directory))
         (journal (expand-file-name "collapsed.jsonl" sessions))
         (checkpoint (expand-file-name "flattened.checkpoint.json" sessions))
         (index (expand-file-name "index.json" directory))
         (script
          (expand-file-name
           "docs/bugs/e-chat-resume-collapsed-attachment/migrate-chat-attachments.py"
           (locate-dominating-file default-directory "Eldev")))
         files
         hashes)
    (unwind-protect
        (progn
          (make-directory sessions t)
          (write-region
           (concat
            "{\"metadata\":{\"context-references\":{\"chat-session\":"
            "{\"attachments\":{\"uri\":[\"file://collapsed.org\","
            "\"label\",\"collapsed\",\"canvas\",true]}}}}}\n")
           nil journal nil 'silent)
          (write-region
           (concat
            "{\"records\":[{\"metadata\":{\"context-references\":"
            "{\"chat-session\":{\"attachments\":[\"uri\","
            "[\"file://flattened.org\",\"label\",\"flattened\","
            "\"canvas\",true]]}}}}]}\n")
           nil checkpoint nil 'silent)
          (write-region
           (concat
            "[{\"id\":\"legacy\",\"metadata\":{\"context-attachments\":"
            "{\"uri\":\"file://legacy.org\",\"label\":\"legacy\","
            "\"canvas\":true}}}]\n")
           nil index nil 'silent)
          (setq files (list journal checkpoint index)
                hashes (mapcar (lambda (file) (secure-hash 'sha256 file)) files))
          (with-temp-buffer
            (let ((status
                   (call-process "python3" nil t nil script
                                 "--session-root" directory)))
              (unless (zerop status)
                (ert-fail
                 (format "Migration failed with status %s:\n%s"
                         status (buffer-string)))))
            (dolist (needle '("\"changed-records\": 3"
                              "\"collapsed\": 1"
                              "\"flattened\": 1"
                              "\"legacy-single\": 1"))
              (should (string-match-p (regexp-quote needle)
                                      (buffer-string)))))
          (should
           (equal hashes
                  (mapcar (lambda (file) (secure-hash 'sha256 file)) files)))
          (with-temp-buffer
            (should
             (zerop
              (call-process "python3" nil t nil script
                            "--session-root" directory
                            "--apply" "--confirm-emacs-stopped"))))
          (cl-labels
              ((attachments
                (metadata)
                (plist-get
                 (plist-get
                  (plist-get metadata :context-references)
                  :chat-session)
                 :attachments))
               (decode (file)
                (json-parse-string
                 (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))
                 :object-type 'plist :array-type 'list
                 :null-object nil :false-object nil)))
            (let* ((journal-record (decode journal))
                   (checkpoint-record (car (plist-get (decode checkpoint)
                                                       :records)))
                   (index-record (car (decode index))))
              (should
               (equal (plist-get (car (attachments
                                       (plist-get journal-record :metadata)))
                                 :uri)
                      "file://collapsed.org"))
              (should
               (equal (plist-get (car (attachments
                                       (plist-get checkpoint-record :metadata)))
                                 :uri)
                      "file://flattened.org"))
              (should
               (equal (plist-get (car (attachments
                                       (plist-get index-record :metadata)))
                                 :uri)
                      "file://legacy.org"))
              (should-not (plist-member (plist-get index-record :metadata)
                                        :context-attachments))))
          (should (= (length (directory-files sessions nil "\\.bak\\.")) 2))
          (should (= (length (directory-files directory nil "\\.bak\\.")) 1))
          (with-temp-buffer
            (should
             (zerop
              (call-process "python3" nil t nil script
                            "--session-root" directory)))
            (should (search-backward "\"changed-records\": 0" nil t)))
          (let* ((conflict (expand-file-name "conflict.jsonl" sessions))
                 (all-files (cons conflict files)))
            (write-region
             (concat
              "{\"metadata\":{"
              "\"context-attachments\":{\"uri\":\"file://old.org\"},"
              "\"context-references\":{\"chat-session\":{"
              "\"attachments\":[{\"uri\":\"file://new.org\"}]}}}}\n")
             nil conflict nil 'silent)
            (setq hashes
                  (mapcar (lambda (file) (secure-hash 'sha256 file)) all-files))
            (with-temp-buffer
              (should-not
               (zerop
                (call-process "python3" nil t nil script
                              "--session-root" directory
                              "--apply" "--confirm-emacs-stopped")))
              (should (search-backward
                       "both legacy and canonical attachment lanes"
                       nil t)))
            (should
             (equal hashes
                    (mapcar (lambda (file) (secure-hash 'sha256 file))
                            all-files)))))
      (delete-directory directory t))))

(ert-deftest e-chat-session-test-non-string-uri-fails-loudly ()
  "An unrecognized sequence-valued URI remains a visible validation error."
  (e-chat-session-test--with-sqlite-harness
      (harness _store (e-backend-fake-create :items nil))
    (e-chat-session-test--create-session harness "invalid-uri")
    (should-error
     (e-chat-session-attach-context
      harness "invalid-uri"
      '(:uri ("file://old.org" "unknown" "value"))
      :current-attachments nil)
     :type 'user-error)))

(ert-deftest e-chat-session-test-attachments-are-current-state-context ()
  "Canvas attachments are rebuilt from current live buffer state per context."
  (e-chat-session-test--with-sqlite-harness
      (harness store (e-backend-fake-create :items nil))
    (e-harness-activate-capability harness (e-chat-session-capability-create))
    (e-chat-session-test--create-session harness "session-1")
    (with-temp-buffer
      (rename-buffer "e-chat-session-canvas" t)
      (insert "first canvas state")
      (e-chat-session-attach-context
       harness
       "session-1"
       (list :uri (concat "buffer://" (buffer-name))
             :label "canvas"
             :buffer-name (buffer-name))
       :canvas t :current-attachments nil)
      (should
       (e-chat-session-test--wait-until
        (lambda ()
          (zerop (e-session-async-pending-count store "session-1")))))
      (let* ((context
              (e-chat-session-test--await
               (e-chat-session-context harness "session-1")))
             (content (plist-get (car (plist-get context :messages))
                                 :content)))
        (should (string-match-p "<canvas" content))
        (should (string-match-p "evidence=\"src:[0-9A-F]\\{16\\}\""
                                content))
        (should (string-match-p "first canvas state" content))
        ;; The canvas guidance must steer writes to the attachment uri and
        ;; warn off look-alike helper buffers, so the model does not write to
        ;; the wrong buffer.
        (should (string-match-p "Always write to the exact uri" content))
        (should (string-match-p "e-org-canvas-input" content)))
      (let* ((context
              (e-chat-session-test--await
               (e-chat-session-context harness "session-1")))
             (source
              (car
               (cl-loop
                for segment in (plist-get context :segments)
                append (plist-get segment e-context-evidence-sources-key)))))
        (should (equal (plist-get source :source-kind)
                       'current-state-attachment))
        (should (equal (plist-get source :provider) 'chat-session))
        (should (equal (plist-get source :uri)
                       (concat "buffer://" (buffer-name)))))
      (erase-buffer)
      (insert "second canvas state")
      (let* ((context
              (e-chat-session-test--await
               (e-chat-session-context harness "session-1")))
             (content (plist-get (car (plist-get context :messages))
                                 :content)))
        (should-not (string-match-p "first canvas state" content))
        (should (string-match-p "second canvas state" content))))))

(ert-deftest e-chat-session-test-file-attachment-prefers-live-buffer ()
  "File attachments read unsaved live buffers before disk contents."
  (let ((file (make-temp-file "e-chat-session-canvas-" nil ".txt")))
    (unwind-protect
        (e-chat-session-test--with-sqlite-harness
            (harness store (e-backend-fake-create :items nil))
          (write-region "disk state" nil file nil 'silent)
          (e-harness-activate-capability harness (e-chat-session-capability-create))
          (e-chat-session-test--create-session harness "session-1")
          (let ((buffer (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buffer
                  (erase-buffer)
                  (insert "unsaved live state")
                  (e-chat-session-attach-context
                   harness
                   "session-1"
                   (list :uri (concat "file://" file)
                         :label "canvas.txt"
                         :buffer-name (buffer-name))
                   :canvas t :current-attachments nil)
                  (should
                   (e-chat-session-test--wait-until
                    (lambda ()
                      (zerop
                       (e-session-async-pending-count store "session-1")))))
                  (let* ((context
                          (e-chat-session-test--await
                           (e-chat-session-context harness "session-1")))
                         (content
                          (plist-get (car (plist-get context :messages))
                                     :content)))
                    (should (string-match-p "unsaved live state" content))
                    (should-not (string-match-p "disk state" content))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (delete-file file))))

(provide 'e-chat-session-test)

;;; e-chat-session-test.el ends here

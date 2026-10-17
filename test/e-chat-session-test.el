;;; e-chat-session-test.el --- Tests for chat session capability -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for chat-session semantic actions without presentation rendering.

;;; Code:

(require 'ert)
(require 'e-context-reference)
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

(ert-deftest e-chat-session-test-queue-validates-and-retains-durable-custody ()
  "Queued input remains durable and hidden until its owning turn consumes it."
  (let (requests finish)
    (e-chat-session-test--with-sqlite-harness
        (harness store
                 (e-backend-create
                  :name "held"
                  :start
                  (cl-function
                   (lambda (&key messages on-item on-done on-request-start
                                  &allow-other-keys)
                     (push (copy-tree messages t) requests)
                     (setq finish
                           (lambda ()
                             (funcall on-item
                                      '(:type assistant-message :content "finished"))
                             (funcall on-item '(:type done :reason stop))
                             (funcall on-done '(:status done))))
                     (let ((request (e-backend-request-create)))
                       (funcall on-request-start request)
                       request)))))
      (e-chat-session-test--create-session harness "session-1")
      (e-chat-session-test--await
       (e-chat-session-submit harness "session-1" "running"))
      (should (e-chat-session-test--wait-until
               (lambda () (= (length requests) 1))))
      (should-error (e-chat-session-queue harness "session-1" "") :type 'user-error)
      (let ((admission
             (e-chat-session-queue
              harness "session-1" "queued"
              :references '((:uri "buffer://source"))
              :metadata '(:source chat-composer))))
        (should (e-work-handle-p admission))
        (e-chat-session-test--await admission)
        (let* ((binding (e-chat-service-binding harness "session-1"))
               (board (e-chat-session-test--await
                       (e-board-sqlite-service-board-get-start
                        (e-chat-service-binding-sqlite-service binding)
                        (e-chat-service-binding-board-id binding))))
               (pickups
                (e-chat-session-test--await
                 (e-board-sqlite-service-pickup-page-start
                  (e-chat-service-binding-sqlite-service binding)
                  (e-chat-service-binding-board-id binding)
                  (plist-get board :generation)
                  (e-chat-service-binding-participant-id binding) 16)))
               (item (seq-find (lambda (pickup)
                                 (equal (plist-get pickup :content) "queued"))
                               pickups))
               (active (e-chat-service-active-turn harness "session-1"))
               (context
                (e-chat-session-test--await
                 (e-harness-turn-context-start harness "session-1"
                                               (plist-get active :id)))))
          (should item)
          (should (eq (plist-get item :state) 'ready))
          (should-not (e-harness-queued-prompts harness "session-1"))
          (should-not (seq-find (lambda (message)
                                 (equal (plist-get message :content) "queued"))
                               (plist-get context :messages))))
        (funcall finish)
        (should (e-chat-session-test--wait-until
                 (lambda () (= (length requests) 2))))
        (should (= (cl-count "queued" (car requests)
                             :key (lambda (message) (plist-get message :content))
                             :test #'equal)
                   1))
        (let* ((message (seq-find (lambda (item)
                                   (equal (plist-get item :content) "queued"))
                                 (car requests)))
               (metadata (plist-get message :metadata)))
          (should (equal (plist-get metadata :references)
                         '((:uri "buffer://source"))))
          (should (equal (plist-get metadata :source) 'chat-composer)))
        (let ((page (e-chat-session-test--await
                     (e-session-async-visible-message-page store "session-1"))))
          (should (= (cl-count "queued" (plist-get page :messages)
                               :key (lambda (message) (plist-get message :content))
                               :test #'equal)
                     1)))))))

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
   (dolist (message
            '((:role user :content "Inspect the canvas.")))
     (e-chat-session-test--await
      (e-chat-service-append-seed-message harness "session-1" message)))
   (let (initial-prefix initial-content request-prefix)
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
              (messages (plist-get context :messages))
              (instructions
               (plist-get (cl-find-if
                           (lambda (message)
                             (eq (plist-get message :role) 'system))
                           messages)
                          :content))
              (canvas
               (cl-find-if #'e-context-canvas-reference-message-p messages))
              (content (plist-get canvas :content)))
         (setq request-prefix
               (seq-remove #'e-context-canvas-reference-message-p messages))
         (setq initial-prefix request-prefix
               initial-content content)
         (should (string-match-p "<canvas" content))
         (should (string-match-p "evidence=\"src:[0-9A-F]\\{16\\}\""
                                 content))
         (should (eq (car (last (e-context-move-canvas-reference-to-end
                                 messages)))
                     canvas))
         (should
          (equal
           (mapcar (lambda (message)
                     (list (plist-get message :role)
                           (plist-get message :content)))
                   (seq-remove (lambda (message)
                                 (or (eq (plist-get message :role) 'system)
                                     (e-context-canvas-reference-message-p
                                      message)))
                               messages))
           '((user "Inspect the canvas."))))
         (should (string-match-p "first canvas state" content))
         (should-not (string-match-p "first canvas state" instructions))
         (should (equal
                  (plist-get (plist-get context :options)
                             :canvas-reference-layout-revision)
                  e-context-canvas-reference-layout-revision))
         (should (equal
                  (cl-loop for segment in (plist-get context :segments)
                           append (copy-tree (plist-get segment :messages)))
                  messages))
         ;; The canvas guidance must steer writes to the attachment uri and
         ;; warn off look-alike helper buffers, so the model does not write to
         ;; the wrong buffer.
         (should (string-match-p "Always write to the exact uri" instructions))
         (should (string-match-p "e-org-canvas-input" instructions)))
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
       (dolist (message
                '((:role tool-call
                   :content (:id "call-1" :name "read" :arguments nil))
                  (:role tool
                   :content (:tool-call-id "call-1" :name "read"
                             :status ok :content "Read the current heading."))
                  (:role user :content "Update the checkbox after reading.")))
         (e-chat-session-test--await
          (e-chat-service-append-seed-message harness "session-1" message)))
       (let* ((context
               (e-chat-session-test--await
                (e-chat-session-context harness "session-1")))
              (messages (plist-get context :messages))
              (canvas
               (cl-find-if #'e-context-canvas-reference-message-p messages)))
         (setq request-prefix
               (seq-remove #'e-context-canvas-reference-message-p messages))
         (should (equal initial-prefix
                        (cl-subseq request-prefix 0 (length initial-prefix))))
         (should (equal (plist-get canvas :content) initial-content))
         (should
          (equal
           (mapcar (lambda (message)
                     (list (plist-get message :role)
                           (plist-get message :content)))
                   (seq-remove (lambda (message)
                                 (or (eq (plist-get message :role) 'system)
                                     (e-context-canvas-reference-message-p
                                      message)))
                               messages))
           '((user "Inspect the canvas.")
             (tool-call (:id "call-1" :name "read" :arguments nil))
             (tool (:tool-call-id "call-1" :name "read" :status ok
                                  :content "Read the current heading."))
             (user "Update the checkbox after reading."))))
         (should (equal (cl-loop for segment in (plist-get context :segments)
                                 append (copy-tree
                                         (plist-get segment :messages)))
                        messages)))
       (let* ((context
               (e-chat-session-test--await
                (e-chat-session-context harness "session-1")))
              (messages (plist-get context :messages))
              (canvas
               (cl-find-if #'e-context-canvas-reference-message-p messages)))
         (should (equal (plist-get canvas :content) initial-content))
         (should (equal request-prefix
                        (seq-remove #'e-context-canvas-reference-message-p
                                    messages))))
       (erase-buffer)
       (insert "first canvas state with checkbox checked")
       (let* ((context
               (e-chat-session-test--await
                (e-chat-session-context harness "session-1")))
              (messages (plist-get context :messages))
              (content
               (plist-get
                (cl-find-if #'e-context-canvas-reference-message-p messages)
                :content)))
         (should (equal request-prefix
                        (seq-remove #'e-context-canvas-reference-message-p
                                    messages)))
         (should
          (string-suffix-p
           "\nfirst canvas state with checkbox checked\n</canvas>"
           content)))
       (erase-buffer)
       (insert "second canvas state")
       (let* ((context
               (e-chat-session-test--await
                (e-chat-session-context harness "session-1")))
              (messages (plist-get context :messages))
              (content
               (plist-get
                (cl-find-if #'e-context-canvas-reference-message-p messages)
                :content)))
         (should (equal request-prefix
                        (seq-remove #'e-context-canvas-reference-message-p
                                    messages)))
         (should (string-match-p "second canvas state" content)))))))

(ert-deftest e-chat-session-test-noncanvas-attachment-remains-system-context ()
  "Ordinary attachments retain their system-context semantics."
  (let ((attachment '(:id "attachment-1"
                      :uri "file:///report.md"
                      :label "report.md"
                      :canvas nil)))
    (cl-letf (((symbol-function 'e-chat-session-attachments)
               (lambda (_harness _session-id) (list attachment)))
              ((symbol-function 'e-chat-session--attachment-content)
               (lambda (_attachment) "ordinary attachment body")))
      (let* ((messages
              (e-chat-session-context-attachments-provider
               :harness 'harness :session-id "session-1"))
             (system (car messages)))
        (should (= (length messages) 1))
        (should (eq (plist-get system :role) 'system))
        (should (string-match-p "<attachment "
                                (plist-get system :content)))
        (should (string-match-p "ordinary attachment body"
                                (plist-get system :content)))
        (should-not (e-context-canvas-reference-message-p system))
        (should-not (e-context-canvas-reference-layout-for-messages
                     messages))))))

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
                          (plist-get
                           (cl-find-if
                            #'e-context-canvas-reference-message-p
                            (plist-get context :messages))
                           :content)))
                    (should (string-match-p "unsaved live state" content))
                    (should-not (string-match-p "disk state" content))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      (delete-file file))))

(ert-deftest e-chat-session-test-canvas-release-fences-pending-chronology ()
  "Detach and replacement release text, fence old responses, and reset snapshots."
  (let* ((session-id "canvas-release-session")
         (harness
          (e-harness-create
           :backend (e-backend-create :name "canvas-release")
           :sessions (e-session-store-create)
           :intrinsic-capabilities
           (list (e-chat-session-capability-create))))
         (buffer (generate-new-buffer "e-chat-session-release-canvas"))
         (uri (concat "buffer://" (buffer-name buffer)))
         (key (list session-id uri))
         (attachments nil))
    (unwind-protect
        (let ((e-context-lifetime-shadow-projection-enabled t))
          (e-harness-create-session harness :id session-id)
          (with-current-buffer buffer
            (insert "initial"))
          (cl-letf (((symbol-function 'e-chat-session-attachments)
                     (lambda (_harness _session-id) attachments)))
            (setq attachments
                  (list (e-chat-session-attach-context
                         harness session-id
                         (list :uri uri :buffer-name (buffer-name buffer))
                         :canvas t :current-attachments nil)))
            (let* ((before-detach
                    (e-harness-turn-context harness session-id "before-detach"))
                   (pending-before-detach
                    (plist-get (plist-get before-detach :options)
                               :canvas-pending-observations)))
              (should (= (length pending-before-detach) 1))
              (setq attachments
                    (e-chat-session-detach-context harness session-id uri))
              (should-not (gethash key (e-harness-canvas-chronologies harness)))
              (e-harness-context-runtime-commit-canvas-response
               harness session-id
               (list :canvas-pending-observations pending-before-detach))
              (should-not (gethash key (e-harness-canvas-chronologies harness))))
            (setq attachments
                  (list (e-chat-session-attach-context
                         harness session-id
                         (list :uri uri :buffer-name (buffer-name buffer))
                         :canvas t)))
            (let* ((baseline
                    (e-harness-turn-context harness session-id "new-baseline"))
                   (pending-baseline
                    (plist-get (plist-get baseline :options)
                               :canvas-pending-observations))
                   (operation
                    (car (plist-get (plist-get (car pending-baseline) :state)
                                    :operations)))
                   (message (plist-get operation :message)))
              (should (= (length pending-baseline) 1))
              (should (eq (plist-get operation :representation) 'snapshot))
              (should message)
              (should (string-match-p
                       (e-context-canvas-revision "initial")
                       (plist-get message :content)))
              (e-harness-context-runtime-commit-canvas-response
               harness session-id
               (list :canvas-pending-observations pending-baseline))
              (should (equal (plist-get (gethash key
                                                   (e-harness-canvas-chronologies
                                                    harness))
                                        :text)
                             "initial"))
              (with-current-buffer buffer
                (erase-buffer)
                (insert "changed"))
              (let* ((before-replacement
                      (e-harness-turn-context harness session-id
                                              "before-replacement"))
                     (pending-before-replacement
                      (plist-get (plist-get before-replacement :options)
                                 :canvas-pending-observations)))
                (should (= (length pending-before-replacement) 1))
                (setq attachments
                      (list (e-chat-session-attach-context
                             harness session-id
                             (list :uri uri :buffer-name (buffer-name buffer)
                                   :label "replacement")
                             :canvas t)))
                (should-not
                 (gethash key (e-harness-canvas-chronologies harness)))
                (e-harness-context-runtime-commit-canvas-response
                 harness session-id
                 (list :canvas-pending-observations
                       pending-before-replacement))
                (should-not
                 (gethash key (e-harness-canvas-chronologies harness)))
                (let* ((replacement
                        (e-harness-turn-context harness session-id
                                                "replacement-baseline"))
                       (replacement-pending
                        (plist-get (plist-get replacement :options)
                                   :canvas-pending-observations))
                       (replacement-operation
                        (car (plist-get
                              (plist-get (car replacement-pending) :state)
                              :operations)))
                       (replacement-message
                        (plist-get replacement-operation :message)))
                  (should (= (length replacement-pending) 1))
                  (should (eq (plist-get replacement-operation :representation)
                              'snapshot))
                  (should replacement-message)
                  (should (string-match-p
                           (e-context-canvas-revision "changed")
                           (plist-get replacement-message :content)))
                  (e-harness-context-runtime-commit-canvas-response
                   harness session-id
                   (list :canvas-pending-observations replacement-pending))
                  (let ((token
                         (e-harness-context-runtime--canvas-lifecycle-token
                          harness session-id uri)))
                    (with-current-buffer buffer
                      (erase-buffer)
                      (insert "preview only"))
                    (let ((preview
                           (e-harness-context harness session-id nil 'preview)))
                      (should-not
                       (plist-get (plist-get preview :options)
                                  :canvas-pending-observations)))
                    (should
                     (equal token
                            (e-harness-context-runtime--canvas-lifecycle-token
                             harness session-id uri)))
                    (should (equal (plist-get
                                    (gethash key
                                             (e-harness-canvas-chronologies
                                              harness))
                                    :text)
                                   "changed"))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-chat-session-test-canvas-demotion-releases-accepted-chronology ()
  "Demotion by URI or ID releases exact text and fences earlier responses."
  (dolist (match '(uri id))
    (let* ((session-id (format "canvas-demotion-%s" match))
           (harness
            (e-harness-create
             :backend (e-backend-create :name "canvas-demotion")
             :sessions (e-session-store-create)
             :intrinsic-capabilities
             (list (e-chat-session-capability-create))))
           (buffer (generate-new-buffer "e-canvas-demotion"))
           (uri (concat "buffer://" (buffer-name buffer)))
           (key (list session-id uri))
           attachments)
      (unwind-protect
          (let ((e-context-lifetime-shadow-projection-enabled t))
            (e-harness-create-session harness :id session-id)
            (with-current-buffer buffer (insert "accepted canvas"))
            (cl-letf (((symbol-function 'e-chat-session-attachments)
                       (lambda (_harness _session-id) attachments)))
              (cl-labels
                  ((pending (label)
                     (plist-get
                      (plist-get
                       (e-harness-turn-context harness session-id label)
                       :options)
                      :canvas-pending-observations))
                   (accept (observations)
                     (e-harness-context-runtime-commit-canvas-response
                      harness session-id
                      (list :canvas-pending-observations observations))))
                (setq attachments
                      (list (e-chat-session-attach-context
                             harness session-id
                             (list :id "canvas-id" :uri uri
                                   :buffer-name (buffer-name buffer))
                             :canvas t :current-attachments nil)))
                (let ((initial (pending "initial")))
                  (should (= (length initial) 1))
                  (accept initial)
                  (with-current-buffer buffer (insert " changed"))
                  (let* ((before-demotion (pending "before-demotion"))
                         (replacement
                          (if (eq match 'uri)
                              (list :id "ordinary-id" :uri uri :canvas nil)
                            (list :id "canvas-id"
                                  :uri "buffer://replacement-attachment"
                                  :canvas nil))))
                    (should (= (length before-demotion) 1))
                    (cl-letf (((symbol-function 'e-session-set-context-references)
                               (lambda (&rest _arguments)
                                 (error "Rejected metadata update"))))
                      (should-error
                       (e-chat-session-attach-context
                        harness session-id replacement :canvas nil)))
                    (should (equal
                             (plist-get
                              (gethash key (e-harness-canvas-chronologies harness))
                              :text)
                             "accepted canvas"))
                    (setq attachments
                          (list (e-chat-session-attach-context
                                 harness session-id replacement :canvas nil)))
                    (should-not (plist-get (car attachments) :canvas))
                    (should-not
                     (gethash key (e-harness-canvas-chronologies harness)))
                    (dolist (stale (list initial before-demotion))
                      (accept stale)
                      (should-not
                       (gethash key (e-harness-canvas-chronologies harness))))
                    (setq attachments
                          (e-chat-session-detach-context
                           harness session-id (plist-get (car attachments) :id)))
                    (should-not attachments)
                    (accept initial)
                    (should-not
                     (gethash key (e-harness-canvas-chronologies harness)))
                    (setq attachments
                          (list (e-chat-session-attach-context
                                 harness session-id
                                 (list :uri uri :buffer-name (buffer-name buffer))
                                 :canvas t)))
                    (let* ((fresh (pending "reattached"))
                           (operation
                            (car (plist-get (plist-get (car fresh) :state)
                                            :operations))))
                      (should (= (length fresh) 1))
                      (should (eq (plist-get operation :representation)
                                  'snapshot))
                      (accept fresh)
                      (should (equal
                               (plist-get
                                (gethash key
                                         (e-harness-canvas-chronologies harness))
                                :text)
                               "accepted canvas changed"))))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest e-chat-session-test-session-delete-releases-canvas-chronology ()
  "A successful harness session deletion drops accepted text and fences replies."
  (e-chat-session-test--with-sqlite-harness
      (harness store (e-backend-create :name "delete-canvas"))
    (let* ((session-id "deleted-canvas-session")
           (uri "buffer://deleted-canvas")
           (key (list session-id uri)))
      (e-chat-session-test--create-session harness session-id)
      (let* ((table (e-harness-canvas-chronologies harness))
             (state
              (list :version 3 :text "PRIVATE-CANVAS-BODY"
                    :revision (e-context-canvas-revision
                               "PRIVATE-CANVAS-BODY")
                    :operations '((:message (:content "PRIVATE-CANVAS-BODY")))))
             (pending-token
              (e-harness-context-runtime--canvas-lifecycle-token
               harness session-id uri))
             (pending
              (list :key key :base-version nil
                    :lifecycle-token pending-token
                    :state (list :text "stale response body"
                                 :revision "stale"
                                 :operations '((:message
                                                (:content
                                                 "stale response body")))))))
        (puthash key state table)
        (e-chat-session-test--await
         (e-harness-delete-session harness session-id))
        (should-not (gethash key table))
        (e-harness-context-runtime-commit-canvas-response
         harness session-id
         (list :canvas-pending-observations (list pending)))
        (should-not (gethash key table))
        (should-not
         (gethash (list session-id uri)
                  (e-harness-canvas-chronologies harness)))))))

(provide 'e-chat-session-test)

;;; e-chat-session-test.el ends here

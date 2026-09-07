;;; e-runtime-store-session-worker-test.el --- v6 session worker contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'sqlite)
(require 'e-runtime-store)
(require 'e-runtime-store-worker)
(require 'e-runtime-store-session-worker)
(require 'e-session-query)
(require 'e-session-storage-sqlite)

(defun e-runtime-store-session-worker-test--state
    (session-id &optional updated-at journal-position)
  "Return a complete bounded query row for SESSION-ID."
  (list :session-id session-id :name (concat "name-" session-id)
        :summary nil :metadata (list :name (concat "meta-" session-id))
        :created-at "2026-09-06T00:00:00Z"
        :updated-at (or updated-at "2026-09-06T00:00:00Z")
        :last-message-at nil :latest-assistant-marker nil :message-count 0
        :current-branch nil :turn-options (list :temperature 0)
        :current-head-id (concat session-id "-root")
        :root-event-id (concat session-id "-root")
        :current-context-generation-id nil
        :board-id nil :principal nil :association-role nil
        :routing-policy nil :root-p t :board-output-sequence 0
        :board-activity-sequence 0
        :journal-position (or journal-position 1)))

(cl-defmacro e-runtime-store-session-worker-test--with-runtime
    ((runtime directory) &rest body)
  "Run BODY with a disposable v6 runtime STORE."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((,directory (make-temp-file "e-runtime-store-session-worker-" t))
          (,runtime (e-runtime-store-open ,directory)))
     (unwind-protect
         (progn
           (e-runtime-store-call ,runtime 'read '(:op store-metrics))
           ,@body)
       (ignore-errors (e-runtime-store-close ,runtime))
       (when (file-directory-p ,directory)
         (delete-directory ,directory t)))))

(defun e-runtime-store-session-worker-test--append
    (runtime session-id position &optional updated-at)
  "Append a root record with deterministic state to RUNTIME."
  (let ((state (e-runtime-store-session-worker-test--state
                session-id updated-at)))
    (plist-put state :journal-position position)
    (e-runtime-store-call
     runtime 'write
     (list :op 'session-append :session-id session-id
           :record (list :type "session" :session-id session-id
                         :id (concat session-id "-root")
                         :timestamp (or updated-at
                                        "2026-09-06T00:00:00Z"))
           :query-delta state))))

(ert-deftest e-runtime-store-session-worker-v6-schema-is-relational-and-narrow ()
  "Fresh v6 storage has query/history relations but no opaque projections."
  (e-runtime-store-session-worker-test--with-runtime (runtime directory)
    (let ((database (sqlite-open (expand-file-name "store.sqlite3" directory))))
      (unwind-protect
          (progn
            (should (= (plist-get (e-runtime-store-metrics runtime)
                                  :schema-version)
                       6))
            (should (car (sqlite-select database
                                        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_query_state'")))
            (should (car (sqlite-select database
                                        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_records'")))
            (should-not (car (sqlite-select database
                                           "SELECT 1 FROM sqlite_master WHERE type='table' AND name='catalog_projection'")))
            (should-not (car (sqlite-select database
                                           "SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_checkpoints'")))
            (let ((columns
                   (mapcar (lambda (row) (if (vectorp row) (aref row 1) (nth 1 row)))
                           (sqlite-select database "PRAGMA table_info(session_query_state)"))))
              (should (equal columns
                             '("session_id" "name" "summary" "metadata"
                               "created_at" "updated_at" "last_message_at"
                               "latest_assistant_marker" "message_count"
                               "current_branch" "turn_options" "current_head_id"
                               "root_event_id" "current_context_generation_id"
                               "board_id" "principal"
                               "association_role" "routing_policy" "root_p"
                               "board_output_sequence" "board_activity_sequence"
                               "journal_position")))))
            (let ((record-columns
                   (mapcar (lambda (row)
                             (if (vectorp row) (aref row 1) (nth 1 row)))
                           (sqlite-select database
                                          "PRAGMA table_info(session_records)"))))
              (should (member "record_identity" record-columns)))
        (sqlite-close database)))))

(ert-deftest e-runtime-store-session-worker-append-read-and-detach ()
  "Append commits one journal row/current row and reads detached values."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let* ((result (e-runtime-store-session-worker-test--append
                    runtime "one" 1))
           (state (e-runtime-store-call
                   runtime 'read '(:op session-query-state :session-id "one")))
           (metadata (e-runtime-store-call
                      runtime 'read '(:op session-metadata :session-id "one")))
           (page (e-runtime-store-call
                  runtime 'read '(:op session-record-page :session-id "one"))))
      (should (= (plist-get result :position) 1))
      (should-not (plist-member result :delta))
      (should (equal (plist-get state :session-id) "one"))
      (should (equal (plist-get metadata :name) "name-one"))
      (should (= (length (plist-get page :records)) 1))
      (let* ((record (car (plist-get page :records)))
             (value (plist-get record :value)))
        (should (equal (plist-get record :record-type) "session"))
        (should (equal (plist-get value :id) "one-root"))
        (should-not (eq value (plist-get state :metadata))))
      ;; Values returned by a read are detached from the next read result.
      (plist-put (plist-get state :metadata) :changed t)
      (should-not (plist-get
                   (plist-get
                    (e-runtime-store-call
                     runtime 'read '(:op session-query-state :session-id "one"))
                    :metadata)
                   :changed)))))

(ert-deftest e-runtime-store-session-worker-board-association-and-cursors ()
  "Board association and newest/root page use typed filters and stable cursors."
  (e-runtime-store-session-worker-test--with-runtime (runtime directory)
    (let ((first (e-runtime-store-session-worker-test--state
                  "first" "2026-09-06T00:00:01Z"))
          (second (e-runtime-store-session-worker-test--state
                   "second" "2026-09-06T00:00:02Z")))
      (dolist (state (list first second))
        (plist-put state :board-id "board")
        (plist-put state :principal "alice")
        (plist-put state :association-role "member")
        (e-runtime-store-call
         runtime 'write
         (list :op 'session-append :session-id (plist-get state :session-id)
               :record (list :type "session" :session-id
                             (plist-get state :session-id)
                             :id (plist-get state :root-event-id)
                             :timestamp (plist-get state :updated-at))
               :query-delta state)))
      (let* ((page (e-runtime-store-call
                    runtime 'read
                    '(:op session-query-page :limit 1 :root-p t
                      :board-id "board" :principal "alice")))
             (row (car (plist-get page :rows)))
             (next (plist-get page :next))
             (rest (e-runtime-store-call
                    runtime 'read
                    (list :op 'session-query-page :limit 1 :root-p t
                          :board-id "board" :principal "alice"
                          :cursor next)))
             (identity-page (e-runtime-store-call
                             runtime 'read
                             '(:op session-id-page :limit 1)))
             (association (e-runtime-store-call
                           runtime 'read
                           '(:op session-board-association
                             :session-id "first"))))
        (should (= (length (plist-get page :rows)) 1))
        (should (equal (plist-get row :session-id) "second"))
        (should (equal (plist-get (car (plist-get rest :rows)) :session-id)
                       "first"))
        (should (equal (plist-get identity-page :ids) '("first")))
        (should (equal (plist-get identity-page :next) "first"))
        (should (= (plist-get identity-page :byte-count) 5))
        (should (equal (plist-get association :board-id) "board"))
        (should (equal (plist-get association :principal) "alice"))
        (should-not (plist-member page :high-water))
        (should-not (plist-member identity-page :high-water))
        ;; Association reads select only their five physical columns.  A
        ;; damaged, unrelated metadata/options payload therefore cannot make
        ;; the Board owner lookup fail.
        (let ((database (sqlite-open
                         (expand-file-name "store.sqlite3" directory))))
          (unwind-protect
              (sqlite-execute
               database
               "UPDATE session_query_state SET metadata='not-base64',turn_options='not-base64' WHERE session_id='first'")
            (sqlite-close database)))
        (let ((association-again
               (e-runtime-store-call
                runtime 'read
                '(:op session-board-association :session-id "first"))))
          (should (equal (plist-get association-again :board-id) "board"))
          (should (equal (plist-get association-again :principal) "alice")))))))

(ert-deftest e-runtime-store-session-worker-record-page-has-typed-predicates-and-bound ()
  "Forward history pages honor identity predicates and their row cap."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "records")))
      (dotimes (index 3)
        (let ((record-id (format "record-%d" index)))
          (setq state (copy-tree state))
          (plist-put state :journal-position (1+ index))
          (plist-put state :updated-at (format "2026-09-06T00:00:0%dZ" index))
          (when (= index 2) (plist-put state :message-count 1))
          (e-runtime-store-call
           runtime 'write
           (list :op 'session-append :session-id "records"
                 :record (list :type (if (= index 0) "session" "message")
                               :session-id "records" :id record-id
                               :parent-id (unless (= index 0) "record-0")
                               :timestamp (plist-get state :updated-at)
                               :message (when (> index 0)
                                          (list :role 'user :content "x")))
                 :query-delta state))))
      (let ((page (e-runtime-store-call
                   runtime 'read
                   '(:op session-record-page :session-id "records"
                     :record-type "message" :parent-id "record-0" :limit 8))))
        (should (= (length (plist-get page :records)) 2))
        (should (equal (plist-get (car (plist-get page :records)) :record-id)
                       "record-1")))
      (should-error
       (e-runtime-store-call runtime 'read
                             '(:op session-record-page :session-id "records"
                               :limit 257))
       :type 'e-runtime-store-worker-error))))

(ert-deftest e-runtime-store-session-worker-context-path-is-selected-and-compacted ()
  "Provider context follows one parent path and returns only its compacted suffix."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "context")))
      (plist-put state :metadata '(:project-root "/tmp/project"))
      (plist-put state :current-head-id "root")
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "context"
             :record '(:type "session" :session-id "context" :id "root"
                       :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (cl-labels
          ((append-record (position id parent type &rest fields)
             (setq state (copy-tree state))
             (plist-put state :journal-position position)
             (plist-put state :current-head-id id)
             (plist-put state :updated-at
                        (format "2026-09-06T00:00:0%dZ" position))
             (e-runtime-store-call
              runtime 'write
              (list :op 'session-append :session-id "context"
                    :record
                    (append (list :type type :session-id "context" :id id
                                  :parent-id parent
                                  :timestamp (plist-get state :updated-at))
                            fields)
                    :query-delta state))))
        (append-record 2 "old" "root" "message"
                       :message '(:id "old" :role user :content "old"))
        (append-record 3 "kept" "old" "message"
                       :message '(:id "kept" :role user :content "kept"))
        (append-record 4 "compact" "kept" "compaction"
                       :summary "summary" :first-kept-entry-id "kept")
        (append-record 5 "answer" "compact" "message"
                       :message '(:id "answer" :role assistant
                                  :content "answer"))
        ;; A later physical row on another branch must not enter the selected
        ;; path even though its journal position is newer.
        (append-record 6 "other" "old" "message"
                       :message '(:id "other" :role assistant
                                  :content "other"))
        (plist-put state :current-head-id "answer")
        (plist-put state :journal-position 7)
        (e-runtime-store-call
         runtime 'write
         (list :op 'session-append :session-id "context"
               :record '(:type "session-info" :session-id "context"
                         :id "head-select" :parent-id "answer"
                         :timestamp "2026-09-06T00:00:07Z"
                         :field metadata :value (:selected t))
               :query-delta state)))
      (let ((path (e-runtime-store-call
                   runtime 'read
                   '(:op session-context-path :session-id "context"))))
        (should (equal (plist-get (plist-get path :compaction) :summary)
                       "summary"))
        (should (equal (mapcar (lambda (message)
                                (plist-get message :content))
                              (plist-get path :messages))
                       '("kept" "answer")))
        (should (equal (plist-get (plist-get path :metadata) :project-root)
                       "/tmp/project"))
        (should-not (member "other"
                            (mapcar (lambda (message)
                                      (plist-get message :content))
                                    (plist-get path :messages))))))))

(ert-deftest e-runtime-store-session-worker-context-path-bounds-tool-receipts ()
  "Provider context returns a projected receipt tail, not receipt history."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "receipts"))
          (parent "root"))
      (plist-put state :current-head-id parent)
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "receipts"
             :record '(:type "session" :session-id "receipts" :id "root"
                       :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (dotimes (index 10)
        (let ((id (format "activity-%d" index))
              (tool-call-id (format "tool-%d" index)))
          (setq state (copy-tree state))
          (plist-put state :journal-position (+ index 2))
          (plist-put state :current-head-id id)
          (plist-put state :updated-at
                     (format "2026-09-06T00:00:%02dZ" (1+ index)))
          (e-runtime-store-call
           runtime 'write
           (list :op 'session-append :session-id "receipts"
                 :record
                 (list :type "activity-event" :session-id "receipts"
                       :id id :parent-id parent
                       :timestamp (plist-get state :updated-at)
                       :event-type 'tool-finished
                       :payload
                       (list :receipt
                             (list :tool-call-id tool-call-id
                                   :tool "read"
                                   :status 'finished
                                   :stated-purpose (format "purpose-%d" index)
                                   :purpose-status 'satisfied
                                   :details-uri (format "tmp://detail-%d" index)
                                   :unrelated-large-copy "must-not-cross")))
                 :query-delta state))
          (setq parent id)))
      (let* ((path
              (e-runtime-store-call
               runtime 'read
               '(:op session-context-path :session-id "receipts")))
             (receipts (plist-get path :tool-receipts)))
        (should (= (plist-get path :tool-receipt-total-count) 10))
        (should (= (length receipts) 8))
        (should (equal (mapcar (lambda (receipt)
                                (plist-get receipt :tool-call-id))
                              receipts)
                       '("tool-2" "tool-3" "tool-4" "tool-5"
                         "tool-6" "tool-7" "tool-8" "tool-9")))
        (should-not (plist-member (car receipts) :unrelated-large-copy))
        (should (<= (plist-get path :tool-receipt-byte-count)
                    (plist-get path :tool-receipt-byte-limit)))))))

(ert-deftest e-runtime-store-session-worker-visible-message-window-is-newest-bounded ()
  "The chat-shaped read filters message rows before decoding a bounded window."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "visible")))
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "visible"
             :record '(:type "session" :session-id "visible" :id "root"
                       :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (dotimes (index 3)
        (setq state (copy-tree state))
        (plist-put state :journal-position (+ index 2))
        (plist-put state :updated-at
                   (format "2026-09-06T00:00:0%dZ" (+ index 1)))
        (plist-put state :last-message-at (plist-get state :updated-at))
        (plist-put state :message-count (+ index 1))
        (e-runtime-store-call
         runtime 'write
         (list :op 'session-append :session-id "visible"
               :record (list :type "message" :session-id "visible"
                             :id (format "m%d" index)
                             :timestamp (plist-get state :updated-at)
                             :message (list :id (format "m%d" index)
                                            :role (if (evenp index)
                                                      'user 'assistant)
                                            :content (format "message-%d" index)))
               :query-delta state)))
      ;; Non-message rows do not enter this consumer window.
      (setq state (copy-tree state))
      (plist-put state :journal-position 5)
      (plist-put state :updated-at "2026-09-06T00:00:04Z")
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "visible"
             :record '(:type "activity-event" :session-id "visible"
                       :id "activity" :timestamp "2026-09-06T00:00:04Z"
                       :event-type progress :payload (:text "unrelated"))
             :query-delta state))
      (let* ((page (e-runtime-store-call
                    runtime 'read
                    '(:op session-visible-message-page
                      :session-id "visible" :limit 2)))
             (messages (plist-get page :messages)))
        (should (equal (mapcar (lambda (message) (plist-get message :id))
                               messages)
                       '("m1" "m2")))
        (should (equal (mapcar (lambda (message) (plist-get message :content))
                               messages)
                       '("message-1" "message-2")))
        (should (plist-get page :truncated))
        (should (= (length messages) 2))
        (should-not (plist-member page :records))))))

(ert-deftest e-runtime-store-session-worker-content-uses-consumer-byte-bounds ()
  "Message content is bounded by its query, not the scalar-row ABI."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let* ((session-id "large-content")
           (root-id "large-content-root")
           (message-id "large-content-message")
           (content (concat (make-string 9000 ?x) "-tool-result"))
           (state (e-runtime-store-session-worker-test--state session-id)))
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id session-id
             :record (list :type "session" :session-id session-id
                           :id root-id :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (setq state (copy-tree state))
      (plist-put state :journal-position 2)
      (plist-put state :updated-at "2026-09-06T00:00:01Z")
      (plist-put state :last-message-at "2026-09-06T00:00:01Z")
      (plist-put state :message-count 1)
      (plist-put state :current-head-id message-id)
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id session-id
             :record (list :type "message" :session-id session-id
                           :id message-id :parent-id root-id
                           :timestamp "2026-09-06T00:00:01Z"
                           :message (list :id message-id :role 'tool-result
                                          :content content))
             :query-delta state))
      (let* ((visible
              (e-runtime-store-call
               runtime 'read
               (list :op 'session-visible-message-page
                     :session-id session-id :limit 1)))
             (context
              (e-runtime-store-call
               runtime 'read
               (list :op 'session-context-path :session-id session-id))))
        (should (equal (plist-get (car (plist-get visible :messages)) :content)
                       content))
        (should (equal (plist-get (car (plist-get context :messages)) :content)
                       content))))))

(ert-deftest e-runtime-store-session-worker-page-rejects-unusable-sort-timestamp ()
  "A corrupted current row cannot produce a cursor with a nil sort field."
  (e-runtime-store-session-worker-test--with-runtime (runtime directory)
    (e-runtime-store-session-worker-test--append runtime "timestamp" 1)
    (let ((database (sqlite-open
                     (expand-file-name "store.sqlite3" directory))))
      (unwind-protect
          (sqlite-execute database
                          "UPDATE session_query_state SET updated_at=NULL WHERE session_id='timestamp'")
        (sqlite-close database)))
    (should-error
     (e-runtime-store-call runtime 'read
                           '(:op session-query-page :root-p t))
     :type 'e-runtime-store-worker-error)))

(ert-deftest e-runtime-store-session-worker-batch-delete-and-noop-control ()
  "Batch rows are transactional; exact controls delete without fake no-op data."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "batch")))
      (plist-put state :journal-position 2)
      (let ((result
             (e-runtime-store-call
              runtime 'write
              (list :op 'session-append-batch :session-id "batch"
                    :records (vector
                              (list :type "session" :session-id "batch" :id "root"
                                    :timestamp "2026-09-06T00:00:00Z")
                              (list :type "message" :session-id "batch" :id "m"
                                    :parent-id "root" :timestamp "2026-09-06T00:00:01Z"))
                    :query-delta state))))
        (should-not (plist-member result :delta))
        (should (= (plist-get result :last-position) 2)))
      (should (= (plist-get
                  (e-runtime-store-call
                   runtime 'read '(:op session-header :session-id "batch"))
                  :record-count)
                 2))
      (should-error
       (e-runtime-store-call
        runtime 'write
        (list :op 'session-append :session-id "batch"
              :record '(:type "message" :session-id "batch" :id "bad")
              :query-delta '(:session-id "batch" :noop t)))
       :type 'e-runtime-store-worker-error)
      (e-runtime-store-call runtime 'write
                            '(:op session-delete :session-id "batch"))
      (should-not (e-runtime-store-call
                   runtime 'read '(:op session-query-state :session-id "batch")))
      (should (= (plist-get
                  (e-runtime-store-call
                   runtime 'read '(:op session-header :session-id "batch"))
                 :record-count)
                 0)))))

(ert-deftest e-runtime-store-session-worker-position-fence-rolls-back-single-and-batch ()
  "A stale or ahead query position rejects the whole append transaction."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "fence")))
      (plist-put state :journal-position 0)
      (should-error
       (e-runtime-store-session-worker-test--append runtime "fence" 0)
       :type 'e-runtime-store-worker-error)
      ;; The helper's fixed position is one; replace it with the deliberately
      ;; stale delta for the actual write.
      (should-error
       (e-runtime-store-call
        runtime 'write
        (list :op 'session-append :session-id "fence"
              :record '(:type "session" :session-id "fence" :id "root"
                        :timestamp "2026-09-06T00:00:00Z")
              :query-delta state))
       :type 'e-runtime-store-worker-error)
      (plist-put state :journal-position 2)
      (should-error
       (e-runtime-store-call
        runtime 'write
        (list :op 'session-append :session-id "fence"
              :record '(:type "session" :session-id "fence" :id "root"
                        :timestamp "2026-09-06T00:00:00Z")
              :query-delta state))
       :type 'e-runtime-store-worker-error)
      (should (= (plist-get
                  (e-runtime-store-call
                   runtime 'read '(:op session-header :session-id "fence"))
                  :record-count)
                 0)))
    (let ((state (e-runtime-store-session-worker-test--state "batch-fence")))
      (dolist (position '(1 3))
        (plist-put state :journal-position position)
        (should-error
         (e-runtime-store-call
          runtime 'write
          (list :op 'session-append-batch :session-id "batch-fence"
                :records [(:type "session" :session-id "batch-fence" :id "root"
                           :timestamp "2026-09-06T00:00:00Z")
                          (:type "message" :session-id "batch-fence" :id "m"
                           :parent-id "root"
                           :timestamp "2026-09-06T00:00:01Z")]
                :query-delta state))
         :type 'e-runtime-store-worker-error))
      (should (= (plist-get
                  (e-runtime-store-call
                   runtime 'read '(:op session-header :session-id "batch-fence"))
                  :record-count)
                 0)))))

(ert-deftest e-runtime-store-session-worker-record-identity-and-null-filters ()
  "Structural delta identities are unique while semantic target ids repeat."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "identity")))
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "identity"
             :record (list :type "session" :session-id "identity"
                           :id "root" :delta-id "root-delta"
                           :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (dotimes (index 2)
        (setq state (copy-tree state))
        (plist-put state :journal-position (+ 2 index))
        (plist-put state :updated-at (format "2026-09-06T00:00:0%dZ"
                                             (+ 1 index)))
        (e-runtime-store-call
         runtime 'write
         (list :op 'session-append :session-id "identity"
               :record (list :type "message-display" :session-id "identity"
                             :id "target" :delta-id (format "display-%d" index)
                             :timestamp (format "2026-09-06T00:00:0%dZ"
                                                 (+ 1 index)))
               :query-delta state)))
      (let ((displays
             (e-runtime-store-call
              runtime 'read
              '(:op session-record-page :session-id "identity"
                :record-type "message-display" :record-id "target")))
            (first-display
             (e-runtime-store-call
              runtime 'read
              '(:op session-record-page :session-id "identity"
                :record-identity "display-0")))
            (root
             (e-runtime-store-call
              runtime 'read
              '(:op session-record-page :session-id "identity"
                :record-type "session" :parent-id nil))))
        (should (= (length (plist-get displays :records)) 2))
        (should (= (length (plist-get first-display :records)) 1))
        (should (= (length (plist-get root :records)) 1)))
      ;; A semantic target id is searchable but not an identity claim: a
      ;; different record family may use the same target id.
      (setq state (copy-tree state))
      (plist-put state :journal-position 4)
      (plist-put state :updated-at "2026-09-06T00:00:03Z")
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "identity"
             :record (list :type "activity-event" :session-id "identity"
                           :id "target" :delta-id "activity-target"
                           :timestamp "2026-09-06T00:00:03Z")
             :query-delta state))
      (let ((duplicate-state (copy-tree state)))
        (plist-put duplicate-state :journal-position 5)
        (should-error
         (e-runtime-store-call
          runtime 'write
          (list :op 'session-append :session-id "identity"
                :record (list :type "other-type" :session-id "identity"
                              :id "different-target" :delta-id "display-1"
                              :timestamp "2026-09-06T00:00:04Z")
                :query-delta duplicate-state))
         :type 'e-runtime-store-worker-error))
      (should (= (plist-get
                  (e-runtime-store-call
                   runtime 'read '(:op session-header :session-id "identity"))
                  :record-count)
                 4)))))

(ert-deftest e-runtime-store-session-worker-receipt-replay-and-rollback ()
  "Receipt replay is idempotent and failed session writes roll back fully.

This exercises the generic worker transaction around the session module
directly, keeping the proof independent of the scheduler's generated request
IDs while still using the production receipt implementation."
  (let ((directory (make-temp-file "e-runtime-store-session-receipt-" t))
        (body nil)
        (request nil))
    (unwind-protect
        (progn
          (e-runtime-store-worker--open directory "receipt-runtime")
          (setq body
                (list :op 'session-append :session-id "receipt"
                      :record (list :type "session" :session-id "receipt"
                                    :id "receipt-root"
                                    :delta-id "receipt-delta"
                                    :timestamp "2026-09-06T00:00:00Z")
                      :query-delta
                      (let ((state
                             (e-runtime-store-session-worker-test--state
                              "receipt")))
                        ;; Make a full query row materially larger than the
                        ;; receipt facts; the receipt must not retain it.
                        (plist-put state :metadata
                                   (list :large (make-string 3000 ?x)))
                        state)))
          (setq request (list :id "receipt-request" :kind 'write :body body
                              :write-prefix 1 :ack-prefix 0))
          (let ((first (e-runtime-store-worker--write request))
                (second (e-runtime-store-worker--write request)))
            (should (equal first second))
            (should (= (car (car (sqlite-select
                                  e-runtime-store-worker--database
                                  "SELECT COUNT(*) FROM session_records WHERE session_id='receipt'")))
                       1))
            (should (= (car (car (sqlite-select
                                  e-runtime-store-worker--database
                                  "SELECT COUNT(*) FROM runtime_store_receipts WHERE runtime_id='receipt-runtime'")))
                       1))
            (should (= (car (car (sqlite-select
                                  e-runtime-store-worker--database
                                  "SELECT COUNT(*) FROM session_query_state WHERE session_id='receipt'")))
                       1)))
          (let* ((row (car (sqlite-select
                            e-runtime-store-worker--database
                            "SELECT result,LENGTH(result) FROM runtime_store_receipts WHERE runtime_id='receipt-runtime'")))
                 (stored (e-runtime-store-worker--value
                          (e-runtime-store-worker--column row 0))))
            (should-not (plist-member stored :delta))
            (should-not (plist-member stored :metadata))
            (should (< (e-runtime-store-worker--column row 1) 1024)))
          (should-error
           (e-runtime-store-worker--write
            (list :id "receipt-collision" :kind 'write
                  :body (plist-put (copy-sequence body) :record
                                   (list :type "session" :session-id "receipt"
                                         :id "receipt-root"
                                         :timestamp "2026-09-06T00:00:01Z"))
                  :write-prefix 2 :ack-prefix 0)))
          (should-error
           (e-runtime-store-worker--write
            (list :id "receipt-invalid" :kind 'write
                  :body (plist-put (copy-sequence body) :query-delta
                                   (list :session-id "other" :noop t))
                  :write-prefix 3 :ack-prefix 0)))
          (should (= (car (car (sqlite-select
                                e-runtime-store-worker--database
                                "SELECT COUNT(*) FROM session_records WHERE session_id='receipt'")))
                     1))
          (should (equal
                   (e-runtime-store-worker--column
                    (car (sqlite-select
                          e-runtime-store-worker--database
                          "SELECT name FROM session_query_state WHERE session_id='receipt'"))
                    0)
                   "name-receipt")))
      (ignore-errors (e-runtime-store-worker--close))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(ert-deftest e-runtime-store-session-worker-adapter-requires-complete-delta ()
  "The SQLite adapter forwards the domain row and rejects legacy omission."
  (let* ((directory (make-temp-file "e-runtime-store-session-adapter-" t))
         (runtime (e-runtime-store-open directory))
         (store (make-symbol "session-sqlite-owner"))
         (record '(:type "session" :session-id "adapter" :id "root"
                   :timestamp "2026-09-06T00:00:00Z"))
         (state (e-runtime-store-session-worker-test--state "adapter")))
    (unwind-protect
        (progn
          (e-runtime-store-call runtime 'read '(:op store-metrics))
          (e-session-storage-sqlite-register store 'sqlite runtime nil)
          (should-error
           (e-session-storage-sqlite-append store "adapter" record)
           :type 'e-runtime-store-worker-error)
          (e-session-storage-sqlite-append-with-query-delta
           store "adapter" record state)
          (let ((page (e-session-storage-sqlite-read-page
                       store "adapter" 0 4)))
            (should (= (length (plist-get page :records)) 1))
            (should (equal (plist-get
                            (plist-get (car (plist-get page :records)) :value)
                            :id)
                           "root"))))
      (e-session-storage-sqlite-register store nil nil nil)
      (ignore-errors (e-runtime-store-close runtime))
      (when (file-directory-p directory)
        (delete-directory directory t)))))

(provide 'e-runtime-store-session-worker-test)

;;; e-runtime-store-session-worker-test.el ends here

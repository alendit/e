;;; e-runtime-store-session-worker-test.el --- v8 session worker contracts -*- lexical-binding: t; -*-

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
  "Run BODY with a disposable v8 runtime STORE."
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

(defun e-runtime-store-session-worker-test--child-admission-body
    (session-id &optional pickup)
  "Return one composite child admission body for SESSION-ID and PICKUP."
  (let ((state (e-runtime-store-session-worker-test--state session-id)))
    (setq state (plist-put state :board-id "admission-board"))
    (setq state (plist-put state :principal "owner"))
    (setq state (plist-put state :association-role "participant"))
    (setq state
          (plist-put state :routing-policy
                     '(:participant-id "child-participant"
                       :pickup-selector (:tags (subagent))
                       :observer-selector :self
                       :default-tags (subagent)
                       :default-to :self)))
    (append
     (list :op 'session-board-participant-admit
           :session-id session-id
           :records
           (vector (list :type "session" :session-id session-id
                         :id (concat session-id "-root")
                         :timestamp "2026-09-08T00:00:00Z"))
           :query-delta state
           :board-id "admission-board" :generation 1
           :participant
           '(:id "child-participant" :author "e-chat"
             :principal "owner" :controller "owner" :role participant
             :state active :subscription-id "child-address"
             :publication-pending nil))
     (when pickup (list :pickup pickup)))))

(ert-deftest e-runtime-store-session-worker-child-admission-is-one-transaction ()
  "Session, participant, and selected pickup either all commit or none do."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (e-runtime-store-call
     runtime 'write
     '(:op board-create :board-id "admission-board"
       :trusted-principal "owner" :root (:kind test)))
    ;; A session/query mismatch fails at the first component.  No participant
    ;; can escape a transaction that never admitted the child identity.
    (let ((invalid-session
           (e-runtime-store-session-worker-test--child-admission-body
            "child-invalid-session")))
      (setf (plist-get (plist-get invalid-session :query-delta) :session-id)
            "different-session")
      (should-error
       (e-runtime-store-call runtime 'write invalid-session)))
    (should-not
     (plist-get
      (e-runtime-store-call
       runtime 'read
       '(:op session-header :session-id "child-invalid-session"))
      :present))
    (should-not
     (e-runtime-store-call
      runtime 'read
      '(:op board-participant-list :board-id "admission-board"
        :generation 1)))
    (e-runtime-store-call
     runtime 'write
     '(:op board-routing-put :board-id "admission-board" :generation 1
       :message-id "message-1" :outcome (:state routed)
       :pickups [(:delivery-id "delivery-1"
                  :participant-id "child-participant"
                  :message-id "message-1" :content "work")]))
    (e-runtime-store-call
     runtime 'write
     '(:op board-pickup-transition :board-id "admission-board"
       :generation 1 :delivery-id "delivery-1" :transition claim))
    ;; The selected pickup is real and claimed.  An invalid lane fails at the
    ;; last component, proving SQLite rolls the earlier session and participant
    ;; writes back without changing the selected claim.
    (should-error
     (e-runtime-store-call
      runtime 'write
      (e-runtime-store-session-worker-test--child-admission-body
       "child-failed"
       '(:delivery-id "delivery-1"
         :record (:type board-input-admission :id "admission-1")
         :lane "invalid"))))
    (should-not
     (e-runtime-store-call
      runtime 'read '(:op session-query-state :session-id "child-failed")))
    (should-not
     (e-runtime-store-call
      runtime 'read
      '(:op board-participant-list :board-id "admission-board"
        :generation 1)))
    (should
     (eq (plist-get
          (car (e-runtime-store-call
                runtime 'read
                '(:op board-pickup-list :board-id "admission-board"
                  :generation 1)))
          :state)
         'claimed))
    (let ((result
           (e-runtime-store-call
            runtime 'write
            (e-runtime-store-session-worker-test--child-admission-body
             "child-success"
             '(:delivery-id "delivery-1"
               :record (:type board-input-admission :id "admission-1")
               :lane idle)))))
      (should (= (plist-get result :board-revision) 5))
      (should (plist-get result :pickup)))
    (should
     (equal
      (plist-get
       (e-runtime-store-call
        runtime 'read
        '(:op session-board-association :session-id "child-success"))
       :association-role)
      "participant"))
    (let ((participants
           (e-runtime-store-call
            runtime 'read
            '(:op board-participant-list :board-id "admission-board"
              :generation 1)))
          (pickups
           (e-runtime-store-call
            runtime 'read
            '(:op board-pickup-list :board-id "admission-board"
              :generation 1))))
      (should (= (length participants) 1))
      (should (eq (plist-get (car participants) :role) 'participant))
      (should (eq (plist-get (car pickups) :state) 'accepted)))
    ;; The participant insert is the middle cut.  A duplicate association
    ;; rejects after the new session rows were written, and those rows must be
    ;; absent after the enclosing transaction rolls back.
    (should-error
     (e-runtime-store-call
      runtime 'write
      (e-runtime-store-session-worker-test--child-admission-body
       "child-participant-conflict")))
    (should-not
     (plist-get
      (e-runtime-store-call
       runtime 'read
       '(:op session-header :session-id "child-participant-conflict"))
      :present))
    (should (= (length
                (e-runtime-store-call
                 runtime 'read
                 '(:op board-participant-list :board-id "admission-board"
                   :generation 1)))
               1))))

(ert-deftest e-runtime-store-session-worker-v8-schema-is-relational-and-narrow ()
  "Fresh v8 storage has query/history relations but no opaque mirrors."
  (e-runtime-store-session-worker-test--with-runtime (runtime directory)
    (let ((database (sqlite-open (expand-file-name "store.sqlite3" directory))))
      (unwind-protect
          (progn
            (should (= (plist-get (e-runtime-store-metrics runtime)
                                  :schema-version)
                                  8))
            (should (car (sqlite-select database
                                        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_query_state'")))
            (should (car (sqlite-select database
                                        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_records'")))
            (should (car (sqlite-select database
                                        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='session_process_report_index'")))
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
                               "root_p"
                               "board_output_sequence" "board_activity_sequence"
                               "journal_position")))))
            (let ((record-columns
                   (mapcar (lambda (row)
                             (if (vectorp row) (aref row 1) (nth 1 row)))
                           (sqlite-select database
                                          "PRAGMA table_info(session_records)"))))
              (should (member "record_identity" record-columns)))
            (let ((association-columns
                   (mapcar (lambda (row)
                             (if (vectorp row) (aref row 1) (nth 1 row)))
                           (sqlite-select
                            database
                            "PRAGMA table_info(board_session_associations)"))))
              (should
               (equal association-columns
                      '("session_id" "board_id" "generation"
                        "participant_id" "routing_policy" "revision"))))
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
    (e-runtime-store-call
     runtime 'write
     '(:op board-create :board-id "board" :trusted-principal "alice"
       :root (:kind test)))
    (let ((first (e-runtime-store-session-worker-test--state
                  "first" "2026-09-06T00:00:01Z"))
          (second (e-runtime-store-session-worker-test--state
                   "second" "2026-09-06T00:00:02Z")))
      (dolist (state (list first second))
        (e-runtime-store-call
         runtime 'write
         (list :op 'session-append :session-id (plist-get state :session-id)
               :record (list :type "session" :session-id
                             (plist-get state :session-id)
                             :id (plist-get state :root-event-id)
                             :timestamp (plist-get state :updated-at))
               :query-delta state))
        (let ((participant-id
               (concat "participant-" (plist-get state :session-id))))
          (e-runtime-store-call
           runtime 'write
           (list :op 'board-participant-put :board-id "board"
                 :generation 1
                 :participant
                 (list :id participant-id :principal "alice"
                       :author "test" :controller "test"
                       :role 'member :state 'active :name participant-id
                       :subscription-id participant-id
                       :publication-pending nil)))
          (e-runtime-store-call
           runtime 'write
           (list :op 'board-session-association-put :board-id "board"
                 :generation 1 :session-id (plist-get state :session-id)
                 :participant-id participant-id
                 :association-role 'member
                 :routing-policy
                 (list :participant-id participant-id
                       :pickup-selector '(:tags (main))
                       :observer-selector :self
                       :default-tags '(main)
                       :default-to :self)))))
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
  "Ordered history pages honor identity predicates and their row cap."
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
      (let* ((newest
              (e-runtime-store-call
               runtime 'read
               '(:op session-record-page :session-id "records"
                 :order newest :record-ids ["record-1" "record-2"] :limit 1)))
             (older
              (e-runtime-store-call
               runtime 'read
               (list :op 'session-record-page :session-id "records"
                     :order 'newest :before (plist-get newest :next)
                     :record-ids ["record-1" "record-2"] :limit 1))))
        (should (equal (mapcar (lambda (row) (plist-get row :record-id))
                               (plist-get newest :records))
                       '("record-2")))
        (should (equal (mapcar (lambda (row) (plist-get row :record-id))
                               (plist-get older :records))
                       '("record-1"))))
      (should-error
       (e-runtime-store-call runtime 'read
                             '(:op session-record-page :session-id "records"
                               :limit 257))
       :type 'e-runtime-store-worker-error))))

(ert-deftest e-runtime-store-session-worker-process-report-projection-queries-before-limit ()
  "All process-report consumers select semantic rows before their bounds."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "reports"))
          (position 1))
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "reports"
             :record '(:type "session" :session-id "reports" :id "root"
                       :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (cl-labels
          ((append-report
            (report)
            (setq position (1+ position)
                  state (copy-tree state))
            (plist-put state :journal-position position)
            (plist-put state :updated-at
                       (format "2026-09-06T00:%02d:%02dZ"
                               (/ position 60) (% position 60)))
            (e-runtime-store-call
             runtime 'write
             (list :op 'session-append :session-id "reports"
                   :record
                   (list :type "process-report" :session-id "reports"
                         :id (or (plist-get report :id)
                                 (format "report-%d" position))
                         :delta-id (format "delta-%d" position)
                         :timestamp (plist-get state :updated-at)
                         :report report)
                   :query-delta state))))
        (append-report
         '(:report-type "marker" :id "marker-a" :marker-id "marker-a"
           :evidence-id "evidence-a" :created-at "2026-09-06T00:00:01Z"))
        ;; Historical v6 triage identity/parent shape: only semantic marker-id
        ;; establishes the association projected by v7.
        (append-report
         '(:report-type "triage" :id "legacy-triage"
           :parent-id "transcript-head" :marker-id "marker-a"
           :status "routed" :outcome "actionable"
           :created-at "2026-09-06T00:00:02Z"))
        (append-report
         '(:report-type "extraction" :id "extract"
           :marker-ids ["marker-b" "marker-a" "marker-a"]
           :created-at "2026-09-06T00:00:03Z"))
        (append-report
         '(:report-type "request-shape" :id "shape-old"
           :provider-request-id "request-a" :shape (:actual-shape (:bytes 1))))
        ;; More than one physical page of newer open markers must not hide the
        ;; older closed marker from a status-filtered list.  Status selection
        ;; belongs in SQL before LIMIT, not in the Emacs consumer.
        (dotimes (index 300)
          (append-report
           (list :report-type "marker"
                 :id (format "open-marker-%d" index)
                 :marker-id (format "open-marker-%d" index)
                 :evidence-id (format "open-evidence-%d" index)
                 :created-at (plist-get state :updated-at))))
        (append-report
         '(:report-type "request-shape" :id "shape-new"
           :provider-request-id "request-a" :shape (:actual-shape (:bytes 2))))
        (let* ((markers
                (e-runtime-store-call
                 runtime 'read
                 '(:op session-process-report-marker-page :session-id "reports"
                   :limit 1)))
               (marker
                (e-runtime-store-call
                 runtime 'read
                 '(:op session-process-report-marker :session-id "reports"
                   :marker-id "marker-a")))
               (triage
                (e-runtime-store-call
                 runtime 'read
                 '(:op session-process-report-triage-page
                   :session-id "reports" :marker-id "marker-a" :limit 1)))
               (extraction
                (e-runtime-store-call
                 runtime 'read
                 '(:op session-process-report-extraction-page
                   :session-id "reports" :marker-id "marker-a" :limit 1)))
               (shapes
                (e-runtime-store-call
                 runtime 'read
                 '(:op session-process-report-request-shapes
                   :session-id "reports" :provider-request-ids ["request-a"])))
               (count
                (e-runtime-store-call
                 runtime 'read
                 '(:op session-process-report-marker-count
                   :session-id "reports"))))
          (should (equal (plist-get
                          (plist-get (car (plist-get markers :markers)) :marker)
                          :marker-id)
                         "open-marker-299"))
          (should-not (plist-get (car (plist-get markers :markers))
                                 :latest-triage))
          (should (equal (plist-get (plist-get marker :marker) :id)
                         "marker-a"))
          (should (equal (plist-get (car (plist-get triage :triage)) :id)
                         "legacy-triage"))
          (should (equal (plist-get
                          (car (plist-get extraction :extractions)) :id)
                         "extract"))
          (should (equal (plist-get
                          (car (plist-get shapes :request-shapes)) :id)
                         "shape-new"))
          (should (= (plist-get count :marker-count) 301))
          (let ((closed
                 (e-runtime-store-call
                  runtime 'read
                  '(:op session-process-report-marker-page
                    :session-id "reports" :status "routed" :limit 1))))
            (should (= (length (plist-get closed :markers)) 1))
            (should
             (equal
              (plist-get
               (plist-get (car (plist-get closed :markers)) :marker)
               :marker-id)
              "marker-a"))
            (should
             (equal
              (plist-get
               (plist-get (car (plist-get closed :markers)) :latest-triage)
               :id)
              "legacy-triage"))))))))

(ert-deftest e-runtime-store-session-worker-process-report-projection-is-atomic ()
  "An invalid derived projection rolls back its canonical journal row."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "atomic-report")))
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "atomic-report"
             :record '(:type "session" :session-id "atomic-report" :id "root"
                       :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (setq state (copy-tree state))
      (plist-put state :journal-position 2)
      (should-error
       (e-runtime-store-call
        runtime 'write
        (list :op 'session-append :session-id "atomic-report"
              :record
              (list :type "process-report" :session-id "atomic-report"
                    :id "too-many" :delta-id "too-many"
                    :timestamp "2026-09-06T00:00:01Z"
                    :report
                    (list :report-type "extraction"
                          :marker-ids
                          (vconcat (mapcar (lambda (index)
                                            (format "marker-%d" index))
                                          (number-sequence 0 64)))))
              :query-delta state)))
      (should (= (plist-get
                  (e-runtime-store-call
                   runtime 'read
                   '(:op session-header :session-id "atomic-report"))
                  :record-count)
                 1)))))

(ert-deftest e-runtime-store-session-worker-process-report-pages-have-byte-cursors ()
  "Semantic report pages stop at the detached byte bound with a usable cursor."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "report-bytes")))
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "report-bytes"
             :record '(:type "session" :session-id "report-bytes" :id "root"
                       :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (dotimes (index 2)
        (setq state (copy-tree state))
        (plist-put state :journal-position (+ index 2))
        (plist-put state :updated-at
                   (format "2026-09-06T00:00:0%dZ" (1+ index)))
        (e-runtime-store-call
         runtime 'write
         (list :op 'session-append :session-id "report-bytes"
               :record
               (list :type "process-report" :session-id "report-bytes"
                     :id (format "marker-%d" index)
                     :delta-id (format "marker-delta-%d" index)
                     :timestamp (plist-get state :updated-at)
                     :report
                     (list :report-type "marker"
                           :id (format "marker-%d" index)
                           :marker-id (format "marker-%d" index)
                           :note (make-string (* 600 1024) (+ ?a index))))
               :query-delta state)))
      (let* ((newest
              (e-runtime-store-call
               runtime 'read
               '(:op session-process-report-marker-page
                 :session-id "report-bytes" :limit 2)))
             (older
              (e-runtime-store-call
               runtime 'read
               (list :op 'session-process-report-marker-page
                     :session-id "report-bytes" :limit 2
                     :before (plist-get newest :next)))))
        (should (= (length (plist-get newest :markers)) 1))
        (should (plist-get newest :truncated))
        (should (integerp (plist-get newest :next)))
        (should (<= (plist-get newest :byte-count)
                    (plist-get newest :byte-limit)))
        (should (= (length (plist-get older :markers)) 1))
        (should-not (plist-get older :truncated))))))

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
      (setq state (copy-tree state))
      (plist-put state :journal-position 6)
      (plist-put state :updated-at "2026-09-06T00:00:05Z")
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "visible"
             :record '(:type "message-display" :session-id "visible"
                       :id "m2" :delta-id "hide-m2"
                       :timestamp "2026-09-06T00:00:05Z"
                       :display "hidden")
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
        (should (equal (plist-get (cadr messages) :display) "hidden"))
        (should (plist-get page :truncated))
        (should (= (length messages) 2))
        (should-not (plist-member page :records))))))

(ert-deftest e-runtime-store-session-worker-context-path-applies-latest-display ()
  "Provider context applies the latest disposition without replay or N+1 reads."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "display")))
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "display"
             :record '(:type "session" :session-id "display" :id "display-root"
                       :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (setq state (copy-tree state))
      (plist-put state :journal-position 2)
      (plist-put state :updated-at "2026-09-06T00:00:01Z")
      (plist-put state :last-message-at "2026-09-06T00:00:01Z")
      (plist-put state :message-count 1)
      (plist-put state :current-head-id "message")
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "display"
             :record '(:type "message" :session-id "display" :id "message"
                       :parent-id "display-root"
                       :timestamp "2026-09-06T00:00:01Z"
                       :message (:id "message" :role assistant :content "reply"))
             :query-delta state))
      (setq state (copy-tree state))
      (plist-put state :journal-position 3)
      (plist-put state :updated-at "2026-09-06T00:00:02Z")
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "display"
             :record '(:type "message-display" :session-id "display"
                       :id "message" :delta-id "hide"
                       :timestamp "2026-09-06T00:00:02Z"
                       :display "hidden")
             :query-delta state))
      (should
       (equal
        (plist-get
         (car (plist-get
               (e-runtime-store-call
                runtime 'read
                '(:op session-context-path :session-id "display"))
               :messages))
         :display)
        "hidden"))
      (setq state (copy-tree state))
      (plist-put state :journal-position 4)
      (plist-put state :updated-at "2026-09-06T00:00:03Z")
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "display"
             :record '(:type "message-display" :session-id "display"
                       :id "message" :delta-id "show"
                       :timestamp "2026-09-06T00:00:03Z")
             :query-delta state))
      (should-not
       (plist-member
        (car (plist-get
              (e-runtime-store-call
               runtime 'read
               '(:op session-context-path :session-id "display"))
              :messages))
        :display)))))

(ert-deftest e-runtime-store-session-worker-failure-inspection-is-bounded-and-detached ()
  "Failure navigation queries only typed recent rows and one exact turn."
  (e-runtime-store-session-worker-test--with-runtime (runtime _directory)
    (let ((state (e-runtime-store-session-worker-test--state "inspect")))
      (plist-put state :name "Inspection session")
      (plist-put state :metadata '(:project-root "/tmp/project"))
      (e-runtime-store-call
       runtime 'write
       (list :op 'session-append :session-id "inspect"
             :record '(:type "session" :session-id "inspect" :id "root"
                       :timestamp "2026-09-06T00:00:00Z")
             :query-delta state))
      (cl-labels
          ((append-record (position record)
             (setq state (copy-tree state t))
             (plist-put state :journal-position position)
             (plist-put state :updated-at (plist-get record :timestamp))
             (plist-put state :current-head-id (plist-get record :id))
             (when (equal (plist-get record :type) "message")
               (plist-put state :message-count
                          (1+ (plist-get state :message-count)))
               (plist-put state :last-message-at (plist-get record :timestamp)))
             (e-runtime-store-call
              runtime 'write
              (list :op 'session-append :session-id "inspect"
                    :record record :query-delta state))))
        (append-record
         2 '(:type "message" :session-id "inspect" :id "message-1"
             :parent-id "root" :timestamp "2026-09-06T00:00:01Z"
             :message (:id "message-1" :role user :content "broken"
                       :turn-id "turn-1")))
        (append-record
         3 '(:type "activity-event" :session-id "inspect" :id "event-1"
             :parent-id "message-1" :timestamp "2026-09-06T00:00:02Z"
             :semantic-event
             (:id "event-1" :turn-id "turn-1"
              :event-type provider-request-started
              :created-at "2026-09-06T00:00:02Z")))
        (append-record
         4 '(:type "activity-event" :session-id "inspect" :id "event-2"
             :parent-id "event-1" :timestamp "2026-09-06T00:00:03Z"
             :semantic-event
             (:id "event-2" :turn-id "turn-1" :event-type turn-failed
              :created-at "2026-09-06T00:00:03Z"
              :payload (:error "provider failed" :details (:status 520)))))
        (append-record
         5 '(:type "message" :session-id "inspect" :id "message-other"
             :parent-id "event-2" :timestamp "2026-09-06T00:00:04Z"
             :message (:id "message-other" :role user :content "other"
                       :turn-id "turn-2"))))
      (let* ((page (e-runtime-store-call
                    runtime 'read
                    '(:op session-recent-failures :limit 1)))
             (failure (car (plist-get page :failures)))
             (detail (e-runtime-store-call
                      runtime 'read
                      '(:op session-turn-inspection
                        :session-id "inspect" :turn-id "turn-1"))))
        (should (= (length (plist-get page :failures)) 1))
        (should (equal (plist-get failure :session-id) "inspect"))
        (should (equal (plist-get failure :turn-id) "turn-1"))
        (should (equal (plist-get failure :error) "provider failed"))
        (should (equal (plist-get failure :details) '(:status 520)))
        (should (equal (plist-get failure :session-title)
                       "Inspection session"))
        (should (plist-get detail :present))
        (should (equal (plist-get (plist-get detail :session) :metadata)
                       '(:project-root "/tmp/project")))
        (should (equal (mapcar (lambda (message) (plist-get message :id))
                               (plist-get detail :messages))
                       '("message-1")))
        (should (equal (mapcar (lambda (event) (plist-get event :id))
                               (plist-get detail :events))
                       '("event-1" "event-2")))
        (setf (plist-get failure :error) "mutated")
        (should (equal
                 (plist-get
                  (car (plist-get
                        (e-runtime-store-call
                         runtime 'read
                         '(:op session-recent-failures :limit 1))
                        :failures))
                  :error)
                 "provider failed"))))))

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

;;; e-board-observation-test.el --- Board participant activity query tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'sqlite)
(require 'e-board-observation)
(require 'e-board-orchestration)
(require 'e-board-sqlite-worker)
(require 'e-runtime-store-codec)
(require 'e-runtime-store)
(require 'e-work)

(defun e-board-observation-test--await (work)
  "Await WORK at this explicit disposable-test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(defun e-board-observation-test--admit
    (service board-id session-id participant-id &optional metadata)
  "Admit one durable participant with optional child METADATA."
  (let* ((principal (format "chat:%s" session-id))
         (policy (list :participant-id participant-id
                       :pickup-selector '(:tags (subagent))
                       :observer-selector
                       (list :subject-participant-id participant-id)
                       :default-tags '(subagent) :default-to participant-id))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id
           :metadata (append (list :name session-id) metadata)
           :principal principal :board-id board-id
           :association-role "participant" :routing-policy policy))
         (records (copy-tree (plist-get session :admission-records) t))
         (position 0))
    (dolist (record records)
      (plist-put record :journal-position (cl-incf position)))
    (e-board-observation-test--await
     (e-board-sqlite-service-admit-participant-start
      service session-id board-id records (plist-get session :query-delta)
      (list :id participant-id :name session-id :author "observation-test"
            :principal principal :controller principal :role 'participant
            :state 'active :subscription-id (concat "sub-" participant-id)
            :publication-pending nil)))))

(defun e-board-observation-test--publish-lifecycle
    (target session-id status summary)
  "Publish one bounded lifecycle fact for SESSION-ID."
  (e-board-observation-test--await
   (e-board-sqlite-publication-target-fact-start
    target
    (format "Subagent %s is %s" session-id status)
    (list 'subagent-lifecycle session-id status)
    :tags (list 'change status)
    :attributes (list :session-id session-id :status status :type 'subagent
                      :summary summary))))

(defun e-board-observation-test--publish-unrelated-status
    (target session-id status summary)
  "Publish an unrelated same-session status fact for SESSION-ID."
  (e-board-observation-test--await
   (e-board-sqlite-publication-target-fact-start
    target
    (format "Unrelated %s is %s" session-id status)
    (list 'unrelated-status session-id status)
    :tags '(unrelated)
    :attributes (list :session-id session-id :status status :summary summary))))

(defun e-board-observation-test--report (target)
  "Publish one accepted run-bound terminal report for TARGET."
  (e-board-observation-test--await
   (e-board-sqlite-publication-target-orchestration-fact-start
    target
    '(:version 1 :type terminal-report :idempotency-key "terminal:run-1:task:0"
      :payload (:run-id "run-1" :task-key "task" :attempt 0 :status done
                :summary "accepted report" :outputs []
                :participant-session-id "03-worker")))))

(defun e-board-observation-test--insert-report
    (database board-id position run-id task-key attempt)
  "Insert one detached terminal report for adversarial history tests."
  (let* ((assignment (list :run-id run-id :task-key task-key :attempt attempt))
         (fact (list :version e-board-orchestration-fact-version
                     :type 'terminal-report
                     :idempotency-key
                     (format "terminal:%s:%s:%d" run-id task-key attempt)
                     :payload
                     (append assignment
                             (list :status 'done :summary "unrelated report"
                                   :outputs []
                                   :participant-session-id
                                   "other-session"))))
         (fields (e-board-orchestration-fact-record-fields fact))
         (record-id (format "unrelated-report-%d" position))
         (record
          (append
           (list :id record-id :board-id board-id :seq position
                 :record-kind 'fact :kind 'fact
                 :tags (plist-get fields :tags)
                 :attributes (plist-get fields :attributes)
                 :durable-position position)
           (cl-loop for (key value) on fields by #'cddr
                    unless (memq key '(:source-key :tags :attributes))
                    append (list key value))))
         (source-key (plist-get fields :source-key)))
    (sqlite-execute
     database
     "INSERT INTO board_records(board_id,generation,position,record_kind,record_id,source_kind,source_key,source_hash,payload) VALUES(?,?,?,?,?,?,?,?,?)"
     (vector board-id 1 position "fact" record-id "fact"
             (e-board-sqlite-worker--sql-value source-key) nil
             (e-board-sqlite-worker--sql-value record)))
    (dolist (tag (plist-get record :tags))
      (sqlite-execute
       database
       "INSERT INTO board_record_tags(board_id,generation,position,tag) VALUES(?,?,?,?)"
       (vector board-id 1 position
               (e-board-sqlite-worker--sql-value tag))))
    (let ((attributes (plist-get record :attributes)))
      (while attributes
        (sqlite-execute
         database
         "INSERT INTO board_record_attributes(board_id,generation,position,attribute_key,attribute_value) VALUES(?,?,?,?,?)"
         (vector board-id 1 position
                 (e-board-sqlite-worker--sql-value (car attributes))
                 (e-board-sqlite-worker--sql-value (cadr attributes))))
        (setq attributes (cddr attributes))))))

(cl-defmacro e-board-observation-test--with-fixture
    ((directory runtime service target board-id) &rest body)
  "Run BODY with one disposable SQL Board runtime and TARGET."
  (declare (indent 1))
  `(let* ((,directory (make-temp-file "e-board-observation-" t))
          (,board-id "observation-board")
          (,runtime (e-runtime-store-open ,directory))
          (,service (e-board-sqlite-service-create ,runtime))
          (,target (e-board-sqlite-publication-target-create
                    ,service ,board-id :author "observation-test")))
     (unwind-protect
         (progn
           (e-board-observation-test--await
            (e-board-sqlite-service-board-create-start
             ,service ,board-id "observation-test"))
           ,@body)
       (ignore-errors (e-runtime-store-close ,runtime))
       (delete-directory ,directory t))))

(defun e-board-observation-test--exact-page-bytes (page)
  "Return PAGE's exact encoded byte count including its :bytes field."
  (let ((candidate (plist-put (copy-tree page t) :bytes 0)))
    (catch 'settled
      (while t
        (let ((bytes (e-runtime-store-codec-measure-bounded
                      candidate e-board-sqlite-activity-page-byte-limit)))
          (if (= bytes (plist-get candidate :bytes))
              (throw 'settled bytes)
            (setq candidate (plist-put candidate :bytes bytes))))))))

(ert-deftest e-board-observation-test-mixed-page-is-one-ordered-consumer-model ()
  "Ordinary and run-bound participants share one ordered outcome page."
  (e-board-observation-test--with-fixture
      (directory runtime service target board-id)
    (ignore directory runtime)
    (e-board-observation-test--admit
     service board-id "01-owner-session" "01-owner")
    (e-board-observation-test--admit
     service board-id "02-child" "02-child")
    (e-board-observation-test--admit
     service board-id "03-worker" "03-worker"
     '(:board-run-id "run-1" :board-task-key "task" :board-attempt 0))
    (e-board-observation-test--publish-lifecycle
     target "02-child" 'running "started")
    (e-board-observation-test--publish-lifecycle
     target "02-child" 'done "ad-hoc complete")
    (e-board-observation-test--publish-lifecycle
     target "03-worker" 'running "worker started")
    (e-board-observation-test--report target)
    (let* ((page (e-board-observation-test--await
                  (e-board-observation-activity-page-start target :limit 8)))
           (participants (plist-get page :participants))
           (owner (nth 0 participants))
           (child (nth 1 participants))
           (worker (nth 2 participants)))
      (should (= (plist-get page :generation) 1))
      (should (equal (mapcar (lambda (row) (plist-get row :participant-id))
                             participants)
                     '("01-owner" "02-child" "03-worker")))
      (should-not (plist-get owner :outcome))
      (should (equal (plist-get child :session-id) "02-child"))
      (should (eq (plist-get (plist-get child :outcome) :source) 'lifecycle))
      (should (eq (plist-get (plist-get child :outcome) :status) 'done))
      (should (equal (plist-get (plist-get child :outcome) :summary)
                     "ad-hoc complete"))
      (should (equal (plist-get worker :run-id) "run-1"))
      (should (equal (plist-get worker :task-key) "task"))
      (should (= (plist-get worker :attempt) 0))
      (should (eq (plist-get (plist-get worker :outcome) :source)
                  'orchestration))
      (should (eq (plist-get (plist-get worker :outcome) :status) 'done))
      (should (equal (plist-get (plist-get worker :outcome) :summary)
                     "accepted report")))))

(ert-deftest e-board-observation-test-lifecycle-needs-runner-source-identity ()
  "An unrelated same-session status fact cannot replace lifecycle state."
  (e-board-observation-test--with-fixture
      (_directory _runtime service target board-id)
    (e-board-observation-test--admit
     service board-id "01-child" "01-child")
    (e-board-observation-test--publish-lifecycle
     target "01-child" 'done "real lifecycle")
    (e-board-observation-test--publish-unrelated-status
     target "01-child" 'failed "unrelated status")
    (let* ((page (e-board-observation-test--await
                  (e-board-observation-activity-page-start target :limit 4)))
           (outcome (plist-get (car (plist-get page :participants)) :outcome)))
      (should (eq (plist-get outcome :source) 'lifecycle))
      (should (eq (plist-get outcome :status) 'done))
      (should (equal (plist-get outcome :summary) "real lifecycle")))))

(ert-deftest e-board-observation-test-cursor-count-and-byte-bounds ()
  "The detached page is count-bounded, cursorable, and validates bounds."
  (e-board-observation-test--with-fixture
      (_directory _runtime service target board-id)
    (dotimes (index 3)
      (let ((id (format "%02d-participant" index)))
        (e-board-observation-test--admit
         service board-id id id)))
    (let* ((first (e-board-observation-test--await
                   (e-board-observation-activity-page-start target :limit 2)))
           (first-rows (plist-get first :participants))
           (next (plist-get first :next))
           (second (e-board-observation-test--await
                    (e-board-observation-activity-page-start
                     target :after next :limit 2))))
      (should (= (length first-rows) 2))
      (should (equal next "01-participant"))
      (should (equal (mapcar (lambda (row) (plist-get row :participant-id))
                             (plist-get second :participants))
                     '("02-participant")))
      (should-not (plist-get second :next))
      (should (integerp (plist-get first :bytes)))
      (should (<= (plist-get first :bytes)
                  e-board-sqlite-activity-page-byte-limit)))
    (should-error
     (e-board-observation-activity-page-start target :limit 0)
     :type 'e-board-sqlite-error)
    (should-error
     (e-board-observation-activity-page-start
      target :byte-limit (1+ e-board-sqlite-activity-page-byte-limit))
     :type 'e-board-sqlite-error)))

(ert-deftest e-board-observation-test-byte-bound-prefix-and-single-row-failure ()
  "Byte pagination keeps a non-empty prefix and fails an oversized first row."
  (e-board-observation-test--with-fixture
      (_directory _runtime service target board-id)
    (dotimes (index 3)
      (let ((id (format "%02d-participant" index)))
        (e-board-observation-test--admit service board-id id id)))
    (let* ((full (e-board-observation-test--await
                  (e-board-observation-activity-page-start target :limit 8)))
           (rows (plist-get full :participants))
           (first-id (plist-get (nth 0 rows) :participant-id))
           (second-id (plist-get (nth 1 rows) :participant-id))
           (prefix
            (list :board-id board-id
                  :generation (plist-get full :generation)
                  :revision (plist-get full :revision)
                  :after ""
                  :participants (cl-subseq rows 0 2)
                  :next second-id :cursor second-id))
           (prefix-bytes (e-board-observation-test--exact-page-bytes prefix))
           (one
            (list :board-id board-id
                  :generation (plist-get full :generation)
                  :revision (plist-get full :revision)
                  :after ""
                  :participants (list (car rows))
                  :next first-id
                  :cursor first-id))
           (one-bytes (e-board-observation-test--exact-page-bytes one)))
      (should (> one-bytes 1))
      (let ((work
             (e-board-observation-activity-page-start
              target :limit 1 :byte-limit (1- one-bytes))))
        (should-error (e-board-observation-test--await work)
                      :type 'e-runtime-store-worker-error))
      (let* ((page (e-board-observation-test--await
                    (e-board-observation-activity-page-start
                     target :limit 8 :byte-limit prefix-bytes)))
             (page-rows (plist-get page :participants)))
        (should (= (length page-rows) 2))
        (should (equal (plist-get page :next) second-id))
        (should (equal (plist-get page :cursor) second-id))
        (should (<= (plist-get page :bytes) prefix-bytes))
        (should (= (plist-get page :bytes)
                   (e-board-observation-test--exact-page-bytes
                    (plist-put (copy-tree page t) :bytes 0))))))))

(ert-deftest e-board-observation-test-relevant-fact-survives-unrelated-history ()
  "Relevant lifecycle selection is independent of unrelated Board history."
  (e-board-observation-test--with-fixture
      (_directory runtime service target board-id)
    (e-board-observation-test--admit
     service board-id "01-child" "01-child")
    (e-board-observation-test--publish-lifecycle
     target "01-child" 'done "behind unrelated history")
    (let ((database-file (e-runtime-store--database-file runtime))
          database)
      ;; Close the owner before adding adversarial durable rows through the
      ;; disposable fixture's SQLite file; observation below uses the worker
      ;; directly so no second runtime owner is created.
      (e-runtime-store-close runtime)
      (unwind-protect
          (progn
            (setq database (sqlite-open database-file))
            (sqlite-execute database "BEGIN")
            (dotimes (index (1+ e-board-sqlite-activity-fact-row-limit))
              (let* ((position (+ 2 index))
                     (record
                      (list :id (format "unrelated-%d" index)
                            :board-id board-id :seq position
                            :record-kind 'fact :kind 'fact
                            :tags '(unrelated-history)
                            :attributes
                            (list :session-id "01-child" :status 'failed
                                  :summary "unrelated status")
                            :durable-position position))
                     (source-key (list 'unrelated-history index)))
                (sqlite-execute
                 database
                 "INSERT INTO board_records(board_id,generation,position,record_kind,record_id,source_kind,source_key,source_hash,payload) VALUES(?,?,?,?,?,?,?,?,?)"
                 (vector board-id 1 position "fact"
                         (plist-get record :id) "fact"
                         (e-board-sqlite-worker--sql-value source-key) nil
                         (e-board-sqlite-worker--sql-value record)))))
            (sqlite-execute database "COMMIT")
            (let ((calls 0)
                  (original (symbol-function 'sqlite-select)))
              (cl-letf (((symbol-function 'sqlite-select)
                         (lambda (&rest arguments)
                           (cl-incf calls)
                           (apply original arguments))))
                (let* ((page
                        (e-board-sqlite-worker-read
                         database
                         (list :op 'board-activity-page :board-id board-id
                               :after "" :limit 8
                               :byte-limit
                               e-board-sqlite-activity-page-byte-limit)))
                       (row (car (plist-get page :participants)))
                       (outcome (plist-get row :outcome)))
                  (should (eq (plist-get outcome :status) 'done))
                  (should (equal (plist-get outcome :summary)
                                 "behind unrelated history"))
                  ;; Root, participant, session, lifecycle, and report set
                  ;; reads stay constant despite 4097 unrelated fact rows.
                  (should (= calls 5))))))
        (when database
          (sqlite-close database))))))

(ert-deftest e-board-observation-test-report-survives-same-run-history ()
  "An accepted assignment report is selected before other-run reports are capped."
  (e-board-observation-test--with-fixture
      (_directory runtime service target board-id)
    (e-board-observation-test--admit
     service board-id "03-worker" "03-worker"
     '(:board-run-id "run-1" :board-task-key "task" :board-attempt 0))
    (e-board-observation-test--report target)
    (let ((database-file (e-runtime-store--database-file runtime))
          database)
      (e-runtime-store-close runtime)
      (unwind-protect
          (progn
            (setq database (sqlite-open database-file))
            (sqlite-execute database "BEGIN")
            (dotimes (index (1+ e-board-sqlite-activity-fact-row-limit))
              (e-board-observation-test--insert-report
               database board-id (+ 2 index) "run-1"
               (format "other-task-%d" index) 0))
            (sqlite-execute database "COMMIT")
            (let ((calls 0)
                  (original (symbol-function 'sqlite-select)))
              (cl-letf (((symbol-function 'sqlite-select)
                         (lambda (&rest arguments)
                           (cl-incf calls)
                           (apply original arguments))))
                (let* ((page
                        (e-board-sqlite-worker-read
                         database
                         (list :op 'board-activity-page :board-id board-id
                               :after "" :limit 8
                               :byte-limit
                               e-board-sqlite-activity-page-byte-limit)))
                       (row (car (plist-get page :participants)))
                       (outcome (plist-get row :outcome)))
                  (should (eq (plist-get outcome :source) 'orchestration))
                  (should (eq (plist-get outcome :status) 'done))
                  (should (equal (plist-get outcome :summary)
                                 "accepted report"))
                  ;; Root, participant, session, lifecycle, and exact report
                  ;; set reads stay constant despite 4097 same-run reports.
                  (should (= calls 5))))))
        (when database
          (sqlite-close database))))))

(ert-deftest e-board-observation-test-held-read-enqueues-and-fails-locally ()
  "A held activity read returns work immediately and settles request-locally."
  (let* ((stall-directory (make-temp-file "e-board-observation-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY="
                        stall-directory)
                process-environment))
         (hold (expand-file-name "board-activity-page.hold" stall-directory))
         (ready (expand-file-name "board-activity-page.ready" stall-directory))
         (release (expand-file-name "board-activity-page.release" stall-directory)))
    (unwind-protect
        (progn
          (write-region "hold" nil hold nil 'silent)
          (e-board-observation-test--with-fixture
              (_directory _runtime service target board-id)
            (let ((work
                   (e-board-observation-activity-page-start target :limit 4)))
              (should (e-work-handle-p work))
              (should (memq (plist-get (e-work-status work) :state)
                            '(started finished)))
              (let ((deadline (+ (float-time) 2.0)))
                (while (and (not (file-exists-p ready))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should (file-exists-p ready))
              (write-region "release" nil release nil 'silent)
              (let ((page (e-board-observation-test--await work)))
                (should (= (plist-get page :generation) 1))
                (should-not (plist-get page :participants)))))
          ;; An unknown Board fails only its own request after the held worker
          ;; has been released; this does not poison the runtime owner.
          (e-board-observation-test--with-fixture
              (_directory _runtime service target _board-id)
            (let ((work
                   (e-board-sqlite-service-activity-page-start
                    service "missing-board" :limit 1)))
              (should-error (e-board-observation-test--await work)))))
      (write-region "release" nil release nil 'silent)
      (delete-directory stall-directory t))))

(ert-deftest e-board-observation-test-worker-uses-bounded-set-reads ()
  "One activity request uses fixed set reads rather than participant N+1 reads."
  (e-board-observation-test--with-fixture
      (directory runtime service target board-id)
    (ignore target)
    (dotimes (index 3)
      (let ((id (format "%02d-participant" index)))
        (e-board-observation-test--admit service board-id id id)))
    (let* ((database (sqlite-open
                      (e-runtime-store--database-file runtime) t))
           (calls 0)
           (original (symbol-function 'sqlite-select)))
      (unwind-protect
          (cl-letf (((symbol-function 'sqlite-select)
                     (lambda (&rest arguments)
                       (cl-incf calls)
                       (apply original arguments))))
            (let ((page
                   (e-board-sqlite-worker-read
                    database
                    (list :op 'board-activity-page :board-id board-id
                          :after "" :limit 8
                          :byte-limit e-board-sqlite-activity-page-byte-limit))))
              (should (= (length (plist-get page :participants)) 3))
              ;; Board root, participant set, current session set, lifecycle
              ;; set, and report set: no participant-specific reads.
              (should (= calls 5))))
        (sqlite-close database)))))

(provide 'e-board-observation-test)

;;; e-board-observation-test.el ends here

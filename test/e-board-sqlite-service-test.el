;;; e-board-sqlite-service-test.el --- SQL-owned Board application tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-sqlite-service)
(require 'e-backend)
(require 'e-chat-service)
(require 'e-chat)
(require 'e-events)
(require 'e-harness)
(require 'e-session)
(require 'e-session-query)
(require 'e-session-storage-sqlite)
(require 'e-runtime-store-worker)
(require 'e-work)

(defun e-board-sqlite-service-test--await (work)
  "Observe request-scoped WORK from this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(defun e-board-sqlite-service-test--wait-until (predicate)
  "Wait a bounded test interval for PREDICATE to return non-nil."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun e-board-sqlite-service-test--deferred-work (id)
  "Return cooperative work named ID for explicit test settlement."
  (e-work-start
   (e-work-spec-create
    :id id :execution 'cooperative :interactive-policy 'async
    :owner 'e-board-sqlite-service-test
    :runner (lambda (_handle _arguments _context) :deferred))
   nil))

(defun e-board-sqlite-service-test--admission (session-id board-id participant-id)
  "Return SESSION-ID's root/association records and final query delta."
  (let* ((policy
          (list :participant-id participant-id
                :pickup-selector '(:tags (main))
                :observer-selector '(:tags (main))
                :default-tags '(main) :default-to nil))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id :metadata (list :name session-id)
           :principal (format "chat:%s" session-id)
           :board-id board-id :association-role "owner"
           :routing-policy policy))
         (position 0)
         (records
          (mapcar
           (lambda (record)
             (let ((copy (copy-tree record t)))
               (plist-put copy :journal-position (cl-incf position))
               copy))
           (plist-get session :admission-records))))
    (list :records records
          :query-delta (plist-get session :query-delta)
          :policy policy)))

(cl-defmacro e-board-sqlite-service-test--with-fixture
    ((store service board-id session-id participant-id) &rest body)
  "Run BODY with one disposable SQL-owned Board fixture."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp symbolp) body)))
  `(let* ((directory (make-temp-file "e-board-sql-service-" t))
          (,board-id "board-sql-truth")
          (,session-id "daily-sql-truth")
          (,participant-id "participant-sql-truth")
          (,store (e-session-sqlite-store-create directory))
          (runtime (e-session-storage-runtime-store ,store))
          (,service (e-board-sqlite-service-create runtime))
          (admission
           (e-board-sqlite-service-test--admission
            ,session-id ,board-id ,participant-id)))
     (unwind-protect
         (progn
           (e-board-sqlite-service-test--await
            (e-board-sqlite-service-admit-session-owner-start
             ,service ,session-id ,board-id
             (format "chat:%s" ,session-id)
             (plist-get admission :records)
             (plist-get admission :query-delta)
             (list :id ,participant-id :author "e-chat"
                   :principal (format "chat:%s" ,session-id)
                   :controller (format "chat:%s" ,session-id)
                   :role 'owner :state 'active
                   :subscription-id "pickup-main"
                   :publication-pending nil)))
           ,@body)
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory directory t))))

(defun e-board-sqlite-service-test--append-index-run
    (service board-id run-id &optional completed)
  "Append RUN-ID's manifest and, when COMPLETED, one successful report."
  (let* ((manifest
          (list :version 1 :type 'manifest
                :idempotency-key (format "manifest:%s" run-id)
                :payload
                (list :run-id run-id
                      :descriptor (list :label run-id)
                      :tasks [(:task-key "task" :required t
                               :accepted-attempt 0)]
                      :deadline '(:kind none))))
         (result
          (e-board-sqlite-service-test--await
           (e-board-sqlite-service-orchestration-fact-start
            service board-id manifest))))
    (when completed
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-orchestration-fact-start
        service board-id
        (list :version 1 :type 'terminal-report
              :idempotency-key (format "report:%s" run-id)
              :payload
              (list :run-id run-id :task-key "task" :attempt 0
                    :status 'done :summary "ready" :outputs [])))))
    result))

(defun e-board-sqlite-service-test--append-terminal-continuation
    (service board-id session-id run-id publication-key)
  "Append RUN-ID's terminal report and a validated continuation manifest."
  (dolist
      (fact
       (list
        (list :version 1 :type 'manifest
              :idempotency-key (format "manifest:%s" run-id)
              :payload
              (list :run-id run-id
                    :tasks [(:task-key "task" :required t
                             :accepted-attempt 0)]
                    :deadline '(:kind none)
                    :continuation
                    (list :session-id session-id :prompt "Finish once."
                          :publication-key publication-key)))
        (list :version 1 :type 'terminal-report
              :idempotency-key (format "report:%s" run-id)
              :payload
              (list :run-id run-id :task-key "task" :attempt 0
                    :status 'done :summary "ready" :outputs []))))
    (e-board-sqlite-service-test--await
     (e-board-sqlite-service-orchestration-fact-start
      service board-id fact))))

(defun e-board-sqlite-service-test--orchestration-record-body
    (board-id record-id fact)
  "Build a canonical worker write body for one orchestration FACT."
  (let* ((fields (e-board-orchestration-fact-record-fields fact))
         (source-key (plist-get fields :source-key))
         (record-fields
          (cl-loop for (key value) on fields by #'cddr
                   unless (eq key :source-key)
                   append (list key value))))
    (list :op 'board-record-put :board-id board-id :generation 1
          :record
          (append (list :id record-id :record-kind 'fact
                        :created-at (float-time))
                  record-fields)
          :source (list :kind 'fact :key source-key
                        :hash (secure-hash 'sha256
                                          (prin1-to-string record-fields))))))

(defun e-board-sqlite-service-test--transactional-worker-write
    (database body)
  "Apply one Board worker write with its runtime transaction boundary."
  (sqlite-execute database "BEGIN IMMEDIATE")
  (condition-case error
      (let ((result (e-board-sqlite-worker-write database body)))
        (sqlite-execute database "COMMIT")
        result)
    (error
     (ignore-errors (sqlite-execute database "ROLLBACK"))
     (signal (car error) (cdr error)))))

(defun e-board-sqlite-service-test--index-limit-conflict-fact
    (run-id index)
  "Return one distinct valid conflict fact for RUN-ID and INDEX."
  (list :version 1 :type 'conflict
        :idempotency-key (format "conflict:%04d" index)
        :payload (list :run-id run-id :task-key "task" :attempt 0
                       :reason "index-boundary")))

(defun e-board-sqlite-service-test--run-index-page (record-page board-id)
  "Build a one-page index response for the run in RECORD-PAGE."
  (let ((record-page (if (functionp record-page)
                         (funcall record-page)
                       record-page))
        entries)
    (dolist (projection
             (e-chat-service--sqlite-orchestration-projections record-page))
      (let* ((manifest-position (1+ (length entries)))
             (summary
              (e-board-orchestration-run-index-summary
               projection board-id (1+ manifest-position) 1.0)))
        (push (list :run-id (plist-get projection :run-id)
                    :manifest-position manifest-position
                    :latest-event-position (1+ manifest-position)
                    :latest-event-at 1.0
                    :active-p (plist-get summary :active-p)
                    :summary summary)
              entries)))
    (list :board-id board-id :generation 1
          :entries (nreverse entries)
          :cursor nil :next-cursor nil :more-p nil
          :active-only t)))

(ert-deftest e-board-sqlite-publication-target-is-opaque-and-mutation-isolated ()
  "Target construction and the one public identity read use defensive copies."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (let* ((caller-board-id (copy-sequence board-id))
           (nested-string (copy-sequence "original"))
           (vector-string (copy-sequence "vector-original"))
           (hash-string (copy-sequence "hash-original"))
           (caller-hash (make-hash-table :test 'equal))
           (_ (puthash "key" hash-string caller-hash))
           (caller-tags (list 'producer (list 'nested 'original)))
           (caller-attributes
            (list :nested (list :value nested-string)
                  :vector (vector vector-string)))
           (target
            (e-board-sqlite-publication-target-create
             service caller-board-id :tags caller-tags
             :attributes caller-attributes))
           (hash-target
            (e-board-sqlite-publication-target-create
             service caller-board-id :attributes (list :table caller-hash)))
           (returned-board-id
            (e-board-sqlite-publication-target-board-id target)))
      (aset caller-board-id 0 ?X)
      (setcar caller-tags 'changed)
      (aset nested-string 0 ?X)
      (aset vector-string 0 ?X)
      (aset hash-string 0 ?X)
      (puthash "extra" "changed" caller-hash)
      (aset returned-board-id 0 ?Y)
      (should (equal (e-board-sqlite-publication-target-board-id target)
                     board-id))
      (let ((stored-hash
             (plist-get
              (e-board-sqlite-publication-target--attributes hash-target)
              :table)))
        (should (equal (gethash "key" stored-hash) "hash-original"))
        (should-not (gethash "extra" stored-hash)))
      (should-not
       (fboundp 'e-board-sqlite-publication-target-tags))
      (e-board-sqlite-service-test--await
       (e-board-sqlite-publication-target-fact-start
        target "immutable target" '(immutable-target 1)))
      (let* ((page
              (e-board-sqlite-service-test--await
               (e-board-sqlite-publication-target-record-page-start
                target :generation 1 :after 0 :limit 4)))
             (record (plist-get (car (plist-get page :records)) :record)))
        (should (equal (plist-get record :tags)
                       '(producer (nested original))))
        (should (equal (plist-get record :attributes)
                       '(:nested (:value "original")
                         :vector ["vector-original"])))))))

(ert-deftest e-board-sqlite-service-append-route-returns-canonical-result-and-dedupes-old-source ()
  "SQLite assigns canonical append/routing identity and dedupes full history."
  (e-board-sqlite-service-test--with-fixture
      (store service board-id _session-id participant-id)
    (let* ((source-key '("client" 1 1))
           (first
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-append-route-start
              service board-id
              :content "canonical prompt" :mode 'inject :tags '(main)
              :source-input-key source-key)))
           (canonical-id (plist-get (plist-get first :message) :id))
           (canonical-pickup (car (plist-get first :pickups))))
      (should (stringp canonical-id))
      (should (eq (plist-get first :status) 'posted))
      (should (equal (plist-get canonical-pickup :board-id) board-id))
      (should (equal (plist-get canonical-pickup :participant-id)
                     participant-id))
      (dotimes (index 140)
        (e-board-sqlite-service-test--await
         (e-board-sqlite-service-append-route-start
          service board-id
          :content (format "noise-%d" index) :mode 'inject :tags '(noise)
          :source-input-key (list "noise" 1 index))))
      (let ((duplicate
             (e-board-sqlite-service-test--await
              (e-board-sqlite-service-append-route-start
               service board-id
               :content "canonical prompt" :mode 'inject :tags '(main)
               :source-input-key source-key))))
        (should (eq (plist-get duplicate :status) 'duplicate))
        (should (equal (plist-get (plist-get duplicate :message) :id)
                       canonical-id))
        (should (equal (plist-get duplicate :pickups)
                       (plist-get first :pickups)))
        (should
         (equal
          (e-runtime-store-call
           (e-session-storage-runtime-store store) 'read
           (list :op 'board-pickup-list :board-id board-id
                 :generation 1 :participant-id participant-id :limit 8))
          (plist-get first :pickups)))))))

(ert-deftest e-board-sqlite-service-append-route-work-settles-with-actual-worker-result ()
  "The application work publishes the worker's canonical row, not a prediction."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (let* ((result
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-append-route-start
              service board-id :content "actual" :mode 'inject :tags '(main)
              :source-input-key '("actual" 1 1))))
           (message (plist-get result :message)))
      (should (integerp (plist-get result :revision)))
      (should (integerp (plist-get result :position)))
      (should (= (plist-get message :seq)
                 (plist-get result :position)))
      (should (equal (plist-get (car (plist-get result :pickups)) :message-id)
                     (plist-get message :id))))))

(ert-deftest e-board-sqlite-service-existing-unresolved-pickup-stays-fifo ()
  "SQLite keeps a participant's unresolved delivery FIFO authoritative."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id participant-id)
    (let* ((first
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-append-route-start
              service board-id :content "first" :tags '(main)
              :source-input-key '("fifo" 1 1))))
           (second
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-append-route-start
              service board-id :content "second" :tags '(main)
              :source-input-key '("fifo" 1 2))))
           (first-pickup (car (plist-get first :pickups)))
           (second-pickup (car (plist-get second :pickups)))
           (generation
            (plist-get
             (e-board-sqlite-service-test--await
              (e-board-sqlite-service-board-get-start service board-id))
             :generation))
           (first-id (plist-get first-pickup :delivery-id)))
      (should (eq (plist-get first-pickup :state) 'ready))
      (should (eq (plist-get second-pickup :state) 'pending))
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-transition-pickup-start
        service board-id first-id 'claim))
      (let ((retried
             (e-board-sqlite-service-test--await
              (e-board-sqlite-service-transition-pickup-start
               service board-id first-id 'retry
               (list :expected-generation generation
                     :expected-participant-id participant-id)))))
        (should (eq (plist-get (plist-get retried :pickup) :state) 'ready)))
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-transition-pickup-start
        service board-id first-id 'claim))
      (let ((consumed
             (e-board-sqlite-service-test--await
              (e-board-sqlite-service-transition-pickup-start
               service board-id first-id 'consume))))
        (should (eq (plist-get (plist-get consumed :pickup) :state)
                    'consumed))
        (should-not
         (seq-some
          (lambda (pickup)
            (equal (plist-get pickup :delivery-id) first-id))
          (e-board-sqlite-service-test--await
           (e-board-sqlite-service-pickup-page-start
            service board-id 1 participant-id 8))))
        (should (equal (plist-get (plist-get consumed :next) :delivery-id)
                       (plist-get second-pickup :delivery-id)))
        (should (eq (plist-get (plist-get consumed :next) :state) 'ready))))))

(ert-deftest e-board-sqlite-service-retry-rejects-stale-pickup-coordinates ()
  "A claimed pickup is not retried with stale generation or participant data."
  (cl-labels
      ((reject-retry (generation-value participant-value)
         (e-board-sqlite-service-test--with-fixture
             (_store service board-id _session-id participant-id)
           (let* ((route
                   (e-board-sqlite-service-test--await
                    (e-board-sqlite-service-append-route-start
                     service board-id :content "claimed" :tags '(main)
                     :source-input-key '("stale-pickup" 1))))
                  (pickup (car (plist-get route :pickups)))
                  (delivery-id (plist-get pickup :delivery-id))
                  (generation
                   (plist-get
                    (e-board-sqlite-service-test--await
                     (e-board-sqlite-service-board-get-start service board-id))
                    :generation)))
             (e-board-sqlite-service-test--await
              (e-board-sqlite-service-transition-pickup-start
               service board-id delivery-id 'claim))
             (should-error
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-transition-pickup-start
                service board-id delivery-id 'retry
                (list :expected-generation
                      (funcall generation-value generation)
                      :expected-participant-id
                      (funcall participant-value participant-id)))))
             (let ((current
                    (car
                     (e-board-sqlite-service-test--await
                      (e-board-sqlite-service-pickup-page-start
                       service board-id generation participant-id 16)))))
               (should (eq (plist-get current :state) 'claimed)))))))
    (reject-retry (lambda (generation) (1- generation)) #'identity)
    (reject-retry #'identity (lambda (_participant-id) "stale-participant"))))

(ert-deftest e-chat-service-sqlite-detects-live-in-flight-board-delivery ()
  "A peer binding's in-flight delivery prevents owner restore from retrying it."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id participant-id)
    (let* ((binding
            (e-chat-service--binding-create
             :sqlite-service service :board-id board-id
             :participant-id participant-id :lifecycle-state 'active
             :executing-turns (make-hash-table :test 'equal)))
           (peer
            (e-chat-service--binding-create
             :sqlite-service service :board-id board-id
             :participant-id participant-id :lifecycle-state 'active
             :executing-turns (make-hash-table :test 'equal)))
           (delivery-id "live-delivery"))
      (unwind-protect
          (progn
            (e-chat-service--register-board-binding binding)
            (e-chat-service--register-board-binding peer)
            (puthash delivery-id "live-turn"
                     (e-chat-service-binding-executing-turns peer))
            (should (e-chat-service--sql-delivery-executing-p
                     binding delivery-id))
            (setf (e-chat-service-binding-lifecycle-state peer) 'retired)
            (should-not (e-chat-service--sql-delivery-executing-p
                         binding delivery-id)))
        (e-chat-service--unregister-board-binding binding)
        (e-chat-service--unregister-board-binding peer)))))

(ert-deftest e-board-sqlite-service-page-cursor-crosses-commits-exactly-once ()
  "A SQLite page high-water, not callback timing, separates later commits."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (let* ((first
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-append-route-start
              service board-id :content "before" :tags '(main)
              :source-input-key '("cursor" 1 1))))
           (snapshot
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-record-page-start
              service board-id :generation 1 :after 0 :limit 32)))
           (cursor (plist-get snapshot :through)))
      (should (= (length (plist-get snapshot :records)) 1))
      (should (equal (plist-get
                      (plist-get (car (plist-get snapshot :records)) :record)
                      :id)
                     (plist-get (plist-get first :message) :id)))
      (let* ((second
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-append-route-start
                service board-id :content "after" :tags '(main)
                :source-input-key '("cursor" 1 2))))
             (old-snapshot
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-record-page-start
                service board-id :generation 1 :after 0 :through cursor
                :limit 32)))
             (changes
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-record-page-start
                service board-id :generation 1 :after cursor :limit 32))))
        (should (= (length (plist-get old-snapshot :records)) 1))
        (should (= (length (plist-get changes :records)) 1))
        (should (equal (plist-get
                        (plist-get (car (plist-get changes :records)) :record)
                        :id)
                       (plist-get (plist-get second :message) :id)))))))

(ert-deftest e-board-sqlite-service-visible-window-returns-recent-bounded-cut ()
  "A visible window is recent, bounded, and carries its SQLite high-water."
  (e-board-sqlite-service-test--with-fixture
      (store service board-id session-id _participant-id)
    (dotimes (index 4)
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-append-route-start
        service board-id :content (format "message-%d" index) :tags '(main)
        :source-input-key (list "visible-window" 1 index))))
    (let* ((window
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-visible-window-start
              service board-id :generation 1 :limit 2)))
           (records (plist-get window :records)))
      (should (= (length records) 2))
      (should (equal
               (mapcar (lambda (row)
                         (plist-get (plist-get row :record) :content))
                       records)
               '("message-2" "message-3")))
      (should (= (plist-get window :cursor) (plist-get window :through)))
      (should (= (plist-get window :through) 4)))
    (let ((view
           (e-board-sqlite-service-test--await
            (e-session-async-chat-view store session-id :limit 2))))
      (should (equal (plist-get (plist-get view :metadata) :name)
                     session-id))
      (should-not (plist-member (plist-get view :metadata) :metadata))
      (should (equal
               (mapcar (lambda (message) (plist-get message :content))
                       (plist-get view :messages))
               '("message-2" "message-3")))
      (should (equal (mapcar (lambda (message) (plist-get message :role))
                             (plist-get view :messages))
                     '(user user)))
      (should (= (plist-get view :cursor) 4)))))

(ert-deftest e-board-sqlite-service-new-session-admission-rolls-back-and-replays-after-read-overtakes-it ()
  "Composite admission rolls back wholly and survives reordered read transport."
  (let* ((directory (make-temp-file "e-board-sql-admit-" t))
         (stall-directory (make-temp-file "e-board-sql-admit-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         (session-id "atomic-new-session")
         (board-id "atomic-new-board")
         (participant-id "atomic-new-owner")
         (principal (format "chat:%s" session-id))
         (admission
          (e-board-sqlite-service-test--admission
           session-id board-id participant-id))
         (store (e-session-sqlite-store-create directory))
         (runtime (e-session-storage-runtime-store store))
         (service (e-board-sqlite-service-create runtime))
         (participant
          (list :id participant-id :author "e-chat" :principal principal
                :controller principal :role 'owner :state 'active
                :subscription-id "atomic-owner-subscription"
                :publication-pending nil))
         held)
    (cl-labels
        ((start-admission
          (candidate source-key)
          (e-board-sqlite-service-admit-session-input-start
           service session-id board-id principal
           (plist-get admission :records) (plist-get admission :query-delta)
           candidate :author (format "session:%s" session-id)
           :tags '(main) :mode 'inject :content "first"
           :source-input-key source-key)))
      (unwind-protect
          (progn
            ;; This participant failure happens after the Board and session
            ;; writes inside the worker transaction.  Neither may survive.
            (should-error
             (e-board-sqlite-service-test--await
              (start-admission
               (plist-put (copy-tree participant t) :id nil)
               '("atomic-admission" 1))))
            (should-not
             (e-runtime-store-call
              runtime 'read
              (list :op 'session-board-association :session-id session-id)))
            (should-not
             (e-runtime-store-call
              runtime 'read (list :op 'board-get :board-id board-id)))

            ;; Replace the disposable runtime after its owner-local failure.
            ;; Replay uses the same database and stable source identity.
            (e-session-sqlite-store-close store)
            (setq store (e-session-sqlite-store-create directory)
                  runtime (e-session-storage-runtime-store store)
                  service (e-board-sqlite-service-create runtime))

            ;; Hold the replay before its transaction.  The read lane overtakes
            ;; it and still observes the rolled-back state, proving there is no
            ;; shared transport FIFO dependency.
            (write-region
             "hold" nil
             (expand-file-name "chat-session-input-admit.hold" stall-directory)
             nil 'silent)
            (setq held
                  (start-admission participant '("atomic-admission" 1)))
            (let ((deadline (+ (float-time) 2.0))
                  (ready
                   (expand-file-name
                    "chat-session-input-admit.ready" stall-directory)))
              (while (and (not (file-exists-p ready))
                          (< (float-time) deadline))
                (accept-process-output nil 0.01))
              (should (file-exists-p ready)))
            (should-not
             (e-runtime-store-call
              runtime 'read
              (list :op 'session-board-association :session-id session-id)))
            (write-region
             "release" nil
             (expand-file-name
              "chat-session-input-admit.release" stall-directory)
             nil 'silent)
            (let ((committed (e-board-sqlite-service-test--await held)))
              (should (eq (plist-get committed :status) 'posted))
              (should (plist-get committed :association)))
            (let ((duplicate
                   (e-board-sqlite-service-test--await
                    (start-admission participant '("atomic-admission" 1)))))
              (should (eq (plist-get duplicate :status) 'duplicate)))
            (let ((page
                   (e-board-sqlite-service-test--await
                    (e-board-sqlite-service-record-page-start
                     service board-id :generation 1 :after 0 :limit 8))))
              (should (= (length (plist-get page :records)) 1))))
        (write-region
         "release" nil
         (expand-file-name "chat-session-input-admit.release" stall-directory)
         nil 'silent)
        (ignore-errors (e-session-sqlite-store-close store))
        (delete-directory directory t)
        (delete-directory stall-directory t)))))

(ert-deftest e-chat-service-sqlite-existing-session-submits-without-board-reconstruction ()
  "Ordinary SQLite chat composes a live port without an aggregate replica."
  (e-board-sqlite-service-test--with-fixture
      (store _service board-id session-id participant-id)
    (let* ((harness
            (e-harness-create
             :sessions store
             :backend
             (e-backend-fake-create
              :items '((:type assistant-message :content "canonical reply")
                       (:type done :reason stop)))))
           (binding
            (e-board-sqlite-service-test--await
             (e-chat-service-binding-start harness session-id)))
           events subscription)
      (unwind-protect
          (progn
            (should (e-chat-service--sql-binding-p binding))
            (dolist (retired
                     '(e-chat-service-binding-client
                       e-chat-service-binding-requester
                       e-chat-service-binding-attachment
                       e-chat-service-binding-observer
                       e-chat-service-binding-turn-map
                       e-chat-service-binding-pending-input-head
                       e-chat-service-binding-pending-input-tail
                       e-chat-service-binding-message-projection
                       e-chat-service-binding-activity-projection))
              (should-not (fboundp retired)))
            (should (equal (e-chat-service-binding-board-id binding) board-id))
            (should (equal (e-chat-service-binding-participant-id binding)
                           participant-id))
            (setq subscription
                  (e-chat-service-subscribe
                   harness session-id
                   (lambda (event) (push (copy-tree event t) events))))
            (let ((admission
                   (e-chat-service-submit-session
                    harness session-id "canonical question")))
              (should (e-work-handle-p admission))
              (e-board-sqlite-service-test--await admission))
            (let ((deadline (+ (float-time) 3.0)))
              (while (and
                      (not (seq-find
                            (lambda (event)
                              (and (eq (plist-get event :type) 'message-added)
                                   (eq (plist-get
                                        (plist-get (plist-get event :payload)
                                                   :message)
                                        :role)
                                       'assistant)))
                            events))
                      (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (let ((messages
                   (seq-filter
                    (lambda (event)
                      (eq (plist-get event :type) 'message-added))
                    events)))
              (should (= (length messages) 2))
              (should
               (equal
                (sort
                 (mapcar
                  (lambda (event)
                    (plist-get
                     (plist-get (plist-get event :payload) :message)
                     :content))
                  messages)
                 #'string<)
                '("canonical question" "canonical reply")))))
        (when subscription
          (e-chat-service-unsubscribe subscription))
        (when (e-chat-service-binding harness session-id)
          (e-chat-service--retire-binding binding))))))

(ert-deftest e-chat-service-sqlite-participant-output-resolves-current-name ()
  "A participant label is resolved relationally for records and observers."
  (e-board-sqlite-service-test--with-fixture
      (store service board-id session-id _participant-id)
    (let* ((parent-harness (e-harness-create :sessions store))
           (parent-binding
            (e-board-sqlite-service-test--await
             (e-chat-service-binding-start parent-harness session-id)))
           (child-harness
            (e-harness-create
             :sessions store
             :backend
             (e-backend-fake-create
              :items '((:type assistant-message :content "Completed.")
                       (:type done :reason stop)))))
           (child-session-id "daily-github-session")
           events subscription child-binding)
      (unwind-protect
          (progn
            (e-board-sqlite-service-test--await
             (e-chat-service-create-participant-start
              parent-binding child-harness
              :id child-session-id
              :metadata '(:subagent-label "Daily GitHub")
              :pickup-selector '(:tags (subagent))
              :observer-selector :self
              :default-tags '(subagent)
              :default-to :self))
            (setq child-binding
                  (e-chat-service-binding child-harness child-session-id))
            (should child-binding)
            (should (equal (e-chat-service-binding-participant-name
                            child-binding)
                           "Daily GitHub"))
            (setq subscription
                  (e-chat-service-subscribe
                   parent-harness session-id
                   (lambda (event) (push (copy-tree event t) events))))
            (e-board-sqlite-service-test--await
             (e-chat-service-submit-session
              child-harness child-session-id "Run GitHub"))
            (let ((deadline (+ (float-time) 3.0)))
              (while (and
                      (not
                       (seq-find
                        (lambda (event)
                          (let ((message
                                 (plist-get (plist-get event :payload)
                                            :message)))
                            (and (eq (plist-get message :role) 'assistant)
                                 (equal (plist-get message :content)
                                        "Completed."))))
                        events))
                      (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (let* ((event
                    (seq-find
                     (lambda (candidate)
                       (equal
                        (plist-get
                         (plist-get (plist-get candidate :payload) :message)
                         :content)
                        "Completed."))
                     events))
                   (message
                    (plist-get (plist-get event :payload) :message))
                   (page
                    (e-board-sqlite-service-test--await
                     (e-board-sqlite-service-record-page-start
                      service board-id :generation 1 :after 0 :limit 16)))
                   (record
                    (seq-find
                     (lambda (row)
                       (equal (plist-get (plist-get row :record) :content)
                              "Completed."))
                     (plist-get page :records))))
              (should event)
              (should-not (plist-get message :selected-participant-p))
              (should (equal (plist-get message :participant-name)
                             "Daily GitHub"))
              (should record)
              (should (equal
                       (plist-get (plist-get record :record)
                                  :participant-name)
                       "Daily GitHub")))
            (e-chat-service--retire-binding child-binding)
            (setq child-binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start
                    child-harness child-session-id)))
            (should (equal (e-chat-service-binding-participant-name
                            child-binding)
                           "Daily GitHub")))
        (when subscription
          (e-chat-service-unsubscribe subscription))
        (when child-binding
          (e-chat-service--retire-binding child-binding))
        (when (e-chat-service-binding parent-harness session-id)
          (e-chat-service--retire-binding parent-binding))))))

(ert-deftest e-board-sqlite-service-clear-advances-session-association ()
  "Clearing a Board keeps its session association routable in the new generation."
  (e-board-sqlite-service-test--with-fixture
      (store service board-id session-id participant-id)
    (let ((runtime (e-session-storage-runtime-store store)))
      (let ((cleared
             (e-runtime-store-call
              runtime 'write (list :op 'board-clear :board-id board-id))))
        (should (= (plist-get cleared :generation) 2)))
      (let ((association
             (e-runtime-store-call
              runtime 'read
              (list :op 'session-board-association :session-id session-id))))
        (should (equal (plist-get association :board-id) board-id))
        (should (equal (plist-get association :participant-id) participant-id)))
      (let ((result
             (e-board-sqlite-service-test--await
              (e-board-sqlite-service-append-route-start
               service nil :session-id session-id :author "e-chat"
               :tags '(main) :content "after clear"
               :source-input-key '(:session "after-clear")))))
        (should (= (plist-get result :generation) 2))
        (should (plist-get result :message))))))

(ert-deftest e-chat-open-board-sqlite-existing-session-is-public-and-asynchronous ()
  "Public Board open returns a buffer without synchronous session/Board reads."
  (e-board-sqlite-service-test--with-fixture
      (store service board-id _root-session-id _root-participant-id)
    (let* ((session-id "public-board-participant")
           (participant-id "public-board-participant-id")
           (principal "chat:daily-sql-truth")
           (policy
            (list :participant-id participant-id
                  :pickup-selector '(:tags (main))
                  :observer-selector '(:tags (main))
                  :default-tags '(main) :default-to nil))
           (session
            (e-board-sqlite-service-session-admission
             :id session-id :metadata '(:name "Public Board participant")
             :principal principal :board-id board-id
             :association-role "participant" :routing-policy policy))
           (records (plist-get session :admission-records))
           (query-delta (plist-get session :query-delta))
           (harness (e-harness-create :sessions store))
           buffer readiness)
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-admit-participant-start
        service session-id board-id records query-delta
        (list :id participant-id :author "e-chat" :principal principal
              :controller principal :role 'participant :state 'active
              :subscription-id "public-board-subscription"
              :publication-pending nil)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'e-session-local-state)
                       (lambda (&rest _)
                         (error "Public SQLite Board open read local state")))
                      ((symbol-function 'accept-process-output)
                       (lambda (&rest _)
                         (error "Public SQLite Board open waited"))))
              (setq buffer
                    (e-chat-open-board
                     board-id :harness harness :session-id session-id)))
            (should (buffer-live-p buffer))
            (with-current-buffer buffer
              (setq readiness e-chat--session-readiness-work)
              (should (e-work-handle-p readiness))
              (should (memq (plist-get (e-work-status readiness) :state)
                            '(started finished))))
            (e-board-sqlite-service-test--await readiness)
            (let ((binding (e-chat-service-binding harness session-id)))
              (should (e-chat-service--sql-binding-p binding))
              (should (equal (e-chat-service-binding-board-id binding)
                             board-id))
              (should (e-board-sqlite-service-p
                       (e-chat-service-binding-sqlite-service binding)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when-let* ((binding (e-chat-service-binding harness session-id)))
          (e-chat-service--retire-binding binding))))))

(ert-deftest e-chat-service-sqlite-source-cannot-reach-board-reconstruction ()
  "Ordinary SQL chat has no source edge to retired Board reconstruction."
  (let* ((source (find-library-name "e-chat-service"))
         (text (with-temp-buffer
                 (insert-file-contents source)
                 (buffer-string))))
    (dolist (retired '("e-board-sqlite-open-controller-start"
                       "board-controller-state"
                       "e-board-sqlite-controller-from-state"))
      (should-not (string-match-p (regexp-quote retired) text)))))

(ert-deftest e-chat-service-sqlite-continuation-owner-uses-bounded-sql-reconciliation ()
  "Making a live SQL binding owner starts one reconciliation and backfill."
  (e-board-sqlite-service-test--with-fixture
      (store _service board-id session-id _participant-id)
    (let ((harness (e-harness-create :sessions store)) binding)
      (unwind-protect
          (progn
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
            (let ((reconciliations 0)
                  (backfills 0))
              (cl-letf (((symbol-function
                          'e-chat-service--reconcile-sqlite-continuation)
                         (lambda (_binding) (cl-incf reconciliations)))
                        ((symbol-function
                          'e-chat-service--continuation-backfill-start)
                         (lambda (_binding) (cl-incf backfills))))
                (e-board-sqlite-service-test--await
                 (e-chat-service-binding-start harness session-id nil t)))
              (should (= reconciliations 1))
              (should (= backfills 1)))
            (should (equal (e-chat-service-binding-board-id binding) board-id))
            (should (e-chat-service-binding-continuation-owner-p binding)))
        (when binding (e-chat-service--retire-binding binding))))))

(ert-deftest e-chat-service-sqlite-continuation-index-scan-walks-pages ()
  "Continuation scans process each cursor page in order."
  (e-board-sqlite-service-test--with-fixture
      (store _service _board-id session-id _participant-id)
    (let* ((harness (e-harness-create :sessions store))
           (page-spec
            (e-work-spec-create
             :id "continuation-page" :execution 'cheap
             :interactive-policy 'cheap
             :runner
             (lambda (arguments _context)
               (if (null (plist-get arguments :cursor))
                   (list :entries '((:run-id "older-active"))
                         :next-cursor "second-page")
                 (list :entries '((:run-id "newer-active"))
                       :next-cursor nil)))))
           binding scan cursors pages)
      (unwind-protect
          (progn
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
            (cl-letf (((symbol-function
                        'e-board-sqlite-service-orchestration-run-index-page-start)
                       (lambda (_service _board-id &rest arguments)
                         (should (eq (plist-get arguments :active-only) t))
                         (should (eq (plist-get arguments
                                               :include-continuation-ref)
                                     t))
                         (should
                          (= (plist-get arguments :limit)
                             e-board-orchestration-run-set-default-record-limit))
                         (push (plist-get arguments :cursor) cursors)
                         (e-work-start
                          page-spec
                          (list :cursor (plist-get arguments :cursor))))))
              (setq scan
                    (e-chat-service--continuation-index-scan-start
                     binding
                     (lambda (_binding page)
                       (push (mapcar (lambda (entry)
                                       (plist-get entry :run-id))
                                     (plist-get page :entries))
                             pages))))
              (e-board-sqlite-service-test--await scan))
            (should (equal (nreverse cursors) '(nil "second-page")))
            (should (equal (nreverse pages)
                           '(("older-active") ("newer-active"))))
            (should (eq (plist-get (e-work-status scan) :state) 'finished))
            (should-not (e-work-handle-result scan)))
        (when binding (e-chat-service--retire-binding binding))))))

(ert-deftest e-chat-service-sqlite-continuation-queues-detached-terminal-view ()
  "One index page and one exact run query supply terminal evidence."
  (e-board-sqlite-service-test--with-fixture
      (store _service _board-id session-id _participant-id)
    (let* ((harness (e-harness-create :sessions store))
           (prompt "Apply and finalize this Daily exactly once.")
           (manifest
            `(:version 1 :type manifest :idempotency-key "manifest:daily"
              :payload (:run-id "daily-run"
                        :tasks [(:task-key "report" :required t
                                 :accepted-attempt 0)]
                        :deadline (:kind none)
                        :continuation (:session-id "coordinator-session"
                                       :prompt ,prompt
                                       :publication-key "continue:daily"))))
           (report
            '(:version 1 :type terminal-report :idempotency-key "report:daily"
              :payload (:run-id "daily-run" :task-key "report" :attempt 0
                        :status done :summary "ready"
                        :outputs ((:kind daily :content "one")))))
           (record
            (lambda (fact)
              (append (list :record-kind 'fact)
                      (e-board-orchestration-fact-record-fields fact))))
           (page
            (list :records
                  (list (list :record (funcall record manifest))
                        (list :record (funcall record report)))
                  :truncated nil))
           (query-spec
            (e-work-spec-create
             :id "continuation-query" :execution 'cheap
             :interactive-policy 'cheap
             :runner (lambda (_arguments _context) page)))
           (index-query-spec
            (e-work-spec-create
             :id "continuation-index-query" :execution 'cheap
             :interactive-policy 'cheap
             :runner
             (lambda (_arguments _context)
               (e-board-sqlite-service-test--run-index-page
                page _board-id))))
           (admission-spec
            (e-work-spec-create
             :id "continuation-admission" :execution 'cheap
             :interactive-policy 'cheap
             :runner (lambda (_arguments _context) '(:admitted t))))
           binding queued-session-id queued-input queued-metadata
           queue-saw-pending-p claim-statuses
           (query-count 0) (projection-count 0)
           (publication-count 0) (backfill-count 0)
           (scan-start-function
            (symbol-function 'e-chat-service--continuation-index-scan-start))
           scan-work)
      (unwind-protect
          (progn
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
            (cl-letf (((symbol-function
                          'e-board-sqlite-service-orchestration-run-index-page-start)
                       (lambda (&rest _)
                         (cl-incf query-count)
                         (e-work-start index-query-spec nil)))
                      ((symbol-function
                        'e-board-sqlite-service-orchestration-run-start)
                       (lambda (&rest _)
                         (cl-incf projection-count)
                         (e-work-start query-spec nil)))
                      ((symbol-function
                        'e-chat-service--continuation-index-scan-start)
                       (lambda (scan-binding page-function)
                         (setq scan-work
                               (funcall scan-start-function
                                        scan-binding page-function))))
                      ((symbol-function
                        'e-chat-service--continuation-backfill-start)
                       (lambda (&rest _)
                         (cl-incf backfill-count)))
                      ((symbol-function 'e-chat-service-queue-session)
                       (lambda (_harness session-id input &rest arguments)
                         (setq queued-session-id session-id
                               queued-input input
                               queue-saw-pending-p
                               (eq (car claim-statuses) 'pending)
                               queued-metadata
                               (plist-get arguments :metadata))
                         (e-work-start admission-spec nil)))
                      ((symbol-function
                        'e-chat-service--publish-sqlite-continuation-claim)
                       (lambda (_binding _run-id _publication-key generation
                                status &optional _error)
                         (should (= generation 1))
                         (cl-incf publication-count)
                         (push status claim-statuses)
                         (e-work-start admission-spec nil))))
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work))
            (should (= query-count 1))
            (should (= projection-count 1))
            (should (= backfill-count 0))
            (should (= publication-count 2))
            (should (equal (nreverse claim-statuses) '(pending published)))
            (should queue-saw-pending-p)
            (should (equal queued-session-id "coordinator-session"))
            (should (eq (plist-get queued-metadata :display) 'hidden))
            (should (equal (plist-get queued-metadata :board-run-id)
                           "daily-run"))
            (should (= (plist-get queued-metadata :board-run-generation) 1))
            (should (equal (plist-get queued-metadata
                                      :board-continuation-key)
                           "continue:daily"))
            (should (string-match-p (regexp-quote prompt) queued-input))
            (should (string-match-p ":terminal-status done" queued-input))
            (should (string-match-p ":summary \"ready\"" queued-input))
            (should-not (string-match-p ":manifest" queued-input))
            (let ((start 0) (occurrences 0))
              (while (string-match (regexp-quote prompt) queued-input start)
                (setq occurrences (1+ occurrences)
                      start (match-end 0)))
              (should (= occurrences 1))))
        (when binding (e-chat-service--retire-binding binding))))))

(ert-deftest e-chat-service-sqlite-continuation-fences-stale-claim-reads ()
  "A settled admission cannot be requeued before SQL exposes its claim."
  (e-board-sqlite-service-test--with-fixture
      (store _service board-id session-id _participant-id)
    (let* ((harness (e-harness-create :sessions store))
           (manifest
            `(:version 1 :type manifest :idempotency-key "manifest:daily"
              :payload (:run-id "daily-run"
                        :tasks [(:task-key "report" :required t
                                 :accepted-attempt 0)]
                        :deadline (:kind none)
                        :continuation (:session-id ,session-id
                                       :prompt "Finalize once."
                                       :publication-key "continue:daily"))))
           (report
            '(:version 1 :type terminal-report :idempotency-key "report:daily"
              :payload (:run-id "daily-run" :task-key "report" :attempt 0
                        :status done :summary "ready" :outputs [])))
           (claim
            '(:version 1 :type continuation-claim
              :idempotency-key "continuation-claim:continue:daily:published"
              :payload (:run-id "daily-run"
                        :publication-key "continue:daily"
                        :status published)))
           (record
            (lambda (fact)
              (append (list :record-kind 'fact)
                      (e-board-orchestration-fact-record-fields fact))))
           (published-p nil)
           (page
            (lambda ()
              (list :records
                    (mapcar
                     (lambda (fact) (list :record (funcall record fact)))
                     (append (list manifest report)
                             (when published-p (list claim))))
                    :truncated nil)))
           (query-spec
            (e-work-spec-create
             :id "continuation-query" :execution 'cheap
             :interactive-policy 'cheap
             :runner (lambda (_arguments _context) (funcall page))))
           (index-query-spec
            (e-work-spec-create
             :id "continuation-index-query" :execution 'cheap
             :interactive-policy 'cheap
             :runner
             (lambda (_arguments _context)
               (e-board-sqlite-service-test--run-index-page
                page board-id))))
           (scan-start-function
            (symbol-function 'e-chat-service--continuation-index-scan-start))
           scan-work
           binding pending-work admission claim-work (queue-count 0))
      (unwind-protect
          (progn
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
            (cl-letf (((symbol-function
                        'e-board-sqlite-service-orchestration-run-index-page-start)
                       (lambda (&rest _) (e-work-start index-query-spec nil)))
                      ((symbol-function
                        'e-board-sqlite-service-orchestration-run-start)
                       (lambda (&rest _) (e-work-start query-spec nil)))
                      ((symbol-function
                        'e-chat-service--continuation-index-scan-start)
                       (lambda (scan-binding page-function)
                         (setq scan-work
                               (funcall scan-start-function
                                        scan-binding page-function))))
                      ((symbol-function 'e-chat-service-queue-session)
                       (lambda (&rest _)
                         (cl-incf queue-count)
                         (setq admission
                               (e-board-sqlite-service-test--deferred-work
                                "held-continuation-admission"))))
                      ((symbol-function
                        'e-chat-service--publish-sqlite-continuation-claim)
                       (lambda (_binding _run-id _publication-key _generation
                                status &optional _error)
                         (if (eq status 'pending)
                             (setq pending-work
                                   (e-board-sqlite-service-test--deferred-work
                                    "held-pending-continuation-claim"))
                           (setq claim-work
                                 (e-board-sqlite-service-test--deferred-work
                                  "held-published-continuation-claim"))))))
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (should (= queue-count 0))
              ;; The pending claim owns the admission fence before the input
              ;; publication starts.
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (should (= queue-count 0))
              (e-work-finish pending-work '(:pending t))
              (should (= queue-count 1))
              ;; An in-flight input admission fences another stale query.
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (should (= queue-count 1))
              (e-work-finish admission '(:admitted t))
              (should (e-work-handle-p claim-work))
              ;; The claim write itself owns the fence.
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (should (= queue-count 1))
              (e-work-finish claim-work '(:published t))
              ;; Even after write settlement, a query that began before the
              ;; commit became visible must not requeue the continuation.
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (should (= queue-count 1))
              (let* ((runtime (e-chat-service--binding-runtime binding))
                     (admissions
                      (e-chat-service--runtime-coordination-table
                       e-chat-service--continuation-admissions runtime)))
                (should
                 (eq (gethash (cons board-id "continue:daily") admissions)
                     'published-awaiting-observation)))
              ;; Only a bounded query that observes the durable claim releases
              ;; the process-local race sentinel.
              (setq published-p t)
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (let* ((runtime (e-chat-service--binding-runtime binding))
                     (admissions
                      (e-chat-service--runtime-coordination-table
                       e-chat-service--continuation-admissions runtime)))
                (should-not
                 (and admissions
                      (gethash (cons board-id "continue:daily") admissions))))))
        (when binding (e-chat-service--retire-binding binding))))))

(ert-deftest e-chat-service-sqlite-continuation-clear-retires-held-run-read ()
  "A clear crossing an exact continuation read retires that old generation."
  (let* ((stall-directory (make-temp-file "e-chat-continuation-clear-read-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         (hold (expand-file-name "board-orchestration-run.hold" stall-directory))
         (ready (expand-file-name "board-orchestration-run.ready" stall-directory))
         (release
          (expand-file-name "board-orchestration-run.release" stall-directory)))
    (unwind-protect
        (progn
          (write-region "hold" nil hold nil 'silent)
          (e-board-sqlite-service-test--with-fixture
              (store service board-id session-id _participant-id)
            (let* ((harness (e-harness-create :sessions store))
                   (run-id "clear-held-read-run")
                   (publication-key "continue:clear-held-read")
                   (scan-start
                    (symbol-function
                     'e-chat-service--continuation-index-scan-start))
                   binding scan-work (queue-count 0) failures)
              (unwind-protect
                  (progn
                    (e-board-sqlite-service-test--append-terminal-continuation
                     service board-id session-id run-id publication-key)
                    (setq binding
                          (e-board-sqlite-service-test--await
                           (e-chat-service-binding-start harness session-id)))
                    (cl-letf (((symbol-function
                                'e-chat-service--continuation-index-scan-start)
                               (lambda (scan-binding page-function)
                                 (setq scan-work
                                       (funcall scan-start
                                                scan-binding page-function))))
                              ((symbol-function 'e-chat-service-queue-session)
                               (lambda (&rest _arguments)
                                 (cl-incf queue-count)))
                              ((symbol-function 'e-chat-service--sql-note-failure)
                               (lambda (_binding error &optional _owner-suspect-p)
                                 (push error failures))))
                      (e-chat-service--reconcile-sqlite-continuation binding)
                      (should (e-work-handle-p scan-work))
                      (e-board-sqlite-service-test--wait-until
                       (lambda () (file-exists-p ready)))
                      (e-board-sqlite-service-test--await
                       (e-board-sqlite-service--start
                        service 'write
                        (list :op 'board-clear :board-id board-id
                              :generation 1)))
                      (write-region "release" nil release nil 'silent)
                      (e-board-sqlite-service-test--await scan-work)
                      (should (eq (plist-get (e-work-status scan-work) :state)
                                  'finished))
                      (should (= (or queue-count 0) 0))
                      (should-not failures)
                      (let ((current-run
                             (e-board-sqlite-service-test--await
                              (e-board-sqlite-service-orchestration-run-start
                               service board-id run-id 1024 2))))
                        (should (= (plist-get current-run :generation) 2))
                        (should-not (plist-get current-run :records)))))
                (when binding (e-chat-service--retire-binding binding))))))
      (write-region "release" nil release nil 'silent)
      (delete-directory stall-directory t))))

(ert-deftest e-chat-service-sqlite-continuation-clear-fences-later-claims-and-outcomes ()
  "A committed old-generation pending claim cannot publish later facts."
  (e-board-sqlite-service-test--with-fixture
      (store service board-id session-id _participant-id)
    (let* ((harness (e-harness-create :sessions store))
           (run-id "postclaim-clear-run")
           (publication-key "continue:postclaim-clear")
           (scan-start
            (symbol-function 'e-chat-service--continuation-index-scan-start))
           (publish-claim
            (symbol-function 'e-chat-service--publish-sqlite-continuation-claim))
           binding scan-work admission-work published-claim-work
           queued-metadata failures)
      (unwind-protect
          (progn
            (e-board-sqlite-service-test--append-terminal-continuation
             service board-id session-id run-id publication-key)
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
            (cl-letf (((symbol-function
                        'e-chat-service--continuation-index-scan-start)
                       (lambda (scan-binding page-function)
                         (setq scan-work
                               (funcall scan-start scan-binding page-function))))
                      ((symbol-function 'e-chat-service-queue-session)
                       (lambda (_harness _session-id _input &rest arguments)
                         (setq queued-metadata (plist-get arguments :metadata)
                               admission-work
                               (e-board-sqlite-service-test--deferred-work
                                "held-postclaim-clear-admission"))
                         admission-work))
                      ((symbol-function
                        'e-chat-service--publish-sqlite-continuation-claim)
                       (lambda (claim-binding claim-run-id claim-key generation
                                status &optional error)
                         (let ((work
                                (funcall publish-claim claim-binding claim-run-id
                                         claim-key generation status error)))
                           (when (eq status 'published)
                             (setq published-claim-work work))
                           work)))
                      ((symbol-function 'e-chat-service--sql-note-failure)
                       (lambda (_binding error &optional _owner-suspect-p)
                         (push error failures))))
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (e-board-sqlite-service-test--wait-until
               (lambda () (e-work-handle-p admission-work)))
              (should (= (plist-get queued-metadata :board-run-generation) 1))
              (let* ((page
                      (e-board-sqlite-service-test--await
                       (e-board-sqlite-service-orchestration-run-start
                        service board-id run-id 1024 1)))
                     (projection
                      (e-board-orchestration-reduce
                       (mapcar (lambda (row) (plist-get row :record))
                               (plist-get page :records)))))
                (should (eq (plist-get (plist-get projection :continuation)
                                       :state)
                            'pending)))
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service--start
                service 'write
                (list :op 'board-clear :board-id board-id :generation 1)))
              (e-work-finish admission-work '(:admitted t))
              (should (e-work-handle-p published-claim-work))
              (should-error
               (e-board-sqlite-service-test--await published-claim-work)
               :type 'e-runtime-store-board-conflict)
              (should
               (e-chat-service--stale-board-generation-error-p
                (e-work-handle-error published-claim-work)))
              (let* ((runtime (e-chat-service--binding-runtime binding))
                     (admissions
                      (e-chat-service--runtime-coordination-table
                       e-chat-service--continuation-admissions runtime)))
                (should-not
                 (and admissions
                      (gethash (cons board-id publication-key) admissions))))
              (let* ((turn-id "late-continuation-turn")
                     (context
                      (e-chat-service--continuation-context
                       queued-metadata turn-id))
                     (turns
                      (e-chat-service-binding-continuation-turns binding))
                     (outcome-work nil))
                (should (= (plist-get context :board-run-generation) 1))
                (puthash turn-id context turns)
                (setq outcome-work
                      (e-chat-service--sql-publish-continuation-outcome
                       binding (list :turn-id turn-id :payload nil) 'done))
                (should (e-work-handle-p outcome-work))
                (should-error
                 (e-board-sqlite-service-test--await outcome-work)
                 :type 'e-runtime-store-board-conflict)
                (should-not (gethash turn-id turns)))
              (let ((current-run
                     (e-board-sqlite-service-test--await
                      (e-board-sqlite-service-orchestration-run-start
                       service board-id run-id 1024 2))))
                (should (= (plist-get current-run :generation) 2))
                (should-not (plist-get current-run :records)))
              (should-not failures)))
        (when binding (e-chat-service--retire-binding binding))))))

(ert-deftest e-chat-service-sqlite-continuation-does-not-queue-without-pending-claim ()
  "A failed durable pending claim keeps the continuation unpublished."
  (e-board-sqlite-service-test--with-fixture
      (store _service board-id session-id _participant-id)
    (let* ((harness (e-harness-create :sessions store))
           (manifest
            `(:version 1 :type manifest :idempotency-key "manifest:daily"
              :payload (:run-id "daily-run"
                        :tasks [(:task-key "report" :required t
                                 :accepted-attempt 0)]
                        :deadline (:kind none)
                        :continuation (:session-id ,session-id
                                       :prompt "Finalize once."
                                       :publication-key "continue:daily"))))
           (report
            '(:version 1 :type terminal-report :idempotency-key "report:daily"
              :payload (:run-id "daily-run" :task-key "report" :attempt 0
                        :status done :summary "ready" :outputs [])))
           (record
            (lambda (fact)
              (append (list :record-kind 'fact)
                      (e-board-orchestration-fact-record-fields fact))))
           (page
            (list :records
                  (mapcar (lambda (fact) (list :record (funcall record fact)))
                          (list manifest report))
                  :truncated nil))
           (query-spec
            (e-work-spec-create
             :id "continuation-query" :execution 'cheap
             :interactive-policy 'cheap
             :runner (lambda (_arguments _context) page)))
           (index-query-spec
            (e-work-spec-create
             :id "continuation-index-query" :execution 'cheap
             :interactive-policy 'cheap
             :runner
             (lambda (_arguments _context)
               (e-board-sqlite-service-test--run-index-page
                page board-id))))
           (scan-start-function
            (symbol-function 'e-chat-service--continuation-index-scan-start))
           scan-work
           binding pending-work (queue-count 0) failures)
      (unwind-protect
          (progn
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
            (cl-letf (((symbol-function
                        'e-board-sqlite-service-orchestration-run-index-page-start)
                       (lambda (&rest _) (e-work-start index-query-spec nil)))
                      ((symbol-function
                        'e-board-sqlite-service-orchestration-run-start)
                       (lambda (&rest _) (e-work-start query-spec nil)))
                      ((symbol-function
                        'e-chat-service--continuation-index-scan-start)
                       (lambda (scan-binding page-function)
                         (setq scan-work
                               (funcall scan-start-function
                                        scan-binding page-function))))
                      ((symbol-function
                        'e-chat-service--publish-sqlite-continuation-claim)
                       (lambda (_binding _run-id _publication-key _generation
                                status &optional _error)
                         (should (eq status 'pending))
                         (setq pending-work
                               (e-board-sqlite-service-test--deferred-work
                                "failed-pending-continuation-claim"))))
                      ((symbol-function 'e-chat-service-queue-session)
                       (lambda (&rest _arguments) (cl-incf queue-count)))
                      ((symbol-function 'e-chat-service--sql-note-failure)
                       (lambda (_binding error &optional owner-suspect-p)
                         (push (list error owner-suspect-p) failures))))
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (should (e-work-handle-p pending-work))
              (e-chat-service--reconcile-sqlite-continuation binding)
              (e-board-sqlite-service-test--await scan-work)
              (should (= queue-count 0))
              (e-work-fail
               pending-work '(e-board-sqlite-error "pending claim failed"))
              (should (= queue-count 0))
              (should (cadar failures))
              (let* ((runtime (e-chat-service--binding-runtime binding))
                     (admissions
                      (e-chat-service--runtime-coordination-table
                       e-chat-service--continuation-admissions runtime)))
                (should-not
                 (and admissions
                      (gethash (cons board-id "continue:daily") admissions)))))
        (when binding (e-chat-service--retire-binding binding)))))))

(ert-deftest e-chat-service-sqlite-nil-final-continuation-claim-reports-failure-and-keeps-fence ()
  "A nil final claim result is visible without reopening the publication race."
  (e-board-sqlite-service-test--with-fixture
      (_store _service board-id session-id _participant-id)
    (let* ((harness (e-harness-create :sessions _store))
           binding failure)
      (unwind-protect
          (progn
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
            (let* ((runtime (e-chat-service--binding-runtime binding))
                   (admissions
                    (e-chat-service--runtime-coordination-table
                     e-chat-service--continuation-admissions runtime t))
                   (publication-key "continue:nil-final-claim")
                   (admission-key (cons board-id publication-key))
                   (expected (list 'admission-in-flight)))
              (puthash admission-key expected admissions)
              (cl-letf (((symbol-function
                          'e-chat-service--publish-sqlite-continuation-claim)
                         (lambda (&rest _) nil))
                        ((symbol-function 'e-chat-service--sql-note-failure)
                         (lambda (_binding error &optional owner-suspect-p)
                           (setq failure (list error owner-suspect-p)))))
                (e-chat-service--finish-sqlite-continuation-admission
                 binding "daily-run" publication-key 1 expected 'published)
                (should
                 (equal (car failure)
                        '(e-chat-service-error
                          "Continuation claim publication returned no work")))
                (should-not (cadr failure))
                (should
                 (eq (gethash admission-key admissions)
                     'publication-failed-awaiting-observation)))))
        (when binding (e-chat-service--retire-binding binding))))))

(ert-deftest e-chat-service-sqlite-continuation-restores-busy-queue-and-backfills-outcome ()
  "A queued completion resumes after restart and backfills its terminal outcome."
  (let* ((directory (make-temp-file "e-chat-continuation-restart-" t))
         (session-id "continuation-restart-owner")
         (board-id "continuation-restart-board")
         (participant-id "continuation-restart-participant")
         (principal (format "chat:%s" session-id))
         (run-id "continuation-restart-run")
         (publication-key "continue:restart")
         (admission
          (e-board-sqlite-service-test--admission
           session-id board-id participant-id))
         (store-1 (e-session-sqlite-store-create directory))
         (service-1
          (e-board-sqlite-service-create
           (e-session-storage-runtime-store store-1)))
         (busy-requests 0)
         (busy-backend
          (e-backend-create
           :name "held-busy-turn"
           :start
           (cl-function
            (lambda (&key on-request-start &allow-other-keys)
              (cl-incf busy-requests)
              (let ((request (e-backend-request-create :cancel (lambda () t))))
                (when on-request-start (funcall on-request-start request))
                request)))))
         (harness-1 (e-harness-create :sessions store-1 :backend busy-backend))
         (store-2 nil)
         (service-2 nil)
         (harness-2 nil)
         (binding-1 nil)
         (binding-2 nil)
         (store-3 nil)
         (service-3 nil)
         (harness-3 nil)
         (binding-3 nil)
         (manifest
          `(:version 1 :type manifest :idempotency-key "manifest:restart"
            :payload (:run-id ,run-id
                      :tasks ((:task-key "report" :required t
                               :accepted-attempt 0))
                      :deadline (:kind none)
                      :continuation (:session-id ,session-id
                                     :prompt "Finalize after the busy turn."
                                     :publication-key ,publication-key))))
         (report
          `(:version 1 :type terminal-report :idempotency-key "report:restart"
            :payload (:run-id ,run-id :task-key "report" :attempt 0
                      :status done :summary "ready" :outputs []))))
    (cl-labels
        ((board-records (service)
           (plist-get
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-record-page-start
              service board-id :generation 1 :after 0 :limit 64))
            :records))
         (continuation-input-p (row)
           (let ((record (plist-get row :record)))
             (and (eq (plist-get record :record-kind) 'input)
                  (equal (plist-get (plist-get record :attributes)
                                    :board-continuation-key)
                         publication-key))))
         (continuation-inputs (service)
           (seq-filter #'continuation-input-p (board-records service)))
         (continuation-pickup-p (pickup)
           (equal
            (plist-get
             (plist-get (plist-get pickup :cause-metadata) :input-attributes)
             :board-continuation-key)
            publication-key))
         (continuation-pickup (service)
           (seq-find
            #'continuation-pickup-p
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-pickup-page-start
              service board-id 1 participant-id 16))))
         (wait-for-claimed-pickup (service)
           (let ((deadline (+ (float-time) 5.0))
                 pickup)
             (while (and (not (eq (plist-get pickup :state) 'claimed))
                         (< (float-time) deadline))
               (setq pickup (continuation-pickup service))
               (unless (eq (plist-get pickup :state) 'claimed)
                 (accept-process-output nil 0.01)))
             pickup))
         (run-projection (service)
           (let* ((page
                   (e-board-sqlite-service-test--await
                    (e-board-sqlite-service-orchestration-run-start
                     service board-id run-id)))
                  (records
                   (mapcar (lambda (row) (plist-get row :record))
                           (plist-get page :records))))
             (e-board-orchestration-reduce records)))
         (wait-for-session-outcome (store)
           (let ((deadline (+ (float-time) 5.0))
                 outcome)
             (while (and (not (plist-get outcome :known-p))
                         (< (float-time) deadline))
               (setq outcome
                     (e-board-sqlite-service-test--await
                      (e-session-async-continuation-outcome
                       store session-id run-id publication-key)))
               (unless (plist-get outcome :known-p)
                 (accept-process-output nil 0.01)))
             outcome))
         (wait-for-output-record (service)
           (let ((deadline (+ (float-time) 5.0))
                 output)
             (while (and (not output) (< (float-time) deadline))
               (setq output
                     (seq-find
                      (lambda (row)
                        (let ((record (plist-get row :record)))
                          (and (eq (plist-get record :record-kind) 'output)
                               (equal (plist-get record :content)
                                      "Coordinator complete."))))
                      (board-records service)))
               (unless output (accept-process-output nil 0.01)))
             output)))
      (unwind-protect
          (progn
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-admit-session-owner-start
              service-1 session-id board-id principal
              (plist-get admission :records)
              (plist-get admission :query-delta)
              (list :id participant-id :author "e-chat"
                    :principal principal :controller principal :role 'owner
                    :state 'active :subscription-id "restart-owner-subscription"
                    :publication-pending nil)))
            (setq binding-1
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness-1 session-id)))
            (e-board-sqlite-service-test--await
             (e-chat-service-submit-session
              harness-1 session-id "Keep this turn busy."))
            (let ((deadline (+ (float-time) 5.0)))
              (while (and (= busy-requests 0) (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (= busy-requests 1))
            (should (e-chat-service-active-turn-p harness-1 session-id))
            (dolist (fact (list manifest report))
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-orchestration-fact-start
                service-1 board-id fact)))
            (e-board-sqlite-service-test--await
             (e-chat-service-binding-start harness-1 session-id nil t))
            (let ((deadline (+ (float-time) 5.0))
                  (pickup (wait-for-claimed-pickup service-1))
                  queued)
              (while (and (null queued) (< (float-time) deadline))
                (setq queued (e-harness-queued-prompts harness-1 session-id))
                (unless queued (accept-process-output nil 0.01)))
              (should (eq (plist-get pickup :state) 'claimed))
              (should (e-chat-service-active-turn-p harness-1 session-id))
              (should (= (length queued) 1))
              (should (equal
                       (plist-get (plist-get (car queued) :metadata)
                                  :board-continuation-key)
                       publication-key)))
            ;; The owner turn queue is process-local; the claimed pickup and
            ;; keyed input are durable at this simulated restart boundary.
            (should (= (length (continuation-inputs service-1)) 1))
            (let ((projection (run-projection service-1)))
              (should (eq (plist-get (plist-get projection :continuation)
                                     :state)
                          'published))
              (should-not (plist-get projection :continuation-outcome)))
            (e-chat-service--retire-binding binding-1)
            (setq binding-1 nil)
            (e-session-sqlite-store-close store-1)

            (setq store-2 (e-session-sqlite-store-create directory)
                  service-2
                  (e-board-sqlite-service-create
                   (e-session-storage-runtime-store store-2))
                  harness-2
                  (e-harness-create
                   :sessions store-2
                   :backend
                   (e-backend-fake-create
                    :items '((:type assistant-message
                              :content "Coordinator complete.")
                             (:type done :reason stop)))))
            ;; Leave the already-durable session completion without its Board
            ;; outcome fact, then prove the next owner can reconstruct it.
            (cl-letf (((symbol-function
                        'e-chat-service--publish-sqlite-continuation-outcome)
                       (lambda (&rest _) nil)))
              (setq binding-2
                    (e-board-sqlite-service-test--await
                     (e-chat-service-binding-start
                      harness-2 session-id nil t)))
              (let ((outcome (wait-for-session-outcome store-2)))
                (should (plist-get outcome :known-p))
                (should (eq (plist-get outcome :status) 'done)))
              (should-not (e-chat-service-active-turn-p harness-2 session-id))
              (should (wait-for-output-record service-2))
              (should (= (length (continuation-inputs service-2)) 1))
              (let ((projection (run-projection service-2)))
                (should (eq (plist-get (plist-get projection :continuation)
                                       :state)
                            'published))
                (should-not (plist-get projection :continuation-outcome))))
            (e-chat-service--retire-binding binding-2)
            (setq binding-2 nil)
            (e-session-sqlite-store-close store-2)

            (setq store-3 (e-session-sqlite-store-create directory)
                  service-3
                  (e-board-sqlite-service-create
                   (e-session-storage-runtime-store store-3))
                  harness-3 (e-harness-create :sessions store-3))
            (setq binding-3
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness-3 session-id nil t)))
            (e-board-sqlite-service-test--await
             (e-chat-service--continuation-backfill-start binding-3))
            (let* ((projection (run-projection service-3))
                   (outcome (plist-get projection :continuation-outcome)))
              (should (eq (plist-get outcome :status) 'done))
              (should (= (length (plist-get projection :continuation-outcomes))
                         1))
              (should
               (equal (plist-get outcome :publication-key) publication-key))
              (should (eq (plist-get (plist-get projection :continuation)
                                     :state)
                          'published)))
            (should (= (length (continuation-inputs service-3)) 1)))
        (when binding-3 (e-chat-service--retire-binding binding-3))
        (when binding-2 (e-chat-service--retire-binding binding-2))
        (when binding-1 (e-chat-service--retire-binding binding-1))
        (dolist (store (list store-3 store-2 store-1))
          (when store (ignore-errors (e-session-sqlite-store-close store))))
        (delete-directory directory t)))))

(ert-deftest e-chat-service-sqlite-pending-continuation-restores-before-input-and-preserves-fifo ()
  "A restored pending claim starts after older input settles and completes once."
  (let* ((directory (make-temp-file "e-chat-pending-continuation-restart-" t))
         (session-id "pending-continuation-restart-owner")
         (board-id "pending-continuation-restart-board")
         (participant-id "pending-continuation-restart-participant")
         (principal (format "chat:%s" session-id))
         (run-id "pending-continuation-restart-run")
         (publication-key "continue:pending-restart")
         (user-prompt "User input queued before completion.")
         (busy-prompt "Keep the restarted owner turn busy.")
         (admission
          (e-board-sqlite-service-test--admission
           session-id board-id participant-id))
         (store-1 (e-session-sqlite-store-create directory))
         (service-1
          (e-board-sqlite-service-create
           (e-session-storage-runtime-store store-1)))
         (harness-1 (e-harness-create :sessions store-1))
         (binding-1 nil)
         (store-2 nil)
         (service-2 nil)
         (harness-2 nil)
         (binding-2 nil)
         (backend-start-count 0)
         backend-starts
         activity-events
         activity-subscription
         user-delivery-id
         continuation-delivery-id
         (busy-backend
          (e-backend-create
           :name "pending-restart-held-turn"
           :start
           (cl-function
            (lambda (&key messages options on-item on-done on-error
                        on-request-start &allow-other-keys)
              (ignore options on-error)
              (cl-incf backend-start-count)
              (let ((request
                     (e-backend-request-create :cancel (lambda () t))))
                (setq backend-starts
                      (append backend-starts
                              (list (list :messages (copy-tree messages t)
                                          :on-item on-item
                                          :on-done on-done))))
                (when on-request-start (funcall on-request-start request))
                request)))))
         (manifest
          `(:version 1 :type manifest :idempotency-key "manifest:pending-restart"
            :payload (:run-id ,run-id
                      :tasks ((:task-key "report" :required t
                               :accepted-attempt 0))
                      :deadline (:kind none)
                      :continuation (:session-id ,session-id
                                     :prompt "Finalize after restore."
                                     :publication-key ,publication-key))))
         (report
          `(:version 1 :type terminal-report
            :idempotency-key "report:pending-restart"
            :payload (:run-id ,run-id :task-key "report" :attempt 0
                      :status done :summary "ready" :outputs []))))
    (cl-labels
        ((board-records (service)
           (plist-get
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-record-page-start
              service board-id :generation 1 :after 0 :limit 64))
            :records))
         (continuation-input-p (row)
           (let ((record (plist-get row :record)))
             (and (eq (plist-get record :record-kind) 'input)
                  (equal (plist-get (plist-get record :attributes)
                                    :board-continuation-key)
                         publication-key))))
         (continuation-inputs (service)
           (seq-filter #'continuation-input-p (board-records service)))
         (pickups (service)
           (e-board-sqlite-service-test--await
            (e-board-sqlite-service-pickup-page-start
             service board-id 1 participant-id 16)))
         (run-projection (service)
           (let* ((page
                   (e-board-sqlite-service-test--await
                    (e-board-sqlite-service-orchestration-run-start
                     service board-id run-id)))
                  (records
                   (mapcar (lambda (row) (plist-get row :record))
                           (plist-get page :records))))
             (e-board-orchestration-reduce records)))
         (wait-for-queue-size (harness wanted)
           (let ((deadline (+ (float-time) 5.0))
                 (queued (e-harness-queued-prompts harness session-id)))
             (while (and (< (length queued) wanted)
                         (< (float-time) deadline))
               (accept-process-output nil 0.01)
               (setq queued (e-harness-queued-prompts harness session-id)))
             queued))
         (wait-for-backend-starts (wanted)
           (let ((deadline (+ (float-time) 5.0)))
             (while (and (< backend-start-count wanted)
                         (< (float-time) deadline))
               (accept-process-output nil 0.01))
             backend-starts))
         (last-backend-user-message (ordinal)
           (let* ((started (nth (1- ordinal) backend-starts))
                  (messages (plist-get started :messages)))
             (car (last
                   (seq-filter
                    (lambda (message)
                      (eq (plist-get message :role) 'user))
                    messages)))))
         (finish-backend-start (ordinal content)
           (let ((started (nth (1- ordinal) backend-starts)))
             (should started)
             (funcall (plist-get started :on-item)
                      (list :type 'assistant-message :content content))
             (funcall (plist-get started :on-item)
                      '(:type done :reason stop))
             (funcall (plist-get started :on-done) '(:status done))))
         (input-consumed-event (delivery-id)
           (seq-find
            (lambda (event)
              (and (eq (plist-get event :type) 'input-consumed)
                   (equal (plist-get (plist-get event :payload) :delivery-id)
                          delivery-id)))
            activity-events))
         (pickup-unresolved-p (service delivery-id)
           (seq-some
            (lambda (pickup)
              (equal (plist-get pickup :delivery-id) delivery-id))
            (pickups service)))
         (wait-for-input-consumed (service delivery-id)
           (let ((deadline (+ (float-time) 5.0))
                 event unresolved-p)
             (while (and (not event) (< (float-time) deadline))
               (setq event (input-consumed-event delivery-id))
               (unless event (accept-process-output nil 0.01)))
             (when event
               (setq unresolved-p (pickup-unresolved-p service delivery-id))
               (while (and unresolved-p (< (float-time) deadline))
                 (accept-process-output nil 0.01)
                 (setq unresolved-p
                       (pickup-unresolved-p service delivery-id))))
             (and event (not unresolved-p) event)))
         (wait-for-session-outcome ()
           (let ((deadline (+ (float-time) 5.0)) outcome)
             (while (and (not (plist-get outcome :known-p))
                         (< (float-time) deadline))
               (setq outcome
                     (e-board-sqlite-service-test--await
                      (e-session-async-continuation-outcome
                       store-2 session-id run-id publication-key)))
               (unless (plist-get outcome :known-p)
                 (accept-process-output nil 0.01)))
             outcome))
         (wait-for-board-outcome ()
           (let ((deadline (+ (float-time) 5.0)) projection)
             (while (and (not (eq (plist-get
                                   (plist-get projection :continuation-outcome)
                                   :status)
                                  'done))
                         (< (float-time) deadline))
               (setq projection (run-projection service-2))
               (unless (eq (plist-get
                            (plist-get projection :continuation-outcome)
                            :status)
                           'done)
                 (accept-process-output nil 0.01)))
             projection))
         (wait-for-published (service)
           (let ((deadline (+ (float-time) 5.0)) projection)
             (while (and (not (eq (plist-get
                                   (plist-get projection :continuation) :state)
                                  'published))
                         (< (float-time) deadline))
               (setq projection (run-projection service))
               (unless (eq (plist-get
                            (plist-get projection :continuation) :state)
                           'published)
                 (accept-process-output nil 0.01)))
             projection)))
      (unwind-protect
          (progn
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-admit-session-owner-start
              service-1 session-id board-id principal
              (plist-get admission :records)
              (plist-get admission :query-delta)
              (list :id participant-id :author "e-chat"
                    :principal principal :controller principal :role 'owner
                    :state 'active :subscription-id "pending-restart-owner"
                    :publication-pending nil)))
            (setq binding-1
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness-1 session-id)))
            (dolist (fact (list manifest report))
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-orchestration-fact-start
                service-1 board-id fact)))
            (e-board-sqlite-service-test--await
             (e-chat-service--publish-sqlite-continuation-claim
              binding-1 run-id publication-key 1 'pending))
            (should (= (length (continuation-inputs service-1)) 0))
            (should (eq (plist-get (plist-get (run-projection service-1)
                                               :continuation)
                                   :state)
                        'pending))
            (e-chat-service--retire-binding binding-1)
            (setq binding-1 nil)
            (e-session-sqlite-store-close store-1)
            (setq store-1 nil)

            (setq store-2 (e-session-sqlite-store-create directory)
                  service-2
                  (e-board-sqlite-service-create
                   (e-session-storage-runtime-store store-2))
                  harness-2
                  (e-harness-create :sessions store-2 :backend busy-backend))
            (setq activity-subscription
                  (e-harness-activity-subscribe
                   harness-2
                   (lambda (event)
                     (when (eq (plist-get event :type) 'input-consumed)
                       (push (copy-tree event t) activity-events)))
                   :session-id session-id))
            ;; Bind without continuation ownership so the restarted owner can
            ;; accumulate ordinary queued input before retrying the pending run.
            (setq binding-2
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness-2 session-id)))
            (e-board-sqlite-service-test--await
             (e-chat-service-submit-session harness-2 session-id busy-prompt))
            (wait-for-backend-starts 1)
            (should (= backend-start-count 1))
            (should (equal (plist-get (last-backend-user-message 1) :content)
                           busy-prompt))
            (should (e-chat-service-active-turn-p harness-2 session-id))
            (e-board-sqlite-service-test--await
             (e-chat-service-queue-session harness-2 session-id user-prompt))
            (let ((queued (wait-for-queue-size harness-2 1)))
              (should (= (length queued) 1))
              (should (equal (plist-get (car queued) :prompt) user-prompt)))

            (e-board-sqlite-service-test--await
             (e-chat-service-binding-start harness-2 session-id nil t))
            (let ((projection (wait-for-published service-2)))
              (should (eq (plist-get (plist-get projection :continuation) :state)
                          'published)))
            (let ((queued (wait-for-queue-size harness-2 1))
                  (pending (pickups service-2)))
              (should (= (length queued) 1))
              (should (equal (plist-get (car queued) :prompt) user-prompt))
              (should (= (length pending) 2))
              (setq user-delivery-id
                    (plist-get (car pending) :delivery-id)
                    continuation-delivery-id
                    (plist-get (cadr pending) :delivery-id))
              (should (eq (plist-get (car pending) :state) 'claimed))
              (should (eq (plist-get (cadr pending) :state) 'pending))
              (should (< (plist-get (car pending) :fifo-position)
                         (plist-get (cadr pending) :fifo-position)))
              (should
               (equal
                (plist-get (plist-get (plist-get (cadr pending) :cause-metadata)
                                      :input-attributes)
                           :board-continuation-key)
                publication-key)))
            (should (= (length (continuation-inputs service-2)) 1))
            ;; Another owner query after publication must not create a second
            ;; keyed input even though both queue items still await the busy turn.
            (e-chat-service--reconcile-sqlite-continuation binding-2)
            (let ((projection (wait-for-published service-2)))
              (should (eq (plist-get (plist-get projection :continuation) :state)
                          'published)))
            (should (= (length (continuation-inputs service-2)) 1))

            ;; Prove FIFO at the actual provider boundary: the older user
            ;; request must start before the durable completion input.
            (finish-backend-start 1 "Busy turn complete.")
            (wait-for-backend-starts 2)
            (should (= backend-start-count 2))
            (should (equal (plist-get (last-backend-user-message 2) :content)
                           user-prompt))
            (should (wait-for-input-consumed service-2 user-delivery-id))
            (should-not (input-consumed-event continuation-delivery-id))
            (should (pickup-unresolved-p service-2 continuation-delivery-id))
            (should-not (plist-get (run-projection service-2)
                                   :continuation-outcome))

            (finish-backend-start 2 "User input handled.")
            (wait-for-backend-starts 3)
            (should (= backend-start-count 3))
            (let ((completion-message (last-backend-user-message 3)))
              (should (string-prefix-p "Finalize after restore."
                                       (plist-get completion-message :content)))
              (should (equal (plist-get (plist-get completion-message :metadata)
                                        :board-continuation-key)
                             publication-key)))
            (let ((receipt
                   (wait-for-input-consumed service-2 continuation-delivery-id)))
              (should (eq (plist-get receipt :type) 'input-consumed))
              (should (equal (plist-get (plist-get receipt :payload) :delivery-id)
                             continuation-delivery-id)))
            ;; The input-consumed receipt says the input started; only the
            ;; later turn-finished event writes its coordinator outcome.
            (let ((outcome
                   (e-board-sqlite-service-test--await
                    (e-session-async-continuation-outcome
                     store-2 session-id run-id publication-key))))
              (should-not (plist-get outcome :known-p)))
            (should-not (plist-get (run-projection service-2)
                                   :continuation-outcome))
            (should (= (length (continuation-inputs service-2)) 1))

            (finish-backend-start 3 "Coordinator complete.")
            (let* ((session-outcome (wait-for-session-outcome))
                   (projection (wait-for-board-outcome))
                   (outcome (plist-get projection :continuation-outcome)))
              (should (plist-get session-outcome :known-p))
              (should (eq (plist-get session-outcome :status) 'done))
              (should (eq (plist-get outcome :status) 'done))
              (should (= (length (plist-get projection :continuation-outcomes))
                         1)))
            (should-not (e-chat-service-active-turn-p harness-2 session-id))
            (should (= backend-start-count 3))
            (should (= (length (continuation-inputs service-2)) 1)))
        (when binding-2 (e-chat-service--retire-binding binding-2))
        (when binding-1 (e-chat-service--retire-binding binding-1))
        (when (and harness-2 activity-subscription)
          (e-harness-activity-unsubscribe harness-2 activity-subscription))
        (dolist (store (list store-2 store-1))
          (when store (ignore-errors (e-session-sqlite-store-close store))))
        (delete-directory directory t)))))

(ert-deftest e-chat-service-sqlite-subscription-crosses-held-snapshot-once ()
  "A commit crossing the initial DB window is neither lost nor duplicated."
  (let* ((stall-directory (make-temp-file "e-chat-sql-view-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         (hold (expand-file-name "board-visible-window.hold" stall-directory))
         (ready (expand-file-name "board-visible-window.ready" stall-directory))
         (release
          (expand-file-name "board-visible-window.release" stall-directory)))
    (unwind-protect
        (progn
          (write-region "hold" nil hold nil 'silent)
          (e-board-sqlite-service-test--with-fixture
              (store service board-id session-id _participant-id)
            (let* ((harness (e-harness-create :sessions store))
                   (binding
                    (e-board-sqlite-service-test--await
                     (e-chat-service-binding-start harness session-id)))
                   events subscription)
              (unwind-protect
                  (progn
                    (setq subscription
                          (e-chat-service-subscribe
                           harness session-id
                           (lambda (event)
                             (when (eq (plist-get event :type) 'message-added)
                               (push (copy-tree event t) events)))))
                    (let ((deadline (+ (float-time) 2.0)))
                      (while (and (not (file-exists-p ready))
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.01))
                      (should (file-exists-p ready)))
                    (e-board-sqlite-service-test--await
                     (e-board-sqlite-service-append-route-start
                      service board-id :content "crossing" :tags '(main)
                      :source-input-key '("subscription-cut" 1)))
                    (write-region "release" nil release nil 'silent)
                    (let ((deadline (+ (float-time) 3.0)))
                      (while (and (< (length events) 1)
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.01)))
                    (e-board-sqlite-service-test--await
                     (e-board-sqlite-service-append-route-start
                      service board-id :content "after-cut" :tags '(main)
                      :source-input-key '("subscription-cut" 2)))
                    (let ((deadline (+ (float-time) 3.0)))
                      (while (and (< (length events) 2)
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.01)))
                    (let ((contents
                           (mapcar
                            (lambda (event)
                              (plist-get
                               (plist-get (plist-get event :payload) :message)
                               :content))
                            events)))
                      (should (= (cl-count "crossing" contents :test #'equal) 1))
                      (should (= (cl-count "after-cut" contents :test #'equal) 1))))
                (when subscription
                  (e-chat-service-unsubscribe subscription))
                (when (e-chat-service-binding harness session-id)
                  (e-chat-service--retire-binding binding))))))
      (write-region "release" nil release nil 'silent)
      (delete-directory stall-directory t))))

(ert-deftest e-chat-service-sqlite-pending-owner-binding-admits-without-input ()
  "A passive new owner binds asynchronously without a synthetic message."
  (let* ((directory (make-temp-file "e-chat-owner-admit-" t))
         (stall-directory (make-temp-file "e-chat-owner-stall-" t))
         (process-environment
          (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY=" stall-directory)
                process-environment))
         (hold
          (expand-file-name "chat-session-owner-admit.hold" stall-directory))
         (ready
          (expand-file-name "chat-session-owner-admit.ready" stall-directory))
         (release
          (expand-file-name "chat-session-owner-admit.release" stall-directory))
         (store (e-session-sqlite-store-create directory))
         (harness (e-harness-create :sessions store))
         (session-id "passive-daily-owner")
         creation ordinary-bind owner-bind binding)
    (unwind-protect
        (progn
          (write-region "hold" nil hold nil 'silent)
          (setq creation
                (e-chat-service-create-session-start
                 :harness harness :id session-id
                 :metadata '(:name "Passive Daily owner")))
          ;; `e-chat-open' may already have requested an ordinary binding.
          ;; Promoting the same in-flight request to continuation owner must
          ;; reuse it and must not wait in the caller.
          (setq ordinary-bind
                (e-chat-service-binding-start harness session-id))
          (setq owner-bind
                (e-chat-service-binding-start harness session-id nil t))
          (should (eq ordinary-bind owner-bind))
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (not (file-exists-p ready))
                        (< (float-time) deadline))
              (accept-process-output nil 0.01))
            (should (file-exists-p ready)))
          (should (eq (plist-get (e-work-status owner-bind) :state) 'started))
          (should (eq (plist-get (e-work-status creation) :state) 'started))
          (should-not (e-chat-service-binding harness session-id))
          (write-region "release" nil release nil 'silent)
          (setq binding (e-board-sqlite-service-test--await owner-bind))
          (should (eq (plist-get (e-work-status creation) :state) 'finished))
          (should (eq binding (e-chat-service-binding harness session-id)))
          (should (e-chat-service-binding-continuation-owner-p binding))
          (let ((association
                 (e-runtime-store-call
                  (e-session-storage-runtime-store store) 'read
                  (list :op 'session-board-association
                        :session-id session-id))))
            (should (equal (plist-get association :association-role) "owner"))
            (should
             (equal (plist-get association :board-id)
                    (e-chat-service-binding-board-id binding))))
          (let ((page
                 (e-board-sqlite-service-test--await
                  (e-board-sqlite-service-record-page-start
                   (e-chat-service-binding-sqlite-service binding)
                   (e-chat-service-binding-board-id binding)
                   :generation 1 :after 0 :limit 16))))
            (should-not (plist-get page :records))))
      (write-region "release" nil release nil 'silent)
      (when binding (e-chat-service--retire-binding binding))
      (ignore-errors (e-session-sqlite-store-close store))
      (delete-directory directory t)
      (delete-directory stall-directory t))))

(ert-deftest e-chat-service-sqlite-new-session-commits-first-input-atomically ()
  "A new SQLite chat remains local until one transaction admits first input."
  (let* ((directory (make-temp-file "e-chat-sql-new-" t))
         (store (e-session-sqlite-store-create directory))
         (session-id "new-sql-chat")
         (harness
          (e-harness-create
           :sessions store
           :backend
           (e-backend-fake-create
            :items '((:type assistant-message :content "new reply")
                     (:type done :reason stop)))))
         creation admission events subscription)
    (unwind-protect
        (progn
          (setq creation
                (e-chat-service-create-session-start
                 :harness harness :id session-id :metadata '(:name "New")))
          (should (eq (plist-get (e-work-status creation) :state) 'started))
          (should
           (gethash session-id
                    (e-chat-service--harness-pending-creations harness)))
          (should-not
           (e-runtime-store-call
            (e-session-storage-runtime-store store) 'read
            (list :op 'session-board-association :session-id session-id)))
          (setq admission
                (e-chat-service-submit-session harness session-id "first"))
          (should (e-work-handle-p admission))
          (let ((result (e-board-sqlite-service-test--await admission)))
            (should (plist-get result :association)))
          (should-not
           (gethash session-id
                    (e-chat-service--harness-pending-creations harness)))
          (let ((created (e-board-sqlite-service-test--await creation)))
            (should (equal (plist-get created :id) session-id)))
          (let ((binding (e-chat-service-binding harness session-id)))
            (should (e-chat-service--sql-binding-p binding))
            (setq subscription
                  (e-chat-service-subscribe
                   harness session-id
                   (lambda (event) (push (copy-tree event t) events))))
            ;; The user event was committed before this deliberately late
            ;; subscriber.  Its canonical row remains queryable; no callback
            ;; timing is treated as storage visibility.
            (let* ((board-id (e-chat-service-binding-board-id binding))
                   (page
                    (e-board-sqlite-service-test--await
                     (e-board-sqlite-service-record-page-start
                      (e-chat-service-binding-sqlite-service binding)
                      board-id :generation 1 :after 0 :limit 16))))
              (should
               (equal
                (plist-get
                 (plist-get (car (plist-get page :records)) :record)
                 :content)
                "first"))))
          (let ((association
                 (e-runtime-store-call
                  (e-session-storage-runtime-store store) 'read
                  (list :op 'session-board-association
                        :session-id session-id))))
            (should (equal (plist-get association :association-role) "owner"))
            (should (stringp
                     (plist-get (plist-get association :routing-policy)
                                :participant-id))))
          (let ((deadline (+ (float-time) 3.0)))
            (while (and
                    (not (seq-find
                          (lambda (event)
                            (eq (plist-get event :type) 'turn-finished))
                          events))
                    (< (float-time) deadline))
              (accept-process-output nil 0.01))))
      (when subscription (e-chat-service-unsubscribe subscription))
      (when-let* ((binding (e-chat-service-binding harness session-id)))
        (e-chat-service--retire-binding binding))
      (ignore-errors (e-session-sqlite-store-close store))
      (delete-directory directory t))))

(ert-deftest e-chat-service-sqlite-context-consumption-is-private-unless-curated ()
  "SQL chat drops raw lifetime audit and durably projects safe curation once."
  (e-board-sqlite-service-test--with-fixture
      (store service board-id session-id _participant-id)
    (let* ((harness (e-harness-create :sessions store))
           (binding
            (e-board-sqlite-service-test--await
             (e-chat-service-binding-start harness session-id)))
           events
           (subscription
            (e-chat-service-subscribe
             harness session-id
             (lambda (event) (push (copy-tree event t) events)))))
      (unwind-protect
          (progn
            (e-chat-service--sql-harness-event
             binding
             (e-events-make
              :type 'context-frame-consumed :session-id session-id
              :turn-id "turn-private"
              :payload '(:frame-id "private-frame"
                         :consumer-request-id "private-consumer"
                         :response-entry-id "private-response")))
            (e-chat-service--sql-harness-event
             binding
             (e-events-make
              :type 'private-lifetime-audit :session-id session-id
              :turn-id "turn-private"
              :payload '(:secret "private-audit")))
            (accept-process-output nil 0.05)
            (should-not events)
            (e-chat-service--sql-harness-event
             binding
             (e-events-make
              :type 'hook-audit :session-id session-id
              :turn-id "turn-hook"
              :payload '(:owner capability :hook-id validate
                         :outcome passed :summary "Validated"
                         :private-capability-state "private-hook-state")))
            (should (= (length events) 1))
            (should
             (equal (plist-get (car events) :payload)
                    '(:owner capability :hook-id validate
                      :outcome passed :summary "Validated")))
            (should-not
             (string-match-p "private-hook-state"
                             (prin1-to-string (car events))))
            (setq events nil)
            (let ((event
                   (e-events-make
                    :type 'context-frame-consumed :session-id session-id
                    :turn-id "turn-curated"
                    :activity-entry-id "private-event"
                    :board-activity-sequence 9
                    :payload
                    '(:frame-id "private-frame"
                      :consumer-request-id "private-consumer"
                      :response-entry-id "private-response"
                      :curation
                      (:kept-source-count 1
                       :summary-count 1
                       :summarized-source-count 1
                       :erased-source-count 0
                       :source-stubs
                       ((:disposition kept
                         :source-kind "dynamic-context")
                        (:disposition summarized
                         :source-kind "tool-result"
                         :tool-name "inspect")))))))
              (e-chat-service--sql-harness-event binding event)
              (e-chat-service--sql-harness-event binding event))
            (let ((deadline (+ (float-time) 3.0)))
              (while (and (not (seq-find
                                (lambda (event)
                                  (eq (plist-get event :type)
                                      'context-curated))
                                events))
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (let* ((curated
                    (seq-find
                     (lambda (event)
                       (eq (plist-get event :type) 'context-curated))
                     events))
                   (printed (prin1-to-string curated))
                   (page
                    (e-board-sqlite-service-test--await
                     (e-board-sqlite-service-record-page-start
                      service board-id :generation 1 :after 0 :limit 16
                      :selector '(:kinds (activity)))))
                   (record
                    (plist-get (car (plist-get page :records)) :record)))
              (should curated)
              (should (= (cl-count 'context-curated events
                                   :key (lambda (event)
                                          (plist-get event :type)))
                         1))
              (should (equal (plist-get curated :payload)
                             (plist-get record :attributes)))
              (should (eq (plist-get record :activity-kind)
                          'context-curated))
              (should-not
               (string-match-p
                "private-frame\\|private-consumer\\|private-response\\|private-event\\|private-audit"
                (concat printed (prin1-to-string record))))))
        (e-chat-service-unsubscribe subscription)
        (e-chat-service--retire-binding binding)))))

(ert-deftest e-chat-service-sqlite-last-subscriber-retires-without-board-object ()
  "Zero-delay SQL idle cleanup retires only coordination-owned live state."
  (e-board-sqlite-service-test--with-fixture
      (store _service board-id session-id _participant-id)
    (let* ((harness (e-harness-create :sessions store))
           (binding
            (e-board-sqlite-service-test--await
             (e-chat-service-binding-start harness session-id)))
           (subscription (e-chat-service-subscribe harness session-id #'ignore))
           (e-chat-service-idle-close-delay 0))
      (should (e-board-sqlite-service-p
               (e-chat-service-binding-sqlite-service binding)))
      (e-chat-service-unsubscribe subscription)
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (e-chat-service-binding harness session-id)
                    (< (float-time) deadline))
          (sit-for 0.01)))
      (should-not (e-chat-service-binding harness session-id))
      (should-not (e-chat-service--board-bindings-for binding))
      (should (eq (e-chat-service-binding-lifecycle-state binding) 'retired))
      (should-not (e-chat-service-binding-idle-close-timer binding))
      (should-not (e-chat-service-binding-activity-subscription binding))
      (should-not (e-chat-service-subscription-sqlite-query-work subscription))
      (should-not (e-chat-service-subscription-drain-timer subscription)))))

(ert-deftest e-chat-service-live-board-coordination-is-runtime-scoped ()
  "Equal Board ids in two databases never share close or continuation state."
  (let ((e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings
         (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--continuation-reconciling
         (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--continuation-admissions
         (make-hash-table :test 'eq :weakness 'key))
        (spec
         (e-work-spec-create
          :id "runtime-scoped-reconcile" :execution 'cooperative
          :interactive-policy 'async :owner 'e-chat-service-runtime-scope-test
          :runner (lambda (_handle _arguments _context) :deferred))))
    (e-board-sqlite-service-test--with-fixture
        (store-a service-a board-a session-a _participant-a)
      (e-board-sqlite-service-test--with-fixture
          (store-b service-b board-b session-b _participant-b)
        (let* ((runtime-a (e-board-sqlite-service-runtime service-a))
               (runtime-b (e-board-sqlite-service-runtime service-b))
               (harness-a (e-harness-create :sessions store-a))
               (harness-b (e-harness-create :sessions store-b))
               (binding-a
                (e-board-sqlite-service-test--await
                 (e-chat-service-binding-start harness-a session-a)))
               (binding-b
                (e-board-sqlite-service-test--await
                 (e-chat-service-binding-start harness-b session-b)))
               reconciliation-works)
          (unwind-protect
              (progn
                (should (equal board-a board-b))
                (should-not (eq runtime-a runtime-b))
                (should (equal (e-chat-service--board-bindings-for binding-a)
                               (list binding-a)))
                (should (equal (e-chat-service--board-bindings-for binding-b)
                               (list binding-b)))
                (cl-letf (((symbol-function
                            'e-chat-service--continuation-index-scan-start)
                           (lambda (_binding _page-function)
                             (let ((work (e-work-start spec nil)))
                               (push work reconciliation-works)
                               work))))
                  (e-chat-service--reconcile-sqlite-continuation binding-a)
                  (e-chat-service--reconcile-sqlite-continuation binding-b)
                  ;; A second request in one runtime is coalesced, while the
                  ;; equal Board id in the other runtime remains independent.
                  (e-chat-service--reconcile-sqlite-continuation binding-a)
                  (should (= (length reconciliation-works) 2))
                  (should (= (hash-table-count
                              e-chat-service--continuation-reconciling)
                             2))
                  ;; The second runtime-A request records a durable edge while
                  ;; its first query is active.  Settling that query launches
                  ;; exactly one follow-up query rather than losing the edge.
                  (e-work-finish (car (last reconciliation-works)) nil)
                  (should (= (length reconciliation-works) 3))
                  (e-work-finish (car reconciliation-works) nil)
                  (e-chat-service-close-board binding-a)
                  (should-not
                   (e-chat-service--runtime-coordination-table
                    e-chat-service--board-bindings runtime-a))
                  (should (e-chat-service-binding harness-b session-b))
                  (should (equal
                           (e-chat-service--board-bindings-for binding-b)
                           (list binding-b)))
                  (dolist (work reconciliation-works)
                    (unless (memq (plist-get (e-work-status work) :state)
                                  '(finished failed cancelled))
                      (e-work-finish work nil)))
                  (should (zerop
                           (hash-table-count
                            e-chat-service--continuation-reconciling)))))
            (when (e-chat-service--binding-live-p binding-a)
              (e-chat-service-close-board binding-a))
            (when (e-chat-service--binding-live-p binding-b)
              (e-chat-service-close-board binding-b))))))))

(ert-deftest e-board-sqlite-service-orchestration-uses-board-id-not-board-replica ()
  "SQL orchestration publishes and queries detached facts by Board identity."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (let ((manifest
           '(:version 1 :type manifest :idempotency-key "manifest:sql-run"
             :payload (:run-id "sql-run"
                       :tasks [(:task-key "task" :required t
                                :accepted-attempt 0)]
                       :deadline (:kind none)))))
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-orchestration-fact-start
        service board-id manifest :author '(:session-id "owner")))
      (let* ((page
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-orchestration-run-start
                service board-id "sql-run")))
             (records (mapcar (lambda (row) (plist-get row :record))
                              (plist-get page :records)))
             (projection (e-board-orchestration-reduce records)))
        (should-not (plist-get page :truncated))
        (should (equal (plist-get projection :run-id) "sql-run"))
        (should (= (length (plist-get projection :tasks)) 1))))))

(ert-deftest e-board-sqlite-service-run-index-keeps-old-active-run-visible ()
  "Completed history does not hide an older active run, including duplicates."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (let ((original
           (e-board-sqlite-service-test--append-index-run
            service board-id "older-active")))
      (dotimes (index 33)
        (e-board-sqlite-service-test--append-index-run
         service board-id (format "completed-%02d" index) t))
      (let* ((duplicate
              (e-board-sqlite-service-test--append-index-run
               service board-id "older-active"))
             (active
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-orchestration-active-runs-start
                service board-id 32 1700000000)))
             (all-runs
              (e-board-sqlite-service-test--await
               (e-board-sqlite-service-orchestration-run-index-page-start
                service board-id :limit 64))))
        (should (eq (plist-get original :status) 'posted))
        (should (eq (plist-get duplicate :status) 'duplicate))
        (should (= (plist-get active :active-count) 1))
        (should-not (plist-get active :more-p))
        (should (equal (mapcar (lambda (run) (plist-get run :run-id))
                               (plist-get active :runs))
                       '("older-active")))
        (should (= (length (plist-get all-runs :entries)) 34))
        (should (= (cl-count-if (lambda (entry)
                                  (plist-get entry :active-p))
                                (plist-get all-runs :entries))
                   1))))))

(ert-deftest e-board-sqlite-service-run-index-pages-more-than-256-active-runs ()
  "Current-generation index cursors include runs beyond the presentation cap."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (dotimes (index 257)
      (e-board-sqlite-service-test--append-index-run
       service board-id (format "active-%03d" index)))
    (let* ((active
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-orchestration-active-runs-start
              service board-id 256 1700000000)))
           (first
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-orchestration-run-index-page-start
              service board-id :active-only t :limit 256)))
           (second
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-orchestration-run-index-page-start
              service board-id :active-only t :limit 256
              :cursor (plist-get first :next-cursor))))
           (ids (append (mapcar (lambda (entry) (plist-get entry :run-id))
                                (plist-get first :entries))
                        (mapcar (lambda (entry) (plist-get entry :run-id))
                                (plist-get second :entries)))))
      (should (= (plist-get active :active-count) 257))
      (should (plist-get active :more-p))
      (should (= (length (plist-get active :runs)) 256))
      (should (= (length (plist-get first :entries)) 256))
      (should (plist-get first :next-cursor))
      (should (= (length (plist-get second :entries)) 1))
      (should-not (plist-get second :next-cursor))
      (should (= (length (delete-dups ids)) 257))
      (should (equal (car (last ids)) "active-000")))))

(ert-deftest e-board-sqlite-service-run-index-clear-starts-a-new-generation ()
  "Board clear retires the run index with its canonical generation."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (e-board-sqlite-service-test--append-index-run
     service board-id "before-clear")
    (let ((before
           (e-board-sqlite-service-test--await
            (e-board-sqlite-service-orchestration-active-runs-start
             service board-id 32 1700000000))))
      (should (= (plist-get before :active-count) 1))
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service--start
        service 'write (list :op 'board-clear :board-id board-id
                             :generation 1)))
      (let ((after
             (e-board-sqlite-service-test--await
              (e-board-sqlite-service-orchestration-active-runs-start
               service board-id 32 1700000000))))
        (should (= (plist-get after :generation) 2))
        (should (= (plist-get after :active-count) 0))
        (should-not (plist-get after :runs))))))

(ert-deftest e-board-sqlite-service-run-index-fact-limit-is-transactional ()
  "The 1024th run fact indexes; the 1025th fact rolls back completely."
  (let* ((directory (make-temp-file "e-board-run-index-limit-" t))
         (database (sqlite-open (expand-file-name "board.sqlite3" directory)))
         (board-id "run-index-limit-board")
         (run-id "run-index-limit")
         (manifest
          (list :version 1 :type 'manifest :idempotency-key "manifest:limit"
                :payload
                (list :run-id run-id
                      :tasks [(:task-key "task" :required t
                               :accepted-attempt 0)]
                      :deadline '(:kind none))))
         (seed-facts
          (cons manifest
                (cl-loop for index from 0 below 1022
                         collect
                         (e-board-sqlite-service-test--index-limit-conflict-fact
                          run-id index))))
         (position 0)
         update-milliseconds)
    (unwind-protect
        (progn
          (e-board-sqlite-worker-initialize database)
          (sqlite-execute
           database
           "INSERT INTO boards(board_id,trusted_principal,generation,revision,next_position,root_payload) VALUES(?,NULL,1,0,0,NULL)"
           (vector board-id))
          (sqlite-execute database "BEGIN IMMEDIATE")
          (condition-case error
              (progn
                (cl-letf (((symbol-function
                            'e-board-sqlite-worker--update-orchestration-run-index)
                           (lambda (&rest _arguments) nil)))
                  (dolist (fact seed-facts)
                    (e-board-sqlite-worker-write
                     database
                     (e-board-sqlite-service-test--orchestration-record-body
                      board-id (format "seed-%04d" (cl-incf position)) fact))))
                (sqlite-execute database "COMMIT"))
            (error
             (ignore-errors (sqlite-execute database "ROLLBACK"))
             (signal (car error) (cdr error))))
          (should (= (length seed-facts) 1023))
          (let ((started (float-time)))
            (e-board-sqlite-service-test--transactional-worker-write
             database
             (e-board-sqlite-service-test--orchestration-record-body
              board-id "boundary-1024"
              (e-board-sqlite-service-test--index-limit-conflict-fact
               run-id 1022)))
            (setq update-milliseconds (* 1000 (- (float-time) started))))
          (let ((count
                 (e-board-sqlite-worker--column
                  (car (sqlite-select
                        database
                        "SELECT COUNT(*) FROM board_records WHERE board_id=? AND record_kind='fact'"
                        (vector board-id)))
                  0))
                (index
                 (car (sqlite-select
                       database
                       (concat
                       "SELECT active,latest_event_position FROM board_orchestration_run_index "
                        "WHERE board_id=? AND generation=1 AND run_id=?")
                       (vector board-id run-id)))))
            (should (= count 1024))
            (should index)
            (should (= (e-board-sqlite-worker--column index 0) 1))
            (should (= (e-board-sqlite-worker--column index 1) 1024)))
          (should-error
           (e-board-sqlite-service-test--transactional-worker-write
            database
            (e-board-sqlite-service-test--orchestration-record-body
             board-id "overflow-1025"
             (e-board-sqlite-service-test--index-limit-conflict-fact
              run-id 1023)))
           :type 'e-runtime-store-worker-error)
          (should
           (= (e-board-sqlite-worker--column
               (car (sqlite-select
                     database
                     "SELECT COUNT(*) FROM board_records WHERE board_id=? AND record_kind='fact'"
                     (vector board-id)))
               0)
              1024))
          (should
           (= (e-board-sqlite-worker--column
               (car (sqlite-select
                     database
                     "SELECT next_position FROM boards WHERE board_id=?"
                     (vector board-id)))
               0)
              1024))
          (let ((index
                 (car (sqlite-select
                       database
                       (concat
                        "SELECT active,latest_event_position FROM board_orchestration_run_index "
                        "WHERE board_id=? AND generation=1 AND run_id=?")
                       (vector board-id run-id)))))
            (should index)
            (should (= (e-board-sqlite-worker--column index 0) 1))
            (should (= (e-board-sqlite-worker--column index 1) 1024)))
          (message "Board run-index 1024-fact append: %.2f ms (%.4f ms/fact)"
                   update-milliseconds (/ update-milliseconds 1024.0)))
      (when database (sqlite-close database))
      (delete-directory directory t))))

(ert-deftest e-board-sqlite-service-addressed-routing-does-not-truncate-at-512 ()
  "An exact addressee and its association remain routable beyond one page."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (dotimes (index 513)
      (let* ((session-id (format "routing-session-%04d" index))
             (participant-id (format "routing-participant-%04d" index))
             (principal (format "chat:%s" session-id))
             (policy
              (list :participant-id participant-id
                    :pickup-selector '(:tags (main))
                    :observer-selector '(:tags (main))
                    :default-tags '(main) :default-to participant-id))
             (session
              (e-board-sqlite-service-session-admission
               :id session-id :metadata nil :principal principal
               :board-id board-id :association-role "participant"
               :routing-policy policy))
             (records (plist-get session :admission-records))
             (position 0))
        (dolist (record records)
          (plist-put record :journal-position (cl-incf position)))
        (e-board-sqlite-service-test--await
         (e-board-sqlite-service-admit-participant-start
          service session-id board-id records
          (plist-get session :query-delta)
          (list :id participant-id :author "e-chat"
                :principal principal :controller principal
                :role 'participant :state 'active
                :subscription-id (format "routing-sub-%04d" index)
                :publication-pending nil)))))
    (let* ((target "routing-participant-0512")
           (result
            (e-board-sqlite-service-test--await
             (e-board-sqlite-service-append-route-start
              service board-id :content "addressed" :tags '(main)
              :to target :source-input-key '(addressed-beyond-page 1))))
           (pickup (car (plist-get result :pickups))))
      (should (= (length (plist-get result :pickups)) 1))
      (should (equal (plist-get pickup :participant-id) target))
      (should (plist-get pickup :addressed-p)))))

(ert-deftest e-board-sqlite-service-commit-observer-wakes-only-while-subscribed ()
  "Committed Board writes wake a bounded consumer until its lease is cancelled."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (let* ((notifications 0)
           (observation
            (e-board-sqlite-service-observe-commits
             service board-id (lambda () (cl-incf notifications)))))
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-record-append-start
        service board-id 'fact 'fact "observer-1"
        :content "first"))
      (should (= notifications 1))
      (should (e-board-sqlite-commit-observation-cancel observation))
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-record-append-start
        service board-id 'fact 'fact "observer-2"
        :content "second"))
      (should (= notifications 1)))))

(ert-deftest e-board-sqlite-service-commit-observer-precedes-write-settlement ()
  "A committed write acknowledges only after Board observers are woken."
  (e-board-sqlite-service-test--with-fixture
      (_store service board-id _session-id _participant-id)
    (let ((observer-seen nil)
          (settlement-seen nil))
      (e-board-sqlite-service-observe-commits
       service board-id (lambda () (setq observer-seen t)))
      (let ((work
             (e-board-sqlite-service-record-append-start
              service board-id 'fact 'fact "observer-before-settlement"
              :content "committed")))
        (e-work-on-settle
         work
         (lambda (_settled)
           (setq settlement-seen t)
           (should observer-seen)))
        (e-board-sqlite-service-test--await work))
      (should observer-seen)
      (should settlement-seen))))

(provide 'e-board-sqlite-service-test)

;;; e-board-sqlite-service-test.el ends here

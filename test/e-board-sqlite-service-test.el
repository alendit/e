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
(require 'e-work)

(defun e-board-sqlite-service-test--await (work)
  "Observe request-scoped WORK from this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

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
      (_store service board-id _session-id _participant-id)
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
           (first-id (plist-get first-pickup :delivery-id)))
      (should (eq (plist-get first-pickup :state) 'ready))
      (should (eq (plist-get second-pickup :state) 'pending))
      (e-board-sqlite-service-test--await
       (e-board-sqlite-service-transition-pickup-start
        service board-id first-id 'claim))
      (let ((consumed
             (e-board-sqlite-service-test--await
              (e-board-sqlite-service-transition-pickup-start
               service board-id first-id 'consume))))
        (should (eq (plist-get (plist-get consumed :pickup) :state)
                    'consumed))
        (should (equal (plist-get (plist-get consumed :next) :delivery-id)
                       (plist-get second-pickup :delivery-id)))
        (should (eq (plist-get (plist-get consumed :next) :state) 'ready))))))

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

(ert-deftest e-chat-service-sqlite-continuation-queues-detached-terminal-view ()
  "One ordinary bounded run query supplies terminal evidence without backfill."
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
           (admission-spec
            (e-work-spec-create
             :id "continuation-admission" :execution 'cheap
             :interactive-policy 'cheap
             :runner (lambda (_arguments _context) '(:admitted t))))
           binding queued-session-id queued-input queued-metadata
           (query-count 0) (publication-count 0) (backfill-count 0))
      (unwind-protect
          (progn
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
              (cl-letf (((symbol-function
                          'e-board-sqlite-service-orchestration-runs-start)
                       (lambda (&rest _)
                         (cl-incf query-count)
                         (e-work-start query-spec nil)))
                      ((symbol-function
                        'e-chat-service--continuation-backfill-start)
                       (lambda (&rest _)
                         (cl-incf backfill-count)))
                      ((symbol-function 'e-chat-service-queue-session)
                       (lambda (_harness session-id input &rest arguments)
                         (setq queued-session-id session-id
                               queued-input input
                               queued-metadata
                               (plist-get arguments :metadata))
                         (e-work-start admission-spec nil)))
                      ((symbol-function
                        'e-chat-service--publish-sqlite-continuation-claim)
                       (lambda (&rest _) (cl-incf publication-count))))
              (e-chat-service--reconcile-sqlite-continuation binding))
            (should (= query-count 1))
            (should (= backfill-count 0))
            (should (= publication-count 1))
            (should (equal queued-session-id "coordinator-session"))
            (should (eq (plist-get queued-metadata :display) 'hidden))
            (should (equal (plist-get queued-metadata :board-run-id)
                           "daily-run"))
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
           binding admission claim-work (queue-count 0))
      (unwind-protect
          (progn
            (setq binding
                  (e-board-sqlite-service-test--await
                   (e-chat-service-binding-start harness session-id)))
            (cl-letf (((symbol-function
                        'e-board-sqlite-service-orchestration-runs-start)
                       (lambda (&rest _) (e-work-start query-spec nil)))
                      ((symbol-function 'e-chat-service-queue-session)
                       (lambda (&rest _)
                         (cl-incf queue-count)
                         (setq admission
                               (e-board-sqlite-service-test--deferred-work
                                "held-continuation-admission"))))
                      ((symbol-function
                        'e-chat-service--publish-sqlite-continuation-claim)
                       (lambda (&rest _)
                         (setq claim-work
                               (e-board-sqlite-service-test--deferred-work
                                "held-continuation-claim")))))
              (e-chat-service--reconcile-sqlite-continuation binding)
              (should (= queue-count 1))
              ;; An in-flight admission fences another stale terminal query.
              (e-chat-service--reconcile-sqlite-continuation binding)
              (should (= queue-count 1))
              (e-work-finish admission '(:admitted t))
              (should (e-work-handle-p claim-work))
              ;; The claim write itself owns the fence.
              (e-chat-service--reconcile-sqlite-continuation binding)
              (should (= queue-count 1))
              (e-work-finish claim-work '(:published t))
              ;; Even after write settlement, a query that began before the
              ;; commit became visible must not requeue the continuation.
              (e-chat-service--reconcile-sqlite-continuation binding)
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
              (let* ((runtime (e-chat-service--binding-runtime binding))
                     (admissions
                      (e-chat-service--runtime-coordination-table
                       e-chat-service--continuation-admissions runtime)))
                (should-not
                 (and admissions
                      (gethash (cons board-id "continue:daily") admissions))))))
        (when binding (e-chat-service--retire-binding binding))))))

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
                            'e-board-sqlite-service-orchestration-runs-start)
                           (lambda (_service _board-id &optional _limit)
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
                  (e-work-finish (car (last reconciliation-works))
                                 '(:records nil :truncated nil))
                  (should (= (length reconciliation-works) 3))
                  (e-work-finish (car reconciliation-works)
                                 '(:records nil :truncated nil))
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
                      (e-work-finish work '(:records nil :truncated nil))))
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

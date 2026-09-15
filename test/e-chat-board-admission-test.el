;;; e-chat-board-admission-test.el --- Board-first owner admission tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-backend)
(require 'e-board)
(require 'e-board-orchestration)
(require 'e-board-sqlite-service)
(require 'e-chat-service)
(require 'e-harness)
(require 'e-session-storage-sqlite)
(require 'e-work)

(defun e-chat-board-admission-test--await (work)
  "Observe WORK at this explicit test boundary."
  (e-work-with-batch-await
    (e-work-await-batch work :timeout 5.0)))

(defun e-chat-board-admission-test--wait (predicate)
  "Wait at this explicit test boundary for PREDICATE."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun e-chat-board-admission-test--held-work (id)
  "Return a started cooperative work handle held by the test until settled."
  (e-work-start
   (e-work-spec-create
    :id id :execution 'cooperative :interactive-policy 'async
    :owner 'e-chat-board-admission-test
    :runner (lambda (_handle _arguments _context) :deferred))
   nil))

(defun e-chat-board-admission-test--stall-file (directory operation suffix)
  "Return one disposable worker stall marker path."
  (expand-file-name (format "%s.%s" operation suffix) directory))

(defun e-chat-board-admission-test--owner-admission
    (session-id board-id participant-id)
  "Return one detached owner admission fixture."
  (let* ((policy (list :participant-id participant-id
                       :pickup-selector '(:tags (main))
                       :observer-selector '(:tags (main))
                       :default-tags '(main) :default-to nil))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id :metadata (list :name session-id)
           :principal (format "chat:%s" session-id)
           :board-id board-id :association-role "owner"
           :routing-policy policy))
         (position 0))
    (list :records
          (mapcar
           (lambda (record)
             (let ((copy (copy-tree record t)))
               (plist-put copy :journal-position (cl-incf position))
               copy))
           (plist-get session :admission-records))
          :query-delta (plist-get session :query-delta)
          :policy policy)))

(defun e-chat-board-admission-test--admit-association
    (service session-id board-id principal participant-id role)
  "Admit one disposable owner or PARTICIPANT association."
  (let* ((policy (list :participant-id participant-id
                       :pickup-selector '(:tags (main))
                       :observer-selector '(:tags (main))
                       :default-tags '(main) :default-to nil))
         (session
          (e-board-sqlite-service-session-admission
           :id session-id :metadata (list :name session-id)
           :principal principal :board-id board-id
           :association-role (if (eq role 'owner) "owner" "participant")
           :routing-policy policy))
         (position 0)
         (records
          (mapcar
           (lambda (record)
             (let ((copy (copy-tree record t)))
               (plist-put copy :journal-position (cl-incf position))
               copy))
           (plist-get session :admission-records)))
         (participant
          (list :id participant-id :author "e-chat"
                :principal principal :controller principal
                :role role :state 'active
                :subscription-id (format "sub-%s" participant-id)
                :publication-pending nil)))
    (e-chat-board-admission-test--await
     (if (eq role 'owner)
         (e-board-sqlite-service-admit-session-owner-start
          service session-id board-id principal records
          (plist-get session :query-delta) participant)
       (e-board-sqlite-service-admit-participant-start
        service session-id board-id records
        (plist-get session :query-delta) participant)))))

(cl-defmacro e-chat-board-admission-test--with-fixture ((store harness) &rest body)
  "Run BODY with a disposable SQLite STORE and HARNESS."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((directory (make-temp-file "e-chat-board-admission-" t))
          (,store (e-session-sqlite-store-create directory :asynchronous t))
          (,harness
           (e-harness-create
            :sessions ,store
            :backend (e-backend-fake-create :items nil))))
     (unwind-protect
         (progn ,@body)
       (when (gethash ,harness e-chat-service--bindings)
         (maphash (lambda (_session-id binding)
                    (ignore-errors (e-chat-service--retire-binding binding)))
                  (gethash ,harness e-chat-service--bindings)))
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory directory t))))

(cl-defmacro e-chat-board-admission-test--with-stalled-fixture
    ((store harness stall-directory) &rest body)
  "Run BODY with a disposable SQLite STORE and a worker stall directory."
  (declare (indent 1) (debug ((symbolp symbolp symbolp) body)))
  `(let* ((directory (make-temp-file "e-chat-board-admission-" t))
          (,stall-directory (make-temp-file
                             "e-chat-board-admission-stall-" t))
          (process-environment
           (cons (concat "E_RUNTIME_STORE_TEST_STALL_DIRECTORY="
                         ,stall-directory)
                 process-environment))
          (,store (e-session-sqlite-store-create directory :asynchronous t))
          (,harness
           (e-harness-create
            :sessions ,store
            :backend (e-backend-fake-create :items nil))))
     (unwind-protect
         (progn ,@body)
       (when (gethash ,harness e-chat-service--bindings)
         (maphash (lambda (_session-id binding)
                    (ignore-errors (e-chat-service--retire-binding binding)))
                  (gethash ,harness e-chat-service--bindings)))
       (ignore-errors (e-session-sqlite-store-close ,store))
       (delete-directory ,stall-directory t)
       (delete-directory directory t))))

(ert-deftest e-chat-board-admission-test-stable-key-immediate-id-and-exact-open ()
  "Stable owner admission shares work and opens one exact Board owner."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((first
            (e-chat-service-owner-admission-start
             :harness harness :creation-key "org:daily:stable"
             :metadata '(:name "Stable Daily")))
           (second
            (e-chat-service-owner-admission-start
             :harness harness :creation-key "org:daily:stable"
             :metadata '(:name "Stable Daily"))))
      (should (string-prefix-p "brd_"
                               (e-chat-service-owner-admission-provisional-board-id
                                first)))
      (should (equal
               (e-chat-service-owner-admission-provisional-board-id first)
               (e-chat-service-owner-admission-provisional-board-id second)))
      (should (eq (e-chat-service-owner-admission-work first)
                  (e-chat-service-owner-admission-work second)))
      (let* ((admitted
              (e-chat-board-admission-test--await
               (e-chat-service-owner-admission-work first)))
             (opened
              (e-chat-board-admission-test--await
               (e-chat-service-open-board-owner-start
                (e-chat-service-owner-admission-provisional-board-id first)
                harness)))
        (should (equal (plist-get admitted :board-id)
                       (e-chat-service-binding-board-id opened)))
        (should (equal (plist-get (plist-get admitted :association)
                                  :session-id)
                       (e-chat-service-binding-session-id opened)))
        (should (equal (plist-get (plist-get admitted :association)
                                  :participant-id)
                       (e-chat-service-binding-participant-id opened)))
        (should (eq opened
                    (e-chat-board-admission-test--await
                     (e-chat-service-open-board-owner-start
                      (e-chat-service-owner-admission-provisional-board-id first)
                      harness)))))))))

(ert-deftest e-chat-board-admission-test-legacy-resolver-is-read-only ()
  "Legacy session resolution validates the exact owner without binding it."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((session-id "legacy-owner-session")
           (board-id "legacy-owner-board")
           (participant-id "legacy-owner-participant")
           (admission
            (e-chat-board-admission-test--owner-admission
             session-id board-id participant-id))
           (service (e-board-sqlite-service-create
                     (e-session-storage-runtime-store store))))
      (e-chat-board-admission-test--await
       (e-board-sqlite-service-admit-session-owner-start
        service session-id board-id (format "chat:%s" session-id)
        (plist-get admission :records) (plist-get admission :query-delta)
        (list :id participant-id :author "e-chat"
              :principal (format "chat:%s" session-id)
              :controller (format "chat:%s" session-id)
              :role 'owner :state 'active
              :subscription-id "pickup-main" :publication-pending nil)))
      (let ((resolved
             (e-chat-board-admission-test--await
              (e-chat-service-resolve-legacy-session-start
               harness session-id))))
        (should (equal (plist-get resolved :board-id) board-id))
        (should (equal (plist-get (plist-get resolved :association) :session-id)
                       session-id))
        (should-not (e-chat-service-binding harness session-id))))))

(ert-deftest e-chat-board-admission-test-open-settles-board-run-readiness ()
  "An exact owner open waits for the initial bounded run-set projection."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((ticket
            (e-chat-service-owner-admission-start
             :harness harness :creation-key "org:daily:run-set"
             :metadata '(:name "Run-set Daily")))
           (admitted
            (e-chat-board-admission-test--await
             (e-chat-service-owner-admission-work ticket)))
           (binding
            (e-chat-board-admission-test--await
             (e-chat-service-open-board-owner-start
              (plist-get admitted :board-id) harness)))
           (state (e-board-run-set-state-for
                   harness
                   (e-chat-service-binding-session-id binding)
                   (e-chat-service-binding-board-id binding)))
           (value (e-board-orchestration-run-set-state-value state))
           (target (e-chat-service-publication-target binding)))
      (should (eq (plist-get value :restore-state) 'ready))
      (should (plist-get value :ready-p))
      (should (equal (plist-get value :board-id)
                     (e-chat-service-binding-board-id binding)))
      (should-not (plist-get value :runs))
      (should (gethash binding e-board--run-set-states))
      (e-chat-board-admission-test--await
       (e-board-sqlite-publication-target-orchestration-fact-start
        target
        '(:version 1 :type manifest :idempotency-key "manifest:run-set"
          :payload (:run-id "run-set"
                    :tasks ((:task-key "task" :required t :accepted-attempt 0))
                    :deadline (:kind none)))))
      (e-chat-board-admission-test--wait
       (lambda ()
         (equal
          (plist-get
           (car (plist-get
                 (e-board-orchestration-run-set-state-value state) :runs))
           :run-id)
          "run-set")))
      (should (eq (plist-get (car (plist-get
                                  (e-board-orchestration-run-set-state-value state)
                                  :runs))
                             :lifecycle)
                  'dispatching)))))

(ert-deftest e-chat-board-admission-test-readiness-waits-for-committed-run-set-refresh ()
  "Owner readiness does not settle on a stale projection after a commit."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((initial-query
            (e-chat-board-admission-test--held-work "held-run-set-initial"))
           (refresh-query
            (e-chat-board-admission-test--held-work "held-run-set-refresh"))
           (queries (list initial-query refresh-query))
           admitted association board-id session-id open binding state)
      (cl-letf (((symbol-function 'e-board-orchestration-actions-run-set)
                 (lambda (&rest _arguments)
                   (or (pop queries)
                       (error "unexpected extra Board run-set query")))))
        (setq admitted
              (e-chat-board-admission-test--await
               (e-chat-service-owner-admission-work
                (e-chat-service-owner-admission-start
                 :harness harness :creation-key "org:daily:refresh-order"
                 :metadata '(:name "Refresh ordering Daily")))))
        (setq association (plist-get admitted :association)
              board-id (plist-get admitted :board-id)
              session-id
              (plist-get
               (e-chat-service-owner-admission-identities
                "org:daily:refresh-order")
               :session-id)
              open (e-chat-service-open-board-owner-start board-id harness))
        (e-chat-board-admission-test--wait
         (lambda ()
           (and (e-chat-service-binding harness session-id)
                (= (length queries) 1))))
        (setq binding (e-chat-service-binding harness session-id)
              state (e-board-run-set-state-for harness session-id board-id))
        (should-not (e-request-terminal-p (e-work-handle-lifecycle open)))
        (should (eq (plist-get (e-board-orchestration-run-set-state-value state)
                               :restore-state)
                    'restoring))
        (e-chat-board-admission-test--await
         (e-board-sqlite-publication-target-orchestration-fact-start
          (e-chat-service-publication-target binding)
          '(:version 1 :type manifest :idempotency-key "manifest:refresh-order"
            :payload (:run-id "refresh-run"
                      :tasks ((:task-key "task" :required t :accepted-attempt 0))
                      :deadline (:kind none)))))
        (should (plist-get (e-board--run-set-controls state) :rerun-p))
        (e-work-finish
         initial-query
         (list :board-id board-id :restore-state 'ready :ready-p t
               :status 'idle :runs nil :active-count 0 :active-run-count 0
               :omitted-count 0 :bytes 0))
        (e-chat-board-admission-test--wait
         (lambda () (= (length queries) 0)))
        ;; The first SQL child was a stale snapshot.  Its detached value must
        ;; not settle owner readiness; the observer-driven refresh owns that
        ;; boundary and is still held here.
        (should-not (e-request-terminal-p (e-work-handle-lifecycle open)))
        (should (eq (plist-get (e-board-orchestration-run-set-state-value state)
                               :restore-state)
                    'restoring))
        (e-work-finish
         refresh-query
         (list :board-id board-id :restore-state 'ready :ready-p t
               :status 'running
               :runs (list (list :run-id "refresh-run" :lifecycle 'running))
               :active-count 1 :active-run-count 1 :omitted-count 0 :bytes 128))
        (should (eq (e-chat-board-admission-test--await open) binding))
        (let ((value (e-board-orchestration-run-set-state-value state)))
          (should (eq (plist-get value :restore-state) 'ready))
          (should (equal (plist-get (car (plist-get value :runs)) :run-id)
                         "refresh-run")))))))

(ert-deftest e-chat-board-admission-test-run-set-barrier-waits-for-exact-run ()
  "The Board barrier waits for the post-controller exact run projection."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((initial-query
            (e-chat-board-admission-test--held-work "held-barrier-initial"))
           (refresh-query
            (e-chat-board-admission-test--held-work "held-barrier-refresh"))
           (queries (list initial-query refresh-query))
           admitted board-id session-id open binding state barrier projection)
      (cl-letf (((symbol-function 'e-board-orchestration-actions-run-set)
                 (lambda (&rest _arguments)
                   (or (pop queries)
                       (error "unexpected extra Board run-set query")))))
        (setq admitted
              (e-chat-board-admission-test--await
               (e-chat-service-owner-admission-work
                (e-chat-service-owner-admission-start
                 :harness harness :creation-key "org:daily:barrier"
                 :metadata '(:name "Barrier Daily")))))
        (setq board-id (plist-get admitted :board-id)
              session-id
              (plist-get
               (e-chat-service-owner-admission-identities
                "org:daily:barrier")
               :session-id)
              open (e-chat-service-open-board-owner-start board-id harness))
        (e-chat-board-admission-test--wait
         (lambda ()
           (and (e-chat-service-binding harness session-id)
                (= (length queries) 1))))
        (setq binding (e-chat-service-binding harness session-id)
              state (e-board-run-set-state-for harness session-id board-id))
        ;; The owner is ready on the initial empty projection.  The controller
        ;; may start only after this point; its later durable commit wakes the
        ;; same state and starts the held refresh below.
        (e-work-finish
         initial-query
         (list :board-id board-id :restore-state 'ready :ready-p t
               :status 'idle :runs nil :active-count 0 :active-run-count 0
               :omitted-count 0 :bytes 0))
        (should (e-chat-service-binding-p
                 (e-chat-board-admission-test--await open)))
        (e-chat-board-admission-test--await
         (e-board-sqlite-publication-target-orchestration-fact-start
          (e-chat-service-publication-target binding)
          '(:version 1 :type manifest :idempotency-key "manifest:barrier"
            :payload (:run-id "controller-run"
                      :tasks ((:task-key "task" :required t :accepted-attempt 0))
                      :deadline (:kind none)))))
        (e-chat-board-admission-test--wait
         (lambda () (= (length queries) 0)))
        ;; The controller receipt is now followed by an active observer query;
        ;; construct the barrier while that post-controller refresh is held.
        (setq barrier (e-board-run-set-await-run-start binding "controller-run"))
        (should-not (e-request-terminal-p (e-work-handle-lifecycle barrier)))
        ;; A completed controller work/result cannot make the app ready while
        ;; its observer-visible Board refresh is still held.
        (should-not (e-request-terminal-p (e-work-handle-lifecycle barrier)))
        (setq projection
              (list :board-id board-id :restore-state 'ready :ready-p t
                    :status 'running
                    :runs (list (list :run-id "controller-run"
                                      :lifecycle 'running))
                    :active-count 1 :active-run-count 1
                    :omitted-count 0 :bytes 128))
        (e-work-finish refresh-query projection)
        (let ((barrier-value (e-chat-board-admission-test--await barrier)))
          (should (equal barrier-value
                         (e-board-orchestration-run-set-state-value state)))
          (should (equal barrier-value projection))
          (let* ((status
                  (e-board-orchestration-run-set-compact-status
                   state "controller-run"))
                 (context
                  (e-board-orchestration-run-set-context state)))
            (should (equal (plist-get status :projection) barrier-value))
            (should (equal (plist-get context :projection) barrier-value))))))))

(ert-deftest e-chat-board-admission-test-run-set-barrier-fails-or-cancels-locally ()
  "Run-set barriers fail on refresh failure and cancel on binding retirement."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((initial-query
            (e-chat-board-admission-test--held-work "held-barrier-failure"))
           (refresh-query
            (e-chat-board-admission-test--held-work "held-barrier-failure-refresh"))
           (cancel-query
            (e-chat-board-admission-test--held-work "held-barrier-cancel-refresh"))
           (queries (list initial-query refresh-query cancel-query))
           admitted board-id session-id open binding state barrier)
      (cl-letf (((symbol-function 'e-board-orchestration-actions-run-set)
                 (lambda (&rest _arguments)
                   (or (pop queries)
                       (error "unexpected extra Board run-set query")))))
        (setq admitted
              (e-chat-board-admission-test--await
               (e-chat-service-owner-admission-work
                (e-chat-service-owner-admission-start
                 :harness harness :creation-key "org:daily:barrier-failure"
                 :metadata '(:name "Barrier failure Daily")))))
        (setq board-id (plist-get admitted :board-id)
              session-id
              (plist-get
               (e-chat-service-owner-admission-identities
                "org:daily:barrier-failure")
               :session-id)
              open (e-chat-service-open-board-owner-start board-id harness))
        (e-chat-board-admission-test--wait
         (lambda ()
           (and (e-chat-service-binding harness session-id)
                (= (length queries) 2))))
        (setq binding (e-chat-service-binding harness session-id)
              state (e-board-run-set-state-for harness session-id board-id))
        (e-work-finish
         initial-query
         (list :board-id board-id :restore-state 'ready :ready-p t
               :status 'idle :runs nil :active-count 0 :active-run-count 0
               :omitted-count 0 :bytes 0))
        (e-chat-board-admission-test--await open)
        (e-chat-board-admission-test--await
         (e-board-sqlite-publication-target-orchestration-fact-start
          (e-chat-service-publication-target binding)
          '(:version 1 :type manifest :idempotency-key "manifest:barrier-failure"
            :payload (:run-id "failure-run"
                      :tasks ((:task-key "task" :required t :accepted-attempt 0))
                      :deadline (:kind none)))))
        (e-chat-board-admission-test--wait
         (lambda () (= (length queries) 1)))
        (setq barrier (e-board-run-set-await-run-start binding "never"))
        (e-work-fail refresh-query '(e-board-orchestration-error
                                     "held refresh failed"))
        (should-error
         (e-chat-board-admission-test--await barrier)
         :type 'e-board-orchestration-error)
        (e-board-orchestration-run-set-state-update
         state nil :restore-state 'ready)
        (e-chat-board-admission-test--await
         (e-board-sqlite-publication-target-orchestration-fact-start
          (e-chat-service-publication-target binding)
          '(:version 1 :type manifest :idempotency-key "manifest:barrier-cancel"
            :payload (:run-id "cancel-run"
                      :tasks ((:task-key "task" :required t :accepted-attempt 0))
                      :deadline (:kind none)))))
        (e-chat-board-admission-test--wait
         (lambda () (= (length queries) 0)))
        (setq barrier (e-board-run-set-await-run-start binding "retired"))
        (e-chat-service--retire-binding binding)
        (should-error
         (e-chat-board-admission-test--await barrier)
         :type 'e-work-cancelled)))))

(ert-deftest e-chat-board-admission-test-run-set-query-start-failure-publishes-unavailable ()
  "A synchronous refresh-start error invalidates the shared projection first."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((ticket
            (e-chat-service-owner-admission-start
             :harness harness :creation-key "org:daily:query-start-failure"
             :metadata '(:name "Query start failure Daily")))
           (admitted
            (e-chat-board-admission-test--await
             (e-chat-service-owner-admission-work ticket)))
           (binding
            (e-chat-board-admission-test--await
             (e-chat-service-open-board-owner-start
              (plist-get admitted :board-id) harness)))
           (session-id (e-chat-service-binding-session-id binding))
           (state (e-board-run-set-state-for
                   harness session-id
                   (e-chat-service-binding-board-id binding))))
      (cl-letf (((symbol-function 'e-board-orchestration-actions-run-set)
                 (lambda (&rest _arguments)
                   (error "synchronous run-set start failure"))))
        (e-chat-board-admission-test--await
         (e-board-sqlite-publication-target-orchestration-fact-start
          (e-chat-service-publication-target binding)
          '(:version 1 :type manifest
            :idempotency-key "manifest:query-start-failure"
            :payload (:run-id "query-start-failure-run" :tasks nil
                      :deadline (:kind none)))))
        (e-chat-board-admission-test--wait
         (lambda ()
           (eq (plist-get (e-board-orchestration-run-set-state-value state)
                          :restore-state)
               'unavailable)))
        (should-not (plist-get (e-board--run-set-controls state) :query-work))
        (let ((barrier
               (e-board-run-set-await-run-start binding "missing-after-start")))
          (should-error
           (e-chat-board-admission-test--await barrier)
           :type 'e-board-orchestration-error))))))

(ert-deftest e-chat-board-admission-test-run-set-value-install-failure-invalidates-state ()
  "A malformed detached result invalidates state before query failure settles."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((initial-query
            (e-chat-board-admission-test--held-work "held-value-install-initial"))
           (refresh-query
            (e-chat-board-admission-test--held-work "held-value-install-refresh"))
           (queries (list initial-query refresh-query))
           admitted binding open state barrier)
      (cl-letf (((symbol-function 'e-board-orchestration-actions-run-set)
                 (lambda (&rest _arguments)
                   (or (pop queries)
                       (error "unexpected extra Board run-set query")))))
        (setq admitted
              (e-chat-board-admission-test--await
               (e-chat-service-owner-admission-work
                (e-chat-service-owner-admission-start
                 :harness harness :creation-key "org:daily:value-install-failure"
                 :metadata '(:name "Value install failure Daily")))))
        (setq open
              (e-chat-service-open-board-owner-start
               (plist-get admitted :board-id) harness))
        (e-work-finish
         initial-query
         (list :board-id (plist-get admitted :board-id)
               :restore-state 'ready :ready-p t :status 'idle :runs nil
               :active-count 0 :active-run-count 0 :omitted-count 0 :bytes 0))
        (setq binding (e-chat-board-admission-test--await open))
        (setq state
              (e-board-run-set-state-for
               harness (e-chat-service-binding-session-id binding)
               (e-chat-service-binding-board-id binding)))
        (e-chat-board-admission-test--await
         (e-chat-service-binding-readiness-work binding))
        (e-chat-board-admission-test--await
         (e-board-sqlite-publication-target-orchestration-fact-start
          (e-chat-service-publication-target binding)
          '(:version 1 :type manifest
            :idempotency-key "manifest:value-install-failure"
            :payload (:run-id "value-install-failure-run" :tasks nil
                      :deadline (:kind none)))))
        (e-chat-board-admission-test--wait (lambda () (null queries)))
        (setq barrier
              (e-board-run-set-await-run-start binding "missing-after-install"))
        ;; :runs is intentionally absent, so the Board-owned value setter must
        ;; reject it and publish unavailable before failing the query carrier.
        (e-work-finish
         refresh-query
         (list :board-id (e-chat-service-binding-board-id binding)
               :restore-state 'ready :ready-p t :status 'running
               :omitted-count 0 :bytes 0))
        (should (eq (plist-get (e-board-orchestration-run-set-state-value state)
                               :restore-state)
                    'unavailable))
        (should-error
         (e-chat-board-admission-test--await barrier)
         :type 'e-board-orchestration-error)))))

(ert-deftest e-chat-board-admission-test-run-set-barrier-fails-on-omitted-run ()
  "A ready bounded page missing an exact run fails after its refresh generation."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((initial-query
            (e-chat-board-admission-test--held-work "held-omitted-run-initial"))
           (refresh-query
            (e-chat-board-admission-test--held-work "held-omitted-run-refresh"))
           (queries (list initial-query refresh-query))
           admitted binding open state barrier)
      (cl-letf (((symbol-function 'e-board-orchestration-actions-run-set)
                 (lambda (&rest _arguments)
                   (or (pop queries)
                       (error "unexpected extra Board run-set query")))))
        (setq admitted
              (e-chat-board-admission-test--await
               (e-chat-service-owner-admission-work
                (e-chat-service-owner-admission-start
                 :harness harness :creation-key "org:daily:omitted-run"
                 :metadata '(:name "Omitted run Daily")))))
        (setq open
              (e-chat-service-open-board-owner-start
               (plist-get admitted :board-id) harness))
        (e-work-finish
         initial-query
         (list :board-id (plist-get admitted :board-id)
               :restore-state 'ready :ready-p t :status 'idle :runs nil
               :active-count 0 :active-run-count 0 :omitted-count 0 :bytes 0))
        (setq binding (e-chat-board-admission-test--await open)
              state (e-board-run-set-state-for
                     harness (e-chat-service-binding-session-id binding)
                     (e-chat-service-binding-board-id binding)))
        (e-chat-board-admission-test--await
         (e-chat-service-binding-readiness-work binding))
        (e-chat-board-admission-test--await
         (e-board-sqlite-publication-target-orchestration-fact-start
          (e-chat-service-publication-target binding)
          '(:version 1 :type manifest :idempotency-key "manifest:omitted-run"
            :payload (:run-id "omitted-run" :tasks nil
                      :deadline (:kind none)))))
        (e-chat-board-admission-test--wait (lambda () (null queries)))
        (setq barrier (e-board-run-set-await-run-start binding "omitted-run"))
        (e-work-finish
         refresh-query
         (list :board-id (e-chat-service-binding-board-id binding)
               :restore-state 'ready :ready-p t :status 'running :runs nil
               :active-count 0 :active-run-count 0 :omitted-count 1 :bytes 64))
        (should (equal (plist-get (e-board-orchestration-run-set-state-value state)
                                  :omitted-count)
                       1))
        (should-error
         (e-chat-board-admission-test--await barrier)
         :type 'e-board-orchestration-error)
        ;; With no current query, the already-published ready generation is a
        ;; completed refresh and cannot leave a missing exact run pending.
        (setq barrier (e-board-run-set-await-run-start binding "omitted-now"))
        (should-error
         (e-chat-board-admission-test--await barrier)
         :type 'e-board-orchestration-error)))))

(ert-deftest e-chat-board-admission-test-run-set-barrier-fenced-finalized-absence ()
  "A finalized run waits for a newer projection proving it is no longer active."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((initial-query
            (e-chat-board-admission-test--held-work "held-finalized-initial"))
           (refresh-query
            (e-chat-board-admission-test--held-work "held-finalized-refresh"))
           (queries (list initial-query refresh-query))
           admitted binding open receipt barrier)
      (cl-letf (((symbol-function 'e-board-orchestration-actions-run-set)
                 (lambda (&rest _arguments)
                   (or (pop queries)
                       (error "unexpected extra Board run-set query")))))
        (setq admitted
              (e-chat-board-admission-test--await
               (e-chat-service-owner-admission-work
                (e-chat-service-owner-admission-start
                 :harness harness :creation-key "org:daily:fenced-finalized"
                 :metadata '(:name "Fenced finalized Daily")))))
        (setq open
              (e-chat-service-open-board-owner-start
               (plist-get admitted :board-id) harness))
        (e-work-finish
         initial-query
         (list :board-id (plist-get admitted :board-id)
               :restore-state 'ready :ready-p t :status 'idle :runs nil
               :active-count 0 :active-run-count 0 :omitted-count 0 :bytes 0))
        (setq binding (e-chat-board-admission-test--await open))
        (e-chat-board-admission-test--await
         (e-chat-service-binding-readiness-work binding))
        (setq receipt (e-board-run-set-generation binding))
        (e-chat-board-admission-test--await
         (e-board-sqlite-publication-target-orchestration-fact-start
          (e-chat-service-publication-target binding)
          '(:version 1 :type manifest :idempotency-key "manifest:fenced-finalized"
            :payload (:run-id "finalized-run" :tasks nil
                      :deadline (:kind none)))))
        (e-chat-board-admission-test--wait (lambda () (null queries)))
        (setq barrier
              (e-board-run-set-await-run-start
               binding "finalized-run" receipt 'absent))
        (should-not (e-request-terminal-p (e-work-handle-lifecycle barrier)))
        (e-work-finish
         refresh-query
         (list :board-id (e-chat-service-binding-board-id binding)
               :restore-state 'ready :ready-p t :status 'idle :runs nil
               :active-count 0 :active-run-count 0 :omitted-count 0 :bytes 0))
        (should (equal (e-chat-board-admission-test--await barrier)
                       (e-board-orchestration-run-set-state-value
                        (e-board-run-set-state-for
                         (e-chat-service-binding-harness binding)
                         (e-chat-service-binding-session-id binding)
                         (e-chat-service-binding-board-id binding)))))))))

(ert-deftest e-chat-board-admission-test-readiness-precedes-pickup-resume ()
  "Owner startup installs Board readiness before restart pickup delivery."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let ((events nil)
          (original-readiness
           (symbol-function 'e-chat-service--binding-open-readiness-start))
          (original-resume
           (symbol-function 'e-chat-service--sql-resume-ready)))
      (cl-letf
          (((symbol-function 'e-chat-service--binding-open-readiness-start)
            (lambda (binding)
              (push 'readiness-installed events)
              (funcall original-readiness binding)))
           ((symbol-function 'e-chat-service--sql-resume-ready)
            (lambda (binding)
              (push 'pickup-resume events)
              (funcall original-resume binding))))
        (let* ((ticket
                (e-chat-service-owner-admission-start
                 :harness harness :creation-key "org:daily:startup-order"
                 :metadata '(:name "Startup order Daily")))
               (admitted
                (e-chat-board-admission-test--await
                 (e-chat-service-owner-admission-work ticket)))
               (binding
                (e-chat-board-admission-test--await
                 (e-chat-service-open-board-owner-start
                  (plist-get admitted :board-id) harness))))
          (should (e-chat-service-binding-p binding))
          (should (equal (nreverse events)
                         '(readiness-installed pickup-resume))))))))

(ert-deftest e-chat-board-admission-test-pickup-waits-for-run-set-readiness ()
  "A ready pickup neither claims nor runs while owner readiness restores.

The success and failure halves use separate disposable stores so the held
run-set query is the only readiness edge under test.  A durable ready pickup
is visible before release, then exactly one turn is submitted only after the
projection succeeds; a failed projection leaves its pickup unclaimed."
  (let ((original-submit
         (symbol-function 'e-harness-attached-turn-port-submit))
        (original-transition
         (symbol-function 'e-board-sqlite-service-transition-pickup-start)))
    (cl-labels
        ((run-case (settle)
           (e-chat-board-admission-test--with-fixture (store harness)
             (let* ((service (e-board-sqlite-service-create
                              (e-session-storage-runtime-store store)))
                    (held (e-chat-board-admission-test--held-work
                           (if (eq settle 'success)
                               "held-run-set-success"
                             "held-run-set-failure")))
                    (submit-count 0)
                    (claim-count 0))
                 (cl-letf
                   (((symbol-function 'e-board-orchestration-actions-run-set)
                     (lambda (&rest _arguments) held))
                    ((symbol-function
                      'e-board-sqlite-service-transition-pickup-start)
                     (lambda (service board-id delivery-id transition &optional data)
                       (when (eq transition 'claim)
                         (cl-incf claim-count))
                       (funcall original-transition service board-id delivery-id
                                transition data)))
                    ((symbol-function 'e-harness-attached-turn-port-submit)
                     (lambda (port prompt &rest arguments)
                       (cl-incf submit-count)
                       (apply original-submit port prompt arguments))))
                 (let* ((admitted
                         (e-chat-board-admission-test--admit-association
                          service
                          (if (eq settle 'success)
                              "held-pickup-success-session"
                            "held-pickup-failure-session")
                          (if (eq settle 'success)
                              "held-pickup-success-board"
                            "held-pickup-failure-board")
                          (if (eq settle 'success)
                              "principal-held-success"
                            "principal-held-failure")
                          (if (eq settle 'success)
                              "participant-held-success"
                            "participant-held-failure")
                          'owner))
                        (association (plist-get admitted :association))
                        (board-id (plist-get association :board-id))
                        (session-id
                         (if (eq settle 'success)
                             "held-pickup-success-session"
                           "held-pickup-failure-session"))
                        (participant-id
                         (plist-get (plist-get association :routing-policy)
                                    :participant-id))
                        (route
                         (e-chat-board-admission-test--await
                          (e-board-sqlite-service-append-route-start
                           service board-id :author "test"
                           :tags '(main) :content "ready pickup"
                           :source-input-key
                           (list "held-pickup" settle 1))))
                        (pickup (car (plist-get route :pickups)))
                        (open
                         (e-chat-service-open-board-owner-start
                          board-id harness)))
                   (should (eq (plist-get pickup :state) 'ready))
                   (e-chat-board-admission-test--wait
                    (lambda ()
                      (or (e-chat-service-binding harness session-id)
                          (e-request-terminal-p
                           (e-work-handle-lifecycle open)))))
                   (should-not (e-request-terminal-p
                                (e-work-handle-lifecycle open)))
                   (should (= claim-count 0))
                   (should (= submit-count 0))
                   (let* ((board
                           (e-chat-board-admission-test--await
                            (e-board-sqlite-service-board-get-start
                             service board-id)))
                          (page
                           (e-chat-board-admission-test--await
                            (e-board-sqlite-service-pickup-page-start
                             service board-id (plist-get board :generation)
                             participant-id 16))))
                     (should (eq (plist-get
                                  (car page) :state)
                                 'ready)))
                   (if (eq settle 'success)
                       (progn
                         (e-work-finish
                          held
                          (list :board-id board-id :restore-state 'ready
                                :ready-p t :runs nil :omitted-count 0))
                         (should (eq
                                  (e-chat-board-admission-test--await open)
                                  (e-chat-service-binding harness session-id)))
                         (e-chat-board-admission-test--wait
                          (lambda () (= submit-count 1)))
                         (should (= claim-count 1))
                         (should (= submit-count 1)))
                     (e-work-fail held
                                  '(e-board-sqlite-error
                                    "held run-set query failed"))
                     (should-error
                      (e-chat-board-admission-test--await open)
                      :type 'e-board-sqlite-error)
                     (should-not (e-chat-service-binding harness session-id))
                     (should (= claim-count 0))
                     (should (= submit-count 0))
                     (let* ((board
                             (e-chat-board-admission-test--await
                              (e-board-sqlite-service-board-get-start
                               service board-id)))
                            (page
                             (e-chat-board-admission-test--await
                              (e-board-sqlite-service-pickup-page-start
                               service board-id (plist-get board :generation)
                               participant-id 16))))
                       (should (eq (plist-get
                                    (car page) :state)
                                   'ready))))))))))
      (run-case 'success)
      (run-case 'failure))))

(ert-deftest e-chat-board-admission-test-participant-binding-skips-owner-readiness ()
  "Participant admission does not clobber or inherit owner run-set readiness."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let* ((service (e-board-sqlite-service-create
                     (e-session-storage-runtime-store store)))
           (owner-result
            (e-chat-board-admission-test--admit-association
             service "participant-parent" "participant-parent-board"
             "participant-parent-principal" "participant-parent-id" 'owner))
           (parent-association (plist-get owner-result :association))
           (parent
            (e-chat-service--install-sqlite-binding
             harness "participant-parent" parent-association nil t))
           (operation
            (e-chat-service-create-participant-start
             parent harness :metadata '(:participant-name "child"))))
      (should (e-chat-service-binding-readiness-work parent))
      (should (e-chat-board-admission-test--await operation))
      (let* ((child
              (e-chat-service-participant-operation-binding
               (e-work-handle-arguments operation)))
             (parent-readiness
              (e-chat-service-binding-readiness-work parent)))
        (should child)
        (should-not (e-chat-service-binding-readiness-work child))
        (should (eq parent-readiness
                    (e-chat-service-binding-readiness-work parent)))))))

(ert-deftest e-chat-board-admission-test-owner-open-fails-closed-partitions ()
  "Owner resolution rejects zero, multiple, wrong-role, and live conflicts."
  (e-chat-board-admission-test--with-fixture (store harness)
    (let ((service (e-board-sqlite-service-create
                    (e-session-storage-runtime-store store))))
      (e-chat-board-admission-test--await
       (e-board-sqlite-service-board-create-start
        service "empty-board" "principal-empty"))
      (should-error
       (e-chat-board-admission-test--await
        (e-chat-service-open-board-owner-start "empty-board" harness))
       :type 'e-session-error)

      (e-chat-board-admission-test--await
       (e-board-sqlite-service-board-create-start
        service "participant-board" "principal-participant"))
      (e-chat-board-admission-test--admit-association
       service "participant-session" "participant-board"
       "principal-participant" "participant-id" 'participant)
      (should-error
       (e-chat-board-admission-test--await
       (e-chat-service-open-board-owner-start "participant-board" harness))
       :type 'e-session-error)

      (e-chat-board-admission-test--await
       (e-board-sqlite-service-board-create-start
        service "wrong-board-target" "principal-target"))
      (e-chat-board-admission-test--await
       (e-board-sqlite-service-board-create-start
        service "wrong-board-owner" "principal-other"))
      (e-chat-board-admission-test--admit-association
       service "wrong-board-session" "wrong-board-owner" "principal-other"
       "wrong-board-owner" 'owner)
      (should-error
       (e-chat-board-admission-test--await
        (e-chat-service-open-board-owner-start "wrong-board-target" harness))
       :type 'e-session-error)

      (e-chat-board-admission-test--admit-association
       service "owner-one" "multiple-board" "principal-multiple" "owner-one"
       'owner)
      (e-chat-board-admission-test--admit-association
       service "owner-two" "multiple-board" "principal-multiple" "owner-two"
       'owner)
      (should-error
       (e-chat-board-admission-test--await
        (e-chat-service-open-board-owner-start "multiple-board" harness))
       :type 'e-session-error)

      (should-error
       (e-chat-board-admission-test--await
        (e-chat-service-open-board-owner-start "missing-board" harness))
       :type 'e-session-missing)

      (e-chat-board-admission-test--admit-association
       service "conflict-owner" "conflict-board" "principal-conflict"
       "conflict-owner" 'owner)
      (let ((binding
             (e-chat-board-admission-test--await
              (e-chat-service-open-board-owner-start
               "conflict-board" harness))))
        (let ((principal (e-chat-service-binding-principal binding))
              (default-tags
               (e-chat-service-binding-default-tags binding)))
          (setf (e-chat-service-binding-principal binding) "wrong-principal")
          (should-error
           (e-chat-board-admission-test--await
            (e-chat-service-open-board-owner-start
             "conflict-board" harness))
           :type 'e-session-error)
          (should (eq (e-chat-service-binding harness "conflict-owner")
                      binding))
          (setf (e-chat-service-binding-principal binding) principal
                (e-chat-service-binding-default-tags binding) '(wrong-tags))
          (should-error
           (e-chat-board-admission-test--await
            (e-chat-service-open-board-owner-start
             "conflict-board" harness))
           :type 'e-session-error)
          (setf (e-chat-service-binding-default-tags binding) default-tags))))))

(ert-deftest e-chat-board-admission-test-ticket-is-immediate-and-cancellable ()
  "Stable owner tickets remain provisional while admission is held or cancelled."
  (e-chat-board-admission-test--with-stalled-fixture
      (store harness stall-directory)
    (write-region
     "hold" nil
     (e-chat-board-admission-test--stall-file
      stall-directory 'chat-session-owner-admit "hold")
     nil 'silent)
    (let* ((ticket
            (e-chat-service-owner-admission-start
             :harness harness :creation-key "org:daily:held"
             :metadata '(:name "Held Daily")))
           (work (e-chat-service-owner-admission-work ticket)))
      (should (string-prefix-p
               "brd_"
               (e-chat-service-owner-admission-provisional-board-id ticket)))
      (e-chat-board-admission-test--wait
       (lambda ()
         (file-exists-p
          (e-chat-board-admission-test--stall-file
           stall-directory 'chat-session-owner-admit "ready"))))
      (should-not (e-request-terminal-p (e-work-handle-lifecycle work)))
      (write-region
       "release" nil
       (e-chat-board-admission-test--stall-file
        stall-directory 'chat-session-owner-admit "release")
       nil 'silent)
      (should (plist-get
               (e-chat-board-admission-test--await work)
               :board-id)))
    (let* ((cancelled
            (e-chat-service-owner-admission-start
             :harness harness :creation-key "org:daily:cancelled"
             :metadata '(:name "Cancelled Daily")))
           (work (e-chat-service-owner-admission-work cancelled)))
      (e-work-cancel work)
      (should (eq (plist-get (e-work-status work) :state) 'cancelled))
      (should-not
       (e-chat-service-binding
        harness
        (plist-get
         (e-chat-service-owner-admission-identities "org:daily:cancelled")
         :session-id))))))

(provide 'e-chat-board-admission-test)

;;; e-chat-board-admission-test.el ends here

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

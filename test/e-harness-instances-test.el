;;; e-harness-instances-test.el --- Tests for harness instance catalog -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for user-facing configured harness instance registration.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-instances)

(defmacro e-harness-instances-test--with-empty-registries (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-harness-instance--session-stores (make-hash-table :test 'equal))
         (e-harness-instance--generation 0)
         (e-harness-instance--request-sequence 0))
     ,@body))

(ert-deftest e-harness-instances-test-registers-and-lists-by-kind ()
  "Registered harness instances can be listed by shell kind."
  (e-harness-instances-test--with-empty-registries
    (e-harness-instance-register
     :id :chat-alpha
     :name "Alpha"
     :kind 'chat
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil)))
     :metadata '(:model "alpha"))
    (e-harness-instance-register
     :id :other
     :name "Other"
     :kind 'canvas
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (should (equal (mapcar #'e-harness-instance-id
                           (e-harness-instance-list :kind 'chat))
                   '(:chat-alpha)))
    (let ((instance (e-harness-instance-get :chat-alpha)))
      (should (equal (e-harness-instance-name instance) "Alpha"))
      (should (equal (e-harness-instance-metadata instance)
                     '(:model "alpha"))))))

(ert-deftest e-harness-instances-test-lazily-creates-harness-once ()
  "Getting an instance harness delegates to the low-level harness registry."
  (e-harness-instances-test--with-empty-registries
    (let ((calls 0))
      (e-harness-instance-register
       :id :chat-alpha
       :name "Alpha"
       :kind 'chat
       :factory (lambda ()
                  (setq calls (1+ calls))
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
      (let ((first (e-harness-instance-get-or-create :chat-alpha))
            (second (e-harness-instance-get-or-create :chat-alpha)))
        (should (e-harness-p first))
        (should (eq first second))
        (should (= calls 1))))))

(ert-deftest e-harness-instances-test-default-is-kind-scoped ()
  "Defaults are selected independently for each harness instance kind."
  (e-harness-instances-test--with-empty-registries
    (e-harness-instance-register
     :id :chat-alpha
     :name "Alpha"
     :kind 'chat
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (e-harness-instance-register
     :id :chat-beta
     :name "Beta"
     :kind 'chat
     :default t
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (should (eq (e-harness-instance-id
                 (e-harness-instance-default :kind 'chat))
                :chat-beta))))

(ert-deftest e-harness-instances-test-catalog-ports-never-activate-a-harness ()
  "Session data ports are declarative metadata, not factory entry points."
  (e-harness-instances-test--with-empty-registries
    (let ((factory-calls 0)
          (catalog (lambda (&rest _arguments) 'pending))
          (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-instance-register
       :id :dormant :kind 'chat :session-store-id "store"
       :session-catalog catalog :session-access-store access-store
       :factory (lambda () (cl-incf factory-calls) (e-harness-create)))
      (let ((instance (e-harness-instance-get :dormant)))
        (should (equal (e-harness-instance-session-store-id instance) "store"))
        (should (eq (e-harness-instance-session-catalog instance) catalog))
        (should (eq (e-harness-instance-session-access-store instance) access-store))
        (should (= factory-calls 0))))))

(ert-deftest e-harness-instances-test-shared-session-store-requires-identical-ports ()
  "Two views of one store must not expose divergent catalog contracts."
  (e-harness-instances-test--with-empty-registries
    (let ((catalog (lambda (&rest _arguments) 'pending))
          (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-instance-register :id :first :kind 'chat :session-store-id "store"
                                   :session-catalog catalog :session-access-store access-store)
      (e-harness-instance-register :id :second :kind 'chat :session-store-id "store"
                                   :session-catalog catalog :session-access-store access-store)
      (should-error
       (e-harness-instance-register
        :id :conflict :kind 'chat :session-store-id "store"
        :session-catalog (lambda (&rest _arguments) 'pending)
        :session-access-store access-store)
       :type 'e-harness-instance-store-conflict))))

(ert-deftest e-harness-instances-test-replaces-duplicate-registration ()
  "Registering the same instance id replaces catalog metadata."
  (e-harness-instances-test--with-empty-registries
    (e-harness-instance-register
     :id :chat-alpha
     :name "Alpha"
     :kind 'chat
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (e-harness-instance-register
     :id :chat-alpha
     :name "Renamed Alpha"
     :kind 'chat
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (should (equal (e-harness-instance-name
                    (e-harness-instance-get :chat-alpha))
                   "Renamed Alpha"))
    (should (equal (mapcar #'e-harness-instance-id
                           (e-harness-instance-list :kind 'chat))
                   '(:chat-alpha)))))

(ert-deftest e-harness-instances-test-session-stores-deduplicate-eligible-instances ()
  "Host catalog metadata returns a shared store once without starting it."
  (e-harness-instances-test--with-empty-registries
    (let ((catalog (lambda (&rest _arguments) 'pending))
          (access-store (lambda (&rest _arguments) 'pending)))
      (dolist (id '(:second :first))
        (e-harness-instance-register :id id :kind 'chat :session-store-id "store"
                                     :session-catalog catalog :session-access-store access-store))
      (let ((stores (e-harness-instance-session-stores)))
        (should (= (length stores) 1))
        (should (equal (plist-get (car stores) :eligible-instance-ids)
                       '(:first :second)))))))

(ert-deftest e-harness-instances-test-replacement-updates-index-and-generation ()
  "Replacing one instance moves only its store eligibility and fences snapshots."
  (e-harness-instances-test--with-empty-registries
    (let ((catalog (lambda (&rest _arguments) 'pending))
          (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-instance-register :id :instance :kind 'chat
                                   :session-store-id "old"
                                   :session-catalog catalog
                                   :session-access-store access-store)
      (should (= e-harness-instance--generation 1))
      (e-harness-instance-register :id :instance :kind 'chat
                                   :session-store-id "new"
                                   :session-catalog catalog
                                   :session-access-store access-store)
      (should (= e-harness-instance--generation 2))
      (let ((stores (e-harness-instance-session-stores)))
        (should (equal (mapcar (lambda (entry)
                                (plist-get entry :session-store-id))
                              stores)
                       '("new")))
        (should (equal (plist-get (car stores) :eligible-instance-ids)
                       '(:instance)))))))

(ert-deftest e-harness-instances-test-catalog-page-is-pending-and-non-activating ()
  "A held store leaves one bounded request pending without invoking a factory."
  (e-harness-instances-test--with-empty-registries
    (let* (arguments succeed fail
           (factory-calls 0)
           (catalog (lambda (request on-done on-error)
                      (setq arguments request
                            succeed on-done
                            fail on-error)
                      nil))
           (access-store (lambda (&rest _arguments) 'pending)))
      (dolist (id '(:second :first))
        (e-harness-instance-register
         :id id :kind 'chat :session-store-id "store"
         :session-catalog catalog :session-access-store access-store
         :factory (lambda () (cl-incf factory-calls) (e-harness-create))))
      (let ((request (e-harness-instance-session-catalog-page-start
                      "store" :principal "owner" :after "cursor" :limit 2)))
        (should (eq (e-request-lifecycle-state request) 'started))
        (should (equal (plist-get arguments :operation) 'list-page))
        (should (equal (plist-get arguments :principal) "owner"))
        (should (equal (plist-get arguments :after) "cursor"))
        (should (= factory-calls 0))
        (should (functionp succeed))
        (should (functionp fail))
        (funcall succeed
                 '(:sessions ((:session-id "session" :state dormant))
                   :next-after "next"))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (let* ((page (e-request-lifecycle-terminal-payload request))
               (session (car (plist-get page :sessions))))
          (should (equal (plist-get page :session-store-id) "store"))
          (should (equal (plist-get page :eligible-instance-ids)
                         '(:first :second)))
          (should (equal (plist-get session :session-id) "session"))
          (should (equal (plist-get session :eligible-instance-ids)
                         '(:first :second))))))))

(ert-deftest e-harness-instances-test-catalog-page-fences-stale-mapping ()
  "A delayed page cannot commit after configured instance metadata changes."
  (e-harness-instances-test--with-empty-registries
    (let* (succeed
           (catalog (lambda (_request on-done _on-error)
                      (setq succeed on-done)
                      nil))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-instance-register
       :id :first :kind 'chat :session-store-id "store"
       :session-catalog catalog :session-access-store access-store)
      (let ((request
             (e-harness-instance-session-catalog-page-start "store" :limit 1)))
        (e-harness-instance-register :id :unrelated :kind 'chat)
        (funcall succeed '(:sessions nil))
        (should (eq (e-request-lifecycle-state request) 'failed))
        (should (eq (car (e-request-lifecycle-terminal-payload request))
                    'e-harness-instance-session-catalog-stale))))))

(ert-deftest e-harness-instances-test-catalog-page-cancellation-fences-late-success ()
  "Cancelling a held page reaches its adapter and prevents later resettlement."
  (e-harness-instances-test--with-empty-registries
    (let* (succeed
           (cancel-calls 0)
           (catalog (lambda (_request on-done _on-error)
                      (setq succeed on-done)
                      (lambda () (cl-incf cancel-calls))))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-instance-register
       :id :instance :kind 'chat :session-store-id "store"
       :session-catalog catalog :session-access-store access-store)
      (let ((request
             (e-harness-instance-session-catalog-page-start "store" :limit 1)))
        (e-request-cancel request 'user-cancelled)
        (should (= cancel-calls 1))
        (should (eq (e-request-lifecycle-state request) 'cancelled))
        (funcall succeed '(:sessions ((:session-id "late"))))
        (should (eq (e-request-lifecycle-state request) 'cancelled))
        (should (eq (e-request-lifecycle-terminal-payload request)
                    'user-cancelled))))))

(ert-deftest e-harness-instances-test-catalog-page-rejects-over-limit-result ()
  "A catalog adapter cannot settle a request with more rows than requested."
  (e-harness-instances-test--with-empty-registries
    (let* (succeed
           (catalog (lambda (_request on-done _on-error)
                      (setq succeed on-done)
                      nil))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-instance-register
       :id :instance :kind 'chat :session-store-id "store"
       :session-catalog catalog :session-access-store access-store)
      (let ((request
             (e-harness-instance-session-catalog-page-start "store" :limit 1)))
        (funcall succeed
                 '(:sessions ((:session-id "first") (:session-id "second"))))
        (should (eq (e-request-lifecycle-state request) 'failed))
        (should (eq (car (e-request-lifecycle-terminal-payload request))
                    'e-harness-instance-session-catalog-invalid-page))))))

(ert-deftest e-harness-instances-test-access-store-is-controlled-and-non-activating ()
  "Optimistic ACL mutation remains pending without activating any harness."
  (e-harness-instances-test--with-empty-registries
    (let* (arguments succeed fail
           (factory-calls 0)
           (catalog (lambda (&rest _arguments) 'pending))
           (access-store
            (lambda (request on-done on-error)
              (setq arguments request
                    succeed on-done
                    fail on-error)
              nil)))
      (e-harness-instance-register
       :id :instance :kind 'chat :session-store-id "store"
       :session-catalog catalog :session-access-store access-store
       :factory (lambda () (cl-incf factory-calls) (e-harness-create)))
      (let ((request
             (e-harness-instance-session-access-start
              "store" 'grant
              '(:session-id "session" :requester-principal "owner"
                :expected-version 3 :principal "member" :rights (discover resume)))))
        (should (eq (e-request-lifecycle-state request) 'started))
        (should (equal (plist-get arguments :operation) 'grant))
        (should (equal (plist-get arguments :session-store-id) "store"))
        (should (= (plist-get arguments :expected-version) 3))
        (should (= factory-calls 0))
        (should (functionp succeed))
        (should (functionp fail))
        (funcall succeed
                 '(:access-record (:controller "owner" :version 4)))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (let ((result (e-request-lifecycle-terminal-payload request)))
          (should (equal (plist-get result :session-store-id) "store"))
          (should (= (plist-get (plist-get result :access-record) :version)
                     4)))))))

(ert-deftest e-harness-instances-test-access-store-requires-optimistic-auth-inputs ()
  "Access mutation rejects unsupported or unauthenticated requests before I/O."
  (e-harness-instances-test--with-empty-registries
    (let ((calls 0)
          (catalog (lambda (&rest _arguments) 'pending))
          (access-store (lambda (&rest _arguments) (cl-incf calls))))
      (e-harness-instance-register
       :id :instance :kind 'chat :session-store-id "store"
       :session-catalog catalog :session-access-store access-store)
      (should-error
       (e-harness-instance-session-access-start
        "store" 'delete
        '(:session-id "session" :requester-principal "owner"
          :expected-version 1))
       :type 'e-harness-instance-session-access-invalid-operation)
      (should-error
       (e-harness-instance-session-access-start
        "store" 'grant
        '(:session-id "session" :requester-principal "owner"))
       :type 'wrong-type-argument)
      (should (= calls 0)))))

(ert-deftest e-harness-instances-test-session-store-requires-both-data-ports ()
  "A declared durable store cannot silently omit a required port."
  (e-harness-instances-test--with-empty-registries
    (should-error
     (e-harness-instance-register :id :incomplete :kind 'chat :session-store-id "store"
                                  :session-catalog (lambda (&rest _arguments) 'pending))
     :type 'e-harness-instance-store-conflict)))

(provide 'e-harness-instances-test)

;;; e-harness-instances-test.el ends here

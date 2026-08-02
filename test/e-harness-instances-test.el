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
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
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

(provide 'e-harness-instances-test)

;;; e-harness-instances-test.el ends here

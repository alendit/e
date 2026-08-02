;;; e-harness-registry-test.el --- Tests for harness registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for live harness instance and factory registration.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-harness)
(require 'e-harness-registry)

(defmacro e-harness-registry-test--with-empty-registry (&rest body)
  "Run BODY with empty harness registry tables."
  (declare (indent 0))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-registry--generations (make-hash-table :test 'equal))
         (e-harness-registry--invalidation-events
          (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest e-harness-registry-test-registers-and-retrieves-instance ()
  "Concrete harness instances can be registered and retrieved by id."
  (e-harness-registry-test--with-empty-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (should (eq (e-harness-registry-register :chat-test harness) harness))
      (should (eq (e-harness-registry-get :chat-test) harness))
      (should (eq (e-harness-registry-get-or-create :chat-test) harness)))))

(ert-deftest e-harness-registry-test-lazily-creates-instance-once ()
  "Registered factories create and cache harness instances lazily."
  (e-harness-registry-test--with-empty-registry
    (let ((calls 0))
      (e-harness-registry-register-factory
       :chat-test
       (lambda ()
         (setq calls (1+ calls))
         (e-harness-create
          :backend (e-backend-fake-create :items nil))))
      (let ((first (e-harness-registry-get-or-create :chat-test))
            (second (e-harness-registry-get-or-create :chat-test)))
        (should (e-harness-p first))
        (should (eq first second))
        (should (= calls 1))))))

(ert-deftest e-harness-registry-test-replacing-factory-keeps-cached-instance ()
  "Replacing a factory does not discard an already-created instance."
  (e-harness-registry-test--with-empty-registry
    (let ((first-harness (e-harness-create
                          :backend (e-backend-fake-create :items nil)))
          (second-harness (e-harness-create
                           :backend (e-backend-fake-create :items nil))))
      (e-harness-registry-register-factory :chat-test (lambda () first-harness))
      (should (eq (e-harness-registry-get-or-create :chat-test) first-harness))
      (e-harness-registry-register-factory :chat-test (lambda () second-harness))
      (should (eq (e-harness-registry-get-or-create :chat-test) first-harness)))))

(ert-deftest e-harness-registry-test-clear-instance-recreates-from-factory ()
  "Clearing an instance makes the next lookup call the registered factory."
  (e-harness-registry-test--with-empty-registry
    (let ((created nil))
      (e-harness-registry-register-factory
       :chat-test
       (lambda ()
         (let ((harness (e-harness-create
                         :backend (e-backend-fake-create :items nil))))
           (push harness created)
           harness)))
      (let ((first (e-harness-registry-get-or-create :chat-test)))
        (should (= (e-harness-registry-generation :chat-test) 1))
        (e-harness-registry-clear-instance :chat-test)
        (should (= (e-harness-registry-generation :chat-test) 2))
        (let ((second (e-harness-registry-get-or-create :chat-test)))
          (should (not (eq first second)))
          (should (= (e-harness-registry-generation :chat-test) 3))
          (should (= (length created) 2)))))))

(ert-deftest e-harness-registry-test-replacement-and-clear-fence-generations ()
  "Concrete object replacement and clear invalidate captured generations."
  (e-harness-registry-test--with-empty-registry
    (let ((first (e-harness-create))
          (second (e-harness-create)))
      (e-harness-registry-register :harness first)
      (should (= (e-harness-registry-generation :harness) 1))
      (should (eq (e-harness-registry-resolve :harness 1) first))
      ;; Re-registering the exact object is not a replacement.
      (e-harness-registry-register :harness first)
      (should (= (e-harness-registry-generation :harness) 1))
      (e-harness-registry-register :harness second)
      (should (= (e-harness-registry-generation :harness) 2))
      (should-error (e-harness-registry-resolve :harness 1)
                    :type 'e-harness-registry-stale-generation)
      (should (equal (e-harness-registry-invalidation-event :harness)
                     '(:kind replaced :harness-id :harness
                       :previous-generation 1 :generation 2)))
      (e-harness-registry-clear-instance :harness)
      (should (= (e-harness-registry-generation :harness) 3))
      (should-error (e-harness-registry-resolve :harness 2)
                    :type 'e-harness-registry-stale-generation)
      (should (equal (e-harness-registry-invalidation-event :harness)
                     '(:kind cleared :harness-id :harness
                       :previous-generation 2 :generation 3))))))

(ert-deftest e-harness-registry-test-missing-id-signals ()
  "Missing harness ids signal an explicit registry error."
  (e-harness-registry-test--with-empty-registry
    (should-error
     (e-harness-registry-get-or-create :missing)
     :type 'e-harness-registry-missing)))

(ert-deftest e-harness-registry-test-list-reports-registered-ids ()
  "Registry listing reports ids with registered instances or factories."
  (e-harness-registry-test--with-empty-registry
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-registry-register :instance-only harness)
      (e-harness-registry-register-factory :factory-only (lambda () harness))
      (should (equal (e-harness-registry-list)
                     '(:factory-only :instance-only))))))

(provide 'e-harness-registry-test)

;;; e-harness-registry-test.el ends here

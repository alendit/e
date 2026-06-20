;;; e-resources-test.el --- Tests for e resource registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for URI-addressed resource methods.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-operations)
(require 'e-resources)

(ert-deftest e-resources-test-dispatches-method-by-operation-and-scheme ()
  "Resource calls dispatch by operation contract and URI scheme."
  (let ((registry (e-resources-registry-create))
        (captured nil))
    (e-resources-register
     registry
     (e-resource-method-create
      :scheme "test"
      :operation e-operation-read
      :description "Test resources."
      :handler (lambda (uri range)
                 (setq captured (list uri range))
                 "read-result")))
    (should (equal (e-resources-call registry
                                     e-operation-read
                                     "test://thing"
                                     '(:unit "line" :start 1 :end 3))
                   "read-result"))
    (should (equal captured
                   '((:scheme "test" :address "thing" :uri "test://thing")
                     (:unit "line" :start 1 :end 3))))))

(ert-deftest e-resources-test-dispatches-multiple-methods-for-same-scheme ()
  "A scheme can implement multiple operations as separate methods."
  (let ((registry (e-resources-registry-create))
        (calls nil))
    (dolist (method
             (list
              (e-resource-method-create
               :scheme "test"
               :operation e-operation-write
               :description "Writable test resources."
               :handler (lambda (uri content)
                          (push (list :write uri content) calls)
                          "write-result"))
              (e-resource-method-create
               :scheme "test"
               :operation e-operation-edit
               :description "Editable test resources."
               :handler (lambda (uri edits)
                          (push (list :edit uri edits) calls)
                          "edit-result"))))
      (e-resources-register registry method))
    (should (equal (e-resources-write registry "test://target" "content")
                   "write-result"))
    (should (equal (e-resources-edit registry "test://target"
                                     '((:oldText "a" :newText "b")))
                   "edit-result"))
    (should (equal (nreverse calls)
                   '((:write (:scheme "test" :address "target" :uri "test://target")
                             "content")
                     (:edit (:scheme "test" :address "target" :uri "test://target")
                            ((:oldText "a" :newText "b"))))))))

(ert-deftest e-resources-test-dispatches-glob-and-search-methods ()
  "Resource discovery operations dispatch through registered methods."
  (let ((registry (e-resources-registry-create))
        (calls nil))
    (dolist (method
             (list
              (e-resource-method-create
               :scheme "test"
               :operation e-operation-glob
               :description "Glob test resources."
               :handler (lambda (uri pattern limit)
                          (push (list :glob uri pattern limit) calls)
                          '(:resources [(:uri "test://one" :name "one")]
                            :truncated nil)))
              (e-resource-method-create
               :scheme "other"
               :operation e-operation-search
               :description "Search other resources."
               :handler (lambda (uri query options)
                          (push (list :search uri query options) calls)
                          '(:matches [(:uri "other://one"
                                       :line 1
                                       :column 1
                                       :text "needle")]
                            :truncated nil)))))
      (e-resources-register registry method))
    (should (equal (e-resources-glob registry "test://root" "*.el" 3)
                   '(:resources [(:uri "test://one" :name "one")]
                     :truncated nil)))
    (should (equal (e-resources-search
                    registry
                    "other://root"
                    "needle"
                    '(:glob "*.el" :literal t :limit 4))
                   '(:matches [(:uri "other://one"
                                :line 1
                                :column 1
                                :text "needle")]
                     :truncated nil)))
    (should (equal (nreverse calls)
                   '((:glob (:scheme "test" :address "root" :uri "test://root")
                            "*.el" 3)
                     (:search (:scheme "other" :address "root" :uri "other://root")
                              "needle"
                              (:glob "*.el" :literal t :limit 4)))))
    (should (equal (mapcar #'e-operation-id
                           (e-resources-operations registry))
                   '(glob search)))))

(ert-deftest e-resources-test-errors-for-unknown-and-unsupported-resources ()
  "The registry reports missing schemes and unsupported operations clearly."
  (let ((registry (e-resources-registry-create)))
    (e-resources-register
     registry
     (e-resource-method-create
      :scheme "readonly"
      :operation e-operation-read
      :handler (lambda (_uri _range) "ok")))
    (should-error (e-resources-read registry "missing://target")
                  :type 'e-resources-unknown-scheme)
    (should-error (e-resources-write registry "readonly://target" "content")
                  :type 'e-resources-unsupported-operation)
    (should-error (e-resources-read registry "not-a-uri")
                  :type 'e-resources-invalid-uri)))

(ert-deftest e-resources-test-register-functions-can-contribute-methods ()
  "Capability-style registration functions can add resource methods."
  (let ((registry (e-resources-registry-create)))
    (e-resources-register
     registry
     (lambda (actual-registry)
       (e-resources-register
        actual-registry
        (e-resource-method-create
         :scheme "generated"
         :operation e-operation-read
         :handler (lambda (_uri _range) "generated")))))
    (should (equal (e-resources-read registry "generated://value" nil)
                   "generated"))))

(ert-deftest e-resources-test-reports-active-operations-and-methods ()
  "The registry exposes active methods grouped by operation."
  (let ((registry (e-resources-registry-create)))
    (e-resources-register
     registry
     (e-resource-method-create
      :scheme "first"
      :operation e-operation-read
      :description "First resources."
      :uri-patterns '("first://<id>")
      :handler (lambda (_uri _range) "first")))
    (e-resources-register
     registry
     (e-resource-method-create
      :scheme "second"
      :operation e-operation-read
      :description "Second resources."
      :uri-patterns '("second://<id>")
      :handler (lambda (_uri _range) "second")))
    (should (equal (mapcar #'e-operation-id
                           (e-resources-operations registry))
                   '(read)))
    (should (equal (mapcar #'e-resource-method-scheme
                           (e-resources-methods-for-operation
                            registry e-operation-read))
                   '("first" "second")))))

(provide 'e-resources-test)

;;; e-resources-test.el ends here

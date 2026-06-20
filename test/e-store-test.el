;;; e-store-test.el --- Tests for e in-memory resource store -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for capability-scoped e:// resources.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-store)

(ert-deftest e-store-test-registers-and-lists-path-addressable-resources ()
  "The store exposes registered resources under capability-scoped e:// URIs."
  (let ((store (e-store-create)))
    (e-store-register
     store
     'planner
     "skills/plan"
     :description "Planning skill."
     :content "Plan carefully.")
    (e-store-register
     store
     "planner"
     "refs/checklist.md"
     :description "Checklist reference."
     :content "Check everything.")
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://planner/skills/plan"
                     "e://planner/refs/checklist.md")))
    (should (equal (e-store-read store "e://planner/skills/plan" nil)
                   "Plan carefully."))))

(ert-deftest e-store-test-reader-provider-receives-entry-and-range ()
  "Dynamic store readers receive their entry and the requested range."
  (let* ((captured nil)
         (store (e-store-create))
         (entry (e-store-register
                 store
                 'dynamic
                 "refs/log.md"
                 :reader (lambda (actual-entry range)
                           (setq captured
                                 (list (e-store-entry-uri actual-entry)
                                       range))
                           "dynamic content"))))
    (should (equal (e-store-read
                    store
                    (e-store-entry-uri entry)
                    '(:unit "line" :start 1 :end 2))
                   "dynamic content"))
    (should (equal captured
                   '("e://dynamic/refs/log.md"
                     (:unit "line" :start 1 :end 2))))))

(ert-deftest e-store-test-rejects-duplicate-uris ()
  "Duplicate e:// store paths are configuration errors."
  (let ((store (e-store-create)))
    (e-store-register store 'duplicate "refs/a.md" :content "a")
    (should-error
     (e-store-register store 'duplicate "refs/a.md" :content "b")
     :type 'e-store-duplicate-uri)))

(ert-deftest e-store-test-errors-for-invalid-and-unknown-uris ()
  "The store rejects non-e URIs and unknown e:// resources."
  (let ((store (e-store-create)))
    (e-store-register store 'known "refs/a.md" :content "a")
    (should-error (e-store-read store "skill://known" nil)
                  :type 'e-store-invalid-uri)
    (should-error (e-store-read store "e://known/refs/missing.md" nil)
                  :type 'e-store-unknown-resource)
    (should-error (e-store-read store "e:///" nil)
                  :type 'e-store-invalid-uri)))

(ert-deftest e-store-test-resource-methods-are-read-only-with-discovery ()
  "The e:// resource methods support read, glob, and search but not mutation."
  (let ((store (e-store-create))
        (resources (e-resources-registry-create))
        captured)
    (e-store-register store 'readonly "refs/a.md" :content "readable")
    (e-store-register store 'readonly "skills/guide" :content "Guide needle")
    (e-store-register
     store
     'dynamic
     "refs/log.md"
     :reader (lambda (entry range)
               (setq captured (list (e-store-entry-uri entry) range))
               "dynamic needle"))
    (e-resources-register resources (e-store-resource-methods store))
    (should (equal (e-resources-read resources "e://readonly/refs/a.md" nil)
                   "readable"))
    (should
     (equal (e-resources-glob resources "e://readonly" "skills/*" 5)
            '(:resources [(:uri "e://readonly/skills/guide"
                            :name "skills/guide"
                            :kind resource)]
              :truncated nil)))
    (should
     (equal (e-resources-search
             resources
             "e://"
             "needle"
             '(:glob "*/refs/*" :limit 5))
            '(:matches [(:uri "e://dynamic/refs/log.md"
                          :line 1
                          :column 9
                          :text "dynamic needle")]
              :truncated nil)))
    (should (equal captured '("e://dynamic/refs/log.md" nil)))
    (should (equal (mapcar #'e-operation-id
                           (e-resources-operations resources))
                   '(read glob search)))
    (should-error (e-resources-write resources "e://readonly/refs/a.md" "no")
                  :type 'e-resources-unsupported-operation)
    (should-error (e-resources-edit resources "e://readonly/refs/a.md"
                                    '((:oldText "read" :newText "write")))
                  :type 'e-resources-unsupported-operation)))

(provide 'e-store-test)

;;; e-store-test.el ends here

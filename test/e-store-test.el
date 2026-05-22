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

(ert-deftest e-store-test-resource-method-is-read-only ()
  "The e:// resource method supports read but not write or edit."
  (let ((store (e-store-create))
        (resources (e-resources-registry-create)))
    (e-store-register store 'readonly "refs/a.md" :content "readable")
    (e-resources-register resources (e-store-resource-method store))
    (should (equal (e-resources-read resources "e://readonly/refs/a.md" nil)
                   "readable"))
    (should-error (e-resources-write resources "e://readonly/refs/a.md" "no")
                  :type 'e-resources-unsupported-operation)
    (should-error (e-resources-edit resources "e://readonly/refs/a.md"
                                    '((:oldText "read" :newText "write")))
                  :type 'e-resources-unsupported-operation)))

(provide 'e-store-test)

;;; e-store-test.el ends here

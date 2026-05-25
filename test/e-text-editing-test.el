;;; e-text-editing-test.el --- Tests for text editing layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for text-editing guidance capabilities.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-capabilities)
(require 'e-store)
(require 'e-text-editing)

(ert-deftest e-text-editing-test-layer-provides-annotations-guidance-only ()
  "The text-editing layer exposes annotation guidance without tools."
  (let* ((layer (e-text-editing-layer-create))
         (capability (car (e-layer-capabilities layer))))
    (should (eq (e-layer-id layer) 'text-editing))
    (should (eq (e-capability-id capability) 'annotations))
    (should-not (e-capability-tools capability))
    (should-not (e-capability-resource-methods capability))
    (should (string-match-p
             "simply-annotate: Work with Simply Annotate annotation databases and threaded replies. Read e://annotations/skills/simply-annotate"
             (e-capability-instructions capability)))))

(ert-deftest e-text-editing-test-annotations-skill-is-readable ()
  "Annotation details are registered as a read-only skill resource."
  (let* ((store (e-store-create))
         (capability (e-text-editing-annotations-capability-create)))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://annotations/skills/simply-annotate")))
    (let ((content (e-store-read store
                                 "e://annotations/skills/simply-annotate"
                                 nil)))
      (should (string-match-p "Working with Simply Annotate annotations" content))
      (should (string-match-p "Add replies as additional comment alists" content))
      (should (string-match-p "Do not evaluate annotation database content as code" content)))))

(provide 'e-text-editing-test)

;;; e-text-editing-test.el ends here

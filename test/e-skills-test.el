;;; e-skills-test.el --- Tests for e skill catalog resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for skill descriptors backed by the generic e:// store.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-skills)
(require 'e-store)

(ert-deftest e-skills-test-create-descriptor ()
  "A skill descriptor carries compact model-facing catalog metadata."
  (let ((skill (e-skill-create
                :name "code-review"
                :description "Review code changes."
                :content "Review the diff carefully.")))
    (should (equal (e-skill-name skill) "code-review"))
    (should (equal (e-skill-description skill) "Review code changes."))
    (should (equal (e-skills-uri-for-name 'review "code-review")
                   "e://review/skills/code-review"))))

(ert-deftest e-skills-test-registers-skill-as-e-store-resource ()
  "Skills register as read-only e:// resources under the capability namespace."
  (let ((store (e-store-create)))
    (e-skills-register
     store
     'review
     (e-skill-create
      :name "code-review"
      :description "Review code changes."
      :content "Review the diff carefully."))
    (should (equal (e-store-read store
                                 "e://review/skills/code-review"
                                 nil)
                   "Review the diff carefully."))))

(ert-deftest e-skills-test-register-functions-can-contribute-skills ()
  "Capability-style skill providers can add skills to the e:// store."
  (let ((store (e-store-create)))
    (e-skills-register
     store
     'generated
     (lambda (actual-store capability-name)
       (e-skills-register
        actual-store
        capability-name
        (e-skill-create
         :name "dynamic"
         :description "Generated skill."
         :content "Generated instructions."))))
    (should (equal (mapcar
                    (lambda (item) (plist-get item :uri))
                    (e-skills-list store))
                   '("e://generated/skills/dynamic")))))

(ert-deftest e-skills-test-catalog-lists-skills-without-full-content ()
  "The skill catalog includes compact metadata and e:// URIs only."
  (let ((store (e-store-create)))
    (e-skills-register
     store
     'review
     (e-skill-create
      :name "code-review"
      :description "Review code changes."
      :content "Secret detailed checklist."))
    (e-store-register
     store
     'review
     "refs/checklist.md"
     :description "Reference checklist."
     :content "Reference content.")
    (let ((catalog (e-skills-catalog-text store)))
      (should (string-match-p "Available skills" catalog))
      (should (string-match-p "code-review" catalog))
      (should (string-match-p "Review code changes" catalog))
      (should (string-match-p "e://review/skills/code-review" catalog))
      (should-not (string-match-p "Secret detailed checklist" catalog))
      (should-not (string-match-p "checklist.md" catalog)))))

(provide 'e-skills-test)

;;; e-skills-test.el ends here

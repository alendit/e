;;; e-skills-test.el --- Tests for e skill builder resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for skill specs that build ordinary capabilities.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-capabilities)
(require 'e-skills)
(require 'e-store)

(ert-deftest e-skills-test-builder-returns-capability-with-preamble-only ()
  "Skill specs build an ordinary capability with compact instructions."
  (let* ((capability
          (e-capability-with-skills-create
           :id 'code-assistant
           :name "Code Assistant"
           :instructions "Use code capabilities."
           :skills
           (list
            (e-skill-spec-create
             :name "code-review"
             :description "Review code changes."
             :content "Secret review checklist.")
            (e-skill-spec-create
             :name "debugging"
             :description "Diagnose failures."
             :content "Secret debugging workflow."))))
         (instructions (e-capability-instructions capability)))
    (should (e-capability-p capability))
    (should (equal (e-capability-id capability) 'code-assistant))
    (should (string-match-p "Use code capabilities." instructions))
    (should (string-match-p "Additional guidance is available on demand"
                            instructions))
    (should (string-match-p
             "code-review: Review code changes. Read e://code-assistant/skills/code-review"
             instructions))
    (should (string-match-p
             "debugging: Diagnose failures. Read e://code-assistant/skills/debugging"
             instructions))
    (should-not (string-match-p "Secret review checklist" instructions))
    (should-not (string-match-p "Secret debugging workflow" instructions))))

(ert-deftest e-skills-test-builder-registers-multiple-skill-resources ()
  "Generated skill resources register in order and remain readable on demand."
  (let* ((captured nil)
         (store (e-store-create))
         (range '(:unit "line" :start 1 :end 2))
         (reader (lambda (skill actual-range)
                   (setq captured (list skill actual-range))
                   "Generated dynamic content."))
         (capability
          (e-capability-with-skills-create
           :id 'code-assistant
           :skills
           (list
            (e-skill-spec-create
             :name "code-review"
             :description "Review code changes."
             :content "Review content.")
            (e-skill-spec-create
             :name "debugging"
             :description "Diagnose failures."
             :reader reader)))))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://code-assistant/skills/code-review"
                     "e://code-assistant/skills/debugging")))
    (should (equal (e-store-read store
                                 "e://code-assistant/skills/code-review"
                                 nil)
                   "Review content."))
    (should (equal (e-store-read store
                                 "e://code-assistant/skills/debugging"
                                 range)
                   "Generated dynamic content."))
    (should (equal (e-skill-spec-name (car captured)) "debugging"))
    (should (eq (cadr captured) range))))

(ert-deftest e-skills-test-spec-validation-and-explicit-path ()
  "Skill specs validate required fields and honor explicit resource paths."
  (should-error (e-skill-spec-create
                 :name ""
                 :description "Invalid."
                 :content "Body."))
  (should-error (e-skill-spec-create
                 :name "bad/name"
                 :description "Invalid."
                 :content "Body."))
  (should-error (e-skill-spec-create
                 :name "missing-body"
                 :description "Invalid."))
  (should-error (e-skill-spec-create
                 :name "ambiguous-body"
                 :description "Invalid."
                 :content "Body."
                 :reader (lambda (_skill _range) "Dynamic body.")))
  (should-error (e-skill-spec-create
                 :name "bad-path"
                 :description "Invalid."
                 :path "/skills/bad-path"
                 :content "Body."))
  (let* ((store (e-store-create))
         (capability
          (e-capability-with-skills-create
           :id 'assistant
           :skills
           (list
            (e-skill-spec-create
             :name "grouped/name"
             :description "Explicit path skill."
             :path "guides/grouped-name.md"
             :content "Explicit path body.")))))
    (should (string-match-p
             "e://assistant/guides/grouped-name.md"
             (e-capability-instructions capability)))
    (e-capabilities-register-resources capability store)
    (should (equal (e-store-read store
                                 "e://assistant/guides/grouped-name.md"
                                 nil)
                   "Explicit path body."))))

(ert-deftest e-skills-test-static-content-is-normalized-to-reader ()
  "Static skill content is stored through the same callable reader contract."
  (let ((skill (e-skill-spec-create
                :name "static"
                :description "Static guidance."
                :content "Static body.")))
    (should (functionp (e-skill-spec-reader skill)))
    (should-not (fboundp 'e-skill-spec-content))
    (should (equal (funcall (e-skill-spec-reader skill) skill nil)
                   "Static body."))))

(ert-deftest e-skills-test-builder-preserves-caller-resources ()
  "Generated skill providers are appended after caller-provided resources."
  (let* ((store (e-store-create))
         (existing-provider
          (lambda (actual-store capability)
            (e-store-register
             actual-store
             (e-capability-id capability)
             "refs/checklist.md"
             :description "Reference checklist."
             :content "Reference content.")))
         (capability
          (e-capability-with-skills-create
           :id 'review
           :resources (list existing-provider)
           :skills
           (list
            (e-skill-spec-create
             :name "code-review"
             :description "Review code changes."
             :content "Review content.")))))
    (e-capabilities-register-resources capability store)
    (should (equal (mapcar #'e-store-entry-uri (e-store-list store))
                   '("e://review/refs/checklist.md"
                     "e://review/skills/code-review")))))

(ert-deftest e-skills-test-builder-uses-custom-heading ()
  "Callers can override the generated skill preamble heading."
  (let* ((heading "Load targeted guidance when useful:")
         (capability
          (e-capability-with-skills-create
           :id 'review
           :skill-heading heading
           :skills
           (list
            (e-skill-spec-create
             :name "code-review"
             :description "Review code changes."
             :content "Review content."))))
         (instructions (e-capability-instructions capability)))
    (should (string-match-p (regexp-quote heading) instructions))
    (should (string-match-p
             "code-review: Review code changes. Read e://review/skills/code-review"
             instructions))
    (should-not (string-match-p
                 "Additional guidance is available on demand"
                 instructions))))

(ert-deftest e-skills-test-builder-preserves-non-resource-fields ()
  "Skill capabilities keep ordinary capability contribution fields."
  (let* ((tool-provider (lambda (_registry) :tool))
         (resource-method-provider (lambda (_registry) :resource-method))
         (context-provider (lambda (_context) nil))
         (action (lambda () :action))
         (actions (list :inspect action))
         (capability
          (e-capability-with-skills-create
           :id 'assistant
           :tools (list tool-provider)
           :resource-methods (list resource-method-provider)
           :context-providers (list context-provider)
           :instruction-priority 220
           :actions actions
           :config-options '(:declared)
           :config '(:configured t)
           :skills
           (list
            (e-skill-spec-create
             :name "focused"
             :description "Stay focused."
             :content "Focus content.")))))
    (should (equal (e-capability-tools capability)
                   (list tool-provider)))
    (should (equal (e-capability-resource-methods capability)
                   (list resource-method-provider)))
    (should (equal (e-capability-context-providers capability)
                   (list context-provider)))
    (should (= (e-capability-instruction-priority capability) 220))
    (should (equal (e-capability-config-options capability) '(:declared)))
    (should (equal (e-capability-config capability) '(:configured t)))
    (should (eq (e-capabilities-action-spec capability :inspect) action))))

(ert-deftest e-skills-test-builder-does-not-add-empty-preamble ()
  "A capability without skills keeps caller instructions unchanged."
  (let ((capability
         (e-capability-with-skills-create
          :id 'plain
          :instructions "Plain instructions.")))
    (should (equal (e-capability-instructions capability)
                   "Plain instructions."))
    (should-not (e-capability-resources capability))))

(provide 'e-skills-test)

;;; e-skills-test.el ends here

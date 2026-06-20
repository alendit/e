;;; e-prompts-test.el --- Tests for prompt specs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for capability-contributed prompt templates.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-prompts)

(ert-deftest e-prompts-test-spec-validation-and-template-render ()
  "Prompt specs validate construction and render templates."
  (let* ((prompt (e-prompt-spec-create
                  :name "explain"
                  :description "Explain something."
                  :parameters
                  (list (e-prompt-parameter-create
                         :name 'focus
                         :description "Focus area."))
                  :template "Explain this, focusing on ${focus}.")))
    (should (e-prompt-spec-p prompt))
    (should (equal (e-prompt-spec-name prompt) "explain"))
    (should (equal (e-prompt-render prompt '((focus . "behavior")))
                   "Explain this, focusing on behavior."))))

(ert-deftest e-prompts-test-spec-rejects-invalid-shapes ()
  "Prompt specs reject invalid names, descriptions, parameters, and bodies."
  (should-error
   (e-prompt-spec-create
    :name ""
    :description "Description."
    :template "Body."))
  (should-error
   (e-prompt-spec-create
    :name "bad/name"
    :description "Description."
    :template "Body."))
  (should-error
   (e-prompt-spec-create
    :name "ok"
    :description ""
    :template "Body."))
  (should-error
   (e-prompt-spec-create
    :name "ok"
    :description "Description."
    :parameters (list "not a parameter")
    :template "Body."))
  (should-error
   (e-prompt-spec-create
    :name "ok"
    :description "Description."
    :parameters
    (list (e-prompt-parameter-create :name "focus" :description "One.")
          (e-prompt-parameter-create :name 'focus :description "Two."))
    :template "Body."))
  (should-error
   (e-prompt-spec-create
    :name "ok"
    :description "Description."
    :template "Body."
    :renderer #'ignore))
  (should-error
   (e-prompt-spec-create
    :name "ok"
    :description "Description.")))

(ert-deftest e-prompts-test-parameter-validation ()
  "Prompt parameters validate names, descriptions, and types."
  (should (e-prompt-parameter-p
           (e-prompt-parameter-create
            :name 'topic
            :description "Topic."
            :required nil
            :default "anything"
            :type 'text)))
  (should-error
   (e-prompt-parameter-create :name "" :description "Description."))
  (should-error
   (e-prompt-parameter-create :name "bad/name" :description "Description."))
  (should-error
   (e-prompt-parameter-create :name "ok" :description ""))
  (should-error
   (e-prompt-parameter-create
    :name "ok"
    :description "Description."
    :type 'integer)))

(ert-deftest e-prompts-test-render-applies-defaults-and-validates-arguments ()
  "Rendering applies defaults and rejects missing or unknown arguments."
  (let ((prompt (e-prompt-spec-create
                 :name "summarize"
                 :description "Summarize something."
                 :parameters
                 (list (e-prompt-parameter-create
                        :name "focus"
                        :description "Focus."
                        :required nil
                        :default "overall behavior")
                       (e-prompt-parameter-create
                        :name "audience"
                        :description "Audience."))
                 :template "Summarize for ${audience}; focus on ${focus}.")))
    (should (equal (e-prompt-render prompt '(("audience" . "maintainers")))
                   "Summarize for maintainers; focus on overall behavior."))
    (should-error (e-prompt-render prompt nil))
    (should-error
     (e-prompt-render prompt '(("audience" . "maintainers")
                               ("extra" . "ignored"))))))

(ert-deftest e-prompts-test-render-rejects-unknown-template-placeholders ()
  "Template placeholders must correspond to declared parameters."
  (let ((prompt (e-prompt-spec-create
                 :name "bad-template"
                 :description "Bad template."
                 :parameters nil
                 :template "Use ${missing}.")))
    (should-error (e-prompt-render prompt nil))))

(ert-deftest e-prompts-test-custom-renderer ()
  "Custom renderers receive the prompt and normalized arguments."
  (let* ((captured nil)
         (prompt (e-prompt-spec-create
                  :name "custom"
                  :description "Custom renderer."
                  :parameters
                  (list (e-prompt-parameter-create
                         :name "value"
                         :description "Value."))
                  :renderer
                  (lambda (prompt arguments)
                    (setq captured (list prompt arguments))
                    (concat "value=" (cdr (assoc "value" arguments)))))))
    (should (equal (e-prompt-render prompt '((value . "42")))
                   "value=42"))
    (should (eq (car captured) prompt))
    (should (equal (cadr captured) '(("value" . "42"))))))

(ert-deftest e-prompts-test-capability-builder ()
  "Prompt builder returns an ordinary capability with prompt specs."
  (let* ((prompt (e-prompt-spec-create
                  :name "review"
                  :description "Review something."
                  :template "Review this."))
         (capability (e-capability-with-prompts-create
                      :id 'review-prompts
                      :name "Review Prompts"
                      :instructions "Use these prompts."
                      :tools (list #'ignore)
                      :prompts (list prompt))))
    (should (eq (e-capability-id capability) 'review-prompts))
    (should (equal (e-capability-instructions capability)
                   "Use these prompts."))
    (should (equal (e-capability-prompts capability) (list prompt)))
    (should (= (length (e-capability-tools capability)) 1))))

(provide 'e-prompts-test)

;;; e-prompts-test.el ends here

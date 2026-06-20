;;; e-prompts.el --- Prompt specs for e capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Prompts are construction-time, capability-contributed text templates.  They
;; produce user-message text; shells own argument collection and submission.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'subr-x)

(cl-defstruct (e-prompt-parameter
               (:constructor e-prompt-parameter--create
                             (&key name description required default type)))
  name
  description
  (required t)
  default
  (type 'string))

(cl-defstruct (e-prompt-spec
               (:constructor e-prompt-spec--create
                             (&key name description parameters renderer
                                   metadata)))
  name
  description
  parameters
  renderer
  metadata)

(defconst e-prompt-parameter-types '(string text choice)
  "Supported prompt parameter types.")

(defun e-prompts--normalize-name (name kind)
  "Return normalized prompt or parameter NAME for KIND."
  (let ((name (cond
               ((symbolp name) (symbol-name name))
               ((stringp name) name)
               (t (signal 'wrong-type-argument (list 'string-or-symbol-p name))))))
    (when (string-empty-p name)
      (signal 'wrong-type-argument (list kind name)))
    (when (string-match-p "/" name)
      (signal 'wrong-type-argument (list kind name)))
    name))

(defun e-prompts--normalize-description (description kind)
  "Return normalized DESCRIPTION for KIND."
  (unless (stringp description)
    (signal 'wrong-type-argument (list 'stringp description)))
  (when (string-empty-p description)
    (signal 'wrong-type-argument (list kind description)))
  description)

(cl-defun e-prompt-parameter-create
    (&key name description (required t) default (type 'string))
  "Create a prompt parameter spec.
NAME is a symbol or string key.  DESCRIPTION is one-line help for shells.
TYPE is one of `string', `text', or `choice'."
  (unless (memq type e-prompt-parameter-types)
    (signal 'wrong-type-argument (list 'e-prompt-parameter-type-p type)))
  (e-prompt-parameter--create
   :name (e-prompts--normalize-name name 'prompt-parameter-name)
   :description (e-prompts--normalize-description
                 description
                 'prompt-parameter-description)
   :required required
   :default default
   :type type))

(defun e-prompts--parameter-names (parameters)
  "Return normalized names for PARAMETERS, validating their shape."
  (let (names)
    (dolist (parameter parameters)
      (unless (e-prompt-parameter-p parameter)
        (signal 'wrong-type-argument (list 'e-prompt-parameter-p parameter)))
      (let ((name (e-prompt-parameter-name parameter)))
        (when (member name names)
          (signal 'wrong-type-argument
                  (list 'unique-prompt-parameter-name name)))
        (push name names)))
    (nreverse names)))

(defun e-prompts--template-renderer (template)
  "Return a renderer for TEMPLATE."
  (lambda (_prompt arguments)
    (let ((start 0)
          parts)
      (while (string-match "\\${\\([^}]+\\)}" template start)
        (push (substring template start (match-beginning 0)) parts)
        (let* ((name (match-string 1 template))
               (value (assoc name arguments)))
          (unless value
            (signal 'wrong-type-argument
                    (list 'known-prompt-placeholder name)))
          (push (cdr value) parts))
        (setq start (match-end 0)))
      (push (substring template start) parts)
      (apply #'concat (nreverse parts)))))

(cl-defun e-prompt-spec-create
    (&key name description parameters template renderer metadata)
  "Create a prompt spec.
Exactly one of TEMPLATE or RENDERER must be supplied.  TEMPLATE strings support
`${name}' substitution for declared parameters."
  (let ((name (e-prompts--normalize-name name 'prompt-name))
        (description (e-prompts--normalize-description
                      description
                      'prompt-description))
        (parameters (or parameters nil)))
    (e-prompts--parameter-names parameters)
    (when (and template renderer)
      (signal 'wrong-type-argument (list 'prompt-body name)))
    (setq renderer
          (cond
           ((stringp template) (e-prompts--template-renderer template))
           ((functionp renderer) renderer)
           (t nil)))
    (unless renderer
      (signal 'wrong-type-argument (list 'string-or-function-p name)))
    (e-prompt-spec--create
     :name name
     :description description
     :parameters parameters
     :renderer renderer
     :metadata metadata)))

(defun e-prompts--argument-name (argument)
  "Return normalized key name for ARGUMENT."
  (e-prompts--normalize-name argument 'prompt-argument-name))

(defun e-prompts--argument-value (parameter value)
  "Return normalized VALUE for PARAMETER."
  (pcase (e-prompt-parameter-type parameter)
    ((or 'string 'text 'choice)
     (unless (stringp value)
       (signal 'wrong-type-argument (list 'stringp value)))
     value)
    (_ value)))

(defun e-prompts--normalize-arguments (prompt arguments)
  "Validate ARGUMENTS for PROMPT and return a string-keyed alist."
  (let ((provided (mapcar (lambda (argument)
                            (cons (e-prompts--argument-name (car argument))
                                  (cdr argument)))
                          arguments))
        normalized)
    (dolist (argument provided)
      (unless (cl-find (car argument)
                       (e-prompt-spec-parameters prompt)
                       :key #'e-prompt-parameter-name
                       :test #'equal)
        (signal 'wrong-type-argument
                (list 'known-prompt-argument (car argument)))))
    (dolist (parameter (e-prompt-spec-parameters prompt))
      (let* ((name (e-prompt-parameter-name parameter))
             (argument (assoc name provided)))
        (cond
         (argument
          (push (cons name (e-prompts--argument-value parameter (cdr argument)))
                normalized))
         ((not (e-prompt-parameter-required parameter))
          (push (cons name (e-prompts--argument-value
                            parameter
                            (e-prompt-parameter-default parameter)))
                normalized))
         (t
          (signal 'wrong-type-argument
                  (list 'required-prompt-argument name))))))
    (nreverse normalized)))

(defun e-prompt-render (prompt arguments)
  "Render PROMPT with ARGUMENTS and return final prompt text."
  (unless (e-prompt-spec-p prompt)
    (signal 'wrong-type-argument (list 'e-prompt-spec-p prompt)))
  (let ((result (funcall (e-prompt-spec-renderer prompt)
                         prompt
                         (e-prompts--normalize-arguments prompt arguments))))
    (unless (stringp result)
      (signal 'wrong-type-argument (list 'stringp result)))
    result))

(cl-defun e-capability-with-prompts-create
    (&key id name instructions prompts tools resource-methods resources
          context-providers actions hooks instruction-priority
          config-options config)
  "Create an ordinary capability with prompt specs.
PROMPTS are construction-time `e-prompt-spec' values."
  (dolist (prompt prompts)
    (unless (e-prompt-spec-p prompt)
      (signal 'wrong-type-argument (list 'e-prompt-spec-p prompt))))
  (e-capability-create
   :id id
   :name name
   :instructions instructions
   :tools tools
   :resource-methods resource-methods
   :resources resources
   :context-providers context-providers
   :actions actions
   :hooks hooks
   :instruction-priority instruction-priority
   :config-options config-options
   :config config
   :prompts prompts))

(provide 'e-prompts)

;;; e-prompts.el ends here

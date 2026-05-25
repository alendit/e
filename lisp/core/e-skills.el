;;; e-skills.el --- Skill builder sugar for e capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Skills are construction-time specs for progressive guidance.  They build an
;; ordinary capability whose instructions point to read-only e:// resources.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-store)
(require 'subr-x)

(cl-defstruct (e-skill-spec
               (:constructor e-skill-spec--create
                             (&key name description reader path metadata)))
  name
  description
  reader
  path
  metadata)

(defconst e-skills-default-heading
  "Additional guidance is available on demand. Load only what is relevant:"
  "Default heading for generated skill preambles.")

(defun e-skills--normalize-name (name path)
  "Return normalized skill NAME for PATH."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (when (string-empty-p name)
    (signal 'wrong-type-argument (list 'skill-name name)))
  (when (and (not path)
             (string-match-p "/" name))
    (signal 'wrong-type-argument (list 'skill-name name)))
  name)

(defun e-skills--normalize-description (description)
  "Return normalized skill DESCRIPTION."
  (unless (stringp description)
    (signal 'wrong-type-argument (list 'stringp description)))
  (when (string-empty-p description)
    (signal 'wrong-type-argument (list 'skill-description description)))
  description)

(defun e-skills--normalize-path (path)
  "Return normalized explicit skill PATH."
  (when path
    (e-store--normalize-path path)))

(cl-defun e-skill-spec-create (&key name description content reader path metadata)
  "Create a construction-time skill spec.
NAME and DESCRIPTION are compact model-facing metadata.  CONTENT is a static
string convenience that is normalized into the same callable READER contract
used for dynamic guidance bodies.  When PATH is omitted, the resource path is
`skills/NAME' and NAME must not contain slashes.  When PATH is explicit, NAME
is display metadata and may contain slashes."
  (let* ((path (e-skills--normalize-path path))
         (name (e-skills--normalize-name name path))
         (description (e-skills--normalize-description description)))
    (when (and content reader)
      (signal 'wrong-type-argument (list 'skill-body name)))
    (setq reader
          (cond
           ((stringp content)
            (lambda (_skill _range) content))
           ((functionp reader) reader)
           (t nil)))
    (unless reader
      (signal 'wrong-type-argument (list 'string-or-function-p name)))
    (e-skill-spec--create
     :name name
     :description description
     :reader reader
     :path path
     :metadata metadata)))

(defun e-skills--path (skill)
  "Return the store path for SKILL."
  (or (e-skill-spec-path skill)
      (format "skills/%s" (e-skill-spec-name skill))))

(defun e-skills-uri-for-name (capability name)
  "Return the conventional skill resource URI for CAPABILITY and NAME."
  (e-store-uri capability
               (format "skills/%s" (e-skills--normalize-name name nil))))

(defun e-skills--uri-for-spec (capability skill)
  "Return the resource URI for SKILL under CAPABILITY."
  (e-store-uri capability (e-skills--path skill)))

(defun e-skills--preamble (capability skills heading)
  "Return generated preamble text for SKILLS under CAPABILITY."
  (when skills
    (string-join
     (cons
      (or heading e-skills-default-heading)
      (mapcar
       (lambda (skill)
         (format "- %s: %s Read %s"
                 (e-skill-spec-name skill)
                 (e-skill-spec-description skill)
                 (e-skills--uri-for-spec capability skill)))
       skills))
     "\n")))

(defun e-skills--instructions (instructions preamble)
  "Return INSTRUCTIONS with PREAMBLE appended when present."
  (cond
   ((not preamble) instructions)
   ((and (stringp instructions)
         (not (string-empty-p instructions)))
    (concat instructions "\n\n" preamble))
   (t preamble)))

(defun e-skills--register-spec (store capability skill)
  "Register SKILL under CAPABILITY in STORE."
  (unless (e-skill-spec-p skill)
    (signal 'wrong-type-argument (list 'e-skill-spec-p skill)))
  (e-store-register
   store
   capability
   (e-skills--path skill)
   :description (e-skill-spec-description skill)
   :reader (lambda (_entry range)
             (funcall (e-skill-spec-reader skill) skill range))
   :metadata (e-skill-spec-metadata skill)))

(defun e-skills--resource-provider (skills)
  "Return a capability resource provider for SKILLS."
  (lambda (store capability)
    (dolist (skill skills)
      (e-skills--register-spec store (e-capability-id capability) skill))))

(cl-defun e-capability-with-skills-create
    (&key id name instructions skills tools resource-methods resources
          context-providers actions instruction-priority skill-heading
          config-options config)
  "Create an ordinary capability with discoverable skill resources.
SKILLS are construction-time `e-skill-spec' values.  The returned object is an
`e-capability' whose instructions contain compact skill references and whose
resources register each full skill body under e://."
  (dolist (skill skills)
    (unless (e-skill-spec-p skill)
      (signal 'wrong-type-argument (list 'e-skill-spec-p skill))))
  (let* ((preamble (e-skills--preamble id skills skill-heading))
         (all-resources
          (append resources
                  (when skills
                    (list (e-skills--resource-provider skills))))))
    (e-capability-create
     :id id
     :name name
     :instructions (e-skills--instructions instructions preamble)
     :tools tools
     :resource-methods resource-methods
     :resources all-resources
     :context-providers context-providers
     :instruction-priority instruction-priority
     :actions actions
     :config-options config-options
     :config config)))

(provide 'e-skills)

;;; e-skills.el ends here

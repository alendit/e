;;; e-skills.el --- Skill catalog entries backed by e:// resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Skills are compact capability-contributed descriptors.  Their full
;; instructions are stored as read-only e:// resources under the conventional
;; path e://<capability>/skills/<skill-name>.

;;; Code:

(require 'cl-lib)
(require 'e-store)
(require 'subr-x)

(cl-defstruct (e-skill
               (:constructor e-skill--create
                             (&key name description content reader metadata)))
  name
  description
  content
  reader
  metadata)

(defun e-skills--normalize-name (name)
  "Return normalized skill NAME."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (when (or (string-empty-p name)
            (string-match-p "/" name))
    (signal 'wrong-type-argument (list 'skill-name name)))
  name)

(defun e-skills--normalize-description (description)
  "Return normalized skill DESCRIPTION."
  (unless (stringp description)
    (signal 'wrong-type-argument (list 'stringp description)))
  description)

(defun e-skills-uri-for-name (capability name)
  "Return the conventional skill resource URI for CAPABILITY and NAME."
  (e-store-uri capability (format "skills/%s" (e-skills--normalize-name name))))

(cl-defun e-skill-create (&key name description content reader metadata)
  "Create a skill descriptor.
NAME and DESCRIPTION are compact catalog metadata.  CONTENT or READER provides
the full instructions stored under the capability's e:// skills path."
  (let ((name (e-skills--normalize-name name))
        (description (e-skills--normalize-description description)))
    (unless (or (stringp content) (functionp reader))
      (signal 'wrong-type-argument (list 'string-or-function-p name)))
    (e-skill--create
     :name name
     :description description
     :content content
     :reader reader
     :metadata metadata)))

(defun e-skills--metadata (skill)
  "Return store metadata for SKILL."
  (append (list :kind 'skill
                :skill-name (e-skill-name skill)
                :skill-description (e-skill-description skill))
          (e-skill-metadata skill)))

(defun e-skills-register (store capability skill-or-register)
  "Register SKILL-OR-REGISTER under CAPABILITY in STORE.
SKILL-OR-REGISTER may be an `e-skill' object or a function accepting STORE and
CAPABILITY."
  (cond
   ((e-skill-p skill-or-register)
    (let ((skill skill-or-register))
      (e-store-register
       store
       capability
       (format "skills/%s" (e-skill-name skill))
       :description (e-skill-description skill)
       :content (e-skill-content skill)
       :reader (when (e-skill-reader skill)
                 (lambda (_entry range)
                   (funcall (e-skill-reader skill) skill range)))
       :metadata (e-skills--metadata skill))))
   ((functionp skill-or-register)
    (funcall skill-or-register store capability))
   (t
    (signal 'wrong-type-argument (list 'e-skill-p skill-or-register)))))

(defun e-skills--entry-item (entry)
  "Return model-facing catalog item for skill ENTRY."
  (let ((metadata (e-store-entry-metadata entry)))
    (list :name (plist-get metadata :skill-name)
          :description (plist-get metadata :skill-description)
          :uri (e-store-entry-uri entry))))

(defun e-skills-list (store)
  "Return active skill catalog items from STORE in resource order."
  (let (items)
    (dolist (entry (e-store-list store))
      (when (eq (plist-get (e-store-entry-metadata entry) :kind) 'skill)
        (push (e-skills--entry-item entry) items)))
    (nreverse items)))

(defun e-skills-catalog-text (store)
  "Return model-facing catalog text for skills in STORE."
  (let ((skills (e-skills-list store)))
    (when skills
      (string-join
       (cons
        "Available skills. Load full instructions with read on the skill URI."
        (mapcar
         (lambda (skill)
           (format "- %s: %s URI: %s"
                   (plist-get skill :name)
                   (plist-get skill :description)
                   (plist-get skill :uri)))
         skills))
       "\n"))))

(provide 'e-skills)

;;; e-skills.el ends here

;;; e-agents-std-context.el --- Standard agent context layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Discovers standard agent context from AGENTS.md files and filesystem skills.
;; The layer contributes context providers and read-only e:// resources while
;; leaving durable state owned by the harness/session layer.

;;; Code:

(require 'cl-lib)
(require 'e-context)
(require 'e-layers)
(require 'e-skills)
(require 'subr-x)

(defgroup e-agents-std-context nil
  "Standard agent context discovery for e."
  :group 'e
  :prefix "e-agents-std-context-")

(defcustom e-agents-std-context-global-agents-files
  (list (expand-file-name "AGENTS.md" "~/.agents")
        (expand-file-name "AGENTS.md" "~/.codex"))
  "Global AGENTS.md files included before project AGENTS.md files."
  :type '(repeat file)
  :group 'e-agents-std-context)

(defcustom e-agents-std-context-global-skills-directory
  (expand-file-name "skills" "~/.agents")
  "Global directory containing skill subdirectories."
  :type 'directory
  :group 'e-agents-std-context)

(defconst e-agents-std-context-instructions
  "Follow discovered AGENTS.md context for this turn. Skills are available as read-only e:// resources; read a full skill resource before applying that skill."
  "Instructions contributed by the standard agent context capability.")

(defun e-agents-std-context--directory (directory)
  "Return normalized context root DIRECTORY."
  (file-name-as-directory
   (expand-file-name (or directory default-directory))))

(defun e-agents-std-context--readable-file-p (path)
  "Return non-nil when PATH is a readable regular file."
  (and (stringp path)
       (file-readable-p path)
       (not (file-directory-p path))))

(defun e-agents-std-context--read-file (path)
  "Return PATH content as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun e-agents-std-context--ancestor-directories (directory)
  "Return ancestor directories for DIRECTORY from root to DIRECTORY."
  (let ((dir (e-agents-std-context--directory directory))
        dirs parent)
    (while dir
      (push dir dirs)
      (setq parent (file-name-directory (directory-file-name dir)))
      (setq dir (unless (equal parent dir)
                  parent)))
    dirs))

(defun e-agents-std-context--project-agents-files (directory)
  "Return project AGENTS.md files discovered from DIRECTORY upward."
  (let (files)
    (dolist (dir (e-agents-std-context--ancestor-directories directory))
      (let ((path (expand-file-name "AGENTS.md" dir)))
        (when (e-agents-std-context--readable-file-p path)
          (push (expand-file-name path) files))))
    (nreverse files)))

(defun e-agents-std-context-agents-files (&optional directory)
  "Return global and project AGENTS.md files for DIRECTORY."
  (cl-delete-duplicates
   (append
    (cl-remove-if-not #'e-agents-std-context--readable-file-p
                      (mapcar #'expand-file-name
                              e-agents-std-context-global-agents-files))
    (e-agents-std-context--project-agents-files
     (e-agents-std-context--directory directory)))
   :test #'string=))

(defun e-agents-std-context--agents-section (path)
  "Return context text for AGENTS.md PATH."
  (format "## %s\n\n%s"
          (abbreviate-file-name path)
          (string-trim-right (e-agents-std-context--read-file path))))

(defun e-agents-std-context--agents-messages (directory)
  "Return context messages for discovered AGENTS.md files under DIRECTORY."
  (let ((files (e-agents-std-context-agents-files directory)))
    (when files
      (list
       (list
        :role 'system
        :content
        (string-join
         (cons "AGENTS.md context discovered for this turn:"
               (mapcar #'e-agents-std-context--agents-section files))
         "\n\n"))))))

(defun e-agents-std-context-agents-provider (&optional directory)
  "Return a context provider for AGENTS.md files under DIRECTORY."
  (let ((root (e-agents-std-context--directory directory)))
    (e-context-provider-create
     :name 'agents-std-context-agents
     :build (cl-function
             (lambda (&key harness session-id turn-id)
               (ignore harness session-id turn-id)
               (e-agents-std-context--agents-messages root))))))

(defun e-agents-std-context--frontmatter-lines (content)
  "Return YAML-like frontmatter lines from CONTENT."
  (let ((lines (split-string content "\n")))
    (when (string= (car lines) "---")
      (catch 'frontmatter
        (let ((remaining (cdr lines))
              frontmatter)
          (while remaining
            (let ((line (car remaining)))
              (if (string= line "---")
                  (throw 'frontmatter (nreverse frontmatter))
                (push line frontmatter)
                (setq remaining (cdr remaining)))))
          nil)))))

(defun e-agents-std-context--strip-quotes (value)
  "Return VALUE without matching surrounding quotes."
  (let ((trimmed (string-trim value)))
    (if (and (>= (length trimmed) 2)
             (or (and (string-prefix-p "\"" trimmed)
                      (string-suffix-p "\"" trimmed))
                 (and (string-prefix-p "'" trimmed)
                      (string-suffix-p "'" trimmed))))
        (substring trimmed 1 -1)
      trimmed)))

(defun e-agents-std-context--frontmatter-value (content key)
  "Return frontmatter KEY value from CONTENT."
  (let ((case-fold-search nil)
        (pattern (format "\\`[ \t]*%s:[ \t]*\\(.+\\)\\'" (regexp-quote key)))
        value)
    (dolist (line (e-agents-std-context--frontmatter-lines content))
      (when (and (not value)
                 (string-match pattern line))
        (setq value
              (e-agents-std-context--strip-quotes (match-string 1 line)))))
    value))

(defun e-agents-std-context--skill-directories (directory)
  "Return skill directories under DIRECTORY in stable order."
  (when (file-directory-p directory)
    (cl-remove-if-not
     #'file-directory-p
     (sort (directory-files directory t "\\`[^.]" t) #'string<))))

(defun e-agents-std-context--skill-spec (scope skill-directory)
  "Return a skill spec for SCOPE and SKILL-DIRECTORY."
  (let* ((slug (file-name-nondirectory (directory-file-name skill-directory)))
         (skill-file (expand-file-name "SKILL.md" skill-directory)))
    (when (e-agents-std-context--readable-file-p skill-file)
      (let* ((content (e-agents-std-context--read-file skill-file))
             (name (or (e-agents-std-context--frontmatter-value content "name")
                       slug))
             (description
              (or (e-agents-std-context--frontmatter-value content "description")
                  (format "Read %s guidance." name)))
             (path (format "skills/%s/%s" scope slug)))
        (e-skill-spec-create
         :name name
         :description description
         :path path
         :reader (lambda (_skill _range)
                   (e-agents-std-context--read-file skill-file))
         :metadata (list :scope scope
                         :source (abbreviate-file-name skill-file)))))))

(defun e-agents-std-context--skill-specs-from-directory (scope directory)
  "Return skill specs for SCOPE from DIRECTORY."
  (delq nil
        (mapcar (lambda (skill-directory)
                  (e-agents-std-context--skill-spec scope skill-directory))
                (e-agents-std-context--skill-directories directory))))

(defun e-agents-std-context--dedupe-skill-specs (skills)
  "Return SKILLS with later resource paths taking precedence."
  (let (paths deduped)
    (dolist (skill (reverse skills))
      (let ((path (e-skill-spec-path skill)))
        (unless (member path paths)
          (push path paths)
          (push skill deduped))))
    deduped))

(defun e-agents-std-context--project-skills-directories (directory)
  "Return project .agents/skills directories discovered from DIRECTORY upward."
  (let ((global-skills-directory
         (when (file-directory-p e-agents-std-context-global-skills-directory)
           (file-truename
            (file-name-as-directory e-agents-std-context-global-skills-directory))))
        directories)
    (dolist (dir (e-agents-std-context--ancestor-directories directory))
      (let ((skills (expand-file-name ".agents/skills" dir)))
        (when (and (file-directory-p skills)
                   (not (and global-skills-directory
                             (string=
                              (file-truename (file-name-as-directory skills))
                              global-skills-directory))))
          (push skills directories))))
    (nreverse directories)))

(defun e-agents-std-context-skill-specs (&optional directory)
  "Return discovered global and project skill specs for DIRECTORY."
  (let ((root (e-agents-std-context--directory directory))
        (skills nil))
    (setq skills
          (append skills
                  (e-agents-std-context--skill-specs-from-directory
                   "global"
                   e-agents-std-context-global-skills-directory)))
    (dolist (skills-directory
             (e-agents-std-context--project-skills-directories root))
      (setq skills
            (append skills
                    (e-agents-std-context--skill-specs-from-directory
                     "project"
                     skills-directory))))
    (e-agents-std-context--dedupe-skill-specs skills)))

(defun e-agents-std-context-capability-create (&optional directory)
  "Create the standard agent context capability rooted at DIRECTORY."
  (let ((root (e-agents-std-context--directory directory)))
    (e-capability-with-skills-create
     :id 'agents-std-context
     :name "Agents Std Context"
     :instructions e-agents-std-context-instructions
     :skills (e-agents-std-context-skill-specs root)
     :context-providers
     (list (e-agents-std-context-agents-provider root)))))

(defun e-agents-std-context-layer-create (&optional directory)
  "Create the agents-std-context layer rooted at DIRECTORY."
  (e-layer-create
   :id 'agents-std-context
   :name "Agents Std Context"
   :capabilities
   (list (e-agents-std-context-capability-create directory))))

(provide 'e-agents-std-context)

;;; e-agents-std-context.el ends here

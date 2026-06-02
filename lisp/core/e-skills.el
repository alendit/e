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


;;;; Filesystem skill discovery

(defun e-skills-normalize-directory (directory)
  "Return DIRECTORY expanded and ending in a slash.
Defaults to `default-directory' when DIRECTORY is nil."
  (file-name-as-directory
   (expand-file-name (or directory default-directory))))

(defun e-skills-readable-file-p (path)
  "Return non-nil when PATH is a readable regular file."
  (and (stringp path)
       (file-readable-p path)
       (not (file-directory-p path))))

(defun e-skills-read-file (path)
  "Return PATH content as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun e-skills-ancestor-directories (directory)
  "Return ancestor directories for DIRECTORY from filesystem root to DIRECTORY."
  (let ((dir (e-skills-normalize-directory directory))
        dirs parent)
    (while dir
      (push dir dirs)
      (setq parent (file-name-directory (directory-file-name dir)))
      (setq dir (unless (equal parent dir)
                  parent)))
    dirs))

(defun e-skills--frontmatter-lines (content)
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

(defun e-skills--strip-quotes (value)
  "Return VALUE without matching surrounding quotes."
  (let ((trimmed (string-trim value)))
    (if (and (>= (length trimmed) 2)
             (or (and (string-prefix-p "\"" trimmed)
                      (string-suffix-p "\"" trimmed))
                 (and (string-prefix-p "'" trimmed)
                      (string-suffix-p "'" trimmed))))
        (substring trimmed 1 -1)
      trimmed)))

(defun e-skills--frontmatter-block-marker-p (value)
  "Return non-nil when VALUE is a supported YAML block scalar marker."
  (string-match-p "\\`[>|][+-]?\\'" (string-trim value)))

(defun e-skills--frontmatter-top-level-key-p (line)
  "Return non-nil when LINE starts a top-level frontmatter key."
  (string-match-p "\\`[[:alnum:]_-]+:[ \t]*" line))

(defun e-skills--line-indent (line)
  "Return leading whitespace width for LINE."
  (if (string-match "\\`[ \t]*" line)
      (length (match-string 0 line))
    0))

(defun e-skills--dedent-block-lines (lines)
  "Return block scalar LINES with common indentation removed."
  (let (indent)
    (dolist (line lines)
      (unless (string-empty-p (string-trim line))
        (let ((line-indent (e-skills--line-indent line)))
          (setq indent
                (if indent
                    (min indent line-indent)
                  line-indent)))))
    (if (and indent (> indent 0))
        (mapcar (lambda (line)
                  (if (>= (length line) indent)
                      (substring line indent)
                    line))
                lines)
      lines)))

(defun e-skills--fold-frontmatter-lines (lines)
  "Return folded scalar text for dedented block LINES."
  (let (paragraphs current)
    (dolist (line lines)
      (if (string-empty-p (string-trim line))
          (when current
            (push (string-join (nreverse current) " ") paragraphs)
            (setq current nil))
        (push (string-trim line) current)))
    (when current
      (push (string-join (nreverse current) " ") paragraphs))
    (string-trim (string-join (nreverse paragraphs) "\n\n"))))

(defun e-skills--frontmatter-block-value (marker lines)
  "Return normalized block scalar value for MARKER and LINES."
  (let ((dedented (e-skills--dedent-block-lines lines)))
    (if (string-prefix-p ">" (string-trim marker))
        (e-skills--fold-frontmatter-lines dedented)
      (string-trim (string-join dedented "\n")))))

(defun e-skills--frontmatter-values (content)
  "Return YAML-like scalar frontmatter values from CONTENT as an alist."
  (let ((lines (e-skills--frontmatter-lines content))
        values)
    (while lines
      (let ((line (car lines)))
        (if (string-match "\\`[ \t]*\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)\\'"
                          line)
            (let ((key (match-string 1 line))
                  (value (match-string 2 line)))
              (setq lines (cdr lines))
              (if (e-skills--frontmatter-block-marker-p value)
                  (let (block-lines)
                    (while (and lines
                                (not
                                 (e-skills--frontmatter-top-level-key-p
                                  (car lines))))
                      (push (car lines) block-lines)
                      (setq lines (cdr lines)))
                    (push
                     (cons key
                           (e-skills--frontmatter-block-value
                            value
                            (nreverse block-lines)))
                     values))
                (push (cons key
                            (e-skills--strip-quotes value))
                      values)))
          (setq lines (cdr lines)))))
    (nreverse values)))

(defun e-skills-frontmatter-value (content key)
  "Return frontmatter KEY value from CONTENT."
  (cdr (assoc key (e-skills--frontmatter-values content))))

(defun e-skills-skill-directories (directory)
  "Return immediate skill subdirectories under DIRECTORY in stable order."
  (when (file-directory-p directory)
    (cl-remove-if-not
     #'file-directory-p
     (sort (directory-files directory t "\\`[^.]" t) #'string<))))

(defun e-skills-spec-from-skill-directory (scope skill-directory)
  "Return a skill spec for SCOPE and SKILL-DIRECTORY.
SCOPE prefixes the generated resource path as `skills/SCOPE/<slug>'.  Returns
nil when no readable SKILL.md is present."
  (let* ((slug (file-name-nondirectory (directory-file-name skill-directory)))
         (skill-file (expand-file-name "SKILL.md" skill-directory)))
    (when (e-skills-readable-file-p skill-file)
      (let* ((content (e-skills-read-file skill-file))
             (name (or (e-skills-frontmatter-value content "name")
                       slug))
             (description
              (let ((value (e-skills-frontmatter-value content "description"))
                    (fallback (format "Read %s guidance." name)))
                (if (and (stringp value)
                         (not (string-empty-p value)))
                    value
                  fallback)))
             (path (format "skills/%s/%s" scope slug)))
        (e-skill-spec-create
         :name name
         :description description
         :path path
         :reader (lambda (_skill _range)
                   (e-skills-read-file skill-file))
         :metadata (list :scope scope
                         :source (abbreviate-file-name skill-file)))))))

(defun e-skills-specs-from-directory (scope directory)
  "Return skill specs for SCOPE from immediate subdirectories of DIRECTORY."
  (delq nil
        (mapcar (lambda (skill-directory)
                  (e-skills-spec-from-skill-directory scope skill-directory))
                (e-skills-skill-directories directory))))

(provide 'e-skills)

;;; e-skills.el ends here

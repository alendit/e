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
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-context)
(require 'e-harness)
(require 'e-layers)
(require 'e-skills)
(require 'e-store)
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

(defconst e-agents-std-context-config-options
  (list
   (e-capability-config-option-create
    :key :include
    :type '(repeat string)
    :default nil
    :documentation
    "Skill names or resource paths to include.  Nil includes all skills."
    :normalizer #'e-capability-config-string-list
    :validator #'e-capability-config-string-list-p)
   (e-capability-config-option-create
    :key :exclude
    :type '(repeat string)
    :default nil
    :documentation
    "Skill names or resource paths to exclude.  Exclude wins over include."
    :normalizer #'e-capability-config-string-list
    :validator #'e-capability-config-string-list-p))
  "Configuration option specs owned by `agents-std-context'.")

(e-capability-config-register-options
 'agents-std-context e-agents-std-context-config-options)

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
     :priority 100
     :build (cl-function
             (lambda (&key harness session-id turn-id)
               (e-agents-std-context--agents-messages
                (e-agents-std-context--context-root
                 root
                 :harness harness
                 :session-id session-id
                 :turn-id turn-id)))))))

(cl-defun e-agents-std-context--context-root
    (fallback-root &key harness session-id turn-id)
  "Return session project root from HARNESS or FALLBACK-ROOT."
  (or (and harness
           (e-harness-project-root harness session-id turn-id))
      fallback-root))

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

(defun e-agents-std-context--frontmatter-block-marker-p (value)
  "Return non-nil when VALUE is a supported YAML block scalar marker."
  (string-match-p "\\`[>|][+-]?\\'" (string-trim value)))

(defun e-agents-std-context--frontmatter-top-level-key-p (line)
  "Return non-nil when LINE starts a top-level frontmatter key."
  (string-match-p "\\`[[:alnum:]_-]+:[ \t]*" line))

(defun e-agents-std-context--line-indent (line)
  "Return leading whitespace width for LINE."
  (if (string-match "\\`[ \t]*" line)
      (length (match-string 0 line))
    0))

(defun e-agents-std-context--dedent-block-lines (lines)
  "Return block scalar LINES with common indentation removed."
  (let (indent)
    (dolist (line lines)
      (unless (string-empty-p (string-trim line))
        (let ((line-indent (e-agents-std-context--line-indent line)))
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

(defun e-agents-std-context--fold-frontmatter-lines (lines)
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

(defun e-agents-std-context--frontmatter-block-value (marker lines)
  "Return normalized block scalar value for MARKER and LINES."
  (let ((dedented (e-agents-std-context--dedent-block-lines lines)))
    (if (string-prefix-p ">" (string-trim marker))
        (e-agents-std-context--fold-frontmatter-lines dedented)
      (string-trim (string-join dedented "\n")))))

(defun e-agents-std-context--frontmatter-values (content)
  "Return YAML-like scalar frontmatter values from CONTENT as an alist."
  (let ((lines (e-agents-std-context--frontmatter-lines content))
        values)
    (while lines
      (let ((line (car lines)))
        (if (string-match "\\`[ \t]*\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)\\'"
                          line)
            (let ((key (match-string 1 line))
                  (value (match-string 2 line)))
              (setq lines (cdr lines))
              (if (e-agents-std-context--frontmatter-block-marker-p value)
                  (let (block-lines)
                    (while (and lines
                                (not
                                 (e-agents-std-context--frontmatter-top-level-key-p
                                  (car lines))))
                      (push (car lines) block-lines)
                      (setq lines (cdr lines)))
                    (push
                     (cons key
                           (e-agents-std-context--frontmatter-block-value
                            value
                            (nreverse block-lines)))
                     values))
                (push (cons key
                            (e-agents-std-context--strip-quotes value))
                      values)))
          (setq lines (cdr lines)))))
    (nreverse values)))

(defun e-agents-std-context--frontmatter-value (content key)
  "Return frontmatter KEY value from CONTENT."
  (cdr (assoc key (e-agents-std-context--frontmatter-values content))))

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
  "Return SKILLS with later resource paths taking precedence.
Project skill discovery appends ancestor directories from outer root to inner
root.  Reversing during de-duplication lets the nearest project directory win
for duplicate project-scope paths while preserving the final stable order."
  (let (paths deduped)
    (dolist (skill (reverse skills))
      (let ((path (e-skill-spec-path skill)))
        (unless (member path paths)
          (push path paths)
          (push skill deduped))))
    deduped))

(defun e-agents-std-context--project-skills-directories (directory)
  "Return project .agents/skills directories from outer roots to DIRECTORY."
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

(defun e-agents-std-context--global-skill-specs ()
  "Return discovered global skill specs."
  (e-agents-std-context--skill-specs-from-directory
   "global"
   e-agents-std-context-global-skills-directory))

(defun e-agents-std-context--project-skill-specs (&optional directory)
  "Return discovered project skill specs for DIRECTORY."
  (let ((root (e-agents-std-context--directory directory))
        (skills nil))
    (dolist (skills-directory
             (e-agents-std-context--project-skills-directories root))
      (setq skills
            (append skills
                    (e-agents-std-context--skill-specs-from-directory
                     "project"
                     skills-directory))))
    (e-agents-std-context--dedupe-skill-specs skills)))

(defun e-agents-std-context-skill-specs (&optional directory)
  "Return discovered global and project skill specs for DIRECTORY.
Global and project skills use separate resource-path scopes, so duplicate
slugs across those scopes intentionally remain distinct resources."
  (e-agents-std-context--dedupe-skill-specs
   (append (e-agents-std-context--global-skill-specs)
           (e-agents-std-context--project-skill-specs directory))))

(cl-defun e-agents-std-context--config
    (&optional directory overrides &key harness session-id turn-id)
  "Return effective agents-std-context config for DIRECTORY."
  (let ((root (e-agents-std-context--directory directory)))
    (if harness
        (e-harness-effective-capability-config
         harness
         'agents-std-context
         e-agents-std-context-config-options
         :session-id session-id
         :directory root
         :overrides overrides)
      (e-capability-config-resolve
       'agents-std-context
       e-agents-std-context-config-options
       :directory root
       :overrides overrides))))

(defun e-agents-std-context--skill-match-p (selector skill)
  "Return non-nil when SELECTOR exactly matches SKILL name or path."
  (or (string= selector (e-skill-spec-name skill))
      (string= selector (e-skill-spec-path skill))))

(defun e-agents-std-context--skill-selected-p (skill config)
  "Return non-nil when SKILL is selected by CONFIG."
  (let ((include (plist-get config :include))
        (exclude (plist-get config :exclude)))
    (and (or (null include)
             (cl-some
              (lambda (selector)
                (e-agents-std-context--skill-match-p selector skill))
              include))
         (not
          (cl-some
           (lambda (selector)
             (e-agents-std-context--skill-match-p selector skill))
           exclude)))))

(defun e-agents-std-context--filter-skill-specs (skills config)
  "Return SKILLS selected by CONFIG."
  (cl-remove-if-not
   (lambda (skill)
     (e-agents-std-context--skill-selected-p skill config))
   skills))

(defun e-agents-std-context-configured-skill-specs (&optional directory)
  "Return discovered skill specs for DIRECTORY after config filtering."
  (let ((root (e-agents-std-context--directory directory)))
    (e-agents-std-context--filter-skill-specs
     (e-agents-std-context-skill-specs root)
     (e-agents-std-context--config root))))

(defun e-agents-std-context--skill-uri (skill)
  "Return the e:// URI for SKILL."
  (format "e://agents-std-context/%s" (e-skill-spec-path skill)))

(defun e-agents-std-context--skills-catalog-message (skills)
  "Return a compact catalog context message for SKILLS."
  (when skills
    (list
     (list
      :role 'system
      :content
      (string-join
       (cons
        e-skills-default-heading
        (mapcar
         (lambda (skill)
           (format "- %s: %s Read %s"
                   (e-skill-spec-name skill)
                   (e-skill-spec-description skill)
                   (e-agents-std-context--skill-uri skill)))
         skills))
       "\n")))))

(defun e-agents-std-context-skills-provider (&optional directory)
  "Return a context provider for configured skills under DIRECTORY."
  (let ((root (e-agents-std-context--directory directory)))
    (e-context-provider-create
     :name 'agents-std-context-skills
     :priority 220
     :build (cl-function
             (lambda (&key harness session-id turn-id)
               (e-agents-std-context--skills-catalog-message
                (let ((context-root
                       (e-agents-std-context--context-root
                        root
                        :harness harness
                        :session-id session-id
                        :turn-id turn-id)))
                  (e-agents-std-context--filter-skill-specs
                   (e-agents-std-context-skill-specs context-root)
                   (e-agents-std-context--config
                    context-root
                    nil
                    :harness harness
                    :session-id session-id
                    :turn-id turn-id)))))))))

(defun e-agents-std-context-project-skills-provider (&optional directory)
  "Return the configured skills context provider rooted at DIRECTORY."
  (e-agents-std-context-skills-provider directory))

(defun e-agents-std-context--register-skill-resource (store capability skill)
  "Register SKILL as an e:// resource for CAPABILITY in STORE."
  (e-store-register
   store
   (e-capability-id capability)
   (e-skill-spec-path skill)
   :description (e-skill-spec-description skill)
   :reader (lambda (_entry range)
             (funcall (e-skill-spec-reader skill) skill range))
   :metadata (e-skill-spec-metadata skill)))

(defun e-agents-std-context-skills-resource-provider (&optional directory)
  "Return a resource provider for configured skills under DIRECTORY."
  (let ((root (e-agents-std-context--directory directory)))
    (cl-function
     (lambda (store capability &key harness session-id turn-id)
       (dolist (skill
                (let ((context-root
                       (e-agents-std-context--context-root
                        root
                        :harness harness
                        :session-id session-id
                        :turn-id turn-id)))
                  (e-agents-std-context--filter-skill-specs
                   (e-agents-std-context-skill-specs context-root)
                   (e-agents-std-context--config
                    context-root
                    nil
                    :harness harness
                    :session-id session-id
                    :turn-id turn-id))))
         (e-agents-std-context--register-skill-resource
          store capability skill))))))

(defun e-agents-std-context-project-skills-resource-provider (&optional directory)
  "Return the configured skills resource provider rooted at DIRECTORY."
  (e-agents-std-context-skills-resource-provider directory))

(defun e-agents-std-context-capability-create (&optional directory)
  "Create the standard agent context capability rooted at DIRECTORY."
  (let* ((root (e-agents-std-context--directory directory))
         (config (e-agents-std-context--config root)))
    (e-capability-create
     :id 'agents-std-context
     :name "Agents Std Context"
     :instruction-priority 220
     :instructions e-agents-std-context-instructions
     :resources (list
                 (e-agents-std-context-skills-resource-provider root))
     :context-providers
     (list (e-agents-std-context-agents-provider root)
           (e-agents-std-context-skills-provider root))
     :config-options e-agents-std-context-config-options
     :config config)))

(defun e-agents-std-context-layer-create (&optional directory)
  "Create the agents-std-context layer rooted at DIRECTORY."
  (e-layer-create
   :id 'agents-std-context
   :name "Agents Std Context"
   :capabilities
   (list (e-agents-std-context-capability-create directory))))

(provide 'e-agents-std-context)

;;; e-agents-std-context.el ends here

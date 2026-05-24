;;; e-agents-std-context-test.el --- Tests for standard agent context -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for AGENTS.md and filesystem skill discovery.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'e)
(require 'e-agents-std-context)
(require 'e-backend)
(require 'e-harness)
(require 'e-store)

(defun e-agents-std-context-test--write-file (path content)
  "Write CONTENT to PATH, creating parent directories."
  (make-directory (file-name-directory path) t)
  (write-region content nil path nil 'silent))

(ert-deftest e-agents-std-context-test-adds-agents-files-to-turn-context ()
  "The layer adds global and project AGENTS.md content to every turn context."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (nested (expand-file-name "src/lisp" project))
         (global-agents (expand-file-name ".codex/AGENTS.md" home))
         (project-agents (expand-file-name "AGENTS.md" project)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (e-agents-std-context-test--write-file
           global-agents
           "# Global Agents\n\nGlobal instruction.")
          (e-agents-std-context-test--write-file
           project-agents
           "# Project Agents\n\nProject instruction.")
          (let* ((e-agents-std-context-global-agents-files
                  (list global-agents))
                 (e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-agents-std-context-layer-create nested)))))
            (e-harness-create-session harness :id "session-1")
            (let* ((messages (plist-get
                              (e-harness-context harness "session-1")
                              :messages))
                   (content (mapconcat
                             (lambda (message)
                               (plist-get message :content))
                             messages
                             "\n\n")))
              (should (string-match-p "AGENTS.md context" content))
              (should (string-match-p "Global instruction" content))
              (should (string-match-p "Project instruction" content))
              (should (string-match-p
                       (regexp-quote (abbreviate-file-name global-agents))
                       content))
              (should (string-match-p
                       (regexp-quote (abbreviate-file-name project-agents))
                       content)))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-discovers-global-and-project-skills ()
  "Global and project skills are advertised compactly and readable on demand."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (global-skill
          (expand-file-name ".agents/skills/research/SKILL.md" home))
         (project-skill
          (expand-file-name ".agents/skills/continue/SKILL.md" project)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           global-skill
           "---\nname: research\ndescription: Use current web and docs research.\n---\n\nGlobal research body.")
          (e-agents-std-context-test--write-file
           project-skill
           "---\nname: Continue\ndescription: Resume project progress.\n---\n\nProject continue body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-agents-std-context-layer-create project))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (context (e-harness-context harness "session-1"))
                 (content (mapconcat
                           (lambda (message)
                             (plist-get message :content))
                           (plist-get context :messages)
                           "\n\n"))
                 (store (e-harness-store harness)))
            (should (string-match-p
                     "research: Use current web and docs research. Read e://agents-std-context/skills/global/research"
                     content))
            (should (string-match-p
                     "Continue: Resume project progress. Read e://agents-std-context/skills/project/continue"
                     content))
            (should-not (string-match-p "Global research body" content))
            (should-not (string-match-p "Project continue body" content))
            (should (equal
                     (e-store-read
                      store
                      "e://agents-std-context/skills/global/research"
                      nil)
                     (with-temp-buffer
                       (insert-file-contents global-skill)
                       (buffer-string))))
            (should (equal
                     (e-store-read
                      store
                      "e://agents-std-context/skills/project/continue"
                      nil)
                     (with-temp-buffer
                       (insert-file-contents project-skill)
                       (buffer-string))))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-global-skills-are-not-project-ancestors ()
  "The configured global skills directory is not also advertised as project."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (expand-file-name "projects/app" home))
         (global-skill
          (expand-file-name ".agents/skills/research/SKILL.md" home)))
    (unwind-protect
        (progn
          (make-directory project t)
          (e-agents-std-context-test--write-file
           global-skill
           "---\nname: research\ndescription: Use current web and docs research.\n---\n\nGlobal research body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (paths (mapcar #'e-skill-spec-path
                                (e-agents-std-context-skill-specs project))))
            (should (member "skills/global/research" paths))
            (should-not (member "skills/project/research" paths))))
      (delete-directory home t))))

(ert-deftest e-agents-std-context-test-nearest-project-skill-wins-same-slug ()
  "Nested project skill directories keep one stable project resource per slug."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (nested (expand-file-name "packages/app" project))
         (parent-skill
          (expand-file-name ".agents/skills/review/SKILL.md" project))
         (nested-skill
          (expand-file-name ".agents/skills/review/SKILL.md" nested)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           parent-skill
           "---\nname: review\ndescription: Parent review.\n---\n\nParent body.")
          (e-agents-std-context-test--write-file
           nested-skill
           "---\nname: review\ndescription: Nested review.\n---\n\nNested body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :active-layers
                   (list (e-agents-std-context-layer-create nested))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (store (e-harness-store harness)))
            (should (equal
                     (e-store-read
                      store
                      "e://agents-std-context/skills/project/review"
                      nil)
                     (with-temp-buffer
                       (insert-file-contents nested-skill)
                       (buffer-string))))
            (should (= 1
                       (cl-count
                        "e://agents-std-context/skills/project/review"
                        (mapcar #'e-store-entry-uri (e-store-list store))
                        :test #'string=)))))
      (delete-directory home t)
      (delete-directory project t))))

(provide 'e-agents-std-context-test)

;;; e-agents-std-context-test.el ends here

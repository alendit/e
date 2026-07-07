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
(require 'e-resources)
(require 'e-store)

(defun e-agents-std-context-test--write-file (path content)
  "Write CONTENT to PATH, creating parent directories."
  (make-directory (file-name-directory path) t)
  (write-region content nil path nil 'silent))

(defun e-agents-std-context-test--context-content (harness session-id)
  "Return combined context message content for HARNESS SESSION-ID."
  (mapconcat
   (lambda (message)
     (plist-get message :content))
   (plist-get (e-harness-context harness session-id) :messages)
   "\n\n"))

(ert-deftest e-agents-std-context-test-built-in-priorities ()
  "Standard agent context uses explicit context fragment priorities."
  (let* ((capability (e-agents-std-context-capability-create default-directory))
         (providers (e-capability-context-providers capability)))
    (should (= (e-capability-instruction-priority capability) 220))
    (should (= (e-context-provider-priority (car providers)) 100))
    (should (= (e-context-provider-priority (cadr providers)) 220))))

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
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create nested)))))
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

(ert-deftest e-agents-std-context-test-includes-nested-agents-root-to-leaf ()
  "Nested AGENTS.md files enter context from project root to leaf."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (app (expand-file-name "packages/app" project))
         (src (expand-file-name "src" app)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           (expand-file-name "AGENTS.md" project)
           "# Project\n\nRoot instruction.")
          (e-agents-std-context-test--write-file
           (expand-file-name "AGENTS.md" app)
           "# App\n\nApp instruction.")
          (e-agents-std-context-test--write-file
           (expand-file-name "AGENTS.md" src)
           "# Src\n\nSrc instruction.")
          (let* ((e-agents-std-context-global-agents-files nil)
                 (e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create src))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (content
                  (e-agents-std-context-test--context-content
                   harness
                   "session-1"))
                 (root-pos (string-match-p "Root instruction" content))
                 (app-pos (string-match-p "App instruction" content))
                 (src-pos (string-match-p "Src instruction" content)))
            (should root-pos)
            (should app-pos)
            (should src-pos)
            (should (< root-pos app-pos))
            (should (< app-pos src-pos))))
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
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project))))
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

(ert-deftest e-agents-std-context-test-folded-description-enters-skill-catalog ()
  "Folded YAML frontmatter descriptions advertise useful text."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (skill (expand-file-name ".agents/skills/find-docs/SKILL.md" project)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           skill
           "---\nname: find-docs\ndescription: >-\n  Locate current project docs\n  before answering.\n---\n\nFull folded skill body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (content
                  (e-agents-std-context-test--context-content
                   harness
                   "session-1")))
            (should (string-match-p
                     "find-docs: Locate current project docs before answering. Read e://agents-std-context/skills/project/find-docs"
                     content))
            (should-not (string-match-p "find-docs: >-" content))
            (should-not (string-match-p "Full folded skill body" content))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-literal-description-enters-skill-catalog ()
  "Literal YAML frontmatter descriptions advertise useful text."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (skill (expand-file-name ".agents/skills/read-guides/SKILL.md" project)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           skill
           "---\nname: read-guides\ndescription: |\n  Read repo guidance first.\n  Keep scope tight.\n---\n\nFull literal skill body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (content
                  (e-agents-std-context-test--context-content
                   harness
                   "session-1")))
            (should (string-match-p "read-guides: Read repo guidance first."
                                    content))
            (should (string-match-p "Keep scope tight. Read e://agents-std-context/skills/project/read-guides"
                                    content))
            (should-not (string-match-p "read-guides: |" content))
            (should-not (string-match-p "Full literal skill body" content))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-missing-description-uses-fallback ()
  "Skill files without descriptions keep a useful compact fallback."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (skill (expand-file-name ".agents/skills/refocus/SKILL.md" project)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           skill
           "---\nname: refocus\n---\n\nRefocus body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (content
                  (e-agents-std-context-test--context-content
                   harness
                   "session-1")))
            (should (string-match-p
                     "refocus: Read refocus guidance. Read e://agents-std-context/skills/project/refocus"
                     content))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-empty-description-uses-fallback ()
  "Skill files with empty descriptions keep a useful compact fallback."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (skill (expand-file-name ".agents/skills/refocus/SKILL.md" project)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           skill
           "---\nname: refocus\ndescription:\n---\n\nRefocus body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (content
                  (e-agents-std-context-test--context-content
                   harness
                   "session-1")))
            (should (string-match-p
                     "refocus: Read refocus guidance. Read e://agents-std-context/skills/project/refocus"
                     content))))
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

(ert-deftest e-agents-std-context-test-global-and-project-same-slug-are-distinct ()
  "Global and project skill scopes keep same-slug resources distinct."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (global-skill
          (expand-file-name ".agents/skills/review/SKILL.md" home))
         (project-skill
          (expand-file-name ".agents/skills/review/SKILL.md" project)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           global-skill
           "---\nname: review\ndescription: Global review.\n---\n\nGlobal body.")
          (e-agents-std-context-test--write-file
           project-skill
           "---\nname: review\ndescription: Project review.\n---\n\nProject body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (resources (e-harness-resources harness "session-1")))
            (should (equal
                     (e-resources-read
                      resources
                      "e://agents-std-context/skills/global/review")
                     (with-temp-buffer
                       (insert-file-contents global-skill)
                       (buffer-string))))
            (should (equal
                     (e-resources-read
                      resources
                      "e://agents-std-context/skills/project/review")
                     (with-temp-buffer
                       (insert-file-contents project-skill)
                       (buffer-string))))))
      (delete-directory home t)
      (delete-directory project t))))

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
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create nested))))
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

(ert-deftest e-agents-std-context-test-parent-and-nested-project-skills-remain-available ()
  "Different project skill slugs from parent and nested roots are both exposed."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (nested (expand-file-name "packages/app" project))
         (parent-skill
          (expand-file-name ".agents/skills/review/SKILL.md" project))
         (nested-skill
          (expand-file-name ".agents/skills/test/SKILL.md" nested)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           parent-skill
           "---\nname: review\ndescription: Review parent changes.\n---\n\nParent body.")
          (e-agents-std-context-test--write-file
           nested-skill
           "---\nname: test\ndescription: Run nested tests.\n---\n\nNested body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create nested))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (content
                  (e-agents-std-context-test--context-content
                   harness
                   "session-1"))
                 (resources (e-harness-resources harness "session-1")))
            (should (string-match-p
                     "review: Review parent changes. Read e://agents-std-context/skills/project/review"
                     content))
            (should (string-match-p
                     "test: Run nested tests. Read e://agents-std-context/skills/project/test"
                     content))
            (should (equal
                     (e-resources-read
                      resources
                      "e://agents-std-context/skills/project/review")
                     (with-temp-buffer
                       (insert-file-contents parent-skill)
                       (buffer-string))))
            (should (equal
                     (e-resources-read
                      resources
                      "e://agents-std-context/skills/project/test")
                     (with-temp-buffer
                       (insert-file-contents nested-skill)
                       (buffer-string))))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-skill-resource-is-read-only-through-resources ()
  "agents-std-context skill resources are read-only through resource dispatch."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (skill (expand-file-name ".agents/skills/read-only/SKILL.md" project)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           skill
           "---\nname: read-only\ndescription: Read-only skill.\n---\n\nRead-only body.")
          (let* ((e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (resources (e-harness-resources harness "session-1"))
                 (uri "e://agents-std-context/skills/project/read-only"))
            (should (string-match-p "Read-only body."
                                    (e-resources-read resources uri)))
            (should-error (e-resources-write resources uri "replacement")
                          :type 'e-resources-unsupported-operation)
            (should-error (e-resources-edit resources uri nil)
                          :type 'e-resources-unsupported-operation)))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-session-root-controls-project-context ()
  "Session project roots, not layer construction roots, select project context."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project-a (make-temp-file "e-agents-project-a-" t))
         (project-b (make-temp-file "e-agents-project-b-" t))
         (agents-a (expand-file-name "AGENTS.md" project-a))
         (agents-b (expand-file-name "AGENTS.md" project-b))
         (skill-a (expand-file-name ".agents/skills/context/SKILL.md" project-a))
         (skill-b (expand-file-name ".agents/skills/context/SKILL.md" project-b)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           agents-a
           "# Project A\n\nProject A instruction.")
          (e-agents-std-context-test--write-file
           agents-b
           "# Project B\n\nProject B instruction.")
          (e-agents-std-context-test--write-file
           skill-a
           "---\nname: context\ndescription: Project A context.\n---\n\nProject A skill body.")
          (e-agents-std-context-test--write-file
           skill-b
           "---\nname: context\ndescription: Project B context.\n---\n\nProject B skill body.")
          (let* ((e-agents-std-context-global-agents-files nil)
                 (e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project-a)))))
            (e-harness-create-session
             harness
             :id "session-a"
             :metadata (list :project-root project-a))
            (e-harness-create-session
             harness
             :id "session-b"
             :metadata (list :project-root project-b))
            (let* ((messages-a (plist-get
                                (e-harness-context harness "session-a")
                                :messages))
                   (messages-b (plist-get
                                (e-harness-context harness "session-b")
                                :messages))
                   (content-a (mapconcat
                               (lambda (message)
                                 (plist-get message :content))
                               messages-a
                               "\n\n"))
                   (content-b (mapconcat
                               (lambda (message)
                                 (plist-get message :content))
                               messages-b
                               "\n\n"))
                   (resources-b (e-harness-resources harness "session-b")))
              (should (string-match-p "Project A instruction" content-a))
              (should-not (string-match-p "Project B instruction" content-a))
              (should (string-match-p "Project B instruction" content-b))
              (should-not (string-match-p "Project A instruction" content-b))
              (should (string-match-p
                       "context: Project B context. Read e://agents-std-context/skills/project/context"
                       content-b))
              (should (equal
                       (e-resources-read
                        resources-b
                        "e://agents-std-context/skills/project/context")
                       (with-temp-buffer
                         (insert-file-contents skill-b)
                         (buffer-string)))))))
      (delete-directory home t)
      (delete-directory project-a t)
      (delete-directory project-b t))))

(ert-deftest e-agents-std-context-test-config-filters-advertised-and-readable-skills ()
  "Include and exclude config filters skill catalog and resources."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (global-skill
          (expand-file-name ".agents/skills/research/SKILL.md" home))
         (keep-skill
          (expand-file-name ".agents/skills/keep/SKILL.md" project))
         (drop-skill
          (expand-file-name ".agents/skills/drop/SKILL.md" project))
         (agents-file (expand-file-name "AGENTS.md" project)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           agents-file
           "# Project\n\nProject instruction remains visible.")
          (e-agents-std-context-test--write-file
           global-skill
           "---\nname: research\ndescription: Global research.\n---\n\nGlobal body.")
          (e-agents-std-context-test--write-file
           keep-skill
           "---\nname: Keep\ndescription: Keep this skill.\n---\n\nKeep body.")
          (e-agents-std-context-test--write-file
           drop-skill
           "---\nname: Drop\ndescription: Drop this skill.\n---\n\nDrop body.")
          (let* ((e-agents-std-context-global-agents-files nil)
                 (e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (e-capability-config
                  '((agents-std-context
                     :skills-exclude ("Drop"))))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project))))
                 (_session (e-harness-create-session harness :id "session-1"))
                 (content
                  (e-agents-std-context-test--context-content
                   harness
                   "session-1"))
                 (store (e-harness-store harness)))
            (should (string-match-p "Project instruction remains visible"
                                    content))
            (should (string-match-p
                     "research: Global research. Read e://agents-std-context/skills/global/research"
                     content))
            (should (string-match-p
                     "Keep: Keep this skill. Read e://agents-std-context/skills/project/keep"
                     content))
            (should-not (string-match-p "Drop this skill" content))
            (should (string-match-p
                     "Global body"
                     (e-store-read
                      store
                      "e://agents-std-context/skills/global/research"
                      nil)))
            (should (string-match-p
                     "Keep body"
                     (e-store-read
                      store
                      "e://agents-std-context/skills/project/keep"
                      nil)))
            (should-error
             (e-store-read
              store
              "e://agents-std-context/skills/project/drop"
              nil))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-global-skill-config-refreshes-for-harness ()
  "Changing global config changes the next context build for the same harness."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (alpha-skill
          (expand-file-name ".agents/skills/alpha/SKILL.md" home))
         (beta-skill
          (expand-file-name ".agents/skills/beta/SKILL.md" home)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           alpha-skill
           "---\nname: Alpha\ndescription: Alpha skill.\n---\n\nAlpha body.")
          (e-agents-std-context-test--write-file
           beta-skill
           "---\nname: Beta\ndescription: Beta skill.\n---\n\nBeta body.")
          (let* ((e-agents-std-context-global-agents-files nil)
                 (e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (e-capability-config
                  '((agents-std-context :skills-include ("Alpha"))))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project)))))
            (e-harness-create-session harness :id "session-1")
            (let ((content
                   (e-agents-std-context-test--context-content
                    harness
                    "session-1")))
              (should (string-match-p "Alpha: Alpha skill" content))
              (should-not (string-match-p "Beta: Beta skill" content)))
            (setq e-capability-config
                  '((agents-std-context :skills-include ("Beta"))))
            (let ((content
                   (e-agents-std-context-test--context-content
                    harness
                    "session-1")))
              (should (string-match-p "Beta: Beta skill" content))
              (should-not (string-match-p "Alpha: Alpha skill" content)))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-global-skill-resources-refresh-for-harness ()
  "Changing global config changes skill resources for the same harness."
  (let* ((home (make-temp-file "e-agents-home-" t))
         (project (make-temp-file "e-agents-project-" t))
         (alpha-skill
          (expand-file-name ".agents/skills/alpha/SKILL.md" home))
         (beta-skill
          (expand-file-name ".agents/skills/beta/SKILL.md" home)))
    (unwind-protect
        (progn
          (e-agents-std-context-test--write-file
           alpha-skill
           "---\nname: Alpha\ndescription: Alpha skill.\n---\n\nAlpha body.")
          (e-agents-std-context-test--write-file
           beta-skill
           "---\nname: Beta\ndescription: Beta skill.\n---\n\nBeta body.")
          (let* ((e-agents-std-context-global-agents-files nil)
                 (e-agents-std-context-global-skills-directory
                  (expand-file-name ".agents/skills" home))
                 (e-capability-config
                  '((agents-std-context :skills-include ("Alpha"))))
                 (harness
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-agents-std-context-layer-create project)))))
            (e-harness-create-session harness :id "session-1")
            (let ((store (e-harness-store harness "session-1")))
              (should (string-match-p
                       "Alpha body"
                       (e-store-read
                        store
                        "e://agents-std-context/skills/global/alpha"
                        nil)))
              (should-error
               (e-store-read
                store
                "e://agents-std-context/skills/global/beta"
                nil)))
            (setq e-capability-config
                  '((agents-std-context :skills-include ("Beta"))))
            (let ((store (e-harness-store harness "session-1")))
              (should (string-match-p
                       "Beta body"
                       (e-store-read
                        store
                        "e://agents-std-context/skills/global/beta"
                        nil)))
              (should-error
               (e-store-read
                store
                "e://agents-std-context/skills/global/alpha"
                nil)))))
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest e-agents-std-context-test-capability-exposes-effective-config ()
  "The capability carries option metadata and resolved config."
  (let ((e-capability-config
         '((agents-std-context :skills-include ("skills/global/research")))))
    (let ((capability
           (e-agents-std-context-capability-create default-directory)))
      (should
       (eq (e-capability-config-options capability)
           e-agents-std-context-config-options))
      (should
       (equal (e-capability-config capability)
              '(:skills-include ("skills/global/research") :skills-exclude nil))))))

(provide 'e-agents-std-context-test)

;;; e-agents-std-context-test.el ends here

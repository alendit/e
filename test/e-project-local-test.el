;;; e-project-local-test.el --- Tests for project-local capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for discovery, allowlist gating, and contribution of project-local
;; layers shipped under `.e/layers/' plus compatibility capabilities under
;; `.e/capabilities/'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'e)
(require 'e-actions)
(require 'e-action-resources)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-layer-selection)
(require 'e-project-local)
(require 'e-shells)
(require 'e-store)
(load (expand-file-name "e-tools-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-work)
(require 'e-tools)

(defvar projectile-after-switch-project-hook)
(defvar projectile-find-dir-hook)
(defvar projectile-find-file-hook)
(defvar e-project-local-test--unexpected-inspect-load)
(defvar e-project-local-test--prime-action-loaded)
(defvar e-project-local-test--factory-helper-loaded)
(defvar e-project-local-test--layer-load-count)

(defun e-project-local-test--write-file (path content)
  "Write CONTENT to PATH, creating parent directories."
  (make-directory (file-name-directory path) t)
  (write-region content nil path nil 'silent))

(defun e-project-local-test--capability-source (id)
  "Return capability.el source contributing a tool and resource for ID."
  (format
   ";;; capability.el -*- lexical-binding: t; -*-
(e-project-capability-register
    :id '%s
    :factory
    (lambda (directory)
      (e-capability-create
       :id '%s
       :name \"Topic\"
       :instructions (format \"Topic rooted at %%s\" directory)
       :tools
       (list (lambda (registry)
               (e-tools-test-register
                registry
                :name \"topic_new\"
                :description \"Create a topic.\"
                :parameters nil
                :handler (lambda (_args) \"ok\"))))
       :resources
       (list (lambda (store capability)
               (e-store-register
                store (e-capability-id capability) \"state/topics.org\"
                :description \"Topic index.\"
                :content (format \"rooted at %%s\" directory)))))))"
   id id))

(defun e-project-local-test--make-capability (root id &optional source)
  "Create a `.e/capabilities/ID/capability.el' fixture under ROOT.
SOURCE overrides the default capability source."
  (e-project-local-test--write-file
   (expand-file-name (format ".e/capabilities/%s/capability.el" id) root)
   (or source (e-project-local-test--capability-source id))))

(defun e-project-local-test--layer-source (id)
  "Return layer.el source contributing a capability and shell for ID."
  (format
   ";;; layer.el -*- lexical-binding: t; -*-
(e-project-layer-register
 :id '%s
 :factory
 (lambda (directory)
   (e-layer-create
    :id '%s
    :name \"Topic Layer\"
    :capabilities
    (list (e-capability-create
           :id '%s
           :name \"Topic\"
           :instructions (format \"Topic rooted at %%s\" directory)
           :tools
           (list (lambda (registry)
                   (e-tools-test-register
                    registry
                    :name \"topic_layer_tool\"
                    :description \"Layer tool.\"
                    :handler (lambda (_args) \"ok\"))))))
    :shells
    (list (e-shell-create
           :id '%s
           :name \"Topic Shell\"
           :commands (list (e-shell-command-create :id 'open)))))))"
   id id id id))

(defun e-project-local-test--make-layer (root id &optional source)
  "Create a `.e/layers/ID/layer.el' fixture under ROOT.
SOURCE overrides the default layer source."
  (e-project-local-test--write-file
   (expand-file-name (format ".e/layers/%s/layer.el" id) root)
   (or source (e-project-local-test--layer-source id))))

(defun e-project-local-test--action-capability-source
    (id description result &optional load-form)
  "Return project-local action source for ID DESCRIPTION and RESULT.
LOAD-FORM, when non-nil, runs when the repository file is loaded."
  (format
   ";;; capability.el -*- lexical-binding: t; -*-
%s
(e-project-capability-register
 :id '%s
 :factory
 (lambda (_directory)
   (e-capability-create
    :id '%s
    :name \"Project action\"
    :actions
    (list :run
          (e-action-cheap-create
           :description %S
           :runner (lambda (_arguments _context) %S))))))"
   (or load-form "") id id description result))

(defun e-project-local-test--action-harness (project &optional resources)
  "Return a harness rooted at PROJECT with dynamic project actions.
When RESOURCES is non-nil, include action-description resources."
  (let ((harness (e-harness-create :enabled-layer-ids nil)))
    (e-harness-set-intrinsic-capabilities
     harness
     (append
      (list (e-project-local--dynamic-capability project))
      (when resources (list (e-action-resources-capability-create)))))
    (e-harness-create-session
     harness :id "session-1" :metadata (list :project-root project))
    harness))

(ert-deftest e-project-local-test-empty-repo-is-no-op ()
  "A repo with no .e/capabilities yields only the guidance capability."
  (let* ((project (make-temp-file "e-project-local-empty-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (let ((layer (e-project-local-layer-create project)))
          (should (eq (e-layer-id layer) 'project-local))
          (should (= (length (e-layer-capabilities layer)) 1))
          (should (null (e-project-local-capabilities project))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-guidance-discourages-raw-elisp-loading ()
  "Project-local guidance points agents away from raw run_elisp loads."
  (let* ((project (make-temp-file "e-project-local-guidance-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (let* ((layer (e-project-local-layer-create project))
               (capability (car (e-layer-capabilities layer)))
               (instructions (e-capability-instructions capability)))
          (should (string-match-p "resource/file tools" instructions))
          (should (string-match-p "Do not raw-load project files from run_elisp"
                                  instructions))
          (should (string-match-p "project-local actions" instructions)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-discovers-ancestor-capabilities ()
  "Discovery walks ancestor .e/capabilities directories."
  (let* ((project (make-temp-file "e-project-local-anc-" t))
         (nested (expand-file-name "src/lisp" project))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (e-project-local-test--make-capability project 'topic)
          (let ((capabilities (e-project-local-capabilities nested)))
            (should (= (length capabilities) 1))
            (should (eq (e-capability-id (car capabilities)) 'topic))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-allowlist-gates-loading ()
  "Disallowed roots are reported and not loaded; allowed roots load."
  (let* ((project (make-temp-file "e-project-local-gate-" t)))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability project 'topic)
          (let ((e-project-local-allowed-roots nil))
            (should (null (e-project-local-capabilities project))))
          (let ((e-project-local-allowed-roots (list project)))
            (let ((capabilities (e-project-local-capabilities project)))
              (should (= (length capabilities) 1))
              (should (eq (e-capability-id (car capabilities)) 'topic)))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-register-requires-load-context ()
  "Calling the registration function outside a load is an error."
  (should-error (e-project-capability-register
                 :id 'topic :factory (lambda (_dir) nil))))

(ert-deftest e-project-local-test-duplicate-ids-nearest-wins ()
  "Nearer capability directories win when ids collide."
  (let* ((outer (make-temp-file "e-project-local-outer-" t))
         (inner (expand-file-name "inner" outer))
         (e-project-local-allowed-roots (list outer)))
    (unwind-protect
        (progn
          (make-directory inner t)
          (e-project-local-test--make-capability outer 'topic)
          (e-project-local-test--make-capability inner 'topic)
          (let ((capabilities (e-project-local-capabilities inner)))
            (should (= (length capabilities) 1))
            (should (eq (e-capability-id (car capabilities)) 'topic))
            (should (string-match-p
                     (regexp-quote inner)
                     (e-capability-instructions (car capabilities))))))
      (delete-directory outer t))))

(ert-deftest e-project-local-test-bad-factory-result-signals ()
  "A factory returning a mismatched id signals an error."
  (let* ((project (make-temp-file "e-project-local-bad-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability
           project 'topic
           "(e-project-capability-register
             :id 'topic
             :factory (lambda (_dir)
                        (e-capability-create :id 'other :name \"Other\")))")
          (should-error (e-project-local-capabilities project)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-contributes-tool-and-resource ()
  "Discovered capabilities contribute tools and resources to the harness."
  (let* ((project (make-temp-file "e-project-local-harness-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability project 'topic)
          (let* ((harness (e-harness-create))
                 (layer (e-project-local-layer-create project)))
            (e-harness-set-intrinsic-capabilities
             harness (e-layer-capabilities layer))
            (let ((tools (e-harness-tools harness))
                  (store (e-harness-store harness)))
              (should (member "topic_new" (e-tools-registry-order tools)))
              (should (cl-find "e://topic/state/topics.org"
                               (e-store-list store)
                               :key #'e-store-entry-uri
                               :test #'string=)))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-tools-follow-session-root-not-layer-root ()
  "Project-local tools are resolved from the session root, not shared layer state."
  (let* ((first (make-temp-file "e-project-local-first-" t))
         (second (make-temp-file "e-project-local-second-" t))
         (e-project-local-allowed-roots (list first second)))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability
           first 'first
           "(e-project-capability-register
             :id 'first
             :factory
             (lambda (_directory)
               (e-capability-create
                :id 'first
                :name \"First\"
                :tools
                (list (lambda (registry)
                        (e-tools-test-register
                         registry
                         :name \"first_tool\"
                         :description \"First project tool.\"
                         :handler (lambda (_args) \"first\")))))))")
          (e-project-local-test--make-capability
           second 'second
           "(e-project-capability-register
             :id 'second
             :factory
             (lambda (_directory)
               (e-capability-create
                :id 'second
                :name \"Second\"
                :tools
                (list (lambda (registry)
                        (e-tools-test-register
                         registry
                         :name \"second_tool\"
                         :description \"Second project tool.\"
                         :handler (lambda (_args) \"second\")))))))")
          (let* ((harness (e-harness-create))
                 (layer (e-project-local-layer-create first))
                 (session (e-harness-create-session
                           harness
                           :metadata (list :project-root second)))
                 (session-id (plist-get session :id)))
            (e-harness-set-intrinsic-capabilities
             harness (e-layer-capabilities layer))
            (should (member "first_tool"
                            (e-tools-registry-order
                             (e-harness-tools harness))))
            (let ((session-tools
                   (e-tools-registry-order
                    (e-harness-tools harness session-id))))
              (should (member "second_tool" session-tools))
              (should-not (member "first_tool" session-tools)))))
      (delete-directory first t)
      (delete-directory second t))))

(ert-deftest e-project-local-test-folds-in-capability-skills ()
  "Capability-scoped skills register as read-only e:// resources."
  (let* ((project (make-temp-file "e-project-local-skills-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability project 'topic)
          (e-project-local-test--write-file
           (expand-file-name ".e/capabilities/topic/skills/daily/SKILL.md"
                             project)
           "---\nname: daily\ndescription: Run the daily ritual.\n---\nDaily steps.")
          (let* ((harness (e-harness-create))
                 (layer (e-project-local-layer-create project)))
            (e-harness-set-intrinsic-capabilities
             harness (e-layer-capabilities layer))
            (let* ((store (e-harness-store harness))
                   (uri "e://topic/skills/project/daily")
                   (entry (cl-find uri (e-store-list store)
                                   :key #'e-store-entry-uri
                                   :test #'string=)))
              (should entry)
              (should (string-match-p "Daily steps."
                                      (e-store-read-entry entry))))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-capability-skills-preserve-action-providers ()
  "Adding skill resources keeps dynamic action-capability providers."
  (let* ((project (make-temp-file "e-project-local-action-skills-" t))
         (directory (expand-file-name ".e/capabilities/topic/" project))
         (child
          (e-capability-create
           :id 'child-action
           :actions
           (list :run
                 (e-action-cheap-create
                  :runner (lambda (_arguments _context) "ok")))))
         (provider (lambda (&rest _context) (list child)))
         (capability
          (e-capability-create
           :id 'topic
           :actions
           (list :run
                 (e-action-cheap-create
                  :runner (lambda (_arguments _context) "topic")))
           :action-capability-providers (list provider))))
    (unwind-protect
        (progn
          (e-project-local-test--write-file
           (expand-file-name "skills/daily/SKILL.md" directory)
           "---
name: daily
description: Daily action skill.
---
Steps.")
          (let ((decorated
                 (e-project-local--with-capability-skills
                  capability directory)))
            (should (equal
                     (e-capability-action-capability-providers decorated)
                     (list provider)))
            (should (equal
                     (mapcar
                      #'e-capability-id
                      (e-capabilities-provided-action-capabilities
                       (list decorated)))
                     '(child-action)))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-folds-in-layer-skills ()
  "Layer-scoped skills register as project-local read-only e:// resources."
  (let* ((project (make-temp-file "e-project-local-layer-skills-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (e-project-local-test--write-file
           (expand-file-name ".e/layers/topic/skills/triage/SKILL.md"
                             project)
           "---\nname: triage\ndescription: Triage project topics.\n---\nTriage steps.")
          (let* ((harness (e-harness-create))
                 (layer (e-project-local-layer-create project)))
            (e-harness-set-intrinsic-capabilities
             harness (e-layer-capabilities layer))
            (let* ((store (e-harness-store harness))
                   (uri "e://project-local/layers/topic/skills/project/triage")
                   (entry (cl-find uri (e-store-list store)
                                   :key #'e-store-entry-uri
                                   :test #'string=)))
              (should entry)
              (should (string-match-p "Triage steps."
                                      (e-store-read-entry entry))))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-project-layer-contributes-capability-and-shell ()
  "Trusted `.e/layers/' packages contribute shells and dynamic model tools."
  (let* ((project (make-temp-file "e-project-local-layer-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (let* ((harness (e-harness-create))
                 (layer (e-project-local-layer-create project)))
            (e-harness-set-intrinsic-capabilities
             harness (e-layer-capabilities layer))
            (should (eq (e-layer-id layer) 'project-local))
            (should (member "topic_layer_tool"
                            (e-tools-registry-order
                             (e-harness-tools harness))))
            (should (cl-find 'topic
                             (e-layer-shells layer)
                             :key #'e-shell-id))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-local-layer-id-is-not-globally-selectable ()
  "A rejected local layer id cannot poison its aggregate harness."
  (let* ((project (make-temp-file "e-project-local-select-" t))
         (e-project-local-allowed-roots (list project))
         (e-layer--registry (make-hash-table :test 'eq))
         (e-shell--registry (make-hash-table :test 'eq))
         (e-shell--scoped-registry (make-hash-table :test 'eq)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (e-layer-register
           (e-layer-spec-create
            :id 'project-local
            :name "Project Local"
            :factory #'e-project-local-layer-create))
          (let ((harness (e-harness-create
                          :project-root project)))
            (e-harness-activate-capability
             harness (e-layer-selection-capability-create))
            (e-harness-enable-layer-id harness 'project-local project)
            (should (e-shell-get-active 'topic harness))
            (should-not (e-layer-get 'topic))
            (should-error
             (e-actions-call 'layer-selection :enable '(:layer "topic")
                             (list :harness harness))
             :type 'e-layer-registry-missing)
            (should (equal (e-harness-enabled-layer-ids harness)
                           '(project-local)))
            (should (equal (e-harness-effective-layer-ids harness)
                           '(project-local)))
            (let* ((layers
                    (e-actions-call 'layer-selection :list nil
                                    (list :harness harness)))
                   (project-local
                    (cl-find 'project-local layers
                             :key (lambda (entry)
                                    (plist-get entry :id)))))
              (should (plist-get project-local :enabled))
              (should (plist-get project-local :active)))
            (should (e-shell-get-active 'topic harness))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-project-layer-requires-are-aggregated ()
  "A project layer's `requires' surface on the aggregate project-local layer."
  (let* ((project (make-temp-file "e-project-local-layer-requires-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer
           project 'topic
           "(e-project-layer-register
             :id 'topic
             :factory (lambda (_dir)
                        (e-layer-create :id 'topic :name \"Topic\"
                                        :requires '(org-canvas))))")
          (let ((layer (e-project-local-layer-create project)))
            (should (memq 'org-canvas (e-layer-requires layer)))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-project-layer-loading-is-allowlisted ()
  "Untrusted project layer roots are skipped before loading layer.el."
  (let ((project (make-temp-file "e-project-local-layer-gate-" t)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (let ((e-project-local-allowed-roots nil))
            (let ((layer (e-project-local-layer-create project)))
              (should-not (cl-find 'topic
                                   (e-layer-shells layer)
                                   :key #'e-shell-id))))
          (let ((e-project-local-allowed-roots (list project)))
            (let ((layer (e-project-local-layer-create project)))
              (should (cl-find 'topic
                               (e-layer-shells layer)
                               :key #'e-shell-id)))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-project-loaders-use-extensionless-loads ()
  "Project-local loaders let Emacs choose `.elc' or `.el'."
  (let* ((project (make-temp-file "e-project-local-load-base-" t))
         (e-project-local-allowed-roots (list project))
         loaded-files)
    (unwind-protect
        (progn
          (e-project-local-test--make-capability project 'topic)
          (e-project-local-test--make-layer project 'topic)
          (cl-letf (((symbol-function 'load)
                     (lambda (file &rest _args)
                       (push file loaded-files)
                       t)))
            (e-project-local--load-capability-file
             (expand-file-name ".e/capabilities/topic" project))
            (e-project-local--load-layer-file
             (expand-file-name ".e/layers/topic" project)))
          (should loaded-files)
          (dolist (file loaded-files)
            (should-not (string-suffix-p ".el" file))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-project-factories-use-extensionless-loads ()
  "Project-local factory loads of sibling `.el' files can use fresh `.elc'."
  (let* ((project (make-temp-file "e-project-local-factory-load-base-" t))
         (layer-dir (expand-file-name ".e/layers/topic" project))
         (helper-file (expand-file-name "helper.el" layer-dir))
         (e-project-local-allowed-roots (list project))
         (byte-compile-warnings nil))
    (unwind-protect
        (progn
          (when (boundp 'e-project-local-test--factory-helper-loaded)
            (makunbound 'e-project-local-test--factory-helper-loaded))
          (e-project-local-test--write-file
           helper-file
           "(setq e-project-local-test--factory-helper-loaded 'compiled)")
          (should (byte-compile-file helper-file))
          (delete-file helper-file)
          (e-project-local-test--make-layer
           project 'topic
           "(e-project-layer-register
             :id 'topic
             :factory (lambda (directory)
                        (load (expand-file-name \"helper.el\" directory)
                              nil 'nomessage)
                        (e-layer-create :id 'topic :name \"Topic\")))")
          (let ((layer (e-project-local-layer-create project)))
            (should (eq (e-layer-id layer) 'project-local))
            (should (eq (symbol-value
                         'e-project-local-test--factory-helper-loaded)
                        'compiled))))
      (when (boundp 'e-project-local-test--factory-helper-loaded)
        (makunbound 'e-project-local-test--factory-helper-loaded))
      (delete-directory project t))))

(ert-deftest e-project-local-test-factory-file-loads-once-per-session ()
  "A factory helper file loads once and is skipped on later factory runs."
  (let* ((project (make-temp-file "e-project-local-load-once-" t))
         (layer-dir (expand-file-name ".e/layers/topic" project))
         (helper-file (expand-file-name "helper.el" layer-dir))
         (e-project-local-allowed-roots (list project))
         (e-project-local--loaded-files (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (setq e-project-local-test--factory-helper-loaded 0)
          (e-project-local-test--write-file
           helper-file
           "(setq e-project-local-test--factory-helper-loaded
                  (1+ e-project-local-test--factory-helper-loaded))")
          (e-project-local-test--make-layer
           project 'topic
           "(e-project-layer-register
             :id 'topic
             :factory (lambda (directory)
                        (load (expand-file-name \"helper.el\" directory)
                              nil 'nomessage)
                        (e-layer-create :id 'topic :name \"Topic\")))")
          (e-project-local-layer-create project)
          (e-project-local-layer-create project)
          (e-project-local-layer-create project)
          (should (= e-project-local-test--factory-helper-loaded 1)))
      (makunbound 'e-project-local-test--factory-helper-loaded)
      (delete-directory project t))))

(ert-deftest e-project-local-test-reset-loaded-files-reloads-factory-file ()
  "Resetting the loaded-file set makes the next factory run reload the file."
  (let* ((project (make-temp-file "e-project-local-reset-" t))
         (layer-dir (expand-file-name ".e/layers/topic" project))
         (helper-file (expand-file-name "helper.el" layer-dir))
         (e-project-local-allowed-roots (list project))
         (e-project-local--loaded-files (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (setq e-project-local-test--factory-helper-loaded 0)
          (e-project-local-test--write-file
           helper-file
           "(setq e-project-local-test--factory-helper-loaded
                  (1+ e-project-local-test--factory-helper-loaded))")
          (e-project-local-test--make-layer
           project 'topic
           "(e-project-layer-register
             :id 'topic
             :factory (lambda (directory)
                        (load (expand-file-name \"helper.el\" directory)
                              nil 'nomessage)
                        (e-layer-create :id 'topic :name \"Topic\")))")
          (e-project-local-layer-create project)
          (should (= e-project-local-test--factory-helper-loaded 1))
          (e-project-local-reset-loaded-files)
          (e-project-local-layer-create project)
          (should (= e-project-local-test--factory-helper-loaded 2)))
      (makunbound 'e-project-local-test--factory-helper-loaded)
      (delete-directory project t))))

(ert-deftest e-project-local-test-failed-factory-rolls-back-loaded-files ()
  "A factory that signals after loading one helper does not mark it loaded.
The next discovery must reload every helper so a transient failure cannot
cement a partial layer for the session."
  (let* ((project (make-temp-file "e-project-local-rollback-" t))
         (layer-dir (expand-file-name ".e/layers/topic" project))
         (helper-file (expand-file-name "helper.el" layer-dir))
         (fail-marker (expand-file-name "fail.marker" layer-dir))
         (e-project-local-allowed-roots (list project))
         (e-project-local--loaded-files (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (setq e-project-local-test--factory-helper-loaded 0)
          (e-project-local-test--write-file
           helper-file
           "(setq e-project-local-test--factory-helper-loaded
                  (1+ e-project-local-test--factory-helper-loaded))")
          (e-project-local-test--write-file fail-marker "x")
          (e-project-local-test--make-layer
           project 'topic
           "(e-project-layer-register
             :id 'topic
             :factory (lambda (directory)
                        (load (expand-file-name \"helper.el\" directory)
                              nil 'nomessage)
                        (when (file-exists-p
                               (expand-file-name \"fail.marker\" directory))
                          (error \"boom\"))
                        (e-layer-create :id 'topic :name \"Topic\")))")
          (should-error (e-project-local-layer-create project))
          (should (= e-project-local-test--factory-helper-loaded 1))
          (should (zerop (hash-table-count e-project-local--loaded-files)))
          (delete-file fail-marker)
          (let ((layer (e-project-local-layer-create project)))
            (should (eq (e-layer-id layer) 'project-local))
            (should (= e-project-local-test--factory-helper-loaded 2))))
      (makunbound 'e-project-local-test--factory-helper-loaded)
      (delete-directory project t))))

(ert-deftest e-project-local-test-projectile-prime-guards-failure ()
  "A failing prime on project entry is caught and resets the discovery cache."
  (let* ((project (make-temp-file "e-project-local-prime-guard-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local-projectile-prime-on-project-entry t)
         (e-project-local--loaded-files (make-hash-table :test 'equal))
         (reset-called nil)
         (warned nil))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer
           project 'topic
           "(e-project-layer-register
             :id 'topic
             :factory (lambda (_dir) (error \"prime boom\")))")
          (cl-letf (((symbol-function 'e-project-local--projectile-project-root)
                     (lambda () (e-skills-normalize-directory project)))
                    ((symbol-function 'e-project-local-reset-loaded-files)
                     (lambda () (setq reset-called t)))
                    ((symbol-function 'display-warning)
                     (lambda (&rest _args) (setq warned t))))
            (should-not (e-project-local-prime-projectile-project))
            (should reset-called)
            (should warned)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-registration-file-reloads-each-discovery ()
  "The `layer.el' registration file re-runs on each discovery despite dedup.
Its `e-project-layer-register' side effect must repopulate the registration
list every time, or discovery after the first would find no layers."
  (let* ((project (make-temp-file "e-project-local-reg-reload-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local--loaded-files (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (let ((first (e-project-local-layer-create project))
                (second (e-project-local-layer-create project)))
            (should (cl-find 'topic (e-layer-shells first) :key #'e-shell-id))
            (should (cl-find 'topic (e-layer-shells second)
                             :key #'e-shell-id))))
      (delete-directory project t))))

(defun e-project-local-test--counting-layer-source ()
  "Return layer.el source that increments the load counter at read time.
The top-level `setq' runs each time the registration file is loaded, so the
counter measures how many times discovery reloaded `layer.el'."
  (concat
   "(setq e-project-local-test--layer-load-count"
   "      (1+ e-project-local-test--layer-load-count))\n"
   "(e-project-layer-register\n"
   " :id 'topic\n"
   " :factory (lambda (_dir)\n"
   "            (e-layer-create :id 'topic :name \"Topic\")))"))

(ert-deftest e-project-local-test-discover-layers-reuses-cached-snapshot ()
  "Repeated layer discovery reuses the warmed snapshot without reloading layer.el.
`e-project-local--discover-layers' reloads every `layer.el' and rebuilds each
layer's skills on every call, so an unchanged root must reuse the snapshot."
  (let* ((project (make-temp-file "e-project-local-layer-warm-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local--loaded-files (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (clrhash e-project-local--layer-snapshots)
          (setq e-project-local-test--layer-load-count 0)
          (e-project-local-test--make-layer
           project 'topic (e-project-local-test--counting-layer-source))
          ;; First discovery loads layer.el.
          (e-project-local--discover-layers project)
          (should (= e-project-local-test--layer-load-count 1))
          ;; Second discovery reuses the snapshot; layer.el must not reload.
          (let ((layers (e-project-local--discover-layers project)))
            (should (cl-find 'topic layers :key #'e-layer-id))
            (should (= e-project-local-test--layer-load-count 1))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-discover-layers-reloads-after-edit ()
  "Editing `layer.el' invalidates the warmed layer snapshot on next discovery."
  (let* ((project (make-temp-file "e-project-local-layer-edit-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local--loaded-files (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (clrhash e-project-local--layer-snapshots)
          (setq e-project-local-test--layer-load-count 0)
          (e-project-local-test--make-layer
           project 'topic (e-project-local-test--counting-layer-source))
          (e-project-local--discover-layers project)
          (should (= e-project-local-test--layer-load-count 1))
          ;; Rewrite layer.el with a strictly newer mtime.
          (e-project-local-test--make-layer
           project 'topic (e-project-local-test--counting-layer-source))
          (set-file-times
           (expand-file-name ".e/layers/topic/layer.el" project)
           (time-add (current-time) 5))
          (e-project-local--discover-layers project)
          (should (= e-project-local-test--layer-load-count 2)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-reset-loaded-files-clears-layer-snapshot ()
  "Resetting the loaded-file set clears the layer snapshot so discovery reloads."
  (let* ((project (make-temp-file "e-project-local-layer-reset-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local--loaded-files (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (clrhash e-project-local--layer-snapshots)
          (setq e-project-local-test--layer-load-count 0)
          (e-project-local-test--make-layer
           project 'topic (e-project-local-test--counting-layer-source))
          (e-project-local--discover-layers project)
          (should (= e-project-local-test--layer-load-count 1))
          (e-project-local-reset-loaded-files)
          (e-project-local--discover-layers project)
          (should (= e-project-local-test--layer-load-count 2)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-extensionless-loads-use-elc-without-source ()
  "Trusted project-local `.el' loads can resolve to `.elc' when source is gone."
  (let* ((project (make-temp-file "e-project-local-elc-only-" t))
         (layer-dir (expand-file-name ".e/layers/topic" project))
         (helper-file (expand-file-name "helper.el" layer-dir))
         (byte-compile-warnings nil))
    (unwind-protect
        (progn
          (when (boundp 'e-project-local-test--elc-only-helper-loaded)
            (makunbound 'e-project-local-test--elc-only-helper-loaded))
          (e-project-local-test--write-file
           helper-file
           "(setq e-project-local-test--elc-only-helper-loaded 'compiled)")
          (should (byte-compile-file helper-file))
          (delete-file helper-file)
          (e-project-local--with-extensionless-loads (list layer-dir)
            (load helper-file nil 'nomessage))
          (should (eq (symbol-value
                       'e-project-local-test--elc-only-helper-loaded)
                      'compiled)))
      (when (boundp 'e-project-local-test--elc-only-helper-loaded)
        (makunbound 'e-project-local-test--elc-only-helper-loaded))
      (delete-directory project t))))

(ert-deftest e-project-local-test-extensionless-loads-prefer-newer-source ()
  "Trusted project-local loads prefer newer `.el' source over stale `.elc'."
  (let* ((project (make-temp-file "e-project-local-prefer-newer-" t))
         (layer-dir (expand-file-name ".e/layers/topic" project))
         (helper-file (expand-file-name "helper.el" layer-dir))
         (byte-compile-warnings nil))
    (unwind-protect
        (progn
          (when (boundp 'e-project-local-test--prefer-newer-loaded)
            (makunbound 'e-project-local-test--prefer-newer-loaded))
          (e-project-local-test--write-file
           helper-file
           "(setq e-project-local-test--prefer-newer-loaded 'compiled)")
          (should (byte-compile-file helper-file))
          (e-project-local-test--write-file
           helper-file
           "(setq e-project-local-test--prefer-newer-loaded 'source)")
          (set-file-times helper-file (time-add (current-time) 1))
          (e-project-local--with-extensionless-loads (list layer-dir)
            (load helper-file nil 'nomessage))
          (should (eq (symbol-value
                       'e-project-local-test--prefer-newer-loaded)
                      'source)))
      (when (boundp 'e-project-local-test--prefer-newer-loaded)
        (makunbound 'e-project-local-test--prefer-newer-loaded))
      (delete-directory project t))))

(ert-deftest e-project-local-test-project-layer-bad-factory-result-signals ()
  "A project layer factory returning a mismatched id signals an error."
  (let* ((project (make-temp-file "e-project-local-layer-bad-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer
           project 'topic
           "(e-project-layer-register
             :id 'topic
             :factory (lambda (_dir)
                        (e-layer-create :id 'other :name \"Other\")))")
          (should-error (e-project-local-layer-create project)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-duplicate-project-layer-ids-nearest-wins ()
  "Nearer project layer directories win when ids collide."
  (let* ((outer (make-temp-file "e-project-local-layer-outer-" t))
         (inner (expand-file-name "inner" outer))
         (e-project-local-allowed-roots (list outer)))
    (unwind-protect
        (progn
          (make-directory inner t)
          (e-project-local-test--make-layer outer 'topic)
          (e-project-local-test--make-layer inner 'topic)
          (let* ((harness (e-harness-create))
                 (layer (e-project-local-layer-create inner))
                 (session (e-harness-create-session
                           harness
                           :metadata (list :project-root inner)))
                 (session-id (plist-get session :id)))
            (e-harness-set-intrinsic-capabilities
             harness (e-layer-capabilities layer))
            (let ((messages (prin1-to-string
                             (plist-get
                              (e-harness-context harness session-id)
                              :messages))))
              (should (string-match-p
                       (regexp-quote
                        (expand-file-name ".e/layers/topic" inner))
                       messages))
              (should-not (string-match-p
                           (regexp-quote
                            (expand-file-name ".e/layers/topic" outer))
                           messages)))))
      (delete-directory outer t))))


(ert-deftest e-project-local-test-action-dispatch-uses-session-root ()
  "Trusted project actions dispatch through the active session root."
  (let* ((project (make-temp-file "e-project-local-action-dispatch-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability
           project 'topic
           (e-project-local-test--action-capability-source
            'topic "Run the project action." "rooted-action"))
          (let ((harness (e-project-local-test--action-harness project)))
            (should
             (equal
              (e-actions-call
               'topic :run nil
               (list :harness harness :session-id "session-1"))
              "rooted-action"))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-action-resources-use-dynamic-actions ()
  "Action resources expose the same project action that dispatch resolves."
  (let* ((project (make-temp-file "e-project-local-action-resource-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability
           project 'topic
           (e-project-local-test--action-capability-source
            'topic "Run the dynamic project action." "resource-action"))
          (let* ((harness
                  (e-project-local-test--action-harness project t))
                 (resources
                  (e-harness-resources harness "session-1" "turn-1")))
            (should (string-match-p
                     "e-action://topic"
                     (e-resources-read resources "e-action://active" nil)))
            (should (string-match-p
                     "e-action://topic/run"
                     (e-resources-read resources "e-action://topic" nil)))
            (should (string-match-p
                     "Run the dynamic project action"
                     (e-resources-read resources "e-action://topic/run" nil)))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-action-discovery-reloads-after-edit ()
  "Editing a project action refreshes the next live lookup contract."
  (let* ((project (make-temp-file "e-project-local-action-edit-" t))
         (file (expand-file-name
                ".e/capabilities/topic/capability.el" project))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability
           project 'topic
           (e-project-local-test--action-capability-source
            'topic "Version one." "version-1"))
          (let ((harness (e-project-local-test--action-harness project t)))
            (should (equal
                     (e-actions-call
                      'topic :run nil
                      (list :harness harness :session-id "session-1"))
                     "version-1"))
            (should (string-match-p
                     "Version one"
                     (e-resources-read
                      (e-harness-resources harness "session-1" "turn-1")
                      "e-action://topic/run" nil)))
            (e-project-local-test--make-capability
             project 'topic
             (e-project-local-test--action-capability-source
              'topic "Version two." "version-2"))
            (set-file-times file (time-add (current-time) 5))
            (should (equal
                     (e-actions-call
                      'topic :run nil
                      (list :harness harness :session-id "session-1"))
                     "version-2"))
            (should (string-match-p
                     "Version two"
                     (e-resources-read
                      (e-harness-resources harness "session-1" "turn-2")
                      "e-action://topic/run" nil)))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-untrusted-action-discovery-does-not-load-elisp ()
  "Cold action discovery does not load Elisp outside the trust allowlist."
  (let* ((project (make-temp-file "e-project-local-action-untrusted-" t))
         (e-project-local-allowed-roots nil)
         (e-project-local-test--unexpected-inspect-load nil))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability
           project 'topic
           (e-project-local-test--action-capability-source
            'topic "Untrusted." "must-not-run"
            "(setq e-project-local-test--unexpected-inspect-load t)"))
          (let ((harness (e-project-local-test--action-harness project)))
            (should
             (equal
              (mapcar #'e-capability-id
                      (e-harness-effective-action-capabilities
                       harness "session-1" "turn-1"))
              '(project-local)))
            (should-not e-project-local-test--unexpected-inspect-load)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-prime-project-loads-allowed-extensions ()
  "Priming eagerly runs normal discovery for trusted extension projects."
  (let* ((project (make-temp-file "e-project-local-prime-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (let ((layer (e-project-local-prime-project project)))
            (should layer)
            (should (eq (e-layer-id layer) 'project-local))
            (should (cl-find 'topic
                             (e-layer-shells layer)
	                             :key #'e-shell-id))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-default-project-is-trusted-for-priming ()
  "A configured default project loads extensions without duplicate allowlisting."
  (let* ((project (make-temp-file "e-project-local-default-trust-" t))
         (e-default-projects (list project))
         (e-project-local-allowed-roots nil))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (let ((layer (e-project-local-prime-project project)))
            (should layer)
            (should (cl-find 'topic (e-layer-shells layer)
                             :key #'e-shell-id))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-startup-primes-default-projects-in-order ()
  "Project-local startup primes every normalized default project in order."
  (let* ((first (make-temp-file "e-project-local-default-first-" t))
         (second (make-temp-file "e-project-local-default-second-" t))
         (e-default-projects (list first first second))
         primed)
    (unwind-protect
        (cl-letf (((symbol-function 'e-project-local-prime-project)
                   (lambda (root) (push root primed))))
          (e-project-local-startup)
          (should (equal (nreverse primed)
                         (mapcar #'e-skills-normalize-directory
                                 (list first second)))))
      (delete-directory first t)
      (delete-directory second t))))

(ert-deftest e-project-local-test-inspect-action-does-not-load-elisp ()
  "The project-local inspect action reports extension files without loading them."
  (let* ((project (make-temp-file "e-project-local-action-inspect-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local-test--unexpected-inspect-load nil))
    (unwind-protect
        (progn
          (e-project-local-test--make-capability
           project 'topic
           (mapconcat #'identity
                      '("(setq e-project-local-test--unexpected-inspect-load t)"
                        "(e-project-capability-register"
                        " :id 'topic"
                        " :factory (lambda (_dir)"
                        "            (e-capability-create :id 'topic :name \"Topic\")))")
                      "\n"))
          (let ((harness (e-harness-create)))
            (e-harness-set-intrinsic-capabilities
             harness
             (list (e-project-local--dynamic-capability project)))
            (let* ((result
                    (e-actions-call
                     'project-local
                     :inspect
                     (list :directory project)
                     (list :harness harness)))
                   (capability (car (plist-get result :capabilities))))
              (should-not e-project-local-test--unexpected-inspect-load)
              (should (equal (plist-get result :directory)
                             (file-name-as-directory project)))
              (should (plist-get result :allowed))
              (should (plist-get result :has-extensions))
              (should (equal (plist-get capability :id) "topic"))
              (should (plist-get capability :allowed))
              (should (plist-get capability :readable)))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-status-context-does-not-load-cold-extensions ()
  "Optional project-local context reads only cached capability snapshots."
  (let* ((project (make-temp-file "e-project-local-status-cold-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local-test--unexpected-inspect-load nil)
         (capability (e-project-local--dynamic-capability project))
         (provider (car (e-capability-context-providers capability))))
    (unwind-protect
        (progn
          (clrhash e-project-local--capability-snapshots)
          (e-project-local-test--make-capability
           project 'topic
           (mapconcat #'identity
                      '("(setq e-project-local-test--unexpected-inspect-load t)"
                        "(e-project-capability-register"
                        " :id 'topic"
                        " :factory (lambda (_dir)"
                        "            (e-capability-create :id 'topic :name \"Topic\")))")
                      "\n"))
          (should-not
           (e-context-provider-build provider :context-purpose 'status))
          (should-not e-project-local-test--unexpected-inspect-load))
      (delete-directory project t))))

(ert-deftest e-project-local-test-status-context-reuses_cached_capabilities ()
  "Optional project-local context reuses warmed capability snapshots."
  (let* ((project (make-temp-file "e-project-local-status-warm-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local-test--unexpected-inspect-load nil)
         (capability (e-project-local--dynamic-capability project))
         (provider (car (e-capability-context-providers capability))))
    (unwind-protect
        (progn
          (clrhash e-project-local--capability-snapshots)
          (e-project-local-test--make-capability
           project 'topic
           (mapconcat #'identity
                      '("(setq e-project-local-test--unexpected-inspect-load t)"
                        "(e-project-capability-register"
                        " :id 'topic"
                        " :factory (lambda (directory)"
                        "            (e-capability-create"
                        "             :id 'topic"
                        "             :name \"Topic\""
                        "             :instructions"
                        "             (format \"Cached rooted at %s\" directory))))")
                      "\n"))
          (should (e-context-provider-build provider))
          (should e-project-local-test--unexpected-inspect-load)
          (setq e-project-local-test--unexpected-inspect-load nil)
          (let ((messages (e-context-provider-build
                           provider
                           :context-purpose 'status)))
            (should (string-match-p
                     "Cached rooted"
                     (mapconcat
                      (lambda (message)
                        (or (plist-get message :content) ""))
                      messages
                      "\n")))
            (should-not e-project-local-test--unexpected-inspect-load)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-turn-context-reuses-cached-capabilities ()
  "Live turn context loads extensions once and reuses the warmed snapshot."
  (let* ((project (make-temp-file "e-project-local-turn-warm-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local-test--unexpected-inspect-load nil)
         (capability (e-project-local--dynamic-capability project))
         (provider (car (e-capability-context-providers capability))))
    (unwind-protect
        (progn
          (clrhash e-project-local--capability-snapshots)
          (e-project-local-test--make-capability
           project 'topic
           (mapconcat #'identity
                      '("(setq e-project-local-test--unexpected-inspect-load t)"
                        "(e-project-capability-register"
                        " :id 'topic"
                        " :factory (lambda (directory)"
                        "            (e-capability-create"
                        "             :id 'topic"
                        "             :name \"Topic\""
                        "             :instructions"
                        "             (format \"Cached rooted at %s\" directory))))")
                      "\n"))
          ;; First live turn loads the extension file.
          (should (e-context-provider-build provider :context-purpose 'turn))
          (should e-project-local-test--unexpected-inspect-load)
          ;; Second live turn must reuse the snapshot, not re-load.
          (setq e-project-local-test--unexpected-inspect-load nil)
          (let ((messages (e-context-provider-build
                           provider :context-purpose 'turn)))
            (should (string-match-p
                     "Cached rooted"
                     (mapconcat
                      (lambda (message)
                        (or (plist-get message :content) ""))
                      messages
                      "\n")))
            (should-not e-project-local-test--unexpected-inspect-load)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-turn-context-reloads-after-edit ()
  "Editing a capability file invalidates the warmed snapshot on next turn."
  (let* ((project (make-temp-file "e-project-local-turn-edit-" t))
         (e-project-local-allowed-roots (list project))
         (capability (e-project-local--dynamic-capability project))
         (provider (car (e-capability-context-providers capability))))
    (cl-flet ((content (messages)
                (mapconcat (lambda (message)
                             (or (plist-get message :content) ""))
                           messages "\n"))
              (source (version)
                (mapconcat
                 #'identity
                 (list "(e-project-capability-register"
                       " :id 'topic"
                       " :factory (lambda (_dir)"
                       (format "  (e-capability-create :id 'topic :name \"Topic\" :instructions \"version-%s\")))"
                               version))
                 "\n")))
      (unwind-protect
          (progn
            (clrhash e-project-local--capability-snapshots)
            (e-project-local-test--make-capability project 'topic (source 1))
            (should (string-match-p
                     "version-1"
                     (content (e-context-provider-build
                               provider :context-purpose 'turn))))
            ;; Rewrite the capability with a strictly newer mtime.
            (e-project-local-test--make-capability project 'topic (source 2))
            (let ((file (expand-file-name
                         ".e/capabilities/topic/capability.el" project)))
              (set-file-times file (time-add (current-time) 5)))
            (should (string-match-p
                     "version-2"
                     (content (e-context-provider-build
                               provider :context-purpose 'turn)))))
        (delete-directory project t)))))

(ert-deftest e-project-local-test-prime-action-loads-allowed-extensions ()
  "The project-local prime action explicitly loads trusted extension files."
  (let* ((project (make-temp-file "e-project-local-action-prime-" t))
         (e-project-local-allowed-roots (list project))
         (e-project-local-test--prime-action-loaded nil))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer
           project 'topic
           (mapconcat #'identity
                      '("(setq e-project-local-test--prime-action-loaded t)"
                        "(e-project-layer-register"
                        " :id 'topic"
                        " :factory (lambda (_dir)"
                        "            (e-layer-create"
                        "             :id 'topic"
                        "             :name \"Topic\""
                        "             :shells (list (e-shell-create"
                        "                            :id 'topic"
                        "                            :name \"Topic\")))))")
                      "\n"))
          (let ((harness (e-harness-create)))
            (e-harness-set-intrinsic-capabilities
             harness
             (list (e-project-local--dynamic-capability project)))
            (let* ((dispatch
                    (e-actions-dispatch
                     'project-local
                     :prime
                     (list :directory project)
                     (list :harness harness)))
                   (result
                    (e-work-with-batch-await
                      (e-work-await-batch
                       (plist-get dispatch :request)
                       :timeout 1))))
              (should e-project-local-test--prime-action-loaded)
              (should (equal (plist-get result :status) "primed"))
              (should (eq (plist-get result :layer-id) 'project-local))
              (should (member 'topic (plist-get result :shell-ids))))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-byte-compile-project-local-files ()
  "The byte-compile command compiles runtime Elisp but excludes tests."
  (let* ((project (make-temp-file "e-project-local-byte-compile-" t))
         (layer-file (expand-file-name ".e/layers/topic/layer.el" project))
         (test-file
          (expand-file-name ".e/layers/topic/test/graphical-test.el" project))
         (e-project-local-allowed-roots (list project))
         (byte-compile-warnings nil))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (make-directory (file-name-directory test-file) t)
          (write-region
           ";;; graphical-test.el -*- lexical-binding: t; -*-\n(require 'feature-that-must-not-load)\n"
           nil test-file nil 'silent)
          (should-not (file-exists-p (byte-compile-dest-file layer-file)))
          (should-not (file-exists-p (byte-compile-dest-file test-file)))
          (let ((compiled (e-project-local-byte-compile-project project)))
            (should (member (byte-compile-dest-file layer-file) compiled))
            (should-not (member (byte-compile-dest-file test-file) compiled)))
          (should (file-exists-p (byte-compile-dest-file layer-file)))
          (should-not (file-exists-p (byte-compile-dest-file test-file))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-byte-compile-resolves-sibling-features-cold ()
  "Project compilation resolves a sibling `require' without prior loading."
  (let* ((project (make-temp-file "e-project-local-byte-sibling-" t))
         (layer-directory (expand-file-name ".e/layers/topic" project))
         (provider-file
          (expand-file-name
           "e-project-local-test-cold-sibling-provider.el" layer-directory))
         (consumer-file (expand-file-name "consumer.el" layer-directory))
         (feature 'e-project-local-test-cold-sibling-provider)
         (e-project-local-allowed-roots (list project))
         (byte-compile-warnings nil))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (e-project-local-test--write-file
           provider-file
           ";;; -*- lexical-binding: t; -*-\n(provide 'e-project-local-test-cold-sibling-provider)\n")
          (e-project-local-test--write-file
           consumer-file
           ";;; -*- lexical-binding: t; -*-\n(require 'e-project-local-test-cold-sibling-provider)\n")
          (when (featurep feature)
            (unload-feature feature t))
          (let* ((load-path (delete layer-directory load-path))
                 (compiled (e-project-local-byte-compile-project project)))
            (should (member (byte-compile-dest-file provider-file) compiled))
            (should (member (byte-compile-dest-file consumer-file) compiled)))
          (should (file-exists-p (byte-compile-dest-file provider-file)))
          (should (file-exists-p (byte-compile-dest-file consumer-file))))
      (when (featurep feature)
        (unload-feature feature t))
      (delete-directory project t))))

(ert-deftest e-project-local-test-prime-project-auto-recompile-is-opt-in ()
  "Project priming byte-recompiles stale local files only when enabled."
  (let* ((project (make-temp-file "e-project-local-auto-byte-" t))
         (layer-file (expand-file-name ".e/layers/topic/layer.el" project))
         (elc-file (byte-compile-dest-file layer-file))
         (e-project-local-allowed-roots (list project))
         (byte-compile-warnings nil))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer project 'topic)
          (let ((e-project-local-auto-byte-recompile nil))
            (should (e-project-local-prime-project project))
            (should-not (file-exists-p elc-file)))
          (let ((e-project-local-auto-byte-recompile t))
            (should (e-project-local-prime-project project))
            (should (file-exists-p elc-file))))
      (delete-directory project t))))

(ert-deftest e-project-local-test-prime-project-skips-untrusted-roots ()
  "Priming does not execute repository elisp outside the allowlist."
  (let ((project (make-temp-file "e-project-local-prime-gate-" t)))
    (unwind-protect
        (progn
          (e-project-local-test--make-layer
           project 'topic
           (mapconcat #'identity
                      '("(setq e-project-local-test--unexpected-prime-load t)"
                        "(e-project-layer-register"
                        " :id 'topic"
                        " :factory (lambda (_dir)"
                        "            (e-layer-create :id 'topic :name \"Topic\")))")
                      "\n"))
          (let ((e-project-local-allowed-roots nil)
                (e-project-local-test--unexpected-prime-load nil))
            (should-not (e-project-local-prime-project project))
            (should-not e-project-local-test--unexpected-prime-load)))
      (delete-directory project t))))

(ert-deftest e-project-local-test-prime-project-skips-projects-without-extensions ()
  "Priming is a no-op for projects without `.e' extension roots."
  (let* ((project (make-temp-file "e-project-local-prime-empty-" t))
         (e-project-local-allowed-roots (list project)))
    (unwind-protect
        (should-not (e-project-local-prime-project project))
      (delete-directory project t))))

(ert-deftest e-project-local-test-projectile-hooks-install-and-uninstall ()
  "Projectile integration installs only the project-local priming hook."
  (let ((old-after (and (boundp 'projectile-after-switch-project-hook)
                        projectile-after-switch-project-hook))
        (old-file (and (boundp 'projectile-find-file-hook)
                       projectile-find-file-hook))
        (old-dir (and (boundp 'projectile-find-dir-hook)
                      projectile-find-dir-hook)))
    (unwind-protect
        (progn
          (setq projectile-after-switch-project-hook nil
                projectile-find-file-hook nil
                projectile-find-dir-hook nil)
          (e-project-local-projectile-hooks-install)
          (should (memq #'e-project-local-prime-projectile-project
                        projectile-after-switch-project-hook))
          (should (memq #'e-project-local-prime-projectile-project
                        projectile-find-file-hook))
          (should (memq #'e-project-local-prime-projectile-project
                        projectile-find-dir-hook))
          (e-project-local-projectile-hooks-uninstall)
          (should-not (memq #'e-project-local-prime-projectile-project
                            projectile-after-switch-project-hook))
          (should-not (memq #'e-project-local-prime-projectile-project
                            projectile-find-file-hook))
          (should-not (memq #'e-project-local-prime-projectile-project
                            projectile-find-dir-hook)))
      (setq projectile-after-switch-project-hook old-after
            projectile-find-file-hook old-file
            projectile-find-dir-hook old-dir))))

(provide 'e-project-local-test)

;;; e-project-local-test.el ends here

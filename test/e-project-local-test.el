;;; e-project-local-test.el --- Tests for project-local capabilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for discovery, allowlist gating, and capability contribution of
;; project-local capabilities shipped under `.e/capabilities/'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'e)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-project-local)
(require 'e-store)
(require 'e-tools)

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
       :instructions \"Topic capability.\"
       :tools
       (list (lambda (registry)
               (e-tools-register
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
            (should (eq (e-capability-id (car capabilities)) 'topic))))
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
            (e-harness-activate-layer harness layer)
            (let ((tools (e-harness-tools harness))
                  (store (e-harness-store harness)))
              (should (member "topic_new" (e-tools-registry-order tools)))
              (should (cl-find "e://topic/state/topics.org"
                               (e-store-list store)
                               :key #'e-store-entry-uri
                               :test #'string=)))))
      (delete-directory project t))))

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
            (e-harness-activate-layer harness layer)
            (let* ((store (e-harness-store harness))
                   (uri "e://topic/skills/project/daily")
                   (entry (cl-find uri (e-store-list store)
                                   :key #'e-store-entry-uri
                                   :test #'string=)))
              (should entry)
              (should (string-match-p "Daily steps."
                                      (e-store-read-entry entry))))))
      (delete-directory project t))))

(provide 'e-project-local-test)

;;; e-project-local-test.el ends here

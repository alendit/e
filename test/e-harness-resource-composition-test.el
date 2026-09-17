;;; e-harness-resource-composition-test.el --- Public harness resource composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Resource, skill, project-root, and workspace composition scenarios.

;;; Code:

(require 'ert)
(load (expand-file-name "e-harness-composition-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest e-harness-test-resources-are-derived-from-effective-capabilities ()
  "The harness resource surface is rebuilt from effective capabilities on demand."
  (let* ((capability
          (e-capability-create
           :id 'resource-capability
           :resource-methods
           (list (lambda (registry)
                   (e-resources-register
                    registry
                    (e-resource-method-create
                     :scheme "derived"
                     :operation e-operation-read
                     :description "Derived resources."
                     :handler (lambda (_uri _range) "derived")))))))
         (layer (e-layer-create
                 :id 'resource-layer
                 :name "Resource Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (stale-resources (e-harness-resources harness)))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (should-error (e-resources-read stale-resources "derived://value" nil)
                  :type 'e-resources-unknown-scheme)
    (should (equal (e-resources-read
                    (e-harness-resources harness)
                    "derived://value"
                    nil)
                   "derived"))))

(ert-deftest e-harness-test-bash-tools-prefer-session-project-root ()
  "Session-scoped bash tools run in the session project root."
  (let* ((fallback-root (make-temp-file "e-harness-fallback-" t))
         (project-root (make-temp-file "e-harness-project-" t))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-base-layer-create fallback-root)))))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness
           :id "session-1"
           :metadata (list :project-root project-root))
          (let ((result (e-tools-execute-batch
                         (e-harness-tools harness "session-1" "turn-1")
                         '(:id "call-1"
                           :name "bash"
                           :arguments (:command "pwd")))))
            (should (equal (string-trim (plist-get result :content))
                           (directory-file-name project-root)))))
      (delete-directory fallback-root t)
      (delete-directory project-root t))))

(ert-deftest e-harness-test-file-resources-prefer-session-project-root ()
  "Session-scoped file resources resolve against the session project root."
  (let* ((fallback-root (make-temp-file "e-harness-fallback-" t))
         (project-root (make-temp-file "e-harness-project-" t))
         (nested (expand-file-name "docs/feature" project-root))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities
                   (e-layer-capabilities (e-base-layer-create fallback-root)))))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "rooted" nil
                        (expand-file-name "README.md" project-root)
                        nil 'silent)
          (e-harness-create-session
           harness
           :id "session-1"
           :metadata (list :project-root project-root))
          (should (equal (e-resources-read
                          (e-harness-resources harness "session-1" "turn-1")
                          "file://README.md"
                          nil)
                         "rooted")))
      (delete-directory fallback-root t)
      (delete-directory project-root t))))

(ert-deftest e-harness-test-built-in-resource-tools-dispatch-through-resources ()
  "Resource operation tools dispatch through active resource methods."
  (let* ((calls nil)
         (capability
          (e-capability-create
           :id 'resource-tool-capability
           :resource-methods
           (list (lambda (registry)
                   (dolist (method
                            (list
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-read
                              :description "Readable test resources."
                              :uri-patterns '("test://<value>")
                              :range-modes '("line")
                              :handler (lambda (uri range)
                                         (push (list :read uri range) calls)
                                         "read-result"))
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-write
                              :description "Writable test resources."
                              :uri-patterns '("test://<value>")
                              :handler (lambda (uri content)
                                         (push (list :write uri content) calls)
                                         "write-result"))
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-edit
                              :description "Editable test resources."
                              :uri-patterns '("test://<value>")
                              :handler (lambda (uri edits)
                                         (push (list :edit uri edits) calls)
                                         "edit-result"))
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-glob
                              :description "Glob test resources."
                              :uri-patterns '("test://<root>")
                              :handler (lambda (uri pattern limit case-sensitive)
                                         (push (list :glob uri pattern limit case-sensitive)
                                               calls)
                                         '(:resources [(:uri "test://value"
                                                       :name "value")]
                                           :truncated nil)))
                             (e-resource-method-create
                              :scheme "test"
                              :operation e-operation-search
                              :description "Search test resources."
                              :uri-patterns '("test://<root>")
                              :handler (lambda (uri query options)
                                         (push (list :search uri query options) calls)
                                         '(:matches [(:uri "test://value"
                                                     :line 1
                                                     :column 1
                                                     :text "needle")]
                                           :truncated nil)))))
                     (e-resources-register registry method))))))
         (layer (e-layer-create
                 :id 'resource-tool-layer
                 :name "Resource Tool Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (e-layer-capabilities layer)))
         (tools (e-harness-tools harness)))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     tools
                     '(:id "call-1"
                       :name "read"
                       :arguments (:uri "test://value"
                                   :range (:unit "line" :start 1 :end 2))))
                    :content)
                   "read-result"))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     tools
                     '(:id "call-2"
                       :name "write"
                       :arguments (:uri "test://value" :content "content")))
                    :content)
                   "write-result"))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     tools
                     '(:id "call-3"
                       :name "edit"
                       :arguments (:uri "test://value"
                                   :edits [(:oldText "a" :newText "b")])))
                    :content)
                   "edit-result"))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     tools
                     '(:id "call-4"
                       :name "glob"
                       :arguments (:uri "test://"
                                   :pattern "*.el"
                                   :limit 5)))
                    :content)
                   '(:resources [(:uri "test://value" :name "value")]
                     :truncated nil)))
    (should (equal (plist-get
                    (e-tools-execute-batch
                     tools
                     '(:id "call-5"
                       :name "search"
                       :arguments (:uri "test://"
                                   :query "needle"
                                   :glob "*.el"
                                   :limit 6)))
                    :content)
                   '(:matches [(:uri "test://value"
                                :line 1
                                :column 1
                                :text "needle")]
                     :truncated nil)))
    (should (equal (nreverse calls)
                   '((:read (:scheme "test" :address "value" :uri "test://value")
                            (:unit "line" :start 1 :end 2))
                     (:write (:scheme "test" :address "value" :uri "test://value")
                             "content")
                     (:edit (:scheme "test" :address "value" :uri "test://value")
                            [(:oldText "a" :newText "b")])
                     (:glob (:scheme "test" :address "" :uri "test://")
                            "*.el" 5 nil)
                     (:search (:scheme "test" :address "" :uri "test://")
                              "needle"
                              (:glob "*.el" :limit 6)))))))

(ert-deftest e-harness-test-resource-tool-descriptions-include-active-methods ()
  "Generated operation tool descriptions include active URI scheme metadata."
  (let* ((capability
          (e-capability-create
           :id 'resource-description-capability
           :resource-methods
           (list (lambda (registry)
                   (e-resources-register
                    registry
                    (e-resource-method-create
                     :scheme "described"
                     :operation e-operation-read
                     :description "Described resources."
                     :uri-patterns '("described://<id>")
                     :range-modes '("line" "offset")
                     :handler (lambda (_uri _range) "ok")))))))
         (layer (e-layer-create
                 :id 'resource-description-layer
                 :name "Resource Description Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :intrinsic-capabilities (e-layer-capabilities layer)))
         (read-tool (seq-find (lambda (definition)
                                (equal (plist-get definition :name) "read"))
                              (e-tools-definitions (e-harness-tools harness))))
         (description (plist-get read-tool :description)))
    (should read-tool)
    (should (string-match-p "described://<id>" description))
    (should (string-match-p "Described resources" description))
    (should (string-match-p "line, offset" description))))

(ert-deftest e-harness-test-built-in-read-loads-skill-resource ()
  "The built-in read tool can load full skill instructions on demand."
  (let* ((capability
          (e-capability-with-skills-create
           :id 'skill-capability
           :skills
           (list
            (e-skill-spec-create
             :name "planner"
             :description "Plan work."
             :content "Full planning instructions."))))
         (layer (e-layer-create
                 :id 'skill-layer
                 :name "Skill Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (read-call '(:id "call-1"
                      :name "read"
                      :arguments (:uri "e://skill-capability/skills/planner"))))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (let ((result (e-tools-execute-batch (e-harness-tools harness) read-call)))
      (should (equal (plist-get result :status) 'ok))
      (should (equal (plist-get result :content)
                     "Full planning instructions.")))))

(ert-deftest e-harness-test-built-in-read-loads-reference-resource ()
  "The built-in read tool can load capability reference resources on demand."
  (let* ((capability
          (e-capability-create
           :id 'reference-capability
           :resources
           (list (lambda (store capability)
                   (e-store-register
                    store
                    (e-capability-id capability)
                    "refs/guide.md"
                    :description "Reference guide."
                    :content "Reference guide content.")))))
         (layer (e-layer-create
                 :id 'reference-layer
                 :name "Reference Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
         (read-call '(:id "call-1"
                      :name "read"
                      :arguments (:uri
                                  "e://reference-capability/refs/guide.md"))))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (let ((result (e-tools-execute-batch (e-harness-tools harness) read-call)))
      (should (equal (plist-get result :status) 'ok))
      (should (equal (plist-get result :content)
                     "Reference guide content.")))))

(ert-deftest e-harness-test-store-resources-expose-glob-and-search ()
  "Capability e:// store resources expose generated glob and search tools."
  (let* ((capability
          (e-capability-create
           :id 'reference-capability
           :resources
           (list (lambda (store capability)
                   (e-store-register
                    store
                    (e-capability-id capability)
                    "refs/guide.md"
                    :description "Reference guide."
                    :content "Reference guide needle")))))
         (layer (e-layer-create
                 :id 'reference-layer
                 :name "Reference Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (should
     (equal (plist-get
             (e-tools-execute-batch
              (e-harness-tools harness)
              '(:id "call-1"
                :name "glob"
                :arguments (:uri "e://reference-capability"
                            :pattern "refs/*"
                            :limit 5)))
             :content)
            '(:resources [(:uri "e://reference-capability/refs/guide.md"
                            :name "refs/guide.md"
                            :kind "resource")]
              :truncated nil)))
    (let* ((content
            (plist-get
             (e-tools-execute-batch
              (e-harness-tools harness)
              '(:id "call-2"
                :name "search"
                :arguments (:uri "e://reference-capability"
                            :query "needle"
                            :glob "refs/*"
                            :limit 5)))
             :content))
           (match (aref (plist-get content :matches) 0)))
      (should-not (plist-get content :truncated))
      (should (equal (plist-get match :uri)
                     "e://reference-capability/refs/guide.md"))
      (should (= (plist-get match :line) 1))
      (should (= (plist-get match :column) 17))
      (should (equal (plist-get match :text)
                     "Reference guide needle")))))

(ert-deftest e-harness-test-skill-resources-do-not-support-write-or-edit ()
  "Skill resources are read-only even when advertised through resource tools."
  (let* ((capability
          (e-capability-with-skills-create
           :id 'skill-capability
           :skills
           (list
            (e-skill-spec-create
             :name "readonly"
             :description "Read-only skill."
             :content "Read-only instructions."))))
         (layer (e-layer-create
                 :id 'skill-layer
                 :name "Skill Layer"
                 :capabilities (list capability)))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
    (e-harness-set-intrinsic-capabilities harness (e-layer-capabilities layer))
    (should-error
     (e-resources-write (e-harness-resources harness)
                        "e://skill-capability/skills/readonly"
                        "Replacement")
     :type 'e-resources-unsupported-operation)
    (should-error
     (e-resources-edit (e-harness-resources harness)
                       "e://skill-capability/skills/readonly"
                       '((:oldText "Read" :newText "Write")))
     :type 'e-resources-unsupported-operation)))

(ert-deftest e-harness-test-workspace-roots-default-to-primary-only ()
  "Without configured extras, workspace roots are just the primary root."
  (let* ((primary (file-name-as-directory (make-temp-file "e-ws-primary-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-default-projects nil)
         (e-workspace-roots-alist nil))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (should (equal (e-harness-workspace-roots harness "s1")
                         (list primary))))
      (delete-directory primary t))))

(ert-deftest e-harness-test-workspace-roots-include-configured-extras ()
  "Configured extras for an ancestor key widen a session's workspace roots."
  (let* ((primary (file-name-as-directory (make-temp-file "e-ws-primary-" t)))
         (extra (file-name-as-directory (make-temp-file "e-ws-extra-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-default-projects nil)
         (e-workspace-roots-alist (list (cons primary (list extra)))))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (should (equal (e-harness-workspace-roots harness "s1")
                         (list primary extra))))
      (delete-directory primary t)
      (delete-directory extra t))))

(ert-deftest e-harness-test-workspace-roots-match-descendant-primary ()
  "An alist key that is an ancestor of the primary root still contributes."
  (let* ((parent (file-name-as-directory (make-temp-file "e-ws-parent-" t)))
         (primary (file-name-as-directory
                   (expand-file-name "child/" parent)))
         (extra (file-name-as-directory (make-temp-file "e-ws-extra-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-default-projects nil)
         (e-workspace-roots-alist (list (cons parent (list extra)))))
    (unwind-protect
        (progn
          (make-directory primary t)
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (should (member extra
                          (e-harness-workspace-roots harness "s1"))))
      (delete-directory parent t)
      (delete-directory extra t))))

(ert-deftest e-harness-test-default-project-roots-normalize-and-deduplicate ()
  "Default project roots preserve configured order after normalization."
  (let* ((first (file-name-as-directory (make-temp-file "e-default-first-" t)))
         (second (file-name-as-directory (make-temp-file "e-default-second-" t)))
         (e-default-projects
          (list (directory-file-name first) "" first second nil)))
    (unwind-protect
        (should (equal (e-default-project-roots) (list first second)))
      (delete-directory first t)
      (delete-directory second t))))

(ert-deftest e-harness-test-workspace-roots-include-default-projects-last ()
  "Specific extras precede global defaults without duplicating the primary."
  (let* ((primary (file-name-as-directory (make-temp-file "e-ws-primary-" t)))
         (specific (file-name-as-directory (make-temp-file "e-ws-specific-" t)))
         (default (file-name-as-directory (make-temp-file "e-ws-default-" t)))
         (store (e-session-store-create))
         (harness (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :sessions store))
         (e-default-projects (list default specific primary default))
         (e-workspace-roots-alist (list (cons primary (list specific)))))
    (unwind-protect
        (progn
          (e-harness-create-session
           harness :id "s1" :metadata (list :project-root primary))
          (should (equal (e-harness-workspace-roots harness "s1")
                         (list primary specific default))))
      (delete-directory primary t)
      (delete-directory specific t)
      (delete-directory default t))))

(provide 'e-harness-resource-composition-test)

;;; e-harness-resource-composition-test.el ends here

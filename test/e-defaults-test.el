;;; e-defaults-test.el --- Tests for default harness startup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for public default harness startup registration and construction.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-context)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-agents-std-context)
(require 'e-layer-selection)
(require 'e-layers)
(require 'e-openai)
(require 'e-session)
(require 'e-shells)

(defmacro e-defaults-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest e-defaults-test-startup-specs-only-include-chat-default ()
  "The startup default harness specs currently include only chat-default."
  (should (equal e-default-harness-specs
                 '((:id :chat-default
                    :factory e-default-chat-harness-create
                    :sync e-default-chat-harness-sync)))))

(ert-deftest e-defaults-test-registers-chat-default-factory ()
  "Startup default registration adds a lazy chat-default factory."
  (e-defaults-test--with-empty-harness-registry
    (e-default-harnesses-register)
    (should (member :chat-default (e-harness-registry-list)))
    (should-not (e-harness-registry-get :chat-default))))

(ert-deftest e-defaults-test-layer-specs-include-dev-harness-base-and-os-base ()
  "Built-in layer specs include dev, harness, and OS base layer ids."
  (should (memq 'e-dev
                (mapcar (lambda (spec) (plist-get spec :id))
                        e-default-layer-specs)))
  (should (memq 'harness-base
                (mapcar (lambda (spec) (plist-get spec :id))
                        e-default-layer-specs)))
  (should (memq 'os-base
                (mapcar (lambda (spec) (plist-get spec :id))
                        e-default-layer-specs)))
  (should (memq 'text-editing
                (mapcar (lambda (spec) (plist-get spec :id))
                        e-default-layer-specs)))
  (should-not (memq 'base
                    (mapcar (lambda (spec) (plist-get spec :id))
                            e-default-layer-specs))))

(ert-deftest e-defaults-test-chat-default-factory-is-lazy ()
  "Looking up chat-default delegates to `e-default-chat-harness-create'."
  (e-defaults-test--with-empty-harness-registry
    (let ((created nil)
          (fake (e-harness-create
                 :backend (e-backend-fake-create :items nil))))
      (cl-letf (((symbol-function 'e-default-chat-harness-create)
                 (lambda ()
                   (setq created t)
                   fake)))
        (e-default-harnesses-register
         '((:id :chat-default :factory e-default-chat-harness-create)))
        (should (eq (e-harness-registry-get-or-create :chat-default) fake))
        (should created)))))

(ert-deftest e-defaults-test-session-store-is-persistent-and-cached ()
  "The default session store is persistent and reused for the same directory."
  (let ((directory (make-temp-file "e-defaults-" t))
        (e-default--chat-sessions nil))
    (unwind-protect
        (let ((e-session-directory directory))
          (let ((first (e-default-session-store))
                (second (e-default-session-store)))
            (should (eq first second))
            (should (e-session-store-p first))
            (should (e-session-store-persistent first))
            (should (equal (file-name-as-directory
                            (expand-file-name directory))
                           (e-session-store-directory first)))))
      (delete-directory directory t))))

(ert-deftest e-defaults-test-chat-harness-uses-provider-and-session-store ()
  "Default chat harness creation delegates provider setup outside presentation."
  (let ((e-openai-default-provider 'openai-compatible-gateway)
        (store (e-session-store-create))
        seen-provider
        seen-sessions)
    (cl-letf (((symbol-function 'e-openai-create-harness)
               (lambda (&rest args)
                 (setq seen-provider (plist-get args :provider))
                 (setq seen-sessions (plist-get args :sessions))
                 (e-harness-create
                  :backend (e-backend-fake-create :items nil)
                  :sessions seen-sessions))))
      (let ((harness (e-default-chat-harness-create :sessions store)))
        (should (e-harness-p harness))
        (should (eq seen-provider 'openai-compatible-gateway))
        (should (eq seen-sessions store))))))

(ert-deftest e-defaults-test-chat-harness-activates-chat-session-base-and-emacs ()
  "Default chat harness activation includes chat-session and configured layers."
  (cl-letf (((symbol-function 'e-openai-create-harness)
             (lambda (&rest _args)
               (e-harness-create
                :backend (e-backend-fake-create :items nil)))))
    (let ((e-default-chat-layer-ids '(agents-std-context harness-base e os-base emacs-base)))
      (let ((harness (e-default-chat-harness-create)))
        (should (equal (mapcar #'e-layer-id
                               (e-harness-active-layers harness))
                       '(chat-session agents-std-context harness-base e os-base emacs-base)))
        (should (memq 'agents-std-context
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'session-tmp-resources
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'tool-output-truncation
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'chat-session
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'layer-selection
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))))))

(ert-deftest e-defaults-test-chat-harness-enables-web-and-text-editing-by-default ()
  "Default chat harness activation includes web and text-editing layers."
  (cl-letf (((symbol-function 'e-openai-create-harness)
             (lambda (&rest _args)
               (e-harness-create
                :backend (e-backend-fake-create :items nil)))))
    (let ((harness (e-default-chat-harness-create)))
      (should (memq 'web
                    (mapcar #'e-layer-id
                            (e-harness-active-layers harness))))
      (should (memq 'text-editing
                    (mapcar #'e-layer-id
                            (e-harness-active-layers harness))))
      (should (memq 'web
                    (mapcar #'e-capability-id
                            (e-harness-active-capabilities harness))))
      (should (memq 'annotations
                    (mapcar #'e-capability-id
                            (e-harness-active-capabilities harness)))))))

(ert-deftest e-defaults-test-chat-harness-uses-layer-ids-as-source-of-truth ()
  "Default chat harness creation uses configured layer ids."
  (cl-letf (((symbol-function 'e-openai-create-harness)
             (lambda (&rest _args)
               (e-harness-create
                :backend (e-backend-fake-create :items nil)))))
    (let ((e-default-chat-layer-ids '(e os-base web)))
      (let ((harness (e-default-chat-harness-create)))
        (should (equal (mapcar #'e-layer-id
                               (e-harness-active-layers harness))
                       '(chat-session e os-base web)))))))

(ert-deftest e-defaults-test-chat-harness-passes-directory-to-configured-layers ()
  "Default chat layer construction uses the requested project directory."
  (let ((project (make-temp-file "e-defaults-project-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".agents/skills/keep" project) t)
          (write-region
           "---\nname: Keep\ndescription: Keep skill.\n---\n\nKeep body."
           nil
           (expand-file-name ".agents/skills/keep/SKILL.md" project)
           nil
           'silent)
          (make-directory (expand-file-name ".agents/skills/drop" project) t)
          (write-region
           "---\nname: Drop\ndescription: Drop skill.\n---\n\nDrop body."
           nil
           (expand-file-name ".agents/skills/drop/SKILL.md" project)
           nil
           'silent)
          (write-region
           "((nil . ((e-capability-config . ((agents-std-context :include (\"Keep\")))))))"
           nil
           (expand-file-name ".dir-locals.el" project)
           nil
           'silent)
          (cl-letf (((symbol-function 'e-openai-create-harness)
                     (lambda (&rest _args)
                       (e-harness-create
                        :backend (e-backend-fake-create :items nil)))))
            (let ((e-default-chat-layer-ids '(agents-std-context))
                  (e-agents-std-context-global-agents-files nil)
                  (e-agents-std-context-global-skills-directory
                   (expand-file-name ".agents/skills/global" project)))
              (let* ((harness
                      (e-default-chat-harness-create :directory project))
                     (_session
                      (e-harness-create-session harness :id "session-1"))
                     (content
                      (mapconcat
                       (lambda (message)
                         (plist-get message :content))
                       (plist-get
                        (e-harness-context harness "session-1")
                        :messages)
                       "\n\n")))
                (should (string-match-p "Keep skill" content))
                (should-not (string-match-p "Drop skill" content))))))
      (delete-directory project t))))

(ert-deftest e-defaults-test-default-chat-layer_changes-update-config ()
  "Layer changes on the default chat harness update configured layer ids."
  (cl-letf (((symbol-function 'e-openai-create-harness)
             (lambda (&rest _args)
               (e-harness-create
                :backend (e-backend-fake-create :items nil)))))
    (let ((e-default-chat-layer-ids '(e os-base)))
      (let ((harness (e-default-chat-harness-create)))
        (e-layer-selection-enable harness 'web)
        (should (equal e-default-chat-layer-ids '(e os-base web)))
        (e-layer-selection-disable harness 'os-base)
        (should (equal e-default-chat-layer-ids '(e web)))))))

(ert-deftest e-defaults-test-startup-syncs-existing-chat-default-instance ()
  "Startup reconciles existing default chat harness instances from config."
  (e-defaults-test--with-empty-harness-registry
    (cl-letf (((symbol-function 'e-openai-create-harness)
               (lambda (&rest _args)
                 (e-harness-create
                  :backend (e-backend-fake-create :items nil)))))
      (let ((e-default-chat-layer-ids '(e os-base)))
        (e-default-harnesses-register)
        (let ((harness (e-harness-registry-get-or-create :chat-default)))
          (should (equal (mapcar #'e-layer-id
                                 (e-harness-active-layers harness))
                         '(chat-session e os-base)))
          (setq e-default-chat-layer-ids '(e web))
          (e-default-harnesses-startup)
          (should (eq (e-harness-registry-get :chat-default) harness))
          (should (equal (mapcar #'e-layer-id
                                 (e-harness-active-layers harness))
                         '(chat-session e web))))))))

(ert-deftest e-defaults-test-startup-refreshes-stale-chat-session-layer ()
  "Startup sync replaces stale internal chat-session layers in place."
  (e-defaults-test--with-empty-harness-registry
    (let* ((harness
            (e-harness-create
             :backend (e-backend-fake-create :items nil)))
           (stale-layer
            (e-layer-create
             :id 'chat-session
             :name "Chat Session"
             :capabilities
             (list (e-capability-create
                    :id 'chat-session
                    :name "Chat Session")))))
      (e-harness-activate-layer harness stale-layer)
      (e-harness-registry-register :chat-default harness)
      (let ((e-default-chat-layer-ids nil))
        (e-default-harnesses-startup))
      (should (eq (e-harness-registry-get :chat-default) harness))
      (should (equal (mapcar #'e-layer-id
                             (e-harness-active-layers harness))
                     '(chat-session)))
      (let* ((capability
              (cl-find 'chat-session
                       (e-harness-active-capabilities harness)
                       :key #'e-capability-id))
             (provider-names
              (mapcar #'e-context-provider--name
                      (e-capability-context-providers capability))))
        (should (memq 'chat-session-attachments provider-names))))))

(ert-deftest e-defaults-test-sync-clears-stale-layer-owned-shells ()
  "Default harness layer sync removes old layer-owned shell registrations."
  (let ((e-layer--registry (make-hash-table :test 'eq))
        (e-shell--registry (make-hash-table :test 'eq))
        (e-shell--scoped-registry (make-hash-table :test 'eq))
        (current-shell-id 'old-topic))
    (e-layer-register
     (e-layer-spec-create
      :id 'shell-sync
      :name "Shell Sync"
      :factory (lambda (&optional _directory)
                 (e-layer-create
                  :id 'shell-sync
                  :name "Shell Sync"
                  :shells (list (e-shell-create
                                 :id current-shell-id
                                 :name "Topic"))))))
    (let ((harness (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-default-chat-sync-harness-layers harness '(shell-sync) nil)
      (should (e-shell-get-active 'old-topic harness))
      (setq current-shell-id 'new-topic)
      (e-default-chat-sync-harness-layers harness '(shell-sync) nil)
      (should-not (e-shell-get-active 'old-topic harness))
      (should (e-shell-get-active 'new-topic harness)))))

(ert-deftest e-defaults-test-startup-refreshes-default-backend-from-spec-factory ()
  "Startup sync replaces stale default backend closures through generic specs."
  (e-defaults-test--with-empty-harness-registry
    (let* ((stale-backend (e-backend-fake-create :name "stale" :items nil))
           (fresh-backend (e-backend-fake-create :name "fresh" :items nil))
           (harness (e-harness-create :backend stale-backend))
           (custom-store (e-harness-sessions harness))
           factory-called)
      (e-harness-registry-register :generic-default harness)
      (cl-letf (((symbol-function 'e-defaults-test--generic-harness-create)
                 (lambda ()
                   (setq factory-called t)
                   (e-harness-create
                    :backend fresh-backend
                    :sessions (e-session-store-create)
                    :default-options '(:model "fresh-model")))))
        (let ((e-default-harness-specs
               '((:id :generic-default
                  :factory e-defaults-test--generic-harness-create))))
          (e-default-harnesses-startup)))
      (should factory-called)
      (should (eq (e-harness-registry-get :generic-default) harness))
      (should (eq (e-harness-backend harness) fresh-backend))
      (should (equal (e-harness-default-options harness)
                     '(:model "fresh-model")))
      (should (eq (e-harness-sessions harness) custom-store)))))

(ert-deftest e-defaults-test-startup-refreshes-default-context-strategy ()
  "Startup refreshes cached default transcript-stack strategies after reload."
  (e-defaults-test--with-empty-harness-registry
    (let* ((store (e-session-store-create))
           (stale-strategy
            (e-context-create
             :name 'transcript-stack
             :build (cl-function
                     (lambda (&key sessions session-id options)
                       (list :strategy 'transcript-stack
                             :messages (e-session-messages sessions session-id)
                             :options options)))))
           (harness (e-harness-create
                     :sessions store
                     :context-strategy stale-strategy)))
      (e-session-create store :id "session-1")
      (e-session-append-message
       store "session-1" '(:id "old" :role user :content "old transcript"))
      (e-session-append-message
       store "session-1" '(:id "kept" :role user :content "kept suffix"))
      (e-session-append-compaction
       store "session-1" "summary"
       :first-kept-entry-id "kept")
      (e-harness-registry-register :chat-default harness)
      (let ((e-default-chat-layer-ids nil))
        (e-default-harnesses-startup))
      (should (eq (e-harness-registry-get :chat-default) harness))
      (should-not (eq (e-harness-context-strategy harness) stale-strategy))
      (let ((context (e-harness-context harness "session-1")))
        (should (equal (mapcar (lambda (message)
                                 (plist-get message :content))
                               (plist-get context :messages))
                       '("summary" "kept suffix")))))))

(ert-deftest e-defaults-test-startup-preserves-custom-context-strategy ()
  "Startup does not replace custom cached context strategies."
  (e-defaults-test--with-empty-harness-registry
    (let* ((custom-strategy
            (e-context-create
             :name 'custom-context
             :build (cl-function
                     (lambda (&key sessions session-id options)
                       (ignore sessions session-id options)
                       '(:strategy custom-context
                         :messages ((:role user :content "custom")))))))
           (harness (e-harness-create :context-strategy custom-strategy)))
      (e-harness-registry-register :chat-default harness)
      (let ((e-default-chat-layer-ids nil))
        (e-default-harnesses-startup))
      (should (eq (e-harness-context-strategy harness) custom-strategy)))))

(provide 'e-defaults-test)

;;; e-defaults-test.el ends here

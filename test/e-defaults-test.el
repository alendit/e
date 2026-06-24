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
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-agents-std-context)
(require 'e-layer-selection)
(require 'e-layers)
(require 'e-prompts)
(require 'e-session)
(require 'e-shells)
(require 'e-tools)

(defmacro e-defaults-test--with-empty-harness-registry (&rest body)
  "Run BODY with an isolated harness registry."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     ,@body))

(defmacro e-defaults-test--with-configured-chat-factory (&rest body)
  "Run BODY with a configured fake default chat backend factory."
  (declare (indent 0) (debug t))
  `(let ((e-default-chat-harness-factory
          (lambda (&rest args)
            (e-harness-create
             :backend (e-backend-fake-create :items nil)
             :sessions (plist-get args :sessions)))))
     ,@body))

(defvar e-defaults-test--custom-chat-backend nil
  "Backend returned by `e-defaults-test--custom-chat-harness-create'.")

(defun e-defaults-test--custom-chat-harness-create (&rest args)
  "Create a custom chat harness for default harness tests."
  (e-harness-create
   :backend e-defaults-test--custom-chat-backend
   :sessions (plist-get args :sessions)))

(ert-deftest e-defaults-test-startup-specs-include-chat-and-debug-defaults ()
  "The startup default harness specs include chat and debug defaults."
  (should (equal e-default-harness-specs
                 '((:id :chat-default
                    :name "Default Chat"
                    :kind chat
                    :default t
                    :factory e-default-chat-harness-create
                    :sync e-default-chat-harness-sync)
                   (:id :debug-default
                    :name "Debug Agent"
                    :kind debug
                    :default t
                    :factory e-default-debug-harness-create
                    :sync e-default-debug-harness-sync)))))

(ert-deftest e-defaults-test-registers-chat-default-factory ()
  "Startup default registration adds a lazy chat-default factory."
  (e-defaults-test--with-empty-harness-registry
    (e-default-harnesses-register)
    (should (member :chat-default (e-harness-registry-list)))
    (should (eq (e-harness-instance-id
                 (e-harness-instance-default :kind 'chat))
                :chat-default))
    (should-not (e-harness-registry-get :chat-default))))

(ert-deftest e-defaults-test-registers-debug-default-factory ()
  "Startup default registration adds a lazy debug-default factory."
  (e-defaults-test--with-empty-harness-registry
    (e-default-harnesses-register)
    (should (member :debug-default (e-harness-registry-list)))
    (should (eq (e-harness-instance-id
                 (e-harness-instance-default :kind 'debug))
                :debug-default))
    (should-not (e-harness-registry-get :debug-default))))

(ert-deftest e-defaults-test-registers-debug-default-with-custom-chat-specs ()
  "Custom chat specs still receive the built-in debug default."
  (e-defaults-test--with-empty-harness-registry
    (let ((e-default-harness-specs
           '((:id :chat-default
              :name "Configured Chat"
              :kind chat
              :default t
              :factory e-default-chat-harness-create
              :sync e-default-chat-harness-sync))))
      (e-default-harnesses-register)
      (should (member :chat-default (e-harness-registry-list)))
      (should (member :debug-default (e-harness-registry-list)))
      (should (eq (e-harness-instance-id
                   (e-harness-instance-default :kind 'debug))
                  :debug-default)))))

(ert-deftest e-defaults-test-debug-default-uses-custom-chat-spec-backend ()
  "The built-in debug default derives from a custom chat default spec."
  (e-defaults-test--with-empty-harness-registry
    (let* ((backend (e-backend-fake-create :name "configured" :items nil))
           (e-defaults-test--custom-chat-backend backend)
           (e-default-chat-harness-factory nil)
           (e-default-harness-specs
            '((:id :chat-default
               :name "Configured Chat"
               :kind chat
               :default t
               :factory e-defaults-test--custom-chat-harness-create
               :sync e-default-chat-harness-sync))))
      (e-default-harnesses-register)
      (let ((chat (e-harness-registry-get-or-create :chat-default))
            (debug (e-harness-registry-get-or-create :debug-default)))
        (should-not (eq debug chat))
        (should (eq (e-harness-backend chat) backend))
        (should (eq (e-harness-backend debug) backend))
        (should-not (eq (e-harness-context-strategy debug)
                        (e-harness-context-strategy chat)))
        (should (memq 'debug-agent
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities debug))))))))

(ert-deftest e-defaults-test-startup-refreshes-debug-default-from-custom-chat-spec ()
  "Startup refreshes an existing debug default from a custom chat default spec."
  (e-defaults-test--with-empty-harness-registry
    (let* ((old-backend (e-backend-fake-create :name "old" :items nil))
           (new-backend (e-backend-fake-create :name "new" :items nil))
           (e-defaults-test--custom-chat-backend old-backend)
           (e-default-chat-harness-factory nil)
           (e-default-harness-specs
            '((:id :chat-default
               :name "Configured Chat"
               :kind chat
               :default t
               :factory e-defaults-test--custom-chat-harness-create
               :sync e-default-chat-harness-sync))))
      (e-default-harnesses-register)
      (let ((chat (e-harness-registry-get-or-create :chat-default))
            (debug (e-harness-registry-get-or-create :debug-default)))
        (should (eq (e-harness-backend chat) old-backend))
        (should (eq (e-harness-backend debug) old-backend))
        (setq e-defaults-test--custom-chat-backend new-backend)
        (e-default-harnesses-startup)
        (should (eq (e-harness-backend chat) new-backend))
        (should (eq (e-harness-backend debug) new-backend))
        (should (memq 'debug-agent
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities debug))))))))

(ert-deftest e-defaults-test-registers-chat-instance-for-legacy-spec ()
  "Legacy chat-default specs still register the default chat instance."
  (e-defaults-test--with-empty-harness-registry
    (e-default-harnesses-register
     '((:id :chat-default
        :factory e-default-chat-harness-create
        :sync e-default-chat-harness-sync)))
    (should (eq (e-harness-instance-id
                 (e-harness-instance-default :kind 'chat))
                :chat-default))))

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
  (should (memq 'agent-shell-fleet
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

(ert-deftest e-defaults-test-chat-harness-uses-unconfigured-backend-without-factory ()
  "Default chat creates no provider-backed harness without user configuration."
  (let ((e-default-chat-harness-factory nil)
        (e-default-chat-layer-ids nil))
    (let ((harness (e-default-chat-harness-create
                    :sessions (e-session-store-create))))
      (should (e-harness-p harness))
      (should (equal (e-backend--name (e-harness-backend harness))
                     "Unconfigured default chat backend"))
      (should-not (e-harness-default-options harness))
      (should-error
       (e-backend-stream
        (e-harness-backend harness)
        :messages nil
        :options nil
        :on-item #'ignore)
       :type 'user-error))))

(ert-deftest e-defaults-test-chat-harness-rejects-provider-argument ()
  "Default chat provider selection belongs in user configuration."
  (e-defaults-test--with-configured-chat-factory
    (let ((e-default-chat-layer-ids nil))
      (should-error
       (e-default-chat-harness-create :provider 'gateway)
       :type 'user-error))))

(ert-deftest e-defaults-test-chat-harness-uses-configured-factory-and-session-store ()
  "Default chat harness creation delegates backend setup to configuration."
  (let ((store (e-session-store-create))
        (directory "/tmp/e-configured-chat/")
        seen-directory
        seen-sessions)
    (let ((e-default-chat-harness-factory
           (lambda (&rest args)
             (setq seen-directory (plist-get args :directory))
             (setq seen-sessions (plist-get args :sessions))
             (e-harness-create
              :backend (e-backend-fake-create :items nil)
              :sessions seen-sessions))))
      (let ((e-default-chat-layer-ids nil))
        (let ((harness (e-default-chat-harness-create
                        :sessions store
                        :directory directory)))
          (should (e-harness-p harness))
          (should (eq seen-sessions store))
          (should (equal seen-directory directory)))))))

(ert-deftest e-defaults-test-chat-harness-preserves-configured-default-options ()
  "Default chat does not add provider-specific turn options."
  (let ((e-default-chat-harness-factory
         (lambda (&rest args)
           (e-harness-create
            :backend (e-backend-fake-create :items nil)
            :default-options '(:model "configured")
            :sessions (plist-get args :sessions)))))
    (let ((e-default-chat-layer-ids nil))
      (let ((harness (e-default-chat-harness-create
                      :sessions (e-session-store-create))))
        (should (equal (e-harness-default-options harness)
                       '(:model "configured")))
        (should-not (plist-member (e-harness-default-options harness)
                                  :prompt-cache))
        (should-not (plist-member (e-harness-default-options harness)
                                  :prompt-cache-ttl))))))

(ert-deftest e-defaults-test-chat-harness-activates-chat-session-base-and-emacs ()
  "Default chat harness activation includes chat-session and configured layers."
  (e-defaults-test--with-configured-chat-factory
    (let ((e-default-chat-layer-ids '(agents-std-context harness-base e os-base emacs-base)))
      (let ((harness (e-default-chat-harness-create)))
        (should (equal (e-harness-enabled-layer-ids harness)
                       '(agents-std-context harness-base e os-base emacs-base)))
        (should (equal (e-harness-effective-layer-ids harness)
                       '(agents-std-context harness-base e os-base emacs-base)))
        (should (memq 'chat-session
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
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
                              (e-harness-active-capabilities harness))))
        (should (e-harness-prompt-by-name harness "summarize"))
        (should (string-match-p
                 "Summarize"
                 (e-prompt-render
                  (e-harness-prompt-by-name harness "summarize")
                  nil)))))))

(ert-deftest e-defaults-test-debug-harness-uses-chat-preset-and-debug-guidance ()
  "Default debug harness activation includes chat layers plus debug guidance."
  (e-defaults-test--with-configured-chat-factory
    (let ((e-default-chat-layer-ids '(agents-std-context harness-base e)))
      (let ((harness (e-default-debug-harness-create)))
        (should (equal (e-harness-enabled-layer-ids harness)
                       '(agents-std-context harness-base e)))
        (should (equal (e-harness-effective-layer-ids harness)
                       '(agents-std-context harness-base e)))
        (should (memq 'chat-session
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'context-inspection
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (should (memq 'debug-agent
                      (mapcar #'e-capability-id
                              (e-harness-active-capabilities harness))))
        (let ((instructions
               (e-capability-instructions
                (cl-find 'debug-agent
                         (e-harness-active-capabilities harness)
                         :key #'e-capability-id))))
          (should (string-match-p "debug popup" instructions))
          (should (string-match-p "Do not treat the debug popup" instructions))
        (should (string-match-p "dismiss the debug popup" instructions)))
        (should-not (e-harness-layer-change-function harness))))))

(ert-deftest e-defaults-test-chat-harness-enables-web-and-text-editing-by-default ()
  "Default chat harness activation includes web and text-editing layers."
  (e-defaults-test--with-configured-chat-factory
    (let ((harness (e-default-chat-harness-create)))
      (should (memq 'web
                    (e-harness-enabled-layer-ids harness)))
      (should (memq 'text-editing
                    (e-harness-enabled-layer-ids harness)))
      (should (memq 'web
                    (mapcar #'e-capability-id
                            (e-harness-active-capabilities harness))))
      (should (memq 'annotations
                    (mapcar #'e-capability-id
                            (e-harness-active-capabilities harness)))))))

(ert-deftest e-defaults-test-chat-harness-uses-layer-ids-as-source-of-truth ()
  "Default chat harness creation uses configured layer ids."
  (e-defaults-test--with-configured-chat-factory
    (let ((e-default-chat-layer-ids '(e os-base web)))
      (let ((harness (e-default-chat-harness-create)))
        (should (equal (e-harness-enabled-layer-ids harness)
                       '(e os-base web)))))))

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
          (e-defaults-test--with-configured-chat-factory
            (let ((e-default-chat-layer-ids '(agents-std-context))
                  (e-agents-std-context-global-agents-files nil)
                  (e-agents-std-context-global-skills-directory
                   (expand-file-name ".agents/skills/global" project)))
              (let* ((harness
                      (e-default-chat-harness-create
                       :directory project
                       :sessions (e-session-store-create)))
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
  (e-defaults-test--with-configured-chat-factory
    (let ((e-default-chat-layer-ids '(e os-base)))
      (let ((harness (e-default-chat-harness-create)))
        (e-layer-selection-enable harness 'web)
        (should (equal e-default-chat-layer-ids '(e os-base web)))
        (e-layer-selection-disable harness 'os-base)
        (should (equal e-default-chat-layer-ids '(e web)))))))

(ert-deftest e-defaults-test-startup-syncs-existing-chat-default-instance ()
  "Startup reconciles existing default chat harness instances from config."
  (e-defaults-test--with-empty-harness-registry
    (e-defaults-test--with-configured-chat-factory
      (let ((e-default-chat-layer-ids '(e os-base)))
        (e-default-harnesses-register)
        (let ((harness (e-harness-registry-get-or-create :chat-default)))
          (should (equal (e-harness-enabled-layer-ids harness)
                         '(e os-base)))
          (setq e-default-chat-layer-ids '(e web))
          (e-default-harnesses-startup)
          (should (eq (e-harness-registry-get :chat-default) harness))
          (should (equal (e-harness-enabled-layer-ids harness)
                         '(e web))))))))

(ert-deftest e-defaults-test-startup-refreshes-stale-chat-session-capability ()
  "Startup sync replaces stale intrinsic chat-session capabilities in place."
  (e-defaults-test--with-empty-harness-registry
    (let* ((harness
            (e-harness-create
             :backend (e-backend-fake-create :items nil)))
           (stale-capability
            (e-capability-create
             :id 'chat-session
             :name "Chat Session")))
      (e-harness-set-intrinsic-capabilities harness (list stale-capability))
      (e-harness-registry-register :chat-default harness)
      (let ((e-default-chat-layer-ids nil))
        (e-default-harnesses-startup))
      (should (eq (e-harness-registry-get :chat-default) harness))
      (should-not (e-harness-enabled-layer-ids harness))
      (let* ((capability
              (cl-find 'chat-session
                       (e-harness-active-capabilities harness)
                       :key #'e-capability-id))
             (provider-names
              (mapcar #'e-context-provider--name
                      (e-capability-context-providers capability))))
        (should (memq 'chat-session-attachments provider-names))
        (should (eq (e-context-provider-cache-placement
                     (car (e-capability-context-providers capability)))
                    'dynamic-context))))))

(ert-deftest e-defaults-test-startup-marks-chat-backend-unconfigured-without-config-factory ()
  "Startup removes cached chat backend providers when no config factory exists."
  (e-defaults-test--with-empty-harness-registry
    (let* ((backend (e-backend-fake-create :name "configured" :items nil))
           (harness (e-harness-create :backend backend))
           (sessions (e-harness-sessions harness)))
      (e-harness-registry-register :chat-default harness)
      (let ((e-default-chat-harness-factory nil)
            (e-default-chat-layer-ids nil))
        (e-default-harnesses-startup))
      (should (eq (e-harness-registry-get :chat-default) harness))
      (should-not (eq (e-harness-backend harness) backend))
      (should (equal (e-backend--name (e-harness-backend harness))
                     "Unconfigured default chat backend"))
      (should (eq (e-harness-sessions harness) sessions))
      (should-error
       (e-backend-stream
        (e-harness-backend harness)
        :messages nil
        :options nil
        :on-item #'ignore)
       :type 'user-error))))

(ert-deftest e-defaults-test-startup-repairs-shifted-chat-default-session-store ()
  "Startup sync repairs cached chat-default harnesses with shifted reload slots."
  (e-defaults-test--with-empty-harness-registry
    (let* ((store (e-session-store-create))
           (bad-session-slot
            (list (e-layer-create :id 'chat-session
                                  :name "Old Chat Session")))
           (harness (e-harness-create
                     :backend (e-backend-fake-create :items nil))))
      (setf (e-harness-runtime-capability-config harness) store)
      (setf (e-harness-sessions harness) bad-session-slot)
      (setf (e-harness-enabled-layer-ids harness)
            (make-hash-table :test 'equal))
      (setf (e-harness-intrinsic-capabilities harness)
            (make-hash-table :test 'equal))
      (setf (e-harness-subscribers harness)
            (make-hash-table :test 'equal))
      (setf (e-harness-active-turns harness) bad-session-slot)
      (setf (e-harness-prompt-queues harness) bad-session-slot)
      (e-harness-registry-register :chat-default harness)
      (let ((e-default-chat-harness-factory nil)
            (e-default-chat-layer-ids nil))
        (e-default-harnesses-startup))
      (should (eq (e-harness-registry-get :chat-default) harness))
      (should (eq (e-harness-sessions harness) store))
      (should (e-session-store-p (e-harness-sessions harness)))
      (should-not (e-session-store-p
                   (e-harness-runtime-capability-config harness)))
      (should-not (e-harness-enabled-layer-ids harness))
      (should (listp (e-harness-subscribers harness)))
      (should (hash-table-p (e-harness-active-turns harness)))
      (should (hash-table-p (e-harness-prompt-queues harness)))
      (should (memq 'chat-session
                    (mapcar #'e-capability-id
                            (e-harness-active-capabilities harness)))))))

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

(ert-deftest e-defaults-test-sync-directory-does-not-change-session-root-context ()
  "Default sync can rebuild shells from one root without changing session tools."
  (let ((e-layer--registry (make-hash-table :test 'eq))
        (e-shell--registry (make-hash-table :test 'eq))
        (e-shell--scoped-registry (make-hash-table :test 'eq))
        (sync-root (make-temp-file "e-default-sync-root-" t))
        (session-root (make-temp-file "e-default-session-root-" t)))
    (unwind-protect
        (let ((harness (e-harness-create
                        :backend (e-backend-fake-create :items nil))))
          (e-layer-register
           (e-layer-spec-create
            :id 'root-sensitive
            :name "Root Sensitive"
            :factory
            (lambda (&optional directory)
              (let* ((session-root-p (and directory
                                          (file-equal-p directory
                                                        session-root)))
                     (tool-name (if session-root-p
                                    "session_root_tool"
                                  "sync_root_tool"))
                     (shell-id (if session-root-p
                                   'session-root-shell
                                 'sync-root-shell)))
                (e-layer-create
                 :id 'root-sensitive
                 :name "Root Sensitive"
                 :capabilities
                 (list
                  (e-capability-create
                   :id 'root-sensitive
                   :instructions "root sensitive instructions"
                   :context-providers
                   (list
                    (e-context-provider-create
                     :name 'root-sensitive
                     :cache-placement 'dynamic-context
                     :build (lambda (&rest _args)
                              (list
                               (list :role 'system
                                     :content
                                     (concat "model root: "
                                             (file-name-nondirectory
                                              (directory-file-name
                                               directory))))))))
                   :tools
                   (list
                    (lambda (registry)
                      (e-tools-register
                       registry
                       :name tool-name
                       :description "Root-sensitive test tool."
                       :handler (lambda (_arguments) tool-name))))))
                 :shells
                 (list (e-shell-create
                        :id shell-id
                        :name (symbol-name shell-id))))))))
          (e-default-chat-sync-harness-layers
           harness '(root-sensitive) session-root)
          (should (e-shell-get-active 'session-root-shell harness))
          (let* ((session (e-harness-create-session
                           harness
                           :metadata (list :project-root session-root)))
                 (session-id (plist-get session :id))
                 (context-before (e-harness-context
                                  harness session-id "turn-1"))
                 (fingerprints-before
                  (e-harness--provider-anchor-fingerprints context-before))
                 (tool-names-before
                  (mapcar (lambda (definition)
                            (plist-get definition :name))
                          (e-tools-definitions
                           (e-harness-tools harness session-id "turn-1"))))
                 (messages-before (plist-get context-before :messages)))
            (e-default-chat-sync-harness-layers
             harness '(root-sensitive) sync-root)
            (should-not (e-shell-get-active 'session-root-shell harness))
            (should (e-shell-get-active 'sync-root-shell harness))
            (let* ((context-after (e-harness-context
                                   harness session-id "turn-2"))
                   (fingerprints-after
                    (e-harness--provider-anchor-fingerprints context-after))
                   (tool-names-after
                    (mapcar (lambda (definition)
                              (plist-get definition :name))
                            (e-tools-definitions
                             (e-harness-tools harness session-id "turn-2")))))
              (should (member "session_root_tool" tool-names-before))
              (should-not (member "sync_root_tool" tool-names-before))
              (should (equal tool-names-after tool-names-before))
              (should (equal (plist-get context-after :messages)
                             messages-before))
              (should (equal (plist-get fingerprints-after :segments)
                             (plist-get fingerprints-before :segments)))
              (should (equal (plist-get fingerprints-after :tools)
                             (plist-get fingerprints-before :tools)))))))
      (delete-directory sync-root t)
      (delete-directory session-root t)))

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

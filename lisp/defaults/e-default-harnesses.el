;;; e-default-harnesses.el --- Startup harness specs for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Startup registration for named default harnesses.  This module registers
;; lazy factories only; provider adapters are selected by user configuration.

;;; Code:

(declare-function e-chat-session-capability-create "e-chat-session")

(require 'cl-lib)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-default-layers)
(require 'e-context)
(require 'e-context-inspection)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-layers)
(require 'e-prompts)
(require 'e-session)
(require 'e-runtime-sqlite)
(require 'e-task-queue-actions)
(require 'e-shells)
(require 'e-startup)

(defgroup e-defaults nil
  "Default harness startup configuration for e."
  :group 'e
  :prefix "e-default-")

(defcustom e-default-harness-specs
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
     :sync e-default-debug-harness-sync))
  "Default harness specs registered during package startup."
  :type '(repeat sexp)
  :group 'e-defaults)

(defconst e-default-debug-harness-spec
  '(:id :debug-default
    :name "Debug Agent"
    :kind debug
    :default t
    :factory e-default-debug-harness-create
    :sync e-default-debug-harness-sync)
  "Built-in debug harness spec appended to chat-only custom defaults.")

(defcustom e-default-chat-layer-ids
  '(agents-std-context harness-base process-reporting harness-advanced e os-base emacs-base resource-toc web annotations org-canvas project-local writing subagents-parent)
  "Layer ids activated by default chat harnesses.

This option is the source of truth for the stateless layer preset attached to
the default chat harness.  Runtime layer enable/disable commands update this
option when they operate on that harness, and existing default harness instances
are reconciled from it during startup/reload."
  :type '(repeat symbol)
  :group 'e-defaults)

(defcustom e-default-chat-harness-factory nil
  "Configured factory used to create the default chat harness backend.

When nil, e creates a default chat harness with an explicit unconfigured
backend that reports a configuration error instead of choosing a provider.
Configure this from config.el/config.org, or replace the `:chat-default' entry
in `e-default-harness-specs'.  The function is called with keyword arguments
`:sessions' and `:directory' and must return an `e-harness'.  Default chat then
attaches the internal chat-session layer and `e-default-chat-layer-ids'."
  :type '(choice (const :tag "Unconfigured" nil)
                 function)
  :group 'e-defaults)

(defconst e-default-chat--unconfigured-backend-message
  "Default chat backend is not configured; set e-default-chat-harness-factory or e-default-harness-specs in config.el/config.org")

(defconst e-default-debug-instructions
  "You are the standing e debug agent. Diagnose the current e runtime state from evidence before recommending a fix. Prefer reading exported context, recent failures, session metadata, buffers, and relevant source over guessing. Do not mutate buffers, retry provider calls, or change configuration unless the user explicitly asks for that action. The debug session is often shown in an e-debug popup/posframe so the user can discuss a separate problem; do not treat the debug popup's frame, window, mode line, workspace, or display parameters as evidence for the reported issue unless the prompt is specifically about the debug popup. If the popup blocks inspection of the underlying UI, you may dismiss the debug popup non-destructively before continuing investigation. When proposing remediation, name the owning component and the smallest safe change."
  "System guidance attached to the default debug harness.")

(defvar e-default--chat-sessions nil
  "Cached default SQLite-backed chat session port.")

(defvar e-default--runtime nil
  "The process-wide default SQLite runtime composition.")

(defun e-default-common-state-directory ()
  "Return the common e state root established by session configuration.

The retired session owner used a dedicated child directory while the other
owners used siblings below the common root.  Keeping this derivation here
lets defaults assemble the process without teaching storage adapters about
another owner's configuration."
  (file-name-as-directory
   (file-name-directory
    (directory-file-name (expand-file-name e-session-directory)))))

(defun e-default-runtime-directory ()
  "Return the ordinary runtime state directory.

Tests and operators may isolate startup by setting
`E_RUNTIME_STATE_DIRECTORY'.  Otherwise use the common e state root that
contains the established session directory and its sibling legacy owners."
  (file-name-as-directory
   (expand-file-name
    (or (getenv "E_RUNTIME_STATE_DIRECTORY")
        (e-default-common-state-directory)))))

(defun e-default-runtime ()
  "Return the process-wide default SQLite runtime, opening it once."
  (let ((directory (e-default-runtime-directory)))
    (unless (and (e-runtime-sqlite-p e-default--runtime)
                 (not (e-runtime-sqlite--closed e-default--runtime))
                 (equal (e-runtime-sqlite--directory e-default--runtime)
                        directory))
      (when (and e-default--runtime
                 (not (e-runtime-sqlite--closed e-default--runtime)))
        (signal 'e-runtime-sqlite-live-composition
                (list "Default runtime directory changed while live"
                      (e-runtime-sqlite--directory e-default--runtime)
                      directory)))
      (setq e-default--runtime
            (e-runtime-sqlite-open directory :load-sessions t)
            e-default--chat-sessions
            (e-runtime-sqlite-session-store e-default--runtime)))
    (e-task-queue-actions-configure-queue
     (e-runtime-sqlite-task-queue e-default--runtime))
    e-default--runtime))

(defun e-default-runtime-close ()
  "Close the process-wide default runtime exactly once."
  (when (e-runtime-sqlite-p e-default--runtime)
    (e-runtime-sqlite-close e-default--runtime))
  (e-task-queue-actions-configure-queue nil)
  (setq e-default--runtime nil
        e-default--chat-sessions nil)
  t)

(defun e-default-chat--prompt-capability ()
  "Return built-in prompt templates for default chat harnesses."
  (e-capability-with-prompts-create
   :id 'chat-prompts
   :name "Chat Prompts"
   :prompts
   (list
    (e-prompt-spec-create
     :name "summarize"
     :description "Summarize the current context."
     :parameters
     (list (e-prompt-parameter-create
            :name "focus"
            :description "What the summary should focus on."
            :required nil
            :default "the current context"))
     :template "Summarize ${focus} clearly and concisely.")
    (e-prompt-spec-create
     :name "review"
     :description "Review for bugs, risks, and missing tests."
     :parameters
     (list (e-prompt-parameter-create
            :name "focus"
            :description "What the review should focus on."
            :required nil
            :default "the current context"))
     :template
     "Review ${focus} for bugs, regressions, and missing tests."))))

(defun e-default-session-store ()
  "Return the default SQLite-backed session port."
  (let ((store (e-runtime-sqlite-session-store (e-default-runtime))))
    (e-session-enable store)
    store))

(add-hook 'kill-emacs-hook #'e-default-runtime-close)

(defun e-default-chat--record-layer-ids (harness)
  "Record HARNESS explicitly enabled registered layer ids as default chat config."
  (setq e-default-chat-layer-ids
        (copy-sequence (e-harness-enabled-layer-ids harness)))
  e-default-chat-layer-ids)

(defun e-default-chat--set-configured-layer-ids
    (harness &optional layer-ids directory)
  "Set configured stateless LAYER-IDS on HARNESS.
When LAYER-IDS is nil, use `e-default-chat-layer-ids'.
DIRECTORY is passed to config-aware shell layer factories."
  (e-default-layers-register)
  (e-harness-set-enabled-layer-ids
   harness
   (or layer-ids e-default-chat-layer-ids))
  (e-harness-sync-layer-shells harness directory)
  harness)

(defun e-default-chat--chat-session-capabilities ()
  "Return fresh internal chat-session capabilities for default chat harnesses."
  (require 'e-chat-session)
  (list (e-chat-session-capability-create)
        (e-default-chat--prompt-capability)))

(defun e-default-debug--debug-capabilities ()
  "Return fresh internal debug guidance capabilities."
  (list (e-capability-create
         :id 'debug-agent
         :name "Debug Agent"
         :instructions e-default-debug-instructions)
        (e-context-inspection-capability-create)))

(defun e-default-harness--debug-spec-p (spec)
  "Return non-nil when SPEC provides a debug default harness."
  (or (eq (plist-get spec :id) :debug-default)
      (eq (plist-get spec :kind) 'debug)))

(defun e-default-harness--effective-specs (&optional specs)
  "Return harness SPECS, appending the built-in debug spec to implicit defaults."
  (if specs
      specs
    (if (cl-some #'e-default-harness--debug-spec-p e-default-harness-specs)
        e-default-harness-specs
      (append e-default-harness-specs
              (list e-default-debug-harness-spec)))))

(defun e-default-chat-sync-harness-layers
    (harness &optional layer-ids directory)
  "Reconcile default chat HARNESS layer ids from `e-default-chat-layer-ids'.
The internal chat-session capabilities are intrinsic to HARNESS.  Registered
layer objects are created only as effective views or for shell sync.  DIRECTORY
is passed to config-aware shell layer factories."
  (e-harness-refresh-default-context-strategy harness)
  (when directory
    (setf (e-harness-default-project-root harness)
          (e-harness-normalize-project-root directory)))
  (e-harness-set-layer-change-function harness nil)
  (e-harness-set-intrinsic-capabilities
   harness
   (e-default-chat--chat-session-capabilities))
  (e-default-chat--set-configured-layer-ids
   harness layer-ids directory)
  (e-harness-set-layer-change-function
   harness #'e-default-chat--record-layer-ids)
  (e-harness-notify-layers-changed harness)
  harness)

(defun e-default-harness-sync-from-factory (harness spec)
  "Refresh HARNESS generic runtime fields from SPEC's fresh factory result.

Live reload can replace adapter helper functions without recreating cached
default harnesses.  Refreshing the runtime updates stale backend closures while
preserving sessions, context state, and active presentation buffers that already
hold HARNESS."
  (let ((fresh (funcall (plist-get spec :factory))))
    (unless (e-harness-p fresh)
      (signal 'wrong-type-argument (list 'e-harness-p fresh)))
    (e-harness-request-runtime-refresh harness fresh))
  harness)

(defun e-default-chat--default-factory-spec-p (spec)
  "Return non-nil when SPEC uses the built-in default chat factory."
  (eq (plist-get spec :factory) 'e-default-chat-harness-create))

(defun e-default-chat--sync-from-configured-factory-p (spec)
  "Return non-nil when SPEC has a configured factory to refresh runtime fields."
  (or (not (e-default-chat--default-factory-spec-p spec))
      (functionp e-default-chat-harness-factory)))

(defun e-default-chat--unconfigured-backend ()
  "Return a backend that reports missing default chat configuration."
  (e-backend-create
   :name "Unconfigured default chat backend"
   :stream (lambda (&rest _args)
             (user-error e-default-chat--unconfigured-backend-message))))

(defun e-default-chat--mark-unconfigured (harness)
  "Mark HARNESS as an unconfigured default chat harness."
  (setf (e-harness-backend harness)
        (e-default-chat--unconfigured-backend))
  (setf (e-harness-default-options harness) nil)
  harness)

(defun e-default-chat-harness-sync (harness spec)
  "Reconcile cached default chat HARNESS from SPEC."
  (if (e-default-chat--sync-from-configured-factory-p spec)
      (e-default-harness-sync-from-factory harness spec)
    (progn
      (e-default-chat--mark-unconfigured harness)))
  (e-default-chat-sync-harness-layers harness)
  harness)

(defun e-default-debug-sync-harness-layers
    (harness &optional layer-ids directory)
  "Reconcile default debug HARNESS layers from chat defaults and debug guidance."
  (e-harness-set-layer-change-function harness nil)
  (e-default-chat-sync-harness-layers harness layer-ids directory)
  (e-harness-set-intrinsic-capabilities
   harness
   (append (e-default-chat--chat-session-capabilities)
           (e-default-debug--debug-capabilities)))
  (e-harness-set-layer-change-function harness nil)
  (e-harness-notify-layers-changed harness)
  harness)

(defun e-default-chat-harness-spec ()
  "Return the effective default chat harness specification.

Startup and integration consumers use this semantic projection to resolve
the configured chat factory without depending on the default-spec registry's
private representation."
  (cl-find-if
   (lambda (spec)
     (eq (plist-get spec :id) :chat-default))
   (e-default-harness--effective-specs)))

(defun e-default-debug--custom-chat-spec-p ()
  "Return non-nil when the effective chat default uses a custom spec factory."
  (let ((factory (plist-get (e-default-chat-harness-spec) :factory)))
    (and factory
         (not (eq factory 'e-default-chat-harness-create)))))

(defun e-default-debug--chat-harness-from-custom-spec ()
  "Return the current chat default harness for a custom chat spec."
  (or (e-harness-registry-get :chat-default)
      (e-harness-registry-get-or-create :chat-default)))

(defun e-default-debug--context-strategy-from-chat (chat)
  "Return an independent debug context strategy derived from CHAT."
  (let ((strategy (e-harness-context-strategy chat)))
    (if (e-context-transcript-stack-p strategy)
        (e-context-transcript-stack-create)
      strategy)))

(defun e-default-debug-harness-sync (harness spec)
  "Reconcile cached default debug HARNESS from SPEC."
  (if (e-default-chat--sync-from-configured-factory-p spec)
      (e-default-harness-sync-from-factory harness spec)
    (progn
      (e-default-chat--mark-unconfigured harness)))
  (e-default-debug-sync-harness-layers harness)
  harness)

(cl-defun e-default-chat-harness-create
    (&key provider sessions layer-ids directory)
  "Create the default chat harness.
The backend is created by `e-default-chat-harness-factory'.  PROVIDER is
rejected because provider selection belongs in config.el/config.org.  SESSIONS
supplies an existing session store.  LAYER-IDS overrides
`e-default-chat-layer-ids'.  DIRECTORY sets the root used by config-aware
default layers.  When `e-default-chat-harness-factory' is nil, create a harness
with an explicit unconfigured backend and no provider."
  (when provider
    (user-error
     "Default chat provider is configured by e-default-chat-harness-factory, not :provider"))
  (require 'e-base)
  (require 'e-emacs-base)
  (require 'e-harness-base)
  (require 'e-process-reporting)
  (require 'e-layer-selection)
  (require 'e-layers)
  (require 'e-org-canvas-capabilities)
  (e-default-layers-register)
  (let* ((root (or directory default-directory))
         (store (or sessions (e-default-session-store)))
         (harness (if (functionp e-default-chat-harness-factory)
                      (funcall e-default-chat-harness-factory
                               :sessions store
                               :directory root)
                    (e-harness-create
                     :backend (e-default-chat--unconfigured-backend)
                     :sessions store))))
    (unless (e-harness-p harness)
      (signal 'wrong-type-argument (list 'e-harness-p harness)))
    (setf (e-harness-default-project-root harness)
          (e-harness-normalize-project-root root))
    (e-harness-set-intrinsic-capabilities
     harness
     (e-default-chat--chat-session-capabilities))
    (e-default-chat--set-configured-layer-ids harness layer-ids root)
    (e-harness-set-layer-change-function
     harness #'e-default-chat--record-layer-ids)
    harness))

(cl-defun e-default-debug-harness-create
    (&key provider sessions layer-ids directory)
  "Create the default standing debug harness.
The debug harness shares the default chat backend/session-store configuration
but keeps layer selection changes local to chat harnesses."
  (let ((harness (if (e-default-debug--custom-chat-spec-p)
                     (let ((chat (e-default-debug--chat-harness-from-custom-spec)))
                       (e-harness-create
                        :backend (e-harness-backend chat)
                        :context-strategy
                        (e-default-debug--context-strategy-from-chat chat)
                        :default-options
                        (copy-sequence (e-harness-default-options chat))
                        :capability-config
                        (copy-tree (e-harness-runtime-capability-config chat))
                        :sessions (or sessions (e-harness-sessions chat))
                        :enabled-layer-ids
                        (e-harness-enabled-layer-ids chat)
                        :intrinsic-capabilities
                        (append (e-default-chat--chat-session-capabilities)
                                (e-default-debug--debug-capabilities))
                        :project-root
                        (or directory
                            (e-harness-default-project-root chat))))
                   (e-default-chat-harness-create
                    :provider provider
                    :sessions sessions
                    :layer-ids layer-ids
                    :directory directory))))
    (e-harness-set-intrinsic-capabilities
     harness
     (append (e-default-chat--chat-session-capabilities)
             (e-default-debug--debug-capabilities)))
    (e-harness-set-layer-change-function harness nil)
    harness))

(defun e-default-harnesses-register (&optional specs)
  "Register default harness SPECS with `e-harness-registry'.
When SPECS is nil, register `e-default-harness-specs'.  Registration is lazy:
factories are recorded, but no harness is created."
  (dolist (spec (e-default-harness--effective-specs specs))
    (let* ((id (plist-get spec :id))
           (factory (plist-get spec :factory))
           (kind (plist-get spec :kind)))
      (e-harness-registry-register-factory id factory)
      (when kind
        (e-harness-instance-register
         :id id
         :name (plist-get spec :name)
         :kind kind
         :factory factory
         :harness-id id
         :metadata (plist-get spec :metadata)
         :default (plist-get spec :default)))))
  (e-default-harness--effective-specs specs))

(defun e-default-harnesses-sync-instances (&optional specs)
  "Reconcile cached default harness instances with current config.
Only existing instances are touched; lazy factories are left lazy."
  (let ((refreshed-stores (make-hash-table :test 'eq)))
    (dolist (spec (e-default-harness--effective-specs specs))
      (when-let ((harness (e-harness-registry-get (plist-get spec :id))))
        (funcall (or (plist-get spec :sync)
                     #'e-default-harness-sync-from-factory)
                 harness spec)
        (let ((store (e-harness-sessions harness)))
          (when (and (e-session-store-p store)
                     (not (gethash store refreshed-stores)))
            (puthash store t refreshed-stores)
            (e-session-refresh-index-metadata store))))))
  nil)

(defun e-default-harnesses-clear-instances (&optional specs)
  "Clear cached instances for default harness SPECS.
Factories remain registered so the next lookup recreates fresh harnesses."
  (dolist (spec (e-default-harness--effective-specs specs))
    (e-harness-registry-clear-instance (plist-get spec :id)))
  nil)

(defun e-default-harnesses-startup ()
  "Register default harness factories and reconcile live default instances."
  (e-default-harnesses-register)
  (e-default-harnesses-sync-instances))

(add-hook 'e-startup-layer-hook #'e-default-harnesses-startup)

(provide 'e-default-harnesses)

;;; e-default-harnesses.el ends here

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
(require 'e-default-layers)
(require 'e-context)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-layers)
(require 'e-session)
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
     :sync e-default-chat-harness-sync))
  "Default harness specs registered during package startup."
  :type '(repeat sexp)
  :group 'e-defaults)

(defcustom e-default-chat-layer-ids
  '(agents-std-context harness-base e os-base emacs-base web text-editing org-canvas project-local)
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

(defvar e-default--chat-sessions nil
  "Cached default persistent chat session store.")

(defun e-default-session-store ()
  "Return the default persistent session store."
  (let ((directory (file-name-as-directory
                    (expand-file-name e-session-directory))))
    (unless (and (e-session-store-p e-default--chat-sessions)
                 (equal (e-session-store-directory e-default--chat-sessions)
                        directory))
      (setq e-default--chat-sessions
            (e-session-persistent-index-store-create directory)))
    e-default--chat-sessions))

(defun e-default-chat--record-layer-ids (harness)
  "Record HARNESS active registered layer ids as default chat config."
  (setq e-default-chat-layer-ids
        (delq nil
              (mapcar (lambda (layer)
                        (let ((id (e-layer-id layer)))
                          (and (not (eq id 'chat-session))
                               (e-layer-get id)
                               id)))
                      (e-harness-active-layers harness))))
  e-default-chat-layer-ids)

(defun e-default-chat--activate-configured-layers
    (harness &optional layer-ids directory)
  "Activate configured stateless LAYER-IDS in HARNESS.
When LAYER-IDS is nil, use `e-default-chat-layer-ids'.
DIRECTORY is passed to config-aware layer factories."
  (e-default-layers-register)
  (dolist (layer-id (or layer-ids e-default-chat-layer-ids))
    (e-harness-activate-layer
     harness (e-layer-create-registered layer-id directory)))
  harness)

(defun e-default-chat--chat-session-layer ()
  "Return a fresh internal chat-session layer for default chat harnesses."
  (require 'e-chat-session)
  (e-layer-create
   :id 'chat-session
   :name "Chat Session"
   :capabilities (list (e-chat-session-capability-create))))

(defun e-default-chat-sync-harness-layers
    (harness &optional layer-ids directory)
  "Reconcile default chat HARNESS layers from `e-default-chat-layer-ids'.
The internal `chat-session' layer is recreated.  Other active layers are
recreated from registered stateless layer specs in the configured order.
DIRECTORY is passed to config-aware layer factories."
  (e-harness-refresh-default-context-strategy harness)
  (let ((change-function (e-harness-layer-change-function harness)))
    (unwind-protect
        (progn
          (e-harness-set-layer-change-function harness nil)
          (e-shell-clear-harness-shells harness)
          (setf (e-harness-active-layers harness) nil)
          (e-harness-activate-layer
           harness (e-default-chat--chat-session-layer))
          (e-default-chat--activate-configured-layers
           harness layer-ids directory))
      (e-harness-set-layer-change-function harness change-function)))
  harness)

(defun e-default-harness--repair-shifted-session-store (harness)
  "Repair HARNESS when a live reload shifted the session store slot."
  (when (and (not (e-session-store-p (e-harness-sessions harness)))
             (e-session-store-p
              (e-harness-runtime-capability-config harness)))
    (setf (e-harness-sessions harness)
          (e-harness-runtime-capability-config harness))
    (setf (e-harness-runtime-capability-config harness) nil))
  harness)

(defun e-default-harness-sync-from-factory (harness spec)
  "Refresh HARNESS generic runtime fields from SPEC's fresh factory result.

Live reload can replace adapter helper functions without recreating cached
default harnesses.  Refreshing the runtime updates stale backend closures while
preserving sessions, context state, and active presentation buffers that already
hold HARNESS."
  (e-default-harness--repair-shifted-session-store harness)
  (let ((fresh (funcall (plist-get spec :factory))))
    (unless (e-harness-p fresh)
      (signal 'wrong-type-argument (list 'e-harness-p fresh)))
    (setf (e-harness-backend harness)
          (e-harness-backend fresh))
    (setf (e-harness-default-options harness)
          (copy-sequence (e-harness-default-options fresh)))
    (setf (e-harness-default-project-root harness)
          (e-harness-default-project-root fresh))
    (setf (e-harness-runtime-capability-config harness)
          (copy-tree (e-harness-runtime-capability-config fresh)))
    (when (e-context-transcript-stack-p (e-harness-context-strategy harness))
      (setf (e-harness-context-strategy harness)
            (e-harness-context-strategy fresh))))
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
      (e-default-harness--repair-shifted-session-store harness)
      (e-default-chat--mark-unconfigured harness)))
  (e-default-chat-sync-harness-layers harness)
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
    (e-harness-activate-layer
     harness
     (e-default-chat--chat-session-layer))
    (e-default-chat--activate-configured-layers harness layer-ids root)
    (e-harness-set-layer-change-function
     harness #'e-default-chat--record-layer-ids)
    harness))

(defun e-default-harnesses-register (&optional specs)
  "Register default harness SPECS with `e-harness-registry'.
When SPECS is nil, register `e-default-harness-specs'.  Registration is lazy:
factories are recorded, but no harness is created."
  (dolist (spec (or specs e-default-harness-specs))
    (let* ((id (plist-get spec :id))
           (factory (plist-get spec :factory))
           (legacy-chat-default-p (eq id :chat-default))
           (kind (or (plist-get spec :kind)
                     (and legacy-chat-default-p 'chat))))
      (e-harness-registry-register-factory id factory)
      (when kind
        (e-harness-instance-register
         :id id
         :name (or (plist-get spec :name)
                   (and legacy-chat-default-p "Default Chat"))
         :kind kind
         :factory factory
         :harness-id id
         :metadata (plist-get spec :metadata)
         :default (or (plist-get spec :default)
                      legacy-chat-default-p)))))
  (or specs e-default-harness-specs))

(defun e-default-harnesses-sync-instances (&optional specs)
  "Reconcile cached default harness instances with current config.
Only existing instances are touched; lazy factories are left lazy."
  (dolist (spec (or specs e-default-harness-specs))
    (when-let ((harness (e-harness-registry-get (plist-get spec :id))))
      (funcall (or (plist-get spec :sync)
                   #'e-default-harness-sync-from-factory)
               harness spec)))
  nil)

(defun e-default-harnesses-clear-instances (&optional specs)
  "Clear cached instances for default harness SPECS.
Factories remain registered so the next lookup recreates fresh harnesses."
  (dolist (spec (or specs e-default-harness-specs))
    (e-harness-registry-clear-instance (plist-get spec :id)))
  nil)

(defun e-default-harnesses-startup ()
  "Register default harness factories and reconcile live default instances."
  (e-default-harnesses-register)
  (e-default-harnesses-sync-instances))

(add-hook 'e-startup-layer-hook #'e-default-harnesses-startup)

(provide 'e-default-harnesses)

;;; e-default-harnesses.el ends here

;;; e-default-harnesses.el --- Startup harness specs for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Startup registration for named default harnesses.  This module registers
;; lazy factories only; provider adapters and concrete layers are loaded by the
;; factory when the corresponding harness is requested.

;;; Code:

(require 'cl-lib)
(require 'e-default-layers)
(require 'e-harness-registry)
(require 'e-layers)
(require 'e-session)
(require 'e-startup)

(defgroup e-defaults nil
  "Default harness startup configuration for e."
  :group 'e
  :prefix "e-default-")

(defcustom e-default-harness-specs
  '((:id :chat-default :factory e-default-chat-harness-create))
  "Default harness specs registered during package startup."
  :type '(repeat sexp)
  :group 'e-defaults)

(defcustom e-default-chat-layer-ids
  '(agents-std-context e base emacs-base)
  "Layer ids activated by default chat harnesses.

This option is the source of truth for the stateless layer preset attached to
the default chat harness.  Runtime layer enable/disable commands update this
option when they operate on that harness, and existing default harness instances
are reconciled from it during startup/reload."
  :type '(repeat symbol)
  :group 'e-defaults)

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

(defun e-default-chat--activate-configured-layers (harness &optional layer-ids)
  "Activate configured stateless LAYER-IDS in HARNESS.
When LAYER-IDS is nil, use `e-default-chat-layer-ids'."
  (e-default-layers-register)
  (dolist (layer-id (or layer-ids e-default-chat-layer-ids))
    (e-harness-activate-layer harness (e-layer-create-registered layer-id)))
  harness)

(defun e-default-chat-sync-harness-layers (harness &optional layer-ids)
  "Reconcile default chat HARNESS layers from `e-default-chat-layer-ids'.
The internal `chat-session' layer is preserved.  Other active layers are
recreated from registered stateless layer specs in the configured order."
  (let ((chat-session (e-harness-active-layer harness 'chat-session))
        (change-function (e-harness-layer-change-function harness)))
    (unwind-protect
        (progn
          (e-harness-set-layer-change-function harness nil)
          (setf (e-harness-active-layers harness)
                (delq nil (list chat-session)))
          (e-default-chat--activate-configured-layers harness layer-ids))
      (e-harness-set-layer-change-function harness change-function)))
  harness)

(cl-defun e-default-chat-harness-create (&key provider sessions layer-ids)
  "Create the default chat harness.
PROVIDER selects the OpenAI-compatible provider.  SESSIONS supplies an existing
session store.  LAYER-IDS overrides `e-default-chat-layer-ids'."
  (require 'e-base)
  (require 'e-chat-session)
  (require 'e-emacs-base)
  (require 'e-harness)
  (require 'e-layer-selection)
  (require 'e-layers)
  (require 'e-openai)
  (e-default-layers-register)
  (let ((harness (e-openai-create-harness
                  :provider (or provider e-openai-default-provider)
                  :sessions (or sessions (e-default-session-store)))))
    (e-harness-activate-layer
     harness
     (e-layer-create
      :id 'chat-session
      :name "Chat Session"
      :capabilities (list (e-chat-session-capability-create))))
    (e-default-chat--activate-configured-layers harness layer-ids)
    (e-harness-set-layer-change-function
     harness #'e-default-chat--record-layer-ids)
    harness))

(defun e-default-harnesses-register (&optional specs)
  "Register default harness SPECS with `e-harness-registry'.
When SPECS is nil, register `e-default-harness-specs'.  Registration is lazy:
factories are recorded, but no harness is created."
  (dolist (spec (or specs e-default-harness-specs))
    (e-harness-registry-register-factory
     (plist-get spec :id)
     (plist-get spec :factory)))
  (or specs e-default-harness-specs))

(defun e-default-harnesses-sync-instances (&optional specs)
  "Reconcile cached default harness instances with current config.
Only existing instances are touched; lazy factories are left lazy."
  (dolist (spec (or specs e-default-harness-specs))
    (when (eq (plist-get spec :id) :chat-default)
      (when-let ((harness (e-harness-registry-get :chat-default)))
        (e-default-chat-sync-harness-layers harness))))
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

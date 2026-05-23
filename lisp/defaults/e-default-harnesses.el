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
(require 'e-harness-registry)
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

(defcustom e-default-chat-layer-functions
  '(e-layer-selection-layer-create e-base-layer-create e-emacs-base-layer-create)
  "Layer factory functions activated by default chat harnesses."
  :type '(repeat function)
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
            (e-session-persistent-store-create directory)))
    e-default--chat-sessions))

(cl-defun e-default-chat-harness-create (&key provider sessions layer-functions)
  "Create the default chat harness.
PROVIDER selects the OpenAI-compatible provider.  SESSIONS supplies an existing
session store.  LAYER-FUNCTIONS overrides `e-default-chat-layer-functions'."
  (require 'e-base)
  (require 'e-chat-session)
  (require 'e-emacs-base)
  (require 'e-harness)
  (require 'e-layer-selection)
  (require 'e-layers)
  (require 'e-openai)
  (let ((harness (e-openai-create-harness
                  :provider (or provider e-openai-default-provider)
                  :sessions (or sessions (e-default-session-store)))))
    (e-harness-activate-layer
     harness
     (e-layer-create
      :id 'chat-session
      :name "Chat Session"
      :capabilities (list (e-chat-session-capability-create))))
    (dolist (create-layer (or layer-functions e-default-chat-layer-functions))
      (e-harness-activate-layer harness (funcall create-layer)))
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

(defun e-default-harnesses-clear-instances (&optional specs)
  "Clear cached instances for default harness SPECS.
Factories remain registered so the next lookup recreates fresh harnesses."
  (dolist (spec (or specs e-default-harness-specs))
    (e-harness-registry-clear-instance (plist-get spec :id)))
  nil)

(defun e-default-harnesses-startup ()
  "Register default harness factories for package startup."
  (e-default-harnesses-clear-instances)
  (e-default-harnesses-register))

(add-hook 'e-startup-layer-hook #'e-default-harnesses-startup)

(provide 'e-default-harnesses)

;;; e-default-harnesses.el ends here

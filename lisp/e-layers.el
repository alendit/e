;;; e-layers.el --- Harness-owned layer bundles for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Layers bundle tools, instructions, prompts, skills, and context providers.
;; The harness activates layers; presentation shells only choose how to expose
;; that active runtime state to users.

;;; Code:

(require 'cl-lib)
(require 'e-context)

(cl-defstruct (e-layer (:constructor e-layer-create))
  id
  name
  capabilities
  defaults
  instructions
  tools
  context-providers
  skills
  prompts)

(defun e-layers-register-tools (layer registry)
  "Register LAYER tools in REGISTRY."
  (dolist (register (e-layer-tools layer))
    (funcall register registry)))

(cl-defun e-layers-context-messages (layers &key harness session-id turn-id)
  "Return backend-neutral context messages contributed by LAYERS.
HARNESS, SESSION-ID, and TURN-ID are passed to context providers."
  (let ((messages nil))
    (dolist (layer layers)
      (when-let ((instructions (e-layer-instructions layer)))
        (push (list :role 'system
                    :content instructions)
              messages))
      (dolist (provider (e-layer-context-providers layer))
        (setq messages
              (append (reverse (e-context-provider-build
                                provider
                                :harness harness
                                :session-id session-id
                                :turn-id turn-id))
                      messages))))
    (nreverse messages)))

(provide 'e-layers)

;;; e-layers.el ends here

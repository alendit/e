;;; e-capabilities.el --- Capability contribution contracts for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capabilities are semantic behavior units.  They contribute instructions,
;; model-facing tools, context providers, and shell-facing actions while layers
;; remain packaging presets over those capabilities.

;;; Code:

(require 'cl-lib)
(require 'e-context)

(cl-defstruct (e-capability (:constructor e-capability-create))
  id
  name
  instructions
  tools
  context-providers
  actions)

(defun e-capabilities-register-tools (capability registry)
  "Register CAPABILITY tool providers in REGISTRY."
  (dolist (register (e-capability-tools capability))
    (funcall register registry)))

(cl-defun e-capabilities--provider-messages
    (provider &key harness session-id turn-id)
  "Return context messages from PROVIDER for the current turn."
  (cond
   ((e-context-provider-p provider)
    (e-context-provider-build
     provider
     :harness harness
     :session-id session-id
     :turn-id turn-id))
   ((functionp provider)
    (funcall provider
             :harness harness
             :session-id session-id
             :turn-id turn-id))
   (t
    (signal 'wrong-type-argument (list 'functionp provider)))))

(cl-defun e-capabilities-context-messages
    (capabilities &key harness session-id turn-id)
  "Return backend-neutral context messages contributed by CAPABILITIES.
HARNESS, SESSION-ID, and TURN-ID are passed to context providers."
  (let ((messages nil))
    (dolist (capability capabilities)
      (when (e-capability-instructions capability)
        (push (list :role 'system
                    :content (e-capability-instructions capability))
              messages))
      (dolist (provider (e-capability-context-providers capability))
        (setq messages
              (append
               (reverse
                (e-capabilities--provider-messages
                 provider
                 :harness harness
                 :session-id session-id
                 :turn-id turn-id))
               messages))))
    (nreverse messages)))

(defun e-capabilities-action (capability action)
  "Return CAPABILITY function for ACTION."
  (plist-get (e-capability-actions capability) action))

(provide 'e-capabilities)

;;; e-capabilities.el ends here

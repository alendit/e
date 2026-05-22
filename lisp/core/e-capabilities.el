;;; e-capabilities.el --- Capability contribution contracts for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capabilities are semantic behavior bundles.  They contribute instructions,
;; model-facing tools, resource methods, context providers, and shell-facing
;; actions while layers remain packaging presets over those capabilities.

;;; Code:

(require 'cl-lib)
(require 'e-context)
(require 'e-resources)

(cl-defstruct (e-capability
               (:constructor e-capability--create
                             (&key id name instructions tools
                                   resource-methods context-providers actions)))
  id
  name
  instructions
  tools
  resource-methods
  context-providers
  actions)

(defun e-capability-create (&rest args)
  "Create an e capability from keyword ARGS."
  (if (keywordp (car args))
      (apply #'e-capability--create args)
    (pcase-let ((`(,id ,name ,instructions ,tools ,context-providers ,actions)
                 args))
      (e-capability--create
       :id id
       :name name
       :instructions instructions
       :tools tools
       :context-providers context-providers
       :actions actions))))

(put 'e-capability-create 'compiler-macro nil)

(defun e-capability-resource-methods (capability)
  "Return CAPABILITY resource method providers.
This accessor tolerates stale capability records compiled before the
`resource-methods' slot existed."
  (if (>= (length capability) 8)
      (aref capability 5)
    nil))

(defun e-capability-context-providers (capability)
  "Return CAPABILITY context providers.
This accessor tolerates stale capability records compiled before the
`resource-methods' slot existed."
  (if (>= (length capability) 8)
      (aref capability 6)
    (aref capability 5)))

(defun e-capability-actions (capability)
  "Return CAPABILITY shell actions.
This accessor tolerates stale capability records compiled before the
`resource-methods' slot existed."
  (if (>= (length capability) 8)
      (aref capability 7)
    (aref capability 6)))

(dolist (symbol '(e-capability-resource-methods
                  e-capability-context-providers
                  e-capability-actions))
  (put symbol 'compiler-macro nil)
  (put symbol 'side-effect-free nil)
  (put symbol 'gv-expander nil))

(defun e-capabilities-register-tools (capability registry)
  "Register CAPABILITY tool providers in REGISTRY."
  (dolist (register (e-capability-tools capability))
    (funcall register registry)))

(defun e-capabilities-register-resource-methods (capability registry)
  "Register CAPABILITY resource method providers in REGISTRY."
  (dolist (register (e-capability-resource-methods capability))
    (e-resources-register registry register)))

(cl-defun e-capabilities--provider-messages
    (provider &key harness session-id turn-id)
  "Return context messages from PROVIDER for the current turn.
HARNESS, SESSION-ID, and TURN-ID identify the active turn."
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

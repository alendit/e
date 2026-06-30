;;; e-capabilities.el --- Capability contribution contracts for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Capabilities are semantic behavior bundles.  They contribute instructions,
;; model-facing tools, resource methods, in-memory resources, context providers,
;; prompts, and shell-facing actions while layers remain packaging presets over
;; those capabilities.

;;; Code:

(require 'cl-lib)
(require 'e-context)
(require 'e-hooks)
(require 'e-resources)
(require 'e-store)

(cl-defstruct (e-capability
               (:constructor e-capability--create
                             (&key id name instructions tools
                                   resource-methods resources
                                   context-providers actions hooks
                                   instruction-priority config-options config
                                   prompts))
               (:conc-name e-capability--))
  id
  name
  instructions
  tools
  resource-methods
  resources
  context-providers
  actions
  hooks
  (instruction-priority 200)
  config-options
  config
  prompts)

(cl-defstruct (e-action
               (:constructor e-action-create
                             (&key handler caller description parameters
                                   requires-session start tool-metadata)))
  handler
  caller
  description
  parameters
  requires-session
  start
  ;; Kept for live reload compatibility with earlier action descriptors.
  tool-metadata)

(defun e-capability-id (capability)
  "Return CAPABILITY id."
  (e-capability--id capability))

(defun e-capability-name (capability)
  "Return CAPABILITY display name."
  (e-capability--name capability))

(defun e-capability-instructions (capability)
  "Return CAPABILITY instructions."
  (e-capability--instructions capability))

(defun e-capability-tools (capability)
  "Return CAPABILITY tool providers."
  (e-capability--tools capability))

(cl-defstruct (e-capability-resource-method-provider
               (:constructor e-capability-resource-method-provider-create))
  handler)

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
      (e-capability--resource-methods capability)
    nil))

(defun e-capability-context-providers (capability)
  "Return CAPABILITY context providers.
This accessor tolerates stale capability records compiled before the
`resources' or `resource-methods' slot existed."
  (if (>= (length capability) 9)
      (e-capability--context-providers capability)
    (if (>= (length capability) 8)
        (aref capability 6)
      (aref capability 5))))

(defun e-capability-resources (capability)
  "Return CAPABILITY in-memory resource providers.
This accessor tolerates stale capability records compiled before the
`resources' slot existed."
  (if (>= (length capability) 9)
      (e-capability--resources capability)
    nil))

(defun e-capability-actions (capability)
  "Return CAPABILITY shell actions.
This accessor tolerates stale capability records compiled before the `resources'
or `resource-methods' slot existed."
  (if (>= (length capability) 9)
      (e-capability--actions capability)
    (if (>= (length capability) 8)
        (aref capability 7)
      (aref capability 6))))

(defun e-capability-hooks (capability)
  "Return CAPABILITY lifecycle hooks.
This accessor tolerates stale capability records compiled before the `hooks'
slot existed."
  (if (>= (length capability) 10)
      (e-capability--hooks capability)
    nil))

(defun e-capability-instruction-priority (capability)
  "Return CAPABILITY instruction priority."
  (unless (e-capability-p capability)
    (signal 'wrong-type-argument (list 'e-capability-p capability)))
  (or (e-capability--instruction-priority capability) 200))

(defun e-capability-config-options (capability)
  "Return CAPABILITY declared config option specs.
This accessor tolerates stale capability records compiled before the
`config-options' slot existed."
  (if (>= (length capability) 12)
      (e-capability--config-options capability)
    nil))

(defun e-capability-config (capability)
  "Return CAPABILITY effective config metadata.
This accessor tolerates stale capability records compiled before the `config'
slot existed."
  (if (>= (length capability) 13)
      (e-capability--config capability)
    nil))

(defun e-capability-prompts (capability)
  "Return CAPABILITY prompt specs.
This accessor tolerates stale capability records compiled before the `prompts'
slot existed."
  (if (>= (length capability) 14)
      (e-capability--prompts capability)
    nil))

(dolist (symbol '(e-capability-id
                  e-capability-name
                  e-capability-instructions
                  e-capability-tools
                  e-capability-resource-methods
                  e-capability-resources
                  e-capability-context-providers
                  e-capability-actions
                  e-capability-hooks
                  e-capability-instruction-priority
                  e-capability-config-options
                  e-capability-config
                  e-capability-prompts))
  (put symbol 'compiler-macro nil)
  (put symbol 'side-effect-free nil)
  (put symbol 'gv-expander nil))

(defun e-capabilities--tool-provider-accepts-context-p (provider)
  "Return non-nil when PROVIDER accepts registration context."
  (condition-case nil
      (let ((max-arity (cdr (func-arity provider))))
        (or (eq max-arity 'many)
            (> max-arity 1)))
    (error nil)))

(defun e-capabilities-register-tools (capability registry &rest context)
  "Register CAPABILITY tool providers in REGISTRY.
CONTEXT is passed only to providers that accept more than REGISTRY."
  (dolist (register (e-capability-tools capability))
    (unless (functionp register)
      (signal 'wrong-type-argument (list 'functionp register)))
    (if (e-capabilities--tool-provider-accepts-context-p register)
        (apply register registry context)
      (funcall register registry))))

(defun e-capabilities-register-resource-methods
    (capability registry &rest context)
  "Register CAPABILITY resource method providers in REGISTRY.
CONTEXT is passed only to context-aware resource method providers."
  (dolist (register (e-capability-resource-methods capability))
    (cond
     ((e-capability-resource-method-provider-p register)
      (apply (e-capability-resource-method-provider-handler register)
             registry
             context))
     (t
      (e-resources-register registry register)))))

(defun e-capabilities--resource-provider-accepts-context-p (provider)
  "Return non-nil when PROVIDER accepts registration context."
  (condition-case nil
      (let ((max-arity (cdr (func-arity provider))))
        (or (eq max-arity 'many)
            (> max-arity 2)))
    (error nil)))

(defun e-capabilities-register-resources (capability store &rest context)
  "Register CAPABILITY in-memory resource providers in STORE.
CONTEXT is passed only to providers that accept more than STORE and
CAPABILITY."
  (dolist (register (e-capability-resources capability))
    (unless (functionp register)
      (signal 'wrong-type-argument (list 'functionp register)))
    (if (e-capabilities--resource-provider-accepts-context-p register)
        (apply register store capability context)
      (funcall register store capability))))

(defun e-capabilities-register-hooks (capability registry)
  "Register CAPABILITY lifecycle hooks in REGISTRY."
  (e-hooks-register-list registry (e-capability-hooks capability)))

(cl-defun e-capabilities--provider-messages
    (provider &key harness session-id turn-id context-purpose)
  "Return context messages from PROVIDER for the current turn.
HARNESS, SESSION-ID, and TURN-ID identify the active turn.
CONTEXT-PURPOSE may be `turn' for model-facing requests, `preview' for
explicit user-requested context inspection, or an optional snapshot purpose for
status-like callers."
  (cond
   ((e-context-provider-p provider)
    (e-context-provider-build
     provider
     :harness harness
     :session-id session-id
     :turn-id turn-id
     :context-purpose context-purpose))
   ((functionp provider)
    (funcall provider
             :harness harness
             :session-id session-id
             :turn-id turn-id))
   (t
    (signal 'wrong-type-argument (list 'functionp provider)))))

(defun e-capabilities--provider-priority (provider)
  "Return PROVIDER context priority."
  (if (e-context-provider-p provider)
      (e-context-provider-priority provider)
    200))

(defun e-capabilities--provider-cache-placement (provider)
  "Return PROVIDER cache-placement rank."
  (if (e-context-provider-p provider)
      (e-context-cache-placement-rank
       (e-context-provider-cache-placement provider))
    (e-context-cache-placement-rank 'stable-context)))

(defun e-capabilities--provider-segment-kind (provider)
  "Return backend-neutral segment kind for PROVIDER."
  (pcase (if (e-context-provider-p provider)
             (e-context-provider-cache-placement provider)
           'stable-context)
    ('static-prefix 'static-prefix)
    ('stable-context 'stable-context)
    ('dynamic-context 'current-state)))

(defun e-capabilities--fragment-less-p (left right)
  "Return non-nil when LEFT context fragment sorts before RIGHT."
  (let ((left-key (list (plist-get left :cache-placement)
                        (plist-get left :priority)
                        (plist-get left :capability-index)
                        (or (plist-get left :provider-index) -1)
                        (plist-get left :message-index)))
        (right-key (list (plist-get right :cache-placement)
                         (plist-get right :priority)
                         (plist-get right :capability-index)
                         (or (plist-get right :provider-index) -1)
                         (plist-get right :message-index))))
    (cl-loop for left-item in left-key
             for right-item in right-key
             thereis (< left-item right-item)
             until (/= left-item right-item))))

(cl-defun e-capabilities--context-fragments
    (capabilities &key harness session-id turn-id context-purpose)
  "Return sorted context fragments contributed by CAPABILITIES.
HARNESS, SESSION-ID, TURN-ID, and CONTEXT-PURPOSE are passed to context
providers."
  (let ((fragments nil)
        (capability-index 0))
    (dolist (capability capabilities)
      (when (e-capability-instructions capability)
        (push (list :cache-placement
                    (e-context-cache-placement-rank 'static-prefix)
                    :priority (e-capability-instruction-priority capability)
                    :capability-index capability-index
                    :message-index 0
                    :segment-kind 'static-prefix
                    :segment-id (list (e-capability-id capability)
                                      'instructions)
                    :message (list :role 'system
                                   :content
                                   (e-capability-instructions capability)))
              fragments))
      (let ((provider-index 0))
        (dolist (provider (e-capability-context-providers capability))
          (let ((message-index 0))
            (dolist (message (e-capabilities--provider-messages
                              provider
                              :harness harness
                              :session-id session-id
                              :turn-id turn-id
                              :context-purpose context-purpose))
              (push (list :cache-placement
                          (e-capabilities--provider-cache-placement provider)
                          :priority (e-capabilities--provider-priority provider)
                          :capability-index capability-index
                          :provider-index provider-index
                          :message-index message-index
                          :segment-kind
                          (e-capabilities--provider-segment-kind provider)
                          :segment-id
                          (list (e-capability-id capability)
                                (if (e-context-provider-p provider)
                                    (e-context-provider-name provider)
                                  provider-index)
                                message-index)
                          :message message)
                    fragments)
              (setq message-index (1+ message-index))))
          (setq provider-index (1+ provider-index))))
      (setq capability-index (1+ capability-index)))
    (sort fragments #'e-capabilities--fragment-less-p)))

(defun e-capabilities--fragment-segment (fragment)
  "Return backend-neutral context segment for FRAGMENT."
  (e-context-segment-create
   :kind (plist-get fragment :segment-kind)
   :id (plist-get fragment :segment-id)
   :messages (list (plist-get fragment :message))))

(cl-defun e-capabilities-context
    (capabilities &key harness session-id turn-id context-purpose)
  "Return context messages and segment metadata from CAPABILITIES.
HARNESS, SESSION-ID, TURN-ID, and CONTEXT-PURPOSE are passed to context
providers."
  (let ((fragments (e-capabilities--context-fragments
                    capabilities
                    :harness harness
                    :session-id session-id
                    :turn-id turn-id
                    :context-purpose context-purpose)))
    (list :messages
          (mapcar (lambda (fragment) (plist-get fragment :message))
                  fragments)
          :segments
          (mapcar #'e-capabilities--fragment-segment fragments))))

(cl-defun e-capabilities-context-messages
    (capabilities &key harness session-id turn-id context-purpose)
  "Return backend-neutral context messages contributed by CAPABILITIES.
HARNESS, SESSION-ID, TURN-ID, and CONTEXT-PURPOSE are passed to context
providers."
  (plist-get (e-capabilities-context
              capabilities
              :harness harness
              :session-id session-id
              :turn-id turn-id
              :context-purpose context-purpose)
             :messages))

(defun e-capabilities-action (capability action)
  "Return CAPABILITY function for ACTION.
When the action entry is an `e-action' descriptor, return its handler to
preserve the historical direct lookup contract."
  (let ((entry (plist-get (e-capability-actions capability) action)))
    (if (e-action-p entry)
        (e-action-handler entry)
      entry)))

(defun e-capabilities-action-spec (capability action)
  "Return CAPABILITY action descriptor or raw function for ACTION."
  (plist-get (e-capability-actions capability) action))

(provide 'e-capabilities)

;;; e-capabilities.el ends here

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
(require 'e-json)
(require 'e-message-details)
(require 'e-resources)
(require 'e-store)
(require 'e-structured-blocks)
(require 'e-work)
(require 'subr-x)

(cl-defstruct (e-capability
               (:constructor e-capability--create
                             (&key id name instructions tools
                                   resource-methods resources
                                   context-providers actions hooks
                                   instruction-priority config-options config
                                   prompts structured-blocks message-details
                                   action-capability-providers readiness))
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
  prompts
  structured-blocks
  message-details
  action-capability-providers
  readiness)

(cl-defstruct (e-action
               (:constructor e-action--create
                             (&key description parameters requires-session
                                   tool-metadata work)))
  description
  parameters
  requires-session
  tool-metadata
  work)

(defconst e-action-empty-parameters
  '(:type "object"
    :properties nil
    :required []
    :additionalProperties :json-false)
  "Strict canonical schema for an action that takes no arguments.")

(defun e-capabilities--copy-schema (schema)
  "Return a detached canonical copy of action SCHEMA.
SCHEMA has already passed the shared canonical schema validator when this
helper is called.  `copy-tree' with VECP non-nil preserves nested vectors while
leaving the canonical keyword/scalar representation unchanged."
  (copy-tree schema t))

(cl-defun e-action-create
    (&key description parameters requires-session tool-metadata work)
  "Create a work-backed capability action descriptor.
Actions must execute through `e-work-start'.  Cheap immediate actions should use
`e-action-cheap-create', which still creates a `:cheap' work spec."
  (unless (e-work-spec-p work)
    (signal 'wrong-type-argument (list 'e-work-spec-p work)))
  (let ((schema (or parameters
                    (e-work-spec-parameters work)
                    e-action-empty-parameters)))
    (condition-case error-data
        (progn
          (e-json-schema-assert-schema schema)
          (e-action--create
           :description description
           :parameters (e-capabilities--copy-schema schema)
           :requires-session requires-session
           :tool-metadata tool-metadata
           :work work))
      (e-json-error
       (signal 'wrong-type-argument
               (list 'canonical-action-schema
                     (error-message-string error-data)))))))

(cl-defun e-action-cheap-create
    (&key id description parameters requires-session tool-metadata owner runner)
  "Create a cheap work-backed action descriptor.
RUNNER is called as (RUNNER ARGUMENTS CONTEXT)."
  (unless (functionp runner)
    (signal 'wrong-type-argument (list 'functionp runner)))
  (e-action-create
   :description description
   :parameters parameters
   :requires-session requires-session
   :tool-metadata tool-metadata
   :work (e-work-spec-create
          :id id
          :description description
          :parameters parameters
          :execution 'cheap
          :interactive-policy 'cheap
          :owner (or owner 'action)
          :runner runner)))

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
  "Create an e capability from keyword or legacy positional ARGS.
The instructions value must be nil or a string of model-facing prose."
  (let ((instructions (if (keywordp (car args))
                          (plist-get args :instructions)
                        (nth 2 args))))
    (unless (or (null instructions) (stringp instructions))
      (signal 'wrong-type-argument (list 'stringp instructions)))
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
         :actions actions)))))

(put 'e-capability-create 'compiler-macro nil)
(put 'e-action-create 'compiler-macro nil)

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

(defun e-capability-structured-blocks (capability)
  "Return CAPABILITY structured-block specs.
This accessor tolerates stale capability records compiled before the
`structured-blocks' slot existed."
  (if (>= (length capability) 15)
      (e-capability--structured-blocks capability)
    nil))

(defun e-capability-message-details (capability)
  "Return CAPABILITY message-detail providers.
This accessor tolerates stale capability records compiled before the
`message-details' slot existed."
  (if (>= (length capability) 16)
      (e-capability--message-details capability)
    nil))

(defun e-capability-action-capability-providers (capability)
  "Return CAPABILITY dynamic action-capability providers.
This accessor tolerates stale capability records compiled before the
`action-capability-providers' slot existed."
  (if (>= (length capability) 17)
      (e-capability--action-capability-providers capability)
    nil))

(defun e-capability-readiness (capability)
  "Return CAPABILITY asynchronous readiness providers.
This accessor tolerates stale capability records compiled before the
`readiness' slot existed."
  (if (>= (length capability) 18)
      (e-capability--readiness capability)
    nil))

(defun e-capabilities-start-readiness (capabilities &rest context)
  "Start readiness providers from CAPABILITIES and return their work handles.
CONTEXT is passed as keyword arguments to every provider.  Providers return
nil when already ready or one non-blocking `e-work' handle otherwise."
  (let (works)
    (condition-case error
        (progn
          (dolist (capability capabilities)
            (dolist (provider (e-capability-readiness capability))
              (unless (functionp provider)
                (signal 'wrong-type-argument (list 'functionp provider)))
              (when-let* ((work (apply provider context)))
                (unless (e-work-handle-p work)
                  (signal 'wrong-type-argument (list 'e-work-handle-p work)))
                (push work works))))
          (nreverse works))
      (error
       (dolist (work works)
         (unless (memq (plist-get (e-work-status work) :state)
                       '(finished failed cancelled))
           (e-work-cancel work)))
       (signal (car error) (cdr error))))))

(defun e-capabilities-provided-action-capabilities
    (capabilities &rest context)
  "Return action capabilities dynamically provided by CAPABILITIES.
CONTEXT is passed as keyword arguments to every provider."
  (let (provided)
    (dolist (capability capabilities)
      (dolist (provider
               (e-capability-action-capability-providers capability))
        (unless (functionp provider)
          (signal 'wrong-type-argument (list 'functionp provider)))
        (let ((children (apply provider context)))
          (unless (listp children)
            (signal 'wrong-type-argument (list 'listp children)))
          (dolist (child children)
            (unless (e-capability-p child)
              (signal 'wrong-type-argument (list 'e-capability-p child)))
            (push child provided)))))
    (nreverse provided)))

(defconst e-capabilities-system-guidance-default-capability-index 100000
  "Synthetic capability index for system-guidance hook fragments.
Hook fragments sort by cache placement and priority first.  This large fallback
index keeps them after ordinary capability instruction fragments at the same
priority unless the hook author supplies a more specific ordering later.")

(cl-defun e-capabilities-system-guidance-fragment-create
    (&key id owner content (priority 200) (cache-placement 'static-prefix)
          (message-index 0))
  "Create a backend-neutral system guidance fragment.
ID is a stable fragment id owned by OWNER.  CONTENT is system-message prose.
PRIORITY and CACHE-PLACEMENT use the same ordering contract as capability
instructions and context providers.  The returned plist is suitable for the
`:system-guidance' capability hook point, which reduces over context fragments
before backend serialization."
  (unless id
    (user-error "System guidance fragment requires :id"))
  (unless owner
    (user-error "System guidance fragment requires :owner"))
  (unless (and (stringp content) (not (string-empty-p content)))
    (signal 'wrong-type-argument (list 'stringp content)))
  (let ((rank (e-context-cache-placement-rank cache-placement)))
    (list :cache-placement rank
          :priority priority
          :capability-index e-capabilities-system-guidance-default-capability-index
          :provider-index 0
          :message-index message-index
          :segment-kind (pcase cache-placement
                          ('static-prefix 'static-prefix)
                          ('stable-context 'stable-context)
                          ('dynamic-context 'current-state))
          :segment-id (list owner 'system-guidance id)
          :message (list :role 'system :content content)
          :owner owner
          :guidance-id id
          :system-guidance t)))

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
                  e-capability-prompts
                  e-capability-structured-blocks
                  e-capability-message-details
                  e-capability-readiness))
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

(defun e-capabilities-register-structured-blocks (capability registry)
  "Register CAPABILITY structured-block specs in REGISTRY."
  (dolist (spec (e-capability-structured-blocks capability))
    (e-structured-blocks-register registry spec)))

(defun e-capabilities-message-details (capability message context)
  "Return CAPABILITY's generic presentation details for MESSAGE and CONTEXT."
  (e-message-details-collect
   (e-capability-message-details capability) message context))

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

(defun e-capabilities--hooks-registry (capabilities)
  "Return a hook registry built from CAPABILITIES."
  (let ((registry (e-hooks-registry-create)))
    (dolist (capability capabilities)
      (e-capabilities-register-hooks capability registry))
    registry))

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
              (let ((sources (plist-get
                              message
                              e-context-evidence-sources-key))
                    (backend-message (copy-sequence message)))
                (cl-remf backend-message e-context-evidence-sources-key)
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
                            :sources (copy-tree sources)
                            :message backend-message)
                      fragments))
              (setq message-index (1+ message-index))))
          (setq provider-index (1+ provider-index))))
      (setq capability-index (1+ capability-index)))
    (setq fragments
          (e-hooks-run-reduce
           (e-capabilities--hooks-registry capabilities)
           :system-guidance
           fragments
           (list :harness harness
                 :session-id session-id
                 :turn-id turn-id
                 :context-purpose context-purpose
                 :capabilities capabilities)))
    (sort fragments #'e-capabilities--fragment-less-p)))

(defun e-capabilities--fragment-segment (fragment)
  "Return backend-neutral context segment for FRAGMENT."
  (let ((segment
         (e-context-segment-create
          :kind (plist-get fragment :segment-kind)
          :id (plist-get fragment :segment-id)
          :messages (list (plist-get fragment :message)))))
    (when-let ((sources (plist-get fragment :sources)))
      (plist-put segment e-context-evidence-sources-key sources))
    segment))

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

(defun e-capabilities-action-spec (capability action)
  "Return CAPABILITY action descriptor for ACTION."
  (plist-get (e-capability-actions capability) action))

(provide 'e-capabilities)

;;; e-capabilities.el ends here

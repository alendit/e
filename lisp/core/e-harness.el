;;; e-harness.el --- Core harness service for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public pure core harness service.

;;; Code:

(require 'cl-lib)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-compaction)
(require 'e-context)
(require 'e-context-budget)
(require 'e-context-lifetime)
(require 'e-events)
(require 'e-hooks)
(require 'e-layers)
(require 'e-message-details)
(require 'e-loop)
(require 'e-operations)
(require 'e-prompts)
(require 'e-request)
(require 'e-resources)
(require 'e-session)
(require 'e-shells)
(require 'e-store)
(require 'e-structured-blocks)
(require 'e-telemetry)
(require 'e-tools)
(require 'e-work)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-harness-no-active-turn "No active turn")
(define-error 'e-harness-active-turn-exists
  "Session already has an active turn")
(define-error 'e-harness-board-attachment-required
  "Live harness execution requires a current board attachment")
(define-error 'e-harness-duplicate-action-capability
  "Duplicate action capability id")

(defvar e-harness--attached-port-authorizer nil
  "Adapter validating private live-port attachment tokens.")

(defvar e-harness--attached-follow-up-publisher nil
  "Adapter publishing settlement follow-ups through the owning interaction bus.")

(defun e-harness--require-attached-port (harness session-id token)
  "Require TOKEN to authorize HARNESS SESSION-ID's private live port."
  (unless (and token e-harness--attached-port-authorizer
               (funcall e-harness--attached-port-authorizer
                        harness session-id token))
    (signal 'e-harness-board-attachment-required (list session-id)))
  token)

(defgroup e-harness nil
  "Core harness service for e."
  :group 'e)

(defcustom e-harness-auto-compaction-enabled t
  "When non-nil, auto-compact before a turn that would near the context window."
  :type 'boolean
  :group 'e-harness)

(defcustom e-harness-auto-compaction-reserve-tokens 16384
  "Tokens to reserve below the model context window before auto-compacting.
Auto-compaction triggers when estimated context exceeds WINDOW minus this."
  :type 'integer
  :group 'e-harness)

(cl-defstruct (e-harness (:constructor e-harness--make))
  backend
  context-strategy
  default-options
  default-project-root
  runtime-capability-config
  (sessions (e-session-store-create))
  (enabled-layer-ids nil)
   (intrinsic-capabilities nil)
   (subscribers nil)
   active-turns
   prompt-queues
   prompt-queue-counts
   provider-compaction-candidates
   (queued-input-count 0)
   (unsettled-generation 0)
   unsettled-change-function
   work-enrollment-function
   board-aggregation-function)

(defvar e-harness--layer-change-functions (make-hash-table :test 'eq :weakness 'key)
  "Layer change callbacks keyed by harness.")

(defvar e-harness--effective-capability-config-caches
  (make-hash-table :test 'eq :weakness 'key)
  "Weakly-owned effective capability configuration caches by harness.")

(defvar e-harness--pending-runtime-refreshes
  (make-hash-table :test 'eq :weakness 'key)
  "Newest deferred runtime refresh request for each busy harness.")

(defvar-local e-current-harness nil
  "Harness currently owned by the active presentation buffer, when any.")

(defun e-harness--clear-derived-accessor-metadata ()
  "Clear stale struct accessor metadata for derived harness views."
  (dolist (symbol '(e-harness-active-capabilities
                    e-harness-store
                    e-harness-resources
                    e-harness-tools))
    (put symbol 'compiler-macro nil)
    (put symbol 'side-effect-free nil)
    (put symbol 'gv-expander nil)))

(e-harness--clear-derived-accessor-metadata)

(defun e-harness--next-turn-id ()
  "Return a new durable turn id."
  (e-session-generate-ulid))

(defun e-harness--turn-work-spec ()
  "Return the lifecycle-only work spec for one harness turn.
The harness owns the provider loop and settles this handle from its existing
turn completion paths; the runner deliberately performs no separate work."
  (e-work-spec-create
   :id "agent-turn"
   :description "Run one agent turn."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'harness
    :runner (lambda (_handle _arguments _context) :deferred)))

(defun e-harness-set-work-enrollment-function (harness function)
  "Set HARNESS's injected prepared-work enrollment FUNCTION.
FUNCTION receives a prepared work handle and, for tool work, an optional exact
invocation callback.  The harness owns no board dependency; the runtime adapter
may install this port for board-attached sessions."
  (unless (or (null function) (functionp function))
    (signal 'wrong-type-argument (list 'functionp function)))
  (setf (e-harness-work-enrollment-function harness) function)
  harness)

(defun e-harness-set-board-aggregation-function (harness function)
  "Set HARNESS's injected board aggregation subscription FUNCTION."
  (unless (or (null function) (functionp function))
    (signal 'wrong-type-argument (list 'functionp function)))
  (setf (e-harness-board-aggregation-function harness) function)
  harness)

(defun e-harness-refresh-default-context-strategy (harness)
  "Refresh HARNESS default context strategy, preserving custom strategies."
  (when (e-context-transcript-stack-p (e-harness-context-strategy harness))
    (setf (e-harness-context-strategy harness)
          (e-context-transcript-stack-create)))
  harness)

(defun e-harness-refresh-runtime-from (harness fresh)
  "Refresh replaceable runtime configuration on HARNESS from FRESH.

This preserves HARNESS identity and lifecycle state: its session store,
subscribers, active turns, prompt queues, and injected board/work ports remain
owned by the original object.  Provider and configuration fields are replaced
so a presentation endpoint retained across live reload does not keep stale
adapter closures after its registry entry has been recreated."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (e-harness-p fresh)
    (signal 'wrong-type-argument (list 'e-harness-p fresh)))
  (setf (e-harness-backend harness) (e-harness-backend fresh))
  (setf (e-harness-default-options harness)
        (copy-sequence (e-harness-default-options fresh)))
  (setf (e-harness-default-project-root harness)
        (e-harness-default-project-root fresh))
  (setf (e-harness-runtime-capability-config harness)
        (copy-tree (e-harness-runtime-capability-config fresh)))
  (e-harness-clear-effective-capability-config-cache harness)
  ;; The transcript stack is the replaceable built-in default.  A custom
  ;; strategy may carry live state and remains owned by the retained harness.
  (when (e-context-transcript-stack-p (e-harness-context-strategy harness))
    (setf (e-harness-context-strategy harness)
          (e-harness-context-strategy fresh)))
  harness)

(defun e-harness--normalize-project-root (root)
  "Return normalized project ROOT, or nil."
  (when (and (stringp root)
             (not (string-empty-p (string-trim root))))
    (file-name-as-directory (expand-file-name root))))

(defun e-harness--normalize-session-metadata (metadata)
  "Return session METADATA with normalized harness-owned fields."
  (let ((metadata (copy-sequence metadata)))
    (when (plist-member metadata :project-root)
      (if-let ((root (e-harness--normalize-project-root
                      (plist-get metadata :project-root))))
          (setq metadata (plist-put metadata :project-root root))
        (cl-remf metadata :project-root)))
    metadata))

(defun e-harness--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-harness--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when developer profiling is enabled."
  (if (e-harness--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(cl-defun e-harness-create
    (&key backend context-strategy default-options capability-config
          sessions enabled-layer-ids intrinsic-capabilities
          project-root layer-change-function)
  "Create a core harness.
BACKEND, CONTEXT-STRATEGY, DEFAULT-OPTIONS, CAPABILITY-CONFIG, SESSIONS,
ENABLED-LAYER-IDS, INTRINSIC-CAPABILITIES, PROJECT-ROOT, and
LAYER-CHANGE-FUNCTION configure the provider-neutral runtime.

When LAYER-CHANGE-FUNCTION is non-nil, it is called with HARNESS after public
layer selection APIs change the enabled layer set."
  (let ((harness
         (e-harness--make :backend backend
                          :context-strategy (or context-strategy
                                                (e-context-transcript-stack-create))
                          :default-options default-options
                          :default-project-root
                          (e-harness--normalize-project-root project-root)
                          :runtime-capability-config
                          (copy-tree capability-config)
                          :sessions (or sessions (e-session-store-create))
                          :enabled-layer-ids (copy-sequence enabled-layer-ids)
                          :intrinsic-capabilities
                          (copy-sequence intrinsic-capabilities)
                          :active-turns (make-hash-table :test 'equal)
                          :prompt-queues (make-hash-table :test 'equal)
                          :prompt-queue-counts (make-hash-table :test 'equal)
                          :provider-compaction-candidates
                          (make-hash-table :test 'equal))))
    (when layer-change-function
      (e-harness-set-layer-change-function harness layer-change-function))
    harness))

(defun e-harness-capability-config (harness capability-id)
  "Return HARNESS-local runtime config plist for CAPABILITY-ID."
  (copy-sequence
   (alist-get capability-id
              (e-harness-runtime-capability-config harness))))

(defun e-harness--effective-capability-config-cache (harness)
  "Return HARNESS's private effective configuration cache."
  (or (gethash harness e-harness--effective-capability-config-caches)
      (let ((cache (make-hash-table :test 'equal)))
        (puthash harness cache e-harness--effective-capability-config-caches)
        cache)))

(defun e-harness-clear-effective-capability-config-cache (harness)
  "Discard all derived effective capability configuration for HARNESS.
Use this after changing harness-local runtime configuration outside the public
configuration setter."
  (remhash harness e-harness--effective-capability-config-caches)
  harness)

(defun e-harness-set-capability-config (harness capability-id config)
  "Set HARNESS-local runtime CONFIG plist for CAPABILITY-ID.
When CONFIG is nil, clear the runtime config for CAPABILITY-ID."
  (let ((configs (copy-tree
                  (e-harness-runtime-capability-config harness))))
    (if config
        (setf (alist-get capability-id configs)
              (copy-sequence config))
      (setq configs (assq-delete-all capability-id configs)))
    (setf (e-harness-runtime-capability-config harness) configs)
    (e-harness-clear-effective-capability-config-cache harness)
    config))

(cl-defun e-harness-effective-capability-config
    (harness capability-id options &key session-id directory overrides)
  "Return effective CAPABILITY-ID config for HARNESS.
Resolution uses DIRECTORY or the session project root, then HARNESS-local
runtime config, then explicit OVERRIDES."
  (let ((root (or directory (e-harness-project-root harness session-id))))
    ;; Explicit overrides are per-call policy.  Keeping them out of this cache
    ;; avoids retaining caller-owned data and preserves the old fresh-result
    ;; semantics for callers that build overrides dynamically.
    (if overrides
        (e-capability-config-resolve
         capability-id options
         :directory root
         :runtime-config (e-harness-capability-config harness capability-id)
         :overrides overrides)
      (let* ((key (list capability-id
                        options
                        root
                        (e-capability-config-generation)
                        (e-capability-config-directory-revision root)))
             (cache (e-harness--effective-capability-config-cache harness))
             (missing (make-symbol "missing"))
             (cached (gethash key cache missing)))
        (if (not (eq cached missing))
            ;; Resolved config was previously a fresh value.  Do not let a
            ;; caller mutate the harness-owned derived value through its return.
            (copy-tree cached)
          (let ((resolved
                 (e-capability-config-resolve
                  capability-id options
                  :directory root
                  :runtime-config
                  (e-harness-capability-config harness capability-id))))
            (puthash key (copy-tree resolved) cache)
            resolved))))))

(defun e-harness--append-layer-id (ids id)
  "Return IDS with ID appended once."
  (if (memq id ids)
      ids
    (append ids (list id))))

(defun e-harness--effective-layer-ids-for-root (layer-ids directory)
  "Return dependency-expanded LAYER-IDS for DIRECTORY in deterministic order."
  (let (resolved)
    (cl-labels
        ((visit (id visiting)
          (unless (memq id resolved)
            (when (memq id visiting)
              (signal 'e-layer-registry-missing
                      (list (format "cyclic layer dependency at %s" id))))
            (let ((layer (e-layer-create-registered id directory)))
              (dolist (required-id (e-layer-requires layer))
                (visit required-id (cons id visiting)))
              (setq resolved (e-harness--append-layer-id resolved id))))))
      (dolist (id layer-ids)
        (visit id nil)))
    resolved))

(defun e-harness-effective-layer-ids (harness &optional session-id turn-id)
  "Return HARNESS enabled layer ids plus transitive requirements.
SESSION-ID and TURN-ID identify the root used for config-aware layer factories."
  (e-harness--effective-layer-ids-for-root
   (e-harness-enabled-layer-ids harness)
   (e-harness-project-root harness session-id turn-id)))

(defun e-harness-effective-layers (harness &optional session-id turn-id)
  "Return fresh effective layer objects for HARNESS SESSION-ID and TURN-ID."
  (let ((directory (e-harness-project-root harness session-id turn-id)))
    (mapcar (lambda (id)
              (e-layer-create-registered id directory))
            (e-harness-effective-layer-ids harness session-id turn-id))))

(defun e-harness-effective-capabilities (harness &optional session-id turn-id)
  "Return fresh model-facing capabilities for HARNESS SESSION-ID and TURN-ID."
  (let ((capabilities (copy-sequence
                       (or (e-harness-intrinsic-capabilities harness) nil))))
    (dolist (layer (e-harness-effective-layers harness session-id turn-id))
      (setq capabilities
            (append capabilities
                    (copy-sequence (or (e-layer-capabilities layer) nil)))))
    capabilities))

(defun e-harness--unique-action-capabilities (capabilities)
  "Return CAPABILITIES after rejecting duplicate capability ids."
  (let ((seen (make-hash-table :test #'eq))
        result)
    (dolist (capability capabilities)
      (let ((id (e-capability-id capability)))
        (when (gethash id seen)
          (signal 'e-harness-duplicate-action-capability (list id)))
        (puthash id t seen)
        (push capability result)))
    (nreverse result)))

(cl-defun e-harness-effective-action-capabilities
    (harness &optional session-id turn-id &key requested-capability-id)
  "Return action capabilities for HARNESS SESSION-ID and TURN-ID.
Ordinary action-bearing capabilities are combined with action capabilities
provided dynamically for the active session context.
REQUESTED-CAPABILITY-ID identifies a specific capability lookup when known."
  (let* ((capabilities
          (e-harness-effective-capabilities harness session-id turn-id))
         (ordinary
          (cl-remove-if-not #'e-capability-actions capabilities))
         (provided
          (e-capabilities-provided-action-capabilities
           capabilities
           :harness harness
           :session-id session-id
           :turn-id turn-id
           :requested-capability-id requested-capability-id)))
    (e-harness--unique-action-capabilities
     (append ordinary
             (cl-remove-if-not #'e-capability-actions provided)))))

(defun e-harness-active-capabilities (harness)
  "Return HARNESS capabilities for callers without session context."
  (e-harness-effective-capabilities harness))

(defun e-harness-set-intrinsic-capabilities (harness capabilities)
  "Set HARNESS intrinsic CAPABILITIES."
  (setf (e-harness-intrinsic-capabilities harness)
        (copy-sequence capabilities))
  capabilities)

(defun e-harness-tools (harness &optional session-id turn-id)
  "Return a fresh tool registry view over HARNESS effective capabilities."
  (let ((registry (e-tools-registry-create)))
    (e-harness--register-resource-operation-tools
     registry
     (e-harness-resources harness session-id turn-id))
    (dolist (capability
             (e-harness-effective-capabilities harness session-id turn-id))
      (e-capabilities-register-tools
       capability
       registry
       :harness harness
       :session-id session-id
       :turn-id turn-id))
    registry))

(defun e-harness-prompts (harness)
  "Return prompt specs contributed by HARNESS effective capabilities."
  (let (prompts)
    (dolist (capability (e-harness-effective-capabilities harness))
      (setq prompts
            (append prompts
                    (copy-sequence (or (e-capability-prompts capability)
                                       nil)))))
    prompts))

(defun e-harness-prompt-by-name (harness name)
  "Return the first active prompt named NAME in HARNESS, or nil."
  (let ((name (e-prompts--normalize-name name 'prompt-name)))
    (cl-find name
             (e-harness-prompts harness)
             :key #'e-prompt-spec-name
             :test #'equal)))

(defun e-harness-prompt-name-collisions (harness)
  "Return duplicate prompt-name diagnostics for HARNESS.
Each diagnostic is (:name NAME :prompts PROMPTS), preserving active capability
order for PROMPTS."
  (let ((table (make-hash-table :test 'equal))
        collisions)
    (dolist (prompt (e-harness-prompts harness))
      (push prompt (gethash (e-prompt-spec-name prompt) table)))
    (maphash (lambda (name prompts)
               (let ((prompts (nreverse prompts)))
                 (when (> (length prompts) 1)
                   (push (list :name name :prompts prompts) collisions))))
             table)
    (nreverse collisions)))

(defun e-harness-hooks (harness)
  "Return a fresh hook registry view over HARNESS effective capabilities."
  (let ((registry (e-hooks-registry-create)))
    (dolist (capability (e-harness-effective-capabilities harness))
      (e-capabilities-register-hooks capability registry))
    registry))

(defun e-harness-structured-blocks (harness &optional session-id turn-id)
  "Return a fresh structured-block registry view over HARNESS capabilities.
SESSION-ID and TURN-ID scope the effective capability/layer set."
  (let ((registry (e-structured-blocks-registry-create)))
    (dolist (capability
             (e-harness-effective-capabilities harness session-id turn-id))
      (e-capabilities-register-structured-blocks capability registry))
    registry))

(cl-defun e-harness-message-details
    (harness session-id message &key structured-blocks)
  "Return capability-owned presentation details for MESSAGE.
STRUCTURED-BLOCKS is the already parsed block list from the presentation
transform.  Passing it keeps semantic providers from reparsing message text."
  (let* ((turn-id (plist-get message :turn-id))
         (context (list :harness harness
                        :session-id session-id
                        :turn-id turn-id
                        :structured-blocks structured-blocks))
         details)
    (dolist (capability
             (e-harness-effective-capabilities harness session-id turn-id))
      (setq details
            (append details
                    (e-capabilities-message-details
                     capability message context))))
    details))

(defun e-harness-store (harness &optional session-id turn-id)
  "Return a fresh e:// store view over HARNESS effective capabilities.
SESSION-ID and TURN-ID are passed to context-aware resource providers."
  (let ((store (e-store-create)))
    (dolist (capability
             (e-harness-effective-capabilities harness session-id turn-id))
      (e-capabilities-register-resources
       capability
       store
       :harness harness
       :session-id session-id
       :turn-id turn-id))
    store))

(defun e-harness-resources (harness &optional session-id turn-id)
  "Return a fresh resource registry view over HARNESS effective capabilities."
  (let ((registry (e-resources-registry-create))
        (store (e-harness-store harness session-id turn-id)))
    (dolist (capability
             (e-harness-effective-capabilities harness session-id turn-id))
      (e-capabilities-register-resource-methods
       capability
       registry
       :harness harness
       :session-id session-id
       :turn-id turn-id
       :store store))
    (when (e-store-list store)
      (e-resources-register registry (e-store-resource-methods store)))
    registry))

(defun e-harness--resource-method-description (method)
  "Return model-facing description fragment for METHOD."
  (let ((patterns (or (e-resource-method-uri-patterns method)
                      (list (format "%s://<resource>"
                                    (e-resource-method-scheme method)))))
        (description (e-resource-method-description method))
        (range-modes (e-resource-method-range-modes method)))
    (string-join
     (delq nil
           (list
            (format "- %s" (string-join patterns ", "))
            description
            (when range-modes
              (format "Range units: %s." (string-join range-modes ", ")))))
     " ")))

(defconst e-harness-lean-resource-operation-ids
  '(glob search table-of-content)
  "Resource operations whose schemas stay lean.
Detailed scheme and advanced-option guidance for these discovery operations is
available through e:// resources instead of repeating it in every request.")

(defun e-harness--resource-operation-description (resources operation)
  "Return model-facing description for OPERATION over active RESOURCES."
  (if (memq (e-operation-id operation)
            e-harness-lean-resource-operation-ids)
      (e-operation-description operation)
    (let ((methods (e-resources-methods-for-operation resources operation)))
      (string-join
       (list (e-operation-description operation)
             ""
             "Active URI schemes:"
             (mapconcat #'e-harness--resource-method-description methods "\n"))
       "\n"))))

(defun e-harness--resource-operation-metadata (operation uri)
  "Return compact resource usage metadata for OPERATION over URI."
  (e-tools-resource-usage-metadata
   (e-operation-tool-name operation)
   (list (list :uri uri
               :operation (e-operation-id operation)))))

(defun e-harness--resource-operation-result (operation uri content)
  "Return CONTENT as the current resource OPERATION tool result when possible."
  (let ((call (plist-get (e-tools-current-context) :tool-call))
        (metadata (e-harness--resource-operation-metadata operation uri)))
    (if call
        (e-tools-result-create call 'ok content metadata)
      content)))

(defun e-harness--resource-operation-call (resources operation uri arguments)
  "Call resource OPERATION for URI with ARGUMENTS and wrap metadata."
  (e-harness--resource-operation-result
   operation
   uri
   (apply #'e-resources-call resources operation uri arguments)))

(defun e-harness--resource-method-work (method)
  "Return METHOD's work spec, tolerating older live resource records."
  (and (e-resource-method-p method)
       (>= (length method) 11)
       (e-resource-method-work method)))

(defun e-harness--resource-operation-work (resources operation)
  "Return a Work spec for async resource OPERATION over RESOURCES."
  (e-work-spec-create
   :id (format "resource.%s" (e-operation-tool-name operation))
   :description (format "Run resource operation %s."
                        (e-operation-tool-name operation))
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'resources
   :runner
   (lambda (handle arguments _context)
     (let ((dispatch (e-operation-dispatch operation))
           child-handle)
       (funcall
        dispatch
        (lambda (uri &rest operation-arguments)
          (let* ((parsed-uri (e-resources-parse-uri uri))
                 (method (e-resources--method resources operation parsed-uri))
                 (work (e-harness--resource-method-work method)))
            (if (e-work-spec-p work)
                (progn
                  (setf (e-work-handle-cancel-function handle)
                        (lambda (_handle)
                          (when (e-work-handle-p child-handle)
                            (e-work-cancel child-handle))
                          t))
                  (setq child-handle
                        (e-work-start
                         work
                         (list :uri parsed-uri
                               :operation-arguments operation-arguments
                               :resource-operation operation)
                         :context (list :resource-uri uri
                                        :resource-operation
                                        (e-operation-id-of operation))
                         :on-done (lambda (content)
                                    (e-work-finish
                                     handle
                                     (e-harness--resource-operation-result
                                      operation uri content)))
                         :on-error (lambda (err)
                                     (e-work-fail handle err))
                         :on-progress (lambda (payload)
                                        (e-work-progress handle payload))))
                  (setf (e-work-handle-metadata handle)
                        (append (e-work-handle-metadata handle)
                                (list :resource-uri uri
                                      :resource-operation
                                      (e-operation-id-of operation)
                                      :child-work-handle child-handle)
                                (e-work-handle-metadata child-handle))))
              (setf (e-work-handle-metadata handle)
                    (append (e-work-handle-metadata handle)
                            (list :resource-uri uri
                                  :resource-operation
                                  (e-operation-id-of operation)
                                  :resource-work 'inline)))
              (e-work-finish
               handle
               (e-harness--resource-operation-call
                resources operation uri operation-arguments)))))
        arguments))
     :deferred)))

(defun e-harness--resource-operation-async-p (operation)
  "Return non-nil when OPERATION should expose an async tool start."
  (memq (e-operation-id-of operation) '(glob search table-of-content)))

(defun e-harness--register-resource-operation-tool (registry resources operation)
  "Register OPERATION in REGISTRY as a model-facing tool backed by RESOURCES."
  (let ((dispatch (e-operation-dispatch operation)))
    (when (functionp dispatch)
      (let* ((async-p (e-harness--resource-operation-async-p operation))
             (tool-name (e-operation-tool-name operation))
             (cheap-runner
              (lambda (arguments)
                (funcall dispatch
                         (lambda (uri &rest operation-arguments)
                           (e-harness--resource-operation-call
                            resources operation uri operation-arguments))
                         arguments))))
        (e-tools-register
         registry
         :name tool-name
         :description (e-harness--resource-operation-description resources operation)
         :parameters (if async-p
                         (e-work-detachable-merge-parameters
                          (e-operation-parameters operation))
                       (e-operation-parameters operation))
         :work (if async-p
                   (e-work-detachable-spec
                    (e-harness--resource-operation-work resources operation)
                    :id (format "resource.%s" tool-name)
                    :owner 'resources)
                 (e-tools-cheap-work
                  (format "resource.%s" tool-name)
                  cheap-runner
                  :owner 'resources))
         :blocking-class (if async-p 'process 'cheap))))))

(defun e-harness--register-resource-operation-tools (registry resources)
  "Register active resource operation tools in REGISTRY backed by RESOURCES."
  (dolist (operation (e-resources-operations resources))
    (when (e-operation-p operation)
      (e-harness--register-resource-operation-tool registry resources operation))))

(defun e-harness--register-resource-tools (registry resources)
  "Register resource operation tools from RESOURCES in REGISTRY."
  (e-harness--register-resource-operation-tools registry resources))

(defun e-harness-layer-change-function (harness)
  "Return HARNESS layer-change callback, or nil."
  (gethash harness e-harness--layer-change-functions))

(defun e-harness-set-layer-change-function (harness function)
  "Set HARNESS layer-change callback to FUNCTION.
When FUNCTION is nil, clear any existing callback."
  (if function
      (puthash harness function e-harness--layer-change-functions)
    (remhash harness e-harness--layer-change-functions))
  function)

(defun e-harness--notify-layers-changed (harness)
  "Notify HARNESS that its enabled layer set changed."
  (when-let ((function (e-harness-layer-change-function harness)))
    (funcall function harness))
  harness)

(defun e-harness-activate-capability (harness capability)
  "Activate CAPABILITY in HARNESS as an intrinsic capability."
  (setf (e-harness-intrinsic-capabilities harness)
        (append (e-harness-intrinsic-capabilities harness)
                (list capability)))
  capability)

(defun e-harness-layer-enabled-p (harness layer-id)
  "Return non-nil when LAYER-ID is explicitly enabled on HARNESS."
  (memq layer-id (e-harness-enabled-layer-ids harness)))

(defun e-harness-layer-effective-p (harness layer-id &optional session-id turn-id)
  "Return non-nil when LAYER-ID is effective for HARNESS SESSION-ID TURN-ID."
  (memq layer-id (e-harness-effective-layer-ids harness session-id turn-id)))

(defun e-harness-set-enabled-layer-ids (harness layer-ids)
  "Set HARNESS explicit enabled LAYER-IDS."
  (setf (e-harness-enabled-layer-ids harness)
        (copy-sequence layer-ids))
  (e-harness--notify-layers-changed harness)
  (e-harness-enabled-layer-ids harness))

(defun e-harness-sync-layer-shells (harness &optional directory)
  "Rebuild HARNESS presentation shells from enabled layers.
DIRECTORY is used for config-aware shell layer factories; when nil, use the
harness default project root."
  (let ((root (or (e-harness--normalize-project-root directory)
                  (e-harness-default-project-root harness))))
    (e-shell-clear-harness-shells harness)
    (dolist (id (e-harness--effective-layer-ids-for-root
                 (e-harness-enabled-layer-ids harness)
                 root))
      (let ((layer (e-layer-create-registered id root)))
        (when (e-layer-shells layer)
          (e-shell-register-layer-shells
           harness id (e-layer-shells layer)
           :project-root root
           :metadata (list :layer-id id))))))
  harness)

(defun e-harness-enable-layer-id (harness layer-id &optional directory)
  "Enable registered LAYER-ID on HARNESS and refresh layer shells."
  (unless (e-harness-layer-enabled-p harness layer-id)
    (let ((prospective-ids
           (e-harness--append-layer-id
            (e-harness-enabled-layer-ids harness)
            layer-id))
          (root (or (e-harness--normalize-project-root directory)
                    (e-harness-default-project-root harness))))
      ;; Resolve the complete prospective graph before mutating harness state.
      ;; An unknown id or dependency must fail without poisoning later turns.
      (e-harness--effective-layer-ids-for-root prospective-ids root)
      (setf (e-harness-enabled-layer-ids harness) prospective-ids))
    (e-harness-sync-layer-shells harness directory)
    (e-harness--notify-layers-changed harness))
  (list :status 'enabled
        :layer-id layer-id
        :enabled t
        :active (e-harness-layer-effective-p harness layer-id)))

(defun e-harness-disable-layer-id (harness layer-id &optional directory)
  "Disable explicit LAYER-ID on HARNESS and refresh layer shells."
  (let ((was-enabled (e-harness-layer-enabled-p harness layer-id)))
    (when was-enabled
      (setf (e-harness-enabled-layer-ids harness)
            (delq layer-id (copy-sequence
                            (e-harness-enabled-layer-ids harness))))
      (e-harness-sync-layer-shells harness directory)
      (e-harness--notify-layers-changed harness))
    (let ((effective (e-harness-layer-effective-p harness layer-id)))
      (list :status (cond
                     ((not was-enabled) 'already-disabled)
                     (effective 'disabled-but-required)
                     (t 'disabled))
            :layer-id layer-id
            :enabled nil
            :active effective))))

(cl-defun e-harness-create-session (harness &key id metadata)
  "Create session ID with METADATA in HARNESS."
  (e-session-create (e-harness-sessions harness)
                    :id id
                    :metadata (e-harness--normalize-session-metadata
                               metadata)))

(cl-defun e-harness-fork-session (harness session-id &key at metadata name)
  "Fork SESSION-ID in HARNESS into a new independent session and return it.
Seeds a fresh session with a snapshot of the source's messages up to AT (a head
entry id; defaults to the current head), copying context metadata and turn
options so the fork resumes with the same working context.  The source session
is left untouched.  See `e-session-fork'."
  (e-session-fork (e-harness-sessions harness)
                  session-id
                  :at at
                  :metadata (e-harness--normalize-session-metadata metadata)
                  :name name))

(defun e-harness-project-root (harness &optional session-id _turn-id)
  "Return the explicit project root for HARNESS SESSION-ID, or nil.
Session metadata wins over the harness default project root.  Consumers that
own a narrower fallback, such as a layer construction root, should apply it
after this accessor returns nil."
  (or
   (when session-id
     (when-let ((session (ignore-errors
                           (e-session-get (e-harness-sessions harness)
                                          session-id))))
       (e-harness--normalize-project-root
        (plist-get (plist-get session :metadata) :project-root))))
   (e-harness-default-project-root harness)))

(defcustom e-default-projects nil
  "Projects loaded by `e' startup and available as workspace roots.

Each entry is a directory.  Project-local extensions under configured projects
are trusted and primed during startup, and every normalized project path is a
secondary root for file resources in sessions whose primary root is elsewhere.
The primary root remains the base for relative paths and the bash working
directory."
  :type '(repeat directory)
  :group 'e)

(defun e-default-project-roots ()
  "Return normalized, de-duplicated roots from `e-default-projects'."
  (let (roots)
    (dolist (project e-default-projects)
      (when-let ((root (e-harness--normalize-project-root project)))
        (unless (member root roots)
          (push root roots))))
    (nreverse roots)))

(defcustom e-workspace-roots-alist nil
  "Extra workspace roots keyed by primary project root.

Each entry is (PROJECT-ROOT . EXTRA-ROOTS): when a session's primary project
root is at or below PROJECT-ROOT, EXTRA-ROOTS additionally become valid roots
for that session's file resources.  Directories are normalized before
comparison.  This only widens the file-resource trust gate; the bash tool still
runs in the primary project root.  Configure it alongside
`e-project-local-allowed-roots'."
  :type '(alist :key-type directory :value-type (repeat directory))
  :group 'e)

(defun e-harness--root-at-or-below-p (target root)
  "Return non-nil when normalized TARGET is at or below normalized ROOT."
  (when-let ((target (e-harness--normalize-project-root target))
             (root (e-harness--normalize-project-root root)))
    (string-prefix-p (file-truename root) (file-truename target))))

(defun e-harness-configured-workspace-roots (primary-root)
  "Return configured extra and default project roots for PRIMARY-ROOT.
Collects EXTRA-ROOTS from `e-workspace-roots-alist' entries whose key is an
ancestor of (or equal to) PRIMARY-ROOT, followed by `e-default-projects'.
Returns a normalized, de-duplicated list, excluding PRIMARY-ROOT itself."
  (when-let ((primary (e-harness--normalize-project-root primary-root)))
    (let (roots)
      (dolist (entry e-workspace-roots-alist)
        (when (e-harness--root-at-or-below-p primary (car entry))
          (dolist (extra (cdr entry))
            (when-let ((extra (e-harness--normalize-project-root extra)))
              (unless (or (equal extra primary) (member extra roots))
                (push extra roots))))))
      (dolist (project (e-default-project-roots))
        (unless (or (equal project primary) (member project roots))
          (push project roots)))
      (nreverse roots))))

(defun e-harness-workspace-roots (harness &optional session-id turn-id)
  "Return active workspace roots for HARNESS SESSION-ID as a normalized list.
The first element is the primary project root (the base for relative paths and
the bash working directory); the rest are configured extra roots from
`e-workspace-roots-alist' followed by `e-default-projects'.  Returns nil when
no primary root is resolvable."
  (when-let ((primary (e-harness--normalize-project-root
                       (e-harness-project-root harness session-id turn-id))))
    (cons primary (e-harness-configured-workspace-roots primary))))

(cl-defun e-harness--install-activity-sink (harness subscriber &key session-id)
  "Register SUBSCRIBER for core events from HARNESS.
When SESSION-ID is non-nil, SUBSCRIBER only receives events for that session."
  (let ((record (list :callback subscriber :session-id session-id)))
    (push record (e-harness-subscribers harness))
    record))

(defun e-harness--remove-activity-sink (harness subscription)
  "Remove SUBSCRIPTION from HARNESS subscribers.
SUBSCRIPTION should be a record returned by
`e-harness--install-activity-sink'.  Removing an already-removed record is a
no-op."
  (setf (e-harness-subscribers harness)
        (delq subscription (e-harness-subscribers harness)))
  nil)

(defun e-harness--running-turns-p (harness)
  "Return non-nil when HARNESS owns any running turn."
  (let (running)
    (maphash
     (lambda (_session-id entry)
       (when (eq (plist-get entry :status) 'running)
         (setq running t)))
     (e-harness-active-turns harness))
    running))

(defun e-harness--apply-pending-runtime-refresh (harness)
  "Apply HARNESS's pending runtime refresh once no turn is running."
  (when-let ((pending (gethash harness e-harness--pending-runtime-refreshes)))
    (plist-put pending :timer nil)
    (unless (e-harness--running-turns-p harness)
      (e-harness--remove-activity-sink harness
                                       (plist-get pending :subscription))
      (remhash harness e-harness--pending-runtime-refreshes)
      (e-harness-refresh-runtime-from harness (plist-get pending :fresh)))))

(defun e-harness--schedule-pending-runtime-refresh (harness)
  "Schedule one post-terminal pending runtime refresh check for HARNESS."
  (when-let ((pending (gethash harness e-harness--pending-runtime-refreshes)))
    (unless (timerp (plist-get pending :timer))
      (plist-put pending :timer
                 (run-at-time
                  0 nil #'e-harness--apply-pending-runtime-refresh harness)))))

(defun e-harness-request-runtime-refresh (harness fresh)
  "Refresh HARNESS from FRESH now, or at its next idle turn boundary.

The newest request replaces an older pending one.  An admitted turn keeps the
backend it started with; the refresh runs after the last running turn settles
and before a queued turn scheduled from that terminal edge can start."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (e-harness-p fresh)
    (signal 'wrong-type-argument (list 'e-harness-p fresh)))
  (if (not (e-harness--running-turns-p harness))
      (e-harness-refresh-runtime-from harness fresh)
    (if-let ((pending
              (gethash harness e-harness--pending-runtime-refreshes)))
        (plist-put pending :fresh fresh)
      (let (subscription)
        (setq subscription
              (e-harness--install-activity-sink
               harness
               (lambda (event)
                 (when (memq (plist-get event :type)
                             '(turn-finished turn-failed turn-cancelled))
                   (e-harness--schedule-pending-runtime-refresh harness)))))
        (puthash harness
                 (list :fresh fresh :subscription subscription :timer nil)
                 e-harness--pending-runtime-refreshes)))
    harness))

(defun e-harness--emit (harness event)
  "Emit EVENT to HARNESS subscribers."
  (let ((event-session-id (plist-get event :session-id)))
    (dolist (subscriber (reverse (e-harness-subscribers harness)))
      (let ((callback (if (functionp subscriber)
                          subscriber
                        (plist-get subscriber :callback)))
            (session-id (and (listp subscriber)
                             (plist-get subscriber :session-id))))
        (when (or (not session-id)
                  (equal session-id event-session-id))
          (funcall callback event))))))

(defconst e-harness--durable-activity-event-types
  '(turn-started provider-request-started provider-request-finished
    turn-retrying
    reasoning-delta reasoning-raw-delta
    tool-started tool-finished action-started action-finished action-failed
    hook-audit turn-finished token-usage
    turn-failed turn-cancelled turn-steered backend-empty-output
    compaction-started compaction-prepared compaction-summary-started
    compaction-finished compaction-failed)
  "Turn event types stored as durable session activity.")

(defconst e-harness--activity-event-classes
  '((turn-started . audit)
    (provider-request-started . audit)
    (provider-request-finished . audit)
    (turn-retrying . audit)
    (reasoning-delta . presentation-log)
    (reasoning-raw-delta . presentation-log)
    (tool-started . audit)
    (tool-finished . presentation-log)
    (action-started . audit)
    (action-finished . presentation-log)
    (action-failed . audit)
    (hook-audit . audit)
    (turn-finished . replay)
    (token-usage . audit)
    (turn-failed . audit)
    (turn-cancelled . audit)
    (turn-steered . audit)
    (backend-empty-output . audit)
    (compaction-started . audit)
    (compaction-prepared . audit)
    (compaction-summary-started . audit)
    (compaction-finished . replay)
    (compaction-failed . audit))
  "Persistence reason for activity event types.
Classes are `audit', `replay', `presentation-log', and `transient-progress'.")

(defconst e-harness--activity-index-flush-event-types
  '(hook-audit turn-finished turn-failed turn-cancelled backend-empty-output
    compaction-finished compaction-failed)
  "Durable activity event types that should flush the session index.")

(defcustom e-harness-durable-tool-finished-result-preview-bytes 4096
  "Maximum UTF-8 bytes retained from tool results in durable activity events.

Tool transcript messages retain the model-visible tool result.  Durable
`tool-finished' activity is presentation history, so it stores only a compact
preview to avoid duplicating large outputs in session JSONL files."
  :type 'integer
  :group 'e)

(defcustom e-harness-provider-request-deadline-seconds nil
  "Optional hard wall-clock deadline attached to a turn's provider requests.

Default nil.  The deadline is stamped once when turn options are built and is
shared by every provider request in the turn, so a non-nil value caps the
whole turn's wall clock, not one request attempt -- a long turn with many tool
calls can exceed it and fail even though no single request is stuck.

Leave this nil in normal use.  Provider adapters may expose transport-specific
idle deadlines, but OpenAI-like HTTP requests deliberately have no implicit
local deadline because some gateways buffer healthy long-reasoning responses.
Transient failures are handled by harness retry/backoff
(`e-harness-retry-max-elapsed-seconds' and friends).  Set a number only when
you deliberately want a per-turn wall-clock cap; explicit `:deadline' turn
options still apply regardless."
  :type '(choice (const :tag "No default deadline" nil)
                 number)
  :group 'e)

(defcustom e-harness-retry-max-elapsed-seconds 1800.0
  "Total wall-clock budget for retrying a transient backend turn.
Retries of a turn that keeps failing with a retryable error (rate limiting,
overload, or a transport reset) stop once this much time has elapsed since the
first attempt; the turn then settles as `turn-failed' as before.  The default
mirrors Claude Code's patience: many minutes of backoff across a long burst of
rate limiting.  When a retryable error names a concrete reset time, that known
reopen can extend this budget (see `e-harness-retry-reset-max-wait-seconds').
Set to 0 to disable retrying."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-initial-backoff-seconds 2.0
  "Initial delay before the first retry of a transient backend turn."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-backoff-multiplier 2.0
  "Multiplier applied to the retry backoff after each failed attempt."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-max-backoff-seconds 60.0
  "Maximum delay between retries of a transient backend turn."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-jitter-fraction 0.25
  "Random fraction of the backoff added as jitter before each retry.
Jitter spreads concurrent retries so a recovering backend is not hit by a
synchronized burst.  A value of 0.25 adds up to 25% of the computed backoff.
Set to 0 to disable jitter (backoff becomes fully deterministic)."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-reset-max-wait-seconds 900.0
  "Maximum seconds to honor an adapter-normalized retry reset delay.
When a backend adapter supplies `:retry-after-seconds', the harness waits that
long instead of using blind exponential backoff, and a wait shorter than this
cap extends the retry budget so a turn is not abandoned minutes before
capacity returns.  A delay farther out falls back to ordinary backoff.  Set to
0 to ignore reset hints entirely."
  :type 'number
  :group 'e)

(defun e-harness--retryable-error-p (details)
  "Return non-nil when adapter-normalized DETAILS permit a retry."
  (and (listp details) (eq (plist-get details :retryable) t)))

(defun e-harness--retry-backoff-seconds (attempt)
  "Return the backoff delay in seconds before retry ATTEMPT (1-based).
The delay grows geometrically and is capped at
`e-harness-retry-max-backoff-seconds', then has up to
`e-harness-retry-jitter-fraction' of itself added as random jitter."
  (let* ((base (min e-harness-retry-max-backoff-seconds
                    (* e-harness-retry-initial-backoff-seconds
                       (expt e-harness-retry-backoff-multiplier
                             (max 0 (1- attempt))))))
         (jitter (if (> e-harness-retry-jitter-fraction 0)
                     (* base e-harness-retry-jitter-fraction (/ (random 1000) 1000.0))
                   0)))
    (+ base jitter)))

(defun e-harness--retry-reset-seconds (details)
  "Return the bounded adapter-normalized reset delay from DETAILS.
The harness owns only the maximum wait policy.  Provider headers, payloads,
and message text are interpreted by the backend adapter and supplied as
`:retry-after-seconds'."
  (when (> e-harness-retry-reset-max-wait-seconds 0)
    (let ((seconds (and (listp details)
                        (plist-get details :retry-after-seconds))))
      (when (numberp seconds)
        (let ((wait (max 1.0 seconds)))
          (when (<= wait e-harness-retry-reset-max-wait-seconds)
            wait))))))

(defun e-harness--durable-activity-event-p (type)
  "Return non-nil when TYPE should be stored as session activity."
  (let ((class (e-harness-activity-event-class type)))
    (and class
         (not (eq class 'transient-progress))
         (memq type e-harness--durable-activity-event-types))))

(defun e-harness-activity-event-class (type)
  "Return persistence class for activity event TYPE."
  (cdr (assq type e-harness--activity-event-classes)))

(defun e-harness--activity-index-flush-event-p (type)
  "Return non-nil when TYPE should flush coalesced activity index writes."
  (memq type e-harness--activity-index-flush-event-types))

(defun e-harness--string-byte-prefix (text max-bytes)
  "Return TEXT prefix limited to MAX-BYTES UTF-8 bytes."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (let ((next-bytes
                       (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next-bytes) max-bytes)
                    (setq bytes (+ bytes next-bytes))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-harness--safe-tool-result-metadata (metadata preview content-text)
  "Return a narrow durable schema derived from tool METADATA."
  (let ((safe (list :activity-preview preview)))
    (dolist (key '(:tmp-uri :resource-uri :work-id :transport
                   :blocking-class :refresh-context))
      (let ((value (plist-get metadata key)))
        (when (or (stringp value) (numberp value)
                  (memq value '(t nil :json-false)))
          (setq safe
                (append safe
                        (list key (if (stringp value)
                                      (e-telemetry-redact-string value)
                                    value)))))))
    (when (plist-get preview :truncated)
      (setq safe
            (append safe
                    (list :activity-truncated t
                          :activity-original-bytes (string-bytes content-text)
                          :activity-shown-bytes
                          (plist-get preview :shown-bytes)))))
    safe))

(defun e-harness--compact-tool-result-for-activity (result)
  "Return compact redacted durable activity representation of tool RESULT."
  (let* ((content (plist-get result :content))
         (content-text (e-tools-result-content-text content))
         (redacted (e-telemetry-redact-string content-text))
         (preview (e-telemetry-preview
                   redacted
                   (max 0 e-harness-durable-tool-finished-result-preview-bytes)))
         (preview-content (if (stringp content)
                              (e-harness--string-byte-prefix
                               redacted
                               (max 0 e-harness-durable-tool-finished-result-preview-bytes))
                            (plist-get preview :content)))
         (metadata
          (e-harness--safe-tool-result-metadata
           (plist-get result :metadata) preview content-text)))
    (list :tool-call-id (plist-get result :tool-call-id)
          :name (plist-get result :name)
          :status (plist-get result :status)
          :content preview-content
          :metadata metadata)))

(defun e-harness--safe-activity-scalar (value)
  "Return VALUE safe for a narrow durable activity identity field."
  (cond
   ((stringp value) (e-telemetry-redact-string value))
   ((or (numberp value) (symbolp value) (null value)) value)
   (t nil)))

(defun e-harness--tool-call-activity-projection (call include-arguments)
  "Return a narrow durable projection of tool CALL.
When INCLUDE-ARGUMENTS is non-nil, retain only a bounded redacted preview."
  (when (listp call)
    (let ((projected
           (list :id (e-harness--safe-activity-scalar (plist-get call :id))
                 :name (e-harness--safe-activity-scalar
                        (plist-get call :name)))))
      (when (and include-arguments (plist-member call :arguments))
        (setq projected
              (append projected
                      (list :arguments
                            (e-telemetry-preview
                             (plist-get call :arguments))))))
      projected)))

(defun e-harness--tool-relation-activity-fields (payload)
  "Return named causal fields retained from tool activity PAYLOAD."
  (let (fields)
    (dolist (key '(:nested :parent-tool-call-id :depth))
      (when (and (listp payload) (plist-member payload key))
        (let ((value (e-harness--safe-activity-scalar
                      (plist-get payload key))))
          (when value
            (setq fields (append fields (list key value)))))))
    fields))

(defun e-harness--compact-tool-started-payload (payload)
  "Return a narrow redacted durable projection of tool-started PAYLOAD."
  (let* ((wrapped (and (listp payload) (plist-member payload :tool-call)))
         (call (and (listp payload)
                    (if wrapped (plist-get payload :tool-call) payload)))
         (projected (e-harness--tool-call-activity-projection call t))
         (relations (e-harness--tool-relation-activity-fields payload)))
    (if wrapped
        (append (list :tool-call projected) relations)
      (append projected relations))))

(defun e-harness--compact-tool-finished-payload (payload)
  "Return a narrow durable projection of tool-finished PAYLOAD."
  (let* ((call (and (listp payload) (plist-get payload :tool-call)))
         (result (and (listp payload) (plist-get payload :result)))
         (projected
          (append
           (list :tool-call
                 (e-harness--tool-call-activity-projection call nil))
           (e-harness--tool-relation-activity-fields payload))))
    ;; Unknown, malformed, and legacy result shapes retain only call identity
    ;; and causal fields.  They never fall back to persisting the raw payload.
    (when (e-tools-result-p result)
      (setq projected
            (append projected
                    (list :result
                          (e-harness--compact-tool-result-for-activity result)))))
    projected))

(defun e-harness--activity-field (payload key predicate &optional transform)
  "Return KEY and its PAYLOAD value when PREDICATE accepts the value.
Apply TRANSFORM when supplied."
  (let ((value (and (listp payload) (plist-get payload key))))
    (when (funcall predicate value)
      (list key (if transform (funcall transform value) value)))))

(defun e-harness--safe-activity-string (value)
  "Return redacted VALUE when it is a string."
  (and (stringp value) (e-telemetry-redact-string value)))

(defun e-harness--safe-error-activity-string (value)
  "Return redacted, bounded error string VALUE."
  (when (stringp value)
    (truncate-string-to-width
     (e-telemetry-redact-string value)
     e-telemetry-preview-max-bytes nil nil "...")))

(defun e-harness--safe-error-activity-details (value)
  "Return redacted error details VALUE, bounding unusually large values.
Ordinary compact provider plists retain their useful structure.  A large or
cyclic value becomes an explicit bounded telemetry preview instead of making a
board activity message unpublishable."
  (let ((preview (e-telemetry-preview value)))
    (if (plist-get preview :truncated)
        preview
      (e-telemetry-redact-value value))))

(defun e-harness--tool-cause-activity-projection (cause)
  "Return a narrow redacted durable projection of tool CAUSE."
  (when (listp cause)
    (append
     (e-harness--activity-field
      cause :id #'stringp #'e-harness--safe-activity-string)
     (e-harness--activity-field
      cause :name #'stringp #'e-harness--safe-activity-string))))

(defun e-harness--tool-causes-activity-projection (causes)
  "Return a vector of narrow durable tool CAUSES."
  (when (or (listp causes) (vectorp causes))
    (vconcat (delq nil
                   (mapcar #'e-harness--tool-cause-activity-projection
                           (append causes nil))))))

(defun e-harness--request-cause-activity-fields (payload)
  "Return narrow causal fields retained from provider PAYLOAD."
  (let (fields)
    (dolist (key '(:caused-by-tool-call-id :caused-by-tool-name))
      (setq fields
            (append fields
                    (e-harness--activity-field
                     payload key #'stringp #'e-harness--safe-activity-string))))
    (when-let ((causes
                (e-harness--tool-causes-activity-projection
                 (plist-get payload :caused-by-tool-calls))))
      (setq fields (append fields (list :caused-by-tool-calls causes))))
    fields))

(defun e-harness--provider-diagnostics-activity-projection (diagnostics)
  "Return named scalar provider DIAGNOSTICS safe for durable activity."
  (let (projected)
    (dolist (key '(:model :reasoning-effort :effort :response-store
                   :prompt-cache-key-present :prompt-cache-retention-present
                   :prompt-cache-mode :prompt-layout-revision
                   :provider-continuation :previous-response-id-present
                   :provider-anchor-present :input-message-count :tool-count
                   :observation-delivery :replaceable-current-state-present
                   :current-state-fingerprint :context-rendering-strategy
                   :provider-anchor-safety
                   :responses-transport :max-tokens :prompt-cache
                   :websocket-connection-id :websocket-reused
                   :websocket-reuse-count :websocket-request-mode
                   :websocket-fallback-reason :websocket-changed-properties
                   :anthropic-cache-mode :anthropic-cache-breakpoint
                   :anthropic-cache-ttl :anthropic-container-id-present))
      (when (and (listp diagnostics) (plist-member diagnostics key))
        (let ((value (e-harness--safe-activity-scalar
                      (plist-get diagnostics key))))
          (when (or value (null (plist-get diagnostics key)))
            (setq projected (append projected (list key value)))))))
    projected))

(defun e-harness--provider-request-activity-projection (payload)
  "Return a narrow redacted durable provider lifecycle PAYLOAD."
  (let (projected)
    (dolist (key '(:provider-request-id :provider :transport :url-host
                   :url-path :status))
      (when (and (listp payload) (plist-member payload key))
        (let ((value (e-harness--safe-activity-scalar (plist-get payload key))))
          (when value
            (setq projected (append projected (list key value)))))))
    (dolist (key '(:provider-request-ordinal :timeout-seconds :deadline
                   :elapsed-seconds))
      (setq projected
            (append projected
                    (e-harness--activity-field payload key #'numberp))))
    (when-let ((diagnostics
                (e-harness--provider-diagnostics-activity-projection
                 (plist-get payload :diagnostics))))
      (setq projected (append projected (list :diagnostics diagnostics))))
    (append projected (e-harness--request-cause-activity-fields payload))))

(defun e-harness--retry-activity-projection (payload)
  "Return redacted durable retry lifecycle PAYLOAD.
The retry error is durable diagnostic evidence.  Keep its structured details
while redacting credentials, and retain only the retry scheduler's scalar
fields outside that error contract."
  (let (projected)
    (setq projected
          (append projected
                  (e-harness--activity-field
                   payload :error #'stringp
                   #'e-harness--safe-error-activity-string)))
    (when (and (listp payload) (plist-member payload :details))
      (setq projected
            (append projected
                    (list :details
                          (e-harness--safe-error-activity-details
                           (plist-get payload :details))))))
    (dolist (key '(:attempt :backoff-seconds :reset-wait))
      (setq projected
            (append projected
                    (e-harness--activity-field payload key #'numberp))))
    projected))

(defun e-harness--token-usage-activity-projection (payload)
  "Return a narrow durable projection of token-usage PAYLOAD."
  (let (projected)
    (dolist (key '(:input-tokens :cached-input-tokens
                   :cache-creation-input-tokens :output-tokens
                   :reasoning-output-tokens :total-tokens
                   :provider-request-ordinal))
      (setq projected
            (append projected
                    (e-harness--activity-field payload key #'numberp))))
    (setq projected
          (append projected
                  (e-harness--activity-field
                   payload :provider-request-id #'stringp
                   #'e-harness--safe-activity-string)))
    (append projected (e-harness--request-cause-activity-fields payload))))

(defun e-harness--durable-activity-payload (type payload)
  "Return narrow durable activity PAYLOAD for event TYPE."
  (pcase type
    ('tool-started (e-harness--compact-tool-started-payload payload))
    ('tool-finished (e-harness--compact-tool-finished-payload payload))
    ((or 'provider-request-started 'provider-request-finished)
     (e-harness--provider-request-activity-projection payload))
    ('turn-retrying (e-harness--retry-activity-projection payload))
    ('token-usage (e-harness--token-usage-activity-projection payload))
    (_ payload)))

(defun e-harness--append-durable-activity-event
    (harness session-id turn-id type payload)
  "Append durable activity TYPE for HARNESS SESSION-ID TURN-ID."
  (e-harness--profile-call
   'harness.activity-append
   (list :session-id session-id
         :turn-id turn-id
         :metadata (list :event-type (and type (symbol-name type))))
   (lambda ()
     (let ((store (e-harness-sessions harness)))
       (let ((event (e-session-append-activity-event
                     store
                     session-id
                     turn-id
                     type
                     (e-harness--durable-activity-payload type payload)
                     :write-index nil)))
         (when (e-harness--activity-index-flush-event-p type)
           (e-session--write-index store))
         event)))))

(defun e-harness--emit-turn-event (harness session-id turn-id type payload)
  "Emit public event TYPE with PAYLOAD for HARNESS SESSION-ID TURN-ID."
  (let ((activity-entry
         (when (and session-id
                    turn-id
                    (e-harness--durable-activity-event-p type)
                    (ignore-errors
                      (e-session-get (e-harness-sessions harness) session-id)))
           (e-harness--append-durable-activity-event
            harness session-id turn-id type payload))))
    (e-harness--emit
     harness
     (e-events-make :type type
                    :session-id session-id
                    :turn-id turn-id
                    :payload payload
                    :activity-entry-id (plist-get activity-entry :id)
                    :board-activity-sequence
                    (plist-get activity-entry :board-activity-sequence)))))

(defun e-harness-messages (harness session-id)
  "Return messages for SESSION-ID in HARNESS."
  (e-session-messages (e-harness-sessions harness) session-id))

(defun e-harness-message-hidden-p (message)
  "Return non-nil when MESSAGE should be hidden from display.
A message is hidden when its display disposition is `hidden', set either as a
top-level `:display' (used to supersede a stored reply after the fact) or in
its `:metadata' `:display' (used when a message is queued hidden from the
start).  The value may be the symbol `hidden' or the string \"hidden\" after a
JSON replay, so both are recognized."
  (let* ((display (or (plist-get message :display)
                      (plist-get (plist-get message :metadata) :display))))
    (or (eq display 'hidden)
        (equal display "hidden"))))

(defun e-harness--queue-item-metadata (item)
  "Return turn metadata for queued ITEM."
  (append (copy-sequence (plist-get item :metadata))
          (when-let ((references (plist-get item :references)))
            (list :references references))))

(defun e-harness--queue-timestamp ()
  "Return an ISO-8601 UTC timestamp for prompt queue entries."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun e-harness-queued-prompts (harness session-id)
  "Return queued prompt items for SESSION-ID in HARNESS."
  (copy-sequence (gethash session-id (e-harness-prompt-queues harness))))

(defun e-harness-unsettled-state (harness)
  "Return HARNESS's constant-time owner-local unsettled snapshot."
  (list :generation (e-harness-unsettled-generation harness)
        :active-turns (hash-table-count (e-harness-active-turns harness))
        :queued-inputs (e-harness-queued-input-count harness)))

(defvar e-harness--aggregate-active-turn-count 0
  "Process-local count of active turns across all harnesses.")

(defvar e-harness--aggregate-queued-input-count 0
  "Process-local count of queued inputs across all harnesses.")

(defvar e-harness--aggregate-unsettled-generation 0
  "Monotonic generation of aggregate harness unsettled state.")

(defvar e-harness--aggregate-unsettled-change-functions nil
  "Hard-bounded observers of aggregate harness unsettled transitions.")

(defun e-harness-aggregate-unsettled-state ()
  "Return the constant-time process-local harness unsettled snapshot."
  (list :generation e-harness--aggregate-unsettled-generation
        :active-turns e-harness--aggregate-active-turn-count
        :queued-inputs e-harness--aggregate-queued-input-count))

(defun e-harness--adjust-aggregate-unsettled (class delta)
  "Adjust aggregate harness unsettled CLASS by DELTA."
  (let ((value
         (pcase class
           ('active-turn
            (cl-incf e-harness--aggregate-active-turn-count delta))
           ('queued-input
            (cl-incf e-harness--aggregate-queued-input-count delta))
           (_
            (signal 'e-harness-error
                    (list "Unknown aggregate unsettled class" class))))))
    (when (< value 0)
      (signal 'e-harness-error
              (list "Negative aggregate unsettled count" class value)))
    (cl-incf e-harness--aggregate-unsettled-generation)
    (run-hook-with-args 'e-harness--aggregate-unsettled-change-functions
                        (e-harness-aggregate-unsettled-state))
    value))

(defun e-harness--unsettled-changed (harness)
  "Record and publish one owner-local unsettled transition in HARNESS."
  (cl-incf (e-harness-unsettled-generation harness))
  (when-let ((function (e-harness-unsettled-change-function harness)))
    (funcall function (e-harness-unsettled-state harness))))

(defun e-harness--adjust-queued-input-count (harness delta)
  "Adjust HARNESS's queued input count by DELTA at its owning transition."
  (let ((count (cl-incf (e-harness-queued-input-count harness) delta)))
    (when (< count 0)
      (signal 'e-harness-error (list "Negative queued input count" count)))
    (unless (= delta 0)
      (e-harness--adjust-aggregate-unsettled 'queued-input delta)
      (e-harness--unsettled-changed harness))
    count))

(defun e-harness--put-active-turn (harness session-id entry)
  "Install ENTRY as SESSION-ID's active turn and publish the transition."
  (when (gethash session-id (e-harness-active-turns harness))
    (signal 'e-harness-active-turn-exists (list session-id)))
  (puthash session-id entry (e-harness-active-turns harness))
  (e-harness--adjust-aggregate-unsettled 'active-turn 1)
  (e-harness--unsettled-changed harness)
  entry)

(defun e-harness--remove-active-turn (harness session-id &optional expected)
  "Remove SESSION-ID's active turn when it matches EXPECTED, if supplied."
  (let ((current (gethash session-id (e-harness-active-turns harness))))
    (when (and current (or (null expected) (eq current expected)))
      (remhash session-id (e-harness-active-turns harness))
      (e-harness--adjust-aggregate-unsettled 'active-turn -1)
      (e-harness--unsettled-changed harness)
      current)))

(defun e-harness--set-queued-prompts (harness session-id items &optional delta)
  "Replace SESSION-ID queued prompt ITEMS in HARNESS by known DELTA.
Clearing a queue may omit DELTA because its owner-local count is indexed."
  (let* ((counts (e-harness-prompt-queue-counts harness))
         (old-count (or (gethash session-id counts) 0))
         (delta (or delta
                    (and (null items) (- old-count))
                    (signal 'e-harness-error
                            (list "Queued prompt delta is required"
                                  session-id))))
         (new-count (+ old-count delta)))
    (when (< new-count 0)
      (signal 'e-harness-error
              (list "Negative session prompt queue count" session-id new-count)))
    (if items
        (progn
          (puthash session-id items (e-harness-prompt-queues harness))
          (puthash session-id new-count counts))
      (remhash session-id (e-harness-prompt-queues harness))
      (remhash session-id counts))
    (e-harness--adjust-queued-input-count harness delta))
  items)

(defun e-harness-discard-queued-board-input
    (harness session-id delivery-id endpoint-token endpoint-generation reason)
  "Discard HARNESS SESSION-ID's exact queued board head with REASON.
Only the first queued item is inspected, keeping reconciliation bounded and
preserving FIFO.  DELIVERY-ID, ENDPOINT-TOKEN, and ENDPOINT-GENERATION must all
match the immutable metadata accepted with that item.  Return the removed item,
or nil without changing the queue when the head belongs to another delivery."
  (let* ((items (e-harness-queued-prompts harness session-id))
         (item (car items))
         (metadata (and item (plist-get item :metadata))))
    (when (and (equal (plist-get metadata :board-delivery-id) delivery-id)
               (equal (plist-get metadata :board-endpoint-token) endpoint-token)
               (equal (plist-get metadata :board-endpoint-generation)
                      endpoint-generation))
      (e-harness--set-queued-prompts harness session-id (cdr items) -1)
      (e-harness--emit-queue-changed harness session-id)
      (e-harness--emit
       harness
       (e-events-make
        :type 'input-discarded :session-id session-id :turn-id nil
        :payload
        (list :delivery-id (copy-tree delivery-id)
              :endpoint-token
              (if (vectorp endpoint-token)
                  (copy-sequence endpoint-token)
                (copy-tree endpoint-token))
              :endpoint-generation (copy-tree endpoint-generation)
              :queue-id (plist-get item :id)
              :reason reason)))
      item)))

(defun e-harness--emit-queue-changed (harness session-id)
  "Emit a queue update event for SESSION-ID."
  (e-harness--emit
   harness
   (e-events-make :type 'queue-changed
                  :session-id session-id
                  :turn-id nil
                  :payload (list :queue
                                 (e-harness-queued-prompts
                                  harness session-id)))))

(defun e-harness--enqueue-prompt-item
    (harness session-id prompt references metadata)
  "Append PROMPT to SESSION-ID's follow-up queue in HARNESS and return its id.
Shared enqueue body with no active-turn guard for attached queue and settlement
follow-up ports."
  (let* ((queue-id (e-session-generate-ulid))
         (item (list :id queue-id
                     :prompt prompt
                     :references (copy-tree references)
                     :metadata (copy-sequence metadata)
                     :created-at (e-harness--queue-timestamp)))
         (items (append (e-harness-queued-prompts harness session-id)
                        (list item))))
    (e-harness--set-queued-prompts harness session-id items 1)
    (e-harness--emit-queue-changed harness session-id)
    queue-id))

(cl-defun e-harness--request-attached-follow-up
    (harness session-id prompt &key references metadata)
  "Queue PROMPT as a follow-up during turn settlement, then return its id.
This is valid from a `:turn-finished' hook, whose turn is already settling.  The
queued prompt is picked up by the normal post-settlement drain
(`e-harness--drain-next-queued-prompt') that runs after the finished turn's
hooks complete, so the drain path stays the single owner of turn scheduling."
  (let* ((entry (gethash session-id (e-harness-active-turns harness)))
         (token (and (listp entry) (plist-get entry :endpoint-token))))
    (e-harness--require-attached-port harness session-id token)
    (setq metadata (plist-put (copy-sequence metadata)
                              :board-endpoint-token token)))
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (let ((metadata (copy-sequence metadata)))
    (setq metadata (plist-put metadata :input-origin 'harness))
    (e-harness--enqueue-prompt-item
     harness session-id prompt references metadata)))

(cl-defun e-harness--publish-attached-follow-up
    (harness session-id prompt &key references metadata tags)
  "Publish PROMPT as an attached settlement follow-up with routing TAGS.
This is the capability-facing continuation port for a `:turn-finished' hook.
The harness verifies the settling attachment but does not own interaction
routing.  Its runtime adapter must publish the follow-up through the owning
board, whose delivery path later starts the new turn."
  (let* ((entry (gethash session-id (e-harness-active-turns harness)))
         (token (and (listp entry) (plist-get entry :endpoint-token))))
    (e-harness--require-attached-port harness session-id token))
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (unless e-harness--attached-follow-up-publisher
    (signal 'e-harness-board-attachment-required (list session-id)))
  (funcall e-harness--attached-follow-up-publisher
           harness session-id prompt
           :references (copy-tree references)
           :metadata (copy-sequence metadata)
           :tags (copy-tree tags)))

(cl-defun e-harness--queue-attached-prompt
    (harness session-id prompt &key references metadata attachment-token)
  "Queue PROMPT as a follow-up for SESSION-ID in HARNESS.
The session must currently have a running active turn."
  (e-harness--require-attached-port harness session-id attachment-token)
  (setq metadata (plist-put (copy-sequence metadata)
                            :board-endpoint-token attachment-token))
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (unless (e-harness--active-turn-running-p
           (gethash session-id (e-harness-active-turns harness)))
    (signal 'e-harness-no-active-turn (list session-id)))
  (let* ((queue-id (e-session-generate-ulid))
         (item (list :id queue-id
                     :prompt prompt
                     :references (copy-tree references)
                     :metadata (copy-sequence metadata)
                     :created-at (e-harness--queue-timestamp)))
         (items (append (e-harness-queued-prompts harness session-id)
                        (list item))))
    (e-harness--set-queued-prompts harness session-id items 1)
    (e-harness--emit-queue-changed harness session-id)
    queue-id))

(defun e-harness--steering-prompt-preview (prompt)
  "Return compact activity preview for steering PROMPT."
  (let ((text (string-trim
               (replace-regexp-in-string "[\n\r\t ]+" " " prompt))))
    (e-harness--string-byte-prefix text 160)))

(defun e-harness--durable-input-metadata (metadata)
  "Return durable input METADATA without the live attachment fence.
The endpoint token authorizes one process-local delivery attempt.  It remains
available to the delivery and receipt paths, but must not enter transcript or
activity persistence."
  (e-session--plist-remove
   (copy-sequence metadata) :board-endpoint-token))

(defun e-harness--pending-steering-items (entry)
  "Return pending steering items from active turn ENTRY."
  (and (listp entry)
       (plist-get entry :pending-steering-input)))

(defun e-harness--append-pending-steering-item (harness entry prompt metadata)
  "Append PROMPT and METADATA as pending steering input on HARNESS ENTRY."
  (plist-put entry
             :pending-steering-input
             (append (e-harness--pending-steering-items entry)
                     (list (list :prompt prompt
                                 :metadata (copy-sequence metadata)))))
  (plist-put entry :pending-steering-count
             (1+ (or (plist-get entry :pending-steering-count) 0)))
  (e-harness--adjust-queued-input-count harness 1)
  entry)

(defun e-harness--drain-pending-steering-input (harness entry)
  "Return and clear pending steering items from HARNESS active turn ENTRY."
  (let ((items (e-harness--pending-steering-items entry))
        (count (or (plist-get entry :pending-steering-count) 0)))
    (when items
      (plist-put entry :pending-steering-input nil)
      (plist-put entry :pending-steering-count 0)
      (e-harness--adjust-queued-input-count harness (- count))
      items)))

(cl-defun e-harness--steer-attached-turn
    (harness session-id prompt &key metadata attachment-token)
  "Steer SESSION-ID's running active turn with PROMPT in HARNESS."
  (e-harness--require-attached-port harness session-id attachment-token)
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (unless (e-harness--active-turn-running-p entry)
      (signal 'e-harness-no-active-turn (list session-id)))
    (let ((turn-id (plist-get entry :id))
          (metadata (e-harness--durable-input-metadata metadata)))
      (e-harness--append-pending-steering-item harness entry prompt metadata)
      (e-harness--emit-turn-event
       harness session-id turn-id 'turn-steered
       (list :prompt-preview (e-harness--steering-prompt-preview prompt)
             :metadata (copy-sequence metadata)))
      turn-id)))

(defun e-harness--drain-next-queued-prompt (harness session-id settled-entry)
  "Start SESSION-ID's next queued prompt after SETTLED-ENTRY clears."
  (let ((current-entry (gethash session-id (e-harness-active-turns harness))))
    (when (and current-entry
               (not (e-harness--active-turn-running-p current-entry))
               (eq current-entry settled-entry))
      (e-harness--remove-active-turn harness session-id settled-entry))
    (unless (e-harness--active-turn-running-p
             (gethash session-id (e-harness-active-turns harness)))
      (when-let ((item (car (e-harness-queued-prompts harness session-id))))
        (e-harness--set-queued-prompts
         harness session-id
         (cdr (e-harness-queued-prompts harness session-id)) -1)
        (e-harness--emit-queue-changed harness session-id)
        (e-harness--prompt-attached-async
         harness
         session-id
         (plist-get item :prompt)
         :metadata (e-harness--queue-item-metadata item)
         :attachment-token
         (plist-get (plist-get item :metadata) :board-endpoint-token))))))

(defun e-harness--schedule-queue-drain (harness session-id settled-entry)
  "Schedule queue drain for SESSION-ID after SETTLED-ENTRY settles."
  (run-at-time 0 nil
               (lambda ()
                 (e-harness--drain-next-queued-prompt
                  harness session-id settled-entry))))

(defun e-harness-session-title (harness session-id)
  "Return display title for SESSION-ID in HARNESS."
  (e-session-display-title (e-harness-sessions harness) session-id))

(defun e-harness-session-name (harness session-id)
  "Return explicit name for SESSION-ID in HARNESS, or nil."
  (plist-get (e-session-get (e-harness-sessions harness) session-id) :name))

(defun e-harness-session-list (harness)
  "Return display metadata for sessions owned by HARNESS."
  (e-session-list (e-harness-sessions harness)))

(defun e-harness-root-session-list (harness)
  "Return user-facing root sessions owned by HARNESS."
  (e-session-list-roots (e-harness-sessions harness)))

(defun e-harness-session-activity-events (harness session-id)
  "Return activity events for SESSION-ID in HARNESS."
  (e-session-activity-events (e-harness-sessions harness) session-id))

(cl-defun e-harness-record-hook-audit
    (harness session-id turn-id
             &key owner hook-id outcome details summary pending-summary)
  "Persist one capability hook audit outcome for a settled turn.

OWNER names the capability, HOOK-ID identifies its hook contract, OUTCOME is a
machine-readable result owned by that capability, DETAILS is an opaque plist or
alist owned by the capability, SUMMARY is optional generic presentation text,
and PENDING-SUMMARY is optional activity text to retain while a capability's
queued follow-up replaces the reply.  Core owns only the durable event envelope;
it must not interpret a capability's policy or mistake an audit outcome for a
truth judgment.  Returns the emitted event.

Callers should record an outcome only when their hook actually checked a turn.
This keeps audit volume proportional to conditional enforcement rather than to
all ordinary replies."
  (unless (and (symbolp owner) (not (keywordp owner)))
    (signal 'wrong-type-argument (list 'symbolp owner)))
  (unless (and (stringp hook-id) (not (string-empty-p hook-id)))
    (signal 'wrong-type-argument (list 'stringp hook-id)))
  (unless (symbolp outcome)
    (signal 'wrong-type-argument (list 'symbolp outcome)))
  (let ((payload (list :owner owner
                       :hook-id hook-id
                       :outcome outcome
                       :truth-status 'not-evaluated
                       :summary summary
                       :pending-summary pending-summary
                       :details details)))
    (e-harness--emit-turn-event harness session-id turn-id 'hook-audit payload)
    (car (last (e-harness-session-activity-events harness session-id)))))

(defun e-harness-turn-hook-audits (harness session-id turn-id &optional owner)
  "Return durable hook-audit records for TURN-ID, optionally filtered by OWNER."
  (seq-filter
   (lambda (event)
     (and (eq (plist-get event :event-type) 'hook-audit)
          (equal (plist-get event :turn-id) turn-id)
          (or (null owner)
              (eq (plist-get (plist-get event :payload) :owner) owner))))
   (e-harness-session-activity-events harness session-id)))

(defun e-harness--merge-turn-options (base overrides)
  "Return BASE options with OVERRIDES applied."
  (let ((options (copy-sequence base))
        (remaining overrides))
    (while remaining
      (setq options (plist-put options (pop remaining) (pop remaining))))
    options))

(defun e-harness-session-options (harness session-id)
  "Return session-specific turn options for SESSION-ID in HARNESS."
  (e-session-turn-options (e-harness-sessions harness) session-id))

(defun e-harness-display-options (harness session-id)
  "Return lightweight display options for HARNESS SESSION-ID.
This merges default and session options without deriving prompt-cache keys or
materializing tool definitions.  Presentation code uses it for status text."
  (e-harness--merge-turn-options
   (e-harness-default-options harness)
   (e-harness-session-options harness session-id)))

(defun e-harness--set-session-options (harness session-id options)
  "Replace SESSION-ID turn OPTIONS in HARNESS and emit an update event."
  (let ((turn-options
         (e-session-set-turn-options
          (e-harness-sessions harness)
          session-id
          options)))
    (e-harness--emit
     harness
     (e-events-make :type 'session-options-changed
                    :session-id session-id
                    :turn-id "session-options"
                    :payload (list :turn-options turn-options)))
    turn-options))

(defun e-harness-set-session-model (harness session-id model)
  "Set SESSION-ID's model override to MODEL in HARNESS."
  (let ((options (copy-sequence (e-harness-session-options harness session-id))))
    (if (and (stringp model) (not (string-empty-p (string-trim model))))
        (setq options (plist-put options :model (string-trim model)))
      (cl-remf options :model))
    (e-harness--set-session-options harness session-id options)))

(defun e-harness-set-session-reasoning-effort (harness session-id effort)
  "Set SESSION-ID's reasoning EFFORT override in HARNESS."
  (let ((options (copy-sequence (e-harness-session-options harness session-id))))
    (if (and (stringp effort) (not (string-empty-p (string-trim effort))))
        (setq options (plist-put options :reasoning-effort (string-trim effort)))
      (cl-remf options :reasoning-effort))
    (e-harness--set-session-options harness session-id options)))

(defconst e-harness-prompt-cache-key-version "pcctx2"
  "Version marker for derived prompt cache keys.")

(defconst e-harness-prompt-cache-key-max-length 64
  "Maximum length of an OpenAI-compatible prompt cache key.")

(defun e-harness--prompt-cache-hash (value length)
  "Return a deterministic LENGTH-character hash for VALUE."
  (substring (secure-hash 'sha256 (format "%S" value)) 0 length))

(defun e-harness--effective-layer-id-strings (harness &optional session-id turn-id)
  "Return JSON-stable effective layer ids for HARNESS SESSION-ID TURN-ID."
  (mapcar #'symbol-name
          (e-harness-effective-layer-ids harness session-id turn-id)))

(defun e-harness--derived-prompt-cache-key (harness session-id options)
  "Return the default prompt cache key for HARNESS SESSION-ID OPTIONS.
The active tool set participates in the key so mid-session tool activation
\(e.g. MCP progressive disclosure) does not silently reuse a cache prefix
built without those tools."
  (let* ((prefix (format "e:%s:" e-harness-prompt-cache-key-version))
         (identity
          (list :model (plist-get options :model)
                :project-root (e-harness-project-root harness session-id)
                :layer-ids
                (e-harness-effective-layer-ids harness session-id)
                :tool-names
                (mapcar (lambda (tool) (plist-get tool :name))
                        (or (plist-get options :tools)
                            (e-tools-definitions
                             (e-harness-tools harness session-id)))))))
    (concat prefix
            (e-harness--prompt-cache-hash
             identity
             (- e-harness-prompt-cache-key-max-length (length prefix))))))

(defun e-harness--apply-prompt-cache-defaults (harness session-id options)
  "Apply opt-in prompt cache defaults to OPTIONS."
  (let ((options (copy-sequence options)))
    (when (and (plist-member options :prompt-cache-key)
               (null (plist-get options :prompt-cache-key)))
      (cl-remf options :prompt-cache-key))
    (when (and (plist-get options :prompt-cache-default)
               (not (plist-member options :prompt-cache-key)))
      (setq options
            (plist-put options
                       :prompt-cache-key
                       (e-harness--derived-prompt-cache-key
                        harness
                        session-id
                        options))))
    (cl-remf options :prompt-cache-default)
    (unless (plist-member options :prompt-cache-key)
      (cl-remf options :prompt-cache-retention))
    options))

(defun e-harness--apply-deadline-default (options)
  "Attach a provider request deadline to OPTIONS when none is explicit."
  (let ((options (copy-sequence options)))
    (when (and e-harness-provider-request-deadline-seconds
               (not (plist-member options :deadline)))
      (setq options
            (plist-put options
                       :deadline
                       (+ (float-time)
                          e-harness-provider-request-deadline-seconds))))
    options))

(defun e-harness-turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID.
Tool definitions are attached before deriving the prompt cache key so the key
reflects the active tool set."
  (let* ((merged (e-harness--merge-turn-options
                  (e-harness-default-options harness)
                  (e-harness-session-options harness session-id)))
         (tool-definitions
          (e-tools-definitions (e-harness-tools harness session-id)))
         (with-tools (if tool-definitions
                         (plist-put merged :tools tool-definitions)
                       merged)))
    (e-harness--apply-deadline-default
     (e-harness--apply-prompt-cache-defaults harness session-id with-tools))))

(defun e-harness--turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID."
  (e-harness-turn-options harness session-id))

(defun e-harness--options-without-tools (options)
  "Return OPTIONS with any tool set removed.
Used for backend requests that must produce plain text (e.g. context
compaction) where exposing tools risks a tool-call instead of a reply."
  (let ((copy (copy-sequence options)))
    (setq copy (plist-put copy :tools nil))
    copy))

(defun e-harness--nested-tool-payload
    (tool-call parent-tool-call depth &rest extra)
  "Return nested tool event payload for TOOL-CALL under PARENT-TOOL-CALL."
  (append
   (list :tool-call tool-call
         :nested t
         :parent-tool-call-id (plist-get parent-tool-call :id)
         :depth depth)
   extra
   (when (listp (plist-get tool-call :metadata))
	     (plist-get tool-call :metadata))))

(defun e-harness--tool-blocking-class (tool)
  "Return TOOL blocking class metadata."
  (let ((metadata (plist-get tool :metadata)))
    (or (plist-get metadata :blocking-class)
        (plist-get metadata :blocking_class)
        (plist-get metadata :blocking))))

(defun e-harness--nested-long-tool-result (tool-call tool)
  "Return a structured error result for long nested TOOL-CALL."
  (let ((class (e-harness--tool-blocking-class tool))
        (name (plist-get tool-call :name)))
    (e-tools-result-create
     tool-call
     'error
     (format
      "Nested tool %s is %s-class and cannot run synchronously inside another tool; call it as a top-level tool instead."
      name class)
     (list :error 'e-nested-long-tool-rejected
           :blocking-class class))))

(defun e-harness--execute-nested-tool
    (harness session-id turn-id tools tool-call _options parent-context)
  "Execute nested TOOL-CALL for HARNESS and return a structured result."
  (let* ((hooks (e-harness-hooks harness))
         (parent-tool-call (plist-get parent-context :tool-call))
         (depth (1+ (or (plist-get parent-context :depth) 0)))
         (context (e-harness--tool-hook-context
                   harness session-id turn-id tools parent-context depth))
         (prepared
          (e-hooks-run-reduce hooks :pre-tool-call tool-call context))
         (tool (gethash (plist-get prepared :name)
                        (e-tools-registry-tools tools)))
         result)
    (e-harness--emit-turn-event
     harness
     session-id
     turn-id
     'tool-started
     (e-harness--nested-tool-payload
      prepared parent-tool-call depth))
    (setq result
          (if (and tool
                   (e-tools-long-blocking-class-p
                    (e-harness--tool-blocking-class tool)))
              (e-harness--nested-long-tool-result prepared tool)
            (e-tools--execute-nested-cheap-with-context
             tools prepared context)))
    (setq result
          (e-hooks-run-reduce hooks :post-tool-call result context))
    (e-harness--emit-turn-event
     harness
     session-id
     turn-id
     'tool-finished
     (e-harness--nested-tool-payload
      prepared parent-tool-call depth :result result))
    result))

(defun e-harness--tool-hook-context
    (harness session-id turn-id tools &optional parent-context depth)
  "Return the narrow hook context for a tool lifecycle in HARNESS."
  (let* ((turn-options (ignore-errors
                         (e-harness-turn-options harness session-id)))
          (turn-work (plist-get (gethash session-id
                                          (e-harness-active-turns harness))
                                :work-handle))
          (context
           (list :harness harness
                 :session-id session-id
                 :turn-id turn-id
                 :parent-work-id (and (e-work-handle-p turn-work)
                                      (e-work-handle-id turn-work))
                 :root-work-id (and (e-work-handle-p turn-work)
                                    (e-work-handle-id turn-work))
                  :deadline (plist-get turn-options :deadline)
                  :board-subscribe-aggregation
                  (e-harness-board-aggregation-function harness)
                 :tools tools
                :capabilities (e-harness-active-capabilities harness)
                :tool-executor
                (lambda (tool-call options current-context)
                  (e-harness--execute-nested-tool
                   harness
                   session-id
                   turn-id
                   tools
                   tool-call
                   options
                   current-context)))))
    (when parent-context
      (setq context
            (append
             (list :nested t
                   :parent-tool-call (plist-get parent-context :tool-call)
                   :parent-tool-call-id
                   (plist-get (plist-get parent-context :tool-call) :id)
                   :depth depth)
             context)))
    context))

(defun e-harness-tool-lifecycle (harness session-id turn-id)
  "Return a harness-owned tool lifecycle for SESSION-ID and TURN-ID."
  (let ((tools (e-harness-tools harness session-id turn-id))
        hook-context)
    (cl-labels
        ((hooks ()
           (e-harness-hooks harness))
         (context ()
           (or hook-context
               (setq hook-context
                     (e-harness--tool-hook-context
                      harness session-id turn-id tools)))))
      (e-tool-lifecycle-create
       :prepare (lambda (tool-call)
                  (e-hooks-run-reduce
                   (hooks)
                   :pre-tool-call
                   tool-call
                   (context)))
       :start
       (cl-function
         (lambda (tool-call &key on-request-start on-done on-error on-event
                            on-work-prepared)
          (e-harness--profile-call
           'harness.tool-start
           (list :session-id session-id
                 :turn-id turn-id
                 :metadata (list :tool-name (plist-get tool-call :name)))
           (lambda ()
             (e-tools-start
              tools
              tool-call
              :context (context)
               :on-request-start on-request-start
               :on-work-prepared on-work-prepared
               :on-event on-event
              :on-done
              (lambda (result)
                (condition-case err
                    (when on-done
                      (funcall on-done
                               (e-hooks-run-reduce
                                (hooks)
                                :post-tool-call
                                result
                                (context))))
                  (error
                   (if on-error
                       (funcall on-error err)
                     (signal (car err) (cdr err))))))
              :on-error on-error)))))))))

(defun e-harness-context
    (harness session-id &optional turn-id context-purpose)
  "Return backend-neutral context for SESSION-ID in HARNESS.
TURN-ID is passed to active capability context providers when present.
CONTEXT-PURPOSE may be `turn' for correctness-critical provider turns,
`preview' for explicit user-requested context inspection, or `status',
`snapshot', or `optional' for callers that must not perform correctness-critical
turn context work."
  (e-harness--profile-call
   'harness.context
   (list :session-id session-id
         :turn-id turn-id
         :metadata (list :context-purpose context-purpose))
   (lambda ()
     (let ((capability-context
            (e-capabilities-context
             (e-harness-effective-capabilities harness session-id turn-id)
             :harness harness
             :session-id session-id
             :turn-id turn-id
             :context-purpose context-purpose)))
       (let* ((turn-options
               (e-harness--strip-reserved-derived-context-options
                (e-harness-turn-options harness session-id)))
         (context-capabilities
               (e-harness--context-capabilities harness turn-options))
              (context
               (e-context-build
                (e-harness-context-strategy harness)
                :sessions (e-harness-sessions harness)
                :session-id session-id
                :options turn-options
                :prefix-messages (plist-get capability-context :messages)
                :prefix-segments (plist-get capability-context :segments))))
         (when (e-harness--context-lifetime-enabled-p context-purpose)
           (setq context
                 (e-harness--context-lifetime-apply-projection
                  harness session-id turn-id context context-capabilities)))
         (plist-put context
                    :provider-anchor-active-layer-ids
                    (e-harness--effective-layer-id-strings
                     harness session-id turn-id))
         (plist-put context
                    :provider-anchor-compaction-boundary
                    (e-harness--provider-anchor-compaction-boundary
                     harness session-id))
         (e-harness--context-observation-frontier
          context
          context-capabilities)
         (e-harness--context-with-segment-message-boundary context)
         (setq context
               (e-harness--context-with-provider-anchor
                harness
                session-id
                context))
         (setq context
               (e-harness--context-with-provider-compaction
                harness session-id context context-purpose))
         (e-harness--context-with-continuation-projection-identity context))))))

(defun e-harness-turn-context (harness session-id turn-id)
  "Return correctness-critical model context for TURN-ID.
Unlike preview, status, snapshot, or optional context, turn context must include
the live dynamic providers needed for the model-facing request."
  (e-harness-context harness session-id turn-id 'turn))

(defun e-harness--active-turn-id (entry)
  "Return active turn id from ENTRY."
  (if (listp entry)
      (plist-get entry :id)
    entry))

(defun e-harness--active-turn-running-p (entry)
  "Return non-nil when active turn ENTRY is still running."
  (and (listp entry)
       (eq (plist-get entry :status) 'running)))

(defun e-harness--provider-compaction-context
    (harness session-id generation)
  "Return a stable optional CONTEXT for provider compaction at GENERATION."
  (let ((context (e-harness-context harness session-id nil 'optional)))
    (plist-put context :lifetime-generation generation)
    (plist-put context
               :options
               (e-harness--strip-reserved-derived-context-options
                (plist-get context :options)))
    context))

(defun e-harness--provider-compaction-messages
    (harness session-id generation)
  "Return portable messages covered by GENERATION for provider compaction."
  (e-compaction-portable-context-messages
   (e-harness-sessions harness)
   session-id
   (e-context-lifetime-generation-checkpoint generation)
   (e-context-lifetime-generation-covered-session-boundary generation)))

(defun e-harness--provider-compaction-input
    (harness session-id generation)
  "Capture provider compaction MESSAGES and their exact coverage identity."
  (let* ((messages (e-harness--provider-compaction-messages
                    harness session-id generation))
         (projection (e-session-context-lifetime-projection
                      (e-harness-sessions harness) session-id))
         (frontier (mapcar #'e-context-lifetime-promotion-id
                           (plist-get projection :promotions)))
         (source-entry-id
          (e-harness--provider-compaction-candidate-source-entry-id
           harness session-id)))
    (list :messages messages
          :source-entry-id source-entry-id
          :promotion-frontier frontier
          :input-fingerprint
          (secure-hash 'sha256
                       (prin1-to-string
                        (list messages source-entry-id frontier))))))

(defun e-harness--provider-compaction-store-result
    (harness session-id context generation result input)
  "Store validated provider RESULT as a runtime candidate."
  (e-harness--provider-compaction-store-candidate
   harness session-id context generation
   (e-backend-provider-compaction-result-output result)
   (e-backend-provider-compaction-result-usage result)
   (plist-get input :source-entry-id)
   (plist-get input :promotion-frontier)
   (plist-get input :input-fingerprint)))

(defun e-harness--maybe-provider-compaction-batch
    (harness session-id portable-generation)
  "Best-effort synchronous provider compaction after PORTABLE-GENERATION.

Canonical local compaction is already complete when this function runs.  Any
provider failure only discards acceleration and never changes session state."
  (when portable-generation
    (condition-case _error
        (let* ((generation
                (e-context-lifetime-generation-from-record
                 (e-session--context-record portable-generation)))
               (context
                (e-harness--provider-compaction-context
                 harness session-id generation))
               (options (plist-get context :options))
               (capabilities (e-harness--context-capabilities harness options)))
          (when (eq (plist-get capabilities :provider-compaction) 'opaque)
            (let ((input (e-harness--provider-compaction-input
                          harness session-id generation)))
              (e-harness--provider-compaction-store-result
               harness session-id context generation
               (e-backend-provider-compaction-batch
                (e-harness-backend harness)
                :messages (plist-get input :messages)
                :options (plist-put (copy-sequence options)
                                    :provider-compaction-boundary t))
               input))))
      (error nil))))

(defun e-harness--maybe-provider-compaction-start
    (harness session-id portable-generation)
  "Best-effort asynchronous provider compaction after PORTABLE-GENERATION."
  (when portable-generation
    (condition-case _error
        (let* ((generation
                (e-context-lifetime-generation-from-record
                 (e-session--context-record portable-generation)))
               (context
                (e-harness--provider-compaction-context
                 harness session-id generation))
               (options (plist-get context :options))
               (capabilities (e-harness--context-capabilities harness options)))
          (when (eq (plist-get capabilities :provider-compaction) 'opaque)
            (let ((input (e-harness--provider-compaction-input
                          harness session-id generation)))
              (e-backend-provider-compaction-start
               (e-harness-backend harness)
               :messages (plist-get input :messages)
               :options (plist-put (copy-sequence options)
                                   :provider-compaction-boundary t)
               :on-done
               (lambda (result)
                 (e-harness--provider-compaction-store-result
                  harness session-id context generation result input))
               :on-error #'ignore))))
      (error nil))))

(cl-defun e-harness-compact-session-batch
    (harness session-id &key instructions keep-recent-tokens allow-active-turn
             (allow-split-turn 'inherit-active-turn) exclude-entry-ids turn-id
             (reason 'manual))
  "Synchronously compact SESSION-ID in HARNESS from batch/test code."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-harness-compact-session-batch))
  (when (and (not allow-active-turn)
             (e-harness--active-turn-running-p
              (gethash session-id (e-harness-active-turns harness))))
    (signal 'e-harness-active-turn-exists (list session-id)))
  (let ((turn-id (or turn-id (e-harness--next-turn-id)))
        preparation
        summary-parts
        summary-message
        summary-item-types
        request)
    (condition-case err
        (progn
          (e-harness--emit-turn-event
           harness session-id turn-id 'compaction-started
           (list :instructions instructions
                 :reason reason
                 :active-turn allow-active-turn))
          (setq preparation
                (e-compaction-prepare
                 (e-harness-sessions harness)
                 session-id
                 :instructions instructions
                 :keep-recent-tokens keep-recent-tokens
                 :allow-split-turn (if (eq allow-split-turn
                                            'inherit-active-turn)
                                       allow-active-turn
                                     allow-split-turn)
                 :exclude-entry-ids exclude-entry-ids
                 :reason reason
                 :portable e-context-lifetime-shadow-projection-enabled))
          (e-harness--emit-turn-event
           harness session-id turn-id 'compaction-prepared
           (list :first-kept-entry-id
                 (plist-get preparation :first-kept-entry-id)
                 :reason reason
                 :tokens-before (plist-get preparation :tokens-before)
                 :tokens-kept (plist-get preparation :tokens-kept)))
          (e-harness--emit-turn-event
           harness session-id turn-id 'compaction-summary-started
           (list :backend t :reason reason))
          (e-backend-stream-batch
           (e-harness-backend harness)
           :messages (e-compaction-prepared-summary-messages preparation)
           ;; Summarization is a pure text task.  Strip the tool set from the
           ;; options: with tools present the model may answer with a tool-call
           ;; instead of an assistant message, yielding an empty summary and a
           ;; spurious compaction failure.
           :options (e-harness--options-without-tools
                     (e-harness-turn-options harness session-id))
           :on-request-start (lambda (value)
                               (setq request value))
           :on-item
           (lambda (item)
             (push (plist-get item :type) summary-item-types)
             (pcase (plist-get item :type)
               ('assistant-message
                (setq summary-message (plist-get item :content)))
               ('assistant-delta
                (push (or (plist-get item :content) "") summary-parts)))))
          (let* ((summary (string-trim
                           (or summary-message
                               (string-join (nreverse summary-parts) ""))))
                 (metadata (plist-get preparation :metadata)))
            (when (string-empty-p summary)
              (signal 'e-compaction-error
                      (list
                       "Compaction backend returned an empty summary"
                       (list :request-started
                             (and request t)
                             :item-types
                             (nreverse (delq nil summary-item-types))
                             :summary-source
                             'none))))
            (let* ((portable-preparation
                    (plist-get preparation :portable-input))
                   (portable-checkpoint
                    (when portable-preparation
                      (e-compaction-portable-checkpoint-from-summary
                       preparation summary)))
                   ;; Preflight both semantic inputs before the legacy
                   ;; compaction append, so stale generation/promotion state
                   ;; cannot leave a partial ordinary-only mutation.
                   (portable-application
                    (when portable-preparation
                      (e-compaction-preflight-portable-boundary
                       (e-harness-sessions harness)
                       session-id
                       preparation
                       portable-checkpoint)))
                   (record
                    (e-session-append-compaction
                     (e-harness-sessions harness)
                     session-id
                     summary
                     :first-kept-entry-id
                     (plist-get preparation :first-kept-entry-id)
                     :tokens-before (plist-get preparation :tokens-before)
                     :tokens-kept (plist-get preparation :tokens-kept)
                     :metadata metadata))
                   ;; Preserve the established compaction record as the
                   ;; audit/legacy owner, then deliberately supersede its
                   ;; model prefix with a portable generation only when the
                   ;; semantic lifetime feature is opted in.
                   (portable-generation
                     (when portable-application
                      (e-compaction-apply-portable-boundary
                       (e-harness-sessions harness)
                       session-id
                       portable-application))))
              (e-harness--maybe-provider-compaction-batch
               harness session-id portable-generation)
              (e-harness--emit-turn-event
               harness session-id turn-id 'compaction-finished
               (list :compaction-id (plist-get record :id)
                     :portable-generation-id
                     (and portable-generation
                          (e-context-lifetime-generation-id
                           (e-context-lifetime-generation-from-record
                            (e-session--context-record portable-generation))))
                     :reason (plist-get (plist-get record :metadata) :reason)
                     :first-kept-entry-id
                     (plist-get record :first-kept-entry-id)
                     :tokens-before (plist-get record :tokens-before)
                     :tokens-kept (plist-get record :tokens-kept)))
              record)))
      (error
       (let ((message (e-harness--backend-error-message err))
             (details (e-harness--backend-error-details err)))
         (when (and request (e-backend-request-p request))
           (ignore-errors (e-backend-cancel-request request)))
         (e-harness--emit-turn-event
          harness session-id turn-id 'compaction-failed
          (list :message message :details details :reason reason))
	         (signal (car err) (cdr err)))))))

(defun e-harness--backend-work-request-metadata (handle)
  "Return provider-facing request metadata projected from backend work HANDLE."
  (when (e-work-handle-p handle)
    (let* ((metadata (e-work-handle-metadata handle))
           (backend-metadata
            (copy-sequence
             (or (plist-get metadata :backend-request-metadata) nil))))
      (append backend-metadata
              (list :work-id (e-work-handle-id handle)
                    :work-handle handle
                    :work-transport (plist-get metadata :transport)
                    :backend-request
                    (plist-get metadata :backend-request))))))

(cl-defun e-harness-compact-session-start
    (harness session-id &key instructions keep-recent-tokens allow-active-turn
             (allow-split-turn 'inherit-active-turn) exclude-entry-ids turn-id
             (reason 'manual) on-done on-error)
  "Start compacting SESSION-ID in HARNESS and return a cancellable request.
ON-DONE receives the durable compaction record.  ON-ERROR receives an Emacs
condition list.  Preparation errors are reported before return by signaling and
also emitting the normal compaction failure event."
  (when (and (not allow-active-turn)
             (e-harness--active-turn-running-p
              (gethash session-id (e-harness-active-turns harness))))
    (signal 'e-harness-active-turn-exists (list session-id)))
  (let ((turn-id (or turn-id (e-harness--next-turn-id)))
        preparation
        summary-parts
        summary-message
        summary-item-types
        work-handle
        settled
        cancelled)
    (cl-labels
        ((record-failure (err)
           (let ((message (e-harness--backend-error-message err))
                 (details (e-harness--backend-error-details err)))
             (e-harness--emit-turn-event
              harness session-id turn-id 'compaction-failed
              (list :message message :details details :reason reason))
             (when on-error
               (funcall on-error err))))
         (finish-error (err)
           (unless settled
             (setq settled t)
             (when (and work-handle
                        (not (e-request-terminal-p
                              (e-work-handle-lifecycle work-handle))))
               (ignore-errors (e-work-cancel work-handle)))
             (record-failure err)))
         (finish-done (_result)
           (unless (or settled cancelled)
             (condition-case err
                 (let* ((summary
                         (string-trim
                          (or summary-message
                              (string-join (nreverse summary-parts) "")))))
                   (when (string-empty-p summary)
                     (signal 'e-compaction-error
                             (list
                              "Compaction backend returned an empty summary"
                              (list :request-started
                                    (and work-handle
                                         (plist-get
                                          (e-work-handle-metadata work-handle)
                                          :backend-request)
                                         t)
                                    :item-types
                                    (nreverse (delq nil summary-item-types))
                                    :summary-source
                                    'none))))
                   (let* ((metadata (plist-get preparation :metadata))
                          (portable-preparation
                           (plist-get preparation :portable-input))
                          (portable-checkpoint
                           (when portable-preparation
                             (e-compaction-portable-checkpoint-from-summary
                              preparation summary)))
                          (portable-application
                           (when portable-preparation
                             (e-compaction-preflight-portable-boundary
                              (e-harness-sessions harness)
                              session-id
                              preparation
                              portable-checkpoint)))
                          (record
                           (e-session-append-compaction
                            (e-harness-sessions harness)
                            session-id
                            summary
                            :first-kept-entry-id
                            (plist-get preparation :first-kept-entry-id)
                            :tokens-before
                            (plist-get preparation :tokens-before)
                            :tokens-kept
                            (plist-get preparation :tokens-kept)
                            :metadata metadata))
                          (portable-generation
                           (when portable-application
                             (e-compaction-apply-portable-boundary
                              (e-harness-sessions harness)
                              session-id
                              portable-application))))
                     (e-harness--maybe-provider-compaction-start
                      harness session-id portable-generation)
                     (setq settled t)
                     (e-harness--emit-turn-event
                      harness session-id turn-id 'compaction-finished
                      (list :compaction-id (plist-get record :id)
                            :portable-generation-id
                            (and portable-generation
                                 (e-context-lifetime-generation-id
                                  (e-context-lifetime-generation-from-record
                                   (e-session--context-record
                                    portable-generation))))
                            :reason
                            (plist-get (plist-get record :metadata) :reason)
                            :first-kept-entry-id
                            (plist-get record :first-kept-entry-id)
                            :tokens-before (plist-get record :tokens-before)
                            :tokens-kept (plist-get record :tokens-kept)))
                     (when on-done
                       (funcall on-done record))))
               (error
                (finish-error err)))))
         (cancel ()
           (unless settled
             (setq cancelled t)
             (setq settled t)
             (when work-handle
               (ignore-errors (e-work-cancel work-handle)))
             (record-failure
              (list 'quit "Context compaction cancelled")))
           t))
      (condition-case err
          (progn
            (e-harness--emit-turn-event
             harness session-id turn-id 'compaction-started
             (list :instructions instructions
                   :reason reason
                   :active-turn allow-active-turn))
            (setq preparation
                  (e-compaction-prepare
                   (e-harness-sessions harness)
                   session-id
                   :instructions instructions
                   :keep-recent-tokens keep-recent-tokens
                   :allow-split-turn (if (eq allow-split-turn
                                              'inherit-active-turn)
                                         allow-active-turn
                                       allow-split-turn)
                   :exclude-entry-ids exclude-entry-ids
                   :reason reason
                   :portable e-context-lifetime-shadow-projection-enabled))
            (e-harness--emit-turn-event
             harness session-id turn-id 'compaction-prepared
             (list :first-kept-entry-id
                   (plist-get preparation :first-kept-entry-id)
                   :reason reason
                   :tokens-before (plist-get preparation :tokens-before)
                   :tokens-kept (plist-get preparation :tokens-kept)))
            (e-harness--emit-turn-event
             harness session-id turn-id 'compaction-summary-started
             (list :backend t :reason reason))
            (setq work-handle
                  (e-work-start
                   (e-work-spec-create
                    :id "compact_session"
                    :description "Summarize older session context."
                    :execution 'backend
                    :interactive-policy 'async
                    :owner 'harness
                    :backend (lambda (_arguments _context)
                               (e-harness-backend harness))
                    :messages (lambda (_arguments _context)
                                (e-compaction-prepared-summary-messages
                                 preparation))
                    :options (lambda (_arguments _context)
                               (e-harness--options-without-tools
                                (e-harness-turn-options harness session-id)))
                    :item-handler
                    (lambda (_handle item _arguments _context)
                      (unless (or settled cancelled)
                        (push (plist-get item :type) summary-item-types)
                        (pcase (plist-get item :type)
                          ('assistant-message
                           (setq summary-message (plist-get item :content)))
                          ('assistant-delta
                           (push (or (plist-get item :content) "")
                                 summary-parts))))))
                   nil
                   :context (list :session-id session-id :turn-id turn-id)
                   :on-done #'finish-done
                   :on-error #'finish-error))
            (e-backend-request-create
             :cancel #'cancel
             :metadata (append
                        (list :operation 'compaction
                              :session-id session-id
                              :turn-id turn-id)
                        (e-harness--backend-work-request-metadata
                         work-handle))))
        (error
         (record-failure err)
         (signal (car err) (cdr err)))))))

(defun e-harness--auto-compaction-reserve-tokens ()
  "Return a normalized auto-compaction reserve."
  (if (and (integerp e-harness-auto-compaction-reserve-tokens)
           (>= e-harness-auto-compaction-reserve-tokens 0))
      e-harness-auto-compaction-reserve-tokens
    16384))

(defun e-harness--auto-compaction-suffix-tokens (harness session-id compaction)
  "Return estimated current suffix tokens since COMPACTION."
  (let* ((boundary-id (plist-get compaction :first-kept-entry-id))
         (entries (and boundary-id
                       (cdr (e-session-entries-from
                             (e-harness-sessions harness)
                             session-id
                             boundary-id)))))
    (when entries
      (apply #'+ (mapcar #'e-compaction-entry-token-estimate entries)))))

(defun e-harness--auto-compaction-no-progress-p (harness session-id)
  "Return non-nil when another auto-compaction would not move the boundary."
  (when-let ((latest (e-session-latest-valid-compaction
                      (e-harness-sessions harness)
                      session-id)))
    (let ((suffix-tokens
           (e-harness--auto-compaction-suffix-tokens
            harness session-id latest))
          (keep (if (and (integerp e-compaction-keep-recent-tokens)
                         (> e-compaction-keep-recent-tokens 0))
                    e-compaction-keep-recent-tokens
                  20000)))
      (and suffix-tokens (< suffix-tokens keep)))))

(defun e-harness--auto-compaction-useful-prefix-p
    (harness session-id exclude-entry-ids)
  "Return non-nil when auto-compaction has enough prefix messages to summarize."
  (> (length
      (cl-remove-if
       (lambda (message)
         (member (plist-get message :id) exclude-entry-ids))
       (e-session-messages (e-harness-sessions harness) session-id)))
     1))

(defun e-harness--auto-compaction-needed-p (harness session-id &optional context)
  "Return non-nil when SESSION-ID should auto-compact before prompting."
  (when e-harness-auto-compaction-enabled
    (when-let*
        ((usage-status
          (e-context-budget-status
           harness session-id
           :prefer-token-usage t
           :estimate-context nil))
         (status
          (if (plist-get usage-status :used-tokens)
              usage-status
            (or (and context
                     (let* ((options (plist-get context :options))
                            (model (plist-get options :model))
                            (used
                             (e-context-budget-context-token-estimate context))
                            (window (e-context-budget-model-window model)))
                       (list :used-tokens used :window window)))
                (e-context-budget-status
                 harness session-id
                 :prefer-token-usage t
                 :estimate-context t)))))
      (let ((used (plist-get status :used-tokens))
            (window (plist-get status :window))
            (reserve (e-harness--auto-compaction-reserve-tokens)))
        (and (integerp used)
             (integerp window)
             (> window reserve)
             (> used (- window reserve))
             (not (e-harness--auto-compaction-no-progress-p
                   harness session-id)))))))

(defun e-harness--maybe-auto-compact-session
    (harness session-id &optional active-turn-id exclude-entry-ids context)
  "Best-effort auto-compact SESSION-ID when it is near the context window."
  (when (e-harness--auto-compaction-needed-p harness session-id context)
    (condition-case nil
        (let ((args (list :reason 'auto)))
          (when active-turn-id
            (setq args
                  (append args
                          (list :allow-active-turn t
                                :allow-split-turn nil
                                :exclude-entry-ids exclude-entry-ids
                                :turn-id active-turn-id))))
          (apply #'e-harness-compact-session-start
                 harness session-id
                 (append args (list :on-error #'ignore))))
      (e-compaction-error nil))))

(defun e-harness--cancel-active-request (entry)
  "Cancel ENTRY's active backend or tool request when one exists."
  (when-let ((request (plist-get entry :request)))
    (condition-case err
        (cond
         ((e-backend-request-p request)
          (e-backend-cancel-request request))
         ((e-tools-request-p request)
          (e-tools-cancel-request request)))
      (error
       (plist-put entry :cancel-error err)
       nil))))

(defun e-harness--cancelled-tool-result (tool-call)
  "Return a structured cancellation result for TOOL-CALL."
  (list :tool-call-id (plist-get tool-call :id)
        :name (plist-get tool-call :name)
        :status 'error
        :content "Cancelled"
        :metadata '(:error cancelled)))

(defun e-harness--append-cancelled-tool-result (harness session-id turn-id entry)
  "Append a cancellation tool result when ENTRY has an open tool call."
  (when-let ((tool-call (plist-get entry :open-tool-call)))
    (let* ((result (e-harness--cancelled-tool-result tool-call))
           (message (list :role 'tool
                          :content result
                          :metadata nil)))
      (e-harness--append-message harness session-id turn-id message)
      (e-harness--emit-turn-event
       harness session-id turn-id 'tool-finished
       (list :tool-call tool-call :result result))
      (plist-put entry :open-tool-call nil))))

(defun e-harness--backend-error-message (err)
  "Return the compact user-visible error message for condition ERR.
For `e-compaction-error' return only the bare reason string; the
`define-error' message and the presentation layer both add their own
\"Context compaction failed\" prefix, so including it here triples it."
  (cond
   ((and (consp err)
         (eq (car err) 'e-loop-backend-error)
         (stringp (cadr err)))
    (cadr err))
   ((and (consp err)
         (eq (car err) 'e-compaction-error)
         (stringp (cadr err)))
    (cadr err))
   ;; Fallthrough for an arbitrary condition.  Route through the bounded
   ;; formatter: a cyclic or huge error payload would otherwise wedge the
   ;; printer and spin Emacs at 100% CPU.
   (t (e-work-error-message err))))

(defun e-harness--backend-error-details (err)
  "Return structured provider details from condition ERR, or nil."
  (when (consp err)
    (pcase (car err)
      ('e-loop-backend-error
       (nth 2 err))
      ('e-compaction-error
       (caddr err))
      ('e-work-deadline-exceeded
       (caddr err)))))

(defun e-harness--emit-turn-failed
    (harness session-id turn-id error-message &optional details)
  "Emit a turn-failed event from HARNESS.
SESSION-ID and TURN-ID identify the failed turn.  ERROR-MESSAGE describes the
provider or loop failure."
  (e-harness--emit-turn-event
   harness
   session-id
   turn-id
   'turn-failed
   (let ((payload (list :error error-message)))
     (when details
       (plist-put payload :details details))
     payload)))

(defun e-harness--append-message (harness session-id turn-id message)
  "Append MESSAGE in HARNESS for SESSION-ID TURN-ID and emit `message-added'."
  (e-harness--profile-call
   'harness.message-append
   (list :session-id session-id
         :turn-id turn-id
         :metadata (list :role (and (plist-get message :role)
                                    (symbol-name (plist-get message :role)))))
   (lambda ()
     (let ((message (copy-sequence message)))
       (when turn-id
         (plist-put message :turn-id turn-id))
       (setq message
             (e-session-append-message (e-harness-sessions harness)
                                       session-id
                                       message))
       (e-harness--emit-turn-event
        harness session-id turn-id 'message-added (list :message message))
       message))))

(defun e-harness-set-message-display (harness session-id message-id display)
  "Set DISPLAY on SESSION-ID's message MESSAGE-ID in HARNESS.
DISPLAY is a display disposition symbol (e.g. `hidden'); nil restores the
default visible state.  Persists the change through the session store and emits
a `message-updated' turn event so a live shell can drop or restore the block.
Returns the updated message, or nil when no such message exists."
  (when-let ((message (e-session-set-message-display
                       (e-harness-sessions harness)
                       session-id message-id display)))
    (e-harness--emit-turn-event
     harness session-id (plist-get message :turn-id)
     'message-updated (list :message message))
    message))

(defun e-harness--append-user-message
    (harness session-id turn-id prompt &optional metadata)
  "Append PROMPT as the user message in HARNESS for SESSION-ID and TURN-ID."
  ;; Endpoint tokens fence one live attachment.  They remain on the active
  ;; entry and queued input while in use, but are neither transcript context
  ;; nor durable data (and production tokens are intentionally opaque
  ;; structs, not JSON values).
  (setq metadata (e-harness--durable-input-metadata metadata))
  (e-harness--append-message
   harness
   session-id
   turn-id
   (list :role 'user
         :origin (or (plist-get metadata :input-origin) 'human)
         :content prompt
         :metadata metadata)))

(defun e-harness--turn-assistant-message (harness session-id turn-id)
  "Return the final assistant message for SESSION-ID TURN-ID in HARNESS.
When a turn produced multiple assistant messages, return the last one."
  (car (last (cl-remove-if-not
              (lambda (message)
                (and (eq (plist-get message :role) 'assistant)
                     (equal (plist-get message :turn-id) turn-id)))
              (e-harness-messages harness session-id)))))

(defun e-harness--context-capabilities (harness options)
  "Return normalized semantic context capabilities for OPTIONS.

The harness asks the backend at the provider-neutral boundary.  It stores the
answer on the request context, but does not interpret provider wire fields or
profile names."
  (let ((backend (e-harness-backend harness)))
    (if (e-backend-p backend)
        (e-backend-context-capabilities backend options)
      (e-backend-default-context-capabilities))))

(defun e-harness--context-lifetime-enabled-p (context-purpose)
  "Return non-nil when semantic lifetime projection is opted in for PURPOSE.

The feature is deliberately limited to correctness-critical turn context.  A
preview/status caller must not create a consumer frame or append a generation
just because the global opt-in is enabled."
  (and e-context-lifetime-shadow-projection-enabled
       (eq context-purpose 'turn)))

(defun e-harness--context-lifetime-ensure-generation (harness session-id)
  "Return SESSION-ID's current v2 generation, creating its first boundary."
  (or (e-session-context-lifetime-current-generation
       (e-harness-sessions harness) session-id)
      (let* ((store (e-harness-sessions harness))
             (session (e-session-get store session-id))
             (boundary (or (plist-get session :current-head-id)
                           (plist-get session :root-event-id)))
             (generation
              (e-context-lifetime-generation-create
               :id (format "generation:%s" boundary)
               :checkpoint nil
               :covered-session-boundary boundary)))
        (e-session-append-context-generation store session-id generation)
        generation)))

(defun e-harness--context-lifetime-fact-messages (promotions)
  "Return backend-facing durable messages for selected PROMOTIONS."
  (cl-loop for promotion in promotions
           append
           (cl-loop for fact in (e-context-lifetime-promotion-facts promotion)
                    collect
                    (list :role 'system
                          :content
                          (format "Promoted fact %s: %s"
                                  (plist-get fact :id)
                                  (let ((value (plist-get fact :value)))
                                    (if (stringp value)
                                        value
                                      (prin1-to-string value))))
                          :metadata
                          (list :context-lifetime 'promotion
                                :promotion-id
                                (e-context-lifetime-promotion-id promotion)
                                :fact-id (plist-get fact :id))))))

(defun e-harness--context-lifetime-apply-projection
    (harness session-id turn-id context capabilities)
  "Apply the opted-in semantic projection to CONTEXT for TURN-ID.

The canonical session path supplies durable message bodies and promotions.  A
new runtime frame is captured for every invocation, even when the source
fingerprints happen to be unchanged."
  (let* ((store (e-harness-sessions harness))
         (projection (e-session-context-lifetime-projection store session-id))
         (generation (or (plist-get projection :generation)
                         (e-harness--context-lifetime-ensure-generation
                          harness session-id)))
         ;; The generation may have been created above; read the projection
         ;; again so the covered branch boundary and durable tail are current.
         (projection (if (plist-get projection :generation)
                         projection
                       (e-session-context-lifetime-projection
                        store session-id)))
         (promotions (plist-get projection :promotions))
         (checkpoint
          (let ((value (and generation
                            (e-context-lifetime-generation-checkpoint
                             generation))))
            (cond
             ((null value) nil)
             ((and (listp value)
                   (e-context-lifetime--keyword-plist-p value))
              (list (copy-tree value)))
             ((listp value) (copy-tree value))
             (t (list (copy-tree value))))))
         (durable-tail
          (append (copy-tree (plist-get projection :durable-tail))
                  (e-harness--context-lifetime-fact-messages promotions)))
         (segments (plist-get context :segments))
         (consumer-request-id (format "consumer:%s:%s"
                                      turn-id (e-session-generate-ulid)))
         (frame-id (format "frame:%s" consumer-request-id))
         (frame
          (e-context-lifetime-frame-create-from-segments
           :id frame-id
           :generation-id (e-context-lifetime-generation-id generation)
           :consumer-request-id consumer-request-id
           :segments segments
           :observation-delivery
           (plist-get capabilities :observation-delivery)))
         (filtered-segments
          (mapcar
           (lambda (segment)
             (if (eq (plist-get segment :kind) 'history)
                 (plist-put (copy-tree segment) :messages
                            (append checkpoint durable-tail))
               segment))
           segments))
         (messages
          (cl-loop for segment in filtered-segments
                   append (copy-tree (plist-get segment :messages))))
         (semantic-projection
          (e-context-lifetime-project
           generation frame
           :durable-tail durable-tail
           :static-prefix
           (cl-loop for segment in segments
                    when (eq (plist-get segment :kind) 'static-prefix)
                    append (copy-tree (plist-get segment :messages)))
           :stable-context
           (cl-loop for segment in segments
                    when (eq (plist-get segment :kind) 'stable-context)
                    append (copy-tree (plist-get segment :messages))))))
    (plist-put context :segments filtered-segments)
    (plist-put context :messages messages)
    (plist-put context :context-lifetime-enabled t)
    (plist-put context :lifetime-generation generation)
    (plist-put context :lifetime-frame frame)
    (plist-put context :lifetime-promotions promotions)
    (plist-put context :lifetime-projection semantic-projection)
    context))

(defun e-harness--lifetime-response-entry-id
    (harness session-id turn-id fallback)
  "Return the current durable response entry for TURN-ID.

Assistant and tool-call messages are the existing session representation of a
completed provider response.  FALLBACK is used only by synthetic backends that
returned no durable message; it remains an opaque runtime response identity."
  (or (plist-get
       (car (last
             (seq-filter
              (lambda (entry)
                (and (eq (plist-get entry :type) 'message)
                     (equal (plist-get entry :turn-id) turn-id)
                     (memq (plist-get entry :role)
                           '(assistant tool-call))))
              (e-session-current-path (e-harness-sessions harness)
                                      session-id))))
       :id)
      fallback))

(defun e-harness--lifetime-commit-response
    (harness session-id turn-id active-entry payload)
  "Complete the runtime frame in PAYLOAD and append valid promotions.

The loop has already validated the effect shape while streaming.  This
boundary resolves source observation IDs against the trusted consumed frame,
derives provenance in core, and performs the ordinary session appends before
the next provider request is started."
  (when (and (e-context-lifetime-shadow-enabled-p)
             (e-harness--active-turn-running-p active-entry))
    (let* ((frame (or (plist-get payload :frame)
                      (plist-get active-entry :context-frame)))
           (effects (plist-get payload :promotion-effects))
           (response-id
            (e-harness--lifetime-response-entry-id
             harness session-id turn-id
             (plist-get payload :provider-request-id))))
      (when (and frame (e-context-lifetime-frame-p frame))
        (unless (e-context-lifetime-frame-consumed-p frame)
          ;; Calculate deterministic ids against the trusted response-bound
          ;; frame, then perform the one public completion operation with that
          ;; exact ordered declaration.
          (let* ((consumer-id
                  (e-context-lifetime-frame-consumer-request-id frame))
                 (candidate
                  (e-context-lifetime-frame-complete-for-consumer
                   frame consumer-id response-id))
                 (promotion-ids
                  (mapcar
                   (lambda (effect)
                     (e-context-lifetime-promotion-id-for candidate effect))
                   effects))
                 (consumed
                  (e-context-lifetime-frame-complete-for-consumer
                   frame consumer-id response-id promotion-ids))
                 (promotions
                  (mapcar
                   (lambda (effect)
                     (e-context-lifetime-promotion-from-effect
                      consumed effect))
                   effects)))
            (dolist (promotion promotions)
              (e-session-append-context-promotion
               (e-harness-sessions harness) session-id promotion))
            (plist-put active-entry :context-frame consumed)
            (e-harness--emit-turn-event
             harness session-id turn-id 'context-frame-consumed
             (list :frame-id (e-context-lifetime-frame-id consumed)
                   :consumer-request-id consumer-id
                   :response-entry-id response-id
                   :promotion-ids promotion-ids))
            consumed))))))

(defun e-harness--lifetime-tool-observation-frame
    (harness session-id turn-id active-entry payload)
  "Return a fresh consumer-bound frame for one tool result PAYLOAD."
  (when (and e-context-lifetime-shadow-projection-enabled
             (e-harness--active-turn-running-p active-entry))
    (let* ((previous (plist-get payload :previous-frame))
           (generation (or (and previous
                                (e-session-context-lifetime-current-generation
                                 (e-harness-sessions harness) session-id))
                           (plist-get active-entry :lifetime-generation)
                           (e-harness--context-lifetime-ensure-generation
                            harness session-id)))
           (tool-call (plist-get payload :tool-call))
           (result (plist-get payload :result))
           (message (plist-get payload :message))
           (tool-id (or (plist-get tool-call :id)
                        (plist-get result :tool-call-id)
                        (e-session-generate-ulid)))
           (existing-observations
            (and previous
                 (not (e-context-lifetime-frame-consumed-p previous))
                 (e-context-lifetime-frame-observations previous)))
           (consumer-id
            (or (and existing-observations
                     (e-context-lifetime-frame-consumer-request-id previous))
                (format "consumer:%s:tool:%s"
                        turn-id (e-session-generate-ulid))))
           (body (list :tool-call (copy-tree tool-call)
                       :tool-result (copy-tree result)
                       :message-id (plist-get message :id)))
           (observation
            (list :observation-id (format "observation:tool-bundle:%s" tool-id)
                  :kind "tool-result"
                  :source-entry-ref
                  (or (plist-get message :id)
                      (format "external:tool-result:%s" tool-id))
                  :source-fingerprint
                  (secure-hash 'sha256 (prin1-to-string body))
                  :effective-delivery "inherited"
                  :body body))
           (observations (append (copy-tree existing-observations)
                                 (list observation)))
           (frame
            (e-context-lifetime-frame-create
             :id (format "frame:%s:%s"
                         consumer-id
                         (substring (secure-hash 'sha256
                                                  (prin1-to-string
                                                   (mapcar
                                                    (lambda (item)
                                                      (plist-get item
                                                                 :observation-id))
                                                    observations)))
                                    0 16))
             :generation-id
             (e-context-lifetime-generation-id generation)
             :consumer-request-id consumer-id
             :observations observations)))
      (plist-put active-entry :context-frame frame)
      (plist-put active-entry :lifetime-generation generation)
      frame)))

(defconst e-harness--reserved-derived-context-option-keys
  '(:context-segment-message-count
    :replaceable-current-state-partitioned
    :context-lifetime-enabled
    :context-promotion-observation-ids
    :context-promotion-frame-id
    :provider-anchor
    :provider-anchor-delta-messages
    :provider-anchor-source-message-count
    :provider-compaction-output
    :provider-compaction-delta-messages
    :provider-compaction-source-entry-id
    :provider-compaction-generation-id
    :provider-compaction-fingerprint
    :provider-compaction-invalidation-reason)
  "Context options owned by the harness rather than callers.

These values are derived from the semantic projection at request construction.
They must not be inherited from defaults, session options, or a provider-facing
caller because such values could forge or stale the frontier partition.")

(defun e-harness--strip-reserved-derived-context-options (options)
  "Return OPTIONS without harness-derived context partition markers."
  (let ((clean (copy-sequence options)))
    (dolist (key e-harness--reserved-derived-context-option-keys clean)
      (cl-remf clean key))))

(defconst e-harness--provider-anchor-derived-context-option-keys
  '(:provider-anchor
    :provider-anchor-delta-messages
    :provider-anchor-source-message-count)
  "Provider-anchor fields derived from the session-owned anchor.")

(defun e-harness--strip-provider-anchor-derived-context-options (options)
  "Return OPTIONS without provider-anchor-derived correctness state."
  (let ((clean (copy-sequence options)))
    (dolist (key e-harness--provider-anchor-derived-context-option-keys clean)
      (cl-remf clean key))))

(defun e-harness--context-observation-frontier (context capabilities)
  "Attach semantic observation metadata to CONTEXT for CAPABILITIES.

The frontier is kind-scoped.  Only observations whose own capability entry is
proven replaceable may be removed from a provider continuation; a replaceable
canvas never authorizes dropping an inherited tool result or trace."
  (let* ((options
          (e-harness--strip-reserved-derived-context-options
           (plist-get context :options)))
         (delivery-map (plist-get capabilities :observation-delivery))
         (delivery (e-backend-observation-delivery-for-kind
                    capabilities 'current-state))
         (messages (e-context-current-state-messages context))
         (fingerprint (and messages
                           (e-context-current-state-fingerprint context)))
         (observations
          (cl-loop for segment in (plist-get context :segments)
                   for kind = (plist-get segment :kind)
                   when (memq kind '(current-state dynamic-context))
                   collect
                   (list :kind kind
                         :delivery
                         (e-backend-observation-delivery-for-kind
                          capabilities kind)
                         :messages (copy-tree (plist-get segment :messages))
                         :fingerprint (plist-get segment :fingerprint))))
         (frontier (list :delivery delivery
                         :delivery-map (copy-tree delivery-map)
                         :messages (copy-tree messages)
                         :fingerprint fingerprint
                         :observations observations))
         (frame (plist-get context :lifetime-frame))
         (clean-p
          (or (null frame)
              (cl-every
               (lambda (observation)
                 (equal (plist-get observation :effective-delivery)
                        "request-local-replaceable"))
               (e-context-lifetime-frame-observations frame)))))
    (setq options (plist-put options :context-capabilities
                             (copy-sequence capabilities)))
    (setq options (plist-put options :observation-delivery delivery))
    (setq options (plist-put options :observation-delivery-map
                             (copy-tree delivery-map)))
    (setq options (plist-put options :observation-frontier frontier))
    (setq options (plist-put options :lifetime-ephemerals-clean-p clean-p))
    (when (plist-get context :context-lifetime-enabled)
      (setq options (plist-put options :context-lifetime-enabled t)))
    (when frame
      (setq options
            (plist-put
             options
             :context-promotion-frame-id
             (e-context-lifetime-frame-id frame)))
      (setq options
            (plist-put
             options
             :context-promotion-observation-ids
             (copy-sequence
              (e-context-lifetime-frame-observation-ids frame)))))
    (when (eq delivery 'request-local-replaceable)
      (setq options
            (plist-put options :replaceable-current-state
                       (copy-tree messages))))
    (when fingerprint
      (setq options
            (plist-put options :current-state-fingerprint fingerprint)))
    (plist-put context :options options)
    (plist-put context :observation-frontier frontier)
    context))

(defun e-harness--context-with-segment-message-boundary (context)
  "Attach the exact message coverage represented by CONTEXT segments.

Only a complete semantic segment projection receives the reserved derived
boundary.  In-turn continuation deltas are already partitioned by the loop's
lexical ownership and do not use this marker; a partial segment list remains
ambiguous and is rejected by the provider adapter."
  (let* ((messages (plist-get context :messages))
         (segments (plist-get context :segments))
         (segment-messages
          (cl-loop for segment in segments
                   append (copy-tree (plist-get segment :messages))))
         (segment-message-count (length segment-messages))
         ;; Counting alone is insufficient: a hostile/stale segment list could
         ;; have the right length while describing different messages.  The
         ;; harness owns both sides of this comparison and only then derives
         ;; the reserved prefix boundary.
         (exact-coverage-p (and segments
                                (equal segment-messages messages)))
         (options
          (e-harness--strip-reserved-derived-context-options
           (plist-get context :options))))
    ;; The boundary pass removes caller-supplied derived values, but the
    ;; semantic lifetime fields are re-derived from the trusted runtime
    ;; projection rather than allowed to disappear between frontier and
    ;; adapter construction.
    (when (plist-get context :context-lifetime-enabled)
      (setq options (plist-put options :context-lifetime-enabled t)))
    (when-let ((frame (plist-get context :lifetime-frame)))
      (setq options
            (plist-put options
                       :context-promotion-frame-id
                       (e-context-lifetime-frame-id frame)))
      (setq options
            (plist-put options
                       :context-promotion-observation-ids
                       (copy-sequence
                        (e-context-lifetime-frame-observation-ids
                         frame)))))
    (when exact-coverage-p
      (setq options
            (plist-put options
                       :context-segment-message-count
                       segment-message-count)))
    (plist-put context :options options)
    context))

(defun e-harness--provider-anchor-fingerprints (context)
  "Return JSON-stable provider-relevant fingerprints from CONTEXT."
  (let* ((options (plist-get context :options))
         (capabilities (plist-get options :context-capabilities))
         (delivery (plist-get options :observation-delivery))
         (delivery-map (or (plist-get options :observation-delivery-map)
                           (plist-get capabilities :observation-delivery)))
         (fingerprints
          (list
           :segments
           (mapcar
            (lambda (segment)
              (list :kind (symbol-name (plist-get segment :kind))
                    :id (prin1-to-string (plist-get segment :id))
                    :fingerprint (plist-get segment :fingerprint)))
            (cl-remove-if
             (lambda (segment)
               (or (memq (plist-get segment :kind) '(history delta))
                   (and (memq (plist-get segment :kind)
                              '(current-state dynamic-context))
                        (eq (e-backend-observation-delivery-for-kind
                             (list :observation-delivery delivery-map)
                             (plist-get segment :kind))
                            'request-local-replaceable))))
             (plist-get context :segments)))
           :active-layer-ids
           (copy-sequence (plist-get context :provider-anchor-active-layer-ids))
           :tools
           (mapcar
            (lambda (tool)
              (list :name (plist-get tool :name)
                    :fingerprint
                    (secure-hash 'sha256 (prin1-to-string tool))))
            (plist-get options :tools))
           :reasoning
           (list :reasoning (plist-get options :reasoning)
                 :reasoning-effort (plist-get options :reasoning-effort)
                 :effort (plist-get options :effort))
           :provider-options
           (list :instructions (plist-get options :instructions)
                 :max-tokens (plist-get options :max-tokens)
                 :prompt-cache (plist-get options :prompt-cache)
                 :prompt-cache-mode (plist-get options :prompt-cache-mode)
                 :prompt-cache-ttl (plist-get options :prompt-cache-ttl)
                 :prompt-cache-key (plist-get options :prompt-cache-key)
                 :prompt-cache-retention (plist-get options :prompt-cache-retention)
                 :anthropic-container-id (plist-get options :anthropic-container-id)
                 :anthropic-context-management
                 (plist-get options :anthropic-context-management)
                 :anthropic-beta-headers
                 (plist-get options :anthropic-beta-headers))
           :compaction-boundary
           (plist-get context :provider-anchor-compaction-boundary)
           :lifetime-generation
           (when-let ((generation (plist-get context :lifetime-generation)))
             (list :id (e-context-lifetime-generation-id generation)
                   :covered-session-boundary
                   (e-context-lifetime-generation-covered-session-boundary
                    generation)
                   :checkpoint-fingerprint
                   (secure-hash
                    'sha256
                    (prin1-to-string
                     (e-context-lifetime-generation-checkpoint generation)))))
           ;; Keep semantic control values JSON-stable.  The observation value
           ;; itself is compared only for inherited delivery; a replaceable
           ;; observation may change without invalidating the stable anchor.
           :observation-delivery
           (and delivery (symbol-name delivery))
           :observation-delivery-map
           (copy-tree delivery-map)
           :reserved-effect-carrier
           (plist-get capabilities :reserved-effect-carrier)
           :reserved-effect-schema-version
           (when (eq (plist-get capabilities :reserved-effect-carrier)
                     'context-promote-wire)
             e-context-lifetime-promotion-schema-version)
           :lifetime-observation-safety
           (if (plist-get options :lifetime-ephemerals-clean-p)
               'clean
             'contaminated))))
    (when (and (not (eq delivery 'request-local-replaceable))
               (plist-get options :current-state-fingerprint))
      (setq fingerprints
            (plist-put fingerprints
                       :current-state-fingerprint
                       (plist-get options :current-state-fingerprint))))
    fingerprints))

(defun e-harness--context-with-continuation-projection-identity (context)
  "Attach the stable continuation identity for CONTEXT's request projection.

The identity describes the semantic input that a provider continuation stores:
stable segments, active tools/layers, provider options, compaction boundary, and
capabilities.  A proven request-local current-state frontier is deliberately
excluded by `e-harness--provider-anchor-fingerprints'.  The loop uses this
opaque provider-neutral value to fence a response candidate when a same-turn
refresh changes the projection that produced it."
  (let* ((options (copy-sequence (plist-get context :options)))
         (capabilities (plist-get options :context-capabilities))
         (identity
          (list
           :provider-anchor-provider-id
           (plist-get options :provider-anchor-provider-id)
           :model (plist-get options :model)
           :provider-continuation
           (plist-get options :provider-continuation)
           :context-capabilities
           (copy-tree capabilities)
           :provider-anchor-fingerprints
           (e-harness--provider-anchor-fingerprints context))))
    (plist-put context
               :options
               (plist-put options
                          :continuation-projection-identity
                          identity))))

(defun e-harness--provider-anchor-compaction-boundary (harness session-id)
  "Return provider-anchor compatibility data for latest compaction boundary."
  (when-let ((compaction
              (e-session-latest-valid-compaction
               (e-harness-sessions harness)
               session-id)))
    (list :id (plist-get compaction :id)
          :first-kept-entry-id (plist-get compaction :first-kept-entry-id))))

(defun e-harness--provider-anchor-invalidation-reason
    (harness session-id provider-id model fingerprints)
  "Return the most relevant provider-anchor invalidation reason."
  (let* ((anchors
          (cl-remove-if-not
           (lambda (anchor)
             (eq (plist-get anchor :provider-id) provider-id))
           (e-session-provider-anchors (e-harness-sessions harness) session-id)))
         (latest (car (last anchors))))
    (if latest
        (e-session-provider-anchor-incompatibility-reason
         (e-harness-sessions harness)
         session-id
         latest
         provider-id
         model
         fingerprints)
      'missing-anchor)))

(defun e-harness--provider-anchor-dynamic-context-messages (context)
  "Return backend-neutral dynamic-context messages from CONTEXT."
  (unless (eq (plist-get (plist-get context :options)
                         :observation-delivery)
              'request-local-replaceable)
    (e-context-current-state-messages context)))

(defun e-harness--provider-anchor-delta-messages
    (harness session-id anchor &optional context)
  "Return backend-neutral fresh messages after ANCHOR coverage in SESSION-ID."
  (let ((dynamic-messages
         (when context
           (e-harness--provider-anchor-dynamic-context-messages context)))
        (entries (cdr (e-session-entries-from
                       (e-harness-sessions harness)
                       session-id
                       (plist-get anchor :covered-entry-id)))))
    (append
     dynamic-messages
    (if (plist-get context :context-lifetime-enabled)
        (delq nil
              (mapcar #'e-session-context-lifetime-durable-message
                      (cl-remove-if-not
                       (lambda (entry)
                         (eq (plist-get entry :type) 'message))
                       entries)))
      (mapcar #'e-context--backend-message
              (cl-remove-if-not
               (lambda (entry)
                 (eq (plist-get entry :type) 'message))
               entries))))))

(defun e-harness--provider-anchor-selection-allowed-p (options)
  "Return non-nil when OPTIONS can safely select a provider anchor.

Continuation is a semantic backend capability, not merely a request option.
An inherited current-state observation may only branch from a clean anchor when
the backend explicitly supports branchable continuation; a linear continuation
must reconstruct statelessly in that case.  A request-local replacement does
not contaminate the anchor and is safe for either supported continuation mode."
  (let* ((capabilities (plist-get options :context-capabilities))
         (continuation (plist-get capabilities :continuation))
         (delivery (plist-get options :observation-delivery))
         (current-state-fingerprint
          (plist-get options :current-state-fingerprint)))
    (and (plist-get options :provider-continuation)
         (memq continuation '(linear branchable))
         (or (not (plist-member options :lifetime-ephemerals-clean-p))
             (plist-get options :lifetime-ephemerals-clean-p)
             (eq continuation 'branchable))
         (or (null current-state-fingerprint)
             (eq delivery 'request-local-replaceable)
             (eq continuation 'branchable)))))

(defun e-harness--provider-anchor-lookup-fingerprints (context)
  "Return fingerprints used to select an anchor for CONTEXT.

An inherited observation may branch repeatedly only from a clean anchor.  For
a branchable backend, omit the current observation from the lookup identity;
this lets a clean anchor match while
`e-session-provider-anchor-incompatibility-reason' still rejects any
persisted anchor that carries a non-nil observation
fingerprint.  Linear backends keep the ordinary fingerprint and are rejected
by `e-harness--provider-anchor-selection-allowed-p' when an observation is
inherited."
  (let* ((options (plist-get context :options))
         (capabilities (plist-get options :context-capabilities))
         (fingerprints (e-harness--provider-anchor-fingerprints context))
         (inherited-observation-p
          (cl-some
           (lambda (observation)
             (equal (plist-get observation :delivery) 'inherited))
           (plist-get (plist-get context :observation-frontier)
                      :observations))))
    (if (and inherited-observation-p
             (eq (plist-get capabilities :continuation) 'branchable))
        (let ((lookup (copy-tree fingerprints)))
          ;; A clean anchor was produced before any current-state segment
          ;; existed.  Remove those volatile segment identities as well as the
          ;; separate value fingerprint; contaminated persisted anchors still
          ;; fail the non-nil fingerprint comparison below.
          (setq lookup
                (plist-put
                 lookup
                 :segments
                 (cl-remove-if
                  (lambda (segment)
                    (member (plist-get segment :kind)
                            '(current-state dynamic-context
                              "current-state" "dynamic-context")))
                  (plist-get lookup :segments))))
          (plist-put lookup :current-state-fingerprint nil))
      fingerprints)))

(defun e-harness--provider-anchor-safety (options)
  "Return the anchor advancement/safety diagnostic for OPTIONS."
  (let* ((capabilities (plist-get options :context-capabilities))
         (continuation (plist-get capabilities :continuation))
         (delivery (plist-get options :observation-delivery))
         (current-state-fingerprint
          (plist-get options :current-state-fingerprint)))
    (cond
     ((not (memq continuation '(linear branchable)))
      'hold-unavailable-capability)
     ((and current-state-fingerprint
           (eq delivery 'inherited)
           (eq continuation 'linear))
      'hold-inherited-observation)
     ((and current-state-fingerprint
           (eq delivery 'inherited)
           (eq continuation 'branchable))
      'branchable-clean-anchor-only)
     ((and (plist-member options :lifetime-ephemerals-clean-p)
           (not (plist-get options :lifetime-ephemerals-clean-p)))
      'hold-inherited-observation)
     (t
      'advance-eligible))))

(defun e-harness--provider-compaction-fingerprint (harness session-id context)
  "Return the runtime identity for an opaque compaction candidate.

The identity is derived from provider-neutral stable projection fields and the
active generation.  Replaceable current-state values are excluded by the
existing anchor fingerprint helper; opaque provider output is never hashed or
otherwise interpreted here."
  (let* ((options (plist-get context :options))
         (generation (or (plist-get context :lifetime-generation)
                         (e-session-context-lifetime-current-generation
                          (e-harness-sessions harness) session-id)))
         (identity
          (list :provider-id (plist-get options :provider-anchor-provider-id)
                :model (plist-get options :model)
                :capabilities (copy-tree
                               (plist-get options :context-capabilities))
                :anchor-fingerprints
                (e-harness--provider-anchor-fingerprints context)
                :generation-id
                (and generation
                     (e-context-lifetime-generation-id generation)))))
    (secure-hash 'sha256 (prin1-to-string identity))))

(defun e-harness--provider-compaction-candidate-source-entry-id
    (harness session-id)
  "Return the exact journal head captured by a candidate.

Entries after this identity are scanned separately when the candidate is
consumed, and only their portable durable message projection may become the
provider delta."
  (when-let ((entry (car (last (e-session-current-path
                               (e-harness-sessions harness) session-id)))))
    (plist-get entry :id)))

(defun e-harness--provider-compaction-delta-messages
    (harness session-id source-entry-id)
  "Return portable durable messages after SOURCE-ENTRY-ID.

Provider-compaction deltas are intentionally separate from provider-anchor
deltas.  Raw tool-call/tool/replay journal entries are not eligible, so an
opaque candidate cannot create an orphaned provider tool bundle."
  (let ((after nil)
        (found nil)
        result)
    (dolist (entry (e-session-current-path
                    (e-harness-sessions harness) session-id))
      (if after
          (when-let ((message
                      (e-session-context-lifetime-durable-message entry)))
            (push (e-context-lifetime-portable-message message) result))
        (when (equal (plist-get entry :id) source-entry-id)
          (setq after t
                found t))))
    (if found
        (nreverse result)
      nil)))

(defun e-harness--provider-compaction-store-candidate
    (harness session-id context generation output usage
             source-entry-id promotion-frontier input-fingerprint)
  "Install one runtime-only opaque provider candidate for SESSION-ID.

No session record is touched.  The candidate is fenced by GENERATION, the
  covered durable source entry, and the stable projection fingerprint; a later
  context rebuild either selects it exactly or discards it."
  (let* ((options (plist-get context :options))
         (current-generation
          (e-session-context-lifetime-current-generation
           (e-harness-sessions harness) session-id))
         (current-frontier
          (mapcar #'e-context-lifetime-promotion-id
                  (plist-get
                   (e-session-context-lifetime-projection
                    (e-harness-sessions harness) session-id)
                   :promotions)))
         (source-on-path
          (seq-some (lambda (entry)
                      (equal (plist-get entry :id) source-entry-id))
                    (e-session-current-path
                     (e-harness-sessions harness) session-id))))
    ;; A promotion frontier changing while the provider request is in flight
    ;; makes its opaque coverage ambiguous.  Keep portable context as the
    ;; correctness path and discard acceleration without mutating the session.
    (when (and source-entry-id generation current-generation
               (equal (e-context-lifetime-generation-id generation)
                      (e-context-lifetime-generation-id current-generation))
               source-on-path
               (equal promotion-frontier current-frontier)
               (stringp input-fingerprint))
      (puthash
       session-id
       (list :provider-id (plist-get options :provider-anchor-provider-id)
             :model (plist-get options :model)
             :generation-id (e-context-lifetime-generation-id generation)
             :covered-session-boundary
             (e-context-lifetime-generation-covered-session-boundary
              generation)
             :source-entry-id source-entry-id
             :promotion-frontier (copy-sequence promotion-frontier)
             :input-fingerprint input-fingerprint
             :fingerprint
             (e-harness--provider-compaction-fingerprint
              harness session-id context)
             :output output
             :usage usage)
       (e-harness-provider-compaction-candidates harness)))))

(defun e-harness--provider-compaction-candidate-compatible-p
    (harness session-id context candidate capabilities)
  "Return non-nil when runtime CANDIDATE is exact for CONTEXT."
  (let* ((options (plist-get context :options))
         (generation (plist-get context :lifetime-generation))
         (source-entry-id (plist-get candidate :source-entry-id))
         (path (e-session-current-path (e-harness-sessions harness) session-id)))
    (and (eq (plist-get capabilities :provider-compaction) 'opaque)
         (equal (plist-get candidate :provider-id)
                (plist-get options :provider-anchor-provider-id))
         (equal (plist-get candidate :model) (plist-get options :model))
         generation
         (equal (plist-get candidate :generation-id)
                (e-context-lifetime-generation-id generation))
         (seq-some (lambda (entry)
                     (equal (plist-get entry :id) source-entry-id))
                   path)
         (equal (plist-get candidate :promotion-frontier)
                (mapcar #'e-context-lifetime-promotion-id
                        (plist-get
                         (e-session-context-lifetime-projection
                          (e-harness-sessions harness) session-id)
                         :promotions)))
         ;; Opaque compaction contains only durable context.  It cannot safely
         ;; replace an inherited observation frontier.
         (or (not (plist-member options :lifetime-ephemerals-clean-p))
             (plist-get options :lifetime-ephemerals-clean-p))
         (equal (plist-get candidate :fingerprint)
                (e-harness--provider-compaction-fingerprint
                 harness session-id context)))))

(defun e-harness--context-with-provider-compaction
    (harness session-id context &optional context-purpose)
  "Attach an exact runtime provider-compaction candidate to CONTEXT.

Candidates are selected only for the same provider/model/generation and stable
semantic fingerprint.  A mismatch is discarded and ordinary portable context
continues unchanged."
  (let* ((options (copy-sequence (plist-get context :options)))
         (capabilities (plist-get options :context-capabilities)))
    ;; Runtime provider state belongs to the correctness-critical request
    ;; path.  Preview/status/optional contexts must neither consume nor fence
    ;; the candidate that the next actual turn may use.
    (when (eq context-purpose 'turn)
      (let ((candidate
             (gethash session-id
                      (e-harness-provider-compaction-candidates harness))))
        (if (and candidate
                 (e-harness--provider-compaction-candidate-compatible-p
                  harness session-id context candidate capabilities))
            (let ((delta
                   (e-harness--provider-compaction-delta-messages
                    harness session-id
                    (plist-get candidate :source-entry-id))))
              (setq options
                    (plist-put options :provider-compaction-output
                               (plist-get candidate :output)))
              (setq options
                    (plist-put options :provider-compaction-delta-messages
                               delta))
              (setq options
                    (plist-put options :provider-compaction-source-entry-id
                               (plist-get candidate :source-entry-id)))
              (setq options
                    (plist-put options :provider-compaction-generation-id
                               (plist-get candidate :generation-id)))
              (setq options
                    (plist-put options :provider-compaction-fingerprint
                               (plist-get candidate :fingerprint)))
              (setq options
                    (plist-put options :context-rendering-strategy
                               'opaque-provider-compaction))
              ;; Opaque output is a one-shot runtime candidate.  A later
              ;; request must use a normal compatible anchor or portable
              ;; reconstruction, never replay the compact response.
              (remhash session-id
                       (e-harness-provider-compaction-candidates harness)))
          (when candidate
            (remhash session-id
                     (e-harness-provider-compaction-candidates harness))
            (setq options
                  (plist-put options :provider-compaction-invalidation-reason
                             'stale-or-incompatible-candidate))))))
    (plist-put context :options options)))

(defun e-harness--context-with-provider-anchor (harness session-id context)
  "Attach only a session-owned compatible provider anchor to CONTEXT.

Provider-anchor fields are harness-derived correctness state.  Clear all stale
or caller-supplied values before looking up the session-owned anchor so a
missing or incompatible anchor cannot accidentally preserve forged continuation
state across a context rebuild."
  (let* ((options
          (e-harness--strip-provider-anchor-derived-context-options
           (plist-get context :options)))
         (provider-id (plist-get options :provider-anchor-provider-id))
         (lookup-fingerprints
          (e-harness--provider-anchor-lookup-fingerprints context)))
    ;; Keep the cleaned options authoritative even when no provider anchor can
    ;; be selected.  The branches below may add only a session-owned anchor or
    ;; a diagnostic explaining why one was not selected.
    (plist-put context :options options)
    (when (and provider-id
               (e-harness--provider-anchor-selection-allowed-p options))
      (let ((anchor
             (e-session-latest-compatible-provider-anchor
              (e-harness-sessions harness)
              session-id
              provider-id
              :model (plist-get options :model)
              :fingerprints lookup-fingerprints))
            (options (copy-sequence options)))
        (if anchor
            (progn
              (setq options (plist-put options :provider-anchor anchor))
              (setq options
                    (plist-put
                     options
                     :provider-anchor-delta-messages
                     (e-harness--provider-anchor-delta-messages
                      harness session-id anchor context)))
              (setq options
                    (plist-put
                     options
                     :provider-anchor-source-message-count
                     (length (plist-get context :messages)))))
          (setq options
                (plist-put
                 options
                 :provider-anchor-invalidation-reason
                 (e-harness--provider-anchor-invalidation-reason
                  harness
                  session-id
                  provider-id
                  (plist-get options :model)
                  lookup-fingerprints))))
        (setq options
              (plist-put
               options
               :context-rendering-strategy
               (cond
                ((eq (plist-get options :observation-delivery)
                     'request-local-replaceable)
                 'replaceable-channel)
                (anchor 'clean-anchor-branch)
                (t 'stateless))))
        (plist-put context :options options)))
    (unless (e-harness--provider-anchor-selection-allowed-p options)
      (setq options (copy-sequence (plist-get context :options)))
      (setq options
            (plist-put options :provider-anchor-invalidation-reason
                       (cond
                        ((not (memq
                               (plist-get
                                (plist-get options :context-capabilities)
                                :continuation)
                               '(linear branchable)))
                         'continuation-capability-unavailable)
                        ((and (plist-get options :current-state-fingerprint)
                              (eq (plist-get options :observation-delivery)
                                  'inherited))
                         'inherited-observation-requires-branchable)
                        (t 'provider-continuation-disabled))))
      (setq options
            (plist-put options
                       :context-rendering-strategy
                       (if (eq (plist-get options :observation-delivery)
                               'request-local-replaceable)
                           'replaceable-channel
                         'stateless)))
      (plist-put context :options options))
    (let ((options (copy-sequence (plist-get context :options))))
      (setq options
            (plist-put options :provider-anchor-safety
                       (e-harness--provider-anchor-safety options)))
      (plist-put context :options options))
    context))

(defun e-harness--provider-anchor-candidate-persistable-p
    (context candidate final-request-ordinal)
  "Return non-nil when accepted CANDIDATE owns CONTEXT's final request.

The loop marks candidates only after projection-compatible promotion.  The
request ordinal and projection identity then fence a candidate from an
earlier request in the same turn, including the case where a refreshed
request emits no candidate at all."
  (let* ((provider-id (plist-get candidate :provider-id))
         (options (plist-get context :options))
         (frontier (plist-get options :observation-frontier))
         (inherited-observation-p
          (or (and (eq (plist-get options :observation-delivery) 'inherited)
                   (plist-get options :current-state-fingerprint))
              (cl-some
               (lambda (observation)
                 (eq (plist-get observation :delivery) 'inherited))
               (plist-get frontier :observations)))))
    (and (plist-get candidate :accepted-for-persistence)
         (plist-member candidate :projection-identity)
         (equal (plist-get candidate :projection-identity)
                (plist-get options :continuation-projection-identity))
         (equal (plist-get candidate :provider-request-ordinal)
                final-request-ordinal)
         provider-id
         (memq (plist-get (plist-get options :context-capabilities)
                          :continuation)
               '(linear branchable))
         (not inherited-observation-p)
         (or (not (plist-member options :lifetime-ephemerals-clean-p))
             (plist-get options :lifetime-ephemerals-clean-p))
         (pcase provider-id
           ('openai
            (and (plist-get options :provider-continuation)
                 (eq (plist-get options :provider-anchor-provider-id)
                     'openai)))
           (_ t)))))

(defun e-harness--latest-provider-anchor-candidates (candidates)
  "Return the latest provider anchor candidate per provider from CANDIDATES."
  (let ((latest-by-provider (make-hash-table :test #'equal))
        result)
    (dolist (candidate candidates)
      (puthash (plist-get candidate :provider-id)
               candidate
               latest-by-provider))
    (dolist (candidate candidates (nreverse result))
      (when (eq candidate
                (gethash (plist-get candidate :provider-id)
                         latest-by-provider))
        (push candidate result)))))

(defun e-harness--persist-provider-anchor-candidates
    (harness session-id turn-id context candidates final-request-ordinal)
  "Persist provider anchor CANDIDATES for completed TURN-ID."
  (when-let ((assistant-message
              (e-harness--turn-assistant-message harness session-id turn-id)))
    (dolist (candidate
             (e-harness--latest-provider-anchor-candidates
              (cl-remove-if-not
               (lambda (candidate)
                 (e-harness--provider-anchor-candidate-persistable-p
                  context candidate final-request-ordinal))
               candidates)))
      (when (e-harness--provider-anchor-candidate-persistable-p
             context candidate final-request-ordinal)
        (e-session-append-provider-anchor
         (e-harness-sessions harness)
         session-id
         (plist-get candidate :provider-id)
         :model (plist-get (plist-get context :options) :model)
         :covered-entry-id (plist-get assistant-message :id)
         :fingerprints (e-harness--provider-anchor-fingerprints context)
         :metadata (plist-get candidate :metadata))))))

(defun e-harness--run-turn-finished-hooks
    (harness session-id turn-id result &optional model-context)
  "Run `:turn-finished' hooks for HARNESS SESSION-ID TURN-ID over RESULT."
  (e-hooks-run-reduce
   (e-harness-hooks harness)
   :turn-finished
   result
   (list :harness harness
         :session-id session-id
         :turn-id turn-id
         :model-context model-context
         :assistant-message
         (e-harness--turn-assistant-message harness session-id turn-id))))

(cl-defun e-harness--run-prompt-turn-async
    (harness session-id turn-id &key on-request-start on-done on-error
             cancelled-p append-message on-event context drain-pending-input
             on-context-refresh on-response-complete on-tool-observation)
  "Start a queued async prompt turn for SESSION-ID and TURN-ID in HARNESS."
  (e-harness--profile-call
   'harness.prompt-turn-async-start
   (list :session-id session-id
         :turn-id turn-id)
   (lambda ()
     (let ((context (or context
                        (e-harness-turn-context harness session-id turn-id))))
        (e-loop-start-turn
        :session-id session-id
        :turn-id turn-id
        :messages (plist-get context :messages)
        :backend (e-harness-backend harness)
        :tools (e-harness-tools harness session-id turn-id)
        :tool-lifecycle (e-harness-tool-lifecycle harness session-id turn-id)
        :options (plist-get context :options)
        :segments (plist-get context :segments)
        :lifetime-frame (plist-get context :lifetime-frame)
        :on-response-complete on-response-complete
        :on-tool-observation on-tool-observation
         :turn-work-handle (plist-get
                            (gethash session-id
                             (e-harness-active-turns harness))
                             :work-handle)
         :board-enroll-work (e-harness-work-enrollment-function harness)
        :on-event (or on-event
                      (lambda (type payload)
                        (e-harness--emit-turn-event
                         harness session-id turn-id type payload)))
        :on-request-start on-request-start
        :cancelled-p cancelled-p
        :on-done on-done
        :on-error on-error
        :refresh-context
        (lambda ()
          ;; A context refresh is atomic at the loop boundary: messages and
          ;; all request-derived options (segments, observation frontier, and
          ;; anchor decision) come from one fresh harness projection.
          (let ((fresh-context
                 (e-harness-turn-context harness session-id turn-id)))
            (when on-context-refresh
              (funcall on-context-refresh fresh-context))
            fresh-context))
        :drain-pending-input
        (or drain-pending-input
            (lambda ()
              (mapcar
               (lambda (item)
                 (list :role 'user
                       :content (plist-get item :prompt)
                       :metadata (plist-get item :metadata)))
               (e-harness--drain-pending-steering-input
                harness
                (gethash session-id
                         (e-harness-active-turns harness))))))
        :append-message
        (or append-message
            (lambda (message)
              (e-harness--append-message
               harness session-id turn-id message))))))))

(cl-defun e-harness--prompt-attached-batch
    (harness session-id prompt &key metadata attachment-token)
  "Synchronously append PROMPT and run one backend turn from batch/test code."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-harness--prompt-attached-batch))
  (e-harness--profile-call
   'harness.prompt-batch
   (list :session-id session-id)
   (lambda ()
     (e-harness--prompt-attached-async
      harness session-id prompt :metadata metadata
      :attachment-token attachment-token)
     (let ((entry (e-harness-wait-batch harness session-id)))
       (pcase (plist-get entry :status)
         ('done
          (plist-get entry :result))
         ('error
          (let ((condition (plist-get entry :condition)))
            (if condition
                (signal (car condition) (cdr condition))
              (error "%s" (or (plist-get entry :error)
                              "Async prompt failed")))))
         ('cancelled
          (signal 'e-harness-no-active-turn (list session-id)))
         (_ entry))))))

(cl-defun e-harness--prompt-attached-async
    (harness session-id prompt &key delay metadata attachment-token)
  "Append PROMPT and run one backend turn asynchronously in HARNESS.
Return the queued turn id.  DELAY is primarily for tests and queued-turn
cancellation.  SESSION-ID identifies the session."
  (e-harness--require-attached-port harness session-id attachment-token)
  (setq metadata (plist-put (copy-sequence metadata)
                            :board-endpoint-token attachment-token))
  (e-harness--profile-call
   'harness.prompt-async
   (list :session-id session-id)
   (lambda ()
     (when (e-harness--active-turn-running-p
            (gethash session-id (e-harness-active-turns harness)))
       (signal 'e-harness-active-turn-exists (list session-id)))
      (let* ((turn-id (e-harness--next-turn-id))
              (turn-work
              (e-work-prepare
               (e-harness--turn-work-spec)
               nil
               :context (list :session-id session-id
                              :turn-id turn-id
                              :work-kind 'turn
                               :domain-ref (format "turn:%s" turn-id))))
             (entry (list :id turn-id
                          :status 'running
                          :work-handle turn-work
                         :result nil
                         :error nil
                         :error-details nil
                          :condition nil
                          :timer nil
                          :endpoint-token attachment-token
                          :request nil)))
        (when-let ((enroll (e-harness-work-enrollment-function harness)))
          (condition-case err
              (funcall enroll turn-work nil)
            (error
             (e-work-fail turn-work err)
             (signal (car err) (cdr err)))))
        (e-harness--put-active-turn harness session-id entry)
        (condition-case err
            (plist-put entry
                      :prompt-message-id
                      (plist-get
                       (e-harness--append-user-message
                        harness session-id turn-id prompt metadata)
                       :id))
          (error
          (let ((message (e-harness--backend-error-message err))
                (details (e-harness--backend-error-details err)))
            (plist-put entry :status 'error)
            (plist-put entry :error message)
            (plist-put entry :error-details details)
            (e-harness--emit-turn-failed
             harness session-id turn-id message details)
            (e-harness--remove-active-turn harness session-id entry)
             (signal (car err) (cdr err)))))
        (when (plist-get metadata :board-delivery-id)
          (e-harness--emit-turn-event
           harness session-id turn-id 'input-consumed
           (list :delivery-id (copy-tree (plist-get metadata :board-delivery-id))
                 :board-id (plist-get metadata :board-id)
                 :participant-id (plist-get metadata :board-participant-id)
                 :endpoint-token
                 (let ((token (plist-get metadata :board-endpoint-token)))
                   (if (vectorp token) (copy-sequence token) (copy-tree token)))
                 :endpoint-generation
                 (copy-tree (plist-get metadata :board-endpoint-generation))
                 :message-id (plist-get entry :prompt-message-id))))
        (e-work-start-prepared turn-work :arguments nil
                               :context (list :session-id session-id
                                              :turn-id turn-id
                                              :work-kind 'turn
                                              :domain-ref
                                              (format "turn:%s" turn-id)))
        (cl-labels
           ((active-entry-p ()
              (eq (gethash session-id (e-harness-active-turns harness))
                  entry))
            (cancelled-p ()
              (or (plist-get entry :cancelled)
                  (not (active-entry-p))))
            (maybe-retry-error
             (message details)
             ;; Schedule a retry for a retryable error (e.g. 429) while inside
             ;; the elapsed budget.  Return non-nil when a retry was scheduled
             ;; so the caller skips settling the turn as failed.
             ;;
             ;; When the error names a concrete reset time, wait until then
             ;; instead of using blind exponential backoff, and let that known
             ;; reopen extend the budget so the turn is not abandoned minutes
             ;; before capacity returns.
             (when (and (> e-harness-retry-max-elapsed-seconds 0)
                        (e-harness--retryable-error-p details))
               (let* ((now (float-time))
                      (deadline (or (plist-get entry :retry-deadline)
                                    (+ now
                                       e-harness-retry-max-elapsed-seconds)))
                      (attempt (1+ (or (plist-get entry :retry-attempt) 0)))
                      (reset-wait
                       (e-harness--retry-reset-seconds details))
                      (wait (or reset-wait
                                (e-harness--retry-backoff-seconds attempt)))
                      ;; A known reset can push the deadline out (bounded by
                      ;; the reset helper's own cap) so we do not give up right
                      ;; before the window reopens.
                      (effective-deadline (if reset-wait
                                              (max deadline (+ now wait 1.0))
                                            deadline)))
                 (plist-put entry :retry-deadline effective-deadline)
                 (when (< (+ now wait) effective-deadline)
                   (plist-put entry :retry-attempt attempt)
                   (e-harness--emit-turn-event
                    harness session-id turn-id 'turn-retrying
                    (list :error message
                          :details details
                          :attempt attempt
                          :backoff-seconds wait
                          :reset-wait reset-wait))
                   (plist-put entry :timer
                              (run-at-time wait nil #'start-turn))
                   t))))
            (finish-error
             (err)
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (let* ((message (e-harness--backend-error-message err))
                      (details
                       (e-backend-normalize-error-details
                        (e-harness-backend harness)
                        message
                        (e-harness--backend-error-details err)
                        err)))
                 (unless (maybe-retry-error message details)
                   (plist-put entry :status 'error)
                   (plist-put entry :condition err)
                   (plist-put entry :error message)
                    (plist-put entry :error-details details)
                    (e-work-fail turn-work err)
                    (e-harness--emit-turn-failed
                    harness session-id turn-id message details)
                   (e-harness--drain-pending-steering-input harness entry)
                   (e-harness--schedule-queue-drain
                    harness session-id entry)))))
            (finish-done
             (result)
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (e-harness--persist-provider-anchor-candidates
                harness
                session-id
                turn-id
                (plist-get entry :context)
                (nreverse (plist-get entry :provider-anchor-candidates))
                (plist-get entry :provider-anchor-final-request-ordinal))
	               (let ((hooked-result
	                      (e-harness--run-turn-finished-hooks
	                       harness session-id turn-id result
                               (plist-get entry :context))))
	                 (plist-put entry :result hooked-result)
	                 (plist-put entry :status 'done)
	                 ;; `e-loop' reports its own loop-level completion before
	                 ;; this callback.  Do not expose that provisional edge as
	                 ;; the harness/session terminal event: capability hooks
	                 ;; still own settlement work at this point.  The public
	                 ;; terminal edge is emitted here, after every hook has
	                 ;; observed the final assistant message and recorded any
	                 ;; durable audit metadata.
	                 (e-harness--emit-turn-event
	                  harness session-id turn-id 'turn-finished
	                  (list :reason (plist-get hooked-result :reason)))
	                 (e-work-finish turn-work hooked-result)
	                 (e-harness--drain-pending-steering-input harness entry)
	                 (e-harness--schedule-queue-drain
	                  harness session-id entry))))
	            (start-provider
	             (context)
	             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (plist-put entry :context context)
               (plist-put entry :context-frame
                          (plist-get context :lifetime-frame))
               (plist-put entry :lifetime-generation
                          (plist-get context :lifetime-generation))
               (plist-put entry :provider-anchor-candidates nil)
               (plist-put entry :provider-anchor-final-request-ordinal nil)
               (e-harness--run-prompt-turn-async
	                harness session-id turn-id
	                :cancelled-p #'cancelled-p
	                :on-request-start
	                (lambda (request)
	                  (when (and (active-entry-p)
	                             (not (plist-get entry :cancelled)))
	                    (plist-put entry :request request)))
	                :on-done #'finish-done
	                :on-error #'finish-error
	                :on-event
	                (lambda (type payload)
	                  (when (and (active-entry-p)
	                             (not (plist-get entry :cancelled)))
	                    (pcase type
	                      ('tool-started
	                       (plist-put entry :open-tool-call payload))
	                      ('tool-finished
	                       (plist-put entry :open-tool-call nil))
                      ('provider-request-finished
	                       ;; A request that completes clears the transient-retry
	                       ;; window: the budget bounds a consecutive failure
	                       ;; burst, not the turn's total wall clock.  Without
	                       ;; this a long turn (many successful requests, slow
	                       ;; tools) lets the deadline planted by an early blip
	                       ;; expire, so a late transport blip settles the turn
	                       ;; failed instead of retrying.
                       (when (eq (plist-get payload :status) 'done)
                         (plist-put entry :retry-deadline nil)
                         (plist-put entry :retry-attempt nil)
                         ;; Candidate ownership is per final successful
                         ;; provider request, not per whole turn.  A prior
                         ;; request may have produced a usable in-turn anchor
                         ;; while a later refreshed request owns the final
                         ;; assistant response.
                         (plist-put
                          entry
                          :provider-anchor-final-request-ordinal
                          (plist-get payload :provider-request-ordinal))))
                      ('provider-anchor-candidate
                       ;; Only loop-accepted candidates are ownership facts.
                       ;; Raw backend candidate items never enter the durable
                       ;; candidate collection.
                       (when (plist-get payload :accepted-for-persistence)
                         (plist-put
                          entry
                          :provider-anchor-candidates
                          (cons payload
                                (plist-get
                                 entry
                                 :provider-anchor-candidates))))))
	                    ;; `turn-finished' here is the loop's private terminal
	                    ;; edge.  The harness emits its public terminal event
	                    ;; after `:turn-finished' hooks settle in `finish-done'.
	                    (unless (eq type 'turn-finished)
	                      (e-harness--emit-turn-event
	                       harness session-id turn-id type payload))))
	                :append-message
	                (lambda (message)
	                  (when (and (active-entry-p)
	                             (not (plist-get entry :cancelled)))
	                    (e-harness--append-message
	                     harness session-id turn-id message)))
                :drain-pending-input
                (lambda ()
	                  (when (and (active-entry-p)
	                             (not (plist-get entry :cancelled)))
	                    (mapcar
	                     (lambda (item)
	                       (list :role 'user
	                             :content (plist-get item :prompt)
	                             :metadata (plist-get item :metadata)))
                     (e-harness--drain-pending-steering-input
                      harness entry))))
                :on-context-refresh
                (lambda (fresh-context)
                  ;; The loop calls this only for an atomic refresh that
                  ;; belongs to this active entry.  Keep the entry's context
                  ;; authoritative for final candidate ownership, but never
                  ;; let a stale callback mutate a replacement turn.
                  (when (and (active-entry-p)
                             (equal (plist-get entry :id) turn-id)
                             (not (plist-get entry :cancelled)))
                    (plist-put entry :context fresh-context)
                    (plist-put entry :context-frame
                               (plist-get fresh-context :lifetime-frame))
                    (plist-put entry :lifetime-generation
                               (plist-get fresh-context
                                          :lifetime-generation))))
                :on-response-complete
                (lambda (payload)
                  (when (and (active-entry-p)
                             (not (plist-get entry :cancelled)))
                    (e-harness--lifetime-commit-response
                     harness session-id turn-id entry payload)))
                :on-tool-observation
                (lambda (payload)
                  (when (and (active-entry-p)
                             (not (plist-get entry :cancelled)))
                    (e-harness--lifetime-tool-observation-frame
                     harness session-id turn-id entry payload)))
                :context context)))
	            (start-auto-compaction
	             (context)
	             (condition-case err
	                 (let (compaction-settled
	                       request)
	                   (setq
	                    request
	                    (e-harness-compact-session-start
	                     harness session-id
	                     :reason 'auto
	                     :allow-active-turn t
	                     :allow-split-turn nil
	                     :exclude-entry-ids
	                     (list (plist-get entry :prompt-message-id))
	                     :turn-id turn-id
	                     :on-done
	                     (lambda (_record)
	                       (setq compaction-settled t)
	                       (when (and (active-entry-p)
	                                  (not (plist-get entry :cancelled)))
	                         (start-provider
	                          (e-harness-turn-context
	                           harness session-id turn-id))))
	                     :on-error
	                     (lambda (err)
	                       (setq compaction-settled t)
	                       (when (and (active-entry-p)
	                                  (not (plist-get entry :cancelled)))
	                         (if (eq (car err) 'e-compaction-error)
	                             (start-provider context)
	                           (finish-error err))))))
	                   (when (and (active-entry-p)
	                              (not compaction-settled)
	                              (not (plist-get entry :cancelled)))
	                     (plist-put entry :request request)))
	               (e-compaction-error
	                (when (and (active-entry-p)
	                           (not (plist-get entry :cancelled)))
	                  (start-provider context)))
	               (error
	                (finish-error err))))
	            (start-turn
	             ()
	             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
	               (let ((context (e-harness-turn-context
	                               harness session-id turn-id))
	                     (excluded (list (plist-get entry :prompt-message-id))))
	                 (plist-put entry :timer nil)
	                 (if (and
	                      (e-harness--auto-compaction-needed-p
	                       harness session-id context)
	                      (e-harness--auto-compaction-useful-prefix-p
	                       harness session-id excluded))
		                     (start-auto-compaction context)
		                   (start-provider context))))))
	         (if (and delay (> delay 0))
	             (plist-put entry :timer (run-at-time delay nil #'start-turn))
	           (start-turn)))
	       turn-id))))

(cl-defun e-harness--follow-up-attached-batch
    (harness session-id prompt &key metadata attachment-token)
  "Synchronously prompt a follow-up from explicit batch/test code."
  (e-harness--prompt-attached-batch
   harness session-id prompt :metadata metadata
   :attachment-token attachment-token))

(defun e-harness--run-session-reset-hooks (harness session-id)
  "Run `:session-reset' hooks for HARNESS SESSION-ID."
  (e-hooks-run-reduce
   (e-harness-hooks harness)
   :session-reset
   nil
   (list :harness harness
         :session-id session-id)))

(defun e-harness-reset (harness session-id)
  "Clear SESSION-ID transcript state in HARNESS."
  (e-session-clear-messages (e-harness-sessions harness) session-id)
  (when (e-harness-queued-prompts harness session-id)
    (e-harness--set-queued-prompts harness session-id nil)
    (e-harness--emit-queue-changed harness session-id))
  (e-harness--run-session-reset-hooks harness session-id)
  (e-harness--emit
   harness
   (e-events-make :type 'session-reset
                  :session-id session-id
                  :turn-id nil
                  :payload nil)))

(defun e-harness-state (harness session-id)
  "Return settled state for SESSION-ID in HARNESS."
  (let* ((entry (gethash session-id (e-harness-active-turns harness)))
         (session (ignore-errors
                    (e-session-get (e-harness-sessions harness) session-id))))
    (list :session-id session-id
          :active-turn (when (or (not (listp entry))
                                 (eq (plist-get entry :status) 'running))
                         (e-harness--active-turn-id entry))
          :message-count (or (plist-get session :message-count) 0))))

(defun e-harness--abort-attached (harness session-id attachment-token)
  "Abort the active turn for SESSION-ID in HARNESS."
  (e-harness--require-attached-port harness session-id attachment-token)
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    (if (listp entry)
        (let ((turn-id (plist-get entry :id)))
          (when-let ((timer (plist-get entry :timer)))
            (cancel-timer timer))
          (plist-put entry :cancelled t)
           (e-harness--cancel-active-request entry)
           (when-let ((turn-work (plist-get entry :work-handle)))
             (e-work-cancel turn-work))
          (e-harness--append-cancelled-tool-result
           harness session-id turn-id entry)
          (plist-put entry :status 'cancelled)
          (e-harness--emit-turn-event
           harness session-id turn-id 'turn-cancelled nil)
          (e-harness--drain-pending-steering-input harness entry)
          (e-harness--schedule-queue-drain harness session-id entry)
          entry)
      (signal 'e-harness-no-active-turn (list session-id)))))

(defun e-harness-wait-batch (harness session-id &optional timeout)
  "Wait for SESSION-ID's async turn in HARNESS from batch/test code.
TIMEOUT is in seconds.  Return the settled active-turn entry and clear it from
active state when it is no longer running."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-harness-wait-batch))
  (let ((deadline (and timeout (+ (float-time) timeout)))
        (entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    ;; Wait on the ENTRY object itself, not a fresh hash lookup each pass.
    ;; `finish-done'/`finish-error' mutate this plist in place, so its status
    ;; and result stay observable even after the queue-drain timer -- which can
    ;; fire inside `accept-process-output' below -- removes it from the hash.
    (while (and (e-harness--active-turn-running-p entry)
                (or (not deadline) (< (float-time) deadline)))
      (accept-process-output nil 0.01))
    ;; Only clear the slot when it still holds this settled entry; a drained
    ;; queue may already have replaced it with the next turn.
    (when (and (eq (gethash session-id (e-harness-active-turns harness)) entry)
               (not (e-harness--active-turn-running-p entry)))
      (e-harness--remove-active-turn harness session-id entry))
    entry))

(provide 'e-harness)

;;; e-harness.el ends here

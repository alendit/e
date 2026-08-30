;;; e-harness-capabilities.el --- Capability and layer derivation owner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns effective capability, layer, resource, tool, prompt, hook, structured
;; block, project-root, and workspace derivation.  It is a downward owner:
;; mutable harness identity is supplied by e-harness-state and no operation
;; calls back into the application facade.

;;; Code:

(require 'cl-lib)
(require 'e-harness-state)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-hooks)
(require 'e-layers)
(require 'e-message-details)
(require 'e-operations)
(require 'e-prompts)
(require 'e-resources)
(require 'e-session)
(require 'e-shells)
(require 'e-store)
(require 'e-structured-blocks)
(require 'e-tools)
(require 'e-work)
(require 'seq)
(require 'subr-x)

(defun e-harness-normalize-project-root (root)
  "Return normalized project ROOT, or nil.
This value operation belongs to capability/layer derivation and has no
dependency on the harness facade."
  (when (and (stringp root)
             (not (string-empty-p (string-trim root))))
    (file-name-as-directory (expand-file-name root))))

(defvar e-harness-capabilities--layer-change-functions
  (make-hash-table :test 'eq :weakness 'key)
  "Layer-change callbacks owned by capability derivation.")

(define-error 'e-harness-duplicate-action-capability
  "Duplicate action capability id")

(defun e-harness-capability-config (harness capability-id)
  "Return HARNESS-local runtime config plist for CAPABILITY-ID."
  (copy-sequence
   (alist-get capability-id
              (e-harness-runtime-capability-config harness))))

(defun e-harness-capabilities--effective-capability-config-cache (harness)
  "Return HARNESS's private effective configuration cache."
  (or (e-harness-capability-state-effective-cache
       (e-harness-capability-state harness))
      (let ((cache (make-hash-table :test 'equal)))
        (setf (e-harness-capability-state-effective-cache
               (e-harness-capability-state harness)) cache)
        cache)))

(defun e-harness-clear-effective-capability-config-cache (harness)
  "Discard all derived effective capability configuration for HARNESS.
Use this after changing harness-local runtime configuration outside the public
configuration setter."
  (setf (e-harness-capability-state-effective-cache
         (e-harness-capability-state harness)) nil)
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
             (cache (e-harness-capabilities--effective-capability-config-cache harness))
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

(defun e-harness-capabilities--append-layer-id (ids id)
  "Return IDS with ID appended once."
  (if (memq id ids)
      ids
    (append ids (list id))))

(defun e-harness-capabilities--effective-layer-ids-for-root (layer-ids directory)
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
              (setq resolved (e-harness-capabilities--append-layer-id resolved id))))))
      (dolist (id layer-ids)
        (visit id nil)))
    resolved))

(defun e-harness-effective-layer-ids (harness &optional session-id turn-id)
  "Return HARNESS enabled layer ids plus transitive requirements.
SESSION-ID and TURN-ID identify the root used for config-aware layer factories."
  (e-harness-capabilities--effective-layer-ids-for-root
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

(defun e-harness-capabilities--unique-action-capabilities (capabilities)
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
    (e-harness-capabilities--unique-action-capabilities
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
    (e-harness-capabilities--register-resource-operation-tools
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

(defun e-harness-capabilities--resource-method-description (method)
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

(defun e-harness-capabilities--resource-operation-description (resources operation)
  "Return model-facing description for OPERATION over active RESOURCES."
  (if (memq (e-operation-id operation)
            e-harness-lean-resource-operation-ids)
      (e-operation-description operation)
    (let ((methods (e-resources-methods-for-operation resources operation)))
      (string-join
       (list (e-operation-description operation)
             ""
             "Active URI schemes:"
             (mapconcat #'e-harness-capabilities--resource-method-description methods "\n"))
       "\n"))))

(defun e-harness-capabilities--resource-operation-metadata (operation uri)
  "Return compact resource usage metadata for OPERATION over URI."
  (e-tools-resource-usage-metadata
   (e-operation-tool-name operation)
   (list (list :uri uri
               :operation (e-operation-id operation)))))

(defun e-harness-capabilities--resource-operation-result (operation uri content)
  "Return CONTENT as the current resource OPERATION tool result when possible."
  (let ((call (plist-get (e-tools-current-context) :tool-call))
        (metadata (e-harness-capabilities--resource-operation-metadata operation uri)))
    (if call
        (e-tools-result-create call 'ok content metadata)
      content)))

(defun e-harness-capabilities--resource-operation-call (resources operation uri arguments)
  "Call resource OPERATION for URI with ARGUMENTS and wrap metadata."
  (e-harness-capabilities--resource-operation-result
   operation
   uri
   (apply #'e-resources-call resources operation uri arguments)))

(defun e-harness-capabilities--resource-method-work (method)
  "Return METHOD's work spec, tolerating older live resource records."
  (and (e-resource-method-p method)
       (>= (length method) 11)
       (e-resource-method-work method)))

(defun e-harness-capabilities--resource-operation-work (resources operation)
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
                 (method (e-resources-method-for-uri
                          resources operation uri))
                 (work (e-harness-capabilities--resource-method-work method)))
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
                                     (e-harness-capabilities--resource-operation-result
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
               (e-harness-capabilities--resource-operation-call
                resources operation uri operation-arguments)))))
        arguments))
     :deferred)))

(defun e-harness-capabilities--resource-operation-async-p (operation)
  "Return non-nil when OPERATION should expose an async tool start."
  (memq (e-operation-id-of operation) '(glob search table-of-content)))

(defun e-harness-capabilities--register-resource-operation-tool (registry resources operation)
  "Register OPERATION in REGISTRY as a model-facing tool backed by RESOURCES."
  (let ((dispatch (e-operation-dispatch operation)))
    (when (functionp dispatch)
      (let* ((async-p (e-harness-capabilities--resource-operation-async-p operation))
             (tool-name (e-operation-tool-name operation))
             (cheap-runner
              (lambda (arguments)
                (funcall dispatch
                         (lambda (uri &rest operation-arguments)
                           (e-harness-capabilities--resource-operation-call
                            resources operation uri operation-arguments))
                         arguments))))
        (e-tools-register
         registry
         :name tool-name
         :description (e-harness-capabilities--resource-operation-description resources operation)
         :parameters (if async-p
                         (e-work-detachable-merge-parameters
                          (e-operation-parameters operation))
                       (e-operation-parameters operation))
         :work (if async-p
                   (e-work-detachable-spec
                    (e-harness-capabilities--resource-operation-work resources operation)
                    :id (format "resource.%s" tool-name)
                    :owner 'resources)
                 (e-tools-cheap-work
                  (format "resource.%s" tool-name)
                  cheap-runner
                  :owner 'resources))
         :blocking-class (if async-p 'process 'cheap))))))

(defun e-harness-capabilities--register-resource-operation-tools (registry resources)
  "Register active resource operation tools in REGISTRY backed by RESOURCES."
  (dolist (operation (e-resources-operations resources))
    (when (e-operation-p operation)
      (e-harness-capabilities--register-resource-operation-tool registry resources operation))))

(defun e-harness-register-resource-tools (registry resources)
  "Register resource operation tools from RESOURCES in REGISTRY."
  (e-harness-capabilities--register-resource-operation-tools registry resources))

(defun e-harness-layer-change-function (harness)
  "Return HARNESS layer-change callback, or nil."
  (gethash harness e-harness-capabilities--layer-change-functions))

(defun e-harness-set-layer-change-function (harness function)
  "Set HARNESS layer-change callback to FUNCTION.
When FUNCTION is nil, clear any existing callback."
  (if function
      (puthash harness function e-harness-capabilities--layer-change-functions)
    (remhash harness e-harness-capabilities--layer-change-functions))
  function)

(defun e-harness-notify-layers-changed (harness)
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
  (e-harness-notify-layers-changed harness)
  (e-harness-enabled-layer-ids harness))

(defun e-harness-sync-layer-shells (harness &optional directory)
  "Rebuild HARNESS presentation shells from enabled layers.
DIRECTORY is used for config-aware shell layer factories; when nil, use the
harness default project root."
  (let ((root (or (e-harness-normalize-project-root directory)
                  (e-harness-default-project-root harness))))
    (e-shell-clear-harness-shells harness)
    (dolist (id (e-harness-capabilities--effective-layer-ids-for-root
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
           (e-harness-capabilities--append-layer-id
            (e-harness-enabled-layer-ids harness)
            layer-id))
          (root (or (e-harness-normalize-project-root directory)
                    (e-harness-default-project-root harness))))
      ;; Resolve the complete prospective graph before mutating harness state.
      ;; An unknown id or dependency must fail without poisoning later turns.
      (e-harness-capabilities--effective-layer-ids-for-root prospective-ids root)
      (setf (e-harness-enabled-layer-ids harness) prospective-ids))
    (e-harness-sync-layer-shells harness directory)
    (e-harness-notify-layers-changed harness))
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
      (e-harness-notify-layers-changed harness))
    (let ((effective (e-harness-layer-effective-p harness layer-id)))
      (list :status (cond
                     ((not was-enabled) 'already-disabled)
                     (effective 'disabled-but-required)
                     (t 'disabled))
            :layer-id layer-id
            :enabled nil
            :active effective))))
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
       (e-harness-normalize-project-root
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
      (when-let ((root (e-harness-normalize-project-root project)))
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

(defun e-harness-capabilities--root-at-or-below-p (target root)
  "Return non-nil when normalized TARGET is at or below normalized ROOT."
  (when-let ((target (e-harness-normalize-project-root target))
             (root (e-harness-normalize-project-root root)))
    (string-prefix-p (file-truename root) (file-truename target))))

(defun e-harness-configured-workspace-roots (primary-root)
  "Return configured extra and default project roots for PRIMARY-ROOT.
Collects EXTRA-ROOTS from `e-workspace-roots-alist' entries whose key is an
ancestor of (or equal to) PRIMARY-ROOT, followed by `e-default-projects'.
Returns a normalized, de-duplicated list, excluding PRIMARY-ROOT itself."
  (when-let ((primary (e-harness-normalize-project-root primary-root)))
    (let (roots)
      (dolist (entry e-workspace-roots-alist)
        (when (e-harness-capabilities--root-at-or-below-p primary (car entry))
          (dolist (extra (cdr entry))
            (when-let ((extra (e-harness-normalize-project-root extra)))
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
  (when-let ((primary (e-harness-normalize-project-root
                       (e-harness-project-root harness session-id turn-id))))
    (cons primary (e-harness-configured-workspace-roots primary))))

(provide 'e-harness-capabilities)

;;; e-harness-capabilities.el ends here

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
(require 'e-events)
(require 'e-hooks)
(require 'e-layers)
(require 'e-loop)
(require 'e-operations)
(require 'e-prompts)
(require 'e-resources)
(require 'e-session)
(require 'e-shells)
(require 'e-store)
(require 'e-tools)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-harness-no-active-turn "No active turn")
(define-error 'e-harness-active-turn-exists
  "Session already has an active turn")

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
  prompt-queues)

(defvar e-harness--layer-change-functions (make-hash-table :test 'eq :weakness 'key)
  "Layer change callbacks keyed by harness.")

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

(defun e-harness-refresh-default-context-strategy (harness)
  "Refresh HARNESS default context strategy, preserving custom strategies."
  (when (e-context-transcript-stack-p (e-harness-context-strategy harness))
    (setf (e-harness-context-strategy harness)
          (e-context-transcript-stack-create)))
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
                          :prompt-queues (make-hash-table :test 'equal))))
    (when layer-change-function
      (e-harness-set-layer-change-function harness layer-change-function))
    harness))

(defun e-harness-capability-config (harness capability-id)
  "Return HARNESS-local runtime config plist for CAPABILITY-ID."
  (copy-sequence
   (alist-get capability-id
              (e-harness-runtime-capability-config harness))))

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
    config))

(cl-defun e-harness-effective-capability-config
    (harness capability-id options &key session-id directory overrides)
  "Return effective CAPABILITY-ID config for HARNESS.
Resolution uses DIRECTORY or the session project root, then HARNESS-local
runtime config, then explicit OVERRIDES."
  (e-capability-config-resolve
   capability-id
   options
   :directory (or directory
                  (e-harness-project-root harness session-id))
   :runtime-config (e-harness-capability-config harness capability-id)
   :overrides overrides))

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
       :turn-id turn-id))
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

(defun e-harness--resource-operation-description (resources operation)
  "Return model-facing description for OPERATION over active RESOURCES."
  (let ((methods (e-resources-methods-for-operation resources operation)))
    (string-join
     (list (e-operation-description operation)
           ""
           "Active URI schemes:"
           (mapconcat #'e-harness--resource-method-description methods "\n"))
     "\n")))

(defun e-harness--register-resource-operation-tool (registry resources operation)
  "Register OPERATION in REGISTRY as a model-facing tool backed by RESOURCES."
  (let ((dispatch (e-operation-dispatch operation)))
    (when (functionp dispatch)
      (e-tools-register
       registry
       :name (e-operation-tool-name operation)
       :description (e-harness--resource-operation-description resources operation)
       :parameters (e-operation-parameters operation)
       :handler
       (lambda (arguments)
         (funcall dispatch
                  (lambda (uri &rest operation-arguments)
                    (let* ((content (apply #'e-resources-call
                                           resources
                                           operation
                                           uri
                                           operation-arguments))
                           (call (plist-get (e-tools-current-context)
                                            :tool-call))
                           (metadata
                            (e-tools-resource-usage-metadata
                             (e-operation-tool-name operation)
                             (list (list :uri uri
                                         :operation
                                         (e-operation-id-of operation))))))
                      (if call
                          (e-tools-result-create call 'ok content metadata)
                        content)))
                  arguments))))))

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
    (setf (e-harness-enabled-layer-ids harness)
          (e-harness--append-layer-id
           (e-harness-enabled-layer-ids harness)
           layer-id))
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
  "Return configured extra workspace roots active for PRIMARY-ROOT.
Collects EXTRA-ROOTS from `e-workspace-roots-alist' entries whose key is an
ancestor of (or equal to) PRIMARY-ROOT.  Returns a normalized, de-duplicated
list, excluding PRIMARY-ROOT itself."
  (when-let ((primary (e-harness--normalize-project-root primary-root)))
    (let (roots)
      (dolist (entry e-workspace-roots-alist)
        (when (e-harness--root-at-or-below-p primary (car entry))
          (dolist (extra (cdr entry))
            (when-let ((extra (e-harness--normalize-project-root extra)))
              (unless (or (equal extra primary) (member extra roots))
                (push extra roots))))))
      (nreverse roots))))

(defun e-harness-workspace-roots (harness &optional session-id turn-id)
  "Return active workspace roots for HARNESS SESSION-ID as a normalized list.
The first element is the primary project root (the base for relative paths and
the bash working directory); the rest are configured extra roots from
`e-workspace-roots-alist'.  Returns nil when no primary root is resolvable."
  (when-let ((primary (e-harness--normalize-project-root
                       (e-harness-project-root harness session-id turn-id))))
    (cons primary (e-harness-configured-workspace-roots primary))))

(cl-defun e-harness-subscribe (harness subscriber &key session-id)
  "Register SUBSCRIBER for core events from HARNESS.
When SESSION-ID is non-nil, SUBSCRIBER only receives events for that session."
  (let ((record (list :callback subscriber :session-id session-id)))
    (push record (e-harness-subscribers harness))
    record))

(defun e-harness-unsubscribe (harness subscription)
  "Remove SUBSCRIPTION from HARNESS subscribers.
SUBSCRIPTION should be a record returned by `e-harness-subscribe'.  Removing
an already-removed record is a no-op."
  (setf (e-harness-subscribers harness)
        (delq subscription (e-harness-subscribers harness)))
  nil)

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
    reasoning-delta tool-started tool-finished turn-finished token-usage
    turn-failed turn-cancelled turn-steered backend-empty-output
    compaction-started compaction-prepared compaction-summary-started
    compaction-finished compaction-failed)
  "Turn event types stored as durable session activity.")

(defconst e-harness--activity-index-flush-event-types
  '(turn-finished turn-failed turn-cancelled backend-empty-output
    compaction-finished compaction-failed)
  "Durable activity event types that should flush the session index.")

(defcustom e-harness-durable-tool-finished-result-preview-bytes 4096
  "Maximum UTF-8 bytes retained from tool results in durable activity events.

Tool transcript messages retain the model-visible tool result.  Durable
`tool-finished' activity is presentation history, so it stores only a compact
preview to avoid duplicating large outputs in session JSONL files."
  :type 'integer
  :group 'e)

(defcustom e-harness-retry-max-elapsed-seconds 600.0
  "Total wall-clock budget for retrying a transient backend turn.
Retries of a turn that keeps failing with a retryable error (rate limiting,
overload, or a transport reset) stop once this much time has elapsed since the
first attempt; the turn then settles as `turn-failed' as before.  The default
mirrors Claude Code's patience: several minutes of backoff across roughly ten
attempts.  Set to 0 to disable retrying."
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

(defconst e-harness--retryable-error-patterns
  '("rate limit"
    "rate_limit_error"
    "too many requests"
    "overloaded"
    "overloaded_error"
    "api_error"
    "internal_server_error"
    "service unavailable"
    "bad gateway"
    "gateway time"
    "connection termination"
    "connection reset"
    "reset by peer"
    "connect error"
    "before headers"
    "disconnect"
    "broken pipe"
    "premature")
  "Lower-cased substrings marking a transient, retryable backend error.
These cover rate limiting, provider overload (HTTP 529 / `overloaded_error'),
and transport-level failures such as the Envoy \"upstream connect error or
disconnect/reset before headers\" body returned when a connection is reset
before the Messages stream starts.")

(defun e-harness--retryable-status-p (status)
  "Return non-nil when HTTP STATUS marks a transient, retryable failure.
408 (request timeout), 409 (conflict), 429 (rate limit), and every 5xx server
error (500, 502, 503, 504, the 529 Anthropic overload code) are retryable, in
line with the Anthropic SDK retry policy.  4xx client errors other than 408/409
are genuine faults and are not retried."
  (and (numberp status)
       (or (= status 408)
           (= status 409)
           (= status 429)
           (>= status 500))))

(defun e-harness--retryable-error-p (message details)
  "Return non-nil when a backend error (MESSAGE, DETAILS) should be retried.
Retryable errors are transient: rate limiting (HTTP 429), provider overload
\(HTTP 529 / `overloaded_error'), and transport resets that drop the connection
before or during the Messages stream.  Genuine faults (HTTP 500, malformed
requests) are not retried."
  (let ((text (downcase (or message ""))))
    (or (string-match-p "\\(^\\|[^0-9]\\)429\\([^0-9]\\|$\\)" (or message ""))
        (string-match-p "\\(^\\|[^0-9]\\)529\\([^0-9]\\|$\\)" (or message ""))
        (seq-some (lambda (pat) (string-match-p (regexp-quote pat) text))
                  e-harness--retryable-error-patterns)
        (let ((status (and (listp details)
                           (or (plist-get details :status)
                               (plist-get details :status-code)
                               (plist-get details :code)))))
          (e-harness--retryable-status-p status)))))

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

(defun e-harness--durable-activity-event-p (type)
  "Return non-nil when TYPE should be stored as session activity."
  (memq type e-harness--durable-activity-event-types))

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

(defun e-harness--compact-tool-result-for-activity (result)
  "Return compact durable activity representation of tool RESULT."
  (let* ((content (plist-get result :content))
         (content-text (e-tools-result-content-text content))
         (original-bytes (string-bytes content-text))
         (max-bytes (max 0 e-harness-durable-tool-finished-result-preview-bytes))
         (truncated (> original-bytes max-bytes))
         (preview (if truncated
                      (e-harness--string-byte-prefix content-text max-bytes)
                    content-text))
         (metadata (copy-sequence (plist-get result :metadata)))
         (summary (list :tool-call-id (plist-get result :tool-call-id)
                        :name (plist-get result :name)
                        :status (plist-get result :status)
                        :content preview
                        :metadata metadata)))
    (when truncated
      (setq metadata (plist-put metadata :activity-truncated t))
      (setq metadata (plist-put metadata :activity-original-bytes
                                original-bytes))
      (setq metadata (plist-put metadata :activity-shown-bytes
                                (string-bytes preview)))
      (plist-put summary :metadata metadata))
    summary))

(defun e-harness--compact-tool-finished-payload (payload)
  "Return PAYLOAD with a compact `:result' for durable activity storage."
  (if (and (listp payload) (e-tools-result-p (plist-get payload :result)))
      (let ((copy (copy-sequence payload)))
        (plist-put copy
                   :result
                   (e-harness--compact-tool-result-for-activity
                    (plist-get payload :result)))
        copy)
    payload))

(defun e-harness--durable-activity-payload (type payload)
  "Return durable activity PAYLOAD for event TYPE."
  (pcase type
    ('tool-finished (e-harness--compact-tool-finished-payload payload))
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
       (e-session-append-activity-event
        store
        session-id
        turn-id
        type
        (e-harness--durable-activity-payload type payload)
        :write-index nil)
       (when (e-harness--activity-index-flush-event-p type)
         (e-session--write-index store))))))

(defun e-harness--emit-turn-event (harness session-id turn-id type payload)
  "Emit public event TYPE with PAYLOAD for HARNESS SESSION-ID TURN-ID."
  (when (and session-id
             turn-id
             (e-harness--durable-activity-event-p type)
             (ignore-errors
               (e-session-get (e-harness-sessions harness) session-id)))
    (e-harness--append-durable-activity-event
     harness session-id turn-id type payload))
  (e-harness--emit
   harness
   (e-events-make :type type
                  :session-id session-id
                  :turn-id turn-id
                  :payload payload)))

(defun e-harness-messages (harness session-id)
  "Return messages for SESSION-ID in HARNESS."
  (e-session-messages (e-harness-sessions harness) session-id))

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

(defun e-harness--set-queued-prompts (harness session-id items)
  "Replace SESSION-ID queued prompt ITEMS in HARNESS."
  (if items
      (puthash session-id items (e-harness-prompt-queues harness))
    (remhash session-id (e-harness-prompt-queues harness)))
  items)

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

(cl-defun e-harness-queue-prompt
    (harness session-id prompt &key references metadata)
  "Queue PROMPT as a follow-up for SESSION-ID in HARNESS.
The session must currently have a running active turn."
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
    (e-harness--set-queued-prompts harness session-id items)
    (e-harness--emit-queue-changed harness session-id)
    queue-id))

(defun e-harness--steering-prompt-preview (prompt)
  "Return compact activity preview for steering PROMPT."
  (let ((text (string-trim
               (replace-regexp-in-string "[\n\r\t ]+" " " prompt))))
    (e-harness--string-byte-prefix text 160)))

(defun e-harness--pending-steering-items (entry)
  "Return pending steering items from active turn ENTRY."
  (and (listp entry)
       (plist-get entry :pending-steering-input)))

(defun e-harness--append-pending-steering-item (entry prompt metadata)
  "Append PROMPT and METADATA as pending steering input on ENTRY."
  (plist-put entry
             :pending-steering-input
             (append (e-harness--pending-steering-items entry)
                     (list (list :prompt prompt
                                 :metadata (copy-sequence metadata))))))

(defun e-harness--drain-pending-steering-input (entry)
  "Return and clear pending steering items from active turn ENTRY."
  (let ((items (e-harness--pending-steering-items entry)))
    (when items
      (plist-put entry :pending-steering-input nil)
      items)))

(cl-defun e-harness-steer-active-turn
    (harness session-id prompt &key metadata)
  "Steer SESSION-ID's running active turn with PROMPT in HARNESS."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (unless (e-harness--active-turn-running-p entry)
      (signal 'e-harness-no-active-turn (list session-id)))
    (let ((turn-id (plist-get entry :id)))
      (e-harness--append-pending-steering-item entry prompt metadata)
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
      (remhash session-id (e-harness-active-turns harness)))
    (unless (e-harness--active-turn-running-p
             (gethash session-id (e-harness-active-turns harness)))
      (when-let ((item (car (e-harness-queued-prompts harness session-id))))
        (e-harness--set-queued-prompts
         harness session-id
         (cdr (e-harness-queued-prompts harness session-id)))
        (e-harness--emit-queue-changed harness session-id)
        (e-harness-prompt-async
         harness
         session-id
         (plist-get item :prompt)
         :metadata (e-harness--queue-item-metadata item))))))

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

(defun e-harness-session-activity-events (harness session-id)
  "Return activity events for SESSION-ID in HARNESS."
  (e-session-activity-events (e-harness-sessions harness) session-id))

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

(defconst e-harness-prompt-cache-key-version "pcctx1"
  "Version marker for derived prompt cache keys.")

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
  (format "e:%s:%s:%s:%s:%s"
          e-harness-prompt-cache-key-version
          (e-harness--prompt-cache-hash (plist-get options :model) 8)
          (e-harness--prompt-cache-hash
           (e-harness-project-root harness session-id)
           16)
          (e-harness--prompt-cache-hash
           (e-harness-effective-layer-ids harness session-id)
           16)
          (e-harness--prompt-cache-hash
           (mapcar (lambda (tool) (plist-get tool :name))
                   (or (plist-get options :tools)
                       (e-tools-definitions
                        (e-harness-tools harness session-id))))
           16)))

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
    (e-harness--apply-prompt-cache-defaults harness session-id with-tools)))

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
         result
         failure)
    (e-harness--emit-turn-event
     harness
     session-id
     turn-id
     'tool-started
     (e-harness--nested-tool-payload
      prepared parent-tool-call depth))
    (e-tools-start
     tools
     prepared
     :context context
     :on-done (lambda (value) (setq result value))
     :on-error (lambda (err) (setq failure err)))
    (while (not (or result failure))
      (accept-process-output nil 0.01))
    (when failure
      (signal (car failure) (cdr failure)))
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
  (let ((context
         (list :harness harness
               :session-id session-id
               :turn-id turn-id
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
  (let ((tools (e-harness-tools harness session-id turn-id)))
    (cl-labels
        ((hooks ()
           (e-harness-hooks harness))
         (context ()
           (e-harness--tool-hook-context harness session-id turn-id tools)))
      (e-tool-lifecycle-create
       :prepare (lambda (tool-call)
                  (e-hooks-run-reduce
                   (hooks)
                   :pre-tool-call
                   tool-call
                   (context)))
       :start
       (cl-function
        (lambda (tool-call &key on-request-start on-done on-error on-event)
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

(defun e-harness-context (harness session-id &optional turn-id)
  "Return backend-neutral context for SESSION-ID in HARNESS.
TURN-ID is passed to active capability context providers when present."
  (e-harness--profile-call
   'harness.context
   (list :session-id session-id
         :turn-id turn-id)
   (lambda ()
     (let ((capability-context
            (e-capabilities-context
             (e-harness-effective-capabilities harness session-id turn-id)
             :harness harness
             :session-id session-id
             :turn-id turn-id)))
       (let ((context
              (e-context-build
               (e-harness-context-strategy harness)
               :sessions (e-harness-sessions harness)
               :session-id session-id
               :options (e-harness-turn-options harness session-id)
               :prefix-messages (plist-get capability-context :messages)
               :prefix-segments (plist-get capability-context :segments))))
         (plist-put context
                    :provider-anchor-active-layer-ids
                    (e-harness--effective-layer-id-strings
                     harness session-id turn-id))
         (plist-put context
                    :provider-anchor-compaction-boundary
                    (e-harness--provider-anchor-compaction-boundary
                     harness session-id))
         (e-harness--context-with-provider-anchor
          harness
          session-id
          context))))))

(defun e-harness--active-turn-id (entry)
  "Return active turn id from ENTRY."
  (if (listp entry)
      (plist-get entry :id)
    entry))

(defun e-harness--active-turn-running-p (entry)
  "Return non-nil when active turn ENTRY is still running."
  (and (listp entry)
       (eq (plist-get entry :status) 'running)))

(cl-defun e-harness-compact-session
    (harness session-id &key instructions keep-recent-tokens allow-active-turn
             (allow-split-turn 'inherit-active-turn) exclude-entry-ids turn-id
             (reason 'manual))
  "Manually compact SESSION-ID in HARNESS and append a durable record."
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
                 :reason reason))
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
          (e-backend-stream
           (e-harness-backend harness)
           :messages (e-compaction-summary-messages preparation)
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
            (let ((record
                   (e-session-append-compaction
                    (e-harness-sessions harness)
                    session-id
                    summary
                    :first-kept-entry-id
                    (plist-get preparation :first-kept-entry-id)
                    :tokens-before (plist-get preparation :tokens-before)
                    :tokens-kept (plist-get preparation :tokens-kept)
                    :metadata metadata)))
              (e-harness--emit-turn-event
               harness session-id turn-id 'compaction-finished
               (list :compaction-id (plist-get record :id)
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
          (apply #'e-harness-compact-session harness session-id args))
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
   (t (error-message-string err))))

(defun e-harness--backend-error-details (err)
  "Return structured provider details from condition ERR, or nil."
  (when (consp err)
    (pcase (car err)
      ('e-loop-backend-error
       (nth 2 err))
      ('e-compaction-error
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

(defun e-harness--append-user-message
    (harness session-id turn-id prompt &optional metadata)
  "Append PROMPT as the user message in HARNESS for SESSION-ID and TURN-ID."
  (e-harness--append-message
   harness
   session-id
   turn-id
   (list :role 'user
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

(defun e-harness--provider-anchor-fingerprints (context)
  "Return JSON-stable provider-relevant fingerprints from CONTEXT."
  (let ((options (plist-get context :options)))
    (list
     :segments
     (mapcar
      (lambda (segment)
        (list :kind (symbol-name (plist-get segment :kind))
              :id (prin1-to-string (plist-get segment :id))
              :fingerprint (plist-get segment :fingerprint)))
      (cl-remove-if
       (lambda (segment)
         (memq (plist-get segment :kind) '(history delta)))
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
     (plist-get context :provider-anchor-compaction-boundary))))

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
  (cl-loop for segment in (plist-get context :segments)
           when (memq (plist-get segment :kind)
                      '(current-state dynamic-context))
           append (mapcar #'e-context--backend-message
                          (plist-get segment :messages))))

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
     (mapcar #'e-context--backend-message
             (cl-remove-if-not
              (lambda (entry)
                (eq (plist-get entry :type) 'message))
              entries)))))

(defun e-harness--context-with-provider-anchor (harness session-id context)
  "Attach a compatible provider anchor to CONTEXT options when available."
  (let* ((options (plist-get context :options))
         (provider-id (plist-get options :provider-anchor-provider-id))
         (fingerprints (e-harness--provider-anchor-fingerprints context)))
    (when (and (plist-get options :provider-continuation) provider-id)
      (let ((anchor
             (e-session-latest-compatible-provider-anchor
              (e-harness-sessions harness)
              session-id
              provider-id
              :model (plist-get options :model)
              :fingerprints fingerprints))
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
                  fingerprints))))
        (plist-put context :options options)))
    context))

(defun e-harness--provider-anchor-candidate-persistable-p (context candidate)
  "Return non-nil when CANDIDATE is durable for CONTEXT."
  (let ((provider-id (plist-get candidate :provider-id))
        (options (plist-get context :options)))
    (and provider-id
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
    (harness session-id turn-id context candidates)
  "Persist provider anchor CANDIDATES for completed TURN-ID."
  (when-let ((assistant-message
              (e-harness--turn-assistant-message harness session-id turn-id)))
    (dolist (candidate
             (e-harness--latest-provider-anchor-candidates candidates))
      (when (e-harness--provider-anchor-candidate-persistable-p
             context candidate)
        (e-session-append-provider-anchor
         (e-harness-sessions harness)
         session-id
         (plist-get candidate :provider-id)
         :model (plist-get (plist-get context :options) :model)
         :covered-entry-id (plist-get assistant-message :id)
         :fingerprints (e-harness--provider-anchor-fingerprints context)
         :metadata (plist-get candidate :metadata))))))

(defun e-harness--run-turn-finished-hooks
    (harness session-id turn-id result)
  "Run `:turn-finished' hooks for HARNESS SESSION-ID TURN-ID over RESULT."
  (e-hooks-run-reduce
   (e-harness-hooks harness)
   :turn-finished
   result
   (list :harness harness
         :session-id session-id
         :turn-id turn-id
         :assistant-message
         (e-harness--turn-assistant-message harness session-id turn-id))))

(cl-defun e-harness--run-prompt-turn
    (harness session-id turn-id &key on-request-start)
  "Run the queued prompt turn for SESSION-ID and TURN-ID in HARNESS."
  (e-harness--profile-call
   'harness.prompt-turn
   (list :session-id session-id
         :turn-id turn-id)
   (lambda ()
     (let ((context (e-harness-context harness session-id turn-id)))
       (e-loop-run-turn
        :session-id session-id
        :turn-id turn-id
        :messages (plist-get context :messages)
        :backend (e-harness-backend harness)
        :tools (e-harness-tools harness session-id turn-id)
        :tool-lifecycle (e-harness-tool-lifecycle harness session-id turn-id)
        :options (plist-get context :options)
        :on-event (lambda (type payload)
                    (e-harness--emit-turn-event
                     harness session-id turn-id type payload))
        :on-request-start on-request-start
        :refresh-messages
        (lambda ()
          (plist-get (e-harness-context harness session-id turn-id)
                     :messages))
        :append-message
        (lambda (message)
          (e-harness--append-message harness session-id turn-id message)))))))

(cl-defun e-harness--run-prompt-turn-async
    (harness session-id turn-id &key on-request-start on-done on-error
             cancelled-p append-message on-event context drain-pending-input)
  "Start a queued async prompt turn for SESSION-ID and TURN-ID in HARNESS."
  (e-harness--profile-call
   'harness.prompt-turn-async-start
   (list :session-id session-id
         :turn-id turn-id)
   (lambda ()
     (let ((context (or context
                        (e-harness-context harness session-id turn-id))))
       (e-loop-start-turn
        :session-id session-id
        :turn-id turn-id
        :messages (plist-get context :messages)
        :backend (e-harness-backend harness)
        :tools (e-harness-tools harness session-id turn-id)
        :tool-lifecycle (e-harness-tool-lifecycle harness session-id turn-id)
        :options (plist-get context :options)
        :on-event (or on-event
                      (lambda (type payload)
                        (e-harness--emit-turn-event
                         harness session-id turn-id type payload)))
        :on-request-start on-request-start
        :cancelled-p cancelled-p
        :on-done on-done
        :on-error on-error
        :refresh-messages
        (lambda ()
          (plist-get (e-harness-context harness session-id turn-id)
                     :messages))
        :drain-pending-input
        (or drain-pending-input
            (lambda ()
              (mapcar
               (lambda (item)
                 (list :role 'user
                       :content (plist-get item :prompt)
                       :metadata (plist-get item :metadata)))
               (e-harness--drain-pending-steering-input
                (gethash session-id
                         (e-harness-active-turns harness))))))
        :append-message
        (or append-message
            (lambda (message)
              (e-harness--append-message
               harness session-id turn-id message))))))))

(cl-defun e-harness-prompt (harness session-id prompt &key metadata)
  "Append PROMPT and run one backend turn for SESSION-ID in HARNESS.
This is the synchronous convenience wrapper over `e-harness-prompt-async'."
  (e-harness--profile-call
   'harness.prompt
   (list :session-id session-id)
   (lambda ()
     (e-harness-prompt-async harness session-id prompt :metadata metadata)
     (let ((entry (e-harness-wait harness session-id)))
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

(cl-defun e-harness-prompt-async
    (harness session-id prompt &key delay metadata)
  "Append PROMPT and run one backend turn asynchronously in HARNESS.
Return the queued turn id.  DELAY is primarily for tests and queued-turn
cancellation.  SESSION-ID identifies the session."
  (e-harness--profile-call
   'harness.prompt-async
   (list :session-id session-id)
   (lambda ()
     (when (e-harness--active-turn-running-p
            (gethash session-id (e-harness-active-turns harness)))
       (signal 'e-harness-active-turn-exists (list session-id)))
     (let* ((turn-id (e-harness--next-turn-id))
            (entry (list :id turn-id
                         :status 'running
                         :result nil
                         :error nil
                         :error-details nil
                         :condition nil
                         :timer nil
                         :request nil)))
       (puthash session-id entry (e-harness-active-turns harness))
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
            (remhash session-id (e-harness-active-turns harness))
            (signal (car err) (cdr err)))))
       (cl-labels
           ((active-entry-p ()
              (eq (gethash session-id (e-harness-active-turns harness))
                  entry))
            (cancelled-p ()
              (or (plist-get entry :cancelled)
                  (not (active-entry-p))))
            (maybe-retry-error
             (message details)
             ;; Schedule a backoff retry for a retryable error (e.g. 429)
             ;; while inside the elapsed budget.  Return non-nil when a retry
             ;; was scheduled so the caller skips settling the turn as failed.
             (when (and (> e-harness-retry-max-elapsed-seconds 0)
                        (e-harness--retryable-error-p message details))
               (let* ((deadline (or (plist-get entry :retry-deadline)
                                    (+ (float-time)
                                       e-harness-retry-max-elapsed-seconds)))
                      (attempt (1+ (or (plist-get entry :retry-attempt) 0)))
                      (backoff (e-harness--retry-backoff-seconds attempt)))
                 (plist-put entry :retry-deadline deadline)
                 (when (< (+ (float-time) backoff) deadline)
                   (plist-put entry :retry-attempt attempt)
                   (e-harness--emit-turn-event
                    harness session-id turn-id 'turn-retrying
                    (list :error message
                          :details details
                          :attempt attempt
                          :backoff-seconds backoff))
                   (plist-put entry :timer
                              (run-at-time backoff nil #'start-turn))
                   t))))
            (finish-error
             (err)
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (let ((message (e-harness--backend-error-message err))
                     (details (e-harness--backend-error-details err)))
                 (unless (maybe-retry-error message details)
                   (plist-put entry :status 'error)
                   (plist-put entry :condition err)
                   (plist-put entry :error message)
                   (plist-put entry :error-details details)
                   (e-harness--emit-turn-failed
                    harness session-id turn-id message details)
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
                (nreverse (plist-get entry :provider-anchor-candidates)))
               (let ((hooked-result
                      (e-harness--run-turn-finished-hooks
                       harness session-id turn-id result)))
                 (plist-put entry :result hooked-result)
                 (plist-put entry :status 'done)
                 (e-harness--schedule-queue-drain
                  harness session-id entry))))
            (start-turn
             ()
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (let ((context (e-harness-context harness session-id turn-id)))
                 (plist-put entry :timer nil)
                 (when (e-harness--maybe-auto-compact-session
                        harness
                        session-id
                        turn-id
                        (list (plist-get entry :prompt-message-id))
                        context)
                   (setq context
                         (e-harness-context harness session-id turn-id)))
                 (plist-put entry :context context))
               (plist-put entry :provider-anchor-candidates nil)
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
                      ('provider-anchor-candidate
                       (plist-put
                        entry
                        :provider-anchor-candidates
                        (cons payload
                              (plist-get
                               entry
                               :provider-anchor-candidates)))))
                    (e-harness--emit-turn-event
                     harness session-id turn-id type payload)))
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
                     (e-harness--drain-pending-steering-input entry))))
                :context (plist-get entry :context)))))
         (if (and delay (> delay 0))
             (plist-put entry :timer (run-at-time delay nil #'start-turn))
           (start-turn)))
       turn-id))))

(cl-defun e-harness-follow-up (harness session-id prompt &key metadata)
  "Submit PROMPT as the next turn for SESSION-ID in HARNESS."
  (e-harness-prompt harness session-id prompt :metadata metadata))

(defun e-harness-reset (harness session-id)
  "Clear SESSION-ID transcript state in HARNESS."
  (e-session-clear-messages (e-harness-sessions harness) session-id)
  (when (e-harness-queued-prompts harness session-id)
    (e-harness--set-queued-prompts harness session-id nil)
    (e-harness--emit-queue-changed harness session-id))
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

(defun e-harness-abort (harness session-id)
  "Abort the active turn for SESSION-ID in HARNESS."
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    (if (listp entry)
        (let ((turn-id (plist-get entry :id)))
          (when-let ((timer (plist-get entry :timer)))
            (cancel-timer timer))
          (plist-put entry :cancelled t)
          (e-harness--cancel-active-request entry)
          (e-harness--append-cancelled-tool-result
           harness session-id turn-id entry)
          (plist-put entry :status 'cancelled)
          (e-harness--emit-turn-event
           harness session-id turn-id 'turn-cancelled nil)
          (e-harness--schedule-queue-drain harness session-id entry)
          entry)
      (signal 'e-harness-no-active-turn (list session-id)))))

(defun e-harness-wait (harness session-id &optional timeout)
  "Wait for SESSION-ID's async turn in HARNESS to settle.
TIMEOUT is in seconds.  Return the settled active-turn entry and clear it from
active state when it is no longer running."
  (let ((deadline (and timeout (+ (float-time) timeout)))
        (entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    (while (and (e-harness--active-turn-running-p entry)
                (or (not deadline) (< (float-time) deadline)))
      (accept-process-output nil 0.01)
      (setq entry (gethash session-id (e-harness-active-turns harness))))
    (unless (e-harness--active-turn-running-p entry)
      (remhash session-id (e-harness-active-turns harness)))
    entry))

(provide 'e-harness)

;;; e-harness.el ends here

;;; e-harness.el --- Core harness service for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public pure core harness service.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-compaction)
(require 'e-context)
(require 'e-events)
(require 'e-hooks)
(require 'e-layers)
(require 'e-loop)
(require 'e-operations)
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

(cl-defstruct (e-harness (:constructor e-harness--make))
  backend
  context-strategy
  default-options
  default-project-root
  runtime-capability-config
  (sessions (e-session-store-create))
  (active-layers nil)
  (subscribers nil)
  active-turns)

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
          sessions active-layers
          project-root layer-change-function)
  "Create a core harness.
BACKEND, CONTEXT-STRATEGY, DEFAULT-OPTIONS, CAPABILITY-CONFIG, SESSIONS,
ACTIVE-LAYERS, PROJECT-ROOT, and LAYER-CHANGE-FUNCTION configure the
provider-neutral runtime.

When LAYER-CHANGE-FUNCTION is non-nil, it is called with HARNESS after public
layer activation or deactivation APIs change the active layer set."
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
                          :active-layers nil
                          :active-turns (make-hash-table :test 'equal))))
    (when layer-change-function
      (e-harness-set-layer-change-function harness layer-change-function))
    (dolist (layer active-layers)
      (e-harness-activate-layer harness layer))
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

(defun e-harness-active-capabilities (harness)
  "Return HARNESS capabilities derived from its active layers."
  (let (capabilities)
    (dolist (layer (e-harness-active-layers harness))
      (setq capabilities
            (append capabilities
                    (copy-sequence (or (e-layer-capabilities layer) nil)))))
    capabilities))

(defun e-harness-tools (harness &optional session-id turn-id)
  "Return a fresh tool registry view over HARNESS active layers."
  (let ((registry (e-tools-registry-create)))
    (e-harness--register-resource-operation-tools
     registry
     (e-harness-resources harness session-id turn-id))
    (dolist (capability (e-harness-active-capabilities harness))
      (e-capabilities-register-tools
       capability
       registry
       :harness harness
       :session-id session-id
       :turn-id turn-id))
    registry))

(defun e-harness-hooks (harness)
  "Return a fresh hook registry view over HARNESS active layers."
  (let ((registry (e-hooks-registry-create)))
    (dolist (capability (e-harness-active-capabilities harness))
      (e-capabilities-register-hooks capability registry))
    registry))

(defun e-harness-store (harness &optional session-id turn-id)
  "Return a fresh e:// store view over HARNESS active layers.
SESSION-ID and TURN-ID are passed to context-aware resource providers."
  (let ((store (e-store-create)))
    (dolist (capability (e-harness-active-capabilities harness))
      (e-capabilities-register-resources
       capability
       store
       :harness harness
       :session-id session-id
       :turn-id turn-id))
    store))

(defun e-harness-resources (harness &optional session-id turn-id)
  "Return a fresh resource registry view over HARNESS active layers."
  (let ((registry (e-resources-registry-create))
        (store (e-harness-store harness session-id turn-id)))
    (dolist (capability (e-harness-active-capabilities harness))
      (e-capabilities-register-resource-methods
       capability
       registry
       :harness harness
       :session-id session-id
       :turn-id turn-id))
    (when (e-store-list store)
      (e-resources-register registry (e-store-resource-method store)))
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
  "Notify HARNESS that its active layer set changed."
  (when-let ((function (e-harness-layer-change-function harness)))
    (funcall function harness))
  harness)

(defun e-harness-activate-capability (harness capability)
  "Activate CAPABILITY in HARNESS as an anonymous capability layer."
  (e-harness-activate-layer
   harness
   (e-layer-create
    :id (e-capability-id capability)
    :name (e-capability-name capability)
    :capabilities (list capability)))
  capability)

(defun e-harness-activate-layer (harness layer)
  "Activate LAYER in HARNESS."
  (setf (e-harness-active-layers harness)
        (append (e-harness-active-layers harness) (list layer)))
  (when (e-layer-shells layer)
    (e-shell-register-layer-shells
     harness (e-layer-id layer) (e-layer-shells layer)
     :project-root (e-harness-default-project-root harness)
     :metadata (list :layer-id (e-layer-id layer))))
  (e-harness--notify-layers-changed harness)
  layer)

(defun e-harness-active-layer (harness layer-id)
  "Return HARNESS active layer matching LAYER-ID, or nil."
  (cl-find layer-id
           (e-harness-active-layers harness)
           :key #'e-layer-id
           :test #'eq))

(defun e-harness-layer-active-p (harness layer-id)
  "Return non-nil when HARNESS has an active layer matching LAYER-ID."
  (and (e-harness-active-layer harness layer-id) t))

(defun e-harness-deactivate-layer (harness layer-id)
  "Deactivate HARNESS layer matching LAYER-ID and return it, or nil."
  (let ((removed nil)
        (layers nil))
    (dolist (layer (e-harness-active-layers harness))
      (if (and (not removed)
               (eq (e-layer-id layer) layer-id))
          (setq removed layer)
        (push layer layers)))
    (setf (e-harness-active-layers harness) (nreverse layers))
    (when removed
      (e-shell-unregister-layer-shells harness layer-id)
      (e-harness--notify-layers-changed harness))
    removed))

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
    turn-failed turn-cancelled backend-empty-output
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

(defun e-harness-turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID."
  (let ((options (e-harness--merge-turn-options
                  (e-harness-default-options harness)
                  (e-harness-session-options harness session-id)))
        (tool-definitions
         (e-tools-definitions (e-harness-tools harness session-id))))
    (if tool-definitions
        (plist-put options :tools tool-definitions)
      options)))

(defun e-harness--turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID."
  (e-harness-turn-options harness session-id))

(defun e-harness--tool-hook-context (harness session-id turn-id tools)
  "Return the narrow hook context for a tool lifecycle in HARNESS."
  (list :harness harness
        :session-id session-id
        :turn-id turn-id
        :tools tools
        :capabilities (e-harness-active-capabilities harness)))

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
        (lambda (tool-call &key on-request-start on-done on-error)
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
     (e-context-build
      (e-harness-context-strategy harness)
      :sessions (e-harness-sessions harness)
      :session-id session-id
      :options (e-harness-turn-options harness session-id)
      :prefix-messages
      (e-capabilities-context-messages
       (e-harness-active-capabilities harness)
       :harness harness
       :session-id session-id
       :turn-id turn-id)))))

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
             turn-id)
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
                 :active-turn allow-active-turn))
          (setq preparation
                (e-compaction-prepare
                 (e-harness-sessions harness)
                 session-id
                 :instructions instructions
                 :keep-recent-tokens keep-recent-tokens
                 :allow-split-turn allow-active-turn))
          (e-harness--emit-turn-event
           harness session-id turn-id 'compaction-prepared
           (list :first-kept-entry-id
                 (plist-get preparation :first-kept-entry-id)
                 :tokens-before (plist-get preparation :tokens-before)
                 :tokens-kept (plist-get preparation :tokens-kept)))
          (e-harness--emit-turn-event
           harness session-id turn-id 'compaction-summary-started
           (list :backend t))
          (e-backend-stream
           (e-harness-backend harness)
           :messages (e-compaction-summary-messages preparation)
           :options (e-harness-turn-options harness session-id)
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
          (list :message message :details details))
         (signal (car err) (cdr err)))))))

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
  "Return the compact user-visible error message for condition ERR."
  (if (and (consp err)
           (eq (car err) 'e-loop-backend-error)
           (stringp (cadr err)))
      (cadr err)
    (error-message-string err)))

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
             cancelled-p append-message on-event)
  "Start a queued async prompt turn for SESSION-ID and TURN-ID in HARNESS."
  (e-harness--profile-call
   'harness.prompt-turn-async-start
   (list :session-id session-id
         :turn-id turn-id)
   (lambda ()
     (let ((context (e-harness-context harness session-id turn-id)))
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
           (e-harness--append-user-message
            harness session-id turn-id prompt metadata)
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
            (finish-error
             (err)
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (let ((message (e-harness--backend-error-message err))
                     (details (e-harness--backend-error-details err)))
                 (plist-put entry :status 'error)
                 (plist-put entry :condition err)
                 (plist-put entry :error message)
                 (plist-put entry :error-details details)
                 (e-harness--emit-turn-failed
                  harness session-id turn-id message details))))
            (finish-done
             (result)
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (plist-put entry :result result)
               (plist-put entry :status 'done)))
            (start-turn
             ()
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (plist-put entry :timer nil)
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
                       (plist-put entry :open-tool-call nil)))
                    (e-harness--emit-turn-event
                     harness session-id turn-id type payload)))
                :append-message
                (lambda (message)
                  (when (and (active-entry-p)
                             (not (plist-get entry :cancelled)))
                    (e-harness--append-message
                     harness session-id turn-id message)))))))
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
  (e-harness--emit
   harness
   (e-events-make :type 'session-reset
                  :session-id session-id
                  :turn-id nil
                  :payload nil)))

(defun e-harness-state (harness session-id)
  "Return settled state for SESSION-ID in HARNESS."
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (list :session-id session-id
          :active-turn (when (or (not (listp entry))
                                 (eq (plist-get entry :status) 'running))
                         (e-harness--active-turn-id entry))
          :message-count (length (e-harness-messages harness session-id)))))

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

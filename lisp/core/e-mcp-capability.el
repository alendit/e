;;; e-mcp-capability.el --- MCP capability composition -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns MCP tool/resource/context composition and progressive disclosure.  It
;; consumes the transport-independent client contract and never reaches into
;; transport state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-capability-config)
(require 'e-context)
(require 'e-request)
(require 'e-store)
(require 'e-tools)
(require 'e-work)
(require 'e-mcp-protocol)
(require 'e-mcp-client)

(declare-function e-harness-sessions "e-harness" (harness))
(declare-function e-harness-effective-capability-config "e-harness")
(declare-function e-harness-executing-session-state
                  "e-harness-state" (harness session-id))
(declare-function e-session-async-enabled-p "e-session-async" (store))
(declare-function e-session-local-state "e-session" (store session-id))
(declare-function e-session-set-capability-state "e-session"
                  (store session-id capability-id state))
(declare-function e-session-metadata-capability-state-value
                  "e-session-metadata" (metadata capability-id))

(defun e-mcp-capability--generated-tool-name (tool)
  "Return the generated e tool name for MCP TOOL.
Uses `__' separators (not dots) so the name satisfies provider tool-name
constraints such as Anthropic/Bedrock's `[a-zA-Z0-9_-]+' pattern.  The name
is only a dispatch key; MCP routing uses the tool metadata, never a parse of
this string."
  (format "mcp__%s__%s"
          (e-mcp-tool-server-id tool)
          (e-mcp-tool-name tool)))

(defun e-mcp-capability--tool-metadata (tool)
  "Return metadata for generated e TOOL."
  (list :kind 'mcp-tool
        :server-id (e-mcp-tool-server-id tool)
        :tool-name (e-mcp-tool-name tool)))

(defun e-mcp-capability--content-block-text (block)
  "Return model-visible text for MCP content BLOCK."
  (pcase (plist-get block :type)
    ("text"
     (or (plist-get block :text) ""))
    (type
     (format "[Unsupported MCP content block: %s]"
             (or type "unknown")))))

(defun e-mcp-capability--content-text (content)
  "Return model-visible text for MCP CONTENT blocks."
  (string-join
   (delq nil
         (mapcar #'e-mcp-capability--content-block-text (append content nil)))
   "\n"))

(defun e-mcp-capability--result-content (mcp-result)
  "Return e tool content mapped from MCP-RESULT."
  (let* ((has-structured (plist-member mcp-result :structuredContent))
         (structured (plist-get mcp-result :structuredContent))
         (text (e-mcp-capability--content-text (plist-get mcp-result :content))))
    (cond
     ((and has-structured (not (string-empty-p text)))
      (list :content text :structuredContent structured))
     (has-structured
      structured)
     (t text))))

(defun e-mcp-capability--result-error-p (mcp-result)
  "Return non-nil when MCP-RESULT is an MCP execution error."
  (e-mcp-protocol-truthy-p (plist-get mcp-result :isError)))

(defun e-mcp-capability--tool-result (call tool mcp-result)
  "Return an e tool result for CALL, TOOL, and MCP-RESULT."
  (let ((content (e-mcp-capability--result-content mcp-result))
        (metadata (e-mcp-capability--tool-metadata tool)))
    (if (e-mcp-capability--result-error-p mcp-result)
        (e-tools-result-create
         call
         'error
         content
         (append metadata (list :error 'mcp-execution-error)))
      (e-tools-result-create call 'ok content metadata))))

(defun e-mcp-capability--tool-handler (servers tool)
  "Return a generated handler for MCP TOOL through SERVERS."
  (lambda (arguments)
    (let* ((call (plist-get (e-tools-current-context) :tool-call))
           (mcp-result (e-mcp-call-tool
                        servers
                        (e-mcp-tool-server-id tool)
                        (e-mcp-tool-name tool)
                        arguments)))
      (e-mcp-capability--tool-result call tool mcp-result))))

(defun e-mcp-capability--tool-blocking-class (servers tool)
  "Return the blocking class for generated MCP TOOL through SERVERS."
  (let ((server (cl-find (e-mcp-tool-server-id tool) servers
                         :key #'e-mcp-server-id :test #'equal)))
    (if (and server (e-mcp-protocol-http-server-p server))
        'network
      'process)))

(defun e-mcp-capability--work-progress (handle type payload)
  "Publish MCP event TYPE with PAYLOAD through HANDLE progress."
  (e-work-progress
   handle
   (if (eq type 'tool-progress)
       payload
     (list :event type :payload payload))))

(defun e-mcp-capability--work-adopt-request (handle request kind)
  "Attach child REQUEST metadata and cancellation to HANDLE for KIND."
  (when (e-tools-request-p request)
    (let ((metadata (e-tools-request-metadata request)))
      (setf (e-work-handle-metadata handle)
            (append (e-work-handle-metadata handle)
                    (list :mcp-child-kind kind
                          :mcp-child-request request
                          :mcp-child-request-metadata metadata)))
      (setf (e-work-handle-cancel-function handle)
            (lambda (_handle)
              (e-tools-cancel-request request)
              t))))
  request)

(defun e-mcp-capability--tool-work (servers tool)
  "Return a Work spec for generated MCP TOOL through SERVERS."
  (let ((blocking-class (e-mcp-capability--tool-blocking-class servers tool)))
    (e-work-spec-create
     :id (format "mcp-tool:%s:%s"
                 (e-mcp-tool-server-id tool)
                 (e-mcp-tool-name tool))
     :description (format "MCP tool %s/%s"
                          (e-mcp-tool-server-id tool)
                          (e-mcp-tool-name tool))
     :parameters (e-mcp-tool-input-schema tool)
     :execution 'cooperative
     :interactive-policy 'async
     :owner 'e-mcp
     :metadata (append (e-mcp-capability--tool-metadata tool)
                       (list :blocking-class blocking-class))
     :runner
     (lambda (handle arguments context)
       (let ((call (plist-get context :tool-call)))
         (e-mcp-capability--work-adopt-request
          handle
          (e-mcp-call-tool-start
           servers
           (e-mcp-tool-server-id tool)
           (e-mcp-tool-name tool)
           arguments
           :on-done (lambda (mcp-result)
                      (e-work-finish
                       handle
                       (e-mcp-capability--tool-result call tool mcp-result)))
           :on-error (lambda (err)
                       (e-work-fail handle err))
           :on-event (lambda (type payload)
                       (e-mcp-capability--work-progress handle type payload)))
          'tool-call)
         :deferred)))))

(defun e-mcp-capability--register-tool (registry servers tool)
  "Register generated e TOOL in REGISTRY for SERVERS."
  (e-tools-register
   registry
   :name (e-mcp-capability--generated-tool-name tool)
   :description (format "[MCP %s] %s"
                        (e-mcp-tool-server-id tool)
                        (e-mcp-tool-description tool))
   :parameters (e-mcp-tool-input-schema tool)
   :metadata (e-mcp-capability--tool-metadata tool)
   :work (e-mcp-capability--tool-work servers tool)
   :blocking-class (e-mcp-capability--tool-blocking-class servers tool)))

;;; Progressive disclosure (Tier 0/1/2)
;;
;; Eager mode (the default) registers every MCP tool's full schema into the
;; per-turn tool set, paying its context cost on every turn.  Progressive mode
;; splits "a tool family exists" from "the full contract for calling it":
;;
;;   Tier 0  one tiny capability card per server, injected into context.
;;   Tier 1  full schemas as on-demand e:// resources, read not injected.
;;   Tier 2  an mcp_activate meta-tool that returns schemas and promotes the
;;           requested tools into the session's active tool set.
;;
;; The three tiers are coherent only together: cards without lazy tools double
;; the cost, lazy tools without cards leave the model unable to discover the
;; family.  They are therefore gated by one per-capability `:progressive' flag
;; rather than three independent switches.  With the flag off, this module
;; behaves exactly as it did before progressive disclosure existed.

(defconst e-mcp-capability--progressive-config-options
  (list (e-capability-config-option-create
         :key :progressive
         :type 'boolean
         :default nil
         :documentation
         "When non-nil, expose MCP tools through progressive disclosure: a tiny per-server capability card in context (Tier 0), full schemas as on-demand e:// resources (Tier 1), and an mcp_activate meta-tool that promotes tools into the active set (Tier 2).  When nil, every tool schema is injected eagerly."))
  "Config option specs every MCP-wrapping capability owns.")

(cl-defun e-mcp-capability--config
    (capability-id config-options &key harness session-id directory overrides)
  "Resolve CAPABILITY-ID config against CONFIG-OPTIONS.
HARNESS, when present, contributes session-scoped runtime config."
  (if harness
      (e-harness-effective-capability-config
       harness capability-id config-options
       :session-id session-id :directory directory :overrides overrides)
    (e-capability-config-resolve
     capability-id config-options :directory directory :overrides overrides)))

(cl-defun e-mcp-capability--progressive-p
    (capability-id config-options &key harness session-id)
  "Return non-nil when CAPABILITY-ID is configured for progressive disclosure."
  (plist-get (e-mcp-capability--config capability-id config-options
                            :harness harness :session-id session-id)
             :progressive))

;;; Tier 2 — active tool set (persisted as capability session state)

(defun e-mcp-capability--active-tools-from-state (tools)
  "Return persisted active TOOLS in runtime shape."
  (cond
   ((eq tools t) t)
   ((vectorp tools) (append tools nil))
   ((listp tools) tools)
   (t nil)))

(defun e-mcp-capability--active-state-entry-to-pair (entry)
  "Return active-set pair for persisted ENTRY."
  (when-let ((server-id (plist-get entry :server-id)))
    (cons server-id
          (e-mcp-capability--active-tools-from-state
           (plist-get entry :tools)))))

(defun e-mcp-capability--active-state-to-set (state)
  "Return active-set alist represented by durable STATE."
  (let ((active (plist-get state :active)))
    (cond
     ((vectorp active)
      (e-mcp-capability--active-state-to-set (list :active (append active nil))))
     ((and (listp active)
           (cl-every (lambda (entry)
                       (and (consp entry)
                            (keywordp (car entry))
                            (plist-member entry :server-id)))
                     active))
      (delq nil (mapcar #'e-mcp-capability--active-state-entry-to-pair active)))
     ((and (listp active)
           (cl-evenp (length active))
           (cl-loop for (key _value) on active by #'cddr
                    always (keywordp key)))
      (let (result)
        (while active
          (let ((server-id (substring (symbol-name (pop active)) 1))
                (tools (pop active)))
            (push (cons server-id (e-mcp-capability--active-tools-from-state tools))
                  result)))
        (nreverse result)))
     ((listp active)
      active))))

(defun e-mcp-capability--active-set-to-state (active)
  "Return durable capability state for active-set ACTIVE."
  (list
   :active
   (vconcat
    (mapcar
     (lambda (entry)
       (let ((tools (cdr entry)))
         (list :server-id (car entry)
               :tools (if (and (listp tools) (not (eq tools t)))
                          (vconcat tools)
                        tools))))
     active))))

(defun e-mcp-capability--active-set (harness session-id)
  "Return the activated-MCP-tools alist for HARNESS SESSION-ID.
Each entry is (SERVER-ID . TOOLS) where TOOLS is t (all tools) or a list of
tool-name strings."
  (when (and harness session-id)
    (let* ((store (e-harness-sessions harness))
           (session
            (or (e-harness-executing-session-state harness session-id)
                (unless (e-session-async-enabled-p store)
                  (ignore-errors
                    (e-session-local-state store session-id))))))
      (when session
      (or (e-mcp-capability--active-state-to-set
           (e-session-metadata-capability-state-value
            (plist-get session :metadata) 'mcp))
          (plist-get (plist-get session :metadata) :mcp-active))))))

(defun e-mcp-capability--tool-activated-p (active server-id tool-name)
  "Return non-nil when TOOL-NAME of SERVER-ID is activated in ACTIVE."
  (let ((tools (cdr (assoc server-id active))))
    (or (eq tools t)
        (and (listp tools) (member tool-name tools) t))))

(defun e-mcp-capability--merge-active-tools (existing tool-names)
  "Merge TOOL-NAMES into EXISTING activation for one server.
Nil or empty TOOL-NAMES means \"all tools\"."
  (cond
   ((eq existing t) t)
   ((null tool-names) t)
   (t (cl-remove-duplicates
       (append (and (listp existing) existing) tool-names)
       :test #'equal))))

(defun e-mcp-capability--activate (harness session-id server-id tool-names)
  "Promote TOOL-NAMES of SERVER-ID into HARNESS SESSION-ID active set.
TOOL-NAMES nil or empty activates the whole server.  Returns the merged value."
  (let* ((store (e-harness-sessions harness))
         (active (copy-alist (e-mcp-capability--active-set harness session-id)))
         (existing (cdr (assoc server-id active)))
         (merged (e-mcp-capability--merge-active-tools existing (append tool-names nil))))
    (setf (alist-get server-id active nil nil #'equal) merged)
    (e-session-set-capability-state
     store session-id 'mcp (e-mcp-capability--active-set-to-state active))
    merged))

;;; Tier 1 — schema text and on-demand resources

(defun e-mcp-capability--tool-schema-text (tool)
  "Return the full on-demand schema document for TOOL.
The callable tool name is the generated name the model invokes later."
  (string-join
   (list (format "# %s" (e-mcp-capability--generated-tool-name tool))
         (e-mcp-tool-description tool)
         ""
         "Input schema (JSON):"
         (json-encode (e-mcp-tool-input-schema tool)))
   "\n"))

(defun e-mcp-capability--family-index-text (server-id catalog)
  "Return the Tier-1 family index document for SERVER-ID CATALOG."
  (string-join
   (cons
    (format "# MCP %s — %d tools" server-id (length catalog))
    (mapcar
     (lambda (tool)
       (format "- %s: %s"
               (e-mcp-tool-name tool)
               (e-mcp-tool-description tool)))
     catalog))
   "\n"))

(defun e-mcp-capability--register-resource-catalogs (store capability pairs)
  "Register Tier-1 MCP schema resources from catalog PAIRS in STORE."
  (let ((capability-id (e-capability-id capability)))
    (dolist (pair pairs)
      (let* ((server (car pair))
             (server-id (e-mcp-server-id server))
             (catalog (cdr pair)))
        (e-store-register
         store capability-id
         (format "mcp/%s/tools" server-id)
         :description (format "MCP %s tool index." server-id)
         :content (e-mcp-capability--family-index-text server-id catalog)
         :metadata (list :kind 'mcp-tool-index :server-id server-id))
        (dolist (tool catalog)
          (e-store-register
           store capability-id
           (format "mcp/%s/tools/%s" server-id (e-mcp-tool-name tool))
           :description (format "MCP %s/%s full schema."
                                server-id (e-mcp-tool-name tool))
           :content (e-mcp-capability--tool-schema-text tool)
           :metadata (e-mcp-capability--tool-metadata tool)))))))

(defun e-mcp-capability--register-resources (store capability servers)
  "Register Tier-1 MCP schema resources for SERVERS in STORE under CAPABILITY."
  (e-mcp-capability--register-resource-catalogs
   store capability (e-mcp-client-catalogs servers)))

(defun e-mcp-capability--resource-provider (servers capability-id all-options)
  "Return a resource provider registering Tier-1 resources for SERVERS.
Resources are registered only when CAPABILITY-ID resolves to progressive mode
against ALL-OPTIONS."
  (cl-function
   (lambda (store capability &key harness session-id &allow-other-keys)
     (when (e-mcp-capability--progressive-p capability-id all-options
                                 :harness harness :session-id session-id)
       (if (or harness session-id)
           (e-mcp-capability--register-resource-catalogs
            store capability (e-mcp-client-catalogs-cached servers t))
         (e-mcp-capability--register-resources store capability servers))))))

;;; Tier 0 — capability cards

(defun e-mcp-capability--server-card-text (capability-id server catalog)
  "Return the Tier-0 capability card for SERVER CATALOG under CAPABILITY-ID."
  (let ((server-id (e-mcp-server-id server)))
    (string-join
     (list
      (format "%s (MCP, %d tools)" server-id (length catalog))
      (format "Load before use: read e://%s/mcp/%s/tools  (or call mcp_activate server=\"%s\")"
              capability-id server-id server-id)
      (format "Tools: %s"
              (string-join (mapcar #'e-mcp-tool-name catalog) ", ")))
     "\n")))

(defun e-mcp-capability--cards-message (capability-id servers)
  "Return a single Tier-0 context message describing SERVERS for CAPABILITY-ID."
  (let* ((pairs (e-mcp-client-catalogs-cached servers t))
         (cached-ids (mapcar (lambda (pair)
                               (e-mcp-server-id (car pair)))
                             pairs))
         (loading (cl-remove-if
                   (lambda (server)
                     (member (e-mcp-server-id server) cached-ids))
                   servers))
         (cards (append
                 (mapcar
                  (lambda (pair)
                    (e-mcp-capability--server-card-text
                     capability-id (car pair) (cdr pair)))
                  pairs)
                 (mapcar
                  (lambda (server)
                    (format "%s (MCP, loading tool catalog)"
                            (e-mcp-server-id server)))
                  loading))))
    (when (or cards loading)
      (list
       (list :role 'system
             :content
             (string-join
              (cons
               "MCP tool families available this session (schemas load on demand):"
               cards)
              "\n\n"))))))

(defun e-mcp-capability--context-provider (servers capability-id all-options)
  "Return a Tier-0 card context provider for SERVERS.
Cards are emitted only when CAPABILITY-ID resolves to progressive mode."
  (e-context-provider-create
   :name (intern (format "mcp-cards-%s" capability-id))
   :priority 210
   :cache-placement 'stable-context
   :build
   (cl-function
    (lambda (&key harness session-id _turn-id _context-purpose)
      (when (e-mcp-capability--progressive-p capability-id all-options
                                  :harness harness :session-id session-id)
        (e-mcp-capability--cards-message capability-id servers))))))

;;; Tier 2 — mcp_activate meta-tool

(defconst e-mcp-capability--activate-tool-parameters
  '(:type "object"
    :properties (:server (:type "string"
                          :description "MCP server id to activate.")
                 :tools (:type "array"
                         :items (:type "string")
                         :description "Tool names to activate; omit for all.")
                 :invoke (:type "object"
                          :description "Optional single tool to call immediately."
                          :properties (:tool (:type "string")
                                       :arguments (:type "object"))))
    :required ["server"])
  "Schema for the always-present mcp_activate meta-tool.")

(defun e-mcp-capability--catalog-for-server (server-id)
  "Return (SERVER . CATALOG) for SERVER-ID from remembered servers, or nil."
  (when-let ((server (e-mcp-client-known-server server-id)))
    (cons server (e-mcp-list-tools (list server)))))

(defun e-mcp-capability--catalog-for-server-cached (server-id)
  "Return cached (SERVER . CATALOG) for SERVER-ID, or nil."
  (when-let ((server (e-mcp-client-known-server server-id)))
    (when (e-mcp-client-catalog-cached-p (list server))
      (cons server (e-mcp-client-catalog-cache-entry (list server))))))

(defun e-mcp-capability--select-tools (catalog tool-names)
  "Return CATALOG entries whose names are in TOOL-NAMES, or all when empty."
  (let ((names (append tool-names nil)))
    (if (null names)
        catalog
      (cl-remove-if-not
       (lambda (tool) (member (e-mcp-tool-name tool) names))
       catalog))))

(defun e-mcp-capability--activate-result (arguments server catalog invoke-result context)
  "Return the model-facing mcp_activate result for ARGUMENTS and CATALOG."
  (let* ((call (plist-get context :tool-call))
         (harness (plist-get context :harness))
         (session-id (plist-get context :session-id))
         (server-id (e-mcp-server-id server))
         (requested (plist-get arguments :tools))
         (selected (e-mcp-capability--select-tools catalog requested))
         (schema-text (string-join
                       (mapcar #'e-mcp-capability--tool-schema-text selected)
                       "\n\n"))
         (sections (list schema-text)))
    (when (and harness session-id)
      (e-mcp-capability--activate harness session-id server-id
                       (mapcar #'e-mcp-tool-name selected))
      (push "Activated for this session; the tools above are now callable."
            sections))
    (when invoke-result
      (push (format "Invoke %s result:\n%s"
                    (plist-get (plist-get arguments :invoke) :tool)
                    (e-tools-result-content-text
                     (e-mcp-capability--result-content invoke-result)))
            sections))
    (let ((content (string-join (nreverse sections) "\n\n")))
      (if call
          (e-tools-result-create call 'ok content (list :kind 'mcp-activate))
        content))))

(defun e-mcp-capability--activate-handler (arguments)
  "Handle an mcp_activate call described by ARGUMENTS."
  (let* ((server-id (plist-get arguments :server))
         (invoke (plist-get arguments :invoke))
         (server+catalog (and server-id (e-mcp-capability--catalog-for-server server-id))))
    (unless server+catalog
      (signal 'e-mcp-protocol-error
              (list (format "Unknown MCP server: %s" server-id))))
    (let* ((server (car server+catalog))
           (catalog (cdr server+catalog))
           (invoke-result
            (when invoke
              (e-mcp-call-tool (list server) server-id
                               (plist-get invoke :tool)
                               (plist-get invoke :arguments)))))
      (e-mcp-capability--activate-result
       arguments server catalog invoke-result (e-tools-current-context)))))

(defun e-mcp-capability--activate-work ()
  "Return a Work spec for mcp_activate."
  (e-work-spec-create
   :id "mcp-activate"
   :description "Load and activate MCP tool schemas"
   :parameters e-mcp-capability--activate-tool-parameters
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'e-mcp
   :metadata '(:kind mcp-activate)
   :runner
   (lambda (handle arguments context)
     (let* ((server-id (plist-get arguments :server))
            (invoke (plist-get arguments :invoke))
            (server (and server-id (e-mcp-client-known-server server-id)))
            child-request
            timer)
       (unless server
         (signal 'e-mcp-protocol-error
                 (list (format "Unknown MCP server: %s" server-id))))
       (setf (e-work-handle-metadata handle)
             (append (e-work-handle-metadata handle)
                     (list :server-id server-id)))
       (cl-labels
           ((terminal-p ()
              (e-request-terminal-p (e-work-handle-lifecycle handle)))
            (cleanup (_handle)
              (when (timerp timer)
                (cancel-timer timer)))
            (remember (request kind)
              (setq child-request request)
              (e-mcp-capability--work-adopt-request handle request kind))
            (fail (condition)
              (unless (terminal-p)
                (e-work-fail handle condition)))
            (finish (catalog &optional invoke-result)
              (unless (terminal-p)
                (e-work-finish
                 handle
                 (e-mcp-capability--activate-result
                  arguments server catalog invoke-result context))))
            (start-invoke (catalog)
              (if invoke
                  (remember
                   (e-mcp-call-tool-start
                    (list server) server-id
                    (plist-get invoke :tool)
                    (plist-get invoke :arguments)
                    :on-done (lambda (mcp-result)
                               (finish catalog mcp-result))
                    :on-error #'fail
                    :on-event (lambda (type payload)
                                (e-mcp-capability--work-progress handle type payload)))
                   'activate-invoke)
                (finish catalog))))
         (e-work-add-cleanup handle #'cleanup)
         (setf (e-work-handle-cancel-function handle)
               (lambda (_handle)
                 (when (timerp timer)
                   (cancel-timer timer))
                 (when child-request
                   (e-tools-cancel-request child-request))
                 t))
         (if-let ((server+catalog (e-mcp-capability--catalog-for-server-cached server-id)))
             (setq timer
                   (run-at-time 0 nil
                                (lambda ()
                                  (unless (terminal-p)
                                    (start-invoke (cdr server+catalog))))))
           (remember
            (e-mcp-list-tools-start
             (list server)
             :on-done #'start-invoke
             :on-error #'fail
             :on-event (lambda (type payload)
                         (e-mcp-capability--work-progress handle type payload)))
            'activate-list))
         :deferred)))))

(defun e-mcp-capability--register-meta-tool (registry)
  "Register the always-present mcp_activate meta-tool in REGISTRY."
  (e-tools-register
   registry
   :name "mcp_activate"
   :description
   "Load full schemas for MCP tools and make them callable for the rest of the session. Pass `server' and optionally `tools' (omit for all). The result returns the full schemas; the named tools become callable on the next turn. Optionally pass `invoke' {tool, arguments} to also call one tool immediately."
   :parameters e-mcp-capability--activate-tool-parameters
   :work (e-mcp-capability--activate-work)
   :blocking-class 'process
   :metadata '(:kind mcp-activate)))

(defun e-mcp-capability--tools-provider (servers capability-id all-options)
  "Return a tools provider for SERVERS.
In eager mode it registers every tool.  In progressive mode it registers the
mcp_activate meta-tool plus only the tools the session has activated."
  (cl-function
   (lambda (registry &key harness session-id &allow-other-keys)
     (if (e-mcp-capability--progressive-p capability-id all-options
                               :harness harness :session-id session-id)
         (progn
           (e-mcp-capability--register-meta-tool registry)
           (let ((active (e-mcp-capability--active-set harness session-id)))
             (dolist (tool (if (or harness session-id)
                               (e-mcp-client-tools-cached servers t)
                             (e-mcp-client-tools servers)))
               (when (e-mcp-capability--tool-activated-p
                      active (e-mcp-tool-server-id tool) (e-mcp-tool-name tool))
                 (e-mcp-capability--register-tool registry servers tool)))))
       (dolist (tool (if (or harness session-id)
                         (e-mcp-client-tools-cached servers t)
                       (e-mcp-client-tools servers)))
         (e-mcp-capability--register-tool registry servers tool))))))

(cl-defun e-capability-with-mcp-create
    (&key id name instructions mcp-servers tools resource-methods resources
          context-providers actions instruction-priority config-options config)
  "Create an ordinary capability that wraps configured MCP servers.
MCP-SERVERS are construction-time `e-mcp-server' values.  Discovered MCP tools
are registered as ordinary e tools under deterministic names."
  (dolist (server mcp-servers)
    (unless (e-mcp-server-p server)
      (signal 'wrong-type-argument (list 'e-mcp-server-p server))))
  (e-mcp-client-remember-servers mcp-servers)
  (let* ((all-options (append e-mcp-capability--progressive-config-options config-options))
         (mcp-tools (when mcp-servers
                      (e-mcp-capability--tools-provider mcp-servers id all-options)))
         (mcp-resources (when mcp-servers
                          (e-mcp-capability--resource-provider mcp-servers id all-options)))
         (mcp-cards (when mcp-servers
                      (e-mcp-capability--context-provider mcp-servers id all-options))))
    (when mcp-servers
      (e-capability-config-register-options id all-options))
    (e-capability-create
     :id id
     :name name
     :instructions instructions
     :tools (append tools (when mcp-tools (list mcp-tools)))
     :resource-methods resource-methods
     :resources (append resources (when mcp-resources (list mcp-resources)))
     :context-providers (append context-providers
                                (when mcp-cards (list mcp-cards)))
     :instruction-priority instruction-priority
     :actions actions
     :config-options all-options
     :config config)))

(provide 'e-mcp-capability)

;;; e-mcp-capability.el ends here

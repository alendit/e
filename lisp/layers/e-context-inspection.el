;;; e-context-inspection.el --- Context inspection tools for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tools for exporting the model-facing context that a harness builds for a
;; session/turn.  The capability is packaged by the e self-management layer and
;; by the dedicated e-dev layer for explicit development-oriented activation.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-harness)
(require 'e-resources)
(require 'e-session)
(require 'e-tools)
(require 'subr-x)

(define-error 'e-context-inspection-invalid
  "Context inspection input is invalid")

(defconst e-context-inspection-default-uri "tmp://default_context.md"
  "Default destination resource URI used by the export-context tool.")

(defconst e-context-inspection-default-failure-limit 10
  "Default number of recent failures returned by failure inspection.")

(defun e-context-inspection--argument-string (arguments key &optional default)
  "Return optional string argument KEY from ARGUMENTS, or DEFAULT."
  (let ((value (plist-get arguments key)))
    (cond
     ((null value) default)
     ((stringp value) value)
     (t (signal 'wrong-type-argument (list 'stringp key))))))

(defun e-context-inspection--argument-boolean (arguments key &optional default)
  "Return optional boolean argument KEY from ARGUMENTS, or DEFAULT."
  (let ((value (plist-get arguments key)))
    (cond
     ((null value) default)
     ((or (eq value t) (eq value :json-false)) value)
     (t (signal 'wrong-type-argument (list 'booleanp key))))))

(defun e-context-inspection--argument-nonnegative-integer
    (arguments key &optional default)
  "Return optional non-negative integer KEY from ARGUMENTS, or DEFAULT."
  (let ((value (plist-get arguments key)))
    (cond
     ((null value) default)
     ((and (integerp value) (>= value 0)) value)
     ((and (numberp value) (>= value 0)) (floor value))
     (t (signal 'wrong-type-argument (list 'natnump key))))))

(defun e-context-inspection--tool-context-value (key)
  "Return KEY from `e-tools-current-context', or nil."
  (plist-get (e-tools-current-context) key))

(defun e-context-inspection--current-harness ()
  "Return the active tool or presentation harness."
  (or (e-context-inspection--tool-context-value :harness)
      (and (boundp 'e-current-harness) e-current-harness)))

(defun e-context-inspection--current-session-id ()
  "Return the active tool or chat session id."
  (or (e-context-inspection--tool-context-value :session-id)
      (and (boundp 'e-chat-session-id) e-chat-session-id)))

(defun e-context-inspection--current-turn-id ()
  "Return the active tool turn id, or nil."
  (e-context-inspection--tool-context-value :turn-id))

(defun e-context-inspection--require-harness (harness)
  "Return HARNESS or signal a user-facing error."
  (unless (e-harness-p harness)
    (signal 'e-context-inspection-invalid
            (list "context inspection requires an active e harness")))
  harness)

(defun e-context-inspection--require-session-id (session-id)
  "Return SESSION-ID or signal a user-facing error."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (signal 'e-context-inspection-invalid
            (list "context inspection requires an active session id")))
  session-id)

(defun e-context-inspection--require-turn-id (turn-id)
  "Return TURN-ID or signal a user-facing error."
  (unless (and (stringp turn-id) (not (string-empty-p turn-id)))
    (signal 'e-context-inspection-invalid
            (list "context inspection requires a failed turn id")))
  turn-id)

(defun e-context-inspection--safe-content (content)
  "Return CONTENT as exportable text."
  (if (stringp content)
      content
    (prin1-to-string content)))

(defun e-context-inspection--session-title (harness session-id)
  "Return display title for SESSION-ID in HARNESS."
  (or (ignore-errors (e-harness-session-title harness session-id))
      session-id))

(defun e-context-inspection--session-project-root (session)
  "Return project root metadata from SESSION when present."
  (or (plist-get (plist-get session :metadata) :project-root)
      (plist-get (plist-get session :metadata) :default-directory)))

(defun e-context-inspection--turn-entry-p (entry turn-id)
  "Return non-nil when ENTRY belongs to TURN-ID."
  (equal (plist-get entry :turn-id) turn-id))

(defun e-context-inspection--turn-failed-event-p (event)
  "Return non-nil when EVENT is a terminal turn-failed event."
  (eq (plist-get event :event-type) 'turn-failed))

(defun e-context-inspection--event-created-at (event)
  "Return comparable created-at value for EVENT."
  (or (plist-get event :created-at)
      (plist-get event :timestamp)
      ""))

(defun e-context-inspection--failure-entry (harness session-id event)
  "Return a compact failure plist for SESSION-ID EVENT."
  (let ((payload (plist-get event :payload)))
    (list :session-id session-id
          :turn-id (plist-get event :turn-id)
          :created-at (e-context-inspection--event-created-at event)
          :event-id (plist-get event :id)
          :error (plist-get payload :error)
          :details (plist-get payload :details)
          :session-title (e-context-inspection--session-title
                          harness session-id))))

(defun e-context-inspection--sort-failures-newest-first (failures)
  "Return FAILURES sorted by created-at descending."
  (sort failures
        (lambda (left right)
          (let ((left-time (or (plist-get left :created-at) ""))
                (right-time (or (plist-get right :created-at) "")))
            (if (string= left-time right-time)
                (string> (or (plist-get left :event-id) "")
                         (or (plist-get right :event-id) ""))
              (string> left-time right-time))))))

(defun e-context-inspection--session-ids (harness)
  "Return known session ids for HARNESS."
  (mapcar (lambda (session)
            (plist-get session :id))
          (e-harness-session-list harness)))

(defun e-context-inspection--turn-events (store session-id turn-id)
  "Return activity events for SESSION-ID TURN-ID from STORE."
  (cl-remove-if-not
   (lambda (event)
     (e-context-inspection--turn-entry-p event turn-id))
   (e-session-activity-events store session-id)))

(defun e-context-inspection--turn-messages (store session-id turn-id)
  "Return messages for SESSION-ID TURN-ID from STORE."
  (cl-remove-if-not
   (lambda (message)
     (e-context-inspection--turn-entry-p message turn-id))
   (e-session-messages store session-id)))

(defun e-context-inspection--terminal-failure-event (events)
  "Return the terminal failure event from EVENTS, or nil."
  (cl-find-if #'e-context-inspection--turn-failed-event-p events))

(defun e-context-inspection--tool-call-message-p (message)
  "Return non-nil when MESSAGE records a tool call."
  (eq (plist-get message :role) 'tool-call))

(defun e-context-inspection--tool-result-for-call (messages tool-call-id)
  "Return tool result from MESSAGES for TOOL-CALL-ID, or nil."
  (cl-find-if
   (lambda (message)
     (let ((content (plist-get message :content)))
       (and (eq (plist-get message :role) 'tool)
            (equal (plist-get content :tool-call-id) tool-call-id))))
   messages))

(defun e-context-inspection--tool-calls (messages events)
  "Return compact tool call details from MESSAGES and EVENTS."
  (let ((tool-call-messages
         (cl-remove-if-not #'e-context-inspection--tool-call-message-p
                           messages)))
    (if tool-call-messages
        (mapcar
         (lambda (message)
           (let* ((content (plist-get message :content))
                  (tool-call-id (plist-get content :id))
                  (result (and tool-call-id
                               (e-context-inspection--tool-result-for-call
                                messages tool-call-id))))
             (list :message message
                   :tool-call content
                   :result (and result (plist-get result :content)))))
         tool-call-messages)
      (cl-loop for event in events
               when (eq (plist-get event :event-type) 'tool-started)
               collect (list :event event
                             :tool-call (plist-get event :payload))))))

(cl-defun e-context-inspection-recent-failures (&key harness limit)
  "Return recent failed turns from HARNESS, newest first.
The result is read-only session-store evidence suitable for agent inspection."
  (let* ((harness (e-context-inspection--require-harness
                   (or harness (e-context-inspection--current-harness))))
         (limit (or limit e-context-inspection-default-failure-limit))
         (store (e-harness-sessions harness))
         failures)
    (dolist (session-id (e-context-inspection--session-ids harness))
      (when session-id
        (dolist (event (e-session-activity-events store session-id))
          (when (e-context-inspection--turn-failed-event-p event)
            (push (e-context-inspection--failure-entry
                   harness session-id event)
                  failures)))))
    (let ((sorted (e-context-inspection--sort-failures-newest-first
                   failures)))
      (if limit
          (cl-subseq sorted 0 (min limit (length sorted)))
        sorted))))

(cl-defun e-context-inspection-raw-provider-preview
    (&key harness session-id turn-id)
  "Return a bounded raw provider preview when available.
No current adapter exposes a stable raw diagnostic attachment to this layer, so
v1 reports an explicit unavailable shape."
  (ignore harness session-id turn-id)
  (list :available nil
        :response-kind nil
        :preview nil
        :source "unavailable"))

(cl-defun e-context-inspection-failure-detail
    (&key harness session-id turn-id)
  "Return a failed turn timeline for SESSION-ID TURN-ID in HARNESS."
  (let* ((harness (e-context-inspection--require-harness
                   (or harness (e-context-inspection--current-harness))))
         (session-id (e-context-inspection--require-session-id
                      (or session-id
                          (e-context-inspection--current-session-id))))
         (turn-id (e-context-inspection--require-turn-id
                   (or turn-id
                       (e-context-inspection--current-turn-id))))
         (store (e-harness-sessions harness))
         (session (e-session-get store session-id))
         (events (and session
                      (e-context-inspection--turn-events
                       store session-id turn-id)))
         (terminal (and events
                        (e-context-inspection--terminal-failure-event
                         events))))
    (unless session
      (signal 'e-context-inspection-invalid
              (list (format "Unknown session `%s`" session-id))))
    (unless terminal
      (signal 'e-context-inspection-invalid
              (list (format "Turn `%s` in session `%s` has no terminal failure"
                            turn-id session-id))))
    (let* ((messages (e-context-inspection--turn-messages
                      store session-id turn-id))
           (raw-preview (e-context-inspection-raw-provider-preview
                         :harness harness
                         :session-id session-id
                         :turn-id turn-id)))
      (list :session (list :id session-id
                           :title (e-context-inspection--session-title
                                   harness session-id)
                           :project-root
                           (e-context-inspection--session-project-root
                            session)
                           :metadata (plist-get session :metadata))
            :turn (list :id turn-id
                        :created-at
                        (or (plist-get (car messages) :created-at)
                            (e-context-inspection--event-created-at
                             (car events))))
            :events events
            :messages messages
            :tool-calls (e-context-inspection--tool-calls messages events)
            :terminal-error (plist-get terminal :payload)
            :diagnostics (list :raw-provider-preview raw-preview)))))

(defun e-context-inspection--turn-options-without-tools (harness session-id)
  "Return HARNESS SESSION-ID turn options without tool definitions."
  (let ((options (copy-sequence (e-harness-turn-options harness session-id))))
    (cl-remf options :tools)
    options))

(defun e-context-inspection--pre-prompt-context (harness session-id turn-id)
  "Return context messages/options before transcript prompt messages."
  (let* ((messages (e-capabilities-context-messages
                    (e-harness-active-capabilities harness)
                    :harness harness
                    :session-id session-id
                    :turn-id turn-id))
         (options (e-context-inspection--turn-options-without-tools
                   harness session-id)))
    (list :strategy 'pre-prompt
          :messages messages
          :options options)))

(defun e-context-inspection--full-context (harness session-id turn-id)
  "Return full HARNESS context for SESSION-ID and TURN-ID."
  (e-harness-context harness session-id turn-id))

(cl-defun e-context-inspection--format-context
    (context &key harness session-id turn-id source-buffer include-metadata mode)
  "Return CONTEXT formatted as Markdown."
  (let ((messages (plist-get context :messages))
        (options (plist-get context :options)))
    (with-temp-buffer
      (insert (if (eq mode 'pre-prompt)
                  "# Default e LLM context before first user prompt\n\n"
                "# e LLM context export\n\n"))
      (when source-buffer
        (insert (format "Captured from buffer: `%s`\n\n" source-buffer)))
      (insert (format "Session: `%s`\n\n" session-id))
      (when turn-id
        (insert (format "Turn: `%s`\n\n" turn-id)))
      (insert (format "Default directory: `%s`\n\n" default-directory))
      (insert (format "Layer count: %d\n\n"
                      (length (e-harness-active-layers harness))))
      (insert (format "Backend-neutral context fragment count: %d\n\n"
                      (length messages)))
      (insert "System fragments may be mapped or collapsed by backend adapters before provider submission.\n\n")
      (when include-metadata
        (insert "## Export metadata\n\n")
        (insert (format "- Mode: `%s`\n" mode))
        (insert (format "- Layers: `%S`\n"
                        (mapcar #'e-layer-id
                                (e-harness-active-layers harness))))
        (insert (format "- Option keys: `%S`\n\n"
                        (cl-loop for (key _value) on options by #'cddr
                                 collect key))))
      (cl-loop for message in messages
               for index from 1
               do (insert (format "## Context fragment %d (%s)\n\n"
                                  index
                                  (plist-get message :role)))
                  (insert (e-context-inspection--safe-content
                           (plist-get message :content)))
                  (insert "\n\n"))
      (buffer-string))))

(cl-defun e-context-inspection-export-context
    (&key harness session-id turn-id uri include-transcript include-metadata)
  "Export HARNESS SESSION-ID context to resource URI and return metadata.
When INCLUDE-TRANSCRIPT is nil, only the pre-prompt capability context is
exported.  This is the default context e sends before the first user prompt."
  (let* ((harness (e-context-inspection--require-harness
                   (or harness (e-context-inspection--current-harness))))
         (session-id (e-context-inspection--require-session-id
                      (or session-id
                          (e-context-inspection--current-session-id))))
         (turn-id (or turn-id (e-context-inspection--current-turn-id)))
         (uri (or uri e-context-inspection-default-uri))
         (source-buffer (buffer-name))
         (mode (if include-transcript 'full 'pre-prompt))
         (context (if include-transcript
                   (e-context-inspection--full-context
                    harness session-id turn-id)
                 (e-context-inspection--pre-prompt-context
                  harness session-id turn-id)))
         (content (e-context-inspection--format-context
                   context
                   :harness harness
                   :session-id session-id
                   :turn-id turn-id
                   :source-buffer source-buffer
                   :include-metadata include-metadata
                   :mode mode)))
    (e-resources-write
     (e-harness-resources harness session-id turn-id)
     uri
     content)
    (list :uri uri
          :mode mode
          :message-count (length (plist-get context :messages))
          :bytes (string-bytes content))))

(defun e-context-inspection--export-context-tool (arguments)
  "Handle export-context tool ARGUMENTS."
  (e-context-inspection-export-context
   :uri (e-context-inspection--argument-string
         arguments
         :uri
         e-context-inspection-default-uri)
   :session-id (e-context-inspection--argument-string arguments :session_id)
   :turn-id (e-context-inspection--argument-string arguments :turn_id)
   :include-transcript (eq (e-context-inspection--argument-boolean
                            arguments
                            :include_transcript
                            :json-false)
                           t)
   :include-metadata (not (eq (e-context-inspection--argument-boolean
                               arguments
                               :include_metadata
                               t)
                              :json-false))))

(defun e-context-inspection--recent-failures-tool (arguments)
  "Handle e_error_recent_failures tool ARGUMENTS."
  (e-context-inspection-recent-failures
   :limit (e-context-inspection--argument-nonnegative-integer
           arguments
           :limit
           e-context-inspection-default-failure-limit)))

(defun e-context-inspection--failure-detail-tool (arguments)
  "Handle e_error_failure_detail tool ARGUMENTS."
  (e-context-inspection-failure-detail
   :session-id (e-context-inspection--argument-string arguments :session_id)
   :turn-id (e-context-inspection--argument-string arguments :turn_id)))

(defun e-context-inspection--raw-provider-preview-tool (arguments)
  "Handle e_error_raw_provider_preview tool ARGUMENTS."
  (e-context-inspection-raw-provider-preview
   :session-id (e-context-inspection--argument-string arguments :session_id)
   :turn-id (e-context-inspection--argument-string arguments :turn_id)))

(defun e-context-inspection-register-export-context (registry)
  "Register the export-context tool in REGISTRY."
  (e-tools-register
   registry
   :name "export-context"
   :description "Export the current e LLM context to a writable URI-addressed resource. By default this writes the pre-prompt default context to tmp://default_context.md and returns only metadata, avoiding large tool output."
   :parameters '(:type "object"
                 :properties (:uri (:type "string"
                                    :description "Destination resource URI. Defaults to tmp://default_context.md.")
                              :session_id (:type "string"
                                           :description "Optional session id. Defaults to the active tool session.")
                              :turn_id (:type "string"
                                        :description "Optional turn id. Defaults to the active tool turn.")
                              :include_transcript (:type "boolean"
                                                   :description "When true, export full context including transcript messages. Defaults to false.")
                              :include_metadata (:type "boolean"
                                                 :description "When true, include export metadata in the Markdown file. Defaults to true."))
                 :required [])
   :handler #'e-context-inspection--export-context-tool))

(defun e-context-inspection-register-error-tools (registry)
  "Register read-only error inspection tools in REGISTRY."
  (e-tools-register
   registry
   :name "e_error_recent_failures"
   :description "List recent failed e turns from the current harness session store, newest first. Returns compact session id, turn id, timestamp, error, details, and session title evidence."
   :parameters '(:type "object"
                 :properties (:limit (:type "integer"
                                      :minimum 0
                                      :description "Maximum failures to return. Defaults to 10."))
                 :required [])
   :handler #'e-context-inspection--recent-failures-tool)
  (e-tools-register
   registry
   :name "e_error_failure_detail"
   :description "Return one failed e turn timeline with prompt messages, provider lifecycle events, token/tool evidence, terminal error payload, session id, turn id, and project root."
   :parameters '(:type "object"
                 :properties (:session_id (:type "string"
                                           :description "Failed session id.")
                              :turn_id (:type "string"
                                        :description "Failed turn id."))
                 :required ["session_id" "turn_id"])
   :handler #'e-context-inspection--failure-detail-tool)
  (e-tools-register
   registry
   :name "e_error_raw_provider_preview"
   :description "Return sanitized bounded raw provider diagnostics when available. In v1 this reports unavailable unless an adapter exposes matching raw diagnostics."
   :parameters '(:type "object"
                 :properties (:session_id (:type "string"
                                           :description "Optional failed session id.")
                              :turn_id (:type "string"
                                        :description "Optional failed turn id."))
                 :required [])
   :handler #'e-context-inspection--raw-provider-preview-tool))

(defun e-context-inspection-capability-create ()
  "Create the context-inspection capability."
  (e-capability-create
   :id 'context-inspection
   :name "Context Inspection"
   :tools (list #'e-context-inspection-register-export-context
                #'e-context-inspection-register-error-tools)))

(provide 'e-context-inspection)

;;; e-context-inspection.el ends here

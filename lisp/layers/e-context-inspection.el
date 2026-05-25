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
(require 'e-tools)
(require 'subr-x)

(define-error 'e-context-inspection-invalid
  "Context inspection input is invalid")

(defconst e-context-inspection-default-uri "tmp://default_context.md"
  "Default destination resource URI used by the export-context tool.")

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
            (list "export-context requires an active e harness")))
  harness)

(defun e-context-inspection--require-session-id (session-id)
  "Return SESSION-ID or signal a user-facing error."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (signal 'e-context-inspection-invalid
            (list "export-context requires an active session id")))
  session-id)

(defun e-context-inspection--safe-content (content)
  "Return CONTENT as exportable text."
  (if (stringp content)
      content
    (prin1-to-string content)))

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

(defun e-context-inspection-capability-create ()
  "Create the context-inspection capability."
  (e-capability-create
   :id 'context-inspection
   :name "Context Inspection"
   :tools (list #'e-context-inspection-register-export-context)))

(provide 'e-context-inspection)

;;; e-context-inspection.el ends here

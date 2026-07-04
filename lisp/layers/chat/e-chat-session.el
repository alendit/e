;;; e-chat-session.el --- Chat session capability actions for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Semantic chat-session actions hosted by presentation shells.

;;; Code:

(require 'cl-lib)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-context)
(require 'e-harness)
(require 'e-session)
(require 'e-work)
(require 'subr-x)

(cl-defun e-chat-session-submit
    (harness session-id prompt &key delay references metadata)
  "Submit PROMPT to SESSION-ID through HARNESS.
When DELAY is non-nil, pass it to `e-harness-prompt-async'.
REFERENCES are ordered source references from the composer.
METADATA is caller-provided turn metadata."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (e-harness-prompt-async
   harness
   session-id
   prompt
   :delay delay
   :metadata (append (copy-sequence metadata)
                     (and references (list :references references)))))

(cl-defun e-chat-session-queue
    (harness session-id prompt &key references metadata)
  "Queue PROMPT as a follow-up for SESSION-ID through HARNESS.
REFERENCES are ordered source references from the composer.
METADATA is caller-provided turn metadata."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (e-harness-queue-prompt
   harness
   session-id
   prompt
   :references references
   :metadata metadata))

(cl-defun e-chat-session-steer
    (harness session-id prompt &key metadata)
  "Steer SESSION-ID's active turn through HARNESS with PROMPT.
METADATA is caller-provided turn activity metadata."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (e-harness-steer-active-turn
   harness
   session-id
   prompt
   :metadata metadata))

(defun e-chat-session-ensure-project-root (harness session-id project-root)
  "Ensure SESSION-ID has PROJECT-ROOT when no durable root is stored."
  (let* ((project-root (and project-root
                            (file-name-as-directory
                             (expand-file-name project-root))))
         (store (e-harness-sessions harness))
         (session (and project-root (e-session-get store session-id)))
         (metadata (plist-get session :metadata))
         (current-root (plist-get metadata :project-root)))
    (when (and project-root
               (not current-root))
      (e-session-set-session-config
       store session-id (list :project-root project-root)))))

(defun e-chat-session-abort (harness session-id)
  "Abort the active chat turn for SESSION-ID through HARNESS."
  (e-harness-abort harness session-id))

(defun e-chat-session-reset (harness session-id)
  "Reset SESSION-ID through HARNESS."
  (e-harness-reset harness session-id))

(cl-defun e-chat-session-compact-start
    (harness session-id &key instructions keep-recent-tokens
             allow-active-turn turn-id on-done on-error)
  "Start compacting SESSION-ID through HARNESS."
  (e-harness-compact-session-start
   harness session-id
   :instructions instructions
   :keep-recent-tokens keep-recent-tokens
   :allow-active-turn allow-active-turn
   :turn-id turn-id
   :on-done on-done
   :on-error on-error))

(defun e-chat-session-rename (harness session-id name)
  "Rename SESSION-ID to NAME through HARNESS session storage."
  (e-session-rename (e-harness-sessions harness) session-id name))

(defun e-chat-session-set-model (harness session-id model)
  "Set SESSION-ID model override to MODEL through HARNESS."
  (e-harness-set-session-model harness session-id model))

(defun e-chat-session-set-effort (harness session-id effort)
  "Set SESSION-ID reasoning EFFORT through HARNESS."
  (e-harness-set-session-reasoning-effort harness session-id effort))

(defun e-chat-session-context (harness session-id)
  "Return context preview data for SESSION-ID through HARNESS."
  (e-harness-context harness session-id nil 'preview))

(defun e-chat-session--attachment-uri (attachment)
  "Return ATTACHMENT's canonical URI."
  (or (plist-get attachment :uri)
      (user-error "Attachment must include :uri")))

(defun e-chat-session--attachment-id (attachment)
  "Return stable id for ATTACHMENT."
  (or (plist-get attachment :id)
      (substring
       (secure-hash 'sha1 (e-chat-session--attachment-uri attachment))
       0
       12)))

(defun e-chat-session--normalize-attachment (attachment &optional canvas)
  "Return normalized ATTACHMENT metadata.
When CANVAS is non-nil, mark the attachment as the session canvas."
  (let* ((attachment (copy-sequence attachment))
         (uri (e-chat-session--attachment-uri attachment)))
    (plist-put attachment :uri uri)
    (plist-put attachment :id (e-chat-session--attachment-id attachment))
    (unless (plist-get attachment :label)
      (plist-put attachment :label uri))
    (if canvas
        (plist-put attachment :canvas t)
      (unless (plist-member attachment :canvas)
        (plist-put attachment :canvas nil)))
    attachment))

(defun e-chat-session-attachments (harness session-id)
  "Return current live context attachments for SESSION-ID in HARNESS."
  (let* ((store (e-harness-sessions harness))
         (references (e-session-context-references
                      store session-id 'chat-session))
         (metadata (plist-get (e-session-get store session-id) :metadata)))
    (copy-sequence
     (or (plist-get references :attachments)
         (plist-get metadata :context-attachments)))))

(defun e-chat-session--same-attachment-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same attachment."
  (or (equal (plist-get left :id) (plist-get right :id))
      (equal (plist-get left :uri) (plist-get right :uri))))

(defun e-chat-session--upsert-attachment (attachments attachment)
  "Return ATTACHMENTS with ATTACHMENT added or replaced by identity."
  (let ((replaced nil)
        result)
    (dolist (existing attachments)
      (if (and (not replaced)
               (e-chat-session--same-attachment-p existing attachment))
          (progn
            (push attachment result)
            (setq replaced t))
        (push existing result)))
    (unless replaced
      (push attachment result))
    (nreverse result)))

(defun e-chat-session--replace-canvas (attachments attachment)
  "Return ATTACHMENTS with the current canvas replaced by ATTACHMENT."
  (cons attachment
        (cl-remove-if (lambda (existing)
                        (or (plist-get existing :canvas)
                            (e-chat-session--same-attachment-p
                             existing attachment)))
                      attachments)))

(defun e-chat-session--set-attachments (harness session-id attachments)
  "Persist ATTACHMENTS as SESSION-ID current-state references."
  (e-session-set-context-references
   (e-harness-sessions harness)
   session-id
   'chat-session
   (list :attachments attachments))
  attachments)

(cl-defun e-chat-session-attach-context
    (harness session-id attachment &key canvas)
  "Attach ATTACHMENT to SESSION-ID live context in HARNESS.
ATTACHMENT is a plist with at least :uri.  Attachments are stored as session
current-state references; their contents are read fresh whenever context is
built.  When CANVAS is non-nil, ATTACHMENT replaces the session's primary canvas
attachment."
  (let* ((attachment (e-chat-session--normalize-attachment attachment canvas))
         (attachments (e-chat-session-attachments harness session-id))
         (next (if canvas
                   (e-chat-session--replace-canvas attachments attachment)
                 (e-chat-session--upsert-attachment attachments attachment))))
    (e-chat-session--set-attachments harness session-id next)
    attachment))

(defun e-chat-session-detach-context (harness session-id attachment-id-or-uri)
  "Detach ATTACHMENT-ID-OR-URI from SESSION-ID live context in HARNESS."
  (let* ((attachments (e-chat-session-attachments harness session-id))
         (next (cl-remove-if
                (lambda (attachment)
                  (or (equal (plist-get attachment :id) attachment-id-or-uri)
                      (equal (plist-get attachment :uri) attachment-id-or-uri)))
                attachments)))
    (e-chat-session--set-attachments harness session-id next)
    next))

(defun e-chat-session--action-harness (context)
  "Return CONTEXT harness for a chat-session action."
  (plist-get context :harness))

(defun e-chat-session--action-session-id (context)
  "Return CONTEXT session id for a chat-session action."
  (plist-get context :session-id))

(defun e-chat-session--action (handler caller &optional parameters work)
  "Return chat-session action descriptor for HANDLER."
  (e-action-create
   :handler handler
   :caller caller
   :parameters parameters
   :requires-session t
   :work work))

(cl-defun e-chat-session--compact-action-request
    (context arguments &key on-done on-error &allow-other-keys)
  "Request chat-session compaction from action CONTEXT and ARGUMENTS."
  (e-chat-session-compact-start
   (e-chat-session--action-harness context)
   (e-chat-session--action-session-id context)
   :instructions (plist-get arguments :instructions)
   :keep-recent-tokens (plist-get arguments :keep_recent_tokens)
   :allow-active-turn (and (plist-get context :turn-id) t)
   :turn-id (plist-get context :turn-id)
   :on-done on-done
   :on-error on-error))

(defun e-chat-session--compact-action-work-runner
    (handle arguments context)
  "Start chat-session compaction ARGUMENTS from action CONTEXT on HANDLE."
  (let ((request
         (e-chat-session--compact-action-request
          context arguments
          :on-done (lambda (record)
                     (e-work-finish handle record))
          :on-error (lambda (err)
                      (e-work-fail handle err)))))
    (setf (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (when (e-backend-request-p request)
              (e-backend-cancel-request request))
            t))
    (setf (e-work-handle-metadata handle)
          (append (e-work-handle-metadata handle)
                  (list :operation 'chat-session-compact
                        :request request)))
    :deferred))

(defun e-chat-session--compact-action-work ()
  "Return Work spec for the chat-session compact action."
  (e-work-spec-create
   :id "chat_session_compact"
   :description "Compact the active chat session."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'actions
   :runner #'e-chat-session--compact-action-work-runner))

(defun e-chat-session--uri-file-name (uri)
  "Return local filename for file URI, or nil."
  (when (string-prefix-p "file://" uri)
    (expand-file-name (substring uri (length "file://")))))

(defun e-chat-session--uri-buffer-name (uri)
  "Return buffer name for buffer URI, or nil."
  (when (string-prefix-p "buffer://" uri)
    (substring uri (length "buffer://"))))

(defun e-chat-session--attachment-live-buffer (attachment)
  "Return a live Emacs buffer for ATTACHMENT when available."
  (or (when-let ((buffer-name (plist-get attachment :buffer-name)))
        (get-buffer buffer-name))
      (when-let ((buffer-name (e-chat-session--uri-buffer-name
                               (plist-get attachment :uri))))
        (get-buffer buffer-name))
      (when-let ((file (e-chat-session--uri-file-name
                        (plist-get attachment :uri))))
        (find-buffer-visiting file))))

(defun e-chat-session--attachment-content (attachment)
  "Return ATTACHMENT current content.
Open Emacs buffers win over disk contents so unsaved canvas edits are included
in the next turn's context."
  (let ((uri (plist-get attachment :uri)))
    (cond
     ((when-let ((buffer (e-chat-session--attachment-live-buffer attachment)))
        (with-current-buffer buffer
          (buffer-substring-no-properties (point-min) (point-max)))))
     ((when-let ((file (e-chat-session--uri-file-name uri)))
        (if (file-readable-p file)
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8))
                (insert-file-contents file))
              (buffer-string))
          (format "[Attachment file is not readable: %s]" file))))
     (t
      (format "[Attachment is not available: %s]" uri)))))

(defun e-chat-session--xml-attribute-escape (value)
  "Return VALUE escaped for a compact XML-like attribute."
  (let ((text (format "%s" (or value ""))))
    (setq text (replace-regexp-in-string "&" "&amp;" text t t))
    (setq text (replace-regexp-in-string "\"" "&quot;" text t t))
    (setq text (replace-regexp-in-string "<" "&lt;" text t t))
    (replace-regexp-in-string ">" "&gt;" text t t)))

(defun e-chat-session--attachment-section (attachment)
  "Return a model-facing current-state section for ATTACHMENT."
  (let* ((canvas (plist-get attachment :canvas))
         (tag (if canvas "canvas" "attachment")))
    (format "<%s id=\"%s\" uri=\"%s\" label=\"%s\">\n%s\n</%s>"
            tag
            (e-chat-session--xml-attribute-escape
             (plist-get attachment :id))
            (e-chat-session--xml-attribute-escape
             (plist-get attachment :uri))
            (e-chat-session--xml-attribute-escape
             (plist-get attachment :label))
            (e-chat-session--attachment-content attachment)
            tag)))

(cl-defun e-chat-session-context-attachments-provider
    (&key harness session-id _turn-id _context-purpose)
  "Return live attachment context messages for SESSION-ID in HARNESS."
  (let ((attachments (and harness
                          session-id
                          (ignore-errors
                            (e-chat-session-attachments harness session-id)))))
    (when attachments
      (let ((has-canvas (cl-some (lambda (attachment)
                                   (plist-get attachment :canvas))
                                 attachments)))
        (list
         (list :role 'system
               :content
               (string-join
                (cons
                 (concat
                  "Live session context attachments follow. These are "
                  "current-state attachments rebuilt for every turn; they "
                  "replace prior attachment state and are not transcript "
                  "history."
                  (when has-canvas
                    (concat
                     "\n\nA <canvas> attachment is the user's working "
                     "document. Prefer to put your answer directly into the "
                     "canvas by editing it. Use the chat response only to "
                     "communicate things about the edit that do not belong "
                     "in the document itself (for example, brief notes, "
                     "questions, or a short summary of what you changed)."
                     "\n\nAlways write to the exact uri given in the <canvas> "
                     "tag's uri attribute (the authoritative write target). "
                     "Do not write to other buffers just because they are "
                     "visible or have a similar name -- editor scratch, input, "
                     "or overlay buffers (for example names like "
                     "*e-org-canvas:...* or *e-org-canvas-input:...*) are NOT "
                     "the canvas and editing them has no effect on the "
                     "document. If a write does not appear in the canvas, "
                     "re-read the <canvas> uri and write to that exact uri "
                     "rather than guessing another buffer.")))
                 (mapcar #'e-chat-session--attachment-section attachments))
                "\n\n")))))))

(defun e-chat-session-capability-create ()
  "Create the chat-session capability."
  (e-capability-create
   :id 'chat-session
   :name "Chat Session"
   :context-providers
   (list (e-context-provider-create
          :name 'chat-session-attachments
          :priority 120
          :cache-placement 'dynamic-context
          :build #'e-chat-session-context-attachments-provider))
   :actions (list :submit
                  (e-chat-session--action
                   #'e-chat-session-submit
                   (lambda (context arguments)
                     (e-chat-session-submit
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)
                      (plist-get arguments :prompt)
                      :references (plist-get arguments :references)
                      :metadata (plist-get arguments :metadata)))
                   '(:type "object"
                     :properties (:prompt (:type "string")
                                  :references (:type "array")
                                  :metadata (:type "object"))
                     :required ["prompt"]))
                  :steer
                  (e-chat-session--action
                   #'e-chat-session-steer
                   (lambda (context arguments)
                     (e-chat-session-steer
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)
                      (plist-get arguments :prompt)
                      :metadata (plist-get arguments :metadata)))
                   '(:type "object"
                     :properties (:prompt (:type "string")
                                  :metadata (:type "object"))
                     :required ["prompt"]))
                  :queue
                  (e-chat-session--action
                   #'e-chat-session-queue
                   (lambda (context arguments)
                     (e-chat-session-queue
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)
                      (plist-get arguments :prompt)
                      :references (plist-get arguments :references)
                      :metadata (plist-get arguments :metadata)))
                   '(:type "object"
                     :properties (:prompt (:type "string")
                                  :references (:type "array")
                                  :metadata (:type "object"))
                     :required ["prompt"]))
                  :abort
                  (e-chat-session--action
                   #'e-chat-session-abort
                   (lambda (context _arguments)
                     (e-chat-session-abort
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context))))
                  :reset
                  (e-chat-session--action
                   #'e-chat-session-reset
                   (lambda (context _arguments)
                     (e-chat-session-reset
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context))))
                  :compact
                  (e-chat-session--action
                   #'e-chat-session-compact-start
                   nil
                   '(:type "object"
                     :properties (:instructions (:type "string")
                                  :keep_recent_tokens (:type "integer"))
                     :required [])
                   (e-chat-session--compact-action-work))
                  :rename
                  (e-chat-session--action
                   #'e-chat-session-rename
                   (lambda (context arguments)
                     (e-chat-session-rename
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)
                      (plist-get arguments :name)))
                   '(:type "object"
                     :properties (:name (:type "string"))
                     :required ["name"]))
                  :set-model
                  (e-chat-session--action
                   #'e-chat-session-set-model
                   (lambda (context arguments)
                     (e-chat-session-set-model
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)
                      (plist-get arguments :model)))
                   '(:type "object"
                     :properties (:model (:type "string"))
                     :required ["model"]))
                  :set-effort
                  (e-chat-session--action
                   #'e-chat-session-set-effort
                   (lambda (context arguments)
                     (e-chat-session-set-effort
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)
                      (plist-get arguments :effort)))
                   '(:type "object"
                     :properties (:effort (:type "string"))
                     :required ["effort"]))
                  :attach-context
                  (e-chat-session--action
                   #'e-chat-session-attach-context
                   (lambda (context arguments)
                     (e-chat-session-attach-context
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)
                      (plist-get arguments :attachment)
                      :canvas (plist-get arguments :canvas)))
                   '(:type "object"
                     :properties (:attachment (:type "object")
                                  :canvas (:type "boolean"))
                     :required ["attachment"]))
                  :detach-context
                  (e-chat-session--action
                   #'e-chat-session-detach-context
                   (lambda (context arguments)
                     (e-chat-session-detach-context
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)
                      (plist-get arguments :attachment)))
                   '(:type "object"
                     :properties (:attachment (:type "string"))
                     :required ["attachment"]))
                  :context
                  (e-chat-session--action
                   #'e-chat-session-context
                   (lambda (context _arguments)
                     (e-chat-session-context
                      (e-chat-session--action-harness context)
                      (e-chat-session--action-session-id context)))))))

(provide 'e-chat-session)

;;; e-chat-session.el ends here

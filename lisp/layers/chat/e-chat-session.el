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
(require 'e-chat-service)
(require 'e-context)
(require 'e-harness)
(require 'e-json)
(require 'e-request)
(require 'e-session)
(require 'e-work)
(require 'subr-x)

(cl-defun e-chat-session-submit
    (harness session-id prompt &key references metadata)
  "Submit PROMPT to SESSION-ID through its board binding.
REFERENCES are ordered source references from the composer.
METADATA is caller-provided turn metadata.  Return admission work."
  (e-chat-service-submit-session
   harness session-id prompt :references references
   :metadata metadata))

(cl-defun e-chat-session-queue
    (harness session-id prompt &key references metadata)
  "Queue PROMPT as a board-routed follow-up for SESSION-ID.
REFERENCES are ordered source references from the composer.
METADATA is caller-provided turn metadata.  Return admission work."
  (e-chat-service-queue-session
   harness session-id prompt :references references :metadata metadata))

(cl-defun e-chat-session-steer
    (harness session-id prompt &key metadata)
  "Steer SESSION-ID's active turn through its board binding with PROMPT.
METADATA is caller-provided turn activity metadata.  Return admission work."
  (e-chat-service-steer-session harness session-id prompt :metadata metadata))

(defun e-chat-session-abort (harness session-id)
  "Abort SESSION-ID's board-attached active chat turn."
  (e-chat-service-abort-session harness session-id))

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

(defun e-chat-session-set-options (harness session-id options)
  "Replace SESSION-ID turn OPTIONS through HARNESS and return persistence work."
  (e-harness-set-session-options harness session-id options))

(defun e-chat-session-context (harness session-id)
  "Return request-scoped work building SESSION-ID context through HARNESS."
  (e-harness-context-preview-start harness session-id))

(defun e-chat-session--attachment-uri (attachment)
  "Return ATTACHMENT's canonical URI."
  (let ((uri (plist-get attachment :uri)))
    (cond
     ((stringp uri) uri)
     ((null uri) (user-error "Attachment must include :uri"))
     (t (user-error "Attachment :uri must be a string")))))

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

(defun e-chat-session--attachment-plist-shape-p (value)
  "Return non-nil when VALUE has the shape of one attachment plist."
  (and (proper-list-p value)
       (let ((tail value)
             (valid t))
         (while (and valid tail)
           (setq valid
                 (and (keywordp (car tail))
                      (consp (cdr tail))))
           (setq tail (cddr tail)))
         valid)))

(defun e-chat-session--attachment-list (attachments)
  "Return canonical durable ATTACHMENTS as a list of attachment plists."
  (when (vectorp attachments)
    (setq attachments (append attachments nil)))
  (unless (and (proper-list-p attachments)
               (cl-every #'e-chat-session--attachment-plist-shape-p
                         attachments))
    (user-error "Attachments must be a sequence of attachment plists"))
  attachments)

(defun e-chat-session-metadata-attachments (metadata)
  "Return canonical chat attachments from session METADATA.
METADATA may come from a live session or a transcript-free catalog entry."
  (let ((references
         (e-session-metadata-context-references metadata 'chat-session)))
    (mapcar #'e-chat-session--normalize-attachment
            (e-chat-session--attachment-list
             (plist-get references :attachments)))))

(defun e-chat-session-attachments (harness session-id)
  "Return request-scoped context attachments for SESSION-ID in HARNESS.

Attachment metadata comes only from the detached state of the executing turn.
Presentation callers must use the metadata returned by their bounded SQLite
query instead of consulting this live execution helper."
  (let ((state (e-harness-executing-session-state harness session-id)))
    (when state
      (e-chat-session-metadata-attachments (plist-get state :metadata)))))

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
    (harness session-id attachment &key canvas
             (current-attachments nil current-attachments-supplied-p))
  "Attach ATTACHMENT to SESSION-ID live context in HARNESS.
ATTACHMENT is a plist with at least :uri.  Attachments are stored as session
current-state references; their contents are read fresh whenever context is
built.  When CANVAS is non-nil, ATTACHMENT replaces the session's primary
canvas attachment.  CURRENT-ATTACHMENTS may provide bounded state already
known by the caller, notably the empty set for a newly created session; this
avoids a redundant durable read without installing a metadata mirror."
  (let* ((attachment (e-chat-session--normalize-attachment attachment canvas))
         (attachments
          (if current-attachments-supplied-p
              (mapcar #'e-chat-session--normalize-attachment
                      (e-chat-session--attachment-list current-attachments))
            (e-chat-session-attachments harness session-id)))
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

(defun e-chat-session--canonical-attachment (attachment)
  "Project ATTACHMENT into the canonical action result object.
Attachment persistence keeps ordinary Elisp lists and nil-valued optional
fields internally; the action boundary uses a vector for collections and the
explicit JSON false sentinel for boolean fields."
  (list :id (or (plist-get attachment :id) e-json-null)
        :uri (or (plist-get attachment :uri) e-json-null)
        :label (or (plist-get attachment :label) e-json-null)
        :canvas (if (plist-get attachment :canvas) t e-json-false)))

(defun e-chat-session--action (handler caller &optional parameters work)
  "Return chat-session action descriptor for HANDLER."
  (if work
      (e-action-create
       :parameters parameters
       :requires-session t
       :work work)
    (e-action-cheap-create
     :id (format "chat_session_%s" (or handler "action"))
     :owner 'chat-session
     :parameters parameters
     :requires-session t
     :runner (lambda (arguments context)
               (funcall caller context arguments)))))

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
    (unless (e-request-terminal-p (e-work-handle-lifecycle handle))
      (setf (e-work-handle-cancel-function handle)
            (lambda (_handle)
              (when (e-backend-request-p request)
                (e-backend-cancel-request request))
              t))
      (setf (e-work-handle-metadata handle)
            (append (e-work-handle-metadata handle)
                    (list :operation 'chat-session-compact
                          :request request))))
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

(defun e-chat-session--context-action-work-runner
    (handle _arguments context)
  "Build detached chat context from action CONTEXT on HANDLE."
  (let ((child
         (e-chat-session-context
          (e-chat-session--action-harness context)
          (e-chat-session--action-session-id context))))
    (setf (e-work-handle-cancel-function handle)
          (lambda (_handle)
            (unless (e-request-terminal-p (e-work-handle-lifecycle child))
              (e-work-cancel child))
            t))
    (e-work-on-settle
     child
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (pcase (plist-get status :state)
           ('finished (e-work-finish handle (plist-get status :result)))
           ('failed (e-work-fail handle (plist-get status :error)))
           ('cancelled (e-work-cancel handle))))))
    :deferred))

(defun e-chat-session--context-action-work ()
  "Return Work spec for detached chat-session context inspection."
  (e-work-spec-create
   :id "chat_session_context"
   :description "Build the active chat session context preview."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'actions
   :runner #'e-chat-session--context-action-work-runner))

(defun e-chat-session--uri-file-name (uri)
  "Return local filename for file URI, or nil."
  (when (string-prefix-p "file://" uri)
    (expand-file-name (substring uri (length "file://")))))

(defun e-chat-session--uri-buffer-name (uri)
  "Return buffer name for buffer URI, or nil."
  (when (string-prefix-p "buffer://" uri)
    (substring uri (length "buffer://"))))

(defun e-chat-session-attachment-live-buffer (attachment)
  "Return a live Emacs buffer for ATTACHMENT when available.

This is the consumer-shaped projection for shells that need to display or
compare an attachment without depending on the chat-session metadata
representation."
  (or (when-let* ((buffer-name (plist-get attachment :buffer-name)))
        (get-buffer buffer-name))
      (when-let* ((buffer-name (e-chat-session--uri-buffer-name
                               (plist-get attachment :uri))))
        (get-buffer buffer-name))
      (when-let* ((file (e-chat-session--uri-file-name
                        (plist-get attachment :uri))))
        (find-buffer-visiting file))))

(defun e-chat-session--attachment-content (attachment)
  "Return ATTACHMENT current content.
Open Emacs buffers win over disk contents so unsaved canvas edits are included
in the next turn's context."
  (let ((uri (plist-get attachment :uri)))
    (cond
     ((when-let* ((buffer (e-chat-session-attachment-live-buffer attachment)))
        (with-current-buffer buffer
          (buffer-substring-no-properties (point-min) (point-max)))))
     ((when-let* ((file (e-chat-session--uri-file-name uri)))
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

(defun e-chat-session--attachment-source (attachment content)
  "Return request-time source provenance for ATTACHMENT CONTENT."
  (e-context-source-create
   :uri (e-chat-session--attachment-uri attachment)
   :label (plist-get attachment :label)
   :content content
   :source-kind 'current-state-attachment
   :provider 'chat-session))

(defun e-chat-session--attachment-section
    (attachment &optional content source)
  "Return a model-facing current-state section for ATTACHMENT.

CONTENT and SOURCE let the caller reuse one attachment read and its matching
request-time source descriptor."
  (let* ((canvas (plist-get attachment :canvas))
         (tag (if canvas "canvas" "attachment"))
         (content (or content
                      (e-chat-session--attachment-content attachment)))
         (source (or source
                     (e-chat-session--attachment-source attachment content))))
    (format "<%s id=\"%s\" uri=\"%s\" label=\"%s\" evidence=\"%s\">\n%s\n</%s>"
            tag
            (e-chat-session--xml-attribute-escape
             (plist-get attachment :id))
            (e-chat-session--xml-attribute-escape
             (plist-get attachment :uri))
            (e-chat-session--xml-attribute-escape
             (plist-get attachment :label))
            (e-chat-session--xml-attribute-escape
             (e-context-source-handle source))
            content
            tag)))

(cl-defun e-chat-session-context-attachments-provider
    (&key harness session-id _turn-id _context-purpose)
  "Return live attachment context messages for SESSION-ID in HARNESS."
  (let* ((request-state
          (and (boundp 'e-harness-context-runtime-current-session-state)
               e-harness-context-runtime-current-session-state))
         (attachments
          (and harness session-id
               (if request-state
                   (e-chat-session-metadata-attachments
                    (plist-get request-state :metadata))
                 (e-chat-session-attachments harness session-id)))))
    (when attachments
      (let* ((has-canvas (cl-some (lambda (attachment)
                                    (plist-get attachment :canvas))
                                  attachments))
             (rendered
              (mapcar
               (lambda (attachment)
                 (let* ((content
                         (e-chat-session--attachment-content attachment))
                        (source
                         (e-chat-session--attachment-source
                          attachment content)))
                   (list :source source
                         :section
                         (e-chat-session--attachment-section
                          attachment content source))))
               attachments))
             (sources (mapcar (lambda (item)
                                (plist-get item :source))
                              rendered))
             (sections (mapcar (lambda (item)
                                 (plist-get item :section))
                               rendered)))
        (list
         (list :role 'system
               e-context-evidence-sources-key sources
               :content
               (string-join
                (cons
                 (concat
                  "Live session context attachments follow. These are "
                  "current-state attachments rebuilt for every turn; they "
                  "replace prior attachment state and are not transcript "
                  "history. When a factual claim relies on an attachment, "
                  "cite the exact `src:' handle in its `evidence' attribute."
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
                 sections)
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
                                  :references (:type "array"
                                                :items (:type "string"))
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
                                  :references (:type "array"
                                                :items (:type "string"))
                                  :metadata (:type "object"))
                     :required ["prompt"]))
                  :abort
                  (e-chat-session--action
                   #'e-chat-session-abort
                   (lambda (context _arguments)
                     (e-chat-session-abort
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
                     (let ((session-id
                            (e-chat-session--action-session-id context))
                           (name (plist-get arguments :name)))
                       (e-chat-session-rename
                        (e-chat-session--action-harness context)
                        session-id name)
                       (list :session-id session-id :name name)))
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
                     (e-chat-session--canonical-attachment
                      (e-chat-session-attach-context
                       (e-chat-session--action-harness context)
                       (e-chat-session--action-session-id context)
                       (plist-get arguments :attachment)
                       :canvas (plist-get arguments :canvas))))
                   '(:type "object"
                     :properties (:attachment (:type "object")
                                  :canvas (:type "boolean"))
                     :required ["attachment"]))
                  :detach-context
                  (e-chat-session--action
                   #'e-chat-session-detach-context
                   (lambda (context arguments)
                     (vconcat
                      (mapcar #'e-chat-session--canonical-attachment
                              (e-chat-session-detach-context
                               (e-chat-session--action-harness context)
                               (e-chat-session--action-session-id context)
                               (plist-get arguments :attachment)))))
                   '(:type "object"
                     :properties (:attachment (:type "string"))
                     :required ["attachment"]))
                  :context
                  (e-chat-session--action
                   #'e-chat-session-context
                   nil nil
                   (e-chat-session--context-action-work)))))

(provide 'e-chat-session)

;;; e-chat-session.el ends here

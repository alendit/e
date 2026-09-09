;;; e-context.el --- Context strategy contract for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral context construction.  Strategies turn session state into
;; backend-ready messages and options without knowing which provider will run.

;;; Code:

(require 'cl-lib)
(require 'e-compaction)
(require 'e-session)
(require 'seq)
(require 'subr-x)

(cl-defstruct (e-context
               (:constructor e-context-create)
               (:conc-name e-context--))
  name
  build
  detached-build)

(cl-defstruct (e-context-provider
               (:constructor e-context-provider-create)
               (:conc-name e-context-provider--))
  name
  (priority 200)
  build
  (cache-placement 'stable-context)
  snapshot-build)

(defconst e-context-evidence-sources-key :evidence-sources
  "Internal message key carrying request-time source descriptors.

Context providers may attach a list of descriptors under this key.  Context
assembly removes the key from backend-facing messages and retains the
descriptors on the corresponding context segment.")

(cl-defun e-context-source-create
    (&key uri label content (source-kind 'current-state) provider)
  "Create an immutable request-time source descriptor for CONTENT at URI.

The short `src:' handle is derived from URI and CONTENT.  The descriptor keeps
CONTENT's full digest for durable audit records.  PROVIDER identifies the
contributing adapter without becoming part of claim policy."
  (unless (and (stringp uri) (not (string-empty-p uri)))
    (signal 'wrong-type-argument (list 'stringp uri)))
  (unless (stringp content)
    (signal 'wrong-type-argument (list 'stringp content)))
  (let* ((identity-digest
          (upcase (secure-hash 'sha256 (concat uri "\0" content))))
         (content-digest (upcase (secure-hash 'sha256 content)))
         (id (substring identity-digest 0 16)))
    (list :id id
          :handle (concat "src:" id)
          :uri uri
          :label (or label uri)
          :source-kind source-kind
          :provider provider
          :content-sha256 content-digest)))

(defun e-context-source-handle (source)
  "Return SOURCE's model-facing `src:' handle."
  (plist-get source :handle))

(defun e-context-name (strategy)
  "Return STRATEGY name."
  (e-context--name strategy))

(defun e-context-transcript-stack-p (strategy)
  "Return non-nil when STRATEGY is the default transcript-stack strategy."
  (eq (e-context-name strategy) 'transcript-stack))

(defun e-context-provider-priority (provider)
  "Return PROVIDER priority."
  (unless (e-context-provider-p provider)
    (signal 'wrong-type-argument (list 'e-context-provider-p provider)))
  (e-context-provider--priority provider))

(put 'e-context-provider-priority 'compiler-macro nil)

(defun e-context-provider-name (provider)
  "Return PROVIDER name."
  (unless (e-context-provider-p provider)
    (signal 'wrong-type-argument (list 'e-context-provider-p provider)))
  (e-context-provider--name provider))

(put 'e-context-provider-name 'compiler-macro nil)

(defun e-context-cache-placement-rank (placement)
  "Return cache-order rank for context PLACEMENT."
  (pcase placement
    ('static-prefix 0)
    ('stable-context 1)
    ('dynamic-context 2)
    (_ (signal 'wrong-type-argument
               (list '(member static-prefix stable-context dynamic-context)
                     placement)))))

(defun e-context-provider-cache-placement (provider)
  "Return PROVIDER prompt-cache placement."
  (unless (e-context-provider-p provider)
    (signal 'wrong-type-argument (list 'e-context-provider-p provider)))
  (let ((placement (if (>= (length provider) 5)
                       (e-context-provider--cache-placement provider)
                     'stable-context)))
    (e-context-cache-placement-rank placement)
    placement))

(put 'e-context-provider-cache-placement 'compiler-macro nil)

(defun e-context-provider--build-function (provider)
  "Return PROVIDER build function."
  (unless (e-context-provider-p provider)
    (signal 'wrong-type-argument (list 'e-context-provider-p provider)))
  (e-context-provider--build provider))

(put 'e-context-provider--build-function 'compiler-macro nil)

(defun e-context-provider--snapshot-build-function (provider)
  "Return PROVIDER snapshot build function, or nil."
  (unless (e-context-provider-p provider)
    (signal 'wrong-type-argument (list 'e-context-provider-p provider)))
  (and (>= (length provider) 6)
       (e-context-provider--snapshot-build provider)))

(put 'e-context-provider--snapshot-build-function 'compiler-macro nil)

(defun e-context-provider--snapshot-purpose-p (purpose)
  "Return non-nil when PURPOSE requests optional snapshot context."
  (memq purpose '(status snapshot optional)))

(defun e-context-segment-fingerprint (messages)
  "Return deterministic fingerprint for backend-neutral MESSAGES."
  (secure-hash 'sha256 (prin1-to-string messages)))

(defun e-context-current-state-segments (context)
  "Return volatile current-state segments from backend-neutral CONTEXT.

The returned segment values are the request-time observation frontier.  They
are deliberately derived from segment metadata rather than from transcript
messages, so a caller can move them to a provider-local replacement channel
without changing the durable history projection."
  (seq-filter
   (lambda (segment)
     (memq (plist-get segment :kind) '(current-state dynamic-context)))
   (plist-get context :segments)))

(defun e-context-current-state-messages (context)
  "Return backend-neutral current-state messages from CONTEXT.

Storage and presentation metadata are removed before the messages cross this
semantic boundary.  An empty result means that the request has no current
state observation; it is distinct from a durable transcript message with the
same content."
  (cl-loop for segment in (e-context-current-state-segments context)
           append (mapcar #'e-context-backend-message
                          (plist-get segment :messages))))

(defun e-context-current-state-fingerprint (context)
  "Return the current-state fingerprint for CONTEXT, or nil when absent."
  (let ((messages (e-context-current-state-messages context)))
    (and messages (e-context-segment-fingerprint messages))))

(cl-defun e-context-segment-create (&key kind id messages)
  "Create a backend-neutral context segment."
  (list :kind kind
        :id id
        :fingerprint (e-context-segment-fingerprint messages)
        :messages messages))

(cl-defun e-context-provider-build
    (provider &key harness session-id turn-id context-purpose)
  "Build read-only context messages with PROVIDER.
HARNESS, SESSION-ID, and TURN-ID identify the current turn.
CONTEXT-PURPOSE may be `turn' for correctness-critical provider requests,
`preview' for explicit user-requested context inspection, or `status',
`snapshot', or `optional' for non-critical callers that must avoid live dynamic
context work.  Dynamic providers without an explicit snapshot builder are
skipped for those optional purposes."
  (let ((snapshot-build (e-context-provider--snapshot-build-function provider))
        (build (e-context-provider--build-function provider)))
    (cond
     ((and (e-context-provider--snapshot-purpose-p context-purpose)
           (functionp snapshot-build))
      (funcall snapshot-build
               :harness harness
               :session-id session-id
               :turn-id turn-id
               :context-purpose context-purpose))
     ((and (e-context-provider--snapshot-purpose-p context-purpose)
           (eq (e-context-provider-cache-placement provider)
               'dynamic-context))
      nil)
     ((functionp build)
      (funcall build
               :harness harness
               :session-id session-id
               :turn-id turn-id
               :context-purpose context-purpose))
     (t
      (signal 'wrong-type-argument (list 'functionp build))))))

(cl-defun e-context-build
    (strategy &key sessions session-id options prefix-messages prefix-segments)
  "Build backend-neutral context with STRATEGY.
SESSIONS and SESSION-ID identify durable state.  OPTIONS are backend-neutral
turn options passed through or adjusted by the strategy.  PREFIX-MESSAGES are
backend-neutral messages that should appear before the session transcript."
  (unless (functionp (e-context--build strategy))
    (signal 'wrong-type-argument (list 'functionp (e-context--build strategy))))
  (let ((context (funcall (e-context--build strategy)
                          :sessions sessions
                          :session-id session-id
                          :options options)))
    (when prefix-messages
      (plist-put context
                 :messages
                 (append prefix-messages (plist-get context :messages))))
    (when (and prefix-messages (not prefix-segments))
      (setq prefix-segments
            (list (e-context-segment-create
                   :kind 'static-prefix
                   :id 'prefix-messages
                   :messages prefix-messages))))
    (when prefix-segments
      (plist-put context
                 :segments
                 (append prefix-segments (plist-get context :segments))))
    context))

(cl-defun e-context-build-detached
    (strategy path &key options prefix-messages prefix-segments)
  "Build backend-neutral context from detached selected session PATH.

PATH is a request-scoped SQLite query result.  This entry point never receives
a session store and therefore cannot reconstruct or consult a durable mirror."
  (unless (functionp (e-context--detached-build strategy))
    (signal 'e-session-storage-error
            (list "Context strategy has no detached SQLite implementation"
                  (e-context--name strategy))))
  (let ((context (funcall (e-context--detached-build strategy)
                          :path path :options options)))
    (when prefix-messages
      (plist-put context :messages
                 (append prefix-messages (plist-get context :messages))))
    (when (and prefix-messages (not prefix-segments))
      (setq prefix-segments
            (list (e-context-segment-create
                   :kind 'static-prefix :id 'prefix-messages
                   :messages prefix-messages))))
    (when prefix-segments
      (plist-put context :segments
                 (append prefix-segments (plist-get context :segments))))
    context))

(defun e-context-backend-message (message)
  "Return MESSAGE without presentation/storage-only metadata."
  (let ((copy (copy-sequence message)))
    (cl-remf copy :created-at)
    (cl-remf copy :id)
    (cl-remf copy :turn-id)
    (cl-remf copy :type)
    (cl-remf copy :parent-id)
    (cl-remf copy :origin)
    (cl-remf copy :board-output-sequence)
    (cl-remf copy e-context-evidence-sources-key)
    (when-let ((metadata (plist-get copy :metadata)))
      (setq metadata (copy-sequence metadata))
      (cl-remf metadata :input-origin)
      (if metadata
          (plist-put copy :metadata metadata)
        (cl-remf copy :metadata)))
    (when (eq (plist-get copy :role) 'compaction-summary)
      (plist-put copy :role 'system))
    copy))

(defun e-context--resolved-tool-call-ids (messages)
  "Return the set of tool-call ids that have a matching tool result in MESSAGES."
  (let ((ids (make-hash-table :test 'equal)))
    (dolist (message messages)
      (when (eq (plist-get message :role) 'tool)
        (when-let ((id (plist-get (plist-get message :content) :tool-call-id)))
          (puthash id t ids))))
    ids))

(defun e-context--drop-orphan-tool-calls (messages)
  "Return MESSAGES without tool-call entries lacking a matching tool result.
A turn interrupted between a tool-call and its result (e.g. Emacs was killed
mid-call) leaves an orphan `tool-call' in the transcript.  Providers reject a
tool-use block with no corresponding tool-result, so the next turn would fail;
drop the unpaired tool-call so the transcript stays valid."
  (let ((resolved (e-context--resolved-tool-call-ids messages)))
    (seq-remove
     (lambda (message)
       (and (eq (plist-get message :role) 'tool-call)
            (let ((id (plist-get (plist-get message :content) :id)))
              (not (and id (gethash id resolved))))))
     messages)))

(defun e-context--backend-messages (messages)
  "Return MESSAGES normalized for backend context."
  (mapcar #'e-context-backend-message
          (e-context--drop-orphan-tool-calls messages)))

(defun e-context--message-entry-message (entry)
  "Return backend-neutral message from session ENTRY."
  (copy-sequence entry))

(defun e-context--compacted-messages (sessions session-id)
  "Return backend messages for SESSION-ID honoring latest compaction."
  (if-let ((compaction (e-session-local-latest-valid-compaction sessions session-id)))
      (let* ((summary (list :role 'compaction-summary
                            :content (plist-get compaction :summary)
                            :id (plist-get compaction :id)
                            :type 'compaction))
             (suffix
              (seq-filter
               (lambda (entry) (eq (plist-get entry :type) 'message))
               (e-session-local-entries-from
                sessions session-id
                (plist-get compaction :first-kept-entry-id)))))
        (e-context--backend-messages
         (cons summary
               (mapcar (lambda (entry)
                         (e-compaction-preview-kept-message
                          (e-context--message-entry-message entry)))
                       suffix))))
    (e-context--backend-messages
     (e-session-local-messages sessions session-id))))

(cl-defun e-context-transcript-stack-create ()
  "Create the classic transcript-stack context strategy."
  (e-context-create
   :name 'transcript-stack
   :build (cl-function
           (lambda (&key sessions session-id options)
             (let ((messages (e-context--compacted-messages
                              sessions session-id)))
             (list :strategy 'transcript-stack
                   :messages messages
                   :segments
                   (list
                    (e-context-segment-create
                     :kind 'history
                     :id 'transcript-history
                     :messages messages))
                   :options options))))
   :detached-build
   (cl-function
    (lambda (&key path options)
      (let* ((messages (plist-get path :messages))
             (compaction (plist-get path :compaction))
             (messages
              (if compaction
                  (cons (list :role 'compaction-summary
                              :content (plist-get compaction :summary)
                              :id (plist-get compaction :id)
                              :type 'compaction)
                        (mapcar #'e-compaction-preview-kept-message messages))
                messages))
             (messages (e-context--backend-messages messages)))
        (list :strategy 'transcript-stack
              :messages messages
              :segments
              (list (e-context-segment-create
                     :kind 'history :id 'transcript-history
                     :messages messages))
              :options options))))))

(provide 'e-context)

;;; e-context.el ends here

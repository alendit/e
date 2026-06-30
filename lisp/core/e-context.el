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

(cl-defstruct (e-context
               (:constructor e-context-create)
               (:conc-name e-context--))
  name
  build)

(cl-defstruct (e-context-provider
               (:constructor e-context-provider-create)
               (:conc-name e-context-provider--))
  name
  (priority 200)
  build
  (cache-placement 'stable-context)
  snapshot-build)

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
CONTEXT-PURPOSE may be `turn' for correctness-critical provider requests, or
`status', `snapshot', or `optional' for non-critical callers that must avoid
live dynamic context work.  Dynamic providers without an explicit snapshot
builder are skipped for those optional purposes."
  (let ((snapshot-build (e-context-provider--snapshot-build-function provider))
        (build (e-context-provider--build-function provider)))
    (cond
     ((and (e-context-provider--snapshot-purpose-p context-purpose)
           (functionp snapshot-build))
      (funcall snapshot-build
               :harness harness
               :session-id session-id
               :turn-id turn-id))
     ((and (e-context-provider--snapshot-purpose-p context-purpose)
           (eq (e-context-provider-cache-placement provider)
               'dynamic-context))
      nil)
     ((functionp build)
      (funcall build
               :harness harness
               :session-id session-id
               :turn-id turn-id))
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

(defun e-context--backend-message (message)
  "Return MESSAGE without presentation/storage-only metadata."
  (let ((copy (copy-sequence message)))
    (cl-remf copy :created-at)
    (cl-remf copy :id)
    (cl-remf copy :turn-id)
    (cl-remf copy :type)
    (cl-remf copy :parent-id)
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
  (mapcar #'e-context--backend-message
          (e-context--drop-orphan-tool-calls messages)))

(defun e-context--message-entry-message (entry)
  "Return backend-neutral message from session ENTRY."
  (copy-sequence entry))

(defun e-context--compacted-messages (sessions session-id)
  "Return backend messages for SESSION-ID honoring latest compaction."
  (if-let ((compaction (e-session-latest-valid-compaction sessions session-id)))
      (let* ((summary (list :role 'compaction-summary
                            :content (plist-get compaction :summary)
                            :id (plist-get compaction :id)
                            :type 'compaction))
             (suffix
              (seq-filter
               (lambda (entry) (eq (plist-get entry :type) 'message))
               (e-session-entries-from
                sessions session-id
                (plist-get compaction :first-kept-entry-id)))))
        (e-context--backend-messages
         (cons summary
               (mapcar (lambda (entry)
                         (e-compaction-preview-kept-message
                          (e-context--message-entry-message entry)))
                       suffix))))
    (e-context--backend-messages
     (e-session-messages sessions session-id))))

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
                   :options options))))))

(provide 'e-context)

;;; e-context.el ends here

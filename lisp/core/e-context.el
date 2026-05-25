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
  build)

(defun e-context-name (strategy)
  "Return STRATEGY name."
  (e-context--name strategy))

(defun e-context-transcript-stack-p (strategy)
  "Return non-nil when STRATEGY is the default transcript-stack strategy."
  (eq (e-context-name strategy) 'transcript-stack))

(defun e-context-provider-priority (provider)
  "Return PROVIDER priority, defaulting stale providers to 200."
  (if (>= (length provider) 4)
      (e-context-provider--priority provider)
    200))

(put 'e-context-provider-priority 'compiler-macro nil)

(defun e-context-provider--build-function (provider)
  "Return PROVIDER build function, tolerating stale provider records."
  (if (>= (length provider) 4)
      (e-context-provider--build provider)
    (aref provider 2)))

(put 'e-context-provider--build-function 'compiler-macro nil)

(cl-defun e-context-provider-build (provider &key harness session-id turn-id)
  "Build read-only context messages with PROVIDER.
HARNESS, SESSION-ID, and TURN-ID identify the current turn."
  (unless (functionp (e-context-provider--build-function provider))
    (signal 'wrong-type-argument
            (list 'functionp (e-context-provider--build-function provider))))
  (funcall (e-context-provider--build-function provider)
           :harness harness
           :session-id session-id
           :turn-id turn-id))

(cl-defun e-context-build
    (strategy &key sessions session-id options prefix-messages)
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

(defun e-context--backend-messages (messages)
  "Return MESSAGES normalized for backend context."
  (mapcar #'e-context--backend-message messages))

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
         (cons summary (mapcar #'e-context--message-entry-message suffix))))
    (e-context--backend-messages
     (e-session-messages sessions session-id))))

(cl-defun e-context-transcript-stack-create ()
  "Create the classic transcript-stack context strategy."
  (e-context-create
   :name 'transcript-stack
   :build (cl-function
           (lambda (&key sessions session-id options)
             (list :strategy 'transcript-stack
                   :messages (e-context--compacted-messages sessions session-id)
                   :options options)))))

(provide 'e-context)

;;; e-context.el ends here

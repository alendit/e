;;; e-context.el --- Context strategy contract for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral context construction.  Strategies turn session state into
;; backend-ready messages and options without knowing which provider will run.

;;; Code:

(require 'cl-lib)
(require 'e-session)

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
    (cl-remf copy :turn-id)
    (cl-remf copy :type)
    (cl-remf copy :parent-id)
    copy))

(defun e-context--backend-messages (messages)
  "Return MESSAGES normalized for backend context."
  (mapcar #'e-context--backend-message messages))

(cl-defun e-context-transcript-stack-create ()
  "Create the classic transcript-stack context strategy."
  (e-context-create
   :name 'transcript-stack
   :build (cl-function
           (lambda (&key sessions session-id options)
             (list :strategy 'transcript-stack
                   :messages (e-context--backend-messages
                              (e-session-messages sessions session-id))
                   :options options)))))

(provide 'e-context)

;;; e-context.el ends here

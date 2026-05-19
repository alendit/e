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
  build)

(cl-defun e-context-provider-build (provider &key harness session-id turn-id)
  "Build read-only context messages with PROVIDER.
HARNESS, SESSION-ID, and TURN-ID identify the current turn."
  (unless (functionp (e-context-provider--build provider))
    (signal 'wrong-type-argument
            (list 'functionp (e-context-provider--build provider))))
  (funcall (e-context-provider--build provider)
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

(cl-defun e-context-transcript-stack-create ()
  "Create the classic transcript-stack context strategy."
  (e-context-create
   :name 'transcript-stack
   :build (cl-function
           (lambda (&key sessions session-id options)
             (list :strategy 'transcript-stack
                   :messages (e-session-messages sessions session-id)
                   :options options)))))

(provide 'e-context)

;;; e-context.el ends here

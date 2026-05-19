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

(cl-defun e-context-build (strategy &key sessions session-id options)
  "Build backend-neutral context with STRATEGY.
SESSIONS and SESSION-ID identify durable state.  OPTIONS are backend-neutral
turn options passed through or adjusted by the strategy."
  (unless (functionp (e-context--build strategy))
    (signal 'wrong-type-argument (list 'functionp (e-context--build strategy))))
  (funcall (e-context--build strategy)
           :sessions sessions
           :session-id session-id
           :options options))

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

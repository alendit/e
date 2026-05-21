;;; e-chat-session.el --- Chat session capability actions for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Semantic chat-session actions hosted by presentation shells.

;;; Code:

(require 'e-capabilities)
(require 'e-harness)
(require 'e-session)
(require 'subr-x)

(cl-defun e-chat-session-submit (harness session-id prompt &key delay)
  "Submit PROMPT to SESSION-ID through HARNESS.
When DELAY is non-nil, pass it to `e-harness-prompt-async'."
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (e-harness-prompt-async harness session-id prompt :delay delay))

(defun e-chat-session-abort (harness session-id)
  "Abort the active chat turn for SESSION-ID through HARNESS."
  (e-harness-abort harness session-id))

(defun e-chat-session-reset (harness session-id)
  "Reset SESSION-ID through HARNESS."
  (e-harness-reset harness session-id))

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
  (e-harness-context harness session-id))

(defun e-chat-session-capability-create ()
  "Create the chat-session capability."
  (e-capability-create
   :id 'chat-session
   :name "Chat Session"
   :actions (list :submit #'e-chat-session-submit
                  :abort #'e-chat-session-abort
                  :reset #'e-chat-session-reset
                  :rename #'e-chat-session-rename
                  :set-model #'e-chat-session-set-model
                  :set-effort #'e-chat-session-set-effort
                  :context #'e-chat-session-context)))

(provide 'e-chat-session)

;;; e-chat-session.el ends here

;;; e-chat.el --- Basic chat presentation for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Minimal Emacs chat buffer for the e harness.  This module owns presentation
;; only: buffer setup, commands, keymaps, and event rendering.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-emacs-base)
(require 'e-harness)
(require 'e-openai)
(require 'e-session)

(defgroup e-chat nil
  "Chat presentation for e."
  :group 'e
  :prefix "e-chat-")

(defcustom e-chat-buffer-name "*e-chat*"
  "Default e chat buffer name."
  :type 'string
  :group 'e-chat)

(defcustom e-chat-default-session-id "default"
  "Default in-memory chat session id."
  :type 'string
  :group 'e-chat)

(defvar-local e-chat-harness nil
  "Harness used by the current chat buffer.")

(defvar-local e-chat-session-id nil
  "Session id used by the current chat buffer.")

(defvar e-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'e-chat-submit)
    (define-key map (kbd "C-c C-k") #'e-chat-abort)
    (define-key map (kbd "C-c C-r") #'e-chat-reset)
    map)
  "Keymap for `e-chat-mode'.")

(define-derived-mode e-chat-mode special-mode "e-chat"
  "Major mode for e chat buffers.")

(defun e-chat--default-harness ()
  "Create the default Codex-backed chat harness."
  (let ((harness (e-openai-codex-create-harness)))
    (e-harness-activate-layer harness (e-emacs-base-layer-create))
    harness))

(defun e-chat--ensure-session (harness session-id)
  "Ensure SESSION-ID exists in HARNESS."
  (condition-case err
      (e-harness-create-session harness :id session-id)
    (e-session-duplicate
     nil)))

(defun e-chat--insert-line (line)
  "Insert LINE into the current chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert line "\n")))

(defun e-chat--clear ()
  "Clear and initialize the current chat buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "e chat\n\n")))

(defun e-chat--set-status (status)
  "Set chat buffer STATUS."
  (setq header-line-format (format "e chat: %s" status)))

(defun e-chat--message-line (message)
  "Return a rendered line for MESSAGE."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('user (format "User: %s" content))
      ('assistant (format "Assistant: %s" content))
      ('tool (format "Tool: %S" content))
      (_ (format "%s: %S" role content)))))

(defun e-chat--render-event (event)
  "Render harness EVENT into the current chat buffer."
  (pcase (plist-get event :type)
    ('turn-started
     (e-chat--set-status (format "running %s" (plist-get event :turn-id)))
     (e-chat--insert-line
      (format "Turn started: %s" (plist-get event :turn-id))))
    ('turn-finished
     (e-chat--set-status "idle")
     (e-chat--insert-line "Turn finished"))
    ('turn-failed
     (e-chat--set-status "error")
     (e-chat--insert-line
      (format "Turn failed: %s"
              (plist-get (plist-get event :payload) :error))))
    ('turn-cancelled
     (e-chat--set-status "cancelled")
     (e-chat--insert-line "Turn cancelled"))
    ('message-added
     (e-chat--insert-line
      (e-chat--message-line
       (plist-get (plist-get event :payload) :message))))
    ('assistant-delta
     (e-chat--insert-line
      (format "Assistant delta: %s"
              (plist-get (plist-get event :payload) :content))))
    ('tool-finished
     (e-chat--insert-line
      (format "Tool finished: %S"
              (plist-get (plist-get event :payload) :result))))
    ('session-reset
     (e-chat--set-status "idle")
     (e-chat--insert-line "Session reset"))
    (_
     (e-chat--insert-line (format "Event: %S" event)))))

(cl-defun e-chat-open (&key harness session-id)
  "Open and return an e chat buffer.
HARNESS and SESSION-ID are injectable for tests.  Interactive calls create a
Codex-backed harness with the emacs-base layer active."
  (interactive)
  (let* ((chat-harness (or harness (e-chat--default-harness)))
         (chat-session-id (or session-id e-chat-default-session-id))
         (buffer (get-buffer-create e-chat-buffer-name)))
    (e-chat--ensure-session chat-harness chat-session-id)
    (with-current-buffer buffer
      (e-chat-mode)
      (setq-local e-chat-harness chat-harness)
      (setq-local e-chat-session-id chat-session-id)
      (e-chat--clear)
      (e-chat--set-status "idle")
      (e-harness-subscribe
       chat-harness
       (lambda (event)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (e-chat--render-event event))))))
    (when (called-interactively-p 'interactive)
      (pop-to-buffer buffer))
    buffer))

;;;###autoload
(defun e-chat ()
  "Open the default e chat buffer."
  (interactive)
  (pop-to-buffer (e-chat-open)))

(defun e-chat-submit (prompt)
  "Submit PROMPT through the current chat buffer harness."
  (interactive (list (read-string "Prompt: ")))
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (when (string-empty-p prompt)
    (user-error "Prompt must not be empty"))
  (e-chat--set-status "queued")
  (e-harness-prompt-async e-chat-harness e-chat-session-id prompt))

(defun e-chat-abort ()
  "Abort the active turn for the current chat buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-harness-abort e-chat-harness e-chat-session-id))

(defun e-chat-reset ()
  "Reset the current chat session and rendered buffer."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (e-chat--clear)
  (e-harness-reset e-chat-harness e-chat-session-id))

(provide 'e-chat)

;;; e-chat.el ends here

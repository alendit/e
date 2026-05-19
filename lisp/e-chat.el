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

(defvar-local e-chat--prompt-marker nil
  "Marker at the beginning of the editable chat prompt text.")

(defconst e-chat--prompt-prefix "> "
  "Prefix shown before editable e chat prompt text.")

(defconst e-chat--prompt-marker-prefix ">"
  "Structural prefix that marks an editable e chat prompt line.")

(defun e-chat--make-mode-map (&optional map)
  "Return MAP configured as the local keymap for `e-chat-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'e-chat-submit)
    (define-key map (kbd "RET") #'e-chat-submit)
    (define-key map (kbd "C-c C-k") #'e-chat-abort)
    (define-key map (kbd "C-c C-r") #'e-chat-reset)
    map))

(defvar e-chat-mode-map (e-chat--make-mode-map)
  "Keymap for `e-chat-mode'.")

(setq e-chat-mode-map (e-chat--make-mode-map e-chat-mode-map))

(define-derived-mode e-chat-mode text-mode "e-chat"
  "Major mode for e chat buffers.")

(defun e-chat--configure-modal-state ()
  "Configure optional modal editing packages for `e-chat-mode'."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'e-chat-mode 'normal)))

(defun e-chat--line-beginning-at (position)
  "Return line beginning at POSITION."
  (save-excursion
    (goto-char position)
    (line-beginning-position)))

(defun e-chat--default-harness ()
  "Create the default Codex-backed chat harness."
  (let ((harness (e-openai-codex-create-harness)))
    (e-harness-activate-layer harness (e-emacs-base-layer-create))
    harness))

(defun e-chat--ensure-session (harness session-id)
  "Ensure SESSION-ID exists in HARNESS."
  (condition-case nil
      (e-harness-create-session harness :id session-id)
    (e-session-duplicate
     nil)))

(defun e-chat--insert-line (line)
  "Insert LINE into the current chat buffer."
  (let ((position (if (and (markerp e-chat--prompt-marker)
                           (marker-position e-chat--prompt-marker))
                      (e-chat--line-beginning-at
                       (marker-position e-chat--prompt-marker))
                    (point-max))))
    (save-excursion
      (goto-char position)
      (insert line "\n"))))

(defun e-chat--insert-prompt ()
  "Insert an editable prompt at the end of the current chat buffer."
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (insert e-chat--prompt-prefix)
  (setq e-chat--prompt-marker (point-marker))
  (set-marker-insertion-type e-chat--prompt-marker nil))

(defun e-chat--prompt-start-at-buffer-end ()
  "Return active prompt text start inferred from the final buffer line."
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (when (looking-at-p (regexp-quote e-chat--prompt-marker-prefix))
      (+ (line-beginning-position)
         (length e-chat--prompt-marker-prefix)))))

(defun e-chat--ensure-prompt-marker ()
  "Ensure `e-chat--prompt-marker' points at the visible active prompt."
  (let ((start (e-chat--prompt-start-at-buffer-end)))
    (cond
     (start
      (unless (markerp e-chat--prompt-marker)
        (setq e-chat--prompt-marker (make-marker)))
      (set-marker e-chat--prompt-marker start)
      (set-marker-insertion-type e-chat--prompt-marker nil)
      start)
     ((and (markerp e-chat--prompt-marker)
           (marker-position e-chat--prompt-marker))
      (marker-position e-chat--prompt-marker))
     (t
      (user-error "No active e chat prompt")))))

(defun e-chat--prompt-text ()
  "Return the current editable prompt text."
  (e-chat--ensure-prompt-marker)
  (string-trim
   (buffer-substring-no-properties e-chat--prompt-marker (point-max))))

(defun e-chat--delete-prompt ()
  "Delete the editable prompt from the current chat buffer."
  (when (ignore-errors (e-chat--ensure-prompt-marker))
    (let ((start (e-chat--line-beginning-at
                  (marker-position e-chat--prompt-marker))))
      (delete-region start (point-max)))
    (set-marker e-chat--prompt-marker nil)))

(defun e-chat--clear ()
  "Clear and initialize the current chat buffer."
  (erase-buffer)
  (insert "e chat\n\n")
  (e-chat--insert-prompt))

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
     (e-chat--insert-line "Turn finished")
     (e-chat--insert-prompt))
    ('turn-failed
     (e-chat--set-status "error")
     (e-chat--insert-line
      (format "Turn failed: %s"
              (plist-get (plist-get event :payload) :error)))
     (e-chat--insert-prompt))
    ('turn-cancelled
     (e-chat--set-status "cancelled")
     (e-chat--insert-line "Turn cancelled")
     (e-chat--insert-prompt))
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
    ('backend-empty-output
     (e-chat--insert-line "Backend returned no assistant output"))
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
      (e-chat--configure-modal-state)
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

(defun e-chat-submit (&optional prompt)
  "Submit PROMPT or the current editable prompt text."
  (interactive)
  (unless (and e-chat-harness e-chat-session-id)
    (user-error "This buffer is not attached to an e chat session"))
  (setq prompt (or prompt (e-chat--prompt-text)))
  (when (string-empty-p prompt)
    (user-error "Prompt must not be empty"))
  (e-chat--delete-prompt)
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

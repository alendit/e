;;; e-chat-starter.el --- Global one-shot chat starter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Presentation shell for asking one contextual question from any buffer and
;; continuing the same persisted chat session when needed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-events)
(require 'e-harness)
(require 'e-harness-registry)
(require 'e-shells)
(require 'e-startup)

(defgroup e-chat-starter nil
  "Global one-shot chat starter."
  :group 'e-chat
  :prefix "e-chat-starter-")

(defcustom e-chat-starter-buffer-name "*e-start-here*"
  "Buffer name for the global chat starter popup."
  :type 'string
  :group 'e-chat-starter)

(defcustom e-chat-starter-context-line-radius 2
  "Number of lines around point captured when no region is active."
  :type 'integer
  :group 'e-chat-starter)

(defcustom e-chat-starter-popup-width 72
  "Preferred popup window width."
  :type 'integer
  :group 'e-chat-starter)

(defcustom e-chat-starter-popup-height 14
  "Preferred popup window height."
  :type 'integer
  :group 'e-chat-starter)

(defcustom e-chat-starter-display-strategy 'window
  "Display strategy for the starter popup.
The first implementation uses normal Emacs display windows.  The custom is kept
so a child-frame adapter can be added without changing controller logic."
  :type '(choice (const :tag "Normal window" window))
  :group 'e-chat-starter)

(cl-defstruct e-chat-starter-state
  harness
  session-id
  source-reference
  question
  status
  turn-id
  latest-answer
  error-message
  subscription
  source-window
  source-buffer
  buffer
  progress)

(defvar e-chat-starter-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "c") #'e-chat-starter-continue)
    (define-key map (kbd "o") #'e-chat-starter-open-answer)
    (define-key map (kbd "y") #'e-chat-starter-copy-answer)
    (define-key map (kbd "q") #'e-chat-starter-dismiss)
    (define-key map (kbd "RET") #'e-chat-starter-continue)
    map)
  "Keymap for `e-chat-starter-mode'.")

(defvar-local e-chat-starter--state nil
  "Starter state owned by the current popup buffer.")

(define-derived-mode e-chat-starter-mode special-mode "e-chat-starter"
  "Major mode for one-shot e chat starter popups."
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'e-chat-starter--cleanup nil t))

(defvar-local e-chat-starter-answer-session-id nil
  "Session id that produced this answer buffer.")

(defun e-chat-starter--ensure-reference-id (reference)
  "Return REFERENCE with a starter-local id."
  (let ((reference (copy-sequence reference)))
    (unless (plist-get reference :id)
      (setq reference (plist-put reference :id "source")))
    reference))

(defun e-chat-starter--capture-source ()
  "Capture source context at point for a one-shot starter question."
  (let* ((line-radius (max 0 e-chat-starter-context-line-radius))
         (reference (e-chat-capture-source-reference line-radius)))
    (e-chat-starter--ensure-reference-id reference)))

(defun e-chat-starter--format-prompt (question reference)
  "Return model-facing prompt for QUESTION and source REFERENCE."
  (let* ((reference (e-chat-starter--ensure-reference-id reference))
         (text (string-trim
                (format "%s\n\n%s"
                        question
                        (e-chat-reference-placeholder reference)))))
    (e-chat-format-reference-prompt text (list reference))))

(defun e-chat-starter--current-state ()
  "Return starter state for the current buffer."
  (or e-chat-starter--state
      (user-error "No e chat starter state in this buffer")))

(defun e-chat-starter--insert-block (text face)
  "Insert TEXT using FACE."
  (let ((start (point)))
    (insert (string-trim-right (or text "")) "\n")
    (add-text-properties start (point) `(font-lock-face ,face))))

(defun e-chat-starter--render-status (state)
  "Insert a compact status line for STATE."
  (pcase (e-chat-starter-state-status state)
    ('running
     (insert "Thinking")
     (when-let ((progress (e-chat-starter-state-progress state)))
       (insert " - " progress))
     (insert "\n"))
    ('failed
     (e-chat-starter--insert-block
      (or (e-chat-starter-state-error-message state)
          "Starter turn failed")
      'error))
    ('answered
     (e-chat-starter--insert-block
      (or (e-chat-starter-state-latest-answer state) "")
      'e-chat-final-assistant-face))
    (_
     (insert "Ready\n"))))

(defun e-chat-starter--render-actions (state)
  "Insert action hints for STATE."
  (pcase (e-chat-starter-state-status state)
    ((or 'answered 'failed 'continued)
     (insert "\n[c] continue   ")
     (when (e-chat-starter-state-latest-answer state)
       (insert "[o] open buffer   [y] copy   "))
     (insert "[q] close\n"))
    (_
     (insert "\n[q] close\n"))))

(defun e-chat-starter--render ()
  "Render the current starter popup."
  (let* ((state (e-chat-starter--current-state))
         (reference (e-chat-starter-state-source-reference state))
         (inhibit-read-only t))
    (erase-buffer)
    (insert "Ask e here\n\n")
    (e-chat-starter--insert-block
     (e-chat-starter-state-question state)
     'e-chat-user-face)
    (when-let ((label (plist-get reference :label)))
      (insert (propertize label 'font-lock-face 'shadow) "\n"))
    (insert "\n")
    (e-chat-starter--render-status state)
    (e-chat-starter--render-actions state)
    (goto-char (point-min))))

(defun e-chat-starter--display-buffer (buffer)
  "Display starter BUFFER using the configured fallback window strategy."
  (display-buffer
   buffer
   `((display-buffer-reuse-window display-buffer-pop-up-window)
     (window-width . ,e-chat-starter-popup-width)
     (window-height . ,e-chat-starter-popup-height))))

(defun e-chat-starter--cleanup ()
  "Clean up the current starter popup subscription."
  (when-let ((state e-chat-starter--state))
    (when (and (e-chat-starter-state-harness state)
               (e-chat-starter-state-subscription state))
      (e-harness-unsubscribe
       (e-chat-starter-state-harness state)
       (e-chat-starter-state-subscription state))
      (setf (e-chat-starter-state-subscription state) nil))))

(defun e-chat-starter--render-state-buffer (state)
  "Render STATE when its popup buffer is live."
  (when-let ((buffer (e-chat-starter-state-buffer state)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'e-chat-starter-mode)
          (e-chat-starter--render))))))

(defun e-chat-starter--event-error-message (event)
  "Return a compact error message for EVENT."
  (or (plist-get (plist-get event :payload) :error)
      (format "%s" (plist-get event :type))))

(defun e-chat-starter--handle-event (state event)
  "Update STATE from harness EVENT."
  (when (equal (plist-get event :session-id)
               (e-chat-starter-state-session-id state))
    (pcase (plist-get event :type)
      ('turn-started
       (setf (e-chat-starter-state-turn-id state)
             (plist-get event :turn-id))
       (setf (e-chat-starter-state-status state) 'running))
      ('reasoning-delta
       (setf (e-chat-starter-state-progress state)
             (or (plist-get (plist-get event :payload) :content)
                 "thinking")))
      ('tool-started
       (setf (e-chat-starter-state-progress state) "using tool"))
      ('tool-finished
       (setf (e-chat-starter-state-progress state) "tool finished"))
      ('message-added
       (let ((message (plist-get (plist-get event :payload) :message)))
         (when (and (eq (plist-get message :role) 'assistant)
                    (stringp (plist-get message :content))
                    (not (string-empty-p
                          (string-trim (plist-get message :content)))))
           (setf (e-chat-starter-state-latest-answer state)
                 (plist-get message :content))
           (setf (e-chat-starter-state-status state) 'answered))))
      ('turn-finished
       (when (e-chat-starter-state-latest-answer state)
         (setf (e-chat-starter-state-status state) 'answered)))
      ((or 'turn-failed 'turn-cancelled 'backend-empty-output)
       (setf (e-chat-starter-state-status state) 'failed)
       (setf (e-chat-starter-state-error-message state)
             (e-chat-starter--event-error-message event))))
    (e-chat-starter--render-state-buffer state))
  state)

(defun e-chat-starter--subscribe (state)
  "Subscribe STATE to its session events."
  (let* ((harness (e-chat-starter-state-harness state))
         (session-id (e-chat-starter-state-session-id state))
         (subscription
          (e-harness-subscribe
           harness
           (lambda (event)
             (e-chat-starter--handle-event state event))
           :session-id session-id)))
    (setf (e-chat-starter-state-subscription state) subscription)
    subscription))

(cl-defun e-chat-starter--start (question &key harness display delay)
  "Start a one-shot starter QUESTION.
HARNESS defaults to the chat default harness.  When DISPLAY is non-nil, show the
popup buffer.  DELAY is forwarded to the chat session submit path for tests."
  (let* ((harness (or harness (e-chat--default-harness)))
         (reference (e-chat-starter--capture-source))
         (session (e-chat-create-session
                   :harness harness
                   :metadata (list :origin :global-session-starter
                                   :source-reference reference)))
         (session-id (plist-get session :id))
         (buffer (get-buffer-create e-chat-starter-buffer-name))
         (state (make-e-chat-starter-state
                 :harness harness
                 :session-id session-id
                 :source-reference reference
                 :question question
                 :status 'running
                 :source-window (selected-window)
                 :source-buffer (current-buffer)
                 :buffer buffer)))
    (with-current-buffer buffer
      (e-chat-starter-mode)
      (setq-local e-chat-starter--state state)
      (e-chat-starter--render))
    (e-chat-starter--subscribe state)
    (e-chat-submit-session
     harness
     session-id
     (e-chat-starter--format-prompt question reference)
     :references (list reference)
     :delay delay)
    (when display
      (e-chat-starter--display-buffer buffer))
    state))

;;;###autoload
(defun e-chat-start-here (question)
  "Ask one contextual e chat QUESTION about the current buffer at point."
  (interactive (list (read-string "Ask e about this: ")))
  (unless (and (stringp question)
               (not (string-empty-p (string-trim question))))
    (user-error "Question must not be empty"))
  (e-chat-starter--start question :display t))

(defun e-chat-starter-continue ()
  "Open the starter session in a normal chat window."
  (interactive)
  (let ((state (e-chat-starter--current-state)))
    (setf (e-chat-starter-state-status state) 'continued)
    (e-chat-open-session
     (e-chat-starter-state-harness state)
     (e-chat-starter-state-session-id state)
     t)))

(defun e-chat-starter--answer-buffer-name (state)
  "Return a buffer name for STATE's latest answer."
  (format "*e-answer:%s*"
          (or (plist-get (e-chat-starter-state-source-reference state) :label)
              (e-chat-starter-state-session-id state))))

(defun e-chat-starter-open-answer ()
  "Open the latest starter answer in a separate buffer."
  (interactive)
  (let* ((state (e-chat-starter--current-state))
         (answer (or (e-chat-starter-state-latest-answer state)
                     (user-error "No starter answer yet")))
         (buffer (get-buffer-create
                  (e-chat-starter--answer-buffer-name state))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Source: %s\nSession: %s\n\n%s\n"
                        (or (plist-get
                             (e-chat-starter-state-source-reference state)
                             :label)
                            "")
                        (e-chat-starter-state-session-id state)
                        answer))
        (if (require 'markdown-mode nil t)
            (markdown-mode)
          (text-mode))
        (setq-local e-chat-starter-answer-session-id
                    (e-chat-starter-state-session-id state))
        (setq-local e-chat-starter-answer-question
                    (e-chat-starter-state-question state))
        (goto-char (point-min))))
    (when (called-interactively-p 'interactive)
      (pop-to-buffer buffer))
    buffer))

(defun e-chat-starter-copy-answer ()
  "Copy the latest starter answer."
  (interactive)
  (let* ((state (e-chat-starter--current-state))
         (answer (or (e-chat-starter-state-latest-answer state)
                     (user-error "No starter answer yet"))))
    (kill-new answer)
    (when (called-interactively-p 'interactive)
      (message "Copied e starter answer"))
    answer))

(defun e-chat-starter-dismiss ()
  "Dismiss the starter popup and clean up subscriptions."
  (interactive)
  (let* ((state (e-chat-starter--current-state))
         (buffer (or (e-chat-starter-state-buffer state)
                     (current-buffer))))
    (with-current-buffer buffer
      (e-chat-starter--cleanup))
    (when (and (called-interactively-p 'interactive)
               (buffer-live-p buffer))
      (kill-buffer buffer))))

;;;###autoload
(defun e-chat-starter-shell ()
  "Return the global session starter shell manifest."
  (e-shell-create
   :id 'global-session-starter
   :name "Global Session Starter"
   :summary "Ask one contextual chat question from any buffer."
   :required-capabilities '(chat-session)
   :metadata '(:depends-on (chat))
   :commands
   (list
    (e-shell-command-create
     :id 'start-here
     :summary "Ask a contextual one-shot chat question at point."
     :interactive 'e-chat-start-here
     :function 'e-chat-start-here
     :scope 'global))))

(defun e-chat-starter-startup ()
  "Register the global session starter shell."
  (e-shell-register (e-chat-starter-shell)))

(add-hook 'e-startup-shell-hook #'e-chat-starter-startup)

(provide 'e-chat-starter)

;;; e-chat-starter.el ends here

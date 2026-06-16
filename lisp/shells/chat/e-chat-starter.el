;;; e-chat-starter.el --- Global one-shot chat starter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Presentation shell for asking one contextual question from any buffer and
;; continuing the same persisted chat session when needed.

;;; Code:

(declare-function markdown-mode "ext:markdown-mode")

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

(defcustom e-chat-starter-progress-interval 0.6
  "Seconds between live repaints of the starter popup while a turn runs.
The elapsed \"Thinking for ...\" duration advances on wall-clock time, so the
popup needs its own repaint tick: harness events alone do not arrive often
enough during a long model turn to keep the duration current."
  :type 'number
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
  popup-window
  progress
  activity-events
  progress-timer)

(defun e-chat-starter--make-mode-map (&optional map)
  "Return MAP configured as the keymap for `e-chat-starter-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "c") #'e-chat-starter-continue)
    (define-key map (kbd "o") #'e-chat-starter-open-answer)
    (define-key map (kbd "y") #'e-chat-starter-copy-answer)
    (define-key map (kbd "q") #'e-chat-starter-dismiss)
    (define-key map (kbd "<escape>") #'e-chat-starter-dismiss)
    (define-key map (kbd "C-c C-c") #'e-chat-starter-continue)
    (define-key map (kbd "RET") #'e-chat-starter-continue)
    map))

(defvar e-chat-starter-mode-map
  (e-chat-starter--make-mode-map)
  "Keymap for `e-chat-starter-mode'.")

(setq e-chat-starter-mode-map
      (e-chat-starter--make-mode-map e-chat-starter-mode-map))

(defvar-local e-chat-starter--state nil
  "Starter state owned by the current popup buffer.")

(defun e-chat-starter--enforce-modal-editing-policy ()
  "Disable modal editing when it is reactivated in starter buffers."
  (when (and (derived-mode-p 'e-chat-starter-mode)
             (boundp 'evil-local-mode)
             evil-local-mode)
    (e-chat--disable-modal-editing)))

(defun e-chat-starter--configure-modal-editing-policy ()
  "Configure modal editors to keep `e-chat-starter-mode' non-normal."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'e-chat-starter-mode 'emacs)))

(define-derived-mode e-chat-starter-mode special-mode "e-chat-starter"
  "Major mode for one-shot e chat starter popups."
  (setq-local truncate-lines nil)
  (add-hook 'evil-local-mode-hook
            #'e-chat-starter--enforce-modal-editing-policy nil t)
  (add-hook 'kill-buffer-hook #'e-chat-starter--cleanup nil t)
  (e-chat--disable-modal-editing))

(e-chat-starter--configure-modal-editing-policy)
(with-eval-after-load 'evil
  (e-chat-starter--configure-modal-editing-policy))

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

(defun e-chat-starter--insert-answer-block (text)
  "Insert assistant answer TEXT with Markdown presentation."
  (let ((start (point)))
    (insert (string-trim-right (or text "")) "\n")
    (e-chat--apply-assistant-markdown start (point))
    (e-chat--apply-final-assistant-face start (point))))

(defun e-chat-starter--activity-turn-id (state event)
  "Return the activity turn id for STATE and EVENT."
  (or (plist-get event :turn-id)
      (e-chat-starter-state-turn-id state)
      (e-chat-starter-state-session-id state)))

(defun e-chat-starter--activity-event (event event-type &optional payload)
  "Return a durable chat activity EVENT with EVENT-TYPE and PAYLOAD."
  (list :event-type event-type
        :turn-id (plist-get event :turn-id)
        :created-at (plist-get event :created-at)
        :payload (or payload (plist-get event :payload))))

(defun e-chat-starter--activity-events-for-event (state event)
  "Return chat activity events derived from starter harness EVENT for STATE."
  (let* ((turn-id (e-chat-starter--activity-turn-id state event))
         (event (plist-put (copy-sequence event) :turn-id turn-id)))
    (pcase (plist-get event :type)
      ('turn-started
       (list (e-chat-starter--activity-event event 'turn-started)))
      ('turn-finished
       (list (e-chat-starter--activity-event event 'turn-finished)))
      ((or 'provider-request-started 'provider-request-finished
           'reasoning-delta 'tool-started 'tool-finished
           'turn-failed 'turn-cancelled)
       (list (e-chat-starter--activity-event event (plist-get event :type))))
      (_ nil))))

(defun e-chat-starter--record-activity-event (state event)
  "Record starter harness EVENT as reusable chat activity for STATE."
  (when-let ((events (e-chat-starter--activity-events-for-event state event)))
    (setf (e-chat-starter-state-activity-events state)
          (append (e-chat-starter-state-activity-events state) events))))

(defun e-chat-starter--activity-record (state)
  "Return an `e-chat' activity record rebuilt from STATE events."
  (when-let ((turn-id (or (e-chat-starter-state-turn-id state)
                         (e-chat-starter-state-session-id state))))
    (setq-local e-chat--turn-registry (make-hash-table :test 'equal))
    (dolist (event (e-chat-starter-state-activity-events state))
      (when (equal (plist-get event :turn-id) turn-id)
        (e-chat--record-activity-event turn-id event)))
    (e-chat--existing-turn-record turn-id)))

(defun e-chat-starter--activity-text (state record)
  "Return chat-formatted activity text for STATE and RECORD."
  (when record
    (pcase (e-chat-starter-state-status state)
      ((or 'answered 'failed 'continued)
       (or (when-let ((summary (e-chat--activity-summary-text record)))
             (concat summary "\n"))
           (e-chat--transient-text record)))
      (_
       (e-chat--transient-text record)))))

(defun e-chat-starter--insert-activity-block (state)
  "Insert STATE activity using the same formatter as normal chat."
  (when-let* ((record (e-chat-starter--activity-record state))
              (text (e-chat-starter--activity-text state record)))
    (let ((start (point)))
      (e-chat--insert-protected text 'e-chat-system-face)
      (e-chat--apply-activity-separator-face start (point)))))

(defun e-chat-starter--render-status (state)
  "Insert a compact status line for STATE."
  (e-chat-starter--insert-activity-block state)
  (pcase (e-chat-starter-state-status state)
    ('running
     (unless (e-chat-starter-state-activity-events state)
       (e-chat-starter--insert-block "Thinking" 'e-chat-system-face)))
    ('failed
     (e-chat-starter--insert-block
      (or (e-chat-starter-state-error-message state)
          "Starter turn failed")
      'error))
    ('answered
     (e-chat-starter--insert-answer-block
      (or (e-chat-starter-state-latest-answer state) "")))
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
  (let ((window
         (display-buffer
          buffer
          `((display-buffer-reuse-window display-buffer-pop-up-window)
            (window-width . ,e-chat-starter-popup-width)
            (window-height . ,e-chat-starter-popup-height)))))
    (when (window-live-p window)
      (select-window window))
    window))

(defun e-chat-starter--cleanup ()
  "Clean up the current starter popup subscription and repaint timer."
  (when-let ((state e-chat-starter--state))
    (e-chat-starter--stop-progress-timer state)
    (when (and (e-chat-starter-state-harness state)
               (e-chat-starter-state-subscription state))
      (e-harness-unsubscribe
       (e-chat-starter-state-harness state)
       (e-chat-starter-state-subscription state))
      (setf (e-chat-starter-state-subscription state) nil))))

(defun e-chat-starter--close-state-buffer (state)
  "Close STATE's popup buffer and clean up its subscription."
  (when-let ((buffer (e-chat-starter-state-buffer state)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (e-chat-starter--cleanup))
      (setf (e-chat-starter-state-buffer state) nil)
      (when-let ((source-window (e-chat-starter-state-source-window state)))
        (when (window-live-p source-window)
          (select-window source-window)))
      (let ((popup-windows
             (delete-dups
              (delq nil
                    (append
                     (list (e-chat-starter-state-popup-window state))
                     (get-buffer-window-list buffer nil t))))))
        (dolist (window popup-windows)
          (when (and (window-live-p window)
                     (window-parent window))
            (delete-window window))))
      (setf (e-chat-starter-state-popup-window state) nil)
      (kill-buffer buffer))))

(defun e-chat-starter--render-state-buffer (state)
  "Render STATE when its popup buffer is live."
  (when-let ((buffer (e-chat-starter-state-buffer state)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'e-chat-starter-mode)
          (e-chat-starter--render))))))

(defun e-chat-starter--stop-progress-timer (state)
  "Cancel STATE's live progress repaint timer when one is running."
  (when-let ((timer (e-chat-starter-state-progress-timer state)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (e-chat-starter-state-progress-timer state) nil)))

(defun e-chat-starter--ensure-progress-timer (state)
  "Start STATE's live progress repaint timer if it is not already running.
The timer repaints the popup on `e-chat-starter-progress-interval' so the
elapsed thinking duration advances between harness events.  It stops itself once
the turn settles or the popup buffer dies."
  (unless (or (e-chat-starter-state-progress-timer state)
              (not (numberp e-chat-starter-progress-interval)))
    (setf (e-chat-starter-state-progress-timer state)
          (run-at-time
           e-chat-starter-progress-interval
           e-chat-starter-progress-interval
           (lambda ()
             (let ((buffer (e-chat-starter-state-buffer state)))
               (if (and (eq (e-chat-starter-state-status state) 'running)
                        (buffer-live-p buffer))
                   (e-chat-starter--render-state-buffer state)
                 (e-chat-starter--stop-progress-timer state))))))))

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
       (setf (e-chat-starter-state-status state) 'running)
       (e-chat-starter--record-activity-event state event))
      ('provider-request-started
       (setf (e-chat-starter-state-status state) 'running)
       (e-chat-starter--record-activity-event state event))
      ('provider-request-finished
       (e-chat-starter--record-activity-event state event))
      ('reasoning-delta
       (setf (e-chat-starter-state-progress state)
             (or (plist-get (plist-get event :payload) :content)
                 "thinking"))
       (e-chat-starter--record-activity-event state event))
      ('tool-started
       (setf (e-chat-starter-state-progress state) "using tool")
       (e-chat-starter--record-activity-event state event))
      ('tool-finished
       (setf (e-chat-starter-state-progress state) "tool finished")
       (e-chat-starter--record-activity-event state event))
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
       (e-chat-starter--record-activity-event state event)
       (when (e-chat-starter-state-latest-answer state)
         (setf (e-chat-starter-state-status state) 'answered)))
      ((or 'turn-failed 'turn-cancelled 'backend-empty-output)
       (unless (eq (plist-get event :type) 'backend-empty-output)
         (e-chat-starter--record-activity-event state event))
       (setf (e-chat-starter-state-status state) 'failed)
       (setf (e-chat-starter-state-error-message state)
             (e-chat-starter--event-error-message event))))
    (if (eq (e-chat-starter-state-status state) 'running)
        (e-chat-starter--ensure-progress-timer state)
      (e-chat-starter--stop-progress-timer state))
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
      (setf (e-chat-starter-state-popup-window state)
            (e-chat-starter--display-buffer buffer)))
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
    (let ((harness (e-chat-starter-state-harness state))
          (session-id (e-chat-starter-state-session-id state)))
      (e-chat-starter--close-state-buffer state)
      (e-chat-open-session harness session-id t))))

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
    (e-chat-starter--close-state-buffer state)
    (pop-to-buffer buffer)
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
    (e-chat-starter--close-state-buffer state)
    answer))

(defun e-chat-starter-dismiss ()
  "Dismiss the starter popup and clean up subscriptions."
  (interactive)
  (let* ((state (e-chat-starter--current-state))
         (buffer (or (e-chat-starter-state-buffer state)
                     (current-buffer))))
    (with-current-buffer buffer
      (e-chat-starter--cleanup))
    (when (buffer-live-p buffer)
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

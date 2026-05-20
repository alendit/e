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

(defface e-chat-user-face
  '((t :inherit font-lock-string-face :extend t))
  "Face used for user-authored chat blocks."
  :group 'e-chat)

(defface e-chat-assistant-face
  '((t :inherit font-lock-function-name-face :extend t))
  "Face used for assistant response chat blocks."
  :group 'e-chat)

(defface e-chat-system-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face used for compact system chat blocks."
  :group 'e-chat)

(defface e-chat-composer-face
  '((t :inherit minibuffer-prompt :extend t))
  "Face used for the composer prompt chrome."
  :group 'e-chat)

(defface e-chat-separator-face
  '((t :inherit shadow :extend t))
  "Face used for the composer separator."
  :group 'e-chat)

(defface e-chat-title-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1 :extend t))
  "Face used for the chat buffer title."
  :group 'e-chat)

(defconst e-chat--user-face-spec
  '((t :inherit font-lock-string-face :extend t))
  "Default face spec for user-authored chat blocks.")

(defconst e-chat--assistant-face-spec
  '((t :inherit font-lock-function-name-face :extend t))
  "Default face spec for assistant response chat blocks.")

(defun e-chat--refresh-face-specs ()
  "Refresh chat face defaults after live reload."
  (face-spec-set 'e-chat-user-face e-chat--user-face-spec)
  (face-spec-set 'e-chat-assistant-face e-chat--assistant-face-spec))

(e-chat--refresh-face-specs)

(defvar-local e-chat-harness nil
  "Harness used by the current chat buffer.")

(defvar-local e-chat-session-id nil
  "Session id used by the current chat buffer.")

(defvar-local e-chat--transcript-end-marker nil
  "Marker at the end of the protected transcript region.")

(defvar-local e-chat--composer-start-marker nil
  "Marker at the beginning of editable composer text.")

(defvar-local e-chat--composer-spacer-marker nil
  "Marker at the beginning of the visual spacer above the composer.")

(defconst e-chat--user-glyph "▌"
  "Glyph shown before user-authored chat blocks.")

(defconst e-chat--assistant-glyph "◆"
  "Glyph shown before assistant chat blocks.")

(defconst e-chat--system-glyph "·"
  "Glyph shown before compact system chat blocks.")

(defconst e-chat--composer-glyph "❯ "
  "Glyph shown before editable e chat composer text.")

(defconst e-chat--composer-separator
  "────────────────────────────────────────────────────────────────"
  "Separator shown above the e chat composer.")

(defconst e-chat--title "E Agent Session"
  "Title shown at the top of e chat buffers.")

(defvar e-chat--test-window-body-height nil
  "Test override for the visible chat window height.")

(defvar e-chat--test-transcript-screen-lines nil
  "Test override for transcript screen-line height.")

(defconst e-chat--protected-properties
  '(read-only t
    e-chat-protected t
    front-sticky (read-only e-chat-protected field)
    rear-nonsticky (read-only e-chat-protected field)
    field e-chat-transcript)
  "Text properties applied to protected e chat presentation text.")

(defun e-chat--make-mode-map (&optional map)
  "Return MAP configured as the local keymap for `e-chat-mode'."
  (let ((map (or map (make-sparse-keymap))))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'e-chat-submit)
    (define-key map (kbd "RET") #'newline)
    (define-key map (kbd "C-c C-k") #'e-chat-abort)
    (define-key map (kbd "C-c C-r") #'e-chat-reset)
    map))

(defvar e-chat-mode-map (e-chat--make-mode-map)
  "Keymap for `e-chat-mode'.")

(setq e-chat-mode-map (e-chat--make-mode-map e-chat-mode-map))

(define-derived-mode e-chat-mode text-mode "e-chat"
  "Major mode for e chat buffers.")

(defun e-chat--disable-modal-editing ()
  "Disable local modal editing state for the chat buffer when available."
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1)))

(defun e-chat--disable-completion ()
  "Disable completion sources and completion UI in the chat composer."
  (when (fboundp 'company-mode)
    (company-mode -1))
  (when (fboundp 'corfu-mode)
    (corfu-mode -1))
  (when (fboundp 'auto-complete-mode)
    (auto-complete-mode -1))
  (setq-local completion-at-point-functions nil)
  (setq-local completion-in-region-function #'ignore)
  (setq-local company-backends nil)
  (setq-local company-idle-delay nil))

(defun e-chat--default-harness ()
  "Create the default Codex-backed chat harness."
  (let ((harness (e-openai-create-harness :provider e-openai-default-provider)))
    (e-harness-activate-layer harness (e-emacs-base-layer-create))
    harness))

(defun e-chat--ensure-session (harness session-id)
  "Ensure SESSION-ID exists in HARNESS."
  (condition-case nil
      (e-harness-create-session harness :id session-id)
    (e-session-duplicate
     nil)))

(defun e-chat--subscribe (harness buffer)
  "Subscribe BUFFER to HARNESS chat events."
  (e-harness-subscribe
   harness
   (lambda (event)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (eq e-chat-harness harness)
           (e-chat--render-event event)))))))

(defun e-chat--mark-protected (start end)
  "Mark text between START and END as protected presentation text."
  (when (< start end)
    (add-text-properties start end e-chat--protected-properties)))

(defun e-chat--insert-protected (text &optional face properties)
  "Insert TEXT as protected presentation text at point.
FACE is applied when non-nil.  PROPERTIES are added with text properties."
  (let ((start (point)))
    (insert text)
    (e-chat--mark-protected start (point))
    (when face
      (add-face-text-property start (point) face nil))
    (when properties
      (add-text-properties start (point) properties))))

(defun e-chat--composer-active-p ()
  "Return non-nil when the current buffer has an active composer."
  (and (markerp e-chat--transcript-end-marker)
       (marker-position e-chat--transcript-end-marker)
       (markerp e-chat--composer-start-marker)
       (marker-position e-chat--composer-start-marker)))

(defun e-chat--delete-composer ()
  "Delete the active composer from the current chat buffer.
Return non-nil when a composer was removed."
  (when (e-chat--composer-active-p)
    (let ((inhibit-read-only t)
          (start (marker-position (or e-chat--composer-spacer-marker
                                      e-chat--transcript-end-marker))))
      (delete-region start (point-max))
      (set-marker e-chat--transcript-end-marker nil)
      (set-marker e-chat--composer-start-marker nil)
      (when (markerp e-chat--composer-spacer-marker)
        (set-marker e-chat--composer-spacer-marker nil))
      t)))

(defun e-chat--visible-height ()
  "Return the visible body height for the current chat buffer."
  (or e-chat--test-window-body-height
      (when-let ((window (get-buffer-window (current-buffer) t)))
        (window-body-height window))))

(defun e-chat--visible-window ()
  "Return a visible window for the current chat buffer."
  (get-buffer-window (current-buffer) t))

(defun e-chat--transcript-screen-lines ()
  "Return screen lines used by transcript content before point."
  (or e-chat--test-transcript-screen-lines
      (when-let ((window (e-chat--visible-window)))
        (count-screen-lines (point-min) (point) nil window))
      (count-lines (point-min) (point))))

(defun e-chat--composer-spacer-lines ()
  "Return how many protected blank lines should precede the composer."
  (let ((height (e-chat--visible-height)))
    (if (not height)
        0
      (max 0 (- height
                (e-chat--transcript-screen-lines)
                4)))))

(defun e-chat--insert-composer ()
  "Insert an editable composer at the end of the current chat buffer."
  (e-chat--disable-completion)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (or (bobp) (bolp))
      (insert "\n"))
    (setq e-chat--composer-spacer-marker (point-marker))
    (set-marker-insertion-type e-chat--composer-spacer-marker nil)
    (let ((spacer-lines (e-chat--composer-spacer-lines)))
      (when (> spacer-lines 0)
        (e-chat--insert-protected
         (make-string spacer-lines ?\n)
         'e-chat-separator-face)))
    (setq e-chat--transcript-end-marker (point-marker))
    (set-marker-insertion-type e-chat--transcript-end-marker nil)
    (e-chat--insert-protected
     (concat e-chat--composer-separator "\n")
     'e-chat-separator-face)
    (e-chat--insert-protected
     e-chat--composer-glyph
     'e-chat-composer-face
     '(e-chat-composer t))
    (setq e-chat--composer-start-marker (point-marker))
    (set-marker-insertion-type e-chat--composer-start-marker nil)
    (goto-char e-chat--composer-start-marker)
    (e-chat--show-composer)))

(defun e-chat--ensure-composer ()
  "Ensure the current chat buffer has an active composer."
  (unless (e-chat--composer-active-p)
    (e-chat--insert-composer)))

(defun e-chat--composer-text ()
  "Return the current editable composer text."
  (unless (e-chat--composer-active-p)
    (user-error "No active e chat composer"))
  (string-trim
   (buffer-substring-no-properties e-chat--composer-start-marker
                                   (point-max))))

(defun e-chat--entry-face (title)
  "Return face for chat entry TITLE."
  (pcase title
    ("You" 'e-chat-user-face)
    ("Assistant" 'e-chat-assistant-face)
    (_ 'e-chat-system-face)))

(defun e-chat--entry-glyph (title)
  "Return glyph for chat entry TITLE."
  (pcase title
    ("You" e-chat--user-glyph)
    ("Assistant" e-chat--assistant-glyph)
    (_ e-chat--system-glyph)))

(defun e-chat--insert-entry (title content &optional ensure-composer)
  "Insert a protected chat entry with TITLE and CONTENT.
When ENSURE-COMPOSER is non-nil, recreate the composer after inserting."
  (let ((had-composer (e-chat--delete-composer)))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (or (bobp) (bolp))
        (insert "\n"))
      (e-chat--insert-protected
       (format "%s %s\n%s\n\n"
               (e-chat--entry-glyph title)
               title
               content)
       (e-chat--entry-face title)))
    (when (or ensure-composer had-composer)
      (e-chat--insert-composer))))

(defun e-chat--refresh-composer-position ()
  "Refresh composer spacer for the current visible window."
  (when (e-chat--composer-active-p)
    (let ((text (buffer-substring-no-properties e-chat--composer-start-marker
                                                (point-max))))
      (e-chat--delete-composer)
      (e-chat--insert-composer)
      (insert text)
      (e-chat--show-composer))))

(defun e-chat--show-composer ()
  "Move point and visible window focus to the composer."
  (goto-char (point-max))
  (when-let ((window (e-chat--visible-window)))
    (set-window-point window (point))
    (with-selected-window window
      (ignore-errors
        (recenter -1)))))

(defun e-chat--clear ()
  "Clear and initialize the current chat buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq e-chat--transcript-end-marker nil)
    (setq e-chat--composer-start-marker nil)
    (setq e-chat--composer-spacer-marker nil)
    (e-chat--insert-protected
     (concat e-chat--title "\n\n")
     'e-chat-title-face)
    (e-chat--insert-composer)))

(defun e-chat--set-status (status)
  "Set chat buffer STATUS."
  (setq header-line-format (format "E Chat: %s" status)))

(defun e-chat--message-entry (message)
  "Return a rendered entry for MESSAGE."
  (let ((role (plist-get message :role))
        (content (plist-get message :content)))
    (pcase role
      ('user (cons "You" content))
      ('assistant (cons "Assistant" content))
      ('tool (cons "Tool" (format "%S" content)))
      (_ (cons (format "%s" role) (format "%S" content))))))

(defun e-chat--render-event (event)
  "Render harness EVENT into the current chat buffer."
  (pcase (plist-get event :type)
    ('turn-started
     (e-chat--set-status (format "running %s" (plist-get event :turn-id))))
    ('turn-finished
     (e-chat--set-status "done")
     (e-chat--ensure-composer)
     (e-chat--refresh-composer-position))
    ('turn-failed
     (e-chat--set-status "error")
     (e-chat--insert-entry
      "System"
      (format "Turn failed: %s"
              (plist-get (plist-get event :payload) :error))
      t))
    ('turn-cancelled
     (e-chat--set-status "cancelled")
     (e-chat--insert-entry "System" "Turn cancelled" t))
    ('message-added
     (let* ((entry (e-chat--message-entry
                    (plist-get (plist-get event :payload) :message))))
       (e-chat--insert-entry (car entry) (cdr entry))))
    ('assistant-delta
     (e-chat--set-status "streaming"))
    ('tool-finished
     (e-chat--set-status "tool done"))
    ('backend-empty-output
     (e-chat--set-status "done"))
    ('session-reset
     (e-chat--set-status "idle")
     (e-chat--insert-entry "System" "Session reset" t))
    (_
     (e-chat--insert-entry "System" (format "Event: %S" event) t))))

(cl-defun e-chat-open (&key harness session-id)
  "Open and return an e chat buffer.
HARNESS and SESSION-ID are injectable for tests.  Interactive calls create a
Codex-backed harness with the emacs-base layer active."
  (interactive)
  (let* ((chat-harness (or harness (e-chat--default-harness)))
         (chat-session-id (or session-id e-chat-default-session-id))
         (buffer (get-buffer-create e-chat-buffer-name)))
    (e-chat--attach-buffer buffer chat-harness chat-session-id)
    (when (called-interactively-p 'interactive)
      (pop-to-buffer buffer))
    buffer))

(defun e-chat--attach-buffer (buffer harness session-id)
  "Attach BUFFER to HARNESS and SESSION-ID."
  (e-chat--ensure-session harness session-id)
  (with-current-buffer buffer
    (e-chat-mode)
    (e-chat--disable-modal-editing)
    (e-chat--disable-completion)
    (setq-local e-chat-harness harness)
    (setq-local e-chat-session-id session-id)
    (e-chat--clear)
    (e-chat--set-status "idle")
    (e-chat--subscribe harness buffer))
  buffer)

(defun e-chat-reload-buffers ()
  "Refresh live e chat buffers after development reload."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (derived-mode-p 'e-chat-mode)
            (let ((session-id (or e-chat-session-id
                                  e-chat-default-session-id)))
              (setq count (1+ count))
              (e-chat--attach-buffer
               buffer
               (e-chat--default-harness)
               session-id))))))
    (when (called-interactively-p 'interactive)
      (message "Refreshed %d e chat buffer%s"
               count
               (if (= count 1) "" "s")))
    count))

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
  (setq prompt (or prompt (e-chat--composer-text)))
  (when (string-empty-p prompt)
    (user-error "Prompt must not be empty"))
  (e-chat--delete-composer)
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

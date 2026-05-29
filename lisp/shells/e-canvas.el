;;; e-canvas.el --- Canvas presentation shell for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Canvas shell commands attach live buffers or files to chat sessions.  The
;; chat-session capability contributes attachment contents to model context on
;; every turn, so canvas state is current-state context rather than accumulated
;; transcript history.

;;; Code:

(require 'cl-lib)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-harness-registry)
(require 'e-shells)
(require 'e-startup)
(require 'seq)
(require 'subr-x)

(defgroup e-canvas nil
  "Canvas shell for e."
  :group 'e
  :prefix "e-canvas-")

(defcustom e-canvas-buffer-name-format "*e-canvas:%s*"
  "Format string used for new non-file-backed canvas buffers."
  :type 'string
  :group 'e-canvas)

(defcustom e-canvas-default-buffer-name "canvas"
  "Default logical name for a new non-file-backed canvas buffer."
  :type 'string
  :group 'e-canvas)

(defun e-canvas--default-harness ()
  "Return the default chat harness used by canvas commands."
  (e-chat--default-harness))

(defun e-canvas--buffer-uri (&optional buffer)
  "Return a resource URI for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if buffer-file-name
        (concat "file://" (expand-file-name buffer-file-name))
      (concat "buffer://" (buffer-name)))))

(defun e-canvas--buffer-label (&optional buffer)
  "Return a compact context label for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if buffer-file-name
        (file-name-nondirectory buffer-file-name)
      (buffer-name))))

(defun e-canvas--buffer-attachment (&optional buffer)
  "Return live context attachment metadata for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((file (and buffer-file-name (expand-file-name buffer-file-name))))
      (append
       (list :uri (e-canvas--buffer-uri (current-buffer))
             :label (e-canvas--buffer-label (current-buffer))
             :buffer-name (buffer-name))
       (when file
         (list :file file))))))

(defun e-canvas--file-attachment (file)
  "Return live context attachment metadata for FILE."
  (let ((file (expand-file-name file)))
    (list :uri (concat "file://" file)
          :label (file-name-nondirectory file)
          :file file)))

(defun e-canvas--create-session (harness canvas-buffer)
  "Create a new chat session in HARNESS for CANVAS-BUFFER."
  (with-current-buffer canvas-buffer
    (e-chat--create-session harness)))

(defun e-canvas--session-canvas-attachment (harness session-id)
  "Return SESSION-ID's primary canvas attachment in HARNESS, or nil."
  (seq-find (lambda (attachment)
              (plist-get attachment :canvas))
            (e-chat-session-attachments harness session-id)))

(defun e-canvas--session-canvas-buffer (harness session-id)
  "Return the live canvas buffer for SESSION-ID in HARNESS, or nil.
Prefer an existing buffer; otherwise visit a file-backed canvas on
demand."
  (when-let ((attachment (e-canvas--session-canvas-attachment
                          harness session-id)))
    (or (e-chat-session--attachment-live-buffer attachment)
        (when-let ((file (plist-get attachment :file)))
          (and (file-readable-p file)
               (find-file-noselect file))))))

(defun e-canvas--buffer-canvas-session (harness buffer)
  "Return a session id in HARNESS whose canvas is BUFFER, or nil.
Match on buffer identity, recorded buffer name, or canvas URI so
file-backed canvases resolve even without a recorded live buffer."
  (let ((buffer-uri (e-canvas--buffer-uri buffer))
        (buffer-name (buffer-name buffer)))
    (catch 'session
      (dolist (session (e-harness-session-list harness))
        (let* ((session-id (plist-get session :id))
               (attachment (e-canvas--session-canvas-attachment
                            harness session-id)))
          (when (and attachment
                     (or (eq (e-chat-session--attachment-live-buffer
                              attachment)
                             buffer)
                         (equal (plist-get attachment :buffer-name)
                                buffer-name)
                         (equal (plist-get attachment :uri) buffer-uri)))
            (throw 'session session-id)))))))

(defun e-canvas--display-buffer-to-side (buffer)
  "Display BUFFER in a side pane next to the current window.
Split off a new pane to the right when the frame has a single window,
otherwise reuse the adjacent window."
  (if (one-window-p)
      (pop-to-buffer buffer
                     '((display-buffer-pop-up-window)
                       (direction . right)))
    (let ((window (next-window)))
      (set-window-buffer window buffer)
      (select-window window)))
  buffer)

(defun e-canvas--display-chat-to-side (chat-buffer)
  "Display CHAT-BUFFER in a side pane next to the current window.
When the frame has a single window, split off a new pane to the
right.  Otherwise reuse the adjacent (side) window so repeated
canvas sessions share one chat pane."
  (if (one-window-p)
      (pop-to-buffer chat-buffer
                     '((display-buffer-pop-up-window)
                       (direction . right)))
    (let ((window (next-window)))
      (set-window-buffer window chat-buffer)
      (select-window window)))
  (e-chat--after-display-buffer chat-buffer)
  chat-buffer)

(defun e-canvas--open-session-for-buffer (buffer &optional display)
  "Create and open a new chat session using BUFFER as the primary canvas.
When DISPLAY is non-nil, keep BUFFER in the current pane and show the
chat buffer in a side pane."
  (let* ((harness (e-canvas--default-harness))
         (session (e-canvas--create-session harness buffer))
         (session-id (plist-get session :id))
         (attachment (e-canvas--buffer-attachment buffer)))
    (e-chat-session-attach-context harness session-id attachment :canvas t)
    (e-chat-session-rename
     harness
     session-id
     (format "Canvas: %s" (plist-get attachment :label)))
    (let ((chat-buffer (e-chat-open :harness harness :session-id session-id)))
      (when display
        (switch-to-buffer buffer)
        (e-canvas--display-chat-to-side chat-buffer))
      chat-buffer)))

(defun e-canvas--session-choice-label (session)
  "Return completion label for SESSION metadata."
  (e-chat--session-choice-label session))

(defun e-canvas--read-session (harness prompt)
  "Read a session id from HARNESS with PROMPT."
  (let* ((sessions (e-harness-session-list harness))
         (labels (mapcar #'e-canvas--session-choice-label sessions))
         (selected (completing-read prompt labels nil t))
         (index (cl-position selected labels :test #'equal)))
    (unless index
      (user-error "No e session selected"))
    (plist-get (nth index sessions) :id)))

(defun e-canvas--target-session (harness)
  "Return the most relevant session id for an attachment command."
  (cond
   ((and (derived-mode-p 'e-chat-mode) e-chat-session-id)
    e-chat-session-id)
   ((e-harness-session-list harness)
    (e-canvas--read-session harness "Attach canvas context to e session: "))
   (t
    (plist-get (e-chat--create-session harness) :id))))

(defun e-canvas--attach (harness session-id attachment &optional canvas)
  "Attach ATTACHMENT to HARNESS SESSION-ID and optionally mark it CANVAS."
  (prog1 (e-chat-session-attach-context
           harness session-id attachment :canvas canvas)
    (message "Attached %s to e session %s"
             (plist-get attachment :label)
             session-id)))

;;;###autoload
(defun e-canvas-open-for-current-buffer ()
  "Open an e session for the current buffer's canvas.
When the current buffer is already a canvas for a session, reveal that
session's chat buffer in a side pane.  Otherwise open a new session
using the current buffer as the primary canvas."
  (interactive)
  (let* ((source (current-buffer))
         (harness (e-canvas--default-harness))
         (existing (e-canvas--buffer-canvas-session harness source)))
    (if existing
        (let ((chat-buffer (e-chat-open :harness harness
                                        :session-id existing)))
          (when (called-interactively-p 'interactive)
            (e-canvas--display-chat-to-side chat-buffer))
          chat-buffer)
      (e-canvas--open-session-for-buffer
       source
       (called-interactively-p 'interactive)))))

;;;###autoload
(defun e-canvas-reveal-canvas ()
  "Reveal the canvas buffer for the current chat session in a side pane.
The current buffer must be an e chat buffer whose session has a canvas
attachment."
  (interactive)
  (unless (and (derived-mode-p 'e-chat-mode) e-chat-session-id)
    (user-error "Not in an e chat buffer"))
  (let* ((harness (or e-chat-harness (e-canvas--default-harness)))
         (buffer (e-canvas--session-canvas-buffer harness e-chat-session-id)))
    (unless buffer
      (user-error "This chat session has no canvas attached"))
    (e-canvas--display-buffer-to-side buffer)))

;;;###autoload
(defun e-canvas-new-buffer (name)
  "Create a new non-file-backed canvas buffer NAME and open an e session for it."
  (interactive
   (list (read-string "Canvas buffer name: " e-canvas-default-buffer-name)))
  (let* ((base (if (string-empty-p (string-trim name))
                   e-canvas-default-buffer-name
                 (string-trim name)))
         (buffer (generate-new-buffer
                  (format e-canvas-buffer-name-format base))))
    (with-current-buffer buffer
      (text-mode))
    (e-canvas--open-session-for-buffer
     buffer
     (called-interactively-p 'interactive))))

;;;###autoload
(defun e-canvas-new-file (file)
  "Create or visit FILE as a canvas and open a new e session for it."
  (interactive "FCanvas file: ")
  (let ((buffer (find-file-noselect file)))
    (e-canvas--open-session-for-buffer
     buffer
     (called-interactively-p 'interactive))))

;;;###autoload
(defun e-canvas-attach-current-buffer (&optional canvas)
  "Attach the current buffer to an e session as live context.
With prefix argument CANVAS, replace the target session's primary canvas."
  (interactive "P")
  (let* ((source (current-buffer))
         (harness (e-canvas--default-harness))
         (session-id (e-canvas--target-session harness)))
    (e-canvas--attach
     harness
     session-id
     (e-canvas--buffer-attachment source)
     canvas)))

;;;###autoload
(defun e-canvas-attach-file (file &optional canvas)
  "Attach FILE to an e session as live context.
With prefix argument CANVAS, replace the target session's primary canvas."
  (interactive "fAttach file to e session: \nP")
  (let* ((harness (e-canvas--default-harness))
         (session-id (e-canvas--target-session harness)))
    (e-canvas--attach
     harness
     session-id
     (e-canvas--file-attachment file)
     canvas)))

;;;###autoload
(defun e-canvas-shell ()
  "Return the canvas presentation shell manifest."
  (e-shell-create
   :id 'canvas
   :name "Canvas"
   :summary "Live buffer/file canvas context for chat sessions."
   :required-capabilities '(chat-session)
   :commands
   (list
    (e-shell-command-create
     :id 'open-for-current-buffer
     :summary "Open a new session using the current buffer as canvas."
     :interactive 'e-canvas-open-for-current-buffer
     :function 'e-canvas-open-for-current-buffer
     :scope 'global)
    (e-shell-command-create
     :id 'new-buffer
     :summary "Create a new buffer canvas and open a new session."
     :interactive 'e-canvas-new-buffer
     :function 'e-canvas-new-buffer
     :scope 'global)
    (e-shell-command-create
     :id 'new-file
     :summary "Create or visit a file canvas and open a new session."
     :interactive 'e-canvas-new-file
     :function 'e-canvas-new-file
     :scope 'global)
    (e-shell-command-create
     :id 'attach-current-buffer
     :summary "Attach the current buffer to a session's live context."
     :interactive 'e-canvas-attach-current-buffer
     :function 'e-canvas-attach-current-buffer
     :scope 'global)
    (e-shell-command-create
     :id 'attach-file
     :summary "Attach a file to a session's live context."
     :interactive 'e-canvas-attach-file
     :function 'e-canvas-attach-file
     :scope 'global)
    (e-shell-command-create
     :id 'reveal-canvas
     :summary "Reveal the current chat session's canvas in a side pane."
     :interactive 'e-canvas-reveal-canvas
     :function 'e-canvas-reveal-canvas
     :scope 'global))))

(defun e-canvas-startup ()
  "Refresh and register the canvas shell provider for package startup."
  (e-shell-register (e-canvas-shell)))

(add-hook 'e-startup-shell-hook #'e-canvas-startup)

(provide 'e-canvas)

;;; e-canvas.el ends here

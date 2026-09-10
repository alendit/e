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
(require 'e-session)
(require 'e-shells)
(require 'e-startup)
(require 'e-ui-work)
(require 'e-work)
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

(cl-defstruct (e-canvas-kind
               (:constructor e-canvas-kind-create))
  "Substitutable policy for a Canvas specialization.
The base lifecycle calls every Canvas kind through this contract; specialized
kinds may add metadata and presentation behavior, but must preserve the base
open/resume/recovery semantics."
  name
  harness-function
  prepare-buffer-function
  prepare-harness-function
  attachment-function
  session-reference-function
  initialize-session-function
  bind-session-function
  present-session-function)

(defvar-local e-canvas-session-id nil
  "Process-local session identity attached to this base Canvas buffer.")

(defvar-local e-canvas-harness nil
  "Process-local harness attached to this base Canvas buffer.")

(defun e-canvas--default-harness ()
  "Return the default chat harness used by canvas commands."
  (e-chat-default-harness))

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

(defun e-canvas--session-canvas-attachment (metadata)
  "Return the primary canvas attachment in detached session METADATA."
  (seq-find (lambda (attachment)
              (plist-get attachment :canvas))
            (e-chat-session-metadata-attachments metadata)))

(defun e-canvas--session-canvas-buffer (metadata)
  "Return the live canvas buffer described by detached METADATA, or nil.
Prefer an existing buffer; otherwise visit a file-backed canvas on
demand."
  (when-let ((attachment (e-canvas--session-canvas-attachment metadata)))
    (or (e-chat-session-attachment-live-buffer attachment)
        (when-let ((file (plist-get attachment :file)))
          (and (file-readable-p file)
               (find-file-noselect file))))))

(defun e-canvas--buffer-canvas-session (harness buffer)
  "Return BUFFER's process-local session id when owned by HARNESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (eq e-canvas-harness harness) e-canvas-session-id))))

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
  (e-chat-after-display-buffer chat-buffer)
  chat-buffer)

(defun e-canvas--identity-prepare-buffer (_buffer _options)
  "Prepare a base Canvas buffer."
  nil)

(defun e-canvas--identity-prepare-harness (harness _buffer _options)
  "Return HARNESS unchanged for a base Canvas."
  harness)

(defun e-canvas--initialize-session
    (harness session-id buffer _options)
  "Initialize base Canvas SESSION-ID in HARNESS for BUFFER."
  (e-chat-session-rename
   harness session-id (format "Canvas: %s" (e-canvas--buffer-label buffer))))

(defun e-canvas--bind-session (harness session-id buffer _options)
  "Bind a base Canvas BUFFER to live HARNESS and SESSION-ID coordination."
  (with-current-buffer buffer
    (setq-local e-canvas-harness harness)
    (setq-local e-canvas-session-id (copy-sequence session-id))))

(defun e-canvas--present-session (buffer chat-buffer display)
  "Present base Canvas BUFFER and CHAT-BUFFER when DISPLAY is non-nil."
  (when display
    (switch-to-buffer buffer)
    (e-canvas--display-chat-to-side chat-buffer))
  chat-buffer)

(defconst e-canvas--kind
  (e-canvas-kind-create
   :name "Canvas"
   :harness-function (lambda (_buffer) (e-canvas--default-harness))
   :prepare-buffer-function #'e-canvas--identity-prepare-buffer
   :prepare-harness-function #'e-canvas--identity-prepare-harness
   :attachment-function #'e-canvas--buffer-attachment
   :session-reference-function #'e-canvas--buffer-canvas-session
   :initialize-session-function #'e-canvas--initialize-session
   :bind-session-function #'e-canvas--bind-session
   :present-session-function #'e-canvas--present-session)
  "Base Canvas kind used by generic buffer and file canvases.")

(defun e-canvas--reference-current-p (kind harness buffer session-id)
  "Return non-nil when KIND still references HARNESS SESSION-ID from BUFFER."
  (and (buffer-live-p buffer)
       (equal
        (funcall (e-canvas-kind-session-reference-function kind)
                 harness buffer)
        session-id)))

(defun e-canvas--confirm-session-replacement
    (kind buffer session-id reason &optional condition)
  "Ask whether KIND's invalid SESSION-ID for BUFFER should be replaced.
REASON is `missing', `different-buffer', or `unavailable'.  CONDITION is the
original load failure for an unavailable session."
  (let* ((name (e-canvas-kind-name kind))
         (message
          (pcase reason
            ('missing
             (format "%s session %s referenced by %s could not be found"
                     name session-id (buffer-name buffer)))
            ('different-buffer
             (format "%s session %s is not a matching canvas for %s"
                     name session-id (buffer-name buffer)))
            ('unavailable
             (format "%s session %s referenced by %s could not be resumed: %s"
                     name session-id (buffer-name buffer)
                     (if condition
                         (error-message-string condition)
                       "unknown session load failure")))
            (_
             (format "%s session %s referenced by %s is invalid"
                     name session-id (buffer-name buffer))))))
    (display-warning 'e-canvas message :warning)
    (yes-or-no-p
     (format "%s session %s is not usable for %s. Start a new session and replace the reference? "
             name session-id (buffer-name buffer)))))

(defun e-canvas--bind-and-open-session
    (kind harness session-id buffer options display)
  "Bind and open KIND's HARNESS SESSION-ID for BUFFER.
OPTIONS are kind-owned creation options and DISPLAY controls presentation."
  (funcall (e-canvas-kind-bind-session-function kind)
           harness session-id buffer options)
  (let ((chat-buffer
         (e-chat-open
          :harness harness
          :session-id session-id
          :on-session-read-error
          (lambda (condition)
            (e-canvas--schedule-session-recovery
             kind harness buffer session-id condition options display)))))
    (funcall (e-canvas-kind-present-session-function kind)
             buffer chat-buffer display)
    chat-buffer))

(defun e-canvas--initialize-admitted-session
    (kind harness session-id buffer options attachment)
  "Persist KIND's dependent Canvas facts after SESSION-ID admission.
ATTACHMENT is detached before the asynchronous boundary.  The session root,
Board association, owner participant, and first input are admitted by the
chat application transaction; these later mutations begin only after its
explicit commit acknowledgement."
  (e-chat-session-attach-context
   harness session-id attachment :canvas t :current-attachments nil)
  (when (buffer-live-p buffer)
    (funcall (e-canvas-kind-initialize-session-function kind)
             harness session-id buffer options)))

(defun e-canvas--initialize-after-admission
    (kind harness session-id buffer options attachment admission-work)
  "Start dependent Canvas mutations after ADMISSION-WORK commits."
  (unless (e-work-handle-p admission-work)
    (signal 'wrong-type-argument
            (list 'e-work-handle-p admission-work)))
  (e-work-on-settle
   admission-work
   (lambda (settled)
     (when (eq (plist-get (e-work-status settled) :state) 'finished)
       (e-canvas--initialize-admitted-session
        kind harness session-id buffer options attachment)))))

(defun e-canvas--create-and-open-session
    (kind harness buffer options display)
  "Create and open KIND's Canvas session in HARNESS for BUFFER.
OPTIONS belong to KIND, and DISPLAY controls presentation."
  (let* ((session-id (e-session-generate-id))
         ;; Canvas creation is an interactive application-service operation.
         ;; Start the chat surface without waiting for SQLite admission.  The
         ;; attachment and metadata commands depend on the new durable root,
         ;; so they begin only from its explicit commit acknowledgement below.
         (chat-buffer
          (e-chat-open :harness harness :session-id session-id :new-session t))
         (attachment
          (funcall (e-canvas-kind-attachment-function kind) buffer))
         (admission-work
          (and (buffer-live-p chat-buffer)
               (buffer-local-value 'e-chat--session-readiness-work
                                   chat-buffer))))
    (funcall (e-canvas-kind-bind-session-function kind)
             harness session-id buffer options)
    (funcall (e-canvas-kind-present-session-function kind)
             buffer chat-buffer display)
    (e-canvas--initialize-after-admission
     kind harness session-id buffer options attachment admission-work)
    chat-buffer))

(defun e-canvas--recover-session-reference
    (kind harness buffer session-id reason condition options display)
  "Offer to replace KIND's invalid HARNESS SESSION-ID for BUFFER.
REASON and CONDITION describe the failed reference validation."
  (when (e-canvas--reference-current-p kind harness buffer session-id)
    (if (e-canvas--confirm-session-replacement
         kind buffer session-id reason condition)
        (e-canvas--create-and-open-session
         kind harness buffer options display)
      (message "Kept invalid %s session reference %s"
               (e-canvas-kind-name kind) session-id))))

(defun e-canvas--schedule-session-recovery
    (kind harness buffer session-id condition options display &optional reason)
  "Schedule UI recovery for KIND's unavailable HARNESS SESSION-ID for BUFFER.
CONDITION is the load failure, OPTIONS belong to KIND, and DISPLAY controls
replacement-session presentation.  REASON defaults to `unavailable'."
  (when (buffer-live-p buffer)
    (e-ui-work-schedule
     (e-ui-work-spec-create
      :id "canvas_session_recovery"
      :description "Offer recovery for an unavailable Canvas session."
      :owner 'canvas-session-recovery
      :target-buffer buffer
      :key (cons (e-canvas-kind-name kind) session-id)
      :generation session-id
      :focus-policy 'preserve
      :reentrancy-policy 'defer
      :coalesce t
      :stale-p
      (lambda (_job)
        (not (e-canvas--reference-current-p
              kind harness buffer session-id)))
      :apply
      (lambda (_job _handle)
        (e-canvas--recover-session-reference
         kind harness buffer session-id (or reason 'unavailable)
         condition options display))))))

(cl-defun e-canvas-open-buffer
    (kind buffer &key (session-id nil session-id-supplied-p)
          force-new options display)
  "Open BUFFER through substitutable Canvas KIND semantics.
SESSION-ID explicitly selects a referenced session.  Without it, KIND supplies
the buffer's reference.  FORCE-NEW bypasses reference resolution.  OPTIONS are
owned by KIND, and DISPLAY requests that KIND present the opened session."
  (unless (e-canvas-kind-p kind)
    (signal 'wrong-type-argument (list 'e-canvas-kind kind)))
  (funcall (e-canvas-kind-prepare-buffer-function kind) buffer options)
  (let* ((initial-harness
          (funcall (e-canvas-kind-harness-function kind) buffer))
         (harness
          (funcall (e-canvas-kind-prepare-harness-function kind)
                   initial-harness buffer options))
         (reference
          (unless force-new
            (if session-id-supplied-p
              session-id
              (funcall (e-canvas-kind-session-reference-function kind)
                       harness buffer))))
         ;; A live chat buffer is presentation state with an immediate use:
         ;; preserve its subscriber/composer/controller lifetime.  It is not
         ;; a durable session replica and requires no SQLite lookup.
         (live-chat
          (and reference
               (e-chat--find-session-buffer reference harness))))
    (unless (e-session-storage-sqlite-p (e-harness-sessions harness))
      (signal 'e-session-storage-error
              (list "Public Canvas requires SQLite" reference)))
    (cond
     (live-chat
      (funcall (e-canvas-kind-bind-session-function kind)
               harness reference buffer options)
      (funcall (e-canvas-kind-present-session-function kind)
               buffer live-chat display))
     (reference
      ;; Open first; the chat surface requests exact metadata, association,
      ;; and visible messages asynchronously and reports an invalid reference
      ;; through its existing recovery callback.
      (e-canvas--bind-and-open-session
       kind harness reference buffer options display))
     (t
      (e-canvas--create-and-open-session
       kind harness buffer options display)))))

(defun e-canvas--open-session-for-buffer (buffer &optional display)
  "Create and open a new chat session using BUFFER as the primary canvas.
When DISPLAY is non-nil, keep BUFFER in the current pane and show the
chat buffer in a side pane."
  (e-canvas-open-buffer
   e-canvas--kind buffer :force-new t :display display))

(defun e-canvas--session-choice-label (session)
  "Return completion label for SESSION metadata."
  (e-chat-session-choice-label session))

(defun e-canvas--read-session (sessions prompt)
  "Read a session id or a new-session choice from detached SESSIONS."
  (let* ((labels (mapcar #'e-canvas--session-choice-label sessions))
         (new-label "[New e session]")
         (choices (cons new-label labels))
         (selected (completing-read prompt choices nil t))
         (index (cl-position selected labels :test #'equal)))
    (cond
     ((equal selected new-label) nil)
     (index (plist-get (nth index sessions) :id))
     (t (user-error "No e session selected")))))

(defun e-canvas--target-session (harness candidates)
  "Return the most relevant detached HARNESS session target.

A new target preallocates its identity and opens its composer immediately.
Its durable creation remains pending until the user's first input is admitted
atomically; callers must not issue a dependent session mutation before then."
  (cond
   ((and (derived-mode-p 'e-chat-mode) e-chat-session-id)
    (list :session-id e-chat-session-id
          :metadata (copy-tree e-chat-session-metadata t)))
   (candidates
    (let* ((sessions
            (mapcar (lambda (candidate) (plist-get candidate :session))
                    candidates))
           (session-id
            (e-canvas--read-session
             sessions "Attach canvas context to e session: ")))
      (if session-id
          (let ((session (seq-find
                          (lambda (candidate)
                            (equal (plist-get candidate :id) session-id))
                          sessions)))
            (list :session-id session-id
                  :metadata (copy-tree (plist-get session :metadata) t)))
      (let* ((session-id (e-session-generate-id))
             (buffer (e-chat-open :harness harness :session-id session-id
                                  :new-session t)))
        (e-chat-surface-pop-to-buffer buffer)
        (list :session-id session-id :new-session t :buffer buffer
              :creation-work
              (buffer-local-value 'e-chat--session-readiness-work buffer))))))
   (t
    (let* ((session-id (e-session-generate-id))
           (buffer (e-chat-open :harness harness :session-id session-id
                                :new-session t)))
      (e-chat-surface-pop-to-buffer buffer)
      (list :session-id session-id :new-session t :buffer buffer
            :creation-work
            (buffer-local-value 'e-chat--session-readiness-work buffer))))))

(defun e-canvas--attach-after-query (harness attachment canvas)
  "Attach ATTACHMENT after a bounded session query for HARNESS settles."
  (if (and (derived-mode-p 'e-chat-mode) e-chat-session-id)
      (e-canvas--attach
       harness e-chat-session-id attachment canvas
       (e-chat-session-metadata-attachments e-chat-session-metadata))
    (let ((page-work (e-chat-session-candidates-start harness)))
      (e-work-on-settle
       page-work
       (lambda (settled)
         (let ((status (e-work-status settled)))
           (if (not (eq (plist-get status :state) 'finished))
               (message "Unable to query e sessions: %s"
                        (e-work-error-message
                         (or (plist-get status :error)
                             '(e-work-cancelled "cancelled"))))
             (let* ((target
                     (e-canvas--target-session
                      harness (plist-get status :result)))
                    (session-id (plist-get target :session-id)))
               (if-let ((creation-work (plist-get target :creation-work)))
                   (e-work-on-settle
                    creation-work
                    (lambda (created)
                      (when (eq (plist-get (e-work-status created) :state)
                                'finished)
                        (e-canvas--attach
                         harness session-id attachment canvas nil))))
                 (e-canvas--attach
                  harness session-id attachment canvas
                  (e-chat-session-metadata-attachments
                   (plist-get target :metadata))))))))
      page-work))))

(defun e-canvas--attach
    (harness session-id attachment &optional canvas current-attachments)
  "Attach ATTACHMENT to HARNESS SESSION-ID from detached CURRENT-ATTACHMENTS.
When CANVAS is non-nil, mark ATTACHMENT as the primary canvas."
  (prog1 (e-chat-session-attach-context
           harness session-id attachment :canvas canvas
           :current-attachments current-attachments)
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
  (e-canvas-open-buffer
   e-canvas--kind
   (current-buffer)
   :display (called-interactively-p 'interactive)))

;;;###autoload
(defun e-canvas-reveal-canvas ()
  "Reveal the canvas buffer for the current chat session in a side pane.
The current buffer must be an e chat buffer whose session has a canvas
attachment."
  (interactive)
  (unless (and (derived-mode-p 'e-chat-mode) e-chat-session-id)
    (user-error "Not in an e chat buffer"))
  (let ((buffer (e-canvas--session-canvas-buffer e-chat-session-metadata)))
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
         (harness (e-canvas--default-harness)))
    (e-canvas--attach-after-query
     harness (e-canvas--buffer-attachment source) canvas)))

;;;###autoload
(defun e-canvas-attach-file (file &optional canvas)
  "Attach FILE to an e session as live context.
With prefix argument CANVAS, replace the target session's primary canvas."
  (interactive "fAttach file to e session: \nP")
  (let ((harness (e-canvas--default-harness)))
    (e-canvas--attach-after-query
     harness (e-canvas--file-attachment file) canvas)))

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

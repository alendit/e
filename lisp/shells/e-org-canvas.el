;;; e-org-canvas.el --- Org Canvas presentation shell for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Org Canvas uses an Org buffer as the primary interaction surface and a small
;; temporary input pane for turns.  The backing conversation remains a normal e
;; chat session marked with :org-canvas session metadata.

;;; Code:

(require 'cl-lib)
(require 'e-backend)
(require 'e-canvas)
(require 'e-chat)
(require 'e-chat-session)
(require 'e-context-status)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-org-canvas-capabilities)
(require 'e-session)
(require 'e-shells)
(require 'e-startup)
(require 'e-workspaces)
(require 'org)
(require 'seq)
(require 'subr-x)

(declare-function e-annotation-tools-available-p "e-annotation-tools")
(declare-function e-annotation-tools-list "e-annotation-tools")

(defgroup e-org-canvas nil
  "Org Canvas shell for e."
  :group 'e
  :prefix "e-org-canvas-")

(defcustom e-org-canvas-buffer-name-format "*e-org-canvas:%s*"
  "Format string used for new unsaved Org Canvas buffers."
  :type 'string
  :group 'e-org-canvas)

(defcustom e-org-canvas-default-buffer-name "org-canvas"
  "Default logical name for new unsaved Org Canvas buffers."
  :type 'string
  :group 'e-org-canvas)

(defcustom e-org-canvas-input-buffer-name-format "*e-org-canvas-input:%s*"
  "Format string used for temporary Org Canvas input buffers."
  :type 'string
  :group 'e-org-canvas)

(defcustom e-org-canvas-input-auto-close-delay 1
  "Seconds to keep a successful Org Canvas prompt pane visible."
  :type 'number
  :group 'e-org-canvas)

(defcustom e-org-canvas-respond-to-threads-instructions
  (string-join
   '("Open annotation threads on this document are awaiting your response."
     "Review each thread listed below, address what it asks, and update the"
     "document where appropriate."
     "Record your reply on the thread with the annotation tools:"
     "`annotation_list` to re-read the current threads, and `annotation_resolve`"
     "to append a reply and set a verdict once a proposal is genuinely settled."
     "Do not resolve a thread whose request you have not actually handled.")
   " ")
  "Prompt preamble seeded by `e-org-canvas-respond-to-threads'."
  :type 'string
  :group 'e-org-canvas)

(defconst e-org-canvas--file-name-suggestion-instructions
  (string-join
   '("Suggest a concise file name stem for saving an Org canvas."
     "Return only lowercase words separated by hyphens."
     "Do not include a path, extension, quotes, Markdown, or explanation."
     "Keep it under 60 characters.")
   " ")
  "System instructions for model-suggested Org Canvas file names.")

(defvar e-org-canvas--project-folders (make-hash-table :test 'equal)
  "Remembered Org Canvas target folders keyed by project root.")

(defvar-local e-org-canvas-session-id nil
  "Org Canvas session associated with this Org buffer.")

(defvar-local e-org-canvas-harness nil
  "Harness associated with this Org Canvas buffer.")

(defvar-local e-org-canvas--status-subscription nil
  "Harness event subscription refreshing this buffer's context status.")

(defvar-local e-org-canvas--status-estimate-cache nil
  "Caller-owned (TOKENS . TIME) cache cell for context-token estimates.
Passed to `e-context-status-text' to reuse approximate estimates between
Org Canvas status refreshes for the current buffer.")

(defvar-local e-org-canvas-input--harness nil
  "Harness used by the current input pane.")

(defvar-local e-org-canvas-input--session-id nil
  "Session id used by the current input pane.")

(defvar-local e-org-canvas-input--scope nil
  "Prompt scope used by the current input pane.")

(defvar-local e-org-canvas-input--target-buffer nil
  "Org Canvas buffer targeted by the current input pane.")

(defvar-local e-org-canvas-input--prompt-start nil
  "Marker for the editable prompt body in the current input pane.")

(defvar-local e-org-canvas-input--status-start nil
  "Marker for the read-only status block in the current input pane.")

(defvar-local e-org-canvas-input--status-lines nil
  "Compact turn status lines displayed by the current input pane.")

(defvar-local e-org-canvas-input--subscription nil
  "Harness event subscription owned by the current input pane.")

(defvar-local e-org-canvas-input--active-turn-id nil
  "Turn id currently tracked by the current input pane.")

(defvar-local e-org-canvas-input--close-timer nil
  "Auto-close timer owned by the current input pane.")

(defvar-local e-org-canvas-input--scope-reference nil
  "Inline chat reference inserted for a thread-scoped input pane.")

(defvar-local e-org-canvas-input--submitting nil
  "Non-nil while an input pane is inside synchronous prompt submission.")

(defvar-local e-org-canvas-input--deferred-events nil
  "Harness events captured while an input pane is submitting.")

(defvar-local e-org-canvas-input--final-message-rendered-p nil
  "Non-nil once an input pane has rendered final assistant output.")

(defvar-local e-org-canvas-input--done-rendered-p nil
  "Non-nil once an input pane has rendered terminal done status.")

(defvar-local e-org-canvas-input--source-selection-buffer nil
  "Org buffer whose source selection should be cleared after this input completes.")

(defconst e-org-canvas--mode-name "Org Canvas"
  "Visible major-mode slot label used while `e-org-canvas-mode' is active.")

(defvar-local e-org-canvas--saved-mode-name nil
  "Mode name value to restore after disabling `e-org-canvas-mode'.")

(defvar-local e-org-canvas--saved-mode-name-local-p nil
  "Non-nil when `mode-name' was buffer-local before Org Canvas changed it.")

(defvar-local e-org-canvas--saved-mode-name-recorded-p nil
  "Non-nil when Org Canvas has recorded the previous `mode-name'.")

(defun e-org-canvas--make-mode-map ()
  "Return the Org Canvas minor-mode keymap."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-i") #'e-org-canvas-prompt-thread)
    (define-key map (kbd "s-I") #'e-org-canvas-prompt-document)
    (define-key map (kbd "C-c C-m") #'e-org-canvas-compact)
    map))

(defvar e-org-canvas-mode-map (e-org-canvas--make-mode-map)
  "Keymap for `e-org-canvas-mode'.")

(defun e-org-canvas--set-evil-local-prompt-bindings (enable)
  "Install or clear buffer-local Evil prompt bindings when ENABLE is non-nil."
  (when (fboundp 'evil-local-set-key)
    (funcall #'evil-local-set-key
             'normal
             (kbd "s-i")
             (and enable #'e-org-canvas-prompt-thread))
    (funcall #'evil-local-set-key
             'normal
             (kbd "s-I")
             (and enable #'e-org-canvas-prompt-document))))

(defun e-org-canvas--context-status-text ()
  "Return Org Canvas context-state status text for the current buffer."
  (unless (consp e-org-canvas--status-estimate-cache)
    (setq-local e-org-canvas--status-estimate-cache (cons nil nil)))
  (e-context-status-text
   e-org-canvas-harness e-org-canvas-session-id
   :prefix e-org-canvas--mode-name
   :estimate-cache e-org-canvas--status-estimate-cache))

(defun e-org-canvas--set-mode-name-indicator (enable)
  "Set or restore the major-mode slot indicator when ENABLE is non-nil."
  (if enable
      (progn
        (unless e-org-canvas--saved-mode-name-recorded-p
          (setq-local e-org-canvas--saved-mode-name mode-name)
          (setq-local e-org-canvas--saved-mode-name-local-p
                      (local-variable-p 'mode-name (current-buffer)))
          (setq-local e-org-canvas--saved-mode-name-recorded-p t))
        (setq-local mode-name (e-org-canvas--context-status-text))
        (force-mode-line-update))
    (when e-org-canvas--saved-mode-name-recorded-p
      (if e-org-canvas--saved-mode-name-local-p
          (setq-local mode-name e-org-canvas--saved-mode-name)
        (kill-local-variable 'mode-name))
      (kill-local-variable 'e-org-canvas--saved-mode-name)
      (kill-local-variable 'e-org-canvas--saved-mode-name-local-p)
      (kill-local-variable 'e-org-canvas--saved-mode-name-recorded-p)
      (force-mode-line-update))))

(defun e-org-canvas--refresh-status ()
  "Refresh the Org Canvas context-state indicator for the current buffer."
  (when (bound-and-true-p e-org-canvas-mode)
    (setq-local mode-name (e-org-canvas--context-status-text))
    (force-mode-line-update)))

(defun e-org-canvas--status-relevant-event-p (event)
  "Return non-nil when EVENT should refresh the Org Canvas context indicator."
  (memq (plist-get event :type)
        '(turn-started turn-finished turn-failed turn-cancelled
          token-usage session-reset
          compaction-started compaction-prepared compaction-summary-started
          compaction-finished compaction-failed)))

(defun e-org-canvas--subscribe-status (buffer harness session-id)
  "Subscribe BUFFER's context indicator to HARNESS events for SESSION-ID."
  (when (and harness session-id)
    (with-current-buffer buffer
      (e-org-canvas--unsubscribe-status)
      (setq-local
       e-org-canvas--status-subscription
       (e-harness-subscribe
        harness
        (lambda (event)
          (when (and (buffer-live-p buffer)
                     (e-org-canvas--status-relevant-event-p event))
            (with-current-buffer buffer
              (when (eq e-org-canvas-harness harness)
                (e-org-canvas--refresh-status)))))
        :session-id session-id)))))

(defun e-org-canvas--unsubscribe-status ()
  "Remove this buffer's Org Canvas context indicator subscription."
  (when (and e-org-canvas-harness e-org-canvas--status-subscription)
    (e-harness-unsubscribe e-org-canvas-harness
                           e-org-canvas--status-subscription))
  (setq-local e-org-canvas--status-subscription nil))

(defun e-org-canvas--refresh-mode-buffers ()
  "Refresh local Org Canvas state in buffers where the mode is already active."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p e-org-canvas-mode)
        (e-org-canvas--set-mode-name-indicator t)
        (e-chat-context-mode-suppress-in-current-buffer t)
        (e-org-canvas--set-evil-local-prompt-bindings t)
        (e-org-canvas--subscribe-status
         buffer e-org-canvas-harness e-org-canvas-session-id)))))

(defun e-org-canvas--make-input-mode-map ()
  "Return the Org Canvas input-mode keymap."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map e-chat-mode-map)
    (define-key map (kbd "C-c C-c") #'e-org-canvas-input-submit)
    (define-key map (kbd "C-c C-k") #'e-org-canvas-input-cancel)
    (define-key map (kbd "C-c C-s") #'e-org-canvas-input-switch-scope)
    (define-key map (kbd "C-c C-o") #'e-org-canvas-input-open-session)
    (define-key map (kbd "<escape>") #'e-org-canvas-input-cancel)
    map))

(defvar e-org-canvas-input-mode-map (e-org-canvas--make-input-mode-map)
  "Keymap for `e-org-canvas-input-mode'.")

(defun e-org-canvas--make-input-result-mode-map ()
  "Return the keymap used after an Org Canvas input has been submitted."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'e-org-canvas-input-close-result)
    map))

(defvar e-org-canvas-input-result-mode-map
  (e-org-canvas--make-input-result-mode-map)
  "Keymap for submitted Org Canvas input/result panes.")

;;;###autoload
(define-minor-mode e-org-canvas-mode
  "Buffer-local presentation state for an Org Canvas."
  :lighter " eOrgCanvas"
  :keymap e-org-canvas-mode-map
  (if e-org-canvas-mode
      (progn
        (e-org-canvas--set-mode-name-indicator t)
        (e-chat-context-mode-suppress-in-current-buffer t)
        (e-org-canvas--set-evil-local-prompt-bindings t)
        (e-org-canvas--subscribe-status
         (current-buffer) e-org-canvas-harness e-org-canvas-session-id))
    (e-org-canvas--unsubscribe-status)
    (e-org-canvas--set-evil-local-prompt-bindings nil)
    (e-chat-context-mode-suppress-in-current-buffer nil)
    (e-org-canvas--set-mode-name-indicator nil)))

;;;###autoload
(define-derived-mode e-org-canvas-input-mode e-chat-mode "e-org-canvas-input"
  "Temporary prompt pane for Org Canvas turns.")

(define-minor-mode e-org-canvas-input-result-mode
  "Result-display state for a submitted Org Canvas input pane."
  :lighter nil
  :keymap e-org-canvas-input-result-mode-map)

(defun e-org-canvas--default-harness ()
  "Return the default chat harness used by Org Canvas commands."
  (e-chat--default-harness))

(defun e-org-canvas--buffer-uri (&optional buffer)
  "Return canonical Org Canvas URI for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if buffer-file-name
        (e-org-canvas--file-uri buffer-file-name)
      (concat "buffer://" (buffer-name)))))

(defun e-org-canvas--buffer-label (&optional buffer)
  "Return compact Org Canvas label for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if buffer-file-name
        (file-name-nondirectory buffer-file-name)
      (buffer-name))))

(defun e-org-canvas--root-for-buffer (buffer)
  "Return chat-compatible project root for BUFFER."
  (with-current-buffer buffer
    (e-chat--project-root
     (if buffer-file-name
         (file-name-directory buffer-file-name)
       default-directory))))

(defun e-org-canvas--sync-default-harness-for-buffer (harness buffer)
  "Sync default chat HARNESS layers for BUFFER's project when appropriate.

Org Canvas uses a file as the active work surface, so project-local layers need
that file's project root rather than whatever directory the default chat harness
was originally created with.  Only sync registered default chat instances, so
ad-hoc test or caller-supplied harnesses keep their explicit layer state."
  (when-let* ((file (buffer-local-value 'buffer-file-name buffer))
              (instance (e-harness-instance-get e-chat-default-harness-id))
              (_default (eq (e-harness-registry-get
                             (e-harness-instance-harness-id instance))
                            harness))
              (root (with-current-buffer buffer
                      (e-chat--project-root (file-name-directory file)))))
    (e-default-chat-sync-harness-layers harness nil root))
  harness)

(cl-defun e-org-canvas--metadata-for-buffer
    (buffer &key scope focus target-folder needs-file-name)
  "Return Org Canvas metadata for BUFFER."
  (with-current-buffer buffer
    (let ((root (e-org-canvas--root-for-buffer buffer)))
      (list :uri (e-org-canvas--buffer-uri buffer)
            :buffer-name (buffer-name buffer)
            :label (e-org-canvas--buffer-label buffer)
            :mode 'org
            :root root
            :last-scope scope
            :last-focus focus
            :needs-file-name needs-file-name
            :target-folder target-folder))))

(defun e-org-canvas--set-session-metadata
    (harness session-id org-canvas-metadata)
  "Persist ORG-CANVAS-METADATA on HARNESS SESSION-ID."
  (let* ((session (e-session-get (e-harness-sessions harness) session-id))
         (metadata (copy-sequence (plist-get session :metadata))))
    (setq metadata (plist-put metadata :org-canvas org-canvas-metadata))
    (when (plist-get org-canvas-metadata :root)
      (setq metadata (plist-put metadata
                                :project-root
                                (plist-get org-canvas-metadata :root))))
    (e-session-set-metadata (e-harness-sessions harness) session-id metadata)
    metadata))

(cl-defun e-org-canvas--mark-session
    (harness session-id buffer &key scope target-folder needs-file-name focus)
  "Mark HARNESS SESSION-ID as an Org Canvas session for BUFFER."
  (let* ((focus (or focus
                    (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (e-org-canvas-capture-focus
                            (or scope 'thread))))))
         (org-canvas (e-org-canvas--metadata-for-buffer
                      buffer
                      :scope scope
                      :focus focus
                      :target-folder target-folder
                      :needs-file-name needs-file-name))
         (attachment (append (e-canvas--buffer-attachment buffer)
                             (list :mode 'org))))
    (e-chat-session-attach-context harness session-id attachment :canvas t)
    (e-org-canvas--set-session-metadata harness session-id org-canvas)
    (e-chat-session-rename
     harness session-id
     (format "Org Canvas: %s" (plist-get org-canvas :label)))
    (with-current-buffer buffer
      (setq-local e-org-canvas-harness harness)
      (setq-local e-org-canvas-session-id session-id)
      (e-org-canvas-mode 1))
    org-canvas))

(defun e-org-canvas--session-uri (session)
  "Return Org Canvas URI for SESSION metadata."
  (plist-get (plist-get session :metadata) :org-canvas))

(defun e-org-canvas--session-canvas (session)
  "Return SESSION's Org Canvas metadata, or nil."
  (plist-get (plist-get session :metadata) :org-canvas))

(defun e-org-canvas--all-sessions (harness)
  "Return full session records for HARNESS."
  (let ((store (e-harness-sessions harness)))
    (mapcar (lambda (session)
              (e-session-get store (plist-get session :id)))
            (e-harness-session-list harness))))

(defun e-org-canvas--normalize-directory (directory)
  "Return normalized DIRECTORY."
  (file-name-as-directory (expand-file-name directory)))

(defun e-org-canvas--session-matches-file-p (session file)
  "Return non-nil when SESSION belongs to FILE."
  (let* ((canvas (e-org-canvas--session-canvas session))
         (uri (and file (e-org-canvas--file-uri file))))
    (and canvas uri (equal (plist-get canvas :uri) uri))))

(defun e-org-canvas--session-matches-project-p (session project-root)
  "Return non-nil when SESSION belongs under PROJECT-ROOT."
  (let* ((canvas (e-org-canvas--session-canvas session))
         (root (and canvas (plist-get canvas :root))))
    (and canvas
         project-root
         root
         (file-in-directory-p
          (directory-file-name root)
          (e-org-canvas--normalize-directory project-root)))))

(cl-defun e-org-canvas--session-candidates
    (harness &key file project-root)
  "Return Org Canvas sessions in HARNESS filtered by FILE or PROJECT-ROOT."
   (seq-filter
    (lambda (session)
      (and (e-org-canvas--session-canvas session)
          (or (not file)
              (e-org-canvas--session-matches-file-p session file))
          (or (not project-root)
              (e-org-canvas--session-matches-project-p
               session project-root))))
     (e-org-canvas--all-sessions harness)))

(cl-defun e-org-canvas--sessions-by-file (harness &key project-root)
  "Return Org Canvas sessions grouped by canvas URI."
  (let (groups)
    (dolist (session (e-org-canvas--session-candidates
                      harness :project-root project-root))
      (let* ((canvas (e-org-canvas--session-canvas session))
             (uri (plist-get canvas :uri))
             (cell (assoc uri groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list session)))
          (push (cons uri (list session)) groups))))
    (nreverse groups)))

(defun e-org-canvas--session-or-nil (harness session-id)
  "Return SESSION-ID from HARNESS, or nil when it is missing."
  (and session-id
       (condition-case nil
           (e-session-get (e-harness-sessions harness) session-id)
         (e-session-missing nil))))

(defun e-org-canvas--session-matches-buffer-p (session buffer)
  "Return non-nil when SESSION's Org Canvas metadata belongs to BUFFER."
  (let ((canvas (and session (e-org-canvas--session-canvas session))))
    (and canvas
         (equal (plist-get canvas :uri)
                (e-org-canvas--buffer-uri buffer)))))

(defun e-org-canvas--buffer-session (harness buffer)
  "Return BUFFER's matching referenced Org Canvas session id in HARNESS, or nil."
  (with-current-buffer buffer
    (and e-org-canvas-session-id
         (e-org-canvas--session-matches-buffer-p
          (e-org-canvas--session-or-nil harness e-org-canvas-session-id)
          buffer)
         e-org-canvas-session-id)))

(defun e-org-canvas--buffer-referenced-session (buffer)
  "Return BUFFER's explicitly referenced Org Canvas session id, or nil."
  (with-current-buffer buffer
    e-org-canvas-session-id))

(defun e-org-canvas--buffer-harness (buffer default-harness)
  "Return BUFFER's referenced Org Canvas harness or DEFAULT-HARNESS."
  (with-current-buffer buffer
    (or e-org-canvas-harness default-harness)))

(defun e-org-canvas--confirm-session-replacement (buffer session-id reason)
  "Ask whether invalid SESSION-ID for BUFFER should be replaced.
REASON is either `missing' or `different-buffer'."
  (let ((message
         (pcase reason
           ('missing
            (format "Org Canvas session %s referenced by %s could not be found"
                    session-id
                    (buffer-name buffer)))
           ('different-buffer
            (format "Org Canvas session %s referenced by %s belongs to a different Org buffer"
                    session-id
                    (buffer-name buffer)))
           (_
            (format "Org Canvas session %s referenced by %s is invalid"
                    session-id
                    (buffer-name buffer))))))
    (display-warning 'e-org-canvas message :warning))
  (unless (yes-or-no-p
           (format "Org Canvas session %s is not valid for %s. Start a new session? "
                   session-id
                   (buffer-name buffer)))
    (user-error "Org Canvas session %s is not valid for this buffer" session-id)))

(defun e-org-canvas--ensure-org-buffer (buffer)
  "Signal unless BUFFER is an Org buffer."
  (with-current-buffer buffer
    (unless (derived-mode-p 'org-mode)
      (user-error "Org Canvas requires an Org buffer"))))

(defun e-org-canvas--select-org-buffer (buffer)
  "Display BUFFER as the selected Org Canvas working buffer.
Selecting from a side window (e.g. a Doom popup) must not split it, so route
the display to a normal window when the selected window is a side window."
  (when (buffer-live-p buffer)
    (cond
     ((get-buffer-window buffer t)
      (select-window (get-buffer-window buffer t)))
     ((e-chat--side-window-p)
      (when-let ((window (e-chat--display-from-side-window buffer)))
        (select-window window)))
     (t (e-workspace-switch-to-buffer
         buffer
         :workspace (or (e-buffer-workspace buffer)
                        (e-workspace-current))))))
  buffer)

(defun e-org-canvas--display-chat-buffer (buffer)
  "Display Org Canvas backing chat BUFFER and keep chat invariants current."
  (when (buffer-live-p buffer)
    (let ((window (if (e-chat--side-window-p)
                      (e-chat--display-from-side-window buffer)
                    (e-workspace-display-buffer
                     buffer
                     :workspace (or (e-buffer-workspace buffer)
                                    (e-workspace-current))
                     :action '(display-buffer-at-bottom)))))
      (when (window-live-p window)
        (e-chat--after-display-buffer buffer))
      window)))

(defun e-org-canvas--open-session-for-buffer
    (buffer &optional needs-file-name target-folder)
  "Create and open an Org Canvas session for BUFFER."
  (e-org-canvas--ensure-org-buffer buffer)
  (let* ((workspace (or (e-buffer-workspace buffer)
                        (e-workspace-current)))
         (harness (e-org-canvas--sync-default-harness-for-buffer
                   (e-org-canvas--default-harness)
                   buffer))
         (session (e-chat--create-session harness))
         (session-id (plist-get session :id)))
    (e-buffer-set-workspace buffer workspace)
    (e-org-canvas--mark-session
     harness session-id buffer
     :scope 'thread
     :target-folder target-folder
     :needs-file-name needs-file-name)
    (message "Org Canvas enabled for %s; use s-i to prompt the current topic"
             (buffer-name buffer))
    (let ((chat-buffer (e-chat-open :harness harness :session-id session-id)))
      (e-buffer-set-workspace chat-buffer workspace)
      chat-buffer)))

(defun e-org-canvas--open-session-for-buffer-and-display
    (buffer &optional needs-file-name target-folder)
  "Create an Org Canvas session for BUFFER, display BUFFER, and return chat."
  (prog1 (let ((chat-buffer (e-org-canvas--open-session-for-buffer
                             buffer
                             needs-file-name
                             target-folder)))
           (e-org-canvas--display-chat-buffer chat-buffer)
           chat-buffer)
    (e-org-canvas--select-org-buffer buffer)))

;;;###autoload
(defun e-org-canvas-open-for-current-buffer ()
  "Start or reuse an Org Canvas session for the current Org buffer."
  (interactive)
  (let* ((source (current-buffer))
         (default-harness (e-org-canvas--default-harness))
         (harness (e-org-canvas--buffer-harness source default-harness))
         (referenced (e-org-canvas--buffer-referenced-session source))
         (referenced-session (e-org-canvas--session-or-nil harness referenced))
         (existing (progn
                     (e-org-canvas--ensure-org-buffer source)
                     (and referenced-session
                          (e-org-canvas--session-matches-buffer-p
                           referenced-session source)
                          referenced))))
    (if existing
        (prog1
            (progn
              (e-org-canvas--sync-default-harness-for-buffer harness source)
              (with-current-buffer source
                (setq-local e-org-canvas-harness harness)
                (setq-local e-org-canvas-session-id existing)
                (e-org-canvas-mode 1))
              (message "Org Canvas resumed for %s; use s-i to prompt the current topic"
                       (buffer-name source))
              (let ((chat-buffer (e-chat-open :harness harness :session-id existing)))
                (e-org-canvas--display-chat-buffer chat-buffer)
                chat-buffer))
          (e-org-canvas--select-org-buffer source))
      (when referenced
        (e-org-canvas--confirm-session-replacement
         source referenced
         (if referenced-session 'different-buffer 'missing)))
      (e-org-canvas--open-session-for-buffer-and-display source))))

;;;###autoload
(defun e-org-canvas-new-file (file)
  "Create or visit Org FILE and start an Org Canvas session for it."
  (interactive "FOrg Canvas file: ")
  (let ((file (expand-file-name file)))
    (if (file-directory-p file)
        (e-org-canvas-new-buffer file)
      (let ((buffer (find-file-noselect file)))
        (with-current-buffer buffer
          (org-mode))
        (e-org-canvas--open-session-for-buffer-and-display buffer)))))

(defun e-org-canvas--project-folder-default (project-root)
  "Return remembered Org Canvas folder for PROJECT-ROOT or PROJECT-ROOT."
  (or (gethash (e-org-canvas--normalize-directory project-root)
               e-org-canvas--project-folders)
      (e-org-canvas--normalize-directory project-root)))

(defun e-org-canvas--remember-project-folder (project-root folder)
  "Remember FOLDER as Org Canvas target folder for PROJECT-ROOT."
  (puthash (e-org-canvas--normalize-directory project-root)
           (e-org-canvas--normalize-directory folder)
           e-org-canvas--project-folders))

(defun e-org-canvas--read-target-folder (project-root)
  "Read a target folder for PROJECT-ROOT."
  (let ((folder (read-directory-name
                 "Org Canvas folder: "
                 (e-org-canvas--project-folder-default project-root)
                 nil
                 t)))
    (e-org-canvas--remember-project-folder project-root folder)
    (e-org-canvas--normalize-directory folder)))

;;;###autoload
(defun e-org-canvas-new-buffer (folder)
  "Create a new unsaved Org Canvas buffer targeting FOLDER."
  (interactive
   (let ((root (e-chat--project-root default-directory)))
     (list (e-org-canvas--read-target-folder root))))
  (let* ((root (e-chat--project-root folder))
         (folder (e-org-canvas--normalize-directory folder))
         (base e-org-canvas-default-buffer-name)
         (buffer (generate-new-buffer
                  (format e-org-canvas-buffer-name-format base))))
    (e-org-canvas--remember-project-folder root folder)
    (with-current-buffer buffer
      (setq-local default-directory folder)
      (org-mode))
    (e-org-canvas--open-session-for-buffer-and-display buffer t folder)))

(defun e-org-canvas--slugify (text)
  "Return a filesystem-safe slug for TEXT."
  (let* ((slug (downcase (string-trim (or text ""))))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
         (slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug)))
    slug))

(defun e-org-canvas--safe-suggested-file (folder suggestion)
  "Return safe target file in FOLDER for SUGGESTION, or nil."
  (let ((raw (string-trim (or suggestion ""))))
    (unless (or (string-empty-p raw)
                (file-name-absolute-p raw)
                (string-match-p "[/\\]" raw)
                (string-match-p "\\`\\.\\.?\\'" raw)
                (string-match-p "\\.\\." raw))
      (let* ((stem (if (equal (downcase (or (file-name-extension raw) ""))
                              "org")
                       (file-name-sans-extension raw)
                     raw))
             (slug (e-org-canvas--slugify stem)))
        (unless (string-empty-p slug)
          (let ((file (expand-file-name (concat slug ".org") folder)))
            (unless (file-exists-p file)
              file)))))))

(defun e-org-canvas--manual-target-file (folder file)
  "Return validated manual Org Canvas FILE inside FOLDER.
Signal `user-error' when FILE is outside FOLDER, not a direct child, or not an
Org file."
  (let* ((folder (e-org-canvas--normalize-directory folder))
         (file (expand-file-name file folder))
         (parent (file-name-as-directory (file-name-directory file)))
         (basename (file-name-nondirectory file)))
    (unless (equal parent folder)
      (user-error "Org Canvas file must be directly inside %s" folder))
    (when (or (string-empty-p basename)
              (member basename '("." "..")))
      (user-error "Org Canvas file name must not be empty"))
    (unless (equal (downcase (or (file-name-extension basename) "")) "org")
      (user-error "Org Canvas file must use a .org extension"))
    file))

(defun e-org-canvas--fallback-file-name-suggestion (buffer)
  "Return a local fallback file name suggestion from BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (or (when (re-search-forward org-heading-regexp nil t)
            (org-get-heading t t t t))
          e-org-canvas-default-buffer-name))))

(defun e-org-canvas--file-name-suggestion-context (prompt buffer)
  "Return user message content for file name suggestion from PROMPT and BUFFER."
  (let ((excerpt
         (with-current-buffer buffer
           (string-trim
            (buffer-substring-no-properties
             (point-min)
             (min (point-max) (+ (point-min) 2000)))))))
    (string-join
     (list
      "Suggest a short file name for this Org canvas."
      ""
      "First prompt:"
      (or prompt "")
      ""
      "Current Org canvas excerpt:"
      (if (string-empty-p excerpt) "(empty)" excerpt))
     "\n")))

(defun e-org-canvas--clean-file-name-suggestion (suggestion)
  "Return cleaned file name SUGGESTION text."
  (when (stringp suggestion)
    (let* ((line (car (split-string (string-trim suggestion) "\n" t)))
           (line (string-trim (or line "") "[`'\"[:space:]]+" "[`'\"[:space:]]+")))
      (unless (string-empty-p line)
        line))))

(defun e-org-canvas--file-name-suggestion-options (harness session-id)
  "Return backend options for a file name suggestion in HARNESS SESSION-ID."
  (let ((options (copy-sequence (e-harness-turn-options harness session-id))))
    (cl-remf options :tools)
    options))

(defun e-org-canvas--suggest-file-name (harness session-id prompt buffer)
  "Ask HARNESS backend to suggest a file name for PROMPT and BUFFER."
  (let (message parts)
    (condition-case nil
        (e-backend-stream
         (e-harness-backend harness)
         :messages
         (list
          (list :role 'system
                :content e-org-canvas--file-name-suggestion-instructions)
          (list :role 'user
                :content
                (e-org-canvas--file-name-suggestion-context prompt buffer)))
         :options (e-org-canvas--file-name-suggestion-options harness session-id)
         :on-item
         (lambda (item)
           (pcase (plist-get item :type)
             ('assistant-message
              (setq message (plist-get item :content)))
             ('assistant-delta
              (push (or (plist-get item :content) "") parts)))))
      (error nil))
    (or (e-org-canvas--clean-file-name-suggestion
         (or message
             (and parts (string-join (nreverse parts) ""))))
        (e-org-canvas--fallback-file-name-suggestion buffer))))

(defun e-org-canvas--maybe-save-new-buffer (harness session-id prompt)
  "Save a new unsaved Org Canvas before first PROMPT when safe."
  (let* ((canvas (e-org-canvas-session-metadata harness session-id))
         (buffer (e-org-canvas-session-buffer harness session-id)))
    (when (and canvas
               (plist-get canvas :needs-file-name)
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (unless buffer-file-name
          (let* ((folder (or (plist-get canvas :target-folder)
                             default-directory))
                 (suggestion (e-org-canvas--suggest-file-name
                              harness session-id prompt buffer))
                 (file (e-org-canvas--safe-suggested-file
                        folder suggestion)))
            (unless file
              (setq file
                    (e-org-canvas--manual-target-file
                     folder
                     (read-file-name
                      "Save Org Canvas as: "
                      folder
                      nil
                      nil
                      (and suggestion
                           (concat (e-org-canvas--slugify suggestion)
                                   ".org"))))))
            (when (or (not (file-exists-p file))
                      (yes-or-no-p
                       (format "Overwrite %s? " file)))
              (let ((coding-system-for-write 'utf-8-unix)
                    (select-safe-coding-system-function nil))
                (write-file file nil))))))
      (when (buffer-file-name buffer)
        (e-org-canvas--mark-session
         harness session-id buffer
         :scope (plist-get canvas :last-scope)
         :target-folder (plist-get canvas :target-folder)
         :needs-file-name nil
         :focus (plist-get canvas :last-focus))))))

(defun e-org-canvas--update-last-turn-metadata
    (harness session-id scope focus)
  "Update HARNESS SESSION-ID Org Canvas metadata with SCOPE and FOCUS."
  (let ((buffer (e-org-canvas-session-buffer harness session-id)))
    (when (buffer-live-p buffer)
      (e-org-canvas--mark-session
       harness session-id buffer
       :scope scope
       :target-folder (plist-get
                       (e-org-canvas-session-metadata harness session-id)
                       :target-folder)
       :needs-file-name (plist-get
                         (e-org-canvas-session-metadata harness session-id)
                         :needs-file-name)
       :focus focus))))

(defun e-org-canvas--input-status-text ()
  "Return the current input pane status block."
  (concat
   (mapconcat #'identity
              (or e-org-canvas-input--status-lines
                  '("Status: Ready"))
              "\n")
   "\n\nPrompt:\n"))

(defun e-org-canvas--input-replace-status ()
  "Refresh the current input pane status block."
  (when (and (markerp e-org-canvas-input--status-start)
             (markerp e-org-canvas-input--prompt-start))
    (let ((inhibit-read-only t))
      (save-excursion
        (delete-region e-org-canvas-input--status-start
                       e-org-canvas-input--prompt-start)
        (goto-char e-org-canvas-input--status-start)
        (insert (e-org-canvas--input-status-text))))))

(defun e-org-canvas--input-record-status (buffer line)
  "Append LINE to BUFFER's compact Org Canvas input status block."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local e-org-canvas-input--status-lines
                  (append e-org-canvas-input--status-lines (list line)))
      (when (> (length e-org-canvas-input--status-lines) 12)
        (setq-local e-org-canvas-input--status-lines
                    (last e-org-canvas-input--status-lines 12)))
      (e-org-canvas--input-replace-status))))

(defun e-org-canvas--input-tool-name (payload)
  "Return a compact tool name from event PAYLOAD."
  (or (plist-get payload :name)
      (plist-get (plist-get payload :tool-call) :name)
      "tool"))

(defun e-org-canvas--input-cleanup ()
  "Release resources owned by the current Org Canvas input pane."
  (when e-org-canvas-input--close-timer
    (cancel-timer e-org-canvas-input--close-timer)
    (setq-local e-org-canvas-input--close-timer nil))
  (when (and e-org-canvas-input--harness e-org-canvas-input--subscription)
    (e-harness-unsubscribe
     e-org-canvas-input--harness e-org-canvas-input--subscription)
    (setq-local e-org-canvas-input--subscription nil)))

(defun e-org-canvas--input-close-buffer (buffer)
  "Close Org Canvas input/result BUFFER when it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (e-org-canvas-input-close-result))))

(defun e-org-canvas--input-schedule-auto-close (buffer)
  "Schedule successful Org Canvas input/result BUFFER to close."
  (when (and (buffer-live-p buffer)
             (numberp e-org-canvas-input-auto-close-delay))
    (with-current-buffer buffer
      (when e-org-canvas-input--close-timer
        (cancel-timer e-org-canvas-input--close-timer))
      (setq-local e-org-canvas-input--close-timer
                  (run-at-time e-org-canvas-input-auto-close-delay
                               nil
                               #'e-org-canvas--input-close-buffer
                               buffer)))))

(defun e-org-canvas--input-clear-source-selection ()
  "Clear the source region that opened the current Org Canvas input pane."
  (when (buffer-live-p e-org-canvas-input--source-selection-buffer)
    (with-current-buffer e-org-canvas-input--source-selection-buffer
      (when mark-active
        (deactivate-mark t))))
  (setq-local e-org-canvas-input--source-selection-buffer nil))

(defun e-org-canvas--input-focus-target ()
  "Move focus to the Org Canvas buffer targeted by the current input pane."
  (when (buffer-live-p e-org-canvas-input--target-buffer)
    (if-let ((window (get-buffer-window e-org-canvas-input--target-buffer t)))
        (select-window window)
      (e-workspace-pop-to-buffer
       e-org-canvas-input--target-buffer
       :workspace (or (e-buffer-workspace e-org-canvas-input--target-buffer)
                      (e-buffer-workspace (current-buffer))
                      (e-workspace-current))))))

(defun e-org-canvas--hide-visible-backing-chat-buffers
    (session-id target-buffer)
  "Hide visible backing chat buffers for SESSION-ID before showing Org Canvas UI."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'e-chat-mode)
                   (not (derived-mode-p 'e-org-canvas-input-mode))
                   (equal e-chat-session-id session-id))
          (dolist (window (get-buffer-window-list buffer nil t))
            (cond
             ((and (buffer-live-p target-buffer)
                   (not (get-buffer-window target-buffer t)))
              (set-window-buffer window target-buffer)
              (select-window window))
             ((> (length (window-list (window-frame window) 'no-minibuf)) 1)
              (delete-window window))
             ((buffer-live-p target-buffer)
              (set-window-buffer window target-buffer)
              (select-window window)))))))))

(defun e-org-canvas--input-display-window (buffer)
  "Display input/result BUFFER below its target canvas window.

When two canvases are visible side by side, the composer must occupy only
the horizontal span of its own canvas window.  Reuse an existing window for
BUFFER when one exists, otherwise split the window showing
`e-org-canvas-input--target-buffer' so the composer sits directly below that
canvas.  Fall back to a frame-wide bottom window only when the target canvas
has no visible window."
  (when (buffer-live-p buffer)
    (or (get-buffer-window buffer t)
        (let* ((target (buffer-local-value
                        'e-org-canvas-input--target-buffer buffer))
               (target-window (and (buffer-live-p target)
                                   (get-buffer-window target t))))
          (if (window-live-p target-window)
              (with-selected-window target-window
                (display-buffer
                 buffer
                 '((display-buffer-below-selected)
                   (window-height . 8)
                   (inhibit-same-window . t))))
            (e-workspace-display-buffer
             buffer
             :workspace (or (e-buffer-workspace buffer)
                            (and (buffer-live-p target)
                                 (e-buffer-workspace target))
                            (e-workspace-current))
             :action '(display-buffer-at-bottom)))))))

(defun e-org-canvas--input-select-result-buffer (buffer)
  "Display and select submitted input/result BUFFER."
  (when (buffer-live-p buffer)
    (let ((window (e-org-canvas--input-display-window buffer)))
      (when (window-live-p window)
        (select-window window)))))

(defun e-org-canvas--input-follow-bottom (buffer)
  "Keep visible Org Canvas input/result windows for BUFFER at the bottom."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((bottom
             (save-excursion
               (goto-char (point-max))
               (skip-chars-backward "\n")
               (line-beginning-position))))
        (save-selected-window
          (dolist (window (get-buffer-window-list buffer nil t))
            (when (window-live-p window)
              (set-window-point window bottom)
              (with-selected-window window
                (goto-char bottom)
                (ignore-errors
                  (recenter -1))))))))))

(defun e-org-canvas--input-follow-bottom-on-redraw (&optional _turn-id)
  "Keep this input pane pinned to the bottom after a running-status redraw.
Registered on `e-chat--running-status-rendered-hook' so timer-driven
progress redraws follow output instead of staying pinned at the top."
  (e-org-canvas--input-follow-bottom (current-buffer)))

(defun e-org-canvas--input-enter-result-state ()
  "Switch the current input pane from editable composer to result display."
  (setq-local e-chat--composer-restore-inhibited t)
  (e-chat--delete-composer)
  (e-org-canvas-input-result-mode 1))

(defun e-org-canvas--input-clear-progress (turn-id)
  "Clear active progress presentation for TURN-ID in the current input pane."
  (e-chat--cancel-pending-activity-redraw turn-id)
  (e-chat--cancel-progress-timer)
  (setq-local e-chat--progress-turn-id nil)
  (setq-local e-chat--progress-frame 0)
  (setq-local e-chat--progress-next-tick-time nil)
  (e-chat--delete-running-status (e-chat--existing-turn-record turn-id))
  (e-chat--delete-composer))

(defun e-org-canvas--input-show-done (turn-id)
  "Replace active progress with a terminal done line for TURN-ID."
  (unless e-org-canvas-input--done-rendered-p
    (e-org-canvas--input-clear-progress turn-id)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (or (bobp) (bolp))
        (insert "\n"))
      (e-chat--insert-protected
       "✓ Done"
       'e-chat-system-face
       `(e-chat-turn-id ,turn-id)))
    (setq-local e-org-canvas-input--done-rendered-p t)))

(defun e-org-canvas--input-render-event (buffer event)
  "Render harness EVENT into submitted Org Canvas input BUFFER."
  (let ((type (plist-get event :type))
        (turn-id (plist-get event :turn-id)))
    (pcase type
      ('message-added
       (let ((message (plist-get (plist-get event :payload) :message)))
         (when (eq (plist-get message :role) 'assistant)
           (setq-local e-org-canvas-input--final-message-rendered-p t)
           (e-org-canvas--input-enter-result-state)
           (e-chat--render-event event)
           (e-org-canvas-input-result-mode 1)
           (e-org-canvas--input-select-result-buffer buffer))))
      ('turn-finished
       (e-org-canvas--input-cleanup)
       (e-org-canvas--input-clear-source-selection)
       (if e-org-canvas-input--final-message-rendered-p
           (progn
             (e-org-canvas--input-clear-progress turn-id)
             (e-org-canvas--input-select-result-buffer buffer)
             (e-org-canvas--input-schedule-auto-close buffer))
         (e-org-canvas--input-show-done turn-id)
         (e-org-canvas--input-focus-target)
         (e-org-canvas--input-schedule-auto-close buffer)))
      ('backend-empty-output
       (e-org-canvas--input-cleanup)
       (e-org-canvas--input-clear-source-selection)
       (e-org-canvas--input-show-done turn-id)
       (e-org-canvas--input-focus-target)
       (e-org-canvas--input-schedule-auto-close buffer))
      ((or 'turn-failed 'turn-cancelled)
       (e-org-canvas--input-cleanup)
       (e-org-canvas--input-clear-source-selection)
       (e-org-canvas--input-enter-result-state)
       (e-chat--render-event event)
       (e-chat--delete-composer)
       (e-org-canvas--input-select-result-buffer buffer))
      ((or 'turn-started 'provider-request-started 'provider-request-finished
           'assistant-delta 'reasoning-delta 'tool-started 'tool-finished
           'token-usage)
       (e-org-canvas--input-enter-result-state)
       (e-chat--render-event event)
       (e-chat--run-pending-activity-redraw)
       (e-org-canvas--input-follow-bottom buffer)))))

(defun e-org-canvas--input-handle-event (buffer event)
  "Render BUFFER updates for its active Org Canvas turn."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if e-org-canvas-input--submitting
          (push event e-org-canvas-input--deferred-events)
        (let ((turn-id (plist-get event :turn-id)))
          (when (and turn-id
                     (or (null e-org-canvas-input--active-turn-id)
                         (equal e-org-canvas-input--active-turn-id turn-id)))
            (unless e-org-canvas-input--active-turn-id
              (setq-local e-org-canvas-input--active-turn-id turn-id))
            (e-org-canvas--input-render-event buffer event)))))))

(defun e-org-canvas--input-subscribe (buffer harness session-id)
  "Subscribe BUFFER to HARNESS events for SESSION-ID."
  (when (and harness session-id)
    (with-current-buffer buffer
      (setq-local
       e-org-canvas-input--subscription
       (e-harness-subscribe
        harness
        (lambda (event)
          (e-org-canvas--input-handle-event buffer event))
        :session-id session-id)))))

(defun e-org-canvas--input-reset-chat-state ()
  "Reset chat-local presentation state for a transient Org Canvas input pane."
  (setq-local e-chat--transcript-end-marker nil)
  (setq-local e-chat--composer-start-marker nil)
  (setq-local e-chat--composer-spacer-marker nil)
  (setq-local e-chat--composer-scroll-needed nil)
  (setq-local e-chat--turn-registry (make-hash-table :test 'equal))
  (setq-local e-chat--block-registry (make-hash-table :test 'equal))
  (setq-local e-chat--block-order nil)
  (setq-local e-chat--block-counter 0)
  (setq-local e-chat--context-reference-counter 0)
  (setq-local e-chat--focused-turn-id nil)
  (setq-local e-chat--focused-block-id nil)
  (setq-local e-chat--latest-final-block-id nil)
  (setq-local e-chat--last-rendered-turn-id nil)
  (setq-local e-chat--last-rendered-side nil)
  (setq-local e-chat--block-view-block-id nil)
  (setq-local e-chat--tool-list-block-id nil)
  (setq-local e-chat--tool-list-index 0)
  (setq-local e-chat--composer-restore-inhibited nil)
  (setq-local e-chat--status nil))

(defun e-org-canvas--input-replay-deferred-events (buffer)
  "Replay events captured while BUFFER was submitting."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((events (nreverse e-org-canvas-input--deferred-events)))
        (setq-local e-org-canvas-input--deferred-events nil)
        (dolist (event events)
          (e-org-canvas--input-handle-event buffer event))))))

(defun e-org-canvas--input-thread-reference (target-buffer)
  "Return a chat-style source reference for TARGET-BUFFER."
  (when (buffer-live-p target-buffer)
    (with-current-buffer target-buffer
      (e-chat-capture-source-reference))))

(defun e-org-canvas--input-insert-scope-reference ()
  "Insert the thread-scope cursor reference into the current input pane."
  (when (and (eq e-org-canvas-input--scope 'thread)
             (null e-org-canvas-input--scope-reference))
    (when-let ((reference
                (e-org-canvas--input-thread-reference
                 e-org-canvas-input--target-buffer)))
      (goto-char (or e-chat--composer-start-marker (point-max)))
      (setq-local e-org-canvas-input--scope-reference
                  (e-chat--insert-context-reference reference))
      (insert " "))))

(defun e-org-canvas--input-remove-scope-reference ()
  "Remove the thread-scope cursor reference from the current input pane."
  (when e-org-canvas-input--scope-reference
    (goto-char (or e-chat--composer-start-marker (point-min)))
    (when (e-chat--delete-context-reference-at (point))
      (when (looking-at-p " ")
        (let ((inhibit-read-only t))
          (delete-char 1))))
    (setq-local e-org-canvas-input--scope-reference nil)))

(cl-defun e-org-canvas-submit-prompt
    (harness session-id prompt scope &key references)
  "Submit PROMPT to HARNESS SESSION-ID with Org Canvas SCOPE metadata."
  (e-org-canvas--maybe-save-new-buffer harness session-id prompt)
  (let* ((buffer (or (e-org-canvas-session-buffer harness session-id)
                     (user-error "Org Canvas session has no live Org buffer")))
         (focus (with-current-buffer buffer
                  (e-org-canvas-capture-focus scope)))
         (uri (plist-get focus :uri)))
    (e-org-canvas--update-last-turn-metadata harness session-id scope focus)
    (e-chat-session-submit
     harness
     session-id
     prompt
     :references references
     :metadata (list :org-canvas-scope scope
                     :org-canvas-focus focus
                     :org-canvas-uri uri))))

(cl-defun e-org-canvas--input-buffer
    (&key harness session-id scope target-buffer)
  "Create a temporary input buffer for HARNESS SESSION-ID and SCOPE."
  (let* ((title (or (and (buffer-live-p target-buffer)
                         (buffer-name target-buffer))
                    session-id
                    "canvas"))
         (buffer (get-buffer-create
                  (format e-org-canvas-input-buffer-name-format title))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (e-org-canvas-input-mode)
        (add-hook 'kill-buffer-hook #'e-org-canvas--input-cleanup nil t)
        (add-hook 'e-chat--running-status-rendered-hook
                  #'e-org-canvas--input-follow-bottom-on-redraw nil t)
        (setq-local e-current-harness harness)
        (setq-local e-chat-harness harness)
        (setq-local e-chat-session-id session-id)
        (e-buffer-set-workspace
         buffer
         (or (and (buffer-live-p target-buffer)
                  (e-buffer-workspace target-buffer))
             (e-workspace-current)))
        (e-org-canvas--input-reset-chat-state)
        (setq-local e-org-canvas-input--harness harness)
        (setq-local e-org-canvas-input--session-id session-id)
        (setq-local e-org-canvas-input--scope scope)
        (setq-local e-org-canvas-input--target-buffer target-buffer)
        (setq-local e-org-canvas-input--scope-reference nil)
        (setq-local e-org-canvas-input--submitting nil)
        (setq-local e-org-canvas-input--deferred-events nil)
        (setq-local e-org-canvas-input--final-message-rendered-p nil)
        (setq-local e-org-canvas-input--done-rendered-p nil)
        (setq-local e-org-canvas-input--source-selection-buffer
                    (and (buffer-live-p target-buffer)
                         (with-current-buffer target-buffer
                           (and (e-chat--active-region-p) target-buffer))))
        (e-org-canvas-input-result-mode -1)
        (e-chat--insert-composer)
        (e-org-canvas--input-insert-scope-reference)))
    (e-org-canvas--input-subscribe buffer harness session-id)
    buffer))

(defun e-org-canvas--input-prompt-text ()
  "Return the editable prompt text from the current input pane."
  (plist-get (e-chat--composer-submission) :prompt))

;;;###autoload
(defun e-org-canvas-input-submit ()
  "Submit the current Org Canvas input pane."
  (interactive)
  (let* ((submission (e-chat--composer-submission))
         (prompt (plist-get submission :prompt))
         (references (plist-get submission :references))
         (harness e-org-canvas-input--harness)
         (session-id e-org-canvas-input--session-id)
         (scope e-org-canvas-input--scope)
         (target e-org-canvas-input--target-buffer))
    (unless (and harness session-id)
      (user-error "This input pane is not attached to an Org Canvas session"))
    (when (string-empty-p prompt)
      (user-error "Prompt must not be empty"))
    (let ((buffer (current-buffer))
          turn-id)
      (setq-local e-org-canvas-input--submitting t)
      (setq-local e-org-canvas-input--deferred-events nil)
      (unwind-protect
          (setq turn-id
                (e-org-canvas-submit-prompt harness session-id prompt scope
                                            :references references))
        (setq-local e-org-canvas-input--submitting nil))
      (setq-local e-org-canvas-input--active-turn-id turn-id)
      (e-org-canvas--input-enter-result-state)
      (e-org-canvas--input-replay-deferred-events buffer)
      (unless e-org-canvas-input--final-message-rendered-p
        (when (buffer-live-p target)
          (e-workspace-pop-to-buffer
           target
           :workspace (or (e-buffer-workspace target)
                          (e-buffer-workspace buffer)
                          (e-workspace-current))))))))

;;;###autoload
(defun e-org-canvas-input-close-result ()
  "Close the submitted Org Canvas input/result pane and focus its Org buffer."
  (interactive)
  (let ((input (current-buffer))
        (target e-org-canvas-input--target-buffer)
        (input-window (get-buffer-window (current-buffer) t)))
    (e-org-canvas--input-cleanup)
    (when (window-live-p input-window)
      (quit-window t input-window))
    (when (buffer-live-p input)
      (kill-buffer input))
    (when (buffer-live-p target)
      (if-let ((window (get-buffer-window target t)))
          (select-window window)
        (e-workspace-pop-to-buffer
         target
         :workspace (or (e-buffer-workspace target)
                        (e-workspace-current)))))))

(defun e-org-canvas--input-abort-active-turn ()
  "Abort the running turn attached to the current input/result pane."
  (when (and e-org-canvas-input--harness
             e-org-canvas-input--session-id
             e-org-canvas-input--active-turn-id
             (equal (plist-get
                     (e-harness-state
                      e-org-canvas-input--harness
                      e-org-canvas-input--session-id)
                     :active-turn)
                    e-org-canvas-input--active-turn-id))
    (e-chat-session-abort e-org-canvas-input--harness
                          e-org-canvas-input--session-id)))

;;;###autoload
(defun e-org-canvas-input-cancel ()
  "Cancel the current Org Canvas input pane."
  (interactive)
  (let ((input (current-buffer))
        (target e-org-canvas-input--target-buffer)
        (input-window (get-buffer-window (current-buffer) t)))
    (e-org-canvas--input-abort-active-turn)
    (e-org-canvas--input-clear-source-selection)
    (e-org-canvas--input-cleanup)
    (if-let ((target-window (and (buffer-live-p target)
                                 (get-buffer-window target t))))
        (progn
          (when (window-live-p input-window)
            (quit-window t input-window))
          (select-window target-window))
      (when (and (window-live-p input-window)
                 (buffer-live-p target))
        (set-window-buffer input-window target)
        (select-window input-window)))
    (when (buffer-live-p input)
      (kill-buffer input))
    (when (buffer-live-p target)
      (unless (get-buffer-window target t)
        (e-workspace-pop-to-buffer
         target
         :workspace (or (e-buffer-workspace target)
                        (e-workspace-current)))))))

;;;###autoload
(defun e-org-canvas-input-open-session ()
  "Open the backing chat session for the current Org Canvas input pane."
  (interactive)
  (unless (and e-org-canvas-input--harness e-org-canvas-input--session-id)
    (user-error "This input pane is not attached to an Org Canvas session"))
  (let ((input (current-buffer))
        (input-window (selected-window)))
    (prog1 (e-chat-open-session
            e-org-canvas-input--harness
            e-org-canvas-input--session-id
            t)
      (when (and (window-live-p input-window)
                 (eq (window-buffer input-window) input))
        (quit-window t input-window))
      (when (buffer-live-p input)
        (kill-buffer input)))))

;;;###autoload
(defun e-org-canvas-input-switch-scope ()
  "Switch the current Org Canvas input pane scope."
  (interactive)
  (e-org-canvas--input-remove-scope-reference)
  (setq-local e-org-canvas-input--scope
              (if (eq e-org-canvas-input--scope 'thread)
                  'document
                'thread))
  (e-org-canvas--input-insert-scope-reference)
  (message "Org Canvas scope: %s" e-org-canvas-input--scope))

(defun e-org-canvas--ensure-current-session ()
  "Return (HARNESS SESSION-ID BUFFER) for the current Org Canvas buffer."
  (let* ((buffer (current-buffer))
         (chat-buffer (e-org-canvas-open-for-current-buffer))
         (harness (with-current-buffer chat-buffer e-chat-harness))
         (session-id (with-current-buffer chat-buffer e-chat-session-id)))
    (list harness session-id buffer)))

(defun e-org-canvas--display-input-buffer (input)
  "Display INPUT and select its editable prompt body."
  (with-current-buffer input
    (e-org-canvas--hide-visible-backing-chat-buffers
     e-org-canvas-input--session-id
     e-org-canvas-input--target-buffer))
  (let ((window (e-org-canvas--input-display-window input)))
    (when (window-live-p window)
      (select-window window))
    (e-chat--after-display-buffer input)
    (with-current-buffer input
      (goto-char (point-max)))
    input))

(defun e-org-canvas--prompt-scope (scope)
  "Open an Org Canvas input pane for SCOPE."
  (pcase-let ((`(,harness ,session-id ,buffer)
               (e-org-canvas--ensure-current-session)))
    (let ((input (e-org-canvas--input-buffer
                  :harness harness
                  :session-id session-id
                  :scope scope
                  :target-buffer buffer)))
      (e-org-canvas--display-input-buffer input))))

;;;###autoload
(defun e-org-canvas-prompt-thread ()
  "Open an Org Canvas input pane focused on the topic under point."
  (interactive)
  (e-org-canvas--prompt-scope 'thread))

;;;###autoload
(defun e-org-canvas-prompt-document ()
  "Open an Org Canvas input pane for the whole Org document."
  (interactive)
  (e-org-canvas--prompt-scope 'document))

;;;###autoload
(defun e-org-canvas-prompt (scope)
  "Open an Org Canvas input pane for SCOPE."
  (interactive
   (list (intern (completing-read "Org Canvas scope: "
                                  '("thread" "document")
                                  nil t nil nil "thread"))))
  (e-org-canvas--prompt-scope scope))

;;;###autoload
(defun e-org-canvas-compact (&optional instructions)
  "Compact the backing session for the current Org Canvas buffer."
  (interactive)
  (unless (and e-org-canvas-harness e-org-canvas-session-id)
    (user-error "This buffer is not an Org Canvas"))
  (e-chat-session-compact
   e-org-canvas-harness
   e-org-canvas-session-id
   :instructions instructions))

(defun e-org-canvas--last-prompt-message (harness session-id)
  "Return the last Org Canvas user prompt message for SESSION-ID."
  (cl-find-if
   (lambda (message)
     (and (eq (plist-get message :role) 'user)
          (plist-get (plist-get message :metadata) :org-canvas-scope)))
   (reverse (e-harness-messages harness session-id))))

;;;###autoload
(defun e-org-canvas-reopen-last-prompt ()
  "Reopen the most recent Org Canvas prompt for the current session."
  (interactive)
  (pcase-let ((`(,harness ,session-id ,target)
               (e-org-canvas--ensure-current-session)))
    (let* ((message (or (e-org-canvas--last-prompt-message harness session-id)
                        (user-error "No previous Org Canvas prompt")))
           (metadata (plist-get message :metadata))
           (scope (or (plist-get metadata :org-canvas-scope) 'thread))
           (input (e-org-canvas--input-buffer
                   :harness harness
                   :session-id session-id
                   :scope scope
                   :target-buffer target)))
      (with-current-buffer input
        (goto-char (point-max))
        (insert (or (plist-get message :content) "")))
      (e-org-canvas--display-input-buffer input))))

(defun e-org-canvas--thread-open-p (thread)
  "Return non-nil when THREAD summary is still awaiting a response.
A thread is open while it carries no accepted/rejected verdict and its status
is not one of Simply Annotate's terminal states."
  (and (null (plist-get thread :verdict))
       (not (member (plist-get thread :status) '("resolved" "closed")))))

(defun e-org-canvas--open-threads (file)
  "Return open annotation thread summaries on FILE."
  (unless (e-annotation-tools-available-p)
    (user-error "Annotation tools are not available; install simply-annotate"))
  (let ((threads (plist-get (e-annotation-tools-list :file file) :threads)))
    (seq-filter #'e-org-canvas--thread-open-p threads)))

(defun e-org-canvas--threads-prompt (file threads)
  "Return the prompt text asking the agent to respond to THREADS on FILE."
  (with-temp-buffer
    (insert e-org-canvas-respond-to-threads-instructions "\n\n")
    (insert (format "File: %s\n\n" file))
    (insert (format "Open threads (%d):\n" (length threads)))
    (dolist (thread threads)
      (insert (format "- thread %s [chars %s..%s]: %s\n"
                      (plist-get thread :thread-id)
                      (plist-get thread :start)
                      (plist-get thread :end)
                      (or (plist-get thread :proposal) "(no text)"))))
    (buffer-string)))

;;;###autoload
(defun e-org-canvas-respond-to-threads ()
  "Prompt the agent to respond to open annotation threads on the current canvas.
The current buffer must be a file-backed Org Canvas: annotation threads are
keyed to a file, so an unsaved buffer has none to answer.  Collect the threads
that still lack a verdict, open a document-scoped input pane seeded with a
prompt enumerating them, and leave the draft for review before submission."
  (interactive)
  (pcase-let ((`(,harness ,session-id ,target)
               (e-org-canvas--ensure-current-session)))
    (let ((file (buffer-file-name target)))
      (unless file
        (user-error "Save the canvas to a file before responding to threads"))
      (let ((threads (e-org-canvas--open-threads file)))
        (unless threads
          (user-error "No open annotation threads on %s"
                      (file-name-nondirectory file)))
        (let ((input (e-org-canvas--input-buffer
                      :harness harness
                      :session-id session-id
                      :scope 'document
                      :target-buffer target)))
          (with-current-buffer input
            (goto-char (point-max))
            (insert (e-org-canvas--threads-prompt file threads)))
          (e-org-canvas--display-input-buffer input))))))

(defun e-org-canvas--session-choice-label (session)
  "Return completion label for Org Canvas SESSION."
  (let ((canvas (e-org-canvas--session-canvas session)))
    (format "%s  [%s]"
            (or (plist-get canvas :label)
                (plist-get session :name)
                (plist-get session :id))
            (plist-get session :id))))

(defun e-org-canvas--read-session (sessions prompt)
  "Read one session id from SESSIONS with PROMPT."
  (let* ((labels (mapcar #'e-org-canvas--session-choice-label sessions))
         (selected (completing-read prompt labels nil t))
         (index (cl-position selected labels :test #'equal)))
    (or (plist-get (nth index sessions) :id)
        (user-error "No Org Canvas session selected"))))

(defun e-org-canvas--current-file-filter ()
  "Return current buffer file when Org-related, or nil."
  (and (derived-mode-p 'org-mode)
       buffer-file-name
       (expand-file-name buffer-file-name)))

;;;###autoload
(defun e-org-canvas-resume-session (harness session-id)
  "Resume HARNESS SESSION-ID and return its Org canvas buffer."
  (let ((buffer (or (e-org-canvas-session-buffer harness session-id)
                    (user-error "Org Canvas session has no buffer"))))
    (e-chat-open :harness harness :session-id session-id)
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (setq-local e-org-canvas-harness harness)
      (setq-local e-org-canvas-session-id session-id)
      (e-org-canvas-mode 1))
    (e-org-canvas--select-org-buffer buffer)
    buffer))

;;;###autoload
(defun e-org-canvas-list-sessions ()
  "List and resume Org Canvas sessions, defaulting to the current Org file."
  (interactive)
  (let* ((harness (e-org-canvas--default-harness))
         (file (e-org-canvas--current-file-filter))
         (sessions (or (and file
                            (e-org-canvas--session-candidates
                             harness :file file))
                       (e-org-canvas--session-candidates harness))))
    (unless sessions
      (user-error "No Org Canvas sessions"))
    (e-org-canvas-resume-session
     harness
     (e-org-canvas--read-session sessions "Org Canvas session: "))))

;;;###autoload
(defun e-org-canvas-resume ()
  "Resume an Org Canvas session."
  (interactive)
  (e-org-canvas-list-sessions))

;;;###autoload
(defun e-org-canvas-list-project-sessions ()
  "List Org Canvas sessions grouped by file under the current project."
  (interactive)
  (let* ((harness (e-org-canvas--default-harness))
         (root (e-chat--project-root default-directory))
         (groups (e-org-canvas--sessions-by-file
                  harness :project-root root)))
    (unless groups
      (user-error "No Org Canvas sessions in project"))
    (let* ((files (mapcar #'car groups))
           (uri (completing-read "Org Canvas file: " files nil t))
           (sessions (cdr (assoc uri groups))))
      (e-org-canvas-resume-session
       harness
       (e-org-canvas--read-session sessions "Org Canvas session: ")))))

;;;###autoload
(defun e-org-canvas-shell ()
  "Return the Org Canvas presentation shell manifest."
  (e-shell-create
   :id 'org-canvas
   :name "Org Canvas"
   :summary "Org document canvas with temporary prompt panes."
   :required-capabilities '(chat-session org-canvas)
   :commands
   (list
    (e-shell-command-create
     :id 'open-for-current-buffer
     :summary "Start or reuse Org Canvas for the current Org buffer."
     :interactive 'e-org-canvas-open-for-current-buffer
     :function 'e-org-canvas-open-for-current-buffer
     :scope 'global)
    (e-shell-command-create
     :id 'new-file
     :summary "Create or visit an Org file and start Org Canvas."
     :interactive 'e-org-canvas-new-file
     :function 'e-org-canvas-new-file
     :scope 'global)
    (e-shell-command-create
     :id 'new-buffer
     :summary "Create a new unsaved Org Canvas buffer."
     :interactive 'e-org-canvas-new-buffer
     :function 'e-org-canvas-new-buffer
     :scope 'global)
    (e-shell-command-create
     :id 'prompt-thread
     :summary "Prompt about the topic under point."
     :interactive 'e-org-canvas-prompt-thread
     :function 'e-org-canvas-prompt-thread
     :scope 'session)
    (e-shell-command-create
     :id 'prompt-document
     :summary "Prompt about the whole Org document."
     :interactive 'e-org-canvas-prompt-document
     :function 'e-org-canvas-prompt-document
     :scope 'session)
    (e-shell-command-create
     :id 'prompt
     :summary "Prompt with selected Org Canvas scope."
     :interactive 'e-org-canvas-prompt
     :function 'e-org-canvas-prompt
     :scope 'session)
    (e-shell-command-create
     :id 'reopen-last-prompt
     :summary "Reopen the previous Org Canvas prompt."
     :interactive 'e-org-canvas-reopen-last-prompt
     :function 'e-org-canvas-reopen-last-prompt
     :scope 'session)
    (e-shell-command-create
     :id 'respond-to-threads
     :summary "Prompt the agent to respond to open annotation threads."
     :interactive 'e-org-canvas-respond-to-threads
     :function 'e-org-canvas-respond-to-threads
     :scope 'session)
    (e-shell-command-create
     :id 'compact
     :summary "Compact the current Org Canvas session context."
     :interactive 'e-org-canvas-compact
     :function 'e-org-canvas-compact
     :scope 'session)
    (e-shell-command-create
     :id 'list-sessions
     :summary "List Org Canvas sessions for the current file."
     :interactive 'e-org-canvas-list-sessions
     :function 'e-org-canvas-list-sessions
     :scope 'global)
    (e-shell-command-create
     :id 'list-project-sessions
     :summary "List project Org Canvas sessions grouped by file."
     :interactive 'e-org-canvas-list-project-sessions
     :function 'e-org-canvas-list-project-sessions
     :scope 'global)
    (e-shell-command-create
     :id 'resume
     :summary "Resume an Org Canvas session."
     :interactive 'e-org-canvas-resume
     :function 'e-org-canvas-resume
     :scope 'global))
   :keymaps
   (list (list :id 'org-canvas-mode
               :keymap e-org-canvas-mode-map
               :scope 'mode)
         (list :id 'org-canvas-input-mode
               :keymap e-org-canvas-input-mode-map
               :scope 'mode)
         (list :id 'org-canvas-input-result-mode
               :keymap e-org-canvas-input-result-mode-map
               :scope 'mode))))

(defun e-org-canvas-startup ()
  "Refresh and register Org Canvas shell provider for package startup."
  (setq e-org-canvas-mode-map (e-org-canvas--make-mode-map))
  (setq e-org-canvas-input-mode-map (e-org-canvas--make-input-mode-map))
  (setq e-org-canvas-input-result-mode-map
        (e-org-canvas--make-input-result-mode-map))
  (e-org-canvas--refresh-mode-buffers)
  (e-shell-register (e-org-canvas-shell)))

(add-hook 'e-startup-shell-hook #'e-org-canvas-startup)

(provide 'e-org-canvas)

;;; e-org-canvas.el ends here

;;; e-chat-composer.el --- Chat composer owner -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns editable chat input, submission extraction, inline completion, command
;; and file/resource references, and composer-local editing state.  The public
;; facade and embedding shells consume the narrow operations at the end.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'subr-x)
(require 'e-capabilities)
(require 'e-chat-service)
(require 'e-chat-surface)
(require 'e-chat-transcript)
(require 'e-picker)
(require 'e-prompts)
(require 'e-request)
(require 'e-tools)

(defcustom e-chat-command-output-timeout 30
  "Seconds to wait for composer ! commands before capturing a timeout."
  :type 'number
  :group 'e-chat)

(defcustom e-chat-command-output-max-bytes 24000
  "Maximum UTF-8 bytes captured from a composer ! command."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-file-reference-max-bytes 64000
  "Maximum UTF-8 bytes read for composer @ file references."
  :type 'integer
  :group 'e-chat)

(defcustom e-chat-project-file-candidate-limit 2000
  "Maximum number of files offered by composer @ completion."
  :type 'integer
  :group 'e-chat)

(declare-function e-chat-surface-composer-p "e-chat-surface")
(declare-function e-chat-surface-transcript-buffer "e-chat-surface")
(declare-function e-chat-surface-transcript-p "e-chat-surface")
(declare-function e-chat-surface-show-composer "e-chat-surface")
(declare-function e-chat-surface-set-window-output-follow "e-chat-surface")
(declare-function e-chat-surface-window-reaches-output-p "e-chat-surface")
(declare-function e-chat-surface-output-follow-position "e-chat-surface")
(declare-function e-chat-surface-selected-chat-surface "e-chat-surface")
(declare-function e-chat-surface-redraw-visible-p "e-chat-surface")
(declare-function e-chat-surface-set-command-map-active "e-chat-surface")
(declare-function e-chat-surface-mark-composer-layout-dirty "e-chat-surface")
(declare-function e-chat-surface-refresh-composer-layout "e-chat-surface")
(declare-function e-chat-surface-pre-command "e-chat-surface")
(declare-function e-chat-surface-post-command "e-chat-surface")
(declare-function e-chat-surface-unbind-composer "e-chat-surface")
(declare-function e-chat-surface-mark-composer "e-chat-surface")
(declare-function e-chat-transcript-leave-navigation "e-chat-transcript")
(declare-function e-chat-transcript-enter-response-navigation "e-chat-transcript")
(declare-function e-chat-transcript-insert-protected "e-chat-transcript")
(declare-function e-chat-response-navigation-mode "e-chat-transcript")
(declare-function e-chat-block-view-mode "e-chat-transcript")
(declare-function e-chat-tool-list-mode "e-chat-transcript")

(defvar e-chat-composer-mode-map (make-sparse-keymap)
  "Keymap for the editable pane of a composed e chat surface.
The facade may install a richer parent map after loading its own commands.")

(defconst e-chat-composer--composer-glyph "❯ "
  "Glyph shown before editable e chat composer text.")
(defconst e-chat-composer--composer-separator
  "────────────────────────────────────────────────────────────────"
  "Separator shown above the e chat composer.")
(defconst e-chat-composer--composer-stripped-properties
  '(read-only e-chat-protected field
    face font-lock-face invisible display
    e-chat-block-id e-chat-turn-id e-chat-separator e-chat-composer
    e-chat-transient-turn-id e-chat-progress-turn-id
    e-chat-markdown-syntax mouse-face help-echo)
  "Presentation properties stripped from ordinary composer text.")
(defconst e-chat-composer--composer-reference-stripped-properties
  '(e-chat-protected field
    face invisible e-chat-block-id e-chat-turn-id e-chat-separator
    e-chat-composer e-chat-transient-turn-id e-chat-progress-turn-id
    e-chat-markdown-syntax)
  "Presentation properties stripped from inline composer references.")
(defvar e-chat-harness nil)
(defvar e-chat-session-id nil)
(defvar e-chat-response-navigation-mode)
(defvar e-chat-block-view-mode)
(defvar e-chat-tool-list-mode)
(defconst e-chat-composer--composer-edit-commands
  '(self-insert-command
    e-chat-composer-bang
    e-chat-composer-at
    e-chat-composer-slash
    newline
    yank
    yank-pop
    clipboard-yank
    quoted-insert
    e-chat-delete-backward-char
    e-chat-delete-forward-char
    delete-backward-char
    backward-delete-char-untabify
    delete-forward-char
    delete-char)
  "Commands that should resume composer input from readback position.")
(defvar e-chat-command-output-max-bytes)
(defvar e-chat-command-output-timeout)
(defvar e-chat-project-file-candidate-limit)
(defvar e-chat-file-reference-max-bytes)

(defun e-chat-composer--composer-buffer-p ()
  "Return non-nil when the current buffer implements the composer contract."
  (derived-mode-p 'e-chat-composer-mode))

(defun e-chat-composer--surface-composer-mode-name ()
  "Return the semantic surface status used by a composed input pane."
  (let ((transcript (e-chat-surface-transcript-buffer)))
    (if (and (buffer-live-p transcript)
             (not (eq transcript (current-buffer))))
      (let ((status (e-chat-surface-mode-line-status transcript)))
        (if (stringp status)
            (e-chat-surface-mode-line-display-text status)
          "e-chat"))
      "e-chat-input")))

(define-derived-mode e-chat-composer-mode text-mode "e-chat-input"
  "Editable input pane using the e chat composer contract.
Most composer buffers are owned by a transcript surface.  Presentation shells
may also derive a transient standalone input/result pane from this mode."
  (use-local-map e-chat-composer-mode-map)
  (e-chat-surface-mark-composer)
  (e-chat-surface-set-command-map-active t)
  (setq-local mode-name '(:eval (e-chat-composer--surface-composer-mode-name)))
  (e-chat-surface-setup-line-wrapping)
  (e-chat-composer-disable-modal-editing)
  (e-chat-composer-disable-completion)
  (add-hook 'after-change-functions
            #'e-chat-composer-mark-scroll-needed nil t)
  (add-hook 'after-change-functions
            #'e-chat-surface-mark-composer-layout-dirty nil t)
  (add-hook 'pre-command-hook #'e-chat-composer-pre-command nil t)
  (add-hook 'pre-command-hook #'e-chat-surface-pre-command nil t)
  (add-hook 'post-command-hook #'e-chat-composer--post-command nil t)
  (add-hook 'post-command-hook #'e-chat-surface-post-command nil t)
  (add-hook 'post-command-hook #'e-chat-surface-refresh-composer-layout nil t)
  (add-hook 'kill-buffer-hook
            #'e-chat-composer-cancel-pending-references nil t)
  (add-hook 'kill-buffer-hook #'e-chat-composer--surface-composer-killed nil t))

(defun e-chat-composer--surface-composer-killed ()
  "Clear this input pane from its surface when it is killed."
  (e-chat-surface-unbind-composer (current-buffer)))

(defun e-chat-composer--enter-insert-state ()
  "Put an Evil-enabled composer into insert state when focused."
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(defun e-chat-composer-enter-navigation ()
  "Focus the transcript and enter response navigation when it has a block."
  (interactive)
  (let ((transcript (e-chat-surface-transcript-buffer)))
    (unless (buffer-live-p transcript)
      (user-error "This e chat composer has no live transcript"))
    (when-let ((window (get-buffer-window transcript t)))
      (select-window window))
    (with-current-buffer transcript
      ;; A brand-new transcript has no block to navigate, but Escape still
      ;; means leave the composer.  Once content exists, retain normal
      ;; navigation behavior.
      (when (e-chat-transcript-has-navigable-blocks-p)
        (e-chat-transcript-enter-response-navigation)))))

(defun e-chat-composer-enter-input-state ()
  "Leave transcript navigation and focus this surface's composer.
The composer owns this transition because it changes both the transcript's
navigation mode and the editable pane's focus; the surface owner only
performs the resulting window operation."
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (e-chat-composer-disable-modal-editing)
  (e-chat-composer-disable-completion)
  (let ((transcript (e-chat-surface-transcript-buffer)))
    (when (buffer-live-p transcript)
      (with-current-buffer transcript
        (e-chat-transcript-leave-navigation)))
    (e-chat-surface-show-composer)
    (let ((composer (and (buffer-live-p transcript)
                         (e-chat-surface-composer-buffer transcript))))
      (when (buffer-live-p composer)
        (with-current-buffer composer
          (e-chat-composer--enter-insert-state)))
      (unless (buffer-live-p composer)
        (e-chat-composer--enter-insert-state)))))

(defvar-local e-chat-composer--composer-start-marker nil
  "Marker at the beginning of editable composer text.")

(defvar-local e-chat-composer--queue-start-marker nil
  "Marker at the start of the queued prompt list.")

(defvar-local e-chat-composer--queue-end-marker nil
  "Marker at the end of the queued prompt list.")

(defvar-local e-chat-composer--composer-scroll-needed nil
  "Non-nil when a composer edit should scroll input fully into view.")

(defvar-local e-chat-composer--composer-scroll-suppressed nil
  "Non-nil while internal composer rewrites should not request scrolling.")

(defvar-local e-chat-composer--context-reference-counter 0
  "Counter used to assign inline composer reference ids.")

(defvar-local e-chat-composer--pending-command-requests nil
  "Alist of pending composer command-reference ids to cancellable requests.")

(defvar-local e-chat-composer--project-file-candidate-cache nil
  "Cached composer file candidates as a plist with :key and :candidates.")

(defvar-local e-chat-composer--project-file-candidate-request nil
  "Active async refresh request for composer file candidates.")

(defvar-local e-chat-composer--project-file-candidate-generation 0
  "Generation token for composer file candidate refresh callbacks.")

(defun e-chat-composer--composer-active-p ()
  "Return non-nil when the current buffer has an active composer."
  (and (markerp e-chat-composer--composer-start-marker)
       (marker-position e-chat-composer--composer-start-marker)))

(defun e-chat-composer--delete-composer ()
  "Clear editable input from the current composer buffer.
Return non-nil when active input was removed."
  (when (and (e-chat-composer--composer-buffer-p)
             (e-chat-composer--composer-active-p))
    (let ((inhibit-read-only t)
          (e-chat-composer--composer-scroll-suppressed t))
      (delete-region (marker-position e-chat-composer--composer-start-marker)
                     (point-max)))
    (set-marker e-chat-composer--composer-start-marker nil)
    (setq e-chat-composer--composer-scroll-needed nil)
    t))

(defun e-chat-composer--sanitize-composer-text (text)
  "Return TEXT without leaked transcript presentation properties."
  (let ((copy (copy-sequence text))
        (position 0)
        next)
    (while (< position (length copy))
      (setq next (or (next-single-property-change
                      position 'e-chat-context-reference copy)
                     (length copy)))
      (remove-list-of-text-properties
       position
       next
       (if (get-text-property position 'e-chat-context-reference copy)
           e-chat-composer--composer-reference-stripped-properties
         e-chat-composer--composer-stripped-properties)
       copy)
      (setq position next))
    copy))

(defun e-chat-composer--visible-window ()
  "Return a visible window for the current chat buffer."
  (get-buffer-window (current-buffer) t))

(defun e-chat-composer--redraw-visible-p ()
  "Return non-nil when this chat buffer should run expensive redraws now.
A chat buffer displayed in no window is never repainted for progress or
  activity; the redraw is deferred until the buffer next becomes visible.  This
  keeps a background turn from stalling the single main thread by repainting a
  transcript nobody is looking at.  Tests without a live window use the
  surface-owned `e-chat-surface-set-redraw-visible' port."
  (e-chat-surface-redraw-visible-p))

(defun e-chat-composer--queued-prompts ()
  "Return queued prompt items for the attached chat session."
  (when (and e-chat-harness e-chat-session-id)
    (ignore-errors
      (e-chat-service-queued-inputs e-chat-harness e-chat-session-id))))

(defun e-chat-composer--queue-preview-text (prompt)
  "Return compact one-line preview text for queued PROMPT."
  (let ((text (string-trim
               (replace-regexp-in-string "[\n\r\t ]+" " " (or prompt "")))))
    (if (> (length text) 96)
      (concat (substring text 0 93) "...")
      text)))

(defun e-chat-composer--string-byte-prefix (text max-bytes)
  "Return TEXT prefix limited to MAX-BYTES UTF-8 bytes."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (let ((next-bytes
                       (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next-bytes) max-bytes)
                    (setq bytes (+ bytes next-bytes))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-chat-composer--insert-queued-prompts ()
  "Insert queued prompt previews above the composer separator."
  (let ((items (e-chat-composer--queued-prompts)))
    (if (not items)
        (progn
          (when (markerp e-chat-composer--queue-start-marker)
            (set-marker e-chat-composer--queue-start-marker nil))
          (when (markerp e-chat-composer--queue-end-marker)
            (set-marker e-chat-composer--queue-end-marker nil)))
      (setq e-chat-composer--queue-start-marker (point-marker))
      (set-marker-insertion-type e-chat-composer--queue-start-marker nil)
      (e-chat-transcript-insert-protected "Queued prompts\n" 'e-chat-separator-face)
      (cl-loop for item in items
               for index from 1
               do (e-chat-transcript-insert-protected
                   (format "%d. %s\n"
                           index
                           (e-chat-composer--queue-preview-text
                            (plist-get item :prompt)))
                   'e-chat-separator-face))
      (setq e-chat-composer--queue-end-marker (point-marker))
      (set-marker-insertion-type e-chat-composer--queue-end-marker nil))))

(defun e-chat-composer--insert-composer (&optional text preserve-focus)
  "Initialize the current chat surface's editable composer.
PRESERVE-FOCUS retains composer point when the current buffer is the composer."
  (if (e-chat-composer--composer-buffer-p)
      (e-chat-composer-initialize text preserve-focus)
    (let ((composer (e-chat-composer--ensure-composer)))
      (when text
        (with-current-buffer composer
          (e-chat-composer-initialize text preserve-focus)))
      composer)))

(defun e-chat-composer--create-composer (transcript)
  "Create and initialize the composer paired with TRANSCRIPT."
  (let ((composer (generate-new-buffer
                   (format " *e-chat input:%s*" (buffer-name transcript)))))
    (with-current-buffer composer
      (e-chat-composer-mode))
    (e-chat-surface-bind-composer composer transcript)
    (with-current-buffer composer
      (e-chat-composer-initialize))
    composer))

(defun e-chat-composer--ensure-composer ()
  "Ensure the current chat buffer has an active composer."
  (cond
   ((e-chat-composer--composer-buffer-p)
    (unless (e-chat-composer--composer-active-p)
      (e-chat-composer-initialize))
    (current-buffer))
   ((let ((transcript (e-chat-surface-transcript-buffer)))
      (when (and (buffer-live-p transcript)
                 (not (e-chat-surface-composer-p)))
        (or (e-chat-surface-composer-buffer transcript)
            (e-chat-composer--create-composer transcript)))))
   (t
    (user-error "This buffer is not an e chat surface"))))

(defun e-chat-composer--point-in-composer-p (&optional position)
  "Return non-nil when POSITION, or point, is in editable composer text."
  (and (e-chat-composer--composer-active-p)
       (>= (or position (point))
           (marker-position e-chat-composer--composer-start-marker))))

(defun e-chat-composer--clamp-to-composer ()
  "Move point back to the editable composer boundary when it escaped upward."
  (when (and (e-chat-composer--composer-active-p)
             (not e-chat-response-navigation-mode)
             (not e-chat-block-view-mode)
             (not e-chat-tool-list-mode)
             (< (point) (marker-position e-chat-composer--composer-start-marker)))
    (goto-char e-chat-composer--composer-start-marker)))

(defun e-chat-composer--mark-composer-scroll-needed (_begin end _length)
  "Record that a composer edit ending at END needs bottom visibility."
  (when (and (not e-chat-composer--composer-scroll-suppressed)
             (e-chat-composer--composer-active-p)
             (> end (marker-position e-chat-composer--composer-start-marker)))
    (setq e-chat-composer--composer-scroll-needed t)))

(defun e-chat-composer--scroll-composer-edit-into-view ()
  "Scroll the current composer edit down without changing user scroll policy."
  (when-let ((window (e-chat-composer--visible-window)))
    (set-window-point window (point))
    (with-selected-window window
      (ignore-errors
        (recenter -2)))))

(defun e-chat-composer--composer-edit-command-p (command)
  "Return non-nil when COMMAND should target composer input."
  (memq command e-chat-composer--composer-edit-commands))

(defun e-chat-composer--pre-command ()
  "Redirect edit commands from readback into the composer."
  (cond
   ((and (e-chat-composer--composer-active-p)
         (not e-chat-response-navigation-mode)
         (not e-chat-block-view-mode)
         (not e-chat-tool-list-mode)
         (not (e-chat-composer--point-in-composer-p))
         (e-chat-composer--composer-edit-command-p this-command))
    (e-chat-surface-show-composer))))

(defun e-chat-previous-line (&optional arg try-vscroll)
  "Move up ARG lines like `previous-line', honoring TRY-VSCROLL.
Keep point inside the composer when movement starts there."
  (interactive "^p\np")
  (let ((started-in-composer (e-chat-composer--point-in-composer-p)))
    (unwind-protect
        (line-move (- (or arg 1)) nil nil try-vscroll)
      (when started-in-composer
        (e-chat-composer--clamp-to-composer)))))

(defun e-chat-composer--composer-text ()
  "Return the current editable composer text."
  (unless (e-chat-composer--composer-active-p)
    (user-error "No active e chat composer"))
  (string-trim
   (buffer-substring-no-properties e-chat-composer--composer-start-marker
                                   (point-max))))

(defun e-chat-composer--composer-text-before-point ()
  "Return composer text from its start through point."
  (buffer-substring-no-properties e-chat-composer--composer-start-marker (point)))

(defun e-chat-composer--composer-leading-prefix-p ()
  "Return non-nil when point is at the first non-whitespace composer input."
  (and (e-chat-composer--point-in-composer-p)
       (string-match-p "\\`[[:space:]]*\\'"
                       (e-chat-composer--composer-text-before-point))))

(defun e-chat-composer--composer-word-boundary-prefix-p ()
  "Return non-nil when point is at a composer prefix word boundary."
  (and (e-chat-composer--point-in-composer-p)
       (let ((start (marker-position e-chat-composer--composer-start-marker)))
         (or (= (point) start)
             (eq (char-syntax (char-before)) ?\s)))))

(defun e-chat-composer--insert-literal-prefix (prefix)
  "Insert literal PREFIX in the composer when possible."
  (when (e-chat-composer--point-in-composer-p)
    (insert prefix)))

(defun e-chat-composer--self-insert-prefix ()
  "Fallback to ordinary self insertion for a non-triggering prefix command."
  (if (e-chat-composer--point-in-composer-p)
      (call-interactively #'self-insert-command)
    nil))

(defun e-chat-composer--command-uri (command)
  "Return a compact command URI for COMMAND."
  (concat "command://"
          (replace-regexp-in-string "[\n\r\t ]+" " " command)))

(cl-defun e-chat-composer--run-shell-command-start
    (command directory &key on-done on-error on-request-start)
  "Start shell COMMAND in DIRECTORY and report captured output asynchronously.
ON-DONE receives the same result plist returned by
`e-chat-composer--run-shell-command'.
ON-ERROR receives an Emacs condition list.  ON-REQUEST-START receives a
cancellable process request."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (buffer (generate-new-buffer " *e-chat-command-output*"))
         (settled nil)
         process
         timeout-timer
         request)
    (cl-labels
        ((cleanup
          ()
          (when (timerp timeout-timer)
            (cancel-timer timeout-timer))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
         (result
          (&optional timed-out)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let* ((output (buffer-string))
                     (truncated (> (string-bytes output)
                                   e-chat-command-output-max-bytes))
                     (output (if truncated
                                 (concat
                                  (e-chat-composer--string-byte-prefix
                                   output
                                   e-chat-command-output-max-bytes)
                                  "\n[Command output truncated]\n")
                               output)))
                (list :output output
                      :exit (unless timed-out
                              (and process (process-exit-status process)))
                      :truncated truncated
                      :timed-out timed-out)))))
         (finish
          (&optional timed-out)
          (unless settled
            (setq settled t)
            (let ((value (result timed-out)))
              (cleanup)
              (when on-done
                (funcall on-done value)))))
         (fail
          (err)
          (unless settled
            (setq settled t)
            (cleanup)
            (when on-error
              (funcall on-error err))))
         (cancel
          ()
          (unless settled
            (setq settled t)
            (when (timerp timeout-timer)
              (cancel-timer timeout-timer))
            (when (and process (process-live-p process))
              (kill-process process))
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))
          t))
      (condition-case err
          (let ((default-directory directory))
            (setq process
                  (make-process
                   :name "e-chat-command-output"
                   :buffer buffer
                   :stderr buffer
                   :command (list shell-file-name shell-command-switch command)
                   :connection-type 'pipe
                   :noquery t
                   :sentinel
                   (lambda (proc _event)
                     (when (and (not settled)
                                (memq (process-status proc) '(exit signal)))
                       (finish nil)))))
            (set-process-query-on-exit-flag process nil)
            (setq request
                  (e-tools-request-create
                   :cancel #'cancel
                   :metadata (list :transport 'process
                                   :process process
                                   :command command
                                   :cancellable t)))
            (when on-request-start
              (funcall on-request-start request))
            (setq timeout-timer
                  (run-at-time
                   e-chat-command-output-timeout
                   nil
                   (lambda ()
                     (unless settled
                       (when (process-live-p process)
                         (kill-process process))
                       (finish t)))))
            request)
        (error
         (fail err)
         nil)))))

(defun e-chat-composer--run-shell-command (command directory)
  "Run shell COMMAND in DIRECTORY and return captured output metadata."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-chat-composer--run-shell-command))
  (let ((done nil)
        result
        failure)
    (e-chat-composer--run-shell-command-start
     command
     directory
     :on-done (lambda (value)
                (setq result value)
                (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.05))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(defun e-chat-composer--command-output-reference (command result)
  "Return a context reference for shell COMMAND RESULT."
  (let* ((timed-out (plist-get result :timed-out))
         (exit (plist-get result :exit))
         (status (if timed-out
                     (format "timed out after %ss"
                             e-chat-command-output-timeout)
                   (format "exit %s" exit)))
         (output (plist-get result :output)))
    (list :uri (e-chat-composer--command-uri command)
          :label (format "$ %s (%s)" command status)
          :text (string-join
                 (delq nil
                       (list (format "$ %s" command)
                             (format "Status: %s" status)
                             (when (plist-get result :truncated)
                               "Output was truncated.")
                             ""
                             output))
                 "\n"))))

(defun e-chat-composer--workspace-roots ()
  "Return active chat workspace roots, falling back to the project root."
  (or (and e-chat-harness
           e-chat-session-id
           (e-harness-workspace-roots e-chat-harness e-chat-session-id))
      (list (e-chat-composer-project-root))))

(defun e-chat-composer--git-root (directory)
  "Return Git worktree root containing DIRECTORY, or nil."
  (when-let ((root (locate-dominating-file directory ".git")))
    (file-name-as-directory (expand-file-name root))))

(defun e-chat-composer--project-root (&optional directory)
  "Return the normalized project root for DIRECTORY.
Projectile is preferred when available, followed by `project-current', then a
plain Git ancestor check.  Fall back to DIRECTORY when no project marker is
present."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (projectile-root
          (when (fboundp 'projectile-project-root)
            (let ((default-directory directory))
              (ignore-errors (projectile-project-root)))))
         (project-root
          (let ((default-directory directory))
            (ignore-errors
              (when-let ((project (project-current nil)))
                (project-root project)))))
         (root (or projectile-root
                   project-root
                   (e-chat-composer--git-root directory)
                   directory)))
    (file-name-as-directory (expand-file-name root))))

(defun e-chat-composer--git-file-candidates (root limit)
  "Return git-tracked and untracked file paths under ROOT, or nil.
When ROOT is inside a git repository with no eligible files, return
`:e-chat-git-empty' so callers do not fall through to an ignore-blind recursive
scan."
  (when (executable-find "git")
    (with-temp-buffer
      (let ((status (process-file
                     "git"
                     nil
                     (list t nil)
                     nil
                     "-C"
                     root
                     "ls-files"
                     "-co"
                     "--exclude-standard")))
        (when (zerop status)
          (let (files)
            (dolist (line (split-string (buffer-string) "\n" t))
              (when (< (length files) limit)
                (push (expand-file-name line root) files)))
            (or (nreverse files) :e-chat-git-empty)))))))

(defun e-chat-composer--fallback-file-candidates (root limit)
  "Return at most LIMIT regular file paths under ROOT."
  (let (files)
    (catch 'done
      (cl-labels ((walk
                   (directory)
                   (dolist (path (directory-files
                                  directory
                                  t
                                  directory-files-no-dot-files-regexp))
                     (cond
                      ((and (file-directory-p path)
                            (not (member (file-name-nondirectory path)
                                         '(".git" ".hg" ".svn"))))
                       (walk path))
                      ((file-regular-p path)
                       (push path files)
                       (when (>= (length files) limit)
                         (throw 'done nil)))))))
        (walk root)))
    (nreverse files)))

(defun e-chat-composer--fd-executable ()
  "Return the fd executable for file candidate discovery, or nil."
  (or (executable-find "fd")
      (executable-find "fdfind")))

(defun e-chat-composer--fd-file-candidates (root limit)
  "Return at most LIMIT regular file paths under ROOT using fd."
  (when-let ((fd (e-chat-composer--fd-executable)))
    (with-temp-buffer
      (let ((status (process-file
                     fd
                     nil
                     (list t nil)
                     nil
                     "--type"
                     "file"
                     "--hidden"
                     "--exclude"
                     ".git"
                     "--color"
                     "never"
                     "--base-directory"
                     root
                     ".")))
        (when (zerop status)
          (let (files)
            (dolist (line (split-string (buffer-string) "\n" t))
              (when (< (length files) limit)
                (push (expand-file-name line root) files)))
            (nreverse files)))))))

(defun e-chat-composer--usable-workspace-root-p (root)
  "Return non-nil when ROOT can be scanned for composer file completion."
  (and (stringp root)
       (file-directory-p root)))

(defun e-chat-composer--project-file-candidate-cache-key ()
  "Return cache key for the active composer file candidate snapshot."
  (list :roots (mapcar (lambda (root)
                         (file-name-as-directory (expand-file-name root)))
                       (e-chat-composer--workspace-roots))
        :limit e-chat-project-file-candidate-limit))

(defun e-chat-composer--project-file-candidate-cache-hit-p (key)
  "Return non-nil when composer file candidate cache matches KEY."
  (and (plist-get e-chat-composer--project-file-candidate-cache :ready)
       (equal key (plist-get e-chat-composer--project-file-candidate-cache :key))))

(defun e-chat-composer--project-file-candidates-loading-p ()
  "Return non-nil when composer file candidate refresh is in flight."
  (and e-chat-composer--project-file-candidate-request t))

(defun e-chat-composer--cancel-project-file-candidate-refresh ()
  "Cancel the active composer file candidate refresh request."
  (when e-chat-composer--project-file-candidate-request
    (e-tools-cancel-request e-chat-composer--project-file-candidate-request)
    (setq e-chat-composer--project-file-candidate-request nil)))

(defun e-chat-composer--disambiguate-file-candidates (candidates)
  "Return CANDIDATES with duplicate labels qualified by root."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (candidate candidates)
      (let ((label (plist-get candidate :label)))
        (puthash label (1+ (gethash label counts 0)) counts)))
    (mapcar
     (lambda (candidate)
       (let ((label (plist-get candidate :label)))
         (if (> (gethash label counts 0) 1)
             (plist-put (copy-sequence candidate)
                        :label
                        (format "%s (%s)"
                                label
                                (abbreviate-file-name
                                 (directory-file-name
                                  (plist-get candidate :root)))))
           candidate)))
     candidates)))

(defun e-chat-composer--project-file-candidates-sync ()
  "Return project file completion candidates for the active chat session."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error
     'e-chat-composer--project-file-candidates-sync))
  (let ((remaining e-chat-project-file-candidate-limit)
        candidates)
    (dolist (root (e-chat-composer--workspace-roots))
      (when (> remaining 0)
        (let ((root (file-name-as-directory (expand-file-name root))))
          (when (e-chat-composer--usable-workspace-root-p root)
            (let* ((files (e-chat-composer--git-file-candidates root remaining))
                   (files (cond
                           ((eq files :e-chat-git-empty) nil)
                           (files files)
                           (t (or (e-chat-composer--fd-file-candidates
                                   root
                                   remaining)
                                  (e-chat-composer--fallback-file-candidates
                                   root
                                   remaining))))))
              (dolist (file files)
                (let ((label (file-relative-name file root)))
                  (push (list :label label :path file :root root) candidates)))
              (setq remaining (- remaining (length files))))))))
    (e-chat-composer--disambiguate-file-candidates (nreverse candidates))))

(defun e-chat-composer--project-file-candidates-refresh-start (key)
  "Start an async refresh of composer file candidates for KEY."
  (unless (and e-chat-composer--project-file-candidate-request
               (equal key (plist-get e-chat-composer--project-file-candidate-cache :key)))
    (e-chat-composer--cancel-project-file-candidate-refresh)
    (setq e-chat-composer--project-file-candidate-generation
          (1+ e-chat-composer--project-file-candidate-generation))
    (setq e-chat-composer--project-file-candidate-cache
          (list :key key :ready nil :candidates nil))
    (let* ((buffer (current-buffer))
           (generation e-chat-composer--project-file-candidate-generation)
           (settled nil)
           timer
           request)
      (cl-labels
          ((current-p
            ()
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (eq request e-chat-composer--project-file-candidate-request)
                        (= generation
                           e-chat-composer--project-file-candidate-generation)
                        (equal key
                               (plist-get
                                e-chat-composer--project-file-candidate-cache
                                :key))))))
           (finish
            (candidates)
            (unless settled
              (setq settled t)
              (when (current-p)
                (with-current-buffer buffer
                  (setq e-chat-composer--project-file-candidate-cache
                        (list :key key
                              :ready t
                              :candidates candidates))
                  (setq e-chat-composer--project-file-candidate-request nil)))))
           (fail
            (_error)
            (unless settled
              (setq settled t)
              (when (current-p)
                (with-current-buffer buffer
                  (setq e-chat-composer--project-file-candidate-cache
                        (list :key key
                              :ready t
                              :candidates nil))
                  (setq e-chat-composer--project-file-candidate-request nil)))))
           (cancel
            ()
            (unless settled
              (setq settled t)
              (when (timerp timer)
                (cancel-timer timer))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (when (eq request
                            e-chat-composer--project-file-candidate-request)
                    (setq e-chat-composer--project-file-candidate-request nil)))))))
        (setq request
              (e-tools-request-create
               :cancel #'cancel
               :metadata (list :kind 'composer-file-candidates
                               :key key
                               :generation generation
                               :cancellable t)))
        (setq timer
              (run-at-time
               0 nil
               (lambda ()
                 (condition-case err
                     (when (current-p)
                       (with-current-buffer buffer
                         (finish (e-chat-composer--project-file-candidates-sync))))
                   (error (fail err))))))
        (setq e-chat-composer--project-file-candidate-request request)
        request))))

(defun e-chat-composer--project-file-candidates ()
  "Return cached project file candidates and refresh stale snapshots async."
  (let ((key (e-chat-composer--project-file-candidate-cache-key)))
    (if (e-chat-composer--project-file-candidate-cache-hit-p key)
        (plist-get e-chat-composer--project-file-candidate-cache :candidates)
      (e-chat-composer--project-file-candidates-refresh-start key)
      nil)))

(defun e-chat-composer--read-file-reference-text (path)
  "Return reference text for PATH, truncated when necessary."
  (with-temp-buffer
    (insert-file-contents-literally
     path nil 0 (1+ e-chat-file-reference-max-bytes))
    (let* ((content (buffer-string))
           (truncated (> (string-bytes content)
                         e-chat-file-reference-max-bytes)))
      (if truncated
          (concat
           (e-chat-composer--string-byte-prefix content e-chat-file-reference-max-bytes)
           "\n[File reference truncated]\n")
        content))))

(defun e-chat-composer--insert-file-reference (candidate)
  "Insert CANDIDATE as an inline file reference."
  (let* ((path (plist-get candidate :path))
         (label (plist-get candidate :label))
         (reference (list :uri (concat "file://" (expand-file-name path))
                          :label label
                          :text (e-chat-composer--read-file-reference-text path))))
    (e-chat-composer--insert-context-reference reference)))

(defun e-chat-composer--resource-candidate-label (entry)
  "Return composer @ candidate label for e:// resource ENTRY."
  (let ((description (e-store-entry-description entry)))
    (if (and (stringp description)
             (not (string-empty-p description)))
        (format "resource: %s - %s" (e-store-entry-uri entry) description)
      (format "resource: %s" (e-store-entry-uri entry)))))

(defun e-chat-composer--capability-candidate-label (capability)
  "Return composer @ candidate label for CAPABILITY."
  (let ((id (e-capability-id capability))
        (name (e-capability-name capability)))
    (if (and (stringp name)
             (not (string-empty-p name)))
        (format "capability: %s - %s" id name)
      (format "capability: %s" id))))

(defun e-chat-composer--resource-candidates ()
  "Return active e:// resource candidates for the current chat harness."
  (when e-chat-harness
    (mapcar (lambda (entry)
              (list :kind 'resource
                    :label (e-chat-composer--resource-candidate-label entry)
                    :entry entry))
            (e-store-list
             (e-harness-store e-chat-harness e-chat-session-id)))))

(defun e-chat-composer--capability-candidates ()
  "Return active capability candidates for the current chat harness."
  (when e-chat-harness
    (mapcar (lambda (capability)
              (list :kind 'capability
                    :label (e-chat-composer--capability-candidate-label capability)
                    :capability capability))
            (e-chat-service-active-capabilities e-chat-harness))))

(defun e-chat-composer--at-candidates ()
  "Return composer @ candidates for files, resources, and capabilities."
  (let ((file-candidates (e-chat-composer--project-file-candidates)))
    (append
     (mapcar (lambda (candidate)
               (let ((candidate (copy-sequence candidate)))
                 (plist-put candidate :kind 'file)
                 (plist-put candidate
                            :label
                            (format "file: %s" (plist-get candidate :label)))))
             file-candidates)
     (when (and (null file-candidates)
                (e-chat-composer--project-file-candidates-loading-p))
       (list (list :kind 'status :label "files: loading...")))
     (e-chat-composer--resource-candidates)
     (e-chat-composer--capability-candidates))))

(defun e-chat-composer--resource-reference-text (entry)
  "Return model-facing reference text for e:// resource ENTRY."
  (let ((description (e-store-entry-description entry))
        (uri (e-store-entry-uri entry)))
    (string-join
     (delq nil
           (list
            (format "Resource: %s" uri)
            (when (and (stringp description)
                       (not (string-empty-p description)))
              (format "Description: %s" description))
            ""
            (condition-case err
                (e-store-read-entry entry nil)
              (error
               (format "Read %s for the full resource. Reading now failed: %s"
                       uri
                       (error-message-string err))))))
     "\n")))

(defun e-chat-composer--insert-resource-reference (candidate)
  "Insert CANDIDATE as an inline e:// resource reference."
  (let* ((entry (plist-get candidate :entry))
         (reference (list :uri (e-store-entry-uri entry)
                          :label (e-store-entry-uri entry)
                          :text (e-chat-composer--resource-reference-text entry))))
    (e-chat-composer--insert-context-reference reference)))

(defun e-chat-composer--resource-line-for-capability (entry)
  "Return one lean resource listing line for e:// resource ENTRY."
  (let ((description (e-store-entry-description entry)))
    (if (and (stringp description)
             (not (string-empty-p description)))
        (format "- %s: %s" (e-store-entry-uri entry) description)
      (format "- %s" (e-store-entry-uri entry)))))

(defun e-chat-composer--capability-resource-lines (capability)
  "Return lean resource lines for active resources owned by CAPABILITY."
  (when e-chat-harness
    (let ((capability-id (symbol-name (e-capability-id capability))))
      (mapcar #'e-chat-composer--resource-line-for-capability
              (cl-remove-if-not
               (lambda (entry)
                 (equal (e-store-entry-capability entry) capability-id))
               (e-store-list
                (e-harness-store e-chat-harness e-chat-session-id)))))))

(defun e-chat-composer--capability-reference-text (capability)
  "Return model-facing reference text for CAPABILITY."
  (let* ((id (e-capability-id capability))
         (name (e-capability-name capability))
         (resource-lines (e-chat-composer--capability-resource-lines capability)))
    (string-join
     (delq nil
           (list
            (format "The user referenced capability `%s` with @." id)
            (when (and (stringp name)
                       (not (string-empty-p name)))
              (format "Capability name: %s" name))
            "Interpret this as: consider using the context, actions, tools, or resources provided by this capability."
            (when resource-lines
              (concat "Available resources:\n"
                      (string-join resource-lines "\n")))))
     "\n\n")))

(defun e-chat-composer--insert-capability-reference (candidate)
  "Insert CANDIDATE as an inline capability reference."
  (let* ((capability (plist-get candidate :capability))
         (id (e-capability-id capability))
         (reference (list :uri (format "e://%s" id)
                          :label (format "capability:%s" id)
                          :text (e-chat-composer--capability-reference-text capability))))
    (e-chat-composer--insert-context-reference reference)))

(defun e-chat-composer--insert-at-reference (candidate)
  "Insert selected composer @ CANDIDATE as an inline reference."
  (pcase (plist-get candidate :kind)
    ('file (e-chat-composer--insert-file-reference candidate))
    ('resource (e-chat-composer--insert-resource-reference candidate))
    ('capability (e-chat-composer--insert-capability-reference candidate))
    ('status (e-chat-composer--insert-literal-prefix "@"))
    (_ (e-chat-composer--insert-file-reference candidate))))

(defun e-chat-composer--prompt-candidates ()
  "Return prompt completion candidates for the active chat harness."
  (when e-chat-harness
    (mapcar (lambda (prompt)
              (list :label (e-prompt-spec-name prompt)
                    :prompt prompt))
            (e-chat-service-prompt-catalog e-chat-harness))))

(defun e-chat-composer--collect-prompt-arguments (prompt)
  "Read arguments for PROMPT and return an alist."
  (let (arguments)
    (dolist (parameter (e-prompt-spec-parameters prompt))
      (let* ((name (e-prompt-parameter-name parameter))
             (default (e-prompt-parameter-default parameter))
             (description (e-prompt-parameter-description parameter))
             (value (read-string
                     (if default
                         (format "%s (%s): " description default)
                       (format "%s: " description))
                     nil
                     nil
                     default)))
        (unless (and (not (e-prompt-parameter-required parameter))
                     (string-empty-p value))
          (push (cons name value) arguments))))
    (nreverse arguments)))

(defvar-local e-chat-composer--inline-completion-overlay nil)

(defun e-chat-composer--inline-completion-delete ()
  "Delete the active composer inline completion overlay."
  (when (overlayp e-chat-composer--inline-completion-overlay)
    (delete-overlay e-chat-composer--inline-completion-overlay))
  (setq e-chat-composer--inline-completion-overlay nil))

(defun e-chat-composer--inline-completion-fuzzy-match-p (filter label)
  "Return non-nil when LABEL contains FILTER characters in order.
Matching is case-insensitive."
  (let ((needle (downcase (or filter "")))
        (haystack (downcase (or label "")))
        (start 0)
        (index 0)
        found)
    (catch 'missing
      (while (< index (length needle))
        (setq found (string-match-p
                     (regexp-quote (char-to-string (aref needle index)))
                     haystack
                     start))
        (unless found
          (throw 'missing nil))
        (setq start (1+ found))
        (setq index (1+ index)))
      t)))

(defun e-chat-composer--inline-completion-matches (candidates filter)
  "Return CANDIDATES whose labels fuzzily match FILTER."
  (if (string-empty-p filter)
      candidates
    (cl-remove-if-not
     (lambda (candidate)
       (e-chat-composer--inline-completion-fuzzy-match-p
        filter
        (plist-get candidate :label)))
     candidates)))

(defun e-chat-composer--inline-completion-render (prompt candidates index filter)
  "Render PROMPT, CANDIDATES, INDEX, and FILTER as popup text."
  (let ((rows (cl-subseq candidates 0 (min 8 (length candidates)))))
    (concat
     prompt
     filter
     "\n"
     (mapconcat
      (lambda (candidate)
        (let ((label (plist-get candidate :label)))
          (format "%s %s"
                  (if (eq candidate (nth index candidates)) ">" " ")
                  label)))
      rows
      "\n"))))

(defun e-chat-composer--inline-completion-show (prompt candidates index filter)
  "Show an inline completion popup at point."
  (e-chat-composer--inline-completion-delete)
  (setq e-chat-composer--inline-completion-overlay (make-overlay (point) (point)))
  (overlay-put e-chat-composer--inline-completion-overlay
               'after-string
               (propertize
                (e-chat-composer--inline-completion-render
                 prompt
                 candidates
                 index
                 filter)
                'face 'shadow)))

(defun e-chat-composer--inline-completion-select (prompt candidates)
  "Select one of CANDIDATES through an inline composer popup."
  (when candidates
    (let ((filter "")
          (index 0))
      (unwind-protect
          (catch 'selected
            (while t
              (let ((matches (e-chat-composer--inline-completion-matches
                              candidates
                              (downcase filter))))
                (setq index (min index (max 0 (1- (length matches)))))
                (e-chat-composer--inline-completion-show prompt matches index filter)
                (let ((key (read-key)))
                  (cond
                   ((memq key '(return ?\r))
                    (when-let ((candidate (nth index matches)))
                      (throw 'selected candidate)))
                   ((memq key '(?\C-g escape))
                    (signal 'quit nil))
                   ((memq key '(?\C-n down tab))
                    (setq index (if matches
                                    (mod (1+ index) (length matches))
                                  0)))
                   ((memq key '(?\C-p up backtab))
                    (setq index (if matches
                                    (mod (1- index) (length matches))
                                  0)))
                   ((memq key '(?\C-h ?\177 backspace delete))
                    (unless (string-empty-p filter)
                      (setq filter (substring filter 0 -1))
                      (setq index 0)))
                   ((and (characterp key)
                         (>= key 32)
                         (/= key 127))
                    (setq filter (concat filter (string key)))
                    (setq index 0)))))))
        (e-chat-composer--inline-completion-delete)))))

(defun e-chat-composer-bang ()
  "Run a leading composer ! command and insert its output as context."
  (interactive)
  (if (not (e-chat-composer--composer-leading-prefix-p))
      (e-chat-composer--self-insert-prefix)
    (condition-case nil
        (let ((command (read-shell-command "! ")))
          (if (string-empty-p (string-trim command))
              (e-chat-composer--insert-literal-prefix "!")
            (let* ((buffer (current-buffer))
                   (pending (e-chat-composer--insert-context-reference
                             (list :uri (e-chat-composer--command-uri command)
                                   :label (format "$ %s (running)" command)
                                   :text "Command output is still running."
                                   :pending t
                                   :command command)))
                   (reference-id (plist-get pending :id))
                   request)
              (setq
               request
               (e-chat-composer--run-shell-command-start
                command
                (e-chat-composer-project-root)
                :on-done
                (lambda (result)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (e-chat-composer--forget-pending-command-request reference-id)
                      (e-chat-composer--replace-context-reference
                       reference-id
                       (e-chat-composer--command-output-reference command result)))))
                :on-error
                (lambda (err)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (e-chat-composer--forget-pending-command-request reference-id)
                      (e-chat-composer--replace-context-reference
                       reference-id
                       (e-chat-composer--command-output-reference
                        command
                        (list :output (error-message-string err)
                              :exit "error"))))))))
              (when request
                (e-chat-composer--remember-pending-command-request
                 reference-id
                 request)))))
      (quit (e-chat-composer--insert-literal-prefix "!")))))

(defun e-chat-composer-at ()
  "Insert a project file reference from a composer @ prefix."
  (interactive)
  (if (not (e-chat-composer--composer-word-boundary-prefix-p))
      (e-chat-composer--self-insert-prefix)
    (condition-case nil
        (if-let ((candidate (e-chat-composer--inline-completion-select
                             "@ reference: "
                             (e-chat-composer--at-candidates))))
            (e-chat-composer--insert-at-reference candidate)
          (e-chat-composer--insert-literal-prefix "@"))
      (quit (e-chat-composer--insert-literal-prefix "@")))))

(defun e-chat-composer-slash ()
  "Expand a capability prompt from a composer / prefix."
  (interactive)
  (if (not (e-chat-composer--composer-leading-prefix-p))
      (e-chat-composer--self-insert-prefix)
    (condition-case nil
        (if-let* ((candidate (e-chat-composer--inline-completion-select
                              "/ prompt: "
                              (e-chat-composer--prompt-candidates)))
                  (prompt (plist-get candidate :prompt)))
            (condition-case err
                (insert (e-prompt-render
                         prompt
                         (e-chat-composer--collect-prompt-arguments prompt)))
              (error (user-error "%s" (error-message-string err))))
          (e-chat-composer--insert-literal-prefix "/"))
      (quit (e-chat-composer--insert-literal-prefix "/")))))

(defun e-chat-composer--active-region-p ()
  "Return non-nil when the current buffer has a meaningful active region."
  (and mark-active
       (mark t)
       (/= (region-beginning) (region-end))))

(defun e-chat-composer--last-content-line-number ()
  "Return the last content line number in the current buffer."
  (save-excursion
    (goto-char (point-max))
    (if (and (bolp) (not (bobp)))
        (line-number-at-pos (1- (point)))
      (line-number-at-pos))))

(defun e-chat-composer--line-range-text (start-line end-line)
  "Return text from START-LINE through END-LINE, preserving final newlines."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- start-line))
    (let ((start (point)))
      (forward-line (1+ (- end-line start-line)))
      (buffer-substring-no-properties start (point)))))

(defun e-chat-composer--source-uri ()
  "Return a resource URI for the current source buffer."
  (if buffer-file-name
      (concat "file://" (expand-file-name buffer-file-name))
    (concat "buffer://" (buffer-name))))

(defun e-chat-composer--source-label (start-line end-line &optional focus-line)
  "Return a compact source label for START-LINE through END-LINE.
When FOCUS-LINE is non-nil, make that point line the primary label and keep
the surrounding line range as context."
  (format "%s:%s"
          (if buffer-file-name
              (file-name-nondirectory buffer-file-name)
            (buffer-name))
          (cond
           ((and focus-line (= start-line end-line))
            (number-to-string focus-line))
           (focus-line
            (format "%d (context %d-%d)"
                    focus-line
                    start-line
                    end-line))
           ((= start-line end-line)
            (number-to-string start-line))
           (t
            (format "%d-%d" start-line end-line)))))

(defun e-chat-composer--capture-context-reference ()
  "Capture the current point or active region as a chat context reference."
  (e-chat-composer--capture-source-reference))

;;;###autoload
(defun e-chat-capture-source-reference (&optional line-radius)
  "Capture the current point or active region as a chat source reference.
LINE-RADIUS controls the number of lines around point used when no region is
active.  It defaults to the historical chat context radius of two lines."
  (e-chat-composer--capture-source-reference line-radius))

(defun e-chat-composer--capture-source-reference (&optional line-radius)
  "Capture the current point or active region as a chat source reference.
This is the composer implementation behind the public source-reference port."
  (let* ((point-line (line-number-at-pos))
         (has-region (e-chat-composer--active-region-p))
         (line-radius (or line-radius 2))
         (start-line (if has-region
                         (line-number-at-pos (region-beginning))
                       (max 1 (- point-line line-radius))))
         (end-line (if has-region
                       (line-number-at-pos
                        (max (region-beginning) (1- (region-end))))
                     (min (e-chat-composer--last-content-line-number)
                          (+ point-line line-radius))))
         (text (if has-region
                   (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))
                 (e-chat-composer--line-range-text start-line end-line))))
    (let ((reference
           (list :uri (e-chat-composer--source-uri)
                 :label (e-chat-composer--source-label
                         start-line
                         end-line
                         (and (not has-region) point-line))
                 :text text
                 :start-line start-line
                 :end-line end-line
                 :point-line point-line)))
      (unless has-region
        (setq reference (plist-put reference :point-context t)))
      reference)))

(defun e-chat-composer--capture-context-reference-for-command ()
  "Capture a chat context reference and clear source buffer selection."
  (prog1 (e-chat-composer--capture-context-reference)
    (when (e-chat-composer--active-region-p)
      (deactivate-mark t))))

(defun e-chat-composer--next-context-reference-id ()
  "Return the next display-local context reference id."
  (setq e-chat-composer--context-reference-counter
        (1+ e-chat-composer--context-reference-counter))
  (format "ref-%d" e-chat-composer--context-reference-counter))

(defun e-chat-composer--context-reference-with-id (reference)
  "Return REFERENCE with a stable id."
  (let ((reference (copy-sequence reference)))
    (unless (plist-get reference :id)
      (setq reference
            (plist-put reference
                       :id
                       (e-chat-composer--next-context-reference-id))))
    reference))

(defun e-chat-composer--insert-context-reference (reference)
  "Insert REFERENCE as a protected inline atom in the composer."
  (if (not (e-chat-composer--composer-buffer-p))
      (with-current-buffer (e-chat-composer--ensure-composer)
        (e-chat-composer--insert-context-reference reference))
    (unless (e-chat-composer--composer-active-p)
      (e-chat-composer--insert-composer))
    (unless (e-chat-composer--point-in-composer-p)
      (goto-char (point-max)))
    (let* ((reference (e-chat-composer--context-reference-with-id reference))
           (display (format "@[%s]" (plist-get reference :label)))
           (start (point))
           (inhibit-read-only t))
      (insert display)
      (add-text-properties
       start
       (point)
       `(read-only t
         e-chat-context-reference ,reference
         font-lock-face e-chat-context-reference-face
         help-echo ,(plist-get reference :uri)
         front-sticky nil
         rear-nonsticky t))
      reference)))

(defun e-chat-composer--replace-context-reference (old-id new-reference)
  "Replace inline composer reference OLD-ID with NEW-REFERENCE."
  (when (and (e-chat-composer--composer-active-p) old-id)
    (let ((position (marker-position e-chat-composer--composer-start-marker))
          (end (point-max))
          bounds
          old-reference)
      (while (and (< position end) (not bounds))
        (let ((reference (get-text-property position 'e-chat-context-reference)))
          (if (and reference (equal (plist-get reference :id) old-id))
              (setq bounds (e-chat-composer--context-reference-bounds-at position)
                    old-reference reference)
            (setq position
                  (or (next-single-property-change
                       position 'e-chat-context-reference nil end)
                      end)))))
      (when bounds
        (let* ((new-reference (copy-sequence new-reference))
               (new-reference (plist-put new-reference :id old-id))
               (display (format "@[%s]" (plist-get new-reference :label)))
               (inhibit-read-only t))
          (when (plist-get old-reference :pending)
            (setq new-reference (plist-put new-reference :pending nil)))
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert display)
            (add-text-properties
             (car bounds)
             (point)
             `(read-only t
               e-chat-context-reference ,new-reference
               font-lock-face e-chat-context-reference-face
               help-echo ,(plist-get new-reference :uri)
               front-sticky nil
               rear-nonsticky t)))
          new-reference)))))

(defun e-chat-composer--pending-context-reference-p (reference)
  "Return non-nil when REFERENCE is still being populated."
  (and (plist-get reference :pending) t))

(defun e-chat-composer--composer-pending-references ()
  "Return pending inline references in the current composer."
  (when (e-chat-composer--composer-active-p)
    (cl-remove-if-not
     #'e-chat-composer--pending-context-reference-p
     (plist-get (e-chat-composer--composer-document) :references))))

(defun e-chat-composer--remember-pending-command-request (reference-id request)
  "Remember cancellable REQUEST for pending command REFERENCE-ID."
  (when (and reference-id request)
    (push (cons reference-id request) e-chat-composer--pending-command-requests)))

(defun e-chat-composer--forget-pending-command-request (reference-id)
  "Forget pending command request for REFERENCE-ID."
  (setq e-chat-composer--pending-command-requests
        (assoc-delete-all reference-id e-chat-composer--pending-command-requests)))

(defun e-chat-composer--cancel-pending-command-references ()
  "Cancel pending command references owned by this chat buffer."
  (dolist (entry e-chat-composer--pending-command-requests)
    (ignore-errors
      (e-tools-cancel-request (cdr entry))))
  (setq e-chat-composer--pending-command-requests nil))

(defun e-chat-composer--context-reference-bounds-at (position)
  "Return bounds of the inline context reference adjacent to POSITION."
  (when (e-chat-composer--composer-active-p)
    (let* ((start-limit (marker-position e-chat-composer--composer-start-marker))
           (end-limit (point-max))
           (probe (cond
                   ((and (< position end-limit)
                         (get-text-property
                          position
                          'e-chat-context-reference))
                    position)
                   ((and (> position start-limit)
                         (get-text-property
                          (1- position)
                          'e-chat-context-reference))
                    (1- position)))))
      (when probe
        (let ((reference (get-text-property probe 'e-chat-context-reference))
              (start probe)
              (end (1+ probe)))
          (while (and (> start start-limit)
                      (equal (get-text-property
                              (1- start)
                              'e-chat-context-reference)
                             reference))
            (setq start (1- start)))
          (while (and (< end end-limit)
                      (equal (get-text-property
                              end
                              'e-chat-context-reference)
                             reference))
            (setq end (1+ end)))
          (cons start end))))))

(defun e-chat-composer--delete-context-reference-at (position)
  "Delete the inline context reference adjacent to POSITION, when present."
  (when-let ((bounds (e-chat-composer--context-reference-bounds-at position)))
    (let ((inhibit-read-only t))
      (delete-region (car bounds) (cdr bounds)))
    t))

(defun e-chat-composer--delete-context-reference-before-point ()
  "Delete the context reference immediately before point, when present."
  (when (and (e-chat-composer--composer-active-p)
             (> (point) (marker-position e-chat-composer--composer-start-marker))
             (get-text-property (1- (point)) 'e-chat-context-reference))
    (e-chat-composer--delete-context-reference-at (1- (point)))))

(defun e-chat-composer--delete-context-reference-after-point ()
  "Delete the context reference immediately after point, when present."
  (when (and (e-chat-composer--composer-active-p)
             (< (point) (point-max))
             (get-text-property (point) 'e-chat-context-reference))
    (e-chat-composer--delete-context-reference-at (point))))

(defun e-chat-delete-backward-char (arg &optional killp)
  "Delete backward ARG chars, removing a preceding context atom as a unit.
KILLP is passed through to `delete-char' for normal text."
  (interactive "p\nP")
  (unless (and (= arg 1)
               (e-chat-composer--delete-context-reference-before-point))
    (delete-char (- arg) killp)))

(defun e-chat-delete-forward-char (arg &optional killp)
  "Delete forward ARG chars, removing a following context atom as a unit.
KILLP is passed through to `delete-char' for normal text."
  (interactive "p\nP")
  (unless (and (= arg 1)
               (e-chat-composer--delete-context-reference-after-point))
    (delete-char arg killp)))

(defun e-chat-kill-region-or-backward-word (arg)
  "Kill the active region, or ARG words backward when no region is active.
This gives the composer the readline-style \\`C-w' users expect in an
input field while preserving standard `kill-region' behaviour on a
selection."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun e-chat-composer--xml-attribute-escape (value)
  "Return VALUE escaped for a compact XML-like attribute."
  (let ((text (format "%s" (or value ""))))
    (setq text (replace-regexp-in-string "&" "&amp;" text t t))
    (setq text (replace-regexp-in-string "\"" "&quot;" text t t))
    (setq text (replace-regexp-in-string "<" "&lt;" text t t))
    (replace-regexp-in-string ">" "&gt;" text t t)))

(defun e-chat-composer--reference-placeholder (reference)
  "Return inline model-facing placeholder for REFERENCE."
  (format "<reference id=\"%s\" label=\"%s\">"
          (e-chat-composer--xml-attribute-escape (plist-get reference :id))
          (e-chat-composer--xml-attribute-escape (plist-get reference :label))))

(defun e-chat-reference-placeholder (reference)
  "Return the model-facing inline placeholder for REFERENCE."
  (e-chat-composer--reference-placeholder reference))

(defun e-chat-composer--reference-text-lines (text)
  "Return TEXT split into content lines, ignoring one trailing newline."
  (let ((lines (split-string (replace-regexp-in-string
                              "\r" "" (or text "") t t)
                             "\n")))
    (if (and (cdr lines)
             (string-empty-p (car (last lines))))
        (butlast lines)
      lines)))

(defun e-chat-composer--point-context-reference-body (reference)
  "Return a line-numbered body for point-context REFERENCE."
  (let* ((start-line (plist-get reference :start-line))
         (end-line (plist-get reference :end-line))
         (point-line (plist-get reference :point-line))
         (width (length (number-to-string (or end-line start-line 0))))
         (line-number start-line)
         lines)
    (dolist (line (e-chat-composer--reference-text-lines (plist-get reference :text)))
      (push (format "%s %s | %s"
                    (if (= line-number point-line) ">" " ")
                    (format (format "%%%dd" width) line-number)
                    line)
            lines)
      (setq line-number (1+ line-number)))
    (format "Context lines %d-%d; focused line %d:\n%s"
            start-line
            end-line
            point-line
            (string-join (nreverse lines) "\n"))))

(defun e-chat-composer--reference-body (reference)
  "Return model-facing body text for REFERENCE."
  (if (plist-get reference :point-context)
      (e-chat-composer--point-context-reference-body reference)
    (plist-get reference :text)))

(defun e-chat-composer--reference-section-entry (reference)
  "Return model-facing reference body for REFERENCE."
  (format "[%s] %s (%s)\n%s"
          (plist-get reference :id)
          (plist-get reference :label)
          (plist-get reference :uri)
          (e-chat-composer--reference-body reference)))

(defun e-chat-format-reference-prompt (text references)
  "Return TEXT with model-facing REFERENCES appended."
  (e-chat-composer--format-reference-prompt text references))

(defun e-chat-composer--format-reference-prompt (text references)
  "Return TEXT with model-facing REFERENCES appended.
This is the composer implementation behind the public formatting port."
  (let ((references (delq nil references)))
    (if references
        (string-trim
         (concat
          text
          "\n\nReferences:\n"
          (mapconcat #'e-chat-composer--reference-section-entry
                     references
                     "\n\n")))
      (string-trim text))))

(defun e-chat-composer--composer-document ()
  "Return composer prompt text and ordered inline reference records."
  (unless (e-chat-composer--composer-active-p)
    (user-error "No active e chat composer"))
  (let ((position (marker-position e-chat-composer--composer-start-marker))
        (end (point-max))
        segments
        references)
    (while (< position end)
      (let ((reference (get-text-property position 'e-chat-context-reference)))
        (if reference
            (let ((next (or (next-single-property-change
                             position 'e-chat-context-reference nil end)
                            end)))
              (push (e-chat-composer--reference-placeholder reference) segments)
              (push (copy-tree reference) references)
              (setq position next))
          (let ((next (or (next-single-property-change
                           position 'e-chat-context-reference nil end)
                          end)))
            (push (buffer-substring-no-properties position next) segments)
            (setq position next)))))
    (list :text (string-trim (apply #'concat (nreverse segments)))
          :references (nreverse references))))

(defun e-chat-composer--composer-submission ()
  "Return submit-ready prompt and ordered reference metadata."
  (let* ((document (e-chat-composer--composer-document))
         (text (plist-get document :text))
         (references (plist-get document :references)))
    (when-let ((pending (e-chat-composer--composer-pending-references)))
      (user-error "Command output still running: %s"
                  (mapconcat (lambda (reference)
                               (or (plist-get reference :label)
                                   (plist-get reference :id)
                                   "pending command"))
                             pending
                             ", ")))
    (when references
      (setq text (e-chat-composer--format-reference-prompt text references)))
    (list :prompt text :references references)))


(defun e-chat-composer-text (&optional buffer)
  "Return editable composer text from BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--composer-text)))

(defun e-chat-composer-glyph ()
  "Return the prompt glyph used at the composer boundary."
  e-chat-composer--composer-glyph)

(defun e-chat-composer-separator ()
  "Return the visual separator used by the composer."
  e-chat-composer--composer-separator)

(defun e-chat-composer-submission (&optional buffer)
  "Return the structured submission extracted from BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--composer-submission)))

(defun e-chat-composer-document (&optional buffer)
  "Return the editable document projection from BUFFER.
The projection contains plain text and ordered reference metadata, without
exposing the composer's markers or completion state."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--composer-document)))

(defun e-chat-composer-insert (&optional text preserve-focus)
  "Ensure the current buffer has a composer, optionally initializing TEXT."
  (e-chat-composer--insert-composer text preserve-focus))

(defun e-chat-composer-delete ()
  "Delete the current buffer's composer region or paired composer buffer."
  (interactive)
  (e-chat-composer--delete-composer))

(defun e-chat-composer-insert-context-reference (reference)
  "Insert REFERENCE into the current composer."
  (e-chat-composer--insert-context-reference reference))

(defun e-chat-composer-delete-context-reference-at (&optional position)
  "Delete the context reference at POSITION, if any."
  (e-chat-composer--delete-context-reference-at (or position (point))))

(defun e-chat-composer-disable-modal-editing (&optional buffer)
  "Disable modal editing in the current chat composer or transcript."
  (with-current-buffer (or buffer (current-buffer))
    (when (fboundp 'evil-local-mode)
      (evil-local-mode -1))
    (when (boundp 'evil-local-mode)
      (setq-local evil-local-mode nil))
    (when (boundp 'evil-state)
      (setq-local evil-state nil))))

(defun e-chat-composer-disable-completion (&optional buffer)
  "Disable completion sources and UI in BUFFER's composer."
  (with-current-buffer (or buffer (current-buffer))
    (when (fboundp 'company-mode)
      (company-mode -1))
    (when (fboundp 'corfu-mode)
      (corfu-mode -1))
    (when (fboundp 'auto-complete-mode)
      (auto-complete-mode -1))
    (setq-local completion-at-point-functions nil)
    (setq-local completion-in-region-function #'ignore)
    (setq-local company-backends nil)
    (setq-local company-idle-delay nil)))

(defun e-chat-composer-active-region-p (&optional buffer)
  "Return non-nil when BUFFER has a meaningful active region."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--active-region-p)))

(defun e-chat-composer-start-position (&optional buffer)
  "Return the beginning position of BUFFER's editable composer text."
  (with-current-buffer (or buffer (current-buffer))
    (and (markerp e-chat-composer--composer-start-marker)
         (marker-position e-chat-composer--composer-start-marker))))

(defun e-chat-composer-reset ()
  "Reset transient editing state in the current composer buffer."
  (when (markerp e-chat-composer--composer-start-marker)
    (set-marker e-chat-composer--composer-start-marker nil))
  (when (markerp e-chat-composer--queue-start-marker)
    (set-marker e-chat-composer--queue-start-marker nil))
  (when (markerp e-chat-composer--queue-end-marker)
    (set-marker e-chat-composer--queue-end-marker nil))
  (setq-local e-chat-composer--composer-scroll-needed nil)
  (setq-local e-chat-composer--composer-scroll-suppressed nil)
  (setq-local e-chat-composer--context-reference-counter 0)
  (setq-local e-chat-composer--pending-command-requests nil)
  (setq-local e-chat-composer--project-file-candidate-cache nil)
  (setq-local e-chat-composer--project-file-candidate-request nil)
  (setq-local e-chat-composer--project-file-candidate-generation 0)
  t)

;; Public composer port.  These operations keep editing state and asynchronous
;; candidate work behind the composer owner while allowing the chat facade and
;; embedding shells to compose it without importing private buffer fields.

(defun e-chat-composer-initialize (&optional text preserve-focus buffer)
  "Initialize BUFFER's editable composer with TEXT.
When PRESERVE-FOCUS is non-nil, keep the previous point when possible."
  (with-current-buffer (or buffer (current-buffer))
    (unless (e-chat-composer--composer-buffer-p)
      (user-error "This buffer is not an e chat composer"))
    (let ((inhibit-read-only t)
          (e-chat-composer--composer-scroll-suppressed t)
          (saved-point (point)))
      (erase-buffer)
      (setq e-chat-composer--queue-start-marker nil)
      (setq e-chat-composer--queue-end-marker nil)
      (e-chat-composer--insert-queued-prompts)
      ;; The composer has its own window, so its prompt glyph is sufficient
      ;; chrome at the input boundary.
      (e-chat-transcript-insert-protected e-chat-composer--composer-glyph
                                          'e-chat-composer-face
                                          '(e-chat-composer t))
      (setq e-chat-composer--composer-start-marker (point-marker))
      (set-marker-insertion-type e-chat-composer--composer-start-marker nil)
      (when text
        (insert (e-chat-composer--sanitize-composer-text text)))
      (setq e-chat-composer--composer-scroll-needed nil)
      (goto-char (if preserve-focus
                   (min saved-point (point-max))
                   (point-max))))))

(defun e-chat-composer-project-root (&optional directory)
  "Return the composer-compatible project root for DIRECTORY."
  (e-chat-composer--project-root directory))

(defun e-chat-composer-active-p (&optional buffer)
  "Return non-nil when BUFFER has an active editable composer."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--composer-active-p)))

(defun e-chat-composer-point-in-composer-p (&optional position buffer)
  "Return non-nil when POSITION is inside BUFFER's editable text."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--point-in-composer-p position)))

(defun e-chat-composer-ensure (&optional buffer)
  "Ensure and return BUFFER's composer surface."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--ensure-composer)))

(defun e-chat-composer-insert-queued-prompts (&optional buffer)
  "Insert queued prompt presentation in BUFFER's composer."
  (let* ((surface (or buffer (current-buffer)))
         (transcript (e-chat-surface-transcript-buffer surface))
         (composer (and (buffer-live-p transcript)
                        (e-chat-surface-composer-buffer transcript))))
    (when (buffer-live-p composer)
      (with-current-buffer composer
        ;; Reinitialize through the owning composer so queue chrome is rebuilt
        ;; under its read-only boundary while preserving the user's draft and
        ;; point.  The low-level insertion helper remains private to this
        ;; initialization path.
        (e-chat-composer-initialize
         (e-chat-composer--composer-text) t)))))

(defun e-chat-composer-cancel-pending-references (&optional buffer)
  "Cancel pending command references owned by BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--cancel-pending-command-references)))

(defun e-chat-composer-cancel-file-candidate-refresh (&optional buffer)
  "Cancel BUFFER's pending project-file candidate refresh."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--cancel-project-file-candidate-refresh)))

(defun e-chat-composer-capture-context-reference-for-command (&optional buffer)
  "Capture the active source context for a command in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--capture-context-reference-for-command)))

(defun e-chat-composer-mark-scroll-needed (begin end length &optional buffer)
  "Mark BUFFER's composer for a post-edit visibility adjustment."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--mark-composer-scroll-needed begin end length)))

(defun e-chat-composer-pre-command (&optional buffer)
  "Run the composer pre-command transition for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (e-chat-composer--pre-command)))

(defun e-chat-composer--post-command (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when e-chat-composer--composer-scroll-needed
      (setq e-chat-composer--composer-scroll-needed nil)
      (when (e-chat-composer--point-in-composer-p)
        (e-chat-composer--scroll-composer-edit-into-view)))))

(provide 'e-chat-composer)

;;; e-chat-composer.el ends here

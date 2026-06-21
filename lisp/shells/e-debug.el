;;; e-debug.el --- Standing debug agent shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Standing debug agent session support.  Presentation commands are added in
;; later feature slices; this module owns the default debug harness lookup and
;; stable session resolver.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-chat)
(require 'e-context-inspection)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-session)
(require 'e-shells)
(require 'e-startup)

(declare-function posframe-hide "ext:posframe")
(declare-function posframe-show "ext:posframe")
(declare-function posframe-workable-p "ext:posframe")

(defgroup e-debug nil
  "Standing debug agent shell."
  :group 'e
  :prefix "e-debug-")

(defcustom e-debug-display-strategy 'popup
  "Display strategy used by `e-debug'."
  :type '(choice (const :tag "Open in a floating popup" popup)
                 (const :tag "Open in a new tab" tab)
                 (const :tag "Pop to another window" window)
                 (const :tag "Use current window" current-window))
  :group 'e-debug)

(defcustom e-debug-tab-name "e-debug"
  "Name of the tab used by the standing debug session."
  :type 'string
  :group 'e-debug)

(defcustom e-debug-popup-width 0.82
  "Preferred floating debug popup width.
A fractional value is interpreted relative to the selected frame width."
  :type '(choice (number :tag "Frame fraction") (integer :tag "Columns"))
  :group 'e-debug)

(defcustom e-debug-popup-height 0.72
  "Preferred floating debug popup height.
A fractional value is interpreted relative to the selected frame height."
  :type '(choice (number :tag "Frame fraction") (integer :tag "Lines"))
  :group 'e-debug)

(defconst e-debug-default-prompt
  "Debug what just happened here."
  "Default prompt used by `e-debug-here' when the question is empty.")

(defvar e-debug--session-id nil
  "Cached standing debug session id.")

(defvar e-debug--popup-buffer nil
  "Buffer currently shown as the floating debug popup.")

(defvar e-debug--popup-frame nil
  "Frame currently used by the floating debug popup.")

(defvar e-debug--notification-harness nil
  "Harness currently subscribed for debug completion notifications.")

(defvar e-debug--notification-subscription nil
  "Harness subscription used for debug completion notifications.")

(defvar e-debug--notification-session-id nil
  "Session id currently subscribed for debug completion notifications.")

(defvar e-debug-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'e-debug--dismiss-popup-or-keyboard-quit)
    map)
  "Keymap active while a debug chat buffer is shown as a popup.")

(define-minor-mode e-debug-popup-mode
  "Minor mode for the floating debug popup presentation."
  :init-value nil
  :lighter " e-debug-popup"
  :keymap e-debug-popup-mode-map)

(defun e-debug--popup-available-p ()
  "Return non-nil when a floating debug popup can be displayed."
  (and (require 'posframe nil t)
       (fboundp 'posframe-workable-p)
       (posframe-workable-p)
       (fboundp 'posframe-show)
       (fboundp 'posframe-hide)))

(defun e-debug--popup-dimension (value frame-size)
  "Return concrete popup dimension for VALUE against FRAME-SIZE."
  (cond
   ((and (numberp value) (> value 0) (< value 1))
    (max 20 (floor (* frame-size value))))
   ((and (integerp value) (> value 0))
    value)
   (t
    (max 20 (floor (* frame-size 0.7))))))

(defun e-debug--default-harness ()
  "Return the configured default debug harness."
  (let ((instance (e-harness-instance-default :kind 'debug)))
    (unless instance
      (user-error "No e debug harness registered"))
    (e-harness-instance-get-or-create (e-harness-instance-id instance))))

(defun e-debug--session-metadata ()
  "Return metadata for the standing debug session."
  (list :name "Debug Agent"
        :source 'e-debug
        :project-root (file-name-as-directory
                       (expand-file-name default-directory))))

(defun e-debug--debug-session-p (session)
  "Return non-nil when SESSION is the standing debug session."
  (eq (plist-get (plist-get session :metadata) :source) 'e-debug))

(defun e-debug--session-exists-p (harness session-id)
  "Return non-nil when SESSION-ID exists in HARNESS as the debug session."
  (when session-id
    (when-let ((session (e-session-get (e-harness-sessions harness) session-id)))
      (e-debug--debug-session-p session))))

(defun e-debug--find-session-id (harness)
  "Return an existing standing debug session id in HARNESS, or nil."
  (catch 'found
    (dolist (session (e-harness-session-list harness))
      (when (e-debug--debug-session-p session)
        (throw 'found (plist-get session :id))))
    nil))

(defun e-debug--normalize-question (question)
  "Return QUESTION, using `e-debug-default-prompt' for blank input."
  (let ((question (string-trim (or question ""))))
    (if (string-empty-p question)
        e-debug-default-prompt
      question)))

(defun e-debug--cursor-uri ()
  "Return a compact URI for the current cursor location."
  (if buffer-file-name
      (concat "file://" (expand-file-name buffer-file-name))
    (format "buffer://%s" (buffer-name))))

(defun e-debug--cursor-label ()
  "Return a compact label for the current cursor location."
  (if buffer-file-name
      (abbreviate-file-name (expand-file-name buffer-file-name))
    (buffer-name)))

(defun e-debug--cursor-location ()
  "Return a compact description of the current cursor location, or nil."
  (unless (minibufferp)
    (condition-case nil
        (let* ((line (line-number-at-pos))
               (column (1+ (current-column)))
               (region
                (when (use-region-p)
                  (format "; region %d-%d"
                          (line-number-at-pos (region-beginning))
                          (line-number-at-pos
                           (max (region-beginning) (1- (region-end))))))))
          (format "%s:%d:%d (%s)%s"
                  (e-debug--cursor-label)
                  line
                  column
                  (e-debug--cursor-uri)
                  (or region "")))
      (error nil))))

(defun e-debug--inspection-harness ()
  "Return the harness currently under inspection, or nil."
  (plist-get (e-debug--inspection-target) :harness))

(defun e-debug--inspection-target ()
  "Return the harness/session currently under inspection, or nil."
  (cond
   ((and (derived-mode-p 'e-chat-mode)
         e-chat-harness
         e-chat-session-id)
    (list :harness e-chat-harness
          :session-id e-chat-session-id
          :source 'chat-buffer))
   (t
    (when-let ((harness (ignore-errors (e-chat--default-harness))))
      (list :harness harness
            :source 'default-chat)))))

(defun e-debug--recent-failure-target (harness)
  "Return the newest recent failure target from HARNESS."
  (when harness
    (car (ignore-errors
           (e-context-inspection-recent-failures :harness harness :limit 1)))))

(defun e-debug--source-reference-location (reference)
  "Return a compact location string for SOURCE REFERENCE."
  (when reference
    (string-trim
     (format "%s (%s)"
             (or (plist-get reference :label) "source")
             (or (plist-get reference :uri) "")))))

(cl-defun e-debug--capture
    (&key question inspection-harness inspection-session-id source-reference)
  "Capture prompt and references for `e-debug-here'."
  (let* ((question (e-debug--normalize-question question))
         (cursor-location (or (e-debug--source-reference-location source-reference)
                              (e-debug--cursor-location)))
         (recent-failure (unless inspection-session-id
                           (e-debug--recent-failure-target inspection-harness)))
         (prompt-text (if cursor-location
                          (format "Cursor: %s\n\n%s" cursor-location question)
                        question)))
    (list :prompt (string-trim prompt-text)
          :references nil
          :metadata (list :source 'e-debug-here
                          :inspection-session-id
                          (or inspection-session-id
                              (plist-get recent-failure :session-id))
                          :inspection-turn-id
                          (plist-get recent-failure :turn-id)))))

(defun e-debug--ensure-session (&optional harness)
  "Return the standing debug session id, creating it in HARNESS when needed."
  (let ((harness (or harness (e-debug--default-harness))))
    (cond
     ((e-debug--session-exists-p harness e-debug--session-id)
      e-debug--session-id)
     ((e-debug--find-session-id harness)
      (setq e-debug--session-id (e-debug--find-session-id harness)))
     (t
      (setq e-debug--session-id
            (plist-get
             (e-harness-create-session
              harness
              :metadata (e-debug--session-metadata))
             :id))))))

(defun e-debug--tab-name (tab)
  "Return the display name from TAB returned by `tab-bar-tabs'."
  (or (alist-get 'name tab nil nil #'eq)
      (alist-get 'name (cdr tab) nil nil #'eq)))

(defun e-debug--debug-tab-exists-p ()
  "Return non-nil when the named debug tab exists."
  (and (fboundp 'tab-bar-tabs)
       (cl-some (lambda (tab)
                  (equal (e-debug--tab-name tab) e-debug-tab-name))
                (tab-bar-tabs))))

(defun e-debug--select-or-create-tab ()
  "Select the named debug tab, or create and name it."
  (cond
   ((not (fboundp 'tab-bar-new-tab))
    nil)
   ((and (e-debug--debug-tab-exists-p)
         (fboundp 'tab-bar-select-tab-by-name))
    (tab-bar-select-tab-by-name e-debug-tab-name))
   (t
    (tab-bar-new-tab)
    (when (fboundp 'tab-bar-rename-tab)
      (tab-bar-rename-tab e-debug-tab-name)))))

(defun e-debug--show-buffer (buffer)
  "Show debug session BUFFER according to `e-debug-display-strategy'."
  (pcase e-debug-display-strategy
    ('popup
     (if (and (eq buffer e-debug--popup-buffer)
              (e-debug--popup-visible-p))
         (when (and e-debug--popup-frame
                    (frame-live-p e-debug--popup-frame))
           (select-frame-set-input-focus e-debug--popup-frame))
       (if (e-debug--popup-available-p)
         (e-debug--show-popup buffer)
       (e-debug--select-or-create-tab)
       (e-chat--pop-to-buffer buffer))))
    ('tab
     (e-debug--select-or-create-tab)
     (e-chat--pop-to-buffer buffer))
    ('window
     (e-chat--pop-to-buffer buffer))
    ('current-window
     (e-chat--switch-to-buffer buffer))
    (_
     (user-error "Unknown e debug display strategy: %S"
                 e-debug-display-strategy))))

(defun e-debug--popup-cleanup ()
  "Clear popup globals when the popup buffer is killed."
  (when (eq (current-buffer) e-debug--popup-buffer)
    (setq e-debug--popup-buffer nil)
    (setq e-debug--popup-frame nil)))

(defun e-debug--popup-parent-frame (&optional frame)
  "Return the root frame that should parent the debug popup.
When FRAME is nil, start from the selected frame."
  (let ((frame (or frame (selected-frame)))
        parent)
    (while (setq parent (frame-parameter frame 'parent-frame))
      (setq frame parent))
    frame))

(defun e-debug--call-interactively-in-frame (command frame)
  "Call COMMAND interactively with FRAME selected."
  (if (frame-live-p frame)
      (with-selected-frame frame
        (call-interactively command))
    (call-interactively command)))

(defun e-debug-popup-call-command-in-parent-frame (command)
  "Call interactive COMMAND in the debug popup's parent frame.
Refocus the popup frame afterward so typing can continue in the debug chat.
This is a generic popup-isolation helper: callers decide which package-specific
commands, such as workspace navigation commands, should be forwarded."
  (interactive
   (list (read-command "Command to run in debug popup parent frame: ")))
  (let ((popup-frame (selected-frame))
        (parent-frame (e-debug--popup-parent-frame)))
    (e-debug--call-interactively-in-frame command parent-frame)
    (when (frame-live-p popup-frame)
      (select-frame-set-input-focus popup-frame))))

(defun e-debug-popup-parent-frame-command (command)
  "Return an interactive command that forwards COMMAND to the popup parent frame."
  (lambda ()
    (interactive)
    (e-debug-popup-call-command-in-parent-frame command)))

(defun e-debug-popup-define-parent-frame-key (key command)
  "In `e-debug-popup-mode-map', bind KEY to run COMMAND in the parent frame.
KEY is a value accepted by `define-key', typically from `kbd'.  COMMAND is any
interactive command; e-debug does not assume it belongs to a specific workspace
or tab package."
  (define-key e-debug-popup-mode-map
              key
              (e-debug-popup-parent-frame-command command)))

(defun e-debug--show-popup (buffer)
  "Show BUFFER in the floating debug popup."
  (let* ((parent-frame (e-debug--popup-parent-frame))
         (frame
          (posframe-show
           buffer
           :parent-frame parent-frame
           :poshandler 'posframe-poshandler-frame-center
           :width (e-debug--popup-dimension e-debug-popup-width
                                             (frame-width parent-frame))
           :height (e-debug--popup-dimension e-debug-popup-height
                                              (frame-height parent-frame))
           :accept-focus t
           :border-width 1
           :override-parameters '((tab-bar-lines . 0)))))
    (setq e-debug--popup-buffer buffer)
    (setq e-debug--popup-frame frame)
    (with-current-buffer buffer
      (setq-local cursor-type 'bar)
      (setq-local cursor-in-non-selected-windows t)
      (e-chat--enter-composer-input-state)
      (e-debug-popup-mode 1)
      (add-hook 'kill-buffer-hook #'e-debug--popup-cleanup nil t))
    (when (frame-live-p frame)
      (select-frame-set-input-focus frame))
    buffer))

(defun e-debug--dismiss-popup ()
  "Dismiss the floating debug popup without aborting the debug session."
  (interactive)
  (let* ((popup-selected-frame
          (and (derived-mode-p 'e-chat-mode)
               (frame-parameter nil 'parent-frame)
               (selected-frame)))
         (buffer (or (and (derived-mode-p 'e-chat-mode)
                          (current-buffer))
                     e-debug--popup-buffer))
         (frame (or e-debug--popup-frame popup-selected-frame)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (fboundp 'posframe-hide)
          (posframe-hide (current-buffer)))
        (when (and frame (frame-live-p frame))
          (delete-frame frame))
        (e-debug-popup-mode -1))))
  (setq e-debug--popup-buffer nil)
  (setq e-debug--popup-frame nil))

(defun e-debug--selected-debug-popup-frame-p ()
  "Return non-nil when point is in a debug chat child frame."
  (and (derived-mode-p 'e-chat-mode)
       (frame-parameter nil 'parent-frame)
       e-chat-harness
       e-chat-session-id
       (or (equal e-chat-session-id e-debug--session-id)
           (e-debug--session-exists-p e-chat-harness e-chat-session-id))))

(defun e-debug--dismiss-popup-or-keyboard-quit ()
  "Dismiss a debug popup when focused, otherwise run `keyboard-quit'."
  (interactive)
  (if (or e-debug-popup-mode
          (e-debug--selected-debug-popup-frame-p))
      (e-debug--dismiss-popup)
    (keyboard-quit)))


(defun e-debug--legacy-popup-workspace-command (index)
  "Return legacy popup workspace command symbol for INDEX.
These commands were installed by older e-debug versions and are removed on
reload so package-specific workspace behavior can live in user config."
  (if (zerop index)
      'e-debug-popup-workspace-switch-to-final
    (intern (format "e-debug-popup-workspace-switch-to-%d" (1- index)))))

(defun e-debug--remove-legacy-popup-workspace-keybindings ()
  "Remove workspace bindings installed by older e-debug versions."
  (dotimes (i 10)
    (let ((command (e-debug--legacy-popup-workspace-command i)))
      (dolist (prefix '("M" "s"))
        (let ((key (kbd (format "%s-%d" prefix i))))
          (when (eq (lookup-key e-debug-popup-mode-map key) command)
            (define-key e-debug-popup-mode-map key nil)))))))

(defun e-debug--install-keybindings ()
  "Install debug popup keybindings into live chat keymaps."
  (define-key e-debug-popup-mode-map (kbd "C-g")
              #'e-debug--dismiss-popup-or-keyboard-quit)
  (e-debug--remove-legacy-popup-workspace-keybindings)
  (define-key e-chat-mode-map (kbd "C-g")
              #'e-debug--dismiss-popup-or-keyboard-quit))

(e-debug--install-keybindings)

(defun e-debug--popup-visible-p ()
  "Return non-nil when the floating debug popup is currently visible."
  (and (buffer-live-p e-debug--popup-buffer)
       (or (not e-debug--popup-frame)
           (frame-live-p e-debug--popup-frame))
       (with-current-buffer e-debug--popup-buffer
         e-debug-popup-mode)))

(defun e-debug--terminal-event-p (event)
  "Return non-nil when EVENT completes a debug turn."
  (memq (plist-get event :type)
        '(turn-finished turn-failed turn-cancelled backend-empty-output)))

(defun e-debug--notification-status (event)
  "Return compact notification status text for terminal EVENT."
  (let ((payload (plist-get event :payload)))
    (pcase (plist-get event :type)
      ('turn-finished
       (format "finished: %s" (or (plist-get event :turn-id) "turn")))
      ('turn-failed
       (format "failed: %s"
               (or (and (listp payload)
                        (plist-get payload :error))
                   "turn failed")))
      ('turn-cancelled
       (format "cancelled: %s" (or (plist-get event :turn-id) "turn")))
      ('backend-empty-output
       "failed: empty output")
      (_ nil))))

(defun e-debug--handle-notification-event (event)
  "Emit a compact debug notification for terminal EVENT when hidden."
  (when (and (e-debug--terminal-event-p event)
             (not (e-debug--popup-visible-p)))
    (when-let ((status (e-debug--notification-status event)))
      (message "*e-debug*: %s" status))))

(defun e-debug--ensure-notification-subscription (harness session-id)
  "Subscribe to terminal events for HARNESS SESSION-ID debug notifications."
  (unless (and (eq e-debug--notification-harness harness)
               (equal e-debug--notification-session-id session-id)
               e-debug--notification-subscription)
    (when (and e-debug--notification-harness
               e-debug--notification-subscription)
      (e-harness-unsubscribe e-debug--notification-harness
                             e-debug--notification-subscription))
    (setq e-debug--notification-harness harness)
    (setq e-debug--notification-session-id session-id)
    (setq e-debug--notification-subscription
          (e-harness-subscribe
           harness
           #'e-debug--handle-notification-event
           :session-id session-id))))

;;;###autoload
(defun e-debug ()
  "Open the standing debug agent session."
  (interactive)
  (let* ((harness (e-debug--default-harness))
         (session-id (e-debug--ensure-session harness))
         (buffer (e-chat-open-session harness session-id nil)))
    (e-debug--ensure-notification-subscription harness session-id)
    (e-debug--show-buffer buffer)
    buffer))

;;;###autoload
(defun e-debug-here (&optional question)
  "Ask the standing debug agent to inspect the current context."
  (interactive)
  (let* ((question (or question
                       (read-string "Debug question: " nil nil
                                    e-debug-default-prompt)))
         (debug-harness (e-debug--default-harness))
         (debug-session-id (e-debug--ensure-session debug-harness))
         (inspection-target (e-debug--inspection-target))
         (capture (e-debug--capture
                   :question question
                   :inspection-harness (plist-get inspection-target :harness)
                   :inspection-session-id
                   (plist-get inspection-target :session-id)))
         (prompt (plist-get capture :prompt))
         (references (plist-get capture :references))
         (metadata (plist-get capture :metadata))
         (buffer (e-chat-open-session debug-harness debug-session-id nil)))
    (e-debug--ensure-notification-subscription debug-harness debug-session-id)
    (e-chat-submit-session
     debug-harness
     debug-session-id
     prompt
     :references references
     :metadata metadata)
    (e-debug--show-buffer buffer)
    debug-session-id))

(defun e-debug-shell ()
  "Return the standing debug shell manifest."
  (e-shell-create
   :id 'debug
   :name "Debug"
   :summary "Standing debug agent session."
   :required-capabilities '(chat-session debug-agent)
   :commands
   (list
    (e-shell-command-create
     :id 'open
     :summary "Open the standing debug agent session."
     :interactive 'e-debug
     :function 'e-debug
     :scope 'global)
    (e-shell-command-create
     :id 'here
     :summary "Debug the current buffer or chat failure in the standing debug session."
     :interactive 'e-debug-here
     :function 'e-debug-here
     :scope 'global))))

(defun e-debug-startup ()
  "Register the standing debug shell manifest."
  (e-debug--install-keybindings)
  (e-shell-register (e-debug-shell)))

(add-hook 'e-startup-shell-hook #'e-debug-startup)

(provide 'e-debug)

;;; e-debug.el ends here

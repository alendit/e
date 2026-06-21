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
(require 'pp)
(require 'subr-x)
(require 'e-chat)
(require 'e-context-inspection)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-session)
(require 'e-shells)
(require 'e-startup)

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

(defconst e-debug-remediation-guidance
  "Debug guidance:
- Diagnose first from the attached evidence.
- Identify the strongest source, session, and failure signals before proposing a fix.
- Do not retry, mutate, edit, or change the inspected session unless explicitly asked.
- If remediation is requested, name the owning component and choose the smallest safe fix or file a bug report when the work should be saved for later."
  "Prompt guidance appended by `e-debug-here'.")

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
    (define-key map (kbd "C-g") #'e-debug--dismiss-popup)
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

(defun e-debug--source-reference ()
  "Return a focused source reference for the current buffer, or nil."
  (unless (minibufferp)
    (condition-case nil
        (let ((reference (e-chat-capture-source-reference)))
          (and reference
               (plist-put reference :id "source")))
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

(defun e-debug--failure-details (harness &optional limit)
  "Return recent failure detail plists from HARNESS."
  (when harness
    (delq nil
          (mapcar
           (lambda (failure)
             (condition-case nil
                 (e-context-inspection-failure-detail
                  :harness harness
                  :session-id (plist-get failure :session-id)
                  :turn-id (plist-get failure :turn-id))
               (error nil)))
           (e-context-inspection-recent-failures
            :harness harness
            :limit (or limit 2))))))

(defun e-debug--failure-reference (detail index)
  "Return a model-facing failure reference for DETAIL at one-based INDEX."
  (let* ((session (plist-get detail :session))
         (turn (plist-get detail :turn))
         (terminal (plist-get detail :terminal-error))
         (session-id (plist-get session :id))
         (turn-id (plist-get turn :id)))
    (list :id (format "failure-%d" index)
          :uri (format "e://session/%s/turn/%s/failure" session-id turn-id)
          :label (format "Failed turn %s/%s: %s"
                         session-id
                         turn-id
                         (or (plist-get terminal :error) "failed turn"))
          :text (pp-to-string detail))))

(defun e-debug--inspected-session-reference (harness session-id)
  "Return a reference describing inspected HARNESS SESSION-ID."
  (when (and harness session-id)
    (when-let ((session (e-session-get (e-harness-sessions harness) session-id)))
      (let ((metadata (plist-get session :metadata)))
        (list :id "inspected-session"
              :uri (format "e://session/%s" session-id)
              :label (format "Inspected chat session %s" session-id)
              :text (string-trim
                     (format "Inspected chat session: %s\nTitle: %s\nProject root: %s\nMetadata:\n%s"
                             session-id
                             (or (ignore-errors
                                   (e-harness-session-title harness session-id))
                                 "")
                             (or (plist-get metadata :project-root) "")
                             (pp-to-string metadata))))))))

(cl-defun e-debug--capture
    (&key question inspection-harness inspection-session-id source-reference)
  "Capture prompt and references for `e-debug-here'."
  (let* ((question (e-debug--normalize-question question))
         (source (or source-reference (e-debug--source-reference)))
         (inspected-session
          (e-debug--inspected-session-reference
           inspection-harness
           inspection-session-id))
         (details (e-debug--failure-details inspection-harness 2))
         (failure-references
          (cl-loop for detail in details
                   for index from 1
                   collect (e-debug--failure-reference detail index)))
         (references (delq nil
                           (append (list source inspected-session)
                                   failure-references)))
         (prompt-text (format "%s\n\n%s" question e-debug-remediation-guidance)))
    (list :prompt (e-chat-format-reference-prompt prompt-text references)
          :references references
          :metadata (list :source 'e-debug-here
                          :inspection-session-id
                          (or inspection-session-id
                              (plist-get (plist-get (car details) :session) :id))
                          :inspection-turn-id
                          (plist-get (plist-get (car details) :turn) :id)))))

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
     (if (e-debug--popup-available-p)
         (e-debug--show-popup buffer)
       (e-debug--select-or-create-tab)
       (e-chat--pop-to-buffer buffer)))
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

(defun e-debug--show-popup (buffer)
  "Show BUFFER in the floating debug popup."
  (let ((frame
         (posframe-show
          buffer
          :poshandler 'posframe-poshandler-frame-center
          :width (e-debug--popup-dimension e-debug-popup-width
                                            (frame-width))
          :height (e-debug--popup-dimension e-debug-popup-height
                                             (frame-height))
          :accept-focus t
          :border-width 1)))
    (setq e-debug--popup-buffer buffer)
    (setq e-debug--popup-frame frame)
    (with-current-buffer buffer
      (e-debug-popup-mode 1)
      (add-hook 'kill-buffer-hook #'e-debug--popup-cleanup nil t))
    (when (frame-live-p frame)
      (select-frame-set-input-focus frame))
    buffer))

(defun e-debug--dismiss-popup ()
  "Dismiss the floating debug popup without aborting the debug session."
  (interactive)
  (let ((buffer (or (and (derived-mode-p 'e-chat-mode)
                         (current-buffer))
                    e-debug--popup-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (fboundp 'posframe-hide)
          (posframe-hide (current-buffer)))
        (e-debug-popup-mode -1))))
  (setq e-debug--popup-buffer nil)
  (setq e-debug--popup-frame nil))

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
  (e-shell-register (e-debug-shell)))

(add-hook 'e-startup-shell-hook #'e-debug-startup)

(provide 'e-debug)

;;; e-debug.el ends here

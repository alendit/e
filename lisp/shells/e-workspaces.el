;;; e-workspaces.el --- Workspace awareness support for e shells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Presentation-owned workspace adapter layer.  This module intentionally keeps
;; the contract narrow: current workspace, token comparison, switching, buffer
;; membership, buffer admission, and visible-window lookup.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup e-workspace nil
  "Workspace awareness support for e presentation shells."
  :group 'e)

(defcustom e-workspace-awareness-enabled t
  "Non-nil means e shells may use workspace-aware presentation helpers."
  :type 'boolean
  :group 'e-workspace)

(defcustom e-workspace-awareness-backend-priority
  '(doom persp tab-bar single)
  "Workspace backend priority for workspace-aware presentation helpers."
  :type '(repeat symbol)
  :group 'e-workspace)

(defcustom e-workspace-display-cross-workspace-policy 'switch
  "Policy for explicit display commands targeting another workspace."
  :type '(choice (const :tag "Switch to target workspace" switch)
                 (const :tag "Route display to target workspace" route)
                 (const :tag "Warn without switching" warn)
                 (const :tag "Allow current workspace" allow))
  :group 'e-workspace)

(defcustom e-workspace-rebind-shell-on-open nil
  "Non-nil means opening a shell may rebind it to the current workspace."
  :type 'boolean
  :group 'e-workspace)

(cl-defstruct e-workspace-token
  backend
  id
  name
  frame)

(defvar-local e-buffer--workspace nil
  "Workspace affinity token owned by an e presentation buffer.")

(declare-function +workspace-current-name "ext:doom-workspaces")
(declare-function +workspace-exists-p "ext:doom-workspaces")
(declare-function +workspace-switch "ext:doom-workspaces")
(declare-function +workspace-buffer-list "ext:doom-workspaces")
(declare-function persp-current "ext:persp-mode")
(declare-function persp-current-name "ext:persp-mode")
(declare-function persp-name "ext:persp-mode")
(declare-function persp-with-name-exists-p "ext:persp-mode")
(declare-function persp-get-by-name "ext:persp-mode")
(declare-function persp-switch "ext:persp-mode")
(declare-function persp-contain-buffer-p "ext:persp-mode")
(declare-function persp-buffer-list "ext:persp-mode")

(defun e-workspace--frame (&optional frame)
  "Return FRAME or the currently selected frame."
  (or frame (selected-frame)))

(defun e-workspace--buffer (buffer)
  "Return live BUFFER from buffer object or name."
  (cond
   ((bufferp buffer) buffer)
   ((stringp buffer) (get-buffer buffer))
   (t nil)))

(defun e-workspace--tab-name (&optional tab)
  "Return the display name for TAB alist."
  (let ((tab (or tab
                 (when (fboundp 'tab-bar--current-tab)
                   (tab-bar--current-tab)))))
    (or (alist-get 'name tab)
        (alist-get 'explicit-name tab)
        "default")))

(defun e-workspace--backend-available-p (backend)
  "Return non-nil when BACKEND can answer workspace queries."
  (pcase backend
    ('doom (fboundp '+workspace-current-name))
    ('persp (or (fboundp 'persp-current-name)
                (fboundp 'persp-current)))
    ('tab-bar (fboundp 'tab-bar--current-tab))
    ('single t)
    (_ nil)))

(defun e-workspace--active-backend ()
  "Return the first configured available workspace backend."
  (or (cl-find-if #'e-workspace--backend-available-p
                  e-workspace-awareness-backend-priority)
      'single))

(defun e-workspace--doom-current (frame)
  "Return the current Doom workspace token for FRAME."
  (let ((name (+workspace-current-name)))
    (make-e-workspace-token
     :backend 'doom
     :id name
     :name name
     :frame frame)))

(defun e-workspace--persp-current (frame)
  "Return the current persp-mode workspace token for FRAME."
  (let* ((persp (when (fboundp 'persp-current)
                  (persp-current)))
         (name (cond
                ((fboundp 'persp-current-name)
                 (persp-current-name))
                ((and persp (fboundp 'persp-name))
                 (persp-name persp))
                (persp
                 (format "%s" persp))
                (t "default"))))
    (make-e-workspace-token
     :backend 'persp
     :id name
     :name name
     :frame frame)))

(defun e-workspace--tab-bar-current (frame)
  "Return the current tab-bar workspace token for FRAME."
  (let ((name (e-workspace--tab-name)))
    (make-e-workspace-token
     :backend 'tab-bar
     :id name
     :name name
     :frame frame)))

(defun e-workspace--single-current (frame)
  "Return the implicit single-workspace token for FRAME."
  (make-e-workspace-token
   :backend 'single
   :id 'single
   :name "single"
   :frame frame))

(defun e-workspace-current (&optional frame)
  "Return a workspace token for FRAME or the selected frame."
  (let ((frame (e-workspace--frame frame)))
    (pcase (if e-workspace-awareness-enabled
               (e-workspace--active-backend)
             'single)
      ('doom (e-workspace--doom-current frame))
      ('persp (e-workspace--persp-current frame))
      ('tab-bar (e-workspace--tab-bar-current frame))
      (_ (e-workspace--single-current frame)))))

(defun e-workspace-equal-p (a b)
  "Return non-nil when workspace tokens A and B identify the same workspace."
  (and (e-workspace-token-p a)
       (e-workspace-token-p b)
       (eq (e-workspace-token-backend a)
           (e-workspace-token-backend b))
       (equal (e-workspace-token-id a)
              (e-workspace-token-id b))
       (eq (e-workspace-token-frame a)
           (e-workspace-token-frame b))))

(defun e-workspace-format (token)
  "Return a readable representation of workspace TOKEN."
  (if (e-workspace-token-p token)
      (format "%s:%s"
              (e-workspace-token-backend token)
              (or (e-workspace-token-name token)
                  (e-workspace-token-id token)
                  "unknown"))
    "none"))

(defun e-buffer-workspace (&optional buffer)
  "Return workspace affinity for BUFFER or the current buffer."
  (let ((buffer (or (e-workspace--buffer buffer) (current-buffer))))
    (when (buffer-live-p buffer)
      (buffer-local-value 'e-buffer--workspace buffer))))

(defun e-buffer-set-workspace (buffer token)
  "Set BUFFER's workspace affinity to TOKEN and return TOKEN."
  (unless (buffer-live-p buffer)
    (user-error "No such buffer"))
  (unless (or (null token) (e-workspace-token-p token))
    (user-error "Invalid workspace token"))
  (with-current-buffer buffer
    (setq-local e-buffer--workspace token))
  token)

(defun e-buffer-ensure-workspace (&optional buffer)
  "Return BUFFER's workspace affinity, capturing the current workspace if absent."
  (let ((buffer (or (e-workspace--buffer buffer) (current-buffer))))
    (or (e-buffer-workspace buffer)
        (e-buffer-set-workspace buffer (e-workspace-current)))))

(defun e-shell-workspace (shell-buffer-or-state)
  "Return workspace affinity for SHELL-BUFFER-OR-STATE."
  (cond
   ((bufferp shell-buffer-or-state)
    (e-buffer-workspace shell-buffer-or-state))
   ((and (listp shell-buffer-or-state)
         (plist-member shell-buffer-or-state :workspace))
    (plist-get shell-buffer-or-state :workspace))
   (t nil)))

(defun e-shell-set-workspace (shell-buffer-or-state token)
  "Set SHELL-BUFFER-OR-STATE workspace affinity to TOKEN."
  (cond
   ((bufferp shell-buffer-or-state)
    (e-buffer-set-workspace shell-buffer-or-state token))
   ((listp shell-buffer-or-state)
    (plist-put shell-buffer-or-state :workspace token))
   (t (user-error "Unsupported shell workspace target"))))

(defun e-chat-buffer-workspace (&optional buffer)
  "Return chat BUFFER workspace affinity."
  (e-buffer-workspace buffer))

(defun e-org-canvas-workspace (&optional buffer-or-state)
  "Return Org Canvas workspace affinity for BUFFER-OR-STATE."
  (e-shell-workspace (or buffer-or-state (current-buffer))))

(defun e-workspace-live-p (token)
  "Return non-nil when TOKEN still names a live workspace."
  (and (e-workspace-token-p token)
       (frame-live-p (or (e-workspace-token-frame token) (selected-frame)))
       (pcase (e-workspace-token-backend token)
         ('doom (if (fboundp '+workspace-exists-p)
                    (+workspace-exists-p (e-workspace-token-id token))
                  t))
         ('persp (cond
                  ((fboundp 'persp-with-name-exists-p)
                   (persp-with-name-exists-p (e-workspace-token-id token)))
                  ((fboundp 'persp-get-by-name)
                   (persp-get-by-name (e-workspace-token-id token)))
                  (t t)))
         ('tab-bar t)
         ('single t)
         (_ nil))))

(defun e-workspace-switch (token)
  "Switch to TOKEN when its backend supports switching.
Return non-nil when the switch was accepted or no switch was required."
  (unless (e-workspace-token-p token)
    (user-error "Invalid workspace token"))
  (pcase (e-workspace-token-backend token)
    ('doom
     (when (fboundp '+workspace-switch)
       (+workspace-switch (e-workspace-token-id token)))
     t)
    ('persp
     (cond
      ((fboundp 'persp-switch)
       (persp-switch (e-workspace-token-id token))
       t)
      (t t)))
    ('tab-bar
     (when (fboundp 'tab-bar-switch-to-tab)
       (tab-bar-switch-to-tab (e-workspace-token-id token)))
     t)
    ('single t)
    (_ nil)))

(defun e-workspace-buffer-member-p (buffer token)
  "Return non-nil when BUFFER belongs to workspace TOKEN."
  (let ((buffer (e-workspace--buffer buffer)))
    (and buffer
         (e-workspace-token-p token)
         (pcase (e-workspace-token-backend token)
           ('doom
            (if (fboundp '+workspace-buffer-list)
                (memq buffer (+workspace-buffer-list))
              t))
           ('persp
            (cond
             ((fboundp 'persp-contain-buffer-p)
              (persp-contain-buffer-p buffer (e-workspace-token-id token)))
             ((fboundp 'persp-buffer-list)
              (memq buffer (persp-buffer-list)))
             (t t)))
           ('tab-bar t)
           ('single t)
           (_ nil)))))

(defun e-workspace-add-buffer (buffer token)
  "Add BUFFER to workspace TOKEN when supported and return BUFFER."
  (let ((buffer (e-workspace--buffer buffer)))
    (unless buffer
      (user-error "No such buffer"))
    (when (e-workspace-token-p token)
      (pcase (e-workspace-token-backend token)
        ('doom
         (when (fboundp 'persp-add-buffer)
           (persp-add-buffer buffer)))
        ('persp
         (when (fboundp 'persp-add-buffer)
           (persp-add-buffer buffer)))
        (_ nil)))
    buffer))

(defun e-workspace-visible-window (buffer token)
  "Return a visible window showing BUFFER in TOKEN's frame, or nil."
  (let ((buffer (e-workspace--buffer buffer)))
    (when (and buffer
               (e-workspace-token-p token)
               (e-workspace-equal-p
                token
                (e-workspace-current (e-workspace-token-frame token))))
      (get-buffer-window buffer (or (e-workspace-token-frame token)
                                    (selected-frame))))))

(cl-defun e-workspace-find-buffer
    (predicate &key workspace prefer-visible buffers)
  "Return an existing live buffer matching PREDICATE.

PREDICATE is called with each live buffer.  When PREFER-VISIBLE is non-nil, an
already visible matching buffer wins over hidden matches.  When WORKSPACE is a
workspace token, a matching buffer already bound to that workspace wins over a
generic hidden fallback.

Presentation shells should use this before creating a new shell/helper buffer
for an identity that may already be live in another workspace."
  (let (fallback workspace-fallback)
    (catch 'buffer
      (dolist (buffer (or buffers (buffer-list)))
        (when (and (buffer-live-p buffer)
                   (funcall predicate buffer))
          (unless fallback
            (setq fallback buffer))
          (when (and workspace
                     (not workspace-fallback)
                     (e-workspace-equal-p workspace
                                          (e-buffer-workspace buffer)))
            (setq workspace-fallback buffer))
          (when (and prefer-visible
                     (get-buffer-window buffer t))
            (throw 'buffer buffer))))
      (or workspace-fallback fallback))))

(defun e-workspace--display-action (&optional action)
  "Return a workspace-scoped display ACTION."
  (let ((functions (or action
                       '(display-buffer-reuse-window
                         display-buffer-use-some-window
                         display-buffer-pop-up-window))))
    (cons functions
          '((reusable-frames . nil)
            (lru-frames . nil)
            (inhibit-switch-frame . t)
            (some-window . mru)))))

(cl-defun e-workspace-display-buffer
    (buffer &key workspace action select side-window-ok)
  "Display BUFFER in WORKSPACE and return the display window.
ACTION is a `display-buffer' action function list.  SELECT non-nil selects the
window.  SIDE-WINDOW-OK is accepted for callers that already handled side-window
policy; the workspace service itself keeps displays frame-scoped."
  (ignore side-window-ok)
  (let* ((buffer (e-workspace--buffer buffer))
         (workspace (or workspace
                        (and buffer (e-buffer-workspace buffer))
                        (e-workspace-current))))
    (unless buffer
      (user-error "No such buffer"))
    (if-let ((window (e-workspace-visible-window buffer workspace)))
        (progn
          (when select
            (select-window window))
          window)
      (pcase e-workspace-display-cross-workspace-policy
        ('allow nil)
        ('warn
         (unless (e-workspace-equal-p workspace (e-workspace-current))
           (message "e workspace: %s belongs to %s"
                    (buffer-name buffer)
                    (e-workspace-format workspace))))
        (_ (e-workspace-switch workspace)))
      (unless (e-workspace-buffer-member-p buffer workspace)
        (e-workspace-add-buffer buffer workspace))
      (let ((window (display-buffer buffer (e-workspace--display-action action))))
        (when (and select (window-live-p window))
          (select-window window))
        window))))

(cl-defun e-workspace-pop-to-buffer (buffer &key workspace)
  "Pop to BUFFER in WORKSPACE and return BUFFER."
  (when-let ((window (e-workspace-display-buffer
                     buffer
                     :workspace workspace
                     :select t)))
    (select-window window))
  (e-workspace--buffer buffer))

(cl-defun e-workspace-switch-to-buffer (buffer &key workspace)
  "Switch to BUFFER in WORKSPACE and return BUFFER."
  (when-let ((window (e-workspace-display-buffer
                     buffer
                     :workspace workspace
                     :action '(display-buffer-same-window
                               display-buffer-reuse-window
                               display-buffer-use-some-window)
                     :select t)))
    (select-window window))
  (e-workspace--buffer buffer))

(cl-defun e-workspace-select-buffer-window (buffer &key workspace)
  "Select BUFFER's visible window in WORKSPACE, displaying it if necessary."
  (e-workspace-switch-to-buffer buffer :workspace workspace))

(provide 'e-workspaces)

;;; e-workspaces.el ends here

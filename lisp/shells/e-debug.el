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
(require 'e-chat)
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

(defcustom e-debug-display-strategy 'tab
  "Display strategy used by `e-debug'."
  :type '(choice (const :tag "Open in a new tab" tab)
                 (const :tag "Pop to another window" window)
                 (const :tag "Use current window" current-window))
  :group 'e-debug)

(defvar e-debug--session-id nil
  "Cached standing debug session id.")

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

(defun e-debug--show-buffer (buffer)
  "Show debug session BUFFER according to `e-debug-display-strategy'."
  (pcase e-debug-display-strategy
    ('tab
     (when (fboundp 'tab-bar-new-tab)
       (tab-bar-new-tab))
     (e-chat--pop-to-buffer buffer))
    ('window
     (e-chat--pop-to-buffer buffer))
    ('current-window
     (e-chat--switch-to-buffer buffer))
    (_
     (user-error "Unknown e debug display strategy: %S"
                 e-debug-display-strategy))))

;;;###autoload
(defun e-debug ()
  "Open the standing debug agent session."
  (interactive)
  (let* ((harness (e-debug--default-harness))
         (session-id (e-debug--ensure-session harness))
         (buffer (e-chat-open-session harness session-id nil)))
    (e-debug--show-buffer buffer)
    buffer))

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
     :scope 'global))))

(defun e-debug-startup ()
  "Register the standing debug shell manifest."
  (e-shell-register (e-debug-shell)))

(add-hook 'e-startup-shell-hook #'e-debug-startup)

(provide 'e-debug)

;;; e-debug.el ends here

;;; e-modernchat.el --- egui-backed modern chat shell for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Optional modern chat presentation shell backed by emacs-egui.  The module is
;; loadable without emacs-egui; invoking the shell checks for the runtime and
;; xwidget support.

;;; Code:

(require 'cl-lib)
(require 'e-chat-service)
(require 'e-modernchat-view-model)
(require 'e-shells)
(require 'e-workspaces)
(require 'project)
(require 'url-util)
(require 'subr-x)

(declare-function emacs-egui-register-app "emacs-egui" (app-name ui-dir))
(declare-function emacs-egui-create-buffer "emacs-egui" (&rest args))
(declare-function e-source-directory "e" ())
(declare-function emacs-egui-on "emacs-egui" (session action callback))
(declare-function emacs-egui-send-state "emacs-egui" (session state))
(declare-function emacs-egui-get-field "emacs-egui" (payload key))

(defconst e-modernchat--app-name "e-modernchat"
  "emacs-egui app name for the modern chat shell.")

(defgroup e-modernchat nil
  "egui-backed modern chat shell for e."
  :group 'e)

(defcustom e-modernchat-update-debounce-seconds 0.05
  "Seconds to debounce modern chat snapshot pushes."
  :type 'number
  :group 'e-modernchat)

(defvar-local e-modernchat-harness nil
  "Harness attached to the current modern chat buffer.")

(defvar-local e-modernchat-session-id nil
  "Session id attached to the current modern chat buffer.")

(defvar-local e-modernchat--egui-session nil
  "emacs-egui session metadata for the current modern chat buffer.")

(defvar-local e-modernchat--event-subscription nil
  "Harness event subscription for the current modern chat buffer.")

(defvar-local e-modernchat--update-timer nil
  "Debounce timer for the current modern chat buffer.")

(defun e-modernchat--source-directory ()
  "Return the root directory of the e source tree."
  (or (and (fboundp 'e-source-directory) (e-source-directory))
      (when-let ((root (locate-dominating-file
                        (or load-file-name buffer-file-name default-directory)
                        "e.el")))
        (file-name-as-directory (expand-file-name root)))
      (error "Cannot locate e source directory")))

(defun e-modernchat--ui-directory ()
  "Return the modern chat UI asset directory."
  (expand-file-name "ui/modernchat/" (e-modernchat--source-directory)))

(defun e-modernchat--vendored-egui-lisp-directory ()
  "Return the vendored emacs-egui lisp directory, or nil."
  (let ((directory (expand-file-name "emacs-egui/lisp/"
                                     (e-modernchat--source-directory))))
    (and (file-readable-p (expand-file-name "emacs-egui.el" directory))
         directory)))

(defun e-modernchat--ensure-runtime-path ()
  "Add vendored emacs-egui to `load-path' when present."
  (when-let ((directory (e-modernchat--vendored-egui-lisp-directory)))
    (add-to-list 'load-path directory)))

(defun e-modernchat--runtime-available-p ()
  "Return non-nil when emacs-egui can be loaded."
  (e-modernchat--ensure-runtime-path)
  (locate-library "emacs-egui"))

(defun e-modernchat--ensure-runtime ()
  "Require optional emacs-egui runtime and xwidget support."
  (unless (e-modernchat--runtime-available-p)
    (user-error "e-modernchat-new requires the emacs-egui submodule; run git submodule update --init emacs-egui"))
  (require 'emacs-egui)
  (unless (fboundp 'xwidget-webkit-browse-url)
    (user-error "e-modernchat-new requires Emacs with xwidget-webkit support"))
  (let ((ui-dir (e-modernchat--ui-directory)))
    (unless (file-readable-p (expand-file-name "index.html" ui-dir))
      (user-error "e-modernchat UI index is missing under %s" ui-dir))
    (unless (file-readable-p (expand-file-name "pkg/e_modernchat.js" ui-dir))
      (user-error "e-modernchat WASM package is missing; run wasm-pack build in %s"
                  ui-dir))
    (emacs-egui-register-app e-modernchat--app-name ui-dir)))

(defun e-modernchat--payload-field (payload key)
  "Return KEY from JSON PAYLOAD supporting alists and plists."
  (let ((string-key (symbol-name key))
        (keyword-key (intern (concat ":" (symbol-name key)))))
    (cond
     ((and (listp (car-safe payload)) (assoc key payload))
      (cdr (assoc key payload)))
     ((and (listp (car-safe payload)) (assoc string-key payload))
      (cdr (assoc string-key payload)))
     ((plist-member payload keyword-key)
      (plist-get payload keyword-key))
     ((plist-member payload key)
      (plist-get payload key))
     ((and (fboundp 'emacs-egui-get-field) payload)
      (emacs-egui-get-field payload key)))))

(defun e-modernchat--snapshot ()
  "Return the current modern chat snapshot."
  (unless (and e-modernchat-harness e-modernchat-session-id)
    (user-error "This buffer is not attached to an e modern chat session"))
  (e-modernchat-view-model-snapshot
   e-modernchat-harness e-modernchat-session-id))

(defun e-modernchat--push-snapshot (&optional buffer)
  "Push a full snapshot for BUFFER or the current buffer to egui."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and e-modernchat--egui-session
                   e-modernchat-harness
                   e-modernchat-session-id)
          (emacs-egui-send-state
           e-modernchat--egui-session
           (e-modernchat--snapshot)))))))

(defun e-modernchat--schedule-push (&optional buffer)
  "Schedule a debounced snapshot push for BUFFER or current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (timerp e-modernchat--update-timer)
          (cancel-timer e-modernchat--update-timer))
        (setq e-modernchat--update-timer
              (run-at-time
               e-modernchat-update-debounce-seconds nil
               (lambda (target)
                 (when (buffer-live-p target)
                   (with-current-buffer target
                     (setq e-modernchat--update-timer nil))
                   (e-modernchat--push-snapshot target)))
               buffer))))))

(defun e-modernchat--handle-event (buffer event)
  "Handle board-observed EVENT for modern chat BUFFER."
  (ignore event)
  (when (buffer-live-p buffer)
    (e-modernchat--schedule-push buffer)))

(defun e-modernchat--subscribe ()
  "Subscribe current modern chat buffer to harness events."
  (let ((buffer (current-buffer)))
    (setq e-modernchat--event-subscription
          (e-chat-service-subscribe
           e-modernchat-harness e-modernchat-session-id
           (lambda (event)
             (e-modernchat--handle-event buffer event))))))

(defun e-modernchat--cleanup ()
  "Clean up current modern chat buffer subscriptions and timers."
  (when (timerp e-modernchat--update-timer)
    (cancel-timer e-modernchat--update-timer))
  (setq e-modernchat--update-timer nil)
  (when (and e-modernchat-harness e-modernchat--event-subscription)
    (e-chat-service-unsubscribe e-modernchat--event-subscription))
  (setq e-modernchat--event-subscription nil))

(defun e-modernchat--open-resource (uri)
  "Open resource URI from modern chat."
  (cond
   ((not (stringp uri))
    (user-error "Missing resource URI"))
   ((string-prefix-p "file://" uri)
    (find-file (url-unhex-string (substring uri (length "file://")))))
   ((string-prefix-p "buffer://" uri)
    (let ((buffer (get-buffer (url-unhex-string
                               (substring uri (length "buffer://"))))))
      (unless buffer
        (user-error "No live buffer for %s" uri))
      (e-workspace-pop-to-buffer buffer)))
   (t
    (message "No direct modernchat opener for %s" uri))))

(defun e-modernchat--handle-ui-action (payload)
  "Handle semantic UI action PAYLOAD from egui."
  (let ((action (e-modernchat--payload-field payload 'action)))
    (pcase (and action (intern-soft action))
      ('send-message
       (let ((text (string-trim
                    (or (e-modernchat--payload-field payload 'text) ""))))
         (unless (string-empty-p text)
           (e-chat-service-submit-session
            e-modernchat-harness e-modernchat-session-id text)
           (e-modernchat--schedule-push))))
      ('cancel-turn
       (e-chat-service-abort-session
        e-modernchat-harness e-modernchat-session-id)
       (e-modernchat--schedule-push))
      ('open-resource
       (e-modernchat--open-resource
        (or (e-modernchat--payload-field payload 'uri)
            (e-modernchat--payload-field payload 'target))))
      ('set-output-mode
       (let* ((mode-text (e-modernchat--payload-field payload 'mode))
              (mode (and mode-text (not (string-empty-p mode-text))
                         (intern mode-text))))
         (e-chat-service-set-output-mode
          e-modernchat-harness e-modernchat-session-id mode)
         (e-modernchat--schedule-push)))
      (_
       (message "e-modernchat: ignored UI action %S" action)))))

(defun e-modernchat--wire-actions (session buffer)
  "Register modern chat callbacks for emacs-egui SESSION and BUFFER."
  (emacs-egui-on session "ui-action"
                 (lambda (payload)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (e-modernchat--handle-ui-action payload)))))
  (emacs-egui-on session "send-message"
                 (lambda (payload)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (e-modernchat--handle-ui-action
                        (cons '(action . "send-message") payload))))))
  (emacs-egui-on session "open-resource"
                 (lambda (payload)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (e-modernchat--handle-ui-action
                        (cons '(action . "open-resource") payload)))))))


(defun e-modernchat--project-root (&optional directory)
  "Return a normalized project root for DIRECTORY."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (project-root
          (let ((default-directory directory))
            (ignore-errors
              (when-let ((project (project-current nil)))
                (project-root project)))))
         (git-root (locate-dominating-file directory ".git")))
    (file-name-as-directory
     (expand-file-name (or project-root git-root directory)))))

(defun e-modernchat--session-metadata ()
  "Return metadata for a modern chat session created from the current buffer."
  (list :project-root (e-modernchat--project-root default-directory)))

(defun e-modernchat--buffer-name (harness session-id)
  "Return modern chat buffer name for HARNESS SESSION-ID."
  (format "*e-modernchat:%s*"
          (or (ignore-errors (e-harness-session-title harness session-id))
              session-id)))

(defun e-modernchat-open-session (harness session-id &optional display)
  "Open HARNESS SESSION-ID in a modern chat shell.
Display the buffer when DISPLAY is non-nil."
  (e-modernchat--ensure-runtime)
  (let* ((session (emacs-egui-create-buffer
                   :app-name e-modernchat--app-name
                   :buffer-name (e-modernchat--buffer-name harness session-id)))
         (buffer (plist-get session :buffer)))
    (with-current-buffer buffer
      (setq-local e-modernchat-harness harness)
      (setq-local e-modernchat-session-id session-id)
      (setq-local e-modernchat--egui-session session)
      (add-hook 'kill-buffer-hook #'e-modernchat--cleanup nil t)
      (e-modernchat--wire-actions session buffer)
      (e-modernchat--subscribe)
      (run-at-time 0.6 nil #'e-modernchat--push-snapshot buffer))
    (when display
      (e-workspace-pop-to-buffer buffer))
    buffer))

;;;###autoload
(defun e-modernchat-new ()
  "Create a new persisted chat session and open it in modern chat."
  (interactive)
  (let* ((harness (e-chat-service-default-harness))
         (session (e-chat-service-create-session
                   :harness harness
                   :metadata (e-modernchat--session-metadata)))
         (session-id (plist-get session :id)))
    (e-modernchat-open-session harness session-id t)))

;;;###autoload
(defun e-modernchat-shell ()
  "Return the modern chat presentation shell manifest."
  (e-shell-create
   :id 'modernchat
   :name "Modern Chat"
   :summary "egui-backed modern chat and flow timeline."
   :required-capabilities '(chat-session)
   :commands
   (list
    (e-shell-command-create
     :id 'new
     :summary "Start a new egui-backed modern chat session."
     :interactive 'e-modernchat-new
     :function #'e-modernchat-new
     :scope 'global))))

(e-shell-register (e-modernchat-shell))

(provide 'e-modernchat)

;;; e-modernchat.el ends here

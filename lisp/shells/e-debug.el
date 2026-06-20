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

(defcustom e-debug-display-strategy 'tab
  "Display strategy used by `e-debug'."
  :type '(choice (const :tag "Open in a new tab" tab)
                 (const :tag "Pop to another window" window)
                 (const :tag "Use current window" current-window))
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
  (cond
   ((and (derived-mode-p 'e-chat-mode)
         e-chat-harness)
    e-chat-harness)
   (t
    (ignore-errors (e-chat--default-harness)))))

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

(cl-defun e-debug--capture (&key question inspection-harness source-reference)
  "Capture prompt and references for `e-debug-here'."
  (let* ((question (e-debug--normalize-question question))
         (source (or source-reference (e-debug--source-reference)))
         (details (e-debug--failure-details inspection-harness 2))
         (failure-references
          (cl-loop for detail in details
                   for index from 1
                   collect (e-debug--failure-reference detail index)))
         (references (delq nil (append (list source) failure-references)))
         (prompt-text (format "%s\n\n%s" question e-debug-remediation-guidance)))
    (list :prompt (e-chat-format-reference-prompt prompt-text references)
          :references references
          :metadata (list :source 'e-debug-here
                          :inspection-session-id
                          (plist-get (plist-get (car details) :session) :id)
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

;;;###autoload
(defun e-debug-here (&optional question)
  "Ask the standing debug agent to inspect the current context."
  (interactive)
  (let* ((question (or question
                       (read-string "Debug question: " nil nil
                                    e-debug-default-prompt)))
         (debug-harness (e-debug--default-harness))
         (debug-session-id (e-debug--ensure-session debug-harness))
         (capture (e-debug--capture
                   :question question
                   :inspection-harness (e-debug--inspection-harness)))
         (prompt (plist-get capture :prompt))
         (references (plist-get capture :references))
         (metadata (plist-get capture :metadata))
         (buffer (e-chat-open-session debug-harness debug-session-id nil)))
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

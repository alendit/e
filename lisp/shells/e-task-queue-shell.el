;;; e-task-queue-shell.el --- Task queue list buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A `tabulated-list' buffer that lists task-queue tasks newest-first and lets
;; an operator watch and steer the queue.  The buffer refreshes on queue-change
;; events and offers row actions to open a task's session and to cancel it.
;; Display is workspace-aware.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'e-task-queue)
(require 'e-task-queue-actions)
(require 'e-keymap-hints)
(require 'e-workspaces)

(declare-function e-chat-open-session "e-chat")
(declare-function e-harness-instance-get-or-create "e-harness-instances")
(declare-function e-task-queue--default-instance-id "e-task-queue")

(defconst e-task-queue-shell-buffer-name "*e-task-queue*"
  "Name of the task queue list buffer.")

(defvar-local e-task-queue-shell--queue nil
  "Task queue backing the current list buffer.")

(defun e-task-queue-shell--status-label (status)
  "Return a short display label for task STATUS."
  (pcase status
    ('queued "queued")
    ('running "running")
    ('done "done")
    ('failed "failed")
    ('cancelled "cancelled")
    ('paused "paused")
    (_ (format "%s" status))))

(defun e-task-queue-shell--format-time (iso)
  "Return a short HH:MM display for ISO-8601 ISO, or empty when nil.
Keeps the date off when it matches today so the common case reads compactly."
  (if (and (stringp iso) (not (string-empty-p iso)))
      (condition-case nil
          (let* ((time (date-to-time iso))
                 (same-day (string= (format-time-string "%F" time)
                                    (format-time-string "%F"))))
            (format-time-string (if same-day "%H:%M" "%m-%d %H:%M") time))
        (error iso))
    ""))

(defun e-task-queue-shell--entry (record)
  "Return a `tabulated-list' entry for task RECORD.
The Task column shows the agent-authored summary stub when present, falling
back to a prompt prefix; the Finished column shows when the task settled."
  (list (plist-get record :task-id)
        (vector (e-task-queue-shell--format-time (plist-get record :enqueued-at))
                (e-task-queue-shell--status-label (plist-get record :status))
                (e-task-queue-shell--format-time (plist-get record :finished-at))
                (e-task-queue-record-display-summary record)
                (number-to-string (length (plist-get record :outputs))))))

(defconst e-task-queue-shell--hint-bindings
  '(("RET" . "open session")
    ("g" . "refresh")
    ("c" . "cancel")
    ("p" . "pause")
    ("r" . "resume")
    ("P" . "pause all")
    ("R" . "resume all"))
  "Ordered key hints shown in the task queue list footer.")

(defun e-task-queue-shell--refresh ()
  "Rebuild the list buffer from its backing queue, preserving point.
Appends a key hint footer below the list so the operator sees the available
row actions without leaving the buffer."
  (when (derived-mode-p 'e-task-queue-shell-mode)
    (setq header-line-format
          (when (e-task-queue-paused-p e-task-queue-shell--queue)
            "Queue paused: dispatch is halted (R to resume all)"))
    (setq tabulated-list-entries
          (mapcar #'e-task-queue-shell--entry
                  (e-task-queue-list e-task-queue-shell--queue)))
    (tabulated-list-print t)
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (e-keymap-hints-insert e-task-queue-shell--hint-bindings)))))

(defun e-task-queue-shell-refresh ()
  "Rebuild the list buffer from its backing queue.
The operator's manual refresh, the escape hatch when a change-hook wakeup is
missed."
  (interactive)
  (e-task-queue-shell--refresh))

(defun e-task-queue-shell--refresh-buffers (_queue)
  "Refresh every live task queue list buffer.
Bound to `e-task-queue-change-functions' so the list tracks live status."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'e-task-queue-shell-mode)
        (e-task-queue-shell--refresh)))))

(defun e-task-queue-shell--task-id-at-point ()
  "Return the task id of the row at point, or signal."
  (or (tabulated-list-get-id)
      (user-error "No task on this line")))

(defun e-task-queue-shell-cancel ()
  "Cancel the task on the current row."
  (interactive)
  (e-task-queue-cancel e-task-queue-shell--queue
                       (e-task-queue-shell--task-id-at-point))
  (e-task-queue-shell--refresh))

(defun e-task-queue-shell-pause ()
  "Pause the task on the current row."
  (interactive)
  (e-task-queue-pause e-task-queue-shell--queue
                      (e-task-queue-shell--task-id-at-point))
  (e-task-queue-shell--refresh))

(defun e-task-queue-shell-resume ()
  "Resume the task on the current row."
  (interactive)
  (e-task-queue-resume e-task-queue-shell--queue
                       (e-task-queue-shell--task-id-at-point))
  (e-task-queue-shell--refresh))

(defun e-task-queue-shell-pause-all ()
  "Set the queue pause gate and pause every non-terminal task."
  (interactive)
  (e-task-queue-pause-all e-task-queue-shell--queue)
  (e-task-queue-shell--refresh))

(defun e-task-queue-shell-resume-all ()
  "Clear the queue pause gate and resume every paused task."
  (interactive)
  (e-task-queue-resume-all e-task-queue-shell--queue)
  (e-task-queue-shell--refresh))

(defun e-task-queue-shell-show-outputs ()
  "Show the outputs and session of the task on the current row.
The default runner records a session id and assistant outputs once a task
settles; this surfaces them in a help buffer for inspection."
  (interactive)
  (let* ((task-id (e-task-queue-shell--task-id-at-point))
         (record (e-task-queue-get e-task-queue-shell--queue task-id)))
    (with-help-window (format "*e-task-queue: %s*" task-id)
      (with-current-buffer standard-output
        (insert (format "Task: %s\nSummary: %s\nStatus: %s\nSession: %s\n\n"
                        task-id
                        (e-task-queue-record-display-summary record)
                        (e-task-queue-shell--status-label
                         (plist-get record :status))
                        (or (plist-get record :session-id) "-")))
        (insert (format "Prompt:\n%s\n\n" (plist-get record :prompt)))
        (when-let ((error (plist-get record :error)))
          (insert (format "Error:\n%s\n\n" error)))
        (insert "Outputs:\n")
        (if-let ((outputs (plist-get record :outputs)))
            (dolist (output outputs)
              (insert (format "- [%s] %s\n"
                              (plist-get output :kind)
                              (or (plist-get output :value) ""))))
          (insert "(none)\n"))))))

(defun e-task-queue-shell-open-session ()
  "Open the chat session backing the task on the current row.
Resolves the task's harness instance (its own, or the queue default) and opens
its session in a chat buffer.  Signals when the task never started a session --
a queued task has no session yet."
  (interactive)
  (let* ((task-id (e-task-queue-shell--task-id-at-point))
         (record (e-task-queue-get e-task-queue-shell--queue task-id))
         (session-id (plist-get record :session-id)))
    (unless session-id
      (user-error "Task %s has no session yet" task-id))
    (unless (require 'e-chat nil t)
      (user-error "e-chat is not available to open the session"))
    (let* ((instance-id
            (or (plist-get record :harness-instance-id)
                (e-task-queue--default-instance-id e-task-queue-shell--queue)))
           (harness (e-harness-instance-get-or-create instance-id)))
      (e-chat-open-session harness session-id t instance-id))))

(defvar e-task-queue-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'e-task-queue-shell-cancel)
    (define-key map (kbd "p") #'e-task-queue-shell-pause)
    (define-key map (kbd "r") #'e-task-queue-shell-resume)
    (define-key map (kbd "P") #'e-task-queue-shell-pause-all)
    (define-key map (kbd "R") #'e-task-queue-shell-resume-all)
    (define-key map (kbd "RET") #'e-task-queue-shell-open-session)
    (define-key map (kbd "o") #'e-task-queue-shell-show-outputs)
    (define-key map (kbd "g") #'e-task-queue-shell-refresh)
    map)
  "Keymap for `e-task-queue-shell-mode'.")

(define-derived-mode e-task-queue-shell-mode tabulated-list-mode "e-Task-Queue"
  "Major mode listing agent task queue tasks newest-first."
  (setq tabulated-list-format
        [("Enqueued" 14 t)
         ("Status" 10 t)
         ("Finished" 14 t)
         ("Task" 48 nil)
         ("Outputs" 8 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;;;###autoload
(cl-defun e-task-queue-list-buffer (&key queue)
  "Open the task queue list buffer for QUEUE and return it.
QUEUE defaults to `e-task-queue-actions-default-queue'.  When showing that
shared default queue, rehydrate it from disk first so opening the buffer after
a restart lists persisted tasks and re-dispatches queued work, instead of
depending on a harness having built the task-queue layer."
  (interactive)
  (let ((queue (or queue (e-task-queue-actions-ensure-loaded)))
        (buffer (get-buffer-create e-task-queue-shell-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'e-task-queue-shell-mode)
        (e-task-queue-shell-mode))
      (setq e-task-queue-shell--queue queue)
      (e-task-queue-shell--refresh))
    (add-hook 'e-task-queue-change-functions
              #'e-task-queue-shell--refresh-buffers)
    (when (called-interactively-p 'interactive)
      (e-workspace-pop-to-buffer buffer))
    buffer))

(provide 'e-task-queue-shell)

;;; e-task-queue-shell.el ends here

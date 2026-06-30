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
(require 'e-workspaces)

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
    (_ (format "%s" status))))

(defun e-task-queue-shell--entry (record)
  "Return a `tabulated-list' entry for task RECORD."
  (list (plist-get record :task-id)
        (vector (or (plist-get record :enqueued-at) "")
                (e-task-queue-shell--status-label (plist-get record :status))
                (or (plist-get record :prompt-summary) "")
                (number-to-string (length (plist-get record :outputs))))))

(defun e-task-queue-shell--refresh ()
  "Rebuild the list buffer from its backing queue, preserving point."
  (when (derived-mode-p 'e-task-queue-shell-mode)
    (setq tabulated-list-entries
          (mapcar #'e-task-queue-shell--entry
                  (e-task-queue-list e-task-queue-shell--queue)))
    (tabulated-list-print t)))

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

(defun e-task-queue-shell-show-outputs ()
  "Show the outputs and session of the task on the current row.
The default runner records a session id and assistant outputs once a task
settles; this surfaces them in a help buffer for inspection."
  (interactive)
  (let* ((task-id (e-task-queue-shell--task-id-at-point))
         (record (e-task-queue-get e-task-queue-shell--queue task-id)))
    (with-help-window (format "*e-task-queue: %s*" task-id)
      (with-current-buffer standard-output
        (insert (format "Task: %s\nStatus: %s\nSession: %s\n\n"
                        task-id
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

(defvar e-task-queue-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'e-task-queue-shell-cancel)
    (define-key map (kbd "RET") #'e-task-queue-shell-show-outputs)
    (define-key map (kbd "g") #'e-task-queue-shell-refresh)
    map)
  "Keymap for `e-task-queue-shell-mode'.")

(define-derived-mode e-task-queue-shell-mode tabulated-list-mode "e-Task-Queue"
  "Major mode listing agent task queue tasks newest-first."
  (setq tabulated-list-format
        [("Enqueued" 26 t)
         ("Status" 10 t)
         ("Task" 50 nil)
         ("Outputs" 8 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;;;###autoload
(cl-defun e-task-queue-list-buffer (&key queue)
  "Open the task queue list buffer for QUEUE and return it.
QUEUE defaults to `e-task-queue-actions-default-queue'."
  (interactive)
  (let ((queue (or queue e-task-queue-actions-default-queue))
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

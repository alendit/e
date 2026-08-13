;;; e-board-runs-shell.el --- Durable board run list buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This shell observes bounded durable run projections.  It never opens or
;; polls a child session; board fact notifications are its refresh trigger.

;;; Code:

(require 'seq)
(require 'tabulated-list)
(require 'e-board-orchestration-actions)
(require 'e-keymap-hints)
(require 'e-workspaces)

(defconst e-board-runs-shell-buffer-name "*e-board-runs*"
  "Name of the durable board run list buffer.")

(defconst e-board-runs-shell-detail-buffer-name "*e-board-run*"
  "Name of the bounded durable board run detail buffer.")

(defvar-local e-board-runs-shell--board nil
  "Core board whose durable runs this buffer displays.")

(defun e-board-runs-shell--state-label (projection)
  "Return the short lifecycle label for run PROJECTION."
  (or (plist-get projection :terminal-status)
      (plist-get projection :state)
      'running))

(defun e-board-runs-shell--task-label (projection)
  "Return completed and total task counts for PROJECTION."
  (let* ((tasks (plist-get projection :tasks))
         (complete (seq-count (lambda (task)
                                (memq (plist-get task :state)
                                      '(done failed cancelled)))
                              tasks)))
    (format "%d/%d" complete (length tasks))))

(defun e-board-runs-shell--deadline-label (projection)
  "Return a concise deadline evidence label for PROJECTION."
  (let ((deadline (plist-get projection :deadline)))
    (cond
     ((not deadline) "-")
     ((plist-get deadline :expired) "expired")
     ((eq (plist-get deadline :kind) 'none) "none")
     (t "active"))))

(defun e-board-runs-shell--entry (projection)
  "Return a `tabulated-list' entry for durable run PROJECTION."
  (let ((conflicts (plist-get projection :conflicts)))
    (list (plist-get projection :run-id)
          (vector (plist-get projection :run-id)
                  (format "%s" (e-board-runs-shell--state-label projection))
                  (e-board-runs-shell--task-label projection)
                  (e-board-runs-shell--deadline-label projection)
                  (format "%s" (or (plist-get (plist-get projection :continuation) :state)
                                    "-"))
                  (if conflicts
                      (propertize (number-to-string (length conflicts))
                                  'face 'font-lock-warning-face)
                    "0")))))

(defconst e-board-runs-shell--hint-bindings
  '(("RET" . "details")
    ("g" . "refresh"))
  "Ordered key hints shown in the durable run list footer.")

(defun e-board-runs-shell--refresh ()
  "Rebuild the durable run list from its bounded board projections."
  (when (derived-mode-p 'e-board-runs-shell-mode)
    (setq tabulated-list-entries
          (mapcar #'e-board-runs-shell--entry
                  (e-board-orchestration-actions-list-runs e-board-runs-shell--board)))
    (tabulated-list-print t)
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (e-keymap-hints-insert e-board-runs-shell--hint-bindings)))))

(defun e-board-runs-shell-refresh ()
  "Manually rebuild the durable run list."
  (interactive)
  (e-board-runs-shell--refresh))

(defun e-board-runs-shell--refresh-buffers (board _run-id _projection)
  "Refresh live run buffers subscribed to BOARD projection notifications."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'e-board-runs-shell-mode)
                 (eq e-board-runs-shell--board board))
        (e-board-runs-shell--refresh)))))

(defun e-board-runs-shell--run-id-at-point ()
  "Return the durable run id on the current row, or signal."
  (or (tabulated-list-get-id)
      (user-error "No durable run on this line")))

(defun e-board-runs-shell-show-details ()
  "Show the bounded durable projection for the run at point."
  (interactive)
  (let* ((run-id (e-board-runs-shell--run-id-at-point))
         (projection (e-board-orchestration-actions-run-projection
                      e-board-runs-shell--board run-id))
         (workspace (e-buffer-ensure-workspace (current-buffer)))
         (buffer (get-buffer-create e-board-runs-shell-detail-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (pp-to-string projection))
        (special-mode))
      (e-buffer-set-workspace buffer workspace))
    (e-workspace-pop-to-buffer buffer)))

(defvar e-board-runs-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'e-board-runs-shell-show-details)
    (define-key map (kbd "g") #'e-board-runs-shell-refresh)
    map)
  "Keymap for `e-board-runs-shell-mode'.")

(define-derived-mode e-board-runs-shell-mode tabulated-list-mode "e-Board-Runs"
  "Major mode listing bounded durable run projections."
  (setq tabulated-list-format
        [("Run" 26 t)
         ("Status" 16 t)
         ("Tasks" 10 t)
         ("Deadline" 12 t)
         ("Continuation" 14 t)
         ("Conflicts" 10 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;;;###autoload
(cl-defun e-board-runs-list-buffer (&key board)
  "Open BOARD's durable run list and return its buffer."
  (interactive)
  (let ((board (e-board-orchestration-actions--source-board board))
        (buffer (get-buffer-create e-board-runs-shell-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'e-board-runs-shell-mode)
        (e-board-runs-shell-mode))
      (setq e-board-runs-shell--board board)
      (e-board-runs-shell--refresh))
    (add-hook 'e-board-orchestration-actions-projection-change-functions
              #'e-board-runs-shell--refresh-buffers)
    (when (called-interactively-p 'interactive)
      (e-workspace-pop-to-buffer buffer))
    buffer))

(provide 'e-board-runs-shell)

;;; e-board-runs-shell.el ends here

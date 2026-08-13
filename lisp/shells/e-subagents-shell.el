;;; e-subagents-shell.el --- Subagent list buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A `tabulated-list' buffer that lists a session's subagent children and lets
;; an operator watch, open, interrupt, and shut down each child.  It mirrors the
;; task-queue shell: read-only listing, workspace-aware display, and row actions
;; that reach the capability-owned registry.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'e-keymap-hints)
(require 'e-subagent-actions)
(require 'e-subagent-registry)
(require 'e-subagent-runner)
(require 'e-workspaces)

(declare-function e-chat-open-session "e-chat")
(declare-function e-harness-instance-get-or-create "e-harness-instances")

(defconst e-subagents-shell-buffer-name "*e-subagents*"
  "Name of the subagents list buffer.")

(defconst e-subagents-shell-progress-buffer-name "*e-subagent-progress*"
  "Name of the bounded subagent progress inspection buffer.")

(defcustom e-subagents-shell-soft-stale-checkpoints 2
  "Unchanged progress checkpoints required before the shell warns.
This is an observation threshold, not a lifecycle transition or cancellation
policy."
  :type 'integer
  :group 'e)

(defvar-local e-subagents-shell--registry nil
  "Subagent registry backing the current list buffer.")

(defvar-local e-subagents-shell--parent-session-id nil
  "Parent session id whose children the current list buffer shows.")

(defvar-local e-subagents-shell--progress-checkpoints nil
  "Latest progress sequence and unchanged count keyed by subagent id.")

(defun e-subagents-shell--status-label (status)
  "Return a short display label for subagent STATUS."
  (pcase status
    ('queued "queued")
    ('running "running")
    ('blocked "blocked")
    ('done "done")
    ('failed "failed")
    ('cancelled "cancelled")
    (_ (format "%s" status))))

(defun e-subagents-shell--age-label (at now)
  "Return a compact age label for AT relative to NOW."
  (if (numberp at)
      (format "%.0fs" (max 0.0 (- now at)))
    "-"))

(defun e-subagents-shell--stale-p (record)
  "Update and return the soft-stale observation for RECORD."
  (let* ((subagent-id (plist-get record :subagent-id))
         (sequence (plist-get record :progress-sequence))
         (previous (gethash subagent-id e-subagents-shell--progress-checkpoints))
         (unchanged (if (and previous (equal sequence (plist-get previous :sequence)))
                        (1+ (plist-get previous :unchanged))
                      1)))
    (puthash subagent-id
             (list :sequence sequence :unchanged unchanged)
             e-subagents-shell--progress-checkpoints)
    (and (eq (plist-get record :status) 'running)
         (>= unchanged e-subagents-shell-soft-stale-checkpoints))))

(defun e-subagents-shell--entry (record)
  "Return a `tabulated-list' entry for subagent RECORD."
  (let* ((now (float-time))
         (stale (e-subagents-shell--stale-p record))
         (progress (plist-get record :progress))
         (status (e-subagents-shell--status-label (plist-get record :status))))
    (list (plist-get record :subagent-id)
          (vector (or (plist-get record :label)
                      (format "%s" (plist-get record :type)))
                  (format "%s" (plist-get record :type))
                  (if stale (propertize status 'face 'font-lock-warning-face) status)
                  (e-subagents-shell--age-label (plist-get record :started-at) now)
                  (e-subagents-shell--age-label
                   (plist-get record :last-activity-at) now)
                  (if progress
                      (format "#%s %s"
                              (or (plist-get progress :sequence) 0)
                              (or (plist-get progress :summary) ""))
                    "-")
                  (or (plist-get record :result-summary) "")
                  (number-to-string (length (plist-get record :outputs)))))))

(defconst e-subagents-shell--hint-bindings
  '(("RET" . "open chat")
    ("g" . "refresh")
    ("s" . "steer")
    ("p" . "progress")
    ("i" . "interrupt")
    ("k" . "shutdown"))
  "Ordered key hints shown in the subagents list footer.")

(defun e-subagents-shell--records ()
  "Return the child records shown in the current buffer, newest-first."
  (e-subagent-registry-list e-subagents-shell--registry
                            e-subagents-shell--parent-session-id))

(defun e-subagents-shell--refresh ()
  "Rebuild the list buffer from its backing registry, preserving point."
  (when (derived-mode-p 'e-subagents-shell-mode)
    (setq tabulated-list-entries
          (mapcar #'e-subagents-shell--entry (e-subagents-shell--records)))
    (tabulated-list-print t)
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (e-keymap-hints-insert e-subagents-shell--hint-bindings)))))

(defun e-subagents-shell-refresh ()
  "Rebuild the list buffer from its backing registry."
  (interactive)
  (e-subagents-shell--refresh))

(defun e-subagents-shell--refresh-buffers (_registry)
  "Refresh every live subagents list buffer.
Bound to `e-subagent-registry-change-functions' so the list tracks live status."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'e-subagents-shell-mode)
        (e-subagents-shell--refresh)))))

(defun e-subagents-shell--subagent-id-at-point ()
  "Return the subagent id of the row at point, or signal."
  (or (tabulated-list-get-id)
      (user-error "No subagent on this line")))

(defun e-subagents-shell-steer ()
  "Steer the child at point with a prompt and optional audit reason."
  (interactive)
  (let* ((subagent-id (e-subagents-shell--subagent-id-at-point))
         (prompt (read-string "Steer prompt: "))
         (reason (read-string "Reason (optional): ")))
    (when (string-empty-p (string-trim prompt))
      (user-error "Steer prompt cannot be empty"))
    (e-subagent-steer e-subagents-shell--registry subagent-id prompt
                      (unless (string-empty-p (string-trim reason)) reason))
    (e-subagents-shell--refresh)))

(defun e-subagents-shell-progress ()
  "Show the child at point's latest progress and bounded transcript tail."
  (interactive)
  (let* ((subagent-id (e-subagents-shell--subagent-id-at-point))
         (record (e-subagent-registry-get e-subagents-shell--registry subagent-id))
         (tail (e-subagent-raw-read e-subagents-shell--registry subagent-id 10))
         (workspace (e-buffer-ensure-workspace (current-buffer)))
         (buffer (get-buffer-create e-subagents-shell-progress-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Subagent: %s\nStatus: %s\nProgress: %S\n\nTranscript tail:\n%S\n"
                        subagent-id (plist-get record :status)
                        (plist-get record :progress) tail))
        (special-mode))
      (e-buffer-set-workspace buffer workspace))
    (e-workspace-pop-to-buffer buffer)))

(defun e-subagents-shell-interrupt ()
  "Interrupt the subagent on the current row."
  (interactive)
  (e-subagent-interrupt e-subagents-shell--registry
                        (e-subagents-shell--subagent-id-at-point))
  (e-subagents-shell--refresh))

(defun e-subagents-shell-shutdown ()
  "Shut down the subagent on the current row."
  (interactive)
  (e-subagent-shutdown e-subagents-shell--registry
                       (e-subagents-shell--subagent-id-at-point))
  (e-subagents-shell--refresh))

(defun e-subagents-shell-open-chat ()
  "Open the child chat session for the subagent on the current row.
Opens the child session on its own type instance, giving the operator a full
live chat with the child."
  (interactive)
  (let* ((subagent-id (e-subagents-shell--subagent-id-at-point))
         (record (e-subagent-registry-get e-subagents-shell--registry
                                          subagent-id))
         (session-id (plist-get record :session-id))
         (type (plist-get record :type)))
    (unless session-id
      (user-error "Subagent %s has no session yet" subagent-id))
    (unless (require 'e-chat nil t)
      (user-error "e-chat is not available to open the session"))
    (let ((harness (e-harness-instance-get-or-create type)))
      (e-chat-open-session harness session-id t type))))

(defvar e-subagents-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'e-subagents-shell-steer)
    (define-key map (kbd "p") #'e-subagents-shell-progress)
    (define-key map (kbd "i") #'e-subagents-shell-interrupt)
    (define-key map (kbd "k") #'e-subagents-shell-shutdown)
    (define-key map (kbd "RET") #'e-subagents-shell-open-chat)
    (define-key map (kbd "g") #'e-subagents-shell-refresh)
    map)
  "Keymap for `e-subagents-shell-mode'.")

(define-derived-mode e-subagents-shell-mode tabulated-list-mode "e-Subagents"
  "Major mode listing a session's subagent children newest-first."
  (setq tabulated-list-format
        [("Label" 28 nil)
         ("Type" 16 t)
         ("Status" 12 t)
         ("Runtime" 10 t)
         ("Last activity" 14 t)
         ("Progress" 32 nil)
         ("Result" 40 nil)
         ("Outputs" 8 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun e-subagents-shell--configure-modal-editing ()
  "Keep this read-only listing in Evil `emacs' state so the mode map is honored."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'e-subagents-shell-mode 'emacs)))

(with-eval-after-load 'evil
  (e-subagents-shell--configure-modal-editing))

;;;###autoload
(cl-defun e-subagents-list-buffer (&key registry parent-session-id)
  "Open the subagents list buffer and return it.
REGISTRY defaults to `e-subagent-actions-default-registry'.  PARENT-SESSION-ID,
when non-nil, scopes the list to that parent's direct children."
  (interactive)
  (let ((registry (or registry e-subagent-actions-default-registry))
        (buffer (get-buffer-create e-subagents-shell-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'e-subagents-shell-mode)
        (e-subagents-shell-mode))
      (setq e-subagents-shell--registry registry)
      (setq e-subagents-shell--parent-session-id parent-session-id)
      (setq e-subagents-shell--progress-checkpoints (make-hash-table :test 'equal))
      (e-subagents-shell--refresh))
    (add-hook 'e-subagent-registry-change-functions
              #'e-subagents-shell--refresh-buffers)
    (when (called-interactively-p 'interactive)
      (e-workspace-pop-to-buffer buffer))
    buffer))

(provide 'e-subagents-shell)

;;; e-subagents-shell.el ends here

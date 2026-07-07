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

(defvar-local e-subagents-shell--registry nil
  "Subagent registry backing the current list buffer.")

(defvar-local e-subagents-shell--parent-session-id nil
  "Parent session id whose children the current list buffer shows.")

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

(defun e-subagents-shell--entry (record)
  "Return a `tabulated-list' entry for subagent RECORD."
  (list (plist-get record :subagent-id)
        (vector (or (plist-get record :label)
                    (format "%s" (plist-get record :type)))
                (format "%s" (plist-get record :type))
                (e-subagents-shell--status-label (plist-get record :status))
                (or (plist-get record :result-summary) "")
                (number-to-string (length (plist-get record :outputs))))))

(defconst e-subagents-shell--hint-bindings
  '(("RET" . "open chat")
    ("g" . "refresh")
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
      (e-subagents-shell--refresh))
    (add-hook 'e-subagent-registry-change-functions
              #'e-subagents-shell--refresh-buffers)
    (when (called-interactively-p 'interactive)
      (e-workspace-pop-to-buffer buffer))
    buffer))

(provide 'e-subagents-shell)

;;; e-subagents-shell.el ends here

;;; e-cron-shell.el --- Cron schedule overview buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A `tabulated-list' buffer that lists every registered `e-cron' schedule with
;; its recurrence, next and last fire, last guard result, and enabled state.  It
;; reads the same registry the engine arms from, so what it shows is the live
;; schedule state, not a separate copy.  Row actions enable, disable, or remove
;; a schedule.  The buffer refreshes on cron-fire events and on manual refresh.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'e-cron)
(require 'e-workspaces)

(defconst e-cron-shell-buffer-name "*e-cron*"
  "Name of the cron schedule overview buffer.")

(defun e-cron-shell--when-label (when)
  "Return a short display label for recurrence WHEN."
  (cond
   ((plist-member when :every)
    (format "every %ss" (plist-get when :every)))
   ((plist-member when :at)
    (let ((on (plist-get when :on)))
      (format "at %s%s" (plist-get when :at)
              (if on (format " on %s"
                             (mapconcat #'symbol-name on ","))
                ""))))
   (t (format "%S" when))))

(defun e-cron-shell--time-label (time)
  "Return a short local-time label for TIME, or an empty string."
  (if time (format-time-string "%m-%d %H:%M:%S" time) ""))

(defun e-cron-shell--guard-label (schedule)
  "Return a short label for SCHEDULE's guard state."
  (cond
   ((null (e-cron-schedule-guard schedule)) "-")
   ((null (e-cron-schedule-last-guard-at schedule)) "?")
   ((e-cron-schedule-last-guard-result schedule) "pass")
   (t "skip")))

(defun e-cron-shell--entry (schedule)
  "Return a `tabulated-list' entry for SCHEDULE."
  (list (format "%s" (e-cron-schedule-id schedule))
        (vector (format "%s" (e-cron-schedule-id schedule))
                (if (e-cron-schedule-enabled schedule) "on" "off")
                (e-cron-shell--when-label (e-cron-schedule-when schedule))
                (e-cron-shell--time-label (e-cron-schedule-next-fire schedule))
                (e-cron-shell--time-label (e-cron-schedule-last-fire schedule))
                (e-cron-shell--guard-label schedule))))

(defun e-cron-shell--refresh ()
  "Rebuild the overview buffer from the live schedule registry."
  (when (derived-mode-p 'e-cron-shell-mode)
    (setq tabulated-list-entries
          (mapcar #'e-cron-shell--entry (e-cron-list)))
    (tabulated-list-print t)))

(defun e-cron-shell-refresh ()
  "Rebuild the overview buffer from the live schedule registry."
  (interactive)
  (e-cron-shell--refresh))

(defun e-cron-shell--refresh-buffers (&rest _)
  "Refresh every live cron overview buffer.
Bound to `e-cron-fire-functions' so the list tracks live fire state."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'e-cron-shell-mode)
        (e-cron-shell--refresh)))))

(defun e-cron-shell--id-at-point ()
  "Return the schedule id symbol of the row at point, or signal."
  (if-let ((id (tabulated-list-get-id)))
      (intern id)
    (user-error "No schedule on this line")))

(defun e-cron-shell-enable ()
  "Enable the schedule on the current row."
  (interactive)
  (e-cron-enable (e-cron-shell--id-at-point))
  (e-cron-shell--refresh))

(defun e-cron-shell-disable ()
  "Disable the schedule on the current row."
  (interactive)
  (e-cron-disable (e-cron-shell--id-at-point))
  (e-cron-shell--refresh))

(defun e-cron-shell-remove ()
  "Stop and unregister the schedule on the current row."
  (interactive)
  (let ((id (e-cron-shell--id-at-point)))
    (when (yes-or-no-p (format "Remove schedule %s? " id))
      (e-cron-remove id)
      (e-cron-shell--refresh))))

(defvar e-cron-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'e-cron-shell-enable)
    (define-key map (kbd "d") #'e-cron-shell-disable)
    (define-key map (kbd "k") #'e-cron-shell-remove)
    (define-key map (kbd "g") #'e-cron-shell-refresh)
    map)
  "Keymap for `e-cron-shell-mode'.")

(define-derived-mode e-cron-shell-mode tabulated-list-mode "e-Cron"
  "Major mode listing e cron schedules."
  (setq tabulated-list-format
        [("Id" 22 t)
         ("On" 4 t)
         ("When" 22 nil)
         ("Next" 15 nil)
         ("Last" 15 nil)
         ("Guard" 6 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;;;###autoload
(defun e-cron-schedules ()
  "Open the cron schedule overview buffer and return it."
  (interactive)
  (let ((buffer (get-buffer-create e-cron-shell-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'e-cron-shell-mode)
        (e-cron-shell-mode))
      (e-cron-shell--refresh))
    (add-hook 'e-cron-fire-functions #'e-cron-shell--refresh-buffers)
    (when (called-interactively-p 'interactive)
      (e-workspace-pop-to-buffer buffer))
    buffer))

(provide 'e-cron-shell)

;;; e-cron-shell.el ends here

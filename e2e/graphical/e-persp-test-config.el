;;; e-persp-test-config.el --- Minimal persp-mode graphical fixture -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A deliberately small non-Doom workspace configuration for graphical E2E.
;; It mirrors the persp-mode settings and delete-window policy relevant to the
;; user's setup without loading Doom's popup, project, modeline, or persistence
;; layers.

;;; Code:

(require 'cl-lib)
(require 'persp-mode)

(defconst e-persp-test-config-main-workspace "main"
  "Name of the fixture's protected initial workspace.")

(defun e-persp-test-config--window-roots ()
  "Return distinct ordinary or atomic roots visible on the selected frame."
  (delete-dups
   (mapcar (lambda (window)
             (or (window-atom-root window) window))
           (window-list nil 'nomini))))

(defun e-persp-test-config-delete-current-workspace (&optional next-name)
  "Delete the current perspective and switch to NEXT-NAME when supplied."
  (let ((current (safe-persp-name (get-current-persp))))
    (when (equal current e-persp-test-config-main-workspace)
      (user-error "Cannot delete the main test workspace"))
    (persp-kill current nil nil)
    (when next-name
      (unless (persp-with-name-exists-p next-name)
        (user-error "No perspective named %s" next-name))
      (persp-frame-switch next-name))
    current))

(defun e-persp-test-config-close-window-or-workspace ()
  "Close the selected window atom, or delete its last-window workspace."
  (interactive)
  (if (cdr (e-persp-test-config--window-roots))
      (delete-window)
    (let* ((current (safe-persp-name (get-current-persp)))
           (next
            (cl-find-if
             (lambda (name)
               (and (not (equal name current))
                    (not (equal name persp-nil-name))))
             (persp-names))))
      (unless next
        (user-error "Cannot delete the last test workspace"))
      (e-persp-test-config-delete-current-workspace next))))

(defun e-persp-test-config-apply (save-directory)
  "Enable the basic persp fixture using SAVE-DIRECTORY."
  (when (bound-and-true-p persp-mode)
    (persp-mode -1)
    (sit-for 0.01))
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (file-name-as-directory save-directory)
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1
        persp-auto-save-opt 0
        persp-init-frame-behaviour t)
  (persp-mode 1)
  (unless (persp-with-name-exists-p e-persp-test-config-main-workspace)
    (persp-add-new e-persp-test-config-main-workspace))
  (persp-frame-switch e-persp-test-config-main-workspace)
  (define-key persp-mode-map [remap delete-window]
              #'e-persp-test-config-close-window-or-workspace)
  e-persp-test-config-main-workspace)

(provide 'e-persp-test-config)

;;; e-persp-test-config.el ends here

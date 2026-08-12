;;; e-window-surfaces.el --- Native window surfaces for presentation shells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Presentation shells may compose several Emacs windows into one native
;; atomic surface.  These helpers let another shell replace that surface
;; without depending on the implementation that created it.

;;; Code:

(defun e-window-surface-replace (&optional window)
  "Return one ordinary window occupying WINDOW's complete surface.
WINDOW defaults to the selected window.  When WINDOW belongs to an atomic
surface, create a replacement beside the atomic root and delete that root as
one unit.  The replacement expands into the vacated space.  An ordinary
WINDOW is returned unchanged.

The caller owns the replacement's buffer, layout, and selection."
  (let* ((window (or window (selected-window)))
         (atom-root (and (window-live-p window)
                         (window-atom-root window))))
    (unless (window-live-p window)
      (user-error "Cannot replace a dead window surface"))
    (if (and atom-root (not (eq atom-root window)))
        (let ((replacement (split-window atom-root nil 'below)))
          (delete-window atom-root)
          replacement)
      window)))

(provide 'e-window-surfaces)

;;; e-window-surfaces.el ends here

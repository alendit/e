;;; e-window-surface-behavior-test.el --- Graphical window surface contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Native graphical coverage for replacing composed presentation surfaces.

;;; Code:

(require 'ert)
(require 'e-window-surfaces)

(ert-deftest e-window-surface-behavior-test-keeps-ordinary-window ()
  "Replacing an ordinary surface is an identity operation."
  (should (display-graphic-p))
  (save-window-excursion
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (let ((window (selected-window)))
      (should-not (window-atom-root window))
      (should (eq (e-window-surface-replace window) window))
      (should (window-live-p window)))))

(ert-deftest e-window-surface-behavior-test-replaces-atomic-surface-as-one-unit ()
  "A shell can replace an atomic surface without knowing who composed it."
  (should (display-graphic-p))
  (let ((configuration (current-window-configuration))
        (frame-size (cons (frame-width) (frame-height)))
        (first-buffer (generate-new-buffer "*e surface first*"))
        (second-buffer (generate-new-buffer "*e surface second*"))
        (external-buffer (generate-new-buffer "*e surface external*")))
    (unwind-protect
        (progn
          (let ((ignore-window-parameters t))
            (delete-other-windows))
          (set-frame-size (selected-frame) 140 48)
          (let* ((first-window (selected-window))
                 (external-window (split-window first-window nil 'right))
                 (second-window (split-window first-window nil 'below))
                 (atom-root (window-parent first-window))
                 (surface-edges (window-edges atom-root)))
            (set-window-buffer first-window first-buffer)
            (set-window-buffer second-window second-buffer)
            (set-window-buffer external-window external-buffer)
            (window-make-atom atom-root)
            (select-window second-window)
            (let ((replacement (e-window-surface-replace)))
              (should (window-live-p replacement))
              (should-not (window-live-p first-window))
              (should-not (window-live-p second-window))
              (should-not (window-atom-root replacement))
              (should (equal (window-edges replacement) surface-edges))
              (should (window-live-p external-window))
              (should (eq (window-buffer external-window) external-buffer))
              ;; The returned window is immediately safe for a caller-owned
              ;; proportional layout based on its own complete dimensions.
              (should (window-live-p
                       (split-window replacement
                                     (/ (* (window-total-height replacement) 2)
                                        3)
                                     'below))))))
      (when (window-configuration-p configuration)
        (set-window-configuration configuration))
      (set-frame-size (selected-frame) (car frame-size) (cdr frame-size))
      (dolist (buffer (list first-buffer second-buffer external-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'e-window-surface-behavior-test)

;;; e-window-surface-behavior-test.el ends here

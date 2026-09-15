;;; e-graphical-test-suite.el --- Load the graphical E2E matrix -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Default graphical E2E entrypoint.  Individual files remain loadable through
;; E_GRAPHICAL_E2E_TEST_FILE for focused framework diagnostics.

;;; Code:

(let ((directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "e-chat-behavior-test.el" directory) nil nil t)
  (load (expand-file-name "e-board-activity-behavior-test.el" directory)
        nil nil t)
  (load (expand-file-name "e-runtime-store-recovery-behavior-test.el" directory) nil nil t)
  (load (expand-file-name "e-window-surface-behavior-test.el" directory) nil nil t)
  (load (expand-file-name "e-workspace-behavior-test.el" directory) nil nil t))

(provide 'e-graphical-test-suite)

;;; e-graphical-test-suite.el ends here

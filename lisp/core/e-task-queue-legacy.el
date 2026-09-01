;;; e-task-queue-legacy.el --- Pure legacy task snapshot decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; P4 migration may inventory the retired file representation through this
;; decoder.  It performs no import, mutation, fallback, or runtime selection.

;;; Code:

(defun e-task-queue-legacy-decode-file (file)
  "Return the legacy task snapshot in FILE, or nil when FILE is absent."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (goto-char (point-min))
      (read (current-buffer)))))

(provide 'e-task-queue-legacy)

;;; e-task-queue-legacy.el ends here

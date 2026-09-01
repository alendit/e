;;; e-raw-results-legacy.el --- Pure legacy raw-result decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(defun e-raw-results-legacy-decode-file (file)
  "Return exact UTF-8 text from legacy raw-result FILE, or nil when absent."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents file))
      (buffer-string))))

(provide 'e-raw-results-legacy)

;;; e-raw-results-legacy.el ends here

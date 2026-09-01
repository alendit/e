;;; e-voice-adjustment-legacy.el --- Pure legacy voice decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(defun e-voice-adjustment-legacy-decode-file (file)
  "Return the legacy voice tell list in FILE, or nil when absent."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((value (read (current-buffer))))
        (unless (listp value)
          (signal 'wrong-type-argument (list 'listp value)))
        value))))

(provide 'e-voice-adjustment-legacy)

;;; e-voice-adjustment-legacy.el ends here

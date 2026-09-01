;;; e-cron-legacy.el --- Pure legacy cron-state decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(defun e-cron-legacy-decode-file (file)
  "Return the legacy cron state hash table in FILE.
Return nil when FILE is absent and signal when its value has the wrong shape."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((value (read (current-buffer))))
        (unless (hash-table-p value)
          (signal 'wrong-type-argument (list 'hash-table-p value)))
        value))))

(provide 'e-cron-legacy)

;;; e-cron-legacy.el ends here

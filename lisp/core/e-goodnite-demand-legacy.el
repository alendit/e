;;; e-goodnite-demand-legacy.el --- Pure legacy demand-log decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'json)
(require 'subr-x)

(defun e-goodnite-demand-legacy-decode-file (file)
  "Return ordered demand objects decoded from legacy JSONL FILE.
Return nil when FILE is absent.  Malformed non-empty lines signal."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (events)
        (while (not (eobp))
          (let ((line (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))
            (unless (string-empty-p line)
              (push (json-parse-string
                     line :object-type 'plist :array-type 'list
                     :false-object nil :null-object nil)
                    events)))
          (forward-line 1))
        (nreverse events)))))

(provide 'e-goodnite-demand-legacy)

;;; e-goodnite-demand-legacy.el ends here

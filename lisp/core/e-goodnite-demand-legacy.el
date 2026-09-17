;;; e-goodnite-demand-legacy.el --- Pure legacy demand-log decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'e-json)
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
              ;; This reader is intentionally named as the compatibility
              ;; boundary for the retired JSONL demand log.  Its domain is
              ;; still the ordered list of event plists; the physical value
              ;; is canonical before this projection.
              (push (e-goodnite-demand-legacy--domain-value
                     (e-json-parse-string line))
                    events)))
          (forward-line 1))
        (nreverse events)))))

(defun e-goodnite-demand-legacy--domain-value (value)
  "Project canonical JSON VALUE into the retired demand-log domain.

The old reader exposed arrays as lists.  Keep that compatibility contract in
  this explicitly named adapter instead of teaching `e-json' to accept lists."
  (cond
   ((eq value e-json-null) nil)
   ((vectorp value)
    (mapcar #'e-goodnite-demand-legacy--domain-value (append value nil)))
   ((and (consp value)
         (cl-evenp (length value))
         (cl-loop for (key _item) on value by #'cddr always (keywordp key)))
    (let ((copy (copy-sequence value))
          (tail value))
      (while tail
        (let ((key (pop tail))
              (item (pop tail)))
          (plist-put copy key
                     (e-goodnite-demand-legacy--domain-value item))))
      copy))
   (t value)))

(provide 'e-goodnite-demand-legacy)

;;; e-goodnite-demand-legacy.el ends here

;;; e-raw-result-cleanup.el --- Mixed raw-result cleanup for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Cleanup helpers for owners that may hold both session-owned tmp:// raw result
;; references and generic raw-result:// references.

;;; Code:

(require 'subr-x)
(require 'e-raw-results)
(require 'e-session-tmp-resources)

(defun e-raw-result-cleanup--storage (reference)
  "Return the raw-result storage kind for REFERENCE, or nil."
  (cond
   ((and (listp reference) (plist-get reference :storage))
    (plist-get reference :storage))
   ((and (stringp reference) (string-prefix-p "tmp://" reference))
    'session-tmp)
   ((and (stringp reference) (string-prefix-p "raw-result://" reference))
    'raw-result-store)
   (t nil)))

(defun e-raw-result-cleanup-reference (harness session-id reference)
  "Delete raw-result REFERENCE for HARNESS SESSION-ID.
REFERENCE may be backed by session tmp resources or by the generic raw-result
store.  Return the deleted file path, or nil when REFERENCE is not a known
raw-result reference or the target is already absent."
  (pcase (e-raw-result-cleanup--storage reference)
    ('session-tmp
     (e-session-tmp-cleanup-reference harness session-id reference))
    ('raw-result-store
     (e-raw-results-cleanup-reference reference))
    (_ nil)))

(defun e-raw-result-cleanup-references (harness session-id references)
  "Delete mixed raw-result REFERENCES for HARNESS SESSION-ID.
Return the list of deleted file paths."
  (delq nil
        (mapcar (lambda (reference)
                  (e-raw-result-cleanup-reference
                   harness session-id reference))
                references)))

(provide 'e-raw-result-cleanup)

;;; e-raw-result-cleanup.el ends here

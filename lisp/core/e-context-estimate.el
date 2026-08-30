;;; e-context-estimate.el --- Pure context size estimation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This is the lower-level value contract shared by context-budget and
;; context-lifetime.  It owns the established ratio setting and exact
;; UTF-8/prin1 token estimate without loading either higher-level owner.

;;; Code:

(defgroup e-context-budget nil
  "Core context budget accounting for e sessions."
  :group 'e)

(defcustom e-context-budget-estimate-bytes-per-token 4.0
  "Approximate UTF-8 bytes per token for context-token estimates."
  :type 'number
  :group 'e-context-budget)

(defun e-context-estimate-effective-bytes-per-token (&optional bytes-per-token)
  "Return the effective positive estimate ratio.
BYTES-PER-TOKEN overrides the configured ratio.  Invalid or non-positive
values use the established 4.0 fallback."
  (let ((ratio (or bytes-per-token
                   e-context-budget-estimate-bytes-per-token)))
    (if (and (numberp ratio) (> ratio 0))
        ratio
      4.0)))

(defun e-context-budget-value-token-estimate
    (value &optional bytes-per-token)
  "Return approximate token count for canonical model-facing VALUE.
BYTES-PER-TOKEN defaults to `e-context-budget-estimate-bytes-per-token'.
Invalid or non-positive ratios use the established 4.0 fallback.  VALUE is
already the semantic value being estimated; callers that add presentation
markers must invoke this helper before doing so."
  (let ((bytes (string-bytes (prin1-to-string value)))
        (ratio (e-context-estimate-effective-bytes-per-token bytes-per-token)))
    (ceiling (/ bytes (float ratio)))))

(provide 'e-context-estimate)

;;; e-context-estimate.el ends here

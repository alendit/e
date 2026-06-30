;;; e-context-status.el --- Shared context-state status text for e shells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Background computation for the context-state indicator that presentation
;; shells show in their status line: the active model, reasoning effort, and the
;; context-token usage or estimate against the model context-token limit.
;;
;; This module is shell-agnostic presentation.  Core token accounting lives in
;; `e-context-budget'; callers own where the text is rendered (mode line, header
;; line, ...) and own any caching by passing an opaque cache cell.

;;; Code:

(require 'cl-lib)
(require 'e-context-budget)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(defgroup e-context-status nil
  "Shared context-state status presentation for e shells."
  :group 'e)

(define-obsolete-variable-alias
  'e-context-status-model-token-limits
  'e-context-budget-model-token-limits
  "0.1.0")

(define-obsolete-variable-alias
  'e-context-status-estimate-bytes-per-token
  'e-context-budget-estimate-bytes-per-token
  "0.1.0")

(defcustom e-context-status-estimate-cache-seconds 2.0
  "Seconds to reuse approximate context-token estimates for status refreshes."
  :type 'number
  :group 'e-context-status)

(defun e-context-status--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-context-status--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when developer profiling is enabled."
  (if (e-context-status--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-context-status-model-token-limit (model &optional limits)
  "Return configured max context tokens for MODEL, or nil.
LIMITS defaults to `e-context-budget-model-token-limits'."
  (e-context-budget-model-window model limits))

(defun e-context-status-context-token-estimate (context &optional bytes-per-token)
  "Return approximate token count for model-facing CONTEXT.
BYTES-PER-TOKEN defaults to `e-context-budget-estimate-bytes-per-token'."
  (e-context-budget-context-token-estimate context bytes-per-token))

(defun e-context-status-format-token-count (tokens)
  "Return compact display text for TOKENS."
  (cond
   ((not (and (integerp tokens) (>= tokens 0)))
    "?")
   ((>= tokens 1000000)
    (let ((millions (/ tokens 1000000.0)))
      (replace-regexp-in-string
       "\\.?0+M\\'"
       "M"
       (format "%.2fM" millions))))
   ((and (>= tokens 1000)
         (< tokens 10000)
         (/= (% tokens 1000) 0))
    (replace-regexp-in-string
     "\\.?0+k\\'"
     "k"
     (format "%.1fk" (/ tokens 1000.0))))
   ((>= tokens 1000)
    (format "%dk" (round (/ tokens 1000.0))))
   (t
    (number-to-string tokens))))

(defun e-context-status-format
    (prefix model effort used-tokens max-tokens &optional approximate)
  "Return compact status text for PREFIX, MODEL, EFFORT, and token usage."
  (let ((model-text (or model "model unset"))
        (effort-text (or effort "effort unset")))
    (if (and (integerp used-tokens) (>= used-tokens 0))
        (let ((used-text (e-context-status-format-token-count used-tokens))
              (mark (if approximate "~" "")))
          (if (and (integerp max-tokens) (> max-tokens 0))
              (let ((percent
                     (if (= used-tokens 0)
                         0
                       (funcall (if approximate #'ceiling #'floor)
                                (* 100.0
                                   (/ used-tokens
                                      (float max-tokens)))))))
                (format "%s %s/%s %s%d%% (%s%s/%s tok)"
                        prefix
                        model-text
                        effort-text
                        mark
                        percent
                        mark
                        used-text
                        (e-context-status-format-token-count max-tokens)))
            (format "%s %s/%s ?%% (%s%s/? tok)"
                    prefix model-text effort-text mark used-text)))
      (format "%s %s/%s" prefix model-text effort-text))))

(cl-defun e-context-status-text
    (harness session-id
             &key (prefix "e-context") prefer-token-usage estimate-cache
             estimate-cache-key
             token-limits token-limit-function bytes-per-token
             (estimate-context t))
  "Return context-state status text for SESSION-ID through HARNESS.
PREFIX is the leading label.  When PREFER-TOKEN-USAGE is non-nil and fresh
provider usage exists, skip the expensive context-token estimate.
ESTIMATE-CACHE is an optional caller-owned cons cell (TOKENS . TIME) reused
across calls.  ESTIMATE-CACHE-KEY, when non-nil, invalidates cached estimates
when the caller's semantic context state changes.
TOKEN-LIMIT-FUNCTION, when non-nil, is called with the model id and should
return its context window in tokens or nil; it takes precedence over the
static TOKEN-LIMITS alias.  TOKEN-LIMITS and BYTES-PER-TOKEN override the
configured defaults.
When ESTIMATE-CONTEXT is nil, avoid building full model-facing context."
  (e-context-status--profile-call
   'context-status.text
   (list :session-id session-id
         :metadata (list :prefix prefix
                         :prefer-token-usage (and prefer-token-usage t)
                         :estimate-context (and estimate-context t)
                         :estimate-cache-key-present
                         (and estimate-cache-key t)))
   (lambda ()
     (if-let ((status (e-context-budget-status
                       harness session-id
                       :prefer-token-usage prefer-token-usage
                       :estimate-cache estimate-cache
                       :token-limits token-limits
                       :token-limit-function token-limit-function
                       :bytes-per-token bytes-per-token
                       :estimate-cache-seconds
                       e-context-status-estimate-cache-seconds
                       :estimate-cache-key estimate-cache-key
                       :estimate-context estimate-context)))
         (let ((model (plist-get status :model))
               (effort (plist-get status :reasoning-effort))
               (used-tokens (plist-get status :used-tokens))
               (max-tokens (plist-get status :window))
               (approximate (plist-get status :approximate)))
           (e-context-status-format
            prefix model effort used-tokens max-tokens approximate))
       prefix))))

(provide 'e-context-status)

;;; e-context-status.el ends here

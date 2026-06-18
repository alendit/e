;;; e-context-status.el --- Shared context-state status text for e shells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Background computation for the context-state indicator that presentation
;; shells show in their status line: the active model, reasoning effort, and the
;; context-token usage or estimate against the model context-token limit.
;;
;; This module is shell-agnostic.  It depends only on stable harness/session
;; reads plus presentation-owned configuration, and it never mutates buffer
;; state.  Callers own where the text is rendered (mode line, header line, ...)
;; and own any caching by passing an opaque cache cell.

;;; Code:

(require 'cl-lib)
(require 'e-harness)
(require 'e-session)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(defgroup e-context-status nil
  "Shared context-state status presentation for e shells."
  :group 'e)

(defcustom e-context-status-model-token-limits
  '(("gpt-5.5" . 258400)
    ("gpt-5.4" . 1050000)
    ("gpt-5.4-pro" . 1050000)
    ("gpt-5.3-codex" . 400000)
    ("gpt-5.3-codex-spark" . 400000)
    ("gpt-5.2" . 400000)
    ("gpt-5.1" . 400000)
    ("gpt-5.1-codex" . 400000)
    ("gpt-5-codex" . 400000)
    ("gpt-5-mini" . 400000)
    ("gpt-5-nano" . 400000)
    ("gpt-5" . 400000)
    ("gpt-5-chat-latest" . 128000))
  "Alist mapping model names to maximum context tokens.
Shells use this presentation-owned table for context usage display."
  :type '(alist :key-type string :value-type integer)
  :group 'e-context-status)

(defcustom e-context-status-estimate-bytes-per-token 4.0
  "Approximate UTF-8 bytes per token for context-token estimates."
  :type 'number
  :group 'e-context-status)

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
LIMITS defaults to `e-context-status-model-token-limits'."
  (when (stringp model)
    (cdr (assoc-string model
                       (or limits e-context-status-model-token-limits)
                       t))))

(defun e-context-status-context-token-estimate (context &optional bytes-per-token)
  "Return approximate token count for model-facing CONTEXT.
BYTES-PER-TOKEN defaults to `e-context-status-estimate-bytes-per-token'."
  (let* ((options (plist-get context :options))
         (model-facing-context
          (list :messages (plist-get context :messages)
                :tools (plist-get options :tools)))
         (bytes (string-bytes (prin1-to-string model-facing-context)))
         (per-token (or bytes-per-token
                        e-context-status-estimate-bytes-per-token))
         (per-token (if (and (numberp per-token) (> per-token 0))
                        per-token
                      4.0)))
    (ceiling (/ bytes (float per-token)))))

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

(defun e-context-status--token-usage-input-tokens (usage)
  "Return input token count from provider-neutral USAGE."
  (let ((tokens (or (plist-get usage :input-tokens)
                    (plist-get usage :input_tokens))))
    (when (and (integerp tokens) (>= tokens 0))
      tokens)))

(defun e-context-status--latest-token-usage-event (harness session-id)
  "Return latest durable provider token usage event for SESSION-ID."
  (when (and harness session-id)
    (let (usage-event)
      (dolist (event (ignore-errors
                       (e-harness-session-activity-events harness session-id))
                     usage-event)
        (when (eq (plist-get event :event-type) 'token-usage)
          (setq usage-event event))))))

(defun e-context-status--latest-valid-compaction (harness session-id)
  "Return latest valid compaction for SESSION-ID."
  (when (and harness session-id)
    (ignore-errors
      (e-session-latest-valid-compaction
       (e-harness-sessions harness)
       session-id))))

(defun e-context-status--session-exists-p (harness session-id)
  "Return non-nil when HARNESS has SESSION-ID."
  (and harness
       session-id
       (ignore-errors
         (e-session-get (e-harness-sessions harness) session-id)
         t)))

(defun e-context-status--usage-before-compaction-p (usage-event compaction)
  "Return non-nil when USAGE-EVENT predates COMPACTION."
  (let ((usage-time (plist-get usage-event :created-at))
        (compaction-time (plist-get compaction :created-at))
        (usage-id (plist-get usage-event :id))
        (compaction-id (plist-get compaction :id)))
    (and (stringp usage-time)
         (stringp compaction-time)
         (or (string< usage-time compaction-time)
             (and (equal usage-time compaction-time)
                  (stringp usage-id)
                  (stringp compaction-id)
                  (string< usage-id compaction-id))))))

(defun e-context-status--cached-estimate (context cache bytes-per-token)
  "Return cached approximate token estimate for CONTEXT.
CACHE is a caller-owned cons cell of (TOKENS . TIME) or nil; when non-nil it is
mutated in place.  BYTES-PER-TOKEN is forwarded to the estimator."
  (let* ((now (float-time))
         (ttl (if (and (numberp e-context-status-estimate-cache-seconds)
                       (>= e-context-status-estimate-cache-seconds 0))
                  e-context-status-estimate-cache-seconds
                2.0)))
    (if (and (consp cache)
             (integerp (car cache))
             (numberp (cdr cache))
             (< (- now (cdr cache)) ttl))
        (car cache)
      (let ((tokens (e-context-status-context-token-estimate
                     context bytes-per-token)))
        (when (consp cache)
          (setcar cache tokens)
          (setcdr cache now))
        tokens))))

(cl-defun e-context-status-text
    (harness session-id
             &key (prefix "e-context") prefer-token-usage estimate-cache
             token-limits token-limit-function bytes-per-token
             (estimate-context t))
  "Return context-state status text for SESSION-ID through HARNESS.
PREFIX is the leading label.  When PREFER-TOKEN-USAGE is non-nil and fresh
provider usage exists, skip the expensive context-token estimate.
ESTIMATE-CACHE is an optional caller-owned cons cell (TOKENS . TIME) reused
across calls.
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
                         :estimate-context (and estimate-context t)))
   (lambda ()
     (if (e-context-status--session-exists-p harness session-id)
         (let* ((usage-event (e-context-status--latest-token-usage-event
                              harness session-id))
                (latest-compaction (e-context-status--latest-valid-compaction
                                     harness session-id))
                (usage-tokens
                 (unless (e-context-status--usage-before-compaction-p
                          usage-event latest-compaction)
                   (e-context-status--token-usage-input-tokens
                    (plist-get usage-event :payload))))
                (context (when (and estimate-context
                                    (not (and prefer-token-usage usage-tokens)))
                           (ignore-errors
                             (e-harness-context harness session-id))))
                (options (or (plist-get context :options)
                             (ignore-errors
                               (e-harness-turn-options harness session-id))))
                (model (plist-get options :model))
                (effort (plist-get options :reasoning-effort))
                (estimated-tokens
                 (and (not usage-tokens)
                      context
                      (ignore-errors
                        (e-context-status--cached-estimate
                         context estimate-cache bytes-per-token))))
                (used-tokens (or usage-tokens estimated-tokens))
                (max-tokens (or (and token-limit-function
                                     (ignore-errors
                                       (funcall token-limit-function model)))
                                (e-context-status-model-token-limit
                                 model token-limits))))
           (e-context-status-format
            prefix model effort used-tokens max-tokens (not usage-tokens)))
       prefix))))

(provide 'e-context-status)

;;; e-context-status.el ends here

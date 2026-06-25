;;; e-context-budget.el --- Core context budget accounting for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; UI-free context budget accounting shared by harness policy and presentation
;; shells.  This module owns model context windows and used-token computation.

;;; Code:

(require 'cl-lib)
(require 'e-context)
(require 'e-session)

(declare-function e-harness-context "e-harness")
(declare-function e-harness-sessions "e-harness")
(declare-function e-harness-turn-options "e-harness")
(declare-function e-session-latest-token-usage-event "e-session")

(defgroup e-context-budget nil
  "Core context budget accounting for e sessions."
  :group 'e)

(defcustom e-context-budget-model-token-limits
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
  "Alist mapping model names to maximum context tokens."
  :type '(alist :key-type string :value-type integer)
  :group 'e-context-budget)

(defcustom e-context-budget-estimate-bytes-per-token 4.0
  "Approximate UTF-8 bytes per token for context-token estimates."
  :type 'number
  :group 'e-context-budget)

(defun e-context-budget-model-window (model &optional limits)
  "Return configured max context tokens for MODEL, or nil.
LIMITS defaults to `e-context-budget-model-token-limits'."
  (when (stringp model)
    (cdr (assoc-string model
                       (or limits e-context-budget-model-token-limits)
                       t))))

(defun e-context-budget-context-token-estimate
    (context &optional bytes-per-token)
  "Return approximate token count for model-facing CONTEXT.
BYTES-PER-TOKEN defaults to `e-context-budget-estimate-bytes-per-token'."
  (let* ((options (plist-get context :options))
         (model-facing-context
          (list :messages (plist-get context :messages)
                :tools (plist-get options :tools)))
         (bytes (string-bytes (prin1-to-string model-facing-context)))
         (per-token (or bytes-per-token
                        e-context-budget-estimate-bytes-per-token))
         (per-token (if (and (numberp per-token) (> per-token 0))
                        per-token
                      4.0)))
    (ceiling (/ bytes (float per-token)))))

(defun e-context-budget--token-usage-input-tokens (usage)
  "Return input token count from provider-neutral USAGE."
  (let ((tokens (or (plist-get usage :input-tokens)
                    (plist-get usage :input_tokens))))
    (when (and (integerp tokens) (>= tokens 0))
      tokens)))

(defun e-context-budget--latest-token-usage-event (harness session-id)
  "Return latest durable provider token usage event for SESSION-ID."
  (when (and harness session-id)
    (ignore-errors
      (e-session-latest-token-usage-event
       (e-harness-sessions harness)
       session-id))))

(defun e-context-budget--latest-valid-compaction (harness session-id)
  "Return latest valid compaction for SESSION-ID."
  (when (and harness session-id)
    (ignore-errors
      (e-session-latest-valid-compaction
       (e-harness-sessions harness)
       session-id))))

(defun e-context-budget-session-exists-p (harness session-id)
  "Return non-nil when HARNESS has SESSION-ID."
  (and harness
       session-id
       (ignore-errors
         (e-session-get (e-harness-sessions harness) session-id)
         t)))

(defun e-context-budget-usage-before-compaction-p (usage-event compaction)
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

(defun e-context-budget--fresh-cached-estimate (cache ttl &optional now)
  "Return fresh cached approximate tokens from CACHE, or nil.
CACHE is a caller-owned cons cell of (TOKENS . TIME) or nil.  TTL is the cache
lifetime in seconds."
  (let ((ttl (if (and (numberp ttl) (>= ttl 0)) ttl 2.0))
        (now (or now (float-time))))
    (when (and (consp cache)
               (integerp (car cache))
               (numberp (cdr cache))
               (< (- now (cdr cache)) ttl))
      (car cache))))

(defun e-context-budget--cached-estimate (context cache bytes-per-token ttl)
  "Return cached approximate token estimate for CONTEXT.
CACHE is a caller-owned cons cell of (TOKENS . TIME) or nil.  BYTES-PER-TOKEN
is forwarded to the estimator.  TTL is the cache lifetime in seconds."
  (or (e-context-budget--fresh-cached-estimate cache ttl)
      (let ((tokens (e-context-budget-context-token-estimate
                     context bytes-per-token)))
        (when (consp cache)
          (setcar cache tokens)
          (setcdr cache (float-time)))
        tokens)))

(cl-defun e-context-budget-used-tokens
    (harness session-id
             &key prefer-token-usage estimate-cache bytes-per-token
             estimate-cache-seconds (estimate-context t))
  "Return current used tokens for SESSION-ID through HARNESS.
Fresh provider usage is preferred.  Provider usage before the latest valid
compaction is ignored and the current model-facing context is estimated."
  (let* ((usage-event (e-context-budget--latest-token-usage-event
                       harness session-id))
         (latest-compaction (e-context-budget--latest-valid-compaction
                             harness session-id))
         (usage-tokens
          (unless (e-context-budget-usage-before-compaction-p
                   usage-event latest-compaction)
            (e-context-budget--token-usage-input-tokens
             (plist-get usage-event :payload))))
         (cached-tokens
          (and estimate-context
               (not usage-tokens)
               (e-context-budget--fresh-cached-estimate
                estimate-cache estimate-cache-seconds)))
         (context (when (and estimate-context
                             (not cached-tokens)
                             (not (and prefer-token-usage usage-tokens)))
                    (ignore-errors
                      (e-harness-context harness session-id)))))
    (or usage-tokens
        cached-tokens
        (and context
             (ignore-errors
               (e-context-budget--cached-estimate
                context estimate-cache bytes-per-token
                estimate-cache-seconds))))))

(cl-defun e-context-budget-status
    (harness session-id
             &key prefer-token-usage estimate-cache token-limits
             token-limit-function bytes-per-token estimate-cache-seconds
             (estimate-context t))
  "Return budget plist for SESSION-ID through HARNESS.
The plist includes `:model', `:reasoning-effort', `:used-tokens', `:window',
and `:approximate'."
  (when (e-context-budget-session-exists-p harness session-id)
    (let* ((usage-event (e-context-budget--latest-token-usage-event
                         harness session-id))
           (latest-compaction (e-context-budget--latest-valid-compaction
                               harness session-id))
           (usage-tokens
            (unless (e-context-budget-usage-before-compaction-p
                     usage-event latest-compaction)
              (e-context-budget--token-usage-input-tokens
               (plist-get usage-event :payload))))
           (cached-tokens
            (and estimate-context
                 (not usage-tokens)
                 (e-context-budget--fresh-cached-estimate
                  estimate-cache estimate-cache-seconds)))
           (context (when (and estimate-context
                               (not cached-tokens)
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
                 (or cached-tokens
                     (and context
                          (ignore-errors
                            (e-context-budget--cached-estimate
                             context estimate-cache bytes-per-token
                             estimate-cache-seconds))))))
           (window (or (and token-limit-function
                            (ignore-errors
                              (funcall token-limit-function model)))
                       (e-context-budget-model-window model token-limits))))
      (list :model model
            :reasoning-effort effort
            :used-tokens (or usage-tokens estimated-tokens)
            :window window
            :approximate (not usage-tokens)))))

(provide 'e-context-budget)

;;; e-context-budget.el ends here

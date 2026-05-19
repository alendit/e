;;; e-harness.el --- Core harness service for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public pure core harness service.

;;; Code:

(require 'cl-lib)
(require 'e-context)
(require 'e-events)
(require 'e-loop)
(require 'e-session)
(require 'e-tools)

(define-error 'e-harness-no-active-turn "No active turn")

(cl-defstruct (e-harness (:constructor e-harness--make))
  backend
  context-strategy
  (sessions (e-session-store-create))
  (tools (e-tools-registry-create))
  (subscribers nil)
  active-turns)

(defvar e-harness--turn-counter 0
  "Monotonic turn id counter.")

(defvar e-harness--message-counter 0
  "Monotonic harness message id counter.")

(defun e-harness--next-turn-id ()
  "Return a new in-process turn id."
  (setq e-harness--turn-counter (1+ e-harness--turn-counter))
  (format "turn-%d" e-harness--turn-counter))

(defun e-harness--next-message-id ()
  "Return a new in-process harness message id."
  (setq e-harness--message-counter (1+ e-harness--message-counter))
  (format "msg-user-%d" e-harness--message-counter))

(cl-defun e-harness-create (&key backend context-strategy sessions tools)
  "Create a core harness with BACKEND, CONTEXT-STRATEGY, SESSIONS, and TOOLS."
  (e-harness--make :backend backend
                   :context-strategy (or context-strategy
                                         (e-context-transcript-stack-create))
                   :sessions (or sessions (e-session-store-create))
                   :tools (or tools (e-tools-registry-create))
                   :active-turns (make-hash-table :test 'equal)))

(cl-defun e-harness-create-session (harness &key id metadata)
  "Create session ID with METADATA in HARNESS."
  (e-session-create (e-harness-sessions harness)
                    :id id
                    :metadata metadata))

(defun e-harness-subscribe (harness subscriber)
  "Register SUBSCRIBER for core events from HARNESS."
  (push subscriber (e-harness-subscribers harness))
  subscriber)

(defun e-harness--emit (harness event)
  "Emit EVENT to HARNESS subscribers."
  (dolist (subscriber (reverse (e-harness-subscribers harness)))
    (funcall subscriber event)))

(defun e-harness-messages (harness session-id)
  "Return messages for SESSION-ID in HARNESS."
  (e-session-messages (e-harness-sessions harness) session-id))

(defun e-harness-prompt (harness session-id prompt)
  "Append PROMPT and run one backend turn for SESSION-ID in HARNESS."
  (let* ((turn-id (e-harness--next-turn-id))
         (user-message (list :id (e-harness--next-message-id)
                             :role 'user
                             :content prompt
                             :metadata nil)))
    (puthash session-id turn-id (e-harness-active-turns harness))
    (unwind-protect
        (progn
          (e-session-append-message (e-harness-sessions harness)
                                    session-id
                                    user-message)
          (e-harness--emit
           harness
           (e-events-make :type 'message-added
                          :session-id session-id
                          :turn-id turn-id
                          :payload (list :message user-message)))
          (let ((context (e-context-build
                          (e-harness-context-strategy harness)
                          :sessions (e-harness-sessions harness)
                          :session-id session-id
                          :options nil)))
            (e-loop-run-turn
             :session-id session-id
             :turn-id turn-id
             :messages (plist-get context :messages)
             :backend (e-harness-backend harness)
             :tools (e-harness-tools harness)
             :options (plist-get context :options)
             :on-event (lambda (event) (e-harness--emit harness event))
             :append-message
             (lambda (message)
               (e-session-append-message (e-harness-sessions harness)
                                         session-id
                                         message)))))
      (remhash session-id (e-harness-active-turns harness)))))

(defun e-harness-follow-up (harness session-id prompt)
  "Submit PROMPT as the next turn for SESSION-ID in HARNESS."
  (e-harness-prompt harness session-id prompt))

(defun e-harness-reset (harness session-id)
  "Clear SESSION-ID transcript state in HARNESS."
  (e-session-clear-messages (e-harness-sessions harness) session-id)
  (e-harness--emit
   harness
   (e-events-make :type 'session-reset
                  :session-id session-id
                  :turn-id nil
                  :payload nil)))

(defun e-harness-state (harness session-id)
  "Return settled state for SESSION-ID in HARNESS."
  (list :session-id session-id
        :active-turn (gethash session-id (e-harness-active-turns harness))
        :message-count (length (e-harness-messages harness session-id))))

(defun e-harness-abort (harness session-id)
  "Abort the active turn for SESSION-ID in HARNESS.
The synchronous first implementation can only surface that no turn is active
after `e-harness-prompt' settles.  Async cancellation belongs to the later
process/queue package."
  (unless (gethash session-id (e-harness-active-turns harness))
    (signal 'e-harness-no-active-turn (list session-id))))

(provide 'e-harness)

;;; e-harness.el ends here

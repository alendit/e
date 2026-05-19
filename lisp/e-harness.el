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
  default-options
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

(cl-defun e-harness-create (&key backend context-strategy default-options sessions tools)
  "Create a core harness.
BACKEND, CONTEXT-STRATEGY, DEFAULT-OPTIONS, SESSIONS, and TOOLS configure the
provider-neutral runtime."
  (e-harness--make :backend backend
                   :context-strategy (or context-strategy
                                         (e-context-transcript-stack-create))
                   :default-options default-options
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

(defun e-harness--turn-options (harness)
  "Return backend-neutral turn options for HARNESS."
  (let ((tool-definitions (e-tools-definitions (e-harness-tools harness))))
    (if tool-definitions
        (append (copy-sequence (e-harness-default-options harness))
                (list :tools tool-definitions))
      (e-harness-default-options harness))))

(defun e-harness--active-turn-id (entry)
  "Return active turn id from ENTRY."
  (if (listp entry)
      (plist-get entry :id)
    entry))

(defun e-harness--active-turn-running-p (entry)
  "Return non-nil when active turn ENTRY is still running."
  (and (listp entry)
       (eq (plist-get entry :status) 'running)))

(defun e-harness--emit-turn-failed (harness session-id turn-id error-message)
  "Emit a turn-failed event from HARNESS.
SESSION-ID and TURN-ID identify the failed turn.  ERROR-MESSAGE describes the
provider or loop failure."
  (e-harness--emit
   harness
   (e-events-make :type 'turn-failed
                  :session-id session-id
                  :turn-id turn-id
                  :payload (list :error error-message))))

(defun e-harness--run-prompt-turn (harness session-id turn-id prompt)
  "Run PROMPT for SESSION-ID and TURN-ID in HARNESS."
  (let ((user-message (list :id (e-harness--next-message-id)
                            :role 'user
                            :content prompt
                            :metadata nil)))
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
                    :options (e-harness--turn-options harness))))
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
                                   message))))))

(defun e-harness-prompt (harness session-id prompt)
  "Append PROMPT and run one backend turn for SESSION-ID in HARNESS."
  (let ((turn-id (e-harness--next-turn-id)))
    (puthash session-id turn-id (e-harness-active-turns harness))
    (unwind-protect
        (condition-case err
            (e-harness--run-prompt-turn harness session-id turn-id prompt)
          (error
           (let ((message (error-message-string err)))
             (e-harness--emit-turn-failed harness session-id turn-id message)
             (signal (car err) (cdr err)))))
      (remhash session-id (e-harness-active-turns harness)))))

(cl-defun e-harness-prompt-async (harness session-id prompt &key delay)
  "Append PROMPT and run one backend turn asynchronously in HARNESS.
Return the queued turn id.  DELAY is primarily for tests and queued-turn
cancellation.  SESSION-ID identifies the session."
  (let* ((turn-id (e-harness--next-turn-id))
         (entry (list :id turn-id
                      :status 'running
                      :result nil
                      :error nil
                      :timer nil)))
    (puthash session-id entry (e-harness-active-turns harness))
    (plist-put
     entry
     :timer
     (run-at-time
      (or delay 0)
      nil
      (lambda ()
        (unless (plist-get entry :cancelled)
          (condition-case err
              (let ((result (e-harness--run-prompt-turn
                             harness session-id turn-id prompt)))
                (plist-put entry :result result)
                (plist-put entry :status 'done))
            (error
             (let ((message (error-message-string err)))
               (plist-put entry :status 'error)
               (plist-put entry :error message)
               (e-harness--emit-turn-failed harness session-id turn-id message))))))))
    turn-id))

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
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (list :session-id session-id
          :active-turn (when (or (not (listp entry))
                                 (eq (plist-get entry :status) 'running))
                         (e-harness--active-turn-id entry))
          :message-count (length (e-harness-messages harness session-id)))))

(defun e-harness-abort (harness session-id)
  "Abort the active turn for SESSION-ID in HARNESS."
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    (if (listp entry)
        (let ((turn-id (plist-get entry :id)))
          (when-let ((timer (plist-get entry :timer)))
            (cancel-timer timer))
          (plist-put entry :cancelled t)
          (plist-put entry :status 'cancelled)
          (e-harness--emit
           harness
           (e-events-make :type 'turn-cancelled
                          :session-id session-id
                          :turn-id turn-id
                          :payload nil))
          entry)
      (signal 'e-harness-no-active-turn (list session-id)))))

(defun e-harness-wait (harness session-id &optional timeout)
  "Wait for SESSION-ID's async turn in HARNESS to settle.
TIMEOUT is in seconds.  Return the settled active-turn entry and clear it from
active state when it is no longer running."
  (let ((deadline (and timeout (+ (float-time) timeout)))
        (entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    (while (and (e-harness--active-turn-running-p entry)
                (or (not deadline) (< (float-time) deadline)))
      (accept-process-output nil 0.01)
      (setq entry (gethash session-id (e-harness-active-turns harness))))
    (unless (e-harness--active-turn-running-p entry)
      (remhash session-id (e-harness-active-turns harness)))
    entry))

(provide 'e-harness)

;;; e-harness.el ends here

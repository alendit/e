;;; e-harness.el --- Core harness service for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Public pure core harness service.

;;; Code:

(require 'cl-lib)
(require 'e-capabilities)
(require 'e-context)
(require 'e-events)
(require 'e-layers)
(require 'e-loop)
(require 'e-session)
(require 'e-tools)
(require 'subr-x)

(define-error 'e-harness-no-active-turn "No active turn")

(cl-defstruct (e-harness (:constructor e-harness--make))
  backend
  context-strategy
  default-options
  (sessions (e-session-store-create))
  (tools (e-tools-registry-create))
  (active-layers nil)
  (active-capabilities nil)
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

(cl-defun e-harness-create
    (&key backend context-strategy default-options sessions tools
          active-layers active-capabilities)
  "Create a core harness.
BACKEND, CONTEXT-STRATEGY, DEFAULT-OPTIONS, SESSIONS, TOOLS, ACTIVE-LAYERS,
and ACTIVE-CAPABILITIES configure the provider-neutral runtime."
  (let ((harness
         (e-harness--make :backend backend
                          :context-strategy (or context-strategy
                                                (e-context-transcript-stack-create))
                          :default-options default-options
                          :sessions (or sessions (e-session-store-create))
                          :tools (or tools (e-tools-registry-create))
                          :active-capabilities active-capabilities
                          :active-turns (make-hash-table :test 'equal))))
    (dolist (capability active-capabilities)
      (e-capabilities-register-tools capability (e-harness-tools harness)))
    (dolist (layer active-layers)
      (e-harness-activate-layer harness layer))
    harness))

(defun e-harness--legacy-context-layers (layers)
  "Return LAYERS that still contribute context directly."
  (cl-remove-if #'e-layer-capabilities layers))

(defun e-harness-activate-layer (harness layer)
  "Activate LAYER in HARNESS and register its tools and capabilities."
  (setf (e-harness-active-layers harness)
        (append (e-harness-active-layers harness) (list layer)))
  (dolist (capability (e-layer-capabilities layer))
    (e-capabilities-register-tools capability (e-harness-tools harness))
    (setf (e-harness-active-capabilities harness)
          (append (e-harness-active-capabilities harness) (list capability))))
  (e-layers-register-tools layer (e-harness-tools harness))
  layer)

(cl-defun e-harness-create-session (harness &key id metadata)
  "Create session ID with METADATA in HARNESS."
  (e-session-create (e-harness-sessions harness)
                    :id id
                    :metadata metadata))

(cl-defun e-harness-subscribe (harness subscriber &key session-id)
  "Register SUBSCRIBER for core events from HARNESS.
When SESSION-ID is non-nil, SUBSCRIBER only receives events for that session."
  (let ((record (list :callback subscriber :session-id session-id)))
    (push record (e-harness-subscribers harness))
    record))

(defun e-harness--emit (harness event)
  "Emit EVENT to HARNESS subscribers."
  (let ((event-session-id (plist-get event :session-id)))
    (dolist (subscriber (reverse (e-harness-subscribers harness)))
      (let ((callback (if (functionp subscriber)
                          subscriber
                        (plist-get subscriber :callback)))
            (session-id (and (listp subscriber)
                             (plist-get subscriber :session-id))))
        (when (or (not session-id)
                  (equal session-id event-session-id))
          (funcall callback event))))))

(defconst e-harness--durable-activity-event-types
  '(turn-started reasoning-delta tool-started tool-finished turn-finished
    turn-failed turn-cancelled backend-empty-output)
  "Turn event types stored as durable session activity.")

(defun e-harness--durable-activity-event-p (type)
  "Return non-nil when TYPE should be stored as session activity."
  (memq type e-harness--durable-activity-event-types))

(defun e-harness--emit-turn-event (harness session-id turn-id type payload)
  "Emit public event TYPE with PAYLOAD for HARNESS SESSION-ID TURN-ID."
  (when (and session-id
             turn-id
             (e-harness--durable-activity-event-p type)
             (ignore-errors
               (e-session-get (e-harness-sessions harness) session-id)))
    (e-session-append-activity-event
     (e-harness-sessions harness)
     session-id
     turn-id
     type
     payload))
  (e-harness--emit
   harness
   (e-events-make :type type
                  :session-id session-id
                  :turn-id turn-id
                  :payload payload)))

(defun e-harness-messages (harness session-id)
  "Return messages for SESSION-ID in HARNESS."
  (e-session-messages (e-harness-sessions harness) session-id))

(defun e-harness--merge-turn-options (base overrides)
  "Return BASE options with OVERRIDES applied."
  (let ((options (copy-sequence base))
        (remaining overrides))
    (while remaining
      (setq options (plist-put options (pop remaining) (pop remaining))))
    options))

(defun e-harness-session-options (harness session-id)
  "Return session-specific turn options for SESSION-ID in HARNESS."
  (e-session-turn-options (e-harness-sessions harness) session-id))

(defun e-harness--set-session-options (harness session-id options)
  "Replace SESSION-ID turn OPTIONS in HARNESS and emit an update event."
  (let ((turn-options
         (e-session-set-turn-options
          (e-harness-sessions harness)
          session-id
          options)))
    (e-harness--emit
     harness
     (e-events-make :type 'session-options-changed
                    :session-id session-id
                    :turn-id "session-options"
                    :payload (list :turn-options turn-options)))
    turn-options))

(defun e-harness-set-session-model (harness session-id model)
  "Set SESSION-ID's model override to MODEL in HARNESS."
  (let ((options (copy-sequence (e-harness-session-options harness session-id))))
    (if (and (stringp model) (not (string-empty-p (string-trim model))))
        (setq options (plist-put options :model (string-trim model)))
      (cl-remf options :model))
    (e-harness--set-session-options harness session-id options)))

(defun e-harness-set-session-reasoning-effort (harness session-id effort)
  "Set SESSION-ID's reasoning EFFORT override in HARNESS."
  (let ((options (copy-sequence (e-harness-session-options harness session-id))))
    (if (and (stringp effort) (not (string-empty-p (string-trim effort))))
        (setq options (plist-put options :reasoning-effort (string-trim effort)))
      (cl-remf options :reasoning-effort))
    (e-harness--set-session-options harness session-id options)))

(defun e-harness--turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID."
  (let ((options (e-harness--merge-turn-options
                  (e-harness-default-options harness)
                  (e-harness-session-options harness session-id)))
        (tool-definitions (e-tools-definitions (e-harness-tools harness))))
    (if tool-definitions
        (plist-put options :tools tool-definitions)
      options)))

(defun e-harness-context (harness session-id &optional turn-id)
  "Return backend-neutral context for SESSION-ID in HARNESS.
TURN-ID is passed to active layer context providers when present."
  (e-context-build
   (e-harness-context-strategy harness)
   :sessions (e-harness-sessions harness)
   :session-id session-id
   :options (e-harness--turn-options harness session-id)
   :prefix-messages
   (append
    (e-capabilities-context-messages
     (e-harness-active-capabilities harness)
     :harness harness
     :session-id session-id
     :turn-id turn-id)
    (e-layers-context-messages
     (e-harness--legacy-context-layers (e-harness-active-layers harness))
     :harness harness
     :session-id session-id
     :turn-id turn-id))))

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
  (e-harness--emit-turn-event
   harness session-id turn-id 'turn-failed (list :error error-message)))

(defun e-harness--append-message (harness session-id turn-id message)
  "Append MESSAGE in HARNESS for SESSION-ID TURN-ID and emit `message-added'."
  (let ((message (copy-sequence message)))
    (when turn-id
      (plist-put message :turn-id turn-id))
    (setq message
          (e-session-append-message (e-harness-sessions harness)
                                    session-id
                                    message))
    (e-harness--emit-turn-event
     harness session-id turn-id 'message-added (list :message message))
    message))

(defun e-harness--append-user-message (harness session-id turn-id prompt)
  "Append PROMPT as the user message in HARNESS for SESSION-ID and TURN-ID."
  (e-harness--append-message
   harness
   session-id
   turn-id
   (list :id (e-harness--next-message-id)
         :role 'user
         :content prompt
         :metadata nil)))

(defun e-harness--run-prompt-turn (harness session-id turn-id)
  "Run the queued prompt turn for SESSION-ID and TURN-ID in HARNESS."
  (let ((context (e-harness-context harness session-id turn-id)))
    (e-loop-run-turn
     :session-id session-id
     :turn-id turn-id
     :messages (plist-get context :messages)
     :backend (e-harness-backend harness)
     :tools (e-harness-tools harness)
     :options (plist-get context :options)
     :on-event (lambda (type payload)
                 (e-harness--emit-turn-event
                  harness session-id turn-id type payload))
     :append-message
     (lambda (message)
       (e-harness--append-message harness session-id turn-id message)))))

(defun e-harness-prompt (harness session-id prompt)
  "Append PROMPT and run one backend turn for SESSION-ID in HARNESS."
  (let ((turn-id (e-harness--next-turn-id)))
    (puthash session-id turn-id (e-harness-active-turns harness))
    (unwind-protect
        (condition-case err
            (progn
              (e-harness--append-user-message harness session-id turn-id prompt)
              (e-harness--run-prompt-turn harness session-id turn-id))
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
    (condition-case err
        (e-harness--append-user-message harness session-id turn-id prompt)
      (error
       (let ((message (error-message-string err)))
         (plist-put entry :status 'error)
         (plist-put entry :error message)
         (e-harness--emit-turn-failed harness session-id turn-id message)
         (signal (car err) (cdr err)))))
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
                             harness session-id turn-id)))
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
          (e-harness--emit-turn-event
           harness session-id turn-id 'turn-cancelled nil)
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

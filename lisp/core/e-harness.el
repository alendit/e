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
(require 'e-operations)
(require 'e-resources)
(require 'e-session)
(require 'e-store)
(require 'e-tools)
(require 'subr-x)

(define-error 'e-harness-no-active-turn "No active turn")
(define-error 'e-harness-active-turn-exists
  "Session already has an active turn")

(cl-defstruct (e-harness (:constructor e-harness--make))
  backend
  context-strategy
  default-options
  (sessions (e-session-store-create))
  (active-layers nil)
  (subscribers nil)
  active-turns)

(defvar e-harness--turn-counter 0
  "Monotonic turn id counter.")

(defvar e-harness--message-counter 0
  "Monotonic harness message id counter.")

(defun e-harness--clear-derived-accessor-metadata ()
  "Clear stale struct accessor metadata for derived harness views."
  (dolist (symbol '(e-harness-active-capabilities
                    e-harness-store
                    e-harness-resources
                    e-harness-tools))
    (put symbol 'compiler-macro nil)
    (put symbol 'side-effect-free nil)
    (put symbol 'gv-expander nil)))

(e-harness--clear-derived-accessor-metadata)

(defun e-harness--next-turn-id ()
  "Return a new in-process turn id."
  (setq e-harness--turn-counter (1+ e-harness--turn-counter))
  (format "turn-%d" e-harness--turn-counter))

(defun e-harness--next-message-id ()
  "Return a new in-process harness message id."
  (setq e-harness--message-counter (1+ e-harness--message-counter))
  (format "msg-user-%d" e-harness--message-counter))

(cl-defun e-harness-create
    (&key backend context-strategy default-options sessions active-layers)
  "Create a core harness.
BACKEND, CONTEXT-STRATEGY, DEFAULT-OPTIONS, SESSIONS, and ACTIVE-LAYERS
configure the provider-neutral runtime."
  (let ((harness
         (e-harness--make :backend backend
                          :context-strategy (or context-strategy
                                                (e-context-transcript-stack-create))
                          :default-options default-options
                          :sessions (or sessions (e-session-store-create))
                          :active-layers nil
                          :active-turns (make-hash-table :test 'equal))))
    (dolist (layer active-layers)
      (e-harness-activate-layer harness layer))
    harness))

(defun e-harness-active-capabilities (harness)
  "Return HARNESS capabilities derived from its active layers."
  (let (capabilities)
    (dolist (layer (e-harness-active-layers harness))
      (setq capabilities
            (append capabilities
                    (copy-sequence (or (e-layer-capabilities layer) nil)))))
    capabilities))

(defun e-harness-tools (harness)
  "Return a fresh tool registry view over HARNESS active layers."
  (let ((registry (e-tools-registry-create)))
    (e-harness--register-resource-operation-tools registry (e-harness-resources harness))
    (dolist (capability (e-harness-active-capabilities harness))
      (e-capabilities-register-tools capability registry))
    registry))

(defun e-harness-store (harness)
  "Return a fresh e:// store view over HARNESS active layers."
  (let ((store (e-store-create)))
    (dolist (capability (e-harness-active-capabilities harness))
      (e-capabilities-register-resources capability store))
    store))

(defun e-harness-resources (harness)
  "Return a fresh resource registry view over HARNESS active layers."
  (let ((registry (e-resources-registry-create))
        (store (e-harness-store harness)))
    (dolist (capability (e-harness-active-capabilities harness))
      (e-capabilities-register-resource-methods capability registry))
    (when (e-store-list store)
      (e-resources-register registry (e-store-resource-method store)))
    registry))

(defun e-harness--resource-method-description (method)
  "Return model-facing description fragment for METHOD."
  (let ((patterns (or (e-resource-method-uri-patterns method)
                      (list (format "%s://<resource>"
                                    (e-resource-method-scheme method)))))
        (description (e-resource-method-description method))
        (range-modes (e-resource-method-range-modes method)))
    (string-join
     (delq nil
           (list
            (format "- %s" (string-join patterns ", "))
            description
            (when range-modes
              (format "Range units: %s." (string-join range-modes ", ")))))
     " ")))

(defun e-harness--resource-operation-description (resources operation)
  "Return model-facing description for OPERATION over active RESOURCES."
  (let ((methods (e-resources-methods-for-operation resources operation)))
    (string-join
     (list (e-operation-description operation)
           ""
           "Active URI schemes:"
           (mapconcat #'e-harness--resource-method-description methods "\n"))
     "\n")))

(defun e-harness--register-resource-operation-tool (registry resources operation)
  "Register OPERATION in REGISTRY as a model-facing tool backed by RESOURCES."
  (let ((dispatch (e-operation-dispatch operation)))
    (when (functionp dispatch)
      (e-tools-register
       registry
       :name (e-operation-tool-name operation)
       :description (e-harness--resource-operation-description resources operation)
       :parameters (e-operation-parameters operation)
       :handler
       (lambda (arguments)
         (funcall dispatch
                  (lambda (uri &rest operation-arguments)
                    (apply #'e-resources-call
                           resources
                           operation
                           uri
                           operation-arguments))
                  arguments))))))

(defun e-harness--register-resource-operation-tools (registry resources)
  "Register active resource operation tools in REGISTRY backed by RESOURCES."
  (dolist (operation (e-resources-operations resources))
    (when (e-operation-p operation)
      (e-harness--register-resource-operation-tool registry resources operation))))

(defun e-harness--register-resource-tools (registry resources)
  "Register resource operation tools from RESOURCES in REGISTRY."
  (e-harness--register-resource-operation-tools registry resources))

(defun e-harness-activate-capability (harness capability)
  "Activate CAPABILITY in HARNESS as an anonymous capability layer."
  (e-harness-activate-layer
   harness
   (e-layer-create
    :id (e-capability-id capability)
    :name (e-capability-name capability)
    :capabilities (list capability)))
  capability)

(defun e-harness-activate-layer (harness layer)
  "Activate LAYER in HARNESS."
  (setf (e-harness-active-layers harness)
        (append (e-harness-active-layers harness) (list layer)))
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

(defun e-harness-session-title (harness session-id)
  "Return display title for SESSION-ID in HARNESS."
  (e-session-display-title (e-harness-sessions harness) session-id))

(defun e-harness-session-name (harness session-id)
  "Return explicit name for SESSION-ID in HARNESS, or nil."
  (plist-get (e-session-get (e-harness-sessions harness) session-id) :name))

(defun e-harness-session-list (harness)
  "Return display metadata for sessions owned by HARNESS."
  (e-session-list (e-harness-sessions harness)))

(defun e-harness-session-activity-events (harness session-id)
  "Return activity events for SESSION-ID in HARNESS."
  (e-session-activity-events (e-harness-sessions harness) session-id))

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

(defun e-harness-turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID."
  (let ((options (e-harness--merge-turn-options
                  (e-harness-default-options harness)
                  (e-harness-session-options harness session-id)))
        (tool-definitions (e-tools-definitions (e-harness-tools harness))))
    (if tool-definitions
        (plist-put options :tools tool-definitions)
      options)))

(defun e-harness--turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID."
  (e-harness-turn-options harness session-id))

(defun e-harness-context (harness session-id &optional turn-id)
  "Return backend-neutral context for SESSION-ID in HARNESS.
TURN-ID is passed to active capability context providers when present."
  (e-context-build
   (e-harness-context-strategy harness)
   :sessions (e-harness-sessions harness)
   :session-id session-id
   :options (e-harness-turn-options harness session-id)
   :prefix-messages
   (e-capabilities-context-messages
    (e-harness-active-capabilities harness)
    :harness harness
    :session-id session-id
    :turn-id turn-id)))

(defun e-harness--active-turn-id (entry)
  "Return active turn id from ENTRY."
  (if (listp entry)
      (plist-get entry :id)
    entry))

(defun e-harness--active-turn-running-p (entry)
  "Return non-nil when active turn ENTRY is still running."
  (and (listp entry)
       (eq (plist-get entry :status) 'running)))

(defun e-harness--cancel-active-request (entry)
  "Cancel ENTRY's active backend or tool request when one exists."
  (when-let ((request (plist-get entry :request)))
    (cond
     ((e-backend-request-p request)
      (e-backend-cancel-request request))
     ((e-tools-request-p request)
      (e-tools-cancel-request request)))))

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

(defun e-harness--append-user-message
    (harness session-id turn-id prompt &optional metadata)
  "Append PROMPT as the user message in HARNESS for SESSION-ID and TURN-ID."
  (e-harness--append-message
   harness
   session-id
   turn-id
   (list :id (e-harness--next-message-id)
         :role 'user
         :content prompt
         :metadata metadata)))

(cl-defun e-harness--run-prompt-turn
    (harness session-id turn-id &key on-request-start)
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
     :on-request-start on-request-start
     :append-message
     (lambda (message)
       (e-harness--append-message harness session-id turn-id message)))))

(cl-defun e-harness--run-prompt-turn-async
    (harness session-id turn-id &key on-request-start on-done on-error
             cancelled-p append-message on-event)
  "Start a queued async prompt turn for SESSION-ID and TURN-ID in HARNESS."
  (let ((context (e-harness-context harness session-id turn-id)))
    (e-loop-start-turn
     :session-id session-id
     :turn-id turn-id
     :messages (plist-get context :messages)
     :backend (e-harness-backend harness)
     :tools (e-harness-tools harness)
     :options (plist-get context :options)
     :on-event (or on-event
                   (lambda (type payload)
                     (e-harness--emit-turn-event
                      harness session-id turn-id type payload)))
     :on-request-start on-request-start
     :cancelled-p cancelled-p
     :on-done on-done
     :on-error on-error
     :append-message
     (or append-message
         (lambda (message)
           (e-harness--append-message
            harness session-id turn-id message))))))

(cl-defun e-harness-prompt (harness session-id prompt &key metadata)
  "Append PROMPT and run one backend turn for SESSION-ID in HARNESS.
This is the synchronous convenience wrapper over `e-harness-prompt-async'."
  (e-harness-prompt-async harness session-id prompt :metadata metadata)
  (let ((entry (e-harness-wait harness session-id)))
    (pcase (plist-get entry :status)
      ('done
       (plist-get entry :result))
      ('error
       (let ((condition (plist-get entry :condition)))
         (if condition
             (signal (car condition) (cdr condition))
           (error "%s" (or (plist-get entry :error)
                           "Async prompt failed")))))
      ('cancelled
       (signal 'e-harness-no-active-turn (list session-id)))
      (_ entry))))

(cl-defun e-harness-prompt-async
    (harness session-id prompt &key delay metadata)
  "Append PROMPT and run one backend turn asynchronously in HARNESS.
Return the queued turn id.  DELAY is primarily for tests and queued-turn
cancellation.  SESSION-ID identifies the session."
  (when (e-harness--active-turn-running-p
         (gethash session-id (e-harness-active-turns harness)))
    (signal 'e-harness-active-turn-exists (list session-id)))
  (let* ((turn-id (e-harness--next-turn-id))
         (entry (list :id turn-id
                      :status 'running
                      :result nil
                      :error nil
                      :condition nil
                      :timer nil
                      :request nil)))
    (puthash session-id entry (e-harness-active-turns harness))
    (condition-case err
        (e-harness--append-user-message
         harness session-id turn-id prompt metadata)
      (error
       (let ((message (error-message-string err)))
         (plist-put entry :status 'error)
         (plist-put entry :error message)
         (e-harness--emit-turn-failed harness session-id turn-id message)
         (remhash session-id (e-harness-active-turns harness))
         (signal (car err) (cdr err)))))
    (cl-labels
        ((active-entry-p ()
           (eq (gethash session-id (e-harness-active-turns harness))
               entry))
         (cancelled-p ()
           (or (plist-get entry :cancelled)
               (not (active-entry-p))))
         (finish-error
          (err)
          (when (and (active-entry-p) (not (plist-get entry :cancelled)))
            (let ((message (error-message-string err)))
              (plist-put entry :status 'error)
              (plist-put entry :condition err)
              (plist-put entry :error message)
              (e-harness--emit-turn-failed
               harness session-id turn-id message))))
         (finish-done
          (result)
          (when (and (active-entry-p) (not (plist-get entry :cancelled)))
            (plist-put entry :result result)
            (plist-put entry :status 'done)))
         (start-turn
          ()
          (when (and (active-entry-p) (not (plist-get entry :cancelled)))
            (plist-put entry :timer nil)
            (e-harness--run-prompt-turn-async
             harness session-id turn-id
             :cancelled-p #'cancelled-p
             :on-request-start
             (lambda (request)
               (when (and (active-entry-p)
                          (not (plist-get entry :cancelled)))
                 (plist-put entry :request request)))
             :on-done #'finish-done
             :on-error #'finish-error
             :on-event
             (lambda (type payload)
               (when (and (active-entry-p)
                          (not (plist-get entry :cancelled)))
                 (e-harness--emit-turn-event
                  harness session-id turn-id type payload)))
             :append-message
             (lambda (message)
               (when (and (active-entry-p)
                          (not (plist-get entry :cancelled)))
                 (e-harness--append-message
                  harness session-id turn-id message)))))))
      (if (and delay (> delay 0))
          (plist-put entry :timer (run-at-time delay nil #'start-turn))
        (start-turn)))
    turn-id))

(cl-defun e-harness-follow-up (harness session-id prompt &key metadata)
  "Submit PROMPT as the next turn for SESSION-ID in HARNESS."
  (e-harness-prompt harness session-id prompt :metadata metadata))

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
          (e-harness--cancel-active-request entry)
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

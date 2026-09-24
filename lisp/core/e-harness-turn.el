;;; e-harness-turn.el --- Turn execution and attached-turn composition -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns active turn submission/settlement, queue and attachment transitions,
;; compaction orchestration, cancellation, retry, and wait behavior.  It is
;; the top composition owner beneath the e-harness application facade.

;;; Code:

(require 'cl-lib)
(require 'e-harness-state)
(require 'e-harness-capabilities)
(require 'e-harness-activity)
(require 'e-harness-turn-state)
(require 'e-harness-context-runtime)
(require 'e-backend)
(require 'e-compaction)
(require 'e-context)
(require 'e-context-budget)
(require 'e-context-lifetime)
(require 'e-events)
(require 'e-hooks)
(require 'e-loop)
(require 'e-request)
(require 'e-session)
(require 'e-telemetry)
(require 'e-tools)
(require 'e-work)
(require 'seq)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-harness-no-active-turn "No active turn")
(define-error 'e-harness-board-attachment-required
  "Live harness execution requires a current board attachment")

(cl-defstruct (e-harness-attached-turn-port
               (:constructor e-harness-attached-turn-port-create))
  "The narrow adapter port for one attached harness session.
AUTHORIZER validates the opaque endpoint token for a concrete harness/session;
FOLLOW-UP-PUBLISHER is used only by a settling capability follow-up.  The port
is passed explicitly by the owning adapter and is never stored process-wide.
The bound HARNESS, SESSION-ID, and ATTACHMENT-TOKEN make the board-facing
operations below one concrete session port rather than a bag of unrelated
callbacks."
  harness session-id attachment-token authorizer follow-up-publisher)

(defun e-harness-turn--require-attached-port
    (harness session-id token &optional port)
  "Return authorized PORT for HARNESS SESSION-ID and TOKEN.
When PORT is omitted, use the port retained by the active turn."
  (let* ((entry (gethash session-id (e-harness-active-turns harness)))
         (port (or port (and (listp entry)
                             (plist-get entry :attached-turn-port)))))
    (unless (and token
                 (e-harness-attached-turn-port-p port)
                 (eq (e-harness-attached-turn-port-harness port) harness)
                 (equal (e-harness-attached-turn-port-session-id port)
                        session-id))
      (signal 'e-harness-board-attachment-required (list session-id)))
    (e-harness-attached-turn-port-authorize port token)
    port))

(defun e-harness-attached-turn-port-authorize (port token)
  "Authorize TOKEN for the session bound to PORT.
Return PORT when its owning attachment accepts the token; signal the same
attachment error used by the turn operations otherwise."
  (unless (and (e-harness-attached-turn-port-p port)
               (functionp (e-harness-attached-turn-port-authorizer port))
               (funcall (e-harness-attached-turn-port-authorizer port)
                        (e-harness-attached-turn-port-harness port)
                        (e-harness-attached-turn-port-session-id port)
                        token))
    (signal 'e-harness-board-attachment-required
            (list (and (e-harness-attached-turn-port-p port)
                       (e-harness-attached-turn-port-session-id port)))))
  port)

(defun e-harness-attached-turn-port-active-turn (port)
  "Return an immutable id/status observation for PORT's session.
The attached adapter must not depend on the active turn's execution plist.
Only the semantic identity and lifecycle status needed for routing are exposed;
the returned values are copied so mutating the observation cannot mutate the
turn owner."
  (when-let* ((entry
              (gethash (e-harness-attached-turn-port-session-id port)
                       (e-harness-active-turns
                        (e-harness-attached-turn-port-harness port)))))
    (let ((id (plist-get entry :id)))
      (list :id (if (stringp id) (copy-sequence id) (copy-tree id))
            :status (plist-get entry :status)))))

(defun e-harness-attached-turn-port-assistant-message (port turn-id)
  "Return final assistant output for TURN-ID through PORT."
  (e-harness-attached-turn-assistant-message
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   turn-id))

(defun e-harness-attached-turn-port-observe-activity (port subscriber)
  "Subscribe SUBSCRIBER to activity for the session bound to PORT.
The event envelope is copied and its payload is replaced with the bounded
consumer projection before SUBSCRIBER sees it.  The returned subscription is
owned by the activity owner and can be passed to
`e-harness-attached-turn-port-stop-observing'."
  (e-harness-activity-subscribe
   (e-harness-attached-turn-port-harness port)
   (lambda (event)
     (let ((projected (copy-tree event)))
       (plist-put
        projected :payload
        (copy-tree
         (e-harness-activity-payload
          (plist-get event :type)
          (plist-get event :payload))))
       (funcall subscriber projected)))
   :session-id (e-harness-attached-turn-port-session-id port)))

(defun e-harness-attached-turn-port-stop-observing (port subscription)
  "Remove SUBSCRIPTION created through PORT."
  (e-harness-activity-unsubscribe
   (e-harness-attached-turn-port-harness port)
   subscription))

(cl-defun e-harness-attached-turn-port-submit
    (port prompt &key delay metadata)
  "Submit PROMPT through the endpoint bound to PORT."
  (e-harness-attached-turn-submit
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   prompt
   :delay delay
   :metadata metadata
   :attachment-token (e-harness-attached-turn-port-attachment-token port)
   :attached-turn-port port))

(cl-defun e-harness-attached-turn-port-submit-batch
    (port prompt &key metadata)
  "Synchronously submit PROMPT through PORT."
  (e-harness-attached-turn-submit-batch
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   prompt
   :metadata metadata
   :attachment-token (e-harness-attached-turn-port-attachment-token port)
   :attached-turn-port port))

(cl-defun e-harness-attached-turn-port-steer
    (port prompt &key metadata)
  "Steer PORT's active turn with PROMPT."
  (e-harness-attached-turn-steer
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   prompt
   :metadata metadata
   :attachment-token (e-harness-attached-turn-port-attachment-token port)
   :attached-turn-port port))

(cl-defun e-harness-attached-turn-port-queue
    (port prompt &key references metadata)
  "Queue PROMPT on PORT's active turn."
  (e-harness-attached-turn-queue
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   prompt
   :references references
   :metadata metadata
   :attachment-token (e-harness-attached-turn-port-attachment-token port)
   :attached-turn-port port))

(cl-defun e-harness-attached-turn-port-follow-up
    (port prompt &key references metadata)
  "Queue a settlement follow-up PROMPT through PORT."
  (e-harness-attached-turn-follow-up
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   prompt
   :references references
   :metadata metadata
   :attached-turn-port port))

(defun e-harness-attached-turn-port-queued-prompts (port)
  "Return the queued follow-ups visible through PORT."
  (e-harness-queued-prompts
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)))

(defun e-harness-attached-turn-port-discard-queued-board-input
    (port delivery-id endpoint-token endpoint-generation reason)
  "Discard one exact queued board input through PORT's session boundary."
  (e-harness-discard-queued-board-input
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   delivery-id endpoint-token endpoint-generation reason))

(cl-defun e-harness-attached-turn-port-publish-follow-up
    (port prompt &key references metadata tags)
  "Publish settlement follow-up PROMPT through PORT's owning adapter."
  (e-harness-attached-turn-publish-follow-up
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   prompt
   :references references
   :metadata metadata
   :tags tags
   :attached-turn-port port))

(defun e-harness-attached-turn-port-abort (port)
  "Abort PORT's active turn."
  (e-harness-attached-turn-abort
   (e-harness-attached-turn-port-harness port)
   (e-harness-attached-turn-port-session-id port)
   (e-harness-attached-turn-port-attachment-token port)
   port))

(defun e-harness-turn--string-byte-prefix (text max-bytes)
  "Return TEXT truncated to MAX-BYTES without splitting UTF-8."
  (if (<= (string-bytes text) max-bytes)
      text
    (let ((end (min (length text) max-bytes)))
      (while (and (> end 0)
                  (> (string-bytes (substring text 0 end)) max-bytes))
        (setq end (1- end)))
      (substring text 0 end))))

(defun e-harness-attached-turn-assistant-message
    (harness session-id turn-id)
  "Return final assistant output for attached SESSION-ID TURN-ID."
  (e-harness-turn--turn-assistant-message harness session-id turn-id))

(defcustom e-harness-auto-compaction-enabled t
  "When non-nil, auto-compact before a turn that would near the context window."
  :type 'boolean
  :group 'e-harness)

(defcustom e-harness-auto-compaction-reserve-tokens 16384
  "Tokens to reserve below the model context window before auto-compacting.
Auto-compaction triggers when estimated context exceeds WINDOW minus this."
  :type 'integer
  :group 'e-harness)

(defcustom e-harness-retry-max-elapsed-seconds 1800.0
  "Total wall-clock budget for retrying a transient backend turn.
Retries stop once this time has elapsed since the first attempt; set to 0 to
disable retrying.  A bounded adapter reset hint may extend this window."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-initial-backoff-seconds 2.0
  "Initial delay before the first retry of a transient backend turn."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-backoff-multiplier 2.0
  "Multiplier applied to retry backoff after each failed attempt."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-max-backoff-seconds 60.0
  "Maximum delay between retries of a transient backend turn."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-jitter-fraction 0.25
  "Random fraction of backoff added as jitter before each retry."
  :type 'number
  :group 'e)

(defcustom e-harness-retry-reset-max-wait-seconds 900.0
  "Maximum adapter-normalized retry reset delay to honor."
  :type 'number
  :group 'e)

(defun e-harness-turn--retryable-error-p (details)
  "Return non-nil when adapter-normalized DETAILS permit a retry."
  (and (listp details) (eq (plist-get details :retryable) t)))

(defun e-harness-turn--retry-backoff-seconds (attempt)
  "Return retry backoff for one-based ATTEMPT, including bounded jitter."
  (let* ((base (min e-harness-retry-max-backoff-seconds
                    (* e-harness-retry-initial-backoff-seconds
                       (expt e-harness-retry-backoff-multiplier
                             (max 0 (1- attempt))))))
         (jitter (if (> e-harness-retry-jitter-fraction 0)
                     (* base e-harness-retry-jitter-fraction
                        (/ (random 1000) 1000.0))
                   0)))
    (+ base jitter)))

(defun e-harness-turn--retry-reset-seconds (details)
  "Return bounded adapter-normalized reset delay from DETAILS, or nil."
  (when (> e-harness-retry-reset-max-wait-seconds 0)
    (let ((seconds (and (listp details)
                        (plist-get details :retry-after-seconds))))
      (when (numberp seconds)
        (let ((wait (max 1.0 seconds)))
          (when (<= wait e-harness-retry-reset-max-wait-seconds)
            wait))))))

(defun e-harness-turn--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-harness-turn--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when developer profiling is enabled."
  (if (e-harness-turn--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-harness-turn--next-turn-id ()
  "Return a new durable turn id."
  (e-session-generate-ulid))

(defun e-harness-turn--turn-work-spec ()
  "Return the lifecycle-only work spec for one harness turn.
The harness owns the provider loop and settles this handle from its existing
turn completion paths; the runner deliberately performs no separate work."
  (e-work-spec-create
   :id "agent-turn"
   :description "Run one agent turn."
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'harness
    :runner (lambda (_handle _arguments _context) :deferred)))
(cl-defun e-harness-attached-turn-follow-up
    (harness session-id prompt &key references metadata attached-turn-port)
  "Queue PROMPT as a follow-up during turn settlement, then return its id.
This is valid from a `:turn-finished' hook, whose turn is already settling.  The
queued prompt is picked up by the normal post-settlement drain
(`e-harness-turn--drain-next-queued-prompt') that runs after the finished turn's
hooks complete, so the drain path stays the single owner of turn scheduling."
  (let* ((entry (gethash session-id (e-harness-active-turns harness)))
         ;; A port receiver supplied by the adapter is authoritative for this
         ;; operation.  Only the direct hook consumer path discovers the
         ;; current port from the settling entry.
         (token (if attached-turn-port
                    (e-harness-attached-turn-port-attachment-token
                     attached-turn-port)
                  (and (listp entry) (plist-get entry :endpoint-token))))
         (port (e-harness-turn--require-attached-port
                harness session-id token attached-turn-port)))
    (setq metadata (plist-put (copy-sequence metadata)
                              :board-endpoint-token token))
    (unless (and (stringp prompt) (not (string-empty-p prompt)))
      (user-error "Prompt must not be empty"))
    (let ((metadata (copy-sequence metadata)))
      (setq metadata (plist-put metadata :input-origin 'harness))
      (e-harness-turn-state-enqueue-prompt
       harness session-id prompt references metadata port))))

(cl-defun e-harness-attached-turn-publish-follow-up
    (harness session-id prompt &key references metadata tags attached-turn-port)
  "Publish PROMPT as an attached settlement follow-up with routing TAGS.
This is the capability-facing continuation port for a `:turn-finished' hook.
  The harness verifies the settling attachment but does not own interaction
routing.  Its runtime adapter must publish the follow-up through the owning
board, whose delivery path later starts the new turn."
  (let* ((entry (gethash session-id (e-harness-active-turns harness)))
         ;; Keep a receiver passed through the attached-turn port all the way
         ;; to authorization and publication.  The direct hook consumer may
         ;; still use the current settling entry as its explicit boundary.
         (token (if attached-turn-port
                    (e-harness-attached-turn-port-attachment-token
                     attached-turn-port)
                  (and (listp entry) (plist-get entry :endpoint-token))))
         (port (e-harness-turn--require-attached-port
                harness session-id token attached-turn-port)))
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (unless (functionp
           (e-harness-attached-turn-port-follow-up-publisher port))
    (signal 'e-harness-board-attachment-required (list session-id)))
  (funcall (e-harness-attached-turn-port-follow-up-publisher port)
           harness session-id prompt
           :references (copy-tree references)
           :metadata (copy-sequence metadata)
           :tags (copy-tree tags))))

(cl-defun e-harness-attached-turn-queue
    (harness session-id prompt &key references metadata attachment-token
             attached-turn-port)
  "Queue PROMPT as a follow-up for SESSION-ID in HARNESS.
The session must currently have a running active turn."
  (setq attached-turn-port
        (e-harness-turn--require-attached-port
         harness session-id attachment-token attached-turn-port))
  (setq metadata (plist-put (copy-sequence metadata)
                            :board-endpoint-token attachment-token))
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (unless (e-harness-turn-state-active-turn-running-p
           (gethash session-id (e-harness-active-turns harness)))
    (signal 'e-harness-no-active-turn (list session-id)))
  (e-harness-turn-state-enqueue-prompt
   harness session-id prompt references metadata attached-turn-port))

(defun e-harness-turn--steering-prompt-preview (prompt)
  "Return compact activity preview for steering PROMPT."
  (let ((text (string-trim
               (replace-regexp-in-string "[\n\r\t ]+" " " prompt))))
    (e-harness-turn--string-byte-prefix text 160)))

(defun e-harness-turn--durable-input-metadata (metadata)
  "Return durable input METADATA without the live attachment fence.
The endpoint token authorizes one process-local delivery attempt.  It remains
available to the delivery and receipt paths, but must not enter transcript or
activity persistence."
  (let ((result nil))
    (dolist (cell (seq-partition (copy-sequence metadata) 2))
      (unless (eq (car cell) :board-endpoint-token)
        (setq result (append result cell))))
    result))

(defun e-harness-turn--pending-steering-items (entry)
  "Return pending steering items from active turn ENTRY."
  (and (listp entry)
       (plist-get entry :pending-steering-input)))

(defun e-harness-turn--append-pending-steering-item (harness entry prompt metadata)
  "Append PROMPT and METADATA as pending steering input on HARNESS ENTRY."
  (plist-put entry
             :pending-steering-input
             (append (e-harness-turn--pending-steering-items entry)
                     (list (list :prompt prompt
                                 :metadata (copy-sequence metadata)))))
  (plist-put entry :pending-steering-count
             (1+ (or (plist-get entry :pending-steering-count) 0)))
  (e-harness-turn-state-adjust-queued-input-count harness 1)
  entry)

(defun e-harness-turn--drain-pending-steering-input (harness entry)
  "Return and clear pending steering items from HARNESS active turn ENTRY."
  (let ((items (e-harness-turn--pending-steering-items entry))
        (count (or (plist-get entry :pending-steering-count) 0)))
    (when items
      (plist-put entry :pending-steering-input nil)
      (plist-put entry :pending-steering-count 0)
      (e-harness-turn-state-adjust-queued-input-count harness (- count))
      items)))

(cl-defun e-harness-attached-turn-steer
    (harness session-id prompt &key metadata attachment-token
             attached-turn-port)
  "Steer SESSION-ID's running active turn with PROMPT in HARNESS."
  (e-harness-turn--require-attached-port
   harness session-id attachment-token attached-turn-port)
  (unless (and (stringp prompt) (not (string-empty-p prompt)))
    (user-error "Prompt must not be empty"))
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (unless (e-harness-turn-state-active-turn-running-p entry)
      (signal 'e-harness-no-active-turn (list session-id)))
    (let ((turn-id (plist-get entry :id))
          (metadata (e-harness-turn--durable-input-metadata metadata)))
      (e-harness-turn--append-pending-steering-item harness entry prompt metadata)
      (e-harness-activity-emit-turn-event
       harness session-id turn-id 'turn-steered
       (list :prompt-preview (e-harness-turn--steering-prompt-preview prompt)
             :metadata (copy-sequence metadata)))
      turn-id)))

(defun e-harness-turn--drain-next-queued-prompt (harness session-id settled-entry)
  "Start SESSION-ID's next queued prompt after SETTLED-ENTRY clears."
  (let ((current-entry (gethash session-id (e-harness-active-turns harness))))
    (when (and current-entry
               (not (e-harness-turn-state-active-turn-running-p current-entry))
               (eq current-entry settled-entry))
      (e-harness-turn-state-remove-active-turn harness session-id settled-entry))
    (unless (e-harness-turn-state-active-turn-running-p
             (gethash session-id (e-harness-active-turns harness)))
      (when-let* ((item (car (e-harness-queued-prompts harness session-id))))
        (e-harness-turn-state-set-queued-prompts
         harness session-id
         (cdr (e-harness-queued-prompts harness session-id)) -1)
        (e-harness-turn-state-emit-queue-changed harness session-id)
        (e-harness-attached-turn-submit
         harness
         session-id
         (plist-get item :prompt)
         :metadata (e-harness-turn-state-queue-item-metadata item)
         :attachment-token
         (plist-get (plist-get item :metadata) :board-endpoint-token)
         :attached-turn-port (plist-get item :attached-turn-port))))))

(defun e-harness-turn--schedule-queue-drain (harness session-id settled-entry)
  "Schedule queue drain for SESSION-ID after SETTLED-ENTRY settles."
  (run-at-time 0 nil
               (lambda ()
                 (e-harness-turn--drain-next-queued-prompt
                  harness session-id settled-entry))))
(defun e-harness-turn--nested-tool-payload
    (tool-call parent-tool-call depth &rest extra)
  "Return nested tool event payload for TOOL-CALL under PARENT-TOOL-CALL."
  (append
   (list :tool-call tool-call
         :nested t
         :parent-tool-call-id (plist-get parent-tool-call :id)
         :depth depth)
   extra
   (when (listp (plist-get tool-call :metadata))
	     (plist-get tool-call :metadata))))

(defun e-harness-turn--tool-blocking-class (tool)
  "Return TOOL blocking class metadata."
  (let ((metadata (plist-get tool :metadata)))
    (or (plist-get metadata :blocking-class)
        (plist-get metadata :blocking_class)
        (plist-get metadata :blocking))))

(defun e-harness-turn--nested-long-tool-result (tool-call tool)
  "Return a structured error result for long nested TOOL-CALL."
  (let ((class (e-harness-turn--tool-blocking-class tool))
        (name (plist-get tool-call :name)))
    (e-tools-result-create
     tool-call
     'error
     (format
      "Nested tool %s is %s-class and cannot run synchronously inside another tool; call it as a top-level tool instead."
      name class)
     (list :error 'e-nested-long-tool-rejected
           :blocking-class class))))

(defun e-harness-turn--execute-nested-tool
    (harness session-id turn-id tools tool-call _options parent-context)
  "Execute nested TOOL-CALL for HARNESS and return a structured result."
  (let* ((hooks (e-harness-hooks harness))
         (parent-tool-call (plist-get parent-context :tool-call))
         (depth (1+ (or (plist-get parent-context :depth) 0)))
         (context (e-harness-turn--tool-hook-context
                   harness session-id turn-id tools parent-context depth))
         (prepared
          (e-hooks-run-reduce hooks :pre-tool-call tool-call context))
         (tool (gethash (plist-get prepared :name)
                        (e-tools-registry-tools tools)))
         result)
    (e-harness-activity-emit-turn-event
     harness
     session-id
     turn-id
     'tool-started
     (e-harness-turn--nested-tool-payload
      prepared parent-tool-call depth))
    (setq result
          (if (and tool
                   (e-tools-long-blocking-class-p
                    (e-harness-turn--tool-blocking-class tool)))
              (e-harness-turn--nested-long-tool-result prepared tool)
            (e-tools-execute-nested-cheap-with-context
             tools prepared context)))
    (setq result
          (e-hooks-run-reduce hooks :post-tool-call result context))
    (setq result
          (e-hooks-run-reduce hooks :tool-result-presentation result context))
    (e-harness-activity-emit-turn-event
     harness
     session-id
     turn-id
     'tool-finished
     (e-harness-turn--nested-tool-payload
      prepared parent-tool-call depth :result result))
    result))

(defun e-harness-turn--tool-hook-context
    (harness session-id turn-id tools &optional parent-context depth)
  "Return the narrow hook context for a tool lifecycle in HARNESS."
  (let* ((turn-options (ignore-errors
                         (e-harness-turn-options harness session-id)))
          (turn-work (plist-get (gethash session-id
                                          (e-harness-active-turns harness))
                                :work-handle))
          (context
           (list :harness harness
                 :session-id session-id
                 :turn-id turn-id
                 :parent-work-id (and (e-work-handle-p turn-work)
                                      (e-work-handle-id turn-work))
                 :root-work-id (and (e-work-handle-p turn-work)
                                    (e-work-handle-id turn-work))
                  :deadline (plist-get turn-options :deadline)
                  :board-subscribe-aggregation
                  (e-harness-board-aggregation-function harness)
                 :tools tools
                :capabilities (e-harness-active-capabilities harness)
                :tool-executor
                (lambda (tool-call options current-context)
                  (e-harness-turn--execute-nested-tool
                   harness
                   session-id
                   turn-id
                   tools
                   tool-call
                   options
                   current-context)))))
    (when parent-context
      (setq context
            (append
             (list :nested t
                   :parent-tool-call (plist-get parent-context :tool-call)
                   :parent-tool-call-id
                   (plist-get (plist-get parent-context :tool-call) :id)
                   :depth depth)
             context)))
    context))

(defun e-harness-tool-lifecycle (harness session-id turn-id)
  "Return a harness-owned tool lifecycle for SESSION-ID and TURN-ID."
  (let ((tools (e-harness-tools harness session-id turn-id))
        hook-context)
    (cl-labels
        ((hooks ()
           (e-harness-hooks harness))
         (context ()
           (or hook-context
               (setq hook-context
                     (e-harness-turn--tool-hook-context
                      harness session-id turn-id tools)))))
      (e-tool-lifecycle-create
       :prepare (lambda (tool-call)
                  (e-hooks-run-reduce
                   (hooks)
                   :pre-tool-call
                   tool-call
                   (context)))
       :start
       (cl-function
         (lambda (tool-call &key on-request-start on-done on-error on-event
                            on-work-prepared archival-call
                            archival-rejected-p archival-received-arguments)
          (e-harness-turn--profile-call
           'harness.tool-start
           (list :session-id session-id
                 :turn-id turn-id
                 :metadata (list :tool-name (plist-get tool-call :name)))
           (lambda ()
             (e-tools-start
              tools
              tool-call
              :context (context)
              :on-request-start on-request-start
              :on-work-prepared on-work-prepared
              :on-event on-event
              :on-done
              (lambda (result)
                  (condition-case err
                      (let ((details-holder (list nil)))
                        (let ((staged-result
                               (e-harness-turn--tool-result-through-stages
                                harness session-id turn-id tool-call result
                                archival-call archival-rejected-p
                                archival-received-arguments (context)
                                details-holder)))
                          (let ((e-harness-activity-trusted-tool-details-uri
                                 (car details-holder)))
                            (when on-done
                              (funcall on-done staged-result)))))
                    (error
                     (if on-error
                         (funcall on-error err)
                       (signal (car err) (cdr err))))))
              :on-error on-error)))))))))
(cl-defun e-harness-compact-session-batch
    (harness session-id &key instructions keep-recent-tokens allow-active-turn
             (allow-split-turn 'inherit-active-turn) exclude-entry-ids turn-id
             (reason 'manual))
  "Synchronously compact SESSION-ID in HARNESS from batch/test code."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-harness-compact-session-batch))
  (when (and (not allow-active-turn)
             (e-harness-turn-state-active-turn-running-p
              (gethash session-id (e-harness-active-turns harness))))
    (signal 'e-harness-active-turn-exists (list session-id)))
  (let ((turn-id (or turn-id (e-harness-turn--next-turn-id)))
        preparation
        summary-parts
        summary-message
        summary-item-types
        request)
    (condition-case err
        (progn
          (e-harness-activity-emit-turn-event
           harness session-id turn-id 'compaction-started
           (list :instructions instructions
                 :reason reason
                 :active-turn allow-active-turn))
          (setq preparation
                (e-compaction-prepare
                 (e-harness-sessions harness)
                 session-id
                 :instructions instructions
                 :keep-recent-tokens keep-recent-tokens
                 :allow-split-turn (if (eq allow-split-turn
                                            'inherit-active-turn)
                                       allow-active-turn
                                     allow-split-turn)
                 :exclude-entry-ids exclude-entry-ids
                 :reason reason
                 :portable e-context-lifetime-shadow-projection-enabled))
          (e-harness-activity-emit-turn-event
           harness session-id turn-id 'compaction-prepared
           (list :first-kept-entry-id
                 (plist-get preparation :first-kept-entry-id)
                 :reason reason
                 :tokens-before (plist-get preparation :tokens-before)
                 :tokens-kept (plist-get preparation :tokens-kept)))
          (e-harness-activity-emit-turn-event
           harness session-id turn-id 'compaction-summary-started
           (list :backend t :reason reason))
          (e-backend-stream-batch
           (e-harness-backend harness)
           :messages (e-compaction-prepared-summary-messages preparation)
           ;; Summarization is a pure text task.  Strip the tool set from the
           ;; options: with tools present the model may answer with a tool-call
           ;; instead of an assistant message, yielding an empty summary and a
           ;; spurious compaction failure.
           :options (e-harness-context-options-without-tools
                     (e-harness-turn-options harness session-id))
           :on-request-start (lambda (value)
                               (setq request value))
           :on-item
           (lambda (item)
             (push (plist-get item :type) summary-item-types)
             (pcase (plist-get item :type)
               ('assistant-message
                (setq summary-message (plist-get item :content)))
               ('assistant-delta
                (push (or (plist-get item :content) "") summary-parts)))))
          (let* ((summary (string-trim
                           (or summary-message
                               (string-join (nreverse summary-parts) ""))))
                 (metadata (plist-get preparation :metadata)))
            (when (string-empty-p summary)
              (signal 'e-compaction-error
                      (list
                       "Compaction backend returned an empty summary"
                       (list :request-started
                             (and request t)
                             :item-types
                             (nreverse (delq nil summary-item-types))
                             :summary-source
                             'none))))
            (let* ((portable-preparation
                    (plist-get preparation :portable-input))
                   (portable-checkpoint
                    (when portable-preparation
                      (e-compaction-portable-checkpoint-from-summary
                       preparation summary)))
                   ;; Preflight both semantic inputs before the legacy
                   ;; compaction append, so stale generation/promotion state
                   ;; cannot leave a partial ordinary-only mutation.
                   (portable-application
                    (when portable-preparation
                      (e-compaction-preflight-portable-boundary
                       (e-harness-sessions harness)
                       session-id
                       preparation
                       portable-checkpoint)))
                   (record
                    (e-session-append-compaction
                     (e-harness-sessions harness)
                     session-id
                     summary
                     :first-kept-entry-id
                     (plist-get preparation :first-kept-entry-id)
                     :tokens-before (plist-get preparation :tokens-before)
                     :tokens-kept (plist-get preparation :tokens-kept)
                     :metadata metadata))
                   ;; Preserve the established compaction record as the
                   ;; audit/legacy owner, then deliberately supersede its
                   ;; model prefix with a portable generation while semantic
                   ;; lifetime projection is enabled.
                   (portable-generation
                     (when portable-application
                      (e-compaction-apply-portable-boundary
                       (e-harness-sessions harness)
                       session-id
                       portable-application))))
              (e-harness-context-provider-compaction-batch
               harness session-id portable-generation)
              (e-harness-activity-emit-turn-event
               harness session-id turn-id 'compaction-finished
               (list :compaction-id (plist-get record :id)
                     :portable-generation-id
                     (and portable-generation
                          (e-context-lifetime-generation-id
                           (e-context-lifetime-generation-from-record
                            (e-session-aggregate-context-record portable-generation))))
                     :reason (plist-get (plist-get record :metadata) :reason)
                     :first-kept-entry-id
                     (plist-get record :first-kept-entry-id)
                     :tokens-before (plist-get record :tokens-before)
                     :tokens-kept (plist-get record :tokens-kept)))
              record)))
      (error
       (let ((message (e-harness-turn--backend-error-message err))
             (details (e-harness-turn--backend-error-details err)))
         (when (and request (e-backend-request-p request))
           (ignore-errors (e-backend-cancel-request request)))
         (e-harness-activity-emit-turn-event
          harness session-id turn-id 'compaction-failed
          (list :message message :details details :reason reason))
	         (signal (car err) (cdr err)))))))

(defun e-harness-turn--backend-work-request-metadata (handle)
  "Return provider-facing request metadata projected from backend work HANDLE."
  (when (e-work-handle-p handle)
    (let* ((metadata (e-work-handle-metadata handle))
           (backend-metadata
            (copy-sequence
             (or (plist-get metadata :backend-request-metadata) nil))))
      (append backend-metadata
              (list :work-id (e-work-handle-id handle)
                    :work-handle handle
                    :work-transport (plist-get metadata :transport)
                    :backend-request
                    (plist-get metadata :backend-request))))))

(cl-defun e-harness-turn--compact-session-sqlite-start
    (harness session-id &key instructions keep-recent-tokens allow-active-turn
             (allow-split-turn 'inherit-active-turn) exclude-entry-ids turn-id
             (reason 'manual) on-done on-error)
  "Start SQL-backed compaction from one detached selected-path projection."
  (when (and (not allow-active-turn)
             (e-harness-turn-state-active-turn-running-p
              (gethash session-id (e-harness-active-turns harness))))
    (signal 'e-harness-active-turn-exists (list session-id)))
  (let* ((turn-id (or turn-id (e-harness-turn--next-turn-id)))
         (entry (gethash session-id (e-harness-active-turns harness)))
         (query-state
          (and entry
               (equal (plist-get entry :id) turn-id)
               (plist-get entry :session-query-state)))
         source-work summary-work append-work
         summary-parts summary-message summary-item-types
         preparation settled cancelled)
    (cl-labels
        ((record-failure (err)
           (e-harness-activity-emit-turn-event
            harness session-id turn-id 'compaction-failed
            (list :message (e-harness-turn--backend-error-message err)
                  :details (e-harness-turn--backend-error-details err)
                  :reason reason))
           (when on-error (funcall on-error err)))
         (fail (err)
           (unless settled
             (setq settled t)
             (dolist (work (list source-work summary-work append-work))
               (when (and (e-work-handle-p work)
                          (not (e-request-terminal-p
                                (e-work-handle-lifecycle work))))
                 (ignore-errors (e-work-cancel work))))
             (record-failure err)))
         (publish-record (record)
           (unless (or settled cancelled)
             (setq settled t)
             (e-harness-activity-emit-turn-event
              harness session-id turn-id 'compaction-finished
              (list :compaction-id (plist-get record :id)
                    :portable-generation-id nil
                    :reason (plist-get (plist-get record :metadata) :reason)
                    :first-kept-entry-id
                    (plist-get record :first-kept-entry-id)
                    :tokens-before (plist-get record :tokens-before)
                    :tokens-kept (plist-get record :tokens-kept)))
             (when on-done (funcall on-done record))))
         (append-summary (summary)
           (condition-case err
               (progn
                 (setq append-work
                       (e-session-append-compaction
                        (e-harness-sessions harness) session-id summary
                        :first-kept-entry-id
                        (plist-get preparation :first-kept-entry-id)
                        :tokens-before (plist-get preparation :tokens-before)
                        :tokens-kept (plist-get preparation :tokens-kept)
                        :metadata (plist-get preparation :metadata)))
                 (e-work-on-settle
                  append-work
                  (lambda (work)
                    (let ((status (e-work-status work)))
                      (if (eq (plist-get status :state) 'finished)
                          (publish-record (plist-get status :result))
                        (fail (or (plist-get status :error)
                                  '(e-compaction-error
                                    "Compaction append cancelled"))))))))
             (error (fail err))))
         (finish-summary (_result)
           (unless (or settled cancelled)
             (condition-case err
                 (let ((summary
                        (string-trim
                         (or summary-message
                             (string-join (nreverse summary-parts) "")))))
                   (when (string-empty-p summary)
                     (signal
                      'e-compaction-error
                      (list "Compaction backend returned an empty summary"
                            (list :item-types
                                  (nreverse (delq nil summary-item-types))))))
                   (let ((pending
                          (seq-filter
                           (lambda (work)
                             (and (e-work-handle-p work)
                                  (not (e-request-terminal-p
                                        (e-work-handle-lifecycle work)))))
                           (copy-sequence
                            (and entry
                                 (plist-get entry :persistence-works))))))
                     (if (null pending)
                         (append-summary summary)
                       (e-work-await-set
                        pending :mode 'all
                        :on-settle
                        (lambda (set-status)
                          (let ((failed
                                 (seq-find
                                  (lambda (work)
                                    (not (eq (plist-get (e-work-status work)
                                                        :state)
                                             'finished)))
                                  (plist-get set-status :done))))
                            (if failed
                                (fail
                                 (or (plist-get (e-work-status failed) :error)
                                     '(e-compaction-error
                                       "Compaction dependency cancelled")))
                              (append-summary summary))))))))
               (error (fail err)))))
         (start-summary (path)
           (condition-case err
               (progn
                 (setq preparation
                       (e-compaction-prepare-detached
                        session-id
                        (plist-get path :messages)
                        (or (plist-get path :compaction)
                            (plist-get path :latest-valid-compaction))
                        :instructions instructions
                        :keep-recent-tokens keep-recent-tokens
                        :allow-split-turn
                        (if (eq allow-split-turn 'inherit-active-turn)
                            allow-active-turn
                          allow-split-turn)
                        :exclude-entry-ids exclude-entry-ids
                        :reason reason))
                 (e-harness-activity-emit-turn-event
                  harness session-id turn-id 'compaction-prepared
                  (list :first-kept-entry-id
                        (plist-get preparation :first-kept-entry-id)
                        :reason reason
                        :tokens-before (plist-get preparation :tokens-before)
                        :tokens-kept (plist-get preparation :tokens-kept)))
                 (e-harness-activity-emit-turn-event
                  harness session-id turn-id 'compaction-summary-started
                  (list :backend t :reason reason))
                 (setq summary-work
                       (e-work-start
                        (e-work-spec-create
                         :id "compact_session_sqlite"
                         :description "Summarize detached SQLite context."
                         :execution 'backend :interactive-policy 'async
                         :owner 'harness
                         :backend (lambda (_arguments _context)
                                    (e-harness-backend harness))
                         :messages
                         (lambda (_arguments _context)
                           (e-compaction-prepared-summary-messages preparation))
                         :options
                         (lambda (_arguments _context)
                           (e-harness-context-options-without-tools
                            (e-harness-context-runtime--merge-turn-options
                             (e-harness-default-options harness)
                             (plist-get path :turn-options))))
                         :item-handler
                         (lambda (_handle item _arguments _context)
                           (unless (or settled cancelled)
                             (push (plist-get item :type) summary-item-types)
                             (pcase (plist-get item :type)
                               ('assistant-message
                                (setq summary-message
                                      (plist-get item :content)))
                               ('assistant-delta
                                (push (or (plist-get item :content) "")
                                      summary-parts))))))
                        nil
                        :context (list :session-id session-id :turn-id turn-id)
                        :on-done #'finish-summary
                        :on-error #'fail)))
             (error (fail err))))
         (cancel ()
           (unless settled
             (setq cancelled t settled t)
             (dolist (work (list source-work summary-work append-work))
               (when (and (e-work-handle-p work)
                          (not (e-request-terminal-p
                                (e-work-handle-lifecycle work))))
                 (ignore-errors (e-work-cancel work))))
             (record-failure (list 'quit "Context compaction cancelled")))
           t))
      (e-harness-activity-emit-turn-event
       harness session-id turn-id 'compaction-started
       (list :instructions instructions :reason reason
             :active-turn allow-active-turn))
      (if query-state
          (start-summary query-state)
        (setq source-work
              (e-session-async-context-path
               (e-harness-sessions harness) session-id))
        (e-work-on-settle
         source-work
         (lambda (work)
           (let ((status (e-work-status work)))
             (if (eq (plist-get status :state) 'finished)
                 (start-summary (plist-get status :result))
               (fail (or (plist-get status :error)
                         '(e-compaction-error
                           "Compaction context query cancelled"))))))))
      (e-backend-request-create
       :cancel #'cancel
       :metadata (list :operation 'compaction :session-id session-id
                       :turn-id turn-id
                       :work-handle (or source-work summary-work append-work))))))

(cl-defun e-harness-turn--compact-session-local-start
    (harness session-id &key instructions keep-recent-tokens allow-active-turn
             (allow-split-turn 'inherit-active-turn) exclude-entry-ids turn-id
             (reason 'manual) on-done on-error)
  "Start compacting local SESSION-ID in HARNESS and return a cancellable request.
ON-DONE receives the durable compaction record.  ON-ERROR receives an Emacs
condition list.  Preparation errors are reported before return by signaling and
also emitting the normal compaction failure event."
  (when (and (not allow-active-turn)
             (e-harness-turn-state-active-turn-running-p
              (gethash session-id (e-harness-active-turns harness))))
    (signal 'e-harness-active-turn-exists (list session-id)))
  (let ((turn-id (or turn-id (e-harness-turn--next-turn-id)))
        preparation
        summary-parts
        summary-message
        summary-item-types
        work-handle
        settled
        cancelled)
    (cl-labels
        ((record-failure (err)
           (let ((message (e-harness-turn--backend-error-message err))
                 (details (e-harness-turn--backend-error-details err)))
             (e-harness-activity-emit-turn-event
              harness session-id turn-id 'compaction-failed
              (list :message message :details details :reason reason))
             (when on-error
               (funcall on-error err))))
         (finish-error (err)
           (unless settled
             (setq settled t)
             (when (and work-handle
                        (not (e-request-terminal-p
                              (e-work-handle-lifecycle work-handle))))
               (ignore-errors (e-work-cancel work-handle)))
             (record-failure err)))
         (finish-done (_result)
           (unless (or settled cancelled)
             (condition-case err
                 (let* ((summary
                         (string-trim
                          (or summary-message
                              (string-join (nreverse summary-parts) "")))))
                   (when (string-empty-p summary)
                     (signal 'e-compaction-error
                             (list
                              "Compaction backend returned an empty summary"
                              (list :request-started
                                    (and work-handle
                                         (plist-get
                                          (e-work-handle-metadata work-handle)
                                          :backend-request)
                                         t)
                                    :item-types
                                    (nreverse (delq nil summary-item-types))
                                    :summary-source
                                    'none))))
                   (let* ((metadata (plist-get preparation :metadata))
                          (portable-preparation
                           (plist-get preparation :portable-input))
                          (portable-checkpoint
                           (when portable-preparation
                             (e-compaction-portable-checkpoint-from-summary
                              preparation summary)))
                          (portable-application
                           (when portable-preparation
                             (e-compaction-preflight-portable-boundary
                              (e-harness-sessions harness)
                              session-id
                              preparation
                              portable-checkpoint)))
                          (record
                           (e-session-append-compaction
                            (e-harness-sessions harness)
                            session-id
                            summary
                            :first-kept-entry-id
                            (plist-get preparation :first-kept-entry-id)
                            :tokens-before
                            (plist-get preparation :tokens-before)
                            :tokens-kept
                            (plist-get preparation :tokens-kept)
                            :metadata metadata))
                          (portable-generation
                           (when portable-application
                             (e-compaction-apply-portable-boundary
                              (e-harness-sessions harness)
                              session-id
                              portable-application))))
                     (e-harness-context-provider-compaction-start
                      harness session-id portable-generation)
                     (setq settled t)
                     (e-harness-activity-emit-turn-event
                      harness session-id turn-id 'compaction-finished
                      (list :compaction-id (plist-get record :id)
                            :portable-generation-id
                            (and portable-generation
                                 (e-context-lifetime-generation-id
                                  (e-context-lifetime-generation-from-record
                                   (e-session-aggregate-context-record
                                    portable-generation))))
                            :reason
                            (plist-get (plist-get record :metadata) :reason)
                            :first-kept-entry-id
                            (plist-get record :first-kept-entry-id)
                            :tokens-before (plist-get record :tokens-before)
                            :tokens-kept (plist-get record :tokens-kept)))
                     (when on-done
                       (funcall on-done record))))
               (error
                (finish-error err)))))
         (cancel ()
           (unless settled
             (setq cancelled t)
             (setq settled t)
             (when work-handle
               (ignore-errors (e-work-cancel work-handle)))
             (record-failure
              (list 'quit "Context compaction cancelled")))
           t))
      (condition-case err
          (progn
            (e-harness-activity-emit-turn-event
             harness session-id turn-id 'compaction-started
             (list :instructions instructions
                   :reason reason
                   :active-turn allow-active-turn))
            (setq preparation
                  (e-compaction-prepare
                   (e-harness-sessions harness)
                   session-id
                   :instructions instructions
                   :keep-recent-tokens keep-recent-tokens
                   :allow-split-turn (if (eq allow-split-turn
                                              'inherit-active-turn)
                                         allow-active-turn
                                       allow-split-turn)
                   :exclude-entry-ids exclude-entry-ids
                   :reason reason
                   :portable e-context-lifetime-shadow-projection-enabled))
            (e-harness-activity-emit-turn-event
             harness session-id turn-id 'compaction-prepared
             (list :first-kept-entry-id
                   (plist-get preparation :first-kept-entry-id)
                   :reason reason
                   :tokens-before (plist-get preparation :tokens-before)
                   :tokens-kept (plist-get preparation :tokens-kept)))
            (e-harness-activity-emit-turn-event
             harness session-id turn-id 'compaction-summary-started
             (list :backend t :reason reason))
            (setq work-handle
                  (e-work-start
                   (e-work-spec-create
                    :id "compact_session"
                    :description "Summarize older session context."
                    :execution 'backend
                    :interactive-policy 'async
                    :owner 'harness
                    :backend (lambda (_arguments _context)
                               (e-harness-backend harness))
                    :messages (lambda (_arguments _context)
                                (e-compaction-prepared-summary-messages
                                 preparation))
                    :options (lambda (_arguments _context)
                               (e-harness-context-options-without-tools
                                (e-harness-turn-options harness session-id)))
                    :item-handler
                    (lambda (_handle item _arguments _context)
                      (unless (or settled cancelled)
                        (push (plist-get item :type) summary-item-types)
                        (pcase (plist-get item :type)
                          ('assistant-message
                           (setq summary-message (plist-get item :content)))
                          ('assistant-delta
                           (push (or (plist-get item :content) "")
                                 summary-parts))))))
                   nil
                   :context (list :session-id session-id :turn-id turn-id)
                   :on-done #'finish-done
                   :on-error #'finish-error))
            (e-backend-request-create
             :cancel #'cancel
             :metadata (append
                        (list :operation 'compaction
                              :session-id session-id
                              :turn-id turn-id)
                        (e-harness-turn--backend-work-request-metadata
                         work-handle))))
        (error
         (record-failure err)
         (signal (car err) (cdr err)))))))

(cl-defun e-harness-compact-session-start
    (harness session-id &key instructions keep-recent-tokens allow-active-turn
             (allow-split-turn 'inherit-active-turn) exclude-entry-ids turn-id
             (reason 'manual) on-done on-error)
  "Start compacting SESSION-ID without blocking interactive callers."
  (apply
   (if (e-session-async-enabled-p (e-harness-sessions harness))
       #'e-harness-turn--compact-session-sqlite-start
     #'e-harness-turn--compact-session-local-start)
   harness session-id
   (append
    (list :instructions instructions
          :keep-recent-tokens keep-recent-tokens
          :allow-active-turn allow-active-turn
          :allow-split-turn allow-split-turn
          :exclude-entry-ids exclude-entry-ids
          :turn-id turn-id :reason reason)
    (when on-done (list :on-done on-done))
    (when on-error (list :on-error on-error)))))
(defun e-harness-turn--auto-compaction-reserve-tokens ()
  "Return a normalized auto-compaction reserve."
  (if (and (integerp e-harness-auto-compaction-reserve-tokens)
           (>= e-harness-auto-compaction-reserve-tokens 0))
      e-harness-auto-compaction-reserve-tokens
    16384))

(defun e-harness-turn--auto-compaction-query-state (harness session-id)
  "Return SESSION-ID's bounded executing query state, or nil."
  (e-harness-executing-session-state harness session-id))

(defun e-harness-turn--auto-compaction-messages (harness session-id)
  "Return messages available to auto-compaction policy for SESSION-ID."
  (let ((store (e-harness-sessions harness)))
    (if (e-session-async-enabled-p store)
        (plist-get (e-harness-turn--auto-compaction-query-state
                    harness session-id)
                   :messages)
      (e-session-local-messages store session-id))))

(defun e-harness-turn--auto-compaction-latest (harness session-id)
  "Return the latest compaction available to policy for SESSION-ID."
  (let ((store (e-harness-sessions harness)))
    (if (e-session-async-enabled-p store)
        (plist-get (e-harness-turn--auto-compaction-query-state
                    harness session-id)
                   :latest-valid-compaction)
      (e-session-local-latest-valid-compaction store session-id))))

(defun e-harness-turn--auto-compaction-suffix-tokens (harness session-id compaction)
  "Return estimated current suffix tokens since COMPACTION."
  (let* ((boundary-id (plist-get compaction :first-kept-entry-id))
         (store (e-harness-sessions harness))
         (entries
          (if (e-session-async-enabled-p store)
              (when boundary-id
                (let ((tail
                       (seq-drop-while
                        (lambda (message)
                          (not (equal (plist-get message :id) boundary-id)))
                        (e-harness-turn--auto-compaction-messages
                         harness session-id))))
                  (cdr tail)))
            (and boundary-id
                 (cdr (e-session-local-entries-from
                       store session-id boundary-id))))))
    (when entries
      (apply #'+ (mapcar #'e-compaction-entry-token-estimate entries)))))

(defun e-harness-turn--auto-compaction-no-progress-p (harness session-id)
  "Return non-nil when another auto-compaction would not move the boundary."
  (when-let* ((latest (e-harness-turn--auto-compaction-latest
                      harness session-id)))
    (let ((suffix-tokens
           (e-harness-turn--auto-compaction-suffix-tokens
            harness session-id latest))
          (keep (if (and (integerp e-compaction-keep-recent-tokens)
                         (> e-compaction-keep-recent-tokens 0))
                    e-compaction-keep-recent-tokens
                  20000)))
      (and suffix-tokens (< suffix-tokens keep)))))

(defun e-harness-turn--auto-compaction-useful-prefix-p
    (harness session-id exclude-entry-ids)
  "Return non-nil when auto-compaction has enough prefix messages to summarize."
  (> (length
      (cl-remove-if
       (lambda (message)
         (member (plist-get message :id) exclude-entry-ids))
       (e-harness-turn--auto-compaction-messages harness session-id)))
     1))

(defun e-harness-turn--auto-compaction-needed-p (harness session-id &optional context)
  "Return non-nil when SESSION-ID should auto-compact before prompting."
  (when e-harness-auto-compaction-enabled
    (when-let*
        ((usage-status
          (e-context-budget-status
           harness session-id
           :prefer-token-usage t
           :estimate-context nil))
         (status
          (if (plist-get usage-status :used-tokens)
              usage-status
            (or (and context
                     (let* ((options (plist-get context :options))
                            (model (plist-get options :model))
                            (used
                             (e-context-budget-context-token-estimate context))
                            (window (e-context-budget-model-window model)))
                       (list :used-tokens used :window window)))
                (e-context-budget-status
                 harness session-id
                 :prefer-token-usage t
                 :estimate-context t)))))
      (let ((used (plist-get status :used-tokens))
            (window (plist-get status :window))
            (reserve (e-harness-turn--auto-compaction-reserve-tokens)))
        (and (integerp used)
             (integerp window)
             (> window reserve)
             (> used (- window reserve))
             (not (e-harness-turn--auto-compaction-no-progress-p
                   harness session-id)))))))

(defun e-harness-turn--maybe-auto-compact-session
    (harness session-id &optional active-turn-id exclude-entry-ids context)
  "Best-effort auto-compact SESSION-ID when it is near the context window."
  (when (e-harness-turn--auto-compaction-needed-p harness session-id context)
    (condition-case nil
        (let ((args (list :reason 'auto)))
          (when active-turn-id
            (setq args
                  (append args
                          (list :allow-active-turn t
                                :allow-split-turn nil
                                :exclude-entry-ids exclude-entry-ids
                                :turn-id active-turn-id))))
          (apply #'e-harness-compact-session-start
                 harness session-id
                 (append args (list :on-error #'ignore))))
      (e-compaction-error nil))))

(defun e-harness-turn--cancel-active-request (entry)
  "Cancel ENTRY's active backend or tool request when one exists."
  (when-let* ((work (plist-get entry :input-admission-work)))
    (when (e-work-handle-p work)
      (e-work-cancel work)))
  (when-let* ((work (plist-get entry :context-work)))
    (when (e-work-handle-p work)
      (e-work-cancel work)))
  (when-let* ((request (plist-get entry :request)))
    (condition-case err
        (cond
         ((e-backend-request-p request)
          (e-backend-cancel-request request))
         ((e-tools-request-p request)
          (e-tools-cancel-request request)))
      (error
       (plist-put entry :cancel-error err)
       nil))))

(defun e-harness-turn--cancelled-tool-result (tool-call)
  "Return a structured cancellation result for TOOL-CALL."
  (list :tool-call-id (plist-get tool-call :id)
        :name (plist-get tool-call :name)
        :status 'error
        :content "Cancelled"
        :metadata '(:error cancelled)))

(defun e-harness-turn--tool-result-through-stages
    (harness session-id turn-id tool-call result
             &optional archival-call archival-rejected-p
             archival-received-arguments stage-context details-holder)
  "Run RESULT through semantic, archival, and presentation stages.
ARCHIVAL-CALL and its rejection fields are an internal detached side channel;
they are never included in the transcript message or lifecycle event."
  (let* ((hooks (e-harness-hooks harness))
         (context (or stage-context
                      (e-harness-turn--tool-hook-context
                       harness session-id turn-id
                       (e-harness-tools harness session-id turn-id))))
         (semantic-result
          (e-hooks-run-reduce hooks :post-tool-call result context))
         (detail-context
          (append (list :tool-call tool-call
                        ;; This slot is populated only by the successful
                        ;; invocation-details stage below.  Keep it separate
                        ;; from result metadata so a caller cannot authorize
                        ;; truncation by forging a canonical URI.
                        :invocation-details-uri nil
                        :archival-call archival-call
                        :archival-rejected-p archival-rejected-p
                        :archival-received-arguments
                        archival-received-arguments)
                  context))
         (archived-result
          (e-hooks-run-reduce hooks :invocation-details semantic-result
                              detail-context))
         ;; Only the details stage may authorize reuse of its canonical URI.
         ;; Preserve the original context for semantic hooks and expose the
         ;; successful stage result to presentation through a fresh overlay.
         (presentation-context
          (append (list :invocation-details-uri
                        (plist-get detail-context :invocation-details-uri))
                  context)))
    (when details-holder
      (setcar details-holder
              (plist-get detail-context :invocation-details-uri)))
    (e-hooks-run-reduce hooks :tool-result-presentation archived-result
                        presentation-context)))

(defun e-harness-turn--append-cancelled-tool-result (harness session-id turn-id entry)
  "Append a cancellation tool result when ENTRY has an open tool call."
  (when-let* ((tool-call (plist-get entry :open-tool-call)))
    (let* ((archival-call (plist-get entry :open-tool-archival-call))
           (archival-rejected-p (plist-get entry :open-tool-archival-rejected-p))
           (archival-received-arguments
            (plist-get entry :open-tool-archival-received-arguments))
           (details-holder (list nil))
           (result (e-harness-turn--tool-result-through-stages
                    harness session-id turn-id tool-call
                    (e-harness-turn--cancelled-tool-result tool-call)
                    archival-call archival-rejected-p
                    archival-received-arguments nil details-holder))
           (message (list :role 'tool
                          :content result
                          :metadata nil)))
      (let ((e-harness-activity-trusted-tool-details-uri (car details-holder)))
        (e-harness-turn--append-message harness session-id turn-id message)
        (e-harness-activity-emit-turn-event
         harness session-id turn-id 'tool-finished
         (list :tool-call tool-call :result result)))
      (plist-put entry :open-tool-call nil)
      (plist-put entry :open-tool-archival-call nil)
      (plist-put entry :open-tool-archival-rejected-p nil)
      (plist-put entry :open-tool-archival-received-arguments nil))))

(defun e-harness-turn--backend-error-message (err)
  "Return the compact user-visible error message for condition ERR.
For `e-compaction-error' return only the bare reason string; the
`define-error' message and the presentation layer both add their own
\"Context compaction failed\" prefix, so including it here triples it."
  (cond
   ((and (consp err)
         (eq (car err) 'e-loop-backend-error)
         (stringp (cadr err)))
    (cadr err))
   ((and (consp err)
         (eq (car err) 'e-compaction-error)
         (stringp (cadr err)))
    (cadr err))
   ;; Fallthrough for an arbitrary condition.  Route through the bounded
   ;; formatter: a cyclic or huge error payload would otherwise wedge the
   ;; printer and spin Emacs at 100% CPU.
   (t (e-work-error-message err))))

(defun e-harness-turn--backend-error-details (err)
  "Return structured provider details from condition ERR, or nil."
  (when (consp err)
    (pcase (car err)
      ('e-loop-backend-error
       (nth 2 err))
      ('e-compaction-error
       (caddr err))
      ('e-work-deadline-exceeded
       (caddr err)))))

(defun e-harness-turn--emit-turn-failed
    (harness session-id turn-id error-message &optional details)
  "Emit a turn-failed event from HARNESS.
SESSION-ID and TURN-ID identify the failed turn.  ERROR-MESSAGE describes the
provider or loop failure."
  (e-harness-activity-emit-turn-event
   harness
   session-id
   turn-id
   'turn-failed
   (let ((payload (list :error error-message)))
     (when details
       (plist-put payload :details details))
     payload)))

(defun e-harness-turn--append-message (harness session-id turn-id message)
  "Append MESSAGE in HARNESS for SESSION-ID TURN-ID and emit `message-added'."
  (e-harness-turn--profile-call
   'harness.message-append
   (list :session-id session-id
         :turn-id turn-id
         :metadata (list :role (and (plist-get message :role)
                                    (symbol-name (plist-get message :role)))))
   (lambda ()
     (let ((message (copy-sequence message)))
       ;; Allocate semantic identity at application admission.  Both the
       ;; optimistic detached path and SQLite's eventual record must expose
       ;; the same message; waiting for relational derivation would publish a
       ;; transient id-less value to context providers and Board subscribers.
       (unless (plist-get message :id)
         (plist-put message :id (e-session-generate-ulid)))
       (when turn-id
         (plist-put message :turn-id turn-id))
       (let ((append-result
              (e-session-append-message (e-harness-sessions harness)
                                        session-id
                                        message)))
         ;; The asynchronous session facade returns the request work for the
         ;; immutable append.  That handle is not
         ;; the semantic message and must never cross the harness event port.
         ;; Synchronous stores still return their normalized durable message.
         (if (not (e-work-handle-p append-result))
             (setq message append-result)
           (pcase-let* ((status (e-work-status append-result))
                        (state (plist-get status :state)))
             ;; Capacity rejection is represented by an already-failed work;
             ;; it is not a successful enqueue and therefore cannot be
             ;; published optimistically.
             (when (eq state 'failed)
               (let ((err (plist-get status :error)))
                 (signal (car err) (cdr err))))
             (when (eq state 'cancelled)
               (signal 'e-work-cancelled (list append-result)))
             ;; The executing turn may need an exact causal boundary (for
             ;; example, a model-requested compaction) before these optimistic
             ;; messages are durable.  Retain only its unsettled write handles
             ;; and bounded selected-path message projection.
             (when-let* ((entry
                          (gethash session-id
                                   (e-harness-active-turns harness))))
               (when (equal (plist-get entry :id) turn-id)
                 (push append-result (plist-get entry :persistence-works))
                 (when-let* ((query-state
                              (plist-get entry :session-query-state)))
                   (let ((messages (plist-get query-state :messages)))
                     (unless (seq-some
                              (lambda (existing)
                                (equal (plist-get existing :id)
                                       (plist-get message :id)))
                              messages)
                       (plist-put query-state :messages
                                  (append messages
                                          (list (copy-tree message t)))))))
                 (e-work-on-settle
                  append-result
                  (lambda (_settled)
                    (when-let* ((current
                                 (gethash session-id
                                          (e-harness-active-turns harness))))
                      (when (equal (plist-get current :id) turn-id)
                        (plist-put
                         current :persistence-works
                         (delq append-result
                               (plist-get current :persistence-works)))))))))
             ;; Provider context causally depends on the durable user input.
             ;; Retain only this live turn's append handle and start the query
             ;; from its explicit commit acknowledgement.
             (when (eq (plist-get message :role) 'user)
               (when-let* ((entry
                            (gethash session-id
                                     (e-harness-active-turns harness))))
                 (when (equal (plist-get entry :id) turn-id)
                   (plist-put entry :input-admission-work append-result)))))))
       ;; Terminal hooks and the Board adapter need the accepted assistant
       ;; value before SQLite acknowledges the append.  Keep that one value on
       ;; the active turn that owns it; durable session state replaces it on
       ;; ordinary synchronous and post-commit reads.
       (when (eq (plist-get message :role) 'assistant)
         (when-let* ((entry (gethash session-id
                                    (e-harness-active-turns harness))))
           (when (equal (plist-get entry :id) turn-id)
             (plist-put entry :assistant-message (copy-sequence message)))))
       (e-harness-activity-emit-turn-event
        harness session-id turn-id 'message-added (list :message message))
       message))))

(defun e-harness-set-message-display (harness session-id message-id display)
  "Set DISPLAY on SESSION-ID's message MESSAGE-ID in HARNESS.
DISPLAY is a display disposition symbol (e.g. `hidden'); nil restores the
default visible state.  Persists the change through the session store and emits
a `message-updated' turn event so a live shell can drop or restore the block.
For asynchronous SQLite, return the admitted work and emit only after its
commit acknowledgement.  For a local test store, return the updated message."
  (let ((result (e-session-set-message-display
                 (e-harness-sessions harness)
                 session-id message-id display)))
    (if (not (e-work-handle-p result))
        (when result
          (e-harness-activity-emit-turn-event
           harness session-id (plist-get result :turn-id)
           'message-updated (list :message result))
          result)
      (let ((entry (gethash session-id (e-harness-active-turns harness))))
        (when entry
          (push result (plist-get entry :persistence-works)))
        (e-work-on-settle
         result
         (lambda (work)
           (when entry
             (plist-put entry :persistence-works
                        (delq work (plist-get entry :persistence-works))))
           (let ((status (e-work-status work)))
             (when (eq (plist-get status :state) 'finished)
               (e-harness-activity-emit-turn-event
                harness session-id nil 'message-updated
                (list :message
                      (list :id message-id :display display))))))))
      result)))

(defun e-harness-turn--append-user-message
    (harness session-id turn-id prompt &optional metadata)
  "Append PROMPT as the user message in HARNESS for SESSION-ID and TURN-ID."
  ;; Endpoint tokens fence one live attachment.  They remain on the active
  ;; entry and queued input while in use, but are neither transcript context
  ;; nor durable data (and production tokens are intentionally opaque
  ;; structs, not JSON values).
  (setq metadata (e-harness-turn--durable-input-metadata metadata))
  (e-harness-turn--append-message
   harness
   session-id
   turn-id
   (list :role 'user
         :origin (or (plist-get metadata :input-origin) 'human)
         :content prompt
         :metadata metadata)))

(defun e-harness-turn--turn-assistant-message (harness session-id turn-id)
  "Return the final assistant message for SESSION-ID TURN-ID in HARNESS.
When a turn produced multiple assistant messages, return the last one."
  (or (when-let* ((entry (gethash session-id
                                  (e-harness-active-turns harness))))
        (when (equal (plist-get entry :id) turn-id)
          (copy-sequence (plist-get entry :assistant-message))))
      (let ((store (e-harness-sessions harness)))
        ;; Async SQLite history is query-only.  A lifecycle hook may inspect
        ;; the live turn value, but it must not reconstruct the session when
        ;; that value is absent.
        (unless (e-session-async-enabled-p store)
          (car (last (cl-remove-if-not
                      (lambda (message)
                        (and (eq (plist-get message :role) 'assistant)
                             (equal (plist-get message :turn-id) turn-id)))
                      (e-session-local-messages store session-id))))))))

(defun e-harness-turn--turn-session-metadata (harness session-id)
  "Return detached metadata for SESSION-ID's executing turn in HARNESS.
For an asynchronous session store, metadata comes only from the bounded query
state retained by the active turn.  In-memory stores may read their local
session value directly."
  (let* ((store (e-harness-sessions harness))
         (session
          (or (e-harness-executing-session-state harness session-id)
              (unless (e-session-async-enabled-p store)
                (e-session-local-state store session-id)))))
    (copy-tree (plist-get session :metadata) t)))

(defun e-harness-turn--run-turn-finished-hooks
    (harness session-id turn-id result &optional model-context)
  "Run `:turn-finished' hooks for HARNESS SESSION-ID TURN-ID over RESULT."
  (e-hooks-run-reduce
   (e-harness-hooks harness)
   :turn-finished
   result
   (list :harness harness
         :session-id session-id
         :turn-id turn-id
         :model-context model-context
         :session-metadata
         (e-harness-turn--turn-session-metadata harness session-id)
         :assistant-message
         (e-harness-turn--turn-assistant-message harness session-id turn-id))))

(cl-defun e-harness-turn--run-prompt-turn-async
    (harness session-id turn-id &key on-request-start on-done on-error
             cancelled-p append-message on-event context drain-pending-input
             on-context-refresh on-response-preflight on-response-complete
             on-tool-observation on-tool-observation-presentation
             on-tool-call-start)
  "Start a queued async prompt turn for SESSION-ID and TURN-ID in HARNESS."
  (e-harness-turn--profile-call
   'harness.prompt-turn-async-start
   (list :session-id session-id
         :turn-id turn-id)
   (lambda ()
     (let ((context (or context
                        (e-harness-turn-context harness session-id turn-id))))
        (e-loop-start-turn
        :session-id session-id
        :turn-id turn-id
        :messages (plist-get context :messages)
        :backend (e-harness-backend harness)
        :tools (e-harness-tools harness session-id turn-id)
        :tool-lifecycle (e-harness-tool-lifecycle harness session-id turn-id)
        :options (plist-get context :options)
        :segments (plist-get context :segments)
        :lifetime-frame (plist-get context :lifetime-frame)
        :on-response-preflight on-response-preflight
        :on-response-complete on-response-complete
        :on-tool-observation on-tool-observation
        :on-tool-observation-presentation on-tool-observation-presentation
        :on-tool-call-start on-tool-call-start
         :turn-work-handle (plist-get
                            (gethash session-id
                             (e-harness-active-turns harness))
                             :work-handle)
         :board-enroll-work (e-harness-work-enrollment-function harness)
        :on-event (or on-event
                      (lambda (type payload)
                        (e-harness-activity-emit-turn-event
                         harness session-id turn-id type payload)))
        :on-request-start on-request-start
        :callback-dispatcher
        (lambda (callback)
          (e-session-dispatch-admitted-callback
           (e-harness-sessions harness) session-id callback))
        :cancelled-p cancelled-p
        :on-done on-done
        :on-error on-error
        :refresh-context
        (lambda ()
          ;; A context refresh is atomic at the loop boundary: messages and
          ;; all request-derived options (segments, observation frontier, and
          ;; anchor decision) come from one fresh harness projection.
          (let ((fresh-context
                 (e-harness-turn-context harness session-id turn-id)))
            (when on-context-refresh
              (funcall on-context-refresh fresh-context))
            fresh-context))
        :drain-pending-input
        (or drain-pending-input
            (lambda ()
              (mapcar
               (lambda (item)
                 (list :role 'user
                       :content (plist-get item :prompt)
                       :metadata (plist-get item :metadata)))
               (e-harness-turn--drain-pending-steering-input
                harness
                (gethash session-id
                         (e-harness-active-turns harness))))))
        :append-message
        (or append-message
            (lambda (message)
              (e-harness-turn--append-message
               harness session-id turn-id message))))))))

(cl-defun e-harness-attached-turn-submit-batch
    (harness session-id prompt &key metadata attachment-token
             attached-turn-port)
  "Synchronously append PROMPT and run one backend turn from batch/test code."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-harness-attached-turn-submit-batch))
  (e-harness-turn--profile-call
   'harness.prompt-batch
   (list :session-id session-id)
   (lambda ()
     (e-harness-attached-turn-submit
     harness session-id prompt :metadata metadata
      :attachment-token attachment-token
      :attached-turn-port attached-turn-port)
     (let ((entry (e-harness-wait-batch harness session-id)))
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
         (_ entry))))))

(cl-defun e-harness-attached-turn-submit
    (harness session-id prompt &key delay metadata attachment-token
             attached-turn-port)
  "Append PROMPT and run one backend turn asynchronously in HARNESS.
Return the queued turn id.  DELAY is primarily for tests and queued-turn
cancellation.  SESSION-ID identifies the session."
  (setq attached-turn-port
        (e-harness-turn--require-attached-port
         harness session-id attachment-token attached-turn-port))
  (setq metadata (plist-put (copy-sequence metadata)
                            :board-endpoint-token attachment-token))
  (e-harness-turn--profile-call
   'harness.prompt-async
   (list :session-id session-id)
   (lambda ()
     (when (e-harness-turn-state-active-turn-running-p
            (gethash session-id (e-harness-active-turns harness)))
       (signal 'e-harness-active-turn-exists (list session-id)))
      (let* ((turn-id (e-harness-turn--next-turn-id))
              (turn-work
              (e-work-prepare
               (e-harness-turn--turn-work-spec)
               nil
               :context (list :session-id session-id
                              :turn-id turn-id
                              :work-kind 'turn
                               :domain-ref (format "turn:%s" turn-id))))
             (entry (list :id turn-id
                          :status 'running
                          :work-handle turn-work
                         :result nil
                         :error nil
                         :error-details nil
                          :condition nil
                          :timer nil
                          :endpoint-token attachment-token
                          :attached-turn-port attached-turn-port
                          :request nil)))
        (when-let* ((enroll (e-harness-work-enrollment-function harness)))
          (condition-case err
              (funcall enroll turn-work nil)
            (error
             (e-work-fail turn-work err)
             (signal (car err) (cdr err)))))
        (e-harness-turn-state-put-active-turn harness session-id entry)
        (condition-case err
            (plist-put entry
                      :prompt-message-id
                      (plist-get
                       (e-harness-turn--append-user-message
                        harness session-id turn-id prompt metadata)
                       :id))
          (error
          (let ((message (e-harness-turn--backend-error-message err))
                (details (e-harness-turn--backend-error-details err)))
            (plist-put entry :status 'error)
            (plist-put entry :error message)
            (plist-put entry :error-details details)
            (e-harness-turn--emit-turn-failed
             harness session-id turn-id message details)
            (e-harness-turn-state-remove-active-turn harness session-id entry)
             (signal (car err) (cdr err)))))
        (when (plist-get metadata :board-delivery-id)
          (e-harness-activity-emit-turn-event
           harness session-id turn-id 'input-consumed
           (list :delivery-id (copy-tree (plist-get metadata :board-delivery-id))
                 :board-id (plist-get metadata :board-id)
                 :participant-id (plist-get metadata :board-participant-id)
                 :endpoint-token
                 (let ((token (plist-get metadata :board-endpoint-token)))
                   (if (vectorp token) (copy-sequence token) (copy-tree token)))
                 :endpoint-generation
                 (copy-tree (plist-get metadata :board-endpoint-generation))
                 :message-id (plist-get entry :prompt-message-id))))
        (e-work-start-prepared turn-work :arguments nil
                               :context (list :session-id session-id
                                              :turn-id turn-id
                                              :work-kind 'turn
                                              :domain-ref
                                              (format "turn:%s" turn-id)))
        (cl-labels
           ((active-entry-p ()
              (eq (gethash session-id (e-harness-active-turns harness))
                  entry))
            (cancelled-p ()
              (or (plist-get entry :cancelled)
                  (not (active-entry-p))))
            (maybe-retry-error
             (message details)
             ;; Schedule a retry for a retryable error (e.g. 429) while inside
             ;; the elapsed budget.  Return non-nil when a retry was scheduled
             ;; so the caller skips settling the turn as failed.
             ;;
             ;; When the error names a concrete reset time, wait until then
             ;; instead of using blind exponential backoff, and let that known
             ;; reopen extend the budget so the turn is not abandoned minutes
             ;; before capacity returns.
             (when (and (> e-harness-retry-max-elapsed-seconds 0)
                        (e-harness-turn--retryable-error-p details))
               (let* ((now (float-time))
                      (deadline (or (plist-get entry :retry-deadline)
                                    (+ now
                                       e-harness-retry-max-elapsed-seconds)))
                      (attempt (1+ (or (plist-get entry :retry-attempt) 0)))
                      (reset-wait
                       (e-harness-turn--retry-reset-seconds details))
                      (wait (or reset-wait
                                (e-harness-turn--retry-backoff-seconds attempt)))
                      ;; A known reset can push the deadline out (bounded by
                      ;; the reset helper's own cap) so we do not give up right
                      ;; before the window reopens.
                      (effective-deadline (if reset-wait
                                              (max deadline (+ now wait 1.0))
                                            deadline)))
                 (plist-put entry :retry-deadline effective-deadline)
                 (when (< (+ now wait) effective-deadline)
                   (plist-put entry :retry-attempt attempt)
                   (e-harness-activity-emit-turn-event
                    harness session-id turn-id 'turn-retrying
                    (list :error message
                          :details details
                          :attempt attempt
                          :backoff-seconds wait
                          :reset-wait reset-wait))
                   (plist-put entry :timer
                              (run-at-time wait nil #'start-turn))
                   t))))
            (finish-error
             (err)
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (let* ((message (e-harness-turn--backend-error-message err))
                      (details
                       (e-backend-normalize-error-details
                        (e-harness-backend harness)
                        message
                        (e-harness-turn--backend-error-details err)
                        err)))
                 (unless (maybe-retry-error message details)
                   (plist-put entry :status 'error)
                   (plist-put entry :condition err)
                   (plist-put entry :error message)
                    (plist-put entry :error-details details)
                    (e-work-fail turn-work err)
                    (e-harness-turn--emit-turn-failed
                    harness session-id turn-id message details)
                   (e-harness-turn--drain-pending-steering-input harness entry)
                   (e-harness-turn--schedule-queue-drain
                    harness session-id entry)))))
            (finish-settlement-error
             (err)
             ;; The provider has already completed, so a terminal lifecycle
             ;; failure is not retryable provider work.  Settle it once and
             ;; publish the same visible failure edge as any other failed turn.
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (let ((message (e-harness-turn--backend-error-message err))
                     (details '(:stage turn-finished-hooks)))
                 (plist-put entry :status 'error)
                 (plist-put entry :condition err)
                 (plist-put entry :error message)
                 (plist-put entry :error-details details)
                 (e-work-fail turn-work err)
                 (e-harness-turn--emit-turn-failed
                  harness session-id turn-id message details)
                 (e-harness-turn--drain-pending-steering-input harness entry)
                 (e-harness-turn--schedule-queue-drain
                  harness session-id entry))))
            (finish-done
             (result)
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (e-harness-context-persist-provider-anchor-candidates
                harness
                session-id
                turn-id
                (plist-get entry :context)
                (nreverse (plist-get entry :provider-anchor-candidates))
                (plist-get entry :provider-anchor-final-request-ordinal))
               (condition-case err
                   (let ((hooked-result
                          (e-harness-turn--run-turn-finished-hooks
                           harness session-id turn-id result
                           (plist-get entry :context))))
                     (plist-put entry :result hooked-result)
                     (plist-put entry :status 'done)
                     ;; `e-loop' reports its own loop-level completion before
                     ;; this callback.  Do not expose that provisional edge as
                     ;; the harness/session terminal event: capability hooks
                     ;; still own settlement work at this point.  The public
                     ;; terminal edge is emitted here, after every hook has
                     ;; observed the final assistant message and recorded any
                     ;; durable audit metadata.
                     (e-harness-activity-emit-turn-event
                      harness session-id turn-id 'turn-finished
                      (list :reason (plist-get hooked-result :reason)))
                     (e-work-finish turn-work hooked-result)
                     (e-harness-turn--drain-pending-steering-input harness entry)
                     (e-harness-turn--schedule-queue-drain
                      harness session-id entry))
                 (error (finish-settlement-error err)))))
            (start-provider
             (context)
             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
               (plist-put entry :context context)
               (e-harness-turn-state-set-context-frame
                entry (plist-get context :lifetime-frame))
               (e-harness-turn-state-set-lifetime-generation
                entry (plist-get context :lifetime-generation))
               (plist-put entry :provider-anchor-candidates nil)
               (plist-put entry :provider-anchor-final-request-ordinal nil)
               (e-harness-turn--run-prompt-turn-async
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
                        (pcase type
                          ('tool-started
                           (plist-put entry :open-tool-call payload))
                          ('tool-finished
                           (plist-put entry :open-tool-call nil)
                           (plist-put entry :open-tool-archival-call nil)
                           (plist-put entry :open-tool-archival-rejected-p nil)
                           (plist-put entry :open-tool-archival-received-arguments nil))
                      ('provider-request-finished
	                       ;; A request that completes clears the transient-retry
	                       ;; window: the budget bounds a consecutive failure
	                       ;; burst, not the turn's total wall clock.  Without
	                       ;; this a long turn (many successful requests, slow
	                       ;; tools) lets the deadline planted by an early blip
	                       ;; expire, so a late transport blip settles the turn
	                       ;; failed instead of retrying.
                       (when (eq (plist-get payload :status) 'done)
                         (plist-put entry :retry-deadline nil)
                         (plist-put entry :retry-attempt nil)
                         ;; Candidate ownership is per final successful
                         ;; provider request, not per whole turn.  A prior
                         ;; request may have produced a usable in-turn anchor
                         ;; while a later refreshed request owns the final
                         ;; assistant response.
                         (plist-put
                          entry
                          :provider-anchor-final-request-ordinal
                          (plist-get payload :provider-request-ordinal))))
                      ('provider-anchor-candidate
                       ;; Only loop-accepted candidates are ownership facts.
                       ;; Raw backend candidate items never enter the durable
                       ;; candidate collection.
                       (when (plist-get payload :accepted-for-persistence)
                         (plist-put
                          entry
                          :provider-anchor-candidates
                          (cons payload
                                (plist-get
                                 entry
                                 :provider-anchor-candidates))))))
	                    ;; `turn-finished' here is the loop's private terminal
	                    ;; edge.  The harness emits its public terminal event
	                    ;; after `:turn-finished' hooks settle in `finish-done'.
                    (unless (eq type 'turn-finished)
	                      (e-harness-activity-emit-turn-event
                       harness session-id turn-id type payload))))
                :on-tool-call-start
                (lambda (tool-call archival-call rejected-p received-arguments)
                  ;; The transcript callback above receives only TOOL-CALL.
                  ;; Keep the complete rejected invocation on this detached
                  ;; lifecycle side channel until the archival stage runs.
                  (when (and (active-entry-p)
                             (not (plist-get entry :cancelled)))
                    (plist-put entry :open-tool-call tool-call)
                    (plist-put entry :open-tool-archival-call archival-call)
                    (plist-put entry :open-tool-archival-rejected-p rejected-p)
                    (plist-put entry :open-tool-archival-received-arguments
                               received-arguments)))
                :append-message
	                (lambda (message)
	                  (when (and (active-entry-p)
	                             (not (plist-get entry :cancelled)))
	                    (e-harness-turn--append-message
	                     harness session-id turn-id message)))
                :drain-pending-input
                (lambda ()
	                  (when (and (active-entry-p)
	                             (not (plist-get entry :cancelled)))
	                    (mapcar
	                     (lambda (item)
	                       (list :role 'user
	                             :content (plist-get item :prompt)
	                             :metadata (plist-get item :metadata)))
                     (e-harness-turn--drain-pending-steering-input
                      harness entry))))
                :on-context-refresh
                (lambda (fresh-context)
                  ;; The loop calls this only for an atomic refresh that
                  ;; belongs to this active entry.  Keep the entry's context
                  ;; authoritative for final candidate ownership, but never
                  ;; let a stale callback mutate a replacement turn.
                  (when (and (active-entry-p)
                             (equal (plist-get entry :id) turn-id)
                             (not (plist-get entry :cancelled)))
                    (plist-put entry :context fresh-context)
                    (e-harness-turn-state-set-context-frame
                     entry (plist-get fresh-context :lifetime-frame))
                    (e-harness-turn-state-set-lifetime-generation
                     entry (plist-get fresh-context
                                     :lifetime-generation))))
                :on-response-preflight
                (lambda (payload)
                  (when (and (active-entry-p)
                             (not (plist-get entry :cancelled)))
                    (e-harness-context-lifetime-preflight-response
                     harness session-id turn-id entry payload)))
                :on-response-complete
                (lambda (payload)
                  (when (and (active-entry-p)
                             (not (plist-get entry :cancelled)))
                    (let ((before
                           (e-harness-turn-state-context-frame entry))
                          (completed
                           (e-harness-context-lifetime-commit-response
                            harness session-id turn-id entry payload)))
                      ;; The context owner returns the completed immutable
                      ;; frame but does not write the active entry.  Install
                      ;; it only when the producer frame is still current;
                      ;; a tool callback may already have installed a newer
                      ;; descendant frame in this turn.
                      (when (and completed
                                 (or (null (plist-get payload :frame))
                                     (null before)
                                     (equal
                                      (e-context-lifetime-frame-id before)
                                      (e-context-lifetime-frame-id
                                       (plist-get payload :frame)))))
                        (e-harness-turn-state-set-context-frame
                         entry completed))
                      completed)))
                :on-tool-observation
                (lambda (payload)
                  (when (and (active-entry-p)
                             (not (plist-get entry :cancelled)))
                    (when-let* ((frame
                                (e-harness-context-lifetime-tool-observation-frame
                                 harness session-id turn-id entry payload)))
                      (e-harness-turn-state-set-context-frame entry frame)
                      ;; The executing turn already owns the request-local
                      ;; generation used to construct FRAME.  Keep it there;
                      ;; consulting the durable session facade after a tool
                      ;; result would attempt aggregate reconstruction for an
                      ;; async SQLite session.
                      frame)))
	                :on-tool-observation-presentation
	                (lambda (payload)
	                  (when (and (active-entry-p)
	                             (not (plist-get entry :cancelled)))
	                    (e-harness-context-lifetime-present-tool-observation
	                     payload)))
	                :context context)))
	            (resume-after-auto-compaction
	             ()
	             (if (not (e-session-async-enabled-p
	                       (e-harness-sessions harness)))
	                 (start-provider
	                  (e-harness-turn-context harness session-id turn-id))
	               (let ((work
	                      (e-harness-turn-context-start
	                       harness session-id turn-id)))
	                 (plist-put entry :context-work work)
	                 (e-work-on-settle
	                  work
	                  (lambda (settled)
	                    (when (active-entry-p)
	                      (plist-put entry :context-work nil)
	                      (let ((status (e-work-status settled)))
	                        (pcase (plist-get status :state)
	                          ('finished
	                           (unless (plist-get entry :cancelled)
	                             (start-provider (plist-get status :result))))
	                          ('failed
	                           (finish-error (plist-get status :error)))
	                          ('cancelled
	                           (unless (plist-get entry :cancelled)
	                             (finish-error
	                              (list 'e-work-cancelled
	                                    "Turn context query cancelled"))))))))))))
	            (start-auto-compaction
	             (context)
	             (condition-case err
	                 (let (compaction-settled
	                       request)
	                   (setq
	                    request
	                    (e-harness-compact-session-start
	                     harness session-id
	                     :reason 'auto
	                     :allow-active-turn t
	                     :allow-split-turn nil
	                     :exclude-entry-ids
	                     (list (plist-get entry :prompt-message-id))
	                     :turn-id turn-id
	                     :on-done
	                     (lambda (_record)
	                       (setq compaction-settled t)
	                       (when (and (active-entry-p)
	                                  (not (plist-get entry :cancelled)))
	                         (resume-after-auto-compaction)))
	                     :on-error
	                     (lambda (err)
	                       (setq compaction-settled t)
	                       (when (and (active-entry-p)
	                                  (not (plist-get entry :cancelled)))
	                         (if (eq (car err) 'e-compaction-error)
	                             (start-provider context)
	                           (finish-error err))))))
	                   (when (and (active-entry-p)
	                              (not compaction-settled)
	                              (not (plist-get entry :cancelled)))
	                     (plist-put entry :request request)))
	               (e-compaction-error
	                (when (and (active-entry-p)
	                           (not (plist-get entry :cancelled)))
	                  (start-provider context)))
	               (error
	                (finish-error err))))
	            (start-turn-after-input
	             ()
	             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
	               (plist-put entry :timer nil)
	               (if (e-session-storage-sqlite-p
	                    (e-harness-sessions harness))
	                   (let ((work
	                          (or (plist-get entry :context-work)
	                              (e-harness-turn-context-start
	                               harness session-id turn-id))))
	                     (plist-put entry :context-work work)
	                     (e-work-on-settle
	                      work
	                      (lambda (settled)
	                        (when (active-entry-p)
	                          (plist-put entry :context-work nil)
	                          (let ((status (e-work-status settled)))
	                            (pcase (plist-get status :state)
	                              ('finished
	                               (unless (plist-get entry :cancelled)
	                                 (let* ((context (plist-get status :result))
	                                        (excluded
	                                         (list
	                                          (plist-get
	                                           entry :prompt-message-id))))
	                                   (if (and
	                                        (e-harness-turn--auto-compaction-needed-p
	                                         harness session-id context)
	                                        (e-harness-turn--auto-compaction-useful-prefix-p
	                                         harness session-id excluded))
	                                       (start-auto-compaction context)
	                                     (start-provider context)))))
	                              ('failed
	                               (finish-error
	                                (plist-get status :error)))
	                              ('cancelled
	                               (unless (plist-get entry :cancelled)
	                                 (finish-error
	                                  (list 'e-work-cancelled
	                                        "Turn context query cancelled"))))))))))
	                 (let ((context (e-harness-turn-context
	                                 harness session-id turn-id))
	                       (excluded
	                        (list (plist-get entry :prompt-message-id))))
	                   (if (and
	                        (e-harness-turn--auto-compaction-needed-p
	                         harness session-id context)
	                        (e-harness-turn--auto-compaction-useful-prefix-p
	                         harness session-id excluded))
	                       (start-auto-compaction context)
	                     (start-provider context))))))
	            (start-turn
	             ()
	             (when (and (active-entry-p) (not (plist-get entry :cancelled)))
	               (if-let* ((input-work
	                          (plist-get entry :input-admission-work)))
	                   (progn
	                     (plist-put entry :input-admission-work nil)
	                     (e-work-on-settle
	                      input-work
	                      (lambda (settled)
	                        (when (active-entry-p)
	                          (let ((status (e-work-status settled)))
	                            (pcase (plist-get status :state)
	                              ('finished
	                               (unless (plist-get entry :cancelled)
	                                 (start-turn-after-input)))
	                              ('failed
	                               (finish-error (plist-get status :error)))
	                              ('cancelled
	                               (unless (plist-get entry :cancelled)
	                                 (finish-error
	                                  (list 'e-work-cancelled
	                                        "Input admission cancelled"))))))))))
	                 (start-turn-after-input)))))
	         (if (and delay (> delay 0))
	             (plist-put entry :timer (run-at-time delay nil #'start-turn))
	           (start-turn)))
	       turn-id))))

(cl-defun e-harness-attached-turn-follow-up-batch
    (harness session-id prompt &key metadata attachment-token
             attached-turn-port)
  "Synchronously prompt a follow-up from explicit batch/test code."
  (e-harness-attached-turn-submit-batch
   harness session-id prompt :metadata metadata
   :attachment-token attachment-token
   :attached-turn-port attached-turn-port))
(defun e-harness-turn--run-session-reset-hooks (harness session-id)
  "Run `:session-reset' hooks for HARNESS SESSION-ID."
  (e-hooks-run-reduce
   (e-harness-hooks harness)
   :session-reset
   nil
   (list :harness harness
         :session-id session-id)))

(defun e-harness-reset (harness session-id)
  "Clear SESSION-ID transcript state in HARNESS."
  (e-session-clear-messages (e-harness-sessions harness) session-id)
  (when (e-harness-queued-prompts harness session-id)
    (e-harness-turn-state-set-queued-prompts harness session-id nil)
    (e-harness-turn-state-emit-queue-changed harness session-id))
  (e-harness-turn--run-session-reset-hooks harness session-id)
  (e-harness-activity-emit
   harness
   (e-events-make :type 'session-reset
                  :session-id session-id
                  :turn-id nil
                  :payload nil)))
(defun e-harness-attached-turn-abort
    (harness session-id attachment-token &optional attached-turn-port)
  "Abort the active turn for SESSION-ID in HARNESS."
  (e-harness-turn--require-attached-port
   harness session-id attachment-token attached-turn-port)
  (let ((entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    (if (listp entry)
        (let ((turn-id (plist-get entry :id)))
          (when-let* ((timer (plist-get entry :timer)))
            (cancel-timer timer))
          (plist-put entry :cancelled t)
           (e-harness-turn--cancel-active-request entry)
           (when-let* ((turn-work (plist-get entry :work-handle)))
             (e-work-cancel turn-work))
          (e-harness-turn--append-cancelled-tool-result
           harness session-id turn-id entry)
          (plist-put entry :status 'cancelled)
          (e-harness-activity-emit-turn-event
           harness session-id turn-id 'turn-cancelled nil)
          (e-harness-turn--drain-pending-steering-input harness entry)
          (e-harness-turn--schedule-queue-drain harness session-id entry)
          entry)
      (signal 'e-harness-no-active-turn (list session-id)))))

(defun e-harness-wait-batch (harness session-id &optional timeout)
  "Wait for SESSION-ID's async turn in HARNESS from batch/test code.
TIMEOUT is in seconds.  Return the settled active-turn entry and clear it from
active state when it is no longer running."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-harness-wait-batch))
  (let ((deadline (and timeout (+ (float-time) timeout)))
        (entry (gethash session-id (e-harness-active-turns harness))))
    (unless entry
      (signal 'e-harness-no-active-turn (list session-id)))
    ;; Wait on the ENTRY object itself, not a fresh hash lookup each pass.
    ;; `finish-done'/`finish-error' mutate this plist in place, so its status
    ;; and result stay observable even after the queue-drain timer -- which can
    ;; fire inside `accept-process-output' below -- removes it from the hash.
    (while (and (e-harness-turn-state-active-turn-running-p entry)
                (or (not deadline) (< (float-time) deadline)))
      (accept-process-output nil 0.01))
    ;; Only clear the slot when it still holds this settled entry; a drained
    ;; queue may already have replaced it with the next turn.
    (when (and (eq (gethash session-id (e-harness-active-turns harness)) entry)
               (not (e-harness-turn-state-active-turn-running-p entry)))
      (e-harness-turn-state-remove-active-turn harness session-id entry))
    entry))

(provide 'e-harness-turn)

;;; e-harness-turn.el ends here

;;; e-harness-activity.el --- Durable activity owner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns activity event classification, bounded durable projection, subscriber
;; delivery, and persistence of the harness activity stream.  It has no turn
;; scheduler or context policy; those owners submit semantic events through
;; e-harness-activity-emit-turn-event.

;;; Code:

(require 'cl-lib)
(require 'e-harness-state)
(require 'e-events)
(require 'e-session)
(require 'e-session-async)
(require 'e-telemetry)
(require 'e-tools)
(require 'e-work)
(require 'seq)
(require 'subr-x)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(defun e-harness-activity--profile-call (event options thunk)
  "Measure activity EVENT with OPTIONS when profiling is available."
  (if (and (fboundp 'e-dev-profile-enabled-p)
           (fboundp 'e-dev-profile-measure-thunk)
           (e-dev-profile-enabled-p))
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(cl-defun e-harness-activity-subscribe (harness subscriber &key session-id)
  "Register SUBSCRIBER for core events from HARNESS.
When SESSION-ID is non-nil, SUBSCRIBER only receives events for that session."
  (let ((record (list :callback subscriber :session-id session-id)))
    (push record (e-harness-subscribers harness))
    record))

(defun e-harness-activity-unsubscribe (harness subscription)
  "Remove SUBSCRIPTION from HARNESS subscribers.
SUBSCRIPTION should be a record returned by
`e-harness-activity-subscribe'.  Removing an already-removed record is a
no-op."
  (setf (e-harness-subscribers harness)
        (delq subscription (e-harness-subscribers harness)))
  nil)

(defun e-harness-activity-emit (harness event)
  "Publish already-constructed EVENT to HARNESS activity subscribers.
Owners use this for non-turn lifecycle edges such as queue and reset events;
turn-scoped events should use `e-harness-activity-emit-turn-event'."
  (e-harness-activity--emit harness event))

(defun e-harness-activity--emit (harness event)
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

(defconst e-harness-activity--durable-activity-event-types
  '(turn-started provider-request-started provider-request-finished
    turn-retrying
    reasoning-delta reasoning-raw-delta
    tool-started tool-finished action-started action-finished action-failed
    hook-audit turn-finished token-usage
    context-frame-consumed
    turn-failed turn-cancelled turn-steered backend-empty-output
    compaction-started compaction-prepared compaction-summary-started
    compaction-finished compaction-failed)
  "Turn event types stored as durable session activity.")

(defconst e-harness-activity-durable-event-types
  e-harness-activity--durable-activity-event-types
  "Public snapshot of turn event types retained in session activity.
The list is a semantic classification contract for activity consumers; event
payload normalization and persistence remain owned by this module.")

(defconst e-harness-activity--activity-event-classes
  '((turn-started . audit)
    (provider-request-started . audit)
    (provider-request-finished . audit)
    (turn-retrying . audit)
    (reasoning-delta . presentation-log)
    (reasoning-raw-delta . presentation-log)
    (tool-started . audit)
    (tool-finished . presentation-log)
    (context-frame-consumed . audit)
    (action-started . audit)
    (action-finished . presentation-log)
    (action-failed . audit)
    (hook-audit . audit)
    (turn-finished . replay)
    (token-usage . audit)
    (turn-failed . audit)
    (turn-cancelled . audit)
    (turn-steered . audit)
    (backend-empty-output . audit)
    (compaction-started . audit)
    (compaction-prepared . audit)
    (compaction-summary-started . audit)
    (compaction-finished . replay)
    (compaction-failed . audit))
  "Persistence reason for activity event types.
Classes are `audit', `replay', `presentation-log', and `transient-progress'.")

(defconst e-harness-activity--activity-index-flush-event-types
  '(hook-audit turn-finished turn-failed turn-cancelled backend-empty-output
    compaction-finished compaction-failed)
  "Durable activity event types that should flush the session index.")

(defvar e-harness-activity-trusted-tool-details-uri nil
  "Dynamically scoped details URI produced by the tool lifecycle.
The value is available only while a completed tool event is projected.  It is
never copied into the transcript, public event payload, or durable activity
payload as provenance; the durable receipt may include the URI itself.")

(cl-defstruct (e-harness-activity--reasoning-stream
               (:constructor e-harness-activity--reasoning-stream-create))
  "Process-local fragments for one active provider request.

Fragments are retained in reverse arrival order so each stream update is O(1).
The activity owner joins and persists them once at the provider-request
boundary; raw streaming events remain transient subscriber notifications."
  provider-request-id
  summary-fragments summary-payload
  raw-fragments raw-payload)

(defun e-harness-activity--reasoning-streams (harness)
  "Return HARNESS's activity-owned reasoning stream table."
  (or (e-harness-activity-state-reasoning-streams
       (e-harness-activity-state harness))
      (setf (e-harness-activity-state-reasoning-streams
             (e-harness-activity-state harness))
            (make-hash-table :test 'equal))))

(defun e-harness-activity--reasoning-stream-key (session-id turn-id)
  "Return the activity-owned stream key for SESSION-ID and TURN-ID."
  (cons session-id turn-id))

(defun e-harness-activity--begin-reasoning-stream
    (harness session-id turn-id payload)
  "Begin one provider reasoning stream for SESSION-ID and TURN-ID.
PAYLOAD is the provider-request-started payload."
  (puthash
   (e-harness-activity--reasoning-stream-key session-id turn-id)
   (e-harness-activity--reasoning-stream-create
    :provider-request-id (plist-get payload :provider-request-id))
   (e-harness-activity--reasoning-streams harness)))

(defun e-harness-activity--record-reasoning-fragment
    (harness session-id turn-id type payload)
  "Retain one transient reasoning PAYLOAD fragment of TYPE.
The fragment is stored in reverse arrival order and is not persisted yet."
  (let* ((streams (e-harness-activity--reasoning-streams harness))
         (key (e-harness-activity--reasoning-stream-key session-id turn-id))
         (stream (or (gethash key streams)
                     (e-harness-activity--reasoning-stream-create)))
         (content (plist-get payload :content)))
    (when (and (stringp content) (not (string-empty-p content)))
      (pcase type
        ('reasoning-delta
         (push content
               (e-harness-activity--reasoning-stream-summary-fragments stream))
         (setf (e-harness-activity--reasoning-stream-summary-payload stream)
               (copy-sequence payload)))
        ('reasoning-raw-delta
         (push content
               (e-harness-activity--reasoning-stream-raw-fragments stream))
         (setf (e-harness-activity--reasoning-stream-raw-payload stream)
               (copy-sequence payload))))
      (puthash key stream streams))))

(defun e-harness-activity--combined-reasoning-payload
    (stream fragments payload)
  "Return one combined durable PAYLOAD for STREAM FRAGMENTS."
  (when fragments
    (let ((combined (copy-sequence payload)))
      (plist-put combined :content
                 (mapconcat #'identity (nreverse fragments) ""))
      (plist-put combined :content-mode 'snapshot)
      (plist-put combined :combined t)
      (when-let* ((request-id
                   (e-harness-activity--reasoning-stream-provider-request-id
                    stream)))
        (plist-put combined :provider-request-id request-id))
      combined)))


(defun e-harness-activity--durable-activity-event-p (type)
  "Return non-nil when TYPE should be stored as session activity."
  (let ((class (e-harness-activity-event-class type)))
    (and class
         (not (eq class 'transient-progress))
         (memq type e-harness-activity--durable-activity-event-types))))

(defun e-harness-activity-event-class (type)
  "Return persistence class for activity event TYPE."
  (cdr (assq type e-harness-activity--activity-event-classes)))

(defun e-harness-activity--activity-index-flush-event-p (type)
  "Return non-nil when TYPE should flush coalesced activity index writes."
  (memq type e-harness-activity--activity-index-flush-event-types))

(defun e-harness-activity--safe-activity-scalar (value)
  "Return VALUE safe for a narrow durable activity identity field."
  (cond
   ((stringp value) (e-telemetry-redact-string value))
   ((or (numberp value) (symbolp value) (null value)) value)
   (t nil)))

(defun e-harness-activity--string-byte-prefix (text max-bytes)
  "Return TEXT prefix limited to MAX-BYTES UTF-8 bytes.
This generic activity helper is used for bounded steering diagnostics; tool
result activity itself is no longer represented by a preview."
  (let ((bytes 0)
        (index 0)
        (length (length text)))
    (while (and (< index length)
                (let ((next-bytes
                       (string-bytes (substring text index (1+ index)))))
                  (when (<= (+ bytes next-bytes) max-bytes)
                    (setq bytes (+ bytes next-bytes))
                    t)))
      (setq index (1+ index)))
    (substring text 0 index)))

(defun e-harness-activity--tool-call-identity-projection (call)
  "Return only stable identity fields from tool CALL."
  (when (listp call)
    (list :id (e-harness-activity--safe-activity-scalar (plist-get call :id))
          :name (e-harness-activity--safe-activity-scalar
                 (plist-get call :name)))))

(defun e-harness-activity--tool-call-activity-projection (call)
  "Return the durable start projection of tool CALL.
Only call identity crosses this boundary; operation arguments remain in the
detached invocation-details artifact when that lifecycle is available."
  (e-harness-activity--tool-call-identity-projection call))

(defun e-harness-activity--tool-relation-activity-fields (payload)
  "Return named causal fields retained from tool activity PAYLOAD."
  (let (fields)
    (dolist (key '(:nested :parent-tool-call-id :depth))
      (when (and (listp payload) (plist-member payload key))
        (let ((value (e-harness-activity--safe-activity-scalar
                      (plist-get payload key))))
          (when value
            (setq fields (append fields (list key value)))))))
    fields))

(defun e-harness-activity--compact-tool-started-payload (payload)
  "Return a narrow redacted durable projection of tool-started PAYLOAD."
  (let* ((wrapped (and (listp payload) (plist-member payload :tool-call)))
         (call (and (listp payload)
                    (if wrapped (plist-get payload :tool-call) payload)))
         (projected (e-harness-activity--tool-call-activity-projection call))
         (relations (e-harness-activity--tool-relation-activity-fields payload)))
    (if wrapped
        (append (list :tool-call projected) relations)
      (append projected relations))))

(defun e-harness-activity--tool-receipt-activity-projection
    (call result details-uri)
  "Return the compact durable receipt for top-level CALL and RESULT.
DETAILS-URI must be supplied by the successful lifecycle archival stage; a
URI copied into RESULT metadata is not sufficient to authorize a receipt."
  (let* ((id (or (plist-get call :id)
                 (plist-get result :tool-call-id)))
         (name (or (plist-get call :name)
                   (plist-get result :name)))
         (receipt
          (list :tool-call-id (e-harness-activity--safe-activity-scalar id)
                :tool (e-harness-activity--safe-activity-scalar name)
                :status (e-harness-activity--safe-activity-scalar
                         (plist-get result :status)))))
    (when (and (stringp details-uri)
               (not (string-empty-p details-uri)))
      (setq receipt
            (append receipt
                    (list :details-uri
                          (e-harness-activity--safe-activity-scalar details-uri)))))
    (append receipt (list :details-lifetime 'session-tmp))))

(defun e-harness-activity--compact-tool-finished-payload (payload)
  "Return the compact durable projection of tool-finished PAYLOAD.
Nested host-authored calls retain causal identity only.  A top-level finished
call records one receipt whose content and operation arguments live in the
detached invocation-details artifact."
  (let* ((call (and (listp payload) (plist-get payload :tool-call)))
         (result (and (listp payload) (plist-get payload :result)))
         (projected
          (append
           (list :tool-call (e-harness-activity--tool-call-identity-projection call))
           (e-harness-activity--tool-relation-activity-fields payload))))
    (when (and (not (plist-get payload :nested))
               (e-tools-result-for-call-p result call)
               (stringp e-harness-activity-trusted-tool-details-uri)
               (not (string-empty-p e-harness-activity-trusted-tool-details-uri)))
      (setq projected
            (append projected
                    (list :receipt
                          (e-harness-activity--tool-receipt-activity-projection
                           call result
                           e-harness-activity-trusted-tool-details-uri)))))
    projected))

(defun e-harness-activity--activity-field (payload key predicate &optional transform)
  "Return KEY and its PAYLOAD value when PREDICATE accepts the value.
Apply TRANSFORM when supplied."
  (let ((value (and (listp payload) (plist-get payload key))))
    (when (funcall predicate value)
      (list key (if transform (funcall transform value) value)))))

(defun e-harness-activity--safe-activity-string (value)
  "Return redacted VALUE when it is a string."
  (and (stringp value) (e-telemetry-redact-string value)))

(defun e-harness-activity--safe-error-activity-string (value)
  "Return redacted, bounded error string VALUE."
  (when (stringp value)
    (truncate-string-to-width
     (e-telemetry-redact-string value)
     e-telemetry-preview-max-bytes nil nil "...")))

(defun e-harness-activity--safe-error-activity-details (value)
  "Return redacted error details VALUE, bounding unusually large values.
Ordinary compact provider plists retain their useful structure.  A large or
cyclic value becomes an explicit bounded telemetry preview instead of making a
board activity message unpublishable."
  (let ((preview (e-telemetry-preview value)))
    (if (plist-get preview :truncated)
        preview
      (e-telemetry-redact-value value))))

(defun e-harness-activity--tool-cause-activity-projection (cause)
  "Return a narrow redacted durable projection of tool CAUSE."
  (when (listp cause)
    (append
     (e-harness-activity--activity-field
      cause :id #'stringp #'e-harness-activity--safe-activity-string)
     (e-harness-activity--activity-field
      cause :name #'stringp #'e-harness-activity--safe-activity-string))))

(defun e-harness-activity--tool-causes-activity-projection (causes)
  "Return a vector of narrow durable tool CAUSES."
  (when (or (listp causes) (vectorp causes))
    (vconcat (delq nil
                   (mapcar #'e-harness-activity--tool-cause-activity-projection
                           (append causes nil))))))

(defun e-harness-activity--request-cause-activity-fields (payload)
  "Return narrow causal fields retained from provider PAYLOAD."
  (let (fields)
    (dolist (key '(:caused-by-tool-call-id :caused-by-tool-name))
      (setq fields
            (append fields
                    (e-harness-activity--activity-field
                     payload key #'stringp #'e-harness-activity--safe-activity-string))))
    (when-let ((causes
                (e-harness-activity--tool-causes-activity-projection
                 (plist-get payload :caused-by-tool-calls))))
      (setq fields (append fields (list :caused-by-tool-calls causes))))
    fields))

(defun e-harness-activity--provider-diagnostics-activity-projection (diagnostics)
  "Return named scalar provider DIAGNOSTICS safe for durable activity."
  (let (projected)
    (dolist (key '(:model :reasoning-effort :reasoning-summary :effort :response-store
                   :prompt-cache-key-present :prompt-cache-retention-present
                   :prompt-cache-mode :prompt-layout-revision
                   :provider-continuation :previous-response-id-present
                   :provider-anchor-present :input-message-count :tool-count
                   :observation-delivery :replaceable-current-state-present
                   :current-state-fingerprint :context-rendering-strategy
                   :provider-anchor-safety
                   :responses-transport :max-tokens :prompt-cache
                   :websocket-connection-id :websocket-reused
                   :websocket-reuse-count :websocket-request-mode
                   :websocket-idle-close-seconds
                   :anthropic-cache-mode :anthropic-cache-breakpoint
                   :anthropic-cache-ttl :anthropic-container-id-present))
      (when (and (listp diagnostics) (plist-member diagnostics key))
        (let ((value (e-harness-activity--safe-activity-scalar
                      (plist-get diagnostics key))))
          (when (or value (null (plist-get diagnostics key)))
            (setq projected (append projected (list key value)))))))
    projected))

(defun e-harness-activity--provider-request-activity-projection (payload)
  "Return a narrow redacted durable provider lifecycle PAYLOAD."
  (let (projected)
    (dolist (key '(:provider-request-id :provider :transport :url-host
                   :url-path :status))
      (when (and (listp payload) (plist-member payload key))
        (let ((value (e-harness-activity--safe-activity-scalar (plist-get payload key))))
          (when value
            (setq projected (append projected (list key value)))))))
    (dolist (key '(:provider-request-ordinal :timeout-seconds :deadline
                   :elapsed-seconds))
      (setq projected
            (append projected
                    (e-harness-activity--activity-field payload key #'numberp))))
    (when-let ((diagnostics
                (e-harness-activity--provider-diagnostics-activity-projection
                 (plist-get payload :diagnostics))))
      (setq projected (append projected (list :diagnostics diagnostics))))
    (append projected (e-harness-activity--request-cause-activity-fields payload))))

(defun e-harness-activity--retry-activity-projection (payload)
  "Return redacted durable retry lifecycle PAYLOAD.
The retry error is durable diagnostic evidence.  Keep its structured details
while redacting credentials, and retain only the retry scheduler's scalar
fields outside that error contract."
  (let (projected)
    (setq projected
          (append projected
                  (e-harness-activity--activity-field
                   payload :error #'stringp
                   #'e-harness-activity--safe-error-activity-string)))
    (when (and (listp payload) (plist-member payload :details))
      (setq projected
            (append projected
                    (list :details
                          (e-harness-activity--safe-error-activity-details
                           (plist-get payload :details))))))
    (dolist (key '(:attempt :backoff-seconds :reset-wait))
      (setq projected
            (append projected
                    (e-harness-activity--activity-field payload key #'numberp))))
    projected))

(defun e-harness-activity--token-usage-activity-projection (payload)
  "Return a narrow durable projection of token-usage PAYLOAD."
  (let (projected)
    (dolist (key '(:input-tokens :cached-input-tokens
                   :cache-creation-input-tokens :output-tokens
                   :reasoning-output-tokens :total-tokens
                   :provider-request-ordinal))
      (setq projected
            (append projected
                    (e-harness-activity--activity-field payload key #'numberp))))
    (setq projected
          (append projected
                  (e-harness-activity--activity-field
                   payload :provider-request-id #'stringp
                   #'e-harness-activity--safe-activity-string)))
    (append projected (e-harness-activity--request-cause-activity-fields payload))))

(defun e-harness-activity-payload (type payload)
  "Return narrow durable activity PAYLOAD for event TYPE."
  (pcase type
    ('tool-started (e-harness-activity--compact-tool-started-payload payload))
    ('tool-finished (e-harness-activity--compact-tool-finished-payload payload))
    ((or 'provider-request-started 'provider-request-finished)
     (e-harness-activity--provider-request-activity-projection payload))
    ('turn-retrying (e-harness-activity--retry-activity-projection payload))
    ('token-usage (e-harness-activity--token-usage-activity-projection payload))
    (_ payload)))

(defun e-harness-activity--append-durable-activity-event
    (harness session-id turn-id type payload)
  "Append durable activity TYPE for HARNESS SESSION-ID TURN-ID."
  (e-harness-activity--profile-call
   'harness.activity-append
   (list :session-id session-id
         :turn-id turn-id
         :metadata (list :event-type (and type (symbol-name type))))
   (lambda ()
     (let* ((store (e-harness-sessions harness))
            (flush-index-p
             (e-harness-activity--activity-index-flush-event-p type))
            (async-p (e-session-async-enabled-p store))
            (durable-payload
             (e-harness-activity-payload type payload))
            (checkpoint-retain
             (and (eq type 'tool-finished)
                  (plist-member durable-payload :receipt)))
            (event (e-session-append-activity-event
                    store
                    session-id
                    turn-id
                    type
                    durable-payload
                    ;; Async completion marks the derived projection dirty
                    ;; without publishing it on this interactive callback.
                    :write-index (and async-p flush-index-p)
                    :checkpoint-retain checkpoint-retain)))
       (when (and flush-index-p (not async-p))
         (e-session-refresh-index store))
       ;; Async admission returns only the durability work.  Do not query or
       ;; reconstruct session state on this interactive callback merely to
       ;; recover the eventual journal identity.  The public live event is
       ;; already identified by its turn/type payload, and the Board adapter
       ;; owns a bounded attachment-local publication sequence until a later
       ;; detached query observes SQLite's durable identity.
       (if (not (e-work-handle-p event))
           event
         ;; Durable activity remains enqueue-and-return, but the executing
         ;; turn owns its bounded in-flight writes until they settle.  This
         ;; lets cancellation and explicit batch boundaries observe the work
         ;; without turning the activity stream into a synchronous barrier.
         (when-let* ((entry (gethash session-id
                                     (e-harness-active-turns harness))))
           (push event (plist-get entry :persistence-works))
           (e-work-on-settle
            event
            (lambda (work)
              (plist-put entry :persistence-works
                         (delq work (plist-get entry :persistence-works))))))
         nil)))))

(defun e-harness-activity--flush-reasoning-stream
    (harness session-id turn-id)
  "Persist at most one combined reasoning entry per stream for this request.
Return public event descriptors in summary/raw order.

Persistence may return either an appended entry or an asynchronous work
handle.  The harness-owned TYPE and PAYLOAD remain authoritative for immediate
publication; persisted provenance is attached only when it is already
available."
  (let* ((streams (e-harness-activity--reasoning-streams harness))
         (key (e-harness-activity--reasoning-stream-key session-id turn-id))
         (stream (gethash key streams))
         appended)
    (when stream
      (remhash key streams)
      (dolist
          (spec
           (list
            (list 'reasoning-delta
                  (e-harness-activity--reasoning-stream-summary-fragments stream)
                  (e-harness-activity--reasoning-stream-summary-payload stream))
            (list 'reasoning-raw-delta
                  (e-harness-activity--reasoning-stream-raw-fragments stream)
                  (e-harness-activity--reasoning-stream-raw-payload stream))))
        (when-let* ((payload
                     (e-harness-activity--combined-reasoning-payload
                      stream (nth 1 spec) (nth 2 spec))))
          (let ((activity-entry
                 (e-harness-activity--append-durable-activity-event
                  harness session-id turn-id (car spec) payload)))
            (setq appended
                  (append
                   appended
                   (list
                    (list :event-type (car spec)
                          :payload payload
                          :activity-entry-id (plist-get activity-entry :id)
                          :board-activity-sequence
                          (plist-get activity-entry
                                     :board-activity-sequence)))))))))
    appended))

(defun e-harness-activity--flush-and-emit-reasoning-stream
    (harness session-id turn-id)
  "Persist and publish the one combined reasoning entry for the active stream."
  (dolist (entry
           (e-harness-activity--flush-reasoning-stream
            harness session-id turn-id))
    (e-harness-activity--emit
     harness
     (e-events-make
      :type (plist-get entry :event-type)
      :session-id session-id
      :turn-id turn-id
      :payload (plist-get entry :payload)
      :activity-entry-id (plist-get entry :activity-entry-id)
      :board-activity-sequence
      (plist-get entry :board-activity-sequence)))))

(defun e-harness-activity-emit-turn-event (harness session-id turn-id type payload)
  "Emit public event TYPE with PAYLOAD for HARNESS SESSION-ID TURN-ID."
  (when (and session-id turn-id (eq type 'token-usage))
    ;; Provider usage belongs to the executing request, not to a reconstructed
    ;; session aggregate.  Retain one bounded value so status presentation can
    ;; use it without an interactive SQLite read; durability remains below.
    (when-let* ((entry (gethash session-id
                                (e-harness-active-turns harness)))
                (state (plist-get entry :session-query-state)))
      (plist-put state :latest-token-usage-event
                 (list :turn-id turn-id :event-type type
                       :payload (copy-tree payload t)))
      (plist-put entry :session-query-state state)))
  (when (and session-id turn-id (eq type 'provider-request-started))
    ;; A replacement request is an ordinary lifecycle boundary.  Preserve any
    ;; completed fragments from the preceding request before installing the
    ;; new O(1) accumulator.
    (e-harness-activity--flush-and-emit-reasoning-stream
     harness session-id turn-id)
    (e-harness-activity--begin-reasoning-stream
     harness session-id turn-id payload))
  (when (and session-id
             turn-id
             (memq type '(reasoning-delta reasoning-raw-delta)))
    (e-harness-activity--record-reasoning-fragment
     harness session-id turn-id type payload))
  (when (and session-id
             turn-id
             (memq type '(provider-request-finished
                          turn-finished turn-failed turn-cancelled)))
    (e-harness-activity--flush-and-emit-reasoning-stream
     harness session-id turn-id))
  (let* ((store (e-harness-sessions harness))
         (activity-entry
          (when (and session-id
                     turn-id
                     (not (memq type '(reasoning-delta reasoning-raw-delta)))
                     (e-harness-activity--durable-activity-event-p type)
                     ;; SQLite-authoritative sessions accept the activity
                     ;; behind any earlier owner mutation.  Synchronous test
                     ;; and legacy stores retain their existing absent-session
                     ;; behavior without making async publication perform a
                     ;; session aggregate read.
                     (or (e-session-async-enabled-p store)
                         (ignore-errors (e-session-local-state store session-id))))
            (e-harness-activity--append-durable-activity-event
             harness session-id turn-id type payload)))
         (event
          (e-events-make :type type
                         :session-id session-id
                         :turn-id turn-id
                         :payload payload
                         :activity-entry-id (plist-get activity-entry :id)
                         :board-activity-sequence
                         (plist-get activity-entry :board-activity-sequence))))
    (e-harness-activity--emit harness event)
    ;; The request-local event is the publication result.  Async persistence
    ;; may not have a journal identity yet, and callers must not reread the
    ;; session aggregate merely to manufacture one.
    event))

(provide 'e-harness-activity)

;;; e-harness-activity.el ends here

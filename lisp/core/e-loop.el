;;; e-loop.el --- Agent turn loop for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral synchronous turn loop for core runtime tests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'e-backend)
(require 'e-context-lifetime)
(require 'e-request)
(require 'e-session)
(require 'e-tools)
(require 'e-work)

(declare-function e-dev-profile-enabled-p "e-dev-profile")
(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-loop-backend-error "Backend returned an error")
(define-error 'e-loop-empty-output "Backend returned no assistant output")
(define-error 'e-loop-repeated-tool-failure
  "The model repeated an identical failing tool call")

(defconst e-loop-tool-failure-diagnostic-byte-limit 512
  "Maximum normalized tool error bytes retained by the turn loop.")

(defun e-loop--tool-failure-fingerprint (tool-call result)
  "Return a consecutive-failure fingerprint for TOOL-CALL and RESULT.
The diagnostic and argument digest are bounded.  TOOL-CALL must carry the
schema-normalized arguments used for execution, or the received arguments when
schema validation rejected the call."
  (when (eq (plist-get result :status) 'error)
    (let* ((preview
            (e-tools-result-content-preview
             (plist-get result :content)
             e-loop-tool-failure-diagnostic-byte-limit
             16 3))
           (text (string-trim
                  (replace-regexp-in-string
                   "[[:space:]]+" " " (plist-get preview :text))))
           (arguments-digest
            (e-tools-arguments-fingerprint
             (list :present (and (plist-member tool-call :arguments) t)
                   :value (plist-get tool-call :arguments)))))
      (list :tool (plist-get tool-call :name)
            :arguments-digest arguments-digest
            :error text))))

(defun e-loop--profile-enabled-p ()
  "Return non-nil when developer profiling is available and enabled."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-loop--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when dev profiling is enabled."
  (if (e-loop--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-loop--assistant-message
    (content &optional metadata response-entry-id phase)
  "Return an assistant message with CONTENT, METADATA, ID, and PHASE.

RESPONSE-ENTRY-ID is allocated before completion preflight so that a pure
preflight and the subsequent durable append share one identity.  PHASE is the
optional Responses assistant phase; older providers leave it absent."
  (let ((message (list :role 'assistant
                       :content content
                       :metadata metadata)))
    (when phase
      (plist-put message :phase phase))
    (when response-entry-id
      (plist-put message :id response-entry-id))
    message))

(defun e-loop--without-provider-replay-items (messages)
  "Return MESSAGES without one-shot provider replay metadata."
  (mapcar
   (lambda (message)
     (let ((copy (copy-tree message)))
       (dolist (slot '(content metadata))
         (let ((value (plist-get copy slot)))
           (when (and (listp value)
                      (plist-member value :provider-replay-items))
             (setq value (copy-sequence value))
             (cl-remf value :provider-replay-items)
             (setq copy (plist-put copy slot value)))))
       copy))
   messages))

(cl-defun e-loop--emit (&key on-event type payload)
  "Report internal turn descriptor TYPE and PAYLOAD through ON-EVENT."
  (funcall on-event type payload))

(defun e-loop--diagnostic-scalar-p (value)
  "Return non-nil when VALUE is safe for lifecycle diagnostics."
  (or (null value)
      (stringp value)
      (numberp value)
      (symbolp value)))

(defun e-loop--sanitize-diagnostics (diagnostics)
  "Return scalar-only DIAGNOSTICS as a plist."
  (when (listp diagnostics)
    (let ((rest diagnostics)
          sanitized)
      (while (and (consp rest) (consp (cdr rest)))
        (let ((key (car rest))
              (value (cadr rest)))
          (when (and (keywordp key)
                     (e-loop--diagnostic-scalar-p value))
            (setq sanitized (append sanitized (list key value)))))
        (setq rest (cddr rest)))
      sanitized)))

(defun e-loop--normalized-backend-item (item)
  "Return backend-neutral ITEM with legacy provider spellings normalized."
  (let ((type (plist-get item :type)))
    (cond
     ((member type '(tool_call "tool_call" "tool-call"))
      (plist-put (copy-sequence item) :type 'tool-call))
     (t item))))

(defun e-loop--retire-frame-curation-markers (messages options frame)
  "Retire FRAME's model-facing source markers from request projections.

Return a plist containing detached `:messages' and `:options'.  The frame's
ordinary source values remain under their existing semantic owners; only the
frame-local labels that became invalid when FRAME was consumed are removed.
Every option-side message projection is updated together so a stateless
adapter cannot reconstruct stale labels from semantic segments or a delta."
  (let* ((marker-contents
          (mapcar (lambda (source)
                    (plist-get source :marker))
                  (e-context-lifetime-frame-curation-presentation frame)))
         (options (copy-sequence options)))
    (cl-labels
        ((without-markers
          (projection)
          (if (listp projection)
              ;; A tool result can establish a descendant frame before the
              ;; producer response commits its curation.  Its markers may be
              ;; textually identical to the producer's, so retire only one
              ;; earlier occurrence per source in each projection.
              (let ((remaining (copy-sequence marker-contents)))
                (cl-remove-if
                 (lambda (message)
                   (let ((content (plist-get message :content)))
                     (when (and (eq (plist-get message :role) 'system)
                                (member content remaining))
                       (setq remaining
                             (cl-delete content remaining
                                        :count 1 :test #'equal))
                       t)))
                 (copy-tree projection)))
            projection)))
      (let ((messages (without-markers messages)))
        (when-let* ((segments (plist-get options :segments)))
          (setq segments
                (mapcar
                 (lambda (segment)
                   (let ((copy (copy-sequence segment)))
                     (plist-put copy :messages
                                (without-markers
                                 (plist-get segment :messages)))
                     copy))
                 segments))
          (setq options (plist-put options :segments segments))
          (when (plist-member options :context-segment-message-count)
            (setq options
                  (plist-put
                   options :context-segment-message-count
                   (cl-loop for segment in segments
                            sum (length (plist-get segment :messages)))))))
        (dolist (key '(:replaceable-current-state
                       :provider-anchor-delta-messages
                       :provider-compaction-delta-messages))
          (when (plist-member options key)
            (setq options
                  (plist-put options key
                             (without-markers (plist-get options key))))))
        (list :messages messages :options options)))))

(defun e-loop--continuation-candidate-p (options candidate &optional immediate-only-p)
  "Return non-nil when CANDIDATE may continue the request in OPTIONS."
  (let* ((continuation
          (plist-get (plist-get options :context-capabilities)
                     :continuation))
         (frontier (plist-get options :observation-frontier))
         (inherited-observation-p
          (or (and (eq (plist-get options :observation-delivery) 'inherited)
                   (plist-get options :current-state-fingerprint))
              (cl-some
               (lambda (observation)
                 (eq (plist-get observation :delivery) 'inherited))
               (plist-get frontier :observations)))))
    (and (plist-get options :provider-continuation)
       (memq continuation '(linear branchable))
       (eq (plist-get candidate :provider-id)
           (plist-get options :provider-anchor-provider-id))
       ;; An inherited current-state observation is already part of the
       ;; provider's causal response.  Its response id is therefore not a
       ;; clean anchor for the next request.  A proven request-local
       ;; replacement may advance normally; a turn with no observation may
       ;; also retain its ordinary continuation candidate.
       (or immediate-only-p
           (and
            ;; A scalar inherited marker is the legacy/synthetic form of the
            ;; same semantic frontier.  It must be unsafe even when the newer
            ;; frame-derived cleanliness field is absent.
            (not inherited-observation-p)
            ;; A request-local canvas replacement is not sufficient when the
            ;; same frontier also contains an inherited tool result or another
            ;; ephemeral kind.  The harness computes this flag from every
            ;; trusted frame observation; require the whole frontier to be
            ;; clean before a candidate can become durable.
            (or (not (plist-member options :lifetime-ephemerals-clean-p))
                (plist-get options :lifetime-ephemerals-clean-p)))))))

(defun e-loop--promote-continuation-candidate
    (options candidate source-message-count delta-messages &optional immediate-only-p)
  "Return OPTIONS advanced to CANDIDATE for an in-turn follow-up.
SOURCE-MESSAGE-COUNT covers the local transcript through the completed
provider response.  DELTA-MESSAGES are new client inputs, normally tool
results, that the stored response does not contain."
  (if (not (e-loop--continuation-candidate-p
            options candidate immediate-only-p))
      options
    (let ((advanced (copy-sequence options)))
      (setq advanced
            (plist-put advanced
                       :provider-anchor
                       (list :provider-id (plist-get candidate :provider-id)
                             :metadata (copy-tree
                                        (plist-get candidate :metadata)))))
      (setq advanced
            (plist-put advanced
                       :provider-anchor-delta-messages
                       (copy-tree delta-messages)))
      (plist-put advanced
                 :provider-anchor-source-message-count
                 source-message-count))))

(defun e-loop--accepted-continuation-candidate
    (candidate projection-identity request-id request-ordinal
                &optional immediate-only-p)
  "Return CANDIDATE marked as accepted by the current loop request.

Acceptance is deliberately recorded at the promotion boundary rather than
when a backend item first arrives.  A candidate that arrives before a tool
refresh may be useful for an immediate follow-up, but it is not automatically
the durable owner of the eventual turn.  REQUEST-ID and REQUEST-ORDINAL let
the harness select only the candidate belonging to the final successful
provider request; PROJECTION-IDENTITY lets it verify that the candidate was
produced by the authoritative semantic projection."
  (let ((accepted (copy-tree candidate)))
    (plist-put accepted :accepted-for-persistence (not immediate-only-p))
    (plist-put accepted :immediate-followup-only immediate-only-p)
    (plist-put accepted :projection-identity
               (copy-tree projection-identity))
    (plist-put accepted :provider-request-id request-id)
    (plist-put accepted :provider-request-ordinal request-ordinal)
    accepted))

(defun e-loop--clear-provider-compaction-request-state (options)
  "Return OPTIONS without one-shot provider compaction request state.

Provider compaction output is valid for the request that selected it only.
After that request completes, ordinary tool-result or steering follow-ups must
use the immediate response anchor/delta or normal stateless messages rather
than replaying the opaque output.  The rendering marker is cleared only when
it is the marker installed by the compaction projection."
  (let ((options (copy-sequence options)))
    (dolist (key '(:provider-compaction-output
                   :provider-compaction-delta-messages
                   :provider-compaction-source-entry-id
                   :provider-compaction-generation-id
                   :provider-compaction-fingerprint
                   :provider-compaction-invalidation-reason))
      (cl-remf options key))
    (when (eq (plist-get options :context-rendering-strategy)
              'opaque-provider-compaction)
      (cl-remf options :context-rendering-strategy))
    options))

(defun e-loop--continuation-projection-compatible-p (request-identity options)
  "Return non-nil when OPTIONS still describes REQUEST-IDENTITY.

The harness supplies this provider-neutral identity from the semantic request
projection.  A missing identity is intentionally incompatible after a refresh:
the loop cannot prove that a response candidate remains valid for an unlabelled
projection, so it keeps the freshly rebuilt anchor decision instead of
advancing it with stale provider state."
  (and request-identity
       (equal request-identity
              (plist-get options :continuation-projection-identity))))

(defun e-loop--request-cause-fields (causes)
  "Return stable lifecycle fields for completed tool-call CAUSES."
  (when causes
    (let* ((last-cause (car (last causes)))
           (records
            (mapcar (lambda (cause)
                      (list :id (plist-get cause :id)
                            :name (plist-get cause :name)))
                    causes)))
      (list :caused-by-tool-call-id (plist-get last-cause :id)
            :caused-by-tool-name (plist-get last-cause :name)
            :caused-by-tool-calls (vconcat records)))))

(defun e-loop--request-lifecycle-payload
    (request status request-id request-ordinal &optional started-at causes)
  "Return sanitized lifecycle payload for REQUEST with stable identity.
REQUEST-ID and REQUEST-ORDINAL join all events for one provider request.
STARTED-AT is the `float-time' value captured when the request was published.
CAUSES lists every completed tool call that induced a follow-up request."
  (let* ((metadata (and (e-backend-request-p request)
                        (e-backend-request-metadata request)))
         (payload (list :provider-request-id request-id
                        :provider-request-ordinal request-ordinal
                        :provider (plist-get metadata :provider)
                        :transport (plist-get metadata :transport)
                        :url-host (plist-get metadata :url-host)
                        :url-path (plist-get metadata :url-path)
                        :timeout-seconds (plist-get metadata
                                                     :timeout-seconds)
                        :deadline (plist-get metadata :deadline)
                        :status status)))
    (when causes
      (setq payload
            (append payload (e-loop--request-cause-fields causes))))
    (when-let* ((diagnostics
                (e-loop--sanitize-diagnostics
                 (plist-get metadata :diagnostics))))
      (setq payload (append payload (list :diagnostics diagnostics))))
    (when started-at
      (plist-put payload
                 :elapsed-seconds
                 (/ (float (round (* 1000 (max 0.0
                                                (- (float-time)
                                                   started-at)))))
                    1000)))
    payload))

(defun e-loop--backend-work-request (handle)
  "Return an `e-backend-request' projection for backend work HANDLE."
  (let* ((metadata (and (e-work-handle-p handle)
                        (e-work-handle-metadata handle)))
         (provider-metadata
          (copy-sequence
           (or (plist-get metadata :backend-request-metadata) nil))))
    (e-backend-request-create
     :cancel (lambda ()
               (when (e-work-handle-p handle)
                 (e-work-cancel handle)))
     :metadata
     (append provider-metadata
             (list :work-id (and (e-work-handle-p handle)
                                 (e-work-handle-id handle))
                   :work-handle handle
                   :work-transport (plist-get metadata :transport)
                   :deadline (plist-get metadata :deadline)
                   :backend-request
                   (plist-get metadata :backend-request))))))

(cl-defun e-loop-start-turn
    (&key session-id turn-id messages backend tools tool-lifecycle options on-event
            append-message refresh-context refresh-messages on-request-start
            on-done on-error callback-dispatcher
            cancelled-p drain-pending-input segments turn-work-handle
            board-enroll-work lifetime-frame on-response-preflight
            on-response-complete
            on-tool-observation on-tool-observation-presentation
            on-tool-call-start)
  "Start one async agent turn for SESSION-ID and TURN-ID.
MESSAGES, BACKEND, TOOLS, TOOL-LIFECYCLE, and OPTIONS describe the turn input.
ON-EVENT, APPEND-MESSAGE, REFRESH-CONTEXT, REFRESH-MESSAGES, ON-REQUEST-START,
ON-DONE, ON-ERROR, CANCELLED-P, and DRAIN-PENDING-INPUT receive turn progress,
output, refreshed context, provider request handles, settlement, failures,
cancellation state, and same-turn pending user input.  REFRESH-CONTEXT, when
supplied, must return one atomic context projection containing at least
`:messages' and `:options'; its `:segments' and observation metadata must be
consistent with those options.  REFRESH-MESSAGES is retained as a legacy
messages-only callback.  The provider request is started through
`e-backend-start'.  Tool execution is started through TOOL-LIFECYCLE when
supplied, otherwise through `e-tools-start'.  Provider I/O, tool I/O, and turn
settlement are callback-driven.  ON-RESPONSE-PREFLIGHT, when supplied, runs
before a non-tool assistant message is appended and returns a pure prepared
completion value for ON-RESPONSE-COMPLETE.  ON-TOOL-OBSERVATION-PRESENTATION,
when supplied, receives the fresh frame and both in-memory provider message
projections after a tool result is observed; it returns those projections with
the frame-local presentation installed.  ON-TOOL-CALL-START receives the
bounded transcript call, plus an optional detached archival call and rejection
metadata, before tool execution begins.  CALLBACK-DISPATCHER, when supplied,
receives one already-admitted asynchronous callback and either runs it now or
schedules it behind the owning session's active commit barrier."
  (let ((turn-messages (copy-sequence messages))
        ;; Session identity is runtime request context, not provider input.  It
        ;; lets stateful backend adapters isolate connection/request ownership
        ;; even when callers do not redundantly persist it in turn options.
        (turn-options
         (let ((options (plist-put (copy-sequence options)
                                   :session-id session-id)))
           ;; Segment metadata is derived model context.  Adapters need it to
           ;; translate the stable/dynamic boundary into provider cache
           ;; controls, but it is not durable session configuration.
           (if segments
               (plist-put options :segments (copy-tree segments))
             options)))
        (settled nil)
        (active-request nil)
        (provider-request-sequence 0)
        (next-request-causes nil)
        ;; A closed frame opportunity gets one corrective provider continuation.
        ;; This is turn-local protocol state, not durable response history.
        (curation-duplicate-correction-used-p nil)
        ;; Retain exactly the just-curated request frame until the turn ends or
        ;; a later curation replaces it.  The consumed runtime frame drops its
        ;; bodies, but duplicate validation must preserve every ordinary
        ;; frame-bound check before the semantic effect is ignored.
        (last-curated-lifetime-frame nil)
        ;; A curation-only response has no semantic message to carry its opaque
        ;; provider acknowledgement.  Hold that wire state only until the next
        ;; request captures it; each request receives its own options snapshot.
        (next-request-provider-replay-items nil)
        ;; Buffered ordinary calls accompany a rejected or duplicate curation
        ;; only as request-local truthful results; they never enter session state.
        (next-request-provider-skipped-calls nil)
        ;; Only the immediately preceding model-facing tool failure matters.
        ;; This is live turn coordination and is discarded at settlement.
        (previous-tool-failure nil)
        (active-lifetime-frame lifetime-frame))
    (cl-labels
        ((dispatch-callback
          (callback)
          (if callback-dispatcher
              (funcall callback-dispatcher callback)
            (funcall callback)))
         (cancelled ()
           (and cancelled-p (funcall cancelled-p)))
         (fail
          (err)
          (unless settled
            (setq settled t)
            (when on-error
              (funcall on-error err))))
         (finish
          (done-reason assistant-content)
          (unless settled
            (setq settled t)
            (e-loop--emit :on-event on-event
                          :type 'turn-finished
                          :payload (list :reason done-reason))
            (when on-done
              (funcall on-done
                       (list :status 'done
                             :reason done-reason
                             :assistant-content assistant-content)))))
         (publish-request
          (request)
          (setq active-request request)
          (when on-request-start
            (funcall on-request-start request)))
         (lifetime-projection-enabled-p
          ()
          ;; Callbacks are installed by the harness for both legacy and
          ;; opted-in turns.  The semantic option on the current request is
          ;; the authority for the new context-lifetime projection behavior.
          (plist-get turn-options :context-lifetime-enabled))
         (drain-pending
          (&optional refresh-before-pending-p)
          (let ((pending (and drain-pending-input
                              (funcall drain-pending-input))))
            (when pending
              ;; A same-turn steering message after an immediate tool
              ;; follow-up must start from the harness's newly committed
              ;; projection.  The refresh callback is invoked only when there
              ;; is actually pending input, so ordinary turns keep their
              ;; existing transcript path.
              (when (and refresh-before-pending-p
                         refresh-context
                         (lifetime-projection-enabled-p))
                (apply-context-refresh (funcall refresh-context)))
              (dolist (message pending)
                (setq turn-messages (append turn-messages (list message)))
                (funcall append-message message))
              t)))
         (apply-context-refresh
          (projection)
          ;; Build the replacement values before mutating the loop state.  A
          ;; context refresh is a request projection, not a messages-only
          ;; convenience: the next request must see the same options,
          ;; segments, frontier, and anchor decision as the refreshed
          ;; messages.
          (unless (and (listp projection)
                       (plist-member projection :messages)
                       (plist-member projection :options))
            (signal 'wrong-type-argument
                    (list 'e-loop-context-projection projection)))
          (let* ((new-messages (copy-tree (plist-get projection :messages)))
                 (new-options (copy-sequence (plist-get projection :options)))
                 (new-segments
                  (if (plist-member projection :segments)
                      (copy-tree (plist-get projection :segments))
                    (plist-get new-options :segments)))
                 (new-frontier
                  (and (plist-member projection :observation-frontier)
                       (copy-tree
                        (plist-get projection :observation-frontier))))
                 (new-options
                  (plist-put new-options :session-id session-id)))
            (when (or (plist-member projection :segments)
                      (plist-member new-options :segments))
              (setq new-options
                    (plist-put new-options :segments new-segments)))
            (when (plist-member projection :observation-frontier)
              (setq new-options
                    (plist-put new-options :observation-frontier
                               new-frontier)))
            (setq turn-messages new-messages
                  turn-options new-options)
            (when (plist-member projection :lifetime-frame)
              (setq active-lifetime-frame
                    (plist-get projection :lifetime-frame))))
         )
         (clear-provider-compaction-request-state
          ()
          (when (or (plist-member turn-options :provider-compaction-output)
                    (plist-member turn-options
                                   :provider-compaction-delta-messages)
                    (plist-member turn-options
                                   :provider-compaction-source-entry-id)
                    (plist-member turn-options
                                   :provider-compaction-generation-id)
                    (plist-member turn-options
                                   :provider-compaction-fingerprint)
                    (plist-member turn-options
                                   :provider-compaction-invalidation-reason)
                    (eq (plist-get turn-options
                                   :context-rendering-strategy)
                        'opaque-provider-compaction))
            (setq turn-options
                  (e-loop--clear-provider-compaction-request-state
                   turn-options))))
         (start-request
          (&optional immediate-curation-followup-p)
          (unless (or settled (cancelled))
            ;; Drain queued steering after the immediate curation follow-up so
            ;; it can use a projection rebuilt past the consumed source.
            (unless immediate-curation-followup-p
              (drain-pending))
            (let ((request-messages
                   (let ((snapshot (copy-tree turn-messages)))
                     ;; Replay metadata belongs to one provider request.  Keep
                     ;; it in that request's detached projection, then remove
                     ;; it from the turn before any later continuation starts.
                     (setq turn-messages
                           (e-loop--without-provider-replay-items
                            turn-messages))
                     snapshot))
                  (tool-called nil)
                  (tool-queue nil)
                  (active-tool nil)
                  (provider-done nil)
                  (followup-started nil)
                  (response-assistant-content nil)
                  (response-assistant-message nil)
                  (response-assistant-phase nil)
                  (response-commentary-appended-p nil)
                  (response-text-appended-p nil)
                  (token-usage nil)
                  (done-reason nil)
                  (provider-anchor-candidate nil)
                  ;; Remember request facts that may be cleared before final
                  ;; candidate promotion.  Final response facts are combined
                  ;; again there so backend item order cannot weaken safety.
                  (provider-anchor-candidate-immediate-only-p nil)
                  (pending-provider-replay-items nil)
                  (pending-provider-corrective-replay-items nil)
                  (pending-provider-invalid-replay-items nil)
                  (provider-followup-messages nil)
                  (provider-request nil)
                  (provider-request-id nil)
                  (provider-request-ordinal nil)
                  (provider-request-started-at nil)
                  (provider-request-finished nil)
                  (provider-request-lifetime-frame active-lifetime-frame)
                  (provider-request-options
                   (let ((request-options (copy-sequence turn-options)))
                     ;; The capability advertises the reserved carrier for the
                     ;; turn, but only a request that actually presents a live
                     ;; frame owns an open curation opportunity.
                     (when (and
                            (eq (if (plist-member request-options
                                                  :reserved-effect-carrier)
                                    (plist-get request-options
                                               :reserved-effect-carrier)
                                  (plist-get
                                   (plist-get request-options
                                              :context-capabilities)
                                   :reserved-effect-carrier))
                                'context-curate-wire)
                            (not (and
                                  (e-context-lifetime-frame-p
                                   active-lifetime-frame)
                                  (not
                                   (e-context-lifetime-frame-consumed-p
                                    active-lifetime-frame)))))
                       ;; Explicit nil prevents an adapter profile from
                       ;; re-materializing its default carrier for this
                       ;; closed request opportunity.
                       (setq request-options
                             (plist-put request-options
                                        :reserved-effect-carrier nil)))
                     (when next-request-provider-replay-items
                       (setq request-options
                             (plist-put
                              request-options
                              :provider-request-replay-items
                              (copy-tree
                               next-request-provider-replay-items)))
                       (setq next-request-provider-replay-items nil))
                     (when next-request-provider-skipped-calls
                       (setq request-options
                             (plist-put
                              request-options
                              :provider-request-skipped-calls
                              (copy-tree
                               next-request-provider-skipped-calls)))
                       (setq next-request-provider-skipped-calls nil))
                     request-options))
                  (response-complete-notified nil)
                  (response-preflight-run nil)
                  (response-preflight-result nil)
                  (response-curation-effects nil)
                  (response-curation-rejection nil)
                  (response-entry-id nil)
                  (pending-tool-call-entry-id nil)
                  (provider-request-causes next-request-causes)
                  (provider-request-curation-followup-p
                   immediate-curation-followup-p)
                  (provider-request-projection-identity
                   (plist-get turn-options
                              :continuation-projection-identity))
                  (context-refreshed-p nil))
              (cl-labels
                  ((response-text ()
                     (or response-assistant-message
                         response-assistant-content))
                   (response-commentary-p ()
                     (equal response-assistant-phase "commentary"))
                   (append-response-commentary ()
                     ;; Responses commentary is an actual assistant message,
                     ;; not a display-only reasoning summary.  When it is
                     ;; followed by an ordinary tool call, append it before
                     ;; that call so an explicit replay retains the original
                     ;; assistant phase and causal order.  A curation-only
                     ;; response defers this append until its preflight below.
                     (when (and (not response-curation-rejection)
                                (response-commentary-p)
                                (not response-commentary-appended-p)
                                (not (string-empty-p
                                      (or (response-text) ""))))
                       (unless response-entry-id
                         (setq response-entry-id (e-session-generate-ulid)))
                       (let ((message
                              (e-loop--assistant-message
                               (response-text)
                               ;; Reserved curation acknowledgement is valid
                               ;; only for the immediate next request, not as
                               ;; durable assistant replay metadata.
                               (unless response-curation-effects
                                 (when pending-provider-replay-items
                                   (list :provider-replay-items
                                         pending-provider-replay-items)))
                               response-entry-id
                               response-assistant-phase)))
                         (setq turn-messages
                               (append turn-messages (list message))
                               response-commentary-appended-p t
                               response-text-appended-p t)
                         (funcall append-message message)
                         message)))
                   (append-curation-response-text
                    (replay-items)
                    (when (and (not response-text-appended-p)
                               (not (string-empty-p
                                     (or (response-text) ""))))
                      (unless response-entry-id
                        (setq response-entry-id (e-session-generate-ulid)))
                      (let* ((request-message
                              (e-loop--assistant-message
                               (response-text)
                               (when replay-items
                                 (list :provider-replay-items
                                       (copy-tree replay-items)))
                               response-entry-id response-assistant-phase))
                             (durable-message
                              (if replay-items
                                  (e-loop--assistant-message
                                   (response-text) nil response-entry-id
                                   response-assistant-phase)
                                request-message)))
                        (setq turn-messages
                              (append turn-messages (list request-message))
                              response-text-appended-p t)
                        (when (response-commentary-p)
                          (setq response-commentary-appended-p t))
                        (funcall append-message durable-message)
                        request-message)))
                   (publish-provider-request
                    (request)
                    (setq provider-request request)
                    (setq next-request-causes nil)
                    (setq provider-request-sequence
                          (1+ provider-request-sequence))
                    (setq provider-request-id (e-session-generate-ulid))
                    (setq provider-request-ordinal provider-request-sequence)
                    (setq provider-request-started-at (float-time))
                    (setq provider-request-finished nil)
                    (publish-request request)
                    (e-loop--emit
                     :on-event on-event
                     :type 'provider-request-started
                     :payload
                     (e-loop--request-lifecycle-payload
                      request 'started provider-request-id
                      provider-request-ordinal nil
                      provider-request-causes)))
                   (finish-provider-request
                    (status)
                    (clear-provider-compaction-request-state)
                    ;; Opaque curation acknowledgement state is valid for one
                    ;; provider request only.  Remove it at settlement even
                    ;; though this request's lexical snapshot cannot be reused
                    ;; by a later request.
                    (when (plist-member provider-request-options
                                        :provider-request-replay-items)
                      (setq provider-request-options
                            (copy-sequence provider-request-options))
                      (cl-remf provider-request-options
                               :provider-request-replay-items))
                    (when (and provider-request
                               (not provider-request-finished))
                      (setq provider-request-finished t)
                      (e-loop--emit
                       :on-event on-event
                       :type 'provider-request-finished
                       :payload
                       (e-loop--request-lifecycle-payload
                        provider-request status provider-request-id
                        provider-request-ordinal
                        provider-request-started-at
                        provider-request-causes))))
                  (promote-provider-anchor
                   (&optional immediate-only-p)
                    (let ((effective-immediate-only-p
                           (or immediate-only-p
                               tool-called
                               response-curation-effects
                               provider-anchor-candidate-immediate-only-p)))
                      (when (and provider-anchor-candidate
                                 (or (not context-refreshed-p)
                                     (e-loop--continuation-projection-compatible-p
                                      provider-request-projection-identity
                                      turn-options)))
                        (setq turn-options
                              (e-loop--promote-continuation-candidate
                               turn-options
                               provider-anchor-candidate
                               (length turn-messages)
                               provider-followup-messages
                               effective-immediate-only-p))
                        ;; Only emit a candidate once this loop has accepted it
                        ;; for the current request projection.  Raw provider
                        ;; items are intentionally not durable ownership facts.
                        (e-loop--emit
                         :on-event on-event
                         :type 'provider-anchor-candidate
                         :payload
                         (e-loop--accepted-continuation-candidate
                          provider-anchor-candidate
                          provider-request-projection-identity
                          provider-request-id
                          provider-request-ordinal
                          effective-immediate-only-p))
                        t)))
                  (response-completion-payload
                    ()
                    (list :frame provider-request-lifetime-frame
                          :provider-request-id provider-request-id
                          :provider-request-ordinal provider-request-ordinal
                          :curation-effects
                          (copy-tree response-curation-effects)
                          :response-entry-id response-entry-id
                          :assistant-content (response-text)
                          :assistant-phase response-assistant-phase
                          :tool-called tool-called
                          :reason done-reason))
                  (run-response-preflight
                    ()
                    (when (not response-preflight-run)
                      ;; Staging curation text consumes the replay bundle
                      ;; before completion notification asks for preflight.
                      (setq response-preflight-run t)
                      (when (and response-curation-effects
                                 (not pending-provider-replay-items))
                        (signal 'e-loop-empty-output
                                (list 'curation :missing-ack-target)))
                      (when on-response-preflight
                        (condition-case err
                            (setq response-preflight-result
                                  (funcall on-response-preflight
                                           (response-completion-payload)))
                          (e-context-lifetime-invalid-record
                           (if response-curation-effects
                               (setq response-curation-rejection err
                                     response-curation-effects nil
                                     response-preflight-result nil
                                     pending-provider-replay-items nil)
                             (signal (car err) (cdr err)))))))
                    response-preflight-result)
                  (notify-response-complete
                    ()
                    (when (and (or on-response-complete
                                   on-response-preflight)
                               (not response-complete-notified))
                      ;; Tool responses have no assistant append boundary, so
                      ;; their preflight runs here.  Non-tool responses call
                      ;; `run-response-preflight' before appending below.
                      (run-response-preflight)
                      (unless response-curation-rejection
                        (setq response-complete-notified t))
                      (when (and on-response-complete
                                 (not response-curation-rejection))
                        (let* ((payload (response-completion-payload))
                               (completed
                                (funcall
                                 on-response-complete
                                 (if on-response-preflight
                                     (append
                                      payload
                                      (list :curation-preflight
                                            response-preflight-result))
                                   payload))))
                          ;; A tool may finish before the provider reports its
                          ;; response complete.  In that ordering the tool
                          ;; callback has already installed the descendant
                          ;; bundle frame; completing the producer frame must
                          ;; not roll the frontier back to that older frame.
                          (when (and (e-context-lifetime-frame-p completed)
                                     (or (null active-lifetime-frame)
                                         (equal
                                          (e-context-lifetime-frame-id
                                           active-lifetime-frame)
                                          (and provider-request-lifetime-frame
                                               (e-context-lifetime-frame-id
                                                provider-request-lifetime-frame)))))
                            (setq active-lifetime-frame completed))
                          ;; A successful curation closes the exact label set
                          ;; presented to this response.  Retire those
                          ;; marker messages before its acknowledgement starts;
                          ;; otherwise a later fresh frame is displayed beside
                          ;; stale labels from this consumed frame and the model
                          ;; can submit a label that no longer exists.
                          (when (and response-curation-effects
                                     (e-context-lifetime-frame-p completed)
                                     (e-context-lifetime-frame-consumed-p
                                      completed)
                                     (e-context-lifetime-frame-p
                                      provider-request-lifetime-frame)
                                     (equal
                                      (e-context-lifetime-frame-id completed)
                                      (e-context-lifetime-frame-id
                                       provider-request-lifetime-frame)))
                            (let ((retired
                                   (e-loop--retire-frame-curation-markers
                                    turn-messages turn-options
                                    provider-request-lifetime-frame)))
                              (setq turn-messages
                                    (plist-get retired :messages)
                                    turn-options
                                    (plist-get retired :options))))))))
                   (attach-pending-provider-replay-items
                    ()
                    ;; A reserved provider effect may arrive after an
                    ;; ordinary tool call in the same response.  Its opaque
                    ;; acknowledgement still belongs to this immediate
                    ;; tool-result follow-up, even though the tool result was
                    ;; appended before the effect was decoded.  Replace the
                    ;; in-memory request message rather than mutating the
                    ;; message handed to the session append callback: replay
                    ;; metadata is wire-only and must not leak into durable
                    ;; transcript state.
                    (when pending-provider-replay-items
                      (let* ((causes
                              ;; Results produced by this response will cause
                              ;; the request about to start.  Otherwise the
                              ;; current request's captured causes identify
                              ;; the only historical results eligible to carry
                              ;; its acknowledgement.
                              (if response-curation-effects
                                  (or next-request-causes
                                      provider-request-causes)
                                (if provider-followup-messages
                                    next-request-causes
                                  provider-request-causes)))
                             (cause-ids
                              (delq nil
                                    (mapcar (lambda (cause)
                                              (plist-get cause :id))
                                            causes)))
                             (tool-message
                             (car (last
                                   (cl-remove-if-not
                                    (lambda (message)
                                      (and
                                       (eq (plist-get message :role) 'tool)
                                       (member
                                        (plist-get
                                         (plist-get message :content)
                                         :tool-call-id)
                                        cause-ids)))
                                    ;; A curation-only response to a tool
                                    ;; follow-up has no request-local result
                                    ;; list; its exact cause IDs select the
                                    ;; eligible result from the turn transcript.
                                    (or provider-followup-messages
                                        turn-messages))))))
                        (when tool-message
                          (let* ((request-message (copy-tree tool-message))
                                 (metadata
                                  (copy-tree
                                   (plist-get request-message :metadata))))
                            (setq metadata
                                  (plist-put
                                   metadata
                                   :provider-replay-items
                                   (copy-tree pending-provider-replay-items)))
                            (setq request-message
                                  (plist-put request-message
                                             :metadata metadata))
                            (setq turn-messages
                                  (mapcar
                                   (lambda (message)
                                     (if (eq message tool-message)
                                         request-message
                                       message))
                                   turn-messages))
                            (setq provider-followup-messages
                                  (if provider-followup-messages
                                      (mapcar
                                       (lambda (message)
                                         (if (eq message tool-message)
                                             request-message
                                           message))
                                       provider-followup-messages)
                                    ;; A curation-only response can follow a
                                    ;; completed tool request whose bundle was
                                    ;; already folded into TURN-MESSAGES.  Keep
                                    ;; the replaced result as the immediate
                                    ;; delta so a retained connection receives
                                    ;; the opaque acknowledgement too.
                                    (list request-message)))
                            (setq pending-provider-replay-items nil)
                            t)))))
                   (fail-provider
                    (err)
                    (finish-provider-request 'error)
                    (fail err))
                   (stage-curation-rejection
                    ()
                    (unless pending-provider-invalid-replay-items
                      (signal 'e-loop-empty-output
                              (list 'curation
                                    :missing-invalid-correction-target)))
                    (setq next-request-provider-replay-items
                          (copy-tree pending-provider-invalid-replay-items)
                          pending-provider-replay-items nil
                          pending-provider-corrective-replay-items nil
                          pending-provider-invalid-replay-items nil))
                   (maybe-start-followup
                    ()
                    (when (and provider-done
                               tool-called
                               (not active-tool)
                               (null tool-queue)
                               (not followup-started)
                               (not settled)
                               (not (cancelled)))
                      (setq followup-started t)
                      (unless (or response-text-appended-p
                                  response-curation-rejection)
                        (if (response-commentary-p)
                            (append-response-commentary)
                          (when (not (string-empty-p
                                      (or (response-text) "")))
                            (e-loop--emit
                             :on-event on-event
                             :type 'reasoning-delta
                             :payload
                             (list :type 'reasoning-delta
                                   :stream-kind 'summary
                                   :content (response-text))))))
                      (if response-curation-rejection
                          (stage-curation-rejection)
                        (attach-pending-provider-replay-items))
                      (promote-provider-anchor)
                      (start-request
                       (or response-curation-effects
                           response-curation-rejection))))
                  (maybe-start-curation-rejection-followup
                    ()
                    (when (and provider-done
                               (not tool-called)
                               response-curation-rejection
                               (not followup-started)
                               (not settled)
                               (not (cancelled)))
                      (setq followup-started t)
                      (stage-curation-rejection)
                      (promote-provider-anchor t)
                      (start-request t)))
                   (duplicate-curation-opportunity-p
                    ()
                    (and
                     (e-context-lifetime-frame-p
                      provider-request-lifetime-frame)
                     (e-context-lifetime-frame-consumed-p
                      provider-request-lifetime-frame)
                     (e-context-lifetime-frame-p
                      last-curated-lifetime-frame)
                     (equal
                      (e-context-lifetime-frame-id
                       provider-request-lifetime-frame)
                      (e-context-lifetime-frame-id
                       last-curated-lifetime-frame))))
                   (start-curation-duplicate-followup
                    ()
                    (discard-curation-tool-queue)
                    (condition-case err
                        (progn
                          ;; Validate every ordinary frame-bound field before
                          ;; ignoring a duplicate semantic effect.
                          (e-context-lifetime-prepare-curation-disposition
                           last-curated-lifetime-frame
                           (plist-get (car response-curation-effects)
                                      :arguments)
                           provider-request-id)
                          (if curation-duplicate-correction-used-p
                              (progn
                                (e-loop--emit
                                 :on-event on-event
                                 :type 'backend-empty-output
                                 :payload (list :reason done-reason))
                                (fail '(e-loop-empty-output)))
                            (unless pending-provider-corrective-replay-items
                              (signal 'e-loop-empty-output
                                      (list 'curation
                                            :missing-correction-target)))
                            (e-loop--emit
                             :on-event on-event
                             :type 'context-curation-duplicate-ignored
                             :payload '(:outcome duplicate-ignored))
                            (setq followup-started t
                                  curation-duplicate-correction-used-p t)
                            (if (not (string-empty-p
                                      (or (response-text) "")))
                                (progn
                                  (append-curation-response-text
                                   pending-provider-corrective-replay-items)
                                  (setq pending-provider-corrective-replay-items nil
                                        pending-provider-invalid-replay-items nil
                                        pending-provider-replay-items nil))
                              (setq next-request-provider-replay-items
                                    (copy-tree
                                     pending-provider-corrective-replay-items)
                                    pending-provider-replay-items nil
                                    pending-provider-corrective-replay-items nil))
                            ;; The duplicate response id is valid only for its
                            ;; matching immediate corrective output.
                            (promote-provider-anchor t)
                            (start-request t)))
                      (e-context-lifetime-invalid-record
                      (setq response-curation-rejection err
                            response-curation-effects nil
                            followup-started nil)
                       (start-curation-rejection-followup))))
                   (maybe-start-curation-followup
                    ()
                    ;; A provider may return the reserved curation call as
                    ;; the complete response to an ordinary tool-result
                    ;; follow-up, without an assistant text item.  It still
                    ;; needs one immediate continuation carrying the opaque
                    ;; function-call-output acknowledgement.  Keep this
                    ;; separate from ordinary tool accounting.
                    (when (and provider-done
                               (not tool-called)
                               response-curation-effects
                               (string-empty-p (or (response-text) ""))
                               (not followup-started)
                               (not settled)
                               (not (cancelled)))
                      (if (duplicate-curation-opportunity-p)
                          (start-curation-duplicate-followup)
                        (unless pending-provider-replay-items
                          (signal 'e-loop-empty-output
                                  (list 'curation :missing-ack-target)))
                        ;; This response has no assistant message to carry the
                        ;; durable identity.  Reserve a distinct response id
                        ;; for the audit-only control entry before pure
                        ;; preparation; the harness persists it only after that
                        ;; preparation succeeds.
                        (unless response-entry-id
                          (setq response-entry-id (e-session-generate-ulid)))
                        (setq followup-started t)
                        ;; Commit/consume through the existing completion
                        ;; callback before dispatching the acknowledgement.
                        (notify-response-complete)
                        (setq last-curated-lifetime-frame
                              provider-request-lifetime-frame)
                        ;; Preserve the established tool-result carrier when one
                        ;; exists.  A fresh curation-only response instead hands
                        ;; its opaque call/output pair to exactly the immediate
                        ;; provider request, without fabricating or persisting a
                        ;; semantic message.
                        (unless (attach-pending-provider-replay-items)
                          (setq next-request-provider-replay-items
                                (copy-tree pending-provider-replay-items))
                          (setq pending-provider-replay-items nil))
                        ;; The response id is usable only for this immediate
                        ;; acknowledgement continuation, even when the frame
                        ;; makes it unsafe as a durable anchor.
                        (promote-provider-anchor t)
                        (start-request t))))
                   (current-tool-p
                    (token)
                    (and (listp active-tool)
                         (eq (plist-get active-tool :token) token)))
                   (provider-followup-message-key
                    (message)
                    (let ((role (plist-get message :role))
                          (content (plist-get message :content)))
                      (pcase role
                        ('tool-call
                         (and (plist-get content :id)
                              (list role (plist-get content :id))))
                        ('tool
                         (and (plist-get content :tool-call-id)
                              (list role
                                    (plist-get content :tool-call-id)))))))
                   (curation-buffering-enabled-p
                    ()
                    ;; The wire definition is request-scoped, but buffering
                    ;; lasts through the turn so a stale duplicate can
                    ;; invalidate calls that arrived before its control item.
                    (eq (if (plist-member turn-options
                                          :reserved-effect-carrier)
                            (plist-get turn-options
                                       :reserved-effect-carrier)
                          (plist-get
                           (plist-get turn-options :context-capabilities)
                           :reserved-effect-carrier))
                        'context-curate-wire))
                   (discard-curation-tool-queue
                    ()
                    (setq next-request-provider-skipped-calls
                          (append
                           next-request-provider-skipped-calls
                           (mapcar
                            (lambda (entry)
                              (let ((call (plist-get entry :tool-call)))
                                (list :id (plist-get call :id)
                                      :name (plist-get call :name)
                                      :arguments
                                      (copy-tree
                                       (plist-get call :arguments)))))
                            tool-queue))
                          tool-queue nil
                          tool-called nil))
                   (start-curation-rejection-followup
                    ()
                    (discard-curation-tool-queue)
                    (stage-curation-rejection)
                    (setq followup-started t)
                    (promote-provider-anchor t)
                    (start-request t))
                   (handle-carrier-terminal-response
                    ()
                    (cond
                     (response-curation-rejection
                      (start-curation-rejection-followup))
                     ((and response-curation-effects
                           (duplicate-curation-opportunity-p))
                      (start-curation-duplicate-followup))
                     (response-curation-effects
                      (unless response-entry-id
                        (setq response-entry-id (e-session-generate-ulid)))
                      (run-response-preflight)
                      (if response-curation-rejection
                          (start-curation-rejection-followup)
                        (if tool-called
                            (progn
                              (append-curation-response-text nil)
                              (notify-response-complete)
                              (setq last-curated-lifetime-frame
                                    provider-request-lifetime-frame)
                              (start-next-tool))
                          (if (not (string-empty-p
                                    (or (response-text) "")))
                              (progn
                                (append-curation-response-text
                                 pending-provider-replay-items)
                                (setq pending-provider-replay-items nil
                                      pending-provider-corrective-replay-items nil
                                      pending-provider-invalid-replay-items nil
                                      followup-started t)
                                (notify-response-complete)
                                (setq last-curated-lifetime-frame
                                      provider-request-lifetime-frame)
                                (promote-provider-anchor t)
                                (start-request t))
                            (maybe-start-curation-followup)))))
                     (tool-called
                      (when (response-commentary-p)
                        (append-response-commentary))
                      (unless response-entry-id
                        ;; Carrier buffering delays tool admission until this
                        ;; terminal boundary. Reserve the entry identity now
                        ;; and reuse it when the first tool call is admitted.
                        (setq response-entry-id (e-session-generate-ulid)
                              pending-tool-call-entry-id response-entry-id))
                      (notify-response-complete)
                      (start-next-tool))))
                   (merge-provider-followup-bundle
                    (bundle)
                    ;; Refresh projections are authoritative for later
                    ;; context, but the current stateless follow-up still
                    ;; needs the runtime-only call/result bundle exactly once.
                    (dolist (message bundle)
                      (let ((key (provider-followup-message-key message)))
                        (unless (and key
                                     (cl-some
                                      (lambda (existing)
                                        (equal key
                                               (provider-followup-message-key
                                                existing)))
                                      turn-messages))
                          (setq turn-messages
                                (append turn-messages (list message)))))))
                   (publish-tool-request
                    (token request)
                    (when (and (current-tool-p token)
                               (not settled)
                               (not (cancelled)))
                      (setq active-tool
                            (list :token token :request request))
                      (publish-request request)))
                   (finish-tool
                    (token tool-call failure-call result)
                    (when (and (not settled)
                               (not (cancelled))
                               (current-tool-p token))
                      (setq active-tool nil)
                      (let* ((message
                              (list :role 'tool
                                    :content result
                                    :metadata (plist-get result :metadata)))
                             (stored-message nil)
                             (tool-call-ids nil)
                             (provider-followup-bundle nil))
                        (setq turn-messages
                              (append turn-messages (list message)))
                        (setq provider-followup-messages
                              (append provider-followup-messages
                                      (list message)))
                        (setq tool-call-ids
                              (mapcar
                               (lambda (result-message)
                                 (plist-get (plist-get result-message :content)
                                            :tool-call-id))
                               provider-followup-messages))
                        (setq provider-followup-bundle
                              (append
                               (cl-remove-if-not
                                (lambda (candidate)
                                  (and (eq (plist-get candidate :role)
                                           'tool-call)
                                       (member
                                        (plist-get
                                         (plist-get candidate :content) :id)
                                        tool-call-ids)))
                                turn-messages)
                               provider-followup-messages))
                        (setq stored-message (funcall append-message message))
                        (e-loop--emit
                         :on-event on-event
                         :type 'tool-finished
                         :payload (list :tool-call tool-call
                                        :result result))
                        (let ((fingerprint
                               (e-loop--tool-failure-fingerprint
                                failure-call result)))
                          (cond
                           ((null fingerprint)
                            (setq previous-tool-failure nil))
                           ((equal fingerprint previous-tool-failure)
                            (fail
                             (list 'e-loop-repeated-tool-failure
                                   :tool (plist-get fingerprint :tool)
                                   :error (plist-get fingerprint :error))))
                           (t
                            (setq previous-tool-failure fingerprint))))
                      (unless settled
                      (when (plist-get (plist-get result :metadata)
                                       :refresh-context)
                        (cond
                         (refresh-context
                          (apply-context-refresh (funcall refresh-context))
                          (when (lifetime-projection-enabled-p)
                            (merge-provider-followup-bundle
                             provider-followup-bundle))
                          (setq context-refreshed-p t))
                         (refresh-messages
                          ;; Compatibility for callers that have not yet
                          ;; adopted the atomic projection contract.
                          (apply-context-refresh
                           (list :messages (funcall refresh-messages)
                                 :options turn-options
                                 :segments (plist-get turn-options :segments)
                                 :observation-frontier
                                 (plist-get turn-options
                                            :observation-frontier)))
                          (when (lifetime-projection-enabled-p)
                            (merge-provider-followup-bundle
                             provider-followup-bundle))
                          (setq context-refreshed-p t))))
                      ;; Refresh is a whole request projection.  Capture the
                      ;; tool/result bundle after it so a refreshed current
                      ;; state and the inherited result remain one frame.
                      (when on-tool-observation
                        (let* ((previous-frame active-lifetime-frame)
                               (observation-payload
                                (list :tool-call tool-call
                                      :result result
                                      :message (or stored-message message)
                                      :previous-frame previous-frame))
                               (new-frame
                                (funcall on-tool-observation
                                         observation-payload)))
                          (setq active-lifetime-frame new-frame)
                          (when active-lifetime-frame
                            (when on-tool-observation-presentation
                              (let ((projection
                                     (funcall
                                      on-tool-observation-presentation
                                      (append
                                       observation-payload
                                       (list :frame active-lifetime-frame
                                             :turn-messages turn-messages
                                             :provider-followup-messages
                                             provider-followup-messages)))))
                                (unless (and (listp projection)
                                             (plist-member projection
                                                           :turn-messages)
                                             (plist-member projection
                                                           :provider-followup-messages))
                                  (signal 'wrong-type-argument
                                          (list 'e-loop-tool-observation-presentation
                                                projection)))
                                (setq turn-messages
                                      (plist-get projection :turn-messages)
                                      provider-followup-messages
                                      (plist-get projection
                                                 :provider-followup-messages))))
                          ;; Provider options are a request snapshot.  Do not
                          ;; mutate the plist captured by the still-running
                          ;; provider callback while installing the descendant
                          ;; frame for its follow-up.
                          (setq turn-options (copy-sequence turn-options))
                          (setq turn-options
                                (plist-put turn-options
                                           :lifetime-ephemerals-clean-p nil)))))
                      (setq next-request-causes
                            (append next-request-causes (list tool-call)))
                      (start-next-tool)
                      (maybe-start-followup)))))
                   (start-next-tool
                    ()
                    (when (and (not active-tool)
                               tool-queue
                               (not settled)
                               (not (cancelled)))
                      (condition-case err
                          (let* ((entry (pop tool-queue))
                                 (execution-call
                                  (if tool-lifecycle
                                      (e-tool-lifecycle-prepare-call
                                       tool-lifecycle
                                       (plist-get entry :tool-call))
                                    (plist-get entry :tool-call)))
                                 (archival-call nil)
                                 (archival-rejected-p nil)
                                 (archival-received-arguments nil)
                                 (tool-call
                                  (if (not (e-tools-registry-p tools))
                                      execution-call
                                    (condition-case err
                                        (e-tools-prepare-call
                                         tools
                                         execution-call)
                                      (e-tools-invalid-arguments
                                       ;; Keep provider protocol shape while
                                       ;; dropping undeclared rejected fields
                                       ;; before transcript and activity writes.
                                       (let* ((prepared
                                               (or (plist-get (cddr err)
                                                              :prepared-call)
                                                   execution-call))
                                              (archival (copy-tree prepared)))
                                         (setq archival-call archival
                                               archival-rejected-p t
                                               archival-received-arguments
                                               (e-tools--copy-canonical-value
                                                (plist-get archival :arguments)))
                                         (let ((rejected
                                                (e-tools-project-call-for-rejection
                                                 tools prepared)))
                                           (plist-put
                                            rejected
                                            :metadata
                                            (plist-put
                                             (copy-sequence
                                              (plist-get rejected :metadata))
                                             :argument-status 'invalid))))))))
                                 ;; Rejection projection deliberately removes
                                 ;; invalid fields from the transcript.  The
                                 ;; anti-loop identity must still distinguish
                                 ;; the actual received calls, so retain those
                                 ;; arguments only in this turn-local copy.
                                 (failure-call
                                  (let ((copy (copy-tree tool-call)))
                                    (when archival-rejected-p
                                      (plist-put copy :arguments
                                                 archival-received-arguments))
                                    copy))
                                 (tool-token (list :tool-call tool-call))
                                 (tool-call-message
                                  (list :role 'tool-call
                                        ;; This provider response owns the
                                        ;; durable identity at admission.  A
                                        ;; completion callback must not query
                                        ;; session history merely to recover
                                        ;; an ID that the live turn can carry.
                                        :id (or pending-tool-call-entry-id
                                                (e-session-generate-ulid))
                                        :content tool-call
                                        :metadata nil)))
                            (setq response-entry-id
                                  (plist-get tool-call-message :id))
                            (setq pending-tool-call-entry-id nil)
                            (setq active-tool (list :token tool-token))
                            (setq turn-messages
                                  (append turn-messages
                                          (list tool-call-message)))
                            (funcall append-message tool-call-message)
                            (e-loop--emit :on-event on-event
                                          :type 'tool-started
                                          :payload tool-call)
                            (when on-tool-call-start
                              (funcall on-tool-call-start
                                       tool-call
                                       archival-call
                                       archival-rejected-p
                                       archival-received-arguments))
                            (let ((request
                                   (if tool-lifecycle
                                       (e-tool-lifecycle-start-call
                                        tool-lifecycle
                                        tool-call
                                        :archival-call archival-call
                                        :archival-rejected-p archival-rejected-p
                                        :archival-received-arguments
                                        archival-received-arguments
                                        :on-request-start
                                        (lambda (request)
                                          (dispatch-callback
                                           (lambda ()
                                             (condition-case err
                                                 (publish-tool-request
                                                  tool-token request)
                                               (error (fail err))))))
                                        :on-event
                                        (lambda (type payload)
                                          (dispatch-callback
                                           (lambda ()
                                             (condition-case err
                                                 (e-loop--emit
                                                  :on-event on-event
                                                  :type type
                                                  :payload payload)
                                               (error (fail err))))))
                                        :on-done
                                        (lambda (result)
                                          (dispatch-callback
                                           (lambda ()
                                             (condition-case err
                                                 (finish-tool
                                                  tool-token tool-call
                                                  failure-call result)
                                               (error (fail err))))))
                                        :on-error
                                        (lambda (err)
                                          (dispatch-callback
                                           (lambda () (fail err)))))
                                     (e-tools-start
                                      tools
                                      tool-call
                                       :context
                                       (list :session-id session-id
                                             :turn-id turn-id
                                             :parent-work-id
                                             (and (e-work-handle-p turn-work-handle)
                                                  (e-work-handle-id turn-work-handle))
                                              :root-work-id
                                              (and (e-work-handle-p turn-work-handle)
                                                   (e-work-handle-id turn-work-handle))
                                              :board-enroll-work board-enroll-work
                                              :deadline
                                              (plist-get turn-options :deadline))
                                      :on-request-start
                                      (lambda (request)
                                        (dispatch-callback
                                         (lambda ()
                                           (condition-case err
                                               (publish-tool-request
                                                tool-token request)
                                             (error (fail err))))))
                                      :on-event
                                      (lambda (type payload)
                                        (dispatch-callback
                                         (lambda ()
                                           (condition-case err
                                               (e-loop--emit
                                                :on-event on-event
                                                :type type
                                                :payload payload)
                                             (error (fail err))))))
                                      :on-done
                                      (lambda (result)
                                        (dispatch-callback
                                         (lambda ()
                                           (condition-case err
                                               (finish-tool
                                                tool-token tool-call
                                                failure-call result)
                                             (error (fail err))))))
                                      :on-error
                                      (lambda (err)
                                        (dispatch-callback
                                         (lambda () (fail err))))))))
                              (when (and request
                                         (current-tool-p tool-token)
                                         (not settled)
                                         (not (cancelled)))
                                (setq active-tool
                                      (list :token tool-token
                                            :request request)))))
                        (error
                         (fail err)))))
                  (handle-backend-item
                   (item)
                   (unless (or settled (cancelled))
                     (condition-case err
                         (progn
                           (setq item (e-loop--normalized-backend-item item))
                           (pcase (plist-get item :type)
                             ('context-curate
                              (when (or response-curation-effects
                                        response-curation-rejection)
                                (signal 'e-context-lifetime-invalid-record
                                        (list 'multiple-curations
                                              provider-request-id)))
                              ;; A provider adapter may attach one opaque
                              ;; acknowledgement item for its reserved
                              ;; carrier.  It is replay metadata for the
                              ;; immediate wire continuation, never an
                              ;; ordinary model-facing tool or semantic fact.
                              (let ((replay-items
                                     (or (plist-get item
                                                   :provider-replay-items)
                                         (when-let* ((replay-item
                                                     (plist-get
                                                      item
                                                      :provider-replay-item)))
                                           (list replay-item)))))
                                (when replay-items
                                  (setq pending-provider-replay-items
                                        (append pending-provider-replay-items
                                                (copy-tree replay-items)))))
                              (when-let* ((corrective-replay-items
                                         (plist-get
                                          item
                                          :provider-corrective-replay-items)))
                                (setq pending-provider-corrective-replay-items
                                      (append
                                       pending-provider-corrective-replay-items
                                       (copy-tree corrective-replay-items))))
                              (when-let* ((invalid-replay-items
                                         (plist-get
                                          item
                                          :provider-invalid-replay-items)))
                                (setq pending-provider-invalid-replay-items
                                      (append
                                       pending-provider-invalid-replay-items
                                       (copy-tree invalid-replay-items))))
                              (setq item (copy-sequence item))
                              (cl-remf item :provider-replay-item)
                              (cl-remf item :provider-replay-items)
                              (cl-remf item :provider-corrective-replay-items)
                              (cl-remf item :provider-invalid-replay-items)
                              (condition-case err
                                  (setq response-curation-effects
                                        (list
                                         (list
                                          :type 'context-curate
                                          :arguments
                                          (e-context-lifetime-normalize-curation-disposition
                                           (plist-get item :arguments)))))
                                (e-context-lifetime-invalid-record
                                 (setq response-curation-rejection err
                                       pending-provider-replay-items nil))))
                             ('assistant-delta
                              (setq response-assistant-content
                                    (concat response-assistant-content
                                            (plist-get item :content)))
                              (e-loop--emit :on-event on-event
                                            :type 'assistant-delta
                                            :payload item))
                             ('assistant-message
                              (setq response-assistant-message
                                    (plist-get item :content)
                                    response-assistant-phase
                                    (plist-get item :phase)))
                             ('reasoning-delta
                              (e-loop--emit :on-event on-event
                                            :type 'reasoning-delta
                                            :payload item))
                             ('reasoning-raw-delta
                              (e-loop--emit :on-event on-event
                                            :type 'reasoning-raw-delta
                                            :payload item))
                             ('provider-replay-item
                              (setq pending-provider-replay-items
                                    (append pending-provider-replay-items
                                            (list (copy-tree item)))))
                             ('tool-call
                              (when (and pending-provider-replay-items
                                         (not (and response-curation-effects
                                                   (curation-buffering-enabled-p))))
                                (setq item (copy-sequence item))
                                (plist-put
                                 item
                                 :provider-replay-items
                                 pending-provider-replay-items)
                                (setq pending-provider-replay-items nil))
                              (enqueue-tool-call item))
                             ('token-usage
                              (setq token-usage
                                    (append
                                     (copy-sequence (plist-get item :usage))
                                     (list :provider-request-id
                                           provider-request-id
                                           :provider-request-ordinal
                                           provider-request-ordinal)
                                     (e-loop--request-cause-fields
                                      provider-request-causes)))
                              (e-loop--emit
                               :on-event on-event
                               :type 'token-usage
                               :payload token-usage))
                             ('provider-anchor-candidate
                              (let ((immediate-only-p
                                     (or tool-called
                                         response-curation-effects
                                         provider-request-curation-followup-p
                                         (plist-member
                                          provider-request-options
                                          :provider-request-replay-items))))
                                (when (e-loop--continuation-candidate-p
                                       turn-options item immediate-only-p)
                                  (setq provider-anchor-candidate item
                                        provider-anchor-candidate-immediate-only-p
                                        (and immediate-only-p t))))
                              ;; Do not forward the raw item.  The accepted
                              ;; event is emitted by `promote-provider-anchor'
                              ;; only after the loop has checked the current
                              ;; projection identity.
                              nil)
                             ('done
                              (setq done-reason
                                    (plist-get item :reason)))
                             ('backend-error
                              (fail-provider
                               (list 'e-loop-backend-error
                                     (plist-get item :content)
                                     (plist-get item :payload))))
                             (_
                              (e-loop--emit
                               :on-event on-event
                               :type 'backend-item-ignored
                               :payload item))))
                       (error
                        (fail-provider err)))))
                   (enqueue-tool-call
                    (item)
                    ;; Curation-capable turns hold calls until the complete
                    ;; response can pass core preflight or duplicate checks.
                    (when (and (or response-curation-effects
                                   response-curation-rejection)
                               (not (curation-buffering-enabled-p)))
                      (signal 'e-context-lifetime-invalid-record
                              (list 'curation-mixed-order
                                    provider-request-id)))
                    ;; A commentary preamble is an assistant message in the
                    ;; provider transcript.  Admit it before the ordinary tool
                    ;; call that follows it, preserving replay order.
                    (unless (curation-buffering-enabled-p)
                      (append-response-commentary))
                    (setq tool-called t)
                    (setq tool-queue
                          (append tool-queue
                                  (list (list :tool-call item))))
                    (unless (curation-buffering-enabled-p)
                      (start-next-tool))))
              (setq
               active-request
               (condition-case err
                   (let ((reported-request nil))
                     (let* ((work-handle
                             (e-loop--profile-call
                              'loop.backend-start
                              (list :session-id session-id
                                    :turn-id turn-id
                                    :metadata
                                    (list :message-count (length turn-messages)
                                          :tool-count (length tools)))
                              (lambda ()
                                (e-work-start
                                 (e-work-spec-create
                                  :id "backend_turn"
                                  :description "Run one provider turn."
                                  :execution 'backend
                                  :interactive-policy 'async
                                  :owner 'loop
                                  :backend (lambda (_arguments _context)
                                             backend)
                                  :messages (lambda (_arguments _context)
                                              request-messages)
                                  :options (lambda (_arguments _context)
                                             provider-request-options)
                                  :request-handler
                                  (lambda (handle _request _arguments _context)
                                    (let ((request
                                           (e-loop--backend-work-request
                                            handle)))
                                      (setq reported-request request)
                                      (dispatch-callback
                                       (lambda ()
                                         (condition-case err
                                             (publish-provider-request request)
                                           (error
                                            (fail-provider err)))))))
                                  :item-handler
                                  (lambda (_handle item _arguments _context)
                                    (dispatch-callback
                                     (lambda ()
                                       (handle-backend-item item)))))
                                 nil
                                  :context (list :session-id session-id
                                                 :turn-id turn-id
                                                 :parent-work-id
                                                 (and (e-work-handle-p turn-work-handle)
                                                      (e-work-handle-id turn-work-handle))
                                                 :root-work-id
                                                 (and (e-work-handle-p turn-work-handle)
                                                      (e-work-handle-id turn-work-handle))
                                                 :deadline
                                                 (plist-get turn-options :deadline))
                                 :on-done
                                 (lambda (_backend-result)
                                   (dispatch-callback
                                    (lambda ()
                                      (unless (or settled (cancelled))
                                        (condition-case err
                                            (progn
                                           (finish-provider-request 'done)
                                           (setq provider-done t)
                                           (if (or
                                                (and (curation-buffering-enabled-p)
                                                     (or tool-called
                                                         response-curation-effects
                                                         response-curation-rejection))
                                                (and (not tool-called)
                                                     (or response-curation-effects
                                                         response-curation-rejection)))
                                               (handle-carrier-terminal-response)
                                             (if tool-called
                                                 (progn
                                                   (notify-response-complete)
                                                   (maybe-start-followup))
                                             (progn
                                               (when (and
                                                      response-curation-effects
                                                      (not
                                                       (duplicate-curation-opportunity-p)))
                                                 (unless response-entry-id
                                                   (setq response-entry-id
                                                         (e-session-generate-ulid)))
                                                 (run-response-preflight))
                                                (cond
                                                (response-curation-rejection
                                                 (maybe-start-curation-rejection-followup))
                                                ((response-commentary-p)
                                                 ;; `commentary' is an
                                                 ;; intermediate assistant
                                                 ;; message.  Preserve it,
                                                 ;; complete any curation
                                                 ;; preflight, and continue
                                                 ;; the same provider turn.
                                                 (append-response-commentary)
                                                 (if response-curation-rejection
                                                     (stage-curation-rejection)
                                                   (progn
                                                     (notify-response-complete)
                                                     (when response-curation-effects
                                                       (unless
                                                           (attach-pending-provider-replay-items)
                                                         (setq
                                                          next-request-provider-replay-items
                                                          (copy-tree
                                                           pending-provider-replay-items))
                                                         (setq pending-provider-replay-items
                                                               nil)))))
                                                 (setq followup-started t)
                                                 (promote-provider-anchor
                                                  (and response-curation-effects t))
                                                 (start-request
                                                  (and response-curation-effects t)))
                                                ((string-empty-p
                                                  (or (response-text) ""))
                                                 (if response-curation-effects
                                                     (maybe-start-curation-followup)
                                                   (progn
                                                     (e-loop--emit
                                                      :on-event on-event
                                                      :type 'backend-empty-output
                                                      :payload (list :reason
                                                                     done-reason))
                                                     (fail '(e-loop-empty-output)))))
                                                (t
                                                 (let ((message
                                                      (progn
                                                        ;; Curation validation
                                                        ;; completes before the
                                                        ;; assistant reaches the
                                                        ;; session append callback.
                                                        (unless response-entry-id
                                                          (setq response-entry-id
                                                                (e-session-generate-ulid)))
                                                        (run-response-preflight)
                                                      (e-loop--assistant-message
                                                       (response-text)
                                                       (when pending-provider-replay-items
                                                         (list
                                                          :provider-replay-items
                                                          pending-provider-replay-items))
                                                       response-entry-id
                                                       response-assistant-phase))))
                                                 (setq turn-messages
                                                       (append turn-messages
                                                               (list message)))
                                                 (funcall append-message message)
                                                 (notify-response-complete)
                                                 (promote-provider-anchor)
                                                 (if (drain-pending t)
                                                     (start-request)
                                                   (finish done-reason
                                                           (response-text))))))))))
                                          (error
                                           (fail-provider err)))))))
                                 :on-error
                                 (lambda (err)
                                   (dispatch-callback
                                    (lambda () (fail-provider err))))))))
                            (request
                             (or reported-request
                                 (when (and work-handle (not settled))
                                   (when (plist-get
                                          (e-work-handle-metadata work-handle)
                                          :backend-request)
                                     (e-loop--backend-work-request
                                      work-handle))))))
                       (when (and request
                                  (not settled)
                                  (not (eq request reported-request)))
                         (publish-provider-request request))
                       request))
                (error
                  (fail-provider err)
                  nil))))))))
      (e-loop--emit :on-event on-event
                    :type 'turn-started
                    :payload nil)
      (start-request)
      active-request)))

(cl-defun e-loop-run-turn-batch
    (&key session-id turn-id messages backend tools tool-lifecycle options on-event
            append-message refresh-context refresh-messages on-request-start
            callback-dispatcher
            segments turn-work-handle
            board-enroll-work lifetime-frame on-response-preflight
            on-response-complete
            on-tool-observation on-tool-observation-presentation
            on-tool-call-start)
  "Synchronously run one agent turn from batch/test code.
SESSION-ID and TURN-ID identify the turn.
MESSAGES, BACKEND, TOOLS, TOOL-LIFECYCLE, OPTIONS, ON-EVENT, APPEND-MESSAGE,
REFRESH-CONTEXT, and REFRESH-MESSAGES define the turn context and output
callbacks.  CALLBACK-DISPATCHER preserves admitted asynchronous callback
ordering across an owning persistence barrier.  REFRESH-CONTEXT returns one
atomic request projection; the
messages-only callback remains for compatibility.
ON-REQUEST-START receives the backend request handle when an adapter exposes
one.  ON-RESPONSE-PREFLIGHT, when supplied, runs before an assistant append
and returns the pure completion value passed to ON-RESPONSE-COMPLETE."
  (when (e-request-hot-path-active-p)
    (e-request-hot-path-blocking-error 'e-loop-run-turn-batch))
  (let ((done nil)
        (result nil)
        (failure nil))
    (e-loop-start-turn
     :session-id session-id
     :turn-id turn-id
     :messages messages
     :backend backend
     :tools tools
     :tool-lifecycle tool-lifecycle
      :options options
      :turn-work-handle turn-work-handle
      :board-enroll-work board-enroll-work
     :segments segments
     :on-event on-event
     :append-message append-message
     :refresh-context refresh-context
     :refresh-messages refresh-messages
     :on-request-start on-request-start
     :callback-dispatcher callback-dispatcher
     :lifetime-frame lifetime-frame
     :on-response-preflight on-response-preflight
     :on-response-complete on-response-complete
     :on-tool-observation on-tool-observation
     :on-tool-observation-presentation on-tool-observation-presentation
     :on-tool-call-start on-tool-call-start
     :on-done (lambda (value)
                (setq result value)
                (setq done t))
     :on-error (lambda (err)
                 (setq failure err)
                 (setq done t)))
    (while (not done)
      (accept-process-output nil 0.01))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(provide 'e-loop)

;;; e-loop.el ends here

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
    (content &optional metadata response-entry-id)
  "Return an assistant message with CONTENT, METADATA, and optional ID.

RESPONSE-ENTRY-ID is allocated before completion preflight so that a pure
preflight and the subsequent durable append share one identity."
  (let ((message (list :role 'assistant
                       :content content
                       :metadata metadata)))
    (when response-entry-id
      (plist-put message :id response-entry-id))
    message))

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
    (when-let ((diagnostics
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
            on-done on-error
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
metadata, before tool execution begins."
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
        ;; A reserved curation-only response gets at most one immediate
        ;; provider continuation for its opaque function-call acknowledgement.
        ;; This is turn-local protocol state, not durable response history.
        (curation-only-followups 0)
        ;; A curation-only response has no semantic message to carry its opaque
        ;; provider acknowledgement.  Hold that wire state only until the next
        ;; request captures it; each request receives its own options snapshot.
        (next-request-provider-replay-items nil)
        (active-lifetime-frame lifetime-frame))
    (cl-labels
        ((cancelled ()
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
          ()
          (unless (or settled (cancelled))
            (drain-pending)
            (let ((tool-called nil)
                  (tool-queue nil)
                  (active-tool nil)
                  (provider-done nil)
                  (followup-started nil)
                  (response-assistant-content nil)
                  (response-assistant-message nil)
                  (token-usage nil)
                  (done-reason nil)
                  (provider-anchor-candidate nil)
                  (pending-provider-replay-items nil)
                  (provider-followup-messages nil)
                  (provider-request nil)
                  (provider-request-id nil)
                  (provider-request-ordinal nil)
                  (provider-request-started-at nil)
                  (provider-request-finished nil)
                  (provider-request-options
                   (let ((request-options (copy-sequence turn-options)))
                     (when next-request-provider-replay-items
                       (setq request-options
                             (plist-put
                              request-options
                              :provider-request-replay-items
                              (copy-tree
                               next-request-provider-replay-items)))
                       (setq next-request-provider-replay-items nil))
                     request-options))
                  (response-complete-notified nil)
                  (response-preflight-run nil)
                  (response-preflight-result nil)
                  (response-curation-effects nil)
                  (response-entry-id nil)
                  (provider-request-causes next-request-causes)
                  (provider-request-lifetime-frame active-lifetime-frame)
                  (provider-request-projection-identity
                   (plist-get turn-options
                              :continuation-projection-identity))
                  (context-refreshed-p nil))
              (cl-labels
                  ((response-text ()
                     (or response-assistant-message
                         response-assistant-content))
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
                             (or immediate-only-p tool-called)))
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
                        (or immediate-only-p tool-called)))
                      t))
                  (response-completion-payload
                    ()
                    (list :frame provider-request-lifetime-frame
                          :provider-request-id provider-request-id
                          :provider-request-ordinal provider-request-ordinal
                          :curation-effects
                          (copy-tree response-curation-effects)
                          :response-entry-id response-entry-id
                          :assistant-content (response-text)
                          :tool-called tool-called
                          :reason done-reason))
                  (run-response-preflight
                    ()
                    (when (and on-response-preflight
                               (not response-preflight-run))
                      ;; Set the guard before entering the callback so a
                      ;; callback that observes completion cannot prepare the
                      ;; same response twice.
                      (setq response-preflight-run t
                            response-preflight-result
                            (funcall on-response-preflight
                                     (response-completion-payload))))
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
                      (setq response-complete-notified t)
                      (when on-response-complete
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
                            (setq active-lifetime-frame completed))))))
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
                      (let ((tool-message
                             (car (last
                                   (cl-remove-if-not
                                    (lambda (message)
                                      (eq (plist-get message :role) 'tool))
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
                      (when (not (string-empty-p
                                  (or (response-text) "")))
                        (e-loop--emit
                         :on-event on-event
                         :type 'reasoning-delta
                         :payload
                         (list :type 'reasoning-delta
                               :stream-kind 'summary
                               :content (response-text))))
                      (attach-pending-provider-replay-items)
                      (promote-provider-anchor)
                      (start-request)))
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
                      (when (> curation-only-followups 0)
                        (signal 'e-context-lifetime-invalid-record
                                (list 'curation
                                      :repeated-empty-response
                                      provider-request-id)))
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
                      (setq curation-only-followups
                            (1+ curation-only-followups))
                      ;; Commit/consume through the existing completion
                      ;; callback before dispatching the acknowledgement.
                      (notify-response-complete)
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
                      (start-request)))
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
                    (token tool-call result)
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
                      (maybe-start-followup))))
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
                                      (e-tools-invalid-stated-purpose
                                       ;; Keep the provider protocol shape
                                       ;; while dropping invalid envelope
                                       ;; fields before transcript writes.
                                       (let* ((tool
                                               (gethash
                                                (plist-get execution-call :name)
                                                (e-tools-registry-tools tools)))
                                              (archival
                                               (e-tools--call-without-stated-purpose
                                                execution-call tool))
                                              (rejected
                                               (e-tools-project-call-for-rejection
                                                tools execution-call)))
                                         (setq archival-call archival
                                               archival-rejected-p t
                                               archival-received-arguments
                                               (e-tools--copy-schema-value
                                                (plist-get archival :arguments)))
                                         (plist-put
                                          rejected
                                          :metadata
                                          (plist-put
                                           (copy-sequence
                                            (plist-get rejected :metadata))
                                           :purpose-status 'invalid))))
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
                                               (e-tools--copy-schema-value
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
                                 (tool-token (list :tool-call tool-call))
                                 (tool-call-message
                                  (list :role 'tool-call
                                        :content tool-call
                                        :metadata nil)))
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
                                          (publish-tool-request
                                           tool-token request))
                                        :on-event
                                        (lambda (type payload)
                                          (e-loop--emit
                                           :on-event on-event
                                           :type type
                                           :payload payload))
                                        :on-done
                                        (lambda (result)
                                          (finish-tool
                                           tool-token tool-call result))
                                        :on-error #'fail)
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
                                        (publish-tool-request
                                         tool-token request))
                                      :on-event
                                      (lambda (type payload)
                                        (e-loop--emit
                                         :on-event on-event
                                         :type type
                                         :payload payload))
                                      :on-done
                                      (lambda (result)
                                        (finish-tool
                                         tool-token tool-call result))
                                      :on-error #'fail))))
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
                              (when response-curation-effects
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
                                         (when-let ((replay-item
                                                     (plist-get
                                                      item
                                                      :provider-replay-item)))
                                           (list replay-item)))))
                                (when replay-items
                                  (setq pending-provider-replay-items
                                        (append pending-provider-replay-items
                                                (copy-tree replay-items)))))
                              (setq item (copy-sequence item))
                              (cl-remf item :provider-replay-item)
                              (cl-remf item :provider-replay-items)
                              (setq response-curation-effects
                                    (list
                                     (list
                                      :type 'context-curate
                                      :arguments
                                      (e-context-lifetime-normalize-curation-disposition
                                       (plist-get item :arguments))))))
                             ('assistant-delta
                              (setq response-assistant-content
                                    (concat response-assistant-content
                                            (plist-get item :content)))
                              (e-loop--emit :on-event on-event
                                            :type 'assistant-delta
                                            :payload item))
                             ('assistant-message
                              (setq response-assistant-message
                                    (plist-get item :content)))
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
                              (when pending-provider-replay-items
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
                              (when (e-loop--continuation-candidate-p
                                     turn-options item tool-called)
                                (setq provider-anchor-candidate item))
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
                   ;; A curation is the terminal semantic decision for this
                   ;; provider response.  Ordinary calls already observed
                   ;; before it retain their existing queued/executing
                   ;; behavior, but a later call would make the response
                   ;; ordering ambiguous and must fail before dispatch.
                   (when response-curation-effects
                     (signal 'e-context-lifetime-invalid-record
                             (list 'curation-mixed-order
                                   provider-request-id)))
                   (setq tool-called t)
                   (setq tool-queue
                         (append tool-queue
                                 (list (list :tool-call item))))
                   (start-next-tool)))
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
                                              turn-messages)
                                  :options (lambda (_arguments _context)
                                             provider-request-options)
                                  :request-handler
                                  (lambda (handle _request _arguments _context)
                                    (let ((request
                                           (e-loop--backend-work-request
                                            handle)))
                                      (setq reported-request request)
                                      (publish-provider-request request)))
                                  :item-handler
                                  (lambda (_handle item _arguments _context)
                                    (handle-backend-item item)))
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
                                   (unless (or settled (cancelled))
                                     (condition-case err
                                         (progn
                                           (finish-provider-request 'done)
                                           (setq provider-done t)
                                           (if tool-called
                                               (progn
                                                 (notify-response-complete)
                                                 (maybe-start-followup))
                                             (if (string-empty-p
                                                  (or (response-text) ""))
                                                 (if response-curation-effects
                                                     (maybe-start-curation-followup)
                                                   (progn
                                                     (e-loop--emit
                                                      :on-event on-event
                                                      :type 'backend-empty-output
                                                      :payload (list :reason
                                                                     done-reason))
                                                     (fail '(e-loop-empty-output))))
                                               (let ((message
                                                      (progn
                                                        ;; Curation validation
                                                        ;; must complete before
                                                        ;; the assistant reaches
                                                        ;; the session append
                                                        ;; callback.  A signal
                                                        ;; here is handled by
                                                        ;; the existing provider
                                                        ;; failure boundary.
                                                        (setq response-entry-id
                                                              (e-session-generate-ulid))
                                                        (run-response-preflight)
                                                      (e-loop--assistant-message
                                                       (response-text)
                                                       (when pending-provider-replay-items
                                                         (list
                                                          :provider-replay-items
                                                          pending-provider-replay-items))
                                                       response-entry-id))))
                                                 (setq turn-messages
                                                       (append turn-messages
                                                               (list message)))
                                                 (funcall append-message message)
                                                 (notify-response-complete)
                                                 (promote-provider-anchor)
                                                 (if (drain-pending t)
                                                     (start-request)
                                                   (finish done-reason
                                                           (response-text)))))))
                                       (error
                                        (fail-provider err)))))
                                 :on-error #'fail-provider))))
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
            segments turn-work-handle
            board-enroll-work lifetime-frame on-response-preflight
            on-response-complete
            on-tool-observation on-tool-observation-presentation
            on-tool-call-start)
  "Synchronously run one agent turn from batch/test code.
SESSION-ID and TURN-ID identify the turn.
MESSAGES, BACKEND, TOOLS, TOOL-LIFECYCLE, OPTIONS, ON-EVENT, APPEND-MESSAGE,
REFRESH-CONTEXT, and REFRESH-MESSAGES define the turn context and output
callbacks.  REFRESH-CONTEXT returns one atomic request projection; the
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

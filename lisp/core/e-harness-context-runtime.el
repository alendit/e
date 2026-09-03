;;; e-harness-context-runtime.el --- Context and compaction policy owner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns backend-neutral context construction, generational context-lifetime
;; projection, provider anchors, auto/provider compaction policy, and prompt
;; option derivation.  It consumes explicit state projections and semantic
;; activity/turn-state ports; it does not call the application facade.

;;; Code:

(require 'cl-lib)
(require 'e-harness-state)
(require 'e-harness-capabilities)
(require 'e-harness-activity)
(require 'e-harness-turn-state)
(require 'e-backend)
(require 'e-compaction)
(require 'e-context)
(require 'e-context-lifetime)
(require 'e-events)
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

(defcustom e-harness-provider-request-deadline-seconds nil
  "Optional hard wall-clock deadline attached to a turn's provider requests.
The deadline is stamped once when turn options are built and shared by every
provider request in that turn.  Leave nil unless a deliberate per-turn cap is
required; explicit `:deadline' options still apply."
  :type '(choice (const :tag "No default deadline" nil) number)
  :group 'e)

(defun e-harness-context-runtime--profile-call (event options thunk)
  "Measure context EVENT with OPTIONS when profiling is available."
  (if (and (fboundp 'e-dev-profile-enabled-p)
           (fboundp 'e-dev-profile-measure-thunk)
           (e-dev-profile-enabled-p))
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-harness-context-runtime--merge-turn-options (base overrides)
  "Return BASE options with OVERRIDES applied."
  (let ((options (copy-sequence base))
        (remaining overrides))
    (while remaining
      (setq options (plist-put options (pop remaining) (pop remaining))))
    options))

(defun e-harness-session-options (harness session-id)
  "Return session-specific turn options for SESSION-ID in HARNESS."
  (e-session-turn-options (e-harness-sessions harness) session-id))

(defun e-harness-display-options (harness session-id)
  "Return lightweight display options for HARNESS SESSION-ID.
This merges default and session options without deriving prompt-cache keys or
materializing tool definitions.  Presentation code uses it for status text."
  (e-harness-context-runtime--merge-turn-options
   (e-harness-default-options harness)
   (e-harness-session-options harness session-id)))

(defun e-harness-context-runtime--set-session-options (harness session-id options)
  "Replace SESSION-ID turn OPTIONS in HARNESS and emit an update event."
  (let ((turn-options
         (e-session-set-turn-options
          (e-harness-sessions harness)
          session-id
          options)))
    (e-harness-activity-emit
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
    (e-harness-context-runtime--set-session-options harness session-id options)))

(defun e-harness-set-session-options (harness session-id options)
  "Replace SESSION-ID's complete turn-options plist with OPTIONS.
This semantic session operation is used by child-session setup and keeps option
mutation in the context/runtime owner rather than exposing its implementation
helper through the facade."
  (e-harness-context-runtime--set-session-options harness session-id options))

(defun e-harness-set-session-reasoning-effort (harness session-id effort)
  "Set SESSION-ID's reasoning EFFORT override in HARNESS."
  (let ((options (copy-sequence (e-harness-session-options harness session-id))))
    (if (and (stringp effort) (not (string-empty-p (string-trim effort))))
        (setq options (plist-put options :reasoning-effort (string-trim effort)))
      (cl-remf options :reasoning-effort))
    (e-harness-context-runtime--set-session-options harness session-id options)))

(defconst e-harness-prompt-cache-key-version "pcctx2"
  "Version marker for derived prompt cache keys.")

(defconst e-harness-prompt-cache-key-max-length 64
  "Maximum length of an OpenAI-compatible prompt cache key.")

(defun e-harness-context-runtime--prompt-cache-hash (value length)
  "Return a deterministic LENGTH-character hash for VALUE."
  (substring (secure-hash 'sha256 (format "%S" value)) 0 length))

(defun e-harness-context-runtime--effective-layer-id-strings (harness &optional session-id turn-id)
  "Return JSON-stable effective layer ids for HARNESS SESSION-ID TURN-ID."
  (mapcar #'symbol-name
          (e-harness-effective-layer-ids harness session-id turn-id)))

(defun e-harness-context-runtime--derived-prompt-cache-key (harness session-id options)
  "Return the default prompt cache key for HARNESS SESSION-ID OPTIONS.
The active tool set participates in the key so mid-session tool activation
\(e.g. MCP progressive disclosure) does not silently reuse a cache prefix
built without those tools."
  (let* ((prefix (format "e:%s:" e-harness-prompt-cache-key-version))
         (identity
          (list :model (plist-get options :model)
                :project-root (e-harness-project-root harness session-id)
                :layer-ids
                (e-harness-effective-layer-ids harness session-id)
                :tool-definitions
                (mapcar
                 (lambda (tool)
                   (list :name (plist-get tool :name)
                         :fingerprint
                         (e-tools-definition-fingerprint tool)))
                 (or (plist-get options :tools)
                     (e-tools-definitions
                      (e-harness-tools harness session-id)))))))
    (concat prefix
            (e-harness-context-runtime--prompt-cache-hash
             identity
             (- e-harness-prompt-cache-key-max-length (length prefix))))))

(defun e-harness-context-runtime--apply-prompt-cache-defaults (harness session-id options)
  "Apply opt-in prompt cache defaults to OPTIONS."
  (let ((options (copy-sequence options)))
    (when (and (plist-member options :prompt-cache-key)
               (null (plist-get options :prompt-cache-key)))
      (cl-remf options :prompt-cache-key))
    (when (and (plist-get options :prompt-cache-default)
               (not (plist-member options :prompt-cache-key)))
      (setq options
            (plist-put options
                       :prompt-cache-key
                       (e-harness-context-runtime--derived-prompt-cache-key
                        harness
                        session-id
                        options))))
    (cl-remf options :prompt-cache-default)
    (unless (plist-member options :prompt-cache-key)
      (cl-remf options :prompt-cache-retention))
    options))

(defun e-harness-context-runtime--apply-deadline-default (options)
  "Attach a provider request deadline to OPTIONS when none is explicit."
  (let ((options (copy-sequence options)))
    (when (and e-harness-provider-request-deadline-seconds
               (not (plist-member options :deadline)))
      (setq options
            (plist-put options
                       :deadline
                       (+ (float-time)
                          e-harness-provider-request-deadline-seconds))))
    options))

(defun e-harness-turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID.
Tool definitions are attached before deriving the prompt cache key so the key
reflects the active tool set."
  (let* ((merged (e-harness-context-runtime--merge-turn-options
                  (e-harness-default-options harness)
                  (e-harness-session-options harness session-id)))
         (tool-definitions
          (e-tools-definitions (e-harness-tools harness session-id)))
         (with-tools (if tool-definitions
                         (plist-put merged :tools tool-definitions)
                       merged)))
    (e-harness-context-runtime--apply-deadline-default
     (e-harness-context-runtime--apply-prompt-cache-defaults harness session-id with-tools))))

(defun e-harness-context-runtime--turn-options (harness session-id)
  "Return backend-neutral turn options for HARNESS and SESSION-ID."
  (e-harness-turn-options harness session-id))

(defun e-harness-context-options-without-tools (options)
  "Return OPTIONS with any tool set removed.
Used for backend requests that must produce plain text (e.g. context
compaction) where exposing tools risks a tool-call instead of a reply."
  (let ((copy (copy-sequence options)))
    (setq copy (plist-put copy :tools nil))
    copy))
(defun e-harness-context
    (harness session-id &optional turn-id context-purpose)
  "Return backend-neutral context for SESSION-ID in HARNESS.
TURN-ID is passed to active capability context providers when present.
CONTEXT-PURPOSE may be `turn' for correctness-critical provider turns,
`preview' for explicit user-requested context inspection, or `status',
`snapshot', or `optional' for callers that must not perform correctness-critical
turn context work."
  (e-harness-context-runtime--profile-call
   'harness.context
   (list :session-id session-id
         :turn-id turn-id
         :metadata (list :context-purpose context-purpose))
   (lambda ()
     (let ((capability-context
            (e-capabilities-context
             (e-harness-effective-capabilities harness session-id turn-id)
             :harness harness
             :session-id session-id
             :turn-id turn-id
             :context-purpose context-purpose)))
       (let* ((turn-options
               (e-harness-context-runtime--strip-reserved-derived-context-options
                (e-harness-turn-options harness session-id)))
         (context-capabilities
               (e-harness-context-capabilities harness turn-options))
              (context
               (e-context-build
                (e-harness-context-strategy harness)
                :sessions (e-harness-sessions harness)
                :session-id session-id
                :options turn-options
                :prefix-messages (plist-get capability-context :messages)
                :prefix-segments (plist-get capability-context :segments))))
         (when (e-harness-context-runtime--context-lifetime-enabled-p context-purpose)
           (setq context
                 (e-harness-context-lifetime-apply-projection
                  harness session-id turn-id context context-capabilities)))
         (plist-put context
                    :provider-anchor-active-layer-ids
                    (e-harness-context-runtime--effective-layer-id-strings
                     harness session-id turn-id))
         (plist-put context
                    :provider-anchor-compaction-boundary
                    (e-harness-context-runtime--provider-anchor-compaction-boundary
                     harness session-id))
         (e-harness-context-runtime--context-observation-frontier
          context
          context-capabilities)
         (e-harness-context-runtime--context-with-segment-message-boundary context)
         (setq context
               (e-harness-context-runtime--context-with-provider-anchor
                harness
                session-id
                context))
         (setq context
               (e-harness-context-runtime--context-with-provider-compaction
                harness session-id context context-purpose))
         (e-harness-context-runtime--context-with-continuation-projection-identity context))))))

(defun e-harness-turn-context (harness session-id turn-id)
  "Return correctness-critical model context for TURN-ID.
Unlike preview, status, snapshot, or optional context, turn context must include
the live dynamic providers needed for the model-facing request."
  (e-harness-context harness session-id turn-id 'turn))

(defun e-harness-context-runtime--provider-compaction-context
    (harness session-id generation)
  "Return a stable optional CONTEXT for provider compaction at GENERATION."
  (let ((context (e-harness-context harness session-id nil 'optional)))
    (plist-put context :lifetime-generation generation)
    (plist-put context
               :options
               (e-harness-context-runtime--strip-reserved-derived-context-options
                (plist-get context :options)))
    context))

(defun e-harness-context-runtime--provider-compaction-messages
    (harness session-id generation)
  "Return portable messages covered by GENERATION for provider compaction."
  (e-compaction-portable-context-messages
   (e-harness-sessions harness)
   session-id
   (e-context-lifetime-generation-checkpoint generation)
   (e-context-lifetime-generation-covered-session-boundary generation)))

(defun e-harness-context-runtime--provider-compaction-input
    (harness session-id generation)
  "Capture provider compaction MESSAGES and their exact coverage identity."
  (let* ((messages (e-harness-context-runtime--provider-compaction-messages
                    harness session-id generation))
         (projection (e-session-context-lifetime-projection
                      (e-harness-sessions harness) session-id))
         ;; The projection's ordered frontier covers both read-only v2
         ;; records and active v3 curations.  Do not discard v3 record ids by
         ;; deriving the frontier from the legacy promotion view.
         (frontier (copy-sequence (plist-get projection :promotion-frontier)))
         (source-entry-id
          (e-harness-context-runtime--provider-compaction-candidate-source-entry-id
           harness session-id)))
    (list :messages messages
          :source-entry-id source-entry-id
          :promotion-frontier frontier
          :input-fingerprint
          (secure-hash 'sha256
                       (prin1-to-string
                        (list messages source-entry-id frontier))))))

(defun e-harness-context-runtime--provider-compaction-store-result
    (harness session-id context generation result input)
  "Store validated provider RESULT as a runtime candidate."
  (e-harness-context-runtime--provider-compaction-store-candidate
   harness session-id context generation
   (e-backend-provider-compaction-result-output result)
   (e-backend-provider-compaction-result-usage result)
   (plist-get input :source-entry-id)
   (plist-get input :promotion-frontier)
   (plist-get input :input-fingerprint)))

(defun e-harness-context-provider-compaction-batch
    (harness session-id portable-generation)
  "Best-effort synchronous provider compaction after PORTABLE-GENERATION.

Canonical local compaction is already complete when this function runs.  Any
provider failure only discards acceleration and never changes session state."
  (when portable-generation
    (condition-case _error
        (let* ((generation
                (e-context-lifetime-generation-from-record
                 (e-session-aggregate-context-record portable-generation)))
               (context
                (e-harness-context-runtime--provider-compaction-context
                 harness session-id generation))
               (options (plist-get context :options))
               (capabilities (e-harness-context-capabilities harness options)))
          (when (eq (plist-get capabilities :provider-compaction) 'opaque)
            (let ((input (e-harness-context-runtime--provider-compaction-input
                          harness session-id generation)))
              (e-harness-context-runtime--provider-compaction-store-result
               harness session-id context generation
               (e-backend-provider-compaction-batch
                (e-harness-backend harness)
                :messages (plist-get input :messages)
                :options (plist-put (copy-sequence options)
                                    :provider-compaction-boundary t))
               input))))
      (error nil))))

(defun e-harness-context-provider-compaction-start
    (harness session-id portable-generation)
  "Best-effort asynchronous provider compaction after PORTABLE-GENERATION."
  (when portable-generation
    (condition-case _error
        (let* ((generation
                (e-context-lifetime-generation-from-record
                 (e-session-aggregate-context-record portable-generation)))
               (context
                (e-harness-context-runtime--provider-compaction-context
                 harness session-id generation))
               (options (plist-get context :options))
               (capabilities (e-harness-context-capabilities harness options)))
          (when (eq (plist-get capabilities :provider-compaction) 'opaque)
            (let ((input (e-harness-context-runtime--provider-compaction-input
                          harness session-id generation)))
              (e-backend-provider-compaction-start
               (e-harness-backend harness)
               :messages (plist-get input :messages)
               :options (plist-put (copy-sequence options)
                                   :provider-compaction-boundary t)
               :on-done
               (lambda (result)
                 (e-harness-context-runtime--provider-compaction-store-result
                  harness session-id context generation result input))
               :on-error #'ignore))))
      (error nil))))

(defun e-harness-context-capabilities (harness options)
  "Return normalized semantic context capabilities for OPTIONS.

The harness asks the backend at the provider-neutral boundary.  It stores the
answer on the request context, but does not interpret provider wire fields or
profile names."
  (let ((backend (e-harness-backend harness)))
    (if (e-backend-p backend)
        (e-backend-context-capabilities backend options)
      (e-backend-default-context-capabilities))))

(defun e-harness-context-runtime--context-lifetime-enabled-p (context-purpose)
  "Return non-nil when semantic lifetime projection is opted in for PURPOSE.

The feature is deliberately limited to correctness-critical turn context.  A
preview/status caller must not create a consumer frame or append a generation
just because the global opt-in is enabled."
  (and e-context-lifetime-shadow-projection-enabled
       (eq context-purpose 'turn)))

(defun e-harness-context-runtime--context-lifetime-ensure-generation (harness session-id)
  "Return SESSION-ID's current v2 generation, creating its first boundary."
  (or (e-session-context-lifetime-current-generation
       (e-harness-sessions harness) session-id)
      (let* ((store (e-harness-sessions harness))
             (session (e-session-get store session-id))
             (boundary (or (plist-get session :current-head-id)
                           (plist-get session :root-event-id)))
             (generation
              (e-context-lifetime-generation-create
               :id (format "generation:%s" boundary)
               :checkpoint nil
               :covered-session-boundary boundary)))
        (e-session-append-context-generation store session-id generation)
        generation)))

(defun e-harness-context-runtime--context-lifetime-curation-marker-messages (frame)
  "Return provider-neutral marker messages for FRAME's curation sources.

The source values remain in their original provider-neutral messages so
ordinary tool-call/result envelopes retain their transport meaning.  The
additional late system messages contain only the detached core-owned marker;
trusted frame and provenance identities remain in FRAME for response-time
binding and never cross the model-facing boundary."
  (mapcar
   (lambda (source)
     (list :role 'system
           :content (copy-sequence (plist-get source :marker))))
   (e-context-lifetime-frame-curation-presentation frame)))

(defun e-harness-context-lifetime-present-tool-observation (payload)
  "Add the fresh tool source marker to both request projections in PAYLOAD.

The loop owns the two in-memory message sequences; the harness owns the
provider-neutral presentation.  Keep the raw tool envelope in place and add
only the marker immediately before its matching result so full/stateless and
continuation-delta requests expose the same label-to-frame binding."
  (let* ((frame (plist-get payload :frame))
         (previous-frame (plist-get payload :previous-frame))
         (previous-presentation
          (when (and (e-context-lifetime-frame-p previous-frame)
                     (not (e-context-lifetime-frame-consumed-p previous-frame)))
            (e-context-lifetime-frame-curation-presentation previous-frame)))
         (previous-count (length previous-presentation))
         (source
          (nth previous-count
               (e-context-lifetime-frame-curation-presentation frame)))
         (marker (and source
                      (list :role 'system
                            :content (copy-sequence
                                      (plist-get source :marker)))))
         (message (plist-get payload :message))
         (tool-call-id
          (plist-get (plist-get message :content) :tool-call-id)))
    (unless (and marker (e-context-lifetime-frame-p frame))
      (signal 'e-context-lifetime-invalid-record
              (list 'curation-source :missing-tool-presentation)))
    (cl-labels
        ((insert-marker (messages)
           (let (result found)
             (dolist (candidate messages)
               (if (and (not found)
                        (eq (plist-get candidate :role) 'tool)
                        (or (eq candidate message)
                            (and tool-call-id
                                 (equal
                                  (plist-get
                                   (plist-get candidate :content)
                                   :tool-call-id)
                                  tool-call-id))))
                   (progn
                     (unless (and result (equal (car result) marker))
                       (push (copy-tree marker) result))
                     (push candidate result)
                     (setq found t))
                 (push candidate result)))
             (unless found
               (signal 'e-context-lifetime-invalid-record
                       (list 'curation-source
                             :tool-result-presentation-target-missing
                             tool-call-id)))
             (nreverse result))))
      (list :turn-messages
            (insert-marker (plist-get payload :turn-messages))
            :provider-followup-messages
            (insert-marker
             (plist-get payload :provider-followup-messages))))))

(defun e-harness-context-lifetime-apply-projection
    (harness session-id turn-id context capabilities)
  "Apply the opted-in semantic projection to CONTEXT for TURN-ID.

The canonical session path supplies durable message bodies and promotions.  A
new runtime frame is captured for every invocation, even when the source
fingerprints happen to be unchanged."
  (let* ((store (e-harness-sessions harness))
         (projection (e-session-context-lifetime-projection store session-id))
         (generation (or (plist-get projection :generation)
                         (e-harness-context-runtime--context-lifetime-ensure-generation
                          harness session-id)))
         ;; The generation may have been created above; read the projection
         ;; again so the covered branch boundary and durable tail are current.
         (projection (if (plist-get projection :generation)
                         projection
                       (e-session-context-lifetime-projection
                        store session-id)))
         (promotions (plist-get projection :promotions))
         ;; Session projection is the semantic authority for both the
         ;; temporary v2 compatibility projection and the literal v3 curation
         ;; messages.  Do not rebuild either representation in the harness.
         (promotion-messages
          (copy-tree (plist-get projection :promotion-messages)))
         (checkpoint
          (let ((value (and generation
                            (e-context-lifetime-generation-checkpoint
                             generation))))
            (cond
             ((null value) nil)
             ((and (listp value)
                   (e-context-lifetime--keyword-plist-p value))
              (list (copy-tree value)))
             ((listp value) (copy-tree value))
             (t (list (copy-tree value))))))
         (durable-tail
          (append (copy-tree (plist-get projection :durable-tail))
                  promotion-messages))
         (segments (plist-get context :segments))
         (consumer-request-id (format "consumer:%s:%s"
                                      turn-id (e-session-generate-ulid)))
         (frame-id (format "frame:%s" consumer-request-id))
         (frame
          (e-context-lifetime-frame-create-from-segments
           :id frame-id
           :generation-id (e-context-lifetime-generation-id generation)
           :consumer-request-id consumer-request-id
           :segments segments
           :observation-delivery
           (plist-get capabilities :observation-delivery)))
         ;; Capture the source markers before moving the raw observation
         ;; messages to the late frontier.  Markers are deliberately separate
         ;; system messages so tool/result envelopes remain intact and each
         ;; semantic source occurs exactly once.
         (curation-markers
          (e-harness-context-runtime--context-lifetime-curation-marker-messages frame))
         (observation-kinds e-context-lifetime-observation-kinds)
         (filtered-segments
          (let ((marker-tail (copy-tree curation-markers))
                (ordinary-segments nil)
                (observation-segments nil)
                (source-count 0))
            (dolist (segment segments)
              (let* ((kind (plist-get segment :kind))
                     (copy (copy-tree segment)))
                (cond
                 ((eq kind 'history)
                  ;; History remains in its canonical slot, with the accepted
                  ;; durable projection substituted for its body.
                  (plist-put copy :messages
                             (append checkpoint durable-tail))
                  (push copy ordinary-segments))
                 ((and curation-markers
                       (member (format "%s" kind) observation-kinds))
                  ;; Keep the original observation segment as the ownership
                  ;; boundary.  Only its model-facing message sequence is
                  ;; transformed; kind, id, fingerprint, and any other fields
                  ;; remain trusted segment metadata.  MARKER-TAIL is global so
                  ;; labels stay in canonical segment/message order even when
                  ;; observations span multiple segments.
                  (let (messages)
                    (dolist (source (plist-get segment :messages))
                      (setq source-count (1+ source-count))
                      (let ((marker (pop marker-tail)))
                        (unless marker
                          (signal 'e-context-lifetime-invalid-record
                                  (list 'curation-source
                                        :presentation-source-count-mismatch
                                        (length curation-markers)
                                        source-count)))
                        (setq messages
                              (append messages
                                      (list marker (copy-tree source))))))
                    (plist-put copy :messages messages)
                    (push copy observation-segments)))
                 (t
                  (push copy ordinary-segments)))))
            (unless (null marker-tail)
              (signal 'e-context-lifetime-invalid-record
                      (list 'curation-source
                            :presentation-source-count-mismatch
                            (length curation-markers)
                            source-count)))
            ;; Observation segments form one late frontier, but their original
            ;; kind-scoped ownership and canonical metadata are preserved.
            (append (nreverse ordinary-segments)
                    (nreverse observation-segments))))
         (messages
          (cl-loop for segment in filtered-segments
                   append (copy-tree (plist-get segment :messages))))
         (semantic-projection
          (e-context-lifetime-project
           generation frame
           :durable-tail durable-tail
           :static-prefix
           (cl-loop for segment in segments
                    when (eq (plist-get segment :kind) 'static-prefix)
                    append (copy-tree (plist-get segment :messages)))
           :stable-context
           (cl-loop for segment in segments
                    when (eq (plist-get segment :kind) 'stable-context)
                    append (copy-tree (plist-get segment :messages))))))
    (plist-put context :segments filtered-segments)
    (plist-put context :messages messages)
    (plist-put context :context-lifetime-enabled t)
    (plist-put context :lifetime-generation generation)
    (plist-put context :lifetime-frame frame)
    (plist-put context :lifetime-promotions promotions)
    (plist-put context :lifetime-projection semantic-projection)
    context))

(defun e-harness-context-runtime--lifetime-response-entry-id
    (harness session-id turn-id &optional response-entry-id)
  "Return the durable response entry identity for TURN-ID.

RESPONSE-ENTRY-ID is allocated before a non-tool completion preflight and is
the identity the later assistant append must preserve.  Otherwise resolve the
already-appended assistant or tool-call entry.  There is no provider-request
identity fallback: a non-tool completion must carry its durable entry ID."
  (or response-entry-id
      (plist-get
       (car (last
             (seq-filter
              (lambda (entry)
                (and (eq (plist-get entry :type) 'message)
                     (equal (plist-get entry :turn-id) turn-id)
                     (memq (plist-get entry :role)
                           '(assistant tool-call))))
              (e-session-current-path (e-harness-sessions harness)
                                      session-id))))
       :id)
      nil))

(defun e-harness-context-lifetime-preflight-response
    (harness session-id turn-id active-entry payload)
  "Return a pure curation completion value for PAYLOAD.

Resolve the provider-request frame before any assistant message is appended.
  The returned value contains the trusted frame, response binding, and optional
  prepared promotion/erasure package; it does not append to the session or
  consume the frame.  Omitted source labels remain ordinary omission and are
  not represented as a durable curation decision."
  (when (and (e-context-lifetime-shadow-enabled-p)
             (e-harness-turn-state-active-turn-running-p active-entry))
    (let* ((payload-frame (plist-get payload :frame))
           (active-frame (e-harness-turn-state-context-frame active-entry))
           (effects (plist-get payload :curation-effects))
           (assistant-content (plist-get payload :assistant-content))
           (reserved-response-p
            (and effects
                 (not (plist-get payload :tool-called))
                 (or (null assistant-content)
                     (and (stringp assistant-content)
                          (string-empty-p assistant-content)))))
           ;; A present payload frame is authoritative: a newer descendant
           ;; may have been installed by an ordinary tool call, but it was not
           ;; presented in this provider request.  The active entry is only a
           ;; fallback for synthetic callers that omit the payload frame.
           (frame (or payload-frame active-frame))
           (response-id
            (if reserved-response-p
                (or (plist-get payload :response-entry-id)
                    (e-session-generate-ulid))
              (e-harness-context-runtime--lifetime-response-entry-id
               harness session-id turn-id
               (and (not (plist-get payload :tool-called))
                    (plist-get payload :response-entry-id))))))
      (unless (or (null effects) (= (length effects) 1))
        (signal 'e-context-lifetime-invalid-record
                (list 'curation :effect-count (length effects))))
      (when (and effects
                 (not (and (e-context-lifetime-frame-p frame)
                           (not (e-context-lifetime-frame-consumed-p frame)))))
        (signal 'e-context-lifetime-invalid-record
                (list 'curation :frame-not-live)))
      (let ((prepared
             (when (= (length effects) 1)
               (e-context-lifetime-prepare-curation-disposition
                frame
                (plist-get (car effects) :arguments)
                response-id))))
        (list :frame frame
              :consumer-request-id
              (and (e-context-lifetime-frame-p frame)
                   (e-context-lifetime-frame-consumer-request-id frame))
              :response-id response-id
              :reserved-response-p reserved-response-p
              :record (plist-get prepared :record)
              :erasure-record (plist-get prepared :erasure-record)
              :package (plist-get prepared :package)
              :curation-preparation prepared)))))

(defun e-harness-context-lifetime-commit-response
    (harness session-id turn-id active-entry payload)
  "Complete the runtime frame in PAYLOAD and append valid curation.

The loop has already validated the effect shape while streaming.  A pure
preflight value from `e-harness-context-lifetime-preflight-response' is
consumed when present; direct synthetic callers without that value are
preflighted here.
For a reserved-only response, the session first appends its semantic package,
then its separate audit control using the preflight response id.  The session
append precedes frame consumption and the next provider request."
  (when (and (e-context-lifetime-shadow-enabled-p)
             (e-harness-turn-state-active-turn-running-p active-entry))
    (let* ((preflight
            (if (plist-member payload :curation-preflight)
                (plist-get payload :curation-preflight)
              (e-harness-context-lifetime-preflight-response
               harness session-id turn-id active-entry payload)))
           (frame (plist-get preflight :frame))
           (response-id (plist-get preflight :response-id))
           (record (plist-get preflight :record))
           (package (plist-get preflight :package))
           (curation-projection
            (e-context-lifetime-curation-activity-projection
             (plist-get preflight :curation-preparation))))
      (when (and frame (e-context-lifetime-frame-p frame)
                 (not (e-context-lifetime-frame-consumed-p frame)))
        (let* ((consumer-id
                (or (plist-get preflight :consumer-request-id)
                    (e-context-lifetime-frame-consumer-request-id frame)))
               (curation-id (and record (plist-get record :id)))
               (appended
                (progn
                  ;; Semantic promotion/erasure components share one session
                  ;; package write.  The audit control remains a separate
                  ;; activity entry and is emitted only after that package is
                  ;; accepted.
                  (when package
                    (e-session-append-context-curation-package
                     (e-harness-sessions harness) session-id package
                     :write-index nil))
                  (when (plist-get preflight :reserved-response-p)
                    (e-session-append-context-curation-response
                     (e-harness-sessions harness) session-id turn-id response-id
                     :write-index nil))
                  package))
               (consumed
                (e-context-lifetime-frame-complete-for-consumer
                 frame consumer-id response-id
                 (and record appended (list curation-id)))))
          ;; Do not replace a newer descendant frame with the producer's
          ;; consumed snapshot.  The loop uses the returned value for the
          ;; provider request that completed; the turn owner decides whether
          ;; to install it on its active entry after this policy result
          ;; returns.  Keeping the write there prevents context policy from
          ;; mutating turn-state representation.
          (e-harness-activity-emit-turn-event
           harness session-id turn-id 'context-frame-consumed
           (list :frame-id (e-context-lifetime-frame-id consumed)
                 :consumer-request-id consumer-id
                 :response-entry-id response-id
                 :curation-ids (and record appended (list curation-id))
                 :curation curation-projection))
          consumed)))))

(defun e-harness-context-lifetime-tool-observation-frame
    (harness session-id turn-id active-entry payload)
  "Return a fresh consumer-bound frame for one tool result PAYLOAD."
  (when (and e-context-lifetime-shadow-projection-enabled
             (e-harness-turn-state-active-turn-running-p active-entry))
    (let* ((previous (plist-get payload :previous-frame))
           (generation (or (and previous
                                (e-session-context-lifetime-current-generation
                                 (e-harness-sessions harness) session-id))
                           (e-harness-turn-state-lifetime-generation active-entry)
                           (e-harness-context-runtime--context-lifetime-ensure-generation
                            harness session-id)))
           (tool-call (plist-get payload :tool-call))
           (result (plist-get payload :result))
           (message (plist-get payload :message))
           (tool-id (or (plist-get tool-call :id)
                        (plist-get result :tool-call-id)
                        (e-session-generate-ulid)))
           (existing-observations
            (and previous
                 (not (e-context-lifetime-frame-consumed-p previous))
                 (e-context-lifetime-frame-observations previous)))
           (consumer-id
            (or (and existing-observations
                     (e-context-lifetime-frame-consumer-request-id previous))
                (format "consumer:%s:tool:%s"
                        turn-id (e-session-generate-ulid))))
           (body (list :tool-call (copy-tree tool-call)
                       :tool-result (copy-tree result)
                       :message-id (plist-get message :id)))
           (observation
            (list :observation-id (format "observation:tool-bundle:%s" tool-id)
                  :kind "tool-result"
                  :source-entry-ref
                  (or (plist-get message :id)
                      (format "external:tool-result:%s" tool-id))
                  :source-fingerprint
                  (secure-hash 'sha256 (prin1-to-string body))
                  :effective-delivery "inherited"
                  :body body))
           (observations (append (copy-tree existing-observations)
                                 (list observation)))
           (frame
            (e-context-lifetime-frame-create
             :id (format "frame:%s:%s"
                         consumer-id
                         (substring (secure-hash 'sha256
                                                  (prin1-to-string
                                                   (mapcar
                                                    (lambda (item)
                                                      (plist-get item
                                                                 :observation-id))
                                                    observations)))
                                    0 16))
             :generation-id
             (e-context-lifetime-generation-id generation)
             :consumer-request-id consumer-id
             :observations observations)))
      frame)))

(defconst e-harness-context-runtime--reserved-derived-context-option-keys
  '(:context-segment-message-count
    :replaceable-current-state-partitioned
    :context-lifetime-enabled
    :provider-anchor
    :provider-anchor-delta-messages
    :provider-anchor-source-message-count
    :provider-compaction-output
    :provider-compaction-delta-messages
    :provider-compaction-source-entry-id
    :provider-compaction-generation-id
    :provider-compaction-fingerprint
    :provider-compaction-invalidation-reason)
  "Context options owned by the harness rather than callers.

These values are derived from the semantic projection at request construction.
They must not be inherited from defaults, session options, or a provider-facing
caller because such values could forge or stale the frontier partition.")

(defun e-harness-context-runtime--strip-reserved-derived-context-options (options)
  "Return OPTIONS without harness-derived context partition markers."
  (let ((clean (copy-sequence options)))
    (dolist (key e-harness-context-runtime--reserved-derived-context-option-keys clean)
      (cl-remf clean key))))

(defconst e-harness-context-runtime--provider-anchor-derived-context-option-keys
  '(:provider-anchor
    :provider-anchor-delta-messages
    :provider-anchor-source-message-count)
  "Provider-anchor fields derived from the session-owned anchor.")

(defun e-harness-context-runtime--strip-provider-anchor-derived-context-options (options)
  "Return OPTIONS without provider-anchor-derived correctness state."
  (let ((clean (copy-sequence options)))
    (dolist (key e-harness-context-runtime--provider-anchor-derived-context-option-keys clean)
      (cl-remf clean key))))

(defun e-harness-context-runtime--context-observation-frontier (context capabilities)
  "Attach semantic observation metadata to CONTEXT for CAPABILITIES.

The frontier is kind-scoped.  Only observations whose own capability entry is
proven replaceable may be removed from a provider continuation; a replaceable
canvas never authorizes dropping an inherited tool result or trace."
  (let* ((options
          (e-harness-context-runtime--strip-reserved-derived-context-options
           (plist-get context :options)))
         (delivery-map (plist-get capabilities :observation-delivery))
         (delivery (e-backend-observation-delivery-for-kind
                    capabilities 'current-state))
         (messages (e-context-current-state-messages context))
         (fingerprint (and messages
                           (e-context-current-state-fingerprint context)))
         (observations
          (cl-loop for segment in (plist-get context :segments)
                   for kind = (plist-get segment :kind)
                   when (memq kind '(current-state dynamic-context))
                   collect
                   (list :kind kind
                         :delivery
                         (e-backend-observation-delivery-for-kind
                          capabilities kind)
                         :messages (copy-tree (plist-get segment :messages))
                         :fingerprint (plist-get segment :fingerprint))))
         (frontier (list :delivery delivery
                         :delivery-map (copy-tree delivery-map)
                         :messages (copy-tree messages)
                         :fingerprint fingerprint
                         :observations observations))
         (frame (plist-get context :lifetime-frame))
         (clean-p
          (or (null frame)
              (cl-every
               (lambda (observation)
                 (equal (plist-get observation :effective-delivery)
                        "request-local-replaceable"))
               (e-context-lifetime-frame-observations frame)))))
    (setq options (plist-put options :context-capabilities
                             (copy-sequence capabilities)))
    (setq options (plist-put options :observation-delivery delivery))
    (setq options (plist-put options :observation-delivery-map
                             (copy-tree delivery-map)))
    (setq options (plist-put options :observation-frontier frontier))
    (setq options (plist-put options :lifetime-ephemerals-clean-p clean-p))
    (when (plist-get context :context-lifetime-enabled)
      (setq options (plist-put options :context-lifetime-enabled t)))
    (when (eq delivery 'request-local-replaceable)
      (setq options
            (plist-put options :replaceable-current-state
                       (copy-tree messages))))
    (when fingerprint
      (setq options
            (plist-put options :current-state-fingerprint fingerprint)))
    (plist-put context :options options)
    (plist-put context :observation-frontier frontier)
    context))

(defun e-harness-context-runtime--context-with-segment-message-boundary (context)
  "Attach the exact message coverage represented by CONTEXT segments.

Only a complete semantic segment projection receives the reserved derived
boundary.  In-turn continuation deltas are already partitioned by the loop's
lexical ownership and do not use this marker; a partial segment list remains
ambiguous and is rejected by the provider adapter."
  (let* ((messages (plist-get context :messages))
         (segments (plist-get context :segments))
         (segment-messages
          (cl-loop for segment in segments
                   append (copy-tree (plist-get segment :messages))))
         (segment-message-count (length segment-messages))
         ;; Counting alone is insufficient: a hostile/stale segment list could
         ;; have the right length while describing different messages.  The
         ;; harness owns both sides of this comparison and only then derives
         ;; the reserved prefix boundary.
         (exact-coverage-p (and segments
                                (equal segment-messages messages)))
         (options
          (e-harness-context-runtime--strip-reserved-derived-context-options
           (plist-get context :options))))
    ;; The boundary pass removes caller-supplied derived values, but the
    ;; semantic lifetime fields are re-derived from the trusted runtime
    ;; projection rather than allowed to disappear between frontier and
    ;; adapter construction.
    (when (plist-get context :context-lifetime-enabled)
      (setq options (plist-put options :context-lifetime-enabled t)))
    (when exact-coverage-p
      (setq options
            (plist-put options
                       :context-segment-message-count
                       segment-message-count)))
    (plist-put context :options options)
    context))

(defun e-harness-context-runtime--context-curation-carrier-active-p (context)
  "Return non-nil when CONTEXT uses the reserved curation carrier.

The OpenAI capability advertises the reserved carrier even when the semantic
lifetime projection is disabled.  Treat the carrier as active only when the
trusted derived option enables the projection, unless a caller has explicitly
provided the carrier option itself."
  (let* ((options (plist-get context :options))
         (capabilities (plist-get options :context-capabilities))
         (carrier
          (if (plist-member options :reserved-effect-carrier)
              (plist-get options :reserved-effect-carrier)
            (and (plist-get options :context-lifetime-enabled)
                 (plist-get capabilities :reserved-effect-carrier)))))
    (eq carrier 'context-curate-wire)))

(defun e-harness-context-runtime--provider-anchor-fingerprints (context)
  "Return JSON-stable provider-relevant fingerprints from CONTEXT."
  (let* ((options (plist-get context :options))
         (capabilities (plist-get options :context-capabilities))
         (delivery (plist-get options :observation-delivery))
         (delivery-map (or (plist-get options :observation-delivery-map)
                           (plist-get capabilities :observation-delivery)))
         (fingerprints
          (list
           :segments
           (mapcar
            (lambda (segment)
              (list :kind (symbol-name (plist-get segment :kind))
                    :id (prin1-to-string (plist-get segment :id))
                    :fingerprint (plist-get segment :fingerprint)))
            (cl-remove-if
             (lambda (segment)
               (or (memq (plist-get segment :kind) '(history delta))
                   (and (memq (plist-get segment :kind)
                              '(current-state dynamic-context))
                        (eq (e-backend-observation-delivery-for-kind
                             (list :observation-delivery delivery-map)
                             (plist-get segment :kind))
                            'request-local-replaceable))))
             (plist-get context :segments)))
           :active-layer-ids
           (copy-sequence (plist-get context :provider-anchor-active-layer-ids))
           :tools
           (mapcar
            (lambda (tool)
              (list :name (plist-get tool :name)
                    :fingerprint
                    (secure-hash 'sha256 (prin1-to-string tool))))
            (plist-get options :tools))
           :reasoning
           (let ((reasoning
                  (list :reasoning (plist-get options :reasoning)
                        :reasoning-effort (plist-get options :reasoning-effort)
                        :effort (plist-get options :effort))))
             (when (plist-member options :reasoning-summary)
               (setq reasoning
                     (plist-put reasoning :reasoning-summary
                                (plist-get options :reasoning-summary))))
             reasoning)
           :provider-options
           (list :instructions (plist-get options :instructions)
                 :max-tokens (plist-get options :max-tokens)
                 :prompt-cache (plist-get options :prompt-cache)
                 :prompt-cache-mode (plist-get options :prompt-cache-mode)
                 :prompt-cache-ttl (plist-get options :prompt-cache-ttl)
                 :prompt-cache-key (plist-get options :prompt-cache-key)
                 :prompt-cache-retention (plist-get options :prompt-cache-retention)
                 :anthropic-container-id (plist-get options :anthropic-container-id)
                 :anthropic-context-management
                 (plist-get options :anthropic-context-management)
                 :anthropic-beta-headers
                 (plist-get options :anthropic-beta-headers))
           :compaction-boundary
           (plist-get context :provider-anchor-compaction-boundary)
           :lifetime-generation
           (when-let ((generation (plist-get context :lifetime-generation)))
             (list :id (e-context-lifetime-generation-id generation)
                   :covered-session-boundary
                   (e-context-lifetime-generation-covered-session-boundary
                    generation)
                   :checkpoint-fingerprint
                   (secure-hash
                    'sha256
                    (prin1-to-string
                     (e-context-lifetime-generation-checkpoint generation)))))
           ;; Keep semantic control values JSON-stable.  The observation value
           ;; itself is compared only for inherited delivery; a replaceable
           ;; observation may change without invalidating the stable anchor.
           :observation-delivery
           (and delivery (symbol-name delivery))
           :observation-delivery-map
           (copy-tree delivery-map)
           :reserved-effect-carrier
           (plist-get capabilities :reserved-effect-carrier)
           :lifetime-observation-safety
           (if (plist-get options :lifetime-ephemerals-clean-p)
               'clean
             'contaminated))))
    (when (e-harness-context-runtime--context-curation-carrier-active-p context)
      (setq fingerprints
            (plist-put
             fingerprints
             :context-curation-revision-identity
             (e-context-lifetime-curation-revision-identity))))
    (when (and (not (eq delivery 'request-local-replaceable))
               (plist-get options :current-state-fingerprint))
      (setq fingerprints
            (plist-put fingerprints
                       :current-state-fingerprint
                       (plist-get options :current-state-fingerprint))))
    fingerprints))

(defun e-harness-context-runtime--context-with-continuation-projection-identity
    (context)
  "Attach the stable continuation identity for CONTEXT's request projection.

The identity describes the semantic input that a provider continuation stores:
stable segments, active tools/layers, provider options, compaction boundary,
and capabilities.  A proven request-local current-state frontier is
deliberately excluded by
`e-harness-context-runtime--provider-anchor-fingerprints'.  The loop uses this
opaque provider-neutral value to fence a response candidate when a same-turn
refresh changes the projection that produced it."
  (let* ((options (copy-sequence (plist-get context :options)))
         (capabilities (plist-get options :context-capabilities))
         (identity
          (list
           :provider-anchor-provider-id
           (plist-get options :provider-anchor-provider-id)
           :model (plist-get options :model)
           :provider-continuation
           (plist-get options :provider-continuation)
           :context-capabilities
           (copy-tree capabilities)
           :provider-anchor-fingerprints
           (e-harness-context-runtime--provider-anchor-fingerprints context))))
    (plist-put context
               :options
               (plist-put options
                          :continuation-projection-identity
                          identity))))

(defun e-harness-context-runtime--provider-anchor-compaction-boundary (harness session-id)
  "Return provider-anchor compatibility data for latest compaction boundary."
  (when-let ((compaction
              (e-session-latest-valid-compaction
               (e-harness-sessions harness)
               session-id)))
    (list :id (plist-get compaction :id)
          :first-kept-entry-id (plist-get compaction :first-kept-entry-id))))

(defun e-harness-context-runtime--provider-anchor-invalidation-reason
    (harness session-id provider-id model fingerprints)
  "Return the most relevant provider-anchor invalidation reason."
  (let* ((anchors
          (cl-remove-if-not
           (lambda (anchor)
             (eq (plist-get anchor :provider-id) provider-id))
           (e-session-provider-anchors (e-harness-sessions harness) session-id)))
         (latest (car (last anchors))))
    (if latest
        (e-session-provider-anchor-incompatibility-reason
         (e-harness-sessions harness)
         session-id
         latest
         provider-id
         model
         fingerprints)
      'missing-anchor)))

(defun e-harness-context-runtime--provider-anchor-dynamic-context-messages (context)
  "Return backend-neutral dynamic-context messages from CONTEXT."
  (unless (eq (plist-get (plist-get context :options)
                         :observation-delivery)
              'request-local-replaceable)
    (e-context-current-state-messages context)))

(defun e-harness-context-runtime--provider-anchor-delta-messages
    (harness session-id anchor &optional context)
  "Return backend-neutral fresh messages after ANCHOR coverage in SESSION-ID."
  (let ((dynamic-messages
         (when context
           (e-harness-context-runtime--provider-anchor-dynamic-context-messages context)))
        (entries (cdr (e-session-entries-from
                       (e-harness-sessions harness)
                       session-id
                       (plist-get anchor :covered-entry-id)))))
    (append
     dynamic-messages
    (if (plist-get context :context-lifetime-enabled)
        (delq nil
              (mapcar #'e-session-context-lifetime-durable-message
                      (cl-remove-if-not
                       (lambda (entry)
                         (eq (plist-get entry :type) 'message))
                       entries)))
      (mapcar #'e-context-backend-message
              (cl-remove-if-not
               (lambda (entry)
                 (eq (plist-get entry :type) 'message))
               entries))))))

(defun e-harness-context-runtime--provider-anchor-selection-allowed-p (options)
  "Return non-nil when OPTIONS can safely select a provider anchor.

Continuation is a semantic backend capability, not merely a request option.
An inherited current-state observation may only branch from a clean anchor when
the backend explicitly supports branchable continuation; a linear continuation
must reconstruct statelessly in that case.  A request-local replacement does
not contaminate the anchor and is safe for either supported continuation mode."
  (let* ((capabilities (plist-get options :context-capabilities))
         (continuation (plist-get capabilities :continuation))
         (delivery (plist-get options :observation-delivery))
         (current-state-fingerprint
          (plist-get options :current-state-fingerprint)))
    (and (plist-get options :provider-continuation)
         (memq continuation '(linear branchable))
         (or (not (plist-member options :lifetime-ephemerals-clean-p))
             (plist-get options :lifetime-ephemerals-clean-p)
             (eq continuation 'branchable))
         (or (null current-state-fingerprint)
             (eq delivery 'request-local-replaceable)
             (eq continuation 'branchable)))))

(defun e-harness-context-runtime--provider-anchor-lookup-fingerprints
    (context)
  "Return fingerprints used to select an anchor for CONTEXT.

An inherited observation may branch repeatedly only from a clean anchor.  For
a branchable backend, omit the current observation from the lookup identity;
this lets a clean anchor match while
`e-session-provider-anchor-incompatibility-reason' still rejects any
persisted anchor that carries a non-nil observation
fingerprint.  Linear backends keep the ordinary fingerprint and are rejected
by `e-harness-context-runtime--provider-anchor-selection-allowed-p' when an
observation is inherited."
  (let* ((options (plist-get context :options))
         (capabilities (plist-get options :context-capabilities))
         (fingerprints (e-harness-context-runtime--provider-anchor-fingerprints context))
         (inherited-observation-p
          (cl-some
           (lambda (observation)
             (equal (plist-get observation :delivery) 'inherited))
           (plist-get (plist-get context :observation-frontier)
                      :observations))))
    (if (and inherited-observation-p
             (eq (plist-get capabilities :continuation) 'branchable))
        (let ((lookup (copy-tree fingerprints)))
          ;; A clean anchor was produced before any current-state segment
          ;; existed.  Remove those volatile segment identities as well as the
          ;; separate value fingerprint; contaminated persisted anchors still
          ;; fail the non-nil fingerprint comparison below.
          (setq lookup
                (plist-put
                 lookup
                 :segments
                 (cl-remove-if
                  (lambda (segment)
                    (member (plist-get segment :kind)
                            '(current-state dynamic-context
                              "current-state" "dynamic-context")))
                  (plist-get lookup :segments))))
          (plist-put lookup :current-state-fingerprint nil))
      fingerprints)))

(defun e-harness-context-runtime--provider-anchor-safety (options)
  "Return the anchor advancement/safety diagnostic for OPTIONS."
  (let* ((capabilities (plist-get options :context-capabilities))
         (continuation (plist-get capabilities :continuation))
         (delivery (plist-get options :observation-delivery))
         (current-state-fingerprint
          (plist-get options :current-state-fingerprint)))
    (cond
     ((not (memq continuation '(linear branchable)))
      'hold-unavailable-capability)
     ((and current-state-fingerprint
           (eq delivery 'inherited)
           (eq continuation 'linear))
      'hold-inherited-observation)
     ((and current-state-fingerprint
           (eq delivery 'inherited)
           (eq continuation 'branchable))
      'branchable-clean-anchor-only)
     ((and (plist-member options :lifetime-ephemerals-clean-p)
           (not (plist-get options :lifetime-ephemerals-clean-p)))
      'hold-inherited-observation)
     (t
      'advance-eligible))))

(defun e-harness-context-runtime--provider-compaction-fingerprint (harness session-id context)
  "Return the runtime identity for an opaque compaction candidate.

The identity is derived from provider-neutral stable projection fields and the
active generation.  Replaceable current-state values are excluded by the
existing anchor fingerprint helper; opaque provider output is never hashed or
otherwise interpreted here."
  (let* ((options (plist-get context :options))
         (generation (or (plist-get context :lifetime-generation)
                         (e-session-context-lifetime-current-generation
                          (e-harness-sessions harness) session-id)))
         (identity
          (list :provider-id (plist-get options :provider-anchor-provider-id)
                :model (plist-get options :model)
                :capabilities (copy-tree
                               (plist-get options :context-capabilities))
                :anchor-fingerprints
                (e-harness-context-runtime--provider-anchor-fingerprints context)
                :generation-id
                (and generation
                     (e-context-lifetime-generation-id generation)))))
    (secure-hash 'sha256 (prin1-to-string identity))))

(defun e-harness-context-runtime--provider-compaction-candidate-source-entry-id
    (harness session-id)
  "Return the exact journal head captured by a candidate.

Entries after this identity are scanned separately when the candidate is
consumed, and only their portable durable message projection may become the
provider delta."
  (when-let ((entry (car (last (e-session-current-path
                               (e-harness-sessions harness) session-id)))))
    (plist-get entry :id)))

(defun e-harness-context-runtime--provider-compaction-delta-messages
    (harness session-id source-entry-id)
  "Return portable durable messages after SOURCE-ENTRY-ID.

Provider-compaction deltas are intentionally separate from provider-anchor
deltas.  Raw tool-call/tool/replay journal entries are not eligible, so an
opaque candidate cannot create an orphaned provider tool bundle."
  (let ((after nil)
        (found nil)
        result)
    (dolist (entry (e-session-current-path
                    (e-harness-sessions harness) session-id))
      (if after
          (when-let ((message
                      (e-session-context-lifetime-durable-message entry)))
            (push (e-context-lifetime-portable-message message) result))
        (when (equal (plist-get entry :id) source-entry-id)
          (setq after t
                found t))))
    (if found
        (nreverse result)
      nil)))

(defun e-harness-context-runtime--provider-compaction-store-candidate
    (harness session-id context generation output usage
             source-entry-id promotion-frontier input-fingerprint)
  "Install one runtime-only opaque provider candidate for SESSION-ID.

No session record is touched.  The candidate is fenced by GENERATION, the
  covered durable source entry, and the stable projection fingerprint; a later
  context rebuild either selects it exactly or discards it."
  (let* ((options (plist-get context :options))
         (current-generation
          (e-session-context-lifetime-current-generation
           (e-harness-sessions harness) session-id))
         (current-frontier
          (plist-get
           (e-session-context-lifetime-projection
            (e-harness-sessions harness) session-id)
           :promotion-frontier))
         (source-on-path
          (seq-some (lambda (entry)
                      (equal (plist-get entry :id) source-entry-id))
                    (e-session-current-path
                     (e-harness-sessions harness) session-id))))
    ;; A promotion frontier changing while the provider request is in flight
    ;; makes its opaque coverage ambiguous.  Keep portable context as the
    ;; correctness path and discard acceleration without mutating the session.
    (when (and source-entry-id generation current-generation
               (equal (e-context-lifetime-generation-id generation)
                      (e-context-lifetime-generation-id current-generation))
               source-on-path
               (equal promotion-frontier current-frontier)
               (stringp input-fingerprint))
      (puthash
       session-id
       (list :provider-id (plist-get options :provider-anchor-provider-id)
             :model (plist-get options :model)
             :generation-id (e-context-lifetime-generation-id generation)
             :covered-session-boundary
             (e-context-lifetime-generation-covered-session-boundary
              generation)
             :source-entry-id source-entry-id
             :promotion-frontier (copy-sequence promotion-frontier)
             :input-fingerprint input-fingerprint
             :fingerprint
             (e-harness-context-runtime--provider-compaction-fingerprint
              harness session-id context)
             :output output
             :usage usage)
       (e-harness-provider-compaction-candidates harness)))))

(defun e-harness-context-runtime--provider-compaction-candidate-compatible-p
    (harness session-id context candidate capabilities)
  "Return non-nil when runtime CANDIDATE is exact for CONTEXT."
  (let* ((options (plist-get context :options))
         (generation (plist-get context :lifetime-generation))
         (source-entry-id (plist-get candidate :source-entry-id))
         (path (e-session-current-path (e-harness-sessions harness) session-id)))
    (and (eq (plist-get capabilities :provider-compaction) 'opaque)
         (equal (plist-get candidate :provider-id)
                (plist-get options :provider-anchor-provider-id))
         (equal (plist-get candidate :model) (plist-get options :model))
         generation
         (equal (plist-get candidate :generation-id)
                (e-context-lifetime-generation-id generation))
         (seq-some (lambda (entry)
                     (equal (plist-get entry :id) source-entry-id))
                   path)
         (equal (plist-get candidate :promotion-frontier)
                (plist-get
                 (e-session-context-lifetime-projection
                  (e-harness-sessions harness) session-id)
                 :promotion-frontier))
         ;; Opaque compaction contains only durable context.  It cannot safely
         ;; replace an inherited observation frontier.
         (or (not (plist-member options :lifetime-ephemerals-clean-p))
             (plist-get options :lifetime-ephemerals-clean-p))
         (equal (plist-get candidate :fingerprint)
                (e-harness-context-runtime--provider-compaction-fingerprint
                 harness session-id context)))))

(defun e-harness-context-runtime--context-with-provider-compaction
    (harness session-id context &optional context-purpose)
  "Attach an exact runtime provider-compaction candidate to CONTEXT.

Candidates are selected only for the same provider/model/generation and stable
semantic fingerprint.  A mismatch is discarded and ordinary portable context
continues unchanged."
  (let* ((options (copy-sequence (plist-get context :options)))
         (capabilities (plist-get options :context-capabilities)))
    ;; Runtime provider state belongs to the correctness-critical request
    ;; path.  Preview/status/optional contexts must neither consume nor fence
    ;; the candidate that the next actual turn may use.
    (when (eq context-purpose 'turn)
      (let ((candidate
             (gethash session-id
                      (e-harness-provider-compaction-candidates harness))))
        (if (and candidate
                 (e-harness-context-runtime--provider-compaction-candidate-compatible-p
                  harness session-id context candidate capabilities))
            (let ((delta
                   (e-harness-context-runtime--provider-compaction-delta-messages
                    harness session-id
                    (plist-get candidate :source-entry-id))))
              (setq options
                    (plist-put options :provider-compaction-output
                               (plist-get candidate :output)))
              (setq options
                    (plist-put options :provider-compaction-delta-messages
                               delta))
              (setq options
                    (plist-put options :provider-compaction-source-entry-id
                               (plist-get candidate :source-entry-id)))
              (setq options
                    (plist-put options :provider-compaction-generation-id
                               (plist-get candidate :generation-id)))
              (setq options
                    (plist-put options :provider-compaction-fingerprint
                               (plist-get candidate :fingerprint)))
              (setq options
                    (plist-put options :context-rendering-strategy
                               'opaque-provider-compaction))
              ;; Opaque output is a one-shot runtime candidate.  A later
              ;; request must use a normal compatible anchor or portable
              ;; reconstruction, never replay the compact response.
              (remhash session-id
                       (e-harness-provider-compaction-candidates harness)))
          (when candidate
            (remhash session-id
                     (e-harness-provider-compaction-candidates harness))
            (setq options
                  (plist-put options :provider-compaction-invalidation-reason
                             'stale-or-incompatible-candidate))))))
    (plist-put context :options options)))

(defun e-harness-context-runtime--context-with-provider-anchor (harness session-id context)
  "Attach only a session-owned compatible provider anchor to CONTEXT.

Provider-anchor fields are harness-derived correctness state.  Clear all stale
or caller-supplied values before looking up the session-owned anchor so a
missing or incompatible anchor cannot accidentally preserve forged continuation
state across a context rebuild."
  (let* ((options
          (e-harness-context-runtime--strip-provider-anchor-derived-context-options
           (plist-get context :options)))
         (provider-id (plist-get options :provider-anchor-provider-id))
         (lookup-fingerprints
          (e-harness-context-runtime--provider-anchor-lookup-fingerprints context)))
    ;; Keep the cleaned options authoritative even when no provider anchor can
    ;; be selected.  The branches below may add only a session-owned anchor or
    ;; a diagnostic explaining why one was not selected.
    (plist-put context :options options)
    (when (and provider-id
               (e-harness-context-runtime--provider-anchor-selection-allowed-p options))
      (let ((anchor
             (e-session-latest-compatible-provider-anchor
              (e-harness-sessions harness)
              session-id
              provider-id
              :model (plist-get options :model)
              :fingerprints lookup-fingerprints))
            (options (copy-sequence options)))
        (if anchor
            (progn
              (setq options (plist-put options :provider-anchor anchor))
              (setq options
                    (plist-put
                     options
                     :provider-anchor-delta-messages
                     (e-harness-context-runtime--provider-anchor-delta-messages
                      harness session-id anchor context)))
              (setq options
                    (plist-put
                     options
                     :provider-anchor-source-message-count
                     (length (plist-get context :messages)))))
          (setq options
                (plist-put
                 options
                 :provider-anchor-invalidation-reason
                 (e-harness-context-runtime--provider-anchor-invalidation-reason
                  harness
                  session-id
                  provider-id
                  (plist-get options :model)
                  lookup-fingerprints))))
        (setq options
              (plist-put
               options
               :context-rendering-strategy
               (cond
                ((eq (plist-get options :observation-delivery)
                     'request-local-replaceable)
                 'replaceable-channel)
                (anchor 'clean-anchor-branch)
                (t 'stateless))))
        (plist-put context :options options)))
    (unless (e-harness-context-runtime--provider-anchor-selection-allowed-p options)
      (setq options (copy-sequence (plist-get context :options)))
      (setq options
            (plist-put options :provider-anchor-invalidation-reason
                       (cond
                        ((not (memq
                               (plist-get
                                (plist-get options :context-capabilities)
                                :continuation)
                               '(linear branchable)))
                         'continuation-capability-unavailable)
                        ((and (plist-get options :current-state-fingerprint)
                              (eq (plist-get options :observation-delivery)
                                  'inherited))
                         'inherited-observation-requires-branchable)
                        (t 'provider-continuation-disabled))))
      (setq options
            (plist-put options
                       :context-rendering-strategy
                       (if (eq (plist-get options :observation-delivery)
                               'request-local-replaceable)
                           'replaceable-channel
                         'stateless)))
      (plist-put context :options options))
    (let ((options (copy-sequence (plist-get context :options))))
      (setq options
            (plist-put options :provider-anchor-safety
                       (e-harness-context-runtime--provider-anchor-safety options)))
      (plist-put context :options options))
    context))

(defun e-harness-context-runtime--provider-anchor-candidate-persistable-p
    (context candidate final-request-ordinal)
  "Return non-nil when accepted CANDIDATE owns CONTEXT's final request.

The loop marks candidates only after projection-compatible promotion.  The
request ordinal and projection identity then fence a candidate from an
earlier request in the same turn, including the case where a refreshed
request emits no candidate at all."
  (let* ((provider-id (plist-get candidate :provider-id))
         (options (plist-get context :options))
         (frontier (plist-get options :observation-frontier))
         (inherited-observation-p
          (or (and (eq (plist-get options :observation-delivery) 'inherited)
                   (plist-get options :current-state-fingerprint))
              (cl-some
               (lambda (observation)
                 (eq (plist-get observation :delivery) 'inherited))
               (plist-get frontier :observations)))))
    (and (plist-get candidate :accepted-for-persistence)
         (plist-member candidate :projection-identity)
         (equal (plist-get candidate :projection-identity)
                (plist-get options :continuation-projection-identity))
         (equal (plist-get candidate :provider-request-ordinal)
                final-request-ordinal)
         provider-id
         (memq (plist-get (plist-get options :context-capabilities)
                          :continuation)
               '(linear branchable))
         (not inherited-observation-p)
         (or (not (plist-member options :lifetime-ephemerals-clean-p))
             (plist-get options :lifetime-ephemerals-clean-p))
         (pcase provider-id
           ('openai
            (and (plist-get options :provider-continuation)
                 (eq (plist-get options :provider-anchor-provider-id)
                     'openai)))
           (_ t)))))

(defun e-harness-context-runtime--latest-provider-anchor-candidates (candidates)
  "Return the latest provider anchor candidate per provider from CANDIDATES."
  (let ((latest-by-provider (make-hash-table :test #'equal))
        result)
    (dolist (candidate candidates)
      (puthash (plist-get candidate :provider-id)
               candidate
               latest-by-provider))
    (dolist (candidate candidates (nreverse result))
      (when (eq candidate
                (gethash (plist-get candidate :provider-id)
                         latest-by-provider))
        (push candidate result)))))

(defun e-harness-context-persist-provider-anchor-candidates
    (harness session-id turn-id context candidates final-request-ordinal)
  "Persist provider anchor CANDIDATES for completed TURN-ID."
  (when-let ((assistant-message
              (e-harness-context-runtime--turn-assistant-message harness session-id turn-id)))
    (dolist (candidate
             (e-harness-context-runtime--latest-provider-anchor-candidates
              (cl-remove-if-not
               (lambda (candidate)
                 (e-harness-context-runtime--provider-anchor-candidate-persistable-p
                  context candidate final-request-ordinal))
               candidates)))
      (when (e-harness-context-runtime--provider-anchor-candidate-persistable-p
             context candidate final-request-ordinal)
        (e-session-append-provider-anchor
         (e-harness-sessions harness)
         session-id
         (plist-get candidate :provider-id)
         :model (plist-get (plist-get context :options) :model)
         :covered-entry-id (plist-get assistant-message :id)
         :fingerprints (e-harness-context-runtime--provider-anchor-fingerprints context)
         :metadata (plist-get candidate :metadata))))))


(defun e-harness-context-runtime--turn-assistant-message (harness session-id turn-id)
  "Return the final assistant message for SESSION-ID TURN-ID.
The result is a detached session projection; no turn registry is inspected."
  (car (last
        (cl-remove-if-not
         (lambda (message)
           (and (eq (plist-get message :role) 'assistant)
                (equal (plist-get message :turn-id) turn-id)))
         (e-session-messages (e-harness-sessions harness) session-id)))))


(provide 'e-harness-context-runtime)

;;; e-harness-context-runtime.el ends here

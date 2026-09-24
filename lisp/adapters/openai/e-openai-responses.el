;;; e-openai-responses.el --- Responses wire mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns provider-neutral to Responses request mapping, continuation identity,
;; replay-safe input projection, and endpoint URL normalization.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-context-lifetime)
(require 'e-json)
(require 'e-tools)
(require 'e-openai-diagnostics)
(require 'e-openai-profile)

(defun e-openai-responses--message-content
    (role content &optional cache-breakpoint-p)
  "Return Responses content item for ROLE and CONTENT.
When CACHE-BREAKPOINT-P is non-nil, mark the input block as the end of the
explicitly cacheable stable prefix.

Provider-neutral context may carry a structured literal value (for example a
curation source presentation).  Responses text blocks are strings, so encode
such values as JSON at this wire boundary rather than using a Lisp printed
representation or dropping their structure."
  (let ((type (if (eq role 'assistant) "output_text" "input_text"))
        (text (cond
               ((null content) "")
               ((stringp content) content)
               (t (e-json-serialize content)))))
    (vector
     (append
      (list :type type :text text)
      (when cache-breakpoint-p
        (list :prompt_cache_breakpoint (list :mode "explicit")))))))

(defun e-openai-responses--input-message
    (message &optional cache-breakpoint-p)
  "Map backend-neutral MESSAGE to a Responses input item.
CACHE-BREAKPOINT-P marks this message's content as the stable-prefix end."
  (let ((role (plist-get message :role))
        (content (plist-get message :content))
        (phase (plist-get message :phase)))
    (pcase role
      ('tool-call
       (list :type "function_call"
             :call_id (plist-get content :id)
             :name (plist-get content :name)
             :arguments (e-json-serialize
                         (if (plist-member content :arguments)
                             (plist-get content :arguments)
                           nil))))
      ('tool
       (let ((result content))
         (list :type "function_call_output"
               :call_id (plist-get result :tool-call-id)
               :output (e-tools-result-content-text
                        (plist-get result :content)))))
      (_
       (append
        (list :type "message"
              :role (if (eq role 'system) "developer" (symbol-name role))
              :content (e-openai-responses--message-content
                        role content cache-breakpoint-p))
        (when (and (eq role 'assistant) phase)
          (list :phase phase)))))))

(defun e-openai-responses--input-replay-item (item)
  "Return an input-safe copy of opaque OpenAI replay ITEM.
OpenAI Responses output may represent a reasoning summary as JSON null or one
object, while Responses input requires the field to contain an array."
  (let* ((normalized (copy-tree item))
         (summary (plist-get normalized :summary)))
    (when (member (plist-get normalized :type) '("reasoning" reasoning))
      ;; Responses input requires a summary array even when the response
      ;; carried a null or one object.  The decoder already gave us the
      ;; canonical shape, so this is an explicit provider projection rather
      ;; than a list/object heuristic: vectors stay arrays, objects become a
      ;; one-element array, and empty/null values become an empty array.
      (setq normalized
            (plist-put normalized :summary
                       (cond
                        ((null summary) [])
                        ((vectorp summary) summary)
                        ((eq summary e-json-null) [])
                        ((and (consp summary)
                              (keywordp (car summary)))
                         (e-json-assert-value summary)
                         (vector summary))
                        (t
                         (signal 'e-openai-provider-invalid
                                 (list "Responses reasoning summary is not an object or array"
                                       summary)))))))
    normalized))

(defun e-openai-responses--normalize-input-items (items)
  "Return Responses input ITEMS with reasoning replay shapes normalized.

Most replay items enter through `e-openai-responses--message-replay-items', but
opaque provider-compaction output is already an input-item sequence and takes a
different path.  Normalize at this final adapter boundary as well; the helper
is idempotent for vectors, so an already valid summary array is not wrapped a
second time."
  (vconcat
   (mapcar (lambda (item)
             (if (and (listp item)
                      (member (plist-get item :type) '("reasoning" reasoning)))
                 (e-openai-responses--input-replay-item item)
               item))
           (if (vectorp items) (append items nil) items))))

(defun e-openai-responses--provider-replay-items
    (records &optional immediate-followup-p)
  "Return input-safe OpenAI opaque replay RECORDS.

When IMMEDIATE-FOLLOWUP-P is non-nil, omit records marked
`:full-replay-only'.  Such records are needed to reconstruct an unanchored
Responses request, but an anchored response already contains them."
  (cl-loop for record in (if (vectorp records) (append records nil) records)
             when (and (member (plist-get record :provider-id)
                               '(openai "openai"))
                       (or (not immediate-followup-p)
                           (not (plist-get record :full-replay-only))))
             collect (e-openai-responses--input-replay-item
                      (plist-get record :item))))

(defun e-openai-responses--message-replay-items (message &optional immediate-followup-p)
  "Return input-safe OpenAI opaque replay items attached to MESSAGE.

IMMEDIATE-FOLLOWUP-P has the meaning described by
`e-openai-responses--provider-replay-items'."
  (let* ((role (plist-get message :role))
         (carrier (if (eq role 'tool-call)
                      (plist-get message :content)
                    (plist-get message :metadata))))
    (e-openai-responses--provider-replay-items
     (plist-get carrier :provider-replay-items)
     immediate-followup-p)))

(defun e-openai-responses--system-message-p (message)
  "Return non-nil when MESSAGE is a backend-neutral system message."
  (eq (plist-get message :role) 'system))

(defun e-openai-responses--replaceable-current-state-messages (options)
  "Return complete request-local current-state messages from OPTIONS.

The harness supplies the canonical frontier explicitly.  Direct adapter
callers may instead provide semantic segments, which keeps request-body tests
and other provider-neutral callers honest without making the adapter infer a
replaceable channel from a raw system-message role."
  (when (eq (e-openai-profile-observation-delivery-for-options options)
            'request-local-replaceable)
    (copy-tree
     (or (plist-get options :replaceable-current-state)
         (cl-loop for segment in (plist-get options :segments)
                  when (memq (plist-get segment :kind)
                             '(current-state dynamic-context))
                  append (copy-tree (plist-get segment :messages)))))))

(defun e-openai-responses--replaceable-current-state-content (options)
  "Return complete current-state value suitable for Responses instructions."
  (let ((messages (e-openai-responses--replaceable-current-state-messages options)))
    (when messages
      (string-join
       (delq nil
             (mapcar
              (lambda (message)
                (let ((content (plist-get message :content)))
                  (cond
                   ((stringp content) content)
                   ((null content) nil)
                   (t (e-json-serialize content)))))
              messages))
       "\n\n"))))

(defun e-openai-responses--provider-compaction-stable-messages (options)
  "Return trusted stable semantic prefix messages from OPTIONS.

Opaque provider output replaces covered session history, but it does not carry
the harness capability prefix.  Only the named static/stable segments may be
resent here; deriving this from the full request would duplicate the portable
checkpoint represented by the opaque output."
  (cl-loop for segment in (plist-get options :segments)
           when (memq (plist-get segment :kind)
                      '(static-prefix stable-context))
           append (copy-tree (plist-get segment :messages))))

(defun e-openai-responses--remove-replaceable-current-state
    (messages options)
  "Return MESSAGES without the request-local current-state frontier.

The semantic segment layout, rather than structural message equality, identifies
which positions belong to the frontier.  This is important when a durable
history/delta message has the same role and content as the current observation.
Callers without segments cannot prove the partition and therefore signal an
explicit projection error instead of deleting an equal durable message by
guesswork or copying the frontier into explicit input.  The harness owns the
derived partition boundary; callers cannot forge an already-partitioned
escape."
  (let* ((replaceable-p
          (eq (e-openai-profile-observation-delivery-for-options options)
              'request-local-replaceable))
         (frontier-messages
          (e-openai-responses--replaceable-current-state-messages options))
         (segments (plist-get options :segments)))
    (cond
     ((not replaceable-p) messages)
     ((null frontier-messages) messages)
     ((null segments)
      (signal 'e-openai-context-projection-invalid
              '("A non-empty replaceable frontier has no segment partition")))
     (t
      (let ((index 0)
            (frontier-indices nil)
            (segment-message-count
             (plist-get options :context-segment-message-count))
            (segment-messages nil)
            (frontier-segment-messages nil))
        (dolist (segment segments)
          (dolist (message (plist-get segment :messages))
            (push message segment-messages)
            (when (memq (plist-get segment :kind)
                        '(current-state dynamic-context))
              (push index frontier-indices)
              (push message frontier-segment-messages))
            (setq index (1+ index))))
        (setq segment-messages (nreverse segment-messages)
              frontier-segment-messages (nreverse frontier-segment-messages))
        ;; The segment list is the sole origin evidence.  It may cover a
        ;; complete request or an exact prefix followed by later in-turn
        ;; messages, but its contents must be that exact prefix.  The reserved
        ;; count is checked when present, never used as an authority, and the
        ;; frontier value must agree with the frontier-bearing segments.  In
        ;; particular, a caller cannot forge a continuation-delta plist value
        ;; to turn an absent or mismatched partition into a safe projection.
        (unless (and (<= index (length messages))
                     (equal segment-messages
                            (cl-subseq messages 0 index))
                     (or (null segment-message-count)
                         (and (integerp segment-message-count)
                              (= index segment-message-count)))
                     (equal frontier-messages frontier-segment-messages))
          (signal 'e-openai-context-projection-invalid
                  '("Replaceable frontier segments do not cover the request prefix")))
        (cl-loop for message in messages
                 for message-index from 0
                 unless (memq message-index frontier-indices)
                 collect message))))))

(defun e-openai-responses--instructions (messages options)
  "Return top-level Codex instructions from MESSAGES and OPTIONS."
  (let* ((base (or (plist-get options :instructions)
                   "You are a helpful assistant."))
         (provider-compaction-p
          (plist-member options :provider-compaction-output))
         (current
          (e-openai-responses--replaceable-current-state-content options))
         (stable-messages
          (and provider-compaction-p
               (e-openai-responses--provider-compaction-stable-messages
                options)))
         (layout (e-openai-profile-segmented-prompt-layout-p options)))
    ;; Validate the semantic partition even when the segmented instruction
    ;; branch can otherwise derive stable system text without filtering input.
    ;; This prevents an absent/partial segment list from silently duplicating a
    ;; non-empty request-local frontier in explicit input.
    (when current
      (e-openai-responses--remove-replaceable-current-state messages options))
    (cond
     ;; In segmented Responses layouts stable system guidance remains a
     ;; developer-input prefix.  The current value is the only changing
     ;; request-local part and is resent in full here.
     ((and current layout)
      (string-join (delq nil (list base current)) "\n\n"))
     ;; Older/flattened Responses layouts have no separate stable developer
     ;; input.  Preserve their existing stable system guidance while replacing
     ;; the old current-state suffix with the complete current value.
     (current
      (let ((stable-messages
             (if provider-compaction-p
                 stable-messages
               (if (plist-get options :segments)
                 (cl-loop for segment in (plist-get options :segments)
                          unless (memq (plist-get segment :kind)
                                       '(current-state dynamic-context))
                          append (seq-filter
                                  #'e-openai-responses--system-message-p
                                  (plist-get segment :messages)))
               (seq-filter
                #'e-openai-responses--system-message-p
                (e-openai-responses--remove-replaceable-current-state
                 messages options))))))
        (string-join
         (delq nil
               (append
                (list base)
                (mapcar (lambda (message)
                          (plist-get message :content))
                        stable-messages)
                (list current)))
         "\n\n")))
     ;; A flattened provider-compaction request has no separate input channel
     ;; for stable capability context, so keep only the trusted stable prefix
     ;; in instructions.  Covered checkpoint/history messages stay exclusively
     ;; in opaque provider output.
     ((and provider-compaction-p (not layout))
      (string-join
       (delq nil
             (append (list base)
                     (mapcar (lambda (message)
                               (plist-get message :content))
                             stable-messages)))
       "\n\n"))
     (layout base)
     (t
      (string-join
       (delq nil
             (append (list base)
                     (mapcar (lambda (message) (plist-get message :content))
                             (seq-filter #'e-openai-responses--system-message-p
                                         messages))))
       "\n\n")))))

(defun e-openai-responses--continuation-response-id (options)
  "Return previous Responses id from OPTIONS when continuation is enabled."
  (when (and (not (plist-member options :provider-compaction-output))
             (plist-get options :provider-continuation)
             ;; WebSocket mode retains the latest response in connection-local
             ;; memory even with store=false.  HTTP continuation still needs a
             ;; stored response.
             (or (e-openai-profile-websocket-request-p options)
                 (not (eq (e-openai-profile-response-store options)
                          :json-false))))
    (let* ((anchor (plist-get options :provider-anchor))
           (metadata (plist-get anchor :metadata))
           (response-id (plist-get metadata :response-id)))
      (when (and (eq (plist-get anchor :provider-id) 'openai)
                 (stringp response-id)
                 (not (string-empty-p response-id))
                 (equal (plist-get metadata :prompt-layout-revision)
                        (e-openai-profile-prompt-layout-revision options))
                 ;; The effective reasoning pair is part of continuation
                 ;; safety.  An anchor without it is legacy/incomplete and
                 ;; must not authorize a material Responses continuation.
                 (and (plist-member metadata :reasoning-identity)
                      (equal (plist-get metadata :reasoning-identity)
                             (e-openai-profile-reasoning-identity options))))
        response-id))))

(defun e-openai-responses--move-inherited-frontier-to-end (messages options)
  "Move inherited observation segments after the canonical durable prefix.

The harness supplies semantic segments for a complete canonical request.  When
those segments cover MESSAGES exactly, current-state and dynamic-context
messages are emitted after every other message, preserving the order within
each group.  A canonical request may carry a harness-owned segment-message
count when same-turn messages follow that semantic prefix; that exact prefix
is accepted and the suffix is retained.  A continuation delta,
provider-compaction projection, or direct caller without segments is already
owned by another projection and is returned unchanged.  A canonical semantic
segment list that names a frontier but matches neither the full request nor
its explicitly counted prefix is invalid and signals instead of silently
guessing from message shape."
  (let ((segments (plist-get options :segments))
        (segment-message-count
         (plist-get options :context-segment-message-count))
        (continuation-delta-p
         (and (e-openai-responses--continuation-response-id options)
              (plist-member options :provider-anchor-delta-messages)
              (listp (plist-get options :provider-anchor-delta-messages))))
        (provider-compaction-p
         (plist-member options :provider-compaction-output)))
    (if (or (not (eq (e-openai-profile-observation-delivery-for-options options)
                     'inherited))
            (not (e-openai-profile-segmented-prompt-layout-p options))
            (null segments)
            continuation-delta-p
            provider-compaction-p)
        messages
      (let ((index 0)
            (frontier-indices nil)
            (segment-messages nil))
        (dolist (segment segments)
          (dolist (message (plist-get segment :messages))
            (push message segment-messages)
            (when (memq (plist-get segment :kind)
                        '(current-state dynamic-context))
              (push index frontier-indices))
            (setq index (1+ index))))
        (setq segment-messages (nreverse segment-messages)
              frontier-indices (nreverse frontier-indices))
        (cond
         ((null frontier-indices)
          messages)
         ((not (or (equal segment-messages messages)
                   (and (integerp segment-message-count)
                        (= index segment-message-count)
                        (<= index (length messages))
                        (equal segment-messages
                               (cl-subseq messages 0 index)))))
          (signal
           'e-openai-context-projection-invalid
           '("Inherited frontier segments do not cover the canonical request")))
         (t
          (append
           (cl-loop for message in messages
                    for message-index from 0
                    unless (memq message-index frontier-indices)
                    collect message)
           (mapcar (lambda (message-index)
                     (nth message-index messages))
                   frontier-indices))))))))

(defun e-openai-responses--request-input-messages (messages options)
  "Return Responses input messages from MESSAGES and OPTIONS."
  (let* ((provider-compaction-p
          (plist-member options :provider-compaction-output))
         (provider-compaction-output
          (and provider-compaction-p
               (plist-get options :provider-compaction-output)))
         (provider-compaction-delta
          (plist-get options :provider-compaction-delta-messages))
         (response-id (e-openai-responses--continuation-response-id options))
         (delta-messages (plist-get options :provider-anchor-delta-messages))
         (source-count
          (plist-get options :provider-anchor-source-message-count))
         (in-turn-messages
          (when (and (integerp source-count)
                     (>= source-count 0)
                     (<= source-count (length messages)))
            (nthcdr source-count messages)))
         (source (cond
                  (provider-compaction-p
                   (append (if (vectorp provider-compaction-output)
                               (append provider-compaction-output nil)
                             provider-compaction-output)
                           (mapcar #'e-openai-responses--input-message
                                   provider-compaction-delta)))
                  ((and response-id (listp delta-messages))
                     (append delta-messages in-turn-messages)
                   )
                  (t messages)))
         ;; A harness-built continuation delta is already partitioned at the
         ;; semantic boundary and contains no request-local replacement.  Do
         ;; not structurally re-filter it: equal durable deltas must survive.
         (source (if (or provider-compaction-p
                         (and response-id (listp delta-messages)))
                     source
                   (e-openai-responses--remove-replaceable-current-state
                    source options)))
         (source (e-openai-responses--move-inherited-frontier-to-end
                  source options)))
    (if (or provider-compaction-p
            (e-openai-profile-segmented-prompt-layout-p options))
        source
      (seq-remove #'e-openai-responses--system-message-p source))))

(defun e-openai-responses--input-items
    (messages options continuation-response-id)
  "Return Responses input items for MESSAGES under OPTIONS.
CONTINUATION-RESPONSE-ID suppresses a new explicit breakpoint because the
retained response already carries the stable segment and its earlier marker."
  (let ((stable-left
         (if (and (e-openai-profile-wire-prompt-layout-revision options)
                  (eq (e-openai-profile-prompt-cache-breakpoint-mode options)
                      'explicit)
                  (null continuation-response-id))
             (e-openai-profile-stable-system-message-count options)
           0))
        items)
    (dolist (message messages (vconcat (nreverse items)))
      (let ((breakpoint-p nil))
        (when (and (> stable-left 0)
                   (e-openai-responses--system-message-p message))
          (setq stable-left (1- stable-left))
          (setq breakpoint-p (= stable-left 0)))
        (let ((input-message
               (e-openai-responses--input-message message breakpoint-p)))
          (if (eq (plist-get message :role) 'tool)
              (progn
                ;; A tool result is the causal predecessor of replay items
                ;; attached by a later reserved curation effect.  Full replay
                ;; must therefore render the result before that call/output
                ;; pair.  Assistant and tool-call carriers retain the
                ;; established replay-before-carrier ordering.
                (push input-message items)
                (dolist (replay-item
                         (e-openai-responses--message-replay-items
                          message continuation-response-id))
                  (push replay-item items)))
            (dolist (replay-item
                     (e-openai-responses--message-replay-items
                      message continuation-response-id))
              (push replay-item items))
            (push input-message items)))))))

(defun e-openai-responses--request-input-items
    (messages options continuation-response-id)
  "Return provider input items for MESSAGES under OPTIONS.

CONTINUATION-RESPONSE-ID selects the anchored immediate replay shape; a nil
value selects full stateless replay."
  (let ((ordinary-items
         (if (plist-member options :provider-compaction-output)
             (let* ((output (plist-get options :provider-compaction-output))
                    (output (if (vectorp output) (append output nil) output))
                    (stable-messages
                     (if (e-openai-profile-segmented-prompt-layout-p options)
                         (e-openai-responses--provider-compaction-stable-messages
                          options)))
                    (stable-items
                     (if stable-messages
                         (append
                          (e-openai-responses--input-items
                           stable-messages options nil)
                          nil)))
                    (delta
                     (plist-get options :provider-compaction-delta-messages)))
               (append output
                       stable-items
                       (mapcar #'e-openai-responses--input-message delta)))
           (append
            (e-openai-responses--input-items
             (e-openai-responses--request-input-messages messages options)
             options
             continuation-response-id)
            nil)))
        (request-replay-items
         (e-openai-responses--provider-replay-items
          (plist-get options :provider-request-replay-items)
          continuation-response-id)))
    ;; A curation-only response has no semantic message carrier.  Its opaque
    ;; call/output pair follows the ordinary stateless input; an anchored
    ;; acknowledgement filters the already-retained call and sends only the
    ;; immediate output delta.
    (e-openai-responses--normalize-input-items
     (append ordinary-items request-replay-items))))

(defun e-openai-responses--without-provider-anchor (options)
  "Return OPTIONS without provider-anchor continuation state."
  (let ((options (copy-sequence options)))
    (dolist (key '(:provider-anchor
                   :provider-anchor-delta-messages
                   :provider-anchor-source-message-count))
      (cl-remf options key))
    options))

(defun e-openai-responses--context-curation-tool-definition ()
  "Return the wire carrier for the core-owned curation effect."
  (list
   :type "function"
   :name "context-curate"
   :description
   "Call context-curate at most once for the currently presented set of labeled ephemeral sources. After its acknowledgement, continue with ordinary tools or a normal answer. Call it again only after later tool work or context refresh presents a new set of labeled ephemeral sources; labels belong only to the currently presented frame and cannot be reused for an earlier frame. Use keep for exact retention and summaries for compact durable replacements. Use erase only for labels whose source marker says erase-eligible; never erase a label marked erase-ineligible. Any presented label you omit loses its exact content; it is valid to omit every label when none should be retained or erased. Separately owned derived context such as receipts may remain."
   :parameters
   (list
    :type "object"
    :additionalProperties :json-false
    :properties
    (list
     :keep
     (list
      :type "array"
      :maxItems 16
      :items (list :type "integer" :minimum 1))
     :summaries
     (list
      :type "array"
      :maxItems 16
      :items
      (list
       :type "object"
       :additionalProperties :json-false
       :required ["sources" "text"]
       :properties
       (list
        :sources
        (list :type "array" :minItems 1 :maxItems 16
              :items (list :type "integer" :minimum 1))
        :text
        (list :type "string" :minLength 1))))
     :erase
     (list :type "array"
           :maxItems 16
           :items (list :type "integer" :minimum 1))))))

(defun e-openai-responses--text-verbosity (model options)
  "Return Responses text verbosity for MODEL under OPTIONS."
  (or (plist-get options :text-verbosity)
      (plist-get options :model-verbosity)
      (when (and (stringp model)
                 (string-prefix-p "gpt-5" model))
        e-openai-default-text-verbosity)))

(cl-defun e-openai-codex-request-body (&key messages options tools)
  "Build a Codex Responses request body from MESSAGES, OPTIONS, and TOOLS."
  (let* ((model (or (plist-get options :model)
                    e-openai-default-model))
         (text-verbosity (e-openai-responses--text-verbosity model options))
         (continuation-response-id
          (e-openai-responses--continuation-response-id options))
         (reasoning (e-openai-profile-effective-reasoning options))
         (store-value (e-openai-profile-response-store options))
         (body (append
                (list :model model)
                (unless (and (e-openai-profile-implicit-websocket-store-p options)
                             (eq store-value t))
                  (list :store store-value))
                (unless (e-openai-profile-websocket-request-p options)
                  (list :stream t))
                (list :instructions (e-openai-responses--instructions
                                     messages
                                     options)
                      :input (e-openai-responses--request-input-items
                              messages options continuation-response-id)
                      :tool_choice "auto"
                      :parallel_tool_calls t))))
    (when (or tools
              (eq (plist-get options :reserved-effect-carrier)
                  'context-curate-wire))
      (setq body
            (append body
                    (list :tools
                          (vconcat
                           (append (copy-tree tools)
                                   (when (eq (plist-get options
                                                       :reserved-effect-carrier)
                                             'context-curate-wire)
                                     (list
                                      (e-openai-responses--context-curation-tool-definition)))))))))
    (when text-verbosity
      (setq body (append body (list :text (list :verbosity text-verbosity)))))
    (when reasoning
      (setq body (append body (list :reasoning reasoning))))
    (when (plist-get options :include-encrypted-reasoning)
      (setq body
            (append body
                    (list :include ["reasoning.encrypted_content"]))))
    (when continuation-response-id
      (setq body
            (append body
                    (list :previous_response_id
                          continuation-response-id))))
    (when (plist-member options :prompt-cache-key)
      (setq body
            (append body
                    (list :prompt_cache_key
                          (plist-get options :prompt-cache-key)))))
    (when (and (e-openai-profile-wire-prompt-layout-revision options)
               (eq (e-openai-profile-prompt-cache-breakpoint-mode options)
                   'explicit))
      (setq body
            (append body
                    (list :prompt_cache_options (list :mode "explicit")))))
    (when (plist-member options :prompt-cache-retention)
      (setq body
            (append body
                    (list :prompt_cache_retention
                          (plist-get options :prompt-cache-retention)))))
    body))


(defun e-openai-responses-url (base-url)
  "Return the Responses endpoint URL for BASE-URL."
  (let ((normalized (string-remove-suffix "/" base-url)))
    (if (string-suffix-p "/responses" normalized)
        normalized
      (concat normalized "/responses"))))

(defun e-openai-responses-websocket-url (base-url)
  "Return the Responses WebSocket URL for BASE-URL."
  (let ((url (e-openai-responses-url base-url)))
    (cond
     ((string-prefix-p "https://" url)
      (concat "wss://" (substring url (length "https://"))))
     ((string-prefix-p "http://" url)
      (concat "ws://" (substring url (length "http://"))))
     ((or (string-prefix-p "wss://" url)
          (string-prefix-p "ws://" url))
      url)
     (t
      (signal 'e-openai-provider-invalid
              (list (format "Unsupported WebSocket base URL %S" base-url)))))))

(defun e-openai-codex-url (&optional base-url)
  "Return the Codex Responses URL for BASE-URL."
  (let ((normalized (string-remove-suffix
                     "/"
                     (or base-url e-openai-codex-default-base-url))))
    (cond
     ((string-suffix-p "/responses" normalized) normalized)
     ((string-suffix-p "/codex" normalized)
      (e-openai-responses-url normalized))
     (t (e-openai-responses-url (concat normalized "/codex"))))))



(defun e-openai-responses-input-message (message &optional cache-breakpoint-p)
  "Map backend-neutral MESSAGE to one Responses input item."
  (e-openai-responses--input-message message cache-breakpoint-p))

(defun e-openai-responses-prompt-layout-revision (options)
  "Return the material Responses prompt-layout identity for OPTIONS."
  (e-openai-profile-prompt-layout-revision options))

(defun e-openai-responses-reasoning-identity (options)
  "Return the effective Responses reasoning identity for OPTIONS."
  (e-openai-profile-reasoning-identity options))

(defun e-openai-responses-replaceable-current-state-present-p (options)
  "Return non-nil when OPTIONS carry a complete replaceable state frontier."
  (and (e-openai-responses--replaceable-current-state-messages options) t))

(defun e-openai-responses-prompt-cache-mode-label (options)
  "Return the diagnostic cache mode label for OPTIONS."
  (e-openai-profile-prompt-cache-mode-label options))

(defun e-openai-responses-options-without-provider-anchor (options)
  "Return OPTIONS without provider-anchor continuation state."
  (e-openai-responses--without-provider-anchor options))

(defun e-openai-responses-context-curation-tool-definition ()
  "Return the reserved Responses context-curation tool definition."
  (e-openai-responses--context-curation-tool-definition))

(provide 'e-openai-responses)

;;; e-openai-responses.el ends here

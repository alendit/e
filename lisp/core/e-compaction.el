;;; e-compaction.el --- Transcript compaction support for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provider-neutral preparation for durable transcript compaction.

;;; Code:

(require 'cl-lib)
(require 'e-session)
(require 'e-tools)
(require 'seq)
(require 'subr-x)

(define-error 'e-compaction-error "Context compaction failed")

(defgroup e-compaction nil
  "Context compaction behavior."
  :group 'e
  :prefix "e-compaction-")

(defcustom e-compaction-keep-recent-tokens 20000
  "Approximate transcript tokens to keep after a compaction boundary."
  :type 'integer
  :group 'e-compaction)

(defcustom e-compaction-tool-result-character-limit 2000
  "Maximum characters kept per tool result in summarization input."
  :type 'integer
  :group 'e-compaction)

(defcustom e-compaction-kept-tool-result-character-limit 2000
  "Maximum characters kept per tool result in compacted context suffixes."
  :type 'integer
  :group 'e-compaction)

(defun e-compaction--message-entry-p (entry)
  "Return non-nil when ENTRY is a transcript message."
  (eq (plist-get entry :type) 'message))

(defun e-compaction--message-role (entry)
  "Return ENTRY message role."
  (plist-get entry :role))

(defun e-compaction--proper-list-p (value)
  "Return non-nil when VALUE is a proper list."
  (and (listp value)
       (proper-list-p value)))

(defun e-compaction--stringify (value)
  "Return a compact text representation of VALUE."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((and (e-compaction--proper-list-p value)
         (plist-member value :content))
    (e-compaction--stringify (plist-get value :content)))
   ((e-compaction--proper-list-p value)
    (string-join
     (delq nil
           (mapcar (lambda (item)
                     (let ((text (e-compaction--stringify item)))
                       (unless (string-empty-p text) text)))
                   value))
     "\n"))
   ((consp value)
    (let ((key (e-compaction--stringify (car value)))
          (val (e-compaction--stringify (cdr value))))
      (cond
       ((string-empty-p key) val)
       ((string-empty-p val) key)
       (t (format "%s: %s" key val)))))
   (t (format "%S" value))))

(defun e-compaction--truncate (text limit)
  "Return TEXT truncated to LIMIT characters with an explicit marker."
  (if (and (integerp limit) (> limit 0) (> (length text) limit))
      (concat (substring text 0 limit)
              (format "\n[truncated %d characters]"
                      (- (length text) limit)))
    text))

(defun e-compaction-estimate-tokens (text)
  "Return a conservative token estimate for TEXT."
  (max 1 (ceiling (/ (float (length (or text ""))) 4.0))))

(defun e-compaction--entry-text (entry)
  "Return human-readable text for ENTRY."
  (let* ((role (plist-get entry :role))
         (content (plist-get entry :content))
         (text (e-compaction--stringify content)))
    (pcase role
      ('user (format "User: %s" text))
      ('assistant (format "Assistant: %s" text))
      ('tool-call
       (format "Tool call: %s\n%s"
               (or (plist-get content :name) "tool")
               (e-compaction--stringify (plist-get content :arguments))))
      ('tool
       (format "Tool result: %s"
               (e-compaction--truncate
                text e-compaction-tool-result-character-limit)))
      (_ (format "%s: %s" role text)))))

(defun e-compaction-entry-token-estimate (entry)
  "Return approximate token count for ENTRY."
  (e-compaction-estimate-tokens (e-compaction--entry-text entry)))

(defun e-compaction--message-entries (store session-id)
  "Return current-path message entries for SESSION-ID."
  (seq-filter #'e-compaction--message-entry-p
              (e-session-current-path store session-id)))

(defun e-compaction--safe-boundary-entry-p (entry boundary-roles)
  "Return non-nil if compaction may keep suffix starting at ENTRY.
BOUNDARY-ROLES is the list of roles accepted as suffix boundaries."
  (memq (e-compaction--message-role entry) boundary-roles))

(defun e-compaction--select-boundary
    (entries keep-tokens boundary-roles previous-boundary)
  "Select a conservative boundary in ENTRIES keeping about KEEP-TOKENS.
BOUNDARY-ROLES controls the accepted suffix-start roles.  PREVIOUS-BOUNDARY is
the previous compaction boundary id, if any."
  (catch 'done
    (let ((kept 0)
          candidate)
      (dolist (entry (reverse entries))
        (setq kept (+ kept (e-compaction-entry-token-estimate entry)))
        (when (and (e-compaction--safe-boundary-entry-p entry boundary-roles)
                   (e-compaction--entries-between
                    entries previous-boundary (plist-get entry :id)))
          (setq candidate entry))
        (when (and candidate (>= kept keep-tokens))
          (throw 'done candidate)))
      candidate)))

(defun e-compaction--entries-between (entries start-id end-id)
  "Return ENTRIES from START-ID inclusive to before END-ID.
When START-ID is nil, start at the first entry."
  (catch 'done
    (let ((collecting (null start-id))
          result)
      (dolist (entry entries)
        (when (equal (plist-get entry :id) end-id)
          (throw 'done (nreverse result)))
        (when (or collecting
                  (equal (plist-get entry :id) start-id))
          (setq collecting t)
          (push entry result)))
      (nreverse result))))

(defun e-compaction--metadata-tool-usage (metadata)
  "Return normalized tool usage list from METADATA."
  (let ((usage (or (plist-get metadata :tool-usage)
                   (plist-get metadata :tool_usage))))
    (cond
     ((null usage) nil)
     ((and (listp usage)
           (or (plist-member usage :kind)
               (plist-member usage :resources)))
      (list usage))
     ((listp usage) usage)
     (t nil))))

(defun e-compaction--entry-tool-usage (entry)
  "Return tool usage metadata from ENTRY."
  (let ((metadata (or (plist-get entry :metadata)
                      (plist-get (plist-get entry :content) :metadata))))
    (e-compaction--metadata-tool-usage metadata)))

(defun e-compaction--affected-resources (entries)
  "Return deduplicated affected-resource metadata for ENTRIES."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (dolist (usage (e-compaction--entry-tool-usage entry))
        (let ((tool (or (plist-get usage :tool)
                        (plist-get usage :tool-name)
                        (plist-get usage :tool_name))))
          (dolist (resource (plist-get usage :resources))
            (let* ((uri (or (plist-get resource :uri)
                            (plist-get resource :resource-uri)
                            (plist-get resource :resource_uri)))
                   (operation (or (plist-get resource :operation)
                                  (plist-get resource :operation-id)
                                  (plist-get resource :operation_id)))
                   (key (list uri operation))
                   (current (gethash key table)))
              (when uri
                (puthash key
                         (list :uri uri
                               :operation operation
                               :tools (seq-uniq
                                       (delq nil (cons tool (plist-get current :tools)))
                                       #'equal))
                         table)))))))
    (let (resources)
      (maphash (lambda (_key value) (push value resources)) table)
      (sort resources
            (lambda (left right)
              (string< (or (plist-get left :uri) "")
                       (or (plist-get right :uri) "")))))))

(defun e-compaction--serialize-entry (entry)
  "Serialize ENTRY for summarization input."
  (format "- id=%s role=%s turn=%s\n%s"
          (plist-get entry :id)
          (e-compaction--message-role entry)
          (or (plist-get entry :turn-id) "")
          (e-compaction--entry-text entry)))

(defun e-compaction--serialize-entries (entries)
  "Serialize ENTRIES for summarization input."
  (string-join (mapcar #'e-compaction--serialize-entry entries) "\n\n"))

(defun e-compaction--kept-tool-result-location (result)
  "Return a readable full-output location from RESULT metadata, when present."
  (let ((metadata (plist-get result :metadata)))
    (or (plist-get metadata :tmp-uri)
        (plist-get metadata :full-output-path))))

(defun e-compaction--kept-tool-result-notice (shown original location)
  "Return compacted-context truncation notice for a kept tool result."
  (format "[Tool output preview truncated: showing first %d of %d characters%s]"
          shown
          original
          (if location
              (format ". Full output: %s" location)
            "")))

(defun e-compaction--kept-tool-result-content-preview (text location)
  "Return compacted-context preview for tool result TEXT and LOCATION."
  (let* ((limit (max 0 e-compaction-kept-tool-result-character-limit))
         (original (length text)))
    (if (<= original limit)
        text
      (let ((preview (substring text 0 limit))
            (notice (e-compaction--kept-tool-result-notice
                     limit
                     original
                     location)))
        (if (string-empty-p preview)
            notice
          (concat preview "\n\n" notice))))))

(defun e-compaction--preview-kept-tool-result (result)
  "Return RESULT with bounded content for compacted context suffixes."
  (if (e-tools-result-p result)
      (let* ((content-text (e-tools-result-content-text
                            (plist-get result :content)))
             (preview (e-compaction--kept-tool-result-content-preview
                       content-text
                       (e-compaction--kept-tool-result-location result))))
        (if (equal preview content-text)
            result
          (let ((copy (copy-sequence result)))
            (plist-put copy :content preview)
            copy)))
    (let* ((content-text (e-tools-result-content-text result))
           (preview (e-compaction--kept-tool-result-content-preview
                     content-text
                     nil)))
      (if (equal preview content-text)
          result
        preview))))

(defun e-compaction-preview-kept-message (message)
  "Return MESSAGE projected for a compacted context kept suffix.

This preserves the transcript shape while bounding verbose tool-result content
that survived the compaction boundary."
  (if (eq (plist-get message :role) 'tool)
      (let ((copy (copy-sequence message)))
        (plist-put copy
                   :content
                   (e-compaction--preview-kept-tool-result
                    (plist-get message :content)))
        copy)
    message))

(defun e-compaction-summary-messages (preparation)
  "Return backend messages that ask the model to summarize PREPARATION."
  (let* ((metadata (plist-get preparation :metadata))
         (previous (plist-get metadata :previous-summary))
         (instructions (plist-get metadata :instructions))
         (resources (plist-get metadata :affected-resources)))
    (list
     (list :role 'system
           :content
           "Compact the transcript into a durable continuation summary. Preserve user intent, decisions, unresolved work, important state, and resource effects. Do not invent new facts.")
     (list
      :role 'user
      :content
      (string-join
       (delq nil
             (list
              (when previous
                (format "Previous summary:\n%s" previous))
              (when (and (stringp instructions)
                         (not (string-empty-p (string-trim instructions))))
                (format "Additional instructions:\n%s" instructions))
              (when resources
                (format "Affected resources:\n%s"
                        (e-compaction--stringify resources)))
              (format "Transcript to compact:\n%s"
                      (plist-get preparation :summary-input))))
       "\n\n")))))

(defun e-compaction--portable-summarized-entries
    (store session-id entries exclude-entry-ids)
  "Return the ordinary summarized ENTRIES safe for portable input.

When an excluded entry is present on the current path, portable context must
not cover it accidentally through a later boundary.  Keep only the ordinary
prefix before the first excluded entry; later entries remain the derived tail
even when the legacy summarizer selected them for its own request."
  (let ((excluded (make-hash-table :test #'equal))
        (summarized (make-hash-table :test #'equal))
        result)
    (dolist (id exclude-entry-ids)
      (puthash id t excluded))
    (dolist (entry entries)
      (puthash (plist-get entry :id) t summarized))
    (catch 'first-excluded
      (dolist (entry (e-session-current-path store session-id))
        (let ((id (plist-get entry :id)))
          (when (gethash id excluded)
            (throw 'first-excluded t))
          (when (gethash id summarized)
            (when (e-session-context-lifetime-durable-message entry)
              (push entry result))))))
    (nreverse result)))

(cl-defun e-compaction-prepare
    (store session-id &key keep-recent-tokens instructions allow-split-turn
           exclude-entry-ids (reason 'manual) portable)
  "Prepare compaction data for SESSION-ID in STORE.

When PORTABLE is non-nil, also capture the opt-in provider-neutral input and
summary request.  The default path deliberately does not build or decode any
portable generation data."
  (let* ((keep (or keep-recent-tokens e-compaction-keep-recent-tokens))
         (entries (cl-remove-if
                   (lambda (entry)
                     (member (plist-get entry :id) exclude-entry-ids))
                   (e-compaction--message-entries store session-id)))
         (previous (e-session-latest-valid-compaction store session-id))
         (previous-boundary (plist-get previous :first-kept-entry-id))
         ;; Prefer a clean user-message boundary.  When none is available, fall
         ;; back to assistant/tool-call boundaries: a long single agentic turn
         ;; has only one user message, so a user-only search returns nil and the
         ;; context could never compact otherwise.  `allow-split-turn' is kept
         ;; as a parameter for callers/tests but no longer gates the fallback.
         (boundary
          (or (e-compaction--select-boundary
               entries keep '(user) previous-boundary)
              (e-compaction--select-boundary
               entries keep '(user assistant tool-call) previous-boundary))))
    (ignore allow-split-turn)
    (unless boundary
      (signal 'e-compaction-error
              (list "No safe message boundary available for compaction")))
    (let* ((boundary-id (plist-get boundary :id))
           (boundary-role (e-compaction--message-role boundary))
           (to-summarize
            (e-compaction--entries-between entries previous-boundary boundary-id))
           (to-keep (member boundary entries)))
      (unless to-summarize
        (signal 'e-compaction-error
                (list "Selected boundary would not compact any new messages")))
      (let* ((tokens-before (apply #'+ (mapcar #'e-compaction-entry-token-estimate
                                               to-summarize)))
             (tokens-kept (apply #'+ (mapcar #'e-compaction-entry-token-estimate
                                             to-keep)))
             (resources (e-compaction--affected-resources to-summarize))
             (portable-summarized-entries
              (when portable
                (e-compaction--portable-summarized-entries
                 store session-id to-summarize exclude-entry-ids)))
             (portable-input
              (when portable
                (unless portable-summarized-entries
                  (signal 'e-compaction-error
                          (list "No eligible durable portable boundary")))
                (e-compaction-portable-input
                 store session-id portable-summarized-entries
                 (plist-get (car (last portable-summarized-entries)) :id))))
             (result
              (list :session-id session-id
                    :first-kept-entry-id boundary-id
                    :summary-input (e-compaction--serialize-entries to-summarize)
                    :tokens-before tokens-before
                    :tokens-kept tokens-kept
                    :metadata
                    (list :reason reason
                          :instructions instructions
                          :previous-compaction-id (plist-get previous :id)
                          :previous-summary (plist-get previous :summary)
                          :boundary-role boundary-role
                          :split-turn (not (eq boundary-role 'user))
                          :compacted-entry-count (length to-summarize)
                          :kept-entry-count (length to-keep)
                          :affected-resources resources))))
        (if portable
            (progn
              (plist-put result :portable-input portable-input)
              (plist-put result :summary-messages
                         (e-compaction-portable-summary-messages
                          portable-input))
              result)
          result)))))

(defun e-compaction-portable-input
    (store session-id summarized-entries covered-session-boundary)
  "Return the provider-neutral input for a portable generation boundary.

The input is derived from the ordinary compaction prefix represented by
SUMMARIZED-ENTRIES.  It includes the current portable checkpoint and only
eligible durable messages through COVERED-SESSION-BOUNDARY; retained,
excluded, and later entries remain a derived tail.  The durable projection
removes tool bodies, provider replay metadata, anchors, continuation ids,
cache counters, runtime frames, and diagnostics."
  (let* ((projection (e-session-context-lifetime-projection store session-id))
         (generation (plist-get projection :generation))
         ;; Keep literal v2 records available to the compatibility summary
         ;; path.  They came from the session journal and are never rebuilt by
         ;; a production v2 encoder; new writes are v3-only.
         (legacy-promotion-ids (plist-get projection :promotion-frontier))
         (promotions
          (delq nil
                (mapcar
                 (lambda (entry)
                   (when (eq (plist-get entry :type) 'context-promotion)
                     (let ((record (e-session-aggregate-context-record entry)))
                       (when (and (equal (plist-get record :record-version)
                                         e-context-lifetime-record-version)
                                  (member (plist-get record :id)
                                          legacy-promotion-ids))
                         record))))
                 (e-session-current-path store session-id))))
         (curations (copy-tree (plist-get projection :curations))))
    (list :generation-id
          (and generation
               (e-context-lifetime-generation-id generation))
          :checkpoint
          (and generation
               (copy-tree
                (e-context-lifetime-generation-checkpoint generation)))
          :durable-tail
          (delq nil
                (mapcar
                 (lambda (entry)
                   (when-let ((message
                               (e-session-context-lifetime-durable-message
                                entry)))
                     (e-context-lifetime-portable-message message)))
                 summarized-entries))
          :promotions (copy-tree promotions)
          :curations curations
          :promotion-messages
          (mapcar #'e-context-lifetime-portable-message
                  (plist-get projection :promotion-messages))
          :promotion-message-entries
          (copy-tree (plist-get projection :promotion-message-entries))
          :promotion-frontier
          (copy-sequence (plist-get projection :promotion-frontier))
          :covered-session-boundary covered-session-boundary)))

(defun e-compaction-portable-summary-messages (portable-input)
  "Return provider-neutral summary messages for PORTABLE-INPUT.

This helper is intentionally a pure presentation of the portable input.  A
caller may pass it to the existing summary backend, but the backend sees only
the checkpoint, eligible durable tail, legacy v2 promotion data, and literal
v3 portable messages; it cannot receive runtime observation bodies or provider
continuation artifacts through this boundary."
  (let ((checkpoint (plist-get portable-input :checkpoint))
        (durable-tail (plist-get portable-input :durable-tail))
        (promotions (plist-get portable-input :promotions))
        (curation-messages
         (e-compaction--portable-curation-messages portable-input)))
    (append
     (list
      (list :role 'system
            :content
            "Compact only the portable semantic context. Preserve intent, decisions, selected facts, and unresolved work. Do not invent facts.")
      (list :role 'user
            :content
            (string-join
             (list (format "Portable checkpoint:\n%s"
                           (e-compaction--stringify checkpoint))
                   (format "Durable tail:\n%s"
                           (e-compaction--stringify durable-tail))
                   (format "Promoted facts and provenance:\n%s"
                           (e-compaction--stringify promotions)))
             "\n\n")))
     ;; V3 curation values are already canonical provider-neutral messages.
     ;; Keep them as real messages so compaction sees the exact role/content
     ;; projection instead of a second stringification or presentation layer.
     (copy-tree curation-messages))))

(defun e-compaction-prepared-summary-messages (preparation)
  "Return the summary request for PREPARATION's selected mode."
  (or (plist-get preparation :summary-messages)
      (e-compaction-summary-messages preparation)))

(defun e-compaction--portable-fact-messages (portable-input)
  "Return selected v2 promotion facts from PORTABLE-INPUT as messages."
  (e-context-lifetime-promotion-fact-messages
   (mapcar #'e-context-lifetime-promotion-from-record
           (plist-get portable-input :promotions))))

(defun e-compaction--portable-promotion-message-entries (portable-input)
  "Return typed selected v2/v3 messages from PORTABLE-INPUT.

The entry kind is internal compaction bookkeeping.  It preserves the v3
contract that every curation item remains a model message, while allowing the
legacy v2 fact projection to retain its historical value-based deduplication.
Older callers that only provide the untyped message list are treated as v2
compatibility input."
  (cond
   ((plist-member portable-input :promotion-message-entries)
    (copy-tree (plist-get portable-input :promotion-message-entries)))
   ;; Preparation values made before the typed internal field existed carry
   ;; only their flattened messages.  Keep those on the legacy deduplicating
   ;; path rather than silently changing their compatibility semantics.
   ((plist-member portable-input :promotion-messages)
    (mapcar (lambda (message) (list :kind 'v2 :message message))
            (copy-tree (plist-get portable-input :promotion-messages))))
   (t
    (append
     (mapcar (lambda (message) (list :kind 'v2 :message message))
             (e-compaction--portable-fact-messages portable-input))
     (mapcar (lambda (message) (list :kind 'v3 :message message))
             (mapcan #'e-context-lifetime-curation-messages
                     (mapcar #'e-context-lifetime-curation-from-record
                             (plist-get portable-input :curations))))))))

(defun e-compaction--portable-curation-messages (portable-input)
  "Return v3 literal messages from PORTABLE-INPUT without audit metadata."
  (if (plist-member portable-input :promotion-message-entries)
      (mapcar (lambda (entry)
                (e-context-lifetime-portable-message
                 (plist-get entry :message)))
              (seq-filter
               (lambda (entry)
                 (eq (plist-get entry :kind) 'v3))
               (plist-get portable-input :promotion-message-entries)))
    (mapcan #'e-context-lifetime-curation-messages
            (mapcar #'e-context-lifetime-curation-from-record
                    (plist-get portable-input :curations)))))

(defun e-compaction--portable-promotion-messages (portable-input)
  "Return all selected v2/v3 promotion messages from PORTABLE-INPUT."
  (mapcar (lambda (entry) (plist-get entry :message))
          (e-compaction--portable-promotion-message-entries portable-input)))

(defun e-compaction--append-portable-promotion-entries
    (checkpoint promotion-message-entries)
  "Return CHECKPOINT with typed PROMOTION-MESSAGE-ENTRIES absorbed.

Version-3 entries are exact append operations; version-2 entries retain their
historical value-based deduplication."
  (let ((messages (copy-tree checkpoint)))
    (dolist (entry promotion-message-entries)
      (let ((message (e-context-lifetime-portable-message
                      (plist-get entry :message))))
        (if (eq (plist-get entry :kind) 'v3)
            (setq messages (append messages (list message)))
          (unless (member message messages)
            (setq messages (append messages (list message)))))))
    messages))

(defun e-compaction--append-new-portable-facts (checkpoint portable-input)
  "Return CHECKPOINT with pre-boundary facts from PORTABLE-INPUT absorbed."
  (e-compaction--append-portable-promotion-entries
   checkpoint
   (e-compaction--portable-promotion-message-entries portable-input)))

(defun e-compaction-portable-checkpoint-from-summary
    (preparation summary)
  "Build a strict portable CHECKPOINT from SUMMARY and PREPARATION.

This is the bridge used by normal manual and automatic compaction.  The
summary replaces the captured portable prefix, and selected facts are
explicitly absorbed into the new checkpoint.  The durable tail was already
part of the summary input and is not copied a second time.  No provider
artifact or runtime observation is copied."
  (let* ((input (e-compaction--portable-preparation-input
                 preparation))
         (messages (list (list :role 'system :content summary))))
    (e-context-lifetime-portable-checkpoint
     (e-compaction--append-new-portable-facts messages input)
     t)))

(defun e-compaction-portable-context-messages
    (store session-id checkpoint covered-session-boundary)
  "Return canonical provider-compaction input after a portable boundary.

The returned messages are the new portable CHECKPOINT followed by the
canonical durable session tail strictly after COVERED-SESSION-BOUNDARY and
any active post-boundary promotion facts.  Runtime frames, tool bodies,
provider replay items, anchors, diagnostics, and other non-message journal
entries are never included.  This is the sole provider-neutral input builder
for optional opaque backend compaction."
  (let ((checkpoint (e-context-lifetime-portable-checkpoint checkpoint t))
        (path (e-session-current-path store session-id))
        (after-boundary nil)
        (tail nil))
    (unless (seq-some (lambda (entry)
                        (equal (plist-get entry :id)
                               covered-session-boundary))
                      path)
      (signal 'e-compaction-error
              (list "Portable compaction boundary is not on the current path"
                    covered-session-boundary)))
    (dolist (entry path)
      (when after-boundary
        (when-let ((message (e-session-context-lifetime-durable-message entry)))
          (push (e-context-lifetime-portable-message message) tail)))
      (when (equal (plist-get entry :id) covered-session-boundary)
        (setq after-boundary t)))
    (let* ((projection (e-session-context-lifetime-projection store session-id))
           (promotion-message-entries
            (or (plist-get projection :promotion-message-entries)
                (mapcar (lambda (message)
                          (list :kind 'v2 :message message))
                        (plist-get projection :promotion-messages))))
           (messages (append checkpoint (nreverse tail))))
      (dolist (entry promotion-message-entries)
        (let ((promotion-message
               (e-context-lifetime-portable-message
                (plist-get entry :message))))
          (if (eq (plist-get entry :kind) 'v3)
              (setq messages (append messages (list promotion-message)))
            (unless (member promotion-message messages)
              (setq messages (append messages (list promotion-message)))))))
      messages)))

(defun e-compaction--portable-boundary-on-path-p
    (store session-id boundary-id)
  "Return non-nil when prepared BOUNDARY-ID remains on SESSION-ID's path."
  (and boundary-id
       (seq-some (lambda (entry)
                   (equal (plist-get entry :id) boundary-id))
                 (e-session-current-path store session-id))))

(defun e-compaction--portable-preparation-input (preparation)
  "Validate and return the captured portable input in PREPARATION."
  (let ((input (plist-get preparation :portable-input)))
    (unless (and (e-context-lifetime--keyword-plist-p input)
                 (= (length input) 18)
                 (plist-member input :generation-id)
                 (plist-member input :checkpoint)
                 (plist-member input :durable-tail)
                 (plist-member input :promotions)
                 (plist-member input :curations)
                 (plist-member input :promotion-messages)
                 (plist-member input :promotion-message-entries)
                 (plist-member input :promotion-frontier)
                 (plist-member input :covered-session-boundary))
      (signal 'e-compaction-error
              (list "Invalid portable compaction preparation" preparation)))
    input))

(defun e-compaction--portable-promotion-frontier (store session-id)
  "Return the active promotion IDs on SESSION-ID's current path."
  (copy-sequence
   (plist-get (e-session-context-lifetime-projection store session-id)
              :promotion-frontier)))

(defun e-compaction-preflight-portable-boundary
    (store session-id preparation checkpoint)
  "Validate portable PREPARATION and return one application value.

This is a pure optimistic preflight.  It validates the captured generation,
branch, and promotion frontier immediately before an application service
performs its ordinary append(s); it does not mutate STORE.  The returned
checkpoint and captured input are detached so the append step cannot redo
domain validation or observe a different preparation."
  (let* ((input (e-compaction--portable-preparation-input preparation))
         (boundary (plist-get input :covered-session-boundary))
         (prepared-generation-id (plist-get input :generation-id))
         (current-generation
          (e-session-context-lifetime-current-generation store session-id))
         (current-generation-id
          (and current-generation
               (e-context-lifetime-generation-id current-generation)))
         (checkpoint
          (condition-case error
              (e-context-lifetime-portable-checkpoint checkpoint t)
            (e-context-lifetime-invalid-record
             (signal 'e-compaction-error
                     (list "Invalid portable checkpoint" error))))))
    (unless (equal prepared-generation-id current-generation-id)
      (signal 'e-compaction-error
              (list "Portable preparation is stale: generation changed"
                    prepared-generation-id current-generation-id)))
    (unless (e-compaction--portable-boundary-on-path-p
             store session-id boundary)
      (signal 'e-compaction-error
              (list "Portable preparation is stale: boundary left current path"
                    boundary)))
    (unless (equal (plist-get input :promotion-frontier)
                   (e-compaction--portable-promotion-frontier
                    store session-id))
      (signal 'e-compaction-error
              (list "Portable preparation is stale: promotion frontier changed")))
    (list :portable-input (copy-tree input)
          :checkpoint
          (condition-case error
              ;; `e-compaction-portable-checkpoint-from-summary' owns
              ;; absorption of the captured v3 entries.  Preflight receives
              ;; that complete checkpoint and must not append v3 items a
              ;; second time: unlike legacy v2 facts, equal v3 messages are
              ;; intentionally distinct items.  Keep the old v2 behavior for
              ;; callers that provide a checkpoint without its legacy facts.
              (e-context-lifetime-portable-checkpoint
               (e-compaction--append-portable-promotion-entries
                checkpoint
                (seq-filter
                 (lambda (entry)
                   (eq (plist-get entry :kind) 'v2))
                 (e-compaction--portable-promotion-message-entries input)))
               t)
            (e-context-lifetime-invalid-record
             (signal 'e-compaction-error
                     (list "Invalid portable checkpoint" error))))
          :covered-session-boundary boundary)))

(defun e-compaction--append-portable-boundary-preflighted
    (store session-id application)
  "Append already-preflighted portable APPLICATION for SESSION-ID.

Only the pure preflight may reject domain input.  This private append operation
is intentionally narrow so a harness can preflight before its ordinary audit
append and then perform both serialized appends without a second rejection."
  (let* ((input (plist-get application :portable-input))
         (checkpoint (plist-get application :checkpoint))
         (generation
          (e-context-lifetime-generation-create
           :id (format "generation:%s" (e-session-generate-ulid))
           :checkpoint checkpoint
           :covered-session-boundary
           (plist-get input :covered-session-boundary))))
    (e-session-append-context-generation store session-id generation)))

(cl-defun e-compaction-apply-portable-boundary
    (store session-id application)
  "Apply preflighted portable APPLICATION for SESSION-ID.

APPLICATION is the immutable value returned by
`e-compaction-preflight-portable-boundary'.  This append operation deliberately
does not repeat domain validation; callers that have not preflighted an
application fail at the narrow shape boundary rather than silently rebuilding
portable input."
  (unless (and (e-context-lifetime--keyword-plist-p application)
               (plist-member application :portable-input)
               (plist-member application :checkpoint)
               (plist-member application :covered-session-boundary))
    (signal 'e-compaction-error
            (list "Expected preflighted portable application" application)))
  (e-compaction--append-portable-boundary-preflighted
   store session-id application))

(provide 'e-compaction)

;;; e-compaction.el ends here

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

(defun e-compaction--stringify (value)
  "Return a compact text representation of VALUE."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((and (listp value) (plist-member value :content))
    (e-compaction--stringify (plist-get value :content)))
   ((listp value)
    (string-join
     (delq nil
           (mapcar (lambda (item)
                     (let ((text (e-compaction--stringify item)))
                       (unless (string-empty-p text) text)))
                   value))
     "\n"))
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

(cl-defun e-compaction-prepare
    (store session-id &key keep-recent-tokens instructions allow-split-turn
           (reason 'manual))
  "Prepare compaction data for SESSION-ID in STORE."
  (let* ((keep (or keep-recent-tokens e-compaction-keep-recent-tokens))
         (entries (e-compaction--message-entries store session-id))
         (previous (e-session-latest-valid-compaction store session-id))
         (previous-boundary (plist-get previous :first-kept-entry-id))
         (boundary-roles (if allow-split-turn
                             '(user assistant tool-call)
                           '(user)))
         (boundary
          (e-compaction--select-boundary
           entries keep boundary-roles previous-boundary)))
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
             (resources (e-compaction--affected-resources to-summarize)))
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
                    :affected-resources resources))))))

(provide 'e-compaction)

;;; e-compaction.el ends here

;;; e-openai-decoder.el --- OpenAI stream decoding -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns bounded Responses and Chat Completions stream decoding.  It returns
;; provider-neutral event items and never performs transport or facade work.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'e-openai-diagnostics)

(defun e-openai-decoder--parse-json (value)
  "Parse VALUE as JSON into plist data."
  (json-parse-string value
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun e-openai-decoder--function-call-item-p (item)
  "Return non-nil when ITEM is a Responses function-call item."
  (and (listp item)
       (member (plist-get item :type)
               '("function_call" "tool_call" function_call tool_call))))

(defun e-openai-decoder--context-curation-name-p (name)
  "Return non-nil when NAME is the reserved curation carrier."
  (member name '("context-curate" context-curate)))

(defun e-openai-decoder--encrypted-reasoning-item-p (item)
  "Return non-nil when ITEM is replayable encrypted OpenAI reasoning."
  (and (listp item)
       (member (plist-get item :type) '("reasoning" reasoning))
       (stringp (plist-get item :encrypted_content))))

(defun e-openai-decoder--parse-function-arguments (arguments)
  "Parse JSON ARGUMENTS from a Responses function call."
  (cond
   ((and (stringp arguments) (not (string-empty-p arguments)))
    (e-openai-decoder--parse-json arguments))
   ((listp arguments) arguments)
   (t nil)))

(defconst e-openai-decoder--context-curation-duplicate-correction
  "Curation was already handled for the currently presented labeled sources. Continue normally; call context-curate again only after new labeled sources are presented."
  "Fixed Responses output used for one duplicate-curation recovery.")

(defun e-openai-decoder--context-curation-replay-bundle
    (arguments call-id output)
  "Return opaque Responses replay state for ARGUMENTS, CALL-ID, and OUTPUT."
  (list
   (list :type 'provider-replay-item
         :provider-id 'openai
         :full-replay-only t
         :item (list :type "function_call"
                     :call_id call-id
                     :name "context-curate"
                     :arguments
                     (json-encode
                      (or arguments (make-hash-table :test 'equal)))))
   (list :type 'provider-replay-item
         :provider-id 'openai
         :item (list :type "function_call_output"
                     :call_id call-id
                     :output output))))

(defun e-openai-decoder--context-curation-effect (arguments &optional call-id)
  "Return the core-owned curation effect decoded from wire ARGUMENTS.

The wire object is deliberately passed through without adding frame or
provider identity.  Core binds its labels to the live frame at completion."
  (let* ((arguments (e-openai-decoder--parse-function-arguments arguments))
         (effect (list :type 'context-curate
                       :arguments arguments)))
    ;; Responses requires a function_call_output for every function_call when
    ;; a subsequent request continues from its response id.  Keep that wire
    ;; acknowledgement opaque and paired with the in-memory response; the
    ;; core curation effect remains exact and provider-neutral, while the
    ;; session projection removes this replay metadata from later durable
    ;; context.
    (when (and (stringp call-id) (not (string-empty-p call-id)))
      (let* ((normal-bundle
              (e-openai-decoder--context-curation-replay-bundle
               arguments call-id ""))
             (corrective-bundle
              (e-openai-decoder--context-curation-replay-bundle
               arguments call-id
               e-openai-decoder--context-curation-duplicate-correction))
             (output-replay (cadr normal-bundle)))
        ;; Retain the singular output field for existing consumers while the
        ;; plural field carries the complete call/output pair for full replay.
        (setq effect
              (plist-put effect :provider-replay-item output-replay))
        (setq effect
              (plist-put effect :provider-replay-items
                         normal-bundle))
        (setq effect
              (plist-put effect :provider-corrective-replay-items
                         corrective-bundle))))
    effect))

(defun e-openai-decoder--sequence-list (value)
  "Return VALUE as a list when it is a JSON array sequence."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun e-openai-decoder--content-text (content)
  "Return concatenated output text from Responses CONTENT."
  (string-join
   (delq nil
         (mapcar
          (lambda (part)
            (pcase (plist-get part :type)
              ((or "output_text" "text") (plist-get part :text))
              ("refusal" (plist-get part :refusal))))
          (e-openai-decoder--sequence-list content)))
   ""))

(defun e-openai-decoder--message-item-text (item)
  "Return assistant text from a Responses message ITEM."
  (when (equal (plist-get item :type) "message")
    (let ((text (e-openai-decoder--content-text (plist-get item :content))))
      (unless (string-empty-p text)
        text))))

(defun e-openai-decoder--event-summary (event item)
  "Return a compact diagnostics summary for provider EVENT and parsed ITEM."
  (let ((provider-item (plist-get event :item))
        (provider-part (plist-get event :part)))
    (list :event-type (plist-get event :type)
          :item-type (or (plist-get provider-item :type)
                         (plist-get provider-part :type))
          :parsed-type (plist-get item :type))))

(defun e-openai-decoder--response-error-message (event)
  "Return a readable error message for a Responses failure EVENT."
  (let* ((response (plist-get event :response))
         (error (or (plist-get response :error)
                    (plist-get event :error)))
         (code (or (and (listp error) (plist-get error :code))
                   (plist-get event :code)))
         (message
          (or (and (listp error) (plist-get error :message))
              (and (stringp error) error)
              (plist-get event :message))))
    (if (stringp message)
        (let ((message (e-openai-diagnostics-bounded-text message)))
          (if (stringp code)
              (format "%s: %s" code message)
            message))
      (e-openai-diagnostics-bounded-string event))))

(defun e-openai-decoder--number-or-nil (value)
  "Return VALUE when it is numeric, otherwise nil."
  (when (numberp value)
    value))

(defun e-openai-decoder--usage-item (usage)
  "Return provider-neutral token usage item for Responses USAGE."
  (when (consp usage)
    (let* ((input-details (plist-get usage :input_tokens_details))
           (output-details (plist-get usage :output_tokens_details))
           (normalized
            (list
             :input-tokens
             (e-openai-decoder--number-or-nil
              (plist-get usage :input_tokens))
             :cached-input-tokens
             (e-openai-decoder--number-or-nil
              (plist-get input-details :cached_tokens))
             :cache-creation-input-tokens
             (e-openai-decoder--number-or-nil
              (plist-get input-details :cache_write_tokens))
             :output-tokens
             (e-openai-decoder--number-or-nil
              (plist-get usage :output_tokens))
             :reasoning-output-tokens
             (e-openai-decoder--number-or-nil
              (plist-get output-details :reasoning_tokens))
             :total-tokens
             (e-openai-decoder--number-or-nil
              (plist-get usage :total_tokens)))))
      (list :type 'token-usage :usage normalized))))

(defun e-openai-decoder--anchor-candidate-item
    (response &optional prompt-layout-revision reasoning-identity)
  "Return provider anchor candidate item from completed RESPONSE.
PROMPT-LAYOUT-REVISION records the request layout carried by the response;
REASONING-IDENTITY fences the effective effort and summary pair."
  (when-let ((response-id (and (consp response)
                               (plist-get response :id))))
    (when (stringp response-id)
      (list :type 'provider-anchor-candidate
            :provider-id 'openai
            :metadata
            (append
             (list :response-id response-id)
             (when prompt-layout-revision
               (list :prompt-layout-revision prompt-layout-revision))
             (when reasoning-identity
               (list :reasoning-identity reasoning-identity)))))))

(defun e-openai-decoder--json-error-item (stream-text)
  "Return a backend error item when STREAM-TEXT is a JSON error response."
  (when (string-prefix-p "{" (string-trim-left stream-text))
    (let* ((payload (e-openai-decoder--parse-json stream-text))
           (error (plist-get payload :error))
           (detail (plist-get payload :detail))
           (code (and (listp error)
                      (or (plist-get error :code)
                          (plist-get error :type))))
           (message (cond
                     ((listp error) (plist-get error :message))
                     ((stringp error) error)
                     ((stringp detail) detail))))
      (when message
        (list :type 'backend-error
              :content (if (stringp code)
                           (format "%s: %s" code message)
                         message)
              :payload payload)))))

(defun e-openai-decoder--text-preview (text &optional limit)
  "Return a compact single-line preview of TEXT.
LIMIT defaults to 240 characters."
  (let* ((limit (or limit 240))
         (preview (string-trim
                   (replace-regexp-in-string
                    "[[:space:]\n\r\t]+" " " (or text "")))))
    (if (> (length preview) limit)
        (concat (substring preview 0 limit) "...")
      preview)))

(defun e-openai-decoder--html-text-preview (html &optional limit)
  "Return a compact text preview for HTML.
LIMIT defaults to 240 characters."
  (e-openai-decoder--text-preview
   (replace-regexp-in-string "<[^>]+>" " " (or html ""))
   limit))

(defun e-openai-decoder--non-stream-error-item (stream-text &optional wire-api)
  "Return a backend error item for non-empty non-SSE STREAM-TEXT.
WIRE-API identifies the expected OpenAI streaming protocol."
  (let* ((raw (or stream-text ""))
         (trimmed (string-trim-left raw))
         (html (string-prefix-p "<" trimmed))
         (preview (if html
                      (e-openai-decoder--html-text-preview stream-text)
                    (e-openai-decoder--text-preview stream-text)))
         (kind (if html 'html 'text)))
    (unless (string-empty-p (string-trim raw))
      (list :type 'backend-error
            :content (format "Provider returned %s instead of a %s stream: %s"
                             (if html "HTML" "non-stream text")
                             (if (eq wire-api 'chat-completion)
                                 "Chat Completions"
                               "Responses")
                             (if (string-empty-p preview)
                                 "(no text content)"
                               preview))
            :payload (list :response-kind kind
                           :preview preview)))))

(defun e-openai-decoder--sse-response-p (text)
  "Return non-nil when TEXT contains at least one SSE data field."
  (and (stringp text)
       (string-match-p "\\(?:\\`\\|\n\\)data:" text)))

(defun e-openai-decoder--sse-chunks (text)
  "Return SSE event chunks from TEXT with LF or CRLF framing."
  (split-string text "\r?\n\r?\n" t))

(defun e-openai-decoder--sse-lines (chunk)
  "Return lines from SSE CHUNK with LF or CRLF framing."
  (split-string chunk "\r?\n"))

(defun e-openai-decoder--incomplete-reason (event)
  "Return the backend-neutral terminal reason for incomplete EVENT."
  (let ((reason (plist-get
                 (plist-get (plist-get event :response) :incomplete_details)
                 :reason)))
    (cond
     ((member reason '("max_output_tokens" "max_tokens")) 'length)
     ((equal reason "content_filter") 'content-filter)
     ((stringp reason)
      (intern (replace-regexp-in-string "_" "-" reason)))
     (t 'incomplete))))

(defun e-openai-decoder--event-item (event)
  "Map parsed Responses EVENT to one backend-neutral item, or nil."
  (let ((type (plist-get event :type)))
    (cond
     ((equal type "response.output_text.delta")
      (list :type 'assistant-delta
            :content (plist-get event :delta)))
     ((equal type "response.output_text.done")
      (list :type 'assistant-message
            :content (plist-get event :text)))
     ((equal type "response.refusal.delta")
      (list :type 'assistant-delta
            :content (plist-get event :delta)))
     ((equal type "response.refusal.done")
      (list :type 'assistant-message
            :content (plist-get event :refusal)))
     ((equal type "response.reasoning_summary_text.delta")
      (list :type 'reasoning-delta
            :stream-kind 'summary
            :content (or (plist-get event :delta)
                         (plist-get event :text))))
     ((equal type "response.reasoning_text.delta")
      (list :type 'reasoning-raw-delta
            :stream-kind 'raw
            :content (or (plist-get event :delta)
                         (plist-get event :text))))
     ((and (equal type "response.output_item.done")
           (e-openai-decoder--encrypted-reasoning-item-p
            (plist-get event :item)))
      (list :type 'provider-replay-item
            :provider-id 'openai
            :item (copy-tree (plist-get event :item))))
     ((and (equal type "response.output_item.done")
           (e-openai-decoder--function-call-item-p (plist-get event :item)))
      (let ((item (plist-get event :item)))
        (if (e-openai-decoder--context-curation-name-p
             (plist-get item :name))
            (e-openai-decoder--context-curation-effect
             (plist-get item :arguments)
             (or (plist-get item :call_id)
                 (plist-get item :call-id)))
          (list :type 'tool-call
                :id (or (plist-get item :call_id)
                        (plist-get item :id))
                :name (plist-get item :name)
                :arguments (e-openai-decoder--parse-function-arguments
                            (plist-get item :arguments))))))
     ((and (equal type "response.output_item.done")
           (e-openai-decoder--message-item-text (plist-get event :item)))
      (list :type 'assistant-message-candidate
            :content (e-openai-decoder--message-item-text
                      (plist-get event :item))
            :source 'output-item))
     ((and (equal type "response.content_part.done")
           (member (plist-get (plist-get event :part) :type)
                   '("output_text" "text")))
      (list :type 'assistant-message-candidate
            :content (plist-get (plist-get event :part) :text)
            :source 'content-part))
     ((member type '("response.completed" "response.done"))
      (list :type 'done :reason 'stop))
     ((equal type "response.incomplete")
      (list :type 'done :reason (e-openai-decoder--incomplete-reason event)))
     ((equal type "response.failed")
      (list :type 'backend-error
            :content (e-openai-decoder--response-error-message event)
            :payload event))
     ((equal type "error")
      (list :type 'backend-error
            :content (e-openai-decoder--response-error-message event)
            :payload event))
     (t nil))))

(defun e-openai-codex-parse-stream
    (stream-text &optional prompt-layout-revision reasoning-identity)
  "Parse Codex Responses STREAM-TEXT into backend-neutral items.
PROMPT-LAYOUT-REVISION and REASONING-IDENTITY are stored on emitted
continuation anchors."
  (e-openai-diagnostics-append-raw-response stream-text)
  (let ((items nil)
        (event-summaries nil)
        (assistant-message-seen nil)
        (assistant-message-candidate nil))
    (cl-labels
        ((handle-item
          (item)
          (pcase (plist-get item :type)
            ('assistant-message
             (setq assistant-message-seen t)
             (push item items))
            ('assistant-message-candidate
             (unless assistant-message-candidate
               (setq assistant-message-candidate
                     (list :type 'assistant-message
                           :content (plist-get item :content)))))
            ('done
             (unless assistant-message-seen
               (when assistant-message-candidate
                 (push assistant-message-candidate items)
                 (setq assistant-message-seen t)))
             (push item items))
            (_
             (push item items)))))
      (dolist (chunk (e-openai-decoder--sse-chunks stream-text))
        (let ((data-lines nil))
          (dolist (line (e-openai-decoder--sse-lines chunk))
            (when (string-prefix-p "data:" line)
              (push (string-trim (substring line 5)) data-lines)))
          (when data-lines
            (let ((data (string-join (nreverse data-lines) "\n")))
              (unless (or (string-empty-p data) (equal data "[DONE]"))
                (let* ((event (e-openai-decoder--parse-json data))
                       (item (e-openai-decoder--event-item event))
                       (completed-event-p
                        (member (plist-get event :type)
                                '("response.completed" "response.done")))
                       (response (plist-get event :response))
                       (anchor-candidate-item
                        (when completed-event-p
                          (e-openai-decoder--anchor-candidate-item
                           response prompt-layout-revision
                           reasoning-identity)))
                       (usage-item
                        (when completed-event-p
                          (e-openai-decoder--usage-item
                           (plist-get response :usage)))))
                  (push (e-openai-decoder--event-summary event item)
                        event-summaries)
                  (when usage-item
                    (handle-item usage-item))
                  (when anchor-candidate-item
                    (handle-item anchor-candidate-item))
                  (when item
                    (handle-item item)))))))))
    (unless assistant-message-seen
      (when assistant-message-candidate
        (push assistant-message-candidate items)))
    (unless items
      (when-let ((error-item (e-openai-decoder--json-error-item stream-text)))
        (push error-item items)))
    (unless (or items (e-openai-decoder--sse-response-p stream-text))
      (when-let ((error-item
                  (e-openai-decoder--non-stream-error-item stream-text)))
        (push error-item items)))
    (e-openai-diagnostics-record-stream stream-text (nreverse event-summaries))
    (nreverse items)))


(defun e-openai-decoder--chat-choice-delta (choice)
  "Return CHOICE delta plist from a Chat Completions chunk."
  (plist-get choice :delta))

(defun e-openai-decoder--chat-delta-content (delta)
  "Return text content from Chat Completions DELTA."
  (let ((content (or (plist-get delta :content)
                     (plist-get delta :refusal))))
    (when (stringp content) content)))

(defun e-openai-decoder--chat-delta-tool-calls (delta)
  "Return tool-call deltas from Chat Completions DELTA."
  (e-openai-decoder--sequence-list (plist-get delta :tool_calls)))

(defun e-openai-decoder--chat-usage-item (usage)
  "Return provider-neutral token usage item for Chat Completions USAGE."
  (when (consp usage)
    (let* ((prompt-details (plist-get usage :prompt_tokens_details))
           (completion-details (plist-get usage :completion_tokens_details))
           (normalized
            (list
             :input-tokens
             (e-openai-decoder--number-or-nil
              (plist-get usage :prompt_tokens))
             :cached-input-tokens
             (e-openai-decoder--number-or-nil
              (plist-get prompt-details :cached_tokens))
             :cache-creation-input-tokens
             (e-openai-decoder--number-or-nil
              (plist-get prompt-details :cache_write_tokens))
             :output-tokens
             (e-openai-decoder--number-or-nil
              (plist-get usage :completion_tokens))
             :reasoning-output-tokens
             (e-openai-decoder--number-or-nil
              (plist-get completion-details :reasoning_tokens))
             :total-tokens
             (e-openai-decoder--number-or-nil
              (plist-get usage :total_tokens)))))
      (list :type 'token-usage :usage normalized))))

(defun e-openai-decoder--chat-tool-call-key (tool-call fallback-index)
  "Return stable accumulator key for TOOL-CALL with FALLBACK-INDEX."
  (or (plist-get tool-call :index)
      (plist-get tool-call :id)
      fallback-index))

(defun e-openai-decoder--chat-merge-tool-call-delta
    (state tool-call fallback-index)
  "Merge one TOOL-CALL delta into STATE and return its accumulator."
  (let* ((key (e-openai-decoder--chat-tool-call-key tool-call fallback-index))
         (existing (or (assoc key state)
                       (let ((entry (cons key (list :arguments ""))))
                         (push entry state)
                         entry)))
         (acc (cdr existing))
         (function (plist-get tool-call :function)))
    (when (plist-get tool-call :id)
      (setq acc (plist-put acc :id (plist-get tool-call :id))))
    (when (plist-get function :name)
      (setq acc (plist-put acc :name (plist-get function :name))))
    (when (plist-member function :arguments)
      (setq acc (plist-put acc :arguments
                           (concat (or (plist-get acc :arguments) "")
                                   (or (plist-get function :arguments) "")))))
    (setcdr existing acc)
    (cons state acc)))

(defun e-openai-decoder--chat-finish-reason-symbol (reason)
  "Return provider-neutral done reason for Chat Completions REASON."
  (cond
   ((or (null reason) (equal reason "stop")) 'stop)
   ((equal reason "length") 'length)
   ((equal reason "tool_calls") 'tool-calls)
   ((equal reason "content_filter") 'content-filter)
   (t (intern (replace-regexp-in-string "_" "-" (format "%s" reason))))))

(defun e-openai-decoder--chat-tool-call-finish-p (reason)
  "Return non-nil when REASON means accumulated tool calls are complete."
  (equal reason "tool_calls"))

(defun e-openai-chat-completion-parse-stream (stream-text)
  "Parse Chat Completions STREAM-TEXT into backend-neutral items."
  (e-openai-diagnostics-append-raw-response stream-text)
  (let ((items nil)
        (text-parts nil)
        (tool-state nil)
        (done-seen nil))
    (cl-labels
        ((emit-tool-calls
          ()
          (dolist (entry (nreverse tool-state))
            (let* ((acc (cdr entry))
                   (arguments (plist-get acc :arguments)))
              (when (and (plist-get acc :id)
                         (plist-get acc :name))
                (if (e-openai-decoder--context-curation-name-p
                     (plist-get acc :name))
                    (push (e-openai-decoder--context-curation-effect
                           arguments
                           (plist-get acc :id))
                          items)
                  (push (list :type 'tool-call
                              :id (plist-get acc :id)
                              :name (plist-get acc :name)
                              :arguments
                              (e-openai-decoder--parse-function-arguments
                               arguments))
                        items)))))))
      (dolist (chunk (e-openai-decoder--sse-chunks stream-text))
        (let ((data-lines nil))
          (dolist (line (e-openai-decoder--sse-lines chunk))
            (when (string-prefix-p "data:" line)
              (push (string-trim (substring line 5)) data-lines)))
          (when data-lines
            (let ((data (string-join (nreverse data-lines) "\n")))
              (cond
               ((or (string-empty-p data) (equal data "[DONE]")) nil)
               (t
                (let* ((event (e-openai-decoder--parse-json data))
                       (usage-item
                        (e-openai-decoder--chat-usage-item
                         (plist-get event :usage))))
                  (when usage-item
                    (push usage-item items))
                  (dolist (choice (e-openai-decoder--sequence-list
                                   (plist-get event :choices)))
                    (let* ((delta (e-openai-decoder--chat-choice-delta
                                   choice))
                           (content
                            (e-openai-decoder--chat-delta-content delta))
                           (tool-calls
                            (e-openai-decoder--chat-delta-tool-calls delta))
                           (finish-reason (plist-get choice :finish_reason)))
                      (when content
                        (push content text-parts)
                        (push (list :type 'assistant-delta
                                    :content content)
                              items))
                      (cl-loop for tool-call in tool-calls
                               for index from 0
                               do (let ((merged
                                         (e-openai-decoder--chat-merge-tool-call-delta
                                          tool-state tool-call index)))
                                    (setq tool-state (car merged))))
                      (when finish-reason
                        (unless done-seen
                          (if (e-openai-decoder--chat-tool-call-finish-p
                               finish-reason)
                              (emit-tool-calls)
                            (when text-parts
                              (push (list :type 'assistant-message
                                          :content
                                          (apply #'concat
                                                 (nreverse text-parts)))
                                    items)))
                          (push (list :type 'done
                                      :reason
                                      (e-openai-decoder--chat-finish-reason-symbol
                                       finish-reason))
                                items)
                          (setq done-seen t)))))))))))))
    (unless items
      (when-let ((error-item (e-openai-decoder--json-error-item stream-text)))
        (push error-item items)))
    (nreverse items)))



(defun e-openai-decoder-event-items
    (event emit-anchor &optional prompt-layout-revision reasoning-identity)
  "Return bounded provider-neutral items for one Responses EVENT.
When EMIT-ANCHOR is nil, completed response ids remain transport-local."
  (let* ((completed-event-p
          (member (plist-get event :type)
                  '("response.completed" "response.done")))
         (response (plist-get event :response))
         (usage-item
          (when completed-event-p
            (e-openai-decoder--usage-item
             (plist-get response :usage))))
         (anchor-candidate-item
          (when (and emit-anchor completed-event-p)
            (e-openai-decoder--anchor-candidate-item
             response prompt-layout-revision reasoning-identity)))
         (event-item (e-openai-decoder--event-item event)))
    (delq nil (list usage-item anchor-candidate-item event-item))))

(defun e-openai-decoder-parse-json (value)
  "Decode JSON VALUE into the adapter's bounded plist representation."
  (e-openai-decoder--parse-json value))

(defun e-openai-decoder-sse-response-p (text)
  "Return non-nil when TEXT contains at least one SSE data field."
  (e-openai-decoder--sse-response-p text))

(defun e-openai-decoder-text-preview (text &optional limit)
  "Return a compact single-line preview of TEXT."
  (e-openai-decoder--text-preview text limit))

(defun e-openai-decoder-non-stream-error-item (stream-text &optional wire-api)
  "Return a backend error item for non-empty non-SSE STREAM-TEXT."
  (e-openai-decoder--non-stream-error-item stream-text wire-api))

(provide 'e-openai-decoder)

;;; e-openai-decoder.el ends here

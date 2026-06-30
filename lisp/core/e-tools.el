;;; e-tools.el --- Tool registry for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure tool registry and dispatch for core tool-call handling.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'e-request)

(cl-defstruct (e-tools-registry (:constructor e-tools-registry-create))
  (tools (make-hash-table :test 'equal))
  (order nil))

(cl-defstruct (e-tools-request (:constructor e-tools-request-create))
  cancel
  metadata)

(cl-defstruct (e-tool-lifecycle (:constructor e-tool-lifecycle-create))
  prepare
  start)

(defvar e-tools--current-context nil
  "Context dynamically visible while a tool implementation starts.")

(define-error 'e-tools-no-active-registry
  "No active tool registry is available")
(define-error 'e-tools-recursive-call
  "Recursive nested tool call rejected")
(define-error 'e-tools-nested-tool-error
  "Nested tool returned a structured error")
(define-error 'e-tools-nested-tool-budget-exceeded
  "Nested tool call budget exceeded")
(define-error 'e-tools-nested-long-tool-rejected
  "Long nested tool call rejected")
(define-error 'e-tools-blocking-handler-rejected
  "Long synchronous tool handler rejected in interactive execution")

(defconst e-tools-nested-tool-default-budget 20
  "Default maximum number of nested tool calls per parent tool execution.")

(defun e-tools-current-context ()
  "Return the current tool start context, or nil."
  e-tools--current-context)

(defun e-tools-current-registry ()
  "Return the active tool registry from `e-tools-current-context'."
  (let ((registry (plist-get (e-tools-current-context) :tools)))
    (unless (e-tools-registry-p registry)
      (signal 'e-tools-no-active-registry
              (list "No active tool registry is available")))
    registry))

(defun e-tools-current-tool-call ()
  "Return the current parent tool call from `e-tools-current-context'."
  (plist-get (e-tools-current-context) :tool-call))

(defun e-tools--current-nested-state ()
  "Return the mutable nested tool state for the current context."
  (or (plist-get (e-tools-current-context) :nested-tool-state)
      (signal 'e-tools-no-active-registry
              (list "No active tool execution context is available"))))

(defun e-tools--next-nested-call-id (options)
  "Return the next nested call id using OPTIONS when supplied."
  (or (plist-get options :call-id)
      (let* ((parent (e-tools-current-tool-call))
             (parent-id (or (plist-get parent :id) "tool-call"))
             (state (e-tools--current-nested-state))
             (next (1+ (or (plist-get state :sequence) 0))))
        (plist-put state :sequence next)
        (format "%s/nested-%d" parent-id next))))

(defun e-tools--check-nested-budget ()
  "Increment and enforce the current nested tool call budget."
  (let* ((context (e-tools-current-context))
         (state (e-tools--current-nested-state))
         (budget (or (plist-get context :nested-tool-budget)
                     e-tools-nested-tool-default-budget))
         (count (1+ (or (plist-get state :count) 0))))
    (when (> count budget)
      (signal 'e-tools-nested-tool-budget-exceeded
              (list "Nested tool call budget exceeded")))
    (plist-put state :count count)
    count))

(defun e-tools--nested-context (context)
  "Return CONTEXT for a nested tool call."
  context)

(defun e-tools-cancel-request (request)
  "Cancel REQUEST when it has a tool cancellation function."
  (when-let ((cancel (and (e-tools-request-p request)
                          (e-tools-request-cancel request))))
    (funcall cancel)))

(defun e-tools--unexpected-on-event-keyword-error-p (err)
  "Return non-nil when ERR is a legacy start-function keyword rejection."
  (and (eq (car err) 'error)
       (string-match-p
        "\\`Keyword argument :on-event not one of "
        (error-message-string err))))

(defun e-tools--apply-start-with-optional-event (start arguments on-event)
  "Apply START to ARGUMENTS, passing ON-EVENT when accepted."
  (if (not on-event)
      (apply start arguments)
    (condition-case err
        (apply start (append arguments (list :on-event on-event)))
      (error
       (if (e-tools--unexpected-on-event-keyword-error-p err)
           (apply start arguments)
         (signal (car err) (cdr err)))))))

(defconst e-tools-cheap-blocking-classes '(nil cheap)
  "Tool blocking classes allowed to run through synchronous handlers.")

(defconst e-tools-long-blocking-classes
  '(network process helper filesystem render unknown)
  "Tool blocking classes that must provide async start functions in hot paths.")

(defun e-tools--blocking-class (tool)
  "Return TOOL blocking class metadata."
  (let ((metadata (plist-get tool :metadata)))
    (or (plist-get metadata :blocking-class)
        (plist-get metadata :blocking_class)
        (plist-get metadata :blocking))))

(defun e-tools-long-blocking-class-p (class)
  "Return non-nil when CLASS names a long blocking family."
  (memq class e-tools-long-blocking-classes))

(defun e-tools-cheap-blocking-class-p (class)
  "Return non-nil when CLASS may use a synchronous handler."
  (memq class e-tools-cheap-blocking-classes))

(cl-defun e-tools-register
    (registry &key name description parameters handler start metadata blocking-class)
  "Register tool NAME in REGISTRY.
DESCRIPTION, PARAMETERS, HANDLER, START, and METADATA describe the tool.
BLOCKING-CLASS may be `cheap', `network', `process', `helper', `filesystem',
`render', or `unknown'.
HANDLER is a synchronous implementation.  START is a callback-driven async
implementation."
  (unless (or (functionp handler) (functionp start))
    (signal 'wrong-type-argument (list 'functionp (or handler start))))
  (when blocking-class
    (setq metadata (plist-put metadata :blocking-class blocking-class)))
  (unless (gethash name (e-tools-registry-tools registry))
    (setf (e-tools-registry-order registry)
          (append (e-tools-registry-order registry) (list name))))
  (puthash name
           (list :name name
                 :description description
                 :parameters parameters
                 :metadata metadata
                 :handler handler
                 :start start)
           (e-tools-registry-tools registry)))

(defun e-tools--empty-json-object ()
  "Return an empty object suitable for `json-encode'."
  (make-hash-table :test 'equal))

(defun e-tools--plist-p (value)
  "Return non-nil when VALUE is a keyword plist."
  (and (listp value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun e-tools--json-key (key)
  "Return stable JSON object key text for KEY."
  (cond
   ((keywordp key)
    (substring (symbol-name key) 1))
   ((symbolp key)
    (symbol-name key))
   ((stringp key)
    key)
   (t
    (format "%s" key))))

(defun e-tools--sort-json-object (entries)
  "Return ENTRIES sorted by their string keys."
  (sort entries (lambda (left right)
                  (string< (car left) (car right)))))

(defun e-tools--json-normalize (value)
  "Return VALUE in a deterministic shape suitable for `json-encode'."
  (cond
   ((or (stringp value)
        (numberp value)
        (eq value t)
        (eq value :json-false)
        (null value))
    value)
   ((keywordp value)
    (substring (symbol-name value) 1))
   ((symbolp value)
    (symbol-name value))
   ((vectorp value)
    (vconcat (mapcar #'e-tools--json-normalize value)))
   ((hash-table-p value)
    (let (entries)
      (maphash
       (lambda (key item)
         (push (cons (e-tools--json-key key)
                     (e-tools--json-normalize item))
               entries))
       value)
      (e-tools--sort-json-object entries)))
   ((e-tools--plist-p value)
    (let (entries)
      (while value
        (push (cons (e-tools--json-key (pop value))
                    (e-tools--json-normalize (pop value)))
              entries))
      (e-tools--sort-json-object entries)))
   ((and (listp value)
         (cl-every #'consp value))
    (e-tools--sort-json-object
     (mapcar (lambda (entry)
               (cons (e-tools--json-key (car entry))
                     (e-tools--json-normalize (cdr entry))))
             value)))
   ((listp value)
    (vconcat (mapcar #'e-tools--json-normalize value)))
   (t
    (signal 'wrong-type-argument (list 'json-serializable-p value)))))

(defun e-tools-result-content-text (content)
  "Return the model-visible text representation for tool result CONTENT."
  (if (stringp content)
      content
    (condition-case nil
        (json-encode (e-tools--json-normalize content))
      (error
       (prin1-to-string content)))))

(defun e-tools--string-byte-prefix (text max-bytes)
  "Return TEXT prefix limited to MAX-BYTES UTF-8 bytes."
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

(defun e-tools--preview-note-truncated (state)
  "Record truncation in preview STATE."
  (plist-put state :truncated t))

(defun e-tools--preview-normalize (value state depth)
  "Return a bounded JSON-normalizable preview of VALUE.
STATE carries shared truncation metadata.  DEPTH is the remaining traversal
budget."
  (let ((max-string-bytes (plist-get state :max-string-bytes))
        (max-items (plist-get state :max-items)))
    (cond
     ((stringp value)
      (if (> (string-bytes value) max-string-bytes)
          (progn
            (e-tools--preview-note-truncated state)
            (concat (e-tools--string-byte-prefix value max-string-bytes)
                    "…"))
        value))
     ((or (numberp value) (eq value t) (eq value :json-false) (null value))
      value)
     ((or (keywordp value) (symbolp value))
      (symbol-name value))
     ((<= depth 0)
      (e-tools--preview-note-truncated state)
      "…")
     ((vectorp value)
      (let* ((items (append value nil))
             (limited (seq-take items max-items)))
        (when (> (length items) max-items)
          (e-tools--preview-note-truncated state))
        (vconcat (mapcar (lambda (item)
                           (e-tools--preview-normalize
                            item state (1- depth)))
                         limited))))
     ((hash-table-p value)
      (let (entries)
        (maphash
         (lambda (key item)
           (push (cons (e-tools--json-key key) item) entries))
         value)
        (setq entries (e-tools--sort-json-object entries))
        (when (> (length entries) max-items)
          (e-tools--preview-note-truncated state))
        (mapcar (lambda (entry)
                  (cons (car entry)
                        (e-tools--preview-normalize
                         (cdr entry) state (1- depth))))
                (seq-take entries max-items))))
     ((e-tools--plist-p value)
      (let (entries)
        (while value
          (push (cons (e-tools--json-key (pop value)) (pop value)) entries))
        (setq entries (e-tools--sort-json-object entries))
        (when (> (length entries) max-items)
          (e-tools--preview-note-truncated state))
        (mapcar (lambda (entry)
                  (cons (car entry)
                        (e-tools--preview-normalize
                         (cdr entry) state (1- depth))))
                (seq-take entries max-items))))
     ((listp value)
      (when (> (length value) max-items)
        (e-tools--preview-note-truncated state))
      (vconcat (mapcar (lambda (item)
                         (e-tools--preview-normalize
                          item state (1- depth)))
                       (seq-take value max-items))))
     (t
      (let ((printed (prin1-to-string value)))
        (if (> (string-bytes printed) max-string-bytes)
            (progn
              (e-tools--preview-note-truncated state)
              (concat (e-tools--string-byte-prefix printed max-string-bytes)
                      "…"))
          printed))))))

(defun e-tools-result-content-preview (content max-bytes &optional max-items max-depth)
  "Return bounded display preview metadata for tool result CONTENT.
MAX-BYTES bounds the returned text.  MAX-ITEMS and MAX-DEPTH bound structured
content traversal so display paths do not force construction of unbounded model
strings."
  (let* ((limit (max 0 (or max-bytes 0)))
         (depth (or max-depth 4))
         (state (list :max-string-bytes limit
                      :max-items (or max-items 40)
                      :truncated nil))
         (text (if (stringp content)
                   content
                 (condition-case nil
                     (json-encode
                      (e-tools--preview-normalize content state depth))
                   (error
                    (e-tools--preview-note-truncated state)
                    (prin1-to-string content)))))
         (bytes (string-bytes text)))
    (when (> bytes limit)
      (setq text (e-tools--string-byte-prefix text limit))
      (setq bytes (string-bytes text))
      (e-tools--preview-note-truncated state))
    (list :text text
          :truncated (plist-get state :truncated)
          :shown-bytes bytes)))

(defun e-tools--condition-message (err)
  "Return a concise message for condition ERR."
  (if (and (memq (car err) '(e-tools-recursive-call
                             e-tools-nested-tool-budget-exceeded
                             e-tools-no-active-registry
                             e-tools-blocking-handler-rejected))
           (stringp (cadr err)))
      (cadr err)
    (error-message-string err)))

(defun e-tool-lifecycle-prepare-call (lifecycle tool-call)
  "Return TOOL-CALL after LIFECYCLE preparation."
  (if-let ((prepare (and (e-tool-lifecycle-p lifecycle)
                         (e-tool-lifecycle-prepare lifecycle))))
      (funcall prepare tool-call)
    tool-call))

(defun e-tool-lifecycle-start-call
    (lifecycle tool-call &rest arguments)
  "Start TOOL-CALL through LIFECYCLE with keyword ARGUMENTS."
  (let ((start (and (e-tool-lifecycle-p lifecycle)
                    (e-tool-lifecycle-start lifecycle))))
    (unless (functionp start)
      (signal 'wrong-type-argument (list 'functionp start)))
    (apply start tool-call arguments)))

(defun e-tools--normalize-parameters (parameters)
  "Return tool PARAMETERS with valid JSON object defaults."
  (let ((normalized (copy-sequence
                     (or parameters
                         (list :type "object"
                               :properties (e-tools--empty-json-object))))))
    (when (and (equal (plist-get normalized :type) "object")
               (null (plist-get normalized :properties)))
      (plist-put normalized :properties (e-tools--empty-json-object)))
    normalized))

(defun e-tools-definitions (registry)
  "Return backend-neutral tool definitions for REGISTRY."
  (let ((definitions nil))
    (dolist (name (e-tools-registry-order registry))
      (let ((tool (gethash name (e-tools-registry-tools registry))))
        (push (list :type "function"
                    :name (plist-get tool :name)
                    :description (plist-get tool :description)
                    :parameters (e-tools--normalize-parameters
                                 (plist-get tool :parameters))
                    :strict :json-false)
              definitions)))
    (nreverse definitions)))

(defun e-tools-available ()
  "Return compact active tool descriptors from the current registry."
  (let ((registry (e-tools-current-registry))
        descriptors)
    (dolist (name (e-tools-registry-order registry))
      (let ((tool (gethash name (e-tools-registry-tools registry))))
        (push (list :name (plist-get tool :name)
                    :description (plist-get tool :description)
                    :parameters (or (plist-get tool :parameters)
                                    '(:type "object" :properties nil))
                    :metadata (plist-get tool :metadata))
              descriptors)))
    (nreverse descriptors)))

(defun e-tools-result-create (call status content &optional metadata)
  "Return a structured tool result for CALL with STATUS, CONTENT, and METADATA."
  (list :tool-call-id (plist-get call :id)
        :name (plist-get call :name)
        :status status
        :content content
        :metadata metadata))

(defun e-tools--resource-usage-operation (operation)
  "Return normalized resource usage OPERATION."
  (cond
   ((symbolp operation) operation)
   ((and (stringp operation) (not (string-empty-p operation)))
    (intern operation))
   (t nil)))

(defun e-tools--resource-usage-resource (resource)
  "Return normalized resource usage RESOURCE plist, or nil."
  (let ((uri (plist-get resource :uri))
        (operation (e-tools--resource-usage-operation
                    (plist-get resource :operation))))
    (when (and (stringp uri) operation)
      (list :uri uri :operation operation))))

(defun e-tools-resource-usage-metadata (tool resources &optional summary)
  "Return metadata recording TOOL resource usage over RESOURCES.
SUMMARY is optional and should stay compact and high value."
  (let ((resources (delq nil
                         (mapcar #'e-tools--resource-usage-resource
                                 resources))))
    (when resources
      (let ((record (list :kind 'resource-usage
                          :tool tool
                          :resources resources)))
        (when (and (stringp summary)
                   (not (string-empty-p summary)))
          (plist-put record :summary summary))
        (list :tool-usage (list record))))))

(defun e-tools-resource-usage-metadata-from-arguments (tool arguments)
  "Return resource usage metadata for TOOL from optional ARGUMENTS."
  (let ((usage (or (plist-get arguments :resource_usage)
                   (plist-get arguments :resourceUsage))))
    (when (listp usage)
      (e-tools-resource-usage-metadata
       tool
       (plist-get usage :resources)
       (plist-get usage :summary)))))

(defun e-tools-merge-metadata (&rest metadata-list)
  "Merge METADATA-LIST plists, appending any `:tool-usage' records."
  (let (merged)
    (dolist (metadata metadata-list)
      (when (listp metadata)
        (while metadata
          (let ((key (pop metadata))
                (value (pop metadata)))
            (if (eq key :tool-usage)
                (setq merged
                      (plist-put merged
                                 key
                                 (append (plist-get merged key) value)))
              (setq merged (plist-put merged key value)))))))
    merged))

(defun e-tools-result-p (value)
  "Return non-nil when VALUE is a structured tool result."
  (and (listp value)
       (plist-member value :tool-call-id)
       (plist-member value :name)
       (plist-member value :status)
       (plist-member value :content)))

(defun e-tools--result-for-call-p (value call)
  "Return non-nil when VALUE is a structured result for CALL."
  (and (e-tools-result-p value)
       (equal (plist-get value :tool-call-id)
              (plist-get call :id))
       (equal (plist-get value :name)
              (plist-get call :name))))

(defun e-tools--result (call status content &optional metadata)
  "Return a structured tool result for CALL with STATUS, CONTENT, and METADATA."
  (e-tools-result-create call status content metadata))

(defun e-tools--interactive-context-p (context)
  "Return non-nil when CONTEXT marks an interactive tool execution path."
  (or (plist-get context :interactive)
      (plist-get context :interactive-p)
      (e-request-hot-path-active-p)))

(defun e-tools--reject-long-sync-handler-p (tool context)
  "Return non-nil when TOOL must not use a sync handler under CONTEXT."
  (and (not (functionp (plist-get tool :start)))
       (functionp (plist-get tool :handler))
       (e-tools--interactive-context-p context)
       (e-tools-long-blocking-class-p (e-tools--blocking-class tool))))

(defun e-tools--nested-long-tool-result (call tool)
  "Return a structured rejection result for long nested CALL to TOOL."
  (let ((class (or (e-tools--blocking-class tool) 'unknown))
        (name (plist-get call :name)))
    (e-tools--result
     call
     'error
     (format
      "Nested tool %s is %s-class and cannot run synchronously inside another tool; call it as a top-level tool instead."
      name class)
     (list :error 'e-tools-nested-long-tool-rejected
           :blocking-class class))))

(defun e-tools-execute (registry call)
  "Execute CALL against REGISTRY and return a structured tool result."
  (let ((done nil)
        (result nil)
        (failure nil))
    (e-tools-start
     registry
     call
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

(defun e-tools--execute-with-context (registry call context)
  "Execute CALL against REGISTRY with CONTEXT and return a structured result."
  (let ((done nil)
        (result nil)
        (failure nil))
    (e-tools-start
     registry
     call
     :context context
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

(defun e-tools--reject-recursive-call (name options)
  "Signal when NAME recursively calls the current tool without OPTIONS opt-in."
  (let ((parent-name (plist-get (e-tools-current-tool-call) :name)))
    (when (and (equal parent-name name)
               (not (plist-get options :allow-recursive)))
      (signal 'e-tools-recursive-call
              (list (format "Recursive nested tool call rejected: %s" name))))))

(defun e-tools-call (name arguments &optional options)
  "Execute active tool NAME with ARGUMENTS and return a structured result.
OPTIONS is a plist.  Supported keys are `:call-id', `:allow-recursive', and
`:metadata'."
  (let* ((options (or options nil))
         (registry (e-tools-current-registry))
         (context (e-tools-current-context)))
    (e-tools--reject-recursive-call name options)
    (e-tools--check-nested-budget)
    (let* ((call (list :id (e-tools--next-nested-call-id options)
                       :name name
                       :arguments arguments))
           (metadata (plist-get options :metadata))
           (executor (plist-get context :tool-executor)))
      (when metadata
        (setq call (plist-put call :metadata metadata)))
      (if executor
          (funcall executor call options context)
        (let ((tool (gethash name (e-tools-registry-tools registry))))
          (if (and tool
                   (e-tools-long-blocking-class-p
                    (e-tools--blocking-class tool)))
              (e-tools--nested-long-tool-result call tool)
            (e-tools--execute-with-context
             registry
             call
             (e-tools--nested-context context))))))))

(defun e-tools-call! (name arguments &optional options)
  "Execute active tool NAME with ARGUMENTS and return successful content.
Signal `e-tools-nested-tool-error' when the structured result is an error."
  (let ((result (e-tools-call name arguments options)))
    (if (eq (plist-get result :status) 'ok)
        (plist-get result :content)
      (signal 'e-tools-nested-tool-error (list result)))))

(cl-defun e-tools-start
    (registry call &key on-done on-error on-request-start on-event context)
  "Start CALL against REGISTRY and report a structured result asynchronously.
ON-DONE receives the structured result.  ON-ERROR receives unexpected Emacs
condition lists.  ON-REQUEST-START receives an optional `e-tools-request'.
ON-EVENT receives tool progress events as TYPE and PAYLOAD.  CONTEXT is
dynamically visible to tool start functions through
`e-tools-current-context'."
  (let* ((name (plist-get call :name))
         (nested-state (or (plist-get context :nested-tool-state)
                           (list :count 0 :sequence 0)))
         (tool-context (append (list :tool-call call
                                     :tools registry
                                     :nested-tool-state nested-state)
                               context))
         (tool (gethash name (e-tools-registry-tools registry))))
    (if (not tool)
        (let ((result (e-tools--result
                       call
                       'error
                       (format "Unknown tool: %s" name)
                       '(:error e-tool-missing))))
          (when on-done
            (funcall on-done result))
          nil)
      (let ((start (plist-get tool :start))
            (handler (plist-get tool :handler)))
        (cl-labels
            ((finish-ok
              (content)
              (when on-done
                (funcall on-done
                         (if (e-tools--result-for-call-p content call)
                             (let ((argument-metadata
                                    (e-tools-resource-usage-metadata-from-arguments
                                     name (plist-get call :arguments))))
                               (if argument-metadata
                                   (plist-put
                                    content :metadata
                                    (e-tools-merge-metadata
                                     (plist-get content :metadata)
                                     argument-metadata))
                                 content))
                           (e-tools--result
                            call 'ok content
                            (e-tools-resource-usage-metadata-from-arguments
                             name (plist-get call :arguments)))))))
             (finish-error
              (err)
              (if (and (get (car err) 'e-tools-infrastructure-error)
                       on-error)
                  (when on-error
                    (funcall on-error err))
                (when on-done
                  (funcall on-done
                           (e-tools--result
                            call
                            'error
                            (e-tools--condition-message err)
                            (list :error (car err)))))))
             (publish-request
              (request)
              (when (and request on-request-start)
                (funcall on-request-start request))))
          (condition-case err
              (e-request-profile-span
               'tool.start
               (list :tool name
                     :blocking-class (or (e-tools--blocking-class tool)
                                         'cheap))
               (lambda ()
                 (let ((e-tools--current-context tool-context))
                   (when (e-tools--reject-long-sync-handler-p tool tool-context)
                     (signal 'e-tools-blocking-handler-rejected
                             (list (format "Tool %s is %s-class and must provide :start in interactive execution"
                                           name (e-tools--blocking-class tool)))))
                   (if (functionp start)
                       (let ((reported-request nil))
                         (let* ((start-arguments
                                 (list
                                  :arguments (plist-get call :arguments)
                                  :on-done #'finish-ok
                                  :on-error #'finish-error
                                  :on-request-start
                                  (lambda (request)
                                    (setq reported-request request)
                                    (publish-request request))))
                                (request
                                 (e-tools--apply-start-with-optional-event
                                  start start-arguments on-event)))
                           (when (and request (not (eq request reported-request)))
                             (publish-request request))
                           request))
                     (let ((cancelled nil)
                           (timer nil)
                           request)
                       (setq request
                             (e-tools-request-create
                              :cancel (lambda ()
                                        (setq cancelled t)
                                        (when (timerp timer)
                                          (cancel-timer timer))
                                        t)
                              :metadata '(:transport timer
                                          :cancellable queued-only)))
                       (publish-request request)
                       (setq timer
                             (run-at-time
                              0 nil
                              (lambda ()
                                (let ((e-tools--current-context tool-context))
                                  (unless cancelled
                                    (condition-case err
                                        (finish-ok
                                         (funcall handler
                                                  (plist-get call :arguments)))
                                      (quit
                                       (finish-error err))
                                      (error
                                       (finish-error err))))))))
                       request)))))
            (quit
             (finish-error err)
             nil)
            (error
             (finish-error err)
             nil)))))))

(provide 'e-tools)

;;; e-tools.el ends here

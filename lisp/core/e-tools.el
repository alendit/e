;;; e-tools.el --- Tool registry for e core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure tool registry and dispatch for core tool-call handling.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'e-json)
(require 'e-request)
(require 'e-work)

(cl-defstruct (e-tools-registry (:constructor e-tools-registry-create))
  (tools (make-hash-table :test 'equal))
  (order nil))

(cl-defstruct (e-tools-request (:constructor e-tools-request-create))
  cancel
  metadata)

(cl-defstruct (e-tool-lifecycle (:constructor e-tool-lifecycle-create))
  prepare
  start)

(cl-defstruct (e-tools-file-content
               (:constructor e-tools-file-content-create))
  "One file-backed semantic text value owned by the tool lifecycle.
PATH is an internal readable file.  URI, when non-nil, is the resource that
already owns that file.  PREVIEW and the byte/line counts are bounded
presentation facts collected while the producer streamed the complete value.
When OWNED is non-nil, the first consumer that durably adopts the value may
delete PATH after its replacement write succeeds."
  path
  uri
  preview
  original-bytes
  original-lines
  preview-bytes
  preview-lines
  owned)

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
(define-error 'e-tools-blocking-execute-rejected
  "Long synchronous tool batch execution rejected in interactive execution")
(define-error 'e-tools-batch-execute-not-allowed
  "Synchronous tool execution requires an explicit batch/test scope")
(define-error 'e-tools-nested-async-tool-rejected
  "Nested async tool call rejected")
(define-error 'e-tools-invalid-arguments
  "Tool arguments do not match the declared schema")
(define-error 'e-tools-invalid-definition
  "Tool definition is not compatible with the declared tool schema contract")
(define-error 'e-tools-invalid-result-content
  "Tool result content is not a canonical JSON value or text carrier")

(defconst e-tools-nested-tool-default-budget 20
  "Default maximum number of nested tool calls per parent tool execution.")

(defun e-tools-current-context ()
  "Return the current tool start context, or nil."
  e-tools--current-context)

(defun e-tools-current-context-summary ()
  "Return a scalar inspection summary of the current tool context.
This is the model-facing inspection surface for the dynamic tool context.
It deliberately omits the harness, registry, capabilities, closures, and
other live runtime objects held by `e-tools-current-context'."
  (let* ((context (e-tools-current-context))
         (call (plist-get context :tool-call))
         (registry (plist-get context :tools)))
    (list :session-id (or (plist-get context :session-id) e-json-null)
          :turn-id (or (plist-get context :turn-id) e-json-null)
          :tool-call-id (or (plist-get call :id) e-json-null)
          :tool-call-name (or (plist-get call :name) e-json-null)
          :deadline (or (plist-get context :deadline) e-json-null)
          :nested (if (plist-get context :nested) t e-json-false)
          :parent-tool-call-id (or (plist-get context :parent-tool-call-id)
                                   e-json-null)
          :tool-names (if (e-tools-registry-p registry)
                          (vconcat (e-tools-registry-order registry))
                        []))))

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
  (when-let* ((cancel (and (e-tools-request-p request)
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

(defun e-tools--work-request (handle)
  "Return the canonical tool request projection for work HANDLE."
  (e-tools-request-create
   :cancel (lambda ()
             (e-work-cancel handle)
             t)
   :metadata (append (list :transport 'work
                           :work-id (e-work-handle-id handle)
                           :work-handle handle)
                     (e-work-handle-metadata handle))))

(cl-defun e-tools-cheap-work (id runner &key description (owner 'tools))
  "Return canonical cheap work ID that invokes RUNNER with tool arguments."
  (e-work-spec-create
   :id id
   :description (or description (format "Run cheap tool %s." id))
   :execution 'cheap
   :interactive-policy 'cheap
   :owner owner
   :runner (lambda (arguments _context) (funcall runner arguments))))

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
    (registry &key name description parameters work metadata blocking-class)
  "Register tool NAME in REGISTRY.
DESCRIPTION, PARAMETERS, WORK, and METADATA describe the tool.
BLOCKING-CLASS may be `cheap', `network', `process', `helper', `filesystem',
`render', or `unknown'.
WORK is the canonical `e-work-spec' lifecycle for the tool."
  (unless (e-work-spec-p work)
    (signal 'wrong-type-argument (list 'e-work-spec-p work)))
  (setq parameters (e-tools--canonical-parameters parameters))
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
                 :work work)
           (e-tools-registry-tools registry)))

(defun e-tools--plist-p (value)
  "Return non-nil when VALUE is a canonical JSON object plist."
  (and (listp value) (e-json-value-p value)))

(defun e-tools--copy-canonical-value (value)
  "Return a detached copy of canonical JSON VALUE."
  (e-json-assert-value value)
  (cond
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (vconcat (mapcar #'e-tools--copy-canonical-value (append value nil))))
   ((consp value)
    (let ((copy nil)
          (rest value))
      (while rest
        (setq copy (append copy
                           (list (pop rest)
                                 (e-tools--copy-canonical-value (pop rest))))))
      copy))
   (t value)))

(defun e-tools--canonical-parameters (parameters)
  "Return a detached canonical schema for PARAMETERS."
  (let ((schema (or parameters '(:type "object" :properties nil))))
    (condition-case error-data
        (progn
          (e-json-schema-assert-schema schema)
          (e-tools--copy-canonical-value schema))
      ((e-json-error e-json-schema-error)
       (signal 'e-tools-invalid-definition
               (list (error-message-string error-data)))))))

(defun e-tools--prepare-call-arguments (call tool)
  "Validate CALL's canonical arguments against TOOL's runtime schema."
  (let ((parameters (plist-get tool :parameters))
        (arguments (plist-get call :arguments)))
    (condition-case error-data
        (e-json-schema-assert arguments parameters)
      ((e-json-error e-json-schema-error)
       (signal 'e-tools-invalid-arguments
               (list (error-message-string error-data)))))
    call))

(defun e-tools--prepare-call-arguments-with-context (call tool)
  "Validate CALL and retain its received arguments on schema failure."
  (condition-case err
      (e-tools--prepare-call-arguments call tool)
    (e-tools-invalid-arguments
     (signal (car err)
             (append (cdr err) (list :prepared-call call))))))

(defun e-tools-prepare-call (registry call)
  "Return a validated copy of CALL from REGISTRY.
Unknown tools are returned unchanged so normal missing-tool handling remains
inside `e-tools-start'."
  (let* ((copy (copy-tree call))
         (tool (gethash (plist-get copy :name)
                        (e-tools-registry-tools registry))))
    (if tool
        (e-tools--prepare-call-arguments-with-context copy tool)
      copy)))

(defun e-tools-project-call-for-rejection (registry call)
  "Return a bounded schema-valid projection of rejected CALL arguments.
Only declared values that independently satisfy their property schema survive.
The original call still goes through `e-tools-start' so the model receives a
normal tool error, while rejected text cannot enter transcript or activity."
  (let* ((copy (copy-tree call))
         (tool (gethash (plist-get copy :name)
                        (e-tools-registry-tools registry)))
         (parameters (and tool (plist-get tool :parameters)))
         (properties (and tool (plist-get parameters :properties)))
         (arguments (plist-get copy :arguments))
         projected)
    (when (and tool (e-tools--plist-p arguments))
      (cl-loop for (key value) on arguments by #'cddr do
               (when (and (plist-member properties key)
                          (condition-case nil
                              (e-json-schema-value-p
                               value (plist-get properties key))
                            (e-json-error nil)
                            (e-json-schema-error nil)))
                 (setq projected (append projected (list key value))))))
    (plist-put copy :arguments projected)))

(defun e-tools--json-key (key)
  "Return stable JSON object key text for canonical keyword KEY."
  (unless (keywordp key)
    (signal 'e-json-error
            (list (format "JSON object keys must be keywords: %S" key))))
  (substring (symbol-name key) 1))

(defun e-tools--sort-json-object (entries)
  "Return ENTRIES sorted by their string keys."
  (sort entries (lambda (left right)
                  (string< (car left) (car right)))))

(defun e-tools--canonical-sort-value (value)
  "Return canonical VALUE with object keys sorted for deterministic output."
  (e-json-assert-value value)
  (cond
   ((vectorp value)
    (vconcat (mapcar #'e-tools--canonical-sort-value (append value nil))))
   ((consp value)
    (let (entries)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (push (cons (e-tools--json-key key)
                      (cons key (e-tools--canonical-sort-value item)))
                entries)))
      (setq entries
            (e-tools--sort-json-object entries))
      (let (result)
        (dolist (entry entries)
          (setq result
                (append result
                        (list (car (cdr entry))
                              (cdr (cdr entry))))))
        result)))
   (t value)))

(defun e-tools-definition-fingerprint (definition)
  "Return a stable material fingerprint for canonical provider DEFINITION."
  (secure-hash 'sha256
               (e-json-serialize
                (e-tools--canonical-sort-value definition))))

(defun e-tools-arguments-fingerprint (arguments)
  "Return a stable SHA-256 fingerprint for canonical tool ARGUMENTS.
Object key order is normalized only after the value passes the canonical
representation assertion."
  (secure-hash 'sha256
               (e-json-serialize
                (e-tools--canonical-sort-value arguments))))

(defun e-tools-result-content-text (content)
  "Return the model-visible text representation for tool result CONTENT."
  (cond
   ((stringp content) content)
   ((e-tools-file-content-p content)
    (or (e-tools-file-content-preview content) ""))
   (t
    (condition-case error-data
        (e-json-serialize content)
      (e-json-error
       (signal 'e-tools-invalid-result-content
               (list (error-message-string error-data))))))))

(defun e-tools-file-content-valid-p (content)
  "Return non-nil when CONTENT is a valid file-backed text carrier."
  (and (e-tools-file-content-p content)
       (stringp (e-tools-file-content-path content))
       (or (null (e-tools-file-content-uri content))
           (stringp (e-tools-file-content-uri content)))
       (stringp (e-tools-file-content-preview content))
       (cl-every (lambda (value) (and (integerp value) (>= value 0)))
                 (list (e-tools-file-content-original-bytes content)
                       (e-tools-file-content-original-lines content)
                       (e-tools-file-content-preview-bytes content)
                       (e-tools-file-content-preview-lines content)))))

(defun e-tools-file-content-dispose (content)
  "Delete owned CONTENT's backing file and mark it consumed.
Return the deleted path, or nil when CONTENT does not own a live file."
  (when (and (e-tools-file-content-valid-p content)
             (e-tools-file-content-owned content)
             (file-exists-p (e-tools-file-content-path content)))
    (let ((path (e-tools-file-content-path content)))
      (delete-file path)
      (setf (e-tools-file-content-owned content) nil)
      path)))

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

(defun e-tools--preview-key-text (key)
  "Return display text for arbitrary preview KEY."
  (cond
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)
   (t (format "%s" key))))

(defun e-tools--preview-project-value (value state depth)
  "Project arbitrary VALUE into bounded display text data.
STATE carries shared truncation metadata.  DEPTH is the remaining traversal
budget.  This is a telemetry/display projector, not a JSON value normalizer."
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
                           (e-tools--preview-project-value
                            item state (1- depth)))
                         limited))))
     ((hash-table-p value)
      (let (entries)
        (maphash
         (lambda (key item)
           (push (cons (e-tools--preview-key-text key) item) entries))
         value)
        (setq entries (e-tools--sort-json-object entries))
        (when (> (length entries) max-items)
          (e-tools--preview-note-truncated state))
        (mapcar (lambda (entry)
                  (cons (car entry)
                        (e-tools--preview-project-value
                         (cdr entry) state (1- depth))))
                (seq-take entries max-items))))
     ((e-tools--plist-p value)
      (let (entries)
        (while value
          (push (cons (e-tools--preview-key-text (pop value)) (pop value))
                entries))
        (setq entries (e-tools--sort-json-object entries))
        (when (> (length entries) max-items)
          (e-tools--preview-note-truncated state))
        (mapcar (lambda (entry)
                  (cons (car entry)
                        (e-tools--preview-project-value
                         (cdr entry) state (1- depth))))
                (seq-take entries max-items))))
     ((listp value)
      (when (> (length value) max-items)
        (e-tools--preview-note-truncated state))
      (vconcat (mapcar (lambda (item)
                         (e-tools--preview-project-value
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
                      (e-tools--preview-project-value content state depth))
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
                             e-tools-blocking-handler-rejected
                             e-tools-blocking-execute-rejected
                             e-tools-batch-execute-not-allowed
                             e-tools-nested-async-tool-rejected
                             e-tools-invalid-arguments
                             e-tools-invalid-result-content))
           (stringp (cadr err)))
      (cadr err)
    (e-work-error-message err)))

(defun e-tool-lifecycle-prepare-call (lifecycle tool-call)
  "Return TOOL-CALL after LIFECYCLE preparation."
  (if-let* ((prepare (and (e-tool-lifecycle-p lifecycle)
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

(defun e-tools--decorated-parameters (parameters)
  (e-tools--copy-canonical-value parameters))

(defun e-tools-definitions (registry)
  "Return backend-neutral tool definitions for REGISTRY."
  (let ((definitions nil))
    (dolist (name (e-tools-registry-order registry))
      (let ((tool (gethash name (e-tools-registry-tools registry))))
        (push (list :type "function"
                    :name (plist-get tool :name)
                    :description (plist-get tool :description)
                    :parameters (e-tools--decorated-parameters
                                 (plist-get tool :parameters))
                    :strict :json-false)
              definitions)))
    (nreverse definitions)))

(defun e-tools-available ()
  "Return a canonical vector of active tool descriptors."
  (let ((registry (e-tools-current-registry))
        descriptors)
    (dolist (name (e-tools-registry-order registry))
      (let ((tool (gethash name (e-tools-registry-tools registry))))
        (push (list :name (plist-get tool :name)
                    :description (plist-get tool :description)
                    :parameters
                    (e-tools--copy-canonical-value
                     (plist-get tool :parameters)))
              descriptors)))
    (vconcat (nreverse descriptors))))

(defun e-tools-result-create (call status content &optional metadata)
  "Return a structured tool result for CALL with STATUS, CONTENT, and METADATA."
  (unless (or (stringp content)
              (e-tools-file-content-valid-p content)
              (condition-case nil
                  (progn (e-json-assert-value content) t)
                (e-json-error nil)))
    (signal 'e-tools-invalid-result-content
            (list "Tool result content must be a canonical JSON value or text carrier")))
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
    (when (and (listp usage) (e-json-value-p usage))
      (e-tools-resource-usage-metadata
       tool
       ;; Resource metadata is a domain-owned telemetry record.  Convert its
       ;; canonical JSON array explicitly at this boundary for the existing
       ;; metadata projector, rather than treating arbitrary lists as JSON.
       (let ((resources (plist-get usage :resources)))
         (cond
          ((vectorp resources) (append resources nil))
          ((null resources) nil)
          (t (signal 'e-tools-invalid-arguments
                     (list "resource_usage.resources must be a JSON array")))))
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

(defun e-tools-result-for-call-p (value call)
  "Return non-nil when structured VALUE is the result for CALL.
This semantic predicate is the stable boundary for lifecycle/activity owners."
  (e-tools--result-for-call-p value call))

(defun e-tools--result (call status content &optional metadata)
  "Return a structured tool result for CALL with STATUS, CONTENT, and METADATA."
  (e-tools-result-create call status content metadata))

(defun e-tools--ok-result-from-content (call name content)
  "Return a structured ok result for CALL/NAME using CONTENT.
When CONTENT is already a result for CALL, preserve it and merge argument-level
resource metadata."
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
      name (plist-get call :arguments)))))

(defun e-tools--error-result-from-condition (call err)
  "Return a structured error result for CALL from condition ERR."
  (e-tools--result
   call
   'error
   (e-tools--condition-message err)
   (list :error (car err))))

(defun e-tools--interactive-context-p (context)
  "Return non-nil when CONTEXT marks an interactive tool execution path."
  (or (plist-get context :interactive)
      (plist-get context :interactive-p)
      (e-request-hot-path-active-p)))

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

(defun e-tools--nested-async-tool-result (call tool)
  "Return a structured rejection result for nested CALL to async TOOL."
  (let ((class (or (e-tools--blocking-class tool) 'unknown))
        (name (plist-get call :name)))
    (e-tools--result
     call
     'error
     (format
      "Nested tool %s is async-backed and cannot run through the synchronous nested path; provide a tool executor or call it as a top-level tool."
      name)
     (list :error 'e-tools-nested-async-tool-rejected
           :blocking-class class))))

(defun e-tools--reject-blocking-execute-p (tool)
  "Return non-nil when TOOL must not use sync batch execution here."
  (and (e-work-spec-p (plist-get tool :work))
       (e-request-hot-path-active-p)
       (e-tools-long-blocking-class-p (e-tools--blocking-class tool))))

(defun e-tools--blocking-execute-result (call tool)
  "Return a structured rejection result for sync execution of long TOOL."
  (let ((class (or (e-tools--blocking-class tool) 'unknown))
        (name (plist-get call :name)))
    (e-tools-result-create
     call
     'error
     (format
      "Tool %s is %s-class and cannot be synchronously executed in an interactive hot path; use e-tools-start instead."
      name class)
     (list :error 'e-tools-blocking-execute-rejected
           :blocking-class class))))

(defun e-tools--execute-batch-with-context (registry call context)
  "Execute CALL against REGISTRY with CONTEXT in an explicit batch/test scope."
  (when (e-request-hot-path-active-p)
    (signal 'e-tools-batch-execute-not-allowed
            (list "Batch tool execution is not allowed in interactive hot paths")))
  (let* ((name (plist-get call :name))
         (tool (and name
                    (gethash name (e-tools-registry-tools registry)))))
    (if (and tool (e-tools--reject-blocking-execute-p tool))
        (e-tools--blocking-execute-result call tool)
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
        result))))

(defun e-tools-execute-batch (registry call)
  "Execute CALL against REGISTRY from explicit batch/test code."
  (e-tools--execute-batch-with-context registry call nil))

(defun e-tools--execute-nested-cheap-with-context (registry call context)
  "Execute cheap nested CALL against REGISTRY with CONTEXT without batch waits."
  (let* ((name (plist-get call :name))
         (tool (and name
                    (gethash name (e-tools-registry-tools registry)))))
    (cond
     ((not tool)
      (e-tools--result
       call
       'error
       (format "Unknown tool: %s" name)
       '(:error e-tool-missing)))
     ((e-tools-long-blocking-class-p (e-tools--blocking-class tool))
      (e-tools--nested-long-tool-result call tool))
     (t
      (setq call (e-tools--prepare-call-arguments call tool))
      (let* ((nested-state (or (plist-get context :nested-tool-state)
                               (list :count 0 :sequence 0)))
             (tool-context (append (list :tool-call call
                                         :tools registry
                                         :nested-tool-state nested-state)
                                   context))
             (work (plist-get tool :work)))
        (condition-case err
            (e-request-profile-span
             'tool.nested-cheap
             (list :tool name
                   :blocking-class (or (e-tools--blocking-class tool) 'cheap))
             (lambda ()
               (let ((e-tools--current-context tool-context))
                 (cond
                  ((and (e-work-spec-p work)
                        (eq (e-work-spec-execution work) 'cheap))
                   (let ((done nil)
                         result
                         failure)
                     (e-work-start
                      work
                      (plist-get call :arguments)
                      :context tool-context
                      :on-done (lambda (value)
                                 (setq result value)
                                 (setq done t))
                      :on-error (lambda (err)
                                  (setq failure err)
                                  (setq done t)))
                     (cond
                      (failure (e-tools--error-result-from-condition call failure))
                      (done (e-tools--ok-result-from-content call name result))
                      (t (e-tools--nested-async-tool-result call tool)))))
                  ((e-work-spec-p work)
                   (e-tools--nested-async-tool-result call tool))
                  (t
                   (e-tools--nested-async-tool-result call tool))))))
          (quit (e-tools--error-result-from-condition call err))
          (error (e-tools--error-result-from-condition call err))))))))

(defun e-tools-execute-nested-cheap-with-context (registry call context)
  "Execute cheap nested CALL against REGISTRY with semantic CONTEXT."
  (e-tools--execute-nested-cheap-with-context registry call context))

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
            (e-tools--execute-nested-cheap-with-context
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
    (registry call &key on-done on-error on-request-start on-event context
              on-work-prepared)
  "Start CALL against REGISTRY and report a structured result asynchronously.
ON-DONE receives the structured result.  ON-ERROR receives unexpected Emacs
condition lists.  ON-REQUEST-START receives an optional `e-tools-request'.
ON-EVENT receives tool progress events as TYPE and PAYLOAD.  CONTEXT is
dynamically visible to tool start functions through
`e-tools-current-context'.  ON-WORK-PREPARED receives each canonical work
handle after allocation and before its runner may execute."
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
      (if (eq (plist-get (plist-get call :metadata) :argument-status)
                'invalid)
            (let ((result
                   (e-tools--result
                    call
                    'error
                    "Tool arguments are invalid"
                    '(:error e-tools-invalid-arguments))))
              (when on-done
                (funcall on-done result))
              nil)
          (let ((work (plist-get tool :work))
              settled
              active-request
              deadline-timer)
          (cl-labels
            ((cancel-deadline
              ()
              (when (timerp deadline-timer)
                (cancel-timer deadline-timer))
              (setq deadline-timer nil))
             (effective-deadline
              ()
              (let ((deadline (plist-get tool-context :deadline)))
                (unless (or (null deadline)
                            (and (numberp deadline) (not (< deadline 0))))
                  (signal 'e-work-invalid-spec
                          (list "Tool deadline must be an absolute float-time timestamp"
                                deadline)))
                deadline))
             (deadline-condition
              (deadline &optional cancel-error)
              (let ((details (list :deadline deadline
                                   :now (float-time)
                                   :tool name
                                   :tool-call-id (plist-get call :id))))
                (when cancel-error
                  (plist-put details :cancel-error cancel-error))
                (list 'e-work-deadline-exceeded
                      (format "Tool %s exceeded its deadline" name)
                      details)))
             (finish-ok
              (content)
              (unless settled
                (cancel-deadline)
                (condition-case err
                    (let ((result (e-tools--ok-result-from-content
                                   call name content)))
                      (setq settled t)
                      (when on-done
                        (funcall on-done result)))
                  (error
                   ;; A handler result is part of the strict model-facing
                   ;; boundary.  If an owner returns a noncanonical value,
                   ;; settle the call as a normal tool error rather than
                   ;; leaving batch callers waiting forever after the
                   ;; completion callback has unwound.
                   (setq settled t)
                   (when on-done
                     (funcall on-done
                              (e-tools--result
                               call
                               'error
                               (e-tools--condition-message err)
                               (list :error (car err)))))))))
             (finish-error
              (err)
              (unless settled
                (setq settled t)
                (cancel-deadline)
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
                              (list :error (car err))))))))
              (publish-request
               (request)
              (when (and request (not settled))
                (setq active-request request))
               (when (and request (not settled) on-request-start)
                 (funcall on-request-start request)))
              (prepare-work
               (handle)
               "Expose HANDLE, then enroll its exact board invocation when requested."
               (when on-work-prepared
                 (funcall on-work-prepared handle))
               (when-let* ((enroll (plist-get tool-context :board-enroll-work)))
                 (condition-case err
                     (funcall
                      enroll handle
                      (lambda (state payload)
                        (pcase state
                          ('finished (finish-ok payload))
                          ('failed (finish-error payload))
                          ('cancelled
                           (finish-error
                            (list 'e-work-cancelled
                                  (format "Work %s was cancelled"
                                          (e-work-handle-id handle))))))))
                     (error
                      ;; Enrollment happens before runner entry.  Failing this
                      ;; local setup must never invoke the external carrier.
                      (e-work-fail handle err)
                      (signal (car err) (cdr err))))))
              (arm-deadline
              ()
              (when-let* ((deadline (effective-deadline)))
                (unless (or settled (timerp deadline-timer))
                  (setq deadline-timer
                        (run-at-time
                         (max 0 (- deadline (float-time))) nil
                         (lambda ()
                           (unless settled
                             (let (cancel-error)
                               (when active-request
                                 (condition-case err
                                     (e-tools-cancel-request active-request)
                                   (error
                                    (setq cancel-error err))))
                               (finish-error
                                (deadline-condition
                                 deadline cancel-error)))))))))))
          (condition-case err
              (e-request-profile-span
               'tool.start
               (list :tool name
                     :blocking-class (or (e-tools--blocking-class tool)
                                         'cheap))
               (lambda ()
                 (let ((e-tools--current-context tool-context))
                   ;; Assert the provider's canonical arguments against the
                   ;; declared schema before dispatch.  Validation is
                   ;; shape-preserving: malformed arguments become a bounded
                   ;; tool error and never enter the handler.
                   (setq call (e-tools--prepare-call-arguments call tool))
                   (arm-deadline)
                   (cond
                     ((e-work-spec-p work)
                      (let* ((handle
                              (e-work-prepare
                               work
                               (plist-get call :arguments)
                               :context tool-context
                               :on-done (unless (plist-get tool-context :board-enroll-work)
                                          #'finish-ok)
                               :on-error (unless (plist-get tool-context :board-enroll-work)
                                           #'finish-error)
                               :on-progress
                               (lambda (payload)
                                 (when on-event
                                   (funcall on-event 'tool-progress payload)))))
                             request)
                        (prepare-work handle)
                        (e-work-start-prepared
                         handle
                         :arguments (plist-get call :arguments)
                         :context tool-context)
                        ;; Carrier setup can add metadata (for example an
                        ;; output file) before its request becomes visible.
                        (setq request (e-tools--work-request handle))
                        (publish-request request)
                        request))
                     (t
                      (signal 'e-work-invalid-spec
                              (list (format "Tool %s has no canonical work spec"
                                            name))))))))
            (quit
             (finish-error err)
             nil)
            (error
             (finish-error err)
              nil))))))))

(provide 'e-tools)

;;; e-tools.el ends here

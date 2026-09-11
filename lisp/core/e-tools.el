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
    (list :session-id (plist-get context :session-id)
          :turn-id (plist-get context :turn-id)
          :tool-call-id (plist-get call :id)
          :tool-call-name (plist-get call :name)
          :deadline (plist-get context :deadline)
          :nested (and (plist-get context :nested) t)
          :parent-tool-call-id (plist-get context :parent-tool-call-id)
          :tool-names (and (e-tools-registry-p registry)
                           (copy-sequence (e-tools-registry-order registry))))))

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

(cl-defun e-tools-callback-work (id start &key description (owner 'tools))
  "Return canonical callback-backed cooperative work ID using START.
START receives ordinary tool callback keyword arguments.  Registrations use
this constructor explicitly instead of installing another dispatch protocol
beside `e-work'."
  (e-work-spec-create
   :id id
   :description (or description (format "Run callback-backed tool %s." id))
   :execution 'cooperative
   :interactive-policy 'async
   :owner owner
   :runner
   (lambda (handle arguments _context)
     (cl-labels
         ((adopt-request
           (request)
           (when request
             (setf (e-work-handle-metadata handle)
                   (append (e-work-handle-metadata handle)
                           (list :request request)))
             (when (e-tools-request-p request)
               (setf (e-tools-request-metadata request)
                     (append (e-tools-request-metadata request)
                             (list :work-id (e-work-handle-id handle)
                                   :work-handle handle))))
             (setf (e-work-handle-cancel-function handle)
                   (lambda (_handle)
                     (e-tools-cancel-request request)
                     t)))))
       (let ((request
              (e-tools--apply-start-with-optional-event
               start
               (list :arguments arguments
                     :on-done (lambda (value) (e-work-finish handle value))
                     :on-error (lambda (err) (e-work-fail handle err))
                     :on-request-start #'adopt-request)
               (lambda (_type payload) (e-work-progress handle payload)))))
         (adopt-request request)
         (when-let ((active-request request))
          (when (e-tools-request-p active-request)
            (setf (e-tools-request-metadata active-request)
                  (append (e-tools-request-metadata active-request)
                          (list :work-id (e-work-handle-id handle)
                                :work-handle handle))))
          (setf (e-work-handle-cancel-function handle)
               (lambda (_handle)
                 (e-tools-cancel-request active-request)
                 t))
           (setf (e-work-handle-metadata handle)
                 (append (e-work-handle-metadata handle)
                         (list :underlying-request-metadata
                               (and (e-tools-request-p active-request)
                                    (e-tools-request-metadata active-request))))))
         :deferred)))))

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

(defun e-tools--empty-json-object ()
  "Return an empty object suitable for `json-encode'."
  (make-hash-table :test 'equal))

(defun e-tools--plist-p (value)
  "Return non-nil when VALUE is a keyword plist."
  (and (listp value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun e-tools--reparse-json-string (value)
  "Return VALUE parsed from a JSON string, or VALUE unchanged on parse failure.
Uses the adapter decode settings: objects become plists, arrays become lists."
  (condition-case nil
      (json-parse-string value
                         :object-type 'plist
                         :array-type 'list
                         :null-object nil
                         :false-object :json-false)
    (error value)))

(defun e-tools--coerce-argument (value schema)
  "Return VALUE coerced to SCHEMA's declared JSON type.
When SCHEMA declares an object or array but VALUE arrived as a JSON string,
parse it back into data.  Providers that JSON-stringify nested tool arguments
\(notably Bedrock) deliver object- and array-typed arguments this way.  Scalar
schemas and non-string values pass through unchanged, so a field the schema
declares a string is never reparsed even when its text is valid JSON."
  (let ((type (and (listp schema) (plist-get schema :type))))
    (cond
     ((and (stringp value) (member type '("object" "array")))
      ;; Reparse once, then re-run: a stringified object may still hold
      ;; inner values the same provider stringified independently.  When the
      ;; string is not valid JSON, `e-tools--reparse-json-string' returns it
      ;; unchanged; re-coercing the identical string would recurse forever
      ;; (a truncated/malformed argument once blew the Lisp eval depth and
      ;; aborted the turn).  Stop when reparsing made no progress.
      (let ((reparsed (e-tools--reparse-json-string value)))
        (if (equal reparsed value)
            value
          (e-tools--coerce-argument reparsed schema))))
     ((and (equal type "object") (e-tools--plist-p value))
      (e-tools--coerce-arguments value schema))
     (t value))))

(defun e-tools--coerce-arguments (arguments parameters)
  "Return ARGUMENTS with each value coerced to PARAMETERS' declared types.
PARAMETERS is the tool's JSON Schema.  Non-plist ARGUMENTS pass through
unchanged.  See `e-tools--coerce-argument'."
  (if (not (e-tools--plist-p arguments))
      arguments
    (let ((properties (and (listp parameters) (plist-get parameters :properties)))
          (result nil))
      (cl-loop for (key value) on arguments by #'cddr do
               (push key result)
               (push (e-tools--coerce-argument
                      value (and properties (plist-get properties key)))
                     result))
      (nreverse result))))

(defun e-tools--schema-property-key (name)
  "Return the plist key represented by JSON Schema property NAME."
  (cond
   ((keywordp name) name)
   ((symbolp name) (intern (concat ":" (symbol-name name))))
   ((stringp name) (intern (concat ":" name)))
   (t nil)))

(defun e-tools--schema-list (value)
  "Return JSON Schema array VALUE as a Lisp list."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun e-tools--schema-property-name (name)
  "Return the JSON name represented by schema property NAME."
  (cond
   ((keywordp name) (substring (symbol-name name) 1))
   ((symbolp name) (symbol-name name))
   ((stringp name) name)
   (t nil)))

(defun e-tools--schema-property-entry (properties name)
  "Return a present/value pair for NAME in schema PROPERTIES, or nil.
PROPERTIES may be the plist, hash-table, or alist form accepted by the
provider-neutral schema boundary.  A present/value pair is used so a
property whose schema is nil is still distinguishable from an absent one."
  (let ((target (e-tools--schema-property-name name)))
    (cond
     ((hash-table-p properties)
      (catch 'found
        (maphash
         (lambda (key value)
           (when (equal (e-tools--schema-property-name key) target)
             (throw 'found (cons t value))))
         properties)
        nil))
     ((e-tools--plist-p properties)
      (catch 'found
        (let ((rest properties))
          (while rest
            (let ((key (pop rest))
                  (value (pop rest)))
              (when (equal (e-tools--schema-property-name key) target)
                (throw 'found (cons t value))))))
        nil))
     ((listp properties)
      (catch 'found
        (dolist (entry properties)
          (when (and (consp entry)
                     (equal (e-tools--schema-property-name (car entry))
                            target))
            (throw 'found (cons t (cdr entry)))))
        nil)))))

(defun e-tools--schema-property-present-p (properties name)
  "Return non-nil when schema PROPERTIES declares JSON property NAME."
  (and (e-tools--schema-property-entry properties name) t))

(defun e-tools--copy-schema-value (value)
  "Return a detached copy of schema VALUE, including hash-table children."
  (cond
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (vconcat (mapcar #'e-tools--copy-schema-value (append value nil))))
   ((hash-table-p value)
    (let ((copy (copy-hash-table value)))
      (clrhash copy)
      (maphash (lambda (key item)
                 (puthash key (e-tools--copy-schema-value item) copy))
               value)
      copy))
   ((consp value)
    (cons (e-tools--copy-schema-value (car value))
          (e-tools--copy-schema-value (cdr value))))
   (t value)))

(defun e-tools--schema-type-p (value type)
  "Return non-nil when VALUE conforms to JSON Schema TYPE."
  (pcase type
    ("string" (stringp value))
    ("number" (numberp value))
    ("integer" (integerp value))
    ("boolean" (memq value '(t :json-false)))
    ("object" (e-tools--plist-p value))
    ("array" (or (listp value) (vectorp value)))
    ("null" (null value))
    (_ t)))

(defun e-tools--validate-schema-value (value schema field)
  "Signal unless VALUE satisfies the supported SCHEMA keywords for FIELD."
  (let ((type (plist-get schema :type))
        (enum (and (plist-member schema :enum)
                   (e-tools--schema-list (plist-get schema :enum)))))
    (unless (e-tools--schema-type-p value type)
      (signal 'e-tools-invalid-arguments
              (list (format "Tool argument %s has the wrong type" field))))
    (when (and enum (not (member value enum)))
      (signal 'e-tools-invalid-arguments
              (list (format "Tool argument %s is not an allowed value" field))))
    (when (stringp value)
      (when (and (numberp (plist-get schema :minLength))
                 (< (length value) (plist-get schema :minLength)))
        (signal 'e-tools-invalid-arguments
                (list (format "Tool argument %s is too short" field))))
      (when (and (numberp (plist-get schema :maxLength))
                 (> (length value) (plist-get schema :maxLength)))
        (signal 'e-tools-invalid-arguments
                (list (format "Tool argument %s is too long" field))))
      (when (and (plist-get schema :nonBlank)
                 (string-empty-p (string-trim value)))
        (signal 'e-tools-invalid-arguments
                (list (format "Tool argument %s must not be blank" field))))
      (when (and (plist-get schema :singleLine)
                 (string-match-p "[\n\r]" value))
        (signal 'e-tools-invalid-arguments
                (list (format "Tool argument %s must be one line" field))))
      (when (and (stringp (plist-get schema :pattern))
                 (not (string-match-p (plist-get schema :pattern) value)))
        (signal 'e-tools-invalid-arguments
                (list (format "Tool argument %s has an invalid format" field)))))))

(defun e-tools--validate-arguments (arguments parameters)
  "Validate object ARGUMENTS against the supported PARAMETERS schema.
The runtime enforces object shape, required fields, scalar types, enum values,
and string length and pattern constraints before transcript persistence."
  (when (equal (plist-get parameters :type) "object")
    (unless (e-tools--plist-p arguments)
      (signal 'e-tools-invalid-arguments
              (list "Tool arguments must be an object"))))
  (let ((properties (plist-get parameters :properties)))
    (dolist (name (e-tools--schema-list (plist-get parameters :required)))
      (let ((key (e-tools--schema-property-key name)))
        (unless (and key (plist-member arguments key))
          (signal 'e-tools-invalid-arguments
                  (list (format "Missing required tool argument: %s" name))))))
    (when (e-tools--plist-p arguments)
      (cl-loop for (key value) on arguments by #'cddr do
               (cond
                ((plist-member properties key)
                 (e-tools--validate-schema-value
                  value (plist-get properties key) key))
                ((eq (plist-get parameters :additionalProperties) :json-false)
                 (signal 'e-tools-invalid-arguments
                         (list "Tool arguments contain undeclared fields"))))))))

(defun e-tools--prepare-call-arguments (call tool)
  "Coerce and validate CALL arguments against TOOL's runtime schema."
  (let* ((parameters (plist-get tool :parameters))
         (arguments (e-tools--coerce-arguments
                     (plist-get call :arguments) parameters)))
    (e-tools--validate-arguments arguments parameters)
    (plist-put call :arguments arguments)))

(defun e-tools--prepare-call-arguments-with-context (call tool)
  "Validate CALL and retain its prepared arguments when schema validation fails."
  (condition-case err
      (e-tools--prepare-call-arguments call tool)
    (e-tools-invalid-arguments
     ;; Preserve the detached, schema-coerced operation arguments for the
     ;; harness archival side channel.  The projected transcript call is built
     ;; separately and may contain only values safe to show after rejection.
     (let* ((parameters (plist-get tool :parameters))
            (arguments (e-tools--coerce-arguments
                        (plist-get call :arguments) parameters)))
       (plist-put call :arguments arguments))
     (signal (car err)
             (append (cdr err) (list :prepared-call call))))))

(defun e-tools-prepare-call (registry call)
  "Return a validated, coerced copy of CALL from REGISTRY.
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
         (properties (and (listp parameters)
                          (plist-get parameters :properties)))
         (arguments (plist-get copy :arguments))
         projected)
    (when (and tool (e-tools--plist-p arguments))
      (cl-loop for (key value) on arguments by #'cddr do
               (when (and (plist-member properties key)
                          (condition-case nil
                              (progn
                                (e-tools--validate-schema-value
                                 value (plist-get properties key) key)
                                t)
                            (e-tools-invalid-arguments nil)))
                 (setq projected (append projected (list key value))))))
    (plist-put copy :arguments projected)))

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

(defun e-tools-definition-fingerprint (definition)
  "Return a stable material fingerprint for provider DEFINITION.
The JSON-normalized representation makes equivalent plist, alist, and hash
table schemas compare identically while preserving all material fields."
  (secure-hash 'sha256
               (json-encode (e-tools--json-normalize definition))))

(defun e-tools-result-content-text (content)
  "Return the model-visible text representation for tool result CONTENT."
  (cond
   ((stringp content) content)
   ((e-tools-file-content-p content)
    (or (e-tools-file-content-preview content) ""))
   (t
    (condition-case nil
        (json-encode (e-tools--json-normalize content))
      (error
       (prin1-to-string content))))))

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
                             e-tools-blocking-handler-rejected
                             e-tools-blocking-execute-rejected
                             e-tools-batch-execute-not-allowed
                             e-tools-nested-async-tool-rejected
                             e-tools-invalid-arguments))
           (stringp (cadr err)))
      (cadr err)
    (e-work-error-message err)))

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
  (let ((normalized (e-tools--copy-schema-value
                     (or parameters
                         (list :type "object"
                               :properties (e-tools--empty-json-object))))))
    (when (and (equal (plist-get normalized :type) "object")
               (null (plist-get normalized :properties)))
      (plist-put normalized :properties (e-tools--empty-json-object)))
    normalized))

(defun e-tools--decorated-parameters (parameters)
  "Return a detached provider-visible schema for PARAMETERS."
  (e-tools--normalize-parameters parameters))

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
                (setq settled t)
                (cancel-deadline)
                (when on-done
                  (funcall on-done
                           (e-tools--ok-result-from-content
                            call name content)))))
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
               (when-let ((enroll (plist-get tool-context :board-enroll-work)))
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
              (when-let ((deadline (effective-deadline)))
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
                   ;; Coerce arguments to the tool's declared schema types
                   ;; before dispatch.  Providers that JSON-stringify nested
                   ;; tool arguments (notably Bedrock) deliver object- and
                   ;; array-typed arguments as strings; reparse them against
                   ;; the schema so every tool sees structured data.  This runs
                   ;; inside the guarded region so a malformed argument fails as
                   ;; a tool-error result rather than aborting the whole turn.
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

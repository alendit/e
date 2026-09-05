;;; e-session-metadata.el --- Durable session metadata policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure schema, validation, JSON-shape, and legacy-normalization policy for
;; durable session metadata.  It owns no aggregate or persistence state.

;;; Code:

(require 'seq)
(require 'subr-x)

(defconst e-session-metadata-schema
  '((:name
     :owner session
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:model
     :owner session
     :state-class session-config
     :lifetime durable
     :indexed t
     :legacy t)
    (:project-root
     :owner session
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:harness-instance-id
     :owner chat
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:origin
     :owner shell
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:source
     :owner shell
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:source-reference
     :owner shell
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t)
    (:context-references
     :owner session
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t)
    (:org-canvas-ref
     :owner org-canvas
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t)
    (:org-canvas
     :owner org-canvas
     :state-class current-state-reference
     :lifetime durable-reference
     :indexed t
     :legacy t)
    (:parent-session-id
     :owner subagents
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:subagent-role
     :owner subagents
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:subagent-label
     :owner subagents
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:tmp-lineage-id
     :owner subagents
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:task-queue-task-id
     :owner task-queue
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:board-run-id
     :owner board-orchestration
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:board-task-key
     :owner board-orchestration
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:board-attempt
     :owner board-orchestration
     :state-class session-config
     :lifetime durable
     :indexed t)
    (:mcp-active
     :owner mcp
     :state-class capability-state
     :lifetime durable
     :indexed t)
    (:capability-state
     :owner capabilities
     :state-class capability-state
     :lifetime durable
     :indexed t))
  "Allowed durable session metadata keys and their state ownership.")

(defconst e-session-metadata--presentation-metadata-keys
  '(:e-chat-read-markers)
  "Presentation-only metadata keys rejected on write and removed on replay.")

(defun e-session-metadata--descriptor (key)
  "Return metadata schema descriptor for KEY."
  (seq-find (lambda (descriptor)
              (eq (car descriptor) key))
            e-session-metadata-schema))

(defun e-session-metadata-policy-key-state-class (key)
  "Return the declared state class for durable metadata KEY."
  (plist-get (cdr (e-session-metadata--descriptor key)) :state-class))

(defun e-session-metadata--plist-remove (plist key)
  "Return PLIST without KEY."
  (let (result)
    (while (consp plist)
      (let ((current-key (pop plist)))
        (when (consp plist)
          (let ((value (pop plist)))
            (unless (eq current-key key)
              (push current-key result)
              (push value result))))))
    (nreverse result)))

(defun e-session-metadata-keyword-plist-p (value)
  "Return non-nil when VALUE has keyword plist shape."
  (and (proper-list-p value)
       (let ((tail value)
             (valid t))
         (while (and valid tail)
           (setq valid
                 (and (consp tail)
                      (keywordp (car tail))
                      (consp (cdr tail))))
           (setq tail (cddr tail)))
         valid)))

(defun e-session-metadata-owner-key (owner)
  "Return stable keyword key for metadata OWNER."
  (cond
   ((keywordp owner) owner)
   ((symbolp owner) (intern (concat ":" (symbol-name owner))))
   ((stringp owner) (intern (concat ":" owner)))
   (t (error "Metadata owner must be a keyword, symbol, or string: %S" owner))))

(defun e-session-metadata--json-array-safe-value (value)
  "Return VALUE with reference arrays encoded unambiguously for JSON.
Keyword plists remain objects.  Other proper lists become vectors so the JSON
writer cannot reinterpret a list of plists as one object."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'e-session-metadata--json-array-safe-value value)))
   ((e-session-metadata-keyword-plist-p value)
    (let (result)
      (while (consp value)
        (let ((key (pop value)))
          (when (consp value)
            (push key result)
            (push (e-session-metadata--json-array-safe-value (pop value))
                  result))))
      (nreverse result)))
   ((proper-list-p value)
    (vconcat (mapcar #'e-session-metadata--json-array-safe-value value)))
   (t value)))

(defun e-session-metadata--public-value (value)
  "Return persisted metadata VALUE in caller-facing Elisp shape."
  (cond
   ((vectorp value)
    (mapcar #'e-session-metadata--public-value value))
   ((e-session-metadata-keyword-plist-p value)
    (let (result)
      (while (consp value)
        (let ((key (pop value)))
          (when (consp value)
            (push key result)
            (push (e-session-metadata--public-value (pop value)) result))))
      (nreverse result)))
   ((proper-list-p value)
    (mapcar #'e-session-metadata--public-value value))
   (t value)))

(defun e-session-metadata--validate-org-canvas-ref (value key)
  "Validate Org Canvas metadata VALUE under KEY."
  (unless (or (null value) (e-session-metadata-keyword-plist-p value))
    (error "Session metadata %S must be a keyword plist" key))
  (when (or (plist-member value :last-focus)
            (plist-member value :last-scope))
    (error "Session metadata %S must not contain volatile focus or scope" key)))

(defun e-session-metadata--validate-context-references (value)
  "Validate durable current-state reference VALUE."
  (unless (or (null value) (e-session-metadata-keyword-plist-p value))
    (error "Session metadata :context-references must be an owner-keyed plist")))

(defun e-session-metadata--validate-capability-state (value)
  "Validate durable capability-state VALUE."
  (unless (or (null value) (e-session-metadata-keyword-plist-p value))
    (error "Session metadata :capability-state must be an owner-keyed plist")))

(defun e-session-metadata--validate-metadata-value (key value)
  "Validate durable session metadata KEY VALUE."
  (pcase key
    ((or :org-canvas :org-canvas-ref)
     (e-session-metadata--validate-org-canvas-ref value key))
    (:context-references
     (e-session-metadata--validate-context-references value))
    (:capability-state
     (e-session-metadata--validate-capability-state value))
    (_ nil)))

(defun e-session-metadata-validate-entry (key value expected-class)
  "Validate durable metadata KEY and VALUE in EXPECTED-CLASS.

Unlike the plist validators this entry-shaped boundary requires no temporary
wrapper allocation, so command admission can reject a bad typed mutation
before reserving or freezing caller state."
  (let* ((descriptor (e-session-metadata--descriptor key))
         (state-class (plist-get (cdr descriptor) :state-class)))
    (unless descriptor
      (error "Session metadata key %S has no durable state schema" key))
    (unless (eq state-class expected-class)
      (error "Session metadata key %S is %S, not %S"
             key state-class expected-class))
    (e-session-metadata--validate-metadata-value key value))
  value)

(defun e-session-metadata-validate-create-input (metadata)
  "Validate caller METADATA accepted by session creation without copying it.

Legacy presentation-only keys are permitted because create normalization drops
them before persistence.  Every surviving key must have a durable schema."
  (unless (or (null metadata) (e-session-metadata-keyword-plist-p metadata))
    (error "Session metadata must be a keyword plist"))
  (let ((tail metadata))
    (while tail
      (let ((key (pop tail))
            (value (pop tail)))
        (unless (memq key e-session-metadata--presentation-metadata-keys)
          (e-session-metadata-validate-entry
           key value (e-session-metadata-policy-key-state-class key))))))
  metadata)

(defun e-session-metadata-validate-class (metadata expected-class)
  "Validate that METADATA only contains keys in EXPECTED-CLASS."
  (let ((tail metadata))
    (while (consp tail)
      (let ((key (pop tail)))
        (unless (consp tail)
          (error "Session metadata has key %S without value" key))
        (let ((value (pop tail)))
          (e-session-metadata-validate-entry
           key value expected-class)))))
  metadata)

(defun e-session-metadata-validate (metadata)
  "Validate durable session METADATA and return it."
  (unless (or (null metadata) (e-session-metadata-keyword-plist-p metadata))
    (error "Session metadata must be a keyword plist"))
  (let ((tail metadata))
    (while (consp tail)
      (let* ((key (pop tail))
             (value (pop tail))
             (descriptor (e-session-metadata--descriptor key)))
        (when (memq key e-session-metadata--presentation-metadata-keys)
          (error "Session metadata key %S is presentation state" key))
        (unless descriptor
          (error "Session metadata key %S has no durable state schema" key))
        (e-session-metadata--validate-metadata-value key value))))
  metadata)

(defun e-session-metadata--normalize-org-canvas-ref-for-replay (value)
  "Return legacy Org Canvas VALUE without volatile focus fields."
  (when value
    (setq value (copy-sequence value))
    (setq value (e-session-metadata--plist-remove value :last-focus))
    (setq value (e-session-metadata--plist-remove value :last-scope)))
  value)

(defun e-session-metadata--legacy-metadata-key (value)
  "Return schema metadata key named by legacy VALUE."
  (let ((name (cond
               ((keywordp value)
                (string-remove-prefix ":" (symbol-name value)))
               ((symbolp value) (symbol-name value))
               ((stringp value) (string-remove-prefix ":" value)))))
    (when name
      (seq-some (lambda (descriptor)
                  (let ((key (car descriptor)))
                    (and (string= name
                                  (string-remove-prefix
                                   ":" (symbol-name key)))
                         key)))
                e-session-metadata-schema))))

(defun e-session-metadata--normalize-legacy-metadata-array (metadata)
  "Repair legacy JSON-array METADATA into a schema-keyed plist.
This is only for replaying old persisted records that encoded metadata as
arrays and sometimes inverted key/value pairs."
  (if (or (null metadata)
          (e-session-metadata-keyword-plist-p metadata)
          (not (proper-list-p metadata)))
      metadata
    (let ((tail metadata)
          result
          repaired)
      (while (consp tail)
        (let* ((first (pop tail))
               (second (and (consp tail) (pop tail)))
               (first-key (e-session-metadata--legacy-metadata-key first))
               (second-key (e-session-metadata--legacy-metadata-key second)))
          (cond
           ((and first-key (not second-key))
            (setq result (plist-put result first-key second)
                  repaired t))
           ((and second-key (not first-key))
            (setq result (plist-put result second-key first)
                  repaired t)))))
      (if repaired result metadata))))

(defun e-session-metadata-normalize-for-replay (metadata &optional legacy)
  "Return replayed METADATA without known transient state."
  (when (and legacy
             (consp metadata)
             (not (keywordp (car metadata))))
    (setq metadata (e-session-metadata--normalize-legacy-metadata-array metadata)))
  (when metadata
    (setq metadata (copy-sequence metadata))
    (dolist (key e-session-metadata--presentation-metadata-keys)
      (setq metadata (e-session-metadata--plist-remove metadata key)))
    (when (plist-member metadata :org-canvas)
      (setq metadata
            (plist-put
             metadata
             :org-canvas
             (e-session-metadata--normalize-org-canvas-ref-for-replay
              (plist-get metadata :org-canvas)))))
    (when (plist-member metadata :org-canvas-ref)
      (setq metadata
            (plist-put
             metadata
             :org-canvas-ref
             (e-session-metadata--normalize-org-canvas-ref-for-replay
              (plist-get metadata :org-canvas-ref))))))
    ;; JSON arrays are read as lists by the codec.  Re-establish the
    ;; aggregate's canonical vector representation for durable current-state
    ;; reference arrays before a checkpoint projection crosses the JSON
    ;; boundary again; otherwise a list of reference plists is encoded as one
    ;; object and loses the established array shape.
    (when (plist-member metadata :context-references)
      (setq metadata
            (plist-put
             metadata
             :context-references
             (e-session-metadata--json-array-safe-value
              (plist-get metadata :context-references)))))
  metadata)

(defun e-session-metadata-reference-value (references)
  "Return REFERENCES in the canonical durable array representation."
  (e-session-metadata--json-array-safe-value references))

(defun e-session-metadata-context-references-value (metadata owner)
  "Return detached current-state references for OWNER from METADATA."
  (let ((references (plist-get metadata :context-references)))
    (copy-tree
     (e-session-metadata--public-value
      (plist-get references (e-session-metadata-owner-key owner))))))

(defun e-session-metadata-capability-state-value (metadata capability-id)
  "Return detached durable state for CAPABILITY-ID from METADATA."
  (let ((state (plist-get metadata :capability-state)))
    (copy-tree
     (e-session-metadata--public-value
      (plist-get state (e-session-metadata-owner-key capability-id))))))

(provide 'e-session-metadata)

;;; e-session-metadata.el ends here

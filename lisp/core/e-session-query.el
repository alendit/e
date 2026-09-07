;;; e-session-query.el --- Pure current-session query-state derivation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module is deliberately detached from the runtime-store, SQLite, and
;; session application services.  It is the domain-owned mapping from one
;; canonical session journal record to the bounded row-shaped current state
;; consumed by the later DP2 storage work.  The worker must receive the
;; resulting delta; it must not reinterpret any session policy.

;;; Code:

(require 'cl-lib)
(require 'e-session-metadata)
(require 'subr-x)

(define-error 'e-session-query-error "Invalid session query state")
(define-error 'e-session-query-record-error
  "Invalid session record for query-state derivation"
  'e-session-query-error)
(define-error 'e-session-query-context-erasure-error
  "Standalone context erasure cannot be replayed"
  'e-session-query-record-error)
(define-error 'e-session-query-delta-error
  "Invalid session query-state delta"
  'e-session-query-error)

(defconst e-session-query-state-string-byte-limit 4096
  "Maximum bytes for a scalar string retained in a query row.")
(defconst e-session-query-state-value-byte-limit (* 256 1024)
  "Maximum encoded-byte-equivalent size of one bounded semantic value.

This is a domain boundary, independent of the transport and SQLite adapter.
It prevents a metadata or option value from becoming an unbounded projection
while leaving the durable aggregate as the canonical source of detail.")
(defconst e-session-query-state-value-node-limit 8192
  "Maximum total scalar/container visits in one bounded semantic value.")
(defconst e-session-query-state-value-width-limit 4096
  "Maximum number of direct elements in one bounded semantic container.")
(defconst e-session-query-state-value-depth-limit 32
  "Maximum nesting depth of one bounded semantic value.")

(defun e-session-query--integer-byte-equivalent-size (value)
  "Return a bounded byte-equivalent size for integer VALUE.

Emacs Lisp does not expose Common Lisp's `integer-length'.  Shift by fixed
machine-independent chunks instead, stopping just beyond the semantic-value
limit so an extreme bignum cannot make validation itself unbounded."
  (let ((magnitude (abs value))
        (bytes 0))
    (while (and (> magnitude 0)
                (<= bytes e-session-query-state-value-byte-limit))
      (setq magnitude (ash magnitude -32)
            bytes (+ bytes 4)))
    (if (> magnitude 0)
        (1+ e-session-query-state-value-byte-limit)
      (max 1 bytes))))

(defconst e-session-query-supported-record-types
    '("session" "message" "activity-event" "message-display"
    "process-report" "branch-summary" "compaction" "provider-anchor"
    "context-generation" "context-promotion" "context-frame"
    "context-frame-settlement" "context-erasure" "context-curation-package"
    "messages-cleared" "board-message" "board-messages-cleared"
    "board-session-state" "current-branch" "session-info"
    "session-deleted")
  "Every durable session record family understood by this derivation seam.

Semantic no-op is intentionally absent.  A no-op is an operation result
returned by `e-session-query-delta-noop' when a command emits no durable
record; it is not an invented journal family.")

(defconst e-session-query-state-keys
  '(:session-id :name :summary :metadata :created-at :updated-at
    :last-message-at :latest-assistant-marker :message-count
    :current-branch :turn-options :current-head-id :root-event-id
    :current-context-generation-id
    :board-id :principal :association-role :routing-policy :root-p
    :board-output-sequence :board-activity-sequence :journal-position)
  "Closed row-shaped query-state ABI.

No key contains a whole aggregate, transcript, catalog, checkpoint, or other
opaque durable collection.  Metadata and turn options are bounded semantic
values retained as named current values; the later adapter decides their
physical bounded payload representation.  `:journal-position' is the stable
durable ordering value supplied by the journal/adapter, never a per-session
replay counter.")

(defun e-session-query--string-p (value)
  "Return non-nil when VALUE is a bounded scalar string or nil."
  (or (null value)
      (and (stringp value)
           (<= (string-bytes value) e-session-query-state-string-byte-limit))))

(defun e-session-query--string-prefix (value)
  "Return a detached UTF-8-safe query-row prefix of string VALUE.

The result contains at most `e-session-query-state-string-byte-limit' bytes.
This is for deliberately lossy preview columns such as a session summary; it
must not be used for durable identity or canonical content."
  (unless (stringp value)
    (signal 'wrong-type-argument (list 'stringp value)))
  (let ((limit e-session-query-state-string-byte-limit))
    (if (<= (string-bytes value) limit)
        (copy-sequence value)
      (let ((low 0)
            (high (min (length value) limit)))
        ;; A prefix's encoded byte size is monotonic in its character count.
        ;; Binary search avoids repeatedly walking a potentially large durable
        ;; message while finding the largest prefix admitted by the row ABI.
        (while (< low high)
          (let ((middle (/ (+ low high 1) 2)))
            (if (<= (string-bytes (substring value 0 middle)) limit)
                (setq low middle)
              (setq high (1- middle)))))
        (substring value 0 low)))))

(defun e-session-query--bounded-copy (value)
  "Return detached VALUE when it fits the domain semantic-value bounds.

Lists and vectors are both admitted as semantic containers, but every nested
container is copied.  The walk charges total nodes and byte-equivalent size,
tracks active containers to reject cycles, and bounds depth; it does not use a
per-level collection allowance that would permit exponential retained state."
  (let ((active (make-hash-table :test 'eq))
        (nodes 0)
        (bytes 0))
    (cl-labels
        ((fail
          (message &rest data)
          (signal 'e-session-query-delta-error
                  (cons message data)))
         (charge-node
          ()
          (setq nodes (1+ nodes))
          (when (> nodes e-session-query-state-value-node-limit)
            (fail "Session query value exceeds node bound"
                  :limit e-session-query-state-value-node-limit
                  :observed-at-least nodes)))
         (charge-bytes
          (count)
          (setq bytes (+ bytes count))
          (when (> bytes e-session-query-state-value-byte-limit)
            (fail "Session query value exceeds byte-equivalent bound"
                  :limit e-session-query-state-value-byte-limit
                  :observed-at-least bytes)))
         (walk
          (item depth)
          (when (> depth e-session-query-state-value-depth-limit)
            (fail "Session query value exceeds nesting bound"
                  :limit e-session-query-state-value-depth-limit))
          (charge-node)
          (cond
           ((null item) (charge-bytes 1) nil)
           ((eq item t) (charge-bytes 1) t)
           ((stringp item)
            (let ((size (string-bytes item)))
              (when (> size e-session-query-state-string-byte-limit)
                (fail "Session query string exceeds scalar bound"
                      :limit e-session-query-state-string-byte-limit))
              (charge-bytes size)
              (copy-sequence item)))
         ((numberp item)
            ;; Charge integer storage from its magnitude.  A fixed machine
            ;; size would let an arbitrarily large bignum evade the total
            ;; semantic-value bound without allocating a printer string.
            (charge-bytes
             (if (integerp item)
                 (e-session-query--integer-byte-equivalent-size item)
               8))
            item)
           ((symbolp item)
            (let ((size (string-bytes (symbol-name item))))
              (when (> size e-session-query-state-string-byte-limit)
                (fail "Session query symbol exceeds scalar bound"
                      :limit e-session-query-state-string-byte-limit))
              (charge-bytes size)
              item))
           ((vectorp item)
            (when (> (length item) e-session-query-state-value-width-limit)
              (fail "Session query vector exceeds width bound"
                    :limit e-session-query-state-value-width-limit))
            (charge-bytes (* 2 (length item)))
            (when (gethash item active)
              (fail "Cyclic session query value"))
            (puthash item t active)
            (unwind-protect
                (let ((copy (make-vector (length item) nil)))
                  (dotimes (index (length item))
                    (aset copy index (walk (aref item index) (1+ depth))))
                  copy)
              (remhash item active)))
           ((consp item)
            (when (gethash item active)
              (fail "Cyclic session query value"))
            (puthash item t active)
            (unwind-protect
                (let ((tail item)
                      (copy nil)
                      (width 0))
                  (while (consp tail)
                    (setq width (1+ width))
                    (when (> width e-session-query-state-value-width-limit)
                      (fail "Session query list exceeds width bound"
                            :limit e-session-query-state-value-width-limit))
                    (push (walk (car tail) (1+ depth)) copy)
                    (setq tail (cdr tail)))
                  (unless (null tail)
                    (fail "Improper session query value"))
                  (charge-bytes (* 2 (length copy)))
                  (nreverse copy))
              (remhash item active)))
           (t
            (fail "Unsupported session query value" :value item)))))
      (walk value 0))))

(defun e-session-query--bounded-value-p (value)
  "Return non-nil when VALUE fits the detached semantic-value boundary."
  (condition-case nil
      (progn (e-session-query--bounded-copy value) t)
    (e-session-query-error nil)))

(defun e-session-query--copy-value (value)
  "Detach VALUE without introducing a second durable projection."
  (e-session-query--bounded-copy value))

(defun e-session-query--exact-plist-keys-p (value keys)
  "Return non-nil when VALUE has exactly the unique keyword set KEYS.

Key order is intentionally not part of this ABI, but missing, unknown, odd,
and duplicate keys are rejected.  The bounded loop avoids first accepting a
large arbitrary plist merely because its first few fields look familiar."
  (and (proper-list-p value)
       (let ((tail value)
             (seen nil)
             (count 0)
             (valid t))
         (while (and valid tail)
           (let ((key (pop tail)))
             (if (or (not (keywordp key))
                     (not (memq key keys))
                     (memq key seen)
                     (null tail))
                 (setq valid nil)
               (pop tail)
               (push key seen)
               (setq count (1+ count)))))
         (and valid
              (= count (length keys))
              (= count (length seen))))))

(defun e-session-query--root-p (metadata)
  "Return the user-facing root predicate for METADATA."
  (not (or (plist-get metadata :parent-session-id)
           (plist-get metadata :subagent-role)
           (plist-get metadata :task-queue-task-id))))

(defun e-session-query--association (session)
  "Return SESSION's possible Board association value."
  (cond
   ((plist-member session :board-session-state)
    (plist-get session :board-session-state))
   ((plist-member session :board-state)
    (plist-get session :board-state))
   (t nil)))

(defun e-session-query--association-valid-p (association)
  "Return non-nil when ASSOCIATION has a bounded Board shape."
  (and (proper-list-p association)
       (let ((tail association)
             (seen nil)
             (valid t))
         (while (and valid tail)
           (let ((key (pop tail)))
             (setq valid
                   (and (keywordp key)
                        (memq key '(:board-id :principal :association-role
                                    :routing-policy))
                        (not (memq key seen))
                        (consp tail)))
             (push key seen)
             (when valid (pop tail))))
         (and valid
              (stringp (plist-get association :board-id))
              (e-session-query--string-p (plist-get association :board-id))
              (stringp (plist-get association :principal))
              (e-session-query--string-p (plist-get association :principal))
              (e-session-query--string-p
               (plist-get association :association-role))
              (e-session-query--bounded-value-p
               (plist-get association :routing-policy))))))

(defun e-session-query--association-fields (association)
  "Return the named Board association columns from ASSOCIATION."
  (when association
    (unless (e-session-query--association-valid-p association)
      (signal 'e-session-query-record-error
              (list "Invalid Board/session association" association))))
  (list :board-id (plist-get association :board-id)
        :principal (plist-get association :principal)
        :association-role (e-session-query--copy-value
                           (plist-get association :association-role))
        :routing-policy (e-session-query--copy-value
                         (plist-get association :routing-policy))))

(defun e-session-query-state-from-session (session)
  "Return a detached row-shaped current state derived from SESSION.

SESSION is an in-memory domain value supplied by the aggregate; this function
does not retain it and never reads storage.  It intentionally reads only
already-derived scalar fields and bounded metadata, not durable transcript
lists, so a caller can use it as the one live-mutation/migration derivation
boundary later."
  (unless (and (listp session) (stringp (plist-get session :id)))
    (signal 'e-session-query-error (list "Session lacks an identity" session)))
  (let* ((metadata (e-session-query--copy-value (plist-get session :metadata)))
         (association (e-session-query--association session))
         ;; This is a derived aggregate field, not a license to scan the
         ;; durable transcript.  The later adapter can therefore materialize
         ;; the row without loading `:messages'.
         (message-count (plist-get session :message-count))
         (state
          (append
           (list :session-id (plist-get session :id)
                 :name (plist-get session :name)
                 :summary (plist-get session :summary)
                 :metadata metadata
                 :created-at (plist-get session :created-at)
                 :updated-at (plist-get session :updated-at)
                 :last-message-at (plist-get session :last-message-at)
                 :latest-assistant-marker
                 (plist-get session :latest-assistant-marker)
                 :message-count message-count
                 :current-branch (plist-get session :current-branch)
                 :turn-options
                 (e-session-query--copy-value
                  (plist-get session :turn-options))
                 :current-head-id (plist-get session :current-head-id)
                 :root-event-id (plist-get session :root-event-id)
                 :current-context-generation-id
                 (when-let* ((entry
                              (car (last (plist-get session
                                                    :context-generations))))
                             (record (plist-get entry :context-record)))
                   (plist-get record :id))
                 :root-p (e-session-query--root-p metadata)
                 :board-output-sequence
                 (or (plist-get session :board-output-sequence) 0)
                 :board-activity-sequence
                 (or (plist-get session :board-activity-sequence) 0)
                 :journal-position
                 (or (plist-get session :journal-position) 0))
           (e-session-query--association-fields association))))
    (e-session-query-state-validate state)
    state))

(defun e-session-query-state-validate (state)
  "Signal unless STATE is a bounded complete query-state row."
  (unless (and (e-session-query--exact-plist-keys-p
                state e-session-query-state-keys)
               (stringp (plist-get state :session-id))
               (e-session-query--string-p (plist-get state :session-id))
               (e-session-query--string-p (plist-get state :name))
               (e-session-query--string-p (plist-get state :summary))
               (e-session-query--bounded-value-p
                (plist-get state :metadata))
               (e-session-query--string-p (plist-get state :created-at))
               (e-session-query--string-p (plist-get state :updated-at))
               (e-session-query--string-p (plist-get state :last-message-at))
               (e-session-query--string-p
                (plist-get state :latest-assistant-marker))
               (integerp (plist-get state :message-count))
               (>= (plist-get state :message-count) 0)
               (e-session-query--string-p (plist-get state :current-branch))
               (e-session-query--bounded-value-p
                (plist-get state :turn-options))
               (e-session-query--string-p
                (plist-get state :current-head-id))
               (e-session-query--string-p (plist-get state :root-event-id))
               (e-session-query--string-p
                (plist-get state :current-context-generation-id))
               (e-session-query--string-p (plist-get state :board-id))
               (e-session-query--string-p (plist-get state :principal))
               (e-session-query--string-p
                (plist-get state :association-role))
               (e-session-query--bounded-value-p
                (plist-get state :routing-policy))
               (memq (plist-get state :root-p) '(nil t))
               (integerp (plist-get state :board-output-sequence))
               (>= (plist-get state :board-output-sequence) 0)
               (integerp (plist-get state :board-activity-sequence))
               (>= (plist-get state :board-activity-sequence) 0)
               (integerp (plist-get state :journal-position))
               (>= (plist-get state :journal-position) 0))
    (signal 'e-session-query-delta-error
            (list "Malformed row-shaped session query state" state)))
  state)

(defun e-session-query-control-delta-validate (delta)
  "Signal unless DELTA is an exact deletion or no-op control delta."
  (unless (and (proper-list-p delta)
               (or (e-session-query--exact-plist-keys-p
                    delta '(:session-id :deleted))
                   (e-session-query--exact-plist-keys-p
                    delta '(:session-id :noop)))
               (stringp (plist-get delta :session-id))
               (e-session-query--string-p (plist-get delta :session-id))
               (or (eq (plist-get delta :deleted) t)
                   (eq (plist-get delta :noop) t)))
    (signal 'e-session-query-delta-error
            (list "Malformed control session query delta" delta)))
  delta)

(defun e-session-query--record-shape-p (record)
  "Return non-nil when RECORD is a proper keyword plist."
  (and (proper-list-p record)
       (let ((tail record)
             (seen nil)
             (valid t))
         (while (and valid tail)
           (let ((key (pop tail)))
             (setq valid
                   (and (keywordp key)
                        (not (memq key seen))
                        (consp tail)))
             (when valid
               (push key seen)
               (pop tail))))
         valid)))

(defun e-session-query--record-type (record)
  "Return RECORD's canonical string type."
  (let ((type (plist-get record :type)))
    (cond ((stringp type) type)
          ((symbolp type) (symbol-name type))
          (t nil))))

(defun e-session-query--record-time (record fallback)
  "Return RECORD timestamp or FALLBACK, validating its scalar shape."
  (let ((time (or (plist-get record :timestamp) fallback)))
    (unless (e-session-query--string-p time)
      (signal 'e-session-query-record-error
              (list "Session record has an invalid timestamp" record)))
    time))

(defun e-session-query--record-position (record fallback)
  "Return RECORD's stable journal position or FALLBACK."
  (let ((position (cond
                   ((plist-member record :journal-position)
                    (plist-get record :journal-position))
                   ((plist-member record :position)
                    (plist-get record :position))
                   (t fallback))))
    (unless (and (integerp position) (>= position 0))
      (signal 'e-session-query-record-error
              (list "Session record has an invalid journal position" record)))
    position))

(defun e-session-query--touch (state record &optional advance-head-p)
  "Update STATE's timestamp/order and optionally its aggregate head.

Only records that become session aggregate entries advance `:current-head-id'.
Display and Board journal records still touch recency but do not become the
session's parent-chain head, matching aggregate replay semantics."
  (let ((timestamp (e-session-query--record-time
                    record (plist-get state :updated-at))))
    (plist-put state :updated-at timestamp)
    (plist-put state :journal-position
               (e-session-query--record-position
                record (plist-get state :journal-position)))
    (when (and advance-head-p (plist-member record :id))
      (let ((id (plist-get record :id)))
        (unless (e-session-query--string-p id)
          (signal 'e-session-query-record-error
                  (list "Session record has an invalid identity" record)))
        (plist-put state :current-head-id id)))
    state))

(defun e-session-query--metadata-merge (metadata key value record)
  "Apply one bounded metadata FIELD KEY/VALUE from RECORD."
  (let ((result (e-session-query--copy-value metadata)))
    (cond
      ((eq key 'metadata)
       (setq result (e-session-query--copy-value value)))
      ((eq key 'config)
       (unless (and (listp value) (proper-list-p value)
                    (zerop (% (length value) 2)))
         (signal 'e-session-query-record-error
                 (list "Session config delta is not a plist" record)))
       (let ((tail value))
         (while tail
           (let ((config-key (pop tail))
                 (config-value (pop tail)))
             (unless (keywordp config-key)
               (signal 'e-session-query-record-error
                       (list "Session config key is not a keyword" record)))
             (setq result
                   (plist-put result config-key
                              (e-session-query--copy-value config-value)))))))
      ((eq key 'context-reference)
       (unless (keywordp (plist-get record :key))
         (signal 'e-session-query-record-error
                 (list "Session context reference key is invalid" record)))
       (setq result
             (plist-put result (plist-get record :key)
                        (e-session-query--copy-value value))))
      ((eq key 'context-references)
       (let* ((references
               (e-session-query--copy-value
                (plist-get result :context-references)))
              (owner-key
               (e-session-metadata-owner-key (plist-get record :owner))))
         (setq references
               (plist-put references owner-key
                          (e-session-metadata-reference-value
                           (e-session-query--copy-value value))))
         (setq result (plist-put result :context-references references))))
      ((eq key 'capability-state)
       (let* ((all (e-session-query--copy-value
                    (plist-get result :capability-state)))
              (owner-key
               (e-session-metadata-owner-key
                (plist-get record :capability-id)))
              (entry (if (plist-get record :version)
                         (list :version (plist-get record :version)
                               :state value)
                       value)))
         (setq all (plist-put all owner-key
                              (e-session-query--copy-value entry)))
         (setq result (plist-put result :capability-state all))))
      (t (signal 'e-session-query-record-error
                (list "Unsupported session metadata field" key))))
    (unless (e-session-query--bounded-value-p result)
      (signal 'e-session-query-record-error
              (list "Session metadata exceeds query bounds" record)))
    result))

(defun e-session-query-metadata-apply-record (metadata record)
  "Return METADATA after applying one session-info RECORD.
This pure domain mapping is shared by complete current-row derivation and by
bounded in-flight context composition."
  (let ((field (plist-get record :field)))
    (unless (memq field '(metadata config context-reference
                          context-references capability-state))
      (signal 'e-session-query-record-error
              (list "Record is not a metadata session-info mutation" record)))
    (e-session-query--metadata-merge
     metadata field (plist-get record :value) record)))

(defun e-session-query--new-state (record)
  "Build the initial query row from a canonical session RECORD."
  (let* ((metadata (e-session-query--copy-value
                    (plist-get record :metadata)))
         (timestamp (e-session-query--record-time record nil))
         (created-at (or (plist-get record :created-at) timestamp))
         (updated-at (or (plist-get record :updated-at) timestamp))
         (state (list :session-id (plist-get record :session-id)
                      :name (or (plist-get record :name)
                                (plist-get metadata :name))
                      :summary nil :metadata metadata
                      :created-at created-at :updated-at updated-at
                      :last-message-at nil
                      :latest-assistant-marker nil :message-count 0
                      :current-branch (plist-get record :current-branch)
                      :turn-options
                      (e-session-query--copy-value
                       (plist-get record :turn-options))
                      :current-head-id (plist-get record :id)
                      :root-event-id (plist-get record :id)
                      :current-context-generation-id nil
                      :board-id nil :principal nil :association-role nil
                      :routing-policy nil
                      :root-p (e-session-query--root-p metadata)
                      :board-output-sequence
                      (or (plist-get record :board-output-sequence) 0)
                      :board-activity-sequence
                      (or (plist-get record :board-activity-sequence) 0)
                      :journal-position
                      (e-session-query--record-position record 0))))
    (unless (and (stringp (plist-get record :session-id))
                 (e-session-query--string-p (plist-get record :session-id))
                 (stringp (plist-get record :id))
                 (e-session-query--string-p (plist-get record :id))
                 (e-session-query--string-p created-at)
                 (e-session-query--string-p updated-at))
      (signal 'e-session-query-record-error
              (list "Malformed session root record" record)))
    (e-session-query-state-validate state)
    state))

(defun e-session-query--sequence-max (state key value record)
  "Set STATE KEY to VALUE's nonnegative maximum when VALUE is present."
  (when (plist-member record key)
    (unless (and (integerp value) (>= value 0))
      (signal 'e-session-query-record-error
              (list "Invalid Board sequence" key value record)))
    (plist-put state key (max (or (plist-get state key) 0) value))))

(defun e-session-query--message-role (message)
  "Return MESSAGE role as a symbol/string for query derivation."
  (let ((role (plist-get message :role)))
    (cond ((equal role "user") 'user)
          ((equal role "assistant") 'assistant)
          (t role))))

(defun e-session-query--apply-message (state record)
  "Apply one message RECORD to STATE."
  (let* ((message (plist-get record :message))
         (created-at (or (plist-get message :created-at)
                         (plist-get record :timestamp)))
         (role (e-session-query--message-role message)))
    ;; A message may contain large canonical content and structured provider
    ;; data.  The current-row projection needs only these named scalars; do not
    ;; copy or reject the durable message merely because an unused field is
    ;; larger than a query-row value.
    (unless (and (e-session-query--record-shape-p message)
                 (e-session-query--string-p created-at))
      (signal 'e-session-query-record-error
              (list "Invalid session message record" record)))
    (plist-put state :message-count
               (1+ (or (plist-get state :message-count) 0)))
    (when (and (null (plist-get state :summary))
               (eq role 'user)
               (stringp (plist-get message :content)))
      (plist-put state :summary
                 (e-session-query--string-prefix
                  (plist-get message :content))))
    (plist-put state :last-message-at created-at)
    (when (eq role 'assistant)
      (plist-put state :latest-assistant-marker
                 (or (plist-get message :id)
                     (plist-get record :id)
                     created-at)))
    (e-session-query--sequence-max
     state :board-output-sequence
     (plist-get message :board-output-sequence) message)
    (e-session-query--touch state record t)))

(defun e-session-query--context-v1-p (record)
  "Return non-nil when RECORD carries the retired version-1 context form."
  (let ((context-record (plist-get record :context-record)))
    (and (listp context-record)
         (equal (plist-get context-record :record-version) 1))))

(defun e-session-query-state-apply-record (state record)
  "Return a new current query STATE after applying RECORD.

This is a pure function: STATE and RECORD are never mutated.  A deletion
returns an exact control delta carrying `:deleted'.  Every other durable
supported record returns a complete row-shaped state, except replay-only
context-frame records and version-1 context records, which preserve the
current state.  A semantic no-op is produced by `e-session-query-delta-noop'
when the application command emits no durable record."
  (unless (and (e-session-query--record-shape-p record)
               (stringp (plist-get record :session-id))
               (e-session-query--string-p (plist-get record :session-id)))
    (signal 'e-session-query-record-error
            (list "Session record lacks a bounded session identity" record)))
  (let* ((type (e-session-query--record-type record))
         (session-id (plist-get record :session-id)))
    (unless (member type e-session-query-supported-record-types)
      (signal 'e-session-query-record-error
              (list "Unsupported session record family" type)))
    (when (and state
               (or (plist-get state :deleted)
                   (plist-get state :noop)))
      (signal 'e-session-query-record-error
              (list "Session record follows a control delta" record)))
    (when (and state (not (equal session-id (plist-get state :session-id))))
      (signal 'e-session-query-record-error
              (list "Session record identity does not match query state"
                    session-id (plist-get state :session-id))))
    (cond
     ((equal type "context-erasure")
      (signal 'e-session-query-context-erasure-error
              (list "Standalone context erasure records are unsupported"
                    record)))
     ((equal type "session")
      (when state
        (signal 'e-session-query-record-error
                (list "Duplicate session root" session-id)))
      (e-session-query--new-state record))
     ((null state)
      (signal 'e-session-query-record-error
              (list "Session record precedes its root" record)))
     ((member type '("context-frame" "context-frame-settlement"))
      (e-session-query--copy-value state))
     ((equal type "session-deleted")
      (e-session-query-delta-delete session-id))
     (t
      (let ((next (e-session-query--copy-value state)))
        (pcase type
          ("message"
           (e-session-query--apply-message next record))
          ("activity-event"
           (e-session-query--sequence-max
            next :board-activity-sequence
            (or (plist-get record :board-activity-sequence)
                (plist-get (plist-get record :semantic-event)
                           :board-activity-sequence))
            record)
           (e-session-query--touch next record t))
          ("message-display"
           ;; A missing message command emits no durable record.  Therefore
           ;; any record reaching this boundary is durable and touches recency
           ;; even when its target is not present in this bounded row.
           (e-session-query--touch next record nil))
          ((or "process-report" "branch-summary" "compaction"
               "provider-anchor" "context-curation-package")
           (e-session-query--touch next record t))
          ("board-message"
           (e-session-query--touch next record nil))
          ((or "context-generation" "context-promotion")
           ;; Version-1 context history is readable but aggregate replay
           ;; intentionally ignores it; newer owned records touch recency.
           (unless (e-session-query--context-v1-p record)
             (when (equal type "context-generation")
               (plist-put next :current-context-generation-id
                          (plist-get (plist-get record :context-record) :id)))
             (e-session-query--touch next record t)))
          ("messages-cleared"
           (plist-put next :message-count 0)
           (plist-put next :summary nil)
           (plist-put next :last-message-at nil)
           (plist-put next :latest-assistant-marker nil)
           ;; Aggregate replay clears message/activity lists but keeps Board
           ;; sequence watermarks and the current context-generation identity.
           ;; The latter owns curation records independently of the visible
           ;; transcript and cannot be discarded by a message reset.
           (plist-put next :current-head-id (plist-get next :root-event-id))
           (e-session-query--touch next record t))
          ("board-messages-cleared"
           (e-session-query--touch next record nil))
          ("board-session-state"
           (let ((association (plist-get record :board-state)))
             (unless (e-session-query--association-valid-p association)
               (signal 'e-session-query-record-error
                       (list "Invalid Board/session association" record)))
             (let ((fields (e-session-query--association-fields association)))
               (while fields
                 (plist-put next (pop fields) (pop fields)))))
           ;; The aggregate keeps Board sequence watermarks from message and
           ;; activity entries; this association record only changes identity.
           (e-session-query--touch next record nil))
          ("current-branch"
           (plist-put next :current-branch (plist-get record :branch-id))
           (e-session-query--touch next record t))
          ("session-info"
           (let ((field (plist-get record :field)))
             (if field
                 (pcase field
                   ('name
                    (plist-put next :name
                               (e-session-query--copy-value
                                (plist-get record :value))))
                   ('turn-options
                    (plist-put next :turn-options
                               (e-session-query--copy-value
                                (plist-get record :value))))
                   ((or 'metadata 'config 'context-reference
                        'context-references 'capability-state)
                    (plist-put next :metadata
                               (e-session-query-metadata-apply-record
                                (plist-get next :metadata) record)))
                   (_ (signal 'e-session-query-record-error
                              (list "Unsupported session-info field" field))))
               (dolist (key '(:name :metadata :turn-options))
                 (when (plist-member record key)
                   (plist-put next key
                              (e-session-query--copy-value
                               (plist-get record key)))))))
           (plist-put next :root-p
                      (e-session-query--root-p
                       (plist-get next :metadata)))
           (e-session-query--touch next record t)))
        (e-session-query-state-validate next)
        next)))))

(defun e-session-query-delta-from-record (state record)
  "Return a bounded explicit delta for RECORD against STATE.

The result is either a complete row-shaped update or `:deleted'.  Callers may
pass nil STATE only for a session root.  Ignored version-1 context history and
replay-only frames still require an already-known root, preventing a worker or
adapter from silently inventing state for an unknown identity."
  (let ((delta (e-session-query-state-apply-record state record)))
    (if (or (plist-get delta :deleted) (plist-get delta :noop))
        (e-session-query-control-delta-validate delta)
      (e-session-query-state-validate delta))))

(defun e-session-query-delta-noop (state session-id)
  "Return an exact semantic no-op delta for SESSION-ID against STATE."
  (unless (and (listp state)
               (equal session-id (plist-get state :session-id)))
    (signal 'e-session-query-delta-error
            (list "No-op identity does not match query state" session-id)))
  (e-session-query-state-validate state)
  (e-session-query-control-delta-validate
   (list :session-id session-id :noop t)))

(defun e-session-query-delta-delete (session-id)
  "Return an exact deletion delta for SESSION-ID."
  (unless (and (stringp session-id)
               (<= (string-bytes session-id)
                   e-session-query-state-string-byte-limit))
    (signal 'e-session-query-delta-error
            (list "Invalid deleted session identity" session-id)))
  (e-session-query-control-delta-validate
   (list :session-id session-id :deleted t)))

(defun e-session-query-derive (records)
  "Replay RECORDS into one detached query delta in deterministic order."
  (unless (and (listp records) (proper-list-p records))
    (signal 'e-session-query-record-error
            (list "Session journal must be a proper list" records)))
  (let (state)
    (dolist (record records)
      (setq state (e-session-query-state-apply-record state record)))
    (when state
      (if (or (plist-get state :deleted) (plist-get state :noop))
          (e-session-query-control-delta-validate state)
        (e-session-query-state-validate state)))
    state))

(provide 'e-session-query)

;;; e-session-query.el ends here

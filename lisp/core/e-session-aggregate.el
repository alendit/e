;;; e-session-aggregate.el --- Session aggregate and semantic mutations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Owns the live session aggregate, identity/path semantics, board journal,
;; provider-neutral metadata, and semantic mutations.  Persistence is an
;; application-service concern: this module has no dependency on the catalog
;; or storage adapter.  It consumes the pure codec only for one canonical
;; routing-value size check; it never performs durable I/O or replay decoding.

;;; Code:

(require 'cl-lib)
(require 'e-context-lifetime)
(require 'e-session-codec)
(require 'e-session-board-policy)
(require 'e-session-identity)
(require 'e-session-metadata)
(require 'e-session-provider-anchor)
(require 'e-board)
(require 'seq)
(require 'subr-x)

(declare-function e-dev-profile-measure-thunk "e-dev-profile")

(define-error 'e-session-missing "Session does not exist")
(define-error 'e-session-duplicate "Session already exists")
(define-error 'e-session-checkpoint-missing "Session resume checkpoint does not exist" 'e-session-missing)
(define-error 'e-session-checkpoint-invalid "Session resume checkpoint is invalid")
(define-error 'e-session-board-message-conflict "Conflicting board message envelope")
(define-error 'e-session-board-message-cycle "Cyclic board message envelope")
(define-error 'e-session-board-message-invalid-record-type "Invalid board message record type")
(define-error 'e-session-error "Session error")
(define-error 'e-session-command-too-large
  "Session command producer exceeds its domain limit" 'e-session-error)

(defconst e-session-aggregate-command-practical-byte-limit (* 1024 1024)
  "Maximum aggregate-owned producer bytes admitted before detachment.")

(defconst e-session-aggregate-command-practical-node-limit 8192
  "Maximum producer container visits admitted before detachment.")

(defvar e-session-aggregate--committed-apply-fault-function nil
  "Optional test-only function called at committed-apply journal boundaries.")

(defun e-session-aggregate--committed-apply-fault (boundary)
  "Invoke the test-only committed-apply fault seam at BOUNDARY."
  (when e-session-aggregate--committed-apply-fault-function
    (funcall e-session-aggregate--committed-apply-fault-function boundary)))

(defgroup e-session nil "Session storage for e." :group 'e :prefix "e-session-")
(defcustom e-session-directory (locate-user-emacs-file "e/sessions/")
  "Default directory used for persisted e sessions."
  :type 'directory :group 'e-session)

;; The aggregate owns the deterministic set of context entry types.  The
;; codec validates the fixed durable record versions independently; no runtime
;; registry or load-order-sensitive registration hook is needed.
(defconst e-session-aggregate--context-lifetime-entry-types
  '(context-generation context-promotion)
  "Durable entry types owned by the generational context lifetime model.")

(cl-defstruct (e-session-store (:constructor e-session-store-create))
  (sessions (make-hash-table :test 'equal))
  (entry-indexes (make-hash-table :test 'equal))
  (board-journals (make-hash-table :test 'equal))
  directory
  sessions-directory
  index-file
  persistent
  write-mode
  (sequence 0))

(cl-defstruct (e-session-board-journal
               (:constructor e-session-aggregate--board-journal-create))
  messages tail (id-index (make-hash-table :test 'equal)))

(cl-defstruct (e-session-aggregate-command
               (:constructor e-session-aggregate-command--create))
  "One sealed durable command owned by the session application service.

The command deliberately stores semantic tag/identity/input only; it never
contains a staged session or a caller-provided mutation closure."
  tag session-id arguments request-id delta-id timestamp)

(cl-defstruct (e-session-aggregate-install-token
               (:constructor e-session-aggregate--install-token-create))
  "One bounded prevalidated aggregate mutation prepared before FIFO admission.

COMMAND, DELTA, and BODY share the frozen producer leaves.  Applying the token
publishes at most DELTA's one record to the effective aggregate; it never owns
a second session projection."
  command delta body result installed)

(defconst e-session-aggregate-command-tags
  '(create append-message append-activity context-curation-response
    message-display process-report branch-summary compaction provider-anchor
    context-generation context-curation-package clear-messages
    board-message board-state board-messages-clear delete session-info)
  "Closed durable command tags implemented by the C07 session grammar.")

(defun e-session-aggregate--bounded-domain-string-p (value limit)
  "Return non-nil when VALUE is nil or a string of at most LIMIT bytes."
  (or (null value) (and (stringp value) (<= (string-bytes value) limit))))

(defun e-session-aggregate--bounded-domain-identity-p (value limit)
  "Return non-nil when VALUE is a string/symbol identity within LIMIT bytes."
  (and (or (stringp value) (symbolp value))
       (<= (string-bytes (if (stringp value) value (symbol-name value))) limit)))

(defun e-session-aggregate--proper-list-length-at-most-p (value limit)
  "Return non-nil when VALUE is a proper list no longer than LIMIT."
  (let ((tail value)
        (count 0))
    (while (and (consp tail) (<= count limit))
      (setq tail (cdr tail) count (1+ count)))
    (and (null tail) (<= count limit))))

(defun e-session-aggregate--context-generation-record-admissible-p (record)
  "Return non-nil when RECORD is an already-canonical generation record."
  (and (e-session-aggregate--exact-plist-keys-p
        record '(:record-version :type :id :checkpoint
                 :covered-session-boundary))
       (equal (plist-get record :record-version)
              e-context-lifetime-record-version)
       (eq (plist-get record :type) 'context-generation)
       (e-session-aggregate--bounded-id-value-p (plist-get record :id))
       (e-session-aggregate--bounded-id-value-p
        (plist-get record :covered-session-boundary))
       (let ((checkpoint (plist-get record :checkpoint))
             (valid t))
         (unless (proper-list-p checkpoint) (setq valid nil))
         (dolist (message checkpoint)
           (unless (and (e-session-aggregate--exact-plist-keys-p
                         message '(:role :content))
                        (memq (plist-get message :role)
                              e-context-lifetime-portable-message-roles)
                        (e-session-aggregate--canonical-context-value-p
                         (plist-get message :content)))
             (setq valid nil)))
         valid)))

(defun e-session-aggregate--string-properties-p (string)
  "Return non-nil when STRING contains presentation text properties."
  (let ((position 0) found)
    (while (and (< position (length string)) (not found))
      (setq found (text-properties-at position string)
            position (next-property-change position string (length string))))
    found))

(defun e-session-aggregate-command-practical-preflight (value)
  "Reject impractical or cyclic producer VALUE before recursive detachment.

This bounded walk counts every wire-visible occurrence instead of retaining an
exact object-graph ledger.  Shared values are therefore charged once per
reference, while ACTIVE exists only to reject cycles."
  (let ((active (make-hash-table :test 'eq))
        (bytes 0)
        (nodes 0))
    (cl-labels
        ((charge-bytes
          (count)
          (cl-incf bytes count)
          (when (> bytes e-session-aggregate-command-practical-byte-limit)
            (signal 'e-session-command-too-large
                    (list "Session command exceeds practical byte limit"
                          :limit e-session-aggregate-command-practical-byte-limit
                          :observed-at-least bytes))))
         (charge-node
          ()
          (cl-incf nodes)
          (when (> nodes e-session-aggregate-command-practical-node-limit)
            (signal 'e-session-command-too-large
                    (list "Session command exceeds practical node limit"
                          :limit e-session-aggregate-command-practical-node-limit
                          :observed-at-least nodes))))
         (walk-container
          (item thunk)
          (when (gethash item active)
            (signal 'e-session-error (list "Cyclic session command")))
          (charge-node)
          (puthash item t active)
          (unwind-protect (funcall thunk) (remhash item active)))
         (walk
          (item)
          (cond
           ((stringp item)
            (when (e-session-aggregate--string-properties-p item)
              (signal 'e-session-error (list "Text properties are not durable")))
            (charge-bytes (string-bytes item)))
           ((null item) nil)
           ((symbolp item) (charge-bytes (string-bytes (symbol-name item))))
           ((numberp item)
            (charge-bytes (string-bytes (prin1-to-string item))))
           ((e-context-lifetime-generation-p item)
            (walk-container
             item
             (lambda ()
               (walk (e-context-lifetime-generation-id item))
               (walk (e-context-lifetime-generation-checkpoint item))
               (walk
                (e-context-lifetime-generation-covered-session-boundary item)))))
           ((consp item)
            (walk-container item (lambda () (walk (car item)) (walk (cdr item)))))
           ((vectorp item)
            (walk-container
             item (lambda ()
                    (dotimes (index (length item)) (walk (aref item index))))))
           ((hash-table-p item)
            (walk-container
             item (lambda ()
                    (maphash (lambda (key entry) (walk key) (walk entry)) item))))
           (t
            (signal 'e-session-error
                    (list "Unsupported durable command value" (type-of item)))))))
      (walk value))
    (list :bytes bytes :nodes nodes)))

(defun e-session-aggregate-command-freeze (value)
  "Detach VALUE while preserving every shared container and string alias."
  (let ((active (make-hash-table :test 'eq))
        (memo (make-hash-table :test 'eq))
        (missing (make-symbol "missing")))
    (cl-labels
        ((freeze
          (item)
          (if (or (stringp item) (consp item) (vectorp item)
                  (e-context-lifetime-generation-p item)
                  (hash-table-p item))
              (let ((known (gethash item memo missing)))
                (cond
                 ((not (eq known missing)) known)
                 ((gethash item active)
                  (signal 'e-session-error (list "Cyclic session command")))
                 (t
                  (puthash item t active)
                  (unwind-protect
                      (cond
                       ((stringp item)
                        (let ((copy (copy-sequence item)))
                          (puthash item copy memo) copy))
                       ((consp item)
                        (let ((copy (cons nil nil)))
                          (puthash item copy memo)
                          (setcar copy (freeze (car item)))
                          (setcdr copy (freeze (cdr item))) copy))
                       ((e-context-lifetime-generation-p item)
                        (let ((copy (e-context-lifetime-generation--create)))
                          (puthash item copy memo)
                          (setf (e-context-lifetime-generation-id copy)
                                (freeze (e-context-lifetime-generation-id item))
                                (e-context-lifetime-generation-checkpoint copy)
                                (freeze
                                 (e-context-lifetime-generation-checkpoint item))
                                (e-context-lifetime-generation-covered-session-boundary
                                 copy)
                                (freeze
                                 (e-context-lifetime-generation-covered-session-boundary
                                  item)))
                          copy))
                       ((vectorp item)
                        (let ((copy (make-vector (length item) nil)))
                          (puthash item copy memo)
                          (dotimes (index (length item))
                            (aset copy index (freeze (aref item index))))
                          copy))
                       (t
                        (let ((copy (make-hash-table
                                     :test (hash-table-test item)
                                     :size (hash-table-count item))))
                          (puthash item copy memo)
                          (maphash (lambda (key entry)
                                     (puthash (freeze key) (freeze entry) copy))
                                   item)
                          copy)))
                    (remhash item active)))))
            item)))
      (freeze value))))

(defun e-session-aggregate-command-validate (tag session-id arguments)
  "Validate the closed command TAG, SESSION-ID, and caller ARGUMENTS graph.

This pass allocates no detached command representation.  Exact graph sizing
remains with the session application service because it owns admission."
  (unless (memq tag e-session-aggregate-command-tags)
    (signal 'e-session-error (list "Unsupported session command tag" tag)))
  (unless (or (and (eq tag 'create) (null session-id))
              (and (stringp session-id) (not (string-empty-p session-id))))
    (signal 'e-session-error (list "Session command requires an id" session-id)))
  (when (and session-id (> (string-bytes session-id) 128))
    (signal 'e-session-error
            (list "Session command id exceeds 128 UTF-8 bytes" session-id)))
  (unless (e-session-aggregate-keyword-plist-shape-p arguments)
    (signal 'e-session-error (list "Session command arguments must be a plist" tag)))
  (pcase tag
    ('create
     ;; Interpretation performs legacy replay normalization after admission;
     ;; every value that survives that normalization is validated here without
     ;; constructing the normalized copy.
     (let ((metadata (plist-get arguments :metadata)))
       (condition-case err
           (e-session-metadata-validate-create-input metadata)
         (error
          (signal 'e-session-error
                  (list "Invalid create metadata" err))))))
    ('append-message
     (unless (plist-member arguments :message)
       (signal 'e-session-error (list "Append-message command requires :message")))
     (let ((message (plist-get arguments :message)))
       (dolist (field '(:id :parent-id))
         (unless (e-session-aggregate--bounded-domain-string-p
                  (plist-get message field) 128)
           (signal 'e-session-error
                   (list "Message identity exceeds 128 bytes" field))))
       (when (eq (plist-get message :role) 'tool-call)
         (let ((call (plist-get message :content)))
           (dolist (field '(:id :name))
             (when-let* ((identity (plist-get call field)))
               (unless (e-session-aggregate--bounded-domain-string-p
                        identity 128)
                 (signal 'e-session-error
                         (list "Tool call identity exceeds 128 bytes"
                               field)))))))))
    ('append-activity
     (unless (and (plist-member arguments :turn-id)
                  (symbolp (plist-get arguments :event-type))
                  (plist-member arguments :payload))
       (signal 'e-session-error (list "Invalid append-activity command")))
     (unless (e-session-aggregate--bounded-domain-string-p
              (plist-get arguments :turn-id) 128)
       (signal 'e-session-error (list "Activity turn id exceeds 128 bytes")))
     (when (memq (plist-get arguments :event-type)
                 '(tool-started tool-finished))
       (let* ((payload (plist-get arguments :payload))
              (tool-call (plist-get payload :tool-call)))
         (dolist (identity
                  (list (plist-get payload :tool-call-id)
                        (plist-get tool-call :id)
                        (plist-get tool-call :name)))
           (unless (e-session-aggregate--bounded-domain-string-p identity 128)
             (signal 'e-session-error
                     (list "Tool activity identity exceeds 128 bytes")))))))
    ('context-curation-response
     (unless (and (plist-member arguments :turn-id)
                  (e-session-aggregate--bounded-domain-string-p
                   (plist-get arguments :turn-id) 128)
                  (stringp (plist-get arguments :response-entry-id))
                  (<= (string-bytes (plist-get arguments :response-entry-id)) 128))
       (signal 'e-session-error
               (list "Invalid context-curation-response command"))))
    ('message-display
     (unless (and (stringp (plist-get arguments :message-id))
                  (<= (string-bytes (plist-get arguments :message-id)) 128)
                  (or (null (plist-get arguments :display))
                      (symbolp (plist-get arguments :display))))
       (signal 'e-session-error (list "Invalid message-display command"))))
    ('process-report
     (let ((report (plist-get arguments :report)))
       (unless (and (plist-member arguments :report)
                    (e-session-aggregate-keyword-plist-shape-p report)
                    (not (e-session-aggregate--context-record-duplicate-key-p
                          report)))
         (signal 'e-session-error (list "Invalid process-report command")))
       (dolist (field '(:id :parent-id))
         (unless (e-session-aggregate--bounded-domain-string-p
                  (plist-get report field) 128)
           (signal 'e-session-error
                   (list "Process-report identity exceeds 128 bytes" field))))))
    ('branch-summary
     (unless (and (e-session-aggregate--bounded-domain-string-p
                   (plist-get arguments :branch-id) 128)
                  (plist-member arguments :summary))
       (signal 'e-session-error (list "Invalid branch-summary command"))))
    ('compaction
     (unless (plist-member arguments :summary)
       (signal 'e-session-error (list "Compaction command requires :summary"))))
    ('provider-anchor
     (unless (e-session-aggregate--bounded-domain-identity-p
              (plist-get arguments :provider-id) 128)
       (signal 'e-session-error (list "Invalid provider-anchor command"))))
    ('context-generation
     (let ((generation (plist-get arguments :generation)))
       (unless (and (plist-member arguments :generation)
                    (or (e-context-lifetime-generation-p generation)
                        (e-session-aggregate--context-generation-record-admissible-p
                         generation)))
         (signal 'e-session-error (list "Invalid context-generation command")))))
    ('context-curation-package
     (unless (and (plist-member arguments :package)
                  (e-session-aggregate--context-curation-package-shape-p
                   (plist-get arguments :package)))
       (signal 'e-session-error (list "Invalid context-curation package"))))
    ('board-message
     (let ((message (plist-get arguments :message)))
       (unless (and (plist-member arguments :message)
                    (e-session-aggregate-keyword-plist-shape-p message)
                    (e-session-aggregate--proper-list-length-at-most-p
                     message 32)
                    (e-session-aggregate--bounded-domain-string-p
                     (plist-get message :id) 128)
                    (e-session-aggregate--proper-list-length-at-most-p
                     (plist-get message :tags) 32)
                    (or (null (plist-get message :attributes))
                        (and (e-session-aggregate-keyword-plist-shape-p
                              (plist-get message :attributes))
                             (e-session-aggregate--proper-list-length-at-most-p
                              (plist-get message :attributes) 32))))
         (signal 'e-session-error (list "Invalid board-message command")))
       (dolist (field '(:kind :mode :activity-kind :routing-state
                        :unrouted-reason :record-type :outcome
                        :failure-policy))
         (let ((value (plist-get message field)))
           (unless (or (null value)
                       (e-session-aggregate--bounded-domain-identity-p value 128))
             (signal 'e-session-error
                     (list "Board category exceeds 128 bytes" field)))))
       (dolist (tag-value (plist-get message :tags))
         (unless (e-session-aggregate--bounded-domain-identity-p tag-value 128)
           (signal 'e-session-error
                   (list "Board tag exceeds 128 bytes"))))
       (when-let* ((attributes (plist-get message :attributes))
                   (status (plist-get attributes :status)))
         (unless (e-session-aggregate--bounded-domain-identity-p status 128)
           (signal 'e-session-error
                   (list "Board status exceeds 128 bytes"))))
       (e-session-aggregate--canonical-board-record-type
        (plist-get message :record-type))))
    ('board-state
     (unless (and (stringp (plist-get arguments :principal))
                  (<= (string-bytes (plist-get arguments :principal)) 128)
                  (stringp (plist-get arguments :board-id))
                  (<= (string-bytes (plist-get arguments :board-id)) 128)
                  (member (plist-get arguments :association-role)
                          '(nil "owner" "participant"))
                  (or (null (plist-get arguments :routing-policy))
                      (e-session-board-routing-policy-valid-p
                       (plist-get arguments :routing-policy))))
       (signal 'e-session-error (list "Invalid board-state command"))))
    ((or 'clear-messages 'board-messages-clear 'delete)
     (when arguments
       (signal 'e-session-error (list "Control command takes no arguments" tag))))
    ('session-info
     (let ((field (plist-get arguments :field)))
       (unless (memq field
                     '(metadata config context-references context-reference
                       capability-state turn-options current-branch name))
         (signal 'e-session-error
                 (list "Unsupported session-info field" field)))
       (condition-case err
           (pcase field
             ('metadata
              (e-session-metadata-validate (plist-get arguments :value)))
             ('config
              (e-session-metadata-validate-class
               (plist-get arguments :value) 'session-config))
             ('context-references
              (unless (e-session-aggregate--bounded-domain-identity-p
                       (plist-get arguments :owner) 128)
                (signal 'e-session-error
                        (list "Context owner exceeds 128 bytes"))))
             ('context-reference
              (let ((key (plist-get arguments :key)))
                (unless (e-session-aggregate--bounded-domain-identity-p key 128)
                  (signal 'e-session-error
                          (list "Context reference key exceeds 128 bytes")))
                (e-session-metadata-validate-entry
                 key (plist-get arguments :value) 'current-state-reference)))
             ('capability-state
              (unless (e-session-aggregate--bounded-domain-identity-p
                       (plist-get arguments :capability-id) 128)
                (signal 'e-session-error
                        (list "Capability identity exceeds 128 bytes"))))
             ('turn-options
              (let ((tail (plist-get arguments :value))
                    seen)
                (unless (e-session-metadata-keyword-plist-p tail)
                  (signal 'e-session-error (list "Invalid turn options")))
                (while tail
                  (let ((key (pop tail))
                        (value (pop tail)))
                    (when (or (not (memq key
                                         '(:model :reasoning-effort
                                           :prompt-cache-default
                                           :prompt-cache-key
                                           :prompt-cache-retention)))
                              (memq key seen)
                              (if (eq key :prompt-cache-default)
                                  (not (memq value '(nil t)))
                                (and value (not (stringp value)))))
                      (signal 'e-session-error
                              (list "Invalid turn option" key)))
                    (push key seen)))))
             ('current-branch
              (unless (e-session-aggregate--bounded-domain-string-p
                       (plist-get arguments :value) 128)
                (signal 'e-session-error
                        (list "Current branch identity exceeds 128 bytes"))))
             ('name
              (let ((name (plist-get arguments :value)))
                (unless (and (stringp name)
                             (let ((index 0)
                                   (length (length name)))
                               (while (and (< index length)
                                           (memq (aref name index)
                                                 '(?\s ?\t ?\n ?\r)))
                                 (setq index (1+ index)))
                               (< index length)))
                  (signal 'e-session-error
                          (list "Session name must not be empty"))))))
         (error
          (signal 'e-session-error
                  (list "Invalid session-info command" field err)))))))
  t)

(defun e-session-aggregate-command-prepare (tag session-id arguments)
  "Return a frozen validated TAG command without legacy exact graph accounting.

The caller must still preflight the resulting physical operation against the
session adapter's practical record and batch limits before enqueue."
  (e-session-aggregate-command-practical-preflight arguments)
  (e-session-aggregate-command-validate tag session-id arguments)
  (let ((frozen (e-session-aggregate-command-freeze arguments)))
    (e-session-aggregate-command--create
     :tag tag
     :session-id (copy-sequence
                  (or session-id (e-session-identity-generate-id)))
     :arguments frozen
     :request-id (e-session-identity-generate-ulid)
     :delta-id (e-session-identity-generate-ulid)
     :timestamp (e-session-aggregate--timestamp))))

(defun e-session-aggregate-prepare-install-token (store command body-function)
  "Prepare COMMAND's one install token against STORE's effective aggregate.

BODY-FUNCTION receives the interpreted delta and returns the exact physical
operation body.  No live state changes before the returned token is applied."
  (let* ((delta (e-session-aggregate-command-interpret store command))
         (body (and (plist-get delta :record)
                    (funcall body-function delta))))
    (e-session-aggregate--install-token-create
     :command command :delta delta :body body)))

(defun e-session-aggregate-apply-install-token (store token)
  "Apply TOKEN once to STORE and return its frozen public facade result.

The aggregate mutation and installed marker form one quit-inhibited
transition.  `e-session-aggregate-apply-committed-record' supplies rollback
for every touched aggregate reference when application fails."
  (unless (e-session-aggregate-install-token-p token)
    (signal 'wrong-type-argument
            (list 'e-session-aggregate-install-token-p token)))
  (when (e-session-aggregate-install-token-installed token)
    (signal 'e-session-error (list "Session install token was already applied")))
  (let ((delta (e-session-aggregate-install-token-delta token))
        (command (e-session-aggregate-install-token-command token)))
    (let ((inhibit-quit t))
      (when-let ((record (plist-get delta :record)))
        (e-session-aggregate-apply-committed-record store record))
      (setf (e-session-aggregate-install-token-installed token) t))
    (let ((result (e-session-aggregate-command-result store command delta)))
      (setf (e-session-aggregate-install-token-result token) result)
      result)))

(defun e-session-aggregate-reset (store)
  "Clear all loaded semantic state in STORE before a replay pass.

Replay/application code uses this operation instead of mutating the
aggregate's hash tables and sequence fields directly.  Physical storage state
and its queues are intentionally unaffected."
  (clrhash (e-session-store-sessions store))
  (clrhash (e-session-store-entry-indexes store))
  (clrhash (e-session-store-board-journals store))
  (setf (e-session-store-sequence store) 0)
  store)

(defun e-session-aggregate-reset-session (store session-id)
  "Remove one SESSION-ID's loaded state and replay indexes from STORE."
  (remhash session-id (e-session-store-sessions store))
  (remhash session-id (e-session-store-entry-indexes store))
  (remhash session-id (e-session-store-board-journals store))
  session-id)

(defun e-session-aggregate-session-present-p (store session-id)
  "Return non-nil when STORE has a loaded or indexed SESSION-ID."
  (and (gethash session-id (e-session-store-sessions store)) t))

(defun e-session-aggregate-install-index-session (store session)
  "Install an unloaded semantic index SESSION in STORE.

The aggregate owns the session map and sequence high-water mark; callers pass a
fully detached stub produced from the catalog projection."
  (let ((session-id (plist-get session :id)))
    (unless session-id
      (signal 'e-session-error (list "Indexed session has no id" session)))
    (puthash session-id session (e-session-store-sessions store))
    (setf (e-session-store-sequence store)
          (max (e-session-store-sequence store)
               (or (plist-get session :updated-seq) 0)))
    session))

(defun e-session-aggregate-session-values (store)
  "Return the current semantic session values for composition.

The returned list is a traversal snapshot; each session remains owned by the
aggregate and must be treated as read-only by projection consumers."
  (let (sessions)
    (maphash (lambda (_session-id session) (push session sessions))
             (e-session-store-sessions store))
    (nreverse sessions)))

(defun e-session-aggregate-merge-index-session (store replacement)
  "Merge detached index metadata into an unloaded aggregate stub.

Loaded transcripts remain authoritative.  This narrow operation is used by a
catalog refresh and avoids exposing the aggregate's session table to the
application service."
  (when-let ((session (gethash (plist-get replacement :id)
                               (e-session-store-sessions store))))
    (unless (plist-get session :loaded)
      (dolist (field '(:metadata :updated-at :updated-seq :name :summary
                       :message-count :last-message-at
                       :latest-assistant-marker :board-session-state :file))
        (when (plist-member replacement field)
          (plist-put session field (plist-get replacement field)))))
    session))


(defconst e-session-aggregate--replay-list-fields
  '(:session-events :messages :activity-events :branch-summaries
    :compactions :provider-anchors :process-reports
    :context-generations :context-promotions
    :context-curation-packages)
  "Session fields accumulated in reverse order while replaying JSONL.")

(defconst e-session-aggregate--list-tail-fields
  '((:session-events . :session-events-tail)
    (:messages . :messages-tail)
    (:activity-events . :activity-events-tail)
    (:branch-summaries . :branch-summaries-tail)
    (:compactions . :compactions-tail)
    (:provider-anchors . :provider-anchors-tail)
    (:process-reports . :process-reports-tail)
    (:context-generations . :context-generations-tail)
    (:context-promotions . :context-promotions-tail)
    (:context-curation-packages . :context-curation-packages-tail))
  "Internal append-only list fields and their cached tail cells.")

(defun e-session-aggregate-keyword-plist-shape-p (value)
  "Return non-nil when aggregate VALUE has keyword plist shape.

This shape predicate is deliberately local to aggregate record validation;
metadata policy has its own contract in `e-session-metadata'."
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

(defun e-session-aggregate--plist-remove (plist key)
  "Return aggregate-owned PLIST without KEY."
  (let (result)
    (while (consp plist)
      (let ((current-key (pop plist)))
        (when (consp plist)
          (let ((value (pop plist)))
            (unless (eq current-key key)
              (push current-key result)
              (push value result))))))
    (nreverse result)))


(defun e-session-aggregate--timestamp (&optional time)
  "Return TIME as a compact UTC timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun e-session-aggregate--next-sequence (store)
  "Return STORE's next mutation sequence."
  (setf (e-session-store-sequence store)
        (1+ (e-session-store-sequence store))))

(defun e-session-aggregate--touch (store session &optional timestamp)
  "Update SESSION's modification metadata in STORE."
  (plist-put session :updated-at (or timestamp (e-session-aggregate--timestamp)))
  (plist-put session :updated-seq (e-session-aggregate--next-sequence store))
  session)


(defun e-session-aggregate--profile-enabled-p ()
  "Return non-nil when developer profiling is currently available."
  (and (fboundp 'e-dev-profile-enabled-p)
       (fboundp 'e-dev-profile-measure-thunk)
       (e-dev-profile-enabled-p)))

(defun e-session-aggregate--profile-call (event options thunk)
  "Measure THUNK as EVENT with OPTIONS when developer profiling is enabled."
  (if (e-session-aggregate--profile-enabled-p)
      (e-dev-profile-measure-thunk event options thunk)
    (funcall thunk)))

(defun e-session-aggregate--entry-index (store session-id)
  "Return STORE's entry-id index for SESSION-ID."
  (or (gethash session-id (e-session-store-entry-indexes store))
      (puthash session-id
               (make-hash-table :test 'equal)
               (e-session-store-entry-indexes store))))

(defun e-session-aggregate--clear-entry-index (store session-id)
  "Clear STORE's entry-id index for SESSION-ID."
  (remhash session-id (e-session-store-entry-indexes store)))

(defun e-session-aggregate--index-entry (store session-id entry)
  "Index durable ENTRY for SESSION-ID in STORE."
  (when-let ((entry-id (plist-get entry :id)))
    (puthash entry-id entry (e-session-aggregate--entry-index store session-id)))
  entry)

(defun e-session-aggregate--index-session-entries (store session)
  "Rebuild STORE's entry-id index for SESSION."
  (let ((session-id (plist-get session :id)))
    (when session-id
      (e-session-aggregate--clear-entry-index store session-id)
      (dolist (entry (e-session-aggregate--entries store session-id))
        (e-session-aggregate--index-entry store session-id entry)))))

(defun e-session-aggregate--list-tail (items)
  "Return the tail cell for ITEMS, or nil."
  (when items
    (last items)))

(defun e-session-aggregate--tail-field (field)
  "Return the cached tail field for append-only FIELD."
  (alist-get field e-session-aggregate--list-tail-fields))

(defun e-session-aggregate-initialize-list-state (session)
  "Destructively initialize SESSION append-only list tail fields.
This repairs or resets the internal cached tail cells from the current
canonical list values.  Callers use this after creating or replaying a session,
or after constructing an unloaded index stub."
  (dolist (pair e-session-aggregate--list-tail-fields)
    (plist-put session (cdr pair) (e-session-aggregate--list-tail
                                   (plist-get session (car pair)))))
  session)

(defun e-session-aggregate--replace-list-field (session field items)
  "Destructively replace SESSION FIELD with ITEMS and update its tail."
  (plist-put session field items)
  (when-let ((tail-field (e-session-aggregate--tail-field field)))
    (plist-put session tail-field (e-session-aggregate--list-tail items)))
  items)

(defun e-session-aggregate--append-list-item (session field item)
  "Append ITEM to SESSION FIELD in O(1) and return ITEM.
The canonical list spine belongs to the session store.  If FIELD has legacy
contents but no cached tail cell, compute and cache the tail once before
appending."
  (let* ((tail-field (e-session-aggregate--tail-field field))
         (cell (list item))
         (tail (or (and tail-field (plist-get session tail-field))
                   (when-let ((items (plist-get session field)))
                     (e-session-aggregate--list-tail items)))))
    (if tail
        (setcdr tail cell)
      (plist-put session field cell))
    (when tail-field
      (plist-put session tail-field cell))
    item))

(defun e-session-aggregate--first-user-message (messages)
  "Return first user-authored content in MESSAGES."
  (catch 'found
    (dolist (message messages)
      (when (eq (plist-get message :role) 'user)
        (let ((content (plist-get message :content)))
          (when (stringp content)
            (throw 'found content)))))))

(defun e-session-aggregate--default-title (prompt)
  "Return PROMPT formatted as a default session title."
  (if (> (length prompt) 25)
      (concat (substring prompt 0 25) "...")
    prompt))

(defun e-session-aggregate--refresh-derived-fields (store session)
  "Refresh derived display fields for SESSION in STORE."
  (let ((messages (plist-get session :messages)))
    (plist-put session :summary (e-session-aggregate--first-user-message messages))
    (plist-put session :message-count (length messages))
    (plist-put session :last-message-at (e-session-aggregate--last-message-at session))
    (plist-put session :latest-assistant-marker
               (e-session-aggregate--latest-assistant-marker session))
    (when-let ((sessions-directory (e-session-store-sessions-directory store)))
      (plist-put session :file
                 (expand-file-name
                  (concat (plist-get session :id) ".jsonl")
                  sessions-directory))))
  session)

(defun e-session-aggregate--refresh-file-field (store session)
  "Refresh persistent file metadata for SESSION in STORE."
  (when-let ((sessions-directory (e-session-store-sessions-directory store)))
    (plist-put session :file
               (expand-file-name
                (concat (plist-get session :id) ".jsonl")
                sessions-directory)))
  session)

(defun e-session-aggregate--message-summary (message)
  "Return MESSAGE content when it should become a session summary."
  (when (eq (plist-get message :role) 'user)
    (let ((content (plist-get message :content)))
      (when (stringp content)
        content))))

(defun e-session-aggregate--update-message-derived-fields-on-append
    (store session message)
  "Update SESSION derived fields incrementally for appended MESSAGE."
  (let ((count (plist-get session :message-count)))
    (plist-put session
               :message-count
               (if (integerp count)
                   (1+ count)
                 (length (plist-get session :messages)))))
  (unless (plist-get session :summary)
    (when-let ((summary (e-session-aggregate--message-summary message)))
      (plist-put session :summary summary)))
  (plist-put session :last-message-at (plist-get message :created-at))
  (when (eq (plist-get message :role) 'assistant)
    (plist-put session :latest-assistant-marker
               (e-session-aggregate--message-assistant-marker message)))
  (e-session-aggregate--refresh-file-field store session))

(defun e-session-aggregate--clear-message-derived-fields (store session)
  "Reset message-derived fields for cleared SESSION."
  (plist-put session :message-count 0)
  (plist-put session :summary nil)
  (plist-put session :last-message-at nil)
  (plist-put session :latest-assistant-marker nil)
  (e-session-aggregate--refresh-file-field store session))

(defun e-session-aggregate--display-title-for-session (session)
  "Return a display title for SESSION."
  (or (plist-get session :name)
      (when-let ((summary (plist-get session :summary)))
        (e-session-aggregate--default-title summary))
      (when-let ((created-at (plist-get session :created-at)))
        (format "Untitled %s" created-at))
      (format "Untitled %s" (plist-get session :id))))

(defun e-session-aggregate--prepend-replayed-item (session field item)
  "Prepend replayed ITEM to SESSION FIELD."
  (plist-put session field (cons item (plist-get session field))))

(defun e-session-aggregate--next-entry-ordinal (session)
  "Return SESSION's next replay entry ordinal."
  (let ((ordinal (1+ (or (plist-get session :entry-count) 0))))
    (plist-put session :entry-count ordinal)
    ordinal))

(defun e-session-aggregate--entry-id-from-record (record entry)
  "Return durable id from RECORD or ENTRY."
  (or (plist-get entry :id)
      (plist-get record :id)))

(defun e-session-aggregate--entry-parent-id-from-record (record entry)
  "Return parent id from RECORD or ENTRY."
  (if (plist-member entry :parent-id)
      (plist-get entry :parent-id)
    (plist-get record :parent-id)))

(defun e-session-aggregate--entry-with-identity (session type entry timestamp &optional record)
  "Return ENTRY with durable identity fields for SESSION and TYPE.
TIMESTAMP is used for creation metadata and legacy deterministic backfill.
When RECORD is non-nil, identity fields may be replayed from the JSONL record."
  (let ((entry (copy-sequence entry)))
    ;; TYPE is the replay dispatch contract.  JSON turns a nested symbol into
    ;; a string, which must never leak into the symbol-based in-memory model.
    (plist-put entry :type type)
    (unless (plist-get entry :id)
      (plist-put
       entry :id
       (or (e-session-aggregate--entry-id-from-record record entry)
           (if record
               (e-session-identity-legacy-entry-id
                session type (e-session-aggregate--next-entry-ordinal session) timestamp)
             (e-session-identity-generate-ulid)))))
    (unless (plist-member entry :parent-id)
      (when-let ((parent-id
                  (or (e-session-aggregate--entry-parent-id-from-record record entry)
                      (plist-get session :current-head-id))))
        (plist-put entry :parent-id parent-id)))
    (unless (plist-member entry :created-at)
      (plist-put entry :created-at timestamp))
    ;; Replay is an explicit durability proof supplied by the journal or
    ;; checkpoint loader.  Live appends receive their lifecycle state from
    ;; the persistence owner instead of being inferred from entry presence.
    (when record
      (plist-put entry :durability-state 'replayed-durable))
    entry))

(defun e-session-aggregate--normalize-entry-from-record
    (session type entry timestamp &optional record)
  "Return normalized durable ENTRY for replay or append."
  (e-session-aggregate--advance-head
   session
   (e-session-aggregate--entry-with-identity session type entry timestamp record)))

(defun e-session-aggregate--advance-head (session entry)
  "Advance SESSION current head to ENTRY."
  (plist-put session :current-head-id (plist-get entry :id))
  entry)

(defun e-session-aggregate--root-event-id (session)
  "Return SESSION root event id, when available."
  (or (plist-get session :root-event-id)
      (plist-get (car (plist-get session :session-events)) :id)))

(defun e-session-aggregate--session-event
    (session event-type timestamp &optional fields record)
  "Return a normalized session EVENT-TYPE entry for SESSION.
TIMESTAMP is used as creation metadata.  FIELDS are copied onto the event,
and RECORD supplies persisted identity fields during replay."
  (let ((entry (append (list :event-type event-type
                             :created-at timestamp)
                       (copy-sequence fields))))
    (e-session-aggregate--normalize-entry-from-record
     session 'session-event entry timestamp record)))

(defun e-session-aggregate--append-session-event
    (session event-type timestamp &optional fields record)
  "Append a normalized session EVENT-TYPE entry to SESSION."
  (let ((event (e-session-aggregate--session-event
                session event-type timestamp fields record)))
    (e-session-aggregate--append-list-item session :session-events event)
    (when (eq event-type 'session-created)
      (plist-put session :root-event-id (plist-get event :id)))
    event))

(defun e-session-aggregate--prepend-replayed-session-event
    (session event-type timestamp &optional fields record)
  "Prepend a replayed session EVENT-TYPE entry to SESSION."
  (let ((event (e-session-aggregate--session-event
                session event-type timestamp fields record)))
    (e-session-aggregate--prepend-replayed-item session :session-events event)
    (when (eq event-type 'session-created)
      (plist-put session :root-event-id (plist-get event :id)))
    event))

(defun e-session-aggregate--entries (store session-id)
  "Return all durable entries for SESSION-ID in insertion order."
  (let ((session (e-session-aggregate-get-live store session-id)))
    (append (plist-get session :session-events)
            (plist-get session :messages)
            (plist-get session :activity-events)
            (plist-get session :branch-summaries)
            (plist-get session :compactions)
            (plist-get session :provider-anchors)
            (plist-get session :process-reports)
            (plist-get session :context-generations)
            (plist-get session :context-promotions)
            (plist-get session :context-curation-packages))))

(defun e-session-aggregate-entry-by-id (store session-id entry-id)
  "Return durable entry ENTRY-ID from SESSION-ID."
  (or (gethash entry-id (e-session-aggregate--entry-index store session-id))
      (seq-find (lambda (entry)
                  (equal (plist-get entry :id) entry-id))
                (e-session-aggregate--entries store session-id))))

(defun e-session-aggregate--entry-children (store session-id parent-id)
  "Return entries whose parent is PARENT-ID in SESSION-ID."
  (seq-filter (lambda (entry)
                (equal (plist-get entry :parent-id) parent-id))
              (e-session-aggregate--entries store session-id)))

(defun e-session-aggregate-current-path (store session-id &optional head-id)
  "Return SESSION-ID current parent path ending at HEAD-ID or current head."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (head-id (or head-id (plist-get session :current-head-id)))
         path)
    (while head-id
      (let ((entry (e-session-aggregate-entry-by-id store session-id head-id)))
        (unless entry
          (setq head-id nil))
        (when entry
          (push entry path)
          (setq head-id (plist-get entry :parent-id)))))
    path))

(defun e-session-aggregate-entries-in-turn (store session-id turn-id)
  "Return entries in SESSION-ID that belong to TURN-ID."
  (seq-filter (lambda (entry)
                (equal (plist-get entry :turn-id) turn-id))
              (e-session-aggregate-current-path store session-id)))

(defun e-session-aggregate-entry-previous (store session-id entry-id)
  "Return the previous entry before ENTRY-ID on SESSION-ID current path."
  (when-let ((entry (e-session-aggregate-entry-by-id store session-id entry-id)))
    (when-let ((parent-id (plist-get entry :parent-id)))
      (e-session-aggregate-entry-by-id store session-id parent-id))))

(defun e-session-aggregate-entry-next (store session-id entry-id)
  "Return the next entry after ENTRY-ID on SESSION-ID current path."
  (let ((path (e-session-aggregate-current-path store session-id)))
    (cadr (member (e-session-aggregate-entry-by-id store session-id entry-id) path))))

(defun e-session-aggregate-latest-entry-of-type (store session-id type)
  "Return latest entry of TYPE on SESSION-ID current path."
  (seq-find (lambda (entry)
              (eq (plist-get entry :type) type))
            (reverse (e-session-aggregate-current-path store session-id))))

(defun e-session-aggregate-entries-from (store session-id first-entry-id)
  "Return current-path entries from FIRST-ENTRY-ID to the current head."
  (let ((path (e-session-aggregate-current-path store session-id)))
    (member (e-session-aggregate-entry-by-id store session-id first-entry-id) path)))

(defun e-session-aggregate-entries-before (store session-id entry-id)
  "Return current-path entries before ENTRY-ID."
  (let ((entries nil)
        (done nil))
    (dolist (entry (e-session-aggregate-current-path store session-id))
      (unless done
        (if (equal (plist-get entry :id) entry-id)
            (setq done t)
          (push entry entries))))
    (nreverse entries)))

(defun e-session-aggregate-compaction-boundary-valid-p (store session-id compaction)
  "Return non-nil when COMPACTION points at an entry on the current path."
  (let ((boundary (plist-get compaction :first-kept-entry-id)))
    (and (stringp boundary)
         (e-session-aggregate-entry-by-id store session-id boundary)
         (seq-some (lambda (entry)
                     (equal (plist-get entry :id) boundary))
                   (e-session-aggregate-current-path store session-id)))))

(defun e-session-aggregate-latest-valid-compaction (store session-id)
  "Return the latest compaction record with a valid current-path boundary."
  (seq-find
   (lambda (entry)
     (and (eq (plist-get entry :type) 'compaction)
          (e-session-aggregate-compaction-boundary-valid-p store session-id entry)))
   (reverse (e-session-aggregate-compactions store session-id))))


(defun e-session-aggregate-finalize-replayed-session (store session)
  "Restore replayed SESSION field ordering and derived metadata."
  (dolist (field e-session-aggregate--replay-list-fields)
    (plist-put session field (nreverse (plist-get session field))))
  (let ((journal (e-session-aggregate--board-journal store (plist-get session :id))))
    ;; Board replay appends through the journal tail, so its physical order is
    ;; already forward (unlike the prepend-based aggregate lists above).
    (setf (e-session-board-journal-tail journal)
          (e-session-aggregate--list-tail (e-session-board-journal-messages journal))))
  (e-session-aggregate-initialize-list-state session)
  (cl-remf session :entry-count)
  (plist-put session :loaded t)
  (e-session-aggregate--refresh-derived-fields store session)
  (e-session-aggregate--index-session-entries store session)
  session)

(defun e-session-aggregate--last-message-at (session)
  "Return SESSION's latest message timestamp, when it has messages."
  (when-let ((message (car (last (plist-get session :messages)))))
    (plist-get message :created-at)))

(defun e-session-aggregate--message-assistant-marker (message)
  "Return MESSAGE's stable assistant read marker."
  (or (plist-get message :id)
      (plist-get message :created-at)))

(defun e-session-aggregate--latest-assistant-marker (session)
  "Return SESSION's latest assistant message marker."
  (let (marker)
    (dolist (message (reverse (plist-get session :messages)))
      (when (and (not marker)
                 (eq (plist-get message :role) 'assistant))
        (setq marker (e-session-aggregate--message-assistant-marker message))))
    marker))

(defconst e-session-aggregate--invalid-board-association
  '(:invalid-board-association t)
  "Bounded internal marker for a present malformed board association.")

(defun e-session-aggregate--board-association-keys-valid-p (association)
  "Return non-nil when ASSOCIATION contains only its bounded unique keys."
  (let ((tail association)
        seen
        (valid t))
    (while (and valid tail)
      (let ((key (pop tail)))
        (setq valid (and (memq key '(:board-id :principal :association-role
                                     :routing-policy))
                         (not (memq key seen))))
        (push key seen)
        (pop tail)))
    valid))

(defun e-session-aggregate--valid-board-association-p (association)
  "Return non-nil when ASSOCIATION has the complete durable board shape."
  (and (e-session-aggregate-keyword-plist-shape-p association)
       (e-session-aggregate--board-association-keys-valid-p association)
       (stringp (plist-get association :board-id))
       (stringp (plist-get association :principal))
       (or (not (plist-member association :association-role))
           (member (plist-get association :association-role)
                   '("owner" "participant")))
       (or (not (plist-member association :routing-policy))
           (e-session-board-routing-policy-valid-p
            (plist-get association :routing-policy)))))

(defun e-session-aggregate--normalize-board-association (association)
  "Return a bounded normalized representation of present ASSOCIATION."
  (if (e-session-aggregate--valid-board-association-p association)
      (let ((normalized (copy-tree association)))
        (when (plist-member normalized :routing-policy)
          (plist-put normalized :routing-policy
                     (e-session-board-routing-policy-normalize
                      (plist-get normalized :routing-policy))))
        normalized)
    (copy-tree e-session-aggregate--invalid-board-association)))

(defun e-session-aggregate-board-routing-policy (session)
  "Return SESSION's detached complete routing policy, or nil when absent."
  (when-let ((association (e-session-aggregate-board-association session)))
    (unless (e-session-aggregate-board-association-invalid-p association)
      (when (plist-member association :routing-policy)
        (e-session-board-routing-policy-copy-value
         (plist-get association :routing-policy))))))

(defun e-session-aggregate-board-association-policy-present-p (association)
  "Return non-nil when ASSOCIATION explicitly carries a routing policy."
  (and (not (e-session-aggregate-board-association-invalid-p association))
       (plist-member association :routing-policy)))

(defun e-session-aggregate-projected-board-association (projection)
  "Return normalized board association from persisted PROJECTION.
The nested representation is authoritative when its key is present.  Flat
identity mirrors reconstruct only the canonical legacy shape in its absence."
  (if (plist-member projection :board-state)
      (let ((state (plist-get projection :board-state))
            (board-id (plist-get projection :board-id))
            (principal (plist-get projection :principal))
            (json-null-p (e-session-codec-json-null-p
                          (plist-get projection :board-state))))
        ;; Historical indexes projected all three keys as JSON null for an
        ;; ordinary non-board session.  Preserve only that exact absence shape;
        ;; omitted or non-null flat mirrors make a null nested value malformed.
        (if (and (plist-member projection :board-id)
                 (plist-member projection :principal)
                 json-null-p
                 (or (null board-id)
                     (e-session-codec-json-null-p board-id))
                 (or (null principal)
                     (e-session-codec-json-null-p principal)))
            nil
          (e-session-aggregate--normalize-board-association
           (if json-null-p nil state))))
    (let ((board-id (plist-get projection :board-id))
          (principal (plist-get projection :principal)))
      (if (and (null board-id) (null principal))
          nil
        (e-session-aggregate--normalize-board-association
         (list :board-id board-id :principal principal))))))

(defun e-session-aggregate-board-association (session)
  "Return SESSION's normalized whole board association, or nil when absent."
  (cond
   ((plist-member session :board-session-state)
    (e-session-aggregate--normalize-board-association
     (plist-get session :board-session-state)))
   ((plist-member session :board-state)
    (e-session-aggregate--normalize-board-association
     (plist-get session :board-state)))
   (t nil)))

(defun e-session-aggregate-board-association-invalid-p (association)
  "Return non-nil when ASSOCIATION is the bounded malformed-state marker."
  (equal association e-session-aggregate--invalid-board-association))

(defun e-session-aggregate--session-index-entry (store session)
  "Return public index metadata for SESSION in STORE."
  (e-session-aggregate--refresh-file-field store session)
  (let* ((state (e-session-aggregate-board-association session))
         (entry
          (list :id (plist-get session :id)
                :name (plist-get session :name)
                :summary (plist-get session :summary)
                :metadata (plist-get session :metadata)
                :title (e-session-aggregate--display-title-for-session session)
                :message-count (or (plist-get session :message-count) 0)
                :created-at (plist-get session :created-at)
                :updated-at (plist-get session :updated-at)
                :updated-seq (plist-get session :updated-seq)
                :last-message-at (or (plist-get session :last-message-at)
                                     (e-session-aggregate--last-message-at session))
                :latest-assistant-marker
                (or (plist-get session :latest-assistant-marker)
                    (e-session-aggregate--latest-assistant-marker session))
                :board-id (plist-get state :board-id)
                :principal (plist-get state :principal)
                :file (plist-get session :file)
                :loaded (plist-get session :loaded))))
    (when (plist-member session :board-session-state)
      (setq entry (plist-put entry :board-state state)))
    entry))

(defun e-session-aggregate--normalize-turn-options (options)
  "Return canonical session turn OPTIONS."
  (let (normalized)
    (when-let ((model (plist-get options :model)))
      (when (and (stringp model) (not (string-empty-p (string-trim model))))
        (setq normalized
              (plist-put normalized :model (string-trim model)))))
    (when-let ((effort (plist-get options :reasoning-effort)))
      (when (and (stringp effort) (not (string-empty-p (string-trim effort))))
        (setq normalized
              (plist-put normalized :reasoning-effort (string-trim effort)))))
    (when (and (plist-member options :prompt-cache-default)
               (memq (plist-get options :prompt-cache-default) '(nil t)))
      (setq normalized
            (plist-put normalized
                       :prompt-cache-default
                       (plist-get options :prompt-cache-default))))
    (when-let ((cache-key (plist-get options :prompt-cache-key)))
      (when (and (stringp cache-key)
                 (not (string-empty-p (string-trim cache-key))))
        (setq normalized
              (plist-put normalized
                         :prompt-cache-key
                         (string-trim cache-key)))))
    (when-let ((retention (plist-get options :prompt-cache-retention)))
      (when (and (stringp retention)
                 (not (string-empty-p (string-trim retention))))
        (setq normalized
              (plist-put normalized
                         :prompt-cache-retention
                         (string-trim retention)))))
    normalized))

(defun e-session-aggregate--message-with-created-at (message timestamp)
  "Return semantic MESSAGE with TIMESTAMP when no creation time is present.
Wire-level role/display normalization belongs to `e-session-codec'; this
aggregate helper only supplies the domain's required timestamp default."
  (let ((normalized (copy-sequence message)))
    (unless (plist-member normalized :created-at)
      (plist-put normalized :created-at timestamp))
    normalized))

(defun e-session-aggregate-peek-session (store session-id)
  "Return SESSION-ID metadata without forcing transcript replay."
  (or (gethash session-id (e-session-store-sessions store))
      (signal 'e-session-missing (list session-id))))

(cl-defun e-session-aggregate-create (store &key id metadata defer-persistence)
  "Create an in-memory session aggregate in STORE with ID and METADATA.
The optional DEFER-PERSISTENCE flag marks a private admission reservation for
the application service.  This owner never publishes durable records; the
composition root supplies the storage adapter after the semantic mutation has
been accepted."
  (setq id (or id (e-session-identity-generate-id)))
  (when (gethash id (e-session-store-sessions store))
    (signal 'e-session-duplicate (list id)))
  (setq metadata (e-session-metadata-validate
                  (e-session-metadata-normalize-for-replay metadata)))
  (let* ((timestamp (e-session-aggregate--timestamp))
         (session (list :id id
                        :metadata metadata
                        :session-events nil
                        :messages nil
                        :board-output-sequence 0
                        :board-activity-sequence 0
                        :activity-events nil
                        :branch-summaries nil
                        :current-branch nil
                        :compactions nil
                        :provider-anchors nil
                        :process-reports nil
                        :context-generations nil
                        :context-promotions nil
                        :context-curation-packages nil
                        :turn-options nil
                        :created-at timestamp
                        :updated-at timestamp
                        :name (plist-get metadata :name)
                        :loaded t)))
    (e-session-aggregate-initialize-list-state session)
    (let ((root (e-session-aggregate--append-session-event
                 session
                 'session-created
                 timestamp
                 (list :metadata metadata))))
      (plist-put session :root-event-id (plist-get root :id)))
    (when defer-persistence
      (plist-put session :admission-pending t))
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    (puthash id session (e-session-store-sessions store))
    (e-session-aggregate--index-session-entries store session)
    session))

(cl-defun e-session-aggregate-create-board-admission
    (store &key id metadata principal board-id association-role routing-policy)
  "Reserve one board participant session before durable publication.
Validate the complete board association and all durable records before placing
the private reservation in STORE.  The owner must call
`e-session-aggregate-commit-board-admission' after runtime attachment
succeeds, or `e-session-aggregate-abort-created' on failure.  No journal,
queue, controller outbox,
or index entry is published by this function."
  (let ((session
         (e-session-aggregate-create store :id id :metadata metadata
                           :defer-persistence t)))
    (condition-case error
        (progn
          (unless (and (stringp board-id) (not (string-empty-p board-id))
                       (stringp principal) (not (string-empty-p principal)))
            (signal 'e-session-error
                    (list "Invalid board admission identity"
                          board-id principal)))
          (when (and association-role
                     (not (member association-role '("owner" "participant"
                                                     owner participant))))
            (signal 'e-session-error
                    (list "Invalid board association role" association-role)))
          (when (and routing-policy
                     (not (e-session-board-routing-policy-valid-p
                           routing-policy)))
            (signal 'e-session-board-routing-invalid
                    (list "Invalid board routing policy" routing-policy)))
          (let ((board-state (list :board-id (copy-sequence board-id)
                                   :principal (copy-sequence principal))))
            (when association-role
              (plist-put board-state :association-role
                         (if (symbolp association-role)
                             (symbol-name association-role)
                           association-role)))
            (when routing-policy
              (plist-put
               board-state :routing-policy
               (e-session-board-routing-policy-normalize routing-policy)))
            (plist-put session :board-session-state board-state)
            (let* ((session-id (plist-get session :id))
                   (state-record
                    (list :type "board-session-state"
                          :session-id session-id
                          :board-state board-state
                          :board-id board-id
                          :principal principal
                          :board-output-sequence
                          (or (plist-get session :board-output-sequence) 0)
                          :board-activity-sequence
                          (or (plist-get session :board-activity-sequence) 0)))
                   (records
                    (list
                     (list :type "session"
                           :session-id session-id
                           :id (e-session-aggregate--root-event-id session)
                           :timestamp (plist-get session :created-at)
                           :created-at (plist-get session :created-at)
                           :updated-at (plist-get session :updated-at)
                           :metadata (plist-get session :metadata))
                     state-record)))
              (plist-put session :admission-records records)
              (e-session-aggregate--index-session-entries store session)
              session)))
      (error
       (ignore-errors
         (e-session-aggregate-abort-created store (plist-get session :id)))
       (signal (car error) (cdr error))))))


(defun e-session-aggregate-commit-board-admission (store session-id)
  "Complete one previously reserved board admission in the aggregate.
The returned session retains its detached semantic admission records for the
application service to submit as one storage transaction.  No physical write
or queue mutation occurs here."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (records (and (plist-get session :admission-pending)
                       (plist-get session :admission-records))))
    (unless (and (plist-get session :admission-pending)
                 (listp records) (= (length records) 2))
      (signal 'e-session-error
              (list "Session has no pending board admission" session-id)))
    (cl-remf session :admission-pending)
    (cl-remf session :admission-records)
    (e-session-aggregate--index-session-entries store session)
    session))

(defun e-session-aggregate-abort-created (store session-id)
  "Remove a newly created SESSION-ID after an owning service failure.

This is intentionally limited to application-service rollback: callers must
  only use it for a session that has just been created and has not been exposed
as a restorable participant.  It removes the in-memory/index/journal state and
any queued direct-store writes, rather than appending a user-visible tombstone
  for an object that never completed admission."
  (when-let* ((session (gethash session-id (e-session-store-sessions store)))
              (_ (plist-get session :loaded)))
    (remhash session-id (e-session-store-entry-indexes store))
    (remhash session-id (e-session-store-board-journals store))
    (remhash session-id (e-session-store-sessions store))
    t))

(defun e-session-aggregate--board-journal (store session-id)
  "Return STORE's private board journal for SESSION-ID."
  (or (gethash session-id (e-session-store-board-journals store))
      (puthash session-id
               (e-session-aggregate--board-journal-create)
               (e-session-store-board-journals store))))

(defun e-session-aggregate--clear-board-journal (store session-id)
  "Remove STORE's private board journal for SESSION-ID."
  (remhash session-id (e-session-store-board-journals store)))

(defun e-session-aggregate--detach-value (value cycle-error)
  "Return aggregate VALUE detached from mutable input.
Signal CYCLE-ERROR for cyclic conses, vectors, and hash tables.  The copy is
deliberately iterative so a large valid durable value does not exhaust the
evaluator while crossing an aggregate ownership boundary."
  (let ((pending (list (list :value value)))
        (results nil)
        (visiting (make-hash-table :test 'eq)))
    (while pending
      (let ((task (pop pending)))
        (pcase (car task)
          (:leave
           (remhash (cadr task) visiting))
          (:assemble-cons
           (let ((cdr-value (pop results))
                 (car-value (pop results)))
             (push (cons car-value cdr-value) results)))
          (:assemble-vector
           (let (items)
             (dotimes (_ (cadr task))
               (push (pop results) items))
             (push (vconcat items) results)))
          (:assemble-hash
           (let ((copy (make-hash-table :test (cadr task)
                                        :size (caddr task))))
             (dotimes (_ (cadddr task))
               (let ((item (pop results))
                     (key (pop results)))
                 (puthash key item copy)))
             (push copy results)))
          (:value
           (let ((current (cadr task)))
             (cond
              ((stringp current)
               (push (copy-sequence current) results))
              ((or (null current) (eq current t) (numberp current)
                   (symbolp current))
               (push current results))
              ((consp current)
               (when (gethash current visiting)
                 (signal cycle-error (list 'cons)))
               (puthash current t visiting)
               (push (list :leave current) pending)
               (push (list :assemble-cons) pending)
               (push (list :value (cdr current)) pending)
               (push (list :value (car current)) pending))
              ((vectorp current)
               (when (gethash current visiting)
                 (signal cycle-error (list 'vector)))
               (puthash current t visiting)
               (push (list :leave current) pending)
               (push (list :assemble-vector (length current)) pending)
               (let ((index (1- (length current))))
                 (while (>= index 0)
                   (push (list :value (aref current index)) pending)
                   (setq index (1- index)))))
              ((hash-table-p current)
               (when (gethash current visiting)
                 (signal cycle-error (list 'hash-table)))
               (puthash current t visiting)
               (let (pairs)
                 (maphash (lambda (key item)
                            (push (cons key item) pairs))
                          current)
                 (push (list :leave current) pending)
                 (push (list :assemble-hash (hash-table-test current)
                             (hash-table-size current) (length pairs))
                       pending)
                 (dolist (pair pairs)
                   (push (list :value (cdr pair)) pending)
                   (push (list :value (car pair)) pending))))
              (t
               (push current results))))))))
    (car results)))

(defun e-session-aggregate--freeze-board-value (value)
  "Return VALUE detached from mutable board-journal input.
Signal `e-session-board-message-cycle' when VALUE is cyclic."
  (e-session-aggregate--detach-value value 'e-session-board-message-cycle))

(defun e-session-aggregate-stage-session-mutation (store &optional session-id)
  "Return an isolated aggregate stage derived from STORE.

When SESSION-ID is non-nil, copy that session and its private board journal
into the stage.  Other sessions are deliberately absent.  The application
service may perform one semantic mutation on the stage while STORE remains the
committed live aggregate.  A nil SESSION-ID creates an empty stage for a new
session.  This boundary owns no persistence or event-loop work."
  (let ((stage (e-session-store-create
                :directory (e-session-store-directory store)
                :sessions-directory (e-session-store-sessions-directory store)
                :index-file (e-session-store-index-file store)
                :persistent (e-session-store-persistent store)
                :write-mode (e-session-store-write-mode store)
                :sequence (e-session-store-sequence store))))
    (when session-id
      (let* ((source (e-session-aggregate-get-live store session-id))
             (session (e-session-aggregate--detach-value source
                                                         'e-session-error)))
        ;; Cached tail cells deliberately alias the canonical list spines.
        ;; Rebuild those aliases after detaching the semantic value.
        (e-session-aggregate-initialize-list-state session)
        (puthash session-id session (e-session-store-sessions stage))
        (e-session-aggregate--index-session-entries stage session))
      (let ((source-journal
             (gethash session-id
                      (e-session-store-board-journals store))))
        (when source-journal
        (let* ((messages
                (e-session-aggregate--freeze-board-value
                 (e-session-board-journal-messages source-journal)))
               (journal
                (e-session-aggregate--board-journal-create
                 :messages messages :tail (and messages (last messages)))))
          (dolist (message messages)
            (puthash (e-session-aggregate-board-message-identity message)
                     message (e-session-board-journal-id-index journal)))
          (puthash session-id journal
                   (e-session-store-board-journals stage))))))
    stage))

(defun e-session-aggregate-publish-staged-session
    (store stage session-id)
  "Publish SESSION-ID from isolated aggregate STAGE into live STORE.

The caller must establish the durable ordering boundary before invoking this
operation.  Publication transfers the staged aggregate-owned representation,
rebuilds its live indexes, and assigns a fresh live projection sequence so a
different session committed reentrantly cannot create a sequence collision."
  (let ((session (gethash session-id (e-session-store-sessions stage))))
    (unless session
      (signal 'e-session-missing (list session-id)))
    (setf (e-session-store-sequence store)
          (1+ (e-session-store-sequence store)))
    (plist-put session :updated-seq (e-session-store-sequence store))
    (puthash session-id session (e-session-store-sessions store))
    (e-session-aggregate--index-session-entries store session)
    (let ((journal
           (gethash session-id (e-session-store-board-journals stage))))
      (if journal
          (puthash session-id journal (e-session-store-board-journals store))
        (remhash session-id (e-session-store-board-journals store))))
    session))

(defun e-session-aggregate-board-messages (store session-id)
  "Return SESSION-ID's durable board envelopes in board order."
  (e-session-aggregate-get-live store session-id)
  (e-session-aggregate--freeze-board-value
   (e-session-board-journal-messages
    (e-session-aggregate--board-journal store session-id))))

(defun e-session-aggregate--canonical-board-record-type (record-type)
  "Return supported RECORD-TYPE in the board journal's representation.
Nil means an ordinary board message."
  (pcase record-type
    (`nil nil)
    ((or 'processing-chain "processing-chain") 'processing-chain)
    ((or 'processing-result "processing-result") 'processing-result)
    (_
     (signal 'e-session-board-message-invalid-record-type
             (list record-type)))))

(defun e-session-aggregate--normalize-board-message (message)
  "Normalize durable board MESSAGE after input or JSONL replay."
  (dolist (field '(:kind :mode :activity-kind :routing-state
                   :unrouted-reason :record-type :outcome :failure-policy))
    (when-let ((value (plist-get message field)))
      (when (stringp value)
        (plist-put message field (intern value)))))
  ;; Keep the historical detached representation: callers can rely on the
  ;; tags slot being present even when the envelope carried no tags.
  (plist-put message :tags
             (mapcar (lambda (tag)
                       (if (stringp tag) (intern tag) tag))
                     (plist-get message :tags)))
  (when-let ((attributes (plist-get message :attributes)))
    (when-let ((status (plist-get attributes :status)))
      (when (stringp status)
        (plist-put attributes :status (intern status)))))
  message)

(defun e-session-aggregate--normalize-owned-board-message (message)
  "Normalize sealed MESSAGE with only schema-bounded spine wrappers.

Large content and nested producer leaves remain shared with the command by
identity.  The copied top-level, tags, and attributes spines are bounded by
the async admission schema and belong to D."
  (let ((normalized (copy-sequence message)))
    (when-let* ((attributes (plist-get normalized :attributes)))
      (plist-put normalized :attributes (copy-sequence attributes)))
    (e-session-aggregate--normalize-board-message normalized)))

(defun e-session-aggregate--copy-owned-board-state (state)
  "Copy bounded STATE spines while retaining its frozen producer leaves."
  (when state
    (let ((copy (copy-sequence state)))
      (when (plist-member copy :routing-policy)
        (plist-put copy :routing-policy
                   (e-session-board-routing-policy-normalize-owned
                    (plist-get copy :routing-policy))))
      copy)))

(defun e-session-aggregate-board-message-identity (message)
  "Return the durable journal identity for board MESSAGE.
Processing records have a record type, while ordinary board messages occupy the
untyped board-message namespace.  The pair prevents equal raw ids from
silently replacing records from another namespace."
  (cons (or (e-session-aggregate--canonical-board-record-type
             (plist-get message :record-type))
            'board-message)
        (plist-get message :id)))

(defun e-session-aggregate--existing-board-message (journal message)
  "Return MESSAGE's retained duplicate, or signal for a typed conflict."
  (let* ((identity (e-session-aggregate-board-message-identity message))
         (existing (and journal
                        (gethash identity
                                 (e-session-board-journal-id-index journal)))))
    (when (and existing
               (plist-get message :record-type)
               (not (equal existing message)))
      (signal 'e-session-board-message-conflict
              (list identity existing message)))
    existing))

(defun e-session-aggregate-append-board-message (store session-id message)
  "Append one immutable board MESSAGE envelope to SESSION-ID's board log."
  (e-session-aggregate-get-live store session-id)
  (let* ((journal (e-session-aggregate--board-journal store session-id))
         (message (e-session-aggregate--freeze-board-value message))
         (record-type
          (e-session-aggregate--canonical-board-record-type
           (plist-get message :record-type)))
         (_ (when record-type
              (plist-put message :record-type record-type)))
         (existing (e-session-aggregate--existing-board-message journal message)))
    (unless existing
      (puthash (e-session-aggregate-board-message-identity message) message
               (e-session-board-journal-id-index journal))
      (let ((cell (list message)))
        (if-let ((tail (e-session-board-journal-tail journal)))
            (setcdr tail cell)
          (setf (e-session-board-journal-messages journal) cell))
        (setf (e-session-board-journal-tail journal) cell))
      (let ((session (e-session-aggregate-get-live store session-id)))
        (e-session-aggregate--touch store session (e-session-aggregate--timestamp)))
      ;; The application service persists this detached envelope.
      nil)
    (e-session-aggregate--freeze-board-value (or existing message))))

(defun e-session-aggregate-clear-board-messages (store session-id)
  "Clear SESSION-ID's durable board log and derived identity index."
  (let ((journal (e-session-aggregate--board-journal store session-id))
        (session (e-session-aggregate-get-live store session-id)))
    (setf (e-session-board-journal-messages journal) nil
          (e-session-board-journal-tail journal) nil
          (e-session-board-journal-id-index journal) (make-hash-table :test 'equal))
    (e-session-aggregate--touch store session (e-session-aggregate--timestamp))
    nil))

(defun e-session-aggregate-declare-board-state
    (store session-id principal board-id &optional association-role
           routing-policy)
  "Persist SESSION-ID's board identity, role, and ROUTING-POLICY.
ASSOCIATION-ROLE is either `owner' or `participant'.  Nil omits the role for
replay-compatible callers that create the legacy board identity shape.
ROUTING-POLICY, when non-nil, must contain every key in
`e-session-aggregate--board-routing-policy-keys'; a present policy is never
partially
persisted."
  (unless (member association-role '(nil "owner" "participant"))
    (error "Invalid board association role: %S" association-role))
  (unless (and (stringp session-id) (stringp principal) (stringp board-id))
    (error "Board association identity must be strings: %S %S %S"
           session-id principal board-id))
  (when (and routing-policy
             (not (e-session-board-routing-policy-valid-p routing-policy)))
    (error "Invalid board routing policy: %S" routing-policy))
  (let* ((session (e-session-aggregate-get-live store session-id))
         (had-state (plist-member session :board-session-state))
         (old-state (plist-get session :board-session-state))
         (board-state (list :board-id (copy-sequence board-id)
                            :principal (copy-sequence principal))))
    (when association-role
      (setq board-state
            (plist-put board-state :association-role association-role)))
    (when routing-policy
      (setq board-state
            (plist-put
             board-state :routing-policy
                     (e-session-board-routing-policy-normalize routing-policy))))
    (let ((record
           (list :type "board-session-state" :session-id session-id
                 :board-state board-state :board-id board-id
                 :principal principal
                 :board-output-sequence
                 (or (plist-get session :board-output-sequence) 0)
                 :board-activity-sequence
                 (or (plist-get session :board-activity-sequence) 0))))
      (ignore had-state old-state record)
      (plist-put session :board-session-state (copy-tree board-state))
      ;; Durable publication is coordinated by the application service after
      ;; this semantic transition succeeds.
      (copy-tree board-state))))

(defun e-session-aggregate--fork-message-seed (message)
  "Return MESSAGE stripped of source-session identity for fork replay.
The fork rebuilds a fresh linear parent chain, so durable identity fields
(`:id', `:parent-id') and the source turn grouping (`:turn-id') are dropped;
the re-append path mints new ones anchored on the fork's own head."
  (let ((seed (copy-sequence message)))
    (dolist (key '(:id :parent-id :turn-id))
      (setq seed (e-session-aggregate--plist-remove seed key)))
    seed))

(cl-defun e-session-aggregate-fork (store session-id &key at metadata name)
  "Fork SESSION-ID in STORE into a new independent session and return it.

The fork is seeded with a snapshot of the source's current-path messages up to
AT (a head entry id; defaults to the source's current head), re-appended in
order so the fork is a clean linear continuation.  Context-bearing durable
metadata (canvas attachment, project root, capability state) and the source's
turn options (model/effort) are copied so the fork resumes with the same
working context.  Provider anchors and compaction structure are intentionally
not copied: the fork starts without provider cache and re-compacts on its own.

The source session is left untouched; new turns append only to the fork.
METADATA overrides merge onto the copied metadata; NAME, when given, sets the
fork's session name (otherwise it inherits the source name)."
  (let* ((source (e-session-aggregate-get-live store session-id))
         (head-id (or at (plist-get source :current-head-id)))
         (path (e-session-aggregate-current-path store session-id head-id))
         (portable-projection
          (e-session-aggregate-context-lifetime-projection store session-id head-id))
         (source-generation (plist-get portable-projection :generation))
         (source-checkpoint
          (and source-generation
               (e-context-lifetime-generation-checkpoint source-generation)))
         (promotion-messages
          (plist-get portable-projection :promotion-messages))
         ;; A deliberate non-empty portable generation is the only fork seed
         ;; that may replace the ordinary message copy.  A generation with
         ;; active promotions but no checkpoint still needs a portable seed so
         ;; v2/v3 durable items are not lost when the branch is selected.
         (portable-checkpoint
          (when (and source-generation
                     (or source-checkpoint promotion-messages))
            (e-context-lifetime-portable-checkpoint
             (append
              (copy-tree source-checkpoint)
              (mapcar #'e-context-lifetime-portable-message
                      (plist-get portable-projection :durable-tail))
              (mapcar #'e-context-lifetime-portable-message
                      promotion-messages))
             t)))
         (messages (seq-filter (lambda (entry)
                                 (eq (plist-get entry :type) 'message))
                               path))
         (base-metadata (copy-sequence (plist-get source :metadata)))
         (merged-metadata (e-session-aggregate--merge-metadata base-metadata metadata))
         (merged-metadata (if name
                              (plist-put merged-metadata :name name)
                            merged-metadata))
         (turn-options (plist-get source :turn-options))
         (fork (e-session-aggregate-create store :metadata merged-metadata)))
    (if portable-checkpoint
        (let (last-seed)
          ;; Keep the portable projection as ordinary fork messages so a
          ;; lifetime-disabled reader still sees the same semantic context.
          ;; The fresh generation then covers those seed messages, avoiding a
          ;; duplicate enabled projection and preventing covered source history
          ;; from returning.
          (dolist (message portable-checkpoint)
            (setq last-seed
                  (e-session-aggregate-append-message
                   store
                   (plist-get fork :id)
                   (e-session-aggregate--fork-message-seed message))))
          (e-session-aggregate-append-context-generation
           store
           (plist-get fork :id)
           (e-context-lifetime-generation-create
            :id (format "generation:fork:%s" (e-session-identity-generate-ulid))
            :checkpoint portable-checkpoint
            :covered-session-boundary (plist-get last-seed :id))))
      (dolist (message messages)
        (e-session-aggregate-append-message store (plist-get fork :id)
                                  (e-session-aggregate--fork-message-seed message))))
    (when turn-options
      (e-session-aggregate-set-turn-options store (plist-get fork :id) turn-options))
    (e-session-aggregate-get store (plist-get fork :id))))

(defun e-session-aggregate-get-live (store session-id)
  "Return the mutable live SESSION-ID state from STORE."
  (e-session-aggregate-peek-session store session-id))

(defun e-session-aggregate-get (store session-id)
  "Return SESSION-ID's mutable generic session state from STORE.
Board journal state is owned privately by STORE and is available only through
its dedicated board journal accessors."
  (e-session-aggregate-get-live store session-id))

(defun e-session-aggregate-messages (store session-id)
  "Return messages for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :messages)))

(defun e-session-aggregate-activity-events (store session-id)
  "Return durable activity events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :activity-events)))

(defun e-session-aggregate-latest-activity-event (store session-id)
  "Return the latest durable activity event for SESSION-ID in STORE."
  (when-let* ((session (e-session-aggregate-get-live store session-id))
              (tail (plist-get session :activity-events-tail)))
    (copy-tree (car tail))))

(defun e-session-aggregate-latest-token-usage-event (store session-id)
  "Return the latest durable token usage event for SESSION-ID in STORE."
  (plist-get (e-session-aggregate-get-live store session-id) :latest-token-usage-event))

(defun e-session-aggregate-session-events (store session-id)
  "Return durable session events for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :session-events)))

(defun e-session-aggregate-compactions (store session-id)
  "Return compaction records for SESSION-ID in STORE in insertion order."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :compactions)))

(defun e-session-aggregate-provider-anchors (store session-id)
  "Return provider anchor records for SESSION-ID in STORE in insertion order."
  (copy-sequence
   (plist-get (e-session-aggregate-get-live store session-id) :provider-anchors)))

(defun e-session-aggregate-context-generations (store session-id)
  "Return context generation entries for SESSION-ID in insertion order."
  (copy-tree
   (plist-get (e-session-aggregate-get-live store session-id) :context-generations)))

(defun e-session-aggregate-context-promotions (store session-id)
  "Return detached context promotion entries for SESSION-ID in path order.

Promotion components carried by a curation package are projected as virtual
entries so this compatibility accessor has the same :context-record shape as
standalone promotion entries.  The package remains the only persisted entry."
  (let (promotions)
    (dolist (entry (e-session-aggregate-current-path store session-id)
                   (nreverse promotions))
      (dolist (component (e-session-aggregate--context-entry-components entry))
        (when (eq (car component) 'context-promotion)
          (let ((record (cdr component)))
            (push
             (if (eq (plist-get entry :type) 'context-promotion)
                 (copy-tree entry)
               (list :type 'context-promotion
                     :id (plist-get record :id)
                     :parent-id (plist-get entry :parent-id)
                     :created-at (plist-get entry :created-at)
                     :context-record (copy-tree record)))
             promotions)))))))

(defun e-session-aggregate-context-erasures (store session-id)
  "Return detached version-1 erasure records for SESSION-ID in path order.

The returned records are detached and contain only identity/provenance.  This
accessor is an audit surface; model-facing projection uses the selected-head
query below instead of scanning every session entry itself."
  (let (records)
    (dolist (entry (e-session-aggregate-current-path store session-id)
                   (nreverse records))
      (dolist (component (e-session-aggregate--context-entry-components entry))
        (when (eq (car component) 'context-erasure)
          (push (e-context-lifetime-curation-erasure-record
                 (cdr component))
                records))))))

(defun e-session-aggregate-erased-tool-call-ids
    (store session-id &optional selected-head-id)
  "Return canonical erased tool-call IDs on SESSION-ID's selected head.

SELECTED-HEAD-ID is an optional durable entry id; nil selects the session's
current head.  The query walks only the parent path ending at that head,
preserves path/source order, and removes duplicate identities.  It is
intentionally read-only and does not inspect any Feature 90 receipt or
provider state."
  (unless (or (null selected-head-id) (stringp selected-head-id))
    (signal 'e-session-error
            (list "Invalid selected context head" selected-head-id)))
  (when (and selected-head-id
             (not (e-session-aggregate-entry-by-id store session-id selected-head-id)))
    (signal 'e-session-error
            (list "Unknown selected context head" selected-head-id)))
  (let ((head-id selected-head-id)
        (seen (make-hash-table :test 'equal))
        ids)
    (dolist (entry (e-session-aggregate-current-path store session-id head-id)
                   (nreverse ids))
      (dolist (component (e-session-aggregate--context-entry-components entry))
        (when (eq (car component) 'context-erasure)
          (dolist (tool-call-id
                   (e-context-lifetime-curation-erasure-tool-call-ids
                    (cdr component)))
            (unless (gethash tool-call-id seen)
              (puthash tool-call-id t seen)
              (push tool-call-id ids))))))))

(defun e-session-aggregate-context-curations (store session-id)
  "Return version-3 curation records for SESSION-ID in insertion order.

The persisted entry family remains `context-promotion' for compatibility, so
this accessor selects the new record version without exposing v2 promotion
records as if they were v3 records."
  (delq nil
        (mapcar
         (lambda (component)
           (let ((record (cdr component)))
             (when (and (eq (car component) 'context-promotion)
                        (equal (plist-get record :record-version)
                               e-context-lifetime-curation-record-version))
               (copy-tree record))))
         (cl-mapcan #'e-session-aggregate--context-entry-components
                    (e-session-aggregate-current-path store session-id)))))

(defun e-session-aggregate-context-lifetime-current-generation
    (store session-id &optional head-id)
  "Return the latest narrowed semantic generation on SESSION-ID's path.

Legacy frame/generation journal entries are intentionally not reconstructed as
runtime frames.  Only the current v2 generation codec participates in this
projection."
  (when-let ((entry (e-session-aggregate--context-active-generation
                     store session-id head-id)))
    (condition-case error
        (e-context-lifetime-generation-from-record
         (e-session-aggregate-context-record entry))
      (e-context-lifetime-invalid-record
       (signal 'e-session-error
               (list "Invalid current context generation" session-id error))))))

(defun e-session-aggregate--context-lifetime-durable-message (entry)
  "Return model-facing durable MESSAGE from canonical session ENTRY."
  (when (eq (plist-get entry :type) 'message)
    (let ((role (plist-get entry :role)))
      (unless (memq role '(tool-call tool))
        (let ((message (copy-tree entry)))
          ;; Provider replay items travel with a paired tool observation.  They
          ;; are not durable model context after that consuming request.
          (cl-remf message :id)
          (cl-remf message :parent-id)
          (cl-remf message :created-at)
          (cl-remf message :turn-id)
          (cl-remf message :type)
          (cl-remf message :durability-state)
          (when-let ((metadata (plist-get message :metadata)))
            (setq metadata (copy-tree metadata))
            (cl-remf metadata :provider-replay-items)
            (if metadata
                (plist-put message :metadata metadata)
              (cl-remf message :metadata)))
          message)))))

(defun e-session-aggregate-context-lifetime-durable-message (entry)
  "Return the portable durable projection of canonical message ENTRY.

This narrow consumer-facing wrapper keeps compaction and fork ownership from
duplicating the session transcript/body filtering rules."
  (e-session-aggregate--context-lifetime-durable-message entry))

(defun e-session-aggregate--checkpoint-path-suffix (store session-id)
  "Return the current path suffix selected by the latest valid compaction.
This is a domain path calculation; checkpoint serialization remains owned by
the catalog owner."
  (let ((path (e-session-aggregate-current-path store session-id)))
    (if-let* ((compaction
               (e-session-aggregate-latest-valid-compaction store session-id))
              (boundary-id (plist-get compaction :first-kept-entry-id))
              (boundary (e-session-aggregate-entry-by-id
                         store session-id boundary-id))
              (suffix (member boundary path)))
        suffix
      path)))

(defun e-session-aggregate--path-after-boundary (path boundary-id)
  "Return PATH strictly after BOUNDARY-ID, or PATH when it is absent."
  (if-let ((boundary (seq-find
                      (lambda (entry)
                        (equal (plist-get entry :id) boundary-id))
                      path)))
      (cdr (member boundary path))
    path))

(defun e-session-aggregate-context-lifetime-projection
    (store session-id &optional head-id)
  "Return canonical inputs for the semantic later-request projection.

The session transcript/current branch is the sole durable body source.  The
result contains no runtime frame or observation body; callers may add a fresh
consumer-bound frame at request construction time."
  (let* ((path (if head-id
                   (e-session-aggregate-current-path store session-id head-id)
                 (e-session-aggregate--checkpoint-path-suffix store session-id)))
         (generation (e-session-aggregate-context-lifetime-current-generation
                      store session-id head-id))
         (generation-path
          ;; The first opt-in generation is an identity boundary with no
          ;; checkpoint; keep the ordinary transcript until a deliberate
          ;; portable compaction supplies a real checkpoint.  A non-empty
          ;; checkpoint is the replacement boundary that covers its prefix.
          (if (and generation
                   (e-context-lifetime-generation-checkpoint generation))
              (e-session-aggregate--path-after-boundary
               path
               (e-context-lifetime-generation-covered-session-boundary
                generation))
            path))
         (messages (delq nil
                         (mapcar #'e-session-aggregate--context-lifetime-durable-message
                                 generation-path)))
         (generation-id (and generation
                             (e-context-lifetime-generation-id generation)))
         ;; Promotions before a deliberate portable boundary are absorbed into
         ;; its checkpoint.  Historical records remain in the audit journal but
         ;; only facts owned by the active generation are eligible here.
         (promotions nil)
         (curations nil)
         (promotion-message-entry-groups nil)
         (promotion-frontier nil))
    (dolist (entry path)
      (dolist (component (e-session-aggregate--context-entry-components entry))
        (when (and (eq (car component) 'context-promotion)
                   generation-id)
          (condition-case error
              (let ((record (cdr component)))
                (when (equal (plist-get record :generation-id)
                             generation-id)
                  (if (equal (plist-get record :record-version)
                             e-context-lifetime-curation-record-version)
                      (let ((curation
                             (e-context-lifetime-curation-from-record record)))
                        (push curation curations)
                        (push
                         (mapcar
                          (lambda (message)
                            (list :kind 'v3 :message
                                  (copy-tree message)))
                          (e-context-lifetime-curation-messages curation))
                         promotion-message-entry-groups))
                    (let ((promotion
                           (e-context-lifetime-promotion-from-record record)))
                      (push promotion promotions)
                      (push
                       (mapcar
                        (lambda (message)
                          (list :kind 'v2 :message
                                (copy-tree message)))
                        (e-context-lifetime-promotion-fact-messages
                         (list promotion)))
                       promotion-message-entry-groups)))
                  (push (plist-get record :id) promotion-frontier)))
            (e-context-lifetime-invalid-record
             (signal 'e-session-error
                     (list "Invalid current context promotion"
                           session-id error)))))))
    (let* ((promotion-message-entries
            (if promotion-message-entry-groups
                (apply #'append (nreverse promotion-message-entry-groups))
              nil))
           (promotion-messages
            (mapcar (lambda (entry) (plist-get entry :message))
                    promotion-message-entries)))
      (list :generation generation
            :durable-tail messages
            :promotions (nreverse promotions)
            :curations (nreverse curations)
            :promotion-messages promotion-messages
            :promotion-message-entries
            (copy-tree promotion-message-entries)
            :promotion-frontier (nreverse promotion-frontier)))))

(defun e-session-aggregate-process-reports (store session-id)
  "Return process reports for SESSION-ID in STORE in insertion order."
  (copy-sequence
   (plist-get (e-session-aggregate-get-live store session-id) :process-reports)))

(cl-defun e-session-aggregate-latest-compatible-provider-anchor
    (store session-id provider-id &key model fingerprints)
  "Return latest provider anchor compatible with SESSION-ID current path."
  (seq-find
   (lambda (anchor)
     (e-session-provider-anchor-policy-compatible-p
      (e-session-aggregate-current-path store session-id)
      anchor provider-id model fingerprints))
   (reverse (e-session-aggregate-provider-anchors store session-id))))

(defun e-session-aggregate-turn-options (store session-id)
  "Return session-scoped turn options for SESSION-ID in STORE."
  (copy-sequence (plist-get (e-session-aggregate-get-live store session-id) :turn-options)))

(defun e-session-aggregate--replace-metadata (store session-id metadata)
  "Replace SESSION-ID METADATA in STORE after validation."
  (let* ((metadata (e-session-metadata-validate metadata))
         (session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :metadata metadata))))
    (e-session-aggregate--index-entry store session-id event)
    (plist-put session :metadata metadata)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    metadata))

(defun e-session-aggregate-set-metadata (store session-id metadata)
  "Replace SESSION-ID METADATA in STORE.
This compatibility path validates that every key has a durable state schema.
New code should prefer the narrower typed metadata helpers."
  (e-session-aggregate--replace-metadata store session-id metadata))

(defun e-session-aggregate--merge-metadata (metadata updates)
  "Return METADATA with UPDATES applied."
  (let ((metadata (copy-sequence metadata)))
    (while (consp updates)
      (let ((key (pop updates)))
        (when (consp updates)
          (setq metadata (plist-put metadata key (pop updates))))))
    metadata))

(defun e-session-aggregate-set-session-config (store session-id config)
  "Merge durable session CONFIG into SESSION-ID metadata."
  (e-session-metadata-validate-class config 'session-config)
  (let* ((session (e-session-aggregate-get-live store session-id))
         (metadata (e-session-aggregate--merge-metadata
                    (plist-get session :metadata)
                    config)))
    (e-session-aggregate--replace-metadata store session-id metadata)))

(defun e-session-aggregate-context-references (store session-id owner)
  "Return current-state references for OWNER in SESSION-ID."
  (e-session-metadata-context-references-value
   (plist-get (e-session-aggregate-get-live store session-id) :metadata)
   owner))

(defun e-session-aggregate-set-context-references (store session-id owner references)
  "Set durable current-state REFERENCES for OWNER in SESSION-ID."
  (let* ((owner-key (e-session-metadata-owner-key owner))
         (session (e-session-aggregate-get-live store session-id))
         (metadata (copy-sequence (plist-get session :metadata)))
         (all-references (copy-sequence
                          (plist-get metadata :context-references))))
    (setq all-references
          (plist-put all-references
                     owner-key
                     (e-session-metadata-reference-value references)))
    (e-session-aggregate--replace-metadata
     store
     session-id
     (plist-put metadata :context-references all-references))
    references))

(defun e-session-aggregate-set-context-reference (store session-id key reference)
  "Set durable current-state REFERENCE metadata KEY for SESSION-ID."
  (e-session-metadata-validate-class (list key reference)
                                      'current-state-reference)
  (let* ((session (e-session-aggregate-get-live store session-id))
         (metadata (e-session-aggregate--merge-metadata
                    (plist-get session :metadata)
                    (list key reference))))
    (e-session-aggregate--replace-metadata store session-id metadata)))

(defun e-session-aggregate-capability-state (store session-id capability-id)
  "Return durable capability state for CAPABILITY-ID in SESSION-ID."
  (e-session-metadata-capability-state-value
   (plist-get (e-session-aggregate-get-live store session-id) :metadata)
   capability-id))

(cl-defun e-session-aggregate-set-capability-state
    (store session-id capability-id state &key version)
  "Set durable capability STATE for CAPABILITY-ID in SESSION-ID."
  (let* ((owner-key (e-session-metadata-owner-key capability-id))
         (session (e-session-aggregate-get-live store session-id))
         (metadata (copy-sequence (plist-get session :metadata)))
         (all-state (copy-sequence (plist-get metadata :capability-state)))
         (entry (if version
                    (list :version version :state state)
                  state)))
    (setq all-state (plist-put all-state owner-key entry))
    (e-session-aggregate--replace-metadata
     store
     session-id
     (plist-put metadata :capability-state all-state))
    entry))

(defun e-session-aggregate-set-turn-options (store session-id options)
  "Replace SESSION-ID turn OPTIONS in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (turn-options (e-session-aggregate--normalize-turn-options options))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :turn-options turn-options))))
    (e-session-aggregate--index-entry store session-id event)
    (plist-put session :turn-options turn-options)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    turn-options))

(defun e-session-aggregate-append-message (store session-id message)
  "Append MESSAGE to SESSION-ID in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
          (message (e-session-aggregate--normalize-entry-from-record
                    session
                    'message
                    (e-session-aggregate--message-with-created-at
                     message timestamp)
                    timestamp)))
    (when (and (eq (plist-get message :role) 'assistant)
               (not (plist-member message :board-output-sequence)))
      (let ((sequence
             (1+ (or (plist-get session :board-output-sequence)
                     (cl-loop for entry in (plist-get session :messages)
                              maximize (or (plist-get entry :board-output-sequence) 0))
                     0))))
        (setq message (plist-put message :board-output-sequence sequence))
        (plist-put session :board-output-sequence sequence)))
    (e-session-aggregate--append-list-item session :messages message)
    (e-session-aggregate--index-entry store session-id message)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--update-message-derived-fields-on-append store session message)
    message))

(defun e-session-aggregate--message-by-id (store session-id message-id)
  "Return SESSION-ID's message with MESSAGE-ID in STORE, or nil.
Prefers the entry-id index; falls back to a scan of `:messages' so a message
appended before an index rebuild is still found."
  (or (let ((entry (gethash message-id (e-session-aggregate--entry-index store session-id))))
        (and entry (eq (plist-get entry :type) 'message) entry))
      (seq-find (lambda (message)
                  (equal (plist-get message :id) message-id))
                (plist-get (e-session-aggregate-get-live store session-id) :messages))))

(defun e-session-aggregate-set-message-display (store session-id message-id display)
  "Set DISPLAY on SESSION-ID's message MESSAGE-ID in STORE and persist it.
DISPLAY is a display disposition symbol (e.g. `hidden'); nil clears it back to
the default visible state.  Mutates the in-memory message in place and appends
a durable `message-display' record so the change replays on reload.  Returns
the updated message, or nil when no such message exists."
  (when-let ((message (e-session-aggregate--message-by-id store session-id message-id)))
    (let ((timestamp (e-session-aggregate--timestamp)))
      (if display
          (plist-put message :display display)
        (cl-remf message :display))
      (e-session-aggregate--touch store (e-session-aggregate-get-live store session-id) timestamp)
      message)))

(defun e-session-aggregate--append-activity-event-entry
    (store session-id turn-id event-type payload entry-id write-index
           checkpoint-retain)
  "Append activity EVENT-TYPE with optional durable ENTRY-ID.

ENTRY-ID is reserved for the one audit-only context-curation response control
entry whose identity must be shared with its prepared record.  Ordinary
activity events continue to mint their own ids."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--normalize-entry-from-record
                 session
                 'activity-event
                 (append (when entry-id (list :id entry-id))
                         (when checkpoint-retain
                           (list :checkpoint-retain t))
                         (list :turn-id turn-id
                               :event-type event-type
                               :payload (copy-tree payload)
                               :created-at timestamp))
                 timestamp)))
    (unless (plist-member event :board-activity-sequence)
      (let ((sequence
             (1+ (or (plist-get session :board-activity-sequence)
                     (cl-loop for entry in (plist-get session :activity-events)
                              maximize (or (plist-get entry :board-activity-sequence) 0))
                     0))))
        (setq event (plist-put event :board-activity-sequence sequence))
        (plist-put session :board-activity-sequence sequence)))
    (e-session-aggregate--append-list-item session :activity-events event)
    (e-session-aggregate--update-activity-derived-fields session event)
    (e-session-aggregate--index-entry store session-id event)
    (e-session-aggregate--touch store session timestamp)
    (ignore write-index)
    event))

(cl-defun e-session-aggregate-append-activity-event
    (store session-id turn-id event-type payload &key (write-index t)
           checkpoint-retain)
  "Append durable activity EVENT-TYPE to STORE for SESSION-ID and TURN-ID.

When CHECKPOINT-RETAIN is non-nil, persist a generic retention marker on the
activity entry.  Checkpoint construction may pin a bounded marked tail from
the complete selected path; it does not interpret PAYLOAD."
  (e-session-aggregate--append-activity-event-entry
   store session-id turn-id event-type payload nil write-index
   checkpoint-retain))

(cl-defun e-session-aggregate-append-context-curation-response
    (store session-id turn-id response-entry-id &key (write-index t))
  "Append the audit-only control entry for a reserved curation response.

RESPONSE-ENTRY-ID is the durable identity already allocated for the provider
response.  The entry is kept in activity history rather than transcript
messages, so it is resolvable by id but cannot enter ordinary model context."
  (unless (and (stringp response-entry-id)
               (not (string-empty-p response-entry-id)))
    (signal 'e-session-error
            (list "Context curation response requires an entry id"
                  response-entry-id)))
  (e-session-aggregate--append-activity-event-entry
   store session-id turn-id 'context-curation-response
   (list :response-entry-id response-entry-id)
   response-entry-id write-index nil))

(defun e-session-aggregate-append-process-report (store session-id report)
  "Append out-of-band process REPORT to SESSION-ID in STORE.
Process reports are durable session entries but are not transcript messages and
therefore never enter backend context."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (report (e-session-aggregate--normalize-entry-from-record
                  session
                  'process-report
                  (copy-sequence report)
                  timestamp)))
    (e-session-aggregate--append-list-item session :process-reports report)
    (e-session-aggregate--index-entry store session-id report)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-file-field store session)
    report))

(cl-defun e-session-aggregate-append-branch-summary
    (store session-id branch-id summary &key metadata)
  "Append BRANCH-ID SUMMARY metadata to SESSION-ID in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (record (e-session-aggregate--normalize-entry-from-record
                  session
                  'branch-summary
                  (list :branch-id branch-id
                        :summary summary
                        :metadata metadata
                        :created-at timestamp)
                  timestamp)))
    (e-session-aggregate--append-list-item session :branch-summaries record)
    (e-session-aggregate--index-entry store session-id record)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-file-field store session)
    record))

(cl-defun e-session-aggregate-append-compaction
    (store session-id summary &key branch-id range first-kept-entry-id
           tokens-before tokens-kept metadata)
  "Append compaction SUMMARY for SESSION-ID in STORE.
BRANCH-ID, RANGE, FIRST-KEPT-ENTRY-ID, and METADATA describe the compacted
source when available."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (record (e-session-aggregate--normalize-entry-from-record
                  session
                  'compaction
                  (list :summary summary
                        :branch-id branch-id
                        :range range
                        :first-kept-entry-id first-kept-entry-id
                        :tokens-before tokens-before
                        :tokens-kept tokens-kept
                        :metadata metadata
                        :created-at timestamp)
                  timestamp)))
    (e-session-aggregate--append-list-item session :compactions record)
    (e-session-aggregate--index-entry store session-id record)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-file-field store session)
    record))

(cl-defun e-session-aggregate-append-provider-anchor
    (store session-id provider-id &key model covered-entry-id fingerprints
           metadata)
  "Append opaque PROVIDER-ID anchor metadata to SESSION-ID in STORE.
COVERED-ENTRY-ID identifies the latest transcript entry covered by the
provider-owned anchor.  FINGERPRINTS and METADATA are opaque to session core."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (record (e-session-aggregate--normalize-entry-from-record
                  session
                  'provider-anchor
                  (list :provider-id provider-id
                        :model model
                        :covered-entry-id covered-entry-id
                        :fingerprints fingerprints
                        :metadata metadata
                        :created-at timestamp)
                  timestamp)))
    (e-session-aggregate--append-list-item session :provider-anchors record)
    (e-session-aggregate--index-entry store session-id record)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-file-field store session)
    record))


(defun e-session-aggregate--context-current-path (store session-id &optional head-id)
  "Return the current canonical path for context ownership validation."
  (let* ((session (gethash session-id (e-session-store-sessions store)))
         (entries (and session
                       (append (plist-get session :session-events)
                               (plist-get session :messages)
                               (plist-get session :activity-events)
                               (plist-get session :branch-summaries)
                               (plist-get session :compactions)
                               (plist-get session :provider-anchors)
                               (plist-get session :process-reports)
                               (plist-get session :context-generations)
                               (plist-get session :context-promotions)
                               (plist-get session :context-curation-packages))))
         (by-id (make-hash-table :test #'equal))
         (visited (make-hash-table :test #'equal))
         path
         (head-id (or head-id
                      (and session (plist-get session :current-head-id)))))
    (dolist (entry entries)
      (puthash (plist-get entry :id) entry by-id))
    (while head-id
      (when (gethash head-id visited)
        (signal 'e-session-error
                (list "Context session path contains a cycle"
                      session-id head-id)))
      (puthash head-id t visited)
      (let ((entry (gethash head-id by-id)))
        (unless entry
          (signal 'e-session-error
                  (list "Context session path has unresolved head or parent"
                        session-id head-id)))
        (push entry path)
        (setq head-id (plist-get entry :parent-id))))
  path))

(defun e-session-aggregate--context-record-duplicate-key-p (record)
  "Return non-nil when semantic context RECORD repeats a keyword."
  (when (e-session-aggregate-keyword-plist-shape-p record)
    (let ((tail record)
          seen
          duplicate)
      (while tail
        (let ((key (pop tail)))
          (pop tail)
          (when (memq key seen)
            (setq duplicate t))
          (push key seen)))
      duplicate)))

(defun e-session-aggregate--normalize-context-record-for-replay (type record)
  "Normalize the semantic TYPE marker in decoded context RECORD."
  (let ((copy (copy-tree record)))
    (when (and (e-session-aggregate-keyword-plist-shape-p copy)
               (stringp (plist-get copy :type))
               (equal (plist-get copy :type) (symbol-name type)))
      (plist-put copy :type type))
    copy))

(defun e-session-aggregate--normalize-context-record
    (type record &optional expected-record-version read-legacy-p)
  "Validate and normalize semantic context RECORD for TYPE.
The aggregate consumes semantic values only; JSON conversion and durable
record decoding remain in `e-session-codec'."
  (when (e-session-aggregate--context-record-duplicate-key-p record)
    (signal 'e-session-error
            (list "Context lifetime record has duplicate fields" type)))
  (condition-case error
      (let* ((record-version (and (e-session-aggregate-keyword-plist-shape-p record)
                                  (plist-get record :record-version)))
             (decoded
              (cond
               ((eq type 'context-generation)
                (e-context-lifetime-generation-from-record record))
               ((eq type 'context-erasure)
                (e-context-lifetime-curation-erasure-from-record record))
               ((equal record-version
                       e-context-lifetime-curation-record-version)
                (e-context-lifetime-curation-from-record record))
               ;; Version-2 promotion records remain readable history.  The
               ;; strict compatibility decoder validates their old shape;
               ;; production writes continue to use the version-3 curation
               ;; record and never manufacture this representation.
               ((and read-legacy-p
                     (equal record-version e-context-lifetime-record-version))
                (e-context-lifetime-promotion-from-record record))
               (t
                (signal 'e-session-error
                        (list "Context lifetime writes require a supported version"
                              record-version)))))
             (normalized
              (cond
               ((eq type 'context-generation)
                (e-context-lifetime-generation-record decoded))
               ((eq type 'context-erasure)
                (e-context-lifetime-curation-erasure-record decoded))
               ((and read-legacy-p
                     (equal record-version e-context-lifetime-record-version))
                (copy-tree record))
               (t (e-context-lifetime-curation-record decoded)))))
        (when (and expected-record-version
                   (not (equal expected-record-version
                               (plist-get normalized :record-version))))
          (signal 'e-session-error
                  (list "Context record version is not accepted"
                        expected-record-version
                        (plist-get normalized :record-version))))
        normalized)
    (e-context-lifetime-invalid-record
     (signal 'e-session-error
             (list "Invalid context lifetime record" type error)))))

(defun e-session-aggregate--context-active-generation (store session-id &optional head-id)
  "Return the latest context generation on SESSION-ID's current path."
  (seq-find (lambda (entry)
              (eq (plist-get entry :type) 'context-generation))
            (reverse (e-session-aggregate--context-current-path store session-id head-id))))

(defun e-session-aggregate--validate-context-entry-ownership
    (store session-id type context-record)
  "Reject CONTEXT-RECORD when its generation owner is not active."
  (let* ((generation-entry
          (e-session-aggregate--context-active-generation store session-id))
         (generation-record
          (and generation-entry
               (e-session-aggregate-context-record generation-entry)))
         (generation-id (and generation-record
                             (plist-get generation-record :id))))
    (when (and (memq type '(context-promotion context-erasure))
               (not (equal (plist-get context-record :generation-id)
                           generation-id)))
      (signal 'e-session-error
              (list "Context lifetime record has no active generation owner"
                    session-id
                    (plist-get context-record :generation-id)
                    generation-id)))
    context-record))

(defun e-session-aggregate--exact-plist-keys-p (value keys)
  "Return non-nil when VALUE has exactly KEYS in canonical order."
  (and (e-session-aggregate-keyword-plist-shape-p value)
       (let ((tail value)
             (expected keys))
         (while (and tail expected (eq (car tail) (car expected)))
           (setq tail (cddr tail)
                 expected (cdr expected)))
         (and (null tail) (null expected)))))

(defun e-session-aggregate--bounded-id-value-p (value)
  "Return non-nil for one canonical non-empty identity scalar."
  (and (stringp value) (not (string-empty-p value))
       (<= (string-bytes value) 128)))

(defun e-session-aggregate--canonical-context-value-p (value)
  "Return non-nil when VALUE is already in context canonical form."
  (cond
   ((or (null value) (eq value t) (eq value :json-false)
        (stringp value) (numberp value)) t)
   ((or (symbolp value) (vectorp value) (hash-table-p value)) nil)
   ((e-session-aggregate-keyword-plist-shape-p value)
    (let ((tail value) previous valid)
      (setq valid t)
      (while (and tail valid)
        (let ((key (pop tail))
              (item (pop tail)))
          (when (and previous
                     (not (string< (symbol-name previous) (symbol-name key))))
            (setq valid nil))
          (unless (e-session-aggregate--canonical-context-value-p item)
            (setq valid nil))
          (setq previous key)))
      valid))
   ((proper-list-p value)
    (let ((tail value) (valid t))
      (while (and tail valid)
        (unless (e-session-aggregate--canonical-context-value-p (pop tail))
          (setq valid nil)))
      valid))
   (t nil)))

(defun e-session-aggregate--prin1-bytes-at-most-p (value limit)
  "Return non-nil when VALUE's default `prin1' form is at most LIMIT bytes.

The printer emits directly into a scalar counter; no payload-sized string or
buffer exists before admission."
  (let ((bytes 0)
        exceeded)
    (catch 'too-large
      (cl-labels
          ((output
            (character)
            (cl-incf bytes
                     (cond ((<= character #x7f) 1)
                           ((<= character #x7ff) 2)
                           ((<= character #xffff) 3)
                           (t 4)))
            (when (> bytes limit)
              (setq exceeded t)
              (throw 'too-large nil))))
        (let ((print-circle nil) (print-level nil) (print-length nil)
              (print-quoted t) (print-escape-newlines nil)
              (print-escape-control-characters nil) (print-escape-nonascii nil)
              (print-gensym nil) (print-integers-as-characters nil))
          (prin1 value #'output))))
    (not exceeded)))

(defun e-session-aggregate--context-component-admissible-p
    (record type version)
  "Return non-nil when RECORD is a canonical bounded TYPE/VERSION component."
  (if (null record)
      t
    (let ((identities-valid t))
      (dolist (field '(:id :frame-id :generation-id :consumer-request-id
                       :response-entry-id))
        (unless (e-session-aggregate--bounded-id-value-p
                 (plist-get record field))
          (setq identities-valid nil)))
      (and
       (e-session-aggregate--exact-plist-keys-p
        record
        (if (eq type 'context-promotion)
            '(:record-version :type :id :frame-id :generation-id
              :consumer-request-id :response-entry-id :items)
          '(:record-version :type :id :frame-id :generation-id
            :consumer-request-id :response-entry-id :sources)))
       (equal (plist-get record :record-version) version)
       (eq (plist-get record :type) type)
       identities-valid
       (e-session-aggregate--prin1-bytes-at-most-p
        record e-context-lifetime-curation-max-record-bytes)
       (if (eq type 'context-promotion)
           (let ((items (plist-get record :items))
                 (source-count 0)
                 (valid t))
             (unless (and (proper-list-p items) items)
               (setq valid nil))
             (dolist (item items)
               (let* ((kind (plist-get item :kind))
                      (ids (plist-get item :source-observation-ids))
                      (refs (plist-get item :source-refs))
                      (fingerprints (plist-get item :source-fingerprints))
                      (count (and (proper-list-p ids) (length ids))))
                 (unless
                     (and (memq kind '(exact summary))
                          (e-session-aggregate--exact-plist-keys-p
                           item
                           (if (eq kind 'exact)
                               '(:kind :value :source-observation-ids
                                 :source-refs :source-fingerprints)
                             '(:kind :text :source-observation-ids
                               :source-refs :source-fingerprints)))
                          count (> count 0)
                          (or (eq kind 'summary) (= count 1))
                          (proper-list-p refs) (proper-list-p fingerprints)
                          (= count (length refs)) (= count (length fingerprints))
                          (cl-every #'e-session-aggregate--bounded-id-value-p ids)
                          (cl-every #'e-session-aggregate--bounded-id-value-p refs)
                          (cl-every #'e-session-aggregate--bounded-id-value-p
                                    fingerprints)
                          (if (eq kind 'exact)
                              (e-session-aggregate--canonical-context-value-p
                               (plist-get item :value))
                            (let ((text (plist-get item :text)))
                              (and (stringp text) (not (string-empty-p text))))))
                   (setq valid nil))
                 (setq source-count (+ source-count (or count 0)))))
             ;; Source observation identities are unique across the package.
             (dolist (item items)
               (dolist (identity (plist-get item :source-observation-ids))
                 (let ((occurrences 0))
                   (dolist (other items)
                     (dolist (candidate
                              (plist-get other :source-observation-ids))
                       (when (equal identity candidate)
                         (cl-incf occurrences))))
                   (unless (= occurrences 1) (setq valid nil)))))
             (and valid
                  (<= source-count e-context-lifetime-curation-max-sources)))
         (let ((sources (plist-get record :sources))
               (count 0)
               (valid t))
           (unless (and (proper-list-p sources) sources)
             (setq valid nil))
           (dolist (source sources)
             (cl-incf count)
             (unless
                 (and (e-session-aggregate--exact-plist-keys-p
                       source '(:source-observation-id :source-ref
                                :source-fingerprint :tool-call-id))
                      (let ((fields-valid t))
                        (dolist (field '(:source-observation-id :source-ref
                                         :source-fingerprint :tool-call-id))
                          (unless (e-session-aggregate--bounded-id-value-p
                                   (plist-get source field))
                            (setq fields-valid nil)))
                        fields-valid))
               (setq valid nil)))
           (dolist (source sources)
             (dolist (field '(:source-observation-id :tool-call-id))
               (let ((identity (plist-get source field))
                     (occurrences 0))
                 (dolist (other sources)
                   (when (equal identity (plist-get other field))
                     (cl-incf occurrences)))
                 (unless (= occurrences 1) (setq valid nil)))))
           (and valid
                (<= count e-context-lifetime-curation-max-sources))))))))

(defun e-session-aggregate--context-curation-package-shape-p (package)
  "Return non-nil when PACKAGE is a canonical bounded component envelope."
  (and (e-session-aggregate-keyword-plist-shape-p package)
       (not (e-session-aggregate--context-record-duplicate-key-p package))
       (= (length package) 4)
       (plist-member package :promotion)
       (plist-member package :erasure)
       (or (plist-get package :promotion) (plist-get package :erasure))
       (e-session-aggregate--context-component-admissible-p
        (plist-get package :promotion) 'context-promotion
        e-context-lifetime-curation-record-version)
       (e-session-aggregate--context-component-admissible-p
        (plist-get package :erasure) 'context-erasure
        e-context-lifetime-curation-erasure-record-version)
       (or (null (plist-get package :promotion))
           (null (plist-get package :erasure))
           (cl-every
            (lambda (field)
              (equal (plist-get (plist-get package :promotion) field)
                     (plist-get (plist-get package :erasure) field)))
            '(:frame-id :generation-id :consumer-request-id
              :response-entry-id)))))

(defun e-session-aggregate--normalize-context-curation-package (package)
  "Return detached validated semantic components from curation PACKAGE.

This is the one Feature 88 package shape owned by the session boundary.  It is
deliberately not a general transaction abstraction: the only allowed fields
are the optional version-3 promotion and version-1 erasure components."
  (unless (e-session-aggregate-keyword-plist-shape-p package)
    (signal 'e-session-error
            (list "Context curation package must be a keyword plist" package)))
  (when (e-session-aggregate--context-record-duplicate-key-p package)
    (signal 'e-session-error
            (list "Context curation package has duplicate fields")))
  (let ((keys nil)
        (tail package))
    (while tail
      (push (pop tail) keys)
      (pop tail))
    (setq keys (nreverse keys))
    (unless (and (= (length keys) 2)
                 (memq :promotion keys)
                 (memq :erasure keys))
      (signal 'e-session-error
              (list "Context curation package has unsupported fields" keys))))
  (let* ((promotion-raw (plist-get package :promotion))
         (erasure-raw (plist-get package :erasure))
         (promotion
          (and promotion-raw
               (e-session-aggregate--normalize-context-record
                'context-promotion promotion-raw
                e-context-lifetime-curation-record-version)))
         (erasure
          (and erasure-raw
               (e-session-aggregate--normalize-context-record
                'context-erasure erasure-raw
                e-context-lifetime-curation-erasure-record-version))))
    (unless (or promotion erasure)
      (signal 'e-session-error
              (list "Context curation package has no semantic component")))
    (when (and promotion erasure)
      (dolist (field '(:frame-id :generation-id :consumer-request-id
                       :response-entry-id))
        (unless (equal (plist-get promotion field)
                       (plist-get erasure field))
          (signal 'e-session-error
                  (list "Context curation package component identity mismatch"
                        field)))))
    (list :promotion promotion :erasure erasure)))

(defun e-session-aggregate--validate-owned-context-curation-package (package)
  "Validate sealed PACKAGE and return its already-owned component graph.

The compatibility codecs are used only as bounded validators.  Their detached
result is discarded; the command, delta, live entry, and public result retain
the single frozen producer leaves from PACKAGE by identity."
  (unless (e-session-aggregate--context-curation-package-shape-p package)
    (signal 'e-session-error (list "Invalid context curation package")))
  ;; Canonical component leaves are retained by identity.  Only the fixed
  ;; two-slot semantic wrapper is newly constructed after admission.
  (list :promotion (plist-get package :promotion)
        :erasure (plist-get package :erasure)))

(defconst e-session-aggregate--sha256-k
  [#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1
   #x923f82a4 #xab1c5ed5 #xd807aa98 #x12835b01 #x243185be #x550c7dc3
   #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 #xe49b69c1 #xefbe4786
   #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
   #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147
   #x06ca6351 #x14292967 #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
   #x650a7354 #x766a0abb #x81c2c92e #x92722c85 #xa2bfe8a1 #xa81a664b
   #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
   #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a
   #x5b9cca4f #x682e6ff3 #x748f82ee #x78a5636f #x84c87814 #x8cc70208
   #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2]
  "SHA-256 round constants for bounded streaming package identity.")

(defun e-session-aggregate--sha256-rotr (word amount)
  "Rotate unsigned 32-bit WORD right by AMOUNT bits."
  (logand #xffffffff
          (logior (lsh word (- amount))
                  (lsh word (- 32 amount)))))

(defun e-session-aggregate--sha256-compress (state block)
  "Compress one 64-byte BLOCK into SHA-256 STATE."
  (let ((words (make-vector 64 0)))
    (dotimes (index 16)
      (let ((offset (* index 4)))
        (aset words index
              (logior (lsh (aref block offset) 24)
                      (lsh (aref block (+ offset 1)) 16)
                      (lsh (aref block (+ offset 2)) 8)
                      (aref block (+ offset 3))))))
    (cl-loop for index from 16 below 64 do
             (let* ((left (aref words (- index 15)))
                    (right (aref words (- index 2)))
                    (sigma0 (logxor
                             (e-session-aggregate--sha256-rotr left 7)
                             (e-session-aggregate--sha256-rotr left 18)
                             (lsh left -3)))
                    (sigma1 (logxor
                             (e-session-aggregate--sha256-rotr right 17)
                             (e-session-aggregate--sha256-rotr right 19)
                             (lsh right -10))))
               (aset words index
                     (logand #xffffffff
                             (+ (aref words (- index 16)) sigma0
                                (aref words (- index 7)) sigma1)))))
    (let ((a (aref state 0)) (b (aref state 1))
          (c (aref state 2)) (d (aref state 3))
          (e (aref state 4)) (f (aref state 5))
          (g (aref state 6)) (h (aref state 7)))
      (dotimes (index 64)
        (let* ((sum1 (logxor
                      (e-session-aggregate--sha256-rotr e 6)
                      (e-session-aggregate--sha256-rotr e 11)
                      (e-session-aggregate--sha256-rotr e 25)))
               (choice (logxor (logand e f) (logand (lognot e) g)))
               (temp1 (logand #xffffffff
                              (+ h sum1 choice
                                 (aref e-session-aggregate--sha256-k index)
                                 (aref words index))))
               (sum0 (logxor
                      (e-session-aggregate--sha256-rotr a 2)
                      (e-session-aggregate--sha256-rotr a 13)
                      (e-session-aggregate--sha256-rotr a 22)))
               (majority (logxor (logand a b) (logand a c) (logand b c)))
               (temp2 (logand #xffffffff (+ sum0 majority))))
          (setq h g g f f e e (logand #xffffffff (+ d temp1))
                d c c b b a a (logand #xffffffff (+ temp1 temp2)))))
      (dotimes (index 8)
        (aset state index
              (logand #xffffffff
                      (+ (aref state index)
                         (nth index (list a b c d e f g h)))))))))

(defun e-session-aggregate--sha256-prin1 (value)
  "Return VALUE's SHA-256 `prin1' digest without a canonical payload copy.

Only one 64-byte block and the fixed compression vectors are retained.  The
explicit printer bindings make package identity independent of caller display
settings while matching the historical default `prin1-to-string' form."
  (let ((state (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                       #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
        (block (make-vector 64 0))
        (fill 0)
        (total 0))
    (cl-labels
        ((byte
          (octet)
          (aset block fill octet)
          (setq fill (1+ fill) total (1+ total))
          (when (= fill 64)
            (e-session-aggregate--sha256-compress state block)
            (setq fill 0)))
         (character
          (character)
          (cond
           ((<= character #x7f) (byte character))
           ((<= character #x7ff)
            (byte (logior #xc0 (lsh character -6)))
            (byte (logior #x80 (logand character #x3f))))
           ((<= character #xffff)
            (byte (logior #xe0 (lsh character -12)))
            (byte (logior #x80 (logand (lsh character -6) #x3f)))
            (byte (logior #x80 (logand character #x3f))))
           (t
            (byte (logior #xf0 (lsh character -18)))
            (byte (logior #x80 (logand (lsh character -12) #x3f)))
            (byte (logior #x80 (logand (lsh character -6) #x3f)))
            (byte (logior #x80 (logand character #x3f))))))
         (output
          (chunk)
          (if (integerp chunk)
              (character chunk)
            (mapc #'character (string-to-list chunk)))))
      (let ((print-circle nil) (print-level nil) (print-length nil)
            (print-quoted t) (print-escape-newlines nil)
            (print-escape-control-characters nil) (print-escape-nonascii nil)
            (print-gensym nil) (print-integers-as-characters nil))
        (prin1 value #'output))
      (let ((bit-length (* total 8)))
        (byte #x80)
        (while (/= fill 56) (byte 0))
        (dotimes (index 8)
          (byte (logand (lsh bit-length (- (* 8 (- 7 index)))) #xff))))
      (mapconcat (lambda (word) (format "%08x" word)) state ""))))

(defun e-session-aggregate--context-curation-package-id (session-id package)
  "Return the deterministic identity for semantic curation PACKAGE."
  (format "context-curation-package:%s"
          (substring
           (e-session-aggregate--sha256-prin1
            (list session-id
                  (plist-get package :promotion)
                  (plist-get package :erasure)))
           0 32)))

(defun e-session-aggregate--context-curation-package-record
    (session-id package package-id parent-id timestamp)
  "Return one JSON-safe session record for curation PACKAGE."
  (list :type "context-curation-package"
        :session-id session-id
        :id package-id
        :parent-id parent-id
        :timestamp timestamp
        :promotion (plist-get package :promotion)
        :erasure (plist-get package :erasure)))

(defun e-session-aggregate--prepare-context-curation-package
    (store session-id package)
  "Prepare one persistent curation PACKAGE without mutating STORE.

The returned value contains detached normalized components and one indexed
session entry.  The package is the commit unit; its optional promotion and
erasure components are never represented as independently persisted entries."
  (let* ((package (e-session-aggregate--normalize-context-curation-package package))
         (session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (base-parent (plist-get session :current-head-id))
         (_promotion
          (when-let ((record (plist-get package :promotion)))
            (e-session-aggregate--validate-context-entry-ownership
             store session-id 'context-promotion record)))
         (_erasure
          (when-let ((record (plist-get package :erasure)))
            (e-session-aggregate--validate-context-entry-ownership
             store session-id 'context-erasure record)))
         (package-id (e-session-aggregate--context-curation-package-id
                      session-id package))
         (record (e-session-aggregate--context-curation-package-record
                  session-id package package-id base-parent timestamp))
         (entry
          ;; Preparation must not move the live head.  The normalized entry
          ;; receives the current parent, then the commit path advances the
          ;; head only after the durable package record is accepted.
          (e-session-aggregate--entry-with-identity
           session 'context-curation-package
           (list :promotion (plist-get package :promotion)
                 :erasure (plist-get package :erasure))
           timestamp record)))
    (list :package package
          :entry entry
          :package-id package-id
          :timestamp timestamp
          :record record)))

(defun e-session-aggregate--context-curation-package-existing-state
    (store session-id prepared)
  "Return an exact selected-path package for PREPARED, or signal a conflict.

An exact retry may run after the package advanced the session head (for
example, when its separate audit append failed), so the proposed parent is
not part of the idempotency comparison.  A package on an inactive sibling is
not a retry for the selected path and remains a conflict."
  (let* ((package-id (plist-get prepared :package-id))
         (existing (gethash package-id
                            (e-session-aggregate--entry-index store session-id)))
         (selected-path
          (and existing (e-session-aggregate-current-path store session-id)))
         (selected-p
          (and existing
               (seq-some
                (lambda (entry)
                  (equal (plist-get entry :id) package-id))
                selected-path))))
    (cond
     ((null existing) nil)
     ((and selected-p
           (eq (plist-get existing :type) 'context-curation-package)
           (equal (plist-get existing :promotion)
                  (plist-get (plist-get prepared :package) :promotion))
           (equal (plist-get existing :erasure)
                  (plist-get (plist-get prepared :package) :erasure)))
      existing)
     (t
      (signal 'e-session-error
              (list "Context curation package identity conflict"
                    package-id))))))

(defun e-session-aggregate--replay-context-curation-package
    (store session-id record)
  "Replay one validated curation PACKAGE RECORD atomically.

All semantic components are normalized and ownership-checked before the one
package entry is installed.  A repeated exact package is idempotent; an entry
with the same identity but different canonical components is rejected."
    (unless (and (e-session-aggregate-keyword-plist-shape-p record)
               (not (e-session-aggregate--context-record-duplicate-key-p record)))
    (signal 'e-session-error
            (list "Invalid context curation package record")))
  (let ((keys nil)
        (tail record))
    (while tail
      (push (pop tail) keys)
      (pop tail))
    (setq keys (nreverse keys))
    (unless (equal keys '(:type :session-id :id :parent-id :timestamp
                          :promotion :erasure))
      (signal 'e-session-error
              (list "Invalid context curation package fields" keys))))
  ;; Replay is already inside the owning session's load transaction.  Calling
  ;; `e-session-aggregate-get-live' here would see the not-yet-finalized replay session
  ;; as unloaded and recursively restart the same journal replay.
  (let* ((session (gethash session-id (e-session-store-sessions store)))
         (package
          (e-session-aggregate--normalize-context-curation-package
           (list :promotion
                 (e-session-aggregate--normalize-context-record-for-replay
                  'context-promotion (plist-get record :promotion))
                 :erasure
                 (e-session-aggregate--normalize-context-record-for-replay
                  'context-erasure (plist-get record :erasure)))))
         (package-id (plist-get record :id))
         (expected-id (e-session-aggregate--context-curation-package-id
                       session-id package))
         ;; Do not use the public lazy-loading lookup while a session is being
         ;; replayed.  Its fallback walks the live entry lists through
         ;; `e-session-aggregate-get-live', which would recursively restart this same
         ;; journal load before the replay session has been finalized.
         (existing (gethash package-id
                            (e-session-aggregate--entry-index store session-id))))
    (unless (and (equal (plist-get record :type)
                        "context-curation-package")
                 (equal (plist-get record :session-id) session-id)
                 (stringp package-id)
                 (stringp (plist-get record :timestamp))
                 (equal package-id expected-id))
      (signal 'e-session-error
              (list "Invalid context curation package identity" package-id)))
    (dolist (component (list (cons 'context-promotion
                                   (plist-get package :promotion))
                             (cons 'context-erasure
                                   (plist-get package :erasure))))
      (when (cdr component)
        (e-session-aggregate--validate-context-entry-ownership
         store session-id (car component) (cdr component))))
    (cond
     (existing
      (unless (and (eq (plist-get existing :type)
                       'context-curation-package)
                   (equal (plist-get existing :parent-id)
                          (plist-get record :parent-id))
                   (equal (plist-get existing :promotion)
                          (plist-get package :promotion))
                   (equal (plist-get existing :erasure)
                          (plist-get package :erasure)))
        (signal 'e-session-error
                (list "Context curation package replay conflict" package-id)))
      nil)
     (t
      (let ((entry
             (e-session-aggregate--normalize-entry-from-record
              session 'context-curation-package
              (list :promotion (plist-get package :promotion)
                    :erasure (plist-get package :erasure))
              (plist-get record :timestamp)
              record)))
        (e-session-aggregate--prepend-replayed-item
         session :context-curation-packages entry)
        (e-session-aggregate--index-entry store session-id entry)
        (e-session-aggregate--touch store session (plist-get record :timestamp)))))))

(cl-defun e-session-aggregate--append-context-entry
    (store session-id type field context-record &key (write-index t)
           expected-record-version)
  "Append narrowed provider-neutral CONTEXT-RECORD under TYPE."
  (unless (memq type e-session-aggregate--context-lifetime-entry-types)
    (signal 'e-session-error (list "Unknown context lifetime entry" type)))
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (context-record (e-session-aggregate--normalize-context-record
                          type context-record expected-record-version))
         (_ownership
          (e-session-aggregate--validate-context-entry-ownership
           store session-id type context-record))
         (entry
          (e-session-aggregate--normalize-entry-from-record
           session type (list :context-record context-record) timestamp)))
    (e-session-aggregate--append-list-item session field entry)
    (e-session-aggregate--index-entry store session-id entry)
    (e-session-aggregate--touch store session timestamp)
    (ignore write-index)
    entry))

(cl-defun e-session-aggregate-append-context-generation
    (store session-id generation &key (write-index t))
  "Append semantic GENERATION and return its durable session entry."
  (e-session-aggregate--append-context-entry
   store session-id 'context-generation :context-generations
   (if (e-context-lifetime-generation-p generation)
       (e-context-lifetime-generation-record generation)
     generation)
   :write-index write-index))

(cl-defun e-session-aggregate-append-context-curation-package
    (store session-id package &key (write-index t))
  "Atomically append semantic curation PACKAGE for SESSION-ID.

PACKAGE is the narrow pure value produced by
`e-context-lifetime-prepare-curation-disposition'.  Its optional promotion and
erasure components are validated and staged on a detached session before one
`context-curation-package' persistence operation is submitted.  The package
is therefore one direct write, one queued record, or one controller outbox
command; it is never split into independent component appends.  A repeated
package with the same component identities is idempotent.  Audit response
controls remain a separate activity append owned by the harness.

The persistence operation precedes live-session mutation, so synchronous write,
queue, or controller submission errors leave both semantic projections absent
and leave frame consumption to the caller."
  (let* ((prepared (e-session-aggregate--prepare-context-curation-package
                    store session-id package))
         (existing (e-session-aggregate--context-curation-package-existing-state
                    store session-id prepared))
         (entry (plist-get prepared :entry))
         (session (e-session-aggregate-get-live store session-id))
         (timestamp (plist-get prepared :timestamp)))
    (if existing
        (list :id (plist-get prepared :package-id)
              :package (plist-get prepared :package)
              :entry existing
              :promotion (plist-get existing :promotion)
              :erasure (plist-get existing :erasure)
              :already-present t)
      (e-session-aggregate--append-list-item
       session :context-curation-packages entry)
      (e-session-aggregate--index-entry store session-id entry)
      (e-session-aggregate--advance-head session entry)
      (e-session-aggregate--touch store session timestamp)
      (ignore write-index)
      (list :id (plist-get prepared :package-id)
            :package (plist-get prepared :package)
            :record (plist-get prepared :record)
            :entry entry
            :promotion (plist-get entry :promotion)
            :erasure (plist-get entry :erasure)))))

(defun e-session-aggregate-set-current-branch (store session-id branch-id)
  "Set SESSION-ID current branch cursor to BRANCH-ID in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--append-session-event
                 session
                 'current-branch
                 timestamp
                 (list :branch-id branch-id))))
    (e-session-aggregate--index-entry store session-id event)
    (plist-put session :current-branch branch-id)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    branch-id))

(defun e-session-aggregate-clear-messages (store session-id)
  "Clear all messages for SESSION-ID in STORE."
  (let* ((session (e-session-aggregate-get-live store session-id))
         (timestamp (e-session-aggregate--timestamp))
         (root-id (e-session-aggregate--root-event-id session))
         (event nil))
    (e-session-aggregate--replace-list-field session :messages nil)
    (e-session-aggregate--replace-list-field session :activity-events nil)
    (e-session-aggregate--replace-list-field session :provider-anchors nil)
    (plist-put session :latest-token-usage-event nil)
    (e-session-aggregate--clear-message-derived-fields store session)
    (e-session-aggregate--clear-entry-index store session-id)
    (dolist (entry (plist-get session :session-events))
      (e-session-aggregate--index-entry store session-id entry))
    (dolist (entry (plist-get session :branch-summaries))
      (e-session-aggregate--index-entry store session-id entry))
    (dolist (entry (plist-get session :compactions))
      (e-session-aggregate--index-entry store session-id entry))
    (dolist (entry (plist-get session :provider-anchors))
      (e-session-aggregate--index-entry store session-id entry))
    (plist-put session :current-head-id root-id)
    (setq event
          (e-session-aggregate--append-session-event
           session
           'messages-cleared
           timestamp
           (list :parent-id root-id)))
    (e-session-aggregate--index-entry store session-id event)
    (e-session-aggregate--touch store session timestamp)
    event))

(defun e-session-aggregate-rename (store session-id name)
  "Rename SESSION-ID in STORE to NAME."
  (when (string-empty-p (string-trim (or name "")))
    (user-error "Session name must not be empty"))
  (let* ((session (e-session-aggregate-get-live store session-id))
         (name (string-trim name))
         (timestamp (e-session-aggregate--timestamp))
         (event (e-session-aggregate--append-session-event
                 session
                 'session-info
                 timestamp
                 (list :name name))))
    (e-session-aggregate--index-entry store session-id event)
    (plist-put session :name name)
    (e-session-aggregate--touch store session timestamp)
    (e-session-aggregate--refresh-derived-fields store session)
    session))

(defun e-session-aggregate-display-title (store session-id)
  "Return display title for SESSION-ID in STORE."
  (e-session-aggregate--display-title-for-session
   (e-session-aggregate-peek-session store session-id)))

(defun e-session-aggregate-root-p (session)
  "Return non-nil when SESSION is a user-facing root session.
Subagent and task-queue sessions remain directly addressable through their own
surfaces, but do not belong in general session pickers."
  (let ((metadata (plist-get session :metadata)))
    (not (or (plist-get metadata :parent-session-id)
             (plist-get metadata :subagent-role)
             (plist-get metadata :task-queue-task-id)))))

(defun e-session-aggregate-list (store)
  "Return STORE sessions sorted by most recent message."
  (let (sessions)
    (maphash (lambda (_id session)
               (push (e-session-aggregate--session-index-entry store session) sessions))
             (e-session-store-sessions store))
    (sort sessions
          (lambda (left right)
            (let ((left-time (or (plist-get left :last-message-at)
                                 (plist-get left :created-at)
                                 ""))
                  (right-time (or (plist-get right :last-message-at)
                                  (plist-get right :created-at)
                                  ""))
                  (left-seq (or (plist-get left :updated-seq) 0))
                  (right-seq (or (plist-get right :updated-seq) 0)))
              (or (string> left-time right-time)
                  (and (string= left-time right-time)
                       (> left-seq right-seq))))))))

(defun e-session-aggregate-list-roots (store)
  "Return user-facing root sessions in STORE, newest first."
  (cl-remove-if-not #'e-session-aggregate-root-p (e-session-aggregate-list store)))


(defun e-session-aggregate-context-record (entry)
  "Return the provider-neutral context record carried by ENTRY."
  (copy-tree (plist-get entry :context-record)))

(defun e-session-aggregate--context-record-sequence (record key)
  "Return RECORD's KEY value as a detached logical-id sequence."
  (let ((value (plist-get record key)))
    (cond
     ((null value) nil)
     ((vectorp value) (append value nil))
     ((listp value) (copy-sequence value))
     (t (list value)))))

(defun e-session-aggregate--update-activity-derived-fields (session event)
  "Update derived SESSION fields for appended activity EVENT.

The replay application path calls this semantic aggregate operation after the
codec has detached the wire value; the codec does not know the aggregate's
plist representation."
  (when (eq (plist-get event :event-type) 'token-usage)
    (plist-put session :latest-token-usage-event event))
  event)

(defun e-session-aggregate--context-entry-components (entry)
  "Return semantic context components carried by durable ENTRY."
  (pcase (plist-get entry :type)
    ('context-promotion
     (list (cons 'context-promotion
                 (e-session-aggregate-context-record entry))))
    ('context-curation-package
     (delq nil
           (list (and (plist-get entry :promotion)
                      (cons 'context-promotion
                            (copy-tree (plist-get entry :promotion))))
                 (and (plist-get entry :erasure)
                      (cons 'context-erasure
                            (copy-tree (plist-get entry :erasure)))))))
    (_ nil)))

(defun e-session-aggregate-apply-record (store record)
  "Apply detached semantic RECORD to STORE during journal replay.

The durable codec owns only the wire-to-value mapping.  This function is the
aggregate's sole replay application boundary: it validates ownership, updates
the domain projections, and never performs a second persistence operation.
RECORD must already be detached by `e-session-codec-decode-record'."
  (let* ((type (plist-get record :type))
         (session-id (plist-get record :session-id))
         (timestamp (plist-get record :timestamp))
         (session (and session-id
                       (gethash session-id
                                (e-session-store-sessions store)))))
    (pcase type
      ("session"
       (e-session-aggregate--clear-board-journal store session-id)
       (let* ((metadata
               (e-session-metadata-validate
                (e-session-metadata-normalize-for-replay
                 (plist-get record :metadata) t)))
              (session
               (list :id session-id
                     ;; C07 applies an acknowledged record directly to the
                     ;; live aggregate; it is not a lazy-loader placeholder.
                     :loaded t
                     :metadata metadata
                     :session-events nil
                     :messages nil
                     :board-output-sequence
                     (or (plist-get record :board-output-sequence) 0)
                     :board-activity-sequence
                     (or (plist-get record :board-activity-sequence) 0)
                     :activity-events nil
                     :branch-summaries nil
                     :current-branch (plist-get record :current-branch)
                     :compactions nil
                     :provider-anchors nil
                     :process-reports nil
                     :context-generations nil
                     :context-promotions nil
                     :context-curation-packages nil
                     :created-at (or (plist-get record :created-at) timestamp)
                     :updated-at (or (plist-get record :updated-at) timestamp)
                     :turn-options
                     (e-session-aggregate--normalize-turn-options
                      (plist-get record :turn-options))
                     :name (plist-get record :name))))
         (e-session-aggregate-initialize-list-state session)
         (e-session-aggregate--prepend-replayed-session-event
          session 'session-created
          (or (plist-get record :created-at) timestamp)
          (list :metadata metadata) record)
         (e-session-aggregate--touch store session
                                     (plist-get session :updated-at))
         (puthash session-id session (e-session-store-sessions store))))
      ("message"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :messages
          (e-session-aggregate--normalize-entry-from-record
           session 'message
           (e-session-aggregate--message-with-created-at
            (plist-get record :message) timestamp)
           timestamp record))
         (when-let ((sequence
                     (plist-get (car (plist-get session :messages))
                                :board-output-sequence)))
           (plist-put session :board-output-sequence
                      (max (or (plist-get session :board-output-sequence) 0)
                           sequence)))
         (e-session-aggregate--touch store session timestamp)))
      ("board-message"
       (when session
         (let* ((journal (e-session-aggregate--board-journal store session-id))
                (message
                 (e-session-aggregate--normalize-board-message
                  (e-session-aggregate--freeze-board-value
                   (copy-tree (plist-get record :message)))))
                (existing
                 (e-session-aggregate--existing-board-message journal message)))
           (unless existing
             (puthash (e-session-aggregate-board-message-identity message)
                      message (e-session-board-journal-id-index journal))
             (let ((cell (list message)))
               (if-let ((tail (e-session-board-journal-tail journal)))
                   (setcdr tail cell)
                 (setf (e-session-board-journal-messages journal) cell))
               (setf (e-session-board-journal-tail journal) cell))))
         (e-session-aggregate--touch store session timestamp)))
      ("board-session-state"
       (when session
         (plist-put session :board-session-state
                   (e-session-aggregate-projected-board-association record))
         (e-session-aggregate--touch store session timestamp)))
      ("board-messages-cleared"
       (when session
         (e-session-aggregate--clear-board-journal store session-id)
         (e-session-aggregate--touch store session timestamp)))
      ("message-display"
       (when session
         (when-let ((message
                     (seq-find
                      (lambda (message)
                        (equal (plist-get message :id)
                               (plist-get record :id)))
                      (plist-get session :messages))))
           (let ((display (plist-get record :display)))
             (if display
                 (plist-put message :display (if (stringp display)
                                                 (intern display) display))
               (cl-remf message :display))))
         (e-session-aggregate--touch store session timestamp)))
      ("activity-event"
       (when session
         (let* ((event
                 (e-session-aggregate--normalize-entry-from-record
                  session 'activity-event
                  (or (plist-get record :semantic-event)
                      (list :id (plist-get record :id)
                            :parent-id (plist-get record :parent-id)
                            :turn-id (plist-get record :turn-id)
                            :event-type (plist-get record :event-type)
                            :payload (plist-get record :payload)
                            :created-at timestamp))
                  timestamp record)))
           (when (plist-member record :checkpoint-retain)
             (plist-put event :checkpoint-retain
                        (plist-get record :checkpoint-retain)))
           (when (plist-member record :board-activity-sequence)
             (plist-put event :board-activity-sequence
                        (plist-get record :board-activity-sequence)))
           (e-session-aggregate--prepend-replayed-item
            session :activity-events event)
           (when-let ((sequence (plist-get event :board-activity-sequence)))
             (plist-put session :board-activity-sequence
                        (max (or (plist-get session :board-activity-sequence) 0)
                             sequence)))
           (e-session-aggregate--update-activity-derived-fields session event)))
       (when session
         (e-session-aggregate--touch store session timestamp)))
      ("branch-summary"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :branch-summaries
          (e-session-aggregate--normalize-entry-from-record
           session 'branch-summary
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :branch-id (plist-get record :branch-id)
                 :summary (plist-get record :summary)
                 :metadata (plist-get record :metadata)
                 :created-at timestamp)
           timestamp record))
         (e-session-aggregate--touch store session timestamp)))
      ("compaction"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :compactions
          (e-session-aggregate--normalize-entry-from-record
           session 'compaction
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :summary (plist-get record :summary)
                 :branch-id (plist-get record :branch-id)
                 :range (plist-get record :range)
                 :first-kept-entry-id (plist-get record :first-kept-entry-id)
                 :tokens-before (plist-get record :tokens-before)
                 :tokens-kept (plist-get record :tokens-kept)
                 :metadata (plist-get record :metadata)
                 :created-at timestamp)
           timestamp record))
         (e-session-aggregate--touch store session timestamp)))
      ("provider-anchor"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :provider-anchors
          (e-session-aggregate--normalize-entry-from-record
           session 'provider-anchor
           (list :id (plist-get record :id)
                 :parent-id (plist-get record :parent-id)
                 :provider-id (let ((value (plist-get record :provider-id)))
                                (if (stringp value) (intern value) value))
                 :model (plist-get record :model)
                 :covered-entry-id (plist-get record :covered-entry-id)
                 :fingerprints (plist-get record :fingerprints)
                 :metadata (plist-get record :metadata)
                 :created-at timestamp)
           timestamp record))
         (e-session-aggregate--touch store session timestamp)))
      ("process-report"
       (when session
         (e-session-aggregate--prepend-replayed-item
          session :process-reports
          (e-session-aggregate--normalize-entry-from-record
           session 'process-report
           (plist-get record :report) timestamp record))
         (e-session-aggregate--touch store session timestamp)))
      ((or "context-frame" "context-frame-settlement") nil)
      ("context-erasure"
       (signal 'e-session-error
               (list "Standalone context erasure records are unsupported")))
      ("context-curation-package"
       (when session
         (e-session-aggregate--replay-context-curation-package
          store session-id record)))
      ((or "context-generation" "context-promotion")
       (let* ((entry-type (intern type))
              (raw-context-record (plist-get record :context-record)))
         ;; Version 1 records remain readable history, but do not recreate the
         ;; retired runtime-frame model.  Version 3 records are the only ones
         ;; admitted to the narrowed semantic projection.
         (when (and session
                    (e-session-aggregate--context-record-duplicate-key-p
                     raw-context-record))
           (signal 'e-session-error
                   (list "Context lifetime replay has duplicate fields"
                         entry-type)))
         (when (and session
                    (not (equal (plist-get raw-context-record :record-version)
                                1)))
           (let* ((field (if (eq entry-type 'context-generation)
                             :context-generations
                           :context-promotions))
                  (context-record
                  (e-session-aggregate--normalize-context-record
                    entry-type
                    (e-session-aggregate--normalize-context-record-for-replay
                     entry-type raw-context-record)
                    nil t))
                  (_ownership
                   (e-session-aggregate--validate-context-entry-ownership
                    store session-id entry-type context-record))
                  (entry
                   (e-session-aggregate--normalize-entry-from-record
                    session entry-type
                    (list :context-record context-record)
                    timestamp record)))
             (e-session-aggregate--prepend-replayed-item session field entry)
             (e-session-aggregate--touch store session timestamp)))))
      ("current-branch"
       (when session
         (plist-put session :current-branch (plist-get record :branch-id))
         (e-session-aggregate--prepend-replayed-session-event
          session 'current-branch timestamp
          (list :branch-id (plist-get record :branch-id)) record)
         (e-session-aggregate--touch store session timestamp)))
      ("session-info"
       (when session
         (let ((fields (e-session-aggregate--session-info-fields
                        session record)))
           (e-session-aggregate--prepend-replayed-session-event
            session 'session-info timestamp fields record)
           (when (plist-member fields :name)
             (plist-put session :name (plist-get fields :name)))
           (when (plist-member fields :metadata)
             (plist-put session :metadata
                        (e-session-metadata-normalize-for-replay
                         (plist-get fields :metadata) t)))
           (when (plist-member fields :turn-options)
             (plist-put session :turn-options
                        (e-session-aggregate--normalize-turn-options
                         (plist-get fields :turn-options))))
           (e-session-aggregate--touch store session timestamp))))
      ("messages-cleared"
       (when session
         (e-session-aggregate--replace-list-field session :messages nil)
         (e-session-aggregate--replace-list-field session :activity-events nil)
         (e-session-aggregate--replace-list-field session :provider-anchors nil)
         (plist-put session :latest-token-usage-event nil)
         (e-session-aggregate--clear-message-derived-fields store session)
         (plist-put session :current-head-id
                    (e-session-aggregate--root-event-id session))
         (e-session-aggregate--prepend-replayed-session-event
          session 'messages-cleared timestamp
          (list :parent-id (e-session-aggregate--root-event-id session)) record)
         (e-session-aggregate--touch store session timestamp))))))

(defun e-session-aggregate--apply-committed-record (store record)
  "Apply one acknowledged detached RECORD to live STORE in commit order.

This is intentionally separate from `e-session-aggregate-apply-record', which
reconstructs reverse journal pages.  The session coordinator calls this after
one durable acknowledgement, preserving forward list order, cached tails,
indexes, head identity, and incremental derived fields in bounded work."
  (let* ((type (plist-get record :type))
         (session-id (plist-get record :session-id))
         (timestamp (plist-get record :timestamp))
         (session (and session-id
                       (gethash session-id (e-session-store-sessions store)))))
    (pcase type
      ("session"
       (let* ((metadata
               (e-session-metadata-validate
                (e-session-metadata-normalize-for-replay
                 (plist-get record :metadata) t)))
              (created
               (list :id session-id :loaded t :metadata metadata
                     :session-events nil :messages nil :activity-events nil
                     :branch-summaries nil :compactions nil :provider-anchors nil
                     :process-reports nil :context-generations nil
                     :context-promotions nil :context-curation-packages nil
                     :board-output-sequence
                     (or (plist-get record :board-output-sequence) 0)
                     :board-activity-sequence
                     (or (plist-get record :board-activity-sequence) 0)
                     :current-branch (plist-get record :current-branch)
                     :created-at (or (plist-get record :created-at) timestamp)
                     :updated-at (or (plist-get record :updated-at) timestamp)
                     :turn-options (e-session-aggregate--normalize-turn-options
                                    (plist-get record :turn-options))
                     :name (plist-get record :name))))
         (e-session-aggregate-initialize-list-state created)
         (e-session-aggregate--append-session-event
          created 'session-created (or (plist-get record :created-at) timestamp)
          (list :metadata metadata) record)
         (e-session-aggregate--touch store created
                                     (plist-get created :updated-at))
         (puthash session-id created (e-session-store-sessions store))
         (e-session-aggregate--committed-apply-fault 'after-list-state)
         (e-session-aggregate--refresh-derived-fields store created)
         (e-session-aggregate--committed-apply-fault 'after-derived)
         (e-session-aggregate--index-session-entries store created)
         (e-session-aggregate--committed-apply-fault 'after-index)))
      ("message"
       (when session
         (let ((entry
                (e-session-aggregate--normalize-entry-from-record
                 session 'message
                 (e-session-aggregate--message-with-created-at
                  (plist-get record :message) timestamp)
                 timestamp record)))
           (e-session-aggregate--append-list-item session :messages entry)
           (e-session-aggregate--committed-apply-fault 'after-list-state)
           (when-let* ((sequence (plist-get entry :board-output-sequence)))
             (plist-put session :board-output-sequence
                        (max (or (plist-get session :board-output-sequence) 0)
                             sequence)))
           (e-session-aggregate--touch store session timestamp)
           (e-session-aggregate--update-message-derived-fields-on-append
            store session entry)
           (e-session-aggregate--committed-apply-fault 'after-derived)
           (e-session-aggregate--index-entry store session-id entry)
           (e-session-aggregate--committed-apply-fault 'after-index))))
      ("activity-event"
       (when session
         (let ((entry
                (e-session-aggregate--normalize-entry-from-record
                 session 'activity-event
                 (or (plist-get record :semantic-event)
                     (list :id (plist-get record :id)
                           :parent-id (plist-get record :parent-id)
                           :turn-id (plist-get record :turn-id)
                           :event-type (plist-get record :event-type)
                           :payload (plist-get record :payload)
                           :created-at timestamp))
                 timestamp record)))
           (when (plist-member record :checkpoint-retain)
             (plist-put entry :checkpoint-retain
                        (plist-get record :checkpoint-retain)))
           (when (plist-member record :board-activity-sequence)
             (plist-put entry :board-activity-sequence
                        (plist-get record :board-activity-sequence)))
           (e-session-aggregate--append-list-item session :activity-events entry)
           (e-session-aggregate--committed-apply-fault 'after-list-state)
           (when-let* ((sequence (plist-get entry :board-activity-sequence)))
             (plist-put session :board-activity-sequence
                        (max (or (plist-get session :board-activity-sequence) 0)
                             sequence)))
           (e-session-aggregate--update-activity-derived-fields session entry)
           (e-session-aggregate--touch store session timestamp)
           (e-session-aggregate--committed-apply-fault 'after-derived)
           (e-session-aggregate--index-entry store session-id entry)
           (e-session-aggregate--committed-apply-fault 'after-index))))
      ("message-display"
       (when session
         (when-let* ((message
                      (e-session-aggregate--message-by-id
                       store session-id (plist-get record :id))))
           (let ((display (plist-get record :display)))
             (if display
                 (plist-put message :display
                            (if (stringp display) (intern display) display))
               (cl-remf message :display)))
           (e-session-aggregate--committed-apply-fault 'after-list-state))
         (e-session-aggregate--touch store session timestamp)
         (e-session-aggregate--committed-apply-fault 'after-derived)))
      ((or "process-report" "branch-summary" "compaction" "provider-anchor"
           "context-generation")
       (when session
         (let* ((entry-type (intern type))
                (field (pcase type
                         ("process-report" :process-reports)
                         ("branch-summary" :branch-summaries)
                         ("compaction" :compactions)
                         ("provider-anchor" :provider-anchors)
                         (_ :context-generations)))
                (fields
                 (pcase type
                   ("process-report" (plist-get record :report))
                   ("branch-summary"
                    (list :id (plist-get record :id)
                          :parent-id (plist-get record :parent-id)
                          :branch-id (plist-get record :branch-id)
                          :summary (plist-get record :summary)
                          :metadata (plist-get record :metadata)))
                   ("compaction"
                    (cl-loop for key in '(:id :parent-id :summary :branch-id
                                          :range :first-kept-entry-id
                                          :tokens-before :tokens-kept :metadata)
                             append (list key (plist-get record key))))
                   ("provider-anchor"
                    (cl-loop for key in '(:id :parent-id :provider-id :model
                                          :covered-entry-id :fingerprints :metadata)
                             append (list key (plist-get record key))))
                   (_
                    (let ((context-record (plist-get record :context-record)))
                      (e-session-aggregate--validate-context-entry-ownership
                       store session-id 'context-generation context-record)
                      (list :id (plist-get record :id)
                            :parent-id (plist-get record :parent-id)
                            :context-record context-record)))))
                (entry (e-session-aggregate--normalize-entry-from-record
                        session entry-type fields timestamp record)))
           (e-session-aggregate--append-list-item session field entry)
           (e-session-aggregate--committed-apply-fault 'after-list-state)
           (e-session-aggregate--touch store session timestamp)
           (e-session-aggregate--refresh-file-field store session)
           (e-session-aggregate--committed-apply-fault 'after-derived)
           (e-session-aggregate--index-entry store session-id entry)
           (e-session-aggregate--committed-apply-fault 'after-index))))
      ("context-curation-package"
       (when session
         (let* ((promotion (plist-get record :promotion))
                (erasure (plist-get record :erasure))
                (id (plist-get record :id))
                (existing (gethash id
                                   (e-session-aggregate--entry-index
                                    store session-id))))
           (unless existing
             (dolist (component
                      (list (cons 'context-promotion
                                  promotion)
                            (cons 'context-erasure
                                  erasure)))
               (when (cdr component)
                 (e-session-aggregate--validate-context-entry-ownership
                  store session-id (car component) (cdr component))))
             (let ((entry
                    (e-session-aggregate--normalize-entry-from-record
                     session 'context-curation-package
                     (list :id id :parent-id (plist-get record :parent-id)
                           :promotion promotion :erasure erasure)
                     timestamp record)))
               (e-session-aggregate--append-list-item
                session :context-curation-packages entry)
               (e-session-aggregate--committed-apply-fault 'after-list-state)
               (e-session-aggregate--touch store session timestamp)
               (e-session-aggregate--committed-apply-fault 'after-derived)
               (e-session-aggregate--index-entry store session-id entry)
               (e-session-aggregate--committed-apply-fault 'after-index))))))
      ("messages-cleared"
       (when session
         (e-session-aggregate--replace-list-field session :messages nil)
         (e-session-aggregate--replace-list-field session :activity-events nil)
         (e-session-aggregate--replace-list-field session :provider-anchors nil)
         (plist-put session :latest-token-usage-event nil)
         (e-session-aggregate--clear-message-derived-fields store session)
         (e-session-aggregate--clear-entry-index store session-id)
         (dolist (field '(:session-events :branch-summaries :compactions
                          :provider-anchors))
           (dolist (entry (plist-get session field))
             (e-session-aggregate--index-entry store session-id entry)))
         (plist-put session :current-head-id
                    (e-session-aggregate--root-event-id session))
         (let ((event (e-session-aggregate--append-session-event
                       session 'messages-cleared timestamp
                       (list :parent-id
                             (e-session-aggregate--root-event-id session))
                       record)))
           (e-session-aggregate--committed-apply-fault 'after-list-state)
           (e-session-aggregate--touch store session timestamp)
           (e-session-aggregate--committed-apply-fault 'after-derived)
           (e-session-aggregate--index-entry store session-id event)
           (e-session-aggregate--committed-apply-fault 'after-index))))
      ("board-message"
       (when session
         (let* ((journal (e-session-aggregate--board-journal store session-id))
                (message (plist-get record :message))
                (existing
                 (e-session-aggregate--existing-board-message journal message)))
           (unless existing
             (puthash (e-session-aggregate-board-message-identity message)
                      message (e-session-board-journal-id-index journal))
             (let ((cell (list message)))
               (if-let* ((tail (e-session-board-journal-tail journal)))
                   (setcdr tail cell)
                 (setf (e-session-board-journal-messages journal) cell))
               (setf (e-session-board-journal-tail journal) cell)))
           (e-session-aggregate--committed-apply-fault 'after-list-state)
           (e-session-aggregate--touch store session timestamp)
           (e-session-aggregate--committed-apply-fault 'after-derived))))
      ("board-messages-cleared"
       (when session
         (e-session-aggregate--clear-board-journal store session-id)
         (e-session-aggregate--committed-apply-fault 'after-list-state)
         (e-session-aggregate--touch store session timestamp)
         (e-session-aggregate--committed-apply-fault 'after-derived)))
      ("board-session-state"
       (when session
         (let ((state (plist-get record :board-state)))
           ;; This path consumes only aggregate-produced acknowledged deltas;
           ;; replay uses the separate projection normalizer.  Retain the
           ;; already-frozen leaves instead of allocating a second policy.
           (unless (e-session-aggregate--valid-board-association-p state)
             (signal 'e-session-error
                     (list "Invalid committed board association" state)))
           (plist-put session :board-session-state state))
         (e-session-aggregate--committed-apply-fault 'after-list-state)
         (e-session-aggregate--touch store session timestamp)
         (e-session-aggregate--committed-apply-fault 'after-derived)))
      ("session-deleted"
       (remhash session-id (e-session-store-sessions store))
       (remhash session-id (e-session-store-entry-indexes store))
       (remhash session-id (e-session-store-board-journals store))
       (e-session-aggregate--committed-apply-fault 'after-list-state)
       (e-session-aggregate--committed-apply-fault 'after-derived)
       (e-session-aggregate--committed-apply-fault 'after-index))
      ("session-info"
       (when session
         (let ((fields (e-session-aggregate--session-info-fields
                        session record)))
           (let ((event (e-session-aggregate--append-session-event
                         session 'session-info timestamp fields record)))
             (e-session-aggregate--committed-apply-fault 'after-list-state)
             (when (plist-member fields :name)
               (plist-put session :name (plist-get fields :name)))
             (when (plist-member fields :metadata)
               (plist-put session :metadata
                          (e-session-metadata-normalize-for-replay
                           (plist-get fields :metadata) t)))
             (when (plist-member fields :turn-options)
               (plist-put session :turn-options
                          (e-session-aggregate--normalize-turn-options
                           (plist-get fields :turn-options))))
             (e-session-aggregate--touch store session timestamp)
             (e-session-aggregate--committed-apply-fault 'after-derived)
             (e-session-aggregate--index-entry store session-id event)
             (e-session-aggregate--committed-apply-fault 'after-index)))))
      ("current-branch"
       (when session
         (plist-put session :current-branch (plist-get record :branch-id))
         (let ((event (e-session-aggregate--append-session-event
                       session 'current-branch timestamp
                       (list :branch-id (plist-get record :branch-id)) record)))
           (e-session-aggregate--committed-apply-fault 'after-list-state)
           (e-session-aggregate--touch store session timestamp)
           (e-session-aggregate--committed-apply-fault 'after-derived)
           (e-session-aggregate--index-entry store session-id event)
           (e-session-aggregate--committed-apply-fault 'after-index))))
      (_
       (signal 'e-session-error
               (list "Unsupported incremental committed session record" type)))))
  (e-session-aggregate--committed-apply-fault 'after-record)
  store)

(defun e-session-aggregate-apply-committed-record (store record)
  "Transactionally apply acknowledged RECORD to live STORE in forward order.

The incremental interpreter owns the closed C07 record families and uses O(1)
forward append/index/derived updates.  It neither stages a whole session nor
runs replay finalization.  A fixed undo journal restores every touched live
reference and append tail before an invariant error escapes, so a caller never
observes an intermediate list/tail/head state.

This is the active C07 post-ACK aggregate boundary."
  (let* ((session-id (plist-get record :session-id))
         (sessions (e-session-store-sessions store))
         (indexes (e-session-store-entry-indexes store))
         (journals (e-session-store-board-journals store))
         (old-session (and session-id (gethash session-id sessions)))
         ;; Plist mutation changes existing cons cells.  A shallow spine copy
         ;; restores every field reference; append-only tail cdrs are journaled
         ;; separately because O(1) append mutates that shared cell.
         (old-session-spine (and old-session (copy-sequence old-session)))
         (old-tail-cdrs
          (and old-session
               (delq nil
                     (mapcar
                      (lambda (pair)
                        (when-let ((tail (plist-get old-session (cdr pair))))
                          (cons tail (cdr tail))))
                      e-session-aggregate--list-tail-fields))))
         ;; Incremental apply never mutates an old index table wholesale.  It
         ;; inserts at most one command identity, which rollback removes below.
         (old-index (and session-id (gethash session-id indexes)))
         (record-id (plist-get record :id))
         (old-index-entry (and old-index record-id
                               (gethash record-id old-index)))
         (old-journal (and session-id (gethash session-id journals)))
         (old-journal-messages
          (and old-journal (e-session-board-journal-messages old-journal)))
         (old-journal-tail
          (and old-journal (e-session-board-journal-tail old-journal)))
         (old-journal-tail-cdr (and old-journal-tail (cdr old-journal-tail)))
         (old-journal-index
          (and old-journal (e-session-board-journal-id-index old-journal)))
         (board-message (and (equal (plist-get record :type) "board-message")
                             (plist-get record :message)))
         (board-identity
          (and board-message
               (e-session-aggregate-board-message-identity board-message)))
         (old-board-entry
          (and old-journal-index board-identity
               (gethash board-identity old-journal-index)))
         (old-message
          (and (equal (plist-get record :type) "message-display")
               old-session
               (e-session-aggregate--message-by-id
                store session-id (plist-get record :id))))
         (old-message-spine (and old-message (copy-sequence old-message)))
         (old-sequence (e-session-store-sequence store))
         success)
    (unwind-protect
        (progn
          (e-session-aggregate--committed-apply-fault 'before-record)
          (e-session-aggregate--apply-committed-record store record)
          (setq success t)
          store)
      (unless success
        ;; Restore mutated append cells before reinstalling the old aggregate
        ;; spine so no failed delta remains reachable through an old tail.
        (dolist (tail-state old-tail-cdrs)
          (setcdr (car tail-state) (cdr tail-state)))
        (if old-session
            (progn
              ;; Keep the aggregate root identity stable for every existing
              ;; reader while restoring the old plist spine and field refs.
              (setcar old-session (car old-session-spine))
              (setcdr old-session (cdr old-session-spine))
              (puthash session-id old-session sessions))
          (remhash session-id sessions))
        (if old-index
            (progn
              (puthash session-id old-index indexes)
              (when record-id
                (if old-index-entry
                    (puthash record-id old-index-entry old-index)
                  (remhash record-id old-index))))
          (remhash session-id indexes))
        (if old-journal
            (progn
              (when old-journal-tail
                (setcdr old-journal-tail old-journal-tail-cdr))
              (setf (e-session-board-journal-messages old-journal)
                    old-journal-messages
                    (e-session-board-journal-tail old-journal) old-journal-tail
                    (e-session-board-journal-id-index old-journal)
                    old-journal-index)
              (when board-identity
                (if old-board-entry
                    (puthash board-identity old-board-entry old-journal-index)
                  (remhash board-identity old-journal-index)))
              (puthash session-id old-journal journals))
          (remhash session-id journals))
        (when old-message
          (setcar old-message (car old-message-spine))
          (setcdr old-message (cdr old-message-spine)))
        (setf (e-session-store-sequence store) old-sequence)))))

(defun e-session-aggregate--command-entry
    (session type fields command-id timestamp &optional explicit-id)
  "Build one detached command entry without mutating SESSION.

FIELDS is already frozen or newly constructed from bounded command wrappers.
Its variable leaves remain shared; only the small semantic plist spine is new."
  (let ((entry (copy-sequence fields)))
    (plist-put entry :type type)
    (plist-put entry :id (or explicit-id (plist-get entry :id) command-id))
    (unless (plist-member entry :parent-id)
      (plist-put entry :parent-id (plist-get session :current-head-id)))
    (unless (plist-member entry :created-at)
      (plist-put entry :created-at timestamp))
    entry))

(defun e-session-aggregate--command-resulting-metadata
    (session field arguments)
  "Return FIELD's validated full metadata delta for SESSION and ARGUMENTS."
  (let ((value (plist-get arguments :value)))
    (pcase field
      ('metadata (e-session-metadata-validate value))
      ('config
       (e-session-metadata-validate-class value 'session-config)
       (e-session-metadata-validate
        (e-session-aggregate--merge-metadata
         (plist-get session :metadata) value)))
      ('context-references
       (let* ((owner-key
               (e-session-metadata-owner-key (plist-get arguments :owner)))
              (metadata (copy-sequence (plist-get session :metadata)))
              (references
               (copy-sequence (plist-get metadata :context-references))))
         (setq references
               (plist-put references owner-key
                          (e-session-metadata-reference-value value)))
         (e-session-metadata-validate
          (plist-put metadata :context-references references))))
      ('context-reference
       (let ((key (plist-get arguments :key)))
         (e-session-metadata-validate-class
          (list key value) 'current-state-reference)
         (e-session-metadata-validate
          (e-session-aggregate--merge-metadata
           (plist-get session :metadata) (list key value)))))
      ('capability-state
       (let* ((owner-key
               (e-session-metadata-owner-key
                (plist-get arguments :capability-id)))
              (metadata (copy-sequence (plist-get session :metadata)))
              (all-state
               (copy-sequence (plist-get metadata :capability-state)))
              (entry (if (plist-get arguments :version)
                         (list :version (plist-get arguments :version)
                               :state value)
                       value)))
         (setq all-state (plist-put all-state owner-key entry))
         (e-session-metadata-validate
          (plist-put metadata :capability-state all-state))))
      (_ (signal 'e-session-error
                 (list "Command field does not produce metadata" field))))))

(defun e-session-aggregate--session-info-fields (session record)
  "Derive public session-event fields from bounded RECORD and SESSION.

Older records containing complete metadata remain readable.  New C07 records
carry :field plus only that mutation's frozen producer leaves."
  (if-let* ((field (plist-get record :field)))
      (pcase field
        ((or 'metadata 'config 'context-references 'context-reference
             'capability-state)
         (list :metadata
               (e-session-aggregate--command-resulting-metadata
                session field record)))
        ('turn-options
         (list :turn-options
               (e-session-aggregate--normalize-turn-options
                (plist-get record :value))))
        ('name (list :name (plist-get record :value)))
        (_ (signal 'e-session-error
                   (list "Unsupported bounded session-info field" field))))
    (let (fields)
      (dolist (field '(:name :metadata :turn-options))
        (when (plist-member record field)
          (setq fields (plist-put fields field (plist-get record field)))))
      fields)))

(defun e-session-aggregate-command-interpret (store command)
  "Interpret sealed COMMAND against committed STORE and return its one delta.

This is the aggregate-owned lane-head interpreter.  It reads only acknowledged
live state and constructs the command's bounded durable record directly; it
never copies or mutates a whole session.  Variable producer leaves are shared
with the sealed command."
  (let* ((session-id (e-session-aggregate-command-session-id command))
         (arguments (e-session-aggregate-command-arguments command))
         (tag (e-session-aggregate-command-tag command))
         (session (and (not (eq tag 'create))
                       (e-session-aggregate-get-live store session-id)))
         (request-id (e-session-aggregate-command-request-id command))
         (delta-id (e-session-aggregate-command-delta-id command))
         (command-time (e-session-aggregate-command-timestamp command))
         record result-kind result-id)
    ;; Identity and time are already sealed, so repeated interpretation yields
    ;; the same record without global generator rebinding or live mutation.
    (pcase tag
      ('create
       (when (e-session-aggregate-session-present-p store session-id)
         (signal 'e-session-duplicate (list session-id)))
       (let ((metadata
              (e-session-metadata-validate
               (e-session-metadata-normalize-for-replay
                (plist-get arguments :metadata)))))
         (setq record
               (list :type "session" :session-id session-id
                     :id delta-id :request-id request-id :delta-id delta-id
                     :timestamp command-time
                     :created-at command-time :updated-at command-time
                     :metadata metadata :name (plist-get metadata :name)
                     :turn-options nil :current-branch nil
                     :board-output-sequence 0 :board-activity-sequence 0)
               result-kind 'session)))
      ('append-message
       (let* ((message
               (e-session-aggregate--message-with-created-at
                (plist-get arguments :message) command-time))
              (entry
                (e-session-aggregate--command-entry
                session 'message message delta-id command-time)))
         (when (and (eq (plist-get entry :role) 'assistant)
                    (not (plist-member entry :board-output-sequence)))
           (plist-put entry :board-output-sequence
                      (1+ (or (plist-get session :board-output-sequence) 0))))
         (setq record
               (list :type "message" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :timestamp (plist-get entry :created-at)
                     :id (plist-get entry :id)
                     :parent-id (plist-get entry :parent-id)
                     :message entry)
               result-kind 'entry
               result-id (plist-get entry :id))))
      ((or 'append-activity 'context-curation-response)
       (let* ((curation-p (eq tag 'context-curation-response))
              (entry-id (and curation-p
                             (plist-get arguments :response-entry-id)))
              (event-type (if curation-p 'context-curation-response
                            (plist-get arguments :event-type)))
              (payload (if curation-p
                           (list :response-entry-id entry-id)
                         (plist-get arguments :payload)))
              (entry
               (e-session-aggregate--command-entry
                session 'activity-event
                (append
                 (when (and (not curation-p)
                            (plist-get arguments :checkpoint-retain))
                   (list :checkpoint-retain t))
                 (list :turn-id (plist-get arguments :turn-id)
                       :event-type event-type :payload payload))
                delta-id command-time entry-id))
              (sequence
               (or (plist-get entry :board-activity-sequence)
                   (1+ (or (plist-get session :board-activity-sequence) 0)))))
         (plist-put entry :board-activity-sequence sequence)
         (setq record
               (append
                (list :type "activity-event" :session-id session-id
                      :request-id request-id :delta-id delta-id
                      :id (plist-get entry :id)
                      :parent-id (plist-get entry :parent-id)
                      :turn-id (plist-get entry :turn-id)
                      :board-activity-sequence sequence
                      :timestamp (plist-get entry :created-at)
                      :event-type event-type :payload payload)
                (when (plist-get entry :checkpoint-retain)
                  (list :checkpoint-retain t)))
               result-kind 'entry
               result-id (plist-get entry :id))))
      ('message-display
       (let ((message-id (plist-get arguments :message-id)))
         (if (not (e-session-aggregate--message-by-id store session-id message-id))
             (setq result-kind 'nil-result)
           (setq record
                 (list :type "message-display" :session-id session-id
                       :request-id request-id :delta-id delta-id
                       :timestamp command-time :id message-id
                       :display (when-let* ((display
                                             (plist-get arguments :display)))
                                  (symbol-name display)))
                 result-kind 'message-display
                 result-id message-id))))
      ('process-report
       (let* ((entry (e-session-aggregate--command-entry
                      session 'process-report (plist-get arguments :report)
                      delta-id command-time)))
         (setq record
               (list :type "process-report" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :id (plist-get entry :id)
                     :parent-id (plist-get entry :parent-id)
                     :timestamp command-time :report entry)
               result-kind 'entry result-id (plist-get entry :id))))
      ('branch-summary
       (let ((entry (e-session-aggregate--command-entry
                     session 'branch-summary
                     (list :branch-id (plist-get arguments :branch-id)
                           :summary (plist-get arguments :summary)
                           :metadata (plist-get arguments :metadata))
                     delta-id command-time)))
         (setq record
               (list :type "branch-summary" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :id (plist-get entry :id)
                     :parent-id (plist-get entry :parent-id)
                     :timestamp command-time
                     :branch-id (plist-get entry :branch-id)
                     :summary (plist-get entry :summary)
                     :metadata (plist-get entry :metadata))
               result-kind 'entry result-id (plist-get entry :id))))
      ('compaction
       (let ((entry (e-session-aggregate--command-entry
                     session 'compaction
                     (list :summary (plist-get arguments :summary)
                           :branch-id (plist-get arguments :branch-id)
                           :range (plist-get arguments :range)
                           :first-kept-entry-id
                           (plist-get arguments :first-kept-entry-id)
                           :tokens-before (plist-get arguments :tokens-before)
                           :tokens-kept (plist-get arguments :tokens-kept)
                           :metadata (plist-get arguments :metadata))
                     delta-id command-time)))
         (setq record
               (append
                (list :type "compaction" :session-id session-id
                      :request-id request-id :delta-id delta-id
                      :id (plist-get entry :id)
                      :parent-id (plist-get entry :parent-id)
                      :timestamp command-time)
                (cl-loop for key in '(:summary :branch-id :range
                                      :first-kept-entry-id :tokens-before
                                      :tokens-kept :metadata)
                         append (list key (plist-get entry key))))
               result-kind 'entry result-id (plist-get entry :id))))
      ('provider-anchor
       (let ((entry (e-session-aggregate--command-entry
                     session 'provider-anchor
                     (list :provider-id (plist-get arguments :provider-id)
                           :model (plist-get arguments :model)
                           :covered-entry-id
                           (plist-get arguments :covered-entry-id)
                           :fingerprints (plist-get arguments :fingerprints)
                           :metadata (plist-get arguments :metadata))
                     delta-id command-time)))
         (setq record
               (append
                (list :type "provider-anchor" :session-id session-id
                      :request-id request-id :delta-id delta-id
                      :id (plist-get entry :id)
                      :parent-id (plist-get entry :parent-id)
                      :timestamp command-time)
                (cl-loop for key in '(:provider-id :model :covered-entry-id
                                      :fingerprints :metadata)
                         append (list key (plist-get entry key))))
               result-kind 'entry result-id (plist-get entry :id))))
      ('context-generation
       (let* ((raw (plist-get arguments :generation))
              (context-record
               (if (e-context-lifetime-generation-p raw)
                   ;; This fixed five-field wrapper is constructed only at the
                   ;; lane head, after reservation, and shares frozen leaves.
                   (e-context-lifetime-generation-record raw)
                 (let ((normalized
                        (e-session-aggregate--normalize-context-record
                         'context-generation raw)))
                   (unless (equal normalized raw)
                     (signal 'e-session-error
                             (list "Context generation is not canonical")))
                   raw)))
              (_ownership
               (e-session-aggregate--validate-context-entry-ownership
                store session-id 'context-generation context-record))
              (entry (e-session-aggregate--command-entry
                      session 'context-generation
                      (list :context-record context-record)
                      delta-id command-time)))
         (setq record
               (list :type "context-generation" :session-id session-id
                     :request-id request-id :delta-id delta-id
                     :id (plist-get entry :id)
                     :parent-id (plist-get entry :parent-id)
                     :timestamp command-time :context-record context-record)
               result-kind 'entry result-id (plist-get entry :id))))
      ('context-curation-package
       (let* ((package
               (e-session-aggregate--validate-owned-context-curation-package
                (plist-get arguments :package)))
              (_promotion
               (when-let* ((component (plist-get package :promotion)))
                 (e-session-aggregate--validate-context-entry-ownership
                  store session-id 'context-promotion component)))
              (_erasure
               (when-let* ((component (plist-get package :erasure)))
                 (e-session-aggregate--validate-context-entry-ownership
                  store session-id 'context-erasure component)))
              (package-id (e-session-aggregate--context-curation-package-id
                           session-id package))
              (package-record
               (e-session-aggregate--context-curation-package-record
                session-id package package-id
                (plist-get session :current-head-id) command-time))
              (entry (e-session-aggregate--command-entry
                      session 'context-curation-package
                      (list :promotion (plist-get package :promotion)
                            :erasure (plist-get package :erasure))
                      delta-id command-time package-id))
              (prepared (list :package package :package-id package-id
                              :entry entry :record package-record))
              (existing
               (e-session-aggregate--context-curation-package-existing-state
                store session-id prepared)))
         (setq record (unless existing package-record)
               result-kind 'curation-package
               result-id package-id)
         (when existing
           (setq result-kind 'curation-package-existing))))
      ('clear-messages
       (setq record
             (list :type "messages-cleared" :session-id session-id
                   :request-id request-id :delta-id delta-id :id delta-id
                   :parent-id (e-session-aggregate--root-event-id session)
                   :timestamp command-time)
             result-kind 'entry result-id delta-id))
      ('board-message
       (let* ((message
               (e-session-aggregate--normalize-owned-board-message
                (plist-get arguments :message)))
              (_type (e-session-aggregate--canonical-board-record-type
                      (plist-get message :record-type))))
         (let* ((journal (gethash session-id
                                  (e-session-store-board-journals store)))
                (existing
                 (e-session-aggregate--existing-board-message journal message))
                (identity
                 (e-session-aggregate-board-message-identity message)))
           (setq record
                 (unless existing
                   (list :type "board-message" :session-id session-id
                         :request-id request-id :delta-id delta-id
                         :id delta-id :timestamp command-time :message message))
                 result-kind 'board-message
                 result-id identity))))
      ('board-messages-clear
       (setq record
             (list :type "board-messages-cleared" :session-id session-id
                   :request-id request-id :delta-id delta-id
                   :id delta-id :timestamp command-time)
             result-kind 'nil-result))
      ('board-state
       (let ((state (list :board-id (plist-get arguments :board-id)
                          :principal (plist-get arguments :principal))))
         (when-let* ((role (plist-get arguments :association-role)))
           (setq state (plist-put state :association-role role)))
         (when-let* ((policy (plist-get arguments :routing-policy)))
           (setq state
                 (plist-put state :routing-policy
                            (e-session-board-routing-policy-normalize-owned
                             policy))))
         (setq record
               (list :type "board-session-state" :session-id session-id
                     :request-id request-id :delta-id delta-id :id delta-id
                     :timestamp command-time :board-state state
                     :board-id (plist-get state :board-id)
                     :principal (plist-get state :principal)
                     :board-output-sequence
                     (or (plist-get session :board-output-sequence) 0)
                     :board-activity-sequence
                     (or (plist-get session :board-activity-sequence) 0))
               result-kind 'board-state)))
      ('delete
       ;; This domain-only delta is never sent as a session record; it gives
       ;; transactional live retirement the same rollback boundary as appends.
       (setq record
             (list :type "session-deleted" :session-id session-id
                   :request-id request-id :delta-id delta-id
                   :id delta-id :timestamp command-time)
             result-kind 'true-result))
      ('session-info
       (let* ((field (plist-get arguments :field))
              (parent-id (plist-get session :current-head-id))
              bounded-fields)
         (pcase field
           ((or 'metadata 'config 'context-references 'context-reference
                'capability-state)
            ;; Persist only the bounded operation delta.  The acknowledged
            ;; apply/replay path derives the complete resulting metadata from
            ;; committed state; a queued command never retains that projection.
            (setq bounded-fields
                  (append (list :field field :value (plist-get arguments :value))
                          (pcase field
                            ('context-references
                             (list :owner (plist-get arguments :owner)))
                            ('context-reference
                             (list :key (plist-get arguments :key)))
                            ('capability-state
                             (list :capability-id
                                   (plist-get arguments :capability-id)
                                   :version (plist-get arguments :version)))))
                  result-kind
                  (pcase field
                    ((or 'metadata 'context-references) 'producer-value)
                    ('capability-state 'capability-state)
                    (_ 'metadata))))
           ('turn-options
            (setq bounded-fields
                  (list :field field :value (plist-get arguments :value))
                  result-kind 'turn-options))
           ('current-branch
            (setq record
                  (list :type "current-branch" :session-id session-id
                        :id delta-id :request-id request-id :delta-id delta-id
                        :parent-id parent-id
                        :timestamp command-time
                        :branch-id (plist-get arguments :value))
                  result-kind 'current-branch))
           ('name
            (let ((name (string-trim (or (plist-get arguments :value) ""))))
              (when (string-empty-p name)
                (user-error "Session name must not be empty"))
              (setq bounded-fields (list :field field :value name)
                    result-kind 'session))))
         (unless record
           (setq record
                 (append
                  (list :type "session-info" :session-id session-id
                        :id delta-id :request-id request-id :delta-id delta-id
                        :parent-id parent-id
                        :timestamp command-time)
                  bounded-fields))))))
    (unless (or record (memq result-kind
                             '(nil-result board-message
                               curation-package-existing true-result)))
      (signal 'e-session-error
              (list "Session command produced no durable delta" tag)))
    ;; The public result is resolved after ACK from authoritative live state.
    (list :record record :result-kind result-kind :result-id result-id)))

(defun e-session-aggregate--public-command-entry (entry)
  "Return ENTRY's bounded public spine without replay-only metadata."
  (when entry
    (let ((public (copy-sequence entry)))
      (cl-remf public :durability-state)
      public)))

(defun e-session-aggregate-command-result (store command delta)
  "Resolve COMMAND's O(1) public result from acknowledged DELTA in STORE."
  (let* ((session-id (e-session-aggregate-command-session-id command))
         (arguments (e-session-aggregate-command-arguments command))
         (kind (plist-get delta :result-kind))
         (session (unless (eq kind 'true-result)
                    (e-session-aggregate-get-live store session-id))))
    (pcase kind
      ('session session)
      ('entry
       (e-session-aggregate--public-command-entry
        (e-session-aggregate-entry-by-id
         store session-id (plist-get (plist-get delta :record) :id))))
      ('message-display
       (e-session-aggregate--public-command-entry
        (e-session-aggregate--message-by-id
         store session-id (plist-get delta :result-id))))
      ('board-message
       (let* ((journal (gethash session-id
                                (e-session-store-board-journals store)))
              (retained
               (and journal
                    (gethash (plist-get delta :result-id)
                             (e-session-board-journal-id-index journal)))))
         (and retained (copy-sequence retained))))
      ('board-state
       (e-session-aggregate--copy-owned-board-state
        (plist-get (e-session-aggregate-get-live store session-id)
                   :board-session-state)))
      ('curation-package
       (let* ((record (plist-get delta :record))
              (id (plist-get delta :result-id))
              (entry (e-session-aggregate-entry-by-id store session-id id)))
         (list :id id
               :package (list :promotion (plist-get entry :promotion)
                              :erasure (plist-get entry :erasure))
               :record record
               :entry (e-session-aggregate--public-command-entry entry)
               :promotion (plist-get entry :promotion)
               :erasure (plist-get entry :erasure))))
      ('curation-package-existing
       (let* ((id (plist-get delta :result-id))
              (entry (e-session-aggregate-entry-by-id store session-id id)))
         (list :id id
               :package (list :promotion (plist-get entry :promotion)
                              :erasure (plist-get entry :erasure))
               :entry (e-session-aggregate--public-command-entry entry)
               :promotion (plist-get entry :promotion)
               :erasure (plist-get entry :erasure)
               :already-present t)))
      ('nil-result nil)
      ('true-result t)
      ('metadata (plist-get session :metadata))
      ('producer-value (plist-get arguments :value))
      ('capability-state
       ;; Preserve the legacy facade's exact result shape from the detached
       ;; producer input.  Reading the metadata projection here can normalize
       ;; a caller vector into a list and breaks side-by-side return parity.
       (let ((value (plist-get arguments :value))
             (version (plist-get arguments :version)))
         (if version (list :version version :state value) value)))
      ('turn-options (plist-get session :turn-options))
      ('current-branch (plist-get session :current-branch))
      (_ (signal 'e-session-error
                 (list "Unsupported session command result" delta))))))

(provide 'e-session-aggregate)

;;; e-session-aggregate.el ends here

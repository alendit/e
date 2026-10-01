;;; e-session-async.el --- Optimistic session persistence service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The session application service admits each bounded mutation independently
;; to the runtime store and returns request-scoped `e-work' values for bounded
;; reads.  SQLite remains authoritative for durable current state, transaction
;; order, history, and query snapshot boundaries.  Emacs retains only unsettled
;; optimistic mutation intents.  Detached reads pass through the database result
;; unchanged; local admission or acknowledgement timing never changes visibility.

;;; Code:

(require 'cl-lib)
(require 'e-session-aggregate)
(require 'e-session-query-command)
(require 'e-session-storage)
(require 'e-runtime-store-codec)
(require 'e-work)

(define-error 'e-session-async-capacity-exhausted
  "Session durable-operation capacity is exhausted"
  'e-session-storage-error)

(defconst e-session-async-suspect-diagnostic-byte-limit 1024)
(defconst e-session-async-owner-pending-limit 64
  "Maximum unsettled mutation intents retained for one session owner.")

(cl-defstruct (e-session-async--state
               (:constructor e-session-async--state-create))
  store
  (pending (make-hash-table :test 'equal))
  (suspects (make-hash-table :test 'equal)))

(cl-defstruct (e-session-async--operation
               (:constructor e-session-async--operation-create))
  state session-id work storage-operation settled command before-submit
  board-pickup)

(cl-defstruct (e-session-async--read-operation
               (:constructor e-session-async--read-operation-create))
  "One request-scoped asynchronous session read.

The storage operation is deliberately opaque here.  The application service
  owns only the returned `e-work' and lets the SQLite adapter own runtime
request details and cancellation."
  store body transform work storage-operation settled)

(defvar e-session-async--states
  (make-hash-table :test 'eq :weakness 'key)
  "Application-owned optimistic state keyed by session store.")

(defvar e-session-async--enabled-stores
  (make-hash-table :test 'eq :weakness 'key)
  "Persistent stores whose facades use optimistic asynchronous persistence.")

(defconst e-session-async--operation-spec
  (e-work-spec-create
   :id "session-durable" :execution 'cooperative :interactive-policy 'async
   :owner 'e-session-async
   :runner (lambda (_handle _operation _context) :deferred)))

(defconst e-session-async--read-spec
  (e-work-spec-create
   :id "session-read" :execution 'cooperative :interactive-policy 'async
   :owner 'e-session-async
   :runner #'e-session-async--run-read)
  "Cooperative spec for bounded request-scoped session reads.")

(defun e-session-async--state (store)
  "Return STORE's application state, creating it when enabled."
  (or (gethash store e-session-async--states)
      (let ((state (e-session-async--state-create :store store)))
        (puthash store state e-session-async--states)
        state)))

(defun e-session-async-enable (store)
  "Enable optimistic asynchronous session persistence for STORE."
  (unless (e-session-storage-runtime-store store)
    (signal 'e-session-storage-error
            (list "Persistent session runtime is unavailable" store)))
  (puthash store t e-session-async--enabled-stores)
  (e-session-async--state store)
  store)

(defun e-session-async-enabled-p (store)
  "Return non-nil when STORE uses optimistic asynchronous persistence."
  (and (gethash store e-session-async--enabled-stores) t))

(defun e-session-async-pending-count (store session-id)
  "Return STORE's admitted unsettled mutation count for SESSION-ID."
  (if-let* ((state (gethash store e-session-async--states)))
      (length (gethash session-id (e-session-async--state-pending state)))
    0))

(defun e-session-async-pending-p (store session-id)
  "Return non-nil when SESSION-ID has an admitted unsettled mutation."
  (> (e-session-async-pending-count store session-id) 0))

(defun e-session-async--utf8-prefix (string byte-limit)
  "Return STRING's longest prefix occupying at most BYTE-LIMIT UTF-8 bytes."
  (if (<= (string-bytes string) byte-limit)
      string
    (let ((low 0) (high (min (length string) byte-limit)))
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring string 0 middle)) byte-limit)
              (setq low middle)
            (setq high (1- middle)))))
      (substring string 0 low))))

(defun e-session-async--detach-error (session-id error)
  "Return ERROR's bounded detached session-owned first-cause shape."
  (let* ((type (if (and (consp error) (symbolp (car error))
                        (get (car error) 'error-conditions))
                   (car error)
                 'e-session-storage-error))
         (properties (and (consp error) (cddr error)))
         (diagnostic
          (e-session-async--utf8-prefix
           (let ((print-circle t) (print-level 6) (print-length 32))
             (condition-case nil
                 (error-message-string error)
               (error "Session persistence failed")))
           e-session-async-suspect-diagnostic-byte-limit)))
    (list type (copy-sequence diagnostic)
          :session-id (and (stringp session-id) (copy-sequence session-id))
          :operation (plist-get properties :operation)
          :kind (plist-get properties :kind)
          :request-id
          (when-let* ((request-id (plist-get properties :request-id)))
            (if (stringp request-id) (copy-sequence request-id) request-id)))))

(defun e-session-async--note-suspect (store session-id error)
  "Retain SESSION-ID's first bounded detached persistence ERROR in STORE."
  (let* ((state (e-session-async--state store))
         (suspects (e-session-async--state-suspects state)))
    (or (gethash session-id suspects)
        (let ((detached (e-session-async--detach-error session-id error)))
          (puthash (copy-sequence session-id) detached suspects)
          detached))))

(defun e-session-async-session-suspect (store session-id)
  "Return SESSION-ID's detached process-local persistence suspicion, or nil."
  (when-let* ((state (gethash store e-session-async--states))
              (status (gethash session-id
                               (e-session-async--state-suspects state))))
    (copy-tree status)))

(defun e-session-async--failed-work (session-id cause)
  "Return a terminal session work handle carrying typed CAUSE."
  (let ((work (e-work-prepare
               e-session-async--operation-spec nil
               :context (list :domain-ref session-id
                              :work-kind 'session-durable))))
    (e-work-fail work cause)
    work))

(defun e-session-async--read-error (error)
  "Return ERROR as a condition suitable for an `e-work' failure.

The storage port normally supplies a condition list.  Keep the application
boundary total for test adapters and future storage implementations that
report a plain value instead."
  (if (and (consp error) (symbolp (car error)))
      error
    (list 'e-session-storage-error
          (format "Session read failed: %S" error))))

(defun e-session-async--settle-read (operation result error)
  "Settle request-scoped read OPERATION with RESULT or ERROR exactly once."
  (unless (e-session-async--read-operation-settled operation)
    (let ((transform (e-session-async--read-operation-transform operation))
          (work (e-session-async--read-operation-work operation)))
      (setf (e-session-async--read-operation-settled operation) t
            (e-session-async--read-operation-storage-operation operation) nil
            ;; A page transform may close over bounded in-flight operations.
            ;; Release that request-scoped snapshot before public observers run.
            (e-session-async--read-operation-transform operation) nil)
      (if error
          (e-work-fail work (e-session-async--read-error error))
        (condition-case transform-error
            ;; The worker already returns detached bounded values.  Copy
            ;; vectors as well as lists at this second domain boundary so a
            ;; test adapter or future backend cannot retain mutable containers.
            (e-work-finish
             work
             (copy-tree (if transform (funcall transform result) result) t))
          ((error quit)
           (e-work-fail work (e-session-async--read-error transform-error))))))))

(defun e-session-async--run-read (handle operation _context)
  "Submit OPERATION through the asynchronous session storage port."
  (let* ((store (e-session-async--read-operation-store operation))
         (body (e-session-async--read-operation-body operation))
         (storage-operation
          (e-session-storage-submit
           store 'read body
           (lambda (result error)
             (e-session-async--settle-read operation result error)))))
    ;; A storage adapter may observe an already-terminal request while
    ;; installing its callback.  In that case SETTLE-READ has already cleared
    ;; the operation; never re-enroll the stale returned handle.  Keep the
    ;; cancellation closure installed for both paths, but make it consult the
    ;; post-submit enrollment state under the same quit-inhibited window.
    (let ((inhibit-quit t))
      (setf (e-session-async--read-operation-work operation) handle
            (e-work-handle-cancel-function handle)
            (lambda (_handle)
              (setf (e-session-async--read-operation-transform operation) nil)
              (when-let* ((pending
                           (e-session-async--read-operation-storage-operation
                            operation)))
                (e-session-storage-cancel-operation store pending))))
      (unless (e-session-async--read-operation-settled operation)
        (setf (e-session-async--read-operation-storage-operation operation)
              storage-operation)))
    :deferred))

(cl-defun e-session-async--start-read (store body &key transform)
  "Return a started request-scoped e-work for bounded read BODY."
  (let* ((operation (e-session-async--read-operation-create
                     :store store :body (copy-tree body t)
                     :transform transform))
         (work (e-work-prepare
                e-session-async--read-spec operation
                :context (list :domain-ref
                               (or (plist-get body :session-id)
                                   'session-query)
                               :work-kind 'session-read))))
    (setf (e-session-async--read-operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    work))

(defun e-session-async-query-state (store session-id)
  "Return immediately with work reading exact SESSION-ID query state."
  (e-session-async--start-read
   store (list :op 'session-query-state :session-id session-id)))

(defun e-session-async-session-metadata (store session-id)
  "Return immediately with work reading exact SESSION-ID metadata."
  (e-session-async--start-read
   store (list :op 'session-metadata :session-id session-id)))

(defun e-session-async-board-association (store session-id)
  "Return immediately with work reading SESSION-ID's Board association."
  (e-session-async--start-read
   store (list :op 'session-board-association :session-id session-id)))

(cl-defun e-session-async-query-page
    (store &key cursor limit root-p board-id principal)
  "Return immediately with one detached bounded session query-state page.

CURSOR is the stable cursor returned by the previous page.  LIMIT, ROOT-P,
BOARD-ID, and PRINCIPAL are passed to the SQLite query adapter as explicit
consumer filters.  The result remains owned by the returned request-scoped
work; it is never installed into a session aggregate or catalog."
  (let ((body (list :op 'session-query-page
                    :limit (or limit 64))))
    (when cursor
      (setq body (append body (list :cursor cursor))))
    (when root-p
      (setq body (append body (list :root-p t))))
    (when (not (null board-id))
      (setq body (append body (list :board-id board-id))))
    (when (not (null principal))
      (setq body (append body (list :principal principal))))
    (e-session-async--start-read store body)))

(defun e-session-async-context-path (store session-id)
  "Return immediately with SESSION-ID's detached SQLite provider path.

The worker result carries its database high-water.  Pending Emacs mutations
never alter this query result; a causally dependent consumer must start only
after the mutation's explicit commit acknowledgement."
  (e-session-async--start-read
   store (list :op 'session-context-path :session-id session-id)))

(defun e-session-async-visible-message-page
    (store session-id &optional limit)
  "Return immediately with SESSION-ID's newest visible message window."
  (e-session-async--start-read
   store (list :op 'session-visible-message-page :session-id session-id
               :limit (or limit 32))))

(cl-defun e-session-async-record-page
    (store session-id &key after before limit order record-type record-id
           record-ids record-identity parent-id parent-ids)
  "Return immediately with one detached bounded journal page for SESSION-ID.

This is the history/inspection boundary.  AFTER is the stable journal-position
cursor returned as `:next' by an oldest-first page; BEFORE is the corresponding
cursor for a newest-first page.  ORDER is `oldest' or `newest'.  Optional scalar
and bounded-set identity fields are typed SQLite predicates, not filters over
an Emacs-owned transcript."
  (let ((body (list :op 'session-record-page
                    :session-id session-id
                    :order (or order 'oldest)
                    :limit (or limit 100))))
    (when after (setq body (append body (list :after after))))
    (when before (setq body (append body (list :before before))))
    (dolist (entry `((:record-type . ,record-type)
                     (:record-id . ,record-id)
                     (:record-ids . ,record-ids)
                     (:record-identity . ,record-identity)
                     (:parent-id . ,parent-id)
                     (:parent-ids . ,parent-ids)))
      (when (cdr entry)
        (setq body (append body (list (car entry) (cdr entry))))))
    (e-session-async--start-read store body)))

(defun e-session-async--reasoning-summary
    (store session-id activity-entry-id)
  "Return exact combined reasoning summary for internal Board composition.
Callers must obtain SESSION-ID and ACTIVITY-ENTRY-ID from an authorized Board
record; this private seam is not a public session-history operation."
  (unless (and (stringp session-id) (not (string-empty-p session-id))
               (stringp activity-entry-id)
               (not (string-empty-p activity-entry-id)))
    (signal 'e-session-storage-error
            (list "Reasoning summary identity is invalid")))
  (e-session-async--start-read
   store (list :op 'session-reasoning-summary
               :session-id session-id
               :activity-entry-id activity-entry-id)))

(cl-defun e-session-async-process-report-marker-page
    (store session-id &key before status (limit 64))
  "Return newest bounded markers with each latest triage from SQLite."
  (let ((body (list :op 'session-process-report-marker-page
                    :session-id session-id :limit limit)))
    (when before (setq body (append body (list :before before))))
    (when status (setq body (append body (list :status status))))
    (e-session-async--start-read store body)))

(defun e-session-async-process-report-marker (store session-id marker-id)
  "Return exact canonical MARKER-ID work for SESSION-ID."
  (e-session-async--start-read
   store (list :op 'session-process-report-marker :session-id session-id
               :marker-id marker-id)))

(cl-defun e-session-async-process-report-triage-page
    (store session-id marker-id &key before (limit 64))
  "Return newest bounded triage rows associated with MARKER-ID."
  (let ((body (list :op 'session-process-report-triage-page
                    :session-id session-id :marker-id marker-id :limit limit)))
    (when before (setq body (append body (list :before before))))
    (e-session-async--start-read store body)))

(cl-defun e-session-async-process-report-extraction-page
    (store session-id marker-id &key before (limit 64))
  "Return newest bounded extraction rows associated with MARKER-ID."
  (let ((body (list :op 'session-process-report-extraction-page
                    :session-id session-id :marker-id marker-id :limit limit)))
    (when before (setq body (append body (list :before before))))
    (e-session-async--start-read store body)))

(defun e-session-async-process-report-request-shapes
    (store session-id provider-request-ids)
  "Return latest exact request shapes for bounded PROVIDER-REQUEST-IDS."
  (e-session-async--start-read
   store (list :op 'session-process-report-request-shapes
               :session-id session-id
               :provider-request-ids (vconcat provider-request-ids))))

(defun e-session-async-process-report-marker-count (store session-id)
  "Return exact durable marker count work for SESSION-ID."
  (e-session-async--start-read
   store (list :op 'session-process-report-marker-count
               :session-id session-id)))

(cl-defun e-session-async-recent-failures (store &key (limit 10))
  "Return immediately with one bounded newest-first failed-turn query."
  (e-session-async--start-read
   store (list :op 'session-recent-failures :limit limit)))

(defun e-session-async-turn-inspection (store session-id turn-id)
  "Return immediately with one bounded detached failed-turn timeline."
  (e-session-async--start-read
   store (list :op 'session-turn-inspection
               :session-id session-id :turn-id turn-id)))

(defun e-session-async-continuation-outcome
    (store session-id run-id publication-key)
  "Return one bounded detached lifecycle outcome for a continuation.

The storage worker correlates the exact durable user-message metadata and
terminal activity event.  The result contains no transcript content; unknown
or ambiguous evidence remains an unsettled/no-outcome result for callers."
  (e-session-async--start-read
   store (list :op 'session-continuation-outcome
               :session-id session-id
               :run-id run-id
               :publication-key publication-key)))

(defun e-session-async-header (store session-id)
  "Return immediately with SESSION-ID's bounded journal header work."
  (e-session-async--start-read
   store (list :op 'session-header :session-id session-id)))

(cl-defun e-session-async-chat-view (store session-id &key (limit 32))
  "Return immediately with one SQLite-snapshot chat view and change cursor."
  (unless (and (integerp limit) (> limit 0) (<= limit 64))
    (signal 'e-session-storage-error
            (list "Persistent chat view limit is outside its bound" limit)))
  (e-session-async--start-read
   store (list :op 'chat-session-view :session-id session-id :limit limit)))

(defun e-session-async--start-work (session-id operation)
  "Return started deferred work for SESSION-ID owned by OPERATION."
  (let ((work (e-work-prepare
               e-session-async--operation-spec operation
               :context (list :domain-ref session-id
                              :work-kind 'session-durable))))
    (setf (e-session-async--operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    work))

(defun e-session-async--add-pending (operation)
  "Publish OPERATION in its session-owned pending set."
  (let* ((state (e-session-async--operation-state operation))
         (session-id (e-session-async--operation-session-id operation))
         (pending (e-session-async--state-pending state))
         (current (gethash session-id pending)))
    (when (>= (length current) e-session-async-owner-pending-limit)
      (signal 'e-session-async-capacity-exhausted
              (list "Session in-flight mutation capacity is exhausted"
                    :session-id session-id
                    :limit e-session-async-owner-pending-limit)))
    (puthash session-id
             (append current (list operation))
             pending)))

(defun e-session-async--remove-pending (operation)
  "Remove OPERATION once from its session-owned pending set."
  (when-let* ((state (e-session-async--operation-state operation))
              (session-id (e-session-async--operation-session-id operation)))
    (let* ((pending (e-session-async--state-pending state))
           (remaining (delq operation (gethash session-id pending))))
      (if remaining
          (puthash session-id remaining pending)
        (remhash session-id pending)))))

(defun e-session-async--fail-work-isolated (work cause)
  "Fail WORK with CAUSE without allowing arbitrary observers to gate cleanup."
  (condition-case nil
      (e-work-fail work cause)
    ((error quit) nil))
  work)

(defun e-session-async--relational-fail-request (operation error)
  "Fail relational OPERATION for a request-local admission ERROR."
  (unless (e-session-async--operation-settled operation)
    (setf (e-session-async--operation-settled operation) t
          (e-session-async--operation-storage-operation operation) nil)
    (let ((work (e-session-async--operation-work operation)))
      (e-session-async--remove-pending operation)
      (e-session-async--fail-work-isolated
       work (e-session-async--read-error error)))))

(defun e-session-async--relational-fail-owner (operation error)
  "Mark OPERATION's owner suspect and fail its unsettled optimistic work."
  (unless (e-session-async--operation-settled operation)
    (let* ((state (e-session-async--operation-state operation))
           (store (e-session-async--state-store state))
           (session-id (e-session-async--operation-session-id operation))
           (cause (e-session-async--note-suspect store session-id error))
           (unsettled (copy-sequence
                       (gethash session-id
                                (e-session-async--state-pending state)))))
      ;; Detach the complete owner set before notifying any public work.
      ;; Other owners remain independently runnable in the shared transport.
      (remhash session-id (e-session-async--state-pending state))
      (dolist (current unsettled)
        (unless (e-session-async--operation-settled current)
          (setf (e-session-async--operation-settled current) t
                (e-session-async--operation-storage-operation current) nil)
          (e-session-async--fail-work-isolated
           (e-session-async--operation-work current) (copy-tree cause)))))))

(defun e-session-async--relational-write-settled (operation result error)
  "Settle one independently admitted relational write OPERATION."
  (if error
      (e-session-async--relational-fail-owner operation error)
    (unless (e-session-async--operation-settled operation)
      (setf (e-session-async--operation-settled operation) t
            (e-session-async--operation-storage-operation operation) nil)
      (let ((work (e-session-async--operation-work operation)))
        (e-session-async--remove-pending operation)
        (e-work-finish work (copy-tree result t))))))

(defun e-session-async--submit-command-operation (operation)
  "Submit OPERATION for transaction-local interpretation by SQLite."
  (unless (e-session-async--operation-settled operation)
    (condition-case error
        (let* ((command (e-session-async--operation-command operation))
               (continuity
                (when-let* ((before-submit
                             (e-session-async--operation-before-submit
                              operation)))
                  (funcall before-submit command)))
               (body (append
                      (list :op 'session-command
                            :session-id
                            (e-session-async--operation-session-id operation)
                            :command
                            (e-session-query-command-to-wire command))
                      (when continuity (list :continuity continuity))
                      (when-let* ((board-pickup
                                   (e-session-async--operation-board-pickup
                                    operation)))
                        (list :board-pickup board-pickup)))))
          (e-session-storage-validate-operation-body
           (e-session-async--state-store
            (e-session-async--operation-state operation))
           body)
          (setf (e-session-async--operation-before-submit operation) nil)
          (let ((submitted
                 (e-session-storage-submit-owned
                  (e-session-async--state-store
                   (e-session-async--operation-state operation))
                  (e-session-async--operation-session-id operation)
                  body
                  (lambda (result write-error)
                    (e-session-async--relational-write-settled
                     operation result write-error)))))
            (unless submitted
              (signal 'e-session-storage-error
                      (list "Session storage rejected an admitted write")))
            (unless (e-session-async--operation-settled operation)
              (setf (e-session-async--operation-storage-operation operation)
                    submitted))))
      ((error quit)
       ;; Validation and admission failures occur before a write crosses
       ;; SQLite, so they remain request-local.
       (e-session-async--relational-fail-request operation error)))))

(cl-defun e-session-async--submit-relational-command
    (store session-id tag arguments &key before-submit board-pickup)
  "Admit one bounded relational TAG independently and return its work."
  (condition-case error
      (let* ((command (e-session-aggregate-command-prepare
                       tag session-id arguments))
             (effective-id (e-session-aggregate-command-session-id command)))
        (if-let* ((suspect (e-session-async-session-suspect store effective-id)))
            (e-session-async--failed-work effective-id suspect)
          (let* ((state (e-session-async--state store))
                 (operation
                  (e-session-async--operation-create
                   :state state :session-id effective-id :command command
                   :before-submit before-submit
                   :board-pickup (and board-pickup
                                      (copy-tree board-pickup t))))
                 (work (e-session-async--start-work effective-id operation)))
            (condition-case admission-error
                (progn
                  (e-session-async--add-pending operation)
                  (e-session-async--submit-command-operation operation)
                  work)
              ((error quit)
               (e-session-async--fail-work-isolated work admission-error))))))
    ((e-session-command-too-large e-runtime-store-codec-too-large
                                  e-runtime-store-request-too-large)
     (e-session-async--failed-work
      session-id
      (list 'e-session-async-capacity-exhausted
            "Session command exceeds its practical capacity" :cause error)))
    ((e-session-error wrong-type-argument)
     (e-session-async--failed-work
      session-id
      (list 'e-session-storage-command-error
            "Invalid asynchronous session command" :cause error)))))

(cl-defun e-session-async-submit-command
    (store session-id tag arguments &key before-submit write-index)
  "Admit one SQLite-authoritative TAG without an application FIFO.

No durable session aggregate is read, installed, or retained.  WRITE-INDEX is
accepted for facade compatibility; relational query rows are updated in the
same SQLite transaction as the journal record.  Concurrent submissions make no
ordering promise; an acknowledgement followed by a later submission establishes
happens-before, and an explicitly dependent group belongs in one transaction."
  (ignore write-index)
  (e-session-async--submit-relational-command
   store session-id tag arguments :before-submit before-submit))

(cl-defun e-session-async-submit-command-with-board-pickup
    (store session-id tag arguments board-pickup &key before-submit)
  "Submit session command TAG with BOARD-PICKUP in the same SQLite write.

The session async owner still admits and settles the command.  BOARD-PICKUP
is an immutable transaction descriptor interpreted by the runtime-store
worker; a failed command rolls its pickup claim back with the session write."
  (unless (and (listp board-pickup)
               (plist-get board-pickup :board-id)
               (plist-get board-pickup :generation)
               (plist-get board-pickup :participant-id)
               (plist-get board-pickup :delivery-id))
    (signal 'e-session-storage-error
            (list "Board pickup transaction coordinates are incomplete")))
  (e-session-async--submit-relational-command
   store session-id tag arguments
   :before-submit before-submit :board-pickup board-pickup))

(defun e-session-async-unsupported-command (session-id name)
  "Return a terminal typed work for unsupported asynchronous command NAME."
  (e-session-async--failed-work
   session-id
   (list 'e-session-storage-command-error
         "Unsupported asynchronous session command" name)))

(defun e-session-async-reset (store)
  "Clear STORE's process-local pending and suspect session state."
  (when-let* ((state (gethash store e-session-async--states)))
    (let (operations)
      ;; Detach every application-owned link before arbitrary work observers.
      (unwind-protect
          (maphash (lambda (_session-id pending)
                     (setq operations (nconc pending operations)))
                   (e-session-async--state-pending state))
        (clrhash (e-session-async--state-pending state))
        (clrhash (e-session-async--state-suspects state)))
      (dolist (operation operations)
        (unless (e-session-async--operation-settled operation)
          (setf (e-session-async--operation-settled operation) t
                (e-session-async--operation-storage-operation operation) nil
                (e-session-async--operation-state operation) nil)
          (e-session-async--fail-work-isolated
           (e-session-async--operation-work operation)
           '(e-session-storage-error "Session store reset"))))))
  store)

(defun e-session-async-teardown (store)
  "Clear STORE's optimistic session state during close or reset."
  (e-session-async-reset store)
  (remhash store e-session-async--states)
  t)

(provide 'e-session-async)

;;; e-session-async.el ends here

;;; e-session-async.el --- Optimistic session persistence service -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The session application service admits each bounded mutation directly to
;; the runtime store's one FIFO and returns request-scoped `e-work' values for
;; bounded reads.  SQLite remains authoritative for durable current state and
;; history.  Emacs retains only unsettled optimistic intents and, while
;; those intents exist, the detached selected-path base needed to construct
;; provider context without waiting for COMMIT.  Settlement retires that
;; in-flight state; it never installs a durable mirror or republishes a delta.

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
  ;; A selected-path query may overtake a subsequently admitted write.  Keep
  ;; that detached base only while this owner has unacknowledged mutations so
  ;; provider context can overlay those bounded intents without waiting for
  ;; COMMIT.  This is in-flight coordination, not a durable session mirror.
  (inflight-context-bases (make-hash-table :test 'equal))
  ;; A context-path SELECT is ordered before mutations admitted after its
  ;; submission, but its finished work may be consumed after those mutations
  ;; have already acknowledged and left `pending'.  Retain only the operations
  ;; crossing each live query cut so the detached result can apply them once.
  ;; Entries are removed when the result is consumed or the read fails.
  (context-query-cuts (make-hash-table :test 'eq))
  (suspects (make-hash-table :test 'equal)))

(cl-defstruct (e-session-async--operation
               (:constructor e-session-async--operation-create))
  state session-id result work storage-operation settled
  command before-submit query-work query-delta)

(cl-defstruct (e-session-async--read-operation
               (:constructor e-session-async--read-operation-create))
  "One request-scoped asynchronous session read.

The storage operation is deliberately opaque here.  The application service
  owns only the returned `e-work' and lets the SQLite adapter own runtime
request details and cancellation."
  store body transform work storage-operation settled)

(cl-defstruct (e-session-async--context-query-cut
               (:constructor e-session-async--context-query-cut-create))
  "Bounded mutations admitted after one detached context SELECT."
  session-id operations)

(cl-defstruct (e-session-async--chat-view-operation
               (:constructor e-session-async--chat-view-operation-create))
  "One request-scoped composition of reads needed by a Daily surface."
  store session-id limit work reads results settled)

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

(defconst e-session-async--chat-view-spec
  (e-work-spec-create
   :id "session-chat-view" :execution 'cooperative
   :interactive-policy 'async :owner 'e-session-async
   :runner #'e-session-async--run-chat-view)
  "Cooperative spec for the three-read persistent chat-view composition.")

(defconst e-session-async--context-path-spec
  (e-work-spec-create
   :id "session-context-path" :execution 'cooperative
   :interactive-policy 'async :owner 'e-session-async
   :runner (lambda (_handle _arguments _context) :deferred))
  "Deferred composition of one detached path plus in-flight mutations.")

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
  (if-let ((state (gethash store e-session-async--states)))
      (length (gethash session-id (e-session-async--state-pending state)))
    0))

(defun e-session-async-pending-p (store session-id)
  "Return non-nil when SESSION-ID has an admitted unsettled mutation."
  (> (e-session-async-pending-count store session-id) 0))

(defun e-session-async-prime-new-context-path (store query-state)
  "Retain QUERY-STATE as NEW session admission context while writes are pending.

QUERY-STATE is the domain-derived current row submitted with the atomic root
admission.  The retained value is an empty selected path used only to bridge
later optimistic setup commands; normal settled sessions always query SQLite."
  (let* ((session-id (plist-get query-state :session-id))
         (state (e-session-async--state store)))
    (unless (and (stringp session-id)
                 (null (plist-get query-state :messages)))
      (signal 'e-session-storage-error
              (list "Invalid new-session context seed" session-id)))
    (puthash
     (copy-sequence session-id)
     (list :session-id (copy-sequence session-id)
           :current-branch
           (copy-tree (plist-get query-state :current-branch) t)
           :metadata (copy-tree (plist-get query-state :metadata) t)
           :turn-options (copy-tree (plist-get query-state :turn-options) t)
           :current-head-id (plist-get query-state :current-head-id)
           :current-head-path-index 0
           :compaction nil :messages nil :message-path-indexes nil
           :context-records nil
           :record-limit 1 :byte-count 0 :byte-limit 0)
     (e-session-async--state-inflight-context-bases state))))

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
          (when-let ((request-id (plist-get properties :request-id)))
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

(defun e-session-async--pending-snapshot (store)
  "Return STORE's bounded unsettled commands grouped by session owner.

The result contains operation references only for the lifetime of one read.
Its size is bounded by admitted in-flight work; it is not a durable catalog or
session replica."
  (when-let ((state (gethash store e-session-async--states)))
    (let (snapshot)
      (maphash
       (lambda (session-id operations)
         (when operations
           (push (cons (copy-sequence session-id)
                       (copy-sequence operations))
                 snapshot)))
       (e-session-async--state-pending state))
      snapshot)))

(defun e-session-async--page-row-before-cursor-p (row cursor)
  "Return non-nil when ROW belongs after stable page CURSOR."
  (or (null cursor)
      (let ((updated-at (plist-get row :updated-at))
            (session-id (plist-get row :session-id))
            (cursor-updated-at (plist-get cursor :updated-at))
            (cursor-session-id (plist-get cursor :session-id)))
        (or (string-lessp updated-at cursor-updated-at)
            (and (equal updated-at cursor-updated-at)
                 (string-lessp session-id cursor-session-id))))))

(defun e-session-async--page-row-matches-p (row body)
  "Return non-nil when detached query ROW matches page request BODY."
  (and (not (plist-get row :deleted))
       (or (not (plist-get body :root-p))
           (plist-get row :root-p))
       (or (not (plist-member body :board-id))
           (equal (plist-get row :board-id) (plist-get body :board-id)))
       (or (not (plist-member body :principal))
           (equal (plist-get row :principal) (plist-get body :principal)))
       (e-session-async--page-row-before-cursor-p
        row (plist-get body :cursor))))

(defun e-session-async--page-row-newer-p (left right)
  "Return non-nil when LEFT sorts before RIGHT in a newest-first page."
  (let ((left-time (plist-get left :updated-at))
        (right-time (plist-get right :updated-at)))
    (if (equal left-time right-time)
        (string-lessp (plist-get right :session-id)
                      (plist-get left :session-id))
      (string-lessp right-time left-time))))

(defun e-session-async--operation-visible-to-page-p (operation)
  "Return non-nil when OPERATION is still an optimistic page mutation."
  (eq (plist-get (e-work-status
                  (e-session-async--operation-work operation))
                 :state)
      'started))

(defun e-session-async--overlay-query-page (page body snapshot)
  "Overlay SNAPSHOT's bounded in-flight commands onto detached PAGE.

PAGE remains a request result.  Complete SQLite rows replace one another by
session identity; no result is installed in process-wide or session-owned
state.  Commands already acknowledged before this SELECT settles are already
visible to SQLite and therefore are not applied twice."
  (let ((rows-by-id (make-hash-table :test 'equal))
        (limit (plist-get body :limit))
        (byte-limit (plist-get page :byte-limit)))
    (dolist (row (plist-get page :rows))
      (puthash (plist-get row :session-id) (copy-tree row t) rows-by-id))
    (dolist (owner snapshot)
      (let* ((session-id (car owner))
             (current (gethash session-id rows-by-id))
             (changed nil))
        (dolist (operation (cdr owner))
          (when (e-session-async--operation-visible-to-page-p operation)
            (setq current
                  (or (e-session-async--operation-query-delta operation)
                      (plist-get
                       (e-session-query-command-interpret
                        current (e-session-async--operation-command operation))
                       :query-delta))
                  changed t)))
        (when changed
          (if (plist-get current :deleted)
              (remhash session-id rows-by-id)
            (puthash session-id current rows-by-id)))))
    (let (candidates selected
          (bytes 0)
          byte-truncated)
      (maphash
       (lambda (_session-id row)
         (when (e-session-async--page-row-matches-p row body)
           (push row candidates)))
       rows-by-id)
      (setq candidates (sort candidates #'e-session-async--page-row-newer-p))
      (catch 'page-full
        (dolist (row candidates)
          (when (>= (length selected) limit)
            (throw 'page-full nil))
          (let ((row-bytes
                 (if byte-limit
                     (e-runtime-store-codec-measure-bounded row byte-limit)
                   0)))
            (when (and selected byte-limit (> (+ bytes row-bytes) byte-limit))
              (setq byte-truncated t)
              (throw 'page-full nil))
            (setq bytes (+ bytes row-bytes)
                  selected (append selected (list row))))))
      (let* ((more-p (or byte-truncated
                         (> (length candidates) (length selected))
                         (plist-get page :next)))
             (last-row (car (last selected)))
             (next (and more-p
                        (if last-row
                            (list :updated-at (plist-get last-row :updated-at)
                                  :session-id (plist-get last-row :session-id))
                          (copy-tree (plist-get page :next) t)))))
        (list :rows selected :next next :limit limit
              :byte-count (if byte-limit bytes (plist-get page :byte-count))
              :byte-limit byte-limit)))))

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
                    :limit (or limit 64)))
        (snapshot (e-session-async--pending-snapshot store)))
    (when cursor
      (setq body (append body (list :cursor cursor))))
    (when root-p
      (setq body (append body (list :root-p t))))
    (when (not (null board-id))
      (setq body (append body (list :board-id board-id))))
    (when (not (null principal))
      (setq body (append body (list :principal principal))))
    (e-session-async--start-read
     store body
     :transform
     (lambda (page)
       (e-session-async--overlay-query-page page body snapshot)))))

(defun e-session-async--context-path-apply-command (path command)
  "Return detached PATH with provider-relevant COMMAND applied once."
  (let* ((copy (copy-tree path t))
         (tag (e-session-aggregate-command-tag command))
         (arguments (e-session-aggregate-command-arguments command))
         (next-path-index
          (1+ (or (plist-get copy :current-head-path-index) -1))))
    (pcase tag
      ('append-message
       (let ((message (copy-tree (plist-get arguments :message) t)))
         (plist-put message :type 'message)
         (unless (plist-get message :id)
           (plist-put message :id
                      (e-session-aggregate-command-delta-id command)))
         (unless (plist-member message :parent-id)
           (plist-put message :parent-id (plist-get copy :current-head-id)))
         (unless (plist-member message :created-at)
           (plist-put message :created-at
                      (e-session-aggregate-command-timestamp command)))
         (plist-put copy :messages
                    (append (plist-get copy :messages) (list message))))
       (plist-put copy :message-path-indexes
                  (append (plist-get copy :message-path-indexes)
                          (list next-path-index))))
      ('session-info
       (pcase (plist-get arguments :field)
         ('metadata
          (plist-put copy :metadata
                     (copy-tree (plist-get arguments :value) t)))
         ('turn-options
          (plist-put copy :turn-options
                     (copy-tree (plist-get arguments :value) t)))
         ('current-branch
         (plist-put copy :current-branch
                     (copy-tree (plist-get arguments :value) t)))))
      ('context-generation
       (let* ((generation (plist-get arguments :generation))
              (record
               (if (e-context-lifetime-generation-p generation)
                   (e-context-lifetime-generation-record generation)
                 (copy-tree generation t))))
         (plist-put
          copy :context-records
          (append
           (plist-get copy :context-records)
           (list
            (list :path-index next-path-index
                  :record-type "context-generation"
                  :covered-boundary-index
                  (and (equal (plist-get record :covered-session-boundary)
                              (plist-get copy :current-head-id))
                       (plist-get copy :current-head-path-index))
                  :record (list :type "context-generation"
                                :context-record record)))))))
      ('context-curation-package
       (plist-put
        copy :context-records
        (append
         (plist-get copy :context-records)
         (list
          (list :path-index next-path-index
                :record-type "context-curation-package"
                :record
                (append (list :type "context-curation-package")
                        (copy-tree (plist-get arguments :package) t)))))))
      (_ nil))
    (plist-put copy :current-head-path-index next-path-index)
    ;; Command identity is fixed at admission, before the exact relational row
    ;; is queried.  The delta id is the aggregate entry id for the context-path
    ;; command families above, so later optimistic generations can name the
    ;; same covered boundary that SQLite will commit.
    (when (memq tag '(append-message append-activity
                      context-curation-response process-report branch-summary
                      compaction provider-anchor context-generation
                      session-info))
      (plist-put
       copy :current-head-id
       (if (eq tag 'append-message)
           (or (plist-get (plist-get arguments :message) :id)
               (e-session-aggregate-command-delta-id command))
         (e-session-aggregate-command-delta-id command))))
    copy))

(defun e-session-async--context-query-cut-remove (state cut-id)
  "Remove CUT-ID from STATE and return its crossing operations."
  (when-let* ((cut (gethash cut-id
                            (e-session-async--state-context-query-cuts state))))
    (remhash cut-id (e-session-async--state-context-query-cuts state))
    (e-session-async--context-query-cut-operations cut)))

(defun e-session-async--context-path-with-pending (state session-id path)
  "Return PATH overlaid with SESSION-ID's bounded unsettled commands."
  (let ((result (copy-tree path t)))
    (dolist (operation
             (gethash session-id (e-session-async--state-pending state)))
      (when (not (e-session-async--operation-settled operation))
        (setq result
              (e-session-async--context-path-apply-command
               result (e-session-async--operation-command operation)))))
    result))

(defun e-session-async--settled-context-path-work (session-id path)
  "Return a finished request-scoped work carrying detached PATH."
  (let ((work
         (e-work-prepare
          e-session-async--context-path-spec nil
          :context (list :domain-ref session-id
                         :work-kind 'session-context-path))))
    (e-work-finish work path)
    work))

(defun e-session-async-context-path-base (store session-id)
  "Return immediately with SESSION-ID's detached SQLite or in-flight base.

This prefetch intentionally does not overlay pending commands: a caller may
submit it before admitting a mutation and compose the result afterward."
  (let* ((state (e-session-async--state store))
         (bases (e-session-async--state-inflight-context-bases state))
         (base (gethash session-id bases)))
    (if base
        (e-session-async--settled-context-path-work
         session-id (copy-tree base t))
      (let* ((cut-id (make-symbol "session-context-query-cut"))
             (cut (e-session-async--context-query-cut-create
                   :session-id session-id))
             (_ (puthash cut-id cut
                         (e-session-async--state-context-query-cuts state)))
             (read
              (e-session-async--start-read
               store (list :op 'session-context-path
                           :session-id session-id)))
             (result
              (e-work-prepare
               e-session-async--context-path-spec nil
               :context (list :domain-ref session-id
                              :work-kind 'session-context-path))))
        (e-work-start-prepared result :arguments nil)
        (e-work-on-settle
         read
         (lambda (settled)
           (let ((status (e-work-status settled)))
             (pcase (plist-get status :state)
               ('finished
                (let ((path (copy-tree (plist-get status :result) t)))
                  (plist-put path :context-query-cut-id cut-id)
                  (if (e-session-async-pending-p store session-id)
                      (puthash (copy-sequence session-id)
                               (copy-tree path t) bases)
                    (remhash session-id bases))
                  (e-work-finish
                   result (copy-tree path t))))
               ('failed
                (e-session-async--context-query-cut-remove state cut-id)
                (e-work-fail result (plist-get status :error)))
               ('cancelled
                (e-session-async--context-query-cut-remove state cut-id)
                (e-work-cancel result))))))
        result))))

(defun e-session-async-context-path-overlay-pending (store session-id path)
  "Return detached PATH with SESSION-ID's current bounded mutations overlaid."
  (let* ((state (e-session-async--state store))
         (result (copy-tree path t))
         (cut-id (plist-get result :context-query-cut-id))
         (crossing
          (and cut-id
               (e-session-async--context-query-cut-remove state cut-id))))
    (cl-remf result :context-query-cut-id)
    ;; Every crossing operation was admitted after the SELECT entered the
    ;; global FIFO, so none can be present in PATH.  Apply it whether or not its
    ;; acknowledgement has already retired it from the pending owner queue.
    (dolist (operation crossing)
      (setq result
            (e-session-async--context-path-apply-command
             result (e-session-async--operation-command operation))))
    ;; Commands admitted before the cut, or after it was consumed, remain the
    ;; ordinary unsettled overlay.  Exclude crossing identities to avoid a
    ;; second application while their writes are still pending.
    (dolist (operation
             (gethash session-id (e-session-async--state-pending state)))
      (unless (or (e-session-async--operation-settled operation)
                  (memq operation crossing))
        (setq result
              (e-session-async--context-path-apply-command
               result (e-session-async--operation-command operation)))))
    result))

(defun e-session-async-context-path (store session-id)
  "Return immediately with SESSION-ID's effective detached provider path."
  (let* ((base-work (e-session-async-context-path-base store session-id))
         (result
          (e-work-prepare
           e-session-async--context-path-spec nil
           :context (list :domain-ref session-id
                          :work-kind 'session-context-path))))
    (e-work-start-prepared result :arguments nil)
    (e-work-on-settle
     base-work
     (lambda (settled)
       (let ((status (e-work-status settled)))
         (pcase (plist-get status :state)
           ('finished
            (e-work-finish
             result
             (e-session-async-context-path-overlay-pending
              store session-id (plist-get status :result))))
           ('failed (e-work-fail result (plist-get status :error)))
           ('cancelled (e-work-cancel result))))))
    result))

(defun e-session-async-visible-message-page
    (store session-id &optional limit)
  "Return immediately with SESSION-ID's newest visible message window."
  (e-session-async--start-read
   store (list :op 'session-visible-message-page :session-id session-id
               :limit (or limit 32))))

(cl-defun e-session-async-record-page
    (store session-id &key after limit record-type record-id record-identity
           parent-id)
  "Return immediately with one detached bounded journal page for SESSION-ID.

This is the history/inspection boundary.  AFTER is the stable journal-position
cursor returned as `:next' by the previous page.  The optional identity fields
are typed SQLite predicates, not filters over an Emacs-owned transcript."
  (let ((body (list :op 'session-record-page
                    :session-id session-id
                    :after (or after 0)
                    :limit (or limit 100))))
    (dolist (entry `((:record-type . ,record-type)
                     (:record-id . ,record-id)
                     (:record-identity . ,record-identity)
                     (:parent-id . ,parent-id)))
      (when (cdr entry)
        (setq body (append body (list (car entry) (cdr entry))))))
    (e-session-async--start-read store body)))

(defun e-session-async-header (store session-id)
  "Return immediately with SESSION-ID's bounded journal header work."
  (e-session-async--start-read
   store (list :op 'session-header :session-id session-id)))

(defun e-session-async--chat-view-session-result-p (value session-id)
  "Return non-nil when VALUE has the requested detached session identity."
  (and (proper-list-p value)
       (plist-member value :session-id)
       (equal (plist-get value :session-id) session-id)))

(defun e-session-async--chat-view-message-p (message)
  "Return non-nil when detached MESSAGE has a renderable bounded shape."
  (and (proper-list-p message)
       (plist-member message :id)
       (stringp (plist-get message :id))
       (> (string-bytes (plist-get message :id)) 0)
       (plist-member message :role)
       (memq (plist-get message :role)
             '(system user assistant tool tool-call))
       (plist-member message :content)))

(defun e-session-async--chat-view-result (operation)
  "Validate and compose OPERATION's detached metadata/association/messages."
  (let* ((session-id
          (e-session-async--chat-view-operation-session-id operation))
         (results (e-session-async--chat-view-operation-results operation))
         (metadata (gethash 'metadata results))
         (association (gethash 'association results))
         (messages-page (gethash 'messages results))
         (messages (and (listp messages-page)
                        (plist-get messages-page :messages))))
    (unless (and (e-session-async--chat-view-session-result-p
                  metadata session-id)
                 (e-session-async--chat-view-session-result-p
                  association session-id)
                 (plist-member association :board-id)
                 (listp messages-page)
                 (plist-member messages-page :messages)
                 (plist-member messages-page :truncated)
                 (memq (plist-get messages-page :truncated) '(nil t))
                 (listp messages)
                 (<= (length messages)
                     (e-session-async--chat-view-operation-limit operation)))
      (signal 'e-session-storage-error
              (list "Persistent chat view returned an invalid bounded shape"
                    session-id metadata association messages-page)))
    (dolist (message messages)
      (unless (e-session-async--chat-view-message-p message)
        (signal 'e-session-storage-error
                (list "Persistent chat view returned an invalid message"
                      session-id message))))
    (list :session-id session-id
          :metadata (copy-tree metadata t)
          :association (copy-tree association t)
          :messages (copy-tree messages t)
          :truncated (and (plist-get messages-page :truncated) t))))

(defun e-session-async--cancel-chat-view-reads (operation)
  "Cancel unsettled child reads owned by OPERATION."
  (dolist (read (e-session-async--chat-view-operation-reads operation))
    (unless (e-request-terminal-p (e-work-handle-lifecycle read))
      (ignore-errors (e-work-cancel read)))))

(defun e-session-async--settle-chat-view (operation result error)
  "Settle OPERATION exactly once with composed RESULT or ERROR."
  (unless (e-session-async--chat-view-operation-settled operation)
    (setf (e-session-async--chat-view-operation-settled operation) t)
    (let ((work (e-session-async--chat-view-operation-work operation)))
      (if error
          (progn
            (e-session-async--cancel-chat-view-reads operation)
            (e-work-fail
             work
             (if (and (consp error) (symbolp (car error)))
                 error
               (list 'e-session-storage-error
                     "Persistent chat view read failed" error))))
        (condition-case shape-error
            (e-work-finish work result)
          (error
           (e-work-fail
            work
            (list 'e-session-storage-error
                  "Persistent chat view composition failed" shape-error))))))))

(defun e-session-async--chat-view-child-settled (operation kind child)
  "Collect KIND CHILD and finish its parent when all reads settle."
  (unless (or (e-session-async--chat-view-operation-settled operation)
              (e-request-terminal-p
               (e-work-handle-lifecycle
                (e-session-async--chat-view-operation-work operation))))
    (let ((status (e-work-status child)))
      (pcase (plist-get status :state)
        ('finished
         (puthash kind (copy-tree (plist-get status :result) t)
                  (e-session-async--chat-view-operation-results operation))
         (when (= (hash-table-count
                   (e-session-async--chat-view-operation-results operation))
                  3)
           (condition-case err
               (e-session-async--settle-chat-view
                operation (e-session-async--chat-view-result operation) nil)
             (error
              (e-session-async--settle-chat-view operation nil err)))))
        ((or 'failed 'cancelled)
         (e-session-async--settle-chat-view
          operation nil
          (or (plist-get status :error)
              (list 'e-session-storage-error
                    "Persistent chat view read cancelled"))))))))

(defun e-session-async--run-chat-view (handle operation _context)
  "Start metadata, association, and visible-message reads for OPERATION."
  (setf (e-session-async--chat-view-operation-work operation) handle
        (e-session-async--chat-view-operation-results operation)
        (make-hash-table :test 'eq)
        (e-work-handle-cancel-function handle)
        (lambda (_handle)
          (setf (e-session-async--chat-view-operation-settled operation) t)
          (e-session-async--cancel-chat-view-reads operation)))
  (dolist (spec '((metadata . e-session-async-session-metadata)
                  (association . e-session-async-board-association)
                  (messages . e-session-async-visible-message-page)))
    (unless (e-session-async--chat-view-operation-settled operation)
      (let* ((kind (car spec))
             (reader (cdr spec))
             (store (e-session-async--chat-view-operation-store operation))
             (session-id
              (e-session-async--chat-view-operation-session-id operation))
             (child (if (eq kind 'messages)
                        (funcall reader
                                 store session-id
                                 (e-session-async--chat-view-operation-limit
                                  operation))
                      (funcall reader store session-id))))
        (push child (e-session-async--chat-view-operation-reads operation))
        (e-work-on-settle
         child
         (lambda (settled)
           (e-session-async--chat-view-child-settled
            operation kind settled))))))
  :deferred)

(cl-defun e-session-async-chat-view (store session-id &key (limit 32))
  "Return immediately with the bounded persistent chat-view composition."
  (unless (and (integerp limit) (> limit 0) (<= limit 64))
    (signal 'e-session-storage-error
            (list "Persistent chat view limit is outside its bound" limit)))
  (let* ((operation (e-session-async--chat-view-operation-create
                    :store store :session-id session-id :limit limit))
         (work (e-work-prepare
                e-session-async--chat-view-spec operation
                :context (list :domain-ref session-id
                               :work-kind 'session-chat-view))))
    (setf (e-session-async--chat-view-operation-work operation) work)
    (e-work-start-prepared work :arguments operation)
    work))

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
              (list "Session mutation intent queue is full"
                    :session-id session-id
                    :limit e-session-async-owner-pending-limit)))
    (puthash session-id
             (append current (list operation))
             pending)
    (maphash
     (lambda (_cut-id cut)
       (when (equal session-id
                    (e-session-async--context-query-cut-session-id cut))
         (setf (e-session-async--context-query-cut-operations cut)
               (append
                (e-session-async--context-query-cut-operations cut)
                (list operation)))))
     (e-session-async--state-context-query-cuts state))))

(defun e-session-async--remove-pending (operation)
  "Remove OPERATION once from its session-owned pending set."
  (when-let* ((state (e-session-async--operation-state operation))
              (session-id (e-session-async--operation-session-id operation)))
    (let* ((pending (e-session-async--state-pending state))
           (remaining (delq operation (gethash session-id pending))))
      (if remaining
          (puthash session-id remaining pending)
        (remhash session-id pending)))))

(defun e-session-async--owner-head-p (operation)
  "Return non-nil when OPERATION is first in its session intent FIFO."
  (let* ((state (e-session-async--operation-state operation))
         (session-id (e-session-async--operation-session-id operation)))
    (eq operation
        (car (gethash session-id
                      (e-session-async--state-pending state))))))

(defun e-session-async--start-next-owner-operation (state session-id)
  "Start SESSION-ID's next queued relational operation, if any."
  (when-let* ((next (car (gethash session-id
                                  (e-session-async--state-pending state)))))
    (e-session-async--start-relational-operation next)))

(defun e-session-async--fail-work-isolated (work cause)
  "Fail WORK with CAUSE without allowing arbitrary observers to gate cleanup."
  (condition-case nil
      (e-work-fail work cause)
    ((error quit) nil))
  work)

(defun e-session-async--relational-fail-request (operation error)
  "Fail relational OPERATION for request-local ERROR and advance its FIFO."
  (unless (e-session-async--operation-settled operation)
    (setf (e-session-async--operation-settled operation) t
          (e-session-async--operation-query-work operation) nil
          (e-session-async--operation-storage-operation operation) nil
          (e-session-async--operation-query-delta operation) nil)
    (let* ((state (e-session-async--operation-state operation))
           (session-id (e-session-async--operation-session-id operation))
           (work (e-session-async--operation-work operation)))
      (e-session-async--remove-pending operation)
      (e-session-async--start-next-owner-operation state session-id)
      (e-session-async--fail-work-isolated
       work (e-session-async--read-error error)))))

(defun e-session-async--relational-fail-owner (operation error)
  "Mark OPERATION's owner suspect after write ERROR and fail its queued work."
  (unless (e-session-async--operation-settled operation)
    (let* ((state (e-session-async--operation-state operation))
           (store (e-session-async--state-store state))
           (session-id (e-session-async--operation-session-id operation))
           (cause (e-session-async--note-suspect store session-id error))
           (queued (copy-sequence
                    (gethash session-id
                             (e-session-async--state-pending state)))))
      ;; Detach the complete owner queue before notifying any public work.
      ;; Other owners remain independently runnable in the shared transport.
      (remhash session-id (e-session-async--state-pending state))
      (remhash session-id
               (e-session-async--state-inflight-context-bases state))
      (dolist (current queued)
        (unless (e-session-async--operation-settled current)
          (setf (e-session-async--operation-settled current) t
                (e-session-async--operation-query-work current) nil
                (e-session-async--operation-storage-operation current) nil
                (e-session-async--operation-query-delta current) nil)
          (e-session-async--fail-work-isolated
           (e-session-async--operation-work current) (copy-tree cause)))))))

(defun e-session-async--relational-write-settled (operation _result error)
  "Settle one relational write OPERATION and advance its owner FIFO."
  (if error
      (e-session-async--relational-fail-owner operation error)
    (unless (e-session-async--operation-settled operation)
      (setf (e-session-async--operation-settled operation) t
            (e-session-async--operation-storage-operation operation) nil)
      (let* ((state (e-session-async--operation-state operation))
             (session-id (e-session-async--operation-session-id operation))
             (work (e-session-async--operation-work operation))
             (result (e-session-async--operation-result operation))
             (bases (e-session-async--state-inflight-context-bases state))
             (base (gethash session-id bases)))
        (setf (e-session-async--operation-result operation) nil
              (e-session-async--operation-query-delta operation) nil)
        ;; Advance a retained in-flight base by the acknowledged command
        ;; before removing it from the overlay set.  When the final mutation
        ;; settles, discard the base entirely.
        (when base
          (puthash session-id
                   (e-session-async--context-path-apply-command
                    base (e-session-async--operation-command operation))
                   bases))
        (e-session-async--remove-pending operation)
        (unless (e-session-async-pending-p
                 (e-session-async--state-store state) session-id)
          (remhash session-id bases))
        ;; Preserve owner ordering before arbitrary completion observers run.
        (e-session-async--start-next-owner-operation state session-id)
        (e-work-finish work (copy-tree result t))))))

(defun e-session-async--submit-derived-command (operation state)
  "Derive and submit OPERATION against detached current query STATE."
  (unless (e-session-async--operation-settled operation)
    (condition-case error
        (let* ((command (e-session-async--operation-command operation))
               (tag (e-session-aggregate-command-tag command))
               (delta (e-session-query-command-interpret state command))
               (record (plist-get delta :record))
               (query-delta (plist-get delta :query-delta))
               (continuity
                (when-let* ((before-submit
                             (e-session-async--operation-before-submit
                              operation)))
                  (funcall before-submit delta)))
               (body
                (if (eq tag 'delete)
                    (list :op 'session-delete
                          :session-id
                          (e-session-async--operation-session-id operation)
                          :query-delta query-delta)
                  (append
                   (list :op (if continuity
                                 'session-append-with-tool-transition
                               'session-append)
                         :session-id
                         (e-session-async--operation-session-id operation)
                         :record record :query-delta query-delta)
                   (when continuity (list :continuity continuity))))))
          (e-session-storage-validate-operation-body
           (e-session-async--state-store
            (e-session-async--operation-state operation))
           body)
          (setf (e-session-async--operation-result operation)
                (plist-get delta :result)
                (e-session-async--operation-query-delta operation)
                (copy-tree query-delta t))
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
       ;; Derivation and admission failures occur before a write crosses
       ;; SQLite, so they remain request-local.
       (e-session-async--relational-fail-request operation error)))))

(defun e-session-async--relational-state-settled (operation query-work)
  "Continue relational OPERATION after exact current-state QUERY-WORK."
  (unless (e-session-async--operation-settled operation)
    (setf (e-session-async--operation-query-work operation) nil)
    (let ((status (e-work-status query-work)))
      (pcase (plist-get status :state)
        ('finished
         (let ((state (plist-get status :result)))
           (if state
               (e-session-async--submit-derived-command operation state)
             (e-session-async--relational-fail-request
              operation
              (list 'e-session-missing
                    (e-session-async--operation-session-id operation))))))
        ((or 'failed 'cancelled)
         (e-session-async--relational-fail-request
          operation
          (or (plist-get status :error)
              '(e-session-storage-error "Session state query cancelled"))))))))

(defun e-session-async--start-relational-operation (operation)
  "Start OPERATION's exact-state query without blocking its caller."
  (unless (or (e-session-async--operation-settled operation)
              (e-session-async--operation-query-work operation)
              (e-session-async--operation-storage-operation operation))
    (if (eq (e-session-aggregate-command-tag
             (e-session-async--operation-command operation))
            'create)
        (e-session-async--submit-derived-command operation nil)
      (let ((query
             (e-session-async-query-state
              (e-session-async--state-store
               (e-session-async--operation-state operation))
              (e-session-async--operation-session-id operation))))
        (setf (e-session-async--operation-query-work operation) query)
        (e-work-on-settle
         query
         (lambda (settled)
           (e-session-async--relational-state-settled operation settled)))))))

(cl-defun e-session-async--submit-relational-command
    (store session-id tag arguments &key before-submit)
  "Queue one bounded relational TAG intent and return its work immediately."
  (condition-case error
      (let* ((command (e-session-aggregate-command-prepare
                       tag session-id arguments))
             (effective-id (e-session-aggregate-command-session-id command)))
        (if-let ((suspect (e-session-async-session-suspect store effective-id)))
            (e-session-async--failed-work effective-id suspect)
          (let* ((state (e-session-async--state store))
                 (operation
                  (e-session-async--operation-create
                   :state state :session-id effective-id :command command
                   :before-submit before-submit))
                 (work (e-session-async--start-work effective-id operation)))
            (condition-case admission-error
                (progn
                  (e-session-async--add-pending operation)
                  (when (e-session-async--owner-head-p operation)
                    (e-session-async--start-relational-operation operation))
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
  "Admit one SQLite-authoritative TAG to STORE's owner FIFO.

No durable session aggregate is read, installed, or retained.  WRITE-INDEX is
accepted for facade compatibility; relational query rows are updated in the
same SQLite transaction as the journal record."
  (ignore write-index)
  (if (eq tag 'board-message)
      ;; Current Board publication uses the Board storage service; retaining
      ;; the former session-aggregate copy would create a second authority.
      (e-session-async-unsupported-command session-id tag)
    (e-session-async--submit-relational-command
     store session-id tag arguments :before-submit before-submit)))

(defun e-session-async-unsupported-command (session-id name)
  "Return a terminal typed work for unsupported asynchronous command NAME."
  (e-session-async--failed-work
   session-id
   (list 'e-session-storage-command-error
         "Unsupported asynchronous session command" name)))

(defun e-session-async-reset (store)
  "Clear STORE's process-local pending and suspect session state."
  (when-let ((state (gethash store e-session-async--states)))
    (let (operations)
      ;; Detach every application-owned link before arbitrary work observers.
      (unwind-protect
          (maphash (lambda (_session-id pending)
                     (setq operations (nconc pending operations)))
                   (e-session-async--state-pending state))
        (clrhash (e-session-async--state-pending state))
        (clrhash (e-session-async--state-inflight-context-bases state))
        (clrhash (e-session-async--state-context-query-cuts state))
        (clrhash (e-session-async--state-suspects state)))
      (dolist (operation operations)
        (unless (e-session-async--operation-settled operation)
          (setf (e-session-async--operation-settled operation) t
                (e-session-async--operation-storage-operation operation) nil
                (e-session-async--operation-query-delta operation) nil
                (e-session-async--operation-result operation) nil
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

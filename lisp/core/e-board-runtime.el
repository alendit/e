;;; e-board-runtime.el --- Harness delivery adapter for board pickups -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This composition adapter connects registry participants to existing harness
;; sessions.  Board routing remains pure: this module receives only the frozen
;; pending pickups produced by the source board and delivers them through an
;; explicit port.

;;; Code:

(require 'cl-lib)
(require 'e-board-registry)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-request)
(require 'e-session)
(require 'e-work)

(define-error 'e-board-runtime-error "e board runtime error")
(define-error 'e-board-runtime-attachment-exists
  "e board runtime participant is already attached"
  'e-board-runtime-error)
(define-error 'e-board-runtime-session-busy
  "e board runtime session is busy"
  'e-board-runtime-error)
(define-error 'e-board-runtime-session-missing
  "e board runtime session is missing"
  'e-board-runtime-error)
(define-error 'e-board-runtime-instance-ineligible
  "e board runtime harness instance is not eligible for session attachment"
  'e-board-runtime-error)
(define-error 'e-board-runtime-resume-denied
  "e board runtime dormant session resume is not authorized"
  'e-board-runtime-error)
(define-error 'e-board-runtime-resume-version-conflict
  "e board runtime dormant session access version is stale"
  'e-board-runtime-error)
(define-error 'e-board-runtime-control-committed
  "e board runtime control request already committed"
  'e-board-runtime-error)
(define-error 'e-board-runtime-admission-closed
  "e board runtime admission is closed"
  'e-board-runtime-error)
(define-error 'e-board-runtime-quiescence-active
  "e board runtime quiescence request is already active"
  'e-board-runtime-error)
(define-error 'e-board-runtime-producer-disabled
  "e board runtime producer has no current live binding"
  'e-board-runtime-error)

(cl-defstruct (e-board-runtime-admission-token
               (:constructor e-board-runtime--admission-token-create))
  "Opaque authority to reopen one exact closed admission epoch."
  epoch)

(cl-defstruct (e-board-runtime-quiescence
               (:constructor e-board-runtime--quiescence-create))
  "One controlled process-quiescence request and its admission authority."
  request admission-token)

(cl-defstruct (e-board-runtime-producer-binding
               (:constructor e-board-runtime--producer-binding-create))
  "Runtime-scoped authority for one trusted application producer."
  id board-id board epoch tags attributes next-sequence client requester state)

(cl-defstruct (e-board-runtime-producer-publication
               (:constructor e-board-runtime--producer-publication-create))
  "One source-key-frozen producer publication attempt."
  binding binding-epoch source-key kind tags attributes to mode content reference
  on-settle state publication pending-delivery-ids terminal-results error)

(defvar e-board-runtime--admission-open-p t
  "Non-nil while public board-runtime roots may be admitted.")

(defvar e-board-runtime--admission-epoch 0
  "Monotonic process-local board-runtime admission epoch.")

(defvar e-board-runtime--quiescence-current nil
  "The one active controlled process-quiescence request, if any.")

(defun e-board-runtime-admission-state ()
  "Return the current bounded board-runtime admission state."
  (list :state (if e-board-runtime--admission-open-p 'open 'closed)
        :epoch e-board-runtime--admission-epoch))

(defun e-board-runtime-close-admission ()
  "Close admission for new public board-runtime roots in O(1).
Return the opaque token that alone may reopen this exact closed epoch.
Operations accepted before this commit may continue to completion."
  (unless e-board-runtime--admission-open-p
    (signal 'e-board-runtime-admission-closed
            (list e-board-runtime--admission-epoch)))
  (setq e-board-runtime--admission-open-p nil)
  (e-board-runtime--admission-token-create
   :epoch (cl-incf e-board-runtime--admission-epoch)))

(defun e-board-runtime-reopen-admission (token)
  "Reopen the closed board-runtime admission epoch authorized by TOKEN."
  (unless (e-board-runtime-admission-token-p token)
    (signal 'wrong-type-argument
            (list 'e-board-runtime-admission-token-p token)))
  (unless (and (not e-board-runtime--admission-open-p)
               (= (e-board-runtime-admission-token-epoch token)
                  e-board-runtime--admission-epoch))
    (signal 'e-board-runtime-error
            (list "Stale board runtime admission token"
                  (e-board-runtime-admission-token-epoch token)
                  e-board-runtime--admission-epoch)))
  (setq e-board-runtime--admission-open-p t)
  (e-board-runtime-admission-state))

(defun e-board-runtime--require-admission ()
  "Reject a new public runtime root while admission is closed."
  (unless e-board-runtime--admission-open-p
    (signal 'e-board-runtime-admission-closed
            (list e-board-runtime--admission-epoch))))

(defvar e-board-runtime--attachments (make-hash-table :test 'equal)
  "Live runtime attachments keyed by board and participant identity.")

(defvar e-board-runtime--session-attachments (make-hash-table :test 'equal)
  "Live attachments keyed by stable session attachment identity.")

(defvar e-board-runtime--endpoint-attachments (make-hash-table :test 'equal)
  "Live attachments keyed by concrete harness object and session identity.")

(defvar e-board-runtime--invocations (make-hash-table :test 'equal)
  "Exact invocation effect targets owned by their original attachment.")

(defvar e-board-runtime--producer-bindings (make-hash-table :test 'equal)
  "Current runtime-scoped trusted producer bindings by producer id.")

(defvar e-board-runtime--producer-inputs (make-hash-table :test 'equal)
  "Nonterminal producer work items keyed by retained input message id.")

(defvar e-board-runtime--producer-deliveries (make-hash-table :test 'equal)
  "Producer work items keyed by their frozen delivery ids.")

(defvar e-board-runtime--producer-turns (make-hash-table :test 'equal)
  "Producer work items keyed by board, participant, and source turn.")

(defvar e-board-runtime--producer-epoch 0
  "Monotonic process-local producer binding epoch.")

(defvar e-board-runtime--producer-head nil)
(defvar e-board-runtime--producer-tail nil)
(defvar e-board-runtime--producer-drain-scheduled nil)
(defvar e-board-runtime--producer-scheduler nil)

(defconst e-board-runtime-producer-drain-limit 16
  "Maximum trusted producer facts applied in one scheduled drain.")

(defvar e-board-runtime--control-sequence 0
  "Process-local sequence for board runtime control request identities.")

(defconst e-board-runtime-deferred-hook-drain-limit 16
  "Maximum deferred carrier hooks the private runtime starts per drain.")

(defvar e-board-runtime--deferred-hook-head nil
  "Head cell of the FIFO of generation-fenced deferred carrier hooks.")

(defvar e-board-runtime--deferred-hook-tail nil
  "Tail cell of the FIFO of generation-fenced deferred carrier hooks.")

(defvar e-board-runtime--deferred-hook-drain-scheduled nil
  "Non-nil while one deferred-hook runtime drain has been scheduled.")

(defvar e-board-runtime--deferred-hook-generation 0
  "Current private runtime generation for deferred carrier hook receipts.")

(defvar e-board-runtime--work-activity-mailboxes (make-hash-table :test 'equal)
  "Latest bounded activity capture for each board-enrolled work handle.")

(defconst e-board-runtime-activity-drain-limit 16
  "Maximum latest-value work activity mailboxes published per drain.")

(defvar e-board-runtime--pending-activity-head nil
  "Head cell of work ids whose latest activity mailbox needs a flush.")

(defvar e-board-runtime--pending-activity-tail nil
  "Tail cell of work ids whose latest activity mailbox needs a flush.")

(defvar e-board-runtime--pending-activity-set (make-hash-table :test 'equal)
  "Deduplication set for scheduled work activity mailbox flushes.")

(defvar e-board-runtime--activity-drain-scheduled nil
  "Non-nil while the runtime has one activity mailbox drain pending.")

(defconst e-board-runtime-pickup-drain-limit 16
  "Maximum frozen pickup attempts the private runtime starts per drain.")

(defconst e-board-runtime-pickup-retry-limit 3
  "Maximum proven-uncommitted delivery attempts before visible failure.")

(defconst e-board-runtime--visible-harness-activity-types
  '(turn-started provider-request-started provider-request-finished
    tool-started tool-finished action-started action-finished action-failed
    turn-steered compaction-started compaction-finished compaction-failed)
  "Harness lifecycle edges that are safe to expose as board activity.
Raw event payloads and high-frequency reasoning or progress edges stay outside
the board transcript.  Terminal events use their dedicated publisher below.")

(defvar e-board-runtime--pending-pickup-head nil
  "Head cell of the FIFO queue of pending board pickup identities.")

(defvar e-board-runtime--pending-pickup-tail nil
  "Tail cell of the FIFO queue of pending board pickup identities.")

(defvar e-board-runtime--pending-pickup-set (make-hash-table :test 'equal)
  "Deduplication set for runtime pickup identities awaiting an attempt.")

(defvar e-board-runtime--pickup-drain-scheduled nil
  "Non-nil while the runtime has one pickup-drain timer pending.")

(cl-defstruct (e-board-runtime-attachment
               (:constructor e-board-runtime-attachment--create)
               (:conc-name e-board-runtime-attachment-))
  board participant harness session-id delivery-function subscription activity-sequence generation
  turn-activity turn-tags turn-delivery-ids instance-id instance-catalog-generation harness-id harness-object-generation
  session-store-id endpoint-token state reconciliation)

(cl-defstruct (e-board-runtime-reconciliation
               (:constructor e-board-runtime-reconciliation--create)
               (:conc-name e-board-runtime-reconciliation-))
  kind request attachment requester target state scheduled)

(cl-defstruct (e-board-runtime-endpoint-token
               (:constructor e-board-runtime-endpoint-token--create)
               (:conc-name e-board-runtime-endpoint-token-))
  harness-id harness-object-generation session-store-id session-id)

(cl-defstruct (e-board-runtime-turn-activity
               (:constructor e-board-runtime-turn-activity--create)
               (:conc-name e-board-runtime-turn-activity-))
  provider-seen provider-started-at tool-count action-count)

(cl-defstruct (e-board-runtime-invocation
               (:constructor e-board-runtime-invocation--create)
               (:conc-name e-board-runtime-invocation-))
  target attachment attachment-generation endpoint-token composite-generation
  callback state)

(defvar e-board-runtime--unsettled-control-count 0
  "Number of nonterminal board-runtime control requests.")

(defvar e-board-runtime--unsettled-invocation-count 0
  "Number of open or applying exact invocation targets.")

(defvar e-board-runtime--unsettled-deferred-hook-count 0
  "Number of queued deferred carrier hook receipts.")

(defvar e-board-runtime--unsettled-producer-count 0
  "Number of accepted producer publications not yet applied.")

(defvar e-board-runtime--unsettled-generation 0
  "Monotonic generation of owner-local unsettled runtime state.")

(defvar e-board-runtime--unsettled-change-function nil
  "Private hard-bounded callback for owner-local unsettled transitions.")

(defvar e-board-runtime--unsettled-change-functions nil
  "Hard-bounded observers of board-runtime unsettled transitions.")

(defun e-board-runtime-unsettled-state ()
  "Return a constant-time snapshot of board-runtime unsettled state."
  (list :generation e-board-runtime--unsettled-generation
        :control-requests e-board-runtime--unsettled-control-count
        :invocations e-board-runtime--unsettled-invocation-count
        :deferred-hooks e-board-runtime--unsettled-deferred-hook-count
        :producer-items e-board-runtime--unsettled-producer-count
        :activity-mailboxes
        (hash-table-count e-board-runtime--pending-activity-set)
        :pickup-attempts
        (hash-table-count e-board-runtime--pending-pickup-set)))

(defun e-board-runtime--unsettled-changed ()
  "Record and publish one owner-local unsettled transition."
  (cl-incf e-board-runtime--unsettled-generation)
  (when e-board-runtime--unsettled-change-function
    (funcall e-board-runtime--unsettled-change-function
             (e-board-runtime-unsettled-state)))
  (run-hook-with-args 'e-board-runtime--unsettled-change-functions
                      (e-board-runtime-unsettled-state)))

(defun e-board-runtime--adjust-unsettled-count (class delta)
  "Adjust runtime unsettled CLASS by DELTA and publish its transition."
  (let ((value
         (pcase class
           ('control
            (cl-incf e-board-runtime--unsettled-control-count delta))
           ('invocation
            (cl-incf e-board-runtime--unsettled-invocation-count delta))
           ('deferred-hook
            (cl-incf e-board-runtime--unsettled-deferred-hook-count delta))
           ('producer
            (cl-incf e-board-runtime--unsettled-producer-count delta))
           (_
            (signal 'e-board-runtime-error
                    (list "Unknown unsettled runtime class" class))))))
    (when (< value 0)
      (signal 'e-board-runtime-error
              (list "Negative unsettled runtime count" class value)))
    (e-board-runtime--unsettled-changed)
    value))

(defun e-board-runtime--producer-binding-current-p (binding)
  "Return non-nil when BINDING still names its exact active board generation."
  (and (e-board-runtime-producer-binding-p binding)
       (eq (e-board-runtime-producer-binding-state binding) 'active)
       (eq (gethash (e-board-runtime-producer-binding-id binding)
                    e-board-runtime--producer-bindings)
           binding)
       (let ((registered
              (ignore-errors
                (e-board-registry-get
                 (e-board-runtime-producer-binding-board-id binding)))))
         (and (eq registered
                  (e-board-runtime-producer-binding-board binding))
              (eq (e-board-registry-board-state registered) 'active)))))

(defun e-board-runtime-producer-binding-live-p (binding)
  "Return non-nil when BINDING is current live runtime producer authority."
  (e-board-runtime--producer-binding-current-p binding))

(cl-defun e-board-runtime-producer-bind
    (producer-id board &key tags attributes)
  "Explicitly bind trusted PRODUCER-ID to live BOARD for this runtime only.
Persisting BOARD's id does not recreate this authority after restart; the
producer remains disabled until an owner supplies the exact live board again."
  (e-board-runtime--require-admission)
  (unless (and producer-id (e-board-registry-board-p board)
               (eq (e-board-registry-board-state board) 'active)
               (eq (ignore-errors
                     (e-board-registry-get (e-board-registry-board-id board)))
                   board))
    (signal 'e-board-runtime-producer-disabled (list producer-id 'missing-board)))
  (when-let ((old (gethash producer-id e-board-runtime--producer-bindings)))
    (setf (e-board-runtime-producer-binding-state old) 'replaced)
    (ignore-errors
      (e-board-registry-detach-client
       (e-board-runtime-producer-binding-board old)
       (e-board-registry-client-id
        (e-board-runtime-producer-binding-client old)))))
  (let* ((client
          (e-board-registry-attach-client
           board :author (format "producer:%s" producer-id)
           :principal (e-board-registry-board-principal board)))
         (requester
          (e-board-registry-client-requester-context
           board (e-board-registry-client-id client)))
         (binding
         (e-board-runtime--producer-binding-create
          :id producer-id
          :board-id (e-board-registry-board-id board)
          :board board
          :epoch (cl-incf e-board-runtime--producer-epoch)
          :tags (copy-tree tags)
          :attributes (copy-tree attributes)
          :next-sequence 0
          :client client
          :requester requester
          :state 'active)))
    (puthash producer-id binding e-board-runtime--producer-bindings)
    binding))

(defun e-board-runtime-producer-disable (binding)
  "Disable current producer BINDING and reject its later timer callbacks."
  (when (e-board-runtime-producer-binding-p binding)
    (when (eq (gethash (e-board-runtime-producer-binding-id binding)
                       e-board-runtime--producer-bindings)
              binding)
      (remhash (e-board-runtime-producer-binding-id binding)
               e-board-runtime--producer-bindings))
    (ignore-errors
      (e-board-registry-detach-client
       (e-board-runtime-producer-binding-board binding)
       (e-board-registry-client-id
        (e-board-runtime-producer-binding-client binding))))
    (setf (e-board-runtime-producer-binding-state binding) 'disabled))
  binding)

(defun e-board-runtime--schedule-producer-drain ()
  "Schedule one bounded producer publication drain."
  (unless e-board-runtime--producer-drain-scheduled
    (setq e-board-runtime--producer-drain-scheduled t)
    (let ((callback #'e-board-runtime-drain-producers))
      (if e-board-runtime--producer-scheduler
          (funcall e-board-runtime--producer-scheduler callback)
        (run-at-time 0 nil callback)))))

(defun e-board-runtime--enqueue-producer-publication (item)
  "Accept source-key-frozen producer publication ITEM."
  (let ((cell (list item)))
    (if e-board-runtime--producer-tail
        (setcdr e-board-runtime--producer-tail cell)
      (setq e-board-runtime--producer-head cell))
    (setq e-board-runtime--producer-tail cell))
  (setf (e-board-runtime-producer-publication-state item) 'queued)
  (e-board-runtime--adjust-unsettled-count 'producer 1)
  (e-board-runtime--schedule-producer-drain)
  item)

(cl-defun e-board-runtime-producer-publish-fact
    (binding &key tags attributes content reference)
  "Accept one observation-only fact from current producer BINDING.
The returned item freezes its source key before scheduling so a retry is
idempotent.  Zero matching subscriptions never creates a participant or turn."
  (e-board-runtime--require-admission)
  (unless (e-board-runtime--producer-binding-current-p binding)
    (signal 'e-board-runtime-producer-disabled (list binding 'stale-binding)))
  (let ((item
         (e-board-runtime--producer-publication-create
          :binding binding
          :binding-epoch (e-board-runtime-producer-binding-epoch binding)
          :source-key
          (list (e-board-runtime-producer-binding-id binding)
                (e-board-runtime-producer-binding-epoch binding)
                (cl-incf (e-board-runtime-producer-binding-next-sequence binding)))
          :kind 'fact
          :tags (append (copy-tree (e-board-runtime-producer-binding-tags binding))
                        (copy-tree tags))
          :attributes
          (append (copy-tree (e-board-runtime-producer-binding-attributes binding))
                  (copy-tree attributes))
          :content content :reference reference :state 'created)))
    (e-board-runtime--enqueue-producer-publication item)))

(cl-defun e-board-runtime-producer-publish-input
    (binding &key tags attributes to (mode 'inject) content reference on-settle)
  "Accept one dispatchable work input from current producer BINDING.
ON-SETTLE receives a terminal plist after routing and all causally identified
participant turns settle.  An unrouted input settles visibly as `unrouted'."
  (e-board-runtime--require-admission)
  (unless (e-board-runtime--producer-binding-current-p binding)
    (signal 'e-board-runtime-producer-disabled (list binding 'stale-binding)))
  (e-board-runtime--enqueue-producer-publication
   (e-board-runtime--producer-publication-create
    :binding binding
    :binding-epoch (e-board-runtime-producer-binding-epoch binding)
    :source-key
    (list (e-board-runtime-producer-binding-id binding)
          (e-board-runtime-producer-binding-epoch binding)
          (cl-incf (e-board-runtime-producer-binding-next-sequence binding)))
    :kind 'input
    :tags (append (copy-tree (e-board-runtime-producer-binding-tags binding))
                  (copy-tree tags))
    :attributes
    (append (copy-tree (e-board-runtime-producer-binding-attributes binding))
            (copy-tree attributes))
    :to to :mode mode :content content :reference reference
    :on-settle on-settle :state 'created)))

(defun e-board-runtime-producer-retry (item)
  "Retry failed producer publication ITEM with its original source key."
  (unless (and (e-board-runtime-producer-publication-p item)
               (eq (e-board-runtime-producer-publication-state item) 'failed)
               (e-board-runtime--producer-binding-current-p
                (e-board-runtime-producer-publication-binding item)))
    (signal 'e-board-runtime-producer-disabled (list item 'not-retryable)))
  (setf (e-board-runtime-producer-publication-error item) nil)
  (e-board-runtime--enqueue-producer-publication item))

(defun e-board-runtime-producer-cancel (item)
  "Cancel trusted producer work ITEM without accepting new work."
  (unless (and (e-board-runtime-producer-publication-p item)
               (eq (e-board-runtime-producer-publication-kind item) 'input))
    (signal 'wrong-type-argument
            (list 'e-board-runtime-producer-publication-p item)))
  (pcase (e-board-runtime-producer-publication-state item)
    ((or 'created 'queued)
     ;; The bounded producer drain owns retirement of the queued unsettled slot.
     (setf (e-board-runtime-producer-publication-state item) 'cancelled))
    ((or 'published 'dispatched)
     (let* ((binding (e-board-runtime-producer-publication-binding item))
            (source
             (e-board-registry-board-source-board
              (e-board-runtime-producer-binding-board binding)))
            (publication
             (e-board-runtime-producer-publication-publication item)))
       (when publication
         (e-board-cancel-input-routing
          source (e-board-message-id (e-board-publication-message publication))
          'producer-cancelled))
       (dolist (delivery-id
                (e-board-runtime-producer-publication-pending-delivery-ids item))
         (when (e-board-pickup source delivery-id)
           (e-board-cancel-pickup source delivery-id 'producer-cancelled)))
       (e-board-runtime--settle-producer-input item 'cancelled)))
    (_ nil))
  item)

(defun e-board-runtime--settle-producer-input (item status &optional payload)
  "Settle producer work ITEM once with STATUS and optional PAYLOAD."
  (unless (memq (e-board-runtime-producer-publication-state item)
                '(done failed cancelled unrouted))
    (setf (e-board-runtime-producer-publication-state item) status)
    (when-let* ((publication
                 (e-board-runtime-producer-publication-publication item))
                (message (e-board-publication-message publication)))
      (remhash (e-board-message-id message) e-board-runtime--producer-inputs))
    (dolist (delivery-id
             (e-board-runtime-producer-publication-pending-delivery-ids item))
      (remhash delivery-id e-board-runtime--producer-deliveries))
    (e-board-runtime--adjust-unsettled-count 'producer -1)
    (when-let ((callback
                (e-board-runtime-producer-publication-on-settle item)))
      (apply callback (append (list :status status) payload)))))

(defun e-board-runtime--producer-routing-finished (board message-id pickup-ids)
  "Advance producer work MESSAGE-ID after BOARD routing produced PICKUP-IDS."
  (when-let ((item (gethash message-id e-board-runtime--producer-inputs)))
    (let* ((source (e-board-registry-board-source-board board))
           (message (e-board-message source message-id)))
      (if (null pickup-ids)
          (e-board-runtime--settle-producer-input
           item 'unrouted
           (list :reason (e-board-message-unrouted-reason message)
                 :message-id message-id))
        (setf (e-board-runtime-producer-publication-state item) 'dispatched
              (e-board-runtime-producer-publication-pending-delivery-ids item)
              (copy-tree pickup-ids))
        (dolist (delivery-id pickup-ids)
          (puthash delivery-id item e-board-runtime--producer-deliveries))))))

(defun e-board-runtime--producer-delivery-terminal (item delivery-id status payload)
  "Record one producer ITEM DELIVERY-ID terminal STATUS and PAYLOAD."
  (remhash delivery-id e-board-runtime--producer-deliveries)
  (setf (e-board-runtime-producer-publication-pending-delivery-ids item)
        (delete delivery-id
                (e-board-runtime-producer-publication-pending-delivery-ids item))
        (e-board-runtime-producer-publication-terminal-results item)
        (cons (list :delivery-id delivery-id :status status :payload payload)
              (e-board-runtime-producer-publication-terminal-results item)))
  (unless (e-board-runtime-producer-publication-pending-delivery-ids item)
    (let* ((results (nreverse
                     (e-board-runtime-producer-publication-terminal-results item)))
           (statuses (mapcar (lambda (result) (plist-get result :status)) results))
           (final (cond ((memq 'failed statuses) 'failed)
                        ((memq 'cancelled statuses) 'cancelled)
                        (t 'done))))
      (e-board-runtime--settle-producer-input
       item final (list :results results)))))

(defun e-board-runtime-drain-producers ()
  "Apply one bounded page of accepted trusted producer publications."
  (setq e-board-runtime--producer-drain-scheduled nil)
  (let ((remaining e-board-runtime-producer-drain-limit))
    (while (and (> remaining 0) e-board-runtime--producer-head)
      (let ((item (pop e-board-runtime--producer-head)))
        (unless e-board-runtime--producer-head
          (setq e-board-runtime--producer-tail nil))
        (unwind-protect
            (if (eq (e-board-runtime-producer-publication-state item) 'cancelled)
                nil
            (condition-case err
                (let ((binding
                       (e-board-runtime-producer-publication-binding item)))
                  (unless (and (e-board-runtime--producer-binding-current-p binding)
                               (= (e-board-runtime-producer-publication-binding-epoch item)
                                  (e-board-runtime-producer-binding-epoch binding)))
                    (signal 'e-board-runtime-producer-disabled
                            (list binding 'stale-drain)))
                  (let ((publication
                         (if (eq (e-board-runtime-producer-publication-kind item)
                                 'input)
                             (e-board-runtime--post-client-input
                              (e-board-runtime-producer-binding-board binding)
                              :author
                              (format "producer:%s"
                                      (e-board-runtime-producer-binding-id binding))
                              :requester
                              (e-board-runtime-producer-binding-requester binding)
                              :tags (e-board-runtime-producer-publication-tags item)
                              :attributes
                              (e-board-runtime-producer-publication-attributes item)
                              :to (e-board-runtime-producer-publication-to item)
                              :mode (e-board-runtime-producer-publication-mode item)
                              :content (e-board-runtime-producer-publication-content item)
                              :reference (e-board-runtime-producer-publication-reference item)
                              :source-input-key
                              (e-board-runtime-producer-publication-source-key item))
                           (e-board-post-fact
                            (e-board-registry-board-source-board
                             (e-board-runtime-producer-binding-board binding))
                            :author
                            (format "producer:%s"
                                    (e-board-runtime-producer-binding-id binding))
                            :tags (e-board-runtime-producer-publication-tags item)
                            :attributes
                            (e-board-runtime-producer-publication-attributes item)
                            :content (e-board-runtime-producer-publication-content item)
                            :reference (e-board-runtime-producer-publication-reference item)
                            :source-fact-key
                            (e-board-runtime-producer-publication-source-key item)))))
                    (setf (e-board-runtime-producer-publication-publication item)
                          publication
                          (e-board-runtime-producer-publication-state item) 'published)
                    (when (eq (e-board-runtime-producer-publication-kind item) 'input)
                      (puthash
                       (e-board-message-id (e-board-publication-message publication))
                       item e-board-runtime--producer-inputs))))
              (error
               (setf (e-board-runtime-producer-publication-error item) err
                     (e-board-runtime-producer-publication-state item) 'failed))))
          (unless (and (eq (e-board-runtime-producer-publication-kind item) 'input)
                       (eq (e-board-runtime-producer-publication-state item)
                           'published))
            (e-board-runtime--adjust-unsettled-count 'producer -1))))
      (cl-decf remaining))
    (when e-board-runtime--producer-head
      (e-board-runtime--schedule-producer-drain))))

(defconst e-board-runtime--quiescence-change-hooks
  '(e-board-runtime--unsettled-change-functions
    e-harness--aggregate-unsettled-change-functions
    e-work--unsettled-change-functions
    e-board-registry--unsettled-change-functions
    e-session--unsettled-change-functions)
  "Owner-transition hooks observed by controlled quiescence requests.")

(defun e-board-runtime--quiescence-sources ()
  "Return all constant-time process-local unsettled source projections."
  (list :runtime (e-board-runtime-unsettled-state)
        :harnesses (e-harness-aggregate-unsettled-state)
        :work (e-work-unsettled-state)
        :boards (e-board-registry-unsettled-state)
        :persistence (e-session-persistence-unsettled-state)))

(defun e-board-runtime--quiescence-blockers (sources)
  "Return the nonzero unsettled counters from SOURCES."
  (let (blockers)
    (dolist (source '(:runtime :harnesses :work :boards :persistence))
      (let ((state (plist-get sources source)))
        (while state
          (let ((key (pop state))
                (value (pop state)))
            (unless (eq key :generation)
              (when (and (integerp value) (> value 0))
                (push (cons (intern (format "%s.%s" source key)) value)
                      blockers)))))))
    (nreverse blockers)))

(defun e-board-runtime--quiescence-unsubscribe ()
  "Remove the controlled quiescence transition observer from all owners."
  (dolist (hook e-board-runtime--quiescence-change-hooks)
    (remove-hook hook #'e-board-runtime--quiescence-source-changed)))

(defun e-board-runtime--quiescence-cleanup (quiescence)
  "Retire transition observation for QUIESCENCE without reopening admission."
  (when (eq quiescence e-board-runtime--quiescence-current)
    (setq e-board-runtime--quiescence-current nil)
    (e-board-runtime--quiescence-unsubscribe)))

(defun e-board-runtime--quiescence-evaluate (quiescence)
  "Reevaluate QUIESCENCE from bounded owner projections."
  (when (and (eq quiescence e-board-runtime--quiescence-current)
             (not e-board-runtime--admission-open-p)
             (= (e-board-runtime-admission-token-epoch
                 (e-board-runtime-quiescence-admission-token quiescence))
                e-board-runtime--admission-epoch))
    (let* ((request (e-board-runtime-quiescence-request quiescence))
           (sources (e-board-runtime--quiescence-sources))
           (blockers (e-board-runtime--quiescence-blockers sources)))
      (if blockers
          (setf (e-request-lifecycle-progress request)
                (list :state 'draining
                      :epoch e-board-runtime--admission-epoch
                      :blockers blockers))
        (e-request-finish
         request
         (list :state 'quiescent
               :epoch e-board-runtime--admission-epoch
               :sources sources))))))

(defun e-board-runtime--quiescence-source-changed (&rest _ignored)
  "Reevaluate the current quiescence request after an owner transition."
  (when e-board-runtime--quiescence-current
    (e-board-runtime--quiescence-evaluate
     e-board-runtime--quiescence-current)))

(defun e-board-runtime-request-quiescence ()
  "Close public-root admission and return a controlled quiescence request.
The request observes only owner-maintained O(1) counters and settles from their
transition notifications.  Settlement leaves admission closed; callers must
retain the returned admission token and reopen it explicitly when appropriate."
  (when e-board-runtime--quiescence-current
    (signal 'e-board-runtime-quiescence-active nil))
  (let* ((token (e-board-runtime-close-admission))
         quiescence
         (request
          (e-request-lifecycle-create
           :id (format "runtime-quiescence-%d"
                       (e-board-runtime-admission-token-epoch token))
           :owner 'e-board-runtime
           :generation (e-board-runtime-admission-token-epoch token)
           :state 'created
           :cancel-function
           (lambda (_request)
             (e-board-runtime--quiescence-cleanup quiescence))
           :cleanup-trigger
           (lambda (_request)
             (e-board-runtime--quiescence-cleanup quiescence)))))
    (setq quiescence
          (e-board-runtime--quiescence-create
           :request request :admission-token token))
    (setq e-board-runtime--quiescence-current quiescence)
    (dolist (hook e-board-runtime--quiescence-change-hooks)
      (add-hook hook #'e-board-runtime--quiescence-source-changed))
    (e-request-start request
                     (list :epoch
                           (e-board-runtime-admission-token-epoch token)))
    (e-board-runtime--quiescence-evaluate quiescence)
    quiescence))

(defun e-board-runtime--track-control-request (request)
  "Count REQUEST until its first terminal transition."
  (let ((cleanup (e-request-lifecycle-cleanup-trigger request)))
    (e-board-runtime--adjust-unsettled-count 'control 1)
    (setf (e-request-lifecycle-cleanup-trigger request)
          (lambda (settled)
            (unwind-protect
                (when cleanup
                  (funcall cleanup settled))
              (e-board-runtime--adjust-unsettled-count 'control -1)))))
  request)

(defun e-board-runtime--drop-invocation (target)
  "Remove TARGET and retire it from unsettled accounting when necessary."
  (when-let ((invocation (gethash target e-board-runtime--invocations)))
    (when (memq (e-board-runtime-invocation-state invocation) '(open applying))
      (e-board-runtime--adjust-unsettled-count 'invocation -1))
    (remhash target e-board-runtime--invocations)))

(defun e-board-runtime--attachment-key (board participant)
  "Return the attachment lookup key for BOARD and PARTICIPANT."
  (list (e-board-registry-board-id board)
         (e-board-registry-participant-id participant)))

(defun e-board-runtime--session-key (harness session-id)
  "Return the concrete endpoint lookup key for HARNESS SESSION-ID."
  (list harness session-id))

(defun e-board-runtime--resolved-session-attachment-key
    (harness session-id session-store-id)
  "Return stable reverse key for one resolved session endpoint."
  (if session-store-id
      (list 'session-store session-store-id session-id)
    (e-board-runtime--session-key harness session-id)))

(defun e-board-runtime--attachment-session-key (attachment)
  "Return ATTACHMENT's stable reverse session identity."
  (e-board-runtime--resolved-session-attachment-key
   (e-board-runtime-attachment-harness attachment)
   (e-board-runtime-attachment-session-id attachment)
   (e-board-runtime-attachment-session-store-id attachment)))

(defun e-board-runtime--attachment-composite-generation (attachment)
  "Return ATTACHMENT's frozen instance/concrete-harness generation pair."
  (list (e-board-runtime-attachment-instance-catalog-generation attachment)
        (e-board-runtime-attachment-harness-object-generation attachment)))

(defun e-board-runtime--invocation-target (attachment turn-id tool-call-id)
  "Return ATTACHMENT's immutable target for TURN-ID and TOOL-CALL-ID."
  (list (e-board-registry-board-id (e-board-runtime-attachment-board attachment))
        (e-board-registry-participant-id
         (e-board-runtime-attachment-participant attachment))
        turn-id tool-call-id))

(defun e-board-runtime--current-attachment-p (attachment)
  "Return non-nil when ATTACHMENT still owns its board participant endpoint."
  (let* ((board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment))
         (instance-id (e-board-runtime-attachment-instance-id attachment))
         (harness-id (e-board-runtime-attachment-harness-id attachment)))
    (and (or (null instance-id)
             (and (equal
                   (e-board-runtime-attachment-instance-catalog-generation
                    attachment)
                   (e-harness-instance-generation))
                  (e-harness-instance-get instance-id)))
         (or (null harness-id)
             (and (equal
                   (e-board-runtime-attachment-harness-object-generation
                    attachment)
                   (e-harness-registry-generation harness-id))
                  (eq (e-board-runtime-attachment-harness attachment)
                      (e-harness-registry-get harness-id))))
         (eq (gethash (e-board-runtime--attachment-key board participant)
                      e-board-runtime--attachments)
             attachment)
         (eq (gethash (e-board-runtime--attachment-session-key attachment)
                      e-board-runtime--session-attachments)
             attachment)
         (eq (gethash (e-board-runtime--session-key
                       (e-board-runtime-attachment-harness attachment)
                       (e-board-runtime-attachment-session-id attachment))
                      e-board-runtime--endpoint-attachments)
             attachment))))

(defun e-board-runtime--register-invocation
    (attachment turn-id tool-call-id callback)
  "Capture CALLBACK behind one exact invocation target before work starts.
The target retains the original live ATTACHMENT and its generation.  Later
effects may use that capture or fail visibly; they never resolve a replacement
endpoint by session identity."
  (unless (and turn-id tool-call-id (functionp callback))
    (signal 'e-board-runtime-error
            (list "Invocation requires turn id, tool call id, and callback")))
  (let ((target (e-board-runtime--invocation-target
                 attachment turn-id tool-call-id)))
    (when (gethash target e-board-runtime--invocations)
      (signal 'e-board-runtime-error (list "Invocation target already exists" target)))
    (puthash target
             (e-board-runtime-invocation--create
              :target target
              :attachment attachment
              :attachment-generation (e-board-runtime-attachment-generation attachment)
              :endpoint-token
              (copy-tree (e-board-runtime-attachment-endpoint-token attachment))
              :composite-generation
              (copy-tree (e-board-runtime--attachment-composite-generation attachment))
              :callback callback
              :state 'open)
             e-board-runtime--invocations)
    (e-board-runtime--adjust-unsettled-count 'invocation 1)
    target))

(defun e-board-runtime--apply-invocation-effect (_board target state payload)
  "Apply TARGET exactly once through its captured runtime invocation service."
  (let ((invocation (gethash target e-board-runtime--invocations)))
    (unless invocation
      (signal 'e-board-runtime-error (list "Unknown invocation target" target)))
    (unless (eq (e-board-runtime-invocation-state invocation) 'open)
      (signal 'e-board-runtime-error (list "Invocation target is not open" target)))
    (let ((attachment (e-board-runtime-invocation-attachment invocation)))
      (unless (and
               (= (e-board-runtime-invocation-attachment-generation invocation)
                  (e-board-runtime-attachment-generation attachment))
               (equal (e-board-runtime-invocation-endpoint-token invocation)
                      (e-board-runtime-attachment-endpoint-token attachment))
               (equal (e-board-runtime-invocation-composite-generation invocation)
                      (e-board-runtime--attachment-composite-generation attachment))
               (e-board-runtime--current-attachment-p attachment))
        (setf (e-board-runtime-invocation-state invocation) 'unavailable)
        (e-board-runtime--adjust-unsettled-count 'invocation -1)
        (signal 'e-board-runtime-error
                (list "Original invocation endpoint is unavailable" target)))
      (setf (e-board-runtime-invocation-state invocation) 'applying)
      (condition-case err
          (funcall (e-board-runtime-invocation-callback invocation) state payload)
        (error
         (setf (e-board-runtime-invocation-state invocation) 'failed)
         (e-board-runtime--adjust-unsettled-count 'invocation -1)
         (signal (car err) (cdr err))))
      (setf (e-board-runtime-invocation-state invocation) 'committed)
      (e-board-runtime--adjust-unsettled-count 'invocation -1))))

(defun e-board-runtime--drain-deferred-hooks ()
  "Start one bounded page of deferred carrier hooks outside settlement.
Queue items retain the installing runtime generation, so a reload/replacement
can invalidate pending callbacks without letting an old closure advance the
new runtime.  Hook thunks are already receipt-deduplicated by `e-work'."
  (setq e-board-runtime--deferred-hook-drain-scheduled nil)
  (let ((processed 0))
    (while (and e-board-runtime--deferred-hook-head
                (< processed e-board-runtime-deferred-hook-drain-limit))
      (pcase-let ((`(,generation ,_receipt ,thunk)
                   (pop e-board-runtime--deferred-hook-head)))
        (e-board-runtime--adjust-unsettled-count 'deferred-hook -1)
        (unless e-board-runtime--deferred-hook-head
          (setq e-board-runtime--deferred-hook-tail nil))
        (cl-incf processed)
        (when (= generation e-board-runtime--deferred-hook-generation)
          (funcall thunk))))
    (when e-board-runtime--deferred-hook-head
      (setq e-board-runtime--deferred-hook-drain-scheduled t)
      (run-at-time 0 nil #'e-board-runtime--drain-deferred-hooks))))

(defun e-board-runtime--schedule-deferred-hook (_handle receipt thunk)
  "Queue deferred carrier THUNK with stable RECEIPT outside its start stack."
  (let ((cell (list (list e-board-runtime--deferred-hook-generation receipt thunk))))
    (if e-board-runtime--deferred-hook-tail
        (setcdr e-board-runtime--deferred-hook-tail cell)
      (setq e-board-runtime--deferred-hook-head cell))
    (setq e-board-runtime--deferred-hook-tail cell))
  (e-board-runtime--adjust-unsettled-count 'deferred-hook 1)
  (unless e-board-runtime--deferred-hook-drain-scheduled
    (setq e-board-runtime--deferred-hook-drain-scheduled t)
    (run-at-time 0 nil #'e-board-runtime--drain-deferred-hooks)))

(defun e-board-runtime--enqueue-activity-flush (work-id)
  "Queue one future flush for WORK-ID without retaining each raw update."
  (unless (gethash work-id e-board-runtime--pending-activity-set)
    (puthash work-id t e-board-runtime--pending-activity-set)
    (let ((cell (list work-id)))
      (if e-board-runtime--pending-activity-tail
          (setcdr e-board-runtime--pending-activity-tail cell)
        (setq e-board-runtime--pending-activity-head cell))
      (setq e-board-runtime--pending-activity-tail cell))
    (e-board-runtime--unsettled-changed))
  (unless e-board-runtime--activity-drain-scheduled
    (setq e-board-runtime--activity-drain-scheduled t)
    (run-at-time 0 nil #'e-board-runtime--drain-activity-mailboxes)))

(defun e-board-runtime--activity-content (payload)
  "Return a bounded diagnostic rendering of raw activity PAYLOAD."
  (truncate-string-to-width (e-prin1-safe payload) 512 nil nil "..."))

(defun e-board-runtime--drain-activity-mailboxes ()
  "Publish one bounded page of latest work activity mailbox snapshots."
  (setq e-board-runtime--activity-drain-scheduled nil)
  (let ((processed 0))
    (while (and e-board-runtime--pending-activity-head
                (< processed e-board-runtime-activity-drain-limit))
      (let* ((work-id (pop e-board-runtime--pending-activity-head))
             (mailbox (gethash work-id e-board-runtime--work-activity-mailboxes)))
        (unless e-board-runtime--pending-activity-head
          (setq e-board-runtime--pending-activity-tail nil))
        (let ((counted (gethash work-id e-board-runtime--pending-activity-set)))
          (remhash work-id e-board-runtime--pending-activity-set)
          (when counted
            (e-board-runtime--unsettled-changed)))
        (remhash work-id e-board-runtime--work-activity-mailboxes)
        (cl-incf processed)
        (when mailbox
          (let* ((attachment (plist-get mailbox :attachment))
                 (board (e-board-registry-board-source-board
                         (e-board-runtime-attachment-board attachment)))
                 (participant-id
                  (e-board-registry-participant-id
                   (e-board-runtime-attachment-participant attachment))))
            (when (e-board-runtime--current-attachment-p attachment)
              (e-board-post-activity
               board
               :author (format "participant:%s" participant-id)
               :subject-participant-id participant-id
               :source-turn-id (plist-get mailbox :turn-id)
               :activity-kind (or (plist-get mailbox :activity-kind)
                                  'work-progress)
               :tags (copy-tree (plist-get mailbox :tags))
               :attributes (list :work-id work-id)
               :content (e-board-runtime--activity-content
                         (plist-get mailbox :payload))
               :caused-by-delivery-ids
               (copy-tree
                (gethash (plist-get mailbox :turn-id)
                         (e-board-runtime-attachment-turn-delivery-ids
                          attachment)))
               :source-activity-key (plist-get mailbox :source-key)))))))
    (when e-board-runtime--pending-activity-head
      (setq e-board-runtime--activity-drain-scheduled t)
      (run-at-time 0 nil #'e-board-runtime--drain-activity-mailboxes))))

(defun e-board-runtime--capture-work-activity (attachment handle payload)
  "Replace HANDLE's bounded progress mailbox with PAYLOAD.
This is the sole synchronous activity observer installed by board enrollment.
It neither formats nor publishes PAYLOAD; a later runtime activity publisher
will consume the mailbox under its own bounded drain."
  (let* ((work-id (e-work-handle-id handle))
         (participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (sequence (cl-incf (e-board-runtime-attachment-activity-sequence attachment))))
    (puthash work-id
             (list :attachment attachment
                   :turn-id (plist-get (e-work-handle-context handle) :turn-id)
                   :activity-kind 'work-progress
                   :payload payload
                   :source-key
                   (list participant-id
                         (e-board-runtime-attachment-generation attachment)
                         sequence))
             e-board-runtime--work-activity-mailboxes)
    (e-board-runtime--enqueue-activity-flush work-id)))

(defun e-board-runtime--capture-turn-progress (attachment event)
  "Coalesce high-frequency reasoning EVENT into one latest-value mailbox."
  (let* ((turn-id (plist-get event :turn-id))
         (activity-kind (e-events-type event))
         (participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (mailbox-id
          (list 'turn-progress participant-id
                (e-board-runtime-attachment-generation attachment)
                turn-id activity-kind))
         (sequence
          (cl-incf (e-board-runtime-attachment-activity-sequence attachment))))
    (puthash mailbox-id
             (list :attachment attachment :turn-id turn-id
                   :activity-kind activity-kind
                   :tags (copy-tree
                          (gethash turn-id
                                   (e-board-runtime-attachment-turn-tags attachment)))
                   :payload (plist-get event :payload)
                   :source-key
                   (list participant-id
                         (e-board-runtime-attachment-generation attachment)
                         sequence))
             e-board-runtime--work-activity-mailboxes)
    (e-board-runtime--enqueue-activity-flush mailbox-id)))

(defun e-board-runtime--install-work-hooks (attachment handle)
  "Install the private board-runtime hook classification on prepared HANDLE."
  (let ((policies '(:cancel deferred :cleanup deferred :settle deferred)))
    (dolist (key '(:on-done :on-error :on-progress :on-event))
      (when (plist-get (e-work-handle-callbacks handle) key)
        (setq policies (plist-put policies key 'deferred))))
    (when (e-work-spec-result-shaper (e-work-handle-spec handle))
      ;; Result shaping is still part of a cheap runner in this foundation.
      ;; The board runtime therefore admits it only under the narrow inline
      ;; classification; a later async shaper will use a separate work unit.
      (setq policies (plist-put policies :result-shaper 'hard-bounded)))
    (e-work-install-hook-dispatcher
     handle #'e-board-runtime--schedule-deferred-hook policies)
    (e-work-install-activity-observer
     handle (lambda (current-handle payload)
              (e-board-runtime--capture-work-activity
               attachment current-handle payload)))))

(defun e-board-runtime--pickup-queue-key (board pickup-id)
  "Return the process-local queue identity for BOARD's PICKUP-ID."
  (list (e-board-registry-board-id board) pickup-id))

(defun e-board-runtime--enqueue-pickups (board pickup-ids)
  "Enqueue BOARD PICKUP-IDS once; never deliver on an append/effect stack."
  (dolist (pickup-id pickup-ids)
    (let ((key (e-board-runtime--pickup-queue-key board pickup-id)))
      (unless (gethash key e-board-runtime--pending-pickup-set)
        (puthash key t e-board-runtime--pending-pickup-set)
        (let ((cell (list key)))
          (if e-board-runtime--pending-pickup-tail
              (setcdr e-board-runtime--pending-pickup-tail cell)
            (setq e-board-runtime--pending-pickup-head cell))
          (setq e-board-runtime--pending-pickup-tail cell))
        (e-board-runtime--unsettled-changed))))
  (unless e-board-runtime--pickup-drain-scheduled
    (setq e-board-runtime--pickup-drain-scheduled t)
    (run-at-time 0 nil #'e-board-runtime--drain-pickups)))

(defun e-board-runtime--drain-pickups ()
  "Attempt one bounded FIFO page of previously frozen pickup envelopes."
  (setq e-board-runtime--pickup-drain-scheduled nil)
  (let ((processed 0)
        (available-at-start 0)
        (cursor e-board-runtime--pending-pickup-head))
    (while (and cursor
                (< available-at-start e-board-runtime-pickup-drain-limit))
      (cl-incf available-at-start)
      (setq cursor (cdr cursor)))
    (while (and e-board-runtime--pending-pickup-head
                (< processed available-at-start))
      (let ((key (pop e-board-runtime--pending-pickup-head)))
        (unless e-board-runtime--pending-pickup-head
          (setq e-board-runtime--pending-pickup-tail nil))
        (let ((counted (gethash key e-board-runtime--pending-pickup-set)))
          (remhash key e-board-runtime--pending-pickup-set)
          (when counted
            (e-board-runtime--unsettled-changed)))
        (cl-incf processed)
        (let ((board (condition-case nil
                         (e-board-registry-get (car key))
                       (e-board-registry-missing nil))))
          (when board
            (e-board-runtime--deliver-pickups board (list (cadr key)))))))
    (when e-board-runtime--pending-pickup-head
      (setq e-board-runtime--pickup-drain-scheduled t)
      (run-at-time 0 nil #'e-board-runtime--drain-pickups))))

(defun e-board-runtime--drain-input-routing (board drain)
  "Run BOARD's bounded classifier, then queue only its finalized pickups."
  (funcall drain)
  (dolist (result (e-board-drain-routed-pickups
                   (e-board-registry-board-source-board board)))
    (e-board-runtime--producer-routing-finished board (car result) (cadr result))
    (e-board-runtime--enqueue-pickups board (cadr result))))

(defun e-board-runtime--enroll-work (harness handle callback)
  "Enroll HANDLE for its attached HARNESS session before runner entry.
CALLBACK is the private loop result seam for executable tool work; turn work
has no callback and is observed only."
  (when-let* ((session-id (plist-get (e-work-handle-context handle) :session-id))
              (attachment (gethash (e-board-runtime--session-key harness session-id)
                                   e-board-runtime--endpoint-attachments)))
    (let* ((board (e-board-registry-board-source-board
                   (e-board-runtime-attachment-board attachment)))
           (metadata (e-work-handle-metadata handle)))
      (e-board-runtime--install-work-hooks attachment handle)
      (if callback
          (let* ((context (e-work-handle-context handle))
                 (turn-id (plist-get context :turn-id))
                 (tool-call-id (plist-get (plist-get context :tool-call) :id))
                 (invocation-id (list turn-id tool-call-id))
                 ;; Capturing this service first means an invalid tool-call
                 ;; identity cannot leave an enrolled board operation behind.
                 (target (e-board-runtime--register-invocation
                          attachment turn-id tool-call-id callback)))
          (condition-case err
              (e-board-enroll-invocation-work
               board handle invocation-id target :metadata metadata)
            (error
             (e-board-runtime--drop-invocation target)
             (signal (car err) (cdr err)))))
        (e-board-enroll-work board handle :metadata metadata)))))

(defun e-board-runtime--subscribe-aggregation
    (harness handles mode timeout callback invocation-context)
  "Subscribe attached HARNESS await work through its source board."
  (when-let* ((session-id (plist-get invocation-context :session-id))
              (attachment (gethash (e-board-runtime--session-key harness session-id)
                                   e-board-runtime--endpoint-attachments)))
    (let* ((board (e-board-registry-board-source-board
                   (e-board-runtime-attachment-board attachment)))
           (call (plist-get invocation-context :tool-call))
           (turn-id (plist-get invocation-context :turn-id))
           (invocation-id (list turn-id (plist-get call :id)))
           (target (e-board-runtime--register-invocation
                    attachment turn-id (plist-get call :id)
                    (lambda (_state reason) (funcall callback reason))))
           aggregation)
      (condition-case err
          (setq aggregation
                (e-board-subscribe-aggregation
                 board (mapcar #'e-work-handle-id handles) mode target
                 :id invocation-id :timeout timeout))
        (error
         (e-board-runtime--drop-invocation target)
         (signal (car err) (cdr err))))
      (lambda ()
        (e-board-cancel-aggregation board (e-board-aggregation-id aggregation))
        (e-board-runtime--drop-invocation target)))))

(defun e-board-runtime--publish-output (attachment turn-id)
  "Publish ATTACHMENT's final assistant message for TURN-ID exactly once."
  (let* ((harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (message (e-harness--turn-assistant-message harness session-id turn-id)))
    (when-let ((sequence (and message
                              (plist-get message :board-output-sequence))))
      (let* ((board (e-board-registry-board-source-board
                     (e-board-runtime-attachment-board attachment)))
             (participant-id
              (e-board-registry-participant-id
               (e-board-runtime-attachment-participant attachment))))
        (e-board-post-output
         board
         :author (format "participant:%s" participant-id)
         :subject-participant-id participant-id
         :source-turn-id turn-id
         :tags (or (copy-tree
                    (gethash turn-id
                             (e-board-runtime-attachment-turn-tags attachment)))
                   '(main))
         :content (plist-get message :content)
         :caused-by-delivery-ids
         (copy-tree
          (gethash turn-id
                   (e-board-runtime-attachment-turn-delivery-ids attachment)))
         :source-output-key
         (list participant-id
               (e-board-runtime-attachment-generation attachment)
               sequence))))))

(defun e-board-runtime--enqueue-ready-participant-pickup (attachment)
  "Queue ATTACHMENT's current ready FIFO head, if it has one.
This is the runtime-side wake edge for a pickup that could not enter a busy
session earlier.  It reads only the owning participant's head rather than
scanning a board-wide delivery table."
  (let* ((registry-board (e-board-runtime-attachment-board attachment))
         (board (e-board-registry-board-source-board registry-board))
         (participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (delivery-id (car (e-board--pickup-queue board participant-id)))
         (pickup (and delivery-id (e-board-pickup board delivery-id))))
    (when (and pickup (eq (e-board-pickup-state pickup) 'ready))
      (e-board-runtime--enqueue-pickups registry-board (list delivery-id)))))

(defun e-board-runtime--event-activity-source-key (attachment event publication-kind)
  "Return EVENT's stable board source key for PUBLICATION-KIND.
The durable session activity sequence is the retry identity when available.
The locally allocated fallback only serves legacy or synthetic events that have
no durable activity entry.  One source event may publish more than one derived
row, so its numeric key reserves an adjacent slot for the terminal summary."
  (let* ((participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (durable-sequence (plist-get event :board-activity-sequence))
         (sequence
          (if durable-sequence
              (+ (* 2 durable-sequence)
                 (if (eq publication-kind 'turn-summary) 1 0))
            (cl-incf (e-board-runtime-attachment-activity-sequence attachment)))))
    (list participant-id
          (e-board-runtime-attachment-generation attachment)
          sequence)))

(defun e-board-runtime--publish-terminal-activity (attachment event)
  "Publish EVENT's failure or cancellation as one terminal board activity.
Successful turns publish their final output through the separate output seam;
these terminal states have no output to close the board-owned open projection."
  (let* ((registry-board (e-board-runtime-attachment-board attachment))
         (board (e-board-registry-board-source-board registry-board))
         (participant-id (e-board-registry-participant-id
                          (e-board-runtime-attachment-participant attachment)))
         (turn-id (plist-get event :turn-id))
         (activity-kind (e-events-type event)))
    (when (and turn-id (memq activity-kind '(turn-failed turn-cancelled)))
      (e-board-post-activity
       board :author (format "participant:%s" participant-id)
       :subject-participant-id participant-id :source-turn-id turn-id
       :tags (copy-tree
              (gethash turn-id (e-board-runtime-attachment-turn-tags attachment)))
       :activity-kind activity-kind
       :attributes
       (append
        (copy-tree
         (e-harness--durable-activity-payload
          activity-kind (plist-get event :payload)))
        (when-let ((source-event-id
                    (plist-get event :activity-entry-id)))
          (list :source-event-id source-event-id)))
       :caused-by-delivery-ids
       (copy-tree
        (gethash turn-id
                 (e-board-runtime-attachment-turn-delivery-ids attachment)))
       :source-activity-key
       (e-board-runtime--event-activity-source-key attachment event activity-kind)))))

(defun e-board-runtime--publish-harness-activity (attachment event)
  "Publish EVENT's bounded lifecycle edge without exposing its raw payload."
  (let ((activity-kind (e-events-type event))
        (turn-id (plist-get event :turn-id)))
    (when (and turn-id
               (memq activity-kind e-board-runtime--visible-harness-activity-types))
      (let* ((registry-board (e-board-runtime-attachment-board attachment))
             (board (e-board-registry-board-source-board registry-board))
             (participant-id
              (e-board-registry-participant-id
               (e-board-runtime-attachment-participant attachment))))
        (e-board-post-activity
         board :author (format "participant:%s" participant-id)
         :subject-participant-id participant-id :source-turn-id turn-id
         :tags (or (copy-tree
                    (gethash turn-id
                             (e-board-runtime-attachment-turn-tags attachment)))
                   '(main))
         :activity-kind activity-kind
         :attributes
         (append
          (copy-tree
           (e-harness--durable-activity-payload
            activity-kind (plist-get event :payload)))
          (when-let ((source-event-id
                      (plist-get event :activity-entry-id)))
            (list :source-event-id source-event-id)))
         :caused-by-delivery-ids
         (copy-tree
          (gethash turn-id
                   (e-board-runtime-attachment-turn-delivery-ids attachment)))
         :source-activity-key
         (e-board-runtime--event-activity-source-key attachment event activity-kind))))))

(defun e-board-runtime--turn-activity (attachment turn-id)
  "Return ATTACHMENT's bounded activity accumulator for TURN-ID."
  (or (gethash turn-id (e-board-runtime-attachment-turn-activity attachment))
      (let ((state (e-board-runtime-turn-activity--create
                    :tool-count 0 :action-count 0)))
        (puthash turn-id state (e-board-runtime-attachment-turn-activity attachment))
        state)))

(defun e-board-runtime--observe-turn-activity (attachment event)
  "Record the narrow summary fields exposed by one attached harness EVENT."
  (when-let ((turn-id (plist-get event :turn-id)))
    (let ((state (e-board-runtime--turn-activity attachment turn-id)))
      (pcase (e-events-type event)
        ('provider-request-started
         (setf (e-board-runtime-turn-activity-provider-seen state) t)
         (unless (e-board-runtime-turn-activity-provider-started-at state)
           (setf (e-board-runtime-turn-activity-provider-started-at state)
                 (plist-get event :created-at))))
        ('tool-started
         (cl-incf (e-board-runtime-turn-activity-tool-count state)))
        ('action-started
         (cl-incf (e-board-runtime-turn-activity-action-count state)))))))

(defun e-board-runtime--publish-turn-summary (attachment event status)
  "Publish one terminal summary for provider-active TURN-ID, then release state."
  (when-let* ((turn-id (plist-get event :turn-id))
              (state (gethash turn-id (e-board-runtime-attachment-turn-activity attachment))))
    (remhash turn-id (e-board-runtime-attachment-turn-activity attachment))
    (when (e-board-runtime-turn-activity-provider-seen state)
      (let* ((registry-board (e-board-runtime-attachment-board attachment))
             (board (e-board-registry-board-source-board registry-board))
             (participant-id (e-board-registry-participant-id
                              (e-board-runtime-attachment-participant attachment))))
        (e-board-post-activity
         board :author (format "participant:%s" participant-id)
         :subject-participant-id participant-id :source-turn-id turn-id
         :tags (copy-tree
                (gethash turn-id (e-board-runtime-attachment-turn-tags attachment)))
         :activity-kind 'turn-summary
         :attributes
         (append (list :status status
                       :duration-seconds
                       (max 0 (- (plist-get event :created-at)
                                 (e-board-runtime-turn-activity-provider-started-at state)))
                       :tool-count (e-board-runtime-turn-activity-tool-count state)
                       :action-count (e-board-runtime-turn-activity-action-count state))
                 (when-let ((source-event-id
                             (plist-get event :activity-entry-id)))
                   (list :source-event-id source-event-id)))
         :caused-by-delivery-ids
         (copy-tree
          (gethash turn-id
                   (e-board-runtime-attachment-turn-delivery-ids attachment)))
         :source-activity-key
         (e-board-runtime--event-activity-source-key attachment event 'turn-summary))))))

(defun e-board-runtime--producer-turn-key (attachment turn-id)
  "Return producer completion key for ATTACHMENT and TURN-ID."
  (list (e-board-registry-board-id
         (e-board-runtime-attachment-board attachment))
        (e-board-registry-participant-id
         (e-board-runtime-attachment-participant attachment))
        turn-id))

(defun e-board-runtime--settle-producer-turn (attachment event status)
  "Settle any producer delivery causally owned by terminal EVENT."
  (when-let* ((turn-id (plist-get event :turn-id))
              (entry (gethash (e-board-runtime--producer-turn-key
                               attachment turn-id)
                              e-board-runtime--producer-turns)))
    (remhash (e-board-runtime--producer-turn-key attachment turn-id)
             e-board-runtime--producer-turns)
    (e-board-runtime--producer-delivery-terminal
     (car entry) (cadr entry) status
     (list :turn-id turn-id :participant-id
           (e-board-registry-participant-id
            (e-board-runtime-attachment-participant attachment))))))

(defun e-board-runtime--handle-harness-event (attachment event)
  "Publish attached output and reconcile board-delivery receipts from EVENT."
  (when (e-board-runtime--current-attachment-p attachment)
    (let ((type (e-events-type event)))
      (when (memq type '(reasoning-delta reasoning-raw-delta))
        (e-board-runtime--capture-turn-progress attachment event))
      (e-board-runtime--observe-turn-activity attachment event)
      (e-board-runtime--publish-harness-activity attachment event)
      (cond
       ((eq type 'turn-finished)
        (e-board-runtime--publish-output attachment (plist-get event :turn-id))
        (e-board-runtime--publish-turn-summary attachment event 'finished)
        (remhash (plist-get event :turn-id)
                 (e-board-runtime-attachment-turn-tags attachment))
        (remhash (plist-get event :turn-id)
                 (e-board-runtime-attachment-turn-delivery-ids attachment))
        (e-board-runtime--settle-producer-turn attachment event 'done)
        (e-board-runtime--enqueue-ready-participant-pickup attachment))
       ((memq type '(turn-failed turn-cancelled))
        (e-board-runtime--publish-terminal-activity attachment event)
        (e-board-runtime--publish-turn-summary attachment event type)
        (remhash (plist-get event :turn-id)
                 (e-board-runtime-attachment-turn-tags attachment))
        (remhash (plist-get event :turn-id)
                 (e-board-runtime-attachment-turn-delivery-ids attachment))
        (e-board-runtime--settle-producer-turn
         attachment event (if (eq type 'turn-cancelled) 'cancelled 'failed))
        (e-board-runtime--enqueue-ready-participant-pickup attachment))
       ((eq type 'input-consumed)
        (let* ((payload (plist-get event :payload))
               (delivery-id (plist-get payload :delivery-id))
               (registry-board (e-board-runtime-attachment-board attachment))
               (board (e-board-registry-board-source-board registry-board))
               (pickup (e-board-pickup board delivery-id)))
          (when (and pickup (plist-get event :turn-id))
            (puthash (plist-get event :turn-id) (list delivery-id)
                     (e-board-runtime-attachment-turn-delivery-ids attachment))
            (puthash (plist-get event :turn-id)
                     (copy-tree
                      (plist-get (e-board-pickup-cause-metadata pickup)
                                 :routing-tags))
                     (e-board-runtime-attachment-turn-tags attachment)))
          (when-let* ((turn-id (plist-get event :turn-id))
                      (item (gethash delivery-id
                                     e-board-runtime--producer-deliveries)))
            (puthash (e-board-runtime--producer-turn-key
                      attachment turn-id)
                     (list item delivery-id)
                     e-board-runtime--producer-turns))
          (when (and pickup
                     (memq (e-board-pickup-state pickup) '(accepted cancelling))
                     (e-board-runtime--consumption-receipt-matches-p
                      pickup attachment payload))
            (when-let ((next-id
                        (e-board-pickup-complete-delivery board delivery-id)))
              (e-board-runtime--enqueue-pickups registry-board (list next-id))))))
       ((eq type 'input-discarded)
        (let* ((payload (plist-get event :payload))
               (delivery-id (plist-get payload :delivery-id))
               (registry-board (e-board-runtime-attachment-board attachment))
               (board (e-board-registry-board-source-board registry-board))
               (pickup (e-board-pickup board delivery-id)))
          (when-let ((item (gethash delivery-id
                                    e-board-runtime--producer-deliveries)))
            (e-board-runtime--producer-delivery-terminal
             item delivery-id 'failed
             (list :reason (or (plist-get payload :reason) 'input-discarded))))
          (when (and pickup
                     (memq (e-board-pickup-state pickup) '(accepted cancelling))
                     (e-board-runtime--consumption-receipt-matches-p
                      pickup attachment payload))
            (when-let ((next-id
                        (e-board-pickup-discard-delivery
                         board delivery-id
                         (or (plist-get payload :reason) 'input-discarded))))
              (e-board-runtime--enqueue-pickups registry-board (list next-id))))))
       ((eq type 'session-reset)
        (let* ((registry-board (e-board-runtime-attachment-board attachment))
               (board (e-board-registry-board-source-board registry-board))
               (participant-id
                (e-board-registry-participant-id
                 (e-board-runtime-attachment-participant attachment)))
               (delivery-id (car (e-board--pickup-queue board participant-id)))
               (pickup (and delivery-id (e-board-pickup board delivery-id))))
          (when (and pickup
                     (memq (e-board-pickup-state pickup) '(accepted cancelling)))
            (when-let ((next-id
                        (e-board-pickup-discard-delivery
                         board delivery-id 'session-reset)))
              (e-board-runtime--enqueue-pickups registry-board (list next-id)))))))
      (when (and (memq type '(queue-changed turn-finished turn-failed
                              turn-cancelled input-consumed input-discarded
                              session-reset))
                 (e-board-runtime-attachment-reconciliation attachment))
        (e-board-runtime--schedule-reconciliation
         (e-board-runtime-attachment-reconciliation attachment))))))

(defun e-board-runtime--active-board (board-or-id)
  "Return active registry BOARD-OR-ID."
  (let ((board (e-board-registry-get board-or-id)))
    (unless (eq (e-board-registry-board-state board) 'active)
      (signal 'e-board-registry-closed
              (list (e-board-registry-board-id board))))
    board))

(defun e-board-runtime--require-live-session (harness session-id)
  "Return HARNESS's existing SESSION-ID, or signal a runtime-specific error."
  (condition-case nil
      (e-session-get (e-harness-sessions harness) session-id)
    (error (signal 'e-board-runtime-session-missing (list session-id)))))

(defun e-board-runtime--delivery-metadata (pickup)
  "Return harness metadata that identifies the frozen PICKUP."
  (let ((attempt (or (e-board-pickup-attempt pickup)
                     (signal 'e-board-runtime-error
                             (list "Pickup has no bound attempt"
                                   (e-board-pickup-delivery-id pickup))))))
    (append
     (list :input-origin 'board
          :board-delivery-id (copy-tree (e-board-pickup-delivery-id pickup))
          :board-id (e-board-pickup-board-id pickup)
          :board-participant-id (e-board-pickup-participant-id pickup)
          :board-message-id (e-board-pickup-message-id pickup)
          :board-subscription-ids
          (copy-sequence (e-board-pickup-subscription-ids pickup))
          :board-event-seq-range
          (copy-sequence (e-board-pickup-event-seq-range pickup))
          :board-input-mode (e-board-pickup-mode pickup)
          :board-reference (copy-tree (e-board-pickup-reference pickup))
          :board-requester-actor
          (copy-tree (e-board-pickup-requester-actor pickup))
          :board-cause-metadata
          (copy-tree (e-board-pickup-cause-metadata pickup))
          :board-endpoint-token
          (e-board--copy-envelope-value
           (e-board-delivery-attempt-endpoint-token attempt))
          :board-endpoint-generation
          (copy-tree
           (e-board-delivery-attempt-composite-generation attempt)))
     (copy-tree
      (plist-get (e-board-pickup-cause-metadata pickup)
                 :input-attributes)))))

(defun e-board-runtime--attempt-belongs-to-attachment-p (pickup attachment)
  "Return non-nil when PICKUP's bound attempt names ATTACHMENT exactly."
  (when-let ((attempt (e-board-pickup-attempt pickup)))
    (and (equal (e-board-delivery-attempt-endpoint-token attempt)
                (e-board-runtime-attachment-endpoint-token attachment))
         (equal (e-board-delivery-attempt-composite-generation attempt)
                (e-board-runtime--attachment-composite-generation attachment)))))

(defun e-board-runtime--consumption-receipt-matches-p
    (pickup attachment payload)
  "Return non-nil when PAYLOAD proves PICKUP consumption on ATTACHMENT."
  (when-let ((attempt (e-board-pickup-attempt pickup)))
    (and (e-board-runtime--attempt-belongs-to-attachment-p pickup attachment)
         (equal (plist-get payload :endpoint-token)
                (e-board-delivery-attempt-endpoint-token attempt))
         (equal (plist-get payload :endpoint-generation)
                (e-board-delivery-attempt-composite-generation attempt)))))

(defun e-board-runtime--deliver-to-harness (attachment pickup _message)
  "Deliver PICKUP's MESSAGE through ATTACHMENT's harness session.
An idle input starts one turn.  During an active turn, inject-mode enters the
steering lane while queue-mode enters the later-turn inbox."
  (let* ((harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (prompt (e-board-pickup-content pickup))
         (metadata (e-board-runtime--delivery-metadata pickup)))
    (unless (and (stringp prompt) (not (string-empty-p prompt)))
      (user-error "Board input content must be a non-empty string"))
    (let ((active-turn (plist-get (e-harness-state harness session-id)
                                  :active-turn)))
      (pcase (e-board-pickup-mode pickup)
        ('queue
         (if active-turn
             (list :accepted
                   (e-harness-request-follow-up
                    harness session-id prompt :metadata metadata))
           (let ((turn-id
                  (e-harness-prompt-async
                   harness session-id prompt :metadata metadata)))
             (puthash turn-id
                      (copy-tree
                       (plist-get (e-board-pickup-cause-metadata pickup)
                                  :routing-tags))
                      (e-board-runtime-attachment-turn-tags attachment))
             :consumed)))
        ('inject
         (let ((turn-id
                (if active-turn
                    (e-harness-steer-active-turn
                     harness session-id prompt :metadata metadata)
                  (e-harness-prompt-async
                   harness session-id prompt :metadata metadata))))
           (puthash turn-id
                    (copy-tree
                     (plist-get (e-board-pickup-cause-metadata pickup)
                                :routing-tags))
                    (e-board-runtime-attachment-turn-tags attachment))
           :consumed))))))

(cl-defun e-board-runtime-attach
    (board-or-id harness session-id
                 &key participant-id author principal controller delivery-function)
  "Attach existing live HARNESS SESSION-ID to BOARD-OR-ID as one participant.

DELIVERY-FUNCTION is called as (FUNCTION ATTACHMENT PICKUP MESSAGE) for each
frozen pending pickup addressed to the participant.  It must perform one
delivery or signal; normal return marks that pickup delivered.  Returning
=(:accepted RECEIPT)= waits for a later consumption receipt; returning
=(:uncertain REASON)= records an ambiguous original-endpoint attempt without
retrying it; returning =(:discarded REASON)= records a proven non-commit;
returning =(:failed REASON)= records a permanent endpoint rejection.
When omitted, the conservative idle-only harness delivery port is used."
  (e-board-runtime--require-admission)
  (e-board-runtime--attach-resolved
   board-or-id harness session-id
   :participant-id participant-id :author author :principal principal
   :controller controller
   :delivery-function delivery-function))

(cl-defun e-board-runtime--attach-resolved
    (board-or-id harness session-id
                 &key participant-id author principal controller delivery-function
                 instance-id instance-catalog-generation harness-id
                 harness-object-generation session-store-id endpoint-token)
  "Attach one already-resolved endpoint with optional qualified metadata."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (or (null delivery-function) (functionp delivery-function))
    (signal 'wrong-type-argument (list 'functionp delivery-function)))
  (e-board-runtime--require-live-session harness session-id)
  (let ((session-key (e-board-runtime--resolved-session-attachment-key
                      harness session-id session-store-id))
        (endpoint-key (e-board-runtime--session-key harness session-id)))
    (when (or (gethash session-key e-board-runtime--session-attachments)
              (gethash endpoint-key e-board-runtime--endpoint-attachments))
      (signal 'e-board-runtime-session-busy (list session-key))))
  (let* ((board (e-board-runtime--active-board board-or-id))
         (participant (e-board-registry-add-participant
                       board :id participant-id :author author :principal principal
                       :controller controller))
         (key (e-board-runtime--attachment-key board participant)))
    (when (gethash key e-board-runtime--attachments)
      (signal 'e-board-runtime-attachment-exists (list key)))
    (e-board-runtime--activate-attachment
     (e-board-runtime--make-attachment
      board participant harness session-id delivery-function 1
      :instance-id instance-id
      :instance-catalog-generation instance-catalog-generation
      :harness-id harness-id
      :harness-object-generation harness-object-generation
      :session-store-id session-store-id
      :endpoint-token endpoint-token))))

(cl-defun e-board-runtime-attach-instance
    (board-or-id instance-id session-id
                 &key participant-id author principal controller delivery-function)
  "Attach an existing live SESSION-ID through configured INSTANCE-ID.
This operation never invokes an instance factory or loads dormant history."
  (e-board-runtime--require-admission)
  (e-board-runtime--attach-instance-resolved
   board-or-id instance-id session-id
   :participant-id participant-id :author author :principal principal
   :controller controller :delivery-function delivery-function))

(cl-defun e-board-runtime--attach-instance-resolved
    (board-or-id instance-id session-id
                 &key participant-id author principal controller delivery-function)
  "Attach one admitted live SESSION-ID through INSTANCE-ID to BOARD-OR-ID."
  (let* ((instance-generation (e-harness-instance-generation))
         (instance (or (e-harness-instance-get instance-id)
                       (signal 'e-harness-instance-missing (list instance-id))))
         (store-id (e-harness-instance-session-store-id instance))
         (harness-id (e-harness-instance-harness-id instance))
         (harness (e-harness-registry-get harness-id))
         (harness-generation (e-harness-registry-generation harness-id)))
    (unless store-id
      (signal 'e-board-runtime-instance-ineligible (list instance-id)))
    (unless harness
      (signal 'e-harness-registry-missing (list harness-id)))
    (unless (= instance-generation (e-harness-instance-generation))
      (signal 'e-board-runtime-error (list instance-id 'stale-instance-catalog)))
    (let ((token (e-board-runtime-endpoint-token--create
                  :harness-id harness-id
                  :harness-object-generation harness-generation
                  :session-store-id store-id
                  :session-id session-id)))
      (e-board-runtime--attach-resolved
       board-or-id harness session-id
       :participant-id participant-id :author author :principal principal
       :controller controller
       :delivery-function delivery-function
       :instance-id instance-id
       :instance-catalog-generation instance-generation
       :harness-id harness-id
       :harness-object-generation harness-generation
      :session-store-id store-id
      :endpoint-token token))))

(cl-defun e-board-runtime--resume-instance-start
    (board-or-id instance-id session-store-id session-id requester-principal
                 expected-version activate-offline
                 &key participant-id author delivery-function on-done on-error)
  "Start controlled resume, optionally activating an offline INSTANCE-ID.
Every path first validates one exact dormant catalog row.  When
ACTIVATE-OFFLINE is non-nil and the concrete harness is absent, the named
instance's asynchronous activation port loads the session without registering
it.  A second exact catalog read then revalidates version, controller, resume
rights, and eligibility before one bounded registry/attachment commit."
  (unless (and requester-principal
               (integerp expected-version) (>= expected-version 0))
    (signal 'wrong-type-argument
            (list 'resume-authorization requester-principal expected-version)))
  (let* ((board (e-board-runtime--active-board board-or-id))
         (instance (or (e-harness-instance-get instance-id)
                       (signal 'e-harness-instance-missing (list instance-id))))
         (store (e-harness-instance-session-store session-store-id))
         (eligible (plist-get store :eligible-instance-ids))
         (harness-id (e-harness-instance-harness-id instance))
         children
         activated-harness
         validated-controller
         request)
    (unless (and (equal (e-harness-instance-session-store-id instance)
                        session-store-id)
                 (memq instance-id eligible))
      (signal 'e-board-runtime-instance-ineligible
              (list instance-id session-store-id)))
    (cl-labels
        ((fail (condition)
           (when (e-request-fail request condition)
             (when on-error
               (funcall on-error condition))))
         (track (child)
           (push child children)
           child)
         (validate-row (row revalidation-p)
           (let ((access-record (plist-get row :access-record)))
             (unless (= (plist-get access-record :version) expected-version)
               (signal 'e-board-runtime-resume-version-conflict
                       (list session-store-id session-id expected-version
                             (plist-get access-record :version))))
             (unless (e-harness-instance-session-access-allows-p
                      access-record requester-principal 'resume)
               (signal 'e-board-runtime-resume-denied
                       (list session-store-id session-id requester-principal)))
             (unless (memq instance-id (plist-get row :eligible-instance-ids))
               (signal 'e-board-runtime-instance-ineligible
                       (list instance-id session-store-id session-id)))
             (when (and revalidation-p
                        (not (equal validated-controller
                                    (plist-get access-record :controller))))
               (signal 'e-board-runtime-resume-version-conflict
                       (list session-store-id session-id expected-version
                             'controller-changed)))
             access-record))
         (finish-attachment (access-record)
           (unless (e-request-terminal-p request)
             (let (attachment)
               (condition-case condition
                   (setq attachment
                         (e-board-runtime--attach-instance-resolved
                          board instance-id session-id
                          :participant-id participant-id :author author
                          :principal requester-principal
                          :controller (plist-get access-record :controller)
                          :delivery-function delivery-function))
                 (error
                  (fail condition)))
               (when (and attachment (e-request-finish request attachment))
                 (when on-done
                   (funcall on-done attachment))))))
         (revalidation-finished (row)
           (unless (e-request-terminal-p request)
             (let (access-record current)
               (condition-case condition
                   (progn
                     (setq access-record (validate-row row t)
                           current (e-harness-registry-get harness-id))
                     (when (and current (not (eq current activated-harness)))
                       (signal 'e-board-runtime-session-busy
                               (list session-store-id session-id harness-id)))
                     (unless current
                       (e-harness-registry-register harness-id activated-harness))
                     (e-request-progress
                      request (list :phase 'revalidated
                                    :session-store-id session-store-id
                                    :session-id session-id))
                     (finish-attachment access-record))
                 (error
                  (fail condition))))))
         (activation-finished (harness)
           (unless (e-request-terminal-p request)
             (setq activated-harness harness)
             (e-request-progress
              request (list :phase 'revalidating
                            :session-store-id session-store-id
                            :session-id session-id))
             (condition-case condition
                 (track
                  (e-harness-instance-session-catalog-read-start
                   session-store-id session-id :principal requester-principal
                   :on-done #'revalidation-finished :on-error #'fail))
               (error
                (fail condition)))))
         (catalog-finished (row)
           (unless (e-request-terminal-p request)
             (let (access-record)
               (condition-case condition
                   (progn
                     (setq access-record (validate-row row nil)
                           validated-controller
                           (plist-get access-record :controller))
                     (e-request-progress
                      request (list :phase 'validated
                                    :session-store-id session-store-id
                                    :session-id session-id))
                     (if (e-harness-registry-get harness-id)
                         (finish-attachment access-record)
                       (unless activate-offline
                         (signal 'e-harness-registry-missing (list harness-id)))
                       (e-request-progress
                        request (list :phase 'activating
                                      :session-store-id session-store-id
                                      :session-id session-id))
                       (track
                        (e-harness-instance-session-activation-start
                         instance-id
                         (list :session-store-id session-store-id
                               :session-id session-id
                               :requester-principal requester-principal
                               :expected-version expected-version)
                         :on-done #'activation-finished :on-error #'fail))))
                 (error
                  (fail condition)))))))
      (setq request
            (e-board-runtime--track-control-request
             (e-request-lifecycle-create
              :id (format "board-resume-%d"
                          (cl-incf e-board-runtime--control-sequence))
              :owner 'e-board-runtime-resume
              :session-id session-id
              :generation (e-harness-instance-generation)
              :state 'created
              :cancel-function
              (lambda (_request)
                (dolist (child children)
                  (unless (e-request-terminal-p child)
                    (e-request-cancel child 'resume-cancelled)))))))
      (e-request-start
       request (list :phase 'catalog-read :instance-id instance-id
                     :session-store-id session-store-id :session-id session-id))
      (condition-case condition
          (track
           (e-harness-instance-session-catalog-read-start
            session-store-id session-id :principal requester-principal
            :on-done #'catalog-finished :on-error #'fail))
        (error
         (if (e-request-terminal-p request)
             (signal (car condition) (cdr condition))
           (fail condition))))
      request)))

(cl-defun e-board-runtime-resume-live-instance-start
    (board-or-id instance-id session-store-id session-id requester-principal
                 expected-version
                 &key participant-id author delivery-function on-done on-error)
  "Start authorized resume of an already-live loaded dormant session.
The operation never invokes either the legacy factory or dormant activation
port.  An offline instance therefore fails visibly after authorization."
  (e-board-runtime--require-admission)
  (e-board-runtime--resume-instance-start
   board-or-id instance-id session-store-id session-id requester-principal
   expected-version nil
   :participant-id participant-id :author author
   :delivery-function delivery-function :on-done on-done :on-error on-error))

(cl-defun e-board-runtime-resume-instance-start
    (board-or-id instance-id session-store-id session-id requester-principal
                 expected-version
                 &key participant-id author delivery-function on-done on-error)
  "Start authorized dormant resume through the named configured instance.
Offline loading uses only the instance's asynchronous activation port.  The
loaded harness remains unregistered and unattached until a second exact catalog
read proves the expected authorization and controller are still current."
  (e-board-runtime--require-admission)
  (e-board-runtime--resume-instance-start
   board-or-id instance-id session-store-id session-id requester-principal
   expected-version t
   :participant-id participant-id :author author
   :delivery-function delivery-function :on-done on-done :on-error on-error))

(defun e-board-runtime--make-attachment
    (board participant harness session-id delivery-function generation
           &rest metadata)
  "Construct one immutable-generation attachment without registering it."
  (e-board-runtime-attachment--create
   :board board :participant participant :harness harness :session-id session-id
   :activity-sequence 0 :generation generation :state 'active
   :turn-activity (make-hash-table :test 'equal)
   :turn-tags (make-hash-table :test 'equal)
   :turn-delivery-ids (make-hash-table :test 'equal)
   :delivery-function (or delivery-function #'e-board-runtime--deliver-to-harness)
   :instance-id (plist-get metadata :instance-id)
   :instance-catalog-generation
   (plist-get metadata :instance-catalog-generation)
   :harness-id (plist-get metadata :harness-id)
   :harness-object-generation
   (plist-get metadata :harness-object-generation)
   :session-store-id (plist-get metadata :session-store-id)
   :endpoint-token (plist-get metadata :endpoint-token)))

(defun e-board-runtime--configure-attachment (attachment)
  "Install the private board/harness ports required by ATTACHMENT."
  (let* ((board (e-board-runtime-attachment-board attachment))
         (source-board (e-board-registry-board-source-board board))
         (harness (e-board-runtime-attachment-harness attachment)))
    (setf (e-board-effect-scheduler source-board)
          (lambda (effect) (run-at-time 0 nil (lambda () (funcall effect))))
          (e-board-invocation-effect-dispatcher source-board)
          #'e-board-runtime--apply-invocation-effect
          (e-board-input-classification-scheduler source-board)
          (lambda (drain)
            (run-at-time 0 nil
                         (lambda ()
                           (e-board-runtime--drain-input-routing board drain)))))
    (e-harness-set-work-enrollment-function
     harness (lambda (handle &optional callback)
               (e-board-runtime--enroll-work harness handle callback)))
    (e-harness-set-board-aggregation-function
     harness (lambda (handles mode timeout callback invocation-context)
               (e-board-runtime--subscribe-aggregation
                harness handles mode timeout callback invocation-context)))))

(defun e-board-runtime--activate-attachment (attachment)
  "Subscribe and register ATTACHMENT after its ownership checks pass."
  (let* ((board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment))
         (harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (key (e-board-runtime--attachment-key board participant))
         (session-key (e-board-runtime--attachment-session-key attachment))
         (endpoint-key (e-board-runtime--session-key harness session-id)))
    (when (or (gethash session-key e-board-runtime--session-attachments)
              (gethash endpoint-key e-board-runtime--endpoint-attachments))
      (signal 'e-board-runtime-session-busy (list session-key endpoint-key)))
    (setf (e-board-runtime-attachment-subscription attachment)
          (e-harness-subscribe
           harness (lambda (event) (e-board-runtime--handle-harness-event attachment event))
           :session-id session-id))
    (puthash key attachment e-board-runtime--attachments)
    (puthash session-key attachment e-board-runtime--session-attachments)
    (puthash endpoint-key attachment e-board-runtime--endpoint-attachments)
    (e-board-runtime--configure-attachment attachment)
    attachment))

(defun e-board-runtime--schedule-reconciliation (reconciliation)
  "Schedule one bounded step for RECONCILIATION, at most once."
  (unless (or (e-board-runtime-reconciliation-scheduled reconciliation)
              (eq (e-board-runtime-reconciliation-state reconciliation)
                  'cancelled)
              (e-request-terminal-p
               (e-board-runtime-reconciliation-request reconciliation)))
    (setf (e-board-runtime-reconciliation-scheduled reconciliation) t)
    (run-at-time
     0 nil
     (lambda ()
       (setf (e-board-runtime-reconciliation-scheduled reconciliation) nil)
       (unless (e-request-terminal-p
                (e-board-runtime-reconciliation-request reconciliation))
         (condition-case condition
             (e-board-runtime--reconciliation-step reconciliation)
           (error
            (e-board-runtime--fail-reconciliation
             reconciliation condition))))))))

(defun e-board-runtime--fail-reconciliation (reconciliation condition)
  "Fail RECONCILIATION with CONDITION and retain a stale attachment."
  (let* ((attachment
          (e-board-runtime-reconciliation-attachment reconciliation))
         (board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment)))
    (setf (e-board-runtime-reconciliation-state reconciliation) 'failed)
    (when (eq (e-board-runtime-attachment-reconciliation attachment)
              reconciliation)
      (setf (e-board-runtime-attachment-reconciliation attachment) nil)
      (when (eq (e-board-runtime-attachment-state attachment) 'detaching)
        (setf (e-board-runtime-attachment-state attachment) 'stale)
        (when (eq (gethash (e-board-registry-participant-id participant)
                           (e-board-registry-board-participants board))
                  participant)
          (e-board-registry-set-participant-state board participant 'stale))))
    (e-request-fail
     (e-board-runtime-reconciliation-request reconciliation) condition)))

(defun e-board-runtime--finish-participant-removal (reconciliation)
  "Commit terminal participant removal for quiescent RECONCILIATION."
  (let* ((attachment
          (e-board-runtime-reconciliation-attachment reconciliation))
         (request (e-board-runtime-reconciliation-request reconciliation))
         (board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment))
         (harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment)))
    (unless (and (e-board-runtime--current-attachment-p attachment)
                 (eq (gethash (e-board-registry-participant-id participant)
                              (e-board-registry-board-participants board))
                     participant))
      (signal 'e-board-runtime-error
              (list "Participant attachment changed during removal"
                    (e-board-registry-participant-id participant))))
    (e-harness-unsubscribe
     harness (e-board-runtime-attachment-subscription attachment))
    (remhash (e-board-runtime--attachment-key board participant)
             e-board-runtime--attachments)
    (remhash (e-board-runtime--attachment-session-key attachment)
             e-board-runtime--session-attachments)
    (remhash (e-board-runtime--session-key harness session-id)
             e-board-runtime--endpoint-attachments)
    (cl-incf (e-board-runtime-attachment-generation attachment))
    (setf (e-board-runtime-attachment-state attachment) 'dormant
          (e-board-runtime-attachment-reconciliation attachment) nil
          (e-board-runtime-reconciliation-state reconciliation) 'finished)
    (e-board-registry-remove-participant board participant)
    (e-request-finish request attachment)))

(defun e-board-runtime--require-rebind-target-free (harness session-id)
  "Validate live HARNESS SESSION-ID and return its unclaimed endpoint key."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (e-board-runtime--require-live-session harness session-id)
  (let ((key (e-board-runtime--session-key harness session-id)))
    (when (or (gethash key e-board-runtime--session-attachments)
              (gethash key e-board-runtime--endpoint-attachments))
      (signal 'e-board-runtime-session-busy (list harness session-id)))
    key))

(defun e-board-runtime--prepare-rebind-attachment (reconciliation)
  "Prepare RECONCILIATION's replacement subscription without publishing it."
  (let* ((old (e-board-runtime-reconciliation-attachment reconciliation))
         (target (e-board-runtime-reconciliation-target reconciliation))
         (board (e-board-runtime-attachment-board old))
         (participant (e-board-runtime-attachment-participant old))
         (harness (plist-get target :harness))
         (session-id (plist-get target :session-id))
         (attachment
          (e-board-runtime--make-attachment
           board participant harness session-id
           (plist-get target :delivery-function)
           (1+ (e-board-runtime-attachment-generation old)))))
    (condition-case condition
        (progn
          (setf (e-board-runtime-attachment-subscription attachment)
                (e-harness-subscribe
                 harness
                 (lambda (event)
                   (e-board-runtime--handle-harness-event attachment event))
                 :session-id session-id))
          (e-board-runtime--configure-attachment attachment)
          attachment)
      (error
       (when (e-board-runtime-attachment-subscription attachment)
         (e-harness-unsubscribe
          harness (e-board-runtime-attachment-subscription attachment)))
       (signal (car condition) (cdr condition))))))

(defun e-board-runtime--finish-participant-rebind (reconciliation)
  "Atomically publish RECONCILIATION's prepared replacement attachment."
  (let* ((old (e-board-runtime-reconciliation-attachment reconciliation))
         (request (e-board-runtime-reconciliation-request reconciliation))
         (target (e-board-runtime-reconciliation-target reconciliation))
         (board (e-board-runtime-attachment-board old))
         (participant (e-board-runtime-attachment-participant old))
         (old-harness (e-board-runtime-attachment-harness old))
         (old-session-id (e-board-runtime-attachment-session-id old))
         (new-harness (plist-get target :harness))
         (new-session-id (plist-get target :session-id))
         new)
    (unless (and (e-board-runtime--current-attachment-p old)
                 (eq (gethash (e-board-registry-participant-id participant)
                              (e-board-registry-board-participants board))
                     participant))
      (signal 'e-board-runtime-error
              (list "Participant attachment changed during rebind"
                    (e-board-registry-participant-id participant))))
    (e-board-runtime--require-rebind-target-free new-harness new-session-id)
    (setq new (e-board-runtime--prepare-rebind-attachment reconciliation))
    (e-harness-unsubscribe
     old-harness (e-board-runtime-attachment-subscription old))
    (remhash (e-board-runtime--attachment-session-key old)
             e-board-runtime--session-attachments)
    (remhash (e-board-runtime--session-key old-harness old-session-id)
             e-board-runtime--endpoint-attachments)
    (cl-incf (e-board-runtime-attachment-generation old))
    (puthash (e-board-runtime--attachment-key board participant)
             new e-board-runtime--attachments)
    (puthash (e-board-runtime--attachment-session-key new)
             new e-board-runtime--session-attachments)
    (puthash (e-board-runtime--session-key new-harness new-session-id)
             new e-board-runtime--endpoint-attachments)
    (setf (e-board-runtime-attachment-state old) 'dormant
          (e-board-runtime-attachment-reconciliation old) nil
          (e-board-runtime-reconciliation-state reconciliation) 'finished)
    (e-board-registry-set-participant-state board participant 'active)
    (e-board-runtime--enqueue-ready-participant-pickup new)
    (e-request-finish request new)))

(defun e-board-runtime--prepare-move-attachment (reconciliation)
  "Prepare RECONCILIATION's destination callback before membership mutation."
  (let* ((old (e-board-runtime-reconciliation-attachment reconciliation))
         (target (e-board-runtime-reconciliation-target reconciliation))
         (destination (plist-get target :board))
         (harness (e-board-runtime-attachment-harness old))
         (session-id (e-board-runtime-attachment-session-id old))
         (attachment
          (e-board-runtime--make-attachment
           destination nil harness session-id
           (e-board-runtime-attachment-delivery-function old)
           (1+ (e-board-runtime-attachment-generation old))
           :instance-id (e-board-runtime-attachment-instance-id old)
           :instance-catalog-generation
           (e-board-runtime-attachment-instance-catalog-generation old)
           :harness-id (e-board-runtime-attachment-harness-id old)
           :harness-object-generation
           (e-board-runtime-attachment-harness-object-generation old)
           :session-store-id (e-board-runtime-attachment-session-store-id old)
           :endpoint-token (e-board-runtime-attachment-endpoint-token old))))
    (condition-case condition
        (progn
          (setf (e-board-runtime-attachment-subscription attachment)
                (e-harness-subscribe
                 harness
                 (lambda (event)
                   (when (e-board-runtime-attachment-participant attachment)
                     (e-board-runtime--handle-harness-event attachment event)))
                 :session-id session-id))
          (e-board-runtime--configure-attachment attachment)
          attachment)
      (error
       (when (e-board-runtime-attachment-subscription attachment)
         (e-harness-unsubscribe
          harness (e-board-runtime-attachment-subscription attachment)))
       (signal (car condition) (cdr condition))))))

(defun e-board-runtime--finish-participant-move (reconciliation)
  "Commit RECONCILIATION's quiescent cross-board attachment replacement."
  (let* ((old (e-board-runtime-reconciliation-attachment reconciliation))
         (request (e-board-runtime-reconciliation-request reconciliation))
         (requester
          (e-board-runtime-reconciliation-requester reconciliation))
         (target (e-board-runtime-reconciliation-target reconciliation))
         (source (e-board-runtime-attachment-board old))
         (destination (plist-get target :board))
         (destination-participant-id (plist-get target :participant-id))
         (participant (e-board-runtime-attachment-participant old))
         (harness (e-board-runtime-attachment-harness old))
         (session-id (e-board-runtime-attachment-session-id old))
         new moved-participant)
    (unless (and (e-board-runtime--current-attachment-p old)
                 (eq (gethash (e-board-registry-participant-id participant)
                              (e-board-registry-board-participants source))
                     participant))
      (signal 'e-board-runtime-error
              (list "Participant attachment changed during move"
                    (e-board-registry-participant-id participant))))
    (e-board-registry-authorize-participant-move
     source destination requester participant destination-participant-id)
    (setq new (e-board-runtime--prepare-move-attachment reconciliation))
    (condition-case condition
        (setq moved-participant
              (e-board-registry-move-participant
               source destination requester participant
               destination-participant-id))
      (error
       (e-harness-unsubscribe
        harness (e-board-runtime-attachment-subscription new))
       (signal (car condition) (cdr condition))))
    (setf (e-board-runtime-attachment-participant new) moved-participant)
    (e-harness-unsubscribe
     harness (e-board-runtime-attachment-subscription old))
    (remhash (e-board-runtime--attachment-key source participant)
             e-board-runtime--attachments)
    (remhash (e-board-runtime--attachment-session-key old)
             e-board-runtime--session-attachments)
    (remhash (e-board-runtime--session-key harness session-id)
             e-board-runtime--endpoint-attachments)
    (cl-incf (e-board-runtime-attachment-generation old))
    (puthash (e-board-runtime--attachment-key destination moved-participant)
             new e-board-runtime--attachments)
    (puthash (e-board-runtime--attachment-session-key new)
             new e-board-runtime--session-attachments)
    (puthash (e-board-runtime--session-key harness session-id)
             new e-board-runtime--endpoint-attachments)
    (setf (e-board-runtime-attachment-state old) 'dormant
          (e-board-runtime-attachment-reconciliation old) nil
          (e-board-runtime-reconciliation-state reconciliation) 'finished)
    (e-request-finish request new)))

(defun e-board-runtime--reconcile-participant-detach
    (reconciliation reason finish-function)
  "Advance one bounded terminal detach step for RECONCILIATION.
REASON tombstones source-board pickups.  FINISH-FUNCTION commits the final
membership operation after the old endpoint becomes quiescent."
  (let* ((attachment
          (e-board-runtime-reconciliation-attachment reconciliation))
         (request (e-board-runtime-reconciliation-request reconciliation))
         (board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment))
         (participant-id (e-board-registry-participant-id participant))
         (source-board (e-board-registry-board-source-board board))
         (delivery-id (car (e-board--pickup-queue source-board participant-id)))
         (pickup (and delivery-id (e-board-pickup source-board delivery-id)))
         (harness (e-board-runtime-attachment-harness attachment))
         (session-id (e-board-runtime-attachment-session-id attachment))
         (active-turn (plist-get (e-harness-state harness session-id)
                                 :active-turn)))
    (unless (e-board-runtime--current-attachment-p attachment)
      (signal 'e-board-runtime-error
              (list "Participant attachment changed during reconciliation"
                    participant-id)))
    (cond
     ((and pickup (memq (e-board-pickup-state pickup) '(pending ready)))
      (e-board-cancel-pickup source-board delivery-id reason)
      (e-request-progress
       request (list :phase 'reconciling-inbox :delivery-id delivery-id))
      (e-board-runtime--schedule-reconciliation reconciliation))
     ((and pickup
           (memq (e-board-pickup-state pickup) '(delivering accepted)))
      (e-board-cancel-pickup source-board delivery-id reason)
      (e-request-progress
       request (list :phase 'awaiting-delivery-receipt
                     :delivery-id delivery-id))
      (e-board-runtime--schedule-reconciliation reconciliation))
     (active-turn
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress
       request (list :phase 'awaiting-active-turn :turn-id active-turn)))
     ((and pickup
           (eq (e-board-pickup-state pickup) 'cancelling)
           (e-harness-discard-queued-board-input
            harness session-id delivery-id
            (e-board-delivery-attempt-endpoint-token
             (e-board-pickup-attempt pickup))
            (e-board-delivery-attempt-composite-generation
             (e-board-pickup-attempt pickup))
            reason))
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress
       request (list :phase 'discarding-session-inbox
                     :delivery-id delivery-id)))
     ((e-harness-queued-prompts harness session-id)
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress request (list :phase 'awaiting-session-inbox)))
     (pickup
      ;; Cancelling delivery remains bound to the old endpoint until the
      ;; harness proves consumption or discard through its normal receipt.
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress
       request (list :phase 'awaiting-delivery-receipt
                     :delivery-id delivery-id)))
     (t
      (funcall finish-function reconciliation)))))

(defun e-board-runtime--reconcile-participant-removal (reconciliation)
  "Advance one bounded participant-removal RECONCILIATION step."
  (e-board-runtime--reconcile-participant-detach
   reconciliation 'participant-removed
   #'e-board-runtime--finish-participant-removal))

(defun e-board-runtime--reconcile-participant-move (reconciliation)
  "Advance one bounded cross-board move RECONCILIATION step."
  (e-board-runtime--reconcile-participant-detach
   reconciliation 'participant-moved
   #'e-board-runtime--finish-participant-move))

(defun e-board-runtime--reconcile-participant-rebind (reconciliation)
  "Advance one bounded old-endpoint step for RECONCILIATION."
  (let* ((old (e-board-runtime-reconciliation-attachment reconciliation))
         (request (e-board-runtime-reconciliation-request reconciliation))
         (board (e-board-runtime-attachment-board old))
         (participant (e-board-runtime-attachment-participant old))
         (participant-id (e-board-registry-participant-id participant))
         (source-board (e-board-registry-board-source-board board))
         (delivery-id (car (e-board--pickup-queue source-board participant-id)))
         (pickup (and delivery-id (e-board-pickup source-board delivery-id)))
         (harness (e-board-runtime-attachment-harness old))
         (session-id (e-board-runtime-attachment-session-id old))
         (active-turn (plist-get (e-harness-state harness session-id)
                                 :active-turn)))
    (unless (e-board-runtime--current-attachment-p old)
      (signal 'e-board-runtime-error
              (list "Participant attachment changed during rebind"
                    participant-id)))
    (cond
     ((and pickup
           (memq (e-board-pickup-state pickup) '(delivering accepted)))
      (e-board-cancel-pickup source-board delivery-id 'endpoint-rebound)
      (e-request-progress
       request (list :phase 'awaiting-delivery-receipt
                     :delivery-id delivery-id))
      (e-board-runtime--schedule-reconciliation reconciliation))
     (active-turn
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress
       request (list :phase 'awaiting-active-turn :turn-id active-turn)))
     ((and pickup
           (eq (e-board-pickup-state pickup) 'cancelling)
           (e-harness-discard-queued-board-input
            harness session-id delivery-id
            (e-board-delivery-attempt-endpoint-token
             (e-board-pickup-attempt pickup))
            (e-board-delivery-attempt-composite-generation
             (e-board-pickup-attempt pickup))
            'endpoint-rebound))
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress
       request (list :phase 'discarding-session-inbox
                     :delivery-id delivery-id)))
     ((e-harness-queued-prompts harness session-id)
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress request (list :phase 'awaiting-session-inbox)))
     ((and pickup (eq (e-board-pickup-state pickup) 'cancelling))
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress
       request (list :phase 'awaiting-delivery-receipt
                     :delivery-id delivery-id)))
     (t
      (e-board-runtime--finish-participant-rebind reconciliation)))))

(defun e-board-runtime--reconciliation-step (reconciliation)
  "Advance one scheduled state transition for RECONCILIATION."
  (let* ((attachment
          (e-board-runtime-reconciliation-attachment reconciliation))
         (request (e-board-runtime-reconciliation-request reconciliation))
         (board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment)))
    (pcase (e-board-runtime-reconciliation-state reconciliation)
      ('prepared
       (pcase (e-board-runtime-reconciliation-kind reconciliation)
         ('remove
          (e-board-registry-authorize-participant-removal
           board (e-board-runtime-reconciliation-requester reconciliation)
           participant))
         ('rebind
          (e-board-registry-authorize-participant-rebind
           board (e-board-runtime-reconciliation-requester reconciliation)
           participant)
          (let ((target (e-board-runtime-reconciliation-target reconciliation)))
            (e-board-runtime--require-rebind-target-free
             (plist-get target :harness) (plist-get target :session-id))))
         ('move
          (let ((target (e-board-runtime-reconciliation-target reconciliation)))
            (e-board-registry-authorize-participant-move
             board (plist-get target :board)
             (e-board-runtime-reconciliation-requester reconciliation)
             participant (plist-get target :participant-id)))))
       (unless (and (e-board-runtime--current-attachment-p attachment)
                    (memq (e-board-runtime-attachment-state attachment)
                          '(active stale)))
         (signal 'e-board-runtime-error
                 (list "Participant attachment is not active"
                       (e-board-registry-participant-id participant))))
       (setf (e-board-runtime-reconciliation-state reconciliation) 'committed
             (e-board-runtime-attachment-state attachment) 'detaching)
       (e-board-registry-set-participant-state board participant 'detaching)
       (e-request-progress request (list :phase 'detaching))
       (e-board-runtime--schedule-reconciliation reconciliation))
      ((or 'committed 'waiting)
       (setf (e-board-runtime-reconciliation-state reconciliation) 'committed)
       (pcase (e-board-runtime-reconciliation-kind reconciliation)
         ('remove
          (e-board-runtime--reconcile-participant-removal reconciliation))
         ('rebind
          (e-board-runtime--reconcile-participant-rebind reconciliation))
         ('move
          (e-board-runtime--reconcile-participant-move reconciliation))
         (_
          (signal 'e-board-runtime-error
                  (list "Unknown reconciliation kind"
                        (e-board-runtime-reconciliation-kind reconciliation)))))))))

(cl-defun e-board-runtime-remove-participant-start
    (board-or-id participant-or-id requester)
  "Start removal from BOARD-OR-ID of PARTICIPANT-OR-ID for owner REQUESTER.
The returned request stays pending while the attachment drains an active turn
or accepted inbox receipt.  Before final removal, undelivered FIFO records are
retained as explicit cancellation tombstones and the session transcript is not
changed.  Cancellation is accepted only before the scheduled detach commit."
  (e-board-runtime--require-admission)
  (let* ((board (e-board-runtime--active-board board-or-id))
         (participant (e-board-registry-participant board participant-or-id))
         (_authorization
          (e-board-registry-authorize-participant-removal
           board requester participant))
         (attachment
          (gethash (e-board-runtime--attachment-key board participant)
                   e-board-runtime--attachments))
         request reconciliation)
    (unless (and attachment
                 (e-board-runtime--current-attachment-p attachment)
                 (memq (e-board-runtime-attachment-state attachment)
                       '(active stale))
                 (null (e-board-runtime-attachment-reconciliation attachment)))
      (signal 'e-board-runtime-error
              (list "Participant has no removable attachment"
                    (e-board-registry-participant-id participant))))
    (setq request
          (e-board-runtime--track-control-request
           (e-request-lifecycle-create
            :id (format "board-remove-participant-%d"
                        (cl-incf e-board-runtime--control-sequence))
            :owner 'e-board-runtime-remove-participant
            :session-id (e-board-runtime-attachment-session-id attachment)
            :generation (e-board-runtime-attachment-generation attachment)
            :state 'created)))
    (setq reconciliation
          (e-board-runtime-reconciliation--create
           :kind 'remove :request request :attachment attachment
           :requester requester :state 'prepared))
    (setf (e-board-runtime-attachment-reconciliation attachment) reconciliation
          (e-request-lifecycle-cancel-function request)
          (lambda (_request)
            (if (eq (e-board-runtime-reconciliation-state reconciliation)
                    'prepared)
                (progn
                  (setf (e-board-runtime-reconciliation-state reconciliation)
                        'cancelled)
                  (when (eq (e-board-runtime-attachment-reconciliation attachment)
                            reconciliation)
                    (setf (e-board-runtime-attachment-reconciliation attachment)
                          nil)))
              (signal 'e-board-runtime-control-committed
                      (list (e-request-lifecycle-id request))))))
    (e-request-start
     request (list :phase 'scheduled
                   :participant-id
                   (e-board-registry-participant-id participant)))
    (e-board-runtime--schedule-reconciliation reconciliation)
    request))

(cl-defun e-board-runtime-rebind-start
    (board-or-id participant-or-id harness session-id requester
                 &key delivery-function)
  "Start owner REQUESTER's controlled participant rebind on BOARD-OR-ID.
PARTICIPANT-OR-ID stops accepting new input in a later scheduled commit.  Its
old active turn and accepted delivery receipts reconcile before live HARNESS
SESSION-ID atomically replaces the old endpoint.  Undelivered FIFO records keep
their logical identities and become eligible on the replacement attachment."
  (e-board-runtime--require-admission)
  (unless (or (null delivery-function) (functionp delivery-function))
    (signal 'wrong-type-argument (list 'functionp delivery-function)))
  (e-board-runtime--require-rebind-target-free harness session-id)
  (let* ((board (e-board-runtime--active-board board-or-id))
         (participant (e-board-registry-participant board participant-or-id))
         (_authorization
          (e-board-registry-authorize-participant-rebind
           board requester participant))
         (attachment
          (gethash (e-board-runtime--attachment-key board participant)
                   e-board-runtime--attachments))
         request reconciliation)
    (unless (and attachment
                 (e-board-runtime--current-attachment-p attachment)
                 (memq (e-board-runtime-attachment-state attachment)
                       '(active stale))
                 (null (e-board-runtime-attachment-reconciliation attachment)))
      (signal 'e-board-runtime-error
              (list "Participant has no rebindable attachment"
                    (e-board-registry-participant-id participant))))
    (setq request
          (e-board-runtime--track-control-request
           (e-request-lifecycle-create
            :id (format "board-rebind-participant-%d"
                        (cl-incf e-board-runtime--control-sequence))
            :owner 'e-board-runtime-rebind-participant
            :session-id session-id
            :generation (e-board-runtime-attachment-generation attachment)
            :state 'created)))
    (setq reconciliation
          (e-board-runtime-reconciliation--create
           :kind 'rebind :request request :attachment attachment
           :requester requester
           :target (list :harness harness :session-id session-id
                         :delivery-function delivery-function)
           :state 'prepared))
    (setf (e-board-runtime-attachment-reconciliation attachment) reconciliation
          (e-request-lifecycle-cancel-function request)
          (lambda (_request)
            (if (eq (e-board-runtime-reconciliation-state reconciliation)
                    'prepared)
                (progn
                  (setf (e-board-runtime-reconciliation-state reconciliation)
                        'cancelled)
                  (when (eq (e-board-runtime-attachment-reconciliation attachment)
                            reconciliation)
                    (setf (e-board-runtime-attachment-reconciliation attachment)
                          nil)))
              (signal 'e-board-runtime-control-committed
                      (list (e-request-lifecycle-id request))))))
    (e-request-start
     request (list :phase 'scheduled
                   :participant-id
                   (e-board-registry-participant-id participant)
                   :session-id session-id))
    (e-board-runtime--schedule-reconciliation reconciliation)
    request))

(cl-defun e-board-runtime-move-participant-start
    (source-board-or-id participant-or-id destination-board-or-id requester
                        &key destination-participant-id)
  "Start REQUESTER's controlled cross-board move of PARTICIPANT-OR-ID.
SOURCE-BOARD-OR-ID and DESTINATION-BOARD-OR-ID name the membership boundary;
DESTINATION-PARTICIPANT-ID defaults to the old local id.  The source attachment
first fences ingress and reconciles its active turn, accepted receipt, and
bounded FIFO.  The quiescent endpoint then receives a fresh destination
membership and attachment generation in one scheduled commit.  Source messages
and pickup tombstones remain on the source board."
  (e-board-runtime--require-admission)
  (let* ((source (e-board-runtime--active-board source-board-or-id))
         (destination
          (e-board-runtime--active-board destination-board-or-id))
         (participant
          (e-board-registry-participant source participant-or-id))
         (destination-id
          (e-board-registry-authorize-participant-move
           source destination requester participant destination-participant-id))
         (attachment
          (gethash (e-board-runtime--attachment-key source participant)
                   e-board-runtime--attachments))
         request reconciliation)
    (unless (and attachment
                 (e-board-runtime--current-attachment-p attachment)
                 (memq (e-board-runtime-attachment-state attachment)
                       '(active stale))
                 (null (e-board-runtime-attachment-reconciliation attachment)))
      (signal 'e-board-runtime-error
              (list "Participant has no movable attachment"
                    (e-board-registry-participant-id participant))))
    (setq request
          (e-board-runtime--track-control-request
           (e-request-lifecycle-create
            :id (format "board-move-participant-%d"
                        (cl-incf e-board-runtime--control-sequence))
            :owner 'e-board-runtime-move-participant
            :session-id (e-board-runtime-attachment-session-id attachment)
            :generation (e-board-runtime-attachment-generation attachment)
            :state 'created)))
    (setq reconciliation
          (e-board-runtime-reconciliation--create
           :kind 'move :request request :attachment attachment
           :requester requester
           :target (list :board destination :participant-id destination-id)
           :state 'prepared))
    (setf (e-board-runtime-attachment-reconciliation attachment) reconciliation
          (e-request-lifecycle-cancel-function request)
          (lambda (_request)
            (if (eq (e-board-runtime-reconciliation-state reconciliation)
                    'prepared)
                (progn
                  (setf (e-board-runtime-reconciliation-state reconciliation)
                        'cancelled)
                  (when (eq (e-board-runtime-attachment-reconciliation attachment)
                            reconciliation)
                    (setf (e-board-runtime-attachment-reconciliation attachment)
                          nil)))
              (signal 'e-board-runtime-control-committed
                      (list (e-request-lifecycle-id request))))))
    (e-request-start
     request (list :phase 'scheduled
                   :participant-id
                   (e-board-registry-participant-id participant)
                   :destination-board-id
                   (e-board-registry-board-id destination)
                   :destination-participant-id destination-id))
    (e-board-runtime--schedule-reconciliation reconciliation)
    request))

(defun e-board-runtime--deliver-pickups (board pickup-ids)
  "Deliver BOARD's frozen ready PICKUP-IDS through their attachments."
  (let ((source-board (e-board-registry-board-source-board board)))
    (dolist (delivery-id pickup-ids)
      (when-let ((pickup (e-board-pickup source-board delivery-id)))
        (when (eq (e-board-pickup-state pickup) 'ready)
          (let* ((participant-id (e-board-pickup-participant-id pickup))
                 (participant
                  (gethash participant-id
                           (e-board-registry-board-participants board)))
                 (authorization
                  (e-board-registry-participant-delivery-authorization
                   board (or participant participant-id)
                   (e-board-pickup-requester-actor pickup)
                   (e-board-pickup-addressed-p pickup))))
            (pcase authorization
              ('revoked
               (when-let ((next-id
                           (e-board-fail-pickup
                            source-board delivery-id 'delivery-authorization-revoked)))
                 (e-board-runtime--enqueue-pickups board (list next-id))))
              ('authorized
               (when-let* ((attachment
                           (gethash
                             (e-board-runtime--attachment-key board participant)
                             e-board-runtime--attachments))
                           ((e-board-runtime--current-attachment-p attachment))
                           ((eq (e-board-runtime-attachment-state attachment)
                                'active))
                           (message
                            (e-board-message
                             source-board (e-board-pickup-message-id pickup))))
                 (e-board-pickup-start-delivery
                  source-board delivery-id
                  (e-board-runtime-attachment-endpoint-token attachment)
                  (e-board-runtime--attachment-composite-generation attachment))
                 (condition-case err
                     (let ((result
                            (funcall
                             (e-board-runtime-attachment-delivery-function attachment)
                             attachment pickup message)))
                       (pcase (car-safe result)
                         (:accepted
                          (if (and
                               (e-board-runtime--current-attachment-p attachment)
                               (e-board-runtime--attempt-belongs-to-attachment-p
                                pickup attachment))
                              (progn
                                (e-board-pickup-accept-delivery
                                 source-board delivery-id (cadr result))
                                (unless
                                    (eq (e-board-registry-participant-delivery-authorization
                                         board participant
                                         (e-board-pickup-requester-actor pickup)
                                         (e-board-pickup-addressed-p pickup))
                                        'authorized)
                                  (e-board-cancel-pickup
                                   source-board delivery-id
                                   'delivery-authorization-revoked)))
                            (when-let ((next-id
                                        (e-board-pickup-mark-uncertain
                                         source-board delivery-id
                                         'acceptance-endpoint-changed)))
                              (e-board-runtime--enqueue-pickups
                               board (list next-id)))))
                         (:uncertain
                          (when-let ((next-id
                                      (e-board-pickup-mark-uncertain
                                       source-board delivery-id
                                       (or (cadr result) 'delivery-uncertain))))
                            (e-board-runtime--enqueue-pickups board (list next-id))))
                         (:discarded
                          (e-board-pickup-accept-delivery source-board delivery-id)
                          (when-let ((next-id
                                      (e-board-pickup-discard-delivery
                                       source-board delivery-id
                                       (or (cadr result) 'delivery-discarded))))
                            (e-board-runtime--enqueue-pickups board (list next-id))))
                         (:failed
                          (when-let ((next-id
                                      (e-board-fail-pickup
                                       source-board delivery-id
                                       (or (cadr result) 'delivery-failed))))
                            (e-board-runtime--enqueue-pickups board (list next-id))))
                         (_
                          (when-let ((next-id
                                      (e-board-pickup-complete-delivery
                                       source-board delivery-id)))
                            (e-board-runtime--enqueue-pickups
                             board (list next-id))))))
                   (error
                    (e-board-pickup-return-ready source-board delivery-id err)
                    (unless (eq (car err) 'e-board-runtime-session-busy)
                      (if (>= (e-board-delivery-attempt-number
                               (e-board-pickup-attempt pickup))
                              e-board-runtime-pickup-retry-limit)
                          (when-let ((next-id
                                      (e-board-fail-pickup
                                       source-board delivery-id
                                       'delivery-retry-exhausted)))
                            (e-board-runtime--enqueue-pickups
                             board (list next-id)))
                        (e-board-runtime--enqueue-pickups
                         board (list delivery-id)))))))))))))))

(cl-defun e-board-runtime--post-client-input
    (board-or-id &key id author tags attributes to requester
                 (mode 'inject) content reference source-input-key)
  "Post input using authenticated client REQUESTER without opening admission."
  (unless requester
    (signal 'e-board-registry-authorization-denied
            (list (if (e-board-registry-board-p board-or-id)
                      (e-board-registry-board-id board-or-id)
                    board-or-id)
                  nil 'missing-requester)))
  (let* ((board (e-board-runtime--active-board board-or-id))
         (target (and to (e-board-registry-participant board to)))
         (requester-actor
          (e-board-registry-resolve-requester-actor board requester))
         (_authorization
          (and target
               (e-board-registry-authorize-actor-exact-post
                board requester-actor target)))
         (publication
          (e-board-post-input
           (e-board-registry-board-source-board board)
           :id id :author author :requester-actor requester-actor
           :tags tags :attributes attributes
           :to (and target (e-board-registry-participant-id target))
           :mode mode :content content
           :reference reference :source-input-key source-input-key)))
    (e-board-runtime--enqueue-pickups
     board (e-board-publication-pickup-ids publication))
    publication))

(cl-defun e-board-runtime-post-input
    (board-or-id &key id author tags attributes to requester
                 (mode 'inject) content reference source-input-key)
  "Post one input to BOARD-OR-ID's source board and enqueue its frozen pickups.
The returned value is the source board's `e-board-publication'.  Duplicate
publications only retry pickups that remain pending.  REQUESTER must be an
active registry client requester context; exact posts additionally
require authority for their target participant."
  (e-board-runtime--require-admission)
  (e-board-runtime--post-client-input
   board-or-id :id id :author author :tags tags :attributes attributes
   :to to :requester requester :mode mode :content content
   :reference reference :source-input-key source-input-key))

(cl-defun e-board-runtime-post-participant-input
    (attachment &key id author tags attributes to (mode 'inject)
                content reference source-input-key)
  "Post input as current board ATTACHMENT's authenticated participant actor."
  (e-board-runtime--require-admission)
  (unless (and (e-board-runtime--current-attachment-p attachment)
               (eq (e-board-runtime-attachment-state attachment) 'active))
    (signal 'e-board-runtime-error (list "Stale participant attachment")))
  (let* ((board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment))
         (target (and to (e-board-registry-participant board to)))
         (actor (list 'participant
                      (e-board-registry-participant-id participant))))
    (when target
      (e-board-registry-authorize-actor-exact-post board actor target))
    (let ((publication
           (e-board-post-input
            (e-board-registry-board-source-board board)
            :id id :author author :requester-actor actor :tags tags
            :attributes attributes
            :to (and target (e-board-registry-participant-id target))
            :mode mode :content content :reference reference
            :source-input-key source-input-key)))
      (e-board-runtime--enqueue-pickups
       board (e-board-publication-pickup-ids publication))
      publication)))

(defun e-board-runtime-attachment-active-turn-p (attachment)
  "Return non-nil when current ATTACHMENT owns a live harness turn."
  (and (e-board-runtime--current-attachment-p attachment)
       (plist-get
        (e-harness-state (e-board-runtime-attachment-harness attachment)
                         (e-board-runtime-attachment-session-id attachment))
        :active-turn)))

(defun e-board-runtime-abort-attachment (attachment)
  "Abort the current board-bound ATTACHMENT turn through runtime admission."
  (e-board-runtime--require-admission)
  (unless (e-board-runtime--current-attachment-p attachment)
    (signal 'e-board-runtime-error (list "Stale attachment" attachment)))
  (e-harness-abort (e-board-runtime-attachment-harness attachment)
                   (e-board-runtime-attachment-session-id attachment)))

(provide 'e-board-runtime)

;;; e-board-runtime.el ends here

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
(require 'e-board)
(require 'e-board-admission)
(require 'e-board-registry)
(require 'e-board-runtime-admission)
(require 'e-board-runtime-error)
(require 'e-board-pickup-admission)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-harness-registry)
(require 'e-request)
(require 'e-session)
(require 'e-work)
(require 'seq)

(define-error 'e-board-runtime-attachment-exists
  "e board runtime participant is already attached"
  'e-board-runtime-error)
(define-error 'e-board-runtime-session-busy
  "e board runtime session is busy"
  'e-board-runtime-error)
(define-error 'e-board-runtime-session-missing
  "e board runtime session is missing"
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
(define-error 'e-board-runtime-invalid-work
  "e board runtime work has invalid provenance"
  'e-board-runtime-error)
(define-error 'e-board-runtime-invalid-activity
  "e board runtime activity has invalid public attributes"
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

(cl-defstruct (e-board-runtime-producer-delivery
               (:constructor e-board-runtime--producer-delivery-create)
               (:conc-name e-board-runtime-producer-delivery-))
  "One producer delivery with its exact runtime attachment lease.
The attachment is optional for ordinary board participants; when present, the
generation is captured at routing commit and is the only authority allowed to
settle a later consumed turn."
  id item attachment generation)

(cl-defstruct (e-board-runtime-producer-turn
               (:constructor e-board-runtime--producer-turn-create)
               (:conc-name e-board-runtime-producer-turn-))
  "One consumed producer delivery awaiting its exact turn terminal event."
  id item delivery-id attachment generation)

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

(defvar e-board-runtime--activity-drain-generation 0
  "Generation fencing scheduled activity drains after attachment retirement.")

(defconst e-board-runtime-pickup-drain-limit 16
  "Maximum frozen pickup attempts the private runtime starts per drain.")

(defconst e-board-runtime-pickup-retry-limit 3
  "Maximum proven-uncommitted delivery attempts before visible failure.")

(defconst e-board-runtime--visible-harness-activity-types
  '(turn-started provider-request-started provider-request-finished
    turn-retrying
    tool-started tool-finished action-started action-finished action-failed
    hook-audit turn-steered compaction-started compaction-finished
    compaction-failed)
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

(defvar e-board-runtime--pickup-drain-generation 0
  "Generation fencing scheduled pickup drains after attachment retirement.")

(cl-defstruct (e-board-runtime-attachment
               (:constructor e-board-runtime-attachment--create)
               (:conc-name e-board-runtime-attachment-))
  board participant harness session-id delivery-function subscription activity-sequence generation
  turn-activity turn-tags turn-delivery-ids turn-port instance-id instance-catalog-generation
  harness-id harness-object-generation endpoint-token state reconciliation
  identity-token
  owned-pickup-ids producer-delivery-ids producer-turn-keys activity-mailbox-keys
  invocation-targets
  retirement-stage retirement-authorized-p retirement-work-authorized-p
  retirement-generation)

(cl-defstruct (e-board-runtime-reconciliation
               (:constructor e-board-runtime-reconciliation--create)
               (:conc-name e-board-runtime-reconciliation-))
  kind request attachment requester target state scheduled)

(cl-defstruct (e-board-runtime-endpoint-token
               (:constructor e-board-runtime-endpoint-token--create)
               (:conc-name e-board-runtime-endpoint-token-))
  harness-id harness-object-generation session-id)

(cl-defstruct (e-board-runtime-turn-activity
               (:constructor e-board-runtime-turn-activity--create)
               (:conc-name e-board-runtime-turn-activity-))
  provider-seen provider-started-at tool-count action-count)

(cl-defstruct (e-board-runtime-invocation
               (:constructor e-board-runtime-invocation--create)
               (:conc-name e-board-runtime-invocation-))
  target attachment attachment-generation endpoint-token composite-generation
  callback state counted-p)

(cl-defstruct (e-board-runtime-invocation-lease
               (:constructor e-board-runtime-invocation-lease--create)
               (:conc-name e-board-runtime-invocation-lease--))
  "Opaque exact authority for one runtime invocation admission.

The descriptive target is deliberately carried with the invocation object;
callers must pass this lease through board staging and later effects rather
than resolving a potentially newer invocation by an equal target key."
  target invocation)

(cl-defstruct (e-board-runtime-work-hooks
               (:constructor e-board-runtime-work-hooks--create)
               (:conc-name e-board-runtime-work-hooks-))
  "Exact Work hook identities installed by one runtime admission attempt."
  dispatcher activity-observer)

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
             (copy-sequence
              (e-board-runtime-producer-publication-pending-delivery-ids item)))
      (let ((record (gethash delivery-id e-board-runtime--producer-deliveries)))
        (when (e-board-runtime-producer-delivery-p record)
          (when-let ((attachment
                      (e-board-runtime-producer-delivery-attachment record)))
            (remhash delivery-id
                     (e-board-runtime-attachment-producer-delivery-ids
                      attachment))))
        (remhash delivery-id e-board-runtime--producer-deliveries)))
    (setf (e-board-runtime-producer-publication-pending-delivery-ids item) nil)
    (e-board-runtime--adjust-unsettled-count 'producer -1)
    (when-let ((callback
                (e-board-runtime-producer-publication-on-settle item)))
      (apply callback (append (list :status status) payload)))))

(defun e-board-runtime--producer-delivery-record (delivery-id)
  "Return the exact producer DELIVERY-ID record, if still unsettled."
  (gethash delivery-id e-board-runtime--producer-deliveries))

(defun e-board-runtime--remember-producer-delivery
    (item delivery-id &optional attachment)
  "Register DELIVERY-ID with its exact ATTACHMENT generation, if any."
  (let ((record
         (e-board-runtime--producer-delivery-create
          :id delivery-id :item item :attachment attachment
          :generation (and attachment
                           (e-board-runtime--attachment-work-generation
                            attachment)))))
    (puthash delivery-id record e-board-runtime--producer-deliveries)
    (when attachment
      (puthash delivery-id t
               (e-board-runtime-attachment-producer-delivery-ids attachment)))
    record))

(defun e-board-runtime--settle-producer-delivery-for-attachment
    (attachment delivery-id status payload)
  "Settle DELIVERY-ID only when its record belongs to ATTACHMENT generation."
  (when-let ((record (e-board-runtime--producer-delivery-record delivery-id)))
    (when (and (eq (e-board-runtime-producer-delivery-attachment record)
                   attachment)
               (= (e-board-runtime-producer-delivery-generation record)
                  (e-board-runtime--attachment-work-generation attachment)))
      (e-board-runtime--producer-delivery-terminal
       (e-board-runtime-producer-delivery-item record)
       delivery-id status payload))))

(defun e-board-runtime--settle-producer-delivery-id
    (delivery-id status payload)
  "Settle the exact producer DELIVERY-ID without resolving a replacement.
This is used when a board pickup has reached a terminal route decision but its
original runtime attachment is no longer current.  The producer delivery table
is keyed by the causal delivery identity, so this operation never scans or
rediscovers an attachment by participant id."
  (when-let ((record (e-board-runtime--producer-delivery-record delivery-id)))
    (e-board-runtime--producer-delivery-terminal
     (e-board-runtime-producer-delivery-item record)
     delivery-id status payload)))

(defun e-board-runtime--settle-producer-turns-for-attachment (attachment)
  "Settle every producer turn key indexed by ATTACHMENT, bounded locally."
  (maphash
   (lambda (key _owned)
     (when-let ((record (gethash key e-board-runtime--producer-turns)))
       (when (and (e-board-runtime-producer-turn-p record)
                  (eq (e-board-runtime-producer-turn-attachment record)
                      attachment)
                  (= (e-board-runtime-producer-turn-generation record)
                     (e-board-runtime--attachment-work-generation attachment)))
         (remhash key e-board-runtime--producer-turns)
         (e-board-runtime--producer-delivery-terminal
          (e-board-runtime-producer-turn-item record)
          (e-board-runtime-producer-turn-delivery-id record)
          'cancelled '(:reason attachment-retired))))
     (remhash key
              (e-board-runtime-attachment-producer-turn-keys attachment)))
   (copy-hash-table
   (e-board-runtime-attachment-producer-turn-keys attachment))))

(defun e-board-runtime--settle-producer-deliveries-for-attachment
    (attachment)
  "Settle every producer delivery indexed by ATTACHMENT, bounded locally.
The pickup index normally contains the same ids, but the producer index is the
authoritative unsettled-work set: retaining this second local pass covers an
accepted/consumed record whose board pickup has already reached a terminal
projection without scanning unrelated producer work."
  (maphash
   (lambda (delivery-id _owned)
     (e-board-runtime--settle-producer-delivery-for-attachment
      attachment delivery-id 'cancelled
      (list :reason 'attachment-retired)))
   (copy-hash-table
    (e-board-runtime-attachment-producer-delivery-ids attachment))))

(defun e-board-runtime--transfer-attachment-pending-work (old new)
  "Transfer OLD's still-pending producer deliveries to replacement NEW.

Participant rebind deliberately preserves pending and ready board pickups, so
their producer accounting follows the exact surviving pickup into NEW's
generation.  Every other producer delivery is terminal by the time rebind
commits and is settled against OLD instead of being copied across a lifetime
boundary.  The operation is bounded by OLD's attachment-local indexes."
  (let ((new-generation (e-board-runtime-attachment-generation new))
        (source-board
         (e-board-registry-board-source-board
          (e-board-runtime-attachment-board old))))
    (maphash
     (lambda (delivery-id _owned)
       (let ((record (gethash delivery-id e-board-runtime--producer-deliveries))
             (pickup (e-board-pickup source-board delivery-id)))
         (cond
          ((and (e-board-runtime-producer-delivery-p record)
                (eq (e-board-runtime-producer-delivery-attachment record) old)
                (= (e-board-runtime-producer-delivery-generation record)
                   (e-board-runtime--attachment-work-generation old))
                pickup
                (memq (e-board-pickup-state pickup) '(pending ready)))
           (setf (e-board-runtime-producer-delivery-attachment record) new
                 (e-board-runtime-producer-delivery-generation record)
                 new-generation)
           (puthash delivery-id t
                    (e-board-runtime-attachment-producer-delivery-ids new)))
          ((and (e-board-runtime-producer-delivery-p record)
                (eq (e-board-runtime-producer-delivery-attachment record) old)
                (= (e-board-runtime-producer-delivery-generation record)
                   (e-board-runtime--attachment-work-generation old)))
           ;; A terminal pickup or an otherwise missing record cannot receive
           ;; a later old-endpoint receipt.  Close it before OLD is dormant.
           (e-board-runtime--producer-delivery-terminal
            (e-board-runtime-producer-delivery-item record)
            delivery-id 'cancelled '(:reason endpoint-rebound)))))
       (remhash delivery-id
                (e-board-runtime-attachment-producer-delivery-ids old)))
     (copy-hash-table
      (e-board-runtime-attachment-producer-delivery-ids old)))
    ;; The surviving participant FIFO is re-indexed by NEW on activation.  A
    ;; local ownership entry is retained only for a pickup that remains live;
    ;; terminal records must not be revisited by NEW retirement.
    (maphash
     (lambda (delivery-id _owned)
       (when-let ((pickup (e-board-pickup source-board delivery-id)))
         (when (memq (e-board-pickup-state pickup) '(pending ready))
           (puthash delivery-id t
                    (e-board-runtime-attachment-owned-pickup-ids new))))
       (remhash delivery-id
                (e-board-runtime-attachment-owned-pickup-ids old)))
     (copy-hash-table
      (e-board-runtime-attachment-owned-pickup-ids old)))))

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
          (let* ((pickup (e-board-pickup source delivery-id))
                 ;; The board pickup retains the exact participant lifetime
                 ;; captured by grouped classification.  Only that lifetime
                 ;; may resolve a runtime attachment; a same-id replacement is
                 ;; deliberately treated as stale causal work.
                 (route-current-p
                  (and pickup
                       (e-board-pickup-route-current-p source pickup)))
                 (attachment
                  (and route-current-p
                       (e-board-runtime--attachment-for-pickup
                        board pickup))))
            (e-board-runtime--remember-producer-delivery
             item delivery-id
             (and (e-board-runtime--current-attachment-p attachment)
                  attachment))
            ;; A routed pickup whose participant lifetime is already gone can
            ;; never be delivered by this producer again.  Close the exact
            ;; causal slot now; leaving a nil attachment record would strand
            ;; producer accounting until an impossible future receipt.  The
            ;; board pickup is settled here as well, rather than waiting for a
            ;; runtime drain that may never run after the owning attachment was
            ;; retired.  Only the ids in this routed transaction are touched.
            (unless route-current-p
              (when (and pickup
                         (memq (e-board-pickup-state pickup) '(pending ready)))
                (when-let ((next-id
                            (e-board-fail-pickup
                             source delivery-id 'participant-lifetime-changed)))
                  (e-board-runtime--enqueue-pickups board (list next-id))))
              (e-board-runtime--settle-producer-delivery-id
               delivery-id 'cancelled
               (list :reason 'participant-lifetime-changed
                     :message-id message-id)))))))))

(defun e-board-runtime--producer-delivery-terminal (item delivery-id status payload)
  "Record one producer ITEM DELIVERY-ID terminal STATUS and PAYLOAD."
  (when (member delivery-id
                (e-board-runtime-producer-publication-pending-delivery-ids item))
    (let ((record (e-board-runtime--producer-delivery-record delivery-id)))
      (when (and (e-board-runtime-producer-delivery-p record)
                 (eq (e-board-runtime-producer-delivery-item record) item))
        (when-let ((attachment
                    (e-board-runtime-producer-delivery-attachment record)))
          (remhash delivery-id
                   (e-board-runtime-attachment-producer-delivery-ids attachment)))
        (remhash delivery-id e-board-runtime--producer-deliveries))
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
           item final (list :results results)))))))

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
    e-harness-aggregate-unsettled-change-hook
    e-work--unsettled-change-functions
    e-session-storage-unsettled-change-hook
    e-task-queue--unsettled-change-functions)
  "Ordinary hook variables observed by controlled quiescence requests.

The Board registry is subscribed through its public listener operation below;
its hook variable remains private to that owner.")

(defun e-board-runtime--quiescence-sources ()
  "Return all constant-time process-local unsettled source projections."
  (append
   (list :runtime (e-board-runtime-unsettled-state)
         :harnesses (e-harness-aggregate-unsettled-state)
         :work (e-work-unsettled-state)
         :boards (e-board-registry-unsettled-state)
         :persistence (e-session-persistence-unsettled-state))
   (when (fboundp 'e-task-queue-unsettled-state)
     (list :task-persistence (e-task-queue-unsettled-state)))))

(defun e-board-runtime--quiescence-blockers (sources)
  "Return the nonzero unsettled counters from SOURCES."
  (let (blockers)
    (dolist (source '(:runtime :harnesses :work :boards :persistence
                      :task-persistence))
      (let ((state (plist-get sources source)))
        (while state
          (let ((key (pop state))
                (value (pop state)))
            (unless (eq key :generation)
              (when (and (integerp value) (> value 0))
                (push (cons
                       (intern
                        (format "%s.%s"
                                (string-remove-prefix ":" (symbol-name source))
                                (string-remove-prefix ":" (symbol-name key))))
                       value)
                      blockers)))))))
    (nreverse blockers)))

(defun e-board-runtime--quiescence-unsubscribe ()
  "Remove the controlled quiescence transition observer from all owners."
  (dolist (hook e-board-runtime--quiescence-change-hooks)
    (remove-hook hook #'e-board-runtime--quiescence-source-changed))
  (e-board-registry-remove-unsettled-listener
   #'e-board-runtime--quiescence-source-changed))

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
    (e-board-registry-add-unsettled-listener
     #'e-board-runtime--quiescence-source-changed)
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

(defun e-board-runtime--terminalize-invocation (target invocation state)
  "Commit INVOCATION's terminal STATE exactly once for TARGET.
  The table removal happens before unsettled notification because the
  notification
may re-enter runtime teardown.  A second transition therefore observes neither
the live invocation nor an unsettled slot and cannot decrement the counter
again.  Return non-nil only when this call owned the terminal transition."
  (when (eq (gethash target e-board-runtime--invocations) invocation)
    (setf (e-board-runtime-invocation-state invocation) state)
    ;; Remove the exact table slot and attachment index before any fallible
    ;; notification.  A reentrant retirement therefore cannot find this
    ;; invocation and decrement its count a second time.
    (remhash target e-board-runtime--invocations)
    (let ((attachment (e-board-runtime-invocation-attachment invocation)))
      (when (and (e-board-runtime-attachment-p attachment)
                 (eq (gethash target
                              (e-board-runtime-attachment-invocation-targets
                               attachment))
                     invocation))
        (remhash target
                 (e-board-runtime-attachment-invocation-targets attachment))))
    (when (e-board-runtime-invocation-counted-p invocation)
      ;; Clear ownership before calling the fallible observer.  If that
      ;; observer re-enters retirement, it sees neither the target nor a second
      ;; count token; if the observer itself signals, the counter is already
      ;; correct and a caller can preserve the initiating error.
      (setf (e-board-runtime-invocation-counted-p invocation) nil)
      (e-board-runtime--adjust-unsettled-count 'invocation -1))
    t))

(defun e-board-runtime--invocation-lease-current-p (lease)
  "Return non-nil when LEASE still owns its exact live invocation authority.

This check is intentionally composed from object identities and owner-local
state.  Equal target values are not sufficient: a same-key replacement must
remain untouched when an older admission resumes after reentrant retirement."
  (when (e-board-runtime-invocation-lease-p lease)
    (let* ((target (e-board-runtime-invocation-lease--target lease))
           (invocation (e-board-runtime-invocation-lease--invocation lease))
           (attachment (and (e-board-runtime-invocation-p invocation)
                            (e-board-runtime-invocation-attachment invocation))))
      (and (e-board-runtime-invocation-p invocation)
           (e-board-runtime-attachment-p attachment)
           (eq (gethash target e-board-runtime--invocations) invocation)
           (eq (gethash
                target
                (e-board-runtime-attachment-invocation-targets attachment))
               invocation)
           (e-board-runtime-invocation-counted-p invocation)
           (eq (e-board-runtime-invocation-state invocation) 'open)
           (e-board-runtime--current-active-attachment-p attachment)))))

(defun e-board-runtime--drop-invocation (lease)
  "Remove exact LEASE and retire it from unsettled accounting when necessary.
An equal target held by a replacement is never rediscovered or removed."
  (when (e-board-runtime-invocation-lease-p lease)
    (let ((target (e-board-runtime-invocation-lease--target lease))
          (invocation (e-board-runtime-invocation-lease--invocation lease)))
      (when (and (e-board-runtime-invocation-p invocation)
                 (eq (gethash target e-board-runtime--invocations)
                     invocation))
        (e-board-runtime--terminalize-invocation target invocation 'cancelled)
        invocation))))

(defun e-board-runtime--attachment-key (board participant)
  "Return the attachment lookup key for BOARD and PARTICIPANT."
  (list (e-board-registry-board-id board)
         (e-board-registry-participant-id participant)))

(defun e-board-runtime--session-key (harness session-id)
  "Return the stable endpoint lookup key for HARNESS SESSION-ID.
The aggregate is mutable, so using it directly as an `equal' hash key would
make endpoint membership disappear when an owner updates its state.  The
immutable identity token belongs to the harness aggregate and is distinct for
each live harness object."
  (list (e-harness-identity-token harness) session-id))

(defun e-board-runtime--attachment-session-key (attachment)
  "Return ATTACHMENT's concrete reverse session identity."
  (e-board-runtime--session-key
   (e-board-runtime-attachment-harness attachment)
   (e-board-runtime-attachment-session-id attachment)))

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
  (when (e-board-runtime-attachment-p attachment)
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
             attachment)))))

(defun e-board-runtime--current-active-attachment-p (attachment)
  "Return non-nil when ATTACHMENT is current and still accepting callbacks."
  (and (e-board-runtime--current-attachment-p attachment)
       (eq (e-board-runtime-attachment-state attachment) 'active)))

(defun e-board-runtime--attachment-work-generation (attachment)
  "Return the generation owning ATTACHMENT-local terminal work.
During retirement the live generation is bumped immediately to fence new
callbacks; the saved retirement generation still names work accepted by that
attachment before the transition began."
  (or (e-board-runtime-attachment-retirement-generation attachment)
      (e-board-runtime-attachment-generation attachment)))

(defun e-board-runtime--attachment-for-pickup (board pickup)
  "Return BOARD's current attachment for frozen PICKUP, if any.
This lookup is one participant-local map operation; it never searches the
board's global pickup table for an owner."
  (let ((source-board (e-board-registry-board-source-board board)))
    (when (e-board-pickup-route-current-p source-board pickup)
      (when-let ((participant
                  (gethash (e-board-pickup-participant-id pickup)
                           (e-board-registry-board-participants board))))
        (gethash (e-board-runtime--attachment-key board participant)
                 e-board-runtime--attachments)))))

(defun e-board-runtime--index-attachment-pickup (attachment pickup-id)
  "Remember PICKUP-ID in ATTACHMENT's exact local ownership index."
  (when (e-board-runtime--current-active-attachment-p attachment)
    (puthash pickup-id t
             (e-board-runtime-attachment-owned-pickup-ids attachment))))

(defun e-board-runtime--forget-attachment-pickup (attachment pickup-id)
  "Forget terminal PICKUP-ID from ATTACHMENT's local ownership index.
The index is for unsettled attachment work, not a second board history.  Keeping
only live FIFO identities bounds ordinary delivery overhead while retirement
still has its producer-delivery and producer-turn indexes for consumed work."
  (remhash pickup-id
           (e-board-runtime-attachment-owned-pickup-ids attachment)))

(defun e-board-runtime--forget-terminal-pickup
    (attachment pickup-id)
  "Drop PICKUP-ID when its board projection has reached a terminal state."
  (when-let ((pickup
              (e-board-pickup
               (e-board-registry-board-source-board
                (e-board-runtime-attachment-board attachment))
               pickup-id)))
    (when (memq (e-board-pickup-state pickup)
                '(consumed cancelled discarded failed uncertain expired overflowed))
      (e-board-runtime--forget-attachment-pickup attachment pickup-id))))

(defun e-board-runtime--forget-terminal-pickup-for-board
    (board pickup-id)
  "Forget one terminal PICKUP-ID through BOARD's participant-local owner."
  (when-let* ((source-board (e-board-registry-board-source-board board))
              (pickup (e-board-pickup source-board pickup-id))
              (attachment (e-board-runtime--attachment-for-pickup
                           board pickup)))
    (e-board-runtime--forget-terminal-pickup attachment pickup-id)))

(defun e-board-runtime--remove-pending-pickup-key (key)
  "Remove one exact pending pickup KEY and settle its queue counter."
  (let ((cursor e-board-runtime--pending-pickup-head)
        previous)
    (while cursor
      (let ((next (cdr cursor)))
        (if (equal (car cursor) key)
            (progn
              (if previous
                  (setcdr previous next)
                (setq e-board-runtime--pending-pickup-head next))
              (when (eq cursor e-board-runtime--pending-pickup-tail)
                (setq e-board-runtime--pending-pickup-tail previous))
              (when (gethash key e-board-runtime--pending-pickup-set)
                (remhash key e-board-runtime--pending-pickup-set)
                (e-board-runtime--unsettled-changed))
              (setq cursor nil))
          (setq previous cursor
                cursor next))))
    (unless e-board-runtime--pending-pickup-head
      (setq e-board-runtime--pending-pickup-tail nil))))

(defun e-board-runtime--remove-pending-activity-key (key)
  "Remove one exact pending activity KEY and settle its queue counter."
  (let ((cursor e-board-runtime--pending-activity-head)
        previous)
    (while cursor
      (let ((next (cdr cursor)))
        (if (equal (car cursor) key)
            (progn
              (if previous
                  (setcdr previous next)
                (setq e-board-runtime--pending-activity-head next))
              (when (eq cursor e-board-runtime--pending-activity-tail)
                (setq e-board-runtime--pending-activity-tail previous))
              (when (gethash key e-board-runtime--pending-activity-set)
                (remhash key e-board-runtime--pending-activity-set)
                (e-board-runtime--unsettled-changed))
              (setq cursor nil))
          (setq previous cursor
                cursor next))))
    (unless e-board-runtime--pending-activity-head
      (setq e-board-runtime--pending-activity-tail nil))))

(defun e-board-runtime--retire-pickups-for-attachment (attachment)
  "Settle every nonterminal pickup in ATTACHMENT's local ownership index.
Pending and ready records are cancelled; an in-flight, accepted, or consumed
record is tombstoned as uncertain and its exact producer delivery/turn slot is
settled.  No successor is enqueued: terminal attachment cleanup owns the
complete participant FIFO and never scans unrelated board deliveries."
  (let* ((board (e-board-runtime-attachment-board attachment))
         (source-board (e-board-registry-board-source-board board)))
    (maphash
     (lambda (delivery-id _owned)
       (when-let ((pickup (e-board-pickup source-board delivery-id)))
         (pcase (e-board-pickup-state pickup)
           ((or 'pending 'ready)
            (e-board-cancel-pickup source-board delivery-id 'attachment-retired))
           ((or 'delivering 'accepted 'cancelling)
            (e-board-pickup-mark-uncertain
             source-board delivery-id 'attachment-retired))))
       (e-board-runtime--settle-producer-delivery-for-attachment
        attachment delivery-id 'cancelled
        (list :reason 'attachment-retired)))
   (e-board-runtime-attachment-owned-pickup-ids attachment))))

(defun e-board-runtime--drop-pending-pickups-for-attachment (attachment)
  "Remove queued runtime pickup callbacks belonging to ATTACHMENT.
The source-board pickup tombstones are authoritative; this process-local queue
must also forget their identities so a later replacement attachment cannot
interpret an old callback as a newly admitted delivery."
  (let ((board (e-board-runtime-attachment-board attachment)))
    (maphash
     (lambda (delivery-id _owned)
       (e-board-runtime--remove-pending-pickup-key
        (e-board-runtime--pickup-queue-key board delivery-id)))
     (e-board-runtime-attachment-owned-pickup-ids attachment))
    (clrhash (e-board-runtime-attachment-owned-pickup-ids attachment))))

(defun e-board-runtime--drop-attachment-activity (attachment)
  "Drop pending activity mailboxes and callbacks belonging to ATTACHMENT."
  (maphash
   (lambda (mailbox-id _owned)
     (remhash mailbox-id e-board-runtime--work-activity-mailboxes)
     (e-board-runtime--remove-pending-activity-key mailbox-id))
   (e-board-runtime-attachment-activity-mailbox-keys attachment))
  (clrhash (e-board-runtime-attachment-activity-mailbox-keys attachment))
  (unless e-board-runtime--pending-activity-head
    (setq e-board-runtime--pending-activity-tail nil))
  ;; A queued callback may still run after this exact attachment has been
  ;; retired.  Fence it by generation, then schedule a fresh page for other
  ;; live attachments if one remains.
  (cl-incf e-board-runtime--activity-drain-generation)
  (setq e-board-runtime--activity-drain-scheduled nil)
  (when e-board-runtime--pending-activity-head
    (e-board-runtime--schedule-activity-drain)))

(defun e-board-runtime--drop-attachment-invocations (attachment)
  "Retire exact invocation callbacks captured by ATTACHMENT."
  (let (entries)
    (maphash (lambda (target invocation)
               (push (cons target invocation) entries))
             (e-board-runtime-attachment-invocation-targets attachment))
    (dolist (entry entries)
      ;; The exact value check preserves a replacement target should a caller
      ;; have deliberately changed the attachment-local index while this
      ;; bounded retirement was being staged.
      (when (eq (gethash (car entry) e-board-runtime--invocations)
                (cdr entry))
        (e-board-runtime--drop-invocation
         (e-board-runtime-invocation-lease--create
          :target (car entry) :invocation (cdr entry)))))
    ;; Do not clear the whole local index: a reentrant terminal observer may
    ;; have installed an equal-key replacement in this attachment between the
    ;; captured entry and the inverse.  Remove only the exact old object and
    ;; leave replacement authority intact.
    (dolist (entry entries)
      (when (eq (gethash (car entry)
                         (e-board-runtime-attachment-invocation-targets attachment))
                (cdr entry))
        (remhash (car entry)
                 (e-board-runtime-attachment-invocation-targets attachment))))))

(defun e-board-runtime-retire-attachment (attachment)
  "Idempotently retire exact ATTACHMENT and its runtime-owned board state.
Chat/application services use this terminal operation after releasing their
presentation leases.  Runtime ownership alone fences and settles the local
pickup, producer, activity, and invocation state, then asks the board-registry
owner to retire ordinary routes while the exact runtime map triple remains
authoritative.  Only after that lower owner succeeds are the maps removed.
The staged operation therefore remains retryable after any lower-owner error;
replacement attachments or leases are never removed, and durable session/board
association is deliberately untouched so a later public ensure can rebuild an
attachment."
  (unless (e-board-runtime-attachment-p attachment)
    (signal 'wrong-type-argument
            (list 'e-board-runtime-attachment-p attachment)))
  ;; A reentrant retirement can occur while a board admission is still on the
  ;; stack.  Fence that exact record first; the outer admission postcheck will
  ;; perform its inverse.  Non-reentrant records are retried now, before any
  ;; attachment-owned route state is mutated.
  (e-board-runtime-admission-fence
   attachment
   (e-board-registry-board-source-board
    (e-board-runtime-attachment-board attachment)))
  (e-board-runtime-admission-retry
   attachment
   (e-board-registry-board-source-board
    (e-board-runtime-attachment-board attachment))
   t)
  (let* ((board (e-board-runtime-attachment-board attachment))
         (participant (e-board-runtime-attachment-participant attachment))
         (stage (e-board-runtime-attachment-retirement-stage attachment))
         (attachment-key
          (and participant
               (e-board-runtime--attachment-key board participant)))
         (session-key (e-board-runtime--attachment-session-key attachment))
         (endpoint-key
          (e-board-runtime--session-key
           (e-board-runtime-attachment-harness attachment)
           (e-board-runtime-attachment-session-id attachment)))
         ;; A rebind/move may leave this object holding the same participant
         ;; while a newer attachment owns the live maps.  Only the exact
         ;; triple captured at retirement start authorizes participant/FIFO
         ;; retirement, and that authorization is retained across retries.
         (owns-runtime-p
          (and attachment-key
               (eq (gethash attachment-key e-board-runtime--attachments)
                   attachment)
               (eq (gethash session-key e-board-runtime--session-attachments)
                   attachment)
               (eq (gethash endpoint-key e-board-runtime--endpoint-attachments)
                   attachment)))
         (reconciliation
          (e-board-runtime-attachment-reconciliation attachment)))
    (unless (eq stage 'done)
      (when (eq stage 'live)
        (setf (e-board-runtime-attachment-retirement-authorized-p attachment)
              owns-runtime-p
              ;; Local producer records are owned by this attachment object
              ;; even after a controlled rebind has published a replacement
              ;; into the runtime maps.  Keep their exact object/generation
              ;; authority independent from map ownership; board pickup/FIFO
              ;; mutation below remains restricted to the current map triple.
              (e-board-runtime-attachment-retirement-work-authorized-p attachment)
              t
              (e-board-runtime-attachment-retirement-generation attachment)
              (e-board-runtime-attachment-generation attachment)
              (e-board-runtime-attachment-retirement-stage attachment)
              'local
              ;; Invalidate every callback captured before this transition.
              (e-board-runtime-attachment-state attachment) 'retiring)
        (cl-incf (e-board-runtime-attachment-generation attachment)))
      (when reconciliation
        (setf (e-board-runtime-reconciliation-state reconciliation) 'cancelled
              (e-board-runtime-reconciliation-scheduled reconciliation) nil
              (e-board-runtime-attachment-reconciliation attachment) nil)
        (unless (e-request-terminal-p
                 (e-board-runtime-reconciliation-request reconciliation))
          (e-request-fail
           (e-board-runtime-reconciliation-request reconciliation)
           (list 'e-board-runtime-attachment-retired))))
      (e-board-runtime--unsubscribe-attachment-activity attachment)
      (when (e-board-runtime-attachment-retirement-work-authorized-p attachment)
        ;; Settlement is attachment-local and runs before route teardown, so
        ;; accepted/consumed producer work cannot strand its aggregate count.
        (e-board-runtime--settle-producer-turns-for-attachment attachment)
        (e-board-runtime--settle-producer-deliveries-for-attachment attachment))
      (when (e-board-runtime-attachment-retirement-authorized-p attachment)
        (e-board-runtime--retire-pickups-for-attachment attachment)
        (e-board-runtime--drop-pending-pickups-for-attachment attachment)
        ;; The exact attachment queue cells are gone.  Fence the global
        ;; scheduler receipt as well so a callback captured before retirement
        ;; cannot later drain a replacement's queue, and ensure unrelated live
        ;; queues receive a fresh callback under the new generation.
        (e-board-runtime--fence-pickup-drain))
      (e-board-runtime--drop-attachment-activity attachment)
      (e-board-runtime--drop-attachment-invocations attachment)
      (when (e-board-runtime-attachment-retirement-authorized-p attachment)
        ;; This call is deliberately made while the exact runtime maps still
        ;; exist.  A signal leaves `retirement-stage' at `local' and permits a
        ;; later retry to use the same authority rather than falling back to
        ;; an id lookup.
        (if (e-board-registry-participant-publication-pending participant)
            (e-board-registry-abort-participant-admission board participant)
          (e-board-registry-retire-participant-exact board participant)))
      (setf (e-board-runtime-attachment-retirement-stage attachment) 'maps)
      (dolist (table/key
               (list (cons e-board-runtime--attachments attachment-key)
                     (cons e-board-runtime--session-attachments session-key)
                     (cons e-board-runtime--endpoint-attachments endpoint-key)))
        (when (eq (gethash (cdr table/key) (car table/key)) attachment)
          (remhash (cdr table/key) (car table/key))))
      (setf (e-board-runtime-attachment-subscription attachment) nil
            (e-board-runtime-attachment-state attachment) 'dormant
            (e-board-runtime-attachment-reconciliation attachment) nil
            (e-board-runtime-attachment-retirement-stage attachment) 'done)
      (clrhash (e-board-runtime-attachment-turn-activity attachment))
      (clrhash (e-board-runtime-attachment-turn-tags attachment))
      (clrhash (e-board-runtime-attachment-turn-delivery-ids attachment)))
    attachment))

(defun e-board-runtime-abort-new-attachment (attachment)
  "Discard unpublished ATTACHMENT after an admission failure.
Remove only exact runtime ownership, activity sink, and registry membership;
this path is not a general participant removal operation and emits no durable
board event."
  (when (e-board-runtime-attachment-p attachment)
    ;; Reuse the exact runtime owner transition.  A fully activated
    ;; attachment retires its participant/routes before map removal; an
    ;; attachment that failed before the map triple existed is then removed by
    ;; the unpublished-admission inverse below, whose board owner applies the
    ;; same exact route fence.  No private route copy or id-only fallback is
    ;; needed here.
    (let ((board (e-board-runtime-attachment-board attachment))
          (participant (e-board-runtime-attachment-participant attachment)))
      (e-board-runtime-retire-attachment attachment)
      (when participant
        (e-board-registry-abort-participant-admission board participant))
      t)))

(defun e-board-runtime--register-invocation
    (attachment turn-id tool-call-id callback)
  "Capture CALLBACK behind one exact invocation target before work starts.
The target retains the original live ATTACHMENT and its generation.  Later
effects may use that capture or fail visibly; they never resolve a replacement
endpoint by session identity."
  (unless (and turn-id tool-call-id (functionp callback))
    (signal 'e-board-runtime-error
            (list "Invocation requires turn id, tool call id, and callback")))
  (let* ((target (e-board-runtime--invocation-target
                  attachment turn-id tool-call-id))
         (invocation
          (e-board-runtime-invocation--create
           :target target
           :attachment attachment
           :attachment-generation
           (e-board-runtime-attachment-generation attachment)
           :endpoint-token
           (copy-tree (e-board-runtime-attachment-endpoint-token attachment))
           :composite-generation
           (copy-tree (e-board-runtime--attachment-composite-generation attachment))
           :callback callback
           :state 'open
           :counted-p t))
         (lease
          (e-board-runtime-invocation-lease--create
           :target target :invocation invocation)))
    (when (gethash target e-board-runtime--invocations)
      (signal 'e-board-runtime-error (list "Invocation target already exists" target)))
    (puthash target invocation e-board-runtime--invocations)
    (puthash target invocation
             (e-board-runtime-attachment-invocation-targets attachment))
    (condition-case err
        (progn
          ;; The counter token is owned before notification so a reentrant
          ;; observer can retire this exact target.  The inverse below then
          ;; observes the token as already consumed and never decrements twice.
          (e-board-runtime--adjust-unsettled-count 'invocation 1)
          ;; `--adjust-unsettled-count' returns the value it computed before
          ;; notifying observers.  A non-signaling observer can nevertheless
          ;; reenter retirement, remove this target, and return normally.  Do
          ;; not let that stale target become board authority: admission is
          ;; committed only while the exact invocation, its attachment-local
          ;; index, its count token, and the current active attachment all
          ;; still agree.
          (unless (e-board-runtime--invocation-lease-current-p lease)
            (signal 'e-board-runtime-error
                    (list "Invocation admission lost active authority"
                          target)))
          lease)
      (error
       ;; `--adjust-unsettled-count' increments before calling either observer;
       ;; its failure is therefore an admission failure, not a committed
       ;; authority.  Remove only this object and preserve the first error even
       ;; when rollback notification faults or re-enters runtime teardown.
       (condition-case _rollback-error
           (e-board-runtime--terminalize-invocation
            target invocation 'cancelled)
         (error nil))
       (signal (car err) (cdr err))))))

(defun e-board-runtime--apply-invocation-effect (_board lease state payload)
  "Apply exact LEASE once through its captured runtime invocation service.
The board supplies the opaque lease returned by admission; this operation never
looks up an equal target to discover a newer invocation."
  (unless (e-board-runtime-invocation-lease-p lease)
    (signal 'e-board-runtime-error (list "Unknown invocation lease" lease)))
  (let* ((target (e-board-runtime-invocation-lease--target lease))
         (invocation (e-board-runtime-invocation-lease--invocation lease))
         (attachment (and (e-board-runtime-invocation-p invocation)
                          (e-board-runtime-invocation-attachment invocation))))
    (unless (and (e-board-runtime-invocation-p invocation)
                 (e-board-runtime-attachment-p attachment)
                 (eq (gethash target e-board-runtime--invocations) invocation)
                 (eq (gethash
                      target
                      (e-board-runtime-attachment-invocation-targets attachment))
                     invocation))
      (signal 'e-board-runtime-error
              (list "Unknown or replaced invocation lease" target)))
    (unless (eq (e-board-runtime-invocation-state invocation) 'open)
      (signal 'e-board-runtime-error (list "Invocation target is not open" target)))
    (unless (and
             (= (e-board-runtime-invocation-attachment-generation invocation)
                (e-board-runtime-attachment-generation attachment))
             (equal (e-board-runtime-invocation-endpoint-token invocation)
                    (e-board-runtime-attachment-endpoint-token attachment))
             (equal (e-board-runtime-invocation-composite-generation invocation)
                    (e-board-runtime--attachment-composite-generation attachment))
             (e-board-runtime--current-attachment-p attachment))
      (e-board-runtime--terminalize-invocation
       target invocation 'unavailable)
      (signal 'e-board-runtime-error
              (list "Original invocation endpoint is unavailable" target)))
    (setf (e-board-runtime-invocation-state invocation) 'applying)
    (condition-case err
        (funcall (e-board-runtime-invocation-callback invocation) state payload)
      (error
       (e-board-runtime--terminalize-invocation target invocation 'failed)
       (signal (car err) (cdr err))))
    ;; The callback may synchronously retire this exact attachment.  In that
    ;; case retirement already owns the terminal transition and removed the
    ;; table entry; the outer apply must not overwrite its state or count.
    (e-board-runtime--terminalize-invocation target invocation 'committed)))

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

(defun e-board-runtime--schedule-activity-drain ()
  "Schedule one generation-fenced activity mailbox drain."
  (unless e-board-runtime--activity-drain-scheduled
    (setq e-board-runtime--activity-drain-scheduled t)
    (let ((generation e-board-runtime--activity-drain-generation))
      (run-at-time
       0 nil
       (lambda ()
         (when (= generation e-board-runtime--activity-drain-generation)
           (e-board-runtime--drain-activity-mailboxes generation)))))))

(defun e-board-runtime--enqueue-activity-flush (mailbox-id &optional attachment)
  "Queue one future flush for MAILBOX-ID without retaining each raw update.
When ATTACHMENT is supplied, the key is indexed in that attachment's local
ownership table so retirement does not scan unrelated global mailboxes."
  (when attachment
    (puthash mailbox-id t
             (e-board-runtime-attachment-activity-mailbox-keys attachment)))
  (unless (gethash mailbox-id e-board-runtime--pending-activity-set)
    (puthash mailbox-id t e-board-runtime--pending-activity-set)
    (let ((cell (list mailbox-id)))
      (if e-board-runtime--pending-activity-tail
          (setcdr e-board-runtime--pending-activity-tail cell)
        (setq e-board-runtime--pending-activity-head cell))
      (setq e-board-runtime--pending-activity-tail cell))
    (e-board-runtime--unsettled-changed))
  (e-board-runtime--schedule-activity-drain))

(defun e-board-runtime--activity-content (activity-kind payload)
  "Return bounded board content for ACTIVITY-KIND and PAYLOAD.
Reasoning events carry presentation text in their `:content' field.  Other
work activity remains a diagnostic snapshot of its arbitrary payload."
  (if (memq activity-kind '(reasoning-delta reasoning-raw-delta))
      (let ((value (plist-get payload :content)))
        (unless (stringp value)
          (signal 'wrong-type-argument (list 'stringp value)))
        value)
    (truncate-string-to-width (e-prin1-safe payload) 512 nil nil "...")))

(defun e-board-runtime--publish-activity-mailbox (mailbox)
  "Publish one materialized work-activity MAILBOX when its owner is current."
  (let* ((attachment (plist-get mailbox :attachment))
         (activity-kind (or (plist-get mailbox :activity-kind)
                            'work-progress)))
    (when (and (e-board-runtime--current-active-attachment-p attachment)
               (= (or (plist-get mailbox :generation)
                      (e-board-runtime-attachment-generation attachment))
                  (e-board-runtime-attachment-generation attachment)))
      (let* ((payload (plist-get mailbox :payload))
             (board (e-board-registry-board-source-board
                     (e-board-runtime-attachment-board attachment)))
             (participant-id
              (e-board-registry-participant-id
               (e-board-runtime-attachment-participant attachment))))
        (e-board-post-activity
         board
         :author (format "participant:%s" participant-id)
         :subject-participant-id participant-id
         :source-turn-id (plist-get mailbox :turn-id)
         :activity-kind activity-kind
         :tags (copy-tree (plist-get mailbox :tags))
         :attributes
         (append (when-let ((work-id (plist-get mailbox :work-id)))
                   (list :work-id work-id))
                 (when-let ((subagent-id
                             (plist-get payload :subagent-id)))
                   (list :subagent-id subagent-id))
                 (when-let ((request-id (plist-get mailbox :provider-request-id)))
                   (list :provider-request-id request-id)))
         :content (e-board-runtime--activity-content activity-kind payload)
         :caused-by-delivery-ids
         (copy-tree
          (gethash (plist-get mailbox :turn-id)
                   (e-board-runtime-attachment-turn-delivery-ids attachment)))
         :source-activity-key (plist-get mailbox :source-key))))))

(defun e-board-runtime--drain-activity-mailboxes (&optional generation)
  "Publish one bounded page of latest work activity mailbox snapshots."
  (when (or (null generation)
            (= generation e-board-runtime--activity-drain-generation))
    (setq e-board-runtime--activity-drain-scheduled nil)
    (let ((processed 0))
      (while (and e-board-runtime--pending-activity-head
                  (< processed e-board-runtime-activity-drain-limit))
        (let* ((mailbox-id (pop e-board-runtime--pending-activity-head))
               (mailbox (gethash mailbox-id
                                  e-board-runtime--work-activity-mailboxes)))
          (unless e-board-runtime--pending-activity-head
            (setq e-board-runtime--pending-activity-tail nil))
          (let ((counted (gethash mailbox-id
                                  e-board-runtime--pending-activity-set)))
            (remhash mailbox-id e-board-runtime--pending-activity-set)
            (when counted
              (e-board-runtime--unsettled-changed)))
          (remhash mailbox-id e-board-runtime--work-activity-mailboxes)
          (when mailbox
            (let ((attachment (plist-get mailbox :attachment)))
              (remhash mailbox-id
                       (e-board-runtime-attachment-activity-mailbox-keys
                        attachment))
              (e-board-runtime--publish-activity-mailbox mailbox)))
          (cl-incf processed)))
      (when e-board-runtime--pending-activity-head
        (e-board-runtime--schedule-activity-drain)))))

(defun e-board-runtime--capture-work-activity (attachment handle payload)
  "Replace HANDLE's bounded progress mailbox with PAYLOAD.
This is the sole synchronous activity observer installed by board enrollment.
It neither formats nor publishes PAYLOAD; a later runtime activity publisher
will consume the mailbox under its own bounded drain."
  ;; Check exact current authority before mutating either mailbox or queue.
  ;; Work listeners outlive a canceled handle by design; a stale listener must
  ;; be a no-op even if a replacement reuses the same Work id.
  (when (e-board-runtime--current-active-attachment-p attachment)
    (let* ((work-id (e-work-handle-id handle))
         (participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (generation (e-board-runtime-attachment-generation attachment))
         (mailbox-id
          (list (e-board-runtime-attachment-identity-token attachment)
                generation 'work work-id))
         (sequence (cl-incf (e-board-runtime-attachment-activity-sequence attachment))))
      (puthash mailbox-id
               (list :attachment attachment :generation generation :work-id work-id
                     :turn-id (plist-get (e-work-handle-context handle) :turn-id)
                     :activity-kind 'work-progress
                     :payload payload
                     :source-key (list participant-id generation sequence))
               e-board-runtime--work-activity-mailboxes)
      (e-board-runtime--enqueue-activity-flush mailbox-id attachment))))

(defun e-board-runtime--capture-turn-progress (attachment event)
  "Publish only a harness-combined reasoning EVENT to the Board."
  (let* ((turn-id (plist-get event :turn-id))
         (activity-kind (e-events-type event))
         (participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant attachment)))
         (payload (plist-get event :payload)))
    (when (plist-get payload :combined)
      (e-board-post-activity
       (e-board-registry-board-source-board
        (e-board-runtime-attachment-board attachment))
       :author (format "participant:%s" participant-id)
       :subject-participant-id participant-id
       :source-turn-id turn-id
       :activity-kind activity-kind
       :tags (or (copy-tree
                  (gethash turn-id
                           (e-board-runtime-attachment-turn-tags attachment)))
                 '(main))
       :attributes
       (list :content-mode 'snapshot
             :combined t
             :provider-request-id (plist-get payload :provider-request-id))
       :content (e-board-runtime--activity-content activity-kind payload)
       :caused-by-delivery-ids
       (copy-tree
        (gethash turn-id
                 (e-board-runtime-attachment-turn-delivery-ids attachment)))
       :source-activity-key
       (e-board-runtime--event-activity-source-key
        attachment event activity-kind)))))

(defun e-board-runtime--install-work-hooks (attachment handle)
  "Install the private board-runtime hook classification on prepared HANDLE."
  (let ((policies '(:cancel deferred :cleanup deferred :settle deferred))
        ;; A fresh closure gives this admission a stable exact identity even
        ;; when a prior owner used the same scheduler function symbol.
        (dispatcher
         (lambda (current-handle receipt thunk)
           (e-board-runtime--schedule-deferred-hook
            current-handle receipt thunk)))
        activity-observer)
    (dolist (key '(:on-done :on-error :on-progress :on-event))
      (when (plist-get (e-work-handle-callbacks handle) key)
        (setq policies (plist-put policies key 'deferred))))
    (when (e-work-spec-result-shaper (e-work-handle-spec handle))
      ;; Result shaping is still part of a cheap runner in this foundation.
      ;; The board runtime therefore admits it only under the narrow inline
      ;; classification; a later async shaper will use a separate work unit.
      (setq policies (plist-put policies :result-shaper 'hard-bounded)))
    (condition-case err
        (progn
          (e-work-install-hook-dispatcher
           handle dispatcher policies)
          (setq activity-observer
                (lambda (current-handle payload)
                  (e-board-runtime--capture-work-activity
                   attachment current-handle payload)))
          (e-work-install-activity-observer handle activity-observer)
          (e-board-runtime-work-hooks--create
           :dispatcher dispatcher
           :activity-observer activity-observer))
      (error
       ;; Either installation can signal after mutating its handle.  Each
       ;; inverse checks exact closure identity, so a pre-existing or reentrant
       ;; replacement remains untouched; independent cleanup keeps one fault
       ;; from masking the initiating error.
       (condition-case _rollback-error
           (e-work-remove-activity-observer handle activity-observer)
         (error nil))
       (condition-case _rollback-error
           (e-work-remove-hook-dispatcher handle dispatcher)
         (error nil))
       (signal (car err) (cdr err))))))

(defun e-board-runtime--uninstall-work-hooks (handle hooks)
  "Remove exactly the Work hooks recorded in HOOKS, preserving replacements."
  (when (e-board-runtime-work-hooks-p hooks)
    (condition-case _rollback-error
        (e-work-remove-activity-observer
         handle (e-board-runtime-work-hooks-activity-observer hooks))
      (error nil))
    (condition-case _rollback-error
        (e-work-remove-hook-dispatcher
         handle (e-board-runtime-work-hooks-dispatcher hooks))
      (error nil))))

(defun e-board-runtime--pickup-queue-key (board pickup-id)
  "Return the process-local queue identity for BOARD's PICKUP-ID."
  (list (e-board-registry-board-id board) pickup-id))

(defun e-board-runtime--schedule-pickup-drain ()
  "Schedule one generation-fenced bounded pickup drain.
The callback captures the generation that admitted it.  Retirement advances the
generation and clears the scheduled bit before scheduling any surviving queue,
so an old callback is a no-op and cannot consume replacement work."
  (unless e-board-runtime--pickup-drain-scheduled
    (setq e-board-runtime--pickup-drain-scheduled t)
    (let ((generation e-board-runtime--pickup-drain-generation))
      (run-at-time
       0 nil
       (lambda ()
         (when (= generation e-board-runtime--pickup-drain-generation)
           (e-board-runtime--drain-pickups generation)))))))

(defun e-board-runtime--fence-pickup-drain ()
  "Invalidate queued pickup callbacks and reschedule the current queue.
This is a global scheduler fence, not an owner lookup: the pending queue remains
the authoritative FIFO while the generation makes every previously captured
callback inert."
  (cl-incf e-board-runtime--pickup-drain-generation)
  (setq e-board-runtime--pickup-drain-scheduled nil)
  (when e-board-runtime--pending-pickup-head
    (e-board-runtime--schedule-pickup-drain)))

(defun e-board-runtime--enqueue-pickups (board pickup-ids)
  "Enqueue BOARD PICKUP-IDS once; never deliver on an append/effect stack."
  (dolist (pickup-id pickup-ids)
    (when-let ((pickup
                (e-board-pickup
                 (e-board-registry-board-source-board board) pickup-id))
               (attachment
                (and (e-board-pickup-route-current-p
                      (e-board-registry-board-source-board board) pickup)
                     (e-board-runtime--attachment-for-pickup board pickup))))
      (e-board-runtime--index-attachment-pickup attachment pickup-id))
    (let ((key (e-board-runtime--pickup-queue-key board pickup-id)))
      (unless (gethash key e-board-runtime--pending-pickup-set)
        (puthash key t e-board-runtime--pending-pickup-set)
        (let ((cell (list key)))
          (if e-board-runtime--pending-pickup-tail
              (setcdr e-board-runtime--pending-pickup-tail cell)
            (setq e-board-runtime--pending-pickup-head cell))
          (setq e-board-runtime--pending-pickup-tail cell))
        (e-board-runtime--unsettled-changed))))
  ;; A missed-wake scan may legitimately produce no unresolved pickups.  Do
  ;; not publish scheduler authority without a queue head: if that empty timer
  ;; is later fenced during process-local teardown, a stale scheduled bit can
  ;; suppress the next real delivery.
  (when e-board-runtime--pending-pickup-head
    (e-board-runtime--schedule-pickup-drain)))

(defun e-board-runtime--drain-pickups (&optional generation)
  "Attempt one bounded FIFO page of previously frozen pickup envelopes.
When GENERATION is supplied it must still be current; a stale scheduled
callback returns without changing the current scheduler authority.  Calls
without it are direct owner drains and use the current generation."
  (when (or (null generation)
            (= generation e-board-runtime--pickup-drain-generation))
    (let ((run-generation e-board-runtime--pickup-drain-generation))
      (setq e-board-runtime--pickup-drain-scheduled nil)
      (let ((processed 0)
            (available-at-start 0)
            (cursor e-board-runtime--pending-pickup-head))
        (while (and cursor
                    (< available-at-start e-board-runtime-pickup-drain-limit))
          (cl-incf available-at-start)
          (setq cursor (cdr cursor)))
        (while (and (= run-generation e-board-runtime--pickup-drain-generation)
                    e-board-runtime--pending-pickup-head
                    (< processed available-at-start))
          (let* ((key (car e-board-runtime--pending-pickup-head))
                 (board (condition-case nil
                            (e-board-registry-get (car key))
                          (e-board-registry-missing nil)))
                 (source-board
                  (and board (e-board-registry-board-source-board board))))
            (if (and source-board (e-board-mutation-frozen-p source-board))
                (progn
                  ;; Keep the exact FIFO head and unsettled count intact.  A
                  ;; timer may have fired inside another pickup/observer ACK;
                  ;; retry only after that Board releases its owner barrier.
                  (setq e-board-runtime--pickup-drain-scheduled t
                        processed available-at-start)
                  (e-board--defer-after-storage-barrier
                   source-board
                   (lambda ()
                     (when (= run-generation
                              e-board-runtime--pickup-drain-generation)
                       (setq e-board-runtime--pickup-drain-scheduled nil)
                       (e-board-runtime--drain-pickups run-generation)))))
              (pop e-board-runtime--pending-pickup-head)
              (unless e-board-runtime--pending-pickup-head
                (setq e-board-runtime--pending-pickup-tail nil))
              (let ((counted (gethash key e-board-runtime--pending-pickup-set)))
                (remhash key e-board-runtime--pending-pickup-set)
                (when counted
                  (e-board-runtime--unsettled-changed)))
              (cl-incf processed)
              (when board
                (e-board-runtime--deliver-pickups board (list (cadr key)))))))
        (when (and (= run-generation e-board-runtime--pickup-drain-generation)
                   e-board-runtime--pending-pickup-head)
          (e-board-runtime--schedule-pickup-drain))))))

(defun e-board-runtime--drain-input-routing (board drain)
  "Run BOARD's bounded classifier, then queue only its finalized pickups."
  (let ((source-board (e-board-registry-board-source-board board)))
    (if (e-board-mutation-frozen-p source-board)
        ;; Worker waits service timers.  Keep owner-scheduled classification
        ;; behind the current commit instead of treating it as a conflicting
        ;; caller mutation and losing the only scheduled drain.
        (e-board--defer-after-storage-barrier
         source-board
         (lambda () (e-board-runtime--drain-input-routing board drain)))
      (funcall drain)
      (dolist (result (e-board-drain-routed-pickups source-board))
        (e-board-runtime--producer-routing-finished
         board (car result) (cadr result))
        (e-board-runtime--enqueue-pickups board (cadr result))))))

(defun e-board-runtime--enroll-work (harness handle callback)
  "Enroll HANDLE for its attached HARNESS session before runner entry.
CALLBACK is the private loop result seam for executable tool work; turn work
has no callback and is observed only."
  (when-let* ((session-id (plist-get (e-work-handle-context handle) :session-id))
              (attachment (gethash (e-board-runtime--session-key harness session-id)
                                   e-board-runtime--endpoint-attachments)))
    (let* ((context (e-work-handle-context handle))
           (turn-id (plist-get context :turn-id))
           (board (e-board-registry-board-source-board
                   (e-board-runtime-attachment-board attachment)))
           (metadata (e-work-handle-metadata handle)))
      ;; A prior admission may have preserved its exact board token after two
      ;; lower-owner inverse faults.  Finish that transaction before allowing a
      ;; new Work relation to be staged on the same board.
      (e-board-runtime-admission-retry nil board)
      ;; Every participant activity is correlated by source turn.  Reject an
      ;; invalid handle before installing observers or enrolling board work so
      ;; the defect surfaces on the initiating call instead of a later timer.
      (unless (stringp turn-id)
        (signal 'e-board-runtime-invalid-work
                (list :work-id (e-work-handle-id handle)
                      :session-id session-id
                      :turn-id turn-id)))
      (let* ((tool-call-id (and callback
                                (plist-get (plist-get context :tool-call) :id)))
             (invocation-id (and callback (list turn-id tool-call-id)))
             hooks lease admission result)
        (condition-case err
            (progn
              ;; Work hooks, the runtime target/count, and the board relation
              ;; are one admission transaction.  No runner can start until all
              ;; three owners have returned successfully.
              (setq hooks (e-board-runtime--install-work-hooks attachment handle))
              (if callback
                  (progn
                    (setq lease
                          (e-board-runtime--register-invocation
                           attachment turn-id tool-call-id callback))
                    (setq admission
                          (e-board-work-admission-token
                           handle :invocation-id invocation-id
                           :effect-target lease))
                    ;; Install runtime recovery authority before entering the
                    ;; reentrant board stage.  The captured attachment may be
                    ;; retired before this call returns.
                    (e-board-runtime-admission-remember
                     attachment board admission t
                     (e-board-runtime-attachment-generation attachment))
                    (setq result
                          (e-board-enroll-invocation-work
                           board handle invocation-id lease
                           :metadata metadata :admission admission)))
                (setq admission (e-board-work-admission-token handle))
                (e-board-runtime-admission-remember
                 attachment board admission t
                 (e-board-runtime-attachment-generation attachment))
                (setq result
                      (e-board-enroll-work
                       board handle :metadata metadata :admission admission)))
              (when (and callback
                         (not (e-board-runtime--invocation-lease-current-p lease)))
                ;; A board publication callback may have retired this exact
                ;; admission and installed an equal-key replacement before
                ;; returning.  Do not report success or bind Work to the new
                ;; authority.
                (signal 'e-board-runtime-error
                        (list "Invocation lease lost after board admission")))
              (when (and admission
                         (not (e-board-work-admission-current-p admission)))
                ;; The no-callback turn path has no invocation lease to use as
                ;; its postcondition.  The board token is the exact authority:
                ;; a non-signaling reentrant retirement must not let this stack
                ;; return a stale Work relation after it has been removed.
                (signal 'e-board-runtime-error
                        (list "Work admission lost active authority" admission)))
              (when admission
                (e-board-runtime-admission-finish admission)
                (e-board-runtime-admission-complete admission))
              result)
          (error
           ;; Roll back in owner order.  Each inverse is exact and idempotent;
           ;; a rollback notification may signal or re-enter, but it cannot
           ;; remove a replacement target/observer and it cannot replace the
           ;; original admission error.
           (when admission
             (e-board-runtime-admission-finish admission)
             (unless (e-board-runtime-admission-abort board admission)
               ;; Keep the exact token reachable if both bounded inverse
               ;; attempts fail.  A subsequent enrollment or attachment
               ;; retirement retries it before mutating the same owner again.
               (condition-case _catalog-error
                   (e-board-runtime-admission-remember
                    attachment board admission nil
                    (e-board-runtime-attachment-generation attachment))
                 (error nil)))
             (when (and (e-board-admission-complete-p admission))
               ;; Catalog cleanup is part of the rollback, but an unexpected
               ;; cleanup fault must not replace the initiating error.  The
               ;; exact recovery record remains reachable for a later retry.
               (condition-case _catalog-error
                   (e-board-runtime-admission-complete admission)
                 (error nil))))
           (when lease
             (condition-case _rollback-error
                 (e-board-runtime--drop-invocation lease)
               (error nil)))
           (when hooks
             (condition-case _rollback-error
                 (e-board-runtime--uninstall-work-hooks handle hooks)
               (error nil)))
           (signal (car err) (cdr err))))))))

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
           ;; Resolve unfinished exact inverses before registering another
           ;; aggregation lease on the same source board.
           (_pending (e-board-runtime-admission-retry
                      nil board))
           (lease (e-board-runtime--register-invocation
                    attachment turn-id (plist-get call :id)
                    (lambda (_state reason) (funcall callback reason))))
           (admission (e-board-aggregation-admission-token board))
           aggregation)
      (condition-case err
          (progn
            ;; Catalog publication is part of the initiating transaction.  It
            ;; must be inside the handler so a bucket/primary fault still
            ;; reaches the exact Board inverse and invocation cleanup.
            (e-board-runtime-admission-remember
             attachment board admission t
             (e-board-runtime-attachment-generation attachment))
            (setq aggregation
                  (e-board-subscribe-aggregation
                   board (mapcar #'e-work-handle-id handles) mode lease
                   :id invocation-id :timeout timeout
                   :admission admission)))
        (error
         (e-board-runtime-admission-finish admission)
         (unless (e-board-runtime-admission-abort board admission)
           ;; Keep this exact aggregation token independently of the
           ;; attachment when lower-owner cleanup remains pending.
           (condition-case _catalog-error
               (e-board-runtime-admission-remember
                attachment board admission nil
                (e-board-runtime-attachment-generation attachment))
             (error nil)))
         (when (e-board-admission-complete-p admission)
           (condition-case _catalog-error
               (e-board-runtime-admission-complete admission)
             (error nil)))
         (condition-case _drop-error
             (e-board-runtime--drop-invocation lease)
           (error nil))
         (signal (car err) (cdr err))))
      ;; Board composition is an exact second lease consumer.  A reentrant
      ;; retirement can invalidate either lease while the board call returns;
      ;; never hand a cancellation closure a stale aggregation authority.
        (unless (and (e-board-runtime--invocation-lease-current-p lease)
                   (e-board-aggregation-admission-current-p admission))
        (e-board-runtime-admission-finish admission)
        (unless (e-board-runtime-admission-abort board admission)
          (condition-case _catalog-error
              (e-board-runtime-admission-remember
               attachment board admission nil
               (e-board-runtime-attachment-generation attachment))
            (error nil)))
        (when (e-board-admission-complete-p admission)
          (condition-case _catalog-error
              (e-board-runtime-admission-complete admission)
            (error nil)))
        (condition-case _drop-error
            (e-board-runtime--drop-invocation lease)
          (error nil))
        (signal 'e-board-runtime-error
                (list "Aggregation admission lost active authority" invocation-id)))
      (e-board-runtime-admission-finish admission)
      (e-board-runtime-admission-complete admission)
      (lambda ()
        (e-board-cancel-aggregation board (e-board-aggregation-id aggregation))
        (e-board-runtime--drop-invocation lease)))))

(defun e-board-runtime--publish-output (attachment turn-id)
  "Publish ATTACHMENT's final assistant message for TURN-ID exactly once."
  (let ((message
         (e-harness-attached-turn-port-assistant-message
          (e-board-runtime-attachment-turn-port attachment) turn-id)))
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
         (delivery-id (car (e-board-pickup-ids board participant-id)))
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
              (let ((durable-key
                     (+ (* 2 durable-sequence)
                        (if (eq publication-kind 'turn-summary) 1 0))))
                ;; Durable and locally allocated activity share one producer
                ;; watermark.  Keep the fallback allocator above every
                ;; durable key so later work progress cannot look like expired
                ;; history merely because it used the local path.
                (setf (e-board-runtime-attachment-activity-sequence attachment)
                      (max durable-key
                           (e-board-runtime-attachment-activity-sequence
                            attachment)))
                durable-key)
            (cl-incf
             (e-board-runtime-attachment-activity-sequence attachment)))))
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
        (copy-tree (plist-get event :payload))
        (when-let ((source-event-id
                    (plist-get event :activity-entry-id)))
          (list :source-event-id source-event-id)))
       :caused-by-delivery-ids
       (copy-tree
        (gethash turn-id
                 (e-board-runtime-attachment-turn-delivery-ids attachment)))
       :source-activity-key
       (e-board-runtime--event-activity-source-key attachment event activity-kind)))))

(defun e-board-runtime--visible-harness-activity-attributes (kind payload)
  "Return bounded board attributes for visible harness activity KIND.
Capability hook details remain in the private durable audit.  The board carries
only the generic status fields presentation consumers need."
  (if (eq kind 'hook-audit)
      (cl-loop for key in '(:owner :hook-id :outcome :truth-status
                            :summary :pending-summary)
               when (plist-member payload key)
               append (list key (copy-tree (plist-get payload key))))
    (copy-tree payload)))

(defconst e-board-runtime--curation-activity-count-keys
  '(:kept-source-count :summary-count :summarized-source-count
    :erased-source-count)
  "Required Board-visible count fields for one committed curation package.")

(defconst e-board-runtime--curation-source-kinds
  '("current-state" "dynamic-context" "tool-result" "trace"
    "retrieved-excerpt")
  "Board-visible semantic source kinds for content-free curation stubs.")

(defun e-board-runtime--curation-source-stub (stub)
  "Return validated content-free Board curation source STUB."
  (unless (and (proper-list-p stub) (zerop (% (length stub) 2)))
    (signal 'e-board-runtime-invalid-activity
            (list 'context-curated :source-stub-shape stub)))
  (let ((tail stub)
        keys)
    (while tail
      (push (pop tail) keys)
      (pop tail))
    (unless (and (= (length keys) (length (delete-dups (copy-sequence keys))))
                 (cl-every (lambda (key)
                             (memq key '(:disposition :source-kind :tool-name)))
                           keys)
                 (plist-member stub :disposition)
                 (plist-member stub :source-kind))
      (signal 'e-board-runtime-invalid-activity
              (list 'context-curated :source-stub-keys (nreverse keys)))))
  (let ((disposition (plist-get stub :disposition))
        (source-kind (plist-get stub :source-kind))
        (tool-name (and (plist-member stub :tool-name)
                        (plist-get stub :tool-name))))
    (unless (and (memq disposition '(kept summarized erased))
                 (member source-kind e-board-runtime--curation-source-kinds)
                 (or (not (plist-member stub :tool-name))
                     (and (equal source-kind "tool-result")
                          (stringp tool-name)
                          (not (string-empty-p tool-name))
                          (not (string-match-p "[[:cntrl:]]" tool-name))
                          (<= (length tool-name) 256))))
      (signal 'e-board-runtime-invalid-activity
              (list 'context-curated :source-stub stub)))
    (append (list :disposition disposition
                  :source-kind (copy-sequence source-kind))
            (when tool-name (list :tool-name (copy-sequence tool-name))))))

(defun e-board-runtime--curation-activity-attributes (projection)
  "Return validated content-free Board attributes from PROJECTION."
  (unless (and (proper-list-p projection)
               (zerop (% (length projection) 2)))
    (signal 'e-board-runtime-invalid-activity
            (list 'context-curated :shape projection)))
  (let ((tail projection)
        keys)
    (while tail
      (let ((key (pop tail)))
        (unless (and (keywordp key) tail)
          (signal 'e-board-runtime-invalid-activity
                  (list 'context-curated :shape projection)))
        (push key keys)
        (pop tail)))
    (unless (and (= (length keys) (length (delete-dups (copy-sequence keys))))
                 (cl-every (lambda (key)
                             (memq key
                                   (append
                                    e-board-runtime--curation-activity-count-keys
                                    '(:source-stubs))))
                           keys)
                 (cl-every (lambda (key) (plist-member projection key))
                           e-board-runtime--curation-activity-count-keys))
      (signal 'e-board-runtime-invalid-activity
              (list 'context-curated :keys (nreverse keys))))
    (let ((kept (plist-get projection :kept-source-count))
          (summaries (plist-get projection :summary-count))
          (summarized (plist-get projection :summarized-source-count))
          (erased (plist-get projection :erased-source-count))
          (stubs-present-p (plist-member projection :source-stubs))
          (stubs (and (plist-member projection :source-stubs)
                      (plist-get projection :source-stubs))))
      (unless (and (cl-every (lambda (value)
                               (and (integerp value) (>= value 0)))
                             (list kept summaries summarized erased))
                   (or (> kept 0) (> summarized 0) (> erased 0))
                   (eq (= summaries 0) (= summarized 0))
                   (<= summaries summarized))
        (signal 'e-board-runtime-invalid-activity
                (list 'context-curated :counts projection)))
      (when stubs-present-p
        (unless (and (proper-list-p stubs) stubs
                     (<= (length stubs) 16))
          (signal 'e-board-runtime-invalid-activity
                  (list 'context-curated :source-stubs stubs)))
        (setq stubs (mapcar #'e-board-runtime--curation-source-stub stubs))
        (unless (and (= kept
                        (seq-count (lambda (stub)
                                     (eq (plist-get stub :disposition) 'kept))
                                   stubs))
                     (= summarized
                        (seq-count
                         (lambda (stub)
                           (eq (plist-get stub :disposition) 'summarized))
                         stubs))
                     (= erased
                        (seq-count (lambda (stub)
                                     (eq (plist-get stub :disposition) 'erased))
                                   stubs)))
          (signal 'e-board-runtime-invalid-activity
                  (list 'context-curated :source-stub-counts projection))))
      (append (list :kept-source-count kept
                    :summary-count summaries
                    :summarized-source-count summarized
                    :erased-source-count erased)
              (when stubs-present-p
                (list :source-stubs stubs))))))

(defun e-board-runtime--publish-harness-activity (attachment event)
  "Publish EVENT's bounded lifecycle edge without exposing its raw payload."
  (let* ((source-kind (e-events-type event))
         (payload (plist-get event :payload))
         (curation (and (eq source-kind 'context-frame-consumed)
                        (plist-get payload :curation)))
         (activity-kind (if curation 'context-curated source-kind))
         (turn-id (plist-get event :turn-id)))
    (when (and turn-id
               (or curation
                   (memq source-kind
                         e-board-runtime--visible-harness-activity-types)))
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
         (if curation
             (e-board-runtime--curation-activity-attributes curation)
           (append
            (e-board-runtime--visible-harness-activity-attributes
             activity-kind payload)
            (when-let ((source-event-id
                        (plist-get event :activity-entry-id)))
              (list :source-event-id source-event-id))))
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

(defun e-board-runtime--record-turn-delivery
    (attachment turn-id delivery-id)
  "Record one unique DELIVERY-ID under ATTACHMENT's TURN-ID projection."
  (let ((ids (gethash turn-id
                      (e-board-runtime-attachment-turn-delivery-ids attachment))))
    (unless (member delivery-id ids)
      (puthash turn-id (append ids (list delivery-id))
               (e-board-runtime-attachment-turn-delivery-ids attachment)))))

(defun e-board-runtime--producer-turn-key (attachment turn-id)
  "Return exact attachment-generation producer key for TURN-ID."
  (list (e-board-registry-board-id
         (e-board-runtime-attachment-board attachment))
        (e-board-registry-participant-id
         (e-board-runtime-attachment-participant attachment))
        (e-board-runtime--attachment-work-generation attachment)
        turn-id))

(defun e-board-runtime--settle-producer-turn (attachment event status)
  "Settle any producer delivery causally owned by terminal EVENT."
  (when-let* ((turn-id (plist-get event :turn-id))
              (key (e-board-runtime--producer-turn-key attachment turn-id))
              (entry (gethash key e-board-runtime--producer-turns)))
    (when (and (e-board-runtime-producer-turn-p entry)
               (eq (e-board-runtime-producer-turn-attachment entry) attachment)
               (= (e-board-runtime-producer-turn-generation entry)
                  (e-board-runtime--attachment-work-generation attachment)))
      (remhash key e-board-runtime--producer-turns)
      (remhash key (e-board-runtime-attachment-producer-turn-keys attachment))
      (e-board-runtime--producer-delivery-terminal
       (e-board-runtime-producer-turn-item entry)
       (e-board-runtime-producer-turn-delivery-id entry) status
       (list :turn-id turn-id :participant-id
             (e-board-registry-participant-id
              (e-board-runtime-attachment-participant attachment)))))))

(defun e-board-runtime--handle-harness-event (attachment event)
  "Publish attached output and reconcile board-delivery receipts from EVENT.
Terminal receipts remain admissible while an attachment is detaching: they are
the lower-owner proof that lets a bounded removal/rebind finish.  High-volume
progress is still restricted to the active lease, so a late callback cannot
recreate an activity mailbox after retirement has begun."
  (when (e-board-runtime--current-attachment-p attachment)
    (let* ((type (e-events-type event))
           (state (e-board-runtime-attachment-state attachment))
           ;; A detaching participant still needs terminal receipts to finish
           ;; its explicit removal transaction.  A retiring attachment is
           ;; different: its application owner has already committed terminal
           ;; teardown, so a retained terminal event may settle a causally
           ;; owned producer slot but must not publish a second output/activity
           ;; row or touch the replacement lifetime.
           (publish-p (not (eq state 'retiring)))
           (receipt-p (memq state '(active detaching))))
      (when (and (memq type '(reasoning-delta reasoning-raw-delta))
                 (e-board-runtime--current-active-attachment-p attachment))
        (e-board-runtime--capture-turn-progress attachment event))
      (when publish-p
        (e-board-runtime--observe-turn-activity attachment event)
        (e-board-runtime--publish-harness-activity attachment event))
      (cond
       ((eq type 'turn-finished)
        (let* ((turn-id (plist-get event :turn-id))
               (delivery-ids
                (copy-tree
                 (gethash turn-id
                          (e-board-runtime-attachment-turn-delivery-ids
                           attachment)))))
          (when publish-p
            (e-board-runtime--publish-output attachment turn-id)
            (e-board-runtime--publish-turn-summary attachment event 'finished))
          (remhash turn-id (e-board-runtime-attachment-turn-tags attachment))
          (remhash turn-id
                   (e-board-runtime-attachment-turn-delivery-ids attachment))
          (dolist (delivery-id delivery-ids)
            (e-board-runtime--forget-terminal-pickup attachment delivery-id))
          (e-board-runtime--settle-producer-turn attachment event 'done)
          (when receipt-p
            (e-board-runtime--enqueue-ready-participant-pickup attachment))))
       ((memq type '(turn-failed turn-cancelled))
        (let* ((turn-id (plist-get event :turn-id))
               (delivery-ids
                (copy-tree
                 (gethash turn-id
                          (e-board-runtime-attachment-turn-delivery-ids
                           attachment)))))
          (when publish-p
            (e-board-runtime--publish-terminal-activity attachment event)
            (e-board-runtime--publish-turn-summary attachment event type))
          (remhash turn-id (e-board-runtime-attachment-turn-tags attachment))
          (remhash turn-id
                   (e-board-runtime-attachment-turn-delivery-ids attachment))
          (dolist (delivery-id delivery-ids)
            (e-board-runtime--forget-terminal-pickup attachment delivery-id))
          (e-board-runtime--settle-producer-turn
           attachment event (if (eq type 'turn-cancelled) 'cancelled 'failed))
          (when receipt-p
            (e-board-runtime--enqueue-ready-participant-pickup attachment))))
       ((eq type 'input-consumed)
        (when receipt-p
          (let* ((payload (plist-get event :payload))
               (delivery-id (plist-get payload :delivery-id))
               (registry-board (e-board-runtime-attachment-board attachment))
               (board (e-board-registry-board-source-board registry-board))
               (pickup (e-board-pickup board delivery-id)))
          (when (and pickup (plist-get event :turn-id)
                     (e-board-runtime--attempt-belongs-to-attachment-p
                      pickup attachment))
            (e-board-runtime--record-turn-delivery
             attachment (plist-get event :turn-id) delivery-id)
            (puthash (plist-get event :turn-id)
                     (copy-tree
                      (plist-get (e-board-pickup-cause-metadata pickup)
                                 :routing-tags))
                     (e-board-runtime-attachment-turn-tags attachment)))
          (when-let* ((turn-id (plist-get event :turn-id))
                      (record (e-board-runtime--producer-delivery-record
                               delivery-id))
                      (item (and (e-board-runtime-producer-delivery-p record)
                                 (e-board-runtime-producer-delivery-item record))))
            (when (and item pickup
                       (eq (e-board-runtime-producer-delivery-attachment record)
                           attachment)
                       (= (e-board-runtime-producer-delivery-generation record)
                          (e-board-runtime--attachment-work-generation attachment))
                       (e-board-runtime--attempt-belongs-to-attachment-p
                        pickup attachment))
              (let ((key (e-board-runtime--producer-turn-key attachment turn-id)))
                (puthash key
                         (e-board-runtime--producer-turn-create
                          :id key :item item :delivery-id delivery-id
                          :attachment attachment
                          :generation
                          (e-board-runtime--attachment-work-generation attachment))
                         e-board-runtime--producer-turns)
                (puthash key t
                         (e-board-runtime-attachment-producer-turn-keys
                          attachment)))))
          (when (and pickup
                     (memq (e-board-pickup-state pickup) '(accepted cancelling))
                     (e-board-runtime--consumption-receipt-matches-p
                      pickup attachment payload))
            (when-let ((next-id
                        (e-board-pickup-complete-delivery board delivery-id)))
              (e-board-runtime--enqueue-pickups registry-board (list next-id)))
            (e-board-runtime--forget-terminal-pickup attachment delivery-id)))))
       ((eq type 'input-discarded)
        (when receipt-p
          (let* ((payload (plist-get event :payload))
               (delivery-id (plist-get payload :delivery-id))
               (registry-board (e-board-runtime-attachment-board attachment))
               (board (e-board-registry-board-source-board registry-board))
               (pickup (e-board-pickup board delivery-id)))
          (when-let ((record (e-board-runtime--producer-delivery-record
                              delivery-id)))
            (when (and pickup
                       (e-board-runtime-producer-delivery-p record)
                       (eq (e-board-runtime-producer-delivery-attachment record)
                           attachment)
                       (e-board-runtime--attempt-belongs-to-attachment-p
                        pickup attachment))
              (e-board-runtime--producer-delivery-terminal
               (e-board-runtime-producer-delivery-item record)
               delivery-id 'failed
               (list :reason (or (plist-get payload :reason)
                                 'input-discarded)))))
          (when (and pickup
                     (memq (e-board-pickup-state pickup) '(accepted cancelling))
                     (e-board-runtime--consumption-receipt-matches-p
                      pickup attachment payload))
            (when-let ((next-id
                        (e-board-pickup-discard-delivery
                         board delivery-id
                         (or (plist-get payload :reason) 'input-discarded))))
              (e-board-runtime--enqueue-pickups registry-board (list next-id)))
            (e-board-runtime--forget-terminal-pickup attachment delivery-id)))))
       ((eq type 'session-reset)
        (when receipt-p
          (let* ((registry-board (e-board-runtime-attachment-board attachment))
               (board (e-board-registry-board-source-board registry-board))
               (participant-id
                (e-board-registry-participant-id
                 (e-board-runtime-attachment-participant attachment)))
               (delivery-id (car (e-board-pickup-ids board participant-id)))
               (pickup (and delivery-id (e-board-pickup board delivery-id))))
          (when (and pickup
                     (memq (e-board-pickup-state pickup) '(accepted cancelling)))
            (when-let ((next-id
                        (e-board-pickup-discard-delivery
                         board delivery-id 'session-reset)))
              (e-board-runtime--enqueue-pickups registry-board (list next-id)))
            (e-board-runtime--forget-terminal-pickup attachment delivery-id))))))
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

(cl-defun e-board-runtime-admission-available-p
    (board-or-id harness session-id participant-id
                 &key principal (require-session t))
  "Return non-nil when one BOARD runtime admission is currently available.
The query owns the process-local conflicts checked by attachment: active board
admission, participant identity, principal authorization, and one concrete
HARNESS SESSION-ID endpoint.  When REQUIRE-SESSION is nil the session may be a
newly predicted id; this is the preflight used before participant creation.
The function only reads registry/runtime/session state and never allocates or
publishes an attachment."
  (e-board-runtime--require-admission)
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (signal 'e-board-runtime-error
            (list "Admission requires a non-empty session id" session-id)))
  (unless (and (stringp participant-id) (not (string-empty-p participant-id)))
    (signal 'e-board-registry-error
            (list "Admission requires a non-empty participant id"
                  participant-id)))
  (let* ((board (e-board-runtime--active-board board-or-id))
         (store (e-harness-sessions harness))
         (session-key (e-board-runtime--session-key harness session-id))
         (participants (e-board-registry-board-participants board)))
    (when require-session
      (e-board-runtime--require-live-session harness session-id))
    (when (and principal
               (not (e-board-registry-principal-role board principal)))
      (signal 'e-board-registry-authorization-denied
              (list (e-board-registry-board-id board) principal 'participant)))
    (when (gethash participant-id participants)
      (signal 'e-board-registry-id-conflict (list participant-id)))
    (when (or (gethash session-key e-board-runtime--session-attachments)
              (gethash session-key e-board-runtime--endpoint-attachments))
      (signal 'e-board-runtime-session-busy (list session-key)))
    ;; For a predicted id, keep the query useful even before the runtime
    ;; session exists: an explicit duplicate is still an admission conflict.
    (when (and (not require-session)
               (e-session-session-present-p store session-id))
      (signal 'e-session-duplicate (list session-id)))
    t))

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
          (e-board-copy-envelope-value
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
  (let* ((attached-turn-port (e-board-runtime-attachment-turn-port
                              attachment))
         (prompt (e-board-pickup-content pickup))
         (metadata (e-board-runtime--delivery-metadata pickup)))
    (unless (and (stringp prompt) (not (string-empty-p prompt)))
      (user-error "Board input content must be a non-empty string"))
    (let* ((active-observation (e-harness-attached-turn-port-active-turn
                                attached-turn-port))
           (active-turn
            (and active-observation
                 (eq (plist-get active-observation :status) 'running)))
           (lane (pcase (e-board-pickup-mode pickup)
                   ('queue (if active-turn 'follow-up 'idle))
                   ('inject (if active-turn 'steer 'idle))))
           (source-board
            (e-board-registry-board-source-board
             (e-board-runtime-attachment-board attachment))))
      (when (e-board-storage-backed-p source-board)
        (e-board-pickup-admission-commit
         source-board (e-board-pickup-delivery-id pickup)
         (e-harness-sessions (e-board-runtime-attachment-harness attachment))
         (e-board-runtime-attachment-session-id attachment) lane
         :metadata (e-board-pickup-cause-metadata pickup)))
      (pcase (e-board-pickup-mode pickup)
        ('queue
         (if active-turn
             (list (if (e-board-storage-backed-p source-board)
                       :committed-accepted
                     :accepted)
                   (e-harness-attached-turn-port-follow-up
                    attached-turn-port prompt :metadata metadata))
           (let ((turn-id
                  (e-harness-attached-turn-port-submit
                   attached-turn-port prompt :metadata metadata)))
             (puthash turn-id
                      (copy-tree
                       (plist-get (e-board-pickup-cause-metadata pickup)
                                  :routing-tags))
                      (e-board-runtime-attachment-turn-tags attachment))
             :consumed)))
        ('inject
         (let ((turn-id
                (if active-turn
                    (e-harness-attached-turn-port-steer
                     attached-turn-port prompt :metadata metadata)
                  (e-harness-attached-turn-port-submit
                   attached-turn-port prompt :metadata metadata))))
           (puthash turn-id
                    (copy-tree
                     (plist-get (e-board-pickup-cause-metadata pickup)
                                :routing-tags))
                    (e-board-runtime-attachment-turn-tags attachment))
           :consumed))))))

(defun e-board-runtime-deliver-to-harness (attachment pickup message)
  "Deliver PICKUP through ATTACHMENT's ordinary harness endpoint."
  (e-board-runtime--deliver-to-harness attachment pickup message))

(cl-defun e-board-runtime-attach
    (board-or-id harness session-id
                 &key participant-id author principal controller delivery-function
                 defer-participant-publication)
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
   :delivery-function delivery-function
   :defer-participant-publication defer-participant-publication))

(cl-defun e-board-runtime-reattach
    (board-or-id harness session-id participant-id
                 &key author principal controller delivery-function)
  "Attach HARNESS SESSION-ID to restored PARTICIPANT-ID on BOARD-OR-ID.
The participant must already be the canonical durable Board projection.  This
operation creates only process-local endpoint state and never republishes or
removes the durable participant identity."
  (e-board-runtime--require-admission)
  (e-board-runtime--attach-resolved
   board-or-id harness session-id
   :participant-id participant-id :author author :principal principal
   :controller controller :delivery-function delivery-function
   :restored-participant-p t))

(cl-defun e-board-runtime--attach-resolved
    (board-or-id harness session-id
                 &key participant-id author principal controller delivery-function
                 instance-id instance-catalog-generation harness-id
                 harness-object-generation endpoint-token
                 defer-participant-publication restored-participant-p)
  "Attach one already-resolved endpoint with optional qualified metadata."
  (unless (e-harness-p harness)
    (signal 'wrong-type-argument (list 'e-harness-p harness)))
  (unless (or (null delivery-function) (functionp delivery-function))
    (signal 'wrong-type-argument (list 'functionp delivery-function)))
  (e-board-runtime--require-live-session harness session-id)
  (let ((session-key (e-board-runtime--session-key harness session-id))
        (endpoint-key (e-board-runtime--session-key harness session-id)))
    (when (or (gethash session-key e-board-runtime--session-attachments)
              (gethash endpoint-key e-board-runtime--endpoint-attachments))
      (signal 'e-board-runtime-session-busy (list session-key))))
  (let* ((board (e-board-runtime--active-board board-or-id))
         (source-board (e-board-registry-board-source-board board))
         participant participant-created-p attachment)
    ;; A previous exact work admission may outlive its captured attachment.
    ;; Re-ensure cannot publish a replacement endpoint around that unfinished
    ;; board inverse; retry the same token before creating new participant
    ;; authority.
    (e-board-runtime-admission-retry nil source-board t)
    (condition-case error
        (progn
          (if restored-participant-p
              (progn
                (unless participant-id
                  (signal 'e-board-runtime-error
                          (list "Restored attachment requires participant id")))
                (setq participant
                      (e-board-registry-participant board participant-id))
                (when (and principal
                           (not (equal
                                 principal
                                 (e-board-registry-participant-principal
                                  participant))))
                  (signal 'e-board-registry-authorization-denied
                          (list (e-board-registry-board-id board)
                                participant-id 'restored-principal))))
            (setq participant
                  (e-board-registry-add-participant
                   board :id participant-id :author author :principal principal
                   :controller controller
                   :publish-event (not defer-participant-publication))
                  participant-created-p t))
          (let ((key (e-board-runtime--attachment-key board participant)))
            (when (gethash key e-board-runtime--attachments)
              (signal 'e-board-runtime-attachment-exists (list key))))
          (setq attachment
                (e-board-runtime--make-attachment
                 board participant harness session-id delivery-function 1
                 :instance-id instance-id
                 :instance-catalog-generation instance-catalog-generation
                 :harness-id harness-id
                 :harness-object-generation harness-object-generation
                 :endpoint-token endpoint-token))
          (e-board-runtime--activate-attachment attachment)
          (when restored-participant-p
            (e-board-registry-activate-restored-participant board participant)
            (e-board-runtime--enqueue-ready-participant-pickup attachment))
          attachment)
      (error
       (when attachment
         (e-board-runtime-abort-new-attachment attachment))
       (when (and participant-created-p participant
                  (gethash (e-board-registry-participant-id participant)
                           (e-board-registry-board-participants board)))
         (e-board-registry-abort-participant-admission board participant))
       (signal (car error) (cdr error))))))

(cl-defun e-board-runtime-attach-instance
    (board-or-id instance-id session-id
                 &key participant-id author principal controller delivery-function
                 defer-participant-publication)
  "Attach an existing live SESSION-ID through configured INSTANCE-ID.
This operation never invokes an instance factory or loads dormant history."
  (e-board-runtime--require-admission)
  (e-board-runtime--attach-instance-resolved
   board-or-id instance-id session-id
   :participant-id participant-id :author author :principal principal
   :controller controller :delivery-function delivery-function
   :defer-participant-publication defer-participant-publication))

(cl-defun e-board-runtime--attach-instance-resolved
    (board-or-id instance-id session-id
                 &key participant-id author principal controller delivery-function
                 defer-participant-publication)
  "Attach one admitted live SESSION-ID through INSTANCE-ID to BOARD-OR-ID."
  (let* ((instance-generation (e-harness-instance-generation))
         (instance (or (e-harness-instance-get instance-id)
                       (signal 'e-harness-instance-missing (list instance-id))))
         (harness-id (e-harness-instance-harness-id instance))
         (harness (e-harness-registry-get harness-id))
         (harness-generation (e-harness-registry-generation harness-id)))
    (unless harness
      (signal 'e-harness-registry-missing (list harness-id)))
    (unless (= instance-generation (e-harness-instance-generation))
      (signal 'e-board-runtime-error (list instance-id 'stale-instance-catalog)))
    (let ((token (e-board-runtime-endpoint-token--create
                  :harness-id harness-id
                  :harness-object-generation harness-generation
                  :session-id session-id)))
      (e-board-runtime--attach-resolved
       board-or-id harness session-id
       :participant-id participant-id :author author :principal principal
       :controller controller
       :delivery-function delivery-function
       :defer-participant-publication defer-participant-publication
       :instance-id instance-id
       :instance-catalog-generation instance-generation
       :harness-id harness-id
       :harness-object-generation harness-generation
       :endpoint-token token))))

(defun e-board-runtime--make-attachment
    (board participant harness session-id delivery-function generation
           &rest metadata)
  "Construct one immutable-generation attachment without registering it."
  (let ((attachment
         (e-board-runtime-attachment--create
          :board board :participant participant :harness harness :session-id session-id
          :activity-sequence 0 :generation generation :state 'active
          :turn-activity (make-hash-table :test 'equal)
          :turn-tags (make-hash-table :test 'equal)
          :turn-delivery-ids (make-hash-table :test 'equal)
          :identity-token (make-symbol "board-runtime-attachment-")
          :owned-pickup-ids (make-hash-table :test 'equal)
          :producer-delivery-ids (make-hash-table :test 'equal)
          :producer-turn-keys (make-hash-table :test 'equal)
          :activity-mailbox-keys (make-hash-table :test 'equal)
          :invocation-targets (make-hash-table :test 'equal)
          :retirement-stage 'live
          :retirement-authorized-p nil
          :retirement-work-authorized-p t
          :retirement-generation nil
          :delivery-function (or delivery-function #'e-board-runtime--deliver-to-harness)
          :instance-id (plist-get metadata :instance-id)
          :instance-catalog-generation
          (plist-get metadata :instance-catalog-generation)
          :harness-id (plist-get metadata :harness-id)
          :harness-object-generation
          (plist-get metadata :harness-object-generation)
          :endpoint-token
          (or (plist-get metadata :endpoint-token)
              (e-board-runtime-endpoint-token--create
               :harness-id (list 'direct (e-session-generate-ulid))
               :session-id session-id)))))
    (setf (e-board-runtime-attachment-turn-port attachment)
          (e-board-runtime--make-attached-turn-port attachment))
    attachment))

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
          (e-harness-attached-turn-port-observe-activity
           (e-board-runtime-attachment-turn-port attachment)
           (lambda (event) (e-board-runtime--handle-harness-event attachment event))))
    (puthash key attachment e-board-runtime--attachments)
    (puthash session-key attachment e-board-runtime--session-attachments)
    (puthash endpoint-key attachment e-board-runtime--endpoint-attachments)
    (e-board-runtime--configure-attachment attachment)
    ;; Existing participant FIFO records become this attachment's local
    ;; ownership set at activation; later enqueue edges add only their exact
    ;; delivery ids.
    (dolist (delivery-id
             (e-board-pickup-ids
              (e-board-registry-board-source-board board)
              (e-board-registry-participant-id participant)))
      (e-board-runtime--index-attachment-pickup attachment delivery-id))
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
    (e-board-runtime--unsubscribe-attachment-activity attachment)
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
                (e-harness-attached-turn-port-observe-activity
                 (e-board-runtime-attachment-turn-port attachment)
                 (lambda (event)
                   (e-board-runtime--handle-harness-event attachment event))))
          (e-board-runtime--configure-attachment attachment)
          attachment)
      (error
       (e-board-runtime--unsubscribe-attachment-activity attachment)
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
    ;; A consumed turn has already quiesced before this commit.  Settle its
    ;; exact producer handoff before OLD becomes dormant; the transfer helper
    ;; settles any non-pending receipt and only pending/ready FIFO work crosses
    ;; the rebind.
    (e-board-runtime--settle-producer-turns-for-attachment old)
    (setq new (e-board-runtime--prepare-rebind-attachment reconciliation))
    (e-board-runtime--transfer-attachment-pending-work old new)
    (e-board-runtime--unsubscribe-attachment-activity old)
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
           :endpoint-token (e-board-runtime-attachment-endpoint-token old))))
    (condition-case condition
        (progn
          (setf (e-board-runtime-attachment-subscription attachment)
                (e-harness-attached-turn-port-observe-activity
                 (e-board-runtime-attachment-turn-port attachment)
                 (lambda (event)
                   (when (e-board-runtime-attachment-participant attachment)
                     (e-board-runtime--handle-harness-event attachment event)))))
          (e-board-runtime--configure-attachment attachment)
          attachment)
      (error
       (e-board-runtime--unsubscribe-attachment-activity attachment)
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
    ;; Move cancels the old participant FIFO rather than preserving it.  Close
    ;; any exact producer turn/receipt left by the quiescence proof before the
    ;; attachment changes board ownership.
    (e-board-runtime--settle-producer-turns-for-attachment old)
    (e-board-runtime--settle-producer-deliveries-for-attachment old)
    (setq new (e-board-runtime--prepare-move-attachment reconciliation))
    (condition-case condition
        (setq moved-participant
              (e-board-registry-move-participant
               source destination requester participant
               destination-participant-id))
      (error
       (e-board-runtime--unsubscribe-attachment-activity new)
       (signal (car condition) (cdr condition))))
    (setf (e-board-runtime-attachment-participant new) moved-participant)
    (e-board-runtime--unsubscribe-attachment-activity old)
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
         (delivery-id (car (e-board-pickup-ids source-board participant-id)))
         (pickup (and delivery-id (e-board-pickup source-board delivery-id)))
         (turn-port (e-board-runtime-attachment-turn-port attachment))
         (active-observation
          (e-harness-attached-turn-port-active-turn turn-port))
         (active-turn (and active-observation
                           (eq (plist-get active-observation :status) 'running)
                           (plist-get active-observation :id))))
    (unless (e-board-runtime--current-attachment-p attachment)
      (signal 'e-board-runtime-error
              (list "Participant attachment changed during reconciliation"
                    participant-id)))
    (cond
     ((and pickup (memq (e-board-pickup-state pickup) '(pending ready)))
      (e-board-cancel-pickup source-board delivery-id reason)
      ;; The pickup is terminal now, so its producer delivery no longer has a
      ;; future harness receipt that could settle it.  Keep the settlement
      ;; keyed by this exact attachment; a replacement must not inherit it.
      (e-board-runtime--settle-producer-delivery-for-attachment
       attachment delivery-id 'cancelled (list :reason reason))
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
           (e-harness-attached-turn-port-discard-queued-board-input
            turn-port delivery-id
            (e-board-delivery-attempt-endpoint-token
             (e-board-pickup-attempt pickup))
            (e-board-delivery-attempt-composite-generation
             (e-board-pickup-attempt pickup))
            reason))
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress
       request (list :phase 'discarding-session-inbox
                     :delivery-id delivery-id)))
     ((e-harness-attached-turn-port-queued-prompts turn-port)
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
         (delivery-id (car (e-board-pickup-ids source-board participant-id)))
         (pickup (and delivery-id (e-board-pickup source-board delivery-id)))
         (turn-port (e-board-runtime-attachment-turn-port old))
         (active-observation
          (e-harness-attached-turn-port-active-turn turn-port))
         (active-turn (and active-observation
                           (eq (plist-get active-observation :status) 'running)
                           (plist-get active-observation :id))))
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
           (e-harness-attached-turn-port-discard-queued-board-input
            turn-port delivery-id
            (e-board-delivery-attempt-endpoint-token
             (e-board-pickup-attempt pickup))
            (e-board-delivery-attempt-composite-generation
             (e-board-pickup-attempt pickup))
            'endpoint-rebound))
      (setf (e-board-runtime-reconciliation-state reconciliation) 'waiting)
      (e-request-progress
       request (list :phase 'discarding-session-inbox
                     :delivery-id delivery-id)))
     ((e-harness-attached-turn-port-queued-prompts turn-port)
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
          ;; A deferred classifier may have produced this pickup under a
          ;; participant object that has since been removed/replaced.  Fence
          ;; that exact causal route before any participant-id lookup; a
          ;; replacement must never inherit the old delivery.
          (if (not (e-board-pickup-route-current-p source-board pickup))
              (progn
                (when-let ((next-id
                            (e-board-fail-pickup
                             source-board delivery-id
                             'participant-lifetime-changed)))
                  (e-board-runtime--enqueue-pickups board (list next-id)))
                (e-board-runtime--settle-producer-delivery-id
                 delivery-id 'cancelled
                 (list :reason 'participant-lifetime-changed)))
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
                 (e-board-runtime--enqueue-pickups board (list next-id)))
               ;; Routing accepted this exact delivery, but authorization has
               ;; since revoked it.  Settle the producer by delivery identity;
               ;; resolving the participant again here could attach the old
               ;; work to a same-id replacement.
               (e-board-runtime--settle-producer-delivery-id
                delivery-id 'cancelled
                (list :reason 'delivery-authorization-revoked)))
              ('authorized
               ;; A classifier already queued before terminal retirement may
               ;; reach this bounded drain after the attachment has entered its
               ;; retiring stage.  Cancel only that exact current attachment's
               ;; ready pickup and settle its producer record; detached/rebind
               ;; reconciliation retains its own normal receipt protocol.
               (when-let ((retiring-attachment
                           (gethash
                            (and participant
                                 (e-board-runtime--attachment-key
                                  board participant))
                            e-board-runtime--attachments)))
                 (when (and (eq (e-board-runtime-attachment-state
                                 retiring-attachment) 'retiring)
                            (e-board-runtime--current-attachment-p
                             retiring-attachment))
                   (e-board-cancel-pickup
                    source-board delivery-id 'attachment-retired)
                   (e-board-runtime--settle-producer-delivery-for-attachment
                    retiring-attachment delivery-id 'cancelled
                    (list :reason 'attachment-retired))
                   (e-board-runtime--forget-attachment-pickup
                    retiring-attachment delivery-id)))
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
                         (:committed-accepted
                          ;; The composite worker command already accepted the
                          ;; pickup before the turn-port effect was invoked.
                          nil)
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
                         (:deferred
                          ;; The application-service attachment owns the
                          ;; bounded delivery id until its prerequisite work
                          ;; settles.  Restore the pickup to ready without
                          ;; scheduling a generic runtime retry; the owner
                          ;; explicitly resumes this exact id once.
                          (e-board-pickup-return-ready
                           source-board delivery-id (cadr result)))
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
                          ;; A synchronous endpoint can publish its exact
                          ;; consumption receipt before returning here.  That
                          ;; receipt already owns terminalization; do not
                          ;; attempt a second durable consume for the same
                          ;; immutable pickup.
                          (when (memq (e-board-pickup-state pickup)
                                      '(delivering accepted cancelling))
                            (when-let* ((next-id
                                         (e-board-pickup-complete-delivery
                                          source-board delivery-id)))
                              (e-board-runtime--enqueue-pickups
                               board (list next-id)))))))
                   (error
                    (if (eq (e-board-pickup-state pickup) 'accepted)
                        (e-board-pickup-mark-uncertain
                         source-board delivery-id
                         (list 'post-admission-effect-failed err))
                      (e-board-pickup-return-ready source-board delivery-id err))
                    (when (and (eq (e-board-pickup-state pickup) 'ready)
                               (not (eq (car err)
                                        'e-board-runtime-session-busy)))
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
                         board (list delivery-id))))))))))
            ;; Terminal delivery results no longer need a second local pickup
            ;; index entry.  The lookup is by this participant's exact map key,
            ;; never by a board-wide scan.
            (e-board-runtime--forget-terminal-pickup-for-board
             board delivery-id)))))))

(defun e-board-runtime-resume-pickup-deliveries (board pickup-ids)
  "Resume application-owned ready PICKUP-IDS for BOARD exactly once."
  (e-board-runtime--deliver-pickups board (copy-sequence pickup-ids)))

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

(cl-defun e-board-runtime--post-participant-input
    (attachment &key id author tags attributes to (mode 'inject)
                content reference source-input-key)
  "Post one continuation as current ATTACHMENT's participant actor.
This private path does not reopen admission: the attached turn already owns
the authority to continue its interaction through the board."
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

(cl-defun e-board-runtime-post-participant-input
    (attachment &key id author tags attributes to (mode 'inject)
                content reference source-input-key)
  "Post a new public input as ATTACHMENT's authenticated participant actor."
  (e-board-runtime--require-admission)
  (e-board-runtime--post-participant-input
   attachment :id id :author author :tags tags :attributes attributes :to to
   :mode mode :content content :reference reference
   :source-input-key source-input-key))

(cl-defun e-board-runtime--publish-attached-follow-up
    (harness session-id prompt &key references metadata tags)
  "Publish HARNESS SESSION-ID's follow-up through its current attachment."
  (let ((attachment
         (gethash (e-board-runtime--session-key harness session-id)
                  e-board-runtime--endpoint-attachments)))
    (unless (and attachment
                 (e-board-runtime--current-attachment-p attachment)
                 (eq (e-board-runtime-attachment-state attachment) 'active))
      (signal 'e-harness-board-attachment-required (list session-id)))
    (let ((participant-id
           (e-board-registry-participant-id
            (e-board-runtime-attachment-participant attachment))))
      (e-board-runtime--post-participant-input
       attachment
       :author (format "participant:%s" participant-id)
       :tags tags
       :attributes
       (append (copy-tree metadata)
               (and references (list :references (copy-tree references))))
       :to participant-id
       :mode 'queue
       :content prompt
       :reference (copy-tree references)))))

(defun e-board-runtime-attachment-active-turn (attachment)
  "Return current ATTACHMENT's attached execution identity/status, or nil."
  (and (e-board-runtime--current-attachment-p attachment)
       (e-harness-attached-turn-port-active-turn
        (e-board-runtime-attachment-turn-port attachment))))

(defun e-board-runtime-attachment-active-turn-p (attachment)
  "Return non-nil when current ATTACHMENT owns a live harness turn."
  (e-board-runtime-attachment-active-turn attachment))

(defun e-board-runtime-abort-attachment (attachment)
  "Abort the current board-bound ATTACHMENT turn through runtime admission."
  (e-board-runtime--require-admission)
  (unless (e-board-runtime--current-attachment-p attachment)
    (signal 'e-board-runtime-error (list "Stale attachment" attachment)))
  (e-harness-attached-turn-port-abort
   (e-board-runtime-attachment-turn-port attachment)))

(defun e-board-runtime--make-attached-turn-port (attachment)
  "Return the explicit harness port owned by ATTACHMENT.
The closures capture one attachment identity and are passed to the harness at
the call boundary.  No process-global callback or service lookup is involved;
stale attachments therefore fail the same current-ownership check as board
publication itself."
  (e-harness-attached-turn-port-create
   :harness (e-board-runtime-attachment-harness attachment)
   :session-id (e-board-runtime-attachment-session-id attachment)
   :attachment-token (e-board-runtime-attachment-endpoint-token attachment)
   :authorizer
   (lambda (harness session-id token)
     (and (eq harness (e-board-runtime-attachment-harness attachment))
          (equal session-id
                 (e-board-runtime-attachment-session-id attachment))
          (e-board-runtime--current-attachment-p attachment)
          (eq (e-board-runtime-attachment-state attachment) 'active)
          (equal token (e-board-runtime-attachment-endpoint-token attachment))))
   :follow-up-publisher
   (lambda (harness session-id prompt &rest args)
     (unless (and (eq harness (e-board-runtime-attachment-harness attachment))
                  (equal session-id
                         (e-board-runtime-attachment-session-id attachment)))
       (signal 'e-harness-board-attachment-required (list session-id)))
     (apply #'e-board-runtime--publish-attached-follow-up
            harness session-id prompt args))))

(defun e-board-runtime--unsubscribe-attachment-activity (attachment)
  "Remove ATTACHMENT's activity observer through its attached-turn port."
  (when (e-board-runtime-attachment-subscription attachment)
    (e-harness-attached-turn-port-stop-observing
     (e-board-runtime-attachment-turn-port attachment)
     (e-board-runtime-attachment-subscription attachment))
    ;; Clear only after the owner accepted the stop.  If it signals, the exact
    ;; handle remains available for the retryable retirement stage.
    (setf (e-board-runtime-attachment-subscription attachment) nil)))

(provide 'e-board-runtime)

;;; e-board-runtime.el ends here

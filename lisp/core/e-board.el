;;; e-board.el --- Process-local board routing core for e -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This is the pure, process-local board model.  It owns no harness endpoint
;; and performs no delivery; consumers inspect the frozen pickup envelopes.

;;; Code:

(require 'cl-lib)
(require 'e-work)
(require 'e-board-state)
(require 'e-board-admission)

(defvar e-board--id-sequence 0
  "Process-local fallback sequence for board identities.")

(defvar e-board--registry (make-hash-table :test 'equal)
  "Live process-local boards keyed by board id.")

(defconst e-board--ordinary-subscription-states
  '(active muted completed faulted cancelled expired)
  "States available to ordinary orchestration subscriptions.")

(defconst e-board--observer-states
  '(active muted faulted cancelled expired)
  "States available to effect-free board observers.")

(defconst e-board-max-derived-hops 8
  "Maximum subscription lineage depth for derived board inputs.")

(defconst e-board-processing-priority-min -1000
  "Lowest accepted priority for a processing subscription.")

(defconst e-board-processing-priority-max 1000
  "Highest accepted priority for a processing subscription.")

(defconst e-board-processing-history-limit 8
  "Maximum retained processor entries in one processing chain.")

(defconst e-board-terminal-classification-drain-limit 16
  "Maximum indexed terminal subscription clauses classified per drain.")

(defconst e-board-input-classification-drain-limit 32
  "Maximum frozen input subscription clauses classified per drain.")

(defconst e-board-input-fanout-limit 64
  "Maximum participants one input may address in one atomic routing commit.")

(defconst e-board-pickup-subscription-limit 64
  "Maximum matching subscription clauses retained by one logical pickup.")

(defconst e-board-subscription-replay-drain-limit 32
  "Maximum retained records classified for explicit continuation replay.")

(defconst e-board-aggregation-deadline-drain-limit 16
  "Maximum aggregation deadline transitions committed per drain.")

(defconst e-board-default-pickup-pending-limit 16
  "Maximum FIFO pickups allowed behind one participant's active head.")

(defconst e-board-message-content-byte-limit (* 256 1024)
  "Maximum retained byte budget for one board message content value.")

(defconst e-board-message-reference-byte-limit (* 64 1024)
  "Maximum retained byte budget for one board message reference value.")

(defconst e-board-message-attributes-byte-limit (* 64 1024)
  "Maximum retained byte budget for one board message attributes value.")

(defconst e-board-message-metadata-byte-limit (* 32 1024)
  "Maximum retained byte budget for one board message causal metadata field.")

(defconst e-board-message-tags-byte-limit (* 8 1024)
  "Maximum retained byte budget for one board message tag collection.")

(defconst e-board--aggregation-modes
  '(all any all-terminal first-terminal on-success on-failure on-terminal)
  "Accepted private aggregation readiness modes.")

(defun e-board-unsettled-state (board)
  "Return BOARD's constant-time nonterminal owner projection."
  (e-board-state-unsettled-state board))

(defun e-board--next-id (board kind)
  "Return BOARD's next identity for KIND.
An injected id function receives KIND.  The fallback is only process-local and
exists so callers need not supply ids outside deterministic tests."
  (let ((id (if-let ((function (e-board-id-function board)))
                (funcall function kind)
              (format "%s%d"
                      (pcase kind
                        ('board "brd_")
                        ('client "cli_")
                        ('participant "ptc_")
                        ('subscription "sub_")
                        ('invocation "inv_")
                        ('message "msg_")
                        (_ (format "%s_" kind)))
                      (cl-incf e-board--id-sequence)))))
    (unless id
      (signal 'e-board-error (list "Id generator returned nil" kind)))
    id))

(defun e-board-pending-admission-count (board)
  "Return the number of exact board admissions awaiting cleanup."
  (e-board-admission-pending-count board))

(defun e-board--recover-pending-admissions (board)
  "Retry all exact board-owned admissions pending on BOARD.

The snapshot is made from the catalog's admission objects, not from ids or
board maps.  A failed inverse remains in the catalog and blocks a new related
operation with `e-board-admission-pending'."
  (e-board-admission-require-clear board))

(defun e-board-aggregation-admission-token (board)
  "Create an opaque exact token for one BOARD aggregation admission.

The token is intentionally empty until the board fills its exact aggregation,
receipt, timer, and classifier fields.  Runtime callers retain this object
across reentrant board entry and pass it to the exact abort inverse."
  (e-board-admission-aggregation-token board))

(defun e-board-aggregation-admission-current-p (admission)
  "Return non-nil when ADMISSION still owns its complete board projection.

The map alone is not enough: a partial index/event/classifier inverse must also
invalidate the application transaction while preserving its exact abort token."
  (e-board-admission-current-p admission))

(cl-defun e-board-work-admission-token
    (handle &key invocation-id effect-target)
  "Create an opaque exact token for one staged HANDLE admission."
  (e-board-admission-work-token
   handle :invocation-id invocation-id :effect-target effect-target))

(defun e-board-work-admission-current-p (admission)
  "Return whether ADMISSION still owns its committed Board projection."
  (e-board-admission-current-p admission))

(defun e-board-abort-work-enrollment (board admission)
  "Abort the exact Board projection represented by ADMISSION."
  (e-board-admission-abort board admission))

(defun e-board-abort-aggregation-admission (board admission)
  "Abort the exact Board aggregation projection represented by ADMISSION."
  (e-board-admission-abort board admission))

(defun e-board-complete-admission (board admission)
  "Finalize a committed BOARD admission after its owner postcheck.

The lower admission owner performs the exact cleanup and drops rollback-only
receipts.  This facade operation is the stable Board completion contract used
by composed owners; it is deliberately distinct from the lower owner's
implementation name so loading the two modules cannot create a recursive
forwarding definition."
  (e-board-admission-complete board admission))

(defun e-board--reserve-imported-fallback-message-id (board id)
  "Advance the fallback allocator past imported message ID when applicable.

An injected board id function owns its own identity domain.  The built-in
process-local allocator uses numeric `msg_N' identities, so replay must reserve
the highest imported suffix before the restored board accepts new messages."
  (when (and (null (e-board-id-function board))
             (stringp id)
             (string-match "\\`msg_\\([0-9]+\\)\\'" id))
    (setq e-board--id-sequence
          (max e-board--id-sequence
               (string-to-number (match-string 1 id))))))

(defun e-board--require-id (id name)
  "Return ID or signal that required identity NAME is absent."
  (unless id
    (signal 'wrong-type-argument (list name id)))
  id)

(defun e-board-register (board)
  "Register BOARD in the process-local board registry and return it."
  (unless (e-board-p board)
    (signal 'wrong-type-argument (list 'e-board-p board)))
  (let ((id (e-board-id board)))
    (when-let ((existing (gethash id e-board--registry)))
      (unless (eq existing board)
        (signal 'e-board-id-conflict (list id))))
    (puthash id board e-board--registry))
  board)

(defun e-board-get (id)
  "Return the registered board ID, or signal `e-board-missing'."
  (or (gethash id e-board--registry)
      (signal 'e-board-missing (list id))))

(defun e-board-unregister (board-or-id)
  "Remove BOARD-OR-ID from the process-local registry.
The board object remains valid for inspection by its holder."
  (let ((id (if (e-board-p board-or-id)
                (e-board-id board-or-id)
              board-or-id)))
    (remhash id e-board--registry))
  nil)

(defun e-board-list ()
  "Return registered boards sorted by printable identity."
  (let (boards)
    (maphash (lambda (_id board) (push board boards)) e-board--registry)
    (sort boards (lambda (left right)
                   (string< (format "%s" (e-board-id left))
                            (format "%s" (e-board-id right)))))))

(cl-defun e-board-create
    (&key id id-function effect-scheduler invocation-effect-dispatcher
          classification-authorizer message-notification-function
          processing-record-notification-function unsettled-change-function
          terminal-classification-scheduler input-classification-scheduler
          aggregation-deadline-scheduler continuation-timer-scheduler
          subscription-timer-scheduler
          (pickup-pending-limit e-board-default-pickup-pending-limit)
          (retention-floor 0)
          (register t))
  "Create a process-local board with ID and optional ID-FUNCTION.
ID-FUNCTION receives a symbol such as `message' or `subscription'.  Passing
explicit ids to individual operations takes precedence over this generator.
PICKUP-PENDING-LIMIT bounds records queued behind a participant's active head."
  (unless (and (integerp pickup-pending-limit) (>= pickup-pending-limit 0))
    (signal 'wrong-type-argument (list 'natnump pickup-pending-limit)))
  (unless (and (integerp retention-floor) (>= retention-floor 0))
    (signal 'wrong-type-argument (list 'natnump retention-floor)))
  (unless (or (null classification-authorizer)
              (functionp classification-authorizer))
    (signal 'wrong-type-argument (list 'functionp classification-authorizer)))
  (unless (or (null unsettled-change-function)
              (functionp unsettled-change-function))
    (signal 'wrong-type-argument (list 'functionp unsettled-change-function)))
  (unless (or (null message-notification-function)
              (functionp message-notification-function))
    (signal 'wrong-type-argument
            (list 'functionp message-notification-function)))
  (unless (or (null processing-record-notification-function)
              (functionp processing-record-notification-function))
    (signal 'wrong-type-argument
            (list 'functionp processing-record-notification-function)))
  (let* ((event-prefix (let ((table (make-hash-table :test 'eql)))
                         (puthash 0 0 table)
                         table))
         (board (e-board-state-create
                  :id (or id (format "brd_%d" (cl-incf e-board--id-sequence)))
                 :id-function id-function
                 :next-seq 0
                 :events nil
                  :events-tail nil
                  :messages nil
                  :messages-tail nil
                  :message-count 0
                  :message-table (make-hash-table :test 'equal)
                  :message-seq-table (make-hash-table :test 'eql)
                  :message-index-table (make-hash-table :test 'eql)
                 :message-kind-newest-table (make-hash-table :test 'eq)
                 :message-kind-tag-newest-table (make-hash-table :test 'equal)
                 :event-node-index (make-hash-table :test 'eq)
                 :pending-admissions (make-hash-table :test 'eq)
                 :event-message-count event-prefix
                 :event-message-prefix-high-watermark 0
                 :participants (make-hash-table :test 'equal)
                 :subscriptions nil
                 :subscriptions-tail nil
                 :subscription-count 0
                 :subscription-index-table (make-hash-table :test 'eql)
                 :subscription-id-table (make-hash-table :test 'equal)
                 :subscription-lifetime-sequence 0
                 :processing-chains-internal nil
                 :processing-chains-tail-internal nil
                 :processing-chain-table-internal (make-hash-table :test 'equal)
                 :processing-chain-reservations (make-hash-table :test 'equal)
                 :processing-results-internal nil
                 :processing-results-tail-internal nil
                 :processing-result-table-internal (make-hash-table :test 'equal)
                 :processing-result-reservations (make-hash-table :test 'equal)
                 :processing-record-notification-function
                 processing-record-notification-function
                 :observers (make-hash-table :test 'equal)
                  :pickups (make-hash-table :test 'equal)
                  :source-high-watermarks (make-hash-table :test 'equal)
                  :source-recent (make-hash-table :test 'equal)
                   :work-table (make-hash-table :test 'equal)
                   :invocations (make-hash-table :test 'equal)
                   :aggregations (make-hash-table :test 'equal)
                  :activations (make-hash-table :test 'equal)
                  :activation-subscription-index (make-hash-table :test 'equal)
                  :pickup-queues (make-hash-table :test 'equal)
                  :pickup-pending-limit pickup-pending-limit
                  :open-activities (make-hash-table :test 'equal)
                  :closed-activities (make-hash-table :test 'equal)
                 :retention-floor retention-floor
                  :pending-effects nil
                  :pending-effects-tail nil
                  :effect-node-index (make-hash-table :test 'eq)
                  :effects-scheduled nil
                  :effect-schedule-generation 0
                  :effect-draining-p nil
                  :effect-callback-generation 0
                  :effect-scheduler effect-scheduler
                  :invocation-effect-dispatcher invocation-effect-dispatcher
                  :classification-authorizer classification-authorizer
                  :message-notification-function message-notification-function
                  :unsettled-pickup-count 0
                  :unsettled-effect-count 0
                  :unsettled-routing-count 0
                  :unsettled-generation 0
                  :unsettled-change-function unsettled-change-function
                  :invocation-work-index (make-hash-table :test 'equal)
                  :aggregation-work-index (make-hash-table :test 'equal)
                  :terminal-classifications nil
                  :terminal-classification-tail nil
                  :terminal-classification-node-index (make-hash-table :test 'eq)
                  :terminal-classification-scheduled nil
                  :terminal-classification-generation 0
                  :terminal-classification-callback-generation 0
                  :terminal-classification-scheduler terminal-classification-scheduler
                  :input-classifications nil
                  :input-classification-tail nil
                  :input-classification-scheduled nil
                  :input-classification-scheduler input-classification-scheduler
                  :subscription-replays nil
                  :subscription-replay-tail nil
                  :subscription-replay-scheduled nil
                  :routed-pickup-results nil
                  :routed-pickup-results-tail nil
                  :aggregation-deadlines nil
                  :aggregation-deadline-tail nil
                  :aggregation-deadline-node-index (make-hash-table :test 'eq)
                  :aggregation-deadline-scheduled nil
                  :aggregation-deadline-generation 0
                  :aggregation-deadline-callback-generation 0
                  :aggregation-deadline-scheduler aggregation-deadline-scheduler
                  :continuation-timer-scheduler continuation-timer-scheduler
                  :subscription-timer-scheduler subscription-timer-scheduler)))
    (when register (e-board-register board))
    board))

(defun e-board-events-after (board seq)
  "Return BOARD events whose sequence is strictly greater than SEQ."
  (cl-remove-if (lambda (event) (<= (e-board-event-seq event) seq))
                (e-board-events board)))

(defvar e-board--processing-replay-p nil
  "Non-nil while restoring processing records without persistence notification.")

(defvar e-board--processing-persistence-board nil
  "Board whose processing-record persistence callback is currently running.")

(defun e-board--copy-processing-record (record)
  "Return a detached copy of retained processing RECORD."
  (cond
   ((e-board-processing-chain-p record)
    (e-board-processing-chain--create
     :id (e-board--freeze-envelope-value
          (e-board-processing-chain-id record) 'processing-chain-id
          e-board-message-metadata-byte-limit)
     :board-id (e-board--freeze-envelope-value
                (e-board-processing-chain-board-id record) 'board-id
                e-board-message-metadata-byte-limit)
     :root-message-id (e-board--freeze-envelope-value
                       (e-board-processing-chain-root-message-id record)
                       'root-message-id e-board-message-metadata-byte-limit)
     :candidate-message-id (e-board--freeze-envelope-value
                            (e-board-processing-chain-candidate-message-id record)
                            'candidate-message-id e-board-message-metadata-byte-limit)
     :caused-by-message-id (e-board--freeze-envelope-value
                            (e-board-processing-chain-caused-by-message-id record)
                            'caused-by-message-id e-board-message-metadata-byte-limit)
     :processor-history (e-board--freeze-envelope-value
                         (e-board-processing-chain-processor-history record)
                         'processor-history e-board-message-metadata-byte-limit)
     :processing-depth (e-board-processing-chain-processing-depth record)
     :created-at (e-board--freeze-envelope-value
                  (e-board-processing-chain-created-at record) 'created-at
                  e-board-message-metadata-byte-limit)))
   ((e-board-processing-result-p record)
    (e-board-processing-result--create
     :id (e-board--freeze-envelope-value
          (e-board-processing-result-id record) 'processing-result-id
          e-board-message-metadata-byte-limit)
     :board-id (e-board--freeze-envelope-value
                (e-board-processing-result-board-id record) 'board-id
                e-board-message-metadata-byte-limit)
     :chain-id (e-board--freeze-envelope-value
                (e-board-processing-result-chain-id record) 'chain-id
                e-board-message-metadata-byte-limit)
     :subscription-id (e-board--freeze-envelope-value
                       (e-board-processing-result-subscription-id record)
                       'subscription-id e-board-message-metadata-byte-limit)
     :participant-id (e-board--freeze-envelope-value
                      (e-board-processing-result-participant-id record)
                      'participant-id e-board-message-metadata-byte-limit)
     :candidate-message-id (e-board--freeze-envelope-value
                            (e-board-processing-result-candidate-message-id record)
                            'candidate-message-id e-board-message-metadata-byte-limit)
     :outcome (e-board-processing-result-outcome record)
     :replacement-message-id (e-board--freeze-envelope-value
                              (e-board-processing-result-replacement-message-id record)
                              'replacement-message-id
                              e-board-message-metadata-byte-limit)
     :failure-policy (e-board-processing-result-failure-policy record)
     :failure (e-board--freeze-envelope-value
               (e-board-processing-result-failure record) 'processing-failure
               e-board-message-metadata-byte-limit)
     :created-at (e-board--freeze-envelope-value
                  (e-board-processing-result-created-at record) 'created-at
                  e-board-message-metadata-byte-limit)))
   (t (signal 'wrong-type-argument (list 'e-board-processing-record-p record)))))

(defun e-board--append-processing-record (board record type)
  "Persist immutable processing RECORD of TYPE before publishing it to BOARD.
A callback failure leaves the ledger unchanged.  Retrying the same record is
safe when the persistence adapter deduplicates its durable identity."
  (pcase-let* ((`(,id ,table ,reservations)
                (pcase type
                  ('processing-chain
                   (list (e-board-processing-chain-id record)
                         (e-board-processing-chain-table-internal board)
                         (e-board-processing-chain-reservations board)))
                  ('processing-result
                   (list (e-board-processing-result-id record)
                         (e-board-processing-result-table-internal board)
                         (e-board-processing-result-reservations board))))))
    (when (or (eq e-board--processing-persistence-board board)
              (gethash id table)
              (gethash id reservations))
      (signal 'e-board-id-conflict (list id)))
    (puthash id t reservations)
    (unwind-protect
        (progn
          ;; A nested record would publish before its outer durable predecessor.
          (unless e-board--processing-replay-p
            (when-let ((notify (e-board-processing-record-notification-function board)))
              (let ((e-board--processing-persistence-board board))
                (funcall notify board (e-board--copy-processing-record record) type))))
          (pcase type
            ('processing-chain
             (puthash id record table)
             (let ((cell (list record)))
               (if (e-board-processing-chains-tail-internal board)
                   (setcdr (e-board-processing-chains-tail-internal board) cell)
                 (setf (e-board-processing-chains-internal board) cell))
               (setf (e-board-processing-chains-tail-internal board) cell)))
            ('processing-result
             (puthash id record table)
             (let ((cell (list record)))
               (if (e-board-processing-results-tail-internal board)
                   (setcdr (e-board-processing-results-tail-internal board) cell)
                 (setf (e-board-processing-results-internal board) cell))
               (setf (e-board-processing-results-tail-internal board) cell))))
          (e-board-admission-append-event
           board type
           (list :id (e-board--freeze-envelope-value
                      id 'processing-record-id e-board-message-metadata-byte-limit)))
          record)
      (remhash id reservations))))

(cl-defun e-board-record-processing-chain
    (board &key id root-message-id candidate-message-id caused-by-message-id
           processor-history processing-depth created-at)
  "Append one immutable processing-chain record to BOARD's routing ledger."
  (unless (and root-message-id candidate-message-id)
    (signal 'wrong-type-argument
            (list 'processing-message-identities
                  (list root-message-id candidate-message-id))))
  (unless (and (integerp processing-depth)
               (>= processing-depth 0)
               (<= processing-depth e-board-processing-history-limit))
    (signal 'wrong-type-argument
            (list (list 'integer-range 0 e-board-processing-history-limit)
                  processing-depth)))
  (unless (and (listp processor-history)
               (<= (length processor-history) e-board-processing-history-limit))
    (signal 'wrong-type-argument
            (list 'bounded-processor-history processor-history)))
  (let ((id (or id (e-board--next-id board 'processing-chain))))
    (e-board--append-processing-record
     board
     (e-board-processing-chain--create
      :id (e-board--freeze-envelope-value id 'processing-chain-id
                                          e-board-message-metadata-byte-limit)
      :board-id (e-board--freeze-envelope-value (e-board-id board) 'board-id
                                                 e-board-message-metadata-byte-limit)
      :root-message-id
      (e-board--freeze-envelope-value root-message-id 'root-message-id
                                      e-board-message-metadata-byte-limit)
      :candidate-message-id
      (e-board--freeze-envelope-value candidate-message-id 'candidate-message-id
                                      e-board-message-metadata-byte-limit)
      :caused-by-message-id
      (e-board--freeze-envelope-value caused-by-message-id 'caused-by-message-id
                                      e-board-message-metadata-byte-limit)
      :processor-history
      (e-board--freeze-envelope-value processor-history 'processor-history
                                      e-board-message-metadata-byte-limit)
      :processing-depth processing-depth
      :created-at (e-board--freeze-envelope-value
                   (or created-at (float-time)) 'created-at
                   e-board-message-metadata-byte-limit))
     'processing-chain)
    (e-board--copy-processing-record
     (gethash id (e-board-processing-chain-table-internal board)))))

(cl-defun e-board-record-processing-result
    (board &key id chain-id subscription-id participant-id candidate-message-id
           outcome replacement-message-id failure-policy failure created-at)
  "Append one immutable terminal processing result to BOARD's routing ledger."
  (unless (and chain-id subscription-id participant-id candidate-message-id)
    (signal 'wrong-type-argument
            (list 'processing-result-identities
                  (list chain-id subscription-id participant-id candidate-message-id))))
  (unless (memq outcome '(pass replace consume fail))
    (signal 'wrong-type-argument (list '(member pass replace consume fail) outcome)))
  (when (eq outcome 'replace)
    (unless replacement-message-id
      (signal 'wrong-type-argument
              (list 'replacement-message-id replacement-message-id))))
  (unless (memq failure-policy '(pass consume))
    (signal 'wrong-type-argument
            (list '(member pass consume) failure-policy)))
  (let ((id (or id (e-board--next-id board 'processing-result))))
    (e-board--append-processing-record
     board
     (e-board-processing-result--create
      :id (e-board--freeze-envelope-value id 'processing-result-id
                                          e-board-message-metadata-byte-limit)
      :board-id (e-board--freeze-envelope-value (e-board-id board) 'board-id
                                                 e-board-message-metadata-byte-limit)
      :chain-id (e-board--freeze-envelope-value chain-id 'chain-id
                                                e-board-message-metadata-byte-limit)
      :subscription-id
      (e-board--freeze-envelope-value subscription-id 'subscription-id
                                      e-board-message-metadata-byte-limit)
      :participant-id
      (e-board--freeze-envelope-value participant-id 'participant-id
                                      e-board-message-metadata-byte-limit)
      :candidate-message-id
      (e-board--freeze-envelope-value candidate-message-id 'candidate-message-id
                                      e-board-message-metadata-byte-limit)
      :outcome outcome
      :replacement-message-id
      (e-board--freeze-envelope-value replacement-message-id 'replacement-message-id
                                      e-board-message-metadata-byte-limit)
      :failure-policy failure-policy
      :failure (e-board--freeze-envelope-value failure 'processing-failure
                                               e-board-message-metadata-byte-limit)
      :created-at (e-board--freeze-envelope-value
                   (or created-at (float-time)) 'created-at
                   e-board-message-metadata-byte-limit))
     'processing-result)
    (e-board--copy-processing-record
     (gethash id (e-board-processing-result-table-internal board)))))

(defun e-board-processing-chains (board)
  "Return detached copies of BOARD's processing-chain ledger in append order."
  (mapcar #'e-board--copy-processing-record
          (e-board-processing-chains-internal board)))

(defun e-board-processing-results (board)
  "Return detached copies of BOARD's processing-result ledger in append order."
  (mapcar #'e-board--copy-processing-record
          (e-board-processing-results-internal board)))

(defalias 'e-board-list-processing-chains #'e-board-processing-chains)
(defalias 'e-board-list-processing-results #'e-board-processing-results)

(defun e-board-processing-record-envelope (record)
  "Return a detached durable processing journal envelope for RECORD."
  (let ((record (e-board--copy-processing-record record)))
    (cond
     ((e-board-processing-chain-p record)
      (list :record-type 'processing-chain
            :id (e-board-processing-chain-id record)
            :root-message-id (e-board-processing-chain-root-message-id record)
            :candidate-message-id (e-board-processing-chain-candidate-message-id record)
            :caused-by-message-id (e-board-processing-chain-caused-by-message-id record)
            :processor-history (e-board-processing-chain-processor-history record)
            :processing-depth (e-board-processing-chain-processing-depth record)
            :created-at (e-board-processing-chain-created-at record)))
     ((e-board-processing-result-p record)
      (list :record-type 'processing-result
            :id (e-board-processing-result-id record)
            :chain-id (e-board-processing-result-chain-id record)
            :subscription-id (e-board-processing-result-subscription-id record)
            :participant-id (e-board-processing-result-participant-id record)
            :candidate-message-id (e-board-processing-result-candidate-message-id record)
            :outcome (e-board-processing-result-outcome record)
            :replacement-message-id
            (e-board-processing-result-replacement-message-id record)
            :failure-policy (e-board-processing-result-failure-policy record)
            :failure (e-board-processing-result-failure record)
            :created-at (e-board-processing-result-created-at record)))
     (t (signal 'wrong-type-argument (list 'e-board-processing-record-p record))))))

(defun e-board-import-processing-record (board envelope)
  "Restore one durable processing ENVELOPE into BOARD without re-notifying."
  (let ((e-board--processing-replay-p t))
    (pcase (plist-get envelope :record-type)
      ((or 'processing-chain "processing-chain")
       (e-board-record-processing-chain
        board :id (plist-get envelope :id)
        :root-message-id (plist-get envelope :root-message-id)
        :candidate-message-id (plist-get envelope :candidate-message-id)
        :caused-by-message-id (plist-get envelope :caused-by-message-id)
        :processor-history (plist-get envelope :processor-history)
        :processing-depth (plist-get envelope :processing-depth)
        :created-at (plist-get envelope :created-at)))
      ((or 'processing-result "processing-result")
       (e-board-record-processing-result
        board :id (plist-get envelope :id) :chain-id (plist-get envelope :chain-id)
        :subscription-id (plist-get envelope :subscription-id)
        :participant-id (plist-get envelope :participant-id)
        :candidate-message-id (plist-get envelope :candidate-message-id)
        :outcome (let ((outcome (plist-get envelope :outcome)))
                   (if (stringp outcome) (intern outcome) outcome))
        :replacement-message-id (plist-get envelope :replacement-message-id)
        :failure-policy (let ((policy (plist-get envelope :failure-policy)))
                          (if (stringp policy) (intern policy) policy))
        :failure (plist-get envelope :failure)
        :created-at (plist-get envelope :created-at)))
      (_ (signal 'e-board-error (list "Unknown processing record" envelope))))))

(defun e-board-advance-retention-floor (board floor)
  "Advance BOARD's logical retained-message floor to FLOOR.
The reducer never scans observers here.  A live observer whose cursor is now
behind this floor transitions to `expired' only when it next requests a page,
at which point that page requires a fresh snapshot."
  (unless (and (integerp floor) (>= floor 0))
    (signal 'wrong-type-argument (list 'natnump floor)))
  (when (> floor (e-board-next-seq board))
    (signal 'e-board-error (list "Retention floor exceeds board sequence" floor)))
  (when (< floor (e-board-retention-floor board))
    (signal 'e-board-error (list "Retention floor cannot move backwards" floor)))
  (when (> floor (e-board-retention-floor board))
    (setf (e-board-retention-floor board) floor)
    (e-board-admission-append-event board 'retention-floor-advanced (list :floor floor)))
  board)

(defun e-board-message (board message-id)
  "Return BOARD message MESSAGE-ID, or nil when it is not retained."
  (gethash message-id (e-board-message-table board)))

(defun e-board-message-envelope (message)
  "Return the frozen durable envelope for MESSAGE.

The envelope is the board-owned interchange value used when a board journal
is persisted or imported.  Callers receive detached values and do not need to
know the message struct's storage representation."
  (list :id (e-board-message-id message)
        :kind (e-board-message-kind message)
        :author (e-board-message-author message)
        :requester-actor (e-board-message-requester-actor message)
        :tags (copy-tree (e-board-message-tags message))
        :attributes (copy-tree (e-board-message-attributes message))
        :to (e-board-message-to message) :mode (e-board-message-mode message)
        :content (e-board-message-content message)
        :reference (copy-tree (e-board-message-reference message))
        :source-input-key (copy-tree (e-board-message-source-input-key message))
        :source-output-key (copy-tree (e-board-message-source-output-key message))
        :reply-to-message-ids
        (copy-tree (e-board-message-reply-to-message-ids message))
        :caused-by-delivery-ids
        (copy-tree (e-board-message-caused-by-delivery-ids message))
        :source-activity-key
        (copy-tree (e-board-message-source-activity-key message))
        :source-fact-key (copy-tree (e-board-message-source-fact-key message))
        :subject-participant-id (e-board-message-subject-participant-id message)
        :source-turn-id (e-board-message-source-turn-id message)
        :activity-kind (e-board-message-activity-kind message)
        :created-at (e-board-message-created-at message)
        :matching-participant-ids
        (copy-tree (e-board-message-matching-participant-ids message))
        :unrouted-reason (e-board-message-unrouted-reason message)
        :routing-state (e-board-message-routing-state message)))

(cl-defun e-board-observer-recent-messages
    (board observer-id &key kinds (limit 32) before-seq)
  "Return OBSERVER-ID's newest matching BOARD messages in ascending order.
KINDS narrows the snapshot to specific message kinds.  LIMIT bounds returned
matches independently of other message kinds, and BEFORE-SEQ excludes messages
at or after that sequence without advancing either observer cursor."
  (unless (and (listp kinds) kinds (cl-every #'symbolp kinds))
    (signal 'wrong-type-argument (list 'list-of-symbols-p kinds)))
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'plusp limit)))
  (when before-seq
    (unless (and (integerp before-seq) (>= before-seq 0))
      (signal 'wrong-type-argument (list 'natnump before-seq))))
  (let* ((observer (or (e-board-observer board observer-id)
                       (signal 'e-board-observer-missing (list observer-id))))
         (selector (e-board-observer-selector observer))
         (required-tags (or (plist-get selector :tags-all)
                            (plist-get selector :tags)))
         (index-tag (car required-tags))
         (streams
          (delq nil
                (mapcar
                 (lambda (kind)
                   (when-let ((messages
                               (gethash
                                (if index-tag (cons kind index-tag) kind)
                                (if index-tag
                                    (e-board-message-kind-tag-newest-table board)
                                  (e-board-message-kind-newest-table board)))))
                     (list messages)))
                 kinds)))
         (match-count 0)
         matches)
    ;; Each source list is newest-first.  Merge their heads so inspecting one
    ;; category never requires walking unrelated board-message kinds.
    (while (and streams (< match-count limit))
      (let ((newest-stream
             (car (sort (copy-sequence streams)
                        (lambda (left right)
                          (> (e-board-message-seq (caar left))
                             (e-board-message-seq (caar right))))))))
        (let ((message (pop (car newest-stream))))
          (unless (car newest-stream)
            (setq streams (delq newest-stream streams)))
          (when (and (or (not before-seq)
                         (< (e-board-message-seq message) before-seq))
                     (e-board--observer-matches-p board observer message))
            (cl-incf match-count)
            (push message matches)))))
    (nreverse matches)))

(defun e-board-participant (board participant-id)
  "Return BOARD participant PARTICIPANT-ID, or nil."
  (gethash participant-id (e-board-participants board)))

(defun e-board-pickup (board delivery-id)
  "Return BOARD pickup DELIVERY-ID, or nil."
  (gethash delivery-id (e-board-pickups board)))

(defun e-board-pickup-route-current-p (board pickup)
  "Return non-nil when PICKUP retains its exact current participant lifetime.
The route snapshot is process-local authority for deferred delivery: it keeps a
retired or same-id replacement participant from inheriting an already-routed
producer pickup.  A current participant without a runtime attachment remains a
valid waiting route and may be delivered after a later attachment is admitted."
  (when (and (e-board-p board)
             (e-board-pickup-p pickup))
    (let* ((participant (e-board-pickup-participant-lifetime pickup))
           (participant-id (and participant
                                (e-board-participant-id participant)))
           (current (and participant-id
                         (e-board-participant board participant-id))))
      (and (eq current participant)
           ;; During a controlled rebind the participant remains the exact
           ;; current lifetime.  Terminal retirement removes it from the
           ;; board map, so the identity check still fences old pickups while
           ;; already-accepted work can drain through rebind.
           (memq (e-board-participant-state participant)
                 '(active detaching dormant stale))))))

(defun e-board--pickup-queue (board participant-id)
  "Return PARTICIPANT-ID's ordered pickup identities on BOARD."
  (gethash participant-id (e-board-pickup-queues board)))

(defun e-board-pickup-ids (board participant-id)
  "Return a detached ordered pickup-id projection for PARTICIPANT-ID.

Runtime adapters use this consumer-shaped projection to choose the next
delivery.  The queue cons cells and their mutation remain Board-private."
  (copy-sequence (or (e-board--pickup-queue board participant-id) nil)))

(defun e-board--enqueue-pickup (board pickup)
  "Append PICKUP to its participant FIFO and return its initial state."
  (let* ((participant-id (e-board-pickup-participant-id pickup))
         (queue (e-board--pickup-queue board participant-id)))
    (if (and queue
             (>= (length (cdr queue)) (e-board-pickup-pending-limit board)))
        (progn
          (setf (e-board-pickup-state pickup) 'overflowed)
          (e-board-admission-append-event
           board 'pickup-overflowed
           (list :delivery-id (e-board-pickup-delivery-id pickup)
                 :pending-limit (e-board-pickup-pending-limit board))))
      (puthash participant-id
               (append queue (list (e-board-pickup-delivery-id pickup)))
               (e-board-pickup-queues board))
      (setf (e-board-pickup-state pickup) (if queue 'pending 'ready))
      (e-board-state-adjust-unsettled board 'pickups 1))
    (e-board-pickup-state pickup)))

(defun e-board--set-pickup-attempt-state (pickup state &optional reason)
  "Move PICKUP's physical attempt to STATE and optionally retain REASON."
  (when-let ((attempt (e-board-pickup-attempt pickup)))
    (setf (e-board-delivery-attempt-state attempt) state)
    (when reason
      (setf (e-board-delivery-attempt-reason attempt)
            (e-board--copy-envelope-value reason))))
  pickup)

(defun e-board-pickup-start-delivery
    (board delivery-id &optional endpoint-token composite-generation)
  "Bind and fence ready DELIVERY-ID as one physical delivery attempt.
ENDPOINT-TOKEN is opaque to the board.  COMPOSITE-GENERATION identifies the
selected logical-instance and concrete-harness generations.  A later attempt
may replace this binding only after the previous call was proven uncommitted."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (and (eq (e-board-pickup-state pickup) 'ready)
                 (equal (car (e-board--pickup-queue
                              board (e-board-pickup-participant-id pickup)))
                        delivery-id))
      (signal 'e-board-error (list "Pickup is not ready head" delivery-id)))
    (let* ((previous (e-board-pickup-attempt pickup))
           (number (if previous
                       (1+ (e-board-delivery-attempt-number previous))
                     1)))
      (when (and previous
                 (not (eq (e-board-delivery-attempt-state previous)
                          'proven-uncommitted)))
        (signal 'e-board-error
                (list "Pickup attempt is not replaceable" delivery-id)))
      (setf (e-board-pickup-attempt pickup)
            (e-board-delivery-attempt--create
             :number number
             :endpoint-token (e-board--copy-envelope-value endpoint-token)
             :composite-generation
             (e-board--copy-envelope-value composite-generation)
             :state 'delivering)))
    (setf (e-board-pickup-state pickup) 'delivering)
    (e-board-admission-append-event board 'pickup-delivering
                           (list :delivery-id delivery-id
                                 :attempt-number
                                 (e-board-delivery-attempt-number
                                  (e-board-pickup-attempt pickup))
                                 :endpoint-token
                                 (e-board--copy-envelope-value endpoint-token)
                                 :composite-generation
                                 (e-board--copy-envelope-value
                                  composite-generation)))
    pickup))

(defun e-board-pickup-complete-delivery (board delivery-id)
  "Consume DELIVERY-ID and promote its participant's next FIFO pickup.
Return the newly ready pickup identity, if any."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(delivering accepted cancelling))
      (signal 'e-board-error
              (list "Pickup is not delivering, accepted, or cancelling" delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id)))
      (unless (equal (car queue) delivery-id)
        (signal 'e-board-error (list "Pickup lost FIFO ownership" delivery-id)))
      (setf (e-board-pickup-state pickup) 'consumed)
      (e-board-state-adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state pickup 'consumed)
      (e-board-admission-append-event board 'pickup-consumed
                             (list :delivery-id delivery-id))
      (setq queue (cdr queue))
      (puthash participant-id queue (e-board-pickup-queues board))
      (when-let ((next-id (car queue)))
        (let ((next (e-board-pickup board next-id)))
          (setf (e-board-pickup-state next) 'ready)
          (e-board-admission-append-event board 'pickup-ready
                                 (list :delivery-id next-id))
          next-id)))))

(defun e-board-pickup-accept-delivery (board delivery-id &optional receipt)
  "Record DELIVERY-ID as harness-owned with optional acceptance RECEIPT."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (eq (e-board-pickup-state pickup) 'delivering)
      (signal 'e-board-error (list "Pickup is not delivering" delivery-id)))
    (setf (e-board-pickup-state pickup) 'accepted)
    (e-board--set-pickup-attempt-state pickup 'accepted)
    (when receipt
      (setf (e-board-delivery-attempt-receipt (e-board-pickup-attempt pickup))
            (e-board--copy-envelope-value receipt)))
    (e-board-admission-append-event board 'pickup-accepted (list :delivery-id delivery-id))
    pickup))

(defun e-board-pickup-discard-delivery (board delivery-id reason)
  "Record accepted DELIVERY-ID as discarded and promote its FIFO successor."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(accepted cancelling))
      (signal 'e-board-error (list "Pickup is not accepted or cancelling" delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id))
           (cancelled-p (eq (e-board-pickup-state pickup) 'cancelling)))
      (unless (equal (car queue) delivery-id)
        (signal 'e-board-error (list "Pickup lost FIFO ownership" delivery-id)))
      (setf (e-board-pickup-state pickup) (if cancelled-p 'cancelled 'discarded))
      (e-board-state-adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state
       pickup (if cancelled-p 'cancelled 'discarded) reason)
      (e-board-admission-append-event board (if cancelled-p 'pickup-cancelled 'pickup-discarded)
                             (list :delivery-id delivery-id :reason reason))
      (setq queue (cdr queue))
      (puthash participant-id queue (e-board-pickup-queues board))
      (when-let ((next-id (car queue)))
        (let ((next (e-board-pickup board next-id)))
          (setf (e-board-pickup-state next) 'ready)
          (e-board-admission-append-event board 'pickup-ready
                                 (list :delivery-id next-id))
          next-id)))))

(defun e-board-pickup-mark-uncertain (board delivery-id reason)
  "Tombstone ambiguous DELIVERY-ID and promote its FIFO successor.
An uncertain physical attempt is never retried as though it had not reached the
original endpoint.  REASON records the reconciliation gap for later inspection."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(delivering accepted cancelling))
      (signal 'e-board-error
              (list "Pickup uncertainty requires delivering or accepted state" delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id)))
      (unless (equal (car queue) delivery-id)
        (signal 'e-board-error (list "Pickup lost FIFO ownership" delivery-id)))
      (setf (e-board-pickup-state pickup) 'uncertain)
      (e-board-state-adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state pickup 'uncertain reason)
      (e-board-admission-append-event board 'pickup-uncertain
                             (list :delivery-id delivery-id :reason reason))
      (setq queue (cdr queue))
      (puthash participant-id queue (e-board-pickup-queues board))
      (when-let ((next-id (car queue)))
        (let ((next (e-board-pickup board next-id)))
          (setf (e-board-pickup-state next) 'ready)
          (e-board-admission-append-event board 'pickup-ready
                                 (list :delivery-id next-id))
          next-id)))))

(defun e-board-cancel-pickup (board delivery-id &optional reason)
  "Cancel DELIVERY-ID without affecting watched work.
Pending and ready pickups become terminal immediately.  In-flight and accepted
pickups become `cancelling' until a consumption or discard receipt resolves the
same immutable delivery id.  Return a newly ready successor only when this
call releases the FIFO head."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup)
                  '(pending ready delivering accepted))
      (signal 'e-board-error
              (list "Pickup cancellation requires a nonterminal state" delivery-id)))
    (if (memq (e-board-pickup-state pickup) '(delivering accepted))
        (progn
          (setf (e-board-pickup-state pickup) 'cancelling)
          (e-board--set-pickup-attempt-state pickup 'cancelling reason)
          (e-board-admission-append-event board 'pickup-cancelling
                                 (list :delivery-id delivery-id :reason reason))
          nil)
      (let* ((participant-id (e-board-pickup-participant-id pickup))
             (queue (e-board--pickup-queue board participant-id))
             (head-p (equal (car queue) delivery-id)))
        (setf (e-board-pickup-state pickup) 'cancelled)
        (e-board-state-adjust-unsettled board 'pickups -1)
        (e-board--set-pickup-attempt-state pickup 'cancelled reason)
        (puthash participant-id (delete delivery-id queue) (e-board-pickup-queues board))
        (e-board-admission-append-event board 'pickup-cancelled
                               (list :delivery-id delivery-id :reason reason))
        (when head-p
          (when-let ((next-id (car (e-board--pickup-queue board participant-id))))
            (let ((next (e-board-pickup board next-id)))
              (setf (e-board-pickup-state next) 'ready)
              (e-board-admission-append-event board 'pickup-ready
                                     (list :delivery-id next-id))
              next-id)))))))

(defun e-board-expire-pickup (board delivery-id &optional reason)
  "Expire pending or ready DELIVERY-ID and release its FIFO successor.
Expiry is a visible terminal tombstone.  It never retries or silently drops a
stalled logical pickup, and an expired head releases only that participant's
next FIFO record."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(pending ready))
      (signal 'e-board-error
              (list "Pickup expiry requires pending or ready state" delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id))
           (head-p (equal (car queue) delivery-id)))
      (setf (e-board-pickup-state pickup) 'expired)
      (e-board-state-adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state pickup 'expired reason)
      (puthash participant-id (delete delivery-id queue) (e-board-pickup-queues board))
      (e-board-admission-append-event board 'pickup-expired
                             (list :delivery-id delivery-id :reason reason))
      (when head-p
        (when-let ((next-id (car (e-board--pickup-queue board participant-id))))
          (let ((next (e-board-pickup board next-id)))
            (setf (e-board-pickup-state next) 'ready)
            (e-board-admission-append-event board 'pickup-ready
                                   (list :delivery-id next-id))
            next-id))))))

(defun e-board-fail-pickup (board delivery-id reason)
  "Record a permanent failure for DELIVERY-ID and release its FIFO successor.
The delivery adapter may use this only when it proves the logical pickup cannot
be delivered through the selected endpoint.  A transient uncommitted failure
uses `e-board-pickup-return-ready' instead."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(pending ready delivering))
      (signal 'e-board-error
              (list "Pickup failure requires pending, ready, or delivering state"
                    delivery-id)))
    (let* ((participant-id (e-board-pickup-participant-id pickup))
           (queue (e-board--pickup-queue board participant-id))
           (head-p (equal (car queue) delivery-id)))
      (setf (e-board-pickup-state pickup) 'failed)
      (e-board-state-adjust-unsettled board 'pickups -1)
      (e-board--set-pickup-attempt-state pickup 'failed reason)
      (puthash participant-id (delete delivery-id queue) (e-board-pickup-queues board))
      (e-board-admission-append-event board 'pickup-failed
                             (list :delivery-id delivery-id :reason reason))
      (when head-p
        (when-let ((next-id (car (e-board--pickup-queue board participant-id))))
          (let ((next (e-board-pickup board next-id)))
            (setf (e-board-pickup-state next) 'ready)
            (e-board-admission-append-event board 'pickup-ready
                                   (list :delivery-id next-id))
            next-id))))))

(defun e-board-pickup-return-ready (board delivery-id err)
  "Return uncommitted delivering DELIVERY-ID to its FIFO head after ERR.
When a cancellation already fenced the in-flight attempt, a proven
uncommitted failure settles that cancellation instead of retrying it."
  (let ((pickup (or (e-board-pickup board delivery-id)
                    (signal 'e-board-error (list "Unknown pickup" delivery-id)))))
    (unless (memq (e-board-pickup-state pickup) '(delivering cancelling))
      (signal 'e-board-error (list "Pickup is not delivering or cancelling" delivery-id)))
    (if (eq (e-board-pickup-state pickup) 'cancelling)
        (let* ((participant-id (e-board-pickup-participant-id pickup))
               (queue (e-board--pickup-queue board participant-id)))
          (unless (equal (car queue) delivery-id)
            (signal 'e-board-error (list "Pickup lost FIFO ownership" delivery-id)))
          (setf (e-board-pickup-state pickup) 'cancelled)
          (e-board-state-adjust-unsettled board 'pickups -1)
          (e-board--set-pickup-attempt-state pickup 'cancelled err)
          (e-board-admission-append-event board 'pickup-cancelled
                                 (list :delivery-id delivery-id :reason err))
          (setq queue (cdr queue))
          (puthash participant-id queue (e-board-pickup-queues board))
          (when-let ((next-id (car queue)))
            (let ((next (e-board-pickup board next-id)))
              (setf (e-board-pickup-state next) 'ready)
              (e-board-admission-append-event board 'pickup-ready
                                     (list :delivery-id next-id))
              next-id)))
      (setf (e-board-pickup-state pickup) 'ready)
      (e-board--set-pickup-attempt-state pickup 'proven-uncommitted)
      (e-board-admission-append-event board 'pickup-delivery-failed
                             (list :delivery-id delivery-id :error err))
      pickup)))

(defun e-board-observed-work (board work-id)
  "Return BOARD's observed work record for WORK-ID, or nil."
  (gethash work-id (e-board-work-table board)))

(defun e-board-invocation (board invocation-id)
  "Return BOARD's exact invocation relation for INVOCATION-ID, or nil."
  (gethash invocation-id (e-board-invocations board)))

(defun e-board-aggregation (board aggregation-id)
  "Return BOARD's aggregation subscription for AGGREGATION-ID, or nil."
  (gethash aggregation-id (e-board-aggregations board)))

(defun e-board-activation (board activation-id)
  "Return BOARD's frozen effect activation for ACTIVATION-ID, or nil."
  (gethash activation-id (e-board-activations board)))

(defun e-board--open-activity-key (participant-id turn-id)
  "Return the board-owned open-activity projection key."
  (list participant-id turn-id))

(defun e-board-open-activity (board participant-id turn-id)
  "Return BOARD's current open activity for PARTICIPANT-ID and TURN-ID."
  (gethash (e-board--open-activity-key participant-id turn-id)
           (e-board-open-activities board)))

(defun e-board--terminal-activity-kind-p (activity-kind)
  "Return non-nil when ACTIVITY-KIND closes a visible turn projection."
  (memq activity-kind '(turn-finished turn-failed turn-cancelled turn-summary)))

(defun e-board--record-open-activity (board message)
  "Update BOARD's derived open-activity projection from activity MESSAGE."
  (let* ((participant-id (e-board-message-subject-participant-id message))
         (turn-id (e-board-message-source-turn-id message))
         (key (e-board--open-activity-key participant-id turn-id)))
    (if (e-board--terminal-activity-kind-p (e-board-message-activity-kind message))
        (e-board--close-open-activity board participant-id turn-id message)
      (unless (gethash key (e-board-closed-activities board))
        (puthash key
                 (e-board-open-activity--create
                  :participant-id participant-id :turn-id turn-id
                  :message-id (e-board-message-id message)
                  :seq (e-board-message-seq message)
                  :activity-kind (e-board-message-activity-kind message))
                 (e-board-open-activities board))))))

(defun e-board--close-open-activity (board participant-id turn-id message)
  "Close BOARD's activity projection for PARTICIPANT-ID and TURN-ID at MESSAGE."
  (when (and participant-id turn-id)
    (let ((key (e-board--open-activity-key participant-id turn-id)))
      (remhash key (e-board-open-activities board))
      (puthash key (e-board-message-id message) (e-board-closed-activities board)))))

(defun e-board-observer (board observer-id)
  "Return BOARD's client observer cursor OBSERVER-ID, or nil."
  (gethash observer-id (e-board-observers board)))

(defun e-board--schedule-terminal-classification (board)
  "Schedule BOARD's bounded terminal classifier once after settlement returns.

The admission owner owns the exact queue and receipt.  This small facade
operation owns only the policy-specific scheduler injection, keeping the
  lower owner independent of Board's semantic reducer."
  (unless (e-board-terminal-classification-scheduled board)
    (cl-incf (e-board-terminal-classification-generation board))
    (let ((generation (e-board-terminal-classification-generation board)))
      (setf (e-board-terminal-classification-callback-generation board)
            generation
            (e-board-terminal-classification-scheduled board) t)
      (condition-case err
          (if-let ((scheduler (e-board-terminal-classification-scheduler board)))
              (funcall scheduler
                       (lambda ()
                         (when (= generation
                                  (e-board-terminal-classification-callback-generation
                                   board))
                           (e-board-drain-terminal-classifications board))))
            (run-at-time 0 nil
                         (lambda ()
                           (when (= generation
                                    (e-board-terminal-classification-callback-generation
                                     board))
                             (e-board-drain-terminal-classifications board)))))
        (error
         ;; A scheduler can retain a callback and still signal.  Clear the
         ;; scheduled authority and advance the generation before surfacing the
         ;; scheduler error, so that callback cannot drain a later replacement.
         (setf (e-board-terminal-classification-scheduled board) nil)
         (cl-incf (e-board-terminal-classification-generation board))
         (setf (e-board-terminal-classification-callback-generation board)
               (e-board-terminal-classification-generation board))
         (signal (car err) (cdr err)))))))

(defun e-board--schedule-aggregation-deadline (board)
  "Schedule BOARD's bounded aggregation deadline reducer once."
  (unless (e-board-aggregation-deadline-scheduled board)
    (cl-incf (e-board-aggregation-deadline-generation board))
    (let ((generation (e-board-aggregation-deadline-generation board)))
      (setf (e-board-aggregation-deadline-callback-generation board)
            generation
            (e-board-aggregation-deadline-scheduled board) t)
      (condition-case err
          (if-let ((scheduler (e-board-aggregation-deadline-scheduler board)))
              (funcall scheduler
                       (lambda ()
                         (when (= generation
                                  (e-board-aggregation-deadline-callback-generation
                                   board))
                           (e-board-drain-aggregation-deadlines board))))
            (run-at-time 0 nil
                         (lambda ()
                           (when (= generation
                                    (e-board-aggregation-deadline-callback-generation
                                     board))
                             (e-board-drain-aggregation-deadlines board)))))
        (error
         ;; Keep queue ownership while fencing a callback that may have been
         ;; accepted before this scheduler reported failure.  A later enqueue
         ;; publishes a fresh generation and can retry the surviving queue.
         (setf (e-board-aggregation-deadline-scheduled board) nil)
         (cl-incf (e-board-aggregation-deadline-generation board))
         (setf (e-board-aggregation-deadline-callback-generation board)
               (e-board-aggregation-deadline-generation board))
         (signal (car err) (cdr err)))))))

(defun e-board-drain-aggregation-deadlines (board)
  "Commit a bounded page of exact aggregation deadline receipts.

Every production queue cell has an admission-owner receipt.  A missing receipt
is an invariant failure, not a reason to mutate a queue by descriptive id."
  (setf (e-board-aggregation-deadline-scheduled board) nil)
  (let ((remaining e-board-aggregation-deadline-drain-limit)
        (first-error nil))
    (while (and (> remaining 0)
                (e-board-aggregation-deadlines board))
      (let* ((cell (e-board-aggregation-deadlines board))
             (receipt (gethash cell
                              (e-board-aggregation-deadline-node-index board)))
             (aggregation (and receipt
                               (e-board-admission-deadline-aggregation receipt))))
        (unless (e-board-aggregation-deadline-receipt-p receipt)
          (signal 'e-board-error
                  (list "Aggregation deadline has no exact receipt" cell)))
        (let ((removed-p nil))
          ;; A post-count inverse fault still leaves the exact receipt removed;
          ;; retain that error but finish this semantic deadline exactly once.
          ;; A pre-count fault restores the head and stops this page so the
          ;; receipt remains the authority for a later retry.
          (condition-case err
              (progn
                (e-board-admission-remove-aggregation-deadline receipt)
                (setq removed-p t))
            (error
             (setq first-error (or first-error err)
                   removed-p
                   (e-board-aggregation-deadline-receipt--removed-p receipt))))
          (if (not removed-p)
              (setq remaining 0)
            (condition-case err
                (when (and (e-board-aggregation-p aggregation)
                           (eq (gethash (e-board-aggregation-id aggregation)
                                        (e-board-aggregations board))
                               aggregation)
                           (eq (e-board-aggregation-state aggregation) 'open))
                  (e-board--settle-aggregation board aggregation 'timed-out))
              (error
               (setq first-error (or first-error err))))
            (cl-decf remaining)))))
    (when (e-board-aggregation-deadlines board)
      (condition-case err
          (e-board--schedule-aggregation-deadline board)
        (error (setq first-error (or first-error err)))))
    (when first-error
      (signal (car first-error) (cdr first-error)))))

(defun e-board-drain-effects (board &optional generation)
  "Apply a bounded page of exact effect receipts in publication order."
  (e-board-admission-drain-effects board generation))

(defun e-board-drain-terminal-classifications (board)
  "Classify one bounded page of exact terminal subscription candidates.

The queue stores frozen object values.  The drain never re-resolves a
replacement invocation or aggregation by id; stale objects simply fail the
current identity check and the receipt still advances the queue."
  (setf (e-board-terminal-classification-scheduled board) nil)
  (let ((remaining e-board-terminal-classification-drain-limit)
        (first-error nil))
    (while (and (> remaining 0)
                (e-board-terminal-classifications board))
      (let* ((cell (e-board-terminal-classifications board))
             (record (car cell))
             (receipt (gethash cell
                              (e-board-terminal-classification-node-index board)))
             (work (e-board-terminal-classification-work record)))
        (unless (e-board-terminal-classification-receipt-p receipt)
          (signal 'e-board-error
                  (list "Terminal classifier has no exact receipt" cell)))
        (let ((state (and (eq (gethash (e-board-work-id work)
                                      (e-board-work-table board))
                              work)
                          (e-board-work-state work)))
              (payload (and (eq (gethash (e-board-work-id work)
                                         (e-board-work-table board))
                                 work)
                             (e-board-work-terminal-payload work))))
          ;; Keep each frozen object in the record until its semantic owner has
          ;; returned.  If that owner signals after a visible mutation, the
          ;; next retry observes its terminal/prepared state and consumes this
          ;; same object; no id lookup or replacement can be substituted.
          (when-let ((invocations
                      (e-board-terminal-classification-invocation-objects record)))
            (unless first-error
              (let ((invocation (car invocations)))
                (if (and (e-board-invocation-p invocation)
                         (eq (gethash (e-board-invocation-id invocation)
                                      (e-board-invocations board))
                             invocation))
                    (condition-case err
                        (progn
                          (e-board--settle-invocation board invocation state payload)
                          (setf
                           (e-board-terminal-classification-invocation-objects
                            record)
                           (cdr invocations)))
                      (error
                       (setq first-error err remaining 0)))
                  (setf (e-board-terminal-classification-invocation-objects record)
                        (cdr invocations))))))
          (when-let ((aggregations
                      (e-board-terminal-classification-aggregation-objects record)))
            (unless first-error
              (let ((aggregation (car aggregations)))
                (if (and (e-board-aggregation-p aggregation)
                         (eq (gethash (e-board-aggregation-id aggregation)
                                      (e-board-aggregations board))
                             aggregation)
                         (eq (e-board-aggregation-state aggregation) 'open)
                         (e-board--aggregation-ready-p board aggregation))
                    (condition-case err
                        (progn
                          (e-board--settle-aggregation board aggregation 'complete)
                          (setf
                           (e-board-terminal-classification-aggregation-objects
                            record)
                           (cdr aggregations)))
                      (error
                       (setq first-error err remaining 0)))
                  (setf (e-board-terminal-classification-aggregation-objects record)
                        (cdr aggregations))))))
        ;; A record is complete only when both frozen object queues are empty.
        ;; Keep the exact receipt at the head while semantic work is pending.
        (when (and (null (e-board-terminal-classification-invocation-objects record))
                   (null (e-board-terminal-classification-aggregation-objects record))
                   (not first-error))
          (let ((removed-p nil))
            (condition-case err
                (progn
                  (e-board-admission-remove-terminal-classification receipt)
                  (setq removed-p t))
              (error
               (setq first-error (or first-error err)
                     removed-p
                     (e-board-terminal-classification-receipt--removed-p receipt))))
            (unless removed-p
              (setq remaining 0))))
        (unless first-error
          (cl-decf remaining))))
    (when (e-board-terminal-classifications board)
      (condition-case err
          (e-board--schedule-terminal-classification board)
        (error (setq first-error (or first-error err)))))
    (when first-error
      (signal (car first-error) (cdr first-error))))))

(defun e-board--settle-invocation (board invocation state payload)
  "Commit INVOCATION's exact reply effect for terminal STATE and PAYLOAD."
  (when (eq (e-board-invocation-state invocation) 'open)
    (setf (e-board-invocation-state invocation) 'prepared)
    (let ((activation-id
           (list (e-board-id board) (e-board-invocation-id invocation) 1))
          (activation nil))
      (setf (e-board-invocation-activation-id invocation) activation-id)
      (setq activation
            (e-board-activation--create
             :id activation-id
             :subscription-id (e-board-invocation-id invocation)
             :message-id (e-board-invocation-work-id invocation)
             :effect 'reply-to-invocation :state 'prepared))
      (puthash activation-id activation (e-board-activations board))
      (e-board-admission-append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :work-id (e-board-invocation-work-id invocation)
             :effect 'reply-to-invocation))
      (e-board-admission-schedule-effect
       board
       (lambda ()
         (when (and (eq (e-board-invocation-state invocation) 'prepared)
                    (eq (e-board-activation-state activation) 'prepared))
           (setf (e-board-invocation-state invocation) 'applying)
           (setf (e-board-activation-state activation) 'applying)
           (e-board-admission-append-event
            board 'activation-applying (list :activation-id activation-id))
           (condition-case err
               (progn
                 (let ((dispatcher (e-board-invocation-effect-dispatcher board)))
                   (unless dispatcher
                     (signal 'e-board-error
                             (list "No invocation effect dispatcher" activation-id)))
                   (funcall dispatcher board
                            (e-board-invocation-effect-target invocation)
                            state payload))
                 (setf (e-board-invocation-state invocation) 'committed)
                 (setf (e-board-activation-state activation) 'committed)
                 (e-board-admission-append-event
                  board 'effect-committed
                  (list :activation-id activation-id
                        :effect 'reply-to-invocation)))
             (error
              (setf (e-board-invocation-state invocation) 'failed)
              (setf (e-board-activation-state activation) 'failed)
              (e-board-admission-append-event
               board 'effect-failed
                (list :activation-id activation-id :error err))))))))))

(defun e-board--aggregation-ready-p (board aggregation)
  "Return non-nil when AGGREGATION's observed work has reached its policy."
  (let ((work-ids (e-board-aggregation-work-ids aggregation)))
    (pcase (e-board-aggregation-mode aggregation)
      ((or 'all 'all-terminal)
       (cl-every (lambda (id)
                   (e-board-work-terminal-seq (e-board-observed-work board id)))
                 work-ids))
      ((or 'any 'first-terminal)
       (cl-some (lambda (id)
                  (e-board-work-terminal-seq (e-board-observed-work board id)))
                work-ids))
      ('on-success
       (eq (e-board-work-state (e-board-observed-work board (car work-ids)))
           'finished))
      ('on-failure
       (eq (e-board-work-state (e-board-observed-work board (car work-ids)))
           'failed))
      ('on-terminal
       (e-board-work-terminal-seq
        (e-board-observed-work board (car work-ids))))
      (_ (signal 'e-board-error
                 (list "Unknown aggregation mode" (e-board-aggregation-mode aggregation)))))))

(defun e-board--settle-aggregation (board aggregation reason &optional admission)
  "Commit AGGREGATION's deferred reply effect with terminal REASON.

When ADMISSION is still being staged, the activation publication is part of its
exact event receipt set.  The activation itself remains fenced by the same
admission if a later stage rejects the operation; normal committed drains pass
no admission and retain their existing asynchronous effect behavior."
  (when (eq (e-board-aggregation-state aggregation) 'open)
    (setf (e-board-aggregation-state aggregation) 'prepared)
    (when-let ((timer (e-board-aggregation-timer aggregation)))
      (cancel-timer timer)
      (setf (e-board-aggregation-timer aggregation) nil))
    (let ((activation-id
           (list (e-board-id board) (e-board-aggregation-id aggregation) 1))
          (activation nil))
      (setf (e-board-aggregation-activation-id aggregation) activation-id)
      (setq activation
            (e-board-activation--create
             :id activation-id
             :subscription-id (e-board-aggregation-id aggregation)
             :message-id nil :effect 'reply-to-invocation :state 'prepared))
      (when-let ((admission (e-board-aggregation-admission aggregation)))
        (setf (e-board-aggregation-admission-activation admission) activation))
      (puthash activation-id activation (e-board-activations board))
      (e-board-admission-append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :work-ids (copy-sequence (e-board-aggregation-work-ids aggregation))
             :effect 'reply-to-invocation)
       admission)
      ;; Keep the supplied admission as an explicit argument to the effect
      ;; scheduler.  In particular, do not place ADMISSION inside the effect
      ;; closure: a ready aggregation can re-enter Board while the closure is
      ;; being built, and the enclosing transaction must still own both the
      ;; activation event and the queued effect.
      (let ((effect
             (lambda ()
               (when (and (eq (e-board-aggregation-state aggregation) 'prepared)
                          (eq (e-board-activation-state activation) 'prepared))
                 (setf (e-board-aggregation-state aggregation) 'applying)
                 (setf (e-board-activation-state activation) 'applying)
                 (e-board-admission-append-event
                  board 'activation-applying (list :activation-id activation-id))
                 (condition-case err
                     (progn
                       (let ((dispatcher
                              (e-board-invocation-effect-dispatcher board)))
                         (unless dispatcher
                           (signal 'e-board-error
                                   (list "No invocation effect dispatcher"
                                         activation-id)))
                         (funcall dispatcher board
                                  (e-board-aggregation-effect-target aggregation)
                                  'aggregation reason))
                       (setf (e-board-aggregation-state aggregation) 'committed)
                       (setf (e-board-activation-state activation) 'committed)
                       (e-board-admission-append-event
                        board 'effect-committed
                        (list :activation-id activation-id
                              :effect 'reply-to-invocation)))
                   (error
                    (setf (e-board-aggregation-state aggregation) 'failed)
                    (setf (e-board-activation-state activation) 'failed)
                    (e-board-admission-append-event
                     board 'effect-failed
                     (list :activation-id activation-id :error err))))))))
        (e-board-admission-schedule-effect board effect admission)))))

(cl-defun e-board-subscribe-aggregation
    (board work-ids mode effect-target &key id timeout admission)
  "Install an ordered work aggregation reply subscription on BOARD.
WORK-IDS must name currently observed work.  MODE is `all'/`all-terminal',
`any'/`first-terminal', `on-success', `on-failure', or `on-terminal'.
EFFECT-TARGET remains opaque to the board and receives a later frozen reason
through the injected invocation effect dispatcher."
  (unless (listp work-ids)
    (signal 'e-board-error (list "Aggregation work ids must be a list")))
  (e-board-admission-require-clear board)
  (unless (memq mode e-board--aggregation-modes)
    (signal 'e-board-error (list "Unknown aggregation mode" mode)))
  (when (and (memq mode '(any first-terminal on-success on-failure on-terminal))
             (null work-ids))
    (signal 'e-board-error (list "Aggregation requires at least one work id")))
  (when (and (memq mode '(on-success on-failure on-terminal))
             (/= (length work-ids) 1))
    (signal 'e-board-error
            (list "Exact readiness requires one work id" mode work-ids)))
  (unless effect-target
    (signal 'e-board-error (list "Aggregation effect target is required")))
  (dolist (work-id work-ids)
    (unless (e-board-observed-work board work-id)
      (signal 'e-board-error (list "Unknown board work" work-id))))
  (let* ((supplied-admission-p (e-board-aggregation-admission-p admission))
         (admission (or admission
                        (e-board-aggregation-admission-token board)))
         (id (or id (e-board--next-id board 'invocation))))
    (unless (e-board-aggregation-admission-p admission)
      (signal 'wrong-type-argument
              (list 'e-board-aggregation-admission-p admission)))
    (when (and (e-board-aggregation-admission-board admission)
               (not (eq (e-board-aggregation-admission-board admission) board)))
      (signal 'e-board-error
              (list "Aggregation admission belongs to another board")))
    (when (e-board-aggregation board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((aggregation (e-board-aggregation--create
                        :id id :work-ids (copy-sequence work-ids) :mode mode
                        :state 'open :effect-target effect-target
                        :admission admission)))
      (setf (e-board-aggregation-admission-board admission) board
            (e-board-aggregation-admission-aggregation admission) aggregation)
      ;; The board owns the exact token even when a caller supplied it.  Begin
      ;; is idempotent and gives reentrant recovery a visible in-flight frame.
      (e-board-admission-begin board admission)
      (condition-case err
          (progn
            ;; The exact aggregation object is installed before any index or
            ;; event operation can reenter.  Its admission captures every
            ;; lower-owner receipt created below.
            (puthash id aggregation (e-board-aggregations board))
            (setf (e-board-aggregation-admission-map-installed-p admission) t)
            (dolist (work-id work-ids)
              (e-board-admission-index-work
               (e-board-aggregation-work-index board) work-id id admission))
            (e-board-admission-append-event
             board 'subscription-added
             (list :subscription-id id :work-ids (copy-sequence work-ids)
                   :readiness (pcase mode
                                ('all 'all-terminal)
                                ('any 'first-terminal)
                                (_ mode))
                   :effect 'reply-to-invocation)
             admission)
            (when timeout
              (let ((timer
                     (run-at-time timeout nil
                                  (lambda ()
                                    (when (and
                                           (eq (gethash id
                                                        (e-board-aggregations board))
                                               aggregation)
                                           (not (memq
                                                 (e-board-aggregation-state
                                                  aggregation)
                                                 '(cancelled failed))))
                                      (e-board-admission-queue-aggregation-deadline
                                       board aggregation admission
                                       #'e-board--schedule-aggregation-deadline))))))
                (setf (e-board-aggregation-timer aggregation) timer
                      (e-board-aggregation-admission-timer admission) timer)))
            (when (e-board--aggregation-ready-p board aggregation)
              ;; Keep an already-ready subscription on the same later
              ;; classifier path as a fresh terminal publication, except an
              ;; empty all-terminal set which has no source terminal record.
              (if work-ids
                  (e-board-admission-queue-terminal-classification
                   board (e-board-observed-work board (car work-ids))
                   nil (list id) (list aggregation)
                   admission #'e-board--schedule-terminal-classification)
                (e-board--settle-aggregation board aggregation 'complete admission)))
            (setf (e-board-aggregation-admission-committed-p admission) t)
            (unless (e-board-admission-current-p admission)
              (signal 'e-board-admission-pending
                      (list "Aggregation admission lost authority" admission)))
            (e-board-admission-finish board admission)
            (unless supplied-admission-p
              (e-board-admission-complete board admission))
            aggregation)
      (error
       (e-board-admission-finish board admission)
       (condition-case _cleanup-error
           (e-board-abort-aggregation-admission board admission)
         (error nil))
         ;; The initiating board error remains authoritative; the exact
         ;; admission stays in the board/runtime catalog if inverse cleanup
         ;; could not finish.
         (signal (car err) (cdr err)))))))

(defun e-board-cancel-aggregation (board aggregation-id)
  "Cancel open or prepared AGGREGATION-ID without affecting watched work.
A prepared reply activation is fenced before its later effect callback can
reach the runtime; an already-applying effect remains outside this local
cancellation boundary because its commit is no longer provably absent."
  (when-let ((aggregation (e-board-aggregation board aggregation-id)))
    (when (memq (e-board-aggregation-state aggregation) '(open prepared))
      (when-let ((timer (e-board-aggregation-timer aggregation)))
        (cancel-timer timer))
      (when (eq (e-board-aggregation-state aggregation) 'prepared)
        (when-let ((activation
                    (e-board-activation board
                                        (e-board-aggregation-activation-id aggregation))))
          (when (eq (e-board-activation-state activation) 'prepared)
            (setf (e-board-activation-state activation) 'cancelled)
            (e-board-admission-append-event
             board 'activation-cancelled
             (list :activation-id (e-board-activation-id activation)
                   :reason 'aggregation-cancelled)))))
      (setf (e-board-aggregation-timer aggregation) nil
            (e-board-aggregation-state aggregation) 'cancelled)
      (e-board-admission-append-event
       board 'subscription-cancelled
       (list :subscription-id aggregation-id))))
  t)

(defun e-board--observe-work-terminal (board work state payload)
  "Append WORK's terminal fact and queue its frozen indexed classifier."
  (unless (e-board-work-terminal-seq work)
    (let ((event (e-board-admission-append-event
                  board state
                  (list :work-id (e-board-work-id work)
                        :state state :payload payload))))
      (setf (e-board-work-state work) state
            (e-board-work-terminal-seq work) (e-board-event-seq event)
            (e-board-work-terminal-payload work) payload)
      (e-board-admission-queue-terminal-classification
       board work nil nil nil nil
       #'e-board--schedule-terminal-classification))))

(cl-defun e-board-enroll-work (board handle &key metadata admission)
  "Enroll prepared HANDLE in BOARD before its runner may start.
The canonical work id is the handle id.  The dedicated observer is installed
before runner entry so synchronous carriers cannot settle outside the log."
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (e-board-admission-require-clear board)
  (when (e-work-handle-started-p handle)
    (signal 'e-board-error (list "Cannot enroll started work" handle)))
  (let* ((id (e-work-handle-id handle))
         (supplied-admission-p (e-board-work-admission-p admission))
         (admission (or admission
                        (e-board-work-admission-token handle))))
    (unless (e-board-work-admission-p admission)
      (signal 'wrong-type-argument
              (list 'e-board-work-admission-p admission)))
    (when (and (e-board-work-admission-board admission)
               (not (eq (e-board-work-admission-board admission) board)))
      (signal 'e-board-error (list "Admission token belongs to another board")))
    (when (or (e-board-work-admission-committed-p admission)
              (e-board-work-admission-cleanup-complete-p admission))
      (signal 'e-board-error
              (list "Work admission token is no longer usable" admission)))
    (when (and (e-board-work-admission-handle admission)
               (not (eq (e-board-work-admission-handle admission) handle)))
      (signal 'e-board-error (list "Admission token belongs to another handle")))
    (when (e-board-observed-work board id)
      (signal 'e-board-id-conflict (list id)))
    (let* ((work (e-board-work--create
                  :id id :handle handle :metadata (copy-tree metadata)
                  :state 'posted))
           (observer
            (lambda (_handle state payload)
              (e-board--observe-work-terminal board work state payload))))
      (setf (e-board-work-admission-board admission) board
            (e-board-work-admission-handle admission) handle
            (e-board-work-admission-work admission) work
            (e-board-work-admission-work-owned-p admission) t)
      (setf (e-board-work-publication-observer work) observer)
      ;; Every admission is owned by the board for the whole cleanup lifetime.
      ;; An outer runtime transaction may have supplied the token already; the
      ;; idempotent begin operation simply refreshes its current stack frame.
      (e-board-admission-begin board admission)
      (condition-case err
          (progn
            ;; Install the owner observer before publishing the board record so
            ;; a carrier can never settle into an unobserved work entry.
            (e-work-install-publication-observer handle observer)
            (setf (e-board-work-posted-event work)
                  (e-board-admission-append-event
                   board 'posted
                   (list :work-id id :metadata (copy-tree metadata))
                   admission))
            (puthash id work (e-board-work-table board))
            (setf (e-board-work-admission-committed-p admission) t)
            (unless (e-board-admission-current-p admission)
              (signal 'e-board-admission-pending
                      (list "Work admission lost authority" admission)))
            (e-board-admission-finish board admission)
            (unless supplied-admission-p
              (e-board-admission-complete board admission))
            work)
        (error
         ;; The stack frame is ending even when an inverse is still pending;
         ;; a later public operation must be allowed to retry that exact token.
         (e-board-admission-finish board admission)
         (condition-case _cleanup-error
             (e-board-abort-work-enrollment board admission)
           (error nil))
         (signal (car err) (cdr err)))))))

(cl-defun e-board-subscribe-invocation (board work-id effect-target &key id)
  "Install one exact reply relation for BOARD WORK-ID.
EFFECT-TARGET is an opaque exact invocation identity owned by the runtime
effect adapter.  The board never retains or invokes a loop callback; it only
commits the terminal event, then asks its injected dispatcher to apply this
target after the start stack unwinds."
  (unless effect-target
    (signal 'e-board-error (list "Invocation effect target is required")))
  (e-board-admission-require-clear board)
  (unless (e-board-observed-work board work-id)
    (signal 'e-board-error (list "Unknown board work" work-id)))
  (let ((id (or id (e-board--next-id board 'invocation))))
    (when (e-board-invocation board id)
      (signal 'e-board-id-conflict (list id)))
    (let* ((work (e-board-observed-work board work-id))
           (invocation (e-board-invocation--create
                        :id id :work-id work-id :state 'open
                        :effect-target effect-target))
           (admission
            (e-board-work-admission--create
             :board board :handle (e-board-work-handle work)
             :work work :invocation invocation)))
      (e-board-admission-begin board admission)
      (condition-case err
          (progn
            (puthash id invocation (e-board-invocations board))
            (e-board-admission-index-work (e-board-invocation-work-index board)
                                              work-id id admission)
            (setf (e-board-invocation-subscription-event invocation)
                  (e-board-admission-append-event
                   board 'subscription-added
                   (list :subscription-id id :work-id work-id
                         :effect 'reply-to-invocation)
                   admission))
            ;; Enrolling and subscribing can be separated by a caller
            ;; transaction.  If an already-terminal handle is intentionally
            ;; subscribed, publish one frozen activation without scanning
            ;; unrelated history.
            (let ((work (e-board-observed-work board work-id)))
              (when (e-board-work-terminal-seq work)
                ;; Keep the frozen classifier record in the same exact board
                ;; admission as the invocation index/event.  A direct caller
                ;; therefore cannot leave a queued terminal route behind when
                ;; the surrounding subscription operation fails.
                (e-board-admission-queue-terminal-classification
                 board work (list id) nil nil admission
                 #'e-board--schedule-terminal-classification)))
            (setf (e-board-work-admission-committed-p admission) t)
            (unless (e-board-admission-current-p admission)
              (signal 'e-board-admission-pending
                      (list "Invocation admission lost authority" admission)))
            (e-board-admission-finish board admission)
            (e-board-admission-complete board admission)
            invocation)
        (error
         (e-board-admission-finish board admission)
         (condition-case _cleanup-error
             (e-board-abort-work-enrollment board admission)
           (error nil))
         (signal (car err) (cdr err)))))))

(cl-defun e-board-enroll-invocation-work
    (board handle invocation-id effect-target &key metadata admission)
  "Atomically enroll prepared HANDLE and its exact INVOCATION-ID relation.
EFFECT-TARGET is owned by an injected runtime invocation service.  This
convenience keeps required pre-run ordering at one application boundary without
making `e-work' depend on board state or making the board retain loop closures."
  ;; Validate every relation that can reject before the staged operation
  ;; appends either record.  A cheap runner may settle immediately, so callers
  ;; must never have to roll a visible enrollment back afterwards.
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (e-board-admission-require-clear board)
  (when (e-work-handle-started-p handle)
    (signal 'e-board-error (list "Cannot enroll started work" handle)))
  (when (e-board-observed-work board (e-work-handle-id handle))
    (signal 'e-board-id-conflict (list (e-work-handle-id handle))))
  (unless effect-target
    (signal 'e-board-error (list "Invocation effect target is required")))
  (when (e-board-invocation board invocation-id)
    (signal 'e-board-id-conflict (list invocation-id)))
  (let ((external-admission-p (e-board-work-admission-p admission)))
    (setq admission
          (or admission
              (e-board-work-admission-token
               handle :invocation-id invocation-id :effect-target effect-target)))
  (unless (e-board-work-admission-p admission)
    (signal 'wrong-type-argument
            (list 'e-board-work-admission-p admission)))
    (when (and (e-board-work-admission-board admission)
               (not (eq (e-board-work-admission-board admission) board)))
      (signal 'e-board-error (list "Admission token belongs to another board")))
  (when (and (e-board-work-admission-handle admission)
             (not (eq (e-board-work-admission-handle admission) handle)))
    (signal 'e-board-error (list "Admission token belongs to another handle")))
  (when (and (e-board-work-admission-invocation-id admission)
             (not (equal (e-board-work-admission-invocation-id admission)
                         invocation-id)))
    (signal 'e-board-error
            (list "Admission token belongs to another invocation")))
  (when (and (e-board-work-admission-effect-target admission)
             (not (eq (e-board-work-admission-effect-target admission)
                      effect-target)))
    (signal 'e-board-error
            (list "Admission token belongs to another effect target")))
  (when (or (e-board-work-admission-committed-p admission)
            (e-board-work-admission-cleanup-complete-p admission))
    (signal 'e-board-error
            (list "Work admission token is no longer usable" admission)))
  (let* ((work-id (e-work-handle-id handle))
         (work (e-board-work--create
                :id work-id :handle handle :metadata (copy-tree metadata)
                :state 'posted))
         (observer
          (lambda (_handle state payload)
            (e-board--observe-work-terminal board work state payload)))
         (invocation
          (e-board-invocation--create
           :id invocation-id :work-id work-id :state 'open
           :effect-target effect-target)))
    (setf (e-board-work-admission-board admission) board
          (e-board-work-admission-handle admission) handle
          (e-board-work-admission-work admission) work
          (e-board-work-admission-invocation admission) invocation
          (e-board-work-admission-work-owned-p admission) t)
    (setf (e-board-work-publication-observer work) observer)
      (e-board-admission-begin board admission)
    (condition-case err
        (progn
          ;; This is one board-owned commit: no public composed helper is
          ;; called between the work and its exact invocation relation.
          (e-work-install-publication-observer handle observer)
          (setf (e-board-work-posted-event work)
                (e-board-admission-append-event
                 board 'posted
                 (list :work-id work-id :metadata (copy-tree metadata))
                 admission))
          (puthash work-id work (e-board-work-table board))
          (puthash invocation-id invocation (e-board-invocations board))
          (e-board-admission-index-work (e-board-invocation-work-index board)
                                            work-id invocation-id admission)
          (setf (e-board-invocation-subscription-event invocation)
                (e-board-admission-append-event
                 board 'subscription-added
                 (list :subscription-id invocation-id :work-id work-id
                       :effect 'reply-to-invocation)
                 admission))
          (setf (e-board-work-admission-committed-p admission) t)
          (unless (e-board-admission-current-p admission)
            (signal 'e-board-admission-pending
                    (list "Combined admission lost authority" admission)))
          (e-board-admission-finish board admission)
          (unless external-admission-p
            (e-board-admission-complete board admission))
          handle)
      (error
       (e-board-admission-finish board admission)
      (condition-case _cleanup-error
           (e-board-abort-work-enrollment board admission)
         (error nil))
       (signal (car err) (cdr err)))))))

(defun e-board--active-participant-p (participant)
  "Return non-nil when PARTICIPANT can receive a new pickup."
  (memq (e-board-participant-state participant) '(active dormant stale)))

(defun e-board--append-subscription (board subscription)
  "Append SUBSCRIPTION to BOARD's ordered list and constant-time indexes."
  (let* ((index (cl-incf (e-board-subscription-count board)))
         (cell (list subscription)))
    (if (e-board-subscriptions-tail board)
        (setcdr (e-board-subscriptions-tail board) cell)
      (setf (e-board-subscriptions board) cell))
    (setf (e-board-subscriptions-tail board) cell)
    (puthash index subscription (e-board-subscription-index-table board))
    (puthash (e-board-subscription-id subscription) subscription
             (e-board-subscription-id-table board))
    subscription))

(defun e-board--next-subscription-lifetime-token (board)
  "Return BOARD's monotonic identity for one subscription lifetime.

The durable subscription id is intentionally reusable by exact restoration
and registry teardown.  This separate board-local token is never reused while
BOARD lives, so queued classifier, replay, timer, and effect callbacks cannot
mistake a replacement with the same id for their original route."
  (list (e-board-id board)
        (cl-incf (e-board-subscription-lifetime-sequence board))))

(cl-defun e-board-add-participant
    (board &key id (state 'active) create-pickup-subscription-id
           (publish-event t))
  "Add participant ID to BOARD and install its built-in exact pickup route.
The identity subscription is membership-owned: ordinary subscriptions cannot
replace it, and exact input ignores descriptive tags and other subscriptions."
  (let* ((id (or id (e-board--next-id board 'participant)))
         (subscription-id
          (or create-pickup-subscription-id
              (e-board--next-id board 'subscription))))
    (e-board--require-id id 'e-board-participant-id)
    (when (e-board-participant board id)
      (signal 'e-board-id-conflict (list id)))
    (e-board--validate-subscription-id subscription-id)
    (setq subscription-id
          (e-board--freeze-envelope-value
           subscription-id 'subscription-id e-board-message-metadata-byte-limit))
    (when (e-board-find-subscription board subscription-id)
      (signal 'e-board-id-conflict (list subscription-id)))
    (let ((participant
           (e-board-participant--create
            :id id :board-id (e-board-id board) :state state
            :create-pickup-subscription-id subscription-id)))
      (puthash id participant (e-board-participants board))
      (e-board--append-subscription
       board
       (e-board-subscription--create
        :id subscription-id
        :board-id (e-board-id board)
        :participant-id id
        :selector (list :to id)
        :effect 'create-pickup
        :state 'active :delivery 'normal :self-delivery t
        :built-in-p t
        :lifetime-token (e-board--next-subscription-lifetime-token board)))
      (when publish-event
        (e-board-admission-append-event board 'participant-added
                               (list :participant-id id
                                     :subscription-id subscription-id)))
      participant)))

(defun e-board-abort-participant-admission (board participant-id)
  "Remove an unpublished PARTICIPANT-ID admission from BOARD.
This narrow Board contract is used when the registry/runtime attachment
transaction fails before the participant has been exposed to board traffic; it
does not append a participant-removed event.  Callers must use the deferred
participant admission path when the participant-added event must not be
published until a surrounding durable declaration succeeds."
  (let ((participant (e-board-participant board participant-id)))
    (when participant
      ;; Admission rollback owns no durable participant event, but it still
      ;; uses the same exact route operation as terminal participant cleanup.
      ;; This keeps timers, classifiers, prepared effects, and replay records
      ;; fenced if a partially admitted client retained one of their callbacks.
      (dolist (subscription (copy-sequence (e-board-subscriptions board)))
        (when (equal (e-board-subscription-participant-id subscription)
                     participant-id)
          (when-let ((current
                      (e-board-find-subscription
                       board (e-board-subscription-id subscription))))
            (e-board-retire-subscription-exact board current))))
      (remhash participant-id (e-board-participants board))
      (setf (e-board-participant-state participant) 'removed))
    participant))

(defun e-board--valid-continuation-readiness-p (readiness)
  "Return non-nil when READINESS is a closed continuation accumulator policy."
  (or (null readiness)
      (and (listp readiness)
           (cond
             ((eq (plist-get readiness :policy) 'batch)
              (let ((count (plist-get readiness :count))
                    (max-delay (plist-get readiness :max-delay)))
                (and (integerp count) (> count 0)
                     (or (null max-delay)
                         (and (numberp max-delay) (> max-delay 0))))))
             ((eq (plist-get readiness :policy) 'latest-after-quiet)
              (let ((quiet-period (plist-get readiness :quiet-period)))
                (and (numberp quiet-period) (> quiet-period 0))))))))

(defun e-board--validate-continuation-readiness (effect readiness)
  "Reject a READINESS declaration that cannot belong to EFFECT."
  (when (and readiness (not (and (listp effect) (eq (car effect) :post-input))))
    (signal 'e-board-error (list "Readiness requires post-input effect" readiness)))
  (unless (e-board--valid-continuation-readiness-p readiness)
    (signal 'e-board-error (list "Invalid continuation readiness" readiness))))

(defun e-board--validate-subscription-id (id &optional processing-p)
  "Validate subscription ID, requiring a durable string when PROCESSING-P.
Normal subscriptions retain the legacy identifier contract."
  (when (and processing-p (not (stringp id)))
    (signal 'wrong-type-argument (list 'stringp id)))
  id)

(defun e-board--validate-subscription-delivery
    (effect delivery priority self-delivery failure-policy)
  "Validate the delivery contract shared by subscription creation and replacement."
  (unless (memq delivery '(normal process))
    (signal 'wrong-type-argument (list '(member normal process) delivery)))
  (unless (memq self-delivery '(nil t))
    (signal 'wrong-type-argument (list 'booleanp self-delivery)))
  (pcase delivery
    ('normal
     (when priority
       (signal 'e-board-error (list "Priority requires process delivery" priority)))
     (when failure-policy
       (signal 'e-board-error
               (list "Failure policy requires process delivery" failure-policy))))
    ('process
     (unless (eq effect 'create-pickup)
       (signal 'e-board-error
               (list "Process delivery requires create-pickup effect" effect)))
     (unless (and (integerp priority)
                  (<= e-board-processing-priority-min priority)
                  (<= priority e-board-processing-priority-max))
       (signal 'wrong-type-argument
               (list (list 'integer-range e-board-processing-priority-min
                           e-board-processing-priority-max)
                     priority)))
     (unless (memq failure-policy '(pass consume))
       (signal 'wrong-type-argument
               (list '(member pass consume) failure-policy))))))

(cl-defun e-board-subscribe
    (board participant-id selector &key id (state 'active) (effect 'create-pickup)
           (delivery 'normal) priority (self-delivery nil)
           failure-policy readiness firing-limit lifetime start-seq)
  "Install an ordinary immutable subscription for PARTICIPANT-ID.
SELECTOR supports kind, activity-kind, identity, attribute, and tag clauses.
Effects are `create-pickup' and declarative `(:post-input ...)'.  A post-input
continuation may declare READINESS as `(:policy batch :count N :max-delay
SECONDS)' or `(:policy latest-after-quiet :quiet-period SECONDS)'; otherwise
every match fires.  DELIVERY is `normal' by default.  `process' uses a pickup,
defaults PRIORITY to zero, and accepts a FAILURE-POLICY of `pass' or `consume'.
PRIORITY and FAILURE-POLICY are invalid for normal delivery.  SELF-DELIVERY is
disabled by default.  FIRING-LIMIT, when non-nil, is the positive number of
post-input activations permitted before the subscription completes.  LIFETIME,
when non-nil, is a positive number of seconds before the subscription expires.
START-SEQ is an explicit retained-history replay cursor for a post-input
continuation; it never reroutes an existing input or creates a pickup.
New subscriptions inspect future board records; only `create-pickup' is
restricted to input records."
  (unless (e-board-participant board participant-id)
    (signal 'e-board-error (list "Unknown participant" participant-id)))
  (unless (or (eq effect 'create-pickup)
              (and (listp effect) (eq (car effect) :post-input)))
    (signal 'e-board-error (list "Unsupported board effect" effect)))
  (setq priority (if (and (eq delivery 'process) (null priority)) 0 priority)
        failure-policy (if (and (eq delivery 'process) (null failure-policy))
                           'pass
                         failure-policy))
  (e-board--validate-subscription-delivery
   effect delivery priority self-delivery failure-policy)
  (e-board--validate-continuation-readiness effect readiness)
  (when firing-limit
    (unless (and (integerp firing-limit) (> firing-limit 0))
      (signal 'wrong-type-argument (list 'plusp firing-limit)))
    (unless (and (listp effect) (eq (car effect) :post-input))
      (signal 'e-board-error
              (list "Firing limit requires post-input effect" firing-limit))))
  (when lifetime
    (unless (and (numberp lifetime) (> lifetime 0))
      (signal 'wrong-type-argument (list 'plusp lifetime))))
  (when start-seq
    (unless (and (integerp start-seq)
                 (>= start-seq (1- (e-board-retention-floor board))))
      (signal 'e-board-error
              (list "Replay start is outside retained board history" start-seq)))
    (unless (and (listp effect) (eq (car effect) :post-input))
      (signal 'e-board-error
              (list "Replay requires post-input effect" start-seq))))
  (unless (listp selector)
    (signal 'wrong-type-argument (list 'listp selector)))
  (unless (memq state '(active muted))
    (signal 'wrong-type-argument (list '(member active muted) state)))
  (let ((id (or id (e-board--next-id board 'subscription))))
    (e-board--validate-subscription-id id (eq delivery 'process))
    (setq id (e-board--freeze-envelope-value
              id 'subscription-id e-board-message-metadata-byte-limit))
    (when (e-board-find-subscription board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((subscription
           (e-board-subscription--create
            :id id :board-id (e-board-id board)
            :participant-id participant-id
            ;; The matcher is immutable even if the caller later mutates its plist.
            :selector (copy-tree selector)
            :effect (copy-tree effect) :state state :built-in-p nil
            :delivery delivery :priority priority :self-delivery self-delivery
            :failure-policy failure-policy
            :readiness (copy-tree readiness) :accumulator nil
            :readiness-generation 0 :firing-number 0
            :firing-limit firing-limit :lifetime lifetime
            :lifetime-generation 0
            :lifetime-token (e-board--next-subscription-lifetime-token board))))
      (e-board--append-subscription board subscription)
      (e-board-admission-append-event
       board 'subscription-added
       (append (list :subscription-id id :participant-id participant-id
                     :firing-limit firing-limit :lifetime lifetime)
               (when (eq delivery 'process)
                 (list :delivery delivery :priority priority
                       :self-delivery self-delivery
                       :failure-policy failure-policy))))
      (when lifetime
        (setf (e-board-subscription-lifetime-timer subscription)
              (e-board--schedule-subscription-timer
               board lifetime
               (lambda ()
                 (e-board--queue-subscription-expiry
                 board id (e-board-subscription-lifetime-generation subscription)
                 (e-board-subscription-lifetime-token subscription))))))
      (when start-seq
        (e-board--queue-subscription-replay board subscription start-seq))
      subscription)))

(defun e-board-order-processing-subscriptions (subscriptions)
  "Return processing SUBSCRIPTIONS in deterministic delivery order.
Higher priority runs first.  Equal priorities order by durable string
subscription IDs, so replay does not depend on traversal or input order."
  (dolist (subscription subscriptions)
    (unless (eq (e-board-subscription-delivery subscription) 'process)
      (signal 'e-board-error (list "Processing order requires process delivery"
                                   subscription)))
    (e-board--validate-subscription-id (e-board-subscription-id subscription) t))
  (sort (copy-sequence subscriptions)
        (lambda (left right)
          (let ((left-priority (e-board-subscription-priority left))
                (right-priority (e-board-subscription-priority right)))
            (if (= left-priority right-priority)
                (string< (e-board-subscription-id left)
                         (e-board-subscription-id right))
              (> left-priority right-priority))))))

(defun e-board--tags-match-p (selector message)
  "Return non-nil when SELECTOR's tag clauses match MESSAGE."
  (let ((tags (e-board-message-tags message))
        (all (or (plist-get selector :tags-all)
                 (plist-get selector :tags)))
        (any (plist-get selector :tags-any)))
    (and (cl-every (lambda (tag) (member tag tags)) all)
         (or (null any) (cl-some (lambda (tag) (member tag tags)) any)))))

(defun e-board--proper-list-p (value)
  "Return non-nil when VALUE is a finite proper list.
The ordinary `proper-list-p' helper is not suitable at this trust boundary:
callers may hand us a cyclic value and the board must reject it without
walking forever.  Keep this small cycle-aware walker local to the board
grammar so session admission can consume one authoritative predicate."
  (let ((tail value)
        (seen (make-hash-table :test 'eq))
        valid)
    (setq valid t)
    (while (and valid (consp tail))
      (if (gethash tail seen)
          (setq valid nil)
        (puthash tail t seen)
        (setq tail (cdr tail))))
    (and valid (null tail))))

(defun e-board--selector-attribute-value-valid-p (value)
  "Return non-nil when nested attribute VALUE is reversible data.
Attribute values are declarative data, not predicates.  Symbols remain data
even when their names are callable; actual function objects and lambda forms
are rejected.  The walk is iterative and cycle-aware because the session
codec preserves vectors, lists, plists, and dotted conses reversibly."
  (let ((pending (list (list :value value)))
        (visiting (make-hash-table :test 'eq))
        (leave-marker (make-symbol "board-attribute-leave"))
        (valid t))
    (while (and valid pending)
      (let ((task (pop pending)))
        (if (eq (car task) leave-marker)
            (remhash (cdr task) visiting)
          (let ((current (cadr task)))
            (cond
             ((or (null current) (eq current t) (numberp current)
                  (stringp current) (symbolp current)) nil)
             ((functionp current)
              (setq valid nil))
             ((and (consp current)
                   (memq (car current) '(lambda function)))
              (setq valid nil))
             ((or (vectorp current) (consp current))
              (if (gethash current visiting)
                  (setq valid nil)
                (puthash current t visiting)
                (push (cons leave-marker current) pending)
                (if (vectorp current)
                    (let ((index (1- (length current))))
                      (while (>= index 0)
                        (push (list :value (aref current index)) pending)
                        (setq index (1- index))))
                  (push (list :value (cdr current)) pending)
                  (push (list :value (car current)) pending))))
             (t
              (setq valid nil)))))))
    valid))

(defun e-board-selector-attributes-valid-p (attributes)
  "Return non-nil when ATTRIBUTES has the board matcher grammar.
The top level is nil, an even keyword plist, or a proper alist of keyword
key/value conses.  Values may contain the reversible declarative data forms
accepted by the session codec.  This function is pure and intentionally does
not normalize or retain caller-owned objects."
  (cond
   ((null attributes) t)
   ((not (e-board--proper-list-p attributes)) nil)
   ((keywordp (car attributes))
    (let ((tail attributes)
          seen
          (valid t))
      (while (and valid tail)
        (if (not (consp (cdr tail)))
            (setq valid nil)
          (let ((key (pop tail))
                (value (pop tail)))
            (setq valid
                  (and (keywordp key)
                       (not (memq key seen))
                       (e-board--selector-attribute-value-valid-p value)))
            (push key seen))))
      valid))
   (t
    (cl-every
     (lambda (pair)
       (and (consp pair)
            (keywordp (car pair))
            ;; An alist entry is a keyword-to-value cons.  Its complete CDR
            ;; is the value, so nested list values remain unambiguous.
            (e-board--selector-attribute-value-valid-p (cdr pair))))
     attributes))))

(defun e-board--selector-attribute-clauses (attributes)
  "Return ATTRIBUTES as canonical key/value conses after validation."
  (unless (e-board-selector-attributes-valid-p attributes)
    (signal 'wrong-type-argument (list 'board-selector-attributes attributes)))
  (cond
   ((null attributes) nil)
   ((keywordp (car attributes))
    (let (clauses)
      (while attributes
        (let ((key (pop attributes))
              (value (pop attributes)))
          (push (cons key value) clauses)))
      (nreverse clauses)))
   (t
    (mapcar (lambda (pair)
              (cons (car pair) (cdr pair)))
            attributes))))

(defun e-board--selector-attributes-match-p (selector message)
  "Return non-nil when SELECTOR's bounded attribute clauses match MESSAGE."
  (cl-every (lambda (pair)
              (equal (plist-get (e-board-message-attributes message) (car pair))
                     (cdr pair)))
            (e-board--selector-attribute-clauses
             (plist-get selector :attributes))))

(defun e-board--fault-subscription (board subscription err)
  "Record trusted predicate ERR without reviving a changed subscription view."
  (when-let ((current (e-board--subscription-lifetime-current-p board subscription)))
    (setf (e-board-subscription-state subscription) 'faulted)
    (when (eq (e-board-subscription-state current) 'active)
      (e-board--transition-subscription board current 'faulted)
      (e-board-admission-append-event
       board 'subscription-faulted
       (list :subscription-id (e-board-subscription-id current)
             :error err)))))

(defun e-board--selector-matches-p (board subscription message)
  "Return non-nil when SUBSCRIPTION's immutable selector matches MESSAGE."
  (let ((selector (e-board-subscription-selector subscription)))
    (and (or (not (plist-member selector :kind))
             (equal (plist-get selector :kind) (e-board-message-kind message)))
         (or (not (plist-member selector :activity-kind))
             (equal (plist-get selector :activity-kind)
                    (e-board-message-activity-kind message)))
         (or (not (plist-member selector :to))
             (equal (plist-get selector :to) (e-board-message-to message)))
         (or (not (plist-member selector :author))
             (equal (plist-get selector :author) (e-board-message-author message)))
         (or (not (plist-member selector :subject-participant-id))
             (equal (plist-get selector :subject-participant-id)
                    (e-board-message-subject-participant-id message)))
          (e-board--selector-attributes-match-p selector message)
          (e-board--tags-match-p selector message)
          (if-let ((predicate (plist-get selector :predicate)))
              (condition-case err
                  (funcall predicate message)
                (error
                 (e-board--fault-subscription board subscription err)
                 nil))
            t))))

(defun e-board--message-count-through-seq (board seq)
  "Return BOARD's number of messages whose event sequence is at most SEQ."
  (cond
   ((<= seq 0) 0)
   ((>= seq (e-board-event-message-prefix-high-watermark board))
    (e-board-message-count board))
   (t
    ;; Every allocated event has an exact retained prefix checkpoint.  The
    ;; checkpoint deliberately survives an admission inverse, including an
    ;; event-log gap.  A missing key below the high-water mark is therefore an
    ;; invariant failure, not a reason to scan the whole history and risk
    ;; replaying an older message prefix.
    (let* ((missing (make-symbol "missing-prefix"))
           ;; An explicit sentinel keeps a stored zero prefix distinguishable
           ;; from a missing checkpoint.
           (value (gethash seq (e-board-event-message-count board) missing)))
      (if (eq value missing)
          (signal 'e-board-error
                  (list "Missing event message-prefix checkpoint" seq))
        value)))))

(cl-defun e-board-observer-subscribe
    (board client-id selector &key id client-generation (state 'active) start-seq
           history-before-seq (history-floor 0))
  "Create an effect-free client observer cursor over BOARD's message sequence.
Observers deliberately share selector fields with participant subscriptions,
but they cannot activate effects, create pickups, alter routedness, or consume
messages.  START-SEQ is an explicit retained-history cursor; live callers use
the returned cursor's advancing `next-seq' for later bounded pages."
  (unless (listp selector)
    (signal 'wrong-type-argument (list 'listp selector)))
  (unless (memq state e-board--observer-states)
    (signal 'wrong-type-argument
            (list e-board--observer-states state)))
  (setq start-seq (or start-seq (e-board-next-seq board)))
  (unless (and (integerp start-seq) (>= start-seq 0))
    (signal 'wrong-type-argument (list 'natnump start-seq)))
  (unless (and (integerp history-floor) (>= history-floor 0))
    (signal 'wrong-type-argument (list 'natnump history-floor)))
  (when client-generation
    (unless (and (integerp client-generation) (> client-generation 0))
      (signal 'wrong-type-argument (list 'plusp client-generation))))
  (when history-before-seq
    (unless (and (integerp history-before-seq)
                 (>= history-before-seq history-floor))
      (signal 'wrong-type-argument (list 'natnump history-before-seq))))
  (let ((id (or id (e-board--next-id board 'observer))))
    (when (e-board-observer board id)
      (signal 'e-board-id-conflict (list id)))
    (let ((observer (e-board-observer--create
                     :id id :board-id (e-board-id board) :client-id client-id
                     :client-generation client-generation
                     :selector (copy-tree selector) :state state
                     :next-seq start-seq
                     :next-index (e-board--message-count-through-seq
                                  board start-seq)
                     :history-before-seq history-before-seq
                     :history-before-index nil
                     :history-floor history-floor)))
      (puthash id observer (e-board-observers board))
      (e-board-admission-append-event board 'observer-added
                             (list :observer-id id :client-id client-id
                                   :start-seq start-seq))
      observer)))

(cl-defun e-board-observer-prepare-history-page (board observer-id &key (limit 32))
  "Prepare one bounded ascending history page without moving its cursor.
Return =:messages= plus an opaque =:receipt= for
`e-board-observer-accept-history-page'."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'plusp limit)))
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (if-let ((prepared (e-board-observer-prepared-history-page observer)))
        (copy-tree prepared)
        (let ((next-before nil)
              (next-before-index nil)
              (index
               (or (e-board-observer-history-before-index observer)
                   (and (e-board-observer-history-before-seq observer)
                        (e-board--message-count-through-seq
                         board
                         (1- (e-board-observer-history-before-seq observer))))))
              (inspected 0)
              matches)
          (when (and (eq (e-board-observer-state observer) 'active)
                     (e-board-observer-history-before-seq observer))
            (let ((floor (e-board-observer-history-floor observer)))
              (while (and index (> index 0) (< inspected limit))
                (let ((message (gethash index (e-board-message-index-table board))))
                  (if (< (e-board-message-seq message) floor)
                      (setq index 0)
                    (cl-incf inspected)
                    (setq next-before (e-board-message-seq message)
                          next-before-index (1- index))
                    (when (e-board--observer-matches-p board observer message)
                      (push message matches))
                    (cl-decf index))))))
          (let* ((receipt
                  (and next-before
                       (list (e-board-id board) observer-id 'history
                             (e-board-observer-history-before-seq observer)
                             next-before)))
                 (page (list :messages matches :before-seq next-before
                             :before-index next-before-index
                             :receipt receipt)))
            (when receipt
              (setf (e-board-observer-prepared-history-page observer) page))
            (copy-tree page))))))

(defun e-board-observer-accept-history-page (board observer-id receipt)
  "Advance OBSERVER-ID's history cursor after its exact prepared RECEIPT."
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (if (and receipt
             (equal receipt
                    (e-board-observer-last-accepted-history-receipt observer)))
        observer
      (let* ((page (e-board-observer-prepared-history-page observer))
             (expected (and page (plist-get page :receipt)))
             (before-seq (and page (plist-get page :before-seq)))
             (before-index (and page (plist-get page :before-index))))
        (unless (and expected (equal receipt expected))
          (signal 'e-board-error
                  (list "Invalid observer history acceptance" observer-id receipt)))
        (unless (and (eq (e-board-observer-state observer) 'active)
                     (integerp before-seq)
                     (e-board-observer-history-before-seq observer)
                     (< before-seq (e-board-observer-history-before-seq observer))
                     (>= before-seq (e-board-observer-history-floor observer)))
          (signal 'e-board-error
                  (list "Stale observer history acceptance" observer-id receipt)))
        (setf (e-board-observer-history-before-seq observer) before-seq
              (e-board-observer-history-before-index observer) before-index
              (e-board-observer-prepared-history-page observer) nil
              (e-board-observer-last-accepted-history-receipt observer)
              (copy-tree receipt))
        (e-board-admission-append-event board 'observer-history-page-accepted
                               (list :observer-id observer-id :before-seq before-seq))
        observer))))

(cl-defun e-board-observer-read-history-page (board observer-id &key (limit 32))
  "Synchronously prepare and accept one bounded ascending history page."
  (let* ((page (e-board-observer-prepare-history-page board observer-id :limit limit))
         (receipt (plist-get page :receipt)))
    (when receipt
      (e-board-observer-accept-history-page board observer-id receipt))
    (plist-get page :messages)))

(defun e-board--observer-matches-p (board observer message)
  "Return non-nil when OBSERVER can observe MESSAGE, faulting only itself."
  (let ((selector (e-board-observer-selector observer)))
    (and (or (not (plist-member selector :kind))
             (equal (plist-get selector :kind) (e-board-message-kind message)))
         (or (not (plist-member selector :activity-kind))
             (equal (plist-get selector :activity-kind)
                    (e-board-message-activity-kind message)))
         (or (not (plist-member selector :to))
             (equal (plist-get selector :to) (e-board-message-to message)))
         (or (not (plist-member selector :author))
             (equal (plist-get selector :author) (e-board-message-author message)))
         (or (not (plist-member selector :subject-participant-id))
             (equal (plist-get selector :subject-participant-id)
                    (e-board-message-subject-participant-id message)))
         (e-board--selector-attributes-match-p selector message)
         (e-board--tags-match-p selector message)
         (if-let ((predicate (plist-get selector :predicate)))
             (condition-case err
                 (funcall predicate message)
               (error
                (e-board--fault-observer board observer err)
                nil))
           t))))

(defun e-board--observer-transition-allowed-p (from to)
  "Return non-nil when observer state FROM may transition to TO."
  (pcase from
    ('active (memq to '(muted faulted cancelled expired)))
    ('muted (memq to '(active cancelled expired)))
    (_ nil)))

(defun e-board--transition-observer (board observer state)
  "Commit OBSERVER's effect-free lifecycle transition to STATE on BOARD."
  (let ((from (e-board-observer-state observer)))
    (unless (memq state e-board--observer-states)
      (signal 'wrong-type-argument (list e-board--observer-states state)))
    (unless (e-board--observer-transition-allowed-p from state)
      (signal 'e-board-error
              (list "Illegal observer transition" from state
                    (e-board-observer-id observer))))
    (setf (e-board-observer-state observer) state
          (e-board-observer-prepared-page observer) nil
          (e-board-observer-prepared-history-page observer) nil)
    (e-board-admission-append-event board 'observer-transition
                           (list :observer-id (e-board-observer-id observer)
                                 :from from :state state))
    observer))

(defun e-board--fault-observer (board observer err)
  "Record trusted observer predicate ERR without reviving a replaced cursor."
  (when (eq (e-board-observer-state observer) 'active)
    (e-board--transition-observer board observer 'faulted)
    (e-board-admission-append-event
     board 'observer-faulted
     (list :observer-id (e-board-observer-id observer) :error err))))

(defun e-board-set-observer-state (board observer-id state)
  "Transition effect-free OBSERVER-ID to STATE on BOARD."
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (e-board--transition-observer board observer state)))

(cl-defun e-board-replace-observer
    (board observer-id selector &key id (state 'active) start-seq)
  "Cancel OBSERVER-ID and install a fresh client-local observer cursor.
Omitted START-SEQ keeps replacement future-only at the old cursor; callers
request retained backfill explicitly with a lower START-SEQ."
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (unless (listp selector)
      (signal 'wrong-type-argument (list 'listp selector)))
    (unless (memq state '(active muted))
      (signal 'wrong-type-argument (list '(member active muted) state)))
    (let ((replacement-id (or id (e-board--next-id board 'observer))))
      (when (e-board-observer board replacement-id)
        (signal 'e-board-id-conflict (list replacement-id)))
      (unless (memq (e-board-observer-state observer)
                    '(faulted cancelled expired))
        (e-board--transition-observer board observer 'cancelled))
      (let ((replacement
             (e-board-observer-subscribe
             board (e-board-observer-client-id observer) selector
              :id replacement-id :state state
              :client-generation (e-board-observer-client-generation observer)
              :start-seq (or start-seq (e-board-observer-next-seq observer)))))
        (e-board-admission-append-event board 'observer-replaced
                               (list :observer-id observer-id
                                     :replacement-id replacement-id))
        replacement))))

(cl-defun e-board-observer-prepare-page (board observer-id &key (limit 32))
  "Prepare one bounded observer page without advancing its live cursor.
Return a plist with =:messages=, =:through-seq=, and an opaque =:receipt=
receipt.  A client queue must call `e-board-observer-accept-page' only after
it accepts this page.  Trusted predicates run here, never from append."
  (unless (and (integerp limit) (> limit 0))
    (signal 'wrong-type-argument (list 'plusp limit)))
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id))))
        (resnapshot-required nil))
    (when (and (eq (e-board-observer-state observer) 'active)
               (< (e-board-observer-next-seq observer)
                  (1- (e-board-retention-floor board))))
      (e-board--transition-observer board observer 'expired)
      (setq resnapshot-required t))
    (when (eq (e-board-observer-state observer) 'expired)
      (setq resnapshot-required t))
    (if-let ((prepared (e-board-observer-prepared-page observer)))
        (copy-tree prepared)
        (let ((inspected 0)
              (index (1+ (e-board-observer-next-index observer)))
              (through-seq nil)
              (through-index nil)
              matches)
          (when (eq (e-board-observer-state observer) 'active)
            (while (and (<= index (e-board-message-count board))
                        (< inspected limit))
              (let ((message (gethash index (e-board-message-index-table board))))
                (when (>= (e-board-message-seq message)
                          (e-board-retention-floor board))
                  (cl-incf inspected)
                  (setq through-seq (e-board-message-seq message)
                        through-index index)
                  (when (e-board--observer-matches-p board observer message)
                    (push message matches)))
                (cl-incf index))))
          (let* ((receipt
                  (and through-seq
                       (list (e-board-id board) observer-id 'live
                             (e-board-observer-next-seq observer) through-seq)))
                 (page (list :messages (nreverse matches)
                             :through-seq through-seq
                             :through-index through-index
                             :receipt receipt
                             :resnapshot-required resnapshot-required)))
            (when receipt
              (setf (e-board-observer-prepared-page observer) page))
            (copy-tree page))))))

(defun e-board-observer-accept-page (board observer-id receipt)
  "Advance OBSERVER-ID through its exact prepared page RECEIPT.
Only the pinned page currently owned by the observer may advance its cursor;
retrying the last committed receipt is idempotent."
  (let ((observer (or (e-board-observer board observer-id)
                      (signal 'e-board-observer-missing (list observer-id)))))
    (if (and receipt
             (equal receipt (e-board-observer-last-accepted-page-receipt observer)))
        observer
      (let* ((page (e-board-observer-prepared-page observer))
             (expected (and page (plist-get page :receipt)))
             (through-seq (and page (plist-get page :through-seq)))
             (through-index (and page (plist-get page :through-index))))
        (unless (and expected (equal receipt expected))
          (signal 'e-board-error
                  (list "Invalid observer page acceptance" observer-id receipt)))
        (unless (and (eq (e-board-observer-state observer) 'active)
                     (integerp through-seq)
                     (> through-seq (e-board-observer-next-seq observer))
                     (<= through-seq (e-board-next-seq board)))
          (signal 'e-board-error
                  (list "Stale observer page acceptance" observer-id receipt)))
        (setf (e-board-observer-next-seq observer) through-seq
              (e-board-observer-next-index observer) through-index
              (e-board-observer-prepared-page observer) nil
              (e-board-observer-last-accepted-page-receipt observer)
              (copy-tree receipt))
        (e-board-admission-append-event board 'observer-page-accepted
                               (list :observer-id observer-id
                                     :through-seq through-seq))
        observer))))

(cl-defun e-board-observer-read-page (board observer-id &key (limit 32))
  "Synchronously prepare and accept one bounded page for OBSERVER-ID.
Asynchronous client adapters should instead use `e-board-observer-prepare-page'
and acknowledge only after their queue accepts the returned page."
  (let* ((page (e-board-observer-prepare-page board observer-id :limit limit))
         (receipt (plist-get page :receipt)))
    (when receipt
      (e-board-observer-accept-page board observer-id receipt))
    (plist-get page :messages)))

(defun e-board--eligible-subscription-p (board subscription)
  "Return non-nil when SUBSCRIPTION is active and its participant can receive."
  (and (eq (e-board-subscription-state subscription) 'active)
       (eq (e-board-subscription-effect subscription) 'create-pickup)
       (when-let ((participant
                   (e-board-participant board
                                        (e-board-subscription-participant-id
                                         subscription))))
          (e-board--active-participant-p participant))))

(defun e-board-find-subscription (board subscription-id)
  "Return BOARD's subscription SUBSCRIPTION-ID, or nil."
  (gethash subscription-id (e-board-subscription-id-table board)))

(defun e-board--subscription-lifetime-current-p (board snapshot)
  "Return non-nil when SNAPSHOT names the current object and lifetime.

The object identity protects ordinary same-process callbacks while the
monotonic token protects copied classifier/replay snapshots and same-id
restoration.  Both checks are intentional: neither a reused id nor a copied
record may regain authority over a replacement subscription."
  (when-let ((current
              (e-board-find-subscription
               board (e-board-subscription-id snapshot))))
    (and (equal (e-board-subscription-lifetime-token current)
                (e-board-subscription-lifetime-token snapshot))
         current)))

(defun e-board--classification-subscription-current-p (board snapshot)
  "Return the active current subscription represented by frozen SNAPSHOT.
  The snapshot keeps historical selector bytes stable, while this lookup fences
  later mute, terminal, cancellation, expiry, and replacement transitions."
  (when-let ((current (e-board--subscription-lifetime-current-p board snapshot)))
    (and (eq (e-board-subscription-state current) 'active)
         current)))

(defun e-board--subscription-effect-current-p (board subscription)
  "Return non-nil when SUBSCRIPTION's exact lifetime may apply an effect.
Completed lifetimes may still commit an already-reserved activation; muted,
cancelled, expired, faulted, and replaced lifetimes may not."
  (let ((current (e-board-find-subscription
                  board (e-board-subscription-id subscription))))
    (and (eq current subscription)
         (equal (e-board-subscription-lifetime-token current)
                (e-board-subscription-lifetime-token subscription))
         (memq (e-board-subscription-state current) '(active completed)))))

(defun e-board--subscription-transition-allowed-p (from to)
  "Return non-nil when ordinary subscription state FROM may move to TO."
  (pcase from
    ('active (memq to '(muted completed faulted cancelled expired)))
    ('muted (memq to '(active cancelled expired)))
    (_ nil)))

(defun e-board--transition-subscription (board subscription state
                                                   &optional suppress-event-p)
  "Commit SUBSCRIPTION's ordinary lifecycle transition to STATE on BOARD.
When SUPPRESS-EVENT-P is non-nil, perform process-local owner teardown without
adding a durable lifecycle event; immutable historical records and callback
fences are still retained."
  (let ((from (e-board-subscription-state subscription)))
    (unless (memq state e-board--ordinary-subscription-states)
      (signal 'wrong-type-argument
              (list e-board--ordinary-subscription-states state)))
    (unless (e-board--subscription-transition-allowed-p from state)
      (signal 'e-board-error
              (list "Illegal subscription transition" from state
                    (e-board-subscription-id subscription))))
    (setf (e-board-subscription-state subscription) state)
    (when (memq state '(muted completed faulted cancelled expired))
      (when-let ((timer (e-board-subscription-readiness-timer subscription)))
        (cancel-timer timer))
      (setf (e-board-subscription-readiness-timer subscription) nil
            (e-board-subscription-accumulator subscription) nil
            (e-board-subscription-readiness-generation subscription)
            (1+ (or (e-board-subscription-readiness-generation subscription) 0))))
    (when (memq state '(completed faulted cancelled expired))
      (when-let ((timer (e-board-subscription-lifetime-timer subscription)))
        (cancel-timer timer))
      (setf (e-board-subscription-lifetime-timer subscription) nil
            (e-board-subscription-lifetime-generation subscription)
            (1+ (or (e-board-subscription-lifetime-generation subscription) 0))))
    ;; Keep the historical transition behavior for ordinary public state
    ;; changes.  Exact owner retirement passes a non-nil suppress marker so
    ;; process-local participant
    ;; teardown does not invent a durable event that callers did not publish.
    (unless suppress-event-p
      (e-board-admission-append-event
       board 'subscription-transition
       (list :subscription-id (e-board-subscription-id subscription)
             :from from :state state)))
    (when (memq state '(muted cancelled))
      (e-board--cancel-prepared-activations
       board (e-board-subscription-id subscription) state
       (e-board-subscription-lifetime-token subscription)
       suppress-event-p))
    subscription))

(defun e-board--cancel-prepared-activations
    (board subscription-id reason &optional subscription-token
           suppress-event-p)
  "Fence prepared effects owned by one subscription lifetime.
When SUBSCRIPTION-TOKEN is supplied, callbacks from an older same-id lifetime
are the only activations cancelled; a replacement with the same id is left
untouched.  SUPPRESS-EVENT-P is used by process-local board close/retirement
so it can fence an activation without manufacturing a durable cancellation
record."
  (dolist (activation-id
           (gethash subscription-id (e-board-activation-subscription-index board)))
    (when-let ((activation (e-board-activation board activation-id)))
      (when (and (eq (e-board-activation-state activation) 'prepared)
                 (or (null subscription-token)
                     (equal subscription-token
                            (e-board-activation-subscription-token activation))))
        (setf (e-board-activation-state activation) 'cancelled)
        (unless suppress-event-p
          (e-board-admission-append-event board 'activation-cancelled
                                 (list :activation-id activation-id :reason reason)))))))

(defun e-board--cancel-subscription-replays-exact
    (board subscription-id subscription-token)
  "Remove queued replay records for one exact subscription lifetime.
Replay records retain copied subscription values so the board can recover from
durable history, but once their source lifetime is retired they must not
re-enter classification under a same-id replacement.  The retained event log
is intentionally untouched; only the pending process-local replay queue is
filtered here."
  (let (kept tail)
    (dolist (record (e-board-subscription-replays board))
      (let ((subscription (e-board-subscription-replay-subscription record)))
        (if (and (equal subscription-id
                        (e-board-subscription-id subscription))
                 (equal subscription-token
                        (e-board-subscription-lifetime-token subscription)))
            nil
          (let ((cell (list record)))
            (if tail
                (setcdr tail cell)
              (setq kept cell))
            (setq tail cell)))))
    (setf (e-board-subscription-replays board) kept
          (e-board-subscription-replay-tail board) tail)
    (unless kept
      (setf (e-board-subscription-replay-scheduled board) nil))
    kept))

(defun e-board-retire-subscription-exact
    (board subscription &optional terminal-state)
  "Retire exactly SUBSCRIPTION's current lifetime on BOARD.

This is the board owner operation for participant teardown.  It cancels
readiness/lifetime timers, clears continuation accumulators, fences queued
classifiers, prepared activations, replay snapshots, and deferred effects by
the subscription's immutable lifetime token, and finally removes only the
current id-table entry.  The ordered subscription/event history is retained.
The operation is idempotent: an absent exact entry is already retired, while a
different object holding the same id is a replacement and is never touched.
By default the process-local terminal state is `cancelled'.  BOARD close may
pass `inactive' to preserve its historical closed-board projection; both
states have the same exact callback fence and route-removal postcondition.
No durable transition event is synthesized for process-local teardown."
  (unless (and (e-board-p board) (e-board-subscription-p subscription))
    (signal 'wrong-type-argument
            (list '(e-board-p e-board-subscription-p) board subscription)))
  (unless (memq (or terminal-state 'cancelled) '(cancelled inactive))
    (signal 'wrong-type-argument
            (list '(member cancelled inactive) terminal-state)))
  (let* ((id (e-board-subscription-id subscription))
         (current (e-board-find-subscription board id)))
    (when (or (null current) (eq current subscription))
      (unless (memq (e-board-subscription-state subscription)
                    '(cancelled inactive))
        (if (e-board--subscription-transition-allowed-p
             (e-board-subscription-state subscription) 'cancelled)
            (e-board--transition-subscription board subscription 'cancelled t)
          ;; A completed/faulted/expired route has no legal ordinary
          ;; transition left, but participant teardown still needs one
          ;; unambiguous terminal state and the same callback fence.
          (setf (e-board-subscription-state subscription) 'cancelled)))
      ;; A terminal subscription may have reached this operation after a
      ;; previous partial retirement.  Re-run the owner-local cleanup so each
      ;; retry reaches the same postcondition.
      (dolist (timer (list (e-board-subscription-readiness-timer subscription)
                           (e-board-subscription-lifetime-timer subscription)))
        (when (timerp timer)
          (cancel-timer timer)))
      (setf (e-board-subscription-readiness-timer subscription) nil
            (e-board-subscription-lifetime-timer subscription) nil
            (e-board-subscription-accumulator subscription) nil
            (e-board-subscription-readiness-generation subscription)
            (1+ (or (e-board-subscription-readiness-generation subscription) 0))
            (e-board-subscription-lifetime-generation subscription)
            (1+ (or (e-board-subscription-lifetime-generation subscription) 0)))
      (e-board--cancel-prepared-activations
       board id 'subscription-retired
       (e-board-subscription-lifetime-token subscription)
       t)
      (e-board--cancel-subscription-replays-exact
       board id (e-board-subscription-lifetime-token subscription))
      (when (eq current subscription)
        (remhash id (e-board-subscription-id-table board)))
      (when (eq terminal-state 'inactive)
        (setf (e-board-subscription-state subscription) 'inactive))
      subscription)))

(defun e-board-set-subscription-state (board subscription-id state)
  "Transition an ordinary BOARD subscription to STATE.
The membership-owned exact address route is not mutable through this API; its
lifetime belongs to participant membership."
  (let ((subscription (e-board-find-subscription board subscription-id)))
    (unless subscription
      (signal 'e-board-error (list "Unknown subscription" subscription-id)))
    (when (e-board-subscription-built-in-p subscription)
      (signal 'e-board-error (list "Membership-owned subscription" subscription-id)))
    (e-board--transition-subscription board subscription state)))

(cl-defun e-board-replace-subscription
    (board subscription-id selector &key id (effect nil effect-supplied-p)
           (delivery nil delivery-supplied-p)
           (priority nil priority-supplied-p)
           (self-delivery nil self-delivery-supplied-p)
           (failure-policy nil failure-policy-supplied-p)
           (state 'active) (readiness nil readiness-supplied-p)
           (firing-limit nil firing-limit-supplied-p)
           (lifetime nil lifetime-supplied-p))
  "Cancel ordinary SUBSCRIPTION-ID and install a future-only replacement.
The replacement receives a fresh id by default, so captured classifier views
continue to name the old immutable subscription.  Replacing a terminal
subscription records the relationship without rewriting its terminal state."
  (let ((subscription (or (e-board-find-subscription board subscription-id)
                          (signal 'e-board-error
                                  (list "Unknown subscription" subscription-id)))))
    (when (e-board-subscription-built-in-p subscription)
      (signal 'e-board-error (list "Membership-owned subscription" subscription-id)))
    (let ((replacement-id (or id (e-board--next-id board 'subscription))))
      ;; Validate the replacement before changing the old subscription.
      (unless (listp selector)
        (signal 'wrong-type-argument (list 'listp selector)))
      (unless (memq state '(active muted))
        (signal 'wrong-type-argument (list '(member active muted) state)))
      (let* ((replacement-effect
              (if effect-supplied-p effect (e-board-subscription-effect subscription)))
             (replacement-delivery
              (if delivery-supplied-p delivery
                (e-board-subscription-delivery subscription)))
             (replacement-priority
              (if priority-supplied-p priority
                (e-board-subscription-priority subscription)))
             (replacement-self-delivery
              (if self-delivery-supplied-p self-delivery
                (e-board-subscription-self-delivery subscription)))
             (replacement-failure-policy
              (if failure-policy-supplied-p failure-policy
                (e-board-subscription-failure-policy subscription)))
             (replacement-readiness
             (if readiness-supplied-p readiness
                (e-board-subscription-readiness subscription)))
             (replacement-firing-limit
              (if firing-limit-supplied-p firing-limit
                (e-board-subscription-firing-limit subscription)))
             (replacement-lifetime
              (if lifetime-supplied-p lifetime
                (e-board-subscription-lifetime subscription))))
        (e-board--validate-subscription-id
         replacement-id (eq replacement-delivery 'process))
        (setq replacement-id
              (e-board--freeze-envelope-value
               replacement-id 'subscription-id e-board-message-metadata-byte-limit))
        (when (e-board-find-subscription board replacement-id)
          (signal 'e-board-id-conflict (list replacement-id)))
        (setq replacement-priority
              (if (and (eq replacement-delivery 'process)
                       (null replacement-priority))
                  0
                replacement-priority)
              replacement-failure-policy
              (if (and (eq replacement-delivery 'process)
                       (null replacement-failure-policy))
                  'pass
                replacement-failure-policy))
        (unless (or (eq replacement-effect 'create-pickup)
                    (and (listp replacement-effect)
                         (eq (car replacement-effect) :post-input)))
          (signal 'e-board-error
                  (list "Unsupported board effect" replacement-effect)))
        (e-board--validate-subscription-delivery
         replacement-effect replacement-delivery replacement-priority
         replacement-self-delivery replacement-failure-policy)
        (e-board--validate-continuation-readiness
         replacement-effect replacement-readiness)
        (when replacement-firing-limit
          (unless (and (integerp replacement-firing-limit)
                       (> replacement-firing-limit 0))
            (signal 'wrong-type-argument
                    (list 'plusp replacement-firing-limit)))
          (unless (and (listp replacement-effect)
                       (eq (car replacement-effect) :post-input))
            (signal 'e-board-error
                    (list "Firing limit requires post-input effect"
                          replacement-firing-limit))))
        (when replacement-lifetime
          (unless (and (numberp replacement-lifetime)
                       (> replacement-lifetime 0))
            (signal 'wrong-type-argument
                    (list 'plusp replacement-lifetime))))
        (unless (memq (e-board-subscription-state subscription)
                      '(completed faulted cancelled expired))
          (e-board--transition-subscription board subscription 'cancelled))
        (let ((replacement
               (e-board-subscribe
                board (e-board-subscription-participant-id subscription) selector
                :id replacement-id :state state :effect replacement-effect
                :delivery replacement-delivery :priority replacement-priority
                :self-delivery replacement-self-delivery
                :failure-policy replacement-failure-policy
                :readiness replacement-readiness
                :firing-limit replacement-firing-limit
                :lifetime replacement-lifetime)))
          (e-board-admission-append-event
           board 'subscription-replaced
           (list :subscription-id subscription-id
                 :replacement-id replacement-id))
          replacement)))))

(defun e-board--schedule-continuation-timer (board seconds callback)
  "Schedule CALLBACK after SECONDS without letting it apply a continuation."
  (if-let ((scheduler (e-board-continuation-timer-scheduler board)))
      (funcall scheduler seconds callback)
    (run-at-time seconds nil callback)))

(defun e-board--schedule-subscription-timer (board seconds callback)
  "Schedule a subscription lifecycle CALLBACK after SECONDS."
  (if-let ((scheduler (e-board-subscription-timer-scheduler board)))
      (funcall scheduler seconds callback)
    (run-at-time seconds nil callback)))

(defun e-board--schedule-subscription-replay (board)
  "Schedule one bounded retained-continuation replay drain for BOARD."
  (unless (e-board-subscription-replay-scheduled board)
    (setf (e-board-subscription-replay-scheduled board) t)
    (e-board-admission-schedule-effect
     board (lambda () (e-board-drain-subscription-replays board)))))

(defun e-board--queue-subscription-replay (board subscription start-seq)
  "Freeze SUBSCRIPTION and queue its explicit retained post-input replay.
START-SEQ is exclusive.  The captured high watermark isolates the replay from
ordinary future routing, which retains its existing append-time classifier."
  (let ((record (e-board-subscription-replay--create
                 :subscription (copy-e-board-subscription subscription)
                 :next-seq (1+ start-seq)
                 :through-seq (e-board-next-seq board))))
    (setf (e-board-subscription-replays board)
          (or (e-board-subscription-replays board) (list record)))
    (if-let ((tail (e-board-subscription-replay-tail board)))
        (let ((cell (list record)))
          (setcdr tail cell)
          (setf (e-board-subscription-replay-tail board) cell))
      (setf (e-board-subscription-replay-tail board)
            (e-board-subscription-replays board)))
    (e-board-admission-append-event
     board 'subscription-replay-requested
     (list :subscription-id (e-board-subscription-id subscription)
           :start-seq start-seq :through-seq (e-board-subscription-replay-through-seq record)))
    (e-board--schedule-subscription-replay board)))

(defun e-board-drain-subscription-replays (board)
  "Classify one bounded page of explicit retained post-input replays."
  (setf (e-board-subscription-replay-scheduled board) nil)
  (let ((remaining e-board-subscription-replay-drain-limit))
    (while (and (> remaining 0) (e-board-subscription-replays board))
      (let* ((record (car (e-board-subscription-replays board)))
             (next-seq (e-board-subscription-replay-next-seq record)))
        (if (> next-seq (e-board-subscription-replay-through-seq record))
            (progn
              (setf (e-board-subscription-replays board)
                    (cdr (e-board-subscription-replays board)))
              (unless (e-board-subscription-replays board)
                (setf (e-board-subscription-replay-tail board) nil))
              (e-board-admission-append-event
               board 'subscription-replay-complete
               (list :subscription-id
                     (e-board-subscription-id
                      (e-board-subscription-replay-subscription record)))))
          (setf (e-board-subscription-replay-next-seq record) (1+ next-seq))
          (when-let ((message (gethash next-seq (e-board-message-seq-table board))))
            (condition-case err
                (when (e-board--message-subscription-matches-p
                       board (e-board-subscription-replay-subscription record) message)
                  (e-board--accept-post-input-match
                   board (e-board-subscription-replay-subscription record) message))
              (error
               (e-board--fault-subscription
                board (e-board-subscription-replay-subscription record) err))))))
        (cl-decf remaining)))
    (when (e-board-subscription-replays board)
      (e-board--schedule-subscription-replay board)))

(defun e-board--queue-subscription-expiry
    (board subscription-id generation &optional subscription-token)
  "Queue one generation-fenced expiry transition outside the timer callback."
  (e-board-admission-schedule-effect
   board
   (lambda ()
     (when-let ((subscription
                 (e-board-find-subscription board subscription-id)))
       (when (and (= generation
                     (e-board-subscription-lifetime-generation subscription))
                  (or (null subscription-token)
                      (equal subscription-token
                             (e-board-subscription-lifetime-token subscription)))
                  (memq (e-board-subscription-state subscription) '(active muted)))
         (e-board--transition-subscription board subscription 'expired))))))

(defun e-board--fire-post-input (board subscription message-or-messages)
  "Reserve one post-input firing and schedule it from frozen matched records.
The firing reservation is part of the board reducer, so a bounded
FIRING-LIMIT fences later classifier work before it can create another effect."
  (when (eq (e-board-subscription-state subscription) 'active)
    (let ((firing-number (1+ (e-board-subscription-firing-number subscription))))
      (when (or (null (e-board-subscription-firing-limit subscription))
                (<= firing-number
                    (e-board-subscription-firing-limit subscription)))
        (setf (e-board-subscription-firing-number subscription) firing-number)
        (e-board--schedule-post-input board subscription message-or-messages
                                      firing-number)
        (when (and (e-board-subscription-firing-limit subscription)
                   (= firing-number
                      (e-board-subscription-firing-limit subscription)))
          (e-board--transition-subscription board subscription 'completed))
        firing-number))))

(defun e-board--flush-continuation-accumulator
    (board subscription-id generation &optional subscription-token)
  "Freeze the current matching source ids for one deferred continuation effect."
  (when-let ((subscription
              (e-board-find-subscription board subscription-id)))
    (when (and (eq (e-board-subscription-state subscription) 'active)
               (= generation (e-board-subscription-readiness-generation subscription))
               (or (null subscription-token)
                   (equal subscription-token
                          (e-board-subscription-lifetime-token subscription)))
               (e-board-subscription-accumulator subscription))
      (when-let ((timer (e-board-subscription-readiness-timer subscription)))
        (cancel-timer timer))
      (let ((message-ids (e-board-subscription-accumulator subscription)))
        (setf (e-board-subscription-readiness-timer subscription) nil
              (e-board-subscription-accumulator subscription) nil
              (e-board-subscription-readiness-generation subscription)
              (1+ (e-board-subscription-readiness-generation subscription)))
        (e-board--fire-post-input
         board subscription
         (mapcar (lambda (message-id) (e-board-message board message-id)) message-ids))))))

(defun e-board--queue-continuation-accumulator-flush
    (board subscription-id generation &optional subscription-token)
  "Queue a fenced continuation accumulator flush through BOARD's effect drain."
  (e-board-admission-schedule-effect
   board
   (lambda ()
     (e-board--flush-continuation-accumulator
      board subscription-id generation subscription-token))))

(defun e-board--accept-post-input-match (board frozen-subscription message)
  "Record MESSAGE for FROZEN-SUBSCRIPTION's current continuation policy.
Classifier snapshots decide matching, while the current subscription fences a
later mute, replacement, or cancellation before any accumulator mutation."
  (when-let ((subscription
              (e-board--subscription-lifetime-current-p
               board frozen-subscription)))
    (when (and (eq (e-board-subscription-state subscription) 'active)
               (e-board--authorize-classification
                board subscription message 'effect-preparation))
      (let ((readiness (e-board-subscription-readiness subscription)))
        (if (null readiness)
            (e-board--fire-post-input board subscription message)
          (pcase (plist-get readiness :policy)
            ('batch
             (let* ((generation (if (e-board-subscription-accumulator subscription)
                                    (e-board-subscription-readiness-generation subscription)
                                  (1+ (e-board-subscription-readiness-generation subscription))))
                    (accumulator
                       (append (e-board-subscription-accumulator subscription)
                               (list (e-board-message-id message))))
                      (count (plist-get readiness :count))
                      (max-delay (plist-get readiness :max-delay)))
               (setf (e-board-subscription-readiness-generation subscription) generation
                     (e-board-subscription-accumulator subscription) accumulator)
                 (cond
                  ((>= (length accumulator) count)
                   (e-board--flush-continuation-accumulator
                    board (e-board-subscription-id subscription) generation
                    (e-board-subscription-lifetime-token subscription)))
                  ((and max-delay
                        (null (e-board-subscription-readiness-timer subscription)))
                   (setf (e-board-subscription-readiness-timer subscription)
                         (e-board--schedule-continuation-timer
                          board max-delay
                          (lambda ()
                            (e-board--queue-continuation-accumulator-flush
                             board (e-board-subscription-id subscription) generation
                             (e-board-subscription-lifetime-token subscription)))))))))
            ('latest-after-quiet
             (let ((generation (1+ (e-board-subscription-readiness-generation subscription))))
               (when-let ((timer (e-board-subscription-readiness-timer subscription)))
                 (cancel-timer timer))
               (setf (e-board-subscription-readiness-generation subscription) generation
                     (e-board-subscription-accumulator subscription)
                     (list (e-board-message-id message))
                     (e-board-subscription-readiness-timer subscription)
                     (e-board--schedule-continuation-timer
                      board (plist-get readiness :quiet-period)
                      (lambda ()
                        (e-board--queue-continuation-accumulator-flush
                         board (e-board-subscription-id subscription) generation
                         (e-board-subscription-lifetime-token subscription)))))))))))))

(defun e-board--schedule-post-input (board subscription message-or-messages &optional firing-number)
  "Freeze and schedule SUBSCRIPTION's declarative post from matched records."
  (let* ((messages (if (listp message-or-messages)
                       message-or-messages
                     (list message-or-messages)))
         (message (car messages))
         (message-ids (mapcar #'e-board-message-id messages))
         (effect (cdr (e-board-subscription-effect subscription)))
         (attributes (copy-tree (plist-get effect :attributes)))
         (lineage (append (copy-sequence
                           (plist-get (e-board-message-attributes message)
                                      :board-subscription-lineage))
                          (list (e-board-subscription-id subscription))))
         (activation-id (if firing-number
                            (list (e-board-id board)
                                  (e-board-subscription-id subscription)
                                  'continuation firing-number)
                          (list (e-board-id board) (e-board-subscription-id subscription)
                                (e-board-message-id message))))
         (activation (e-board-activation--create
                      :id activation-id
                      :subscription-id (e-board-subscription-id subscription)
                      :subscription-token
                      (e-board-subscription-lifetime-token subscription)
                      :message-id (e-board-message-id message)
                      :effect 'post-input :state 'prepared)))
    (if (> (length lineage) e-board-max-derived-hops)
        (e-board-admission-append-event
         board 'effect-stopped
         (list :activation-id activation-id :reason 'causal-hop-limit))
      (puthash activation-id activation (e-board-activations board))
      (puthash (e-board-subscription-id subscription)
               (append (gethash (e-board-subscription-id subscription)
                                (e-board-activation-subscription-index board))
                       (list activation-id))
               (e-board-activation-subscription-index board))
      (e-board-admission-append-event
       board 'activation-prepared
       (list :activation-id activation-id
             :subscription-id (e-board-subscription-id subscription)
             :subscription-token
             (e-board-subscription-lifetime-token subscription)
             :effect 'post-input))
      (e-board-admission-schedule-effect
       board
       (lambda ()
         (when (and (eq (e-board-activation-state activation) 'prepared)
                    (e-board--subscription-effect-current-p board subscription))
           (setf (e-board-activation-state activation) 'applying)
           (e-board-admission-append-event
            board 'activation-applying (list :activation-id activation-id))
           (condition-case err
               (let ((publication
                      (e-board-post-input
                       board
                       :author (or (plist-get effect :author)
                                   (format "participant:%s"
                                           (e-board-subscription-participant-id subscription)))
                       :requester-actor
                       (list 'participant
                             (e-board-subscription-participant-id subscription))
                       :tags (copy-tree (plist-get effect :tags))
                       :attributes
                       (append attributes
                               (list :board-subscription-lineage lineage
                                     :board-subscription-source-message-ids message-ids))
                       :to (plist-get effect :to)
                       :mode (or (plist-get effect :mode) 'inject)
                       :content (plist-get effect :content)
                       :reference (plist-get effect :reference)
                       :source-input-key
                       (list (e-board-subscription-id subscription)
                             (or firing-number 1)
                             (e-board-message-seq message)))))
                 (setf (e-board-activation-state activation) 'committed)
                 (e-board-admission-append-event
                  board 'effect-committed
                  (list :activation-id activation-id :effect 'post-input
                        :message-id (and (e-board-publication-message publication)
                                         (e-board-message-id
                                          (e-board-publication-message publication))))))
             (error
              (setf (e-board-activation-state activation) 'failed)
              (e-board-admission-append-event
               board 'effect-failed
               (list :activation-id activation-id :error err))
       ))))))))

(defun e-board--source-key-parts (source-key)
  "Return SOURCE-KEY as (PRODUCER GENERATION SEQ), or signal.
Source identities are board-scoped producer/generation/monotonic-sequence
tuples.  Lists and vectors are accepted to keep adapters representation-neutral."
  (let ((parts (cond ((listp source-key) source-key)
                     ((vectorp source-key) (append source-key nil)))))
    (unless (and (= (length parts) 3)
                 (nth 0 parts) (nth 1 parts)
                 (integerp (nth 2 parts)) (>= (nth 2 parts) 0))
      (signal 'e-board-invalid-source-key (list source-key)))
    parts))

(defun e-board--source-publication (board kind source-key)
  "Return existing or expired publication status for BOARD KIND SOURCE-KEY.
Return nil when the key is new and may be appended."
  (when source-key
    (pcase-let* ((`(,producer ,generation ,sequence)
                  (e-board--source-key-parts source-key))
                 (recent-key (list kind producer generation sequence))
                 (watermark-key (list kind producer generation))
                 (existing (gethash recent-key (e-board-source-recent board)))
                 (watermark (gethash watermark-key
                                     (e-board-source-high-watermarks board))))
      (cond
       (existing
        (e-board-publication--create
         :status 'duplicate :message existing
         :pickup-ids (e-board-message-pickup-ids existing)))
       ((and watermark (<= sequence watermark))
        (e-board-publication--create :status 'source-history-expired))
       (t nil)))))

(defun e-board--remember-source (board kind source-key message)
  "Atomically retain SOURCE-KEY's MESSAGE and advance its high watermark."
  (when source-key
    (pcase-let ((`(,producer ,generation ,sequence)
                 (e-board--source-key-parts source-key)))
      (puthash (list kind producer generation sequence) message
               (e-board-source-recent board))
      (puthash (list kind producer generation) sequence
               (e-board-source-high-watermarks board)))))

(defun e-board--freeze-envelope-value (value field byte-limit)
  "Deep-copy VALUE for retained FIELD while enforcing BYTE-LIMIT.
The walk charges atomic payload bytes and one byte per container cell.  It
stops as soon as the field exceeds its budget, rejects cyclic structures, and
copies strings, conses, vectors, and hash tables so later caller mutation
cannot rewrite retained board state."
  (let ((remaining byte-limit)
        (visiting (make-hash-table :test 'eq)))
    (cl-labels
        ((charge (amount)
           (setq remaining (- remaining amount))
           (when (< remaining 0)
             (signal 'e-board-envelope-too-large
                     (list field byte-limit))))
         (copy-value (current)
           (cond
            ((stringp current)
             (charge (string-bytes current))
             (copy-sequence current))
            ((symbolp current)
             (charge (length (symbol-name current)))
             current)
            ((numberp current)
             (charge 16)
             current)
            ((consp current)
             (when (gethash current visiting)
               (signal 'e-board-invalid-envelope (list field 'cyclic-cons)))
             (charge 1)
             (puthash current t visiting)
             (unwind-protect
                 (cons (copy-value (car current))
                       (copy-value (cdr current)))
               (remhash current visiting)))
            ((vectorp current)
             (when (gethash current visiting)
               (signal 'e-board-invalid-envelope (list field 'cyclic-vector)))
             (charge (length current))
             (puthash current t visiting)
             (unwind-protect
                 (let ((copy (make-vector (length current) nil)))
                   (dotimes (index (length current))
                     (aset copy index (copy-value (aref current index))))
                   copy)
               (remhash current visiting)))
            ((hash-table-p current)
             (when (gethash current visiting)
               (signal 'e-board-invalid-envelope (list field 'cyclic-hash-table)))
             (charge (hash-table-count current))
             (puthash current t visiting)
             (unwind-protect
                 (let ((copy (make-hash-table :test (hash-table-test current)
                                              :size (hash-table-count current))))
                   (maphash (lambda (key item)
                              (puthash (copy-value key) (copy-value item) copy))
                            current)
                   copy)
               (remhash current visiting)))
            (t
             (signal 'e-board-invalid-envelope
                     (list field (type-of current)))))))
      (copy-value value))))

(defun e-board--make-message (board kind id author requester-actor
                                     tags attributes to mode content reference
                                     source-input-key source-output-key
                                     reply-to-message-ids caused-by-delivery-ids
                                     &optional source-activity-key source-fact-key
                                     subject-participant-id source-turn-id activity-kind
                                     created-at)
  "Create and record one immutable BOARD message, returning it."
  (when (e-board-message board id)
    (signal 'e-board-id-conflict (list id)))
  (let* ((frozen-author
          (e-board--freeze-envelope-value
           author 'author e-board-message-metadata-byte-limit))
         (frozen-requester
          (e-board--freeze-envelope-value
           requester-actor 'requester-actor e-board-message-metadata-byte-limit))
         (frozen-tags
          (e-board--freeze-envelope-value
           tags 'tags e-board-message-tags-byte-limit))
         (frozen-attributes
          (e-board--freeze-envelope-value
           attributes 'attributes e-board-message-attributes-byte-limit))
         (frozen-to
          (e-board--freeze-envelope-value
           to 'to e-board-message-metadata-byte-limit))
         (frozen-content
          (e-board--freeze-envelope-value
           content 'content e-board-message-content-byte-limit))
         (frozen-reference
          (e-board--freeze-envelope-value
           reference 'reference e-board-message-reference-byte-limit))
         (frozen-source-input-key
          (e-board--freeze-envelope-value
           source-input-key 'source-input-key e-board-message-metadata-byte-limit))
         (frozen-source-output-key
          (e-board--freeze-envelope-value
           source-output-key 'source-output-key e-board-message-metadata-byte-limit))
         (frozen-reply-to-message-ids
          (e-board--freeze-envelope-value
           reply-to-message-ids 'reply-to-message-ids
           e-board-message-metadata-byte-limit))
         (frozen-caused-by-delivery-ids
          (e-board--freeze-envelope-value
           caused-by-delivery-ids 'caused-by-delivery-ids
           e-board-message-metadata-byte-limit))
         (frozen-source-activity-key
          (e-board--freeze-envelope-value
           source-activity-key 'source-activity-key
           e-board-message-metadata-byte-limit))
         (frozen-source-fact-key
          (e-board--freeze-envelope-value
           source-fact-key 'source-fact-key e-board-message-metadata-byte-limit))
         (frozen-subject-participant-id
          (e-board--freeze-envelope-value
           subject-participant-id 'subject-participant-id
           e-board-message-metadata-byte-limit))
         (frozen-source-turn-id
          (e-board--freeze-envelope-value
           source-turn-id 'source-turn-id e-board-message-metadata-byte-limit))
         (event (e-board-admission-append-event
                 board (intern (format "%s-posted" kind))
                 (list :message-id id)))
         (message
          (e-board-message--create
           :id id :board-id (e-board-id board) :seq (e-board-event-seq event)
           :kind kind :author frozen-author :requester-actor frozen-requester
           :tags frozen-tags
           :attributes frozen-attributes :to frozen-to :mode mode
           :content frozen-content :reference frozen-reference
           :source-input-key frozen-source-input-key
           :source-output-key frozen-source-output-key
           :reply-to-message-ids frozen-reply-to-message-ids
           :caused-by-delivery-ids frozen-caused-by-delivery-ids
           :source-activity-key frozen-source-activity-key
           :source-fact-key frozen-source-fact-key
           :subject-participant-id frozen-subject-participant-id
           :source-turn-id frozen-source-turn-id
           :activity-kind activity-kind
           :created-at (or created-at (float-time))
           :routing-state (and (eq kind 'input) 'routing))))
    (puthash id message (e-board-message-table board))
    (puthash (e-board-message-seq message) message (e-board-message-seq-table board))
    (push message
          (gethash kind (e-board-message-kind-newest-table board)))
    (dolist (tag frozen-tags)
      (push message
            (gethash (cons kind tag)
                     (e-board-message-kind-tag-newest-table board))))
    (let* ((index (cl-incf (e-board-message-count board)))
           (cell (list message)))
      (if (e-board-messages-tail board)
          (setcdr (e-board-messages-tail board) cell)
        (setf (e-board-messages board) cell))
      (setf (e-board-messages-tail board) cell)
      (puthash index message (e-board-message-index-table board))
      (puthash (e-board-message-seq message) index
               (e-board-event-message-count board)))
    ;; Notification runs only after the immutable append.  Adapters must do no
    ;; more than enqueue one bounded wake token; they must not re-enter board
    ;; mutation here.
    (when-let ((notify (e-board-message-notification-function board)))
      (condition-case err
          (funcall notify board message)
        (error
         (e-board-admission-append-event
          board 'message-notification-failed
          (list :message-id (e-board-message-id message) :error err)))))
    message))

(defun e-board--authorize-classification (board subscription message phase)
  "Authorize SUBSCRIPTION's access to MESSAGE at deferred reducer PHASE."
  (if-let ((authorizer (e-board-classification-authorizer board)))
      (if (funcall authorizer subscription message phase)
          t
        (e-board-admission-append-event
         board 'authorization-revoked
         (list :subscription-id (e-board-subscription-id subscription)
               :message-id (e-board-message-id message) :phase phase))
        nil)
    t))

(defun e-board--message-subscription-matches-p (board subscription message)
  "Classify one frozen SUBSCRIPTION against MESSAGE in a later router turn.
Only input records may create pickups.  An explicit `:post-input' continuation
may instead match any unaddressed board record, including output, activity, and
fact publications."
  (cond
   ((eq (e-board-subscription-effect subscription) 'create-pickup)
    (and (eq (e-board-message-kind message) 'input)
         (e-board--eligible-subscription-p board subscription)
         (e-board--authorize-classification
          board subscription message 'selector)
         (if-let ((to (e-board-message-to message)))
             (and (e-board-subscription-built-in-p subscription)
                  (equal (e-board-subscription-participant-id subscription) to))
           (and (not (e-board-subscription-built-in-p subscription))
                (e-board--selector-matches-p board subscription message)))))
   ((and (not (e-board-message-to message))
         (eq (e-board-subscription-state subscription) 'active)
         (not (e-board-subscription-built-in-p subscription))
         (listp (e-board-subscription-effect subscription))
         (eq (car (e-board-subscription-effect subscription)) :post-input))
    (and (e-board--authorize-classification
          board subscription message 'selector)
         (let ((lineage (plist-get (e-board-message-attributes message)
                                   :board-subscription-lineage)))
           (and (not (member (e-board-subscription-id subscription) lineage))
                (e-board--selector-matches-p board subscription message)))))))

(defun e-board--pickup-cause-metadata (message)
  "Return MESSAGE's bounded causal fields for one logical pickup envelope."
  (let ((attributes (e-board-message-attributes message)))
    (list :reply-to-message-ids
          (copy-tree (e-board-message-reply-to-message-ids message))
          :caused-by-delivery-ids
          (copy-tree (e-board-message-caused-by-delivery-ids message))
          :source-input-key
          (copy-tree (e-board-message-source-input-key message))
          :routing-tags
          (copy-tree (e-board-message-tags message))
          :input-attributes
          (copy-tree attributes)
          :subscription-lineage
          (copy-tree (plist-get attributes :board-subscription-lineage))
          :source-message-ids
          (copy-tree
           (plist-get attributes :board-subscription-source-message-ids)))))

(defun e-board--copy-envelope-value (value)
  "Recursively copy mutable sequence storage in logical envelope VALUE."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value)
    (cons (e-board--copy-envelope-value (car value))
          (e-board--copy-envelope-value (cdr value))))
   ((vectorp value)
   (apply #'vector (mapcar #'e-board--copy-envelope-value value)))
   (t value)))

(defun e-board-copy-envelope-value (value)
  "Return a detached copy of the logical envelope VALUE.

This is the narrow boundary for adapters that need to retain a board-owned
payload without sharing mutable sequence storage."
  (e-board--copy-envelope-value value))

(defun e-board--classification-append-match (record subscription post-p)
  "Append frozen SUBSCRIPTION to RECORD's ordinary or POST-P match FIFO."
  (let ((cell (list subscription)))
    (if post-p
        (progn
          (if (e-board-input-classification-post-subscriptions-tail record)
              (setcdr (e-board-input-classification-post-subscriptions-tail record)
                      cell)
            (setf (e-board-input-classification-post-subscriptions record) cell))
          (setf (e-board-input-classification-post-subscriptions-tail record) cell))
      (if (e-board-input-classification-matches-tail record)
          (setcdr (e-board-input-classification-matches-tail record) cell)
        (setf (e-board-input-classification-matches record) cell))
      (setf (e-board-input-classification-matches-tail record) cell))))

(defun e-board--classification-append-participant (record participant-id)
  "Append PARTICIPANT-ID to RECORD's unique bounded fan-out FIFO."
  (let ((cell (list participant-id)))
    (if (e-board-input-classification-participant-ids-tail record)
        (setcdr (e-board-input-classification-participant-ids-tail record) cell)
      (setf (e-board-input-classification-participant-ids record) cell))
    (setf (e-board-input-classification-participant-ids-tail record) cell)
    (cl-incf (e-board-input-classification-participant-count record))))

(defun e-board--classification-route-current-p (board route)
  "Return non-nil when ROUTE still owns its exact board lifetimes."
  (let* ((subscription (e-board-classification-route-subscription route))
         (subscription-token
          (e-board-classification-route-subscription-token route))
         (participant (e-board-classification-route-participant route))
         (participant-id (and participant
                              (e-board-participant-id participant)))
         (current-subscription
          (and subscription
               (e-board-find-subscription
                board (e-board-subscription-id subscription))))
         (current-participant
          (and participant-id
               (e-board-participant board participant-id))))
    (and (e-board-classification-route-p route)
         (e-board-subscription-p subscription)
         (e-board-participant-p participant)
         (eq current-subscription subscription)
         (equal (e-board-subscription-lifetime-token subscription)
                subscription-token)
         (equal (e-board-subscription-participant-id subscription)
                participant-id)
         (eq (e-board-subscription-state subscription) 'active)
         (eq current-participant participant)
         (memq (e-board-participant-state participant)
               '(active dormant stale)))))

(defun e-board--classification-capture-route (board subscription)
  "Capture SUBSCRIPTION's exact participant lifetime for deferred routing.
The participant lookup intentionally happens at authorize time.  A later
group/prepare/commit phase may only validate this captured object; it must not
rediscover a same-id participant that was admitted after authorization."
  (let ((participant
         (e-board-participant board
                              (e-board-subscription-participant-id
                               subscription))))
    (when participant
      (e-board-classification-route--create
       :subscription subscription
       :subscription-token
       (copy-tree (e-board-subscription-lifetime-token subscription))
       :participant participant))))

(defun e-board--classification-bucket-current-p (board participant-id bucket)
  "Return non-nil when BUCKET's exact route entries remain current."
  (and (e-board-subscription-bucket-p bucket)
       (e-board-subscription-bucket-routes bucket)
       (cl-every
        (lambda (route)
          (and (equal participant-id
                      (e-board-subscription-participant-id
                       (e-board-classification-route-subscription route)))
               (e-board--classification-route-current-p board route)))
        (e-board-subscription-bucket-routes bucket))))

(defun e-board--classification-routes-current-p (board record)
  "Return non-nil when every grouped pickup route still owns its lifetimes."
  (catch 'e-board-classification-route-invalid
    (maphash
     (lambda (participant-id bucket)
       (unless (e-board--classification-bucket-current-p
                board participant-id bucket)
         (throw 'e-board-classification-route-invalid nil)))
     (e-board-input-classification-by-participant record))
    t))

(defun e-board--classification-clear-prepared-pickups (record)
  "Discard RECORD's uncommitted pickup set after a route lifetime change."
  (setf (e-board-input-classification-prepared-pickups record) nil
        (e-board-input-classification-prepared-pickups-tail record) nil
        (e-board-input-classification-pickup-ids record) nil
        (e-board-input-classification-pickup-ids-tail record) nil))

(defun e-board--classification-group-subscription
    (board record route)
  "Group authorized ROUTE with exact participant/lifetime authority.
ROUTE is captured by the authorize phase.  Keeping this function route-shaped
prevents a grouped classifier from silently selecting a replacement object by
the subscription or participant id."
  (let* ((subscription
          (e-board-classification-route-subscription route))
         (participant-id
          (and subscription
               (e-board-subscription-participant-id subscription)))
         (table (e-board-input-classification-by-participant record))
         (bucket (gethash participant-id table)))
    ;; The current subscription check alone is insufficient: membership may
    ;; have been retired and replaced between selector matching and this
    ;; deferred grouping phase.  Do not create a bucket without the exact
    ;; participant object that authorized the route.
    (unless (and route
                 (eq route
                     (gethash (e-board-subscription-id subscription)
                              (e-board-input-classification-authorized-routes
                               record)))
                 (e-board--classification-route-current-p board route))
      (setf (e-board-input-classification-overflow-reason record)
            'subscription-lifetime-changed))
    (unless (e-board-input-classification-overflow-reason record)
      (unless bucket
        (if (>= (e-board-input-classification-participant-count record)
                e-board-input-fanout-limit)
            (setf (e-board-input-classification-overflow-reason record)
                  'fanout-limit-exceeded)
          (setq bucket (e-board-subscription-bucket--create :count 0))
          (puthash participant-id bucket table)
          (e-board--classification-append-participant record participant-id)))
      (when bucket
        (if (>= (e-board-subscription-bucket-count bucket)
                e-board-pickup-subscription-limit)
            (setf (e-board-input-classification-overflow-reason record)
                  'subscription-clause-limit-exceeded)
          (let* ((route-cell (list route))
                 (id-cell (list (e-board-subscription-id subscription))))
            (if (e-board-subscription-bucket-tail bucket)
                (setcdr (e-board-subscription-bucket-tail bucket) id-cell)
              (setf (e-board-subscription-bucket-ids bucket) id-cell))
            (setf (e-board-subscription-bucket-tail bucket) id-cell)
            (if (e-board-subscription-bucket-routes-tail bucket)
                (setcdr (e-board-subscription-bucket-routes-tail bucket)
                        route-cell)
              (setf (e-board-subscription-bucket-routes bucket) route-cell))
            (setf (e-board-subscription-bucket-routes-tail bucket) route-cell)
            (cl-incf (e-board-subscription-bucket-count bucket))))))))

(defun e-board--classification-prepare-pickup (board record participant-id)
  "Prepare PARTICIPANT-ID's pickup off-registry for RECORD."
  (let* ((message (e-board-input-classification-message record))
         (bucket (gethash participant-id
                          (e-board-input-classification-by-participant record)))
         (valid-p (e-board--classification-bucket-current-p
                   board participant-id bucket)))
    (if (not valid-p)
        (progn
          (setf (e-board-input-classification-overflow-reason record)
                'subscription-lifetime-changed)
          (e-board--classification-clear-prepared-pickups record))
      (let* ((route (car (e-board-subscription-bucket-routes bucket)))
             (participant
              (e-board-classification-route-participant route))
             (delivery-id (list (e-board-id board)
                                (e-board-message-id message)
                                participant-id))
         (pickup
          (e-board-pickup--create
           :delivery-id delivery-id :board-id (e-board-id board)
           :participant-id participant-id :message-id (e-board-message-id message)
           :subscription-ids (copy-sequence (e-board-subscription-bucket-ids bucket))
           :participant-lifetime participant
           :event-seq-range (list (e-board-message-seq message)
                                  (e-board-message-seq message))
           :mode (e-board-message-mode message)
           :requester-actor
           (e-board--copy-envelope-value (e-board-message-requester-actor message))
           :addressed-p (and (e-board-message-to message) t)
           :cause-metadata
           (e-board--copy-envelope-value (e-board--pickup-cause-metadata message))
           :content (e-board--copy-envelope-value (e-board-message-content message))
           :reference
           (e-board--copy-envelope-value (e-board-message-reference message))))
           (pickup-cell (list pickup))
           (id-cell (list delivery-id)))
        (if (e-board-input-classification-prepared-pickups-tail record)
            (setcdr (e-board-input-classification-prepared-pickups-tail record)
                    pickup-cell)
          (setf (e-board-input-classification-prepared-pickups record)
                pickup-cell))
        (setf (e-board-input-classification-prepared-pickups-tail record)
              pickup-cell)
        (if (e-board-input-classification-pickup-ids-tail record)
            (setcdr (e-board-input-classification-pickup-ids-tail record)
                    id-cell)
          (setf (e-board-input-classification-pickup-ids record) id-cell))
        (setf (e-board-input-classification-pickup-ids-tail record) id-cell)))))

(defun e-board--classification-commit-routing (board record)
  "Atomically expose RECORD's fixed-cap pickup set and routing projection."
  (let* ((message (e-board-input-classification-message record))
         (publication (e-board-input-classification-publication record))
         (participant-ids (e-board-input-classification-participant-ids record))
         (pickup-ids (e-board-input-classification-pickup-ids record))
         (overflow (e-board-input-classification-overflow-reason record)))
    ;; Revalidate immediately before the atomic pickup set becomes visible.  A
    ;; subscription or participant may have retired after prepare-pickups;
    ;; invalidating the whole transaction preserves fan-out atomicity and keeps
    ;; a same-id replacement out of the old classifier.
    (unless overflow
      (unless (e-board--classification-routes-current-p board record)
        (setq overflow 'subscription-lifetime-changed)
        (setf (e-board-input-classification-overflow-reason record) overflow)
        (e-board--classification-clear-prepared-pickups record)
        (setq participant-ids nil
              pickup-ids nil)))
    (when overflow
      (setq participant-ids nil
            pickup-ids nil))
    (cond
     (overflow
      (setf (e-board-message-unrouted-reason message) overflow
            (e-board-message-routing-state message) 'routing-failed)
      (e-board-admission-append-event board 'input-routing-failed
                             (list :message-id (e-board-message-id message)
                                   :reason overflow)))
     ((null participant-ids)
      (let ((reason (if (e-board-message-to message)
                        'target-unavailable
                      'no-matching-subscription)))
        (setf (e-board-message-unrouted-reason message) reason
              (e-board-message-routing-state message) 'unrouted)
        (e-board-admission-append-event board 'input-unrouted
                               (list :message-id (e-board-message-id message)
                                     :reason reason))))
     (t
      ;; The fixed fan-out cap makes this publication transaction a constant
      ;; upper bound while keeping every prepared pickup invisible until here.
      (dolist (pickup (e-board-input-classification-prepared-pickups record))
        (puthash (e-board-pickup-delivery-id pickup) pickup (e-board-pickups board))
        (e-board--enqueue-pickup board pickup))
      (setf (e-board-message-matching-participant-ids message) participant-ids
            (e-board-message-pickup-ids message) pickup-ids
            (e-board-message-routing-state message) 'routed)
      (e-board-admission-append-event board 'input-routed
                             (list :message-id (e-board-message-id message)
                                   :participant-ids participant-ids
                                   :pickup-ids pickup-ids))))
    (let ((cell (list (list (e-board-message-id message) pickup-ids))))
      (if (e-board-routed-pickup-results-tail board)
          (setcdr (e-board-routed-pickup-results-tail board) cell)
        (setf (e-board-routed-pickup-results board) cell))
      (setf (e-board-routed-pickup-results-tail board) cell))
    (setf (e-board-publication-pickup-ids publication) pickup-ids)))

(defun e-board--advance-input-finalization (board record)
  "Advance RECORD by one bounded finalization unit; return non-nil when done."
  (let ((message (e-board-input-classification-message record)))
    (pcase (e-board-input-classification-phase record)
      ('nil
       (setf (e-board-input-classification-phase record) 'authorize-pickups
             (e-board-input-classification-cursor record)
             (e-board-input-classification-matches record)
             (e-board-input-classification-by-participant record)
             (make-hash-table :test 'equal)
             (e-board-input-classification-participant-count record) 0)
       nil)
      ('authorize-pickups
       (if-let ((cursor (e-board-input-classification-cursor record)))
           (let ((subscription (car cursor)))
             (setf (e-board-input-classification-cursor record) (cdr cursor))
             (when-let ((current
                         (e-board--classification-subscription-current-p
                          board subscription)))
               ;; Capture the participant object before invoking the external
               ;; authorization hook.  If that hook retires or replaces the
               ;; route, later phases retain this object and reject it rather
               ;; than resolving a same-id replacement.
               (let ((route
                      (e-board--classification-capture-route board current)))
                 (when (and route
                            (e-board--authorize-classification
                             board current message 'pickup-finalization))
                   (puthash (e-board-subscription-id current) route
                            (e-board-input-classification-authorized-routes
                             record))
                   (when (eq (e-board-message-kind message) 'input)
                     (e-board--classification-group-subscription
                      board record route))))))
         (if (eq (e-board-message-kind message) 'input)
             (setf (e-board-input-classification-phase record) 'prepare-pickups
                   (e-board-input-classification-cursor record)
                   (and (not (e-board-input-classification-overflow-reason record))
                        (e-board-input-classification-participant-ids record)))
           (setf (e-board-input-classification-phase record) 'authorize-effects
                 (e-board-input-classification-cursor record)
                 (e-board-input-classification-post-subscriptions record))))
       nil)
      ('prepare-pickups
       (if-let ((cursor (e-board-input-classification-cursor record)))
           (progn
             (setf (e-board-input-classification-cursor record) (cdr cursor))
             (unless (e-board-input-classification-overflow-reason record)
               (e-board--classification-prepare-pickup
                board record (car cursor))))
         (setf (e-board-input-classification-phase record) 'commit-routing))
       nil)
      ('commit-routing
       (e-board--classification-commit-routing board record)
       (setf (e-board-input-classification-phase record) 'authorize-effects
             (e-board-input-classification-cursor record)
             (e-board-input-classification-post-subscriptions record))
       nil)
      ('authorize-effects
       (if-let ((cursor (e-board-input-classification-cursor record)))
           (let ((subscription (car cursor)))
             (setf (e-board-input-classification-cursor record) (cdr cursor))
             (when-let ((current
                         (e-board--classification-subscription-current-p
                          board subscription)))
               (when (e-board--authorize-classification
                      board current message 'effect-finalization)
                 (e-board--accept-post-input-match board current message))))
         (setf (e-board-input-classification-phase record) 'done))
       nil)
      ('done t))))

(defun e-board--schedule-input-classification (board)
  "Schedule BOARD's frozen input classifier once after append returns."
  (unless (e-board-input-classification-scheduled board)
    (setf (e-board-input-classification-scheduled board) t)
    (if-let ((scheduler (e-board-input-classification-scheduler board)))
        (funcall scheduler (lambda () (e-board-drain-input-classifications board)))
      (run-at-time 0 nil (lambda () (e-board-drain-input-classifications board))))))

(defun e-board--queue-input-classification (board message publication)
  "Freeze BOARD's subscription view for MESSAGE without matching on append.
The legacy name reflects its original pickup-only caller; non-input records use
the same bounded queue solely to classify explicit continuation subscriptions."
  (let ((cell
         (list (e-board-input-classification--create
                :message message
                :subscription-count (e-board-subscription-count board)
                :index 0 :publication publication
                :matches nil :post-subscriptions nil
                :authorized-routes (make-hash-table :test 'equal)))))
    (if (e-board-input-classification-tail board)
        (setcdr (e-board-input-classification-tail board) cell)
      (setf (e-board-input-classifications board) cell))
    (setf (e-board-input-classification-tail board) cell))
  (e-board-state-adjust-unsettled board 'routing 1)
  (e-board--schedule-input-classification board))

(defun e-board--fail-input-classification (board record err)
  "Stop RECORD before pickup commit after a core classifier ERR."
  (let ((message (e-board-input-classification-message record)))
    (if (eq (e-board-message-kind message) 'input)
        (progn
          (setf (e-board-message-routing-state message) 'routing-failed)
          (e-board-admission-append-event
           board 'input-routing-failed
           (list :message-id (e-board-message-id message) :error err)))
      (e-board-admission-append-event
       board 'continuation-classification-failed
       (list :message-id (e-board-message-id message) :error err)))
    (setf (e-board-input-classifications board)
          (cdr (e-board-input-classifications board)))
    (e-board-state-adjust-unsettled board 'routing -1)
    (unless (e-board-input-classifications board)
      (setf (e-board-input-classification-tail board) nil))))

(defun e-board-drain-input-classifications (board)
  "Classify a bounded page of frozen input subscriptions in board order."
  (setf (e-board-input-classification-scheduled board) nil)
  (let ((remaining e-board-input-classification-drain-limit))
    (while (and (> remaining 0) (e-board-input-classifications board))
      (let* ((record (car (e-board-input-classifications board)))
             (subscription-count
              (e-board-input-classification-subscription-count record))
             (index (e-board-input-classification-index record))
             (message (e-board-input-classification-message record)))
        (if (eq (e-board-message-routing-state message) 'routing-cancelled)
            (progn
              (setf (e-board-input-classifications board)
                    (cdr (e-board-input-classifications board)))
              (e-board-state-adjust-unsettled board 'routing -1)
              (unless (e-board-input-classifications board)
                (setf (e-board-input-classification-tail board) nil)))
          (if (< index subscription-count)
            (let ((subscription
                   (copy-e-board-subscription
                    (gethash (1+ index)
                             (e-board-subscription-index-table board)))))
              (setf (e-board-input-classification-index record) (1+ index))
              (condition-case err
                  (when (e-board--message-subscription-matches-p board subscription message)
                    (if (eq (e-board-subscription-effect subscription) 'create-pickup)
                        (e-board--classification-append-match record subscription nil)
                      (e-board--classification-append-match record subscription t)))
                (error
                 (e-board--fail-input-classification board record err))))
          (condition-case err
              (when (e-board--advance-input-finalization board record)
                (setf (e-board-input-classifications board)
                      (cdr (e-board-input-classifications board)))
                (e-board-state-adjust-unsettled board 'routing -1)
                (unless (e-board-input-classifications board)
                  (setf (e-board-input-classification-tail board) nil)))
            (error (e-board--fail-input-classification board record err)))))
        (cl-decf remaining)))
    (when (e-board-input-classifications board)
      (e-board--schedule-input-classification board))))

(defun e-board-drain-routed-pickups (board)
  "Return and clear finalized pickup ids for separately scheduled delivery."
  (prog1 (e-board-routed-pickup-results board)
    (setf (e-board-routed-pickup-results board) nil
          (e-board-routed-pickup-results-tail board) nil)))

(defun e-board-cancel-input-routing (board message-id &optional reason)
  "Cancel nonterminal routing for retained input MESSAGE-ID on BOARD."
  (let ((message (e-board-message board message-id)))
    (when (eq (e-board-message-routing-state message) 'routing)
      (setf (e-board-message-routing-state message) 'routing-cancelled
            (e-board-message-unrouted-reason message) (or reason 'cancelled))
      (e-board-admission-append-event
       board 'input-routing-cancelled
       (list :message-id message-id :reason (or reason 'cancelled))))
    message))

(cl-defun e-board-post-input
    (board &key id author requester-actor tags attributes to (mode 'inject)
           content reference source-input-key)
  "Append one input message and queue its routing, returning a publication.
With TO, only its participant's built-in address subscription is considered.
Without TO, active ordinary tag subscriptions receive one frozen pickup each.
SOURCE-INPUT-KEY retries return the existing message; old or out-of-order keys
return status `source-history-expired' without appending or routing again."
  (unless (memq mode '(inject queue))
    (signal 'wrong-type-argument (list '(member inject queue) mode)))
  (or (e-board--source-publication board 'input source-input-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message (e-board--make-message
                        board 'input id author requester-actor
                        tags attributes to mode content reference
                       source-input-key nil nil nil))
             (publication (e-board-publication--create
                           :status 'posted :message message :pickup-ids nil)))
        (e-board--remember-source board 'input source-input-key message)
        (e-board--queue-input-classification board message publication)
        publication)))

(cl-defun e-board-post-output
    (board &key id author tags content reference source-output-key
           subject-participant-id source-turn-id
           reply-to-message-ids caused-by-delivery-ids)
  "Append one non-routable output message and return an `e-board-publication'.
SOURCE-OUTPUT-KEY is required because output publication retries must be
at-most-once.  Outputs never create participant pickups."
  (unless source-output-key
    (signal 'e-board-invalid-source-key (list source-output-key)))
  (when (or subject-participant-id source-turn-id)
    (unless (and (stringp subject-participant-id)
                 (equal author (format "participant:%s" subject-participant-id))
                 source-turn-id)
      (signal 'e-board-invalid-activity
              (list :author author :subject-participant-id subject-participant-id
                    :source-turn-id source-turn-id))))
  (or (e-board--source-publication board 'output source-output-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message (e-board--make-message
                        board 'output id author nil tags nil nil nil content reference nil
                       source-output-key reply-to-message-ids
                       caused-by-delivery-ids nil nil subject-participant-id
                       source-turn-id nil)))
        (e-board--remember-source board 'output source-output-key message)
        (e-board--close-open-activity
         board subject-participant-id source-turn-id message)
        (let ((publication (e-board-publication--create
                            :status 'posted :message message :pickup-ids nil)))
          (e-board--queue-input-classification board message publication)
          publication))))

(cl-defun e-board-post-activity
    (board &key id author subject-participant-id source-turn-id activity-kind
           tags attributes content reference source-activity-key
           reply-to-message-ids caused-by-delivery-ids)
  "Append one source-keyed, observation-only participant activity message.
Activity is never pickup-eligible: a later continuation may react to it, but
an activity tag by itself cannot re-enter a participant inbox."
  (unless source-activity-key
    (signal 'e-board-invalid-source-key (list source-activity-key)))
  (unless (and (stringp subject-participant-id)
               (equal author (format "participant:%s" subject-participant-id))
               source-turn-id activity-kind)
    (signal 'e-board-invalid-activity
            (list :author author :subject-participant-id subject-participant-id
                  :source-turn-id source-turn-id :activity-kind activity-kind)))
  (or (e-board--source-publication board 'activity source-activity-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message
              (e-board--make-message
               board 'activity id author nil tags attributes nil nil content reference
               nil nil reply-to-message-ids caused-by-delivery-ids
               source-activity-key nil subject-participant-id source-turn-id
               activity-kind)))
        (e-board--remember-source board 'activity source-activity-key message)
        (e-board--record-open-activity board message)
        (let ((publication (e-board-publication--create
                            :status 'posted :message message :pickup-ids nil)))
          (e-board--queue-input-classification board message publication)
          publication))))

(cl-defun e-board-post-fact
    (board &key id author tags attributes content reference source-fact-key)
  "Append one source-keyed, observation-only board fact message."
  (unless source-fact-key
    (signal 'e-board-invalid-source-key (list source-fact-key)))
  (or (e-board--source-publication board 'fact source-fact-key)
      (let* ((id (or id (e-board--next-id board 'message)))
             (message
              (e-board--make-message
               board 'fact id author nil tags attributes nil nil content reference
               nil nil nil nil nil source-fact-key nil nil nil)))
        (e-board--remember-source board 'fact source-fact-key message)
        (let ((publication (e-board-publication--create
                            :status 'posted :message message :pickup-ids nil)))
          (e-board--queue-input-classification board message publication)
          publication))))

(defun e-board-import-message (board envelope)
  "Restore one historical ENVELOPE into BOARD without routing or delivery.
The restored message receives a fresh process-local sequence while preserving
its durable source identity fields and order in the imported stream."
  (let* ((kind (plist-get envelope :kind))
         (id (plist-get envelope :id))
         (message
          (e-board--make-message
           board kind id
           (plist-get envelope :author)
           (plist-get envelope :requester-actor)
           (plist-get envelope :tags)
           (plist-get envelope :attributes)
           (plist-get envelope :to)
           (plist-get envelope :mode)
           (plist-get envelope :content)
           (plist-get envelope :reference)
           (plist-get envelope :source-input-key)
           (plist-get envelope :source-output-key)
           (plist-get envelope :reply-to-message-ids)
           (plist-get envelope :caused-by-delivery-ids)
           (plist-get envelope :source-activity-key)
           (plist-get envelope :source-fact-key)
           (plist-get envelope :subject-participant-id)
           (plist-get envelope :source-turn-id)
           (plist-get envelope :activity-kind)
           (plist-get envelope :created-at))))
    (e-board--reserve-imported-fallback-message-id board id)
    (setf (e-board-message-routing-state message)
          (and (eq kind 'input)
               (or (plist-get envelope :routing-state) 'historical))
          (e-board-message-unrouted-reason message)
          (plist-get envelope :unrouted-reason)
          (e-board-message-matching-participant-ids message)
          (copy-tree (plist-get envelope :matching-participant-ids))
          (e-board-message-pickup-ids message) nil)
    message))

(defun e-board-unrouted-inputs (board)
  "Return retained BOARD input messages that have a visible unrouted reason."
  (cl-remove-if-not
   (lambda (message)
     (and (eq (e-board-message-kind message) 'input)
          (e-board-message-unrouted-reason message)))
   (e-board-messages board)))

(provide 'e-board)

;;; e-board.el ends here

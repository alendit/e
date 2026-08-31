;;; e-board-admission.el --- Board admission and receipt owner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This is the Board domain's private admission owner.  It contains only exact
;; receipts, queue/link progress, and the bounded inverse protocol.  Semantic
;; routing, observer, and settlement policy remains in `e-board'.  The module
;; depends only on the lower Board state/value contract, so it can be loaded
;; and exercised without loading the facade.

;;; Code:

(require 'cl-lib)
(require 'e-board-state)
(require 'e-work)

(defconst e-board-effect-drain-limit 16
  "Maximum frozen effects applied in one scheduler turn.

The admission owner consumes this stable bound; the historical Board variable
name remains the public setting so existing callers retain their behavior.")

(cl-defstruct (e-board-event-receipt
               (:constructor e-board-event-receipt--create)
               (:conc-name e-board-event-receipt--))
  "Exact receipt for one event append and its resumable inverse."
  board event cell previous previous-node root-node next-node prefix-value
  forward-stage inverse-stage registered-p linked-p head-p tail-p count-p
  prefix-p removed-p)

(cl-defstruct (e-board-index-receipt
               (:constructor e-board-index-receipt--create)
               (:conc-name e-board-index-receipt--))
  "Exact receipt for one work-index queue cell."
  index work-id subscription-id queue cell previous previous-node root-node next-node
  queue-created-p forward-stage inverse-stage registered-p linked-p head-p
  tail-p mapped-p removed-p)

(cl-defstruct (e-board-terminal-classification-receipt
               (:constructor e-board-terminal-classification-receipt--create)
               (:conc-name e-board-terminal-classification-receipt--))
  "Exact receipt for one terminal-classifier queue cell."
  board record cell previous previous-receipt next-receipt
  invocation-index invocation-index-value aggregation-index aggregation-index-value
  invocation-index-detached-p aggregation-index-detached-p
  forward-stage inverse-stage head-p tail-p registered-p linked-p queued-p counted-p
  scheduled-p removed-p)

(cl-defstruct (e-board-aggregation-deadline-receipt
               (:constructor e-board-aggregation-deadline-receipt--create)
               (:conc-name e-board-aggregation-deadline-receipt--))
  "Exact receipt for one aggregation-deadline queue cell."
  board aggregation cell previous previous-receipt next-receipt
  forward-stage inverse-stage head-p tail-p registered-p linked-p queued-p counted-p
  scheduled-p removed-p)

(cl-defstruct (e-board-effect-receipt
               (:constructor e-board-effect-receipt--create)
               (:conc-name e-board-effect-receipt--))
  "Exact receipt for an effect queued by a pending aggregation admission."
  board effect cell previous previous-receipt next-receipt
  generation forward-stage inverse-stage head-p tail-p linked-p queued-p
  registered-p scheduled-p callback-accepted-p removed-p)

(cl-defstruct (e-board-work-admission
               (:constructor e-board-work-admission--create)
               (:conc-name e-board-work-admission-))
  "Opaque exact identity for one staged Board work admission."
  board handle work invocation invocation-id effect-target
  event-receipts index-receipts classification-receipts effect-receipts
  cleanup-complete-p committed-p aborted-p in-flight-p cancel-requested-p
  finalized-p work-owned-p)

(cl-defstruct (e-board-aggregation-admission
               (:constructor e-board-aggregation-admission--create)
               (:conc-name e-board-aggregation-admission-))
  "Opaque exact inverse for one staged aggregation admission."
  board aggregation event-receipts index-receipts timer deadline-receipt
  classification-receipts effect-receipts activation map-installed-p
  committed-p aborted-p cleanup-complete-p in-flight-p cancel-requested-p
  finalized-p)

(defun e-board-admission--admission-p (value)
  "Return non-nil when VALUE is a Board admission token."
  (or (e-board-work-admission-p value)
      (e-board-aggregation-admission-p value)))

(defun e-board-admission--adjust-unsettled (board class delta)
  "Adjust BOARD's low-level unsettled state through the state contract."
  (e-board-state-adjust-unsettled board class delta))

(defun e-board-admission--receipts (admission slot)
  "Return ADMISSION's receipt list named by SLOT."
  (if (e-board-work-admission-p admission)
      (pcase slot
        ('event (e-board-work-admission-event-receipts admission))
        ('index (e-board-work-admission-index-receipts admission))
        ('classification (e-board-work-admission-classification-receipts admission))
        ('effect (e-board-work-admission-effect-receipts admission)))
    (pcase slot
      ('event (e-board-aggregation-admission-event-receipts admission))
      ('index (e-board-aggregation-admission-index-receipts admission))
      ('classification
       (e-board-aggregation-admission-classification-receipts admission))
      ('effect (e-board-aggregation-admission-effect-receipts admission)))))

(defun e-board-admission--push-receipt (admission slot receipt)
  "Attach RECEIPT to ADMISSION's exact rollback list."
  (when (e-board-admission--admission-p admission)
    (pcase slot
      ('event
       (if (e-board-work-admission-p admission)
           (push receipt (e-board-work-admission-event-receipts admission))
         (push receipt (e-board-aggregation-admission-event-receipts admission))))
      ('index
       (if (e-board-work-admission-p admission)
           (push receipt (e-board-work-admission-index-receipts admission))
         (push receipt (e-board-aggregation-admission-index-receipts admission))))
      ('classification
       (if (e-board-work-admission-p admission)
           (push receipt
                 (e-board-work-admission-classification-receipts admission))
         (push receipt
               (e-board-aggregation-admission-classification-receipts admission))))
      ('effect
       (if (e-board-work-admission-p admission)
           (push receipt (e-board-work-admission-effect-receipts admission))
         (push receipt (e-board-aggregation-admission-effect-receipts admission))))))
  receipt)

(defun e-board-admission-begin (board admission)
  "Register ADMISSION before a Board-owned visible mutation."
  (when (and (e-board-p board) (e-board-admission--admission-p admission))
    (setf (e-board-pending-admissions board)
          (or (e-board-pending-admissions board)
              (make-hash-table :test 'eq)))
    (puthash admission t (e-board-pending-admissions board))
    (if (e-board-work-admission-p admission)
        (setf (e-board-work-admission-in-flight-p admission) t)
      (setf (e-board-aggregation-admission-in-flight-p admission) t)))
  admission)

(defun e-board-admission-finish (board admission)
  "Mark ADMISSION's current stack frame complete without losing its token."
  (when (and (e-board-p board) (e-board-admission--admission-p admission))
    (if (e-board-work-admission-p admission)
        (setf (e-board-work-admission-in-flight-p admission) nil)
      (setf (e-board-aggregation-admission-in-flight-p admission) nil)))
  admission)

(defun e-board-admission-pending-count (board)
  "Return the count of Board admissions awaiting exact cleanup."
  (hash-table-count (or (e-board-pending-admissions board)
                        (make-hash-table :test 'eq))))

(defun e-board-admission--pending-in-flight-p (admission)
  "Return whether ADMISSION is executing on the current call stack."
  (if (e-board-work-admission-p admission)
      (e-board-work-admission-in-flight-p admission)
    (e-board-aggregation-admission-in-flight-p admission)))

(defun e-board-admission--committed-p (admission)
  "Return whether ADMISSION has handed its projection to Board consumers."
  (if (e-board-work-admission-p admission)
      (e-board-work-admission-committed-p admission)
    (e-board-aggregation-admission-committed-p admission)))

(defun e-board-admission--set-cancel-requested (admission)
  "Fence ADMISSION without recursively entering its inverse."
  (if (e-board-work-admission-p admission)
      (setf (e-board-work-admission-cancel-requested-p admission) t)
    (setf (e-board-aggregation-admission-cancel-requested-p admission) t)))

(defun e-board-admission--cancel-requested-p (admission)
  "Return whether a reentrant owner entry fenced ADMISSION."
  (if (e-board-work-admission-p admission)
      (e-board-work-admission-cancel-requested-p admission)
    (e-board-aggregation-admission-cancel-requested-p admission)))

(defun e-board-admission--set-cleanup (admission complete)
  "Set ADMISSION cleanup acknowledgement."
  (if (e-board-work-admission-p admission)
      (setf (e-board-work-admission-cleanup-complete-p admission) complete)
    (setf (e-board-aggregation-admission-cleanup-complete-p admission) complete))
  admission)

(defun e-board-admission--cleanup-complete-p (admission)
  "Return public cleanup status for ADMISSION."
  (and (e-board-admission--admission-p admission)
       (if (e-board-work-admission-p admission)
           (e-board-work-admission-cleanup-complete-p admission)
         (e-board-aggregation-admission-cleanup-complete-p admission))))

(defun e-board-admission-complete-p (admission)
  "Return whether ADMISSION's exact cleanup has completed."
  (e-board-admission--cleanup-complete-p admission))

(defun e-board-admission--finalize-event-node (receipt)
  "Retain only the link state needed by a committed event node.

An ordinary event is part of the immutable log for the Board lifetime, while
its admission receipt is only needed until the append transaction commits.
Keep the exact cell/neighbour topology used by a later append or a staged
neighbour inverse, but drop the Board/event payload and all rollback progress
flags.  This is intentionally an event-specific compaction operation, not a
generic shared state bag.
"
  (when (e-board-event-receipt-p receipt)
    (setf (e-board-event-receipt--board receipt) nil
          (e-board-event-receipt--event receipt) nil
          (e-board-event-receipt--forward-stage receipt) nil
          (e-board-event-receipt--inverse-stage receipt) nil
          (e-board-event-receipt--registered-p receipt) nil
          (e-board-event-receipt--prefix-p receipt) nil
          (e-board-event-receipt--count-p receipt) nil
          (e-board-event-receipt--prefix-value receipt) nil))
  receipt)

(defun e-board-admission--finalize-index-node (receipt)
  "Retain only queue-link state for a committed work-index node.

The queue may outlive the admission and be appended to again, so its exact
cell, queue neighbours, and link flags remain.  Identity, mapping, and inverse
progress are rollback-only and can be released after the enclosing admission
has committed.
"
  (when (e-board-index-receipt-p receipt)
    (setf (e-board-index-receipt--index receipt) nil
          (e-board-index-receipt--work-id receipt) nil
          (e-board-index-receipt--subscription-id receipt) nil
          (e-board-index-receipt--forward-stage receipt) nil
          (e-board-index-receipt--inverse-stage receipt) nil
          (e-board-index-receipt--registered-p receipt) nil
          (e-board-index-receipt--mapped-p receipt) nil
          (e-board-index-receipt--removed-p receipt) nil))
  receipt)

(defun e-board-admission--drop-transient-fields (admission)
  "Drop rollback-only fields after a committed admission is acknowledged."
  (when (e-board-work-admission-p admission)
    (dolist (receipt (e-board-work-admission-event-receipts admission))
      (e-board-admission--finalize-event-node receipt))
    (dolist (receipt (e-board-work-admission-index-receipts admission))
      (e-board-admission--finalize-index-node receipt))
    (setf (e-board-work-admission-event-receipts admission) nil
          (e-board-work-admission-index-receipts admission) nil
          (e-board-work-admission-classification-receipts admission) nil
          (e-board-work-admission-effect-receipts admission) nil
          (e-board-work-admission-finalized-p admission) t))
  (when (e-board-aggregation-admission-p admission)
    (dolist (receipt (e-board-aggregation-admission-event-receipts admission))
      (e-board-admission--finalize-event-node receipt))
    (dolist (receipt (e-board-aggregation-admission-index-receipts admission))
      (e-board-admission--finalize-index-node receipt))
    (setf (e-board-aggregation-admission-event-receipts admission) nil
          (e-board-aggregation-admission-index-receipts admission) nil
          (e-board-aggregation-admission-classification-receipts admission) nil
          (e-board-aggregation-admission-effect-receipts admission) nil
          (e-board-aggregation-admission-deadline-receipt admission) nil
          (e-board-aggregation-admission-finalized-p admission) t))
  admission)

(defun e-board-admission-complete (board admission)
  "Finalize committed ADMISSION and remove it from BOARD's pending catalog."
  (when (and (e-board-p board) (e-board-admission--admission-p admission))
    (e-board-admission-finish board admission)
    (e-board-admission--set-cleanup admission t)
    (when (e-board-pending-admissions board)
      (remhash admission (e-board-pending-admissions board)))
    (e-board-admission--drop-transient-fields admission))
  admission)

(defun e-board-admission--remove-pending (board admission)
  "Remove fully acknowledged ADMISSION from BOARD's pending catalog."
  (when (e-board-pending-admissions board)
    (remhash admission (e-board-pending-admissions board))))

(defun e-board-admission--recover (board)
  "Recover non-running pending admissions, fencing reentrant active ones."
  (let (pending first-error in-flight)
    (when (e-board-pending-admissions board)
      (maphash (lambda (admission _state) (push admission pending))
               (e-board-pending-admissions board)))
    (dolist (admission pending)
      (if (e-board-admission--pending-in-flight-p admission)
          (progn
            (e-board-admission--set-cancel-requested admission)
            (setq in-flight t))
        (condition-case err
            (if (e-board-admission--committed-p admission)
                (e-board-admission-complete board admission)
              (e-board-admission-abort board admission))
          (error (unless first-error (setq first-error err))))))
    (when (or first-error in-flight
              (> (e-board-admission-pending-count board) 0))
      (signal 'e-board-admission-pending
              (list "Board admission cleanup remains pending"
                    (or first-error (e-board-admission-pending-count board)))))
    t))

(defun e-board-admission-require-clear (board)
  "Ensure no earlier Board admission can overlap a new related operation."
  (when (> (e-board-admission-pending-count board) 0)
    (e-board-admission--recover board)))

(cl-defun e-board-admission-work-token (handle &key invocation-id effect-target)
  "Create an opaque token for HANDLE's Board work admission."
  (unless (e-work-handle-p handle)
    (signal 'wrong-type-argument (list 'e-work-handle-p handle)))
  (e-board-work-admission--create
   :handle handle :invocation-id invocation-id :effect-target effect-target))

(defun e-board-admission-aggregation-token (board)
  "Create an opaque token for one BOARD aggregation admission."
  (unless (e-board-p board)
    (signal 'wrong-type-argument (list 'e-board-p board)))
  (e-board-aggregation-admission--create :board board))

(defun e-board-admission--receipt-live-p (receipt)
  "Return non-nil when RECEIPT has not completed its exact inverse."
  (and (e-board-admission--admission-p receipt)
       (not (e-board-admission--cleanup-complete-p receipt))))

(defun e-board-admission-current-p (admission)
  "Return whether ADMISSION still owns its committed Board projection."
  (cond
   ((e-board-work-admission-p admission)
    (let ((board (e-board-work-admission-board admission))
          (work (e-board-work-admission-work admission))
          (invocation (e-board-work-admission-invocation admission)))
      (and (e-board-work-admission-committed-p admission)
           (not (e-board-work-admission-aborted-p admission))
           (not (e-board-work-admission-cancel-requested-p admission))
           (e-board-p board)
           (e-board-work-p work)
           (eq (gethash (e-board-work-id work) (e-board-work-table board)) work)
           (or (null invocation)
               (eq (gethash (e-board-invocation-id invocation)
                            (e-board-invocations board)) invocation))
           (cl-every (lambda (receipt)
                       (not (e-board-event-receipt--removed-p receipt)))
                     (e-board-work-admission-event-receipts admission))
           (cl-every (lambda (receipt)
                       (not (e-board-index-receipt--removed-p receipt)))
                     (e-board-work-admission-index-receipts admission))
           (cl-every (lambda (receipt)
                       (not (e-board-terminal-classification-receipt--removed-p
                             receipt)))
                     (e-board-work-admission-classification-receipts admission))
           (cl-every (lambda (receipt)
                       (not (e-board-effect-receipt--removed-p receipt)))
                     (e-board-work-admission-effect-receipts admission)))))
   ((e-board-aggregation-admission-p admission)
    (let ((board (e-board-aggregation-admission-board admission))
          (aggregation (e-board-aggregation-admission-aggregation admission)))
      (and (e-board-aggregation-admission-committed-p admission)
           (not (e-board-aggregation-admission-aborted-p admission))
           (not (e-board-aggregation-admission-cancel-requested-p admission))
           (e-board-p board)
           (e-board-aggregation-p aggregation)
           (eq (gethash (e-board-aggregation-id aggregation)
                        (e-board-aggregations board)) aggregation)
           (e-board-aggregation-admission-map-installed-p admission)
           (not (memq (e-board-aggregation-state aggregation)
                      '(cancelled failed)))
           (cl-every (lambda (receipt)
                       (not (e-board-event-receipt--removed-p receipt)))
                     (e-board-aggregation-admission-event-receipts admission))
           (cl-every (lambda (receipt)
                       (not (e-board-index-receipt--removed-p receipt)))
                     (e-board-aggregation-admission-index-receipts admission))
           (cl-every (lambda (receipt)
                       (not (e-board-terminal-classification-receipt--removed-p
                             receipt)))
                     (e-board-aggregation-admission-classification-receipts
                      admission))
           (let ((deadline
                  (e-board-aggregation-admission-deadline-receipt admission)))
             (or (null deadline)
                 (not (e-board-aggregation-deadline-receipt--removed-p
                       deadline))))
           (cl-every (lambda (receipt)
                       (not (e-board-effect-receipt--removed-p receipt)))
                     (e-board-aggregation-admission-effect-receipts admission)))))
    (t nil)))

(defun e-board-admission--event-raw-linked-p (board receipt)
  "Return whether RECEIPT's captured event cell is still physically linked."
  (let ((previous-node (e-board-event-receipt--previous-node receipt))
        (cell (e-board-event-receipt--cell receipt)))
    (if previous-node
        (eq (cdr (e-board-event-receipt--cell previous-node)) cell)
      (eq (e-board-events board) cell))))

(defun e-board-admission--event-raw-unlinked-p (board receipt)
  "Return whether RECEIPT's exact event gap has already been unlinked."
  (let* ((previous-node (e-board-event-receipt--previous-node receipt))
         (next-node (e-board-event-receipt--next-node receipt))
         (previous (and previous-node
                        (e-board-event-receipt--cell previous-node)))
         (next (and next-node (e-board-event-receipt--cell next-node))))
    (if previous-node
        (and (not (eq (cdr previous) (e-board-event-receipt--cell receipt)))
             (if next-node
                 (eq (cdr previous) next)
               (eq (e-board-events-tail board) previous)))
      (if next-node
          (eq (e-board-events board) next)
        (null (e-board-events board))))))

(defun e-board-admission--append-event (board type data &optional admission)
  "Append one Board event for an already-owned ADMISSION."
  (let* ((previous (e-board-events-tail board))
         (previous-node (and previous
                             (gethash previous (e-board-event-node-index board))))
         (cell (list nil))
         (receipt (e-board-event-receipt--create
                   :board board :cell cell :previous previous
                   :previous-node previous-node :head-p (null previous)
                   :forward-stage 'allocated :inverse-stage 'live)))
    (when (and previous
               (not (and (e-board-event-receipt-p previous-node)
                         (e-board-event-receipt--linked-p previous-node)
                         (e-board-event-receipt--tail-p previous-node)
                         (eq (e-board-event-receipt--cell previous-node)
                             previous))))
      (signal 'e-board-error (list "Event tail has no exact live node" previous)))
    (when (and (null previous) (e-board-events board))
      (signal 'e-board-error
              (list "Event head exists without an exact tail" board)))
    (e-board-admission--push-receipt admission 'event receipt)
    (let ((event (e-board-event--create
                  :seq (cl-incf (e-board-next-seq board))
                  :type type :data data)))
      (setcar cell event)
      (setf (e-board-event-receipt--event receipt) event
            ;; Keep the value even when the checkpoint table adapter signals
            ;; before installing its key.  The inverse can then complete the
            ;; monotonic sequence checkpoint instead of leaving a future gap
            ;; that makes observer cursors replay or fail.
            (e-board-event-receipt--prefix-value receipt)
            (e-board-message-count board)
            (e-board-event-receipt--forward-stage receipt) 'registering)
      ;; Hash-table mutation is an observable boundary in tests and in a
      ;; reentrant notification.  If an adapter signals after installing the
      ;; exact entry, acknowledge that postcondition before preserving the
      ;; initiating error so the inverse can resume rather than guessing.
      (condition-case err
          (puthash cell receipt (e-board-event-node-index board))
        (error
         (when (eq (gethash cell (e-board-event-node-index board)) receipt)
           (setf (e-board-event-receipt--registered-p receipt) t
                 (e-board-event-receipt--forward-stage receipt) 'registered))
         (signal (car err) (cdr err))))
      (setf (e-board-event-receipt--registered-p receipt) t
            (e-board-event-receipt--root-node receipt)
            (or (and previous-node
                     (or (e-board-event-receipt--root-node previous-node)
                         previous-node))
                receipt)
            (e-board-event-receipt--forward-stage receipt) 'registered)
      (setf (e-board-event-receipt--forward-stage receipt)
            (if previous 'linking 'heading))
      (if previous
          (condition-case err
              (setcdr previous cell)
            (error
             (when (eq (cdr previous) cell)
               (setf (e-board-event-receipt--linked-p receipt) t
                     (e-board-event-receipt--forward-stage receipt) 'linked))
             (signal (car err) (cdr err))))
        (setf (e-board-events board) cell))
      (setf (e-board-event-receipt--linked-p receipt) t
            (e-board-event-receipt--forward-stage receipt) 'linked)
      (when previous-node
        (setf (e-board-event-receipt--next-node previous-node) receipt
              (e-board-event-receipt--tail-p previous-node) nil))
      (setf (e-board-event-receipt--forward-stage receipt) 'tailing)
      (setf (e-board-events-tail board) cell
            (e-board-event-receipt--tail-p receipt) t
            (e-board-event-receipt--forward-stage receipt) 'tailed)
      ;; This map is a retained monotonic sequence checkpoint, not removable
      ;; event membership.  It deliberately survives an exact inverse gap.
      (setf (e-board-event-receipt--forward-stage receipt) 'prefixing)
      (condition-case err
          (puthash (e-board-event-seq event) (e-board-message-count board)
                   (e-board-event-message-count board))
        (error
         (when (eql (gethash (e-board-event-seq event)
                             (e-board-event-message-count board))
                    (e-board-message-count board))
           (setf (e-board-event-receipt--prefix-p receipt) t
                 (e-board-event-receipt--count-p receipt) t
                 (e-board-event-message-prefix-high-watermark board)
                 (max (e-board-event-message-prefix-high-watermark board)
                      (e-board-event-seq event))
                 (e-board-event-receipt--forward-stage receipt) 'counted))
         (signal (car err) (cdr err))))
      (setf (e-board-event-receipt--prefix-p receipt) t
            (e-board-event-receipt--count-p receipt) t
            (e-board-event-message-prefix-high-watermark board)
            (max (e-board-event-message-prefix-high-watermark board)
                 (e-board-event-seq event))
            (e-board-event-receipt--forward-stage receipt) 'counted)
      event)))

(defun e-board-admission-append-event (board type data &optional admission)
  "Append one Board event, retaining exact recovery when no token is supplied.

Internal Board callers normally pass an enclosing admission.  Standalone Board
notifications receive a short-lived Board admission token so an error after a
list or index mutation remains discoverable through the Board pending catalog.
The token is finalized only after the event append commits, and is never
reconstructed from an event type or sequence number."
  (if (e-board-admission--admission-p admission)
      (e-board-admission--append-event board type data admission)
    (let ((standalone (e-board-admission-aggregation-token board)))
      (e-board-admission-begin board standalone)
      (let (event)
        (condition-case err
            (progn
              (setq event
                    (e-board-admission--append-event
                     board type data standalone))
              (setf (e-board-aggregation-admission-committed-p standalone) t)
              (e-board-admission-finish board standalone))
          (error
           (e-board-admission-finish board standalone)
           ;; If the append itself failed, the exact token owns its inverse.
           (condition-case _cleanup-error
               (e-board-admission-abort board standalone)
             (error nil))
           (signal (car err) (cdr err))))
        ;; Completion errors remain retryable through the committed catalog
        ;; entry; they must not inverse an already-visible event.
        (e-board-admission-complete board standalone)
        event))))

(defun e-board-admission--event-repair-neighbours (receipt)
  "Repair RECEIPT's exact neighbouring node links, resumably."
  (let ((previous-node (e-board-event-receipt--previous-node receipt))
        (next-node (e-board-event-receipt--next-node receipt))
        (previous (e-board-event-receipt--previous receipt)))
    (when previous-node
      (when (eq (e-board-event-receipt--next-node previous-node) receipt)
        (setf (e-board-event-receipt--next-node previous-node) next-node)))
    (when next-node
      (setf (e-board-event-receipt--previous-node next-node) previous-node
            (e-board-event-receipt--previous next-node) previous
            (e-board-event-receipt--root-node next-node)
            (if previous-node
                (e-board-event-receipt--root-node previous-node)
              next-node)
            (e-board-event-receipt--head-p next-node) (null previous-node)))
    (when (null previous-node)
      (setf (e-board-event-receipt--head-p receipt) nil))))

(defun e-board-admission--event-ensure-prefix-checkpoint (receipt)
  "Ensure RECEIPT's allocated sequence retains its monotonic prefix value.

An append that failed while inserting the checkpoint still consumed a
monotonic event sequence.  Its exact rollback receipt remains the only safe
authority for installing that missing value; no history scan or guessed zero
is allowed.
"
  (let* ((board (e-board-event-receipt--board receipt))
         (event (e-board-event-receipt--event receipt))
         (seq (and event (e-board-event-seq event)))
         (value (e-board-event-receipt--prefix-value receipt))
         (missing (make-symbol "missing-prefix"))
         (current (and seq
                       (gethash seq (e-board-event-message-count board)
                                missing))))
    (cond
     ((eq current missing)
      (condition-case err
          (puthash seq value (e-board-event-message-count board))
        (error (signal (car err) (cdr err))))
      (setf (e-board-event-message-prefix-high-watermark board)
            (max (e-board-event-message-prefix-high-watermark board) seq)))
     ((not (eql current value))
      (signal 'e-board-error
              (list "Event prefix checkpoint was replaced" seq current value)))
     (t nil))
    (unless (eql (gethash seq (e-board-event-message-count board)) value)
      (signal 'e-board-error
              (list "Event prefix checkpoint did not retain" seq)))
    (setf (e-board-event-receipt--prefix-p receipt) t)
    t))

(defun e-board-admission-remove-event (receipt)
  "Remove RECEIPT's event cell with exact resumable progress."
  (when (e-board-event-receipt-p receipt)
    (unless (e-board-event-receipt--removed-p receipt)
      (let* ((board (e-board-event-receipt--board receipt))
             (cell (e-board-event-receipt--cell receipt))
             (current (gethash cell (e-board-event-node-index board)))
             (stage (e-board-event-receipt--inverse-stage receipt)))
        (when (and (e-board-event-receipt--registered-p receipt)
                   (not (or (eq current receipt)
                            (and (null current)
                                 (memq stage
                                       '(unregistering unregistered removed))))))
          (signal 'e-board-error
                  (list "Event admission node is not exact" receipt)))
        ;; Link inverse: retain the stage when a fallible setcdr signals after
        ;; changing the exact predecessor.  The next call then starts at the
        ;; acknowledged gap and never repeats that mutation.
        (unless (memq stage '(unlinked repaired tail count unregistered removed))
          (setf (e-board-event-receipt--inverse-stage receipt) 'unlinking)
          (cond
           ((e-board-admission--event-raw-linked-p board receipt)
            (let ((previous-node
                   (e-board-event-receipt--previous-node receipt)))
              (if previous-node
                  (let* ((previous-cell
                          (e-board-event-receipt--cell previous-node))
                         (before-cdr (cdr previous-cell)))
                    (condition-case err
                        (setcdr previous-cell (cdr cell))
                      (error
                       (if (and (not (eq (cdr previous-cell) before-cdr))
                                (equal (cdr previous-cell) (cdr cell)))
                           (progn
                             (setf (e-board-event-receipt--linked-p receipt) nil
                                   (e-board-event-receipt--inverse-stage receipt)
                                   'unlinked)
                             (signal (car err) (cdr err)))
                         (signal (car err) (cdr err)))))
                nil)
              (when (null previous-node)
                (setf (e-board-events board) (cdr cell)))
              (setf (e-board-event-receipt--linked-p receipt) nil))))
           ((e-board-admission--event-raw-unlinked-p board receipt)
            (setf (e-board-event-receipt--linked-p receipt) nil))
           (t
            (signal 'e-board-error
                    (list "Event admission exact link is absent" receipt))))
          (setf (e-board-event-receipt--inverse-stage receipt) 'unlinked))
        ;; Neighbour repair is independent of the physical unlink, and its
        ;; postconditions are checked on every retry.
        (unless (memq (e-board-event-receipt--inverse-stage receipt)
                      '(repaired tail count unregistered removed))
          (e-board-admission--event-repair-neighbours receipt)
          (setf (e-board-event-receipt--inverse-stage receipt) 'repaired))
        (when (or (e-board-event-receipt--tail-p receipt)
                  (eq (e-board-events-tail board) cell))
          (setf (e-board-event-receipt--inverse-stage receipt) 'tailing)
          (when (eq (e-board-events-tail board) cell)
            (setf (e-board-events-tail board)
                  (e-board-event-receipt--previous receipt)))
          (when-let ((previous-node
                      (e-board-event-receipt--previous-node receipt)))
            (setf (e-board-event-receipt--tail-p previous-node) t))
          (setf (e-board-event-receipt--tail-p receipt) nil
                (e-board-event-receipt--inverse-stage receipt) 'tail))
        ;; Prefix checkpoints intentionally survive an event inverse.  If the
        ;; forward append failed before that table mutation, install the exact
        ;; captured value now; this is a real resumable inverse stage rather
        ;; than a removable event-list side effect.
        (unless (e-board-event-receipt--prefix-p receipt)
          (setf (e-board-event-receipt--inverse-stage receipt) 'prefixing)
          (e-board-admission--event-ensure-prefix-checkpoint receipt)
          (setf (e-board-event-receipt--inverse-stage receipt) 'count))
        (setf (e-board-event-receipt--count-p receipt) nil)
        (when (or (e-board-event-receipt--registered-p receipt)
                  (memq (e-board-event-receipt--forward-stage receipt)
                        '(registering registered)))
          (setf (e-board-event-receipt--inverse-stage receipt) 'unregistering)
          (let ((node (gethash cell (e-board-event-node-index board))))
            (cond
             ((eq node receipt)
              (condition-case err
                  (remhash cell (e-board-event-node-index board))
                (error
                 (if (null (gethash cell (e-board-event-node-index board)))
                     (progn
                       (setf (e-board-event-receipt--registered-p receipt) nil)
                       (signal (car err) (cdr err)))
                   (signal (car err) (cdr err))))))
             ((null node) nil)
             (t
              (signal 'e-board-error
                      (list "Event admission node was replaced" receipt))))
            (setf (e-board-event-receipt--registered-p receipt) nil))
          (setf (e-board-event-receipt--inverse-stage receipt) 'unregistered))
        (setf (e-board-event-receipt--removed-p receipt) t
              (e-board-event-receipt--root-node receipt) nil
              (e-board-event-receipt--inverse-stage receipt) 'removed)))
    t))

(defun e-board-admission-index-work (index work-id subscription-id
                                           &optional admission)
  "Append SUBSCRIPTION-ID to WORK-ID's INDEX and return an exact receipt."
  (unless (e-board-admission--admission-p admission)
    (signal 'e-board-error
            (list "Work index admission requires an exact owner token"
                  work-id subscription-id)))
  (let* ((queue (gethash work-id index))
         (queue-created-p (null queue))
         (queue (or queue (e-board-id-queue--create
                           :node-index (make-hash-table :test 'eq))))
         (previous (e-board-id-queue-tail queue))
         (previous-node (and previous
                             (gethash previous (e-board-id-queue-node-index queue))))
         (cell (list subscription-id))
         (receipt (e-board-index-receipt--create
                   :index index :work-id work-id :subscription-id subscription-id
                   :queue queue :cell cell :previous previous
                   :previous-node previous-node :queue-created-p queue-created-p
                   :head-p (null previous) :forward-stage 'allocated
                   :inverse-stage 'live)))
    (when (and previous
               (not (and (e-board-index-receipt-p previous-node)
                         (e-board-index-receipt--linked-p previous-node)
                         (e-board-index-receipt--tail-p previous-node)
                         (eq (e-board-index-receipt--cell previous-node)
                             previous))))
      (signal 'e-board-error
              (list "Work index tail has no exact live node" work-id)))
    (when (and (null previous) (e-board-id-queue-head queue))
      (signal 'e-board-error
              (list "Work index head exists without an exact tail" work-id)))
    (e-board-admission--push-receipt admission 'index receipt)
    (when queue-created-p
      (setf (e-board-index-receipt--forward-stage receipt) 'mapping)
      (condition-case err
          (puthash work-id queue index)
        (error
         (when (eq (gethash work-id index) queue)
           (setf (e-board-index-receipt--mapped-p receipt) t
                 (e-board-index-receipt--forward-stage receipt) 'mapped))
         (signal (car err) (cdr err))))
      (setf (e-board-index-receipt--mapped-p receipt) t
            (e-board-index-receipt--forward-stage receipt) 'mapped))
    (setf (e-board-index-receipt--forward-stage receipt) 'registering)
    (condition-case err
        (puthash cell receipt (e-board-id-queue-node-index queue))
      (error
       (when (eq (gethash cell (e-board-id-queue-node-index queue)) receipt)
         (setf (e-board-index-receipt--registered-p receipt) t
               (e-board-index-receipt--forward-stage receipt) 'registered))
       (signal (car err) (cdr err))))
    (setf (e-board-index-receipt--registered-p receipt) t
          (e-board-index-receipt--root-node receipt)
          (or (and previous-node
                   (or (e-board-index-receipt--root-node previous-node)
                       previous-node))
              receipt)
          (e-board-index-receipt--forward-stage receipt) 'registered)
    (setf (e-board-index-receipt--forward-stage receipt)
          (if previous 'linking 'heading))
    (if previous
        (condition-case err
            (setcdr previous cell)
          (error
           (when (eq (cdr previous) cell)
             (setf (e-board-index-receipt--linked-p receipt) t
                   (e-board-index-receipt--forward-stage receipt) 'linked))
           (signal (car err) (cdr err))))
      (setf (e-board-id-queue-head queue) cell))
    (setf (e-board-index-receipt--linked-p receipt) t
          (e-board-index-receipt--forward-stage receipt) 'linked)
    (when previous-node
      (setf (e-board-index-receipt--next-node previous-node) receipt
            (e-board-index-receipt--tail-p previous-node) nil))
    (setf (e-board-index-receipt--forward-stage receipt) 'tailing)
    (setf (e-board-id-queue-tail queue) cell
          (e-board-index-receipt--tail-p receipt) t
          (e-board-index-receipt--forward-stage receipt) 'tailed)
    receipt))

(defun e-board-admission--index-values (index work-id)
  "Return a detached ordered list of subscription values for WORK-ID.

The queue cell remains owned by this module; callers receive only its semantic
values and cannot mutate the exact inverse through a returned cons cell."
  (when-let ((queue (gethash work-id index)))
    (copy-sequence (e-board-id-queue-head queue))))

(defun e-board-admission--index-raw-linked-p (receipt)
  "Return whether RECEIPT's exact index cell is physically linked."
  (let ((queue (e-board-index-receipt--queue receipt))
        (previous-node (e-board-index-receipt--previous-node receipt))
        (cell (e-board-index-receipt--cell receipt)))
    (if previous-node
        (eq (cdr (e-board-index-receipt--cell previous-node)) cell)
      (eq (e-board-id-queue-head queue) cell))))

(defun e-board-admission--index-raw-unlinked-p (receipt)
  "Return whether RECEIPT's index gap has already been unlinked."
  (let* ((queue (e-board-index-receipt--queue receipt))
         (previous-node (e-board-index-receipt--previous-node receipt))
         (next-node (e-board-index-receipt--next-node receipt))
         (previous (and previous-node
                        (e-board-index-receipt--cell previous-node)))
         (next (and next-node (e-board-index-receipt--cell next-node))))
    (if previous-node
        (and (not (eq (cdr previous) (e-board-index-receipt--cell receipt)))
             (if next-node
                 (eq (cdr previous) next)
               (eq (e-board-id-queue-tail queue) previous)))
      (if next-node
          (eq (e-board-id-queue-head queue) next)
        (null (e-board-id-queue-head queue))))))

(defun e-board-admission--index-repair-neighbours (receipt)
  "Repair RECEIPT's exact queue neighbours."
  (let* ((previous-node (e-board-index-receipt--previous-node receipt))
         (next-node (e-board-index-receipt--next-node receipt))
         (previous (e-board-index-receipt--previous receipt)))
    (when previous-node
      (when (eq (e-board-index-receipt--next-node previous-node) receipt)
        (setf (e-board-index-receipt--next-node previous-node) next-node)))
    (when next-node
      (setf (e-board-index-receipt--previous-node next-node) previous-node
            (e-board-index-receipt--previous next-node) previous
            (e-board-index-receipt--root-node next-node)
            (if previous-node
                (e-board-index-receipt--root-node previous-node)
              next-node)
            (e-board-index-receipt--head-p next-node) (null previous-node)))
    (when (null previous-node)
      (setf (e-board-index-receipt--head-p receipt) nil))))

(defun e-board-admission-remove-index (receipt)
  "Remove one exact work-index receipt with resumable primitive stages."
  (when (e-board-index-receipt-p receipt)
    (unless (e-board-index-receipt--removed-p receipt)
      (let* ((index (e-board-index-receipt--index receipt))
             (queue (e-board-index-receipt--queue receipt))
             (work-id (e-board-index-receipt--work-id receipt))
             (cell (e-board-index-receipt--cell receipt))
             (node (gethash cell (e-board-id-queue-node-index queue)))
             (stage (e-board-index-receipt--inverse-stage receipt)))
        (when (and (or (e-board-index-receipt--registered-p receipt)
                       (e-board-index-receipt--linked-p receipt))
                   (not (or (eq node receipt)
                            (and (null node)
                                 (memq stage '(unregistering unregistered removed))))))
          (signal 'e-board-error
                  (list "Work index receipt is not an exact node" work-id)))
        (unless (memq stage '(unlinked repaired tail unmapped unregistered removed))
          (setf (e-board-index-receipt--inverse-stage receipt) 'unlinking)
          (cond
           ((e-board-admission--index-raw-linked-p receipt)
            (let ((previous-node (e-board-index-receipt--previous-node receipt))
                  (cell (e-board-index-receipt--cell receipt)))
              (if previous-node
                  (let* ((previous-cell
                          (e-board-index-receipt--cell previous-node))
                         (before-cdr (cdr previous-cell)))
                    (condition-case err
                        (setcdr previous-cell (cdr cell))
                      (error
                       (if (and (not (eq (cdr previous-cell) before-cdr))
                                (equal (cdr previous-cell) (cdr cell)))
                           (progn
                             (setf (e-board-index-receipt--linked-p receipt) nil
                                   (e-board-index-receipt--inverse-stage receipt)
                                   'unlinked)
                             (signal (car err) (cdr err)))
                         (signal (car err) (cdr err)))))
                nil)
              (when (null previous-node)
                (setf (e-board-id-queue-head queue) (cdr cell)))
              (setf (e-board-index-receipt--linked-p receipt) nil))))
           ((e-board-admission--index-raw-unlinked-p receipt)
            (setf (e-board-index-receipt--linked-p receipt) nil))
           (t (signal 'e-board-error
                     (list "Work index receipt exact link is absent" receipt))))
          (setf (e-board-index-receipt--inverse-stage receipt) 'unlinked))
        (unless (memq (e-board-index-receipt--inverse-stage receipt)
                      '(repaired tail unmapped unregistered removed))
          (e-board-admission--index-repair-neighbours receipt)
          (setf (e-board-index-receipt--inverse-stage receipt) 'repaired))
        (when (or (e-board-index-receipt--tail-p receipt)
                  (eq (e-board-id-queue-tail queue) cell))
          (setf (e-board-index-receipt--inverse-stage receipt) 'tailing)
          (when (eq (e-board-id-queue-tail queue) cell)
            (setf (e-board-id-queue-tail queue)
                  (e-board-index-receipt--previous receipt)))
          (when-let ((previous-node (e-board-index-receipt--previous-node receipt)))
            (setf (e-board-index-receipt--tail-p previous-node) t))
          (setf (e-board-index-receipt--tail-p receipt) nil
                (e-board-index-receipt--inverse-stage receipt) 'tail))
        (when (and (null (e-board-id-queue-head queue))
                   (eq (gethash work-id index) queue))
          (setf (e-board-index-receipt--inverse-stage receipt) 'unmapping)
          (condition-case err
              (remhash work-id index)
            (error
             (if (null (gethash work-id index))
                 (progn
                   (setf (e-board-index-receipt--mapped-p receipt) nil)
                   (signal (car err) (cdr err)))
               (signal (car err) (cdr err)))))
          (setf (e-board-index-receipt--mapped-p receipt) nil
                (e-board-index-receipt--inverse-stage receipt) 'unmapped))
        (when (or (e-board-index-receipt--registered-p receipt)
                  (memq (e-board-index-receipt--forward-stage receipt)
                        '(registering registered)))
          (setf (e-board-index-receipt--inverse-stage receipt) 'unregistering)
          (let ((current (gethash cell (e-board-id-queue-node-index queue))))
            (cond
             ((eq current receipt)
              (condition-case err
                  (remhash cell (e-board-id-queue-node-index queue))
                (error
                 (if (null (gethash cell (e-board-id-queue-node-index queue)))
                     (progn
                       (setf (e-board-index-receipt--registered-p receipt) nil)
                       (signal (car err) (cdr err)))
                   (signal (car err) (cdr err))))))
             ((null current) nil)
             (t (signal 'e-board-error
                       (list "Work index node was replaced" receipt))))
            (setf (e-board-index-receipt--registered-p receipt) nil))
          (setf (e-board-index-receipt--inverse-stage receipt) 'unregistered))
        (setf (e-board-index-receipt--removed-p receipt) t
              (e-board-index-receipt--root-node receipt) nil
              (e-board-index-receipt--inverse-stage receipt) 'removed)))
    t))

(defun e-board-admission--queue-terminal-classification
    (board work &optional invocation-ids aggregation-ids aggregation-objects
           admission schedule-function)
  "Queue one exact terminal classifier record for WORK.

SCHEDULE-FUNCTION is the Board semantic owner's bounded scheduler operation; it
is supplied by the facade so this lower owner never calls upward into policy."
  (unless (e-board-work-p work)
    (signal 'e-board-error
            (list "Terminal classifier requires an exact work" work)))
  (unless (eq (gethash (e-board-work-id work) (e-board-work-table board)) work)
    (signal 'e-board-error
            (list "Terminal classifier work is not current" work)))
  (let* ((work-id (e-board-work-id work))
         (frozen-invocation-ids
          (or invocation-ids
              (when-let ((queue (gethash work-id
                                         (e-board-invocation-work-index board))))
                (e-board-id-queue-head queue))))
         (frozen-invocation-objects
          (mapcar (lambda (id) (gethash id (e-board-invocations board)))
                  frozen-invocation-ids))
         (frozen-aggregation-ids
          (or aggregation-ids
              (when-let ((queue (gethash work-id
                                         (e-board-aggregation-work-index board))))
                (e-board-id-queue-head queue))))
         (frozen-aggregation-objects
          (or aggregation-objects
              (mapcar (lambda (id) (gethash id (e-board-aggregations board)))
                      frozen-aggregation-ids)))
         (invocation-index (gethash work-id (e-board-invocation-work-index board)))
         (aggregation-index (gethash work-id (e-board-aggregation-work-index board)))
         (record (e-board-terminal-classification--create
                  :work-id work-id :work work
                   :invocation-ids frozen-invocation-ids
                   :invocation-objects frozen-invocation-objects
                   :aggregation-ids frozen-aggregation-ids
                   :aggregation-objects frozen-aggregation-objects))
         (previous (e-board-terminal-classification-tail board))
         (previous-receipt
          (and previous
               (gethash previous
                        (e-board-terminal-classification-node-index board))))
         (cell (list record))
         (receipt (e-board-terminal-classification-receipt--create
                   :board board :record record :cell cell :previous previous
                   :previous-receipt previous-receipt :head-p (null previous)
                   :invocation-index (e-board-invocation-work-index board)
                   :invocation-index-value invocation-index
                   :aggregation-index (e-board-aggregation-work-index board)
                   :aggregation-index-value aggregation-index
                   :forward-stage 'allocated :inverse-stage 'live)))
    (unless (cl-every #'e-board-invocation-p frozen-invocation-objects)
      (signal 'e-board-error
              (list "Terminal classifier invocation is not current" work)))
    (unless (cl-every #'e-board-aggregation-p frozen-aggregation-objects)
      (signal 'e-board-error
              (list "Terminal classifier aggregation is not current" work)))
    (when (and previous
               (not (e-board-terminal-classification-receipt-p previous-receipt)))
      (signal 'e-board-error
              (list "Terminal classifier tail has no exact receipt" work-id)))
    (e-board-admission--push-receipt admission 'classification receipt)
    ;; The frozen index is consumed by this classifier.  Its exact queues are
    ;; not rediscovered during draining; a replacement map remains untouched.
    (setf (e-board-terminal-classification-receipt--forward-stage receipt)
          'detaching)
    (when invocation-index
      (unless (eq (gethash work-id (e-board-invocation-work-index board))
                  invocation-index)
        (signal 'e-board-error
                (list "Invocation classifier index was replaced" work-id)))
      (condition-case err
          (remhash work-id (e-board-invocation-work-index board))
        (error
         (when (null (gethash work-id (e-board-invocation-work-index board)))
           (setf (e-board-terminal-classification-receipt--invocation-index-detached-p
                  receipt)
                 t))
         (signal (car err) (cdr err))))
      (setf (e-board-terminal-classification-receipt--invocation-index-detached-p
             receipt)
            t))
    (when aggregation-index
      (unless (eq (gethash work-id (e-board-aggregation-work-index board))
                  aggregation-index)
        (signal 'e-board-error
                (list "Aggregation classifier index was replaced" work-id)))
      (condition-case err
          (remhash work-id (e-board-aggregation-work-index board))
        (error
         (when (null (gethash work-id (e-board-aggregation-work-index board)))
           (setf (e-board-terminal-classification-receipt--aggregation-index-detached-p
                  receipt)
                 t))
         (signal (car err) (cdr err))))
      (setf (e-board-terminal-classification-receipt--aggregation-index-detached-p
             receipt)
            t))
    (setf (e-board-terminal-classification-receipt--forward-stage receipt)
          'registering)
    (condition-case err
        (puthash cell receipt (e-board-terminal-classification-node-index board))
      (error
       (when (eq (gethash cell (e-board-terminal-classification-node-index board))
                 receipt)
         (setf (e-board-terminal-classification-receipt--registered-p receipt) t
               (e-board-terminal-classification-receipt--forward-stage receipt)
               'registered))
       (signal (car err) (cdr err))))
    (setf (e-board-terminal-classification-receipt--registered-p receipt) t
          (e-board-terminal-classification-receipt--forward-stage receipt)
          'registered)
    (setf (e-board-terminal-classification-receipt--forward-stage receipt)
          'linking)
    (if previous
        (let ((before-cdr (cdr previous)))
          (condition-case err
              (setcdr previous cell)
            (error
             (if (and (not (eq (cdr previous) before-cdr))
                      (eq (cdr previous) cell))
                 (progn
                   (setf (e-board-terminal-classification-receipt--linked-p receipt)
                         t
                         (e-board-terminal-classification-receipt--forward-stage
                          receipt)
                         'linked)
                   (signal (car err) (cdr err)))
               (signal (car err) (cdr err))))))
      (setf (e-board-terminal-classifications board) cell))
    (setf (e-board-terminal-classification-receipt--linked-p receipt) t
          (e-board-terminal-classification-receipt--forward-stage receipt)
          'linked)
    (when previous-receipt
      (setf (e-board-terminal-classification-receipt--next-receipt
             previous-receipt)
            receipt))
    (setf (e-board-terminal-classification-tail board) cell
          (e-board-terminal-classification-receipt--tail-p receipt) t
          (e-board-terminal-classification-receipt--forward-stage receipt)
          'tailing)
    (setf (e-board-terminal-classification-receipt--forward-stage receipt)
          'counting)
    (let ((before (e-board-unsettled-routing-count board)))
      (condition-case err
          (progn
            (e-board-admission--adjust-unsettled board 'routing 1)
            (setf (e-board-terminal-classification-receipt--queued-p receipt) t
                  (e-board-terminal-classification-receipt--counted-p receipt) t
                  (e-board-terminal-classification-receipt--forward-stage receipt)
                  'counted))
        (error
         (when (= (e-board-unsettled-routing-count board) (1+ before))
           (setf (e-board-terminal-classification-receipt--queued-p receipt) t
                 (e-board-terminal-classification-receipt--counted-p receipt) t
                 (e-board-terminal-classification-receipt--forward-stage receipt)
                 'counted))
         (signal (car err) (cdr err)))))
    (when schedule-function
      (condition-case err
          (progn (funcall schedule-function board)
                 (setf (e-board-terminal-classification-receipt--scheduled-p receipt)
                       t))
        (error
         (when (e-board-terminal-classification-scheduled board)
           (setf (e-board-terminal-classification-receipt--scheduled-p receipt) t))
         (signal (car err) (cdr err)))))
    receipt))

(defun e-board-admission-queue-terminal-classification
    (board work &optional invocation-ids aggregation-ids aggregation-objects
           admission schedule-function)
  "Queue a terminal classifier, retaining exact recovery when standalone.

Board composition passes its enclosing admission.  A direct notification gets
an opaque short-lived Board admission so every queue, count, and scheduler
mutation remains recoverable without reconstructing an admission from WORK or
its descriptive ids."
  (if (e-board-admission--admission-p admission)
      (e-board-admission--queue-terminal-classification
       board work invocation-ids aggregation-ids aggregation-objects admission
       schedule-function)
    (let ((standalone (e-board-admission-aggregation-token board))
          receipt)
      (e-board-admission-begin board standalone)
      (condition-case err
          (progn
            (setq receipt
                  (e-board-admission--queue-terminal-classification
                   board work invocation-ids aggregation-ids aggregation-objects
                   standalone schedule-function))
            (setf (e-board-aggregation-admission-committed-p standalone) t)
            (e-board-admission-finish board standalone))
        (error
         (e-board-admission-finish board standalone)
         (condition-case _cleanup-error
             (e-board-admission-abort board standalone)
           (error nil))
         (signal (car err) (cdr err))))
      (e-board-admission-complete board standalone)
      receipt)))

(defun e-board-admission-remove-terminal-classification (receipt)
  "Remove one exact terminal-classifier receipt, retrying each stage."
  (when (e-board-terminal-classification-receipt-p receipt)
    (unless (e-board-terminal-classification-receipt--removed-p receipt)
      (let* ((board (e-board-terminal-classification-receipt--board receipt))
             (cell (e-board-terminal-classification-receipt--cell receipt))
             (previous-receipt
              (let ((candidate
                     (e-board-terminal-classification-receipt--previous-receipt
                      receipt)))
                (while (and candidate
                             (e-board-terminal-classification-receipt--removed-p
                              candidate))
                  (setq candidate
                        (e-board-terminal-classification-receipt--previous-receipt
                         candidate)))
                candidate))
             (next-receipt
              (let ((candidate
                     (e-board-terminal-classification-receipt--next-receipt
                      receipt)))
                (while (and candidate
                             (e-board-terminal-classification-receipt--removed-p
                              candidate))
                  (setq candidate
                        (e-board-terminal-classification-receipt--next-receipt
                         candidate)))
                candidate))
             (previous (and previous-receipt
                            (e-board-terminal-classification-receipt--cell
                             previous-receipt)))
             (node (gethash cell (e-board-terminal-classification-node-index board)))
             (stage (e-board-terminal-classification-receipt--inverse-stage receipt)))
        (when (and (e-board-terminal-classification-receipt--registered-p receipt)
                   (not (or (eq node receipt)
                            (and (null node)
                                 (memq stage '(unregistering unregistered removed))))))
          (signal 'e-board-error
                  (list "Terminal classifier receipt is not current" receipt)))
        (unless (memq stage '(unlinked repaired tail count unregistered removed))
          (setf (e-board-terminal-classification-receipt--inverse-stage receipt)
                'unlinking)
          (cond
           ((and previous (eq (cdr previous) cell))
            (let ((before-cdr (cdr previous)))
              (condition-case err
                  (setcdr previous (cdr cell))
                (error
                 (if (and (not (eq (cdr previous) before-cdr))
                          (eq (cdr previous) (cdr cell)))
                     (progn
                       (setf (e-board-terminal-classification-receipt--linked-p
                              receipt)
                             nil
                             (e-board-terminal-classification-receipt--inverse-stage
                              receipt)
                             'unlinked)
                       (signal (car err) (cdr err)))
                   (signal (car err) (cdr err)))))))
           ((and (null previous)
                 (eq (e-board-terminal-classifications board) cell))
            (setf (e-board-terminal-classifications board) (cdr cell)))
           ((if next-receipt
                (and previous (eq (cdr previous)
                                  (e-board-terminal-classification-receipt--cell
                                   next-receipt)))
              (and (null next-receipt)
                   (eq (e-board-terminal-classification-tail board) previous)))
            ;; The exact gap was already acknowledged by a reentrant inverse.
            nil)
           (t (signal 'e-board-error
                     (list "Terminal classifier receipt exact link is absent"
                           receipt))))
          (setf (e-board-terminal-classification-receipt--linked-p receipt) nil)
          (setf (e-board-terminal-classification-receipt--inverse-stage receipt)
                'unlinked))
        (unless (memq (e-board-terminal-classification-receipt--inverse-stage receipt)
                      '(repaired tail count unregistered removed))
          (when previous-receipt
            (setf (e-board-terminal-classification-receipt--next-receipt
                   previous-receipt)
                  next-receipt))
          (when next-receipt
            (setf (e-board-terminal-classification-receipt--previous-receipt
                   next-receipt)
                  previous-receipt
                  (e-board-terminal-classification-receipt--head-p next-receipt)
                  (null previous-receipt)))
          (setf (e-board-terminal-classification-receipt--inverse-stage receipt)
                'repaired))
        (when (or (e-board-terminal-classification-receipt--tail-p receipt)
                  (eq (e-board-terminal-classification-tail board) cell))
          (setf (e-board-terminal-classification-receipt--inverse-stage receipt)
                'tailing)
          (when (eq (e-board-terminal-classification-tail board) cell)
            (setf (e-board-terminal-classification-tail board) previous))
          (when previous-receipt
            (setf (e-board-terminal-classification-receipt--tail-p
                   previous-receipt) t))
          (setf (e-board-terminal-classification-receipt--tail-p receipt) nil
                (e-board-terminal-classification-receipt--inverse-stage receipt)
                'tail))
        (when (e-board-terminal-classification-receipt--queued-p receipt)
          (let ((before (e-board-unsettled-routing-count board)))
            (condition-case err
                (progn
                  (e-board-admission--adjust-unsettled board 'routing -1)
                  (setf (e-board-terminal-classification-receipt--queued-p receipt)
                        nil
                        (e-board-terminal-classification-receipt--inverse-stage
                         receipt)
                        'count))
              (error
               (when (= (e-board-unsettled-routing-count board) (1- before))
                 (setf (e-board-terminal-classification-receipt--queued-p receipt)
                       nil
                       (e-board-terminal-classification-receipt--inverse-stage
                        receipt)
                       'count))
               (signal (car err) (cdr err))))))
        (when (or (e-board-terminal-classification-receipt--registered-p receipt)
                  (memq (e-board-terminal-classification-receipt--forward-stage receipt)
                        '(registering registered)))
          (setf (e-board-terminal-classification-receipt--inverse-stage receipt)
                'unregistering)
          (let ((current (gethash cell
                                  (e-board-terminal-classification-node-index board))))
            (cond
             ((eq current receipt)
              (condition-case err
                  (remhash cell (e-board-terminal-classification-node-index board))
                (error
                 (if (null (gethash cell
                                    (e-board-terminal-classification-node-index
                                     board)))
                     (progn
                       (setf (e-board-terminal-classification-receipt--registered-p
                              receipt)
                             nil
                             (e-board-terminal-classification-receipt--inverse-stage
                              receipt)
                             'unregistered)
                       (signal (car err) (cdr err)))
                   (signal (car err) (cdr err))))))
             ((null current) nil)
             (t (signal 'e-board-error
                       (list "Terminal classifier node was replaced" receipt))))
            (setf (e-board-terminal-classification-receipt--registered-p receipt)
                  nil))
          (setf (e-board-terminal-classification-receipt--inverse-stage receipt)
                'unregistered))
        (when (and (null (e-board-terminal-classifications board))
                   (e-board-terminal-classification-scheduled board))
          ;; Invalidate a callback accepted before this exact inverse.  A
          ;; replacement queue obtains a fresh generation when it is next
          ;; scheduled.
          (setf (e-board-terminal-classification-scheduled board) nil)
          (cl-incf (e-board-terminal-classification-generation board))
          (setf (e-board-terminal-classification-callback-generation board)
                (e-board-terminal-classification-generation board)))
        (setf (e-board-terminal-classification-receipt--removed-p receipt) t))
    t)))

(defun e-board-admission--restore-classification-index
    (receipt index key value detached-slot)
  "Restore one captured classifier INDEX VALUE without touching replacements.
DETACHED-SLOT names the receipt boolean acknowledging the forward remhash.
The restoration is used only while an admission is aborting; a committed
classifier has consumed its source indexes permanently."
  (when (and value (e-board-terminal-classification-receipt-p receipt)
             (pcase detached-slot
               ('invocation
                (e-board-terminal-classification-receipt--invocation-index-detached-p
                 receipt))
               ('aggregation
                (e-board-terminal-classification-receipt--aggregation-index-detached-p
                 receipt))))
    (let ((current (gethash key index)))
      (cond
       ((null current)
        (condition-case err
            (puthash key value index)
          (error
           ;; The captured value is still the only safe restoration target,
           ;; but a postmutation signal remains visible so the enclosing
           ;; admission can retry this exact stage.
           (signal (car err) (cdr err)))))
       ((eq current value) nil)
       (t (signal 'e-board-error
                  (list "Classifier index replacement is authoritative"
                        key))))
    (when (eq (gethash key index) value)
      (pcase detached-slot
        ('invocation
         (setf (e-board-terminal-classification-receipt--invocation-index-detached-p
                receipt)
               nil))
        ('aggregation
         (setf (e-board-terminal-classification-receipt--aggregation-index-detached-p
                receipt)
               nil)))))))

(defun e-board-admission--queue-aggregation-deadline
    (board aggregation &optional admission schedule-function)
  "Queue an exact aggregation deadline without resolving a replacement id."
  (unless (e-board-aggregation-p aggregation)
    (signal 'e-board-error
            (list "Aggregation deadline requires an exact aggregation object"
                  aggregation)))
  (unless (eq (gethash (e-board-aggregation-id aggregation)
                       (e-board-aggregations board))
              aggregation)
    (signal 'e-board-error
            (list "Aggregation deadline is not current" aggregation)))
  (let* ((aggregation aggregation)
         (value aggregation)
         (previous (e-board-aggregation-deadline-tail board))
         (previous-receipt
          (and previous
               (gethash previous
                        (e-board-aggregation-deadline-node-index board))))
         (cell (list value))
         (receipt (e-board-aggregation-deadline-receipt--create
                   :board board :aggregation aggregation :cell cell
                   :previous previous :previous-receipt previous-receipt
                   :head-p (null previous) :forward-stage 'allocated
                   :inverse-stage 'live)))
    (when (and previous
               (not (e-board-aggregation-deadline-receipt-p previous-receipt)))
      (signal 'e-board-error
              (list "Aggregation deadline tail has no exact receipt")))
    (if (e-board-aggregation-admission-p admission)
        (setf (e-board-aggregation-admission-deadline-receipt admission) receipt))
    (setf (e-board-aggregation-deadline-receipt--forward-stage receipt)
          'registering)
    (condition-case err
        (puthash cell receipt (e-board-aggregation-deadline-node-index board))
      (error
       (when (eq (gethash cell (e-board-aggregation-deadline-node-index board))
                 receipt)
         (setf (e-board-aggregation-deadline-receipt--registered-p receipt) t
               (e-board-aggregation-deadline-receipt--forward-stage receipt)
               'registered))
       (signal (car err) (cdr err))))
    (setf (e-board-aggregation-deadline-receipt--registered-p receipt) t
          (e-board-aggregation-deadline-receipt--forward-stage receipt)
          'registered)
    (setf (e-board-aggregation-deadline-receipt--forward-stage receipt) 'linking)
    (if previous
        (let ((before-cdr (cdr previous)))
          (condition-case err
              (setcdr previous cell)
            (error
             (if (and (not (eq (cdr previous) before-cdr))
                      (eq (cdr previous) cell))
                 (progn
                   (setf (e-board-aggregation-deadline-receipt--linked-p receipt)
                         t
                         (e-board-aggregation-deadline-receipt--forward-stage receipt)
                         'linked)
                   (signal (car err) (cdr err)))
               (signal (car err) (cdr err))))))
      (setf (e-board-aggregation-deadlines board) cell))
    (setf (e-board-aggregation-deadline-receipt--linked-p receipt) t
          (e-board-aggregation-deadline-receipt--forward-stage receipt) 'linked)
    (when previous-receipt
      (setf (e-board-aggregation-deadline-receipt--next-receipt previous-receipt)
            receipt))
    (setf (e-board-aggregation-deadline-tail board) cell
          (e-board-aggregation-deadline-receipt--tail-p receipt) t
          (e-board-aggregation-deadline-receipt--forward-stage receipt) 'tailing)
    (setf (e-board-aggregation-deadline-receipt--forward-stage receipt) 'counting)
    (let ((before (e-board-unsettled-routing-count board)))
      (condition-case err
          (progn
            (e-board-admission--adjust-unsettled board 'routing 1)
            (setf (e-board-aggregation-deadline-receipt--queued-p receipt) t
                  (e-board-aggregation-deadline-receipt--counted-p receipt) t
                  (e-board-aggregation-deadline-receipt--forward-stage receipt)
                  'counted))
        (error
         (when (= (e-board-unsettled-routing-count board) (1+ before))
           (setf (e-board-aggregation-deadline-receipt--queued-p receipt) t
                 (e-board-aggregation-deadline-receipt--counted-p receipt) t
                 (e-board-aggregation-deadline-receipt--forward-stage receipt)
                 'counted))
         (signal (car err) (cdr err)))))
    (when schedule-function
      (condition-case err
          (progn (funcall schedule-function board)
                 (setf (e-board-aggregation-deadline-receipt--scheduled-p receipt) t))
        (error
         (when (e-board-aggregation-deadline-scheduled board)
           (setf (e-board-aggregation-deadline-receipt--scheduled-p receipt) t))
         (signal (car err) (cdr err)))))
    receipt))

(defun e-board-admission-queue-aggregation-deadline
    (board aggregation &optional admission schedule-function)
  "Queue an aggregation deadline, retaining exact recovery when standalone.

The public direct path receives an opaque Board admission before it changes the
deadline list or routing count.  Composition passes its existing admission so
the deadline remains part of the surrounding transaction."
  (if (e-board-admission--admission-p admission)
      (e-board-admission--queue-aggregation-deadline
       board aggregation admission schedule-function)
    (let ((standalone (e-board-admission-aggregation-token board))
          receipt)
      (e-board-admission-begin board standalone)
      (condition-case err
          (progn
            (setq receipt
                  (e-board-admission--queue-aggregation-deadline
                   board aggregation standalone schedule-function))
            (setf (e-board-aggregation-admission-committed-p standalone) t)
            (e-board-admission-finish board standalone))
        (error
         (e-board-admission-finish board standalone)
         (condition-case _cleanup-error
             (e-board-admission-abort board standalone)
           (error nil))
         (signal (car err) (cdr err))))
      (e-board-admission-complete board standalone)
      receipt)))

(defun e-board-admission-remove-aggregation-deadline (receipt)
  "Remove one exact aggregation deadline receipt."
  (when (e-board-aggregation-deadline-receipt-p receipt)
    (unless (e-board-aggregation-deadline-receipt--removed-p receipt)
      (let* ((board (e-board-aggregation-deadline-receipt--board receipt))
             (cell (e-board-aggregation-deadline-receipt--cell receipt))
             (previous-receipt
              ;; A neighbouring admission can be aborted first.  Follow the
              ;; exact receipt links to the nearest live node; do not
              ;; rediscover a predecessor by value or by scanning.
              (let ((candidate
                     (e-board-aggregation-deadline-receipt--previous-receipt
                      receipt)))
                (while (and candidate
                             (e-board-aggregation-deadline-receipt--removed-p
                              candidate))
                  (setq candidate
                        (e-board-aggregation-deadline-receipt--previous-receipt
                         candidate)))
                candidate))
             (next-receipt
              (let ((candidate
                     (e-board-aggregation-deadline-receipt--next-receipt
                      receipt)))
                (while (and candidate
                             (e-board-aggregation-deadline-receipt--removed-p
                              candidate))
                  (setq candidate
                        (e-board-aggregation-deadline-receipt--next-receipt
                         candidate)))
                candidate))
             (previous (and previous-receipt
                            (e-board-aggregation-deadline-receipt--cell
                             previous-receipt)))
             (node (gethash cell
                            (e-board-aggregation-deadline-node-index board)))
             (stage (e-board-aggregation-deadline-receipt--inverse-stage receipt)))
        (when (and (e-board-aggregation-deadline-receipt--registered-p receipt)
                   (not (or (eq node receipt)
                            (and (null node)
                                 (memq stage '(unregistering unregistered removed))))))
          (signal 'e-board-error
                  (list "Aggregation deadline receipt is not current" receipt)))
        (unless (memq stage '(unlinked repaired tail count unregistered removed))
          (setf (e-board-aggregation-deadline-receipt--inverse-stage receipt)
                'unlinking)
          (cond
           ((and previous (eq (cdr previous) cell))
            (let ((before-cdr (cdr previous)))
              (condition-case err
                  (setcdr previous (cdr cell))
                (error
                 (if (and (not (eq (cdr previous) before-cdr))
                          (eq (cdr previous) (cdr cell)))
                     (progn
                       (setf (e-board-aggregation-deadline-receipt--linked-p
                              receipt)
                             nil
                             (e-board-aggregation-deadline-receipt--inverse-stage
                              receipt)
                             'unlinked)
                       (signal (car err) (cdr err)))
                   (signal (car err) (cdr err)))))))
           ((and (null previous)
                 (eq (e-board-aggregation-deadlines board) cell))
            (setf (e-board-aggregation-deadlines board) (cdr cell)))
           ((if next-receipt
                (and previous
                     (eq (cdr previous)
                         (e-board-aggregation-deadline-receipt--cell next-receipt)))
              (and (null next-receipt)
                   (eq (e-board-aggregation-deadline-tail board) previous)))
            nil)
           (t (signal 'e-board-error
                     (list "Aggregation deadline exact link is absent" receipt))))
          (setf (e-board-aggregation-deadline-receipt--linked-p receipt) nil)
          (setf (e-board-aggregation-deadline-receipt--inverse-stage receipt)
                'unlinked))
        (unless (memq (e-board-aggregation-deadline-receipt--inverse-stage receipt)
                      '(repaired tail count unregistered removed))
          (when previous-receipt
            (setf (e-board-aggregation-deadline-receipt--next-receipt
                   previous-receipt) next-receipt))
          (when next-receipt
            (setf (e-board-aggregation-deadline-receipt--previous-receipt
                   next-receipt) previous-receipt
                  (e-board-aggregation-deadline-receipt--head-p next-receipt)
                  (null previous-receipt)))
          (setf (e-board-aggregation-deadline-receipt--inverse-stage receipt)
                'repaired))
        (when (or (e-board-aggregation-deadline-receipt--tail-p receipt)
                  (eq (e-board-aggregation-deadline-tail board) cell))
          (setf (e-board-aggregation-deadline-receipt--inverse-stage receipt)
                'tailing)
          (when (eq (e-board-aggregation-deadline-tail board) cell)
            (setf (e-board-aggregation-deadline-tail board) previous))
          (when previous-receipt
            (setf (e-board-aggregation-deadline-receipt--tail-p previous-receipt)
                  t))
          (setf (e-board-aggregation-deadline-receipt--tail-p receipt) nil
                (e-board-aggregation-deadline-receipt--inverse-stage receipt)
                'tail))
        (when (e-board-aggregation-deadline-receipt--queued-p receipt)
          (let ((before (e-board-unsettled-routing-count board)))
            (condition-case err
                (progn
                  (e-board-admission--adjust-unsettled board 'routing -1)
                  (setf (e-board-aggregation-deadline-receipt--queued-p receipt)
                        nil
                        (e-board-aggregation-deadline-receipt--inverse-stage
                         receipt)
                        'count))
              (error
               (when (= (e-board-unsettled-routing-count board) (1- before))
                 (setf (e-board-aggregation-deadline-receipt--queued-p receipt)
                       nil
                       (e-board-aggregation-deadline-receipt--inverse-stage
                        receipt)
                       'count))
               (signal (car err) (cdr err))))))
        (when (or (e-board-aggregation-deadline-receipt--registered-p receipt)
                  (memq (e-board-aggregation-deadline-receipt--forward-stage receipt)
                        '(registering registered)))
          (setf (e-board-aggregation-deadline-receipt--inverse-stage receipt)
                'unregistering)
          (let ((current (gethash cell
                                  (e-board-aggregation-deadline-node-index board))))
            (cond
             ((eq current receipt)
              (condition-case err
                  (remhash cell (e-board-aggregation-deadline-node-index board))
                (error
                 (if (null (gethash cell
                                    (e-board-aggregation-deadline-node-index
                                     board)))
                     (progn
                       (setf (e-board-aggregation-deadline-receipt--registered-p
                              receipt)
                             nil
                             (e-board-aggregation-deadline-receipt--inverse-stage
                              receipt)
                             'unregistered)
                       (signal (car err) (cdr err)))
                   (signal (car err) (cdr err))))))
             ((null current) nil)
             (t (signal 'e-board-error
                       (list "Aggregation deadline node was replaced" receipt))))
            (setf (e-board-aggregation-deadline-receipt--registered-p receipt) nil))
          (setf (e-board-aggregation-deadline-receipt--inverse-stage receipt)
                'unregistered))
        (when (and (null (e-board-aggregation-deadlines board))
                   (e-board-aggregation-deadline-scheduled board))
          (setf (e-board-aggregation-deadline-scheduled board) nil)
          (cl-incf (e-board-aggregation-deadline-generation board))
          (setf (e-board-aggregation-deadline-callback-generation board)
                (e-board-aggregation-deadline-generation board)))
        (setf (e-board-aggregation-deadline-receipt--removed-p receipt) t))
    t)))

(defun e-board-admission-deadline-aggregation (receipt)
  "Return the exact aggregation owned by deadline RECEIPT.

This is the narrow semantic value consumed by the Board deadline reducer.  It
does not expose the receipt's list cell, indexes, generation, or inverse state;
all of those remain private to this owner."
  (when (e-board-aggregation-deadline-receipt-p receipt)
    (e-board-aggregation-deadline-receipt--aggregation receipt)))

(defun e-board-admission--schedule-effect-callback (board generation)
  "Publish one generation-fenced effect callback for BOARD."
  (let ((callback
         (lambda ()
           (when (= generation (e-board-effect-callback-generation board))
             (e-board-admission-drain-effects board generation)))))
    (if-let ((scheduler (e-board-effect-scheduler board)))
        (funcall scheduler callback)
      (run-at-time 0 nil callback))))

(defun e-board-admission--fence-effect-callback (board)
  "Invalidate BOARD's current effect callback authority.

This is used when a scheduler signals after an uncertain publication.  It is
safe whether the callback was accepted, rejected, or already consumed; an old
callback can no longer drain a replacement queue after the generation advance.
"
  (setf (e-board-effects-scheduled board) nil)
  (cl-incf (e-board-effect-schedule-generation board))
  (setf (e-board-effect-callback-generation board)
        (e-board-effect-schedule-generation board)))

(defun e-board-admission--reschedule-effects (board schedule-function)
  "Publish a fresh callback for surviving BOARD effects.

The scheduled bit and generation are established before invoking the fallible
scheduler.  On another scheduler error, the bit is cleared and the new
generation fences any callback that may have been accepted before signalling.
The caller decides which earlier error remains primary.
"
  (when (e-board-pending-effects board)
    (cl-incf (e-board-effect-schedule-generation board))
    (let ((generation (e-board-effect-schedule-generation board)))
      (setf (e-board-effect-callback-generation board) generation
            (e-board-effects-scheduled board) t)
      (condition-case err
          (progn
            (if schedule-function
                (funcall schedule-function board)
              (e-board-admission--schedule-effect-callback board generation))
            t)
        (error
         (e-board-admission--fence-effect-callback board)
         (signal (car err) (cdr err)))))))

(defun e-board-admission--schedule-effect
    (board effect &optional admission schedule-function)
  "Queue EFFECT, optionally attaching its exact receipt to ADMISSION.

SCHEDULE-FUNCTION is a narrow scheduler hook owned by the Board composition
root.  The receipt is acquired before the queue/count/scheduled mutations and
keeps a callback generation so a stale callback cannot drain a replacement."
  (let* ((previous (e-board-pending-effects-tail board))
         (previous-receipt (and previous
                                (gethash previous
                                         (e-board-effect-node-index board))))
         (cell (list effect))
         ;; Every effect has a local node receipt, including ordinary effects
         ;; which are not part of a rollback admission.  This keeps the FIFO
         ;; predecessor exact when an admission effect is appended behind a
         ;; previously queued ordinary effect, without exposing a raw queue to
         ;; another owner.
         (receipt (e-board-effect-receipt--create
                   :board board :effect effect :cell cell
                   :previous previous-receipt
                   :generation (1+ (e-board-effect-schedule-generation board))
                   :forward-stage 'allocated :inverse-stage 'live
                   :head-p (null previous)))
         ;; A drain keeps its scheduling authority while arbitrary effect code
         ;; runs.  Without this separate in-progress bit an effect that queues
         ;; a successor observes the consumed callback as absent and publishes
         ;; a second callback; the drain then publishes another one for the
         ;; same FIFO.  The bit is Board state, not a process-global lock.
         (scheduled-p (and (not (e-board-effects-scheduled board))
                           (not (e-board-effect-draining-p board)))))
    (when (and previous
               (not (e-board-effect-receipt-p previous-receipt)))
      (signal 'e-board-error
              (list "Effect tail has no exact receipt" board)))
    (when (e-board-admission--admission-p admission)
      (e-board-admission--push-receipt admission 'effect receipt))
    (setf (e-board-effect-receipt--forward-stage receipt) 'linking)
    (if previous
        (let ((before-cdr (cdr previous)))
          (condition-case err
            (setcdr previous cell)
            (error
             (if (and (not (eq (cdr previous) before-cdr))
                      (eq (cdr previous) cell))
                 (progn
                   ;; The primitive signalled after installing the exact
                   ;; link.  Record that postcondition before propagating the
                   ;; original error so an inverse can resume safely.
                   (setf (e-board-effect-receipt--linked-p receipt) t
                         (e-board-effect-receipt--forward-stage receipt)
                         'linked)
                   (signal (car err) (cdr err)))
               (signal (car err) (cdr err))))))
      (setf (e-board-pending-effects board) cell))
    (setf (e-board-effect-receipt--linked-p receipt) t
          (e-board-effect-receipt--forward-stage receipt) 'linked)
    (when previous-receipt
      (setf (e-board-effect-receipt--next-receipt previous-receipt) receipt
            (e-board-effect-receipt--tail-p previous-receipt) nil))
    (setf (e-board-pending-effects-tail board) cell
          (e-board-effect-receipt--tail-p receipt) t)
    (condition-case err
        (puthash cell receipt (e-board-effect-node-index board))
      (error
       (when (eq (gethash cell (e-board-effect-node-index board)) receipt)
         (setf (e-board-effect-receipt--registered-p receipt) t
               (e-board-effect-receipt--forward-stage receipt) 'registered))
       (signal (car err) (cdr err))))
    (setf (e-board-effect-receipt--registered-p receipt) t)
    (setf (e-board-effect-receipt--forward-stage receipt) 'counting)
    (let ((before (e-board-unsettled-effect-count board)))
      (condition-case err
          (progn
            (e-board-admission--adjust-unsettled board 'effects 1)
            (setf (e-board-effect-receipt--queued-p receipt) t
                  (e-board-effect-receipt--forward-stage receipt) 'counted))
        (error
         (when (= (e-board-unsettled-effect-count board) (1+ before))
           (setf (e-board-effect-receipt--queued-p receipt) t
                 (e-board-effect-receipt--forward-stage receipt) 'counted))
         (signal (car err) (cdr err)))))
    (when scheduled-p
      (cl-incf (e-board-effect-schedule-generation board))
      (setf (e-board-effect-callback-generation board)
            (e-board-effect-schedule-generation board)
            (e-board-effects-scheduled board) t)
      (setf (e-board-effect-receipt--generation receipt)
            (e-board-effect-schedule-generation board)
            (e-board-effect-receipt--scheduled-p receipt) t)
      (condition-case err
          (progn
            (if schedule-function
                (funcall schedule-function board)
              (e-board-admission--schedule-effect-callback
               board (e-board-effect-schedule-generation board)))
            (setf (e-board-effect-receipt--callback-accepted-p receipt) t))
        (error
         ;; The receipt already owns the queue/count and generation.  Remove
         ;; that exact cell on scheduler failure; an accepted callback is
         ;; fenced by the generation bump performed by the inverse.
         (e-board-admission--fence-effect-callback board)
         (condition-case cleanup-error
             (e-board-admission-remove-effect receipt)
           (error
            ;; Preserve the exact receipt for the enclosing admission's
            ;; retry.  Cleanup faults must not erase its recovery authority;
            ;; the scheduler error remains the initiating error.
            (setf (e-board-effect-receipt--inverse-stage receipt)
                  (or (e-board-effect-receipt--inverse-stage receipt)
                      'pending))
            (ignore cleanup-error)))
         ;; Removing the failed cell must not strand unrelated FIFO effects
         ;; behind a scheduled bit whose callback was never accepted.  A new
         ;; generation owns the successor callback; if its scheduler fails as
         ;; well, the original scheduler error remains authoritative.
         (condition-case reschedule-error
             (e-board-admission--reschedule-effects board schedule-function)
           (error (ignore reschedule-error)))
         (signal (car err) (cdr err)))))
    receipt))

(defun e-board-admission-schedule-effect
    (board effect &optional admission schedule-function)
  "Queue EFFECT, retaining exact recovery when standalone.

An enclosing Board admission is used when supplied.  Direct effect
publication gets a short-lived Board admission before the FIFO cell, node
index, count, and callback generation change, so a later scheduler failure
cannot leave an unowned receipt behind."
  (if (e-board-admission--admission-p admission)
      (e-board-admission--schedule-effect
       board effect admission schedule-function)
    (let ((standalone (e-board-admission-aggregation-token board))
          receipt)
      (e-board-admission-begin board standalone)
      (condition-case err
          (progn
            (setq receipt
                  (e-board-admission--schedule-effect
                   board effect standalone schedule-function))
            (setf (e-board-aggregation-admission-committed-p standalone) t)
            (e-board-admission-finish board standalone))
        (error
         (e-board-admission-finish board standalone)
         (condition-case _cleanup-error
             (e-board-admission-abort board standalone)
           (error nil))
         (signal (car err) (cdr err))))
      (e-board-admission-complete board standalone)
      receipt)))

(defun e-board-admission--effect-raw-linked-p (receipt)
  "Return whether RECEIPT's exact effect cell remains in the FIFO."
  (let* ((board (e-board-effect-receipt--board receipt))
         (previous (e-board-effect-receipt--previous receipt))
        (cell (e-board-effect-receipt--cell receipt)))
    (if previous
        (eq (cdr (e-board-effect-receipt--cell previous)) cell)
      (eq (e-board-pending-effects board) cell))))

(defun e-board-admission-remove-effect (receipt)
  "Remove one exact pending effect receipt with generation fencing."
  (when (e-board-effect-receipt-p receipt)
    (unless (e-board-effect-receipt--removed-p receipt)
      (let* ((board (e-board-effect-receipt--board receipt))
             (cell (e-board-effect-receipt--cell receipt))
             (previous
              ;; A neighbouring admission can be aborted first.  Its exact
              ;; receipt links are the only authority for the live inverse;
              ;; skip only receipts already acknowledged as removed.
              (let ((candidate (e-board-effect-receipt--previous receipt)))
                (while (and candidate
                             (e-board-effect-receipt--removed-p candidate))
                  (setq candidate
                        (e-board-effect-receipt--previous candidate)))
                candidate))
             (next-receipt
              (let ((candidate
                     (e-board-effect-receipt--next-receipt receipt)))
                (while (and candidate
                             (e-board-effect-receipt--removed-p candidate))
                  (setq candidate
                        (e-board-effect-receipt--next-receipt candidate)))
                candidate))
             (stage (e-board-effect-receipt--inverse-stage receipt)))
        (unless (memq stage '(unlinked repaired tail count removed))
          (setf (e-board-effect-receipt--inverse-stage receipt) 'unlinking)
          (cond
           ((and previous (eq (cdr (e-board-effect-receipt--cell previous)) cell))
            (let* ((previous-cell (e-board-effect-receipt--cell previous))
                   (before-cdr (cdr previous-cell)))
              (condition-case err
                (setcdr previous-cell (cdr cell))
                (error
                 (if (and (not (eq (cdr previous-cell) before-cdr))
                          (eq (cdr previous-cell) (cdr cell)))
                     (progn
                       (setf (e-board-effect-receipt--linked-p receipt) nil
                             (e-board-effect-receipt--inverse-stage receipt)
                             'unlinked)
                       (signal (car err) (cdr err)))
                   (signal (car err) (cdr err)))))))
           ((and (null previous) (eq (e-board-pending-effects board) cell))
            (setf (e-board-pending-effects board) (cdr cell)))
           ((and next-receipt previous
                 (eq (cdr (e-board-effect-receipt--cell previous))
                     (e-board-effect-receipt--cell next-receipt))) nil)
           ((and (null next-receipt)
                 (eq (e-board-pending-effects-tail board) previous)) nil)
           (t (signal 'e-board-error
                     (list "Effect receipt exact link is absent" receipt))))
          ;; Physical unlink is independent of unsettled-count removal.  Keep
          ;; the queued flag until the count inverse acknowledges its own
          ;; postcondition; otherwise a retry would skip the decrement and
          ;; strand Board quiescence.
          (setf (e-board-effect-receipt--linked-p receipt) nil
                (e-board-effect-receipt--inverse-stage receipt) 'unlinked))
        (unless (memq (e-board-effect-receipt--inverse-stage receipt)
                      '(repaired tail count removed))
          (when previous
            (setf (e-board-effect-receipt--next-receipt previous) next-receipt))
          (when next-receipt
            (setf (e-board-effect-receipt--previous next-receipt) previous
                  (e-board-effect-receipt--head-p next-receipt) (null previous)))
          (setf (e-board-effect-receipt--inverse-stage receipt) 'repaired))
        (when (or (e-board-effect-receipt--tail-p receipt)
                  (eq (e-board-pending-effects-tail board) cell))
          (when (eq (e-board-pending-effects-tail board) cell)
            (setf (e-board-pending-effects-tail board)
                  (and previous (e-board-effect-receipt--cell previous))))
          (when previous
            (setf (e-board-effect-receipt--tail-p previous) t))
          (setf (e-board-effect-receipt--tail-p receipt) nil
                (e-board-effect-receipt--inverse-stage receipt) 'tail))
        (when (e-board-effect-receipt--queued-p receipt)
          (let ((before (e-board-unsettled-effect-count board)))
            (condition-case err
                (progn
                  (e-board-admission--adjust-unsettled board 'effects -1)
                  (setf (e-board-effect-receipt--queued-p receipt) nil))
              (error
               (when (= (e-board-unsettled-effect-count board) (1- before))
                 (setf (e-board-effect-receipt--queued-p receipt) nil))
               (signal (car err) (cdr err))))))
        (when (or (e-board-effect-receipt--registered-p receipt)
                  (eq (gethash cell (e-board-effect-node-index board)) receipt))
          (condition-case err
            (remhash cell (e-board-effect-node-index board))
            (error
             (if (null (gethash cell (e-board-effect-node-index board)))
                 (progn
                   (setf (e-board-effect-receipt--registered-p receipt) nil
                         (e-board-effect-receipt--inverse-stage receipt)
                         'unregistered)
                   (signal (car err) (cdr err)))
               (signal (car err) (cdr err)))))
          (setf (e-board-effect-receipt--registered-p receipt) nil))
        (unless (e-board-pending-effects board)
          (when (= (e-board-effect-receipt--generation receipt)
                   (e-board-effect-callback-generation board))
            (setf (e-board-effects-scheduled board) nil)
            (cl-incf (e-board-effect-schedule-generation board))
            (setf (e-board-effect-callback-generation board)
                  (e-board-effect-schedule-generation board))))
        (setf (e-board-effect-receipt--removed-p receipt) t
              (e-board-effect-receipt--inverse-stage receipt) 'removed))
    t)))

(defun e-board-admission--effect-receipt-for-cell (board cell)
  "Return the exact effect receipt for CELL, or nil for a normal effect."
  (gethash cell (e-board-effect-node-index board)))

(defun e-board-admission-drain-effects (board &optional generation)
  "Apply one bounded, generation-fenced effect page."
  (when (or (null generation)
            (= generation (e-board-effect-callback-generation board)))
    (setf (e-board-effect-draining-p board) t)
    (unwind-protect
        ;; Freeze the page boundary before invoking arbitrary effect code.
        ;; Effects queued reentrantly belong to the next scheduler turn even
        ;; when this page has spare capacity.  The unsettled count is the
        ;; Board-owned constant-time queue cardinality; it is decremented for
        ;; each consumed cell and incremented only by enqueue.
        (let ((remaining
               (min e-board-effect-drain-limit
                    (e-board-unsettled-effect-count board))))
          (while (and (> remaining 0) (e-board-pending-effects board))
            (let* ((cell (e-board-pending-effects board))
                   (effect (car cell))
                   (receipt (e-board-admission--effect-receipt-for-cell board cell)))
              (setf (e-board-pending-effects board) (cdr cell))
              (when (eq (e-board-pending-effects-tail board) cell)
                (setf (e-board-pending-effects-tail board) nil))
              (when receipt
                ;; Mark the exact cell terminal before invoking arbitrary effect
                ;; code so reentrant abort cannot remove a later replacement.
                (remhash cell (e-board-effect-node-index board))
                (when-let ((next-receipt
                            (gethash (cdr cell) (e-board-effect-node-index board))))
                  (setf (e-board-effect-receipt--previous next-receipt) nil
                        (e-board-effect-receipt--head-p next-receipt) t))
                (setf (e-board-effect-receipt--queued-p receipt) nil
                      (e-board-effect-receipt--removed-p receipt) t
                      (e-board-effect-receipt--inverse-stage receipt) 'removed))
              (e-board-admission--adjust-unsettled board 'effects -1)
              (condition-case err
                  (funcall effect)
                (error
                 ;; The effect drain owner records an unexpected effect failure
                 ;; in the event log; it never retries arbitrary user code inline.
                 (e-board-admission-append-event
                  board 'effect-drain-failed (list :error err))))
              (cl-decf remaining)))
          ;; The callback which entered this drain has been consumed.  Only now
          ;; clear its scheduling bit and publish one successor callback for any
          ;; remaining FIFO (including effects queued reentrantly by a callback).
          (setf (e-board-effects-scheduled board) nil)
          (when (e-board-pending-effects board)
            (cl-incf (e-board-effect-schedule-generation board))
            (setf (e-board-effect-callback-generation board)
                  (e-board-effect-schedule-generation board)
                  (e-board-effects-scheduled board) t)
            (e-board-admission--schedule-effect-callback
             board (e-board-effect-callback-generation board))))
      (setf (e-board-effect-draining-p board) nil))))

(defun e-board-admission--abort-work (board admission)
  "Apply ADMISSION's exact inverse for a staged work relation."
  (let ((work (e-board-work-admission-work admission))
        (invocation (e-board-work-admission-invocation admission))
        (cleanup-error nil)
        (incomplete nil))
    (dolist (receipt (e-board-work-admission-effect-receipts admission))
      (when (and (e-board-effect-receipt-p receipt)
                 (not (e-board-effect-receipt--removed-p receipt)))
        (condition-case err
            (e-board-admission-remove-effect receipt)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-effect-receipt--removed-p receipt)
          (setq incomplete t))))
    (dolist (receipt (e-board-work-admission-classification-receipts admission))
      (when (and (e-board-terminal-classification-receipt-p receipt)
                 (not (e-board-terminal-classification-receipt--removed-p receipt)))
        (condition-case err
            (e-board-admission-remove-terminal-classification receipt)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-terminal-classification-receipt--removed-p receipt)
          (setq incomplete t))
        (dolist (spec
                 `((invocation
                    ,(e-board-terminal-classification-receipt--invocation-index
                      receipt)
                    ,(e-board-terminal-classification-receipt--invocation-index-value
                      receipt))
                   (aggregation
                    ,(e-board-terminal-classification-receipt--aggregation-index
                      receipt)
                    ,(e-board-terminal-classification-receipt--aggregation-index-value
                      receipt))))
          (condition-case err
              (e-board-admission--restore-classification-index
               receipt (nth 1 spec)
               (e-board-terminal-classification-work-id
                (e-board-terminal-classification-receipt--record receipt))
               (nth 2 spec) (car spec))
            (error (setq cleanup-error (or cleanup-error err))))
          (when (pcase (car spec)
                  ('invocation
                   (e-board-terminal-classification-receipt--invocation-index-detached-p
                    receipt))
                  ('aggregation
                   (e-board-terminal-classification-receipt--aggregation-index-detached-p
                    receipt)))
            (setq incomplete t)))))
    (dolist (receipt (e-board-work-admission-index-receipts admission))
      (when (and (e-board-index-receipt-p receipt)
                 (not (e-board-index-receipt--removed-p receipt)))
        (condition-case err
            (e-board-admission-remove-index receipt)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-index-receipt--removed-p receipt)
          (setq incomplete t))))
    (dolist (receipt (e-board-work-admission-event-receipts admission))
      (when (and (e-board-event-receipt-p receipt)
                 (not (e-board-event-receipt--removed-p receipt)))
        (condition-case err
            (e-board-admission-remove-event receipt)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-event-receipt--removed-p receipt)
          (setq incomplete t))))
    (unless incomplete
      (when (and (e-board-work-admission-work-owned-p admission)
                 (e-board-work-p work)
                 (eq (gethash (e-board-work-id work)
                              (e-board-work-table board)) work))
        (remhash (e-board-work-id work) (e-board-work-table board)))
      (when (and (e-board-invocation-p invocation)
                 (eq (gethash (e-board-invocation-id invocation)
                              (e-board-invocations board)) invocation))
        (remhash (e-board-invocation-id invocation)
                 (e-board-invocations board)))
      (when (and (e-board-work-p work)
                 (e-board-work-publication-observer work))
        (condition-case err
            (e-work-remove-publication-observer
             (e-board-work-handle work)
             (e-board-work-publication-observer work))
          (error (setq cleanup-error (or cleanup-error err)))))
      (unless cleanup-error
        (setf (e-board-work-admission-aborted-p admission) t)))
    (e-board-admission--set-cleanup admission
                                     (and (not incomplete) (null cleanup-error)))
    (if cleanup-error
        (signal (car cleanup-error) (cdr cleanup-error))
      (when incomplete
        (signal 'e-board-admission-pending
                (list "Work admission inverse remains pending" admission))))
    (when (e-board-admission--cleanup-complete-p admission)
      (e-board-admission--remove-pending board admission))
    admission))

(defun e-board-admission--abort-aggregation (board admission)
  "Apply ADMISSION's exact inverse for a staged aggregation relation."
  (let* ((aggregation (e-board-aggregation-admission-aggregation admission))
         (cleanup-error nil)
         (incomplete nil))
    (when-let ((timer (e-board-aggregation-admission-timer admission)))
      (condition-case err
          (progn (cancel-timer timer)
                 (setf (e-board-aggregation-admission-timer admission) nil)
                 (when (e-board-aggregation-p aggregation)
                   (setf (e-board-aggregation-timer aggregation) nil)))
        (error (setq cleanup-error (or cleanup-error err))
               (setq incomplete t))))
    (when-let ((deadline
                (e-board-aggregation-admission-deadline-receipt admission)))
      (unless (e-board-aggregation-deadline-receipt--removed-p deadline)
        (condition-case err
            (e-board-admission-remove-aggregation-deadline deadline)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-aggregation-deadline-receipt--removed-p deadline)
          (setq incomplete t))))
    (dolist (receipt (e-board-aggregation-admission-effect-receipts admission))
      (when (and (e-board-effect-receipt-p receipt)
                 (not (e-board-effect-receipt--removed-p receipt)))
        (condition-case err
            (e-board-admission-remove-effect receipt)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-effect-receipt--removed-p receipt)
          (setq incomplete t))))
    (when (e-board-aggregation-p aggregation)
      (when (eq (gethash (e-board-aggregation-id aggregation)
                         (e-board-aggregations board)) aggregation)
        (setf (e-board-aggregation-state aggregation) 'cancelled))
      (when-let ((activation-id (e-board-aggregation-activation-id aggregation)))
        (when-let ((activation (gethash activation-id
                                       (e-board-activations board))))
          (when (and (eq activation
                           (e-board-aggregation-admission-activation admission))
                     (eq (e-board-activation-state activation) 'prepared))
            (setf (e-board-activation-state activation) 'cancelled)
            (remhash activation-id (e-board-activations board))))))
    (dolist (receipt
             (e-board-aggregation-admission-classification-receipts admission))
      (when (and (e-board-terminal-classification-receipt-p receipt)
                 (not (e-board-terminal-classification-receipt--removed-p receipt)))
        (condition-case err
            (e-board-admission-remove-terminal-classification receipt)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-terminal-classification-receipt--removed-p receipt)
          (setq incomplete t))
        (dolist (spec
                 `((invocation
                    ,(e-board-terminal-classification-receipt--invocation-index
                      receipt)
                    ,(e-board-terminal-classification-receipt--invocation-index-value
                      receipt))
                   (aggregation
                    ,(e-board-terminal-classification-receipt--aggregation-index
                      receipt)
                    ,(e-board-terminal-classification-receipt--aggregation-index-value
                      receipt))))
          (condition-case err
              (e-board-admission--restore-classification-index
               receipt (nth 1 spec)
               (e-board-terminal-classification-work-id
                (e-board-terminal-classification-receipt--record receipt))
               (nth 2 spec) (car spec))
            (error (setq cleanup-error (or cleanup-error err))))
          (when (pcase (car spec)
                  ('invocation
                   (e-board-terminal-classification-receipt--invocation-index-detached-p
                    receipt))
                  ('aggregation
                   (e-board-terminal-classification-receipt--aggregation-index-detached-p
                    receipt)))
            (setq incomplete t)))))
    (dolist (receipt (e-board-aggregation-admission-index-receipts admission))
      (when (and (e-board-index-receipt-p receipt)
                 (not (e-board-index-receipt--removed-p receipt)))
        (condition-case err
            (e-board-admission-remove-index receipt)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-index-receipt--removed-p receipt)
          (setq incomplete t))))
    (dolist (receipt (e-board-aggregation-admission-event-receipts admission))
      (when (and (e-board-event-receipt-p receipt)
                 (not (e-board-event-receipt--removed-p receipt)))
        (condition-case err
            (e-board-admission-remove-event receipt)
          (error (setq cleanup-error (or cleanup-error err))))
        (unless (e-board-event-receipt--removed-p receipt)
          (setq incomplete t))))
    (unless incomplete
      (when (and (e-board-aggregation-p aggregation)
                 (eq (gethash (e-board-aggregation-id aggregation)
                              (e-board-aggregations board)) aggregation))
        (remhash (e-board-aggregation-id aggregation)
                 (e-board-aggregations board)))
      (setf (e-board-aggregation-admission-aborted-p admission) t))
    (e-board-admission--set-cleanup admission
                                     (and (not incomplete) (null cleanup-error)))
    (if cleanup-error
        (signal (car cleanup-error) (cdr cleanup-error))
      (when incomplete
        (signal 'e-board-admission-pending
                (list "Aggregation admission inverse remains pending" admission))))
    (when (e-board-admission--cleanup-complete-p admission)
      (e-board-admission--remove-pending board admission))
    admission))

(defun e-board-admission-abort (board admission)
  "Abort exact ADMISSION, retaining it when any inverse remains pending."
  (unless (e-board-admission--admission-p admission)
    (signal 'wrong-type-argument (list 'e-board-admission-p admission)))
  (let ((owner-board (if (e-board-work-admission-p admission)
                         (e-board-work-admission-board admission)
                       (e-board-aggregation-admission-board admission))))
    (when (and owner-board (not (eq owner-board board)))
      (signal 'e-board-error (list "Admission token belongs to another board")))
    ;; A direct nested abort is a related owner entry, not permission to
    ;; mutate an admission whose outer stack is still between fallible
    ;; primitives.  Fence it and let the outer postcheck perform the exact
    ;; inverse.  All normal error handlers finish their frame first.
    (when (e-board-admission--pending-in-flight-p admission)
      (e-board-admission--set-cancel-requested admission)
      (signal 'e-board-admission-pending
              (list "Cannot abort an in-flight Board admission" admission)))
    (if (e-board-work-admission-p admission)
        (progn (setf (e-board-work-admission-board admission) board)
               (e-board-admission--abort-work board admission))
      (progn (setf (e-board-aggregation-admission-board admission) board)
             (e-board-admission--abort-aggregation board admission)))))

(provide 'e-board-admission)

;;; e-board-admission.el ends here

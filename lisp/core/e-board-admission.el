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
  prefix-p removed-p compact-node compact-map-p compact-stage previous-ack-p
  next-ack-p)

(cl-defstruct (e-board-index-receipt
               (:constructor e-board-index-receipt--create)
               (:conc-name e-board-index-receipt--))
  "Exact receipt for one work-index queue cell."
  index work-id subscription-id queue cell previous previous-node root-node next-node
  queue-created-p forward-stage inverse-stage registered-p linked-p head-p
  tail-p mapped-p removed-p compact-node compact-map-p compact-stage
  previous-ack-p next-ack-p)

(cl-defstruct (e-board-event-link-node
               (:constructor e-board-event-link-node--create)
               (:conc-name e-board-event-link-node--))
  "Compact permanent topology for a committed event.

The full event receipt above is a transaction object and may carry inverse
progress, payload, and Board ownership for only one admission.  Once that
admission is acknowledged, the node index retains this small link value: raw
cell identity plus exact neighbour topology.  It deliberately has no
transaction or event payload state."
  cell previous previous-node root-node next-node linked-p head-p tail-p)

(cl-defstruct (e-board-index-link-node
               (:constructor e-board-index-link-node--create)
               (:conc-name e-board-index-link-node--))
  "Compact permanent topology for a committed work-index queue cell.

The node is its own root authority.  A committed queue never retains the
rollback receipt that created it, so no per-queue receipt is reachable through
this value."
  queue cell previous previous-node next-node linked-p head-p tail-p)

(defun e-board-admission--event-node-p (node)
  "Return non-nil when NODE is a live receipt or compact event node."
  (or (e-board-event-receipt-p node)
      (e-board-event-link-node-p node)))

(defun e-board-admission--event-node-cell (node)
  "Return NODE's exact event cell."
  (cond ((e-board-event-receipt-p node) (e-board-event-receipt--cell node))
        ((e-board-event-link-node-p node) (e-board-event-link-node--cell node))
        (t nil)))

(defun e-board-admission--event-node-previous (node)
  "Return NODE's captured predecessor cell."
  (cond ((e-board-event-receipt-p node) (e-board-event-receipt--previous node))
        ((e-board-event-link-node-p node) (e-board-event-link-node--previous node))
        (t nil)))

(defun e-board-admission--event-node-previous-node (node)
  "Return NODE's exact predecessor node."
  (cond ((e-board-event-receipt-p node)
         (e-board-event-receipt--previous-node node))
        ((e-board-event-link-node-p node)
         (e-board-event-link-node--previous-node node))
        (t nil)))

(defun e-board-admission--event-node-next-node (node)
  "Return NODE's exact successor node."
  (cond ((e-board-event-receipt-p node) (e-board-event-receipt--next-node node))
        ((e-board-event-link-node-p node) (e-board-event-link-node--next-node node))
        (t nil)))

(defun e-board-admission--event-node-root-node (node)
  "Return NODE's exact root node."
  (cond ((e-board-event-receipt-p node) (e-board-event-receipt--root-node node))
        ((e-board-event-link-node-p node) (e-board-event-link-node--root-node node))
        (t nil)))

(defun e-board-admission--event-node-linked-p (node)
  "Return whether NODE is acknowledged as linked."
  (cond ((e-board-event-receipt-p node) (e-board-event-receipt--linked-p node))
        ((e-board-event-link-node-p node) (e-board-event-link-node--linked-p node))
        (t nil)))

(defun e-board-admission--event-node-head-p (node)
  "Return whether NODE is the exact event head."
  (cond ((e-board-event-receipt-p node) (e-board-event-receipt--head-p node))
        ((e-board-event-link-node-p node) (e-board-event-link-node--head-p node))
        (t nil)))

(defun e-board-admission--event-node-tail-p (node)
  "Return whether NODE is the exact event tail."
  (cond ((e-board-event-receipt-p node) (e-board-event-receipt--tail-p node))
        ((e-board-event-link-node-p node) (e-board-event-link-node--tail-p node))
        (t nil)))

(defun e-board-admission--event-node-set-next (node value)
  "Set NODE's exact successor to VALUE."
  (cond ((e-board-event-receipt-p node)
         (setf (e-board-event-receipt--next-node node) value))
        ((e-board-event-link-node-p node)
         (setf (e-board-event-link-node--next-node node) value))
        (t (signal 'e-board-error (list "Invalid event link node" node)))))

(defun e-board-admission--event-node-set-previous (node value)
  "Set NODE's captured predecessor cell to VALUE."
  (cond ((e-board-event-receipt-p node)
         (setf (e-board-event-receipt--previous node) value))
        ((e-board-event-link-node-p node)
         (setf (e-board-event-link-node--previous node) value))
        (t (signal 'e-board-error (list "Invalid event link node" node)))))

(defun e-board-admission--event-node-set-previous-node (node value)
  "Set NODE's exact predecessor node to VALUE."
  (cond ((e-board-event-receipt-p node)
         (setf (e-board-event-receipt--previous-node node) value))
        ((e-board-event-link-node-p node)
         (setf (e-board-event-link-node--previous-node node) value))
        (t (signal 'e-board-error (list "Invalid event link node" node)))))

(defun e-board-admission--event-node-set-root-node (node value)
  "Set NODE's exact root node to VALUE."
  (cond ((e-board-event-receipt-p node)
         (setf (e-board-event-receipt--root-node node) value))
        ((e-board-event-link-node-p node)
         (setf (e-board-event-link-node--root-node node) value))
        (t (signal 'e-board-error (list "Invalid event link node" node)))))

(defun e-board-admission--event-node-set-linked (node value)
  "Set NODE's acknowledged linked state to VALUE."
  (cond ((e-board-event-receipt-p node)
         (setf (e-board-event-receipt--linked-p node) value))
        ((e-board-event-link-node-p node)
         (setf (e-board-event-link-node--linked-p node) value))
        (t (signal 'e-board-error (list "Invalid event link node" node)))))

(defun e-board-admission--event-node-set-head (node value)
  "Set NODE's exact head state to VALUE."
  (cond ((e-board-event-receipt-p node)
         (setf (e-board-event-receipt--head-p node) value))
        ((e-board-event-link-node-p node)
         (setf (e-board-event-link-node--head-p node) value))
        (t (signal 'e-board-error (list "Invalid event link node" node)))))

(defun e-board-admission--event-node-set-tail (node value)
  "Set NODE's exact tail state to VALUE."
  (cond ((e-board-event-receipt-p node)
         (setf (e-board-event-receipt--tail-p node) value))
        ((e-board-event-link-node-p node)
         (setf (e-board-event-link-node--tail-p node) value))
        (t (signal 'e-board-error (list "Invalid event link node" node)))))

(defun e-board-admission--index-node-p (node)
  "Return non-nil when NODE is a live receipt or compact index node."
  (or (e-board-index-receipt-p node)
      (e-board-index-link-node-p node)))

(defun e-board-admission--index-node-cell (node)
  "Return NODE's exact queue cell."
  (cond ((e-board-index-receipt-p node) (e-board-index-receipt--cell node))
        ((e-board-index-link-node-p node) (e-board-index-link-node--cell node))
        (t nil)))

(defun e-board-admission--index-node-previous (node)
  "Return NODE's captured predecessor cell."
  (cond ((e-board-index-receipt-p node) (e-board-index-receipt--previous node))
        ((e-board-index-link-node-p node) (e-board-index-link-node--previous node))
        (t nil)))

(defun e-board-admission--index-node-previous-node (node)
  "Return NODE's exact predecessor node."
  (cond ((e-board-index-receipt-p node)
         (e-board-index-receipt--previous-node node))
        ((e-board-index-link-node-p node)
         (e-board-index-link-node--previous-node node))
        (t nil)))

(defun e-board-admission--index-node-next-node (node)
  "Return NODE's exact successor node."
  (cond ((e-board-index-receipt-p node) (e-board-index-receipt--next-node node))
        ((e-board-index-link-node-p node) (e-board-index-link-node--next-node node))
        (t nil)))

(defun e-board-admission--index-node-root-node (node)
  "Return NODE's exact root node."
  (cond ((e-board-index-receipt-p node) (e-board-index-receipt--root-node node))
        ;; A committed index node is self-authoritative.  The old root pointer
        ;; was only rollback topology and retaining it kept one full receipt
        ;; reachable for every singleton queue.
        ((e-board-index-link-node-p node) node)
        (t nil)))

(defun e-board-admission--index-node-linked-p (node)
  "Return whether NODE is acknowledged as linked."
  (cond ((e-board-index-receipt-p node) (e-board-index-receipt--linked-p node))
        ((e-board-index-link-node-p node) (e-board-index-link-node--linked-p node))
        (t nil)))

(defun e-board-admission--index-node-head-p (node)
  "Return whether NODE is the exact queue head."
  (cond ((e-board-index-receipt-p node) (e-board-index-receipt--head-p node))
        ((e-board-index-link-node-p node) (e-board-index-link-node--head-p node))
        (t nil)))

(defun e-board-admission--index-node-tail-p (node)
  "Return whether NODE is the exact queue tail."
  (cond ((e-board-index-receipt-p node) (e-board-index-receipt--tail-p node))
        ((e-board-index-link-node-p node) (e-board-index-link-node--tail-p node))
        (t nil)))

(defun e-board-admission--index-node-set-next (node value)
  "Set NODE's exact successor to VALUE."
  (cond ((e-board-index-receipt-p node)
         (setf (e-board-index-receipt--next-node node) value))
        ((e-board-index-link-node-p node)
         (setf (e-board-index-link-node--next-node node) value))
        (t (signal 'e-board-error (list "Invalid index link node" node)))))

(defun e-board-admission--index-node-set-previous (node value)
  "Set NODE's captured predecessor cell to VALUE."
  (cond ((e-board-index-receipt-p node)
         (setf (e-board-index-receipt--previous node) value))
        ((e-board-index-link-node-p node)
         (setf (e-board-index-link-node--previous node) value))
        (t (signal 'e-board-error (list "Invalid index link node" node)))))

(defun e-board-admission--index-node-set-previous-node (node value)
  "Set NODE's exact predecessor node to VALUE."
  (cond ((e-board-index-receipt-p node)
         (setf (e-board-index-receipt--previous-node node) value))
        ((e-board-index-link-node-p node)
         (setf (e-board-index-link-node--previous-node node) value))
        (t (signal 'e-board-error (list "Invalid index link node" node)))))

(defun e-board-admission--index-node-set-root-node (node value)
  "Set NODE's exact root node to VALUE."
  (cond ((e-board-index-receipt-p node)
         (setf (e-board-index-receipt--root-node node) value))
        ;; There is no mutable root slot after commit.  Callers that need to
        ;; update a live compact neighbour use its self identity as the root;
        ;; silently accepting that already-proven value keeps this setter
        ;; useful at a receipt/compact boundary without recreating a receipt.
        ((e-board-index-link-node-p node)
         (unless (eq value node)
           (signal 'e-board-error
                   (list "Committed index root is self-authoritative" node value)))
         node)
        (t (signal 'e-board-error (list "Invalid index link node" node)))))

(defun e-board-admission--index-node-set-linked (node value)
  "Set NODE's acknowledged linked state to VALUE."
  (cond ((e-board-index-receipt-p node)
         (setf (e-board-index-receipt--linked-p node) value))
        ((e-board-index-link-node-p node)
         (setf (e-board-index-link-node--linked-p node) value))
        (t (signal 'e-board-error (list "Invalid index link node" node)))))

(defun e-board-admission--index-node-set-head (node value)
  "Set NODE's exact head state to VALUE."
  (cond ((e-board-index-receipt-p node)
         (setf (e-board-index-receipt--head-p node) value))
        ((e-board-index-link-node-p node)
         (setf (e-board-index-link-node--head-p node) value))
        (t (signal 'e-board-error (list "Invalid index link node" node)))))

(defun e-board-admission--index-node-set-tail (node value)
  "Set NODE's exact tail state to VALUE."
  (cond ((e-board-index-receipt-p node)
         (setf (e-board-index-receipt--tail-p node) value))
        ((e-board-index-link-node-p node)
         (setf (e-board-index-link-node--tail-p node) value))
        (t (signal 'e-board-error (list "Invalid index link node" node)))))

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
  finalized-p work-owned-p runtime-admission-record)

(cl-defstruct (e-board-aggregation-admission
               (:constructor e-board-aggregation-admission--create)
               (:conc-name e-board-aggregation-admission-))
  "Opaque exact inverse for one staged aggregation admission."
  board aggregation event-receipts index-receipts timer deadline-receipt
  classification-receipts effect-receipts activation map-installed-p
  committed-p aborted-p cleanup-complete-p in-flight-p cancel-requested-p
  finalized-p runtime-admission-record)

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

(defun e-board-admission-active-p (board admission)
  "Return whether ADMISSION still owns an active Board transaction.

This is a narrow authority observation for semantic reducers that retain an
admission across a fallible callback.  It intentionally reports only exact
membership and cancellation, not the lower receipt representation."
  (and (e-board-p board)
       (e-board-admission--admission-p admission)
       (e-board-pending-admissions board)
       (eq (gethash admission (e-board-pending-admissions board)) t)
       (not (e-board-admission--cancel-requested-p admission))))

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

(defun e-board-admission--standalone-postcheck (board admission)
  "Require that standalone ADMISSION still owns its in-flight frame.

The low-level receipt operation may call an observer which enters a related
Board operation.  That entry fences the outer token and can be caught by the
observer, so a normal return from the primitive is not sufficient evidence of
success.  This check runs before the wrapper commits or reports success."
  (unless (and (e-board-admission--pending-in-flight-p admission)
               (not (e-board-admission--cancel-requested-p admission))
               (e-board-pending-admissions board)
               (eq (gethash admission (e-board-pending-admissions board)) t))
    (signal 'e-board-admission-pending
            (list "Standalone Board admission lost active authority" admission)))
  admission)

(defun e-board-admission--finalize-event-node (receipt)
  "Retain only the link state needed by a committed event node.

An ordinary event is part of the immutable log for the Board lifetime, while
its admission receipt is only needed until the append transaction commits.
Keep the exact cell/neighbour topology used by a later append or a staged
neighbour inverse, but drop the Board/event payload and all rollback progress
flags.  This is intentionally an event-specific compaction operation, not a
generic shared state bag.  The compact map and both neighbour acknowledgements
are explicit resumable stages: a callback may signal after any one mutation,
and the next call repairs the exact published node before releasing the
receipt."
  (if (not (e-board-event-receipt-p receipt))
      receipt
    (let* ((board (e-board-event-receipt--board receipt))
           (cell (e-board-event-receipt--cell receipt))
           (index (and (e-board-p board) (e-board-event-node-index board)))
           (node (e-board-event-receipt--compact-node receipt)))
      (unless node
        (let ((root (e-board-event-receipt--root-node receipt)))
          (when (e-board-event-receipt-p root)
            (setq root
                  (or (e-board-event-receipt--compact-node root) root)))
          ;; A root receipt is a staging value, never a committed topology
          ;; value.  Build the compact node first, then make a root point to
          ;; itself so no committed node retains the full receipt.
          (setq node
                (e-board-event-link-node--create
                 :cell cell
                 :previous (e-board-event-receipt--previous receipt)
                 :previous-node (e-board-event-receipt--previous-node receipt)
                 :root-node root
                 :next-node (e-board-event-receipt--next-node receipt)
                 :linked-p (e-board-event-receipt--linked-p receipt)
                 :head-p (e-board-event-receipt--head-p receipt)
                 :tail-p (e-board-event-receipt--tail-p receipt)))
          (when (or (null root) (eq root receipt))
            (setf (e-board-event-link-node--root-node node) node))
          (setf (e-board-event-receipt--compact-node receipt) node)))
      (unless (and (e-board-p board) (hash-table-p index))
        (signal 'e-board-error
                (list "Committed event has no exact node index" receipt)))
      (let ((current (gethash cell index)))
        (cond
         ((eq current node)
          (setf (e-board-event-receipt--compact-stage receipt) 'map
                (e-board-event-receipt--compact-map-p receipt) t))
         ((eq current receipt)
          (condition-case err
              (puthash cell node index)
            (error
             ;; A postmutation signal is still a visible failed stage; the
             ;; exact node remains authoritative for the retry.
             (if (eq (gethash cell index) node)
                 (progn
                   (setf (e-board-event-receipt--compact-stage receipt) 'map
                         (e-board-event-receipt--compact-map-p receipt) t)
                   (signal (car err) (cdr err)))
               (signal (car err) (cdr err)))))
          (unless (eq (gethash cell index) node)
            (signal 'e-board-error
                    (list "Committed event node did not compact" cell)))
          (setf (e-board-event-receipt--compact-stage receipt) 'map
                (e-board-event-receipt--compact-map-p receipt) t))
         ((null current)
          (signal 'e-board-error
                  (list "Committed event node disappeared before compaction"
                        cell)))
         (t
          (signal 'e-board-error
                  (list "Committed event node was replaced" cell current)))))
      ;; Retarget the exact predecessor only after the compact map is
      ;; authoritative.  The postcondition, not the setter's return, is the
      ;; acknowledgement that permits receipt release.
      (unless (e-board-event-receipt--previous-ack-p receipt)
        (let ((previous-node (e-board-event-receipt--previous-node receipt)))
          (if (null previous-node)
              (setf (e-board-event-receipt--previous-ack-p receipt) t)
            (let ((actual (e-board-admission--event-node-next-node previous-node)))
              (cond
               ((eq actual node)
                (setf (e-board-event-receipt--previous-ack-p receipt) t))
               ((eq actual receipt)
                (condition-case err
                    (e-board-admission--event-node-set-next previous-node node)
                  (error
                   (if (eq (e-board-admission--event-node-next-node previous-node)
                           node)
                       (progn
                         (setf (e-board-event-receipt--previous-ack-p receipt) t)
                         (signal (car err) (cdr err)))
                     (signal (car err) (cdr err)))))
                (unless (eq (e-board-admission--event-node-next-node previous-node)
                            node)
                  (signal 'e-board-error
                          (list "Committed event predecessor was not repaired"
                                cell)))
                (setf (e-board-event-receipt--previous-ack-p receipt) t))
               (t
                (signal 'e-board-error
                        (list "Committed event predecessor has replacement"
                              cell previous-node actual))))))))
      ;; Retarget the exact successor in two separately acknowledged steps.
      ;; This is normally nil for append-at-tail, but is required for a
      ;; middle-node completion/recovery and is intentionally testable.
      (unless (e-board-event-receipt--next-ack-p receipt)
        (let ((next-node (e-board-event-receipt--next-node receipt)))
          (if (null next-node)
              (setf (e-board-event-receipt--next-ack-p receipt) t)
            (let ((actual (e-board-admission--event-node-previous-node next-node)))
              (unless (eq actual node)
                (unless (eq actual receipt)
                  (signal 'e-board-error
                          (list "Committed event successor has replacement"
                                cell next-node actual)))
                (condition-case err
                    (e-board-admission--event-node-set-previous-node next-node node)
                  (error
                   (if (eq (e-board-admission--event-node-previous-node next-node)
                           node)
                       (signal (car err) (cdr err))
                     (signal (car err) (cdr err))))))
            (unless (eq (e-board-admission--event-node-previous-node next-node)
                        node)
              (signal 'e-board-error
                      (list "Committed event successor was not repaired" cell)))
            (let ((previous (e-board-event-receipt--previous receipt)))
              (unless (eq (e-board-admission--event-node-previous next-node)
                          previous)
                (condition-case err
                    (e-board-admission--event-node-set-previous next-node previous)
                  (error
                   (if (eq (e-board-admission--event-node-previous next-node)
                           previous)
                       (signal (car err) (cdr err))
                     (signal (car err) (cdr err))))))
              (unless (eq (e-board-admission--event-node-previous next-node)
                          previous)
                (signal 'e-board-error
                        (list "Committed event successor cell was not repaired"
                              cell))))
            (setf (e-board-event-receipt--next-ack-p receipt) t))))
      (unless (and (e-board-event-receipt--compact-map-p receipt)
                   (e-board-event-receipt--previous-ack-p receipt)
                   (e-board-event-receipt--next-ack-p receipt))
        (signal 'e-board-error
                (list "Committed event compact acknowledgement incomplete" cell)))
      (setf (e-board-event-receipt--board receipt) nil
            (e-board-event-receipt--event receipt) nil
            (e-board-event-receipt--cell receipt) nil
            (e-board-event-receipt--previous receipt) nil
            (e-board-event-receipt--previous-node receipt) nil
            (e-board-event-receipt--root-node receipt) nil
            (e-board-event-receipt--next-node receipt) nil
            (e-board-event-receipt--compact-node receipt) nil
            (e-board-event-receipt--forward-stage receipt) nil
            (e-board-event-receipt--inverse-stage receipt) nil
            (e-board-event-receipt--registered-p receipt) nil
            (e-board-event-receipt--prefix-p receipt) nil
            (e-board-event-receipt--count-p receipt) nil
            (e-board-event-receipt--prefix-value receipt) nil
            (e-board-event-receipt--compact-map-p receipt) nil
            (e-board-event-receipt--compact-stage receipt) 'done
            (e-board-event-receipt--previous-ack-p receipt) nil
            (e-board-event-receipt--next-ack-p receipt) nil)
      node))))

(defun e-board-admission--finalize-index-node (receipt)
  "Retain only queue-link state for a committed work-index node.

The queue may outlive the admission and be appended to again, so its exact
cell, queue neighbours, and link flags remain.  Identity, mapping, and inverse
progress are rollback-only and can be released after the enclosing admission
has committed.  The compact map and neighbour links are acknowledged in
separate stages before the receipt is cleared.
"
  (if (not (e-board-index-receipt-p receipt))
      receipt
    (let* ((index (e-board-index-receipt--index receipt))
           (queue (e-board-index-receipt--queue receipt))
           (cell (e-board-index-receipt--cell receipt))
           (node (e-board-index-receipt--compact-node receipt)))
      (unless node
        ;; The old root pointer is rollback-only.  A committed index node is
        ;; self-authoritative and therefore does not retain it or its receipt.
        (setq node
              (e-board-index-link-node--create
               :queue queue :cell cell
               :previous (e-board-index-receipt--previous receipt)
               :previous-node (e-board-index-receipt--previous-node receipt)
               :next-node (e-board-index-receipt--next-node receipt)
               :linked-p (e-board-index-receipt--linked-p receipt)
               :head-p (e-board-index-receipt--head-p receipt)
               :tail-p (e-board-index-receipt--tail-p receipt)))
        (setf (e-board-index-receipt--compact-node receipt) node))
      (unless (and (hash-table-p index) (e-board-id-queue-p queue))
        (signal 'e-board-error
                (list "Committed index has no exact queue authority" receipt)))
      (let ((node-index (e-board-id-queue-node-index queue))
            (current (gethash cell (e-board-id-queue-node-index queue))))
        (cond
         ((eq current node)
          (setf (e-board-index-receipt--compact-stage receipt) 'map
                (e-board-index-receipt--compact-map-p receipt) t))
         ((eq current receipt)
          (condition-case err
              (puthash cell node node-index)
            (error
             (if (eq (gethash cell node-index) node)
                 (progn
                   (setf (e-board-index-receipt--compact-stage receipt) 'map
                         (e-board-index-receipt--compact-map-p receipt) t)
                   (signal (car err) (cdr err)))
               (signal (car err) (cdr err)))))
          (unless (eq (gethash cell node-index) node)
            (signal 'e-board-error
                    (list "Committed index node did not compact" cell)))
          (setf (e-board-index-receipt--compact-stage receipt) 'map
                (e-board-index-receipt--compact-map-p receipt) t))
         ((null current)
          (signal 'e-board-error
                  (list "Committed index node disappeared before compaction"
                        cell)))
         (t
          (signal 'e-board-error
                  (list "Committed index node was replaced" cell current)))))
      (unless (e-board-index-receipt--previous-ack-p receipt)
        (let ((previous-node (e-board-index-receipt--previous-node receipt)))
          (if (null previous-node)
              (setf (e-board-index-receipt--previous-ack-p receipt) t)
            (let ((actual (e-board-admission--index-node-next-node previous-node)))
              (cond
               ((eq actual node)
                (setf (e-board-index-receipt--previous-ack-p receipt) t))
               ((eq actual receipt)
                (condition-case err
                    (e-board-admission--index-node-set-next previous-node node)
                  (error
                   (if (eq (e-board-admission--index-node-next-node previous-node)
                           node)
                       (progn
                         (setf (e-board-index-receipt--previous-ack-p receipt) t)
                         (signal (car err) (cdr err)))
                     (signal (car err) (cdr err)))))
                (unless (eq (e-board-admission--index-node-next-node previous-node)
                            node)
                  (signal 'e-board-error
                          (list "Committed index predecessor was not repaired"
                                cell)))
                (setf (e-board-index-receipt--previous-ack-p receipt) t))
               (t
                (signal 'e-board-error
                        (list "Committed index predecessor has replacement"
                              cell previous-node actual))))))))
      (unless (e-board-index-receipt--next-ack-p receipt)
        (let ((next-node (e-board-index-receipt--next-node receipt)))
          (if (null next-node)
              (setf (e-board-index-receipt--next-ack-p receipt) t)
            (let ((actual (e-board-admission--index-node-previous-node next-node)))
              (unless (eq actual node)
                (unless (eq actual receipt)
                  (signal 'e-board-error
                          (list "Committed index successor has replacement"
                                cell next-node actual)))
                (condition-case err
                    (e-board-admission--index-node-set-previous-node next-node node)
                  (error
                   (if (eq (e-board-admission--index-node-previous-node next-node)
                           node)
                       (signal (car err) (cdr err))
                     (signal (car err) (cdr err))))))
            (unless (eq (e-board-admission--index-node-previous-node next-node)
                        node)
              (signal 'e-board-error
                      (list "Committed index successor was not repaired" cell)))
            (let ((previous (e-board-index-receipt--previous receipt)))
              (unless (eq (e-board-admission--index-node-previous next-node)
                          previous)
                (condition-case err
                    (e-board-admission--index-node-set-previous next-node previous)
                  (error
                   (if (eq (e-board-admission--index-node-previous next-node)
                           previous)
                       (signal (car err) (cdr err))
                     (signal (car err) (cdr err))))))
              (unless (eq (e-board-admission--index-node-previous next-node)
                          previous)
                (signal 'e-board-error
                        (list "Committed index successor cell was not repaired"
                              cell))))
            (setf (e-board-index-receipt--next-ack-p receipt) t))))
      (unless (and (e-board-index-receipt--compact-map-p receipt)
                   (e-board-index-receipt--previous-ack-p receipt)
                   (e-board-index-receipt--next-ack-p receipt))
        (signal 'e-board-error
                (list "Committed index compact acknowledgement incomplete" cell)))
      (setf (e-board-index-receipt--index receipt) nil
            (e-board-index-receipt--work-id receipt) nil
            (e-board-index-receipt--subscription-id receipt) nil
            (e-board-index-receipt--queue receipt) nil
            (e-board-index-receipt--cell receipt) nil
            (e-board-index-receipt--previous receipt) nil
            (e-board-index-receipt--previous-node receipt) nil
            (e-board-index-receipt--root-node receipt) nil
            (e-board-index-receipt--next-node receipt) nil
            (e-board-index-receipt--compact-node receipt) nil
            (e-board-index-receipt--forward-stage receipt) nil
            (e-board-index-receipt--inverse-stage receipt) nil
            (e-board-index-receipt--registered-p receipt) nil
            (e-board-index-receipt--mapped-p receipt) nil
            (e-board-index-receipt--removed-p receipt) nil
            (e-board-index-receipt--compact-map-p receipt) nil
            (e-board-index-receipt--compact-stage receipt) 'done
            (e-board-index-receipt--previous-ack-p receipt) nil
            (e-board-index-receipt--next-ack-p receipt) nil)
      node))))

(defun e-board-admission--drop-transient-fields (admission)
  "Drop rollback-only fields after a committed admission is acknowledged."
  (when (e-board-work-admission-p admission)
    ;; Receipts are pushed newest-first.  Finalize oldest-first so a compact
    ;; root never captures a still-live sibling receipt from the same commit.
    (dolist (receipt (reverse (e-board-work-admission-event-receipts admission)))
      (unless (or (e-board-event-receipt--removed-p receipt)
                  (eq (e-board-event-receipt--compact-stage receipt) 'done))
        (e-board-admission--finalize-event-node receipt)))
    (dolist (receipt (reverse (e-board-work-admission-index-receipts admission)))
      (unless (or (e-board-index-receipt--removed-p receipt)
                  (eq (e-board-index-receipt--compact-stage receipt) 'done))
        (e-board-admission--finalize-index-node receipt)))
    (setf (e-board-work-admission-event-receipts admission) nil
          (e-board-work-admission-index-receipts admission) nil
          (e-board-work-admission-classification-receipts admission) nil
          (e-board-work-admission-effect-receipts admission) nil
          ;; The Board projection is committed; no later inverse needs to
          ;; retain the handle/work graph through this token.  The committed
          ;; Work and invocation objects remain owned by their Board tables.
          (e-board-work-admission-board admission) nil
          (e-board-work-admission-handle admission) nil
          (e-board-work-admission-work admission) nil
          (e-board-work-admission-invocation admission) nil
          (e-board-work-admission-effect-target admission) nil
          (e-board-work-admission-finalized-p admission) t))
  (when (e-board-aggregation-admission-p admission)
    (let ((aggregation (e-board-aggregation-admission-aggregation admission)))
      (dolist (receipt
               (reverse (e-board-aggregation-admission-event-receipts admission)))
        (unless (or (e-board-event-receipt--removed-p receipt)
                    (eq (e-board-event-receipt--compact-stage receipt) 'done))
          (e-board-admission--finalize-event-node receipt)))
      (dolist (receipt
               (reverse (e-board-aggregation-admission-index-receipts admission)))
        (unless (or (e-board-index-receipt--removed-p receipt)
                    (eq (e-board-index-receipt--compact-stage receipt) 'done))
          (e-board-admission--finalize-index-node receipt)))
      ;; `e-board-aggregation' is the durable Board projection.  Its admission
      ;; slot and the token's reciprocal Board/aggregation/activation links are
      ;; rollback-only and must not keep a committed object cycle alive.
      (when (e-board-aggregation-p aggregation)
        (setf (e-board-aggregation-admission aggregation) nil))
      (setf (e-board-aggregation-admission-event-receipts admission) nil
            (e-board-aggregation-admission-index-receipts admission) nil
            (e-board-aggregation-admission-classification-receipts admission) nil
            (e-board-aggregation-admission-effect-receipts admission) nil
            (e-board-aggregation-admission-deadline-receipt admission) nil
            (e-board-aggregation-admission-board admission) nil
            (e-board-aggregation-admission-aggregation admission) nil
            (e-board-aggregation-admission-timer admission) nil
            (e-board-aggregation-admission-activation admission) nil
            (e-board-aggregation-admission-map-installed-p admission) nil
            (e-board-aggregation-admission-finalized-p admission) t)))
  admission)

(defun e-board-admission-complete (board admission)
  "Finalize committed ADMISSION and then remove its exact pending token.

Finalization is performed before catalog removal, so a post-mutation removal
signal cannot make a visible commit undiscoverable.  If the exact `remhash'
postcondition is already true after a signal, completion is considered
acknowledged; a pre-mutation failure leaves the token for an exact retry."
  (when (and (e-board-p board) (e-board-admission--admission-p admission))
    (e-board-admission-finish board admission)
    ;; This local compaction has no external side effect; do it while the
    ;; Board token is still the authoritative pending entry.
    (e-board-admission--drop-transient-fields admission)
    (e-board-admission--set-cleanup admission t)
    (when (e-board-pending-admissions board)
      (let ((catalog (e-board-pending-admissions board)))
        (when (eq (gethash admission catalog) t)
          (condition-case err
              (remhash admission catalog)
            (error
             ;; A hash adapter may signal after removing the exact key.  The
             ;; committed/finalized postcondition is then already proven.
             (when (eq (gethash admission catalog) t)
               (signal (car err) (cdr err))))))
        (when (eq (gethash admission catalog) t)
          (signal 'e-board-admission-pending
                  (list "Committed Board admission remains pending" admission))))))
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
        (eq (cdr (e-board-admission--event-node-cell previous-node)) cell)
      (eq (e-board-events board) cell))))

(defun e-board-admission--event-raw-unlinked-p (board receipt)
  "Return whether RECEIPT's exact event gap has already been unlinked."
  (let* ((previous-node (e-board-event-receipt--previous-node receipt))
         (next-node (e-board-event-receipt--next-node receipt))
         (previous (and previous-node
                        (e-board-admission--event-node-cell previous-node)))
         (next (and next-node (e-board-admission--event-node-cell next-node))))
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
               (not (and (e-board-admission--event-node-p previous-node)
                         (e-board-admission--event-node-linked-p previous-node)
                         (e-board-admission--event-node-tail-p previous-node)
                         (eq (e-board-admission--event-node-cell previous-node)
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
                     (or (e-board-admission--event-node-root-node previous-node)
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
        (e-board-admission--event-node-set-next previous-node receipt)
        (e-board-admission--event-node-set-tail previous-node nil))
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

(defun e-board-admission-append-event
    (board type data &optional admission receipt-holder)
  "Append one Board event, retaining exact recovery when no token is supplied.

Internal Board callers normally pass an enclosing admission.  Standalone Board
notifications receive a short-lived Board admission token so an error after a
list or index mutation remains discoverable through the Board pending catalog.
The token is finalized only after the event append commits, and is never
reconstructed from an event type or sequence number.  When RECEIPT-HOLDER is a
mutable one-element list, the exact receipt for this append is stored there
even if a lower primitive signals after making its mutation.  Resumable Board
terminalization uses this to avoid publishing a duplicate event."
  (if (e-board-admission--admission-p admission)
      (condition-case err
          (let ((event (e-board-admission--append-event
                        board type data admission)))
            (when receipt-holder
              (setcar receipt-holder
                      (car (e-board-admission--receipts admission 'event))))
            event)
        (error
         (when receipt-holder
           (setcar receipt-holder
                   (car (e-board-admission--receipts admission 'event))))
         (signal (car err) (cdr err))))
    (let ((standalone (e-board-admission-aggregation-token board)))
      (e-board-admission-begin board standalone)
      (let (event)
        (condition-case err
            (progn
              (setq event
                    (e-board-admission--append-event
                     board type data standalone))
              (when receipt-holder
                ;; A standalone receipt is completed before this wrapper
                ;; returns and therefore cannot be retained by the caller.
                (setcar receipt-holder nil))
              (e-board-admission--standalone-postcheck board standalone)
              (setf (e-board-aggregation-admission-committed-p standalone) t)
              (e-board-admission-finish board standalone))
          (error
           (e-board-admission-finish board standalone)
           (when receipt-holder
             (setcar receipt-holder nil))
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
      (when (eq (e-board-admission--event-node-next-node previous-node) receipt)
        (e-board-admission--event-node-set-next previous-node next-node)))
    (when next-node
      (e-board-admission--event-node-set-previous-node next-node previous-node)
      (e-board-admission--event-node-set-previous next-node previous)
      (e-board-admission--event-node-set-root-node
       next-node
       (if previous-node
           (e-board-admission--event-node-root-node previous-node)
         next-node))
      (e-board-admission--event-node-set-head next-node (null previous-node)))
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
                          (e-board-admission--event-node-cell previous-node))
                         (before-cdr (cdr previous-cell)))
                    (condition-case err
                        (setcdr previous-cell (cdr cell))
                      (error
                       (if (and (not (eq (cdr previous-cell) before-cdr))
                                (eq (cdr previous-cell) (cdr cell)))
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
            (e-board-admission--event-node-set-tail previous-node t))
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

(defun e-board-admission-event-receipt-committed-p (receipt)
  "Return non-nil when RECEIPT has completed an event append.

This is a semantic status operation for Board terminalization.  It does not
expose the receipt's cell, map, or neighbour representation to callers."
  (and (e-board-event-receipt-p receipt)
       (not (e-board-event-receipt--removed-p receipt))
       (e-board-event-receipt--prefix-p receipt)
       (e-board-event-receipt--count-p receipt)
       (eq (e-board-event-receipt--forward-stage receipt) 'counted)))

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
               (not (and (e-board-admission--index-node-p previous-node)
                         (e-board-admission--index-node-linked-p previous-node)
                         (e-board-admission--index-node-tail-p previous-node)
                         (eq (e-board-admission--index-node-cell previous-node)
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
                   (or (e-board-admission--index-node-root-node previous-node)
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
      (e-board-admission--index-node-set-next previous-node receipt)
      (e-board-admission--index-node-set-tail previous-node nil))
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
        (eq (cdr (e-board-admission--index-node-cell previous-node)) cell)
      (eq (e-board-id-queue-head queue) cell))))

(defun e-board-admission--index-raw-unlinked-p (receipt)
  "Return whether RECEIPT's index gap has already been unlinked."
  (let* ((queue (e-board-index-receipt--queue receipt))
         (previous-node (e-board-index-receipt--previous-node receipt))
         (next-node (e-board-index-receipt--next-node receipt))
         (previous (and previous-node
                        (e-board-admission--index-node-cell previous-node)))
         (next (and next-node (e-board-admission--index-node-cell next-node))))
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
      (when (eq (e-board-admission--index-node-next-node previous-node) receipt)
        (e-board-admission--index-node-set-next previous-node next-node)))
    (when next-node
      (e-board-admission--index-node-set-previous-node next-node previous-node)
      (e-board-admission--index-node-set-previous next-node previous)
      ;; A committed successor is already self-rooted; only a still-staged
      ;; receipt carries mutable rollback root state.
      (when (e-board-index-receipt-p next-node)
        (e-board-admission--index-node-set-root-node
         next-node
         (if previous-node
             (e-board-admission--index-node-root-node previous-node)
           next-node)))
      (e-board-admission--index-node-set-head next-node (null previous-node)))
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
                          (e-board-admission--index-node-cell previous-node))
                         (before-cdr (cdr previous-cell)))
                    (condition-case err
                        (setcdr previous-cell (cdr cell))
                      (error
                       (if (and (not (eq (cdr previous-cell) before-cdr))
                                (eq (cdr previous-cell) (cdr cell)))
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
            (e-board-admission--index-node-set-tail previous-node t))
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
            (e-board-admission--standalone-postcheck board standalone)
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
             (stage (e-board-terminal-classification-receipt--inverse-stage receipt))
             (stage-error nil))
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
           ((and (not (e-board-terminal-classification-receipt--registered-p
                       receipt))
                 (not (e-board-terminal-classification-receipt--linked-p receipt))
                 (null node)
                 (not (eq (e-board-terminal-classifications board) cell)))
            ;; Forward registration can fail before the queue head/link is
            ;; installed.  There is then no physical inverse to perform, but
            ;; the receipt remains the exact authority for later stages.
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
                       'count)
                 (setq stage-error err))
               (unless stage-error
                 ;; The count callback failed before mutation.  Restore the
                 ;; exact queue cell and leave its node/index authority live;
                 ;; a later retry must not have to rediscover this receipt.
                 (e-board-admission--restore-terminal-classification-queue
                  receipt previous-receipt next-receipt)
                 (signal (car err) (cdr err)))))))
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
        (setf (e-board-terminal-classification-receipt--removed-p receipt) t)
        (when stage-error
          (signal (car stage-error) (cdr stage-error))))
    t)))

(defun e-board-admission--restore-terminal-classification-queue
    (receipt previous-receipt next-receipt)
  "Restore RECEIPT's exact queue cell after a pre-count failure.

The caller has already completed the purely local unlink stages, but the
unsettled count was not changed.  Reinstall only the captured gap and its
nearest still-live neighbours; no work id or descriptive queue lookup is
performed."
  (let* ((board (e-board-terminal-classification-receipt--board receipt))
         (cell (e-board-terminal-classification-receipt--cell receipt))
         (previous (and previous-receipt
                        (e-board-terminal-classification-receipt--cell
                         previous-receipt))))
    (if previous
        (setcdr previous cell)
      (setf (e-board-terminal-classifications board) cell))
    (when previous-receipt
      (setf (e-board-terminal-classification-receipt--next-receipt
             previous-receipt)
            receipt))
    (when next-receipt
      (setf (e-board-terminal-classification-receipt--previous-receipt
             next-receipt)
            receipt))
    (when (null next-receipt)
      (setf (e-board-terminal-classification-tail board) cell))
    (setf (e-board-terminal-classification-receipt--linked-p receipt) t
          (e-board-terminal-classification-receipt--head-p receipt)
          (null previous-receipt)
          (e-board-terminal-classification-receipt--tail-p receipt)
          (null next-receipt)
          (e-board-terminal-classification-receipt--inverse-stage receipt)
          'live)
  receipt))

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
    (let ((current (gethash key index))
          (acknowledged-p nil))
      (cond
       ((null current)
        (condition-case err
            (puthash key value index)
          (error
           ;; The captured value is still the only safe restoration target,
           ;; but a postmutation signal remains visible so the enclosing
           ;; admission can retry this exact stage.
           (signal (car err) (cdr err))))
        (setq acknowledged-p (eq (gethash key index) value)))
       ((eq current value)
        (setq acknowledged-p t))
       ;; A non-nil different value is an exact replacement authority for this
       ;; key.  The old classifier must not overwrite it, but its detached
       ;; inverse is nevertheless complete: retaining the old flag forever
       ;; would strand an otherwise fully cleaned admission.  This is the one
       ;; explicit replacement postcondition, not an id-based fallback.
       (t
        (setq acknowledged-p :replacement)))
      (unless acknowledged-p
        (signal 'e-board-error
                (list "Classifier index restoration did not retain" key)))
      (pcase detached-slot
        ('invocation
         (setf (e-board-terminal-classification-receipt--invocation-index-detached-p
                receipt)
               nil))
        ('aggregation
         (setf (e-board-terminal-classification-receipt--aggregation-index-detached-p
                receipt)
               nil)))
      acknowledged-p)))

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
            (e-board-admission--standalone-postcheck board standalone)
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
             (stage (e-board-aggregation-deadline-receipt--inverse-stage receipt))
             (stage-error nil))
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
           ((and (not (e-board-aggregation-deadline-receipt--registered-p
                       receipt))
                 (not (e-board-aggregation-deadline-receipt--linked-p receipt))
                 (null node)
                 (not (eq (e-board-aggregation-deadlines board) cell)))
            ;; Forward registration can fail before the queue head/link is
            ;; installed.  The receipt is still the exact authority for the
            ;; later stages, but there is no physical link to inverse.
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
                       'count)
                 (setq stage-error err))
               (unless stage-error
                 ;; Count notification failed before mutation.  Restore the
                 ;; exact queue cell so a retry retains its receipt authority.
                 (e-board-admission--restore-aggregation-deadline-queue
                  receipt previous-receipt next-receipt)
                 (signal (car err) (cdr err)))))))
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
        (setf (e-board-aggregation-deadline-receipt--removed-p receipt) t)
        (when stage-error
          (signal (car stage-error) (cdr stage-error))))
    t)))

(defun e-board-admission--restore-aggregation-deadline-queue
    (receipt previous-receipt next-receipt)
  "Restore RECEIPT's exact deadline cell after a pre-count failure.

The inverse has already acknowledged its local gap and tail stages, but the
routing count is unchanged.  Reinstall only the captured cell and its exact
neighbours; no aggregation id is consulted."
  (let* ((board (e-board-aggregation-deadline-receipt--board receipt))
         (cell (e-board-aggregation-deadline-receipt--cell receipt))
         (previous (and previous-receipt
                        (e-board-aggregation-deadline-receipt--cell
                         previous-receipt))))
    (if previous
        (setcdr previous cell)
      (setf (e-board-aggregation-deadlines board) cell))
    (when previous-receipt
      (setf (e-board-aggregation-deadline-receipt--next-receipt
             previous-receipt)
            receipt))
    (when next-receipt
      (setf (e-board-aggregation-deadline-receipt--previous-receipt
             next-receipt)
            receipt))
    (when (null next-receipt)
      (setf (e-board-aggregation-deadline-tail board) cell))
    (setf (e-board-aggregation-deadline-receipt--linked-p receipt) t
          (e-board-aggregation-deadline-receipt--head-p receipt)
          (null previous-receipt)
          (e-board-aggregation-deadline-receipt--tail-p receipt)
          (null next-receipt)
          (e-board-aggregation-deadline-receipt--inverse-stage receipt)
          'live)
    receipt))

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
    (board effect &optional admission schedule-function receipt-holder)
  "Queue EFFECT, retaining exact recovery when standalone.

An enclosing Board admission is used when supplied.  Direct effect
publication gets a short-lived Board admission before the FIFO cell, node
index, count, and callback generation change, so a later scheduler failure
cannot leave an unowned receipt behind.  RECEIPT-HOLDER, when supplied, is
updated with the exact effect receipt on both normal and post-mutation error
returns."
  (if (e-board-admission--admission-p admission)
      (condition-case err
          (let ((receipt (e-board-admission--schedule-effect
                          board effect admission schedule-function)))
            (when receipt-holder (setcar receipt-holder receipt))
            receipt)
        (error
         (when receipt-holder
           (setcar receipt-holder
                   (car (e-board-admission--receipts admission 'effect))))
         (signal (car err) (cdr err))))
    (let ((standalone (e-board-admission-aggregation-token board))
          receipt)
      (e-board-admission-begin board standalone)
      (condition-case err
          (progn
            (setq receipt
                  (e-board-admission--schedule-effect
                   board effect standalone schedule-function))
            (when receipt-holder (setcar receipt-holder nil))
            (e-board-admission--standalone-postcheck board standalone)
            (setf (e-board-aggregation-admission-committed-p standalone) t)
            (e-board-admission-finish board standalone))
        (error
         (e-board-admission-finish board standalone)
         (when receipt-holder (setcar receipt-holder nil))
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

(defun e-board-admission-effect-receipt-queued-p (receipt)
  "Return non-nil when RECEIPT still owns a queued effect.

The effect receipt remains an admission-owned implementation value; this
predicate is the narrow status needed by a Board settlement continuation."
  (and (e-board-effect-receipt-p receipt)
       (not (e-board-effect-receipt--removed-p receipt))
       (e-board-effect-receipt--queued-p receipt)))

(defun e-board-admission--effect-receipt-for-cell (board cell)
  "Return the exact effect receipt for CELL, or nil for a normal effect."
  (gethash cell (e-board-effect-node-index board)))

(defun e-board-admission--restore-effect-head (board cell receipt)
  "Restore an effect CELL and RECEIPT after a pre-count failure.

The drain removes the head before entering the fallible unsettled callback so
reentrant effect code cannot consume the same cell.  If that callback failed
before changing the count, put the exact cell back at the head and restore only
the captured successor authority."
  (let ((next-cell (e-board-pending-effects board)))
    (setcdr cell next-cell)
    (setf (e-board-pending-effects board) cell)
    (unless next-cell
      (setf (e-board-pending-effects-tail board) cell))
    (let ((current (gethash cell (e-board-effect-node-index board))))
      (cond
       ((eq current receipt) nil)
       ((null current)
        (condition-case err
            (puthash cell receipt (e-board-effect-node-index board))
          (error
           ;; A hash adapter may signal after restoring the exact entry.  Keep
           ;; the entry as the authoritative postcondition; otherwise the
           ;; caller must retain its original error and leave the queue
           ;; discoverable for an exact retry.
           (unless (eq (gethash cell (e-board-effect-node-index board)) receipt)
             (signal (car err) (cdr err)))))
        (unless (eq (gethash cell (e-board-effect-node-index board)) receipt)
          (signal 'e-board-error
                  (list "Effect head restoration did not retain receipt" cell))))
       (t
        (signal 'e-board-error
                (list "Effect head restoration found a replacement" cell)))))
    (when-let ((next-receipt
                (and next-cell
                     (gethash next-cell (e-board-effect-node-index board)))))
      (setf (e-board-effect-receipt--previous next-receipt) receipt))
    (setf (e-board-effect-receipt--previous receipt) nil
          (e-board-effect-receipt--head-p receipt) t
          (e-board-effect-receipt--queued-p receipt) t
          (e-board-effect-receipt--removed-p receipt) nil
          (e-board-effect-receipt--inverse-stage receipt) 'live))
  receipt)

(defun e-board-admission-drain-effects (board &optional generation)
  "Apply one bounded, generation-fenced effect page."
  (when (or (null generation)
            (= generation (e-board-effect-callback-generation board)))
    (setf (e-board-effect-draining-p board) t)
    (unwind-protect
        (let ((remaining
               (min e-board-effect-drain-limit
                    (e-board-unsettled-effect-count board)))
              (first-error nil))
          ;; Freeze the page boundary before invoking arbitrary effect code.
          ;; Effects queued reentrantly belong to the next scheduler turn even
          ;; when this page has spare capacity.  A count callback is an
          ;; independent fallible boundary: a postmutation signal is retained
          ;; while the exact effect is still applied, whereas a premutation
          ;; signal restores the exact head for a later retry.
          (while (and (> remaining 0) (e-board-pending-effects board))
            (let* ((cell (e-board-pending-effects board))
                   (effect (car cell))
                   (receipt (e-board-admission--effect-receipt-for-cell board cell)))
              (unless (e-board-effect-receipt-p receipt)
                (signal 'e-board-error
                        (list "Effect queue has no exact receipt" cell)))
              ;; Remove the cell from the live FIFO while the count and effect
              ;; callback execute.  This is the same exact cell authority used
              ;; by the inverse, and prevents reentrant code from consuming a
              ;; replacement at the head.
              (setf (e-board-pending-effects board) (cdr cell))
              (when (eq (e-board-pending-effects-tail board) cell)
                (setf (e-board-pending-effects-tail board) nil))
              ;; Removing the exact node entry is another fallible boundary.
              ;; A pre-mutation signal restores the same cell at the head and
              ;; leaves its receipt queued; a post-mutation signal still lets
              ;; this page finish the captured effect, while preserving the
              ;; adapter error for the caller.
              (let ((node-error nil)
                    (node-removed-p nil))
                (condition-case err
                    (progn
                      (remhash cell (e-board-effect-node-index board))
                      (setq node-removed-p t))
                  (error
                   (if (null (gethash cell (e-board-effect-node-index board)))
                       (setq node-removed-p t
                             node-error err)
                     (e-board-admission--restore-effect-head board cell receipt)
                     (setq first-error (or first-error err)
                           remaining 0))))
                (when node-removed-p
                  (setf (e-board-effect-receipt--registered-p receipt) nil)
                  (when-let ((next-receipt
                              (gethash (cdr cell)
                                       (e-board-effect-node-index board))))
                    (setf (e-board-effect-receipt--previous next-receipt) nil
                          (e-board-effect-receipt--head-p next-receipt) t))
                  (let ((before (e-board-unsettled-effect-count board))
                        (count-consumed-p nil))
                (condition-case err
                    (progn
                      (e-board-admission--adjust-unsettled board 'effects -1)
                      (setq count-consumed-p t))
                  (error
                   (if (= (e-board-unsettled-effect-count board) (1- before))
                       (progn
                         ;; The callback signalled after mutation.  Keep the
                         ;; error visible, but finish this exact effect and
                         ;; queue cell exactly once.
                         (setq count-consumed-p t
                               first-error (or first-error err)))
                     ;; No count mutation occurred.  Restore the head and
                     ;; leave its node discoverable for a subsequent drain.
                     (e-board-admission--restore-effect-head board cell receipt)
                     (setq first-error (or first-error err))
                     (setq remaining 0))))
                    (when count-consumed-p
                      (setf (e-board-effect-receipt--queued-p receipt) nil
                            (e-board-effect-receipt--removed-p receipt) t
                            (e-board-effect-receipt--inverse-stage receipt) 'removed)
                      (condition-case err
                          (funcall effect)
                        (error
                         ;; The effect drain owner records an unexpected
                         ;; effect failure in the event log; it never retries
                         ;; arbitrary user code inline.  Diagnostic logging is
                         ;; another fallible boundary, but it must not bypass
                         ;; the single scheduling-finalization path below.  A
                         ;; successful diagnostic preserves the historical
                         ;; containment behavior; if the diagnostic itself
                         ;; fails, surface the original callback error.
                         (let ((callback-error err))
                           (condition-case _diagnostic-error
                               (e-board-admission-append-event
                                board 'effect-drain-failed (list :error err))
                             (error
                              (setq first-error
                                    (or first-error callback-error))))))))
                    (when node-error
                      (setq first-error (or first-error node-error))))))
              (cl-decf remaining)))
          ;; The callback which entered this drain has been consumed.  Only now
          ;; clear its scheduling bit and publish one successor callback for any
          ;; remaining FIFO (including effects queued reentrantly by a callback).
          (setf (e-board-effects-scheduled board) nil)
          (when (e-board-pending-effects board)
            (condition-case err
                (e-board-admission--reschedule-effects board nil)
              (error (setq first-error (or first-error err)))))
          (when first-error
            (signal (car first-error) (cdr first-error))))
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
      (when (e-board-terminal-classification-receipt-p receipt)
        (unless (e-board-terminal-classification-receipt--removed-p receipt)
          (condition-case err
              (e-board-admission-remove-terminal-classification receipt)
            (error (setq cleanup-error (or cleanup-error err))))
          (unless (e-board-terminal-classification-receipt--removed-p receipt)
            (setq incomplete t)))
        ;; Queue removal and source-index restoration are separate exact
        ;; inverse stages.  The captured index remains authoritative even when
        ;; queue cleanup completed or signalled after mutation.
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
      (when (e-board-terminal-classification-receipt-p receipt)
        (unless (e-board-terminal-classification-receipt--removed-p receipt)
          (condition-case err
              (e-board-admission-remove-terminal-classification receipt)
            (error (setq cleanup-error (or cleanup-error err))))
          (unless (e-board-terminal-classification-receipt--removed-p receipt)
            (setq incomplete t)))
        ;; Queue removal and source-index restoration are separate exact
        ;; inverse stages.  Keep retrying the latter after the former has
        ;; completed or signalled after mutation.
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

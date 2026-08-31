;;; e-board-admission-test.el --- Direct Board admission mechanism tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests exercise the exact receipt owner directly.  Facade enrollment,
;; routing, and composed settlement scenarios live in the composition suite.

;;; Code:

(require 'ert)
(require 'e-board-state)
(require 'e-board-admission)
(require 'e-work)

(cl-defun e-board-admission-test--make-board
    (&key (id "admission-test") effect-scheduler
          terminal-classification-scheduler aggregation-deadline-scheduler
          unsettled-change-function)
  "Build the smallest fully initialized Board value for mechanism tests.

This fixture deliberately uses the lower state constructor rather than the
facade.  The admission owner only needs these queues, indexes, and counters;
semantic Board creation and registration stay in the composition suite."
  (let ((prefix (make-hash-table :test 'eql)))
    (puthash 0 0 prefix)
    (e-board-state-create
     :id id :next-seq 0 :events nil :events-tail nil
     :message-count 0 :event-message-count prefix
     :event-message-prefix-high-watermark 0
     :event-node-index (make-hash-table :test 'eq)
     :pending-admissions (make-hash-table :test 'eq)
     :work-table (make-hash-table :test 'equal)
     :invocations (make-hash-table :test 'equal)
     :aggregations (make-hash-table :test 'equal)
     :pending-effects nil :pending-effects-tail nil
     :effect-node-index (make-hash-table :test 'eq)
     :effects-scheduled nil :effect-schedule-generation 0
     :effect-draining-p nil :effect-callback-generation 0
     :effect-scheduler effect-scheduler
     :invocation-work-index (make-hash-table :test 'equal)
     :aggregation-work-index (make-hash-table :test 'equal)
     :terminal-classifications nil :terminal-classification-tail nil
     :terminal-classification-node-index (make-hash-table :test 'eq)
     :terminal-classification-scheduled nil
     :terminal-classification-generation 0
     :terminal-classification-callback-generation 0
     :terminal-classification-scheduler terminal-classification-scheduler
     :aggregation-deadlines nil :aggregation-deadline-tail nil
     :aggregation-deadline-node-index (make-hash-table :test 'eq)
     :aggregation-deadline-scheduled nil
     :aggregation-deadline-generation 0
     :aggregation-deadline-callback-generation 0
     :aggregation-deadline-scheduler aggregation-deadline-scheduler
     :activations (make-hash-table :test 'equal)
     :unsettled-pickup-count 0 :unsettled-effect-count 0
     :unsettled-routing-count 0 :unsettled-generation 0
     :unsettled-change-function unsettled-change-function)))

(defmacro e-board-admission-test--with-empty-registry (&rest body)
  "Run admission mechanism BODY without loading Board composition code."
  (declare (indent 0))
  `(progn ,@body))

(ert-deftest e-board-test-admission-event-receipts-remove-interleaved-events ()
  "Exact event receipts preserve unrelated and interleaved event cells."
  (e-board-admission-test--with-empty-registry
    (let* ((board (e-board-admission-test--make-board :id "event-receipt"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "event-receipt-work" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) :done))
             nil))
           (admission (e-board-admission-work-token handle)))
      (e-board-admission-append-event board 'staged-one nil admission)
      (e-board-admission-append-event board 'unrelated nil)
      (e-board-admission-append-event board 'staged-two nil admission)
      (dolist (receipt (e-board-work-admission-event-receipts admission))
        (e-board-admission-remove-event receipt))
      (should (equal (mapcar #'e-board-event-type (e-board-events board))
                     '(unrelated)))
      (should (= (e-board-next-seq board) 3))
      (should (= (gethash 2 (e-board-event-message-count board)) 0)))))

(ert-deftest e-board-test-admission-index-receipt-preserves-replacement-queue ()
  "A stale exact index receipt cannot remove a same-key replacement queue."
  (e-board-admission-test--with-empty-registry
    (let* ((index (make-hash-table :test 'equal))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "index-replacement-work" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) :done))
             nil))
           (admission (e-board-admission-work-token handle))
           (receipt (e-board-admission-index-work
                     index "work" "old" admission))
           (replacement (e-board-id-queue--create))
           (cell (list "new")))
      (setf (e-board-id-queue-head replacement) cell
            (e-board-id-queue-tail replacement) cell)
      (puthash "work" replacement index)
      (e-board-admission-remove-index receipt)
      (should (eq (gethash "work" index) replacement))
      (should (equal (e-board-id-queue-head replacement) '("new")))
      (should (e-board-index-receipt--removed-p receipt)))))

(ert-deftest e-board-test-admission-event-authority-precedes-first-mutation ()
  "A malformed tail is rejected before an append can mutate the event list."
  (e-board-admission-test--with-empty-registry
    (let* ((board (e-board-admission-test--make-board :id "event-authority"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "event-authority-work" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) :done))
             nil))
           (admission (e-board-admission-work-token handle))
           (stale-cell (list 'stale-event)))
      ;; This is the only state in which the append tail has no receipt
      ;; authority.  The guard must run before the new receipt is registered or
      ;; any live list cell is changed.
      (setf (e-board-events board) stale-cell
            (e-board-events-tail board) nil)
      (should-error (e-board-admission-append-event board 'posted nil admission))
      (should (eq (e-board-events board) stale-cell))
      (should (= (hash-table-count (e-board-event-node-index board)) 0))
      ;; No sequence is allocated for a precondition failure; once an append
      ;; starts, all later attempts still use the board's monotonic allocator.
      (should (= (e-board-next-seq board) 0))
      (should-not (e-board-work-admission-event-receipts admission))
      (setf (e-board-events board) nil)
      (should (e-board-event-p
               (e-board-admission-append-event board 'posted nil admission)))
      (should (= (length (e-board-work-admission-event-receipts admission)) 1)))))

(ert-deftest e-board-test-admission-event-receipts-abort-in-either-order ()
  "Two exact event tokens can abort in either order without losing the tail."
  (e-board-admission-test--with-empty-registry
    (dolist (order '((0 1) (1 0)))
      (let* ((board (e-board-admission-test--make-board :id (format "event-order-%S" order)))
             (first-handle
              (e-work-prepare
               (e-work-spec-create
                :id (format "event-first-%S" order) :execution 'cheap
                :interactive-policy 'cheap :runner #'ignore)
               nil))
             (second-handle
              (e-work-prepare
               (e-work-spec-create
                :id (format "event-second-%S" order) :execution 'cheap
                :interactive-policy 'cheap :runner #'ignore)
               nil))
             (first-admission (e-board-admission-work-token first-handle))
             (second-admission (e-board-admission-work-token second-handle)))
        (e-board-admission-append-event board 'staged-first nil first-admission)
        (e-board-admission-append-event board 'unrelated nil)
        (e-board-admission-append-event board 'staged-second nil second-admission)
        (let ((receipts (vector
                         (car (e-board-work-admission-event-receipts
                               first-admission))
                         (car (e-board-work-admission-event-receipts
                               second-admission)))))
          (dolist (index order)
            (e-board-admission-remove-event (aref receipts index))))
        (should (equal (mapcar #'e-board-event-type (e-board-events board))
                       '(unrelated)))
        (should (eq (e-board-events-tail board)
                    (e-board-events board)))
        (should (= (hash-table-count (e-board-event-node-index board)) 1))
        (should (= (gethash 2 (e-board-event-message-count board)) 0))
        ;; Repeated inverse calls are no-ops after exact acknowledgement.
        (dolist (admission (list first-admission second-admission))
          (e-board-admission-remove-event
           (car (e-board-work-admission-event-receipts admission))))))))

(ert-deftest e-board-test-admission-index-authority-precedes-first-mutation ()
  "A malformed queue tail is rejected before index insertion can mutate it."
  (e-board-admission-test--with-empty-registry
    (let* ((index (make-hash-table :test 'equal))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "index-authority-work" :execution 'cheap
              :interactive-policy 'cheap :runner #'ignore)
             nil))
           (admission (e-board-admission-work-token handle))
           (queue (e-board-id-queue--create
                   :node-index (make-hash-table :test 'eq)))
           (stale-cell (list "stale")))
      (setf (e-board-id-queue-head queue) stale-cell
            (e-board-id-queue-tail queue) nil)
      (puthash "work" queue index)
      (should-error
       (e-board-admission-index-work index "work" "first" admission))
      (should (eq (gethash "work" index) queue))
      (should (eq (e-board-id-queue-head queue) stale-cell))
      (should (= (hash-table-count (e-board-id-queue-node-index queue)) 0))
      (setf (e-board-id-queue-head queue) nil)
      (should (e-board-index-receipt-p
               (e-board-admission-index-work
                index "work" "first" admission)))
      (should (equal
               (e-board-admission--index-values index "work")
               '("first"))))))

(ert-deftest e-board-test-admission-index-rejects-receiptless-entry ()
  "A work-index mutation cannot start without an exact admission token."
  (e-board-admission-test--with-empty-registry
    (let ((index (make-hash-table :test 'equal)))
      (should-error (e-board-admission-index-work index "work" "subscription")
                    :type 'e-board-error)
      (should (= (hash-table-count index) 0)))))

(ert-deftest e-board-test-admission-index-receipts-abort-in-either-order ()
  "Queue-cell inverses preserve FIFO and exact tail authority in either order."
  (e-board-admission-test--with-empty-registry
    (dolist (order '((0 1) (1 0)))
      (let* ((index (make-hash-table :test 'equal))
             (first-handle
              (e-work-prepare
               (e-work-spec-create
                :id (format "index-first-%S" order) :execution 'cheap
                :interactive-policy 'cheap :runner #'ignore)
               nil))
             (second-handle
              (e-work-prepare
               (e-work-spec-create
                :id (format "index-second-%S" order) :execution 'cheap
                :interactive-policy 'cheap :runner #'ignore)
               nil))
             (first-admission (e-board-admission-work-token first-handle))
             (second-admission (e-board-admission-work-token second-handle))
             (first-receipt
              (e-board-admission-index-work
               index "work" "first" first-admission))
             (unrelated-admission (e-board-admission-work-token first-handle))
             (_unrelated
              (e-board-admission-index-work
               index "work" "unrelated" unrelated-admission))
             (second-receipt
              (e-board-admission-index-work
               index "work" "second" second-admission)))
        (let ((receipts (vector first-receipt second-receipt)))
          (dolist (index order)
            (e-board-admission-remove-index (aref receipts index))))
        (should (equal (e-board-admission--index-values index "work")
                       '("unrelated")))
        (let ((queue (gethash "work" index)))
          (should (eq (e-board-id-queue-head queue)
                      (e-board-id-queue-tail queue)))
          (should (= (hash-table-count
                      (e-board-id-queue-node-index queue))
                     1)))
        (e-board-admission-remove-index first-receipt)
        (e-board-admission-remove-index second-receipt)))))

(ert-deftest e-board-test-classifier-index-restore-accepts-exact-replacement ()
  "Classifier abort acknowledges a proven replacement without overwriting it."
  (let* ((board (e-board-admission-test--make-board :id "classifier-replacement"))
         (handle
          (e-work-prepare
           (e-work-spec-create
            :id "classifier-replacement-work" :execution 'cheap
            :interactive-policy 'cheap :runner #'ignore)
           nil))
         (work (e-board-work--create
                :id "classifier-replacement-work" :handle handle :state 'running))
         (invocation (e-board-invocation--create
                      :id "classifier-replacement-invocation"
                      :work-id (e-board-work-id work) :state 'open
                      :effect-target '(target)))
         (index (e-board-invocation-work-index board))
         (index-admission (e-board-admission-work-token handle))
         (admission (e-board-admission-work-token handle))
         replacement receipt)
    (puthash (e-board-work-id work) work (e-board-work-table board))
    (puthash (e-board-invocation-id invocation) invocation
             (e-board-invocations board))
    (e-board-admission-index-work
     index (e-board-work-id work) (e-board-invocation-id invocation)
     index-admission)
    (setf (e-board-work-admission-board admission) board
          (e-board-work-admission-work admission) work)
    (e-board-admission-begin board admission)
    (setq receipt
          (e-board-admission-queue-terminal-classification
           board work (list (e-board-invocation-id invocation)) nil nil
           admission #'ignore))
    (should-not (gethash (e-board-work-id work) index))
    ;; A new owner has already installed a replacement queue.  The old
    ;; classifier inverse must preserve that exact object and finish without
    ;; resurrecting the captured queue.
    (setq replacement (e-board-id-queue--create))
    (puthash (e-board-work-id work) replacement index)
    (e-board-admission-finish board admission)
    (e-board-admission-abort board admission)
    (should (eq (gethash (e-board-work-id work) index) replacement))
    (should (= (e-board-admission-pending-count board) 0))
    (should (e-board-terminal-classification-receipt--removed-p receipt))
    (should-not
     (e-board-terminal-classification-receipt--invocation-index-detached-p
      receipt))))

(ert-deftest e-board-test-deadline-receipts-abort-in-either-order ()
  "Aggregation deadline receipts repair exact neighbours in either order."
  (e-board-admission-test--with-empty-registry
    (dolist (order '((0 1) (1 0)))
      (let* ((board (e-board-admission-test--make-board
                     :id (format "deadline-order-%s" order)))
             (aggregation
              (e-board-aggregation--create
               :id (format "deadline-aggregation-%s" order)
               :work-ids nil :mode 'all :state 'open :effect-target '(target)))
             (admission-one (e-board-admission-aggregation-token board))
             (admission-two (e-board-admission-aggregation-token board)))
        (puthash (e-board-aggregation-id aggregation) aggregation
                 (e-board-aggregations board))
        (setf (e-board-aggregation-admission-board admission-one) board
              (e-board-aggregation-admission-aggregation admission-one)
              aggregation
              (e-board-aggregation-admission-board admission-two) board
              (e-board-aggregation-admission-aggregation admission-two)
              aggregation)
        (let* ((first (e-board-admission-queue-aggregation-deadline
                       board aggregation admission-one))
               (second (e-board-admission-queue-aggregation-deadline
                        board aggregation admission-two))
               (receipts (vector first second)))
          (dolist (index order)
            (e-board-admission-remove-aggregation-deadline
             (aref receipts index)))
          (should-not (e-board-aggregation-deadlines board))
          (should-not (e-board-aggregation-deadline-tail board))
          (should (= (e-board-unsettled-routing-count board) 0))
          (should (= (hash-table-count
                      (e-board-aggregation-deadline-node-index board))
                     0)))))))

(ert-deftest e-board-test-effect-receipts-abort-in-either-order ()
  "Pending effect receipts repair exact FIFO neighbours in either order."
  (e-board-admission-test--with-empty-registry
    (dolist (order '((0 1) (1 0)))
      (let* ((board (e-board-admission-test--make-board
                     :id (format "effect-order-%s" order)
                     :effect-scheduler #'ignore))
             (admission-one (e-board-admission-aggregation-token board))
             (admission-two (e-board-admission-aggregation-token board))
             (first (e-board-admission-schedule-effect
                     board #'ignore admission-one))
             (second (e-board-admission-schedule-effect
                      board #'ignore admission-two))
             (receipts (vector first second)))
        (dolist (index order)
          (e-board-admission-remove-effect (aref receipts index)))
        (should-not (e-board-pending-effects board))
        (should-not (e-board-pending-effects-tail board))
        (should (= (e-board-unsettled-effect-count board) 0))
        (should (= (hash-table-count (e-board-effect-node-index board)) 0))))))

(ert-deftest e-board-test-effect-drain-yields-after-its-bounded-page ()
  "Queued effects retain FIFO order while each scheduler turn stays bounded."
  (e-board-admission-test--with-empty-registry
    (let ((e-board-effect-drain-limit 1)
          drains applied)
      (let ((board (e-board-admission-test--make-board
                    :id "board"
                    :effect-scheduler (lambda (drain) (push drain drains)))))
        (e-board-admission-schedule-effect board (lambda () (push 'first applied)))
        (e-board-admission-schedule-effect board (lambda () (push 'second applied)))
        (e-board-admission-schedule-effect board (lambda () (push 'third applied)))
        (should (= (length drains) 1))
        (funcall (pop drains))
        (should (equal (reverse applied) '(first)))
        (should (= (length drains) 1))
        (funcall (pop drains))
        (should (equal (reverse applied) '(first second)))
        (funcall (pop drains))
        (should (equal (reverse applied) '(first second third)))
        (should-not drains)))))

(ert-deftest e-board-test-effect-drain-keeps-one-successor-when-an-effect-enqueues ()
  "An effect-created successor reuses the same one scheduled drain token."
  (e-board-admission-test--with-empty-registry
    (let ((e-board-effect-drain-limit 1)
          drains applied)
      (let ((board (e-board-admission-test--make-board
                    :id "board"
                    :effect-scheduler (lambda (drain) (push drain drains)))))
        (e-board-admission-schedule-effect
         board
         (lambda ()
           (push 'first applied)
           (e-board-admission-schedule-effect board (lambda () (push 'second applied)))))
        (funcall (pop drains))
        (should (equal (reverse applied) '(first)))
        (should (= (length drains) 1))
        (funcall (pop drains))
        (should (equal (reverse applied) '(first second)))
        (should-not drains)))))

(ert-deftest e-board-test-effect-fifo-publishes-exact-unsettled-transitions ()
  "Effect ownership is indexed at enqueue and retirement without queue scans."
  (e-board-admission-test--with-empty-registry
    (let* ((drains nil)
           (snapshots nil)
           (applied nil)
           (e-board-effect-drain-limit 1)
           (board
            (e-board-admission-test--make-board
             :id "board"
             :effect-scheduler (lambda (drain) (push drain drains))
             :unsettled-change-function
             (lambda (_board _class _delta state) (push state snapshots)))))
      (e-board-admission-schedule-effect board (lambda () (push 'first applied)))
      (e-board-admission-schedule-effect board (lambda () (push 'second applied)))
      (should (equal (e-board-state-unsettled-state board)
                     '(:generation 2 :pickups 0 :effects 2 :routing 0)))
      (funcall (pop drains))
      (should (equal (e-board-state-unsettled-state board)
                     '(:generation 3 :pickups 0 :effects 1 :routing 0)))
      (funcall (pop drains))
      (should (equal (e-board-state-unsettled-state board)
                     '(:generation 4 :pickups 0 :effects 0 :routing 0)))
      (should-not (e-board-pending-effects-tail board))
      (should (equal (nreverse applied) '(first second)))
      (should (= (length snapshots) 4)))))

(ert-deftest e-board-test-effect-drain-contains-one-unexpected-callback-failure ()
  "A failed queued callback records its fault without blocking later FIFO work."
  (e-board-admission-test--with-empty-registry
    (let ((e-board-effect-drain-limit 2)
          drains applied)
      (let ((board (e-board-admission-test--make-board
                    :id "board"
                    :effect-scheduler (lambda (drain) (push drain drains)))))
        (e-board-admission-schedule-effect board (lambda () (error "unexpected effect failure")))
        (e-board-admission-schedule-effect board (lambda () (push 'second applied)))
        (e-board-admission-schedule-effect board (lambda () (push 'third applied)))
        (funcall (pop drains))
        (should (equal (reverse applied) '(second)))
        (should (member 'effect-drain-failed
                        (mapcar #'e-board-event-type (e-board-events board))))
        (should (= (length drains) 1))
        (funcall (pop drains))
        (should (equal (reverse applied) '(second third)))
        (should-not drains)))))

(ert-deftest e-board-test-effect-successor-scheduler-fault-is-retryable ()
  "A rejected successor scheduler leaves a fresh enqueue able to retry FIFO."
  (e-board-admission-test--with-empty-registry
    (let ((e-board-effect-drain-limit 1)
          callbacks applied (calls 0))
      (let ((board
             (e-board-admission-test--make-board
              :id "effect-successor-retry"
              :effect-scheduler
              (lambda (callback)
                (cl-incf calls)
                (if (= calls 1)
                    (push callback callbacks)
                  (error "effect successor scheduler"))))))
        (e-board-admission-schedule-effect
         board (lambda () (push 'first applied)))
        (e-board-admission-schedule-effect
         board (lambda () (push 'second applied)))
        ;; The admission owner itself drains effects; the first callback is
        ;; enough to prove the failed successor left the second exact cell.
        (should-error (funcall (pop callbacks)))
        (should (equal applied '(first)))
        (should (e-board-pending-effects board))
        (should (= (e-board-unsettled-effect-count board) 1))
        (should-not (e-board-effects-scheduled board))
        ;; A fresh enqueue obtains fresh scheduling authority instead of being
        ;; stranded behind the rejected successor publication.
        (setf (e-board-effect-scheduler board)
              (lambda (callback) (push callback callbacks)))
        (e-board-admission-schedule-effect
         board (lambda () (push 'third applied)))
        (should callbacks)
        (funcall (pop callbacks))
        (should (equal applied '(second first)))
        (should callbacks)
        (funcall (pop callbacks))
        (should (equal applied '(third second first)))
        (should-not (e-board-pending-effects board))
        (should (= (e-board-unsettled-effect-count board) 0))))))

(ert-deftest e-board-test-standalone-effect-wrapper-rejects-fenced-success ()
  "A reentrant owner entry cannot turn a standalone effect into success."
  (let (entered drains)
    (let ((board
           (e-board-admission-test--make-board
            :id "effect-reentry"
            :effect-scheduler (lambda (drain) (push drain drains))
            :unsettled-change-function
            (lambda (source class delta _state)
              (when (and (not entered) (eq class 'effects) (= delta 1))
                (setq entered t)
                (condition-case nil
                    (e-board-admission-require-clear source)
                  (error nil)))))))
      (should-error (e-board-admission-schedule-effect board #'ignore))
      (should entered)
      (should-not (e-board-pending-effects board))
      (should (= (e-board-unsettled-effect-count board) 0))
      (should-not (e-board-effects-scheduled board))
      (should (= (e-board-admission-pending-count board) 0)))))

(ert-deftest e-board-test-standalone-classifier-wrapper-rejects-fenced-success ()
  "A reentrant owner entry cannot turn a standalone classifier into success."
  (let (entered)
    (let* ((board
            (e-board-admission-test--make-board
             :id "classifier-reentry"
             :unsettled-change-function
             (lambda (source class delta _state)
               (when (and (not entered) (eq class 'routing) (= delta 1))
                 (setq entered t)
                 (condition-case nil
                     (e-board-admission-require-clear source)
                   (error nil))))))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "classifier-reentry-work" :execution 'cheap
              :interactive-policy 'cheap :runner #'ignore)
             nil))
           (work (e-board-work--create
                  :id "classifier-reentry-work" :handle handle :state 'posted)))
      (puthash (e-board-work-id work) work (e-board-work-table board))
      (should-error
       (e-board-admission-queue-terminal-classification
        board work nil nil nil nil #'ignore))
      (should entered)
      (should-not (e-board-terminal-classifications board))
      (should (= (e-board-unsettled-routing-count board) 0))
      (should (= (e-board-admission-pending-count board) 0)))))

(ert-deftest e-board-test-standalone-deadline-wrapper-rejects-fenced-success ()
  "A reentrant owner entry cannot turn a standalone deadline into success."
  (let (entered)
    (let* ((board
            (e-board-admission-test--make-board
             :id "deadline-reentry"
             :unsettled-change-function
             (lambda (source class delta _state)
               (when (and (not entered) (eq class 'routing) (= delta 1))
                 (setq entered t)
                 (condition-case nil
                     (e-board-admission-require-clear source)
                   (error nil))))))
           (aggregation
            (e-board-aggregation--create
             :id "deadline-reentry-aggregation" :work-ids nil :mode 'all
             :state 'open :effect-target '(target))))
      (puthash (e-board-aggregation-id aggregation) aggregation
               (e-board-aggregations board))
      (should-error
       (e-board-admission-queue-aggregation-deadline
        board aggregation nil #'ignore))
      (should entered)
      (should-not (e-board-aggregation-deadlines board))
      (should (= (e-board-unsettled-routing-count board) 0))
      (should (= (e-board-admission-pending-count board) 0)))))

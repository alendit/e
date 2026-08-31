;;; e-board-admission-composition-test.el --- Board admission composition tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; These tests exercise the public Board enrollment/aggregation composition
;; contract.  Exact receipt/link mechanics live in e-board-admission-test.el;
;; this suite intentionally loads the Board facade and tests observable owner
;; behavior, recovery, and retry.

;;; Code:

(require 'ert)
(require 'e-board)
(require 'e-work)

(defmacro e-board-admission-composition-test--with-empty-registry (&rest body)
  "Run composition tests with board registration disabled."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'e-board-register) (lambda (_board) nil)))
     ,@body))

(ert-deftest e-board-test-admission-event-receipts-remove-postmutation-events ()
  "A posted append that signals after mutation leaves an exact rollback receipt."
  (e-board-admission-composition-test--with-empty-registry
    (let* ((board (e-board-create :id "posted-receipt"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "posted-receipt-work" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) :done))
             nil))
           (admission (e-board-work-admission-token handle))
           (original (symbol-function 'e-board-admission-append-event)))
      (cl-letf (((symbol-function 'e-board-admission-append-event)
                 (lambda (&rest arguments)
                   (let ((event (apply original arguments)))
                     (if (eq (nth 1 arguments) 'posted)
                         (error "posted append postmutation")
                       event)))))
        (should-error
         (e-board-enroll-work board handle :admission admission)))
      ;; The composed contract observes the cleaned board, not the private
      ;; receipt representation retained by the mechanism owner.
      (should (= (e-board-pending-admission-count board) 0))
      (should-not (e-board-events board))
      (should-not (e-board-observed-work board (e-work-handle-id handle)))
      (should (= (e-board-next-seq board) 1))
      ;; Sequence allocation is monotonic even when the exact event cell is
      ;; removed; a retry creates one fresh posted record rather than a ghost.
      (e-board-enroll-work board handle)
      (should (equal (mapcar #'e-board-event-type (e-board-events board))
                     '(posted)))
      (should (= (e-board-next-seq board) 2)))))

(ert-deftest e-board-test-admission-event-receipt-removes-postmutation-subscription ()
  "A subscription append signal cannot leave an invocation event ghost."
  (e-board-admission-composition-test--with-empty-registry
    (let* ((board (e-board-create :id "subscription-receipt"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "subscription-receipt-work" :execution 'cheap
              :interactive-policy 'cheap
              :runner (lambda (_arguments _context) :done))
             nil))
           (target (list 'target))
           (admission (e-board-work-admission-token
                       handle :invocation-id "call"
                       :effect-target target))
           (original (symbol-function 'e-board-admission-append-event)))
      (cl-letf (((symbol-function 'e-board-admission-append-event)
                 (lambda (&rest arguments)
                   (let ((event (apply original arguments)))
                     (if (eq (nth 1 arguments) 'subscription-added)
                         (error "subscription append postmutation")
                       event)))))
        (should-error
         (e-board-enroll-invocation-work
          board handle "call" target :admission admission)))
      (should (= (e-board-pending-admission-count board) 0))
      (should-not (e-board-events board))
      (should-not (e-board-observed-work board (e-work-handle-id handle)))
      (should-not (e-board-invocation board "call"))
      (should (= (e-board-next-seq board) 2))
      (e-board-enroll-invocation-work board handle "call" target)
      (should (equal (mapcar #'e-board-event-type (e-board-events board))
                     '(posted subscription-added))))))

(ert-deftest e-board-test-admission-index-receipts-retry-and-preserve-replacement ()
  "Exact index inverse faults retain retry authority and fence replacements."
  (e-board-admission-composition-test--with-empty-registry
    (dolist (failure-mode '(before after))
      (let* ((board (e-board-create :id (format "index-receipt-%s" failure-mode)))
             (handle
              (e-work-prepare
               (e-work-spec-create
                :id (format "index-receipt-work-%s" failure-mode)
                :execution 'cheap :interactive-policy 'cheap
                :runner (lambda (_arguments _context) :done))
               nil))
             (effect (list 'lease failure-mode))
             (admission (e-board-work-admission-token
                         handle :invocation-id "call"
                         :effect-target effect))
             (original (symbol-function 'e-board-admission-remove-index))
             failed)
        (e-board-enroll-invocation-work
         board handle "call" effect
         :admission admission)
        (setq failed t)
        (cl-letf (((symbol-function 'e-board-admission-remove-index)
                   (lambda (receipt)
                     (if failed
                         (progn
                           (setq failed nil)
                           (if (eq failure-mode 'before)
                               (error "index inverse before mutation")
                             (prog1 (funcall original receipt)
                               (error "index inverse after mutation"))))
                       (funcall original receipt)))))
          (should-error (e-board-abort-work-enrollment board admission)))
        (if (eq failure-mode 'before)
            (progn
              ;; The sole invocation map and its exact queue remain until the
              ;; inverse is retried; the event receipts may already be gone.
              (should (e-board-invocation board "call"))
              (should (e-board-invocation board "call"))
              (e-board-abort-work-enrollment board admission))
          (should-not (e-board-invocation board "call"))
          (e-board-abort-work-enrollment board admission))
        (should-not (e-board-observed-work board (e-work-handle-id handle)))
        (should-not (e-board-events board))
        ;; A successful retry has no duplicate invocation index or stale event.
        (e-board-enroll-invocation-work
         board handle "call" (list 'retry failure-mode))
        (should (e-board-invocation board "call"))))))

(ert-deftest e-board-test-direct-admission-pending-recovery-preserves-primary-error ()
  "An omitted admission token remains board-reachable across inverse failure."
  (e-board-admission-composition-test--with-empty-registry
    (let* ((board (e-board-create :id "direct-pending"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "direct-pending-work" :execution 'cheap
              :interactive-policy 'cheap :runner #'ignore)
             nil))
           (append-original (symbol-function 'e-board-admission-append-event))
           (remove-original (symbol-function 'e-board-admission-remove-event))
           primary)
      (cl-letf (((symbol-function 'e-board-admission-append-event)
                 (lambda (&rest arguments)
                   (prog1 (apply append-original arguments)
                     (when (eq (nth 1 arguments) 'posted)
                       (error "direct admission primary")))))
                ((symbol-function 'e-board-admission-remove-event)
                 (lambda (_receipt)
                   (error "direct admission cleanup"))))
        (setq primary
              (condition-case error
                  (progn (e-board-enroll-work board handle) nil)
                (error error))))
      (should primary)
      (should (equal (error-message-string primary)
                     "direct admission primary"))
      (should (= (e-board-pending-admission-count board) 1))
      (should (= (length (e-board-events board)) 1))
      ;; The board's exact pending catalog is consulted before the retry.  It
      ;; removes the same event/observer and only then allows a fresh attempt.
      (e-board-enroll-work board handle)
      (should (= (e-board-pending-admission-count board) 0))
      (should (equal (mapcar #'e-board-event-type (e-board-events board))
                     '(posted)))
      (should (eq (symbol-function 'e-board-admission-remove-event)
                  remove-original)))))

(ert-deftest e-board-test-direct-invocation-admission-pending-recovery ()
  "A direct invocation subscription retains its exact cleanup authority."
  (e-board-admission-composition-test--with-empty-registry
    (let* ((board (e-board-create :id "direct-invocation-pending"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "direct-invocation-work" :execution 'cheap
              :interactive-policy 'cheap :runner #'ignore)
             nil))
           (append-original (symbol-function 'e-board-admission-append-event))
           (remove-original
            (symbol-function 'e-board-admission-remove-index))
           primary)
      (e-board-enroll-work board handle)
      (cl-letf (((symbol-function 'e-board-admission-append-event)
                 (lambda (&rest arguments)
                   (prog1 (apply append-original arguments)
                     (when (eq (nth 1 arguments) 'subscription-added)
                       (error "direct invocation primary")))))
                ((symbol-function 'e-board-admission-remove-index)
                 (lambda (_receipt)
                   (error "direct invocation cleanup"))))
        (setq primary
              (condition-case error
                  (progn
                    (e-board-subscribe-invocation
                     board (e-work-handle-id handle) '(target) :id "call")
                    nil)
                (error error))))
      (should primary)
      (should (equal (error-message-string primary)
                     "direct invocation primary"))
      (should (= (e-board-pending-admission-count board) 1))
      (should (e-board-invocation board "call"))
      (should (e-board-invocation board "call"))
      ;; The next public call first retries the board-owned token.  It removes
      ;; the old exact invocation/index/event and only then admits the retry.
      (e-board-subscribe-invocation
       board (e-work-handle-id handle) '(retry-target) :id "call")
      (should (= (e-board-pending-admission-count board) 0))
      (should (equal
               (e-board-invocation-effect-target
                (e-board-invocation board "call"))
               '(retry-target)))
      (should (eq (symbol-function 'e-board-admission-remove-index)
                  remove-original)))))

(ert-deftest e-board-test-direct-combined-admission-pending-recovery ()
  "Combined direct enrollment retains both exact event inverses on failure."
  (e-board-admission-composition-test--with-empty-registry
    (let* ((board (e-board-create :id "direct-combined-pending"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "direct-combined-work" :execution 'cheap
              :interactive-policy 'cheap :runner #'ignore)
             nil))
           (append-original (symbol-function 'e-board-admission-append-event))
           (remove-original (symbol-function 'e-board-admission-remove-event))
           primary)
      (cl-letf (((symbol-function 'e-board-admission-append-event)
                 (lambda (&rest arguments)
                   (prog1 (apply append-original arguments)
                     (when (eq (nth 1 arguments) 'subscription-added)
                       (error "direct combined primary")))))
                ((symbol-function 'e-board-admission-remove-event)
                 (lambda (_receipt)
                   (error "direct combined cleanup"))))
        (setq primary
              (condition-case error
                  (progn
                    (e-board-enroll-invocation-work
                     board handle "call" '(target))
                    nil)
                (error error))))
      (should primary)
      (should (equal (error-message-string primary)
                     "direct combined primary"))
      (should (= (e-board-pending-admission-count board) 1))
      (should (e-board-observed-work board (e-work-handle-id handle)))
      (should (e-board-invocation board "call"))
      ;; Public retry recovers the same combined token before creating a new
      ;; Work/invocation pair; no duplicate event or index remains.
      (e-board-enroll-invocation-work board handle "call" '(retry-target))
      (should (= (e-board-pending-admission-count board) 0))
      (should (equal (mapcar #'e-board-event-type (e-board-events board))
                     '(posted subscription-added)))
      (should (eq (symbol-function 'e-board-admission-remove-event)
                  remove-original)))))

(ert-deftest e-board-test-direct-aggregation-admission-pending-recovery ()
  "A direct aggregation admission keeps its exact inverse after a fault."
  (e-board-admission-composition-test--with-empty-registry
    (let* ((board (e-board-create :id "direct-aggregation-pending"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "direct-aggregation-work" :execution 'cheap
              :interactive-policy 'cheap :runner #'ignore)
             nil))
           (append-original (symbol-function 'e-board-admission-append-event))
           (remove-original
            (symbol-function 'e-board-admission-remove-index))
           primary)
      (e-board-enroll-work board handle)
      (cl-letf (((symbol-function 'e-board-admission-append-event)
                 (lambda (&rest arguments)
                   (prog1 (apply append-original arguments)
                     (when (eq (nth 1 arguments) 'subscription-added)
                       (error "direct aggregation primary")))))
                ((symbol-function 'e-board-admission-remove-index)
                 (lambda (_receipt)
                   (error "direct aggregation cleanup"))))
        (setq primary
              (condition-case error
                  (progn
                    (e-board-subscribe-aggregation
                     board (list (e-work-handle-id handle)) 'all '(target)
                     :id "aggregation")
                    nil)
                (error error))))
      (should primary)
      (should (equal (error-message-string primary)
                     "direct aggregation primary"))
      (should (= (e-board-pending-admission-count board) 1))
      (should (e-board-aggregation board "aggregation"))
      (should (e-board-aggregation board "aggregation"))
      ;; The next public aggregation call retries the same exact token before
      ;; installing its replacement projection.
      (e-board-subscribe-aggregation
       board (list (e-work-handle-id handle)) 'all '(retry-target)
       :id "aggregation")
      (should (= (e-board-pending-admission-count board) 0))
      (should (equal
               (e-board-aggregation-effect-target
                (e-board-aggregation board "aggregation"))
               '(retry-target)))
      (should (eq (symbol-function 'e-board-admission-remove-index)
                  remove-original)))))

(ert-deftest e-board-test-aggregation-admission-abort-removes-exact-projection ()
  "Aggregation abort removes only its map, index, event, and timer projection."
  (e-board-admission-composition-test--with-empty-registry
    (let* ((board (e-board-create :id "aggregation-admission"))
           (handle
            (e-work-prepare
             (e-work-spec-create
              :id "aggregation-source" :execution 'cheap
              :interactive-policy 'cheap :runner #'ignore)
             nil))
           (admission (e-board-aggregation-admission-token board)))
      (e-board-enroll-work board handle)
      (let ((aggregation
             (e-board-subscribe-aggregation
              board (list (e-work-handle-id handle)) 'all (list 'target)
              :id "aggregation" :timeout 30 :admission admission)))
        (should (e-board-aggregation-admission-current-p admission))
        (should (eq (gethash "aggregation" (e-board-aggregations board))
                    aggregation))
        (e-board-abort-aggregation-admission board admission)
        (should-not (e-board-aggregation board "aggregation"))
        (should (equal (mapcar #'e-board-event-type (e-board-events board))
                       '(posted)))
        (should-not (e-board-aggregation-admission-current-p admission))
        (should (= (e-board-pending-admission-count board) 0))))))

(ert-deftest e-board-test-aggregation-admission-captures-precommit-activation-event ()
  "A pre-commit ready activation is removed by its exact admission receipt."
  (e-board-admission-composition-test--with-empty-registry
    (let* ((board (e-board-create :id "aggregation-precommit-activation"))
           (admission (e-board-aggregation-admission-token board))
           (append-original (symbol-function 'e-board-admission-append-event)))
      (cl-letf (((symbol-function 'e-board-admission-append-event)
                 (lambda (&rest arguments)
                   (prog1 (apply append-original arguments)
                     (when (eq (nth 1 arguments) 'activation-prepared)
                       (error "activation publication fault"))))))
      (should-error
         (e-board-subscribe-aggregation
          board nil 'all-terminal '(target)
          :id "precommit-activation" :admission admission)))
      (should-not (e-board-aggregation board "precommit-activation"))
      (should-not (e-board-events board))
      (should (= (hash-table-count (e-board-activations board)) 0))
      (should-not (e-board-aggregation-admission-current-p admission))
      (should (= (e-board-pending-admission-count board) 0)))))

(ert-deftest e-board-test-aggregation-admission-deadline-inverse-settles-routing-once ()
  "An exact queued deadline abort clears its routing count idempotently."
  (e-board-admission-composition-test--with-empty-registry
    (let (timer-callback deadline-drain)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq timer-callback
                         (lambda () (apply function arguments)))
                   'test-timer))
                ((symbol-function 'cancel-timer)
                 (lambda (_timer) nil)))
        (let* ((board
                (e-board-create
                 :id "aggregation-deadline-receipt"
                 :aggregation-deadline-scheduler
                 (lambda (drain) (setq deadline-drain drain))))
               (handle
                (e-work-prepare
                 (e-work-spec-create
                  :id "deadline-source" :execution 'cooperative
                  :interactive-policy 'async
                  :runner (lambda (_handle _arguments _context) :deferred))
                 nil))
               (admission (e-board-aggregation-admission-token board)))
          (e-board-enroll-work board handle)
          (e-board-subscribe-aggregation
           board (list (e-work-handle-id handle)) 'all '(target)
           :id "deadline-aggregation" :timeout 1 :admission admission)
          (funcall timer-callback)
          (should deadline-drain)
          (should (= (plist-get (e-board-unsettled-state board) :routing) 1))
          (e-board-abort-aggregation-admission board admission)
          (should (= (plist-get (e-board-unsettled-state board) :routing) 0))
          (should (= (hash-table-count
                      (e-board-aggregation-deadline-node-index board))
                     0))
          ;; Both an explicit repeated abort and a callback captured before
          ;; retirement are no-ops; neither can decrement routing twice.
          (e-board-abort-aggregation-admission board admission)
          (funcall deadline-drain)
          (should (= (plist-get (e-board-unsettled-state board) :routing) 0)))))))

(ert-deftest e-board-test-classifier-receipts-abort-in-either-order ()
  "Terminal classifier receipts repair exact neighbours in either order."
  (e-board-admission-composition-test--with-empty-registry
    (dolist (order '((0 1) (1 0)))
      (let* ((board (e-board-create
                     :id (format "classifier-order-%s" order)))
             (handle
              (e-work-prepare
               (e-work-spec-create
                :id (format "classifier-work-%s" order)
                :execution 'cheap :interactive-policy 'cheap
                :runner #'ignore)
               nil))
             (admission-one (e-board-aggregation-admission-token board))
             (admission-two (e-board-aggregation-admission-token board)))
        (e-board-enroll-work board handle)
        (let* ((work (e-board-observed-work board (e-work-handle-id handle)))
               (first (e-board-admission-queue-terminal-classification
                       board work nil nil nil admission-one))
               (second (e-board-admission-queue-terminal-classification
                        board work nil nil nil admission-two))
               (receipts (vector first second)))
          (dolist (index order)
            (e-board-admission-remove-terminal-classification
             (aref receipts index)))
          (should-not (e-board-terminal-classifications board))
          (should-not (e-board-terminal-classification-tail board))
          (should (= (e-board-unsettled-routing-count board) 0))
          (should (= (hash-table-count
                      (e-board-terminal-classification-node-index board))
                     0)))))))

(provide 'e-board-admission-composition-test)

;;; e-board-admission-composition-test.el ends here

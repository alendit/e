;;; e-board-runtime-test.el --- Tests for board harness delivery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-runtime)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-task-queue)

(defconst e-board-runtime-test--production-post-input
  (symbol-function 'e-board-runtime-post-input)
  "Unwrapped production ingress used by the runtime test adapter.")

(defconst e-board-runtime-test--core-post-input
  (symbol-function 'e-board-post-input)
  "Unwrapped core ingress used by the runtime test adapter.")

(defun e-board-runtime-test--source-post-input (source-board &rest arguments)
  "Post through SOURCE-BOARD with an authenticated registry test actor."
  (if (plist-member arguments :requester-actor)
      (apply e-board-runtime-test--core-post-input source-board arguments)
    (let* ((board (e-board-registry-get (e-board-id source-board)))
           (id "runtime-source-test-client")
           (client (or (gethash id (e-board-registry-board-clients board))
                       (e-board-registry-attach-client
                        board :id id
                        :principal (e-board-registry-board-principal board))))
           (actor (list 'client id (e-board-registry-client-generation client))))
      (apply e-board-runtime-test--core-post-input
             source-board (append arguments (list :requester-actor actor))))))

(defun e-board-runtime-test--post-input (board &rest arguments)
  "Call production ingress, supplying a generation-fenced test client.
Tests that explicitly provide `:requester' retain that exact requester."
  (if (or (plist-member arguments :requester)
          (not e-board-runtime--admission-open-p))
      (apply e-board-runtime-test--production-post-input board arguments)
    (let* ((id "runtime-test-client")
           (clients (e-board-registry-board-clients board))
           (client (or (gethash id clients)
                       (e-board-registry-attach-client
                        board :id id
                        :principal (e-board-registry-board-principal board))))
           (requester
            (e-board-registry-client-requester-context
             board (e-board-registry-client-id client))))
      (apply e-board-runtime-test--production-post-input
             board (append arguments (list :requester requester))))))

(defmacro e-board-runtime-test--with-empty-state (&rest body)
  "Run BODY with isolated board, registry, and runtime attachment state."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'e-board-runtime-post-input)
              #'e-board-runtime-test--post-input)
             ((symbol-function 'e-board-post-input)
              #'e-board-runtime-test--source-post-input))
     (let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
          (e-board-registry--boards (make-hash-table :test 'equal))
          (e-board-registry--id-sequence 0)
          (e-board-runtime--attachments (make-hash-table :test 'equal))
          (e-board-runtime--session-attachments (make-hash-table :test 'equal))
          (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
          (e-board-runtime--invocations (make-hash-table :test 'equal))
          (e-board-runtime--producer-bindings (make-hash-table :test 'equal))
          (e-board-runtime--producer-inputs (make-hash-table :test 'equal))
          (e-board-runtime--producer-deliveries (make-hash-table :test 'equal))
          (e-board-runtime--producer-turns (make-hash-table :test 'equal))
          (e-board-runtime--producer-epoch 0)
          (e-board-runtime--producer-head nil)
          (e-board-runtime--producer-tail nil)
          (e-board-runtime--producer-drain-scheduled nil)
          (e-board-runtime--producer-scheduler nil)
          (e-board-runtime--admission-open-p t)
          (e-board-runtime--admission-epoch 0)
          (e-board-runtime--quiescence-current nil)
          (e-board-runtime--unsettled-control-count 0)
          (e-board-runtime--unsettled-invocation-count 0)
          (e-board-runtime--unsettled-deferred-hook-count 0)
          (e-board-runtime--unsettled-producer-count 0)
          (e-board-runtime--unsettled-generation 0)
          (e-board-runtime--unsettled-change-function nil)
          (e-board-runtime--unsettled-change-functions nil)
          (e-board-registry--unsettled-pickup-count 0)
          (e-board-registry--unsettled-effect-count 0)
          (e-board-registry--unsettled-routing-count 0)
          (e-board-registry--unsettled-generation 0)
          (e-board-registry--unsettled-change-function nil)
          (e-board-registry--unsettled-change-functions nil)
          (e-harness-aggregate-unsettled-change-hook nil)
          (e-work--unsettled-count 0)
          (e-work--unsettled-generation 0)
          (e-work--unsettled-change-functions nil)
          (e-task-queue--unsettled-write-count 0)
          (e-task-queue--failed-write-count 0)
          (e-task-queue--unsettled-generation 0)
          (e-task-queue--unsettled-change-functions nil)
          (e-board-runtime--control-sequence 0)
          (e-board-runtime--deferred-hook-head nil)
          (e-board-runtime--deferred-hook-tail nil)
          (e-board-runtime--deferred-hook-drain-scheduled nil)
          (e-board-runtime--deferred-hook-generation 0)
          (e-board-runtime--work-activity-mailboxes (make-hash-table :test 'equal))
          (e-board-runtime--pending-activity-head nil)
          (e-board-runtime--pending-activity-tail nil)
          (e-board-runtime--pending-activity-set (make-hash-table :test 'equal))
          (e-board-runtime--activity-drain-scheduled nil)
          (e-board-runtime--pending-pickup-head nil)
          (e-board-runtime--pending-pickup-tail nil)
          (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
          (e-board-runtime--pickup-drain-scheduled nil)
          (e-harness-registry--instances (make-hash-table :test 'equal))
          (e-harness-registry--factories (make-hash-table :test 'equal))
          (e-harness-registry--generations (make-hash-table :test 'equal))
          (e-harness-registry--invalidation-events (make-hash-table :test 'equal))
          (e-harness-instance--instances (make-hash-table :test 'equal))
          (e-harness-instance--defaults (make-hash-table :test 'equal))
          (e-harness-instance--session-stores (make-hash-table :test 'equal))
          (e-harness-instance--generation 0))
       (e-harness-turn-state-reset-aggregate)
       (unwind-protect
           (progn ,@body)
         (e-harness-turn-state-reset-aggregate)))))

(ert-deftest e-board-runtime-test-unsettled-queues-publish-owner-transitions ()
  "Runtime queue counts change with enqueue/pop rather than a later scan."
  (e-board-runtime-test--with-empty-state
    (let ((board (e-board-registry-create :id "board"))
          scheduled snapshots)
      (setq e-board-runtime--unsettled-change-function
            (lambda (snapshot) (push snapshot snapshots)))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (push (lambda () (apply function arguments)) scheduled))))
        (e-board-runtime--schedule-deferred-hook nil 'receipt #'ignore)
        (should (= (plist-get (e-board-runtime-unsettled-state)
                              :deferred-hooks)
                   1))
        (funcall (pop scheduled))
        (should (= (plist-get (e-board-runtime-unsettled-state)
                              :deferred-hooks)
                   0))
        (e-board-runtime--enqueue-activity-flush "work")
        (should (= (plist-get (e-board-runtime-unsettled-state)
                              :activity-mailboxes)
                   1))
        (funcall (pop scheduled))
        (should (= (plist-get (e-board-runtime-unsettled-state)
                              :activity-mailboxes)
                   0))
        (e-board-runtime--enqueue-pickups board '("pickup"))
        (should (= (plist-get (e-board-runtime-unsettled-state)
                              :pickup-attempts)
                   1))
        (funcall (pop scheduled))
        (should (= (plist-get (e-board-runtime-unsettled-state)
                              :pickup-attempts)
                   0))
        (should (= (plist-get (e-board-runtime-unsettled-state) :generation)
                   6))
        (should (= (length snapshots) 6))))))

(ert-deftest e-board-runtime-test-unsettled-controls-and-invocations-settle-once ()
  "Control and invocation counts retire on their owning terminal transition."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           scheduled callback-state)
      (e-harness-create-session harness :id "session")
      (let ((attachment
             (e-board-runtime-attach
              board harness "session" :participant-id "participant"
              :principal "owner")))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (push (lambda () (apply function arguments)) scheduled))))
          (let ((request
                 (e-board-runtime-remove-participant-start
                  board "participant" "owner")))
            (should (= (plist-get (e-board-runtime-unsettled-state)
                                  :control-requests)
                       1))
            (e-request-cancel request 'test-cancel)
            (should (= (plist-get (e-board-runtime-unsettled-state)
                                  :control-requests)
                       0))
            (funcall (pop scheduled))
            (should (= (plist-get (e-board-runtime-unsettled-state)
                                  :control-requests)
                       0))))
        (let ((target
               (e-board-runtime--register-invocation
                attachment "turn" "call"
                (lambda (state _payload) (setq callback-state state)))))
          (should (= (plist-get (e-board-runtime-unsettled-state) :invocations)
                     1))
          (e-board-runtime--apply-invocation-effect board target 'completed nil)
          (should (eq callback-state 'completed))
          (should (= (plist-get (e-board-runtime-unsettled-state) :invocations)
                     0))
          (should-error
           (e-board-runtime--apply-invocation-effect
            board target 'completed nil)
           :type 'e-board-runtime-error)
          (should (= (plist-get (e-board-runtime-unsettled-state) :invocations)
                     0)))))))

(ert-deftest e-board-runtime-test-admission-close-is-bounded-and-token-fenced ()
  "One exact closed epoch rejects new roots and only its token reopens it."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (first-token (e-board-runtime-close-admission)))
      (e-harness-create-session harness :id "session")
      (should (equal (e-board-runtime-admission-state)
                     '(:state closed :epoch 1)))
      (dolist (root
               (list
                (lambda ()
                  (e-board-runtime-attach
                   board harness "session" :participant-id "participant"))
                (lambda () (e-board-runtime-attach-instance nil nil nil))
                (lambda ()
                  (e-board-runtime-remove-participant-start nil nil nil))
                (lambda ()
                  (e-board-runtime-rebind-start nil nil nil nil nil))
                (lambda ()
                  (e-board-runtime-move-participant-start nil nil nil nil))
                (lambda ()
                  (e-board-runtime-post-input
                   board :id "input" :content "blocked"))))
        (should-error (funcall root) :type 'e-board-runtime-admission-closed))
      (should (= (hash-table-count
                  (e-board-registry-board-participants board))
                 0))
      (should (equal (e-board-runtime-reopen-admission first-token)
                     '(:state open :epoch 1)))
      (let ((second-token (e-board-runtime-close-admission)))
        (should-error (e-board-runtime-reopen-admission first-token)
                      :type 'e-board-runtime-error)
        (should (equal (e-board-runtime-reopen-admission second-token)
                       '(:state open :epoch 2))))
      (should
       (e-board-runtime-attach
        board harness "session" :participant-id "participant")))))

(ert-deftest e-board-runtime-test-quiescence-settles-from-owner-notification ()
  "A controlled request settles after the last indexed owner transition."
  (e-board-runtime-test--with-empty-state
    (let* ((scheduled nil)
           (heartbeat 0)
           (board (e-board-registry-create :id "board")))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (push (lambda () (apply function arguments)) scheduled))))
        (e-board--schedule-effect
         (e-board-registry-board-source-board board) #'ignore)
        (let* ((quiescence (e-board-runtime-request-quiescence))
               (request (e-board-runtime-quiescence-request quiescence)))
          (should (eq (e-request-lifecycle-state request) 'started))
          (should (equal (plist-get (e-request-lifecycle-progress request)
                                    :state)
                         'draining))
          (run-at-time 0 nil (lambda () (cl-incf heartbeat)))
          ;; The independent callback remains runnable while quiescence waits.
          (funcall (pop scheduled))
          (should (= heartbeat 1))
          (should-not (e-request-terminal-p request))
          ;; The board owner transition itself wakes and settles the request.
          (funcall (pop scheduled))
          (should (eq (e-request-lifecycle-state request) 'finished))
          (should (equal (plist-get (e-request-lifecycle-terminal-payload request)
                                    :state)
                         'quiescent))
          (should-not e-board-runtime--quiescence-current)
          (should (equal (e-board-runtime-admission-state)
                         '(:state closed :epoch 1)))
          (e-board-runtime-reopen-admission
           (e-board-runtime-quiescence-admission-token quiescence)))))))

(ert-deftest e-board-runtime-test-quiescence-waits-for-terminal-and-deadline-queues ()
  "Deferred board reducers remain blockers until their bounded drains retire."
  (e-board-runtime-test--with-empty-state
    (let* ((scheduled nil)
           (board (e-board-registry-create :id "board"))
           (source (e-board-registry-board-source-board board)))
      (setf (e-board-terminal-classification-scheduler source)
            (lambda (drain) (setq scheduled (append scheduled (list drain))))
            (e-board-aggregation-deadline-scheduler source)
            (lambda (drain) (setq scheduled (append scheduled (list drain)))))
      (e-board--queue-terminal-classification source "work" '("invocation") nil)
      (e-board--queue-aggregation-deadline source "aggregation")
      (let* ((quiescence (e-board-runtime-request-quiescence))
             (request (e-board-runtime-quiescence-request quiescence)))
        (should (eq (e-request-lifecycle-state request) 'started))
        (should (= (plist-get (e-board-unsettled-state source) :routing) 2))
        (funcall (pop scheduled))
        (should (eq (e-request-lifecycle-state request) 'started))
        (funcall (pop scheduled))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (should (= (plist-get (e-board-unsettled-state source) :routing) 0))))))

(ert-deftest e-board-runtime-test-quiescence-waits-for-task-writer ()
  "A domain writer timer is an activation blocker until its owner retires it."
  (e-board-runtime-test--with-empty-state
    (let* ((e-task-queue--unsettled-write-count 1)
           (quiescence (e-board-runtime-request-quiescence))
           (request (e-board-runtime-quiescence-request quiescence)))
      (should (eq (e-request-lifecycle-state request) 'started))
      (should (equal (plist-get (e-request-lifecycle-progress request) :blockers)
                     '((task-persistence.writes . 1))))
      (e-task-queue--adjust-writer-state 'writes -1)
      (should (eq (e-request-lifecycle-state request) 'finished)))))

(ert-deftest e-board-runtime-test-cancelled-quiescence-keeps-admission-closed ()
  "Cancelling observation never silently reopens the fenced admission epoch."
  (e-board-runtime-test--with-empty-state
    (let* ((e-work--unsettled-count 1)
           (quiescence (e-board-runtime-request-quiescence))
           (request (e-board-runtime-quiescence-request quiescence)))
      (e-request-cancel request 'stopped)
      (should (eq (e-request-lifecycle-state request) 'cancelled))
      (should-not e-board-runtime--quiescence-current)
      (should (equal (e-board-runtime-admission-state)
                     '(:state closed :epoch 1)))
      (e-board-runtime-reopen-admission
       (e-board-runtime-quiescence-admission-token quiescence)))))

(ert-deftest e-board-runtime-test-producer-requires-explicit-live-binding ()
  "A producer cannot restore authority from a historical board id alone."
  (e-board-runtime-test--with-empty-state
    (should-error (e-board-runtime-producer-bind 'cron "historical-board")
                  :type 'e-board-runtime-producer-disabled)
    (let* ((board (e-board-registry-create :id "board"))
           (binding (e-board-runtime-producer-bind 'cron board)))
      (e-board-runtime-producer-disable binding)
      (should-error (e-board-runtime-producer-publish-fact binding :tags '(cron))
                    :type 'e-board-runtime-producer-disabled))))

(ert-deftest e-board-runtime-test-stale-producer-binding-never-fires-work ()
  "Replacing a binding fences an already scheduled old-generation fact."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           scheduled
           (e-board-runtime--producer-scheduler
            (lambda (drain) (push drain scheduled)))
           (old (e-board-runtime-producer-bind 'cron board :tags '(cron)))
           (item (e-board-runtime-producer-publish-fact old :content "fire")))
      (e-board-runtime-producer-bind 'cron board :tags '(cron replacement))
      (funcall (pop scheduled))
      (should (eq (e-board-runtime-producer-publication-state item) 'failed))
      (should-not (e-board-messages (e-board-registry-board-source-board board)))
      (should (= (plist-get (e-board-runtime-unsettled-state) :producer-items)
                 0)))))

(ert-deftest e-board-runtime-test-producer-retry-reuses-source-key ()
  "A retry after uncertain append resolves as one idempotent board fact."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           scheduled
           (e-board-runtime--producer-scheduler
            (lambda (drain) (push drain scheduled)))
           (binding (e-board-runtime-producer-bind 'cron board :tags '(cron)))
           (original (symbol-function 'e-board-post-fact))
           (first t)
           item)
      (cl-letf (((symbol-function 'e-board-post-fact)
                 (lambda (&rest arguments)
                   (let ((publication (apply original arguments)))
                     (when first
                       (setq first nil)
                       (error "uncertain producer acknowledgement"))
                     publication))))
        (setq item
              (e-board-runtime-producer-publish-fact
               binding :attributes '(:schedule "daily") :content "fire"))
        (funcall (pop scheduled))
        (should (eq (e-board-runtime-producer-publication-state item) 'failed))
        (e-board-runtime-producer-retry item)
        (funcall (pop scheduled)))
      (should (eq (e-board-runtime-producer-publication-state item) 'published))
      (should (eq (e-board-publication-status
                   (e-board-runtime-producer-publication-publication item))
                  'duplicate))
      (should (= (e-board-message-count
                  (e-board-registry-board-source-board board))
                 1)))))

(ert-deftest e-board-runtime-test-producer-zero-match-stays-observation-only ()
  "A fact with no orchestration match creates no participant, pickup, or turn."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           scheduled
           (e-board-runtime--producer-scheduler
            (lambda (drain) (push drain scheduled)))
           (binding (e-board-runtime-producer-bind 'file-watch board :tags '(file)))
           (item (e-board-runtime-producer-publish-fact binding :content "changed")))
      (funcall (pop scheduled))
      (let ((source (e-board-registry-board-source-board board)))
        (should (eq (e-board-runtime-producer-publication-state item) 'published))
        (should (= (hash-table-count (e-board-registry-board-participants board)) 0))
        (should (= (hash-table-count (e-board-pickups source)) 0))
        (should (= (e-board-message-count source) 1))))))

(ert-deftest e-board-runtime-test-producer-input-settles-from-causal-turn ()
  "Trusted work remains unsettled from routing through its terminal turn."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (binding (e-board-runtime-producer-bind 'tasks board))
           result attachment item)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach
             board harness "session" :participant-id "participant"
             :delivery-function (lambda (&rest _arguments) '(:accepted receipt))))
      (e-board-registry-install-subscription
       board "participant" '(:tags (task)) :id "tasks")
      (setq item
            (e-board-runtime-producer-publish-input
             binding :tags '(task) :content "work"
             :on-settle (lambda (&rest terminal) (setq result terminal))))
      (should (= (plist-get (e-board-runtime-unsettled-state) :producer-items) 1))
      (e-board-runtime-drain-producers)
      (let ((source (e-board-registry-board-source-board board)))
        (should
         (equal (e-board-message-requester-actor
                 (e-board-publication-message
                  (e-board-runtime-producer-publication-publication item)))
                (list 'client
                      (e-board-registry-client-id
                       (e-board-runtime-producer-binding-client binding))
                      (e-board-registry-client-generation
                       (e-board-runtime-producer-binding-client binding)))))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source)))
        (should (eq (e-board-runtime-producer-publication-state item) 'dispatched))
        (should-not result)
        (e-board-runtime--drain-pickups)
        (let* ((delivery-id
                (car (e-board-runtime-producer-publication-pending-delivery-ids item)))
               (pickup (e-board-pickup source delivery-id))
               (attempt (e-board-pickup-attempt pickup)))
          (e-board-runtime--handle-harness-event
           attachment
           (e-events-make
            :type 'input-consumed :session-id "session" :turn-id "turn"
            :payload
            (list :delivery-id delivery-id
                  :endpoint-token
                  (e-board-delivery-attempt-endpoint-token attempt)
                  :endpoint-generation
                  (e-board-delivery-attempt-composite-generation attempt))))
          (should-not result)
          (e-board-runtime--handle-harness-event
           attachment
           (e-events-make :type 'turn-finished :session-id "session"
                          :turn-id "turn"))
          (should (eq (plist-get result :status) 'done))
          (should (= (plist-get (e-board-runtime-unsettled-state)
                                :producer-items)
                     0)))))))

(ert-deftest e-board-runtime-test-producer-input-broadcast-waits-for-all-pickups ()
  "Broadcast producer work aggregates all participant terminal outcomes."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (binding (e-board-runtime-producer-bind 'tasks board))
           result)
      (dolist (id '("one" "two"))
        (e-board-registry-add-participant board :id id)
        (e-board-registry-install-subscription board id '(:tags (task))))
      (let ((item
             (e-board-runtime-producer-publish-input
              binding :tags '(task) :content "work"
              :on-settle (lambda (&rest terminal) (setq result terminal)))))
        (e-board-runtime-drain-producers)
        (e-board-runtime--drain-input-routing
         board
         (lambda ()
           (e-board-drain-input-classifications
            (e-board-registry-board-source-board board))))
        (let ((deliveries
               (copy-tree
                (e-board-runtime-producer-publication-pending-delivery-ids item))))
          (should (= (length deliveries) 2))
          (e-board-runtime--producer-delivery-terminal
           item (car deliveries) 'done '(:turn-id "one"))
          (should-not result)
          (e-board-runtime--producer-delivery-terminal
           item (cadr deliveries) 'failed '(:turn-id "two"))
          (should (eq (plist-get result :status) 'failed)))))))

(ert-deftest e-board-runtime-test-cancelled-producer-input-never-routes ()
  "Cancelling accepted producer work fences both queued and routing states."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (binding (e-board-runtime-producer-bind 'tasks board))
           (queued (e-board-runtime-producer-publish-input
                    binding :tags '(task) :content "queued")))
      (e-board-runtime-producer-cancel queued)
      (e-board-runtime-drain-producers)
      (should-not (e-board-messages (e-board-registry-board-source-board board)))
      (let ((routing (e-board-runtime-producer-publish-input
                      binding :tags '(task) :content "routing")))
        (e-board-runtime-drain-producers)
        (e-board-runtime-producer-cancel routing)
        (e-board-runtime--drain-input-routing
         board
         (lambda ()
           (e-board-drain-input-classifications
            (e-board-registry-board-source-board board))))
        (should (eq (e-board-message-routing-state
                     (e-board-publication-message
                      (e-board-runtime-producer-publication-publication routing)))
                    'routing-cancelled))
        (should (= (plist-get (e-board-runtime-unsettled-state) :producer-items)
                   0))))))

(ert-deftest e-board-runtime-test-deferred-hooks-use-bounded-fifo-drains ()
  "Deferred hooks preserve order and yield after each configured record page."
  (e-board-runtime-test--with-empty-state
    (let ((e-board-runtime-deferred-hook-drain-limit 1)
          scheduled started)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (push (lambda () (apply function arguments)) scheduled))))
        (e-board-runtime--schedule-deferred-hook
         nil 'first (lambda () (setq started (append started '(first)))))
        (e-board-runtime--schedule-deferred-hook
         nil 'second (lambda () (setq started (append started '(second)))))
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
        (should (equal started '(first)))
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
        (should (equal started '(first second)))
        (should-not scheduled)))))

(ert-deftest e-board-runtime-test-stale-deferred-hooks-use-the-drain-budget ()
  "Generation-fenced stale records cannot bypass the deferred hook page bound."
  (e-board-runtime-test--with-empty-state
    (let ((e-board-runtime-deferred-hook-drain-limit 1)
          scheduled started)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (push (lambda () (apply function arguments)) scheduled))))
        (e-board-runtime--schedule-deferred-hook nil 'stale-one #'ignore)
        (e-board-runtime--schedule-deferred-hook nil 'stale-two #'ignore)
        (cl-incf e-board-runtime--deferred-hook-generation)
        (e-board-runtime--schedule-deferred-hook
         nil 'current (lambda () (setq started t)))
        (funcall (pop scheduled))
        (should-not started)
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
        (should-not started)
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
        (should started)
        (should-not scheduled)))))

(ert-deftest e-board-runtime-test-stale-activity-mailboxes-use-the-drain-budget ()
  "Stale activity queue entries yield after each configured record page."
  (e-board-runtime-test--with-empty-state
    (let ((e-board-runtime-activity-drain-limit 1)
          scheduled)
      (setq e-board-runtime--pending-activity-head '(first second)
            e-board-runtime--pending-activity-tail (last e-board-runtime--pending-activity-head))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (push (lambda () (apply function arguments)) scheduled))))
        (e-board-runtime--drain-activity-mailboxes)
        (should (equal e-board-runtime--pending-activity-head '(second)))
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
        (should-not e-board-runtime--pending-activity-head)
        (should-not scheduled)))))

(ert-deftest e-board-runtime-test-stale-pickups-use-the-drain-budget ()
  "Expired board lookup entries yield after each configured pickup page."
  (e-board-runtime-test--with-empty-state
    (let ((e-board-runtime-pickup-drain-limit 1)
          scheduled)
      (setq e-board-runtime--pending-pickup-head '(("missing-one" "one")
                                                    ("missing-two" "two"))
            e-board-runtime--pending-pickup-tail (last e-board-runtime--pending-pickup-head))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (push (lambda () (apply function arguments)) scheduled))))
        (e-board-runtime--drain-pickups)
        (should (equal e-board-runtime--pending-pickup-head
                       '(("missing-two" "two"))))
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
        (should-not e-board-runtime--pending-pickup-head)
        (should-not scheduled)))))

(ert-deftest e-board-runtime-test-attachment-maps-live-session-and-delivers-exact-and-tags ()
  "Attached sessions receive only their frozen exact or tag-routed pickups."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (first-harness (e-harness-create))
           (second-harness (e-harness-create))
           (deliveries nil)
           (delivery
            (lambda (attachment pickup message)
              (setq deliveries
                    (append deliveries
                            (list (list
                                   (e-board-registry-participant-id
                                    (e-board-runtime-attachment-participant attachment))
                                   (e-board-pickup-delivery-id pickup)
                                   (e-board-message-content message))))))))
      (e-harness-create-session first-harness :id "first-session")
      (e-harness-create-session second-harness :id "second-session")
      (let* ((first (e-board-runtime-attach
                     board first-harness "first-session"
                     :participant-id "first" :delivery-function delivery))
             (_second (e-board-runtime-attach
                       board second-harness "second-session"
                       :participant-id "second" :delivery-function delivery)))
        (e-board-registry-install-subscription board "first" '(:tags (main))
                                               :id "first-main")
        (e-board-registry-install-subscription board "second" '(:tags (main))
                                               :id "second-main")
        (should (eq (e-board-runtime-attachment-board first) board))
        (should (equal (e-board-registry-participant-id
                        (e-board-runtime-attachment-participant first))
                       "first"))
        (should (eq (e-board-runtime-attachment-harness first) first-harness))
        (should (equal (e-board-runtime-attachment-session-id first)
                       "first-session"))
        (e-board-runtime-post-input board :id "exact" :to "first" :tags '(main)
                                    :content "exact message")
        (e-board-runtime-post-input board :id "tagged" :tags '(main)
                                    :content "tagged message")
        ;; Ingress only appends/enqueues.  A bounded later drain owns all
        ;; harness delivery attempts, so routing cannot start a turn inline.
        (should-not deliveries)
        (e-board-runtime--drain-input-routing
         board
         (lambda ()
           (e-board-drain-input-classifications
            (e-board-registry-board-source-board board))))
        (should-not deliveries)
        (e-board-runtime--drain-pickups)
        ;; The second pickup for first waits behind its exact-address head,
        ;; then becomes ready before the other participant's tag pickup.
        (should (equal (mapcar #'car deliveries) '("first" "first" "second")))
        (should (equal (mapcar #'car (mapcar #'cdr deliveries))
                       '(("board" "exact" "first")
                         ("board" "tagged" "first")
                         ("board" "tagged" "second"))))))))

(ert-deftest e-board-runtime-test-qualified-attachment-captures-composite-generation ()
  "Configured attachment never creates a harness and fences replaced endpoints."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (factory-calls 0)
           deliveries)
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :qualified :kind 'chat :harness-id :live
       :factory (lambda () (cl-incf factory-calls) harness))
      (should-error
       (e-board-runtime-attach-instance
        board :qualified "session" :participant-id "participant")
       :type 'e-harness-registry-missing)
      (should (= factory-calls 0))
      (should (= (hash-table-count
                  (e-board-registry-board-participants board))
                 0))
      (e-harness-registry-register :live harness)
      (let* ((attachment
              (e-board-runtime-attach-instance
               board :qualified "session" :participant-id "participant"
               :delivery-function
               (lambda (_attachment _pickup message)
                 (push (e-board-message-content message) deliveries))))
             (token (e-board-runtime-attachment-endpoint-token attachment)))
        (should (= factory-calls 0))
        (should (e-board-runtime--current-attachment-p attachment))
        (should (eq (e-board-runtime-attachment-instance-id attachment)
                    :qualified))
        (should (= (e-board-runtime-attachment-instance-catalog-generation
                    attachment)
                   1))
        (should (= (e-board-runtime-attachment-harness-object-generation
                    attachment)
                   1))
        (should (eq (e-board-runtime-endpoint-token-harness-id token) :live))
        (should (equal (e-board-runtime-endpoint-token-session-id token)
                       "session"))
        (e-board-runtime-post-input board :id "pending" :to "participant"
                                    :content "never deliver stale")
        (e-board-runtime--drain-input-routing
         board
         (lambda ()
           (e-board-drain-input-classifications
            (e-board-registry-board-source-board board))))
        (e-harness-registry-clear-instance :live)
        (should-not (e-board-runtime--current-attachment-p attachment))
        (e-board-runtime--drain-pickups)
        (should-not deliveries)
        (let* ((source-board (e-board-registry-board-source-board board))
               (delivery-id (car (e-board--pickup-queue
                                  source-board "participant"))))
          (should (eq (e-board-pickup-state
                       (e-board-pickup source-board delivery-id))
                      'ready)))))))

(ert-deftest e-board-runtime-test-commits-qualified-attempt-before-adapter-call ()
  "The board owns the selected endpoint binding before delivery code runs."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           observed)
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :qualified :kind 'chat :harness-id :live)
      (e-harness-registry-register :live harness)
      (let ((attachment
             (e-board-runtime-attach-instance
              board :qualified "session" :participant-id "participant"
              :delivery-function
              (lambda (current pickup _message)
                (let ((attempt (e-board-pickup-attempt pickup)))
                  (setq observed
                        (list (e-board-pickup-state pickup)
                              (e-board-delivery-attempt-state attempt)
                              (equal (e-board-delivery-attempt-endpoint-token attempt)
                                     (e-board-runtime-attachment-endpoint-token current))
                              (e-board-delivery-attempt-composite-generation attempt))))))))
        (let* ((publication
                (e-board-runtime-post-input
                 board :id "input" :to "participant" :content "deliver"))
               (source-board (e-board-registry-board-source-board board))
               (delivery-id (car (e-board-publication-pickup-ids publication))))
          (e-board-runtime--drain-input-routing
           board (lambda () (e-board-drain-input-classifications source-board)))
          (setq delivery-id
                (car (e-board-message-pickup-ids
                      (e-board-message source-board "input"))))
          (e-board-runtime--drain-pickups)
          (should (equal observed '(delivering delivering t (1 1))))
          (let* ((pickup (e-board-pickup source-board delivery-id))
                 (attempt (e-board-pickup-attempt pickup)))
            (should (eq (e-board-pickup-state pickup) 'consumed))
            (should (eq (e-board-delivery-attempt-state attempt) 'consumed))
            (should (equal
                     (e-board-delivery-attempt-endpoint-token attempt)
                     (e-board-runtime-attachment-endpoint-token attachment)))))))))

(ert-deftest e-board-runtime-test-acceptance-rechecks-bound-endpoint-generation ()
  "An endpoint replacement during delivery cannot accept the stale attempt."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :qualified :kind 'chat :harness-id :live)
      (e-harness-registry-register :live harness)
      (e-board-runtime-attach-instance
       board :qualified "session" :participant-id "participant"
       :delivery-function
       (lambda (&rest _arguments)
         (e-harness-registry-clear-instance :live)
         '(:accepted stale-receipt)))
      (e-board-runtime-post-input
       board :id "input" :to "participant" :content "deliver")
      (let ((source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (let ((delivery-id
               (car (e-board-message-pickup-ids
                     (e-board-message source-board "input")))))
          (e-board-runtime--drain-pickups)
          (let* ((pickup (e-board-pickup source-board delivery-id))
                 (attempt (e-board-pickup-attempt pickup)))
            (should (eq (e-board-pickup-state pickup) 'uncertain))
            (should (eq (e-board-delivery-attempt-state attempt) 'uncertain))
            (should-not (equal (e-board-delivery-attempt-receipt attempt)
                               'stale-receipt))))))))

(ert-deftest e-board-runtime-test-revoked-delivery-fails-before-adapter-call ()
  "A post-routing principal revoke creates a tombstone without endpoint access."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           (calls 0))
      (e-board-registry-authorize-principal board "owner" "agent" 'member)
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach
       board harness "session" :participant-id "participant" :principal "agent"
       :delivery-function
       (lambda (&rest _arguments) (cl-incf calls)))
      (e-board-runtime-post-input
       board :id "input" :to "participant" :content "never expose")
      (let ((source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (e-board-registry-revoke-principal board "owner" "agent")
        (let ((delivery-id
               (car (e-board-message-pickup-ids
                     (e-board-message source-board "input")))))
          (e-board-runtime--drain-pickups)
          (let ((pickup (e-board-pickup source-board delivery-id)))
            (should (= calls 0))
            (should (eq (e-board-pickup-state pickup) 'failed))
            (should-not (e-board-pickup-attempt pickup))))))))

(ert-deftest e-board-runtime-test-requester-membership-revoke-fences-delivery ()
  "A routed tagged pickup rechecks its frozen requester's current membership."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           (calls 0))
      (e-board-registry-authorize-principal board "owner" "requester" 'member)
      (let* ((client
              (e-board-registry-attach-client
               board :id "client" :principal "requester"))
             (context
              (e-board-registry-client-requester-context
               board (e-board-registry-client-id client))))
        (e-harness-create-session harness :id "session")
        (e-board-runtime-attach
         board harness "session" :participant-id "participant"
         :delivery-function (lambda (&rest _arguments) (cl-incf calls)))
        (e-board-registry-install-subscription
         board "participant" '(:tags (main)) :id "main")
        (e-board-runtime-post-input
         board :id "input" :tags '(main) :requester context :content "secret")
        (let ((source-board (e-board-registry-board-source-board board)))
          (e-board-runtime--drain-input-routing
           board (lambda () (e-board-drain-input-classifications source-board)))
          (e-board-registry-revoke-principal board "owner" "requester")
          (let ((delivery-id
                 (car (e-board-message-pickup-ids
                       (e-board-message source-board "input")))))
            (e-board-runtime--drain-pickups)
            (should (= calls 0))
            (should (eq (e-board-pickup-state
                         (e-board-pickup source-board delivery-id))
                        'failed))))))))

(ert-deftest e-board-runtime-test-exact-grant-revoke-fences-delivery ()
  "A routed exact pickup rechecks the target-owned post grant before exposure."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           (calls 0))
      (e-board-registry-authorize-principal board "owner" "requester" 'member)
      (e-board-registry-authorize-principal board "owner" "agent" 'member)
      (let* ((client
              (e-board-registry-attach-client
               board :id "client" :principal "requester"))
             (context
              (e-board-registry-client-requester-context
               board (e-board-registry-client-id client))))
        (e-harness-create-session harness :id "session")
        (e-board-runtime-attach
         board harness "session" :participant-id "participant" :principal "agent"
         :delivery-function (lambda (&rest _arguments) (cl-incf calls)))
        (e-board-registry-grant-participant-access
         board "owner" "participant" "requester" '(post))
        (e-board-runtime-post-input
         board :id "input" :to "participant" :requester context :content "secret")
        (let ((source-board (e-board-registry-board-source-board board)))
          (e-board-runtime--drain-input-routing
           board (lambda () (e-board-drain-input-classifications source-board)))
          (e-board-registry-revoke-participant-access
           board "owner" "participant" "requester")
          (let ((delivery-id
                 (car (e-board-message-pickup-ids
                       (e-board-message source-board "input")))))
            (e-board-runtime--drain-pickups)
            (should (= calls 0))
            (should (eq (e-board-pickup-state
                         (e-board-pickup source-board delivery-id))
                        'failed))))))))

(ert-deftest e-board-runtime-test-revoke-during-acceptance-cancels-bound-item ()
  "A proven accepted item reconciles after a grant disappears during its call."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           attachment)
      (e-board-registry-authorize-principal board "owner" "agent" 'member)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach
             board harness "session" :participant-id "participant"
             :principal "agent"
             :delivery-function
             (lambda (&rest _arguments)
               (e-board-registry-revoke-principal board "owner" "agent")
               '(:accepted receipt))))
      (e-board-runtime-post-input
       board :id "input" :to "participant" :content "accepted")
      (let ((source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (let ((delivery-id
               (car (e-board-message-pickup-ids
                     (e-board-message source-board "input")))))
          (e-board-runtime--drain-pickups)
          (let* ((pickup (e-board-pickup source-board delivery-id))
                 (attempt (e-board-pickup-attempt pickup)))
            (should (eq (e-board-pickup-state pickup) 'cancelling))
            (should (equal (e-board-delivery-attempt-receipt attempt) 'receipt))
            (should (eq (e-board-delivery-attempt-reason attempt)
                        'delivery-authorization-revoked))
            (e-board-runtime--handle-harness-event
             attachment
             (e-events-make
              :type 'input-consumed :session-id "session" :turn-id "turn"
              :payload
              (list :delivery-id delivery-id
                    :endpoint-token
                    (e-board-delivery-attempt-endpoint-token attempt)
                    :endpoint-generation
                    (e-board-delivery-attempt-composite-generation attempt))))
            (should (eq (e-board-pickup-state pickup) 'consumed))))))))

(ert-deftest e-board-runtime-test-remove-participant-cancels-only-before-commit ()
  "A prepared removal can cancel without changing the live attachment."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           scheduled)
      (e-harness-create-session harness :id "session")
      (let ((attachment
             (e-board-runtime-attach
              board harness "session" :participant-id "participant")))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq scheduled
                           (append scheduled
                                   (list (lambda ()
                                           (apply function arguments))))))))
          (let ((request
                 (e-board-runtime-remove-participant-start
                  board "participant" "owner")))
            (should (eq (e-request-lifecycle-state request) 'started))
            (should (= (length scheduled) 1))
            (e-request-cancel request 'user-cancelled)
            (should (eq (e-request-lifecycle-state request) 'cancelled))
            (should (eq (e-board-runtime-attachment-state attachment) 'active))
            (should-not
             (e-board-runtime-attachment-reconciliation attachment))
            (funcall (pop scheduled))
            (should (e-board-runtime--current-attachment-p attachment))
            (should (e-board-registry-participant board "participant"))))))))

(ert-deftest e-board-runtime-test-remove-participant-revalidates-owner-at-commit ()
  "A requester revoked before the scheduled commit cannot begin detaching."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           scheduled)
      (e-board-registry-authorize-principal board "owner" "other-owner" 'owner)
      (e-harness-create-session harness :id "session")
      (let ((attachment
             (e-board-runtime-attach
              board harness "session" :participant-id "participant")))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq scheduled
                           (append scheduled
                                   (list (lambda ()
                                           (apply function arguments))))))))
          (let ((request
                 (e-board-runtime-remove-participant-start
                  board "participant" "owner")))
            (e-board-registry-revoke-principal
             board "other-owner" "owner")
            (funcall (pop scheduled))
            (should (eq (e-request-lifecycle-state request) 'failed))
            (should (eq (car (e-request-lifecycle-terminal-payload request))
                        'e-board-registry-authorization-denied))
            (should (eq (e-board-runtime-attachment-state attachment) 'active))
            (should (e-board-runtime--current-attachment-p attachment))
            (should-not
             (e-board-runtime-attachment-reconciliation attachment))))))))

(ert-deftest e-board-runtime-test-remove-participant-reconciles-bounded-fifo ()
  "Removal fences ingress and tombstones one undelivered FIFO item per step."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           (source-board (e-board-registry-board-source-board board))
           scheduled request attachment first-id second-id)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach
             board harness "session" :participant-id "participant"))
      (e-board-registry-attach-client
       board :id "owner-client" :principal "owner")
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq scheduled
                         (append scheduled
                                 (list (lambda ()
                                         (apply function arguments))))))))
        (let ((first (e-board-post-input
                      source-board :id "first" :to "participant"
                      :content "first"))
              (second (e-board-post-input
                       source-board :id "second" :to "participant"
                       :content "second")))
          (e-board-drain-input-classifications source-board)
          (setq first-id (car (e-board-publication-pickup-ids first))
                second-id (car (e-board-publication-pickup-ids second))
                scheduled nil))
        (setq request
              (e-board-runtime-remove-participant-start
               board "participant" "owner"))
        (funcall (pop scheduled))
        (should (eq (e-board-runtime-attachment-state attachment) 'detaching))
        (e-board-runtime--deliver-pickups board (list first-id))
        (should (eq (e-board-pickup-state
                     (e-board-pickup source-board first-id))
                    'ready))
        (should-error
         (e-board-runtime-post-input
          board :id "late" :to "participant"
          :requester
          (e-board-registry-client-requester-context board "owner-client")
          :content "late")
         :type 'e-board-registry-authorization-denied)
        (should-not (e-board-message source-board "late"))
        (should-error (e-request-cancel request 'too-late)
                      :type 'e-board-runtime-control-committed)
        (funcall (pop scheduled))
        (should (eq (e-board-pickup-state
                     (e-board-pickup source-board first-id))
                    'cancelled))
        (should (eq (e-board-pickup-state
                     (e-board-pickup source-board second-id))
                    'ready))
        (funcall (pop scheduled))
        (should (eq (e-board-pickup-state
                     (e-board-pickup source-board second-id))
                    'cancelled))
        (should (eq (e-request-lifecycle-state request) 'progress))
        (funcall (pop scheduled))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (should (eq (e-board-runtime-attachment-state attachment) 'dormant))
        (should-not (e-board-runtime--current-attachment-p attachment))
        (should-not
         (gethash "participant"
                  (e-board-registry-board-participants board)))
        (should (equal (mapcar #'e-board-message-content
                               (e-board-messages source-board))
                       '("first" "second")))))))

(ert-deftest e-board-runtime-test-remove-participant-awaits-accepted-receipt ()
  "Removal retains an accepted endpoint binding until its receipt settles."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           (source-board (e-board-registry-board-source-board board))
           scheduled request attachment delivery-id pickup attempt)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach
             board harness "session" :participant-id "participant"
             :delivery-function (lambda (&rest _arguments)
                                  '(:accepted receipt))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq scheduled
                         (append scheduled
                                 (list (lambda ()
                                         (apply function arguments))))))))
        (let ((publication
               (e-board-post-input
                source-board :id "accepted" :to "participant"
                :content "accepted")))
          (e-board-drain-input-classifications source-board)
          (setq delivery-id (car (e-board-publication-pickup-ids publication))
                scheduled nil))
        (e-board-runtime--deliver-pickups board (list delivery-id))
        (setq pickup (e-board-pickup source-board delivery-id)
              attempt (e-board-pickup-attempt pickup)
              request (e-board-runtime-remove-participant-start
                       board "participant" "owner"))
        (should (eq (e-board-pickup-state pickup) 'accepted))
        (funcall (pop scheduled))
        (funcall (pop scheduled))
        (should (eq (e-board-pickup-state pickup) 'cancelling))
        (should (equal (e-board-delivery-attempt-receipt attempt) 'receipt))
        (funcall (pop scheduled))
        (should-not scheduled)
        (should (eq (e-request-lifecycle-state request) 'progress))
        (should (e-board-registry-participant board "participant"))
        (e-board-runtime--handle-harness-event
         attachment
         (e-events-make
          :type 'input-consumed :session-id "session" :turn-id "turn"
          :payload
          (list :delivery-id delivery-id
                :endpoint-token
                (e-board-delivery-attempt-endpoint-token attempt)
                :endpoint-generation
                (e-board-delivery-attempt-composite-generation attempt))))
        (should (eq (e-board-pickup-state pickup) 'consumed))
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (should-not
         (gethash "participant"
                  (e-board-registry-board-participants board)))))))

(ert-deftest e-board-runtime-test-remove-participant-discards-fenced-queue-head ()
  "Removal gets an exact harness discard receipt for an accepted queued input."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create
                     :backend (e-backend-create
                               :name "held" :start (lambda (&rest _) nil))))
           (source-board (e-board-registry-board-source-board board))
           scheduled request delivery-id pickup attachment active-turn)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach
             board harness "session" :participant-id "participant"))
      (e-harness-test-prompt-async harness "session" "running")
      (setq active-turn (gethash "session" (e-harness-active-turns harness)))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq scheduled
                         (append scheduled
                                 (list (lambda ()
                                         (apply function arguments))))))))
        (let ((publication
               (e-board-post-input
                source-board :id "queued" :to "participant" :mode 'queue
                :content "queued")))
          (e-board-drain-input-classifications source-board)
          (setq delivery-id (car (e-board-publication-pickup-ids publication))
                scheduled nil))
        (e-board-runtime--deliver-pickups board (list delivery-id))
        (setq pickup (e-board-pickup source-board delivery-id)
              request (e-board-runtime-remove-participant-start
                       board "participant" "owner"))
        (should (eq (e-board-pickup-state pickup) 'accepted))
        (should (= (length (e-harness-queued-prompts harness "session")) 1))
        (while scheduled
          (funcall (pop scheduled)))
        (should (eq (e-board-pickup-state pickup) 'cancelling))
        (should (eq (plist-get (e-request-lifecycle-progress request) :phase)
                    'awaiting-active-turn))
        (plist-put active-turn :status 'finished)
        (e-board-runtime--handle-harness-event
         attachment
         (e-events-make
          :type 'turn-finished :session-id "session"
          :turn-id (plist-get active-turn :id) :payload nil))
        (let ((steps 0))
          (while (and scheduled
                      (not (eq (e-board-pickup-state pickup) 'cancelled))
                      (< steps 64))
            (cl-incf steps)
            (funcall (pop scheduled)))
          (should (< steps 64)))
        (should (eq (e-board-pickup-state pickup) 'cancelled))
        (should-not (e-harness-queued-prompts harness "session"))
        (while (and scheduled
                    (not (eq (e-request-lifecycle-state request) 'finished)))
          (funcall (pop scheduled)))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (should-not
         (gethash "participant"
                  (e-board-registry-board-participants board)))))))

(ert-deftest e-board-runtime-test-remove-participant-awaits-active-turn ()
  "Removal resumes from a terminal turn event instead of polling the harness."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           scheduled request attachment entry)
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach
             board harness "session" :participant-id "participant")
            entry '(:id "turn" :status running))
      (puthash "session" entry (e-harness-active-turns harness))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq scheduled
                         (append scheduled
                                 (list (lambda ()
                                         (apply function arguments))))))))
        (setq request
              (e-board-runtime-remove-participant-start
               board "participant" "owner"))
        (funcall (pop scheduled))
        (funcall (pop scheduled))
        (should-not scheduled)
        (should (eq (plist-get (e-request-lifecycle-progress request) :phase)
                    'awaiting-active-turn))
        (plist-put entry :status 'finished)
        (e-board-runtime--handle-harness-event
         attachment
         (e-events-make
          :type 'turn-finished :session-id "session" :turn-id "turn"
          :payload nil))
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (should-not
         (gethash "participant"
                  (e-board-registry-board-participants board)))))))

(ert-deftest e-board-runtime-test-move-participant-reconciles-source-fifo ()
  "A move tombstones source input and publishes one fresh destination attachment."
  (e-board-runtime-test--with-empty-state
    (let* ((source
            (e-board-registry-create :id "source" :principal "owner"))
           (destination
            (e-board-registry-create :id "destination" :principal "owner"))
           (source-core (e-board-registry-board-source-board source))
           (destination-core
            (e-board-registry-board-source-board destination))
           (harness (e-harness-create))
           deliveries scheduled old request source-delivery-id)
      (e-harness-create-session harness :id "session")
      (setq old
            (e-board-runtime-attach
             source harness "session" :participant-id "participant"
             :delivery-function
             (lambda (_attachment _pickup message)
               (push (e-board-message-content message) deliveries))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq scheduled
                         (append scheduled
                                 (list (lambda ()
                                         (apply function arguments))))))))
        (let ((publication
               (e-board-post-input
                source-core :id "source-input" :to "participant"
                :content "source history")))
          (e-board-drain-input-classifications source-core)
          (setq source-delivery-id
                (car (e-board-publication-pickup-ids publication))
                scheduled nil))
        (setq request
              (e-board-runtime-move-participant-start
               source "participant" destination "owner"))
        (funcall (pop scheduled))
        (should (eq (e-board-runtime-attachment-state old) 'detaching))
        (funcall (pop scheduled))
        (should (eq (e-board-pickup-state
                     (e-board-pickup source-core source-delivery-id))
                    'cancelled))
        (funcall (pop scheduled))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (let* ((new (e-request-lifecycle-terminal-payload request))
               (moved (e-board-runtime-attachment-participant new)))
          (should-not (e-board-runtime--current-attachment-p old))
          (should (e-board-runtime--current-attachment-p new))
          (should (eq (e-board-runtime-attachment-board new) destination))
          (should (equal (e-board-registry-participant-id moved) "participant"))
          (should-not
           (gethash "participant"
                    (e-board-registry-board-participants source)))
          (should (eq moved
                      (gethash "participant"
                               (e-board-registry-board-participants destination))))
          (should (equal (mapcar #'e-board-message-content
                                 (e-board-messages source-core))
                         '("source history")))
          (let ((publication
                 (e-board-post-input
                  destination-core :id "destination-input" :to "participant"
                  :content "destination work")))
            (e-board-drain-input-classifications destination-core)
            (e-board-runtime--deliver-pickups
             destination (e-board-publication-pickup-ids publication))
            (should (equal deliveries '("destination work")))))))))

(ert-deftest e-board-runtime-test-move-revalidates-destination-after-wait ()
  "A destination claimed during source quiescence leaves the source attached."
  (e-board-runtime-test--with-empty-state
    (let* ((source
            (e-board-registry-create :id "source" :principal "owner"))
           (destination
            (e-board-registry-create :id "destination" :principal "owner"))
           (harness (e-harness-create))
           scheduled old request entry)
      (e-harness-create-session harness :id "session")
      (setq old
            (e-board-runtime-attach
             source harness "session" :participant-id "participant")
            entry '(:id "turn" :status running))
      (puthash "session" entry (e-harness-active-turns harness))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq scheduled
                         (append scheduled
                                 (list (lambda ()
                                         (apply function arguments))))))))
        (setq request
              (e-board-runtime-move-participant-start
               source "participant" destination "owner"))
        (funcall (pop scheduled))
        (funcall (pop scheduled))
        (should (eq (plist-get (e-request-lifecycle-progress request) :phase)
                    'awaiting-active-turn))
        (e-board-registry-add-participant destination :id "participant")
        (plist-put entry :status 'finished)
        (e-board-runtime--handle-harness-event
         old
         (e-events-make
          :type 'turn-finished :session-id "session" :turn-id "turn"
          :payload nil))
        (funcall (pop scheduled))
        (should (eq (e-request-lifecycle-state request) 'failed))
        (should (eq (car (e-request-lifecycle-terminal-payload request))
                    'e-board-registry-id-conflict))
        (should (eq (e-board-runtime-attachment-state old) 'stale))
        (should (e-board-runtime--current-attachment-p old))
        (should (e-board-registry-participant source "participant"))))))

(ert-deftest e-board-runtime-test-move-awaits-source-accepted-receipt ()
  "A move retains its source binding until the accepted item settles there."
  (e-board-runtime-test--with-empty-state
    (let* ((source
            (e-board-registry-create :id "source" :principal "owner"))
           (destination
            (e-board-registry-create :id "destination" :principal "owner"))
           (source-core (e-board-registry-board-source-board source))
           (harness (e-harness-create))
           scheduled old request delivery-id pickup attempt)
      (e-harness-create-session harness :id "session")
      (setq old
            (e-board-runtime-attach
             source harness "session" :participant-id "participant"
             :delivery-function
             (lambda (&rest _arguments) '(:accepted receipt))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq scheduled
                         (append scheduled
                                 (list (lambda ()
                                         (apply function arguments))))))))
        (let ((publication
               (e-board-post-input
                source-core :id "accepted" :to "participant"
                :content "accepted")))
          (e-board-drain-input-classifications source-core)
          (setq delivery-id (car (e-board-publication-pickup-ids publication))
                scheduled nil))
        (e-board-runtime--deliver-pickups source (list delivery-id))
        (setq pickup (e-board-pickup source-core delivery-id)
              attempt (e-board-pickup-attempt pickup)
              request
              (e-board-runtime-move-participant-start
               source "participant" destination "owner"))
        (funcall (pop scheduled))
        (funcall (pop scheduled))
        (funcall (pop scheduled))
        (should-not scheduled)
        (should (eq (e-board-pickup-state pickup) 'cancelling))
        (should (eq (e-request-lifecycle-state request) 'progress))
        (should (e-board-runtime--current-attachment-p old))
        (e-board-runtime--handle-harness-event
         old
         (e-events-make
          :type 'input-consumed :session-id "session" :turn-id "turn"
          :payload
          (list :delivery-id delivery-id
                :endpoint-token
                (e-board-delivery-attempt-endpoint-token attempt)
                :endpoint-generation
                (e-board-delivery-attempt-composite-generation attempt))))
        (funcall (pop scheduled))
        (should (eq (e-board-pickup-state pickup) 'consumed))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (should
         (e-board-runtime--current-attachment-p
          (e-request-lifecycle-terminal-payload request)))
        (should-not
         (gethash "participant"
                  (e-board-registry-board-participants source)))))))

(ert-deftest e-board-runtime-test-rebind-preserves-participant-and-fences-old-session ()
  "A participant rebind retains its logical identity and uses the new endpoint."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           (deliveries nil)
           scheduled)
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (let ((old-attachment
             (e-board-runtime-attach board old-harness "old" :participant-id "participant")))
        (e-board-runtime-post-input board :id "before-rebind" :to "participant"
                                    :content "preserved pickup")
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications
                           (e-board-registry-board-source-board board))))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq scheduled
                           (append scheduled
                                   (list (lambda ()
                                           (apply function arguments))))))))
          (let ((request
                 (e-board-runtime-rebind-start
                  board "participant" new-harness "new" "owner"
                  :delivery-function
                  (lambda (_attachment _pickup message)
                    (push (e-board-message-content message) deliveries)))))
            (should (eq (e-request-lifecycle-state request) 'started))
            (funcall (pop scheduled))
            (should (eq (e-board-runtime-attachment-state old-attachment)
                        'detaching))
            (funcall (pop scheduled))
            (should (eq (e-request-lifecycle-state request) 'finished))
            (let ((attachment (e-request-lifecycle-terminal-payload request)))
              (should (eq (e-board-runtime-attachment-harness attachment)
                          new-harness))
              (should-not (e-board-runtime--current-attachment-p old-attachment))
              (should (e-board-runtime--current-attachment-p attachment))
              (should-not
               (gethash (e-board-runtime--session-key old-harness "old")
                        e-board-runtime--session-attachments))
              (let ((source-board (e-board-registry-board-source-board board)))
                (puthash "stale-work"
                         (list :attachment old-attachment :turn-id "turn"
                               :payload 'stale :source-key '(participant 1 1))
                         e-board-runtime--work-activity-mailboxes)
                (e-board-runtime--enqueue-activity-flush "stale-work")
                (let ((message-count (length (e-board-messages source-board))))
                  (e-board-runtime--drain-activity-mailboxes)
                  (should (= (length (e-board-messages source-board))
                             message-count))))
              (e-board-runtime-post-input
               board :id "after-rebind" :to "participant"
               :content "new endpoint")
              (e-board-runtime--drain-input-routing
               board (lambda () (e-board-drain-input-classifications
                                  (e-board-registry-board-source-board board))))
              (e-board-runtime--drain-pickups)
              (should (equal (nreverse deliveries)
                             '("preserved pickup" "new endpoint"))))))))))

(ert-deftest e-board-runtime-test-rebind-revalidates-target-before-detach ()
  "A replacement claimed before commit leaves the old attachment active."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           scheduled)
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (let ((old (e-board-runtime-attach
                  board old-harness "old" :participant-id "participant")))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq scheduled
                           (append scheduled
                                   (list (lambda ()
                                           (apply function arguments))))))))
          (let ((request (e-board-runtime-rebind-start
                          board "participant" new-harness "new" "owner")))
            (e-board-runtime-attach
             board new-harness "new" :participant-id "claim")
            (funcall (pop scheduled))
            (should (eq (e-request-lifecycle-state request) 'failed))
            (should (eq (car (e-request-lifecycle-terminal-payload request))
                        'e-board-runtime-session-busy))
            (should (eq (e-board-runtime-attachment-state old) 'active))
            (should (e-board-runtime--current-attachment-p old))
            (should-not (e-board-runtime-attachment-reconciliation old))))))))

(ert-deftest e-board-runtime-test-rebind-awaits-old-accepted-receipt ()
  "A replacement endpoint waits for the old accepted pickup's exact receipt."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           deliveries scheduled old-attachment)
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (setq old-attachment
            (e-board-runtime-attach
             board old-harness "old" :participant-id "participant"
             :delivery-function (lambda (&rest _arguments)
                                  '(:accepted receipt))))
      (let* ((first (e-board-runtime-post-input board :id "first" :to "participant"
                                                :content "first"))
             (second (e-board-runtime-post-input board :id "second" :to "participant"
                                                 :content "second"))
             (source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (e-board-runtime--drain-pickups)
        (let ((first-id (car (e-board-publication-pickup-ids first)))
              (second-id (car (e-board-publication-pickup-ids second))))
          (should (eq (e-board-pickup-state (e-board-pickup source-board first-id))
                      'accepted))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest arguments)
                       (setq scheduled
                             (append scheduled
                                     (list (lambda ()
                                             (apply function arguments))))))))
            (let ((request
                   (e-board-runtime-rebind-start
                    board "participant" new-harness "new" "owner"
                    :delivery-function
                    (lambda (_attachment _pickup message)
                      (push (e-board-message-content message) deliveries)))))
              (funcall (pop scheduled))
              (funcall (pop scheduled))
              (funcall (pop scheduled))
              (let* ((pickup (e-board-pickup source-board first-id))
                     (attempt (e-board-pickup-attempt pickup)))
                (should (eq (e-board-pickup-state pickup) 'cancelling))
                (should (eq (e-request-lifecycle-state request) 'progress))
                (e-board-runtime--handle-harness-event
                 old-attachment
                 (e-events-make
                  :type 'input-consumed :session-id "old" :turn-id "turn"
                  :payload
                  (list :delivery-id first-id
                        :endpoint-token
                        (e-board-delivery-attempt-endpoint-token attempt)
                        :endpoint-generation
                        (e-board-delivery-attempt-composite-generation attempt)))))
              (funcall (pop scheduled))
              (funcall (pop scheduled))
              (should (eq (e-request-lifecycle-state request) 'finished))
              (funcall (pop scheduled))
              (should (eq (e-board-pickup-state
                           (e-board-pickup source-board first-id))
                          'consumed))
              (should (eq (e-board-pickup-state
                           (e-board-pickup source-board second-id))
                          'consumed))
              (should (equal deliveries '("second"))))))))))

(ert-deftest e-board-runtime-test-busy-session-rejects-before-participant-creation ()
  "A session cannot acquire a second participant through a failed attach."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "first")
      (should-error (e-board-runtime-attach board harness "session" :participant-id "second")
                    :type 'e-board-runtime-session-busy)
      (should-not (gethash "second" (e-board-registry-board-participants board))))))

(ert-deftest e-board-runtime-test-default-queue-delivery-enters-active-follow-up-queue ()
  "Default queue delivery enters the inbox behind an active turn."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create
                     :backend (e-backend-create
                               :name "held" :start (lambda (&rest _) nil))))
           attachment)
      (e-harness-create-session harness :id "session")
      (setq attachment (e-board-runtime-attach
                        board harness "session" :participant-id "participant"))
      (e-harness-test-prompt-async harness "session" "running")
      (let* ((publication (e-board-runtime-post-input board :to "participant" :mode 'queue
                                                      :content "queued input"))
             (pickup-id nil))
        (should-not (e-harness-queued-prompts harness "session"))
        (e-board-runtime--drain-input-routing
         board
         (lambda ()
           (e-board-drain-input-classifications
            (e-board-registry-board-source-board board))))
        (setq pickup-id
              (car (e-board-message-pickup-ids
                    (e-board-publication-message publication))))
        (setf (e-board-message-content (e-board-publication-message publication))
              "mutated after routing")
        (e-board-runtime--drain-pickups)
        (let* ((queued (car (e-harness-queued-prompts harness "session")))
               (metadata (plist-get queued :metadata)))
          (should (equal (plist-get queued :prompt) "queued input"))
          (should (eq (plist-get metadata :board-input-mode) 'queue))
          (should (equal (plist-get metadata :board-message-id)
                         (e-board-message-id
                          (e-board-publication-message publication))))
          (should (equal (plist-get metadata :board-event-seq-range)
                         (list (e-board-message-seq
                                (e-board-publication-message publication))
                               (e-board-message-seq
                                (e-board-publication-message publication)))))
          (should (equal (plist-get metadata :board-endpoint-generation)
                         '(nil nil))))
        (should (eq (e-board-pickup-state
                     (e-board-pickup (e-board-registry-board-source-board board)
                                     pickup-id))
                    'accepted))
        (e-board-runtime--handle-harness-event
         attachment
         (e-events-make :type 'input-consumed :session-id "session" :turn-id "turn"
                        :payload (list :delivery-id pickup-id
                                       :endpoint-generation '(9 9))))
        (should (eq (e-board-pickup-state
                     (e-board-pickup (e-board-registry-board-source-board board)
                                     pickup-id))
                    'accepted))
        (let ((attempt
               (e-board-pickup-attempt
                (e-board-pickup (e-board-registry-board-source-board board)
                                pickup-id))))
          (e-board-runtime--handle-harness-event
           attachment
           (e-events-make
            :type 'input-consumed :session-id "session" :turn-id "turn"
            :payload
            (list :delivery-id pickup-id
                  :endpoint-token
                  (copy-tree (e-board-delivery-attempt-endpoint-token attempt))
                  :endpoint-generation
                  (copy-tree
                   (e-board-delivery-attempt-composite-generation attempt))))))
        (should (eq (e-board-pickup-state
                     (e-board-pickup (e-board-registry-board-source-board board)
                                     pickup-id))
                    'consumed))
        (should (plist-get (e-harness-state harness "session") :active-turn))))))

(ert-deftest e-board-runtime-test-busy-delivery-retries-after-turn-finished ()
  "A retryable busy adapter result stays ready until the terminal wake edge."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attempts 0))
      (e-harness-create-session harness :id "session")
      (let ((attachment
             (e-board-runtime-attach
              board harness "session" :participant-id "participant"
              :delivery-function
              (lambda (&rest _arguments)
                (cl-incf attempts)
                (when (= attempts 1)
                  (signal 'e-board-runtime-session-busy '("session")))))))
        (let* ((publication (e-board-runtime-post-input
                             board :id "input" :to "participant" :content "queued"))
               (source-board (e-board-registry-board-source-board board))
               delivery-id)
          (e-board-runtime--drain-input-routing
           board (lambda () (e-board-drain-input-classifications source-board)))
          (setq delivery-id (car (e-board-publication-pickup-ids publication)))
          (e-board-runtime--drain-pickups)
          (should (= attempts 1))
          (should (eq (e-board-pickup-state (e-board-pickup source-board delivery-id))
                      'ready))
          (e-board-runtime--handle-harness-event
           attachment (e-events-make :type 'turn-finished :session-id "session"
                                     :turn-id "finished"))
          (e-board-runtime--drain-pickups)
          (should (= attempts 2))
          (should (eq (e-board-pickup-state (e-board-pickup source-board delivery-id))
                      'consumed)))))))

(ert-deftest e-board-runtime-test-busy-delivery-retries-after-failed-or-cancelled-turn ()
  "Every terminal idle edge wakes a retryable participant-local FIFO head."
  (dolist (terminal-type '(turn-failed turn-cancelled))
    (e-board-runtime-test--with-empty-state
      (let* ((board (e-board-registry-create :id "board"))
             (harness (e-harness-create))
             (attempts 0))
        (e-harness-create-session harness :id "session")
        (let ((attachment
               (e-board-runtime-attach
                board harness "session" :participant-id "participant"
                :delivery-function
                (lambda (&rest _arguments)
                  (cl-incf attempts)
                  (when (= attempts 1)
                    (signal 'e-board-runtime-session-busy '("session")))))))
          (let* ((publication
                  (e-board-runtime-post-input
                   board :id "input" :to "participant" :content "queued"))
                 (source-board (e-board-registry-board-source-board board)))
            (e-board-runtime--drain-input-routing
             board (lambda () (e-board-drain-input-classifications source-board)))
            (let ((delivery-id (car (e-board-publication-pickup-ids publication))))
              (e-board-runtime--drain-pickups)
              (e-board-runtime--handle-harness-event
               attachment
               (e-events-make :type terminal-type :session-id "session"
                              :turn-id "terminal"))
              (e-board-runtime--drain-pickups)
              (should (= attempts 2))
              (should (eq (e-board-pickup-state
                           (e-board-pickup source-board delivery-id))
                          'consumed)))))))))

(ert-deftest e-board-runtime-test-signalled-delivery-retries-on-later-drain ()
  "A proven-uncommitted signal retains scheduler ownership until success."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (attempts 0))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach
       board harness "session" :participant-id "participant"
       :delivery-function
       (lambda (&rest _arguments)
         (cl-incf attempts)
         (when (= attempts 1) (error "temporary"))))
      (let* ((publication
              (e-board-runtime-post-input
               board :id "input" :to "participant" :content "retry"))
             (source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (let ((delivery-id (car (e-board-publication-pickup-ids publication))))
          (e-board-runtime--drain-pickups)
          (should (= attempts 1))
          (should (eq (e-board-pickup-state
                       (e-board-pickup source-board delivery-id))
                      'ready))
          (should (= (hash-table-count e-board-runtime--pending-pickup-set) 1))
          (e-board-runtime--drain-pickups)
          (should (= attempts 2))
          (should (eq (e-board-pickup-state
                       (e-board-pickup source-board delivery-id))
                      'consumed)))))))

(ert-deftest e-board-runtime-test-signalled-delivery-exhausts-and-other-lane-continues ()
  "Repeated signals fail visibly while an independent participant still runs."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (first-harness (e-harness-create))
           (second-harness (e-harness-create))
           (first-attempts 0)
           (second-attempts 0))
      (e-harness-create-session first-harness :id "first-session")
      (e-harness-create-session second-harness :id "second-session")
      (e-board-runtime-attach
       board first-harness "first-session" :participant-id "first"
       :delivery-function
       (lambda (&rest _arguments)
         (cl-incf first-attempts)
         (error "always")))
      (e-board-runtime-attach
       board second-harness "second-session" :participant-id "second"
       :delivery-function
       (lambda (&rest _arguments) (cl-incf second-attempts)))
      (let* ((first
              (e-board-runtime-post-input
               board :id "first-input" :to "first" :content "fail"))
             (second
              (e-board-runtime-post-input
               board :id "second-input" :to "second" :content "continue"))
             (source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (e-board-runtime--drain-pickups)
        (should (= first-attempts 1))
        (should (= second-attempts 1))
        (should (eq (e-board-pickup-state
                     (e-board-pickup
                      source-board (car (e-board-publication-pickup-ids second))))
                    'consumed))
        (e-board-runtime--drain-pickups)
        (e-board-runtime--drain-pickups)
        (let ((pickup
               (e-board-pickup
                source-board (car (e-board-publication-pickup-ids first)))))
          (should (= first-attempts e-board-runtime-pickup-retry-limit))
          (should (eq (e-board-pickup-state pickup) 'failed))
          (should (eq (e-board-delivery-attempt-reason
                       (e-board-pickup-attempt pickup))
                      'delivery-retry-exhausted)))))))

(ert-deftest e-board-runtime-test-terminal-turn-events-close-open-activity ()
  "Failed and cancelled turns publish terminal activity without an output."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (participant "participant"))
      (e-harness-create-session harness :id "session")
      (let* ((attachment (e-board-runtime-attach
                          board harness "session" :participant-id participant))
             (source-board (e-board-registry-board-source-board board)))
        (e-board-post-activity
         source-board :id "progress" :author "participant:participant"
         :subject-participant-id participant :source-turn-id "failed-turn"
         :activity-kind 'work-progress :source-activity-key '(participant 0 1))
        (e-board-runtime--handle-harness-event
         attachment (e-events-make :type 'turn-failed :session-id "session"
                                   :turn-id "failed-turn"
                                   :activity-entry-id "failed-event"))
        (should-not (e-board-open-activity source-board participant "failed-turn"))
        (let ((activity (car (last (e-board-messages source-board)))))
          (should (eq (e-board-message-activity-kind activity) 'turn-failed))
          (should (equal (e-board-message-attributes activity)
                         '(:source-event-id "failed-event"))))
        (e-board-runtime--handle-harness-event
         attachment (e-events-make :type 'turn-cancelled :session-id "session"
                                   :turn-id "cancelled-turn"
                                   :activity-entry-id "cancelled-event"))
        (let ((activity (car (last (e-board-messages source-board)))))
          (should (eq (e-board-message-activity-kind activity) 'turn-cancelled))
          (should (equal (e-board-message-attributes activity)
                         '(:source-event-id "cancelled-event"))))))))

(ert-deftest e-board-runtime-test-provider-active-turn-publishes-one-bounded-summary ()
  "Provider event edges produce one structured summary without transcript scans."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (participant "participant"))
      (e-harness-create-session harness :id "session")
      (let* ((attachment (e-board-runtime-attach
                          board harness "session" :participant-id participant))
             (source-board (e-board-registry-board-source-board board)))
        (dolist (type '(provider-request-started tool-started action-started))
          (e-board-runtime--handle-harness-event
           attachment (e-events-make :type type :session-id "session" :turn-id "turn"
                                     :created-at 100)))
        (e-board-runtime--handle-harness-event
         attachment (e-events-make :type 'turn-finished :session-id "session" :turn-id "turn"
                                   :created-at 102.5
                                   :activity-entry-id "finish-activity"))
        (let ((summary (car (last (e-board-messages source-board)))))
          (should (eq (e-board-message-activity-kind summary) 'turn-summary))
          (should (equal (e-board-message-attributes summary)
                         '(:status finished :duration-seconds 2.5
                           :tool-count 1 :action-count 1
                           :source-event-id "finish-activity"))))
        (let ((message-count (length (e-board-messages source-board))))
          (e-board-runtime--handle-harness-event
           attachment (e-events-make :type 'turn-finished :session-id "session"
                                     :turn-id "no-provider"))
          (should (= (length (e-board-messages source-board)) message-count)))
        (e-board-runtime--handle-harness-event
         attachment (e-events-make :type 'provider-request-started :session-id "session"
                                   :turn-id "cancelled" :created-at 200))
        (e-board-runtime--handle-harness-event
         attachment (e-events-make :type 'turn-cancelled :session-id "session"
                                   :turn-id "cancelled" :created-at 203
                                   :activity-entry-id "cancel-activity"))
        (let ((summary (car (last (e-board-messages source-board)))))
          (should (eq (e-board-message-activity-kind summary) 'turn-summary))
          (should (equal (e-board-message-attributes summary)
                         '(:status turn-cancelled :duration-seconds 3
                           :tool-count 0 :action-count 0
                           :source-event-id "cancel-activity"))))))))

(ert-deftest e-board-runtime-test-harness-lifecycle-activity-is-bounded-and-provenanced ()
  "Visible lifecycle events expose a bounded payload and durable identity."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (participant "participant"))
      (e-harness-create-session harness :id "session")
      (let* ((attachment (e-board-runtime-attach
                          board harness "session" :participant-id participant))
             (source-board (e-board-registry-board-source-board board)))
        (let ((event
               (e-events-make :type 'provider-request-started :session-id "session"
                              :turn-id "turn" :payload '(:secret "not-board-content")
                              :activity-entry-id "provider-event"
                              :board-activity-sequence 17)))
          (e-harness-activity-emit
           (e-board-runtime-attachment-harness attachment) event)
          ;; A durable event may be replayed after an interrupted publication.
          (e-harness-activity-emit
           (e-board-runtime-attachment-harness attachment) event))
        (let ((activity (car (last (e-board-messages source-board)))))
          (should (= (length (e-board-messages source-board)) 1))
          (should (eq (e-board-message-kind activity) 'activity))
          (should (eq (e-board-message-activity-kind activity)
                      'provider-request-started))
          (should (equal (e-board-message-tags activity) '(main)))
          (should-not (e-board-message-content activity))
          (should (equal (e-board-message-attributes activity)
                         '(:caused-by-tool-calls []
                           :source-event-id "provider-event")))
          (should-not (plist-member (e-board-message-attributes activity)
                                    :secret))
          (should (equal (e-board-message-source-activity-key activity)
                         '("participant" 1 34))))))))

(ert-deftest e-board-runtime-test-retrying-activity-retains-bounded-error ()
  "Retry activity publishes its redacted error and retry schedule."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (participant "participant"))
      (e-harness-create-session harness :id "session")
      (let* ((attachment (e-board-runtime-attach
                          board harness "session" :participant-id participant))
             (source-board (e-board-registry-board-source-board board)))
        (e-harness-activity-emit
         (e-board-runtime-attachment-harness attachment)
         (e-events-make
          :type 'turn-retrying :session-id "session" :turn-id "turn"
          :payload '(:error "503 upstream; token=secret-value"
                     :details (:status 503 :authorization "Bearer secret")
                     :attempt 2 :backoff-seconds 4.0)
          :activity-entry-id "retry-event"))
        (let ((activity (car (e-board-messages source-board))))
          (should (eq (e-board-message-activity-kind activity) 'turn-retrying))
          (should
           (equal
            (e-board-message-attributes activity)
            '(:error "503 upstream; token=[REDACTED]"
              :details (:status 503 :authorization "[REDACTED]")
              :attempt 2 :backoff-seconds 4.0
              :source-event-id "retry-event"))))))))

(ert-deftest e-board-runtime-test-reasoning-deltas-coalesce-on-board ()
  "Latest reasoning is board-visible without appending every high-rate delta."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           scheduled)
      (e-harness-create-session harness :id "session")
      (let ((attachment
             (e-board-runtime-attach
              board harness "session" :participant-id "participant")))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (push (lambda () (apply function arguments)) scheduled))))
          (dolist (content '("first" "latest"))
            (e-board-runtime--handle-harness-event
             attachment
             (e-events-make :type 'reasoning-delta :session-id "session"
                            :turn-id "turn"
                            :payload (list :type 'reasoning-delta
                                           :stream-kind 'summary
                                           :content content))))
          (should (= (hash-table-count e-board-runtime--pending-activity-set) 1))
          (e-board-runtime--drain-activity-mailboxes)
          (let ((message
                 (car (e-board-messages
                       (e-board-registry-board-source-board board)))))
            (should (eq (e-board-message-activity-kind message)
                        'reasoning-delta))
            (should (equal (e-board-message-tags message) '(main)))
            (should (equal (e-board-message-content message) "latest"))))))))

(ert-deftest e-board-runtime-test-reasoning-follows-durable-lifecycle-watermark ()
  "Reasoning activity remains publishable after durable lifecycle edges."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (let ((attachment
             (e-board-runtime-attach
              board harness "session" :participant-id "participant")))
        (dolist (event
                 (list
                  (e-events-make
                   :type 'turn-started :session-id "session" :turn-id "turn"
                   :board-activity-sequence 1)
                  (e-events-make
                   :type 'provider-request-started
                   :session-id "session" :turn-id "turn"
                   :board-activity-sequence 2)
                  (e-events-make
                   :type 'reasoning-delta :session-id "session" :turn-id "turn"
                   :payload '(:type reasoning-delta :stream-kind summary
                              :content "visible progress")
                   :board-activity-sequence 3)))
          (e-board-runtime--handle-harness-event attachment event))
        (e-board-runtime--drain-activity-mailboxes)
        (let* ((source (e-board-registry-board-source-board board))
               (messages (e-board-messages source)))
          (should (equal (mapcar #'e-board-message-activity-kind messages)
                         '(turn-started provider-request-started
                           reasoning-delta)))
          (should (equal (mapcar #'e-board-message-source-activity-key messages)
                         '(("participant" 1 2)
                           ("participant" 1 4)
                           ("participant" 1 6))))
          (should (equal (e-board-message-content (car (last messages)))
                         "visible progress")))))))

(ert-deftest e-board-runtime-test-authorized-exact-input-checks-requester-before-post ()
  "An explicit requester cannot create an unauthorized exact board input."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner")))
      (e-board-registry-authorize-principal board "owner" "target-owner" 'member)
      (e-board-registry-authorize-principal board "owner" "member" 'member)
      (let* ((target (e-board-registry-add-participant
                      board :id "target" :principal "target-owner"))
             (_owner-client (e-board-registry-attach-client
                             board :id "owner-client" :principal "owner"))
             (_member-client (e-board-registry-attach-client
                              board :id "member-client" :principal "member"))
             (owner-context
              (e-board-registry-client-requester-context board "owner-client"))
             (member-context
              (e-board-registry-client-requester-context board "member-client"))
             (source-board (e-board-registry-board-source-board board)))
        (should-error
         (e-board-runtime-post-input board :id "bare" :to target :requester "owner"
                                     :content "no")
         :type 'e-board-registry-authorization-denied)
        (should-not (e-board-messages source-board))
        (should-error
         (e-board-runtime-post-input board :id "denied" :to target
                                     :requester member-context
                                     :content "no")
         :type 'e-board-registry-authorization-denied)
        (should-not (e-board-messages source-board))
        (e-board-runtime-post-input board :id "allowed" :to target
                                    :requester owner-context
                                    :content "yes")
        (should (equal (e-board-message-content (car (e-board-messages source-board)))
                       "yes"))))))

(ert-deftest e-board-runtime-test-explicit-tagged-ingress-requires-live-requester ()
  "Tagged ingress authenticates its client generation before board append."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (_client (e-board-registry-attach-client
                     board :id "client" :principal "owner"))
           (context (e-board-registry-client-requester-context board "client"))
           (source-board (e-board-registry-board-source-board board)))
      (e-board-runtime-post-input board :id "allowed" :tags '(main)
                                  :requester context :content "yes")
      (should (equal (e-board-message-requester-actor
                      (car (e-board-messages source-board)))
                     '(client "client" 1)))
      (e-board-registry-detach-client board "client")
      (should-error
       (e-board-runtime-post-input board :id "denied" :tags '(main)
                                   :requester context :content "no")
       :type 'e-board-registry-authorization-denied)
      (should (= (length (e-board-messages source-board)) 1)))))

(ert-deftest e-board-runtime-test-ingress-rejects-nil-and-fences-client-generation ()
  "No retained input can originate from omitted or stale client identity."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (_client (e-board-registry-attach-client
                     board :id "client" :principal "owner"))
           (old (e-board-registry-client-requester-context board "client"))
           (source (e-board-registry-board-source-board board)))
      (should-error
       (funcall e-board-runtime-test--production-post-input
                board :id "missing" :content "no")
       :type 'e-board-registry-authorization-denied)
      (e-board-registry-detach-client board "client")
      (e-board-registry-attach-client board :id "client" :principal "owner")
      (should-error
       (funcall e-board-runtime-test--production-post-input
                board :id "stale" :requester old :content "no")
       :type 'e-board-registry-authorization-denied)
      (should-not (e-board-messages source)))))

(ert-deftest e-board-runtime-test-participant-originated-post-derives-current-actor ()
  "The participant ingress never accepts caller-supplied or nil authority."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (let* ((attachment
              (e-board-runtime-attach
               board harness "session" :participant-id "participant"))
             (publication
              (e-board-runtime-post-participant-input
               attachment :id "self" :to "participant" :content "work"))
             (message (e-board-publication-message publication)))
        (should (equal (e-board-message-requester-actor message)
                       '(participant "participant")))
        (setf (e-board-runtime-attachment-state attachment) 'retired)
        (should-error
         (e-board-runtime-post-participant-input attachment :content "late")
         :type 'e-board-runtime-error)))))

(ert-deftest e-board-runtime-test-uncertain-delivery-does-not-retry-old-pickup ()
  "An adapter can tombstone an ambiguous attempt and advance the FIFO."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (deliveries nil)
           (delivery
            (lambda (_attachment _pickup message)
              (push (e-board-message-content message) deliveries)
              (when (equal (e-board-message-content message) "first")
                '(:uncertain lost-ack)))))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session"
                              :participant-id "participant" :delivery-function delivery)
      (let* ((first (e-board-runtime-post-input board :id "first" :to "participant"
                                                :content "first"))
             (second (e-board-runtime-post-input board :id "second" :to "participant"
                                                 :content "second"))
             (source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (e-board-runtime--drain-pickups)
        (let ((first-id (car (e-board-publication-pickup-ids first)))
              (second-id (car (e-board-publication-pickup-ids second))))
          (should (equal (nreverse deliveries) '("first" "second")))
          (should (eq (e-board-pickup-state (e-board-pickup source-board first-id))
                      'uncertain))
          (should (eq (e-board-pickup-state (e-board-pickup source-board second-id))
                      'consumed)))))))

(ert-deftest e-board-runtime-test-discarded-delivery-releases-fifo-successor ()
  "An adapter can prove a delivery did not commit without marking it consumed."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (delivery
            (lambda (_attachment _pickup message)
              (when (equal (e-board-message-content message) "first")
                '(:discarded rejected-by-endpoint)))))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session"
                              :participant-id "participant" :delivery-function delivery)
      (let* ((first (e-board-runtime-post-input board :id "first" :to "participant"
                                                :content "first"))
             (second (e-board-runtime-post-input board :id "second" :to "participant"
                                                 :content "second"))
             (source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (e-board-runtime--drain-pickups)
        (let ((first-id (car (e-board-publication-pickup-ids first)))
              (second-id (car (e-board-publication-pickup-ids second))))
          (should (eq (e-board-pickup-state (e-board-pickup source-board first-id))
                      'discarded))
          (should (eq (e-board-pickup-state (e-board-pickup source-board second-id))
                      'consumed)))))))

(ert-deftest e-board-runtime-test-failed-delivery-releases-fifo-successor ()
  "A permanent adapter rejection records failure and continues the FIFO."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (delivery
            (lambda (_attachment _pickup message)
              (when (equal (e-board-message-content message) "first")
                '(:failed permanent-rejection)))))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session"
                              :participant-id "participant" :delivery-function delivery)
      (let* ((first (e-board-runtime-post-input board :id "first" :to "participant"
                                                :content "first"))
             (second (e-board-runtime-post-input board :id "second" :to "participant"
                                                 :content "second"))
             (source-board (e-board-registry-board-source-board board)))
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications source-board)))
        (e-board-runtime--drain-pickups)
        (let ((first-id (car (e-board-publication-pickup-ids first)))
              (second-id (car (e-board-publication-pickup-ids second))))
          (should (eq (e-board-pickup-state (e-board-pickup source-board first-id))
                      'failed))
          (should (eq (e-board-pickup-state (e-board-pickup source-board second-id))
                      'consumed)))))))

(ert-deftest e-board-runtime-test-session-reset-discards-accepted-queue-pickup ()
  "Resetting a queued harness item releases its accepted board pickup."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create
                     :backend (e-backend-create
                               :name "held" :start (lambda (&rest _) nil)))))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (e-harness-test-prompt-async harness "session" "running")
      (let ((publication (e-board-runtime-post-input
                          board :to "participant" :mode 'queue :content "queued")))
        (e-board-runtime--drain-input-routing
         board
         (lambda ()
           (e-board-drain-input-classifications
            (e-board-registry-board-source-board board))))
        (let ((pickup-id (car (e-board-message-pickup-ids
                               (e-board-publication-message publication)))))
          (e-board-runtime--drain-pickups)
          (should (eq (e-board-pickup-state
                       (e-board-pickup (e-board-registry-board-source-board board) pickup-id))
                      'accepted))
          (e-harness-reset harness "session")
          (should (eq (e-board-pickup-state
                       (e-board-pickup (e-board-registry-board-source-board board) pickup-id))
                      'discarded)))))))

(ert-deftest e-board-runtime-test-session-reset-settles-cancelling-queue-pickup ()
  "A reset acknowledgement closes an already fenced accepted delivery cancelled."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create
                     :backend (e-backend-create
                               :name "held" :start (lambda (&rest _) nil)))))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (e-harness-test-prompt-async harness "session" "running")
      (let ((publication (e-board-runtime-post-input
                          board :to "participant" :mode 'queue :content "queued")))
        (e-board-runtime--drain-input-routing
         board
         (lambda ()
           (e-board-drain-input-classifications
            (e-board-registry-board-source-board board))))
        (let* ((source-board (e-board-registry-board-source-board board))
               (pickup-id (car (e-board-message-pickup-ids
                                (e-board-publication-message publication)))))
          (e-board-runtime--drain-pickups)
          (e-board-cancel-pickup source-board pickup-id 'owner-cancelled)
          (should (eq (e-board-pickup-state (e-board-pickup source-board pickup-id))
                      'cancelling))
          (e-harness-reset harness "session")
          (should (eq (e-board-pickup-state (e-board-pickup source-board pickup-id))
                      'cancelled)))))))

(ert-deftest e-board-runtime-test-attached-session-enrolls-prepared-work-before-start ()
  "An attached harness maps turn/tool work to its participant board."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((handle (e-work-prepare
                      (e-work-spec-create
                       :id "tool" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done"))
                      nil
                      :context
                      '(:session-id "session" :turn-id "turn" :tool-call (:id "call"))))
             (source-board (e-board-registry-board-source-board board))
             (enroll (e-harness-work-enrollment-function harness)))
        (funcall enroll handle (lambda (&rest _args)))
        (should (e-board-observed-work source-board (e-work-handle-id handle)))
        (should (e-board-invocation source-board '("turn" "call")))
        (should-not (e-work-handle-started-p handle))))))

(ert-deftest e-board-runtime-test-exact-invocation-effect-uses-opaque-target ()
  "A terminal board effect reaches only the target's captured loop service."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           reply)
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "tool" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done"))
                      nil
                      :context
                      '(:session-id "session" :turn-id "turn" :tool-call (:id "call"))))
             (enroll (e-harness-work-enrollment-function harness)))
        (funcall enroll handle (lambda (state payload) (setq reply (list state payload))))
        (let ((invocation (e-board-invocation source-board '("turn" "call"))))
          (should invocation)
          (should-not (functionp (e-board-invocation-effect-target invocation))))
        (e-work-start-prepared handle)
        (should-not reply)
        (e-board-drain-terminal-classifications source-board)
        (e-board-drain-effects source-board)
        (should (equal reply '(finished "done")))
        (should (eq (e-board-invocation-state
                     (e-board-invocation source-board '("turn" "call")))
                    'committed))))))

(ert-deftest e-board-runtime-test-invocation-effect-rejects-stale-endpoint ()
  "A qualified invocation cannot call back after its harness generation clears."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           (calls 0))
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :qualified :kind 'chat :harness-id :live)
      (e-harness-registry-register :live harness)
      (e-board-runtime-attach-instance
       board :qualified "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle
              (e-work-prepare
               (e-work-spec-create
                :id "tool" :execution 'cheap :interactive-policy 'cheap
                :runner (lambda (_arguments _context) "done"))
               nil :context
               '(:session-id "session" :turn-id "turn"
                 :tool-call (:id "call"))))
             (enroll (e-harness-work-enrollment-function harness)))
        (funcall enroll handle (lambda (&rest _arguments) (cl-incf calls)))
        (e-work-start-prepared handle)
        (e-board-drain-terminal-classifications source-board)
        (let* ((invocation (e-board-invocation source-board '("turn" "call")))
               (target (e-board-invocation-effect-target invocation)))
          (e-harness-registry-clear-instance :live)
          (e-board-drain-effects source-board)
          (should (= calls 0))
          (should (eq (e-board-invocation-state invocation) 'failed))
          (should (eq (e-board-runtime-invocation-state
                       (gethash target e-board-runtime--invocations))
                      'unavailable)))))))

(ert-deftest e-board-runtime-test-await-aggregation-uses-opaque-target ()
  "Await completion uses the captured awaiting call rather than a board closure."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           reply)
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "watched" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done"))
                      nil :context '(:session-id "session" :turn-id "source"
                                     :tool-call (:id "source-call"))))
             (enroll (e-harness-work-enrollment-function harness)))
        (funcall enroll handle nil)
        (let ((cancel
               (e-board-runtime--subscribe-aggregation
                harness (list handle) 'all 30
                (lambda (reason) (setq reply reason))
                '(:session-id "session" :turn-id "await" :tool-call (:id "await-call")))))
          (unwind-protect
              (progn
                (let ((aggregation (e-board-aggregation source-board
                                                         '("await" "await-call"))))
                  (should aggregation)
                  (should-not (functionp
                               (e-board-aggregation-effect-target aggregation))))
                (e-work-start-prepared handle)
                (e-board-drain-terminal-classifications source-board)
                (e-board-drain-effects source-board)
                (should (eq reply 'complete)))
            (funcall cancel)))))))

(ert-deftest e-board-runtime-test-invalid-invocation-rejects-before-enrollment ()
  "An invalid exact invocation leaves its prepared work off the board."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "tool" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "never"))
                      nil :context '(:session-id "session" :turn-id "turn")))
             (enroll (e-harness-work-enrollment-function harness)))
        (should-error (funcall enroll handle (lambda (&rest _args)))
                      :type 'e-board-runtime-error)
        (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
        (should-not (e-work-handle-started-p handle))))))

(ert-deftest e-board-runtime-test-missing-source-turn-rejects-before-enrollment ()
  "Board work without participant activity provenance mutates no owner."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((source-board (e-board-registry-board-source-board board))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "invalid" :execution 'cooperative
                       :interactive-policy 'async
                       :runner (lambda (_handle _arguments _context) :deferred))
                      nil :context '(:session-id "session")))
             (enroll (e-harness-work-enrollment-function harness)))
        (should-error (funcall enroll handle nil)
                      :type 'e-board-runtime-invalid-work)
        (should-not (e-board-observed-work source-board (e-work-handle-id handle)))
        (should-not (e-work-handle-activity-observer handle))
        (should-not (e-work-handle-hook-dispatcher handle))
        (should-not (e-work-handle-started-p handle))))))

(ert-deftest e-board-runtime-test-enrollment-installs-bounded-activity-mailbox ()
  "Board enrollment captures progress before it schedules general hook work."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((handle (e-work-prepare
                      (e-work-spec-create
                       :id "stream" :execution 'cooperative :interactive-policy 'async
                       :runner (lambda (_handle _arguments _context) :deferred))
                      nil
                      :context '(:session-id "session" :turn-id "turn")))
             (enroll (e-harness-work-enrollment-function harness)))
        (unwind-protect
            (progn
              (funcall enroll handle nil)
              (should (equal (plist-get (e-work-handle-context handle) :turn-id)
                             "turn"))
              (should (functionp (e-work-handle-activity-observer handle)))
              (e-work-start-prepared handle)
              (e-work-progress handle '(:step first))
              (let ((mailbox (gethash (e-work-handle-id handle)
                                      e-board-runtime--work-activity-mailboxes)))
                (should (plist-member mailbox :attachment))
                (should (equal (plist-get mailbox :turn-id) "turn"))
                (should (equal (plist-get mailbox :payload) '(:step first))))
              (e-board-runtime--drain-activity-mailboxes)
              (let ((message (car (last (e-board-messages
                                         (e-board-registry-board-source-board board))))))
                (should (eq (e-board-message-kind message) 'activity))
                (should (equal (e-board-message-source-turn-id message) "turn"))
                (should (equal (e-board-message-source-activity-key message)
                               '("participant" 1 1)))))
          (e-work-cancel handle))))))

(ert-deftest e-board-runtime-test-publishes-final-assistant-output-idempotently ()
  "Repeated completion notifications retain one participant board output."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (let ((attachment (e-board-runtime-attach
                         board harness "session" :participant-id "participant")))
        (e-session-append-message
         (e-harness-sessions harness) "session"
         '(:id "assistant-1" :role assistant :turn-id "turn" :content "done"))
        (e-board-runtime--publish-output attachment "turn")
        (e-board-runtime--publish-output attachment "turn")
        (let ((messages (e-board-messages
                         (e-board-registry-board-source-board board))))
          (should (= (length messages) 1))
          (should (eq (e-board-message-kind (car messages)) 'output))
          (should (equal (e-board-message-content (car messages)) "done")))))))

(ert-deftest e-board-runtime-test-rebind-gives-fresh-session-output-a-new-source-generation ()
  "A fresh session after rebind cannot deduplicate a prior endpoint's output."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           scheduled)
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (let ((old (e-board-runtime-attach board old-harness "old"
                                         :participant-id "participant")))
        (e-session-append-message
         (e-harness-sessions old-harness) "old"
         '(:id "assistant-old" :role assistant :turn-id "old-turn" :content "old"))
        (e-board-runtime--publish-output old "old-turn")
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq scheduled
                           (append scheduled
                                   (list (lambda ()
                                           (apply function arguments))))))))
          (let ((request (e-board-runtime-rebind-start
                          board "participant" new-harness "new" "owner")))
            (funcall (pop scheduled))
            (funcall (pop scheduled))
            (let ((new (e-request-lifecycle-terminal-payload request)))
              (e-session-append-message
               (e-harness-sessions new-harness) "new"
               '(:id "assistant-new" :role assistant :turn-id "new-turn"
                 :content "new"))
              (e-board-runtime--publish-output new "new-turn"))))
        (let ((messages (e-board-messages
                         (e-board-registry-board-source-board board))))
          (should (equal (mapcar #'e-board-message-content messages) '("old" "new")))
          (should (equal (mapcar #'e-board-message-source-output-key messages)
                         '(("participant" 1 1) ("participant" 2 1)))))))))

(ert-deftest e-board-runtime-test-retire-attachment-releases-exact-runtime-life ()
  "Terminal attachment retirement removes routes and permits later re-ensure."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           (source nil)
           (attachment nil)
           (ordinary nil))
      (e-harness-create-session harness :id "session")
      (setq attachment
            (e-board-runtime-attach
             board harness "session" :participant-id "participant"
             :principal "owner"))
      (setq source (e-board-registry-board-source-board board)
            ordinary (e-board-registry-install-subscription
                      board (e-board-runtime-attachment-participant attachment)
                      '(:tags (ordinary))))
      (should (gethash
               (e-board-runtime--session-key harness "session")
               e-board-runtime--session-attachments))
      (e-board-runtime-retire-attachment attachment)
      (should (eq (e-board-runtime-attachment-state attachment) 'dormant))
      (should-not (gethash
                   (e-board-runtime--session-key harness "session")
                   e-board-runtime--session-attachments))
      (should-not (gethash
                   (e-board-runtime--session-key harness "session")
                   e-board-runtime--endpoint-attachments))
      (should-not (gethash "participant"
                           (e-board-registry-board-participants board)))
      (should-not (e-board-participant source "participant"))
      (should (eq (e-board-subscription-state ordinary) 'cancelled))
      (should-not (e-board-find-subscription source
                                              (e-board-subscription-id ordinary)))
      ;; Repeated terminal calls are no-ops, and the same durable session and
      ;; participant identity may be admitted again while the board remains
      ;; active.
      (e-board-runtime-retire-attachment attachment)
      (let ((replacement
             (e-board-runtime-attach
              board harness "session" :participant-id "participant"
              :principal "owner")))
        (should (e-board-runtime--current-attachment-p replacement))
        (should (not (eq replacement attachment)))))))

(ert-deftest e-board-runtime-test-retire-stale-attachment-preserves-replacement ()
  "Retiring an old rebind attachment cannot remove the replacement lease."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           scheduled)
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (let* ((old (e-board-runtime-attach
                   board old-harness "old" :participant-id "participant"))
             (request nil)
             new)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest arguments)
                     (push (lambda () (apply function arguments)) scheduled))))
          (setq request
                (e-board-runtime-rebind-start
                 board "participant" new-harness "new" "owner"))
          (while scheduled
            (funcall (pop scheduled))))
        (setq new (e-request-lifecycle-terminal-payload request))
        (should (e-board-runtime-attachment-p new))
        (should (eq (e-board-runtime-attachment-state old) 'dormant))
        (e-board-runtime-retire-attachment old)
        (should (e-board-runtime--current-attachment-p new))
        (should (eq (gethash
                     (e-board-runtime--attachment-key
                      board (e-board-runtime-attachment-participant new))
                     e-board-runtime--attachments)
                    new))
        (should (e-board-participant
                 (e-board-registry-board-source-board board) "participant"))))))

(provide 'e-board-runtime-test)

;;; e-board-runtime-test.el ends here

(ert-deftest e-board-runtime-test-work-progress-preserves-subagent-provenance ()
  "A child work update reaches the board as coalesced work-progress activity."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((handle (e-work-prepare
                      (e-work-spec-create
                       :id "subagent" :execution 'cooperative :interactive-policy 'async
                       :runner (lambda (_handle _arguments _context) :deferred))
                      nil :context '(:session-id "session" :turn-id "turn")))
             (enroll (e-harness-work-enrollment-function harness)))
        (unwind-protect
            (progn
              (funcall enroll handle nil)
              (e-work-start-prepared handle)
              (e-work-progress handle
                               '(:subagent-id "sub_000001" :sequence 3
                                 :event tool-finished :summary "Finished tool" :at 1.0))
              (e-work-progress handle
                               '(:subagent-id "sub_000001" :sequence 4
                                 :event action-started :summary "Started action" :at 2.0))
              (e-board-runtime--drain-activity-mailboxes)
              (let ((message (car (last (e-board-messages
                                         (e-board-registry-board-source-board board))))))
                (should (eq (e-board-message-activity-kind message) 'work-progress))
                (should (equal (plist-get (e-board-message-attributes message)
                                          :work-id)
                               (e-work-handle-id handle)))
                (should (equal (plist-get (e-board-message-attributes message)
                                          :subagent-id)
                               "sub_000001"))
                (should (string-match-p "action-started"
                                        (e-board-message-content message)))))
          (e-work-cancel handle))))))

;;; e-chat-continuation-outcome-test.el --- Continuation outcome adapter tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-orchestration)
(require 'e-chat-service)
(require 'e-work)

(defconst e-chat-continuation-outcome-test--spec
  (e-work-spec-create
   :id "chat-continuation-outcome-test"
   :execution 'cheap
   :interactive-policy 'cheap
   :owner 'e-chat-continuation-outcome-test
   :runner (lambda (arguments _context) arguments)))

(defconst e-chat-continuation-outcome-test--deferred-spec
  (e-work-spec-create
   :id "chat-continuation-outcome-deferred-test"
   :execution 'cooperative
   :interactive-policy 'async
   :owner 'e-chat-continuation-outcome-test
   :runner (lambda (_handle _arguments _context) :deferred)))

(defun e-chat-continuation-outcome-test--finished-work (&optional value)
  "Return a settled Work carrying VALUE."
  (e-work-start e-chat-continuation-outcome-test--spec value))

(defun e-chat-continuation-outcome-test--pending-work ()
  "Return an unsettled Work used to exercise async publication settlement."
  (e-work-start e-chat-continuation-outcome-test--deferred-spec nil))

(defun e-chat-continuation-outcome-test--binding (&optional turns)
  "Return a detached binding fixture with continuation TURN correlation."
  (e-chat-service--binding-create
   :harness 'harness
   :session-id "coordinator"
   :board-id "board-1"
   :participant-id "participant-1"
   :sqlite-service 'sqlite-service
   :turn-port 'port
   :lifecycle-state 'active
   :subscribers nil
   :executing-turns (make-hash-table :test 'equal)
   :continuation-deliveries (make-hash-table :test 'equal)
   :continuation-turns (or turns (make-hash-table :test 'equal))
   :continuation-outcome-inflight (make-hash-table :test 'equal)))

(ert-deftest e-chat-continuation-outcome-test-publication-uses-stable-fact-key ()
  "The terminal adapter publishes one generic outcome fact identity."
  (let* ((binding (e-chat-continuation-outcome-test--binding))
         (context '(:run-id "run-1" :publication-key "continue-1"
                    :turn-id "turn-1"))
         fact)
    (cl-letf (((symbol-function 'e-board-sqlite-service-orchestration-fact-start)
               (lambda (_service _board-id value)
                 (setq fact (copy-tree value t))
                 (e-chat-continuation-outcome-test--finished-work
                  '(:status posted)))))
      (let ((work (e-chat-service--publish-sqlite-continuation-outcome
                   binding context 'done)))
        (should (e-work-handle-p work))
        (should (eq (plist-get (plist-get fact :payload) :status) 'done))
        (should (equal (plist-get fact :idempotency-key)
                       (e-board-orchestration-continuation-outcome-key
                        "run-1" "continue-1")))
        (should (equal (plist-get (plist-get fact :payload) :turn-id)
                       "turn-1"))))))

(ert-deftest e-chat-continuation-outcome-test-terminal-event-publishes-once ()
  "A duplicate terminal callback cannot publish a second process-local fact."
  (let* ((turns (make-hash-table :test 'equal))
         (binding (e-chat-continuation-outcome-test--binding turns))
         (calls nil))
    (puthash "turn-1"
             '(:run-id "run-1" :publication-key "continue-1"
               :turn-id "turn-1")
             turns)
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (_binding context status &optional error)
                 (push (list context status error) calls)
                 (e-chat-continuation-outcome-test--finished-work
                  '(:status posted))))
              ((symbol-function 'e-harness-attached-turn-port-assistant-message)
               (lambda (_port _turn-id)
                 '(:content "coordinator output")))
              ((symbol-function 'e-board-sqlite-service-record-append-start)
               (lambda (&rest _arguments)
                 (e-chat-continuation-outcome-test--finished-work nil)))
              ((symbol-function 'e-board-sqlite-service-notify-delivery-outcome)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'e-chat-service--sql-notify-event)
               (lambda (&rest _arguments) nil)))
      (let ((event '(:type turn-finished :turn-id "turn-1"
                     :payload (:reason completed))))
        (e-chat-service--sql-harness-event binding event)
        (e-chat-service--sql-harness-event binding event))
      (should (= (length calls) 1))
      (should (eq (cadar calls) 'done))
      (should-not (gethash "turn-1" turns)))))

(ert-deftest e-chat-continuation-outcome-test-sync-publication-failure-retains-correlation ()
  "A synchronous publication error keeps the turn available for retry."
  (let* ((turns (make-hash-table :test 'equal))
         (binding (e-chat-continuation-outcome-test--binding turns))
         (attempts 0)
         (failures nil))
    (puthash "turn-1"
             '(:run-id "run-1" :publication-key "continue-1"
               :turn-id "turn-1")
             turns)
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (&rest _arguments)
                 (cl-incf attempts)
                 (if (= attempts 1)
                     (error "synchronous publication failed")
                   (e-chat-continuation-outcome-test--finished-work
                    '(:status posted)))))
              ((symbol-function 'e-chat-service--sql-note-failure)
               (lambda (_binding error &optional _owner-suspect-p)
                 (push error failures)))
              ((symbol-function 'e-harness-attached-turn-port-assistant-message)
               (lambda (&rest _arguments) '(:content "output")))
              ((symbol-function 'e-board-sqlite-service-record-append-start)
               (lambda (&rest _arguments)
                 (e-chat-continuation-outcome-test--finished-work nil)))
              ((symbol-function 'e-chat-service--sql-notify-event)
               (lambda (&rest _arguments) nil)))
      (let ((event '(:type turn-finished :turn-id "turn-1"
                     :payload (:reason completed))))
        (e-chat-service--sql-harness-event binding event)
        (should (= attempts 1))
        (should (gethash "turn-1" turns))
        (e-chat-service--sql-harness-event binding event)
        (should (= attempts 2))
        (should-not (gethash "turn-1" turns))
        (should failures)))))

(ert-deftest e-chat-continuation-outcome-test-async-publication-failure-retains-and-retries ()
  "An async publication failure retains correlation until a later success."
  (let* ((turns (make-hash-table :test 'equal))
         (binding (e-chat-continuation-outcome-test--binding turns))
         (attempts 0)
         (pending nil)
         (retry nil)
         (failures nil))
    (puthash "turn-1"
             '(:run-id "run-1" :publication-key "continue-1"
               :turn-id "turn-1")
             turns)
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (&rest _arguments)
                 (cl-incf attempts)
                 (if (= attempts 1)
                     (setq pending
                           (e-chat-continuation-outcome-test--pending-work))
                   (setq retry
                         (e-chat-continuation-outcome-test--pending-work)))))
              ((symbol-function 'e-chat-service--sql-note-failure)
               (lambda (_binding error &optional _owner-suspect-p)
                 (push error failures)))
              ((symbol-function 'e-harness-attached-turn-port-assistant-message)
               (lambda (&rest _arguments) '(:content "output")))
              ((symbol-function 'e-board-sqlite-service-record-append-start)
               (lambda (&rest _arguments)
                 (e-chat-continuation-outcome-test--finished-work nil)))
              ((symbol-function 'e-chat-service--sql-notify-event)
               (lambda (&rest _arguments) nil)))
      (let ((event '(:type turn-finished :turn-id "turn-1"
                     :payload (:reason completed))))
        (e-chat-service--sql-harness-event binding event)
        (e-chat-service--sql-harness-event binding event)
        (should (= attempts 1))
        (should (eq (gethash "turn-1"
                             (e-chat-service-binding-continuation-outcome-inflight
                              binding))
                    pending))
        (should (gethash "turn-1" turns))
        (e-work-fail pending '(e-chat-service-error "async publication failed"))
        (should-not
         (gethash "turn-1"
                  (e-chat-service-binding-continuation-outcome-inflight binding)))
        (should (gethash "turn-1" turns))
        (e-chat-service--sql-harness-event binding event)
        (should (= attempts 2))
        (should (eq (gethash "turn-1"
                             (e-chat-service-binding-continuation-outcome-inflight
                              binding))
                    retry))
        (e-work-finish retry '(:status posted))
        (should-not (gethash "turn-1" turns))
        (should failures)))))

(ert-deftest e-chat-continuation-outcome-test-synchronous-terminal-failure-correlates-submit ()
  "A terminal admission failure before submit returns still gets persisted."
  (let* ((binding (e-chat-continuation-outcome-test--binding))
         (delivery '("board-1" "message-1" "participant-1"))
         (calls nil))
    (puthash delivery
             '(:run-id "run-1" :publication-key "continue-1")
             (e-chat-service-binding-continuation-deliveries binding))
    (puthash delivery 'submitting
             (e-chat-service-binding-executing-turns binding))
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (_binding context status &optional error)
                 (push (list context status error) calls)
                 (e-chat-continuation-outcome-test--finished-work
                  '(:status posted))))
              ((symbol-function 'e-board-sqlite-service-notify-delivery-outcome)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'e-chat-service--sql-notify-event)
               (lambda (&rest _arguments) nil)))
      (e-chat-service--sql-harness-event
       binding
       '(:type turn-failed :turn-id "turn-1" :payload (:error "admission")))
      (should (= (length calls) 1))
      (should (eq (cadar calls) 'failed))
      (should-not
       (gethash delivery
                (e-chat-service-binding-continuation-deliveries binding))))))

(ert-deftest e-chat-continuation-outcome-test-outcome-precedes-output-publication-failure ()
  "A failed ancillary output row cannot suppress the terminal outcome."
  (let* ((turns (make-hash-table :test 'equal))
         (binding (e-chat-continuation-outcome-test--binding turns))
         (calls nil))
    (puthash "turn-1"
             '(:run-id "run-1" :publication-key "continue-1"
               :turn-id "turn-1")
             turns)
    (cl-letf (((symbol-function 'e-chat-service--publish-sqlite-continuation-outcome)
               (lambda (&rest _arguments)
                 (push t calls)
                 (e-chat-continuation-outcome-test--finished-work
                  '(:status posted))))
              ((symbol-function 'e-harness-attached-turn-port-assistant-message)
               (lambda (&rest _arguments) '(:content "output")))
              ((symbol-function 'e-board-sqlite-service-record-append-start)
               (lambda (&rest _arguments)
                 (error "output publication failed"))))
      (should-error
       (e-chat-service--sql-harness-event
        binding '(:type turn-finished :turn-id "turn-1" :payload nil)))
      (should (= (length calls) 1)))))

(provide 'e-chat-continuation-outcome-test)

;;; e-chat-continuation-outcome-test.el ends here

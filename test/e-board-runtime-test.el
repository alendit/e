;;; e-board-runtime-test.el --- Tests for board harness delivery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-runtime)

(defmacro e-board-runtime-test--with-empty-state (&rest body)
  "Run BODY with isolated board, registry, and runtime attachment state."
  (declare (indent 0) (debug t))
  `(let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
          (e-board-registry--boards (make-hash-table :test 'equal))
          (e-board-registry--id-sequence 0)
          (e-board-runtime--attachments (make-hash-table :test 'equal))
          (e-board-runtime--session-attachments (make-hash-table :test 'equal))
          (e-board-runtime--deferred-hooks nil)
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
          (e-board-runtime--pickup-drain-scheduled nil))
     ,@body))

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
        (e-board-runtime--drain-pickups)
        (should (equal (mapcar #'car deliveries) '("first" "first" "second")))
        (should (equal (mapcar #'car (mapcar #'cdr deliveries))
                       '(("board" "exact" "first")
                         ("board" "tagged" "first")
                         ("board" "tagged" "second"))))))))

(ert-deftest e-board-runtime-test-default-queue-delivery-enters-idle-follow-up-queue ()
  "Default queue delivery uses the harness queue without starting a turn."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (e-board-runtime-post-input board :to "participant" :mode 'queue
                                  :content "queued input")
      (should-not (e-harness-queued-prompts harness "session"))
      (e-board-runtime--drain-pickups)
      (should (equal (plist-get (car (e-harness-queued-prompts harness "session"))
                                :prompt)
                     "queued input"))
       (should-not (plist-get (e-harness-state harness "session") :active-turn)))))

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

(ert-deftest e-board-runtime-test-enrollment-installs-bounded-activity-mailbox ()
  "Board enrollment captures progress before it schedules general hook work."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
      (let* ((handle (e-work-prepare
                      (e-work-spec-create
                       :id "stream" :execution 'render :interactive-policy 'async
                       :runner (lambda (_arguments _context) :never))
                      '(:delay 600)
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

(provide 'e-board-runtime-test)

;;; e-board-runtime-test.el ends here

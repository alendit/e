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
          (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
          (e-board-runtime--invocations (make-hash-table :test 'equal))
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
     ,@body))

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
           deliveries
           (catalog (lambda (&rest _arguments) 'pending))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :qualified :kind 'chat :harness-id :live
       :session-store-id "store"
       :session-catalog catalog :session-access-store access-store
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
        (should (equal (e-board-runtime-attachment-session-store-id attachment)
                       "store"))
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
           (catalog (lambda (&rest _arguments) 'pending))
           (access-store (lambda (&rest _arguments) 'pending))
           observed)
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :qualified :kind 'chat :harness-id :live
       :session-store-id "store"
       :session-catalog catalog :session-access-store access-store)
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
           (harness (e-harness-create))
           (catalog (lambda (&rest _arguments) 'pending))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :qualified :kind 'chat :harness-id :live
       :session-store-id "store"
       :session-catalog catalog :session-access-store access-store)
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
  "Removal gets an exact harness discard receipt for an idle queued input."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "owner"))
           (harness (e-harness-create))
           (source-board (e-board-registry-board-source-board board))
           scheduled request delivery-id pickup)
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach
       board harness "session" :participant-id "participant")
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
        (funcall (pop scheduled))
        (funcall (pop scheduled))
        (funcall (pop scheduled))
        (should (eq (e-board-pickup-state pickup) 'cancelled))
        (should-not (e-harness-queued-prompts harness "session"))
        (should (= (length scheduled) 1))
        (funcall (pop scheduled))
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

(ert-deftest e-board-runtime-test-qualified-store-session-attaches-only-once ()
  "Two harness objects cannot attach the same stable store/session identity."
  (e-board-runtime-test--with-empty-state
    (let* ((first-board (e-board-registry-create :id "first-board"))
           (second-board (e-board-registry-create :id "second-board"))
           (first-harness (e-harness-create))
           (second-harness (e-harness-create))
           (catalog (lambda (&rest _arguments) 'pending))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-create-session first-harness :id "session")
      (e-harness-create-session second-harness :id "session")
      (e-harness-instance-register
       :id :first :kind 'chat :harness-id :first-harness
       :session-store-id "shared-store"
       :session-catalog catalog :session-access-store access-store)
      (e-harness-instance-register
       :id :second :kind 'chat :harness-id :second-harness
       :session-store-id "shared-store"
       :session-catalog catalog :session-access-store access-store)
      (e-harness-registry-register :first-harness first-harness)
      (e-harness-registry-register :second-harness second-harness)
      (e-board-runtime-attach-instance
       first-board :first "session" :participant-id "first")
      (should-error
       (e-board-runtime-attach-instance
        second-board :second "session" :participant-id "second")
       :type 'e-board-runtime-session-busy)
      (should (= (hash-table-count
                  (e-board-registry-board-participants second-board))
                 0)))))

(ert-deftest e-board-runtime-test-live-resume-validates-catalog-before-attach ()
  "Authorized current dormant row attaches later without invoking a factory."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "board-owner"))
           (harness (e-harness-create))
           (factory-calls 0)
           succeed
           (catalog (lambda (_request on-done _on-error)
                      (setq succeed on-done)
                      nil))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-board-registry-authorize-principal
       board "board-owner" "resumer" 'member)
      (e-harness-create-session harness :id "session")
      (e-harness-instance-register
       :id :instance :kind 'chat :harness-id :live
       :session-store-id "store"
       :session-catalog catalog :session-access-store access-store
       :factory (lambda () (cl-incf factory-calls) harness))
      (e-harness-registry-register :live harness)
      (let ((request
             (e-board-runtime-resume-live-instance-start
              board :instance "store" "session" "resumer" 3
              :participant-id "participant")))
        (should (eq (e-request-lifecycle-state request) 'started))
        (should (= (hash-table-count
                    (e-board-registry-board-participants board))
                   0))
        (should (= factory-calls 0))
        (funcall succeed
                 '(:session-id "session" :state dormant
                   :access-record
                   (:controller "controller" :version 3
                    :discover-principals nil :resume-principals ("resumer"))
                   :board-output-sequence 5 :board-activity-sequence 8))
        (should (eq (e-request-lifecycle-state request) 'finished))
        (let* ((attachment (e-request-lifecycle-terminal-payload request))
               (participant
                (e-board-runtime-attachment-participant attachment)))
          (should (equal (e-board-registry-participant-principal participant)
                         "resumer"))
          (should (equal (e-board-registry-participant-controller participant)
                         "controller"))
          (should (= factory-calls 0)))))))

(ert-deftest e-board-runtime-test-live-resume-denial-precedes-factory-or-membership ()
  "Denied durable resume creates no harness, grant, participant, or attachment."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "requester"))
           (factory-calls 0)
           succeed
           (catalog (lambda (_request on-done _on-error)
                      (setq succeed on-done)
                      nil))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-instance-register
       :id :instance :kind 'chat :harness-id :offline
       :session-store-id "store"
       :session-catalog catalog :session-access-store access-store
       :factory (lambda () (cl-incf factory-calls) (e-harness-create)))
      (let ((request
             (e-board-runtime-resume-live-instance-start
              board :instance "store" "session" "requester" 2
              :participant-id "participant")))
        (funcall succeed
                 '(:session-id "session" :state dormant
                   :access-record
                   (:controller "controller" :version 2
                    :discover-principals ("requester") :resume-principals nil)
                   :board-output-sequence 0 :board-activity-sequence 0))
        (should (eq (e-request-lifecycle-state request) 'failed))
        (should (eq (car (e-request-lifecycle-terminal-payload request))
                    'e-board-runtime-resume-denied))
        (should (= factory-calls 0))
        (should (= (hash-table-count
                    (e-board-registry-board-participants board))
                   0))))))

(ert-deftest e-board-runtime-test-live-resume-offline-instance-never-calls-factory ()
  "Authorized offline resume fails visibly instead of invoking legacy factory."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "requester"))
           (factory-calls 0)
           succeed
           (catalog (lambda (_request on-done _on-error)
                      (setq succeed on-done)
                      nil))
           (access-store (lambda (&rest _arguments) 'pending)))
      (e-harness-instance-register
       :id :instance :kind 'chat :harness-id :offline
       :session-store-id "store"
       :session-catalog catalog :session-access-store access-store
       :factory (lambda () (cl-incf factory-calls) (e-harness-create)))
      (let ((request
             (e-board-runtime-resume-live-instance-start
              board :instance "store" "session" "requester" 2
              :participant-id "participant")))
        (funcall succeed
                 '(:session-id "session" :state dormant
                   :access-record
                   (:controller "requester" :version 2
                    :discover-principals nil :resume-principals nil)
                   :board-output-sequence 0 :board-activity-sequence 0))
        (should (eq (e-request-lifecycle-state request) 'failed))
        (should (eq (car (e-request-lifecycle-terminal-payload request))
                    'e-harness-registry-missing))
        (should (= factory-calls 0))
        (should (= (hash-table-count
                    (e-board-registry-board-participants board))
                   0))))))

(ert-deftest e-board-runtime-test-offline-resume-revalidates-before-atomic-attach ()
  "Offline activation stays private until a second authorized catalog read."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "requester"))
           (factory-calls 0)
           catalog-requests
           activation-arguments
           activation-succeed
           (catalog
            (lambda (arguments on-done on-error)
              (setq catalog-requests
                    (append catalog-requests
                            (list (list arguments on-done on-error))))
              nil))
           (access-store (lambda (&rest _arguments) 'pending))
           (activation
            (lambda (arguments on-done _on-error)
              (setq activation-arguments arguments
                    activation-succeed on-done)
              nil))
           (row '(:session-id "session" :state dormant
                  :access-record
                  (:controller "controller" :version 3
                   :discover-principals nil :resume-principals ("requester"))
                  :board-output-sequence 5 :board-activity-sequence 8)))
      (e-harness-instance-register
       :id :instance :kind 'chat :harness-id :offline
       :session-store-id "store"
       :session-catalog catalog :session-access-store access-store
       :session-activation activation
       :factory (lambda () (cl-incf factory-calls) (e-harness-create)))
      (let ((request
             (e-board-runtime-resume-instance-start
              board :instance "store" "session" "requester" 3
              :participant-id "participant")))
        (should (= (length catalog-requests) 1))
        (funcall (nth 1 (car catalog-requests)) row)
        (should (equal activation-arguments
                       '(:session-store-id "store" :session-id "session"
                         :requester-principal "requester" :expected-version 3)))
        (should-not (e-harness-registry-get :offline))
        (should-not
         (gethash "participant" (e-board-registry-board-participants board)))
        (let ((harness (e-harness-create)))
          (e-harness-create-session harness :id "session")
          (funcall activation-succeed harness)
          (should (= (length catalog-requests) 2))
          (should-not (e-harness-registry-get :offline))
          (should-not
           (gethash "participant" (e-board-registry-board-participants board)))
          (funcall (nth 1 (cadr catalog-requests)) row)
          (should (eq (e-request-lifecycle-state request) 'finished))
          (should (eq (e-harness-registry-get :offline) harness))
          (let* ((attachment (e-request-lifecycle-terminal-payload request))
                 (participant (e-board-runtime-attachment-participant attachment)))
            (should (equal (e-board-registry-participant-principal participant)
                           "requester"))
            (should (equal (e-board-registry-participant-controller participant)
                           "controller"))))
        (should (= factory-calls 0))))))

(ert-deftest e-board-runtime-test-offline-resume-revocation-during-load-stays-dormant ()
  "A version/authorization change during loading prevents registry mutation."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board" :principal "requester"))
           catalog-requests
           activation-succeed
           (catalog
            (lambda (arguments on-done on-error)
              (setq catalog-requests
                    (append catalog-requests
                            (list (list arguments on-done on-error))))
              nil))
           (access-store (lambda (&rest _arguments) 'pending))
           (activation
            (lambda (_arguments on-done _on-error)
              (setq activation-succeed on-done)
              nil))
           (authorized
            '(:session-id "session" :state dormant
              :access-record
              (:controller "controller" :version 3
               :discover-principals nil :resume-principals ("requester"))
              :board-output-sequence 0 :board-activity-sequence 0))
           (revoked
            '(:session-id "session" :state dormant
              :access-record
              (:controller "controller" :version 4
               :discover-principals nil :resume-principals nil)
              :board-output-sequence 0 :board-activity-sequence 0)))
      (e-harness-instance-register
       :id :instance :kind 'chat :harness-id :offline
       :session-store-id "store"
       :session-catalog catalog :session-access-store access-store
       :session-activation activation)
      (let ((request
             (e-board-runtime-resume-instance-start
              board :instance "store" "session" "requester" 3
              :participant-id "participant")))
        (funcall (nth 1 (car catalog-requests)) authorized)
        (let ((harness (e-harness-create)))
          (e-harness-create-session harness :id "session")
          (funcall activation-succeed harness))
        (funcall (nth 1 (cadr catalog-requests)) revoked)
        (should (eq (e-request-lifecycle-state request) 'failed))
        (should (eq (car (e-request-lifecycle-terminal-payload request))
                    'e-board-runtime-resume-version-conflict))
        (should-not (e-harness-registry-get :offline))
        (should-not
         (gethash "participant"
                  (e-board-registry-board-participants board)))))))

(ert-deftest e-board-runtime-test-rebind-preserves-participant-and-fences-old-session ()
  "A participant rebind retains its logical identity and uses the new endpoint."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           (deliveries nil))
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (let ((old-attachment
             (e-board-runtime-attach board old-harness "old" :participant-id "participant")))
      (e-board-runtime-post-input board :id "before-rebind" :to "participant"
                                  :content "preserved pickup")
      (e-board-runtime--drain-input-routing
       board (lambda () (e-board-drain-input-classifications
                         (e-board-registry-board-source-board board))))
      (let ((attachment
             (e-board-runtime-rebind
              board "participant" new-harness "new"
              :delivery-function
              (lambda (_attachment _pickup message)
                (push (e-board-message-content message) deliveries)))))
        (should (eq (e-board-runtime-attachment-harness attachment) new-harness))
        (should-not (e-board-runtime--current-attachment-p old-attachment))
        (should (e-board-runtime--current-attachment-p attachment))
        (should-not (gethash (e-board-runtime--session-key old-harness "old")
                             e-board-runtime--session-attachments))
        (let ((source-board (e-board-registry-board-source-board board)))
          (puthash "stale-work"
                   (list :attachment old-attachment :turn-id "turn"
                         :payload 'stale :source-key '(participant 1 1))
                   e-board-runtime--work-activity-mailboxes)
          (e-board-runtime--enqueue-activity-flush "stale-work")
          (let ((message-count (length (e-board-messages source-board))))
            (e-board-runtime--drain-activity-mailboxes)
            (should (= (length (e-board-messages source-board)) message-count))))
        (e-board-runtime-post-input board :id "after-rebind" :to "participant"
                                    :content "new endpoint")
        (e-board-runtime--drain-input-routing
         board (lambda () (e-board-drain-input-classifications
                           (e-board-registry-board-source-board board))))
        (e-board-runtime--drain-pickups)
        (should (equal (nreverse deliveries)
                       '("preserved pickup" "new endpoint"))))))))

(ert-deftest e-board-runtime-test-rebind-tombstones-unresolved-accepted-head ()
  "A replacement endpoint never receives an old accepted pickup again."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create))
           deliveries)
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (e-board-runtime-attach
       board old-harness "old" :participant-id "participant"
       :delivery-function (lambda (&rest _arguments) '(:accepted receipt)))
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
          (e-board-runtime-rebind
           board "participant" new-harness "new"
           :delivery-function
           (lambda (_attachment _pickup message)
             (push (e-board-message-content message) deliveries)))
          (e-board-runtime--drain-pickups)
          (should (eq (e-board-pickup-state (e-board-pickup source-board first-id))
                      'uncertain))
          (should (eq (e-board-pickup-state (e-board-pickup source-board second-id))
                      'consumed))
          (should (equal deliveries '("second"))))))))

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

(ert-deftest e-board-runtime-test-default-queue-delivery-enters-idle-follow-up-queue ()
  "Default queue delivery uses the harness queue without starting a turn."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (harness (e-harness-create))
           attachment)
      (e-harness-create-session harness :id "session")
      (setq attachment (e-board-runtime-attach
                        board harness "session" :participant-id "participant"))
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
        (should-not (plist-get (e-harness-state harness "session") :active-turn))))))

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
  "Visible lifecycle events expose only their type and durable event identity."
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
          (e-board-runtime--handle-harness-event attachment event)
          ;; A durable event may be replayed after an interrupted publication.
          (e-board-runtime--handle-harness-event attachment event))
        (let ((activity (car (last (e-board-messages source-board)))))
          (should (= (length (e-board-messages source-board)) 1))
          (should (eq (e-board-message-kind activity) 'activity))
          (should (eq (e-board-message-activity-kind activity)
                      'provider-request-started))
          (should-not (e-board-message-content activity))
          (should (equal (e-board-message-attributes activity)
                         '(:source-event-id "provider-event")))
          (should (equal (e-board-message-source-activity-key activity)
                         '("participant" 1 34))))))))

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
                     "owner"))
      (e-board-registry-detach-client board "client")
      (should-error
       (e-board-runtime-post-input board :id "denied" :tags '(main)
                                   :requester context :content "no")
       :type 'e-board-registry-authorization-denied)
      (should (= (length (e-board-messages source-board)) 1)))))

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
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
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
           (harness (e-harness-create)))
      (e-harness-create-session harness :id "session")
      (e-board-runtime-attach board harness "session" :participant-id "participant")
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

(ert-deftest e-board-runtime-test-rebind-gives-fresh-session-output-a-new-source-generation ()
  "A fresh session after rebind cannot deduplicate a prior endpoint's output."
  (e-board-runtime-test--with-empty-state
    (let* ((board (e-board-registry-create :id "board"))
           (old-harness (e-harness-create))
           (new-harness (e-harness-create)))
      (e-harness-create-session old-harness :id "old")
      (e-harness-create-session new-harness :id "new")
      (let ((old (e-board-runtime-attach board old-harness "old"
                                         :participant-id "participant")))
        (e-session-append-message
         (e-harness-sessions old-harness) "old"
         '(:id "assistant-old" :role assistant :turn-id "old-turn" :content "old"))
        (e-board-runtime--publish-output old "old-turn")
        (let ((new (e-board-runtime-rebind board "participant" new-harness "new")))
          (e-session-append-message
           (e-harness-sessions new-harness) "new"
           '(:id "assistant-new" :role assistant :turn-id "new-turn" :content "new"))
          (e-board-runtime--publish-output new "new-turn"))
        (let ((messages (e-board-messages
                         (e-board-registry-board-source-board board))))
          (should (equal (mapcar #'e-board-message-content messages) '("old" "new")))
          (should (equal (mapcar #'e-board-message-source-output-key messages)
                         '(("participant" 1 1) ("participant" 2 1)))))))))

(provide 'e-board-runtime-test)

;;; e-board-runtime-test.el ends here

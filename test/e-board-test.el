;;; e-board-test.el --- Tests for the pure board core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board)

(defmacro e-board-test--with-empty-registry (&rest body)
  "Run BODY with isolated process-local board state."
  (declare (indent 0))
  `(let ((e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0))
     ;; Most behavior tests assert post-classification projections.  Give them
     ;; an explicit test scheduler while the production default stays deferred.
     (cl-letf (((symbol-function 'e-board--schedule-input-classification)
                (lambda (board)
                  (e-board-drain-input-classifications board))))
       ,@body)))

(ert-deftest e-board-test-generated-and-injected-identities ()
  "Board creation and member creation support deterministic identities."
  (e-board-test--with-empty-registry
    (let ((ids '("participant-generated" "subscription-generated")))
      (let* ((board (e-board-create :id "board-1"
                                    :id-function (lambda (_kind) (pop ids))))
             (participant (e-board-add-participant
                           board :create-pickup-subscription-id "sub-address")))
        (should (eq board (e-board-get "board-1")))
        (should (equal (e-board-participant-id participant)
                       "participant-generated"))
        (should (equal (e-board-participant-create-pickup-subscription-id
                        participant)
                       "sub-address"))
        (should-error (e-board-create :id "board-1")
                       :type 'e-board-id-conflict)))))

(ert-deftest e-board-test-default-identities-use-distinct-namespaces ()
  "Default generated board identities cannot be mistaken for participants."
  (e-board-test--with-empty-registry
    (let* ((board (e-board-create))
           (participant (e-board-add-participant board)))
      (should (string-prefix-p "brd_" (e-board-id board)))
      (should (string-prefix-p "ptc_" (e-board-participant-id participant)))
      (should (string-prefix-p "sub_"
                             (e-board-participant-create-pickup-subscription-id
                              participant))))))

(ert-deftest e-board-test-exact-address-ignores-tags ()
  "Addressed input reaches only the built-in subscription of its target."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "one-address")
      (e-board-add-participant board :id "two" :create-pickup-subscription-id "two-address")
      (e-board-subscribe board "two" '(:tags (main)) :id "two-main")
      (let* ((publication (e-board-post-input
                           board :id "message" :to "one" :tags '(main)))
             (message (e-board-publication-message publication))
             (pickup (e-board-pickup board (car (e-board-publication-pickup-ids
                                                 publication)))))
        (should (eq (e-board-publication-status publication) 'posted))
        (should (equal (e-board-message-matching-participant-ids message)
                       '("one")))
        (should (equal (e-board-pickup-participant-id pickup) "one"))
        (should (equal (e-board-pickup-subscription-ids pickup)
                       '("one-address")))))))

(ert-deftest e-board-test-tag-routes-once-per-participant-and-freezes-matches ()
  "Tag routing broadcasts while coalescing several matching subscriptions."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "one-address")
      (e-board-add-participant board :id "two" :create-pickup-subscription-id "two-address")
      (e-board-subscribe board "one" '(:tags (main)) :id "one-main")
      (e-board-subscribe board "one" '(:tags-all (main)) :id "one-main-again")
      (e-board-subscribe board "two" '(:tags-any (main other)) :id "two-main")
      (let* ((publication (e-board-post-input board :id "message" :tags '(main)))
             (message (e-board-publication-message publication))
             (one (e-board-pickup board (list "board" "message" "one"))))
        (should (equal (e-board-message-matching-participant-ids message)
                       '("one" "two")))
        (should (= (length (e-board-publication-pickup-ids publication)) 2))
        (should (equal (e-board-pickup-subscription-ids one)
                       '("one-main" "one-main-again")))
        ;; A later subscription cannot receive a message whose match set froze.
        (e-board-subscribe board "two" '(:tags (main)) :id "late")
         (should-not (member "late" (e-board-pickup-subscription-ids one)))))))

(ert-deftest e-board-test-condition-selector-matches-author-and-attributes ()
  "Generic routes conjunctively match immutable author and attributes."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (e-board-subscribe board "one"
                         '(:tags (main) :author "client:local"
                           :attributes (:priority high))
                         :id "condition")
      (should-not (e-board-publication-pickup-ids
                   (e-board-post-input board :tags '(main) :author "client:other"
                                       :attributes '(:priority high))))
      (should (= (length (e-board-publication-pickup-ids
                          (e-board-post-input board :tags '(main) :author "client:local"
                                              :attributes '(:priority high))))
                  1)))))

(ert-deftest e-board-test-predicate-faults-only-its-subscription ()
  "A trusted predicate error is visible and cannot abort other routes."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (e-board-subscribe board "one" '(:tags (main)) :id "working")
      (e-board-subscribe board "one"
                         '(:tags (main) :predicate (lambda (_message) (error "bad")))
                         :id "faulting")
      (let ((publication (e-board-post-input board :tags '(main))))
        (should (= (length (e-board-publication-pickup-ids publication)) 1))
        (should (eq (e-board-subscription-state
                     (e-board-find-subscription board "faulting"))
                    'faulted))
        (should (member 'subscription-faulted
                        (mapcar #'e-board-event-type (e-board-events board))))))))

(ert-deftest e-board-test-unrouted-inputs-and-monotonic-event-log ()
  "Unmatched input remains inspectable and every mutation advances sequence."
  (e-board-test--with-empty-registry
    (let* ((board (e-board-create :id "board"))
           (publication (e-board-post-input board :id "message" :tags '(main)))
           (message (e-board-publication-message publication))
           (events (e-board-events board)))
      (should (eq (e-board-publication-status publication) 'posted))
      (should (eq (e-board-message-unrouted-reason message)
                  'no-matching-subscription))
      (should (equal (e-board-unrouted-inputs board) (list message)))
      (should (equal (mapcar #'e-board-event-seq events) '(1 2)))
      (should (equal (mapcar #'e-board-event-type events)
                     '(input-posted input-unrouted))))))

(ert-deftest e-board-test-input-routing-is-deferred-and-freezes-subscriptions ()
  "Append queues a frozen view; later subscriptions cannot route its message."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        routers)
    (let ((board (e-board-create
                  :id "board"
                  :input-classification-scheduler
                  (lambda (drain) (push drain routers)))))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((publication (e-board-post-input board :id "input" :tags '(main)))
             (message (e-board-publication-message publication)))
        (should-not (e-board-publication-pickup-ids publication))
        (should-not (e-board-message-matching-participant-ids message))
        (e-board-subscribe board "one" '(:tags (main)) :id "too-late")
        (should (= (length routers) 1))
        (funcall (pop routers))
        (should (eq (e-board-message-unrouted-reason message)
                    'no-matching-subscription))
        (should-not (e-board-publication-pickup-ids publication))))))

(ert-deftest e-board-test-source-key-retries-and-expired-history-do-not-route-twice ()
  "Retained source keys return their pickup; late keys remain explicit failures."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "one-address")
      (let* ((first (e-board-post-input
                     board :id "message" :to "one" :source-input-key '(chat 1 2)))
             (event-count (length (e-board-events board)))
             (retry (e-board-post-input
                     board :id "different-id" :to "one" :source-input-key '(chat 1 2)))
             (late (e-board-post-input board :to "one" :source-input-key '(chat 1 1))))
        (should (eq (e-board-publication-status retry) 'duplicate))
        (should (eq (e-board-publication-message first)
                    (e-board-publication-message retry)))
        (should (equal (e-board-publication-pickup-ids first)
                       (e-board-publication-pickup-ids retry)))
        (should (= event-count (length (e-board-events board))))
        (should (eq (e-board-publication-status late) 'source-history-expired))
        (should (= event-count (length (e-board-events board))))))))

(ert-deftest e-board-test-output-is-idempotent-and-never-routes ()
  "Output requires a source key and only appends an output event."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (should-error (e-board-post-output board :author "participant:one")
                    :type 'e-board-invalid-source-key)
      (let* ((first (e-board-post-output
                     board :id "output" :author "participant:one"
                     :source-output-key '(one 1 1)))
             (retry (e-board-post-output
                     board :id "other" :author "participant:one"
                     :source-output-key '(one 1 1)))
             (message (e-board-publication-message first)))
        (should (eq (e-board-publication-status retry) 'duplicate))
        (should (eq (e-board-message-kind message) 'output))
        (should-not (e-board-message-pickup-ids message))
        (should (equal (mapcar #'e-board-event-type (e-board-events board))
                        '(output-posted)))))))

(ert-deftest e-board-test-activity-and-fact-are-idempotent-and-never-route ()
  "Only input messages create pickups; activity and facts stay observable."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((activity
              (e-board-post-activity
               board :author "participant:one" :subject-participant-id "one"
               :source-turn-id "turn" :activity-kind 'thinking :tags '(main)
               :source-activity-key '(one 1 1)))
             (activity-retry
              (e-board-post-activity
               board :author "participant:one" :subject-participant-id "one"
               :source-turn-id "turn" :activity-kind 'thinking
               :source-activity-key '(one 1 1)))
             (fact (e-board-post-fact board :author "producer:cron"
                                      :source-fact-key '(cron 1 1))))
        (should (eq (e-board-publication-status activity-retry) 'duplicate))
        (should (eq (e-board-message-kind (e-board-publication-message activity))
                    'activity))
        (should (equal (e-board-message-source-turn-id
                        (e-board-publication-message activity))
                       "turn"))
        (should (eq (e-board-message-kind (e-board-publication-message fact))
                    'fact))
        (should-not (e-board-publication-pickup-ids activity))
        (should-not (e-board-publication-pickup-ids fact))
        (should-error (e-board-post-activity
                       board :author "participant:one" :subject-participant-id "two"
                       :source-turn-id "turn" :activity-kind 'thinking
                       :source-activity-key '(one 1 2))
                      :type 'e-board-invalid-activity)))))

(ert-deftest e-board-test-observer-cursor-pages-without-routing-or-consuming ()
  "Client observation uses the common selector fields without side effects."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (e-board-subscribe board "one" '(:tags (main)) :id "main")
      (let ((observer (e-board-observer-subscribe
                       board "client" '(:tags (main)) :id "observer")))
        (e-board-post-input board :id "input" :tags '(main))
        (e-board-post-activity
         board :id "activity" :author "participant:one"
         :subject-participant-id "one" :source-turn-id "turn"
         :activity-kind 'thinking :tags '(main)
         :source-activity-key '(one 1 1))
        (e-board-post-fact board :id "fact" :tags '(other)
                           :source-fact-key '(producer 1 1))
        (let ((first-page (e-board-observer-read-page board "observer" :limit 1))
              (second-page (e-board-observer-read-page board "observer" :limit 2)))
          (should (equal (mapcar #'e-board-message-id first-page) '("input")))
          (should (equal (mapcar #'e-board-message-id second-page) '("activity")))
          (should (= (e-board-observer-next-seq observer) 7))
          ;; Observation does not affect input pickup state or routedness.
          (should (eq (e-board-pickup-state
                       (e-board-pickup board '("board" "input" "one")))
                      'pending)))))))

(ert-deftest e-board-test-enrolled-work-publishes-before-exact-invocation-effect ()
  "Cheap work publishes its terminal fact before its deferred exact reply."
  (e-board-test--with-empty-registry
    (let (effects routers replies)
      (let* ((board (e-board-create
                     :id "board"
                     :effect-scheduler
                     (lambda (effect) (push effect effects))
                     :terminal-classification-scheduler
                     (lambda (drain) (push drain routers))
                     :invocation-effect-dispatcher
                     (lambda (_board target state payload)
                       (should (equal target "turn-1/call-1"))
                       (push (list state payload) replies))))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "cheap" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done")) nil)))
        (e-board-enroll-work board handle)
        (e-board-subscribe-invocation
         board (e-work-handle-id handle)
         "turn-1/call-1"
         :id "turn-1/call-1")
        (should (equal (e-board-invocation-effect-target
                        (e-board-invocation board "turn-1/call-1"))
                       "turn-1/call-1"))
        (e-work-start-prepared handle)
        (should-not replies)
        (should (eq (e-board-work-state
                     (e-board-observed-work board (e-work-handle-id handle)))
                    'finished))
        (should (equal (mapcar #'e-board-event-type (e-board-events board))
                       '(posted subscription-added finished)))
        (should (= (length routers) 1))
        (funcall (pop routers))
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (equal replies '((finished "done"))))
        (should (eq (e-board-invocation-state
                     (e-board-invocation board "turn-1/call-1"))
                    'committed))
        (should (equal (mapcar #'e-board-event-type (e-board-events board))
                       '(posted subscription-added finished activation-prepared
                                 effect-committed)))))))

(ert-deftest e-board-test-aggregation-replies-after-all-observed-work-settles ()
  "Ordered aggregation waits for every watched terminal board fact."
  (e-board-test--with-empty-registry
    (let (effects routers reasons)
      (let* ((board (e-board-create :id "board"
                                    :effect-scheduler
                                    (lambda (effect) (push effect effects))
                                    :invocation-effect-dispatcher
                                    (lambda (_board target state reason)
                                      (should (equal target "turn-1/call-1"))
                                      (should (eq state 'aggregation))
                                      (push reason reasons))
                                    :terminal-classification-scheduler
                                    (lambda (drain) (push drain routers))))
             (first (e-work-prepare
                     (e-work-spec-create
                      :id "first" :execution 'render :interactive-policy 'async
                      :runner (lambda (_arguments _context) :never)) nil))
             (second (e-work-prepare
                      (e-work-spec-create
                       :id "second" :execution 'render :interactive-policy 'async
                       :runner (lambda (_arguments _context) :never)) '(:delay 600))))
        (e-board-enroll-work board first)
        (e-board-enroll-work board second)
        (e-board-subscribe-aggregation
         board (list (e-work-handle-id first) (e-work-handle-id second)) 'all
         "turn-1/call-1" :id "turn-1/call-1")
        (e-work-start-prepared first)
        (e-work-start-prepared second)
        (e-work-finish first "one")
        (should-not effects)
        (e-work-finish second "two")
        (should (= (length routers) 1))
        (funcall (pop routers))
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (equal reasons '(complete)))
        (should (eq (e-board-aggregation-state
                     (e-board-aggregation board "turn-1/call-1"))
                    'committed))))))

(ert-deftest e-board-test-aggregation-timeout-does-not-cancel-work ()
  "Aggregation timeout closes only its deferred reply subscription."
  (e-board-test--with-empty-registry
    (let (effects reasons)
      (let* ((board (e-board-create :id "board"
                                    :effect-scheduler
                                    (lambda (effect) (push effect effects))
                                    :invocation-effect-dispatcher
                                    (lambda (_board target state reason)
                                      (should (equal target "timeout"))
                                      (should (eq state 'aggregation))
                                      (push reason reasons))))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "pending" :execution 'render :interactive-policy 'async
                       :runner (lambda (_arguments _context) :never)) '(:delay 600))))
        (e-board-enroll-work board handle)
        (e-board-subscribe-aggregation
         board (list (e-work-handle-id handle)) 'any
         "timeout" :timeout 0.01)
        (e-work-start-prepared handle)
        (sleep-for 0.05)
        (funcall (pop effects))
        (should (equal reasons '(timed-out)))
        (should (eq (plist-get (e-work-status handle) :state) 'started))))))

(ert-deftest e-board-test-ordinary-subscription-lifecycle-preserves-address-route ()
  "Muting or cancelling an ordinary route cannot alter exact addressing."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "participant"
                               :create-pickup-subscription-id "address")
      (e-board-subscribe board "participant" '(:tags (main)) :id "main")
      (e-board-set-subscription-state board "main" 'muted)
      (should (eq (e-board-subscription-state
                   (e-board-find-subscription board "main")) 'muted))
      (should (eq (e-board-message-unrouted-reason
                   (e-board-publication-message
                    (e-board-post-input board :tags '(main))))
                  'no-matching-subscription))
      (e-board-set-subscription-state board "main" 'active)
      (should (= (length (e-board-publication-pickup-ids
                          (e-board-post-input board :tags '(main)))) 1))
      (e-board-set-subscription-state board "main" 'cancelled)
      (should-error (e-board-set-subscription-state board "main" 'active)
                    :type 'e-board-error)
      (should-error (e-board-set-subscription-state board "address" 'muted)
                     :type 'e-board-error))))

(ert-deftest e-board-test-ordinary-subscription-terminal-states-are-one-way ()
  "Ordinary subscriptions expose the complete terminal lifecycle."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "participant"
                               :create-pickup-subscription-id "address")
      (e-board-subscribe board "participant" '(:tags (main)) :id "expired")
      (e-board-set-subscription-state board "expired" 'expired)
      (should (eq (e-board-subscription-state
                   (e-board-find-subscription board "expired")) 'expired))
      (should-error (e-board-set-subscription-state board "expired" 'active)
                    :type 'e-board-error)
      (e-board-subscribe board "participant" '(:tags (complete)) :id "completed")
      (e-board-set-subscription-state board "completed" 'completed)
      (should-error (e-board-set-subscription-state board "completed" 'cancelled)
                    :type 'e-board-error)
      (e-board-subscribe board "participant"
                         '(:tags (fault) :predicate (lambda (_message) (error "bad")))
                         :id "faulted")
      (e-board-post-input board :tags '(fault))
      (should (eq (e-board-subscription-state
                   (e-board-find-subscription board "faulted")) 'faulted)))))

(ert-deftest e-board-test-replacing-subscription-cancels-old-future-route ()
  "Changing a matcher installs a new future-only subscription identity."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "participant"
                               :create-pickup-subscription-id "address")
      (e-board-subscribe board "participant" '(:tags (old)) :id "old")
      (let ((replacement (e-board-replace-subscription
                          board "old" '(:tags (new)) :id "new")))
        (should (equal (e-board-subscription-id replacement) "new"))
        (should (eq (e-board-subscription-state
                     (e-board-find-subscription board "old")) 'cancelled))
        (should-not (e-board-publication-pickup-ids
                     (e-board-post-input board :tags '(old))))
        (should (= (length (e-board-publication-pickup-ids
                            (e-board-post-input board :tags '(new)))) 1))
        (should (member 'subscription-replaced
                        (mapcar #'e-board-event-type (e-board-events board))))))))

(ert-deftest e-board-test-post-input-effect-creates-a-deferred-derived-message ()
  "A matching continuation posts through the board after routing commits."
  (e-board-test--with-empty-registry
    (let (effects)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler
                                   (lambda (effect) (push effect effects)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe
         board "one" '(:tags (source))
         :id "continuation"
         :effect '(:post-input :to "one" :content "derived"))
        (e-board-post-input board :id "source" :tags '(source) :content "original")
        (should (= (length effects) 1))
        (funcall (pop effects))
        (let* ((derived (car (last (e-board-messages board))))
               (pickup (e-board-pickup board (car (e-board-message-pickup-ids derived)))))
          (should (equal (e-board-message-content derived) "derived"))
           (should (equal (e-board-message-to derived) "one"))
           (should (equal (e-board-pickup-participant-id pickup) "one")))))))

(ert-deftest e-board-test-post-input-effect-does-not-reenter-its-lineage ()
  "A continuation cannot schedule itself from its own derived input."
  (e-board-test--with-empty-registry
    (let (effects)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler
                                   (lambda (effect) (push effect effects)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe
         board "one" '(:tags (source)) :id "loop"
         :effect '(:post-input :tags (source) :content "derived"))
        (e-board-post-input board :tags '(source) :content "original")
        (funcall (pop effects))
        (should-not effects)
        (should (= (length (e-board-messages board)) 2))))))

(provide 'e-board-test)

;;; e-board-test.el ends here

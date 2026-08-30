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

(ert-deftest e-board-test-processing-record-reservation-rejects-reentrant-commit ()
  "A persistence callback cannot commit the record ID its outer call owns."
  (e-board-test--with-empty-registry
    (let (reentrant-error)
      (let ((board
             (e-board-create
              :id "board"
              :processing-record-notification-function
              (lambda (callback-board _record _type)
                (condition-case error
                    (e-board-record-processing-chain
                     callback-board :id "chain" :root-message-id "root"
                     :candidate-message-id "reentrant"
                     :caused-by-message-id "root" :processor-history nil
                     :processing-depth 0)
                  (e-board-id-conflict
                   (setq reentrant-error error)))))))
        (e-board-record-processing-chain
         board :id "chain" :root-message-id "root"
         :candidate-message-id "outer" :caused-by-message-id "root"
         :processor-history nil :processing-depth 0)
        (should reentrant-error)
        (should (equal (mapcar #'e-board-processing-chain-id
                               (e-board-list-processing-chains board))
                       '("chain")))
        (should (= (cl-count 'processing-chain
                            (mapcar #'e-board-event-type (e-board-events board)))
                   1))))))

(ert-deftest e-board-test-processing-record-rejects-cross-record-reentrancy ()
  "A callback cannot publish a later record before its durable predecessor."
  (e-board-test--with-empty-registry
    (let (reentrant-error)
      (let ((board
             (e-board-create
              :id "board"
              :processing-record-notification-function
              (lambda (callback-board _record _type)
                (condition-case error
                    (e-board-record-processing-chain
                     callback-board :id "nested" :root-message-id "root"
                     :candidate-message-id "nested" :caused-by-message-id "root"
                     :processor-history nil :processing-depth 0)
                  (e-board-id-conflict
                   (setq reentrant-error error)))))))
        (e-board-record-processing-chain
         board :id "outer" :root-message-id "root"
         :candidate-message-id "outer" :caused-by-message-id "root"
         :processor-history nil :processing-depth 0)
        (should reentrant-error)
        (should (equal (mapcar #'e-board-processing-chain-id
                               (e-board-list-processing-chains board))
                       '("outer")))
        (should (equal (mapcar #'e-board-event-type (e-board-events board))
                       '(processing-chain)))))))

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

(ert-deftest e-board-test-import-reserves-fallback-message-identities ()
  "A restored board allocates new messages after imported numeric ids."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-import-message
       board '(:id "msg_7" :kind output :author "test"
               :tags (main) :content "restored"
               :source-output-key (test 1 1)))
      (let ((message
             (e-board-publication-message
              (e-board-post-input board :author "test" :content "new"))))
        (should (equal (e-board-message-id message) "msg_8"))))))

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
        (should (eq (e-board-message-routing-state message) 'routed))
        (should (equal (e-board-message-matching-participant-ids message)
                       '("one")))
        (should (equal (e-board-pickup-participant-id pickup) "one"))
        (should (equal (e-board-pickup-subscription-ids pickup)
                       '("one-address")))))))

(ert-deftest e-board-test-attribute-grammar-routes-valid-plist-and-alist ()
  "The board-owned attribute grammar routes both supported top-level forms."
  (e-board-test--with-empty-registry
    (dolist (case
             (list
              (list "nil" nil)
              (list "plist"
                    '(:kind "ordinary"
                      :nested (car "car" (:inside [car "car"])))
                    '(:kind "ordinary"
                      :nested (car "car" (:inside [car "car"]))))
              (list "alist"
                    '((:kind . "ordinary")
                      (:nested . (car "car" (:inside [car "car"]))))
                    '(:kind "ordinary"
                      :nested (car "car" (:inside [car "car"]))))))
      (pcase-let ((`(,name ,selector-attributes ,message-attributes) case))
        (should (e-board-selector-attributes-valid-p selector-attributes))
        (let* ((board (e-board-create :id (concat "attribute-" name)))
               (_participant
                (e-board-add-participant
                 board :id "participant" :create-pickup-subscription-id
                 "address"))
               (_subscription
                (e-board-subscribe
                 board "participant" (list :attributes selector-attributes)
                 :id "attribute-selector"))
               (publication
                (e-board-post-input
                 board :id "attribute-input" :attributes message-attributes
                 :content "attribute input"))
               (message (e-board-publication-message publication)))
          (should (eq (e-board-message-routing-state message) 'routed))
          (should (equal (e-board-message-matching-participant-ids message)
                         '("participant")))
          (should (= (length (e-board-message-pickup-ids message)) 1))
          (should (equal
                   (e-board-pickup-participant-id
                    (e-board-pickup
                     board (car (e-board-message-pickup-ids message))))
                   "participant")))))))

(ert-deftest e-board-test-attribute-grammar-rejects-non-clause-shapes ()
  "The board matcher rejects unsupported top-level attribute shapes early."
  (dolist (attributes
           (list [car "car"]
                 '(car "car")
                 '(:kind "ordinary" :odd)
                 '((:kind . "ordinary") ("kind" . "bad"))))
    (should-not (e-board-selector-attributes-valid-p attributes))))

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

(ert-deftest e-board-test-pickup-freezes-logical-delivery-envelope ()
  "A pickup owns immutable payload, provenance, and causal routing metadata."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one"
                               :create-pickup-subscription-id "address")
      (e-board-subscribe board "one" '(:tags (main)) :id "main")
      (let* ((publication
              (e-board-post-input
               board :id "message" :requester-actor "principal"
               :tags '(main)
               :attributes '(:board-subscription-lineage (origin)
                             :board-subscription-source-message-ids (source))
               :content "frozen" :reference '(:resource "board://message")
               :source-input-key '(producer 3 7)))
             (message (e-board-publication-message publication))
             (pickup (e-board-pickup board '("board" "message" "one"))))
        (should (equal (e-board-pickup-event-seq-range pickup)
                       (list (e-board-message-seq message)
                             (e-board-message-seq message))))
        (should (equal (e-board-pickup-subscription-ids pickup) '("main")))
        (should (equal (e-board-pickup-requester-actor pickup) "principal"))
        (should (equal (e-board-pickup-content pickup) "frozen"))
        (should (equal (e-board-pickup-reference pickup)
                       '(:resource "board://message")))
        (should
         (equal (e-board-pickup-cause-metadata pickup)
                '(:reply-to-message-ids nil
                  :caused-by-delivery-ids nil
                  :source-input-key (producer 3 7)
                  :routing-tags (main)
                  :input-attributes
                  (:board-subscription-lineage (origin)
                   :board-subscription-source-message-ids (source))
                  :subscription-lineage (origin)
                  :source-message-ids (source))))
        ;; The message is public audit state; later accidental mutation cannot
        ;; rewrite the already-routed logical delivery bytes or provenance.
        (setf (e-board-message-content message) "changed"
              (e-board-message-requester-actor message) "changed"
              (e-board-message-reference message) nil
              (e-board-message-attributes message) nil)
        (should (equal (e-board-pickup-content pickup) "frozen"))
        (should (equal (e-board-pickup-requester-actor pickup) "principal"))
        (should (equal (e-board-pickup-reference pickup)
                       '(:resource "board://message")))
        (should (equal (plist-get (e-board-pickup-cause-metadata pickup)
                                  :subscription-lineage)
                       '(origin)))))))

(ert-deftest e-board-test-retained-message-deep-copies-all-mutable-envelope-values ()
  "Caller mutation cannot rewrite any retained message or pickup envelope."
  (e-board-test--with-empty-registry
    (let* ((board (e-board-create :id "board"))
           (content (list (copy-sequence "before") ["vector"]))
           (reference (list :resource (copy-sequence "board://before")))
           (tags (list (copy-sequence "main")))
           (attributes (list :nested (list (copy-sequence "value"))))
           (requester (list 'participant (copy-sequence "source")))
           (causes (list (vector (copy-sequence "delivery")))))
      (e-board-add-participant board :id "one"
                               :create-pickup-subscription-id "address")
      (let* ((publication
              (e-board-post-input
               board :id "message" :to "one" :requester-actor requester
               :tags tags :attributes attributes :content content
               :reference reference))
             (message (e-board-publication-message publication))
             (pickup (e-board-pickup board '("board" "message" "one")))
             (activity
              (e-board-publication-message
               (e-board-post-activity
                board :id "activity" :author "participant:one"
                :subject-participant-id "one" :source-turn-id "turn"
                :activity-kind 'reasoning :source-activity-key '(one 1 1)
                :caused-by-delivery-ids causes))))
        (aset (car content) 0 ?X)
        (aset (cadr content) 0 "changed")
        (aset (plist-get reference :resource) 0 ?X)
        (aset (car tags) 0 ?X)
        (aset (car (plist-get attributes :nested)) 0 ?X)
        (aset (cadr requester) 0 ?X)
        (aset (aref (car causes) 0) 0 ?X)
        (should (equal (e-board-message-content message)
                       '("before" ["vector"])))
        (should (equal (e-board-message-reference message)
                       '(:resource "board://before")))
        (should (equal (e-board-message-tags message) '("main")))
        (should (equal (e-board-message-attributes message)
                       '(:nested ("value"))))
        (should (equal (e-board-message-requester-actor message)
                       '(participant "source")))
        (should (equal (e-board-message-caused-by-delivery-ids activity)
                       '(["delivery"])))
        (should (equal (e-board-pickup-content pickup)
                       '("before" ["vector"])))))))

(ert-deftest e-board-test-message-envelope-rejects-oversized-and-cyclic-values ()
  "Retained message fields fail closed at their fixed byte and shape bounds."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board"))
          (cycle (list 'cycle)))
      (setcdr cycle cycle)
      (should-error
       (e-board-post-fact
        board :source-fact-key '(test 1 1)
        :content (make-string (1+ e-board-message-content-byte-limit) ?x))
       :type 'e-board-envelope-too-large)
      (should-error (e-board-post-fact board :source-fact-key '(test 1 2)
                                             :content cycle)
                    :type 'e-board-invalid-envelope)
      (should-not (e-board-messages board)))))

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
      (should (eq (e-board-message-routing-state message) 'unrouted))
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
        (should (eq (e-board-message-routing-state message) 'routing))
        (e-board-subscribe board "one" '(:tags (main)) :id "too-late")
        (should (= (length routers) 1))
        (funcall (pop routers))
        (should (eq (e-board-message-unrouted-reason message)
                    'no-matching-subscription))
        (should (eq (e-board-message-routing-state message) 'unrouted))
        (should-not (e-board-publication-pickup-ids publication))))))

(ert-deftest e-board-test-routing-finalization-pages-and-rejects-fanout-over-cap ()
  "Classification yields between pages and never exposes a partial over-cap fan-out."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0))
    (let ((e-board-input-classification-drain-limit 8)
          scheduled)
      (let ((board
             (e-board-create
              :id "board"
              :input-classification-scheduler
              (lambda (drain) (setq scheduled (append scheduled (list drain)))))))
        (dotimes (index (1+ e-board-input-fanout-limit))
          (let ((participant-id (format "p%d" index)))
            (e-board-add-participant
             board :id participant-id
             :create-pickup-subscription-id (format "exact-%d" index))
            (e-board-subscribe board participant-id '(:tags (main))
                               :id (format "tag-%d" index))))
        (let* ((publication (e-board-post-input board :id "input" :tags '(main)))
               (message (e-board-publication-message publication))
               (heartbeats 0))
          (while scheduled
            (let ((drain (pop scheduled)))
              (cl-incf heartbeats)
              (funcall drain)
              (unless (eq (e-board-message-routing-state message) 'routing-failed)
                (should (eq (e-board-message-routing-state message) 'routing))
                (should (= (hash-table-count (e-board-pickups board)) 0)))))
          (should (> heartbeats 2))
          (should (eq (e-board-message-routing-state message) 'routing-failed))
          (should (eq (e-board-message-unrouted-reason message)
                      'fanout-limit-exceeded))
          (should-not (e-board-publication-pickup-ids publication))
          (should (= (hash-table-count (e-board-pickups board)) 0)))))))

(ert-deftest e-board-test-core-routing-fault-is-visible-without-partial-pickups ()
  "A malformed trusted selector fails one routing attempt before commit."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (e-board-subscribe board "one" '(:tags (main)) :id "working")
      ;; Attribute syntax is core-owned matching schema, unlike a predicate
      ;; fault which only terminally faults its own subscription.
      (e-board-subscribe board "one" '(:tags (main) :attributes invalid)
                         :id "malformed")
      (let* ((publication (e-board-post-input board :tags '(main)))
             (message (e-board-publication-message publication)))
        (should (eq (e-board-message-routing-state message) 'routing-failed))
        (should-not (e-board-message-pickup-ids message))
        (should (member 'input-routing-failed
                        (mapcar #'e-board-event-type (e-board-events board))))))))

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

(ert-deftest e-board-test-open-activity-projection-closes-on-terminal-or-output ()
  "The board tracks latest active work and suppresses it at either terminal edge."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-post-activity
       board :id "progress" :author "participant:one" :subject-participant-id "one"
       :source-turn-id "turn-one" :activity-kind 'thinking
       :source-activity-key '(one 1 1))
      (let ((open (e-board-open-activity board "one" "turn-one")))
        (should (equal (e-board-open-activity-message-id open) "progress"))
        (should (eq (e-board-open-activity-activity-kind open) 'thinking)))
      (e-board-post-output
       board :id "output" :author "participant:one" :subject-participant-id "one"
       :source-turn-id "turn-one" :source-output-key '(one 1 1))
      (should-not (e-board-open-activity board "one" "turn-one"))
      (e-board-post-activity
       board :id "late-progress" :author "participant:one" :subject-participant-id "one"
       :source-turn-id "turn-one" :activity-kind 'work-progress
       :source-activity-key '(one 1 2))
      (should-not (e-board-open-activity board "one" "turn-one"))
      (e-board-post-activity
       board :id "progress-two" :author "participant:one" :subject-participant-id "one"
       :source-turn-id "turn-two" :activity-kind 'work-progress
       :source-activity-key '(one 1 2))
      (e-board-post-activity
       board :id "terminal" :author "participant:one" :subject-participant-id "one"
       :source-turn-id "turn-two" :activity-kind 'turn-failed
       :source-activity-key '(one 1 3))
      (should-not (e-board-open-activity board "one" "turn-two"))
      (e-board-post-activity
       board :id "late-progress-two" :author "participant:one" :subject-participant-id "one"
       :source-turn-id "turn-two" :activity-kind 'work-progress
       :source-activity-key '(one 1 4))
      (should-not (e-board-open-activity board "one" "turn-two")))))

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
                      'ready)))))))

(ert-deftest e-board-test-observer-default-live-cursor-starts-after-high-watermark ()
  "A new observer sees future live messages unless it explicitly requests history."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-post-fact board :id "old" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (let ((observer (e-board-observer-subscribe board "client" '(:tags (main)))))
        (should (= (e-board-observer-next-seq observer) 1))
        (e-board-post-fact board :id "new" :tags '(main)
                           :source-fact-key '(producer 1 2))
        (should (equal (mapcar #'e-board-message-id
                               (e-board-observer-read-page board
                                                           (e-board-observer-id observer)))
                       '("new")))))))

(ert-deftest e-board-test-observer-page-advances-only-after-acceptance ()
  "Prepared client pages leave the live cursor unchanged until acknowledged."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (let ((observer (e-board-observer-subscribe
                       board "client" '(:tags (main)) :id "observer" :start-seq 0)))
        (e-board-post-fact board :id "first" :tags '(main)
                           :source-fact-key '(producer 1 1))
        (e-board-post-fact board :id "second" :tags '(main)
                           :source-fact-key '(producer 1 2))
        (let ((page (e-board-observer-prepare-page board "observer" :limit 1)))
          (should (equal (mapcar #'e-board-message-id (plist-get page :messages))
                         '("first")))
          (should (= (e-board-observer-next-seq observer) 0))
          (let ((repeated (e-board-observer-prepare-page
                           board "observer" :limit 2)))
            (should-not (eq page repeated))
            (should (equal (plist-get page :receipt)
                           (plist-get repeated :receipt)))
            (should (equal (mapcar #'e-board-message-id
                                   (plist-get repeated :messages))
                           '("first")))
            (setcar (plist-get repeated :receipt) "tampered")
            (should (equal (plist-get page :receipt)
                           (plist-get (e-board-observer-prepare-page
                                       board "observer" :limit 2)
                                      :receipt))))
          (should-error
           (e-board-observer-accept-page board "observer" '(fabricated receipt))
           :type 'e-board-error)
          (e-board-observer-accept-page board "observer" (plist-get page :receipt))
          (e-board-observer-accept-page board "observer" (plist-get page :receipt))
          (should (= (e-board-observer-next-seq observer)
                     (plist-get page :through-seq)))
          (should (= (cl-count 'observer-page-accepted
                               (mapcar #'e-board-event-type (e-board-events board)))
                     1)))
        (should (equal (mapcar #'e-board-message-id
                               (e-board-observer-read-page board "observer"))
                       '("second")))))))

(ert-deftest e-board-test-observer-transition-releases-prepared-page-pins ()
  "Muting an observer fences and releases its unaccepted live/history pages."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-post-fact board :id "fact" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (let* ((observer (e-board-observer-subscribe
                        board "client" '(:tags (main)) :id "observer"
                        :start-seq 0 :history-before-seq 2))
             (live (e-board-observer-prepare-page board "observer"))
             (history (e-board-observer-prepare-history-page board "observer")))
        (should (e-board-observer-prepared-page observer))
        (should (e-board-observer-prepared-history-page observer))
        (e-board-set-observer-state board "observer" 'muted)
        (should-not (e-board-observer-prepared-page observer))
        (should-not (e-board-observer-prepared-history-page observer))
        (should-error
         (e-board-observer-accept-page board "observer" (plist-get live :receipt))
         :type 'e-board-error)
        (should-error
         (e-board-observer-accept-history-page
          board "observer" (plist-get history :receipt))
         :type 'e-board-error)))))

(ert-deftest e-board-test-pickups-preserve-participant-fifo-order ()
  "Only a participant's oldest pickup may become deliverable."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((first (e-board-post-input board :id "first" :to "one"))
             (second (e-board-post-input board :id "second" :to "one"))
             (first-id (car (e-board-publication-pickup-ids first)))
             (second-id (car (e-board-publication-pickup-ids second))))
        (should (eq (e-board-pickup-state (e-board-pickup board first-id)) 'ready))
        (should (eq (e-board-pickup-state (e-board-pickup board second-id)) 'pending))
        (should-error (e-board-pickup-start-delivery board second-id)
                      :type 'e-board-error)
        (e-board-pickup-start-delivery board first-id)
        (e-board-pickup-return-ready board first-id '(error "temporary"))
        (should (eq (e-board-pickup-state (e-board-pickup board first-id)) 'ready))
        (should (eq (e-board-pickup-state (e-board-pickup board second-id)) 'pending))
        (e-board-pickup-start-delivery board first-id)
        (should (equal (e-board-pickup-complete-delivery board first-id) second-id))
        (should (eq (e-board-pickup-state (e-board-pickup board first-id)) 'consumed))
        (should (eq (e-board-pickup-state (e-board-pickup board second-id)) 'ready))))))

(ert-deftest e-board-test-physical-attempt-binding-is-replaceable-only-after-proof ()
  "Endpoint binding is outside the logical envelope and obeys commit proof."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((delivery-id
              (car (e-board-publication-pickup-ids
                    (e-board-post-input board :id "input" :to "one"
                                        :content '(immutable bytes)))))
             (pickup (e-board-pickup board delivery-id))
             (token (vector 'endpoint "old")))
        (e-board-pickup-start-delivery board delivery-id token '(3 7))
        (let ((first (e-board-pickup-attempt pickup)))
          (should (= (e-board-delivery-attempt-number first) 1))
          (should (equal (e-board-delivery-attempt-endpoint-token first)
                         token))
          (should (equal (e-board-delivery-attempt-composite-generation first)
                         '(3 7)))
          (aset token 1 "mutated")
          (should (equal (e-board-delivery-attempt-endpoint-token first)
                         [endpoint "old"]))
          (should (equal (e-board-pickup-content pickup) '(immutable bytes)))
          (should-error
           (e-board-pickup-start-delivery board delivery-id [endpoint "new"] '(4 8))
           :type 'e-board-error)
          (e-board-pickup-return-ready board delivery-id 'not-submitted)
          (should (eq (e-board-delivery-attempt-state first)
                      'proven-uncommitted)))
        (e-board-pickup-start-delivery board delivery-id [endpoint "new"] '(4 8))
        (let ((second (e-board-pickup-attempt pickup)))
          (should (= (e-board-delivery-attempt-number second) 2))
          (should (equal (e-board-delivery-attempt-endpoint-token second)
                         [endpoint "new"]))
          (should (equal (e-board-delivery-attempt-composite-generation second)
                         '(4 8)))
          (e-board-pickup-mark-uncertain board delivery-id 'lost-ack)
          (should (eq (e-board-delivery-attempt-state second) 'uncertain))
          (should-error
           (e-board-pickup-start-delivery board delivery-id [endpoint "third"] '(5 9))
           :type 'e-board-error))))))

(ert-deftest e-board-test-cancelling-pickup-releases-only-its-fifo-position ()
  "Pending cancellation does not overtake work; head cancellation promotes once."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((first (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "first" :to "one"))))
             (second (car (e-board-publication-pickup-ids
                           (e-board-post-input board :id "second" :to "one"))))
             (third (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "third" :to "one")))))
        (should-not (e-board-cancel-pickup board second 'owner-cancelled))
        (should (eq (e-board-pickup-state (e-board-pickup board second)) 'cancelled))
        (should (equal (e-board-cancel-pickup board first 'owner-cancelled) third))
        (should (eq (e-board-pickup-state (e-board-pickup board third)) 'ready))))))

(ert-deftest e-board-test-uncertain-pickup-tombstones-and-releases-fifo-head ()
  "An ambiguous accepted pickup is not retried and releases only its successor."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((first (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "first" :to "one"))))
             (second (car (e-board-publication-pickup-ids
                           (e-board-post-input board :id "second" :to "one")))))
        (e-board-pickup-start-delivery board first)
        (e-board-pickup-accept-delivery board first)
        (should (equal (e-board-pickup-mark-uncertain board first 'lost-ack) second))
        (should (eq (e-board-pickup-state (e-board-pickup board first)) 'uncertain))
        (should (eq (e-board-pickup-state (e-board-pickup board second)) 'ready))
        (should-error (e-board-pickup-start-delivery board first)
                      :type 'e-board-error)))))

(ert-deftest e-board-test-cancelling-delivery-awaits-its-own-receipt ()
  "Consumption wins a fenced cancellation; a discard settles it cancelled."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((first (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "first" :to "one"))))
             (second (car (e-board-publication-pickup-ids
                           (e-board-post-input board :id "second" :to "one")))))
        (e-board-pickup-start-delivery board first)
        (e-board-pickup-accept-delivery board first 'accepted-receipt)
        (should-not (e-board-cancel-pickup board first 'owner-cancelled))
        (should (eq (e-board-pickup-state (e-board-pickup board first)) 'cancelling))
        (let ((attempt (e-board-pickup-attempt (e-board-pickup board first))))
          (should (eq (e-board-delivery-attempt-receipt attempt)
                      'accepted-receipt))
          (should (eq (e-board-delivery-attempt-reason attempt)
                      'owner-cancelled)))
        (should (equal (e-board-pickup-complete-delivery board first) second))
        (should (eq (e-board-pickup-state (e-board-pickup board first)) 'consumed))
        (should (eq (e-board-pickup-state (e-board-pickup board second)) 'ready))
        (e-board-pickup-start-delivery board second)
        (e-board-pickup-complete-delivery board second))
      (let* ((first (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "third" :to "one"))))
             (second (car (e-board-publication-pickup-ids
                           (e-board-post-input board :id "fourth" :to "one")))))
        (e-board-pickup-start-delivery board first)
        (e-board-pickup-accept-delivery board first)
        (e-board-cancel-pickup board first 'owner-cancelled)
        (should (equal (e-board-pickup-discard-delivery board first 'reset) second))
        (should (eq (e-board-pickup-state (e-board-pickup board first)) 'cancelled))
        (should (eq (e-board-pickup-state (e-board-pickup board second)) 'ready))))))

(ert-deftest e-board-test-pickup-pending-cap-overflows-without-blocking-head ()
  "A participant's bounded pending tail becomes an explicit tombstone."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board" :pickup-pending-limit 1)))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((first (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "first" :to "one"))))
             (second (car (e-board-publication-pickup-ids
                           (e-board-post-input board :id "second" :to "one"))))
             (third (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "third" :to "one")))))
        (should (eq (e-board-pickup-state (e-board-pickup board first)) 'ready))
        (should (eq (e-board-pickup-state (e-board-pickup board second)) 'pending))
        (should (eq (e-board-pickup-state (e-board-pickup board third)) 'overflowed))
        (should (equal (e-board--pickup-queue board "one") (list first second)))
        (e-board-pickup-start-delivery board first)
        (should (equal (e-board-pickup-complete-delivery board first) second))
        (should (eq (e-board-pickup-state (e-board-pickup board second)) 'ready))))))

(ert-deftest e-board-test-expiring-ready-pickup-releases-fifo-successor ()
  "Expiry is a visible head tombstone that promotes exactly one successor."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((first (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "first" :to "one"))))
             (second (car (e-board-publication-pickup-ids
                           (e-board-post-input board :id "second" :to "one")))))
        (should (equal (e-board-expire-pickup board first 'deadline) second))
        (should (eq (e-board-pickup-state (e-board-pickup board first)) 'expired))
        (should (eq (e-board-pickup-state (e-board-pickup board second)) 'ready))))))

(ert-deftest e-board-test-failing-delivering-pickup-releases-fifo-successor ()
  "A permanent adapter failure becomes a terminal head tombstone."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
      (let* ((first (car (e-board-publication-pickup-ids
                          (e-board-post-input board :id "first" :to "one"))))
             (second (car (e-board-publication-pickup-ids
                           (e-board-post-input board :id "second" :to "one")))))
        (e-board-pickup-start-delivery board first)
        (should (equal (e-board-fail-pickup board first 'permanent-rejection) second))
        (should (eq (e-board-pickup-state (e-board-pickup board first)) 'failed))
        (should (eq (e-board-pickup-state (e-board-pickup board second)) 'ready))))))

(ert-deftest e-board-test-observer-lifecycle-and-replacement-keep-cursors-local ()
  "Observer muting, terminal states, and replacement never route input."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (let ((observer (e-board-observer-subscribe
                       board "client" '(:tags (main)) :id "old")))
        (e-board-post-fact board :id "first" :tags '(main)
                           :source-fact-key '(producer 1 1))
        (should (equal (mapcar #'e-board-message-id
                               (e-board-observer-read-page board "old"))
                       '("first")))
        (e-board-set-observer-state board "old" 'muted)
        (should-not (e-board-observer-read-page board "old"))
        (let ((replacement (e-board-replace-observer
                            board "old" '(:tags (other)) :id "new")))
          (should (eq (e-board-observer-state observer) 'cancelled))
          (should (= (e-board-observer-next-seq replacement)
                     (e-board-observer-next-seq observer)))
          (e-board-post-fact board :id "second" :tags '(other)
                             :source-fact-key '(producer 1 2))
          (should (equal (mapcar #'e-board-message-id
                                 (e-board-observer-read-page board "new"))
                         '("second")))
          (e-board-set-observer-state board "new" 'expired)
          (should-error (e-board-set-observer-state board "new" 'active)
                        :type 'e-board-error))))))

(ert-deftest e-board-test-observer-history-does-not_advance_live_cursor ()
  "Reverse history and forward live observation retain independent cursors."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-post-fact board :id "one" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (e-board-post-fact board :id "two" :tags '(main)
                         :source-fact-key '(producer 1 2))
      (e-board-post-fact board :id "three" :tags '(main)
                         :source-fact-key '(producer 1 3))
      (let ((observer (e-board-observer-subscribe
                       board "client" '(:tags (main)) :id "observer"
                       :start-seq 3 :history-before-seq 4)))
        (should (equal (mapcar #'e-board-message-id
                               (e-board-observer-read-history-page board "observer" :limit 2))
                       '("two" "three")))
        (should (= (e-board-observer-next-seq observer) 3))
        (should (equal (mapcar #'e-board-message-id
                               (e-board-observer-read-page board "observer"))
                       nil))))))

(ert-deftest e-board-test-observer-history-page-advances-only-after-acceptance ()
  "A prepared history page leaves its reverse cursor unchanged until accepted."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-post-fact board :id "one" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (e-board-post-fact board :id "two" :tags '(main)
                         :source-fact-key '(producer 1 2))
      (let ((observer (e-board-observer-subscribe
                       board "client" '(:tags (main)) :id "observer"
                       :history-before-seq 3)))
        (let ((page (e-board-observer-prepare-history-page
                     board "observer" :limit 1)))
          (should (= (e-board-observer-history-before-seq observer) 3))
          (should (equal (mapcar #'e-board-message-id (plist-get page :messages))
                         '("two")))
          (e-board-observer-accept-history-page
           board "observer" (plist-get page :receipt))
          (should (= (e-board-observer-history-before-seq observer)
                     (plist-get page :before-seq))))))))

(ert-deftest e-board-test-observer-history-acceptance-retries-idempotently ()
  "A repeated history receipt preserves the first accepted reverse cursor."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-post-fact board :id "fact" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (let* ((observer (e-board-observer-subscribe
                        board "client" '(:tags (main)) :id "observer"
                        :history-before-seq 2))
             (page (e-board-observer-prepare-history-page board "observer" :limit 1))
             (receipt (plist-get page :receipt))
             (before-seq (plist-get page :before-seq)))
        (e-board-observer-accept-history-page board "observer" receipt)
        (should (= (e-board-observer-history-before-seq observer) before-seq))
        (e-board-observer-accept-history-page board "observer" receipt)
        (should (= (e-board-observer-history-before-seq observer) before-seq))
        (should (= (cl-count 'observer-history-page-accepted
                             (mapcar #'e-board-event-type (e-board-events board)))
                   1))))))

(ert-deftest e-board-test-observer-history-page-bounds-sparse-filter-inspection ()
  "A sparse history selector advances over one record-bounded page at a time."
  (e-board-test--with-empty-registry
    (let ((board (e-board-create :id "board")))
      (e-board-post-fact board :id "match" :tags '(main)
                         :source-fact-key '(producer 1 1))
      (e-board-post-fact board :id "latest-other" :tags '(other)
                         :source-fact-key '(producer 1 2))
      (let* ((observer (e-board-observer-subscribe
                        board "client" '(:tags (main)) :id "observer"
                        :history-before-seq 3))
             (first (e-board-observer-prepare-history-page
                     board "observer" :limit 1)))
        (should-not (plist-get first :messages))
        (should (= (plist-get first :before-seq) 2))
        (e-board-observer-accept-history-page
         board "observer" (plist-get first :receipt))
        (let ((second (e-board-observer-prepare-history-page
                       board "observer" :limit 1)))
          (should (equal (mapcar #'e-board-message-id (plist-get second :messages))
                         '("match")))
          (should (= (plist-get second :before-seq) 1)))))))

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
        (should (eq (e-board-activation-state
                     (e-board-activation board '("board" "turn-1/call-1" 1)))
                    'prepared))
        (funcall (pop effects))
        (should (equal replies '((finished "done"))))
        (should (eq (e-board-invocation-state
                     (e-board-invocation board "turn-1/call-1"))
                    'committed))
        (should (equal (mapcar #'e-board-event-type (e-board-events board))
                       '(posted subscription-added finished activation-prepared
                                 activation-applying effect-committed)))))))

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
                      :id "first" :execution 'cooperative :interactive-policy 'async
                      :runner (lambda (_handle _arguments _context) :deferred)) nil))
             (second (e-work-prepare
                      (e-work-spec-create
                       :id "second" :execution 'cooperative :interactive-policy 'async
                       :runner (lambda (_handle _arguments _context) :deferred)) nil)))
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
        (should (eq (e-board-activation-state
                     (e-board-activation board '("board" "turn-1/call-1" 1)))
                    'prepared))
        (funcall (pop effects))
        (should (equal reasons '(complete)))
        (should (eq (e-board-aggregation-state
                     (e-board-aggregation board "turn-1/call-1"))
                    'committed))
        (should (eq (e-board-activation-state
                     (e-board-activation board '("board" "turn-1/call-1" 1)))
                    'committed))))))

(ert-deftest e-board-test-exact-aggregation-readiness-filters-terminal-outcomes ()
  "Exact readiness policies react only to their declared terminal outcome."
  (e-board-test--with-empty-registry
    (let (effects routers replies)
      (let* ((board (e-board-create
                     :id "board"
                     :effect-scheduler (lambda (effect) (push effect effects))
                     :terminal-classification-scheduler (lambda (drain) (push drain routers))
                     :invocation-effect-dispatcher
                     (lambda (_board target state reason)
                       (push (list target state reason) replies))))
             (successful (e-work-prepare
                          (e-work-spec-create
                           :id "successful" :execution 'cooperative :interactive-policy 'async
                           :runner (lambda (_handle _arguments _context) :deferred)) nil))
             (failed (e-work-prepare
                      (e-work-spec-create
                       :id "failed" :execution 'cooperative :interactive-policy 'async
                       :runner (lambda (_handle _arguments _context) :deferred)) nil)))
        (e-board-enroll-work board successful)
        (e-board-enroll-work board failed)
        (e-board-subscribe-aggregation board (list (e-work-handle-id successful))
                                       'on-success "success" :id "success")
        (e-board-subscribe-aggregation board (list (e-work-handle-id failed))
                                       'on-failure "failure" :id "failure")
        (e-board-subscribe-aggregation board (list (e-work-handle-id failed))
                                       'on-terminal "terminal" :id "terminal")
        (e-work-start-prepared successful)
        (e-work-start-prepared failed)
        (e-work-fail failed (list 'e-work-error "failed"))
        (funcall (pop routers))
        ;; The board schedules one bounded effect drain, which applies both
        ;; ready exact activations in FIFO order.
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (= (length replies) 2))
        (e-work-finish successful "done")
        (funcall (pop routers))
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (equal (sort (mapcar #'car replies) #'string<)
                       '("failure" "success" "terminal")))
        (should (cl-every (lambda (reply)
                            (and (eq (nth 1 reply) 'aggregation)
                                 (eq (nth 2 reply) 'complete)))
                          replies))
        (should-error
         (e-board-subscribe-aggregation
          board (list (e-work-handle-id successful) (e-work-handle-id failed))
          'on-success "invalid")
         :type 'e-board-error)))))

(ert-deftest e-board-test-empty-all-terminal-aggregation-is-ready-without-a-terminal-event ()
  "An empty all-terminal set reaches its deferred effect without a fake work id."
  (e-board-test--with-empty-registry
    (let (effects replies)
      (let ((board (e-board-create
                    :id "board"
                    :effect-scheduler (lambda (effect) (push effect effects))
                    :invocation-effect-dispatcher
                    (lambda (_board target state reason)
                      (push (list target state reason) replies)))))
        (e-board-subscribe-aggregation board nil 'all-terminal "empty" :id "empty")
        (should (eq (e-board-aggregation-state (e-board-aggregation board "empty"))
                    'prepared))
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (equal replies '(("empty" aggregation complete))))
        (should (eq (e-board-aggregation-state (e-board-aggregation board "empty"))
                    'committed))))))

(ert-deftest e-board-test-exact-invocation-failure-records-its-activation-state ()
  "A failed exact reply is visible on both its invocation and activation."
  (e-board-test--with-empty-registry
    (let (effects routers)
      (let* ((board (e-board-create
                     :id "board"
                     :effect-scheduler (lambda (effect) (push effect effects))
                     :terminal-classification-scheduler
                     (lambda (drain) (push drain routers))
                     :invocation-effect-dispatcher
                     (lambda (&rest _arguments) (error "reply failed"))))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "cheap" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done")) nil)))
        (e-board-enroll-work board handle)
        (e-board-subscribe-invocation board (e-work-handle-id handle) "call"
                                      :id "call")
        (e-work-start-prepared handle)
        (funcall (pop routers))
        (funcall (pop effects))
        (should (eq (e-board-invocation-state (e-board-invocation board "call"))
                    'failed))
        (should (eq (e-board-activation-state
                     (e-board-activation board '("board" "call" 1)))
                    'failed))))))

(ert-deftest e-board-test-aggregation-timeout-does-not-cancel-work ()
  "Aggregation timeout queues its deferred reply transition without cancelling work."
  (e-board-test--with-empty-registry
    (let (effects deadline-drains reasons)
      (let* ((board (e-board-create :id "board"
                                    :effect-scheduler
                                    (lambda (effect) (push effect effects))
                                    :aggregation-deadline-scheduler
                                    (lambda (drain) (push drain deadline-drains))
                                    :invocation-effect-dispatcher
                                    (lambda (_board target state reason)
                                      (should (equal target "timeout"))
                                      (should (eq state 'aggregation))
                                      (push reason reasons))))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "pending" :execution 'cooperative :interactive-policy 'async
                       :runner (lambda (_handle _arguments _context) :deferred)) nil)))
        (e-board-enroll-work board handle)
        (e-board-subscribe-aggregation
         board (list (e-work-handle-id handle)) 'any
         "timeout" :timeout 0.01)
        (e-work-start-prepared handle)
        ;; Batch Emacs does not dispatch timers while `sleep-for' is blocked.
        ;; Pump the event loop until the timeout schedules its deferred drain.
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (null deadline-drains) (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        ;; The timer only records the deadline; it cannot settle or reply.
        (should (= (length deadline-drains) 1))
        (should-not effects)
        (funcall (pop deadline-drains))
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (equal reasons '(timed-out)))
        (should (eq (plist-get (e-work-status handle) :state) 'started))))))

(ert-deftest e-board-test-cancelling-prepared-aggregation-fences-its-reply-effect ()
  "Cancelling a ready aggregation never replies or changes its watched work."
  (e-board-test--with-empty-registry
    (let (effects routers replies)
      (let* ((board (e-board-create
                     :id "board"
                     :effect-scheduler (lambda (effect) (push effect effects))
                     :terminal-classification-scheduler
                     (lambda (drain) (push drain routers))
                     :invocation-effect-dispatcher
                     (lambda (&rest _arguments) (push 'replied replies))))
             (handle (e-work-prepare
                      (e-work-spec-create
                       :id "cheap" :execution 'cheap :interactive-policy 'cheap
                       :runner (lambda (_arguments _context) "done")) nil)))
        (e-board-enroll-work board handle)
        (e-board-subscribe-aggregation board (list (e-work-handle-id handle)) 'all
                                       "call" :id "call")
        (e-work-start-prepared handle)
        (funcall (pop routers))
        (let ((activation (e-board-activation board '("board" "call" 1))))
          (should (eq (e-board-activation-state activation) 'prepared))
          (e-board-cancel-aggregation board "call")
          (should (eq (e-board-aggregation-state
                       (e-board-aggregation board "call"))
                      'cancelled))
          (should (eq (e-board-activation-state activation) 'cancelled)))
        (funcall (pop effects))
        (should-not replies)
        (should (eq (e-board-work-state
                     (e-board-observed-work board (e-work-handle-id handle)))
                    'finished))))))

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
        (let ((activation (e-board-activation board '("board" "continuation" continuation 1))))
          (should activation)
          (should (eq (e-board-activation-state activation) 'prepared)))
        (funcall (pop effects))
        (let* ((derived (car (last (e-board-messages board))))
               (pickup (e-board-pickup board (car (e-board-message-pickup-ids derived))))
               (activation (e-board-activation board '("board" "continuation" continuation 1))))
          (should (equal (e-board-message-content derived) "derived"))
           (should (equal (e-board-message-to derived) "one"))
           (should (eq (e-board-activation-state activation) 'committed))
           (should (equal (e-board-pickup-participant-id pickup) "one")))))))

(ert-deftest e-board-test-batch-continuation-freezes-matches-into-one-derived-input ()
  "A count-ready continuation posts once with every matched source id."
  (e-board-test--with-empty-registry
    (let (drains)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler (lambda (drain) (push drain drains)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe board "one" '(:kind fact :tags (source)) :id "batch"
                           :effect '(:post-input :to "one" :content "batched")
                           :readiness '(:policy batch :count 2))
        (e-board-post-fact board :id "one" :tags '(source) :source-fact-key '(test 1 1))
        (should-not drains)
        (e-board-post-fact board :id "two" :tags '(source) :source-fact-key '(test 1 2))
        (should (= (length drains) 1))
        (funcall (pop drains))
        (let* ((derived (car (last (e-board-messages board))))
               (activation (e-board-activation board
                                               '("board" "batch" continuation 1))))
          (should (equal (e-board-message-content derived) "batched"))
          (should (equal (plist-get (e-board-message-attributes derived)
                                    :board-subscription-source-message-ids)
                         '("one" "two")))
          (should (eq (e-board-activation-state activation) 'committed)))))))

(ert-deftest e-board-test-continuation-firing-limit-completes-after-reserving-final-effect ()
  "A bounded continuation commits its last reserved effect, then stops matching."
  (e-board-test--with-empty-registry
    (let (effects)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler
                                   (lambda (effect) (push effect effects)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe board "one" '(:kind fact :tags (source)) :id "limited"
                           :effect '(:post-input :to "one" :content "derived")
                           :firing-limit 2)
        (e-board-post-fact board :id "one" :tags '(source) :source-fact-key '(test 1 1))
        (e-board-post-fact board :id "two" :tags '(source) :source-fact-key '(test 1 2))
        (e-board-post-fact board :id "three" :tags '(source) :source-fact-key '(test 1 3))
        (let ((subscription (e-board-find-subscription board "limited")))
          (should (eq (e-board-subscription-state subscription) 'completed))
          (should (= (e-board-subscription-firing-number subscription) 2)))
        ;; The board schedules one bounded effect drain, which commits both
        ;; reserved continuations in FIFO order inside its configured page.
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (= (length (cl-remove-if-not
                            (lambda (message) (eq (e-board-message-kind message) 'input))
                            (e-board-messages board)))
                   2))
        (should-error
         (e-board-subscribe board "one" '(:tags (source)) :id "invalid-limit"
                            :firing-limit 1)
         :type 'e-board-error)))))

(ert-deftest e-board-test-explicit-continuation-replay-is-bounded-and-never-reroutes-history ()
  "A post-input replay scans only its retained source range through a later drain."
  (e-board-test--with-empty-registry
    (let (effects)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler (lambda (effect) (push effect effects)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-post-fact board :id "source" :tags '(source)
                           :source-fact-key '(test 1 1))
        (e-board-subscribe board "one" '(:kind fact :tags (source)) :id "replay"
                           :effect '(:post-input :to "one" :content "derived")
                           :start-seq 0)
        (should (= (length effects) 1))
        (funcall (pop effects))
        ;; Replay classification schedules its derived post for a successor
        ;; effect drain; it cannot recursively apply it from that scan.
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (equal (mapcar #'e-board-message-id (e-board-messages board))
                       '("source" "msg_1")))
        (should (member 'subscription-replay-complete
                        (mapcar #'e-board-event-type (e-board-events board))))
        (should-error
         (e-board-subscribe board "one" '(:tags (source)) :id "pickup-replay"
                            :start-seq 0)
         :type 'e-board-error)))))

(ert-deftest e-board-test-subscription-lifetime-queues-a-fenced-expiry-transition ()
  "A lifetime timer only queues a valid expiry through the board effect drain."
  (e-board-test--with-empty-registry
    (let (effects timers)
      (let ((board (e-board-create
                    :id "board"
                    :effect-scheduler (lambda (effect) (push effect effects))
                    :subscription-timer-scheduler
                    (lambda (_seconds callback) (push callback timers) nil))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe board "one" '(:tags (source)) :id "expiring"
                           :lifetime 1)
        (e-board-set-subscription-state board "expiring" 'muted)
        (funcall (pop timers))
        (should (eq (e-board-subscription-state
                     (e-board-find-subscription board "expiring"))
                    'muted))
        (should (= (length effects) 1))
        (funcall (pop effects))
        (should (eq (e-board-subscription-state
                     (e-board-find-subscription board "expiring"))
                    'expired))
        (should-error
         (e-board-subscribe board "one" '(:tags (source)) :id "invalid-lifetime"
                            :lifetime 0)
         :type 'wrong-type-argument)))))

(ert-deftest e-board-test-batch-deadline-and-quiet-timers-enqueue-fenced-effects ()
  "Timers only enqueue later effects; a stale quiet callback cannot post."
  (e-board-test--with-empty-registry
    (let (drains timers)
      (let ((board (e-board-create
                    :id "board"
                    :effect-scheduler (lambda (drain) (push drain drains))
                    :continuation-timer-scheduler
                    (lambda (_seconds callback) (push callback timers) nil))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe board "one" '(:kind fact :tags (batch)) :id "batch"
                           :effect '(:post-input :to "one" :content "deadline")
                           :readiness '(:policy batch :count 3 :max-delay 1))
        (e-board-subscribe board "one" '(:kind fact :tags (quiet)) :id "quiet"
                           :effect '(:post-input :to "one" :content "quiet")
                           :readiness '(:policy latest-after-quiet :quiet-period 1))
        (e-board-post-fact board :id "batch-one" :tags '(batch)
                           :source-fact-key '(test 1 1))
        (e-board-post-fact board :id "batch-two" :tags '(batch)
                           :source-fact-key '(test 1 2))
        (e-board-post-fact board :id "quiet-one" :tags '(quiet)
                           :source-fact-key '(test 1 3))
        (e-board-post-fact board :id "quiet-two" :tags '(quiet)
                           :source-fact-key '(test 1 4))
        ;; Timer callbacks enqueue, but do not apply, their effects.  The
        ;; superseded quiet callback remains harmless behind its generation.
        (dolist (timer timers) (funcall timer))
        (should (= (length drains) 1))
        (funcall (pop drains))
        ;; The timer-owned accumulator flushes schedule their own derived-post
        ;; effects for a later bounded drain.
        (should (= (length drains) 1))
        (funcall (pop drains))
        (let ((derived (cl-remove-if-not
                        (lambda (message) (eq (e-board-message-kind message) 'input))
                        (e-board-messages board))))
          (should (equal (sort (mapcar #'e-board-message-content derived) #'string<)
                         '("deadline" "quiet")))
          (let ((deadline (cl-find "deadline" derived
                                   :key #'e-board-message-content :test #'equal))
                (quiet (cl-find "quiet" derived
                                :key #'e-board-message-content :test #'equal)))
            (should (equal (plist-get (e-board-message-attributes deadline)
                                    :board-subscription-source-message-ids)
                           '("batch-one" "batch-two")))
            (should (equal (plist-get (e-board-message-attributes quiet)
                                    :board-subscription-source-message-ids)
                           '("quiet-two")))))))))

(ert-deftest e-board-test-continuation-accumulators-fence-on-mute-and-invalid-replacement ()
  "Pending continuation state cannot survive a mute or failed replacement."
  (e-board-test--with-empty-registry
    (let (drains timers)
      (let ((board (e-board-create
                    :id "board"
                    :effect-scheduler (lambda (drain) (push drain drains))
                    :continuation-timer-scheduler
                    (lambda (_seconds callback) (push callback timers) nil))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe board "one" '(:kind fact :tags (source)) :id "continuation"
                           :effect '(:post-input :to "one" :content "derived")
                           :readiness '(:policy batch :count 2 :max-delay 1))
        (e-board-post-fact board :id "source" :tags '(source)
                           :source-fact-key '(test 1 1))
        (e-board-set-subscription-state board "continuation" 'muted)
        (should-not (e-board-subscription-accumulator
                     (e-board-find-subscription board "continuation")))
        (funcall (pop timers))
        (funcall (pop drains))
        (should-not (cl-find-if (lambda (message)
                                  (eq (e-board-message-kind message) 'input))
                                (e-board-messages board)))
        (should-error
         (e-board-replace-subscription
          board "continuation" '(:tags (replacement))
          :readiness '(:policy batch :count 0))
         :type 'e-board-error)
        (should (eq (e-board-subscription-state
                     (e-board-find-subscription board "continuation"))
                    'muted))))))

(ert-deftest e-board-test-continuations-classify-output-activity-and-fact ()
  "Explicit continuations can derive input from every non-input board record."
  (e-board-test--with-empty-registry
    (let (effects)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler (lambda (effect) (push effect effects)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe board "one" '(:tags (source)) :id "continuation"
                           :effect '(:post-input :to "one" :content "derived"))
        (let ((output (e-board-post-output
                       board :id "output" :author "worker" :tags '(source)
                       :source-output-key '(worker 1 1)))
              (activity (e-board-post-activity
                         board :id "activity" :author "participant:one"
                         :subject-participant-id "one" :source-turn-id "turn"
                         :activity-kind 'tool-started :tags '(source)
                         :source-activity-key '(one 1 1)))
              (fact (e-board-post-fact
                     board :id "fact" :tags '(source)
                     :source-fact-key '(fact 1 1))))
          (should (= (length effects) 1))
          (dolist (publication (list output activity fact))
            (should-not (e-board-message-routing-state
                         (e-board-publication-message publication))))
          (funcall (pop effects))
          (should (e-board-activation board '("board" "continuation" continuation 1)))
          (should (e-board-activation board '("board" "continuation" continuation 2)))
          (should (e-board-activation board '("board" "continuation" continuation 3)))
          (should (equal (mapcar #'e-board-message-kind (e-board-messages board))
                         '(output activity fact input input input))))))))

(ert-deftest e-board-test-selectors-filter-continuations-and-observers-by-record-kind ()
  "Kind, activity-kind, and subject selectors apply to both subscription classes."
  (e-board-test--with-empty-registry
    (let (effects)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler (lambda (effect) (push effect effects)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (let ((selector '(:kind activity :activity-kind tool-started
                          :subject-participant-id "one")))
          (e-board-subscribe board "one" selector :id "continuation"
                             :effect '(:post-input :to "one" :content "derived"))
          (let ((observer (e-board-observer-subscribe
                           board "client" selector :id "observer" :start-seq 0)))
            (e-board-post-output board :id "output" :author "worker"
                                 :source-output-key '(worker 1 1))
            (e-board-post-activity
             board :id "progress" :author "participant:one"
             :subject-participant-id "one" :source-turn-id "turn"
             :activity-kind 'tool-progress :source-activity-key '(one 1 1))
            (e-board-post-activity
             board :id "started" :author "participant:one"
             :subject-participant-id "one" :source-turn-id "turn"
             :activity-kind 'tool-started :source-activity-key '(one 1 2))
            (e-board-post-activity
             board :id "other" :author "participant:two"
             :subject-participant-id "two" :source-turn-id "turn"
             :activity-kind 'tool-started :source-activity-key '(two 1 1))
            (should (= (length effects) 1))
            (should (e-board-activation board '("board" "continuation" continuation 1)))
            (should (equal (mapcar #'e-board-message-id
                                   (e-board-observer-read-page board "observer"))
                           '("started")))))))))

(ert-deftest e-board-test-effect-drain-yields-after-its-bounded-page ()
  "Queued effects retain FIFO order while each scheduler turn stays bounded."
  (e-board-test--with-empty-registry
    (let ((e-board-effect-drain-limit 1)
          drains applied)
      (let ((board (e-board-create
                    :id "board"
                    :effect-scheduler (lambda (drain) (push drain drains)))))
        (e-board--schedule-effect board (lambda () (push 'first applied)))
        (e-board--schedule-effect board (lambda () (push 'second applied)))
        (e-board--schedule-effect board (lambda () (push 'third applied)))
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
  (e-board-test--with-empty-registry
    (let ((e-board-effect-drain-limit 1)
          drains applied)
      (let ((board (e-board-create
                    :id "board"
                    :effect-scheduler (lambda (drain) (push drain drains)))))
        (e-board--schedule-effect
         board
         (lambda ()
           (push 'first applied)
           (e-board--schedule-effect board (lambda () (push 'second applied)))))
        (funcall (pop drains))
        (should (equal (reverse applied) '(first)))
        (should (= (length drains) 1))
        (funcall (pop drains))
        (should (equal (reverse applied) '(first second)))
        (should-not drains)))))

(ert-deftest e-board-test-effect-fifo-publishes-exact-unsettled-transitions ()
  "Effect ownership is indexed at enqueue and retirement without queue scans."
  (e-board-test--with-empty-registry
    (let* ((drains nil)
           (snapshots nil)
           (applied nil)
           (e-board-effect-drain-limit 1)
           (board
            (e-board-create
             :id "board"
             :effect-scheduler (lambda (drain) (push drain drains))
             :unsettled-change-function
             (lambda (_board _class _delta state) (push state snapshots)))))
      (e-board--schedule-effect board (lambda () (push 'first applied)))
      (e-board--schedule-effect board (lambda () (push 'second applied)))
      (should (equal (e-board-unsettled-state board)
                     '(:generation 2 :pickups 0 :effects 2 :routing 0)))
      (funcall (pop drains))
      (should (equal (e-board-unsettled-state board)
                     '(:generation 3 :pickups 0 :effects 1 :routing 0)))
      (funcall (pop drains))
      (should (equal (e-board-unsettled-state board)
                     '(:generation 4 :pickups 0 :effects 0 :routing 0)))
      (should-not (e-board-pending-effects-tail board))
      (should (equal (nreverse applied) '(first second)))
      (should (= (length snapshots) 4)))))

(ert-deftest e-board-test-effect-drain-contains-one-unexpected-callback-failure ()
  "A failed queued callback records its fault without blocking later FIFO work."
  (e-board-test--with-empty-registry
    (let ((e-board-effect-drain-limit 2)
          drains applied)
      (let ((board (e-board-create
                    :id "board"
                    :effect-scheduler (lambda (drain) (push drain drains)))))
        (e-board--schedule-effect board (lambda () (error "unexpected effect failure")))
        (e-board--schedule-effect board (lambda () (push 'second applied)))
        (e-board--schedule-effect board (lambda () (push 'third applied)))
        (funcall (pop drains))
        (should (equal (reverse applied) '(second)))
        (should (member 'effect-drain-failed
                        (mapcar #'e-board-event-type (e-board-events board))))
        (should (= (length drains) 1))
        (funcall (pop drains))
        (should (equal (reverse applied) '(second third)))
        (should-not drains)))))

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

(ert-deftest e-board-test-cancelling-subscription-fences-prepared-post-input ()
  "A prepared continuation cannot append after its owner is cancelled."
  (e-board-test--with-empty-registry
    (let (effects)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler (lambda (effect) (push effect effects)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe board "one" '(:tags (source)) :id "continuation"
                           :effect '(:post-input :to "one" :content "derived"))
        (e-board-post-input board :id "source" :tags '(source))
        (e-board-set-subscription-state board "continuation" 'cancelled)
        (funcall (pop effects))
        (should (= (length (e-board-messages board)) 1))
        (should (eq (e-board-activation-state
                    (e-board-activation board '("board" "continuation" continuation 1)))
                    'cancelled))))))

(ert-deftest e-board-test-muting-subscription-fences-prepared-post-input ()
  "Muting a continuation fences its prepared post effect too."
  (e-board-test--with-empty-registry
    (let (effects)
      (let ((board (e-board-create :id "board"
                                   :effect-scheduler (lambda (effect) (push effect effects)))))
        (e-board-add-participant board :id "one" :create-pickup-subscription-id "address")
        (e-board-subscribe board "one" '(:tags (source)) :id "continuation"
                           :effect '(:post-input :to "one" :content "derived"))
        (e-board-post-input board :id "source" :tags '(source))
        (e-board-set-subscription-state board "continuation" 'muted)
        (funcall (pop effects))
        (should (= (length (e-board-messages board)) 1))
        (should (eq (e-board-activation-state
                     (e-board-activation board '("board" "continuation" continuation 1)))
                    'cancelled))))))

(ert-deftest e-board-test-held-classifier-respects-later-subscription-lifecycle ()
  "Frozen selector matches cannot create pickups after later lifecycle fences."
  (dolist (transition '(muted cancelled expired replaced))
    (let ((e-board--registry (make-hash-table :test 'equal))
          (e-board--id-sequence 0))
      (let (drains)
        (let ((board
               (e-board-create
                :id "board"
                :input-classification-scheduler
                (lambda (drain) (push drain drains)))))
          (e-board-add-participant
           board :id "participant" :create-pickup-subscription-id "address")
          (e-board-subscribe
           board "participant" '(:tags (main)) :id "route")
          (let ((publication
                 (e-board-post-input board :id "input" :tags '(main))))
            (pcase transition
              ('replaced
               (e-board-replace-subscription
                board "route" '(:tags (replacement)) :id "replacement"))
              (_ (e-board-set-subscription-state board "route" transition)))
            (funcall (pop drains))
            (should-not (e-board-publication-pickup-ids publication))
            (should (eq (e-board-message-routing-state
                         (e-board-publication-message publication))
                        'unrouted))
            (should-not (e-board-input-classifications board))))))))

(ert-deftest e-board-test-held-classifier-respects-later-effect-cancellation ()
  "A continuation cancelled before classification never prepares its effect."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0))
    (let (drains effects)
      (let ((board
             (e-board-create
              :id "board"
              :input-classification-scheduler
              (lambda (drain) (push drain drains))
              :effect-scheduler (lambda (effect) (push effect effects)))))
        (e-board-add-participant
         board :id "participant" :create-pickup-subscription-id "address")
        (e-board-subscribe
         board "participant" '(:tags (source)) :id "continuation"
         :effect '(:post-input :to "participant" :content "derived"))
        (e-board-post-input board :id "source" :tags '(source) :content "source")
        (e-board-set-subscription-state board "continuation" 'cancelled)
        (funcall (pop drains))
        (should-not effects)
        (should (= (length (e-board-messages board)) 1))
        (should (= (hash-table-count (e-board-activations board)) 0))))))

(ert-deftest e-board-test-append-and-observer-pages-use-owner-indexes ()
  "Retained prefixes and subscriber count do not enter append or page work."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        drains)
    (let ((board
           (e-board-create
            :id "board"
            :input-classification-scheduler
            (lambda (drain) (push drain drains)))))
      (e-board-add-participant
       board :id "participant" :create-pickup-subscription-id "address")
      (dotimes (index 64)
        (e-board-subscribe
         board "participant" (list :tags (list index))
         :id (format "subscription-%d" index)))
      (dotimes (index 64)
        (e-board-post-fact
         board :id (format "fact-%d" index)
         :tags '(main) :source-fact-key (list 'producer 1 (1+ index))))
      (let ((copies 0))
        (cl-letf (((symbol-function 'copy-e-board-subscription)
                   (lambda (subscription)
                     (cl-incf copies)
                     subscription)))
          (e-board-post-input board :id "input" :tags '(main)))
        (should (= copies 0)))
      (let* ((observer
              (e-board-observer-subscribe
               board "client" '(:tags (main)) :id "observer"
               :start-seq 0 :history-before-seq (1+ (e-board-next-seq board))))
             live history)
        (cl-letf (((symbol-function 'e-board-messages)
                   (lambda (&rest _arguments)
                     (error "observer page scanned retained message list")))
                  ((symbol-function 'reverse)
                   (lambda (&rest _arguments)
                     (error "observer history reversed retained log"))))
          (setq live (e-board-observer-prepare-page board "observer" :limit 3)
                history
                (e-board-observer-prepare-history-page
                 board "observer" :limit 3)))
        (should (= (length (plist-get live :messages)) 3))
        (should (= (length (plist-get history :messages)) 3))
        (should (= (e-board-observer-next-index observer) 0))))))

(ert-deftest e-board-test-observer-recent-messages-bounds-kinds-independently ()
  "A recent snapshot finds matching kinds without unrelated tail starvation."
  (let ((board (e-board-create)))
    (e-board-observer-subscribe
     board "client" '(:tags (main)) :id "recent" :start-seq 0)
    (e-board-post-output
     board :id "main-output" :author "test" :tags '(main)
     :content "durable main answer" :source-output-key '(test 1 1))
    (e-board-post-output
     board :id "other-output" :author "test" :tags '(subagent)
     :content "not in the main view" :source-output-key '(test 1 2))
    (dotimes (index 20)
      (e-board-post-activity
       board :id (format "recent-activity-%02d" index)
       :author "participant:test" :subject-participant-id "test"
       :source-turn-id "turn" :activity-kind 'work-progress
       :tags '(main) :source-activity-key (list 'test 1 (1+ index))))
    (let ((messages
           (e-board-observer-recent-messages
            board "recent" :kinds '(input output) :limit 1
            :before-seq (1+ (e-board-next-seq board)))))
      (should (equal (mapcar #'e-board-message-id messages)
                     '("main-output"))))))

(ert-deftest e-board-test-exact-retirement-fences-all-deferred-route-work ()
  "An exact route retirement fences classifiers, effects, replay, and timers.

Each route is restored with the same durable id after retirement.  Deferred
callbacks from the old object must not mutate that replacement, while the
board retains the old immutable subscription history for diagnostics."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        classifiers effects continuation-timers subscription-timers)
    (let ((board
           (e-board-create
            :id "board"
            :input-classification-scheduler
            (lambda (drain) (push drain classifiers))
            :effect-scheduler (lambda (effect) (push effect effects))
            :continuation-timer-scheduler
            (lambda (_seconds callback)
              (push callback continuation-timers)
              nil)
            :subscription-timer-scheduler
            (lambda (_seconds callback)
              (push callback subscription-timers)
              nil))))
      (e-board-add-participant
       board :id "participant" :create-pickup-subscription-id "address")

      ;; A frozen classifier contains the old token, not merely the reusable
      ;; subscription id.  Retiring first therefore makes the input unrouted.
      (let* ((old (e-board-subscribe board "participant" '(:tags (old))
                                     :id "classifier"))
             (publication (e-board-post-input board :id "classifier-input"
                                               :tags '(old)))
             (drain (pop classifiers)))
        (e-board-retire-subscription-exact board old)
        (let ((replacement
               (e-board-subscribe board "participant" '(:tags (new))
                                  :id "classifier")))
          (funcall drain)
          (should (eq (e-board-message-routing-state
                       (e-board-publication-message publication))
                      'unrouted))
          (should (eq (e-board-subscription-state replacement) 'active))))

      ;; A prepared continuation effect is cancelled before its retained
      ;; effect-drain callback runs, and its same-id replacement stays active.
      (let* ((old (e-board-subscribe
                   board "participant" '(:tags (prepared)) :id "prepared"
                   :effect '(:post-input :content "old-prepared")))
             (_publication
              (e-board-post-fact board :id "prepared-fact" :tags '(prepared)
                                 :source-fact-key '(test 91 1)))
             (drain (pop classifiers)))
        (funcall drain)
        (should effects)
        (e-board-retire-subscription-exact board old)
        (let ((replacement
               (e-board-subscribe
                board "participant" '(:tags (replacement)) :id "prepared"
                :effect '(:post-input :content "new-prepared"))))
          (funcall (pop effects))
          (should (eq (e-board-subscription-state replacement) 'active))
          (should-not
           (cl-find "old-prepared" (e-board-messages board)
                    :key #'e-board-message-content :test #'equal))))

      ;; Explicit retained replay is removed from the process-local replay
      ;; queue; the immutable historical fact remains on the board.
      (e-board-post-fact board :id "replay-fact" :tags '(replay)
                         :source-fact-key '(test 91 2))
      (let ((old (e-board-subscribe
                  board "participant" '(:tags (replay)) :id "replay"
                  :effect '(:post-input :content "old-replay")
                  :start-seq 0)))
        (should (e-board-subscription-replays board))
        (e-board-retire-subscription-exact board old)
        (let ((replacement
               (e-board-subscribe
                board "participant" '(:tags (replacement)) :id "replay"
                :effect '(:post-input :content "new-replay"))))
          (funcall (pop effects))
          (should-not (e-board-subscription-replays board))
          (should (eq (e-board-subscription-state replacement) 'active))))

      ;; Retained quiet and expiry timer callbacks are both stale after a
      ;; same-id replacement, even though the scheduler still invokes them.
      (let* ((old (e-board-subscribe
                   board "participant" '(:tags (quiet)) :id "quiet"
                   :effect '(:post-input :content "old-quiet")
                   :readiness '(:policy latest-after-quiet :quiet-period 1)))
             (_publication
              (e-board-post-fact board :id "quiet-fact" :tags '(quiet)
                                 :source-fact-key '(test 91 3)))
             (drain (pop classifiers)))
        (funcall drain)
        (let ((quiet-timer (pop continuation-timers)))
          (e-board-retire-subscription-exact board old)
          (let ((replacement
                 (e-board-subscribe
                  board "participant" '(:tags (replacement)) :id "quiet"
                  :effect '(:post-input :content "new-quiet"))))
            (funcall quiet-timer)
            (funcall (pop effects))
            (should (eq (e-board-subscription-state replacement) 'active))
            (should-not
             (cl-find "old-quiet" (e-board-messages board)
                      :key #'e-board-message-content :test #'equal)))))
      (let ((old (e-board-subscribe
                  board "participant" '(:tags (expiry)) :id "expiry"
                  :lifetime 1)))
        (let ((expiry-timer (pop subscription-timers)))
          (e-board-retire-subscription-exact board old)
          (let ((replacement
                 (e-board-subscribe
                  board "participant" '(:tags (replacement)) :id "expiry")))
            (funcall expiry-timer)
            (funcall (pop effects))
            (should (eq (e-board-subscription-state replacement) 'active)))))
      (should (cl-every
               (lambda (subscription)
                 (or (not (equal (e-board-subscription-id subscription)
                                 "classifier"))
                     (memq (e-board-subscription-state subscription)
                           '(cancelled active))))
               (e-board-subscriptions board))))))

(provide 'e-board-test)

;;; e-board-test.el ends here

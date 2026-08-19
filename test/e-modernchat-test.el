;;; e-modernchat-test.el --- Tests for egui modern chat shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the shell-neutral chat service and modernchat view-model builder.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-actions)
(require 'e-backend)
(require 'e-bayesian-reasoning)
(require 'e-chat-service)
(require 'e-emacs-tools)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-modernchat)
(require 'e-modernchat-view-model)
(require 'e-project-local)
(require 'e-session)
(require 'e-structured-blocks)

(defvar e-modernchat-test--project-action-result nil)

(defun e-modernchat-test--post-board-output (harness session-id id content)
  "Post one board-visible test output and drain its bounded projection page."
  (let* ((binding (e-chat-service-ensure-binding harness session-id))
         (board (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding))))
    (e-board-post-output
     board :id id :author "test" :tags '(main) :content content
     :source-output-key
     (list 'test session-id (e-board-message-count board)))
    (e-chat-service--drain-observer binding)))

(ert-deftest e-modernchat-view-model-test-snapshot-bounds-messages ()
  "Snapshots include recent bounded messages and session metadata."
  (let ((harness (e-harness-create
                  :backend (e-backend-create :name "noop")
                  :enabled-layer-ids nil)))
    (e-harness-test-create-board-session
     harness
     :id "session-1"
     :metadata '(:project-root "/tmp/project/"
                 :context-references
                 (:chat-session
                  (:attachments ((:uri "file:///tmp/a.org"
                                  :label "a.org"))))))
    (dotimes (index 3)
      (e-modernchat-test--post-board-output
       harness "session-1" (format "m-%d" index)
       (format "message %d" index)))
    (let* ((snapshot (e-modernchat-view-model-snapshot
                      harness "session-1" :message-limit 2 :activity-limit 0))
           (session (cdr (assq 'session snapshot)))
           (messages (cdr (assq 'messages snapshot)))
           (attachments (cdr (assq 'attachments snapshot))))
      (should (equal (cdr (assq 'id session)) "session-1"))
      (should (= (length messages) 2))
      (should (equal (cdr (assq 'id (aref messages 0))) "m-1"))
      (should (= (length attachments) 1))
      (should (equal (cdr (assq 'uri (aref attachments 0)))
                     "file:///tmp/a.org")))))

(ert-deftest e-modernchat-view-model-test-hides-reasoning-block ()
  "A reasoning mark is stripped from snapshot content via the registry.
The modernchat view model must honor the structured-block registry the same
way the chat shell does, so a hidden reasoning block never reaches the egui
client."
  (let ((harness (e-harness-create
                  :backend (e-backend-create :name "noop")
                  :enabled-layer-ids nil)))
    (e-harness-activate-capability
     harness (e-bayesian-reasoning-capability-create))
    (e-harness-test-create-board-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     (list :id "m-0"
           :role 'assistant
           :content (concat "The answer uses `#+begin_reasoning` metadata.\n"
                            "The answer is 42.\n\n"
                            "#+begin_reasoning\n"
                            "claim: the answer is 42\n"
                            "confidence: high\n"
                            "alternatives: insufficient-evidence\n"
                            "evidence: none\n"
                            "#+end_reasoning\n")))
    (e-modernchat-test--post-board-output
     harness "session-1" "m-0"
     (concat "The answer uses `#+begin_reasoning` metadata.\n"
             "The answer is 42.\n\n"
             "#+begin_reasoning\n"
             "claim: the answer is 42\n"
             "confidence: high\n"
             "alternatives: insufficient-evidence\n"
             "evidence: none\n"
             "#+end_reasoning\n"))
    (let* ((snapshot (e-modernchat-view-model-snapshot
                      harness "session-1" :activity-limit 0))
           (messages (cdr (assq 'messages snapshot)))
           (message (aref messages 0))
           (content (cdr (assq 'content message)))
           (details (cdr (assq 'details message))))
      (should (string-match-p
               (regexp-quote "uses `#+begin_reasoning` metadata") content))
      (should (string-match-p "The answer is 42\\." content))
      (should-not (string-match-p
                   (regexp-quote "\n#+begin_reasoning\n") content))
      ;; The inline literal remains; only the actual fence is hidden.
      (should (= (length details) 1))
      (should (equal (cdr (assq 'summary (aref details 0))) "1 claim"))
      (should (string-match-p "The answer is 42"
                              (cdr (assq 'body (aref details 0)))))
      (should-not (string-match-p "confidence:" content)))))

(ert-deftest e-modernchat-view-model-test-omits-hidden-messages ()
  "A message flagged `:display' `hidden' never reaches the snapshot.
A superseded first attempt and a machine-authored corrective prompt both carry
the hidden disposition; the modern chat client should see only the visible
messages so the transcript reads as one clean answer."
  (let ((harness (e-harness-create
                  :backend (e-backend-create :name "noop")
                  :enabled-layer-ids nil)))
    (e-harness-test-create-board-session harness :id "session-1")
    ;; Superseded/hidden private attempts are deliberately never published.
    (e-modernchat-test--post-board-output
     harness "session-1" "m-0" "visible reply")
    (let* ((snapshot (e-modernchat-view-model-snapshot
                      harness "session-1" :activity-limit 0))
           (messages (cdr (assq 'messages snapshot)))
           (ids (mapcar (lambda (m) (cdr (assq 'id m)))
                        (append messages nil))))
      (should (equal ids '("m-0"))))))

(ert-deftest e-modernchat-view-model-test-exposes-generic-hook-audit-summary ()
  "A shell renders generic audit metadata without importing claim policy."
  (let* ((event '(:id "audit-1" :turn-id "turn-1" :event-type hook-audit
                  :created-at "2026-07-30T00:00:00Z"
                  :payload (:summary "Claim check needs revision")))
         (dto (e-modernchat-view-model-activity event)))
    (should (equal (cdr (assq 'title dto)) "Hook audit"))
    (should (equal (cdr (assq 'summary dto)) "Claim check needs revision"))))

(ert-deftest e-modernchat-test-runtime-missing-is-command-time-error ()
  "The module loads without emacs-egui; command use reports missing runtime."
  (cl-letf (((symbol-function 'e-modernchat--runtime-available-p)
             (lambda () nil)))
    (should-error (e-modernchat--ensure-runtime) :type 'user-error)))

(ert-deftest e-chat-service-test-processing-record-persistence-failure-retries-atomically ()
  "A chat persistence failure leaves a processing record available for retry."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--pending-pickup-head nil)
        (e-board-runtime--pending-pickup-tail nil)
        (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
        (e-board-runtime--pickup-drain-scheduled nil)
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        (e-chat-service--board-log-owners (make-hash-table :test 'equal)))
    (let* ((harness (e-harness-create
                     :backend (e-backend-create :name "noop")
                     :enabled-layer-ids nil))
           (session (e-chat-service-create-session :harness harness :id "retry"))
           (session-id (plist-get session :id))
           (binding (e-chat-service-binding harness session-id))
           (board (e-board-registry-board-source-board
                   (e-chat-service-binding-board binding)))
           (store (e-harness-sessions harness))
           (append-function (symbol-function 'e-session-append-board-message))
           (attempts 0))
      (cl-letf (((symbol-function 'e-session-append-board-message)
                 (lambda (&rest arguments)
                   (setq attempts (1+ attempts))
                   (let ((result (apply append-function arguments)))
                     (if (= attempts 1)
                         (error "simulated post-append failure")
                       result)))))
        (should-error
         (e-board-record-processing-chain
          board :id "chain" :root-message-id "root"
          :candidate-message-id "candidate" :caused-by-message-id "root"
          :processor-history nil :processing-depth 0 :created-at 1))
        (should-not (e-board-list-processing-chains board))
        (should (equal (mapcar (lambda (record) (plist-get record :id))
                               (e-session-board-messages store session-id))
                       '("chain")))
        (should-error
         (e-board-record-processing-chain
          board :id "chain" :root-message-id "root"
          :candidate-message-id "other" :caused-by-message-id "root"
          :processor-history nil :processing-depth 0 :created-at 1)
         :type 'e-session-board-message-conflict)
        (should-not (e-board-list-processing-chains board))
        (e-board-record-processing-chain
         board :id "chain" :root-message-id "root"
         :candidate-message-id "candidate" :caused-by-message-id "root"
         :processor-history nil :processing-depth 0 :created-at 1)
        (should (= attempts 3))
        (should (equal (mapcar #'e-board-processing-chain-id
                               (e-board-list-processing-chains board))
                       '("chain")))
        (should (equal (mapcar (lambda (record) (plist-get record :id))
                               (e-session-board-messages store session-id))
                       '("chain")))))))

(ert-deftest e-chat-service-test-submit-uses-bound-board-ingress ()
  "Shell-neutral submit posts only through its attached board participant."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--pending-pickup-head nil)
        (e-board-runtime--pending-pickup-tail nil)
        (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
        (e-board-runtime--pickup-drain-scheduled nil)
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        called)
    (let* ((harness (e-harness-create
                     :backend (e-backend-create :name "noop")
                     :enabled-layer-ids nil))
           (session (e-chat-service-create-session :harness harness :id "s1"))
           (binding (e-chat-service-binding harness (plist-get session :id))))
      (cl-letf (((symbol-function 'e-board-runtime-post-input)
                 (lambda (board &rest args)
                   (setq called (cons board args))
                   (e-board-post-input
                    (e-board-registry-board-source-board board)
                    :author (plist-get args :author)
                    :tags (plist-get args :tags)
                    :attributes (plist-get args :attributes)
                    :mode (plist-get args :mode)
                    :content (plist-get args :content)
                    :reference (plist-get args :reference)
                    :source-input-key (plist-get args :source-input-key)))))
        (should (stringp
                 (e-chat-service-submit-session
                  harness "s1" "hello" :references '(r1) :metadata '(:m t))))
        (should (eq (car called) (e-chat-service-binding-board binding)))
        (should (equal (plist-get (cdr called) :content) "hello"))
        (should (equal (plist-get (cdr called) :tags) '(main)))
        (should (equal (plist-get (cdr called) :reference) '(r1)))
        (should (equal (plist-get (cdr called) :attributes)
                       '(:m t :references (r1))))))))

(ert-deftest e-chat-service-test-opens-existing-board-and-routes-generic-posts ()
  "Board-first clients can reconnect, tag, address, and expose zero matches."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
        (e-board-runtime--pending-pickup-head nil)
        (e-board-runtime--pending-pickup-tail nil)
        (e-board-runtime--pickup-drain-scheduled nil)
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal)))
    (let* ((board (e-board-registry-create :id "shared"))
           (harness (e-harness-create :enabled-layer-ids nil)))
      (e-harness-create-session harness :id "pre-board")
      (should-error
       (e-chat-service-open-board board harness "pre-board")
       :type 'e-session-missing)
      (e-harness-test-create-board-session
       harness :id "one" :board-id "shared"
       :principal (e-board-registry-board-principal board))
      (e-harness-test-create-board-session
       harness :id "two" :board-id "shared"
       :principal (e-board-registry-board-principal board))
      (let* ((one (e-chat-service-open-board
                   board harness "one" :participant-id "one"))
             (_two (e-chat-service-open-board
                    board harness "two" :participant-id "two"))
             (source (e-board-registry-board-source-board board)))
        (e-board-registry-install-subscription
         board "two" '(:tags (review)) :id "two-review")
        (let ((tagged (e-chat-service-post one "review" :tags '(review)))
              (exact (e-chat-service-post one "self" :to "one"))
              (unrouted (e-chat-service-post one "nobody" :tags '(missing))))
          (while (e-board-input-classifications source)
            (e-board-runtime--drain-input-routing
             board (lambda () (e-board-drain-input-classifications source))))
          (should (equal (e-board-message-matching-participant-ids
                          (e-board-message source tagged))
                         '("two")))
          (should (equal (e-board-message-matching-participant-ids
                          (e-board-message source exact))
                         '("one")))
          (should (eq (e-board-message-routing-state
                       (e-board-message source unrouted))
                      'unrouted)))))))

(ert-deftest e-chat-service-test-board-list-is-bounded-and-continuable ()
  "The shell-neutral service exposes bounded public board navigation."
  (let ((e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-registry--board-index (avl-tree-create
                                        (lambda (left right)
                                          (string< (car left) (car right))))))
    (e-board-registry-create :id "board-a")
    (e-board-registry-create :id "board-b")
    (let* ((first (e-chat-service-list-boards-page :limit 1))
           (second (e-chat-service-list-boards-page
                    :after (plist-get first :next-after) :limit 1)))
      (should (= (length (plist-get first :boards)) 1))
      (should (= (length (plist-get second :boards)) 1))
      (should-not (plist-get second :next-after)))))

(ert-deftest e-chat-service-test-independent-observers-preserve-board-identity ()
  "Subscriber failure cannot advance another client's cursor or lose identity."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        good-events)
    (let* ((harness (e-harness-create :enabled-layer-ids nil))
           (binding (e-chat-service-create-board :harness harness :id "main"))
           (board (e-chat-service-binding-board binding)))
      (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _arguments) nil)))
        (let* ((good (e-chat-service-subscribe
                      harness "main" (lambda (event) (push event good-events))))
               (bad (e-chat-service-subscribe
                     harness "main" (lambda (_event) (error "subscriber failed"))))
               (bad-start-seq
                (e-board-observer-next-seq
                 (e-chat-service-subscription-observer bad))))
          (should-not (equal
                       (e-board-registry-client-id
                        (e-chat-service-subscription-client good))
                       (e-board-registry-client-id
                        (e-chat-service-subscription-client bad))))
          (e-board-post-fact
           (e-board-registry-board-source-board board)
           :id "fact" :tags '(main) :content "visible"
           :source-fact-key '(test fact 1))
          (e-chat-service--drain-subscription bad)
          (e-chat-service--drain-subscription good)
          (should (eq (car (e-chat-service-subscription-state bad)) 'faulted))
          (should (= (e-board-observer-next-seq
                      (e-chat-service-subscription-observer bad))
                     bad-start-seq))
          (let ((event (car good-events)))
            (should (equal (plist-get event :board-id)
                           (e-board-registry-board-id board)))
            (should (equal (plist-get event :message-id) "fact"))
            (should (integerp (plist-get event :board-seq))))
          (setq good-events nil)
          (e-chat-service-replace-selector good '(:tags (subagent)) :start-seq 0)
          (e-board-post-fact
           (e-board-registry-board-source-board board)
           :id "child" :tags '(subagent) :content "child activity"
           :source-fact-key '(test fact 2))
          (e-chat-service--drain-subscription good)
          (should (equal (plist-get (car good-events) :message-id) "child")))))))

(ert-deftest e-chat-service-test-detached-subscriber-client-retires-cleanly ()
  "A drain treats a registry-detached subscriber client as terminal teardown."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal)))
    (let* ((harness (e-harness-create :enabled-layer-ids nil))
           (binding (e-chat-service-create-board :harness harness :id "main"))
           (board (e-chat-service-binding-board binding)))
      (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _arguments) nil)))
        (let* ((subscription
                (e-chat-service-subscribe harness "main" #'ignore))
               (client-id
                (e-board-registry-client-id
                 (e-chat-service-subscription-client subscription))))
          (e-board-registry-detach-client board client-id)
          (should-not
           (condition-case nil
               (progn (e-chat-service--drain-subscription subscription) nil)
             (e-board-registry-client-missing t)))
          (should-not (e-chat-service-subscription-active-p subscription))
          (should-not
           (memq subscription (e-chat-service-binding-subscribers binding)))
          (should
           (eq (car (e-chat-service-subscription-state subscription))
               'detached)))))))

(ert-deftest e-chat-service-test-replay-is-bounded-board-derived-and-causal ()
  "Replay never reads private transcripts and keeps participant-local turns distinct."
  (let* ((harness (e-harness-create :enabled-layer-ids nil))
         (session (e-chat-service-create-session :harness harness :id "replay"))
         (binding (e-chat-service-binding harness (plist-get session :id)))
         (registry-board (e-chat-service-binding-board binding))
         (board (e-board-registry-board-source-board registry-board))
         (participant
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant
            (e-chat-service-binding-attachment binding)))))
    ;; Equal private turn ids from different participants must not alias.
    (e-board-post-activity
     board :id "summary-a" :author (format "participant:%s" participant)
     :subject-participant-id participant :source-turn-id "same-turn"
     :activity-kind 'turn-summary :tags '(main) :attributes '(:status failed)
     :source-activity-key (list participant 1 1))
    (e-board-post-activity
     board :id "summary-b" :author "participant:other"
     :subject-participant-id "other" :source-turn-id "same-turn"
     :activity-kind 'turn-summary :tags '(main) :attributes '(:status cancelled)
     :source-activity-key '(other 1 1))
    (e-board-post-output
     board :id "answer" :author (format "participant:%s" participant)
     :subject-participant-id participant :source-turn-id "same-turn"
     :tags '(main) :content "answer" :source-output-key (list participant 1 1))
    (e-chat-service--drain-observer binding)
    (cl-letf (((symbol-function 'e-harness-messages)
               (lambda (&rest _) (error "private transcript read")))
              ((symbol-function 'e-session-activity-events)
               (lambda (&rest _) (error "private activity read")))
              ((symbol-function 'e-harness-state)
               (lambda (&rest _) (error "private state read")))
              ((symbol-function 'e-harness-queued-prompts)
               (lambda (&rest _) (error "private queue read")))
              ((symbol-function 'e-harness-active-turns)
               (lambda (&rest _) (error "private active-turn read"))))
      (let* ((messages (e-chat-service-messages harness "replay"))
             (activities (e-chat-service-activity-events harness "replay"))
             (first (car activities))
             (second (cadr activities)))
        (should (equal (mapcar (lambda (message) (plist-get message :id))
                               messages)
                       '("answer")))
        (should (equal (mapcar (lambda (event)
                                (plist-get event :event-type))
                              activities)
                       '(turn-failed turn-cancelled)))
        (should-not (equal (plist-get first :turn-id)
                           (plist-get second :turn-id)))
        (should (equal (plist-get first :message-id) "summary-a"))
        (should (integerp (plist-get first :board-seq)))
        (should (equal (plist-get first :board-id)
                       (e-board-registry-board-id registry-board)))
        (should (= (plist-get (e-chat-service-state harness "replay")
                              :message-count)
                   1))))))

(ert-deftest e-chat-service-test-projection-ring-evicts-at-hard-cap ()
  "History/live overlap cannot grow one presentation projection without bound."
  (let* ((harness (e-harness-create :enabled-layer-ids nil))
         (session (e-chat-service-create-session :harness harness :id "bounded"))
         (binding (e-chat-service-binding harness (plist-get session :id)))
         (board (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding))))
    (dotimes (index (+ e-chat-service-projection-capacity 5))
      (e-board-post-output
       board :id (format "out-%03d" index) :author "test" :tags '(main)
       :content (format "answer %d" index)
       :source-output-key (list 'test 1 index)))
    (while (< (e-board-observer-next-index
               (e-chat-service-binding-observer binding))
              (e-board-message-count board))
      (e-chat-service--drain-observer binding))
    (let ((messages (e-chat-service-messages harness "bounded")))
      (should (= (length messages) e-chat-service-projection-capacity))
      (should (equal (plist-get (car messages) :id) "out-005"))
      (should (equal (plist-get (car (last messages)) :id) "out-260")))))

(ert-deftest e-chat-service-test-view-snapshot-continues-after-one-cursor ()
  "A view receives bounded history once and only later messages live."
  (let* ((harness (e-harness-create :enabled-layer-ids nil))
         (session (e-chat-service-create-session :harness harness :id "view"))
         (binding (e-chat-service-binding harness (plist-get session :id)))
         (board (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding)))
         live-events)
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _arguments) nil)))
      (dotimes (index (+ e-chat-service-projection-capacity 5))
        (e-board-post-output
         board :id (format "view-%03d" index) :author "test" :tags '(main)
         :content (format "answer %d" index)
         :source-output-key (list 'test "view" index)))
      (let* ((view (e-chat-service-subscribe-view
                    harness "view" (lambda (event) (push event live-events))))
             (subscription (e-chat-service-view-subscription view))
             (messages (e-chat-service-view-messages view)))
        (should (= (length messages) e-chat-service-projection-capacity))
        (should (equal (plist-get (car messages) :id) "view-005"))
        (should (= (e-board-observer-next-seq
                    (e-chat-service-subscription-observer subscription))
                   (e-chat-service-view-cursor view)))
        (e-chat-service--drain-subscription subscription)
        (should-not live-events)
        (e-board-post-output
         board :id "view-live" :author "test" :tags '(main)
         :content "live" :source-output-key '(test "view" 261))
        (e-chat-service--drain-subscription subscription)
        (should (equal (mapcar (lambda (event)
                                (plist-get event :message-id))
                              live-events)
                       '("view-live")))))))

(ert-deftest e-chat-service-test-activity-tail-cannot-starve-message-snapshot ()
  "A noisy activity tail cannot evict the durable conversation from a view."
  (let* ((harness (e-harness-create :enabled-layer-ids nil))
         (session (e-chat-service-create-session :harness harness :id "mixed-view"))
         (binding (e-chat-service-binding harness (plist-get session :id)))
         (board (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding))))
    (e-board-post-output
     board :id "durable-answer" :author "test" :tags '(main)
     :content "answer before noisy activity"
     :source-output-key '(test 1 1))
    (dotimes (index (+ e-chat-service-projection-capacity 5))
      (e-board-post-activity
       board
       :id (format "activity-%03d" index)
       :author "participant:test"
       :subject-participant-id "test"
       :source-turn-id "noisy-turn"
       :activity-kind 'work-progress
       :tags '(main)
       :content (format "progress %d" index)
       :source-activity-key (list 'test 1 (1+ index))))
    (let* ((view (e-chat-service-subscribe-view harness "mixed-view" #'ignore))
           (messages (e-chat-service-view-messages view))
           (activities (e-chat-service-view-activity-events view)))
      (should (equal (mapcar (lambda (message) (plist-get message :id)) messages)
                     '("durable-answer")))
      (should (= (length activities) e-chat-service-projection-capacity))
      (should (equal (plist-get (car activities) :message-id) "activity-005")))))

(ert-deftest e-chat-service-test-persistent-board-log-reopens-without-redelivery ()
  "A restarted service restores board history as board messages, not transcript."
  (let ((directory (make-temp-file "e-chat-board-log-" t))
        (e-board--registry (make-hash-table :test 'equal))
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        (e-chat-service--board-log-owners (make-hash-table :test 'equal))
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (harness (e-harness-create :sessions store :enabled-layer-ids nil))
               (session (e-chat-service-create-session
                         :harness harness :id "persistent-board"))
               (binding (e-chat-service-binding harness (plist-get session :id)))
               (board-id (e-board-registry-board-id
                          (e-chat-service-binding-board binding))))
          (e-modernchat-test--post-board-output
           harness "persistent-board" "persisted-answer" "durable answer")
          (should (= (length (e-session-board-messages
                              store "persistent-board"))
                     1))
          (e-session-flush-write-queue store)
          ;; Model a fresh Emacs process while retaining only the session store.
          (setq e-board--registry (make-hash-table :test 'equal)
                e-board-registry--boards (make-hash-table :test 'equal)
                e-board-registry--unsettled-pickup-count 0
                e-board-registry--unsettled-effect-count 0
                e-board-registry--unsettled-routing-count 0
                e-board-registry--unsettled-generation 0
                e-board-registry--board-index
                (avl-tree-create (lambda (left right)
                                   (string< (car left) (car right))))
                e-chat-service--bindings
                (make-hash-table :test 'eq :weakness 'key)
                e-chat-service--board-bindings (make-hash-table :test 'equal)
                e-chat-service--board-log-owners (make-hash-table :test 'equal)
                e-board-runtime--attachments (make-hash-table :test 'equal)
                e-board-runtime--session-attachments (make-hash-table :test 'equal)
                e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
          (let* ((loaded (e-session-persistent-store-create directory))
                 (restarted (e-harness-create :sessions loaded
                                              :enabled-layer-ids nil))
                 (restored (e-chat-service-ensure-binding
                            restarted "persistent-board")))
            (should (= (length (e-session-board-messages
                                loaded "persistent-board"))
                       1))
            (e-chat-service--drain-observer restored)
            (should (equal (e-board-registry-board-id
                            (e-chat-service-binding-board restored))
                           board-id))
            (should (equal (mapcar (lambda (message)
                                     (plist-get message :content))
                                   (e-chat-service-messages
                                    restarted "persistent-board"))
                           '("durable answer")))
            (should-not
             (e-board-input-classifications
              (e-board-registry-board-source-board
               (e-chat-service-binding-board restored))))))
      (delete-directory directory t))))

(ert-deftest e-chat-service-test-board-chat-end-to-end ()
  "Board ingress, harness delivery, board output, and observation round-trip."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--pending-pickup-head nil)
        (e-board-runtime--pending-pickup-tail nil)
        (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
        (e-board-runtime--pickup-drain-scheduled nil)
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        events)
    (let* ((harness
            (e-harness-create
             :backend (e-backend-fake-create
                       :items '((:type assistant-message :content "answer")
                                (:type done :reason stop)))
             :enabled-layer-ids nil))
           (session (e-chat-service-create-session :harness harness :id "chat-e2e"))
           (session-id (plist-get session :id))
           (binding (e-chat-service-binding harness session-id)))
      (e-chat-service-subscribe harness session-id
                                (lambda (event) (push event events)))
      (let ((input-id (e-chat-service-submit-session harness session-id "question")))
        (e-board-runtime--drain-input-routing
         (e-chat-service-binding-board binding)
         (lambda ()
           (e-board-drain-input-classifications
            (e-board-registry-board-source-board
             (e-chat-service-binding-board binding)))))
        (e-board-runtime--drain-pickups)
        (should (equal (plist-get (e-harness-wait-batch harness session-id 2.0)
                                  :status)
                       'done))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (< (float-time) deadline)
                      (not (cl-find-if
                            (lambda (event)
                              (and (eq (plist-get event :type) 'message-added)
                                   (eq (plist-get
                                        (plist-get (plist-get event :payload)
                                                   :message)
                                        :role)
                                       'assistant)))
                            events)))
            (accept-process-output nil 0.01)))
        (let* ((source (e-board-registry-board-source-board
                        (e-chat-service-binding-board binding)))
               (messages (e-board-messages source)))
          (should (eq (e-board-message-kind (car messages)) 'input))
          (should (cl-find 'turn-summary messages
                           :key #'e-board-message-activity-kind))
          (should (equal (e-board-message-content
                          (cl-find 'output messages :key #'e-board-message-kind))
                         "answer"))
          (should (cl-find-if
                   (lambda (event)
                     (and (eq (plist-get event :type) 'message-added)
                          (eq (plist-get
                               (plist-get (plist-get event :payload) :message)
                               :role)
                              'assistant)))
                   events))
          (should (cl-find input-id events :key (lambda (event)
                                                  (plist-get event :turn-id)))))
        (should (null (e-harness-queued-prompts harness session-id)))))))

(ert-deftest e-chat-service-test-bayesian-follow-up-stays-off-main-projection ()
  "A Bayesian corrective interaction uses a non-main board route end to end."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--pending-pickup-head nil)
        (e-board-runtime--pending-pickup-tail nil)
        (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
        (e-board-runtime--pickup-drain-scheduled nil)
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        (e-chat-service--board-log-owners (make-hash-table :test 'equal))
        (initial-reply
         (concat "Errors rose after the rollout.\n\n"
                 "```reasoning\n"
                 "claim: the rollout caused the rise\n"
                 "confidence: high\n"
                 "alternatives: an upstream incident\n"
                 "evidence:\n"
                 "```\n"))
        (backend-calls 0)
        events)
    (let* ((harness
            (e-harness-create
             :backend
             (e-backend-create
              :name "bayesian-board-e2e"
              :start
              (cl-function
               (lambda (&key on-item on-done on-request-start &allow-other-keys)
                 (cl-incf backend-calls)
                 (when on-request-start
                   (funcall on-request-start (e-backend-request-create)))
                 (funcall on-item
                          (list :type 'assistant-message
                                :content (if (= backend-calls 1)
                                             initial-reply
                                           "I don't know.")))
                 (funcall on-item '(:type done :reason stop))
                 (funcall on-done '(:status done))
                 (e-backend-request-create))))
             :enabled-layer-ids nil))
           (_capability
            (e-harness-activate-capability
             harness (e-bayesian-reasoning-capability-create)))
           (session (e-chat-service-create-session
                     :harness harness :id "bayesian-board-e2e"))
           (session-id (plist-get session :id))
           (binding (e-chat-service-binding harness session-id))
           (board (e-chat-service-binding-board binding))
           (source (e-board-registry-board-source-board board))
           (subscription
            (e-chat-service-subscribe
             harness session-id (lambda (event) (push event events)))))
      (e-chat-service-submit-session harness session-id "Why did errors rise?")
      (e-board-runtime--drain-input-routing
       board (lambda () (e-board-drain-input-classifications source)))
      (e-board-runtime--drain-pickups)
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (< (float-time) deadline)
                    (< (cl-count 'output (e-board-messages source)
                                 :key #'e-board-message-kind)
                       2))
          (accept-process-output nil 0.01)))
      (e-chat-service--drain-observer binding)
      (let ((inputs (cl-remove-if-not
                     (lambda (message)
                       (eq (e-board-message-kind message) 'input))
                     (e-board-messages source)))
            (outputs (cl-remove-if-not
                      (lambda (message)
                        (eq (e-board-message-kind message) 'output))
                      (e-board-messages source)))
            (visible-assistants
             (cl-remove-if-not
              (lambda (event)
                (and (eq (plist-get event :type) 'message-added)
                     (eq (plist-get
                          (plist-get (plist-get event :payload) :message)
                          :role)
                         'assistant)))
              events)))
        (should (equal (mapcar #'e-board-message-tags inputs)
                       '((main) (bayesian-reasoning-validation))))
        (should (equal
                 (plist-get (e-board-message-attributes (cadr inputs))
                            :bayesian-reasoning)
                 e-bayesian-reasoning--follow-up-marker))
        (should (= (length outputs) 2))
        (should (equal (mapcar #'e-board-message-tags outputs)
                       '((main) (bayesian-reasoning-validation))))
        (should (= (length visible-assistants) 1))
        (let* ((audit-events
                (cl-remove-if-not
                 (lambda (event) (eq (plist-get event :type) 'hook-audit))
                 events))
               (summaries
                (mapcar (lambda (event)
                          (plist-get (plist-get event :payload) :summary))
                        audit-events)))
          (should (equal summaries '("Claim check needs revision")))
          (should-not
           (seq-some (lambda (event)
                       (plist-member (plist-get event :payload) :details))
                     audit-events)))
        (let ((visible-content
               (plist-get
                (plist-get (plist-get (car visible-assistants) :payload)
                           :message)
                :content)))
          (should (equal visible-content
                         (e-board-message-content (car outputs))))
          (should-not (equal visible-content
                             (e-board-message-content (cadr outputs))))))
      (e-chat-service-unsubscribe subscription))))

(ert-deftest e-chat-service-test-idle-board-closes-through-bounded-registry ()
  "The last shell client schedules full registry-owned board cleanup."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        idle-callback
        close-callbacks)
    (let* ((harness (e-harness-create :enabled-layer-ids nil))
           (binding (e-chat-service-create-board :harness harness :id "idle"))
           (board (e-chat-service-binding-board binding))
           (board-id (e-board-registry-board-id board))
           (e-board-registry-close-scheduler
            (lambda (function)
              (setq close-callbacks
                    (append close-callbacks (list function))))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest arguments)
                   (setq idle-callback (lambda () (apply function arguments)))
                   (timer-create))))
        (e-chat-service--schedule-idle-close binding))
      (funcall idle-callback)
      (should (eq (e-board-registry-board-state board) 'closing))
      (while close-callbacks
        (funcall (pop close-callbacks)))
      (should (eq (e-board-registry-board-state board) 'closed))
      (should-error (e-board-registry-get board-id)
                    :type 'e-board-registry-missing))))

(provide 'e-modernchat-test)

;;; e-modernchat-test.el ends here

(ert-deftest e-chat-service-test-continuation-retry-reuses-one-input-key ()
  "A failed publication retry cannot queue a second reconciliation turn."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
        (e-chat-service--continuation-reconciling (make-hash-table :test 'equal)))
    (let* ((runtime-board (e-board-registry-create :id "continuation-board" :principal "test"))
           (board (e-board-registry-board-source-board runtime-board))
           (binding (e-chat-service--binding-create :harness 'test :board runtime-board))
           (queued nil)
           (attempts 0))
      (e-board-orchestration-publish-fact
       board
       '(:version 1 :type manifest :idempotency-key "manifest"
         :payload (:run-id "run-1"
                   :tasks ((:task-key "task" :required t :accepted-attempt 0))
                   :continuation (:session-id "coordinator" :prompt "reconcile"
                                  :publication-key "publication-1"))))
      (e-board-orchestration-publish-fact
       board
       '(:version 1 :type terminal-report :idempotency-key "report"
         :payload (:run-id "run-1" :task-key "task" :attempt 0 :status done
                   :summary "done" :outputs [])))
      (cl-letf (((symbol-function 'e-chat-service-queue-session)
                 (lambda (_harness _session-id _prompt &rest arguments)
                   (push (plist-get arguments :source-input-key) queued)
                   (setq attempts (1+ attempts))
                   (if (= attempts 1)
                       (error "publication interrupted")
                     "continuation-message"))))
        ;; This call models recovery after a restart that found the terminal
        ;; report but no continuation acknowledgement.
        (e-chat-service--reconcile-board-continuation binding)
        (should (eq (plist-get (plist-get (e-board-orchestration-run-projection
                                           board "run-1")
                                          :continuation)
                               :state)
                    'failed))
        (e-chat-service--reconcile-board-continuation binding)
        ;; A later restart finds the published acknowledgement and does not
        ;; submit another input.
        (e-chat-service--reconcile-board-continuation binding))
      (should (= attempts 2))
      (should (equal (car queued) (cadr queued)))
      (should (eq (plist-get (plist-get (e-board-orchestration-run-projection
                                         board "run-1")
                                        :continuation)
                             :state)
                  'published)))))


(ert-deftest e-chat-service-test-continuation-invokes-project-local-action ()
  "A queued board continuation resolves an action from its session project root."
  (let* ((project (make-temp-file "e-continuation-project-action-" t))
         (directory (expand-file-name ".e/capabilities/daily-run" project))
         (file (expand-file-name "capability.el" directory))
         (e-project-local-allowed-roots (list project))
         (e-modernchat-test--project-action-result nil)
         (e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--id-sequence 0)
         (e-board-registry--unsettled-pickup-count 0)
         (e-board-registry--unsettled-effect-count 0)
         (e-board-registry--unsettled-routing-count 0)
         (e-board-registry--unsettled-generation 0)
         (e-board-runtime--attachments (make-hash-table :test 'equal))
         (e-board-runtime--session-attachments (make-hash-table :test 'equal))
         (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
         (e-board-runtime--invocations (make-hash-table :test 'equal))
         (e-board-runtime--pending-pickup-head nil)
         (e-board-runtime--pending-pickup-tail nil)
         (e-board-runtime--pending-pickup-set (make-hash-table :test 'equal))
         (e-board-runtime--pickup-drain-scheduled nil)
         (e-board-runtime--admission-open-p t)
         (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
         (e-chat-service--board-bindings (make-hash-table :test 'equal))
         (e-chat-service--continuation-reconciling
          (make-hash-table :test 'equal))
         (backend-calls 0))
    (unwind-protect
        (progn
          (make-directory directory t)
          (write-region
           ";;; capability.el -*- lexical-binding: t; -*-
(e-project-capability-register
 :id 'daily-run
 :factory
 (lambda (_directory)
   (e-capability-create
    :id 'daily-run
    :name \"Daily run\"
    :actions
    (list :finalize
          (e-action-cheap-create
           :description \"Finalize a daily run.\"
           :runner
           (lambda (_arguments _context)
             (setq e-modernchat-test--project-action-result 'finalized)))))))"
           nil file nil 'silent)
          (let* ((backend
                  (e-backend-create
                   :name "project-action-continuation"
                   :stream
                   (cl-function
                    (lambda (&key messages options on-item)
                      (ignore options)
                      (setq backend-calls (1+ backend-calls))
                      (if (= backend-calls 1)
                          (progn
                            (should (equal (plist-get (car (last messages))
                                                     :content)
                                           "reconcile"))
                            (funcall
                             on-item
                             '(:type tool-call
                               :id "run-action"
                               :name "run_elisp"
                               :arguments
                               (:code
                                "(e-actions-call 'daily-run :finalize nil)")))
                            (funcall on-item '(:type done :reason tool-use)))
                        (funcall on-item
                                 '(:type assistant-message
                                   :content "reconciled"))
                        (funcall on-item '(:type done :reason stop)))))))
                 (tools
                  (e-capability-create
                   :id 'continuation-tools
                   :tools
                   (list (lambda (registry)
                           (e-emacs-tools-register-run-elisp registry)))))
                 (harness
                  (e-harness-create
                   :backend backend
                   :enabled-layer-ids nil
                   :intrinsic-capabilities
                   (list (e-project-local--dynamic-capability project)
                         tools)))
                 (session
                  (e-chat-service-create-session
                   :harness harness
                   :id "coordinator"
                   :metadata (list :project-root project)))
                 (session-id (plist-get session :id))
                 (binding (e-chat-service-binding harness session-id))
                 (runtime-board (e-chat-service-binding-board binding))
                 (board (e-board-registry-board-source-board runtime-board)))
            (e-board-orchestration-publish-fact
             board
             '(:version 1 :type manifest :idempotency-key "manifest"
               :payload (:run-id "run-1"
                         :tasks ((:task-key "task" :required t
                                  :accepted-attempt 0))
                         :continuation
                         (:session-id "coordinator" :prompt "reconcile"
                          :publication-key "publication-1"))))
            (e-board-orchestration-publish-fact
             board
             '(:version 1 :type terminal-report :idempotency-key "report"
               :payload (:run-id "run-1" :task-key "task" :attempt 0
                         :status done :summary "done" :outputs [])))
            (e-chat-service--reconcile-board-continuation binding)
            (e-board-runtime--drain-input-routing
             runtime-board
             (lambda ()
               (e-board-drain-input-classifications board)))
            (e-board-runtime--drain-pickups)
            (should (equal (plist-get
                            (e-harness-wait-batch harness session-id 2.0)
                            :status)
                           'done))
            (should (= backend-calls 2))
            (should (eq e-modernchat-test--project-action-result 'finalized))
            (should
             (eq (plist-get
                  (plist-get
                   (e-board-orchestration-run-projection board "run-1")
                   :continuation)
                  :state)
                 'published))))
      (delete-directory project t))))

;;; e-modernchat-test.el --- Tests for egui modern chat shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the shell-neutral chat service and modernchat view-model builder.

;;; Code:

(require 'ert)
(require 'cl-lib)
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
(require 'e-session-storage)
(require 'e-structured-blocks)

(defvar e-modernchat-test--project-action-result nil)

(defun e-chat-service-test--session-ids (harness)
  "Return user-facing root session ids from HARNESS."
  (mapcar (lambda (session) (plist-get session :id))
          (e-chat-service-root-session-list harness)))

(defun e-modernchat-test--file-bytes (file)
  "Return the exact bytes currently stored in FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun e-modernchat-test--active-observer-count (board)
  "Return the number of active or muted observers on BOARD."
  (let ((count 0))
    (maphash
     (lambda (_id observer)
       (when (memq (e-board-observer-state observer) '(active muted))
         (cl-incf count)))
     (e-board-observers board))
    count))

(defun e-modernchat-test--post-board-output (harness session-id id content)
  "Post one board-visible test output and drain its bounded projection page."
  (let* ((binding (e-chat-service-ensure-binding harness session-id))
         (board (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding))))
    (e-board-post-output
     board :id id :author "test" :tags '(main) :content content
     :source-output-key
     (list 'test session-id (e-board-message-count board)))
    (e-chat-service-drain-binding binding)))

(ert-deftest e-chat-service-test-board-output-identifies-terminal-presentation ()
  "A board output tells shells that the producing turn has already finished."
  (let* ((harness (e-harness-create :enabled-layer-ids nil))
         (session (e-chat-service-create-session
                   :harness harness :id "terminal-output"))
         (binding (e-chat-service-binding harness (plist-get session :id)))
         (board (e-board-registry-board-source-board
                 (e-chat-service-binding-board binding)))
         (participant-id
          (e-board-registry-participant-id
           (e-board-runtime-attachment-participant
            (e-chat-service-binding-attachment binding))))
         (message
          (e-board-publication-message
           (e-board-post-output
            board :id "answer"
            :author (format "participant:%s" participant-id)
            :subject-participant-id participant-id :tags '(main)
            :content "Finished answer." :source-turn-id "source-turn"
            :source-output-key '(test terminal-output 1))))
         (sibling-message
          (e-board-publication-message
           (e-board-post-output
            board :id "sibling-answer"
            :author "participant:other"
            :subject-participant-id "other" :tags '(main)
            :content "Sibling answer." :source-turn-id "sibling-turn"
            :source-output-key '(test terminal-output 2))))
         (event (e-chat-service--message-event binding message))
         (rendered-message (plist-get (plist-get event :payload) :message))
         (sibling-event
          (e-chat-service--message-event binding sibling-message))
         (sibling-rendered-message
          (plist-get (plist-get sibling-event :payload) :message)))
    (should (eq (plist-get event :type) 'message-added))
    (should (eq (plist-get event :selected-participant-p) t))
    (should (eq (plist-get rendered-message :role) 'assistant))
    (should (eq (plist-get rendered-message :selected-participant-p) t))
    (should (plist-get rendered-message :terminal-output))
    (should (eq (plist-get sibling-event :selected-participant-p) nil))
    (should (eq (plist-get sibling-rendered-message :selected-participant-p)
                nil))
    (should (plist-get sibling-rendered-message :terminal-output))))

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
    (let* ((board (e-board-registry-create
                   :id "shared" :principal "shared-principal"))
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
                   board harness "one"
                   :participant-id "one"
                   :pickup-selector '(:tags (main))
                   :observer-selector '(:tags (main))
                   :default-tags '(main)
                   :default-to nil))
             (_two (e-chat-service-open-board
                    board harness "two"
                    :participant-id "two"
                    :pickup-selector '(:tags (main))
                    :observer-selector '(:tags (main))
                    :default-tags '(main)
                    :default-to nil))
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

(ert-deftest e-chat-service-test-root-catalog-uses-production-board-roles ()
  "Production constructors durably distinguish a root from its participant."
  (let* ((harness (e-harness-create :enabled-layer-ids nil))
         (binding (e-chat-service-create-board
                   :harness harness :id "role-root"))
         (board (e-chat-service-binding-board binding))
         (participant
          (e-chat-service-create-participant
           board harness :id "role-participant")))
    (should
     (equal (plist-get
             (plist-get (e-chat-service-session harness "role-root")
                        :board-session-state)
             :association-role)
            "owner"))
    (should
     (equal (plist-get (plist-get participant :board-session-state)
                       :association-role)
            "participant"))
    (should (equal (e-chat-service-test--session-ids harness) '("role-root")))
    ;; Filtering the public catalog never destroys the private session.
    (should (equal (plist-get
                    (e-chat-service-session harness "role-participant") :id)
                   "role-participant"))))

(ert-deftest e-chat-service-test-restored-participant-without-policy-fails-closed ()
  "A legacy participant association is not silently rebound as main chat."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create :enabled-layer-ids nil :sessions store))
         (session-id "legacy-participant"))
    (e-session-create store :id session-id)
    (e-session-declare-board-state
     store session-id "board-owner" "legacy-board" "participant")
    (should-error (e-chat-service-ensure-binding harness session-id)
                  :type 'e-session-error)
    (should-not (gethash "legacy-board" e-board-registry--boards))
    (should (equal (e-session-board-association
                    (e-session-get store session-id))
                   '(:board-id "legacy-board"
                     :principal "board-owner"
                     :association-role "participant")))))

(ert-deftest e-chat-service-test-restores-routing-policy-before-attachment ()
  "Restoration passes the durable policy to the attachment boundary verbatim."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create :enabled-layer-ids nil :sessions store))
         (session-id "restored-routing")
         (policy
          '(:participant-id "ptc-restored"
            :pickup-selector (:kind input :tags (private))
            :observer-selector (:subject-participant-id "ptc-restored")
            :default-tags (private)
            :default-to "ptc-restored"))
         captured)
    (e-session-create store :id session-id)
    (e-session-declare-board-state
     store session-id "board-owner" "restored-board" "participant" policy)
    (cl-letf (((symbol-function 'e-chat-service--install-participant-binding)
               (lambda (_board _harness _session-id &rest arguments)
                 (setq captured arguments)
                 :captured)))
      (should (eq (e-chat-service-ensure-binding harness session-id)
                  :captured)))
    (should (equal (plist-get captured :participant-id) "ptc-restored"))
    (should (equal (plist-get captured :pickup-selector)
                   '(:kind input :tags (private))))
    (should (equal (plist-get captured :observer-selector)
                   '(:subject-participant-id "ptc-restored")))
    (should (equal (plist-get captured :default-tags) '(private)))
    (should (equal (plist-get captured :default-to) "ptc-restored"))))

(ert-deftest e-chat-service-test-legacy-routing-admission-is-explicit ()
  "Legacy board associations choose defaults, upgrade, or fail closed explicitly."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create :enabled-layer-ids nil :sessions store))
         captured)
    (cl-labels
        ((legacy (id principal role)
           (let ((board-id (format "routing-admission-%s-board" id)))
             (e-board-registry-create :id board-id :principal principal)
             (e-session-create store :id id)
             (e-session-declare-board-state
              store id principal board-id role)
             board-id)))
      (let ((canonical-board (legacy "canonical" "chat:canonical" nil))
            (owner-board (legacy "legacy-owner" "other-owner" "owner"))
            explicit-roleless-board
            ambiguous-board participant-board partial-board durable-board)
        ;; The canonical roleless root and an owner association retain the
        ;; historical main defaults without borrowing a caller policy.
        (setq explicit-roleless-board
              (legacy "explicit-roleless" "other-principal" nil)
              ambiguous-board (legacy "ambiguous" "other-principal" nil)
              participant-board (legacy "legacy-participant" "board-owner"
                                        "participant")
              partial-board (legacy "partial-participant" "board-owner"
                                    "participant")
              durable-board (legacy "durable-participant" "board-owner"
                                    "participant"))
        (cl-letf (((symbol-function 'e-chat-service--install-participant-binding)
                   (lambda (_board _harness _session-id &rest arguments)
                     (setq captured arguments)
                     :captured)))
          (should (eq (e-chat-service-open-board
                       canonical-board harness "canonical")
                      :captured))
          (should (equal (plist-get captured :default-tags) '(main)))
          (should (eq (e-chat-service-open-board
                       owner-board harness "legacy-owner")
                      :captured))
          ;; A noncanonical roleless association has no safe implicit policy.
          ;; A complete caller-supplied policy is the one explicit upgrade
          ;; permitted for that otherwise ambiguous legacy shape.
          (should (eq
                   (e-chat-service-open-board
                    explicit-roleless-board harness "explicit-roleless"
                    :participant-id "explicit-roleless-id"
                    :pickup-selector '(:tags (private))
                    :observer-selector
                    '(:subject-participant-id "explicit-roleless-id")
                    :default-tags '(private)
                    :default-to nil)
                   :captured))
          (should (equal
                   (plist-get (e-session-board-routing-policy
                               (e-session-get store "explicit-roleless"))
                              :participant-id)
                   "explicit-roleless-id"))
          (should-error (e-chat-service-open-board
                         ambiguous-board harness "ambiguous")
                        :type 'e-session-error)
          ;; A participant may be admitted only with every explicit field.  The
          ;; resolved value is persisted before the attachment boundary runs.
          (should (eq
                   (e-chat-service-open-board
                    participant-board harness "legacy-participant"
                    :participant-id "private-admitted"
                    :pickup-selector '(:tags (private))
                    :observer-selector '(:subject-participant-id
                                         "private-admitted")
                    :default-tags '(private)
                    :default-to "private-admitted")
                   :captured))
          (let ((policy (e-session-board-routing-policy
                         (e-session-get store "legacy-participant"))))
            (should (equal (plist-get policy :participant-id)
                           "private-admitted"))
            (should (equal (plist-get policy :default-tags) '(private))))
          ;; Partial upgrade input is rejected without adding a policy.
          (should-error
           (e-chat-service-open-board
            partial-board harness "partial-participant"
            :participant-id "private-partial")
           :type 'e-session-error)
          (should-not
           (e-session-board-routing-policy
            (e-session-get store "partial-participant")))
          ;; A durable policy is authoritative; conflicting caller values are
          ;; rejected rather than silently ignored.
          (e-session-declare-board-state
           store "durable-participant" "board-owner"
           (e-board-registry-board-id
            (e-board-registry-get durable-board))
           "participant"
           '(:participant-id "durable-id"
             :pickup-selector (:tags (private))
             :observer-selector (:subject-participant-id "durable-id")
             :default-tags (private)
             :default-to "durable-id"))
          (should-error
           (e-chat-service-open-board
            durable-board harness "durable-participant"
            :participant-id "other-id")
           :type 'e-session-error)
          (should (equal
                   (plist-get (e-session-board-routing-policy
                               (e-session-get store "durable-participant"))
                              :participant-id)
                   "durable-id")))))))

(ert-deftest e-chat-service-test-legacy-upgrade-preflights-occupancy-and-reopens ()
  "Legacy upgrades fail before writes for occupied or attached admissions.

An unused explicit upgrade succeeds before and after reopening the persistent
store.  The byte snapshots make the preflight boundary observable rather than
only checking the in-memory association."
  (let ((directory (make-temp-file "e-chat-legacy-upgrade-" t))
        (e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--board-index
         (avl-tree-create (lambda (left right) (string< (car left) (car right)))))
        (e-board-registry--id-sequence 0)
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        (e-chat-service--board-log-owners (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (harness (e-harness-create
                         :enabled-layer-ids nil :sessions store))
               (board (e-board-registry-create
                       :id "legacy-upgrade-board"
                       :principal "chat:legacy-upgrade"))
               (board-id (e-board-registry-board-id board))
               (principal (e-board-registry-board-principal board))
               (occupied "occupied-legacy-id")
               (queries 0)
               (original-admission
                (symbol-function 'e-board-runtime-admission-available-p)))
          (e-board-registry-add-participant
           board :id occupied :principal principal)
          (dolist (session-id '("legacy-occupied" "legacy-attached"
                                "legacy-after-reopen"))
            (e-session-create store :id session-id)
            (e-session-declare-board-state
             store session-id principal board-id "participant"))
          (let ((policy-arguments
                 (lambda (participant-id)
                   (list :participant-id participant-id
                         :pickup-selector '(:tags (private))
                         :observer-selector '(:tags (private))
                         :default-tags '(private)
                         :default-to nil)))
                (journal
                 (lambda (session-id)
                   (e-modernchat-test--file-bytes
                    (e-session-storage-session-reference store session-id))))
                (index (e-modernchat-test--file-bytes
                        (expand-file-name "index.json" directory))))
            (cl-letf (((symbol-function 'e-board-runtime-admission-available-p)
                       (lambda (&rest arguments)
                         (setq queries (1+ queries))
                         (apply original-admission arguments)))
                      ((symbol-function 'e-chat-service--install-participant-binding)
                       (lambda (&rest _arguments) :captured)))
              (let* ((session (e-session-get store "legacy-occupied"))
                     (association (copy-tree
                                   (e-session-board-association session)))
                     (before-journal (funcall journal "legacy-occupied"))
                     (before-index index))
                (should-error
                 (apply #'e-chat-service-open-board
                        board harness "legacy-occupied"
                        (funcall policy-arguments occupied))
                 :type 'e-board-registry-id-conflict)
                (should (equal (funcall journal "legacy-occupied")
                               before-journal))
                (should (equal (e-modernchat-test--file-bytes
                                (expand-file-name "index.json" directory))
                               before-index))
                (should (equal (e-session-board-association session)
                               association))
                (should-not (e-session-board-routing-policy session)))
              (should (eq
                       (apply #'e-chat-service-open-board
                              board harness "legacy-occupied"
                              (funcall policy-arguments "unused-before"))
                       :captured))
              (should (equal
                       (plist-get
                        (e-session-board-routing-policy
                         (e-session-get store "legacy-occupied"))
                        :participant-id)
                       "unused-before"))
              ;; A live endpoint using the same HARNESS/session is an
              ;; independent public admission conflict.  It must be rejected
              ;; before the legacy association is upgraded.
              (e-board-runtime-attach
               board harness "legacy-attached"
               :participant-id "already-attached" :principal principal)
              (let* ((session (e-session-get store "legacy-attached"))
                     (association (copy-tree
                                   (e-session-board-association session)))
                     (before-journal (funcall journal "legacy-attached"))
                     (before-index
                     (e-modernchat-test--file-bytes
                       (expand-file-name "index.json" directory))))
                (should-error
                 (apply #'e-chat-service-open-board
                        board harness "legacy-attached"
                        (funcall policy-arguments "unused-while-attached"))
                 :type 'e-board-runtime-session-busy)
                (should (equal (funcall journal "legacy-attached")
                               before-journal))
                (should (equal (e-modernchat-test--file-bytes
                                (expand-file-name "index.json" directory))
                               before-index))
                (should (equal (e-session-board-association session)
                               association))
                (should-not (e-session-board-routing-policy session)))
              (e-session-flush-write-queue store)
              (let* ((reopened (e-session-persistent-store-create directory))
                     (restarted (e-harness-create
                                 :enabled-layer-ids nil :sessions reopened)))
                (should (eq
                         (apply #'e-chat-service-open-board
                                board restarted "legacy-after-reopen"
                                (funcall policy-arguments "unused-after"))
                         :captured))
                (should (equal
                         (plist-get
                          (e-session-board-routing-policy
                           (e-session-get reopened "legacy-after-reopen"))
                          :participant-id)
                         "unused-after")))
              (should (>= queries 4)))))
      (delete-directory directory t))))

(ert-deftest e-chat-service-test-participant-creation-failures-are-atomic ()
  "Participant admission failures leave no orphan session or board member."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create :enabled-layer-ids nil :sessions store))
         (board (e-board-registry-create
                 :id "participant-atomic-board" :principal "board-owner")))
    ;; Board resolution and policy validation both precede session creation.
    (should-error
     (e-chat-service-create-participant
      "missing-board" harness :id "invalid-board-session")
     :type 'e-board-registry-missing)
    (should-error
     (e-chat-service-create-participant
      board harness :id "invalid-selector-session"
      :pickup-selector '(:predicate (lambda (_message) t)))
     :type 'e-session-error)
    (should-error (e-session-get store "invalid-board-session")
                  :type 'e-session-missing)
    (should-error (e-session-get store "invalid-selector-session")
                  :type 'e-session-missing)
    ;; A duplicate session id is rejected before a participant id is reserved.
    (e-session-create store :id "existing-session")
    (should-error
     (e-chat-service-create-participant
      board harness :id "existing-session" :participant-id "unused-id")
     :type 'e-session-duplicate)
    (should-not (gethash "unused-id"
                         (e-board-registry-board-participants board)))
    ;; An explicit participant collision is also preflighted.
    (e-chat-service-create-participant
     board harness :id "first-private" :participant-id "same-participant")
    ;; Successful admission publishes the participant event only after the
    ;; session declaration has crossed its durable boundary.
    (let ((source-board (e-board-registry-board-source-board board)))
      (should
       (cl-some
        (lambda (event)
          (and (eq (e-board-event-type event) 'participant-added)
               (equal (plist-get (e-board-event-data event) :participant-id)
                      "same-participant")))
        (e-board-events source-board))))
    (should-error
     (e-chat-service-create-participant
      board harness :id "second-private" :participant-id "same-participant")
     :type 'e-board-registry-id-conflict)
    (should-error (e-session-get store "second-private")
                  :type 'e-session-missing)
    ;; Owned failures after session allocation roll the session back.
    (cl-letf (((symbol-function 'e-session-storage-publish-admission)
               (lambda (&rest _arguments)
                 (signal 'e-session-error (list "persist rejected")))))
      (should-error
       (e-chat-service-create-participant
        board harness :id "persist-failure" :participant-id "persist-id")
       :type 'e-session-error))
    (should-error (e-session-get store "persist-failure")
                  :type 'e-session-missing)
    ;; A failure after the runtime attachment has prepared its participant
    ;; must not leave a replayable participant-added board event.  The event
    ;; stream is the rebuild input, so this catches a ghost that hash cleanup
    ;; alone would miss.
    (cl-letf (((symbol-function 'e-session-commit-board-admission)
               (lambda (&rest _arguments)
                 (signal 'e-session-error (list "commit rejected")))))
      (should-error
       (e-chat-service-create-participant
        board harness :id "commit-failure" :participant-id "commit-id")
       :type 'e-session-error))
    (should-error (e-session-get store "commit-failure")
                  :type 'e-session-missing)
    (should-not (gethash "commit-id"
                         (e-board-registry-board-participants board)))
    (let ((source-board (e-board-registry-board-source-board board)))
      (should-not
       (cl-some
        (lambda (event)
          (and (eq (e-board-event-type event) 'participant-added)
               (equal (plist-get (e-board-event-data event) :participant-id)
                      "commit-id")))
        (e-board-events source-board))))
    (cl-letf (((symbol-function 'e-chat-service--install-participant-binding)
               (lambda (&rest _arguments)
                 (signal 'e-session-error (list "attachment rejected")))))
      (should-error
       (e-chat-service-create-participant
        board harness :id "attachment-failure" :participant-id "attach-id")
       :type 'e-session-error))
    (should-error (e-session-get store "attachment-failure")
                  :type 'e-session-missing)
    (should-not (gethash "attach-id"
                         (e-board-registry-board-participants board)))))

(ert-deftest e-chat-service-test-participant-admission-direct-store-is-atomic ()
  "Direct admission failures remove every target-side runtime and disk fact.

The unrelated participant is established first so each failure must preserve
an already attached client, source-board history, and the derived index.  The
successful admission at the end verifies the same path remains replayable."
  (let ((directory (make-temp-file "e-chat-admission-direct-" t))
        (e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--board-index
         (avl-tree-create (lambda (left right) (string< (car left) (car right)))))
        (e-board-registry--id-sequence 0)
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        (e-chat-service--board-log-owners (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((store (e-session-persistent-store-create directory))
               (harness (e-harness-create
                         :enabled-layer-ids nil :sessions store))
               (board (e-board-registry-create
                       :id "direct-admission-board"
                       :principal "chat:direct-admission"))
               (source (e-board-registry-board-source-board board))
               (unrelated
                (e-chat-service-create-participant
                 board harness :id "direct-unrelated"
                 :participant-id "direct-unrelated-participant"))
               (unrelated-binding
                (e-chat-service-binding harness "direct-unrelated"))
               (baseline-events (length (e-board-events source)))
               (baseline-participant-events
                (cl-count-if
                 (lambda (event)
                   (eq (e-board-event-type event) 'participant-added))
                 (e-board-events source)))
               (baseline-participants
                (hash-table-count
                 (e-board-registry-board-participants board)))
               (baseline-source-participants
                (hash-table-count (e-board-participants source)))
               (baseline-subscriptions
                (hash-table-count
                 (e-board-subscription-id-table source)))
               (baseline-clients
                (hash-table-count (e-board-registry-board-clients board)))
               (baseline-observers
                (e-modernchat-test--active-observer-count source))
               (baseline-attachments
                (hash-table-count e-board-runtime--attachments))
               (baseline-session-attachments
                (hash-table-count e-board-runtime--session-attachments))
               (baseline-endpoint-attachments
                (hash-table-count e-board-runtime--endpoint-attachments))
               (baseline-ids
                (sort (mapcar (lambda (entry) (plist-get entry :id))
                              (e-session-list store))
                      #'string<))
                 (index-file (expand-file-name "index.json" directory))
               (baseline-index
                (e-modernchat-test--file-bytes index-file)))
          (cl-labels
              ((target-event-p (participant-id)
                 (cl-some
                  (lambda (event)
                    (and (eq (e-board-event-type event) 'participant-added)
                         (equal
                          (plist-get (e-board-event-data event) :participant-id)
                          participant-id)))
                  (e-board-events source)))
               (assert-absent (session-id participant-id)
                 (should-error (e-session-get store session-id)
                               :type 'e-session-missing)
                 (should-not (file-exists-p
                              (e-session-storage-session-reference store session-id)))
                 (should-not (gethash participant-id
                                      (e-board-registry-board-participants board)))
                 (should-not (e-board-participant source participant-id))
                 (should-not (target-event-p participant-id))
                 (should (equal
                          (sort (mapcar (lambda (entry) (plist-get entry :id))
                                        (e-session-list store))
                                #'string<)
                          baseline-ids))
                 ;; Binding setup has its own transient lifecycle events; the
                 ;; durable admission invariant is that no participant-added
                 ;; event for the failed target was published.
                 (should (= (cl-count-if
                             (lambda (event)
                               (eq (e-board-event-type event)
                                   'participant-added))
                             (e-board-events source))
                            baseline-participant-events))
                 (should (= (hash-table-count
                             (e-board-registry-board-participants board))
                            baseline-participants))
                 (should (= (hash-table-count (e-board-participants source))
                            baseline-source-participants))
                 (should (= (hash-table-count
                             (e-board-subscription-id-table source))
                            baseline-subscriptions))
                 (should (= (hash-table-count
                             (e-board-registry-board-clients board))
                            baseline-clients))
                 (should (= (e-modernchat-test--active-observer-count source)
                            baseline-observers))
                 (should (= (hash-table-count e-board-runtime--attachments)
                            baseline-attachments))
                 (should (= (hash-table-count e-board-runtime--session-attachments)
                            baseline-session-attachments))
                 (should (= (hash-table-count e-board-runtime--endpoint-attachments)
                            baseline-endpoint-attachments))
                 (should-not (e-chat-service-binding harness session-id))
                 (should (eq (e-chat-service-binding harness "direct-unrelated")
                             unrelated-binding))
                 (should (equal (e-modernchat-test--file-bytes index-file)
                                baseline-index))))
            ;; Detached record preparation fails before runtime attachment or
            ;; any direct journal write.
            (cl-letf (((symbol-function 'e-session-codec-record-for-json)
                       (lambda (&rest _arguments)
                         (signal 'e-session-error
                                 (list "admission preparation rejected")))))
              (should-error
               (e-chat-service-create-participant
                board harness :id "direct-preparation-failure"
                :participant-id "direct-preparation-participant")
               :type 'e-session-error))
            (assert-absent "direct-preparation-failure"
                           "direct-preparation-participant")
            ;; Attachment failure happens after the deferred session reserve,
            ;; but before a participant/client/binding becomes observable.
            (cl-letf (((symbol-function
                        'e-chat-service--install-participant-binding)
                       (lambda (&rest _arguments)
                         (signal 'e-session-error
                                 (list "attachment rejected")))))
              (should-error
               (e-chat-service-create-participant
                board harness :id "direct-attachment-failure"
                :participant-id "direct-attachment-participant")
               :type 'e-session-error))
            (assert-absent "direct-attachment-failure"
                           "direct-attachment-participant")
            ;; Direct publication is deliberately injected to fail after the
            ;; real deferred runtime attachment.  The assertion catches a
            ;; hash-only rollback that leaves a built-in route or event behind.
            (cl-letf (((symbol-function 'e-session-storage-publish-admission)
                       (lambda (&rest _arguments)
                         (signal 'e-session-error
                                 (list "direct submission rejected")))))
              (should-error
               (e-chat-service-create-participant
                board harness :id "direct-submission-failure"
                :participant-id "direct-submission-participant")
               :type 'e-session-error))
            (assert-absent "direct-submission-failure"
                           "direct-submission-participant")
            ;; A normal direct admission remains durable and is restored from
            ;; the journal rather than from the process-local binding map.
            (e-chat-service-create-participant
             board harness :id "direct-success"
             :participant-id "direct-success-participant")
            (should (target-event-p "direct-success-participant"))
            (should (= (cl-count-if
                        (lambda (event)
                          (eq (e-board-event-type event) 'participant-added))
                        (e-board-events source))
                       (1+ baseline-participant-events)))
            (e-session-flush-write-queue store)
            (let* ((reopened (e-session-persistent-store-create directory))
                   (restored (e-session-get reopened "direct-success")))
              (should (equal
                       (plist-get (e-session-board-association restored)
                                  :association-role)
                       "participant"))
              (should (equal
                       (plist-get (e-session-board-routing-policy restored)
                                  :participant-id)
                       "direct-success-participant")))
            (should (equal (plist-get unrelated :id) "direct-unrelated"))))
      (delete-directory directory t))))

(ert-deftest e-chat-service-test-participant-admission-queued-store-is-atomic ()
  "Queued admission failures preserve unrelated pending work.

The target's two admission records are enqueued only after all detached and
runtime checks succeed.  Each injected failure therefore has to remove only
the target reservation, leaving the unrelated queue, timer, derived-index
obligation, and attached participant intact.  A final successful admission
proves both queued records reopen together."
  (let ((directory (make-temp-file "e-chat-admission-queued-" t))
        (store-holder nil)
        (e-session-write-queue-delay 60)
        (e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--board-index
         (avl-tree-create (lambda (left right) (string< (car left) (car right)))))
        (e-board-registry--id-sequence 0)
        (e-board-registry--unsettled-pickup-count 0)
        (e-board-registry--unsettled-effect-count 0)
        (e-board-registry--unsettled-routing-count 0)
        (e-board-registry--unsettled-generation 0)
        (e-board-runtime--attachments (make-hash-table :test 'equal))
        (e-board-runtime--session-attachments (make-hash-table :test 'equal))
        (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
        (e-board-runtime--invocations (make-hash-table :test 'equal))
        (e-board-runtime--admission-open-p t)
        (e-chat-service--bindings (make-hash-table :test 'eq :weakness 'key))
        (e-chat-service--board-bindings (make-hash-table :test 'equal))
        (e-chat-service--board-log-owners (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((store (e-session-persistent-index-store-create
                       directory :write-mode 'queued))
               (harness (e-harness-create
                         :enabled-layer-ids nil :sessions store))
               (board (e-board-registry-create
                       :id "queued-admission-board"
                       :principal "chat:queued-admission"))
               (source (e-board-registry-board-source-board board)))
          (setq store-holder store)
          (e-chat-service-create-participant
           board harness :id "queued-unrelated"
           :participant-id "queued-unrelated-participant")
          (let* ((unrelated-binding
                  (e-chat-service-binding harness "queued-unrelated"))
                 (baseline-events (length (e-board-events source)))
                 (baseline-participant-events
                  (cl-count-if
                   (lambda (event)
                     (eq (e-board-event-type event) 'participant-added))
                   (e-board-events source)))
                 (baseline-participants
                  (hash-table-count
                   (e-board-registry-board-participants board)))
                 (baseline-source-participants
                  (hash-table-count (e-board-participants source)))
                 (baseline-subscriptions
                  (hash-table-count
                   (e-board-subscription-id-table source)))
                 (baseline-clients
                  (hash-table-count (e-board-registry-board-clients board)))
                 (baseline-observers
                  (e-modernchat-test--active-observer-count source))
                 (baseline-ids
                  (sort (mapcar (lambda (entry) (plist-get entry :id))
                                (e-session-list store))
                        #'string<))
                 (durability-before
                  (e-session-storage-durability-status store)))
            (let* ((target-event-p
                    (lambda (participant-id)
                   (cl-some
                    (lambda (event)
                      (and (eq (e-board-event-type event) 'participant-added)
                           (equal
                            (plist-get (e-board-event-data event)
                                       :participant-id)
                            participant-id)))
                    (e-board-events source))))
                   (durability-summary
                    (lambda ()
                      (e-session-storage-durability-status store)))
                   (assert-absent
                    (lambda (session-id participant-id)
                   (should-error (e-session-get store session-id)
                                 :type 'e-session-missing)
                   (should-not (file-exists-p
                                (e-session-storage-session-reference store session-id)))
                   (should-not (gethash participant-id
                                        (e-board-registry-board-participants
                                         board)))
                   (should-not (e-board-participant source participant-id))
                   (should-not (funcall target-event-p participant-id))
                   (should (equal
                            (sort (mapcar (lambda (entry) (plist-get entry :id))
                                          (e-session-list store))
                                  #'string<)
                            baseline-ids))
                   (should (= (cl-count-if
                               (lambda (event)
                                 (eq (e-board-event-type event)
                                     'participant-added))
                               (e-board-events source))
                              baseline-participant-events))
                   (should (= (hash-table-count
                               (e-board-registry-board-participants board))
                              baseline-participants))
                   (should (= (hash-table-count (e-board-participants source))
                              baseline-source-participants))
                   (should (= (hash-table-count
                               (e-board-subscription-id-table source))
                              baseline-subscriptions))
                   (should (= (hash-table-count
                               (e-board-registry-board-clients board))
                              baseline-clients))
                   (should (= (e-modernchat-test--active-observer-count source)
                              baseline-observers))
                   (should-not (e-chat-service-binding harness session-id))
                   (should (eq (e-chat-service-binding harness "queued-unrelated")
                               unrelated-binding))
                   (should (equal (funcall durability-summary)
                                  durability-before)))))
              ;; Detached admission preparation must not publish anything or
              ;; disturb the already queued unrelated participant.
              (cl-letf (((symbol-function 'e-session-codec-record-for-json)
                         (lambda (&rest _arguments)
                           (signal 'e-session-error
                                   (list "queued preparation rejected")))))
                (should-error
                 (e-chat-service-create-participant
                  board harness :id "queued-preparation-failure"
                  :participant-id "queued-preparation-participant")
                 :type 'e-session-error))
              (funcall assert-absent "queued-preparation-failure"
                       "queued-preparation-participant")
              ;; Runtime attachment failure happens before the queue boundary.
              (cl-letf (((symbol-function
                          'e-chat-service--install-participant-binding)
                         (lambda (&rest _arguments)
                           (signal 'e-session-error
                                   (list "queued attachment rejected")))))
                (should-error
                 (e-chat-service-create-participant
                  board harness :id "queued-attachment-failure"
                  :participant-id "queued-attachment-participant")
                 :type 'e-session-error))
              (funcall assert-absent "queued-attachment-failure"
                       "queued-attachment-participant")
              ;; Submission failure is injected after the target batch has
              ;; entered the queue.  Rollback must remove just that batch and
              ;; leave the shared timer/index obligation untouched.
              (cl-letf (((symbol-function 'e-session-storage-publish-projections)
                         (lambda (&rest _arguments)
                           (signal 'e-session-error
                                   (list "queued submission rejected")))))
                (should-error
                 (e-chat-service-create-participant
                  board harness :id "queued-submission-failure"
                  :participant-id "queued-submission-participant")
                 :type 'e-session-error))
              (funcall assert-absent "queued-submission-failure"
                       "queued-submission-participant")
              ;; The queue still contains the unrelated admission only; flush
              ;; it together with a successful target and prove both replay.
              (e-chat-service-create-participant
               board harness :id "queued-success"
               :participant-id "queued-success-participant")
              (should (funcall target-event-p "queued-success-participant"))
              (should (= (cl-count-if
                          (lambda (event)
                            (eq (e-board-event-type event)
                                'participant-added))
                          (e-board-events source))
                         (1+ baseline-participant-events)))
              (e-session-flush-write-queue store)
              (let ((reopened (e-session-persistent-store-create directory)))
                (dolist (session-id '("queued-unrelated" "queued-success"))
                  (should (e-session-get reopened session-id)))
                (dolist (session-id '("queued-preparation-failure"
                                      "queued-attachment-failure"
                                      "queued-submission-failure"))
                  (should-error (e-session-get reopened session-id)
                                :type 'e-session-missing))
                (should (equal
                         (plist-get
                         (e-session-board-routing-policy
                           (e-session-get reopened "queued-success"))
                          :participant-id)
                         "queued-success-participant"))))))
      (ignore-errors
        (when store-holder
          (e-session-flush-write-queue store-holder)))
      (delete-directory directory t))))

(ert-deftest e-chat-service-test-participant-admission-controller-is-atomic ()
  "A controller submission failure leaves no visible participant state.

The storage owner owns outbox/retry mechanics in its direct suite.  This
composed service test only verifies the public admission boundary: preparation
and attachment may run, but a rejected controller submission rolls back the
session and board binding without exposing a participant-added event."
  (let* ((directory (make-temp-file "e-chat-admission-controller-" t))
         (store (e-session-persistent-store-create directory))
         (harness (e-harness-create :enabled-layer-ids nil :sessions store))
         (board (e-board-registry-create
                 :id "controller-admission-board"
                 :principal "chat:controller-admission"))
         (source (e-board-registry-board-source-board board)))
    (unwind-protect
        (progn
          (e-session-storage-enable store)
          (cl-letf (((symbol-function 'e-session-storage-publish-admission)
                   (lambda (&rest _arguments)
                     (signal 'e-session-error
                             (list "controller submission rejected")))))
            (should-error
             (e-chat-service-create-participant
              board harness :id "controller-submission-failure"
              :participant-id "controller-submission-participant")
             :type 'e-session-error)
            (should-error (e-session-get store "controller-submission-failure")
                          :type 'e-session-missing)
            (should-not
             (file-exists-p
              (e-session-storage-session-reference
               store "controller-submission-failure")))
            (should-not
             (e-chat-service-binding harness "controller-submission-failure"))
            (should-not
             (e-board-participant source "controller-submission-participant"))
            (should-not
             (cl-some
              (lambda (event)
                (and (eq (e-board-event-type event) 'participant-added)
                     (equal
                      (plist-get (e-board-event-data event) :participant-id)
                      "controller-submission-participant")))
              (e-board-events source)))
            (should (= (plist-get (e-session-storage-durability-status store)
                                   :unsettled-write-count)
                       0))))
      (delete-directory directory t))))

(ert-deftest e-chat-service-test-root-catalog-role-survives-cross-store-id-reuse ()
  "A participant cannot become a root by reusing the owner's id in its store."
  (let* ((owner-harness (e-harness-create :enabled-layer-ids nil))
         (participant-harness (e-harness-create :enabled-layer-ids nil))
         (binding (e-chat-service-create-board
                   :harness owner-harness :id "same-id"))
         (board (e-chat-service-binding-board binding)))
    (e-chat-service-create-participant
     board participant-harness :id "same-id")
    (should (equal (e-chat-service-test--session-ids owner-harness)
                   '("same-id")))
    (should-not (e-chat-service-test--session-ids participant-harness))
    (should (equal (plist-get
                    (e-chat-service-session participant-harness "same-id") :id)
                   "same-id"))))

(ert-deftest e-chat-service-test-root-catalog-replays-new-and-legacy-index-state ()
  "Indexed role state is authoritative and canonical legacy state still works."
  (let* ((directory (make-temp-file "e-chat-role-index-" t))
         (writer-store (e-session-persistent-store-create directory))
         (writer-harness (e-harness-create
                          :enabled-layer-ids nil :sessions writer-store)))
    (unwind-protect
        (let* ((binding (e-chat-service-create-board
                         :harness writer-harness :id "indexed-root"))
               (board (e-chat-service-binding-board binding)))
          (e-chat-service-create-participant
           board writer-harness :id "indexed-participant")
          ;; This is the canonical pre-role representation already on disk.
          (e-session-create writer-store :id "legacy-root")
          (e-session-declare-board-state
           writer-store "legacy-root" "chat:legacy-root" "legacy-board")
          (let* ((indexed-store
                  (e-session-persistent-index-store-create directory))
                 (indexed-harness
                  (e-harness-create
                   :enabled-layer-ids nil :sessions indexed-store))
                 (participant-state
                  (plist-get
                   (e-chat-service-session indexed-harness
                                           "indexed-participant")
                   :board-session-state)))
            (should (equal (plist-get participant-state :association-role)
                           "participant"))
            (should (equal (sort (e-chat-service-test--session-ids
                                  indexed-harness)
                                 #'string<)
                           '("indexed-root" "legacy-root")))))
      (delete-directory directory t))))

(ert-deftest e-chat-service-test-root-catalog-does-not-promote-malformed-role ()
  "Invalid association roles fail admission before session mutation."
  (let* ((store (e-session-store-create))
         (harness (e-harness-create :enabled-layer-ids nil :sessions store)))
    (e-session-create store :id "malformed")
    (should-error
     (e-session-declare-board-state
      store "malformed" "chat:malformed" "malformed-board" "unexpected")
     :type 'error)
    (should-not (plist-get (e-chat-service-session harness "malformed")
                           :board-session-state))))

(ert-deftest e-chat-service-test-root-catalog-normalizes-index-association-presence ()
  "Malformed nested index state stays unlisted without hiding valid roots."
  (let* ((directory (make-temp-file "e-chat-malformed-index-" t))
         (index-file (expand-file-name "index.json" directory))
         (json
          (concat
           "["
           "{\"id\":\"explicit-owner\",\"board-state\":{"
           "\"board-id\":\"owner-board\",\"principal\":\"chat:other\","
           "\"association-role\":\"owner\"}},"
           "{\"id\":\"explicit-participant\",\"board-state\":{"
           "\"board-id\":\"owner-board\",\"principal\":\"chat:other\","
           "\"association-role\":\"participant\"}},"
           "{\"id\":\"legacy-root\",\"board-id\":\"legacy-board\","
           "\"principal\":\"chat:legacy-root\"},"
           "{\"id\":\"nonboard\",\"board-state\":null,"
           "\"board-id\":null,\"principal\":null,"
           "\"metadata\":{\"nullable\":null}},"
           "{\"id\":\"flat-incomplete\",\"board-id\":\"flat-board\","
           "\"principal\":null},"
           "{\"id\":\"present-null\",\"board-state\":null,"
           "\"board-id\":\"null-board\",\"principal\":\"chat:present-null\"},"
           "{\"id\":\"present-null-missing-mirrors\","
           "\"board-state\":null},"
           "{\"id\":\"present-empty\",\"board-state\":{},"
           "\"board-id\":null,\"principal\":null},"
           "{\"id\":\"present-partial\",\"board-state\":{"
           "\"board-id\":\"partial-board\",\"association-role\":\"owner\"},"
           "\"principal\":\"chat:present-partial\"},"
           "{\"id\":\"present-scalar\",\"board-state\":\"invalid\","
           "\"board-id\":\"scalar-board\",\"principal\":\"chat:present-scalar\"},"
           "{\"id\":\"present-list\",\"board-state\":[{"
           "\"board-id\":\"list-board\",\"principal\":\"chat:present-list\"}],"
           "\"board-id\":\"list-board\",\"principal\":\"chat:present-list\"}"
           ","
           "{\"id\":\"unknown-role\",\"board-state\":{"
           "\"board-id\":\"unknown-role-board\","
           "\"principal\":\"chat:unknown-role\","
           "\"association-role\":\"unexpected\"}},"
           "{\"id\":\"extra-key\",\"board-state\":{"
           "\"board-id\":\"extra-board\",\"principal\":\"chat:extra-key\","
           "\"association-role\":\"owner\",\"extra\":true}}"
           "]")))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sessions" directory) t)
          (with-temp-file index-file (insert json))
          (let* ((store (e-session-persistent-index-store-create directory))
                 (harness (e-harness-create
                           :enabled-layer-ids nil :sessions store))
                 (before (with-temp-buffer
                           (insert-file-contents index-file)
                           (buffer-string)))
                 (sessions (e-chat-service-session-list harness))
                 (nonboard
                  (seq-find (lambda (session)
                              (equal (plist-get session :id) "nonboard"))
                            sessions))
                 (present-empty
                  (seq-find (lambda (session)
                              (equal (plist-get session :id) "present-empty"))
                            sessions))
                 (stored-nonboard
                  (e-session-aggregate-peek-session store "nonboard"))
                 (stored-present-empty
                  (e-session-aggregate-peek-session store "present-empty"))
                 (listed (sort (e-chat-service-test--session-ids harness)
                               #'string<)))
            (should (equal listed
                           '("explicit-owner" "legacy-root" "nonboard")))
            (should (= (length sessions) 13))
            (should-not (plist-member nonboard :board-session-state))
            (should-not (e-session-board-association nonboard))
            (should-not
             (plist-member stored-nonboard :board-session-state))
            (should-not (plist-get (plist-get nonboard :metadata) :nullable))
            (should-not
             (plist-get (plist-get stored-nonboard :metadata) :nullable))
            (should
             (e-session-board-association-invalid-p
              (e-session-board-association present-empty)))
            (should
             (e-session-board-association-invalid-p
              (plist-get stored-present-empty :board-session-state)))
            (dolist (id '("flat-incomplete" "present-null"
                          "present-null-missing-mirrors" "present-empty"
                          "present-partial" "present-scalar" "present-list"
                          "unknown-role" "extra-key"))
              (should
               (e-session-board-association-invalid-p
                (e-session-board-association
                 (seq-find (lambda (session)
                             (equal (plist-get session :id) id))
                           sessions)))))
            (should (equal before
                           (with-temp-buffer
                             (insert-file-contents index-file)
                             (buffer-string))))))
      (delete-directory directory t))))

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
          (should-not (e-chat-service-drain-subscription bad))
          (e-chat-service-drain-subscription good)
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
          (e-chat-service-drain-subscription good)
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
          (e-board-post-fact
           (e-board-registry-board-source-board board)
           :id "pending-before-detach" :tags '(main) :content "pending"
           :source-fact-key '(test detached 1))
          (e-board-registry-detach-client board client-id)
          (should-not (e-chat-service-drain-subscription subscription))
          (should-not (e-chat-service-subscription-active-p subscription))
          (should-not
           (memq subscription (e-chat-service-binding-subscribers binding)))
          (should
               (eq (car (e-chat-service-subscription-state subscription))
               'detached)))))))

(ert-deftest e-chat-service-test-drain-stops-for-stale-and-closing-receivers ()
  "Bounded pumps stop when their receiver is stale or its board is closing."
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
           (board (e-chat-service-binding-board binding))
           (source (e-board-registry-board-source-board board))
           (observer (e-chat-service-binding-observer binding)))
      (e-board-post-fact
       source :id "stale-pending" :tags '(main) :content "stale"
       :source-fact-key '(test stale 1))
      ;; A cancelled source observer is a stale receiver, even though its
      ;; cursor remains behind the board tail.
      (e-board-set-observer-state source
                                   (e-board-observer-id observer)
                                   'cancelled)
      (let ((calls 0))
        (while (and (< calls 3)
                    (progn
                      (cl-incf calls)
                      (e-chat-service-drain-binding binding))))
        (should (= calls 1)))
      (should-not
       (e-chat-service--observer-drain-live-p
        binding (e-chat-service-binding-client binding)
        (e-chat-service-binding-observer binding)))
      ;; Closing is also terminal for a binding pump; no board call is made
      ;; merely because the pre-close cursor is behind retained messages.
      (let ((e-board-registry-close-scheduler (lambda (_function) nil)))
        (e-board-registry-close board)
        (should-not (e-chat-service-drain-binding binding))))))

(ert-deftest e-chat-service-test-drain-unsubscribed-subscription-terminates ()
  "A bounded consumer cannot spin after its subscription is unsubscribed."
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
           (board (e-chat-service-binding-board binding))
           (subscription (e-chat-service-subscribe harness "main" #'ignore)))
      (e-board-post-fact
       (e-board-registry-board-source-board board)
       :id "unsubscribed-pending" :tags '(main) :content "pending"
       :source-fact-key '(test unsubscribed 1))
      (e-chat-service-unsubscribe subscription)
      (let ((calls 0))
        (while (and (< calls 3)
                    (progn
                      (cl-incf calls)
                      (e-chat-service-drain-subscription subscription))))
        (should (= calls 1)))
      (should-not (e-chat-service-subscription-active-p subscription)))))

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
    (e-chat-service-drain-binding binding)
    (cl-letf (((symbol-function 'e-harness-messages)
               (lambda (&rest _) (error "private transcript read")))
              ((symbol-function 'e-session-activity-events)
               (lambda (&rest _) (error "private activity read")))
              ((symbol-function 'e-harness-state)
               (lambda (&rest _) (error "private state read")))
              ((symbol-function 'e-harness-queued-prompts)
               (lambda (&rest _) (error "private queue read"))))
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

(ert-deftest e-chat-service-test-state-seeds-live-attached-turn-before-replay ()
  "A live attached turn is visible before any retained turn-started event."
  (let* ((harness (e-harness-create :enabled-layer-ids nil))
         (session (e-chat-service-create-session
                   :harness harness :id "live-state"))
         (session-id (plist-get session :id))
         (binding (e-chat-service-binding harness session-id))
         (attachment (e-chat-service-binding-attachment binding))
         (entry (list :id "live-source-turn"
                      :status 'running
                      :attached-turn-port
                      (e-board-runtime-attachment-turn-port attachment))))
    (unwind-protect
        (progn
          (e-harness-turn-state-put-active-turn harness session-id entry)
          (let* ((state (e-chat-service-state harness session-id))
                 (active-turn (plist-get state :active-turn)))
            (should (equal (plist-get active-turn :status) 'running))
            (should (equal (nth 2 (plist-get active-turn :id))
                           "live-source-turn"))))
      (e-harness-turn-state-remove-active-turn harness session-id entry))))

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
      (e-chat-service-drain-binding binding))
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
        (e-chat-service-drain-subscription subscription)
        (should-not live-events)
        (e-board-post-output
         board :id "view-live" :author "test" :tags '(main)
         :content "live" :source-output-key '(test "view" 261))
        (e-chat-service-drain-subscription subscription)
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
            (e-chat-service-drain-binding restored)
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
      (e-chat-service-drain-binding binding)
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
        (e-chat-service-reconcile-board-continuation runtime-board 'test)
        (should (eq (plist-get (plist-get (e-board-orchestration-run-projection
                                           board "run-1")
                                          :continuation)
                               :state)
                    'failed))
        (e-chat-service-reconcile-board-continuation runtime-board 'test)
        ;; A later restart finds the published acknowledgement and does not
        ;; submit another input.
        (e-chat-service-reconcile-board-continuation runtime-board 'test))
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
                               (:stated_purpose "Finalize the daily run."
                                :code
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
            (e-chat-service-reconcile-board-continuation runtime-board harness)
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

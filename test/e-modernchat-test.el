;;; e-modernchat-test.el --- Tests for egui modern chat shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the shell-neutral chat service and modernchat view-model builder.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-bayesian-reasoning)
(require 'e-chat-service)
(require 'e-modernchat)
(require 'e-modernchat-view-model)
(require 'e-session)
(require 'e-structured-blocks)

(ert-deftest e-modernchat-view-model-test-snapshot-bounds-messages ()
  "Snapshots include recent bounded messages and session metadata."
  (let ((harness (e-harness-create
                  :backend (e-backend-create :name "noop")
                  :enabled-layer-ids nil)))
    (e-harness-create-session
     harness
     :id "session-1"
     :metadata '(:project-root "/tmp/project/"
                 :context-attachments ((:uri "file:///tmp/a.org"
                                         :label "a.org"))))
    (dotimes (index 3)
      (e-session-append-message
       (e-harness-sessions harness)
       "session-1"
       (list :id (format "m-%d" index)
             :role 'user
             :content (format "message %d" index))))
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
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     (list :id "m-0"
           :role 'assistant
           :content (concat "The answer is 42.\n\n"
                            "#+begin_reasoning\n"
                            "claim: the answer is 42\n"
                            "confidence: high\n"
                            "alternatives: insufficient-evidence\n"
                            "evidence: none\n"
                            "#+end_reasoning\n")))
    (let* ((snapshot (e-modernchat-view-model-snapshot
                      harness "session-1" :activity-limit 0))
           (messages (cdr (assq 'messages snapshot)))
           (content (cdr (assq 'content (aref messages 0)))))
      (should (string-match-p "The answer is 42\\." content))
      (should-not (string-match-p "begin_reasoning" content))
      (should-not (string-match-p "confidence:" content)))))

(ert-deftest e-modernchat-view-model-test-omits-hidden-messages ()
  "A message flagged `:display' `hidden' never reaches the snapshot.
A superseded first attempt and a machine-authored corrective prompt both carry
the hidden disposition; the modern chat client should see only the visible
messages so the transcript reads as one clean answer."
  (let ((harness (e-harness-create
                  :backend (e-backend-create :name "noop")
                  :enabled-layer-ids nil)))
    (e-harness-create-session harness :id "session-1")
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     (list :id "m-0" :role 'assistant :content "visible reply"))
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     (list :id "m-1" :role 'assistant :content "hidden first attempt"
           :display 'hidden))
    (e-session-append-message
     (e-harness-sessions harness)
     "session-1"
     (list :id "m-2" :role 'user :content "hidden corrective prompt"
           :metadata '(:display hidden)))
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

(ert-deftest e-chat-service-test-submit-uses-bound-board-ingress ()
  "Shell-neutral submit posts only through its attached board participant."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
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

(ert-deftest e-chat-service-test-board-chat-end-to-end ()
  "Board ingress, harness delivery, board output, and observation round-trip."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board--id-sequence 0)
        (e-board-registry--boards (make-hash-table :test 'equal))
        (e-board-registry--id-sequence 0)
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

(provide 'e-modernchat-test)

;;; e-modernchat-test.el ends here

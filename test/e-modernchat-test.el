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

(ert-deftest e-chat-service-test-submit-delegates-to-chat-session ()
  "Shell-neutral submit service delegates to chat-session submit."
  (let ((called nil))
    (cl-letf (((symbol-function 'e-chat-session-submit)
               (lambda (harness session-id prompt &rest args)
                 (setq called (list harness session-id prompt args))
                 :submitted)))
      (should (eq (e-chat-service-submit-session
                   'harness "s1" "hello" :references '(r1) :metadata '(:m t))
                  :submitted))
      (should (equal (list (nth 0 called) (nth 1 called) (nth 2 called))
                     '(harness "s1" "hello")))
      (should (equal (plist-get (nth 3 called) :references) '(r1)))
      (should (equal (plist-get (nth 3 called) :metadata) '(:m t))))))

(provide 'e-modernchat-test)

;;; e-modernchat-test.el ends here

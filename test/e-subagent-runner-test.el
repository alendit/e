;;; e-subagent-runner-test.el --- Tests for subagent spawn and runner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the subagent registry, spawn coordination, seeding, result
;; precedence, and lifecycle transitions using a fake runner, plus the
;; capability action round-trip and the assertion that subagents stay out of
;; model-facing tool definitions.

;;; Code:

(require 'ert)
(require 'e-backend)
(require 'e-capabilities)
(require 'e-harness)
(load (expand-file-name "e-harness-test-support.el" (file-name-directory (or load-file-name buffer-file-name))) nil nil t)
(require 'e-harness-instances)
(require 'e-session)
(require 'e-store)
(require 'e-subagent-registry)
(require 'e-subagent-runner)
(require 'e-subagent-actions)
(require 'e-subagents)
(require 'e-waitable)
(require 'e-work)
(require 'e-request)

(defmacro e-subagent-runner-test--with-instances (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--id-sequence 0)
         (e-board-runtime--attachments (make-hash-table :test 'equal))
         (e-board-runtime--session-attachments (make-hash-table :test 'equal))
         (e-board-runtime--endpoint-attachments (make-hash-table :test 'equal))
         (e-board-runtime--invocations (make-hash-table :test 'equal))
         (e-board-runtime--producer-bindings (make-hash-table :test 'equal))
         (e-board-runtime--producer-epoch 0)
         (e-board-runtime--producer-head nil)
         (e-board-runtime--producer-tail nil)
         (e-board-runtime--producer-drain-scheduled nil)
         (e-board-runtime--producer-scheduler (lambda (_callback)))
         (e-board-runtime--work-activity-mailboxes (make-hash-table :test 'equal))
         (e-board-runtime--pending-activity-head nil)
         (e-board-runtime--pending-activity-tail nil)
         (e-board-runtime--pending-activity-set (make-hash-table :test 'equal))
         (e-board-runtime--activity-drain-scheduled nil)
         (e-board-runtime--admission-open-p t)
         (e-board-runtime--unsettled-producer-count 0)
         (e-board-runtime--unsettled-generation 0)
         (e-chat-service--bindings
          (make-hash-table :test 'eq :weakness 'key))
         (e-subagent--producer-bindings (make-hash-table :test 'equal))
         (e-subagent--configured-harnesses
          (make-hash-table :test 'eq :weakness 'key))
         (e-work--unsettled-count 0)
         (e-work--unsettled-generation 0)
         (e-work--unsettled-change-functions nil))
     (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
               ((symbol-function 'timerp) (lambda (value) (eq value 'timer)))
               ((symbol-function 'cancel-timer) #'ignore))
       (e-harness-instance-register
        :id :reviewer
        :name "Reviewer"
        :kind 'reviewer
        :subagent t
        :description "Use for review."
        :factory (lambda () (e-harness-create
                             :backend (e-backend-fake-create :items nil))))
       ,@body)))

(defun e-subagent-runner-test--capturing-runner (captured)
  "Return a runner that records its call into CAPTURED and never settles."
  (lambda (child-harness child-session-id prompt seed-messages on-settle)
    (setcar captured (list :child-harness child-harness
                           :child-session-id child-session-id
                           :prompt prompt
                           :seed-messages seed-messages
                           :on-settle on-settle))
    ;; Seed like the real runner so seeding is observable through the session.
    (e-subagent--seed-child child-harness child-session-id seed-messages)
    (list :cancel #'ignore)))

(defun e-subagent-runner-test--spawn
    (registry parent-harness parent-session-id &rest arguments)
  "Spawn test work from one explicit parent turn."
  (apply #'e-subagent-spawn registry parent-harness parent-session-id
         :source-turn-id "parent-turn" arguments))

(defun e-subagent-runner-test--resume
    (registry subagent-id &optional prompt runner)
  "Resume test work from one explicit parent turn."
  (e-subagent-resume registry subagent-id prompt runner
                     :source-turn-id "parent-resume-turn"))

(ert-deftest e-subagent-runner-test-spawn-records-lineage-and-seeds ()
  "Spawn creates a child under the parent lineage and seeds explicit context."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :project-root "/tmp/example-project/"))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               registry parent "parent-1"
               :type :reviewer
               :prompt "Review tmp://plan.org"
               :seed-messages (list '(:role user :content "context note"))
               :label "review plan.org"
               :runner (e-subagent-runner-test--capturing-runner captured)))
             (child-session-id (plist-get record :session-id))
             (call (car captured))
             (child-harness (plist-get call :child-harness))
             (work-handle
              (e-subagent-registry-work-handle
               registry (plist-get record :subagent-id))))
        (should (eq (plist-get record :type) :reviewer))
        (should (eq (plist-get record :role) 'reviewer))
        (should (eq (plist-get record :status) 'running))
        (should (equal (plist-get record :await-ref)
                       (format "subagent:%s"
                               (plist-get record :subagent-id))))
        (should (equal (plist-get record :parent-session-id) "parent-1"))
        (should (equal (plist-get call :prompt) "Review tmp://plan.org"))
        (should (equal (plist-get (e-work-handle-context work-handle) :turn-id)
                       "parent-turn"))
        ;; Child session carries durable lineage metadata sharing the parent id.
        (let ((metadata (plist-get (e-session-get
                                    (e-harness-sessions child-harness)
                                    child-session-id)
                                   :metadata)))
          (should (equal (plist-get metadata :parent-session-id) "parent-1"))
          (should (equal (plist-get metadata :tmp-lineage-id) "parent-1"))
          (should (equal (plist-get metadata :project-root)
                         "/tmp/example-project/"))
          (should (equal (plist-get metadata :subagent-label) "review plan.org")))
        ;; The explicit seed landed in the child's own store before the task.
        (should (equal (mapcar (lambda (m) (plist-get m :content))
                               (e-harness-messages child-harness
                                                   child-session-id))
                       '("context note")))
        (e-board-runtime-drain-producers)
        (let* ((binding (e-chat-service-binding parent "parent-1"))
               (child-binding
                (e-chat-service-binding child-harness child-session-id))
               (messages (e-board-messages
                          (e-board-registry-board-source-board
                           (e-chat-service-binding-board binding))))
               (facts (cl-remove-if-not
                       (lambda (message)
                         (eq (e-board-message-kind message) 'fact))
                       messages)))
          (should (eq (e-chat-service-binding-board child-binding)
                      (e-chat-service-binding-board binding)))
          (should (equal
                   (e-chat-service-binding-default-to child-binding)
                   (e-board-registry-participant-id
                    (e-board-runtime-attachment-participant
                     (e-chat-service-binding-attachment child-binding)))))
          (should (equal (e-chat-service-binding-default-tags child-binding)
                         '(subagent)))
          (should (>= (length facts) 2))
          (should (cl-every
                   (lambda (message)
                     (string-match-p
                      "\\`producer:subagent:brd_[[:alnum:]]+:parent-1\\'"
                      (e-board-message-author message)))
                   facts))
          (let ((tags (mapcar #'e-board-message-tags facts)))
            (should (member '(subagent change queued) tags))
            (should (member '(subagent change running) tags))))
        ;; A real child progress event publishes with the spawning turn instead
        ;; of failing later from the activity-drain timer.
        (e-subagent--record-progress
         registry (plist-get record :subagent-id) work-handle 'tool-finished)
        (e-board-runtime--drain-activity-mailboxes)
        (let* ((binding (e-chat-service-binding parent "parent-1"))
               (activities
                (cl-remove-if-not
                 (lambda (message) (eq (e-board-message-kind message) 'activity))
                 (e-board-messages
                  (e-board-registry-board-source-board
                   (e-chat-service-binding-board binding))))))
          (should (= (length activities) 1))
          (should (equal (e-board-message-source-turn-id (car activities))
                         "parent-turn")))))))

(ert-deftest e-subagent-runner-test-final-message-is-default-result ()
  "A settle with a summary records it as the compact result."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (settle (plist-get (car captured) :on-settle)))
        (funcall settle 'done :summary "3 issues found")
        (let ((final (e-subagent-registry-get registry subagent-id)))
          (should (eq (plist-get final :status) 'done))
          (should (equal (plist-get final :result-summary) "3 issues found")))))))

(ert-deftest e-subagent-runner-test-report-overrides-final-message ()
  "A child-reported result is authoritative over a later final message."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (child-session-id (plist-get record :session-id))
             (settle (plist-get (car captured) :on-settle)))
        (e-subagent-report registry child-session-id
                           (list '(:kind org-link :uri "tmp://r.org" :label "review"))
                           "reported summary")
        ;; A later final message must not overwrite the reported result.
        (funcall settle 'done :summary "chatter final message")
        (let ((final (e-subagent-registry-get registry subagent-id)))
          (should (equal (plist-get final :result-summary) "reported summary"))
          (should (equal (plist-get final :outputs)
                         (list '(:kind org-link :uri "tmp://r.org" :label "review"))))
          (should (eq (plist-get final :status) 'done)))))))

(ert-deftest e-subagent-runner-test-interrupt-and-shutdown ()
  "Interrupt calls the cancel function and marks the record cancelled."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (cancelled nil))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on-settle)
                                (list :cancel (lambda () (setq cancelled t))))))
             (subagent-id (plist-get record :subagent-id)))
        (e-subagent-interrupt registry subagent-id)
        (should cancelled)
        (should (eq (plist-get (e-subagent-registry-get registry subagent-id)
                               :status)
                    'cancelled))))))

(ert-deftest e-subagent-runner-test-resume-restarts-failed-child ()
  "Resume drives a new turn on a failed child's live session and clears residue.
The child session and its context stay intact; resume re-opens the record to
`running', clears the prior error, and re-arms the settle callback so the new
turn's result lands."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (first-handle (e-subagent-registry-work-handle registry subagent-id))
             (settle (plist-get (car captured) :on-settle)))
        ;; The first turn dies on a transient backend error.
        (funcall settle 'failed :error "Backend returned no assistant output")
        (should (eq (e-subagent-registry-status registry subagent-id) 'failed))
        ;; Resume with a fresh capturing runner; the record re-opens.
        (let ((resumed (e-subagent-runner-test--resume
                        registry subagent-id "keep going"
                        (e-subagent-runner-test--capturing-runner captured))))
          (should (eq (plist-get resumed :status) 'running))
          (should-not (plist-get resumed :error))
          (should (equal (plist-get (car captured) :prompt) "keep going"))
          ;; A fresh awaitable work handle is minted for the resumed turn.
          (let ((second-handle
                 (e-subagent-registry-work-handle registry subagent-id)))
            (should (e-work-handle-p second-handle))
            (should-not (eq second-handle first-handle)))
          ;; The resumed turn settles and its result is recorded.
          (funcall (plist-get (car captured) :on-settle)
                   'done :summary "finished on retry")
          (let ((final (e-subagent-registry-get registry subagent-id)))
            (should (eq (plist-get final :status) 'done))
            (should (equal (plist-get final :result-summary)
                           "finished on retry"))))))))

(ert-deftest e-subagent-runner-test-resume-defaults-prompt ()
  "Resume with no prompt uses a minimal continue prompt."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (settle (plist-get (car captured) :on-settle)))
        (funcall settle 'cancelled)
        (e-subagent-runner-test--resume registry subagent-id nil
                           (e-subagent-runner-test--capturing-runner captured))
        (should (stringp (plist-get (car captured) :prompt)))
        (should-not (string-empty-p (plist-get (car captured) :prompt)))))))

(ert-deftest e-subagent-runner-test-resume-refuses-running-and-shutdown ()
  "Resume refuses a running child, a shut-down child, and a done child."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-test-create-board-session parent :id "parent-1")
      ;; A running child is not resumable.
      (let* ((running (e-subagent-runner-test--spawn
                       registry parent "parent-1"
                       :type :reviewer :prompt "go" :runner noop))
             (running-id (plist-get running :subagent-id)))
        (should-error (e-subagent-runner-test--resume registry running-id nil noop)
                      :type 'user-error))
      ;; A deliberately shut-down child is refused even though it is terminal.
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id)))
        (e-subagent-shutdown registry subagent-id)
        (should (eq (e-subagent-registry-status registry subagent-id) 'cancelled))
        (should (e-subagent-registry-shutdown-p registry subagent-id))
        (should-error (e-subagent-runner-test--resume registry subagent-id nil noop)
                      :type 'user-error)))))

(ert-deftest e-subagent-runner-test-list-scopes-to-parent ()
  "List returns only the calling parent's direct children."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-test-create-board-session parent :id "parent-1")
      (e-harness-test-create-board-session parent :id "parent-2")
      (e-subagent-runner-test--spawn registry parent "parent-1"
                        :type :reviewer :prompt "a" :runner noop)
      (e-subagent-runner-test--spawn registry parent "parent-2"
                        :type :reviewer :prompt "b" :runner noop)
      (should (equal (mapcar (lambda (r) (plist-get r :parent-session-id))
                             (e-subagent-registry-list registry "parent-1"))
                     '("parent-1"))))))

(ert-deftest e-subagent-runner-test-unknown-type-signals ()
  "Spawning a non-subagent type signals."
  (e-subagent-runner-test--with-instances
    (e-harness-instance-register
     :id :chat-plain
     :name "Chat"
     :kind 'chat
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-board-session parent :id "parent-1")
      (should-error
       (e-subagent-runner-test--spawn registry parent "parent-1"
                         :type :chat-plain :prompt "go"
                         :runner (lambda (_h _s _p _seed _on)
                                   (list :cancel #'ignore)))
       :type 'e-subagent-unknown-type))))

(ert-deftest e-subagent-runner-test-steer-and-send-dispatch ()
  "Steer and send route through the child board application service."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (steered nil)
           (queued nil))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (subagent-id (plist-get record :subagent-id)))
        (cl-letf (((symbol-function 'e-chat-service-steer-session)
                   (lambda (_h _s prompt &rest _) (setq steered prompt) "turn-1"))
                  ((symbol-function 'e-chat-service-queue-session)
                   (lambda (_h _s prompt &rest _) (setq queued prompt) nil)))
          (e-subagent-steer registry subagent-id "steer this")
          (e-subagent-send registry subagent-id "follow up")
          (should (equal steered "steer this"))
          (should (equal queued "follow up")))))))

(ert-deftest e-subagent-runner-test-send-refuses-settled-child ()
  "Send refuses a failed, cancelled, or done child instead of queueing.
A settled child has no active turn, so `e-harness-test-queue-prompt' would raise the
low-level `e-harness-no-active-turn'.  Send must guard status up front like
`e-subagent-resume', point failed/cancelled children at resume, and never reach
the harness."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil))
           (queued nil))
      (e-harness-test-create-board-session parent :id "parent-1")
      (cl-letf (((symbol-function 'e-harness-test-queue-prompt)
                 (lambda (_h _s prompt &rest _) (setq queued prompt) nil)))
        ;; A failed child is not sendable; the error points at resume.
        (let* ((record (e-subagent-runner-test--spawn
                        registry parent "parent-1"
                        :type :reviewer :prompt "go"
                        :runner (e-subagent-runner-test--capturing-runner captured)))
               (subagent-id (plist-get record :subagent-id)))
          (funcall (plist-get (car captured) :on-settle)
                   'failed :error "boom")
          (should (eq (e-subagent-registry-status registry subagent-id) 'failed))
          (let ((err (should-error
                      (e-subagent-send registry subagent-id "follow up")
                      :type 'user-error)))
            (should (string-match-p "resume" (cadr err))))
          (should-not queued))
        ;; A done child is likewise refused.
        (let* ((record (e-subagent-runner-test--spawn
                        registry parent "parent-1"
                        :type :reviewer :prompt "go"
                        :runner (e-subagent-runner-test--capturing-runner captured)))
               (subagent-id (plist-get record :subagent-id)))
          (funcall (plist-get (car captured) :on-settle) 'done :summary "ok")
          (should (eq (e-subagent-registry-status registry subagent-id) 'done))
          (should-error (e-subagent-send registry subagent-id "follow up")
                        :type 'user-error)
          (should-not queued))))))

(ert-deftest e-subagent-runner-test-raw-read-returns-excerpt-and-uri ()
  "Raw read returns a bounded excerpt and the child session:// URI."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :seed-messages (list '(:role user :content "one")
                                           '(:role assistant :content "two"))
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (child-session-id (plist-get record :session-id))
             (child-harness (plist-get (car captured) :child-harness))
             (binding (e-chat-service-binding child-harness child-session-id))
             (participant-id
              (e-board-registry-participant-id
               (e-board-runtime-attachment-participant
                (e-chat-service-binding-attachment binding)))))
        (e-board-post-output
         (e-board-registry-board-source-board
          (e-chat-service-binding-board binding))
         :author (format "participant:%s" participant-id)
         :content "visible output"
         :subject-participant-id participant-id
         :source-turn-id "turn-visible"
         :source-output-key (list participant-id 1 1))
        (e-chat-service--drain-observer binding)
        (let ((raw (e-subagent-raw-read registry subagent-id 1)))
          (should (equal (plist-get raw :session-uri)
                         (format "session://e/sessions/%s/messages"
                                 child-session-id)))
          ;; Bounded to the last board-visible message only; private seed
          ;; context is intentionally absent from the observable transcript.
          (should (equal (mapcar (lambda (m) (plist-get m :content))
                                 (plist-get raw :messages))
                         '("visible output"))))))))

(ert-deftest e-subagent-runner-test-configure-type-toggles-layers ()
  "configure-type enables and disables layers on the type's shared harness."
  (e-subagent-runner-test--with-instances
    ;; Give the reviewer type a real harness with a couple of default layers so
    ;; enable/disable have something to move.  `os-base' and `emacs-base' are
    ;; ordinary registered layers.
    (let* ((harness (e-harness-instance-get-or-create :reviewer)))
      (e-harness-set-enabled-layer-ids harness '(os-base))
      (let ((result (e-subagent-configure-type
                     :reviewer :enable-layers '("emacs-base")
                     :disable-layers '("os-base"))))
        (should (eq (plist-get result :type) :reviewer))
        (should (memq 'emacs-base (plist-get result :enabled-layers)))
        (should-not (memq 'os-base (plist-get result :enabled-layers)))))))

(ert-deftest e-subagent-runner-test-configure-type-before-first-spawn-persists ()
  "configure-type preserves pre-spawn overrides when child defaults are seeded."
  (e-subagent-runner-test--with-instances
    (e-harness-instance-register
     :id :lean
     :name "Lean"
     :kind 'tool-user
     :subagent t
     :description "Lean tool runner."
     :layers '(harness-base os-base)
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-test-create-board-session parent :id "parent-1")
      (e-subagent-configure-type :lean :enable-layers '("web"))
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :lean :prompt "go" :runner noop))
             (harness (e-subagent-registry-child-harness
                       registry (plist-get record :subagent-id))))
        (should (equal (e-harness-enabled-layer-ids harness)
                       '(harness-base os-base subagents-child web)))))))

(ert-deftest e-subagent-runner-test-instance-layers-seed-child-harness ()
  "An instance's declared :layers/:layer-config seed its child harness once.
A later configure-type override is preserved across subsequent spawns."
  (e-subagent-runner-test--with-instances
    (e-harness-instance-register
     :id :lean
     :name "Lean"
     :kind 'tool-user
     :subagent t
     :description "Lean tool runner."
     :layers '(harness-base os-base)
     :layer-config '((agents-std-context :skills-include ("writing")))
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn registry parent "parent-1"
                                       :type :lean :prompt "go" :runner noop))
             (harness (e-subagent-registry-child-harness
                       registry (plist-get record :subagent-id))))
        ;; Declared layers land on the child harness, with the always-added
        ;; child report layer appended.
        (should (equal (e-harness-enabled-layer-ids harness)
                       '(harness-base os-base subagents-child)))
        (should (equal (e-harness-capability-config harness 'agents-std-context)
                       '(:skills-include ("writing"))))
        ;; A parent override persists; the second spawn does not re-seed.
        (e-subagent-configure-type :lean :enable-layers '("web"))
        (e-subagent-runner-test--spawn registry parent "parent-1"
                          :type :lean :prompt "again" :runner noop)
        (should (memq 'web (e-harness-enabled-layer-ids harness)))))))

(ert-deftest e-subagent-runner-test-child-inherits-prompt-cache-policy ()
  "A child without cache policy derives its own key from the parent's opt-in."
  (e-subagent-runner-test--with-instances
    (let ((e-subagent-child-layer-ids nil))
      (e-harness-instance-register
       :id :cached-child
       :name "Cached child"
       :kind 'tool-user
       :subagent t
       :description "Child used to verify inherited cache policy."
       :factory (lambda ()
                  (e-harness-create
                   :backend (e-backend-fake-create :items nil)
                   :default-options '(:model "child-model"))))
      (let* ((registry (e-subagent-registry-create))
             (parent
              (e-harness-create
               :backend (e-backend-fake-create :items nil)
               :default-options '(:model "parent-model"
                                  :prompt-cache-default t
                                  :prompt-cache-retention "24h")))
             (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
        (e-harness-test-create-board-session parent :id "parent-1")
        (let* ((record
                (e-subagent-runner-test--spawn registry parent "parent-1"
                                  :type :cached-child
                                  :prompt "go"
                                  :runner noop))
               (child
                (e-subagent-registry-child-harness
                 registry (plist-get record :subagent-id)))
               (child-session-id (plist-get record :session-id))
               (session-options
                (e-harness-session-options child child-session-id))
               (turn-options (e-harness-turn-options child child-session-id)))
          (should (eq (plist-get session-options :prompt-cache-default) t))
          (should (equal (plist-get session-options :prompt-cache-retention)
                         "24h"))
          (should (stringp (plist-get turn-options :prompt-cache-key)))
          (should (equal (plist-get turn-options :prompt-cache-retention)
                         "24h"))
          (should-not (plist-member turn-options :prompt-cache-default)))))))

(ert-deftest e-subagent-runner-test-configure-type-passes-layer-config ()
  "configure-type writes a capability's runtime config on the type's harness.
This is the generic way to pass or overwrite layer configuration, e.g. the
`agents-std-context' skill allowlist."
  (e-subagent-runner-test--with-instances
    (let ((harness (e-harness-instance-get-or-create :reviewer)))
      (e-subagent-configure-type
       :reviewer
       :layer-config '((agents-std-context :skills-include ("writing"))))
      (should (equal (e-harness-capability-config harness 'agents-std-context)
                     '(:skills-include ("writing")))))))

(ert-deftest e-subagent-runner-test-parent-capability-actions-and-skill ()
  "The parent capability exposes spawn/observe/steer actions and a skill.
report is child-side and must not be on the parent surface."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (capability (e-subagents-parent-capability-create :registry registry))
           (store (e-store-create)))
      (should (eq (e-capability-id capability) 'subagents))
      (dolist (action '(:spawn :list :status :read :steer :send :resume
                        :interrupt :shutdown :configure-type))
        (should (e-capabilities-action-spec capability action)))
      (should-not (e-capabilities-action-spec capability :report))
      ;; Actions only: no model-facing tool definitions, like elisp-job.
      (should-not (e-capability-tools capability))
      (e-capabilities-register-resources capability store)
      (let ((uris (mapcar #'e-store-entry-uri (e-store-list store))))
        (should (member "e://subagents/skills/subagents" uris))
        (should (member "e://subagents/refs/types.md" uris)))
      (let ((skill (e-store-read store "e://subagents/skills/subagents" nil)))
        (should (string-match-p "spawn" skill))
        (should (string-match-p "Delegate by replacement" skill))
        (should (string-match-p "Use `any`" skill))))))

(ert-deftest e-subagent-runner-test-child-capability-is-report-only ()
  "The child capability exposes only report, and no spawn surface or catalog."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (capability (e-subagents-child-capability-create :registry registry))
           (store (e-store-create)))
      (should (eq (e-capability-id capability) 'subagents))
      (should (e-capabilities-action-spec capability :report))
      (dolist (action '(:spawn :list :status :read :steer :send
                        :interrupt :shutdown :configure-type))
        (should-not (e-capabilities-action-spec capability action)))
      (should-not (e-capability-tools capability))
      ;; No types context and no catalog resource: a lean child stays lean.
      (should-not (e-capability-context-providers capability))
      (e-capabilities-register-resources capability store)
      (should-not (e-store-list store)))))

(ert-deftest e-subagent-runner-test-child-gets-report-layer ()
  "Every spawned child harness carries the child-side report action."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (child (e-subagent-registry-child-harness
                     registry (plist-get record :subagent-id)))
             (caps (mapcar #'e-capability-id
                           (e-harness-effective-capabilities child))))
        (should (memq 'subagents-child (e-harness-enabled-layer-ids child)))
        (should (memq 'subagents caps))))))

(ert-deftest e-subagent-runner-test-spawn-exposes-awaitable-work-handle ()
  "A spawned subagent carries an `e-work' handle that settles with its result."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (handle (e-subagent-registry-work-handle registry subagent-id)))
        (should (e-work-handle-p handle))
        (should-not (e-request-terminal-p (e-work-handle-lifecycle handle)))
        (funcall (plist-get (car captured) :on-settle)
                 'done :summary "done" :outputs [:x])
        (should (eq (plist-get (e-work-status handle) :state) 'finished))
        (should (equal (plist-get (plist-get (e-work-status handle) :result)
                                  :summary)
                       "done"))))))

(ert-deftest e-subagent-runner-test-work-handle-fails-on-failed-settle ()
  "A failed subagent settle fails the work handle."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (handle (e-subagent-registry-work-handle
                      registry (plist-get record :subagent-id))))
        (funcall (plist-get (car captured) :on-settle) 'failed :error "boom")
        (should (eq (plist-get (e-work-status handle) :state) 'failed))))))

(ert-deftest e-subagent-runner-test-waitable-resolver-returns-handle ()
  "The registered `subagent' scheme resolves an id to its work handle."
  (e-subagent-runner-test--with-instances
    (let* ((e-waitable--resolvers (make-hash-table :test 'equal))
           (registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-subagents-register-waitable-resolver registry)
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (subagent-id (plist-get record :subagent-id))
             (reference (concat "subagent:" subagent-id)))
        (should (e-work-handle-p
                 (plist-get (e-waitable-resolve reference) :handle)))
        ;; An unknown id is a per-reference error, not a signal.
        (should (plist-get (e-waitable-resolve "subagent:sub_999999") :error))))))

(ert-deftest e-subagent-runner-test-awaitable-handle-is-enrolled-on-parent-board ()
  "The mirror handle is board-visible before a parent await subscribes to it."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on)
                                (list :cancel #'ignore))))
             (handle (e-subagent-registry-work-handle
                      registry (plist-get record :subagent-id)))
             (binding (e-chat-service-binding parent "parent-1"))
             (board (e-board-registry-board-source-board
                     (e-chat-service-binding-board binding))))
        (should (eq (e-board-work-handle
                     (e-board-observed-work board (e-work-handle-id handle)))
                    handle))
        (let ((aggregation
               (e-board-subscribe-aggregation
                board (list (e-work-handle-id handle)) 'all "target")))
          (should aggregation))))))

(provide 'e-subagent-runner-test)

;;; e-subagent-runner-test.el ends here

(ert-deftest e-subagent-runner-test-progress-snapshots-are-monotonic-and-bounded ()
  "Child progress retains only the latest bounded snapshot on its work handle."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (work-handle (e-subagent-registry-work-handle registry subagent-id))
             (first (plist-get record :progress)))
        (should (= (plist-get first :sequence) 1))
        (e-subagent--record-progress registry subagent-id work-handle 'tool-finished)
        (let* ((updated (e-subagent-registry-get registry subagent-id))
               (progress (plist-get updated :progress)))
          (should (= (plist-get updated :progress-sequence) 2))
          (should (eq (plist-get progress :event) 'tool-finished))
          (should (equal (plist-get progress :summary) "Finished tool"))
          (should (numberp (plist-get updated :started-at)))
          (should (numberp (plist-get updated :last-activity-at))))))))

(ert-deftest e-subagent-runner-test-direct-runner-ignores-reasoning-deltas ()
  "The direct runner maps meaningful lifecycle events but not reasoning deltas."
  (let (subscriber progress-events)
    (cl-letf (((symbol-function 'e-subagent--seed-child) #'ignore)
              ((symbol-function 'e-chat-service-subscribe)
               (lambda (_harness _session callback)
                 (setq subscriber callback)
                 'subscription))
              ((symbol-function 'e-chat-service-unsubscribe) #'ignore)
              ((symbol-function 'e-chat-service-submit-session)
               (lambda (_harness _session _prompt)
                 (funcall subscriber '(:type reasoning-delta :payload (:content "hidden")))
                 (funcall subscriber '(:type tool-started :payload (:result "hidden")))
                 (funcall subscriber '(:type tool-finished :payload (:result "hidden")))
                 (funcall subscriber '(:type turn-finished :payload nil))))
              ((symbol-function 'e-chat-service-abort-session) #'ignore))
      (e-subagent-direct-runner
       nil "child" "go" nil (lambda (&rest _) nil)
       (lambda (event) (push event progress-events)))
      (should (equal (nreverse progress-events)
                     '(tool-started tool-finished turn-finished))))))

(ert-deftest e-subagent-runner-test-interventions-publish-provenance-and-stay-explicit ()
  "Steer, interrupt, and shutdown retain bounded audit facts without auto-cancel."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1" :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (reason (make-string 300 ?r)))
        (cl-letf (((symbol-function 'e-chat-service-steer-session)
                   (lambda (_harness _session prompt &rest _)
                     (should (equal prompt "Run one focused test.")))))
          (e-subagent-steer registry subagent-id "Run one focused test." reason))
        (let ((intervention (plist-get (e-subagent-registry-get registry subagent-id)
                                       :last-intervention)))
          (should (eq (plist-get intervention :action) 'steer))
          (should (<= (string-width (plist-get intervention :reason)) 240)))
        (e-board-runtime-drain-producers)
        (let* ((binding (e-chat-service-binding parent "parent-1"))
               (messages (e-board-messages
                          (e-board-registry-board-source-board
                           (e-chat-service-binding-board binding))))
               (fact (car (last (cl-remove-if-not
                                 (lambda (message)
                                   (member 'intervention (e-board-message-tags message)))
                                 messages)))))
          (should (equal (plist-get (e-board-message-attributes fact) :subagent-id)
                         subagent-id))
          (should (equal (plist-get (e-board-message-attributes fact)
                                    :parent-session-id)
                         "parent-1"))
          (should (equal (plist-get (e-board-message-attributes fact) :action)
                         'steer)))
        ;; The child remains running until an explicit intervention changes it.
        (should (eq (plist-get (e-subagent-registry-get registry subagent-id) :status)
                    'running))
        (e-subagent-interrupt registry subagent-id "No progress after steer.")
        (should (eq (plist-get (e-subagent-registry-get registry subagent-id) :status)
                    'cancelled))))))

(ert-deftest e-subagent-runner-test-durable-report-precedes-local-settlement ()
  "A structured durable report is published once before the registry settles."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1" :type :reviewer :prompt "go"
                      :run-id "run-1" :task-key "review" :attempt 0
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (child-session-id (plist-get record :session-id))
             (settle (plist-get (car captured) :on-settle))
             (board (e-board-registry-board-source-board
                     (e-chat-service-binding-board
                      (e-chat-service-ensure-binding parent "parent-1")))))
        (e-subagent-report registry child-session-id [] "reported")
        (should (eq (plist-get (e-subagent-registry-get registry
                                                        (plist-get record :subagent-id))
                               :status)
                    'running))
        (funcall settle 'done :summary "later final")
        (let ((reports (delq nil (mapcar #'e-board-orchestration-fact-from-message
                                         (e-board-messages board)))))
          (should (= (length reports) 1))
          (should (equal (plist-get (plist-get (car reports) :payload) :summary)
                         "reported")))))))

(ert-deftest e-subagent-runner-test-durable-assignment-survives-registry-loss ()
  "A child context resolves its report assignment from session metadata alone."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-board-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1" :type :reviewer :prompt "go"
                      :run-id "run-1" :task-key "review" :attempt 0
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (child (e-subagent-registry-child-harness registry
                                                        (plist-get record :subagent-id)))
             (session-id (plist-get record :session-id))
             (board (e-board-registry-board-source-board
                     (e-chat-service-binding-board
                      (e-chat-service-ensure-binding child session-id)))))
        (should (e-board-orchestration-actions-report-from-context
                 (list :harness child :session-id session-id)
                 :summary "durable" :outputs []))
        (let ((report (e-board-orchestration-fact-from-message
                       (cl-find-if (lambda (message)
                                     (e-board-orchestration-fact-from-message message))
                                   (e-board-messages board)))))
          ;; The fact is directly readable without the process-local registry.
          (should (equal (plist-get (plist-get report :payload) :task-key)
                         "review")))))))

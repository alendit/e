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
(require 'e-harness-instances)
(require 'e-session)
(require 'e-store)
(require 'e-subagent-registry)
(require 'e-subagent-runner)
(require 'e-subagent-actions)
(require 'e-subagents)
(require 'e-work)

(defmacro e-subagent-runner-test--with-instances (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal))
         (e-subagent--configured-harnesses
          (make-hash-table :test 'eq :weakness 'key)))
     (e-harness-instance-register
      :id :reviewer
      :name "Reviewer"
      :kind 'reviewer
      :subagent t
      :description "Use for review."
      :factory (lambda () (e-harness-create
                           :backend (e-backend-fake-create :items nil))))
     ,@body))

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

(ert-deftest e-subagent-runner-test-spawn-records-lineage-and-seeds ()
  "Spawn creates a child under the parent lineage and seeds explicit context."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-spawn
               registry parent "parent-1"
               :type :reviewer
               :prompt "Review tmp://plan.org"
               :seed-messages (list '(:role user :content "context note"))
               :label "review plan.org"
               :runner (e-subagent-runner-test--capturing-runner captured)))
             (child-session-id (plist-get record :session-id))
             (call (car captured))
             (child-harness (plist-get call :child-harness)))
        (should (eq (plist-get record :type) :reviewer))
        (should (eq (plist-get record :role) 'reviewer))
        (should (eq (plist-get record :status) 'running))
        (should (equal (plist-get record :parent-session-id) "parent-1"))
        (should (equal (plist-get call :prompt) "Review tmp://plan.org"))
        ;; Child session carries durable lineage metadata sharing the parent id.
        (let ((metadata (plist-get (e-session-get
                                    (e-harness-sessions child-harness)
                                    child-session-id)
                                   :metadata)))
          (should (equal (plist-get metadata :parent-session-id) "parent-1"))
          (should (equal (plist-get metadata :tmp-lineage-id) "parent-1"))
          (should (equal (plist-get metadata :subagent-label) "review plan.org")))
        ;; The explicit seed landed in the child's own store before the task.
        (should (equal (mapcar (lambda (m) (plist-get m :content))
                               (e-harness-messages child-harness
                                                   child-session-id))
                       '("context note")))))))

(ert-deftest e-subagent-runner-test-final-message-is-default-result ()
  "A settle with a summary records it as the compact result."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-create-session parent :id "parent-1")
      (let* ((record (e-subagent-spawn
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
      (e-harness-create-session parent :id "parent-1")
      (let* ((record (e-subagent-spawn
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
      (e-harness-create-session parent :id "parent-1")
      (let* ((record (e-subagent-spawn
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

(ert-deftest e-subagent-runner-test-list-scopes-to-parent ()
  "List returns only the calling parent's direct children."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-create-session parent :id "parent-1")
      (e-harness-create-session parent :id "parent-2")
      (e-subagent-spawn registry parent "parent-1"
                        :type :reviewer :prompt "a" :runner noop)
      (e-subagent-spawn registry parent "parent-2"
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
      (e-harness-create-session parent :id "parent-1")
      (should-error
       (e-subagent-spawn registry parent "parent-1"
                         :type :chat-plain :prompt "go"
                         :runner (lambda (_h _s _p _seed _on)
                                   (list :cancel #'ignore)))
       :type 'e-subagent-unknown-type))))

(ert-deftest e-subagent-runner-test-steer-and-send-dispatch ()
  "Steer and send route to the child harness turn control."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (steered nil)
           (queued nil))
      (e-harness-create-session parent :id "parent-1")
      (let* ((record (e-subagent-spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (subagent-id (plist-get record :subagent-id)))
        (cl-letf (((symbol-function 'e-harness-steer-active-turn)
                   (lambda (_h _s prompt &rest _) (setq steered prompt) "turn-1"))
                  ((symbol-function 'e-harness-queue-prompt)
                   (lambda (_h _s prompt &rest _) (setq queued prompt) nil)))
          (e-subagent-steer registry subagent-id "steer this")
          (e-subagent-send registry subagent-id "follow up")
          (should (equal steered "steer this"))
          (should (equal queued "follow up")))))))

(ert-deftest e-subagent-runner-test-raw-read-returns-excerpt-and-uri ()
  "Raw read returns a bounded excerpt and the child session:// URI."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-create-session parent :id "parent-1")
      (let* ((record (e-subagent-spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :seed-messages (list '(:role user :content "one")
                                           '(:role assistant :content "two"))
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (child-session-id (plist-get record :session-id))
             (raw (e-subagent-raw-read registry subagent-id 1)))
        (should (equal (plist-get raw :session-uri)
                       (format "session://e/sessions/%s/messages" child-session-id)))
        ;; Bounded to the last message only.
        (should (equal (mapcar (lambda (m) (plist-get m :content))
                               (plist-get raw :messages))
                       '("two")))))))

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
      (e-harness-create-session parent :id "parent-1")
      (let* ((record (e-subagent-spawn registry parent "parent-1"
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
        (e-subagent-spawn registry parent "parent-1"
                          :type :lean :prompt "again" :runner noop)
        (should (memq 'web (e-harness-enabled-layer-ids harness)))))))

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
      (dolist (action '(:spawn :list :status :read :steer :send
                        :interrupt :shutdown :configure-type))
        (should (e-capabilities-action-spec capability action)))
      (should-not (e-capabilities-action-spec capability :report))
      ;; Actions only: no model-facing tool definitions, like elisp-job.
      (should-not (e-capability-tools capability))
      (e-capabilities-register-resources capability store)
      (let ((uris (mapcar #'e-store-entry-uri (e-store-list store))))
        (should (member "e://subagents/skills/subagents" uris))
        (should (member "e://subagents/refs/types.md" uris)))
      (should (string-match-p
               "spawn"
               (e-store-read store "e://subagents/skills/subagents" nil))))))

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
      (e-harness-create-session parent :id "parent-1")
      (let* ((record (e-subagent-spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (child (e-subagent-registry-child-harness
                     registry (plist-get record :subagent-id)))
             (caps (mapcar #'e-capability-id
                           (e-harness-effective-capabilities child))))
        (should (memq 'subagents-child (e-harness-enabled-layer-ids child)))
        (should (memq 'subagents caps))))))

(provide 'e-subagent-runner-test)

;;; e-subagent-runner-test.el ends here

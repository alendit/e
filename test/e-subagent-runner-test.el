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
(load (expand-file-name "e-chat-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(load (expand-file-name "e-board-producer-test-support.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)
(require 'e-harness-instances)
(require 'e-session)
(require 'e-session-async)
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
         (e-subagent--configured-harnesses
          (make-hash-table :test 'eq :weakness 'key))
         (e-chat-test-support-share-sqlite-store t)
         (e-chat-test-support--shared-sqlite-fixture nil)
         (e-work--unsettled-count 0)
         (e-work--unsettled-generation 0)
         (e-work--unsettled-change-functions nil))
     (e-harness-instance-register
      :id :reviewer
      :name "Reviewer"
      :kind 'reviewer
     :subagent t
     :description "Use for review."
     :factory (lambda () (e-harness-create
                           :backend (e-backend-fake-create :items nil))))
     (unwind-protect
         (cl-letf (((symbol-function 'e-harness-test-create-session)
                    #'e-subagent-runner-test--create-board-session))
           ,@body)
       ;; Retire SQL callbacks while the per-test e-work accounting remains
       ;; dynamically bound; the outer shared ERT teardown then has no work.
       (e-chat-test-support--close-sqlite-fixtures))))

(cl-defun e-subagent-runner-test--create-board-session
    (harness &key id metadata)
  "Create and bind one disposable SQL chat session for HARNESS."
  (let* ((creation
          (e-chat-service-create-session-start
           :harness harness :id id :metadata metadata))
         (binding (e-chat-service-binding-start harness id nil t)))
    (e-work-with-batch-await
      (e-work-await-batch creation :timeout 5.0))
    (e-work-with-batch-await
      (e-work-await-batch binding :timeout 5.0))))

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

(defun e-subagent-runner-test--deferred-work (id)
  "Return a started cooperative work handle named ID that tests settle later."
  (e-work-start
   (e-work-spec-create
    :id id :execution 'cooperative :interactive-policy 'async
    :owner 'e-subagent-runner-test
    :runner (lambda (_handle _arguments _context) :deferred))
   nil))

(defun e-subagent-runner-test--spawn
    (registry parent-harness parent-session-id &rest arguments)
  "Spawn test work and await its real disposable-SQL admission."
  (let* ((result
          (apply #'e-subagent-spawn registry parent-harness parent-session-id
                 :source-turn-id "parent-turn" arguments))
         (subagent-id (plist-get result :subagent-id)))
    (should
     (e-chat-test--wait-until
      (lambda ()
        (gethash subagent-id (e-subagent-registry-records registry)))
      5.0))
    (e-subagent-registry-get registry subagent-id)))

(defun e-subagent-runner-test--spawn-pending
    (registry parent-harness parent-session-id &rest arguments)
  "Spawn test work without awaiting deliberately controlled admission."
  (apply #'e-subagent-spawn registry parent-harness parent-session-id
         :source-turn-id "parent-turn" arguments))

(defun e-subagent-runner-test--publication-target (harness session-id)
  "Return SESSION-ID's detached SQL publication target in HARNESS."
  (e-chat-service-publication-target
   (e-chat-service-binding harness session-id)))

(defun e-subagent-runner-test--records (harness session-id)
  "Read SESSION-ID's bounded canonical Board records from disposable SQLite."
  (e-board-producer-test-records
   (e-subagent-runner-test--publication-target harness session-id)))

(ert-deftest e-subagent-runner-test-lifecycle-key-uses-durable-session-id ()
  "Restarted process-local child ids cannot collide in durable Board facts."
  (let (keys)
    (cl-letf (((symbol-function 'e-subagent--publish-board-fact)
               (lambda (_target &rest arguments)
                 (push (plist-get arguments :source-fact-key) keys))))
      (e-subagent--publish-lifecycle
       'target '(:subagent-id "sub_000001"
                 :session-id "child-before-restart"
                 :status queued))
      (e-subagent--publish-lifecycle
       'target '(:subagent-id "sub_000001"
                 :session-id "child-after-restart"
                 :status queued)))
    (should
     (equal (nreverse keys)
            '((subagent-lifecycle "child-before-restart" queued)
              (subagent-lifecycle "child-after-restart" queued))))))

(ert-deftest e-subagent-runner-test-register-rejects-unreserved-explicit-id ()
  "An explicit child id cannot bypass admission with a nil work handle."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-session parent :id "parent-1")
      (should-error
       (e-subagent-registry-register
        registry :subagent-id "sub_fake" :work-handle nil
        :type :reviewer :role 'reviewer :session-id "child-1"
        :parent-session-id "parent-1" :schedule 'direct)
       :type 'e-subagent-registry-error)
      (should-not (gethash "sub_fake"
                           (e-subagent-registry-records registry)))
      (should-not (e-subagent-registry-list registry "parent-1")))))

(ert-deftest e-subagent-runner-test-delayed-admission-stays-pending-and-unpublished ()
  "A child is neither registered nor started before durable admission settles."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (admission (e-subagent-runner-test--deferred-work "held-admission"))
           (runner-calls 0)
           (running-calls 0))
      (e-harness-test-create-session parent :id "parent-1")
      (cl-letf (((symbol-function 'e-chat-service-create-participant-start)
                 (lambda (&rest _arguments) admission))
                ((symbol-function 'e-subagent--inherit-prompt-cache-policy)
                 #'ignore))
        (let* ((pending
                (e-subagent-runner-test--spawn-pending
                 registry parent "parent-1" :type :reviewer :prompt "go"
                 :run-id "run-1" :task-key "review" :attempt 0
                 :on-running (lambda (_record) (cl-incf running-calls))
                 :runner (lambda (&rest _arguments)
                           (cl-incf runner-calls)
                           (list :cancel #'ignore))))
               (subagent-id (plist-get pending :subagent-id))
               (await-ref (plist-get pending :await-ref))
               (pending-record
                (e-subagent-registry-pending-admission
                 registry subagent-id)))
          (should (eq (plist-get pending :status) 'pending))
          (should (eq (plist-get pending-record :status) 'pending))
          (should (eq (plist-get pending-record :type) :reviewer))
          (should (equal (plist-get pending-record :session-id)
                         (plist-get pending :session-id)))
          (should (equal (plist-get pending-record :parent-session-id)
                         "parent-1"))
          (should (equal (plist-get pending-record :run-id) "run-1"))
          (should (equal (plist-get pending-record :task-key) "review"))
          (should (= (plist-get pending-record :attempt) 0))
          (should
           (equal
            (plist-get
             (e-subagent-registry-find-pending-assignment
              registry "run-1" "review" 0)
             :subagent-id)
            subagent-id))
          (should-not (e-subagent-registry-list registry "parent-1"))
          (should (equal await-ref (format "subagent:%s" subagent-id)))
          (should (e-work-handle-p
                   (e-subagent-registry-work-handle registry subagent-id)))
          (should (= runner-calls 0))
          (should (= running-calls 0))
          (should-not
           (cl-find-if
            (lambda (record)
              (member (plist-get record :tags)
                      '((subagent change queued) (subagent change running))))
            (e-subagent-runner-test--records parent "parent-1")))
          (e-work-finish admission '(:id "child"))
          (should-not
           (e-subagent-registry-pending-admission registry subagent-id))
          (should (= runner-calls 1))
          (should (= running-calls 1))
          (should (eq (e-subagent-registry-status registry subagent-id)
                      'running)))))))

(ert-deftest e-subagent-runner-test-runner-start-failure-settles-once ()
  "A post-commit runner-start error publishes one running then one failure."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (admission
            (e-subagent-runner-test--deferred-work
             "held-post-commit-start"))
           (original-create
            (symbol-function 'e-chat-service-create-participant-start))
           (running-calls 0)
           (early-failure-calls 0))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((pending
              (cl-letf
                  (((symbol-function 'e-chat-service-create-participant-start)
                    (lambda (&rest arguments)
                      ;; Execute the real admission through its commit before
                      ;; holding delivery of the successful acknowledgement.
                      (let ((committed (apply original-create arguments)))
                        (e-board-producer-test-await committed)
                        admission))))
                (e-subagent-runner-test--spawn-pending
                 registry parent "parent-1" :type :reviewer :prompt "go"
                 :run-id "run-1" :task-key "review" :attempt 0
                 :on-running (lambda (_record) (cl-incf running-calls))
                 :on-failure
                 (lambda (_error _pending) (cl-incf early-failure-calls))
                 :runner (lambda (&rest _arguments)
                           (error "runner start exploded")))))
             (subagent-id (plist-get pending :subagent-id))
             (session-id (plist-get pending :session-id))
             (handle (e-subagent-registry-work-handle registry subagent-id)))
        (should (eq (plist-get pending :status) 'pending))
        (should (= running-calls 0))
        (e-work-finish admission '(:id "child"))
        (should (= running-calls 1))
        (should (= early-failure-calls 0))
        (should (eq (plist-get (e-work-status handle) :state) 'failed))
        (should-not (gethash subagent-id
                             (e-subagent-registry-records registry)))
        ;; Admission and all terminal history are durable even though live
        ;; execution coordination is gone.
        (let* ((records (e-subagent-runner-test--records parent "parent-1"))
                 (running
                  (cl-count-if
                   (lambda (record)
                     (equal (plist-get record :tags)
                            '(subagent change running)))
                   records))
                 (terminal
                  (cl-remove-if-not
                   (lambda (record)
                     (let ((fact
                            (e-board-orchestration-fact-from-record record)))
                       (eq (plist-get fact :type) 'terminal-report)))
                   records)))
            (should (= running 1))
            (should (= (length terminal) 1))
            (let ((payload
                   (plist-get
                    (e-board-orchestration-fact-from-record (car terminal))
                    :payload)))
              (should (equal (plist-get payload :run-id) "run-1"))
              (should (equal (plist-get payload :task-key) "review"))
              (should (= (plist-get payload :attempt) 0))
              (should (eq (plist-get payload :status) 'failed))
              (should
               (string-match-p "runner start exploded"
                               (plist-get payload :error))))
            (e-subagent--settle
             registry (e-subagent-runner-test--publication-target
                       parent "parent-1")
             subagent-id 'failed :error "late")
            (should
             (= (cl-count-if
                 (lambda (record)
                   (let ((fact
                          (e-board-orchestration-fact-from-record record)))
                     (eq (plist-get fact :type) 'terminal-report)))
                 (e-subagent-runner-test--records parent "parent-1"))
                1)))))))

(ert-deftest e-subagent-runner-test-admission-failure-never-registers-or-starts ()
  "A rejected durable admission fails once without a child registry ghost."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (admission (e-subagent-runner-test--deferred-work "failed-admission"))
           (runner-calls 0)
           (failure-calls 0))
      (e-harness-test-create-session parent :id "parent-1")
      (cl-letf (((symbol-function 'e-chat-service-create-participant-start)
                 (lambda (&rest _arguments) admission)))
        (let* ((pending
                (e-subagent-runner-test--spawn-pending
                 registry parent "parent-1" :type :reviewer :prompt "go"
                 :on-failure
                 (lambda (_error _pending) (cl-incf failure-calls))
                 :runner (lambda (&rest _arguments)
                           (cl-incf runner-calls)
                           (list :cancel #'ignore))))
               (await-ref (plist-get pending :await-ref))
               (handle
                (e-subagent-registry-work-handle
                 registry (substring await-ref (length "subagent:")))))
          (e-work-fail admission '(e-session-storage-error "denied"))
          (should (= runner-calls 0))
          (should (= failure-calls 1))
          (should-not (e-subagent-registry-list registry "parent-1"))
          (should (eq (plist-get (e-work-status handle) :state) 'failed))
          (should (equal (plist-get (e-work-status handle) :error)
                         '(e-session-storage-error "denied")))
          ;; A stale late callback cannot create a child or settle twice.
          (e-work-finish admission '(:id "late-child"))
          (should (= runner-calls 0))
          (should (= failure-calls 1))
          (should-not (e-subagent-registry-list registry "parent-1")))))))

(ert-deftest e-subagent-runner-test-spawn-records-lineage-and-seeds ()
  "Spawn creates a child under the parent lineage and seeds explicit context."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :project-root "/tmp/example-project/"))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
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
        (let ((metadata
               (plist-get
                (e-board-producer-test-await
                 (e-session-async-session-metadata
                  (e-harness-sessions child-harness) child-session-id))
                :metadata)))
          (should (equal (plist-get metadata :parent-session-id) "parent-1"))
          (should (equal (plist-get metadata :tmp-lineage-id) "parent-1"))
          (should (equal (plist-get metadata :project-root)
                         "/tmp/example-project/"))
          (should (equal (plist-get metadata :subagent-label) "review plan.org")))
        ;; The explicit seed landed in the child's own store before the task.
        (should (equal (mapcar
                        (lambda (message) (plist-get message :content))
                        (plist-get
                         (e-board-producer-test-await
                          (e-session-async-visible-message-page
                           (e-harness-sessions child-harness)
                           child-session-id 8))
                         :messages))
                       '("context note")))
        (let* ((binding (e-chat-service-binding parent "parent-1"))
               (child-binding
                (e-chat-service-binding child-harness child-session-id))
               (records (e-subagent-runner-test--records parent "parent-1"))
               (facts (cl-remove-if-not
                       (lambda (record)
                         (eq (plist-get record :kind) 'fact))
                       records)))
          (should (equal (e-chat-service-binding-board-id child-binding)
                         (e-chat-service-binding-board-id binding)))
          (should (equal
                   (e-chat-service-binding-default-to child-binding)
                   (e-chat-service-binding-participant-id child-binding)))
          (should (equal (e-chat-service-binding-default-tags child-binding)
                         '(subagent)))
          (should (>= (length facts) 2))
          (should (cl-every
                   (lambda (record)
                     (string-match-p
                      "\\`producer:subagent:brd_[[:alnum:]]+:parent-1\\'"
                      (plist-get record :author)))
                   facts))
          (let ((tags (mapcar (lambda (record) (plist-get record :tags)) facts)))
            (should (member '(subagent change queued) tags))
            (should (member '(subagent change running) tags))))
        ;; Live progress remains bounded process-local execution state.
        (let ((snapshot
               (e-subagent--record-progress
                registry (plist-get record :subagent-id)
                work-handle 'tool-finished)))
          (should (eq (plist-get snapshot :event) 'tool-finished))
          (should
           (equal snapshot
                   (plist-get
                    (e-subagent-registry-get
                     registry (plist-get record :subagent-id))
                   :progress))))))))

(ert-deftest e-subagent-runner-test-explicit-project-root-wins-without-parent-state ()
  "An application-owned project root reaches the child admission directly."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :project-root "/tmp/ambient/"))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               registry parent "parent-1"
               :type :reviewer :prompt "work"
               :project-root "/tmp/grimoire/"
               :runner (e-subagent-runner-test--capturing-runner captured)))
             (child-harness (plist-get (car captured) :child-harness))
             (metadata
              (plist-get
               (e-board-producer-test-await
                (e-session-async-session-metadata
                 (e-harness-sessions child-harness)
                 (plist-get record :session-id)))
               :metadata)))
        (should (equal (plist-get metadata :project-root) "/tmp/grimoire/"))))))

(ert-deftest e-subagent-runner-test-final-message-is-default-result ()
  "A settle with a summary records it as the compact result."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (settle (plist-get (car captured) :on-settle)))
        (let ((final (funcall settle 'done :summary "3 issues found")))
          (should (eq (plist-get final :status) 'done))
          (should (equal (plist-get final :result-summary) "3 issues found"))
          (should-not (gethash subagent-id
                               (e-subagent-registry-records registry))))))))

(ert-deftest e-subagent-runner-test-report-overrides-final-message ()
  "A child-reported result is authoritative over a later final message."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (child-session-id (plist-get record :session-id))
             (settle (plist-get (car captured) :on-settle)))
        (e-subagent-report
         registry child-session-id
         (list '(:kind org-link :uri "tmp://r.org" :label "review"))
         "reported summary" '(:source-status data :item-count 3))
        ;; A later final message must not overwrite the reported result.
        (let ((final (funcall settle 'done :summary "chatter final message")))
          (should (equal (plist-get final :result-summary) "reported summary"))
          (should (equal (plist-get final :result)
                         '(:source-status data :item-count 3)))
          (should (equal (plist-get final :outputs)
                         (list '(:kind org-link :uri "tmp://r.org" :label "review"))))
          (should (eq (plist-get final :status) 'done)))))))

(ert-deftest e-subagent-runner-test-report-admission-rejects-then-accepts-exact-report ()
  "Report admission rejects visibly without consuming the child's retry."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (e-subagent-actions-default-registry registry)
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil))
           (calls nil)
           (accepted
            '(:summary "accepted summary"
              :outputs [(:kind "artifact" :uri "tmp://accepted.org")]))
           (admission
            (lambda (assignment report)
              (push (list assignment report) calls)
              (unless (equal report accepted)
                (user-error "report artifact is missing"))
              report)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               registry parent "parent-1"
               :type :reviewer :prompt "go"
               :run-id "run-1" :task-key "review" :attempt 2
               :report-admission admission
               :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (child-session-id (plist-get record :session-id))
             (child-harness
              (e-subagent-registry-child-harness registry subagent-id))
             (settle (plist-get (car captured) :on-settle)))
        (should-error
         (e-actions-call
          'subagents :report
          '(:summary "bad" :outputs [])
          (list :harness child-harness :session-id child-session-id
                :turn-id "child-turn"))
         :type 'user-error)
        (should-not (e-subagent-registry-reported-p registry subagent-id))
        (should (eq (e-subagent-registry-status registry subagent-id) 'running))
        (should-not (plist-member (e-subagent-registry-get registry subagent-id)
                                  :report-admission))
        (should
         (equal
          (e-actions-call
           'subagents :report accepted
           (list :harness child-harness :session-id child-session-id
                 :turn-id "child-turn"))
          (e-subagent-registry-get registry subagent-id)))
        (e-subagent-report registry child-session-id [] "late replacement")
        (let* ((call (car calls))
               (assignment (car call)))
          (should (= (length calls) 2))
          (should (equal assignment
                         (list :run-id "run-1" :task-key "review" :attempt 2
                               :subagent-id subagent-id
                               :session-id child-session-id
                               :parent-session-id "parent-1")))
          (should-not (plist-member assignment :report-admission))
          (should (equal (cadr call) accepted)))
        (let ((final (funcall settle 'done :summary "ignored prose")))
          (should (eq (plist-get final :status) 'done))
          (should (equal (plist-get final :result-summary)
                         (plist-get accepted :summary)))
          (should (equal (plist-get final :outputs)
                         (plist-get accepted :outputs))))))))

(ert-deftest e-subagent-runner-test-report-admission-is-private-while-pending-and-live ()
  "The callback stays only in pending/live internal coordination records."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (held (e-subagent-runner-test--deferred-work "held-admission"))
           (admission (lambda (_assignment report) report)))
      (e-harness-test-create-session parent :id "parent-1")
      (cl-letf (((symbol-function 'e-chat-service-create-participant-start)
                 (lambda (&rest _arguments) held))
                ((symbol-function 'e-subagent--inherit-prompt-cache-policy)
                 #'ignore))
        (let* ((pending
                (e-subagent-runner-test--spawn-pending
                 registry parent "parent-1" :type :reviewer :prompt "go"
                 :report-admission admission
                 :runner (lambda (&rest _arguments) (list :cancel #'ignore))))
               (subagent-id (plist-get pending :subagent-id))
               (internal
                (gethash subagent-id
                         (e-subagent-registry-pending-admissions registry))))
          (should (eq (plist-get internal :report-admission) admission))
          (should-not (plist-member pending :report-admission))
          (should-not
           (plist-member
            (e-subagent-registry-pending-admission registry subagent-id)
            :report-admission))
          (e-work-finish held '(:status admitted))
          (should
           (e-chat-test--wait-until
            (lambda ()
              (gethash subagent-id (e-subagent-registry-records registry)))
            5.0))
          (should
           (eq (plist-get
                (gethash subagent-id (e-subagent-registry-records registry))
                :report-admission)
               admission))
          (should-not
           (plist-member (e-subagent-registry-get registry subagent-id)
                         :report-admission)))))))

(ert-deftest e-subagent-runner-test-forgotten-admission-does-not-leak-callback ()
  "Retiring pending coordination returns no report-admission function."
  (let* ((registry (e-subagent-registry-create))
         (admission (lambda (_assignment report) report))
         (subagent-id
          (e-subagent-registry-reserve-admission
           registry :session-id "child" :parent-session-id "parent"
           :report-admission admission))
         (forgotten
          (e-subagent-registry-forget-admission registry subagent-id)))
    (should-not (plist-member forgotten :report-admission))
    (should-not
     (e-subagent-registry-pending-admission registry subagent-id))))

(ert-deftest e-subagent-runner-test-admission-required-child-cannot-finish-with-prose ()
  "A gated child that never reports settles as one explicit failed assignment."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil))
           terminal-calls)
      (e-harness-test-create-session parent :id "parent-1")
      (cl-letf (((symbol-function
                  'e-board-orchestration-actions-publish-terminal)
                 (lambda (_target assignment status &rest arguments)
                   (push (list assignment status arguments) terminal-calls))))
        (let* ((record
                (e-subagent-runner-test--spawn
                 registry parent "parent-1"
                 :type :reviewer :prompt "go"
                 :run-id "run-1" :task-key "review" :attempt 0
                 :report-admission (lambda (_assignment report) report)
                 :runner (e-subagent-runner-test--capturing-runner captured)))
               (subagent-id (plist-get record :subagent-id))
               (handle (e-subagent-registry-work-handle registry subagent-id))
               (settle (plist-get (car captured) :on-settle))
               (terminal (funcall settle 'done :summary "final prose only")))
          (should (eq (plist-get terminal :status) 'failed))
          (should (string-match-p "accepted report"
                                  (plist-get terminal :error)))
          (should (eq (plist-get (e-work-status handle) :state) 'failed))
          (should (= (length terminal-calls) 1))
          (should (eq (cadar terminal-calls) 'failed))
          (should-not (gethash subagent-id
                               (e-subagent-registry-records registry)))
          ;; A competing terminal callback cannot publish or settle twice.
          (should-not (funcall settle 'done :summary "late"))
          (should (= (length terminal-calls) 1)))))))

(ert-deftest e-subagent-runner-test-report-admission-failure-is-owner-local ()
  "A gated failure does not prevent an ungated sibling from succeeding."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (failed-captured (list nil))
           (sibling-captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((failed
              (e-subagent-runner-test--spawn
               registry parent "parent-1" :type :reviewer :prompt "fail"
               :report-admission (lambda (_assignment _report)
                                   (user-error "invalid artifact"))
               :runner
               (e-subagent-runner-test--capturing-runner failed-captured)))
             (sibling
              (e-subagent-runner-test--spawn
               registry parent "parent-1" :type :reviewer :prompt "succeed"
               :runner
               (e-subagent-runner-test--capturing-runner sibling-captured))))
        (should-error
         (e-subagent-report registry (plist-get failed :session-id) [] "bad")
         :type 'user-error)
        (should
         (eq (plist-get
              (funcall (plist-get (car failed-captured) :on-settle) 'done)
              :status)
             'failed))
        (should
         (eq (plist-get
              (funcall (plist-get (car sibling-captured) :on-settle)
                       'done :summary "ok")
              :status)
             'done))))))

(ert-deftest e-subagent-runner-test-report-admission-preserves-failure-and-cancellation ()
  "A gate changes only false success, not genuine failure or cancellation."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (failed-captured (list nil))
           (cancelled-captured (list nil))
           (admission (lambda (_assignment report) report)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((failed
              (e-subagent-runner-test--spawn
               registry parent "parent-1" :type :reviewer :prompt "fail"
               :report-admission admission
               :runner
               (e-subagent-runner-test--capturing-runner failed-captured)))
             (cancelled
              (e-subagent-runner-test--spawn
               registry parent "parent-1" :type :reviewer :prompt "cancel"
               :report-admission admission
               :runner
               (e-subagent-runner-test--capturing-runner cancelled-captured)))
             (failed-handle
              (e-subagent-registry-work-handle
               registry (plist-get failed :subagent-id)))
             (cancelled-handle
              (e-subagent-registry-work-handle
               registry (plist-get cancelled :subagent-id))))
        (let ((failure
               (funcall (plist-get (car failed-captured) :on-settle)
                        'failed :error "provider failed"))
              (cancellation
               (funcall (plist-get (car cancelled-captured) :on-settle)
                        'cancelled)))
          (should (eq (plist-get failure :status) 'failed))
          (should (equal (plist-get failure :error) "provider failed"))
          (should (eq (plist-get cancellation :status) 'cancelled))
          (should (eq (plist-get (e-work-status failed-handle) :state) 'failed))
          (should (eq (plist-get (e-work-status cancelled-handle) :state)
                      'cancelled))
          (should-not
           (gethash (plist-get failed :subagent-id)
                    (e-subagent-registry-records registry)))
          (should-not
           (gethash (plist-get cancelled :subagent-id)
                    (e-subagent-registry-records registry))))))))

(ert-deftest e-subagent-runner-test-interrupt-and-shutdown ()
  "Interrupt calls the cancel function and marks the record cancelled."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (cancelled nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on-settle)
                                (list :cancel (lambda () (setq cancelled t))))))
             (subagent-id (plist-get record :subagent-id)))
        (let ((terminal
               (e-subagent-interrupt
                registry
                (e-subagent-runner-test--publication-target parent "parent-1")
                subagent-id)))
          (should (eq (plist-get terminal :status) 'cancelled)))
        (should cancelled)
        (should-not (gethash subagent-id
                             (e-subagent-registry-records registry)))))))

(ert-deftest e-subagent-runner-test-interrupt-cleans-up-when-audit-target-fails ()
  "Explicit cancellation is not conditional on Board audit availability."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (cancelled nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               registry parent "parent-1"
               :type :reviewer :prompt "go"
               :runner
               (lambda (_h _s _p _seed _on-settle)
                 (list :cancel (lambda () (setq cancelled t))))))
             (subagent-id (plist-get record :subagent-id))
             (work (e-subagent-registry-work-handle registry subagent-id)))
        (should-error
         (e-subagent-interrupt registry nil subagent-id "audit unavailable")
         :type 'wrong-type-argument)
        (should cancelled)
        (should (eq (plist-get (e-work-status work) :state) 'cancelled))
        (should-not (gethash subagent-id
                             (e-subagent-registry-records registry)))
        (should-not (e-subagent-registry-list registry "parent-1"))))))

(ert-deftest e-subagent-runner-test-action-interrupt-cleans-up-without-binding ()
  "The public action still cancels when its parent SQL target is unavailable."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (cancelled nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               registry parent "parent-1"
               :type :reviewer :prompt "go"
               :runner
               (lambda (_h _s _p _seed _on-settle)
                 (list :cancel (lambda () (setq cancelled t))))))
             (subagent-id (plist-get record :subagent-id))
             (work (e-subagent-registry-work-handle registry subagent-id)))
        (e-chat-service-close-board
         (e-chat-service-binding parent "parent-1"))
        (should-error
         (e-subagent-actions--interrupt
          registry (list :harness parent :session-id "parent-1")
          (list :subagent-id subagent-id :reason "parent view closed"))
         :type 'e-subagent-error)
        (should cancelled)
        (should (eq (plist-get (e-work-status work) :state) 'cancelled))
        (should-not (gethash subagent-id
                             (e-subagent-registry-records registry)))))))

(ert-deftest e-subagent-runner-test-list-scopes-to-parent ()
  "List returns only the calling parent's direct children."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-test-create-session parent :id "parent-1")
      (e-harness-test-create-session parent :id "parent-2")
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
      (e-harness-test-create-session parent :id "parent-1")
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
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (subagent-id (plist-get record :subagent-id)))
        (cl-letf (((symbol-function 'e-chat-service-steer-session)
                   (lambda (_h _s prompt &rest _) (setq steered prompt) "turn-1"))
                  ((symbol-function 'e-chat-service-queue-session)
                   (lambda (_h _s prompt &rest _) (setq queued prompt) nil)))
          (e-subagent-steer
           registry
           (e-subagent-runner-test--publication-target parent "parent-1")
           subagent-id "steer this")
          (e-subagent-send registry subagent-id "follow up")
          (should (equal steered "steer this"))
          (should (equal queued "follow up")))))))

(ert-deftest e-subagent-runner-test-send-refuses-retired-child ()
  "Send rejects a terminal child after its live record has been retired."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil))
           (queued nil))
      (e-harness-test-create-session parent :id "parent-1")
      (cl-letf (((symbol-function 'e-harness-test-queue-prompt)
                 (lambda (_h _s prompt &rest _) (setq queued prompt) nil)))
        ;; A failed child is no longer process-local or sendable.
        (let* ((record (e-subagent-runner-test--spawn
                        registry parent "parent-1"
                        :type :reviewer :prompt "go"
                        :runner (e-subagent-runner-test--capturing-runner captured)))
               (subagent-id (plist-get record :subagent-id)))
          (funcall (plist-get (car captured) :on-settle)
                   'failed :error "boom")
          (should-not (gethash subagent-id
                               (e-subagent-registry-records registry)))
          (should-error (e-subagent-send registry subagent-id "follow up")
                        :type 'user-error)
          (should-not queued))
        ;; A done child is likewise refused.
        (let* ((record (e-subagent-runner-test--spawn
                        registry parent "parent-1"
                        :type :reviewer :prompt "go"
                        :runner (e-subagent-runner-test--capturing-runner captured)))
               (subagent-id (plist-get record :subagent-id)))
          (funcall (plist-get (car captured) :on-settle) 'done :summary "ok")
          (should-not (gethash subagent-id
                               (e-subagent-registry-records registry)))
          (should-error (e-subagent-send registry subagent-id "follow up")
                        :type 'user-error)
          (should-not queued))))))

(ert-deftest e-subagent-runner-test-raw-read-returns-excerpt-and-uri ()
  "Raw read returns a bounded live excerpt and the child session:// URI."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :seed-messages (list '(:role user :content "one")
                                           '(:role assistant :content "two"))
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (child-session-id (plist-get record :session-id)))
        (let ((raw
               (cl-letf
                   (((symbol-function 'e-harness-executing-session-state)
                     (lambda (harness session-id)
                       (when (and (eq harness
                                      (plist-get (car captured) :child-harness))
                                  (equal session-id child-session-id))
                         '(:messages ((:role user :content "one")
                                      (:role assistant :content "two")))))))
                 (e-subagent-raw-read registry subagent-id 1))))
          (should (equal (plist-get raw :session-uri)
                         (format "session://e/sessions/%s/messages"
                                 child-session-id)))
          ;; The request is bounded to the last live child message.
          (should (equal (mapcar (lambda (m) (plist-get m :content))
                                 (plist-get raw :messages))
                         '("two"))))))))

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
      (e-harness-test-create-session parent :id "parent-1")
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
      (e-harness-test-create-session parent :id "parent-1")
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
        (e-harness-test-create-session parent :id "parent-1")
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
                (plist-get
                 (e-board-producer-test-await
                  (e-session-async-query-state
                   (e-harness-sessions child) child-session-id))
                 :turn-options)))
          (should (eq (plist-get session-options :prompt-cache-default) t))
          (should (equal (plist-get session-options :prompt-cache-retention)
                         "24h")))))))

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
      (dolist (action '(:spawn :list :status :read :steer :send :resume
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
      (e-harness-test-create-session parent :id "parent-1")
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
      (e-harness-test-create-session parent :id "parent-1")
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
      (e-harness-test-create-session parent :id "parent-1")
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
      (e-harness-test-create-session parent :id "parent-1")
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

(ert-deftest e-subagent-runner-test-progress-snapshots-are-monotonic-and-bounded ()
  "Child progress retains only the latest bounded snapshot on its work handle."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (work-handle (e-subagent-registry-work-handle registry subagent-id))
             (first (plist-get record :progress))
             (first-turn-at (plist-get record :last-turn-at)))
        (should (= (plist-get first :sequence) 1))
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 200.0)))
          (e-subagent--record-progress
           registry subagent-id work-handle 'tool-finished))
        (let* ((updated (e-subagent-registry-get registry subagent-id))
               (progress (plist-get updated :progress)))
          (should (= (plist-get updated :progress-sequence) 2))
          (should (eq (plist-get progress :event) 'tool-finished))
          (should (equal (plist-get progress :summary) "Finished tool"))
          (should (numberp (plist-get updated :started-at)))
          (should (= (plist-get updated :last-activity-at) 200.0))
          (should (= (plist-get updated :last-turn-at) first-turn-at)))
        (let ((finished
               (cl-letf (((symbol-function 'float-time)
                          (lambda (&optional _) 300.0)))
                 (e-subagent--settle
                  registry
                  (e-subagent-runner-test--publication-target parent "parent-1")
                  subagent-id 'done :summary "done"))))
          (should (eq (plist-get finished :status) 'done))
          (should (= (plist-get finished :last-turn-at) 300.0))
          (should-not (gethash subagent-id
                               (e-subagent-registry-records registry))))))))

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
                 (funcall subscriber '(:type turn-finished :payload nil))
                 (let ((work
                        (e-subagent-runner-test--deferred-work
                         "direct-admission")))
                   (e-work-finish work '(:status posted))
                   work)))
              ((symbol-function 'e-chat-service-abort-session) #'ignore))
      (e-subagent-direct-runner
       nil "child" "go" nil (lambda (&rest _) nil)
       (lambda (event) (push event progress-events)))
      (should (equal (nreverse progress-events)
                     '(tool-started tool-finished turn-finished))))))

(ert-deftest e-subagent-runner-test-direct-runner-surfaces-admission-settlement ()
  "A rejected child input settles once even though no harness turn started."
  (let (admission settlements progress-events unsubscribed)
    (cl-letf (((symbol-function 'e-subagent--seed-child) #'ignore)
              ((symbol-function 'e-chat-service-subscribe)
               (lambda (_harness _session _callback) 'subscription))
              ((symbol-function 'e-chat-service-unsubscribe)
               (lambda (_subscription) (setq unsubscribed t)))
              ((symbol-function 'e-chat-service-submit-session)
               (lambda (&rest _arguments)
                 (setq admission
                       (e-subagent-runner-test--deferred-work
                        "held-direct-admission"))))
              ((symbol-function 'e-chat-service-abort-session) #'ignore))
      (e-subagent-direct-runner
       nil "child" "go" nil
       (lambda (status &rest arguments)
         (push (cons status arguments) settlements))
       (lambda (event) (push event progress-events)))
      (e-work-fail admission '(e-board-sqlite-error "admission rejected"))
      (e-work-cancel admission)
      (should (= (length settlements) 1))
      (should (eq (caar settlements) 'failed))
      (should (string-match-p
               "admission rejected"
               (plist-get (cdar settlements) :error)))
      (should (equal progress-events '(turn-failed)))
      (should unsubscribed))))

(ert-deftest e-subagent-runner-test-interventions-publish-provenance-and-stay-explicit ()
  "Steer, interrupt, and shutdown retain bounded audit facts without auto-cancel."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1" :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (subagent-id (plist-get record :subagent-id))
             (reason (make-string 300 ?r)))
        (cl-letf (((symbol-function 'e-chat-service-steer-session)
                   (lambda (_harness _session prompt &rest _)
                     (should (equal prompt "Run one focused test.")))))
          (e-subagent-steer
           registry
           (e-subagent-runner-test--publication-target parent "parent-1")
           subagent-id "Run one focused test." reason))
        (let ((intervention (plist-get (e-subagent-registry-get registry subagent-id)
                                       :last-intervention)))
          (should (eq (plist-get intervention :action) 'steer))
          (should (<= (string-width (plist-get intervention :reason)) 240)))
        (let* ((records (e-subagent-runner-test--records parent "parent-1"))
               (fact (car (last (cl-remove-if-not
                                 (lambda (record)
                                   (member 'intervention
                                           (plist-get record :tags)))
                                 records)))))
          (should (equal (plist-get (plist-get fact :attributes) :subagent-id)
                         subagent-id))
          (should (equal (plist-get (plist-get fact :attributes)
                                    :parent-session-id)
                         "parent-1"))
          (should (equal (plist-get (plist-get fact :attributes) :action)
                         'steer)))
        ;; The child remains running until an explicit intervention changes it.
        (should (eq (plist-get (e-subagent-registry-get registry subagent-id) :status)
                    'running))
        (let ((terminal
               (e-subagent-interrupt
                registry
                (e-subagent-runner-test--publication-target parent "parent-1")
                subagent-id "No progress after steer.")))
          (should (eq (plist-get terminal :status) 'cancelled)))
        (should-not (gethash subagent-id
                             (e-subagent-registry-records registry)))))))

(ert-deftest e-subagent-runner-test-durable-report-precedes-local-settlement ()
  "A structured durable report is published once before the registry settles."
  (e-subagent-runner-test--with-instances
    (let* ((registry (e-subagent-registry-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      registry parent "parent-1" :type :reviewer :prompt "go"
                      :run-id "run-1" :task-key "review" :attempt 0
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (child-session-id (plist-get record :session-id))
             (subagent-id (plist-get record :subagent-id))
             (settle (plist-get (car captured) :on-settle))
             (live (gethash subagent-id
                            (e-subagent-registry-records registry))))
        (should live)
        (should-not (plist-member live :publication-target))
        (should-not (plist-member live :publication-function))
        (should-not (plist-member live :parent-harness))
        (e-subagent-report registry child-session-id [] "reported")
        (should (eq (plist-get (e-subagent-registry-get registry
                                                        (plist-get record :subagent-id))
                               :status)
                    'running))
        (funcall settle 'done :summary "later final")
        (should-not (gethash (plist-get record :subagent-id)
                             (e-subagent-registry-records registry)))
        (should (zerop (hash-table-count
                        (e-subagent-registry-pending-admissions registry))))
        (should-not (e-subagent-registry-order registry))
        (should-not (e-subagent-registry-list registry))
        (should-not (e-subagent-registry-work-handle
                     registry (plist-get record :subagent-id)))
        (let ((reports
               (delq nil
                     (mapcar #'e-board-orchestration-fact-from-record
                             (e-subagent-runner-test--records
                              parent "parent-1")))))
          (should (= (length reports) 1))
          (should (equal (plist-get (plist-get (car reports) :payload) :summary)
                         "reported")))))))

(provide 'e-subagent-runner-test)

;;; e-subagent-runner-test.el ends here

;;; e-subagent-runner-test.el --- Tests for subagent spawn and runner -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the subagent live, spawn coordination, seeding, result
;; precedence, and lifecycle transitions using a fake runner, plus the
;; capability action round-trip and the assertion that subagents stay out of
;; model-facing tool definitions.

;;; Code:

(require 'ert)
(require 'seq)
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
(require 'e-subagent-live)
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
         (e-subagent-runner--live-owner (e-subagent-live-create))
         (e-subagent-runner--dispatch-claims (make-hash-table :test 'equal))
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

(defun e-subagent-runner-test--orchestration-facts (harness session-id)
  "Return detached orchestration facts for SESSION-ID."
  (delq nil
        (mapcar #'e-board-orchestration-fact-from-record
                (e-subagent-runner-test--records harness session-id))))

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
    (live parent-harness parent-session-id &rest arguments)
  "Spawn test work and await its real disposable-SQL admission."
  (let* ((result
          (apply #'e-subagent-spawn live parent-harness parent-session-id
                 :source-turn-id "parent-turn" arguments))
         (participant-id (plist-get result :participant-id)))
    (should
     (e-chat-test--wait-until
      (lambda ()
        (gethash participant-id (e-subagent-runner-test--live-entries live)))
      5.0))
    (e-subagent-runner-test--live-get live participant-id)))

(defun e-subagent-runner-test--spawn-pending
    (live parent-harness parent-session-id &rest arguments)
  "Spawn test work without awaiting deliberately controlled admission."
  (apply #'e-subagent-spawn live parent-harness parent-session-id
         :source-turn-id "parent-turn" arguments))

(defun e-subagent-runner-test--publication-target (harness session-id)
  "Return SESSION-ID's detached SQL publication target in HARNESS."
  (e-chat-service-publication-target
   (e-chat-service-binding harness session-id)))

(defun e-subagent-runner-test--records (harness session-id)
  "Read SESSION-ID's bounded canonical Board records from disposable SQLite."
  (e-board-producer-test-records
   (e-subagent-runner-test--publication-target harness session-id)))

(defun e-subagent-runner-test--entry (live participant-id &optional pending)
  "Return LIVE's raw entry for PARTICIPANT-ID, searching PENDING when set."
  (let ((table (if pending
                   (e-subagent-live-pending-admissions live)
                 (e-subagent-live-entries live)))
        found)
    (maphash (lambda (key value)
               (when (equal (cdr key) participant-id)
                 (setq found value)))
             table)
    found))

(defun e-subagent-runner-test--board-id (live participant-id)
  "Return the Board key for PARTICIPANT-ID in LIVE."
  (let (found)
    (dolist (table (list (e-subagent-live-entries live)
                         (e-subagent-live-pending-admissions live)))
      (maphash (lambda (key _value)
                 (when (equal (cdr key) participant-id)
                   (setq found (car key))))
               table))
    (or found (signal 'e-subagent-live-error
                      (list "participant is not live" participant-id)))))

(defun e-subagent-runner-test--record-from-entry (entry status)
  "Return a detached execution-context snapshot from raw ENTRY."
  (when-let* ((callbacks (plist-get entry :callbacks))
              (getter (plist-get callbacks :record))
              ((functionp getter)))
    (let ((record (copy-tree (funcall getter))))
      (setq record (plist-put record :status status))
      (setq record
            (plist-put record :await-ref
                       (e-subagent-live-reference
                        (plist-get entry :board-id)
                        (plist-get entry :participant-id))))
      (setq record (plist-put record :work-handle
                              (plist-get entry :work-handle)))
      (setq record (plist-put record :child-harness
                              (plist-get entry :harness)))
      (setq record (plist-put record :progress
                              (plist-get entry :progress)))
      ;; Report-admission is a runner callback, not part of a detached
      ;; observation-shaped snapshot.
      (setq record
            (cl-loop for (key value) on record by #'cddr
                     unless (eq key :report-admission)
                     append (list key value)))
      record)))

(defun e-subagent-runner-test--live-entries (live)
  "Return a participant-keyed detached view of LIVE entries for assertions."
  (let ((result (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (puthash (cdr key) value result))
             (e-subagent-live-entries live))
    result))

(defun e-subagent-runner-test--live-pending-entries (live)
  "Return a participant-keyed detached view of pending LIVE entries."
  (let ((result (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (puthash (cdr key) value result))
             (e-subagent-live-pending-admissions live))
    result))

(defun e-subagent-runner-test--live-get (live participant-id)
  "Return the current internal execution snapshot for PARTICIPANT-ID."
  (when-let ((entry (e-subagent-runner-test--entry live participant-id)))
    (e-subagent-runner-test--record-from-entry entry 'running)))

(defun e-subagent-runner-test--live-pending (live participant-id)
  "Return a bounded pending snapshot without private callbacks."
  (when-let ((entry (e-subagent-runner-test--entry live participant-id t)))
    (let ((record (e-subagent-runner-test--record-from-entry entry 'pending)))
      (setq record (plist-put record :work-handle
                              (plist-get entry :work-handle)))
      record)))

(defun e-subagent-runner-test--live-work-handle (live participant-id)
  "Return PARTICIPANT-ID's work handle, if pending or live."
  (when-let ((identity (e-subagent-live-find-identity live participant-id)))
    (e-subagent-live-work-handle live (car identity) participant-id)))

(defun e-subagent-runner-test--await-work-state (handle state)
  "Wait boundedly for HANDLE to reach STATE and return HANDLE."
  (should
   (e-chat-test--wait-until
    (lambda ()
      (eq (plist-get (e-work-status handle) :state) state))
    5.0))
  handle)

(defun e-subagent-runner-test--await-retired (live participant-id)
  "Wait boundedly until PARTICIPANT-ID has no private live entry."
  (should
   (e-chat-test--wait-until
    (lambda ()
      (null (gethash participant-id
                     (e-subagent-runner-test--live-entries live))))
    5.0)))

(defun e-subagent-runner-test--live-child-harness (live participant-id)
  "Return PARTICIPANT-ID's child harness."
  (let ((board-id (e-subagent-runner-test--board-id live participant-id)))
    (e-subagent-live-harness live board-id participant-id)))

(defun e-subagent-runner-test--live-status (live participant-id)
  "Return process-local pending/running state for PARTICIPANT-ID."
  (cond ((e-subagent-runner-test--entry live participant-id) 'running)
        ((e-subagent-runner-test--entry live participant-id t) 'pending)))

(defun e-subagent-runner-test--live-reported (live participant-id)
  "Return whether the runner accepted a report for PARTICIPANT-ID."
  (let* ((entry (e-subagent-runner-test--entry live participant-id))
         (callbacks (and entry (plist-get entry :callbacks)))
         (reported (and callbacks (plist-get callbacks :reported))))
    (and (functionp reported) (funcall reported))))

(defun e-subagent-runner-test--live-list (&rest _arguments)
  "The private owner has no public live inventory."
  nil)

(defun e-subagent-runner-test--live-order (&rest _arguments)
  "The private owner has no process-local ordering projection."
  nil)

(defun e-subagent-runner-test--live-find-pending
    (live run-id task-key attempt)
  "Find one pending assignment by its bounded coordinates."
  (let (participant)
    (maphash
     (lambda (key record)
       (when (and (equal (cdr key) (plist-get record :participant-id))
                  (equal (plist-get (plist-get record :assignment) :run-id)
                         run-id)
                  (equal (plist-get (plist-get record :assignment) :task-key)
                         task-key)
                  (equal (plist-get (plist-get record :assignment) :attempt)
                         attempt))
         (setq participant (cdr key))))
     (e-subagent-live-pending-admissions live))
    (and participant (e-subagent-runner-test--live-pending live participant))))

(defun e-subagent-runner-test--live-reserve (live &rest properties)
  "Reserve one synthetic test participant using LIVE's real owner."
  (let ((participant-id (or (plist-get properties :participant-id)
                            (plist-get properties :session-id)))
        (board-id "test-board"))
    (e-subagent-live-reserve-admission
     live board-id participant-id
     :session-id participant-id properties)
    participant-id))

(defun e-subagent-runner-test--live-forget (live participant-id)
  "Forget one synthetic pending participant."
  (let ((board-id (e-subagent-runner-test--board-id live participant-id)))
    (e-subagent-live-forget-admission live board-id participant-id)))

(defun e-subagent-runner-test--record-progress
    (live participant-id work-handle event)
  "Record bounded progress using the participant's Board key."
  (e-subagent--record-progress
   live (e-subagent-runner-test--board-id live participant-id)
   participant-id work-handle event))

(defun e-subagent-runner-test--report
    (live participant-id outputs summary &optional result)
  "Submit one report through the live owner using the durable Board key."
  (e-subagent-report
   live (e-subagent-runner-test--board-id live participant-id)
   participant-id outputs summary result))

(defun e-subagent-runner-test--interrupt
    (live publication-target participant-id &optional reason)
  "Interrupt PARTICIPANT-ID through its Board/participant key."
  (e-subagent-interrupt
   live (e-subagent-runner-test--board-id live participant-id)
   publication-target participant-id reason))

(defun e-subagent-runner-test--steer
    (live publication-target participant-id prompt &optional reason)
  "Steer PARTICIPANT-ID through its Board/participant key."
  (e-subagent-steer
   live (e-subagent-runner-test--board-id live participant-id)
   publication-target participant-id prompt reason))

(defun e-subagent-runner-test--send (live participant-id prompt)
  "Send PROMPT to PARTICIPANT-ID through its Board/participant key."
  (e-subagent-send
   live (e-subagent-runner-test--board-id live participant-id)
   participant-id prompt))

(ert-deftest e-subagent-runner-test-lifecycle-key-uses-durable-session-id ()
  "Restarted process-local child ids cannot collide in durable Board facts."
  (let (keys)
    (cl-letf (((symbol-function 'e-subagent--publish-board-fact)
               (lambda (_target &rest arguments)
                 (push (plist-get arguments :source-fact-key) keys))))
      (e-subagent--publish-lifecycle
       'target '(:participant-id "child-before-restart"
                 :session-id "child-before-restart"
                 :status queued))
      (e-subagent--publish-lifecycle
       'target '(:participant-id "child-after-restart"
                 :session-id "child-after-restart"
                 :status queued)))
    (should
     (equal (nreverse keys)
            '((subagent-lifecycle "child-before-restart" queued)
              (subagent-lifecycle "child-after-restart" queued))))))

(ert-deftest e-subagent-runner-test-register-rejects-unreserved-explicit-id ()
  "An explicit child id cannot bypass admission with a nil work handle."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-session parent :id "parent-1")
      (should-error
       (e-subagent-live-install
        live "test-board" "participant_fake" :work-handle nil)
       :type 'e-subagent-live-error)
      (should (= (hash-table-count (e-subagent-live-entries live)) 0))
      (should-not (e-subagent-runner-test--live-list live "parent-1")))))

(ert-deftest e-subagent-runner-test-delayed-admission-stays-pending-and-unpublished ()
  "A child is neither registered nor started before durable admission settles."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
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
                 live parent "parent-1" :type :reviewer :prompt "go"
                 :run-id "run-1" :task-key "review" :attempt 0
                 :on-running (lambda (_record) (cl-incf running-calls))
                 :runner (lambda (&rest _arguments)
                           (cl-incf runner-calls)
                           (list :cancel #'ignore))))
               (participant-id (plist-get pending :participant-id))
               (await-ref (plist-get pending :await-ref))
               (pending-record
                (e-subagent-runner-test--live-pending
                 live participant-id)))
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
             (e-subagent-runner-test--live-find-pending
              live "run-1" "review" 0)
             :participant-id)
            participant-id))
          (should-not (e-subagent-runner-test--live-list live "parent-1"))
          (should (equal (e-subagent-live-reference-identity await-ref)
                         (list (e-subagent-runner-test--board-id
                                live participant-id)
                               participant-id)))
          (should (e-work-handle-p
                   (e-subagent-runner-test--live-work-handle live participant-id)))
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
           (e-subagent-runner-test--live-pending live participant-id))
          (should (= runner-calls 1))
          (should (= running-calls 1))
          (should (eq (e-subagent-runner-test--live-status live participant-id)
                      'running)))))))

(ert-deftest e-subagent-runner-test-runner-start-failure-settles-once ()
  "A post-commit runner-start error publishes one running then one failure."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
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
                 live parent "parent-1" :type :reviewer :prompt "go"
                 :run-id "run-1" :task-key "review" :attempt 0
                 :on-running (lambda (_record) (cl-incf running-calls))
                 :on-failure
                 (lambda (_error _pending) (cl-incf early-failure-calls))
                 :runner (lambda (&rest _arguments)
                           (error "runner start exploded")))))
             (participant-id (plist-get pending :participant-id))
             (session-id (plist-get pending :session-id))
             (handle (e-subagent-runner-test--live-work-handle live participant-id)))
        (should (eq (plist-get pending :status) 'pending))
        (should (= running-calls 0))
        (e-work-finish admission '(:id "child"))
        (should (= running-calls 1))
        (should (= early-failure-calls 0))
        (e-subagent-runner-test--await-work-state handle 'failed)
        (e-subagent-runner-test--await-retired live participant-id)
        ;; Admission and all terminal history are durable even though live
        ;; execution coordination is gone.
        (let (records running terminal)
          (should
           (e-chat-test--wait-until
            (lambda ()
              (setq records (e-subagent-runner-test--records parent "parent-1")
                    running
                    (cl-count-if
                     (lambda (record)
                       (equal (plist-get record :tags)
                              '(subagent change running)))
                     records)
                    terminal
                    (cl-remove-if-not
                     (lambda (record)
                       (let ((fact
                              (e-board-orchestration-fact-from-record record)))
                         (eq (plist-get fact :type) 'terminal-report)))
                     records))
              (and (= running 1) (= (length terminal) 1)))
            5.0))
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
            (should
             (= (cl-count-if
                 (lambda (record)
                   (let ((fact
                          (e-board-orchestration-fact-from-record record)))
                     (eq (plist-get fact :type) 'terminal-report)))
                 (e-subagent-runner-test--records parent "parent-1"))
                1)))))))

(ert-deftest e-subagent-runner-test-admission-failure-never-registers-or-starts ()
  "A rejected durable admission fails once without a child live ghost."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
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
                 live parent "parent-1" :type :reviewer :prompt "go"
                 :on-failure
                 (lambda (_error _pending) (cl-incf failure-calls))
                 :runner (lambda (&rest _arguments)
                           (cl-incf runner-calls)
                           (list :cancel #'ignore))))
             (await-ref (plist-get pending :await-ref))
             (handle
              (e-subagent-runner-test--live-work-handle
               live (plist-get pending :participant-id))))
          (e-work-fail admission '(e-session-storage-error "denied"))
          (should (= runner-calls 0))
          (should (= failure-calls 1))
          (should-not (e-subagent-runner-test--live-list live "parent-1"))
          (should (eq (plist-get (e-work-status handle) :state) 'failed))
          (should (equal (plist-get (e-work-status handle) :error)
                         '(e-session-storage-error "denied")))
          ;; A stale late callback cannot create a child or settle twice.
          (e-work-finish admission '(:id "late-child"))
          (should (= runner-calls 0))
          (should (= failure-calls 1))
          (should-not (e-subagent-runner-test--live-list live "parent-1")))))))

(ert-deftest e-subagent-runner-test-spawn-records-lineage-and-seeds ()
  "Spawn creates a child under the parent lineage and seeds explicit context."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :project-root "/tmp/example-project/"))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               live parent "parent-1"
               :type :reviewer
               :prompt "Review tmp://plan.org"
               :seed-messages (list '(:role user :content "context note"))
               :label "review plan.org"
               :runner (e-subagent-runner-test--capturing-runner captured)))
             (child-session-id (plist-get record :session-id))
             (call (car captured))
             (child-harness (plist-get call :child-harness))
             (work-handle
              (e-subagent-runner-test--live-work-handle
               live (plist-get record :participant-id))))
        (should (eq (plist-get record :type) :reviewer))
        (should (eq (plist-get record :role) 'reviewer))
        (should (eq (plist-get record :status) 'running))
        (should (equal (e-subagent-live-reference-identity
                        (plist-get record :await-ref))
                       (list (e-subagent-runner-test--board-id
                              live (plist-get record :participant-id))
                             (plist-get record :participant-id))))
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
                         (eq (plist-get record :record-kind) 'fact))
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
               (e-subagent-runner-test--record-progress
                live (plist-get record :participant-id)
                work-handle 'tool-finished)))
          (should (eq (plist-get snapshot :event) 'tool-finished))
          (should
           (equal snapshot
                   (plist-get
                    (e-subagent-runner-test--live-get
                     live (plist-get record :participant-id))
                   :progress))))))))

(ert-deftest e-subagent-runner-test-explicit-project-root-wins-without-parent-state ()
  "An application-owned project root reaches the child admission directly."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)
                    :project-root "/tmp/ambient/"))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               live parent "parent-1"
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
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (participant-id (plist-get record :participant-id))
             (settle (plist-get (car captured) :on-settle)))
        (let ((final (funcall settle 'done :summary "3 issues found")))
          (should (eq (plist-get final :status) 'done))
          (should (equal (plist-get final :result-summary) "3 issues found"))
          (e-subagent-runner-test--await-retired live participant-id))))))

(ert-deftest e-subagent-runner-test-report-overrides-final-message ()
  "A child-reported result is authoritative over a later final message."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (participant-id (plist-get record :participant-id))
             (child-session-id (plist-get record :session-id))
             (settle (plist-get (car captured) :on-settle)))
        (e-subagent-runner-test--report
         live child-session-id
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
    (let* ((live (e-subagent-live-create))
           (e-subagent-actions-default-live live)
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
               live parent "parent-1"
               :type :reviewer :prompt "go"
               :run-id "run-1" :task-key "review" :attempt 2
               :report-admission admission
               :runner (e-subagent-runner-test--capturing-runner captured)))
             (participant-id (plist-get record :participant-id))
             (child-session-id (plist-get record :session-id))
             (child-harness
              (e-subagent-runner-test--live-child-harness live participant-id))
             (settle (plist-get (car captured) :on-settle)))
        (should-error
         (e-actions-call
          'subagents :report
          '(:summary "bad" :outputs [])
          (list :harness child-harness :session-id child-session-id
                :turn-id "child-turn"))
         :type 'user-error)
        (should-not (e-subagent-runner-test--live-reported live participant-id))
        (should (eq (e-subagent-runner-test--live-status live participant-id) 'running))
        (should-not (plist-member (e-subagent-runner-test--live-get live participant-id)
                                  :report-admission))
        (let ((ack
               (e-actions-call
                'subagents :report accepted
                (list :harness child-harness :session-id child-session-id
                      :turn-id "child-turn"))))
          (should (equal (plist-get ack :participant-id) participant-id))
          (should (plist-get ack :reported)))
        (e-subagent-runner-test--report live child-session-id [] "late replacement")
        (let* ((call (car calls))
               (assignment (car call)))
          (should (= (length calls) 2))
          (should (equal assignment
                         (list :run-id "run-1" :task-key "review" :attempt 2
                               :participant-id participant-id
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
    (let* ((live (e-subagent-live-create))
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
                 live parent "parent-1" :type :reviewer :prompt "go"
                 :report-admission admission
                 :runner (lambda (&rest _arguments) (list :cancel #'ignore))))
               (participant-id (plist-get pending :participant-id))
               (internal
                (gethash participant-id
                         (e-subagent-runner-test--live-pending-entries live))))
          (should (eq (plist-get internal :report-admission) admission))
          (should-not (plist-member pending :report-admission))
          (should-not
           (plist-member
            (e-subagent-runner-test--live-pending live participant-id)
            :report-admission))
          (e-work-finish held '(:status admitted))
          (should
           (e-chat-test--wait-until
            (lambda ()
              (gethash participant-id (e-subagent-runner-test--live-entries live)))
            5.0))
          (should
           (eq (plist-get
                (gethash participant-id (e-subagent-runner-test--live-entries live))
                :report-admission)
               admission))
          (should-not
           (plist-member (e-subagent-runner-test--live-get live participant-id)
                         :report-admission)))))))

(ert-deftest e-subagent-runner-test-forgotten-admission-does-not-leak-callback ()
  "Retiring pending coordination returns no report-admission function."
  (let* ((live (e-subagent-live-create))
         (admission (lambda (_assignment report) report))
         (participant-id
          (e-subagent-runner-test--live-reserve
           live :session-id "child" :parent-session-id "parent"
           :report-admission admission))
         (forgotten
          (e-subagent-runner-test--live-forget live participant-id)))
    (should-not (plist-member forgotten :report-admission))
    (should-not
     (e-subagent-runner-test--live-pending live participant-id))))

(ert-deftest e-subagent-runner-test-admission-required-child-cannot-finish-with-prose ()
  "A gated child that never reports settles as one explicit failed assignment."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil))
           terminal-calls)
      (e-harness-test-create-session parent :id "parent-1")
      (cl-letf (((symbol-function
                 'e-board-orchestration-actions-publish-terminal)
                 (lambda (_target assignment status &rest arguments)
                   (push (list assignment status arguments) terminal-calls)
                   (let ((work
                          (e-subagent-runner-test--deferred-work
                           "held-terminal-report")))
                     (e-work-finish work t)
                     work))))
        (let* ((record
                (e-subagent-runner-test--spawn
                 live parent "parent-1"
                 :type :reviewer :prompt "go"
                 :run-id "run-1" :task-key "review" :attempt 0
                 :report-admission (lambda (_assignment report) report)
                 :runner (e-subagent-runner-test--capturing-runner captured)))
               (participant-id (plist-get record :participant-id))
               (handle (e-subagent-runner-test--live-work-handle live participant-id))
               (settle (plist-get (car captured) :on-settle))
               (terminal (funcall settle 'done :summary "final prose only")))
          (should (eq (plist-get terminal :status) 'failed))
          (should (string-match-p "accepted report"
                                  (plist-get terminal :error)))
          (e-subagent-runner-test--await-work-state handle 'failed)
          (should (= (length terminal-calls) 1))
          (should (eq (cadar terminal-calls) 'failed))
          (e-subagent-runner-test--await-retired live participant-id)
          ;; A competing terminal callback cannot publish or settle twice.
          (should-not (funcall settle 'done :summary "late"))
          (should (= (length terminal-calls) 1)))))))

(ert-deftest e-subagent-runner-test-report-admission-failure-is-owner-local ()
  "A gated failure does not prevent an ungated sibling from succeeding."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (failed-captured (list nil))
           (sibling-captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((failed
              (e-subagent-runner-test--spawn
               live parent "parent-1" :type :reviewer :prompt "fail"
               :report-admission (lambda (_assignment _report)
                                   (user-error "invalid artifact"))
               :runner
               (e-subagent-runner-test--capturing-runner failed-captured)))
             (sibling
              (e-subagent-runner-test--spawn
               live parent "parent-1" :type :reviewer :prompt "succeed"
               :runner
               (e-subagent-runner-test--capturing-runner sibling-captured))))
        (should-error
         (e-subagent-runner-test--report live (plist-get failed :session-id) [] "bad")
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
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (failed-captured (list nil))
           (cancelled-captured (list nil))
           (admission (lambda (_assignment report) report)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((failed
              (e-subagent-runner-test--spawn
               live parent "parent-1" :type :reviewer :prompt "fail"
               :report-admission admission
               :runner
               (e-subagent-runner-test--capturing-runner failed-captured)))
             (cancelled
              (e-subagent-runner-test--spawn
               live parent "parent-1" :type :reviewer :prompt "cancel"
               :report-admission admission
               :runner
               (e-subagent-runner-test--capturing-runner cancelled-captured)))
             (failed-handle
              (e-subagent-runner-test--live-work-handle
               live (plist-get failed :participant-id)))
             (cancelled-handle
              (e-subagent-runner-test--live-work-handle
               live (plist-get cancelled :participant-id))))
        (let ((failure
               (funcall (plist-get (car failed-captured) :on-settle)
                        'failed :error "provider failed"))
              (cancellation
               (funcall (plist-get (car cancelled-captured) :on-settle)
                        'cancelled)))
          (should (eq (plist-get failure :status) 'failed))
          (should (equal (plist-get failure :error) "provider failed"))
          (should (eq (plist-get cancellation :status) 'cancelled))
          (e-subagent-runner-test--await-work-state failed-handle 'failed)
          (e-subagent-runner-test--await-work-state
           cancelled-handle 'cancelled)
          (e-subagent-runner-test--await-retired
           live (plist-get failed :participant-id))
          (e-subagent-runner-test--await-retired
           live (plist-get cancelled :participant-id)))))))

(ert-deftest e-subagent-runner-test-interrupt-and-shutdown ()
  "Interrupt calls the cancel function and marks the record cancelled."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (cancelled nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on-settle)
                                (list :cancel (lambda () (setq cancelled t))))))
             (participant-id (plist-get record :participant-id)))
        (let ((terminal
               (e-subagent-runner-test--interrupt
                live
                (e-subagent-runner-test--publication-target parent "parent-1")
                participant-id)))
        (should (eq (plist-get terminal :status) 'cancelled)))
        (should cancelled)
        (e-subagent-runner-test--await-work-state
         (e-subagent-runner-test--live-work-handle live participant-id)
         'cancelled)
        (e-subagent-runner-test--await-retired live participant-id)))))

(ert-deftest e-subagent-runner-test-interrupt-cleans-up-when-audit-target-fails ()
  "Explicit cancellation is not conditional on Board audit availability."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (cancelled nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               live parent "parent-1"
               :type :reviewer :prompt "go"
               :runner
               (lambda (_h _s _p _seed _on-settle)
                 (list :cancel (lambda () (setq cancelled t))))))
             (participant-id (plist-get record :participant-id))
             (work (e-subagent-runner-test--live-work-handle live participant-id)))
        (should-error
         (e-subagent-runner-test--interrupt live nil participant-id "audit unavailable")
         :type 'wrong-type-argument)
        (should cancelled)
        (e-subagent-runner-test--await-work-state work 'cancelled)
        (e-subagent-runner-test--await-retired live participant-id)
        (should-not (e-subagent-runner-test--live-list live "parent-1"))))))

(ert-deftest e-subagent-runner-test-action-interrupt-cleans-up-without-binding ()
  "The public action still cancels when its parent SQL target is unavailable."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (cancelled nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record
              (e-subagent-runner-test--spawn
               live parent "parent-1"
               :type :reviewer :prompt "go"
               :runner
               (lambda (_h _s _p _seed _on-settle)
                 (list :cancel (lambda () (setq cancelled t))))))
             (participant-id (plist-get record :participant-id))
             (work (e-subagent-runner-test--live-work-handle live participant-id)))
        (e-chat-service-close-board
         (e-chat-service-binding parent "parent-1"))
        (should-error
         (e-subagent-actions--interrupt
          live (list :harness parent :session-id "parent-1")
          (list :participant-id participant-id :reason "parent view closed"))
         :type 'e-subagent-error)
        (should cancelled)
        (e-subagent-runner-test--await-work-state work 'cancelled)
        (e-subagent-runner-test--await-retired live participant-id)))))

(ert-deftest e-subagent-runner-test-action-interrupt-rejects-ambiguous-binding-loss ()
  "A missing Board binding never selects an arbitrary same-id child."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (participant-id "participant-shared")
           (cancelled-a nil)
           (cancelled-b nil))
      (dolist (board-id '("board-a" "board-b"))
        (e-subagent-live-reserve-admission
         live board-id participant-id :work-handle (make-symbol "work"))
        (e-subagent-live-install
         live board-id participant-id
         :cancel (if (equal board-id "board-a")
                     (lambda () (setq cancelled-a t))
                   (lambda () (setq cancelled-b t)))))
      (should-error
       (e-subagent-actions--interrupt
        live (list :harness parent :session-id "missing-parent")
        (list :participant-id participant-id :reason "parent view closed"))
       :type 'e-subagent-live-error)
      (should-not cancelled-a)
      (should-not cancelled-b)
      (should (e-subagent-live-get live "board-a" participant-id))
      (should (e-subagent-live-get live "board-b" participant-id)))))

(ert-deftest e-subagent-runner-test-list-scopes-to-parent ()
  "The private owner keeps Board-keyed capabilities without a public list."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-test-create-session parent :id "parent-1")
      (e-harness-test-create-session parent :id "parent-2")
      (e-subagent-runner-test--spawn live parent "parent-1"
                        :type :reviewer :prompt "a" :runner noop)
      (e-subagent-runner-test--spawn live parent "parent-2"
                        :type :reviewer :prompt "b" :runner noop)
      (should-not (e-subagent-runner-test--live-list live "parent-1"))
      (should (= (hash-table-count
                  (e-subagent-runner-test--live-entries live))
                 2)))))

(ert-deftest e-subagent-runner-test-unknown-type-signals ()
  "Spawning a non-subagent type signals."
  (e-subagent-runner-test--with-instances
    (e-harness-instance-register
     :id :chat-plain
     :name "Chat"
     :kind 'chat
     :factory (lambda () (e-harness-create
                          :backend (e-backend-fake-create :items nil))))
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-session parent :id "parent-1")
      (should-error
       (e-subagent-runner-test--spawn live parent "parent-1"
                         :type :chat-plain :prompt "go"
                         :runner (lambda (_h _s _p _seed _on)
                                   (list :cancel #'ignore)))
       :type 'e-subagent-unknown-type))))

(ert-deftest e-subagent-runner-test-steer-and-send-dispatch ()
  "Steer and send route through the child board application service."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (steered nil)
           (queued nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (participant-id (plist-get record :participant-id)))
        (cl-letf (((symbol-function 'e-chat-service-steer-session)
                   (lambda (_h _s prompt &rest _) (setq steered prompt) "turn-1"))
                  ((symbol-function 'e-chat-service-queue-session)
                   (lambda (_h _s prompt &rest _) (setq queued prompt) nil)))
          (e-subagent-runner-test--steer
           live
           (e-subagent-runner-test--publication-target parent "parent-1")
           participant-id "steer this")
          (e-subagent-runner-test--send live participant-id "follow up")
          (should (equal steered "steer this"))
          (should (equal queued "follow up")))))))

(ert-deftest e-subagent-runner-test-send-refuses-retired-child ()
  "Send rejects a terminal child after its live record has been retired."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil))
           (queued nil))
      (e-harness-test-create-session parent :id "parent-1")
      (cl-letf (((symbol-function 'e-harness-test-queue-prompt)
                 (lambda (_h _s prompt &rest _) (setq queued prompt) nil)))
        ;; A failed child is no longer process-local or sendable.
        (let* ((record (e-subagent-runner-test--spawn
                        live parent "parent-1"
                        :type :reviewer :prompt "go"
                        :runner (e-subagent-runner-test--capturing-runner captured)))
               (participant-id (plist-get record :participant-id)))
          (funcall (plist-get (car captured) :on-settle)
                   'failed :error "boom")
          (e-subagent-runner-test--await-retired live participant-id)
          (should-error (e-subagent-runner-test--send live participant-id "follow up")
                        :type 'e-subagent-live-error)
          (should-not queued))
        ;; A done child is likewise refused.
        (let* ((record (e-subagent-runner-test--spawn
                        live parent "parent-1"
                        :type :reviewer :prompt "go"
                        :runner (e-subagent-runner-test--capturing-runner captured)))
               (participant-id (plist-get record :participant-id)))
          (funcall (plist-get (car captured) :on-settle) 'done :summary "ok")
          (e-subagent-runner-test--await-retired live participant-id)
          (should-not (gethash participant-id
                               (e-subagent-runner-test--live-entries live)))
          (should-error (e-subagent-runner-test--send live participant-id "follow up")
                        :type 'e-subagent-live-error)
          (should-not queued))))))

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
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-test-create-session parent :id "parent-1")
      (e-subagent-configure-type :lean :enable-layers '("web"))
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :lean :prompt "go" :runner noop))
             (harness (e-subagent-runner-test--live-child-harness
                       live (plist-get record :participant-id))))
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
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn live parent "parent-1"
                                       :type :lean :prompt "go" :runner noop))
             (harness (e-subagent-runner-test--live-child-harness
                       live (plist-get record :participant-id))))
        ;; Declared layers land on the child harness, with the always-added
        ;; child report layer appended.
        (should (equal (e-harness-enabled-layer-ids harness)
                       '(harness-base os-base subagents-child)))
        (should (equal (e-harness-capability-config harness 'agents-std-context)
                       '(:skills-include ("writing"))))
        ;; A parent override persists; the second spawn does not re-seed.
        (e-subagent-configure-type :lean :enable-layers '("web"))
        (e-subagent-runner-test--spawn live parent "parent-1"
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
      (let* ((live (e-subagent-live-create))
             (parent
              (e-harness-create
               :backend (e-backend-fake-create :items nil)
               :default-options '(:model "parent-model"
                                  :prompt-cache-default t
                                  :prompt-cache-retention "24h")))
             (noop (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
        (e-harness-test-create-session parent :id "parent-1")
        (let* ((record
                (e-subagent-runner-test--spawn live parent "parent-1"
                                  :type :cached-child
                                  :prompt "go"
                                  :runner noop))
               (child
                (e-subagent-runner-test--live-child-harness
                 live (plist-get record :participant-id)))
               (child-session-id (plist-get record :session-id))
               session-options)
          ;; Live installation precedes the callback step that enqueues this
          ;; durable session update.  Wait for the semantic condition rather
          ;; than treating an unrelated live-table observation as its barrier.
          (should
           (e-chat-test--wait-until
            (lambda ()
              (setq session-options
                    (plist-get
                     (e-board-producer-test-await
                      (e-session-async-query-state
                       (e-harness-sessions child) child-session-id))
                     :turn-options))
              (eq (plist-get session-options :prompt-cache-default) t))
            5.0))
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
    (let* ((live (e-subagent-live-create))
           (capability (e-subagents-parent-capability-create :live live))
           (store (e-store-create)))
      (should (eq (e-capability-id capability) 'subagents))
      (dolist (action '(:spawn :steer :send :interrupt :shutdown
                        :configure-type))
        (should (e-capabilities-action-spec capability action)))
      ;; Durable observation is supplied by the independent Board capability;
      ;; the parent surface retains only spawn/configure/live controls.
      (dolist (action '(:list :status :read))
        (should-not (e-capabilities-action-spec capability action)))
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
    (let* ((live (e-subagent-live-create))
           (capability (e-subagents-child-capability-create :live live))
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
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (child (e-subagent-runner-test--live-child-harness
                     live (plist-get record :participant-id)))
             (caps (mapcar #'e-capability-id
                           (e-harness-effective-capabilities child))))
        (should (memq 'subagents-child (e-harness-enabled-layer-ids child)))
        (should (memq 'subagents caps))))))

(ert-deftest e-subagent-runner-test-spawn-exposes-awaitable-work-handle ()
  "A spawned subagent carries an `e-work' handle that settles with its result."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (participant-id (plist-get record :participant-id))
             (handle (e-subagent-runner-test--live-work-handle live participant-id)))
        (should (e-work-handle-p handle))
        (should-not (e-request-terminal-p (e-work-handle-lifecycle handle)))
        (funcall (plist-get (car captured) :on-settle)
                 'done :summary "done" :outputs [:x])
        (e-subagent-runner-test--await-work-state handle 'finished)
        (should (equal (plist-get (plist-get (e-work-status handle) :result)
                                  :summary)
                       "done"))))))

(ert-deftest e-subagent-runner-test-work-handle-fails-on-failed-settle ()
  "A failed subagent settle fails the work handle."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (handle (e-subagent-runner-test--live-work-handle
                      live (plist-get record :participant-id))))
        (funcall (plist-get (car captured) :on-settle) 'failed :error "boom")
        (e-subagent-runner-test--await-work-state handle 'failed)))))

(ert-deftest e-subagent-runner-test-waitable-resolver-returns-handle ()
  "The registered `subagent' scheme resolves an id to its work handle."
  (e-subagent-runner-test--with-instances
    (let* ((e-waitable--resolvers (make-hash-table :test 'equal))
           (live (e-subagent-live-create))
           (parent (e-harness-create
                    :backend (e-backend-fake-create :items nil))))
      (e-subagents-register-waitable-resolver live)
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (lambda (_h _s _p _seed _on) (list :cancel #'ignore))))
             (participant-id (plist-get record :participant-id))
             (reference (plist-get record :await-ref)))
        (should (e-work-handle-p
                 (plist-get (e-waitable-resolve reference) :handle)))
        ;; An unknown opaque Board/participant reference is a per-reference
        ;; error, not a signal.
        (should
         (plist-get
          (e-waitable-resolve
           (e-subagent-live-reference "board-missing" "participant-missing"))
          :error))))))

(ert-deftest e-subagent-runner-test-progress-snapshots-are-monotonic-and-bounded ()
  "Child progress retains only the latest bounded snapshot on its work handle."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1"
                      :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (participant-id (plist-get record :participant-id))
             (work-handle (e-subagent-runner-test--live-work-handle live participant-id))
             (first (plist-get record :progress)))
        (should (eq (plist-get first :event) 'turn-started))
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 200.0)))
          (e-subagent-runner-test--record-progress
           live participant-id work-handle 'tool-finished))
        (let* ((updated (e-subagent-runner-test--live-get live participant-id))
               (progress (plist-get updated :progress)))
          (should-not (plist-member updated :progress-sequence))
          (should (eq (plist-get progress :event) 'tool-finished))
          (should (equal (plist-get progress :summary) "Finished tool"))
          (should (<= (length (prin1-to-string progress)) 4096)))
        (let ((finished
               (funcall (plist-get (car captured) :on-settle)
                        'done :summary "done")))
          (should (eq (plist-get finished :status) 'done))
          (e-subagent-runner-test--await-work-state work-handle 'finished)
          (e-subagent-runner-test--await-retired live participant-id))))))

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

(ert-deftest e-subagent-runner-test-direct-runner-awaits-capability-readiness ()
  "The first child provider request starts only after capability readiness."
  (let ((readiness (e-subagent-runner-test--deferred-work "child-readiness"))
        (submit-calls 0)
        subscriber)
    (cl-letf (((symbol-function 'e-subagent--seed-child) #'ignore)
              ((symbol-function 'e-harness-capability-readiness-start)
               (lambda (_harness _session-id) (list readiness)))
              ((symbol-function 'e-chat-service-subscribe)
               (lambda (_harness _session callback)
                 (setq subscriber callback)
                 'subscription))
              ((symbol-function 'e-chat-service-unsubscribe) #'ignore)
              ((symbol-function 'e-chat-service-submit-session)
               (lambda (&rest _arguments)
                 (cl-incf submit-calls)
                 (let ((work
                        (e-subagent-runner-test--deferred-work
                         "ready-child-admission")))
                   (e-work-finish work '(:status posted))
                   work)))
              ((symbol-function 'e-chat-service-abort-session) #'ignore))
      (e-subagent-direct-runner
       'child-harness "child" "go" nil (lambda (&rest _) nil))
      (should subscriber)
      (should (= submit-calls 0))
      (e-work-finish readiness '(:catalog-count 1))
      (should (= submit-calls 1)))))

(ert-deftest e-subagent-runner-test-readiness-failure-prevents-provider-request ()
  "A failed capability prerequisite settles the child before provider use."
  (let ((readiness (e-subagent-runner-test--deferred-work "failed-readiness"))
        (submit-calls 0)
        settlements)
    (cl-letf (((symbol-function 'e-subagent--seed-child) #'ignore)
              ((symbol-function 'e-harness-capability-readiness-start)
               (lambda (_harness _session-id) (list readiness)))
              ((symbol-function 'e-chat-service-subscribe)
               (lambda (&rest _arguments) 'subscription))
              ((symbol-function 'e-chat-service-unsubscribe) #'ignore)
              ((symbol-function 'e-chat-service-submit-session)
               (lambda (&rest _arguments) (cl-incf submit-calls)))
              ((symbol-function 'e-chat-service-abort-session) #'ignore))
      (e-subagent-direct-runner
       'child-harness "child" "go" nil
       (lambda (status &rest arguments)
         (push (cons status arguments) settlements)))
      (e-work-fail readiness '(e-mcp-backend-error "catalog unavailable"))
      (should (= submit-calls 0))
      (should (= (length settlements) 1))
      (should (eq (caar settlements) 'failed))
      (should (string-match-p
               "catalog unavailable"
               (plist-get (cdar settlements) :error))))))

(ert-deftest e-subagent-runner-test-readiness-cancel-prevents-provider-request ()
  "Cancelling during capability readiness never starts the child provider."
  (let ((readiness (e-subagent-runner-test--deferred-work "cancel-readiness"))
        (submit-calls 0)
        settlements)
    (cl-letf (((symbol-function 'e-subagent--seed-child) #'ignore)
              ((symbol-function 'e-harness-capability-readiness-start)
               (lambda (_harness _session-id) (list readiness)))
              ((symbol-function 'e-chat-service-subscribe)
               (lambda (&rest _arguments) 'subscription))
              ((symbol-function 'e-chat-service-unsubscribe) #'ignore)
              ((symbol-function 'e-chat-service-submit-session)
               (lambda (&rest _arguments) (cl-incf submit-calls)))
              ((symbol-function 'e-chat-service-abort-session) #'ignore))
      (let ((runner
             (e-subagent-direct-runner
              'child-harness "child" "go" nil
              (lambda (status &rest arguments)
                (push (cons status arguments) settlements)))))
        (funcall (plist-get runner :cancel)))
      (should (= submit-calls 0))
      (should (eq (plist-get (e-work-status readiness) :state) 'cancelled))
      (should (equal (mapcar #'car settlements) '(cancelled))))))

(ert-deftest e-subagent-runner-test-interventions-publish-provenance-and-stay-explicit ()
  "Steer, interrupt, and shutdown retain bounded audit facts without auto-cancel."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1" :type :reviewer :prompt "go"
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (participant-id (plist-get record :participant-id))
             (reason (make-string 300 ?r)))
        (let ((publication-work nil)
              (publish (symbol-function 'e-subagent--publish-board-fact)))
          (cl-letf (((symbol-function 'e-chat-service-steer-session)
                     (lambda (_harness _session prompt &rest _)
                       (should (equal prompt "Run one focused test."))))
                    ((symbol-function 'e-subagent--publish-board-fact)
                     (lambda (target &rest arguments)
                       (setq publication-work
                             (apply publish target arguments)))))
            (e-subagent-runner-test--steer
             live
             (e-subagent-runner-test--publication-target parent "parent-1")
             participant-id "Run one focused test." reason))
          ;; The publication target uses an independent read connection after
          ;; SQLite opens.  Await the exact write at this explicit test boundary
          ;; before inspecting its detached record page.
          (should (e-work-handle-p publication-work))
          (e-work-with-batch-await
            (e-work-await-batch publication-work :timeout 5.0)))
        (let* ((records (e-subagent-runner-test--records parent "parent-1"))
               (fact (car (last (cl-remove-if-not
                                 (lambda (record)
                                   (member 'intervention
                                           (plist-get record :tags)))
                                 records)))))
          (should (equal (plist-get (plist-get fact :attributes) :participant-id)
                         participant-id))
          (should (equal (plist-get (plist-get fact :attributes)
                                    :parent-session-id)
                         "parent-1"))
          (should (equal (plist-get (plist-get fact :attributes) :action)
                         'steer))
          (should (<= (string-width
                       (plist-get (plist-get fact :attributes) :reason))
                      240)))
        ;; The child remains running until an explicit intervention changes it.
        (should (eq (plist-get (e-subagent-runner-test--live-get live participant-id) :status)
                    'running))
        (let ((terminal
               (e-subagent-runner-test--interrupt
                live
                (e-subagent-runner-test--publication-target parent "parent-1")
                participant-id "No progress after steer.")))
          (should (eq (plist-get terminal :status) 'cancelled)))
        (e-subagent-runner-test--await-work-state
         (e-subagent-runner-test--live-work-handle live participant-id)
         'cancelled)
        (e-subagent-runner-test--await-retired live participant-id)))))

(ert-deftest e-subagent-runner-test-durable-report-precedes-local-settlement ()
  "A structured durable report is published once before the live settles."
  (e-subagent-runner-test--with-instances
    (let* ((live (e-subagent-live-create))
           (parent (e-harness-create :backend (e-backend-fake-create :items nil)))
           (captured (list nil)))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((record (e-subagent-runner-test--spawn
                      live parent "parent-1" :type :reviewer :prompt "go"
                      :run-id "run-1" :task-key "review" :attempt 0
                      :runner (e-subagent-runner-test--capturing-runner captured)))
             (child-session-id (plist-get record :session-id))
             (participant-id (plist-get record :participant-id))
             (handle (e-subagent-runner-test--live-work-handle live participant-id))
             (settle (plist-get (car captured) :on-settle))
             (live-entry (gethash participant-id
                                  (e-subagent-runner-test--live-entries live))))
        (should live-entry)
        (should-not (plist-member live-entry :publication-target))
        (should-not (plist-member live-entry :publication-function))
        (should-not (plist-member live-entry :parent-harness))
        (e-subagent-runner-test--report live child-session-id [] "reported")
        (should (eq (plist-get (e-subagent-runner-test--live-get live
                                                        (plist-get record :participant-id))
                               :status)
                    'running))
        (funcall settle 'done :summary "later final")
        ;; Terminal publication is the acknowledgement barrier: while Board
        ;; storage is in flight, the Work and private live entry remain
        ;; nonterminal/live.
        (should (e-request-lifecycle-progress
                 (e-work-handle-lifecycle handle)))
        (should (gethash (plist-get record :participant-id)
                         (e-subagent-runner-test--live-entries live)))
        (let (reports)
          (should
           (e-chat-test--wait-until
            (lambda ()
              (setq reports
                    (delq nil
                          (mapcar #'e-board-orchestration-fact-from-record
                                  (e-subagent-runner-test--records
                                   parent "parent-1"))))
              (= (length reports) 1))
            5.0))
          (should (= (length reports) 1))
          (should (equal (plist-get (plist-get (car reports) :payload) :summary)
                         "reported")))
        (e-subagent-runner-test--await-work-state handle 'finished)
        (e-subagent-runner-test--await-retired
         live (plist-get record :participant-id))
        (should (zerop (hash-table-count
                        (e-subagent-runner-test--live-pending-entries live))))
        (should-not (e-subagent-runner-test--live-order live))
        (should-not (e-subagent-runner-test--live-list live))
        (should-not (e-subagent-runner-test--live-work-handle
                     live (plist-get record :participant-id)))))))

(ert-deftest e-subagent-runner-test-board-dispatch-publishes-running-after-admission ()
  "Board dispatch settles only after queued and running facts are durable."
  (e-subagent-runner-test--with-instances
    (let ((parent (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((binding (e-chat-service-binding parent "parent-1"))
             (board-id (e-chat-service-binding-board-id binding))
             (target (e-subagent-runner-test--publication-target
                      parent "parent-1"))
             (assignment '(:run-id "run-1" :task-key "review" :attempt 0))
             work result state)
        (cl-letf (((symbol-function 'e-subagent-direct-runner)
                   (lambda (&rest _arguments) (list :cancel #'ignore))))
          (setq work
                 (e-subagent-runner-dispatch-start
                   target parent "parent-1"
                 :source-turn-id "parent-turn" :type :reviewer
                 :prompt "Review the Board task."
                 :run-id "run-1" :task-key "review" :attempt 0))
          (should (e-work-handle-p work))
          (setq result (e-board-producer-test-await work))
          (should (eq (plist-get result :status) 'admitted))
          (should (equal (plist-get result :board-id) board-id))
          (setq state
                (e-subagent-runner-assignment-state
                 board-id "run-1" "review" 0))
          (should (eq (plist-get state :state) 'live))
          (should (equal (plist-get state :participant-id)
                         (plist-get result :participant-id)))
          (should-not (plist-member state :work-handle))
          (should-not (plist-member state :harness)))
        (let ((statuses
               (delq nil
                     (mapcar
                      (lambda (fact)
                        (when (and (eq (plist-get fact :type) 'task-attempt)
                                   (equal (cl-loop for key in '(:run-id :task-key :attempt)
                                                   collect (plist-get
                                                            (plist-get fact :payload)
                                                            key))
                                          '("run-1" "review" 0)))
                          (plist-get (plist-get fact :payload) :status)))
                      (e-subagent-runner-test--orchestration-facts
                       parent "parent-1")))))
          (should (equal statuses '(queued running))))))))

(ert-deftest e-subagent-runner-test-board-dispatch-deadline-cancels-and-reports-once ()
  "A child deadline cancels the provider and publishes one typed terminal report."
  (e-subagent-runner-test--with-instances
    (let ((parent (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
          (cancelled nil)
          (settle nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((binding (e-chat-service-binding parent "parent-1"))
             (board-id (e-chat-service-binding-board-id binding))
             (target (e-subagent-runner-test--publication-target
                      parent "parent-1"))
             (deadline (+ (float-time) 0.5))
             (work nil)
             result child-work)
        (cl-letf (((symbol-function 'e-subagent-direct-runner)
                   (lambda (_child-harness _child-session-id _prompt _seed
                            on-settle _on-progress)
                     (setq settle on-settle)
                     (list :cancel
                           (lambda ()
                             (setq cancelled t)
                             ;; Model the provider's late cancellation event.
                             (funcall on-settle 'cancelled))))))
          (setq work
                (e-subagent-runner-dispatch-start
                 target parent "parent-1"
                 :source-turn-id "parent-turn" :type :reviewer
                 :prompt "Review the Board task."
                 :run-id "run-1" :task-key "review" :attempt 0
                 :deadline deadline))
          (setq result (e-board-producer-test-await work)))
        (should (eq (plist-get result :status) 'admitted))
        (setq child-work
              (e-subagent-runner-test--live-work-handle
               (e-subagent-runner-live-owner)
               (plist-get result :participant-id)))
        (should (e-work-handle-p child-work))
        (should (equal (plist-get (e-work-handle-context child-work) :deadline)
                       deadline))
        (should (equal (plist-get (e-work-handle-metadata child-work) :deadline)
                       deadline))
        (should
         (e-chat-test--wait-until
          (lambda ()
            (eq (plist-get (e-work-status child-work) :state) 'failed))
          5.0))
        (should cancelled)
        (let ((error (plist-get (e-work-status child-work) :error)))
          (should (eq (car error) 'e-work-deadline-exceeded)))
        ;; The provider callback runs after the deadline settlement and cannot
        ;; resurrect the private live record or publish a second report.
        (funcall settle 'done :summary "late provider result")
        (should-not
         (e-subagent-runner-assignment-state board-id "run-1" "review" 0))
        (let (reports)
          (should
           (e-chat-test--wait-until
            (lambda ()
              (setq reports
                    (delq nil
                          (mapcar #'e-board-orchestration-fact-from-record
                                  (e-subagent-runner-test--records
                                   parent "parent-1"))))
              (= (length (seq-filter
                          (lambda (fact)
                            (eq (plist-get fact :type) 'terminal-report))
                          reports))
                 1))
            5.0))
          (let ((terminal
                 (seq-find (lambda (fact)
                             (eq (plist-get fact :type) 'terminal-report))
                           reports)))
            (should (eq (plist-get (plist-get terminal :payload) :status)
                        'failed))
            (should
             (string-match-p
              "e-work-deadline-exceeded"
              (plist-get (plist-get terminal :payload) :error)))))))))

(ert-deftest e-subagent-runner-test-board-dispatch-coalesces-exact-assignment-claim ()
  "Concurrent calls share one pre-ack assignment dispatch and participant."
  (e-subagent-runner-test--with-instances
    (let ((parent (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
          (queued nil)
          (spawn-count 0)
          (publication-count 0))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((binding (e-chat-service-binding parent "parent-1"))
             (board-id (e-chat-service-binding-board-id binding))
             (target (e-subagent-runner-test--publication-target
                      parent "parent-1"))
             (assignment '(:run-id "run-1" :task-key "review" :attempt 0)))
        (cl-letf (((symbol-function 'e-subagent-runner--publish-attempt)
                   (lambda (_target _assignment status)
                     (cl-incf publication-count)
                     (let ((work
                            (e-subagent-runner-test--deferred-work
                             (format "dispatch-%s" status))))
                       (if (eq status 'queued)
                           (setq queued work)
                         (e-work-finish work t))
                       work)))
                  ((symbol-function 'e-subagent-spawn)
                   (lambda (&rest arguments)
                     (cl-incf spawn-count)
                     (let ((on-runner-started
                            (plist-get (nthcdr 3 arguments)
                                       :on-runner-started)))
                       (funcall on-runner-started
                                '(:participant-id "participant-one"
                                  :session-id "participant-one")))
                     nil)))
          (let* ((first
                  (e-subagent-runner-dispatch-start
                   target parent "parent-1"
                   :source-turn-id "parent-turn" :type :reviewer
                   :prompt "Review the Board task."
                   :run-id "run-1" :task-key "review" :attempt 0))
                 (second
                  (e-subagent-runner-dispatch-start
                   target parent "parent-1"
                   :source-turn-id "parent-turn" :type :reviewer
                   :prompt "Review the Board task."
                   :run-id "run-1" :task-key "review" :attempt 0)))
            (should (eq first second))
            (should queued)
            (should (= publication-count 1))
            (should (= spawn-count 0))
            (e-work-finish queued t)
            (should
             (e-chat-test--wait-until
              (lambda ()
                (eq (plist-get (e-work-status first) :state) 'finished))
              5.0))
            (should (= spawn-count 1))
            (should (= publication-count 2))
            (let ((result (e-work-handle-result first)))
              (should (eq (plist-get result :status) 'admitted))
              (should (equal (plist-get result :participant-id)
                             "participant-one")))
            (should (eq (plist-get (e-work-status second) :state) 'finished))
            (should-not
             (gethash
              (e-subagent-runner--dispatch-claim-key board-id assignment)
              e-subagent-runner--dispatch-claims))))))))

(ert-deftest e-subagent-runner-test-board-dispatch-runner-start-failure-is-not-admitted ()
  "A synchronous provider-start error publishes failure, not running."
  (e-subagent-runner-test--with-instances
    (let ((parent (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((binding (e-chat-service-binding parent "parent-1"))
             (board-id (e-chat-service-binding-board-id binding))
             (target (e-subagent-runner-test--publication-target
                      parent "parent-1"))
             work result)
        (cl-letf (((symbol-function 'e-subagent-direct-runner)
                   (lambda (&rest _arguments)
                     (error "dispatch provider start exploded"))))
          (setq work
                (e-subagent-runner-dispatch-start
                 target parent "parent-1"
                 :source-turn-id "parent-turn" :type :reviewer
                 :prompt "Review the Board task."
                 :run-id "run-1" :task-key "review" :attempt 0))
          (setq result (e-board-producer-test-await work)))
        (should (eq (plist-get result :status) 'failed))
        (should (string-match-p "dispatch provider start exploded"
                                (plist-get result :error)))
        (should-not
         (e-subagent-runner-assignment-state board-id "run-1" "review" 0))
        (let* ((facts (e-subagent-runner-test--orchestration-facts
                       parent "parent-1"))
               (attempt-statuses
                (mapcar
                 (lambda (fact)
                   (when (and (eq (plist-get fact :type) 'task-attempt)
                              (equal (plist-get (plist-get fact :payload)
                                                :run-id)
                                     "run-1")
                              (equal (plist-get (plist-get fact :payload)
                                                :task-key)
                                     "review")
                              (= (plist-get (plist-get fact :payload)
                                            :attempt)
                                 0))
                     (plist-get (plist-get fact :payload) :status)))
                 facts))
               (terminal
                (seq-find (lambda (fact)
                            (eq (plist-get fact :type) 'terminal-report))
                          facts)))
          (should (equal (delq nil attempt-statuses) '(queued)))
          (should-not
           (cl-find-if
            (lambda (record)
              (equal (plist-get record :tags) '(subagent change running)))
            (e-subagent-runner-test--records parent "parent-1")))
          (should terminal)
          (should (eq (plist-get (plist-get terminal :payload) :status)
                      'failed))
          (should-not
           (gethash
            (e-subagent-runner--dispatch-claim-key
             board-id '(:run-id "run-1" :task-key "review" :attempt 0))
            e-subagent-runner--dispatch-claims)))))))

(ert-deftest e-subagent-runner-test-board-dispatch-failure-publication-error-fails-work ()
  "Failure-publication storage errors remain failures of the action work."
  (e-subagent-runner-test--with-instances
    (let ((parent (e-harness-create
                   :backend (e-backend-fake-create :items nil))))
      (e-harness-test-create-session parent :id "parent-1")
      (let ((target (e-subagent-runner-test--publication-target
                     parent "parent-1")))
        (cl-letf (((symbol-function 'e-subagent-runner--publish-attempt)
                   (lambda (&rest _arguments)
                     (let ((work
                            (e-subagent-runner-test--deferred-work
                             "queued-dispatch")))
                       (e-work-finish work t)
                       work)))
                  ((symbol-function 'e-subagent-spawn)
                   (lambda (&rest arguments)
                     (funcall
                      (plist-get (nthcdr 3 arguments) :on-failure)
                      '(e-subagent-error "admission rejected") nil)))
                  ((symbol-function
                    'e-board-orchestration-actions-publish-terminal)
                   (lambda (&rest _arguments)
                     (let ((work
                            (e-subagent-runner-test--deferred-work
                             "failed-terminal-publication")))
                       (e-work-fail
                        work
                        '(e-board-sqlite-error
                          "terminal publication rejected"))
                       work))))
          (let* ((work
                  (e-subagent-runner-dispatch-start
                   target parent "parent-1"
                   :source-turn-id "parent-turn" :type :reviewer
                   :prompt "Review the Board task."
                   :run-id "run-1" :task-key "review" :attempt 0))
                 (status
                  (progn
                    (should
                     (e-chat-test--wait-until
                      (lambda ()
                        (memq (plist-get (e-work-status work) :state)
                              '(finished failed cancelled)))
                      5.0))
                    (e-work-status work))))
            (should (eq (plist-get status :state) 'failed))
            (should (string-match-p
                     "terminal publication rejected"
                     (e-work-error-message (plist-get status :error))))))))))

(ert-deftest e-subagent-runner-test-board-dispatch-failure-is-durable-domain-result ()
  "Admission failure publishes a terminal report before dispatch finishes."
  (e-subagent-runner-test--with-instances
    (let ((parent (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
          held)
      (e-harness-test-create-session parent :id "parent-1")
      (let* ((binding (e-chat-service-binding parent "parent-1"))
             (board-id (e-chat-service-binding-board-id binding))
             (target (e-subagent-runner-test--publication-target
                      parent "parent-1"))
             work result)
        (cl-letf (((symbol-function 'e-chat-service-create-participant-start)
                   (lambda (&rest _arguments)
                     (setq held
                           (e-subagent-runner-test--deferred-work
                            "held-board-dispatch-admission")))))
          (setq work
                (e-subagent-runner-dispatch-start
                 target parent "parent-1"
                 :source-turn-id "parent-turn" :type :reviewer
                 :prompt "Review the Board task."
                 :run-id "run-1" :task-key "review" :attempt 0))
          (should (e-chat-test--wait-until (lambda () held) 5.0))
          (e-work-fail held '(e-subagent-error "admission rejected"))
          (setq result (e-board-producer-test-await work)))
        (should (eq (plist-get result :status) 'failed))
        (should (string-match-p "admission rejected"
                                (plist-get result :error)))
        (should-not
         (e-subagent-runner-assignment-state board-id "run-1" "review" 0))
        (let* ((facts (e-subagent-runner-test--orchestration-facts
                       parent "parent-1"))
               (terminal
                (seq-find
                 (lambda (fact)
                   (eq (plist-get fact :type) 'terminal-report))
                 facts)))
          (should terminal)
          (should (eq (plist-get (plist-get terminal :payload) :status)
                      'failed)))))))

(ert-deftest e-subagent-runner-test-board-dispatch-publication-failure-surfaces ()
  "A failed queued publication fails dispatch without starting a child."
  (e-subagent-runner-test--with-instances
    (let ((parent (e-harness-create
                   :backend (e-backend-fake-create :items nil)))
          (spawned nil))
      (e-harness-test-create-session parent :id "parent-1")
      (let ((target (e-subagent-runner-test--publication-target
                     parent "parent-1")))
        (cl-letf (((symbol-function 'e-subagent-runner--publish-attempt)
                   (lambda (&rest _arguments)
                     (let ((work (e-subagent-runner-test--deferred-work
                                  "failed-queued-publication")))
                       (e-work-fail work '(e-board-sqlite-error
                                           "queued publication rejected"))
                       work)))
                  ((symbol-function 'e-subagent-spawn)
                   (lambda (&rest _arguments) (setq spawned t))))
          (let* ((work
                  (e-subagent-runner-dispatch-start
                   target parent "parent-1"
                   :source-turn-id "parent-turn" :type :reviewer
                   :prompt "Review the Board task."
                   :run-id "run-1" :task-key "review" :attempt 0))
                 (status
                  (progn
                    (should
                     (e-chat-test--wait-until
                      (lambda ()
                        (memq (plist-get (e-work-status work) :state)
                              '(finished failed cancelled)))
                      5.0))
                    (e-work-status work))))
            (should (eq (plist-get status :state) 'failed))
            (should (string-match-p
                     "queued publication rejected"
                     (e-work-error-message (plist-get status :error))))
            (should-not spawned)))))))

(provide 'e-subagent-runner-test)

;;; e-subagent-runner-test.el ends here

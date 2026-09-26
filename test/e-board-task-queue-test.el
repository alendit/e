;;; e-board-task-queue-test.el --- Board assignment queue bridge tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'e-backend)
(require 'e-board-task-queue)
(require 'e-chat-service)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-runtime-store)
(require 'e-session-sqlite)
(require 'e-subagent-live)
(require 'e-subagent-runner)
(require 'e-task-queue)
(require 'e-task-storage)
(require 'e-task-storage-sqlite)
(require 'e-work)
(load (expand-file-name "e-board-producer-test-support.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(cl-defmacro e-board-task-queue-test--with-runtime
    ((target parent storage runtime) &rest body)
  "Run BODY with TARGET, PARENT, STORAGE, and RUNTIME in isolated SQLite state."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  (let ((runtime-directory (make-symbol "runtime-directory"))
        (session-directory (make-symbol "session-directory"))
        (session-store (make-symbol "session-store"))
        (binding (make-symbol "binding")))
    `(let* ((e-harness-registry--instances (make-hash-table :test 'equal))
            (e-harness-registry--factories (make-hash-table :test 'equal))
            (e-harness-instance--instances (make-hash-table :test 'equal))
            (e-harness-instance--defaults (make-hash-table :test 'equal))
            (e-subagent--configured-harnesses
             (make-hash-table :test 'eq :weakness 'key))
            (e-subagent-runner--live-owner (e-subagent-live-create))
            (e-subagent-runner--dispatch-claims (make-hash-table :test 'equal))
            (e-work--unsettled-count 0)
            (e-work--unsettled-generation 0)
            (e-work--unsettled-change-functions nil)
            (,runtime-directory (make-temp-file "e-board-task-queue-" t))
            (,runtime (e-runtime-store-open ,runtime-directory))
            (,session-directory
             (make-temp-file "e-board-task-queue-session-" t))
            (,session-store
             (e-session-sqlite-store-create
              ,session-directory :runtime-store ,runtime :asynchronous t))
            (,parent
             (e-harness-create
              :sessions ,session-store
              :backend (e-backend-fake-create :items nil)))
            (,storage (e-task-storage-sqlite-create ,runtime))
            (,binding nil))
       (unwind-protect
           (progn
             (e-harness-instance-register
              :id :reviewer :name "Reviewer" :kind 'reviewer :subagent t
              :description "Board queue test runner."
              :factory
              (lambda ()
                (e-harness-create
                 :sessions ,session-store
                 :backend (e-backend-fake-create :items nil))))
             (let ((creation
                    (e-chat-service-create-session-start
                     :harness ,parent :id "parent-1"))
                   (binding-work
                    (e-chat-service-binding-start
                     ,parent "parent-1" nil t)))
               (e-board-producer-test-await creation)
               (setq ,binding
                     (e-board-producer-test-await binding-work)))
             (let ((,target (e-chat-service-publication-target ,binding)))
               ,@body))
         (when ,binding
           (ignore-errors (e-chat-service-close-board ,binding)))
         (ignore-errors (e-session-sqlite-store-close ,session-store))
         (ignore-errors (e-runtime-store-close ,runtime))
         (when (file-directory-p ,session-directory)
           (delete-directory ,session-directory t))
         (when (file-directory-p ,runtime-directory)
           (delete-directory ,runtime-directory t))))))

(defun e-board-task-queue-test--manifest (target run-id task-key attempt)
  "Publish one selected assignment manifest to TARGET."
  (e-board-producer-test-await
   (e-board-sqlite-publication-target-orchestration-fact-start
    target
    (list :version 1 :type 'manifest
          :idempotency-key (format "manifest:%s" run-id)
          :payload
          (list :run-id run-id
                :tasks (list (list :task-key task-key :required t
                                   :accepted-attempt attempt))
                :deadline '(:kind none)
                :descriptor (list :label run-id))))))

(defun e-board-task-queue-test--wait-until (predicate &optional timeout)
  "Wait up to TIMEOUT seconds for PREDICATE to return non-nil."
  (let ((deadline (+ (float-time) (or timeout 5.0))) value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun e-board-task-queue-test--await (work)
  "Await one WORK result at this explicit batch test boundary."
  (e-board-producer-test-await work))

(defun e-board-task-queue-test--real-subagent-runner
    (target parent parent-session-id dispatch-count)
  "Return a queue runner that dispatches through the real Board runner path."
  (let ((board-id (e-board-sqlite-publication-target-board-id target)))
    (lambda (task _harness on-settle)
      (cl-incf (car dispatch-count))
      (let* ((metadata (plist-get task :metadata))
             (run-id (plist-get metadata :board-run-id))
             (task-key (plist-get metadata :board-task-key))
             (attempt (plist-get metadata :board-attempt))
             (generation (plist-get metadata :daily-board-generation))
             (dispatch
              (e-subagent-runner-dispatch-start
               target parent parent-session-id
               :source-turn-id "board-task-turn" :type :reviewer
               :prompt (plist-get task :prompt)
               :label (plist-get task :summary)
               :run-id run-id :task-key task-key :attempt attempt
               :generation generation))
             child)
        (e-work-on-settle
         dispatch
         (lambda (settled)
           (let ((status (e-work-status settled)))
             (pcase (plist-get status :state)
               ('finished
                (let ((result (plist-get status :result)))
                  (if (eq (plist-get result :status) 'admitted)
                      (progn
                        (setq child
                              (e-subagent-live-work-handle
                               (e-subagent-runner-live-owner)
                               board-id (plist-get result :participant-id)))
                        (if (e-work-handle-p child)
                            (e-work-on-settle
                             child
                             (lambda (child-settled)
                               (let* ((child-status
                                       (e-work-status child-settled))
                                      (state (plist-get child-status :state)))
                                 (pcase state
                                   ('finished
                                    (funcall
                                     on-settle :status 'done
                                     :outputs
                                     (plist-get
                                      (e-work-handle-result child-settled)
                                      :outputs)))
                                   ('cancelled
                                    (funcall on-settle :status 'cancelled))
                                   (_
                                    (funcall
                                     on-settle :status 'failed
                                     :error
                                     (e-work-error-message
                                      (e-work-handle-error child-settled))))))))
                          (funcall on-settle :status 'interrupted
                                   :error "Admitted assignment has no live work")))
                    (funcall on-settle :status 'failed
                             :error "Subagent dispatch did not admit a child"))))
               ('cancelled (funcall on-settle :status 'cancelled))
               (_ (funcall on-settle :status 'failed
                           :error (e-work-error-message
                                   (e-work-handle-error settled)))))))
        (list :cancel (lambda ()
                        (if (e-work-handle-p child)
                            (e-work-cancel child)
                          (e-work-cancel dispatch)))))))))

(defun e-board-task-queue-test--facts (target)
  "Return orchestration facts currently stored on TARGET."
  (delq nil
        (mapcar #'e-board-orchestration-fact-from-record
                (e-board-producer-test-records target))))

(ert-deftest e-board-task-queue-test-restarts-reconcile-exact-board-assignment ()
  "A manifest before queue persistence dispatches once; a stale running row is orphaned."
  (e-board-task-queue-test--with-runtime (target parent storage runtime)
    (let* ((queue-id "board-assignment-queue")
           (dispatch-count (list 0))
           (queue
            (e-task-queue-create
             :storage storage :id queue-id :max-retries 0 :max-parallel 1
             :runner
             (e-board-task-queue-test--real-subagent-runner
              target parent "parent-1" dispatch-count)))
           (prompt "Review the durable Board assignment.")
           (board-metadata '(:daily-board-generation 1))
           (task-id (e-board-task-queue-task-id
                     target "run-1" "review" 0))
           (operations nil)
           (real-submit (symbol-function 'e-task-storage-submit))
           provider-settle task child-work)
      (e-board-task-queue-test--manifest target "run-1" "review" 0)
      (should (equal task-id
                     (e-board-task-queue-task-id target "run-1" "review" 0)))
      (should (= (length task-id) 70))
      (should-not (equal task-id
                         (e-board-task-queue-task-id target "run-1" "review" 1)))
      (should-not (equal task-id
                         (e-board-task-queue-task-id target "run-2" "review" 0)))
      (let ((other-target
             (e-board-sqlite-publication-target-create
              (e-chat-service-binding-sqlite-service
               (e-chat-service-binding parent "parent-1"))
              "other-board")))
        (should-not (equal task-id
                           (e-board-task-queue-task-id
                            other-target "run-1" "review" 0))))
      (cl-letf (((symbol-function 'e-task-storage-submit)
                 (lambda (candidate kind operation arguments on-settle)
                   (setq operations
                         (append operations (list (list kind operation))))
                   (funcall real-submit candidate kind operation arguments
                            on-settle)))
                ((symbol-function 'e-subagent-direct-runner)
                 (lambda (_child-harness _child-session-id _prompt _seed
                          on-settle &optional _progress _metadata)
                   (setq provider-settle on-settle)
                   (list :cancel #'ignore))))
        (setq task
              (e-board-task-queue-test--await
               (e-board-task-queue-reconcile
                queue target "run-1" "review" 0
                :prompt prompt :summary "Review" :harness-instance-id :reviewer
                :metadata board-metadata)))
        (should (eq (plist-get task :status) 'created))
        (should (= (plist-get (plist-get (plist-get task :record) :metadata)
                              :daily-board-generation)
                   1))
        (should (equal (seq-take operations 2)
                       '((write enqueue) (write claim-runnable))))
        (should-not (memq 'snapshot (mapcar #'cadr operations)))
        (setq task-id (plist-get task :task-id))
        (should
         (e-board-task-queue-test--wait-until
          (lambda ()
            (and provider-settle
                 (eq (plist-get
                      (e-subagent-runner-assignment-state
                       (e-board-sqlite-publication-target-board-id target)
                       "run-1" "review" 0)
                      :state)
                     'live)
                 (eq (plist-get (e-task-queue-get queue task-id) :status)
                     'running)))))
        (setq child-work
              (e-subagent-live-work-handle
               (e-subagent-runner-live-owner)
               (e-board-sqlite-publication-target-board-id target)
               (plist-get
                (e-subagent-runner-assignment-state
                 (e-board-sqlite-publication-target-board-id target)
                 "run-1" "review" 0)
                :participant-id)))
        (should (e-work-handle-p child-work))
        (should (= (car dispatch-count) 1))
        (let ((existing
               (e-board-task-queue-test--await
                (e-board-task-queue-reconcile
                 queue target "run-1" "review" 0
                 :prompt prompt :summary "Review"
                 :harness-instance-id :reviewer :metadata board-metadata))))
          (should (eq (plist-get existing :status) 'existing))
          (should (eq (plist-get (plist-get existing :record) :status) 'running))
          (should (= (car dispatch-count) 1)))
        (let* ((observer-queue
                (e-task-queue-create
                 :storage storage :id queue-id :max-retries 0
                 :runner (lambda (&rest _)
                           (ert-fail "An exact assignment must not dispatch"))))
               (existing-assignment
                (e-board-task-queue-test--await
                 (e-board-task-queue-reconcile
                  observer-queue target "run-1" "review" 0
                  :prompt prompt :summary "Review"
                  :harness-instance-id :reviewer :metadata board-metadata)))
               (observer-task-id (plist-get existing-assignment :task-id)))
          (should (eq (plist-get existing-assignment :status) 'existing))
          (should (equal observer-task-id task-id))
          (should-not (e-task-queue-work-handle observer-queue task-id))
          (should (= (car dispatch-count) 1)))
        (should-error
         (e-board-task-queue-test--await
          (e-board-task-queue-reconcile
           queue target "run-1" "review" 0
           :prompt "Changed immutable prompt." :summary "Review"
           :harness-instance-id :reviewer :metadata board-metadata))
         :type 'e-runtime-store-task-conflict)
        (let* ((restart-count (list 0))
               (restarted-queue
                (e-task-queue-create
                 :storage storage :id queue-id :max-retries 0
                 :runner (lambda (&rest _)
                           (cl-incf (car restart-count))
                           (error "An orphan must not be dispatched"))))
               (e-subagent-runner--live-owner (e-subagent-live-create))
               (e-subagent-runner--dispatch-claims
                (make-hash-table :test 'equal))
               (orphan
                (e-board-task-queue-test--await
                 (e-board-task-queue-reconcile
                  restarted-queue target "run-1" "review" 0
                  :prompt prompt :summary "Review"
                  :harness-instance-id :reviewer :metadata board-metadata))))
          (should (eq (plist-get orphan :status) 'orphan))
          (should (eq (plist-get (plist-get orphan :record) :status) 'running))
          (should (= (car restart-count) 0))
          (should (= (car dispatch-count) 1)))
        (funcall provider-settle 'done :summary "Review finished"
                 :outputs (vector (list :kind 'text :value "Reviewed")))
        (should
         (e-board-task-queue-test--wait-until
          (lambda ()
            (eq (plist-get (e-task-queue-get queue task-id) :status) 'done))))
        (let ((terminal
               (e-board-task-queue-test--await
                (e-board-task-queue-reconcile
                 queue target "run-1" "review" 0
                 :prompt prompt :summary "Review"
                 :harness-instance-id :reviewer :metadata board-metadata))))
          (should (eq (plist-get terminal :status) 'terminal))
          (should (eq (plist-get (plist-get terminal :record) :status) 'done)))
        (let* ((facts (e-board-task-queue-test--facts target))
               (attempts
                (seq-filter (lambda (fact)
                              (eq (plist-get fact :type) 'task-attempt))
                            facts))
               (reports
                (seq-filter (lambda (fact)
                              (eq (plist-get fact :type) 'terminal-report))
                            facts)))
          (should (= (length attempts) 2))
          (should (= (seq-count
                      (lambda (fact)
                        (eq (plist-get (plist-get fact :payload) :status) 'queued))
                      attempts)
                     1))
          (should (= (seq-count
                      (lambda (fact)
                        (eq (plist-get (plist-get fact :payload) :status) 'running))
                      attempts)
                     1))
          (should (= (length reports) 1))
          (should (eq (plist-get (plist-get (car reports) :payload) :status)
                      'done)))))))

(ert-deftest e-board-task-queue-test-live-handle-covers-claim-runner-window ()
  "A claimed task is existing before its runner registers a Board assignment."
  (e-board-task-queue-test--with-runtime (target _parent storage _runtime)
    (let* ((queue-id "board-claim-window-queue")
           (task-id (e-board-task-queue-task-id
                     target "run-window" "review" 0))
           (board-id (e-board-sqlite-publication-target-board-id target))
           (runner-entered nil)
           (work-handle-before-run nil)
           (assignment-state-before-run nil)
           (duplicate-work nil)
           (runner-settle nil)
           (operations nil)
           (real-submit (symbol-function 'e-task-storage-submit))
           (queue nil))
      (setq queue
            (e-task-queue-create
             :storage storage :id queue-id :max-retries 0
             :runner
             (lambda (_task _harness on-settle)
               (setq runner-entered t
                     runner-settle on-settle
                     work-handle-before-run
                     (e-task-queue-work-handle queue task-id)
                     assignment-state-before-run
                     (e-subagent-runner-assignment-state
                      board-id "run-window" "review" 0)
                     duplicate-work
                     (e-board-task-queue-reconcile
                      queue target "run-window" "review" 0
                      :prompt "Review transient claim." :summary "Window"
                      :harness-instance-id :reviewer))
               (list :cancel #'ignore))))
      (e-board-task-queue-test--manifest target "run-window" "review" 0)
      (cl-letf (((symbol-function 'e-task-storage-submit)
                 (lambda (candidate kind operation arguments on-settle)
                   (setq operations
                         (append operations (list (list kind operation))))
                   (funcall real-submit candidate kind operation arguments
                            on-settle))))
        (let ((created
               (e-board-task-queue-test--await
                (e-board-task-queue-reconcile
                 queue target "run-window" "review" 0
                 :prompt "Review transient claim." :summary "Window"
                 :harness-instance-id :reviewer))))
          (should (eq (plist-get created :status) 'created))
          (should (e-board-task-queue-test--wait-until
                   (lambda () runner-entered)))
          (should (e-work-handle-p work-handle-before-run))
          (should-not assignment-state-before-run)
          (should (e-work-handle-p duplicate-work))
          (let ((existing
                 (e-board-task-queue-test--await duplicate-work)))
            (should (eq (plist-get existing :status) 'existing))
            (should (eq (plist-get (plist-get existing :record) :status)
                        'running))
            (should-not assignment-state-before-run)
            (should (equal (seq-take operations 3)
                           '((write enqueue)
                             (write claim-runnable)
                             (write enqueue))))
            (should-not (memq 'snapshot (mapcar #'cadr operations))))
          (funcall runner-settle :status 'done
                   :outputs (vector (list :kind 'text :value "Complete")))
          (should
           (e-board-task-queue-test--wait-until
            (lambda ()
              (not (gethash task-id (e-task-queue-records queue)))))))))))

(ert-deftest e-board-task-queue-test-rejects-automatic-retries ()
  "Board assignments require retries to advance the Board attempt first."
  (e-board-task-queue-test--with-runtime (target _parent storage _runtime)
    (should-error
     (e-board-task-queue-reconcile
      (e-task-queue-create :storage storage :id "retries-enabled"
                           :max-retries 1)
      target "run-1" "review" 0 :prompt "Review.")
     :type 'e-board-task-queue-error)))

(ert-deftest e-board-task-queue-test-rejects-mismatched-canonical-task-id ()
  "A canonical enqueue result must retain the requested stable task id."
  (e-board-task-queue-test--with-runtime (target _parent _storage _runtime)
    (should-error
     (e-board-task-queue--classify
      nil target '(:run-id "run-1" :task-key "review" :attempt 0)
      "expected-task-id"
      '(:task-id "other-task-id" :created-p t
        :record (:task-id "other-task-id" :status queued)))
     :type 'e-board-task-queue-error)))

(provide 'e-board-task-queue-test)

;;; e-board-task-queue-test.el ends here

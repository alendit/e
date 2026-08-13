;;; e-board-orchestration-test.el --- Tests for durable board runs -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-board)
(require 'e-board-orchestration)

(defun e-board-orchestration-test--fact (type key payload)
  (list :version 1 :type type :idempotency-key key :payload payload))

(defun e-board-orchestration-test--manifest (&optional tasks deadline)
  (e-board-orchestration-test--fact
   'manifest "manifest-1"
   (list :run-id "run-1"
         :tasks (or tasks (list '(:task-key "required" :required t :accepted-attempt 0)
                                '(:task-key "optional" :required nil :accepted-attempt 0)))
         :deadline (or deadline '(:kind none)))))

(ert-deftest e-board-orchestration-test-fact-schema-is-bounded ()
  "Terminal reports trim text and reject unbounded output collections."
  (let ((report (e-board-orchestration-validate-fact
                 (e-board-orchestration-test--fact
                  'terminal-report "report-1"
                  (list :run-id "run-1" :task-key "task" :attempt 0 :status 'done
                        :summary (make-string 1000 ?x) :outputs [] :error (make-string 1000 ?e))))))
    (should (= (length (plist-get (plist-get report :payload) :summary))
               e-board-orchestration-summary-limit))
    (should (= (length (plist-get (plist-get report :payload) :error))
               e-board-orchestration-error-limit)))
  (should-error
   (e-board-orchestration-validate-fact
    (e-board-orchestration-test--fact
     'terminal-report "many" (list :run-id "run-1" :task-key "task" :attempt 0
                                   :status 'done :outputs (make-list 33 '(:kind file)))))))

(ert-deftest e-board-orchestration-test-publication-is-idempotent ()
  "The board source key makes retrying one durable fact a no-op."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "orchestration-test")))
    (let ((fact (e-board-orchestration-test--manifest)))
      (should (eq (e-board-publication-status
                   (e-board-orchestration-publish-fact board fact)) 'posted))
      (should (eq (e-board-publication-status
                   (e-board-orchestration-publish-fact board fact)) 'duplicate))
      (should (= (length (e-board-messages board)) 1)))))

(ert-deftest e-board-orchestration-test-required-and-optional-completion ()
  "Optional work cannot block a successful required group."
  (let* ((facts (list (e-board-orchestration-test--manifest)
                      (e-board-orchestration-test--fact
                       'terminal-report "required-0"
                       '(:run-id "run-1" :task-key "required" :attempt 0 :status done
                         :summary "done" :outputs []))))
         (projection (e-board-orchestration-reduce facts 0)))
    (should (eq (plist-get projection :terminal-status) 'done))))

(ert-deftest e-board-orchestration-test-selects-manifest-attempt ()
  "Only the manifest's accepted attempt settles the task."
  (let* ((facts (list (e-board-orchestration-test--manifest
                       (list '(:task-key "task" :required t :accepted-attempt 1)))
                      (e-board-orchestration-test--fact
                       'terminal-report "task-0"
                       '(:run-id "run-1" :task-key "task" :attempt 0 :status done
                         :summary "old" :outputs []))))
         (projection (e-board-orchestration-reduce facts)))
    (should (eq (plist-get (car (plist-get projection :tasks)) :state) 'pending))
    (should-not (plist-get projection :terminal-status))))

(ert-deftest e-board-orchestration-test-conflicting-report-is-visible ()
  "Conflicting accepted reports block a successful terminal projection."
  (let* ((facts (list (e-board-orchestration-test--manifest
                       (list '(:task-key "task" :required t :accepted-attempt 0)))
                      (e-board-orchestration-test--fact
                       'terminal-report "first"
                       '(:run-id "run-1" :task-key "task" :attempt 0 :status done :summary "a" :outputs []))
                      (e-board-orchestration-test--fact
                       'terminal-report "second"
                       '(:run-id "run-1" :task-key "task" :attempt 0 :status failed :summary "b" :outputs []))))
         (projection (e-board-orchestration-reduce facts)))
    (should (plist-get projection :conflicts))
    (should (eq (plist-get projection :terminal-status) 'failed))))

(ert-deftest e-board-orchestration-test-deadline-is-evidence-not-cancellation ()
  "An expired deadline changes only the projection evidence."
  (let* ((projection (e-board-orchestration-reduce
                      (list (e-board-orchestration-test--manifest nil '(:kind at :at 10))) 11))
         (task (car (plist-get projection :tasks))))
    (should (plist-get (plist-get projection :deadline) :expired))
    (should (eq (plist-get task :state) 'pending))
    (should-not (plist-get projection :terminal-status))))

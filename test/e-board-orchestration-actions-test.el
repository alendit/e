;;; e-board-orchestration-actions-test.el --- Tests for durable run actions -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-actions)
(require 'e-backend)
(require 'e-board)
(require 'e-board-orchestration)
(require 'e-board-orchestration-actions)
(require 'e-capabilities)
(require 'e-harness)

(defun e-board-orchestration-actions-test--fact (type key payload)
  "Return one versioned durable fact fixture."
  (list :version 1 :type type :idempotency-key key :payload payload))

(defun e-board-orchestration-actions-test--publish (board fact)
  "Publish FACT to BOARD as a durable orchestration fixture."
  (e-board-orchestration-publish-fact board fact))

(defun e-board-orchestration-actions-test--manifest (tasks)
  "Return a manifest fixture with TASKS."
  (e-board-orchestration-actions-test--fact
   'manifest "manifest"
   (list :run-id "run-1" :tasks tasks :deadline '(:kind none))))

(ert-deftest e-board-orchestration-actions-test-bounds-run-projections ()
  "Run observation keeps every durable section bounded with evidence."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (e-board-orchestration-actions-task-limit 1)
        (board (e-board-create :id "run-actions-bounded")))
    (e-board-orchestration-actions-test--publish
     board
     (e-board-orchestration-actions-test--manifest
      '((:task-key "one" :required t :accepted-attempt 0)
        (:task-key "two" :required t :accepted-attempt 0))))
    (dolist (task '("one" "two"))
      (e-board-orchestration-actions-test--publish
       board
       (e-board-orchestration-actions-test--fact
        'terminal-report (format "report-%s" task)
        (list :run-id "run-1" :task-key task :attempt 0 :status 'done
              :summary task :outputs []))))
    (let ((projection (e-board-orchestration-actions-run-projection board "run-1")))
      (should (= (length (plist-get projection :tasks)) 1))
      (should (= (length (plist-get projection :accepted-reports)) 1))
      (should (plist-get projection :tasks-truncated))
      (should (plist-get projection :accepted-reports-truncated))
      (should (= (length (plist-get (plist-get projection :manifest) :tasks)) 1))
      (should (eq (plist-get projection :terminal-status) 'done)))))

(ert-deftest e-board-orchestration-actions-test-notifies-projection-subscribers ()
  "A posted board fact pushes one bounded projection without session reads."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "run-actions-notify"))
        observed)
    (let ((e-board-orchestration-actions-projection-change-functions
           (list (lambda (changed-board run-id projection)
                   (setq observed (list changed-board run-id projection))))) )
      (e-board-orchestration-actions-test--publish
       board
       (e-board-orchestration-actions-test--manifest
        '((:task-key "task" :required t :accepted-attempt 0))))
      (should (eq (car observed) board))
      (should (equal (cadr observed) "run-1"))
      (should (equal (plist-get (nth 2 observed) :run-id) "run-1")))))

(ert-deftest e-board-orchestration-actions-test-actions-read-the-context-board ()
  "The parent action surface returns bounded run state from its board context."
  (let* ((e-board--registry (make-hash-table :test 'equal))
         (board (e-board-create :id "run-actions-context"))
         (harness (e-harness-create :backend (e-backend-fake-create :items nil)))
         (capability (e-capability-create
                      :id 'runs
                      :actions (e-board-orchestration-actions-parent-alist))))
    (e-board-orchestration-actions-test--publish
     board
     (e-board-orchestration-actions-test--manifest
      '((:task-key "task" :required t :accepted-attempt 0))))
    (e-harness-activate-capability harness capability)
    (e-harness-create-session harness :id "parent")
    (cl-letf (((symbol-function 'e-board-orchestration-actions--context-board)
               (lambda (_context) board)))
      (let ((context (list :harness harness :session-id "parent")))
        (should (equal (plist-get (e-actions-call 'runs :run-status
                                                   '(:run-id "run-1") context)
                                  :run-id)
                       "run-1"))
        (should (= (length (e-actions-call 'runs :list-runs nil context)) 1))))))

(ert-deftest e-board-orchestration-actions-test-replays-report-after-manifest-arrives ()
  "A validated report may precede its manifest without losing later projection."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "run-actions-report-first"))
        observed)
    (let ((e-board-orchestration-actions-projection-change-functions
           (list (lambda (_board _run-id projection) (push projection observed)))))
      (e-board-orchestration-actions-test--publish
       board
       (e-board-orchestration-actions-test--fact
        'terminal-report "report"
        '(:run-id "run-1" :task-key "task" :attempt 0 :status done
          :summary "done" :outputs [])))
      (should-not observed)
      (e-board-orchestration-actions-test--publish
       board
       (e-board-orchestration-actions-test--manifest
        '((:task-key "task" :required t :accepted-attempt 0))))
      (should (= (length observed) 1))
      (should (eq (plist-get (car observed) :terminal-status) 'done))
      (should (= (length (plist-get (car observed) :accepted-reports)) 1)))))

(ert-deftest e-board-orchestration-actions-test-terminal-retains-participant-identity ()
  "A durable terminal report keeps the exact participant session label."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "run-actions-participant")))
    (e-board-orchestration-actions-test--publish
     board
     (e-board-orchestration-actions-test--manifest
      '((:task-key "task" :required t :accepted-attempt 0))))
    (e-board-orchestration-actions-publish-terminal
     board '(:run-id "run-1" :task-key "task" :attempt 0) 'failed
     :summary "stopped" :outputs [] :error "worker died"
     :author '(:session-id "participant-1"))
    (let* ((projection
            (e-board-orchestration-actions-run-projection board "run-1"))
           (report (plist-get (car (plist-get projection :tasks))
                              :accepted-report)))
      (should (equal (plist-get report :participant-session-id)
                     "participant-1")))))

(ert-deftest e-board-orchestration-actions-test-select-next-attempt-is-idempotent ()
  "Retry selection advances exactly once and reuses one durable fact identity."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "run-actions-retry")))
    (e-board-orchestration-actions-test--publish
     board
     (e-board-orchestration-actions-test--manifest
      '((:task-key "task" :required t :accepted-attempt 0))))
    (let ((first
           (e-board-orchestration-actions-select-next-attempt
            board "run-1" "task" 0))
          (again
           (e-board-orchestration-actions-select-next-attempt
            board "run-1" "task" 0)))
      (should (= first 1))
      (should (= again 1))
      (should (= (plist-get
                  (car (plist-get
                        (e-board-orchestration-run-projection board "run-1")
                        :tasks))
                  :accepted-attempt)
                 1))
      (should (= (length (e-board-messages board)) 2)))))

(provide 'e-board-orchestration-actions-test)

;;; e-board-orchestration-actions-test.el ends here

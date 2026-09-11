;;; e-board-orchestration-actions-test.el --- SQL run action tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-actions)
(require 'e-backend)
(require 'e-board-orchestration)
(require 'e-board-orchestration-actions)
(require 'e-capabilities)
(require 'e-harness)
(load (expand-file-name "e-board-producer-test-support.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-board-orchestration-actions-test--fact (type key payload)
  "Return one versioned durable fact fixture."
  (list :version 1 :type type :idempotency-key key :payload payload))

(defun e-board-orchestration-actions-test--publish (target fact)
  "Publish FACT to SQL TARGET and return its canonical result."
  (e-board-producer-test-await
   (e-board-sqlite-publication-target-orchestration-fact-start target fact)))

(defun e-board-orchestration-actions-test--manifest (tasks)
  "Return a manifest fixture with TASKS."
  (e-board-orchestration-actions-test--fact
   'manifest "manifest"
   (list :run-id "run-1" :tasks tasks :deadline '(:kind none))))

(defun e-board-orchestration-actions-test--projection (target)
  "Return TARGET's detached RUN-1 projection."
  (e-board-producer-test-await
   (e-board-orchestration-actions-run-projection target "run-1")))

(ert-deftest e-board-orchestration-actions-test-terminal-publishes-one-sql-fact ()
  "Terminal publication commits exactly one canonical orchestration fact."
  (e-board-producer-test-with-target (target)
    (let (reconciled)
      (cl-letf (((symbol-function
                  'e-chat-service-reconcile-sqlite-continuation-target)
                 (lambda (candidate) (setq reconciled candidate))))
        (e-board-producer-test-await
         (e-board-orchestration-actions-publish-terminal
          target '(:run-id "run-1" :task-key "task" :attempt 0) 'done
          :summary "done" :outputs [])))
      (should (eq reconciled target)))
    (let* ((records (e-board-producer-test-records target))
           (fact (e-board-orchestration-fact-from-record (car records))))
      (should (= (length records) 1))
      (should (eq (plist-get fact :type) 'terminal-report))
      (should (equal (plist-get fact :idempotency-key)
                     "terminal:run-1:task:0")))))

(ert-deftest e-board-orchestration-actions-test-bounds-sql-run-projections ()
  "Run observation keeps every detached durable section bounded with evidence."
  (e-board-producer-test-with-target (target)
    (let ((e-board-orchestration-actions-task-limit 1))
      (e-board-orchestration-actions-test--publish
       target
       (e-board-orchestration-actions-test--manifest
        '((:task-key "one" :required t :accepted-attempt 0)
          (:task-key "two" :required t :accepted-attempt 0))))
      (dolist (task '("one" "two"))
        (e-board-orchestration-actions-test--publish
         target
         (e-board-orchestration-actions-test--fact
          'terminal-report (format "report-%s" task)
          (list :run-id "run-1" :task-key task :attempt 0 :status 'done
                :summary task :outputs []))))
      (let ((projection (e-board-orchestration-actions-test--projection target)))
        (should (= (length (plist-get projection :tasks)) 1))
        (should (= (length (plist-get projection :accepted-reports)) 1))
        (should (plist-get projection :tasks-truncated))
        (should (plist-get projection :accepted-reports-truncated))
        (should (= (length (plist-get (plist-get projection :manifest) :tasks))
                   1))
        (should (eq (plist-get projection :terminal-status) 'done))))))

(ert-deftest e-board-orchestration-actions-test-actions-read-sql-context-target ()
  "The parent action surface returns request-scoped SQL run state."
  (e-board-producer-test-with-target (target)
    (let* ((harness (e-harness-create
                     :backend (e-backend-fake-create :items nil)))
           (capability (e-capability-create
                        :id 'runs
                        :actions (e-board-orchestration-actions-parent-alist))))
      (e-board-orchestration-actions-test--publish
       target
       (e-board-orchestration-actions-test--manifest
        '((:task-key "task" :required t :accepted-attempt 0))))
      (e-harness-activate-capability harness capability)
      (e-harness-create-session harness :id "parent")
      (cl-letf (((symbol-function 'e-board-orchestration-actions--context-target)
                 (lambda (_context) target)))
        (let* ((context (list :harness harness :session-id "parent"))
               (run-dispatch
                (e-actions-dispatch 'runs :run-status
                                    '(:run-id "run-1") context))
               (list-dispatch
                (e-actions-dispatch 'runs :list-runs nil context))
               (run (e-board-producer-test-await
                     (plist-get run-dispatch :request)))
               (runs (e-board-producer-test-await
                      (plist-get list-dispatch :request))))
          (should (equal (plist-get run :run-id) "run-1"))
          (should (= (length runs) 1)))))))

(ert-deftest e-board-orchestration-actions-test-report-before-manifest-reduces ()
  "A report may precede its manifest without losing the later SQL projection."
  (e-board-producer-test-with-target (target)
    (e-board-orchestration-actions-test--publish
     target
     (e-board-orchestration-actions-test--fact
      'terminal-report "report"
      '(:run-id "run-1" :task-key "task" :attempt 0 :status done
        :summary "done" :outputs [])))
    (should-error (e-board-orchestration-actions-test--projection target)
                  :type 'e-board-orchestration-invalid-fact)
    (e-board-orchestration-actions-test--publish
     target
     (e-board-orchestration-actions-test--manifest
      '((:task-key "task" :required t :accepted-attempt 0))))
    (let ((projection (e-board-orchestration-actions-test--projection target)))
      (should (eq (plist-get projection :terminal-status) 'done))
      (should (= (length (plist-get projection :accepted-reports)) 1)))))

(ert-deftest e-board-orchestration-actions-test-terminal-retains-participant-identity ()
  "A SQL terminal report keeps the exact participant session label."
  (e-board-producer-test-with-target (target)
    (e-board-orchestration-actions-test--publish
     target
     (e-board-orchestration-actions-test--manifest
      '((:task-key "task" :required t :accepted-attempt 0))))
    (e-board-producer-test-await
     (e-board-orchestration-actions-publish-terminal
      target '(:run-id "run-1" :task-key "task" :attempt 0) 'failed
      :summary "stopped" :outputs [] :error "worker died"
      :author '(:session-id "participant-1")))
    (let* ((projection (e-board-orchestration-actions-test--projection target))
           (report (plist-get (car (plist-get projection :tasks))
                              :accepted-report)))
      (should (equal (plist-get report :participant-session-id)
                     "participant-1")))))

(provide 'e-board-orchestration-actions-test)

;;; e-board-orchestration-actions-test.el ends here

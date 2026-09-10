;;; e-board-runs-shell-test.el --- Tests for SQL-backed run shell -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-board-orchestration)
(require 'e-board-orchestration-actions)
(require 'e-board-runs-shell)
(require 'e-subagent-registry)
(load (expand-file-name "e-board-producer-test-support.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defun e-board-runs-shell-test--fact (type key payload)
  "Return one versioned durable fact fixture."
  (list :version 1 :type type :idempotency-key key :payload payload))

(defun e-board-runs-shell-test--publish (target fact)
  "Publish FACT to SQL TARGET."
  (e-board-producer-test-await
   (e-board-sqlite-publication-target-orchestration-fact-start target fact)))

(defun e-board-runs-shell-test--manifest ()
  "Return the shell fixture manifest."
  (e-board-runs-shell-test--fact
   'manifest "manifest"
   '(:run-id "run-1"
     :descriptor (:date "2026-09-08")
     :tasks ((:task-key "task" :required t :accepted-attempt 0))
     :continuation (:session-id "owner-1" :prompt "continue"
                    :publication-key "continue-run-1")
     :deadline (:kind none))))

(defun e-board-runs-shell-test--projection (target)
  "Return TARGET's detached RUN-1 projection."
  (e-board-producer-test-await
   (e-board-orchestration-actions-run-projection target "run-1")))

(defun e-board-runs-shell-test--wait (predicate)
  "Wait at this explicit test boundary until PREDICATE returns non-nil."
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defconst e-board-runs-shell-test--deferred-work-spec
  (e-work-spec-create
   :id "board-runs-shell-test-query" :execution 'cooperative
   :interactive-policy 'async :owner 'test
   :runner (lambda (_handle _arguments _context) :deferred))
  "Manual query work used to prove list-buffer return-before-read behavior.")

(defun e-board-runs-shell-test--cleanup (buffer)
  "Kill BUFFER and remove the process-local registry observer."
  (when (buffer-live-p buffer) (kill-buffer buffer))
  (remove-hook 'e-subagent-registry-change-functions
               #'e-board-runs-shell--refresh-registry-buffers))

(ert-deftest e-board-runs-shell-test-renders-sql-projection-and-conflict ()
  "The run shell renders detached SQL state and warns about conflicts."
  (e-board-producer-test-with-target (target)
    (e-board-runs-shell-test--publish target (e-board-runs-shell-test--manifest))
    (e-board-runs-shell-test--publish
     target
     (e-board-runs-shell-test--fact
      'terminal-report "report-a"
      '(:run-id "run-1" :task-key "task" :attempt 0 :status done
        :summary "first" :outputs [])))
    (e-board-runs-shell-test--publish
     target
     (e-board-runs-shell-test--fact
      'terminal-report "report-b"
      '(:run-id "run-1" :task-key "task" :attempt 0 :status failed
        :summary "second" :outputs [])))
    (let ((buffer (e-board-runs-list-buffer :target target)))
      (unwind-protect
          (progn
            (e-board-runs-shell-test--wait
             (lambda ()
               (with-current-buffer buffer (= (length tabulated-list-entries) 1))))
            (with-current-buffer buffer
              (let ((cells (cadr (car tabulated-list-entries))))
                (should (equal (aref cells 1) "producer-board"))
                (should (equal (aref cells 3) "failed"))
                (should (equal (aref cells 7) "1"))
                (should (eq (get-text-property 0 'face (aref cells 7))
                            'font-lock-warning-face)))))
        (e-board-runs-shell-test--cleanup buffer)))))

(ert-deftest e-board-runs-shell-test-query-renders-after-immediate-return ()
  "The list buffer returns before its detached SQL query settles."
  (e-board-producer-test-with-target (target)
    (let ((work (e-work-start e-board-runs-shell-test--deferred-work-spec nil))
          buffer)
      (cl-letf (((symbol-function 'e-board-orchestration-actions-list-runs)
                 (lambda (_target &optional _now) work)))
        (setq buffer (e-board-runs-list-buffer :target target))
        (unwind-protect
            (progn
              (with-current-buffer buffer (should-not tabulated-list-entries))
              (e-work-finish
               work
               (list '(:run-id "run-1" :manifest (:tasks nil)
                       :tasks nil :terminal-status done)))
              (with-current-buffer buffer
                (should (= (length tabulated-list-entries) 1))))
          (e-board-runs-shell-test--cleanup buffer))))))

(ert-deftest e-board-runs-shell-test-commands-are-interactive ()
  "The bounded run shell exposes detail and manual refresh commands."
  (e-board-producer-test-with-target (target)
    (let ((buffer (e-board-runs-list-buffer :target target)))
      (unwind-protect
          (with-current-buffer buffer
            (dolist (cell '(("RET" . e-board-runs-shell-show-details)
                            ("r" . e-board-runs-shell-show-raw-activity)
                            ("g" . e-board-runs-shell-refresh)))
              (let ((binding
                     (keymap-lookup e-board-runs-shell-mode-map (car cell))))
                (should (eq binding (cdr cell)))
                (should (commandp binding)))))
        (e-board-runs-shell-test--cleanup buffer)))))

(ert-deftest e-board-runs-shell-test-summary-labels-identities-and-failure ()
  "The default detail names durable identities and the first failure."
  (e-board-producer-test-with-target (target)
    (e-board-runs-shell-test--publish target (e-board-runs-shell-test--manifest))
    (e-board-runs-shell-test--publish
     target
     (e-board-runs-shell-test--fact
      'terminal-report "failed"
      '(:run-id "run-1" :task-key "task" :attempt 0 :status failed
        :summary "worker stopped" :error "first admission failure"
        :participant-session-id "participant-1" :outputs [])))
    (let ((summary (e-board-runs-shell--format-summary
                    target (e-board-runs-shell-test--projection target))))
      (dolist (line '("Daily run id: run-1"
                      "Board id: producer-board"
                      "Owner session id: owner-1"
                      "Task: task"
                      "Attempt: 0"
                      "Participant session id: participant-1"
                      "Admission: terminal"
                      "Disposition: terminal"
                      "First failure: first admission failure"))
        (should (string-match-p (regexp-quote line) summary)))
      (should-not (string-match-p ":manifest" summary)))))

(ert-deftest e-board-runs-shell-test-summary-exposes-live-pending-admission ()
  "A live reserved child supplements, but does not replace, SQL run state."
  (e-board-producer-test-with-target (target)
    (let ((registry (e-subagent-registry-create)))
      (e-board-runs-shell-test--publish
       target (e-board-runs-shell-test--manifest))
      (e-subagent-registry-reserve-admission
       registry :type :worker :role 'worker :session-id "participant-pending"
       :parent-session-id "owner-1" :label "Daily task" :schedule 'direct
       :run-id "run-1" :task-key "task" :attempt 0)
      (should-not (e-subagent-registry-list registry))
      (let ((summary (e-board-runs-shell--format-summary
                      target (e-board-runs-shell-test--projection target)
                      registry)))
        (should (string-match-p "Participant session id: participant-pending"
                                summary))
        (should (string-match-p "Admission: pending" summary))
        (should (string-match-p "Disposition: pending" summary))))))

(ert-deftest e-board-runs-shell-test-summary-labels-successor-retrying ()
  "A selected successor attempt is retrying while live admission waits."
  (e-board-producer-test-with-target (target)
    (let ((registry (e-subagent-registry-create)))
      (e-board-runs-shell-test--publish
       target (e-board-runs-shell-test--manifest))
      (e-board-runs-shell-test--publish
       target
       (e-board-runs-shell-test--fact
        'attempt-selection "retry-1"
        '(:run-id "run-1" :task-key "task" :attempt 1)))
      (e-subagent-registry-reserve-admission
       registry :type :worker :role 'worker :session-id "participant-retry"
       :parent-session-id "owner-1" :label "Daily task retry" :schedule 'direct
       :run-id "run-1" :task-key "task" :attempt 1)
      (let ((summary (e-board-runs-shell--format-summary
                      target (e-board-runs-shell-test--projection target)
                      registry)))
        (should (string-match-p "Attempt: 1" summary))
        (should (string-match-p "Admission: pending" summary))
        (should (string-match-p "Disposition: retrying" summary))))))

(provide 'e-board-runs-shell-test)

;;; e-board-runs-shell-test.el ends here

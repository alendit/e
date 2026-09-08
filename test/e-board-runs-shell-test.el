;;; e-board-runs-shell-test.el --- Tests for durable run shell -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-board)
(require 'e-board-orchestration)
(require 'e-board-orchestration-actions)
(require 'e-board-runs-shell)
(require 'e-subagent-registry)

(defun e-board-runs-shell-test--fact (type key payload)
  "Return one versioned durable fact fixture."
  (list :version 1 :type type :idempotency-key key :payload payload))

(defun e-board-runs-shell-test--publish (board fact)
  "Publish FACT to BOARD as a durable orchestration fixture."
  (e-board-orchestration-publish-fact board fact))

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

(defun e-board-runs-shell-test--summary (board &optional registry)
  "Return BOARD's signal-focused RUN-1 summary using REGISTRY."
  (e-board-runs-shell--format-summary
   board
   (e-board-orchestration-actions-run-projection board "run-1")
   registry))

(ert-deftest e-board-runs-shell-test-renders-projection-and-conflict ()
  "The run shell displays durable status and warns about visible conflicts."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "run-shell-conflict")))
    (e-board-runs-shell-test--publish board (e-board-runs-shell-test--manifest))
    (e-board-runs-shell-test--publish
     board
     (e-board-runs-shell-test--fact
      'terminal-report "report-a"
      '(:run-id "run-1" :task-key "task" :attempt 0 :status done
        :summary "first" :outputs [])))
    (e-board-runs-shell-test--publish
     board
     (e-board-runs-shell-test--fact
      'terminal-report "report-b"
      '(:run-id "run-1" :task-key "task" :attempt 0 :status failed
        :summary "second" :outputs [])))
    (let ((buffer (e-board-runs-list-buffer :board board)))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'e-board-runs-shell-mode))
            (should (= (length tabulated-list-entries) 1))
            (let ((cells (cadr (car tabulated-list-entries))))
              (should (equal (aref cells 3) "failed"))
              (should (equal (aref cells 7) "1"))
              (should (eq (get-text-property 0 'face (aref cells 7))
                          'font-lock-warning-face))))
        (kill-buffer buffer)
        (remove-hook 'e-board-orchestration-actions-projection-change-functions
                     #'e-board-runs-shell--refresh-buffers)
        (remove-hook 'e-subagent-registry-change-functions
                     #'e-board-runs-shell--refresh-registry-buffers)))))

(ert-deftest e-board-runs-shell-test-refreshes-from-projection-notification ()
  "A fact publication refreshes the shell without inspecting a child session."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "run-shell-notification")))
    (let ((buffer (e-board-runs-list-buffer :board board)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (should-not tabulated-list-entries))
            (e-board-runs-shell-test--publish board (e-board-runs-shell-test--manifest))
            (with-current-buffer buffer
              (should (= (length tabulated-list-entries) 1))))
        (kill-buffer buffer)
        (remove-hook 'e-board-orchestration-actions-projection-change-functions
                     #'e-board-runs-shell--refresh-buffers)
        (remove-hook 'e-subagent-registry-change-functions
                     #'e-board-runs-shell--refresh-registry-buffers)))))

(ert-deftest e-board-runs-shell-test-commands-are-interactive ()
  "The bounded run shell exposes detail and manual refresh commands."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "run-shell-commands"))
        (buffer nil))
    (setq buffer (e-board-runs-list-buffer :board board))
    (unwind-protect
        (with-current-buffer buffer
          (dolist (cell '(("RET" . e-board-runs-shell-show-details)
                          ("r" . e-board-runs-shell-show-raw-activity)
                          ("g" . e-board-runs-shell-refresh)))
            (let ((binding (keymap-lookup e-board-runs-shell-mode-map (car cell))))
              (should (eq binding (cdr cell)))
              (should (commandp binding)))))
      (kill-buffer buffer)
      (remove-hook 'e-board-orchestration-actions-projection-change-functions
                   #'e-board-runs-shell--refresh-buffers)
      (remove-hook 'e-subagent-registry-change-functions
                   #'e-board-runs-shell--refresh-registry-buffers))))

(ert-deftest e-board-runs-shell-test-default-summary-labels-identities-and-failure ()
  "The default detail names every identity and the first bounded failure."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "daily-board")))
    (e-board-runs-shell-test--publish board (e-board-runs-shell-test--manifest))
    (e-board-runs-shell-test--publish
     board
     (e-board-runs-shell-test--fact
      'terminal-report "failed"
      '(:run-id "run-1" :task-key "task" :attempt 0 :status failed
        :summary "worker stopped" :error "first admission failure"
        :participant-session-id "participant-1" :outputs [])))
    (let ((summary (e-board-runs-shell-test--summary board)))
      (dolist (line '("Daily run id: run-1"
                      "Board id: daily-board"
                      "Owner session id: owner-1"
                      "Task: task"
                      "Attempt: 0"
                      "Participant session id: participant-1"
                      "Admission: terminal"
                      "Disposition: terminal"
                      "First failure: first admission failure"))
        (should (string-match-p (regexp-quote line) summary)))
      (should-not (string-match-p ":manifest" summary)))))

(ert-deftest e-board-runs-shell-test-default-summary-exposes-pending-admission ()
  "A reserved child is visibly pending without becoming a registered child."
  (let* ((e-board--registry (make-hash-table :test 'equal))
         (board (e-board-create :id "daily-board-pending"))
         (registry (e-subagent-registry-create)))
    (e-board-runs-shell-test--publish board (e-board-runs-shell-test--manifest))
    (e-subagent-registry-reserve-admission
     registry :type :worker :role 'worker :session-id "participant-pending"
     :parent-session-id "owner-1" :label "Daily task" :schedule 'direct
     :run-id "run-1" :task-key "task" :attempt 0)
    (should-not (e-subagent-registry-list registry))
    (let ((summary (e-board-runs-shell-test--summary board registry)))
      (should (string-match-p "Participant session id: participant-pending" summary))
      (should (string-match-p "Admission: pending" summary))
      (should (string-match-p "Disposition: pending" summary))
      (should-not (string-match-p "Admission: running" summary)))))

(ert-deftest e-board-runs-shell-test-default-summary-labels-successor-retrying ()
  "A selected successor attempt is labelled retrying while admission waits."
  (let* ((e-board--registry (make-hash-table :test 'equal))
         (board (e-board-create :id "daily-board-retry"))
         (registry (e-subagent-registry-create)))
    (e-board-runs-shell-test--publish board (e-board-runs-shell-test--manifest))
    (e-board-runs-shell-test--publish
     board
     (e-board-runs-shell-test--fact
      'attempt-selection "retry-1"
      '(:run-id "run-1" :task-key "task" :attempt 1)))
    (e-subagent-registry-reserve-admission
     registry :type :worker :role 'worker :session-id "participant-retry"
     :parent-session-id "owner-1" :label "Daily task retry" :schedule 'direct
     :run-id "run-1" :task-key "task" :attempt 1)
    (let ((summary (e-board-runs-shell-test--summary board registry)))
      (should (string-match-p "Attempt: 1" summary))
      (should (string-match-p "Admission: pending" summary))
      (should (string-match-p "Disposition: retrying" summary)))))

(provide 'e-board-runs-shell-test)

;;; e-board-runs-shell-test.el ends here

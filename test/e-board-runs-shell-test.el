;;; e-board-runs-shell-test.el --- Tests for durable run shell -*- lexical-binding: t; -*-

(require 'ert)
(require 'e-board)
(require 'e-board-orchestration)
(require 'e-board-orchestration-actions)
(require 'e-board-runs-shell)

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
     :tasks ((:task-key "task" :required t :accepted-attempt 0))
     :deadline (:kind none))))

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
              (should (equal (aref cells 1) "failed"))
              (should (equal (aref cells 5) "1"))
              (should (eq (get-text-property 0 'face (aref cells 5))
                          'font-lock-warning-face))))
        (kill-buffer buffer)
        (remove-hook 'e-board-orchestration-actions-projection-change-functions
                     #'e-board-runs-shell--refresh-buffers)))))

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
                     #'e-board-runs-shell--refresh-buffers)))))

(ert-deftest e-board-runs-shell-test-commands-are-interactive ()
  "The bounded run shell exposes detail and manual refresh commands."
  (let ((e-board--registry (make-hash-table :test 'equal))
        (board (e-board-create :id "run-shell-commands"))
        (buffer nil))
    (setq buffer (e-board-runs-list-buffer :board board))
    (unwind-protect
        (with-current-buffer buffer
          (dolist (cell '(("RET" . e-board-runs-shell-show-details)
                          ("g" . e-board-runs-shell-refresh)))
            (let ((binding (keymap-lookup e-board-runs-shell-mode-map (car cell))))
              (should (eq binding (cdr cell)))
              (should (commandp binding)))))
      (kill-buffer buffer)
      (remove-hook 'e-board-orchestration-actions-projection-change-functions
                   #'e-board-runs-shell--refresh-buffers))))

(provide 'e-board-runs-shell-test)

;;; e-board-runs-shell-test.el ends here

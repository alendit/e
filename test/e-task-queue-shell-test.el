;;; e-task-queue-shell-test.el --- Tests for the task queue list shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shell smoke tests for `e-task-queue-shell': the list buffer renders rows and
;; refreshes when the backing queue changes.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-default-harnesses)
(require 'e-harness)
(require 'e-harness-instances)
(require 'e-task-queue)
(require 'e-task-queue-shell)
(require 'e-task-queue-actions)

(defmacro e-task-queue-shell-test--with-instances (&rest body)
  "Run BODY with isolated harness and harness-instance registries."
  (declare (indent 0) (debug t))
  `(let ((e-harness-registry--instances (make-hash-table :test 'equal))
         (e-harness-registry--factories (make-hash-table :test 'equal))
         (e-harness-instance--instances (make-hash-table :test 'equal))
         (e-harness-instance--defaults (make-hash-table :test 'equal)))
     (e-harness-instance-register
      :id :chat-test
      :name "Test"
      :kind 'chat
      :default t
      :factory (lambda () (e-harness-create
                           :backend (e-backend-fake-create :items nil))))
     ,@body))

(defun e-task-queue-shell-test--queue ()
  "Return a queue whose fake runner never auto-settles."
  (e-task-queue-create
   :runner (lambda (_task _harness _on-settle) (list :cancel #'ignore))))

(ert-deftest e-task-queue-shell-test-renders-rows-newest-first ()
  "The list buffer renders one row per task, newest-first."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (a (e-task-queue-enqueue queue :prompt "first"))
           (b (e-task-queue-enqueue queue :prompt "second"))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'e-task-queue-shell-mode))
            (should (equal (mapcar #'car tabulated-list-entries)
                           (list (plist-get b :task-id)
                                 (plist-get a :task-id)))))
        (kill-buffer buffer)))))

(ert-deftest e-task-queue-shell-test-refreshes-on-queue-change ()
  "An enqueue after opening the buffer is reflected by the change hook."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (should (null tabulated-list-entries)))
            (e-task-queue-enqueue queue :prompt "late arrival")
            (with-current-buffer buffer
              (should (= 1 (length tabulated-list-entries)))))
        (kill-buffer buffer)
        (remove-hook 'e-task-queue-change-functions
                     #'e-task-queue-shell--refresh-buffers)))))

(ert-deftest e-task-queue-shell-test-refresh-key-is-command ()
  "The `g' refresh keybinding is bound to an interactive command."
  (e-task-queue-shell-test--with-instances
    (let ((buffer (e-task-queue-list-buffer
                   :queue (e-task-queue-shell-test--queue))))
      (unwind-protect
          (with-current-buffer buffer
            (let ((binding (key-binding (kbd "g"))))
              (should (eq binding #'e-task-queue-shell-refresh))
              (should (commandp binding))))
        (kill-buffer buffer)
        (remove-hook 'e-task-queue-change-functions
                     #'e-task-queue-shell--refresh-buffers)))))

(ert-deftest e-task-queue-shell-test-pause-commands-are-commands ()
  "The pause/resume shell commands are interactive and bound."
  (e-task-queue-shell-test--with-instances
    (let ((buffer (e-task-queue-list-buffer
                   :queue (e-task-queue-shell-test--queue))))
      (unwind-protect
          (with-current-buffer buffer
            (dolist (cell '(("p" . e-task-queue-shell-pause)
                            ("r" . e-task-queue-shell-resume)
                            ("P" . e-task-queue-shell-pause-all)
                            ("R" . e-task-queue-shell-resume-all)))
              (let ((binding (key-binding (kbd (car cell)))))
                (should (eq binding (cdr cell)))
                (should (commandp binding)))))
        (kill-buffer buffer)
        (remove-hook 'e-task-queue-change-functions
                     #'e-task-queue-shell--refresh-buffers)))))

(ert-deftest e-task-queue-shell-test-paused-queue-sets-header ()
  "Pausing the queue shows a paused header line in the list buffer."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (with-current-buffer buffer
            (should (null header-line-format))
            (e-task-queue-shell-pause-all)
            (should (stringp header-line-format)))
        (kill-buffer buffer)
        (remove-hook 'e-task-queue-change-functions
                     #'e-task-queue-shell--refresh-buffers)))))

(ert-deftest e-task-queue-shell-test-default-list-reopens-sqlite-queue ()
  "The default list action restores one task through the SQLite composition."
  (let* ((directory (make-temp-file "e-task-queue-shell-sqlite-" t))
         (process-environment (copy-sequence process-environment))
         (e-default--runtime nil)
         (e-default--chat-sessions nil)
         (e-runtime-sqlite--live-composition nil)
         (e-task-queue-actions-default-queue nil)
         (e-cron-storage nil)
         (e-voice-adjustment-storage nil)
         (e-goodnite-resources-storage nil)
         (e-raw-results-storage nil)
         record buffer)
    (setenv "E_RUNTIME_STATE_DIRECTORY" directory)
    (unwind-protect
        (progn
          (let ((queue (e-runtime-sqlite-task-queue (e-default-runtime))))
            (e-task-queue-load queue)
            (e-task-queue-pause-all queue)
            ;; Execution authority is process-local.  This task remains paused,
            ;; but enqueue still requires a truthful runner capability.
            (setf (e-task-queue-runner queue)
                  (lambda (&rest _arguments)
                    (ert-fail "A paused task must not start")))
            (setq record
                  (e-task-queue-enqueue
                   queue :prompt "persisted through SQLite"
                   :summary "Persisted task")))
          (e-default-runtime-close)
          (let ((queue (e-runtime-sqlite-task-queue (e-default-runtime))))
            (should-not (e-task-queue-loaded-p queue)))
          (setq buffer (e-task-queue-list-buffer))
          (with-current-buffer buffer
            (should (eq e-task-queue-shell--queue
                        e-task-queue-actions-default-queue))
            (should (e-task-queue-loaded-p e-task-queue-shell--queue))
            (should (= (length tabulated-list-entries) 1))
            (should (equal (caar tabulated-list-entries)
                           (plist-get record :task-id)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (remove-hook 'e-task-queue-change-functions
                   #'e-task-queue-shell--refresh-buffers)
      (e-default-runtime-close)
      (delete-directory directory t))))


(ert-deftest e-task-queue-shell-test-shows-summary-stub-over-prompt ()
  "The Task column shows the agent-authored summary when present."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (_ (e-task-queue-enqueue
               queue
               :prompt "A long verbose prompt that should not be shown verbatim"
               :summary "Morsel runtime analysis"))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((cols (cadr (car tabulated-list-entries))))
              ;; The Task column (index 3) shows the stub, not the prompt.
              (should (equal (aref cols 3) "Morsel runtime analysis"))))
        (kill-buffer buffer)))))

(ert-deftest e-task-queue-shell-test-falls-back-to-prompt-prefix ()
  "Without a summary, the Task column falls back to the prompt prefix."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (_ (e-task-queue-enqueue queue :prompt "Research the thing"))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((cols (cadr (car tabulated-list-entries))))
              (should (string-prefix-p "Research the thing" (aref cols 3)))))
        (kill-buffer buffer)))))

(ert-deftest e-task-queue-shell-test-renders-key-hint-footer ()
  "The list buffer renders a key hint footer with the row actions."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (_ (e-task-queue-enqueue queue :prompt "task"))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "\\[RET\\] open session" text))
              (should (string-match-p "\\[c\\] cancel" text))))
        (kill-buffer buffer)))))

(ert-deftest e-task-queue-shell-test-open-session-key-is-command ()
  "RET is bound to the open-session command."
  (e-task-queue-shell-test--with-instances
    (let ((buffer (e-task-queue-list-buffer
                   :queue (e-task-queue-shell-test--queue))))
      (unwind-protect
          (with-current-buffer buffer
            (should (eq (keymap-lookup e-task-queue-shell-mode-map "RET")
                        #'e-task-queue-shell-open-session))
            (should (commandp #'e-task-queue-shell-open-session)))
        (kill-buffer buffer)))))

(ert-deftest e-task-queue-shell-test-open-session-without-session-errors ()
  "Opening a task that never started a session signals a user error."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-shell-test--queue))
           (_ (e-task-queue-enqueue queue :prompt "queued only"))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            ;; Move onto the first data row.
            (when (get-text-property (point) 'tabulated-list-id)
              (should-error (e-task-queue-shell-open-session)
                            :type 'user-error)))
        (kill-buffer buffer)))))

(ert-deftest e-task-queue-shell-test-status-shows-retry-count ()
  "The Status column annotates a re-armed task with its retry count."
  (e-task-queue-shell-test--with-instances
    (let* ((queue (e-task-queue-create
                   :max-retries 1
                   :runner (lambda (_task _harness _on-settle)
                             (list :session-id "sess-1"
                                   :cancel #'ignore))))
           (task (e-task-queue-enqueue queue :prompt "do the thing"))
           (task-id (plist-get task :task-id))
           (buffer (e-task-queue-list-buffer :queue queue)))
      (unwind-protect
          (progn
            ;; Fail the running task; with a session and a retry left it is
            ;; re-armed and its retry counter bumps to 1.
            (e-task-queue--settle queue task-id 'failed :error "boom")
            (with-current-buffer buffer
              (let ((cols (cadr (car tabulated-list-entries))))
                ;; Status column (index 1) carries the retry annotation.
                (should (string-match-p "retry 1" (aref cols 1))))))
        (kill-buffer buffer)
        (remove-hook 'e-task-queue-change-functions
                     #'e-task-queue-shell--refresh-buffers)))))

(provide 'e-task-queue-shell-test)

;;; e-task-queue-shell-test.el ends here

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

(ert-deftest e-task-queue-shell-test-list-buffer-rehydrates-default-queue ()
  "Opening the list buffer for the default queue rehydrates persisted tasks.
The buffer must show disk-backed tasks even when no harness built the
task-queue layer to trigger rehydration first."
  (e-task-queue-shell-test--with-instances
    (let* ((dir (make-temp-file "e-task-queue-shell-rehydrate-" t))
           (e-task-queue-directory (file-name-as-directory dir))
           (e-task-queue-actions-default-queue
            (e-task-queue-create
             :directory (file-name-as-directory dir)
             :runner (lambda (_t _h _s) (list :cancel #'ignore)))))
      (unwind-protect
          (progn
            ;; Persist a task through a separate queue on the same directory.
            (let ((writer (e-task-queue-create
                           :directory (file-name-as-directory dir)
                           :runner (lambda (_t _h _s) (list :cancel #'ignore)))))
              (e-task-queue-enqueue writer :prompt "persisted")
              (e-task-queue-flush writer))
            ;; Default queue is empty and unloaded, as after a restart.
            (put 'e-task-queue-actions-default-queue 'loaded nil)
            (clrhash (e-task-queue-records e-task-queue-actions-default-queue))
            (let ((buffer (e-task-queue-list-buffer)))
              (unwind-protect
                  (with-current-buffer buffer
                    (should (= (length tabulated-list-entries) 1)))
                (kill-buffer buffer)
                (remove-hook 'e-task-queue-change-functions
                             #'e-task-queue-shell--refresh-buffers))))
        (put 'e-task-queue-actions-default-queue 'loaded nil)
        (delete-directory dir t)))))

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

(provide 'e-task-queue-shell-test)

;;; e-task-queue-shell-test.el ends here

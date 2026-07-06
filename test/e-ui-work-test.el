;;; e-ui-work-test.el --- Tests for nonblocking UI work -*- lexical-binding: t; -*-

(require 'ert)
(require 'e)
(require 'e-request)
(require 'e-ui-work)
(require 'e-work)

(defun e-ui-work-test--buffer ()
  "Create a temporary UI work test buffer."
  (generate-new-buffer " *e-ui-work-test*"))

(defun e-ui-work-test--drain (buffer &optional timeout)
  "Drain pending UI work for BUFFER in batch/test context."
  (e-ui-work-with-batch-drain
    (e-ui-work-drain-batch :buffer buffer :timeout (or timeout 1.0))))

(defun e-ui-work-test--live-handle-p (handle)
  "Return non-nil when HANDLE is not terminal."
  (and (e-work-handle-p handle)
       (not (e-request-terminal-p (e-work-handle-lifecycle handle)))))

(ert-deftest e-ui-work-test-spec-requires-explicit-ui-policy ()
  "UI work specs require target, owner, focus, reentrancy, and apply policy."
  (let ((buffer (e-ui-work-test--buffer)))
    (unwind-protect
        (progn
          (should-error
           (e-ui-work-spec-create
            :id "bad"
            :description "Bad UI work."
            :owner 'test
            :target-buffer buffer
            :focus-policy 'preserve
            :reentrancy-policy 'defer)
           :type 'e-ui-work-invalid-spec)
          (should-error
           (e-ui-work-spec-create
            :id "bad"
            :description "Bad UI work."
            :owner 'test
            :target-buffer buffer
            :apply #'ignore
            :reentrancy-policy 'defer)
           :type 'e-ui-work-invalid-spec)
          (should-error
           (e-ui-work-spec-create
            :id "bad"
            :description "Bad UI work."
            :owner 'test
            :target-buffer buffer
            :apply #'ignore
            :focus-policy 'preserve)
           :type 'e-ui-work-invalid-spec))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-schedule-returns-work-handle-and-finishes ()
  "Scheduling UI work returns an e-work handle and settles through lifecycle."
  (let ((buffer (e-ui-work-test--buffer))
        ran)
    (unwind-protect
        (let ((handle
               (e-ui-work-schedule
                (e-ui-work-spec-create
                 :id "basic"
                 :description "Basic UI work."
                 :owner 'test
                 :target-buffer buffer
                 :key 'basic
                 :focus-policy 'preserve
                 :reentrancy-policy 'defer
                 :apply (lambda (_job _handle)
                          (setq ran (current-buffer))
                          '(:ok t))))))
          (should (e-work-handle-p handle))
          (should (e-ui-work-test--live-handle-p handle))
          (should (= (length (e-ui-work-pending buffer :owner 'test)) 1))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch
             :buffer buffer
             :include-intervals t))
          (should (eq ran buffer))
          (should (eq (plist-get (e-ui-work-status handle) :state) 'finished))
          (should-not (e-ui-work-pending buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-coalescing-cancels-older-pending-work ()
  "Coalescing cancels older pending work with the same owner/key."
  (let ((buffer (e-ui-work-test--buffer)))
    (unwind-protect
        (let* ((spec (lambda ()
                       (e-ui-work-spec-create
                        :id "coalesce"
                        :description "Coalesced UI work."
                        :owner 'test
                        :target-buffer buffer
                        :key 'same
                        :delay 60
                        :coalesce t
                        :focus-policy 'preserve
                        :reentrancy-policy 'defer
                        :apply (lambda (_job _handle) :ok))))
               (first (e-ui-work-schedule (funcall spec)))
               (second (e-ui-work-schedule (funcall spec))))
          (should (eq (plist-get (e-ui-work-status first) :state) 'cancelled))
          (let ((pending (e-ui-work-pending buffer :owner 'test :key 'same)))
            (should (= (length pending) 1))
            (should (eq (plist-get (car pending) :handle) second)))
          (e-ui-work-cancel second))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-cancel-matching-removes-pending-work ()
  "Owner/key cancellation settles matching UI work and leaves other keys alone."
  (let ((buffer (e-ui-work-test--buffer)))
    (unwind-protect
        (let ((first
               (e-ui-work-schedule
                (e-ui-work-spec-create
                 :id "cancel-a"
                 :description "Cancel UI work A."
                 :owner 'test
                 :target-buffer buffer
                 :key 'a
                 :delay 60
                 :focus-policy 'preserve
                 :reentrancy-policy 'defer
                 :apply (lambda (_job _handle) :a))))
              (second
               (e-ui-work-schedule
                (e-ui-work-spec-create
                 :id "cancel-b"
                 :description "Cancel UI work B."
                 :owner 'test
                 :target-buffer buffer
                 :key 'b
                 :delay 60
                 :focus-policy 'preserve
                 :reentrancy-policy 'defer
                 :apply (lambda (_job _handle) :b)))))
          (e-ui-work-cancel-matching buffer 'test :key 'a)
          (should (eq (plist-get (e-ui-work-status first) :state) 'cancelled))
          (should (e-ui-work-test--live-handle-p second))
          (should-not (e-ui-work-pending buffer :owner 'test :key 'a))
          (should (= (length (e-ui-work-pending buffer :owner 'test :key 'b))
                     1))
          (e-ui-work-cancel second))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-stale-work-cancels-without-running ()
  "Stale UI work is cancelled visibly and does not invoke apply."
  (let ((buffer (e-ui-work-test--buffer))
        ran)
    (unwind-protect
        (let ((handle
               (e-ui-work-schedule
                (e-ui-work-spec-create
                 :id "stale"
                 :description "Stale UI work."
                 :owner 'test
                 :target-buffer buffer
                 :key 'stale
                 :focus-policy 'preserve
                 :reentrancy-policy 'defer
                 :stale-p (lambda (_job) t)
                 :apply (lambda (_job _handle)
                          (setq ran t))))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch
             :buffer buffer
             :include-intervals t))
          (should-not ran)
          (should (eq (plist-get (e-ui-work-status handle) :state)
                      'cancelled))
          (should-not (e-ui-work-pending buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-errors-fail-handle-and-clear-pending ()
  "Apply errors surface through the work handle and clear pending UI state."
  (let ((buffer (e-ui-work-test--buffer))
        error)
    (unwind-protect
        (let ((handle
               (e-ui-work-schedule
                (e-ui-work-spec-create
                 :id "error"
                 :description "Error UI work."
                 :owner 'test
                 :target-buffer buffer
                 :key 'error
                 :focus-policy 'preserve
                 :reentrancy-policy 'defer
                 :apply (lambda (_job _handle)
                          (error "ui boom")))
                :on-error (lambda (err)
                            (setq error err)))))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch
             :buffer buffer
             :include-intervals t))
          (should (eq (plist-get (e-ui-work-status handle) :state) 'failed))
          (should (eq (car error) 'error))
          (should (string-match-p "ui boom" (error-message-string error)))
          (should-not (e-ui-work-pending buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-deadline-fails-and-clears-pending ()
  "Deadlines compose through e-work and remove pending UI work on failure."
  (let ((buffer (e-ui-work-test--buffer))
        ran
        error)
    (unwind-protect
        (let ((handle
               (e-ui-work-schedule
                (e-ui-work-spec-create
                 :id "deadline"
                 :description "Deadline UI work."
                 :owner 'test
                 :target-buffer buffer
                 :key 'deadline
                 :delay 1
                 :deadline (+ (float-time) 0.02)
                 :focus-policy 'preserve
                 :reentrancy-policy 'defer
                 :apply (lambda (_job _handle)
                          (setq ran t)))
                :on-error (lambda (err)
                            (setq error err)))))
          (e-ui-work-test--drain buffer 0.5)
          (should-not ran)
          (should (eq (plist-get (e-ui-work-status handle) :state) 'failed))
          (should (eq (car error) 'e-work-deadline-exceeded))
          (should-not (e-ui-work-pending buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-reentrant-same-key-defers-follow-up ()
  "Reentrant same owner/key work is deferred instead of running inline."
  (let ((buffer (e-ui-work-test--buffer))
        ran
        handle)
    (unwind-protect
        (progn
          (setq handle
                (e-ui-work-schedule
                 (e-ui-work-spec-create
                  :id "reentrant"
                  :description "Reentrant UI work."
                  :owner 'test
                  :target-buffer buffer
                  :key 'same
                  :delay 60
                  :focus-policy 'preserve
                  :reentrancy-policy 'defer
                  :apply (lambda (_job _handle)
                           (setq ran t)))))
          (with-current-buffer buffer
            (let* ((job (car e-ui-work--pending-jobs))
                   (key (e-ui-work--job-key job)))
              (setq-local e-ui-work--running-keys (list key))
              (e-ui-work--run-job-now job)
              (should-not ran)
              (should (e-ui-work-pending buffer :owner 'test :key 'same))
              (setq-local e-ui-work--running-keys nil)))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch
             :buffer buffer
             :include-intervals t))
          (should ran)
          (should (eq (plist-get (e-ui-work-status handle) :state)
                      'finished)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-chunks-render-with-budget ()
  "Chunk scheduling renders bounded batches and finishes after all items."
  (let ((buffer (e-ui-work-test--buffer))
        rendered
        schedules
        finished)
    (unwind-protect
        (let ((handle
               (e-ui-work-schedule-chunks
                buffer
                '(1 2 3)
                (lambda (item)
                  (push item rendered))
                :id "chunks"
                :description "Chunked UI work."
                :owner 'test
                :key 'chunks
                :budget 2
                :focus-policy 'preserve
                :reentrancy-policy 'defer
                :on-schedule (lambda (_handle)
                               (setq schedules (1+ (or schedules 0))))
                :on-finish (lambda ()
                             (setq finished t)))))
          (should (e-work-handle-p handle))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch
             :buffer buffer
             :include-intervals t))
          (should (equal (nreverse rendered) '(1 2 3)))
          (should (= schedules 2))
          (should finished)
          (should-not (e-ui-work-pending buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-interval-repeats-and-finishes ()
  "Intervals repeat until apply returns a terminal result."
  (let ((buffer (e-ui-work-test--buffer))
        (ticks 0))
    (unwind-protect
        (let ((handle
               (e-ui-work-schedule-interval
                (e-ui-work-spec-create
                 :id "interval"
                 :description "Interval UI work."
                 :owner 'test
                 :target-buffer buffer
                 :key 'interval
                 :focus-policy 'preserve
                 :reentrancy-policy 'defer
                 :apply (lambda (_job _handle)
                          (setq ticks (1+ ticks))
                          (if (< ticks 2)
                              :continue
                            (list :ticks ticks))))
                0.01)))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch
             :buffer buffer
             :include-intervals t))
          (should (= ticks 2))
          (should (eq (plist-get (e-ui-work-status handle) :state) 'finished))
          (should (equal (plist-get (e-ui-work-status handle) :result)
                         '(:ticks 2)))
          (should-not (e-ui-work-pending buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-interval-stale-self-cancels ()
  "Intervals cancel themselves when the target becomes stale."
  (let ((buffer (e-ui-work-test--buffer))
        ran)
    (unwind-protect
        (let ((handle
               (e-ui-work-schedule-interval
                (e-ui-work-spec-create
                 :id "stale-interval"
                 :description "Stale interval UI work."
                 :owner 'test
                 :target-buffer buffer
                 :key 'interval
                 :focus-policy 'preserve
                 :reentrancy-policy 'defer
                 :stale-p (lambda (_job) t)
                 :apply (lambda (_job _handle)
                          (setq ran t)
                          :continue))
                0.01)))
          (e-ui-work-with-batch-drain
            (e-ui-work-drain-batch
             :buffer buffer
             :include-intervals t))
          (should-not ran)
          (should (eq (plist-get (e-ui-work-status handle) :state)
                      'cancelled))
          (should-not (e-ui-work-pending buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-drain-is-batch-only-and-rejects-hot-paths ()
  "UI work batch drain cannot be used from interactive hot paths."
  (let ((buffer (e-ui-work-test--buffer)))
    (unwind-protect
        (progn
          (should-error
           (e-ui-work-drain-batch :buffer buffer :timeout 0.01)
           :type 'e-work-await-not-allowed)
          (e-ui-work-with-batch-drain
            (e-request-with-hot-path 'ui-work-test
              (should-error
               (e-ui-work-drain-batch :buffer buffer :timeout 0.01)
               :type 'e-work-await-in-hot-path))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest e-ui-work-test-removed-chat-render-scheduler-apis-stay-removed ()
  "Chat-local render scheduler APIs must not return as compatibility shims."
  (dolist (symbol '(e-chat--schedule-render-job
                    e-chat--schedule-render-work-items
                    e-chat--cancel-render-job
                    e-chat--cancel-render-jobs
                    e-chat--cancel-render-job-timer
                    e-chat--render-job-work-handle))
    (should-not (fboundp symbol)))
  (dolist (symbol '(e-chat-render-job
                    e-chat--pending-render-jobs
                    e-chat--pending-render-job-timers
                    e-chat--pending-markdown-presentation-timers))
    (should-not (boundp symbol))))

(ert-deftest e-ui-work-test-direct-render-carrier-use-is-guarded ()
  "Raw e-work :render use is limited to core adapters and direct tests."
  (let ((allowed '("lisp/core/e-work.el"
                   "lisp/core/e-ui-work.el"
                   "test/e-work-test.el"
                   "test/e-ui-work-test.el"
                   "test/e-tools-test.el"
                   "e2e/e-work-e2e-test.el"
                   "lisp/dev/e-dev-perf.el"))
        violations)
    (dolist (file (append (directory-files-recursively "lisp" "\\.el\\'")
                          (directory-files-recursively "test" "\\.el\\'")
                          (directory-files-recursively "e2e" "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (search-forward ":execution 'render" nil t)
          (unless (member file allowed)
            (push file violations)))))
    (should-not (sort violations #'string<))))

(provide 'e-ui-work-test)

;;; e-ui-work-test.el ends here

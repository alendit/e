;;; e-work-test.el --- Tests for e work substrate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the uniform non-blocking work lifecycle.

;;; Code:

(require 'ert)
(require 'e)
(require 'e-backend)
(require 'e-request)
(require 'e-task-queue)
(require 'e-work)

(ert-deftest e-work-test-spec-requires-explicit-policy ()
  "Work specs require execution and interactive policies."
  (should-error
   (e-work-spec-create :id "missing-execution" :interactive-policy 'async)
   :type 'e-work-invalid-spec)
  (should-error
   (e-work-spec-create :id "missing-policy" :execution 'cheap)
   :type 'e-work-invalid-spec))

(ert-deftest e-work-test-cheap-work-uses-lifecycle ()
  "Cheap work still returns a handle and finishes through callbacks."
  (let (done events)
    (let* ((spec (e-work-spec-create
                  :id "cheap"
                  :execution 'cheap
                  :interactive-policy 'cheap
                  :runner (lambda (arguments _context)
                            (plist-get arguments :value))))
           (handle (e-work-start
                    spec
                    '(:value 42)
                    :on-done (lambda (value) (setq done value))
                    :on-event (lambda (type payload)
                                (push (list type payload) events)))))
      (should (e-work-handle-p handle))
      (should (equal done 42))
      (should (eq (plist-get (e-work-status handle) :state) 'finished))
      (should (equal (plist-get (e-work-status handle) :result) 42))
       (should (assoc 'finished events)))))

(ert-deftest e-work-test-prepared-work-does-not-run-before-start ()
  "Prepared work has its canonical id before its runner can execute."
  (let* ((runs 0)
         (spec (e-work-spec-create
                :id "prepared"
                :execution 'cheap
                :interactive-policy 'cheap
                :runner (lambda (_arguments _context)
                          (cl-incf runs)
                          :done))))
    (let ((handle (e-work-prepare spec nil)))
      (should (stringp (e-work-handle-id handle)))
      (should (= runs 0))
      (should (eq (plist-get (e-work-status handle) :state) 'created))
      (e-work-start-prepared handle)
      (should (= runs 1))
      (should (eq (plist-get (e-work-status handle) :state) 'finished))
      (should-error (e-work-start-prepared handle)
                    :type 'e-work-prepared-start-invalid))))

(ert-deftest e-work-test-unsettled-state-follows-prepare-and-terminal-owner ()
  "Prepared work counts once and retires on its first terminal transition."
  (let ((e-work--unsettled-count 0)
        (e-work--unsettled-generation 0)
        (e-work--unsettled-change-function nil)
        snapshots)
    (setq e-work--unsettled-change-function
          (lambda (snapshot) (push snapshot snapshots)))
    (let ((first
           (e-work-prepare
            (e-work-spec-create
             :id "counted-first" :execution 'cooperative
             :interactive-policy 'async
             :runner (lambda (&rest _arguments) :deferred))
            nil))
          (second
           (e-work-prepare
            (e-work-spec-create
             :id "counted-second" :execution 'cooperative
             :interactive-policy 'async
             :runner (lambda (&rest _arguments) :deferred))
            nil)))
      (should (equal (e-work-unsettled-state)
                     '(:generation 2 :work-handles 2)))
      (e-work-finish first :done)
      (should (equal (e-work-unsettled-state)
                     '(:generation 3 :work-handles 1)))
      (should-not (e-work-fail first '(error "late")))
      (should (equal (e-work-unsettled-state)
                     '(:generation 3 :work-handles 1)))
      (e-work-cancel second)
      (should (equal (e-work-unsettled-state)
                     '(:generation 4 :work-handles 0)))
      (should (= (length snapshots) 4)))))

(ert-deftest e-work-test-cancelled-prepared-work-never-runs ()
  "Cancellation before start settles the handle without entering its carrier."
  (let ((runs 0)
        (handle
         (e-work-prepare
          (e-work-spec-create
           :id "prepared-cancelled"
           :execution 'cheap
           :interactive-policy 'cheap
           :runner (lambda (_arguments _context)
                     (cl-incf runs)))
          nil)))
    (e-work-cancel handle)
    (should (eq (plist-get (e-work-status handle) :state) 'cancelled))
    (should (= runs 0))
    (should-error (e-work-start-prepared handle)
                  :type 'e-work-prepared-start-invalid)))

(ert-deftest e-work-test-exact-owner-hook-inverses-preserve-replacements ()
  "Owner-specific hook inverses remove only the exact installed identity."
  (let* ((handle
          (e-work-prepare
           (e-work-spec-create
            :id "exact-hook-inverses" :execution 'cheap
            :interactive-policy 'cheap
            :runner (lambda (_arguments _context) :done))
           nil))
         (publication-a (lambda (&rest _arguments) nil))
         (publication-b (lambda (&rest _arguments) nil))
         (activity-a (lambda (&rest _arguments) nil))
         (activity-b (lambda (&rest _arguments) nil))
         (dispatcher-a (lambda (&rest _arguments) nil))
         (dispatcher-b (lambda (&rest _arguments) nil)))
    (e-work-install-publication-observer handle publication-a)
    (should-not (e-work-remove-publication-observer handle publication-b))
    (should (eq (e-work-handle-publication-observer handle) publication-a))
    (should (e-work-remove-publication-observer handle publication-a))
    (e-work-install-activity-observer handle activity-a)
    (should-not (e-work-remove-activity-observer handle activity-b))
    (should (eq (e-work-handle-activity-observer handle) activity-a))
    (should (e-work-remove-activity-observer handle activity-a))
    (e-work-install-hook-dispatcher handle dispatcher-a nil)
    (should-not (e-work-remove-hook-dispatcher handle dispatcher-b))
    (should (eq (e-work-handle-hook-dispatcher handle) dispatcher-a))
    (should (e-work-remove-hook-dispatcher handle dispatcher-a))
    (should-not (e-work-handle-publication-observer handle))
    (should-not (e-work-handle-activity-observer handle))
    (should-not (e-work-handle-hook-dispatcher handle))
    (e-work-cancel handle)))

(ert-deftest e-work-test-publication-observer-precedes-cleanup-and-callbacks ()
  "The dedicated publication observer sees terminal work first."
  (let (events)
    (e-work-start
     (e-work-spec-create
      :id "publication-order"
      :execution 'cheap
      :interactive-policy 'cheap
      :setup (lambda (_arguments _context)
               (list :cleanup (lambda (_handle) (push 'cleanup events))))
      :runner (lambda (_arguments _context) :done))
     nil
     :publication-observer
     (lambda (_handle state payload)
       (push (list 'publication state payload) events))
     :on-event (lambda (state _payload) (push (list 'event state) events))
     :on-done (lambda (_payload) (push 'done events)))
    (should (equal (nreverse events)
                   '((publication finished :done) cleanup
                     (event finished) done)))))

(ert-deftest e-work-test-terminal-gate-defers-first-proposal-until-commit ()
  "A terminal gate holds Work open until its owner authorizes settlement."
  (let (commit done events)
    (let* ((handle
            (e-work-prepare
             (e-work-spec-create
              :id "terminal-gate"
              :execution 'cooperative
              :interactive-policy 'async
              :runner (lambda (_handle _arguments _context) :deferred))
             nil
             :terminal-gate
             (lambda (_handle state payload authorize)
               (setq commit (list state payload authorize)))
             :on-done (lambda (payload) (setq done payload))
             :on-event (lambda (state payload)
                         (push (list state payload) events)))))
      (e-work-start-prepared handle)
      (e-work-finish handle :result)
      (should (equal (plist-get (e-work-status handle) :state) 'started))
      (should (equal (e-work-handle-terminal-proposal handle)
                     '(:state finished :payload :result)))
      (should (functionp (e-work-handle-terminal-gate handle)))
      (should-not done)
      (should-not events)
      (should (functionp (nth 2 commit)))
      (funcall (nth 2 commit))
      (should (equal done :result))
      (should (equal (plist-get (e-work-status handle) :state) 'finished))
      (should-not (e-work-handle-terminal-gate handle))
      (should-not (e-work-handle-terminal-proposal handle))
      (should (equal events '((finished :result)))))))

(ert-deftest e-work-test-terminal-gate-first-proposal-wins-and-cancel-cancels-carrier ()
  "A gated handle ignores late proposals and still cancels its carrier once."
  (let (authorize (cancel-calls 0))
    (let* ((handle
            (e-work-prepare
             (e-work-spec-create
              :id "terminal-gate-race"
              :execution 'cooperative
              :interactive-policy 'async
              :runner (lambda (_handle _arguments _context) :deferred))
             nil
             :terminal-gate
             (lambda (_handle _state _payload commit)
               (setq authorize commit)))))
      (e-work-start-prepared handle)
      (setf (e-work-handle-cancel-function
             handle)
            (lambda (_handle) (cl-incf cancel-calls)))
      (e-work-finish handle :done)
      (e-work-fail handle '(error "late"))
      (should-not (e-work-handle-terminal-commit-p handle))
      (e-work-cancel handle)
      (should (= cancel-calls 1))
      (should (equal (plist-get (e-work-status handle) :state) 'started))
      (should (eq (plist-get (e-work-handle-terminal-proposal handle) :state)
                  'finished))
      (funcall authorize)
      (should (equal (plist-get (e-work-status handle) :state) 'finished))
      (should-not (e-work-fail handle '(error "later"))))))

(ert-deftest e-work-test-activity-observer-precedes-deferred-progress-hook ()
  "A board-facing progress mailbox capture stays before general hook work."
  (let (events scheduled)
    (let ((handle
           (e-work-start
            (e-work-spec-create
             :id "activity-order" :execution 'render :interactive-policy 'async
             :runner (lambda (_arguments _context) :never))
            '(:delay 600)
            :activity-observer
            (lambda (_handle payload) (push (list 'activity payload) events))
            :on-progress
            (lambda (payload) (push (list 'progress payload) events))
            :hook-dispatcher
            (lambda (_handle _receipt thunk) (push thunk scheduled))
            :hook-policies '(:on-progress deferred :cancel deferred))))
      (unwind-protect
          (progn
            (e-work-progress handle '(:step one))
            (should (equal events '((activity (:step one)))))
            (funcall (pop scheduled))
            (should (equal (nreverse events)
                           '((activity (:step one)) (progress (:step one))))))
        (e-work-cancel handle)))))

(ert-deftest e-work-test-interactive-hook-classification-rejects-before-runner ()
  "A classified start cannot leave a general callback policy implicit."
  (let ((runs 0))
    (should-error
     (e-work-prepare
      (e-work-spec-create
       :id "unclassified" :execution 'cheap :interactive-policy 'cheap
       :runner (lambda (_arguments _context) (cl-incf runs)))
      nil
      :on-done (lambda (_value) nil)
      :hook-dispatcher (lambda (&rest _args) nil)
      :hook-policies nil)
     :type 'e-work-unclassified-hook)
    (should (= runs 0))))

(ert-deftest e-work-test-deferred-hooks-do-not-run-on-settlement-stack ()
  "Deferred cancellation and terminal callbacks await the owner scheduler."
  (let (scheduled events)
    (let ((handle
           (e-work-start
            (e-work-spec-create
             :id "deferred-hooks" :execution 'render :interactive-policy 'async
             :runner (lambda (_arguments _context) :never))
            '(:delay 600)
            :on-event (lambda (state _payload) (push (list 'event state) events))
            :hook-dispatcher
            (lambda (_handle receipt thunk)
              (push (cons receipt thunk) scheduled))
            :hook-policies '(:on-event deferred :cancel deferred))))
      (e-work-cancel handle)
      (should-not events)
      (should (= (length scheduled) 2))
      (dolist (entry scheduled)
        (funcall (cdr entry)))
      (should (equal events '((event cancelled)))))))

(ert-deftest e-work-test-fail-cancel-and-stale-callbacks ()
  "Failures and cancellation settle once; late callbacks are ignored."
  (let (error)
    (let* ((bad (e-work-spec-create
                 :id "bad"
                 :execution 'cheap
                 :interactive-policy 'cheap
                 :runner (lambda (_arguments _context)
                           (error "boom"))))
           (handle (e-work-start bad nil
                                 :on-error (lambda (err)
                                             (setq error err)))))
      (should (eq (plist-get (e-work-status handle) :state) 'failed))
      (should (eq (car error) 'error))))
  (let* ((spec (e-work-spec-create
                :id "cancel-me"
                :execution 'render
                :interactive-policy 'async
                :runner (lambda (_arguments _context) :late)))
         (handle (e-work-start spec '(:delay 60))))
    (e-work-progress handle '(:step queued))
    (should (equal (plist-get (e-work-status handle) :progress)
                   '(:step queued)))
    (e-work-cancel handle)
    (should (eq (plist-get (e-work-status handle) :state) 'cancelled))
    (should-not (e-work-finish handle :late))
    (should (eq (plist-get (e-work-status handle) :state) 'cancelled))))

(ert-deftest e-work-test-await-is-batch-only ()
  "Batch await requires an explicit batch/test scope and rejects hot paths."
  (let* ((spec (e-work-spec-create
                :id "await"
                :execution 'render
                :interactive-policy 'async
                :runner (lambda (_arguments _context) :done)))
         (handle (e-work-start spec '(:delay 60))))
    (unwind-protect
        (progn
          (should-error (e-work-await-batch handle :timeout 0.01)
                        :type 'e-work-await-not-allowed)
          (e-work-with-batch-await
            (e-request-with-hot-path 'test
              (should-error (e-work-await-batch handle :timeout 0.01)
                            :type 'e-work-await-in-hot-path))))
      (e-work-cancel handle))))

(defun e-work-test--pending-handle ()
  "Return a fresh non-terminal handle on the render carrier."
  (e-work-start
   (e-work-spec-create
    :id "pending"
    :execution 'render
    :interactive-policy 'async
    :runner (lambda (_arguments _context) :never))
   '(:delay 600)))

(defun e-work-test--terminal-handle ()
  "Return a fresh already-finished handle."
  (e-work-start
   (e-work-spec-create
    :id "terminal"
    :execution 'cheap
    :interactive-policy 'cheap
    :runner (lambda (_arguments _context) :ok))
   nil))

(ert-deftest e-work-test-on-settle-fires-once-on-terminal ()
  "`e-work-on-settle' fires immediately when terminal, else on settle."
  ;; Already terminal: fires now.
  (let ((calls 0))
    (e-work-on-settle (e-work-test--terminal-handle)
                      (lambda (_h) (cl-incf calls)))
    (should (= calls 1)))
  ;; Pending: fires on the terminal event, exactly once.
  (let ((handle (e-work-test--pending-handle))
        (calls 0))
    (unwind-protect
        (progn
          (e-work-on-settle handle (lambda (_h) (cl-incf calls)))
          (should (= calls 0))
          (e-work-finish handle :done)
          (should (= calls 1)))
      (e-work-cancel handle))))

(ert-deftest e-work-test-await-set-all-settles-after-last ()
  "MODE all settles only when every handle is terminal."
  (let* ((a (e-work-test--pending-handle))
         (b (e-work-test--pending-handle))
         report)
    (unwind-protect
        (progn
          (e-work-await-set (list a b)
                            :mode 'all
                            :on-settle (lambda (r) (setq report r)))
          (should-not report)
          (e-work-finish a :a)
          (should-not report)
          (e-work-finish b :b)
          (should report)
          (should (eq (plist-get report :reason) 'complete))
          (should (= (length (plist-get report :done)) 2))
          (should-not (plist-get report :pending)))
      (e-work-cancel a)
      (e-work-cancel b))))

(ert-deftest e-work-test-await-set-any-settles-after-first ()
  "MODE any settles when the first handle is terminal."
  (let* ((a (e-work-test--pending-handle))
         (b (e-work-test--pending-handle))
         report)
    (unwind-protect
        (progn
          (e-work-await-set (list a b)
                            :mode 'any
                            :on-settle (lambda (r) (setq report r)))
          (should-not report)
          (e-work-finish a :a)
          (should report)
          (should (eq (plist-get report :reason) 'complete))
          (should (= (length (plist-get report :done)) 1))
          (should (= (length (plist-get report :pending)) 1)))
      (e-work-cancel a)
      (e-work-cancel b))))

(ert-deftest e-work-test-await-set-already-terminal-settles-at-once ()
  "Awaiting handles that are already terminal settles synchronously."
  (let ((report nil))
    (e-work-await-set (list (e-work-test--terminal-handle)
                            (e-work-test--terminal-handle))
                      :mode 'all
                      :timeout 60
                      :on-settle (lambda (r) (setq report r)))
    (should report)
    (should (eq (plist-get report :reason) 'complete))))

(ert-deftest e-work-test-await-set-failed-counts-as-terminal ()
  "A failed or cancelled handle counts toward set completion."
  (let* ((a (e-work-test--pending-handle))
         (b (e-work-test--pending-handle))
         report)
    (unwind-protect
        (progn
          (e-work-await-set (list a b)
                            :mode 'all
                            :on-settle (lambda (r) (setq report r)))
          (e-work-fail a (list 'e-work-error "boom"))
          (should-not report)
          (e-work-cancel b)
          (should report)
          (should (eq (plist-get report :reason) 'complete)))
      (e-work-cancel a)
      (e-work-cancel b))))

(ert-deftest e-work-test-await-set-timeout-reports-pending ()
  "The timeout settles once with the pending handles and cancels the timer."
  (let* ((a (e-work-test--pending-handle))
         (b (e-work-test--pending-handle))
         report)
    (unwind-protect
        (progn
          (e-work-await-set (list a b)
                            :mode 'all
                            :timeout 0.05
                            :on-settle (lambda (r) (setq report r)))
          (e-work-finish a :a)
          ;; Pump the event loop until the timeout timer fires.
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (not report) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should report)
          (should (eq (plist-get report :reason) 'timed-out))
          (should (= (length (plist-get report :done)) 1))
          (should (= (length (plist-get report :pending)) 1)))
      (e-work-cancel a)
      (e-work-cancel b))))

(ert-deftest e-work-test-await-set-canceller-detaches ()
  "The returned canceller stops settlement without invoking ON-SETTLE."
  (let* ((a (e-work-test--pending-handle))
         (report nil)
         (cancel (e-work-await-set (list a)
                                   :mode 'all
                                   :on-settle (lambda (r) (setq report r)))))
    (unwind-protect
        (progn
          (funcall cancel)
          (e-work-finish a :a)
          (should-not report))
      (e-work-cancel a))))

(ert-deftest e-work-test-process-carrier-starts-before-exit ()
  "The process carrier returns a handle before process completion."
  (let* ((spec (e-work-spec-create
                :id "process"
                :execution 'process
                :interactive-policy 'async
                :command (lambda (_arguments _context)
                           (list :program "/bin/sh"
                                 :args '("-c" "sleep 0.05; printf ok")))))
         (handle (e-work-start spec nil)))
    (should (memq (plist-get (e-work-status handle) :state)
                  '(started progress)))
    (let ((result (e-work-with-batch-await
                    (e-work-await-batch handle :timeout 2))))
      (should (equal (plist-get result :stdout) "ok"))
      (should (equal (plist-get result :lines) '("ok"))))))

(ert-deftest e-work-test-process-carrier-runs-owner-cancel-hook ()
  "The process carrier lets its owner clean private state on cancellation."
  (let* ((state (list :staging "private"))
         seen
         (spec
          (e-work-spec-create
           :id "process-cancel-cleanup"
           :execution 'process
           :interactive-policy 'async
           :command
           (lambda (_arguments _context)
             (list :program "/bin/sh"
                   :args '("-c" "sleep 5")
                   :state state
                   :on-cancel
                   (lambda (_handle _process active-state)
                     (setq seen active-state))))))
         (handle (e-work-start spec nil)))
    (should (e-work-cancel handle))
    (should (eq seen state))))

(ert-deftest e-work-test-process-carrier-cleans-owner-state-on-start-failure ()
  "A process command owner can clean allocations when startup fails."
  (let* ((state (list :allocated t))
         seen
         (spec
          (e-work-spec-create
           :id "process-start-cleanup"
           :execution 'process
           :interactive-policy 'async
           :command
           (lambda (_arguments _context)
             (list :program "/definitely/missing/e-work-program"
                   :state state
                   :on-cancel
                   (lambda (_handle _process active-state)
                     (setq seen active-state)))))))
    (let ((handle
           (cl-letf (((symbol-function 'make-process)
                      (lambda (&rest _)
                        (signal 'file-missing '("simulated start failure")))))
             (e-work-start spec nil))))
      (should (eq (e-request-lifecycle-state (e-work-handle-lifecycle handle))
                  'failed))
      (should (eq seen state)))))

(ert-deftest e-work-test-process-carrier-publishes-streaming-progress ()
  "The process carrier can publish push-style progress from output chunks."
  (let ((seen "")
        progress-events)
    (let* ((spec (e-work-spec-create
                  :id "stream-process"
                  :execution 'process
                  :interactive-policy 'async
                  :command
                  (lambda (_arguments _context)
                    (list :program "/bin/sh"
                          :args '("-c" "printf one; printf two")
                          :capture-output nil
                          :on-output
                          (lambda (_handle _process chunk _state)
                            (setq seen (concat seen chunk)))
                          :progress
                          (lambda (_handle _process _state)
                            (list :preview seen))
                          :progress-interval 0))
                  :result-shaper
                  (lambda (raw _arguments _context)
                    (list :status (plist-get raw :status)
                          :seen seen))))
           (handle (e-work-start
                    spec
                    nil
                    :on-progress
                    (lambda (payload)
                      (push payload progress-events)))))
      (let ((result (e-work-with-batch-await
                      (e-work-await-batch handle :timeout 2))))
        (should (equal (plist-get result :status) 'ok))
        (should (equal (plist-get result :seen) "onetwo"))
        (should progress-events)
        (should (string-match-p
                 "one"
                 (plist-get (car (last progress-events)) :preview)))))))

(ert-deftest e-work-test-url-carrier-starts-before-callback ()
  "The URL carrier returns a handle before its callback settles."
  (let (callback buffer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url cb &rest _args)
                 (setq callback cb)
                 (setq buffer (generate-new-buffer " *e-work-url-test*"))
                 buffer)))
      (let* ((spec (e-work-spec-create
                    :id "url"
                    :execution 'url
                    :interactive-policy 'async
                    :url (lambda (_arguments _context)
                           "https://example.invalid/")
                    :timeout 30
                    :result-shaper (lambda (raw _arguments _context)
                                     (buffer-live-p
                                      (plist-get raw :buffer)))))
             (handle (e-work-start spec nil)))
        (should (eq (plist-get (e-work-status handle) :state) 'progress))
        (with-current-buffer buffer
          (funcall callback nil))
        (should (eq (e-work-with-batch-await
                      (e-work-await-batch handle :timeout 1))
                    t))
        (should-not (buffer-live-p buffer))))))

(ert-deftest e-work-test-url-carrier-cleans-late-callback-after-timeout ()
  "A URL callback arriving after timeout is ignored and its buffer is killed."
  (let (callback error)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url cb &rest _args)
                 (setq callback cb)
                 nil)))
      (let* ((spec (e-work-spec-create
                    :id "url-timeout"
                    :execution 'url
                    :interactive-policy 'async
                    :url (lambda (_arguments _context)
                           "https://example.invalid/")
                    :timeout 0.01))
             (handle (e-work-start spec nil
                                   :on-error (lambda (err)
                                               (setq error err)))))
        (should-error
         (e-work-with-batch-await
           (e-work-await-batch handle :timeout 1))
         :type 'e-work-url-failed)
        (should error)
        (let ((late-buffer (generate-new-buffer " *e-work-url-late*")))
          (with-current-buffer late-buffer
            (funcall callback nil))
          (should-not (buffer-live-p late-buffer)))))))

(ert-deftest e-work-test-render-carrier-runs-through-timer ()
  "The render carrier schedules and settles through a work handle."
  (let ((ran nil))
    (let* ((spec (e-work-spec-create
                  :id "render"
                  :execution 'render
                  :interactive-policy 'async
                  :runner (lambda (_arguments _context)
                            (setq ran t)
                            :rendered)))
           (handle (e-work-start spec '(:delay 0))))
      (should (timerp (plist-get (e-work-handle-metadata handle) :timer)))
      (should (eq (e-work-with-batch-await
                    (e-work-await-batch handle :timeout 1))
                  :rendered))
      (should ran))))

(ert-deftest e-work-test-cooperative-carrier-self-settles ()
  "The cooperative carrier lets a runner settle through the returned handle."
  (let (progress handle)
    (setq handle
          (e-work-start
           (e-work-spec-create
            :id "cooperative"
            :execution 'cooperative
            :interactive-policy 'async
            :runner (lambda (work-handle _arguments _context)
                      (run-at-time
                       0 nil
                       (lambda ()
                         (e-work-progress work-handle '(:step "running"))
                         (e-work-finish work-handle :done)))
                      :deferred))
           nil
           :on-progress (lambda (payload)
                          (setq progress payload))))
    (should (e-work-handle-p handle))
    (should (eq (plist-get (e-work-status handle) :state) 'started))
    (should (eq (e-work-with-batch-await
                  (e-work-await-batch handle :timeout 1))
                :done))
    (should (equal progress '(:step "running")))))

(ert-deftest e-work-test-backend-carrier-streams-progress ()
  "The backend carrier starts provider work and publishes request/item progress."
  (let (cancelled progress request-seen seen)
    (let* ((backend
            (e-backend-create
             :name 'work-backend
             :start
             (cl-function
              (lambda (&key messages options on-item on-done on-request-start
                            &allow-other-keys)
                (ignore messages options)
                (let ((request
                       (e-backend-request-create
                        :cancel (lambda ()
                                  (setq cancelled t)
                                  t)
                        :metadata '(:provider fake :transport timer))))
                  (funcall on-request-start request)
                  (run-at-time
                   0 nil
                   (lambda ()
                     (funcall on-item
                              '(:type assistant-delta :content "hi"))
                     (funcall on-done '(:status done))))
                  request)))))
           (spec
            (e-work-spec-create
             :id "backend"
             :execution 'backend
             :interactive-policy 'async
             :backend (lambda (_arguments _context) backend)
             :messages (lambda (_arguments _context)
                         '((:role user :content "hello")))
             :options (lambda (_arguments _context)
                        '(:model "fake"))
             :request-handler
             (lambda (_handle request _arguments _context)
               (setq request-seen request))
             :item-handler
             (lambda (_handle item _arguments _context)
               (push item seen)))))
      (let ((handle (e-work-start
                     spec
                     nil
                     :on-progress (lambda (payload)
                                    (push payload progress)))))
        (should (e-work-handle-p handle))
        (should (eq (plist-get (e-work-handle-metadata handle)
                               :transport)
                    'backend))
        (should (equal (plist-get
                        (plist-get (e-work-handle-metadata handle)
                                   :backend-request-metadata)
                        :provider)
                       'fake))
        (should (e-backend-request-p request-seen))
        (should (equal (e-work-with-batch-await
                         (e-work-await-batch handle :timeout 1))
                       '(:status done)))
        (should (equal (plist-get (car progress) :item)
                       '(:type assistant-delta :content "hi")))
        (should (equal seen '((:type assistant-delta :content "hi"))))
        (e-work-cancel handle)
        (should-not cancelled)))
    (let* ((backend
            (e-backend-create
             :name 'cancellable-work-backend
             :start
             (cl-function
              (lambda (&key on-done &allow-other-keys)
                (let ((request
                       (e-backend-request-create
                        :cancel (lambda ()
                                  (setq cancelled t)
                                  t)
                        :metadata '(:provider fake :transport timer))))
                  (run-at-time 60 nil (lambda ()
                                        (funcall on-done '(:status done))))
                  request)))))
           (handle
            (e-work-start
             (e-work-spec-create
              :id "backend-cancel"
              :execution 'backend
              :interactive-policy 'async
              :backend (lambda (_arguments _context) backend))
             nil)))
      (e-work-cancel handle)
      (should cancelled))))

(ert-deftest e-work-test-backend-carrier-ignores-request-after-terminal ()
  "A backend request returned after synchronous completion is not remembered."
  (let (done request-seen)
    (let* ((backend
            (e-backend-create
             :name 'sync-terminal-backend
             :start
             (cl-function
              (lambda (&key on-done &allow-other-keys)
                (funcall on-done '(:status done))
                (e-backend-request-create
                 :metadata '(:provider stale-after-done))))))
           (spec
            (e-work-spec-create
             :id "backend-terminal"
             :execution 'backend
             :interactive-policy 'async
             :backend (lambda (_arguments _context) backend)
             :request-handler
             (lambda (_handle request _arguments _context)
               (setq request-seen request)))))
      (let ((handle (e-work-start spec nil
                                  :on-done (lambda (value)
                                             (setq done value)))))
        (should (equal done '(:status done)))
        (should-not request-seen)
        (should-not (plist-get (e-work-handle-metadata handle)
                               :backend-request))))))

(ert-deftest e-work-test-backend-deadline-fails-and-cancels-request ()
  "A stalled backend work item fails visibly at its absolute deadline."
  (let (cancelled error)
    (let* ((backend
            (e-backend-create
             :name 'stalled-work-backend
             :start
             (cl-function
              (lambda (&key on-request-start &allow-other-keys)
                (let ((request
                       (e-backend-request-create
                        :cancel (lambda ()
                                  (setq cancelled t)
                                  t)
                        :metadata '(:provider fake :transport timer))))
                  (funcall on-request-start request)
                  request)))))
           (spec
            (e-work-spec-create
             :id "backend-deadline"
             :execution 'backend
             :interactive-policy 'async
             :backend (lambda (_arguments _context) backend)
             :deadline (lambda (_arguments _context)
                         (+ (float-time) 0.02)))))
      (let ((handle (e-work-start
                     spec
                     nil
                     :on-error (lambda (err)
                                 (setq error err)))))
        (should-error
         (e-work-with-batch-await
           (e-work-await-batch handle :timeout 1))
         :type 'e-work-deadline-exceeded)
        (should cancelled)
        (should (eq (car error) 'e-work-deadline-exceeded))
        (should (numberp (plist-get (caddr error) :deadline)))
        (should (eq (plist-get (e-work-status handle) :state) 'failed))))))

(ert-deftest e-work-test-cancel-settles-when-carrier-cancel-errors ()
  "Underlying cancel errors are exposed without blocking cancellation state."
  (let* ((spec
          (e-work-spec-create
           :id "cancel-error"
           :execution 'cooperative
           :interactive-policy 'async
           :runner (lambda (handle _arguments _context)
                     (setf (e-work-handle-cancel-function handle)
                           (lambda (_handle)
                             (error "cancel exploded")))
                     :deferred)))
         (handle (e-work-start spec nil)))
    (e-work-cancel handle)
    (let* ((status (e-work-status handle))
           (error-payload (plist-get status :error))
           (cancel-error (plist-get error-payload :cancel-error)))
      (should (eq (plist-get status :state) 'cancelled))
      (should (eq (plist-get error-payload :status) 'cancelled))
      (should (eq (car cancel-error) 'error))
      (should (string-match-p "cancel exploded"
                              (error-message-string cancel-error))))))

(ert-deftest e-work-test-agent-task-carrier-returns-task-record ()
  "The agent-task carrier enqueues and finishes with a task record."
  (let* ((queue (e-task-queue-create
                 :max-parallel 0 :directory nil
                 :runner (lambda (&rest _)
                           (ert-fail "A zero-parallel queue must not run"))))
         (spec (e-work-spec-create
                :id "agent-task"
                :execution 'agent-task
                :interactive-policy 'async
                :task-queue (lambda (_arguments _context) queue)
                :prompt (lambda (arguments _context)
                          (plist-get arguments :prompt))
                :summary (lambda (arguments _context)
                           (plist-get arguments :summary))))
         (handle (e-work-start spec '(:prompt "do work"
                                      :summary "Work"))))
    (should (eq (plist-get (e-work-status handle) :state) 'finished))
    (should (equal (plist-get (e-work-handle-result handle) :status)
                   'queued))
    (should (equal (plist-get (e-work-handle-result handle) :summary)
                   "Work"))))

(ert-deftest e-work-test-error-message-formats-plain-condition ()
  "A plain error condition formats to its normal message."
  (should (string-match-p
           "boom"
           (e-work-error-message '(error "boom")))))

(ert-deftest e-work-test-error-message-bounds-huge-payload ()
  "A condition carrying a huge data payload formats to a bounded string.
This is the shape that hung Emacs: a backend error whose condition data was
enormous.  `error-message-string' with the caller's print settings builds one
ever-growing string (RSS climbing ~1GB/min, 100% CPU) until memory runs out.
The guard binds `print-length'/`print-level', so the message stays small."
  (let* ((big (make-list 1000000 42))
         (err (list 'e-loop-backend-error "upstream reset" big))
         (message (e-work-error-message err)))
    (should (stringp message))
    ;; Unbounded this would be millions of chars; the caps keep it tiny.
    (should (< (length message) 4096))))

(ert-deftest e-work-test-error-message-terminates-on-cyclic-payload ()
  "A condition whose data is a self-referential cycle still formats.
`print-circle' lets the printer emit cycle markers instead of recursing
forever, so the call returns promptly rather than hanging."
  (let ((cyclic (list 1 2 3)))
    (setcdr (cddr cyclic) cyclic)      ; 3's cdr points back at the head
    (let ((message (with-timeout (5 (ert-fail "e-work-error-message did not terminate"))
                     ;; A defined error prints its data (unlike bare `error',
                     ;; which collapses to "peculiar error").
                     (e-work-error-message (list 'e-loop-backend-error "reset" cyclic)))))
      (should (stringp message))
      ;; `print-circle' emits the #N=/#N# cycle markers rather than looping.
      (should (string-match-p "#[0-9]+" message)))))

(ert-deftest e-work-test-format-safe-formats-plain-value ()
  "`e-format-safe' behaves like `format' for a small value."
  (should (equal "x=(1 2 3)" (e-format-safe "x=%S" (list 1 2 3)))))

(ert-deftest e-work-test-format-safe-bounds-huge-value ()
  "`e-format-safe' caps a huge argument instead of building a giant string.
This is the async-timer shape of the printer hang: a timer callback runs
\(format \"...%S\" value) on an enormous value, and the printer spins at 100%
CPU while RSS climbs until the process is killed.  The length/level caps keep
the result tiny."
  (let ((message (e-format-safe "event=%S" (make-list 1000000 42))))
    (should (stringp message))
    (should (< (length message) 4096))))

(ert-deftest e-work-test-format-safe-terminates-on-cyclic-value ()
  "`e-format-safe' returns on a self-referential value rather than hanging.
`print-circle' emits cycle markers instead of recursing forever."
  (let ((cyclic (list 1 2 3)))
    (setcdr (cddr cyclic) cyclic)
    (let ((message (with-timeout (5 (ert-fail "e-format-safe did not terminate"))
                     (e-format-safe "event=%S" cyclic))))
      (should (stringp message))
      (should (string-match-p "#[0-9]+" message)))))

(ert-deftest e-work-test-prin1-safe-terminates-on-cyclic-value ()
  "`e-prin1-safe' returns on a cyclic value rather than hanging."
  (let ((cyclic (list 1 2 3)))
    (setcdr (cddr cyclic) cyclic)
    (let ((text (with-timeout (5 (ert-fail "e-prin1-safe did not terminate"))
                  (e-prin1-safe cyclic))))
      (should (stringp text))
      (should (string-match-p "#[0-9]+" text)))))

(ert-deftest e-work-test-kill-buffer-quietly-no-process ()
  "`e-kill-buffer-quietly' kills an ordinary buffer and tolerates a dead one."
  (let ((buffer (generate-new-buffer " *e-test-quiet*")))
    (e-kill-buffer-quietly buffer)
    (should-not (buffer-live-p buffer))
    ;; A second call on the now-dead buffer is a no-op, not an error.
    (e-kill-buffer-quietly buffer)))

(ert-deftest e-work-test-kill-buffer-quietly-live-process-does-not-prompt ()
  "`e-kill-buffer-quietly' kills a buffer with a live process without prompting.
This is the headless-agent hang: `kill-buffer' on a buffer whose request
process is still live triggers `process-kill-buffer-query-function', which
asks \"has a running process; kill it?\" and blocks the turn forever.  Bind a
query function that fails the test if it is ever consulted, so any prompt path
is caught."
  (let* ((buffer (generate-new-buffer " *e-test-quiet-proc*"))
         (process (start-process "e-test-sleep" buffer
                                 (or (executable-find "sleep") "sleep") "60")))
    (should (process-live-p process))
    ;; Emacs would normally prompt because the process is live.
    (let ((kill-buffer-query-functions
           (list (lambda () (ert-fail "kill-buffer prompted about a live process")))))
      (with-timeout (5 (ert-fail "e-kill-buffer-quietly did not return"))
        (e-kill-buffer-quietly buffer)))
    (should-not (buffer-live-p buffer))
    (should-not (process-live-p process))))

;;; Detach coordinator and detached-work registry.

(defmacro e-work-test--with-clean-detach-registry (&rest body)
  "Run BODY with a fresh detached-work registry."
  (declare (indent 0))
  `(let ((e-work--detached-handles (make-hash-table :test 'equal)))
     ,@body))

(defun e-work-test--child-spec ()
  "Return a render-carrier child spec that stays pending unless finished."
  (e-work-spec-create
   :id "race-child"
   :execution 'render
   :interactive-policy 'async
   :runner (lambda (_arguments _context) :never)))

(ert-deftest e-work-test-race-detaches-immediately-on-zero-wait ()
  "wait-for 0 detaches now without arming a timer and never inlines."
  (let* ((child (e-work-start (e-work-test--child-spec) '(:delay 600)))
         inline detach)
    (unwind-protect
        (progn
          (e-work-race-or-detach
           child
           :wait-for 0
           :on-inline (lambda (_c) (setq inline t))
           :on-detach (lambda (_c) (setq detach t)))
          (should detach)
          (should-not inline))
      (e-work-cancel child))))

(ert-deftest e-work-test-race-inlines-when-work-wins ()
  "A child terminal inside the window settles on the inline branch."
  (let* ((child (e-work-start (e-work-test--child-spec) '(:delay 600)))
         inline detach)
    (unwind-protect
        (progn
          (e-work-race-or-detach
           child
           :wait-for 60
           :on-inline (lambda (_c) (setq inline t))
           :on-detach (lambda (_c) (setq detach t)))
          (should-not inline)
          (e-work-finish child :done)
          (should inline)
          (should-not detach))
      (e-work-cancel child))))

(ert-deftest e-work-test-race-detaches-when-timer-wins ()
  "A child still running at the deadline settles on the detach branch."
  (let* ((child (e-work-start (e-work-test--child-spec) '(:delay 600)))
         inline detach)
    (unwind-protect
        (progn
          (e-work-race-or-detach
           child
           :wait-for 0.05
           :on-inline (lambda (_c) (setq inline t))
           :on-detach (lambda (_c) (setq detach t)))
          ;; `sleep-for' does not dispatch timers in batch Emacs.  Pump the
          ;; event loop until the deadline callback has had a chance to run.
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (not detach) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should detach)
          (should-not inline))
      (e-work-cancel child))))

(ert-deftest e-work-test-detach-register-and-resolve ()
  "A detached handle is resolvable by its own id and lists in the registry."
  (e-work-test--with-clean-detach-registry
    (let ((child (e-work-start (e-work-test--child-spec) '(:delay 600))))
      (unwind-protect
          (progn
            (e-work-detach-register child)
            (should (eq (e-work-detached-handle (e-work-handle-id child))
                        child))
            (should (member (e-work-handle-id child)
                            (e-work-detached-handle-ids))))
        (e-work-cancel child)))))

(ert-deftest e-work-test-detachable-spec-inline-returns-child-result ()
  "When the child finishes inside the window, the parent returns its result."
  (e-work-test--with-clean-detach-registry
    (let* ((spec (e-work-detachable-spec
                  (e-work-spec-create
                   :id "inline-child"
                   :execution 'process
                   :interactive-policy 'async
                   :command (lambda (_a _c)
                              (list :program "/bin/sh"
                                    :args '("-c" "printf ok"))))
                  :default-wait-for 60))
           done)
      (e-work-start spec nil :on-done (lambda (v) (setq done v)))
      ;; Drive the event loop until the parent settles.
      (with-timeout (3 (ert-fail "detachable inline did not settle"))
        (while (not done)
          (accept-process-output nil 0.02)))
      (should (equal (plist-get done :stdout) "ok"))
      ;; Nothing detached on the inline path.
      (should-not (e-work-detached-handle-ids)))))

(ert-deftest e-work-test-detachable-spec-detaches-and-acks ()
  "When the window expires, the parent detaches and returns a work: reference."
  (e-work-test--with-clean-detach-registry
    (let* ((spec (e-work-detachable-spec
                  (e-work-test--child-spec)
                  :default-wait-for 0.05))
           done
           (handle (e-work-start spec '(:delay 600)
                                 :on-done (lambda (v) (setq done v)))))
      (unwind-protect
          (progn
            (with-timeout (3 (ert-fail "detachable detach did not settle"))
              (while (not done)
                (accept-process-output nil 0.02)))
            (should (equal (plist-get done :state) "running"))
            (let ((reference (plist-get done :reference)))
              (should (string-prefix-p "work:" reference))
              ;; The reference resolves to a still-live detached handle.
              (let ((id (substring reference (length "work:"))))
                (should (e-work-handle-p (e-work-detached-handle id))))))
        (dolist (id (e-work-detached-handle-ids))
          (when-let ((h (e-work-detached-handle id)))
             (e-work-cancel h)))))))

(ert-deftest e-work-test-detachable-spec-enrolls-child-before-runner ()
  "The detachable child reaches an injected enrollment port before execution."
  (let (enrolled done)
    (let ((spec
           (e-work-detachable-spec
            (e-work-spec-create
             :id "enrolled-child" :execution 'cheap :interactive-policy 'cheap
             :runner (lambda (_arguments _context)
                       (unless enrolled
                         (error "Child runner started before enrollment"))
                       "done"))
            :default-wait-for nil)))
      (e-work-start
       spec nil
       :context (list :board-enroll-work
                      (lambda (handle)
                        (setq enrolled handle)))
       :on-done (lambda (value) (setq done value)))
      (should (e-work-handle-p enrolled))
      (should (equal done "done")))))

(provide 'e-work-test)

;;; e-work-test.el ends here

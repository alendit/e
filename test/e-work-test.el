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
  (let* ((queue (e-task-queue-create :max-parallel 0 :directory nil))
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

(provide 'e-work-test)

;;; e-work-test.el ends here

;;; e-work-test.el --- Tests for e work substrate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the uniform non-blocking work lifecycle.

;;; Code:

(require 'ert)
(require 'e)
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
  "Batch await rejects interactive hot paths."
  (let* ((spec (e-work-spec-create
                :id "await"
                :execution 'render
                :interactive-policy 'async
                :runner (lambda (_arguments _context) :done)))
         (handle (e-work-start spec '(:delay 60))))
    (unwind-protect
        (e-request-with-hot-path 'test
          (should-error (e-work-await-batch handle :timeout 0.01)
                        :type 'e-work-await-in-hot-path))
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
    (let ((result (e-work-await-batch handle :timeout 2)))
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
      (let ((result (e-work-await-batch handle :timeout 2)))
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
        (should (eq (e-work-await-batch handle :timeout 1) t))
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
      (should (eq (e-work-await-batch handle :timeout 1) :rendered))
      (should ran))))

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

;;; e-cron-actions-test.el --- Tests for cron action routing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona

;; Author: Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for `e-cron-actions'.  These exercise the seam that turns a
;; declarative action spec into an engine closure and routes a fire to the
;; task queue or a background session.  The task queue and background-session
;; entry points are stubbed so a fire is observed as a recorded call, without
;; running a real harness turn.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-cron)
(require 'e-cron-actions)

(defmacro e-cron-actions-test--with-registry (&rest body)
  "Run BODY with an isolated schedule registry and stubbed timers."
  (declare (indent 0) (debug t))
  `(let ((e-cron--schedules (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'stub-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'timerp) (lambda (v) (eq v 'stub-timer))))
       ,@body)))

(ert-deftest e-cron-actions-test-enqueue-routes-to-queue ()
  "An :enqueue action enqueues its prompt on the default task queue."
  (e-cron-actions-test--with-registry
    (let (calls)
      (cl-letf (((symbol-function 'e-task-queue-actions-ensure-loaded)
                 (lambda () 'the-queue))
                ((symbol-function 'e-task-queue-enqueue)
                 (lambda (queue &rest args) (push (cons queue args) calls))))
        (let ((schedule (e-cron-actions-register
                         :id 'nightly :when '(:every 60)
                         :action '(:enqueue (:prompt "triage the inbox"
                                             :harness-instance-id "chat-project-e")))))
          (should (e-cron-fire schedule))
          (should (= 1 (length calls)))
          (let ((call (car calls)))
            (should (eq 'the-queue (car call)))
            (should (string= "triage the inbox"
                             (plist-get (cdr call) :prompt)))
            (should (eq :chat-project-e
                       (plist-get (cdr call) :harness-instance-id)))))))))

(ert-deftest e-cron-actions-test-wake-routes-to-trigger ()
  "A :wake action fires the named background-session trigger."
  (e-cron-actions-test--with-registry
    (let (fired)
      (cl-letf (((symbol-function 'e-background-session-get)
                 (lambda (id) (and (eq id 'sources) 'the-trigger)))
                ((symbol-function 'e-background-session-fire)
                 (lambda (trigger) (push trigger fired))))
        (let ((schedule (e-cron-actions-register
                         :id 'refresh :when '(:every 60)
                         :action '(:wake sources))))
          (should (e-cron-fire schedule))
          (should (equal '(the-trigger) fired)))))))

(ert-deftest e-cron-actions-test-wake-unknown-trigger-signals ()
  "A :wake action for an unregistered trigger signals when it fires."
  (e-cron-actions-test--with-registry
    (cl-letf (((symbol-function 'e-background-session-get) (lambda (_id) nil)))
      (let ((schedule (e-cron-actions-register
                       :id 'refresh :when '(:every 60)
                       :action '(:wake missing))))
        (should-error (e-cron-fire schedule)
                      :type 'e-cron-actions-invalid-action)))))

(ert-deftest e-cron-actions-test-call-routes-to-handler ()
  "A :call action invokes the named handler with the schedule."
  (e-cron-actions-test--with-registry
    (let (seen)
      (let ((schedule (e-cron-actions-register
                       :id 'custom :when '(:every 60)
                       :action (list :call (lambda (s) (setq seen s))))))
        (should (e-cron-fire schedule))
        (should (eq schedule seen))))))

(ert-deftest e-cron-actions-test-describe-carries-action-spec ()
  "The descriptor surfaces the original action spec, when, and enabled state."
  (e-cron-actions-test--with-registry
    (e-cron-actions-register
     :id 'nightly :when '(:every 1800)
     :action '(:enqueue (:prompt "x")))
    (let ((descriptor (e-cron-actions--status '(:id nightly))))
      (should (eq 'nightly (plist-get descriptor :id)))
      (should (equal '(:every 1800) (plist-get descriptor :when)))
      (should (equal '(:enqueue (:prompt "x")) (plist-get descriptor :action)))
      (should (eq t (plist-get descriptor :enabled))))))

(ert-deftest e-cron-actions-test-enable-disable-remove ()
  "The control actions arm, disarm, and unregister a schedule."
  (e-cron-actions-test--with-registry
    (e-cron-actions-register :id 'ctl :when '(:every 60)
                             :action '(:call ignore))
    (should-not (plist-get (e-cron-actions--disable '(:id ctl)) :enabled))
    (should-not (e-cron-schedule-enabled (e-cron-get 'ctl)))
    (should (plist-get (e-cron-actions--enable '(:id ctl)) :enabled))
    (should (e-cron-schedule-enabled (e-cron-get 'ctl)))
    (should (plist-get (e-cron-actions--remove '(:id ctl)) :removed))
    (should-not (e-cron-get 'ctl))))

(ert-deftest e-cron-actions-test-invalid-enqueue-signals ()
  "An :enqueue spec without a prompt signals at build time."
  (e-cron-actions-test--with-registry
    (should-error (e-cron-actions-register
                   :id 'bad :when '(:every 60) :action '(:enqueue nil))
                  :type 'e-cron-actions-invalid-action)))

(provide 'e-cron-actions-test)

;;; e-cron-actions-test.el ends here

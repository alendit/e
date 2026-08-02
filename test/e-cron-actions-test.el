;;; e-cron-actions-test.el --- Board-bound cron action tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'e-cron)
(require 'e-cron-actions)

(cl-defmacro e-cron-actions-test--with-board ((board binding) &body body)
  "Run BODY with isolated cron/runtime state and producer BINDING on BOARD."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let ((e-cron--schedules (make-hash-table :test 'equal))
         (e-cron--state (make-hash-table :test 'equal))
         (e-cron--state-loaded t)
         (e-cron-state-file nil)
         (e-board--registry (make-hash-table :test 'equal))
         (e-board--id-sequence 0)
         (e-board-registry--boards (make-hash-table :test 'equal))
         (e-board-registry--id-sequence 0)
         (e-board-runtime--producer-bindings (make-hash-table :test 'equal))
         (e-board-runtime--producer-epoch 0)
         (e-board-runtime--producer-head nil)
         (e-board-runtime--producer-tail nil)
         (e-board-runtime--producer-drain-scheduled nil)
         (e-board-runtime--producer-scheduler (lambda (_callback)))
         (e-board-runtime--admission-open-p t)
         (e-board-runtime--unsettled-producer-count 0)
         (e-board-runtime--unsettled-generation 0)
         (e-cron-actions-producer-binding nil))
     (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
               ((symbol-function 'timerp) (lambda (value) (eq value 'timer)))
               ((symbol-function 'cancel-timer) #'ignore))
       (let* ((,board (e-board-registry-create :id "cron-board"))
              (,binding (e-board-runtime-producer-bind
                         'cron-test ,board :tags '(scheduled))))
         ,@body))))

(ert-deftest e-cron-actions-test-register-requires-live-binding ()
  "Schedule registration fails visibly without a current board binding."
  (let ((e-cron-actions-producer-binding nil))
    (should-error
     (e-cron-actions-register
      :id 'missing :when '(:every 60)
      :action '(:publish (:content "tick")))
     :type 'e-board-runtime-producer-disabled)))

(ert-deftest e-cron-actions-test-fire-publishes-fact ()
  "A cron fire queues and publishes exactly one descriptive board fact."
  (e-cron-actions-test--with-board (board binding)
    (let ((schedule
           (e-cron-actions-register
            :id 'refresh :when '(:every 60) :producer-binding binding
            :action '(:publish (:content "refresh" :tags (maintenance)
                                :attributes (:scope sources))))))
      (e-cron-fire schedule)
      (should (= e-board-runtime--unsettled-producer-count 1))
      (e-board-runtime-drain-producers)
      (let ((message (car (e-board-messages
                           (e-board-registry-board-source-board board)))))
        (should (equal (e-board-message-content message) "refresh"))
        (should (equal (e-board-message-tags message)
                       '(scheduled cron maintenance)))
        (should (equal (plist-get (e-board-message-attributes message) :scope)
                       'sources))))))

(ert-deftest e-cron-actions-test-retires-arbitrary-coordination-actions ()
  "Legacy callback, queue, and wake action forms are rejected."
  (e-cron-actions-test--with-board (_board binding)
    (dolist (action (list '(:enqueue (:prompt "x"))
                          '(:wake trigger)
                          (list :call #'ignore)
                          #'ignore))
      (should-error
       (e-cron-actions-register
        :id (gensym "retired") :when '(:every 60)
        :producer-binding binding :action action)
       :type 'e-cron-actions-invalid-action))))

(ert-deftest e-cron-actions-test-stale-binding-fences-later-fire ()
  "A retained cron callback cannot publish after its process binding is stale."
  (e-cron-actions-test--with-board (_board binding)
    (let ((schedule
           (e-cron-actions-register
            :id 'stale :when '(:every 60) :producer-binding binding
            :action '(:publish (:content "tick")))))
      (e-board-runtime-producer-disable binding)
      (should-error (e-cron-fire schedule)
                    :type 'e-board-runtime-producer-disabled))))

(provide 'e-cron-actions-test)

;;; e-cron-actions-test.el ends here

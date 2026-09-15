;;; e-board-run-set-test.el --- Bounded Board run-set tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-orchestration)

(defun e-board-run-set-test--projection
    (run-id state position &optional extras)
  "Return a reduced run projection fixture."
  (append
   (list :run-id run-id
         :tasks (list (list :task-key "required" :required t :state state))
         :latest-event-position position)
   extras))

(ert-deftest e-board-run-set-test-orders-actionable-and-bounds-records ()
  "Attention and newest evidence lead a deterministic bounded run-set."
  (let ((value
         (e-board-orchestration-run-set-projection
          (list
           (e-board-run-set-test--projection
            "finishing" 'done 9 '(:terminal-status done
                                   :continuation (:state pending)))
           (e-board-run-set-test--projection
            "attention" 'failed 2 '(:terminal-status failed
                                    :continuation (:state pending)
                                    :conflicts ((:reason "conflict"))))
           (e-board-run-set-test--projection "running" 'running 10))
          :board-id "board-1" :record-limit 2)))
    (should (equal (mapcar (lambda (entry) (plist-get entry :run-id))
                           (plist-get value :runs))
                   '("attention" "running")))
    (should (= (plist-get value :omitted-count) 1))
    (should (eq (plist-get value :status) 'attention))
    (should (equal (plist-get (car (plist-get value :runs)) :lifecycle)
                   'attention))))

(ert-deftest e-board-run-set-test-excludes-consumed-terminal-runs ()
  "Only runs that remain active belong to the active run-set projection."
  (let* ((value
          (e-board-orchestration-run-set-projection
           (list
            (e-board-run-set-test--projection
             "consumed" 'done 4 '(:terminal-status done
                                   :continuation (:state consumed)))
            (e-board-run-set-test--projection
             "terminal-without-continuation" 'done 5 '(:terminal-status done))
            (e-board-run-set-test--projection "running" 'running 3))
           :board-id "board-1"))
         (entries (plist-get value :runs)))
    (should (= (plist-get value :active-count) 1))
    (should (equal (mapcar (lambda (entry) (plist-get entry :run-id)) entries)
                   '("running")))
    (should (eq (plist-get value :status) 'running))))

(ert-deftest e-board-run-set-test-active-definition-includes-optional-and-unconsumed ()
  "Optional work and unconsumed terminal continuation stay active."
  (let* ((optional
          (list :run-id "optional" :terminal-status 'done
                :tasks (list (list :task-key "required" :required t :state 'done)
                             (list :task-key "optional" :required nil :state 'running))))
         (unconsumed
          (list :run-id "continuation" :terminal-status 'done
                :continuation '(:state pending)
                :tasks (list (list :task-key "required" :required t :state 'done))))
         (value (e-board-orchestration-run-set-projection
                 (list optional unconsumed) :board-id "board-1"))
         (entries (plist-get value :runs)))
    (should (= (plist-get value :active-count) 2))
    (should (cl-every (lambda (entry) (plist-get entry :active-p)) entries))
    (should (eq (plist-get (car entries) :lifecycle) 'finishing))))

(ert-deftest e-board-run-set-test-final-byte-accounting-and-readiness ()
  "The final detached representation includes exact bytes and restore state."
  (let* ((projection
          (e-board-run-set-test--projection
           "bounded" 'running 1 '(:descriptor (:label "bounded"))))
         (value
          (e-board-orchestration-run-set-projection
           (list projection) :board-id "board-1" :byte-limit 4096))
         (state (e-board-orchestration-run-set-state-create :board-id "board-1"))
         (notifications nil))
    (should (<= (plist-get value :bytes)
                4096))
    (should (= (plist-get value :bytes)
               (string-bytes (prin1-to-string value))))
    (should-not (plist-get state :ready-p))
    (e-board-orchestration-run-set-state-subscribe
     state (lambda (new-value generation)
             (push (list new-value generation) notifications)))
    (e-board-orchestration-run-set-state-update
     state (list projection) :restore-state 'ready)
    (should (e-board-orchestration-run-set-state-ready-p state))
    (should (= (length notifications) 1))
    (should (equal (plist-get (e-board-orchestration-run-set-compact-status
                              state "bounded")
                              :selected-run-id)
                   "bounded"))
    (should (equal (plist-get
                    (plist-get (e-board-orchestration-run-set-compact-status
                                state "bounded")
                               :summary)
                    :run-id)
                   "bounded"))))

(provide 'e-board-run-set-test)

;;; e-board-run-set-test.el ends here

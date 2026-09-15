;;; e-board-run-set-test.el --- Bounded Board run-set tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board)
(require 'e-board-orchestration)
(require 'e-context)

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

(ert-deftest e-board-run-set-test-zero-projection-is-ready-and-bounded ()
  "A settled empty run-set is distinct from an in-progress restore."
  (let ((value
         (e-board-orchestration-run-set-projection
          nil :board-id "board-1" :restore-state 'ready)))
    (should (plist-get value :ready-p))
    (should (eq (plist-get value :restore-state) 'ready))
    (should-not (plist-get value :runs))
    (should (= (plist-get value :active-count) 0))
    (should (= (plist-get value :active-run-count) 0))
    (should (eq (plist-get value :status) 'idle))
    (should (= (plist-get value :omitted-count) 0))
    (should (<= (plist-get value :bytes) 4096))))

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

(ert-deftest e-board-run-set-test-context-and-status-share-detached-projection ()
  "Context and persistent status consume the same bounded run projection."
  (let* ((projection
          (e-board-run-set-test--projection
           "selected" 'running 7 '(:label "Selected run")))
         (value
          (e-board-orchestration-run-set-projection
           (list projection) :board-id "board-1"))
         (state (e-board-orchestration-run-set-state-create :board-id "board-1"))
         (provider
          (e-board-run-set-context-provider (lambda (_harness _session-id)
                                              state))))
    (e-board-orchestration-run-set-state-set-value state value)
    (let* ((context (e-board-orchestration-run-set-context state))
           (message (car (e-context-provider-build
                          provider :harness 'harness :session-id "session-1"
                          :context-purpose 'turn)))
           (status (e-board-orchestration-run-set-compact-status
                    state "selected")))
      (should (equal (plist-get context :runs)
                     (plist-get value :runs)))
      (should (equal (plist-get (plist-get context :projection) :runs)
                     (plist-get context :runs)))
      (should (string-match-p "selected"
                              (plist-get message :content)))
      (should (equal (plist-get status :selected-run-id) "selected"))
      (should (equal (plist-get (plist-get status :activity-link) :run-id)
                     "selected"))
      (should (equal (plist-get (plist-get status :projection) :runs)
                     (plist-get context :runs))))))

(ert-deftest e-board-run-set-test-compact-status-covers-five-visible-states ()
  "The persistent summary distinguishes every admitted compact run state."
  (let ((state (e-board-orchestration-run-set-state-create :board-id "board-1")))
    (should (string-match-p "restoring"
                           (plist-get
                            (e-board-orchestration-run-set-compact-status state)
                            :text)))
    (dolist (case '((dispatching :state pending)
                    (running :state running)
                    (finishing :terminal-status done
                               :continuation (:state pending))
                    (attention :state running
                               :conflicts ((:reason conflict)))))
      (let* ((projection
              (e-board-run-set-test--projection
               (symbol-name (car case))
               (or (plist-get (cdr case) :state) 'done)
               1
               (cdr case)))
             (value
              (e-board-orchestration-run-set-projection
               (list projection) :board-id "board-1")))
        (e-board-orchestration-run-set-state-set-value state value)
        (let ((status
               (e-board-orchestration-run-set-compact-status
                state (symbol-name (car case)))))
          (should (eq (plist-get status :status) (car case))))))))

(provide 'e-board-run-set-test)

;;; e-board-run-set-test.el ends here

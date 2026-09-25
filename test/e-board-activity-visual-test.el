;;; e-board-activity-visual-test.el --- Visual Board activity tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dimitri Vorona
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'e-board-activity-visual-shell)

(ert-deftest e-board-activity-visual-test-run-selector-keeps-bounded-status ()
  "The selector exposes counts, restore state, attention, and omitted runs."
  (let* ((projection
          '(:board-id "board-1" :status attention :restore-state ready
            :ready-p t :active-run-count 33 :omitted-count 1
            :runs ((:run-id "run-1" :label "Daily update"
                    :lifecycle attention :required-count 2 :optional-count 1
                    :required-state-counts (:total 2 :running 1 :pending 1)
                    :optional-state-counts (:total 1 :done 1)
                    :attention-p t :conflicts ((:reason "conflict"))
                    :deadline (:expired t) :restore-state ready
                    :completion-delivery-state failed
                    :completion-execution-state cancelled))))
         (run-set
          (e-board-activity-visual-view-model-run-set projection)))
    (should (eq (alist-get 'ready run-set) t))
    (should (eq (alist-get 'showMoreAvailable run-set) t))
    (should (= (alist-get 'activeCount run-set) 33))
    (should (= (alist-get 'omittedCount run-set) 1))
    (let ((run (aref (alist-get 'runs run-set) 0)))
      (should (equal (alist-get 'label run) "Daily update"))
      (should (equal (alist-get 'lifecycle run) "attention"))
      (should (eq (alist-get 'attention run) t))
      (should (= (alist-get 'conflictCount run) 1))
      (should (equal (alist-get 'deadlineLabel run) "expired"))
      (should (equal (alist-get 'completionDeliveryState run) "failed"))
      (should (equal (alist-get 'completionExecutionState run) "cancelled")))))

(ert-deftest e-board-activity-visual-test-manifest-sentinel-keeps-exact-counts ()
  "A manifest sentinel enables Show more without inflating materialized counts."
  (let* ((projection '(:status running :restore-state ready :ready-p t
                       :active-run-count 32 :omitted-count 0 :more-p t))
         (run-set (e-board-activity-visual-view-model-run-set projection))
         (expanded
          (e-board-activity-visual-view-model-run-set projection t)))
    (should (eq (alist-get 'showMoreAvailable run-set) t))
    (should (= (alist-get 'activeCount run-set) 32))
    (should (= (alist-get 'omittedCount run-set) 0))
    (should (eq (alist-get 'moreMayExist expanded) t))
    (should-not (eq (alist-get 'showMoreAvailable expanded) t))
    (should (= (alist-get 'activeCount expanded) 32))
    (should (= (alist-get 'omittedCount expanded) 0))))

(ert-deftest e-board-activity-visual-test-page-keeps-tasks-and-boardwide-participants ()
  "A coherent page retains an unadmitted task without inventing a participant."
  (let* ((page
          '(:board-id "board-1" :generation 4 :revision 17
            :run (:run-id "run-1" :label "Daily update"
                  :terminal-status nil)
            :tasks ((:task-key "calendar" :label "Calendar" :required t
                     :accepted-attempt 1 :state queued)
                    (:task-key "slack" :label "Slack" :required t
                     :accepted-attempt 2 :state running
                     :outcome (:status done :summary "sent")
                     :participant-id "worker-1"
                     :participant-row (:participant-id "worker-1"
                                       :name "Slack worker" :state running
                                       :outcome (:status done :summary "sent"))))
            :participants ((:participant-id "ad-hoc" :name "Ad hoc"
                           :state waiting))))
         (snapshot
          (e-board-activity-visual-view-model-snapshot
           :board-id "board-1" :selected-run-id "run-1"
           :selected-task '(:run-task "run-1" "calendar" 1)
           :detail-state 'ready :page page)))
    (let* ((detail (alist-get 'detail snapshot))
           (tasks (alist-get 'requiredTasks detail))
           (pending (aref tasks 0))
           (running (aref tasks 1)))
      (should (equal (alist-get 'state detail) "ready"))
      (should (= (alist-get 'generation detail) 4))
      (should (= (alist-get 'revision detail) 17))
      (should (equal (alist-get 'taskKey pending) "calendar"))
      (should (equal (alist-get 'state pending) "queued"))
      (should-not (alist-get 'participantId pending))
      (should (equal (alist-get 'participantId running) "worker-1"))
      (should (equal (alist-get 'outcomeSummary running) "sent"))
      (should (equal (alist-get 'participantId
                                (aref (alist-get 'participants detail) 0))
                     "ad-hoc"))
      (should (equal (alist-get 'taskKey (alist-get 'selectedTask snapshot))
                     "calendar")))))

(ert-deftest e-board-activity-visual-test-loading-and-mismatched-pages-hide-rows ()
  "Detail rows stay hidden while loading or when page identities disagree."
  (let* ((page '(:board-id "board-1" :generation 1 :revision 3
                 :run (:run-id "run-1")
                 :tasks ((:task-key "task" :required t
                          :accepted-attempt 0 :state running))))
         (loading
          (e-board-activity-visual-view-model-snapshot
           :board-id "board-1" :selected-run-id "run-1"
           :detail-state 'loading :page page))
         (mismatch
          (e-board-activity-visual-view-model-snapshot
           :board-id "board-2" :selected-run-id "run-1"
           :detail-state 'ready :page page)))
    (should (equal (alist-get 'state (alist-get 'detail loading)) "loading"))
    (should (= (length (alist-get 'requiredTasks (alist-get 'detail loading))) 0))
    (should (equal (alist-get 'state (alist-get 'detail mismatch)) "loading"))
    (should (= (length (alist-get 'requiredTasks (alist-get 'detail mismatch)))
               0))))

(ert-deftest e-board-activity-visual-test-rejects-stale-board-and-run-epoch ()
  "Semantic UI events must match the exact Board and current selector epoch."
  (let ((e-board-activity-visual--binding
         (e-chat-service--binding-create
          :lifecycle-state 'ready :board-id "board-1"))
        (e-board-activity-visual--run-set-epoch 9))
    (cl-letf (((symbol-function 'e-board-activity-visual--target-id)
               (lambda () "board-1")))
      (should
       (e-board-activity-visual--payload-current-p
        '((boardId . "board-1") (runSetEpoch . 9))))
      (should-not
       (e-board-activity-visual--payload-current-p
        '((boardId . "board-2") (runSetEpoch . 9))))
      (should-not
       (e-board-activity-visual--payload-current-p
        '((boardId . "board-1") (runSetEpoch . 8)))))))

(ert-deftest e-board-activity-visual-test-rejects-stale-task-clicks ()
  "A click from an older Board revision cannot change selected task identity."
  (let ((e-board-activity-visual--binding
         (e-chat-service--binding-create
          :lifecycle-state 'ready :board-id "board-1"))
        (e-board-activity-visual--target 'target)
        (e-board-activity-visual--run-set-epoch 4)
        (e-board-activity-visual--selected-run-id "run-1")
        (e-board-activity-visual--detail-state 'ready)
        (e-board-activity-visual--detail-page
         '(:board-id "board-1" :generation 2 :revision 10
           :run (:run-id "run-1")
           :tasks ((:task-key "calendar" :accepted-attempt 1))))
        e-board-activity-visual--selected-task)
    (cl-letf (((symbol-function 'e-board-activity-visual--target-id)
               (lambda () "board-1"))
              ((symbol-function 'e-board-activity-visual--schedule-push)
               #'ignore))
      (e-board-activity-visual--handle-ui-action
       '((action . "select-task") (boardId . "board-1")
         (runSetEpoch . 4) (generation . 2) (revision . 9)
         (runId . "run-1") (taskKey . "calendar") (attempt . 1)))
      (should-not e-board-activity-visual--selected-task)
      (e-board-activity-visual--handle-ui-action
       '((action . "select-task") (boardId . "board-1")
         (runSetEpoch . 4) (generation . 2) (revision . 10)
         (runId . "run-1") (taskKey . "calendar") (attempt . 1)))
      (should (equal e-board-activity-visual--selected-task
                     '(:run-task "run-1" "calendar" 1))))))

(ert-deftest e-board-activity-visual-test-show-more-accepts-manifest-sentinel ()
  "Show more runs is accepted when the bounded query found another manifest."
  (let ((e-board-activity-visual--binding
         (e-chat-service--binding-create
          :lifecycle-state 'ready :board-id "board-1"))
        (e-board-activity-visual--run-set-epoch 4)
        (e-board-activity-visual--run-set-projection
         '(:active-run-count 32 :omitted-count 0 :more-p t))
        started)
    (cl-letf (((symbol-function 'e-board-activity-visual--target-id)
               (lambda () "board-1"))
              ((symbol-function 'e-board-activity-visual--start-expanded-run-set)
               (lambda () (setq started t)))
              ((symbol-function 'e-board-activity-visual--schedule-push)
               #'ignore))
      (e-board-activity-visual--handle-ui-action
       '((action . "show-more-runs") (boardId . "board-1")
         (runSetEpoch . 4)))
      (should started))))

(ert-deftest e-board-activity-visual-test-fallback-names-native-command ()
  "Unavailable WASM views open text and tell the user the explicit command."
  (let (message-text arguments)
    (cl-letf (((symbol-function 'e-board-sqlite-publication-target-valid-p)
               (lambda (_target) t))
              ((symbol-function 'e-board-activity-visual-unavailable-reason)
               (lambda () "WASM assets missing"))
              ((symbol-function 'message)
               (lambda (format-string &rest values)
                 (setq message-text (apply #'format format-string values))))
              ((symbol-function 'e-board-activity-list-buffer)
               (lambda (&rest values) (setq arguments values) 'native-buffer)))
      (should (eq (e-board-activity-visual-open-or-text
                   'target 'binding "run-1")
                  'native-buffer))
      (should (equal (plist-get arguments :run-id) "run-1"))
      (should (string-match-p "e-chat-open-board-activity-text"
                              message-text)))))

(provide 'e-board-activity-visual-test)

;;; e-board-activity-visual-test.el ends here
